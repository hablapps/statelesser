package org.apps.gist
package university

import Function.const
import scalaz._, Scalaz._

/**
 * State algebras
 */

case class Getter[P[_],S](get: P[S]) {
  def apply() = get
}

case class Setter[P[_],S](set: S => P[Unit]) {
  def apply(s: S): P[Unit] = set(s)
}

case class Field[P[_],S](get: Getter[P,S], put: Setter[P,S]) {
  def modify(f: S => S)(implicit B: Bind[P]): P[Unit] = 
    get() >>= { s => put(f(s)) } 
}

case class ListP[Alg[_[_],_], P[_], S](algs: P[List[Alg[P, S]]]) {

  def apply() = algs

  def traverse[T](
      f: Alg[P,S] => P[T])(implicit 
      M: Monad[P]): P[List[T]] = 
    apply >>= { _ traverse[P,T] f }

  def traverseZip[A,T](
      values: List[A])(
      f: (Alg[P,S],A) => P[T])(implicit 
      M: Monad[P]): P[List[T]] =
    apply >>= { algs => 
      (algs zip values) traverse f.tupled
    }
}

trait StdTraverse[P[_],S] extends ListP[Field,P,S]{
  def getAll(implicit M: Monad[P]): P[List[S]] =
    traverse(_.get())

  def putAll(values: List[S])(implicit M: Monad[P]): P[List[Unit]] =
    traverseZip(values)(_ put _)
}


/** 
 * Utilities & Combinators 
 */

object Util {

  type StateField[S, A] = Field[State[S, ?], A]

  object StateField {

    def apply[S, A](g: S => A, p: A => S => S): StateField[S, A] =
      Field[State[S, ?], A](
        Getter(State.gets(g)),
        Setter(a => State(s => (p(a)(s), ()))))

    def refl[S]: StateField[S, S] = apply(identity, const)
  }

  type Lens[S, A] = State[A, ?] ~> State[S, ?]

  object Lens {

    def apply[S, A](get: S => A, set: A => S => S): Lens[S, A] =
      λ[State[A, ?] ~> State[S, ?]] { sa =>
        State(s => sa(get(s)).leftMap(set(_)(s)))
      }

    def id[S]: Lens[S, S] = NaturalTransformation.refl
  }

  // Lens & StateField are isomorphic

  def lens2StateField[S, A](ln: Lens[S, A]): StateField[S, A] =
    Field[State[S, ?], A](
      Getter(ln(State.get)),
      Setter(a => ln(State.put(a))))

  def stateField2Lens[S, A](fl: StateField[S, A]): Lens[S, A] =
    λ[State[A, ?] ~> State[S, ?]] { sta =>
      State { s =>
        val (a, x) = sta.run(fl.get().eval(s))
        (fl.put(a).exec(s), x)
      }
    }
  
  // Maps the interpretation of an algebra. 
  trait AlgFunctor[Alg[_[_], _]] {
    def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P): Alg[Q, ?] ~> Alg[P, ?]
  }

  object AlgFunctor {

    // XXX: boilerplate instances, consider using Shapeless here?
    
    implicit val GetterAlgFunctor = new AlgFunctor[Getter] {
      def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
        λ[Getter[Q, ?] ~> Getter[P, ?]] { alg =>
          Getter(f(alg.apply))
        }
    }

    implicit val SetterAlgFunctor = new AlgFunctor[Setter] {
      def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
        λ[Setter[Q, ?] ~> Setter[P, ?]] { alg =>
          Setter(x => f(alg(x)))
        }
    }

    implicit val FieldAlgFunctor = new AlgFunctor[Field] {
      def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
        λ[Field[Q, ?] ~> Field[P, ?]] { alg =>
          // XXX: syntax isn't working properly here 
          Field(
            GetterAlgFunctor.amap(f)(implicitly, implicitly)(alg.get), 
            SetterAlgFunctor.amap(f)(implicitly, implicitly)(alg.put))
        }
    }

    implicit def ListPAlgFunctor[Alg[_[_], _] : AlgFunctor] =
      new AlgFunctor[ListP[Alg, ?[_], ?]] {
        def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
          λ[ListP[Alg, Q, ?] ~> ListP[Alg, P, ?]] { alg =>
            ListP(f(alg.algs.map(_.map(_ amap f))))
          }
      }
    
    implicit class AlgFunctorOps[Alg[_[_], _], Q[_]: Functor, A](
        al: Alg[Q, A]) {
      def amap[P[_]: Functor](f: Q ~> P)(implicit AF: AlgFunctor[Alg]) = 
        AF.amap(f)(implicitly, implicitly)(al)
    }
  }
}
 
import Util.{ Lens, _ }, AlgFunctor._


/**
 * Data Model
 */

object Primitive{
  type IntegerP[P[_]]=Field[P,Int]
  type BooleanP[P[_]]=Field[P,Boolean]
  type StringP[P[_]]=Field[P,String]
}

import Primitive._

case class Department[P[_],D](
  self: Field[P,D],
  budget: IntegerP[P])

object Department {
  implicit val DepartmentAlgFunctor = new AlgFunctor[Department] {
    def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
      λ[Department[Q, ?] ~> Department[P, ?]] { alg =>
        Department(alg.self.amap(f), alg.budget.amap(f))
      }
  }
}

case class University[P[_], U, D](
  self: Field[P, U],
  name: StringP[P],
  deps: ListP[Department, P, D])

object University {
  implicit def UniversityAlgFunctor[D] = 
    new AlgFunctor[University[?[_], ?, D]] {
      def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
        λ[University[Q, ?, D] ~> University[P, ?, D]] { alg =>
          University(alg.self.amap(f), alg.name.amap(f), alg.deps.amap(f))
        }
    }
}

trait Logic[P[_], U, D]{

  val univ: University[P, U, D]

  def doubleBudget(implicit M: Monad[P]): P[List[Unit]] =
    univ.deps traverse { _.budget.modify(_*2) }
  
  // should be reused
  def getDepts(implicit M: Monad[P]): P[List[D]] = 
    univ.deps traverse { _.self.get() }
}


/** 
 * Data Layer Interpretation
 */

case class SUniversity(name: String, deps: Map[String, SDepartment])

object SUniversity {

  val nameLn: Lens[SUniversity, String] =
    Lens(_.name, n => _.copy(name = n))
  
  // XXX: I find `d` quite contrived, but it's the only way to decouple the
  // natural transformation while avoiding `Option`al values.
  def depLn(k: String, d: SDepartment): Lens[SUniversity, SDepartment] =
    Lens(const(d), d2 => u => u.copy(deps = u.deps.updated(k, d2)))
}

case class SDepartment(budget: Int)

object SDepartment {
  val budgetLn: Lens[SDepartment, Int] =
    Lens(_.budget, b => _.copy(budget = b))
}

import StateField.refl
import SUniversity._, SDepartment._

object Main extends App {

  val StateDepartment = Department[State[SDepartment, ?], SDepartment](
    refl[SDepartment],
    refl[Int] amap budgetLn)

  val StateUniversity = 
    University[State[SUniversity, ?], SUniversity, SDepartment](
      refl[SUniversity], 
      refl[String] amap nameLn,
      ListP(State.gets(_.deps.toList.map { case (k, d) =>
        StateDepartment amap depLn(k, d)
      })))

  val logic = new Logic[State[SUniversity, ?], SUniversity, SDepartment] {
    val univ = StateUniversity
  }
  
  val math = SDepartment(3000)
  val cs   = SDepartment(5000)
  val univ = SUniversity("urjc", Map("math" -> math, "cs" -> cs))
 
  val univ2 = logic.doubleBudget(implicitly).exec(univ)
  val deps  = logic.getDepts(implicitly).eval(univ2)

  println(s"univ2 = $univ2")
  println(s"deps  = $deps")
}

