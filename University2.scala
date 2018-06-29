package org.apps.gist
package university

import scalaz._, Scalaz._

/**
 * State algebras
 */

trait Getter[P[_],S]{
  def apply(): P[S]
}

object Getter {
  def apply[P[_], S](get: P[S]): Getter[P, S] =
    new Getter[P, S] { def apply() = get }
}

trait Setter[P[_],S]{
  def apply(t: S): P[Unit]
}

object Setter {
  def apply[P[_], S](put: S => P[Unit]): Setter[P, S] =
    new Setter[P, S] { def apply(s: S) = put(s) }
}

trait Field[P[_],S]{
  val get: Getter[P,S]
  val put: Setter[P,S]

  def modify(f: S => S)(implicit B: Bind[P]): P[Unit] = 
    get() >>= { s => put(f(s)) } 
}

object Field {
  def apply[P[_], S](getter: Getter[P, S], setter: Setter[P, S]) =
    new Field[P, S] { val get = getter; val put = setter }
}

trait ListP[Alg[_[_],_],P[_],S]{
  val apply: P[List[Alg[P,S]]]

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

object ListP {
  def apply[Alg[_[_], _], P[_], S](ap: P[List[Alg[P, S]]]) =
    new ListP[Alg, P, S] { val apply = ap }
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
      new Field[State[S, ?], A] {
        val get = Getter(State.gets(g))
        val put = Setter(a => State(s => (p(a)(s), ())))
      }

    def id[S]: StateField[S, S] = Lens.id[S]
  }

  type Lens[S, A] = State[A, ?] ~> State[S, ?]

  object Lens {
    def id[S]: Lens[S, S] = NaturalTransformation.refl
  }

  implicit def lens2StateField[S, A](ln: Lens[S, A]): StateField[S, A] =
    new Field[State[S, ?], A] {
      val get = Getter(ln(State.get)) 
      val put = Setter(a => ln(State.put(a)))
    }

  implicit def stateField2Lens[S, A](fl: StateField[S, A]): Lens[S, A] =
    λ[State[A, ?] ~> State[S, ?]] { sta =>
      State { s =>
        val (a, x) = sta.run(fl.get().eval(s))
        (fl.put(a).exec(s), x)
      }
    }
  
  // Maps the interpretation of an algebra. 
  trait AlgFunctor[Alg[_[_], _]] {
    def amap[Q[_], P[_], X](f: Q ~> P)(alg: Alg[Q, X]): Alg[P, X]
  }

  object AlgFunctor {

    // XXX: boilerplate instances, consider using Shapeless here?
    
    implicit def GetterAlgFunctor = new AlgFunctor[Getter] {
      def amap[Q[_], P[_], X](f: Q ~> P)(alg: Getter[Q, X]) =
        Getter(f(alg.apply))
    }

    implicit def SetterAlgFunctor = new AlgFunctor[Setter] {
      def amap[Q[_], P[_], X](f: Q ~> P)(alg: Setter[Q, X]) =
        Setter(x => f(alg(x)))
    }

    implicit def FieldAlgFunctor = new AlgFunctor[Field] {
      def amap[Q[_], P[_], X](f: Q ~> P)(alg: Field[Q, X]) =
        Field(alg.get.amap(f), alg.put.amap(f))
    }
    
    implicit class AlgFunctorOps[Alg[_[_], _], Q[_], A](al: Alg[Q, A]) {
      def amap[P[_]](f: Q ~> P)(implicit AF: AlgFunctor[Alg]) = AF.amap(f)(al)
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

trait Department[P[_],D]{
  val self: Field[P,D]
  val budget: IntegerP[P]
}

object Department {
  
  def apply[P[_], D](se: Field[P, D], bu: IntegerP[P]): Department[P, D] =
    new Department[P, D] {
      val self = se
      val budget = bu
    }

  implicit def DepartmentAlgFunctor = new AlgFunctor[Department] {
    def amap[Q[_], P[_], X](f: Q ~> P)(alg: Department[Q, X]) =
      Department(alg.self.amap(f), alg.budget.amap(f))
  }
}

trait University[P[_],U]{
  type D

  val self: Field[P,U]
  val name: StringP[P]
  val deps: ListP[Department,P,D]
}

trait Logic[P[_],U]{
  val univ: University[P,U]

  def doubleBudget(implicit M: Monad[P]): P[List[Unit]] =
    univ.deps traverse { _.budget.modify(_*2) }
  
  // should be reused
  def getDepts(implicit M: Monad[P]): P[List[univ.D]] = 
    univ.deps traverse { _.self.get() }
}


/** 
 * Data Layer Interpretation
 */

case class SUniversity(name: String, departs: Map[String, SDepartment])

case class SDepartment(budget: Int)

object StateDepartment extends Department[State[SDepartment, ?], SDepartment] {
  val self   = StateField.id
  val budget = StateField(_.budget, b => _.copy(budget = b))
}

object StateUniversity extends University[State[SUniversity, ?], SUniversity] {
  
  type D = SDepartment

  val self = StateField.id
  val name = StateField(_.name, n => _.copy(name = n))
  val deps = ListP(State.gets(_.departs.toList.map { case (k, d) =>
    StateDepartment.amap(
      // XXX: the natural transformation is coupled to this interpretation, but
      // if we moved it to an external module, we'd need to pass the value as
      // argument, which seems weird.
      λ[State[SDepartment, ?] ~> State[SUniversity, ?]] { sd =>
        State { u => 
          sd(d).leftMap(d2 => u.copy(departs = u.departs.updated(k, d2)))
        }
      })
    }))
}

object Main extends App {

  val logic = new Logic[State[SUniversity, ?], SUniversity] {
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

