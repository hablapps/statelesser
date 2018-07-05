package org.apps.gist
package university

import Function.const
import scalaz._, Scalaz._
import shapeless._, labelled._, shapeless.syntax.singleton._

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

object Field {

  import Util._, StateField._, AlgFunctor._
  
  implicit def genField[K, P[_]: Functor, A](implicit 
      ln: FieldType[K, State[A, ?] ~> P]): GetEvidence[FieldType[K, Field[P, A]]] =
    GetEvidence(field[K](refl[A].apply amap ln))
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

  type ReaderGetter[S, A] = Getter[Reader[S, ?], A]

  object ReaderGetter {
    
    def apply[S, A](g: S => A): ReaderGetter[S, A] =
      Getter[Reader[S, ?], A](Reader(g))

    implicit def refl[S]: ReaderGetter[S, S] = apply(identity)
  }
  
  type StateField[S, A] = Field[State[S, ?], A]

  object StateField {

    def apply[S, A](g: S => A, p: A => S => S): StateField[S, A] =
      Field[State[S, ?], A](
        Getter(State.gets(g)),
        Setter(a => State(s => (p(a)(s), ()))))

    implicit def refl[S]: GetEvidence[StateField[S, S]] = 
      GetEvidence(apply[S, S](identity, const))
  }

  type Lens[S, A] = State[A, ?] ~> State[S, ?]

  object Lens {

    def apply[S, A](get: S => A, set: A => S => S): Lens[S, A] =
      λ[State[A, ?] ~> State[S, ?]] { sa =>
        State(s => sa(get(s)).leftMap(set(_)(s)))
      }
  }
 
  def lensId[S]: Lens[S, S] = NaturalTransformation.refl
  
  implicit def compNat[P[_], Q[_], R[_]](implicit
      nat1: Q ~> P, 
      nat2: R ~> Q): R ~> P =
    nat1 compose nat2

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

    // implicit def ListPAlgFunctor[Alg[_[_], _]: AlgFunctor] =
    //   new AlgFunctor[ListP[Alg, ?[_], ?]] {
    //     def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
    //       λ[ListP[Alg, Q, ?] ~> ListP[Alg, P, ?]] { alg =>
    //         ListP(f(alg.algs.map(_.map(_ amap f))))
    //       }
    //   }
    
    implicit class AlgFunctorOps[Alg[_[_], _], Q[_]: Functor, A](
        al: Alg[Q, A]) {
      def amap[P[_]: Functor](f: Q ~> P)(implicit AF: AlgFunctor[Alg]) = 
        AF.amap(f)(implicitly, implicitly)(al)
    }
  }

  // Shapeless class to automate instances
  
  trait GetEvidence[A] {
    def apply: A
  }

  object GetEvidence {

    def apply[A](implicit ge: GetEvidence[A]): GetEvidence[A] = ge
  
    def apply[A](a: A): GetEvidence[A] =
      new GetEvidence[A] { def apply = a }
    
    implicit val hnil: GetEvidence[HNil] = GetEvidence(HNil)

    implicit def hcons[K, H, T <: HList](implicit
        // witness: Witness.Aux[K], 
        hev: Lazy[GetEvidence[FieldType[K, H]]],
        tev: GetEvidence[T]): GetEvidence[FieldType[K, H] :: T] =
      GetEvidence[FieldType[K, H] :: T](
        hev.value.apply :: tev.apply)

    implicit def genericGetEvidence[A, R](implicit
        generic: LabelledGeneric.Aux[A, R],
        rInstance: Lazy[GetEvidence[R]]): GetEvidence[A] =
      GetEvidence[A](generic.from(rInstance.value.apply))
  
    // XXX: perhaps this isn't the best idea, but it's what I needed while
    // debugging implicits with *splain*.
    implicit def genericGetEvidence2[K, A, R](implicit
        generic: LabelledGeneric.Aux[A, R],
        rInstance: Lazy[GetEvidence[R]]): GetEvidence[FieldType[K, A]] =
      GetEvidence[FieldType[K, A]](field[K](generic.from(rInstance.value.apply)))
  }
}
 
import Util.{ Lens, _ }, AlgFunctor._, GetEvidence._


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

  def instance[P[_], D](implicit 
      ge: GetEvidence[Department[P, D]]): Department[P, D] = 
    ge.apply

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
  math: Department[P, D])

object University {

  def instance[P[_], U, D](implicit 
      ge: GetEvidence[University[P, U, D]]): University[P, U, D] =
    ge.apply

  implicit def UniversityAlgFunctor[D] = 
    new AlgFunctor[University[?[_], ?, D]] {
      def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
        λ[University[Q, ?, D] ~> University[P, ?, D]] { alg =>
          University(alg.self.amap(f), alg.name.amap(f), alg.math.amap(f))
        }
    }
}

case class City[P[_], C, U, D](
  self: Field[P, C],
  popu: Field[P, Double],
  univ: University[P, U, D])

object City {
  
  def instance[P[_], C, U, D](implicit
      ge: GetEvidence[City[P, C, U, D]]): City[P, C, U, D] =
    ge.apply
}

case class Logic[P[_], U, D](univ: University[P, U, D]) {

  def doubleBudget(implicit M: Monad[P]): P[Unit] =
    univ.math.budget.modify(_ * 2)
  
  // should be reused
  def getMathDep: P[D] = 
    univ.math.self.get()
}


/** 
 * Data Layer Interpretation
 */

case class SCity(population: Double, univ: SUniversity)

object SCity {
 
  val popuLn: Lens[SCity, Double] =
    Lens(_.population, p => _.copy(population = p))

  val univLn: Lens[SCity, SUniversity] =
    Lens(_.univ, u => _.copy(univ = u))
}

case class SUniversity(name: String, math: SDepartment)

object SUniversity {

  val nameLn: Lens[SUniversity, String] =
    Lens(_.name, n => _.copy(name = n))
  
  val mathLn: Lens[SUniversity, SDepartment] =
    Lens(_.math, d => _.copy(math = d))
}

case class SDepartment(budget: Int)

object SDepartment {

  val budgetLn = Lens[SDepartment, Int](_.budget, b => _.copy(budget = b))
}

import SCity._, SUniversity._, SDepartment._

object Main extends App {

  object TestDepartment {
    implicit val self = 'self ->> lensId[SDepartment]
    implicit val budg = 'budget ->> budgetLn

    val StateDepartment =
      Department.instance[State[SDepartment, ?], SDepartment]
  }

  object TestCity {
    implicit val self = 'self ->> lensId[SCity]
    implicit val popu = 'popu ->> popuLn
    implicit val univ = 'univ ->> univLn
    implicit val sel1 = 'self ->> (univ compose lensId[SUniversity])
    implicit val name = 'name ->> (univ compose nameLn)
    implicit val math = 'math ->> (univ compose mathLn)
    implicit val sel2 = 'self ->> (math compose lensId[SDepartment])
    implicit val budg = 'budget ->> (math compose budgetLn)

    val StateCity = 
      City.instance[State[SCity, ?], SCity, SUniversity, SDepartment]
  }

  // University example (includes some logic)
  
  implicit val sel1 = 'self ->> lensId[SUniversity]
  implicit val name = 'name ->> nameLn
  implicit val matx = 'math ->> mathLn
  implicit val sel2 = 'self ->> (mathLn compose lensId[SDepartment])
  implicit val budg = 'budget ->> (mathLn compose budgetLn)

  val StateUniversity = 
    University.instance[State[SUniversity, ?], SUniversity, SDepartment]

  val logic = Logic(StateUniversity)
  
  val math = SDepartment(3000)
  val univ = SUniversity("urjc", math)

  val univ2 = logic.doubleBudget(implicitly).exec(univ)
  val deps  = logic.getMathDep.eval(univ2)

  println(s"univ2 = $univ2")
  println(s"deps  = $deps")
}

