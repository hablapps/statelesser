package org.hablapps.statesome

import Function.const
import scalaz._, Scalaz._
import shapeless._, shapeless.syntax.singleton._, labelled._

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

    def lensId[S]: Lens[S, S] = NaturalTransformation.refl
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

    def apply[Alg[_[_], _]](implicit ev: AlgFunctor[Alg]): AlgFunctor[Alg] = 
      ev

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
  
    implicit def genericGetEvidence2[K, Alg[_[_], _], P[_], Q[_], S, R](implicit
        algf: AlgFunctor[Alg],
        nat: FieldType[K, Q ~> P],
        f0: Functor[Q],
        f1: Functor[P],
        generic: LabelledGeneric.Aux[Alg[Q, S], R],
        rInstance: Lazy[GetEvidence[R]]): GetEvidence[FieldType[K, Alg[P, S]]] =
      GetEvidence(field[K](algf.amap(nat)(f0, f1)(
        genericGetEvidence[Alg[Q, S], R].apply)))
  }

  trait TaggedLens[K, S, A] {
    val getTaggedLens: FieldType[K, Lens[S, A]]
  }

  object TaggedLens extends TaggedLens1 {

    def apply[K, S, A](implicit tl: TaggedLens[K, S, A]): TaggedLens[K, S, A] = 
      tl

    def apply[K, S, A](ft: FieldType[K, Lens[S, A]]): TaggedLens[K, S, A] =
      new TaggedLens[K, S, A] { val getTaggedLens = ft }

    implicit def hcons[K, H, T <: HList]: TaggedLens[K, FieldType[K, H] :: T, H] =
      TaggedLens(field[K](Lens[FieldType[K, H] :: T, H](
        _.head, h2 => field[K](h2) :: _.tail)))

     implicit def genericTaggedLens[C, R, K, A](implicit
        generic: LabelledGeneric.Aux[C, R],
        rInstance: Lazy[TaggedLens[K, R, A]])
        : TaggedLens[K, C, A] =
      rInstance.value.getTaggedLens |> (ln => TaggedLens(field[K](Lens[C, A](
        c => ln(State.get).eval(generic.to(c)),
        a2 => c => generic.from(ln(State.put(a2)).exec(generic.to(c)))))))
  }

  trait TaggedLens1 {
    implicit def hcons2[K, H, A, T <: HList](implicit
        ev: TaggedLens[K, T, A]): TaggedLens[K, H :: T, A] =
      TaggedLens(field[K](Lens[H :: T, A](
        l => ev.getTaggedLens(State.get).eval(l.tail), 
        a2 => l => l.head :: ev.getTaggedLens(State.put(a2)).exec(l.tail))))
  }

  implicit def getTaggedLensId[K, S]: FieldType[K, Lens[S, S]] =
    field[K](Lens.lensId[S])

  implicit def getTaggedLens[K, S, A](implicit 
      tl: TaggedLens[K, S, A]): FieldType[K, Lens[S, A]] =
    tl.getTaggedLens
}
 
