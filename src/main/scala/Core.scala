package org.hablapps.statelesser

import Function.const
import scalaz._, Scalaz._
import shapeless._, labelled._, ops.hlist._

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

  // Generates an evidence of identity `Field` (same state & focus) for any
  // label, ignoring the context. We use this for the top `self` attribute.
  implicit def genFieldId[Ctx <: HList, K, A]
      : GetEvidence[K :: Ctx, Field[State[A, ?], A]] =
    GetEvidence(field[K](refl[K :: Ctx, A].apply)) 

  // Generates an evidence of `Field`, as long as we have a lens that respects
  // the context path. We use this as base case to fulfill most of `Field`s.
  implicit def genField[Ctx <: HList, K, P[_]: Functor, A](implicit 
      ln: FieldType[K :: Ctx, State[A, ?] ~> P])
      : GetEvidence[K :: Ctx, Field[P, A]] =
    GetEvidence(refl[K :: Ctx, A].apply amap ln)
}

object Primitive{
  type IntegerP[P[_]]=Field[P,Int]
  type BooleanP[P[_]]=Field[P,Boolean]
  type StringP[P[_]]=Field[P,String]
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

    implicit def refl[Ctx <: HList, S]: GetEvidence[Ctx, StateField[S, S]] = 
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
  //
  // XXX: we're not using this algebra right now, but we keep it since I find
  // it interesting anyway, so maybe we could use it in the future.
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

    implicit class AlgFunctorOps[Alg[_[_], _], Q[_]: Functor, A](
        al: Alg[Q, A]) {
      def amap[P[_]: Functor](f: Q ~> P)(implicit AF: AlgFunctor[Alg]) = 
        AF.amap(f)(implicitly, implicitly)(al)
    }
  }

  // Just a wrapper of values of type `A` which carries a phantom `Ctx`, that
  // represents the location of the value. 
  trait GetEvidence[Ctx <: HList, A] {
    def apply: A
  }

  object GetEvidence {

    def apply[Ctx <: HList, A](implicit 
        ge: GetEvidence[Ctx, A]): GetEvidence[Ctx, A] = 
      ge
  
    def apply[Ctx <: HList, A](a: A): GetEvidence[Ctx, A] =
      new GetEvidence[Ctx, A] { def apply = a }
  
    // We can always get an evidence of `HNil`.
    implicit def emptyProduct[Ctx <: HList]: GetEvidence[Ctx, HNil] = 
      GetEvidence(HNil)

    // Returns an evidence of `H :: T` by searching for an implicit evidende of
    // `H` and recursing over `T`. While searching for the `H` evidence, the
    // context is extended with the label, to keep track of the full context.
    implicit def product[Ctx <: HList, K, H, T <: HList](implicit
        hev: Lazy[GetEvidence[K :: Ctx, H]],
        tev: GetEvidence[Ctx, T]): GetEvidence[Ctx, FieldType[K, H] :: T] =
      GetEvidence[Ctx, FieldType[K, H] :: T](
        field[K](hev.value.apply) :: tev.apply)

    implicit def genericGetEvidence[Ctx <: HList, A, R](implicit
        generic: LabelledGeneric.Aux[A, R],
        rInstance: Lazy[GetEvidence[Ctx, R]]): GetEvidence[Ctx, A] =
      GetEvidence(generic.from(rInstance.value.apply))
  }

  // Provides a lens tagged with a context.
  trait TaggedLens[Ctx <: HList, S, A] {
    val getTaggedLens: FieldType[Ctx, Lens[S, A]]
  }

  object TaggedLens {

    def apply[Ctx <: HList, S, A](implicit 
        tl: TaggedLens[Ctx, S, A]): TaggedLens[Ctx, S, A] = 
      tl

    def apply[Ctx <: HList, S, A](
        ft: FieldType[Ctx, Lens[S, A]]): TaggedLens[Ctx, S, A] =
      new TaggedLens[Ctx, S, A] { val getTaggedLens = ft }

    // Provides a lens pointing at the head of this `HList`, as long as the
    // label of the head is compliant with the context, I mean, the label is
    // the unique tag standing in the context list.
    implicit def productHead[K, H, T <: HList]
        : TaggedLens[K :: HNil, FieldType[K, H] :: T, H] =
      TaggedLens(field[K :: HNil](Lens[FieldType[K, H] :: T, H](
        _.head, h2 => field[K](h2) :: _.tail)))

    // Ad hoc evidence to deal with `'self`s. The problem is that there's no
    // attribute named `self` in the case class, but we need to fulfill it for
    // the data layer. In fact, the lens that we are interested in for this
    // case is the one that acts as nexus between classes. Thereby, we need to
    // stop one step early, before searching for `'self`.  
    implicit def productSelf[K, H, T <: HList] =
      TaggedLens(field[K :: Witness.`'self`.T :: HNil](
        Lens[FieldType[K, H] :: T, H](
          _.head, h2 => field[K](h2) :: _.tail)))
    
    // Provides a lens that points at some component in the resulting tail.
    implicit def productTail[Ctx <: HList, K, H, A, T <: HList](implicit
        ev: TaggedLens[K :: Ctx, T, A]): TaggedLens[K :: Ctx, H :: T, A] =
      TaggedLens(field[K :: Ctx](Lens[H :: T, A](
        l => ev.getTaggedLens(State.get).eval(l.tail), 
        a2 => l => l.head :: ev.getTaggedLens(State.put(a2)).exec(l.tail))))

    // Provides a lens that points at a subcomponent of this head, which we can
    // see as lens composition, since we are pointing at a nested value.
    implicit def hcons3[Ctx <: HList, J, K, H, A, T <: HList](implicit
        ev: TaggedLens[Ctx, H, A])
        : TaggedLens[K :: Ctx, FieldType[J, H] :: T, A] =
      ev.getTaggedLens |> (ln =>
        TaggedLens(field[K :: Ctx](Lens[FieldType[J, H] :: T, A](
          l => ln(State.get).eval(l.head),
          a2 => l => field[J](ln(State.put(a2)).exec(l.head)) :: l.tail))))

    implicit def genericTaggedLens[C, R, Ctx <: HList, A](implicit
        generic: LabelledGeneric.Aux[C, R],
        rInstance: Lazy[TaggedLens[Ctx, R, A]]): TaggedLens[Ctx, C, A] =
      rInstance.value.getTaggedLens |> (ln => TaggedLens(field[Ctx](Lens[C, A](
        c => ln(State.get).eval(generic.to(c)),
        a2 => c => generic.from(ln(State.put(a2)).exec(generic.to(c)))))))
  }

  implicit def getTaggedLens[Ctx <: HList, Ctx2 <: HList, S, A](implicit 
      rv: Reverse.Aux[Ctx, Ctx2],
      tl: TaggedLens[Ctx2, S, A]): FieldType[Ctx, Lens[S, A]] =
    field[Ctx](tl.getTaggedLens)
}
 
