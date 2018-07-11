package org.hablapps.statelesser

import scalaz._, Scalaz._
import shapeless._, labelled._
import Util.Lens

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
