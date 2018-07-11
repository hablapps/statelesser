package org.hablapps.statelesser

import shapeless._, labelled._

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

