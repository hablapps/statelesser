package org.hablapps.statelesser

import scalaz._
import shapeless._, labelled._

trait GetEvidence[Ctx <: HList, A] {
  def apply(): A
}

object GetEvidence {

  def apply[Ctx <: HList, A](implicit 
      ge: GetEvidence[Ctx, A]): GetEvidence[Ctx, A] = 
    ge

  def apply[Ctx <: HList, A](a: A): GetEvidence[Ctx, A] =
    new GetEvidence[Ctx, A] { def apply = a }

  implicit def monadStateGetEvidence[S](implicit 
      ev: MonadState[State[S, ?], S])
      : GetEvidence[HNil, MonadState[State[S, ?], S]] =
    GetEvidence(ev)

  implicit def emptyProduct[Ctx <: HList]: GetEvidence[Ctx, HNil] = 
    GetEvidence(HNil)
    
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

