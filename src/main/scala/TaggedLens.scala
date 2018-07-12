package org.hablapps.statelesser

import scalaz._, Scalaz._
import shapeless._, labelled._
import Util.Lens

trait Shapelens[S, Ctx <: HList] {
  type A
  val getLens: Lens[S, A]
}

object Shapelens {

  type Aux[S, Ctx <: HList, A2] = Shapelens[S, Ctx] { type A = A2 }
  
  def apply[S, Ctx <: HList](implicit 
      tl: Shapelens[S, Ctx]): Aux[S, Ctx, tl.A] = 
    tl

  def apply[S, Ctx <: HList, A2](ln: Lens[S, A2]): Aux[S, Ctx, A2] =
    new Shapelens[S, Ctx] { type A = A2; val getLens = ln }

  implicit def productHead[K, H, T <: HList]
      : Aux[FieldType[K, H] :: T, K :: HNil, H] =
    apply(Lens[FieldType[K, H] :: T, H](
      _.head, h2 => field[K](h2) :: _.tail))

  implicit def productSelf[J, H, T <: HList]
      : Aux[FieldType[J, H] :: T, J :: Witness.`'self`.T :: HNil, H] =
    apply(Lens[FieldType[J, H] :: T, H](
      _.head, h2 => field[J](h2) :: _.tail))
  
  implicit def productTail[Ctx <: HList, J, K: J =:!= ?, H, A, T <: HList](implicit
      tl: Aux[T, K :: Ctx, A]): Aux[FieldType[J, H] :: T, K :: Ctx, A] =
    apply(Lens[FieldType[J, H] :: T, A](
      l => tl.getLens(State.get).eval(l.tail), 
      a2 => l => l.head :: tl.getLens(State.put(a2)).exec(l.tail)))

  implicit def productDepth[Ctx <: HList, K, H, A, T <: HList](implicit
      tl: Aux[H, Ctx, A])
      : Aux[FieldType[K, H] :: T, K :: Ctx, A] =
    apply(Lens[FieldType[K, H] :: T, A](
      l => tl.getLens(State.get).eval(l.head),
      a2 => l => field[K](tl.getLens(State.put(a2)).exec(l.head)) :: l.tail))

  implicit def genericShapelens[C, R, Ctx <: HList, A](implicit
      generic: LabelledGeneric.Aux[C, R],
      rInstance: Lazy[Aux[R, Ctx, A]]): Aux[C, Ctx, A] =
    rInstance.value.getLens |> (ln => apply(Lens[C, A](
      c => ln(State.get).eval(generic.to(c)),
      a2 => c => generic.from(ln(State.put(a2)).exec(generic.to(c))))))
}

