package org.hablapps

import scalaz._
import shapeless._, ops.hlist._
import naturally._

package object statelesser {

  type LensAlg[P[_], A] = LensAlgHom[MonadState, P, A]

  object LensAlg {
    def apply[P[_], A](hom: State[A, ?] ~> P): LensAlg[P, A] =
      LensAlgHom[MonadState, P, State[A, ?], A](implicitly, hom)
  }

  implicit class LensSyntax[P[_], A](la: LensAlg[P, A]) {
    def get: P[A] = la.fold(_.get)
    def set(a: A): P[Unit] = la.fold(_.put(a))
    def modify(f: A => A): P[Unit] = la.fold(_.modify(f))
  }

  implicit def genLensAlg[
        Rev <: HList, Ctx <: HList : Reverse.Aux[Rev, ?], S, A](implicit 
      ev: DeepLens.Aux[S, Ctx, A]): GetEvidence[Rev, LensAlg[State[S, ?], A]] =
    GetEvidence(LensAlg(ev()))

  implicit def genLensAlgHom[
        Rev <: HList, Ctx <: HList : Reverse.Aux[Rev, ?], 
        Alg[_[_], _], S, A](implicit 
      ge: GetEvidence[HNil, Alg[State[A, ?], A]],
      dl: DeepLens.Aux[S, Ctx, A])
      : GetEvidence[Rev, LensAlgHom[Alg, State[S, ?], A]] =
    GetEvidence(LensAlgHom(ge.apply, dl()))

  def make[A](implicit ev: GetEvidence[HNil, A]): A =
    ev.apply
}

