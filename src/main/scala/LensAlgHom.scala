package org.hablapps.statelesser

import scalaz._
import shapeless._

trait LensAlgHom[Alg[_[_], _], P[_], A] {
  type Q[_]
  val alg: Alg[Q, A]
  val hom: Q ~> P

  def fold[B](f: Alg[Q, A] => Q[B]): P[B] =
    hom(f(alg))

  def composeLens[Alg2[_[_], _], B](
      ln: LensAlgHom[Alg2, Q, B]): LensAlgHom.Aux[Alg2, P, ln.Q, B] =
    LensAlgHom[Alg2, P, ln.Q, B](ln.alg, hom compose ln.hom)
}

object LensAlgHom {
  
  type Aux[Alg[_[_], _], P[_], Q2[_], A] = 
    LensAlgHom[Alg, P, A] { type Q[x] = Q2[x] }

  def apply[Alg[_[_], _], P[_], Q2[_], A](
      alg2: Alg[Q2, A],
      hom2: Q2 ~> P): Aux[Alg, P, Q2, A] =
    new LensAlgHom[Alg, P, A] {
      type Q[x] = Q2[x]
      val alg = alg2
      val hom = hom2
    }

  implicit def genLensAlgHom[H, T <: HList, Alg[_[_], _], S, A](implicit 
      ge: GetEvidence[HNil, Alg[State[A, ?], A]],
      fl: MkFieldLens.Aux[S, H, A])
      : GetEvidence[H :: T, LensAlgHom[Alg, State[S, ?], A]] =
    GetEvidence(LensAlgHom(ge(), fl()))

  trait Syntax {
    implicit class LensAlgSyntax[P[_], A](la: LensAlg[P, A]) {
      def get: P[A] = la.fold(_.get)
      def set(a: A): P[Unit] = la.fold(_.put(a))
      def modify(f: A => A): P[Unit] = la.fold(_.modify(f))
    }
  }
}

