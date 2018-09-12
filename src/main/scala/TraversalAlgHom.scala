package org.hablapps.statelesser

import scalaz._, Scalaz._
import shapeless._

trait TraversalAlgHom[Alg[_[_], _], P[_], A] {
  type Q[_]
  val alg: Alg[Q, A]
  val hom: Q ~> ListT[P, ?]

  def apply[B](f: Alg[Q, A] => Q[B]): P[List[B]] =
    hom(f(alg)).run

  def fold[B](f: Alg[Q, A] => Q[B])(implicit 
      F: Functor[P],
      M: Monoid[B]): P[B] =
    apply(f).map(_.suml)

  def composeLens[Alg2[_[_], _], B](
      ln: LensAlgHom[Alg2, Q, B]): TraversalAlgHom.Aux[Alg2, P, ln.Q, B] =
    TraversalAlgHom[Alg2, P, ln.Q, B](ln.alg, hom compose ln.hom)
}

object TraversalAlgHom {

  type Aux[Alg[_[_], _], P[_], Q2[_], A] = 
    TraversalAlgHom[Alg, P, A] { type Q[x] = Q2[x] }

  def apply[Alg[_[_], _], P[_], Q2[_], A](
      alg2: Alg[Q2, A],
      hom2: Q2 ~> ListT[P, ?]): Aux[Alg, P, Q2, A] =
    new TraversalAlgHom[Alg, P, A] {
      type Q[x] = Q2[x]
      val alg = alg2
      val hom = hom2
    }

  implicit def genTraversalAlgHom[H, T <: HList, Alg[_[_], _], S, A](implicit 
      ge: GetEvidence[HNil, Alg[State[A, ?], A]],
      fl: MkFieldLens.Aux[S, H, List[A]])
      : GetEvidence[H :: T, TraversalAlgHom[Alg, State[S, ?], A]] =
    GetEvidence(TraversalAlgHom(ge(), fl()))

  trait Syntax {
    implicit class Syntax[P[_], A](ta: TraversalAlg[P, A]) {
      def foldMap[B: Monoid](f: A => B)(implicit F: Functor[P]): P[B] = 
        ta.fold(_.gets(f))
      def modify(f: A => A)(implicit F: Functor[P]): P[Unit] = 
        ta(_.modify(f)).void
      def getAll: P[List[A]] = ta(_.get)
    }
  }
}

