package org.hablapps.statelesser

import scalaz._, Scalaz._
import shapeless._

trait TraversalAlgHom[Alg[_[_], _], P[_], A] {
  type Q[_]
  val alg: Alg[Q, A]
  val hom: Q ~> ListT[P, ?]

  def apply[B](f: InitialSAlg[Alg, A, B]): P[List[B]] =
    hom(f(alg)).run

  def fold[B](f: InitialSAlg[Alg, A, B])(implicit 
      F: Functor[P],
      M: Monoid[B]): P[B] =
    apply(f).map(_.suml)

  def filter[B](
      p: InitialSAlg[Alg, A, Boolean],
      g: InitialSAlg[Alg, A, B])(implicit
      F: Functor[P],
      M: Monad[Q]): P[List[B]] =
    hom(p(alg).ifM(
      g(alg).map(_.point[List]), 
      List.empty[B].point[Q])).run.map(_.suml)

  def composeLens[Alg2[_[_], _], B](
      ln: LensAlgHom[Alg2, Q, B]): TraversalAlgHom.Aux[Alg2, P, ln.Q, B] =
    TraversalAlgHom[Alg2, P, ln.Q, B](ln.alg, hom compose ln.hom)

  def composeTraversal[Alg2[_[_], _], B](
      tr: TraversalAlgHom[Alg2, Q, B])(implicit
      F: Functor[P]): TraversalAlgHom.Aux[Alg2, P, tr.Q, B] =
    TraversalAlgHom[Alg2, P, tr.Q, B](tr.alg, 
      λ[ListT[Q, ?] ~> ListT[P, ?]](ltq => ListT(hom(ltq.run).run.map(_.join))) 
        compose tr.hom)
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

  import InitialSAlg._

  trait Syntax {
    implicit class Syntax[P[_], A](ta: TraversalAlg[P, A]) {
      def foldMap[B: Monoid](f: A => B)(implicit F: Functor[P]): P[B] = 
        ta.fold(getsMS(f))
      def modify(f: A => A)(implicit F: Functor[P]): P[Unit] = 
        ta(modMS(f)).void
      def getAll: P[List[A]] = ta(getMS)
    }
  }
}

