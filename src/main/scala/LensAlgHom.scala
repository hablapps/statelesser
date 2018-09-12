package org.hablapps.statelesser

import scalaz._

trait LensAlgHom[Alg[_[_], _], P[_], A] {
  type Q[_]
  val alg: Alg[Q, A]
  def apply(): Q ~> P

  def fold[B](f: Alg[Q, A] => Q[B]): P[B] =
    apply()(f(alg))

  def composeLens[Alg2[_[_], _], B](
      ln: LensAlgHom[Alg2, Q, B]): LensAlgHom.Aux[Alg2, P, ln.Q, B] =
    LensAlgHom[Alg2, P, ln.Q, B](ln.alg, apply() compose ln())
}

object LensAlgHom {
  
  type Aux[Alg[_[_], _], P[_], Q2[_], A] = 
    LensAlgHom[Alg, P, A] { type Q[x] = Q2[x] }

  def apply[Alg[_[_], _], P[_], Q2[_], A](
      alg2: Alg[Q2, A],
      app2: Q2 ~> P): Aux[Alg, P, Q2, A] =
    new LensAlgHom[Alg, P, A] {
      type Q[x] = Q2[x]
      val alg = alg2
      def apply() = app2
    }
}

