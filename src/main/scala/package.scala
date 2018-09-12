package org.hablapps

import scalaz._
import shapeless._

package object statelesser extends LensAlgHom.Syntax {

  type LensAlg[P[_], A] = LensAlgHom[MonadState, P, A]

  object LensAlg {
    def apply[P[_], A](hom: State[A, ?] ~> P): LensAlg[P, A] =
      LensAlgHom[MonadState, P, State[A, ?], A](implicitly, hom)
  }

  def make[A](implicit ev: GetEvidence[HNil, A]): A = ev.apply()
}

