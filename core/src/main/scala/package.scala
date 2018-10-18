package org.hablapps

import monocle.Lens

package object `statelesser` {

  type Id[A] = A

  implicit class LensOps[S, A](ln: Lens[S, A]) {
    def *[B](other: Lens[S, B]): Lens[S, (A, B)] =
      Lens[S, (A, B)](
        s => (ln.get(s), other.get(s)))(
        ab => s => other.set(ab._2)(ln.set(ab._1)(s)))
  }
}

