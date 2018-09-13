package org.hablapps

import scalaz._, Scalaz._
import shapeless._

package object statelesser extends LensAlgHom.Syntax
    with TraversalAlgHom.Syntax {

  type LensAlg[P[_], A] = LensAlgHom[MonadState, P, A]

  object LensAlg {
    def apply[P[_], A](hom: State[A, ?] ~> P): LensAlg[P, A] =
      LensAlgHom[MonadState, P, State[A, ?], A](implicitly, hom)
  }

  type TraversalAlg[P[_], A] = TraversalAlgHom[MonadState, P, A]

  object TraversalAlg {
    def apply[P[_], A](hom: State[A, ?] ~> ListT[P, ?]): TraversalAlg[P, A] =
      TraversalAlgHom[MonadState, P, State[A, ?], A](implicitly, hom)
  }

  // XXX: no such method in scalaz?
  implicit class FunctorTuple[F[_]: Functor, A, B](fab: F[(A, B)]) {
    val unzip: (F[A], F[B]) = (fab.map(_._1), fab.map(_._2))
  }
  
  implicit def slensToLens[S, A](
      ln: shapeless.Lens[S, A]): naturally.Lens[S, A] =
    λ[State[A, ?] ~> State[S, ?]] { sa => 
      State(s => sa(ln.get(s)).leftMap(ln.set(s)))
    }

  implicit def slensToWeakTraversal[F[_]: Traverse, S, A](
      ln: shapeless.Lens[S, F[A]]): State[A, ?] ~> ListT[State[S, ?], ?] =
    λ[State[A, ?] ~> ListT[State[S, ?], ?]] { sa =>
      ListT(State(s => ln.get(s).map(sa.run).unzip.bimap(ln.set(s), _.toList)))
    }

  def make[A](implicit ev: GetEvidence[HNil, A]): A = ev.apply()
}

