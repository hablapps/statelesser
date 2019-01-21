package monocle

import scalaz._, Scalaz._

case class AffineFold[S, A](pre: S => Option[A]) {

  def >[B](other: AffineFold[A, B]): AffineFold[S, B] =
    AffineFold(s => pre(s) >>= other.pre)

  def *[B](other: AffineFold[S, B]): AffineFold[S, (A, B)] =
    AffineFold(s => pre(s).tuple(other.pre(s)))

  def asFold: Fold[S, A] = new Fold[S, A] {
    def foldMap[M: Monoid](f: A => M)(s: S): M = pre(s).foldMap(f)
  }
}

object AffineFold {
  def filtered[S](p: Getter[S, Boolean]): AffineFold[S, S] =
    AffineFold(s => if (p.get(s)) Some(s) else None)
}

