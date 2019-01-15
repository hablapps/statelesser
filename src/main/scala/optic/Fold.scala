package statelesser
package optic

import scalaz._, Scalaz._

trait Fold[S, A] { self =>

  def foldMap[M: Monoid](f : A => M): S => M

  def >[B](other: Fold[A, B]): Fold[S, B] = new Fold[S, B] {
    def foldMap[M: Monoid](f: B => M): S => M =
      self.foldMap(other.foldMap(f))
  }

  def *[B](other: Fold[S, B]): Fold[S, (A, B)] = new Fold[S, (A, B)] {
    def foldMap[M: Monoid](f: ((A, B)) => M): S => M = { s =>
      self.foldMap(List(_)).apply(s)
        .tuple(other.foldMap(List(_)).apply(s))
        .foldMap(f)
    }
  }
}

