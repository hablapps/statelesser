package statelesser

import Function.const
import scalaz._, Scalaz._

case class Getter[S, A](get : S => A) {
  
  def >[B](other: Getter[A, B]): Getter[S, B] =
    Getter(other.get compose get)

  def *[B](other: Getter[S, B]): Getter[S, (A, B)] =
    Getter(s => (get(s), other.get(s)))

  def asAffineFold: AffineFold[S, A] =
    AffineFold(s => Some(get(s)))
}

object Getter {

  def first[A, B]: Getter[(A, B), A] = Getter(_._1)

  def second[A, B]: Getter[(A, B), B] = Getter(_._2)

  def like[S, A](a: A): Getter[S, A] = Getter(const(a))

  def id[S]: Getter[S, S] = Getter(identity)

  def sub: Getter[(Int, Int), Int] = Getter { case (x, y) => x - y }

  def not: Getter[Boolean, Boolean] = Getter(!(_))

  def gt: Getter[(Int, Int), Boolean] = Getter { case (x, y) => x > y }
}

case class AffineFold[S, A](pre: S => Option[A]) {

  def >[B](other: AffineFold[A, B]): AffineFold[S, B] =
    AffineFold(s => pre(s) >>= other.pre)

  def *[B](other: AffineFold[S, B]): AffineFold[S, (A, B)] =
    AffineFold(s => pre(s).tuple(other.pre(s)))

  def asFold: Fold[S, A] = new Fold[S, A] {
    def foldMap[M: Monoid](f: A => M): S => M = pre(_).foldMap(f)
  }
}

object AffineFold {
  
  def filtered[S](p: Getter[S, Boolean]): AffineFold[S, S] =
    AffineFold(s => if (p.get(s)) Some(s) else None)
}

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

