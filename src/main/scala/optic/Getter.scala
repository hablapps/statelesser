package statelesser
package optic

import Function.const

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

