package statelesser
package core
package interpreter

import Function.const
import scalaz._, Scalaz._
import monocle._, function.{Field1, Field2}, extension._

class Metacircular extends Statelesser[Id] {

  def flVert[S, A, B](u: Fold[S, A], d: Fold[A, B]): Fold[S, B] =
    u composeFold d

  def flHori[S, A, B](l: Fold[S, A], r: Fold[S, B]): Fold[S, (A, B)] =
    l * r

  def gtVert[S, A, B](u: Getter[S, A], d: Getter[A, B]): Getter[S, B] =
    u composeGetter d

  def gtHori[S, A, B](l: Getter[S, A], r: Getter[S, B]): Getter[S, (A, B)] =
    l * r

  def aflVert[S, A, B](
      u: AffineFold[S, A], 
      d: AffineFold[A, B]): AffineFold[S, B] =
    u > d

  def aflHori[S, A, B](
      l: AffineFold[S, A],
      r: AffineFold[S, B]): AffineFold[S, (A, B)] =
    l * r

  def filtered[S](p: Getter[S, Boolean]): AffineFold[S, S] =
    AffineFold.filtered(p)

  def sub: Getter[(Int, Int), Int] = Getter { case (x, y) => x - y }

  def greaterThan: Getter[(Int, Int), Boolean] = Getter { case (x, y) => x > y }

  def first[A, B]: Getter[(A, B), A] = 
    Field1.tuple2Field1[A, B].first.asGetter

  def second[A, B]: Getter[(A, B), B] =
    Field2.tuple2Field2[A, B].second.asGetter

  def likeInt[S](i: Int): Getter[S, Int] = Getter(const(i))

  def likeBool[S](b: Boolean): Getter[S, Boolean] = Getter(const(b))

  def likeStr[S](s: String): Getter[S, String] = Getter(const(s))

  def id[S]: Getter[S, S] = Getter.id

  def not: Getter[Boolean, Boolean] = Getter(!(_))

  def gtAsAfl[S, A](gt: Getter[S, A]): AffineFold[S, A] = gt.asAffineFold

  def aflAsFl[S, A](afl: AffineFold[S, A]): Fold[S, A] = afl.asFold
}

