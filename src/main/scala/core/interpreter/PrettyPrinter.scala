package statelesser
package core
package interpreter

import scalaz._
import optic._

class PrettyPrinter extends Statelesser[Const[String, ?]] {

  def flVert[S, A, B](
      u: Const[String, Fold[S, A]],
      d: Const[String, Fold[A, B]]): Const[String, Fold[S, B]] =
    Const(s"${u.getConst} > ${d.getConst}")

  def flHori[S, A, B](
      l: Const[String, Fold[S, A]],
      r: Const[String, Fold[S, B]]): Const[String, Fold[S, (A, B)]] =
    Const(s"${l.getConst} * ${r.getConst}")

  def gtVert[S, A, B](
      u: Const[String, Getter[S, A]],
      d: Const[String, Getter[A, B]]): Const[String, Getter[S, B]] =
    Const(s"${u.getConst} > ${d.getConst}")

  def gtHori[S, A, B](
      l: Const[String, Getter[S, A]],
      r: Const[String, Getter[S, B]]): Const[String, Getter[S, (A, B)]] =
    Const(s"(${l.getConst} * ${r.getConst})")

  def aflVert[S, A, B](
      u: Const[String, AffineFold[S, A]],
      d: Const[String, AffineFold[A, B]]): Const[String, AffineFold[S, B]] =
    Const(s"${u.getConst} > ${d.getConst}")

  def aflHori[S, A, B](
      l: Const[String, AffineFold[S, A]],
      r: Const[String, AffineFold[S, B]]): Const[String, AffineFold[S, (A, B)]] =
    Const(s"${l.getConst} * ${r.getConst}")

  def filtered[S](
      p: Const[String, Getter[S, Boolean]]): Const[String, AffineFold[S, S]] =
    Const(s"filtered(${p.getConst})")

  def sub: Const[String, Getter[(Int, Int), Int]] =
    Const("sub")

  def greaterThan: Const[String, Getter[(Int, Int), Boolean]] =
    Const("greaterThan")

  def first[A, B]: Const[String, Getter[(A, B), A]] =
    Const("first")

  def second[A, B]: Const[String, Getter[(A, B), B]] =
    Const("second")

  def likeInt[S](i: Int): Const[String, Getter[S, Int]] =
    Const(s"likeInt(${i.toString})")

  def likeBool[S](b: Boolean): Const[String, Getter[S, Boolean]] =
    Const(s"likeBool(${b.toString})")

  def likeStr[S](s: String): Const[String, Getter[S, String]] =
    Const(s"""likeStr("$s")""")

  def id[S]: Const[String, Getter[S, S]] =
    Const("id")

  def not: Const[String, Getter[Boolean, Boolean]] =
    Const("not")

  def gtAsAfl[S, A](
      gt: Const[String, Getter[S, A]]): Const[String, AffineFold[S, A]] =
    Const(s"${gt.getConst}.asAffineFold")

  def aflAsFl[S, A](
      afl: Const[String, AffineFold[S, A]]): Const[String, Fold[S, A]] =
    Const(s"${afl.getConst}.asFold")
}

