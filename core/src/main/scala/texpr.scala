package statelesser

import scalaz.Leibniz, Leibniz.===
import OpticLang._

sealed abstract class TExpr[E[_], O[_, _], S, A] {

  def mapO[O2[_, _]](f: OpticMap[E, O, O2]): TExpr[E, O2, S, A] = this match {
    case Product(l, r, is) => Product(l.mapO(f), r.mapO(f), is)
    case Vertical(u, d) => Vertical(u.mapO(f), d.mapO(f))
    case LikeInt(i, is) => LikeInt(i, is)
    case LikeBool(b, is) => LikeBool(b, is)
    case x: TSel[E, O, S, A] => x.mapOSel(f)
  }

  def vars: Set[String] = this match {
    case Product(l, r, _) => l.vars ++ r.vars
    case Vertical(u, d) => u.vars ++ d.vars
    case Var(name) => Set(name)
    case _ => Set.empty
  }
}

object TExpr {

  def product[E[_], O[_, _], S, A, B, C](
      l: TExpr[E, O, S, A],
      r: TExpr[E, O, S, B],
      is: (A, B) === C): TExpr[E, O, S, C] =
    Product(l, r, is)
  
  def vertical[E[_], O[_, _], S, A, B](
      u: TExpr[E, O, S, A],
      d: TExpr[E, O, A, B]): TExpr[E, O, S, B] =
    Vertical(u, d)

  def likeInt[E[_], O[_, _], S, A](
      i: Int,
      is: A === Int): TExpr[E, O, S, A] =
    LikeInt(i, is)

  def likeBool[E[_], O[_, _], S, A](
      b: Boolean,
      is: A === Boolean): TExpr[E, O, S, A] =
    LikeBool(b, is)
}

case class Product[E[_], O[_, _], S, A, B, C](
  l: TExpr[E, O, S, A],
  r: TExpr[E, O, S, B],
  is: (A, B) === C) extends TExpr[E, O, S, C]

case class Vertical[E[_], O[_, _], S, A, B](
  u: TExpr[E, O, S, A],
  d: TExpr[E, O, A, B]) extends TExpr[E, O, S, B]

case class LikeInt[E[_], O[_, _], S, A](
  i: Int,
  is: A === Int) extends TExpr[E, O, S, A]

case class LikeBool[E[_], O[_, _], S, A](
  b: Boolean,
  is: A === Boolean) extends TExpr[E, O, S, A]

sealed abstract class TSel[E[_], O[_, _], S, A] extends TExpr[E, O, S, A] {
  def mapOSel[O2[_, _]](f: OpticMap[E, O, O2]): TSel[E, O2, S, A] = this match {
    case Var(name) => Var(name)
    case Wrap(e, inf) => Wrap(f(e), inf)
    case Sub(is1, is2) => Sub(is1, is2)
    case Gt(is1, is2) => Gt(is1, is2)
    case Not(is1, is2) => Not(is1, is2)
  }
}

case class Var[E[_], O[_, _], S, A](
  name: String) extends TSel[E, O, S, A]

case class Wrap[E[_], O[_, _], S, A](
  e: E[O[S, A]],
  info: OpticInfo) extends TSel[E, O, S, A]

case class Sub[E[_], O[_, _], S, A](
  is1: S === (Int, Int),
  is2: A === Int) extends TSel[E, O, S, A]

case class Gt[E[_], O[_, _], S, A](
  is1: S === (Int, Int),
  is2: A === Boolean) extends TSel[E, O, S, A]

case class Not[E[_], O[_, _], S, A](
  is1: S === Boolean,
  is2: A === Boolean) extends TSel[E, O, S, A]

