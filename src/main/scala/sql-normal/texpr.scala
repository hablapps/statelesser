package statelesser
package sqlnormal

import scalaz._, Leibniz.===
import monocle.Optional

sealed abstract class TExpr[S, A] {

  def vars: Set[Optional[TVarMap, TVarTree]] = 
    this match {
      case Var(op) => Set(op)
      case Select(v, _) => v.vars
      case Not(b, _) => b.vars
      case Sub(l, r, _) => l.vars ++ r.vars
      case Gt(l, r, _) => l.vars ++ r.vars
      case _ => Set.empty
    }
}

object TExpr {

  def not[S, A](b: TExpr[S, Boolean], is: A === Boolean): TExpr[S, A] = 
    Not(b, is)

  def sub[S, A](
      l: TExpr[S, Int], 
      r: TExpr[S, Int], 
      is: A === Int): TExpr[S, A] = 
    Sub(l, r, is)

  def gt[S, A](
      l: TExpr[S, Int], 
      r: TExpr[S, Int], 
      is: A === Boolean): TExpr[S, A] = 
    Gt(l, r, is)
}

case class LikeInt[S](i: Int) extends TExpr[S, Int]

case class LikeBool[S](b: Boolean) extends TExpr[S, Boolean]

case class LikeStr[S](s: String) extends TExpr[S, String]

case class Var[S, A](op: Optional[TVarMap, TVarTree]) extends TExpr[S, A]

case class Select[S, A, B](
  v: Var[S, A], 
  label: (String, OpticType[A, B])) extends TExpr[S, B]

case class Not[S, A](
  b: TExpr[S, Boolean], 
  is: A === Boolean) extends TExpr[S, A]

case class Sub[S, A](
  l: TExpr[S, Int], 
  r: TExpr[S, Int],
  is: A === Int) extends TExpr[S, A]

case class Gt[S, A](
  l: TExpr[S, Int], 
  r: TExpr[S, Int],
  is: A === Boolean) extends TExpr[S, A]

