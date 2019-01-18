package statelesser
package sqlnormal

import scalaz._, Scalaz._, Leibniz.===

sealed abstract class TExpr[S, A] {

  def vars: Set[String] = this match {
    case Var(syms) => syms.toList.toSet
    case Select(Var(syms), _) => syms.toList.toSet
    case Not(b, _) => b.vars
    case Sub(l, r, _) => l.vars ++ r.vars
    case Gt(l, r, _) => l.vars ++ r.vars
    case _ => Set.empty
  }
  
  def renameVars(rws: Set[(String, String)]): TExpr[S, A] = this match {
    case Var(syms) => 
      Var(syms.map(sym => rws.find(_._1 == sym).fold(sym)(_._2)))
    case Select(Var(syms), ot) =>
      Select(Var(syms.map(sym => rws.find(_._1 == sym).fold(sym)(_._2))), ot)
    case Not(b, is) => TExpr.not(b.renameVars(rws), is)
    case Sub(l, r, is) => TExpr.sub(l.renameVars(rws), r.renameVars(rws), is)
    case Gt(l, r, is) => TExpr.gt(l.renameVars(rws), r.renameVars(rws), is)
    case _ => this
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

case class Var[S, A](syms: NonEmptyList[Symbol]) extends TExpr[S, A]

case class Select[S, A, B](
  v: Var[S, A], 
  ot: OpticType[A, B]) extends TExpr[S, B]

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

