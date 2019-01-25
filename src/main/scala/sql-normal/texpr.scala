package statelesser
package sqlnormal

import scalaz._, Leibniz.===
import monocle.Optional, monocle.function.all._

sealed abstract class TExpr[S, A] {

  def vars: Set[TVar[_, _]] = this match {
    case v: TVar[_, _] => Set(v)
    case Select(v, _) => Set(v)
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

sealed abstract class TVar[S, A] extends TExpr[S, A] {

  def apply: Optional[TVarMap, TVarTree]

  def symbols(tvm: TVarMap): Option[NonEmptyList[Symbol]] = this match {
    case RootVar(op) => op.getOption(tvm).map(tvt => NonEmptyList(tvt.label))
    case v@Var(t, i) => v.apply.getOption(tvm).flatMap { tvt => 
      t.symbols(tvm).map(_ append NonEmptyList(tvt.label))
    }
  }
}

case class RootVar[S, A](apply: Optional[TVarMap, TVarTree]) extends TVar[S, A]

case class Var[S, A, B](
    top: TVar[S, A], 
    i: OpticType[A, B]) extends TVar[S, B] {
  def apply: Optional[TVarMap, TVarTree] = 
    top.apply.composeOptional(index[TVarTree, OpticType[_, _], TVarTree](i))
}

case class Select[S, A, B](
  v: TVar[S, A], 
  label: OpticType[A, B]) extends TExpr[S, B]

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

