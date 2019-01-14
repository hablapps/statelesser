package statelesser

import scalaz._, Leibniz.===

sealed abstract class OpticKind
case object KGetter extends OpticKind
case object KAffineFold extends OpticKind
case object KFold extends OpticKind

case class TypeInfo(nme: TypeNme, isPrimitive: Boolean = false)

case class OpticType[S, A](
  kind: OpticKind, 
  nme: Symbol, 
  src: TypeInfo, 
  tgt: TypeInfo)

sealed abstract class TExpr[S, A] {
  def renameVars(rws: Set[(String, String)]): TExpr[S, A] = this match {
    case Var(sym) => rws.find(_._1 == sym).fold(this)(kv => Var(kv._2))
    case Select(Var(sym), ot) =>
      rws.find(_._1 == sym).fold(this)(kv => Select(Var(kv._2), ot))
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

case class Var[S, A](sym: Symbol) extends TExpr[S, A]

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

sealed abstract class TSel[S, A] {
  def renameVars(rws: Set[(String, String)]): TSel[S, A] = this match {
    case Pair(l, r, is) => Pair(l.renameVars(rws), r.renameVars(rws), is)
    case Just(e) => Just(e.renameVars(rws))
  }
}

case class Just[S, A](e: TExpr[S, A]) extends TSel[S, A]

case class Pair[S, A, L, R](
  l: TSel[S, L], 
  r: TSel[S, R],
  is: (L, R) === A) extends TSel[S, A]

/*sealed*/ abstract class TSemantic[A]

case class Done[O[_, _], S, A](
  expr: TSel[S, A], 
  filt: Set[TExpr[S, Boolean]],
  vars: Map[Symbol, Value]) extends TSemantic[O[S, A]] {

  def as[O2[_, _]]: Done[O2, S, A] = Done(expr, filt, vars)
}

trait Todo[O[_, _], A, B] extends TSemantic[O[A, B]] { self =>

  def apply[S](done: Done[O, S, A]): Done[O, S, B]

  def compose[S](other: TSemantic[O[S, A]]): TSemantic[O[S, B]] = other match {
    case sem@Done(_, _, _) => apply(sem)
    case sem: Todo[O, S @ unchecked, A] => new Todo[O, S, B] {
      def apply[T](done: Done[O, T, S]): Done[O, T, B] = self(sem(done))
    }
  }

  def as[O2[_, _]]: Todo[O2, A, B] = new Todo[O2, A, B] {
    def apply[S](done: Done[O2, S, A]) =
      self.apply(done.as[O]).as[O2]
  }
}

