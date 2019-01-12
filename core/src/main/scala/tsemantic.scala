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

sealed abstract class TExpr[S, A]

case class LikeInt[S](i: Int) extends TExpr[S, Int]

case class LikeBool[S](b: Boolean) extends TExpr[S, Boolean]

case class LikeStr[S](s: String) extends TExpr[S, String]

case class Var[S, A](sym: Symbol) extends TExpr[S, A]

case class Select[S, A, B](
  v: Var[S, A], 
  ot: OpticType[A, B]) extends TExpr[S, B]

case class Not[S](b: TExpr[S, Boolean]) extends TExpr[S, Boolean]

case class Sub[S](l: TExpr[S, Int], r: TExpr[S, Int]) extends TExpr[S, Int]

case class Gt[S](l: TExpr[S, Int], r: TExpr[S, Int]) extends TExpr[S, Boolean]

sealed abstract class TSel[S, A]

case class Just[S, A](e: TExpr[S, A]) extends TSel[S, A]

case class Pair[S, A, L, R](
  l: TSel[S, L], 
  r: TSel[S, R],
  is: (L, R) === A) extends TSel[S, A]

sealed abstract class TSemantic[A]

case class Done[O[_, _], S, A](
  expr: TSel[S, A], 
  filt: Set[TExpr[S, Boolean]],
  vars: Map[Symbol, Value]) extends TSemantic[O[S, A]]

trait Todo[O[_, _], A, B] extends TSemantic[O[A, B]] { self =>

  def apply[S](done: Done[O, S, A]): Done[O, S, B]

  def compose[S](other: TSemantic[O[S, A]]): TSemantic[O[S, B]] = other match {
    case sem@Done(_, _, _) => apply(sem)
    case sem: Todo[O, S @ unchecked, A] => new Todo[O, S, B] {
      def apply[T](done: Done[O, T, S]): Done[O, T, B] = self(sem(done))
    }
  }
}

