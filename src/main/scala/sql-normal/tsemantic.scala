package statelesser
package sqlnormal

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

sealed abstract class TSel[S, A] {

  def vars: Set[String] = this match {
    case Pair(l, r, _) => l.vars ++ r.vars
    case Just(e) => e.vars
  }

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
  vars: Map[Symbol, statelesser.Value]) extends TSemantic[O[S, A]] {

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

