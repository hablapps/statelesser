package statelesser
package sqlnormal

import scalaz._, Leibniz.===

sealed abstract class TSel[S, A] {

  // def vars: Set[String] = this match {
  //   case Pair(l, r, _) => l.vars ++ r.vars
  //   case Just(e) => e.vars
  // }

  // def renameVars(rws: Set[(String, String)]): TSel[S, A] = this match {
  //   case Pair(l, r, is) => Pair(l.renameVars(rws), r.renameVars(rws), is)
  //   case Just(e) => Just(e.renameVars(rws))
  // }
}

case class Just[S, A](e: TExpr[S, A]) extends TSel[S, A]

case class Pair[S, A, L, R](
  l: TSel[S, L], 
  r: TSel[S, R],
  is: (L, R) === A) extends TSel[S, A]

