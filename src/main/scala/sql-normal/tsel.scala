package statelesser
package sqlnormal

import scalaz._, Leibniz.===
import monocle.Optional

sealed abstract class TSel[S, A] {

  def vars: Set[TVar[_, _]] = this match {
    case Pair(l, r, _) => l.vars ++ r.vars
    case Just(e) => e.vars
  }
}

case class Just[S, A](e: TExpr[S, A]) extends TSel[S, A]

case class Pair[S, A, L, R](
  l: TSel[S, L], 
  r: TSel[S, R],
  is: (L, R) === A) extends TSel[S, A]

