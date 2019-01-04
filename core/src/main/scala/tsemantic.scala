package statelesser

import OpticLang.Table

sealed abstract class TSemantic[E[_], A]

case class TGetter[E[_], S, A](
  expr: TExpr[E, Getter, S, A]) extends TSemantic[E, Getter[S, A]]

case class TAffineFold[E[_], S, A](
    expr: TExpr[E, AffineFold, S, A],
    filt: Set[TExpr[E, AffineFold, S, Boolean]] =
      Set.empty[TExpr[E, AffineFold, S, Boolean]])
  extends TSemantic[E, AffineFold[S, A]]

case class TFold[E[_], S, A](
    expr: TExpr[E, Fold, S, A],
    filt: Set[TExpr[E, Fold, S, Boolean]] =
      Set.empty[TExpr[E, Fold, S, Boolean]],
    vars: Table = Table())
  extends TSemantic[E, Fold[S, A]]

