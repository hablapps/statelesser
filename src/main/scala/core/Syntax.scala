package statelesser
package core

import optic._

trait Syntax {

  implicit class FoldOps[Expr[_], S, A](
      l: Expr[Fold[S, A]])(implicit
      O: Statelesser[Expr]) {
    def >[B](r: Expr[Fold[A, B]]): Expr[Fold[S, B]] = O.flVert(l, r)
    def *[B](r: Expr[Fold[S, B]]): Expr[Fold[S, (A, B)]] = O.flHori(l, r)
  }

  implicit class AffineFoldOps[Expr[_], S, A](
      l: Expr[AffineFold[S, A]])(implicit
      O: Statelesser[Expr]) {
    def asFold: Expr[Fold[S, A]] = O.aflAsFl(l)
    def >[B](r: Expr[AffineFold[A, B]]): Expr[AffineFold[S, B]] =
      O.aflVert(l, r)
    def *[B](r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, (A, B)]] =
      O.aflHori(l, r)
    def <*[B](r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, A]] =
      l * r > O.first.asAffineFold
    def *>[B](r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, B]] =
      l * r > O.second.asAffineFold
  }

  implicit class GetterOps[Expr[_], S, A](
      l: Expr[Getter[S, A]])(implicit
      O: Statelesser[Expr]) {
    def asAffineFold: Expr[AffineFold[S, A]] = O.gtAsAfl(l)
    def asFold: Expr[Fold[S, A]] = l.asAffineFold.asFold
    def >[B](r: Expr[Getter[A, B]]): Expr[Getter[S, B]] = O.gtVert(l, r)
    def *[B](r: Expr[Getter[S, B]]): Expr[Getter[S, (A, B)]] = O.gtHori(l, r)
  }

  implicit class IntGetterOps[Expr[_], S](
      l: Expr[Getter[S, Int]])(implicit
      O: Statelesser[Expr]) {
    def -(r: Expr[Getter[S, Int]]): Expr[Getter[S, Int]] = l * r > O.sub
  }
}

