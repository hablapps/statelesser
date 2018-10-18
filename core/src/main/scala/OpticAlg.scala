package org.hablapps.statelesser

import monocle._

trait OpticAlg[Expr[_], Obs[_]] {

  def point[A](a: A): Expr[A]

  def lens[S, A](
    ln: Lens[S, A], 
    tab: String,
    att: String): Expr[Lens[S, A]]

  def traversal[S, A](
    tr: Traversal[S, A], 
    tab: String): Expr[Traversal[S, A]]

  def lensComposeLens[S, A, B](
    ln1: Expr[Lens[S, A]], 
    ln2: Expr[Lens[A, B]]): Expr[Lens[S, B]]

  def lensComposeTraversal[S, A, B](
    ln: Expr[Lens[S, A]],
    tr: Expr[Traversal[A, B]]): Expr[Traversal[S, B]]

  def traversalComposeLens[S, A, B](
    tr: Expr[Traversal[S, A]],
    ln: Expr[Lens[A, B]]): Expr[Traversal[S, B]]

  def traversalComposeTraversal[S, A, B](
    tr1: Expr[Traversal[S, A]],
    tr2: Expr[Traversal[A, B]]): Expr[Traversal[S, B]]

  def lensHorizComposeLens[S, A, B](
    ln1: Expr[Lens[S, A]],
    ln2: Expr[Lens[S, B]]): Expr[Lens[S, (A, B)]]

  def lensAsFold[S, A](
    ln: Expr[Lens[S, A]]): Expr[Fold[S, A]]

  def traversalAsFold[S, A](
    tr: Expr[Traversal[S, A]]): Expr[Fold[S, A]]

  def foldWithFilter[S, A, B](
    fl: Expr[Fold[S, A]],
    f: Expr[A => B]): Expr[Fold[S, B]]

  def fun[A, B](f: Expr[A] => Expr[B]): Expr[A => B]

  def app[A, B](f: Expr[A => B], a: Expr[A]): Expr[B]

  def traversalGetAll[S, A](
    tr: Expr[Traversal[S, A]]): Obs[S => List[A]]
}

object OpticAlg {

  def apply[Expr[_], Obs[_]](
    implicit alg: OpticAlg[Expr, Obs]): OpticAlg[Expr, Obs] = alg

  trait Syntax {

    def point[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], A](a: A): Expr[A] =
      OpticAlg[Expr, Obs].point(a)

    def lens[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], S, A](
        ln: Lens[S, A],
        tab: String,
        att: String): Expr[Lens[S, A]] =
      OpticAlg[Expr, Obs].lens(ln, tab, att)

    def traversal[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], S, A](
        tr: Traversal[S, A],
        tab: String): Expr[Traversal[S, A]] =
      OpticAlg[Expr, Obs].traversal(tr, tab)

    def lensComposeLens[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], S, A, B](
        ln1: Expr[Lens[S, A]], 
        ln2: Expr[Lens[A, B]]): Expr[Lens[S, B]] = 
      OpticAlg[Expr, Obs].lensComposeLens(ln1, ln2)

    def lensComposeTraversal[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], S, A, B](
        ln: Expr[Lens[S, A]],
        tr: Expr[Traversal[A, B]]): Expr[Traversal[S, B]] =
      OpticAlg[Expr, Obs].lensComposeTraversal(ln, tr)

    def traversalComposeLens[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], S, A, B](
        tr: Expr[Traversal[S, A]],
        ln: Expr[Lens[A, B]]): Expr[Traversal[S, B]] =
      OpticAlg[Expr, Obs].traversalComposeLens(tr, ln)

    def traversalComposeTraversal[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], S, A, B](
        tr1: Expr[Traversal[S, A]],
        tr2: Expr[Traversal[A, B]]): Expr[Traversal[S, B]] =
      OpticAlg[Expr, Obs].traversalComposeTraversal(tr1, tr2)

    def lensHorizComposeLens[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], S, A, B](
        ln1: Expr[Lens[S, A]],
        ln2: Expr[Lens[S, B]]): Expr[Lens[S, (A, B)]] =
      OpticAlg[Expr, Obs].lensHorizComposeLens(ln1, ln2)

    def lensAsFold[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], S, A](
        ln: Expr[Lens[S, A]]): Expr[Fold[S, A]] =
      OpticAlg[Expr, Obs].lensAsFold(ln)

    def traversalAsFold[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], S, A](
        tr: Expr[Traversal[S, A]]): Expr[Fold[S, A]] =
      OpticAlg[Expr, Obs].traversalAsFold(tr)      

    def foldWithFilter[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], S, A, B](
        fl: Expr[Fold[S, A]],
        f: Expr[A => B]): Expr[Fold[S, B]] =
      OpticAlg[Expr, Obs].foldWithFilter(fl, f)

    def traversalGetAll[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], S, A](
        tr: Expr[Traversal[S, A]]): Obs[S => List[A]] =
      OpticAlg[Expr, Obs].traversalGetAll(tr)

    def fun[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], A, B](
        f: Expr[A] => Expr[B]): Expr[A => B] =
      OpticAlg[Expr, Obs].fun(f)

    def app[Expr[_], Obs[_]: OpticAlg[Expr, ?[_]], A, B](
        f: Expr[A => B], 
        a: Expr[A]): Expr[B] =
      OpticAlg[Expr, Obs].app(f, a)
  }

  object syntax extends Syntax
}

