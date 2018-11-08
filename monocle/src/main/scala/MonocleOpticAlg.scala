package org.hablapps.statelesser
package monocle

import _root_.monocle._

object MonocleOpticAlg extends OpticAlg[Id, Id] {

  def point[A](a: A): A = a

  def lens[S, A](
      ln: Lens[S, A],
      tab: String,
      att: String): Lens[S, A] = 
    ln
  
  def traversal[S, A](
      tr: Traversal[S, A],
      tab: String): Traversal[S, A] = 
    tr
  
  def lensComposeLens[S, A, B](
      ln1: Lens[S, A], 
      ln2: Lens[A, B]): Lens[S, B] =
    ln1 composeLens ln2

  def lensComposeTraversal[S, A, B](
      ln: Lens[S, A],
      tr: Traversal[A, B]): Traversal[S, B] =
    ln composeTraversal tr

  def traversalComposeLens[S, A, B](
      tr: Traversal[S, A],
      ln: Lens[A, B]): Traversal[S, B] =
    tr composeLens ln

  def traversalComposeTraversal[S, A, B](
      tr1: Traversal[S, A],
      tr2: Traversal[A, B]): Traversal[S, B] =
    tr1 composeTraversal tr2

  def lensHorizComposeLens[S, A, B](
      ln1: Lens[S, A],
      ln2: Lens[S, B]): Lens[S, (A, B)] =
    ln1 * ln2

  def lensAsFold[S, A](ln: Lens[S, A]): Fold[S, A] = ln.asFold

  def traversalAsFold[S, A](tr: Traversal[S, A]): Fold[S, A] = tr.asFold

  def foldWithFilter[S, A, B](fl: Fold[S, A], f: A => B): Fold[S, B] =
    ???        

  def fun[A, B](f: A => B): A => B = f

  def app[A, B](f: A => B, a: A): B = f(a)

  def traversalGetAll[S, A](tr: Traversal[S, A]): S => List[A] = tr.getAll
}

