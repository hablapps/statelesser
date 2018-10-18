package org.hablapps.statelesser
package sql

import _root_.monocle._
import scalaz.{Lens => _, _}, Scalaz._

object SqlOpticAlg extends OpticAlg[At, Sql] {

  def point[A](a: A): At[A] =
    ???

  def lens[S, A](
      ln: Lens[S, A],
      tab: String,
      att: String): At[Lens[S, A]] = 
    modify((s: Rel) => s.copy(sym = s.sym + (ln -> Node(tab, att)))) >>
    ln.point[At]
  
  def traversal[S, A](
      tr: Traversal[S, A],
      tab: String): At[Traversal[S, A]] =
    modify((s: Rel) => s.copy(sym = s.sym + (tr -> Node(tab, "*")))) >>
    tr.point[At]
  
  def lensComposeLens[S, A, B](
      ln1: At[Lens[S, A]], 
      ln2: At[Lens[A, B]]): At[Lens[S, B]] =
    for {
      a <- ln1
      b <- ln2
      c = a composeLens b
      _ <- modify[Rel] { s => 
        s.copy(sym = s.sym + (c -> s.sym(a).append(s.sym(b))))
      }
    } yield c

  def lensComposeTraversal[S, A, B](
      ln: At[Lens[S, A]],
      tr: At[Traversal[A, B]]): At[Traversal[S, B]] =
    ???

  def traversalComposeLens[S, A, B](
      tr: At[Traversal[S, A]],
      ln: At[Lens[A, B]]): At[Traversal[S, B]] =
    for {
      a <- tr
      b <- ln
      c = a composeLens b
      _ <- modify[Rel] { s => 
        s.copy(sym = s.sym + (c -> s.sym(a).append(s.sym(b))))
      }
    } yield c

  def traversalComposeTraversal[S, A, B](
      tr1: At[Traversal[S, A]],
      tr2: At[Traversal[A, B]]): At[Traversal[S, B]] =
    ???

  def lensHorizComposeLens[S, A, B](
      ln1: At[Lens[S, A]],
      ln2: At[Lens[S, B]]): At[Lens[S, (A, B)]] =
    for {
      a <- ln1
      b <- ln2
      c = a * b
      _ <- modify[Rel] { s => 
        s.copy(sym = s.sym + (c -> Bunch(List(s.sym(a), s.sym(b)))))
      }
    } yield c

  def lensAsFold[S, A](ln: At[Lens[S, A]]): At[Fold[S, A]] = 
    ???

  def traversalAsFold[S, A](tr: At[Traversal[S, A]]): At[Fold[S, A]] = 
    ???

  def foldWithFilter[S, A, B](
      fl: At[Fold[S, A]], 
      f: At[A => B]): At[Fold[S, B]] =
    ???        

  def fun[A, B](f: At[A] => At[B]): At[A => B] =
    ???

  def app[A, B](f: At[A => B], a: At[A]): At[B] = 
    ???

  def traversalGetAll[S, A](tr: At[Traversal[S, A]]): Sql[S => List[A]] =
    (for {
      a <- tr
      r <- gets[Rel, RelTree](_.sym(a))
      select = r.leafs.map(n => s"${n.tab.head.toLower}.${n.att}").mkString(", ")
      from = r.innerTabs.map(t => s"$t ${t.head.toLower}").mkString(" INNER JOIN ")
    } yield s"SELECT $select FROM $from").eval(Rel())
}

