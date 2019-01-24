package statelesser
package core
package interpreter

import monocle._
import Statelesser.Church

class ChurchEncoding extends Statelesser[Church] {

  def flVert[S, A, B](
      u: Church[Fold[S, A]],
      d: Church[Fold[A, B]]): Church[Fold[S, B]] =
    new Church[Fold[S, B]] {
      def apply[E[_]: Statelesser]: E[Fold[S, B]] =
        Statelesser[E].flVert(u[E], d[E])
    }

  def flHori[S, A, B](
      l: Church[Fold[S, A]],
      r: Church[Fold[S, B]]): Church[Fold[S, (A, B)]] =
    new Church[Fold[S, (A, B)]] {
      def apply[E[_]: Statelesser]: E[Fold[S, (A, B)]] =
        Statelesser[E].flHori(l[E], r[E])
    }

  def gtVert[S, A, B](
      u: Church[Getter[S, A]],
      d: Church[Getter[A, B]]): Church[Getter[S, B]] =
    new Church[Getter[S, B]] {
      def apply[E[_]: Statelesser]: E[Getter[S, B]] =
        Statelesser[E].gtVert(u[E], d[E])
    }

  def gtHori[S, A, B](
      l: Church[Getter[S, A]],
      r: Church[Getter[S, B]]): Church[Getter[S, (A, B)]] =
    new Church[Getter[S, (A, B)]] {
      def apply[E[_]: Statelesser]: E[Getter[S, (A, B)]] =
        Statelesser[E].gtHori(l[E], r[E])
    }

  def aflVert[S, A, B](
      u: Church[AffineFold[S, A]],
      d: Church[AffineFold[A, B]]): Church[AffineFold[S, B]] =
    new Church[AffineFold[S, B]] {
      def apply[E[_]: Statelesser]: E[AffineFold[S, B]] =
        Statelesser[E].aflVert(u[E], d[E])
    }

  def aflHori[S, A, B](
      l: Church[AffineFold[S, A]],
      r: Church[AffineFold[S, B]]): Church[AffineFold[S, (A, B)]] =
    new Church[AffineFold[S, (A, B)]] {
      def apply[E[_]: Statelesser]: E[AffineFold[S, (A, B)]] =
        Statelesser[E].aflHori(l[E], r[E])
    }

  def filtered[S](p: Church[Getter[S, Boolean]]): Church[AffineFold[S, S]] =
    new Church[AffineFold[S, S]] {
      def apply[E[_]: Statelesser]: E[AffineFold[S, S]] =
        Statelesser[E].filtered(p[E])
    }

  def sub: Church[Getter[(Int, Int), Int]] =
    new Church[Getter[(Int, Int), Int]] {
      def apply[E[_]: Statelesser]: E[Getter[(Int, Int), Int]] =
        Statelesser[E].sub
    }

  def greaterThan: Church[Getter[(Int, Int), Boolean]] =
    new Church[Getter[(Int, Int), Boolean]] {
      def apply[E[_]: Statelesser]: E[Getter[(Int, Int), Boolean]] =
        Statelesser[E].greaterThan
    }

  def first[A, B]: Church[Getter[(A, B), A]] =
    new Church[Getter[(A, B), A]] {
      def apply[E[_]: Statelesser]: E[Getter[(A, B), A]] =
        Statelesser[E].first
    }

  def second[A, B]: Church[Getter[(A, B), B]] =
    new Church[Getter[(A, B), B]] {
      def apply[E[_]: Statelesser]: E[Getter[(A, B), B]] =
        Statelesser[E].second
    }

  def likeInt[S](i: Int): Church[Getter[S, Int]] =
    new Church[Getter[S, Int]] {
      def apply[E[_]: Statelesser]: E[Getter[S, Int]] =
        Statelesser[E].likeInt(i)
    }

  def likeBool[S](b: Boolean): Church[Getter[S, Boolean]] =
    new Church[Getter[S, Boolean]] {
      def apply[E[_]: Statelesser]: E[Getter[S, Boolean]] =
        Statelesser[E].likeBool(b)
    }

  def likeStr[S](s: String): Church[Getter[S, String]] =
    new Church[Getter[S, String]] {
      def apply[E[_]: Statelesser]: E[Getter[S, String]] =
        Statelesser[E].likeStr(s)
    }

  def id[S]: Church[Getter[S, S]] =
    new Church[Getter[S, S]] {
      def apply[E[_]: Statelesser]: E[Getter[S, S]] =
        Statelesser[E].id
    }

  def not: Church[Getter[Boolean, Boolean]] =
    new Church[Getter[Boolean, Boolean]] {
      def apply[E[_]: Statelesser]: E[Getter[Boolean, Boolean]] =
        Statelesser[E].not
    }

  def gtAsAfl[S, A](gt: Church[Getter[S, A]]): Church[AffineFold[S, A]] =
    new Church[AffineFold[S, A]] {
      def apply[E[_]: Statelesser]: E[AffineFold[S, A]] =
        Statelesser[E].gtAsAfl(gt[E])
    }

  def aflAsFl[S, A](afl: Church[AffineFold[S, A]]): Church[Fold[S, A]] =
    new Church[Fold[S, A]] {
      def apply[E[_]: Statelesser]: E[Fold[S, A]] =
        Statelesser[E].aflAsFl(afl[E])
    }
}

