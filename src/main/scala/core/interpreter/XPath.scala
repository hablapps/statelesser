package statelesser
package core
package interpreter

import scalaz._
import monocle._
import xpath._

class XPath extends Statelesser[Const[Path, ?]] {

  private def vert[O[_, _], S, A, B](
      u: Const[Path, O[S, A]],
      d: Const[Path, O[A, B]]): Const[Path, O[S, B]] = {
    def aux(up: Path, dp: Path): Path = (up, dp) match {
      // left associativity
      case (x, Seq(y, z)) => aux(aux(x, y), z)
      // force simple optimizations 
      // XXX: could we get in looping troubles?
      case (Seq(x, y), z) => aux(x, aux(y, z))
      // self optimization
      case (PAxis(Self), y) => y
      case (x, PAxis(self)) => x
      // constant optimization
      case (_, c: Constant) => c
      case (x, y) => Seq(x, y)
    }
    Const(aux(u.getConst, d.getConst))
  }

  def flVert[S, A, B](
      u: Const[Path, Fold[S, A]],
      d: Const[Path, Fold[A, B]]): Const[Path, Fold[S, B]] =
    vert(u, d)

  def flHori[S, A, B](
      l: Const[Path, Fold[S, A]],
      r: Const[Path, Fold[S, B]]): Const[Path, Fold[S, (A, B)]] =
    ???

  def gtVert[S, A, B](
      u: Const[Path, Getter[S, A]],
      d: Const[Path, Getter[A, B]]): Const[Path, Getter[S, B]] =
    vert(u, d)

  def gtHori[S, A, B](
      l: Const[Path, Getter[S, A]],
      r: Const[Path, Getter[S, B]]): Const[Path, Getter[S, (A, B)]] =
    Const(Union(l.getConst, r.getConst))

  def aflVert[S, A, B](
      u: Const[Path, AffineFold[S, A]],
      d: Const[Path, AffineFold[A, B]]): Const[Path, AffineFold[S, B]] =
    vert(u, d)

  def aflHori[S, A, B](
      l: Const[Path, AffineFold[S, A]],
      r: Const[Path, AffineFold[S, B]]): Const[Path, AffineFold[S, (A, B)]] =
    ???

  def filtered[S](
      p: Const[Path, Getter[S, Boolean]]): Const[Path, AffineFold[S, S]] =
    Const(Filter(p.getConst))

  def sub: Const[Path, Getter[(Int, Int), Int]] =
    ???

  def greaterThan: Const[Path, Getter[(Int, Int), Boolean]] =
    ???

  def first[A, B]: Const[Path, Getter[(A, B), A]] =
    ???

  def second[A, B]: Const[Path, Getter[(A, B), B]] =
    ???

  def likeInt[S](i: Int): Const[Path, Getter[S, Int]] =
    Const(PInt(i))

  def likeBool[S](b: Boolean): Const[Path, Getter[S, Boolean]] =
    Const(PBoolean(b))

  def likeStr[S](s: String): Const[Path, Getter[S, String]] =
    Const(PString(s))

  def id[S]: Const[Path, Getter[S, S]] =
    Const(PAxis(Self))

  def not: Const[Path, Getter[Boolean, Boolean]] =
    ???

  def gtAsAfl[S, A](
      gt: Const[Path, Getter[S, A]]): Const[Path, AffineFold[S, A]] =
    Const(gt.getConst)

  def aflAsFl[S, A](
      afl: Const[Path, AffineFold[S, A]]): Const[Path, Fold[S, A]] =
    Const(afl.getConst)
}

