package statelesser
package core
package interpreter

import scalaz._
import monocle._
import xpath._

class XPath extends Statelesser[Const[Path, ?]] {

  // XXX: Look at you, functional programmer!
  var last: Char = ('a' - 1).toChar
  def fresh: String = { last = (last + 1).toChar; last.toString }

  private def vert[O[_, _], S, A, B](
      u: Const[Path, O[S, A]],
      d: Const[Path, O[A, B]]): Const[Path, O[S, B]] = {
    def aux(up: Path, dp: Path): Path = (up, dp) match {
      // left associativity
      case (x, Seq(y, z)) => aux(aux(x, y), z)
      // self optimization
      case (PAxis(Self), y) => y
      case (x, PAxis(self)) => x
      // constant optimization
      case (_, c: Constant) => c
      // function composition
      case (Todo(f), Todo(g)) => Todo(f andThen g)
      // app
      case (Seq(p, q), Todo(f)) => Seq(p, f(q))
      case (x, Todo(f)) => f(x)
      // attribute does not support child axis, force reductions!
      case (att: Attribute, For(List((x, l), (y, r)), ret)) =>
        For(List((x, aux(att, l)), (y, aux(att, r))), ret)
      case (att: Attribute, GreaterThan(p, q)) =>
        GreaterThan(aux(att, p), aux(att, q))
      // no optimization
      case (x, y) => Seq(x, y)
    }
    Const(aux(u.getConst, d.getConst))
  }

  private def hori[O[_, _], S, A, B](
      l: Const[Path, O[S, A]],
      r: Const[Path, O[S, B]]): Const[Path, O[S, (A, B)]] = {
    val x = Var(fresh); val y = Var(fresh)
    Const((l.getConst, r.getConst) match {
      case (Seq(p, q), Seq(r, s)) if p == r => 
        Seq(p, For(List(x -> q, y -> s), Union(x, y)))
      case (p, q) => For(List(x -> p, y -> q), Union(x, y))
    })
  }

  def flVert[S, A, B](
      u: Const[Path, Fold[S, A]],
      d: Const[Path, Fold[A, B]]): Const[Path, Fold[S, B]] =
    vert(u, d)

  def flHori[S, A, B](
      l: Const[Path, Fold[S, A]],
      r: Const[Path, Fold[S, B]]): Const[Path, Fold[S, (A, B)]] =
    hori(l, r)

  def gtVert[S, A, B](
      u: Const[Path, Getter[S, A]],
      d: Const[Path, Getter[A, B]]): Const[Path, Getter[S, B]] =
    vert(u, d)

  def gtHori[S, A, B](
      l: Const[Path, Getter[S, A]],
      r: Const[Path, Getter[S, B]]): Const[Path, Getter[S, (A, B)]] =
    hori(l, r)

  def aflVert[S, A, B](
      u: Const[Path, AffineFold[S, A]],
      d: Const[Path, AffineFold[A, B]]): Const[Path, AffineFold[S, B]] =
    vert(u, d)

  def aflHori[S, A, B](
      l: Const[Path, AffineFold[S, A]],
      r: Const[Path, AffineFold[S, B]]): Const[Path, AffineFold[S, (A, B)]] =
    hori(l, r)

  def filtered[S](
      p: Const[Path, Getter[S, Boolean]]): Const[Path, AffineFold[S, S]] =
    Const(Todo {
      case Attribute(s) => vert(
        Const(Filter(vert(Const(Attribute(s)), p).getConst)), 
        Const(Attribute(s))).getConst
      case x => vert(Const(x), Const(Filter(p.getConst))).getConst
    })

  def sub: Const[Path, Getter[(Int, Int), Int]] = Const(Todo {
    case Union(p, PInt(0)) => p
    case Union(p, q) if p == q => PInt(0)
    case Union(PInt(i), PInt(j)) => PInt(i - j)
    case Union(p, q) => Sub(p, q)
  })

  def greaterThan: Const[Path, Getter[(Int, Int), Boolean]] = Const(Todo {
    case (For(List((_, PInt(i)), (_, PInt(j))), Union(_, _))) => PBool(i > j)
    case (For(List((_, p), (_, q)), Union(_, _))) => GreaterThan(p, q)
  })

  def first[A, B]: Const[Path, Getter[(A, B), A]] = Const(Todo {
    case For(List((_, v), _), Union(_, _)) => v
  })

  def second[A, B]: Const[Path, Getter[(A, B), B]] = Const(Todo {
    case For(List(_, (_, v)), Union(_, _)) => v
  })

  def not: Const[Path, Getter[Boolean, Boolean]] = Const(Todo {
    case Not(p) => p
    case PBool(b) => PBool(!b)
    case p => Not(p)
  })

  def likeInt[S](i: Int): Const[Path, Getter[S, Int]] =
    Const(PInt(i))

  def likeBool[S](b: Boolean): Const[Path, Getter[S, Boolean]] =
    Const(PBool(b))

  def likeStr[S](s: String): Const[Path, Getter[S, String]] =
    Const(PString(s))

  def id[S]: Const[Path, Getter[S, S]] =
    Const(PAxis(Self))

  def gtAsAfl[S, A](
      gt: Const[Path, Getter[S, A]]): Const[Path, AffineFold[S, A]] =
    Const(gt.getConst)

  def aflAsFl[S, A](
      afl: Const[Path, AffineFold[S, A]]): Const[Path, Fold[S, A]] =
    Const(afl.getConst)
}

