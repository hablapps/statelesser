package statelesser
package sqlnormal

import scalaz._
import monocle._

sealed abstract class TSemantic[A]

object TSemantic {
  def toSql[S, A](
      sem: Semantic[Fold[S, A]], 
      keys: Map[String, String]): sql.SSelect =
    (new interpreter.ToSql).toSql(sem, keys)
}

case class Done[O[_, _], S, A](
    expr: TSel[S, A], 
    filt: Set[TExpr[S, Boolean]],
    vars: TVarMap) extends TSemantic[O[S, A]] {
  def as[O2[_, _]]: Done[O2, S, A] = Done(expr, filt, vars)
}

case class Todo[O[_, _], A, B](
    f: Done[O, ?, A] ~> Done[O, ?, B]) extends TSemantic[O[A, B]] {

  def compose[S](other: TSemantic[O[S, A]]): TSemantic[O[S, B]] = other match {
    case sem@Done(_, _, _) => f(sem)
    case sem@Todo(g) => Todo(f.compose[Done[O, ?, S]](g))
  }

  def as[O2[_, _]]: Todo[O2, A, B] = 
    Todo(λ[Done[O2, ?, A] ~> Done[O2, ?, B]](done => f(done.as[O]).as[O2]))
}

