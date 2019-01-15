package statelesser
package sqlnormal

/*sealed*/ abstract class TSemantic[A]

case class Done[O[_, _], S, A](
  expr: TSel[S, A], 
  filt: Set[TExpr[S, Boolean]],
  vars: Map[Symbol, statelesser.Value]) extends TSemantic[O[S, A]] {

  def as[O2[_, _]]: Done[O2, S, A] = Done(expr, filt, vars)
}

trait Todo[O[_, _], A, B] extends TSemantic[O[A, B]] { self =>

  def apply[S](done: Done[O, S, A]): Done[O, S, B]

  def compose[S](other: TSemantic[O[S, A]]): TSemantic[O[S, B]] = other match {
    case sem@Done(_, _, _) => apply(sem)
    case sem: Todo[O, S @ unchecked, A] => new Todo[O, S, B] {
      def apply[T](done: Done[O, T, S]): Done[O, T, B] = self(sem(done))
    }
  }

  def as[O2[_, _]]: Todo[O2, A, B] = new Todo[O2, A, B] {
    def apply[S](done: Done[O2, S, A]) =
      self.apply(done.as[O]).as[O2]
  }
}

