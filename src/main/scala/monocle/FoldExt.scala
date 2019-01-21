package monocle

import scalaz._, Scalaz._

trait FoldExt {

  implicit class FoldOps[S, A](fl: Fold[S, A]) {
    def *[B](other: Fold[S, B]): Fold[S, (A, B)] = new Fold[S, (A, B)] {
      def foldMap[M: Monoid](f: ((A, B)) => M)(s: S): M = 
        fl.foldMap(List(_))(s)
          .tuple(other.foldMap(List(_))(s))
          .foldMap(f)
    }
  }
}

