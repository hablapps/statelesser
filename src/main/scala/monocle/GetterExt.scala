package monocle

trait GetterExt {

  implicit class GetterOps[S, A](gt: Getter[S, A]) {

    def *[B](other: Getter[S, B]): Getter[S, (A, B)] =
      Getter(s => (gt.get(s), other.get(s)))

    def asAffineFold: AffineFold[S, A] =
      AffineFold(s => Some(gt.get(s)))
  }
}

