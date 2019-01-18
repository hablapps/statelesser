package statelesser

import scalaz._, Scalaz._

package object `sqlnormal` {

  type TypeNme = String

  type OpticNme = String

  type Value = OpticType[_, _] \/ Select[_, _, _]

  type Semantic[A] = State[Stream[Symbol], TSemantic[A]]

  type Symbol = String

  def fresh: State[Stream[String], String] =
    for {
      s <- get[Stream[String]]
      _ <- modify[Stream[String]](_.tail)
    } yield s.head 

  type IForest[I, A] = Map[I, ITree[I, A]]

  type TVarTree = NonEmptyList[(Symbol, ITree[Symbol, OpticType[_, _]])]

  implicit class TVarTreeOps(tvt: TVarTree) {
    def loc(s: Symbol): ITreeLoc[Symbol, OpticType[_, _]] = {
      val ifrs = tvt.toList.toMap
      ITreeLoc(s, ifrs(s), ifrs - s, Stream.empty)
    }
  }
}

