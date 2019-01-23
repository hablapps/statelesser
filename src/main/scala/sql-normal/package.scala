package statelesser

import scalaz._, Scalaz._
import monocle.{Optional, _}, function.Index, Index._

package object `sqlnormal` {

  type TypeNme = String

  type OpticNme = String

  type Semantic[A] = State[Stream[Symbol], TSemantic[A]]

  type Symbol = String

  def fresh: State[Stream[String], String] =
    for {
      s <- get[Stream[String]]
      _ <- modify[Stream[String]](_.tail)
    } yield s.head 

  type IForest[I, A] = Map[I, ITree[I, A]]

  type TVarTree = ITree[OpticType[_, _], Symbol]

  type TVarMap = IForest[OpticType[_, _], Symbol]
}

