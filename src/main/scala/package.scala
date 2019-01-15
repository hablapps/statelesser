import scalaz._

import scalaz._, Scalaz._

package object `statelesser` {

  type TypeNme = String
  type OpticNme = String

  type Symbol = String

  type Value = sqlnormal.OpticType[_, _] \/ sqlnormal.Select[_, _, _]

  type Semantic[A] = State[Stream[Symbol], sqlnormal.TSemantic[A]]

  def fresh: State[Stream[String], String] =
    for {
      s <- get[Stream[String]]
      _ <- modify[Stream[String]](_.tail)
    } yield s.head

}

