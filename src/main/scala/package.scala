import scalaz._, Scalaz._

package object `statelesser` {

  type TypeNme = String
  type OpticNme = String

  type Symbol = String

  def fresh: State[Stream[String], String] =
    for {
      s <- get[Stream[String]]
      _ <- modify[Stream[String]](_.tail)
    } yield s.head

}

