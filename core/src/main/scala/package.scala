import scalaz._

package object `statelesser` {

  type TypeNme = String
  type OpticNme = String

  type Symbol = String

  type Value = OpticType[_, _] \/ Select[_, _, _]

  type Semantic[A] = State[Stream[Symbol], TSemantic[A]]
}

