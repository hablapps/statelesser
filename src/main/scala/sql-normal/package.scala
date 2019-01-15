package statelesser

import scalaz._, Scalaz._

package object `sqlnormal` {

  type Value = OpticType[_, _] \/ Select[_, _, _]

  type Semantic[A] = State[Stream[Symbol], TSemantic[A]]
}

