import scalaz._

import statelesser.OpticLang._

package object `statelesser` {

  type TypeNme = String
  type OpticNme = String

  type Symbol = String

  type Semantic[E[_], A] = State[Table, TSemantic[E, A]]
}

