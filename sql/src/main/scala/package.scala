package org.hablapps.statelesser

import scalaz.State

package object `sql` {

  type Table = String

  type Column = String

  type At[A] = State[Rel, A]

  type Sql[A] = String
}

