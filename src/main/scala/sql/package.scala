package statelesser

package object `sql` {

  type Table = String

  type Var = String

  type FieldName = String

  object SQL extends FromSemantic with ToString
}

