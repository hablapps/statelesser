package statelesser
package sqlnormal

sealed abstract class OpticKind
case object KGetter extends OpticKind
case object KAffineFold extends OpticKind
case object KFold extends OpticKind

case class TypeInfo(nme: TypeNme, isPrimitive: Boolean = false)

case class OpticType[S, A](
  kind: OpticKind, 
  nme: Symbol, 
  src: TypeInfo, 
  tgt: TypeInfo)

