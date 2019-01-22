package statelesser
package sqlnormal

case class TypeInfo(nme: TypeNme, isPrimitive: Boolean = false)

sealed abstract class OpticType[S, A] {
  val nme: String
  val src: TypeInfo
  val tgt: TypeInfo
}

case class GetterType[S, A](
  nme: String,
  src: TypeInfo, 
  tgt: TypeInfo) extends OpticType[S, A]

case class AffineFoldType[S, A](
  nme: String,
  src: TypeInfo, 
  tgt: TypeInfo) extends OpticType[S, A]

case class FoldType[S, A](
    nme: String,
    src: TypeInfo, 
    tgt: TypeInfo) extends OpticType[S, A] {
  override def equals(that: Any): Boolean = that match {
    case other: FoldType[S, A] => this eq other
    case _ => false
  }
}

