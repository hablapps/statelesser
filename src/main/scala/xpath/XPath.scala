package statelesser
package xpath

sealed abstract class Path
case class Name(s: String) extends Path
case class Seq(p: Path, q: Path) extends Path
case class Attribute(s: String) extends Path
case class Filter(p: Path) extends Path
case class PAxis(a: Axis) extends Path
case class Union(p: Path, q: Path) extends Path
case class Unary(s: String, p: Path) extends Path
case class Binary(s: String, p: Path, q: Path) extends Path

sealed abstract class Constant extends Path
case class PInt(i: Int) extends Constant
case class PBoolean(b: Boolean) extends Constant
case class PString(s: String) extends Constant

sealed abstract class Axis
case object Self extends Axis
case object Child extends Axis

