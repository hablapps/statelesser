package statelesser
package xpath

sealed abstract class Path {
  override def toString: String = Path.toString(this)
}

object Path {
  
  def toString(path: Path): String = path match {
    case Seq(p, q) => s"$p/($q)"
    case Name(s) => s
    case Attribute(s) => s"@$s"
    case Filter(p) => s"[$p]"
    case PAxis(self) => "self::*"
    case Union(p, q) => s"$p | $q" 
    case For(pairs, ret) => s"for ${pairs.map {
      case (k, v) => s"$k in ($v)"
    } mkString(", ")} return ($ret)"
    case Var(s) => "$" + s
    case Not(p) => s"not($p)"
    case Sub(p, q) => s"$p - $q"
    case GreaterThan(p, q) => s"$p > $q"
    case PInt(i) => s"$i"
    case PBool(b) => s"$b()"
    case PString(s) => s""""$s""""
  }
}

case class Seq(p: Path, q: Path) extends Path
case class Name(s: String) extends Path
case class Attribute(s: String) extends Path
case class Filter(p: Path) extends Path
case class PAxis(a: Axis) extends Path
case class Union(p: Path, q: Path) extends Path
case class For(vars: List[(Var, Path)], ret: Path) extends Path
case class Var(s: String) extends Path

sealed abstract class Function extends Path
case class Not(p: Path) extends Function

sealed abstract class Operator extends Path
case class Sub(p: Path, q: Path) extends Path
case class GreaterThan(p: Path, q: Path) extends Path

sealed abstract class Constant extends Path
case class PInt(i: Int) extends Constant
case class PBool(b: Boolean) extends Constant
case class PString(s: String) extends Constant

sealed abstract class Axis
case object Self extends Axis
// case object Child extends Axis

// XXX: this isn't XPath, move to another semantic case! Path \/ Todo
case class Todo(f: Path => Path) extends Path

