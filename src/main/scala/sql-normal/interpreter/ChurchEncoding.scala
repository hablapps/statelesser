package statelesser
package sqlnormal
package interpreter

import scalaz._, Scalaz._

class ChurchEncoding {

  // XXX: Feels like a brand new DSL! Therefore, this shouldn't be located here.

  sealed abstract class Normal[S, A]

  object Normal {

    def not: Normal[Boolean, Boolean] = NNot()

    def sub: Normal[(Int, Int), Int] = NSub()

    def gt: Normal[(Int, Int), Boolean] = NGt()

    def optic[S, A](ot: OpticType[S, A]): Normal[S, A] = NOptic(ot)

    def sym[S, A](s: Symbol): Normal[S, A] = NSym(s)
  }

  case class NVertical[S, A, B](
    u: Normal[S, A], 
    d: Normal[A, B]) extends Normal[S, B]

  case class NHorizontal[S, A, B](
    l: Normal[S, A],
    r: Normal[S, B]) extends Normal[S, (A, B)]

  case class NLikeInt[S](i: Int) extends Normal[S, Int]

  case class NLikeBool[S](b: Boolean) extends Normal[S, Boolean]

  case class NLikeStr[S](s: String) extends Normal[S, String]

  case class NNot() extends Normal[Boolean, Boolean]

  case class NSub() extends Normal[(Int, Int), Int]

  case class NGt() extends Normal[(Int, Int), Boolean]

  case class NSym[S, A](s: Symbol) extends Normal[S, A]

  case class NOptic[S, A](ot: OpticType[S, A]) extends Normal[S, A]

  def varAsNormal[S, A](
      tvar: TVar[S, A], 
      tmap: TVarMap): Option[Normal[S, A]] = tvar match {
    case RootVar(op) => op.getOption(tmap).map(tvt => Normal.sym(tvt.label))
    case v@Var(top, _) => (varAsNormal(top, tmap) |@| v.apply.getOption(tmap)) {
      case (u, tvt) => NVertical(u, Normal.sym(tvt.label))
    }
  }

  def asNormal[S, A](
      texpr: TExpr[S, A],
      tmap: TVarMap): Option[Normal[S, A]] = texpr match {
    case LikeInt(i) => Option(NLikeInt(i))
    case LikeBool(b) => Option(NLikeBool(b))
    case LikeStr(s) => Option(NLikeStr(s))
    case Not(b, is) => 
      asNormal(b, tmap).map(NVertical(_, is.flip.subst(Normal.not)))
    case Sub(l, r, is) => (asNormal(l, tmap) |@| asNormal(r, tmap)) { (nl, nr) =>
      NVertical(NHorizontal(nl, nr), is.flip.subst(Normal.sub))
    }
    case Gt(l, r, is) => (asNormal(l, tmap) |@| asNormal(r, tmap)) { (nl, nr) =>
      NVertical(NHorizontal(nl, nr), is.flip.subst(Normal.gt))
    }
    case v: TVar[S, A] => varAsNormal(v, tmap)
    case Select(v, ot) => varAsNormal(v, tmap).map(NVertical(_, Normal.optic(ot)))
  }
}

