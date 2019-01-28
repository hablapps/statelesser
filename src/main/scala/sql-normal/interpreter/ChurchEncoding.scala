package statelesser
package sqlnormal
package interpreter

import scalaz._, Scalaz._
import core.Statelesser.Church
import monocle._

class ChurchEncoding {

  // XXX: Feels like a brand new DSL! Therefore, this shouldn't be located here.

  sealed abstract class Normal[S, A]

  object Normal {

    def not: Normal[Boolean, Boolean] = NNot()

    def sub: Normal[(Int, Int), Int] = NSub()

    def gt: Normal[(Int, Int), Boolean] = NGt()

    def optic[S, A](ot: OpticType[S, A]): Normal[S, A] = NOptic(ot)

    def sym[S, A](s: Symbol, ot: OpticType[S, A]): Normal[S, A] = NSym(s, ot)
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

  case class NSym[S, A](s: Symbol, ot: OpticType[S, A]) extends Normal[S, A]

  case class NOptic[S, A](ot: OpticType[S, A]) extends Normal[S, A]

  def varAsNormal[S, A](
      tvar: TVar[S, A], 
      tmap: TVarMap): Option[Normal[S, A]] = tvar match {
    case v@RootVar(i) => 
      v.apply.getOption(tmap).map(tvt => Normal.sym(tvt.label, i))
    case v@Var(top, i) => (varAsNormal(top, tmap) |@| v.apply.getOption(tmap)) {
      case (u, tvt) => NVertical(u, Normal.sym(tvt.label, i))
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
      NVertical(mergeNormal(nl, nr), is.flip.subst(Normal.sub))
    }
    case Gt(l, r, is) => (asNormal(l, tmap) |@| asNormal(r, tmap)) { (nl, nr) =>
      NVertical(mergeNormal(nl, nr), is.flip.subst(Normal.gt))
    }
    case v: TVar[S, A] => varAsNormal(v, tmap)
    case Select(v, ot) => 
      varAsNormal(v, tmap).map(NVertical(_, Normal.optic(ot)))
  }

  def mergeNormal[S, A, B](
      l: Normal[S, A],
      r: Normal[S, B]): Normal[S, (A, B)] = (l, r) match {
    case (NVertical(NSym(s, i), dl), NVertical(NSym(t, _), dr)) if s == t =>
      NVertical(NSym(s, i), mergeNormal(dl, dr))
    case (l, r) => NHorizontal(l, r)
  }

  // XXX: using `toNel` leads us to an untyped computation
  def doneToNormal[O[_, _], S, A](done: Done[O, S, A]): Option[Normal[S, A]] =
    done.expr.toNel.foldMapLeft1(e => asNormal(e, done.vars)) { (acc, e) =>
      (acc |@| asNormal(e, done.vars))((l, r) => mergeNormal(l, r))
    }.asInstanceOf[Option[Normal[S, A]]]

  val alg = core.Statelesser[Church]

  def normalToChurch[S, A](nor: Normal[S, A]): Church[Fold[S, A]] = nor match {
    case NVertical(u, d) => alg.flVert(normalToChurch(u), normalToChurch(d))
    case NHorizontal(l, r) => alg.flHori(normalToChurch(l), normalToChurch(r))
    case NLikeInt(i) => alg.gtAsFl(alg.likeInt(i))
    case NLikeBool(b) => alg.gtAsFl(alg.likeBool(b))
    case NLikeStr(s) => alg.gtAsFl(alg.likeStr(s))
    case NNot() => alg.gtAsFl(alg.not)
    case NSub() => alg.gtAsFl(alg.sub)
    case NGt() => alg.gtAsFl(alg.greaterThan)
    case NSym(_, _) => ???
    case NOptic(ot) => ???
  }

  def tsemToChurch[A](tsem: TSemantic[A]): Option[Church[A]] = tsem match {
    case done@Done(_, _, _) => ??? // doneToChurch(done)
    case Todo(nat) => ???
  }
}

