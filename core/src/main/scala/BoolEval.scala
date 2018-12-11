package statelesser

import OpticLang.AnnotatedOpticLang

sealed abstract class BoolEval[E[_], A]

object BoolEval {

  case class Unk[E[_], A](e: E[A]) extends BoolEval[E, A]

  case class LikeBool[E[_], S](b: Boolean) 
    extends BoolEval[E, Getter[S, Boolean]]

  case class Not[E[_]](d: E[Getter[Boolean, Boolean]]) 
    extends BoolEval[E, Getter[Boolean, Boolean]]

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) =
      new AnnotatedOpticLang[BoolEval, E] {

    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: BoolEval[E, A]) = ann match {
      case Unk(e) => e
      case LikeBool(b) => alg.likeBool(b)
      case Not(_) => alg.not
    }

    override def gtVert[S, A, B](
        l: BoolEval[E, Getter[S, A]],
        r: BoolEval[E, Getter[A, B]]) = (l, r) match {
      case (LikeBool(b), Not(_)) => inject(alg.likeBool(!b))
      case _ => inject(alg.gtVert(run(l), run(r)))
    }

    override def not = Not(alg.not)

    override def likeBool[S](b: Boolean) = LikeBool(b)
  }
}

