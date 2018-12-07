package statelesser

import OpticLang.AnnotatedOpticLang

sealed abstract class BoolEval[E[_], A]

object BoolEval {

  case class Unk[E[_], A](e: E[A]) extends BoolEval[E, A]

  case class LikeBool[E[_], S](b: Boolean) 
    extends BoolEval[E, Getter[S, Boolean]]

  case class Not[E[_]](d: E[Getter[Boolean, Boolean]]) 
    extends BoolEval[E, Getter[Boolean, Boolean]]

  case class NotAndNext[E[_], A](
      d: E[Getter[Boolean, Boolean]],
      e: E[Getter[Boolean, A]])
    extends BoolEval[E, Getter[Boolean, A]]

  def landing1[E[_]: OpticLang, A](
      d: E[Getter[Boolean, Boolean]],
      e: BoolEval[E, Getter[Boolean, A]]): NotAndNext[E, A] =
    NotAndNext[E, A](d, optimization.run(e))

  def landing2[E[_]: OpticLang, A, B](
      d: E[Getter[Boolean, Boolean]],
      e: E[Getter[Boolean, A]],
      b: BoolEval[E, Getter[A, B]]): NotAndNext[E, B] =
    NotAndNext(d, OpticLang[E].gtVert(e, optimization.run(b)))

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) =
      new AnnotatedOpticLang[BoolEval, E] {

    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: BoolEval[E, A]) = ann match {
      case Unk(e) => e
      case LikeBool(b) => alg.likeBool(b)
      case Not(_) => alg.not
      case NotAndNext(_, e) => alg.gtVert(alg.not, e)
    }

    override def gtVert[S, A, B](
        l: BoolEval[E, Getter[S, A]],
        r: BoolEval[E, Getter[A, B]]) = (l, r) match {
      case (Not(d), e) => landing1(d, e)
      case (NotAndNext(d, e1), e2) => landing2(d, e1, e2)
      case (LikeBool(b), Not(_)) => inject(alg.likeBool(!b))
      case (LikeBool(b), NotAndNext(_, e)) => 
        inject(alg.gtVert(alg.likeBool(!b), e))
      case _ => inject(alg.gtVert(run(l), run(r)))
    }

    override def not = Not(alg.not)

    override def likeBool[S](b: Boolean) = LikeBool(b)
  }
}

