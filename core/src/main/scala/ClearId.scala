package statelesser

import scalaz._, Leibniz._

import OpticLang.AnnotatedOpticLang

sealed abstract class ClearId[E[_], A]

object ClearId {

  case class Unk[E[_], A](e: E[A]) extends ClearId[E, A]

  case class Id[E[_], S, A](is: S === A) extends ClearId[E, Getter[S, A]]

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) =
      new AnnotatedOpticLang[ClearId, E] {
    
    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: ClearId[E, A]) = ann match {
      case Unk(e) => e
      case Id(e) => alg.id 
    }

    override def id[A] = Id(implicitly)

    override def gtVert[S, A, B](
        l: ClearId[E, Getter[S, A]],
        r: ClearId[E, Getter[A, B]]): ClearId[E, Getter[S, B]] = (l, r) match {
      case (Id(is), Unk(e)) => inject(is.flip.subst[λ[x => E[Getter[x, B]]]](e))
      case (Unk(e), Id(is)) => inject(is.subst[λ[x => E[Getter[S, x]]]](e))
      case _ => Unk(OpticLang[E].gtVert(run(l), run(r)))
    }
  }
}

