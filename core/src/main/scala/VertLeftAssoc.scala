package statelesser

import OpticLang.AnnotatedOpticLang

sealed abstract class VertLeftAssoc[E[_], A]

object VertLeftAssoc {
  
  case class Unk[E[_], A](e: E[A]) extends VertLeftAssoc[E, A]

  case class Vertical[E[_], S, A, B](
    l: E[Getter[S, A]],
    r: E[Getter[A, B]]) extends VertLeftAssoc[E, Getter[S, B]]

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) =
      new AnnotatedOpticLang[VertLeftAssoc, E] {

    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: VertLeftAssoc[E, A]) = ann match {
      case Vertical(l, r) => alg.gtVert(l, r)
      case Unk(e) => e
    }

    override def gtVert[S, A, B](
        l: VertLeftAssoc[E, Getter[S, A]],
        r: VertLeftAssoc[E, Getter[A, B]]) = (l, r) match {
      case (Unk(e1), Unk(e2)) => Vertical(e1, e2)
      case (ann, Vertical(e1, e2)) => 
        inject(alg.gtVert(alg.gtVert(run(ann), e1), e2))
      case _ => inject(alg.gtVert(run(l), run(r)))
    }
  }
}

