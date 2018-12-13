package statelesser

import OpticLang.AnnotatedOpticLang

sealed abstract class DistAs[E[_], A]

object DistAs {
  
  case class Unk[E[_], A](e: E[A]) extends DistAs[E, A]

  case class ProductGt[E[_], S, A, B](
    l: E[Getter[S, A]],
    r: E[Getter[S, B]]) extends DistAs[E, Getter[S, (A, B)]]

  case class VerticalGt[E[_], S, A, B](
    u: E[Getter[S, A]],
    d: E[Getter[A, B]]) extends DistAs[E, Getter[S, B]]

  case class ProductAfl[E[_], S, A, B](
    l: E[AffineFold[S, A]],
    r: E[AffineFold[S, B]]) extends DistAs[E, AffineFold[S, (A, B)]]

  case class VerticalAfl[E[_], S, A, B](
    u: E[AffineFold[S, A]],
    d: E[AffineFold[A, B]]) extends DistAs[E, AffineFold[S, B]]

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) =
      new AnnotatedOpticLang[DistAs, E] {

    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: DistAs[E, A]) = ann match {
      case Unk(e) => e
      case ProductGt(l, r) => alg.gtHori(l, r)
      case VerticalGt(u, d) => alg.gtVert(u, d)
      case ProductAfl(l, r) => alg.flHori(l, r)
      case VerticalAfl(u, d) => alg.flVert(u, d)
    }

    override def gtVert[S, A, B](
        u: DistAs[E, Getter[S, A]],
        d: DistAs[E, Getter[A, B]]) =
      VerticalGt(run(u), run(d))
    
    override def gtHori[S, A, B](
        l: DistAs[E, Getter[S, A]],
        r: DistAs[E, Getter[S, B]]) =
      ProductGt(run(l), run(r))

    override def aflVert[S, A, B](
        u: DistAs[E, AffineFold[S, A]],
        d: DistAs[E, AffineFold[A, B]]) =
      VerticalAfl(run(u), run(d))
    
    override def aflHori[S, A, B](
        l: DistAs[E, AffineFold[S, A]], 
        r: DistAs[E, AffineFold[S, B]]) =
      ProductAfl(run(l), run(r))

    override def gtAsAfl[S, A](gt: DistAs[E, Getter[S, A]]) = gt match {
      case ProductGt(l, r) => inject(alg.aflHori(alg.gtAsAfl(l), alg.gtAsAfl(r)))
      case VerticalGt(u, d) => inject(alg.aflVert(alg.gtAsAfl(u), alg.gtAsAfl(d)))
      case _ => inject(alg.gtAsAfl(run(gt)))
    }

    override def aflAsFl[S, A](afl: DistAs[E, AffineFold[S, A]]) = afl match {
      case ProductAfl(l, r) => inject(alg.flHori(alg.aflAsFl(l), alg.aflAsFl(r)))
      case VerticalAfl(u, d) => inject(alg.flVert(alg.aflAsFl(u), alg.aflAsFl(d)))
      case _ => inject(alg.aflAsFl(run(afl)))
    }
  }
}

