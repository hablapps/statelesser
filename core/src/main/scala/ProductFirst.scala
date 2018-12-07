package statelesser

import OpticLang.AnnotatedOpticLang

sealed abstract class ProductFirst[E[_], A]

object ProductFirst {

  case class Unk[E[_], A](e: E[A]) extends ProductFirst[E, A]

  case class Product[E[_], S, A, B](
    l: E[Getter[S, A]], 
    r: E[Getter[S, B]]) extends ProductFirst[E, Getter[S, (A, B)]]

  case class First[E[_], A, B](
      d: E[Getter[(A, B), A]]) 
    extends ProductFirst[E, Getter[(A, B), A]]

  case class FirstAndNext[E[_], A, B, C](
      d: E[Getter[(A, B), A]], 
      e: E[Getter[A, C]])
    extends ProductFirst[E, Getter[(A, B), C]]

  // Scala WTF of the day: landing types with Scala!

  def landing1[E[_]: OpticLang, A, B, C](
      d: E[Getter[(A, B), A]],
      e: ProductFirst[E, Getter[A, C]]): FirstAndNext[E, A, B, C] = 
    FirstAndNext[E, A, B, C](OpticLang[E].first, optimization.run(e))
    
  def landing2[E[_]: OpticLang, A, B, C, D](
      d: E[Getter[(A, B), A]],
      e: E[Getter[A, C]],
      p: ProductFirst[E, Getter[C, D]]): FirstAndNext[E, A, B, D] =
    FirstAndNext[E, A, B, D](d, OpticLang[E].gtVert(e, optimization.run(p)))

  def landing3[E[_]: OpticLang, S, A, B](
      e1: E[Getter[S, A]], 
      e2: E[Getter[A, B]]): Unk[E, Getter[S, B]] =
    Unk(OpticLang[E].gtVert(e1, e2))

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) = 
      new AnnotatedOpticLang[ProductFirst, E] {

    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: ProductFirst[E, A]) = ann match {
      case Unk(e) => e
      case Product(l, r) => alg.gtHori(l, r)
      case First(_) => alg.first
      case FirstAndNext(_, e) => alg.gtVert(alg.first, e)
    }

    override def gtVert[S, A, B](
        l: ProductFirst[E, Getter[S, A]], 
        r: ProductFirst[E, Getter[A, B]]): ProductFirst[E, Getter[S, B]] =
      (l, r) match {
        case (First(d), e) => landing1(d, e)
        case (FirstAndNext(d, e1), e2) => landing2(d, e1, e2)
        case (Product(e, _), First(_)) => Unk(e)
        case (Product(e1, _), FirstAndNext(_, e2)) => landing3(e1, e2)
        case _ => Unk(OpticLang[E].gtVert(run(l), run(r)))
      }

    override def gtHori[S, A, B](
        l: ProductFirst[E, Getter[S, A]],
        r: ProductFirst[E, Getter[S, B]]): ProductFirst[E, Getter[S, (A, B)]] =
      Product[E, S, A, B](run(l), run(r))

    override def first[A, B]: ProductFirst[E, Getter[(A, B), A]] = 
      First(OpticLang[E].first)
  }
}

