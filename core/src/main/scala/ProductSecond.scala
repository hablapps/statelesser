package statelesser

import OpticLang.AnnotatedOpticLang

sealed abstract class ProductSecond[E[_], A]

object ProductSecond {

  case class Unk[E[_], A](e: E[A]) extends ProductSecond[E, A]

  case class Product[E[_], S, A, B](
    l: E[Getter[S, A]], 
    r: E[Getter[S, B]]) extends ProductSecond[E, Getter[S, (A, B)]]

  case class Second[E[_], A, B](
      d: E[Getter[(A, B), B]]) 
    extends ProductSecond[E, Getter[(A, B), B]]

  case class SecondAndNext[E[_], A, B, C](
      d: E[Getter[(A, B), B]], 
      e: E[Getter[B, C]])
    extends ProductSecond[E, Getter[(A, B), C]]

  // Scala WTF of the day: landing types with Scala!

  def landing1[E[_]: OpticLang, A, B, C](
      d: E[Getter[(A, B), B]],
      e: ProductSecond[E, Getter[B, C]]): SecondAndNext[E, A, B, C] = 
    SecondAndNext[E, A, B, C](OpticLang[E].second, optimization.run(e))
    
  def landing2[E[_]: OpticLang, A, B, C, D](
      d: E[Getter[(A, B), B]],
      e: E[Getter[B, C]],
      p: ProductSecond[E, Getter[C, D]]): SecondAndNext[E, A, B, D] =
    SecondAndNext[E, A, B, D](d, OpticLang[E].gtVert(e, optimization.run(p)))

  def landing3[E[_]: OpticLang, S, A, B](
      e1: E[Getter[S, A]], 
      e2: E[Getter[A, B]]): Unk[E, Getter[S, B]] =
    Unk(OpticLang[E].gtVert(e1, e2))

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) = 
      new AnnotatedOpticLang[ProductSecond, E] {

    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: ProductSecond[E, A]) = ann match {
      case Unk(e) => e
      case Product(l, r) => alg.gtHori(l, r)
      case Second(_) => alg.second
      case SecondAndNext(_, e) => alg.gtVert(alg.second, e)
    }

    override def gtVert[S, A, B](
        l: ProductSecond[E, Getter[S, A]], 
        r: ProductSecond[E, Getter[A, B]]): ProductSecond[E, Getter[S, B]] =
      (l, r) match {
        case (Second(d), e) => landing1(d, e)
        case (SecondAndNext(d, e1), e2) => landing2(d, e1, e2)
        case (Product(e, _), Second(_)) => Unk(e)
        case (Product(e1, _), SecondAndNext(_, e2)) => landing3(e1, e2)
        case _ => Unk(OpticLang[E].gtVert(run(l), run(r)))
      }

    override def gtHori[S, A, B](
        l: ProductSecond[E, Getter[S, A]],
        r: ProductSecond[E, Getter[S, B]]): ProductSecond[E, Getter[S, (A, B)]] =
      Product[E, S, A, B](run(l), run(r))

    override def second[A, B]: ProductSecond[E, Getter[(A, B), B]] = 
      Second(OpticLang[E].second)
  }
}

