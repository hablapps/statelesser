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

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) = 
      new AnnotatedOpticLang[ProductFirst, E] {

    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: ProductFirst[E, A]) = ann match {
      case Unk(e) => e
      case Product(l, r) => alg.gtHori(l, r)
      case First(_) => alg.first
    }

    override def gtVert[S, A, B](
        l: ProductFirst[E, Getter[S, A]], 
        r: ProductFirst[E, Getter[A, B]]): ProductFirst[E, Getter[S, B]] =
      (l, r) match {
        case (Product(e, _), First(_)) => inject(e)
        case _ => inject(alg.gtVert(run(l), run(r)))
      }

    override def gtHori[S, A, B](
        l: ProductFirst[E, Getter[S, A]],
        r: ProductFirst[E, Getter[S, B]]): ProductFirst[E, Getter[S, (A, B)]] =
      Product(run(l), run(r))

    override def first[A, B]: ProductFirst[E, Getter[(A, B), A]] = 
      First(alg.first)
  }
}

