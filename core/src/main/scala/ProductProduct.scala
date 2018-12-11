package statelesser

import OpticLang.AnnotatedOpticLang

sealed abstract class ProductProduct[E[_], A]

object ProductProduct {

  case class Unk[E[_], A](e: E[A]) extends ProductProduct[E, A]

  case class Product[E[_], S, A, B](
    l: E[Getter[S, A]],
    r: E[Getter[S, B]]) extends ProductProduct[E, Getter[S, (A, B)]]

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) =
      new AnnotatedOpticLang[ProductProduct, E] {

    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: ProductProduct[E, A]) = ann match {
      case Unk(e) => e
      case Product(l, r) => alg.gtHori(l, r)
    }

    override def gtVert[S, A, B](
        l: ProductProduct[E, Getter[S, A]],
        r: ProductProduct[E, Getter[A, B]]): ProductProduct[E, Getter[S, B]] =
      (l, r) match {
        case (e, Product(l, r)) => 
          inject(alg.gtHori(alg.gtVert(run(e), l), alg.gtVert(run(e), r)))
        case _ => inject(alg.gtVert(run(l), run(r)))
      }
    
    override def gtHori[S, A, B](
        l: ProductProduct[E, Getter[S, A]],
        r: ProductProduct[E, Getter[S, B]]): ProductProduct[E, Getter[S, (A, B)]] =
      Product(run(l), run(r))
  }
}

