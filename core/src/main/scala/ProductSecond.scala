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

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) = 
      new AnnotatedOpticLang[ProductSecond, E] {

    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: ProductSecond[E, A]) = ann match {
      case Unk(e) => e
      case Product(l, r) => alg.gtHori(l, r)
      case Second(_) => alg.second
    }

    override def gtVert[S, A, B](
        l: ProductSecond[E, Getter[S, A]], 
        r: ProductSecond[E, Getter[A, B]]): ProductSecond[E, Getter[S, B]] =
      (l, r) match {
        case (Product(_, e), Second(_)) => inject(e)
        case _ => Unk(alg.gtVert(run(l), run(r)))
      }

    override def gtHori[S, A, B](
        l: ProductSecond[E, Getter[S, A]],
        r: ProductSecond[E, Getter[S, B]]): ProductSecond[E, Getter[S, (A, B)]] =
      Product(run(l), run(r))

    override def second[A, B]: ProductSecond[E, Getter[(A, B), B]] = 
      Second(alg.second)
  }
}

