package statelesser

import scala.reflect.runtime.universe._

import OpticLang.AnnotatedOpticLang

sealed abstract class ProductProduct[E[_], A]

object ProductProduct {

  case class Unk[E[_], A](e: E[A]) extends ProductProduct[E, A]

  case class Product[E[_], O[_, _], S, A, B](
      l: E[O[S, A]],
      r: E[O[S, B]])(
      implicit val tag: WeakTypeTag[O[_, _]]) 
    extends ProductProduct[E, O[S, (A, B)]]

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) =
      new AnnotatedOpticLang[ProductProduct, E] {

    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: ProductProduct[E, A]) = ann match {
      case Unk(e) => e
      case p@Product(l, r) if p.tag.tpe <:< weakTypeOf[Getter[_, _]] => 
        alg.gtHori(l, r)
      case p@Product(l, r) if p.tag.tpe <:< weakTypeOf[Fold[_, _]] => 
        alg.flHori(l, r)
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

    override def flVert[S, A, B](
        l: ProductProduct[E, Fold[S, A]],
        r: ProductProduct[E, Fold[A, B]]): ProductProduct[E, Fold[S, B]] =
      (l, r) match {
        case (e, Product(l, r)) =>
          inject(alg.flHori(alg.flVert(run(e), l), alg.flVert(run(e), r)))
        case _ => inject(alg.flVert(run(l), run(r)))
      }
    
    override def flHori[S, A, B](
        l: ProductProduct[E, Fold[S, A]],
        r: ProductProduct[E, Fold[S, B]]): ProductProduct[E, Fold[S, (A, B)]] =
      Product(run(l), run(r))
  }
}

