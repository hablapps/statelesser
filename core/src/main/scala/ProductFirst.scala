package statelesser

sealed abstract class ProductFirst[E[_], A]

object ProductFirst {

  case class Unk[E[_], A](e: E[A]) extends ProductFirst[E, A]

  case class Product[E[_], S, A, B, C](
    l: E[Getter[S, A]], 
    r: E[Getter[S, B]]) extends ProductFirst[E, Getter[S, C]]

  case class First[E[_], A, B, S]() 
    extends ProductFirst[E, Getter[S, A]]

  case class FirstAndThen[E[_], A, B, S, C](e: E[Getter[A, C]])
    extends ProductFirst[E, Getter[S, C]]

  def run[E[_], A](pf: ProductFirst[E, A])(implicit ev: OpticLang[E]): E[A] =
    pf match {
      case Unk(e) => e
      case Product(l, r) => ev.gtHori(l, r)
      case First() => ev.first
      case FirstAndThen(e) => ev.gtVert(ev.first, e)
    }

  implicit def pfOpticLang[E[_]: OpticLang] = new OpticLang[ProductFirst[E, ?]] {

    def flVert[S, A, B](
        l: ProductFirst[E, Fold[S, A]], 
        r: ProductFirst[E, Fold[A, B]]): ProductFirst[E, Fold[S, B]] =
      Unk(OpticLang[E].flVert(run(l), run(r)))

    def flHori[S, A, B](
        l: ProductFirst[E, Fold[S, A]], 
        r: ProductFirst[E, Fold[S, B]]): ProductFirst[E, Fold[S, (A, B)]] =
      Unk(OpticLang[E].flHori(run(l), run(r)))

    def gtVert[S, A, B](
        l: ProductFirst[E, Getter[S, A]], 
        r: ProductFirst[E, Getter[A, B]]): ProductFirst[E, Getter[S, B]] =
      (l, r) match {
        case (First(), e) => FirstAndThen(run(e))
        case (FirstAndThen(e1), e2) => 
          FirstAndThen(OpticLang[E].gtVert(e1, run(e2)))
        case (Product(e, _), First()) => Unk(e.asInstanceOf[E[Getter[S, B]]])
        case (Product(e1, _), FirstAndThen(e2)) =>
          Unk(OpticLang[E].gtVert(e1, e2))
        case _ => Unk(OpticLang[E].gtVert(run(l), run(r)))
      }

    def gtHori[S, A, B](
        l: ProductFirst[E, Getter[S, A]],
        r: ProductFirst[E, Getter[S, B]]): ProductFirst[E, Getter[S, (A, B)]] =
      Product[E, S, A, B, (A, B)](run(l), run(r))

    def aflVert[S, A, B](
        l: ProductFirst[E, AffineFold[S, A]], 
        r: ProductFirst[E, AffineFold[A, B]]): ProductFirst[E, AffineFold[S, B]] =
      Unk(OpticLang[E].aflVert(run(l), run(r)))

    def aflHori[S, A, B](
        l: ProductFirst[E, AffineFold[S, A]],
        r: ProductFirst[E, AffineFold[S, B]]): ProductFirst[E, AffineFold[S, (A, B)]] =
      Unk(OpticLang[E].aflHori(run(l), run(r)))

    def filtered[S](
        p: ProductFirst[E, Getter[S, Boolean]]): ProductFirst[E, AffineFold[S, S]] =
      Unk(OpticLang[E].filtered(run(p)))

    def sub: ProductFirst[E, Getter[(Int, Int), Int]] =
      Unk(OpticLang[E].sub)

    def greaterThan: ProductFirst[E, Getter[(Int, Int), Boolean]] =
      Unk(OpticLang[E].greaterThan)

    def equal[A]: ProductFirst[E, Getter[(A, A), Boolean]] = 
      Unk(OpticLang[E].equal)

    def first[A, B]: ProductFirst[E, Getter[(A, B), A]] = 
      First[E, A, B, (A, B)]()

    def second[A, B]: ProductFirst[E, Getter[(A, B), B]] = 
      Unk(OpticLang[E].second)

    def like[S, A](a: A): ProductFirst[E, Getter[S, A]] = 
      Unk(OpticLang[E].like(a))

    def id[S]: ProductFirst[E, Getter[S, S]] = 
      Unk(OpticLang[E].id)

    def not: ProductFirst[E, Getter[Boolean, Boolean]] = 
      Unk(OpticLang[E].not)

    def getAll[S, A](
        fl: ProductFirst[E, Fold[S, A]]): ProductFirst[E, S => List[A]] = 
      Unk(OpticLang[E].getAll(run(fl)))

    def lnAsGt[S, A](
        ln: ProductFirst[E, Lens[S, A]]): ProductFirst[E, Getter[S, A]] = 
      Unk(OpticLang[E].lnAsGt(run(ln)))

    def gtAsFl1[S, A](
        gt: ProductFirst[E, Getter[S, A]]): ProductFirst[E, Fold1[S, A]] = 
      Unk(OpticLang[E].gtAsFl1(run(gt)))

    def gtAsAfl[S, A](
        gt: ProductFirst[E, Getter[S, A]]): ProductFirst[E, AffineFold[S, A]] = 
      Unk(OpticLang[E].gtAsAfl(run(gt)))

    def aflAsFl[S, A](
        afl: ProductFirst[E, AffineFold[S, A]]): ProductFirst[E, Fold[S, A]] = 
      Unk(OpticLang[E].aflAsFl(run(afl)))

    def fl1AsFl[S, A](
        fl1: ProductFirst[E, Fold1[S, A]]): ProductFirst[E, Fold[S, A]] = 
      Unk(OpticLang[E].fl1AsFl(run(fl1)))
  }
}

