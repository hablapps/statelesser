package statelesser

import OpticLang.AnnotatedOpticLang

sealed abstract class IntEval[E[_], A]

object IntEval {

  case class Unk[E[_], A](e: E[A]) extends IntEval[E, A]

  case class ProductLike[E[_], S](
      d: E[Getter[S, (Int, Int)]],
      l: Int,
      r: Int) 
    extends IntEval[E, Getter[S, (Int, Int)]]

  case class Product0[E[_], S, A](
      l: E[Getter[S, A]],
      d: E[Getter[S, Int]])
    extends IntEval[E, Getter[S, (A, Int)]]

  case class Sub[E[_], A](
      d: E[Getter[(Int, Int), Int]])
    extends IntEval[E, Getter[(Int, Int), Int]]

  case class SubAndNext[E[_], A](
      d: E[Getter[(Int, Int), Int]],
      e: E[Getter[Int, A]])
    extends IntEval[E, Getter[(Int, Int), A]]

  case class LikeInt[E[_], S](
      d: E[Getter[S, Int]],
      i: Int)
    extends IntEval[E, Getter[S, Int]]

  def landing1[E[_]: OpticLang, S, A](
      e: IntEval[E, Getter[S, A]],
      d: E[Getter[S, Int]]): Product0[E, S, A] =
    Product0(optimization.run(e), OpticLang[E].likeInt(0))

  def landing2[E[_]: OpticLang, S](
      d1: E[Getter[S, Int]],
      d2: E[Getter[S, Int]],
      x: Int, y: Int): ProductLike[E, S] = {
    val ev = OpticLang[E]; import ev._
    ProductLike(gtHori(likeInt(x), likeInt(y)), x, y)
  }

  def landing3[E[_]: OpticLang, A](
      d: E[Getter[(Int, Int), Int]],
      e: IntEval[E, Getter[Int, A]]): SubAndNext[E, A] = 
    SubAndNext(OpticLang[E].sub, optimization.run(e))

  def landing4[E[_]: OpticLang, A, B](
      d: E[Getter[(Int, Int), Int]],
      e: E[Getter[Int, A]],
      p: IntEval[E, Getter[A, B]]): SubAndNext[E, B] =
    SubAndNext(d, OpticLang[E].gtVert(e, optimization.run(p)))

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) =
      new AnnotatedOpticLang[IntEval, E] {
    
    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: IntEval[E, A]) = ann match {
      case Unk(e) => e
      case Product0(l, r) => alg.gtHori(l, r)
      case ProductLike(_, l, r) => alg.gtHori(l, r)
      case Sub(_) => alg.sub
      case SubAndNext(_, e) => alg.gtVert(alg.sub, e)
      case LikeInt(_, i) => alg.likeInt(i)
    }

    override def gtVert[S, A, B](
        l: IntEval[E, Getter[S, A]],
        r: IntEval[E, Getter[A, B]]) = (l, r) match {
      case (Sub(d), e) => landing3(d, e)
      case (SubAndNext(d, e1), e2) => landing4(d, e1, e2)
      case (Product0(e, _), Sub(_)) => inject(e)
      case (ProductLike(_, x, y), Sub(_)) => inject(alg.likeInt(x - y))
      case (Product0(e1, _), SubAndNext(_, e2)) =>
        inject(alg.gtVert(e1, e2))
      case (ProductLike(_, x, y), SubAndNext(_, e)) =>
        inject(alg.gtVert(alg.likeInt(x-y), e))
      case _ => inject(alg.gtVert(run(l), run(r)))
    }

    override def gtHori[S, A, B](
        l: IntEval[E, Getter[S, A]],
        r: IntEval[E, Getter[S, B]]) = (l, r) match {
      case (e, LikeInt(d, 0)) => landing1(e, d)
      case (LikeInt(d1, x), LikeInt(d2, y)) => landing2(d1, d2, x, y)
      case _ => inject(alg.gtHori(run(l), run(r)))
    }

    override def likeInt[S](i: Int) = LikeInt(alg.likeInt(i), i)

    override def sub = Sub(alg.sub)
  }
}

