package statelesser

import OpticLang.AnnotatedOpticLang

sealed abstract class VerticalLike[E[_], A]

object VerticalLike {

  case class Unk[E[_], A](e: E[A]) extends VerticalLike[E, A]

  case class LikeInt[E[_], S](i: Int) 
    extends VerticalLike[E, Getter[S, Int]]

  case class LikeBool[E[_], S](b: Boolean)
    extends VerticalLike[E, Getter[S, Boolean]]
    
  case class LikeStr[E[_], S](s: String)
    extends VerticalLike[E, Getter[S, String]]

  implicit def optimization[E[_]](implicit ev: OpticLang[E]) =
      new AnnotatedOpticLang[VerticalLike, E] {
    
    val alg = ev

    def inject[A](e: E[A]) = Unk(e)

    def run[A](ann: VerticalLike[E, A]) = ann match {
      case Unk(e) => e
      case LikeInt(i) => alg.likeInt(i)
      case LikeBool(b) => alg.likeBool(b)
      case LikeStr(s) => alg.likeStr(s)
    }

    override def gtVert[S, A, B](
        l: VerticalLike[E, Getter[S, A]],
        r: VerticalLike[E, Getter[A, B]]) = (l, r) match {
      case (_, LikeInt(i)) => inject(alg.likeInt(i))
      case (_, LikeBool(b)) => inject(alg.likeBool(b))
      case (_, LikeStr(s)) => inject(alg.likeStr(s))
      case _ => inject(alg.gtVert(run(l), run(r)))
    }

    override def likeInt[S](i: Int) = LikeInt(i)

    override def likeBool[S](b: Boolean) = LikeBool(b)

    override def likeStr[S](s: String) = LikeStr(s)
  }
}

