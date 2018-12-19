package statelesser

import scalaz.Leibniz._
import OpticLang._

trait RewriteVar[A] {
  def rewrite(a: A, rw: Rewrite): A
}

object RewriteVar {
  import syntax._
  
  def apply[A](implicit ev: RewriteVar[A]): RewriteVar[A] = ev

  def make[A](f: (A, Rewrite) => A): RewriteVar[A] = new RewriteVar[A] {
    def rewrite(a: A, rw: Rewrite): A = f(a, rw) 
  }

  implicit def tableRewrite[E[_], O[_, _]] = make[Table[E, O]] { (t, rw) => 
    t.map {
      case (v, e) if v == rw._1 => (rw._2, e)
      case x => x
    }
  }

  implicit def selRewrite[E[_], O[_, _], S, A] = make[TSel[E, O, S, A]] { 
    case (Var(x), rw) if x == rw._1 => Var(rw._2)
    case (x, _) => x 
  }

  private def gtProductLand[E[_], S, A, B, C](
      l: TGetter[E, S, A],
      r: TGetter[E, S, B],
      is: (A, B) === C,
      rw: Rewrite): Product[E, Getter, S, A, B, C] =
    Product(
      tGetterRewrite[E, S, A].rewrite(l, rw), 
      tGetterRewrite[E, S, B].rewrite(r, rw), 
      is)

  private def gtVerticalLand[E[_], S, A, B](
      l: TGetter[E, S, A],
      r: TGetter[E, A, B],
      rw: Rewrite): Vertical[E, Getter, S, A, B] =
    Vertical(
      tGetterRewrite[E, S, A].rewrite(l, rw), 
      tGetterRewrite[E, A, B].rewrite(r, rw))

  implicit def gtExprRewrite[E[_], S, A] = 
    make[TExpr[E, Getter, S, A]] { (e, rw) =>
      e match {
        case Product(l@TGetter(_, _), r@TGetter(_, _), is) => 
          gtProductLand(l, r, is, rw)
        case Vertical(u@TGetter(_, _), d@TGetter(_, _)) => 
          gtVerticalLand(u, d, rw)
        case LikeInt(i, is) => LikeInt(i, is)
        case LikeBool(b, is) => LikeBool(b, is)
        case sel: TSel[E, Getter, S, A] => sel.rewrite(rw)
      }
    }

  private def aflProductLand[E[_], S, A, B, C](
      l: TAffineFold[E, S, A],
      r: TAffineFold[E, S, B],
      is: (A, B) === C,
      rw: Rewrite): Product[E, AffineFold, S, A, B, C] =
    Product(
      tAffineFoldRewrite[E, S, A].rewrite(l, rw), 
      tAffineFoldRewrite[E, S, B].rewrite(r, rw), 
      is)

  private def aflVerticalLand[E[_], S, A, B](
      l: TAffineFold[E, S, A],
      r: TAffineFold[E, A, B],
      rw: Rewrite): Vertical[E, AffineFold, S, A, B] =
    Vertical(
      tAffineFoldRewrite[E, S, A].rewrite(l, rw), 
      tAffineFoldRewrite[E, A, B].rewrite(r, rw))

  implicit def aflExprRewrite[E[_], S, A] = 
    make[TExpr[E, AffineFold, S, A]] { (e, rw) =>
      e match {
        case Product(l@TAffineFold(_, _, _), r@TAffineFold(_, _, _), is) => 
          aflProductLand(l, r, is, rw)
        case Vertical(u@TAffineFold(_, _, _), d@TAffineFold(_, _, _)) => 
          aflVerticalLand(u, d, rw)
        case LikeInt(i, is) => LikeInt(i, is)
        case LikeBool(b, is) => LikeBool(b, is)
        case sel: TSel[E, AffineFold, S, A] => sel.rewrite(rw)
      }
    }

  private def flProductLand[E[_], S, A, B, C](
      l: TFold[E, S, A],
      r: TFold[E, S, B],
      is: (A, B) === C,
      rw: Rewrite): Product[E, Fold, S, A, B, C] =
    Product(
      tFoldRewrite[E, S, A].rewrite(l, rw), 
      tFoldRewrite[E, S, B].rewrite(r, rw), 
      is)

  private def flVerticalLand[E[_], S, A, B](
      l: TFold[E, S, A],
      r: TFold[E, A, B],
      rw: Rewrite): Vertical[E, Fold, S, A, B] =
    Vertical(
      tFoldRewrite[E, S, A].rewrite(l, rw), 
      tFoldRewrite[E, A, B].rewrite(r, rw))

  implicit def flExprRewrite[E[_], S, A] = 
    make[TExpr[E, Fold, S, A]] { (e, rw) =>
      e match {
        case Product(l@TFold(_, _, _), r@TFold(_, _, _), is) => 
          flProductLand(l, r, is, rw)
        case Vertical(u@TFold(_, _, _), d@TFold(_, _, _)) => 
          flVerticalLand(u, d, rw)
        case LikeInt(i, is) => LikeInt(i, is)
        case LikeBool(b, is) => LikeBool(b, is)
        case sel: TSel[E, Fold, S, A] => sel.rewrite(rw)
      }
    }

  implicit def tGetterRewrite[E[_], S, A] = make[TGetter[E, S, A]] { 
    case (TGetter(vars, expr), rw) => 
      TGetter(vars.rewrite(rw), expr.rewrite(rw))
  }

  implicit def tAffineFoldRewrite[E[_], S, A] = make[TAffineFold[E, S, A]] {
    case (TAffineFold(vars, expr, filt), rw) =>
      TAffineFold(vars.rewrite(rw), expr.rewrite(rw), filt.map(_.rewrite(rw)))
  }

  implicit def tFoldRewrite[E[_], S, A] = make[TFold[E, S, A]] {
    case (TFold(vars, expr, filt), rw) =>
      TFold(vars.rewrite(rw), expr.rewrite(rw), filt.map(_.rewrite(rw)))
  }
  
  trait Syntax {
    implicit class RewriteVarSyntax[A](a: A)(implicit ev: RewriteVar[A]) {
      def rewrite(rw: Rewrite): A = ev.rewrite(a, rw)
    }
  }

  object syntax extends Syntax
}

