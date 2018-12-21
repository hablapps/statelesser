package statelesser

import scalaz.Leibniz._
import OpticLang._

trait RewriteVar[A] {
  def rewrite(a: A, rw: RewriteVar.Rewrite): A
}

object RewriteVar {
  import syntax._
  
  type Rewrite = (String, String)

  def apply[A](implicit ev: RewriteVar[A]): RewriteVar[A] = ev

  def make[A](f: (A, Rewrite) => A): RewriteVar[A] = new RewriteVar[A] {
    def rewrite(a: A, rw: Rewrite): A = f(a, rw) 
  }

  implicit def selRewrite[E[_], O[_, _], S, A] = make[TSel[E, O, S, A]] { 
    case (Var(x), rw) if x == rw._1 => Var(rw._2)
    case (x, _) => x 
  }

  implicit def exprRewrite[E[_], O[_, _], S, A]: RewriteVar[TExpr[E, O, S, A]] = 
    make[TExpr[E, O, S, A]] { (e, rw) =>
      e match {
        case Product(l, r, is) => Product(l.rewrite(rw), r.rewrite(rw), is)
        case Vertical(u, d) => Vertical(u.rewrite(rw), d.rewrite(rw))
        case LikeInt(i, is) => LikeInt(i, is)
        case LikeBool(b, is) => LikeBool(b, is)
        case sel: TSel[E, O, S, A] => sel.rewrite(rw)
      }
    }

  implicit def tGetterRewrite[E[_], S, A] = make[TGetter[E, S, A]] { 
    case (TGetter(expr), rw) => TGetter(expr.rewrite(rw))
  }

  implicit def tAffineFoldRewrite[E[_], S, A] = make[TAffineFold[E, S, A]] {
    case (TAffineFold(expr, filt), rw) =>
      TAffineFold(expr.rewrite(rw), filt.map(_.rewrite(rw)))
  }

  implicit def tFoldRewrite[E[_], S, A] = make[TFold[E, S, A]] {
    case (TFold(expr, filt), rw) =>
      TFold(expr.rewrite(rw), filt.map(_.rewrite(rw)))
  }
  
  trait Syntax {
    implicit class RewriteVarSyntax[A](a: A)(implicit ev: RewriteVar[A]) {
      def rewrite(rw: Rewrite): A = ev.rewrite(a, rw)
    }
  }

  object syntax extends Syntax
}

