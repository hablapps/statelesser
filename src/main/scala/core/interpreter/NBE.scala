package statelesser
package core
package interpreter

import Function.const
import scalaz._, Scalaz._

import optic._
import sqlnormal._

class NBE extends Statelesser[Semantic] {

  def gtVert[S, A, B](
      usem: Semantic[Getter[S, A]],
      dsem: Semantic[Getter[A, B]]) =
    vert(usem, dsem)

  def gtHori[S, A, B](
      lsem: Semantic[Getter[S, A]],
      rsem: Semantic[Getter[S, B]]): Semantic[Getter[S, (A, B)]] =
    cart(lsem, rsem)

  def aflVert[S, A, B](
      usem: Semantic[AffineFold[S, A]],
      dsem: Semantic[AffineFold[A, B]]) =
    vert(usem, dsem)

  def aflHori[S, A, B](
      lsem: Semantic[AffineFold[S, A]],
      rsem: Semantic[AffineFold[S, B]]) =
    cart(lsem, rsem)

  def flVert[S, A, B](
      usem: Semantic[Fold[S, A]],
      dsem: Semantic[Fold[A, B]]) =
    vert(usem, dsem)

  def flHori[S, A, B](
      lsem: Semantic[Fold[S, A]],
      rsem: Semantic[Fold[S, B]]): Semantic[Fold[S, (A, B)]] =
    cart(lsem, rsem)

  def filtered[S](p: Semantic[Getter[S, Boolean]]) =
    p.map(sem => Todo(
      λ[Done[AffineFold, ?, S] ~> Done[AffineFold, ?, S]] { done =>
        sem match {
          case todo: Todo[Getter, S, Boolean] => {
            // We're just selecting a unique Boolean, so must be `Just` 
            // XXX: weird, turning an `AffineFold` into a `Getter`. It's just a
            // phantom type so it won't fail, but conceptually, this is wrong.
            val Just(e) = todo.f(Done(
              done.expr, 
              done.filt, 
              done.vars)).expr
            done.copy(filt = done.filt + e)
          }
        }
      }))

  def sub: Semantic[Getter[(Int, Int), Int]] =
    state(Todo(new (Done[Getter, ?, (Int, Int)] ~> Done[Getter, ?, Int]) {
      def apply[S](done: Done[Getter, S, (Int, Int)]) = done.expr match {
        case pair: Pair[S, (Int, Int), Int, Int]@unchecked =>
          Done(Just((pair.l, pair.r) match {
            case (Just(l), Just(LikeInt(0))) => l
            case (Just(LikeInt(x)), Just(LikeInt(y))) => LikeInt(x - y)
            case (Just(l), Just(r)) => Sub(l, r, implicitly)
          }), done.filt, done.vars)
      }
    }))

  def greaterThan: Semantic[Getter[(Int, Int), Boolean]] =
    state(Todo(new (Done[Getter, ?, (Int, Int)] ~> Done[Getter, ?, Boolean]) {
      def apply[S](done: Done[Getter, S, (Int, Int)]) = done.expr match {
        case pair: Pair[S, (Int, Int), Int, Int]@unchecked =>
          Done(Just((pair.l, pair.r) match {
            case (Just(LikeInt(x)), Just(LikeInt(y))) => LikeBool(x > y)
            case (Just(l), Just(r)) => Gt(l, r, implicitly)
          }), done.filt, done.vars)
      }
    }))

  def first[A, B]: Semantic[Getter[(A, B), A]] =
    state(Todo(new (Done[Getter, ?, (A, B)] ~> Done[Getter, ?, A]) {
      def apply[S](done: Done[Getter, S, (A, B)]) = done.expr match {
        case pair: Pair[S, (A, B), A, B]@unchecked =>
          cleanUnusedVars(Done(pair.l, done.filt, done.vars))
      }
    }))

  def second[A, B]: Semantic[Getter[(A, B), B]] =
    state(Todo(new (Done[Getter, ?, (A, B)] ~> Done[Getter, ?, B]) {
      def apply[S](done: Done[Getter, S, (A, B)]) = done.expr match {
        case pair: Pair[S, (A, B), A, B]@unchecked =>
          cleanUnusedVars(Done(pair.r, done.filt, done.vars))
      }
    }))

  def likeInt[S](i: Int): Semantic[Getter[S, Int]] =
    state(Todo(λ[Done[Getter, ?, S] ~> Done[Getter, ?, Int]] { done =>
      Done(Just(LikeInt(i)), done.filt, done.vars)
    }))

  def likeBool[S](b: Boolean): Semantic[Getter[S, Boolean]] =
    state(Todo(
      λ[Done[Getter, ?, S] ~> Done[Getter, ?, Boolean]] { done =>
        Done(Just(LikeBool(b)), done.filt, done.vars)
      }))

  def likeStr[S](s: String): Semantic[Getter[S, String]] =
    state(Todo(λ[Done[Getter, ?, S] ~> Done[Getter, ?, String]] { done =>
      Done(Just(LikeStr(s)), done.filt, done.vars)
    }))

  def id[S]: Semantic[Getter[S, S]] =
    state(Todo(λ[Done[Getter, ?, S] ~> Done[Getter, ?, S]] { done =>
      done
    }))

  def not: Semantic[Getter[Boolean, Boolean]] =
    state(Todo(new (Done[Getter, ?, Boolean] ~> Done[Getter, ?, Boolean]) {
      def apply[S](done: Done[Getter, S, Boolean]) = done.expr match {
        case just: Just[S, Boolean] =>
          Done(Just(just.e match {
            case LikeBool(b) => LikeBool(!b)
            case Not(b, _) => b
          }), done.filt, done.vars)
      }
    }))

  def gtAsAfl[S, A](gt: Semantic[Getter[S, A]]) = 
    gt.map(_ match {
      case done: Done[Getter, S, A] => done.as[AffineFold]
      case todo: Todo[Getter, S, A] => todo.as[AffineFold]
    })

  def aflAsFl[S, A](afl: Semantic[AffineFold[S, A]]) = 
    afl.map(_ match {
      case done: Done[AffineFold, S, A] => done.as[Fold]
      case todo: Todo[AffineFold, S, A] => todo.as[Fold]
    })

  private def vert[O[_, _], S, A, B](
      usem: Semantic[O[S, A]],
      dsem: Semantic[O[A, B]]): Semantic[O[S, B]] = 
    for {
      sem1 <- usem
      sem2 <- dsem
    } yield (sem1, sem2) match {
      case (x, todo: Todo[O, A, B]@unchecked) => todo compose x
      case _ => throw new Error(s"Can't vert compose with 'Done' semantic: $sem2")
    }

  // XXX: what if our lists have more elements?
  private def unifyVarTrees[O[_, _], S, A](l: TVarTree, r: TVarTree): TVarTree =
    NonEmptyList(unifyITree(l.head, r.head))

  private def unifyITree(
      l: ITree[String, (Symbol, OpticType[_, _])],
      r: ITree[String, (Symbol, OpticType[_, _])]): ITree[String, (Symbol, OpticType[_, _])] = {
    val kmap = (l.children.keySet & r.children.keySet).foldLeft(
      Map.empty[String, ITree[String, (Symbol, OpticType[_, _])]]) { (acc, k) => 
      acc + (k -> unifyITree(l.children(k), r.children(k)))
    }
    ITree(l.label, l.children ++ r.children ++ kmap)
  }

  private def cleanUnusedVars[O[_, _], S, A](
      done: Done[O, S, A]): Done[O, S, A] = {

    val used: Set[(Symbol, OpticType[_, _])] = 
      (done.expr.vars ++ done.filt.flatMap(_.vars))
        .map(_.getOption(done.vars).map(_.label))
        .flatten

    def aux(it: ITree[String, (Symbol, OpticType[_, _])])
        : ITree[String, (Symbol, OpticType[_, _])] = 
      it.copy(children = it.children.flatMap { case (k, it2) => 
        if (used.contains(it2.label)) Option(k -> aux(it2)) else None
      })

    done.copy(vars = done.vars.map(aux))
  }

  private def cart[O[_, _], S, A, B](
      lsem: Semantic[O[S, A]],
      rsem: Semantic[O[S, B]]): Semantic[O[S, (A, B)]] =
    for {
      sem1 <- lsem
      sem2 <- rsem
    } yield (sem1, sem2) match {
      case (ltodo: Todo[O, S, A], rtodo: Todo[O, S, B]) => 
        Todo(new (Done[O, ?, S] ~> Done[O, ?, (A, B)]) {
          def apply[T](done: Done[O, T, S]) = (ltodo.f(done), rtodo.f(done)) match {
            case (Done(le, lf, lv), Done(re, rf, rv)) =>
              Done[O, T, (A, B)](
                Pair[T, (A, B), A, B](le, re, implicitly), 
                lf ++ rf, 
                unifyVarTrees(lv, rv))
          }
        })
      case (Done(le, lf, lv), Done(re, rf, rv)) => Done(
        Pair[S, (A, B), A, B](le, re, implicitly), lf ++ rf, lv append rv)
    }
}

