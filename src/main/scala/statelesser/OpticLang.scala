package statelesser

import Function.const
import scalaz._, Scalaz._
import Leibniz._

import optic._

trait OpticLang[Expr[_]] {

  def flVert[S, A, B](
    u: Expr[Fold[S, A]],
    d: Expr[Fold[A, B]]): Expr[Fold[S, B]]

  def flHori[S, A, B](
    l: Expr[Fold[S, A]],
    r: Expr[Fold[S, B]]): Expr[Fold[S, (A, B)]]

  def gtVert[S, A, B](
    u: Expr[Getter[S, A]],
    d: Expr[Getter[A, B]]): Expr[Getter[S, B]]

  def gtHori[S, A, B](
    l: Expr[Getter[S, A]],
    r: Expr[Getter[S, B]]): Expr[Getter[S, (A, B)]]

  def aflVert[S, A, B](
    u: Expr[AffineFold[S, A]],
    d: Expr[AffineFold[A, B]]): Expr[AffineFold[S, B]]

  def aflHori[S, A, B](
    l: Expr[AffineFold[S, A]],
    r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, (A, B)]]

  def filtered[S](p: Expr[Getter[S, Boolean]]): Expr[AffineFold[S, S]]

  def sub: Expr[Getter[(Int, Int), Int]]

  def greaterThan: Expr[Getter[(Int, Int), Boolean]]

  def first[A, B]: Expr[Getter[(A, B), A]]

  def second[A, B]: Expr[Getter[(A, B), B]]

  def likeInt[S](i: Int): Expr[Getter[S, Int]]

  def likeBool[S](b: Boolean): Expr[Getter[S, Boolean]]

  def likeStr[S](s: String): Expr[Getter[S, String]]

  def id[S]: Expr[Getter[S, S]]

  def not: Expr[Getter[Boolean, Boolean]]

  def gtAsAfl[S, A](gt: Expr[Getter[S, A]]): Expr[AffineFold[S, A]]

  def aflAsFl[S, A](afl: Expr[AffineFold[S, A]]): Expr[Fold[S, A]]

  // derived methods

  def gt(i: Int): Expr[Getter[Int, Boolean]] =
    gtVert(gtHori(id, likeInt(i)), greaterThan)

  def gtAsFl[S, A](gt: Expr[Getter[S, A]]): Expr[Fold[S, A]] =
    aflAsFl(gtAsAfl(gt))
}

object OpticLang {

  def apply[E[_]](implicit ev: OpticLang[E]): OpticLang[E] = ev

  implicit val prettyPrinter = new OpticLang[Const[String, ?]] {

    def flVert[S, A, B](
        u: Const[String, Fold[S, A]],
        d: Const[String, Fold[A, B]]): Const[String, Fold[S, B]] =
      Const(s"${u.getConst} > ${d.getConst}")

    def flHori[S, A, B](
        l: Const[String, Fold[S, A]],
        r: Const[String, Fold[S, B]]): Const[String, Fold[S, (A, B)]] =
      Const(s"${l.getConst} * ${r.getConst}")

    def gtVert[S, A, B](
        u: Const[String, Getter[S, A]],
        d: Const[String, Getter[A, B]]): Const[String, Getter[S, B]] =
      Const(s"${u.getConst} > ${d.getConst}")

    def gtHori[S, A, B](
        l: Const[String, Getter[S, A]],
        r: Const[String, Getter[S, B]]): Const[String, Getter[S, (A, B)]] =
      Const(s"(${l.getConst} * ${r.getConst})")

    def aflVert[S, A, B](
        u: Const[String, AffineFold[S, A]],
        d: Const[String, AffineFold[A, B]]): Const[String, AffineFold[S, B]] =
      Const(s"${u.getConst} > ${d.getConst}")

    def aflHori[S, A, B](
        l: Const[String, AffineFold[S, A]],
        r: Const[String, AffineFold[S, B]]): Const[String, AffineFold[S, (A, B)]] =
      Const(s"${l.getConst} * ${r.getConst}")

    def filtered[S](
        p: Const[String, Getter[S, Boolean]]): Const[String, AffineFold[S, S]] =
      Const(s"filtered(${p.getConst})")

    def sub: Const[String, Getter[(Int, Int), Int]] =
      Const("sub")

    def greaterThan: Const[String, Getter[(Int, Int), Boolean]] =
      Const("greaterThan")

    def first[A, B]: Const[String, Getter[(A, B), A]] =
      Const("first")

    def second[A, B]: Const[String, Getter[(A, B), B]] =
      Const("second")

    def likeInt[S](i: Int): Const[String, Getter[S, Int]] =
      Const(s"likeInt(${i.toString})")

    def likeBool[S](b: Boolean): Const[String, Getter[S, Boolean]] =
      Const(s"likeBool(${b.toString})")

    def likeStr[S](s: String): Const[String, Getter[S, String]] =
      Const(s"""likeStr("$s")""")

    def id[S]: Const[String, Getter[S, S]] =
      Const("id")

    def not: Const[String, Getter[Boolean, Boolean]] =
      Const("not")

    def gtAsAfl[S, A](
        gt: Const[String, Getter[S, A]]): Const[String, AffineFold[S, A]] =
      Const(s"${gt.getConst}.asAffineFold")

    def aflAsFl[S, A](
        afl: Const[String, AffineFold[S, A]]): Const[String, Fold[S, A]] =
      Const(s"${afl.getConst}.asFold")
  }

  implicit def metacircular: OpticLang[Id] = new OpticLang[Id] {

    def flVert[S, A, B](u: Fold[S, A], d: Fold[A, B]): Fold[S, B] =
      u > d

    def flHori[S, A, B](l: Fold[S, A], r: Fold[S, B]): Fold[S, (A, B)] =
      l * r

    def gtVert[S, A, B](u: Getter[S, A], d: Getter[A, B]): Getter[S, B] =
      u > d

    def gtHori[S, A, B](l: Getter[S, A], r: Getter[S, B]): Getter[S, (A, B)] =
      l * r

    def aflVert[S, A, B](
        u: AffineFold[S, A], 
        d: AffineFold[A, B]): AffineFold[S, B] =
      u > d

    def aflHori[S, A, B](
        l: AffineFold[S, A],
        r: AffineFold[S, B]): AffineFold[S, (A, B)] =
      l * r

    def filtered[S](p: Getter[S, Boolean]): AffineFold[S, S] =
      AffineFold.filtered(p)

    def sub: Getter[(Int, Int), Int] = Getter.sub

    def greaterThan: Getter[(Int, Int), Boolean] = Getter.gt

    def first[A, B]: Getter[(A, B), A] = Getter.first

    def second[A, B]: Getter[(A, B), B] = Getter.second

    def likeInt[S](i: Int): Getter[S, Int] = Getter.like(i)

    def likeBool[S](b: Boolean): Getter[S, Boolean] = Getter.like(b)

    def likeStr[S](s: String): Getter[S, String] = Getter.like(s)

    def id[S]: Getter[S, S] = Getter.id

    def not: Getter[Boolean, Boolean] = Getter.not

    def gtAsAfl[S, A](gt: Getter[S, A]): AffineFold[S, A] = gt.asAffineFold

    def aflAsFl[S, A](afl: AffineFold[S, A]): Fold[S, A] = afl.asFold
  }

  implicit def tsemantic: OpticLang[Semantic] = new OpticLang[Semantic] {

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
      p.map(sem => new Todo[AffineFold, S, S] {
        def apply[T](done: Done[AffineFold, T, S]) = sem match {
          case todo: Todo[Getter, S, Boolean] => {
            // We're just selecting a unique Boolean, so must be `Just` 
            // XXX: weird, turning an `AffineFold` into a `Getter`. It's just a
            // phantom type so it won't fail, but conceptually, this is wrong.
            val Just(e) = todo(Done[Getter, T, S](
              done.expr, 
              done.filt, 
              done.vars)).expr
            done.copy(filt = done.filt + e)
          }
        }
      })

    def sub: Semantic[Getter[(Int, Int), Int]] =
      state(new Todo[Getter, (Int, Int), Int] {
        def apply[S](done: Done[Getter, S, (Int, Int)]) = done.expr match {
          case pair: Pair[S, (Int, Int), Int, Int]@unchecked =>
            Done(Just((pair.l, pair.r) match {
              case (Just(l), Just(LikeInt(0))) => l
              case (Just(LikeInt(x)), Just(LikeInt(y))) => LikeInt(x - y)
              case (Just(l), Just(r)) => Sub(l, r, implicitly)
            }), done.filt, done.vars)
        }
      })

    def greaterThan: Semantic[Getter[(Int, Int), Boolean]] =
      state(new Todo[Getter, (Int, Int), Boolean] {
        def apply[S](done: Done[Getter, S, (Int, Int)]) = done.expr match {
          case pair: Pair[S, (Int, Int), Int, Int]@unchecked =>
            Done(Just((pair.l, pair.r) match {
              case (Just(LikeInt(x)), Just(LikeInt(y))) => LikeBool(x > y)
              case (Just(l), Just(r)) => Gt(l, r, implicitly)
            }), done.filt, done.vars)
        }
      })

    def first[A, B]: Semantic[Getter[(A, B), A]] =
      state(new Todo[Getter, (A, B), A] {
        def apply[S](done: Done[Getter, S, (A, B)]) = done.expr match {
          case pair: Pair[S, (A, B), A, B]@unchecked =>
            cleanUnusedVars(Done(pair.l, done.filt, done.vars))
        }
      })

    def second[A, B]: Semantic[Getter[(A, B), B]] =
      state(new Todo[Getter, (A, B), B] {
        def apply[S](done: Done[Getter, S, (A, B)]) = done.expr match {
          case pair: Pair[S, (A, B), A, B]@unchecked =>
            cleanUnusedVars(Done(pair.r, done.filt, done.vars))
        }
      })

    def likeInt[S](i: Int): Semantic[Getter[S, Int]] =
      state(new Todo[Getter, S, Int] {
        def apply[T](done: Done[Getter, T, S]) =
          Done(Just(LikeInt(i)), done.filt, done.vars)
      })
 
    def likeBool[S](b: Boolean): Semantic[Getter[S, Boolean]] =
      state(new Todo[Getter, S, Boolean] {
        def apply[T](done: Done[Getter, T, S]) =
          Done(Just(LikeBool(b)), done.filt, done.vars)
      })

    def likeStr[S](s: String): Semantic[Getter[S, String]] =
      state(new Todo[Getter, S, String] {
        def apply[T](done: Done[Getter, T, S]) =
          Done(Just(LikeStr(s)), done.filt, done.vars)
      })

    def id[S]: Semantic[Getter[S, S]] =
      state(new Todo[Getter, S, S] {
        def apply[T](done: Done[Getter, T, S]) = done
      })

    def not: Semantic[Getter[Boolean, Boolean]] =
      state(new Todo[Getter, Boolean, Boolean] {
        def apply[S](done: Done[Getter, S, Boolean]) = done.expr match {
          case just: Just[S, Boolean] =>
            Done(Just(just.e match {
              case LikeBool(b) => LikeBool(!b)
              case Not(b, _) => b
            }), done.filt, done.vars)
        }
      })

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

    private def unifyVars[O[_, _], S, A](done: Done[O, S, A]): Done[O, S, A] = {
      val (t, rws) = done.vars
        .groupBy(_._2)
        .map { case (k, v) => (k, v.keys.toList) }
        .foldLeft((Map.empty[Symbol, Value], Set.empty[(String, String)])) {
          case ((m, rws), (v, k :: ks)) =>
            (m + (k -> v), rws ++ ks.map((_, k)))
        }
      if (rws.isEmpty) done
      else unifyVars(Done(
        done.expr.renameVars(rws), 
        done.filt.map(_.renameVars(rws)), 
        t))
    }

    private def cleanUnusedVars[O[_, _], S, A](
        done: Done[O, S, A]): Done[O, S, A] = {
      def deps(k: String): Set[String] =
        done.vars(k).fold(const(Set()), sel => deps(sel.v.sym) + sel.v.sym)
      val used: Set[String] =
        done.expr.vars ++ done.filt.flatMap(_.vars)
      done.copy(vars = done.vars
        .filterKeys((used ++ (used.flatMap(deps))).contains(_)))
    }

    private def cart[O[_, _], S, A, B](
        lsem: Semantic[O[S, A]],
        rsem: Semantic[O[S, B]]): Semantic[O[S, (A, B)]] =
      for {
        sem1 <- lsem
        sem2 <- rsem
      } yield (sem1, sem2) match {
        case (ltodo: Todo[O, S, A], rtodo: Todo[O, S, B]) => 
          new Todo[O, S, (A, B)] {
            def apply[T](done: Done[O, T, S]) = (ltodo(done), rtodo(done)) match {
              case (Done(le, lf, lv), Done(re, rf, rv)) =>
                unifyVars(Done[O, T, (A, B)](
                  Pair[T, (A, B), A, B](le, re, implicitly), 
                  lf ++ rf, 
                  lv ++ rv))
            }
          }
        case (Done(le, lf, lv), Done(re, rf, rv)) =>
          Done(Pair[S, (A, B), A, B](le, re, implicitly), lf ++ rf, lv ++ rv)
      }
  }
 
  def fresh: State[Stream[String], String] =
    for {
      s <- get[Stream[String]]
      _ <- modify[Stream[String]](_.tail)
    } yield s.head

  trait Syntax {

    implicit class FoldOps[Expr[_], S, A](
        l: Expr[Fold[S, A]])(implicit
        O: OpticLang[Expr]) {
      def >[B](r: Expr[Fold[A, B]]): Expr[Fold[S, B]] = O.flVert(l, r)
      def *[B](r: Expr[Fold[S, B]]): Expr[Fold[S, (A, B)]] = O.flHori(l, r)
    }

    implicit class AffineFoldOps[Expr[_], S, A](
        l: Expr[AffineFold[S, A]])(implicit
        O: OpticLang[Expr]) {
      def asFold: Expr[Fold[S, A]] = O.aflAsFl(l)
      def >[B](r: Expr[AffineFold[A, B]]): Expr[AffineFold[S, B]] =
        O.aflVert(l, r)
      def *[B](r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, (A, B)]] =
        O.aflHori(l, r)
      def <*[B](r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, A]] =
        l * r > O.first.asAffineFold
      def *>[B](r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, B]] =
        l * r > O.second.asAffineFold
    }

    implicit class GetterOps[Expr[_], S, A](
        l: Expr[Getter[S, A]])(implicit
        O: OpticLang[Expr]) {
      def asAffineFold: Expr[AffineFold[S, A]] = O.gtAsAfl(l)
      def asFold: Expr[Fold[S, A]] = l.asAffineFold.asFold
      def >[B](r: Expr[Getter[A, B]]): Expr[Getter[S, B]] = O.gtVert(l, r)
      def *[B](r: Expr[Getter[S, B]]): Expr[Getter[S, (A, B)]] = O.gtHori(l, r)
    }

    implicit class IntGetterOps[Expr[_], S](
        l: Expr[Getter[S, Int]])(implicit
        O: OpticLang[Expr]) {
      def -(r: Expr[Getter[S, Int]]): Expr[Getter[S, Int]] = l * r > O.sub
    }
  }

  object syntax extends Syntax
}

