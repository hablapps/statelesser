package org.hablapps.statelesser

import monocle._, macros.Lenses, function.all._

object Dsl {
  
  trait OpticLang[Expr[_]] {

    def point[A](a: A): Expr[A]

    def lensComposeLens[S, A, B](
      ln1: Expr[Lens[S, A]], 
      ln2: Expr[Lens[A, B]]): Expr[Lens[S, B]]

    def lensComposeTraversal[S, A, B](
      ln: Expr[Lens[S, A]],
      tr: Expr[Traversal[A, B]]): Expr[Traversal[S, B]]

    def traversalComposeLens[S, A, B](
      tr: Expr[Traversal[S, A]],
      ln: Expr[Lens[A, B]]): Expr[Traversal[S, B]]

    def traversalComposeTraversal[S, A, B](
      tr1: Expr[Traversal[S, A]],
      tr2: Expr[Traversal[A, B]]): Expr[Traversal[S, B]]

    def lensVertComposeLens[S, A, B](
      ln1: Expr[Lens[S, A]],
      ln2: Expr[Lens[S, B]]): Expr[Lens[S, (A, B)]]

    def lensAsFold[S, A](
      ln: Expr[Lens[S, A]]): Expr[Fold[S, A]]

    def traversalAsFold[S, A](
      tr: Expr[Traversal[S, A]]): Expr[Fold[S, A]]

    def foldWithFilter[S, A, B](
      fl: Expr[Fold[S, A]],
      f: Expr[A => B]): Expr[Fold[S, B]]

    def traversalGetAll[S, A](
      tr: Expr[Traversal[S, A]]): Expr[S => List[A]]

    def fun[A, B](f: Expr[A] => Expr[B]): Expr[A => B]

    def app[A, B](f: Expr[A => B], a: Expr[A]): Expr[B]
  }

  object OpticLang {

    def apply[Expr[_]](implicit alg: OpticLang[Expr]): OpticLang[Expr] = alg

    trait Church[Out] {
      def apply[Expr[_]](alg: OpticLang[Expr]): Expr[Out]
    }

    sealed trait GADT[Out]

    case class Point[A](a: A) extends GADT[A]

    case class LensComposeLens[S, A, B](
      ln1: GADT[Lens[S, A]], 
      ln2: GADT[Lens[A, B]]) extends GADT[Lens[S, B]]

    case class LensComposeTraversal[S, A, B](
      ln: GADT[Lens[S, A]],
      tr: GADT[Traversal[A, B]]) extends GADT[Traversal[S, B]]

    case class TraversalComposeLens[S, A, B](
      tr: GADT[Traversal[S, A]],
      ln: GADT[Lens[A, B]]) extends GADT[Traversal[S, B]]

    case class TraversalComposeTraversal[S, A, B](
      tr1: GADT[Traversal[S, A]],
      tr2: GADT[Traversal[A, B]]) extends GADT[Traversal[S, B]]

    case class LensVertComposeLens[S, A, B](
      ln1: GADT[Lens[S, A]],
      ln2: GADT[Lens[S, B]]) extends GADT[Lens[S, (A, B)]]

    case class LensAsFold[S, A](
      ln: GADT[Lens[S, A]]) extends GADT[Fold[S, A]]

    case class TraversalAsFold[S, A](
      tr: GADT[Traversal[S, A]]) extends GADT[Fold[S, A]]

    case class FoldWithFilter[S, A, B](
      fl: GADT[Fold[S, A]],
      f: GADT[A => B]) extends GADT[Fold[S, B]]

    case class TraversalGetAll[S, A](
      tr: GADT[Traversal[S, A]]) extends GADT[S => List[A]]

    case class Fun[A, B](f: GADT[A] => GADT[B]) extends GADT[A => B]

    case class App[A, B](f: GADT[A => B], a: GADT[A]) extends GADT[B]

    implicit def churchOpticLang: OpticLang[Church] = new OpticLang[Church] {

      def point[A](a: A): Church[A] =
        new Church[A] {
          def apply[Expr[_]](alg: OpticLang[Expr]) =
            alg.point(a)
        }

      def lensComposeLens[S, A, B](
          ln1: Church[Lens[S, A]], 
          ln2: Church[Lens[A, B]]): Church[Lens[S, B]] =
        new Church[Lens[S, B]] {
          def apply[Expr[_]](alg: OpticLang[Expr]) =
            alg.lensComposeLens(ln1(alg), ln2(alg))
        }

      def lensComposeTraversal[S, A, B](
          ln: Church[Lens[S, A]],
          tr: Church[Traversal[A, B]]): Church[Traversal[S, B]] =
        new Church[Traversal[S, B]] {
          def apply[Expr[_]](alg: OpticLang[Expr]) =
            alg.lensComposeTraversal(ln(alg), tr(alg))
        }

      def traversalComposeLens[S, A, B](
          tr: Church[Traversal[S, A]],
          ln: Church[Lens[A, B]]): Church[Traversal[S, B]] =
        new Church[Traversal[S, B]] {
          def apply[Expr[_]](alg: OpticLang[Expr]) =
            alg.traversalComposeLens(tr(alg), ln(alg))
        }

      def traversalComposeTraversal[S, A, B](
          tr1: Church[Traversal[S, A]],
          tr2: Church[Traversal[A, B]]): Church[Traversal[S, B]] =
        new Church[Traversal[S, B]] {
          def apply[Expr[_]](alg: OpticLang[Expr]) = 
            alg.traversalComposeTraversal(tr1(alg), tr2(alg))
        }

      def lensVertComposeLens[S, A, B](
          ln1: Church[Lens[S, A]],
          ln2: Church[Lens[S, B]]): Church[Lens[S, (A, B)]] =
        new Church[Lens[S, (A, B)]] {
          def apply[Expr[_]](alg: OpticLang[Expr]) = 
            alg.lensVertComposeLens(ln1(alg), ln2(alg))
        }

      def lensAsFold[S, A](
          ln: Church[Lens[S, A]]): Church[Fold[S, A]] =
        new Church[Fold[S, A]] {
          def apply[Expr[_]](alg: OpticLang[Expr]) = 
            alg.lensAsFold(ln(alg))
        }

      def traversalAsFold[S, A](
          tr: Church[Traversal[S, A]]): Church[Fold[S, A]] =
        new Church[Fold[S, A]] {
          def apply[Expr[_]](alg: OpticLang[Expr]) = 
            alg.traversalAsFold(tr(alg))
        }

      def foldWithFilter[S, A, B](
          fl: Church[Fold[S, A]],
          f: Church[A => B]): Church[Fold[S, B]] =
        new Church[Fold[S, B]] {
          def apply[Expr[_]](alg: OpticLang[Expr]) = 
            alg.foldWithFilter(fl(alg), f(alg))
        }

      def traversalGetAll[S, A](
          tr: Church[Traversal[S, A]]): Church[S => List[A]] =
        new Church[S => List[A]] {
          def apply[Expr[_]](alg: OpticLang[Expr]) = 
            alg.traversalGetAll(tr(alg))
        }

      def fun[A, B](f: Church[A] => Church[B]): Church[A => B] =
        new Church[A => B] {
          def apply[Expr[_]](alg: OpticLang[Expr]) = {
            // alg.point(a => f(point(a))(alg))
            // val g: Expr[A] => Expr[B] = a => ???
            // alg.fun(g) 
            ???
          }
        }

      def app[A, B](f: Church[A => B], a: Church[A]): Church[B] =
        new Church[B] {
          def apply[Expr[_]](alg: OpticLang[Expr]) = 
            alg.app(f(alg), a(alg))
        }
    }
    
    implicit def gadtOpticLang: OpticLang[GADT] = new OpticLang[GADT] {

      def point[A](a: A): GADT[A] =
        Point(a)

      def lensComposeLens[S, A, B](
          ln1: GADT[Lens[S, A]], 
          ln2: GADT[Lens[A, B]]): GADT[Lens[S, B]] =
        LensComposeLens(ln1, ln2)

      def lensComposeTraversal[S, A, B](
          ln: GADT[Lens[S, A]],
          tr: GADT[Traversal[A, B]]): GADT[Traversal[S, B]] =
        LensComposeTraversal(ln, tr)

      def traversalComposeLens[S, A, B](
          tr: GADT[Traversal[S, A]],
          ln: GADT[Lens[A, B]]): GADT[Traversal[S, B]] =
        TraversalComposeLens(tr, ln)

      def traversalComposeTraversal[S, A, B](
          tr1: GADT[Traversal[S, A]],
          tr2: GADT[Traversal[A, B]]): GADT[Traversal[S, B]] =
        TraversalComposeTraversal(tr1, tr2)

      def lensVertComposeLens[S, A, B](
          ln1: GADT[Lens[S, A]],
          ln2: GADT[Lens[S, B]]): GADT[Lens[S, (A, B)]] =
        LensVertComposeLens(ln1, ln2)

      def lensAsFold[S, A](
          ln: GADT[Lens[S, A]]): GADT[Fold[S, A]] =
        LensAsFold(ln)

      def traversalAsFold[S, A](
          tr: GADT[Traversal[S, A]]): GADT[Fold[S, A]] =
        TraversalAsFold(tr)

      def foldWithFilter[S, A, B](
          fl: GADT[Fold[S, A]],
          f: GADT[A => B]): GADT[Fold[S, B]] =
        FoldWithFilter(fl, f)

      def traversalGetAll[S, A](
          tr: GADT[Traversal[S, A]]): GADT[S => List[A]] =
        TraversalGetAll(tr)

      def fun[A, B](f: GADT[A] => GADT[B]): GADT[A => B] =
        Fun(f)

      def app[A, B](f: GADT[A => B], a: GADT[A]): GADT[B] =
        App(f, a)
    }

    type Id[A] = A

    implicit def idOpticLang: OpticLang[Id] = new OpticLang[Id] {

      def point[A](a: A): A = a

      def lensComposeLens[S, A, B](
          ln1: Lens[S, A], 
          ln2: Lens[A, B]): Lens[S, B] =
        ln1 composeLens ln2

      def lensComposeTraversal[S, A, B](
          ln: Lens[S, A],
          tr: Traversal[A, B]): Traversal[S, B] =
        ln composeTraversal tr

      def traversalComposeLens[S, A, B](
          tr: Traversal[S, A],
          ln: Lens[A, B]): Traversal[S, B] =
        tr composeLens ln

      def traversalComposeTraversal[S, A, B](
          tr1: Traversal[S, A],
          tr2: Traversal[A, B]): Traversal[S, B] =
        tr1 composeTraversal tr2

      def lensVertComposeLens[S, A, B](
          ln1: Lens[S, A],
          ln2: Lens[S, B]): Lens[S, (A, B)] =
        Lens[S, (A, B)](
          s => (ln1.get(s), ln2.get(s)))(
          ab => s => ln2.set(ab._2)(ln1.set(ab._1)(s)))

      def lensAsFold[S, A](ln: Lens[S, A]): Fold[S, A] = ln.asFold

      def traversalAsFold[S, A](tr: Traversal[S, A]): Fold[S, A] = tr.asFold

      def foldWithFilter[S, A, B](fl: Fold[S, A], f: A => B): Fold[S, B] =
        ???        

      def traversalGetAll[S, A](tr: Traversal[S, A]): S => List[A] = tr.getAll

      def fun[A, B](f: A => B): A => B = f

      def app[A, B](f: A => B, a: A): B = f(a)
    }

    trait Syntax {

      def point[Expr[_]: OpticLang, A](a: A): Expr[A] =
        OpticLang[Expr].point(a)

      def lensComposeLens[Expr[_]: OpticLang, S, A, B](
          ln1: Expr[Lens[S, A]], 
          ln2: Expr[Lens[A, B]]): Expr[Lens[S, B]] = 
        OpticLang[Expr].lensComposeLens(ln1, ln2)

      def lensComposeTraversal[Expr[_]: OpticLang, S, A, B](
          ln: Expr[Lens[S, A]],
          tr: Expr[Traversal[A, B]]): Expr[Traversal[S, B]] =
        OpticLang[Expr].lensComposeTraversal(ln, tr)

      def traversalComposeLens[Expr[_]: OpticLang, S, A, B](
          tr: Expr[Traversal[S, A]],
          ln: Expr[Lens[A, B]]): Expr[Traversal[S, B]] =
        OpticLang[Expr].traversalComposeLens(tr, ln)

      def traversalComposeTraversal[Expr[_]: OpticLang, S, A, B](
          tr1: Expr[Traversal[S, A]],
          tr2: Expr[Traversal[A, B]]): Expr[Traversal[S, B]] =
        OpticLang[Expr].traversalComposeTraversal(tr1, tr2)

      def lensVertComposeLens[Expr[_]: OpticLang, S, A, B](
          ln1: Expr[Lens[S, A]],
          ln2: Expr[Lens[S, B]]): Expr[Lens[S, (A, B)]] =
        OpticLang[Expr].lensVertComposeLens(ln1, ln2)

      def lensAsFold[Expr[_]: OpticLang, S, A](
          ln: Expr[Lens[S, A]]): Expr[Fold[S, A]] =
        OpticLang[Expr].lensAsFold(ln)

      def traversalAsFold[Expr[_]: OpticLang, S, A](
          tr: Expr[Traversal[S, A]]): Expr[Fold[S, A]] =
        OpticLang[Expr].traversalAsFold(tr)      

      def foldWithFilter[Expr[_]: OpticLang, S, A, B](
          fl: Expr[Fold[S, A]],
          f: Expr[A => B]): Expr[Fold[S, B]] =
        OpticLang[Expr].foldWithFilter(fl, f)

      def traversalGetAll[Expr[_]: OpticLang, S, A](
          tr: Expr[Traversal[S, A]]): Expr[S => List[A]] =
        OpticLang[Expr].traversalGetAll(tr)

      def fun[Expr[_]: OpticLang, A, B](
          f: Expr[A] => Expr[B]): Expr[A => B] =
        OpticLang[Expr].fun(f)

      def app[Expr[_]: OpticLang, A, B](
          f: Expr[A => B], 
          a: Expr[A]): Expr[B] =
        OpticLang[Expr].app(f, a)
    }

    object syntax extends Syntax
  }


  /* couples example */

  // data layer

  @Lenses case class Person(name: String, age: Int)
  @Lenses case class Couple(her: Person, him: Person)

  import Person._, Couple._

  type People  = List[Person]
  type Couples = List[Couple]

  val people:  Traversal[People, Person] = each
  val couples: Traversal[Couples, Couple] = each

  // logic

  import OpticLang.syntax._

  def coupledPeople[Expr[_]: OpticLang]: Expr[Traversal[Couples, Person]] =
    traversalComposeTraversal(
      point(couples), 
      point(Traversal.applyN(her, him)))

  coupledPeople[OpticLang.Id]

  def getPeopleName[Expr[_]: OpticLang]: Expr[Couples => List[String]] =
    fun { (s: Expr[Couples]) => 
      app(traversalGetAll(traversalComposeLens(coupledPeople, point(name))), s)
    }

  getPeopleName[OpticLang.Id]

  def getHerAges[Expr[_]: OpticLang]: Expr[Couples => List[Int]] =
    fun { (s: Expr[Couples]) => 
      app(
        traversalGetAll(
          traversalComposeLens(
            point(couples), 
            lensComposeLens(point(her), point(age)))), 
        s)
    }

  getHerAges[OpticLang.Id]
}

