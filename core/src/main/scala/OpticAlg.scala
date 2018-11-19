package org.hablapps.statelesser

trait OpticAlg[Expr[_]] {

  def flVert[S, A, B](
    l: Expr[Fold[S, A]], 
    r: Expr[Fold[A, B]]): Expr[Fold[S, B]]

  def gtHori[S, A, B](
    l: Expr[Getter[S, A]],
    r: Expr[Getter[S, B]]): Expr[Getter[S, (A, B)]]

  def getAll[S, A](fl: Expr[Fold[S, A]]): Expr[S => List[A]]

  def lnAsGt[S, A](ln: Expr[Lens[S, A]]): Expr[Getter[S, A]]

  def gtAsFl1[S, A](gt: Expr[Getter[S, A]]): Expr[Fold1[S, A]]

  def fl1AsFl[S, A](fl1: Expr[Fold1[S, A]]): Expr[Fold[S, A]]
}

object OpticAlg {

  trait Semantic  

  trait Syntax {

    implicit class FoldOps[Expr[_], S, A](
        l: Expr[Fold[S, A]])(implicit 
        O: OpticAlg[Expr]) {
      def >[B](r: Expr[Fold[A, B]]): Expr[Fold[S, B]] = O.flVert(l, r)
    }

    implicit class Fold1Ops[Expr[_], S, A](
        l: Expr[Fold1[S, A]])(implicit
        O: OpticAlg[Expr]) {
      def asFold: Expr[Fold[S, A]] = O.fl1AsFl(l)
    }

    implicit class GetterOps[Expr[_], S, A](
        l: Expr[Getter[S, A]])(implicit
        O: OpticAlg[Expr]) {
      def asFold1: Expr[Fold1[S, A]] = O.gtAsFl1(l)
      def *[B](r: Expr[Getter[S, B]]): Expr[Getter[S, (A, B)]] = O.gtHori(l, r)
    }
  }

  object syntax extends Syntax
}

trait CoupleModel[Expr[_]] {

  implicit val ev: OpticAlg[Expr]
  
  type Couple
  type Couples = List[Couple]
  type Person
  type People = List[Person]

  val couples: Expr[Fold[Couples, Couple]]
  val her: Expr[Getter[Couple, Person]]
  val him: Expr[Getter[Couple, Person]]
  val people: Expr[Fold[People, Person]]
  val name: Expr[Getter[Person, String]]
  val age: Expr[Getter[Person, Int]]
}

trait CoupleLogic[Expr[_]] {
  import OpticAlg.syntax._
  
  val model: CoupleModel[Expr]
  import model._, ev._

  def getPeople: Expr[People => List[Person]] =
    getAll(people)

  def getPeopleName: Expr[People => List[String]] =
    getAll(people > name.asFold1.asFold)

  def getPeopleNameAndAge: Expr[People => List[(String, Int)]] =
    getAll(people > (name * age).asFold1.asFold)
}

