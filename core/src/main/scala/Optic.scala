package org.hablapps.statelesser

import scalaz._

// Optics

case class Iso[S, A](from : S => A, to : A => S)

case class Lens[S, A](get : S => A, set : A => S => S)

case class Getter[S, A](get : S => A)

trait Fold1[S, A] {
  def foldMap[M : Semigroup](f : A => M): S => M
}

case class AffineFold[S, A](pre: S => Option[A])

trait Fold[S, A] {
  def foldMap[M : Monoid](f : A => M): S => M
}

// Typeclass-based composition

trait AsIso[Op[_, _]] {
  def apply[S, A](op: Op[S, A]): Iso[S, A]
}

object AsIso {
  implicit val isoAsIso: AsIso[Iso] = new AsIso[Iso] {
    def apply[S, A](op: Iso[S, A]): Iso[S, A] = op
  }
}

trait AsLens[Op[_, _]] {
  def apply[S, A](op: Op[S, A]): Lens[S, A]
}

object AsLens {

  implicit val lensAsLens: AsLens[Lens] = new AsLens[Lens] {
    def apply[S, A](op: Lens[S, A]): Lens[S, A] = op
  }

  implicit def isoAsLens[Op[_, _]: AsIso]: AsLens[Op] = new AsLens[Op] {
    def apply[S, A](op: Op[S, A]): Lens[S, A] = ???
  }
}

trait AsGetter[Op[_, _]] {
  def apply[S, A](op: Op[S, A]): Getter[S, A]
}

object AsGetter {

  implicit val gtAsGetter: AsGetter[Getter] = new AsGetter[Getter] {
    def apply[S, A](op: Getter[S, A]): Getter[S, A] = op
  }

  implicit def lnAsGetter[Op[_, _]: AsLens] : AsGetter[Op] = new AsGetter[Op] {
    def apply[S, A](op: Op[S, A]): Getter[S, A] = ???
  }
}

trait AsFold1[Op[_, _]] {
  def apply[S, A](op: Op[S, A]): Fold1[S, A]
}

object AsFold1 {

  implicit val fl1AsFold1: AsFold1[Fold1] = new AsFold1[Fold1] {
    def apply[S, A](op: Fold1[S, A]): Fold1[S, A] = op
  }

  implicit def gtAsFold1[Op[_, _]: AsGetter] : AsFold1[Op] = new AsFold1[Op] {
    def apply[S, A](op: Op[S, A]): Fold1[S, A] = ???
  }
}

trait AsFold[Op[_, _]] {
  def apply[S, A](op: Op[S, A]): Fold[S, A]
}

object AsFold {

  implicit val flAsFold: AsFold[Fold] = new AsFold[Fold] {
    def apply[S, A](op: Fold[S, A]): Fold[S, A] = op
  }

  implicit def fl1AsFold[Op[_, _]: AsFold1] : AsFold[Op] = new AsFold[Op] {
    def apply[S, A](op: Op[S, A]): Fold[S, A] = ???
  }
}

trait VerticalCompose[L[_, _], R[_, _]] {
  type Out[_, _]
  def apply[S, A, B](l: L[S, A], r: R[A, B]): Out[S, B]
}

object VerticalCompose extends VerticalCompose1 {

  implicit def isoCompose[L[_, _]: AsIso, R[_, _]: AsIso]: Aux[L, R, Iso] =
    new VerticalCompose[L, R] {
      type Out[S, A] = Iso[S, A]
      def apply[S, A, B](l: L[S, A], r: R[A, B]): Out[S, B] = ???
    }

  trait Syntax {
    implicit class VerticalOps[L[_, _], S, A](l: L[S, A]) {
      def >[R[_, _], B](r: R[A, B])(implicit ev: VerticalCompose[L, R]) =
        ev(l, r)
    }
  }

  object syntax extends Syntax
}

trait VerticalCompose1 extends VerticalCompose2 {
  implicit def lnCompose[L[_, _]: AsLens, R[_, _]: AsLens]: Aux[L, R, Lens] =
    new VerticalCompose[L, R] {
      type Out[S, A] = Lens[S, A]
      def apply[S, A, B](l: L[S, A], r: R[A, B]): Out[S, B] = ???
    }
}

trait VerticalCompose2 extends VerticalCompose3 {
  implicit def gtCompose[L[_, _]: AsGetter, R[_, _]: AsGetter]: Aux[L, R, Getter] =
    new VerticalCompose[L, R] {
      type Out[S, A] = Getter[S, A]
      def apply[S, A, B](l: L[S, A], r: R[A, B]): Out[S, B] = ???
    }
}

trait VerticalCompose3 extends VerticalCompose4 {
  implicit def fl1Compose[L[_, _]: AsFold1, R[_, _]: AsFold1]: Aux[L, R, Fold1] =
    new VerticalCompose[L, R] {
      type Out[S, A] = Fold1[S, A]
      def apply[S, A, B](l: L[S, A], r: R[A, B]): Out[S, B] = ???
    }
}

trait VerticalCompose4 {
  
  type Aux[L[_, _], R[_, _], Out2[_, _]] = 
    VerticalCompose[L, R] { type Out[S, A] = Out2[S, A] }
  
  implicit def flCompose[L[_, _]: AsFold, R[_, _]: AsFold]: Aux[L, R, Fold] =
    new VerticalCompose[L, R] {
      type Out[S, A] = Fold[S, A]
      def apply[S, A, B](l: L[S, A], r: R[A, B]): Out[S, B] = ???
    }
}

trait HorizontalCompose[L[_, _], R[_, _]] {
  type Out[_, _]
  def apply[S, A, B](l: L[S, A], r: R[S, B]): Out[S, (A, B)]
}

object HorizontalCompose extends HorizontalCompose1 {

  implicit def isoCompose[L[_, _]: AsIso, R[_, _]: AsIso]: Aux[L, R, Iso] =
    new HorizontalCompose[L, R] {
      type Out[S, A] = Iso[S, A]
      def apply[S, A, B](l: L[S, A], r: R[S, B]): Out[S, (A, B)] = ???
    }

  trait Syntax {
    implicit class HorizontalOps[L[_, _], S, A](l: L[S, A]) {
      def *[R[_, _], B](r: R[S, B])(implicit ev: HorizontalCompose[L, R]) =
        ev(l, r)
    }
  }

  object syntax extends Syntax
}

trait HorizontalCompose1 extends HorizontalCompose2 {
  implicit def lnCompose[L[_, _]: AsLens, R[_, _]: AsLens]: Aux[L, R, Lens] =
    new HorizontalCompose[L, R] {
      type Out[S, A] = Lens[S, A]
      def apply[S, A, B](l: L[S, A], r: R[S, B]): Out[S, (A, B)] = ???
    }
}

trait HorizontalCompose2 extends HorizontalCompose3 {
  implicit def gtCompose[L[_, _]: AsGetter, R[_, _]: AsGetter]: Aux[L, R, Getter] =
    new HorizontalCompose[L, R] {
      type Out[S, A] = Getter[S, A]
      def apply[S, A, B](l: L[S, A], r: R[S, B]): Out[S, (A, B)] = ???
    }
}

trait HorizontalCompose3 extends HorizontalCompose4 {
  implicit def fl1Compose[L[_, _]: AsFold1, R[_, _]: AsFold1]: Aux[L, R, Fold1] =
    new HorizontalCompose[L, R] {
      type Out[S, A] = Fold1[S, A]
      def apply[S, A, B](l: L[S, A], r: R[S, B]): Out[S, (A, B)] = ???
    }
}

trait HorizontalCompose4 {
  
  type Aux[L[_, _], R[_, _], Out2[_, _]] = 
    HorizontalCompose[L, R] { type Out[S, A] = Out2[S, A] }
  
  implicit def flCompose[L[_, _]: AsFold, R[_, _]: AsFold]: Aux[L, R, Fold] =
    new HorizontalCompose[L, R] {
      type Out[S, A] = Fold[S, A]
      def apply[S, A, B](l: L[S, A], r: R[S, B]): Out[S, (A, B)] = ???
    }
}

// Typeclass-based actions

object Actions {

  def get[Op[_, _]: AsGetter, S, A](op: Op[S, A]): S => A = ???
  
  def getAll[Op[_, _] : AsFold, S, A](op: Op[S, A]): S => List[A] = ???
}

object Example {

  import VerticalCompose.syntax._
  import HorizontalCompose.syntax._
  import Actions._

  trait CoupleModel {
    type Couple
    type Person
    type Couples = List[Couple]
    type People = List[Person]

    val couples: Fold[Couples, Couple]
    val her: Getter[Couple, Person]
    val him: Getter[Couple, Person]
    val people: Fold[People, Person]
    val name: Getter[Person, String]
    val age: Getter[Person, Int]
  }

  trait CoupleLogic {

    val model: CoupleModel
    import model._

    def getHerNames: Couples => List[String] =
      getAll(couples > her > name)

    def getHerNameAndAge: Couples => List[(String, Int)] =
      getAll(couples > her > name * age)
  }
}

