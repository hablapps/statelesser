package org.hablapps.statelesser

import Function.const
import scalaz._, Scalaz._
import shapeless._, labelled._, ops.hlist._

/**
 * State algebras
 */

case class Getter[P[_],S](get: P[S]) {
  def apply() = get
}

case class Setter[P[_],S](set: S => P[Unit]) {
  def apply(s: S): P[Unit] = set(s)
}

case class Field[P[_],S](get: Getter[P,S], put: Setter[P,S]) {
  def modify(f: S => S)(implicit B: Bind[P]): P[Unit] = 
    get() >>= { s => put(f(s)) } 
}

object Field {
  import Util._, StateField._, AlgFunctor._

  implicit def genFieldId[Ctx <: HList, K, A]
      : GetEvidence[K :: Ctx, Field[State[A, ?], A]] =
    GetEvidence(refl[K :: Ctx, A].apply) 

  implicit def genField[Ctx <: HList, P[_]: Functor, S](implicit 
      ln: FieldType[Ctx, State[S, ?] ~> P]): GetEvidence[Ctx, Field[P, S]] =
    GetEvidence(refl[Ctx, S].apply.amap(ln))
}

object Primitive{
  type IntegerP[P[_]]=Field[P,Int]
  type BooleanP[P[_]]=Field[P,Boolean]
  type StringP[P[_]]=Field[P,String]
}

case class ListP[Alg[_[_],_], P[_], S](algs: P[List[Alg[P, S]]]) {

  def apply() = algs

  def traverse[T](
      f: Alg[P,S] => P[T])(implicit 
      M: Monad[P]): P[List[T]] = 
    apply >>= { _ traverse[P,T] f }

  def traverseZip[A,T](
      values: List[A])(
      f: (Alg[P,S],A) => P[T])(implicit 
      M: Monad[P]): P[List[T]] =
    apply >>= { algs => 
      (algs zip values) traverse f.tupled
    }
}

trait StdTraverse[P[_],S] extends ListP[Field,P,S]{
  def getAll(implicit M: Monad[P]): P[List[S]] =
    traverse(_.get())

  def putAll(values: List[S])(implicit M: Monad[P]): P[List[Unit]] =
    traverseZip(values)(_ put _)
}


/** 
 * Utilities & Combinators 
 */

object Util {

  type ReaderGetter[S, A] = Getter[Reader[S, ?], A]

  object ReaderGetter {
    
    def apply[S, A](g: S => A): ReaderGetter[S, A] =
      Getter[Reader[S, ?], A](Reader(g))

    implicit def refl[S]: ReaderGetter[S, S] = apply(identity)
  }
  
  type StateField[S, A] = Field[State[S, ?], A]

  object StateField {

    def apply[S, A](g: S => A, p: A => S => S): StateField[S, A] =
      Field[State[S, ?], A](
        Getter(State.gets(g)),
        Setter(a => State(s => (p(a)(s), ()))))

    implicit def refl[Ctx <: HList, S]: GetEvidence[Ctx, StateField[S, S]] = 
      GetEvidence(apply[S, S](identity, const))
  }

  type Lens[S, A] = State[A, ?] ~> State[S, ?]

  object Lens {

    def apply[S, A](get: S => A, set: A => S => S): Lens[S, A] =
      λ[State[A, ?] ~> State[S, ?]] { sa =>
        State(s => sa(get(s)).leftMap(set(_)(s)))
      }

    def lensId[S]: Lens[S, S] = NaturalTransformation.refl
  }
  
  // Lens & StateField are isomorphic

  def lens2StateField[S, A](ln: Lens[S, A]): StateField[S, A] =
    Field[State[S, ?], A](
      Getter(ln(State.get)),
      Setter(a => ln(State.put(a))))

  def stateField2Lens[S, A](fl: StateField[S, A]): Lens[S, A] =
    λ[State[A, ?] ~> State[S, ?]] { sta =>
      State { s =>
        val (a, x) = sta.run(fl.get().eval(s))
        (fl.put(a).exec(s), x)
      }
    }

  implicit def getTaggedLens[
        Ctx <: HList, 
        Ctx2 <: HList : Reverse.Aux[Ctx, ?], S, A](implicit 
      tl: Shapelens.Aux[S, Ctx2, A]): FieldType[Ctx, Lens[S, A]] =
    field[Ctx](tl.getLens)
  
  def make[A](implicit ev: GetEvidence[HNil, A]): A =
    ev.apply
}

