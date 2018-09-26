package org.hablapps.statelesser

import scalaz.MonadState

trait InitialSAlg[Alg[_[_], _], A, X] {
  def apply[Q[_]](alg: Alg[Q, A]): Q[X]
}

object InitialSAlg {

  def getMS[A]: InitialSAlg[MonadState, A, A] =
    new InitialSAlg[MonadState, A, A] {
      def apply[Q[_]](alg: MonadState[Q, A]) = alg.get
    }

  def getsMS[A, B](f: A => B): InitialSAlg[MonadState, A, B] =
    new InitialSAlg[MonadState, A, B] {
      def apply[Q[_]](alg: MonadState[Q, A]) = alg.gets(f)
    }

  def putMS[A](a: A): InitialSAlg[MonadState, A, Unit] =
    new InitialSAlg[MonadState, A, Unit] {
      def apply[Q[_]](alg: MonadState[Q, A]) = alg.put(a)
    }

  def modMS[A](f: A => A): InitialSAlg[MonadState, A, Unit] =
    new InitialSAlg[MonadState, A, Unit] {
      def apply[Q[_]](alg: MonadState[Q, A]) = alg.modify(f)
    }
}

