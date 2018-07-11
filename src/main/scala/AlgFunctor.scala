package org.hablapps.statelesser

import scalaz._

// Maps the interpretation of an algebra. 
//
// XXX: we're not using this algebra right now, but we keep it since I find
// it interesting anyway, so maybe we could use it in the future.
trait AlgFunctor[Alg[_[_], _]] {
  def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P): Alg[Q, ?] ~> Alg[P, ?]
}

object AlgFunctor {

  def apply[Alg[_[_], _]](implicit ev: AlgFunctor[Alg]): AlgFunctor[Alg] = 
    ev

  // XXX: boilerplate instances, consider using Shapeless here?
  
  implicit val GetterAlgFunctor = new AlgFunctor[Getter] {
    def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
      λ[Getter[Q, ?] ~> Getter[P, ?]] { alg =>
        Getter(f(alg.apply))
      }
  }

  implicit val SetterAlgFunctor = new AlgFunctor[Setter] {
    def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
      λ[Setter[Q, ?] ~> Setter[P, ?]] { alg =>
        Setter(x => f(alg(x)))
      }
  }

  implicit val FieldAlgFunctor = new AlgFunctor[Field] {
    def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
      λ[Field[Q, ?] ~> Field[P, ?]] { alg =>
        // XXX: syntax isn't working properly here 
        Field(
          GetterAlgFunctor.amap(f)(implicitly, implicitly)(alg.get), 
          SetterAlgFunctor.amap(f)(implicitly, implicitly)(alg.put))
      }
  }

  implicit class AlgFunctorOps[Alg[_[_], _], Q[_]: Functor, A](
      al: Alg[Q, A]) {
    def amap[P[_]: Functor](f: Q ~> P)(implicit AF: AlgFunctor[Alg]) = 
      AF.amap(f)(implicitly, implicitly)(al)
  }
}

