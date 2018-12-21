package statelesser

import OpticLang._

sealed abstract class OList[E[_], O[_, _], S, A] {

  def mapO[O2[_, _]](f: OpticMap[E, O, O2]): OList[E, O2, S, A] = this match {
    case ONil(Var(x)) => ONil(Var(x))
    case OCons(Var(x), t) => OCons(Var(x), t.mapO(f))
  }

  def lastVar: Var[E, O, _, A] = this match {
    case ONil(x) => x
    case OCons(_, tail) => tail.lastVar
  }

  def vars: Set[String] = this match {
    case ONil(Var(x)) => Set(x)
    case OCons(Var(x), t) => t.vars + x
  }
}

case class ONil[E[_], O[_, _], S, A](last: Var[E, O, S, A])
  extends OList[E, O, S, A]

case class OCons[E[_], O[_, _], S, A, B](
    head: Var[E, O, S, A], 
    tail: OList[E, O, A, B])
  extends OList[E, O, S, B]

