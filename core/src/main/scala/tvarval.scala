package statelesser

import OpticLang._

sealed abstract class TVarVal[E[_], O[_, _], S, A] {
  def mapO[O2[_, _]](f: OpticMap[E, O, O2]): TVarVal[E, O2, S, A] = this match {
    case TVarSimpleVal(Wrap(e, info)) => TVarSimpleVal(Wrap(f(e), info))
    case vnv: TVarNestedVal[E, O, S, A] =>  
      TVarNestedVal(vnv.vs.mapO(f), Wrap(f(vnv.w.e), vnv.w.info))
  }
}

case class TVarSimpleVal[E[_], O[_, _], S, A](w: Wrap[E, O, S, A])
  extends TVarVal[E, O, S, A]

trait TVarNestedVal[E[_], O[_, _], S, B] extends TVarVal[E, O, S, B] {
  type A
  def vs: OList[E, O, S, A]
  def w: Wrap[E, O, A, B]
  override def equals(other: Any): Boolean = other match {
    case vnv: TVarNestedVal[E, O, S, _] => vs == vnv.vs && vnv.w == w
    case _ => false
  }
}

object TVarNestedVal {

  type Aux[E[_], O[_, _], S, A2, B] = 
    TVarNestedVal[E, O, S, B] { type A = A2 }

  def apply[E[_], O[_, _], S, A2, B](
      vs2: OList[E, O, S, A2], 
      w2: Wrap[E, O, A2, B]): Aux[E, O, S, A2, B] = 
    new TVarNestedVal[E, O, S, B] {
      type A = A2
      val vs = vs2
      val w = w2
    }
}

