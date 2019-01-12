// package statelesser
// 
// import OpticLang._
// 
// sealed abstract class TVarVal[E[_], O[_, _], S, A] {
// 
//   def mapO[O2[_, _]](f: OpticMap[E, O, O2]): TVarVal[E, O2, S, A] = this match {
//     case TVarSimpleVal(Wrap(e, info)) => TVarSimpleVal(Wrap(f(e), info))
//     case TVarNestedVal(Var(x), Wrap(e, info)) =>
//       TVarNestedVal(Var(x), Wrap(f(e), info))
//   }
// 
//   def rwVars(rws: Set[(String, String)]): TVarVal[E, O, S, A] =
//     this match {
//       case TVarNestedVal(Var(s), w) => rws.find(_._1 == s).fold(this){ rw =>
//         TVarNestedVal(Var(rw._2), w)
//       }
//       case _ => this
//     }
// }
// 
// case class TVarSimpleVal[E[_], O[_, _], S, A](w: Wrap[E, O, S, A])
//   extends TVarVal[E, O, S, A]
// 
// case class TVarNestedVal[E[_], O[_, _], S, A, B](
//   v: Var[E, O, S, A],
//   w: Wrap[E, O, A, B]) extends TVarVal[E, O, S, B]

