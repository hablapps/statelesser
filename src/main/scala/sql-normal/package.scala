package statelesser

import scalaz._, Scalaz._
import monocle.{Optional, _}, function.Index, Index._

package object `sqlnormal` {

  type TypeNme = String

  type OpticNme = String

  type Value = OpticType[_, _] \/ Select[_, _, _]

  type Semantic[A] = State[Stream[Symbol], TSemantic[A]]

  type Symbol = String

  def fresh: State[Stream[String], String] =
    for {
      s <- get[Stream[String]]
      _ <- modify[Stream[String]](_.tail)
    } yield s.head 

  type IForest[I, A] = Map[I, ITree[I, A]]

  type TVarTree = NonEmptyList[ITree[String, (Symbol, OpticType[_, _])]]

  implicit val indexTVarTree = 
    new Index[TVarTree, Symbol, ITree[String, (Symbol, OpticType[_, _])]] {
      def index(i: Symbol) = 
        Optional[TVarTree, ITree[String, (Symbol, OpticType[_, _])]](
          s => s.list.find(_.label._1 == i))(
          a => s => 
            if (s.head.label._1 == i) 
              NonEmptyList.nel((i, a), s.tail) 
            else 
              NonEmptyList.nel(
                s.head, 
                iMapIndex[Symbol, ITree[Symbol, (Symbol, OpticType[_, _])]]
                  // XXX: using `toIList` directly doesn't work, wat!?!?
                  .index(i).set(a)(s.tail.toMap).toList.toIList))
    }
}

