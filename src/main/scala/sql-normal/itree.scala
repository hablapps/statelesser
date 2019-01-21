package statelesser
package sqlnormal

import monocle._, function.Index

case class ITree[I, A](
  label: A, 
  children: IForest[I, A] = Map.empty[I, ITree[I, A]])

object ITree {
  
  implicit def indexITree[I, A] = new Index[ITree[I, A], I, ITree[I, A]] {
    
    def index(i: I): Optional[ITree[I, A], ITree[I, A]] =
      Optional[ITree[I, A], ITree[I, A]](
        s => s.children.get(i))(
        a => s => s.copy(children = s.children.updated(i, a)))
  }
}

