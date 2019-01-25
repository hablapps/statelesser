package statelesser
package sqlnormal

import monocle._, function.Index, Index._

case class ITree[I, A](
    label: A, 
    children: IForest[I, A] = Map.empty[I, ITree[I, A]]) {

  def exists(p: A => Boolean): Boolean =
    p(label) || children.values.toList.exists(_.exists(p)) 

  def contains(a: A): Boolean = exists(_ == a)
}

object ITree {
  
  implicit def indexITree[I, A] = new Index[ITree[I, A], I, ITree[I, A]] {
    
    def index(i: I): Optional[ITree[I, A], ITree[I, A]] =
      Optional[ITree[I, A], ITree[I, A]](
        s => mapIndex.index(i).getOption(s.children))(
        a => s => s.copy(children = mapIndex.index(i).set(a)(s.children)))
  }
}

