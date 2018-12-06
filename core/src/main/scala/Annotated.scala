package statelesser

trait Annotated[Alg[_[_]], Ann[_[_], _], E[_]] {

  val alg: Alg[E]

  def inject[A](e: E[A]): Ann[E, A]

  def run[A](ann: Ann[E, A]): E[A]
}

