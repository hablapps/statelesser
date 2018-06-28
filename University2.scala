package org.apps.gist
package university

import scalaz._, Scalaz._

/**
 * State algebras
 */

trait Getter[P[_],S]{
  def apply(): P[S]
}

object Getter {
  def apply[P[_], S](get: P[S]): Getter[P, S] =
    new Getter[P, S] { def apply() = get }
}

trait Setter[P[_],S]{
  def apply(t: S): P[Unit]
}

object Setter {
  def apply[P[_], S](put: S => P[Unit]): Setter[P, S] =
    new Setter[P, S] { def apply(s: S) = put(s) }
}

trait Field[P[_],S]{
  val get: Getter[P,S]
  val put: Setter[P,S]

  def modify(f: S => S)(implicit B: Bind[P]): P[Unit] = 
    get() >>= { s => put(f(s)) } 
}

trait ListP[Alg[_[_],_],P[_],S]{
  val apply: P[List[Alg[P,S]]]

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

  // def putAll(values: List[S])(implicit M: Monad[P]): P[List[Unit]] =
  //   traverseZip(values)(_ put _)

  def putAll(values: List[S])(implicit M: Monad[P]): P[List[Unit]] =
    apply >>= (algs => (algs zip values) traverse {
      case (alg, v) => alg put v
    })
}

/**
 * Data Model
 */

object Primitive{
  type IntegerP[P[_]]=Field[P,Int]
  type BooleanP[P[_]]=Field[P,Boolean]
  type StringP[P[_]]=Field[P,String]
}

import Primitive._

trait Department[P[_],D]{
  val self: Field[P,D]
  val budget: IntegerP[P]
}

trait University[P[_],U]{
  type D

  val self: Field[P,U]
  val name: StringP[P]
  val deps: ListP[Department,P,D]
}

trait Logic[P[_],U]{
  val univ: University[P,U]

  def doubleBudget(implicit M: Monad[P]): P[List[Unit]] =
    univ.deps traverse { _.budget.modify(_*2) }
  
  // should be reused
  def getDepts(implicit M: Monad[P]): P[List[univ.D]] = 
    univ.deps traverse { _.self.get() }
}

/* Data instance */


case class SUniversity(name: String, departs: Map[String, SDepartment])

case class SDepartment(budget: Int)

object Util {

  type Lens[S, A] = State[A, ?] ~> State[S, ?]

  object Lens {

    def apply[S, A](get: S => A, put: A => S => S): Lens[S, A] =
      λ[State[A, ?] ~> State[S, ?]] { sta =>
        State { s =>
          val (a, x) = sta.run(get(s))
          (put(a)(s), x)
        }
      }
    
    def id[S]: Lens[S, S] = NaturalTransformation.refl
  }

  implicit def lens2StateField[S, A](ln: Lens[S, A]): Field[State[S, ?], A] =
    new Field[State[S, ?], A] {
      val get = Getter(ln(State.get)) 
      val put = Setter(a => ln(State.put(a)))
    }
}
 
import Util.{ Lens, _ }

object StateDepartment extends Department[State[SDepartment, ?], SDepartment] {
  val self   = Lens.id[SDepartment]
  val budget = Lens[SDepartment, Int](_.budget, b => _.copy(budget = b))
}

object StateUniversity extends University[State[SUniversity, ?], SUniversity] {
  type D = SDepartment
  val self = Lens.id[SUniversity]
  val name = Lens[SUniversity, String](_.name, n => _.copy(name = n))
  val deps = new ListP[Department, State[SUniversity, ?], SDepartment] {
    val apply = State.gets(s => s.departs.toList.map { case (k, v) =>
      new Department[State[SUniversity, ?], D] {
        val self = Lens[SUniversity, SDepartment](
          _ => v, v2 => s => s.copy(departs = s.departs.updated(k, v2)))
        val budget = Lens[SUniversity, Int](
          _ => v.budget, 
          v2 => s => s.copy(departs = s.departs.updated(k, v.copy(v2))))
      }
    })
  }
}

object Main extends App {

  val logic = new Logic[State[SUniversity, ?], SUniversity] {
    val univ = StateUniversity
  }
  
  val math = SDepartment(3000)
  val cs = SDepartment(5000)
  val univ = SUniversity("urjc", Map("math" -> math, "cs" -> cs))
 
  val univ2 = logic.doubleBudget(implicitly).exec(univ)
  val deps  = logic.getDepts(implicitly).eval(univ2)

  println(s"univ2 = $univ2")
  println(s"deps  = $deps")
}

