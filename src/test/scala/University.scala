package org.hablapps.statesome
package test
package university

import scalaz._
import shapeless._, shapeless.syntax.singleton._

import Util.{ Lens, _ }, AlgFunctor._, GetEvidence._

/**
 * Data Model
 */

object Primitive{
  type IntegerP[P[_]]=Field[P,Int]
  type BooleanP[P[_]]=Field[P,Boolean]
  type StringP[P[_]]=Field[P,String]
}

import Primitive._

case class Department[P[_],D](
  self: Field[P,D],
  budget: IntegerP[P])

object Department {

  def instance[P[_], D](implicit 
      ge: GetEvidence[Department[P, D]]): Department[P, D] = 
    ge.apply

  implicit val DepartmentAlgFunctor = new AlgFunctor[Department] {
    def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
      λ[Department[Q, ?] ~> Department[P, ?]] { alg =>
        Department(alg.self.amap(f), alg.budget.amap(f))
      }
  }
}

case class University[P[_], U, D](
  self: Field[P, U],
  name: StringP[P],
  math: Department[P, D])

object University {

  def instance[P[_], U, D](implicit 
      ge: GetEvidence[University[P, U, D]]): University[P, U, D] =
    ge.apply

  implicit def UniversityAlgFunctor[D] = 
    new AlgFunctor[University[?[_], ?, D]] {
      def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
        λ[University[Q, ?, D] ~> University[P, ?, D]] { alg =>
          University(alg.self.amap(f), alg.name.amap(f), alg.math.amap(f))
        }
    }
}

case class City[P[_], C, U, D](
  self: Field[P, C],
  popu: Field[P, Int],
  univ: University[P, U, D])

object City {
  
  def instance[P[_], C, U, D](implicit
      ge: GetEvidence[City[P, C, U, D]]): City[P, C, U, D] =
    ge.apply
}

case class Logic[P[_], C, U, D](city: City[P, C, U, D]) {

  def getPopu: P[Int] =
    city.popu.get()

  def doubleBudget(implicit M: Monad[P]): P[Unit] =
    city.univ.math.budget.modify(_ * 2)
  
  // should be reused
  def getMathDep: P[D] = 
    city.univ.math.self.get()
}


/** 
 * Data Layer Interpretation
 */

case class SCity(population: Int, univ: SUniversity)

object SCity {
 
  val popuLn: Lens[SCity, Int] =
    Lens(_.population, p => _.copy(population = p))

  val univLn: Lens[SCity, SUniversity] =
    Lens(_.univ, u => _.copy(univ = u))
}

case class SUniversity(name: String, math: SDepartment)

object SUniversity {

  val nameLn: Lens[SUniversity, String] =
    Lens(_.name, n => _.copy(name = n))
  
  val mathLn: Lens[SUniversity, SDepartment] =
    Lens(_.math, d => _.copy(math = d))
}

case class SDepartment(budget: Int)

object SDepartment {

  val budgetLn = Lens[SDepartment, Int](_.budget, b => _.copy(budget = b))
}

import SCity._, SUniversity._, SDepartment._

import org.scalatest._

class UniversitySpec extends FlatSpec with Matchers {

  "Automagic instances" should "be generated for city" in {

    implicit val self = 'self ->> lensId[SCity]
    implicit val popu = 'popu ->> popuLn
    implicit val univ = 'univ ->> univLn
    implicit val sel1 = 'self ->> (univ compose lensId[SUniversity])
    implicit val name = 'name ->> (univ compose nameLn)
    implicit val math = 'math ->> (univ compose mathLn)
    implicit val sel2 = 'self ->> (math compose lensId[SDepartment])
    implicit val budg = 'budget ->> (math compose budgetLn)

    val StateCity = 
      City.instance[State[SCity, ?], SCity, SUniversity, SDepartment]
  
     val logic = Logic(StateCity)
    
     val urjc = SUniversity("urjc", SDepartment(3000))
     val most = SCity(200000, urjc)

     val popu2 = logic.getPopu.eval(most)
     val most2 = logic.doubleBudget(implicitly).exec(most)
     val math2 = logic.getMathDep.eval(most2)

     popu2 should be (200000)
     most2 should be (SCity(200000, SUniversity("urjc", SDepartment(6000))))
     math2 should be (SDepartment(6000))
  }

  it should "be generated for department" in {
    
    implicit val self = 'self ->> lensId[SDepartment]
    implicit val budg = 'budget ->> budgetLn

    val StateDepartment =
      Department.instance[State[SDepartment, ?], SDepartment]
  }
  
  it should "be generated for university" in {

    implicit val sel1 = 'self ->> lensId[SUniversity]
    implicit val name = 'name ->> nameLn
    implicit val matx = 'math ->> mathLn
    implicit val sel2 = 'self ->> (mathLn compose lensId[SDepartment])
    implicit val budg = 'budget ->> (mathLn compose budgetLn)

    val StateUniversity = 
      University.instance[State[SUniversity, ?], SUniversity, SDepartment]
  }
}

