package org.hablapps.statesome
package test
package university

import scalaz._
import shapeless.syntax.singleton._
import monocle.macros.Lenses

import Util._, AlgFunctor._, GetEvidence._

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

case class University[D, P[_], U](
  self: Field[P, U],
  name: StringP[P],
  math: Department[P, D])

object University {

  def instance[P[_], U, D](implicit 
      ge: GetEvidence[University[D, P, U]]): University[D, P, U] =
    ge.apply

  implicit def UniversityAlgFunctor[D] = 
    new AlgFunctor[University[D, ?[_], ?]] {
      def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
        λ[University[D, Q, ?] ~> University[D, P, ?]] { alg =>
          University(alg.self.amap(f), alg.name.amap(f), alg.math.amap(f))
        }
    }
}

case class City[U, D, P[_], C](
  self: Field[P, C],
  popu: Field[P, Int],
  univ: University[D, P, U])

object City {
  
  def instance[P[_], C, U, D](implicit
      ge: GetEvidence[City[U, D, P, C]]): City[U, D, P, C] =
    ge.apply

  implicit def CityAlgFunctor[U, D] =
    new AlgFunctor[City[U, D, ?[_], ?]] {
      def amap[Q[_]: Functor, P[_]: Functor](f: Q ~> P) =
        λ[City[U, D, Q, ?] ~> City[U, D, P, ?]] { alg =>
          City(alg.self.amap(f), alg.popu.amap(f), alg.univ.amap(f))
        }
    }
}

case class Logic[P[_], C, U, D](city: City[U, D, P, C]) {

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

@Lenses case class SCity(population: Int, univ: SUniversity)
@Lenses case class SUniversity(name: String, math: SDepartment)
@Lenses case class SDepartment(budget: Int)

import SCity._, SUniversity._, SDepartment._

import org.scalatest._

class UniversitySpec extends FlatSpec with Matchers {

  implicit val popuLn = 'popu ->> population
  implicit val univLn = 'univ ->> univ
  implicit val nameLn = 'name ->> name
  implicit val mathLn = 'math ->> math 
  implicit val budgLn = 'budget ->> budget

  "Automagic instances" should "be generated for city" in {
  
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
    Department.instance[State[SDepartment, ?], SDepartment]
  }
  
  it should "be generated for university" in {
    University.instance[State[SUniversity, ?], SUniversity, SDepartment]
  }
}

