package org.hablapps
package statelesser

package test
package university

import scalaz._
import org.scalatest._
import GetEvidence._

class UniversitySpec extends FlatSpec with Matchers {
  
  case class SPerson(first: String, last: String)
  val john = SPerson("john", "doe")

  case class SDepartment(budget: Int, boss: SPerson)
  val math = SDepartment(100000, john)

  "Automagic instances" should "be generated for a one-field algebra" in {

    case class Person[P[_], Per: MonadState[P, ?]](last: LensAlg[P, String])

    val personState = make[Person[State[SPerson, ?], SPerson]]

    val getLast = personState.last.get
    val upcLast = personState.last.modify(_.toUpperCase)

    getLast(john) shouldBe ((john, john.last))
    upcLast.exec(john) shouldBe john.copy(last = john.last.toUpperCase)
  }

  it should "be generated for an algebra with several fields" in {

    case class Person[P[_], Per: MonadState[P, ?]](
      first: LensAlg[P, String],
      last:  LensAlg[P, String])

    val personState = make[Person[State[SPerson, ?], SPerson]]

    val getFirst = personState.first.get
    val revFirst = personState.first.modify(_.reverse)
    val getLast  = personState.last.get
    val upcLast  = personState.last.modify(_.toUpperCase)

    getFirst(john) shouldBe ((john, john.first))
    revFirst.exec(john) shouldBe john.copy(first = john.first.reverse)
    getLast(john) shouldBe ((john, john.last))
    upcLast.exec(john) shouldBe john.copy(last = john.last.toUpperCase)
  }

  it should "be generated for an algebra with a nested field" in {

    case class Department[Per, P[_], Dep: MonadState[P, ?]](
      budget: LensAlg[P, Int],
      boss: LensAlgHom[Person, P, Per])

    case class Person[P[_], Per: MonadState[P, ?]](
      first: LensAlg[P, String],
      last:  LensAlg[P, String])

    val departmentState = 
      make[Department[SPerson, State[SDepartment, ?], SDepartment]]

    import departmentState._, boss.alg._

    val getBudg = budget.get
    val douBudg = budget.modify(_ * 2)
    val bossLast = boss composeLens last
    val getLast = bossLast.get
    val upcLast = bossLast.modify(_.toUpperCase)

    getBudg(math) shouldBe ((math, math.budget))
    douBudg.exec(math) shouldBe math.copy(budget = math.budget * 2)
    getLast(math) shouldBe ((math, math.boss.last))
    upcLast.exec(math) shouldBe 
      math.copy(boss = math.boss.copy(last = math.boss.last.toUpperCase))
  }
}

