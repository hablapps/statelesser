package org.hablapps
package statelesser

package test
package university

import scalaz._, Scalaz._
import org.scalatest._
import GetEvidence._

class UniversitySpec extends FlatSpec with Matchers {
  
  case class SPerson(first: String, last: String)
  val john = SPerson("john", "doe")

  case class SDepartment(budget: Int, boss: SPerson)
  val math = SDepartment(100000, john)

  case class SUniversity(dep: SDepartment)
  val urjc = SUniversity(math)

  "Automagic instance" should "be generated for a one-field algebra" in {

    case class Person[P[_], Per: MonadState[P, ?]](last: LensAlg[P, String])

    val personState = make[Person[State[SPerson, ?], SPerson]]

    val getLast = personState.last.get
    val upcLast = personState.last.modify(_.toUpperCase)

    getLast(john) shouldBe ((john, john.last))
    upcLast.exec(john) shouldBe john.copy(last = john.last.toUpperCase)
  }

  case class University[Dep, Per, P[_], Univ: MonadState[P, ?]](
    dep: LensAlgHom[Department[Per, ?[_], ?], P, Dep])

  case class Department[Per, P[_], Dep: MonadState[P, ?]](
    budget: LensAlg[P, Int],
    boss: LensAlgHom[Person, P, Per])

  case class Person[P[_], Per: MonadState[P, ?]](
    first: LensAlg[P, String],
    last:  LensAlg[P, String])

  it should "be generated for an algebra with several fields" in {

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

  it should "be generated for an algebra with several nested fields" in {

    val universityState = 
      make[University[SDepartment, SPerson, State[SUniversity, ?], SUniversity]]

    import universityState._, dep.alg._, boss.alg._

    val depBossLast = dep composeLens boss composeLens last
    val getLast = depBossLast.get
    val upcLast = depBossLast.modify(_.toUpperCase)

    getLast(urjc) shouldBe ((urjc, urjc.dep.boss.last))
    upcLast.exec(urjc) shouldBe
      urjc.copy(dep = 
        urjc.dep.copy(boss = 
          urjc.dep.boss.copy(last = 
            urjc.dep.boss.last.toUpperCase)))
  }

  it should "be generated for an algebra with a n-ary field" in {
    
    case class Person[P[_], Per: MonadState[P, ?]](
      nicks: TraversalAlg[P, String])

    case class SPerson(nicks: List[String])
    val cristina = SPerson(List("cifu", "presi", "cris"))

    val personState = make[Person[State[SPerson, ?], SPerson]]

    val getNicks = personState.nicks.getAll
    val upcNicks = personState.nicks.modify(_.toUpperCase)

    getNicks(cristina) shouldBe ((cristina, cristina.nicks))
    upcNicks.exec(cristina) shouldBe 
      cristina.copy(nicks = cristina.nicks.map(_.toUpperCase))
  }

  it should "be generated for an algebra with mixed fields" in {
    
    case class Person[P[_], Per: MonadState[P, ?]](
      last: LensAlg[P, String],
      nicks: TraversalAlg[P, String])

    case class SPerson(last: String, nicks: List[String])
    val cris = SPerson("cifuentes", List("cifu", "presi", "cris"))

    val personState = make[Person[State[SPerson, ?], SPerson]]

    val getLast = personState.last.get
    val upcLast = personState.last.modify(_.toUpperCase)
    val getNicks = personState.nicks.getAll
    val upcNicks = personState.nicks.modify(_.toUpperCase)

    getLast(cris) shouldBe ((cris, cris.last))
    upcLast.exec(cris) shouldBe cris.copy(last = cris.last.toUpperCase)
    getNicks(cris) shouldBe ((cris, cris.nicks))
    upcNicks.exec(cris) shouldBe 
      cris.copy(nicks = cris.nicks.map(_.toUpperCase))
  }


  it should "be generated for an algebra with a nested n-ary field" in {
    
    case class Department[Per, P[_], Dep: MonadState[P, ?]](
      budget: LensAlg[P, Int],
      bosses: TraversalAlgHom[Person, P, Per])

    case class Person[P[_], Per: MonadState[P, ?]](
      last: LensAlg[P, String])

    case class SDepartment(budget: Int, bosses: List[SPerson])
    case class SPerson(last: String)
    val cris = SPerson("cifuentes")
    val john = SPerson("doe")
    val math = SDepartment(100000, List(cris, john))

    val departmentState = 
      make[Department[SPerson, State[SDepartment, ?], SDepartment]]
    import departmentState._, bosses.alg._

    val bossLasts = bosses composeLens last
    val getLasts = bossLasts.getAll
    val upcLasts = bossLasts.modify(_.toUpperCase)

    getLasts(math) shouldBe ((math, math.bosses.map(_.last)))
    upcLasts.exec(math) shouldBe
      math.copy(bosses = math.bosses.map(p => p.copy(last = p.last.toUpperCase)))
  }

  it should "be generated for a complex data layer" in {

    case class University[Dep, Per, P[_], Univ: MonadState[P, ?]](
      deps: TraversalAlgHom[Department[Per, ?[_], ?], P, Dep])

    case class Department[Per, P[_], Dep: MonadState[P, ?]](
      budget: LensAlg[P, Int],
      boss: LensAlgHom[Person, P, Per])

    case class Person[P[_], Per: MonadState[P, ?]](
      first: LensAlg[P, String],
      last:  LensAlg[P, String])

    case class SPerson(first: String, last: String)
    val john = SPerson("john", "doe")
    val cris = SPerson("cristina", "cifuentes")

    case class SDepartment(budget: Int, boss: SPerson)
    val cs = SDepartment(150000, cris)
    val math = SDepartment(100000, john)

    case class SUniversity(deps: List[SDepartment])
    val urjc = SUniversity(List(cs, math))

    val universityState = 
      make[University[SDepartment, SPerson, State[SUniversity, ?], SUniversity]]
    import universityState._, deps.alg._, boss.alg._

    val univBossLasts = deps composeLens boss composeLens last
    val getUnivBossLasts = univBossLasts.getAll
    val ucUnivBossLasts = univBossLasts.modify(_.toUpperCase)

    getUnivBossLasts(urjc) shouldBe ((urjc, urjc.deps.map(_.boss.last)))
    ucUnivBossLasts.exec(urjc) shouldBe
      urjc.copy(deps =
        urjc.deps.map(d => d.copy(boss =
          d.boss.copy(last = d.boss.last.toUpperCase))))
  }
}

