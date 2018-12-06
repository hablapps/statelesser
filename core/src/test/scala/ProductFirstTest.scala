package statelesser
package test

import scalaz._

import org.scalatest._

class ProductFirstTest extends FlatSpec with Matchers {

  type Stack[A] = ClearId[ProductFirst[ProductFirst[Const[String, ?], ?], ?], A]

  val pretty = new CoupleExample[Stack] {
    type Couple = Unit
    type Person = Unit

    val ev2 = ProductFirst.optimization[Const[String, ?]]
    val ev1 = ProductFirst.optimization(ev2)
    val ev  = ClearId.optimization(ev1)

    private def wrap[A](s: String): Stack[A] =
      ev.inject(ev1.inject(ev2.inject(Const[String, A](s))))

    val couples = wrap("couples")
    val her = wrap("her")
    val him = wrap("him")
    val people = wrap("people")
    val name = wrap("name")
    val age = wrap("age")
    val weight = wrap("weight")
  }

  import pretty._, ev2.{run => run2}, ev1.{run => run1}, ev.{run => run0}

  val exp = "getAll(people > name.asFold1.asFold)"

  def run[A](p: Stack[A]): String = run2(run1(run0(p))).getConst

  "Product-first optimization" should "remove simple first from query" in {
    run(getPeopleName_2) shouldBe exp
  }

  it should "remove several firsts from query" in {
    run(getPeopleName_3) shouldBe exp
  }

  it should "remove a chain of composed firsts" in {
    run(getPeopleName_4) shouldBe exp
  }

  it should "remove a first which contains a continuation" in {
    run(getPeopleName_5) shouldBe exp
  }
}

