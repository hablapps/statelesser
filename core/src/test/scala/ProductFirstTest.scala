package statelesser
package test

import scalaz._

import org.scalatest._

import ProductFirst.{pfOpticLang, Unk}

class ProductFirstTest extends FlatSpec with Matchers {

  val pretty = new CoupleExample[
      ProductFirst[ProductFirst[Const[String, ?], ?], ?]] {
    type Couple = Unit
    type Person = Unit

    val ev1 = pfOpticLang[Const[String, ?]]
    val ev = pfOpticLang(ev1)

    val couples = Unk(Unk(Const("couples")))
    val her = Unk(Unk(Const("her")))
    val him = Unk(Unk(Const("him")))
    val people = Unk(Unk(Const("people")))
    val name = Unk(Unk(Const("name")))
    val age = Unk(Unk(Const("age")))
    val weight = Unk(Unk(Const("weight")))
  }

  import pretty._, ev1.{run => run1}, ev.{run => run0}

  val exp = "getAll(people > name.asFold1.asFold)"

  "Product-first optimization" should "remove simple first from query" in {
    run1(run0(getPeopleName_2)).getConst shouldBe exp
  }

  it should "remove several firsts from query" in {
    run1(run0(getPeopleName_3)).getConst shouldBe exp
  }

  it should "remove a chain of composed firsts" in {
    run1(run0(getPeopleName_4)).getConst shouldBe exp
  }

  it should "remove a first which contains a continuation" in {
    run1(run0(getPeopleName_5)).getConst shouldBe exp
  }
}

