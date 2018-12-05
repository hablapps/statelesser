package statelesser
package test

import scalaz._

import org.scalatest._

import ProductFirst.{pfOpticLang, Unk, run => runPF}

class ProductFirstTest extends FlatSpec with Matchers {

  val pretty = new CoupleExample[ProductFirst[Const[String, ?], ?]] {
    type Couple = Unit
    type Person = Unit

    val ev = pfOpticLang[Const[String, ?]]

    val couples = Unk(Const("couples"))
    val her = Unk(Const("her"))
    val him = Unk(Const("him"))
    val people = Unk(Const("people"))
    val name = Unk(Const("name"))
    val age = Unk(Const("age"))
    val weight = Unk(Const("weight"))
  }

  import pretty._

  val exp = "getAll(people > name.asFold1.asFold)"

  "Product-first optimization" should "remove simple first from query" in {
    runPF(getPeopleName_2).getConst shouldBe exp
  }

  it should "remove several firsts from query" in {
    runPF(getPeopleName_3).getConst shouldBe exp
  }

  it should "remove a chain of composed firsts" in {
    runPF(getPeopleName_4).getConst shouldBe exp
  }

  it should "remove a first which contains a continuation" in {
    runPF(getPeopleName_5).getConst shouldBe exp
  }
}

