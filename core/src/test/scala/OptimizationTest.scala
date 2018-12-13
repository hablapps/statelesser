package statelesser
package test

import org.scalatest._

import CoupleExample._, instance._

class OptimizationTest extends FlatSpec with Matchers {

  "Test optimization stack" should "remove simple first from query" in {
    runStack(peopleName_1) shouldBe ""
  }

  it should "normalise a dummy query" in {
    runStack(peopleName_2) shouldBe ""
  }

  it should "produce variables" in {
    runStack(herNameAndStreet) shouldBe ""
  }

  it should "produce variables again" in {
    runStack(herAndHimStreet_1) shouldBe ""
  }

  it should "produce something" in {
    runStack(getPeopleNameAndAge_1) shouldBe ""
  }

  // "Test optimization stack" should "remove simple first from query" in {
  //   run(getPeopleName_2) shouldBe exp
  // }

  // it should "remove several firsts from query" in {
  //   run(getPeopleName_3) shouldBe exp
  // }

  // it should "remove a chain of composed firsts" in {
  //   run(getPeopleName_4) shouldBe exp
  // }

  // it should "remove a first which contains a continuation" in {
  //   run(getPeopleName_5) shouldBe exp
  // }

  // it should "reduce a quite dummy query" in {
  //   run(getPeopleName_5) shouldBe exp
  // }

  // val exp2 = "getAll(people > (name * likeInt(3)).asFold1.asFold)"

  // it should "reduce a query with likes" in {
  //   run(getPeopleNameAnd3_1) shouldBe exp2
  // }

  // it should "reduce a quite dummy query with likes" in {
  //   run(getPeopleNameAnd3_2) shouldBe exp2
  // }

  // it should "reduce a substraction with 0 on the right" in {
  //   run(getPeopleNameAnd3_3) shouldBe exp2
  // }

  // it should "reduce a substraction by evaluating likes" in {
  //   run(getPeopleNameAnd3_4) shouldBe exp2
  // }
}

