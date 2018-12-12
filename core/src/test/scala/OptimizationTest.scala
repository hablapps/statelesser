package statelesser
package test

import scalaz._
import org.scalatest._

import OpticLang._

class OptimizationTest extends FlatSpec with Matchers {

  // XXX: Handling the stack is insane! We should take a look at Oleg's
  // implementation. It feels like Shapeless could help here.

  type Stack[A] = 
    ProductProduct[
    ProductProduct[
    ProductProduct[
    VertLeftAssoc[
    VertLeftAssoc[
    VertLeftAssoc[
    VertLeftAssoc[
    VertLeftAssoc[
    ClearId[
    ProductFirst[
    ProductFirst[
    ProductSecond[
    ProductSecond[
    VerticalLike[
    IntEval[
    TSemantic[Const[String, ?], 
    ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], A]

  val ev20 = IntEval.optimization[TSemantic[Const[String, ?], ?]]
  val ev18 = VerticalLike.optimization(ev20)
  val ev15 = ProductSecond.optimization(ev18)
  val ev14 = ProductSecond.optimization(ev15)
  val ev11 = ProductFirst.optimization(ev14)
  val ev10 = ProductFirst.optimization(ev11)
  val ev06 = ClearId.optimization(ev10)
  val ev0a = VertLeftAssoc.optimization(ev06)
  val ev05 = VertLeftAssoc.optimization(ev0a)
  val ev04 = VertLeftAssoc.optimization(ev05)
  val ev03 = VertLeftAssoc.optimization(ev04)
  val ev02 = VertLeftAssoc.optimization(ev03)
  val ev01 = ProductProduct.optimization(ev02)
  val ev00 = ProductProduct.optimization(ev01)
  val ev_f = ProductProduct.optimization(ev00)

  private def wrap[O[_, _], S, A](
      sem: TSemantic[Const[String, ?], O[S, A]]): Stack[O[S, A]] =
    ProductProduct.Unk(
    ProductProduct.Unk(
    ProductProduct.Unk(
    VertLeftAssoc.Unk(
    VertLeftAssoc.Unk(
    VertLeftAssoc.Unk(
    VertLeftAssoc.Unk(
    VertLeftAssoc.Unk(
    ClearId.Unk(
    ProductFirst.Unk(
    ProductFirst.Unk(
    ProductSecond.Unk(
    ProductSecond.Unk(
    VerticalLike.Unk(  
    IntEval.Unk(sem)
    ))))))))))))))

  private def wrapPlainG[S, A](s: String): Stack[Getter[S, A]] =
    wrap(TGetter(expr = Wrap(Const(s))))

  private def wrapTableG[S, A](v: String, s: String): Stack[Getter[S, A]] =
    wrap(TGetter(Set(v -> Wrap[Const[String, ?], Getter, S, A](Const(s))), Var(v)))

  private def wrapTableF[S, A](v: String, s: String): Stack[Fold[S, A]] =
    wrap(TFold(Set(v -> Wrap[Const[String, ?], Fold, S, A](Const(s))), Var(v)))

  val pretty = new CoupleExample[Stack] {
    type Couple = Unit
    type Person = Unit

    val ev = ev_f

    def couples = wrapTableF("c", "couples")
    val her = wrapTableG("w", "her")
    val him = wrapTableG("m", "him")
    def people = wrapTableF("p", "people")
    val name = wrapPlainG("name")
    val age = wrapPlainG("age")
    val weight = wrapPlainG("weight")
    val address = wrapTableG("a", "address")
    val street = wrapPlainG("street")
  }

  import pretty._

  val exp = ()

  def run[A](p: Stack[A]): TSemantic[Const[String, ?], A] =
    ev20.run(
    ev18.run(
    ev15.run(
    ev14.run(
    ev11.run(
    ev10.run(
    ev06.run(
    ev0a.run(
    ev05.run(
    ev04.run(
    ev03.run(
    ev02.run(
    ev01.run(
    ev00.run(
    ev.run(p)))))))))))))))

  "Test optimization stack" should "remove simple first from query" in {
    run(peopleName_1) shouldBe exp
  }

  it should "normalise a dummy query" in {
    run(peopleName_2) shouldBe exp
  }

  it should "produce variables" in {
    run(herNameAndStreet) shouldBe exp
  }

  it should "produce variables again" in {
    run(herAndHimStreet_1) shouldBe exp
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

