package statelesser
package test

import scalaz.Const

import OpticLang._

trait CoupleExample[Expr[_]] {

  implicit val ev: OpticLang[Expr]

  /* data layer */
  
  type Couple
  type Couples = List[Couple]
  type Person
  type People = List[Person]
  type Address

  def couples: Expr[Fold[Couples, Couple]]
  def her: Expr[Getter[Couple, Person]]
  def him: Expr[Getter[Couple, Person]]
  def people: Expr[Fold[People, Person]]
  def name: Expr[Getter[Person, String]]
  def age: Expr[Getter[Person, Int]]
  def weight: Expr[Getter[Person, Int]]
  def address: Expr[Getter[Person, Address]]
  def street: Expr[Getter[Address, String]]

  /* logic */

  import ev._
  import OpticLang.syntax._

  def getPeople: Expr[Fold[People, Person]] =
    people

  def getPeopleName_1: Expr[Fold[People, String]] =
    people > name.asFold

  def getPeopleName_2: Expr[Fold[People, String]] =
    people > ((name * age) > first).asFold

  def peopleName_1: Expr[Fold[People, String]] = 
    people > (name * age * weight > first > first).asFold

  def peopleName_2: Expr[Fold[People, String]] =
    people > (
      name * weight * age * name * weight >
      id >
      second * first >
      second * first >
      first * id >
      first >
      second).asFold

  def getPeopleName_3: Expr[People => List[String]] =
    getAll(people > (((name * age) * weight > first) > first).asFold)

  def getPeopleName_4: Expr[People => List[String]] =
    getAll(people > (((name * age) * weight > (first > first))).asFold)

  def getPeopleName_5: Expr[People => List[String]] =
    getAll(people > (name * (age * weight) > (first > id)).asFold)

  def getPeopleName_6: Expr[People => List[String]] =
    getAll(people >
      (name * weight * age * name * weight >
      id >
      second * first >
      second * first >
      first * id >
      first >
      second).asFold)

  def getPeopleNameAnd3_1: Expr[People => List[(String, Int)]] =
    getAll(people > (name * likeInt(3)).asFold)

  def getPeopleNameAnd3_2: Expr[People => List[(String, Int)]] =
    getAll(people >
      (name * weight * age * name * weight >
      id >
      first * likeInt(3) >
      (first[(((String, Int), Int), String), Int] > second) * second >
      id).asFold)

  def getPeopleNameAnd3_3: Expr[People => List[(String, Int)]] =
    getAll(people >
      ((name * (likeInt(3) * likeInt(0))) >
      first * (second > sub)).asFold)

  def getPeopleNameAnd3_4: Expr[People => List[(String, Int)]] =
    getAll(people >
      ((name * (likeInt(4) * likeInt(1))) >
      first * (second > sub)).asFold)

  def getPeopleNameAndAge_1: Expr[Fold[People, (String, Int)]] =
    people > (name * age).asFold

  def getPeopleNameAndAge_2: Expr[Fold[People, (String, Int)]] =
    people > ((name * age * weight) > first).asFold

  def getHer: Expr[Fold[Couples, Person]] =
    couples > her.asFold

  def herNameAndStreet: Expr[Fold[Couples, (String, String)]] =
    couples > (her > name * (address > street)).asFold

  def herAndHimStreet_1: Expr[Fold[Couples, (String, String)]] =
    couples > (
      her * him > (first > address > street) * (second > address > street)).asFold

  def getHerName: Expr[Fold[Couples, String]] =
    couples > her.asFold > name.asFold

  def getHerNameAndAge_1: Expr[Fold[Couples, (String, Int)]] =
    couples > her.asFold > (name * age).asFold

  def getHerNameAndAge_2: Expr[Fold[Couples, (String, Int)]] =
    couples > ((her > name) * (her > age)).asFold

  def getHerNameAndAge_3: Expr[Fold[Couples, (String, Int)]] =
    couples > (her > name * age).asFold

  def getPeopleGt30: Expr[People => List[(String, Int)]] =
    getAll(people > (name.asAffineFold * 
      (age.asAffineFold > filtered (gt(30)))).asFold)

  def getHerGt30_1: Expr[Couples => List[(String, Int)]] =
    getAll(couples > her.asFold > (name.asAffineFold * 
      (age.asAffineFold > filtered (gt(30)))).asFold)

  def getHerGt30_2: Expr[Couples => List[(String, Int)]] =
    getAll(couples > ((her > name).asAffineFold * 
      ((her > age).asAffineFold > filtered (gt(30)))).asFold)

  def getHerNameGt30_1: Expr[Couples => List[String]] =
    getAll(couples > her.asFold > (name.asAffineFold <* 
      (age.asAffineFold > filtered (gt(30)))).asFold)
  
  def getHerNameGt30_2: Expr[Couples => List[String]] =
    getAll(couples > ((her > name).asAffineFold <* 
      ((her > age).asAffineFold > filtered (gt(30)))).asFold)

  def differenceAll: Expr[Couples => List[(String, Int)]] =
    getAll(couples >
      ((her > name) * ((her > age) * (him > age) > sub)).asFold)

  def difference: Expr[Couples => List[(String, Int)]] =
    getAll(couples > 
      ((her > name).asAffineFold * 
        (((her > age) - (him > age)).asAffineFold > filtered (gt(0)))).asFold)

  def differenceName_1: Expr[Couples => List[String]] =
    getAll(couples > 
      ((her > name).asAffineFold <* 
        (((her > age) - (him > age)).asAffineFold > filtered (gt(0)))).asFold)

  def differenceName_2: Expr[Couples => List[String]] =
    getAll(couples > 
      ((her > name).asAffineFold * 
        (((her > age) - (him > age)).asAffineFold > filtered (gt(0))) > 
          first.asAffineFold).asFold)

  def dummyNameAndAge: Expr[People => List[(String, Int)]] =
    getAll(people > ((name.asAffineFold * ((name * age > 
      first * second > 
      second * first > 
      second * first > 
      second).asAffineFold 
      > filtered (gt(30))
      > filtered (gt(40))))).asFold)
}

object CoupleExample {

  type Stack[A] = 
    DistAs[
    DistAs[
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
    ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], ?], A]

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
  val ev_z = ProductProduct.optimization(ev00)
  val ev_y = DistAs.optimization(ev_z)
  val ev_x = DistAs.optimization(ev_y)

  def runStack[A](p: Stack[A]): TSemantic[Const[String, ?], A] =
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
    ev_z.run(
    ev_y.run(
    ev_x.run(p)))))))))))))))))

  private def wrap[O[_, _], S, A](
      sem: TSemantic[Const[String, ?], O[S, A]]): Stack[O[S, A]] =
    DistAs.Unk(
    DistAs.Unk(
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
    ))))))))))))))))

  private def wrapPlainG[S, A](s: String, inf: OpticInfo): Stack[Getter[S, A]] =
    wrap(TGetter(expr = Wrap(Const(s), inf)))

  private def wrapTableG[S, A](
      v: String, s: String, inf: OpticInfo): Stack[Getter[S, A]] =
    wrap(TGetter(Set(v -> Wrap[Const[String, ?], Getter, S, A](Const(s), inf)), Var(v)))

  private def wrapTableF[S, A](
      v: String, s: String, inf: OpticInfo): Stack[Fold[S, A]] =
    wrap(TFold(Set(v -> Wrap[Const[String, ?], Fold, S, A](Const(s), inf)), Var(v)))

  val instance = new CoupleExample[Stack] {
    type Couple = Unit
    type Person = Unit

    val ev = ev_x

    val couples = wrapTableF("c", "couples", 
      OpticInfo(KFold, "couples", TypeInfo("Couples"), TypeInfo("Couple", true)))
    
    val her = wrapTableG("w", "her",
      OpticInfo(KGetter, "her", TypeInfo("Couple", true), TypeInfo("Person", true)))

    val him = wrapTableG("m", "him",
      OpticInfo(KGetter, "him", TypeInfo("Couple", true), TypeInfo("Person", true)))
    
    val people = wrapTableF("p", "people",
      OpticInfo(KFold, "people", TypeInfo("People"), TypeInfo("Person", true)))

    val name = wrapPlainG("name",
      OpticInfo(KGetter, "name", TypeInfo("Person", true), TypeInfo("String")))

    val age = wrapPlainG("age",
      OpticInfo(KGetter, "age", TypeInfo("Person", true), TypeInfo("Int")))

    val weight = wrapPlainG("weight",
      OpticInfo(KGetter, "weight", TypeInfo("Person", true), TypeInfo("Int")))

    val address = wrapTableG("a", "address",
      OpticInfo(KGetter, "address", TypeInfo("Person", true), TypeInfo("Address", true)))
    
    val street = wrapPlainG("street",
      OpticInfo(KGetter, "street", TypeInfo("Address", true), TypeInfo("String")))
  }
}

