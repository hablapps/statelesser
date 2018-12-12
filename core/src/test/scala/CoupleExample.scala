package statelesser
package test

import scalaz.Const

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

  def getPeople: Expr[People => List[Person]] =
    getAll(people)

  def getPeopleName_1: Expr[People => List[String]] =
    getAll(people > name.asFold)

  def getPeopleName_2: Expr[People => List[String]] =
    getAll(people > ((name * age) > first).asFold)

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

  def getPeopleNameAndAge_1: Expr[People => List[(String, Int)]] =
    getAll(people > (name * age).asFold)

  def getPeopleNameAndAge_2: Expr[People => List[(String, Int)]] =
    getAll(people > (((name * age) * weight) > first).asFold)

  def getHer: Expr[Couples => List[Person]] =
    getAll(couples > her.asFold)

  def herNameAndStreet: Expr[Fold[Couples, (String, String)]] =
    couples > (her > name * (address > street)).asFold

  def herAndHimStreet_1: Expr[Fold[Couples, (String, String)]] =
    couples > (
      her * him > (first > address > street) * (second > address > street)).asFold

  def getHerName: Expr[Couples => List[String]] =
    getAll(couples > her.asFold > name.asFold)

  def getHerNameAndAge_1: Expr[Couples => List[(String, Int)]] =
    getAll(couples > her.asFold > (name * age).asFold)

  def getHerNameAndAge_2: Expr[Couples => List[(String, Int)]] =
    getAll(couples > ((her > name) * (her > age)).asFold)

  def getHerNameAndAge_3: Expr[Couples => List[(String, Int)]] =
    getAll(couples > (her > name * age).asFold)

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
  import OpticLang._

  implicit val semantic = new CoupleExample[Const[Semantic, ?]] {

    implicit val ev = OpticLang.semantic

    type Couple = Unit
    type Person = Unit

    val couples = {

      val oi = OpticInfo(
        KFold,
        "couples", 
        TypeInfo("Couples", false), 
        TypeInfo("Couple", true))

      Const(Semantic(TVar("c"), List("c" -> TOptic(oi))))
    }

    val her = {

      val oi = OpticInfo(
        KGetter,
        "her", 
        TypeInfo("Couple", true), 
        TypeInfo("Person", true))

      Const(Semantic(TVar("w"), List("w" -> TOptic(oi))))
    }

    val him = {

      val oi = OpticInfo( 
        KGetter,
        "him", 
        TypeInfo("Couple", true), 
        TypeInfo("Person", true))

      Const(Semantic(TVar("m"), List("m" -> TOptic(oi))))
    }

    val address = {

      val oi = OpticInfo( 
        KGetter,
        "address", 
        TypeInfo("Person", true), 
        TypeInfo("Address", true))

      Const(Semantic(TVar("a"), List("a" -> TOptic(oi))))
    }

    val people = {

      val oi = OpticInfo(
        KFold,
        "people", 
        TypeInfo("People", false), 
        TypeInfo("Person", true))

      Const(Semantic(TVar("p"), List("p" -> TOptic(oi))))
    }

    val name = {

      val oi = OpticInfo(
        KGetter,
        "name", 
        TypeInfo("Person", false), 
        TypeInfo("String", true))

      Const(Semantic(TOptic(oi)))
    }

    val age = {

      val oi = OpticInfo(
        KGetter,
        "age", 
        TypeInfo("Person", false), 
        TypeInfo("Int", true))

      Const(Semantic(TOptic(oi)))
    }

    val weight = {

      val oi = OpticInfo(
        KGetter,
        "weight", 
        TypeInfo("Person", false), 
        TypeInfo("Int", true))

      Const(Semantic(TOptic(oi)))
    }

    val street = {

      val oi = OpticInfo(
        KGetter,
        "street", 
        TypeInfo("Address", true), 
        TypeInfo("String", false))

      Const(Semantic(TOptic(oi)))
    }
  }
}

