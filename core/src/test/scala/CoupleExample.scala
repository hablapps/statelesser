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

  val couples: Expr[Fold[Couples, Couple]]
  val her: Expr[Getter[Couple, Person]]
  val him: Expr[Getter[Couple, Person]]
  val people: Expr[Fold[People, Person]]
  val name: Expr[Getter[Person, String]]
  val age: Expr[Getter[Person, Int]]
  val weight: Expr[Getter[Person, Int]]

  /* logic */

  import ev._
  import OpticLang.syntax._

  def getPeople: Expr[People => List[Person]] =
    getAll(people)

  def getPeopleName_1: Expr[People => List[String]] =
    getAll(people > name.asFold)

  def getPeopleName_2: Expr[People => List[String]] =
    getAll(people > ((name * age) > first).asFold)

  def getPeopleName_3: Expr[People => List[String]] =
    getAll(people > (((name * age) * weight > first) > first).asFold)

  def getPeopleName_4: Expr[People => List[String]] =
    getAll(people > (((name * age) * weight > (first > first))).asFold)

  def getPeopleName_5: Expr[People => List[String]] =
    getAll(people > (name * (age * weight) > first).asFold)

  def getPeopleNameAndAge_1: Expr[People => List[(String, Int)]] =
    getAll(people > (name * age).asFold)

  def getPeopleNameAndAge_2: Expr[People => List[(String, Int)]] =
    getAll(people > (((name * age) * weight) > first).asFold)

  def getHer: Expr[Couples => List[Person]] =
    getAll(couples > her.asFold)

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
        TypeInfo("Couple", false), 
        TypeInfo("Person", true))

      Const(Semantic(TVar("w"), List("w" -> TOptic(oi))))
    }

    val him = {

      val oi = OpticInfo( 
        KGetter,
        "him", 
        TypeInfo("Couple", false), 
        TypeInfo("Person", true))

      Const(Semantic(TVar("m"), List("m" -> TOptic(oi))))
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
        TypeInfo("Person", true))

      Const(Semantic(TOptic(oi)))
    }

    val age = {

      val oi = OpticInfo(
        KGetter,
        "age", 
        TypeInfo("Person", false), 
        TypeInfo("Person", true))

      Const(Semantic(TOptic(oi)))
    }

    val weight = {

      val oi = OpticInfo(
        KGetter,
        "weight", 
        TypeInfo("Person", false), 
        TypeInfo("Person", true))

      Const(Semantic(TOptic(oi)))
    }
  }
}
