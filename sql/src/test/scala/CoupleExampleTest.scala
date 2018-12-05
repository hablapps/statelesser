package statelesser
package sql
package test

import scalaz.Const
import OpticLang.Semantic
import statelesser.test._
import org.scalatest._

class CoupleExampleTest extends FlatSpec with Matchers {

  import CoupleExample.semantic._
  import SQL._

  val keys = Map("Person" -> "name")

  def genSql[A](sem: Const[Semantic, A]): String =
    sqlToString(fromSemantic(sem.getConst, keys))

  def matchSql[A](q: String, sem: Const[Semantic, A]*) =
    sem.map(genSql) should contain theSameElementsAs List.fill(sem.length)(q)

  "Statelesser" should "generate wildcard '*' selection" in {
    matchSql("SELECT p.* FROM Person AS p;", getPeople)
  }

  it should "generate specific selection" in {
    matchSql("SELECT p.name FROM Person AS p;", getPeopleName)
  }

  it should "generate multi-selection" in {
    matchSql(
      "SELECT p.name, p.age FROM Person AS p;", 
      getPeopleNameAndAge_1, getPeopleNameAndAge_2)
  }

  it should "generate wildcard nested selection" in {
    matchSql(
      "SELECT w.* FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name;", 
      getHer)
  }

  it should "generate nested specific selection" in {
    matchSql(
      "SELECT w.name FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name;",
      getHerName)
  }

  it should "generate nested multi-selection" in {
    matchSql(
      "SELECT w.name, w.age FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name;",
      getHerNameAndAge_1, getHerNameAndAge_2, getHerNameAndAge_3)
  }

  it should "generate filters" in {
    matchSql(
      "SELECT p.name, p.age FROM Person AS p WHERE (p.age > 30);", 
      getPeopleGt30)
  }

  it should "generate filters for nested fields" in {
    matchSql(
      "SELECT w.name, w.age FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name WHERE (w.age > 30);",
      getHerGt30_1, getHerGt30_2)
  }

  it should "generate remove filtering fields from select" in {
    matchSql(
      "SELECT w.name FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name WHERE (w.age > 30);",
      getHerNameGt30_1, getHerNameGt30_2)
  }

  it should "generate complex queries" in {
    matchSql(
      "SELECT w.name, (w.age - m.age) FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name INNER JOIN Person AS m ON c.him = m.name WHERE ((w.age - m.age) > 0);",
      difference)

    matchSql(
      "SELECT w.name FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name INNER JOIN Person AS m ON c.him = m.name WHERE ((w.age - m.age) > 0);",
      differenceName_1, differenceName_2)
  }

  it should "normalise a stupid query" in {
    matchSql(
      "SELECT p.name, p.age FROM Person AS p WHERE ((p.age > 30) AND (p.age > 40));", 
      dummyNameAndAge)
  }
}
