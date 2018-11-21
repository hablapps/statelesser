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

  def genSql[A](sem: Const[Semantic, A]): String =
    sqlToString(fromSemantic(sem.getConst))

  def matchSql[A](q: String, sem: Const[Semantic, A]*) =
    sem.map(genSql) should contain theSameElementsAs List.fill(sem.length)(q)

  "Statelesser" should "generate wildcard '*' selection" in {
    matchSql("SELECT p.* FROM Person AS p;", getPeople)
  }

  it should "generate specific selection" in {
    matchSql("SELECT p.name FROM Person AS p;", getPeopleName)
  }

  it should "generate multi-selection" in {
    matchSql("SELECT p.name, p.age FROM Person AS p;", getPeopleNameAndAge)
  }

  it should "generate wildcard nested selection" in {
    matchSql(
      "SELECT w.* FROM Couple AS c INNER JOIN Person AS w ON c.her = w.id;", 
      getHer)
  }

  it should "generate nested specific selection" in {
    matchSql(
      "SELECT w.name FROM Couple AS c INNER JOIN Person AS w ON c.her = w.id;",
      getHerName)
  }

  it should "generate nested multi-selection" in {
    matchSql(
      "SELECT w.name, w.age FROM Couple AS c INNER JOIN Person AS w ON c.her = w.id;",
      getHerNameAndAge_1, getHerNameAndAge_2, getHerNameAndAge_3)
  }

  it should "generate filters" in {
    matchSql(
      "SELECT p.name, p.age FROM Person AS p WHERE (>30)(p.age);", 
      getPeopleGt30)
  }

  it should "generate filters for nested fields" in {
    matchSql(
      "SELECT w.name, w.age FROM Couple AS c INNER JOIN Person AS w ON c.her = w.id WHERE (>30)(w.age);",
      getHerGt30_1, getHerGt30_2)
  }

  it should "generate remove filtering fields from select" in {
    matchSql(
      "SELECT w.name FROM Couple AS c INNER JOIN Person AS w ON c.her = w.id WHERE (>30)(w.age);",
      getHerNameGt30_1, getHerNameGt30_2)
  }

  it should "generate complex queries" in {
    matchSql(
      "SELECT w.name, w.age - m.age FROM Couple AS c INNER JOIN Person AS w ON c.her = w.id INNER JOIN Person AS m ON c.him = m.id WHERE (>0)(w.age - m.age);",
      difference)

    matchSql(
      "SELECT w.name FROM Couple AS c INNER JOIN Person AS w ON c.her = w.id INNER JOIN Person AS m ON c.him = m.id WHERE (>0)(w.age - m.age);",
      differenceName_1, differenceName_2)
  }

  it should "normalise a stupid query" in {
    matchSql(
      "SELECT p.name, p.age FROM Person AS p WHERE (>30)(p.age) AND (>40)(p.age);", 
      dummyNameAndAge)
  }
}

