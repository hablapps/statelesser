package statelesser
package sql
package test

import scala.util.matching.Regex
import scalaz._, Scalaz._
import OpticLang.{Semantic, Table}
import statelesser.test._
import org.scalatest._

class CoupleExampleTest extends FlatSpec with Matchers {

  import CoupleExample._, instance._
  import SQL._

  val keys = Map("Person" -> "name")

  def syms(pattern: Stream[Char], i: Int): Stream[String] = 
    pattern.zipWithL(Stream.continually(i)) { (c, oi) => 
      s"""$c${oi.getOrElse("")}""" 
    } #::: syms(pattern, i + 1)

  def genSql[S, A](sem: Semantic[Const[String, ?], Fold[S, A]]): String = {
    // println(sem)
    val (t, out) = sem(Table(Stream.range('a', 'z').map(_.toString), Map()))
    sqlToString(fromSemantic(t, out, keys))
  }

  def matchSql[S, A](r: Regex, stks: Stack[Fold[S, A]]*) =
    stks.foreach { stk => 
      genSql(stk) should fullyMatch regex r
    } 

  "Statelesser" should "generate wildcard '*' selection" in {
    matchSql(raw"SELECT (.)\.\* FROM Person AS \1;".r, getPeople)
  }

  it should "generate specific selection" in {
    matchSql(raw"SELECT (.)\.name FROM Person AS \1;".r, getPeopleName_1)
  }

  it should "generate multi-selection" in {
    matchSql(
      raw"SELECT (.)\.name, \1\.age FROM Person AS \1;".r, 
      getPeopleNameAndAge_1,
      getPeopleNameAndAge_2)
  }

  it should "generate wildcard nested selection" in {
    matchSql(
      raw"SELECT (.)\.\* FROM Couple AS (.) INNER JOIN Person AS \1 ON \2\.her = \1\.name;".r, 
      getHer)
  }

  it should "generate nested specific selection" in {
    matchSql(
      raw"SELECT (.)\.name FROM Couple AS (.) INNER JOIN Person AS \1 ON \2\.her = \1\.name;".r,
      getHerName)
  }

  it should "generate nested multi-selection" in {
    matchSql(
      raw"SELECT (.)\.name, \1\.age FROM Couple AS (.) INNER JOIN Person AS \1 ON \2\.her = \1\.name;".r,
      getHerNameAndAge_1, 
      getHerNameAndAge_2, 
      getHerNameAndAge_3)
  }

  it should "generate multi-selection with literals" in {
    matchSql(
      raw"SELECT (.)\.name, 3 FROM Person AS \1;".r,
      getPeopleNameAnd3_1,
      getPeopleNameAnd3_2,
      getPeopleNameAnd3_3,
      getPeopleNameAnd3_4)
  }

  // it should "generate filters" in {
  //   matchSql(
  //     "SELECT p.name, p.age FROM Person AS p WHERE (p.age > 30);", 
  //     getPeopleGt30)
  // }

  // it should "generate filters for nested fields" in {
  //   matchSql(
  //     "SELECT w.name, w.age FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name WHERE (w.age > 30);",
  //     getHerGt30_1, getHerGt30_2)
  // }

  // it should "generate remove filtering fields from select" in {
  //   matchSql(
  //     "SELECT w.name FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name WHERE (w.age > 30);",
  //     getHerNameGt30_1, getHerNameGt30_2)
  // }

  // it should "generate complex queries" in {
  //   matchSql(
  //     "SELECT w.name, (w.age - m.age) FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name INNER JOIN Person AS m ON c.him = m.name WHERE ((w.age - m.age) > 0);",
  //     difference)

  //   matchSql(
  //     "SELECT w.name FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name INNER JOIN Person AS m ON c.him = m.name WHERE ((w.age - m.age) > 0);",
  //     differenceName_1, differenceName_2)
  // }

  // it should "normalise a stupid query" in {
  //   matchSql(
  //     "SELECT p.name, p.age FROM Person AS p WHERE ((p.age > 30) AND (p.age > 40));", 
  //     dummyNameAndAge)
  // }
}

