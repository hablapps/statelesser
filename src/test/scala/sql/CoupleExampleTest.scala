package statelesser
package sql
package test

import scala.util.matching.Regex
import org.scalatest._

import monocle._
import core.test._
import sqlnormal._

class CoupleExampleTest extends FlatSpec with Matchers {

  import CoupleExample._, instance._

  val keys = Map("Person" -> "name", "Address" -> "id")

  def genSql[S, A](sem: Semantic[Fold[S, A]]): String =
    SSelect.toString(TSemantic.toSql(sem, keys))

  def matchSql[S, A](r: Regex, stks: Semantic[Fold[S, A]]*) =
    stks.foreach { stk =>
      genSql(stk) should fullyMatch regex r
    }

  "Statelesser" should "generate wildcard '*' selection" in {
    matchSql(raw"SELECT (.+)\.\* FROM Person AS \1;".r, getPeople)
  }

  it should "generate specific selection" in {
    matchSql(raw"SELECT (.+)\.name FROM Person AS \1;".r, getPeopleName_1)
  }

  it should "generate multi-selection" in {
    matchSql(
      raw"SELECT (.+)\.name, \1\.age FROM Person AS \1;".r,
      getPeopleNameAndAge_1,
      getPeopleNameAndAge_2)
  }

  it should "generate wildcard nested selection" in {
    matchSql(
      raw"SELECT (.+)\.\* FROM Couple AS (.+) INNER JOIN Person AS \1 ON \2\.her = \1\.name;".r,
      getHer)
  }

  it should "generate multiple wildcard nested selection" in {
    matchSql(
      raw"SELECT (.+)\.\*, (.+)\.\* FROM Couple AS (.+) INNER JOIN Person AS \1 ON \3\.her = \1\.name INNER JOIN Person AS \2 ON \3\.him = \2\.name;".r,
      getHerAndHim)
  }

  it should "generate nested specific selection" in {
    matchSql(
      raw"SELECT (.+)\.name FROM Couple AS (.+) INNER JOIN Person AS \1 ON \2\.her = \1\.name;".r,
      getHerName)
  }

  it should "generate nested multi-selection" in {
    matchSql(
      raw"SELECT (.+)\.name, \1\.age FROM Couple AS (.+) INNER JOIN Person AS \1 ON \2\.her = \1\.name;".r,
      getHerNameAndAge_1,
      getHerNameAndAge_2,
      getHerNameAndAge_3)
  }

  it should "generate multi-selection with literals" in {
    matchSql(
      raw"SELECT (.+)\.name, 3 FROM Person AS \1;".r,
      getPeopleNameAnd3_1,
      getPeopleNameAnd3_2,
      getPeopleNameAnd3_3,
      getPeopleNameAnd3_4)
  }

  it should "generate multi-selection with multi-valued fields" ignore {
    matchSql(
      raw"".r,
      getHerNameAndHimAliases_1)
  }

  it should "generate filters" in {
    matchSql(
      raw"SELECT (.+)\.name, \1\.age FROM Person AS \1 WHERE \(\1\.age > 30\);".r,
      getPeopleGt30)
  }

  it should "generate filters for nested fields" in {
    matchSql(
      raw"SELECT (.+)\.name, \1\.age FROM Couple AS (.+) INNER JOIN Person AS \1 ON \2\.her = \1\.name WHERE \(\1\.age > 30\);".r,
      getHerGt30_1,
      getHerGt30_2)
  }

  it should "generate remove filtering fields from select" in {
    matchSql(
      raw"SELECT (.+)\.name FROM Couple AS (.+) INNER JOIN Person AS \1 ON \2\.her = \1\.name WHERE \(\1\.age > 30\);".r,
      getHerNameGt30_1,
      getHerNameGt30_2)
  }

  it should "generate complex queries" in {
    matchSql(
      raw"SELECT (.+)\.name, \(\1\.age - (.+)\.age\) FROM Couple AS (.+) INNER JOIN Person AS \1 ON \3\.her = \1\.name INNER JOIN Person AS \2 ON \3\.him = \2\.name WHERE \(\(\1\.age - \2\.age\) > 0\);".r,
      difference)
      //difference_1)

    matchSql(
      raw"SELECT (.+)\.name FROM Couple AS (.+) INNER JOIN Person AS \1 ON \2\.her = \1\.name INNER JOIN Person AS (.+) ON \2\.him = \3\.name WHERE \(\(\1\.age - \3\.age\) > 0\);".r,
      differenceName_1,
      differenceName_2)
  }

  it should "normalise a stupid query" in {
    matchSql(
      raw"SELECT (.+)\.name, \1\.age FROM Person AS \1 WHERE \(\(\1\.age > 30\) AND \(\1\.age > 40\)\);".r,
      dummyNameAndAge)
  }

  it should "normalise a deeply nested query" in {
    matchSql(
      raw"SELECT (.+)\.street, (.+)\.street FROM Couple AS (.+) INNER JOIN Person AS (.+) ON \3\.her = \4\.name INNER JOIN Address AS \1 ON \4\.address = \1\.id INNER JOIN Person AS (.+) ON \3\.him = \5\.name INNER JOIN Address AS \2 ON \5\.address = \2.id;".r, 
      herAndHimStreet_1)
  }

  it should "generate cartesian product when achieving fold products" in {
    matchSql(raw"".r, getHerCartesianAliases)
  }

  it should "generate cartesian product in the root, aka. JOIN" in {
    matchSql(raw"".r, difference_1)
  }
}

