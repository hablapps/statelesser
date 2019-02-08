package statelesser
package xpath
package test

import scala.util.matching.Regex
import org.scalatest._

import scalaz.Const
import monocle._
import core.test._
import xpath._

class CoupleExampleTest extends FlatSpec with Matchers {

  import CoupleExample._, xpathInstance._

  def matchXPath[S, A](r: Regex, paths: Const[Path, Fold[S, A]]*) =
    paths.foreach { p =>
      p.getConst.toString should fullyMatch regex r
    }

  "Statelesser" should "generate wildcard '*' selection" in {
    matchXPath(raw"people".r, getPeople)
  }

  it should "generate specific selection" in {
    matchXPath(raw"people/\(@name\)".r, getPeopleName_1)
  }

  it should "generate multi-selection" in {
    matchXPath(
      raw"people/\(@name \| @age\)".r,
      getPeopleNameAndAge_1,
      getPeopleNameAndAge_2)
  }

  it should "generate wildcard nested selection" in {
    matchXPath(raw"couples/\(her\)".r, getHer)
  }

  it should "generate multiple wildcard nested selection" in {
    matchXPath(raw"couples/\(her \| him\)".r, getHerAndHim)
  }

  it should "generate nested specific selection" in {
    matchXPath(raw"couples/\(her\)/\(@name\)".r, getHerName)
  }

  it should "generate nested multi-selection" in {
    matchXPath(
      raw"couples/\(her\)/\(@name \| @age\)".r,
      getHerNameAndAge_1,
      getHerNameAndAge_2,
      getHerNameAndAge_3)
  }

  it should "generate multi-selection with literals" ignore {
    matchXPath(
      raw"THIS SHOULD RAISE AN ERROR".r,
      getPeopleNameAnd3_1,
      getPeopleNameAnd3_2,
      getPeopleNameAnd3_3,
      getPeopleNameAnd3_4)
  }

  it should "generate multi-selection with multi-valued fields" in {
    matchXPath(
      raw"couples/\(for \$$a in \(her/\(@name\)\), \$$b in \(him/\(aliases\)\) return \(\$$a \| \$$b\)\)".r,
      getHerNameAndHimAliases_1)
  }

  it should "generate filters" in {
    matchXPath(
      raw"people[@age > 30]/(@name | @age)".r, 
      //getPeopleGt30_1, 
      getPeopleGt30_2)
  }

  // it should "generate filters for nested fields" in {
  //   matchSql(
  //     raw"SELECT (.+)\.name, \1\.age FROM Couple AS (.+) INNER JOIN Person AS \1 ON \2\.her = \1\.name WHERE \(\1\.age > 30\);".r,
  //     getHerGt30_1,
  //     getHerGt30_2)
  // }

  // it should "generate remove filtering fields from select" in {
  //   matchSql(
  //     raw"SELECT (.+)\.name FROM Couple AS (.+) INNER JOIN Person AS \1 ON \2\.her = \1\.name WHERE \(\1\.age > 30\);".r,
  //     getHerNameGt30_1,
  //     getHerNameGt30_2)
  // }

  // it should "generate complex queries" in {
  //   matchSql(
  //     raw"SELECT (.+)\.name, \(\1\.age - (.+)\.age\) FROM Couple AS (.+) INNER JOIN Person AS \1 ON \3\.her = \1\.name INNER JOIN Person AS \2 ON \3\.him = \2\.name WHERE \(\(\1\.age - \2\.age\) > 0\);".r,
  //     difference)
  //     //difference_1)

  //   matchSql(
  //     raw"SELECT (.+)\.name FROM Couple AS (.+) INNER JOIN Person AS \1 ON \2\.her = \1\.name INNER JOIN Person AS (.+) ON \2\.him = \3\.name WHERE \(\(\1\.age - \3\.age\) > 0\);".r,
  //     differenceName_1,
  //     differenceName_2)
  // }

  // it should "normalise a stupid query" in {
  //   matchSql(
  //     raw"SELECT (.+)\.name, \1\.age FROM Person AS \1 WHERE \(\(\1\.age > 30\) AND \(\1\.age > 40\)\);".r,
  //     dummyNameAndAge)
  // }

  // it should "normalise a deeply nested query" in {
  //   matchSql(
  //     raw"SELECT (.+)\.street, (.+)\.street FROM Couple AS (.+) INNER JOIN Person AS (.+) ON \3\.her = \4\.name INNER JOIN Address AS \1 ON \4\.address = \1\.id INNER JOIN Person AS (.+) ON \3\.him = \5\.name INNER JOIN Address AS \2 ON \5\.address = \2.id;".r, 
  //     herAndHimStreet_1)
  // }

  // it should "generate cartesian product when achieving fold products" in {
  //   matchSql(raw"".r, /*getHerCartesianAliases_1,*/ getHerCartesianAliases_2)
  // }

  // it should "generate cartesian product in the root, aka. JOIN" in {
  //   matchSql(raw"".r, difference_1)
  // }
}

