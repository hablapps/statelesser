package statelesser
package sql
package test

import scalaz.Const
import OpticLang.Semantic
import statelesser.test._
import org.scalatest._

class DepartmentExampleTest extends FlatSpec with Matchers {

  import DepartmentExample.semantic._
  import SQL._

  def genSql[A](sem: Const[Semantic, A]): String =
    sqlToString(fromSemantic(sem.getConst))

  def matchSql[A](q: String, sem: Const[Semantic, A]*) =
    sem.map(genSql) should contain theSameElementsAs List.fill(sem.length)(q)

  "Statelesser" should "query all employees in the organization" in {
    matchSql("SELECT e.* FROM Department AS d INNER JOIN Employee AS e ON d.employees = e.id;", 
      getOrgEmployees)
  }
}

