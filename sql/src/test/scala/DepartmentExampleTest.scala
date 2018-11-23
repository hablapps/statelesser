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

  val keys = Map("Department" -> "dpt", "Employee" -> "emp")

  def genSql[A](sem: Const[Semantic, A]): String =
    sqlToString(fromSemantic(sem.getConst, keys))

  def matchSql[A](q: String, sem: Const[Semantic, A]*) =
    sem.map(genSql) should contain theSameElementsAs List.fill(sem.length)(q)

  "Statelesser" should "query all employees in the organization" in {
    matchSql(
      "SELECT e.emp FROM Department AS d INNER JOIN Employee AS e USING (dpt);", 
      getOrgEmployees)
  }

  it should "query all employees along with their department name" in {
    matchSql(
      "SELECT d.dpt, e.emp FROM Department AS d INNER JOIN Employee AS e USING (dpt);",
      getOrgEmployeesAndDpt)
  }

  it should "query containing subqueries" in {
    matchSql(
      "SELECT e.emp FROM Department AS d INNER JOIN Employee AS e USING (dpt) WHERE EXISTS(SELECT t.tsk FROM Task AS t WHERE (t.tsk = \"abstract\") AND (t.dpt = e.dpt));", 
      getAbstractEmployees)
  }

  it should "query containing subsubqueries" in {
    matchSql(
      "SELECT d.dpt FROM Department AS d WHERE NOT (EXISTS (SELECT e.* FROM Employee AS e WHERE (e.dpt = d.dpt) AND NOT (EXISTS (SELECT t.tsk FROM Task AS t WHERE (t.tsk = \"abstract\") AND (t.emp = e.emp)))));",
      expertise)
  }
}

