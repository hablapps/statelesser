package statelesser
package test

import scalaz.Const

trait DepartmentExample[Expr[_]] {

  implicit val ev: OpticLang[Expr]

  /* data layer */

  type Org = List[Department]
  type Department
  type Employee
  type Task

  val org: Expr[Fold[Org, Department]]
  val dpt: Expr[Getter[Department, String]]
  val employees: Expr[Fold[Department, Employee]]
  val emp: Expr[Getter[Employee, String]]
  val tasks: Expr[Fold[Employee, Task]]
  val tsk: Expr[Getter[Task, String]]

  /* logic */

  import ev._
  import OpticLang.syntax._

  def getOrgEmployees: Expr[Org => List[String]] =
    getAll(org > employees > emp.asFold)

  def getOrgEmployeesAndDpt: Expr[Org => List[(String, String)]] = 
    getAll(org > (dpt.asFold * (employees > emp.asFold)))

  def getAbstractEmployees: Expr[Org => List[String]] =
    getAll(org > employees
      > filtered(contains(tasks > tsk.asFold, "abstract")).asFold
      > emp.asFold)

  def expertise: Expr[Org => List[String]] =
    getAll(org 
      > filtered(all(employees, contains(tasks > tsk.asFold, "abstract"))).asFold 
      > dpt.asFold)
}

object DepartmentExample {
  import OpticLang._

  implicit val semantic = new DepartmentExample[Const[Semantic, ?]] {
    
    implicit val ev = OpticLang.semantic

    type Department = Unit
    type Employee = Unit
    type Task = Unit

    val org = {

      val oi = OpticInfo(
        KFold,
        "org", 
        TypeInfo("Org", false), 
        TypeInfo("Department", true))

      Const(Semantic(Map(Var("d") -> GLabel(oi)), List(Var("d"))))
    }

    val dpt = {

      val oi = OpticInfo(
        KGetter,
        "dpt", 
        TypeInfo("Department", true), 
        TypeInfo("String", false))

      Const(Semantic(select = List(GLabel(oi))))
    }

    val employees = {

      val oi = OpticInfo(
        KFold,
        "employees", 
        TypeInfo("Department", true), 
        TypeInfo("Employee", true))

      Const(Semantic(Map(Var("e") -> GLabel(oi)), List(Var("e"))))
    }

    val emp = {

      val oi = OpticInfo(
        KGetter,
        "emp", 
        TypeInfo("Employee", true), 
        TypeInfo("String", false))

      Const(Semantic(select = List(GLabel(oi))))
    }

    val tasks = {

      val oi = OpticInfo(
        KFold,
        "tasks", 
        TypeInfo("Employee", true), 
        TypeInfo("Task", true))

      Const(Semantic(Map(Var("t") -> GLabel(oi)), List(Var("t"))))
    }

    val tsk = {

      val oi = OpticInfo(
        KGetter,
        "tsk", 
        TypeInfo("Task", true), 
        TypeInfo("String", false))

      Const(Semantic(select = List(GLabel(oi))))
    }
  }
}

