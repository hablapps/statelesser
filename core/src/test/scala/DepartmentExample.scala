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

  val departments: Expr[Fold[Org, Department]]
  val dpt: Expr[Getter[Department, String]]
  val employees: Expr[Fold[Department, Employee]]
  val emp: Expr[Getter[Employee, String]]
  val tasks: Expr[Fold[Employee, Task]]
  val tsk: Expr[Getter[Task, String]]

  /* logic */

  import ev._
  import OpticLang.syntax._

  def getOrgEmployees: Expr[Org => List[Employee]] =
    getAll(departments > employees)
}

object DepartmentExample {
  import OpticLang._

  implicit val semantic = new DepartmentExample[Const[Semantic, ?]] {
    
    implicit val ev = OpticLang.semantic

    type Department = Unit
    type Employee = Unit
    type Task = Unit

    val departments = {

      val oi = OpticInfo(
        "departments", 
        TypeInfo("Org", false), 
        TypeInfo("Department", true))

      Const(Semantic(Map(Var("d") -> GLabel(oi)), List(Var("d"))))
    }

    val dpt = {

      val oi = OpticInfo(
        "dpt", 
        TypeInfo("Department", true), 
        TypeInfo("String", false))

      Const(Semantic(select = List(GLabel(oi))))
    }

    val employees = {

      val oi = OpticInfo(
        "employees", 
        TypeInfo("Department", true), 
        TypeInfo("Employee", true))

      Const(Semantic(Map(Var("e") -> GLabel(oi)), List(Var("e"))))
    }

    val emp = {

      val oi = OpticInfo(
        "emp", 
        TypeInfo("Employee", true), 
        TypeInfo("String", false))

      Const(Semantic(select = List(GLabel(oi))))
    }

    val tasks = {

      val oi = OpticInfo(
        "tasks", 
        TypeInfo("Employee", true), 
        TypeInfo("Task", true))

      Const(Semantic(Map(Var("t") -> GLabel(oi)), List(Var("t"))))
    }

    val tsk = {

      val oi = OpticInfo(
        "tsk", 
        TypeInfo("Task", true), 
        TypeInfo("String", false))

      Const(Semantic(select = List(GLabel(oi))))
    }
  }
}

