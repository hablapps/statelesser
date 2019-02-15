package statelesser
package core
package test

import monocle._

trait DepartmentExample[Expr[_]] {

  implicit val ev: Statelesser[Expr]

  type Organization
  type Department
  type Employee
  type Task = String

  def departments: Expr[Fold[Organization, Department]]
  def dpt: Expr[Getter[Department, String]]
  def employees: Expr[Fold[Department, Employee]]
  def emp: Expr[Getter[Employee, String]]
  def tasks: Expr[Fold[Employee, Task]]

  /* logic */

  import ev._
  import Statelesser.syntax._

  def expertise(tsk: Task): Expr[Fold[Organization, String]] =
    departments > 
      filtered(allOf(employees)(elemOf(tasks)(tsk))).asFold > 
      dpt.asFold
}

