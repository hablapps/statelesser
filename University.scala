package org.hablapps
package university

import scalaz._, Scalaz._

case class University(name: String, departs: Map[String, Department])
case class Department(budget: Double)

object UniversityManyDep extends App {

  trait UniversityRepo[P[_]] {
    def getName: P[String]
    def setName(name: String): P[Unit]
    type Dep
    def getDeps: P[List[Dep]]
    val deps: Dep => DepartmentRepo[P]
  }

  trait DepartmentRepo[P[_]] {
    def getBudget: P[Double]
    def setBudget(budget: Double): P[Unit]
  }

  def doubleBudget[P[_]: Monad](data: UniversityRepo[P]): P[Unit] = {
    import data._
    getDeps >>= (_.traverse_(d => deps(d) |>
      (alg => alg.getBudget >>= (b => alg.setBudget(b * 2)))))
  }

  val data = new UniversityRepo[State[University, ?]] {
    def getName = gets(_.name)
    def setName(name: String) = modify(_.copy(name = name))
    type Dep = String
    def getDeps = gets(_.departs.keys.toList)
    val deps = d => new DepartmentRepo[State[University, ?]] {
      def getBudget = gets(_.departs(d).budget) // XXX: unsafe!
      def setBudget(budget: Double) =
        modify(u => u.copy(departs = u.departs.updated(d, Department(budget))))
    }
  }

  val origin = University(
    "Rey Juan Carlos",
    Map("math" -> Department(10), "physics" -> Department(9)))
  println(s"Duplicating budget for: $origin")
  val result = doubleBudget(data).exec(origin)
  println(s"...results in: $result")
}

object UniversityManyDep2 extends App {

  trait UniversityRepo[P[_]] {
    def getName: P[String]
    def setName(name: String): P[Unit]
    type Dep
    val deps: DepartmentsRepo[P, Dep]
  }

  trait DepartmentsRepo[P[_], Dep] {
    def getAll: P[List[Dep]]
    def getBudget(d: Dep): P[Double]
    def setBudget(d: Dep)(budget: Double): P[Unit]
  }

  def doubleBudget[P[_]: Monad](data: UniversityRepo[P]): P[Unit] = {
    import data._, deps._
    getAll >>= (_.traverse_(d => getBudget(d) >>= (b => setBudget(d)(b * 2))))
  }

  val data = new UniversityRepo[State[University, ?]] {
    def getName = gets(_.name)
    def setName(name: String) = modify(_.copy(name = name))
    type Dep = String
    val deps = new DepartmentsRepo[State[University, ?], Dep] {
      def getAll = gets(_.departs.keys.toList)
      def getBudget(d: Dep) = gets(_.departs(d).budget) // XXX: unsafe!
      def setBudget(d: Dep)(budget: Double) =
        modify(u => u.copy(departs = u.departs.updated(d, Department(budget))))
    }
  }

  val origin = University(
    "Rey Juan Carlos",
    Map("math" -> Department(10), "physics" -> Department(9)))
  println(s"Duplicating budget for: $origin")
  val result = doubleBudget(data).exec(origin)
  println(s"...results in: $result")
}

object UniversityManyDep3 extends App {

  trait UniversityRepo[P[_]] {
    def getName: P[String]
    def setName(name: String): P[Unit]
    type Dep
    type Q[_]
    def getDeps: P[List[Dep]]
    val dep: DepartmentRepo[Q]
    val nat: Dep => Q ~> P
  }

  trait DepartmentRepo[Q[_]] {
    def getBudget: Q[Double]
    def setBudget(budget: Double): Q[Unit]
  }

  def doubleBudget[P[_]: Monad](
      data: UniversityRepo[P])(implicit
      MQ: Monad[data.Q]): P[Unit] = {
    import data._, dep._
    getDeps >>= (_.traverse_(d =>
      nat(d)(MQ.bind(getBudget)(b => setBudget(b * 2)))))
  }

  val data = new UniversityRepo[State[University, ?]] {
    def getName = gets(_.name)
    def setName(name: String) = modify(_.copy(name = name))
    type Dep = String
    type Q[x] = State[Department, x]
    def getDeps = gets(_.departs.keys.toList)
    val dep = new DepartmentRepo[Q] {
      def getBudget = gets(_.budget)
      def setBudget(budget: Double) = put(Department(budget))
    }
    val nat = d => λ[Q ~> State[University, ?]] { qx =>
      for {
        dep <- gets[University, Department](_.departs(d)) // XXX: unsafe!
        (dep2, out) = qx(dep)
        _ <- modify[University](u => u.copy(departs = u.departs.updated(d, dep2)))
      } yield out
    }
  }

  val origin = University(
    "Rey Juan Carlos",
    Map("math" -> Department(10), "physics" -> Department(9)))
  println(s"Duplicating budget for: $origin")
  val result = doubleBudget(data).exec(origin)
  println(s"...results in: $result")
}
