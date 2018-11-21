package org.hablapps.statelesser

object Sql {

  type Table = String
  type Var = String
  type FieldName = String

  case class SSelect(select: SqlSelect, from: SqlFrom, where: Option[SqlExp]) {
    override def toString: String = sqlToString(this)
  }

  sealed abstract class SqlFrom
  case class SFrom(ts: List[SqlTable]) extends SqlFrom

  sealed abstract class SqlTable
  case class STable(t: Table, v: Var, js: List[SqlJoin]) extends SqlTable

  sealed abstract class SqlJoin
  case class SJoin(t: Table, v: Var) extends SqlJoin
  case class SEqJoin(t: Table, v: Var, cond: SqlEqJoinCond) extends SqlJoin

  sealed abstract class SqlEqJoinCond
  case class SOn(l: SProj, r: SProj) extends SqlEqJoinCond
  case class SUsing(fn: FieldName) extends SqlEqJoinCond

  sealed abstract class SqlSelect
  case class SList(es: List[SField]) extends SqlSelect
  case class SAll(e: String) extends SqlSelect

  sealed abstract class SqlExp
  case class SField(e: SqlExp, fn: FieldName) extends SqlExp
  case class SProj(v: Var, fn: FieldName) extends SqlExp
  case class SBinOp(op: String, l: SqlExp, r: SqlExp) extends SqlExp
  case class SUnOp(op: String, e: SqlExp) extends SqlExp

  private def sqlToString(sql: SSelect): String = {
    val sel = selToString(sql.select)
    val frm = frmToString(sql.from)
    val whr = sql.where.fold("")(e => s" WHERE ${expToString(e)}")
    s"SELECT $sel FROM $frm$whr;"
  }

  private def selToString(sel: SqlSelect): String = sel match {
    case SList(es) => es.map(expToString).mkString(", ")
    case SAll(e) => s"$e.*"
  }

  private def frmToString(frm: SqlFrom): String = frm match {
    case SFrom(ts) => ts.map(tabToString).mkString(", ")
  }

  private def tabToString(tab: SqlTable): String = tab match {
    case STable(t, v, js) => {
      val s = (if (js.nonEmpty) " " else "") ++ 
        js.map(joinToString).mkString(" ")
      s"$t AS $v$s"
    }
  }

  private def joinToString(join: SqlJoin): String = join match {
    case SJoin(t, v) => s"JOIN $t AS $v"
    case SEqJoin(t, v, c) => s"INNER JOIN $t AS $v ${joinCondToString(c)}"
  }

  private def joinCondToString(cond: SqlEqJoinCond): String = cond match {
    case SOn(l, r) => s"ON ${expToString(l)} = ${expToString(r)}"
    case SUsing(fn) => s"USING ($fn)"
  }

  private def expToString(exp: SqlExp): String = exp match {
    case SField(e, "") => s"${expToString(e)}"
    case SField(e, fn) => s"${expToString(e)} AS $fn"
    case SProj(v, fn) => s"$v.$fn"
    case SBinOp(op, l, r) => s"${expToString(l)} $op ${expToString(r)}"
    case SUnOp(op, e) => s"$op(${expToString(e)})"
  }
}

