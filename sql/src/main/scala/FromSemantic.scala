package statelesser
package sql

import scalaz._, Scalaz._

import OpticLang._

trait FromSemantic {

  def fromSemantic(
      sem: Semantic, 
      keys: Map[TypeNme, FieldName] = Map()): SSelect = 
    SSelect(
      selToSql(sem.pointer, keys), 
      tabToSql(sem.symbols, keys), 
      whrToSql(sem.filters, keys))

  private def flatProduct(x: TTree): List[TTree] = x match {
    case TBinary("horizontal", l, r) => List(flatProduct(l), flatProduct(r)).join
    case _ => List(x)
  }

  private def selToSql(
      sel: TTree,
      keys: Map[TypeNme, FieldName]): SqlSelect = sel match {
    case TVar(e) => SAll(e)
    case t => SList(flatProduct(t).map(e => SField(treeToExpr(e, keys), "")))
  }

  private def tabToSql(
      tab: List[(String, TTree)], 
      keys: Map[TypeNme, FieldName]): SqlFrom = 
    SFrom(List(tab.toList match {
      case (nme, TOptic(info)) :: xs => 
        STable(info.tgt.nme, nme, xs.map(joinToSql(_, keys)))
      case _ => throw new Error(s"No table was selected for FROM clause")
    }))

  private def condToSql(
      v1: String, n1: FieldName, 
      v2: String, n2: FieldName): SqlEqJoinCond =
    if (n1 == n2) SUsing(n1) else SOn(SProj(v1, n1), SProj(v2, n2))

  private def joinToSql(
      vt: (String, TTree),
      keys: Map[TypeNme, FieldName]): SqlJoin = vt match {
    case (v1, TProj(v2: TVar, TOptic(inf@OpticInfo(KGetter | KLens, _, _, _)))) => {
      val cond = condToSql(v2.nme, inf.nme, v1, keys(inf.tgt.nme))
      SEqJoin(s"${inf.tgt.nme}", v1, cond)
    }
    case (n1, TProj(_, TOptic(inf))) => {
      SEqJoin(s"${inf.tgt.nme}", n1, SUsing(keys(inf.src.nme)))
    }
    case _ => throw new Error(s"Don't know how to generate join for '$vt'")
  }

  private def whrToSql(
      whr: Set[TTree],
      keys: Map[TypeNme, FieldName]): Option[SqlExp] =
    whr.foldLeft(Option.empty[SqlExp]) {
      case (None, t) => Some(treeToExpr(t, keys))
      case (Some(e), t) => Some(SBinOp("AND", e, treeToExpr(t, keys)))
    }

  private def treeToExpr(
      t: TTree, 
      keys: Map[TypeNme, FieldName]): SqlExp = t match {
    case TOptic(info) => SProj("", info.nme)
    case TProj(TVar(nme), TOptic(info)) => SProj(nme, info.nme)
    case TBinary(op, l, r) => 
      SBinOp(op, treeToExpr(l, keys), treeToExpr(r, keys))
    case TUnary(op, t) => SUnOp(op, treeToExpr(t, keys))
    case TLiteral(x) => SCons(x)
    case _ => throw new Error(s"Don't know how to translate '$t' into SQL")
  }
}

