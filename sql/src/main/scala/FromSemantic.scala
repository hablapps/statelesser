package statelesser
package sql

import OpticLang._

trait FromSemantic {

  import OpticLang.Var

  def fromSemantic(
      sem: Semantic, 
      keys: Map[TypeNme, FieldName] = Map()): SSelect = 
    SSelect(
      selToSql(sem.select, keys), 
      tabToSql(sem.table, keys), 
      whrToSql(sem.where, keys))

  private def selToSql(
      sel: List[Tree],
      keys: Map[TypeNme, FieldName]): SqlSelect = sel match {
    case Var(e) :: Nil => SAll(e)
    case xs => SList(xs.map(e => SField(treeToExpr(e, keys), "")))
  }

  private def tabToSql(
      tab: Map[Var, Tree], 
      keys: Map[TypeNme, FieldName]): SqlFrom = 
    SFrom(List(tab.toList match {
      case (Var(nme), GLabel(info)) :: xs => 
        STable(info.tgt.nme, nme, xs.map(joinToSql(_, keys)))
      case _ => throw new Error(s"No table was selected for FROM clause")
    }))

  private def condToSql(
      v1: Var, n1: FieldName, 
      v2: Var, n2: FieldName): SqlEqJoinCond =
    if (n1 == n2) SUsing(n1) else SOn(SProj(v1.nme, n1), SProj(v2.nme, n2))

  private def joinToSql(
      vt: (OpticLang.Var, Tree),
      keys: Map[TypeNme, FieldName]): SqlJoin = vt match {
    case (v1, VL(v2, GLabel(inf@OpticInfo(KGetter | KLens, _, _, _)))) => {
      val cond = condToSql(v2, inf.nme, v1, keys(inf.tgt.nme)) // XXX: unsafe
      SEqJoin(s"${inf.tgt.nme}", v1.nme, cond)
    }
    case (Var(n1), VL(_, GLabel(inf))) => {
      SEqJoin(s"${inf.tgt.nme}", n1, SUsing(keys(inf.src.nme))) // XXX: unsafe
    }
    case _ => throw new Error(s"Don't know how to generate join for '$vt'")
  }

  private def whrToSql(
      whr: Set[Tree],
      keys: Map[TypeNme, FieldName]): Option[SqlExp] =
    whr.foldLeft(Option.empty[SqlExp]) {
      case (None, t) => Some(treeToExpr(t, keys))
      case (Some(e), t) => Some(SBinOp("AND", e, treeToExpr(t, keys)))
    }

  private def treeToExpr(
      t: Tree, 
      keys: Map[TypeNme, FieldName]): SqlExp = t match {
    case VL(Var(nme), GLabel(info)) => SProj(nme, info.nme)
    case Op(op, l, r) => SBinOp(op, treeToExpr(l, keys), treeToExpr(r, keys))
    case Unary(t, op) => SUnOp(op, treeToExpr(t, keys))
    case Val(x) => SCons(x)
    case Sub(sem) => SExists(fromSemantic(sem, keys))
    case _ => throw new Error(s"Don't know how to translate '$t' into SQL")
  }
}

