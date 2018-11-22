package statelesser
package sql

import OpticLang._

trait FromSemantic {

  import OpticLang.Var

  def fromSemantic(
      sem: Semantic, 
      keys: Map[TypeNme, FieldName] = Map()): SSelect = 
    SSelect(selToSql(sem.select), tabToSql(sem.table, keys), whrToSql(sem.where))

  private def selToSql(sel: List[Tree]): SqlSelect = sel match {
    case Var(e) :: Nil => SAll(e)
    case xs => SList(xs.map(e => SField(treeToExpr(e), "")))
  }

  private def tabToSql(
      tab: Map[Var, Tree], 
      keys: Map[TypeNme, FieldName]): SqlFrom = 
    SFrom(List(tab.toList match {
      case (Var(nme), GLabel(info)) :: xs => 
        STable(info.tgt.nme, nme, xs.map(joinToSql(_, keys)))
      case _ => throw new Error(s"No table was selected for FROM clause")
    }))

  private def joinToSql(
      vt: (OpticLang.Var, Tree),
      keys: Map[TypeNme, FieldName]): SqlJoin = vt match {
    case (Var(n1), VL(Var(n2), GLabel(inf@OpticInfo(KGetter | KLens, _, _, _)))) => {
      val k = keys(inf.tgt.nme)  
      SEqJoin(s"${inf.tgt.nme}", n1, SOn(SProj(n2, inf.nme), SProj(n1, k)))
    }
    case (Var(n1), VL(Var(n2), GLabel(inf))) => {
      SEqJoin(s"${inf.tgt.nme}", n1, SOn(SProj(n2, inf.nme), SProj(n1, "id")))
    }
    case _ => throw new Error(s"Don't know how to generate join for '$vt'")
  }

  private def whrToSql(whr: Set[Tree]): Option[SqlExp] =
    whr.foldLeft(Option.empty[SqlExp]) {
      case (None, t) => Some(treeToExpr(t))
      case (Some(e), t) => Some(SBinOp("AND", e, treeToExpr(t)))
    }

  private def treeToExpr(t: Tree): SqlExp = t match {
    case VL(Var(nme), GLabel(info)) => SProj(nme, info.nme)
    case Op(op, l, r) => SBinOp(op, treeToExpr(l), treeToExpr(r))
    case Unary(t, op) => SUnOp(op, treeToExpr(t))
    case _ => throw new Error(s"Don't know how to translate '$t' into SQL")
  }
}

