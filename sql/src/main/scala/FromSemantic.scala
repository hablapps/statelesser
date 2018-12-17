package statelesser
package sql

import scalaz._, Scalaz._

import OpticLang._

trait FromSemantic {
  import OpticLang.Table

  def fromSemantic[E[_], S, A](
      sem: TSemantic[E, Fold[S, A]],
      keys: Map[TypeNme, FieldName] = Map()): SSelect = {
    val TFold(vars, expr) = sem
    SSelect(selToSql(expr, keys), tabToSql(vars, keys), None)
  }

  private def flatProduct[E[_]](
      x: TExpr[E, Fold, _, _]): List[TExpr[E, Fold, _, _]] = x match {
    case Product(l, r, is, _, _) => List(flatProduct(l), flatProduct(r)).join
    case _ => List(x)
  }

  private def selToSql[E[_], S, A](
      sel: TExpr[E, Fold, S, A],
      keys: Map[TypeNme, FieldName]): SqlSelect = sel match {
    case Var(e) => SAll(e)
    case t => SList(flatProduct(t).map(e => SField(treeToExpr(e, keys), "")))
  }

  private def tabToSql[E[_]](
      tab: Table[E, Fold],
      keys: Map[TypeNme, FieldName]): SqlFrom =
    tab.simpleTable.toList match {
      case List((nme, TVarSimpleVal(Wrap(_, info)))) => SFrom(List(
        STable(info.tgt.nme, nme, tab.nestedTable.toList.map(joinToSql(_, keys)))))
      case _ => 
        throw new Error(s"Sorry, but we don't support product roots yet: $tab")
    }

  private def condToSql(
      v1: String, n1: FieldName, 
      v2: String, n2: FieldName): SqlEqJoinCond =
    if (n1 == n2) SUsing(n1) else SOn(SProj(v1, n1), SProj(v2, n2))

  private def joinToSql[E[_]](
      vt: (String, TVarNestedVal[E, Fold, _, _]),
      keys: Map[TypeNme, FieldName]): SqlJoin = {
    val nme = vt._1
    val inf = vt._2.w.info
    val v = vt._2.vs.lastVar
    if (inf.kind == KGetter) {
      val cond = condToSql(v.name, inf.nme, nme, keys(inf.tgt.nme))
      SEqJoin(s"${inf.tgt.nme}", nme, cond)
    } else {
      SEqJoin(s"${inf.tgt.nme}", nme, SUsing(keys(inf.src.nme)))
    }
  }

  // private def whrToSql(
  //     whr: Set[TTree],
  //     keys: Map[TypeNme, FieldName]): Option[SqlExp] =
  //   whr.foldLeft(Option.empty[SqlExp]) {
  //     case (None, t) => Some(treeToExpr(t, keys))
  //     case (Some(e), t) => Some(SBinOp("AND", e, treeToExpr(t, keys)))
  //   }


  private def treeToExpr[E[_]](
      t: TExpr[E, Fold, _, _],
      keys: Map[TypeNme, FieldName]): SqlExp = t match {
    case Wrap(_, info) => SProj("", info.nme)
    case Vertical(Var(nme), Wrap(_, info), _, _) => SProj(nme, info.nme)
    case Vertical(Product(l, r, _, _, _), Sub(_, _), _, _) => 
      SBinOp("-", treeToExpr(l, keys), treeToExpr(r, keys))
    case Vertical(e, Not(_, _), _, _) => SUnOp("NOT", treeToExpr(e, keys))
    case LikeInt(i, _) => SCons(i.toString)
    case LikeBool(b, _) => SCons(b.toString)
    case _ => throw new Error(s"Don't know how to translate '$t' into SQL")
  }
}

