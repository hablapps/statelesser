package statelesser
package sql

import scalaz._, Scalaz._

import statelesser.Value

trait FromSemantic {

  val varStr: Stream[String] = {
    def syms(pattern: Stream[String], i: Int = 0): Stream[String] =
      pattern.map(_ + i) #::: syms(pattern, i + 1)
    val pattern = Stream.range('a', 'z').map(_.toString)
    pattern #::: syms(pattern)
  }

  def fromSemantic[S, A](
      sem: Semantic[Fold[S, A]],
      keys: Map[TypeNme, FieldName] = Map()): SSelect = {
    val Done(expr, filt, vars) = sem.eval(varStr)
    SSelect(selToSql(expr, keys), tabToSql(vars, keys), whrToSql(filt, keys))
  }

  private def flatProduct(
      x: TSel[_, _]): List[TExpr[_, _]] = x match {
    case Pair(l, r, _) => List(flatProduct(l), flatProduct(r)).join
    case Just(e) => List(e)
  }

  private def selToSql[S, A](
      sel: TSel[S, A],
      keys: Map[TypeNme, FieldName]): SqlSelect = sel match {
    case t => SList(flatProduct(t).map(e => SField(treeToExpr(e, keys), "")))
  }

  private def tabToSql[E[_]](
      vars: Map[Symbol, Value],
      keys: Map[TypeNme, FieldName]): SqlFrom = {
    val (roots, nodes) = vars.foldLeft(
      (List.empty[(String, OpticType[_, _])], List.empty[(String, Select[_, _, _])])) {
        case ((rs, ns), (k, -\/(x))) => (rs :+ (k, x), ns)
        case ((rs, ns), (k, \/-(x))) => (rs, ns :+ (k, x))
      }
    roots.toList match {
      case List((nme, ot)) => SFrom(List(
        STable(ot.tgt.nme, nme, nodes.toList.sortBy(_._1).map(joinToSql(_, keys)))))
      case Nil => 
        throw new Error(s"There's no table to select from: $vars")
      case _ =>
        // TODO: we should be able to translate this case into a raw JOIN
        throw new Error(s"Can't translate semantic with multiple roots: $vars")
    }
  }

  private def condToSql(
      v1: String, n1: FieldName,
      v2: String, n2: FieldName): SqlEqJoinCond =
    if (n1 == n2) SUsing(n1) else SOn(SProj(v1, n1), SProj(v2, n2))

  private def joinToSql[E[_]](
      vt: (String, Select[_, _, _]),
      keys: Map[TypeNme, FieldName]): SqlJoin = {
    val (nme, Select(v, ot)) = vt
    if (ot.kind == KGetter) {
      val cond = condToSql(v.sym, ot.nme, nme, keys(ot.tgt.nme))
      SEqJoin(s"${ot.tgt.nme}", nme, cond)
    } else {
      SEqJoin(s"${ot.tgt.nme}", nme, SUsing(keys(ot.src.nme)))
    }
  }

  private def whrToSql[S](
      whr: Set[TExpr[S, Boolean]],
      keys: Map[TypeNme, FieldName]): Option[SqlExp] =
    whr.foldLeft(Option.empty[SqlExp]) {
      case (None, t) => Some(treeToExpr(t, keys))
      case (Some(e), t) => Some(SBinOp("AND", e, treeToExpr(t, keys)))
    }

  private def treeToExpr(
      t: TExpr[_, _],
      keys: Map[TypeNme, FieldName]): SqlExp = t match {
    case Var(e) => SAll(e)
    case Select(Var(s), ot) => SProj(s, ot.nme)
    case Sub(l, r, _) => SBinOp("-", treeToExpr(l, keys), treeToExpr(r, keys))
    case Gt(l, r, _) => SBinOp(">", treeToExpr(l, keys), treeToExpr(r, keys))
    case Not(e, _) => SUnOp("NOT", treeToExpr(e, keys))
    case LikeInt(i) => SCons(i.toString)
    case LikeBool(b) => SCons(b.toString)
    case LikeStr(s) => SCons(s""""$s"""")
  }
}

