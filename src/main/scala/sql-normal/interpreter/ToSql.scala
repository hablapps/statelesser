package statelesser
package sqlnormal
package interpreter

import scalaz._, Scalaz._
import optic._
import sql._

class ToSql {

  def toSql[S, A](
      sem: Semantic[Fold[S, A]],
      keys: Map[TypeNme, FieldName] = Map()): SSelect = {
    val Done(expr, filt, vars) = sem.eval(varStr)
    SSelect(selToSql(expr, keys), tabToSql(vars, keys), whrToSql(filt, keys))
  }

  private val varStr: Stream[String] = {
    def syms(pattern: Stream[String], i: Int = 0): Stream[String] =
      pattern.map(_ + i) #::: syms(pattern, i + 1)
    val pattern = Stream.range('a', 'z').map(_.toString)
    pattern #::: syms(pattern)
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
      vars: TVarTree,
      keys: Map[TypeNme, FieldName]): SqlFrom = vars.toList match {
    case (nme, ITree(ot, nodes)) :: Nil => 
      SFrom(List(STable(ot.tgt.nme, nme, seqJoinToSql(nme, nodes, keys))))
    case _ =>
      throw new Error(s"Can't translate semantic with multiple roots: $vars")
  }

  private def condToSql(
      v1: String, n1: FieldName,
      v2: String, n2: FieldName): SqlEqJoinCond =
    if (n1 == n2) SUsing(n1) else SOn(SProj(v1, n1), SProj(v2, n2))

  private def seqJoinToSql(
      top: String, 
      frs: IForest[Symbol, OpticType[_, _]],
      keys: Map[TypeNme, FieldName]): List[SqlJoin] =
    frs.foldLeft(List.empty[SqlJoin]) { 
      case (acc, (now, ITree(ot, child))) =>
        (acc :+ joinToSql(top, now, ot, keys)) ++ seqJoinToSql(now, child, keys)
    }

  private def joinToSql(
      top: String,
      now: String,
      ot: OpticType[_, _],
      keys: Map[TypeNme, FieldName]): SqlJoin = SEqJoin(ot.tgt.nme, now, 
    if (ot.kind == KGetter) 
      condToSql(now, ot.nme, top, keys(ot.tgt.nme)) 
    else 
      SUsing(keys(ot.src.nme)))

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
    case Var(es) => SAll(es.head)
    case Select(Var(syms), ot) => SProj(syms.head, ot.nme)
    case Sub(l, r, _) => SBinOp("-", treeToExpr(l, keys), treeToExpr(r, keys))
    case Gt(l, r, _) => SBinOp(">", treeToExpr(l, keys), treeToExpr(r, keys))
    case Not(e, _) => SUnOp("NOT", treeToExpr(e, keys))
    case LikeInt(i) => SCons(i.toString)
    case LikeBool(b) => SCons(b.toString)
    case LikeStr(s) => SCons(s""""$s"""")
  }
}

