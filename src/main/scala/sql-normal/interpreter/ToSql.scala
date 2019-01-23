package statelesser
package sqlnormal
package interpreter

import scalaz._, Scalaz._
import monocle._
import sql._

class ToSql {

  def toSql[S, A](
      sem: Semantic[Fold[S, A]],
      keys: Map[TypeNme, FieldName] = Map()): SSelect = {
    val Done(expr, filt, vars) = sem.eval(varStr)
    SSelect(
      selToSql(expr, vars, keys), 
      tabToSql(vars, keys), 
      whrToSql(filt, vars, keys))
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
      vars: TVarMap,
      keys: Map[TypeNme, FieldName]): SqlSelect = sel match {
    case t => SList(flatProduct(t).map(e => SField(treeToExpr(e, vars, keys), "")))
  }

  private def tabToSql[E[_]](
      vars: TVarMap,
      keys: Map[TypeNme, FieldName]): SqlFrom = vars.toList match {
    case (ot, ITree(v, nodes)) :: xs =>
      SFrom(List(STable(ot.tgt.nme, v, xs.foldLeft(seqJoinToSql(v, nodes, keys)) {
        case (acc, (ot2, ITree(v2, nodes2))) => 
          acc ::: (SJoin(ot2.tgt.nme, v2) :: seqJoinToSql(v2, nodes2, keys))
      })))
    case _ =>
      throw new Error(s"Can't translate semantic with multiple roots: $vars")
  }

  private def condToSql(
      v1: String, n1: FieldName,
      v2: String, n2: FieldName): SqlEqJoinCond =
    if (n1 == n2) SUsing(n1) else SOn(SProj(v1, n1), SProj(v2, n2))

  private def seqJoinToSql(
      topv: Symbol, 
      frs: TVarMap,
      keys: Map[TypeNme, FieldName]): List[SqlJoin] =
    frs.foldLeft(List.empty[SqlJoin]) { 
      case (acc, (ot, ITree(v, child))) =>
        (acc :+ joinToSql(topv, ot.nme, v, ot, keys)) ++ 
          seqJoinToSql(ot.nme, child, keys)
    }

  private def joinToSql(
      topv: Symbol,
      nme: String,
      v: Symbol,
      ot: OpticType[_, _],
      keys: Map[TypeNme, FieldName]): SqlJoin = SEqJoin(ot.tgt.nme, v, 
    ot match {
      case FoldType(_, src, _) => SUsing(keys(src.nme))
      case other => condToSql(topv, nme, v, keys(other.tgt.nme))
    })

  private def whrToSql[S](
      whr: Set[TExpr[S, Boolean]],
      vars: TVarMap,
      keys: Map[TypeNme, FieldName]): Option[SqlExp] =
    whr.foldLeft(Option.empty[SqlExp]) {
      case (None, t) => Some(treeToExpr(t, vars, keys))
      case (Some(e), t) => Some(SBinOp("AND", e, treeToExpr(t, vars, keys)))
    }

  private def treeToExpr(
      t: TExpr[_, _],
      vars: TVarMap,
      keys: Map[TypeNme, FieldName]): SqlExp = t match {
        case Var(op) => op.getOption(vars).fold(???)(it => SAll(it.label))
    case Select(Var(op), ot) => 
      op.getOption(vars).fold(???)(it => SProj(it.label, ot.nme))
    case Sub(l, r, _) => 
      SBinOp("-", treeToExpr(l, vars, keys), treeToExpr(r, vars, keys))
    case Gt(l, r, _) => 
      SBinOp(">", treeToExpr(l, vars, keys), treeToExpr(r, vars, keys))
    case Not(e, _) => SUnOp("NOT", treeToExpr(e, vars, keys))
    case LikeInt(i) => SCons(i.toString)
    case LikeBool(b) => SCons(b.toString)
    case LikeStr(s) => SCons(s""""$s"""")
  }
}

