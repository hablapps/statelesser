package org.hablapps.statelesser

import scalaz._

trait OpticAlg[Expr[_]] {

  def flVert[S, A, B](
    l: Expr[Fold[S, A]], 
    r: Expr[Fold[A, B]]): Expr[Fold[S, B]]

  def gtVert[S, A, B](
    l: Expr[Getter[S, A]], 
    r: Expr[Getter[A, B]]): Expr[Getter[S, B]]

  def gtHori[S, A, B](
    l: Expr[Getter[S, A]],
    r: Expr[Getter[S, B]]): Expr[Getter[S, (A, B)]]

  def gtSub[S](
    l: Expr[Getter[S, Int]],
    r: Expr[Getter[S, Int]]): Expr[Getter[S, Int]]

  def aflVert[S, A, B](
    l: Expr[AffineFold[S, A]], 
    r: Expr[AffineFold[A, B]]): Expr[AffineFold[S, B]]

  def aflHori[S, A, B](
    l: Expr[AffineFold[S, A]],
    r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, (A, B)]]

  def aflFirst[S, A, B](
    l: Expr[AffineFold[S, A]], 
    r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, A]]

  def filtered[S](p: Expr[Getter[S, Boolean]]): Expr[AffineFold[S, S]]

  // greater than, bad name!
  def gt(i: Int): Expr[Getter[Int, Boolean]]

  def first[A, B]: Expr[Getter[(A, B), A]]

  def second[A, B]: Expr[Getter[(A, B), B]]

  def getAll[S, A](fl: Expr[Fold[S, A]]): Expr[S => List[A]]

  def lnAsGt[S, A](ln: Expr[Lens[S, A]]): Expr[Getter[S, A]]

  def gtAsFl1[S, A](gt: Expr[Getter[S, A]]): Expr[Fold1[S, A]]

  def gtAsAfl[S, A](gt: Expr[Getter[S, A]]): Expr[AffineFold[S, A]]

  def aflAsFl[S, A](afl: Expr[AffineFold[S, A]]): Expr[Fold[S, A]]

  def fl1AsFl[S, A](fl1: Expr[Fold1[S, A]]): Expr[Fold[S, A]]
}

object OpticAlg {

  type TypeNme = String
  type OpticNme = String

  case class TypeInfo(nme: TypeNme, isPrimitive: Boolean = false)
  case class OpticInfo(nme: OpticNme, src: TypeInfo, tgt: TypeInfo) {
    override def toString = nme
  }
  
  case class Semantic(
      table: Map[Var, Tree] = Map(),
      select: List[Tree] = List(),
      where: Set[Tree] = Set()) {
    
    import Sql._

    def toSql: SSelect = SSelect(selToSql, tabToSql, whrToSql)

    private def selToSql: SqlSelect = select match {
      case Var(e) :: Nil => SAll(e)
      case xs => SList(xs.map(e => SField(treeToExpr(e), "")))
    }

    private def tabToSql: SqlFrom = SFrom(List(table.toList match {
      case (Var(nme), GLabel(info)) :: xs => 
        STable(info.src.nme, nme, xs.map(joinToSql))
      case _ => throw new Error(s"No table was selected for FROM clause")
    }))

    private def joinToSql(vt: (OpticAlg.Var, Tree)): SqlJoin = vt match {
      case (Var(n1), VL(Var(n2), GLabel(inf))) => SEqJoin(
        s"Dummy_${inf.tgt.nme}", n1, SOn(SProj(n2, inf.nme), SProj(n1, "Dummy_Id")))
      case _ => throw new Error(s"Don't know how to generate join for '$vt'")
    }

    private def whrToSql: Option[SqlExp] =
      where.foldLeft(Option.empty[SqlExp]) {
        case (None, t) => Some(treeToExpr(t))
        case (Some(e), t) => Some(SBinOp("AND", e, treeToExpr(t)))
      }

    private def treeToExpr(t: Tree): SqlExp = t match {
      case VL(Var(nme), GLabel(info)) => SProj(nme, info.nme)
      case Op(op, l, r) => SBinOp(op, treeToExpr(l), treeToExpr(r))
      case Unary(t, op) => SUnOp(op, treeToExpr(t))
      case _ => throw new Error(s"Don't know how to translate '$t' into SQL")
    }

    def hCompose(other: Semantic): Semantic = {
      val Semantic(t1, s1, w1) = this
      val Semantic(t2, s2, w2) = other
      Semantic(t1 ++ t2, s1 ++ s2, w1 ++ w2)
    }

    def vCompose(other: Semantic): Semantic = {
      (this, other) match {
        case (Semantic(t1, List(tr1), w1), Semantic(t2, s2, w2)) => Semantic(
          t1 ++ t2.mapValues(tr1.vCompose), 
          if (s2.isEmpty) List(tr1) else s2.map(tr1.vCompose), 
          w1 ++ w2.map(tr1.vCompose))
        case (Semantic(t1, List(tr1, tr2), w1), 
              Semantic(t2, List(Unapplied(f)), w2)) => {
          val tr3 = f(tr1)(tr2)
          Semantic(
            t1 ++ t2.mapValues(tr3.vCompose), 
            List(tr3),
            w1 ++ w2.map(tr3.vCompose))
        }
        case (Semantic(t1, List(tr1, tr2), w1), 
              Semantic(t2, List(Unapplied(f), Unapplied(g)), w2)) => {
          Semantic(t1, List(f(tr1)(tr2), g(tr1)(tr2)), w1 ++ w2)
        }
      }
    }

    def fstCompose(other: Semantic): Semantic = {
      val Semantic(t1, s1, w1) = this
      val Semantic(t2, s2, w2) = other
      Semantic(t1 ++ t2, s1, w1 ++ w2)
    }

    def subCompose(other: Semantic): Semantic = {
      val Semantic(t1, List(tr1), w1) = this
      val Semantic(t2, List(tr2), w2) = other
      Semantic(t1 ++ t2, List(Op("-", tr1, tr2)), w1 ++ w2)
    }
  }

  sealed abstract class Tree {
    def vCompose(other: Tree): Tree = (this, other) match {
      case (v: Var, l: GLabel) => VL(v, l)
      case (_, v: Var) => v
      case (_, vl: VL) => vl
      case (x, Op(op, l, r)) => Op(op, x vCompose l, x vCompose r)
      case (x, Unary(t, op)) => Unary(x vCompose t, op)
      case (l, Pred(op)) => Unary(l, op)
      case (l, r) => println(l); println(r); ???
    }
  }
  case class GLabel(info: OpticInfo) extends Tree {
    override def toString = info.toString
  }
  case class Var(nme: String) extends Tree {
    override def toString = nme
  }
  case class VL(vr: Var, lbl: GLabel) extends Tree {
    override def toString = s"$vr.$lbl"
  }
  case class Op(op: String, l: Tree, r: Tree) extends Tree {
    override def toString = s"$l $op $r"
  }
  case class Unary(t: Tree, op: String) extends Tree {
    override def toString = s"$t $op"
  }
  case class Unapplied(f: Tree => Tree => Tree) extends Tree
  case class Pred(op: String) extends Tree

  implicit val semantic = new OpticAlg[Const[Semantic, ?]] {

    def flVert[S, A, B](
        l: Const[Semantic, Fold[S, A]], 
        r: Const[Semantic, Fold[A, B]]) = {
      Const(l.getConst vCompose r.getConst)
    }

    def gtVert[S, A, B](
        l: Const[Semantic, Getter[S, A]], 
        r: Const[Semantic, Getter[A, B]]) = {
      Const(l.getConst vCompose r.getConst)
    }

    def gtHori[S, A, B](
        l: Const[Semantic, Getter[S, A]],
        r: Const[Semantic, Getter[S, B]]) = {
      Const(l.getConst hCompose r.getConst)
    }

    def gtSub[S](
        l: Const[Semantic, Getter[S, Int]],
        r: Const[Semantic, Getter[S, Int]]) =
      Const(l.getConst subCompose r.getConst)

    def filtered[S](p: Const[Semantic, Getter[S, Boolean]]) =
      Const(Semantic(Map(), List.empty, p.getConst.select.toSet))

    def gt(i: Int): Const[Semantic, Getter[Int, Boolean]] =
      Const(Semantic(Map(), List(Pred(s"(>$i)"))))

    def aflAsFl[S, A](afl: Const[Semantic, AffineFold[S, A]]) =
      Const(afl.getConst)

    def aflHori[S, A, B](
        l: Const[Semantic, AffineFold[S, A]], 
        r: Const[Semantic, AffineFold[S, B]]) =
      Const(l.getConst hCompose r.getConst)

    def aflVert[S, A, B](
        l: Const[Semantic, AffineFold[S, A]], 
        r: Const[Semantic, AffineFold[A, B]]) =
      Const(l.getConst vCompose r.getConst)

    def aflFirst[S, A, B](
        l: Const[Semantic, AffineFold[S, A]], 
        r: Const[Semantic, AffineFold[S, B]]) =
      Const(l.getConst fstCompose r.getConst)

    def first[A, B] =
      Const(Semantic(select = List(Unapplied(l => _ => l))))

    def second[A, B] =
      Const(Semantic(select = List(Unapplied(_ => r => r))))

    def gtAsAfl[S, A](gt: Const[Semantic, Getter[S, A]]) =
      Const(gt.getConst)

    def getAll[S, A](fl: Const[Semantic, Fold[S, A]]) =
      Const(fl.getConst)

    def lnAsGt[S, A](ln: Const[Semantic, Lens[S, A]]) =
      Const(ln.getConst)

    def gtAsFl1[S, A](gt: Const[Semantic, Getter[S, A]]) =
      Const(gt.getConst)

    def fl1AsFl[S, A](fl1: Const[Semantic, Fold1[S, A]]) =
      Const(fl1.getConst)
  }

  trait Syntax {

    implicit class FoldOps[Expr[_], S, A](
        l: Expr[Fold[S, A]])(implicit 
        O: OpticAlg[Expr]) {
      def >[B](r: Expr[Fold[A, B]]): Expr[Fold[S, B]] = O.flVert(l, r)
    }

    implicit class Fold1Ops[Expr[_], S, A](
        l: Expr[Fold1[S, A]])(implicit
        O: OpticAlg[Expr]) {
      def asFold: Expr[Fold[S, A]] = O.fl1AsFl(l)
    }

    implicit class AffineFoldOps[Expr[_], S, A](
        l: Expr[AffineFold[S, A]])(implicit
        O: OpticAlg[Expr]) {
      def asFold: Expr[Fold[S, A]] = O.aflAsFl(l)
      def >[B](r: Expr[AffineFold[A, B]]): Expr[AffineFold[S, B]] = 
        O.aflVert(l, r)
      def *[B](r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, (A, B)]] = 
        O.aflHori(l, r)
      def <*[B](r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, A]] =
        O.aflFirst(l, r)
    }

    implicit class GetterOps[Expr[_], S, A](
        l: Expr[Getter[S, A]])(implicit
        O: OpticAlg[Expr]) {
      def asFold1: Expr[Fold1[S, A]] = O.gtAsFl1(l)
      def asAffineFold: Expr[AffineFold[S, A]] = O.gtAsAfl(l)
      def >[B](r: Expr[Getter[A, B]]): Expr[Getter[S, B]] = O.gtVert(l, r)
      def *[B](r: Expr[Getter[S, B]]): Expr[Getter[S, (A, B)]] = O.gtHori(l, r)
    }

    implicit class GetterIntOps[Expr[_], S](
        l: Expr[Getter[S, Int]])(implicit
        O: OpticAlg[Expr]) {
      def -(r: Expr[Getter[S, Int]]): Expr[Getter[S, Int]] = O.gtSub(l, r)
    }
  }

  object syntax extends Syntax
}

trait CoupleExample[Expr[_]] {

  /* data layer */

  implicit val ev: OpticAlg[Expr]
  
  type Couple
  type Couples = List[Couple]
  type Person
  type People = List[Person]

  val couples: Expr[Fold[Couples, Couple]]
  val her: Expr[Getter[Couple, Person]]
  val him: Expr[Getter[Couple, Person]]
  val people: Expr[Fold[People, Person]]
  val name: Expr[Getter[Person, String]]
  val age: Expr[Getter[Person, Int]]

  /* logic */

  import ev._
  import OpticAlg.syntax._

  def getPeople: Expr[People => List[Person]] =
    getAll(people)

  def getPeopleName: Expr[People => List[String]] =
    getAll(people > name.asFold1.asFold)

  def getPeopleNameAndAge: Expr[People => List[(String, Int)]] =
    getAll(people > (name * age).asFold1.asFold)

  def getHerNames: Expr[Couples => List[String]] =
    getAll(couples > her.asFold1.asFold > name.asFold1.asFold)

  def getHerNameAndAge: Expr[Couples => List[(String, Int)]] =
    getAll(couples > her.asFold1.asFold > (name * age).asFold1.asFold)

  def getHerNameAndAge2: Expr[Couples => List[(String, Int)]] =
    getAll(couples > ((her > name) * (her > age)).asFold1.asFold)

  def getHerNameAndAge3: Expr[Couples => List[(String, Int)]] =
    getAll(couples > (her > name * age).asFold1.asFold)

  def getPeopleGt30: Expr[People => List[(String, Int)]] =
    getAll(people > (name.asAffineFold * 
      (age.asAffineFold > filtered (gt(30)))).asFold)

  def getHerGt30: Expr[Couples => List[(String, Int)]] =
    getAll(couples > her.asFold1.asFold > (name.asAffineFold * 
      (age.asAffineFold > filtered (gt(30)))).asFold)

  def getHerGt30_2: Expr[Couples => List[(String, Int)]] =
    getAll(couples > ((her > name).asAffineFold * 
      ((her > age).asAffineFold > filtered (gt(30)))).asFold)

  def getHerNameGt30: Expr[Couples => List[String]] =
    getAll(couples > her.asFold1.asFold > (name.asAffineFold <* 
      (age.asAffineFold > filtered (gt(30)))).asFold)
  
  def getHerNameGt30_2: Expr[Couples => List[String]] =
    getAll(couples > ((her > name).asAffineFold <* 
      ((her > age).asAffineFold > filtered (gt(30)))).asFold)

  def difference: Expr[Couples => List[(String, Int)]] =
    getAll(couples > 
      ((her > name).asAffineFold * 
        (((her > age) - (him > age)).asAffineFold > filtered (gt(0)))).asFold)

  def differenceName: Expr[Couples => List[String]] =
    getAll(couples > 
      ((her > name).asAffineFold <* 
        (((her > age) - (him > age)).asAffineFold > filtered (gt(0)))).asFold)

  def differenceName2: Expr[Couples => List[String]] =
    getAll(couples > 
      ((her > name).asAffineFold * 
        (((her > age) - (him > age)).asAffineFold > filtered (gt(0))) > 
          first.asAffineFold).asFold)

  def dummyNameAndAge: Expr[People => List[(String, Int)]] =
    getAll(people > ((name.asAffineFold * ((name * age > 
      first * second > 
      second * first > 
      second * first > 
      second).asAffineFold 
      > filtered (gt(30))
      > filtered (gt(40))))).asFold)
}

object CoupleExample {
  import OpticAlg._

  implicit val semantic = new CoupleExample[Const[Semantic, ?]] {

    implicit val ev = OpticAlg.semantic

    type Couple = Unit
    type Person = Unit

    val couples = {

      val oi = OpticInfo(
        "couples", 
        TypeInfo("Couples", false), 
        TypeInfo("Couple", true))

      Const(Semantic(Map(Var("c") -> GLabel(oi)), List(Var("c"))))
    }

    val her = {

      val oi = OpticInfo(
        "her", 
        TypeInfo("Couple", false), 
        TypeInfo("Person", true))

      Const(Semantic(Map(Var("w") -> GLabel(oi)), List(Var("w"))))
    }

    val him = {

      val oi = OpticInfo(
        "him", 
        TypeInfo("Couple", false), 
        TypeInfo("Person", true))

      Const(Semantic(Map(Var("m") -> GLabel(oi)), List(Var("m"))))
    }

    val people = {

      val oi = OpticInfo(
        "people", 
        TypeInfo("People", false), 
        TypeInfo("Person", true))

      Const(Semantic(Map(Var("p") -> GLabel(oi)), List(Var("p"))))
    }

    val name = {

      val oi = OpticInfo(
        "name", 
        TypeInfo("Person", false), 
        TypeInfo("Person", true))

      Const(Semantic(Map(), List(GLabel(oi))))
    }

    val age = {

      val oi = OpticInfo(
        "age", 
        TypeInfo("Person", false), 
        TypeInfo("Person", true))

      Const(Semantic(Map(), List(GLabel(oi))))
    }
  }
}

object Run extends App {
  import CoupleExample.semantic._

  println(getPeople.getConst.toSql)
  println(getPeopleName.getConst.toSql)
  println(getPeopleNameAndAge.getConst.toSql)
  println(getHerNames.getConst.toSql)
  println(getHerNameAndAge.getConst.toSql)
  println(getHerNameAndAge2.getConst.toSql)
  println(getHerNameAndAge3.getConst.toSql)
  println(getPeopleGt30.getConst.toSql)
  println(getHerGt30.getConst.toSql)
  println(getHerGt30_2.getConst.toSql)
  println(getHerNameGt30.getConst.toSql)
  println(getHerNameGt30_2.getConst.toSql)
  println(difference.getConst.toSql)
  println(differenceName.getConst.toSql)
  println(differenceName2.getConst.toSql)
  println(dummyNameAndAge.getConst.toSql)
}

