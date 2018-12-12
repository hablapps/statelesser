package statelesser

import scalaz._, Scalaz._
import Leibniz._

trait OpticLang[Expr[_]] {

  def flVert[S, A, B](
    l: Expr[Fold[S, A]], 
    r: Expr[Fold[A, B]]): Expr[Fold[S, B]]

  def flHori[S, A, B](
    l: Expr[Fold[S, A]], 
    r: Expr[Fold[S, B]]): Expr[Fold[S, (A, B)]]

  def gtVert[S, A, B](
    l: Expr[Getter[S, A]], 
    r: Expr[Getter[A, B]]): Expr[Getter[S, B]]

  def gtHori[S, A, B](
    l: Expr[Getter[S, A]],
    r: Expr[Getter[S, B]]): Expr[Getter[S, (A, B)]]

  def aflVert[S, A, B](
    l: Expr[AffineFold[S, A]], 
    r: Expr[AffineFold[A, B]]): Expr[AffineFold[S, B]]

  def aflHori[S, A, B](
    l: Expr[AffineFold[S, A]],
    r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, (A, B)]]

  def filtered[S](p: Expr[Getter[S, Boolean]]): Expr[AffineFold[S, S]]

  def sub: Expr[Getter[(Int, Int), Int]]

  def greaterThan: Expr[Getter[(Int, Int), Boolean]]

  def equal[A]: Expr[Getter[(A, A), Boolean]]

  def first[A, B]: Expr[Getter[(A, B), A]]

  def second[A, B]: Expr[Getter[(A, B), B]]

  def likeInt[S](i: Int): Expr[Getter[S, Int]]

  def likeBool[S](b: Boolean): Expr[Getter[S, Boolean]]

  def likeStr[S](s: String): Expr[Getter[S, String]]

  def id[S]: Expr[Getter[S, S]]

  def not: Expr[Getter[Boolean, Boolean]]

  def getAll[S, A](fl: Expr[Fold[S, A]]): Expr[S => List[A]]

  def lnAsGt[S, A](ln: Expr[Lens[S, A]]): Expr[Getter[S, A]]

  def gtAsFl1[S, A](gt: Expr[Getter[S, A]]): Expr[Fold1[S, A]]

  def gtAsAfl[S, A](gt: Expr[Getter[S, A]]): Expr[AffineFold[S, A]]

  def aflAsFl[S, A](afl: Expr[AffineFold[S, A]]): Expr[Fold[S, A]]

  def fl1AsFl[S, A](fl1: Expr[Fold1[S, A]]): Expr[Fold[S, A]]

  // derived methods

  def gt(i: Int): Expr[Getter[Int, Boolean]] = 
    gtVert(gtHori(id, likeInt(i)), greaterThan)

  // def eq(i: Int): Expr[Getter[Int, Boolean]] =
  //   gtVert(gtHori(id, like(i)), equal[A])

  // def contains[S, A](fl: Expr[Fold[S, A]], a: A): Expr[Getter[S, Boolean]] =
  //   exists(fl, eq(a))

  // def all[S, A](fl: Expr[Fold[S, A]], p: Expr[Getter[A, Boolean]]) =
  //   gtVert(exists(fl, gtVert(p, not)), not)
}

object OpticLang {

  def apply[E[_]](implicit ev: OpticLang[E]): OpticLang[E] = ev

  trait AnnotatedOpticLang[Ann[_[_], _], E[_]] 
      extends OpticLang[Ann[E, ?]] with Annotated[OpticLang, Ann, E] {

    def flVert[S, A, B](
        l: Ann[E, Fold[S, A]], 
        r: Ann[E, Fold[A, B]]): Ann[E, Fold[S, B]] =
      inject(alg.flVert(run(l), run(r)))

    def flHori[S, A, B](
        l: Ann[E, Fold[S, A]], 
        r: Ann[E, Fold[S, B]]): Ann[E, Fold[S, (A, B)]] =
      inject(alg.flHori(run(l), run(r)))

    def gtVert[S, A, B](
        l: Ann[E, Getter[S, A]], 
        r: Ann[E, Getter[A, B]]): Ann[E, Getter[S, B]] =
      inject(alg.gtVert(run(l), run(r)))

    def gtHori[S, A, B](
        l: Ann[E, Getter[S, A]],
        r: Ann[E, Getter[S, B]]): Ann[E, Getter[S, (A, B)]] =
      inject(alg.gtHori(run(l), run(r)))

    def aflVert[S, A, B](
        l: Ann[E, AffineFold[S, A]], 
        r: Ann[E, AffineFold[A, B]]): Ann[E, AffineFold[S, B]] =
      inject(alg.aflVert(run(l), run(r)))

    def aflHori[S, A, B](
        l: Ann[E, AffineFold[S, A]],
        r: Ann[E, AffineFold[S, B]]): Ann[E, AffineFold[S, (A, B)]] =
      inject(alg.aflHori(run(l), run(r)))

    def filtered[S](p: Ann[E, Getter[S, Boolean]]): Ann[E, AffineFold[S, S]] =
      inject(alg.filtered(run(p)))

    def sub: Ann[E, Getter[(Int, Int), Int]] =
      inject(alg.sub)

    def greaterThan: Ann[E, Getter[(Int, Int), Boolean]] =
      inject(alg.greaterThan)

    def equal[A]: Ann[E, Getter[(A, A), Boolean]] =
      inject(alg.equal)

    def first[A, B]: Ann[E, Getter[(A, B), A]] =
      inject(alg.first)

    def second[A, B]: Ann[E, Getter[(A, B), B]] =
      inject(alg.second)

    def likeInt[S](i: Int): Ann[E, Getter[S, Int]] =
      inject(alg.likeInt(i))

    def likeBool[S](b: Boolean): Ann[E, Getter[S, Boolean]] =
      inject(alg.likeBool(b))

    def likeStr[S](s: String): Ann[E, Getter[S, String]] =
      inject(alg.likeStr(s))

    def id[S]: Ann[E, Getter[S, S]] =
      inject(alg.id)

    def not: Ann[E, Getter[Boolean, Boolean]] =
      inject(alg.not)

    def getAll[S, A](fl: Ann[E, Fold[S, A]]): Ann[E, S => List[A]] =
      inject(alg.getAll(run(fl)))

    def lnAsGt[S, A](ln: Ann[E, Lens[S, A]]): Ann[E, Getter[S, A]] =
      inject(alg.lnAsGt(run(ln)))

    def gtAsFl1[S, A](gt: Ann[E, Getter[S, A]]): Ann[E, Fold1[S, A]] =
      inject(alg.gtAsFl1(run(gt)))

    def gtAsAfl[S, A](gt: Ann[E, Getter[S, A]]): Ann[E, AffineFold[S, A]] =
      inject(alg.gtAsAfl(run(gt)))

    def aflAsFl[S, A](afl: Ann[E, AffineFold[S, A]]): Ann[E, Fold[S, A]] =
      inject(alg.aflAsFl(run(afl)))

    def fl1AsFl[S, A](fl1: Ann[E, Fold1[S, A]]): Ann[E, Fold[S, A]] =
      inject(alg.fl1AsFl(run(fl1)))
  }

  implicit val prettyPrinter = new OpticLang[Const[String, ?]] {

    def flVert[S, A, B](
        l: Const[String, Fold[S, A]], 
        r: Const[String, Fold[A, B]]): Const[String, Fold[S, B]] =
      Const(s"${l.getConst} > ${r.getConst}")

    def flHori[S, A, B](
        l: Const[String, Fold[S, A]], 
        r: Const[String, Fold[S, B]]): Const[String, Fold[S, (A, B)]] =
      Const(s"${l.getConst} * ${r.getConst}")

    def gtVert[S, A, B](
        l: Const[String, Getter[S, A]], 
        r: Const[String, Getter[A, B]]): Const[String, Getter[S, B]] =
      Const(s"${l.getConst} > ${r.getConst}")

    def gtHori[S, A, B](
        l: Const[String, Getter[S, A]],
        r: Const[String, Getter[S, B]]): Const[String, Getter[S, (A, B)]] =
      Const(s"(${l.getConst} * ${r.getConst})")

    def aflVert[S, A, B](
        l: Const[String, AffineFold[S, A]], 
        r: Const[String, AffineFold[A, B]]): Const[String, AffineFold[S, B]] =
      Const(s"${l.getConst} > ${r.getConst}")

    def aflHori[S, A, B](
        l: Const[String, AffineFold[S, A]],
        r: Const[String, AffineFold[S, B]]): Const[String, AffineFold[S, (A, B)]] =
      Const(s"${l.getConst} * ${r.getConst}")

    def filtered[S](
        p: Const[String, Getter[S, Boolean]]): Const[String, AffineFold[S, S]] =
      Const(s"filtered(${p.getConst})")

    def sub: Const[String, Getter[(Int, Int), Int]] =
      Const("sub")

    def greaterThan: Const[String, Getter[(Int, Int), Boolean]] =
      Const("greaterThan")

    def equal[A]: Const[String, Getter[(A, A), Boolean]] =
      Const("equal")

    def first[A, B]: Const[String, Getter[(A, B), A]] =
      Const("first")

    def second[A, B]: Const[String, Getter[(A, B), B]] =
      Const("second")

    def likeInt[S](i: Int): Const[String, Getter[S, Int]] =
      Const(s"likeInt(${i.toString})")

    def likeBool[S](b: Boolean): Const[String, Getter[S, Boolean]] =
      Const(s"likeBool(${b.toString})")
    
    def likeStr[S](s: String): Const[String, Getter[S, String]] =
      Const(s"""likeStr("$s")""")

    def id[S]: Const[String, Getter[S, S]] =
      Const("id")

    def not: Const[String, Getter[Boolean, Boolean]] =
      Const("not")

    def getAll[S, A](
        fl: Const[String, Fold[S, A]]): Const[String, S => List[A]] =
      Const(s"getAll(${fl.getConst})")

    def lnAsGt[S, A](
        ln: Const[String, Lens[S, A]]): Const[String, Getter[S, A]] =
      Const(s"${ln.getConst}.asGetter")

    def gtAsFl1[S, A](
        gt: Const[String, Getter[S, A]]): Const[String, Fold1[S, A]] =
      Const(s"${gt.getConst}.asFold1")

    def gtAsAfl[S, A](
        gt: Const[String, Getter[S, A]]): Const[String, AffineFold[S, A]] =
      Const(s"${gt.getConst}.asAffineFold")

    def aflAsFl[S, A](
        afl: Const[String, AffineFold[S, A]]): Const[String, Fold[S, A]] =
      Const(s"${afl.getConst}.asAffineFold")

    def fl1AsFl[S, A](
        fl1: Const[String, Fold1[S, A]]): Const[String, Fold[S, A]] =
      Const(s"${fl1.getConst}.asFold")
  }

  type TypeNme = String
  type OpticNme = String
  
  sealed abstract class OpticKind
  case object KLens extends OpticKind
  case object KGetter extends OpticKind
  case object KAffineFold extends OpticKind
  case object KFold1 extends OpticKind
  case object KFold extends OpticKind

  case class TypeInfo(nme: TypeNme, isPrimitive: Boolean = false)
  case class OpticInfo(
    kind: OpticKind, 
    nme: OpticNme, 
    src: TypeInfo, 
    tgt: TypeInfo)
  
  sealed abstract class TTree

  case object TEmpty extends TTree

  sealed abstract class TApply extends TTree
  
  case class TApplyUnary(f: TTree => TTree) extends TApply

  case class TApplyBinary(f: TTree => TTree => TTree) 
    extends TApply

  sealed abstract class TExpression extends TTree

  case class TLiteral(s: String) extends TExpression

  case class TUnary(op: String, e: TTree) extends TExpression

  case class TBinary(op: String, l: TTree, r: TTree)
    extends TExpression

  sealed abstract class TVariable extends TExpression

  case class TVar(nme: String) extends TVariable

  sealed abstract class TSelection extends TExpression
  
  case class TOptic(info: OpticInfo) extends TSelection

  case class TProj(vr: TVariable, op: TOptic) extends TSelection

  def treeVertCompose(l: TTree, r: TTree): TTree = (l, r) match {
    case (TEmpty, r) => r
    case (l, TEmpty) => l
    case (_, lit: TLiteral) => lit
    case (TApplyUnary(f), TApplyUnary(g)) => TApplyUnary(g compose f)
    case (TApplyBinary(f), TApplyUnary(g)) => 
      TApplyBinary(t1 => t2 => g(f(t1)(t2)))
    case (TApplyUnary(f), r: TExpression) => 
      TApplyUnary(t => treeVertCompose(f(t), r))
    case (l: TExpression, TApplyUnary(f)) => f(l)
    case (TApplyBinary(f), r: TExpression) => 
      TApplyBinary(t1 => t2 => treeVertCompose(f(t1)(t2), r))
    case (TBinary("horizontal", l, r), TApplyBinary(f)) => f(l)(r)
    case (_, r: TVar) => r
    case (_, r: TProj) => r
    case (l: TVar, r: TOptic) => TProj(l, r)
    case (l: TExpression, TUnary(op, e)) => TUnary(op, treeVertCompose(l, e))
    case (l: TExpression, TBinary(op, e1, e2)) => 
      TBinary(op, treeVertCompose(l, e1), treeVertCompose(l, e2))
    case _ => throw new Error(s"Invalid vertical tree composition: `$l` & `$r`")
  }

  case class Semantic(
    pointer: TTree = TEmpty,
    symbols: List[(String, TSelection)] = List(),
    filters: Set[TTree] = Set())

  def vertCompose(l: Semantic, r: Semantic): Semantic = {

    val Semantic(p1, s1, f1) = l
    val Semantic(p2, s2, f2) = r

    val p3 = treeVertCompose(p1, p2)
    
    val s3 = ((s1.reverse, s2) match {
      case (xs, Nil) => xs.reverse
      case (Nil, ys) => ys
      case ((n, _) :: xs, ys) => s1 ++ ys.map { 
        case (k, opt: TOptic) => (k, TProj(TVar(n), opt))
        case x => x
      }
      case (_, _) => 
        throw new Error(s"Invalid vertical composition of symbols: $s1 $s2")
    }).distinct

    val f3 = f1 ++ f2.map(t => treeVertCompose(p1, t))

    Semantic(p3, s3, f3)
  }

  def horiCompose(l: Semantic, r: Semantic): Semantic =
    Semantic(
      TBinary("horizontal", l.pointer, r.pointer), 
      l.symbols ++ r.symbols, 
      l.filters ++ r.filters)

  implicit val semantic = new OpticLang[Const[Semantic, ?]] {

    def flVert[S, A, B](
        l: Const[Semantic, Fold[S, A]], 
        r: Const[Semantic, Fold[A, B]]) =
      Const(vertCompose(l.getConst, r.getConst))

    def flHori[S, A ,B](
        l: Const[Semantic, Fold[S, A]],
        r: Const[Semantic, Fold[S, B]]) =
      Const(horiCompose(l.getConst, r.getConst))

    def gtVert[S, A, B](
        l: Const[Semantic, Getter[S, A]], 
        r: Const[Semantic, Getter[A, B]]) =
      Const(vertCompose(l.getConst, r.getConst))

    def gtHori[S, A, B](
        l: Const[Semantic, Getter[S, A]],
        r: Const[Semantic, Getter[S, B]]) =
      Const(horiCompose(l.getConst, r.getConst))

    def filtered[S](p: Const[Semantic, Getter[S, Boolean]]) =
      Const(Semantic(filters = Set(p.getConst.pointer)))

    def sub =
      Const(Semantic(pointer = TApplyBinary(l => r => TBinary("-", l, r))))

    def greaterThan: Const[Semantic, Getter[(Int, Int), Boolean]] =
      Const(Semantic(pointer = TApplyBinary(l => r => TBinary(">", l, r))))

    def equal[A] =
      Const(Semantic(pointer = TApplyBinary(l => r => TBinary("=", l, r))))

    def aflAsFl[S, A](afl: Const[Semantic, AffineFold[S, A]]) =
      Const(afl.getConst)

    def aflHori[S, A, B](
        l: Const[Semantic, AffineFold[S, A]], 
        r: Const[Semantic, AffineFold[S, B]]) =
      Const(horiCompose(l.getConst, r.getConst))

    def aflVert[S, A, B](
        l: Const[Semantic, AffineFold[S, A]], 
        r: Const[Semantic, AffineFold[A, B]]) =
      Const(vertCompose(l.getConst, r.getConst))

    def first[A, B] =
      Const(Semantic(pointer = TApplyBinary(l => _ => l)))

    def second[A, B] =
      Const(Semantic(pointer = TApplyBinary(_ => r => r)))

    def likeInt[S](i: Int) =
      Const(Semantic(pointer = TLiteral(i.toString)))

    def likeBool[S](b: Boolean) =
      Const(Semantic(pointer = TLiteral(b.toString)))
    
    def likeStr[S](s: String) =
      Const(Semantic(pointer = TLiteral(s""""$s"""")))

    def id[S] =
      Const(Semantic(pointer = TApplyUnary(identity)))

    def not =
      Const(Semantic(pointer = TApplyUnary(t => TUnary("NOT", t))))

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

  sealed abstract class TSemantic[E[_], A] 

  type Table = Set[(String, Any)]

  type Rewrite = (String, String)

  private def unifyRewritings(t1: Table, t2: Table): List[Rewrite \/ Rewrite] = 
    (t1 ++ t2).groupBy(_._2).values.filter(_.length > 1)
      .foldLeft(List.empty[Rewrite \/ Rewrite])(
        (acc, s) => acc ++ List(\/-(s.last._1 -> s.head._1)))

  private def diffRewritings(t1: Table, t2: Table): List[Rewrite \/ Rewrite] = 
    (t1 ++ t2).groupBy(_._1).values.filter(_.length > 1)
      .foldLeft(List.empty[Rewrite \/ Rewrite])(
        (acc, s) => acc ++ List(
          -\/(s.head._1 -> (s.head._1 ++ "1")), 
          \/-(s.last._1 -> (s.last._1 ++ "2"))))

  private def getRewritings(t1: Table, t2: Table): List[Rewrite \/ Rewrite] =
    unifyRewritings(t1, t2) ++ diffRewritings(t1, t2)
  
  private def rewriteExpr[E[_], S, A](
      e: TExpr[E, S, A], rw: Rewrite): TExpr[E, S, A] = e match {
    case Product(l, r, is) => Product(rewriteExpr(l, rw), rewriteExpr(r, rw), is)
    case Vertical(u, d) => Vertical(rewriteExpr(u, rw), rewriteExpr(d, rw))
    case Var(x) if x == rw._1 => Var(rw._2)
    case _ => e
  }

  private def rewriteTable(t: Table, rw: Rewrite): Table =
    t.map { 
      case (v, e) if v == rw._1 => (rw._2, e)
      case x => x
    }

  case class TGetter[E[_], S, A](
    vars: Table = Set.empty[(String, Any)],
    expr: TExpr[E, S, A]) extends TSemantic[E, Getter[S, A]]

  sealed abstract class TExpr[E[_], S, A]

  case class Product[E[_], S, A, B, C](
      l: TExpr[E, S, A],
      r: TExpr[E, S, B],
      is: (A, B) === C)
    extends TExpr[E, S, C]

  case class Vertical[E[_], S, A, B](
      u: TExpr[E, S, A],
      d: TExpr[E, A, B])
    extends TExpr[E, S, B]

  case class Var[E[_], S, A](name: String) extends TExpr[E, S, A]

  case class Wrap[E[_], S, A](e: E[Getter[S, A]]) extends TExpr[E, S, A]

  implicit class TableOps[E[_]](table: Table) {

    def getV[S, A](v: Var[E, S, A]): E[Getter[S, A]] = 
      table.toMap.apply(v.name).asInstanceOf[E[Getter[S, A]]]

    def deleteV[S, A](v: Var[E, S, A]): Table =
      (table.toMap - v.name).toSet
  }

  implicit def tsemantic[E[_]: OpticLang] = new OpticLang[TSemantic[E, ?]] {

    def flVert[S, A, B](
      l: TSemantic[E, Fold[S, A]], 
      r: TSemantic[E, Fold[A, B]]): TSemantic[E, Fold[S, B]] = ???

    def flHori[S, A, B](
      l: TSemantic[E, Fold[S, A]], 
      r: TSemantic[E, Fold[S, B]]): TSemantic[E, Fold[S, (A, B)]] = ???

    def gtVert[S, A, B](
        l: TSemantic[E, Getter[S, A]], 
        r: TSemantic[E, Getter[A, B]]) = {
      val TGetter(vars1, expr1) = l
      val TGetter(vars2, expr2) = r
      (expr1, expr2) match {
        case (x@Var(_), y@Var(s)) => {
          val e = OpticLang[E].gtVert(vars1.getV(x), vars2.getV(y))
          TGetter((vars1.deleteV(x) ++ vars2.deleteV(y)) + (s -> e), Var(s))
        }
        case (Vertical(prev, x@Var(_)), y@Var(s)) => {
          val e = OpticLang[E].gtVert(vars1.getV(x), vars2.getV(y))
          TGetter(
            (vars1.deleteV(x) ++ vars2.deleteV(y)) + (s -> e), 
            Vertical(prev, Var(s)))
        }
        case _ => TGetter(vars1 ++ vars2, Vertical(expr1, expr2))
      }
    }

    def gtHori[S, A, B](
        l: TSemantic[E, Getter[S, A]],
        r: TSemantic[E, Getter[S, B]]): TSemantic[E, Getter[S, (A, B)]] = {
      val TGetter(vars1, expr1) = l
      val TGetter(vars2, expr2) = r
      val rws = getRewritings(vars1, vars2)
      val (vars3, vars4, expr3, expr4) = 
        rws.foldLeft((vars1, vars2, expr1, expr2)) {
          case ((vl, vr, el, er), -\/(rw)) => 
            (rewriteTable(vl, rw), vr, rewriteExpr(el, rw), er)
          case ((vl, vr, el, er), \/-(rw)) =>
            (vl, rewriteTable(vr, rw), el, rewriteExpr(er, rw))
        }
      TGetter(vars3 ++ vars4, Product(expr3, expr4, Leibniz.refl))
    }

    def aflVert[S, A, B](
      l: TSemantic[E, AffineFold[S, A]], 
      r: TSemantic[E, AffineFold[A, B]]): TSemantic[E, AffineFold[S, B]] = ???

    def aflHori[S, A, B](
      l: TSemantic[E, AffineFold[S, A]],
      r: TSemantic[E, AffineFold[S, B]]): TSemantic[E, AffineFold[S, (A, B)]] = ???

    def filtered[S](p: TSemantic[E, Getter[S, Boolean]]): TSemantic[E, AffineFold[S, S]] = ???

    def sub: TSemantic[E, Getter[(Int, Int), Int]] =
      TGetter(expr = Wrap(OpticLang[E].sub))

    def greaterThan: TSemantic[E, Getter[(Int, Int), Boolean]] = ???

    def equal[A]: TSemantic[E, Getter[(A, A), Boolean]] = ???

    def first[A, B]: TSemantic[E, Getter[(A, B), A]] =
      TGetter(expr = Wrap(OpticLang[E].first))

    def second[A, B]: TSemantic[E, Getter[(A, B), B]] =
      TGetter(expr = Wrap(OpticLang[E].second))

    def likeInt[S](i: Int): TSemantic[E, Getter[S, Int]] =
      ???

    def likeBool[S](b: Boolean): TSemantic[E, Getter[S, Boolean]] =
      ???

    def likeStr[S](s: String): TSemantic[E, Getter[S, String]] =
      ???

    def id[S]: TSemantic[E, Getter[S, S]] =
      ???

    def not: TSemantic[E, Getter[Boolean, Boolean]] =
      ???

    def getAll[S, A](fl: TSemantic[E, Fold[S, A]]): TSemantic[E, S => List[A]] = ???

    def lnAsGt[S, A](ln: TSemantic[E, Lens[S, A]]): TSemantic[E, Getter[S, A]] = ???

    def gtAsFl1[S, A](gt: TSemantic[E, Getter[S, A]]): TSemantic[E, Fold1[S, A]] = ???

    def gtAsAfl[S, A](gt: TSemantic[E, Getter[S, A]]): TSemantic[E, AffineFold[S, A]] = ???

    def aflAsFl[S, A](afl: TSemantic[E, AffineFold[S, A]]): TSemantic[E, Fold[S, A]] = ???

    def fl1AsFl[S, A](fl1: TSemantic[E, Fold1[S, A]]): TSemantic[E, Fold[S, A]] = ???
  }

  trait Syntax {

    implicit class FoldOps[Expr[_], S, A](
        l: Expr[Fold[S, A]])(implicit 
        O: OpticLang[Expr]) {
      def >[B](r: Expr[Fold[A, B]]): Expr[Fold[S, B]] = O.flVert(l, r)
      def *[B](r: Expr[Fold[S, B]]): Expr[Fold[S, (A, B)]] = O.flHori(l, r)
    }

    implicit class Fold1Ops[Expr[_], S, A](
        l: Expr[Fold1[S, A]])(implicit
        O: OpticLang[Expr]) {
      def asFold: Expr[Fold[S, A]] = O.fl1AsFl(l)
    }

    implicit class AffineFoldOps[Expr[_], S, A](
        l: Expr[AffineFold[S, A]])(implicit
        O: OpticLang[Expr]) {
      def asFold: Expr[Fold[S, A]] = O.aflAsFl(l)
      def >[B](r: Expr[AffineFold[A, B]]): Expr[AffineFold[S, B]] = 
        O.aflVert(l, r)
      def *[B](r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, (A, B)]] = 
        O.aflHori(l, r)
      def <*[B](r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, A]] = 
        O.aflVert(O.aflHori(l, r), O.gtAsAfl(O.first[A, B]))
      def *>[B](r: Expr[AffineFold[S, B]]): Expr[AffineFold[S, B]] = 
        O.aflVert(O.aflHori(l, r), O.gtAsAfl(O.second[A, B]))
    }

    implicit class GetterOps[Expr[_], S, A](
        l: Expr[Getter[S, A]])(implicit
        O: OpticLang[Expr]) {
      def asFold1: Expr[Fold1[S, A]] = O.gtAsFl1(l)
      def asAffineFold: Expr[AffineFold[S, A]] = O.gtAsAfl(l)
      def asFold: Expr[Fold[S, A]] = O.fl1AsFl(O.gtAsFl1(l))
      def >[B](r: Expr[Getter[A, B]]): Expr[Getter[S, B]] = O.gtVert(l, r)
      def *[B](r: Expr[Getter[S, B]]): Expr[Getter[S, (A, B)]] = O.gtHori(l, r)
    }

    implicit class IntGetterOps[Expr[_], S](
        l: Expr[Getter[S, Int]])(implicit
        O: OpticLang[Expr]) {
      def -(r: Expr[Getter[S, Int]]): Expr[Getter[S, Int]] =
        O.gtVert(O.gtHori(l, r), O.sub)
    }
  }

  object syntax extends Syntax
}

