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
      Const(s"${afl.getConst}.asFold")

    def fl1AsFl[S, A](
        fl1: Const[String, Fold1[S, A]]): Const[String, Fold[S, A]] =
      Const(s"${fl1.getConst}.asFold")
  }

  type TypeNme = String
  type OpticNme = String
  
  sealed abstract class OpticKind
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
  
  private def unifyRewritings[E[_], O[_, _]](
      t1: Table[E, O], t2: Table[E, O]): List[Rewrite \/ Rewrite] = 
    (t1 ++ t2).groupBy(_._2).values.filter(_.length > 1)
      .foldLeft(List.empty[Rewrite \/ Rewrite])(
        (acc, s) => acc ++ List(\/-(s.last._1 -> s.head._1)))

  private def diffRewritings[E[_], O[_, _]](
      t1: Table[E, O], t2: Table[E, O]): List[Rewrite \/ Rewrite] = 
    (t1 ++ t2).groupBy(_._1).values.filter(_.length > 1)
      .foldLeft(List.empty[Rewrite \/ Rewrite])(
        (acc, s) => acc ++ List(
          -\/(s.head._1 -> (s.head._1 ++ "1")), 
          \/-(s.last._1 -> (s.last._1 ++ "2"))))

  private def getRewritings[E[_], O[_, _]](
      t1: Table[E, O], t2: Table[E, O]): List[Rewrite \/ Rewrite] =
    unifyRewritings(t1, t2) ++ diffRewritings(t1, t2)
  
  private def rewriteExpr[E[_], O[_, _], S, A](
      e: TExpr[E, O, S, A], rw: Rewrite): TExpr[E, O, S, A] = e match {
    case Product(l, r, is, lt, rt) => Product(
      rewriteExpr(l, rw), rewriteExpr(r, rw), is, 
      rewriteTable(lt, rw), rewriteTable(rt, rw))
    case Vertical(u, d, ut, dt) => Vertical(
      rewriteExpr(u, rw), rewriteExpr(d, rw),
      rewriteTable(ut, rw), rewriteTable(dt, rw))
    case Var(x) if x == rw._1 => Var(rw._2)
    case _ => e
  }

  private def rewriteTable[E[_], O[_, _]](
      t: Table[E, O], rw: Rewrite): Table[E, O] = t.map { 
    case (v, e) if v == rw._1 => (rw._2, e)
    case x => x
  }

  sealed abstract class TSemantic[E[_], A] 

  type Row[E[_], O[_, _]] = (String, TExpr[E, O, _, _])

  type Table[E[_], O[_, _]] = Set[Row[E, O]]

  type Rewrite = (String, String)

  case class TGetter[E[_], S, A](
    vars: Table[E, Getter] = Set.empty[Row[E, Getter]],
    expr: TExpr[E, Getter, S, A]) extends TSemantic[E, Getter[S, A]]

  case class TAffineFold[E[_], S, A](
    vars: Table[E, AffineFold] = Set.empty[Row[E, AffineFold]],
    expr: TExpr[E, AffineFold, S, A]) extends TSemantic[E, AffineFold[S, A]]

  case class TFold[E[_], S, A](
    vars: Table[E, Fold] = Set.empty[Row[E, Fold]],
    expr: TExpr[E, Fold, S, A]) extends TSemantic[E, Fold[S, A]]

  trait OpticMap[E[_], O[_, _], O2[_, _]] {
    def apply[S, A](e: E[O[S, A]]): E[O2[S, A]]
  }

  sealed abstract class TExpr[E[_], O[_, _], S, A] {
    def mapO[O2[_, _]](f: OpticMap[E, O, O2]): TExpr[E, O2, S, A] = this match {
      case Product(l, r, is, lt, rt) => 
        Product(l.mapO(f), r.mapO(f), is, lt.mapO(f), rt.mapO(f))
      case Vertical(u, d, ut, dt) => 
        Vertical(u.mapO(f), d.mapO(f), ut.mapO(f), dt.mapO(f))
      case Var(name) => Var(name)
      case Wrap(e, inf) => Wrap(f(e), inf)
      case Unary(op) => Unary(op)
      case Binary(op) => Binary(op)
      case Like(s) => Like(s)
    }
  }

  case class Product[E[_], O[_, _], S, A, B, C](
      l: TExpr[E, O, S, A],
      r: TExpr[E, O, S, B],
      is: (A, B) === C,
      lt: Table[E, O],
      rt: Table[E, O])
    extends TExpr[E, O, S, C]

  case class Vertical[E[_], O[_, _], S, A, B](
      u: TExpr[E, O, S, A],
      d: TExpr[E, O, A, B],
      ut: Table[E, O],
      dt: Table[E, O])
    extends TExpr[E, O, S, B]

  case class Var[E[_], O[_, _], S, A](name: String) extends TExpr[E, O, S, A]

  case class Wrap[E[_], O[_, _], S, A](e: E[O[S, A]], info: OpticInfo) 
    extends TExpr[E, O, S, A]

  case class Unary[E[_], O[_, _], S, A](name: String) extends TExpr[E, O, S, A]

  case class Binary[E[_], O[_, _], S, A](name: String) extends TExpr[E, O, S, A]

  case class Like[E[_], O[_, _], S, A](s: String) extends TExpr[E, O, S, A]

  implicit class TableOps[E[_], O[_, _]](table: Table[E, O]) {

    def getV[S, A](v: Var[E, O, S, A]): TExpr[E, O, S, A] = 
      table.toMap.apply(v.name).asInstanceOf[TExpr[E, O, S, A]]

    def deleteV[S, A](v: Var[E, O, S, A]): Table[E, O] =
      (table.toMap - v.name).toSet

    def mapO[O2[_, _]](f: OpticMap[E, O, O2]): Table[E, O2] =
      table.map { case (s, e) => (s, e.mapO(f)) }
  }

  implicit def tsemantic[E[_]: OpticLang] = new OpticLang[TSemantic[E, ?]] {

    private def vertical[O[_, _], S, A, B](
        vars1: Table[E, O],
        vars2: Table[E, O],
        expr1: TExpr[E, O, S, A],
        expr2: TExpr[E, O, A, B]): (Table[E, O], TExpr[E, O, S, B]) = 
      (expr1, expr2) match {
        case (e, Product(l, r, is, lt, rt)) => {
          val (vars3, expr3) = vertical(vars1, lt, e, l)
          val (vars4, expr4) = vertical(vars1, rt, e, r)
          horizontal(vars3, vars4, expr3, expr4).rightMap(is.subst)
        }
        case (e, Vertical(u, d, ut, dt)) => {
          val (vars3, expr3) = vertical(vars1, ut, e, u)
          vertical(vars3, dt, expr3, d)
        }
        case (Product(l, _, _, lt, _), Wrap(_, inf)) if inf.nme == "first" => 
          (lt, l.asInstanceOf[TExpr[E, O, S, B]])
        case (Product(_, r, _, _, rt), Wrap(_, inf)) if inf.nme == "second" =>
          (rt, r.asInstanceOf[TExpr[E, O, S, B]])
        case (x@Var(_), y@Var(s)) => {
          val e = Vertical(x, vars2.getV(y), vars1, vars2)
          ((vars1 ++ vars2.deleteV(y)) + (s -> e), Var(s))
        }
        case (Vertical(prev, x@Var(_), _, _), y@Var(s)) => {
          val e = Vertical(x, vars2.getV(y), vars1, vars2)
          vertical(vars1, vars2.deleteV(y) + (s -> e), prev, Var(s))
        }
        case (Wrap(_, inf), e) if inf.nme == "id" =>
          (vars2, e.asInstanceOf[TExpr[E, O, S, B]])
        case (e, Wrap(_, inf)) if inf.nme == "id" =>
          (vars1, e.asInstanceOf[TExpr[E, O, S, B]])
        case _ => (vars1 ++ vars2, Vertical(expr1, expr2, vars1, vars2))
      }

    private def horizontal[O[_, _], S, A, B](
        vars1: Table[E, O],
        vars2: Table[E, O],
        expr1: TExpr[E, O, S, A],
        expr2: TExpr[E, O, S, B]): (Table[E, O], TExpr[E, O, S, (A, B)]) = {
      val rws = getRewritings(vars1, vars2)
      val (vars3, vars4, expr3, expr4) = 
        rws.foldLeft((vars1, vars2, expr1, expr2)) {
          case ((vl, vr, el, er), -\/(rw)) => 
            (rewriteTable(vl, rw), vr, rewriteExpr(el, rw), er)
          case ((vl, vr, el, er), \/-(rw)) =>
            (vl, rewriteTable(vr, rw), el, rewriteExpr(er, rw))
        }
      (vars3 ++ vars4, Product(expr3, expr4, Leibniz.refl, vars3, vars4))
    }

    def flVert[S, A, B](
        l: TSemantic[E, Fold[S, A]], 
        r: TSemantic[E, Fold[A, B]]) = {
      val TFold(vars1, expr1) = l
      val TFold(vars2, expr2) = r
      val (vars3, expr3) = vertical(vars1, vars2, expr1, expr2)
      TFold(vars3, expr3)
    }

    def flHori[S, A, B](
        l: TSemantic[E, Fold[S, A]], 
        r: TSemantic[E, Fold[S, B]]) = {
      val TFold(vars1, expr1) = l
      val TFold(vars2, expr2) = r
      val (vars3, expr3) = horizontal(vars1, vars2, expr1, expr2)
      TFold(vars3, expr3)
    }

    def gtVert[S, A, B](
        l: TSemantic[E, Getter[S, A]], 
        r: TSemantic[E, Getter[A, B]]) = {
      val TGetter(vars1, expr1) = l
      val TGetter(vars2, expr2) = r
      val (vars3, expr3) = vertical(vars1, vars2, expr1, expr2)
      TGetter(vars3, expr3)
    }

    def gtHori[S, A, B](
        l: TSemantic[E, Getter[S, A]],
        r: TSemantic[E, Getter[S, B]]) = {
      val TGetter(vars1, expr1) = l
      val TGetter(vars2, expr2) = r
      val (vars3, expr3) = horizontal(vars1, vars2, expr1, expr2)
      TGetter(vars3, expr3)
    }

    def aflVert[S, A, B](
        l: TSemantic[E, AffineFold[S, A]], 
        r: TSemantic[E, AffineFold[A, B]]) = {
      val TAffineFold(vars1, expr1) = l
      val TAffineFold(vars2, expr2) = r
      val (vars3, expr3) = vertical(vars1, vars2, expr1, expr2)
      TAffineFold(vars3, expr3)
    }

    def aflHori[S, A, B](
        l: TSemantic[E, AffineFold[S, A]],
        r: TSemantic[E, AffineFold[S, B]]) = {
      val TAffineFold(vars1, expr1) = l
      val TAffineFold(vars2, expr2) = r
      val (vars3, expr3) = horizontal(vars1, vars2, expr1, expr2)
      TAffineFold(vars3, expr3)
    }

    def filtered[S](p: TSemantic[E, Getter[S, Boolean]]): TSemantic[E, AffineFold[S, S]] = ???

    def sub: TSemantic[E, Getter[(Int, Int), Int]] =
      TGetter(expr = Binary("sub"))

    def greaterThan: TSemantic[E, Getter[(Int, Int), Boolean]] = ???

    def equal[A]: TSemantic[E, Getter[(A, A), Boolean]] = ???

    def first[A, B]: TSemantic[E, Getter[(A, B), A]] =
      TGetter(expr = Wrap(OpticLang[E].first,
        OpticInfo(KGetter, "first", TypeInfo("(A, B)"), TypeInfo("A"))))

    def second[A, B]: TSemantic[E, Getter[(A, B), B]] =
      TGetter(expr = Wrap(OpticLang[E].second,
        OpticInfo(KGetter, "first", TypeInfo("(A, B)"), TypeInfo("B"))))

    def likeInt[S](i: Int): TSemantic[E, Getter[S, Int]] =
      ???

    def likeBool[S](b: Boolean): TSemantic[E, Getter[S, Boolean]] =
      ???

    def likeStr[S](s: String): TSemantic[E, Getter[S, String]] =
      ???

    def id[S]: TSemantic[E, Getter[S, S]] =
      TGetter(expr = Wrap(OpticLang[E].id,
        OpticInfo(KGetter, "id", TypeInfo("S"), TypeInfo("S"))))

    def not: TSemantic[E, Getter[Boolean, Boolean]] =
      ???

    def getAll[S, A](fl: TSemantic[E, Fold[S, A]]): TSemantic[E, S => List[A]] = ???

    def lnAsGt[S, A](ln: TSemantic[E, Lens[S, A]]): TSemantic[E, Getter[S, A]] = ???

    def gtAsFl1[S, A](gt: TSemantic[E, Getter[S, A]]): TSemantic[E, Fold1[S, A]] = ???

    def gtAsAfl[S, A](gt: TSemantic[E, Getter[S, A]]) = {
      val TGetter(vars, expr) = gt
      val f = new OpticMap[E, Getter, AffineFold] {
        def apply[X, Y](gt: E[Getter[X, Y]]) = OpticLang[E].gtAsAfl(gt)
      }
      TAffineFold(vars.mapO(f), expr.mapO(f))
    }

    def aflAsFl[S, A](afl: TSemantic[E, AffineFold[S, A]]) = {
      val TAffineFold(vars, expr) = afl
      val f = new OpticMap[E, AffineFold, Fold] {
        def apply[X, Y](afl: E[AffineFold[X, Y]]) = OpticLang[E].aflAsFl(afl)
      }
      TFold(vars.mapO(f), expr.mapO(f))
    }

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
      def asFold: Expr[Fold[S, A]] = O.aflAsFl(O.gtAsAfl(l))
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

