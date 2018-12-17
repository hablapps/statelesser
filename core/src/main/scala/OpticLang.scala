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

  def gtAsFl[S, A](gt: Expr[Getter[S, A]]): Expr[Fold[S, A]] =
    aflAsFl(gtAsAfl(gt))

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

  private def rewriteSel[E[_], O[_, _], S, A](
      e: TSel[E, O, S, A], rw: Rewrite): TSel[E, O, S, A] = e match {
    case Var(x) if x == rw._1 => Var(rw._2)
    case _ => e
  }
  
  private def rewriteExpr[E[_], O[_, _], S, A](
      e: TExpr[E, O, S, A], rw: Rewrite): TExpr[E, O, S, A] = e match {
    case Product(l, r, is, lt, rt) => Product(
      rewriteExpr(l, rw), rewriteExpr(r, rw), is,
      rewriteTable(lt, rw), rewriteTable(rt, rw))
    case Vertical(u, d, ut, dt) => Vertical(
      rewriteExpr(u, rw), rewriteSel(d, rw),
      rewriteTable(ut, rw), rewriteTable(dt, rw))
    case sel: TSel[E, O, S, A] => rewriteSel(sel, rw)
  }

  private def rewriteTable[E[_], O[_, _]](
      t: Table[E, O], rw: Rewrite): Table[E, O] = t.map { 
    case (v, e) if v == rw._1 => (rw._2, e)
    case x => x
  }

  type Row[E[_], O[_, _]] = (String, TVarVal[E, O, _, _])

  type Table[E[_], O[_, _]] = Set[Row[E, O]]

  type Rewrite = (String, String)

  sealed abstract class TSemantic[E[_], A] 

  case class TGetter[E[_], S, A](
    vars: Table[E, Getter] = Set.empty[Row[E, Getter]],
    expr: TExpr[E, Getter, S, A]) extends TSemantic[E, Getter[S, A]]

  case class TAffineFold[E[_], S, A](
    vars: Table[E, AffineFold] = Set.empty[Row[E, AffineFold]],
    expr: TExpr[E, AffineFold, S, A]) extends TSemantic[E, AffineFold[S, A]]

  case class TFold[E[_], S, A](
      vars: Table[E, Fold] = Set.empty[Row[E, Fold]],
      expr: TExpr[E, Fold, S, A]) extends TSemantic[E, Fold[S, A]] {
    def reify(implicit ev: OpticLang[E]): E[Fold[S, A]] = reifyExpr(expr, vars)
  }

  trait OpticMap[E[_], O[_, _], O2[_, _]] {
    def apply[S, A](e: E[O[S, A]]): E[O2[S, A]]
  }

  def reifyVV[E[_], S, A](
      vv: TVarVal[E, Fold, S, A],
      t: Table[E, Fold])(implicit 
      ev: OpticLang[E]): E[Fold[S, A]] = vv match {
    case TVarSimpleVal(w) => w.e
    case vnv: TVarNestedVal[E, Fold, S, A] => vnv.vs match {
      case ONil(v) => ev.flVert(reifyVV(t.getV(v), t), vnv.w.e)
      case OCons(v, vs) => 
        ev.flVert(reifyVV(t.getV(v), t), reifyVV(TVarNestedVal(vs, vnv.w), t))
    }
  }

  sealed abstract class OList[E[_], O[_, _], S, A] {

    def mapO[O2[_, _]](f: OpticMap[E, O, O2]): OList[E, O2, S, A] = this match {
      case ONil(Var(x)) => ONil(Var(x))
      case OCons(Var(x), t) => OCons(Var(x), t.mapO(f))
    }

    def lastVar: Var[E, O, _, A] = this match {
      case ONil(x) => x
      case OCons(_, tail) => tail.lastVar
    }
  }

  case class ONil[E[_], O[_, _], S, A](last: Var[E, O, S, A])
    extends OList[E, O, S, A]

  case class OCons[E[_], O[_, _], S, A, B](
      head: Var[E, O, S, A], 
      tail: OList[E, O, A, B])
    extends OList[E, O, S, B]

  sealed abstract class TVarVal[E[_], O[_, _], S, A] {
    def mapO[O2[_, _]](f: OpticMap[E, O, O2]): TVarVal[E, O2, S, A] = this match {
      case TVarSimpleVal(Wrap(e, info)) => TVarSimpleVal(Wrap(f(e), info))
      case vnv: TVarNestedVal[E, O, S, A] =>  
        TVarNestedVal(vnv.vs.mapO(f), Wrap(f(vnv.w.e), vnv.w.info))
    }
  }

  case class TVarSimpleVal[E[_], O[_, _], S, A](w: Wrap[E, O, S, A])
    extends TVarVal[E, O, S, A]

  trait TVarNestedVal[E[_], O[_, _], S, B] extends TVarVal[E, O, S, B] {
    type A
    def vs: OList[E, O, S, A]
    def w: Wrap[E, O, A, B]
    override def equals(other: Any): Boolean = other match {
      case vnv: TVarNestedVal[E, O, S, _] => vs == vnv.vs && vnv.w == w
      case _ => false
    }
  }

  object TVarNestedVal {

    type Aux[E[_], O[_, _], S, A2, B] = 
      TVarNestedVal[E, O, S, B] { type A = A2 }

    def apply[E[_], O[_, _], S, A2, B](
        vs2: OList[E, O, S, A2], 
        w2: Wrap[E, O, A2, B]): Aux[E, O, S, A2, B] = 
      new TVarNestedVal[E, O, S, B] {
        type A = A2
        val vs = vs2
        val w = w2
      }
  }

  def reifyExpr[E[_], S, A](
      expr: TExpr[E, Fold, S, A],
      t: Table[E, Fold])(implicit
      ev: OpticLang[E]): E[Fold[S, A]] = expr match {
    case Product(l, r, is, lt, rt) => 
      is.subst[λ[x => E[Fold[S, x]]]](ev.flHori(reifyExpr(l, lt), reifyExpr(r, rt)))
    case Vertical(u, d, ut, dt) => ev.flVert(reifyExpr(u, ut), reifyExpr(d, dt))
    case x: Var[E, Fold, S, A] => reifyVV(t.getV(x), t)
    case w: Wrap[E, Fold, S, A] => w.e
    case LikeInt(i, is) => 
      is.flip.subst[λ[x => E[Fold[S, x]]]](ev.gtAsFl(ev.likeInt[S](i)))
    case LikeBool(b, is) =>
      is.flip.subst[λ[x => E[Fold[S, x]]]](ev.gtAsFl(ev.likeBool[S](b)))
    case Not(is1, is2) => 
      is2.flip.subst[λ[x => E[Fold[S, x]]]](
        is1.flip.subst[λ[x => E[Fold[x, Boolean]]]](ev.gtAsFl(ev.not)))
    case Sub(is1, is2) =>
      is2.flip.subst[λ[x => E[Fold[S, x]]]](
        is1.flip.subst[λ[x => E[Fold[x, Int]]]](ev.gtAsFl(ev.sub)))
  }

  sealed abstract class TExpr[E[_], O[_, _], S, A] {
    def mapO[O2[_, _]](f: OpticMap[E, O, O2]): TExpr[E, O2, S, A] = this match {
      case Product(l, r, is, lt, rt) => 
        Product(l.mapO(f), r.mapO(f), is, lt.mapO(f), rt.mapO(f))
      case Vertical(u, d, ut, dt) =>
        Vertical(u.mapO(f), d.mapOSel(f), ut.mapO(f), dt.mapO(f))
      case x: TSel[E, O, S, A] => x.mapOSel(f)
    }
  }
  case class Product[E[_], O[_, _], S, A, B, C](
    l: TExpr[E, O, S, A],
    r: TExpr[E, O, S, B],
    is: (A, B) === C,
    lt: Table[E, O],
    rt: Table[E, O]) extends TExpr[E, O, S, C]

  case class Vertical[E[_], O[_, _], S, A, B](
    u: TExpr[E, O, S, A],
    d: TSel[E, O, A, B],
    ut: Table[E, O],
    dt: Table[E, O]) extends TExpr[E, O, S, B]

  sealed abstract class TSel[E[_], O[_, _], S, A] extends TExpr[E, O, S, A] {
    def mapOSel[O2[_, _]](f: OpticMap[E, O, O2]): TSel[E, O2, S, A] = this match {
      case Var(name) => Var(name)
      case Wrap(e, inf) => Wrap(f(e), inf)
      case LikeInt(i, is) => LikeInt(i, is)
      case LikeBool(b, is) => LikeBool(b, is)
      case Sub(is1, is2) => Sub(is1, is2)
      case Not(is1, is2) => Not(is1, is2)
    }
  }

  case class Var[E[_], O[_, _], S, A](
    name: String) extends TSel[E, O, S, A]

  case class Wrap[E[_], O[_, _], S, A](
    e: E[O[S, A]],
    info: OpticInfo) extends TSel[E, O, S, A]

  case class LikeInt[E[_], O[_, _], S, A](
    i: Int,
    is: A === Int) extends TSel[E, O, S, A]

  case class LikeBool[E[_], O[_, _], S, A](
    b: Boolean,
    is: A === Boolean) extends TSel[E, O, S, A]

  case class Sub[E[_], O[_, _], S, A](
    is1: S === (Int, Int),
    is2: A === Int) extends TSel[E, O, S, A]

  case class Not[E[_], O[_, _], S, A](
    is1: S === Boolean,
    is2: A === Boolean) extends TSel[E, O, S, A]

  // XXX: we need a safer table implementation, since this is a really ugly
  // workaround, but this is good enough for the time being.
  implicit class TableOps[E[_], O[_, _]](table: Table[E, O]) {

    def getV[S, A](v: Var[E, O, S, A]): TVarVal[E, O, S, A] = 
      table.toMap.apply(v.name).asInstanceOf[TVarVal[E, O, S, A]]

    def deleteV[S, A](v: Var[E, O, S, A]): Table[E, O] =
      (table.toMap - v.name).toSet

    def mapO[O2[_, _]](f: OpticMap[E, O, O2]): Table[E, O2] =
      table.map { case (s, e) => (s, e.mapO(f)) }

    private def splitTables =
      table.partition { case (_, TVarSimpleVal(_)) => true; case _ => false }

    def simpleTable: Set[(String, TVarSimpleVal[E, O, _, _])] =
      splitTables._1.asInstanceOf[Set[(String, TVarSimpleVal[E, O, _, _])]]

    def nestedTable: Set[(String, TVarNestedVal[E, O, _, _])] =
      splitTables._2.asInstanceOf[Set[(String, TVarNestedVal[E, O, _, _])]]
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
        case (Wrap(_, inf), e) if inf.nme == "id" =>
          (vars2, e.asInstanceOf[TExpr[E, O, S, B]])
        case (e, Wrap(_, inf)) if inf.nme == "id" =>
          (vars1, e.asInstanceOf[TExpr[E, O, S, B]])
        case (_, LikeInt(i, is)) => (vars2, LikeInt(i, is))
        case (_, LikeBool(b, is)) => (vars2, LikeBool(b, is))
        case (Product(l, LikeInt(0, _), _, lt, _), Sub(_, _)) => 
          (lt, l.asInstanceOf[TExpr[E, O, S, B]])
        case (Product(LikeInt(x, _), LikeInt(y, _), _, _, _), Sub(_, is)) =>
          (vars1 ++ vars2, LikeInt(x - y, is))
        case (LikeBool(b, _), Not(_, is)) =>
          (vars1 ++ vars2, LikeBool(! b, is))
        case (Vertical(e, Not(is1, _), ut, _), Not(_, is2)) =>
          (ut, is2.flip.subst(is1.subst(e)))
        case (x@Var(_), y@Var(s)) => {
          val vv = vars2.getV(y)
          val e = vv match {
            case TVarSimpleVal(w) => TVarNestedVal(ONil(x), w)
            case vnv: TVarNestedVal[E, O, _, _] => 
              TVarNestedVal(OCons(x, vnv.vs), vnv.w)
          }
          ((vars1 ++ vars2.deleteV(y)) + (s -> e), Var(s))
        }
        case (Vertical(prev, x@Var(_), _, _), y@Var(s)) => {
          val vv = vars2.getV(y)
          val e = vv match {
            case TVarSimpleVal(w) => TVarNestedVal(ONil(x), w)
            case vnv: TVarNestedVal[E, O, _, _] => 
              TVarNestedVal(OCons(x, vnv.vs), vnv.w)
          }
          vertical(vars1, vars2.deleteV(y) + (s -> e), prev, Var(s))
        }
        case (_, e: TSel[E, O, A, B]) => 
          (vars1 ++ vars2, Vertical(expr1, e, vars1, vars2))
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
      TGetter(expr = Sub(implicitly, implicitly))

    def greaterThan: TSemantic[E, Getter[(Int, Int), Boolean]] = ???

    def equal[A]: TSemantic[E, Getter[(A, A), Boolean]] = ???

    def first[A, B]: TSemantic[E, Getter[(A, B), A]] =
      TGetter(expr = Wrap(OpticLang[E].first,
        OpticInfo(KGetter, "first", TypeInfo("(A, B)"), TypeInfo("A"))))

    def second[A, B]: TSemantic[E, Getter[(A, B), B]] =
      TGetter(expr = Wrap(OpticLang[E].second,
        OpticInfo(KGetter, "second", TypeInfo("(A, B)"), TypeInfo("B"))))

    def likeInt[S](i: Int): TSemantic[E, Getter[S, Int]] =
      TGetter(expr = LikeInt(i, implicitly))

    def likeBool[S](b: Boolean): TSemantic[E, Getter[S, Boolean]] =
      TGetter(expr = LikeBool(b, implicitly))


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

