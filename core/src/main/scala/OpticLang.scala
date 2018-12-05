package statelesser

import scalaz._, Scalaz._

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

  def like[S, A](a: A): Expr[Getter[S, A]]

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
    gtVert(gtHori(id, like(i)), greaterThan)

  def eq[A](a: A): Expr[Getter[A, Boolean]] =
    gtVert(gtHori(id[A], like[A, A](a)), equal[A])

  // def contains[S, A](fl: Expr[Fold[S, A]], a: A): Expr[Getter[S, Boolean]] =
  //   exists(fl, eq(a))

  // def all[S, A](fl: Expr[Fold[S, A]], p: Expr[Getter[A, Boolean]]) =
  //   gtVert(exists(fl, gtVert(p, not)), not)
}

object OpticLang {

  def apply[E[_]](implicit ev: OpticLang[E]): OpticLang[E] = ev

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
      Const(s"${l.getConst} * ${r.getConst}")

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

    def like[S, A](a: A): Const[String, Getter[S, A]] =
      Const("like")

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

    def like[S, A](a: A) =
      Const(Semantic(pointer = TLiteral(a.toString)))

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
  case class TGetter[E[_], S, A](
    gt: Getter[S, A] \/ E[Getter[S, A]]) extends TSemantic[E, Getter[S, A]]

  implicit def tsemantic[E[_]: OpticLang] = new OpticLang[TSemantic[E, ?]] {

    def flVert[S, A, B](
      l: TSemantic[E, Fold[S, A]], 
      r: TSemantic[E, Fold[A, B]]): TSemantic[E, Fold[S, B]] = ???

    def flHori[S, A, B](
      l: TSemantic[E, Fold[S, A]], 
      r: TSemantic[E, Fold[S, B]]): TSemantic[E, Fold[S, (A, B)]] = ???

    def gtVert[S, A, B](
        l: TSemantic[E, Getter[S, A]], 
        r: TSemantic[E, Getter[A, B]]): TSemantic[E, Getter[S, B]] = 
      (l, r) match {
        case (TGetter(-\/(gt)), x) if gt == Getter.id => 
          x.asInstanceOf[TSemantic[E, Getter[S, B]]]
        case (x, TGetter(-\/(gt))) if gt == Getter.id =>
          x.asInstanceOf[TSemantic[E, Getter[S, B]]]
      }

    def gtHori[S, A, B](
        l: TSemantic[E, Getter[S, A]],
        r: TSemantic[E, Getter[S, B]]): TSemantic[E, Getter[S, (A, B)]] = {
      (l, r) match {
        case (TGetter(-\/(gt1)), TGetter(-\/(gt2))) 
            if gt1 == Getter.first && gt2 == Getter.second => 
          TGetter(Getter.id[S].asInstanceOf[Getter[S, (A, B)]].left)
      }
    }

    def aflVert[S, A, B](
      l: TSemantic[E, AffineFold[S, A]], 
      r: TSemantic[E, AffineFold[A, B]]): TSemantic[E, AffineFold[S, B]] = ???

    def aflHori[S, A, B](
      l: TSemantic[E, AffineFold[S, A]],
      r: TSemantic[E, AffineFold[S, B]]): TSemantic[E, AffineFold[S, (A, B)]] = ???

    def filtered[S](p: TSemantic[E, Getter[S, Boolean]]): TSemantic[E, AffineFold[S, S]] = ???

    def sub: TSemantic[E, Getter[(Int, Int), Int]] =
      TGetter(Getter.sub.left)

    def greaterThan: TSemantic[E, Getter[(Int, Int), Boolean]] = ???

    def equal[A]: TSemantic[E, Getter[(A, A), Boolean]] = ???

    def first[A, B]: TSemantic[E, Getter[(A, B), A]] =
      TGetter(Getter.first.left)

    def second[A, B]: TSemantic[E, Getter[(A, B), B]] =
      TGetter(Getter.second.left)

    def like[S, A](a: A): TSemantic[E, Getter[S, A]] =
      TGetter(Getter.like(a).left)

    def id[S]: TSemantic[E, Getter[S, S]] =
      TGetter(Getter.id.left)

    def not: TSemantic[E, Getter[Boolean, Boolean]] =
      TGetter(Getter.not.left)

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

