Require Import Coq.Program.Basics.
Require Import Coq.Strings.String.
Require Import Coq.Bool.Bool.
Require Import Coq.Lists.List.

Generalizable All Variables.

Open Scope program_scope.

Definition fork {S A B} (f : S -> A) (g : S -> B) : S -> A * B :=
  fun s => (f s, g s).

Inductive listCart (A : Type) : Type :=
| wrap : list A -> listCart A.

Arguments wrap [A].

Definition unwrap {A} (w : listCart A) : list A :=
  match w with | wrap xs => xs end.

Inductive boolOr : Type :=
| wrapBool : bool -> boolOr.

Definition unwrapBool (bo : boolOr) : bool :=
  match bo with | wrapBool b => b end.

Class Eq (A : Type) :=
{ eqb : A -> A -> bool }.

Notation "a1 == a2" := (eqb a1 a2) (at level 40, left associativity).

Instance stringEq : Eq string :=
{ eqb s t := if string_dec s t then true else false }.

Class Semigroup (M : Type) :=
{ mappend : M -> M -> M }.

Class Monoid (M : Type) `{Semigroup M} :=
{ mempty : M }.

Instance boolSemigroup : Semigroup bool :=
{ mappend m1 m2 := m1 && m2 }.

Instance boolMonoid : Monoid bool :=
{ mempty := true }.

Instance boolOrSemigroup : Semigroup boolOr :=
{ mappend m1 m2 := wrapBool (unwrapBool m1 || unwrapBool m2) }.

Instance boolOrMonoid : Monoid boolOr :=
{ mempty := wrapBool false }.

Instance listSemigroup {A : Type} : Semigroup (list A) :=
{ mappend m1 m2 := m1 ++ m2 }.

Instance listMonoid {A : Type} : Monoid (list A) :=
{ mempty := List.nil }.

Instance listCartSemigroup {A : Type} : Semigroup (listCart A) :=
{ mappend m1 m2 := wrap (mappend (unwrap m1) (unwrap m2)) }.

Instance listCartMonoid {A : Type} : Monoid (listCart A) :=
{ mempty := wrap (List.nil) }.

Class Foldable (F : Type -> Type) :=
{ fold : forall {A} `{Monoid M}, (A -> M) -> F A -> M }.

Instance listFoldable : Foldable list :=
{ fold := fun _ _ _ _ f => List.fold_right (mappend ∘ f) mempty }.

Definition option_fold {A B} (some : A -> B) (none : B) (oa : option A) : B :=
  match oa with
  | Some a => some a
  | None => none
end.

Instance optionFoldable : Foldable option :=
{ fold := fun _ _ _ _ f s => option_fold f mempty s}.

Inductive nel (A : Type) : Type :=
| nel_nil : A -> nel A
| nel_cons : A -> nel A -> nel A.

Arguments nel_nil [A].
Arguments nel_cons [A].

Fixpoint nel_fold {A B} (f : A -> B -> B) (g : A -> B) (xs : nel A) : B :=
  match xs with
  | nel_nil a => g a
  | nel_cons a xs' => f a (nel_fold f g xs')
  end.

Fixpoint nel_combine {A B} (xs : nel A) (ys : nel B) : nel (A * B) :=
  match (xs, ys) with
  | (nel_nil a, nel_nil b) => nel_nil (a,  b)
  | (nel_nil a, nel_cons b _) => nel_nil (a, b)
  | (nel_cons a _, nel_nil b) => nel_nil (a, b)
  | (nel_cons a xs', nel_cons b ys') => nel_cons (a, b) (nel_combine xs' ys')
  end.

Fixpoint nel_cat {A} (xs : nel A) (ys : nel A) : nel A :=
  nel_fold (fun a b => nel_cons a b) (fun a => nel_cons a ys) xs.

Require Import Coq.Vectors.VectorDef.

Fixpoint vec1ToNelAux {A n} (v : t A n) (prev : A) : nel A :=
  match v with
  | nil _ => nel_nil prev
  | cons _ nprev _ v2 => nel_cons prev (vec1ToNelAux v2 nprev)
  end.

Definition vec1ToNel {A n} (v : t A (S n)) : nel A :=
  vec1ToNelAux (tl v) (hd v).

Instance nelSemigroup {A : Type} : Semigroup (nel A) :=
{ mappend m1 m2 := nel_cat m1 m2 }.

Class Foldable1 (F : Type -> Type) :=
{ fold1 : forall {A} `{Semigroup M}, (A -> M) -> F A -> M }.

Instance nelFoldable1 : Foldable1 nel :=
{ fold1 := fun _ _ _ f s => nel_fold (mappend ∘ f) f s }.

Class Applicative (M : Type -> Type) : Type :=
{ pure : forall {A}, A -> M A
; ap : forall {A B}, M A -> M (A -> B) -> M B
; tupled {A B} (ma : M A) (mb : M B) : M (prod A B) :=
    ap mb (ap ma (pure pair))
}.

Instance listApplicative : Applicative list :=
{ pure := fun _ a => List.cons a List.nil
; ap := fun _ _ la lf => 
    List.map (fun fa => match fa with | (a, f) => f a end) (List.combine la lf)
}.

Instance listCartFoldable : Foldable listCart :=
{ fold := fun _ _ _ _ f s => fold f (unwrap s) }.

Instance listCartApplicative : Applicative listCart :=
{ pure := fun _ a => wrap (pure a)
; ap := fun A B la lf =>
    let f := fun pair => match pair with | (a, f) => f a end in
    wrap (List.map f (list_prod (unwrap la) (unwrap lf)))
}.

Instance optionApplicative : Applicative option :=
{ pure := fun _ => Some
; ap := fun _ _ oa og => match (oa, og) with
    | (Some a, Some g) => Some (g a)
    | _ => None
    end
}.

Class Monad (M : Type -> Type) `{Applicative M} : Type :=
{ bind : forall {A B}, M A -> (A -> M B) -> M B
}.

Notation "ma >>= f" := (bind ma f) (at level 40, left associativity).

Instance optionMonad : Monad option :=
{ bind := fun _ _ oa f => match oa with | Some a => f a | None => None end 
}.

