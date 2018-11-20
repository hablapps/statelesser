Require Import Program.Basics.
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Coq.Init.Specif.
Require Import Coq.Lists.ListSet.
Require Import Coq.Lists.List.
Require Import Coq.Arith.PeanoNat.
Require Import Coq.Init.Logic.
Require Import Coq.Bool.Bool.
Require Import Coq.Logic.FunctionalExtensionality.
Require Export Koky.
Require Import Optic.

Open Scope program_scope.

Generalizable All Variables.

(******************************)
(* Finally, an optic language *)
(******************************)

Class OpticLang (expr : Type -> Type) :=
{ trAsFold : forall {S A}, expr (Traversal S A) -> expr (Fold S A)
; getAll : forall {S A}, expr (Fold S A) -> expr (S -> list A)
}.

(***********)
(* Example *)
(***********)

Class PeopleModel (expr : Type -> Type) `{OpticLang expr} :=
{ Person : Type
; People : Type
; name : expr (Lens Person string)
; age : expr (Lens Person nat)
; people : expr (Traversal People Person)
}.

Definition getPeople `{PeopleModel expr} : expr (People -> list Person) :=
  getAll (trAsFold people).

(******************)
(* Interpretation *)
(******************)

Record Const (A B : Type) := mkConst
{ unConst : A }.

Arguments mkConst [A B].
Arguments unConst [A B].

(* Semantic *)

(* name, source *)
Definition OpticInfo : Type := string * string.

Definition Vars : Type := ListSet.set (string * OpticInfo).

Definition Select : Type := list string.

Record Semantic := mkSemantic
{ var : Vars
; select : Select 
}.

Instance semanticOpticLang : OpticLang (Const Semantic) :=
{ trAsFold S A tr := mkConst (unConst tr)
; getAll S A fl := mkConst (unConst fl) 
}.

Lemma product_dec :
  forall (x y z : string), y = z <-> (x, y) = (x, z).
Proof.
  intros.
  split.
  - now apply f_equal.
  - intros.
    now apply (@f_equal (string * string) string snd (x, y) (x, z)).
Qed.

Lemma product_dec' :
  forall (x1 x2 y1 y2 : string), x1 = x2 /\ y1 = y2 -> (x1, y1) = (x2, y2).
Proof.
  intros.
  destruct H.
  now rewrite H, H0.
Qed.

Lemma product_dec'' :
  forall (x1 x2 y1 y2 : string), x1 <> x2 -> (x1, y1) <> (x2, y2).
Proof.
  intros.
  pose proof string_dec y1 y2.
Admitted.

Definition tuple2str_dec :
  forall (x y : string * string), {x = y} + {x <> y}.
Proof.
  intros.
  destruct x, y.
  pose proof string_dec s s1.
  pose proof string_dec s0 s2.
  unfold not in *.
  destruct H, H0.
  - rewrite e, e0.
    now left.
  - rewrite <- e.
    right.
    now rewrite <- product_dec.
  - rewrite <- e.
    right.
    admit.
  - 
Qed.

Definition tuple3str_dec : 
  forall (x y : string * (string * string)), {x = y} + {x <> y}.
Proof. 
  intros.
  destruct x, y.
  destruct p, p0.
  pose proof (string_dec s s0).
  pose proof (string_dec s1 s3).
  pose proof (string_dec s2 s4).
  destruct H.
  - rewrite e.
    destruct H0.
    + rewrite e0.
      destruct H1.
      * rewrite e1.
        auto.
      * rewrite n.
Qed.

Instance semanticPeopleModel : PeopleModel (Const Semantic) :=
{ Person := unit
; People := unit
; name := mkConst (mkSemantic (empty_set _) ("name" :: nil))
; age := mkConst (mkSemantic (empty_set _) ("age" :: nil))
; people := mkConst (mkSemantic 
    (set_add tuple3str_dec ("p", ("people", "Person")) (empty_set _)) 
    ("p" :: nil))
}.

Definition semanticToSql : Semantic -> string :=
 fun _ => "".

Definition xyz : Const Semantic (People -> list Person) :=
  @getPeople (Const Semantic) semanticOpticLang semanticPeopleModel.

(* Pretty Printing *)

Instance prettyOpticLang : OpticLang (Const string) :=
{ each A := mkConst "each"
; trAsFold S A tr := mkConst ("asFold (" ++ unConst tr ++ ")")
; getAll S A fl := mkConst ("getAll (" ++ unConst fl ++ ")")
}.

Compute unConst (@getPeople _ prettyOpticLang).

