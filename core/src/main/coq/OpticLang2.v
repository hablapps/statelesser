Require Import Program.Basics.
Require Import Coq.Strings.String.
Require Import Coq.Init.Specif.
Require Import Coq.Lists.List.
Require Import Coq.Vectors.VectorDef.
Require Import Coq.Arith.PeanoNat.
Require Import Coq.Init.Logic.
Require Import Coq.Bool.Bool.
Require Import Coq.Logic.FunctionalExtensionality.
Require Export Koky.
Require Export Optic.

Open Scope program_scope.

Generalizable All Variables.

(******************************)
(* Finally, an optic language *)
(******************************)

Class OpticLang2 (expr : Type -> Type) :=
{ each : forall {A}, expr (Traversal (list A) A)
; trAsFold : forall {S A}, expr (Traversal S A) -> expr (Fold S A)
; getAll : forall {S A}, expr (Fold S A) -> expr (S -> list A)
}.

(***********)
(* Example *)
(***********)

Record Person := mkPerson
{ name : string
; age : nat
}.

Definition People := list Person.

Definition getPeople `{OpticLang2 expr} : expr (People -> list Person) :=
  getAll (trAsFold each).

(******************)
(* Interpretation *)
(******************)

Record Const (A B : Type) := mkConst
{ unConst : A }.

Arguments mkConst [A B].
Arguments unConst [A B].

Instance prettyOpticLang2 : OpticLang2 (Const string) :=
{ each A := mkConst "each"
; trAsFold S A tr := mkConst ("asFold (" ++ unConst tr ++ ")")
; getAll S A fl := mkConst ("getAll (" ++ unConst fl ++ ")")
}.

Compute unConst (@getPeople _ prettyOpticLang2).

Record Semantic (Op1 Op2 : Type -> Type -> Type) (S A B : Type) `{OpticLang2 expr} :=
{ select : option (expr (Op1 S A))
; filter : option (expr (Op2 S B))
}.

