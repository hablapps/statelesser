Require Import Program.Basics.
Require Import Coq.Strings.String.
Require Import Coq.Init.Specif.
Require Import Coq.Vectors.VectorDef.
Require Import Coq.Arith.PeanoNat.
Require Import Coq.Init.Logic.

Open Scope program_scope.

(* Plain optics *)

Record Lens S A := mkLens
{ get : S -> A
; put : S -> A -> S
}.

Arguments mkLens [S A].

Definition idLn {S : Type} : Lens S S :=
  mkLens id (fun _ s => s).

Definition fstLn {A B : Type} : Lens (A * B) A :=
  mkLens fst (fun ab a => (a, snd ab)).

Definition sndLn {A B : Type} : Lens (A * B) B :=
  mkLens snd (fun ab b => (fst ab, b)).

Class Monoid (M : Type) :=
{ mempty : M
; mappend : M -> M -> M
}.

Record Fold (S A : Type) := mkFold 
{ foldMap : forall M `{Monoid M}, (A -> M) -> S -> M }.

Arguments mkFold [S A].

Definition idFl {S : Type} : Fold S S :=
  mkFold (fun M _ f s => f s).

Definition result S A (n : nat) : Type := 
  t A n * (t A n -> S).

Record Traversal S A := mkTraversal
{ extract : S -> sigT (result S A) }.

Arguments mkTraversal [S A].

Check existT.
Check cons.

Definition idTr {S : Type} : Traversal S S :=
  mkTraversal (fun s => existT (result S S) 1 (cons S s 0 (nil S), hd)).

(* Finally, an optic language *)

Class OpticLang (expr obs : Type -> Type) :=
{ point : forall A, A -> expr A

  (* lens-related primitives *)
; lens : forall {S A : Type}, Lens S A -> expr (Lens S A)
; lnAsTraversal : forall {S A : Type}, expr (Lens S A) -> expr (Traversal S A)
; lnComposeHoriz : forall {S A B : Type},
    expr (Lens S A) -> expr (Lens S B) -> expr (Lens S (A * B))
; lnComposeVerti : forall {S A B : Type},
    expr (Lens S A) -> expr (Lens A B) -> expr (Lens S B)

  (* traversal-related primitives *)
; traversal : forall {S A : Type}, Traversal S A -> expr (Traversal S A)
; trAsFold : forall {S A : Type}, expr (Traversal S A) -> expr (Fold S A)
; trComposeHoriz : forall {S A B : Type},
    expr (Traversal S A) -> expr (Traversal S B) -> expr (Traversal S (A * B))
; trComposeVerti : forall {S A B : Type},
    expr (Traversal S A) -> expr (Traversal A B) -> expr (Traversal S B)

  (* fold-related primitives *)
; fold : forall {S A : Type}, Fold S A -> expr (Fold S A)
; filter : forall {S : Type}, (S -> Prop) -> expr (Fold S S)
; flComposeHoriz : forall {S A B : Type},
    expr (Fold S A) -> expr (Fold S B) -> expr (Fold S (A * B))
; flComposeVerti : forall {S A B : Type},
    expr (Fold S A) -> expr (Fold A B) -> expr (Fold S B)
; flGetAll : forall {S A : Type},
    expr (Fold S A) -> obs (S -> list A)
}.

Notation "ln1 +_ln ln2" := (lnComposeVerti ln1 ln2) (at level 50, left associativity).
Notation "ln1 *_ln ln2" := (lnComposeHoriz ln1 ln2) (at level 40, left associativity).

Notation "tr1 +_tr tr2" := (trComposeVerti tr1 tr2) (at level 50, left associativity).
Notation "tr1 *_tr tr2" := (trComposeHoriz tr1 tr2) (at level 40, left associativity).

Notation "fl1 +_fl fl2" := (flComposeVerti fl1 fl2) (at level 50, left associativity).
Notation "fl1 *_fl fl2" := (flComposeHoriz fl1 fl2) (at level 40, left associativity).

Class OpticLangOpt (expr obs : Type -> Type) `{OpticLang expr obs} :=
{ flLeftId : forall S A (fl : expr (Fold S A)),
    fold idFl +_fl fl = fl
; flRightId : forall S A (fl : expr (Fold S A)),
    fl +_fl fold idFl = fl
; flAssocV : forall S A B C (fl1 : expr (Fold S A)) (fl2 : expr (Fold A B)) (fl3 : expr (Fold B C)),
    fl1 +_fl (fl2 +_fl fl3) = fl1 +_fl fl2 +_fl fl3
; trAsFoldDistH : forall S A B (tr1 : expr (Traversal S A)) (tr2 : expr (Traversal S B)),
    trAsFold (tr1 *_tr tr2) = trAsFold tr1 *_fl trAsFold tr2
; trAsFoldDistV : forall S A B (tr1 : expr (Traversal S A)) (tr2 : expr (Traversal A B)),
    trAsFold (tr1 +_tr tr2) = trAsFold tr1 +_fl trAsFold tr2
; lnAsTravDistH : forall S A B (ln1 : expr (Lens S A)) (ln2 : expr (Lens S B)),
    lnAsTraversal (ln1 *_ln ln2) = lnAsTraversal ln1 *_tr lnAsTraversal ln2
; lnAsTravDistV : forall S A B (ln1 : expr (Lens S A)) (ln2 : expr (Lens A B)),
    lnAsTraversal (ln1 +_ln ln2) = lnAsTraversal ln1 +_tr lnAsTraversal ln2
; filterFilter : forall S (p q : S -> Prop),
    filter p +_fl filter q = filter (fun s => p s /\ q s)
; filterTrue : forall S, @filter _ _ _ S (fun _ => True) = fold idFl
; filterProd : forall S A B (fl1 : expr (Fold S A)) (fl2 : expr (Fold S B)) (p : A -> Prop),
    (fl1 +_fl filter p) *_fl fl2 = fl1 *_fl fl2 +_fl filter (p ∘ fst)
; vertDistHoriz :
    forall S A B C (fl1 : expr (Fold S A)) (fl2 : expr (Fold A B)) (fl3 : expr (Fold A C)),
      fl1 +_fl (fl2 *_fl fl3) = (fl1 +_fl fl2) *_fl (fl1 +_fl fl3)
; liftIdLn : forall S, lnAsTraversal (lens (@idLn S)) = traversal idTr
; liftIdTr : forall S, trAsFold (traversal (@idTr S)) = fold idFl
}.

(* Couples example *)

Record Person := mkPerson
{ name: string
; age: nat
}.

Record Couple := mkCouple
{ her: Person
; him: Person
}.

Definition nameLn : Lens Person string. Proof. Admitted.
Definition ageLn : Lens Person nat. Proof. Admitted.
Definition herLn : Lens Couple Person. Proof. Admitted.
Definition himLn : Lens Couple Person. Proof. Admitted.
Definition peopleTr : Traversal (list Person) Person. Proof. Admitted.
Definition couplesTr : Traversal (list Couple) Couple. Proof. Admitted.

Generalizable All Variables.

(* Query [getPeople], already normalized *)

Definition getPeople `{OpticLang expr obs} : obs (list Person -> list Person) :=
  flGetAll (trAsFold (traversal peopleTr)).

(* Query [getName] *)

Definition getName `{OpticLang expr obs} : obs (list Person -> list string) :=
  flGetAll (trAsFold (traversal peopleTr +_tr lnAsTraversal (lens nameLn))).

Definition getNameN `{OpticLang expr obs} : obs (list Person -> list string) :=
  flGetAll (trAsFold (traversal peopleTr) +_fl trAsFold (lnAsTraversal (lens nameLn))).

Example normalize_getName : forall expr obs `{OpticLangOpt expr obs},
  getName = getNameN.
Proof.
  intros.
  destruct H0.
  unfold getName, getNameN.
  congruence.
Qed.

(* Query [getAgeAndName] *)

Definition getAgeAndName `{OpticLang expr obs} : obs (list Person -> list (nat * string)) :=
  flGetAll (trAsFold (traversal peopleTr +_tr lnAsTraversal (lens ageLn *_ln lens nameLn))).

Definition getAgeAndNameN `{OpticLang expr obs} : obs (list Person -> list (nat * string)) :=
  flGetAll ((trAsFold (traversal peopleTr) +_fl trAsFold (lnAsTraversal (lens ageLn))) *_fl
           (trAsFold (traversal peopleTr) +_fl trAsFold (lnAsTraversal (lens nameLn)))).

Example normalize_getAgeAndName : forall expr obs `{OpticLangOpt expr obs},
  getAgeAndName = getAgeAndNameN.
Proof.
  intros.
  destruct H0.
  unfold getAgeAndName, getAgeAndNameN.
  congruence.
Qed.

(* Query [getHerAges] *)

Definition getHerAges `{OpticLang expr obs} : obs (list Couple -> list nat) :=
  flGetAll (trAsFold (traversal couplesTr +_tr lnAsTraversal (lens herLn +_ln lens ageLn))).

Definition getHerAgesN `{OpticLang expr obs} : obs (list Couple -> list nat) :=
  flGetAll (trAsFold (traversal couplesTr) +_fl 
           trAsFold (lnAsTraversal (lens herLn)) +_fl 
           trAsFold (lnAsTraversal (lens ageLn))).

Example normalize_getHerAges : forall expr obs `{OpticLangOpt expr obs},
  getHerAges = getHerAgesN.
Proof.
  intros.
  destruct H0.
  unfold getHerAges, getHerAgesN.
  congruence.
Qed.

(* Query [getPeopleOnTheirThirties] *)

Definition getPeopleOnTheirThirties `{OpticLang expr obs} : obs (list Person -> list Person) :=
  flGetAll (trAsFold (traversal peopleTr +_tr lnAsTraversal (lens idLn *_ln lens ageLn)) +_fl
            filter ((fun a => (30 <= a) /\ (a < 40)) ∘ snd) +_fl
            trAsFold (lnAsTraversal (lens fstLn))).

Definition getPeopleOnTheirThirtiesN `{OpticLang expr obs} : obs (list Person -> list Person) :=
  flGetAll (trAsFold (traversal peopleTr) *_fl 
             (trAsFold (traversal peopleTr) +_fl trAsFold (lnAsTraversal (lens ageLn))) +_fl
            filter ((fun a => (30 <= a) /\ (a < 40)) ∘ snd) +_fl
            trAsFold (lnAsTraversal (lens fstLn))).

Example normalize_getPeopleOnTheirThirties : forall expr obs `{OpticLangOpt expr obs},
  getPeopleOnTheirThirties = getPeopleOnTheirThirtiesN.
Proof.
  intros.
  destruct H0.
  unfold getPeopleOnTheirThirties, getPeopleOnTheirThirtiesN.
  congruence.
Qed.
