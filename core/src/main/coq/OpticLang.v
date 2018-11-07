Require Import Program.Basics.
Require Import Coq.Strings.String.
Require Import Coq.Init.Specif.
Require Import Coq.Lists.List.
Require Import Coq.Vectors.VectorDef.
Require Import Coq.Arith.PeanoNat.
Require Import Coq.Init.Logic.
Require Import Coq.Logic.FunctionalExtensionality.

Open Scope program_scope.

Generalizable All Variables.

(* koky *)

Class Monoid (M : Type) :=
{ mempty : M
; mappend : M -> M -> M
}.

Instance listMonoid {A : Type} : Monoid (list A) :=

{| mempty := List.nil
;  mappend m1 m2 := m1 ++ m2
|}.

(****************)
(* Plain optics *)
(****************)

Record Fold (S A : Type) := mkFold
{ foldMap `{Monoid M} : (A -> M) -> S -> M }.

Arguments mkFold [S A].
Arguments foldMap [S A].

Check foldMap.

Definition flVertCompose {S A B} (fl1 : Fold S A) (fl2 : Fold A B) : Fold S B :=
  mkFold (fun M _ f s => foldMap fl1 _ _ (foldMap fl2 _ _ f) s).

Definition flProdCompose {S A B}
    (fl1 : Fold S A) (fl2 : Fold S B) : Fold S (A * B) :=
  mkFold (fun M _ f s => foldMap fl1 _ _ (fun a => 
    foldMap fl2 _ _ (fun b => f (a, b)) s) s).

Definition flHoriCompose {S A B}
    (fl1 : Fold S A) (fl2 : Fold S B) : Fold S (prod A B) :=
  mkFold (fun M _ f s => 
    List.fold_right (mappend ∘ f) mempty (
      List.combine (foldMap fl1 _ _ (fun a => List.cons a List.nil) s) 
                   (foldMap fl2 _ _ (fun b => List.cons b List.nil) s))).

Definition idFl {S : Type} : Fold S S :=
  mkFold (fun M _ f s => f s).

Definition result S A (n : nat) : Type := 
  t A n * (t A n -> S).

Record Traversal S A := mkTraversal
{ extract : S -> sigT (result S A) }.

Arguments mkTraversal [S A].

Definition idTr {S : Type} : Traversal S S :=
  mkTraversal (fun s => existT (result S S) 1 (cons S s 0 (nil S), hd)).

Record Iso S A := mkIso
{ to : S -> A
; from : A -> S
}.

Arguments mkIso [S A].

Definition idIso {S : Type} : Iso S S :=
  mkIso id id.

Record Lens S A := mkLens
{ get : S -> A
; set : S -> A -> S
}.

Arguments mkLens [S A].

Definition idLn {S : Type} : Lens S S :=
  mkLens id (fun _ s => s).

Definition fstLn {A B : Type} : Lens (A * B) A :=
  mkLens fst (fun ab a => (a, snd ab)).

Definition sndLn {A B : Type} : Lens (A * B) B :=
  mkLens snd (fun ab b => (fst ab, b)).


Record Getter S A := mkGetter
{ view : S -> A }.

Arguments mkGetter [S A].

Definition idGt {S : Type} : Getter S S :=
  mkGetter id.

Record AffineFold S A := mkAffineFold
{ afold : S -> option A }.

(******************************)
(* Finally, an optic language *)
(******************************)

Class OpticLang (expr : Type -> Type) :=

{ (* higher-order abstract syntax (hoas) *)
  lift : forall {A : Type}, A -> expr A
; lam : forall {A B : Type}, (expr A -> expr B) -> expr (A -> B)
; app : forall {A B : Type}, expr (A -> B) -> expr A -> expr B

  (* product-related primitives *)
; curry : forall {A B C : Type}, expr (A * B -> C) -> expr (A -> B -> C)
; uncurry : forall {A B C : Type}, expr (A -> B -> C) -> expr (A * B -> C)
; product : forall {A B : Type}, expr A -> expr B -> expr (prod A B)

  (* basic types *)
; ntr : nat -> expr nat
; str : string -> expr string

 (* lens-related primitives *)
; lens : forall {S A : Type}, Lens S A -> expr (Lens S A)
; lnAsGetter : forall {S A : Type}, expr (Lens S A) -> expr (Getter S A)
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
; trComposeHorizL : forall {S A B : Type},
    expr (Traversal S A) -> expr (Traversal S B) -> expr (Traversal S (A * option B))
; trComposeHorizR : forall {S A B : Type},
    expr (Traversal S A) -> expr (Traversal S B) -> expr (Traversal S (option A * B))
; unsafeFiltered : forall {S A : Type},
    expr (Getter S A) -> expr (A -> Prop) -> expr (Traversal S S)

  (* getter-related primitives *)
; getter : forall {S A : Type}, expr (S -> A) -> expr (Getter S A)
; gtAsFold : forall {S A : Type}, expr (Getter S A) -> expr (Fold S A)
; gtComposeHoriz : forall {S A B : Type},
    expr (Getter S A) -> expr (Getter S B) -> expr (Getter S (A * B))
; gtComposeVerti : forall {S A B : Type},
    expr (Getter S A) -> expr (Getter A B) -> expr (Getter S B)

  (* propositional primitives and generic methods *)
; and : expr Prop -> expr Prop -> expr Prop
; or  : expr Prop -> expr Prop -> expr Prop
; leqt : expr nat -> expr nat -> expr Prop
; lt : expr nat -> expr nat -> expr Prop
; eq : expr string -> expr string -> expr Prop
; sub : expr (prod nat nat -> nat)
; upper : expr (string -> string)
; incr : expr (nat -> nat)
; append : forall {A : Type}, expr A -> expr (list A) -> expr (list A)
; identity {A : Type} : expr (A -> A)
; first {A B : Type} : expr (A * B -> A)
; second {A B : Type} : expr (A * B -> B)

  (* affine fold-related primitives*)
; affineFold : forall {S A : Type}, AffineFold S A -> expr (AffineFold S A)
; aflAsFold : forall {S A : Type}, expr (AffineFold S A) -> expr (Fold S A)
; afolding : forall {S A : Type}, expr (S -> option A) -> expr (AffineFold S A)
; filtered : forall {S A : Type}, expr (Getter S A) -> expr (A -> Prop) -> expr (AffineFold S S)

  (* fold-related primitives *)
; fold : forall {S A : Type}, Fold S A -> expr (Fold S A)
; flComposeHoriz : forall {S A B : Type},
    expr (Fold S A) -> expr (Fold S B) -> expr (Fold S (A * B))
; flComposeVerti : forall {S A B : Type},
    expr (Fold S A) -> expr (Fold A B) -> expr (Fold S B)
; flComposeHorizL : forall {S A B : Type},
    expr (Fold S A) -> expr (Fold S B) -> expr (Fold S (A * option B))
; flComposeHorizR : forall {S A B : Type},
    expr (Fold S A) -> expr (Fold S B) -> expr (Fold S (option A * B))

  (* action primitives *)
; foldM : forall {S A M : Type} `{Monoid M}, expr (Fold S A) -> expr (A -> M) -> expr (S -> M)
; getAll : forall {S A : Type}, expr (Fold S A) -> expr (S -> list A)
; getHead : forall {S A : Type}, expr (Fold S A) -> expr (S -> option A)
; all : forall {S A : Type}, expr (Fold S A) -> expr (A -> Prop) -> expr (S -> Prop)
; any : forall {S A : Type}, expr (Fold S A) -> expr (A -> Prop) -> expr (S -> Prop)
; contains : forall {S A : Type}, expr (Fold S A) -> expr A -> expr (S -> Prop)
; putAll : forall {S A : Type}, expr (Traversal S A) -> expr A -> expr (S -> S)
; modifyAll : forall {S A : Type}, expr (Traversal S A) -> expr (A -> A) -> expr (S -> S)

  (* derived methods *)
; firstLn {A B : Type} : expr (Lens (A * B) A) := lens fstLn
; secondLn {A B : Type} : expr (Lens (A * B) B) := lens sndLn
; firstGt {A B : Type} : expr (Getter (A * B) A) := getter first
; secondGt {A B : Type} : expr (Getter (A * B) B) := getter second
; lnAsFold {S A : Type} (ln : expr (Lens S A)) : expr (Fold S A) := trAsFold (lnAsTraversal ln)
; filtered' {S : Type} (p : expr (S -> Prop)) : expr (AffineFold S S) := filtered (getter identity) p
}.

Notation "ln1 +_ln ln2" := (lnComposeVerti ln1 ln2) (at level 50, left associativity).
Notation "ln1 *_ln ln2" := (lnComposeHoriz ln1 ln2) (at level 40, left associativity).

Notation "tr1 +_tr tr2" := (trComposeVerti tr1 tr2) (at level 50, left associativity).
Notation "tr1 *_tr tr2" := (trComposeHoriz tr1 tr2) (at level 40, left associativity).

Notation "gt1 +_gt gt2" := (gtComposeVerti gt1 gt2) (at level 50, left associativity).
Notation "gt1 *_gt gt2" := (gtComposeHoriz gt1 gt2) (at level 40, left associativity).

Notation "fl1 +_fl fl2" := (flComposeVerti fl1 fl2) (at level 50, left associativity).
Notation "fl1 *_fl fl2" := (flComposeHoriz fl1 fl2) (at level 40, left associativity).

Notation "n1 <= n2" := (leqt n1 n2) (at level 70, no associativity).
Notation "n1 < n2" := (lt n1 n2) (at level 70, no associativity).
Notation "n1 == n2" := (eq n1 n2) (at level 70, no associativity).
Notation "p /\ q" := (and p q) (at level 80, right associativity).

Notation "a |*| b" := (product a b) (at level 40, left associativity).

(*******************)
(* Couples example *)
(*******************)

Record Person := mkPerson
{ name: string
; age: nat
}.

Record Couple := mkCouple
{ her: Person
; him: Person
}.

Definition nameLn `{OpticLang expr} : expr (Lens Person string). 
Proof. Admitted.

Definition ageLn `{OpticLang expr} : expr (Lens Person nat). 
Proof. Admitted.

Definition herLn `{OpticLang expr} : expr (Lens Couple Person). 
Proof. Admitted.

Definition himLn `{OpticLang expr} : expr (Lens Couple Person).
Proof. Admitted.

Definition peopleTr `{OpticLang expr} : expr (Traversal (list Person) Person).
Proof. Admitted.

Definition couplesTr `{OpticLang expr} : expr (Traversal (list Couple) Couple).
Proof. Admitted.

Definition bothTr `{OpticLang expr} : expr (Traversal (list Couple) Person).
Proof. Admitted.

(* Query [getPeople], already normalized *)

Definition getPeople `{OpticLang expr} : expr (list Person -> list Person) :=
  getAll (trAsFold peopleTr).

(* Query [getName] *)

Definition personNameTr `{OpticLang expr} : expr (Traversal (list Person) string) :=
  peopleTr +_tr lnAsTraversal nameLn.

Definition getName `{OpticLang expr} : expr (list Person -> list string) :=
  getAll (trAsFold personNameTr).

(* Todos mis amigos se llaman Cayetano ~ https://www.youtube.com/watch?v=ZiUhV12G024  *)
Definition putName `{OpticLang expr} : expr (list Person -> list Person) :=
  putAll personNameTr (str "Cayetano").

Definition modifyName `{OpticLang expr} : expr (list Person -> list Person) :=
  modifyAll personNameTr upper.

(* Query [getAgeAndName] *)

Definition personAgeAndNameTr `{OpticLang expr} : expr (Traversal (list Person) (nat * string)) :=
  peopleTr +_tr lnAsTraversal (ageLn *_ln nameLn).

Definition getAgeAndName `{OpticLang expr} : expr (list Person -> list (nat * string)) :=
  getAll (trAsFold personAgeAndNameTr).

Definition putAgeAndName `{OpticLang expr} : expr (list Person -> list Person) :=
  putAll personAgeAndNameTr (ntr 33 |*| str "Cayetano").

(* Query [getHerAges] *)

Definition herAgesTr `{OpticLang expr} : expr (Traversal (list Couple) nat) :=
  couplesTr +_tr lnAsTraversal (herLn +_ln ageLn).

Definition getHerAges `{OpticLang expr} : expr (list Couple -> list nat) :=
  getAll (trAsFold herAgesTr).

Definition putHerAges `{OpticLang expr} : expr (list Couple -> list Couple) :=
  putAll herAgesTr (ntr 33).

Definition modifyHerAges `{OpticLang expr} : expr (list Couple -> list Couple) :=
  modifyAll herAgesTr incr.

(* Query [getPeopleOnTheirThirties] *)

Definition peopleOnTheirThirtiesTr `{OpticLang expr} : expr (Traversal (list Person) Person) :=
  peopleTr +_tr unsafeFiltered (lnAsGetter ageLn) (lam (fun a => ntr 30 <= a /\ a < ntr 40)).

Definition getPeopleOnTheirThirties `{OpticLang expr} : expr (list Person -> list Person) :=
  getAll (trAsFold peopleOnTheirThirtiesTr).

(* This is safe, since Cayetano is 33 years old, and therefore traversal laws hold. *)
Definition putPeopleOnTheirThirties `{OpticLang expr} : expr (list Person -> list Person) :=
  putAll peopleOnTheirThirtiesTr (lift (mkPerson "Cayetano" 33)).

(* Query [difference] *)

Definition difference `{OpticLang expr} :=
  getAll (trAsFold couplesTr +_fl
    lnAsFold (herLn +_ln nameLn) *_fl 
      gtAsFold (lnAsGetter ((herLn +_ln ageLn) *_ln (himLn +_ln ageLn)) +_gt getter sub) +_fl
    aflAsFold (filtered secondGt (lam (leqt (ntr 0))))).

(* Query [range] *)

Definition rangeFl `{OpticLang expr} (a b : expr nat) : expr (Fold (list Couple) string) :=
  trAsFold (bothTr +_tr lnAsTraversal (nameLn *_ln ageLn)) +_fl
    aflAsFold (filtered secondGt (lam (fun i => a <= i /\ i < b))) +_fl
    lnAsFold firstLn.

(* Query [getAge] *)

Definition getAgeFl `{OpticLang expr} (s : expr string) : expr (Fold (list Couple) nat) :=
  trAsFold (bothTr +_tr lnAsTraversal (nameLn *_ln ageLn)) +_fl
    aflAsFold (filtered firstGt (lam (fun n => n == s))) +_fl
    lnAsFold secondLn.

(* Query [compose] *)

Definition bind `{OpticLang expr} {S A M} `{Monoid M} 
    (fl : expr (Fold S A)) (f : expr A -> expr (S -> M)) : expr (S -> M) :=
  lam (fun s => app (foldM fl (lam (fun a => app (app (lam f) a) s))) s).

Notation "fl >>= f" := (bind fl f) (at level 40, left associativity).

Definition compose `{OpticLang expr} 
    (s t : expr string) : expr (list Couple -> list string) :=
  getAgeFl s >>= (fun a1 => getAgeFl t >>= (fun a2 => getAll (rangeFl a1 a2))).

Definition compose' `{OpticLang expr} 
    (s t : expr string) : expr (list Couple -> list string) :=
  getAgeFl s *_fl getAgeFl t >>= (getAll ∘ (app (uncurry (lam (lam ∘ rangeFl))))).

Notation "'do' a ← e ; c" := (e >>= (fun a => c)) (at level 60, right associativity).

Definition compose_do `{OpticLang expr} 
    (s t : expr string) : expr (list Couple -> list string) :=
  do a1 ← getAgeFl s;
  do a2 ← getAgeFl t;
  getAll (rangeFl a1 a2).

Definition compose'_do `{OpticLang expr} 
    (s t : expr string) : expr (list Couple -> list string) :=
  do ages ← getAgeFl s *_fl getAgeFl t;
  getAll (app (uncurry (lam (lam ∘ rangeFl))) ages).

(**********************)
(* Department example *)
(**********************)

Definition Task : Type := string.

Record Employee := mkEmployee
{ emp : string
; tasks : list Task
}.

Record Department := mkNestedOrg
{ dpt : string
; employees : list Employee
}.

Definition NestedOrg := list Department.

Definition eachTr {A : Type} `{OpticLang expr} : expr (Traversal (list A) A).
Proof. Admitted.

Definition eachFl {A : Type} `{OpticLang expr} : expr (Fold (list A) A).
Proof. Admitted.

Definition empLn `{OpticLang expr} : expr (Lens Employee string).
Proof. Admitted.

Definition tasksLn `{OpticLang expr} : expr (Lens Employee (list Task)).
Proof. Admitted.

Definition dptLn `{OpticLang expr} : expr (Lens Department string).
Proof. Admitted.

Definition employeesLn `{OpticLang expr} : expr (Lens Department (list Employee)).
Proof. Admitted.

(* Query [expertise] *)

Definition expertise `{OpticLang expr} (tsk : expr Task) : expr (NestedOrg -> list string) :=
  getAll (eachFl +_fl
    aflAsFold (filtered (lnAsGetter employeesLn)
      (all eachFl (contains (lnAsFold tasksLn +_fl eachFl) tsk))) +_fl
    lnAsFold dptLn).

(* Bonus: Query [insertCayetano] *)

(* We could use this trick to insert new values, while working with optics *)

(* Cayetano works in all departments *)
Definition insertCayetano `{OpticLang expr} : expr (NestedOrg -> NestedOrg) :=
  modifyAll (eachTr +_tr lnAsTraversal employeesLn)
            (lam (append (lift (mkEmployee "Cayetano" List.nil)))).

