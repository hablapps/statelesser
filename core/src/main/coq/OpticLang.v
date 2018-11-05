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

(****************)
(* Plain optics *)
(****************)

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

Class Monoid (M : Type) :=
{ mempty : M
; mappend : M -> M -> M
}.

Instance listMonoid {A : Type} : Monoid (list A) :=
{| mempty := List.nil
;  mappend m1 m2 := m1 ++ m2
|}.

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

Definition idTr {S : Type} : Traversal S S :=
  mkTraversal (fun s => existT (result S S) 1 (cons S s 0 (nil S), hd)).

Record Getter S A := mkGetter
{ view : S -> A }.

Arguments mkGetter [S A].

Definition idGt {S : Type} : Getter S S :=
  mkGetter id.

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
; getter : forall {S A : Type}, Getter S A -> expr (Getter S A)

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

  (* fold-related primitives *)
; fold : forall {S A : Type}, Fold S A -> expr (Fold S A)
; filtered : forall {S A : Type},
    expr (Getter S A) -> expr (A -> Prop) -> expr (Fold S S)
; mapped : forall {S A B : Type},
    expr (Getter S A) -> expr (A -> B) -> expr (Fold S B)
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
; liftLam {A B : Type} : (A -> B) -> expr (A -> B) := fun f => lam (app (lift f))
; first {A B : Type} : expr (A * B -> A) := liftLam fst
; second {A B : Type} : expr (A * B -> B) := liftLam snd
; firstLn {A B : Type} : expr (Lens (A * B) A) := lens fstLn
; secondLn {A B : Type} : expr (Lens (A * B) B) := lens sndLn
; firstGt {A B : Type} : expr (Getter (A * B) A) := getter (mkGetter fst)
; secondGt {A B : Type} : expr (Getter (A * B) B) := getter (mkGetter snd)
; lnAsFold {S A : Type} (ln : expr (Lens S A)) : expr (Fold S A) := trAsFold (lnAsTraversal ln)
; mapped' {S A : Type} (f : expr (S -> A)) : expr (Fold S A) := mapped (getter idGt) f
; filtered' {S : Type} (p : expr (S -> Prop)) : expr (Fold S S) := filtered (getter idGt) p
}.

Notation "ln1 +_ln ln2" := (lnComposeVerti ln1 ln2) (at level 50, left associativity).
Notation "ln1 *_ln ln2" := (lnComposeHoriz ln1 ln2) (at level 40, left associativity).

Notation "tr1 +_tr tr2" := (trComposeVerti tr1 tr2) (at level 50, left associativity).
Notation "tr1 *_tr tr2" := (trComposeHoriz tr1 tr2) (at level 40, left associativity).

Notation "fl1 +_fl fl2" := (flComposeVerti fl1 fl2) (at level 50, left associativity).
Notation "fl1 *_fl fl2" := (flComposeHoriz fl1 fl2) (at level 40, left associativity).

Notation "n1 <= n2" := (leqt n1 n2) (at level 70, no associativity).
Notation "n1 < n2" := (lt n1 n2) (at level 70, no associativity).
Notation "n1 == n2" := (eq n1 n2) (at level 70, no associativity).
Notation "p /\ q" := (and p q) (at level 80, right associativity).

Notation "a |*| b" := (product a b) (at level 40, left associativity).

Class OpticLangOpt (expr : Type -> Type) `{OpticLang expr} :=

{ (* fold category laws *)
  flLeftId : forall S A (fl : expr (Fold S A)),
    fold idFl +_fl fl = fl
; flRightId : forall S A (fl : expr (Fold S A)),
    fl +_fl fold idFl = fl
; flAssocV : forall S A B C (fl1 : expr (Fold S A)) (fl2 : expr (Fold A B)) (fl3 : expr (Fold B C)),
    fl1 +_fl (fl2 +_fl fl3) = fl1 +_fl fl2 +_fl fl3

  (* spread conversions *)
; trAsFoldDistH : forall S A B (tr1 : expr (Traversal S A)) (tr2 : expr (Traversal S B)),
    trAsFold (tr1 *_tr tr2) = trAsFold tr1 *_fl trAsFold tr2
; trAsFoldDistV : forall S A B (tr1 : expr (Traversal S A)) (tr2 : expr (Traversal A B)),
    trAsFold (tr1 +_tr tr2) = trAsFold tr1 +_fl trAsFold tr2
; lnAsTravDistH : forall S A B (ln1 : expr (Lens S A)) (ln2 : expr (Lens S B)),
    lnAsTraversal (ln1 *_ln ln2) = lnAsTraversal ln1 *_tr lnAsTraversal ln2
; lnAsTravDistV : forall S A B (ln1 : expr (Lens S A)) (ln2 : expr (Lens A B)),
    lnAsTraversal (ln1 +_ln ln2) = lnAsTraversal ln1 +_tr lnAsTraversal ln2

  (* preserve identity *)
; liftIdLn : forall S, lnAsTraversal (lens (@idLn S)) = traversal idTr
; liftIdTr : forall S, trAsFold (traversal (@idTr S)) = fold idFl

  (* fold-specific optimizations *)
; filterFilter : forall S (p q : expr (S -> Prop)),
    filtered (getter idGt) p +_fl filtered (getter idGt) q = 
      filtered (getter idGt) (lam (fun s => and (app p s) (app q s)))
; filterTrue : forall S A (gt : expr (Getter S A)),
    filtered gt (lam (fun _ => lift True)) = fold idFl
; vertDistHoriz :
    forall S A B C (fl1 : expr (Fold S A)) (fl2 : expr (Fold A B)) (fl3 : expr (Fold A C)),
      fl1 +_fl (fl2 *_fl fl3) = (fl1 +_fl fl2) *_fl (fl1 +_fl fl3)
}.

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
      (lnAsFold ((herLn +_ln ageLn) *_ln (himLn +_ln ageLn)) +_fl mapped' sub) +_fl
    filtered secondGt (lam (leqt (ntr 0)))).

(* Query [range] *)

Definition rangeFl `{OpticLang expr} (a b : expr nat) : expr (Fold (list Couple) string) :=
  trAsFold (bothTr +_tr lnAsTraversal (nameLn *_ln ageLn)) +_fl
    filtered secondGt (lam (fun i => a <= i /\ i < b)) +_fl
    lnAsFold firstLn.

(* Query [getAge] *)

Definition getAgeFl `{OpticLang expr} (s : expr string) : expr (Fold (list Couple) nat) :=
  trAsFold (bothTr +_tr lnAsTraversal (nameLn *_ln ageLn)) +_fl
    filtered firstGt (lam (fun n => n == s)) +_fl
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
    filtered (lnAsGetter employeesLn)
             (all eachFl (contains (lnAsFold tasksLn +_fl eachFl) tsk)) +_fl
    lnAsFold dptLn).

(* Bonus: Query [newEmployee] *)

(* We could use this trick to insert new values, while working with optics *)

(* Cayetano works in all departments *)
Definition insertCayetano `{OpticLang expr} : expr (NestedOrg -> NestedOrg) :=
  modifyAll (eachTr +_tr lnAsTraversal employeesLn) 
            (lam (append (lift (mkEmployee "Cayetano" List.nil)))).
