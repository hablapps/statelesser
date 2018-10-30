Require Import Program.Basics.
Require Import Coq.Strings.String.
Require Import Coq.Init.Specif.
Require Import Coq.Vectors.VectorDef.
Require Import Coq.Arith.PeanoNat.
Require Import Coq.Init.Logic.
Require Import Coq.Logic.FunctionalExtensionality.

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

{ (* higher-order abstract syntax (hoas) *)
  lift : forall {A : Type}, A -> expr A
; lam : forall {A B : Type}, (expr A -> expr B) -> expr (A -> B)
; app : forall {A B : Type}, expr (A -> B) -> expr A -> expr B

  (* product-related primitives *)
; curry : forall {A B C : Type}, expr (A * B -> C) -> expr (A -> B -> C)
; uncurry : forall {A B C : Type}, expr (A -> B -> C) -> expr (A * B -> C)
; product : forall {A B : Type}, expr A -> expr B -> expr (prod A B)

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
; trComposeHorizL : forall {S A B : Type},
    expr (Traversal S A) -> expr (Traversal S B) -> expr (Traversal S (A * option B))
; trComposeHorizR : forall {S A B : Type},
    expr (Traversal S A) -> expr (Traversal S B) -> expr (Traversal S (option A * B))

  (* propositional primitives *)
; and : expr Prop -> expr Prop -> expr Prop
; or  : expr Prop -> expr Prop -> expr Prop
; leqt : expr nat -> expr nat -> expr Prop
; lt : expr nat -> expr nat -> expr Prop
; eq : expr string -> expr string -> expr Prop

  (* fold-related primitives *)
; fold : forall {S A : Type}, Fold S A -> expr (Fold S A)
; filter : forall {S A : Type},
    expr (Fold S A) -> expr (A -> Prop) -> expr (Fold S S)
; flComposeHoriz : forall {S A B : Type},
    expr (Fold S A) -> expr (Fold S B) -> expr (Fold S (A * B))
; flComposeVerti : forall {S A B : Type},
    expr (Fold S A) -> expr (Fold A B) -> expr (Fold S B)
; flComposeHorizL : forall {S A B : Type},
    expr (Fold S A) -> expr (Fold S B) -> expr (Fold S (A * option B))
; flComposeHorizR : forall {S A B : Type},
    expr (Fold S A) -> expr (Fold S B) -> expr (Fold S (option A * B))
; join : forall {A B C : Type},
    expr (A * B -> C) -> expr (Fold (A * B) C)
; bind : forall {S A B : Type},
    expr (Fold S A) -> expr (A -> Fold S B) -> expr (Fold S B)

  (* action primitives *)
; flGetAll : forall {S A : Type},
    expr (Fold S A) -> obs (S -> list A)
; flGetHead : forall {S A : Type},
    expr (Fold S A) -> obs (S -> option A)

  (* derived methods *)
; liftLam {A B : Type} : (A -> B) -> expr (A -> B) := fun f => lam (app (lift f))
; first {A B : Type} : expr (A * B -> A) := liftLam fst
; second {A B : Type} : expr (A * B -> B) := liftLam snd
}.

Notation "ln1 +_ln ln2" := (lnComposeVerti ln1 ln2) (at level 50, left associativity).
Notation "ln1 *_ln ln2" := (lnComposeHoriz ln1 ln2) (at level 40, left associativity).

Notation "tr1 +_tr tr2" := (trComposeVerti tr1 tr2) (at level 50, left associativity).
Notation "tr1 *_tr tr2" := (trComposeHoriz tr1 tr2) (at level 40, left associativity).

Notation "fl1 +_fl fl2" := (flComposeVerti fl1 fl2) (at level 50, left associativity).
Notation "fl1 *_fl fl2" := (flComposeHoriz fl1 fl2) (at level 40, left associativity).
Notation "fl1 >>= f"  := (bind fl1 f) (at level 50, left associativity).
Notation "fl1 >> fl2"  := (fl1 >>= fun _ => fl2) (at level 50, left associativity).

Notation "n1 <= n2" := (leqt n1 n2) (at level 70, no associativity).
Notation "n1 < n2" := (lt n1 n2) (at level 70, no associativity).
Notation "n1 == n2" := (eq n1 n2) (at level 70, no associativity).
Notation "p /\ q" := (and p q) (at level 80, right associativity).

Class OpticLangOpt (expr obs : Type -> Type) `{OpticLang expr obs} :=

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
    filter (fold idFl) p +_fl filter (fold idFl) q = filter (fold idFl) (lam (fun s => and (app p s) (app q s)))
; filterTrue : forall S A (fl : expr (Fold S A)),
    filter fl (lam (fun _ => lift True)) = fold idFl
; vertDistHoriz :
    forall S A B C (fl1 : expr (Fold S A)) (fl2 : expr (Fold A B)) (fl3 : expr (Fold A C)),
      fl1 +_fl (fl2 *_fl fl3) = (fl1 +_fl fl2) *_fl (fl1 +_fl fl3)
; joinFst : forall S A B (fl1 : expr (Fold S A)) (fl2 : expr (Fold S B)),
    fl1 *_fl fl2 +_fl join first = fl1
; joinSnd : forall S A B (fl1 : expr (Fold S A)) (fl2 : expr (Fold S B)),
    fl1 *_fl fl2 +_fl join second = fl2
; bindHorizR :
    forall S A B C (fl1 : expr (Fold S A)) (f : expr (A -> Fold S B)) (fl2 : expr (Fold S C)),
      (fl1 >>= f) *_fl fl2 = fl1 >>= lam (fun a => (app f a) *_fl fl2)
; bindHorizL : 
    forall S A B C (fl1 : expr (Fold S A)) (fl2 : expr (Fold S B)) (f : expr (B -> Fold S C)),
      fl1 *_fl (fl2 >>= f) = fl2 >>= lam (fun b => fl1 *_fl app f b)
; bindVertR :
    forall S A B C (fl1 : expr (Fold S A)) (f : expr (A -> Fold S B)) (fl2 : expr (Fold B C)),
      (fl1 >>= f) +_fl fl2 = fl1 >>= lam (fun a => app f a +_fl fl2)
; bindVertL : 
    forall S A B C (fl1 : expr (Fold S A)) (fl2 : expr (Fold A B)) (f : expr (B -> Fold A C)),
      fl1 +_fl (fl2 >>= f) = (fl1 +_fl fl2) >>= lam (fun b => fl1 +_fl app f b)
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
Definition bothTr : Traversal (list Couple) Person. Proof. Admitted.

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
            filter (trAsFold (lnAsTraversal (lens sndLn))) (lam (fun a => lift 30 <= a /\ a < lift 40)) +_fl
            join first).

Definition getPeopleOnTheirThirtiesN `{OpticLang expr obs} : obs (list Person -> list Person) :=
  flGetAll (trAsFold (traversal peopleTr) *_fl 
             (trAsFold (traversal peopleTr) +_fl trAsFold (lnAsTraversal (lens ageLn))) +_fl
            filter (trAsFold (lnAsTraversal (lens sndLn))) (lam (fun a => lift 30 <= a /\ a < lift 40)) +_fl
            join first).

Example normalize_getPeopleOnTheirThirties : forall expr obs `{OpticLangOpt expr obs},
  getPeopleOnTheirThirties = getPeopleOnTheirThirtiesN.
Proof.
  intros.
  destruct H0.
  unfold getPeopleOnTheirThirties, getPeopleOnTheirThirtiesN.
  congruence.
Qed.

(* Query [difference] *)

Definition difference `{OpticLang expr obs} : obs (list Couple -> list (string * nat)) :=
  let himAge := lens himLn +_ln lens ageLn in
  let herNameAge := lens herLn +_ln lens nameLn *_ln lens ageLn in
  flGetAll (
    trAsFold (traversal couplesTr +_tr lnAsTraversal (himAge *_ln herNameAge)) +_fl
      filter (trAsFold (lnAsTraversal (lens fstLn)) *_fl (trAsFold (lnAsTraversal (lens sndLn +_ln lens sndLn))))
            (uncurry (lam (fun ma => lam (fun wa => ma < wa)))) +_fl
      (join second +_fl join first) *_fl
        (join first *_fl (join second +_fl join second) +_fl join (liftLam (fun ab => snd ab - fst ab)))).

Definition differenceN `{OpticLang expr obs} : obs (list Couple -> list (string * nat)) :=
  let couples := trAsFold (traversal couplesTr) in
  let him := trAsFold (lnAsTraversal (lens himLn)) in
  let her := trAsFold (lnAsTraversal (lens herLn)) in
  let age := trAsFold (lnAsTraversal (lens ageLn)) in
  let name := trAsFold (lnAsTraversal (lens nameLn)) in
  let shared := (couples +_fl him +_fl age) *_fl
      ((couples +_fl her +_fl name) *_fl
       (couples +_fl her +_fl age)) +_fl
    filter (trAsFold (lnAsTraversal (lens fstLn)) *_fl (trAsFold (lnAsTraversal (lens sndLn +_ln lens sndLn))))
            (uncurry (lam (fun ma => lam (fun wa => ma < wa)))) in
  flGetAll((shared +_fl (join second +_fl join first)) *_fl
    ((shared +_fl join first) *_fl (shared +_fl join second +_fl join second) +_fl join (liftLam (fun ab => snd ab - fst ab)))).

Example normalize_difference : forall expr obs `{OpticLangOpt expr obs},
  difference = differenceN.
Proof.
  intros.
  destruct H0.
  unfold difference, differenceN.
  congruence.
Qed.

(* Query [range] *)

Definition rangeFl `{OpticLang expr obs} (a b : expr nat) : expr (Fold (list Couple) string) :=
  trAsFold (traversal bothTr +_tr lnAsTraversal (lens nameLn *_ln lens ageLn)) +_fl
  filter (trAsFold (lnAsTraversal (lens sndLn))) (lam (fun i => a <= i /\ i < b)) +_fl
  join first.

Definition rangeFlN `{OpticLang expr obs} (a b : expr nat)  : expr (Fold (list Couple) string) :=
  let both := trAsFold (traversal bothTr) in
  let bothName := both +_fl trAsFold (lnAsTraversal (lens nameLn)) in
  let bothAge := both +_fl trAsFold (lnAsTraversal (lens ageLn)) in
  bothName *_fl bothAge +_fl
  filter (trAsFold (lnAsTraversal (lens sndLn))) (lam (fun i => a <= i /\ i < b)) +_fl
  join first.

Example normalize_rangeFl : forall expr obs `{OpticLangOpt expr obs} a b,
  rangeFl a b = rangeFlN a b.
Proof.
  intros.
  destruct H0.
  unfold rangeFl, rangeFlN.
  congruence.
Qed.

Definition range `{OpticLang expr obs} (a b : expr nat) : obs (list Couple -> list string) :=
  flGetAll (rangeFl a b).

Definition rangeN `{OpticLang expr obs} (a b : expr nat) : obs (list Couple -> list string) :=
  flGetAll (rangeFlN a b).

Example normalize_range : forall expr obs `{OpticLangOpt expr obs} a b,
  rangeFl a b = rangeFlN a b.
Proof.
  intros.
  destruct H0.
  unfold range, rangeN.
  now rewrite normalize_rangeFl.
Qed.

(* Query [getAge] *)

Definition getAgeFl `{OpticLang expr obs} (s : expr string) : expr (Fold (list Couple) nat) :=
  trAsFold (traversal bothTr +_tr lnAsTraversal (lens nameLn *_ln lens ageLn)) +_fl
  filter (trAsFold (lnAsTraversal (lens fstLn))) (lam (fun n => n == s)) +_fl
  join second.

Definition getAgeFlN `{OpticLang expr obs} (s : expr string)  : expr (Fold (list Couple) nat) :=
  let bothName := trAsFold (traversal bothTr) +_fl trAsFold (lnAsTraversal (lens nameLn)) in
  let bothAge := trAsFold (traversal bothTr) +_fl trAsFold (lnAsTraversal (lens ageLn)) in
  bothName *_fl bothAge +_fl
  filter (trAsFold (lnAsTraversal (lens fstLn))) (lam (fun n => n == s)) +_fl
  join second.

Example normalize_getAgeFl : forall expr obs `{OpticLangOpt expr obs} s,
  getAgeFl s = getAgeFlN s.
Proof.
  intros.
  destruct H0.
  unfold getAgeFl, getAgeFlN.
  congruence.
Qed.

Definition getAge `{OpticLang expr obs} (s : expr string)  : obs (list Couple -> option nat) :=
  flGetHead (getAgeFl s).

Definition getAgeN `{OpticLang expr obs} (s : expr string)  : obs (list Couple -> option nat) :=
  flGetHead (getAgeFlN s).

Example normalize_getAge : forall expr obs `{OpticLangOpt expr obs} s,
  getAge s = getAgeN s.
Proof.
  intros.
  unfold getAge, getAgeN.
  now rewrite normalize_getAgeFl.
Qed.

(* Query [compose] *)

Definition compose `{OpticLang expr obs} (s t : expr string) : obs (list Couple -> list string) :=
  flGetAll (getAgeFl s >>= lam (fun a => getAgeFl t >>= lam (fun b => rangeFl a b))).

Definition compose' `{OpticLang expr obs} (s t : expr string) : obs (list Couple -> list string) :=
  flGetAll (getAgeFl s *_fl getAgeFl t >>= uncurry (lam (fun a => lam (fun b => rangeFl a b)))).

Definition rangeFlP `{OpticLang expr obs} : expr (prod nat nat -> Fold (list Couple) string) :=
  uncurry (lam (fun a => lam (fun b => rangeFl a b))).

Definition compose'' `{OpticLang expr obs} (s t : expr string) : obs (list Couple -> list string) :=
  flGetAll (getAgeFl s *_fl getAgeFl t >>= rangeFlP).

Definition composeN `{OpticLang expr obs} (s t : expr string) : obs (list Couple -> list string) :=
  flGetAll (getAgeFlN s *_fl getAgeFlN t >>= uncurry (lam (fun a => lam (fun b => rangeFlN a b)))).

Example normalize_compose : forall expr obs `{OpticLangOpt expr obs} s t,
  compose' s t = composeN s t.
Proof.
  intros.
  unfold compose', composeN, getAgeFl, getAgeFlN, rangeFl, rangeFlN.
  destruct H0.
  now rewrite trAsFoldDistV0, lnAsTravDistH0, trAsFoldDistH0, vertDistHoriz0.
Qed.
