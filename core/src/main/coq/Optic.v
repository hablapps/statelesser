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

Open Scope program_scope.

Generalizable All Variables.
(****************)
(* Plain optics *)
(****************)

(* FOLD *)

Record Fold (S A : Type) := mkFold
{ foldMap `{Monoid M} : (A -> M) -> S -> M }.

Arguments mkFold [S A].
Arguments foldMap [S A].

Definition flVerCompose {S A B} (fl1 : Fold S A) (fl2 : Fold A B) : Fold S B :=
  mkFold (fun _ _ _ f s => foldMap fl1 _ _ _ (foldMap fl2 _ _ _ f) s).

Definition flGenCompose {S A B} F 
    `{Applicative F, Foldable F, Monoid (F A), Monoid (F B)}
    (fl1 : Fold S A) (fl2 : Fold S B) : Fold S (A * B) :=
  mkFold (fun _ _ _ f s => @fold F _ _ _ _ _ f (tupled
    (foldMap fl1 _ _ _ pure s)
    (foldMap fl2 _ _ _ pure s))).

Definition flProCompose {S A B}
    (fl1 : Fold S A) (fl2 : Fold S B) : Fold S (A * B) :=
  flGenCompose listCart fl1 fl2.

Definition flHorCompose {S A B}
    (fl1 : Fold S A) (fl2 : Fold S B) : Fold S (A * B) :=
  flGenCompose list fl1 fl2.

Definition idFl {S : Type} : Fold S S :=
  mkFold (fun _ _ _ f s => f s).

(* SETTER *)

Record Setter (S A : Type) := mkSetter
{ modify : (A -> A) -> S -> S
}.

Arguments mkSetter [S A].
Arguments modify [S A].

Definition stVerCompose {S A B}
    (st1 : Setter S A) (st2: Setter A B) : Setter S B :=
  mkSetter (fun f => modify st1 (modify st2 f)).

(* I wasn't able to implement horizontal composition for Setters! *)

Definition idSt {S : Type} : Setter S S :=
  mkSetter id.

(* FOLD1 *)

Record Fold1 S A := mkFold1
{ foldMap1 `{Semigroup M} : (A -> M) -> S -> M }.

Arguments mkFold1 [S A].
Arguments foldMap1 [S A].

Definition fl1VerCompose {S A B} 
    (fl1 : Fold1 S A) (fl2 : Fold1 A B) : Fold1 S B :=
  mkFold1 (fun _ _ f => foldMap1 fl1 _ _ (foldMap1 fl2 _ _ f)).

Definition fl1ProCompose {S A B}
    (fl1 : Fold1 S A) (fl2 : Fold1 S B) : Fold1 S (A * B) :=
  mkFold1 (fun _ _ f s => foldMap1 fl1 _ _ (fun a => 
    foldMap1 fl2 _ _ (fun b => f (a, b)) s) s).

Definition fl1HorCompose {S A B}
    (fl1 : Fold1 S A) (fl2 : Fold1 S B) : Fold1 S (A * B) :=
  mkFold1 (fun M m f s => fold1 f (nel_combine
    (foldMap1 fl1 _ _ (fun a => nel_nil a) s)
    (foldMap1 fl2 _ _ (fun b => nel_nil b) s))).

Definition fl1AsFold {S A} (fl : Fold1 S A) : Fold S A :=
  mkFold (fun _ _ _ => foldMap1 fl _ _).

Definition idFl1 {S : Type} : Fold1 S S :=
  mkFold1 (fun _ _ => id).

(* TRAVERSAL *)

Definition result S A (n : nat) : Type := 
  t A n * (t A n -> S).

Definition nResult {S A} (sig : sigT (result S A)) : nat :=
  match sig with | existT _ n _ => n end.

Record Traversal S A := mkTraversal
{ extract : S -> sigT (result S A) }.

Arguments mkTraversal [S A].
Arguments extract [S A].

(* TODO: Combinators for this Traversal representation aren't trivial at all! *)

Definition trVerCompose {S A B}
    (tr1 : Traversal S A) (tr2 : Traversal A B) : Traversal S B.
Proof. Admitted.

(* XXX: I was not able to prove this statement, but I think it makes a lot of 
   sense. Broadly, I think that you can't provide a cartesian combinator for 
   traversals, but you can provide the horizontal (zip) one, as long as both 
   traversals have the very same number of foci. Only in this case it's feasible 
   to implement putAll. *)
Definition trHorCompose {S A B}
    (tr1 : Traversal S A) (tr2 : Traversal S B)
    (pro : forall s, nResult (extract tr1 s) = nResult (extract tr2 s))
    : Traversal S (A * B).
Proof. Admitted.

Definition idTr {S : Type} : Traversal S S :=
  mkTraversal (fun s => existT _ 1 (cons S s 0 (nil S), hd)).

Definition each {S : Type} : Traversal (list S) S :=
  mkTraversal (fun xs => existT _ (length xs) (of_list xs, to_list)).

Definition trAsSetter {S A} (tr : Traversal S A) : Setter S A.
Proof. Admitted.

Definition trAsFold {S A} (tr : Traversal S A) : Fold S A :=
  mkFold (fun _ _ _ f s =>
    match extract tr s with | existT _ _ (v, _) => fold f (to_list v) end).

(* AFFINE FOLD *)

Record AffineFold S A := mkAffineFold
{ afold : S -> option A }.

Arguments mkAffineFold [S A].
Arguments afold [S A].

Definition aflVerCompose {S A B}
    (af1 : AffineFold S A) (af2 : AffineFold A B) : AffineFold S B :=
  mkAffineFold (fun s => afold af1 s >>= (fun a => afold af2 a)).

(* Both product and horizontal are equivalent *)

Definition aflHorCompose {S A B}
    (af1 : AffineFold S A) (af2 : AffineFold S B) : AffineFold S (A * B) :=
  mkAffineFold (fun s => tupled (afold af1 s) (afold af2 s)).

Definition aflAsFold {S A} (afl : AffineFold S A) : Fold S A :=
  mkFold (fun _ _ _ f s => fold f (afold afl s)).

Definition idAfl {S} : AffineFold S S :=
  mkAffineFold Some.

(* GETTER *)

Record Getter S A := mkGetter
{ view : S -> A }.

Arguments mkGetter [S A].
Arguments view [S A].

Definition gtVerCompose {S A B}
    (gt1 : Getter S A) (gt2 : Getter A B) : Getter S B :=
  mkGetter (view gt2 ∘ view gt1).

Definition getHorCompose {S A B}
    (gt1 : Getter S A) (gt2 : Getter S B) : Getter S (A * B) :=
  mkGetter (fork (view gt1) (view gt2)).

Definition gtAsFold1 {S A} (gt : Getter S A) : Fold1 S A :=
  mkFold1 (fun _ _ f => f ∘ view gt).

Definition gtAsAffineFold {S A} (gt : Getter S A) : AffineFold S A :=
  mkAffineFold (Some ∘ view gt).

Definition idGt {S : Type} : Getter S S :=
  mkGetter id.

(* Notice that the following combinators build [AffineFold]s but they are placed
   here because of the dependency with [Getter] *)

(* XXX: the gt parameter is not standard in optics but it's practical to deal
   with product types. *)
Definition filtered' {S A} (gt : Getter S A) (p : A -> bool) : AffineFold S S :=
  mkAffineFold (fun s => if p (view gt s) then Some s else None).

Definition filtered {S} (p : S -> bool) : AffineFold S S :=
  filtered' idGt p.

(* TRAVERSAL1 *)

Definition result1 T A (n : nat) : Type :=
  t A (S n) * (t A (S n) -> T).

Record Traversal1 (S A : Type) := mkTraversal1
{ extract1 : S -> sigT (result1 S A) }.

Arguments mkTraversal1 [S A].
Arguments extract1 [S A].

Definition tr1VerCompose {S A B}
    (tr1 : Traversal1 S A) (tr2 : Traversal1 A B) : Traversal1 S B.
Proof. Admitted.

Definition tr1HorCompose {S A B}
    (tr1 : Traversal1 S A) (tr2 : Traversal1 S B) : Traversal1 S (A * B).
Proof. Admitted.

Definition tr1AsFold1 {S A} (tr1 : Traversal1 S A) : Fold1 S A :=
  mkFold1 (fun _ _ f s => match extract1 tr1 s with 
                          | existT _ _ (v, _) => fold1 f (vec1ToNel v)
                          end).

Definition tr1AsTraversal {S A} (tr1 : Traversal1 S A) : Traversal S A :=
  mkTraversal (fun s => match extract1 tr1 s with
                        | existT _ _ (v, f) => existT _ _ (v, f)
                        end).

(* AFFINE TRAVERSAL *)

Record AffineTraversal (S A : Type) := mkAffineTraversal
{ preview : S -> option A
; set : A -> S -> S
}.

Arguments mkAffineTraversal [S A].
Arguments preview [S A].
Arguments set [S A].

Definition atrVerCompose {S A B}
    (atr1 : AffineTraversal S A) (atr2 : AffineTraversal A B) : AffineTraversal S B :=
  mkAffineTraversal 
    (fun s => preview atr1 s >>= preview atr2) 
    (fun b s => option_fold (fun a => set atr1 (set atr2 b a) s) s (preview atr1 s)).

Definition atrHorCompose {S A B}
    (atr1 : AffineTraversal S A) (atr2 : AffineTraversal S B) : AffineTraversal S (A * B) :=
  mkAffineTraversal
    (fun s => tupled (preview atr1 s) (preview atr2 s))
    (fun ab => match ab with | (a, b) => set atr2 b ∘ set atr1 a end).

Definition atrAsAffineFold {S A} (atr : AffineTraversal S A) : AffineFold S A :=
  mkAffineFold (preview atr).

Definition atrAsTraversal {S A} (atr : AffineTraversal S A) : Traversal S A :=
  mkTraversal (fun s => option_fold 
    (fun a => existT _ _ (cons _ a _ (nil A), fun v => set atr (hd v) s)) 
    (existT  _ _ (nil A, fun _ => s)) 
    (preview atr s)).

Definition idAtr {S} : AffineTraversal S S :=
  mkAffineTraversal Some Basics.const.

(* LENS *)

Record Lens S A := mkLens
{ get : S -> A
; put : A -> S -> S
}.

Arguments mkLens [S A].
Arguments get [S A].
Arguments put [S A].

Definition lnVerCompose {S A B} (ln1 : Lens S A) (ln2 : Lens A B) : Lens S B :=
  mkLens (get ln2 ∘ get ln1) (fun b s => put ln1 (put ln2 b (get ln1 s)) s).

Definition lnHorCompose {S A B} 
    (ln1 : Lens S A) (ln2 : Lens S B) : Lens S (A * B) :=
  mkLens (fork (get ln1) (get ln2)) (fun ab => 
    match ab with | (a, b) => put ln2 b ∘ put ln1 a end).

Definition lnAsGetter {S A} (ln : Lens S A) : Getter S A :=
  mkGetter (get ln).

Definition lnAsAffineTraversal {S A} (ln : Lens S A) : AffineTraversal S A :=
  mkAffineTraversal (Some ∘ get ln) (put ln). 

Definition lnAsTraversal1 {S A} (ln : Lens S A) : Traversal1 S A :=
  mkTraversal1 (fun s => 
    existT _ _ (cons _ (get ln s) _ (nil _), fun v => put ln (hd v) s)).

Definition idLn {S : Type} : Lens S S :=
  mkLens id Basics.const.

Definition fstLn {A B : Type} : Lens (A * B) A :=
  mkLens fst (fun a ab => (a, snd ab)).

Definition sndLn {A B : Type} : Lens (A * B) B :=
  mkLens snd (fun b ab => (fst ab, b)).

(* PRISM *)

Record Prism S A := mkPrism
{ peek : S -> option A
; build : A -> S
}.

Arguments mkPrism [S A].
Arguments peek [S A].
Arguments build [S A].

Definition prVerCompose {S A B}
    (pr1 : Prism S A) (pr2 : Prism A B) : Prism S B :=
  mkPrism (fun s => peek pr1 s >>= peek pr2) (build pr1 ∘build pr2).

(* XXX: can't compose prisms horizontally! Notice how building, though possible, 
 * is lossy, because we would ignore one of the building methods.
 * 
 * Definition prHorCompose {S A B}
 *     (pr1 : Prism S A) (pr2 : Prism S B) : Prism S (A * B) :=
 *   mkPrism (fun s => (peek pr1 s, peek pr2 s))
 *           (fun ab => match ab with | (a, b) => ??? end)
 *)

Definition prAsAffineTraversal {S A} (pr : Prism S A) : AffineTraversal S A :=
  mkAffineTraversal (peek pr) (fun a _ => build pr a).

Definition idPr {S} : Prism S S :=
  mkPrism Some id.

(* ISO *)

Record Iso S A := mkIso
{ to : S -> A
; from : A -> S
}.

Arguments mkIso [S A].
Arguments to [S A].
Arguments from [S A].

Definition isoVerCompose {S A B}
    (iso1 : Iso S A) (iso2 : Iso A B) : Iso S B :=
  mkIso (to iso2 ∘ to iso1) (from iso1 ∘ from iso2).

(* XXX: can't compose isos horizontally!
 * 
 * Definition isoHorCompose {S A B}
 *     (iso1 : Iso S A) (iso2 : Iso S B) : Iso S (A * B) :=
 *   mkIso (fun s => (to iso1 s) (to iso2 s))
 *         (fun ab => match ab with | (a, b) => ? end).
 *)

Definition isoAsLens {S A} (iso : Iso S A) : Lens S A :=
  mkLens (to iso) (fun a _ => from iso a).

Definition isoAsPrism {S A} (iso : Iso S A) : Prism S A :=
  mkPrism (Some ∘ to iso) (from iso).

Definition idIso {S : Type} : Iso S S :=
  mkIso id id.

(* Optic class hierarchy *)

Class AsIso (op : Type -> Type -> Type) :=
{ asIso : forall {S A}, op S A -> Iso S A }.

Instance isoAsIso : AsIso Iso :=
{ asIso S A := id }.

Class AsLens (op : Type -> Type -> Type) :=
{ asLens : forall {S A}, op S A -> Lens S A }.

Instance lnToLens : AsLens Lens :=
{ asLens S A := id }.

Instance isoToLens `{AsIso op} : AsLens op :=
{ asLens S A := isoAsLens ∘ asIso }.

Class AsPrism (op : Type -> Type -> Type) :=
{ asPrism : forall {S A}, op S A -> Prism S A }.

Instance lnToPrism : AsPrism Prism :=
{ asPrism S A := id }.

Instance isoToPrism `{AsIso op} : AsPrism op :=
{ asPrism S A := isoAsPrism ∘ asIso }.

Class AsGetter (op : Type -> Type -> Type) :=
{ asGetter : forall {S A}, op S A -> Getter S A }.

Instance gtToGetter : AsGetter Getter :=
{ asGetter S A := id }.

Instance lnToGetter `{AsLens op} : AsGetter op :=
{ asGetter S A := lnAsGetter ∘ asLens }.

Class AsTraversal1 (op : Type -> Type -> Type) :=
{ asTraversal1 : forall {S A}, op S A -> Traversal1 S A }.

Instance tr1ToTraversal1 : AsTraversal1 Traversal1 :=
{ asTraversal1 S A := id }.

Instance lnToTraversal1 `{AsLens op} : AsTraversal1 op :=
{ asTraversal1 S A := lnAsTraversal1 ∘ asLens }.

Class AsAffineTraversal (op : Type -> Type -> Type) :=
{ asAffineTraversal : forall {S A}, op S A -> AffineTraversal S A }.

Instance atrToAffineTraversal : AsAffineTraversal AffineTraversal :=
{ asAffineTraversal S A := id }.

Instance lnToAffineTraversal `{AsLens op} : AsAffineTraversal op :=
{ asAffineTraversal S A := lnAsAffineTraversal ∘ asLens }.

Instance prToAffineTraversal `{AsPrism op} : AsAffineTraversal op :=
{ asAffineTraversal S A := prAsAffineTraversal ∘ asPrism }.

Class AsFold1 (op : Type -> Type -> Type) :=
{ asFold1 : forall {S A}, op S A -> Fold1 S A }.

Instance fl1ToFold1 : AsFold1 Fold1 :=
{ asFold1 S A := id }.

Instance gtToFold1 `{AsGetter op} : AsFold1 op :=
{ asFold1 S A := gtAsFold1 ∘ asGetter }.

Instance tr1ToFold1 `{AsTraversal1 op} : AsFold1 op :=
{ asFold1 S A := tr1AsFold1 ∘ asTraversal1 }.

Class AsAffineFold (op : Type -> Type -> Type) :=
{ asAffineFold : forall {S A}, op S A -> AffineFold S A }.

Instance fl1ToAffineFold : AsAffineFold AffineFold :=
{ asAffineFold S A := id }.

Instance gtToAffineFold `{AsGetter op} : AsAffineFold op :=
{ asAffineFold S A := gtAsAffineFold ∘ asGetter }.

Instance atrToAffineFold `{AsAffineTraversal op} : AsAffineFold op :=
{ asAffineFold S A := atrAsAffineFold ∘ asAffineTraversal }.

Class AsTraversal (op : Type -> Type -> Type) :=
{ asTraversal : forall {S A}, op S A -> Traversal S A }.

Instance fl1ToTraversal : AsTraversal Traversal :=
{ asTraversal S A := id }.

Instance atrToTraversal `{AsAffineTraversal op} : AsTraversal op :=
{ asTraversal S A := atrAsTraversal ∘ asAffineTraversal }.

Instance tr1ToTraversal `{AsTraversal1 op} : AsTraversal op :=
{ asTraversal S A := tr1AsTraversal ∘ asTraversal1 }.

Class AsSetter (op : Type -> Type -> Type) :=
{ asSetter : forall {S A}, op S A -> Setter S A }.

Instance stToSetter : AsSetter Setter :=
{ asSetter S A := id }.

Instance trToSetter `{AsTraversal op} : AsSetter op :=
{ asSetter S A := trAsSetter ∘ asTraversal }.

Class AsFold (op : Type -> Type -> Type) :=
{ asFold : forall {S A}, op S A -> Fold S A }.

Instance flToFold : AsFold Fold :=
{ asFold S A := id }.

Instance fl1ToFold `{AsFold1 op} : AsFold op :=
{ asFold S A := fl1AsFold ∘ asFold1 }.

Instance trToFold `{AsTraversal op} : AsFold op :=
{ asFold S A := trAsFold ∘ asTraversal }.

Instance aflToFold `{AsAffineFold op} : AsFold op :=
{ asFold S A := aflAsFold ∘ asAffineFold }.

(* VERTICAL COMPOSITION *)

Class VerCompose 
  (op1 : Type -> Type -> Type) 
  (op2 : Type -> Type -> Type)
  (res : Type -> Type -> Type) :=
{ verCompose : forall {S A B}, op1 S A -> op2 A B -> res S B }.

Notation "op1 › op2" := (verCompose op1 op2) (at level 50, left associativity).

Instance isoVerCom `{AsIso op1} `{AsIso op2} : VerCompose op1 op2 Iso :=
{ verCompose S A B op1 op2 :=
    isoVerCompose (asIso op1) (asIso op2)
}.

Instance lnVerCom `{AsLens op1} `{AsLens op2} : VerCompose op1 op2 Lens :=
{ verCompose S A B op1 op2 :=
    lnVerCompose (asLens op1) (asLens op2)
}.

Instance prVerCom `{AsPrism op1} `{AsPrism op2} : VerCompose op1 op2 Prism :=
{ verCompose S A B op1 op2 :=
    prVerCompose (asPrism op1) (asPrism op2)
}.

Instance gtVerCom `{AsGetter op1} `{AsGetter op2} : VerCompose op1 op2 Getter :=
{ verCompose S A B op1 op2 :=
    gtVerCompose (asGetter op1) (asGetter op2)
}.

Instance tr1VerCom `{AsTraversal1 op1} `{AsTraversal1 op2} : VerCompose op1 op2 Traversal1 :=
{ verCompose S A B op1 op2 :=
    tr1VerCompose (asTraversal1 op1) (asTraversal1 op2)
}.

Instance atrVerCom `{AsAffineTraversal op1} `{AsAffineTraversal op2} : VerCompose op1 op2 AffineTraversal :=
{ verCompose S A B op1 op2 :=
    atrVerCompose (asAffineTraversal op1) (asAffineTraversal op2)
}.

Instance fl1VerCom `{AsFold1 op1} `{AsFold1 op2} : VerCompose op1 op2 Fold1 :=
{ verCompose S A B op1 op2 :=
    fl1VerCompose (asFold1 op1) (asFold1 op2)
}.

Instance aflVerCom `{AsAffineFold op1} `{AsAffineFold op2} : VerCompose op1 op2 AffineFold :=
{ verCompose S A B op1 op2 :=
    aflVerCompose (asAffineFold op1) (asAffineFold op2)
}.

Instance trVerCom `{AsTraversal op1} `{AsTraversal op2} : VerCompose op1 op2 Traversal :=
{ verCompose S A B op1 op2 :=
    trVerCompose (asTraversal op1) (asTraversal op2)
}.

Instance flVerCom `{AsFold op1} `{AsFold op2} : VerCompose op1 op2 Fold :=
{ verCompose S A B op1 op2 :=
    flVerCompose (asFold op1) (asFold op2)
}.

Instance stVerCom `{AsSetter op1} `{AsSetter op2} : VerCompose op1 op2 Setter :=
{ verCompose S A B op1 op2 :=
    stVerCompose (asSetter op1) (asSetter op2)
}.

(* HORIZONTAL COMPOSITION *)

Class HorCompose
  (op1 : Type -> Type -> Type) 
  (op2 : Type -> Type -> Type)
  (res : Type -> Type -> Type) :=
{ horCompose : forall {S A B}, op1 S A -> op2 S B -> res S (prod A B) }.

(* digraph: 2h *)
Notation "op1 ⑂ op2" := (horCompose op1 op2) (at level 49, left associativity).

(* TODO: provide remaining instances *)

Instance flHorComp `{AsFold op1} `{AsFold op2} : HorCompose op1 op2 Fold :=
{ horCompose S A B fl1 fl2 := flHorCompose (asFold fl1) (asFold fl2)
}.

(* PRODUCT COMPOSITION *)

Class ProdCompose
  (op1 : Type -> Type -> Type) 
  (op2 : Type -> Type -> Type)
  (res : Type -> Type -> Type) :=
{ prodCompose : forall {S A B}, op1 S A -> op2 S B -> res S (prod A B) }.

Notation "op1 × op2" := (prodCompose op1 op2) (at level 48, left associativity).

(* TODO: provide remaining instances *)

Instance lnProdComp `{AsLens op1} `{AsLens op2} : ProdCompose op1 op2 Lens :=
{ prodCompose S A B ln1 ln2 := lnHorCompose (asLens ln1) (asLens ln2)
}.

Instance aflProdComp `{AsAffineFold op1} `{AsAffineFold op2} : ProdCompose op1 op2 AffineFold :=
{ prodCompose S A B afl1 afl2 := aflHorCompose (asAffineFold afl1) (asAffineFold afl2)
}.

Instance flProdComp `{AsFold op1} `{AsFold op2} : ProdCompose op1 op2 Fold :=
{ prodCompose S A B fl1 fl2 := flProCompose (asFold fl1) (asFold fl2)
}.

(* ACTIONS *)

Definition getAll {S A} `{AsFold op} (fl : op S A) : S -> list A :=
  foldMap (asFold fl) _ _ _ pure.

Definition getHead {S A} `{AsFold op} (fl : op S A) : S -> option A :=
  fun s => List.hd_error (getAll fl s).

Definition all {S A} `{AsFold op} (fl : op S A) (f : A -> bool) : S -> bool :=
  foldMap (asFold fl) _ _ _ f. 

Definition any {S A} `{AsFold op} (fl : op S A) (f : A -> bool) : S -> bool :=
  unwrapBool ∘ foldMap (asFold fl) _ _ _ (wrapBool ∘ f).

Definition contains {S A} `{AsFold op} `{Eq A} (fl : op S A) (a : A) : S -> bool :=
  any fl (eqb a).

(*******************)
(* COUPLES EXAMPLE *)
(*******************)

(* Data layer *)

Record Person := mkPerson
{ name : string
; age : nat
}.

Record Couple := mkCouple
{ her : Person
; him : Person
}.

Definition nameLn : Lens Person string :=
  mkLens name (fun s => mkPerson s ∘ age).

Definition ageLn : Lens Person nat :=
  mkLens age (fun n p => mkPerson (name p) n).

Definition herLn : Lens Couple Person :=
  mkLens her (fun p => mkCouple p ∘ him).

Definition himLn : Lens Couple Person :=
  mkLens him (fun p c => mkCouple (her c) p).

(* Logic *)

Definition getPeople : list Person -> list Person :=
  getAll each.

Definition getPeopleName : list Person -> list string :=
  getAll (each › nameLn).

Definition getPeopleNameAndAge : list Person -> list (string * nat) :=
  getAll (each › nameLn × ageLn).

Definition getPeopleGt30 : list Person -> list string :=
  getAll (each › nameLn × (ageLn › filtered (Nat.leb 30)) › fstLn).

Definition getPeopleGt30' : list Person -> list string :=
  getAll (each › nameLn × ageLn › filtered' (asGetter sndLn) (Nat.leb 30) › fstLn).

Definition getPeopleGt30'' : list Person -> list string :=
  getAll (each › filtered' (asGetter ageLn) (Nat.leb 30) › nameLn).

Definition subGt : Getter (nat * nat) nat := 
  mkGetter (fun ab => match ab with | (a, b) => a - b end).

Definition difference : list Couple -> list (string * nat) :=
  getAll (each › (herLn › nameLn) × 
    ((herLn › ageLn) × (himLn › ageLn) › subGt › filtered (Nat.ltb 0))).

Definition nat_in_range (x y n : nat) : bool :=
  Nat.leb x n && Nat.ltb n y.

Definition rangeFl (x y : nat) : Fold (list Person) string :=
  each › nameLn × (ageLn › filtered (nat_in_range x y)) › fstLn.

Definition rangeFl' (x y : nat) : Fold (list Person) string :=
  each › filtered' (asGetter ageLn) (nat_in_range x y) › nameLn.

Definition getAgeFl (s : string) : Fold (list Person) nat :=
  each › (nameLn › filtered (eqb s)) × ageLn › sndLn.

Definition compose (s t : string) (xs : list Person) : list string :=
  option_fold 
    (fun xy => match xy with | (x, y) => getAll (rangeFl x y) xs end) 
    List.nil 
    (getHead (getAgeFl s ⑂ getAgeFl t) xs).

(* Test *)

Open Scope string_scope.

Definition alex := mkPerson "Alex" 60.
Definition bert := mkPerson "Bert" 55.
Definition cora := mkPerson "Cora" 33.
Definition drew := mkPerson "Drew" 31.
Definition edna := mkPerson "Edna" 21.
Definition fred := mkPerson "Fred" 60.

Definition people : list Person :=
  alex :: bert :: cora :: drew :: edna :: fred :: List.nil.

Definition couples : list Couple :=
  mkCouple alex bert ::
  mkCouple cora drew ::
  mkCouple edna fred :: List.nil.

Example test1 : getPeople people = people.
Proof. auto. Qed.

Example test2 : getPeopleName people = List.map name people.
Proof. auto. Qed.

Example test3 : getPeopleNameAndAge people = List.map (fun p => (name p, age p)) people.
Proof. auto. Qed.

Example test4 : 
  getPeopleGt30 people = "Alex" :: "Bert" :: "Cora" :: "Drew" :: "Fred" :: List.nil.
Proof. auto. Qed.

Example test5 : getPeopleGt30 people = getPeopleGt30' people.
Proof. auto. Qed.

Example test6 : difference couples = ("Alex", 5) :: ("Cora", 2) :: List.nil.
Proof. auto. Qed.

Example test7 : getAll (rangeFl 30 40) people = "Cora" :: "Drew" :: List.nil.
Proof. auto. Qed.

Example test8 : getHead (getAgeFl "Alex") people = Some 60.
Proof. auto. Qed.

Example test9 : compose "Edna" "Bert" people = 
  (* XXX: almost there! It seems order is reversed somewhere... Therefore rev *)
  List.rev ("Edna" :: "Drew" :: "Cora" :: List.nil).
Proof. auto. Qed.

(* Department example *)

Definition Task : Type := string.

Record Employee := mkEmployee
{ emp : string
; tasks : list Task
}.

Record Department := mkDepartment
{ dpt : string
; employees : list Employee
}.

Definition NestedOrg := list Department.

Definition empGt := mkGetter emp.
Definition tasksGt := mkGetter tasks.
Definition dptGt := mkGetter dpt.  
Definition employeesGt := mkGetter employees.

Definition expertise (tsk : Task) : NestedOrg -> list string :=
  getAll (each › filtered' employeesGt (
    all each (contains (tasksGt › each) tsk)) › dptGt).

Definition employeeDepartment : NestedOrg -> list (string * string) :=
  getAll (each › dptGt × (employeesGt › each › empGt)).

(* ... which is different from the next version, where we compute the cartesian 
 * product among departments and employees. *)

Definition employeeDepartment' : NestedOrg -> list (string * string) :=
  getAll ((each › dptGt) × (each › employeesGt › each › empGt)).

Definition alex' := mkEmployee "Alex" ("build" :: List.nil).
Definition bert' := mkEmployee "Bert" ("build" :: List.nil).
Definition cora' := mkEmployee "Cora" ("abstract" :: "build" :: "design" :: List.nil).
Definition drew' := mkEmployee "Drew" ("abstract" :: "design" :: List.nil).
Definition edna' := mkEmployee "Edna" ("abstract" :: "call" :: List.nil).
Definition fred' := mkEmployee "Fred" ("call" :: List.nil).

Definition product := mkDepartment "Product" (alex' :: bert' :: List.nil).
Definition research := mkDepartment "Research" (cora' :: drew' :: edna' :: List.nil).
Definition sales := mkDepartment "Sales" (fred' :: List.nil).
Definition quality := mkDepartment "Quality" List.nil.

Definition org : NestedOrg :=
  product :: research :: sales :: quality :: List.nil.

Example test10 : expertise "abstract" org = "Research" :: "Quality" :: List.nil.
Proof. auto. Qed.

Example test11 : employeeDepartment org =
  ("Product", "Alex") :: 
    ("Product", "Bert") :: 
    ("Research", "Cora") :: 
    ("Research", "Drew") :: 
    ("Research", "Edna") :: 
    ("Sales", "Fred") :: List.nil.
Proof. auto. Qed.

