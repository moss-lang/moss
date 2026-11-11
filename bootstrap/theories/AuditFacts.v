(* Facts about the definitions in Audit.v that the proofs use. Audit.v
   holds only definitions, so that a reader of the trusted statement need
   not read any proof; check.sh rejects a proof in it. *)

From Stdlib Require Import String List Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.

(* Context items. *)

Lemma citem_specs_map : forall l, flat_map citem_specs (map CISpec l) = l.
Proof. induction l as [|s l IH]; [reflexivity|cbn; rewrite IH; reflexivity]. Qed.

Lemma map_CISpec_inj : forall a b, map CISpec a = map CISpec b -> a = b.
Proof.
  induction a as [|x a IH]; intros [|y b] E; try discriminate; [reflexivity|].
  injection E as -> E. rewrite (IH b E). reflexivity.
Qed.

Lemma ctx_specs_map : forall l, ctx_specs (map CISpec l) = l.
Proof. induction l as [|s l IH]; [reflexivity|]. change (s :: ctx_specs (map CISpec l) = s :: l). rewrite IH. reflexivity. Qed.

Lemma ctx_specs_le : forall its, length (ctx_specs its) <= length its.
Proof.
  induction its as [|[s|l r] its IH]; [constructor| |].
  - change (S (length (ctx_specs its)) <= S (length its)). apply le_n_S. exact IH.
  - change (length (ctx_specs its) <= S (length its)). apply le_S. exact IH.
Qed.

Lemma ctx_specs_all : forall its, length (ctx_specs its) = length its -> its = map CISpec (ctx_specs its).
Proof.
  induction its as [|[s|l r] its IH]; intro E; [reflexivity| |].
  - change (S (length (ctx_specs its)) = S (length its)) in E. injection E as E.
    change (CISpec s :: its = CISpec s :: map CISpec (ctx_specs its)). f_equal. exact (IH E).
  - exfalso. change (length (ctx_specs its) = S (length its)) in E.
    pose proof (ctx_specs_le its) as L. rewrite E in L. exact (Nat.nle_succ_diag_l _ L).
Qed.

Lemma in_ctx_specs : forall sp its, In sp (ctx_specs its) <-> In (CISpec sp) its.
Proof.
  intros sp its. unfold ctx_specs. rewrite in_flat_map. split.
  - intros [[s0|l r] [I E]]; [destruct E as [<-|[]]; exact I|destruct E].
  - intro I. exists (CISpec sp). split; [exact I|left; reflexivity].
Qed.

(* The rest are stated, as in Audit.v's Elaboration, with implicit
   arguments. *)
Set Implicit Arguments.

(* Imports. *)

Lemma explicit_in_imports : forall b m t imp,
  In (t, imp) (explicit_imports m) -> In (t, imp) (imports_of b m).
Proof.
  intros b m t imp H. unfold imports_of, explicit_imports in *.
  apply in_or_app. right. exact H.
Qed.

Lemma prelude_in_imports : forall b m, am_prelude m = true ->
  In (b_prelude b, AImport (b_prelude b) None true []) (imports_of b m).
Proof. intros b m H. unfold imports_of. rewrite H. left. reflexivity. Qed.

(* Every visible binding still originates in an export, either here or
   in a dependency. Clients that only need declaration provenance need
   not depend on the priority rules used to select that binding. *)
Lemma scope_exports : forall b W p ns n e, InScope b W p ns n e ->
  Exports b W p ns n e \/ exists m t imp, module_of b p = Some m /\
    In (t, imp) (imports_of b m) /\ Exports b W t ns n e.
Proof.
  intros b W p ns n e H. destruct H as [p ns n e Hordinary | p m ns n e Hm Hpre Hexp Hnone].
  - destruct Hordinary as [p ns n e Hexp | p m t imp ns n e Hm Hin Hg Hexp].
    + left. exact Hexp.
    + right. exists m, t, imp. split; [exact Hm|].
      split; [apply explicit_in_imports; exact Hin|exact Hexp].
  - right. exists m, (b_prelude b), (AImport (b_prelude b) None true []).
    split; [exact Hm|]. split; [apply prelude_in_imports; exact Hpre|exact Hexp].
Qed.

(* Ascending lists, as in SigOutside. *)

Lemma ascending_tail : forall x r, Ascending (x :: r) -> Ascending r.
Proof. intros x [|y r] H; [exact I | exact (proj2 H)]. Qed.
Lemma ascending_lt : forall r x, Ascending (x :: r) -> forall y, In y r -> x < y.
Proof.
  induction r as [|z r IH]; intros x H y Hy; [destruct Hy|]. destruct H as [Hxz Hr].
  destruct Hy as [<-|Hy]; [exact Hxz | exact (Nat.lt_trans _ _ _ Hxz (IH z Hr y Hy))].
Qed.

(* An ascending list is fixed by its members. *)
Lemma ascending_det : forall l1 l2, Ascending l1 -> Ascending l2 ->
  (forall t, In t l1 <-> In t l2) -> l1 = l2.
Proof.
  induction l1 as [|x r1 IH]; intros [|y r2] A1 A2 E.
  - reflexivity.
  - exfalso. exact (proj2 (E y) (or_introl eq_refl)).
  - exfalso. exact (proj1 (E x) (or_introl eq_refl)).
  - assert (Exy : x = y).
    { destruct (proj1 (E x) (or_introl eq_refl)) as [->|Hx]; [reflexivity|].
      destruct (proj2 (E y) (or_introl eq_refl)) as [->|Hy]; [reflexivity|].
      exfalso. exact (Nat.lt_asymm _ _ (ascending_lt r2 y A2 x Hx) (ascending_lt r1 x A1 y Hy)). }
    subst y. f_equal. apply IH; [exact (ascending_tail _ _ A1) | exact (ascending_tail _ _ A2)|].
    intro t. split; intro Ht.
    + destruct (proj1 (E t) (or_intror Ht)) as [Ex|H]; [|exact H].
      exfalso. rewrite <- Ex in Ht. exact (Nat.lt_irrefl _ (ascending_lt r1 x A1 x Ht)).
    + destruct (proj2 (E t) (or_intror Ht)) as [Ex|H]; [|exact H].
      exfalso. rewrite <- Ex in Ht. exact (Nat.lt_irrefl _ (ascending_lt r2 x A2 x Ht)).
Qed.

Lemma sig_outside_det : forall W s e1 e2, SigOutside W s e1 -> SigOutside W s e2 -> e1 = e2.
Proof.
  intros W s e1 e2 [A1 M1] [A2 M2]. apply ascending_det; [exact A1 | exact A2|].
  intro t. rewrite M1, M2. reflexivity.
Qed.

(* Safety lemmas can relate the static family check to its emitted map. *)
Definition FamilyApplication (W : world) (C : cx) (u : usym)
    (matching arguments : subst) (inputs : smap) (result : cx) (keys : list item) : Prop :=
  exists formal needs,
    TypesFamilyApplication W C u matching arguments formal needs result keys /\
    inputs = satisfaction_map (family_context C matching) formal arguments needs.

Lemma family_application_with_map : forall W C u matching th formal needs Cr ks,
  TypesFamilyApplication W C u matching th formal needs Cr ks ->
  FamilyApplication W C u matching th
    (satisfaction_map (family_context C matching) formal th needs) Cr ks.
Proof. intros. exists formal, needs. split; [assumption|reflexivity]. Qed.
