(* Runtime type meanings carried alongside executable context entries. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss XCast.
Import ListNotations.
Set Implicit Arguments.

Definition types_match (g : subst) (A : list item) (d : xdenv) : Prop :=
 forall t, In (ITy t) A ->
 exists tau, lookup_xdenv (ITy t) d = Some (XDType tau) /\ tau = app_subst g (TAbs t).
Definition runtime_types (P : xprog) g A d : Prop :=
 xreify P = true -> types_match g A d.

Lemma lookup_type_xdtypes : forall d t tau,
 lookup_xdenv (ITy t) d = Some (XDType tau) -> lookup_subst t (xdtypes d) = Some tau.
Proof.
 induction d as [|[i en] d IH]; intros t tau H; simpl in *; [discriminate|].
 destruct i; simpl in H.
 - destruct (Nat.eqb t t0) eqn:E.
   + inversion H; subst; simpl; rewrite E; reflexivity.
   + destruct en; simpl; try apply IH; auto; rewrite E; apply IH; auto.
 - destruct en; simpl; apply IH; auto.
 - destruct en; simpl; apply IH; auto.
 - destruct en; simpl; apply IH; auto.
 - destruct en; simpl; apply IH; auto.
Qed.

Lemma types_match_lookup : forall g A d t,
 types_match g A d -> In (ITy t) A ->
 lookup_subst t (xdtypes d) = Some (app_subst g (TAbs t)).
Proof.
 intros g A d t H Hi; destruct (H t Hi) as [tau [Hl ->]]; eapply lookup_type_xdtypes; eauto.
Qed.

Lemma types_match_reads : forall g A d tau,
 types_match g A d -> covers g (tyreqs A) -> incl (ty_frees tau) (tyreqs A) ->
 xground d tau = app_subst g tau.
Proof.
 intros g A d tau H Hcov Hfree; unfold xground; apply app_subst_frees_agree.
 intros t Ht; specialize (Hfree t Ht).
 assert (In (ITy t) A) as Hi.
 { unfold tyreqs in Hfree; apply in_flat_map in Hfree as [i [Hi Ht']].
   destruct i; simpl in Ht'; try contradiction; destruct Ht' as [<-|[]]; exact Hi. }
 rewrite (types_match_lookup t H Hi).
 unfold covers in Hcov; destruct (Hcov t Hfree) as [a Ha]; simpl; rewrite Ha; reflexivity.
Qed.

Lemma types_match_restrict : forall g A B d,
 incl A B -> types_match g B d -> types_match g A d.
Proof. intros g A B d Hi H t Ht; apply H,Hi,Ht. Qed.

Lemma types_match_reground : forall g g' A d,
 (forall t, In (ITy t) A -> app_subst g (TAbs t) = app_subst g' (TAbs t)) ->
 types_match g A d -> types_match g' A d.
Proof.
 intros g g' A d He H t Hi; destruct (H t Hi) as [tau [Hl Htau]].
 exists tau; split; auto; rewrite Htau; apply He,Hi.
Qed.

Lemma types_match_app : forall g A d dc,
 types_match g A dc -> types_match g A (dc ++ d).
Proof.
 intros g A d dc H t Hi; destruct (H t Hi) as [tau [Hl Htau]]; exists tau; split; auto.
 clear H; revert Hl; induction dc as [|[i en] dc IH]; simpl; [discriminate|].
 destruct i; simpl; try (intros Hl; apply IH; exact Hl).
 destruct (Nat.eqb t t0); intros Hl; [exact Hl|apply IH; exact Hl].
Qed.

Lemma types_match_close : forall g A d dc,
 types_match g A dc -> types_match g A (xclose d dc).
Proof. intros; unfold xclose; apply types_match_app; auto. Qed.

Lemma types_match_type_env : forall g A d,
 types_match g A d -> types_match g A (xtype_env d).
Proof.
 intros g A d H t Hi; destruct (H t Hi) as [tau [Hl Htau]]; exists tau; split; auto.
 clear H; revert Hl; induction d as [|[i en] d IH]; simpl; [discriminate|].
 destruct i, en; simpl; try (intros Hl; apply IH; exact Hl).
 all: destruct (Nat.eqb t t0); intros Hl; try discriminate; try exact Hl; apply IH; exact Hl.
Qed.

Definition typefree (d : xdenv) : Prop :=
 Forall (fun p => is_ty_item (fst p) = false) d.
Lemma typefree_lookup : forall d t, typefree d -> lookup_xdenv (ITy t) d = None.
Proof.
 intros d t H; induction H as [|[i en] d Hi H IH]; simpl; auto.
 destruct i; simpl in *; try discriminate; exact IH.
Qed.
Lemma lookup_xdenv_app_typefree : forall d dc t,
 typefree dc -> lookup_xdenv (ITy t) (dc ++ d) = lookup_xdenv (ITy t) d.
Proof.
 intros d dc t H; induction H as [|[i en] dc Hi H IH]; simpl; auto.
 destruct i; simpl in *; try discriminate; exact IH.
Qed.
Lemma types_match_prefix : forall g A d dc,
 typefree dc -> types_match g A d -> types_match g A (dc ++ d).
Proof.
 intros g A d dc Hfree H t Hi; destruct (H t Hi) as [tau [Hl Htau]]; exists tau; split; auto.
 rewrite lookup_xdenv_app_typefree; auto.
Qed.
Lemma xcompose_typefree : forall d sm dc,
 Forall (fun p => is_ty_item (fst p) = false) sm ->
 xcompose d sm = Some dc -> typefree dc.
Proof.
 intros d sm; induction sm as [|[i j] sm IH]; intros dc Hf Hc; simpl in Hc.
 - inversion Hc; constructor.
 - inversion Hf; subst; destruct (lookup_xdenv j d); try discriminate.
   destruct (xcompose d sm) eqn:E; try discriminate; inversion Hc; subst; constructor; auto.
   eapply IH; eauto.
Qed.
