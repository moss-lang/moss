(* The consolidation of the functor-application rules (ob-consol) is
   acceptance-preserving where it says so. Each lemma here states, for one
   removed clause or restated definition, that the old form follows from
   (or equals) what the rule now asks. The old definitions are restated
   here verbatim, as they stood before the change.

   - BFunClosure's "G has a body": LinkBody states it ([link_body_has_body]).
   - BApplyAbstract's fresh-output `Fresh W (gensof t)`: FreshFor gives it
     (PublicFormation.freshfor_fresh).
   - ResultRealizes and LinkBody's sources: the renaming `link_renaming`
     was the identity on the keys, and `read_rename` at it is
     `role_reads` ([result_realizes_restated], [link_sources_restated]). *)
From Stdlib Require Import List.
From NM Require Import Audit.
From NM Require PublicTypeRead.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Lemma link_body_has_body : forall W P u gg h, A.LinkBody W P u gg h ->
  exists n ps a r b, A.ast_of W gg = Some (ADFunctor n ps a r (Some b)).
Proof.
  intros W P u gg h LB. destruct LB as [u gg h dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0 Dg Ast].
  exists n, ps, a, r, bodies. unfold A.ast_of. rewrite Dg. cbn. rewrite Ast. reflexivity.
Qed.

(* The definitions before the restatement. *)
Definition old_link_renaming (outs : list nat) (gen : nat -> nat) : subst :=
  map (fun t => (gen t, TAbs (gen t))) outs.
Definition old_read_rename (body : A.cx) (rename : subst) : subst :=
  map (fun q => (fst q, A.canon body (app_subst (A.c_ty body) (snd q)))) rename.
Definition OldResultRealizes (body promised : A.cx) (rename : subst) (keys : list item) : Prop :=
  forall k, In k keys -> exists required actual,
    A.ilook (A.provision_id k) (A.c_provisions promised) = Some required /\
    A.ilook (A.provision_id (A.at_site body (app_subst_item rename k))) (A.c_provisions body) = Some actual /\
    A.provision_refines
      (A.provision_subst (A.c_sub body) (A.provision_subst (old_read_rename body rename) required))
      (A.provision_subst (A.c_sub body) actual).

Lemma old_renaming_id : forall (gen : nat -> nat) outs t, app_subst (old_link_renaming outs gen) t = t.
Proof.
  intros gen outs t. rewrite <- (PublicTypeRead.app_subst_nil t) at 2. apply PublicTypeRead.subst_agree_on.
  intros x _. cbn [app_subst]. destruct (lookup_subst x (old_link_renaming outs gen)) as [v|] eqn:L; [|reflexivity].
  unfold old_link_renaming in L. induction outs as [|t0 r IHo]; [discriminate|]. cbn in L.
  destruct (PeanoNat.Nat.eqb_spec x (gen t0)) as [->|Ne]; [injection L as <-; reflexivity|exact (IHo L)].
Qed.

Lemma old_renaming_item_id : forall (gen : nat -> nat) outs k, app_subst_item (old_link_renaming outs gen) k = k.
Proof.
  intros gen outs [t|v|f|r m a|u]; try reflexivity. cbn. rewrite old_renaming_id. f_equal.
  unfold map_snd. induction a as [|[x t] a IH]; [reflexivity|]. cbn. rewrite old_renaming_id, IH. reflexivity.
Qed.

Lemma old_read_rename_roles : forall S outs gen,
  old_read_rename S (old_link_renaming outs gen) = A.role_reads S outs gen.
Proof. intros S outs gen. unfold old_read_rename, old_link_renaming, A.role_reads. rewrite map_map. reflexivity. Qed.

Theorem result_realizes_restated : forall S Cr outs gen ks,
  OldResultRealizes S Cr (old_link_renaming outs gen) ks <-> A.ResultRealizes S Cr (A.role_reads S outs gen) ks.
Proof.
  intros S Cr outs gen ks. unfold OldResultRealizes, A.ResultRealizes.
  rewrite old_read_rename_roles. split; intros H k Ik; rewrite ?old_renaming_item_id in *; specialize (H k Ik);
    rewrite ?old_renaming_item_id in H; exact H.
Qed.

Theorem link_sources_restated : forall S outs gen ks,
  map (fun k => A.rkey_of S (A.at_site S (app_subst_item (old_link_renaming outs gen) k))) ks =
  map (fun k => A.rkey_of S (A.at_site S k)) ks.
Proof. intros S outs gen ks. apply map_ext. intros k. rewrite old_renaming_item_id. reflexivity. Qed.
