(* Core formation supplies the weak local shape used by schema substitution.
   This does not strengthen formation of generic tag payload templates. *)
From Stdlib Require Import List.
From NM Require Import CoreMoss PayloadInst.
Import ListNotations.

Lemma wf_payload_shape : forall tau Sg Phi,
    wf_ty Sg Phi tau -> payload_shape tau.
Proof.
  induction tau using payload_type_ind; intros Sg Phi Hwf.
  - constructor.
  - constructor.
  - constructor. rewrite Forall_forall in H |- *. intros [key tau] Hin.
    apply (H (key,tau) Hin Sg Phi). inversion Hwf; subst;
      match goal with Hp : Forall (fun p => wf_ty _ _ (snd p)) _ |- _ =>
        rewrite Forall_forall in Hp; exact (Hp (key,tau) Hin) end.
  - inversion Hwf; subst. constructor.
    + rewrite Forall_forall in H |- *. intros t Hin. apply (H t Hin Sg Phi).
      match goal with Hp : Forall (wf_ty _ _) _ |- _ =>
        rewrite Forall_forall in Hp; exact (Hp t Hin) end.
    + change (Forall is_nom ms).
      match goal with Hnom : Forall is_nom _ |- _ => exact Hnom end.
Qed.

Lemma payload_covered_from_frees : forall template th,
    (forall t, In t (ty_frees template) -> exists actual, lookup_subst t th = Some actual) ->
    payload_covered th template.
Proof.
  induction template using payload_type_ind; intros th Hcov.
  - constructor.
  - destruct (Hcov t (or_introl eq_refl)) as [actual Hactual]. econstructor. exact Hactual.
  - constructor. rewrite Forall_forall in H |- *. intros [key tau] Hin.
    apply (H (key,tau) Hin th). intros t Ht. apply Hcov.
    change (In t (flat_map (fun p => ty_frees (snd p)) args)).
    apply in_flat_map. exists (key,tau). auto.
  - constructor. rewrite Forall_forall in H |- *. intros tau Hin.
    apply (H tau Hin th). intros t Ht. apply Hcov.
    change (In t (flat_map ty_frees ms)). apply in_flat_map. exists tau. auto.
Qed.

Lemma payload_covered_frees : forall template th,
    payload_covered th template -> incl (ty_frees template) (map fst th).
Proof.
  induction template using payload_type_ind; intros th Hcov u Ht.
  - contradiction.
  - cbn in Ht. destruct Ht as [<-|[]]. inversion Hcov; subst.
    apply in_map_iff. exists (t,actual). split; [reflexivity|].
    eapply callee_env_agree_aux1; eassumption.
  - inversion Hcov; subst. change (In u (flat_map (fun p => ty_frees (snd p)) args)) in Ht.
    apply in_flat_map in Ht as [p [Hp Ht]]. rewrite Forall_forall in H.
    apply (H p Hp th); [|exact Ht].
    match goal with Hargs : Forall (fun p => payload_covered _ (snd p)) _ |- _ =>
      rewrite Forall_forall in Hargs; exact (Hargs p Hp) end.
  - inversion Hcov; subst. change (In u (flat_map ty_frees ms)) in Ht.
    apply in_flat_map in Ht as [tau [Hin Ht]]. rewrite Forall_forall in H.
    apply (H tau Hin th); [|exact Ht].
    match goal with Hmembers : Forall (payload_covered _) _ |- _ =>
      rewrite Forall_forall in Hmembers; exact (Hmembers tau Hin) end.
Qed.

Lemma ordinary_payload_commutes : forall P template th g,
    payload_covered th template -> ordinary_payload_ok P th template ->
    app_subst g (ordinary_payload P th template) =
      ordinary_payload P (map_snd (app_subst g) th) template.
Proof.
  intros P template th g Hcov Hok. unfold ordinary_payload.
  destruct (xp_immutable P) eqn:Mode.
  - apply payload_inst_commutes; [exact Hcov|exact (proj2 (Hok Mode))].
  - apply app_subst_chain. apply payload_covered_frees. exact Hcov.
Qed.

Lemma ordinary_payload_nil_shape_identity : forall P template,
    payload_shape template -> ordinary_payload P [] template = template.
Proof.
  intros P template Hshape. unfold ordinary_payload. destruct (xp_immutable P).
  - apply payload_inst_nil_shape_identity. exact Hshape.
  - apply app_subst_nil.
Qed.
