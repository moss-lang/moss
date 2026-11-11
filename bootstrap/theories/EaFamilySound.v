(* The executable family classifier implies Audit's family conditions. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit EaBase EaTypes EaFamily EaTypesSound EaMentionsSound
  EaDependencyFuel EaResultBudget EaOutputSound EaMetadataSound EaMain EaWorldSound ElabWorld.
From NM Require PublicContextFacts PublicTypeFacts.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Section FamilyShape.
Variable E : env.
Let W := e_W E.

Theorem family_result_sound : forall n p syntax,
  family_result E n p syntax = true -> FamilyResult W p syntax.
Proof.
  induction n as [|n IH]; intros p [[pa|] [detached|] application] Checked; try discriminate.
  - apply FRMethod.
  - cbn [family_result] in Checked. fold W in Checked.
    destruct (resolve_sym W p pa) as [symbol|] eqn:Resolve; [|discriminate].
    destruct (w_decl W symbol) as [d|] eqn:Declaration; [|discriminate].
    destruct (d_ast d) as [items declarations|name|name syntax|name parameters payload|name|name value_type initial|
      receiver name parameters result body|name items|name parameters arguments results body] eqn:Ast;
      try discriminate.
    destruct (ctx_spec_items items) as [specs|] eqn:Specs; [|discriminate].
    eapply FRContext; [exact Resolve|exact Declaration|exact Ast|apply ctx_spec_items_sound; exact Specs|].
    apply Forall_forall. intros child Member. apply IH.
    apply (proj1 (forallb_forall _ _) Checked child Member).
Qed.

Theorem is_family_shape_sound : forall symbol,
  is_family E symbol = true -> FamilyShape W symbol.
Proof.
  intros symbol Checked. unfold is_family in Checked. fold W in Checked.
  destruct (w_decl W symbol) as [d|] eqn:Declaration; [|discriminate].
  destruct (d_ast d) as [items declarations|name|name syntax|name parameters payload|name|name value_type initial|
    receiver name parameters result body|name items|name parameters arguments results body] eqn:Ast;
    try discriminate.
  destruct body as [body|]; [discriminate|].
  apply andb_true_iff in Checked as [Shape Keyed]. apply andb_true_iff in Shape as [Results Names].
  exists d, name, parameters, arguments, results. split; [exact Declaration|]. split; [exact Ast|]. split.
  - apply Forall_forall. intros child Member. apply (family_result_sound (e_fuel E)).
    exact (proj1 (forallb_forall _ _) Results child Member).
  - intros names x t Params Member Resolve.
    subst parameters. pose proof (proj1 (forallb_forall _ _) Names x Member) as Kind.
    cbn beta in Kind. rewrite Resolve in Kind. apply kind_is_spec. exact Kind.
Qed.

Theorem family_bracket_free_sound : forall p application x,
  family_bracket_free E p application x = true -> FamilyBracketFree W p application x.
Proof.
  intros p application x Checked. unfold family_bracket_free in Checked. fold W in Checked.
  apply existsb_exists in Checked as [[bp [[pa|] detached bindings]] [Member Present]]; try discriminate.
  exists bp, pa, detached, bindings. split; [exact Member|].
  eapply free_in_sound. apply nmem_spec. exact Present.
Qed.

Theorem family_bracket_free_complete : forall p application x,
  largest (fun b => spec_depth (snd b)) (bracket_list application) <= e_metadata_fuel E ->
  FamilyBracketFree W p application x -> family_bracket_free E p application x = true.
Proof.
  intros p application x Depth (bp & pa & detached & bindings & Member & Free).
  unfold family_bracket_free. fold W. apply existsb_exists.
  exists (bp, Spec (Some pa) detached bindings). split; [exact Member|].
  apply nmem_spec. eapply free_in_complete_above; [exact Free|].
  pose proof (largest_member _ (fun b => spec_depth (snd b))
    (bracket_list application) (bp, Spec (Some pa) detached bindings) Member) as Child.
  cbn [snd] in Child. rewrite spec_depth_ref in Child. lia.
Qed.

End FamilyShape.

Section FamilyKeys.
Variable E : env.
Let W := e_W E.
Let metadata_fuel := e_metadata_fuel E.
Variable bound : nat.
Hypothesis declarations_bounded : forall symbol d, w_decl W symbol = Some d -> symbol < bound.
Hypothesis enough_metadata : metadata_budget W bound < metadata_fuel.

Lemma metadata_depth_enough : metadata_depth W bound < metadata_fuel.
Proof. unfold metadata_budget in enough_metadata. lia. Qed.

Theorem fixed_agreement_spec : forall C u,
  PublicContextFacts.CxSection C ->
  fixed_agreement E C u = true <-> FixedAgreement W C u.
Proof.
  intros C u Section. unfold fixed_agreement, FixedAgreement.
  rewrite forallb_forall. fold W metadata_fuel.
  assert (Read : forall x,
    (match capp C (IFun u), nlook x (c_ty C) with
     | Some recorded, Some now =>
         match nlook x recorded with
         | Some (Some was) => ty_eqb (canon C was) (canon C now)
         | _ => true
         end
     | _, _ => true
     end) = true <->
    match ilook (IFun u) (c_apps C), nlook x (c_ty C) with
    | Some recorded, Some now =>
        match nlook x recorded with
        | Some (Some was) => MergeEq (c_merge C) was now
        | _ => True
        end
    | _, _ => True
    end).
  { intro x. unfold capp. rewrite <- PublicContextFacts.capps_ilook.
    destruct (ilook (IFun u) (c_apps C)) as [recorded|], (nlook x (c_ty C)) as [now|];
      try (split; [intros _; exact I | intros _; reflexivity]).
    destruct (nlook x recorded) as [[was|]|];
      try (split; [intros _; exact I | intros _; reflexivity]).
    rewrite PublicTypeFacts.ty_eqb_spec. apply PublicContextFacts.context_canon_iff_mergeeq.
    exact Section. }
  split; intros Ag x Member; apply Read, Ag;
    apply (fixed_exact W bound declarations_bounded metadata_fuel u enough_metadata x);
    exact Member.
Qed.

Lemma family_keyed_sound : forall n symbol p syntax,
  spec_depth syntax <= metadata_depth W bound ->
  family_keyed E n symbol (poly W metadata_fuel symbol) p syntax = true ->
  FamilyKeyed W symbol p syntax.
Proof.
  induction n as [|n IH]; intros symbol p [[pa|] [detached|] application] Syntax Checked; try discriminate.
  - cbn [family_keyed] in Checked. fold W metadata_fuel in Checked.
    destruct (resolve_sym W p pa) as [receiver|] eqn:Resolve; [|discriminate].
    destruct (method_named W p receiver detached) as [method|] eqn:Method; [|discriminate].
    eapply FKMethod; [exact Resolve|eapply method_named_sound; exact Method|].
    intros x Poly Mention NotBound.
    assert (Member : In x (poly W metadata_fuel symbol)).
    { apply (proj2 (poly_exact W bound declarations_bounded metadata_fuel symbol enough_metadata x)), Poly. }
    pose proof (proj1 (forallb_forall _ _) Checked x Member) as Keyed.
    cbn beta zeta in Keyed.
    assert (Unbound : nmem x (bracket_bound W p application) = false).
    { apply nmem_false_spec. intro Bound.
      apply NotBound, bracket_bound_members. exact Bound. }
    assert (Mentioned : nmem x (sig_mentions W metadata_fuel method) || family_bracket_free E p application x = true).
    { apply orb_true_iff. destruct Mention as [Signature|Bracket].
      - left. apply nmem_spec.
        assert (Depth : signature_depth W method <= metadata_fuel).
        { pose proof (signatures_depth_bounded W bound declarations_bounded method).
          pose proof metadata_depth_enough. lia. }
        apply (proj2 (sig_mentions_exact W method metadata_fuel Depth x)), Signature.
      - right. apply family_bracket_free_complete; [|exact Bracket].
        cbn [spec_depth] in Syntax.
        destruct application; cbn [bracket_list largest fold_right] in *; unfold largest in *; pose proof metadata_depth_enough; lia. }
    rewrite Mentioned, Unbound in Keyed. cbn [negb andb] in Keyed.
    eapply free_in_sound. apply nmem_spec. exact Keyed.
  - cbn [family_keyed] in Checked. fold W metadata_fuel in Checked.
    destruct (resolve_sym W p pa) as [symbol_context|] eqn:Resolve; [|discriminate].
    destruct (w_decl W symbol_context) as [d|] eqn:Declaration; [|discriminate].
    destruct (d_ast d) as [items declarations|name|name ty|name parameters payload|name|name value_type initial|
      receiver name parameters result body|name items|name parameters arguments results body] eqn:Ast;
      try discriminate.
    destruct (ctx_spec_items items) as [specs|] eqn:Specs; [|discriminate].
    assert (Items : items = map CISpec specs) by (apply ctx_spec_items_sound; exact Specs).
    eapply FKContext; [exact Resolve|exact Declaration|exact Ast|exact Items|].
    apply Forall_forall. intros child Member. apply IH.
    + eapply contexts_depth_bounded; [exact declarations_bounded|exact Declaration|exact Ast|].
      rewrite Items. apply in_map. exact Member.
    + exact (proj1 (forallb_forall _ _) Checked child Member).
Qed.

Theorem is_family_sound : forall symbol,
  is_family E symbol = true -> Family W symbol.
Proof.
  intros symbol Checked. split; [exact (is_family_shape_sound E symbol Checked)|].
  unfold FamilyKeys. intros d name parameters arguments results Declaration Ast.
  unfold is_family in Checked. fold W metadata_fuel in Checked. rewrite Declaration, Ast in Checked.
  apply andb_true_iff in Checked as [_ Keyed]. apply Forall_forall. intros child Member.
  apply (family_keyed_sound (e_fuel E)).
  - eapply results_depth_bounded; [exact declarations_bounded|exact Declaration|exact Ast|exact Member].
  - exact (proj1 (forallb_forall _ _) Keyed child Member).
Qed.

End FamilyKeys.

Corollary bundle_world_r_is_family_sound : forall bundle W bound,
  EaMain.bundle_world_r bundle = Ok (W, bound) ->
  forall contexts strategy modules mains,
  let E := Env (ElabWorld.with_context W contexts) strategy modules mains
    EaMain.elab_fuel (EaMain.metadata_fuel W bound) in
  forall symbol, is_family E symbol = true -> Family (e_W E) symbol.
Proof.
  intros bundle W bound Run contexts strategy modules mains. cbn zeta.
  apply (is_family_sound _ bound).
  - exact (EaWorldSound.bundle_world_r_bound _ _ _ Run).
  - exact (metadata_fuel_sufficient W bound).
Qed.
