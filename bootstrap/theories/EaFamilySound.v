(* The executable family classifier implies Audit's family conditions. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require AuditFacts.
From NM Require Import Audit EaBase EaTypes EaFamily EaTypesSound EaMentionsSound
  EaDependencyFuel EaResultBudget EaOutputSound EaMetadataSound EaMain EaWorldSound ElabWorld.
From NM Require EaCall EaContextTableSound EaMergeSound EaSatisfiesSound PublicContextFacts PublicFormation PublicTypeFacts.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
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

Local Lemma family_context_section : forall C matching,
  PublicContextFacts.CxSection C ->
  PublicContextFacts.CxSection (family_context C matching).
Proof.
  intros C matching Section. unfold PublicContextFacts.CxSection in *.
  rewrite PublicContextFacts.family_context_merge.
  destruct (PublicFormation.fold_set_ty_shape matching C) as [_ [Sub _]].
  unfold family_context. now rewrite Sub.
Qed.

Local Lemma inodupb_sound : forall keys, inodupb keys = true -> NoDup keys.
Proof.
  induction keys as [|key rest IH]; intro Checked; [constructor|].
  cbn [inodupb] in Checked. apply andb_true_iff in Checked as [New Distinct].
  constructor; [|exact (IH Distinct)].
  intro Member. apply EaMergeSound.imem_spec in Member. now rewrite Member in New.
Qed.

Theorem family_application_sound : forall E bound C u matching arguments inputs result keys,
  (forall symbol d, w_decl (e_W E) symbol = Some d -> symbol < bound) ->
  metadata_budget (e_W E) bound < e_metadata_fuel E ->
  EaContextTableSound.ContextTableSound (e_W E) ->
  PublicContextFacts.CxSection C ->
  family_application E C u matching = Ok (arguments, inputs, result, keys) ->
  AuditFacts.FamilyApplication (e_W E) C u matching arguments inputs result keys.
Proof.
  intros E bound C u matching arguments inputs result keys
    Bounded Budget Contexts Section Run.
  unfold family_application in Run. cbv zeta in Run.
  apply rbind_ok in Run as (available & Read & Run).
  destruct (cfun C u) as [fixed|] eqn:Available; [|discriminate Read].
  injection Read as <-.
  apply rbind_ok in Run as (checked & Read & Run).
  destruct (is_family E u) eqn:IsFamily; [|discriminate Read].
  clear Read.
  apply rbind_ok in Run as (checked_poly & Read & Run).
  destruct (forallb (fun q => nmem (fst q) (poly (e_W E) (e_metadata_fuel E) u)) matching)
    eqn:Polymorphic; [|discriminate Read].
  clear Read.
  apply rbind_ok in Run as ([formal needs] & Read & Run).
  destruct (w_context (e_W E) u) as [[formal' needs']|] eqn:Context; [|discriminate Read].
  injection Read as <- <-.
  set (caller := family_context C matching) in *.
  set (th := map (fun q => (fst q, canon caller (app_subst (c_ty caller) (TAbs (fst q))))) (c_ty formal')) in *.
  apply rbind_ok in Run as (bound_types & Read & Run).
  destruct (find (fun q => negb (bound_in caller (fst q))) (c_ty formal'))
    as [[missing missing_type]|] eqn:InForce; [discriminate Read|].
  clear Read.
  apply rbind_ok in Run as (fixed_types & Read & Run).
  destruct (fixed_agreement E caller u) eqn:Fixed; [|discriminate Read].
  clear Read.
  apply rbind_ok in Run as (sm & CheckedNeeds & Run).
  apply rbind_ok in Run as ([C' ks] & Provided & Run).
  apply rbind_ok in Run as (distinct & Read & Run).
  destruct (inodupb (map provision_id ks)) eqn:Distinct; [|discriminate Read].
  injection Run as <- <- <- <-.
  pose proof (is_family_sound _ bound Bounded Budget _ IsFamily) as FamilyFormed.
  assert (Kind : kind_sym (e_W E) u = Some DKFunctor).
  { destruct FamilyFormed as ((d & name & parameters & args & outputs & Declaration & Shape & _) & _).
    unfold kind_sym. rewrite Declaration, Shape. reflexivity. }
  pose proof (Contexts _ _ _ Context) as Formed. rewrite Kind in Formed.
  apply EaSatisfiesSound.satisfies_sound in CheckedNeeds;
    [|eapply EaSatisfiesSound.functx_subst_functional; exact Formed].
  destruct CheckedNeeds as [InputMap Needs].
  exists formal', needs'. split; [|exact InputMap].
  unfold TypesFamilyApplication. cbn zeta. fold caller.
  exists fixed. split; [now rewrite <- EaSatisfiesSound.cfun_exact|].
  split; [exact FamilyFormed|].
  split.
  { apply Forall_forall. intros q Present.
    apply forallb_forall with (x := q) in Polymorphic; [|exact Present].
    apply nmem_spec in Polymorphic.
    apply (EaResultBudget.poly_exact _ bound Bounded _ u Budget). exact Polymorphic. }
  split; [exact Formed|].
  split; [reflexivity|].
  split.
  { intros t Present Absent. apply in_map_iff in Present as [[x ty] [Equal Present]].
    cbn [fst] in Equal. subst t.
    change (nlook x (c_ty caller) = None) in Absent.
    pose proof (find_none _ _ InForce (x, ty) Present) as Bound.
    apply negb_false_iff in Bound. unfold bound_in in Bound.
    change (is_some (nlook x (c_ty caller)) = true) in Bound.
    rewrite Absent in Bound. discriminate Bound. }
  split.
  { eapply (fixed_agreement_spec E bound Bounded Budget); [|exact Fixed].
    now apply family_context_section. }
  split; [exact Needs|].
  split.
  - eapply provide_result_sound_at_budget; eassumption.
  - now apply inodupb_sound.
Qed.
