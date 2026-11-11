(* A family demand checks a source application and then emits its keys.
   Receiver matching and producer coverage are the independent obligations. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit EaBase EaBudget EaFamily EaTypesSound.
From NM Require AuditFacts EaCallExpressionSound EaContextTableSound EaDefinedCallSound EaFamilySound
  EaMergeSound EaSatisfiesSound PublicCallRealize PublicContextFacts PublicFormation.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Definition FamilyMatchingSound (E : env) : Prop :=
  forall C u receiver method matching,
    PublicContextFacts.CxSection C ->
    family_read_off E C u receiver method = Ok matching ->
    FamilyReadOff (e_W E) C u receiver method matching.

Definition ProducersCover (E : env) : Prop :=
  forall C u receiver method,
    PublicContextFacts.CxSection C ->
    FamilyProduces (e_W E) C u (mkey receiver method) ->
    In u (producers E C (mkey receiver method)).

Definition FamilyUseSound (E : env) : Prop :=
  forall C0 C receiver method u matching arguments sm result keys,
    PublicContextFacts.CxSection C0 ->
    PublicContextFacts.CxSection C ->
    family_for E C0 C receiver method = Ok (u, matching, arguments, sm, result, keys) ->
    exists formal needs,
      TypesFamilyApplication (e_W E) C u matching arguments formal needs result keys /\
      FamilyReadOff (e_W E) C u receiver method matching /\
      In (mkey receiver method) keys /\ OnlyFamily (e_W E) C0 u (mkey receiver method) /\
      MatchingInForce C matching /\
      sm = satisfaction_map (family_context C matching) formal arguments needs.

Lemma matching_in_force_sound : forall C matching,
  matching_in_force C matching = true -> MatchingInForce C matching.
Proof.
  intros C matching Checked row y Present Generation.
  unfold matching_in_force in Checked.
  apply forallb_forall with (x := row) in Checked; [|exact Present].
  apply forallb_forall with (x := y) in Checked; [|exact Generation].
  apply existsb_exists in Checked as ([x t] & Found & Read).
  exists x, t. split; [exact Found|]. now apply nmem_spec in Read.
Qed.

Theorem family_for_sound : forall E bound,
  (forall symbol d, w_decl (e_W E) symbol = Some d -> symbol < bound) ->
  metadata_budget (e_W E) bound < e_metadata_fuel E ->
  EaContextTableSound.ContextTableSound (e_W E) ->
  FamilyMatchingSound E -> ProducersCover E -> FamilyUseSound E.
Proof.
  intros E bound Bounded Enough Contexts Match Cover.
  intros C0 C receiver method u matching arguments sm result keys InitialSection Section Run.
  unfold family_for in Run.
  destruct (producers E C0 (mkey receiver method)) as [|chosen [|other rest]] eqn:Producers;
    try discriminate.
  apply rbind_ok in Run as (mt & Read & Run).
  apply rbind_ok in Run as ([ [[th input_map] formed] provided ] & App & Run).
  apply rbind_ok in Run as (present & Present & Run).
  destruct (imem (mkey receiver method) provided) eqn:Membership; [|discriminate Present].
  apply rbind_ok in Run as (checked & Force & Run).
  destruct (matching_in_force C mt) eqn:InForce; [|discriminate Force].
  inversion Run; subst u matching arguments sm result keys.
  destruct (EaFamilySound.family_application_sound E bound C chosen mt th input_map formed provided
    Bounded Enough Contexts Section App) as (formal & needs & Typed & Map).
  exists formal, needs. split; [exact Typed|].
  split; [eapply Match; eassumption|]. split; [now apply EaMergeSound.imem_spec|].
  split.
  - intros u Different Produces.
    pose proof (Cover _ _ _ _ InitialSection Produces) as Candidate.
    rewrite Producers in Candidate. destruct Candidate as [Equal|[]]. congruence.
  - split; [now apply matching_in_force_sound|exact Map].
Qed.

Local Lemma restored_family_section : forall W C u matching arguments formal needs result keys,
  PublicContextFacts.CxSection C ->
  TypesFamilyApplication W C u matching arguments formal needs result keys ->
  PublicContextFacts.CxSection (family_restore C result).
Proof.
  intros W C u matching arguments formal needs result keys Section App.
  destruct App as (fixed & _ & _ & _ & _ & _ & _ & _ & _ & Provide & _).
  destruct (PublicFormation.provide_result_shape _ _ _ _ _ _ Provide) as [Sub _].
  unfold PublicContextFacts.CxSection, family_restore. cbn [c_sub c_merge].
  rewrite Sub, (PublicContextFacts.provided_result_merge _ _ _ _ _ _ Provide).
  apply EaFamilySound.family_context_section. exact Section.
Qed.

Theorem demand_go_sound : forall E,
  FamilyUseSound E -> forall rest C0 used wanted C wrapper called,
  PublicContextFacts.CxSection C0 ->
  PublicContextFacts.CxSection C ->
  demand_go E C0 used wanted rest C = Ok (wrapper, called) ->
  exists steps, Demand (e_W E) C0 wanted used C steps called /\
    PublicContextFacts.CxSection called /\
    forall body, wrapper body = translate_demands C steps body.
Proof.
  intros E Families rest. induction rest as [|key rest IH];
    intros C0 used wanted C wrapper called InitialSection Section Run.
  - cbn [demand_go] in Run. inversion Run; subst wrapper called.
    exists []. split; [apply DemandDone|split; [exact Section|reflexivity]].
  - destruct key as [type|value|fn|receiver method inst|functor]; cbn [demand_go] in Run;
      try (eapply IH; eassumption).
    destruct (negb (is_some (cmth C0 receiver method)) && negb (is_some (cmth C receiver method))
      && imem (mkey receiver method) wanted &&
      negb (is_nil (producers E C0 (mkey receiver method)))) eqn:Enabled;
      [|eapply IH; eassumption].
    apply andb_true_iff in Enabled as [Checks _].
    apply andb_true_iff in Checks as [Missing Needed].
    apply andb_true_iff in Missing as [NotInitial _].
    apply EaMergeSound.imem_spec in Needed.
    apply negb_true_iff in NotInitial.
    destruct (cmth C0 receiver method) as [signature|] eqn:Initial; [discriminate NotInitial|].
    apply rbind_ok in Run as ([ [[[[u matching] arguments] sm] result] keys ] & Family & Run).
    destruct (Families _ _ _ _ _ _ _ _ _ _ InitialSection Section Family)
      as (formal & needs & App & Read & Provided & Only & InForce & Map).
    apply rbind_ok in Run as (fresh & Fresh & Run).
    destruct (existsb (fun q => Nat.eqb (fst q) u && subst_eqb (snd q) matching) used)
      eqn:Existing; [discriminate Fresh|].
    apply rbind_ok in Run as ([tail final] & Rest & Run).
    inversion Run; subst wrapper called.
    destruct (IH _ _ _ _ _ _ InitialSection (restored_family_section _ _ _ _ _ _ _ _ _ Section App) Rest)
      as (steps & Tail & Final & Emit).
    exists (FamilyUse u matching arguments formal needs result keys :: steps).
    split.
    + eapply DemandApply.
      * intro Present.
        assert (Repeated : existsb (fun q => Nat.eqb (fst q) u && subst_eqb (snd q) matching) used = true).
        { apply existsb_exists. exists (u, matching). split; [exact Present|].
          cbn [fst snd]. rewrite Nat.eqb_refl. apply PublicCallRealize.subst_eqb_spec. reflexivity. }
        congruence.
      * eapply FamilyStepApply; try eassumption.
        now rewrite <- EaCallExpressionSound.cmth_exact.
      * exact Tail.
    + split; [exact Final|]. intro body.
      cbn [translate_demands demand_result]. unfold translate_family_use.
      rewrite Emit, Map. f_equal. apply map_ext. apply EaSatisfiesSound.rkof_exact.
Qed.

Theorem demand_sound_from_family_reads : forall E,
  FamilyUseSound E -> EaDefinedCallSound.DemandSound E.
Proof.
  intros E Family C wanted wrapper called Section Run.
  unfold demand in Run. eapply demand_go_sound; eassumption.
Qed.
