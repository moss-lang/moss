(* Producer probes separate source inputs from caller generations before
   matching a result receiver. Instantiating those temporary roles
   recovers every successful reading of the same result. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaBudget EaFamily EaTypes EaTypesSound.
From NM Require AuditFacts EaAssumesSound EaContextTableSound EaFamilyComplete EaFamilyMatching EaFamilyUseSound EaMain EaMainSymbols
  EaDeclarationSound EaOutputSound EaResultBudget EaResultCoverage EaSatisfiesSound
  PublicContextFacts PublicCopySubst PublicFamilyCoherent
  PublicFormation PublicOutputSubst PublicSubstFacts PublicTypeRead.
Import Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration ListNotations.
Local Open Scope list_scope.

Section Markers.
Variables (E : env) (C : cx).

Local Notation marker := (producer_marker E C).

Local Lemma marker_injective : forall x y, marker x = marker y -> x = y.
Proof. intros x y Equal. unfold producer_marker in Equal. lia. Qed.

Local Lemma marker_fresh : forall x, ~ In (marker x) (producer_marker_support C).
Proof.
  intros x Present. apply (EaAssumesSound.scope_floor_support _ (e_metadata_fuel E)) in Present.
  unfold producer_marker in Present. lia.
Qed.

Local Lemma support_row : forall rows g t y,
  (rows = c_ty C \/ rows = c_sub C) -> In (g, t) rows ->
  In y (g :: gens t) -> In y (producer_marker_support C).
Proof.
  intros rows g t y Location Row Uses. unfold producer_marker_support.
  apply in_flat_map. exists (g, t). split; [|exact Uses].
  apply in_app_iff. destruct Location as [-> | ->]; auto.
Qed.

Local Lemma marker_not_substituted : forall x, lookup_subst (marker x) (c_sub C) = None.
Proof.
  intro x. destruct (lookup_subst (marker x) (c_sub C)) as [t|] eqn:Read; [|reflexivity].
  exfalso. apply (marker_fresh x).
  apply PublicSubstFacts.lookup_subst_in_pair in Read.
  eapply support_row; [now right|exact Read|now left].
Qed.

Local Lemma canon_marker : forall x, canon C (TAbs (marker x)) = TAbs (marker x).
Proof. intro x. unfold canon. cbn [app_subst]. now rewrite marker_not_substituted. Qed.

Local Definition marker_values (marks : list nat) (actual : subst) : subst :=
  map (fun x => (marker x,
    match nlook x actual with Some t => canon C t | None => TAbs (marker x) end)) marks.

Local Lemma marker_values_domain : forall marks actual g,
  In g (map fst (marker_values marks actual)) <-> In g (map marker marks).
Proof.
  intros marks actual g. unfold marker_values. now rewrite map_map.
Qed.

Local Lemma marker_values_read : forall marks actual x,
  In x marks ->
  lookup_subst (marker x) (marker_values marks actual) =
    Some (match nlook x actual with Some t => canon C t | None => TAbs (marker x) end).
Proof.
  induction marks as [|y marks IH]; intros actual x Present; [contradiction|].
  cbn [marker_values map lookup_subst].
  destruct (Nat.eqb_spec (marker x) (marker y)) as [Equal|Different].
  - now apply marker_injective in Equal as ->.
  - apply IH. destruct Present as [Equal|Present]; [congruence|exact Present].
Qed.

Local Lemma marker_values_fixed : forall marks actual g t,
  PublicContextFacts.CxSection C ->
  lookup_subst g (marker_values marks actual) = Some t ->
  exists x, g = marker x /\ canon C t = t.
Proof.
  intros marks actual g t Section Read.
  apply PublicSubstFacts.lookup_subst_in_pair in Read.
  unfold marker_values in Read.
  apply in_map_iff in Read as (x & Equal & Present).
  inversion Equal. subst g t. exists x. split; [reflexivity|].
  destruct (nlook x actual); [now apply PublicContextFacts.section_canonical|apply canon_marker].
Qed.

Local Lemma marker_values_miss : forall marks actual g,
  In g (producer_marker_support C) -> lookup_subst g (marker_values marks actual) = None.
Proof.
  intros marks actual g Supported.
  destruct (lookup_subst g (marker_values marks actual)) as [t|] eqn:Read; [|reflexivity].
  exfalso. apply PublicSubstFacts.lookup_subst_in_pair in Read.
  unfold marker_values in Read. apply in_map_iff in Read as (x & Equal & _).
  inversion Equal. subst g. now apply (marker_fresh x).
Qed.

Local Lemma probe_subst_commutes : forall marks actual t,
  PublicContextFacts.CxSection C ->
  canon C (app_subst (marker_values marks actual) (canon C t)) =
  app_subst (marker_values marks actual ++ c_sub C) t.
Proof.
  intros marks actual t Section. set (rows := marker_values marks actual).
  unfold canon. rewrite <- (PublicTypeRead.app_subst_nil (app_subst (rows ++ c_sub C) t)).
  apply PublicTypeRead.subst3_agree. intros g _.
  rewrite PublicTypeRead.app_subst_nil. cbn [app_subst].
  rewrite PublicCopySubst.lookup_subst_cat.
  destruct (lookup_subst g rows) as [v|] eqn:Filled.
  - destruct (marker_values_fixed marks actual g v Section Filled) as (x & -> & Fixed).
    rewrite marker_not_substituted. cbn [app_subst]. now rewrite Filled.
  - destruct (lookup_subst g (c_sub C)) as [v|] eqn:Merge.
    + assert (NoMarkers : app_subst rows v = v).
      { rewrite <- (PublicTypeRead.app_subst_nil v) at 2.
        apply PublicTypeRead.subst_agree_on. intros y Present. cbn [app_subst lookup_subst].
        unfold rows. rewrite (marker_values_miss marks actual y); [reflexivity|].
        apply PublicSubstFacts.lookup_subst_in_pair in Merge.
        eapply support_row; [now right|exact Merge|now right]. }
      rewrite NoMarkers.
      pose proof (PublicContextFacts.section_canonical C Section (TAbs g)) as Fixed.
      unfold canon in Fixed. cbn [app_subst] in Fixed. now rewrite Merge in Fixed.
    + cbn [app_subst]. rewrite Filled. cbn [app_subst]. now rewrite Merge.
Qed.

Local Lemma marking_read : forall marks x,
  nlook x (rev (producer_markings E C marks)) =
    if in_dec Nat.eq_dec x marks then Some (TAbs (marker x)) else None.
Proof.
  intros marks x. unfold producer_markings. rewrite <- map_rev.
  induction marks as [|y marks IH]; [reflexivity|].
  cbn [rev]. rewrite map_app, PublicTypeRead.nlook_app', IH. cbn [map nlook].
  destruct (in_dec Nat.eq_dec x marks) as [Present|Absent];
    destruct (in_dec Nat.eq_dec x (y :: marks)) as [Present'|Absent'];
    try solve [reflexivity | exfalso; apply Absent'; now right].
  - destruct (Nat.eqb_spec x y) as [->|Different]; [reflexivity|].
    destruct Present' as [Equal|Present]; [congruence|contradiction].
  - destruct (Nat.eqb_spec x y) as [->|Different]; [exfalso; apply Absent'; now left|reflexivity].
Qed.

Local Lemma actual_read_unmarked : forall marks th x,
  Forall (fun q => In (fst q) marks) th -> ~ In x marks ->
  nlook x (rev th ++ c_ty C) = nlook x (c_ty C).
Proof.
  intros marks th x Marked Absent. rewrite PublicTypeRead.nlook_app'.
  destruct (nlook x (rev th)) as [t|] eqn:Read; [|reflexivity].
  exfalso. apply PublicTypeRead.nlook_in in Read. apply <- in_rev in Read.
  apply Forall_forall with (x := (x, t)) in Marked; [now apply Absent|exact Read].
Qed.

Local Lemma probe_tables : forall marks th,
  let actual := rev th ++ c_ty C in
  let marked := rev (producer_markings E C marks) ++ c_ty C in
  PublicContextFacts.CxSection C ->
  Forall (fun q => In (fst q) marks) th ->
  PublicTypeRead.Agree (c_sub C) (marker_values marks actual ++ c_sub C) actual marked /\
  forall x, ~ In x marks -> (nlook x actual = None <-> nlook x marked = None).
Proof.
  intros marks th actual marked Section Marked.
  assert (MarkedRead : forall x, nlook x marked =
    if in_dec Nat.eq_dec x marks then Some (TAbs (marker x)) else nlook x (c_ty C)).
  { intros x. unfold marked. rewrite PublicTypeRead.nlook_app', marking_read.
    now destruct (in_dec Nat.eq_dec x marks). }
  split.
  - intros x v1 v2 ActualRead MarkedValue. rewrite MarkedRead in MarkedValue.
    destruct (in_dec Nat.eq_dec x marks) as [Present|Absent].
    + injection MarkedValue as <-. cbn [app_subst]. rewrite PublicCopySubst.lookup_subst_cat.
      now rewrite marker_values_read, ActualRead.
    + assert (Values : Some v1 = Some v2).
      { etransitivity; [symmetry; exact ActualRead|].
        etransitivity; [exact (actual_read_unmarked marks th x Marked Absent)|exact MarkedValue]. }
      injection Values as <-. fold (canon C v1). rewrite <- probe_subst_commutes by exact Section.
      assert (NoMarkers : app_subst (marker_values marks actual) (canon C v1) = canon C v1).
      { rewrite <- (PublicTypeRead.app_subst_nil (canon C v1)) at 2.
        apply PublicTypeRead.subst_agree_on. intros y Present. cbn [app_subst lookup_subst].
        rewrite (marker_values_miss marks actual y); [reflexivity|].
        unfold canon in Present. apply PublicSubstFacts.gens_subst in Present as
          (g & Occurs & [[_ ->]|(t & Read & Inner)]).
        - apply PublicTypeRead.nlook_in in MarkedValue.
          eapply support_row; [now left|exact MarkedValue|now right].
        - apply PublicSubstFacts.lookup_subst_in_pair in Read.
          eapply support_row; [now right|exact Read|now right]. }
      rewrite NoMarkers. symmetry. now apply PublicContextFacts.section_canonical.
  - intros x Absent. rewrite MarkedRead.
    destruct (in_dec Nat.eq_dec x marks); [contradiction|].
    rewrite <- (actual_read_unmarked marks th x Marked Absent). reflexivity.
Qed.

Local Lemma restricted_marker_domain : forall marks actual receiver g,
  In g (map fst (filter (fun q => nmem (fst q) (gens receiver)) (marker_values marks actual))) <->
    In g (map marker marks) /\ In g (gens receiver).
Proof.
  intros marks actual receiver g. split.
  - intros Present. apply in_map_iff in Present as ([x t] & <- & Present).
    apply filter_In in Present as [Row Used]. cbn [fst] in Used.
    split; [exact (proj1 (marker_values_domain marks actual x) (in_map fst _ _ Row))|now apply nmem_spec in Used].
  - intros [Marker Used]. apply (proj2 (marker_values_domain marks actual g)) in Marker.
    apply in_map_iff in Marker as ([x t] & <- & Row).
    apply in_map_iff. exists (x, t). split; [reflexivity|].
    apply filter_In. split; [exact Row|now apply nmem_spec].
Qed.

Local Lemma restrict_marker_values : forall (rows : subst) receiver,
  app_subst (filter (fun q => nmem (fst q) (gens receiver)) rows) receiver =
    app_subst rows receiver.
Proof.
  intros rows receiver. apply PublicTypeRead.subst_agree_on.
  intros g Used. apply nmem_spec in Used. cbn [app_subst].
  induction rows as [|[key value] rest IH]; [reflexivity|].
  cbn [filter fst lookup_subst]. destruct (Nat.eqb_spec g key) as [<-|Different].
  - rewrite Used. cbn [lookup_subst]. now rewrite Nat.eqb_refl.
  - destruct (nmem key (gens receiver)); cbn [lookup_subst];
      [rewrite (proj2 (Nat.eqb_neq g key) Different)|]; exact IH.
Qed.

End Markers.

Section ProducerCoverage.
Variable E : env.
Let W := e_W E.
Variable bound : nat.
Hypothesis Bounded : forall symbol d, w_decl W symbol = Some d -> symbol < bound.
Hypothesis Budget : metadata_budget W bound < e_metadata_fuel E.
Hypothesis Checks : EaMain.decls_form E bound = Ok tt.
Hypothesis TypedMentions : PublicFormation.MentionsTyped W.

Local Lemma family_poly_kind : forall u x,
  FamilyShape W u -> Poly W u x -> kind_sym W x = Some DKType.
Proof.
  intros u x (d & n & ps & args & results & Declared & Ast & _ & Names)
    (d' & n' & ps' & args' & results' & Declared' & Ast' & Marked).
  rewrite Declared in Declared'. injection Declared' as <-.
  rewrite Ast in Ast'. injection Ast' as <- <- <- <-.
  destruct ps as [names|].
  - destruct Marked as (name & Present & Resolved). eapply Names; eauto.
  - now apply (TypedMentions u x).
Qed.

Local Lemma family_result_outputs_empty : forall u outs,
  FamilyShape W u -> (forall x, In x outs <-> FunOutputs W u x) -> outs = [].
Proof.
  intros u [|x rest] Shape Exact; [reflexivity|].
  exfalso. apply (PublicFormation.family_no_outputs W u x Shape).
  apply Exact. now left.
Qed.

Local Lemma result_key_method : forall C es keys receiver method,
  ResultKeys (c_sub C) es keys -> In (mkey receiver method) keys ->
  exists raw sig app, In (NMth raw method sig app) es /\ receiver = canon C raw.
Proof.
  intros C es keys receiver method Keys Present. unfold ResultKeys in Keys. subst keys.
  apply in_flat_map in Present as (e & Row & Key).
  destruct e; cbn [entry_key] in Key; try contradiction;
    destruct Key as [Equal|[]]; try discriminate.
  inversion Equal; subst. eauto.
Qed.

Local Lemma marked_method : forall C u th receiver method result keys marked_result marked_keys,
  PublicContextFacts.CxSection C ->
  FamilyShape W u ->
  Forall (fun q => Poly W u (fst q)) th ->
  ProvideResult W u (family_context C th) (fun t => TAbs t) result keys ->
  In (mkey receiver method) keys ->
  ProvideResult W u (family_context C (producer_markings E C (poly W (e_metadata_fuel E) u)))
    (fun t => TAbs t) marked_result marked_keys ->
  exists formal matching,
    In (mkey formal method) marked_keys /\
    read_off_one C (map (producer_marker E C) (poly W (e_metadata_fuel E) u)) formal receiver = Some matching.
Proof.
  intros C u th receiver method result keys marked_result marked_keys Section Shape PolyRows
    (d & ps & args & results & body & outs & es & meanings & Declared & Parts & Outputs & Entries & Keys & _)
    Present
    (d' & ps' & args' & results' & body' & outs' & es' & meanings' & Declared' & Parts' & Outputs' & Entries' & Keys' & _).
  rewrite Declared in Declared'. injection Declared' as <-.
  rewrite Parts in Parts'. injection Parts' as <- <- <- <-.
  rewrite (family_result_outputs_empty u outs Shape Outputs) in Entries.
  rewrite (family_result_outputs_empty u outs' Shape Outputs') in Entries'.
  cbn [output_type_map map app] in Entries, Entries'.
  set (marks := poly W (e_metadata_fuel E) u) in *.
  destruct (PublicFormation.family_context_shape th C) as [Actual [ActualSub _]].
  destruct (PublicFormation.family_context_shape (producer_markings E C marks) C) as [Marked [MarkedSub _]].
  rewrite Actual in Entries. rewrite Marked in Entries'.
  unfold canon in Entries, Entries'.
  rewrite ActualSub in Entries, Keys. rewrite MarkedSub in Entries', Keys'.
  assert (MarkedRows : Forall (fun q => In (fst q) marks) th).
  { apply Forall_forall. intros q Row. unfold marks.
    apply (EaResultBudget.poly_exact W bound Bounded _ u Budget).
    apply Forall_forall with (x := q) in PolyRows; assumption. }
  set (rows := marker_values E C marks (rev th ++ c_ty C)).
  destruct (probe_tables E C marks th Section MarkedRows) as [Agreement Domain].
  set (actual_table := PublicTypeRead.smap (c_sub C) (rev th ++ c_ty C)).
  set (marked_table := PublicTypeRead.smap (c_sub C) (rev (producer_markings E C marks) ++ c_ty C)).
  assert (CanonicalAgreement : PublicTypeRead.Agree (c_sub C) (rows ++ c_sub C) actual_table marked_table).
  { intros x t t' First Second. unfold actual_table, marked_table in First, Second.
    rewrite PublicTypeRead.nlook_smap in First, Second.
    destruct (nlook x (rev th ++ c_ty C)) as [v|] eqn:Read; [|discriminate].
    destruct (nlook x (rev (producer_markings E C marks) ++ c_ty C)) as [v'|] eqn:Probe; [|discriminate].
    injection First as <-. injection Second as <-.
    pose proof (Agreement x v v' Read Probe) as Same.
    pose proof (PublicContextFacts.section_canonical C Section) as Idempotent.
    change (canon C (canon C v) = app_subst (rows ++ c_sub C) (canon C v')).
    rewrite Idempotent. unfold rows in *. rewrite <- !probe_subst_commutes in Same |- * by exact Section.
    now rewrite Idempotent. }
  assert (NoGap : PublicOutputSubst.ReceiverDomainGap W actual_table marked_table False).
  { intros x NotType Different. apply Different.
    assert (NotMarker : ~ In x marks).
    { intro Marker. apply NotType. eapply family_poly_kind; [exact Shape|].
      eapply (EaResultBudget.poly_exact W bound Bounded _ u Budget). exact Marker. }
    unfold actual_table, marked_table. rewrite !PublicTypeRead.nlook_smap.
    assert (NoneMap : forall value : option ty,
      option_map (app_subst (c_sub C)) value = None <-> value = None).
    { intros [value|]; cbn; [split; discriminate|reflexivity]. }
    rewrite !NoneMap. exact (Domain x NotMarker). }
  pose proof (proj2 (PublicOutputSubst.output_entries_rel_under W (c_sub C) (rows ++ c_sub C) False)
    _ _ _ _ _ _ Entries _ _ _ _ Entries' (Forall2_nil _) CanonicalAgreement NoGap) as Related.
  destruct (result_key_method C es keys receiver method Keys Present) as (raw & sg & a & Row & ->).
  destruct (PublicFamilyCoherent.Forall2_In_l' _ _ _ _ _ _ Related Row) as (entry & MarkedRow & Relation).
  destruct entry as [| | |marked_raw marked_method marked_sig marked_app| |];
    cbn [PublicOutputSubst.EntryRelUnder] in Relation; try contradiction.
  destruct Relation as [<- [_ [[Receiver _]|Impossible]]]; [|contradiction].
  unfold PublicOutputSubst.TyRel in Receiver. fold (canon C raw) in Receiver.
  set (formal := canon C marked_raw).
  assert (Instantiated : EaFamilyMatching.instantiated C rows formal = canon C raw).
  { unfold EaFamilyMatching.instantiated, formal, rows. rewrite probe_subst_commutes by exact Section.
    symmetry. exact Receiver. }
  set (relevant := filter (fun q => nmem (fst q) (gens formal)) rows).
  assert (Relevant : EaFamilyMatching.instantiated C relevant formal = canon C raw).
  { unfold EaFamilyMatching.instantiated, relevant. now rewrite restrict_marker_values. }
  destruct (EaFamilyMatching.read_off_one_complete C (map (producer_marker E C) marks)
    formal (canon C raw) relevant Section (restricted_marker_domain E C marks _ formal) Relevant)
    as (matching & Match & _).
  exists formal, matching. split; [|exact Match].
  unfold ResultKeys in Keys'. subst marked_keys. apply in_flat_map.
  eexists. split; [exact MarkedRow|now left].
Qed.

Local Lemma result_entries_cover_methods :
  (forall p ob tm sp es tm', OutputItemEntries W p ob tm sp es tm' ->
    forall depth rt m sig inst,
      EaFamilyComplete.ResultDepth E depth p sp -> In (NMth rt m sig inst) es ->
      result_has_method E depth p m sp = true) /\
  (forall p ob tm sps es tm', OutputItemsEntries W p ob tm sps es tm' ->
    forall depth rt m sig inst,
      Forall (EaFamilyComplete.ResultDepth E depth p) sps -> In (NMth rt m sig inst) es ->
      existsb (result_has_method E depth p m) sps = true).
Proof.
  apply (PublicFormation.oies_mut W).
  - intros p ob tm pa a bs t Resolve Kind Brackets depth rt m sig inst Depth [Equal|[]].
    discriminate Equal.
  - intros p ob tm pa a bs s k Resolve Kind Brackets Other depth rt m sig inst Depth [].
  - intros p ob tm a pa s d n t ty0 bs ap Resolve Declared Ast Brackets TyRead App
      depth rt m sig inst Depth [Equal|[]]. discriminate Equal.
  - intros p ob tm a pa s g bs ap d rcv n ps rt Resolve Kind Declared Ast Brackets Sig App
      depth recv m sig inst Depth [Equal|[]]. discriminate Equal.
  - intros p ob tm a bs pa u d n ps args res fx Resolve Declared Ast Brackets Fixed Readings
      depth rt m sig inst Depth [Equal|[]]. discriminate Equal.
  - intros p ob tm a pa dn r m rt g bs ap Resolve Named Brackets Receiver Sig App
      depth recv method sig inst Depth [Equal|[]].
    inversion Equal; subst. destruct depth as [|depth]; [contradiction|].
    cbn [result_has_method]. fold W.
    rewrite Resolve, (EaResultCoverage.method_named_complete W _ _ _ _ Named). apply Nat.eqb_refl.
  - intros p ob tm a pa c d n items specs bs es tm' Resolve Declared Ast Syntax Brackets Inner IH
      depth rt m sig inst Depth Present.
    subst items. destruct depth as [|depth]; [contradiction|].
    cbn [result_has_method EaFamilyComplete.ResultDepth] in Depth |- *. fold W in Depth |- *.
    rewrite Resolve, Declared, Ast, AuditFacts.citem_specs_map in Depth |- *.
    exact (IH depth rt m sig inst Depth Present).
  - intros p ob tm depth rt m sig inst Depth [].
  - intros p ob tm sp rest es1 tm1 es2 tm2 First IHfirst Tail IHtail depth rt m sig inst Depth Present.
    inversion Depth as [|? ? DepthFirst DepthTail]; subst. cbn [existsb].
    apply orb_true_iff. apply in_app_iff in Present as [Head|TailRow].
    + left. exact (IHfirst depth rt m sig inst DepthFirst Head).
    + right. exact (IHtail depth rt m sig inst DepthTail TailRow).
Qed.

Local Lemma declaration_result_depth : forall u d ps args results body,
  kind_sym W u = Some DKFunctor ->
  w_decl W u = Some d -> functor_parts (d_ast d) = Some (ps, args, results, body) ->
  Forall (EaFamilyComplete.ResultDepth E (e_fuel E) (d_mod d)) results.
Proof.
  intros u d ps args results body Kind Declared Parts.
  destruct (EaDeclarationSound.checked_functor_result_read E bound u Bounded Checks Kind)
    as (C & out & C' & keys & Read).
  unfold provide_result, functor_decl in Read. fold W in Read.
  rewrite Declared, Parts in Read. cbn [rbind] in Read.
  apply rbind_ok in Read as ([entries meanings] & Items & _).
  eapply (proj2 (EaFamilyComplete.result_depth_from_reads E (e_fuel E))). exact Items.
Qed.

Local Lemma failed_probe_covers : forall C u th receiver method result keys,
  FamilyShape W u ->
  ProvideResult W u (family_context C th) (fun t => TAbs t) result keys ->
  In (mkey receiver method) keys ->
  functor_has_result_method E u method = true.
Proof.
  intros C u th receiver method result keys Shape
    (d & ps & args & results & body & outs & es & meanings & Declared & Parts & Outputs & Entries & Keys & _)
    Present.
  assert (Kind : kind_sym W u = Some DKFunctor).
  { destruct Shape as (d' & n & p & a & r & Declared' & Ast & _).
    unfold kind_sym. now rewrite Declared', Ast. }
  destruct (result_key_method _ es keys receiver method Keys Present) as (rt & sig & inst & Row & _).
  unfold functor_has_result_method, functor_decl. fold W. rewrite Declared, Parts.
  eapply (proj2 result_entries_cover_methods); [exact Entries| |exact Row].
  eapply declaration_result_depth; eassumption.
Qed.

Theorem family_produces_covers : forall C u receiver method,
  PublicContextFacts.CxSection C ->
  FamilyProduces W C u (mkey receiver method) ->
  family_produces E C u (mkey receiver method) = true.
Proof.
  intros C u receiver method Section (fx & th & result & keys & Provision & Family & Rows & Provide & Present).
  change (ProvideResult W u (family_context C th) (fun t => TAbs t) result keys) in Provide.
  cbn [family_produces mkey]. rewrite EaSatisfiesSound.cfun_exact, Provision.
  cbn [is_some andb]. rewrite (EaFamilyComplete.decls_form_family_classifier E bound Bounded Budget Checks u Family).
  cbn [andb]. destruct (probe_producer E C u receiver method) as [matches|error] eqn:Probe.
  - unfold probe_producer in Probe. apply rbind_ok in Probe as ([marked marked_keys] & Read & Equal).
    injection Equal as <-.
    apply (EaOutputSound.provide_result_sound_at_budget E bound Bounded Budget) in Read.
    destruct (marked_method C u th receiver method result keys marked marked_keys Section (proj1 Family)
      Rows Provide Present Read) as (formal & matching & Key & Match).
    apply existsb_exists. exists (mkey formal method). split; [exact Key|].
    change (Nat.eqb method method &&
      is_some (read_off_one C (map (producer_marker E C) (poly W (e_metadata_fuel E) u)) formal receiver) = true).
    now rewrite Nat.eqb_refl, Match.
  - apply orb_true_iff. right. eapply failed_probe_covers; [exact (proj1 Family)|exact Provide|exact Present].
Qed.

Theorem producers_cover : forall C u receiver method,
  PublicContextFacts.CxSection C ->
  FamilyProduces W C u (mkey receiver method) ->
  In u (producers E C (mkey receiver method)).
Proof.
  intros C u receiver method Section Produces. unfold producers.
  apply filter_In. split.
  - destruct Produces as (fx & th & result & keys & Provision & _).
    apply EaMainSymbols.uniq_members. eapply EaSatisfiesSound.cfun_lookup_in_keys. exact Provision.
  - now apply family_produces_covers.
Qed.

Theorem checked_family_use :
  EaContextTableSound.ContextTableSound W -> EaFamilyUseSound.FamilyUseSound E.
Proof.
  intro Contexts.
  apply (EaFamilyUseSound.family_for_sound E bound Bounded Budget Contexts).
  - now apply (EaFamilyMatching.family_read_off_sound E bound).
  - exact producers_cover.
Qed.

End ProducerCoverage.
