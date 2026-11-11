(* A successful copy checks the source provisions and their result positions
   at one fresh reading of the target telescope. *)
From Stdlib Require Import List Bool String Arith Lia Sorting.Sorted.
From NM Require Import Audit EaBase EaBudget EaCall EaFunctor EaTypes EaTypesSound.
From NM Require EaAllocationSound EaClosureSound EaContextFuel EaContextTableSound EaControlSound
  EaCopyNeedSound EaDeclarationSound EaDependencyFuel EaFamilySound EaLinkBodySound EaMain EaMainSymbols
  EaOutputSound EaResultBudget EaRootSound EaSatisfiesSound PublicCallRealize PublicContextFacts
  PublicTypeFacts PublicTypeRead.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Local Lemma pure_read : forall A (read : er A) before value after,
  pure read before = Ok (value, after) -> read = Ok value /\ after = before.
Proof. intros A [x|] before value after Run; [now injection Run as <- <-|discriminate]. Qed.

Local Lemma mmap_checks : forall A B (step : A -> M B) (R : A -> B -> Prop),
  (forall x y before after, step x before = Ok (y, after) -> R x y) ->
  forall values results before after,
    mmap step values before = Ok (results, after) -> Forall2 R values results.
Proof.
  intros A B step R Sound values. induction values as [|first rest IH]; intros results before after Run.
  - injection Run as <- _. constructor.
  - cbn [mmap] in Run.
    apply EaControlSound.mbind_ok in Run as (value & middle & Head & Run).
    apply EaControlSound.mbind_ok in Run as (tail & finish & Tail & Run).
    injection Run as <- _. constructor; [eapply Sound; exact Head|eapply IH; exact Tail].
Qed.

Local Lemma forall2_position : forall A B (R : A -> B -> Prop) left right,
  Forall2 R left right -> forall i x,
  nth_error left i = Some x -> exists y, nth_error right i = Some y /\ R x y.
Proof.
  intros A B R left right Rows. induction Rows; intros [|i] value Read; try discriminate.
  - injection Read as <-. exists y. split; [reflexivity|assumption].
  - now apply IHRows.
Qed.

Local Lemma indexed_read : forall A start (keys : list A) position key,
  In (position, key) (combine (seq start (List.length keys)) keys) <->
  exists offset, position = start + offset /\ nth_error keys offset = Some key.
Proof.
  intros A start keys. revert start. induction keys as [|first rest IH]; intros start position key.
  - split; [contradiction|intros [offset [_ Read]]; now destruct offset].
  - change (((start,first) = (position,key) \/ In (position,key)
      (combine (seq (S start) (List.length rest)) rest)) <->
      exists offset, position = start + offset /\ nth_error (first :: rest) offset = Some key).
    rewrite IH. split.
    + intros [Equal|[offset [At Read]]].
      * injection Equal as <- <-. exists 0. split; [lia|reflexivity].
      * exists (S offset). split; [lia|exact Read].
    + intros [[|offset] [At Read]].
      * cbn in Read. injection Read as <-. left. f_equal. lia.
      * right. exists offset. split; [lia|exact Read].
Qed.

Local Lemma indexed_order : forall A start (keys : list A),
  StronglySorted (fun x y : nat * A => fst x < fst y) (combine (seq start (List.length keys)) keys).
Proof.
  intros A start keys. revert start. induction keys as [|first rest IH]; intro start.
  - constructor.
  - cbn [List.length seq combine]. constructor; [apply IH|]. apply Forall_forall.
    intros [position key] Member. apply indexed_read in Member as [offset [At _]]. cbn. lia.
Qed.

Local Lemma filtered_order : forall A (R : A -> A -> Prop) test rows,
  StronglySorted R rows -> StronglySorted R (filter test rows).
Proof.
  intros A R test rows Sorted. induction Sorted as [|first rest Sorted IH Below]; [constructor|].
  cbn [filter]. destruct (test first); [|exact IH]. constructor; [exact IH|].
  apply Forall_forall. intros x Member. apply filter_In in Member as [Member _].
  exact (proj1 (Forall_forall _ _) Below x Member).
Qed.

Local Lemma last_sorted_index : forall A (rows : list (nat * A)) last rest,
  StronglySorted (fun x y => fst x < fst y) rows -> rev rows = last :: rest ->
  forall row, In row rows -> fst row <= fst last.
Proof.
  intros A rows last rest Sorted Last row Member.
  assert (End : rows = rev rest ++ [last]) by (rewrite <- (rev_involutive rows), Last; reflexivity).
  rewrite End in Sorted, Member. clear End Last rows. revert row Sorted Member.
  induction (rev rest) as [|first prefix IH]; intros row Sorted Member.
  - destruct Member as [<-|[]]. lia.
  - inversion Sorted as [|head tail Tail Below]; subst head tail.
    destruct Member as [<-|Later].
    + apply Nat.lt_le_incl. apply (proj1 (Forall_forall _ _) Below), in_or_app. right. now left.
    + now apply IH.
Qed.

Theorem copy_result_position_sound : forall E source target Cu result source_result rho keys key before position after,
  copy_result_position E source target Cu result source_result rho
    (combine (seq 0 (List.length keys)) keys) key before = Ok (position, after) ->
  exists selected provided actual,
    nth_error keys position = Some selected /\
    app_subst_item rho (canon_item Cu (provision_id selected)) = app_subst_item rho (provision_id key) /\
    (forall other found, nth_error keys other = Some found ->
       app_subst_item rho (canon_item Cu (provision_id found)) = app_subst_item rho (provision_id key) ->
       provision_id found = provision_id selected /\ other <= position) /\
    ilook (provision_id key) (c_provisions result) = Some provided /\
    ilook (provision_id selected) (c_provisions source_result) = Some actual /\
    provision_subst rho provided = provision_subst rho (canon_provision Cu actual).
Proof.
  intros E source target Cu result source_result rho keys key before position after Run.
  unfold copy_result_position in Run. cbv zeta in Run.
  set (matches := fun q : nat * item => item_eqb
    (app_subst_item rho (canon_item Cu (provision_id (snd q)))) (app_subst_item rho (provision_id key))) in *.
  set (indexed := combine (seq 0 (List.length keys)) keys) in *.
  set (candidates := filter matches indexed) in *.
  destruct (rev candidates) as [|[last selected] rest] eqn:Last; [discriminate|].
  apply EaControlSound.mbind_ok in Run as (accepted & middle & Distinct & Run).
  apply EaLinkBodySound.successful_link_check in Distinct as [Distinct ->].
  destruct (ilook (provision_id key) (c_provisions result)) as [provided|] eqn:Provided; [|discriminate].
  destruct (ilook (provision_id selected) (c_provisions source_result)) as [actual|] eqn:Actual; [|discriminate].
  apply EaControlSound.mbind_ok in Run as (accepted_type & finish & Types & Run).
  apply EaLinkBodySound.successful_link_check in Types as [Types ->].
  injection Run as Position _ . subst position.
  assert (Member : In (last, selected) candidates).
  { apply in_rev. rewrite Last. now left. }
  assert (Selected : In (last, selected) indexed /\ matches (last, selected) = true).
  { now apply filter_In in Member. }
  destruct Selected as [At Equal]. apply PublicCallRealize.item_eqb_spec in Equal.
  apply indexed_read in At as [offset [Position Read]]. cbn in Position. subst offset.
  exists selected, provided, actual. split; [exact Read|]. split; [exact Equal|]. split.
  - intros other found Read' Same.
    assert (InCandidates : In (other, found) candidates).
    { apply filter_In. split.
      - apply indexed_read. exists other. split; [reflexivity|exact Read'].
      - apply PublicCallRealize.item_eqb_spec. exact Same. }
    split.
    + pose proof (proj1 (forallb_forall _ _) Distinct (other,found) InCandidates) as Chosen.
      now apply PublicCallRealize.item_eqb_spec in Chosen.
    + exact (last_sorted_index _ candidates (last,selected) rest
        (filtered_order _ _ _ _ (indexed_order _ 0 keys)) Last (other,found) InCandidates).
  - split; [first [exact Provided|reflexivity]|]. split; [exact Actual|]. now apply EaCopyNeedSound.provision_eqb_spec.
Qed.

Local Lemma copy_need_rows : forall E source target Cu Ch rho nsu nsh before rows after,
  mmap (fun k =>
    let* p := lift (ilook (provision_id k) (c_provisions Ch))
      (source ++ " cannot be copied to " ++ target ++ ": its input interpretation differs (D78)")%string in
    let* chosen := pure (choose_copy_need E source target Cu Ch rho nsu k p) in ret (k, chosen))
    nsh before = Ok (rows, after) ->
  NoDup nsh ->
  exists choose : item -> item, rows = map (fun k => (k, choose k)) nsh /\
    forall k, In k nsh -> exists p, ilook (provision_id k) (c_provisions Ch) = Some p /\
      CopyNeedChoice Cu Ch rho nsu k p (choose k).
Proof.
  intros E source target Cu Ch rho nsu nsh before rows after Run Distinct.
  assert (Rows : Forall2 (fun k q => fst q = k /\ exists p,
    ilook (provision_id k) (c_provisions Ch) = Some p /\ CopyNeedChoice Cu Ch rho nsu k p (snd q)) nsh rows).
  { eapply mmap_checks; [|exact Run]. intros k q start finish Read.
    apply EaControlSound.mbind_ok in Read as (p & middle & Provision & Read).
    apply EaLinkBodySound.successful_link_lift in Provision as [Found ->].
    apply EaControlSound.mbind_ok in Read as (chosen & last & Checked & Read).
    apply pure_read in Checked as [Selected ->]. injection Read as <- _.
    split; [reflexivity|]. exists p. split; [exact Found|now apply EaCopyNeedSound.choose_copy_need_sound in Selected]. }
  clear Run before after. induction Rows as [|key [read chosen] needs tail Head Rows IH].
  - exists (fun k => k). split; [reflexivity|intros k []].
  - inversion Distinct as [|head rest NotPresent Unique]; subst head rest.
    destruct Head as [Same [p [Found Selected]]]. cbn [fst snd] in Same, Selected. subst read.
    destruct (IH Unique) as [choose [Tail Choices]].
    exists (fun k => if item_eqb k key then chosen else choose k). split.
    + cbn [map]. rewrite (proj2 (PublicCallRealize.item_eqb_spec _ _) eq_refl). f_equal. rewrite Tail.
      apply map_ext_in. intros k Member. destruct (item_eqb k key) eqn:Equal; [|reflexivity].
      apply PublicCallRealize.item_eqb_spec in Equal. subst k. contradiction.
    + intros k [<-|Member].
      * rewrite (proj2 (PublicCallRealize.item_eqb_spec _ _) eq_refl). eauto.
      * destruct (item_eqb k key) eqn:Equal; [|now apply Choices].
        apply PublicCallRealize.item_eqb_spec in Equal. subst k. contradiction.
Qed.

Local Lemma uniq_distinct : forall xs, NoDup (uniq xs).
Proof.
  assert (Fold : forall xs acc, NoDup acc ->
    NoDup (fold_left (fun acc x => if nmem x acc then acc else x :: acc) xs acc)).
  { induction xs as [|first rest IH]; intros acc Distinct; [exact Distinct|].
    cbn [fold_left]. apply IH. destruct (nmem first acc) eqn:Member; [exact Distinct|].
    constructor; [|exact Distinct]. intro Present. apply nmem_spec in Present. congruence. }
  intro xs. unfold uniq. apply NoDup_rev, Fold. constructor.
Qed.

Local Lemma copy_classes_exact : forall Cu fixed y,
  In y (uniq (filter (fun g => negb (is_some (lookup_subst g fixed)))
    (flat_map (fun q => match nlook (fst q) (c_ty Cu) with Some t => gens t | None => [] end) (c_ty Cu)))) <->
  (exists x tx, nlook x (c_ty Cu) = Some tx /\ In y (gens tx)) /\ lookup_subst y fixed = None.
Proof.
  intros Cu fixed y. rewrite EaMainSymbols.uniq_members, filter_In, in_flat_map.
  destruct (lookup_subst y fixed) as [found|] eqn:Read; cbn [is_some negb]; [intuition discriminate|].
  split.
  - intros [[[x t] [Member Leaf]] _]. cbn [fst] in Leaf.
    destruct (nlook x (c_ty Cu)) as [tx|] eqn:Lookup; [|contradiction].
    split; [now exists x, tx|reflexivity].
  - intros [[x [tx [Lookup Leaf]]] _]. split; [|reflexivity].
    exists (x,tx). split; [now apply PublicTypeRead.nlook_in|].
    cbn [fst]. now rewrite Lookup.
Qed.

Section Copy.
Variable E : env.
Let W := e_W E.
Variable bound : nat.
Hypotheses
  (Bounded : forall symbol d, w_decl W symbol = Some d -> symbol < bound)
  (Budget : metadata_budget W bound < e_metadata_fuel E).

Theorem copy_interpretation_sound : forall sigma u h Cu nsu nsh Ch before sm rm after,
  FunCtx W h Ch nsh -> bound <= es_next before ->
  (forall g, In g (scope_support (copy_allocation_scope W sigma)) -> g < es_next before) ->
  copy_interpretation E sigma u h Cu nsu nsh Ch before = Ok ((sm, rm), after) ->
  exists chosen, sm = map (fun k => (rkey_of Ch k, rkey_of Cu (chosen k))) nsh /\
    CopyInterpretation W sigma u h Cu nsu nsh Ch rm chosen.
Proof.
  intros sigma u h Cu nsu nsh Ch before sm rm after Source Floor Scope Run.
  unfold copy_interpretation in Run. cbv zeta in Run. fold W in Run.
  set (outs := fun_outputs W (e_metadata_fuel E) u) in *.
  set (fixed := copy_fixed_readings Cu sigma) in *.
  set (ys := uniq (filter (fun g => negb (is_some (lookup_subst g fixed)))
    (flat_map (fun q => match nlook (fst q) (c_ty Cu) with Some t => gens t | None => [] end) (c_ty Cu)))) in *.
  assert (OutputsDistinct : NoDup outs).
  { unfold outs, fun_outputs. destruct (functor_decl W u) as [[d [[[ps inputs] outputs] body]]|]; [|constructor].
    apply EaRootSound.ascending_nodup, EaDependencyFuel.sort_uniq_ascending. }
  assert (ClassesDistinct : NoDup ys) by (unfold ys; apply uniq_distinct).
  destruct (EaAllocationSound.fresh_tables_allocate E u outs ys (copy_allocation_scope W sigma) before
    OutputsDistinct ClassesDistinct) as (gt & s1 & ct & s2 & First & Second & Allocation).
  - intros g Above. unfold copy_allocation_scope, scope_permits, Fresh.
    destruct (w_decl W g) as [d|] eqn:Declaration; [|reflexivity].
    pose proof (Bounded g d Declaration). lia.
  - exact Scope.
  - apply EaControlSound.mbind_ok in Run as (first & after_first & Outputs & Run).
    rewrite First in Outputs. injection Outputs as <- <-.
    apply EaControlSound.mbind_ok in Run as (second & after_second & Classes & Run).
    rewrite Second in Classes. injection Classes as <- <-.
    set (gen := gen_of gt) in *. set (cg := gen_of ct) in *.
    set (rho := copy_subst fixed ys cg) in *.
    apply EaControlSound.mbind_ok in Run as (inputs_ok & after_inputs & Inputs & Run).
    apply EaLinkBodySound.successful_link_check in Inputs as [Inputs ->].
    apply EaControlSound.mbind_ok in Run as (needs & after_needs & Needs & Run).
    destruct (PublicCallRealize.functx_scoped W h Ch nsh Source) as (_ & _ & _ & Distinct & _).
    destruct (copy_need_rows _ _ _ _ _ _ _ _ _ _ _ Needs Distinct) as [chosen [NeedsMap Chosen]].
    apply EaControlSound.mbind_ok in Run as ([Cr ks] & after_result & Result & Run).
    apply pure_read in Result as [Result ->].
    apply EaControlSound.mbind_ok in Run as ([Cr' ks'] & after_source_result & SourceResult & Run).
    apply pure_read in SourceResult as [SourceResult ->].
    apply EaControlSound.mbind_ok in Run as (positions & finish & Positions & Run).
    injection Run as <- <- _.
    assert (PositionReads : Forall2 (fun key position => exists selected provided actual,
      nth_error ks' position = Some selected /\
      app_subst_item rho (canon_item Cu (provision_id selected)) = app_subst_item rho (provision_id key) /\
      (forall other found, nth_error ks' other = Some found ->
        app_subst_item rho (canon_item Cu (provision_id found)) = app_subst_item rho (provision_id key) ->
        provision_id found = provision_id selected /\ other <= position) /\
      ilook (provision_id key) (c_provisions Cr) = Some provided /\
      ilook (provision_id selected) (c_provisions Cr') = Some actual /\
      provision_subst rho provided = provision_subst rho (canon_provision Cu actual)) ks positions).
    { eapply mmap_checks; [|exact Positions]. intros. eapply copy_result_position_sound. eassumption. }
    assert (Enough : bound < e_metadata_fuel E).
    { unfold metadata_budget, metadata_depth in Budget.
      pose proof (Nat.le_max_l bound (largest (fun symbol => match w_decl W symbol with
        Some declaration => declaration_depth (d_ast declaration) | None => 0 end) (seq 0 bound))). lia. }
    exists chosen. split.
    + rewrite NeedsMap, map_map. apply map_ext. intro k. cbn [fst snd]. now rewrite !EaSatisfiesSound.rkof_exact.
    + exists outs, gen, ys, cg, Cr, ks, Cr', ks',
        (extend_allocation_scope (copy_allocation_scope W sigma) (map gen outs ++ map cg ys)).
      split; [intro t; apply (EaContextFuel.fun_outputs_exact W bound Bounded _ u Enough t)|].
      split; [exact Allocation|]. split; [intro y; apply copy_classes_exact|]. split.
      * intros x tx Read. pose proof (PublicTypeRead.nlook_in _ _ _ _ Read) as Member.
        pose proof (proj1 (forallb_forall _ _) Inputs (x,tx) Member) as Equal.
        cbn [fst] in Equal. rewrite Read in Equal. now apply PublicTypeFacts.ty_eqb_spec.
      * split; [exact Chosen|]. split.
        { eapply EaOutputSound.provide_result_sound_at_budget; eassumption. }
        split.
        { eapply EaOutputSound.provide_result_sound_at_budget; eassumption. }
        split.
        { exact (eq_sym (Forall2_length PositionReads)). }
        intros i k Read. destruct (forall2_position _ _ _ _ _ PositionReads i k Read)
          as (position & At & selected & provided & actual & Data).
        exists position, selected, provided, actual. split; [exact At|exact Data].
Qed.

Theorem fun_copy_sound : forall P F C pa u pg h before wrap C' after,
  EaContextTableSound.ContextTableSound W -> EaMain.decls_form E bound = Ok tt ->
  resolve_sym W (fr_mod F) pa = Some u -> resolve_sym W (fr_mod F) pg = Some h ->
  kind_sym W h = Some DKFunctor -> PublicContextFacts.CxSection C ->
  bound <= es_next before -> (forall g, In g (context_gens C) -> g < es_next before) ->
  fun_copy E F C u h before = Ok ((wrap, C'), after) ->
  BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) wrap C' F [].
Proof.
  intros P F C pa u pg h before wrap C' after Contexts Decls Target Provider ProviderKind Section Floor Scope Run.
  unfold fun_copy in Run. fold W in Run.
  apply EaControlSound.mbind_ok in Run as (checked & s1 & Abstract & Run).
  apply EaLinkBodySound.successful_link_check in Abstract as [Abstract ->].
  unfold abstract_functor, ast_of in Abstract. fold W in Abstract.
  destruct (w_decl W u) as [d|] eqn:Declaration; [|discriminate].
  cbn [option_map] in Abstract.
  destruct (d_ast d) as [| | | | | | | |name parameters arguments results body]
    eqn:Shape; try discriminate Abstract. destruct body; [discriminate Abstract|].
  assert (TargetKind : kind_sym W u = Some DKFunctor) by
    (unfold kind_sym; rewrite Declaration, Shape; reflexivity).
  assert (TargetAbstract : exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None)).
  { exists name, parameters, arguments, results. unfold ast_of. rewrite Declaration. cbn [option_map]. now rewrite Shape. }
  apply EaControlSound.mbind_ok in Run as (available & s2 & Provision & Run).
  apply EaLinkBodySound.successful_link_lift in Provision as [Provision ->].
  rewrite EaSatisfiesSound.cfun_exact in Provision.
  apply EaControlSound.mbind_ok in Run as ([Cu nsu] & s3 & Read & Run).
  apply EaLinkBodySound.successful_link_lift in Read as [ReadTarget ->].
  apply EaControlSound.mbind_ok in Run as ([Ch nsh] & s4 & Read & Run).
  apply EaLinkBodySound.successful_link_lift in Read as [ReadProvider ->].
  assert (FormalTarget : FunCtx W u Cu nsu).
  { pose proof (Contexts _ _ _ ReadTarget) as Valid. now rewrite TargetKind in Valid. }
  assert (FormalProvider : FunCtx W h Ch nsh).
  { pose proof (Contexts _ _ _ ReadProvider) as Valid. now rewrite ProviderKind in Valid. }
  apply EaControlSound.mbind_ok in Run as (checked_fixed & s5 & FixedHere & Run).
  destruct (find (fun x => negb (bound_in C x)) (fixed W (e_metadata_fuel E) u))
    eqn:BoundHere; [discriminate FixedHere|]. injection FixedHere as _ <-.
  apply EaControlSound.mbind_ok in Run as (checked_included & s6 & FixedIncluded & Run).
  destruct (find (fun x => negb (nmem x (fixed W (e_metadata_fuel E) u))) (fixed W (e_metadata_fuel E) h))
    eqn:Included; [discriminate FixedIncluded|]. injection FixedIncluded as _ <-.
  apply EaControlSound.mbind_ok in Run as (checked_agreement & s7 & Agreement & Run).
  apply EaLinkBodySound.successful_link_check in Agreement as [Agreement ->].
  apply EaControlSound.mbind_ok in Run as (checked_results & s8 & Results & Run).
  apply EaLinkBodySound.successful_link_check in Results as [Results ->].
  pose proof (proj1 (EaDeclarationSound.same_result_exact_at_declarations E bound u h Bounded Decls TargetKind ProviderKind)
    Results) as SameResults.
  set (fx := fixed_here E u C) in *.
  apply EaControlSound.mbind_ok in Run as (checked_inputs & s9 & Inputs & Run).
  apply EaLinkBodySound.successful_link_check in Inputs as [Inputs ->].
  apply EaControlSound.mbind_ok in Run as ([sm rm] & finish & Copied & Run).
  injection Run as <- <- _.
  assert (FixedRows : forall x t, In (x,t) fx <-> Fixed W u x /\ nlook x (c_ty C) = Some t).
  { intros x t. unfold fx. eapply EaClosureSound.fixed_here_exact; eassumption. }
  assert (Reserved : forall g, In g (scope_support (copy_allocation_scope W
    (map (fun q => (fst q, canon C (snd q))) fx))) -> g < es_next before).
  { intros g Member. apply Scope. cbn [scope_support copy_allocation_scope] in Member.
    apply in_flat_map in Member as [[x t] [Row Occurs]].
    apply in_map_iff in Row as [[y original] [Equal Row]]. cbn [fst snd] in Equal. injection Equal as <- <-.
    apply FixedRows in Row as [_ Found]. unfold context_gens. apply in_or_app. right. apply in_or_app. left.
    apply in_flat_map. exists (y,original). split; [now apply PublicTypeRead.nlook_in|].
    apply in_or_app. now right. }
  destruct (copy_interpretation_sound _ _ _ _ _ _ _ _ _ _ _ FormalProvider Floor Reserved Copied)
    as [chosen [Selected Interpretation]].
  eapply (@BFunCopy W P F C pa u pg h Cu nsu Ch nsh chosen sm fx rm); try eassumption.
  - congruence.
  - intros x Fixed. apply (EaResultBudget.fixed_exact W bound Bounded _ u Budget x) in Fixed.
    pose proof (find_none _ _ BoundHere x Fixed) as Bound.
    apply negb_false_iff in Bound. unfold bound_in in Bound. now destruct (nlook x (c_ty C)).
  - intros x Fixed. apply (EaResultBudget.fixed_exact W bound Bounded _ h Budget x) in Fixed.
    pose proof (find_none _ _ Included x Fixed) as InTarget. apply negb_false_iff, nmem_spec in InTarget.
    now apply (EaResultBudget.fixed_exact W bound Bounded _ u Budget x).
  - now apply (EaFamilySound.fixed_agreement_spec E bound Bounded Budget C h Section).
  - intros x t Found. pose proof (PublicTypeRead.nlook_in _ _ _ _ Found) as Member.
    pose proof (proj1 (forallb_forall _ _) Inputs (x,t) Member) as Bound. cbn [fst] in Bound.
    unfold bound_in in Bound. now destruct (nlook x (c_ty Cu)).
Qed.

End Copy.
