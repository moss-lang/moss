(* Linking allocates private output probes after elaborating the body.
   The probes are consumed by result comparison before code emission. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit EaBase EaCall EaCodeEffects.
From NM Require AllocationFacts EaAllocationSound EaBudget EaContextFuel
  EaExpressionCodeEffects EaFamily EaFunctor EaMain EaOutputSound EaRootSound EaTypes.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Theorem link_output_roles_sound : forall E bound u Cu Cb who before outs table Cr keys after,
  (forall n d, w_decl (e_W E) n = Some d -> n < bound) ->
  EaBudget.metadata_budget (e_W E) bound < e_metadata_fuel E ->
  bound <= es_next before ->
  EaFunctor.link_output_roles E u Cu Cb who before = Ok (((outs, table), (Cr, keys)), after) ->
  TypesLinkOutputs (e_W E) u Cu Cb outs (EaFunctor.gen_of table) Cr keys /\
  map (EaFunctor.gen_of table) outs = seq (es_next before) (List.length outs) /\
  es_next after = es_next before + List.length outs.
Proof.
  intros E bound u Cu Cb who before outs table Cr keys after Declared Budget Floor Run.
  unfold EaFunctor.link_output_roles in Run. cbn zeta in Run.
  set (roles := EaTypes.fun_outputs (e_W E) (e_metadata_fuel E) u) in *.
  unfold mbind in Run.
  destruct (find (fun t => negb (bound_in Cb t)) roles) as [missing|] eqn:Present;
    [discriminate|].
  cbn [ret] in Run.
  destruct (EaAllocationSound.fresh_table_range E u roles before) as (middle & Allocated & Next).
  rewrite Allocated in Run.
  set (chosen := combine roles (seq (es_next before) (List.length roles))) in *.
  unfold pure in Run.
  destruct (EaFamily.provide_result E u Cu (fun t => TAbs (EaFunctor.gen_of chosen t)))
    as [[result result_keys]|message] eqn:Provided; [|discriminate].
  unfold ret in Run. injection Run as <- <- <- <- <-.
  assert (Batch : map (EaFunctor.gen_of chosen) roles = seq (es_next before) (List.length roles)).
  { unfold chosen. apply EaAllocationSound.read_fresh_table.
    apply EaRootSound.fun_outputs_nodup. }
  split; [|split; assumption].
  unfold TypesLinkOutputs. split.
  - unfold roles. apply (EaContextFuel.fun_outputs_exact _ bound Declared).
    assert (Depth : bound <= EaBudget.metadata_depth (e_W E) bound).
    { unfold EaBudget.metadata_depth. apply Nat.le_max_l. }
    unfold EaBudget.metadata_budget in Budget. lia.
  - split.
    + intros t Output. pose proof (find_none _ _ Present t Output) as Found.
      unfold bound_in in Found. destruct (nlook t (c_ty Cb)) as [meaning|];
        [exists meaning; reflexivity|discriminate].
    + split.
      * assert (SourceFresh : forall g, es_next before <= g -> Fresh (e_W E) g).
        { intros g Beyond. unfold Fresh.
          destruct (w_decl (e_W E) g) as [d|] eqn:Decl; [|reflexivity].
          pose proof (Declared g d Decl). lia. }
        rewrite Batch. refine (proj1 (AllocationFacts.allocate_range (link_output_scope (e_W E))
          (es_next before) (List.length roles) SourceFresh _)). intros g [].
      * eapply EaOutputSound.provide_result_sound_at_budget; eassumption.
Qed.

Corollary link_output_roles_after_body : forall E bound fuel F C items wrapper Cb Fb made
  before middle after u Cu who outs table Cr keys,
  (forall n d, w_decl (e_W E) n = Some d -> n < bound) ->
  EaBudget.metadata_budget (e_W E) bound < e_metadata_fuel E ->
  bound <= es_next before ->
  r_bind_items (EaMain.mkrec (EaMain.run E fuel)) F C items before =
    Ok (((wrapper, Cb), Fb), made, middle) ->
  EaFunctor.link_output_roles E u Cu Cb who middle = Ok (((outs, table), (Cr, keys)), after) ->
  TypesLinkOutputs (e_W E) u Cu Cb outs (EaFunctor.gen_of table) Cr keys /\
  forall t, In t outs -> es_next before <= EaFunctor.gen_of table t < es_next after.
Proof.
  intros E bound fuel F C items wrapper Cb Fb made before middle after u Cu who outs table Cr keys
    Declared Budget Floor Body Run.
  destruct (EaExpressionCodeEffects.mkrec_code_effect _ (EaExpressionCodeEffects.run_code_effect E fuel))
    as (_ & _ & BodyEffect).
  pose proof (counter_grows _ _ (BodyEffect _ _ _ _ _ _ Body)) as Grows.
  destruct (link_output_roles_sound E bound _ _ _ _ _ _ _ _ _ _ Declared Budget
    (Nat.le_trans _ _ _ Floor Grows) Run) as (Typed & Batch & Next).
  split; [exact Typed|].
  intros t Member. pose proof (in_map (EaFunctor.gen_of table) outs t Member) as InBatch.
  rewrite Batch in InBatch. apply in_seq in InBatch. lia.
Qed.
