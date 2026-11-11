(* The executable linker checks the promised output types and provisions,
   validates the body's pins, and records exactly the code it emits. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit EaBase EaCall EaTypes EaCodeAllocationSound EaCodeEffects.
From NM Require EaAssemblyCodeEffects EaBindCodeEffects EaBudget EaContextTableSound EaControlSound
  EaDependencyFuel EaExpressionCodeEffects EaFamily EaFamilyComplete EaFamilySound EaFunctor
  EaLinkAllocationSound EaMain EaMergeSound EaNumbering EaResultBudget EaSatisfiesSound EaTypesSound
  ProgramPrune PublicCallRealize PublicTypeFacts PublicTypeRead.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Import EaFunctor.
Local Open Scope list_scope.

Local Lemma provision_refinesb_sound : forall promised actual,
  EaFunctor.provision_refinesb promised actual = true -> provision_refines promised actual.
Proof.
  intros promised actual Checked. unfold EaFunctor.provision_refinesb in Checked.
  apply andb_true_iff in Checked as [Agrees Instantiation].
  apply andb_true_iff in Agrees as [Kind Signature].
  split.
  - destruct promised, actual; try discriminate Kind; exact I.
  - split; [now apply EaMergeSound.ofsig_eqb_spec|].
    intros t value Member.
    pose proof (proj1 (forallb_forall _ _) Instantiation (t, Some value) Member) as Present.
    cbn [fst snd] in Present.
    apply existsb_exists in Present as [[name reading] [Member' Equal]].
    apply andb_true_iff in Equal as [Key Read].
    apply Nat.eqb_eq in Key. apply EaControlSound.oty_eqb_exact in Read.
    cbn [fst snd] in *. now subst name reading.
Qed.

Local Lemma binds_of_sound : forall e bindings,
  EaFunctor.binds_of e = Some bindings -> BindsOf e bindings.
Proof.
  fix IH 1. intros e bindings Read. destruct e; cbn [EaFunctor.binds_of] in Read;
    try discriminate Read.
  - injection Read as <-. constructor.
  - match type of Read with context [EaFunctor.binds_of ?rest] =>
      destruct (EaFunctor.binds_of rest) as [tail|] eqn:Tail; [|discriminate Read]
    end.
    injection Read as <-. constructor. eapply IH; exact Tail.
Qed.

Local Lemma linked_pins_spec : forall Cg Cb fixed x value,
  In (x, value) (linked_pins Cg Cb fixed) <->
  In x fixed /\ nlook x (c_ty Cb) = Some value /\
  (In x (map fst (c_ty Cg)) -> nlook x (c_ty Cg) <> Some value).
Proof.
  intros Cg Cb fixed x value. unfold linked_pins. rewrite in_flat_map.
  split.
  - intros [y [Fixed Generated]].
    destruct (nlook y (c_ty Cb)) as [v|] eqn:Body; [|contradiction].
    destruct (nlook y (c_ty Cg)) as [w|] eqn:Input.
    + destruct (ty_eqb w v) eqn:Equal; [contradiction|].
      destruct Generated as [Row|[]]. injection Row as <- <-.
      split; [exact Fixed|]. split; [exact Body|].
      intros _ Same. rewrite Input in Same. injection Same as ->. rewrite PublicTypeFacts.ty_eqb_refl in Equal. discriminate.
    + destruct Generated as [Row|[]]. injection Row as <- <-.
      split; [exact Fixed|]. split; [exact Body|].
      intros _ Same. rewrite Input in Same. discriminate.
  - intros [Fixed [Body Distinct]]. exists x. split; [exact Fixed|].
    rewrite Body. destruct (nlook x (c_ty Cg)) as [w|] eqn:Input; [|now left].
    assert (Member : In x (map fst (c_ty Cg))).
    { apply PublicTypeRead.nlook_in in Input. exact (in_map fst _ _ Input). }
    specialize (Distinct Member).
    destruct (ty_eqb w value) eqn:Equal; [|now left].
    apply PublicTypeFacts.ty_eqb_spec in Equal. subst. exfalso. apply Distinct. reflexivity.
Qed.

Lemma bound_in_key : forall C t, bound_in C t = true <-> In t (map fst (c_ty C)).
Proof.
  intros C t. unfold bound_in. destruct (nlook t (c_ty C)) as [value|] eqn:Found.
  - split; [intros _|reflexivity]. apply PublicTypeRead.nlook_in in Found.
    exact (in_map fst _ _ Found).
  - split; [discriminate|]. intro Member.
    destruct (PublicCallRealize.nlook_some _ _ _ Member) as [value Read]. congruence.
Qed.

Local Lemma valid_link_pin_sound : forall Cu Cg made fixed y,
  valid_link_pin Cu Cg made fixed y = true ->
  In y (map fst (c_ty Cg)) /\ lookup_subst y (c_sub Cg) = None /\ ~ In y made /\
  (In y (map fst (c_ty Cu)) -> In y fixed).
Proof.
  intros Cu Cg made fixed y Checked. unfold valid_link_pin in Checked.
  apply andb_true_iff in Checked as [Safe Fixed].
  apply andb_true_iff in Safe as [Safe NotMade].
  apply andb_true_iff in Safe as [Input Unmerged].
  split; [now apply bound_in_key|]. split.
  - apply negb_true_iff in Unmerged. destruct (lookup_subst y (c_sub Cg)); [discriminate|reflexivity].
  - split; [apply negb_true_iff in NotMade; now apply EaDependencyFuel.nmem_false_spec|].
    intro Key. apply bound_in_key in Key. rewrite Key in Fixed. now apply EaTypesSound.nmem_spec.
Qed.

Local Lemma link_result_realizes : forall body promised reads keys,
  find (fun k => negb
      match ilook (provision_id k) (c_provisions promised),
            ilook (provision_id (at_site body k)) (c_provisions body) with
      | Some required, Some actual =>
          EaFunctor.provision_refinesb
            (provision_subst (c_sub body) (provision_subst reads required))
            (provision_subst (c_sub body) actual)
      | _, _ => false
      end) keys = None ->
  ResultRealizes body promised reads keys.
Proof.
  intros body promised reads keys Checked k Member.
  pose proof (find_none _ _ Checked k Member) as Present. apply negb_false_iff in Present.
  destruct (ilook (provision_id k) (c_provisions promised)) as [required|]; [|discriminate].
  destruct (ilook (provision_id (at_site body k)) (c_provisions body)) as [actual|]; [|discriminate].
  exists required, actual. split; [reflexivity|]. split; [reflexivity|]. now apply provision_refinesb_sound.
Qed.

Local Lemma link_pin_provisions : forall site plain Cg keys,
  find (fun k =>
    negb (item_eqb (at_site site k) (at_site plain k)) &&
    is_some (ilook (provision_id (at_site site k)) (c_provisions Cg))) keys = None ->
  forall k, In k keys -> at_site site k <> at_site plain k ->
    ilook (provision_id (at_site site k)) (c_provisions Cg) = None.
Proof.
  intros site plain Cg keys Checked k Member Changed.
  pose proof (find_none _ _ Checked k Member) as CheckedKey.
  assert (Different : item_eqb (at_site site k) (at_site plain k) = false).
  { destruct (item_eqb (at_site site k) (at_site plain k)) eqn:Equal; [|reflexivity].
    apply ProgramPrune.item_eqb_true in Equal. contradiction. }
  cbn beta in CheckedKey. rewrite Different in CheckedKey.
  destruct (ilook (provision_id (at_site site k)) (c_provisions Cg)); [discriminate|reflexivity].
Qed.

Local Lemma family_bindings_checked : forall E bindings,
  (forall v, EaFamily.is_family E v = true -> Family (e_W E) v) ->
  find (fun b => match b with
    | BValue _ _ => true
    | BApply v _ _ _ _ => negb (EaFamily.is_family E v)
    | _ => false end) bindings = None ->
  FamilyBindings (e_W E) bindings.
Proof.
  intros E bindings FamilySound Checked. apply Forall_forall.
  intros binding Member. pose proof (find_none _ _ Checked binding Member) as Safe.
  destruct binding; try exact I; try discriminate Safe.
  apply negb_false_iff in Safe. now apply FamilySound.
Qed.

Local Lemma pins_have_link_scope : forall Cu Cg made fixed (pins : subst),
  find (fun q => negb (forallb (valid_link_pin Cu Cg made fixed) (gens (snd q)))) pins = None ->
  forall x value y, In (x, value) pins -> In y (gens value) ->
    In y (map fst (c_ty Cg)) /\ lookup_subst y (c_sub Cg) = None /\ ~ In y made /\
    (In y (map fst (c_ty Cu)) -> In y fixed).
Proof.
  intros Cu Cg made fixed pins Checked x value y Pin Generation.
  pose proof (find_none _ _ Checked (x, value) Pin) as Good.
  cbn [snd] in Good. apply negb_false_iff in Good.
  apply valid_link_pin_sound. exact (proj1 (forallb_forall _ _) Good y Generation).
Qed.

Record TypesLinkedResult (W : world) (u : nat) (Cu Cg Cb Cr : cx)
    (made outs : list nat) (gen : nat -> nat) (keys : list item) (plan : linked_result_plan) : Prop := {
  linked_plan_result_keys : link_plan_keys plan = keys;
  linked_plan_result_site : link_plan_site plan = link_site Cg Cb (link_plan_pins plan) outs gen;
  linked_plan_pin_provisions : forall k, In k keys ->
    at_site (link_plan_site plan) k <> at_site (link_site Cg Cb [] outs gen) k ->
    ilook (provision_id (at_site (link_plan_site plan) k)) (c_provisions Cg) = None;
  linked_plan_realizes : ResultRealizes (link_plan_site plan) Cr
    (role_reads (link_plan_site plan) outs gen ++ pin_reads Cb (link_plan_pins plan)) keys;
  linked_plan_pins_exact : forall x tx, In (x, tx) (link_plan_pins plan) <->
    Fixed W u x /\ nlook x (c_ty Cb) = Some tx /\
    (In x (map fst (c_ty Cg)) -> nlook x (c_ty Cg) <> Some tx);
  linked_plan_pin_scope : forall x tx y, In (x, tx) (link_plan_pins plan) -> In y (gens tx) ->
    In y (map fst (c_ty Cg)) /\ lookup_subst y (c_sub Cg) = None /\ ~ In y made /\
    (In y (map fst (c_ty Cu)) -> Fixed W u y)
}.

Theorem check_linked_result_sound : forall E u gg Cu Cg Cb Cr made outs gen keys plan,
  (forall x, In x (fixed (e_W E) (e_metadata_fuel E) u) <-> Fixed (e_W E) u x) ->
  check_linked_result E u gg Cu Cg Cb Cr made outs gen keys = Ok plan ->
  TypesLinkedResult (e_W E) u Cu Cg Cb Cr made outs gen keys plan.
Proof.
  intros E u gg Cu Cg Cb Cr made outs gen keys plan FixedExact Check.
  unfold check_linked_result in Check. cbv zeta in Check.
  set (fixed_inputs := fixed (e_W E) (e_metadata_fuel E) u) in *.
  set (pins := linked_pins Cg Cb fixed_inputs) in *.
  set (site := link_site Cg Cb pins outs gen) in *.
  set (plain := link_site Cg Cb [] outs gen) in *.
  apply EaTypesSound.rbind_ok in Check as ([] & PinsCheck & Check).
  match type of PinsCheck with
  | context [match ?bad with Some _ => _ | None => _ end] =>
      destruct bad as [[bad_pin wrong]|] eqn:PinsSafe
  end.
  { match type of PinsCheck with
    | context [match ?bad with Some _ => _ | None => _ end] => destruct bad; discriminate PinsCheck
    end. }
  apply EaTypesSound.rbind_ok in Check as ([] & PinKeysCheck & Check).
  match type of PinKeysCheck with
  | context [match ?bad with Some _ => _ | None => _ end] =>
      destruct bad eqn:PinKeys; [discriminate PinKeysCheck|]
  end.
  apply EaTypesSound.rbind_ok in Check as ([] & ResultsCheck & Check).
  match type of ResultsCheck with
  | context [match ?bad with Some _ => _ | None => _ end] =>
      destruct bad eqn:Results; [discriminate ResultsCheck|]
  end.
  injection Check as <-. constructor; cbn [link_plan_keys link_plan_site link_plan_pins].
  - reflexivity.
  - reflexivity.
  - apply link_pin_provisions. exact PinKeys.
  - apply link_result_realizes. exact Results.
  - intros t tx. unfold pins. rewrite linked_pins_spec, FixedExact. reflexivity.
  - intros t tx y Pin Generation.
    destruct (pins_have_link_scope _ _ _ _ _ PinsSafe t tx y Pin Generation)
      as (Live & Unmerged & NotMade & FixedInput).
    split; [exact Live|]. split; [exact Unmerged|]. split; [exact NotMade|].
    intro Taken. now apply FixedExact, FixedInput.
Qed.

Lemma successful_link_lift : forall A (read : option A) message before value after,
  lift read message before = Ok (value, after) -> read = Some value /\ after = before.
Proof. intros A [x|] message before value after Run; [now injection Run as <- <-|discriminate]. Qed.

Lemma successful_link_check : forall (passed : bool) message before value after,
  check passed message before = Ok (value, after) -> passed = true /\ after = before.
Proof. intros [] message before value after Run; [now injection Run as _ <-|discriminate]. Qed.

Definition LinkedCodeInProgram (P : program) (bound : nat) (s : est) : Prop :=
  (forall h code, bound <= h < es_next s ->
    read_code s FunctorTable h = Some code -> funbody_of P h = Some code) /\
  (forall h target pins, bound <= h < es_next s ->
    read_origin s FunctorTable h = Some (target, pins) ->
    nlook h (linked_origins (program_origins P)) = Some target).

Definition TypedLinkCache (W : world) (P : program) (s : est) : Prop :=
  forall u gg slot, pair_look (u, gg) (es_link s) = Some slot ->
    LinkBody W P u gg (slot_index slot) (link_pins_at s (slot_index slot)).

Lemma initial_typed_link_cache : forall W P start,
  TypedLinkCache W P (init_state start).
Proof. intros W P start u gg slot Read. discriminate. Qed.

Theorem linked_code_in_program_earlier : forall P bound before after,
  CodeTransition before after ->
  LinkedCodeInProgram P bound after -> LinkedCodeInProgram P bound before.
Proof.
  intros P bound before after Effect [Code Origin].
  pose proof (counter_grows _ _ Effect) as Counter. split.
  - intros h body Issued Read. apply Code; [lia|].
    now rewrite (old_slots_preserved _ _ Effect FunctorTable h) by lia.
  - intros h target pins Issued Read. apply (Origin h target pins); [lia|].
    now rewrite (old_origins_preserved _ _ Effect FunctorTable h) by lia.
Qed.

Local Lemma fresh_table_keeps_link_fields : forall E u roles before table after,
  EaFunctor.fresh_table E u roles before = Ok (table, after) ->
  es_link after = es_link before /\ es_pins after = es_pins before.
Proof.
  intros E u roles. induction roles as [|role rest IH]; intros before table after Run.
  - inversion Run. now split.
  - cbn [EaFunctor.fresh_table mmap] in Run.
    apply EaControlSound.mbind_ok in Run as (head & middle & Head & Run).
    apply EaControlSound.mbind_ok in Run as (tail & finish & Tail & Run).
    injection Run as _ <-. specialize (IH _ _ _ Tail).
    cbn [mbind fresh put_name ret] in Head. inversion Head; subst middle.
    exact IH.
Qed.

Local Lemma link_output_roles_keeps_cache : forall E W P u Cu Cb who before result after,
  TypedLinkCache W P before ->
  link_output_roles E u Cu Cb who before = Ok (result, after) ->
  TypedLinkCache W P after.
Proof.
  intros E W P u Cu Cb who before result after Cache Run. unfold link_output_roles in Run.
  apply EaControlSound.mbind_ok in Run as (checked & s1 & Check & Run).
  match type of Check with
  | context [match ?missing with Some _ => _ | None => _ end] =>
      destruct missing; [discriminate Check|]
  end.
  injection Check as _ <-.
  apply EaControlSound.mbind_ok in Run as (table & s2 & Allocated & Run).
  apply EaControlSound.mbind_ok in Run as (promised & s3 & Promised & Run).
  unfold pure in Promised.
  match type of Promised with
  | context [match ?provided with Ok _ => _ | Err _ => _ end] =>
      destruct provided; [|discriminate Promised]
  end.
  injection Promised as _ <-. injection Run as _ <-.
  destruct (fresh_table_keeps_link_fields _ _ _ _ _ _ Allocated) as [Links Pins].
  unfold TypedLinkCache, link_pins_at in *. rewrite Links, Pins. exact Cache.
Qed.

Local Lemma emitted_link_cache_rows : forall u gg pins code before h after,
  emit_linked_body u gg pins code before = Ok (h, after) ->
  es_link after = ((u, gg), h) :: es_link before.
Proof.
  intros u gg pins code before h after Run.
  unfold emit_linked_body, emit_fun_code, reserve_code, mbind, fresh, ret,
    write_fun_code, set_fun_body, put_pins, put_link in Run.
  inversion Run. reflexivity.
Qed.

Theorem emit_linked_body_preserves_typed_cache : forall W P u gg pins code before h after,
  LinkRecords before -> TypedLinkCache W P before ->
  emit_linked_body u gg pins code before = Ok (h, after) ->
  LinkBody W P u gg (slot_index h) (link_pins_at after (slot_index h)) ->
  TypedLinkCache W P after.
Proof.
  intros W P u gg pins code before h after Registered Cache Emitted Linked target provider slot Lookup.
  rewrite (emitted_link_cache_rows _ _ _ _ _ _ _ Emitted) in Lookup.
  cbn [pair_look fst snd] in Lookup.
  destruct (Nat.eqb target u && Nat.eqb provider gg) eqn:Same.
  - apply andb_true_iff in Same as [Target Provider].
    apply Nat.eqb_eq in Target, Provider. subst target provider.
    now injection Lookup as <-.
  - pose proof (EaBindCodeEffects.cached_link_records_origin before target provider slot before Registered
      ltac:(unfold get_link; now rewrite Lookup)) as (_ & Below & Origin & _).
    pose proof (proj1 (EaBindCodeEffects.emit_linked_body_records_origin _ _ _ _ _ _ _ Emitted)) as Effect.
    pose proof (old_origins_preserved _ _ Effect FunctorTable _ Below) as Kept.
    rewrite Origin in Kept. cbn [read_origin] in Kept.
    destruct (read_link_target after (slot_index slot)); [|discriminate].
    injection Kept as _ Pins. rewrite Pins. exact (Cache _ _ _ Lookup).
Qed.

Section Linking.
Variables (E : env) (P : program) (R : rec) (bound : nat) (Contains : est -> Prop).
Let W := e_W E.

Hypotheses
  (Declared : forall n d, w_decl W n = Some d -> n < bound)
  (Budget : EaBudget.metadata_budget W bound < e_metadata_fuel E)
  (Contexts : EaContextTableSound.ContextTableSound W)
  (FamilyComplete : forall u, Family W u -> EaFamily.is_family E u = true)
  (LinkedRead : forall state, Contains state -> LinkedCodeInProgram P bound state)
  (ReadEarlier : forall before after,
    CodeTransition before after -> Contains after -> Contains before)
  (NestedEffect : forall F C items, PreservesOldCode (r_bind_items R F C items)).

Variable Extra : est -> Prop.
Hypothesis RolesExtra : forall u Cu Cb label before result after,
  Extra before -> link_output_roles E u Cu Cb label before = Ok (result, after) -> Extra after.
Hypothesis EmitExtra : forall u gg pins code before h after,
  Extra before -> emit_linked_body u gg pins code before = Ok (h, after) -> Extra after.

Theorem link_body_sound : forall u gg before h after,
  kind_sym W u = Some DKFunctor ->
  bound <= es_next before ->
  LinkRecords before -> TypedLinkCache W P before ->
  Extra before -> Contains after ->
  (forall dg bodies Cg nsg wrap Cb Fb made finish,
    FunCtx W gg Cg nsg -> Contains finish ->
    let Fg := link_body_frame E u dg bodies in
    r_bind_items R Fg Cg (List.concat bodies) before = Ok (((wrap, Cb), Fb), made, finish) ->
    BindItems W P Fg Cg (List.concat bodies) wrap Cb Fb made /\ TypedLinkCache W P finish /\ Extra finish) ->
  EaFunctor.link_body E R u gg before = Ok (h, after) ->
  LinkBody W P u gg (slot_index h) (link_pins_at after (slot_index h)) /\
  TypedLinkCache W P after /\ Extra after.
Proof.
  intros u gg before h after Abstract Floor Registered Cache State Final NestedTyping Run.
  destruct (LinkedRead _ Final) as [CodeTable OriginTable].
  unfold EaFunctor.link_body in Run. fold W in Run.
  apply EaControlSound.mbind_ok in Run as (cached & s1 & Cached & Run).
  unfold get_link in Cached. injection Cached as <- <-.
  destruct (pair_look (u, gg) (es_link before)) as [old|] eqn:CacheRead.
  { injection Run as <- <-. split; [exact (Cache _ _ _ CacheRead)|now split]. }
  apply EaControlSound.mbind_ok in Run as (dg & s1 & Read & Run).
  apply successful_link_lift in Read as [Provider ->].
  destruct (d_ast dg) as [| | | | | | | |name parameters inputs results body] eqn:Ast; try discriminate Run.
  destruct body as [bodies|]; [|discriminate Run].
  apply EaControlSound.mbind_ok in Run as ([Cu nsu] & s2 & Read & Run).
  apply successful_link_lift in Read as [TargetContext ->].
  apply EaControlSound.mbind_ok in Run as ([Cg nsg] & s3 & Read & Run).
  apply successful_link_lift in Read as [BodyContext ->].
  set (fixed_inputs := fixed W (e_metadata_fuel E) u) in *.
  set (Fg := link_body_frame E u dg bodies) in Run.
  apply EaControlSound.mbind_ok in Run as ([[[wrap Cb] Fb] made] & s4 & Body & Run).
  apply EaControlSound.mbind_ok in Run as (made_check & s5 & Made & Run).
  apply successful_link_check in Made as [Made ->].
  apply EaControlSound.mbind_ok in Run as ([[outs table] [Cr keys]] & s6 & Outputs & Run).
  pose proof (NestedEffect _ _ _ _ _ _ Body) as BodyEffect.
  pose proof (counter_grows _ _ BodyEffect) as Grows.
  destruct (EaLinkAllocationSound.link_output_roles_sound E bound _ _ _ _ _ _ _ _ _ _
    Declared Budget (Nat.le_trans _ _ _ Floor Grows) Outputs) as (OutputsTyped & _ & Next).
  set (generation := EaFunctor.gen_of table) in *.
  apply EaControlSound.mbind_ok in Run as (plan & s7 & CheckedResult & Run).
  unfold pure in CheckedResult.
  destruct (check_linked_result E u gg Cu Cg Cb Cr made outs generation keys)
    as [checked|] eqn:ResultPlan; [|discriminate CheckedResult].
  assert (CheckedPlan : checked = plan) by congruence. subst checked.
  assert (SameState : s7 = s6) by congruence. subst s7.
  assert (FixedExact : forall x, In x fixed_inputs <-> Fixed W u x).
  { intro x. unfold fixed_inputs. apply (EaResultBudget.fixed_exact W bound Declared); exact Budget. }
  destruct (check_linked_result_sound E u gg Cu Cg Cb Cr made outs generation keys plan
    FixedExact ResultPlan) as [PlanKeys PlanSite PinKeys Results PinsExact PinsScope].
  set (pins := link_plan_pins plan) in *.
  set (site := link_site Cg Cb pins outs generation) in *.
  rewrite PlanSite in PinKeys, Results.
  unfold translate_linked_result in Run. rewrite PlanKeys, PlanSite in Run.
  apply EaControlSound.mbind_ok in Run as (bindings & s10 & Read & Run).
  apply successful_link_lift in Read as [BindingList ->].
  apply EaControlSound.mbind_ok in Run as (family_check & s11 & FamilyCheck & Run).
  assert (FamilyReady : (Family W u -> FamilyBindings W bindings) /\ s11 = s6).
  {
    destruct (EaFamily.is_family E u) eqn:Classified.
    - match type of FamilyCheck with
      | context [match ?bad with Some _ => _ | None => _ end] =>
          destruct bad as [binding|] eqn:FamilyBindingsCheck
      end.
      + apply find_some in FamilyBindingsCheck as [_ Unsafe].
        destruct binding; try discriminate Unsafe; discriminate FamilyCheck.
      + split; [|now injection FamilyCheck as _ <-].
        intros _. apply (family_bindings_checked E); [|exact FamilyBindingsCheck].
        exact (EaFamilySound.is_family_sound E bound Declared Budget).
    - split; [intros Valid; rewrite (FamilyComplete _ Valid) in Classified; discriminate|].
      now injection FamilyCheck as _ <-.
  }
  destruct FamilyReady as [FamilyRule ->].
  destruct (EaBindCodeEffects.emit_linked_body_records_origin _ _ _ _ _ _ _ Run)
    as (Emitted & Code & Recorded & Range).
  pose proof (EaBindCodeEffects.link_output_roles_code_effect _ _ _ _ _ _ _ _ Outputs) as RolesEffect.
  pose proof (code_transition_compose _ _ _ RolesEffect Emitted) as AfterBody.
  pose proof (link_records_preserved _ _ BodyEffect Registered) as BodyRecords.
  pose proof (ReadEarlier _ _ AfterBody Final) as BodyRead.
  assert (Issued : bound <= slot_index h < es_next after) by lia.
  pose proof (OriginTable _ _ _ Issued Recorded) as OriginInProgram.
  cbn [read_origin] in Recorded.
  destruct (read_link_target after (slot_index h)) as [origin|] eqn:Origin; [|discriminate].
  injection Recorded as OriginPair PinsAt. subst origin.
  assert (FreshSlot : Fresh W (slot_index h)).
  { unfold Fresh. destruct (w_decl W (slot_index h)) as [d|] eqn:Collision; [|reflexivity].
    pose proof (Declared _ _ Collision). lia. }
  assert (FreshCode : TranslatesLinkedBody W P u gg (slot_index h) Cu Cg site outs bindings keys).
  { split; [exact FreshSlot|]. split; [|exact OriginInProgram].
    apply CodeTable; [exact Issued|]. replace (map (fun k => rkey_of site (at_site site k)) keys)
      with (map (fun k => rkof site (at_site site k)) keys); [exact Code|].
    apply map_ext. intro key. apply EaSatisfiesSound.rkof_exact. }
  assert (KindProvider : kind_sym W gg = Some DKFunctor).
  { unfold kind_sym. rewrite Provider, Ast. reflexivity. }
  assert (Target : FunCtx W u Cu nsu).
  { pose proof (Contexts _ _ _ TargetContext) as Valid. now rewrite Abstract in Valid. }
  assert (ProviderContext : FunCtx W gg Cg nsg).
  { pose proof (Contexts _ _ _ BodyContext) as Valid. now rewrite KindProvider in Valid. }
  destruct (NestedTyping _ _ _ _ _ _ _ _ _ ProviderContext BodyRead Body) as (TypedBody & BodyCache & BodyState).
  assert (Linked : LinkBody W P u gg (slot_index h) pins).
  {
    eapply (@LB W P u gg (slot_index h) dg name parameters inputs results bodies Cu nsu Cg nsg
      Fg wrap Cb Fb made outs generation Cr keys bindings pins); try reflexivity; try eassumption.
    - eapply EaNumbering.mk_frame_vars_numbering; [reflexivity|constructor].
    - intro t. change (In t (filter (fun x => nmem x (poly W (e_metadata_fuel E) u))
          (mentions W (e_metadata_fuel E) u)) <-> Mentions W u t /\ Poly W u t).
      rewrite filter_In, EaTypesSound.nmem_spec.
      now rewrite (EaResultBudget.mentions_exact W bound Declared _ u Budget t),
        (EaResultBudget.poly_exact W bound Declared _ u Budget t).
    - intros t Member. pose proof (proj1 (forallb_forall _ _) Made t Member) as Unbound.
      apply negb_true_iff in Unbound. unfold bound_in in Unbound.
      now destruct (nlook t (c_ty Cu)).
    - now apply binds_of_sound.
  }
  assert (Current : LinkBody W P u gg (slot_index h) (link_pins_at after (slot_index h)))
    by (rewrite PinsAt; exact Linked).
  split; [exact Current|split].
  - eapply emit_linked_body_preserves_typed_cache; [| |exact Run|exact Current].
    + exact (link_records_preserved _ _ RolesEffect BodyRecords).
    + eapply link_output_roles_keeps_cache; eassumption.
  - eapply EmitExtra; [eapply RolesExtra|]; eassumption.
Qed.

End Linking.

Theorem assembled_linked_code_in_program : forall E bound entry P after,
  EaMain.assemble E bound entry (init_state bound) = Ok (P, after) ->
  LinkedCodeInProgram P bound after.
Proof.
  intros E bound entry P after Run. split.
  - intros h code _ Read. rewrite (proj2 (EaAssemblyCodeEffects.assembled_code_lookup_exact _ _ _ _ _ Run)).
    exact Read.
  - intros h target pins _ Read. rewrite (assembled_link_origin _ _ _ _ _ _ h Run).
    fold (read_link_target after h). cbn [read_origin] in Read.
    destruct (read_link_target after h); [|discriminate]. now injection Read as -> _.
Qed.

Theorem link_body_at_checked_decls : forall E bound P fuel (Contains : est -> Prop) u gg before h after,
  (forall n d, w_decl (e_W E) n = Some d -> n < bound) ->
  EaBudget.metadata_budget (e_W E) bound < e_metadata_fuel E ->
  EaMain.decls_form E bound = Ok tt ->
  EaContextTableSound.ContextTableSound (e_W E) ->
  (forall state, Contains state -> LinkedCodeInProgram P bound state) ->
  (forall start finish, CodeTransition start finish -> Contains finish -> Contains start) ->
  kind_sym (e_W E) u = Some DKFunctor ->
  bound <= es_next before ->
  LinkRecords before -> TypedLinkCache (e_W E) P before ->
  Contains after ->
  (forall dg bodies Cg nsg wrap Cb Fb made finish,
    FunCtx (e_W E) gg Cg nsg -> Contains finish ->
    let Fg := link_body_frame E u dg bodies in
    r_bind_items (EaMain.mkrec (EaMain.run E fuel)) Fg Cg (List.concat bodies) before =
      Ok (((wrap, Cb), Fb), made, finish) ->
    BindItems (e_W E) P Fg Cg (List.concat bodies) wrap Cb Fb made /\
    TypedLinkCache (e_W E) P finish) ->
  EaFunctor.link_body E (EaMain.mkrec (EaMain.run E fuel)) u gg before = Ok (h, after) ->
  LinkBody (e_W E) P u gg (slot_index h) (link_pins_at after (slot_index h)) /\
  TypedLinkCache (e_W E) P after.
Proof.
  intros E bound P fuel Contains u gg before h after Declared Budget Decls Contexts
    LinkedRead ReadEarlier Kind Floor Records Cache Code Nested Run.
  destruct (EaExpressionCodeEffects.mkrec_code_effect _ (EaExpressionCodeEffects.run_code_effect E fuel))
    as (_ & _ & Effects).
  destruct (link_body_sound E P (EaMain.mkrec (EaMain.run E fuel)) bound Contains
    Declared Budget Contexts (EaFamilyComplete.decls_form_family_classifier E bound Declared Budget Decls)
    LinkedRead ReadEarlier Effects (fun _ => True) ltac:(auto) ltac:(auto)
    u gg before h after Kind Floor Records Cache I Code) as [Typed [Checked _]].
  - intros dg bodies Cg nsg wrap Cb Fb made finish Formal Kept Fg Run'.
    destruct (Nested dg bodies Cg nsg wrap Cb Fb made finish Formal Kept Run') as [Body Good].
    now split.
  - exact Run.
  - now split.
Qed.
