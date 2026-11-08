(* Initial unresolved generations have closed, distinct opaque identities.
   Opaque heads occupy a reserved prefix disjoint from all immutable slots. *)
From Stdlib Require Import List Arith Lia String.
From NM Require Import ExecutionModel CoreMoss Elab ReifyData Reify ReifyRoles ReifyInterfaces ReifySource.
Import ListNotations.

Definition initial_descriptors (machines : subst) (generations : list tsym)
    (opaque_start : nat) : subst :=
  map (fun p => (fst p,match lookup_subst (fst p) machines with
    | Some known => known | None => TNom (snd p) [] end))
    (combine generations (seq opaque_start (List.length generations))).

Definition initial_type_entries (roles descriptors : subst) : xdenv :=
  flat_map (fun p => match snd p,lookup_subst (fst p) descriptors with
    | TAbs role,Some meaning => [(ITy role,XDType meaning)]
    | _,_ => [] end) roles.
Definition initial_type_grounding (roles descriptors : subst) : subst :=
  flat_map (fun p => match snd p,lookup_subst (fst p) descriptors with
    | TAbs role,Some meaning => [(role,meaning)]
    | _,_ => [] end) roles.

Record reified_initial_types : Type := ReifiedInitialTypes {
  rit_opaque_heads : list nsym;
  rit_descriptors : subst;
  rit_interface : source_type_interface;
  rit_environment : xdenv;
  rit_grounding : subst
}.

Definition lower_initial_types (source : st) (scope : reify_scope ety key)
    : reified_initial_types :=
  let generations := source_scope_type_keys scope in
  let opaque_start := st_next source + 6 in
  let heads := seq opaque_start (List.length generations) in
  let machines := runtime_machines (reify_runtime_schema source) (st_next source) in
  let descriptors := initial_descriptors machines generations opaque_start in
  let before := ReifyState (opaque_start + List.length generations) [] [] [] [] in
  let prepared := lower_source_type_interface source scope before in
  ReifiedInitialTypes heads descriptors prepared
    (initial_type_entries (sti_roles prepared) descriptors)
    (initial_type_grounding (sti_roles prepared) descriptors).

Lemma initial_descriptors_domain : forall machines generations start,
    map fst (initial_descriptors machines generations start) = generations.
Proof.
  intros machines generations start. unfold initial_descriptors. rewrite map_map.
  change (map fst (combine generations (seq start (List.length generations))) = generations).
  revert start. induction generations as [|g rest IH]; intro start; cbn.
  - reflexivity.
  - f_equal. apply IH.
Qed.

Lemma initial_descriptors_closed : forall machines generations start,
    grounding machines -> grounding (initial_descriptors machines generations start).
Proof.
  intros machines generations start Hmachines. unfold grounding,initial_descriptors.
  rewrite Forall_forall. intros [g tau] Hin. apply in_map_iff in Hin.
  destruct Hin as [[key head] [E _]]. cbn in E. inversion E; subst.
  destruct (lookup_subst g machines) eqn:Hlookup; cbn.
  - eapply grounding_lookup_closed; eassumption.
  - reflexivity.
Qed.

Lemma initial_opaque_heads_below_slots : forall source scope n,
    In n (rit_opaque_heads (lower_initial_types source scope)) ->
    n < st_next source + 6 + List.length (source_scope_type_keys scope).
Proof.
  intros source scope n Hn. cbn in Hn. apply in_seq in Hn. lia.
Qed.

Lemma runtime_machines_grounding : forall schema base,
    grounding (runtime_machines schema base).
Proof.
  intros schema base. unfold grounding,runtime_machines. rewrite Forall_forall.
  intros [g tau] Hin. apply in_concat in Hin as [part [Hpart Hin]].
  apply in_map_iff in Hpart as [[index name] [<- _]]. cbn in Hin.
  destruct (runtime_id schema "wasm"%string name); cbn in Hin; [|contradiction].
  destruct Hin as [E|[]]. inversion E; reflexivity.
Qed.

Lemma initial_type_grounding_closed : forall roles descriptors,
    grounding descriptors -> grounding (initial_type_grounding roles descriptors).
Proof.
  intros roles descriptors Hdesc. induction roles as [|[generation tau] rest IH]; [constructor|].
  destruct tau as [|formal|n args|ms]; cbn [initial_type_grounding]; try exact IH.
  destruct (lookup_subst generation descriptors) as [meaning|] eqn:E; cbn; try rewrite E; cbn; [|exact IH].
  constructor; [exact (@grounding_lookup_closed descriptors generation meaning Hdesc E)|exact IH].
Qed.

Lemma lower_initial_types_grounding : forall source scope,
    grounding (rit_grounding (lower_initial_types source scope)).
Proof.
  intros. unfold lower_initial_types. cbv zeta. cbn [rit_grounding].
  apply initial_type_grounding_closed,initial_descriptors_closed,runtime_machines_grounding.
Qed.

Lemma initial_type_entries_lookup : forall roles descriptors t,
    lookup_xdenv (ITy t) (initial_type_entries roles descriptors) =
      option_map XDType (lookup_subst t (initial_type_grounding roles descriptors)).
Proof.
  intros roles descriptors t. induction roles as [|[generation tau] rest IH]; [reflexivity|].
  destruct tau as [|formal|n args|ms]; cbn [initial_type_entries initial_type_grounding]; try exact IH.
  destruct (lookup_subst generation descriptors) eqn:E; cbn; try rewrite E; cbn; [|exact IH].
  destruct (Nat.eqb t formal); [reflexivity|exact IH].
Qed.

(* Default identities belong only to original source-world type symbols.
   Generated application outputs are deliberately absent: each such output
   receives its fresh meaning at its own binding, never at program entry. *)
Definition original_type_generations (source : st) : list tsym :=
  filter (fun t => match NMap.nget (w_syms (st_w source)) t with
    | Some g => kind_eqb (gs_kind g) KType | None => false end)
    (seq 0 (w_nsyms (st_w source))).
Definition global_default_scope (source : st) : reify_scope ety key :=
  ReifyScope ety key []
    (map (fun t => (t,YAbs t)) (original_type_generations source))
    [] [] [] [] [].
Definition lower_global_defaults (source : st) : reified_initial_types :=
  lower_initial_types source (global_default_scope source).

Lemma global_defaults_do_not_add_source_names : forall source,
    rf_names (sti_state (rit_interface (lower_global_defaults source))) = [].
Proof.
  intro source. unfold lower_global_defaults,lower_initial_types,
    lower_source_type_interface,enter_source_names,enter_source_generations.
  destruct (allocate_type_roles _ _) as [[roles sm] after] eqn:E.
  destruct (allocate_type_roles_domain _ _ _ _ _ E) as [_ [_ [_ Hscope]]].
  cbn in *. unfold enter_type_roles. cbn. exact (proj1 Hscope).
Qed.

Lemma original_type_generations_unique : forall source,
    NoDup (original_type_generations source).
Proof. intro source. apply NoDup_filter. apply seq_NoDup. Qed.

Lemma original_type_generations_declared : forall source t,
    In t (original_type_generations source) ->
    exists g, NMap.nget (w_syms (st_w source)) t = Some g /\
      gs_kind g = KType /\ t < w_nsyms (st_w source).
Proof.
  intros source t Hin. unfold original_type_generations in Hin.
  apply filter_In in Hin as [Hin Hkind]. apply in_seq in Hin.
  destruct (NMap.nget (w_syms (st_w source)) t) as [g|] eqn:E;
    [|discriminate].
  destruct (gs_kind g) eqn:K; cbn in Hkind; try discriminate.
  exists g. repeat split; try assumption; lia.
Qed.

Definition install_generation_defaults (defaults : subst) (runtime : reify_state)
    : reify_state :=
  ReifyState (rf_next runtime) (rf_names runtime)
    (rf_generations runtime ++ defaults) (rf_items runtime) (rf_declarations runtime).

Lemma generation_defaults_preserve_local : forall defaults runtime t tau,
    lookup_subst t (rf_generations runtime) = Some tau ->
    lookup_subst t (rf_generations (install_generation_defaults defaults runtime)) = Some tau.
Proof. intros. cbn. rewrite lookup_subst_app,H. reflexivity. Qed.

Lemma generation_defaults_supply_missing : forall defaults runtime t,
    lookup_subst t (rf_generations runtime) = None ->
    lookup_subst t (rf_generations (install_generation_defaults defaults runtime)) =
      lookup_subst t defaults.
Proof. intros. cbn. rewrite lookup_subst_app,H. reflexivity. Qed.

Lemma generation_defaults_do_not_add_names : forall defaults runtime,
    rf_names (install_generation_defaults defaults runtime) = rf_names runtime /\
    rf_items (install_generation_defaults defaults runtime) = rf_items runtime /\
    rf_next (install_generation_defaults defaults runtime) = rf_next runtime.
Proof. intros. repeat split; reflexivity. Qed.
