(* Executable type-role preparation for checked source interfaces. Roles
   represent immutable canonical generations, not mutable name-map keys.
   Input/output phases allocate disjoint roles; current source names are
   rendered separately and may denote composite types or old generations. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import Audit CoreMoss Elab ReifyData Reify ReifyRoles ReifySource PayloadInst.
Import ListNotations.

Definition source_scope_type_keys (scope : reify_scope ety key) : list tsym :=
  nodup Nat.eq_dec (flat_map (fun p => ety_gens (snd p)) (rs_generations ety key scope)).

Record source_type_interface : Type := SourceTypeInterface {
  sti_roles : subst;
  sti_map : smap;
  sti_equations : subst;
  sti_state : reify_state
}.

Definition source_role_equations (source : st) (scope : reify_scope ety key)
    (roles : subst) : subst :=
  flat_map (fun p => match snd p with
    | TAbs role => [(role,Audit.app_subst roles
        (reify_snapshot_type source scope (YAbs (fst p))))]
    | _ => [] end) roles.

(* Cached raw IR may retain any immutable source generation key, including
   aliases which the snapshot UF has since equated. Preserve every such
   key, while all RHS meanings use the newly allocated canonical roles. *)
Definition enter_source_generations (source : st) (scope : reify_scope ety key)
    (runtime : reify_state) : reify_state :=
  ReifyState (rf_next runtime) (rf_names runtime)
    (map (fun p => (fst p,Audit.app_subst (rf_generations runtime)
      (reify_snapshot_type source scope (snd p))))
      (rs_generations ety key scope) ++ rf_generations runtime)
    (rf_items runtime) (rf_declarations runtime).

Lemma enter_source_generations_domain : forall source scope runtime,
    map fst (rf_generations (enter_source_generations source scope runtime)) =
      map fst (rs_generations ety key scope) ++ map fst (rf_generations runtime).
Proof.
  intros. unfold enter_source_generations. cbn.
  rewrite map_app, map_map. reflexivity.
Qed.

Lemma enter_source_generations_preserves_names : forall source scope runtime,
    rf_names (enter_source_generations source scope runtime) = rf_names runtime /\
    rf_items (enter_source_generations source scope runtime) = rf_items runtime /\
    rf_next (enter_source_generations source scope runtime) = rf_next runtime.
Proof. intros. repeat split; reflexivity. Qed.

Definition enter_source_names (source : st) (scope : reify_scope ety key)
    (runtime : reify_state) : reify_state :=
  ReifyState (rf_next runtime)
    (map (fun p => (fst p,Audit.app_subst (rf_generations runtime)
      (reify_snapshot_type source scope (snd p)))) (rs_names ety key scope) ++ rf_names runtime)
    (rf_generations runtime) (rf_items runtime) (rf_declarations runtime).

Definition lower_source_type_interface (source : st) (scope : reify_scope ety key)
    (before : reify_state) : source_type_interface :=
  let '(roles,sm,after) := allocate_type_roles (source_scope_type_keys scope) before in
  SourceTypeInterface roles sm (source_role_equations source scope roles)
    (enter_source_names source scope
      (enter_source_generations source scope (enter_type_roles roles after))).

Lemma source_scope_type_keys_unique : forall scope,
    NoDup (source_scope_type_keys scope).
Proof. intros. apply NoDup_nodup. Qed.

Lemma lower_source_type_interface_domain : forall source scope before,
    map fst (sti_roles (lower_source_type_interface source scope before)) =
      source_scope_type_keys scope /\
    map fst (sti_map (lower_source_type_interface source scope before)) =
      map ITy (source_scope_type_keys scope) /\
    rf_next (sti_state (lower_source_type_interface source scope before)) =
      rf_next before + length (source_scope_type_keys scope).
Proof.
  intros source scope before. unfold lower_source_type_interface.
  destruct (allocate_type_roles (source_scope_type_keys scope) before)
    as [[roles sm] after] eqn:E. cbn.
  destruct (allocate_type_roles_domain _ _ _ _ _ E) as [Hroles [Hsm [Hnext _]]].
  repeat split; assumption.
Qed.

Lemma source_role_equations_domain : forall source scope roles,
    Forall (fun p => exists role, snd p = TAbs role) roles ->
    map fst (source_role_equations source scope roles) =
      flat_map (fun p => match snd p with TAbs role => [role] | _ => [] end) roles.
Proof.
  intros source scope roles _. unfold source_role_equations.
  rewrite payload_map_flat_map. apply payload_flat_map_ext.
  intros [t tau] Hin. destruct tau; reflexivity.
Qed.

Definition signature_type_inputs (signature : fnsig) : tele :=
  map ITy (nodup Nat.eq_dec (CoreMoss.msig_frees signature)).
Definition value_type_inputs (tau : ty) : tele :=
  map ITy (nodup Nat.eq_dec (CoreMoss.ty_frees tau)).
Definition snapshot_signature (source : st) (scope : reify_scope ety key)
    (signature : reify_signature ety) : fnsig :=
  mkSig (map (reify_snapshot_type source scope) (rsg_params ety signature))
    (reify_snapshot_type source scope (rsg_result ety signature)).

Fixpoint lower_scope_values (source : st) (scope : reify_scope ety key)
    (values : list (vsym * ety)) (before : reify_state) : tele * reify_state :=
  match values with
  | [] => ([],before)
  | (name,tau) :: rest =>
      let raw := reify_snapshot_type source scope tau in
      let inputs := value_type_inputs (Audit.app_subst (rf_generations before) raw) in
      let '(slot,middle) := lower_value_provision name inputs raw before in
      let '(tail,after) := lower_scope_values source scope rest middle in
      (IVal slot :: tail,after)
  end.
Fixpoint lower_scope_functions (source : st) (scope : reify_scope ety key)
    (functions : list (fsym * reify_signature ety)) (before : reify_state)
    : tele * reify_state :=
  match functions with
  | [] => ([],before)
  | (name,signature) :: rest =>
      let raw := snapshot_signature source scope signature in
      let inputs := signature_type_inputs (lower_fn_signature before raw) in
      let '(slot,middle) := lower_function_provision name inputs raw before in
      let '(tail,after) := lower_scope_functions source scope rest middle in
      (IFn slot :: tail,after)
  end.
Fixpoint lower_scope_methods (source : st) (scope : reify_scope ety key)
    (methods : list ((ety * msym) * reify_signature ety)) (before : reify_state)
    : tele * reify_state :=
  match methods with
  | [] => ([],before)
  | ((receiver,name),signature) :: rest =>
      let raw := snapshot_signature source scope signature in
      let inputs := signature_type_inputs (lower_fn_signature before raw) in
      let original := reify_snapshot_item source scope (KyMth receiver name) in
      let '(slot,middle) := lower_method_provision original
        (reify_snapshot_type source scope receiver) inputs raw before in
      let '(tail,after) := lower_scope_methods source scope rest middle in
      (slot :: tail,after)
  end.

Definition lower_scope_first_order (source : st) (scope : reify_scope ety key)
    (before : reify_state) : tele * reify_state :=
  let '(values,s1) := lower_scope_values source scope (rs_values ety key scope) before in
  let '(functions,s2) := lower_scope_functions source scope (rs_functions ety key scope) s1 in
  let '(methods,s3) := lower_scope_methods source scope (rs_methods ety key scope) s2 in
  (values ++ functions ++ methods,s3).

Lemma lower_scope_values_preserves_types : forall source scope values before outputs after,
    lower_scope_values source scope values before = (outputs,after) ->
    rf_names after = rf_names before /\ rf_generations after = rf_generations before /\
    length outputs = length values /\ rf_next after = rf_next before + length values.
Proof.
  induction values as [|[name tau] rest IH]; intros before outputs after H.
  - cbn in H. inversion H; subst. repeat split; try reflexivity; cbn; lia.
  - cbn in H. destruct (lower_scope_values source scope rest _) as [tail middle] eqn:E.
    inversion H; subst. destruct (IH _ _ _ E) as [Hn [Hg [Hl Hnext]]].
    cbn in *. repeat split; try congruence; lia.
Qed.

Example scope_old_generation_is_not_current_name : forall source,
  let snapshot := ReifyScope ety key [(1,YAbs 2)]
    [(1,YAbs 1);(2,YAbs 2)] [] [] [] [] [] in
  let before := ReifyState 100 [] [] [] [] in
  let after := sti_state (lower_source_type_interface source snapshot before) in
  lookup_subst 1 (rf_generations after) = Some (TAbs 100) /\
  lookup_subst 1 (rf_names after) = Some (TAbs 101).
Proof. intros. split; reflexivity. Qed.

Example scope_closed_name_needs_no_generation : forall source,
  let snapshot := ReifyScope ety key [(1,YNom 7 [])]
    [(1,YNom 7 [])] [] [] [] [] [] in
  let before := ReifyState 100 [] [] [] [] in
  let result := lower_source_type_interface source snapshot before in
  sti_roles result = [] /\
  lookup_subst 1 (rf_names (sti_state result)) = Some (TNom 7 []).
Proof. intros. split; reflexivity. Qed.

Example scope_cached_generation_alias_retained : forall source,
  let snapshot := ReifyScope ety key [(1,YAbs 2)]
    [(1,YAbs 2);(2,YAbs 2)] [] [] [] [] [] in
  let before := ReifyState 100 [] [] [] [] in
  let result := lower_source_type_interface source snapshot before in
  sti_roles result = [(2,TAbs 100)] /\
  lookup_subst 1 (rf_generations (sti_state result)) = Some (TAbs 100) /\
  lookup_subst 2 (rf_generations (sti_state result)) = Some (TAbs 100).
Proof. intros. repeat split; reflexivity. Qed.

(* A lookup stack is newest first; telescope/maps retain written order. *)
Lemma lower_scope_values_item_order : forall source scope values before outputs after,
    lower_scope_values source scope values before = (outputs,after) ->
    map snd (rev (rf_items after)) = map snd (rev (rf_items before)) ++ outputs.
Proof.
  intros source scope values. induction values as [|[name raw] rest IH];
    intros before outputs after H; cbn [lower_scope_values] in H.
  - inversion H. rewrite app_nil_r. reflexivity.
  - cbn [lower_value_provision] in H.
    destruct (lower_scope_values source scope rest _) as [tail middle] eqn:E.
    inversion H; subst. rewrite (IH _ _ _ E).
    cbn [lower_value_provision rf_items rev]. rewrite map_app. cbn.
    rewrite <- app_assoc. reflexivity.
Qed.

Lemma lower_scope_functions_item_order : forall source scope functions before outputs after,
    lower_scope_functions source scope functions before = (outputs,after) ->
    map snd (rev (rf_items after)) = map snd (rev (rf_items before)) ++ outputs.
Proof.
  intros source scope functions. induction functions as [|[name signature] rest IH];
    intros before outputs after H; cbn [lower_scope_functions] in H.
  - inversion H. rewrite app_nil_r. reflexivity.
  - cbn [lower_function_provision] in H.
    destruct (lower_scope_functions source scope rest _) as [tail middle] eqn:E.
    inversion H; subst. rewrite (IH _ _ _ E).
    cbn [lower_function_provision rf_items rev]. rewrite map_app. cbn.
    rewrite <- app_assoc. reflexivity.
Qed.

Lemma lower_scope_methods_item_order : forall source scope methods before outputs after,
    lower_scope_methods source scope methods before = (outputs,after) ->
    map snd (rev (rf_items after)) = map snd (rev (rf_items before)) ++ outputs.
Proof.
  intros source scope methods. induction methods as [|[[receiver name] signature] rest IH];
    intros before outputs after H; cbn [lower_scope_methods] in H.
  - inversion H. rewrite app_nil_r. reflexivity.
  - cbn [lower_method_provision] in H.
    destruct (lower_scope_methods source scope rest _) as [tail middle] eqn:E.
    inversion H; subst. rewrite (IH _ _ _ E).
    cbn [lower_method_provision rf_items rev]. rewrite map_app. cbn.
    rewrite <- app_assoc. reflexivity.
Qed.

Lemma lower_scope_first_order_item_order : forall source scope before outputs after,
    lower_scope_first_order source scope before = (outputs,after) ->
    map snd (rev (rf_items after)) = map snd (rev (rf_items before)) ++ outputs.
Proof.
  intros source scope before outputs after H. unfold lower_scope_first_order in H.
  destruct (lower_scope_values source scope _ before) as [values s1] eqn:E1.
  destruct (lower_scope_functions source scope _ s1) as [functions s2] eqn:E2.
  destruct (lower_scope_methods source scope _ s2) as [methods s3] eqn:E3.
  inversion H; subst. rewrite (lower_scope_methods_item_order _ _ _ _ _ _ E3),
    (lower_scope_functions_item_order _ _ _ _ _ _ E2),
    (lower_scope_values_item_order _ _ _ _ _ _ E1).
  repeat rewrite app_assoc. reflexivity.
Qed.

Lemma lower_source_type_interface_preserves_items : forall source scope before,
    rf_items (sti_state (lower_source_type_interface source scope before)) = rf_items before.
Proof.
  intros source scope before. unfold lower_source_type_interface.
  destruct (allocate_type_roles (source_scope_type_keys scope) before)
    as [[roles sm] after] eqn:E.
  cbn [sti_state enter_source_names enter_source_generations enter_type_roles rf_items].
  destruct (allocate_type_roles_domain _ _ _ _ _ E) as [_ [_ [_ [_ [_ Hitems]]]]].
  exact Hitems.
Qed.
