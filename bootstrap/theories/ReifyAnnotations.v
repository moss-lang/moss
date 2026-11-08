(* Reusable source annotations are selected by identity and the current
   canonical type-name view. Checking chronology need not match IR nesting. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel CoreMoss Elab ReifyData Reify ReifySource BodyTr.
Import ListNotations.

Definition annotation_type (source : st) (runtime : reify_state) (t : ety) : ty :=
  ExecutionModel.app_subst (rf_generations runtime) (tr (reify_field_number source) t).

Definition annotation_arguments_match (source : st) (aliases : subst)
    (runtime : reify_state) (arguments : list (tsym * ety)) : bool :=
  forallb (fun p => match lookup_subst (fst p) (rf_names runtime) with
    | Some meaning => ty_eqb (ExecutionModel.app_subst aliases meaning)
        (ExecutionModel.app_subst aliases (annotation_type source runtime (snd p)))
    | None => false end) arguments &&
  forallb (fun t => existsb (fun p => Nat.eqb t (fst p)) arguments)
    (map fst (rf_names runtime)).

Lemma annotation_match_meanings : forall source aliases runtime arguments,
    annotation_arguments_match source aliases runtime arguments = true ->
    forall name raw, In (name,raw) arguments ->
    exists meaning, lookup_subst name (rf_names runtime) = Some meaning /\
      ExecutionModel.app_subst aliases meaning =
      ExecutionModel.app_subst aliases (annotation_type source runtime raw).
Proof.
  intros source aliases runtime arguments H name raw Hin.
  apply andb_true_iff in H as [H _].
  apply forallb_forall with (x:=(name,raw)) in H; [|exact Hin].
  change (match lookup_subst name (rf_names runtime) with
    | Some meaning => ty_eqb (ExecutionModel.app_subst aliases meaning)
      (ExecutionModel.app_subst aliases (annotation_type source runtime raw))
    | None => false end = true) in H.
  destruct (lookup_subst name (rf_names runtime)) as [meaning|] eqn:E;
    [|discriminate].
  exists meaning. split; [reflexivity|]. apply ty_eqb_eq. exact H.
Qed.

Lemma annotation_match_domain : forall source aliases runtime arguments,
    annotation_arguments_match source aliases runtime arguments = true ->
    forall name, In name (map fst (rf_names runtime)) ->
      In name (map fst arguments).
Proof.
  intros source aliases runtime arguments H name Hin.
  apply andb_true_iff in H as [_ H]. apply forallb_forall with (x:=name) in H; [|exact Hin].
  apply existsb_exists in H as [[key raw] [Hkey Heq]].
  apply Nat.eqb_eq in Heq. cbn in Heq. subst key.
  apply in_map with (f:=@fst nat ety) in Hkey. exact Hkey.
Qed.

Fixpoint find_value_annotation (source : st) (aliases : subst) (runtime : reify_state)
    (name : vsym) (events : list (reify_event ety key)) : option ety :=
  match events with
  | [] => None
  | REValue _ _ candidate meaning arguments :: rest =>
      if Nat.eqb name candidate && annotation_arguments_match source aliases runtime arguments
      then Some meaning else find_value_annotation source aliases runtime name rest
  | _ :: rest => find_value_annotation source aliases runtime name rest
  end.

Lemma found_value_annotation_matches : forall source aliases runtime name events raw,
    find_value_annotation source aliases runtime name events = Some raw ->
    exists arguments,
      In (REValue ety key name raw arguments) events /\
      annotation_arguments_match source aliases runtime arguments = true.
Proof.
  intros source aliases runtime name events. induction events as [|event rest IH];
    intros raw H; [discriminate|].
  destruct event as [type_name meaning | value_name meaning arguments |
    provider inputs signature arguments | function_name provider signature arguments |
    receiver method_name provider signature arguments | functor_name provider inputs arguments copy_view |
    functor_name inputs outputs input_arguments output_arguments output_scope]; cbn in H;
    try (destruct (IH _ H) as [args [Hin Hmatch]];
      exists args; split; [right; exact Hin|exact Hmatch]).
  destruct (Nat.eqb name value_name &&
    annotation_arguments_match source aliases runtime arguments) eqn:E.
  - inversion H; subst meaning. apply andb_true_iff in E as [E Hmatch].
    apply Nat.eqb_eq in E. subst value_name.
    exists arguments. split; [left; reflexivity|exact Hmatch].
  - destruct (IH _ H) as [args [Hin Hmatch]].
    exists args. split; [right; exact Hin|exact Hmatch].
Qed.

Definition function_annotation_matches (source : st) (aliases : subst)
    (runtime : reify_state) (name provider : fsym) (event : reify_event ety key) : bool :=
  match event with
  | REFunction _ _ candidate code _ arguments =>
      Nat.eqb name candidate && Nat.eqb provider code &&
      annotation_arguments_match source aliases runtime arguments
  | _ => false end.
(* Inferred method parameters belong to this checked method occurrence.
   They are carried by its actual IR theta, rather than installed as mutable
   caller source names. Use that explicit evidence when selecting metadata. *)
Definition annotation_parameter_runtime (runtime : reify_state) (theta : subst)
    : reify_state :=
  ReifyState (rf_next runtime)
    (map (fun p => (fst p,ExecutionModel.app_subst (rf_generations runtime) (snd p))) theta ++
      rf_names runtime)
    (rf_generations runtime) (rf_items runtime) (rf_declarations runtime).

Lemma annotation_parameter_match_meaning : forall source aliases runtime theta arguments name raw tau,
  annotation_arguments_match source aliases
    (annotation_parameter_runtime runtime theta) arguments = true ->
  In (name,raw) arguments -> lookup_subst name theta = Some tau ->
  ExecutionModel.app_subst aliases (ExecutionModel.app_subst (rf_generations runtime) tau) =
    ExecutionModel.app_subst aliases (annotation_type source runtime raw).
Proof.
  intros source aliases runtime theta arguments name raw tau Match Member Assigned.
  destruct (annotation_match_meanings _ _ _ _ Match _ _ Member)
    as [meaning [Lookup Meaning]].
  cbn [annotation_parameter_runtime rf_names] in Lookup.
  rewrite lookup_subst_app in Lookup.
  change (match lookup_subst name
    (map_snd (ExecutionModel.app_subst (rf_generations runtime)) theta) with
    | Some x => Some x | None => lookup_subst name (rf_names runtime) end = Some meaning) in Lookup.
  rewrite lookup_map_snd,Assigned in Lookup;injection Lookup as <-.
  exact Meaning.
Qed.

Definition method_annotation_matches (source : st) (aliases : subst)
    (runtime : reify_state) (receiver : ty) (method : msym) (theta : subst) (provider : fsym)
    (event : reify_event ety key) : bool :=
  match event with
  | REMethod _ _ raw candidate code _ arguments =>
      Nat.eqb method candidate && Nat.eqb provider code &&
      ty_eqb (annotation_type source runtime raw)
        (ExecutionModel.app_subst (rf_generations runtime) receiver) &&
      annotation_arguments_match source aliases (annotation_parameter_runtime runtime theta) arguments
  | _ => false end.
Definition functor_annotation_matches (source : st) (aliases : subst)
    (runtime : reify_state) (name provider : usym) (event : reify_event ety key) : bool :=
  match event with
  | REFunctor _ _ candidate code _ arguments _ =>
      Nat.eqb name candidate && Nat.eqb provider code &&
      annotation_arguments_match source aliases runtime arguments
  | _ => false end.

Definition annotation_apply_theta (source : st)
    (inputs outputs : list (tsym * ety)) : subst :=
  map (fun p => (fst p,tr (reify_field_number source) (snd p))) inputs ++
  flat_map (fun p => match snd p with
    | YAbs generation => if Nat.eqb (fst p) generation then []
                         else [(fst p,TAbs generation)]
    | _ => [] end) outputs.
Definition application_annotation_matches (source : st) (name : usym)
    (theta : subst) (event : reify_event ety key) : bool :=
  match event with
  | REApply _ _ candidate _ _ inputs outputs _ =>
      Nat.eqb name candidate && subst_eqb theta (annotation_apply_theta source inputs outputs)
  | _ => false end.

Definition find_reify_annotation (matches : reify_event ety key -> bool)
    (events : list (reify_event ety key)) : option (reify_event ety key) :=
  find matches events.
Lemma found_reify_annotation : forall matches events event,
    find_reify_annotation matches events = Some event ->
    In event events /\ matches event = true.
Proof. intros. apply find_some. exact H. Qed.

Example value_annotation_selected_by_current_name_view : forall source,
  let runtime := ReifyState 300 [(1,TAbs 100)] [(1,TAbs 200);(2,TAbs 201)] [] [] in
  let aliases := [(100,TAbs 201)] in
  let events := [REValue ety key 8 (YAbs 1) [(1,YAbs 1)];
                 REValue ety key 8 (YAbs 2) [(1,YAbs 2)]] in
  find_value_annotation source aliases runtime 8 events = Some (YAbs 2).
Proof. intro source. reflexivity. Qed.

Example application_annotation_keeps_input_and_output_occurrences : forall source,
  application_annotation_matches source 7 [(1,TAbs 9);(1,TAbs 10)]
    (REApply ety key 7 [] [] [(1,YAbs 9)] [(1,YAbs 10)]
      (ReifyScope ety key [] [] [] [] [] [] [])) = true.
Proof. intro source. reflexivity. Qed.
