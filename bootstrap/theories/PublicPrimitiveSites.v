(* Base's nonallocating primitive calls ignore the recorded method site.
   The normalization below is proof-side. It exposes exactly the fields
   that FastPath may erase while retaining type meanings and source aliases. *)
From Stdlib Require Import List Lia.
From NM Require Import Audit AuditFacts PublicValueTyping.
Import ListNotations Audit.Semantics.

Module BaseSites.
Definition retained_site (p : prim) (site : option ty) : option ty :=
  match p with PBox _ | PArrNew _ => site | _ => None end.

Fixpoint erase_entry (entry : provision_value) : provision_value :=
  match entry with
  | ValueProvision value => ValueProvision value
  | FunctionClosure function captured => FunctionClosure function (erase_environment captured)
  | FunctorClosure body adaptation captured outputs =>
      FunctorClosure body adaptation (erase_environment captured) outputs
  | PrimitiveClosure operation site => PrimitiveClosure operation (retained_site operation site)
  end
with erase_environment (runtime : environment) : environment :=
  match runtime with
  | EnvironmentWithAliases meanings aliases entries =>
      EnvironmentWithAliases meanings aliases
        ((fix erase_entries (rest : list (item * provision_value)) :=
          match rest with
          | [] => []
          | (key, entry) :: tail => (key, erase_entry entry) :: erase_entries tail
          end) entries)
  end.

Definition erase_entries (entries : list (item * provision_value)) :=
  map (fun entry => (fst entry, erase_entry (snd entry))) entries.

Lemma erase_environment_eq : forall meanings aliases entries,
  erase_environment (EnvironmentWithAliases meanings aliases entries) =
  EnvironmentWithAliases meanings aliases (erase_entries entries).
Proof.
  intros meanings aliases entries. cbn [erase_environment]. f_equal.
  induction entries as [|[key entry] tail IH]; cbn [erase_entries map]; [reflexivity|].
  f_equal. exact IH.
Qed.

Lemma retained_site_idempotent : forall operation site,
  retained_site operation (retained_site operation site) = retained_site operation site.
Proof. intros operation site; destruct operation; reflexivity. Qed.

Fixpoint erase_entry_idempotent (entry : provision_value) :
  erase_entry (erase_entry entry) = erase_entry entry
with erase_environment_idempotent (environment : environment) :
  erase_environment (erase_environment environment) = erase_environment environment.
Proof.
  - destruct entry; cbn [erase_entry]; rewrite ?erase_environment_idempotent,
      ?retained_site_idempotent; reflexivity.
  - destruct environment as [meanings aliases entries]. rewrite !erase_environment_eq.
    unfold erase_entries. rewrite map_map. f_equal.
    induction entries as [|[key entry] tail IH]; cbn in *; [reflexivity|].
    rewrite erase_entry_idempotent, IH. reflexivity.
Qed.

Lemma erased_meanings : forall d, type_meanings (erase_environment d) = type_meanings d.
Proof. intros [meanings aliases entries]; reflexivity. Qed.
Lemma erased_aliases : forall d, type_aliases (erase_environment d) = type_aliases d.
Proof. intros [meanings aliases entries]; reflexivity. Qed.
Lemma erased_ground : forall d tau, ground (erase_environment d) tau = ground d tau.
Proof. intros d tau. unfold ground. rewrite erased_meanings. reflexivity. Qed.
Lemma erased_output_meaning : forall d source,
  output_meaning (erase_environment d) source = output_meaning d source.
Proof. intros [meanings aliases entries] source; reflexivity. Qed.

Lemma erase_install : forall key entry d,
  erase_environment (install_provision key entry d) =
  install_provision key (erase_entry entry) (erase_environment d).
Proof.
  intros key entry [meanings aliases entries]. unfold install_provision.
  rewrite !erase_environment_eq. reflexivity.
Qed.
Lemma erase_append : forall left right,
  erase_environment (append_environment left right) =
  append_environment (erase_environment left) (erase_environment right).
Proof.
  intros [lm la lp] [rm ra rp]. unfold append_environment.
  rewrite !erase_environment_eq. cbn [type_meanings type_aliases provision_values].
  unfold erase_entries. rewrite map_app. reflexivity.
Qed.
Lemma erase_bind_type : forall d symbol meaning,
  erase_environment (bind_type d symbol meaning) =
  bind_type (erase_environment d) symbol meaning.
Proof.
  intros [meanings aliases entries] symbol meaning. unfold bind_type, ground.
  rewrite !erase_environment_eq. reflexivity.
Qed.
Lemma erase_instantiate : forall d substitution,
  erase_environment (instantiate d substitution) = instantiate (erase_environment d) substitution.
Proof.
  intros d substitution. unfold instantiate. rewrite erase_environment_eq.
  f_equal; apply map_ext; intros [symbol meaning]; cbn [fst snd]; rewrite erased_ground; reflexivity.
Qed.
Lemma erase_typed_capture : forall d capture captured,
  erase_environment (typed_capture d capture captured) =
  typed_capture (erase_environment d) capture (erase_environment captured).
Proof.
  intros d capture [meanings aliases entries]. unfold typed_capture.
  rewrite !erase_environment_eq. cbn [provision_values].
  f_equal; apply map_ext; intros [symbol meaning]; cbn [fst snd]; rewrite erased_ground; reflexivity.
Qed.
Lemma erase_capture_environment : forall caller captured,
  erase_environment (capture_environment caller captured) =
  capture_environment (erase_environment caller) (erase_environment captured).
Proof.
  intros [cm ca cp] [dm da dp]. unfold capture_environment.
  rewrite !erase_environment_eq. reflexivity.
Qed.
Lemma erase_call_environment : forall caller substitution captured,
  erase_environment (call_environment caller substitution captured) =
  call_environment (erase_environment caller) substitution (erase_environment captured).
Proof.
  intros caller substitution captured. unfold call_environment.
  rewrite erase_append, erase_instantiate. reflexivity.
Qed.
Lemma erase_application_environment : forall P caller body substitution captured,
  erase_environment (application_environment P caller body substitution captured) =
  application_environment P (erase_environment caller) body substitution (erase_environment captured).
Proof.
  intros P [cm ca cp] body substitution [dm da dp]. unfold application_environment.
  rewrite !erase_environment_eq. reflexivity.
Qed.

Lemma lookup_erased_entry : forall entries key,
  lookup_entry key (erase_entries entries) = option_map erase_entry (lookup_entry key entries).
Proof.
  intros entries key. induction entries as [|[source entry] tail IH]; cbn [erase_entries lookup_entry map fst snd]; [reflexivity|].
  destruct (item_eqb key source); [reflexivity|exact IH].
Qed.
Lemma lookup_erased_environment : forall d key,
  lookup_provision key (erase_environment d) = option_map erase_entry (lookup_provision key d).
Proof.
  intros [meanings aliases entries] key. rewrite erase_environment_eq.
  apply lookup_erased_entry.
Qed.
Lemma erase_selection : forall d selection,
  select_provisions (erase_environment d) selection = option_map erase_environment (select_provisions d selection).
Proof.
  intros d selection. induction selection as [|[required source] tail IH]; [reflexivity|].
  cbn [select_provisions]. rewrite lookup_erased_environment, IH.
  destruct (lookup_provision source d), (select_provisions d tail); cbn [option_map]; [|reflexivity..].
  rewrite erase_install. reflexivity.
Qed.
Lemma erase_adaptation : forall d adaptation,
  adapt_provisions (erase_environment d) adaptation = option_map erase_environment (adapt_provisions d adaptation).
Proof. intros d [selection|]; [apply erase_selection|reflexivity]. Qed.

Lemma erased_read_result_types : forall outputs d results,
  read_result_types (erase_environment d) results outputs = read_result_types d results outputs.
Proof.
  induction outputs as [|[source destination] tail IH]; intros d results; [reflexivity|].
  cbn [read_result_types]. rewrite erased_output_meaning, IH. reflexivity.
Qed.
Lemma erase_result_environment : forall outputs body results returned,
  result_environment outputs (erase_environment body) results (erase_environment returned) =
  option_map erase_environment (result_environment outputs body results returned).
Proof.
  intros outputs body results [meanings aliases entries]. unfold result_environment.
  rewrite erased_read_result_types. destruct (read_result_types body results outputs) as [[writes aliases']|]; [|reflexivity].
  cbn [option_map]. rewrite !erase_environment_eq. reflexivity.
Qed.

Lemma base_call_site_irrelevant : forall S
    (primitive : program_view -> prim -> list value -> S -> signal * S)
    P operation first second ws st,
  (forall tau out, BaseCall.allocation_result operation tau out = out) ->
  BaseCall.call primitive P operation first ws st =
  BaseCall.call primitive P operation second ws st.
Proof.
  intros S primitive P operation first second ws st Plain. unfold BaseCall.call.
  destruct (primitive P operation (BaseCall.primitive_arguments operation ws) st)
    as [out next]. destruct first, second; rewrite ?Plain; reflexivity.
Qed.

Lemma erased_primitive_call : forall S
    (primitive : program_view -> prim -> list value -> S -> signal * S)
    P operation site ws st,
  BaseCall.call primitive P operation site ws st =
  BaseCall.call primitive P operation (retained_site operation site) ws st.
Proof.
  intros S primitive P operation site ws st. destruct operation; cbn [retained_site]; try reflexivity;
    apply base_call_site_irrelevant; intros tau out; reflexivity.
Qed.

Fixpoint erased_entry_primitives (entry : provision_value) :
  entry_primitives (erase_entry entry) = entry_primitives entry.
Proof.
  destruct entry as [value|function captured|body adaptation captured outputs|operation site];
    cbn [erase_entry entry_primitives]; try reflexivity.
  all: destruct captured as [meanings aliases entries]; rewrite erase_environment_eq;
    cbn [entry_primitives]; unfold erase_entries;
    induction entries as [|[key entry] tail IH]; cbn in *; [reflexivity|];
    rewrite erased_entry_primitives, IH; reflexivity.
Qed.

Definition environments_agree (left right : environment) :=
  erase_environment left = erase_environment right.

Inductive entries_agree : provision_value -> provision_value -> Prop :=
| AgreeValue : forall value, entries_agree (ValueProvision value) (ValueProvision value)
| AgreeFunction : forall function left right, environments_agree left right ->
    entries_agree (FunctionClosure function left) (FunctionClosure function right)
| AgreeFunctor : forall body adaptation left right outputs, environments_agree left right ->
    entries_agree (FunctorClosure body adaptation left outputs) (FunctorClosure body adaptation right outputs)
| AgreePrimitive : forall operation left right, retained_site operation left = retained_site operation right ->
    entries_agree (PrimitiveClosure operation left) (PrimitiveClosure operation right).

Lemma entries_agree_of_erasure : forall left right,
  erase_entry left = erase_entry right -> entries_agree left right.
Proof.
  intros left right Eq. destruct left, right; cbn [erase_entry] in Eq; try discriminate;
    injection Eq as Eq; repeat match goal with E : _ = _ |- _ => first [subst | progress injection E as E] end;
    constructor; assumption.
Qed.

Lemma agreement_ground : forall left right, environments_agree left right ->
  forall tau, ground left tau = ground right tau.
Proof.
  intros left right Eq tau. rewrite <- (erased_ground left), <- (erased_ground right).
  unfold environments_agree in Eq. rewrite Eq. reflexivity.
Qed.
Lemma agreement_lookup : forall left right key entry,
  environments_agree left right -> lookup_provision key left = Some entry ->
  exists other, lookup_provision key right = Some other /\ entries_agree entry other.
Proof.
  intros left right key entry Eq Found.
  assert (E : option_map erase_entry (lookup_provision key left) =
    option_map erase_entry (lookup_provision key right)).
  { rewrite <- !lookup_erased_environment. unfold environments_agree in Eq. rewrite Eq. reflexivity. }
  rewrite Found in E. destruct (lookup_provision key right) as [other|] eqn:Other; [|discriminate].
  exists other. split; [reflexivity|]. apply entries_agree_of_erasure. injection E as E. exact E.
Qed.
Lemma agreement_none : forall left right key,
  environments_agree left right -> lookup_provision key left = None -> lookup_provision key right = None.
Proof.
  intros left right key Eq Found.
  assert (E : option_map erase_entry (lookup_provision key left) =
    option_map erase_entry (lookup_provision key right)).
  { rewrite <- !lookup_erased_environment. unfold environments_agree in Eq. rewrite Eq. reflexivity. }
  rewrite Found in E. destruct (lookup_provision key right); [discriminate|reflexivity].
Qed.

Definition optional_environments_agree (left right : option environment) : Prop :=
  match left, right with
  | Some d, Some d' => environments_agree d d'
  | None, None => True
  | _, _ => False
  end.
Lemma agreement_select : forall left right selection,
  environments_agree left right ->
  optional_environments_agree (select_provisions left selection) (select_provisions right selection).
Proof.
  intros left right selection Eq.
  assert (E : option_map erase_environment (select_provisions left selection) =
    option_map erase_environment (select_provisions right selection)).
  { rewrite <- !erase_selection. unfold environments_agree in Eq. rewrite Eq. reflexivity. }
  destruct (select_provisions left selection), (select_provisions right selection); cbn in *;
    try discriminate; [injection E as E; exact E|exact I].
Qed.
Lemma agreement_adapt : forall left right adaptation,
  environments_agree left right ->
  optional_environments_agree (adapt_provisions left adaptation) (adapt_provisions right adaptation).
Proof. intros left right [selection|] Eq; [apply agreement_select|exact Eq]; exact Eq. Qed.

Lemma agreement_install : forall key entry other left right,
  entries_agree entry other -> environments_agree left right ->
  environments_agree (install_provision key entry left) (install_provision key other right).
Proof.
  intros key entry other left right Entries Envs. unfold environments_agree in *.
  rewrite !erase_install, Envs. inversion Entries; subst; cbn [erase_entry];
    unfold environments_agree in *; try congruence.
Qed.
Lemma agreement_append : forall left right captured other,
  environments_agree left right -> environments_agree captured other ->
  environments_agree (append_environment left captured) (append_environment right other).
Proof. intros left right captured other E C. unfold environments_agree in *. rewrite !erase_append, E, C. reflexivity. Qed.
Lemma agreement_capture : forall left right capture captured other,
  environments_agree left right -> environments_agree captured other ->
  environments_agree (typed_capture left capture captured) (typed_capture right capture other).
Proof. intros left right capture captured other E C. unfold environments_agree in *. rewrite !erase_typed_capture, E, C. reflexivity. Qed.
Lemma agreement_call : forall left right substitution captured other,
  environments_agree left right -> environments_agree captured other ->
  environments_agree (call_environment left substitution captured) (call_environment right substitution other).
Proof. intros left right substitution captured other E C. unfold environments_agree in *. rewrite !erase_call_environment, E, C. reflexivity. Qed.
Lemma agreement_application : forall P left right body substitution captured other,
  environments_agree left right -> environments_agree captured other ->
  environments_agree (application_environment P left body substitution captured)
    (application_environment P right body substitution other).
Proof. intros P left right body substitution captured other E C. unfold environments_agree in *. rewrite !erase_application_environment, E, C. reflexivity. Qed.
Lemma agreement_bind_type : forall left right symbol meaning,
  environments_agree left right -> environments_agree (bind_type left symbol meaning) (bind_type right symbol meaning).
Proof. intros left right symbol meaning E. unfold environments_agree in *. rewrite !erase_bind_type, E. reflexivity. Qed.
Lemma agreement_primitive : forall left right key operation,
  environments_agree left right -> entries_agree (primitive_closure left key operation) (primitive_closure right key operation).
Proof.
  intros left right key operation E. unfold primitive_closure.
  destruct operation; destruct key; constructor; cbn [retained_site];
    try reflexivity; rewrite (agreement_ground left right E); reflexivity.
Qed.
Lemma agreement_result : forall outputs left right results returned other,
  environments_agree left right -> environments_agree returned other ->
  optional_environments_agree (result_environment outputs left results returned)
    (result_environment outputs right results other).
Proof.
  intros outputs left right results returned other E C.
  assert (Eq : option_map erase_environment (result_environment outputs left results returned) =
    option_map erase_environment (result_environment outputs right results other)).
  { rewrite <- !erase_result_environment. unfold environments_agree in *. rewrite E, C. reflexivity. }
  destruct (result_environment outputs left results returned), (result_environment outputs right results other);
    cbn in *; try discriminate; [injection Eq as Eq; exact Eq|exact I].
Qed.
(* Canonical entries used by the primitive-only platform-family fast path. *)
Definition nonallocating (operation : prim) : bool :=
  match operation with PBox _ | PArrNew _ => false | _ => true end.
Definition primitive_binding (binding : binding) : bool :=
  match binding with BPrimitive _ operation => nonallocating operation | _ => false end.
Definition canonical_binding (binding : binding) : item * provision_value :=
  match binding with
  | BPrimitive key operation => (key, PrimitiveClosure operation None)
  | _ => (ITy 0, ValueProvision VUnit)
  end.
Definition canonical_installs (d : environment) (bindings : list binding) : environment :=
  EnvironmentWithAliases (type_meanings d) (type_aliases d)
    (rev (map canonical_binding bindings) ++ provision_values d).

Lemma erase_nonallocating_closure : forall d key operation,
  nonallocating operation = true ->
  erase_entry (primitive_closure d key operation) = PrimitiveClosure operation None.
Proof.
  intros d key operation Plain. destruct operation; cbn in Plain; try discriminate;
    destruct key; reflexivity.
Qed.
Lemma canonical_installs_erased_step : forall d key operation bindings,
  nonallocating operation = true ->
  environments_agree
    (canonical_installs (install_provision key (primitive_closure d key operation) d) bindings)
    (canonical_installs d (BPrimitive key operation :: bindings)).
Proof.
  intros [meanings aliases entries] key operation bindings Plain.
  unfold environments_agree, canonical_installs, install_provision.
  rewrite !erase_environment_eq. cbn [type_meanings type_aliases provision_values map rev canonical_binding].
  rewrite <- app_assoc. unfold erase_entries. rewrite !map_app.
  cbn [map fst snd]. rewrite erase_nonallocating_closure by exact Plain.
  cbn [erase_entry]. destruct operation; reflexivity.
Qed.
End BaseSites.

Module SiteSimulation (Prims : PRIMITIVES) (I : INTERP Prims).
Import Prims I BaseSites.
Section Execution.
Variable P : program_view.
Hypothesis CallSites : forall operation site ws st,
  run_prim P operation site ws st =
  run_prim P operation (retained_site operation site) ws st.

Lemma primitive_results_agree : forall operation first second ws st,
  retained_site operation first = retained_site operation second ->
  run_prim_at P operation first ws st = run_prim_at P operation second ws st.
Proof.
  intros operation first second ws st Eq. unfold run_prim_at.
  rewrite (CallSites operation first), (CallSites operation second), Eq. reflexivity.
Qed.
Lemma check_tags_agree : forall left right tag substitution value,
  environments_agree left right ->
  check_tag P left tag substitution value = check_tag P right tag substitution value.
Proof.
  intros left right tag substitution value Eq. unfold check_tag.
  destruct (lookup_cast tag (view_casts P)) as [spec|]; [|reflexivity].
  destruct (cast_handle spec) as [[handle expected]|];
    repeat rewrite (agreement_ground left right Eq);
    destruct (lookup_subst (cast_held spec) substitution); destruct value;
    repeat rewrite (agreement_ground left right Eq); reflexivity.
Qed.

Definition bindings_agree (left right : (environment * locals * mstate) + evaluation) : Prop :=
  match left, right with
  | inl (d, g, st), inl (d', g', st') => environments_agree d d' /\ g = g' /\ st = st'
  | inr result, inr other => result = other
  | _, _ => False
  end.
Lemma bindings_agree_transitive : forall first middle last,
  bindings_agree first middle -> bindings_agree middle last -> bindings_agree first last.
Proof.
  intros [[ [first fg] fs]|first] [[[middle mg] ms]|middle] [[[last lg] ls]|last];
    cbn [bindings_agree]; intros First Last; try contradiction.
  - destruct First as [First [-> ->]], Last as [Last [-> ->]].
    split; [|split; reflexivity]. unfold environments_agree in *. congruence.
  - congruence.
Qed.
Definition applications_agree (left right : (environment * mstate) + evaluation) : Prop :=
  match left, right with
  | inl (d, st), inl (d', st') => environments_agree d d' /\ st = st'
  | inr result, inr other => result = other
  | _, _ => False
  end.
(* The primitive-only body computes a canonical environment up to erased
   sites. No callback law is needed here: binding primitives never calls it. *)
Theorem primitive_bindings_normalized : forall bindings fuel P0 d g st,
  forallb primitive_binding bindings = true ->
  bindings_agree (eval_bindings fuel P0 d g st bindings)
    (if Nat.ltb (length bindings) fuel then inl (canonical_installs d bindings, g, st)
     else inr (Evaluation OutOfFuel g st)).
Proof.
  induction bindings as [|binding tail IH]; intros [|fuel] P0 d g st Plain; cbn [eval_bindings];
    try reflexivity.
  - cbn [Nat.ltb canonical_installs rev map app bindings_agree].
    repeat split; try reflexivity. unfold environments_agree. destruct d; reflexivity.
  - destruct binding; cbn [primitive_binding] in Plain; try discriminate.
    apply Bool.andb_true_iff in Plain as [Operation Plain].
    specialize (IH fuel P0 (install_provision i (primitive_closure d i p) d) g st Plain).
    change (bindings_agree
      (eval_bindings fuel P0 (install_provision i (primitive_closure d i p) d) g st tail)
      (if Nat.ltb (length tail) fuel then inl (canonical_installs d (BPrimitive i p :: tail), g, st)
       else inr (Evaluation OutOfFuel g st))).
    destruct (Nat.ltb (length tail) fuel) eqn:Enough;
      destruct (eval_bindings fuel P0 (install_provision i (primitive_closure d i p) d) g st tail)
        as [[[returned locals] next]|result]; cbn [bindings_agree] in IH |- *; try contradiction.
    + destruct IH as [Environments [-> ->]]. repeat split; try reflexivity.
      unfold environments_agree in *. etransitivity; [exact Environments|].
      apply canonical_installs_erased_step. exact Operation.
    + exact IH.
Qed.

Definition execution_agrees (fuel : nat) : Prop :=
  (forall d d' g st e, environments_agree d d' -> eval fuel P d g st e = eval fuel P d' g st e) /\
  (forall d d' g st args, environments_agree d d' -> eval_arguments fuel P d g st args = eval_arguments fuel P d' g st args) /\
  (forall d d' g st bindings, environments_agree d d' ->
    bindings_agree (eval_bindings fuel P d g st bindings) (eval_bindings fuel P d' g st bindings)) /\
  (forall d d' functor substitution outputs selection results st, environments_agree d d' ->
    applications_agree (apply_functor fuel P d functor substitution outputs selection results st)
      (apply_functor fuel P d' functor substitution outputs selection results st)).

Theorem execution_site_erasure : forall fuel, execution_agrees fuel.
Proof.
  induction fuel as [|fuel [IHe [IHargs [IHbindings IHapply]]]].
  { repeat split; intros; reflexivity. }
  unfold execution_agrees. repeat split.
  - intros d d' g st e Eq. destruct e; cbn [eval].
    + reflexivity.
    + reflexivity.
    + destruct (lookup_provision (IVal v) d) as [entry|] eqn:Found.
      * destruct (agreement_lookup _ _ _ _ Eq Found) as [other [Other Rel]]. rewrite Other.
        inversion Rel; reflexivity.
      * rewrite (agreement_none _ _ _ Eq Found). reflexivity.
    + rewrite (IHe d d' g st e Eq). destruct (eval fuel P d' g st e) as [signal g1 st1].
      destruct signal; try reflexivity. rewrite (check_tags_agree d d' _ _ _ Eq). reflexivity.
    + destruct (view_function P f) as [body|]; [|reflexivity].
      rewrite (IHargs d d' g st l Eq).
      destruct (eval_arguments fuel P d' g st l) as [[[values g1] st1]|]; [|reflexivity].
      pose proof (agreement_select d d' s0 Eq) as Selected.
      destruct (select_provisions d s0), (select_provisions d' s0); cbn in Selected; try contradiction; [|reflexivity].
      rewrite (IHe _ _ _ _ body (agreement_call _ _ _ _ _ Eq Selected)). reflexivity.
    + destruct (lookup_provision (IFn f) d) as [entry|] eqn:Found.
      2:{ rewrite (agreement_none _ _ _ Eq Found). reflexivity. }
      destruct (agreement_lookup _ _ _ _ Eq Found) as [other [Other Rel]]. rewrite Other.
      inversion Rel; subst; try reflexivity.
      * destruct (view_function P function) as [body|]; [|reflexivity].
        rewrite (IHargs d d' g st l Eq). destruct (eval_arguments fuel P d' g st l) as [[[values g1] st1]|]; [|reflexivity].
        rewrite (IHe _ _ _ _ body H). reflexivity.
      * rewrite (IHargs d d' g st l Eq). destruct (eval_arguments fuel P d' g st l) as [[[values g1] st1]|]; [|reflexivity].
        rewrite (primitive_results_agree _ _ _ _ _ H). reflexivity.
    + destruct (lookup_provision i d) as [entry|] eqn:Found.
      2:{ rewrite (agreement_none _ _ _ Eq Found). reflexivity. }
      destruct (agreement_lookup _ _ _ _ Eq Found) as [other [Other Rel]]. rewrite Other.
      inversion Rel; subst; try reflexivity.
      * destruct (view_function P function) as [body|]; [|reflexivity].
        rewrite (IHe d d' g st e Eq). destruct (eval fuel P d' g st e) as [signal g0 st0].
        destruct signal; try reflexivity. rewrite (IHargs d d' g0 st0 l Eq).
        destruct (eval_arguments fuel P d' g0 st0 l) as [[[values g1] st1]|]; [|reflexivity].
        rewrite (IHe _ _ _ _ body H). reflexivity.
      * rewrite (IHe d d' g st e Eq). destruct (eval fuel P d' g st e) as [signal g0 st0].
        destruct signal; try reflexivity. rewrite (IHargs d d' g0 st0 l Eq).
        destruct (eval_arguments fuel P d' g0 st0 l) as [[[values g1] st1]|]; [|reflexivity].
        rewrite (primitive_results_agree _ _ _ _ _ H). reflexivity.
    + rewrite (IHe d d' g st e1 Eq). destruct (eval fuel P d' g st e1) as [signal g1 st1].
      destruct signal; try reflexivity. rewrite (IHe d d' _ _ e2 Eq). reflexivity.
    + pose proof (IHbindings d d' g st [b] Eq) as Bound.
      destruct (eval_bindings fuel P d g st [b]) as [[[d1 g1] st1]|result],
        (eval_bindings fuel P d' g st [b]) as [[[d1' g1'] st1']|other]; cbn in Bound; try contradiction.
      * destruct Bound as [Envs [-> ->]]. apply IHe. exact Envs.
      * exact Bound.
    + rewrite (IHe d d' g st e1 Eq). destruct (eval fuel P d' g st e1) as [signal g1 st1].
      destruct signal; try reflexivity. apply IHe. exact Eq.
    + rewrite (IHe d d' g st e Eq). reflexivity.
    + rewrite (IHe d d' g st e1 Eq). destruct (eval fuel P d' g st e1) as [signal g1 st1].
      destruct signal; try reflexivity. destruct (truthy P v) as [[]|]; try reflexivity; apply IHe; exact Eq.
    + rewrite (IHe d d' g st e1 Eq). destruct (eval fuel P d' g st e1) as [signal g1 st1].
      destruct signal; try reflexivity. destruct (truthy P v) as [[]|]; try reflexivity.
      rewrite (IHe d d' g1 st1 e2 Eq). destruct (eval fuel P d' g1 st1 e2) as [signal g2 st2].
      destruct signal; try reflexivity. apply IHe. exact Eq.
    + rewrite (IHe d d' g st e Eq). destruct (eval fuel P d' g st e) as [signal g1 st1].
      destruct signal; try reflexivity. apply IHe. exact Eq.
    + rewrite (IHe d d' g st e Eq). reflexivity.
    + reflexivity.
    + rewrite (IHargs d d' g st (map snd l) Eq). reflexivity.
    + rewrite (IHe d d' g st e Eq). reflexivity.
    + rewrite (IHe d d' g st e Eq). destruct (eval fuel P d' g st e) as [signal g1 st1].
      destruct signal; try reflexivity. destruct (select_arm l v) as [[bindings body]|]; [|reflexivity].
      rewrite (IHe d d' _ _ body Eq). reflexivity.
  - intros d d' g st args Eq. destruct args as [|e tail]; cbn [eval_arguments]; [reflexivity|].
    rewrite (IHe d d' g st e Eq). destruct (eval fuel P d' g st e) as [signal g1 st1].
    destruct signal; try reflexivity. rewrite (IHargs d d' g1 st1 tail Eq). reflexivity.
  - intros d d' g st bindings Eq. destruct bindings as [|binding tail]; cbn [eval_bindings].
    { repeat split; try reflexivity. exact Eq. }
    destruct binding; cbn [eval_bindings].
    + apply IHbindings. apply agreement_bind_type. exact Eq.
    + rewrite (IHe d d' g st e Eq). destruct (eval fuel P d' g st e) as [signal g1 st1].
      destruct signal; try reflexivity. apply IHbindings. apply agreement_install; [constructor|exact Eq].
    + pose proof (agreement_select d d' s Eq) as Selected.
      destruct (select_provisions d s), (select_provisions d' s); cbn in Selected; try contradiction; try reflexivity.
      apply IHbindings. apply agreement_install; [constructor; apply agreement_capture; assumption|exact Eq].
    + pose proof (agreement_select d d' s0 Eq) as Selected.
      destruct (select_provisions d s0), (select_provisions d' s0); cbn in Selected; try contradiction; try reflexivity.
      apply IHbindings. apply agreement_install; [constructor; apply agreement_capture; assumption|exact Eq].
    + pose proof (agreement_select d d' s0 Eq) as Selected.
      destruct (select_provisions d s0), (select_provisions d' s0); cbn in Selected; try contradiction; try reflexivity.
      apply IHbindings. apply agreement_install; [constructor; apply agreement_capture; assumption|exact Eq].
    + destruct (lookup_provision (IFun u0) d) as [entry|] eqn:Found.
      2:{ rewrite (agreement_none _ _ _ Eq Found). reflexivity. }
      destruct (agreement_lookup _ _ _ _ Eq Found) as [other [Other Rel]]. rewrite Other.
      inversion Rel; subst; try reflexivity.
      apply IHbindings. apply agreement_install; [constructor; assumption|exact Eq].
    + pose proof (IHapply d d' u s l s0 l0 st Eq) as Applied.
      destruct (apply_functor fuel P d u s l s0 l0 st) as [[returned st1]|result],
        (apply_functor fuel P d' u s l s0 l0 st) as [[other st1']|result']; cbn in Applied; try contradiction.
      * destruct Applied as [Envs ->]. apply IHbindings. apply agreement_append; assumption.
      * subst. reflexivity.
    + apply IHbindings. apply agreement_install; [apply agreement_primitive|]; exact Eq.
  - intros d d' functor substitution outputs selection results st Eq. cbn [apply_functor].
    destruct (lookup_provision (IFun functor) d) as [entry|] eqn:Found.
    2:{ rewrite (agreement_none _ _ _ Eq Found). reflexivity. }
    destruct (agreement_lookup _ _ _ _ Eq Found) as [other [Other Rel]]. rewrite Other.
    inversion Rel; subst; try reflexivity.
    destruct (view_functor P body) as [code|]; [|reflexivity].
    pose proof (agreement_select d d' selection Eq) as Selected.
    destruct (select_provisions d selection), (select_provisions d' selection); cbn in Selected; try contradiction; try reflexivity.
    pose proof (agreement_adapt _ _ adaptation Selected) as Adapted.
    destruct (adapt_provisions e adaptation), (adapt_provisions e0 adaptation); cbn in Adapted; try contradiction; try reflexivity.
    pose proof (IHbindings _ _ [] st (fc_bindings code)
      (agreement_application P d d' body substitution _ _ Eq (agreement_append _ _ _ _ Adapted H))) as Body.
    destruct (eval_bindings fuel P (application_environment P d body substitution (append_environment e1 left)) [] st (fc_bindings code)) as [[[bodyenv ignored] st1]|result],
      (eval_bindings fuel P (application_environment P d' body substitution (append_environment e2 right)) [] st (fc_bindings code)) as [[[bodyenv' ignored'] st1']|result'];
      cbn in Body; try contradiction.
    + destruct Body as [Envs [-> ->]].
      pose proof (agreement_select bodyenv bodyenv' (result_selection results outputs0 (fc_sources code)) Envs) as Returned.
      destruct (select_provisions bodyenv (result_selection results outputs0 (fc_sources code))),
        (select_provisions bodyenv' (result_selection results outputs0 (fc_sources code))); cbn in Returned; try contradiction; try reflexivity.
      pose proof (agreement_result outputs bodyenv bodyenv' (fc_outputs code) e3 e4 Envs Returned) as Result.
      destruct (result_environment outputs bodyenv (fc_outputs code) e3),
        (result_environment outputs bodyenv' (fc_outputs code) e4); cbn in Result; try contradiction; try reflexivity.
      split; [exact Result|reflexivity].
    + subst. reflexivity.
Qed.
Lemma primitive_bind_step : forall n d g st key operation e,
  2 <= n ->
  eval (S n) P d g st (EBind (BPrimitive key operation) e) =
  eval n P (install_provision key (primitive_closure d key operation) d) g st e.
Proof. intros [|[|n]] d g st key operation e Large; try lia; reflexivity. Qed.

Theorem primitive_chain_normalized : forall bindings continuation fuel d g st,
  forallb primitive_binding bindings = true -> 2 <= fuel ->
  eval (length bindings + fuel) P d g st (fold_right EBind continuation bindings) =
  eval fuel P (canonical_installs d bindings) g st continuation.
Proof.
  induction bindings as [|binding tail IH]; intros continuation fuel d g st Plain Large.
  - cbn [List.length Nat.add fold_right canonical_installs rev map app]. destruct d; reflexivity.
  - destruct binding; cbn [primitive_binding] in Plain; try discriminate.
    apply Bool.andb_true_iff in Plain as [Operation Plain].
    cbn [List.length Nat.add fold_right]. rewrite primitive_bind_step by lia.
    rewrite IH by assumption. apply (proj1 (execution_site_erasure fuel)).
    apply canonical_installs_erased_step. exact Operation.
Qed.

Theorem primitive_prefix_normalized : forall bindings continuation fuel d g st,
  forallb primitive_binding bindings = true ->
  bindings_agree
    (eval_bindings (length bindings + fuel) P d g st (bindings ++ continuation))
    (eval_bindings fuel P (canonical_installs d bindings) g st continuation).
Proof.
  induction bindings as [|binding tail IH]; intros continuation fuel d g st Plain.
  - cbn [List.length Nat.add app canonical_installs rev map]. destruct d as [meanings aliases entries].
    pose proof (proj1 (proj2 (proj2 (execution_site_erasure fuel)))
      (EnvironmentWithAliases meanings aliases entries) (EnvironmentWithAliases meanings aliases entries) g st continuation eq_refl) as Same.
    exact Same.
  - destruct binding; cbn [primitive_binding] in Plain; try discriminate.
    apply Bool.andb_true_iff in Plain as [Operation Plain].
    cbn [List.length Nat.add app eval_bindings].
    eapply bindings_agree_transitive; [apply IH; exact Plain|].
    apply (proj1 (proj2 (proj2 (execution_site_erasure fuel)))).
    apply canonical_installs_erased_step. exact Operation.
Qed.

Corollary eval_erased : forall fuel d g st e,
  eval fuel P d g st e = eval fuel P (erase_environment d) g st e.
Proof.
  intros fuel d g st e. apply (proj1 (execution_site_erasure fuel)).
  unfold environments_agree. symmetry. apply erase_environment_idempotent.
Qed.

Corollary run_main_erased : forall fuel main d st,
  run_main fuel P main d st = run_main fuel P main (erase_environment d) st.
Proof.
  intros fuel main d st. unfold run_main. destruct (view_function P main) as [body|];
    [rewrite eval_erased|]; reflexivity.
Qed.
End Execution.
End SiteSimulation.

(* The site law holds for every public interface, including interfaces with
   unusual success, trap, missing-operation, or state-transition behavior.
   Its only premise is the explicit BaseCall adapter used by ReferenceHost. *)
Module ReferenceSites (Host : HOST) (M : PUBLIC Host).
Module Simulation := SiteSimulation M.ReferenceHost M.Reference.

Lemma calls_ignore_nonallocating_sites : forall P operation site ws st,
  M.ReferenceHost.run_prim P operation site ws st =
  M.ReferenceHost.run_prim P operation (BaseSites.retained_site operation site) ws st.
Proof.
  intros P operation site ws st. unfold M.ReferenceHost.run_prim.
  apply BaseSites.erased_primitive_call.
Qed.

Theorem reference_site_erasure : forall fuel P main d st,
  M.Reference.run_main fuel P main d st =
  M.Reference.run_main fuel P main (BaseSites.erase_environment d) st.
Proof.
  intros fuel P main d st. apply Simulation.run_main_erased.
  apply calls_ignore_nonallocating_sites.
Qed.
End ReferenceSites.
