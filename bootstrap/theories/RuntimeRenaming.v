(* Renaming generated symbols preserves runtime lookup and typed casts.
   Local positions, heap addresses, scalar bits and diagnostic codes
   keep their values. A concrete renaming fixes source and platform
   symbols in its support. Primitive operations keep their platform data,
   including diagnostic indices. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit BaseFacts PublicValueTyping.
From NM Require PublicCastTyping.
Import Audit.Semantics ListNotations.

Section Renaming.
Variable rename : nat -> nat.

Fixpoint rename_ty (tau : ty) : ty :=
  match tau with
  | TMachine m => TMachine (rename m)
  | TRecord fields => TRecord (map (fun '(name, t) => (name, rename_ty t)) fields)
  | TUnit => TUnit
  | TAbs t => TAbs (rename t)
  | TNom name arguments => TNom (rename name) (map (fun '(t, u) => (rename t, rename_ty u)) arguments)
  | TUnion members => TUnion (map rename_ty members)
  end.

Definition rename_subst (theta : subst) : subst :=
  map (fun '(name, tau) => (rename name, rename_ty tau)) theta.

Definition rename_item (key : item) : item :=
  match key with
  | ITy t => ITy (rename t)
  | IVal v => IVal (rename v)
  | IFn f => IFn (rename f)
  | IMth receiver method arguments =>
      IMth (rename_ty receiver) (rename method) (rename_subst arguments)
  | IFun u => IFun (rename u)
  end.

Definition rename_smap (selection : smap) : smap :=
  map (fun '(requirement, provision) => (rename_item requirement, rename_item provision)) selection.

Definition rename_site (site : option ty) : option ty := option_map rename_ty site.

Fixpoint rename_value (w : value) : value :=
  match w with
  | VUnit => VUnit
  | VTag tag payload => VTag (rename tag) (rename_value payload)
  | VRecord fields => VRecord (map (fun '(name, field) => (name, rename_value field)) fields)
  | VScalar sort bits => VScalar sort bits
  | VRef address header =>
      VRef address (option_map (fun '(kind, held) => (kind, rename_ty held)) header)
  end.

Definition rename_locals (g : locals) : locals :=
  map (fun '(position, w) => (position, rename_value w)) g.

Fixpoint rename_provision (provided : provision_value) : provision_value :=
  match provided with
  | ValueProvision w => ValueProvision (rename_value w)
  | FunctionClosure f d => FunctionClosure (rename f) (rename_environment d)
  | FunctorClosure h adaptation d positions =>
      FunctorClosure (rename h) (option_map rename_smap adaptation) (rename_environment d) positions
  | PrimitiveClosure operation site => PrimitiveClosure operation (rename_site site)
  end
with rename_environment (d : environment) : environment :=
  match d with
  | EnvironmentWithAliases meanings aliases entries =>
      EnvironmentWithAliases (rename_subst meanings) (rename_subst aliases)
        (map (fun '(key, provided) => (rename_item key, rename_provision provided)) entries)
  end.

Definition rename_cast_spec (spec : cast_spec) : cast_spec :=
  CastSpec (cast_kind spec) (rename (cast_held spec))
    (option_map (fun '(handle, tau) => (rename handle, rename_ty tau)) (cast_handle spec)).

Definition rename_casts (casts : list (nsym * cast_spec)) : list (nsym * cast_spec) :=
  map (fun '(tag, spec) => (rename tag, rename_cast_spec spec)) casts.

Definition rename_pattern (pat : pattern) : pattern :=
  match pat with
  | PAny x => PAny x
  | PHead tag binder fields => PHead (rename tag) binder fields
  end.

Definition rename_capture (capture : type_capture) : type_capture :=
  TypeCapture (rename_subst (capture_generations capture)) (rename_subst (capture_aliases capture)).

Definition rename_outputs (outputs : list (tsym * tsym)) : list (tsym * tsym) :=
  map (fun '(source, destination) => (rename source, rename destination)) outputs.

Fixpoint rename_expr (e : expr) : expr :=
  match e with
  | EVar x => EVar x
  | EUnit => EUnit
  | EVal v => EVal (rename v)
  | ETag tag th body => ETag (rename tag) (rename_subst th) (rename_expr body)
  | ECall f th needs args => ECall (rename f) (rename_subst th) (rename_smap needs) (map rename_expr args)
  | EInvoke f args => EInvoke (rename f) (map rename_expr args)
  | EMethod receiver m key args =>
      EMethod (rename_expr receiver) (rename m) (rename_item key) (map rename_expr args)
  | ELet x value body => ELet x (rename_expr value) (rename_expr body)
  | EBind b body => EBind (rename_binding b) (rename_expr body)
  | ESeq first second => ESeq (rename_expr first) (rename_expr second)
  | EAssign x value => EAssign x (rename_expr value)
  | EIf cond yes no => EIf (rename_expr cond) (rename_expr yes) (rename_expr no)
  | EWhile cond body => EWhile (rename_expr cond) (rename_expr body)
  | ELoop body => ELoop (rename_expr body)
  | EReturn value => EReturn (rename_expr value)
  | EBreak => EBreak
  | ERecord fields => ERecord (map (fun '(name, field) => (name, rename_expr field)) fields)
  | EField receiver field => EField (rename_expr receiver) field
  | EMatch scrutinee arms =>
      EMatch (rename_expr scrutinee) (map (fun '(pat, body) => (rename_pattern pat, rename_expr body)) arms)
  end
with rename_binding (b : binding) : binding :=
  match b with
  | BType t tau => BType (rename t) (rename_ty tau)
  | BValue v value => BValue (rename v) (rename_expr value)
  | BFunction f body capture needs =>
      BFunction (rename f) (rename body) (rename_capture capture) (rename_smap needs)
  | BMethod receiver m th body capture needs =>
      BMethod (rename_ty receiver) (rename m) (rename_subst th) (rename body)
        (rename_capture capture) (rename_smap needs)
  | BFunctor u body capture linked needs =>
      BFunctor (rename u) (rename body) (rename_capture capture) (rename_smap linked) (rename_smap needs)
  | BCopyFunctor u provider needs positions =>
      BCopyFunctor (rename u) (rename provider) (rename_smap needs) positions
  | BApply u th outputs needs results =>
      BApply (rename u) (rename_subst th) (rename_outputs outputs)
        (rename_smap needs) (map rename_item results)
  | BPrimitive key operation => BPrimitive (rename_item key) operation
  end.

Definition rename_functor_code (code : functor_code) : functor_code :=
  FunctorCode (map rename (fc_inputs code)) (map rename (fc_linked code)) (rename_subst (fc_table code))
    (map rename_item (fc_outputs code)) (map rename_binding (fc_bindings code)) (map rename_item (fc_sources code)).

Definition rename_machines (machines : list (tsym * machine_class)) : list (tsym * machine_class) :=
  map (fun '(symbol, representation) => (rename symbol, representation)) machines.

Definition rename_program (p : program) : program :=
  Program (map (fun '(f, body) => (rename f, rename_expr body)) (program_functions p))
    (map (fun '(u, code) => (rename u, rename_functor_code code)) (program_functors p))
    (rename (program_bool p)) (rename (program_true p)) (rename (program_false p))
    (rename (program_main p)) (rename_environment (program_environment p)) (program_unsupported p)
    (rename_casts (program_casts p)) (rename_machines (program_machines p)).

Record views_related (P P' : program_view) : Prop := ViewsRelated {
  related_functions : forall f, view_function P' (rename f) = option_map rename_expr (view_function P f);
  related_functors : forall u, view_functor P' (rename u) = option_map rename_functor_code (view_functor P u);
  related_bool : view_bool P' = rename (view_bool P);
  related_true : view_true P' = rename (view_true P);
  related_false : view_false P' = rename (view_false P);
  related_arguments : view_arguments P' = view_arguments P;
  related_read : view_read P' = view_read P;
  related_casts : view_casts P' = rename_casts (view_casts P);
  related_machines : view_machines P' = rename_machines (view_machines P)
}.

Definition rename_signal (result : signal) : signal :=
  match result with
  | Normal w => Normal (rename_value w)
  | Returning w => Returning (rename_value w)
  | other => other
  end.

Hypothesis injective : forall left right, rename left = rename right -> left = right.

Lemma rename_eqb : forall left right, Nat.eqb (rename left) (rename right) = Nat.eqb left right.
Proof.
  intros left right. destruct (Nat.eqb_spec left right) as [->|Different]; [apply Nat.eqb_refl|].
  apply Nat.eqb_neq. intro Equal. apply Different, injective. exact Equal.
Qed.

Lemma rename_ty_eqb : forall left right, ty_eqb (rename_ty left) (rename_ty right) = ty_eqb left right.
Proof.
  fix IH 1. intros left right. destruct left as [m|fields| |t|n arguments|members], right;
    cbn [rename_ty ty_eqb]; try reflexivity; try apply rename_eqb.
  - revert l. induction fields as [|[f tau] rest Tail]; intros [|[g tau'] rest']; cbn [map]; try reflexivity.
    now rewrite IH, Tail.
  - rewrite rename_eqb. f_equal. revert l. induction arguments as [|[k tau] rest Tail];
      intros [|[k' tau'] rest']; cbn [map fst snd]; try reflexivity.
    now rewrite rename_eqb, IH, Tail.
  - revert l. induction members as [|tau rest Tail]; intros [|tau' rest']; cbn [map]; try reflexivity.
    now rewrite IH, Tail.
Qed.

Lemma rename_subst_eqb : forall left right,
  subst_eqb (rename_subst left) (rename_subst right) = subst_eqb left right.
Proof.
  induction left as [|[t tau] rest IH]; intros [|[t' tau'] rest']; cbn [rename_subst map subst_eqb fst snd]; try reflexivity.
  now rewrite rename_eqb, rename_ty_eqb, IH.
Qed.

Lemma rename_item_eqb : forall left right,
  item_eqb (rename_item left) (rename_item right) = item_eqb left right.
Proof.
  intros left right. destruct left, right; cbn [item_eqb rename_item]; try reflexivity; try apply rename_eqb.
  now rewrite rename_ty_eqb, rename_eqb, rename_subst_eqb.
Qed.

Lemma rename_lookup_subst : forall t theta,
  lookup_subst (rename t) (rename_subst theta) = option_map rename_ty (lookup_subst t theta).
Proof.
  intros t theta. induction theta as [|[k tau] rest IH]; [reflexivity|].
  cbn [rename_subst map lookup_subst]. rewrite rename_eqb.
  destruct (Nat.eqb t k); [reflexivity|exact IH].
Qed.

Lemma rename_app_subst : forall theta tau,
  app_subst (rename_subst theta) (rename_ty tau) = rename_ty (app_subst theta tau).
Proof.
  intros theta. fix IH 1. intros tau. destruct tau as [m|fields| |t|n arguments|members];
    cbn [rename_ty app_subst]; try reflexivity.
  - f_equal. induction fields as [|[name t] rest Tail]; [reflexivity|].
    cbn [map]. now rewrite IH, Tail.
  - rewrite rename_lookup_subst. destruct (lookup_subst t theta); reflexivity.
  - f_equal. induction arguments as [|[name t] rest Tail]; [reflexivity|].
    cbn [map fst snd]. now rewrite IH, Tail.
  - f_equal. induction members as [|t rest Tail]; [reflexivity|].
    cbn [map]. now rewrite IH, Tail.
Qed.

Lemma rename_ground : forall d tau,
  ground (rename_environment d) (rename_ty tau) = rename_ty (ground d tau).
Proof. intros [meanings aliases entries] tau. apply rename_app_subst. Qed.

Lemma rename_lookup_local : forall x g,
  lookup_local x (rename_locals g) = option_map rename_value (lookup_local x g).
Proof.
  intros x g. induction g as [|[position w] rest IH]; [reflexivity|].
  cbn [rename_locals map lookup_local]. destruct (Nat.eqb x position); [reflexivity|exact IH].
Qed.

Lemma rename_lookup_entry : forall key entries,
  lookup_entry (rename_item key)
    (map (fun '(source, provided) => (rename_item source, rename_provision provided)) entries) =
  option_map rename_provision (lookup_entry key entries).
Proof.
  intros key entries. induction entries as [|[source provided] rest IH]; [reflexivity|].
  cbn [map lookup_entry]. rewrite rename_item_eqb.
  destruct (item_eqb key source); [reflexivity|exact IH].
Qed.

Lemma rename_lookup_provision : forall key d,
  lookup_provision (rename_item key) (rename_environment d) =
  option_map rename_provision (lookup_provision key d).
Proof. intros key [meanings aliases entries]. apply rename_lookup_entry. Qed.

Lemma rename_lookup_cast : forall tag casts,
  lookup_cast (rename tag) (rename_casts casts) = option_map rename_cast_spec (lookup_cast tag casts).
Proof.
  intros tag casts. induction casts as [|[key spec] rest IH]; [reflexivity|].
  cbn [rename_casts map lookup_cast]. rewrite rename_eqb.
  destruct (Nat.eqb tag key); [reflexivity|exact IH].
Qed.

Lemma rename_nlook : forall A B (action : A -> B) key rows,
  Elaboration.nlook (rename key) (map (fun '(symbol, value) => (rename symbol, action value)) rows) =
  option_map action (Elaboration.nlook key rows).
Proof.
  intros A B action key rows. induction rows as [|[symbol value] rest IH]; [reflexivity|].
  cbn [map Elaboration.nlook]. rewrite rename_eqb.
  destruct (Nat.eqb key symbol); [reflexivity|exact IH].
Qed.

Theorem program_view_equivariant : forall p args read,
  views_related (program_of p args read) (program_of (rename_program p) args read).
Proof.
  intros p args read. constructor; try reflexivity; intros; apply rename_nlook.
Qed.
End Renaming.

Module ExecutionRenaming (Prims : PRIMITIVES) (I : INTERP Prims).
Section Equivariance.
Variable rename : nat -> nat.
Hypothesis injective : forall left right, rename left = rename right -> left = right.
Variable state_related : Prims.mstate -> Prims.mstate -> Prop.

Definition evaluations_related (left right : I.evaluation) : Prop :=
  I.evaluation_signal right = rename_signal rename (I.evaluation_signal left) /\
  I.evaluation_locals right = rename_locals rename (I.evaluation_locals left) /\
  state_related (I.evaluation_state left) (I.evaluation_state right).

Definition arguments_related
    (left right : (list value * locals * Prims.mstate) + I.evaluation) : Prop :=
  match left, right with
  | inl (values, g, state), inl (values', g', state') =>
      values' = map (rename_value rename) values /\ g' = rename_locals rename g /\
      state_related state state'
  | inr result, inr result' => evaluations_related result result'
  | _, _ => False
  end.

(* A host may observe its operation, values or program view. Execution
   equivalence needs the following condition on those calls. *)
Definition primitives_related (P P' : program_view) : Prop :=
  forall operation site values left right,
    state_related left right ->
    let '(out, next) := Prims.run_prim P operation site values left in
    let '(out', next') := Prims.run_prim P' operation
      (rename_site rename site) (map (rename_value rename) values) right in
    out' = rename_signal rename out /\ state_related next next'.

Lemma rename_type_argument : forall arguments symbol,
  type_argument (rename_subst rename arguments) (rename symbol) =
  rename_ty rename (type_argument arguments symbol).
Proof.
  intros arguments symbol. unfold type_argument. rewrite rename_lookup_subst by exact injective.
  destruct (lookup_subst symbol arguments); reflexivity.
Qed.

Lemma rename_cast_guard : forall d schema arguments,
  PublicCastTyping.runtime_guard (rename_environment rename d)
    (rename_cast_spec rename schema) (rename_subst rename arguments) =
  PublicCastTyping.runtime_guard d schema arguments.
Proof.
  intros d [kind held [[symbol expected]|]] arguments; [|reflexivity].
  cbn [PublicCastTyping.runtime_guard rename_cast_spec cast_handle option_map].
  now rewrite rename_type_argument, rename_ground, rename_ty_eqb by exact injective.
Qed.

Theorem check_tag_equivariant : forall P P' d tag arguments value,
  view_casts P' = rename_casts rename (view_casts P) ->
  I.check_tag P' (rename_environment rename d) (rename tag)
    (rename_subst rename arguments) (rename_value rename value) =
  rename_signal rename (I.check_tag P d tag arguments value).
Proof.
  intros P P' d tag arguments value Casts. unfold I.check_tag.
  rewrite Casts, rename_lookup_cast by exact injective.
  destruct (lookup_cast tag (view_casts P)) as [schema|]; [|reflexivity].
  cbn [option_map].
  pose proof (rename_cast_guard d schema arguments) as Guard.
  unfold PublicCastTyping.runtime_guard, type_argument in Guard. rewrite Guard.
  destruct (PublicCastTyping.runtime_guard d schema arguments) eqn:Enabled;
    unfold PublicCastTyping.runtime_guard, type_argument in Enabled;
    try rewrite Enabled; [|reflexivity].
  cbn [rename_cast_spec cast_kind cast_held].
  rewrite rename_lookup_subst by exact injective.
  destruct (lookup_subst (cast_held schema) arguments) as [held|]; [|reflexivity].
  destruct value as [|n value|fields|sort bits|address [[kind header]|]]; try reflexivity.
  cbn [option_map rename_value].
  rewrite rename_ground, rename_ty_eqb by exact injective.
  destruct (_ && _); reflexivity.
Qed.

Theorem eval_tag_equivariant : forall fuel P P' d g left right tag arguments e,
  views_related rename P P' ->
  evaluations_related
    (I.eval fuel P d g left e)
    (I.eval fuel P' (rename_environment rename d) (rename_locals rename g) right (rename_expr rename e)) ->
  evaluations_related
    (I.eval (S fuel) P d g left (ETag tag arguments e))
    (I.eval (S fuel) P' (rename_environment rename d) (rename_locals rename g) right
      (rename_expr rename (ETag tag arguments e))).
Proof.
  intros fuel P P' d g left right tag arguments e Views Related.
  cbn [I.eval rename_expr].
  destruct (I.eval fuel P d g left e) as [out locals next].
  destruct (I.eval fuel P' (rename_environment rename d) (rename_locals rename g) right (rename_expr rename e))
    as [out' locals' next'].
  destruct Related as [Signal [Locals State]]. cbn in Signal, Locals, State. subst out' locals'.
  destruct out; try (exact (conj eq_refl (conj eq_refl State))).
  split; [apply check_tag_equivariant; exact (related_casts _ _ _ Views)|exact (conj eq_refl State)].
Qed.

Theorem eval_primitive_equivariant : forall fuel P P' d g left right f operation site args,
  lookup_provision (IFn f) d = Some (PrimitiveClosure operation site) ->
  primitives_related P P' ->
  arguments_related
    (I.eval_arguments fuel P d g left args)
    (I.eval_arguments fuel P' (rename_environment rename d) (rename_locals rename g) right
      (map (rename_expr rename) args)) ->
  evaluations_related
    (I.eval (S fuel) P d g left (EInvoke f args))
    (I.eval (S fuel) P' (rename_environment rename d) (rename_locals rename g) right
      (rename_expr rename (EInvoke f args))).
Proof.
  intros fuel P P' d g left right f operation site args Lookup Calls Args.
  cbn [rename_expr I.eval].
  change (IFn (rename f)) with (rename_item rename (IFn f)).
  rewrite rename_lookup_provision by exact injective. rewrite Lookup.
  cbn [option_map rename_provision].
  destruct (I.eval_arguments fuel P d g left args) as [[[values g1] state]|err];
    destruct (I.eval_arguments fuel P' (rename_environment rename d) (rename_locals rename g) right
      (map (rename_expr rename) args)) as [[[values' g1'] state']|err']; try exact Args.
  - destruct Args as [-> [-> Related]].
    specialize (Calls operation site values state state' Related).
    unfold I.run_prim_at.
    destruct (Prims.run_prim P operation site values state) as [out next].
    destruct (Prims.run_prim P' operation (rename_site rename site)
      (map (rename_value rename) values) state') as [out' next'].
    exact (conj (proj1 Calls) (conj eq_refl (proj2 Calls))).
  - contradiction.
  - contradiction.
Qed.
End Equivariance.
End ExecutionRenaming.

Module PublicRenaming (Host : HOST) (M : PUBLIC Host).
Module E := ExecutionRenaming M.ReferenceHost M.Reference.

Section Observations.
Variable rename : nat -> nat.
Variable interfaces_related : M.interface -> M.interface -> Prop.
Variable external_related : Host.state -> Host.state -> Prop.

Definition worlds_related (left right : M.state) : Prop :=
  M.arguments right = M.arguments left /\ M.read_file right = M.read_file left /\
  external_related (M.external left) (M.external right).

Definition machine_states_related (left right : M.ReferenceHost.mstate) : Prop :=
  interfaces_related (fst left) (fst right) /\ worlds_related (snd left) (snd right).

Definition rename_outcome (outcome : M.outcome) : M.outcome :=
  match outcome with M.Returned value => M.Returned (rename_value rename value) | other => other end.

Definition executions_related (left right : M.execution) : Prop :=
  M.execution_outcome right = rename_outcome (M.execution_outcome left) /\
  worlds_related (M.final_state left) (M.final_state right).

Theorem observe_equivariant : forall left right,
  E.evaluations_related rename machine_states_related left right ->
  match M.observe left, M.observe right with
  | Some result, Some result' => executions_related result result'
  | None, None => True
  | _, _ => False
  end.
Proof.
  intros [out g [interface state]] [out' g' [interface' state']] Related.
  destruct Related as [Signal [_ [_ World]]]. cbn in Signal. subst out'.
  destruct out; cbn [M.observe rename_signal rename_outcome executions_related];
    try exact I; exact (conj eq_refl World).
Qed.

Theorem runs_transport : forall program program' interface interface' initial initial',
  (forall fuel,
    E.evaluations_related rename machine_states_related
      (M.Reference.run_main fuel (M.runtime program initial)
        (program_main program) (program_environment program) (interface, initial))
      (M.Reference.run_main fuel (M.runtime program' initial')
        (program_main program') (program_environment program') (interface', initial'))) ->
  forall result, M.Runs program interface initial result ->
    exists result', M.Runs program' interface' initial' result' /\ executions_related result result'.
Proof.
  intros program program' interface interface' initial initial' Related result [fuel Seen].
  pose proof (observe_equivariant _ _ (Related fuel)) as Observed. rewrite Seen in Observed.
  destruct (M.observe (M.Reference.run_main fuel (M.runtime program' initial')
    (program_main program') (program_environment program') (interface', initial')))
    as [result'|] eqn:Seen'; [|contradiction].
  exists result'. split; [now exists fuel|exact Observed].
Qed.
End Observations.
End PublicRenaming.
