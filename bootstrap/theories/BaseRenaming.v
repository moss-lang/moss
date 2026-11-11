(* Base allocation operations name their source tag. Other operands,
   including unsupported-operation diagnostic indices, are fixed. *)
From Stdlib Require Import List Bool String.
From NM Require Import Audit AuditBase RuntimeRenaming.
From NM Require BaseHost PublicOperationFacts.
Import AuditBase.Semantics AuditBase.Semantics.Elaboration ListNotations.

Definition rename_base_primitive (rename : nat -> nat) (primitive : wasm_prim) : wasm_prim :=
  match primitive with
  | PBox tag => PBox (rename tag)
  | PArrNew tag => PArrNew (rename tag)
  | PGcCast array tag => PGcCast array (rename tag)
  | other => other
  end.

Definition rename_base_operation (rename : nat -> nat) (operation : op) : op :=
  match decode operation with
  | Some (PBox tag) => encode (PBox (rename tag))
  | Some (PArrNew tag) => encode (PArrNew (rename tag))
  | Some (PGcCast array tag) => encode (PGcCast array (rename tag))
  | _ => operation
  end.

Definition base_renaming (rename : nat -> nat) : renaming :=
  Renaming rename (rename_base_operation rename).

Theorem decode_renamed_base_operation : forall rename operation,
  decode (rename_base_operation rename operation) =
  option_map (rename_base_primitive rename) (decode operation).
Proof.
  intros rename operation. unfold rename_base_operation.
  destruct (decode operation) as [primitive|] eqn:Decoded; [|exact Decoded].
  destruct primitive; try exact Decoded; apply PublicOperationFacts.decode_encode.
Qed.

Lemma rename_base_encoding : forall rename primitive,
  rename_base_operation rename (encode primitive) = encode (rename_base_primitive rename primitive).
Proof.
  intros rename primitive. unfold rename_base_operation.
  rewrite PublicOperationFacts.decode_encode. destruct primitive; reflexivity.
Qed.

Theorem rename_unsupported_operation : forall rename name index,
  rename_base_operation rename (unsupported_operation name index) = unsupported_operation name index /\
  unsupported_code (rename_base_operation rename (unsupported_operation name index)) = index.
Proof. intros. split; reflexivity. Qed.

Definition fixes_declarations (rename : nat -> nat) (W : world) : Prop :=
  forall symbol declaration, w_decl W symbol = Some declaration -> rename symbol = symbol.

Theorem typed_base_operation_fixed : forall rename W operation params result,
  fixes_declarations rename W ->
  Platform.PrimitiveSignature W operation params result ->
  rename_base_operation rename operation = operation.
Proof.
  intros rename W operation params result Fixed Typed.
  unfold rename_base_operation, Platform.PrimitiveSignature in *.
  destruct (decode operation) as [primitive|] eqn:Decoded; [|reflexivity].
  destruct primitive; try reflexivity.
  all: destruct Typed as [Plain|[Method|GC]];
    try (destruct Plain as (? & ? & Impossible & _); discriminate);
    try (destruct Method as (? & ? & Impossible & _); discriminate).
  - destruct params as [|held [|extra rest]]; try contradiction.
    destruct GC as [(declaration & path & Declared & _) _].
    apply PublicOperationFacts.decode_canonical in Decoded.
    rewrite (Fixed _ _ Declared). symmetry. exact Decoded.
  - destruct params as [|held [|count [|extra rest]]]; try contradiction.
    destruct GC as [(declaration & path & Declared & _) _].
    apply PublicOperationFacts.decode_canonical in Decoded.
    rewrite (Fixed _ _ Declared). symmetry. exact Decoded.
  - destruct params as [|request [|extra rest]]; try contradiction.
    destruct GC as [(declaration & path & Declared & _) _].
    apply PublicOperationFacts.decode_canonical in Decoded.
    rewrite (Fixed _ _ Declared). symmetry. exact Decoded.
Qed.

Lemma operation_at_key_fixed : forall rename W context key operation,
  fixes_declarations rename W ->
  Platform.AtKey W context key operation ->
  rename_base_operation rename operation = operation.
Proof.
  intros rename W context key operation Fixed AtKey.
  destruct key; cbn [Platform.AtKey] in AtKey; try contradiction;
    destruct AtKey as (signature & _ & Typed);
    eapply typed_base_operation_fixed; eassumption.
Qed.

Theorem linked_entry_operation_fixed : forall rename W P context key operation site,
  fixes_declarations rename W ->
  Platform.Entry W P context key (PrimitiveClosure operation site) ->
  rename_base_operation rename operation = operation.
Proof.
  intros rename W P context key operation [site|] Fixed Entry.
  all: destruct key; cbn [Platform.Entry] in Entry; try contradiction.
  destruct Entry as [AtKey _]. eapply operation_at_key_fixed; eassumption.
Qed.

Theorem linked_family_operations_fixed : forall rename W P source code body,
  fixes_declarations rename W ->
  Platform.Family W P source code ->
  funbody_of P code = Some body ->
  Forall (fun binding => match binding with
    | BPrimitive _ operation => rename_base_operation rename operation = operation
    | _ => True end) (fc_bindings body).
Proof.
  intros rename W P source code body Fixed Family Lookup.
  destruct Family as (inputs & needs & outputs & keys & bindings & _ & _ & _ & Code & Methods & _).
  rewrite Code in Lookup. injection Lookup as <-. cbn [fc_bindings].
  clear Code. induction Methods as [|key binding rest tail Method Methods IH].
  - constructor.
  - constructor; [|exact IH]. destruct Method as (operation & -> & _ & AtKey & _).
    eapply operation_at_key_fixed; eassumption.
Qed.

Theorem base_host_ignores_renamed_code : forall rename P P',
  views_related rename P P' ->
  rename (view_bool P) = view_bool P ->
  rename (view_true P) = view_true P ->
  rename (view_false P) = view_false P ->
  forall operation values state,
    BaseHost.primitive P' operation values state = BaseHost.primitive P operation values state.
Proof.
  intros rename P P' Views Bool True False. apply BaseHost.primitive_ignores_code.
  - now rewrite (related_bool _ _ _ Views).
  - now rewrite (related_true _ _ _ Views).
  - now rewrite (related_false _ _ _ Views).
  - exact (related_arguments _ _ _ Views).
  - exact (related_read _ _ _ Views).
Qed.

Lemma rename_raw_receiver : forall rename value,
  BaseCall.raw_receiver (rename_value rename value) = rename_value rename (BaseCall.raw_receiver value).
Proof.
  intros rename [|tag payload|fields|[sort bits|address header]]; try reflexivity.
  destruct payload as [|inner value|fields|[sort bits|address [[kind held]|]]]; reflexivity.
Qed.

Lemma rename_base_arguments : forall rename primitive values,
  BaseCall.primitive_arguments (rename_base_primitive rename primitive)
    (map (rename_value (base_renaming rename)) values) =
  map (rename_value (base_renaming rename)) (BaseCall.primitive_arguments primitive values).
Proof.
  intros rename primitive values. destruct primitive; try reflexivity;
    destruct values as [|a [|b [|c [|d [|e [|f rest]]]]]];
    cbn [BaseCall.primitive_arguments rename_base_primitive map]; try reflexivity;
    now rewrite !rename_raw_receiver.
Qed.

Lemma rename_memory_arguments : forall rename primitive site values,
  BaseCall.memory_arguments (rename_base_primitive rename primitive)
    (fun _ => rename_site (base_renaming rename) (site tt))
    (map (rename_value (base_renaming rename)) values) =
  map (rename_value (base_renaming rename)) (BaseCall.memory_arguments primitive site values).
Proof.
  intros rename primitive site values.
  unfold BaseCall.memory_arguments.
  destruct primitive; try reflexivity; destruct (site tt); reflexivity.
Qed.

Lemma rename_arguments_at_site : forall rename primitive site values,
  BaseCall.arguments_at_site (rename_base_primitive rename primitive)
    (fun _ => rename_site (base_renaming rename) site)
    (map (rename_value (base_renaming rename)) values) =
  map (rename_value (base_renaming rename))
    (BaseCall.arguments_at_site primitive (fun _ => site) values).
Proof.
  intros rename primitive site values. unfold BaseCall.arguments_at_site.
  rewrite rename_base_arguments.
  exact (rename_memory_arguments rename primitive (fun _ => site)
    (BaseCall.primitive_arguments primitive values)).
Qed.

Lemma rename_allocation_result : forall rename primitive site outcome,
  BaseCall.allocation_result (rename_base_primitive rename primitive) (rename_ty (base_renaming rename) site)
    (rename_signal (base_renaming rename) outcome) =
  rename_signal (base_renaming rename) (BaseCall.allocation_result primitive site outcome).
Proof.
  intros rename primitive site outcome. destruct primitive, outcome; try reflexivity.
  all: destruct v as [|tag payload|fields|[sort bits|address header]]; try reflexivity.
  all: destruct payload as [|inner value|fields|[sort bits|address [[kind held]|]]]; reflexivity.
Qed.

Definition raw_calls_related {S : Type} (rename : nat -> nat) (related : S -> S -> Prop)
    (raw : program_view -> op -> list value -> S -> signal * S) (P P' : program_view) : Prop :=
  forall operation values left right,
    related left right ->
    let '(out, next) := raw P operation values left in
    let '(out', next') := raw P' (rename_base_operation rename operation)
      (map (rename_value (base_renaming rename)) values) right in
    out' = rename_signal (base_renaming rename) out /\ related next next'.

Lemma cast_result_renamed : forall rename array tag site values,
  (forall x y, rename x = rename y -> x = y) ->
  BaseCall.cast_result array (rename tag) (rename_site (base_renaming rename) site)
      (map (rename_value (base_renaming rename)) values) =
  rename_signal (base_renaming rename) (BaseCall.cast_result array tag site values).
Proof.
  intros rename array tag [site|] values Injective; [|reflexivity].
  destruct site as [| | | |head [|[param expected] rest]|]; try reflexivity.
  destruct values as [|[|source [| | |[|address [[kind actual]|]]]| |] [|extra tail]];
    try reflexivity.
  cbn [BaseCall.cast_result rename_site option_map rename_value rename_ty
    rename_signal map fst snd base_renaming rename_symbol].
  rewrite rename_ty_eqb by exact Injective.
  destruct (_ && _); reflexivity.
  all: destruct p; reflexivity.
Qed.

Lemma reference_matches_renamed : forall rename array site value,
  (forall x y, rename x = rename y -> x = y) ->
  BaseCall.reference_matches array (rename_site (base_renaming rename) site)
      (rename_value (base_renaming rename) value) =
  BaseCall.reference_matches array site value.
Proof.
  intros rename array [site|] value Injective; [|reflexivity].
  destruct site as [| | | |head [|[param expected] rest]|]; try reflexivity.
  destruct value as [|source [| | |[|address [[kind actual]|]]]| |[]]; try reflexivity.
  cbn [BaseCall.reference_matches rename_site option_map rename_value rename_ty
    map fst snd base_renaming rename_symbol].
  now rewrite rename_ty_eqb by exact Injective.
Qed.

Lemma heap_arguments_match_renamed : forall rename p site values,
  (forall x y, rename x = rename y -> x = y) ->
  BaseCall.heap_arguments_match (rename_base_primitive rename p)
      (fun _ => rename_site (base_renaming rename) site)
      (map (rename_value (base_renaming rename)) values) =
  BaseCall.heap_arguments_match p (fun _ => site) values.
Proof.
  intros rename [] site values Injective; try reflexivity;
    destruct values as [|receiver [|index [|source rest]]];
    cbn [BaseCall.heap_arguments_match rename_base_primitive map];
    now rewrite ?reference_matches_renamed by exact Injective.
Qed.

Theorem base_call_equivariant : forall S rename related raw P P',
  (forall x y, rename x = rename y -> x = y) ->
  @raw_calls_related S rename related raw P P' ->
  forall operation site values left right,
    related left right ->
    let '(out, next) := BaseCall.call raw P operation site values left in
    let '(out', next') := BaseCall.call raw P' (rename_base_operation rename operation)
      (rename_site (base_renaming rename) site) (map (rename_value (base_renaming rename)) values) right in
    out' = rename_signal (base_renaming rename) out /\ related next next'.
Proof.
  intros S rename related raw P P' Injective Raw operation site values left right Related.
  unfold BaseCall.call. rewrite decode_renamed_base_operation.
  destruct (decode operation) as [primitive|].
  - cbn [option_map BaseCall.cast_at_site BaseCall.heap_call_ready].
    assert (Cast : BaseCall.cast_operation (rename_base_primitive rename primitive) =
      option_map (fun '(array, tag) => (array, rename tag)) (BaseCall.cast_operation primitive))
      by (destruct primitive; reflexivity).
    rewrite Cast. destruct (BaseCall.cast_operation primitive) as [[array tag]|].
    { split; [now apply cast_result_renamed|exact Related]. }
    cbn [option_map]. rewrite heap_arguments_match_renamed by exact Injective.
    destruct (BaseCall.heap_arguments_match primitive _ values); [|now split].
    rewrite rename_arguments_at_site.
    specialize (Raw operation (BaseCall.arguments_at_site primitive (fun _ => site) values) left right Related).
    destruct (raw P operation (BaseCall.arguments_at_site primitive (fun _ => site) values) left) as [out next].
    destruct (raw P' (rename_base_operation rename operation)
      (map (rename_value (base_renaming rename)) (BaseCall.arguments_at_site primitive (fun _ => site) values)) right)
      as [out' next'].
    destruct Raw as [-> State].
    destruct site; cbn [rename_site option_map]; split; try assumption; try reflexivity.
    apply rename_allocation_result.
  - cbn [option_map]. specialize (Raw operation values left right Related).
    destruct (raw P operation values left).
    destruct (raw P' (rename_base_operation rename operation)
      (map (rename_value (base_renaming rename)) values) right).
    destruct site; exact Raw.
Qed.
