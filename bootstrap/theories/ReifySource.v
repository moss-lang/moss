(* Source-specific preparation for immutable lowering.  Raw metadata is
   converted structurally, with no additional type-depth budget. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import CoreMoss Elab BodyTr ReifyData Reify NMap.
Import ListNotations.
Import ExecutionModel.Input.
Local Open Scope list_scope.

Fixpoint reify_type_fields (t : ety) : list string :=
  match t with
  | YUnit | YNever | YAbs _ => []
  | YNom _ args => flat_map (fun p => reify_type_fields (snd p)) args
  | YUnion ms => flat_map reify_type_fields ms
  | YRec fs => flat_map (fun p => fst p :: reify_type_fields (snd p)) fs
  end.

Definition reify_key_fields (k : key) : list string :=
  match k with KyMth receiver _ => reify_type_fields receiver | _ => [] end.
Definition reify_argument_fields (args : list (nat * ety)) : list string :=
  flat_map (fun p => reify_type_fields (snd p)) args.
Definition reify_signature_fields (sg : reify_signature ety) : list string :=
  flat_map reify_type_fields (rsg_params ety sg) ++
  reify_type_fields (rsg_result ety sg) ++
  match rsg_receiver ety sg with Some t => reify_type_fields t | None => [] end.
Definition reify_scope_fields (scope : reify_scope ety key) : list string :=
  reify_argument_fields (rs_names ety key scope) ++
  reify_argument_fields (rs_generations ety key scope) ++
  flat_map reify_key_fields (rs_items ety key scope) ++
  reify_argument_fields (rs_values ety key scope) ++
  flat_map (fun p => reify_signature_fields (snd p)) (rs_functions ety key scope) ++
  flat_map (fun p => reify_type_fields (fst (fst p)) ++
    reify_signature_fields (snd p)) (rs_methods ety key scope) ++
  flat_map (fun p => reify_key_fields (fst p) ++
    flat_map (fun q => match snd q with Some t => reify_type_fields t | None => [] end)
      (fst (snd p))) (rs_provisions ety key scope).

Definition reify_event_fields (event : reify_event ety key) : list string :=
  match event with
  | REType _ _ _ t => reify_type_fields t
  | REValue _ _ _ t arguments => reify_type_fields t ++ reify_argument_fields arguments
  | REProvider _ _ _ inputs sg args =>
      flat_map reify_key_fields inputs ++ reify_signature_fields sg ++ reify_argument_fields args
  | REFunction _ _ _ _ sg args => reify_signature_fields sg ++ reify_argument_fields args
  | REMethod _ _ receiver _ _ sg args =>
      reify_type_fields receiver ++ reify_signature_fields sg ++ reify_argument_fields args
  | REFunctor _ _ _ _ inputs args view =>
      flat_map reify_key_fields inputs ++ reify_argument_fields args ++
      match view with Some v => reify_scope_fields (rx_inputs ety key v) ++
        reify_scope_fields (rx_promised ety key v) ++ reify_scope_fields (rx_actual ety key v)
      | None => [] end
  | REApply _ _ _ inputs outputs args results output_scope =>
      flat_map reify_key_fields (inputs ++ outputs) ++
      reify_argument_fields args ++ reify_argument_fields results ++ reify_scope_fields output_scope
  end.
Definition reify_code_fields (code : reify_code ety key) : list string :=
  reify_scope_fields (rc_scope ety key code) ++
  flat_map reify_key_fields (rc_inputs ety key code) ++
  (match rc_signature ety key code with
   | Some sg => reify_signature_fields sg | None => [] end) ++
  (match rc_exports ety key code with
   | Some exports => flat_map reify_key_fields (rx_items ety key exports) ++
      reify_scope_fields (rx_promised ety key exports) ++
      reify_scope_fields (rx_actual ety key exports) ++
      reify_scope_fields (rx_inputs ety key exports)
   | None => [] end) ++
  flat_map reify_event_fields (rc_events ety key code).
Definition reify_metadata_fields (metadata : reify_metadata ety key) : list string :=
  flat_map (fun p => reify_code_fields (snd p)) (rm_codes metadata) ++
  flat_map (fun p => flat_map reify_event_fields (snd p)) (rm_constants metadata).

(* Nominal payload schemas are lazy: record labels can occur in an unused
   schema even when no source expression has interned them yet.  Include all
   declaration type labels while preserving every previously allocated ID. *)
Fixpoint reify_annotation_fields (t : ExecutionModel.Input.aty) : list string :=
  match t with
  | ExecutionModel.Input.ATRec fs => flat_map (fun p =>
      fst p :: reify_annotation_fields (snd p)) fs
  | ExecutionModel.Input.ATUnion ms => flat_map reify_annotation_fields ms
  | _ => []
  end.
Definition reify_decl_fields (d : ExecutionModel.Input.adecl) : list string :=
  match d with
  | ExecutionModel.Input.ADAlias _ t | ExecutionModel.Input.ADTag _ _ t | ExecutionModel.Input.ADVal _ t _ =>
      reify_annotation_fields t
  | ExecutionModel.Input.ADFn _ _ ps rt _ =>
      flat_map (fun p => reify_annotation_fields (snd p)) ps ++
      match rt with Some t => reify_annotation_fields t | None => [] end
  | _ => []
  end.
Definition reify_declaration_fields (s : st) : list string :=
  flat_map (fun p => reify_decl_fields (gs_decl (snd p))) (elems (st_syms s)).

Fixpoint reify_field_lookup (name : string) (fields : list (string * nat)) : option nat :=
  match fields with
  | [] => None
  | (s,n) :: rest => if String.eqb name s then Some n else reify_field_lookup name rest
  end.
Fixpoint extend_reify_fields (fields : list (string * nat)) (names : list string)
    : list (string * nat) :=
  match names with
  | [] => fields
  | name :: rest =>
      let fields' := match reify_field_lookup name fields with
        | Some _ => fields
        | None => fields ++ [(name,S (List.length fields))]
        end in extend_reify_fields fields' rest
  end.
Definition reify_field_table (s : st) : list (string * nat) :=
  extend_reify_fields (st_flds s)
    (reify_metadata_fields (st_reify s) ++ reify_declaration_fields s).
Definition reify_field_number (s : st) (name : string) : nat :=
  match reify_field_lookup name (reify_field_table s) with Some n => n | None => 0 end.
Definition reify_source_type (s : st) (scope : reify_state) (t : ety) : ty :=
  ExecutionModel.app_subst (rf_generations scope) (tr (reify_field_number s) t).

Lemma reify_field_lookup_app : forall fields extra name n,
    reify_field_lookup name fields = Some n ->
    reify_field_lookup name (fields ++ extra) = Some n.
Proof.
  induction fields as [|[s k] rest IH]; intros extra name n H; cbn in *;
    [discriminate|]. destruct (String.eqb name s); [exact H|eauto].
Qed.
Lemma reify_field_lookup_new : forall fields name n,
    reify_field_lookup name fields = None ->
    reify_field_lookup name (fields ++ [(name,n)]) = Some n.
Proof.
  induction fields as [|[s k] rest IH]; intros name n H; cbn in *.
  - rewrite String.eqb_refl. reflexivity.
  - destruct (String.eqb name s); [discriminate|eauto].
Qed.
Lemma extend_reify_fields_preserve : forall names fields name n,
    reify_field_lookup name fields = Some n ->
    reify_field_lookup name (extend_reify_fields fields names) = Some n.
Proof.
  induction names as [|s rest IH]; intros fields name n H; cbn; [exact H|].
  destruct (reify_field_lookup s fields); apply IH; [exact H|].
  apply reify_field_lookup_app. exact H.
Qed.
Lemma extend_reify_fields_contains : forall names fields name,
    In name names -> exists n,
    reify_field_lookup name (extend_reify_fields fields names) = Some n.
Proof.
  induction names as [|s rest IH]; intros fields name H; [contradiction|].
  cbn in H |- *. destruct H as [<-|H].
  - destruct (reify_field_lookup s fields) as [n|] eqn:E.
    + exists n. apply extend_reify_fields_preserve. exact E.
    + exists (S (List.length fields)). apply extend_reify_fields_preserve.
      apply reify_field_lookup_new. exact E.
  - apply IH. exact H.
Qed.

Lemma reify_fields_keep_source : forall s name n,
    reify_field_lookup name (st_flds s) = Some n ->
    reify_field_lookup name (reify_field_table s) = Some n.
Proof. intros. apply extend_reify_fields_preserve. assumption. Qed.
Lemma reify_fields_cover_metadata : forall s name,
    In name (reify_metadata_fields (st_reify s)) ->
    exists n, reify_field_lookup name (reify_field_table s) = Some n.
Proof. intros. apply extend_reify_fields_contains. apply in_or_app. left. assumption. Qed.

Definition reify_source_signature (s : st) (runtime : reify_state)
    (signature : reify_signature ety) : fnsig :=
  mkSig (map (reify_source_type s runtime) (rsg_params ety signature))
    (reify_source_type s runtime (rsg_result ety signature)).
Definition reify_source_code_signature (s : st) (runtime : reify_state)
    (signature : reify_signature ety) : fnsig :=
  let ordinary := reify_source_signature s runtime signature in
  mkSig ((match rsg_receiver ety signature with
          | Some receiver => [reify_source_type s runtime receiver]
          | None => [] end) ++ fs_params ordinary) (fs_ret ordinary).

Definition reify_symbol_owner (g : gsym) : reify_owner :=
  match gs_origin g with
  | OLink target provider => ROLink target provider
  | OPlatformFun target => ROPlatform target
  | _ => ROFunction (gs_id g)
  end.
Fixpoint lookup_reify_code (owner : reify_owner)
    (codes : list (reify_owner * reify_code ety key)) : option (reify_code ety key) :=
  match codes with
  | [] => None
  | (key,code) :: rest =>
      if reify_owner_eqb owner key then Some code else lookup_reify_code owner rest
  end.
Definition reify_code_for_symbol (s : st) (id : nat) : option (reify_code ety key) :=
  match nget (st_syms s) id with
  | Some g => lookup_reify_code (reify_symbol_owner g) (rm_codes (st_reify s))
  | None => None
  end.

(* Snapshot generation meanings are fully canonical checker results. Render
   them with one structural substitution; mutable source names never enter
   this operation. *)
Definition reify_snapshot_subst (s : st) (scope : reify_scope ety key) : subst :=
  map (fun p => (fst p,tr (reify_field_number s) (snd p))) (rs_generations ety key scope).
Definition reify_snapshot_type (s : st) (scope : reify_scope ety key) (t : ety) : ty :=
  ExecutionModel.app_subst (reify_snapshot_subst s scope) (tr (reify_field_number s) t).
Fixpoint reify_provision_arguments (k : key)
    (provisions : list (key * (list (tsym * option ety) * nat)))
    : list (tsym * option ety) :=
  match provisions with
  | [] => []
  | (other,(arguments,_)) :: rest =>
      if key_eqb k other then arguments else reify_provision_arguments k rest
  end.
Definition reify_snapshot_theta (s : st) (scope : reify_scope ety key) (k : key) : subst :=
  flat_map (fun p => match snd p with
     | Some t => [(fst p,reify_snapshot_type s scope t)]
     | None => [] end)
    (reify_provision_arguments k (rs_provisions ety key scope)).
Definition reify_snapshot_item (s : st) (scope : reify_scope ety key) (k : key) : item :=
  match k with
  | KyVal v => IVal v
  | KyFn f => IFn f
  | KyFun u => IFun u
  | KyMth receiver method =>
      IMth (tr (reify_field_number s) receiver) method (reify_snapshot_theta s scope k)
  end.

Lemma reify_source_code_signature_arity : forall s runtime signature,
    List.length (fs_params (reify_source_code_signature s runtime signature)) =
    (match rsg_receiver ety signature with Some _ => 1 | None => 0 end) +
      List.length (rsg_params ety signature).
Proof.
  intros. unfold reify_source_code_signature,reify_source_signature. cbn.
  rewrite app_length,map_length. destruct (rsg_receiver ety signature); reflexivity.
Qed.

Definition reify_runtime_schema (source : st) : runtime_schema :=
  map (fun q => (q,match sassoc (fst q) (st_lang source) with
    | None => None
    | Some mi => sassoc (snd q) (gm_names (get_mod (st_w source) mi)) end)) runtime_names.

Lemma reify_runtime_schema_exact : forall source,
    runtime_symbols source = Good (reify_runtime_schema source,source).
Proof.
  intros source. unfold runtime_symbols,reify_runtime_schema.
  generalize runtime_names as names. intro names.
  induction names as [|[short name] rest IH]; cbn [mmap mbind ret lang get the_mod].
  - reflexivity.
  - unfold mbind,get,ret,the_mod in *. cbn in *.
    destruct (sassoc short (st_lang source)); cbn; rewrite IH; reflexivity.
Qed.
