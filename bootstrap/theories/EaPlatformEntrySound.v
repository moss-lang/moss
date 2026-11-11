(* Soundness of the platform provisions installed by executable assembly. *)
From Stdlib Require Import List String Bool Arith ZArith Lia.
From NM Require Import Audit EaBase EaBudget EaPlatform EaTypes EaTypesSound.
From NM Require AuditBase EaCall EaContextFuel EaContextTableSound EaForwarderSound EaMergeSound
  EaOutputSound EaResultBudget EaRootSound EaSatisfiesSound EaCodeAllocationSound
  EaAssemblyCodeEffects EaCodeEffects EaControlSound EaMain EaMetadataSound EaWorldSound ElabWorld
  PublicCallRealize PublicOperationFacts PublicTypeFacts PublicTypeRead WorldSound.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration AuditBase.
Local Open Scope string_scope.
Local Open Scope list_scope.
Module P := AuditBase.Platform.

Section Entries.
Variable E : env.
Let W := e_W E.

Lemma machine_type_sound : forall name t,
  machine_type E name t = true -> P.MachineType W name t.
Proof.
  intros name [] Checked; unfold machine_type in Checked;
    destruct (wasm_sym E name) as [symbol|] eqn:Read; try discriminate.
  apply andb_true_iff in Checked as [Equal Kind]. apply Nat.eqb_eq in Equal. subst symbol.
  eexists. repeat split; [eapply lang_sym_sound; exact Read|now apply kind_is_spec].
Qed.

Lemma value_type_sound : forall t shape,
  value_type E t shape = true -> P.ValueType W t shape.
Proof.
  intros t []; unfold value_type, P.ValueType; fold W; try apply machine_type_sound;
    try apply PublicTypeFacts.ty_eqb_spec.
  - destruct (lang_sym W "bool" NSName "Bool") as [symbol|] eqn:Read; [|discriminate].
    intros Checked. exists symbol. split; [eapply lang_sym_sound; exact Read|now apply PublicTypeFacts.ty_eqb_spec].
  - unfold nominal_type. fold W. destruct (lang_sym W "wasm" NSName "Addr") as [symbol|] eqn:Read;
      destruct t; try discriminate. intros Checked. apply Nat.eqb_eq in Checked. subst symbol.
    eexists _, _. split; [eapply lang_sym_sound; exact Read|reflexivity].
  - destruct t; try discriminate. intros _. now eexists _, _.
Qed.

Lemma signatureb_sound : forall params result signature,
  signatureb E params result signature = true -> P.Signature W params result signature.
Proof.
  intros params result [[inputs output]|] Checked; [|discriminate].
  apply andb_true_iff in Checked as [Params Result].
  exists inputs, output. split; [reflexivity|]. split; [|now apply value_type_sound].
  clear Result output. revert inputs Params.
  induction params as [|p params IH]; intros [|i inputs] Params; try discriminate.
  - constructor.
  - apply andb_true_iff in Params as [Head Tail]. constructor; [now apply value_type_sound|now apply IH].
Qed.

Lemma reference_held_sound : forall array t held,
  reference_held E array t = Some held -> P.ReferenceType W array t held.
Proof.
  intros array t held Read. unfold reference_held in Read.
  destruct (wasm_sym E (if array then "Array" else "Ref")) as [symbol|] eqn:Tag;
    destruct (wasm_sym E (if array then "Arrayed" else "Boxed")) as [parameter|] eqn:ParamRead;
    destruct t; try discriminate.
  destruct (Nat.eqb symbol n) eqn:Equal; [|discriminate]. apply Nat.eqb_eq in Equal. subst symbol.
  eexists _, parameter, _. repeat split; [eapply lang_sym_sound; exact Tag|eapply lang_sym_sound; exact ParamRead|exact Read].
Qed.

Local Lemma held_is_sound : forall array t held,
  held_is E array t held = true -> P.ReferenceType W array t held.
Proof.
  intros array t held. unfold held_is. destruct (reference_held E array t) as [found|] eqn:Read; [|discriminate].
  intro Checked. apply PublicTypeFacts.ty_eqb_spec in Checked. subst found. now apply reference_held_sound.
Qed.

Local Lemma is_ref_sound : forall array t,
  is_ref E array t = true -> exists held, P.ReferenceType W array t held.
Proof.
  intros array t. unfold is_ref. destruct (reference_held E array t) as [found|] eqn:Read; [|discriminate].
  intros _. exists found. now apply reference_held_sound.
Qed.

Local Lemma wasm_symbol_sound : forall name symbol,
  opt_eqb (wasm_sym E name) symbol = true -> LangSym W "wasm" NSName name symbol.
Proof.
  intros name symbol. unfold opt_eqb. destruct (wasm_sym E name) as [found|] eqn:Read; [|discriminate].
  intro Checked. apply Nat.eqb_eq in Checked. subst found. eapply lang_sym_sound. exact Read.
Qed.

Local Lemma unit_checked : forall t, ty_eqb t TUnit = true -> t = TUnit.
Proof. intros t Checked. now apply PublicTypeFacts.ty_eqb_spec. Qed.

Theorem gc_signature_sound : forall op params result,
  gc_signature E op params result = true -> P.GCSignature W op params result.
Proof.
  intros op params result Checked. destruct op; cbn [gc_signature] in Checked; try discriminate.
  all: destruct params as [|a [|b [|c [|d [|e [|f rest]]]]]];
    cbn [gc_signature P.GCSignature] in Checked |- *; try discriminate.
  all: repeat rewrite andb_true_iff in Checked.
  all: try solve [intuition eauto using machine_type_sound, wasm_symbol_sound, held_is_sound,
    is_ref_sound, unit_checked].
  destruct Checked as [[[[Destination Source] Count] Result] Read].
  repeat split; try solve [eauto using machine_type_sound, unit_checked].
  destruct (reference_held E true a) as [held|] eqn:Held; [|discriminate].
  exists held. split; [now apply reference_held_sound|now apply held_is_sound].
Qed.

Lemma primitive_signature_sound : forall op params result,
  primitive_signature E op params result = true -> P.PrimitiveSignature W (encode op) params result.
Proof.
  intros op params result Checked. unfold P.PrimitiveSignature.
  rewrite PublicOperationFacts.decode_encode. unfold P.WasmSignature, primitive_signature in *.
  repeat rewrite orb_true_iff in Checked.
  intuition eauto using signatureb_sound, gc_signature_sound.
Qed.

Lemma cmth_exact : forall C receiver method, cmth C receiver method = mlook receiver method (c_mths C).
Proof.
  intros C receiver method. unfold cmth, c_mths.
  induction (c_provisions C) as [|[k p] rest IH]; [reflexivity|].
  destruct k as [ty|v|f|r m args|u], p as [value_type inst|signature inst|signature inst|fixed];
    cbn [cmth_go flat_map app mlook]; try exact IH.
  rewrite (andb_comm (Nat.eqb method m) (ty_eqb receiver r)).
  destruct (ty_eqb receiver r && Nat.eqb method m); [reflexivity|exact IH].
Qed.

Theorem at_key_sound : forall C key op,
  at_key E C key op = true -> P.AtKey W C key (encode op).
Proof.
  intros C [ty|value|f|receiver method args|functor] op Checked; cbn [at_key P.AtKey] in *; try discriminate.
  - rewrite EaForwarderSound.cfn_exact in Checked.
    destruct (nlook f (c_fns C)) as [signature|]; [|discriminate].
    exists signature. split; [reflexivity|now apply primitive_signature_sound].
  - rewrite cmth_exact in Checked.
    destruct (mlook receiver method (c_mths C)) as [signature|]; [|discriminate].
    exists signature. split; [reflexivity|now apply primitive_signature_sound].
Qed.

Lemma raw_handle_read_sound : forall t, raw_handle_read E t = true -> P.RawHandleRead W t.
Proof.
  intros t Checked name handle symbol args Member Nominal -> raw Handle.
  unfold raw_handle_read in Checked.
  pose proof (proj1 (forallb_forall _ _) Checked (name,handle) Member) as Row. cbv beta iota zeta in Row. cbn [fst snd] in Row.
  unfold wasm_sym in Row. fold W in Row. rewrite (EaRootSound.lang_sym_complete _ _ _ _ _ Nominal),
    (EaRootSound.lang_sym_complete _ _ _ _ _ Handle), Nat.eqb_refl in Row.
  cbn [negb orb] in Row. destruct (nlook raw args) as [found|] eqn:Read; [|discriminate].
  cbn [oty_eqb] in Row. apply PublicTypeFacts.ty_eqb_spec in Row. now subst found.
Qed.

Theorem handles_read_sound : forall C key,
  handles_read E C key = true -> P.HandlesRead W C key.
Proof.
  intros C [ty|value|f|receiver method args|functor] Checked; cbn [handles_read P.HandlesRead] in *; try exact I.
  - intros signature Found. rewrite EaForwarderSound.cfn_exact, Found in Checked.
    apply Forall_forall. intros t Member. apply raw_handle_read_sound.
    exact (proj1 (forallb_forall _ _) Checked t Member).
  - intros signature Found. rewrite cmth_exact, Found in Checked.
    apply Forall_forall. intros t Member. apply raw_handle_read_sound.
    exact (proj1 (forallb_forall _ _) Checked t Member).
Qed.

Lemma cval_exact : forall C value, cval C value = nlook value (c_vals C).
Proof.
  intros C value. unfold cval, c_vals.
  induction (c_provisions C) as [|[k p] rest IH]; [reflexivity|].
  destruct k as [ty|v|f|r m args|u], p as [value_type inst|signature inst|signature inst|fixed];
    cbn [cval_go flat_map app nlook]; try exact IH.
  now destruct (Nat.eqb value v).
Qed.

Lemma platform_modb_sound : forall path,
  platform_modb E path = true -> P.platform_mod W path.
Proof.
  assert (Library : forall short path, library_module E short path = true -> LibraryModule W short path).
  { intros short path. unfold library_module, LibraryModule. fold W.
    destruct (w_library W); [apply String.eqb_eq|discriminate]. }
  intros path Checked. unfold platform_modb, P.platform_mod in *.
  repeat rewrite orb_true_iff in Checked. intuition eauto.
Qed.

Theorem platform_value_sound : forall C v s value after,
  platform_entry E C (IVal v) s = Ok (value, after) ->
  after = s /\ forall program, P.Provides W program C (IVal v) value.
Proof.
  intros C v s value after Checked. unfold platform_entry, mbind, ret, fail in Checked. cbv zeta in Checked. fold W in Checked.
  destruct (w_decl W v) as [declaration|] eqn:Decl; [|discriminate].
  destruct (platform_modb E (d_mod declaration)) eqn:ModuleOk; [|discriminate].
  destruct (cval C v) as [t|] eqn:TypeRead; [|discriminate].
  destruct (machine_type E "I32" t) eqn:TypeChecked; [|discriminate].
  destruct (filed_name (d_ast declaration)) as [[space name]|] eqn:Name; [|discriminate].
  destruct space; try discriminate.
  assert (ValueType : exists t, nlook v (c_vals C) = Some t /\ P.MachineType W "I32" t).
  { exists t. split; [now rewrite <- cval_exact|now apply machine_type_sound]. }
  destruct (String.eqb name "i32_zero") eqn:Zero.
  - injection Checked as <- <-. apply String.eqb_eq in Zero. subst name.
    split; [reflexivity|]. intro program. split; [exact ValueType|]. econstructor; eauto using platform_modb_sound.
  - destruct (String.eqb name "i32_one") eqn:One; [|discriminate].
    injection Checked as <- <-. apply String.eqb_eq in One. subst name.
    split; [reflexivity|]. intro program. split; [exact ValueType|]. econstructor; eauto using platform_modb_sound.
Qed.

Theorem platform_function_sound : forall P C f s value after,
  platform_entry E C (IFn f) s = Ok (value, after) -> P.Provides W P C (IFn f) value.
Proof.
  intros P C f s value after Checked.
  unfold platform_entry, mbind, ret, fail in Checked. cbv zeta in Checked. fold W in Checked.
  destruct (w_decl W f) as [declaration|] eqn:Decl; [|discriminate].
  destruct (platform_modb E (d_mod declaration)) eqn:ModuleOk; [|discriminate].
  destruct (d_ast declaration) as [| | | | | |receiver name params result body| |] eqn:Ast; try discriminate.
  destruct receiver; try discriminate.
  destruct (prim_of_name name) as [operation|] eqn:Primitive; [|discriminate].
  destruct (at_key E C (IFn f) operation && handles_read E C (IFn f)) eqn:Checks; [|discriminate].
  injection Checked as <- <-. apply andb_true_iff in Checks as [Signature Handles].
  split; [split|].
  - now apply at_key_sound.
  - now apply handles_read_sound.
  - eapply P.PEPrim; eauto using platform_modb_sound.
Qed.

(* Platform families install the GC and address operations at their checked keys. *)
Theorem method_primitive_sound : forall key operation,
  method_primitive E key = Some operation -> P.MethodPrimitive W key (encode operation).
Proof.
  intros [| | |receiver method theta|] operation Checked; try discriminate.
  cbn [method_primitive] in Checked. fold W in Checked.
  destruct (w_decl W method) as [d|] eqn:Decl; [|discriminate].
  destruct (d_ast d) as [| | | | | |form name params result body| |] eqn:Ast; try discriminate.
  exists receiver, method, theta, d, form, name, params, result, body. repeat split; try assumption.
  destruct form as [| |receiver_path]; try discriminate.
  - destruct (String.eqb name "boxed") eqn:Box.
    + destruct (wasm_sym E "Ref") as [symbol|] eqn:Read; [|discriminate].
      injection Checked as <-. left. apply String.eqb_eq in Box. split; [exact Box|].
      exists symbol. split; [eapply lang_sym_sound; exact Read|reflexivity].
    + destruct (String.eqb name "arrayed") eqn:Array; [|discriminate].
      destruct (wasm_sym E "Array") as [symbol|] eqn:Read; [|discriminate].
      injection Checked as <-. right. apply String.eqb_eq in Array. split; [exact Array|].
      exists symbol. split; [eapply lang_sym_sound; exact Read|reflexivity].
  - destruct (resolve_sym W (d_mod d) [receiver_path]) as [symbol|] eqn:Resolve; [|discriminate].
    exists symbol. split; [reflexivity|].
    destruct (opt_eqb (wasm_sym E "Ref") symbol) eqn:Ref.
    + left. split; [now apply wasm_symbol_sound|].
      destruct (String.eqb name "deref") eqn:Deref.
      * injection Checked as <-. left. now apply String.eqb_eq in Deref.
      * destruct (String.eqb name "set") eqn:SetMethod; [|discriminate].
        injection Checked as <-. right. now apply String.eqb_eq in SetMethod.
    + destruct (opt_eqb (wasm_sym E "Array") symbol) eqn:Array.
      * right. left. split; [now apply wasm_symbol_sound|].
        destruct (String.eqb name "length") eqn:Len.
        -- injection Checked as <-. left. now apply String.eqb_eq in Len.
        -- destruct (String.eqb name "get") eqn:Get.
           ++ injection Checked as <-. right. left. now apply String.eqb_eq in Get.
           ++ destruct (String.eqb name "set") eqn:SetMethod.
              ** injection Checked as <-. right. right. left. now apply String.eqb_eq in SetMethod.
              ** destruct (String.eqb name "fill") eqn:Fill.
                 --- injection Checked as <-. right. right. right. left. now apply String.eqb_eq in Fill.
                 --- destruct (String.eqb name "copy") eqn:Copy; [|discriminate].
                     injection Checked as <-. right. right. right. right. now apply String.eqb_eq in Copy.
      * destruct (opt_eqb (wasm_sym E "Addr") symbol) eqn:Addr; [|discriminate].
        right. right. split; [now apply wasm_symbol_sound|]. now exists operation.
Qed.

Lemma machines_fixed_sound : forall bound program u inputs outputs key,
  (forall symbol d, w_decl W symbol = Some d -> symbol < bound) ->
  metadata_budget W bound < e_metadata_fuel E ->
  program_machines program = P.machine_table W ->
  machines_fixed E u inputs outputs key = true -> P.MachinesFixed W program u inputs outputs key.
Proof.
  intros bound program u inputs outputs [| | |receiver method args|] Bounded Enough Machines Checked;
    cbn [machines_fixed P.MachinesFixed] in *; try exact I.
  intros signature Read symbol Machine Occurs. rewrite cmth_exact, Read in Checked.
  rewrite Machines in Machine. fold W in Checked.
  pose proof (proj1 (forallb_forall _ _) Checked symbol Machine) as Row. cbv beta iota zeta in Row.
  apply EaTypesSound.nmem_spec in Occurs. rewrite Occurs in Row.
  apply andb_true_iff in Row as [FixedRead BoundRead].
  split.
  - apply (proj1 (EaResultBudget.fixed_exact W bound Bounded _ u Enough symbol)). now apply EaTypesSound.nmem_spec.
  - unfold EaCall.bound_in in BoundRead. destruct (nlook symbol (c_ty inputs)) as [meaning|] eqn:Bound; [|discriminate].
    change symbol with (fst (symbol, meaning)). apply in_map. now apply PublicTypeRead.nlook_in.
Qed.

Lemma detached_method_complete : forall method,
  P.DetachedMethod W method -> detached_method E method = true.
Proof.
  intros method (d & name & params & result & body & Decl & Ast).
  unfold detached_method, ast_of. fold W. rewrite Decl. cbn [option_map]. now rewrite Ast.
Qed.

Lemma methods_distinct_sound : forall keys,
  methods_distinct E keys = true -> P.MethodsDistinct W keys.
Proof.
  intros keys Checked k1 k2 r1 r2 m1 m2 a1 a2 First Second -> -> Compare.
  unfold methods_distinct in Checked.
  apply forallb_forall with (x := IMth r1 m1 a1) in Checked; [|exact First].
  apply forallb_forall with (x := IMth r2 m2 a2) in Checked; [|exact Second].
  cbv beta iota in Checked. apply PublicCallRealize.item_eqb_spec.
  destruct Compare as [->|[Detached1 Detached2]].
  - now rewrite Nat.eqb_refl in Checked.
  - now rewrite (detached_method_complete _ Detached1), (detached_method_complete _ Detached2), andb_true_r, orb_true_r in Checked.
Qed.

Lemma provision_ids_distinct : forall keys, inodupb keys = true -> NoDup keys.
Proof.
  induction keys as [|key rest IH]; intro Checked; [constructor|].
  cbn [inodupb] in Checked. apply andb_true_iff in Checked as [New Distinct].
  constructor; [|exact (IH Distinct)].
  intro Member. apply EaMergeSound.imem_spec in Member. now rewrite Member in New.
Qed.

Theorem platform_code_sound : forall bound program u code h,
  (forall symbol d, w_decl W symbol = Some d -> symbol < bound) ->
  metadata_budget W bound < e_metadata_fuel E ->
  EaContextTableSound.ContextTableSound W ->
  program_machines program = P.machine_table W ->
  (exists d n ps a r, w_decl W u = Some d /\ d_ast d = ADFunctor n ps a r None) ->
  funbody_of program h = Some code ->
  platform_code E u = Ok code -> P.Family W program u h.
Proof.
  intros bound program u code h Bounded Enough Contexts Machines
    (d & name & params & arguments & results & Decl & Ast) Code Checked.
  unfold platform_code in Checked. cbv zeta in Checked. fold W in Checked.
  apply rbind_ok in Checked as ([inputs needs] & Context & Checked).
  apply rbind_ok in Checked as (no_outputs & NoOutputs & Checked).
  apply rbind_ok in Checked as ([outputs keys] & Provided & Checked).
  apply rbind_ok in Checked as (unique & Unique & Checked).
  apply rbind_ok in Checked as (bindings & Bindings & Checked).
  apply rbind_ok in Checked as (distinct & Distinct & Checked).
  destruct (w_context W u) as [[Ci ns]|] eqn:ContextRead; [|discriminate].
  injection Context as InputEqual NeedsEqual. subst Ci ns.
  destruct (fun_outputs W (e_metadata_fuel E) u) as [|output more] eqn:OutputRead; [|discriminate].
  destruct (inodupb (map provision_id keys)) eqn:Ids; [|discriminate].
  destruct (methods_distinct E keys) eqn:Methods; [|discriminate].
  injection Checked as <-. rewrite (map_ext _ _ (EaSatisfiesSound.rkof_exact outputs)) in Code.
  exists inputs, needs, outputs, keys, bindings. repeat split.
  - specialize (Contexts _ _ _ ContextRead). unfold kind_sym in Contexts. rewrite Decl, Ast in Contexts. exact Contexts.
  - assert (Fuel : bound < e_metadata_fuel E).
    { unfold metadata_budget, metadata_depth in Enough.
      pose proof (Nat.le_max_l bound (largest (fun symbol => match w_decl W symbol with
        Some declaration => declaration_depth (d_ast declaration) | None => 0 end) (seq 0 bound))). lia. }
    intros t Output. apply (proj2 (EaContextFuel.fun_outputs_exact W bound Bounded _ u Fuel t)) in Output.
    now rewrite OutputRead in Output.
  - eapply EaOutputSound.provide_result_sound_at_budget; eassumption.
  - exact Code.
  - eapply rmap_sound; [|exact Bindings]. intros key binding Run.
    apply rbind_ok in Run as (op & Op & Run).
    apply rbind_ok in Run as (sig_check & Signature & Run).
    apply rbind_ok in Run as (machine_check & Fixed & Run).
    apply rbind_ok in Run as (handle_check & Handles & Run).
    destruct (method_primitive E key) as [operation|] eqn:Operation; [|discriminate].
    injection Op as <-. injection Run as <-. rewrite EaSatisfiesSound.rkof_exact.
    exists (encode operation). repeat split; try reflexivity.
    + now apply method_primitive_sound.
    + apply at_key_sound. now destruct (at_key E outputs key operation).
    + eapply machines_fixed_sound; try eassumption. now destruct (machines_fixed E u inputs outputs key).
    + apply handles_read_sound. now destruct (handles_read E outputs key).
  - now apply methods_distinct_sound.
  - intros declaration ps a r b Read Form. rewrite Decl in Read. injection Read as <-.
    rewrite Ast in Form. now injection Form.
  - now apply provision_ids_distinct.
Qed.

Lemma canon_atom_in_class : forall C y, In y (EaCall.class_of C (canon C (TAbs y))).
Proof.
  intros C y. unfold canon, EaCall.class_of. cbn [app_subst].
  destruct (lookup_subst y (c_sub C)) as [t|] eqn:Read.
  - apply in_or_app. right. apply in_flat_map. exists (y,t). split.
    + now apply PublicCallRealize.lookup_subst_in_pair.
    + cbn [fst snd]. rewrite (proj2 (PublicTypeFacts.ty_eqb_spec _ _) eq_refl). now left.
  - apply in_or_app. left. rewrite Read. now left.
Qed.

Lemma fixed_machines_read_sound : forall program main C needs u,
  program_machines program = P.machine_table W -> CtxOf W main C needs ->
  fixed_machines_read E C u = true -> P.FixedMachinesRead program C u.
Proof.
  intros program main C needs u Machines Ctx Checked fx Found symbol t Machine Fixed.
  unfold fixed_machines_read in Checked. rewrite EaSatisfiesSound.cfun_exact, Found in Checked.
  assert (Provided : In (IFun u, ProvidedFunctor fx) (c_provisions C)).
  { apply PublicTypeRead.nlook_in in Found. unfold c_funs in Found.
    apply in_flat_map in Found as [[k p] [Member Row]].
    destruct k, p; cbn in Row; try contradiction. destruct Row as [Equal|[]]. injection Equal as <- <-. exact Member. }
  destruct (PublicCallRealize.ctx_fun_fixed W main C needs Ctx u fx Provided) as [_ [_ Unique]].
  assert (FixedRead : lookup_subst symbol fx = Some t).
  { assert (Domain : In symbol (map fst fx)) by (change symbol with (fst (symbol,t)); now apply in_map).
    destruct (lookup_subst symbol fx) as [read|] eqn:Lookup; [|now apply PublicCallRealize.lookup_subst_in in Domain].
    f_equal. eapply Unique; [eapply PublicCallRealize.lookup_subst_in_pair; exact Lookup|exact Fixed]. }
  rewrite Machines in Machine. fold W in Checked.
  pose proof (proj1 (forallb_forall _ _) Checked symbol Machine) as Row. cbv beta zeta in Row.
  rewrite FixedRead in Row. apply andb_true_iff in Row as [Check Members].
  apply andb_true_iff in Check as [Reading Atomic].
  apply PublicTypeFacts.ty_eqb_spec in Reading. split; [exact Reading|]. split.
  - destruct (canon C (TAbs symbol)); try discriminate. eexists. reflexivity.
  - intros y Equal. pose proof (canon_atom_in_class C y) as Member. rewrite Equal in Member.
    apply forallb_forall with (x := y) in Members; [|exact Member]. now apply Nat.eqb_eq in Members.
Qed.

Lemma emit_fun_code_records : forall code before slot after,
  emit_fun_code code before = Ok (slot, after) ->
  slot_index slot < es_next after /\
  EaCodeAllocationSound.read_code after FunctorTable (slot_index slot) = Some code.
Proof.
  intros code [] slot after Run. cbv [emit_fun_code mbind reserve_code fresh write_fun_code set_fun_body ret] in Run.
  injection Run as <- <-. split; [cbn; lia|apply WorldSound.nget_nset_same].
Qed.

Theorem platform_functor_sound : forall bound program main C needs u before value after,
  (forall symbol d, w_decl W symbol = Some d -> symbol < bound) ->
  metadata_budget W bound < e_metadata_fuel E -> EaContextTableSound.ContextTableSound W ->
  program_machines program = P.machine_table W -> CtxOf W main C needs ->
  (forall h code, h < es_next after -> EaCodeAllocationSound.read_code after FunctorTable h = Some code -> funbody_of program h = Some code) ->
  platform_entry E C (IFun u) before = Ok (value, after) -> P.Provides W program C (IFun u) value.
Proof.
  intros bound program main C needs u before value after Bounded Enough Contexts Machines Ctx Codes Run.
  unfold platform_entry, mbind, ret, fail in Run. cbv zeta in Run. fold W in Run.
  destruct (w_decl W u) as [declaration|] eqn:Decl; [|discriminate].
  destruct (platform_modb E (d_mod declaration)) eqn:ModuleOk; [|discriminate].
  destruct (d_ast declaration) as [| | | | | | | |name params arguments results body] eqn:Ast; try discriminate.
  destruct body; try discriminate. destruct (fixed_machines_read E C u) eqn:MachinesRead; [|discriminate].
  destruct (platform_code E u) as [code|] eqn:Code; [|discriminate].
  change ((match emit_fun_code code before with
    | Ok (slot, state) => Ok (FunctorClosure (slot_index slot) None empty_environment None, state)
    | Err message => Err message end) = Ok (value, after)) in Run.
  destruct (emit_fun_code code before) as [[slot state]|] eqn:Emit; [|discriminate].
  injection Run as <- <-. destruct (emit_fun_code_records _ _ _ _ Emit) as [Below Stored].
  pose proof (Codes _ _ Below Stored) as BodyStored.
  split; [split|].
  - eapply platform_code_sound; try eassumption. eauto 8.
  - eapply fixed_machines_read_sound; eassumption.
  - eapply P.PEFun; try eassumption. now apply platform_modb_sound. congruence.
Qed.

(* Later entries retain the family bodies installed by earlier entries. *)
Definition PlatformResources (program : program) (s : est) : Prop :=
  forall h code, h < es_next s ->
    EaCodeAllocationSound.read_code s FunctorTable h = Some code ->
    funbody_of program h = Some code.

Lemma platform_entries_provide : forall bound program main C needs required before entries after,
  (forall symbol d, w_decl W symbol = Some d -> symbol < bound) ->
  metadata_budget W bound < e_metadata_fuel E -> EaContextTableSound.ContextTableSound W ->
  program_machines program = P.machine_table W -> CtxOf W main C needs ->
  PlatformResources program after ->
  mmap (fun k => let* value := platform_entry E C (rkof C k) in ret (rkof C k, value)) required before = Ok (entries, after) ->
  Forall (fun q => P.Provides W program C (fst q) (snd q)) entries.
Proof.
  intros bound program main C needs required before entries after Bounded Enough Contexts Machines Ctx.
  revert before entries after. induction required as [|k required IH]; intros before entries after Resources Run.
  - injection Run as <- <-. constructor.
  - cbn [mmap] in Run. apply EaControlSound.mbind_ok in Run as (row & middle & Head & Run).
    apply EaControlSound.mbind_ok in Head as (value & state & Entry & Head).
    injection Head as RowEqual MiddleEqual. subst row middle.
    apply EaControlSound.mbind_ok in Run as (tail & finish & Tail & Run).
    injection Run as EntriesEqual FinishEqual. subst entries finish.
    constructor; [|eapply IH; eassumption]. cbn [fst snd].
    destruct k as [ty|v|f|receiver method args|u]; cbn [rkof] in *; try discriminate Entry.
    + exact (proj2 (platform_value_sound C v before value state Entry) program).
    + eapply platform_function_sound. exact Entry.
    + eapply platform_functor_sound; [exact Bounded|exact Enough|exact Contexts|exact Machines|exact Ctx| |exact Entry].
      assert (Transition : EaCodeEffects.CodeTransition state after).
      { eapply EaCodeEffects.code_effect_mmap; [|exact Tail]. intros key.
        apply EaCodeEffects.code_effect_bind; [apply EaAssemblyCodeEffects.platform_entry_code_effect|].
        intro provided. apply EaCodeEffects.code_effect_return. }
      intros h code Below Read. apply (Resources h code).
      * pose proof (EaCodeEffects.counter_grows _ _ Transition). lia.
      * now rewrite (EaCodeEffects.old_slots_preserved _ _ Transition FunctorTable h Below).
Qed.

Theorem main_environment_provides : forall bound program main before after,
  (forall symbol d, w_decl W symbol = Some d -> symbol < bound) ->
  metadata_budget W bound < e_metadata_fuel E -> EaContextTableSound.ContextTableSound W ->
  kind_sym W main = Some DKFn -> program_machines program = P.machine_table W ->
  PlatformResources program after ->
  main_environment E main before = Ok (program_environment program, after) ->
  exists C needs, CtxOf W main C needs /\
    Forall (fun q => P.Provides W program C (fst q) (snd q)) (provision_values (program_environment program)).
Proof.
  intros bound program main before after Bounded Enough Contexts Kind Machines Resources Run.
  unfold main_environment in Run. apply EaControlSound.mbind_ok in Run as ([C needs] & middle & Context & Run).
  fold W in Context. destruct (w_context W main) as [[formal required]|] eqn:Read; [|discriminate Context].
  injection Context as ContextEqual NeedsEqual StateEqual. subst formal required before.
  pose proof (Contexts main C needs Read) as Ctx. rewrite Kind in Ctx.
  apply EaControlSound.mbind_ok in Run as (provisions & finish & Entries & Run).
  injection Run as Environment State. subst finish. rewrite <- Environment. cbn [provision_values].
  exists C, needs. split; [exact Ctx|].
  exact (platform_entries_provide bound program main C needs needs middle provisions after
    Bounded Enough Contexts Machines Ctx Resources Entries).
Qed.

End Entries.

Section Assembly.
Variable E : env.
Let W := e_W E.

Lemma assemble_platform_resources : forall bound entry before program after,
  EaMain.assemble E bound entry before = Ok (program, after) ->
  kind_sym W (program_main program) = Some DKFn /\
  program_machines program = P.machine_table W /\
  exists start, main_environment E (program_main program) start = Ok (program_environment program, after).
Proof.
  intros bound entry before program after Run. unfold EaMain.assemble, EaMain.finish_assembly in Run.
  do 2 (apply EaControlSound.mbind_ok in Run as (_ & ?state & _ & Run)).
  apply EaControlSound.mbind_ok in Run as (main & looked_up & _ & Run).
  apply EaControlSound.mbind_ok in Run as (kind_result & checked_kind & Kind & Run).
  assert (MainKind : kind_sym W main = Some DKFn).
  { unfold ast_of in Kind. fold W in Kind. unfold kind_sym.
    destruct (w_decl W main) as [d|] eqn:Decl; [|discriminate Kind].
    cbn [option_map] in Kind. destruct (d_ast d) as [| | | | | |form name params result body| |] eqn:Ast; try discriminate Kind.
    destruct form; try discriminate Kind. reflexivity. }
  clear Kind kind_result.
  apply EaControlSound.mbind_ok in Run as (_ & start & _ & Run).
  apply EaControlSound.mbind_ok in Run as (environment & finish & Initial & Run).
  apply EaControlSound.mbind_ok in Run as ([C ns] & context_state & Context & Run).
  fold W in Context. destruct (w_context W main) as [[formal required]|] eqn:Read; [|discriminate Context].
  injection Context as <- <- <-.
  repeat apply EaControlSound.mbind_ok in Run as (?value & ?state & ?step & Run).
  destruct (bool_syms E) as [[boolean truth] falsity].
  repeat apply EaControlSound.mbind_ok in Run as (?value & ?state & ?step & Run).
  repeat match goal with
  | Step : pure ?result ?state = Ok (?value, ?state') |- _ =>
      destruct result; [injection Step as <- <-|discriminate Step]
  | Step : (if ?condition then ret tt else fail ?message) ?state = Ok (?value, ?state') |- _ =>
      rewrite (EaCodeAllocationSound.successful_check_state condition message state value state' Step) in *;
      clear Step; try clear state'
  | Step : get ?state = Ok (?saved, ?state') |- _ =>
      injection Step as <- <-
  end.
  injection Run as <- <-. cbn [program_main program_machines program_unsupported program_environment]. fold W.
  split; [exact MainKind|]. repeat split; try reflexivity. now exists start.
Qed.

Theorem assemble_platform_provides : forall bound entry program after,
  (forall symbol d, w_decl W symbol = Some d -> symbol < bound) ->
  metadata_budget W bound < e_metadata_fuel E -> EaContextTableSound.ContextTableSound W ->
  EaMain.assemble E bound entry (init_state bound) = Ok (program, after) ->
  exists C needs, CtxOf W (program_main program) C needs /\
    Forall (fun q => P.Provides W program C (fst q) (snd q)) (provision_values (program_environment program)).
Proof.
  intros bound entry program after Bounded Enough Contexts Run.
  destruct (assemble_platform_resources _ _ _ _ _ Run) as [Kind [Machines [start Initial]]].
  destruct (EaAssemblyCodeEffects.assembled_code_lookup_exact _ _ _ _ _ Run) as [_ Codes].
  apply (main_environment_provides E bound program (program_main program) start after
    Bounded Enough Contexts Kind Machines); [|exact Initial].
  intros h code _ Read. now rewrite Codes.
Qed.

End Assembly.

Theorem elaborate_platform_provides : forall strategy bundle W program,
  EaMain.elaborate_with strategy bundle = Ok (W, program) ->
  exists C needs, CtxOf W (program_main program) C needs /\
    Forall (fun q => P.Provides W program C (fst q) (snd q)) (provision_values (program_environment program)).
Proof.
  intros strategy bundle W program Run.
  pose proof (EaContextTableSound.elaborate_with_context_table_sound _ _ _ _ Run) as Contexts.
  unfold EaMain.elaborate_with, EaMain.elaborate_with_metadata in Run.
  destruct (EaMain.bundle_world_r bundle) as [[world bound]|message] eqn:World; [|discriminate Run].
  repeat match type of Run with
  | context [match ?step with _ => _ end] => destruct step eqn:?; try discriminate Run
  end.
  injection Run as <- <-.
  match goal with
  | Assembled : EaMain.assemble ?env bound _ _ = Ok (_, _) |- _ =>
      eapply (assemble_platform_provides env bound); [| |exact Contexts|exact Assembled]
  end.
  - exact (EaWorldSound.bundle_world_r_bound _ _ _ World).
  - cbn [e_W e_metadata_fuel]. rewrite EaMetadataSound.metadata_budget_context. apply EaMetadataSound.metadata_fuel_sufficient.
Qed.
