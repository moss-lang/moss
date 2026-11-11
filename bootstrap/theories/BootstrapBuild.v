From Stdlib Require Import List String Arith Lia.
From NM Require Audit AuditBase EaBase EaContextTableSound EaCodeAllocationSound EaAssemblyCodeEffects EaExpand EaExpansionSound EaLayoutSound EaMain EaPlatform EaPlatformSound EaSatisfiesSound EaTypes EaTypesSound EaWorldSound
  SnapshotLoader Sx.
From NM Require Import EaControlSound.
From NM Require EaPlatformEntrySound.

Module L := Audit.Loader.
Module E := Audit.Semantics.Elaboration.
Module P := AuditBase.Platform.

(* The first two stages of the driver build the same world as the audit
   semantics on the bundle loaded from source. *)
Theorem load_elaborate_world : forall read root prelude entry bundle world program,
  SnapshotLoader.load_from read root prelude entry = Sx.Good bundle ->
  EaMain.elaborate bundle = EaBase.Ok (world, program) ->
  L.Bundled read root prelude entry bundle /\ E.WorldOf bundle world.
Proof.
  intros read root prelude entry bundle world program Loaded Elaborated.
  split.
  - exact (SnapshotLoader.load_from_sound _ _ _ _ _ Loaded).
  - eapply EaWorldSound.elaborate_with_world_sound. exact Elaborated.
Qed.

Import ListNotations Audit.Input Audit.Semantics E EaBase.

Definition InstalledBool (W : world) (program : program) : Prop :=
  (LangItem W LIBool (program_bool program) /\ LangItem W LITrue (program_true program) /\
    LangItem W LIFalse (program_false program)) \/
  ((forall symbol, ~ LangItem W LIBool symbol) /\
    program_bool program = 0 /\ program_true program = 0 /\ program_false program = 0).

(* Successful assembly selects the source entry and gives its environment
   to the result. The platform proof can use the successful environment
   calculation directly. *)
Theorem assemble_entry : forall env bound entry before program after,
  EaContextTableSound.ContextTableSound (e_W env) ->
  EaMain.assemble env bound entry before = Ok (program, after) ->
  w_scope (e_W env) entry NSName "main"%string = Some (ESym (program_main program)) /\
  (exists d n rt body, w_decl (e_W env) (program_main program) = Some d /\
    d_ast d = ADFn RPlain n [] rt (Some body)) /\
  (exists start finish, EaPlatform.main_environment env (program_main program) start =
    Ok (program_environment program, finish)) /\
  P.Tables (e_W env) (program_machines program) (program_casts program) /\
  P.EntryPoint (e_W env) program /\ LangItemsFormed (e_W env) /\ InstalledBool (e_W env) program /\
  EaExpand.finite_layouts (e_W env) (e_fuel env) bound = Ok tt /\
  EaExpand.expansion_ok (e_W env) (e_fuel env) program (es_fns after) (es_funs after)
    bound (es_next after) = true.
Proof.
  intros env bound entry before program after Contexts Run.
  unfold EaMain.assemble, EaMain.finish_assembly in Run.
  apply mbind_ok in Run as (_ & after_functions & _ & Run).
  apply mbind_ok in Run as (_ & after_constants & _ & Run).
  apply mbind_ok in Run as (main & after_lookup & Lookup & Run).
  destruct (w_scope (e_W env) entry NSName "main"%string) as [[symbol|module_path]|]
    eqn:Scope; try discriminate Lookup.
  injection Lookup as <- _.
  apply mbind_ok in Run as (kind_checked & after_kind & Kind & Run).
  apply mbind_ok in Run as (arity_checked & after_arity & Arity & Run).
  assert (Main : exists d n rt body, w_decl (e_W env) symbol = Some d /\
    d_ast d = ADFn RPlain n [] rt (Some body)).
  {
    unfold ast_of in Kind, Arity.
    destruct (w_decl (e_W env) symbol) as [d|] eqn:Declared; [|discriminate Kind].
    cbn [option_map] in Kind, Arity.
    destruct (d_ast d) as [| | | | | |receiver name parameters result body| |]
      eqn:Shape; try discriminate Kind.
    destruct receiver; try discriminate Kind.
    destruct body as [body|]; [|discriminate Kind].
    destruct parameters as [|parameter parameters]; [|discriminate Arity].
    exists d, name, result, body. split; [reflexivity|exact Shape].
  }
  apply mbind_ok in Run as (environment & after_environment & Env & Run).
  apply mbind_ok in Run as ([context needs] & after_context & Read & Run).
  destruct (w_context (e_W env) symbol) as [[C ns]|] eqn:Context; [|discriminate Read].
  injection Read as <- <- <-.
  apply mbind_ok in Run as (signature & after_signature & Read & Run).
  unfold pure in Read.
  destruct (EaTypes.sig_of (e_W env) (e_fuel env) symbol (c_ty C) None)
    as [sg|message] eqn:Signature; [|discriminate Read].
  injection Read as <- <-.
  apply mbind_ok in Run as (result_checked & after_result & Read & Run).
  destruct (sg_params sg) as [|parameter params] eqn:Parameters; [|discriminate Read].
  destruct (EaPlatform.entry_result env (e_fuel env) (sg_ret sg)) eqn:Result;
    [|discriminate Read].
  clear Read.
  apply mbind_ok in Run as (layouts_checked & after_layouts & Read & Run).
  destruct (EaPlatform.layouts env) as [[]|message] eqn:Layouts; [|discriminate Read].
  clear Read.
  apply mbind_ok in Run as (language_checked & after_language & Read & Run).
  destruct (EaPlatform.lang_items_formed env) as [[]|message] eqn:Language; [|discriminate Read].
  apply mbind_ok in Run as (_ & after_casts & _ & Run).
  apply mbind_ok in Run as (finite_checked & after_finite & FiniteRun & Run).
  destruct (EaExpand.finite_layouts (e_W env) (e_fuel env) bound) as [[]|message] eqn:Finite;
    [|discriminate FiniteRun].
  apply mbind_ok in Run as (snapshot & after_get & ReadSnapshot & Run).
  inversion ReadSnapshot; subst snapshot after_get.
  destruct (EaPlatform.bool_syms env) as [[boolean truth] falsity] eqn:Symbols.
  apply mbind_ok in Run as (checked & after_expansion & Check & Run).
  lazymatch type of Check with
  | context [EaExpand.expansion_ok ?W ?fuel ?P ?fns ?funs ?bound ?next] =>
      destruct (EaExpand.expansion_ok W fuel P fns funs bound next) eqn:Expansion;
        [|discriminate Check]
  end.
  inversion Check; subst checked after_expansion.
  injection Run as <- <-.
  split; [reflexivity|split; [exact Main|]].
  split; [exists after_arity, after_environment; exact Env|].
  split; [now apply EaPlatformSound.tables_sound|].
  pose proof (EaPlatformSound.lang_items_formed_sound _ Language) as LanguageFormed.
  split; [|split; [exact LanguageFormed|]].
  2: {
    split; [|split; [reflexivity|exact Expansion]].
    eapply EaPlatformSound.bool_symbols_sound; [exact LanguageFormed|exact Symbols].
  }
  assert (KindMain : kind_sym (e_W env) symbol = Some DKFn).
  { destruct Main as (d & n & rt & body & Declaration & Shape).
    unfold kind_sym. rewrite Declaration, Shape. reflexivity. }
  exists C, ns, sg. split.
  - specialize (Contexts symbol C ns Context). now rewrite KindMain in Contexts.
  - split; [eapply EaTypesSound.sig_of_sound; exact Signature|].
    split; [exact Parameters|eapply EaPlatformSound.entry_result_sound; exact Result].
Qed.

Lemma platform_entries_keys : forall env C needs before entries after,
  mmap (fun k => let* value := EaPlatform.platform_entry env C (rkof C k) in
                   ret (rkof C k, value)) needs before = Ok (entries, after) ->
  map fst entries = map (rkey_of C) needs.
Proof.
  intros env C needs. induction needs as [|k rest IH]; intros before entries after Run.
  - injection Run as <- _. reflexivity.
  - cbn [mmap] in Run.
    apply mbind_ok in Run as (entry & middle & Head & Run).
    apply mbind_ok in Head as (value & finish & _ & Head).
    injection Head as <- <-.
    apply mbind_ok in Run as (tail & last & Tail & Run).
    injection Run as <- _. cbn. rewrite EaSatisfiesSound.rkof_exact, (IH _ _ _ Tail). reflexivity.
Qed.

Theorem main_environment_typed : forall env main before initial after,
  EaContextTableSound.ContextTableSound (e_W env) ->
  kind_sym (e_W env) main = Some DKFn ->
  EaPlatform.main_environment env main before = Ok (initial, after) ->
  exists C needs types provisions,
    CtxOf (e_W env) main C needs /\
    InitialTypes (AuditBase.Platform.machine_table (e_W env)) C types /\
    initial = Environment types provisions /\ map fst provisions = map (rkey_of C) needs.
Proof.
  intros env main before initial after Contexts Kind Run.
  unfold EaPlatform.main_environment in Run.
  apply mbind_ok in Run as ([C needs] & middle & Read & Run).
  destruct (w_context (e_W env) main) as [[formal required]|] eqn:Found;
    [|discriminate Read].
  injection Read as <- <- <-.
  apply mbind_ok in Run as (provisions & finish & Entries & Run).
  injection Run as <- _.
  exists formal, required,
    (map (fun q => (fst q,
      app_subst (machine_subst (AuditBase.Platform.machine_table (e_W env))) (snd q)))
      (c_ty formal)), provisions.
  split.
  - specialize (Contexts main formal required Found). rewrite Kind in Contexts. exact Contexts.
  - repeat split. eapply platform_entries_keys; eassumption.
Qed.

Lemma elaborate_uses_assemble : forall strategy bundle world program,
  EaMain.elaborate_with strategy bundle = Ok (world, program) ->
  exists env bound after,
    e_W env = world /\
    (forall symbol d, w_decl world symbol = Some d -> symbol < bound) /\
    EaMain.decls_form env bound = Ok tt /\
    EaMain.assemble env bound (b_entry bundle) (init_state bound) = Ok (program, after).
Proof.
  intros strategy bundle world program Run.
  unfold EaMain.elaborate_with, EaMain.elaborate_with_metadata in Run.
  destruct (EaMain.bundle_world_r bundle) as [[source_world bound]|message] eqn:World;
    [|discriminate].
  repeat match type of Run with
  | context [match ?step with _ => _ end] => destruct step eqn:?; try discriminate
  end.
  injection Run as <- <-.
  match goal with
  | Assembled : EaMain.assemble ?env bound _ _ = Ok (_, ?after) |- _ =>
      exists env, bound, after
  end.
  split; [reflexivity|]. split.
  - intros symbol d Declared. eapply EaWorldSound.bundle_world_r_bound;
      [exact World|exact Declared].
  - match goal with
    | Declarations : EaMain.decls_form _ _ = Ok ?u |- _ => destruct u
    end.
    split; eassumption.
Qed.

Theorem elaborate_main_environment : forall strategy bundle world program,
  EaMain.elaborate_with strategy bundle = Ok (world, program) ->
  w_scope world (b_entry bundle) NSName "main"%string = Some (ESym (program_main program)) /\
  (exists d n rt body, w_decl world (program_main program) = Some d /\
    d_ast d = ADFn RPlain n [] rt (Some body)) /\
  exists C needs types provisions,
    CtxOf world (program_main program) C needs /\
    InitialTypes (program_machines program) C types /\
    program_environment program = Environment types provisions /\
    map fst provisions = map (rkey_of C) needs.
Proof.
  intros strategy bundle world program Run.
  pose proof (EaContextTableSound.elaborate_with_context_table_sound _ _ _ _ Run) as Contexts.
  destruct (elaborate_uses_assemble _ _ _ _ Run) as (env & bound & after & <- & Bounded & Declarations & Assembled).
  destruct (assemble_entry _ _ _ _ _ _ Contexts Assembled)
    as (Scope & Main & (start & finish & EnvironmentRun) & Tables & _).
  split; [exact Scope|split; [exact Main|]].
  assert (Kind : kind_sym (e_W env) (program_main program) = Some DKFn).
  { destruct Main as (d & n & rt & body & Declaration & Shape).
    unfold kind_sym. rewrite Declaration, Shape. reflexivity. }
  rewrite (proj1 (proj2 Tables)). eapply main_environment_typed; eassumption.
Qed.

Theorem elaborate_platform_sound : forall strategy bundle world program,
  EaMain.elaborate_with strategy bundle = Ok (world, program) ->
  P.Tables world (program_machines program) (program_casts program) /\
  P.EntryPoint world program /\ LangItemsFormed world /\ InstalledBool world program.
Proof.
  intros strategy bundle world program Run.
  pose proof (EaContextTableSound.elaborate_with_context_table_sound _ _ _ _ Run) as Contexts.
  destruct (elaborate_uses_assemble _ _ _ _ Run) as (env & bound & after & <- & Bounded & Declarations & Assembled).
  destruct (assemble_entry _ _ _ _ _ _ Contexts Assembled) as (_ & _ & _ & Tables & Entry & Language & Bool & _ & _).
  exact (conj Tables (conj Entry (conj Language Bool))).
Qed.

(* Assembly checks the platform ABI and tables, then records the checked
   provisions for the exact context that elaboration chose for main. *)
Theorem elaborate_linked : forall strategy bundle world program,
  EaMain.elaborate_with strategy bundle = Ok (world, program) -> P.Linked world program.
Proof.
  intros strategy bundle world program Run.
  destruct (elaborate_platform_sound _ _ _ _ Run) as [Tables [Entry _]].
  destruct (elaborate_main_environment _ _ _ _ Run)
    as (_ & _ & C & needs & types & provisions & MainCtx & Initial & Env & _).
  destruct (EaPlatformEntrySound.elaborate_platform_provides _ _ _ _ Run)
    as (provided_context & provided_needs & PlatformCtx & Provided).
  assert (provided_context = C).
  { destruct MainCtx as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & Read).
    destruct PlatformCtx as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & ProvidedRead).
    congruence. }
  subst provided_context. rewrite Env in Provided.
  split; [exact (EaWorldSound.elaborate_with_root _ _ _ _ Run)|].
  split; [exact Entry|split; [exact Tables|]].
  exists C, needs, types, provisions. repeat split; assumption.
Qed.

(* Declaration checking reads each payload in the full context of source
   names. The layout check reads the same payload at its own requirements. *)
Theorem elaborate_finite_layouts : forall strategy bundle world program,
  EaMain.elaborate_with strategy bundle = Ok (world, program) -> FiniteLayouts world.
Proof.
  intros strategy bundle world program Run.
  pose proof (EaWorldSound.elaborate_with_world_sound _ _ _ _ Run) as World.
  pose proof (EaContextTableSound.elaborate_with_context_table_sound _ _ _ _ Run) as Contexts.
  destruct (elaborate_uses_assemble _ _ _ _ Run)
    as (env & bound & after & <- & Bounded & Declarations & Assembled).
  destruct (assemble_entry _ _ _ _ _ _ Contexts Assembled)
    as (_ & _ & _ & _ & _ & _ & _ & Finite & _).
  eapply EaLayoutSound.finite_layouts_sound; [| |exact Finite].
  - intros tag payload Read. inversion Read. eapply Bounded. eassumption.
  - eapply EaLayoutSound.declarations_cover_payload_reads; eassumption.
Qed.

Theorem elaborate_types_bounded : forall strategy bundle world program,
  EaMain.elaborate_with strategy bundle = Ok (world, program) -> TypesBounded world.
Proof.
  intros strategy bundle world program Run.
  pose proof (EaWorldSound.elaborate_with_world_sound _ _ _ _ Run) as World.
  pose proof (EaContextTableSound.elaborate_with_context_table_sound _ _ _ _ Run) as Contexts.
  destruct (elaborate_uses_assemble _ _ _ _ Run)
    as (env & bound & after & <- & Bounded & Declarations & Assembled).
  destruct (assemble_entry _ _ _ _ _ _ Contexts Assembled)
    as (_ & _ & _ & _ & _ & _ & _ & _ & Expansion).
  unfold EaExpand.expansion_ok in Expansion. apply Bool.andb_true_iff in Expansion as [Types _].
  eapply EaExpansionSound.type_graph_sound; [| |exact Types].
  - intros tag payload Read. inversion Read. eapply Bounded. eassumption.
  - eapply EaLayoutSound.declarations_cover_payload_reads; eassumption.
Qed.

(* The expansion certificate checks each code node at its selected
   translation's merge section. *)
Theorem assemble_instantiation_bounded : forall env bound entry program after,
  EaContextTableSound.ContextTableSound (e_W env) ->
  EaMain.assemble env bound entry (init_state bound) = Ok (program, after) ->
  InstantiationsBounded (e_W env) program.
Proof.
  intros env bound entry program after Contexts Run.
  destruct (assemble_entry _ _ _ _ _ _ Contexts Run)
    as (_ & _ & _ & _ & _ & _ & _ & _ & Expansion).
  unfold EaExpand.expansion_ok in Expansion. apply Bool.andb_true_iff in Expansion as [_ Codes].
  destruct (EaAssemblyCodeEffects.assembly_code_tables_bound _ _ _ _ _ Run) as [_ Bounded].
  destruct (EaAssemblyCodeEffects.assembled_code_lookup_exact _ _ _ _ _ Run)
    as [Functions Functors].
  eapply EaExpansionSound.instantiation_graph_sound; [| | |exact Codes].
  - intro f. symmetry. exact (Functions f).
  - intro h. symmetry. exact (Functors h).
  - intros [f|h] Present; apply EaExpansionSound.code_nodes_contain.
    + cbn [CodePresent] in Present. rewrite Functions in Present. cbn [EaCodeAllocationSound.read_code] in Present.
      destruct (NMap.nget (es_fns after) f) as [body|] eqn:Found; [|contradiction].
      split; [|discriminate].
      apply Nat.lt_nge. intro Outside.
      specialize (Bounded f Outside FunctionTable).
      cbn [EaCodeAllocationSound.read_code] in Bounded. congruence.
    + cbn [CodePresent] in Present. rewrite Functors in Present. cbn [EaCodeAllocationSound.read_code] in Present.
      destruct (NMap.nget (es_funs after) h) as [code|] eqn:Found; [|contradiction].
      split; [|discriminate].
      apply Nat.lt_nge. intro Outside.
      specialize (Bounded h Outside FunctorTable).
      cbn [EaCodeAllocationSound.read_code] in Bounded. congruence.
Qed.

Theorem elaborate_with_expansion_bounded : forall strategy bundle world program,
  EaMain.elaborate_with strategy bundle = Ok (world, program) ->
  NoExpandingCycle world program.
Proof.
  intros strategy bundle world program Run. split.
  - eapply elaborate_types_bounded; exact Run.
  - pose proof (EaContextTableSound.elaborate_with_context_table_sound _ _ _ _ Run) as Contexts.
    destruct (elaborate_uses_assemble _ _ _ _ Run)
      as (env & bound & after & <- & _ & _ & Assembled).
    eapply assemble_instantiation_bounded; eassumption.
Qed.
