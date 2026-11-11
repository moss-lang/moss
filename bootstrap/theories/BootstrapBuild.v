From Stdlib Require Import List String.
From NM Require Audit AuditBase EaBase EaContextTableSound EaMain EaPlatform EaSatisfiesSound EaWorldSound
  SnapshotLoader Sx.
From NM Require Import EaControlSound.

Module L := Audit.Loader.
Module E := Audit.Semantics.Elaboration.

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

(* Successful assembly selects the source entry and gives its environment
   to the result. The platform proof can use the successful environment
   calculation directly. *)
Theorem assemble_entry : forall env bound entry before program after,
  EaMain.assemble env bound entry before = Ok (program, after) ->
  w_scope (e_W env) entry NSName "main"%string = Some (ESym (program_main program)) /\
  (exists d n rt body, w_decl (e_W env) (program_main program) = Some d /\
    d_ast d = ADFn RPlain n [] rt (Some body)) /\
  program_machines program = AuditBase.Platform.machine_table (e_W env) /\
  exists start finish, EaPlatform.main_environment env (program_main program) start =
    Ok (program_environment program, finish).
Proof.
  intros env bound entry before program after Run.
  unfold EaMain.assemble in Run.
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
  apply mbind_ok in Run as ([context needs] & after_context & _ & Run).
  destruct (EaPlatform.bool_syms env) as [[boolean truth] falsity].
  repeat apply mbind_ok in Run as (?value & ?state & ?step & Run).
  injection Run as <- _.
  split; [reflexivity|split; [exact Main|]].
  split; [reflexivity|]. exists after_arity, after_environment. exact Env.
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
  do 3 eexists. split; [|eassumption]. reflexivity.
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
  destruct (elaborate_uses_assemble _ _ _ _ Run) as (env & bound & after & <- & Assembled).
  destruct (assemble_entry _ _ _ _ _ _ Assembled) as (Scope & Main & Machines & start & finish & EnvironmentRun).
  split; [exact Scope|split; [exact Main|]].
  assert (Kind : kind_sym (e_W env) (program_main program) = Some DKFn).
  { destruct Main as (d & n & rt & body & Declaration & Shape).
    unfold kind_sym. rewrite Declaration, Shape. reflexivity. }
  rewrite Machines. eapply main_environment_typed; eassumption.
Qed.
