(* The pass order used by moss-ref and its composition with the compiled
   interpreter. The native caller supplies the compiled lookup tables. *)
From Stdlib Require Import List String.
From NM Require AuditBase.
From NM Require Import Audit BaseFacts NatTrie CodeTable.
From NM Require InterpreterCompiled InterpreterFast InterpreterState ProgramFastPath ProgramPrune ProgramCapTrim.
Import AuditBase.Semantics ListNotations.
Open Scope string_scope.

Definition passes : list (string * (program -> program)) :=
  [("fastpath", ProgramFastPath.fastpath);
   ("prune", ProgramPrune.prune);
   ("captrim", ProgramCapTrim.captrim)].

Definition optimize (p : program) : program :=
  fold_left (fun p pass => snd pass p) passes p.

Definition indexed_program_of (p : program) (argv : list (list PrimInt63.int))
    (read : list PrimInt63.int -> option (list PrimInt63.int)) : program_view :=
  let functions := index_rows (program_functions p) in
  let functors := index_rows (program_functors p) in
  ProgramView (tget functions) (tget functors)
    (program_bool p) (program_true p) (program_false p) argv read (program_casts p) (program_machines p).

Lemma indexed_function_lookup : forall p argv read f,
  view_function (indexed_program_of p argv read) f = view_function (program_of p argv read) f.
Proof. intros. apply index_rows_lookup. Qed.

Lemma indexed_functor_lookup : forall p argv read u,
  view_functor (indexed_program_of p argv read) u = view_functor (program_of p argv read) u.
Proof. intros. apply index_rows_lookup. Qed.

Module Sound (Host : HOST) (M : PUBLIC Host).
Module Forced := InterpreterCompiled.ForcePrimitives M.ReferenceHost.
Module C := InterpreterCompiled.CloInterpreter M.ReferenceHost M.Reference Forced.
Module Fast := ProgramFastPath.FastRuns Host M.
Module Prune := ProgramPrune.PruneRuns Host M.
Module Trim := ProgramCapTrim.TrimRuns Host M.

Definition run_compiled (p : program) (interface : M.interface) (initial : M.state)
    (tbl : fsym -> option C.code) : M.partial M.execution :=
  fun fuel => M.observe (C.crun_program (M.runtime p initial) tbl fuel p (interface, initial)).

Theorem run_compiled_agrees : forall p interface initial tbl ftbl fuel,
  C.tbl_ok (M.runtime p initial) tbl ftbl ->
  run_compiled p interface initial tbl fuel =
  M.observe (M.Reference.run_main fuel (M.runtime p initial)
    (program_main p) (program_environment p) (interface, initial)).
Proof.
  intros p interface initial tbl ftbl fuel Tables.
  unfold run_compiled, C.crun_program.
  rewrite (C.crun_main_reference _ _ _ _ _ _ _ Tables). reflexivity.
Qed.

Theorem run_compiled_sound : forall p interface initial tbl ftbl fuel result,
  C.tbl_ok (M.runtime p initial) tbl ftbl ->
  run_compiled p interface initial tbl fuel = Some result ->
  M.Runs p interface initial result.
Proof.
  intros p interface initial tbl ftbl fuel result Tables Run.
  exists fuel. rewrite <- (run_compiled_agrees p interface initial tbl ftbl fuel Tables).
  exact Run.
Qed.

Theorem runs_optimized : forall p interface initial result,
  Prune.code_blind interface ->
  (forall code final, ~ M.Runs p interface initial (M.Result (M.Wrong code) final)) ->
  M.Runs (optimize p) interface initial result <-> M.Runs p interface initial result.
Proof.
  intros p interface initial result Blind Safe. unfold optimize. cbn [fold_left passes snd].
  rewrite (@Trim.runs_trimmed _ _ _ _ Blind).
  rewrite (@Prune.runs_pruned _ _ _ _ Blind).
  - apply Fast.runs_fastpath; assumption.
  - intros code final Run. apply (Safe code final).
    apply (proj1 (@Fast.runs_fastpath p interface initial _ Blind Safe)). exact Run.
Qed.

Theorem run_optimized_sound : forall p interface initial tbl ftbl fuel result,
  Prune.code_blind interface ->
  (forall code final, ~ M.Runs p interface initial (M.Result (M.Wrong code) final)) ->
  C.tbl_ok (M.runtime (optimize p) initial) tbl ftbl ->
  run_compiled (optimize p) interface initial tbl fuel = Some result ->
  M.Runs p interface initial result.
Proof.
  intros p interface initial tbl ftbl fuel result Blind Safe Tables Run.
  apply (proj1 (runs_optimized p interface initial result Blind Safe)).
  eapply run_compiled_sound; eassumption.
Qed.
End Sound.

(* The extracted driver keeps only the primitive-managed state in the
   interpreter. Public observations add the read-only part of the world. *)
Module Native (Host : HOST) (M : PUBLIC Host).
Module Model := Sound Host M.

Module RawHost <: PRIMITIVES.
  Definition mstate := Host.state.
  Definition run_prim := @BaseCall.call mstate Host.primitive.
End RawHost.

Module RawReference := Interpreter RawHost.
Module Deferred <: InterpreterCompiled.DEFERRED_PRIMITIVES RawHost.
  Definition run_prim_deferred P operation site :=
    DeferredBaseCall.call Host.primitive P operation (fun _ => InterpreterFast.force_held site).
  Theorem run_prim_deferred_agrees : forall P operation site values state,
    run_prim_deferred P operation site values state =
    RawHost.run_prim P operation (InterpreterFast.force_held site) values state.
  Proof. intros. apply DeferredBaseCall.call_agrees. Qed.
End Deferred.
Module C := InterpreterCompiled.CloInterpreter RawHost RawReference Deferred.
Module State := InterpreterState.StateSimulation RawHost M.ReferenceHost RawReference M.Reference.

Definition lift_state (initial : M.state) (external : Host.state) : M.ReferenceHost.mstate :=
  (M.default_interface, M.World (M.arguments initial) (M.read_file initial) external).

Definition native_view (p : program) (initial : M.state) : program_view :=
  indexed_program_of p (M.arguments initial) (M.read_file initial).

Theorem native_tables_ok : forall p initial tbl ftbl,
  (forall f, tbl f = CodeTable.get (C.compile_functions (native_view p initial) tbl ftbl p) f) ->
  (forall u, ftbl u = CodeTable.get (C.compile_functors (native_view p initial) tbl ftbl p) u) ->
  C.tbl_ok (native_view p initial) tbl ftbl.
Proof.
  intros p initial tbl ftbl Functions Functors.
  apply (C.materialized_tables_ok (native_view p initial) p); auto.
  - intros. apply indexed_function_lookup.
  - intros. apply indexed_functor_lookup.
Qed.

Lemma primitive_commutes : Model.Prune.code_blind M.default_interface ->
  forall p initial operation site values state,
  M.ReferenceHost.run_prim (M.runtime p initial) operation site values (lift_state initial state) =
    let '(out, next) := RawHost.run_prim (native_view p initial) operation site values state in
    (out, lift_state initial next).
Proof.
  intros Blind p initial operation site values state.
  unfold M.ReferenceHost.run_prim, Base.call, RawHost.run_prim, BaseCall.call.
  unfold M.ReferenceHost.raw_primitive, lift_state.
  cbn [M.default_interface M.supports M.primitive M.external M.arguments M.read_file].
  assert (Primitive : forall arguments, Host.primitive (native_view p initial) operation arguments state =
    Host.primitive (M.runtime p initial) operation arguments state).
  { intros. exact (Blind (M.runtime p initial) (native_view p initial)
      eq_refl eq_refl eq_refl eq_refl eq_refl eq_refl operation arguments state). }
  rewrite Primitive.
  destruct (decode operation); destruct (Host.primitive _ _ _ _); reflexivity.
Qed.

Definition run (p : program) (initial : M.state) (tbl : fsym -> option C.code)
    : M.partial M.execution :=
  fun fuel =>
    M.observe (State.lift_eval (lift_state initial)
      (C.crun_program (native_view p initial) tbl fuel p (M.external initial))).

Theorem run_agrees : forall p initial tbl ftbl fuel,
  Model.Prune.code_blind M.default_interface ->
  C.tbl_ok (native_view p initial) tbl ftbl ->
  run p initial tbl fuel =
  M.observe (M.Reference.run_main fuel (M.runtime p initial)
    (program_main p) (program_environment p) (M.default_interface, initial)).
Proof.
  intros p initial tbl ftbl fuel Blind Tables.
  unfold run, C.crun_program.
  rewrite (C.crun_main_reference _ _ _ _ _ _ _ Tables).
  rewrite <- (State.run_main_commutes (lift_state initial) (native_view p initial) (M.runtime p initial)).
  - destruct initial. reflexivity.
  - intros. symmetry. apply indexed_function_lookup.
  - intros. symmetry. apply indexed_functor_lookup.
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - apply primitive_commutes. exact Blind.
Qed.

Theorem run_sound : forall p initial tbl ftbl fuel result,
  Model.Prune.code_blind M.default_interface ->
  C.tbl_ok (native_view p initial) tbl ftbl ->
  run p initial tbl fuel = Some result ->
  M.Runs p M.default_interface initial result.
Proof.
  intros p initial tbl ftbl fuel result Blind Tables Run.
  exists fuel. rewrite <- (run_agrees p initial tbl ftbl fuel Blind Tables). exact Run.
Qed.

Theorem run_optimized_sound : forall p initial tbl ftbl fuel result,
  Model.Prune.code_blind M.default_interface ->
  (forall code final, ~ M.Runs p M.default_interface initial (M.Result (M.Wrong code) final)) ->
  C.tbl_ok (native_view (optimize p) initial) tbl ftbl ->
  run (optimize p) initial tbl fuel = Some result ->
  M.Runs p M.default_interface initial result.
Proof.
  intros p initial tbl ftbl fuel result Blind Safe Tables Run.
  apply (proj1 (Model.runs_optimized p M.default_interface initial result Blind Safe)).
  eapply run_sound; eassumption.
Qed.
End Native.
