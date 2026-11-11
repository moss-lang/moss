(* The pass order used by moss-ref and its composition with the compiled
   interpreter. The native caller supplies the compiled lookup tables. *)
From Stdlib Require Import List String.
From NM Require Import Audit BaseFacts.
From NM Require InterpreterCompiled ProgramFastPath ProgramPrune ProgramCapTrim.
Import Audit.Semantics ListNotations.
Open Scope string_scope.

Definition passes : list (string * (program -> program)) :=
  [("fastpath", ProgramFastPath.fastpath);
   ("prune", ProgramPrune.prune);
   ("captrim", ProgramCapTrim.captrim)].

Definition optimize (p : program) : program :=
  fold_left (fun p pass => snd pass p) passes p.

Module Sound (Host : HOST) (M : PUBLIC Host).
Module C := InterpreterCompiled.CloInterpreter M.ReferenceHost M.Reference.
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
