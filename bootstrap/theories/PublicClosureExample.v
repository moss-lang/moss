(* A closed runtime fixture for the common-target compiler experiment.
   It has no pending compiled-table premise and makes no source Builds or
   default-host safety claim. The checked handle is an input to the fixture;
   this exercises cast checking, not allocation or heap-law verification. *)
From Stdlib Require Import List Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit PublicClosurePrototype.
Import AuditBase.Semantics ListNotations.

Module Prims <: PRIMITIVES.
Definition mstate := unit.
Definition run_prim (_ : program_view) (_ : op) (_ : option ty) (_ : list value) (st : unit) :=
  (Trap 0, st).
End Prims.
Module C := PublicClosurePrototype.Compiler Prims.
Module R := C.R.

Definition captured_handle := VRef 0 (Some (0, TMachine 0)).
Definition replacement_handle := VRef 1 (Some (0, TMachine 0)).
Definition provider_bindings := [BType 2 (TMachine 0); BValue 3 (EVal 1)].
Definition provider_code := FunctorCode [] [] [] [ITy 2; IVal 3] provider_bindings [IVal 3].
Definition runtime := ProgramView (fun _ => None)
  (fun h => if Nat.eqb h 20 then Some provider_code else None)
  0 0 0 [] (fun _ => None) [(11, CastSpec 0 9 None)] [].
Definition initial := Environment [(2, TMachine 1)]
  [(IVal 1, ValueProvision captured_handle); (IVal 4, ValueProvision replacement_handle)].

(* Capture seed, rebind the caller's seed, export a new meaning of Fresh,
   then check the old captured handle at that new output generation. *)
Definition setup_bindings :=
  [BFunctor 10 20 (TypeCapture [] []) [] [(IVal 1,IVal 1)]; BValue 1 (EVal 4); BApply 10 [] [(2,7)] [] [IVal 3]].
Definition expression :=
  fold_right EBind (ETag 11 [(9,TAbs 7)] (EVal 3)) setup_bindings.

Definition provider_compilation : {body : C.bcode |
  C.compile_bindings runtime (fun _ => None) provider_bindings = Some body}.
Proof. eexists; reflexivity. Defined.

Definition compiled_bodies : C.table := fun h =>
  if Nat.eqb h 20 then Some ([ITy 2; IVal 3], proj1_sig provider_compilation) else None.

Lemma compiled_bodies_correct : C.tables_correct runtime compiled_bodies.
Proof.
  intro h. unfold C.tables_correct, runtime, compiled_bodies; cbn [view_functor].
  destruct (Nat.eqb h 20); [|exact I]. split; [reflexivity|].
  intros fuel [types aliases provisions] g st.
  destruct fuel as [|[|[|fuel]]]; cbv [provider_compilation provider_bindings provider_code
    fc_bindings fc_outputs proj1_sig
    C.compile_bindings C.compile_step C.compile_atom C.bcons C.bnil R.eval_bindings
    R.eval bind_type ground app_subst type_meanings provision_values lookup_provision]; try reflexivity.
  all: destruct (lookup_entry (IVal 1) provisions) as [entry|];
    [destruct entry|]; reflexivity.
Qed.

Definition expression_compilation : {body : C.code |
  C.compile_expr runtime compiled_bodies expression = Some body}.
Proof. eexists; reflexivity. Defined.
Definition compiled := proj1_sig expression_compilation.

Theorem all_fuel_agreement : forall fuel d g st,
  compiled fuel d g st = R.eval fuel runtime d g st expression.
Proof.
  intros fuel d g st.
  eapply C.expression_correct; [exact compiled_bodies_correct|].
  exact (proj2_sig expression_compilation).
Qed.

Example caller_started_with_another_type : ground initial (TAbs 2) = TMachine 1.
Proof. reflexivity. Qed.

Definition setup_environment :=
  match R.eval_bindings 30 runtime initial [] tt setup_bindings with
  | inl (d, _, _) => Some d
  | inr _ => None
  end.

Example output_preserves_old_generation :
  option_map (fun d => ground d (TAbs 2)) setup_environment = Some (TMachine 1).
Proof. vm_compute; reflexivity. Qed.

Example output_installs_fresh_generation :
  option_map (fun d => ground d (TAbs 7)) setup_environment = Some (TMachine 0).
Proof. vm_compute; reflexivity. Qed.

Example compiled_capture_output_cast :
  R.evaluation_signal (compiled 30 initial [] tt) = Normal (VTag 11 captured_handle).
Proof. vm_compute; reflexivity. Qed.

Example reference_capture_output_cast :
  R.evaluation_signal (R.eval 30 runtime initial [] tt expression) = Normal (VTag 11 captured_handle).
Proof. rewrite <- all_fuel_agreement. exact compiled_capture_output_cast. Qed.
