(* Review fixture only: this is not a full Admissible counterexample.
   It isolates inherited caller generations at a new activation boundary. *)
From Stdlib Require Import List PeanoNat.
From NM Require Import Audit.
Import Audit.Semantics ListNotations.

Module Prims <: PRIMITIVES.
Definition mstate := unit.
Definition run_prim (_ : program_view) (_ : prim) (_ : list value) (st : unit) := (Trap 0,st).
End Prims.
Module R := Interpreter Prims.

Definition output_code := FunctorCode [] [] [] [ITy 2] [BType 2 (TMachine MI32)] [].
Definition output_binding := BApply 1 [] [(2,2)] [] [].
Definition callee := EBind output_binding EUnit.
Definition runtime := ProgramView
  (fun f => if Nat.eqb f 10 then Some callee else None)
  (fun h => if Nat.eqb h 20 then Some output_code else None)
  0 0 0 [] (fun _ => None) [].
Definition initial := Environment [] [(IFun 1,FunctorClosure 20 None empty_environment None)].
Definition call := ECall 10 [] [(IFun 1,IFun 1)] [].

Example callee_application_succeeds_without_prior_caller_output :
  R.evaluation_signal (R.eval 30 runtime initial [] tt call) = Normal VUnit.
Proof. reflexivity. Qed.

Example prior_caller_output_does_not_occupy_callee_destination :
  R.evaluation_signal (R.eval 40 runtime initial [] tt (EBind output_binding call)) = Normal VUnit.
Proof. reflexivity. Qed.

(* Unrelated ambient meanings do not enter the callee's activation. *)
Example empty_type_call_does_not_inherit_caller_generation :
  lookup_subst 2 (type_meanings
    (call_environment (Environment [(2,TMachine MI32)] []) [] empty_environment)) =
  None.
Proof. reflexivity. Qed.
