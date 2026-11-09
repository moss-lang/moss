(* Small dynamic regressions for generation meanings, mutable source aliases,
   formal-input shadowing and simultaneous functor output transfer. These
   calculations do not assert a whole-program Admissible derivation. *)
From Stdlib Require Import List.
From NM Require Import Audit.
Import Audit.Semantics ListNotations.

Definition caller := Environment [(100,TMachine MI32);(101,TMachine MI64)] [].
Definition captured := EnvironmentWithAliases
  [(10,TMachine MF32);(11,TMachine MF64)]
  [(10,TMachine MF32);(11,TMachine MF64)] [].
Definition called := call_environment caller [(10,TAbs 100)] captured.

Example formal_input_shadows_captured_alias :
  output_meaning called 10 = Some (TMachine MI32).
Proof. reflexivity. Qed.

Example formal_input_shadows_captured_generation :
  ground called (TAbs 10) = TMachine MI32.
Proof. reflexivity. Qed.

Example unrelated_capture_keeps_lexical_meaning :
  output_meaning called 11 = Some (TMachine MF64).
Proof. reflexivity. Qed.

Definition rebound := bind_type called 10 (TMachine MI64).
Example body_rebind_changes_only_output_reading :
  output_meaning rebound 10 = Some (TMachine MI64) /\
  ground rebound (TAbs 10) = TMachine MI32.
Proof. split; reflexivity. Qed.

Definition provider_view := ProgramView (fun _ => None)
  (fun _ => Some (FunctorCode [10;11] [10;11] [ITy 10] []))
  0 0 0 [] (fun _ => None) [].
Definition applied := application_environment provider_view caller 1
  [(10,TAbs 100);(12,TAbs 101)] captured.

Example provider_inputs_shadow_and_private_capture_survives :
  output_meaning applied 10 = Some (TMachine MI32) /\
  output_meaning applied 11 = Some (TMachine MF64).
Proof. split; reflexivity. Qed.

Example result_reads_final_alias :
  result_environment caller [(10,200)] rebound [ITy 10] empty_environment =
  Some (EnvironmentWithAliases [(200,TMachine MI64)] [(10,TMachine MI64)] []).
Proof. reflexivity. Qed.

Definition two_outputs := Environment [(10,TMachine MF32);(11,TMachine MF64)] [].
Example output_reads_share_one_snapshot :
  result_environment empty_environment [(10,11);(11,10)] two_outputs [ITy 10;ITy 11] empty_environment =
  Some (EnvironmentWithAliases [(11,TMachine MF32);(10,TMachine MF64)]
    [(10,TMachine MF32);(11,TMachine MF64)] []).
Proof. reflexivity. Qed.

Example output_cannot_overwrite_callers_generation :
  result_environment caller [(10,100)] called [ITy 10] empty_environment = None.
Proof. reflexivity. Qed.

Module Primitives <: PRIMITIVES.
Definition mstate := unit.
Definition run_prim (_ : program_view) (_ : prim) (_ : list value) (state : unit) := (Trap 0,state).
End Primitives.
Module R := Interpreter Primitives.
Definition with_functor := install_provision (IFun 1) (FunctorClosure 1 [] captured) caller.
Example actual_application_exports_unchanged_formal_not_stale_capture :
  R.apply_functor 3 provider_view with_functor 1 [(10,TAbs 100)] [(10,200)] [] tt =
  inl (EnvironmentWithAliases [(200,TMachine MI32)] [(10,TMachine MI32)] [],tt).
Proof. reflexivity. Qed.

(* New activations receive declared inputs and lexical captures, without
   unrelated ambient caller meanings. Schematic
   Moss: F[]: -> T; Make: -> T { bind T = Concrete; }
   assume F { fn helper() { bind F; } }
   assume T,F { fn outer() { helper(); } }
   helper's T output is initially out of force, so its destination is T
   itself. An ambient T inherited from outer must not make this get stuck.
   These calculations check the repaired boundary; they do not construct
   a complete source-world or Admissible proof for the schematic program. *)
Definition fresh_provider_view := ProgramView
  (fun _ => Some (EBind (BApply 1 [] [(10,10)] []) EUnit))
  (fun _ => Some (FunctorCode [] [] [ITy 10] [BType 10 (TNom 20 [])]))
  0 0 0 [] (fun _ => None) [].
Definition ambient_caller := install_provision (IFun 1)
  (FunctorClosure 1 [] empty_environment)
  (Environment [(10,TNom 30 [])] []).

Example ambient_generation_does_not_block_callee_first_output :
  R.eval 8 fresh_provider_view ambient_caller [] tt
    (ECall 2 [] [(IFun 1,IFun 1)] []) =
  R.Evaluation (Normal VUnit) [] tt.
Proof. reflexivity. Qed.

Example same_callee_without_ambient_generation_succeeds :
  R.eval 8 fresh_provider_view
    (install_provision (IFun 1) (FunctorClosure 1 [] empty_environment) empty_environment)
    [] tt (ECall 2 [] [(IFun 1,IFun 1)] []) =
  R.Evaluation (Normal VUnit) [] tt.
Proof. reflexivity. Qed.


(* A bound defined provider captures only its declared type roles. The
   helper assumes F, so its empty type plan excludes outer's unrelated T;
   executing it through a FunctionClosure has the same behavior as ECall. *)
Example bound_function_ambient_generation_does_not_block_first_output :
  R.eval 12 fresh_provider_view ambient_caller [] tt
    (EBind (BFunction 40 2 (TypeCapture [] []) [(IFun 1,IFun 1)])
      (EInvoke 40 [])) =
  R.Evaluation (Normal VUnit) [] tt.
Proof. reflexivity. Qed.

Example extra_abstract_inputs_do_not_enter_provider_scope :
  output_meaning applied 12 = None.
Proof. reflexivity. Qed.
