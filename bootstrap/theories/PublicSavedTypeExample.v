(* Regression for the repaired static/dynamic generation boundary, together
   with the previous helper behavior for comparison. No full Admissible
   counterexample or universal safety theorem is asserted. *)
From Stdlib Require Import List String.
From NM Require Import Audit.
From NM Require Import LegacyBodyJudgments.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.
Local Open Scope string_scope.

(* Symbols: Saved=2, Boxed=3, F64=4, I32=5. *)
Definition before := A.Cx [(4,TAbs 4);(5,TAbs 5)] [] [] [] [].
Definition save_f64 := A.set_ty before 2 (A.canon before (TAbs 4)).
Definition rebind_f64 := A.set_ty save_f64 4 (A.canon save_f64 (TAbs 5)).
Definition read_saved :=
  match A.nlook 2 (A.c_ty rebind_f64) with
  | Some meaning => A.canon rebind_f64 meaning
  | None => TUnit
  end.

Definition runtime_before := Environment [(4,TMachine 1);(5,TMachine 0)] [].
Definition runtime_saved := bind_type runtime_before 2 (TAbs 4).
Definition runtime_rebound := bind_type runtime_saved 4 (TAbs 5).
Definition runtime_boxed := bind_type runtime_rebound 3 read_saved.

Example static_saved_identity : read_saved = TAbs 4.
Proof. reflexivity. Qed.

Example static_current_f64 : A.nlook 4 (A.c_ty rebind_f64) = Some (TAbs 5).
Proof. reflexivity. Qed.

Example runtime_saved_keeps_f64 : output_meaning runtime_rebound 2 = Some (TMachine 1).
Proof. reflexivity. Qed.

Example emitted_saved_identity_keeps_f64 :
  ground runtime_rebound read_saved = TMachine 1.
Proof. reflexivity. Qed.

Example boxed_receives_saved_meaning :
  output_meaning runtime_boxed 3 = Some (TMachine 1).
Proof. reflexivity. Qed.

Theorem alias_binding_preserves_old_generation :
  ground runtime_rebound read_saved = ground runtime_before read_saved.
Proof. reflexivity. Qed.

Example runtime_saved_alias_keeps_f64 : output_meaning runtime_rebound 2 = Some (TMachine 1).
Proof. reflexivity. Qed.

(* The public source rule still emits the already-read generation. It now
   has its stable meaning, rather than being read as a mutable source name. *)
Definition frame := A.Frame "/example" None None false [] [] [] [].
Theorem public_bind_boxed_from_saved : forall W P,
  A.resolve_sym W "/example" ["Boxed"] = Some 3 ->
  A.kind_sym W 3 = Some A.DKType ->
  A.resolve_sym W "/example" ["Saved"] = Some 2 ->
  A.kind_sym W 2 = Some A.DKType ->
  LegacyBodyJudgments.BindItem W P frame rebind_f64
    (Input.Spec (Some ["Boxed"]) None None, Some (Input.AEPath ["Saved"] None))
    (bind_type_expression 3 (TAbs 4)) (A.set_ty rebind_f64 3 (TAbs 4)) frame [].
Proof.
  intros W P Boxed BoxedKind Saved SavedKind.
  eapply LegacyBodyJudgments.BTypeBind with (t:=3) (rp:=["Saved"]) (ra:=None) (tau:=TAbs 4).
  - exact Boxed.
  - exact BoxedKind.
  - intros [].
  - eapply A.TyType with (t:=2); [exact Saved|exact SavedKind|reflexivity].
Qed.

(* Independent calculations at the existing output-transfer boundary. They
   assert only how the helpers evaluate, not that this whole environment
   arises from a formed world and admissible linked body. *)
Definition body_result := Environment [(4,TMachine 0)] [].
Definition caller_after_result :=
  match result_environment [(4,6)] body_result [ITy 4] empty_environment with
  | Some result => append_environment result runtime_before
  | None => empty_environment
  end.

Example returned_fresh_generation :
  ground caller_after_result (TAbs 6) = TMachine 0.
Proof. reflexivity. Qed.

Example returned_source_preserves_old_generation :
  ground caller_after_result (TAbs 4) = TMachine 1.
Proof. reflexivity. Qed.

Example result_transfer_preserves_old_generation :
  ground caller_after_result (TAbs 4) = ground runtime_before (TAbs 4).
Proof. reflexivity. Qed.

(* Previous behavior, isolated from the repaired semantics. *)
Definition legacy_bind_type (d : environment) symbol meaning :=
  Environment ((symbol, ground d meaning)::type_meanings d) (provision_values d).
Definition legacy_saved := legacy_bind_type runtime_before 2 (TAbs 4).
Definition legacy_rebound := legacy_bind_type legacy_saved 4 (TAbs 5).
Theorem legacy_saved_and_emitted_reads_differ :
  ground legacy_rebound (TAbs 2) <> ground legacy_rebound read_saved.
Proof. discriminate. Qed.

Module Primitives <: PRIMITIVES.
Definition mstate := unit.
Definition run_prim (_ : program_view) (_ : op) (_ : option ty) (_ : list value) (state : unit) := (Trap 0,state).
End Primitives.
Module R := Interpreter Primitives.
Definition casts := ProgramView (fun _ => None) (fun _ => None) 0 0 0 []
  (fun _ => None) [(11,CastSpec 0 9 None)] [].
Example saved_f64_rejects_integer_handle :
  R.check_tag casts runtime_boxed 11 [(9,read_saved)]
    (VRef 0 (Some (0, TMachine 0))) = Trap 37.
Proof. reflexivity. Qed.
