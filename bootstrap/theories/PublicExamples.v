(* Public-boundary examples: platform signature checking, operation
   requirements, world effects, and missing interface operations. *)
From Stdlib Require Import List String.
From NM Require Import Audit DefaultRuntime.
Import ListNotations.
Local Open Scope string_scope.
Local Open Scope list_scope.

Example absolute_path_accepted : exists path, path_of_string "/main.moss" = Some path.
Proof. eexists; reflexivity. Qed.
Example relative_path_rejected : path_of_string "main.moss" = None.
Proof. reflexivity. Qed.
Example unnormalized_path_rejected : path_of_string "/lib/../main.moss" = None.
Proof. reflexivity. Qed.

Example wrong_primitive_arity : forall world,
  ~ Platform.PrimitiveSignature world (PI32Bin IAdd) [] TUnit.
Proof.
  intros world [[inputs [output [Signature [Typed Result]]]]|[[inputs [output [Signature Rest]]]|GC]].
  - injection Signature as <- <-. inversion Typed.
  - discriminate Signature.
  - exact GC.
Qed.

Module Host <: HOST.
  Definition state := nat.
  Definition primitive (_ : xprog) (_ : prim) (_ : list xvalue) (state : state) :=
    (XUns 0, S state).
  Definition heap_ok (_ : Heap.hty) (_ : state) := True.
End Host.
Module Model := Public Host.
Import Model.

Definition test_interface : interface := default_interface.
Definition empty_interface : interface := HostInterface (fun _ => false) Host.primitive Host.heap_ok.
Definition initial : state := World [] (fun _ => None) 7.
Definition unit_program : program :=
  XProgram [(1, XUnit)] [] 0 0 0 1 [] [] [] [] [] false [] [].
Definition missing_main : program :=
  XProgram [] [] 0 0 0 1 [] [] [] [] [] false [] [].
Definition primitive_program : program :=
  XProgram [(1, XCallA 2 [])] [] 0 0 0 1 [(IFn 2, XDPrim (PUnsupported 0))]
    [] [] [] [] false [] [].

Example finite_observation : Runs unit_program test_interface initial (Result (Returned XVUnit) initial).
Proof. exists 2; reflexivity. Qed.
Example fuel_exhaustion :
  observe (Reference.run_main 0 (runtime unit_program initial) 1 [] (test_interface, initial)) = None.
Proof. reflexivity. Qed.
Example internal_error_is_explicit :
  Runs missing_main test_interface initial (Result (Wrong Reference.e_nobody) initial).
Proof. exists 0; reflexivity. Qed.
Example primitive_effect_updates_world :
  Runs primitive_program test_interface initial
    (Result (Unsupported 0) (World [] (fun _ => None) 8)).
Proof. exists 2; reflexivity. Qed.
Example missing_operation_is_explicit :
  Runs primitive_program empty_interface initial (Result (Wrong e_missing_primitive) initial).
Proof. exists 2; reflexivity. Qed.
Example installed_operation_is_required : required_primitives primitive_program = [PUnsupported 0].
Proof. reflexivity. Qed.
Example concrete_default_laws : forall P,
  DefaultRuntime.Model.interface_laws P DefaultRuntime.default_interface.
Proof. exact DefaultRuntime.default_interface_laws. Qed.

Example requirements_include_captures_and_families :
  required_primitives
    (XProgram [] [(3, ([], [XBApplyBr 9 [(4, [XBPrim (IFn 5) (PI32Bin IAdd)], [])]]))]
      0 0 0 1 [(IFn 7, XDClo 8 [(IFn 6, XDPrimAt PDeref TUnit)])]
      [] [] [] [] false [] []) = [PI32Bin IAdd; PDeref].
Proof. reflexivity. Qed.

Example named_i32_signature : forall world symbol,
  Elaboration.LangSym world "wasm" Elaboration.NSName "I32" symbol ->
  Elaboration.kind_sym world symbol = Some Elaboration.DKType ->
  Platform.PrimitiveSignature world (PI32Bin IAdd)
    [TAbs symbol; TAbs symbol] (TAbs symbol).
Proof.
  intros world symbol Named Kind.
  assert (Shape : Platform.ValueType world (TAbs symbol) XTI32).
  { exists symbol. repeat split; assumption. }
  left. exists [XTI32; XTI32], XTI32. split; [reflexivity|]. split; [|exact Shape].
  constructor; [exact Shape|]. constructor; [exact Shape|constructor].
Qed.

Example wrong_primitive_types : forall world,
  ~ Platform.PrimitiveSignature world (PI32Bin IAdd) [TUnit; TUnit] TUnit.
Proof.
  intros world [[inputs [output [Signature [Typed Result]]]]|[[inputs [output [Signature Rest]]]|GC]].
  - injection Signature as <- <-. destruct Result as (symbol & Named & Kind & Impossible).
    discriminate Impossible.
  - discriminate Signature.
  - exact GC.
Qed.
