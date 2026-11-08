(* A required nested formal need not have been provided or applied. Its
   interface comes from the checked declaration, not a fictitious provider. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel Sx Parse ReifyProgram ReifyProviderExamples Reify.
Import ListNotations ExecutionModel.Input.
Local Open Scope string_scope.
Definition nested_unprovided_interface_text:string:=
 "val x:();functor Inner:->x;functor Outer:Inner->x;functor G:Inner->x{bind x=();}fn main(){bind Outer=G;}".
Definition nested_unprovided_interface_program:option xprogram:=
 match parse_moss nested_unprovided_interface_text with
 | Bad _=>None|Good file=>match elaborate_runtime_candidate(Bundle "main.moss" ""[AModule "main.moss" false[]file])with
   | Bad _=>None|Good result=>Some(rr_program result)end end.
Example nested_unprovided_interface_lowers_and_runs :
 option_map example_program_outcome nested_unprovided_interface_program=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in nested_unprovided_interface_program in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.
