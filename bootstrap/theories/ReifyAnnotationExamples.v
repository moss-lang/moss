(* Actual method provision inference introduces formal parameter readings
   without rebinding the caller's mutable type names. Both accessor arities
   must select their exact checked annotations and lower successfully. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel Sx Parse ReifyProgram ReifyProviderExamples Reify.
Import ListNotations ExecutionModel.Input.
Local Open Scope string_scope.
Definition inferred_accessor_program(text:string):option xprogram:=
 match parse_moss text with
 | Bad _=>None|Good file=>match elaborate_runtime_candidate(Bundle "main.moss" ""[AModule "main.moss" false[]file])with
   | Bad _=>None|Good result=>Some(rr_program result)end end.
Example inferred_length_parameter_lowers_and_runs :
 option_map example_program_outcome(inferred_accessor_program
 "type Index;tag A;tag B;assume Index{fn .length():Index;}fn length():A{A}fn main(){bind B.length[Index=A]=length;B.length();}")=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in(inferred_accessor_program
 "type Index;tag A;tag B;assume Index{fn .length():Index;}fn length():A{A}fn main(){bind B.length[Index=A]=length;B.length();}")in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.
Example inferred_get_parameters_lower_and_run :
 option_map example_program_outcome(inferred_accessor_program
 "type Index;type Elem;tag A;tag B;assume Index,Elem{fn .get(i:Index):Elem;}fn get(i:A):A{i}fn main(){bind B.get[Index=A,Elem=A]=get;B.get(A);}")=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in(inferred_accessor_program
 "type Index;type Elem;tag A;tag B;assume Index,Elem{fn .get(i:Index):Elem;}fn get(i:A):A{i}fn main(){bind B.get[Index=A,Elem=A]=get;B.get(A);}")in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.
