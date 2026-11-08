(* A legitimate closure may require a capability whose generic declaration
   cannot be formed. It cannot be applied, but binding the outer closure is
   valid source and must not force a fictitious schema for the capability. *)
From Stdlib Require Import List String Bool.
From NM Require Import ExecutionModel CoreMoss Sx Parse Elab ReifyData Reify ReifyProgram
 ReifyOpaqueFunctors ReifyProviderExamples.
Import ListNotations ExecutionModel.Input.
Local Open Scope string_scope.
Definition latent_inner_text:string:=
 "type T;tag U;assume T{fn offered():T|U;}functor Inner[T]:T->offered;val x:();functor Outer[]:Inner->x;functor G:Inner->x{bind x=();}fn main(){bind Outer=G;}".
Definition latent_inner_candidate:option reify_result:=
 match parse_moss latent_inner_text with
 | Bad _=>None|Good file=>match elaborate_runtime_candidate
     (Bundle "main.moss" ""[AModule "main.moss" false[]file])with
     | Bad _=>None|Good result=>Some result end end.
Definition has_opaque_functor_declaration(declarations:list(item*reify_decl)):bool:=
 existsb(fun p=>match p with(IFun _,RDOpaqueFunctor _)=>true|_=>false end)declarations.
Example actual_latent_inner_preserves_an_opaque_capability :
 option_map(fun result=>has_opaque_functor_declaration(rr_declarations result))latent_inner_candidate=Some true.
Proof. vm_compute;reflexivity. Qed.
Example actual_latent_inner_candidate_runs :
 option_map(fun result=>example_program_outcome(rr_program result))latent_inner_candidate=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in latent_inner_candidate in
 change(option_map(fun result=>example_program_outcome(rr_program result))compiled=Some(XOk XVUnit));reflexivity.
Qed.
Definition selfcopy_text:string:=
 "fn q();functor F[]:->q;functor G:->q{bind q=()=>();}fn main(){bind F=G;bind F=F;bind F;q();}".
Definition selfcopy_source:option(xprogram*st):=
 match parse_moss selfcopy_text with
 | Bad _=>None|Good file=>match elaborate_with_metadata
     (Bundle "main.moss" ""[AModule "main.moss" false[]file])with
     | Bad _=>None|Good result=>Some result end end.
Definition identity_copy_without_profile(event:reify_event ety key):bool:=
 match event with REFunctor _ _ name provider _ _ None=>Nat.eqb name provider|_=>false end.
Example actual_inhabited_selfcopy_records_no_fabricated_profile :
 option_map(fun p=>existsb identity_copy_without_profile
   (flat_map(fun code=>rc_events ety key(snd code))(rm_codes(st_reify(snd p)))))selfcopy_source=Some true.
Proof. vm_compute;reflexivity. Qed.
Example actual_inhabited_selfcopy_candidate_runs :
 option_map(fun p=>option_map(fun result=>example_program_outcome(rr_program result))
   (reify_program(snd p)(fst p)))selfcopy_source=Some(Some(XOk XVUnit)).
Proof.
 let compiled:=eval vm_compute in
   (option_map(fun p=>reify_program(snd p)(fst p))selfcopy_source)in
 change(option_map(fun result=>option_map(fun result=>example_program_outcome(rr_program result))result)
   compiled=Some(Some(XOk XVUnit)));reflexivity.
Qed.
Definition latent_inner_copy_text:string:=
 "type T;tag U;assume T{fn offered():T|U;}functor Inner[T]:T->offered;val x:();functor Outer[]:Inner->x;functor G:Inner->x{bind Inner=Inner;bind x=();}fn main(){bind Outer=G;}".
Example actual_latent_selfcopy_candidate_runs :
 (match parse_moss latent_inner_copy_text with
 | Bad _=>None|Good file=>match elaborate_runtime_candidate
     (Bundle "main.moss" ""[AModule "main.moss" false[]file])with
     | Bad _=>None|Good result=>Some(example_program_outcome(rr_program result))end end)=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in
  (match parse_moss latent_inner_copy_text with
   | Bad _=>None|Good file=>match elaborate_runtime_candidate
       (Bundle "main.moss" ""[AModule "main.moss" false[]file])with
       | Bad _=>None|Good result=>Some(rr_program result)end end)in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.
