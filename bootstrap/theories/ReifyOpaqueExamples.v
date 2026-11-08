(* Actual accepted source carries an unresolved nominal payload through a
   phantom constructor. The computed base keeps its real opaque identity. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel CoreMoss NMap Sx Parse Elab ReifyData Reify
  ReifyNominal ReifyProgram ReifyProviderExamples XTyping.
Import ListNotations ExecutionModel.Input.
Local Open Scope string_scope.
Definition opaque_phantom_text:string:=
 "type Broken=Broken;tag Bad(Broken);type T;assume T{tag Phantom[T](());}fn main(){Phantom[T=Bad](());}".
Definition opaque_phantom_raw:option(xprogram*st):=
 match parse_moss opaque_phantom_text with
 | Bad _=>None|Good file=>match elaborate_with_metadata(Bundle "main.moss" ""[AModule "main.moss" false[]file])with
   | Bad _=>None|Good result=>Some result end end.
Definition opaque_phantom_program:option xprogram:=
 match opaque_phantom_raw with
 | Some(raw,source)=>option_map rr_program(reify_program source raw)|None=>None end.
Definition opaque_phantom_schema:option(option(tele*ty)*option tele):=
 match opaque_phantom_raw with
 | Some(raw,source)=>match sassoc "Bad"(gm_names(get_mod(st_w source)(w_entry(st_w source))))with
   | Some head=>Some(g_tag(reify_nominal_base source)head,g_opaque(reify_nominal_base source)head)
   | None=>None end
 | None=>None end.
Example actual_carried_head_has_no_invented_payload :
 opaque_phantom_schema=Some(None,Some[]).
Proof. vm_compute;reflexivity. Qed.
Example actual_opaque_argument_candidate_runs :
 option_map example_program_outcome opaque_phantom_program=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in opaque_phantom_program in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.

Lemma opaque_head_has_no_payload_value : forall Sg Rp P H n theta value,
 n<>rec_nom->Rp n=None->g_tag Sg n=None->
 ~xvt Sg Rp P H value(TNom n theta).
Proof. intros Sg Rp P H n theta value NonRecord Representation Payload Typed;inversion Typed;subst;congruence. Qed.

Definition latent_opaque_payload_text:string:=
 "type Broken=Broken;tag Bad(Broken);val out:Bad;functor F[]:->out;functor G:->out{bind out={loop{}};}fn main(){bind F=G;}".
Definition latent_opaque_payload_program:option xprogram:=
 match parse_moss latent_opaque_payload_text with
 | Bad _=>None|Good file=>match elaborate_runtime_candidate(Bundle "main.moss" ""[AModule "main.moss" false[]file])with
   | Bad _=>None|Good result=>Some(rr_program result)end end.
Example latent_unobserved_payload_candidate_still_lowers :
 option_map example_program_outcome latent_opaque_payload_program=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in latent_opaque_payload_program in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.
