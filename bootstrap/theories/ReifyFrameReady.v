(* Real compiler code headers discharge the finite frame allocator's input. *)
From Stdlib Require Import List.
From NM Require Import ReifyData ExecutionModel NMap Elab ReifyHeader ReifyWalkData ReifyReach ReifyEta ReifyProgramWalk.
Import ListNotations.
Lemma function_header_signature_ready:forall source f owner scope inputs signature,
 function_header source f=Some(owner,scope,inputs,Some signature)->
 function_frame_node_ready source(RnFunction f).
Proof.
 intros source f owner scope inputs signature Header.
 unfold function_header in Header.
 destruct(assoc reify_owner_eqb(ROFunction f)(rm_codes(st_reify source)))as[code|]eqn:Code;[|discriminate].
 cbn[option_map code_header]in Header;injection Header as Owner Scope Inputs Signature.
 left;exists code;split;[exact Code|eexists;exact Signature].
Qed.
Lemma function_header_eta_ready:forall source f owner scope inputs original,
 function_header source f=Some(owner,scope,inputs,None)->
 source_eta_callee source f=Some original->
 function_frame_node_ready source(RnFunction f).
Proof.
 intros source f owner scope inputs original Header Eta.
 unfold function_header in Header.
 destruct(assoc reify_owner_eqb(ROFunction f)(rm_codes(st_reify source)))as[code|]eqn:Code;[|discriminate].
 cbn[option_map code_header]in Header;injection Header as Owner Scope Inputs Signature.
 right;exists code,original;auto.
Qed.
