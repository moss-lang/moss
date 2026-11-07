(* Actual generated compiler invocations supply their final frames. *)
(* Real compiler code headers discharge the finite frame allocator's input. *)
From Stdlib Require Import List.
From NM Require Import ReifyData Audit NMap Elab ReifyHeader ReifyWalkData ReifyReach ReifyEta ReifyProgramWalk.
From NM Require Import ReifyFrameReady.
Import ListNotations.
From NM Require Import Sx ElabFrame LambdaOrigin LambdaHeader EtaHeader ReifyBodyHeader LambdaOriginProgram.
Lemma generated_origin_frame_ready:forall source f body owner,
 nget(st_syms source)f=Some owner->nassoc f(st_fns source)=Some body->
 LambdaOrigin f body(function_header source f)(gs_origin owner)->
 function_frame_node_ready source(RnFunction f).
Proof.
 intros source f body owner Symbol Present [Lam|[Eta|Forward]].
 - destruct Lam as(fuel&c&e&ps&rt&expression&bound&sg&this&what&initial&pre&sm&completed&Origin&I&Run&Body&Headers).
   destruct(@lambda_provider_header _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ Run I)as(le&lets&compiled&Header&Emitted).
   rewrite Headers in Header;eapply function_header_signature_ready;exact Header.
 - destruct Eta as(fuel&provider&initial&completed&Origin&I&Absent&Run&Body&Headers).
   destruct(wrapper_created_header _ _ _ _ _ Absent Run)as(physical&info&th&sm&Header&Emitted&Ready).
   rewrite Headers in Header.
   assert(Current:nassoc f(st_fns source)=Some(XCallD(gs_id physical)th sm(map XVar(seq 1(List.length(fn_params physical)))))).
   {rewrite Present;rewrite Body in Emitted;injection Emitted as <-;reflexivity. }
   eapply function_header_eta_ready;[exact Header|].
   eapply source_eta_callee_header;[exact Header|exact Symbol|exact Origin|exact Current].
 - destruct Forward as(fuel&g&block&initial&completed&Origin&I&Id&Physical&Has&Absent&Below&Run&Body&Headers).
   subst f.
   destruct(completed_body_header _ _ _ _ _ Has Absent Below I Run)
    as(f0&info&si&this&sr&sg&ss&marker&registered&x&t&bt&bodyFinal&Facts).
   destruct Facts as(Fuel&Info&Receiver&Signature&Marker&Register&Compiled&Emitted&Header).
   rewrite Headers in Header;eapply function_header_signature_ready;exact Header.
Qed.
Theorem actual_generated_frame_ready:forall b w p final,
 load_world b=Good w->elab_program(s_init w)=Good(p,final)->
 forall f owner,nget(st_syms final)f=Some owner->GeneratedCode owner->
 function_frame_node_ready final(RnFunction f).
Proof.
 intros b w p final Load Run f owner Symbol Origin.
 destruct(@actual_final_generated_origins _ _ _ _ Load Run _ _ Symbol Origin)as[body[Present Fact]].
 eapply generated_origin_frame_ready;eassumption.
Qed.
