(* An old input representative can share the declared name of a new output.
   Fresh roles distinguish them until the code-header renaming. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabFrameCore BodyTr LinkBodyTrace LinkOutputShadow.
Import ListNotations.
Local Open Scope string_scope.
Module A:=ExecutionModel.Elaboration.
Definition link_capture_bundle:=Bundle "/case.moss" ""[AModule "/case.moss" false[](AFile[]
 [ADType"P";ADType"T";ADType"U";ADAssume[(["P"],None)][ADVal"seed"(ATRef["P"]None)None];
 ADCtx"Left"[link_shadow_spec"T";link_shadow_seed"T"];ADCtx"Right"[link_shadow_spec"U";link_shadow_seed"U"];
 ADFn RDetached"out"[]None None;ADUnit"Concrete";
 ADFunctor"Target"(Some[])[link_shadow_spec"Right";link_shadow_spec"Left"]
  [link_shadow_spec"T";Spec(Some["U"])(Some"out")None]None])].
Definition link_capture_world:world:=ltac:(let q:=eval vm_compute in(load_world link_capture_bundle)in lazymatch q with Good ?w=>exact w end).
Definition link_capture_target:=opt_default(GSym 0 0""KType(ADType"")None[]OSource)(nget(w_syms link_capture_world)9).
Example link_capture_old_input_meanings:
 match fun_env link_capture_target(s_init link_capture_world)with
 |Good((e,_),_)=>Some(en_uf e,tn_get e 2,canon e(YAbs 3))|Bad _=>None end
 =Some([(3,YAbs 2)],Some(YAbs 2),YAbs 2).
Proof. vm_compute;reflexivity. Qed.
Example link_capture_actual_output_keys:
 match fun_env link_capture_target(s_init link_capture_world)with
 |Good((e,_),s)=>match fresh_target_output_view e link_capture_target s with
  |Good((view,keys,roles),s')=>match mmap(fun k=>let!it:=key_item_at view k in ret(output_role_item roles it))keys s' with
   |Good(items,_)=>Some(roles,keys,items)|Bad _=>None end
  |Bad _=>None end
 |Bad _=>None end
 =Some([(2,16)],[KyMth(YAbs 2)7],[IMth(TAbs 2)7[]]).
Proof. vm_compute;reflexivity. Qed.
Definition link_capture_body_env:=
 set_tn(env_with_edges[(2,YAbs 2);(3,YAbs 3)][(3,YAbs 2)])2(YNom 8[])99.
Definition link_capture_body_context:=A.Cx[(2,TNom 8[]);(3,TAbs 2)][(3,TAbs 2)][][][][][][].
Example link_capture_distinct_body_and_site:
 link_trace_body_key link_capture_body_env(link_trace_made_types link_capture_body_env[(2,16)][3])(KyMth(YAbs 2)7)=KyMth(YAbs 2)7 /\
 A.at_site link_capture_body_context(output_role_item[(2,16)](IMth(TAbs 2)7[]))=IMth(TNom 8[])7[].
Proof. split;vm_compute;reflexivity. Qed.
