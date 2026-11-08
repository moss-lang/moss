(* Fresh output roles remain distinct from old, merged input meanings. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel NMap Sx Elab ElabFrameCore BodyTr.
Module A:=ExecutionModel.Elaboration.
Import ListNotations.
Local Open Scope string_scope.
Definition link_shadow_spec n:=Spec(Some[n])None None.
Definition link_shadow_seed n:=Spec(Some["seed"])None(Some[(["P"],link_shadow_spec n)]).
Definition link_shadow_bundle:=Bundle "/case.moss" ""[AModule "/case.moss" false[](AFile[]
 [ADType"P";ADType"T";ADType"U";ADAssume[(["P"],None)][ADVal"seed"(ATRef["P"]None)None];
 ADCtx"Left"[link_shadow_spec"T";link_shadow_seed"T"];ADCtx"Right"[link_shadow_spec"U";link_shadow_seed"U"];
 ADFn RDetached"out"[]None None;ADUnit"Concrete";
 ADFunctor"Target"(Some[])[link_shadow_spec"Left";link_shadow_spec"Right"][link_shadow_spec"T";Spec(Some["T"])(Some"out")None]None])].
Definition link_shadow_world:world:=ltac:(let q:=eval vm_compute in(load_world link_shadow_bundle)in lazymatch q with Good ?w=>exact w end).
Definition link_shadow_target:=opt_default(GSym 0 0""KType(ADType"")None[]OSource)(nget(w_syms link_shadow_world)9).

Example link_shadow_input_meanings:
 match fun_env link_shadow_target(s_init link_shadow_world)with
 |Good((e,_),_)=>Some(en_uf e,tn_get e 2,canon e(YAbs 2))|Bad _=>None end
 =Some([(2,YAbs 3)],Some(YAbs 2),YAbs 3).
Proof. vm_compute;reflexivity. Qed.
Example link_shadow_actual_output_keys:
 match fun_env link_shadow_target(s_init link_shadow_world)with
 |Good((e,_),s)=>match fresh_target_output_view e link_shadow_target s with
  |Good((view,keys,roles),s')=>match mmap(fun k=>let!it:=key_item_at view k in ret(output_role_item roles it))keys s' with
   |Good(items,_)=>Some(roles,keys,items)|Bad _=>None end
  |Bad _=>None end
 |Bad _=>None end
 =Some([(2,16)],[KyMth(YAbs 16)7],[IMth(TAbs 2)7[]]).
Proof. vm_compute;reflexivity. Qed.
Definition link_shadow_Cu:=A.Cx[(2,TAbs 3);(3,TAbs 3)][(2,TAbs 3)][][][][][][].
Example link_shadow_identity_output_collision:
 A.nlook 2(A.output_type_map link_shadow_Cu[2](fun t=>TAbs t))=Some(TAbs 3).
Proof. reflexivity. Qed.
Example link_shadow_fresh_output_separation:
 A.nlook 2(A.output_type_map link_shadow_Cu[2](fun _=>TAbs 16))=Some(TAbs 16) /\
 output_role_item[(2,16)](IMth(TAbs 16)7[])=IMth(TAbs 2)7[].
Proof. split;reflexivity. Qed.
