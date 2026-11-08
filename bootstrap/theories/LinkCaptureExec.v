(* Successful capture checks exclude the types supplied at each application. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound BodyTr NeedExec LinkBodyTrace.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque [canon canon_go].

Lemma link_capture_value_fixed : forall fg bg ef eg fneeds gneeds xf s u final v vt t,
 link_trace_capture_guard fg bg ef eg fneeds gneeds xf s=Good(u,final) ->
 In (KyVal v) gneeds -> key_mem (KyVal v) fneeds=false ->
 nget(en_vals eg)v=Some vt -> In t(ety_gens(canon eg vt)) ->
 In t(map fst(en_tn ef)) -> In t xf.
Proof.
 intros fg bg ef eg fneeds gneeds xf s u final v vt t Run Member Captured Value Gen Input.
 unfold link_trace_capture_guard in Run.
 destruct(miter_checked _ _ _ _ _ _ Run (KyVal v) Member)as(before&unit&after&Check).
 cbn in Check;rewrite Captured,Value in Check;cbn[negb andb]in Check.
 destruct(existsb(fun g=>nmem g(filter(fun t=>negb(nmem t xf))(map fst(en_tn ef))))(ety_gens(canon eg vt)))eqn:Any.
 - apply bind_inv in Check as(shown&state&Show&Bad);exfalso;eapply err_not_good;exact Bad.
 - destruct(nmem t xf)eqn:Fixed;[apply nmem_in;exact Fixed|].
   exfalso.
   assert(InArg:In t(filter(fun t=>negb(nmem t xf))(map fst(en_tn ef)))).
   {apply filter_In;split;[exact Input|now rewrite Fixed]. }
   assert(Found:existsb(fun g=>nmem g(filter(fun t=>negb(nmem t xf))(map fst(en_tn ef))))(ety_gens(canon eg vt))=true).
   {apply existsb_exists;exists t;split;[exact Gen|apply nmem_in;exact InArg]. }
   congruence.
Qed.
