(* The immutable application's induction step. The callback is the smaller
   body evaluation supplied by the eventual mutual safety induction. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel CoreMoss MossX XTyping XEnv XHeapC XSafety
 ReifyMaps ReifyChains ReifyContracts ReifyEntries XProvision XProvisionPost.
Import ListNotations.
Set Implicit Arguments.
Section Apply.
Variables(Sg:gsig)(Rp:nsym->option rep)(P:xprog)(Fb:usym->option(tele*tele*subst)).
Variable k:nat.
Hypothesis Immutable:xp_immutable P=true.
Hypothesis WF:wf_gsig Sg.
Hypothesis ClosedInputs:forall h Dh Dr,g_fun Sg h=Some(Dh,Dr,None)->reified_scope_closed Sg P Dh.
Hypothesis Bodies:forall h Dh Dr Dc pins code PhiD PhiC,
 g_fun Sg h=Some(Dh,Dr,None)->Fb h=Some(Dh,Dc,pins)->xp_fun P h=Some code ->
 tele_ctx Dh=Some PhiD ->tele_ctx Dc=Some PhiC ->
 exists PhiEnd,
 body_export_contract Sg P(fun_ctx PhiD PhiC pins)PhiEnd Dh Dr(xp_exports P h) /\
 (forall H gb bodyenv st,
 ctx_grounded gb(fun_ctx PhiD PhiC pins)->
 reified_environment Sg Rp P Fb H gb(cA(fun_ctx PhiD PhiC pins))bodyenv ->
 XHeapC.ConcreteHeap.heap_ok(XSafetyC.PsiH Sg Rp P H)st ->
 provision_binds_post Sg Rp P Fb H(fun_ctx PhiD PhiC pins)PhiEnd gb
   (xinterp_binds k P bodyenv [] st(snd code))).

Lemma reified_apply_step : forall H Phi g d st u Du Dr results inputs outputs,
 g_fun Sg u=Some(Du,Dr,None)->In(IFun u)(cA Phi)->
 ctx_grounded g Phi ->provision_environment Sg Rp P Fb H g(cA Phi)d ->
 application_input_contract Sg P Phi u Du inputs ->
 result_map_contract Sg P Phi u Du Dr results outputs ->
 reified_scope_closed Sg P(cA(open_res results Phi))->
 XHeapC.ConcreteHeap.heap_ok(XSafetyC.PsiH Sg Rp P H)st ->
 provision_apply_post Sg Rp P Fb H Phi results g d
 (xinterp_apply_r(S k)P d u inputs outputs st).
Proof.
 intros H Phi g d st u Du Dr results inputs outputs Decl Hin Ground Env Inputs Outputs ClosedResult Heap.
 destruct(@provision_lookup Sg Rp P Fb H g (cA Phi) d (IFun u) Env eq_refl Hin)as[entry[Lookup Entry]].
 unfold provision_entry in Entry;rewrite Immutable in Entry.
 destruct entry as[w|f dc|h sms dc|p|sym|ty|p ty];
 try solve[cbn[reified_entry]in Entry;contradiction].
 (* Keep the original closure relation intact while exposing its shape. *)
 destruct(Inputs g Ground)as[gin[Input InputMap]].
 unfold provision_environment in Env;rewrite Immutable in Env.
 destruct Env as[Env Closed].
 destruct(@reified_map_composes Sg Rp P Fb H gin g Du (cA Phi) inputs d Env InputMap)as[din[Compose[Keys[InEnv All]]]].
 destruct(@reified_application_body_ready Sg Rp P Fb H g u h sms dc Du Dr gin din
 Immutable WF Decl Entry Input InEnv Keys(ClosedInputs h))
 as[Dh[Dc[pins[code[PhiD[PhiC[gb[dh[FB[Code[TD[TC[GB[Run[BodyEnv[Hdecl Back]]]]]]]]]]]]]]]].
 destruct code as[bodyresults bs].
 destruct(Bodies Hdecl FB Code TD TC)as[PhiEnd[Exports BodySafe]].
 specialize(BodySafe H gb(dh++dc)st GB BodyEnv Heap).
 cbn [snd]in BodySafe.
 cbn[xinterp_apply_r];rewrite Lookup,Code,Compose,Run.
 destruct(xinterp_binds k P(dh++dc)[]st bs)as[[[bodyenv ge]st']|rr]eqn:Eval.
 - cbn in BodySafe;destruct BodySafe as[Ge[H'[bodyout[Ext[Heap'[GroundOut[EnvOut Mean]]]]]]].
   unfold provision_environment in EnvOut;rewrite Immutable in EnvOut.
   destruct EnvOut as[EnvOut ClosedOut].
   destruct(@reified_body_exports Sg Rp P Fb H' _ _ Dh Dr _ gb bodyout bodyenv Exports GB GroundOut Mean EnvOut)
   as[canonical[returned[Schema[Export[ReturnKeys ReturnEnv]]]]].
   destruct(Back canonical Schema)as[gout[Result Trail]].
   pose proof(@reified_result_transport Sg Rp P Fb H' gout canonical Dr Trail returned ReturnEnv)as Returned.
   assert(OldEnv:reified_environment Sg Rp P Fb H' g(cA Phi)d).
   { eapply (@reified_environment_mono Sg Rp P Fb H H');eassumption. }
   destruct(@reified_application_results Sg Rp P Fb H' Phi g d u Du Dr results outputs gin gout returned Immutable WF Closed Ground OldEnv Outputs Input Result Returned)
   as[gcaller[dr[GroundCaller[ComposeOut[KeysOut[EnvCaller OldMean]]]]]].
   rewrite Export,ComposeOut;cbn.
   exists H',gcaller;split;[exact Ext|]. split;[exact Heap'|].
   split;[exact GroundCaller|]. split;[|exact OldMean].
   unfold provision_environment;rewrite Immutable;split;[exact EnvCaller|].
   exact ClosedResult.
 - exact BodySafe.
Qed.

Corollary reified_apply_not_error : forall H Phi g d st u Du Dr results inputs outputs rr c,
 g_fun Sg u=Some(Du,Dr,None)->In(IFun u)(cA Phi)->
 ctx_grounded g Phi ->provision_environment Sg Rp P Fb H g(cA Phi)d ->
 application_input_contract Sg P Phi u Du inputs ->
 result_map_contract Sg P Phi u Du Dr results outputs ->
 reified_scope_closed Sg P(cA(open_res results Phi))->
 XHeapC.ConcreteHeap.heap_ok(XSafetyC.PsiH Sg Rp P H)st ->
 xinterp_apply_r(S k)P d u inputs outputs st=inr rr ->xr_out rr<>XErr c.
Proof.
 intros H Phi g d st u Du Dr results inputs outputs rr c Decl Available Ground Env
 Inputs Outputs Closed Heap Run.
 pose proof(@reified_apply_step H Phi g d st u Du Dr results inputs outputs Decl Available Ground Env Inputs Outputs Closed Heap)as Safe.
 rewrite Run in Safe;cbn in Safe.
 eapply (@provision_control_not_error Sg Rp P H rr c);exact Safe.
Qed.
End Apply.
