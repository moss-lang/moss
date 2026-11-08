(* Shared environment posts for actual immutable closure-binding operations. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel CoreMoss XTyping ReifyMaps ReifyContracts ReifyEntries XProvision.
Import ListNotations.
Set Implicit Arguments.
Section Bind.
Variables(Sg:gsig)(Rp:nsym->option rep)(P:xprog)(Fb:usym->option(tele*tele*subst))(H:sty).
Hypothesis Mode:xp_immutable P=true.

Lemma provision_bind_function : forall Phi g d f code Df Sf Dc Sc PhiC sm body,
 g_fn Sg f=Some(Df,Sf,None)->g_fn Sg code=Some(Dc,Sc,None)->xp_fn P code=Some body ->
 tele_ctx Dc=Some PhiC ->ctx_grounded g Phi ->provision_environment Sg Rp P Fb H g(cA Phi)d ->
 reified_scope_closed Sg P(cA PhiC) ->callable_map_contract Sg P Phi PhiC sm Sf Sc ->
 reified_scope_closed Sg P(cA(add_item(IFn f)Phi))->
 exists dc,xcompose d sm=Some dc /\ctx_grounded g(add_item(IFn f)Phi) /\
 provision_environment Sg Rp P Fb H g(cA(add_item(IFn f)Phi))((IFn f,XDClo code(xclose d dc))::d).
Proof.
 intros Phi g d f code Df Sf Dc Sc PhiC sm body Slot Code Body Tele Ground Env Closed Contract Next.
 pose proof Env as Raw;unfold provision_environment in Raw;rewrite Mode in Raw;destruct Raw as[Raw Scope].
 destruct(@reified_function_capture Sg Rp P Fb H Phi g d f code Df Sf Dc Sc PhiC sm body
 Slot Code Body Tele Ground Raw Closed Contract)as[dc[Compose Entry]].
 exists dc;split;[exact Compose|].
 eapply(@provision_add_closed Sg Rp P Fb H Phi g d(IFn f)(XDClo code(xclose d dc)));
 [reflexivity|exact Ground|exact Env| |exact Next].
 unfold provision_entry;rewrite Mode;exact Entry.
Qed.

Lemma provision_bind_method : forall Phi g d rt m th code Dm Sm Dc Sc PhiC sm body,
 g_mth Sg m=Some(Dm,Sm)->g_fn Sg code=Some(Dc,Sc,None)->xp_fn P code=Some body ->
 tele_ctx Dc=Some PhiC ->ctx_grounded g Phi ->provision_environment Sg Rp P Fb H g(cA Phi)d ->
 reified_scope_closed Sg P(cA PhiC) ->
 callable_map_contract Sg P Phi PhiC sm(reified_method_signature rt th Sm)Sc ->
 reified_scope_closed Sg P(cA(add_item(IMth rt m th)Phi))->
 exists dc,xcompose d sm=Some dc /\ctx_grounded g(add_item(IMth rt m th)Phi) /\
 provision_environment Sg Rp P Fb H g(cA(add_item(IMth rt m th)Phi))((IMth rt m th,XDClo code(xclose d dc))::d).
Proof.
 intros Phi g d rt m th code Dm Sm Dc Sc PhiC sm body Slot Code Body Tele Ground Env Closed Contract Next.
 pose proof Env as Raw;unfold provision_environment in Raw;rewrite Mode in Raw;destruct Raw as[Raw Scope].
 destruct(@reified_method_capture Sg Rp P Fb H Phi g d rt m th code Dm Sm Dc Sc PhiC sm body
 Slot Code Body Tele Ground Raw Closed Contract)as[dc[Compose Entry]].
 exists dc;split;[exact Compose|].
 eapply(@provision_add_closed Sg Rp P Fb H Phi g d(IMth rt m th)(XDClo code(xclose d dc)));
 [reflexivity|exact Ground|exact Env| |exact Next].
 unfold provision_entry;rewrite Mode;exact Entry.
Qed.

Lemma provision_bind_functor : forall Phi g d u h Du Dh Dr Dc pins PhiD PhiC sm smc code,
 g_fun Sg u=Some(Du,Dr,None)->g_fun Sg h=Some(Dh,Dr,None)->
 Fb h=Some(Dh,Dc,pins)->xp_fun P h=Some code ->tele_ctx Dh=Some PhiD ->tele_ctx Dc=Some PhiC ->
 ctx_grounded g Phi ->provision_environment Sg Rp P Fb H g(cA Phi)d ->
 reified_scope_closed Sg P(cA PhiC) ->
 functor_capture_contract Sg P Phi PhiD PhiC u h Dr pins sm smc ->
 reified_scope_closed Sg P(cA(add_item(IFun u)Phi))->
 exists dc,xcompose d smc=Some dc /\ctx_grounded g(add_item(IFun u)Phi) /\
 provision_environment Sg Rp P Fb H g(cA(add_item(IFun u)Phi))((IFun u,XDFun h[sm](xclose d dc))::d).
Proof.
 intros Phi g d u h Du Dh Dr Dc pins PhiD PhiC sm smc code U Hdecl FB Code TD TC Ground Env Closed Contract Next.
 pose proof Env as Raw;unfold provision_environment in Raw;rewrite Mode in Raw;destruct Raw as[Raw Scope].
 destruct(@reified_functor_capture Sg Rp P Fb H Phi g d u h Du Dh Dr Dc pins PhiD PhiC sm smc code
 U Hdecl FB Code TD TC Ground Raw Closed Contract)as[dc[Compose Entry]].
 exists dc;split;[exact Compose|].
 eapply(@provision_add_closed Sg Rp P Fb H Phi g d(IFun u)(XDFun h[sm](xclose d dc)));
 [reflexivity|exact Ground|exact Env| |exact Next].
 unfold provision_entry;rewrite Mode;exact Entry.
Qed.

Lemma provision_link_functor : forall Phi g d u w Du Dw Dr sm,
 g_fun Sg u=Some(Du,Dr,None)->g_fun Sg w=Some(Dw,Dr,None)->In(IFun w)(cA Phi)->
 ctx_grounded g Phi ->provision_environment Sg Rp P Fb H g(cA Phi)d ->
 functor_link (runtime_signature Sg P)(xp_fixed_inst P)g u g w sm(identity_smap Dr)->
 reified_scope_closed Sg P(cA(add_item(IFun u)Phi))->
 exists h sms dc,lookup_xdenv(IFun w)d=Some(XDFun h sms dc) /\
 ctx_grounded g(add_item(IFun u)Phi) /\
 provision_environment Sg Rp P Fb H g(cA(add_item(IFun u)Phi))((IFun u,XDFun h(sms++[sm])dc)::d).
Proof.
 intros Phi g d u w Du Dw Dr sm U W Available Ground Env Link Next.
 destruct(@provision_lookup Sg Rp P Fb H g(cA Phi)d(IFun w)Env eq_refl Available)as[en[Lookup Entry]].
 unfold provision_entry in Entry;rewrite Mode in Entry.
 destruct en as[v|f dd|h sms dc|p|sym|ty|p ty];try solve[cbn[reified_entry]in Entry;contradiction].
 exists h,sms,dc;split;[exact Lookup|].
 eapply(@provision_add_closed Sg Rp P Fb H Phi g d(IFun u)(XDFun h(sms++[sm])dc));
 [reflexivity|exact Ground|exact Env| |exact Next].
 unfold provision_entry;rewrite Mode.
 eapply reified_functor_link;eassumption.
Qed.
End Bind.
