(* One environment interface for legacy and immutable MossX execution. *)
From Stdlib Require Import List Bool.
From NM Require Import ExecutionModel CoreMoss XTyping XEnv XRuntimeEnv ReifyContracts ReifyEntries.
Import ListNotations.
Set Implicit Arguments.

Definition provision_entry Sg Rp P Fb H g i en : Prop :=
 if xp_immutable P then reified_entry Sg Rp P Fb H g i en
 else xdentry_agree Sg Rp P Fb H g i en.
Definition provision_environment Sg Rp P Fb H g A d : Prop :=
 if xp_immutable P then (reified_environment Sg Rp P Fb H g A d /\reified_scope_closed Sg P A)
 else xdenv_agree Sg Rp P Fb H g A d.

Section Provision.
Variables(Sg:gsig)(Rp:nsym->option rep)(P:xprog)(Fb:usym->option(tele*tele*subst))(H:sty).
Lemma provision_lookup : forall g A d i,
 provision_environment Sg Rp P Fb H g A d ->is_ty_item i=false ->In i A ->
 exists en,lookup_xdenv i d=Some en /\provision_entry Sg Rp P Fb H g i en.
Proof.
 intros g A d i Env Nt Hin;unfold provision_environment in Env;unfold provision_entry.
 destruct(xp_immutable P).
 - destruct Env as[[Available[Agree Types]]Closed].
   destruct(Available i Hin)as[en Lookup];exists en;split;[exact Lookup|eapply Agree;eassumption].
 - destruct Env as[[Available[Agree Closed]]Types].
   destruct(proj1(Forall_forall _ _)Available i Hin)as[Ty|[en Lookup]];[congruence|].
   exists en;split;[exact Lookup|eapply Agree;eassumption].
Qed.

Lemma provision_types : forall g A d,
 provision_environment Sg Rp P Fb H g A d ->runtime_types P g A d.
Proof.
 intros g A d Env;unfold provision_environment in Env;destruct(xp_immutable P).
 - intros _;exact(proj2(proj2(proj1 Env))).
 - exact(proj2 Env).
Qed.

Lemma provision_closed : forall g A d,
 provision_environment Sg Rp P Fb H g A d ->reified_scope_closed Sg P A.
Proof.
 intros g A d Env;unfold provision_environment in Env;destruct(xp_immutable P)eqn:Mode.
 - exact(proj2 Env).
 - destruct Env as[[Available[Agree Closed]]Types].
   unfold reified_scope_closed;eapply Forall_impl;[|exact Closed].
   intros i Ci;unfold runtime_dep_tys;destruct i;cbn;try exact Ci.
   rewrite Mode;exact Ci.
Qed.

Lemma provision_value : forall g A d v D tau,
 provision_environment Sg Rp P Fb H g A d ->In(IVal v)A ->g_val Sg v=Some(D,tau) ->
 exists value,lookup_xdenv(IVal v)d=Some(XDVal value) /\xvt Sg Rp P H value(app_subst g tau).
Proof.
 intros g A d v D tau Env Hin Sig.
 destruct(@provision_lookup g A d (IVal v) Env eq_refl Hin)as[en[Lookup Entry]].
 unfold provision_entry in Entry;destruct(xp_immutable P);destruct en;
 cbn[reified_entry xdentry_agree]in Entry;try contradiction;
 rewrite Sig in Entry;eauto.
Qed.

Lemma provision_push_non_type : forall g A d i en,
 provision_environment Sg Rp P Fb H g A d ->is_ty_item i=false ->
 provision_entry Sg Rp P Fb H g i en ->
 incl(runtime_dep_tys Sg P i++item_ty_frees i++method_item_frees Sg i)(tyreqs A) ->
 provision_environment Sg Rp P Fb H g(i::A)((i,en)::d).
Proof.
 intros g A d i en Env Nt Entry Closed.
 unfold provision_environment in Env|-*;unfold provision_entry in Entry.
 destruct(xp_immutable P)eqn:Mode.
 - destruct Env as[Env ClosedA];split.
   + apply reified_environment_cons_non_type;assumption.
   + unfold reified_scope_closed in ClosedA|-*;constructor.
     * intros t Hin;unfold tyreqs;cbn;apply in_or_app;right;apply Closed;exact Hin.
     * eapply Forall_impl;[|exact ClosedA]. intros j Cj t Hin.
       unfold tyreqs;cbn;apply in_or_app;right;apply Cj;exact Hin.
 - eapply xdenv_agree_push;[exact Env|exact Nt|exact Entry|].
   unfold runtime_dep_tys in Closed;destruct i;cbn in Closed|-*;try exact Closed.
   rewrite Mode in Closed;exact Closed.
Qed.
Lemma provision_add_non_type : forall Phi g d i en,
 is_ty_item i=false ->ctx_grounded g Phi ->provision_environment Sg Rp P Fb H g(cA Phi)d ->
 provision_entry Sg Rp P Fb H g i en ->
 incl(runtime_dep_tys Sg P i++item_ty_frees i++method_item_frees Sg i)(tyreqs(cA Phi)) ->
 ctx_grounded g(add_item i Phi) /\
 provision_environment Sg Rp P Fb H g(cA(add_item i Phi))((i,en)::d).
Proof.
 intros Phi g d i en Nt Ground Env Entry Closed;split.
 - eapply reified_ground_add_non_type;eassumption.
 - eapply provision_push_non_type;eassumption.
Qed.

Lemma provision_add_closed : forall Phi g d i en,
 is_ty_item i=false ->ctx_grounded g Phi ->
 provision_environment Sg Rp P Fb H g(cA Phi)d ->
 provision_entry Sg Rp P Fb H g i en ->
 reified_scope_closed Sg P(cA(add_item i Phi)) ->
 ctx_grounded g(add_item i Phi) /\
 provision_environment Sg Rp P Fb H g(cA(add_item i Phi))((i,en)::d).
Proof.
 intros Phi g d i en Nt Ground Env Entry Closed.
 eapply provision_add_non_type;[exact Nt|exact Ground|exact Env|exact Entry|].
 unfold reified_scope_closed in Closed;inversion Closed as[|j js Head Tail];subst.
 destruct i;cbn in Nt;try discriminate;exact Head.
Qed.

Lemma provision_bind_value : forall Phi g d v D tau value,
 g_val Sg v=Some(D,tau) ->sat0 Sg Phi D ->
 xvt Sg Rp P H value(app_subst g tau) ->ctx_grounded g Phi ->
 provision_environment Sg Rp P Fb H g(cA Phi)d ->
 ctx_grounded g(add_item(IVal v)Phi) /\
 provision_environment Sg Rp P Fb H g(cA(add_item(IVal v)Phi))((IVal v,XDVal value)::d).
Proof.
 intros Phi g d v D tau value Sig Sat Value Ground Env.
 eapply provision_add_non_type;[reflexivity|exact Ground|exact Env| |].
 - unfold provision_entry;destruct(xp_immutable P);
   cbn[reified_entry xdentry_agree];rewrite Sig;exact Value.
 - destruct Sat as[PhiD[Tele[Same Available]]].
   intros t Hin;cbn[runtime_dep_tys dep_tys item_ty_frees method_item_frees]in Hin.
   rewrite Sig in Hin;cbn in Hin;rewrite app_nil_r in Hin.
   apply wf_ty_frees_aux3,avail_ty_in.
   apply(proj1(Forall_forall _ _)Available t).
   apply(proj2(tele_ctx_tyreqs D Tele t));exact Hin.
Qed.

Lemma provision_environment_from_capture : forall g A d,
 Forall(fun i=>is_ty_item i=true \/exists en,lookup_xdenv i d=Some en)A ->
 Forall(fun p=>provision_entry Sg Rp P Fb H g(fst p)(snd p))d ->
 Forall(fun p=>incl(runtime_dep_tys Sg P(fst p)++item_ty_frees(fst p)++method_item_frees Sg(fst p))(tyreqs A))d ->
 runtime_types P g A d ->provision_environment Sg Rp P Fb H g A d.
Proof.
 intros g A d Cover Entries Closed Types.
 unfold provision_environment;unfold provision_entry in Entries;destruct(xp_immutable P)eqn:Mode.
 - assert(Reify:xreify P=true)by(unfold xreify;rewrite Mode;reflexivity).
   assert(Available:Forall(fun i=>exists en,lookup_xdenv i d=Some en)A).
   { apply Forall_forall;intros i Hin.
     destruct(proj1(Forall_forall _ _)Cover i Hin)as[Ty|Found];[|exact Found].
     destruct i;cbn in Ty;try discriminate.
     destruct(Types Reify t Hin)as[tau[Lookup Meaning]];eauto. }
   exact(@reified_capture_environment Sg Rp P Fb H g A d Available Entries Closed(Types Reify)).
 - split;[|exact Types]. split;[exact Cover|]. split.
   + intros i en Hin Nt Lookup.
     exact(proj1(Forall_forall _ _)Entries(i,en)(lookup_xdenv_in i d Lookup)).
   + apply Forall_forall;intros i Hin.
     destruct(is_ty_item i)eqn:Ty.
     * destruct i;cbn in Ty;try discriminate.
       cbn[dep_tys item_ty_frees method_item_frees];intros x Bad;contradiction.
     * destruct(proj1(Forall_forall _ _)Cover i Hin)as[Bad|[en Lookup]];[congruence|].
       pose proof(proj1(Forall_forall _ _)Closed(i,en)(lookup_xdenv_in i d Lookup))as Dep.
       cbn[fst snd]in Dep;unfold runtime_dep_tys in Dep;destruct i;cbn in Dep|-*;try exact Dep.
       rewrite Mode in Dep;exact Dep.
Qed.

Lemma provision_primitive_agreement : forall g i p,
 provision_entry Sg Rp P Fb H g i(XDPrim p) <->xdentry_agree Sg Rp P Fb H g i(XDPrim p).
Proof.
 intros g i p;unfold provision_entry;destruct(xp_immutable P);[destruct i;reflexivity|reflexivity].
Qed.
Lemma provision_header_primitive_agreement : forall g i p held,
 provision_entry Sg Rp P Fb H g i(XDPrimAt p held) <->xdentry_agree Sg Rp P Fb H g i(XDPrimAt p held).
Proof.
 intros g i p held;unfold provision_entry;destruct(xp_immutable P);[destruct i;reflexivity|reflexivity].
Qed.

Lemma provision_function_primitive : forall g f D S p,
 g_fn Sg f=Some(D,S,None) ->provision_entry Sg Rp P Fb H g(IFn f)(XDPrim p) ->
 prim_ok Sg Rp P p(map(app_subst g)(fs_params S))(app_subst g(fs_ret S)).
Proof.
 intros g f D S p Sig Entry;unfold provision_entry in Entry;destruct(xp_immutable P);
 cbn[reified_entry xdentry_agree]in Entry;rewrite Sig in Entry;exact Entry.
Qed.
Lemma provision_function_primitive_at : forall g f D S p held,
 g_fn Sg f=Some(D,S,None) ->provision_entry Sg Rp P Fb H g(IFn f)(XDPrimAt p held) ->
 prim_at_ok Sg Rp P p held(map(app_subst g)(fs_params S))(app_subst g(fs_ret S)).
Proof.
 intros g f D S p held Sig Entry;unfold provision_entry in Entry;destruct(xp_immutable P);
 cbn[reified_entry xdentry_agree]in Entry;rewrite Sig in Entry;exact Entry.
Qed.
Lemma provision_method_primitive : forall g rt m th D S p,
 g_mth Sg m=Some(D,S) ->provision_entry Sg Rp P Fb H g(IMth rt m th)(XDPrim p) ->
 prim_ok Sg Rp P p(app_subst g rt::map(fun tau=>app_subst g(app_subst(this_subst rt)(app_subst th tau)))(fs_params S))
 (app_subst g(app_subst(this_subst rt)(app_subst th(fs_ret S)))).
Proof.
 intros g rt m th D S p Sig Entry;unfold provision_entry in Entry;destruct(xp_immutable P);
 cbn[reified_entry xdentry_agree]in Entry;rewrite Sig in Entry;exact Entry.
Qed.
Lemma provision_method_primitive_at : forall g rt m th D S p held,
 g_mth Sg m=Some(D,S) ->provision_entry Sg Rp P Fb H g(IMth rt m th)(XDPrimAt p held) ->
 prim_at_ok Sg Rp P p held(app_subst g rt::map(fun tau=>app_subst g(app_subst(this_subst rt)(app_subst th tau)))(fs_params S))
 (app_subst g(app_subst(this_subst rt)(app_subst th(fs_ret S)))).
Proof.
 intros g rt m th D S p held Sig Entry;unfold provision_entry in Entry;destruct(xp_immutable P);
 cbn[reified_entry xdentry_agree]in Entry;rewrite Sig in Entry;exact Entry.
Qed.

(* Closure code is a real defined function in either environment mode. *)
Lemma provision_function_code : forall g f D S code dc,
 g_fn Sg f=Some(D,S,None) ->provision_entry Sg Rp P Fb H g(IFn f)(XDClo code dc) ->
 exists Dc Sc body,g_fn Sg code=Some(Dc,Sc,None) /\xp_fn P code=Some body.
Proof.
 intros g f D S code dc Sig Entry;unfold provision_entry in Entry;
 destruct(xp_immutable P);cbn[reified_entry xdentry_agree]in Entry;
 rewrite Sig in Entry;
 destruct(g_fn Sg code)as[[[Dc Sc][b|]]|]eqn:Code;try contradiction;
 destruct(xp_fn P code)as[body|]eqn:Body;try contradiction;
 exists Dc,Sc,body;auto.
Qed.
Lemma provision_method_code : forall g rt m th D S code dc,
 g_mth Sg m=Some(D,S) ->provision_entry Sg Rp P Fb H g(IMth rt m th)(XDClo code dc) ->
 exists Dc Sc body,g_fn Sg code=Some(Dc,Sc,None) /\xp_fn P code=Some body.
Proof.
 intros g rt m th D S code dc Sig Entry;unfold provision_entry in Entry;
 destruct(xp_immutable P);cbn[reified_entry xdentry_agree]in Entry;
 rewrite Sig in Entry;
 destruct(g_fn Sg code)as[[[Dc Sc][b|]]|]eqn:Code;try contradiction;
 destruct(xp_fn P code)as[body|]eqn:Body;try contradiction;
 exists Dc,Sc,body;auto.
Qed.

Lemma provision_function_closure : forall g f code dc Df Sf Dc Sc body,
 g_fn Sg f=Some(Df,Sf,None) ->g_fn Sg code=Some(Dc,Sc,None) ->xp_fn P code=Some body ->
 provision_entry Sg Rp P Fb H g(IFn f)(XDClo code dc) ->
 exists PhiC gc,tele_ctx Dc=Some PhiC /\grounding gc /\covers gc(tyreqs(cA PhiC)) /\
 absorbs gc(cS PhiC) /\
 map(app_subst gc)(fs_params Sc)=map(app_subst g)(fs_params Sf) /\
 app_subst gc(fs_ret Sc)=app_subst g(fs_ret Sf) /\
 provision_environment Sg Rp P Fb H gc(cA PhiC)dc.
Proof.
 intros g f code dc Df Sf Dc Sc body Slot Code Body Entry.
 unfold provision_entry in Entry;destruct(xp_immutable P)eqn:Mode;
 cbn[reified_entry xdentry_agree]in Entry;rewrite Slot,Code,Body in Entry.
 all: destruct Entry as[PhiC[gc[Tele[G[Cover[Abs[Params[Result[Available[Closed[Types All]]]]]]]]]]].
 all: exists PhiC,gc;split;[exact Tele|];split;[exact G|];split;[exact Cover|];
      split;[exact Abs|];split;[exact Params|];split;[exact Result|].
 all: apply provision_environment_from_capture;[exact Available| | |exact Types].
 - unfold provision_entry;rewrite Mode;apply reified_all_Forall;exact All.
 - exact Closed.
 - unfold provision_entry;rewrite Mode;apply xdentry_all_Forall;exact All.
 - eapply Forall_impl;[|exact Closed]. intros[i en]Ci;cbn[fst snd]in Ci|-*.
   unfold runtime_dep_tys;destruct i;cbn;try exact Ci;rewrite Mode;exact Ci.
Qed.

Lemma provision_method_closure : forall g rt m th code dc Dm Sm Dc Sc body,
 g_mth Sg m=Some(Dm,Sm) ->g_fn Sg code=Some(Dc,Sc,None) ->xp_fn P code=Some body ->
 provision_entry Sg Rp P Fb H g(IMth rt m th)(XDClo code dc) ->
 exists PhiC gc,tele_ctx Dc=Some PhiC /\grounding gc /\covers gc(tyreqs(cA PhiC)) /\
 absorbs gc(cS PhiC) /\
 map(app_subst gc)(fs_params Sc)=app_subst g rt::
 map(fun tau=>app_subst g(app_subst(this_subst rt)(app_subst th tau)))(fs_params Sm) /\
 app_subst gc(fs_ret Sc)=app_subst g(app_subst(this_subst rt)(app_subst th(fs_ret Sm))) /\
 provision_environment Sg Rp P Fb H gc(cA PhiC)dc.
Proof.
 intros g rt m th code dc Dm Sm Dc Sc body Slot Code Body Entry.
 unfold provision_entry in Entry;destruct(xp_immutable P)eqn:Mode;
 cbn[reified_entry xdentry_agree]in Entry;rewrite Slot,Code,Body in Entry.
 all: destruct Entry as[PhiC[gc[Tele[G[Cover[Abs[Params[Result[Available[Closed[Types All]]]]]]]]]]].
 all: exists PhiC,gc;split;[exact Tele|];split;[exact G|];split;[exact Cover|];
      split;[exact Abs|];split;[exact Params|];split;[exact Result|].
 all: apply provision_environment_from_capture;[exact Available| | |exact Types].
 - unfold provision_entry;rewrite Mode;apply reified_all_Forall;exact All.
 - exact Closed.
 - unfold provision_entry;rewrite Mode;apply xdentry_all_Forall;exact All.
 - eapply Forall_impl;[|exact Closed]. intros[i en]Ci;cbn[fst snd]in Ci|-*.
   unfold runtime_dep_tys;destruct i;cbn;try exact Ci;rewrite Mode;exact Ci.
Qed.

Lemma provision_capture_grounded : forall D Phi g,
 wf_tele Sg empty_ctx D Phi ->grounding g ->covers g(tyreqs(cA Phi)) ->
 absorbs g(cS Phi) ->ctx_grounded g Phi.
Proof.
 intros D Phi g Tele G Cover Abs;split;[exact G|]. split;[exact Abs|]. split;[exact Cover|]. split.
 - eapply tele_sigma_regular;[exact Tele|]. split;[intros x Bad;contradiction|constructor].
 - apply(@callee_env_agree_aux13 Sg empty_ctx D Phi Tele);intros Bad;contradiction.
Qed.

Lemma provision_bind_type : forall Phi t tau d g,
 wf_gsig Sg ->t<>this_sym ->~In(ITy t)(cA Phi) ->wf_ty Sg Phi tau ->
 ctx_grounded g Phi ->provision_environment Sg Rp P Fb H g(cA Phi)d ->
 ctx_grounded((t,app_subst g(app_subst(cS Phi)tau))::g)(bind_ty t tau Phi) /\
 provision_environment Sg Rp P Fb H((t,app_subst g(app_subst(cS Phi)tau))::g)
 (cA(bind_ty t tau Phi))(xbind_ty P d t tau).
Proof.
 intros Phi t tau d g WF NotThis Fresh Wtau Ground Env.
 unfold provision_environment in Env|-*;destruct(xp_immutable P)eqn:Mode.
 - destruct Env as[Env Closed].
   destruct(reified_bind_type Mode WF Closed NotThis Fresh Wtau Ground Env)as[GroundNew EnvNew].
   split;[exact GroundNew|]. split;[exact EnvNew|].
   cbn[cA bind_ty];unfold reified_scope_closed in Closed|-*;constructor.
   + cbn[runtime_dep_tys dep_tys item_ty_frees method_item_frees];intros x Bad;contradiction.
   + eapply Forall_impl;[|exact Closed]. intros i Ci x Hin;right;apply Ci;exact Hin.
 - destruct(world_bindty WF NotThis Fresh Wtau Ground Env)as[GroundNew[EnvNew Lookup]].
   split;assumption.
Qed.

End Provision.

Lemma provision_entry_mono : forall Sg Rp P Fb H H',sext H H' ->forall g i en,
 provision_entry Sg Rp P Fb H g i en ->provision_entry Sg Rp P Fb H' g i en.
Proof.
 intros Sg Rp P Fb H H' Extend g i en Entry;unfold provision_entry in Entry|-*.
 destruct(xp_immutable P);[eapply reified_entry_mono|eapply xdentry_agree_mono];eassumption.
Qed.
Lemma provision_environment_mono : forall Sg Rp P Fb H H',sext H H' ->forall g A d,
 provision_environment Sg Rp P Fb H g A d ->provision_environment Sg Rp P Fb H' g A d.
Proof.
 intros Sg Rp P Fb H H' Extend g A d Env;unfold provision_environment in Env|-*.
 destruct(xp_immutable P).
 - destruct Env as[Env Closed];split;[eapply reified_environment_mono;eassumption|exact Closed].
 - eapply xdenv_agree_mono;eassumption.
Qed.
