(* A functor's own input context is the initial context for its linked body. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound BodySoundCore BodyTr BodyTy
 FunctorContext FunctorContextFacts CtxWalk CtxMerge ContextClean CanonTypes
 FnSnapshot FunctorDecl KeyTranslation NominalSupport Merge SourceClean.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Open Scope string_scope.
Definition functor_body_context(g:gsym)(pins:list nat):cx:=
 Cx(gs_mod g)(FSig [] YUnit None)None 0[]""[]pins.
Definition functor_body_frame(w:Elab.world)(g:gsym)(pins:list nat)(variables:list(name*nat)):A.frame:=
 A.Frame(gm_path(get_mod w(gs_mod g)))TUnit None false variables[][]pins.
Section Context.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySoundCore.W w R Fl).
Lemma functx_initial_body : forall g raw C needs entries tm pins variables,
 let e:=freeze_all_apps raw in
 A.FunCtx W(gs_id g)C(map(key_tr Fl)needs) -> FunEnvTables Fl e C ->
 WS Fl(en_uf raw)raw tm entries needs ->
 C=A.entries_cx(suf Fl(en_uf raw))tm(map(tr_ent Fl)entries) ->
 EnvSupport W e -> en_locals e=[] -> In(gs_mod g)(w_order w) ->
 CxRel w R Fl(functor_body_context g pins)e(functor_body_frame w g pins variables)C.
Proof.
 intros g raw C needs entries tm pins variables e Context Tables Formed Eq Supported Locals Mod.
 assert(Clean:ContextClean e).
 {destruct(ws_clean _ _ _ _ _ _ Formed);constructor;assumption. }
 pose proof(ws_tri _ _ _ _ _ _ Formed)as Triangular.
 assert(CLocals:A.c_locals C=[]).
 {rewrite Eq;reflexivity. }
 constructor;cbn[functor_body_context functor_body_frame cx_mod cx_sig cx_this cx_loop cx_pins cx_applying cx_fresh
  A.fr_mod A.fr_ret A.fr_this A.fr_loop A.fr_pins A.fr_applying A.fr_fresh sg_ret].
 - exact Mod.
 - reflexivity.
 - reflexivity.
 - constructor.
 - reflexivity.
 - exact I.
 - reflexivity.
 - split;[exact Triangular|apply context_clean_uf_wf;exact Clean].
 - exact(fet_sub _ _ _ Tables).
 - intro n;rewrite CLocals,Locals;reflexivity.
 - intros n p Bad;rewrite Locals in Bad;discriminate.
 - exact(fet_vals _ _ _ Tables).
 - intros v t At;eapply context_clean_vals_wf;eassumption.
 - exact(fet_ty _ _ _ Tables).
 - intros t y At;eapply context_clean_types_wf;eassumption.
 - intro f;rewrite(fet_fns _ _ _ Tables);destruct(nget(en_fns e)f)as[entry|];[|reflexivity].
   cbn[option_map];unfold csig;rewrite map_map;reflexivity.
 - intros receiver method Good.
   rewrite(fet_mths _ _ _ Tables(canon e receiver)method Good(canon_idem _ _ Triangular)).
   destruct(get_mth e(canon e receiver)method)as[entry|];[|reflexivity].
   cbn[option_map];unfold csig;rewrite map_map;reflexivity.
 - exact(cc_fns e Clean).
 - exact(cc_mths e Clean).
 - exact Supported.
 - constructor.
 - exact I.
 - intros receiver method Good;exact(fet_apps _ _ _ Tables(KyMth receiver method)eq_refl Good).
 - intro v;exact(fet_apps _ _ _ Tables(KyVal v)eq_refl eq_refl).
 - intro f;exact(fet_apps _ _ _ Tables(KyFn f)eq_refl eq_refl).
 - intro u;exact(fet_apps _ _ _ Tables(KyFun u)eq_refl eq_refl).
 - intro u;exact(proj2(fet_funs _ _ _ Tables u)).
 - intros u fx x tx At Hin;eapply(functx_functor_captures w R Fl(gs_id g)e C _ Context);try eassumption.
   intros k Nofun Good;exact(fet_apps _ _ _ Tables k Nofun Good).
 - reflexivity.
 - reflexivity.
 - reflexivity.
 - intros u fx At;eapply functx_functor_decls;eassumption.
 - eapply functx_fn_snapshots;exact Context.
 - rewrite Locals;constructor.
 - intros m rows At;split.
   + exact(ws_method_unique _ _ _ _ _ _ Formed _ _ At).
   + exact(ws_method_canonical _ _ _ _ _ _ Formed eq_refl _ _ At).
 - intros receiver method signature At;rewrite Eq in At.
   eapply(ctx_mths_domain Fl(en_uf raw)raw tm entries needs Formed eq_refl);exact At.
Qed.
End Context.
