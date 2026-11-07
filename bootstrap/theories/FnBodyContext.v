From NM Require Import CanonBodyTy.
(* A loaded declaration's own checked context is the body-level CxRel,
   before parameter locals are installed. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame InfoInvariant
 BodySoundCore BodyTy BodyTr Merge CanonTypes CtxWalk CtxMerge CtxMethodExec InfoCache
 ContextClean ContextMergeSupport NominalSupport FnContextFunctors ContextFixed
 FunctorDecl MainRoot MainContextInfo SourceSatisfiesAny LoaderOrigin LoadedContextSupport
 FnSigExec ProviderSig SourceClean ContextSupport ContextFnSnapshots.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Definition initial_body_frame(w:Elab.world)(Fl:string->nat)(g:gsym)(e:env)(sg:Elab.fsig)
    (variables:list(name*nat)):A.frame:=
 A.Frame(gm_path(get_mod w(gs_mod g)))(tr Fl(canon e(sg_ret sg)))
   (option_map(fun t=>tr Fl(canon e t))(sg_this sg))false variables[][][].
Definition initial_body_context(g:gsym)(sg:Elab.fsig):cx:=Cx(gs_mod g)sg(sg_this sg)0[]""[][].

Lemma fn_info_loaded_locals_empty:forall b w g s info final,
 load_world b=Good w->st_w s=w->nget(w_syms w)(gs_id g)=Some g->InfoInv s->
 fn_info g s=Good(info,final)->en_locals(fi_env info)=[].
Proof.
 intros b w g s info final Load Hw Loaded Cache Run.
 unfold fn_info in Run;apply bind_inv in Run as[snapshot[mid[Read Run]]];injection Read as <- <-.
 destruct(nget(st_info s)(gs_id g))as[old|]eqn:Lookup.
 - injection Run as <- <-;destruct(Cache _ _ Lookup)as[(g0&s0&e&needs&s1&Sym&Inv&Walk&->&Frame)|[Missing _]];
     [reflexivity|].
   exfalso;specialize(Missing b ltac:(rewrite Hw;exact Load));rewrite Hw,Loaded in Missing;discriminate.
 - apply bind_inv in Run as[g0[s1[Sym Run]]].
   apply bind_inv in Run as[[e needs][s2[Walk Run]]].
   apply bind_inv in Run as[u[s3[Update Run]]];injection Run as <- _;reflexivity.
Qed.

Section BodyContext.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Notation WW:=(BodySoundCore.W w R Fl).
Lemma fn_ctx_initial_body:forall g info sg variables,
 FnCtx w R Fl g info ->ContextClean(fi_env info)->Tri(en_uf(fi_env info))->
 EnvSupport WW(fi_env info)->en_locals(fi_env info)=[]->In(gs_mod g)(w_order w)->
 SigOK sg->SigSupport WW sg->
 exists C,A.CtxOf WW(gs_id g)C(map(key_tr Fl)(fi_needs info))/\
 CxRel w R Fl(initial_body_context g sg)(fi_env info)
   (initial_body_frame w Fl g(fi_env info)sg variables)C.
Proof.
 intros g info sg variables[C[Ctx[Sub[Merges[Types[Values[Functions[Methods[Functors[Apps[Rows[Canonical Domain]]]]]]]]]]]]
   Clean Tri Support Locals Mod SigClean SigSupported.
 set(e:=fi_env info)in *.
 pose proof(ctx_of_ie_complete WW(gs_id g)C _ Ctx)as Formed.
 assert(CLocals:A.c_locals C=[]).
 {destruct Formed as(d&es&tm&er&tmr&sigma&Decl&Items&Root&Merge&Eq&Needs);rewrite Eq;reflexivity. }
 destruct SigClean as[Params[Ret This]].
 destruct SigSupported as[ParamsSupport[RetSupport ThisSupport]].
 exists C;split;[exact Formed|].
 constructor;cbn[initial_body_context initial_body_frame cx_mod cx_sig cx_this cx_loop cx_pins cx_applying cx_fresh
   A.fr_mod A.fr_ret A.fr_this A.fr_loop A.fr_pins A.fr_applying A.fr_fresh].
 - exact Mod.
 - reflexivity.
 - reflexivity.
 - apply ety_ok_wf;exact Ret.
 - reflexivity.
 - destruct(sg_this sg);[apply ety_ok_wf;exact This|exact I].
 - reflexivity.
 - split;[exact Tri|apply context_clean_uf_wf;exact Clean].
 - exact Sub.
 - intro n;rewrite CLocals,Locals;reflexivity.
 - intros n p Bad;rewrite Locals in Bad;discriminate.
 - exact Values.
 - intros v t At;apply context_clean_vals_wf with(e:=e)(n:=v);assumption.
 - exact Types.
 - intros t y At;eapply context_clean_types_wf;eassumption.
 - intro f;rewrite Functions;destruct(nget(en_fns e)f)as[entry|];[|reflexivity].
   cbn[option_map];unfold csig;rewrite map_map;reflexivity.
 - intros receiver method Good.
   rewrite(Methods(canon e receiver)method Good(canon_idem _ _ Tri)).
   destruct(get_mth e(canon e receiver)method)as[entry|];[|reflexivity].
   cbn[option_map];unfold csig;rewrite map_map;reflexivity.
 - exact(cc_fns e Clean).
 - exact(cc_mths e Clean).
 - exact Support.
 - exact RetSupport.
 - destruct(sg_this sg);exact ThisSupport.
 - intros receiver method Good;exact(Apps(KyMth receiver method)eq_refl Good).
 - intro v;exact(Apps(KyVal v)eq_refl eq_refl).
 - intro f;exact(Apps(KyFn f)eq_refl eq_refl).
 - intro u;exact(Apps(KyFun u)eq_refl eq_refl).
 - intro u;exact(proj2(Functors u)).
 - intros u fx x tx At Hin.
   exact(proj2(fn_context_functor_captures w R Fl(gs_id g)e C _ Formed Apps u fx At)x tx Hin).
 - reflexivity.
 - reflexivity.
 - reflexivity.
 - intros u fx At;exact(ctxof_functor_decls WW(gs_id g)C _ Formed u fx At).
 - exact(ctxof_fn_snapshots WW(gs_id g)C _ Formed).
 - rewrite Locals;constructor.
 - intros m rows At;split;[exact(Rows _ _ At)|exact(Canonical _ _ At)].
 - exact Domain.
Qed.
End BodyContext.

Section LoadedContext.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Theorem fn_info_context_loaded:forall g s info final,
 nget(w_syms w)(gs_id g)=Some g->fn_info g s=Good(info,final)->
 Inv s->InfoInv s->st_w s=w->Later w P R Fl final->FnCtx w R Fl g info.
Proof.
 intros g s info final Loaded Run Inv Cache Hw Late.
 assert(Finj:forall a c,Fl a=Fl c->a=c)by exact(A.wo_fld Hworld).
 destruct(is_main_total g s)as[main Main].
 pose proof(is_main_spec w R Fl b Hload g s main s Loaded Hw Main)as Spec.
 destruct main.
 - assert(IsMain:A.IsMainFn(BodySoundCore.W w R Fl)(gs_id g))by(apply(proj1 Spec);reflexivity).
   exact(proj1(fn_info_main w(P:=P)R Fl b Hload Hworld Finj g s info final
     Loaded IsMain Run Inv Cache Hw Late)).
 - assert(NotMain:~A.IsMainFn(BodySoundCore.W w R Fl)(gs_id g)).
   {intro IsMain;apply(proj2 Spec)in IsMain;discriminate. }
   eapply fn_info_ctx_any;[exact Hload|exact Finj|exact(meth_step_holds w R Fl b Hload)|
     exact Loaded|exact(loaded_no_forwarder b w g Hload Loaded)|exact NotMain|
     exact(not_main w R Fl b Hload g Loaded NotMain)|exact Run|exact Inv|exact Cache|exact Hw|exact Late].
Qed.
Lemma fn_sig_receiver:forall g tm e this s sg final,
 fn_sig g tm e this s=Good(sg,final)->sg_this sg=this.
Proof.
 intros g tm e this s sg final Run;unfold fn_sig in Run.
 apply bind_inv in Run as[parameters[mid[Params Run]]].
 apply bind_inv in Run as[result[last[Ret Run]]];injection Run as <- <-;reflexivity.
Qed.

Theorem loaded_function_body_context:forall g s info si sr sg final this variables receiver name parameters result body,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn receiver name parameters result body->
 fn_info g s=Good(info,si)->fn_sig g(tymap(fi_env info))(fi_env info)this sr=Good(sg,final)->
 Inv s->InfoInv s->Inv sr->Fr si sr->st_w s=w->Later w P R Fl final->
 (match this with None=>True|Some t=>ety_ok t=true/\Support(BodySoundCore.W w R Fl)t end)->
 exists C,A.CtxOf(BodySoundCore.W w R Fl)(gs_id g)C(map(key_tr Fl)(fi_needs info))/\
 A.SigOf(BodySoundCore.W w R Fl)(gs_id g)(A.c_ty C)
   (option_map(fun t=>tr Fl(canon(fi_env info)t))this)(canonical_sig Fl(fi_env info)sg)/\
 CxRel w R Fl(initial_body_context g sg)(fi_env info)
   (initial_body_frame w Fl g(fi_env info)sg variables)C/\sg_this sg=this/\
 SigOK sg/\SigSupport(BodySoundCore.W w R Fl)sg.
Proof.
 intros g s info si sr sg final this variables receiver name parameters result body
   Loaded Decl Info Signature Inv Cache InvR ReceiverFrame Hw Late This.
 destruct(ok_fn_info_frame _ _ _ _ Inv Info)as[InvI FrameI].
 assert(Wi:st_w si=w)by(rewrite(fr_w _ _ FrameI);exact Hw).
 assert(Os:ok(fn_sig g(tymap(fi_env info))(fi_env info)this))by(unfold fn_sig;okt).
 assert(Wr:st_w sr=w)by(rewrite(fr_w _ _ ReceiverFrame);exact Wi).
 destruct(Os _ _ _ InvR Signature)as[_ FrameS].
 pose proof(later_back w P R Fl _ _ FrameS Late)as LateR.
 pose proof(later_back w P R Fl _ _ ReceiverFrame LateR)as LateI.
 destruct(fn_info_valid_loaded w(P:=P)R Fl b Hload Hworld g s info si Loaded Info Inv Cache Hw LateI)
   as[Clean[Tri Support]].
 pose proof(fn_info_context_loaded g s info si Loaded Info Inv Cache Hw LateI)as Context.
 assert(Tm:TmClean(tymap(fi_env info)))by(apply context_clean_tymap;exact Clean).
 assert(ThisClean:match this with None=>True|Some t=>ety_ok t=true end).
 {destruct this;[exact(proj1 This)|exact I]. }
 assert(ThisWf:opt_wf this).
 {destruct this;[apply ety_ok_wf;exact(proj1 This)|exact I]. }
 pose proof(fn_sig_clean_at w(P:=P)R Fl b Hload g _ _ _ _ _ _ Signature InvR Wr Loaded
   Tm Tm ThisClean Late)as SigClean.
 assert(SigSupported:SigSupport(BodySoundCore.W w R Fl)sg).
 {eapply fn_sig_support_at;[exact Hload|exact Signature|exact InvR|exact Wr|
    exact(loader_mod_in w R Fl b Hload _ _ Loaded)| | |exact Support|
     | |exact Late].
  - intros k y At;rewrite nassoc_tymap in At;eapply tn_get_support;eassumption.
  - intros k y At;rewrite nassoc_tymap in At;eapply context_clean_types_wf;eassumption.
  - intros k y At;eapply context_clean_types_wf;eassumption.
  - destruct this;[split;[exact(proj2 This)|apply ety_ok_wf;exact(proj1 This)]|exact I]. }
 destruct(fn_ctx_initial_body w R Fl g info sg variables Context Clean Tri Support
   (fn_info_loaded_locals_empty b w g s info si Hload Hw Loaded Cache Info)
   (loader_mod_in w R Fl b Hload _ _ Loaded)SigClean SigSupported)as[C[Formed Relation]].
 exists C;split;[exact Formed|];split.
 - eapply fn_sig_canonical;[exact Hload|exact Signature|exact InvR|exact Wr|exact Relation|
     exact Loaded|exact Decl|exact ThisWf|exact Late].
 - split;[exact Relation|];split;[exact(fn_sig_receiver _ _ _ _ _ _ _ Signature)|];split;assumption.
Qed.
End LoadedContext.
