(* Method requirements of real callee contexts have well formed supported receivers. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTr BodyTy
 CoreMoss Merge CanonTypes SourceClean ContextClean NominalSupport TypeReadingInj CtxWalk CtxMerge
 InfoInvariant InfoCache FnBodyContext LoadedContextSupport SourceSatisfiesAny MainContextInfo
 NeedInfoExec MainRoot LoaderOrigin NeedMapExec InstantiateTr.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma need_mlook_member:forall entries r m sg,
 In((r,m),sg)entries->exists got,A.mlook r m entries=Some got.
Proof.
 intros entries;induction entries as[|[[r0 m0]sg0]rest IH];intros r m sg Hin;[contradiction|].
 cbn[A.mlook];destruct(ty_eqb r r0&&Nat.eqb m m0)eqn:Same;[eexists;reflexivity|].
 destruct Hin as[Eq|Hin].
 - assert(Er:ty_eqb r r0=true)by(apply ty_eqb_eq;congruence).
   assert(Em:Nat.eqb m m0=true)by(apply Nat.eqb_eq;congruence).
   rewrite Er,Em in Same;discriminate Same.
 - exact(IH _ _ _ Hin).
Qed.
Lemma ctx_need_method:forall W f C ns r m,
 A.CtxOf W f C ns->In(A.mkey r m)ns->
 (exists sg,A.mlook r m(A.c_mths C)=Some sg)/\A.canon C r=r.
Proof.
 intros W f C ns r m[d[es[tm[er[tmr[sigma[Decl[Items[Root[MG[-> Needs]]]]]]]]]]]Hin.
 destruct Needs as[Nd Keys];apply Keys in Hin as(entry&InE&Key).
 destruct entry as[t|v ty ap|fn sg ap|raw mid sg ap|u fx];cbn[A.entry_key]in Key;try discriminate Key.
 injection Key as Eq Mid;subst r mid;split.
 - apply need_mlook_member with(sg:=A.sig_subst sigma sg).
   cbn[A.entries_cx A.c_mths];apply in_flat_map;exists(A.NMth raw m sg ap);split;[exact InE|left;reflexivity].
 - cbn[A.entries_cx A.c_sub A.canon];exact(proj1(proj2 MG)raw).
Qed.
Section Formation.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySound.W w R Fl).
Notation W:=(BodySound.W w R Fl).
Definition NeedReceivers(info:fninfo):Prop:=forall r m,In(KyMth r m)(fi_needs info)->
 WFty r/\Support W r.
Lemma fn_context_need_receivers:forall g info,
 FnCtx w R Fl g info->ContextClean(fi_env info)->Tri(en_uf(fi_env info))->EnvSupport W(fi_env info)->
 Forall(fun k=>key_ok k=true)(fi_needs info)->NeedReceivers info.
Proof.
 intros g info[C[Ctx[Sub[Merges[Ty[Val[FnRead[Mth[Fun[Apps[Rows[Canonical Domain]]]]]]]]]]]]Clean TriU Supported Keys r m Hin.
 rewrite Forall_forall in Keys;pose proof(Keys(KyMth r m)Hin)as RawOk;cbn[key_ok]in RawOk.
 assert(KeyIn:In(A.mkey(tr Fl r)m)(map(key_tr Fl)(fi_needs info))).
 {change(In(key_tr Fl(KyMth r m))(map(key_tr Fl)(fi_needs info)));apply in_map;exact Hin. }
 destruct(ctx_need_method _ _ _ _ _ _ Ctx KeyIn)as[Present Fixed].
 assert(Normal:canon(fi_env info)r=r).
 {eapply tr_inj;[exact(@A.wo_fld b W Hworld)| |exact RawOk|].
  - apply canon_ok;[exact(cc_uf _ Clean)|exact RawOk].
  - change(tr Fl(canon(fi_env info)r)=tr Fl r).
    rewrite <-(CanonTypes.tr_canon Fl(fi_env info)r TriU).
    rewrite <-Sub;exact Fixed. }
 destruct Present as[sg Present].
 pose proof(Mth r m RawOk Normal)as Reading;rewrite Present in Reading.
 destruct(get_mth(fi_env info)r m)as[rawsg|]eqn:Lookup;[|discriminate Reading].
 split;[apply ety_ok_wf;exact RawOk|].
 exact(proj1(@es_mths W(fi_env info)Supported r m rawsg Lookup)).
Qed.
Theorem fn_info_need_receivers:forall g s info sf,
 nget(w_syms w)(gs_id g)=Some g->fn_info g s=Good(info,sf)->
 Inv s->InfoInv s->st_w s=w->Later w P R Fl sf->NeedReceivers info.
Proof.
 intros g s info sf Loaded Run Hi Cache Hw Late.
 pose proof(fn_info_context_loaded w R Fl b Hload Hworld g s info sf Loaded Run Hi Cache Hw Late)as Ctx.
 destruct(fn_info_valid_loaded w R Fl b Hload Hworld g s info sf Loaded Run Hi Cache Hw Late)as[Clean[TriU Supported]].
 assert(Keys:Forall(fun k=>key_ok k=true)(fi_needs info)).
 {destruct(is_main_total g s)as[main Main].
  pose proof(is_main_spec w R Fl b Hload g s main s Loaded Hw Main)as Spec.
  destruct main.
  - assert(TrueMain:A.IsMainFn W(gs_id g))by(apply(proj1 Spec);reflexivity).
    exact(proj2(proj2(fn_info_main w (P:=P) R Fl b Hload Hworld(@A.wo_fld b W Hworld)
     g s info sf Loaded TrueMain Run Hi Cache Hw Late))).
  - assert(NotMain:~A.IsMainFn W(gs_id g))by(intro M;apply(proj2 Spec)in M;discriminate M).
    exact(fn_info_needs_ok_any_nonroot w R Fl b Hload g s info sf Loaded
     (loaded_no_forwarder b w g Hload Loaded)(not_main w R Fl b Hload g Loaded NotMain)
     Run Hi Cache Hw Late). }
 exact(fn_context_need_receivers g info Ctx Clean TriU Supported Keys).
Qed.
Theorem need_site_receiver_formed:forall c e F C info r m,
 CxRel w R Fl c e F C->NeedReceivers info->In(KyMth r m)(fi_needs info)->
 WFty(canon e(instantiate e r))/\Support W(canon e(instantiate e r))/\
 canon e(canon e(instantiate e r))=canon e(instantiate e r).
Proof.
 intros c e F C info r m HC Needs Hin;destruct(Needs r m Hin)as[WF Supp].
 destruct(caller_types_ok w R Fl b Hload c e F C HC)as[U T].
 assert(Raw:ety_ok r=true)by exact(TySupport.support_clean(SourceSupport.loaded_record_unowned w R Fl b Hload)Supp WF).
 split;[apply ety_ok_wf;apply canon_ok;[exact U|apply instantiate_ok;assumption]|].
 split.
 - apply canon_support;[exact(es_uf(cr_support _ _ _ _ _ _ _ HC))|].
   exact(instantiate_support(cr_support _ _ _ _ _ _ _ HC)Supp).
 - apply canon_idem;exact(proj1(cr_uf _ _ _ _ _ _ _ HC)).
Qed.
End Formation.
