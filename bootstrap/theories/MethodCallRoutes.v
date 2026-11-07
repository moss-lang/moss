From NM Require Import BodyProofTools.
(* Selected provision routes, justified by actual attached/scope tables. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import InfoPreserve Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy
 CanonTypes Merge NominalSupport TySupport SourceSupport MethodLookup MethodSelectExec MethodCallCore DetachedResolve ElabMono ProvidedDirect.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL canon].
Lemma head_tr_some:forall Fl y h,head y=Some h->A.head(tr Fl y)=Some h.
Proof. intros Fl y h H;destruct y;cbn[head]in H;try discriminate H;injection H as <-;reflexivity. Qed.
Section Routes.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Notation tc:=(fun e y=>tr Fl(canon e y)).
Lemma method_receiver_clean:forall c e F C r,
 CxRel w R Fl c e F C->WFty r->Support WW r->ety_ok(canon e r)=true.
Proof.
 intros c e F C r Hc Wr Sr.
 apply(support_clean (loaded_record_unowned w R Fl b Hload)).
 - apply canon_support;[exact(@es_uf WW e(cr_support _ _ _ _ _ _ _ Hc))|exact Sr].
 - apply canon_wf;[exact(proj2(cr_uf _ _ _ _ _ _ _ Hc))|exact Wr].
Qed.
Lemma detached_scope:forall c e F C n,
 CxRel w R Fl c e F C->A.w_scope WW(A.fr_mod F)A.NSDetached n=
 option_map A.ESym(sassoc n(gm_detached(get_mod w(cx_mod c)))).
Proof.
 intros c e F C n Hc;rewrite(cr_mod _ _ _ _ _ _ _ Hc).
 change(A.w_scope(W_read w R Fl)(gm_path(get_mod w(cx_mod c)))A.NSDetached n=option_map A.ESym(sassoc n(gm_detached(get_mod w(cx_mod c))))).
 rewrite(loaded_scope_table b w R Fl Hload(cx_mod c)A.NSDetached n(cr_mod_in _ _ _ _ _ _ _ Hc));reflexivity.
Qed.
Lemma detached_provision_absent:forall c e F C r n,
 CxRel w R Fl c e F C->ety_ok(canon e r)=true->
 (forall d,sassoc n(gm_detached(get_mod w(cx_mod c)))=Some d->get_mth e(canon e r)d=None)->
 ~(exists m,A.w_scope WW(A.fr_mod F)A.NSDetached n=Some(A.ESym m)/\A.mlook(tc e r)m(A.c_mths C)<>None).
Proof.
 intros c e F C r n Hc Clean Absent(m&Scope&Present).
 rewrite(detached_scope c e F C n Hc)in Scope.
 destruct(sassoc n(gm_detached(get_mod w(cx_mod c))))as[d|]eqn:E;[|discriminate Scope].
 injection Scope as <-;apply Present.
 rewrite(cr_mths _ _ _ _ _ _ _ Hc r d Clean),(Absent d eq_refl);reflexivity.
Qed.
Lemma provision_wins_exec:forall f,OK w P R Fl f->ProvidedFamilyStep w P R Fl f->
 forall c e F C ox r n h att d dg sg args s x t s',
 CxRel w R Fl c e F C->WFty r->Support WW r->Inv s->InfoInv s->FwdInv s->Post w P R Fl s' F->
 head(canon e r)=Some h->attached_call_lookup(cx_mod c)h n s=Good(Some att,s)->st_w s=w->
 sassoc n(gm_detached(get_mod w(cx_mod c)))=Some d->sym d s=Good(dg,s)->get_mth e(canon e r)d=Some sg->
 provided_call f c e ox r dg(map AA args)s=Good((x,t),s')->
 A.MethCall WW P F C ox(tc e r)[n]args x(tc e t)/\WFty t/\Support WW t.
Proof.
 intros f IH HF c e F C ox r n h att d dg sg args s x t s' Hc Wr Sr Hi Ii Hfi Hp Head Lookup Hw Det Sym Mth Call.
 destruct(attached_call_lookup_any w R Fl b Hload(cx_mod c)h n s(Some att)s(cr_mod_in _ _ _ _ _ _ _ Hc)Hi Hw Lookup)as[_[Ha Hatt]].
 assert(Id:gs_id dg=d).
 {unfold sym in Sym;destruct(nget(st_syms s)d)as[g|]eqn:E;[|discriminate Sym];injection Sym as <-;exact(iv_ids _ Hi _ _ E). }
 destruct(@provided_step w P R Fl b Hload f IH HF c e ox r dg args s x t s' Call F C Hi Ii Hfi Hc Hp Wr Sr)as[Prov[Wt St]].
 split;[|split;assumption].
 eapply A.MCProvisionWins with(h:=h)(att:=gs_id att)(m:=d).
 - exact(head_tr_some Fl(canon e r)h Head).
 - exact Ha.
 - rewrite(detached_scope c e F C n Hc),Det;reflexivity.
 - rewrite(cr_mths _ _ _ _ _ _ _ Hc r d(method_receiver_clean c e F C r Hc Wr Sr)),Mth;discriminate.
 - rewrite<-Id;exact Prov.
Qed.
Lemma attached_abstract_ast:forall h n att,
 A.Attached WW h n(gs_id att)->nget(w_syms w)(gs_id att)=Some att->fn_body att=None->
 exists rn ps rt,A.ast_of WW(gs_id att)=Some(ADFn(RAttached rn)n ps rt None).
Proof.
 intros h n att Ha Hg Hb.
 apply(proj1(lr_attached _ _(load_world_reads b w R Fl Hload)h n(gs_id att)))in Ha.
 destruct Ha as(d&rn&ps&rt&body&Hd&D&Hrest).
 change(A.w_decl WW(gs_id att)=Some d)in Hd.
 rewrite(w_decl_rd w R Fl _ _ Hg)in Hd;injection Hd as <-;cbn[A.d_ast]in D.
 unfold fn_body in Hb;rewrite D in Hb;subst body.
 exists rn,ps,rt;unfold A.ast_of;rewrite(w_decl_rd w R Fl _ _ Hg);cbn;rewrite D;reflexivity.
Qed.
Lemma attached_abstract_exec:forall f,OK w P R Fl f->ProvidedFamilyStep w P R Fl f->
 forall c e F C ox r n h att args s x t s',
 CxRel w R Fl c e F C->WFty r->Support WW r->Inv s->InfoInv s->FwdInv s->Post w P R Fl s' F->
 head(canon e r)=Some h->attached_call_lookup(cx_mod c)h n s=Good(Some att,s)->st_w s=w->fn_body att=None->
 (forall d,sassoc n(gm_detached(get_mod w(cx_mod c)))=Some d->get_mth e(canon e r)d=None)->
 provided_call f c e ox r att(map AA args)s=Good((x,t),s')->
 A.MethCall WW P F C ox(tc e r)[n]args x(tc e t)/\WFty t/\Support WW t.
Proof.
 intros f IH HF c e F C ox r n h att args s x t s' Hc Wr Sr Hi Ii Hfi Hp Head Lookup Hw Hb Absent Call.
 destruct(attached_call_lookup_any w R Fl b Hload(cx_mod c)h n s(Some att)s(cr_mod_in _ _ _ _ _ _ _ Hc)Hi Hw Lookup)as[_[Ha Hatt]].
 destruct(@provided_step w P R Fl b Hload f IH HF c e ox r att args s x t s' Call F C Hi Ii Hfi Hc Hp Wr Sr)as[Prov[Wt St]].
 split;[|split;assumption].
 eapply A.MCAttachedAbstract with(h:=h)(att:=gs_id att).
 - exact(head_tr_some Fl(canon e r)h Head).
 - exact Ha.
 - exact(detached_provision_absent c e F C r n Hc(method_receiver_clean c e F C r Hc Wr Sr)Absent).
 - exact(attached_abstract_ast h n att Ha Hatt Hb).
 - exact Prov.
Qed.
Lemma qualified_route_exec:forall f,OK w P R Fl f->ProvidedFamilyStep w P R Fl f->
 forall c e F C ox r pa args s x t s',
 CxRel w R Fl c e F C->WFty r->Support WW r->Inv s->InfoInv s->FwdInv s->Post w P R Fl s' F->st_w s=w->
 List.length pa>1->method_after_receiver f c e ox r pa args s=Good((x,t),s')->
 A.MethCall WW P F C ox(tc e r)pa args x(tc e t)/\WFty t/\Support WW t.
Proof.
 intros f IH HF c e F C ox r pa args s x t s' Hc Wr Sr Hi Ii Hfi Hp Hw Len E.
 destruct(method_qualified_exec f c e ox r pa args s x t s' Len E)as(mid&m&Ed&Es&Call).
 destruct(@rdetached_rel b w R Fl(cx_mod c)pa s(Some mid)s Hload(cr_mod_in _ _ _ _ _ _ _ Hc)Hw Ed)as[_ Res].
 assert(Id:gs_id m=mid).
 {unfold sym in Es;destruct(nget(st_syms s)mid)as[g|]eqn:G;[|discriminate Es];injection Es as <-;exact(iv_ids _ Hi _ _ G). }
 destruct(@provided_step w P R Fl b Hload f IH HF c e ox r m args s x t s' Call F C Hi Ii Hfi Hc Hp Wr Sr)as[Prov[Wt St]].
 split;[|split;assumption].
 eapply A.MCQualified with(m:=mid);[exact Len| |rewrite<-Id;exact Prov].
 rewrite(cr_mod _ _ _ _ _ _ _ Hc);exact Res.
Qed.
Lemma context_canon_tc:forall c e F C r,CxRel w R Fl c e F C->A.canon C(tc e r)=tc e r.
Proof.
 intros c e F C r Hc;unfold A.canon;rewrite(cr_sub _ _ _ _ _ _ _ Hc).
 rewrite tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
 rewrite canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc));reflexivity.
Qed.
Theorem method_qualified_sound:forall f,OK w P R Fl f->ProvidedFamilyStep w P R Fl f->
 forall c e o pa args exp s x t s' F C,
 synth(S f)c e(AEMeth o pa args)exp s=Good((x,t),s')->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl s' F->List.length pa>1->
 A.Ex WW P F C(AEMeth o pa args)(option_map(tc e)exp)x(tc e t)/\WFty t/\Support WW t.
Proof.
 intros f IH HF c e o pa args exp s x t s' F C E Hi Ii Hfi Hc Hp Len.
 cbn[synth]in E;apply bind_inv in E as([v vt]&sm&Em&E).
 apply bind_inv in E as(u&si&Ei&E);injection E as <- <- <-.
 destruct(ok_synth_method f c e o pa args s(v,vt)sm Hi Em)as[Im Fm].
 destruct(ok_inject_check e vt exp sm u si Im Ei)as[_ Fi].
 pose proof(post_back w P R Fl _ _ F Fi Hp)as Pm.
 pose proof(mono_group f)as Mon;do 14(apply proj2 in Mon);apply proj1 in Mon.
 specialize(Mon c e o pa args s((v,vt),sm)Em).
 rewrite synth_method_view in Mon;apply bind_inv in Mon as([ox ot]&sr&Er&Tail).
 destruct(ok_synth f c e o None s(ox,ot)sr Hi Er)as[Ir Fr].
 destruct(ok_method_after_receiver f c e ox ot pa args sr(v,vt)sm Ir Tail)as[_ Ft].
 pose proof(post_back w P R Fl _ _ F Ft Pm)as Pr.
 destruct(bs_synth _ _ _ _ _ IH c e o None s ox ot sr Er F C Hi Ii Hfi Hc I I Pr)as[Exo[Wr Sr]].
 assert(Iir:InfoInv sr)by cache_inv.
 destruct(qualified_route_exec f IH HF c e F C ox ot pa args sr v vt sm Hc Wr Sr Ir Iir ltac:(fwd_inv) Pm
   (lt_w _ _ _ _ _(proj1 Pr))Len Tail)as[Mc[Wt St]].
 split;[|split;assumption].
 eapply A.ExMeth;[exact Exo|rewrite(context_canon_tc c e F C ot Hc);exact Mc].
Qed.
Lemma no_attached_zero:forall n m,~A.Attached WW 0 n m.
Proof.
 intros n m H;unfold A.Attached in H.
 assert(E:nget(w_syms w)0=None).
 {destruct(nget(w_syms w)0)as[g|]eqn:E;[|reflexivity].
  exfalso;exact(loader_nonzero w R Fl b Hload 0 g E eq_refl). }
 change(match nget(w_syms w)0 with Some g=>assoc attached_eqb(0,n)(gm_attached(get_mod w(gs_mod g)))|None=>None end=Some m)in H.
 rewrite E in H;discriminate H.
Qed.
Lemma no_attached_head_tr:forall r n,
 (forall h,head r=Some h->forall m,~A.Attached WW h n m)->
 forall h m,A.head(tr Fl r)=Some h->~A.Attached WW h n m.
Proof.
 intros r n Absent h m Hd;destruct r;cbn[tr A.head]in Hd;try discriminate Hd.
 - injection Hd as <-;exact(Absent _ eq_refl m).
 - injection Hd as <-;exact(Absent _ eq_refl m).
 - injection Hd as <-;apply no_attached_zero.
Qed.
Lemma detached_route_exec:forall f,OK w P R Fl f->ProvidedFamilyStep w P R Fl f->
 forall c e F C ox r n d dg args s x t s',
 CxRel w R Fl c e F C->WFty r->Support WW r->Inv s->InfoInv s->FwdInv s->Post w P R Fl s' F->
 (forall h,head(canon e r)=Some h->attached_call_lookup(cx_mod c)h n s=Good(None,s))->st_w s=w->
 sassoc n(gm_detached(get_mod w(cx_mod c)))=Some d->sym d s=Good(dg,s)->
 provided_call f c e ox r dg(map AA args)s=Good((x,t),s')->
 A.MethCall WW P F C ox(tc e r)[n]args x(tc e t)/\WFty t/\Support WW t.
Proof.
 intros f IH HF c e F C ox r n d dg args s x t s' Hc Wr Sr Hi Ii Hfi Hp Lookup Hw Det Sym Call.
 assert(Id:gs_id dg=d).
 {unfold sym in Sym;destruct(nget(st_syms s)d)as[g|]eqn:E;[|discriminate Sym];injection Sym as <-;exact(iv_ids _ Hi _ _ E). }
 destruct(@provided_step w P R Fl b Hload f IH HF c e ox r dg args s x t s' Call F C Hi Ii Hfi Hc Hp Wr Sr)as[Prov[Wt St]].
 split;[|split;assumption].
 eapply A.MCDetached with(m:=d).
 - apply no_attached_head_tr;intros h Hd m.
   exact(proj2(attached_call_lookup_any w R Fl b Hload(cx_mod c)h n s None s
    (cr_mod_in _ _ _ _ _ _ _ Hc)Hi Hw(Lookup h Hd))m).
 - rewrite(detached_scope c e F C n Hc),Det;reflexivity.
 - rewrite<-Id;exact Prov.
Qed.
Definition AttachedDefinedStep(f:nat):Prop:=
 forall c e F C ox r n h att body args s x t s',
 CxRel w R Fl c e F C->WFty r->Support WW r->Inv s->InfoInv s->FwdInv s->Post w P R Fl s' F->
 head(canon e r)=Some h->attached_call_lookup(cx_mod c)h n s=Good(Some att,s)->st_w s=w->fn_body att=Some body->
 (forall d,sassoc n(gm_detached(get_mod w(cx_mod c)))=Some d->get_mth e(canon e r)d=None)->
 method_attached_code f c e ox r att args s=Good((x,t),s')->
 A.MethCall WW P F C ox(tc e r)[n]args x(tc e t)/\WFty t/\Support WW t.
Lemma attached_route_exec:forall f,OK w P R Fl f->ProvidedFamilyStep w P R Fl f->AttachedDefinedStep f->
 forall c e F C ox r n h att args s x t s',
 CxRel w R Fl c e F C->WFty r->Support WW r->Inv s->InfoInv s->FwdInv s->Post w P R Fl s' F->
 head(canon e r)=Some h->attached_call_lookup(cx_mod c)h n s=Good(Some att,s)->st_w s=w->
 (forall d,sassoc n(gm_detached(get_mod w(cx_mod c)))=Some d->get_mth e(canon e r)d=None)->
 method_attached_code f c e ox r att args s=Good((x,t),s')->
 A.MethCall WW P F C ox(tc e r)[n]args x(tc e t)/\WFty t/\Support WW t.
Proof.
 intros f IH HF HD c e F C ox r n h att args s x t s' Hc Wr Sr Hi Ii Hfi Hp Head Lookup Hw Absent Call.
 destruct(fn_body att)as[body|]eqn:Hbody.
 - exact(HD c e F C ox r n h att body args s x t s' Hc Wr Sr Hi Ii Hfi Hp Head Lookup Hw Hbody Absent Call).
 - unfold method_attached_code in Call;rewrite Hbody in Call;cbn[is_some]in Call.
   exact(attached_abstract_exec f IH HF c e F C ox r n h att args s x t s' Hc Wr Sr Hi Ii Hfi Hp Head Lookup Hw Hbody Absent Call).
Qed.
Theorem singleton_route_exec:forall f,OK w P R Fl f->ProvidedFamilyStep w P R Fl f->AttachedDefinedStep f->
 forall c e F C ox r n args s x t s',
 CxRel w R Fl c e F C->WFty r->Support WW r->Inv s->InfoInv s->FwdInv s->Post w P R Fl s' F->st_w s=w->
 method_singleton_code f c e ox r n args s=Good((x,t),s')->
 A.MethCall WW P F C ox(tc e r)[n]args x(tc e t)/\WFty t/\Support WW t.
Proof.
 intros f IH HF HD c e F C ox r n args s x t s' Hc Wr Sr Hi Ii Hfi Hp Hw Call.
 unfold method_singleton_code in Call.
 apply bind_inv in Call as(here&s1&Eh&Call);unfold the_mod in Eh;injection Eh as <- <-.
 apply bind_inv in Call as(att&s1&Look0&Call).
 assert(State:s1=s/\match att with
 |Some at0=>exists h,head(canon e r)=Some h/\attached_call_lookup(cx_mod c)h n s=Good(Some at0,s)
 |None=>forall h,head(canon e r)=Some h->attached_call_lookup(cx_mod c)h n s=Good(None,s)end).
 {destruct(head(canon e r))as[h|]eqn:Head.
  - destruct(attached_call_lookup_any w R Fl b Hload(cx_mod c)h n s att s1(cr_mod_in _ _ _ _ _ _ _ Hc)Hi Hw Look0)as[-> A].
    split;[reflexivity|]. destruct att as[at0|].
    + exists h;split;[reflexivity|exact Look0].
    + intros h' Hh;injection Hh as <-;exact Look0.
  - injection Look0 as <- <-;split;[reflexivity|]. intros h Hh;discriminate Hh. }
 destruct State as[-> Select];rewrite Hw in Call.
 destruct att as[att|];destruct(sassoc n(gm_detached(get_mod w(cx_mod c))))as[d|]eqn:Det.
 - destruct Select as(h&Head&Lookup).
   apply bind_inv in Call as(dg&s1&Sym&Call);pose proof(sym_state _ _ _ _ Sym)as ->.
   destruct(get_mth e(canon e r)d)as[sg|]eqn:Mth;cbn[is_some]in Call.
   + exact(provision_wins_exec f IH HF c e F C ox r n h att d dg sg args s x t s' Hc Wr Sr Hi Ii Hfi Hp Head Lookup Hw Det Sym Mth Call).
   + eapply attached_route_exec;[exact IH|exact HF|exact HD|exact Hc|exact Wr|exact Sr|exact Hi|exact Ii|exact Hfi|exact Hp|exact Head|exact Lookup|exact Hw| |exact Call].
     intros d' D;rewrite Det in D;injection D as <-;exact Mth.
 - destruct Select as(h&Head&Lookup).
   eapply attached_route_exec;[exact IH|exact HF|exact HD|exact Hc|exact Wr|exact Sr|exact Hi|exact Ii|exact Hfi|exact Hp|exact Head|exact Lookup|exact Hw| |exact Call].
   intros d D;rewrite Det in D;discriminate D.
 - apply bind_inv in Call as(dg&s1&Sym&Call);pose proof(sym_state _ _ _ _ Sym)as ->.
   exact(detached_route_exec f IH HF c e F C ox r n d dg args s x t s' Hc Wr Sr Hi Ii Hfi Hp Select Hw Det Sym Call).
 - destruct(head(canon e r));method_noerr Call.
Qed.
Theorem method_route_exec:forall f,OK w P R Fl f->ProvidedFamilyStep w P R Fl f->AttachedDefinedStep f->
 forall c e F C ox r pa args s x t s',
 CxRel w R Fl c e F C->WFty r->Support WW r->Inv s->InfoInv s->FwdInv s->Post w P R Fl s' F->st_w s=w->
 method_after_receiver f c e ox r pa args s=Good((x,t),s')->
 A.MethCall WW P F C ox(tc e r)pa args x(tc e t)/\WFty t/\Support WW t.
Proof.
 intros f IH HF HD c e F C ox r pa args s x t s' Hc Wr Sr Hi Ii Hfi Hp Hw E.
 destruct pa as[|n[|n2 rest]].
 - exfalso;exact(method_empty_impossible f c e ox r args s x t s' E).
 - apply(singleton_route_exec f IH HF HD c e F C ox r n args s x t s' Hc Wr Sr Hi Ii Hfi Hp Hw).
   exact(method_singleton_exec f c e ox r n args s x t s' E).
 - apply(qualified_route_exec f IH HF c e F C ox r(n::n2::rest)args s x t s' Hc Wr Sr Hi Ii Hfi Hp Hw);[cbn;lia|exact E].
Qed.
Theorem method_sound:forall f,OK w P R Fl f->ProvidedFamilyStep w P R Fl f->AttachedDefinedStep f->
 forall c e o pa args exp s x t s' F C,
 synth(S f)c e(AEMeth o pa args)exp s=Good((x,t),s')->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl s' F->
 A.Ex WW P F C(AEMeth o pa args)(option_map(tc e)exp)x(tc e t)/\WFty t/\Support WW t.
Proof.
 intros f IH HF HD c e o pa args exp s x t s' F C E Hi Ii Hfi Hc Hp.
 cbn[synth]in E;apply bind_inv in E as([v vt]&sm&Em&E).
 apply bind_inv in E as(u&si&Ei&E);injection E as <- <- <-.
 destruct(ok_synth_method f c e o pa args s(v,vt)sm Hi Em)as[Im Fm].
 destruct(ok_inject_check e vt exp sm u si Im Ei)as[_ Fi].
 pose proof(post_back w P R Fl _ _ F Fi Hp)as Pm.
 pose proof(mono_group f)as Mon;do 14(apply proj2 in Mon);apply proj1 in Mon.
 specialize(Mon c e o pa args s((v,vt),sm)Em).
 rewrite synth_method_view in Mon;apply bind_inv in Mon as([ox ot]&sr&Er&Tail).
 destruct(ok_synth f c e o None s(ox,ot)sr Hi Er)as[Ir Fr].
 destruct(ok_method_after_receiver f c e ox ot pa args sr(v,vt)sm Ir Tail)as[_ Ft].
 pose proof(post_back w P R Fl _ _ F Ft Pm)as Pr.
 destruct(bs_synth _ _ _ _ _ IH c e o None s ox ot sr Er F C Hi Ii Hfi Hc I I Pr)as[Exo[Wr Sr]].
 assert(Iir:InfoInv sr)by cache_inv.
 destruct(method_route_exec f IH HF HD c e F C ox ot pa args sr v vt sm Hc Wr Sr Ir Iir ltac:(fwd_inv) Pm
   (lt_w _ _ _ _ _(proj1 Pr))Tail)as[Mc[Wt St]].
 split;[|split;assumption].
 eapply A.ExMeth;[exact Exo|rewrite(context_canon_tc c e F C ot Hc);exact Mc].
Qed.
End Routes.
