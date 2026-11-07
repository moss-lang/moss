(* Actual method output formation preserves the full source context invariant. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTr BodyTy
 Merge CanonTypes CanonBodyTy TySupport CtxWalk CtxMethodExec MethodLookup MethodLookupNone MethodKind MethodKeyTr SigSound FreeTypesExec
 SourceClean SourceSupport FnSigExec NeedMapExec NominalSupport FamilyMethodExec
 FamilyResultFold ProvideResultMethod ProvideResultFunction ProvideResultReading ProvideResultContext.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Local Strategy opaque[fn_sig symbol_type provision_app app_subst ety_of FUEL reqs type_requirements].
#[local]Hint Resolve output_signature_ok output_provided_ok:okdb.
Section MethodContext.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySoundCore.W w R Fl).
Ltac family_step H Hi Hw L a si E Ii Wi Li:=
 match type of H with mbind ?m ?k ?s=Good(?v,?sf)=>
 let O:=fresh"O"in assert(O:ok m)by okt;
 apply bind_inv in H as(a&si&E&H);
 let Fr:=fresh"Fr"in destruct(O _ _ _ Hi E)as[Ii Fr];
 pose proof(fr_w' w _ _ Fr Hw)as Wi;
 let Q:=fresh"Q"in assert(Q:ok(k a))by okt;
 let It:=fresh"It"in let Ft:=fresh"Ft"in destruct(Q _ _ _ Ii H)as[It Ft];
 pose proof(later_back w P R Fl _ _ Ft L)as Li;clear O Q It Ft end.
Theorem provide_method_context:forall c e F C mi recv dn tm tmr out s e' keys s',
 provide_method_code e mi recv dn tm out s=Good((e',keys),s')->
 Inv s->st_w s=w->In mi(w_order w)->nget(w_syms w)(gs_id recv)=Some recv->
 Later w P R Fl s'->CxRel w R Fl c e F C->TmRel Fl tm tmr e->
 (forall k y,nassoc k tm=Some y->Support W y)->
 exists m this sg app stamp,
 e'=set_app(set_mth e(canon e this)(gs_id m)sg)
 (KyMth(canon e this)(gs_id m))(freeze_app(set_mth e(canon e this)(gs_id m)sg)app)stamp/\
 keys=out++[KyMth(canon e this)(gs_id m)]/\
 CxRel w R Fl c e' F
 (A.provide C(A.mkey(tr Fl(canon e this))(gs_id m))
 (output_inst Fl(freeze_app e app))(A.set_mth C(tr Fl(canon e this))(gs_id m)(output_sig Fl e sg))).
Proof.
 intros c e F C mi recv dn tm tmr out s e' keys s' E Hi Hw Hmi Hr L HC TR St.
 pose proof(proj1(proj2 TR))as Wt.
 assert(Owned:A.kind_sym W A.rec_head=None)by exact(loaded_record_unowned w R Fl b Hload).
 assert(Tc:TmClean tm)by(intros k y Ey;exact(support_clean Owned(St _ _ Ey)(Wt _ _ Ey))).
 destruct(caller_types_ok w R Fl b Hload c e F C HC)as[U Tn].
 assert(Tnc:TmClean(tymap e)).
 {intros k y Ey;rewrite CanonBodyTy.nassoc_tymap in Ey;exact(Tn _ _ Ey). }
 unfold provide_method_code in E;apply bind_inv in E as(mo&sm&Find&E).
 destruct mo as[m|];[|exfalso;eapply err_not_good;exact E].
 destruct(find_method_decl_loaded b w R Fl mi recv dn s m sm Hload Hmi Hr Hi Hw Find)as[-> [Hm Dm]].
 family_step E Hi Hw L this sy Et Iy Wy Ly.
 destruct(recvty_exec w R Fl b Hload mi recv tm tmr e s this sy Et Hi Hw Hr TR Ly)as[Rthis Wthis].
 assert(Sthis:Support W this).
 {destruct(nassoc(gs_id recv)tm)as[y|]eqn:Nt.
  - injection Et as <- _;exact(St _ _ Nt).
  - eapply(symbol_type_support_at w R Fl b Hload);[exact Et|exact Hi|exact Hw|exact Hr|exact St|exact Wt|exact(cr_support _ _ _ _ _ _ _ HC)|exact(cr_ty_wf _ _ _ _ _ _ _ HC)|exact Ly]. }
 assert(Othis:ety_ok this=true)by exact(support_clean Owned Sthis Wthis).
 family_step E Iy Wy L sg sz Sig Iz Wz Lz.
 assert(Csg:SigOK sg).
 {eapply(fn_sig_clean_at w R Fl b Hload);[exact Sig|exact Iy|exact Wy|exact Hm|exact Tc|exact Tnc|exact Othis|exact Lz]. }
 assert(Ssg:SigSupport W sg).
 {eapply(fn_sig_support_at w R Fl b Hload);[exact Sig|exact Iy|exact Wy|exact(loader_mod_in w R Fl b Hload _ _ Hm)|exact St|exact Wt|exact(cr_support _ _ _ _ _ _ _ HC)|exact(cr_ty_wf _ _ _ _ _ _ _ HC)|split;assumption|exact Lz]. }
 family_step E Iz Wz L k sk Key Ik Wk Lk.
 destruct(@method_key_value e mi this m sz k sk Key)as[-> ->].
 apply bind_inv in E as(ep&se&Provided&E);injection E as <- <- <-.
 unfold provided_at in Provided;apply bind_inv in Provided as(app&sa&Provision&Provided).
 apply bind_inv in Provided as(stamp&ss&Fresh&Provided);injection Provided as <- <-.
 exists m,this,sg,app,stamp;split;[reflexivity|];split;[reflexivity|].
 eapply(cxrel_method w R Fl Finj);[exact HC| | |exact Csg|exact Ssg| |reflexivity].
 - apply canon_idem;exact(proj1(cr_uf _ _ _ _ _ _ _ HC)).
 - apply canon_ok;assumption.
 - apply canon_support;[exact(@es_uf W e (cr_support _ _ _ _ _ _ _ HC))|exact Sthis].
Qed.
Theorem provide_method_context_read:forall c e F C mi recv dn tm tmr out s e' keys s',
 provide_method_code e mi recv dn tm out s=Good((e',keys),s')->
 Inv s->st_w s=w->In mi(w_order w)->nget(w_syms w)(gs_id recv)=Some recv->
 Later w P R Fl s'->CxRel w R Fl c e F C->TmRel Fl tm(map(trp Fl)tm)e->
 (forall k y,nassoc k tm=Some y->Support W y)->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))->
 exists m this sg app stamp,
 e'=set_app(set_mth e(canon e this)(gs_id m)sg)
 (KyMth(canon e this)(gs_id m))(freeze_app(set_mth e(canon e this)(gs_id m)sg)app)stamp/\
 keys=out++[KyMth(canon e this)(gs_id m)]/\
 A.MethodNamed W(gm_path(get_mod w mi))(gs_id recv)dn(gs_id m)/\
 A.RecvTy W(gs_id recv)tmr(tr Fl(canon e this))/\
 A.SigOf W(gs_id m)tmr(Some(tr Fl(canon e this)))(output_sig Fl e sg)/\
 A.AppOf W(gs_id m)tmr(output_inst Fl(freeze_app e app))/\
 TySupport W(tr Fl(canon e this))/\
 CxRel w R Fl c e' F
 (A.provide C(A.mkey(tr Fl(canon e this))(gs_id m))
 (output_inst Fl(freeze_app e app))(A.set_mth C(tr Fl(canon e this))(gs_id m)(output_sig Fl e sg))).
Proof.
 intros c e F C mi recv dn tm tmr out s e' keys s' E Hi Hw Hmi Hr L HC TR St Mnorm.
 pose proof(proj1(proj2 TR))as Wt.
 assert(Owned:A.kind_sym W A.rec_head=None)by exact(loaded_record_unowned w R Fl b Hload).
 assert(Tc:TmClean tm)by(intros k y Ey;exact(support_clean Owned(St _ _ Ey)(Wt _ _ Ey))).
 destruct(caller_types_ok w R Fl b Hload c e F C HC)as[U Tn].
 assert(Tnc:TmClean(tymap e)).
 {intros k y Ey;rewrite CanonBodyTy.nassoc_tymap in Ey;exact(Tn _ _ Ey). }
 unfold provide_method_code in E;apply bind_inv in E as(mo&sm&Find&E).
 destruct mo as[m|];[|exfalso;eapply err_not_good;exact E].
 destruct(find_method_decl_loaded b w R Fl mi recv dn s m sm Hload Hmi Hr Hi Hw Find)as[-> [Hm Dm]].
 destruct(find_method_decl_named b w R Fl mi recv dn s m s Hload Hmi Hr Hi Hw Find)as[_ Named].
 pose proof(method_named_kind b w R Fl mi(gs_id recv)dn(gs_id m)Hload Hmi Named)as Km.
 destruct Dm as(rcv&nm&params&rt&body&Dm).
 family_step E Hi Hw L this sy Et Iy Wy Ly.
 destruct(recvty_exec w R Fl b Hload mi recv tm(map(trp Fl)tm)e s this sy Et Hi Hw Hr TR Ly)as[Rthis Wthis].
 assert(Sthis:Support W this).
 {destruct(nassoc(gs_id recv)tm)as[y|]eqn:Nt.
  - injection Et as <- _;exact(St _ _ Nt).
  - eapply(symbol_type_support_at w R Fl b Hload);[exact Et|exact Hi|exact Hw|exact Hr|exact St|exact Wt|exact(cr_support _ _ _ _ _ _ _ HC)|exact(cr_ty_wf _ _ _ _ _ _ _ HC)|exact Ly]. }
 assert(Othis:ety_ok this=true)by exact(support_clean Owned Sthis Wthis).
 family_step E Iy Wy L sg sz Sig Iz Wz Lz.
 assert(Csg:SigOK sg).
 {eapply(fn_sig_clean_at w R Fl b Hload);[exact Sig|exact Iy|exact Wy|exact Hm|exact Tc|exact Tnc|exact Othis|exact Lz]. }
 assert(Ssg:SigSupport W sg).
 {eapply(fn_sig_support_at w R Fl b Hload);[exact Sig|exact Iy|exact Wy|exact(loader_mod_in w R Fl b Hload _ _ Hm)|exact St|exact Wt|exact(cr_support _ _ _ _ _ _ _ HC)|exact(cr_ty_wf _ _ _ _ _ _ _ HC)|split;assumption|exact Lz]. }
 destruct(fn_sig_rel_at w R Fl b Hload m tm(map(trp Fl)tm)e(Some this)sy sg sz rcv nm params rt body
 Sig Iy Wy Hm Dm TR Wthis Lz)as[RawSig This].
 family_step E Iz Wz L k sk Key Ik Wk Lk.
 destruct(@method_key_value e mi this m sz k sk Key)as[-> ->].
 apply bind_inv in E as(ep&se&Provided&E);injection E as <- <- <-.
 unfold provided_at in Provided;apply bind_inv in Provided as(app&sa&Provision&Provided).
 apply bind_inv in Provided as(stamp&ss&Fresh&Provided);injection Provided as <- <-.
 destruct(output_provision_ok m tm true _ _ _ Iz Provision)as[Ia Fa].
 destruct(ok_fresh _ _ _ Ia Fresh)as[_ Fs].
 pose proof(later_back w P R Fl _ _ Fs L)as La.
 pose proof(method_provision_app_rel w R Fl b Hload m rcv nm params rt body tm(map(trp Fl)tm)e sz app sa
 Hm Dm Km Provision Iz Wz La TR)as RawApp.
 destruct(canonical_method_reading w R Fl e tm tmr(gs_id recv)(gs_id m)this sg app
 (proj1(cr_uf _ _ _ _ _ _ _ HC))Mnorm Rthis RawSig RawApp)as[Recv[SigNorm AppNorm]].
 exists m,this,sg,app,stamp;split;[reflexivity|];split;[reflexivity|].
 split;[exact Named|];split;[exact Recv|];split;[exact SigNorm|];split;[exact AppNorm|];split.
 - apply support_tr;apply canon_support;[exact(@es_uf W e (cr_support _ _ _ _ _ _ _ HC))|exact Sthis].
 -
 eapply(cxrel_method w R Fl Finj);[exact HC| | |exact Csg|exact Ssg| |reflexivity].
   + apply canon_idem;exact(proj1(cr_uf _ _ _ _ _ _ _ HC)).
   + apply canon_ok;assumption.
   + apply canon_support;[exact(@es_uf W e (cr_support _ _ _ _ _ _ _ HC))|exact Sthis].
Qed.
Theorem family_method_step:forall c e F C f fg mi pa dn a outer ob out s e' keys s',
 provide_spec(S f)e fg mi(Spec(Some pa)(Some dn)a)outer out s=Good((e',keys),s')->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->CxRel w R Fl c e F C->
 Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists r mid sg app,
 A.OutputItemEntries W(gm_path(get_mod w mi))ob(A.c_ty C)(Spec(Some pa)(Some dn)a)
 [A.NMth(tr Fl r)mid sg app](A.c_ty C)/\
 A.canon C(tr Fl r)=tr Fl r/\A.sig_subst(A.c_sub C)sg=sg/\
 keys=out++[KyMth r mid]/\
 TySupport W(tr Fl r)/\
 CxRel w R Fl c e' F(A.provide C(A.mkey(tr Fl r)mid)app(A.set_mth C(tr Fl r)mid sg)).
Proof.
 intros c e F C f fg mi pa dn a outer ob out s e' keys s' E Hi Hw Hmi L HC Wo So O.
 destruct(provide_spec_method_phases _ _ _ _ _ _ _ _ _ _ _ _ _ E)
 as(sub&sx&recv&sr&Sub&Resolve&Method).
 assert(Os:ok(match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end))by okt.
 destruct(Os _ _ _ Hi Sub)as[Ix Fx].
 assert(Wx:st_w sx=w)by(rewrite(fr_w _ _ Fx);exact Hw).
 destruct(output_receiver_resolve w R Fl b Hload _ _ _ _ _ Resolve Ix Wx Hmi)as[->[Res Hr]].
 assert(Om:ok(provide_method_code e mi recv dn(sub++tymap e)out)).
 {unfold provide_method_code;okt. }
 destruct(Om _ _ _ Ix Method)as[_ Fm].
 pose proof(later_back w P R Fl _ _ Fm L)as Lx.
 assert(TR0:TmRel Fl(tymap e)(map(trp Fl)(tymap e))e).
 {apply tmrel_tymap;[exact(cr_ty_wf _ _ _ _ _ _ _ HC)|intro k;rewrite nlook_trp,nassoc_tymap;reflexivity]. }
 assert(TR:TmRel Fl(outer++tymap e)(map(trp Fl)(outer++tymap e))e).
 {rewrite map_app;apply tmrel_app;[exact TR0|reflexivity|exact Wo]. }
 assert(M:forall k,A.nlook k(ob++A.c_ty C)=
 option_map(fun y=>tr Fl(canon e y))(nassoc k(outer++tymap e))).
 {intro k;rewrite <-O,nlook_app,family_canon_lookup,(cr_ty _ _ _ _ _ _ _ HC),nassoc_app,nassoc_tymap.
  destruct(nassoc k outer);reflexivity. }
 destruct(family_output_brackets w R Fl b Hload e mi a outer ob(A.c_ty C)s sub sx Sub Hi Hw Hmi Lx
  (proj1(cr_uf _ _ _ _ _ _ _ HC))TR O Wo M)as(bs&Bmap&Wsub&Br).
 assert(TRs:TmRel Fl(sub++tymap e)(map(trp Fl)(sub++tymap e))e).
 {rewrite map_app;apply tmrel_app;[exact TR0|reflexivity|exact Wsub]. }
 assert(Ms:forall k,A.nlook k(bs++ob++A.c_ty C)=
 option_map(fun y=>tr Fl(canon e y))(nassoc k(sub++tymap e))).
 {intro k;rewrite app_assoc,<-Bmap,nlook_app,family_canon_lookup,
 (cr_ty _ _ _ _ _ _ _ HC),nassoc_app,nassoc_tymap.
 destruct(nassoc k sub);reflexivity. }
 assert(Souter:forall k y,nassoc k outer=Some y->Support W y).
 {intros k y Ey;rewrite Forall_forall in So;exact(So(k,y)(Merge.nassoc_in _ _ _ _ Ey)). }
 assert(Wouter:forall k y,nassoc k outer=Some y->WFty y).
 {intros k y Ey;rewrite Forall_forall in Wo;exact(Wo(k,y)(Merge.nassoc_in _ _ _ _ Ey)). }
 assert(Ssub:Forall(fun q=>Support W(snd q))sub).
 {destruct a as[bs0|].
  - apply bind_inv in Sub as(xs&sy&Read&Ret);injection Ret as <- <-.
    apply Forall_app;split;[|exact So].
    eapply(app_subst_support_at w R Fl b Hload);[exact Read|exact Hi|exact Hw|exact Hmi|exact Souter|exact Wouter|exact(cr_support _ _ _ _ _ _ _ HC)|exact(cr_ty_wf _ _ _ _ _ _ _ HC)|exact Lx].
  - injection Sub as <- _;exact So. }
 assert(Stm:forall k y,nassoc k(sub++tymap e)=Some y->Support W y).
 {intros k y Ey;rewrite nassoc_app in Ey;destruct(nassoc k sub)as[v|]eqn:N.
  - injection Ey as <-;rewrite Forall_forall in Ssub;exact(Ssub(k,v)(Merge.nassoc_in _ _ _ _ N)).
  - rewrite nassoc_tymap in Ey;exact(@tn_get_support W e k y(cr_support _ _ _ _ _ _ _ HC)Ey). }
 destruct(provide_method_context_read c e F C mi recv dn(sub++tymap e)(bs++ob++A.c_ty C)out sx e' keys s'
 Method Ix Wx Hmi Hr L HC TRs Stm Ms)
 as(m&this&sg&app&stamp&Env&Keys&Named&Recv&Sig&App&ReceiverSupport&Ctx).
 exists(canon e this),(gs_id m),(output_sig Fl e sg),(output_inst Fl(freeze_app e app));split.
 - eapply A.OIEMeth;[exact Res|exact Named|exact Br|exact Recv|exact Sig|exact App].
 - split;[exact(family_canonical_receiver w R Fl _ _ _ _ HC this)|].
   split;[exact(family_canonical_signature w R Fl _ _ _ _ HC sg)|].
   split;[exact Keys|split;[exact ReceiverSupport|exact Ctx]].
Qed.
End MethodContext.
