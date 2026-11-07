(* Actual method output reads, including declaration selection and receiver
   instantiation.  Method output brackets do not use input relevance gates. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore
 BodyTy CtxWalk CtxMethodExec MethodLookup MethodClean MethodKind SigSound
 FreeTypesExec MethodKeyTr ProvideResultReading ProvideResultFunction.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Definition provide_method_code(e:env)(mi:nat)(recv:gsym)(dn:name)
 (tm:list(nat*ety))(out:list key):M(env*list key):=
 let!mo:=find_method_decl mi recv dn in
 match mo with
 |None=>err mi("no method `."++dn++"`")
 |Some m=>
 let!this:=match nassoc(gs_id recv)tm with Some y=>ret y|None=>symbol_type 200 mi recv tm e end in
 let!sg:=fn_sig m tm e(Some this)in
 let!k:=method_key e mi this m in
 let e:=match k with KyMth t mid=>set_mth e t mid sg|_=>e end in
 let!ep:=provided_at e k m tm true in ret(ep,out++[k])
 end.
Lemma output_provided_ok:forall e k g tm meth,ok(provided_at e k g tm meth).
Proof. intros;unfold provided_at;apply ok_bind;[apply output_provision_ok|intros;okt]. Qed.
#[local]Hint Resolve output_provided_ok output_signature_ok output_provision_ok:okdb.
Section Method.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Ltac output_step H Hi Hw L a si E Ii Wi Li:=
 match type of H with mbind ?m ?k ?s=Good(?v,?sf)=>
 let O:=fresh"O"in assert(O:ok m)by okt;
 apply bind_inv in H as(a&si&E&H);
 let F:=fresh"F"in destruct(O _ _ _ Hi E)as[Ii F];
 pose proof(fr_w' w _ _ F Hw)as Wi;
 let Q:=fresh"Q"in assert(Q:ok(k a))by okt;
 let It:=fresh"It"in let Ft:=fresh"Ft"in destruct(Q _ _ _ Ii H)as[It Ft];
 pose proof(later_back w P R Fl _ _ Ft L)as Li;clear O Q It Ft end.
Local Opaque reqs type_requirements provision_app fn_sig ety_of symbol_type.
Theorem provide_method_read:forall e mi recv dn tm tmr out s e' keys s',
 provide_method_code e mi recv dn tm out s=Good((e',keys),s')->
 Inv s->st_w s=w->In mi(w_order w)->
 nget(w_syms w)(gs_id recv)=Some recv->Later w P R Fl s'->TmRel Fl tm tmr e->
 exists m this sg a stamp,
 A.MethodNamed WW(gm_path(get_mod w mi))(gs_id recv)dn(gs_id m) /\
 A.RecvTy WW(gs_id recv)tmr(tr Fl this) /\ WFty this /\
 A.SigOf WW(gs_id m)tmr(Some(tr Fl this))(tr_sig Fl sg) /\
 A.AppOf WW(gs_id m)tmr(output_inst Fl a) /\
 e'=set_app(set_mth e(canon e this)(gs_id m)sg)
   (KyMth(canon e this)(gs_id m))(freeze_app(set_mth e(canon e this)(gs_id m)sg)a)stamp /\
 keys=out++[KyMth(canon e this)(gs_id m)].
Proof.
 intros e mi recv dn tm tmr out s e' keys s' E Hi Hw Hmi Hr L T.
 unfold provide_method_code in E;apply bind_inv in E as(mo&sm&Em&E).
 destruct mo as[m|];[|discriminate E].
 pose proof(find_method_decl_named b w R Fl mi recv dn s m sm Hload Hmi Hr Hi Hw Em)as[Es Named].
 destruct(find_method_decl_loaded b w R Fl mi recv dn s m sm Hload Hmi Hr Hi Hw Em)as[-> Hm].
 pose proof(method_named_kind b w R Fl mi(gs_id recv)dn(gs_id m)Hload Hmi Named)as Km.
 destruct(loaded_method_fn w R Fl m Hm Km)as(rcv&nm&ps&rt&body&Dm).
 output_step E Hi Hw L this sy0 Et Ii0 Wi0 Li0.
 destruct(recvty_exec w R Fl b Hload mi recv tm tmr e s this sy0 Et Hi Hw Hr T Li0)as[Rt Wthis].
 output_step E Ii0 Wi0 L sg sy1 Eg Ii1 Wi1 Li1.
 destruct(fn_sig_rel_at w R Fl b Hload m tm tmr e(Some this)sy0 sg sy1 rcv nm ps rt body
   Eg Ii0 Wi0 Hm Dm T Wthis Li1)as[Sg Th].
 output_step E Ii1 Wi1 L k sy2 Ek Ii2 Wi2 Li2.
 destruct(@method_key_value e mi this m sy1 k sy2 Ek)as[K Sk].
 subst k;subst sy2.
 apply bind_inv in E as(ep&se&Ep&E);injection E as <- <- <-.
 unfold provided_at in Ep;apply bind_inv in Ep as(a&sa&Ea&Ep).
 apply bind_inv in Ep as(stamp&sp&Est&Ep);injection Ep as <- <-.
 destruct(output_provision_ok m tm true _ _ _ Ii2 Ea)as[Ia Fa].
 destruct(ok_fresh _ _ _ Ia Est)as[_ Fs].
 pose proof(later_back w P R Fl _ _ Fs L)as La.
 pose proof(method_provision_app_rel w R Fl b Hload m rcv nm ps rt body tm tmr e sy1 a sa
   Hm Dm Km Ea Ii2 Wi2 La T)as Ap.
 exists m,this,sg,a,stamp;repeat split;try assumption;try reflexivity.
Qed.
End Method.
