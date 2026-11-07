From NM Require Import BodyProofTools.
(* A successful anonymous-function bind uses the actual complete lambda
   provider and the actual target signature/application snapshot. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound WorldSound ElabFrame InfoPreserve
 BodySoundCore BodyTr BodyTy Merge CanonTypes ProviderSig FnSigExec ProvideResultContext BindValue
 BindSequence CallCacheExec SignatureLeavesExec LambdaProviderExec.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Ltac lambda_bind_noerr H:=exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(? &? &? &H)])].
Section Bind.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySoundCore.W w R Fl).
Theorem check_bind_function_lambda:forall fuel c e pp lps lrt lbody s xs e' c' sf F C f,
 OK w P R Fl fuel->
 check_bind(S fuel)c e(Spec(Some pp)None None)(AELam lps lrt lbody)s=Good((xs,e',c'),sf)->
 A.resolve_sym W(A.fr_mod F)pp=Some f->A.kind_sym W f=Some A.DKFn->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl sf F->
 BindResult w P R Fl c e F C(Spec(Some pp)None None,Some(AELam lps lrt lbody))xs e' c'.
Proof.
 intros fuel c e pp lps lrt lbody s xs e' c' sf F C f IH Run Target KindF Hi Cache Fwd HC Hp.
 destruct(ok_check_bind(S fuel)c e(Spec(Some pp)None None)(AELam lps lrt lbody)_ _ _ Hi Run)as[_ FullFr].
 assert(Hw:st_w s=w)by(rewrite<-(fr_w _ _ FullFr);exact(lt_w _ _ _ _ _(proj1 Hp))).
 cbn[check_bind]in Run;apply bind_inv in Run as(found&s1&Path&Run).
 destruct(rpath_rel w R Fl b Hload _ _ _ _ _ Hw(cr_mod_in _ _ _ _ _ _ _ HC)Path)as[-> Res].
 destruct found as[[sid|modid]|];[|lambda_bind_noerr Run|lambda_bind_noerr Run].
 cbn[option_map ent_of]in Res;apply bind_inv in Run as(g&s2&Sym&Run).
 destruct(sym_loader w R Fl b Hload _ _ _ _ _ _ Hi Hw Res Sym)as[-> Hg].
 assert(IdF:f=sid).
 {unfold A.resolve_sym in Target;rewrite(cr_mod _ _ _ _ _ _ _ HC),Res in Target;congruence. }
 subst f.
 pose proof(kind_sym_read b w R Fl Hload sid g Hg)as Kd.
 change(A.kind_sym W sid=Some(kconv(gs_kind g)))in Kd.
 assert(Kind:gs_kind g=KFn)by(rewrite Kd in KindF;destruct(gs_kind g);try discriminate KindF;reflexivity).
 rewrite Kind in Run.
 apply bind_inv in Run as(sg&sSig&Sig&Run).
 assert(Os:ok(fn_sig g(tymap e)e None))by(unfold fn_sig;okt).
 destruct(Os _ _ _ Hi Sig)as[Is Fs].
 apply bind_inv in Run as([[pid pre]sm]&sProvider&Provider&Run).
 destruct(ok_lambda_provider fuel c e lps lrt lbody g sg None("`"++gs_name g++"`")%string _ _ _ Is Provider)as[Ip Fp].
 apply bind_inv in Run as(eout&sProv&Provision&Run).
 assert(Op:ok(provided_at(set_fn e sid sg)(KyFn sid)g(tymap(set_fn e sid sg))false))by(unfold provided_at,provision_app;okt).
 destruct(Op _ _ _ Ip Provision)as[Iprov Fprov].
 apply bind_inv in Run as(u&sEvent&Event&Run);injection Run as <- <- <- <-.
 match type of Event with ?m _ = _=>assert(Oevent:ok m)by(unfold record_reify_event;apply ok_reify_update)end.
 destruct(Oevent _ _ _ Iprov Event)as[_ Fevent].
 pose proof(post_back w P R Fl _ _ _ (fr_trans _ _ _ Fprov Fevent)Hp)as HpProvider.
 pose proof(later_back w P R Fl _ _ Fp(proj1 HpProvider))as LSig.
 assert(Wsig:st_w sSig=w)by(rewrite(fr_w _ _ Fs);exact Hw).
 assert(Csig:InfoInv sSig)by(eapply info_step;[exact Cache|exact(frl_of_fr _ _ Fs)|exact(ni_fn_sig _ _ _ _ _ _ _ Sig)|wrapper_cache_same]).
 assert(Fsig:FwdInv sSig)by(fwd_inv).
 pose proof(load_world_ids b w Hload sid g Hg)as Gid.
 assert(Loaded:nget(w_syms w)(gs_id g)=Some g)by(now rewrite Gid).
 pose proof(signature_loaded_kind w R Fl b Hload _ _ Loaded)as GKind.
 unfold KindOK in GKind;rewrite Kind in GKind;cbn[kconv]in GKind.
 assert(Decl:exists n ps rt body,gs_decl g=ADFn RPlain n ps rt body).
 {destruct(gs_decl g);cbn[A.kind_of]in GKind;try discriminate.
  destruct f;try discriminate;repeat eexists;reflexivity. }
 destruct Decl as(n&ps&rt&body&Decl).
 pose proof(fn_sig_canonical w(P:=P)R Fl b Hload c e F C g None s sg sSig RPlain n ps rt body
 Sig Hi Hw HC Loaded Decl I LSig)as Signature.
 destruct(fn_sig_body_properties w(P:=P)R Fl b Hload c e F C g s sg sSig Sig Hi Hw Loaded HC LSig)as[Clean Support].
 pose proof(lambda_provider_sound w P R Fl b Hload Finj fuel c e lps lrt lbody g sg None("`"++gs_name g++"`")%string sSig pid pre sm sProvider
 IH Provider F C Is Csig Fsig Wsig Loaded HC Clean Support I I HpProvider)as LP.
 unfold provided_at in Provision;apply bind_inv in Provision as(a&sa&AppRun&Provision).
 apply bind_inv in Provision as(stamp&st&Stamp&Provision);injection Provision as <- <-.
 assert(Oa:ok(provision_app g(tymap(set_fn e sid sg))false))by(unfold provision_app;okt).
 destruct(Oa _ _ _ Ip AppRun)as[Ia Fa].
 pose proof(later_back w P R Fl _ _ (proj2(ok_fresh _ _ _ Ia Stamp))
  (later_back w P R Fl _ _ Fevent(proj1 Hp)))as LApp.
 pose proof(provision_app_canonical w R Fl c e F C g sProvider a sa AppRun Ip HC LApp
  ltac:(rewrite Gid,KindF;discriminate))as App.
 rewrite Gid in Signature,App.
 set(C':=A.provide C(IFn sid)(bind_inst Fl e a)(A.set_fn C sid(canonical_sig Fl e sg))).
 exists F,C',[],(fun tail=>fold_stmts pre(XBindFn sid pid sm tail));split.
 - apply(cxrel_function w R Fl c e F C sid sg _ _ stamp HC Clean Support);[reflexivity|].
   exists(A.c_ty C);split;[exact Signature|exact App].
 - split;[reflexivity|];split;[now rewrite app_nil_r|];split.
   + eapply A.BFnLambda;[exact Target|exact KindF|exact Signature|exact LP|exact App].
   + intro tail;rewrite fold_stmts_app;reflexivity.
Qed.
End Bind.
