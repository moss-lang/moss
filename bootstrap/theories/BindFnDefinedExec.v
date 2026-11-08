From NM Require Import WrapperKeyReadonly.
(* A real named defined provider closes the ordinary function-bind branch. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame InfoPreserve
 BodySoundCore BodyTr BodyTy Merge CanonTypes ProviderSig FnProviderDefinedExec FnSigExec
 ProvideResultContext BindValue CallCacheExec SignatureLeavesExec.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].
Ltac named_bind_noerr H:=exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(? &? &? &H)])].
Section Bind.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Notation WW:=(BodySoundCore.W w R Fl).
Theorem check_bind_named_defined:forall fuel c e pp pg s xs e' c' s' F C f u pn pps prt pbody,
 check_bind fuel c e(Spec(Some pp)None None)(AEPath pg None)s=Good((xs,e',c'),s')->
 A.resolve_sym WW(A.fr_mod F)pp=Some f ->A.kind_sym WW f=Some A.DKFn ->
 A.resolve_sym WW(A.fr_mod F)pg=Some u ->A.ast_of WW u=Some(ADFn RPlain pn pps prt(Some pbody)) ->
 Inv s->InfoInv s->CxRel w R Fl c e F C->Later w P R Fl s' ->
 c'=c /\exists C',CxRel w R Fl c' e' F C' /\
 A.BindItem WW P F C(Spec(Some pp)None None,Some(AEPath pg None))(fun tail=>fold_stmts xs tail)C' F[].
Proof.
 intros fuel c e pp pg s xs e' c' s' F C f u pn pps prt pbody Run Target KindF Provider DeclP Hi Ii Hc L.
 destruct(ok_check_bind fuel c e(Spec(Some pp)None None)(AEPath pg None)_ _ _ Hi Run)as[_ FullFr].
 assert(Hw:st_w s=w)by(rewrite <-(fr_w _ _ FullFr);exact(lt_w _ _ _ _ _ L)).
 destruct fuel as[|fuel];[discriminate Run|]. cbn[check_bind]in Run.
 apply bind_inv in Run as(found&s1&Path&Run).
 destruct(rpath_rel w R Fl b Hload _ _ _ _ _ Hw(cr_mod_in _ _ _ _ _ _ _ Hc)Path)as[-> Res].
 destruct found as[[sid|modid]|];[|named_bind_noerr Run|named_bind_noerr Run].
 cbn[option_map ent_of]in Res;apply bind_inv in Run as(g&s2&Sym&Run).
 destruct(sym_loader w R Fl b Hload _ _ _ _ _ _ Hi Hw Res Sym)as[-> Hg].
 assert(IdF:f=sid).
 {unfold A.resolve_sym in Target;rewrite(cr_mod _ _ _ _ _ _ _ Hc),Res in Target;congruence. }
 subst f.
 pose proof(kind_sym_read b w R Fl Hload sid g Hg)as Kd.
 change(A.kind_sym WW sid=Some(kconv(gs_kind g)))in Kd.
 assert(Kind:gs_kind g=KFn)by(rewrite Kd in KindF;destruct(gs_kind g);try discriminate KindF;reflexivity).
 rewrite Kind in Run.
 apply bind_inv in Run as(sg&sSig&SigRun&Run).
 assert(Osig:ok(fn_sig g(tymap e)e None))by(unfold fn_sig;okt).
 destruct(Osig _ _ _ Hi SigRun)as[ISig FSig].
 apply bind_inv in Run as([[target pre]sm]&sProvider&Prov&Run).
 apply bind_inv in Prov as(pv&sExpect&Expect&Prov).
 destruct(ok_expect_fn_provider fuel c e(AEPath pg None)_ _ _ ISig Expect)as[IExpect FExpect].
 apply bind_inv in Prov as(sm0&sMatch&Match&Prov);injection Prov as <- <- <- <-.
 destruct(ok_match_fn_sig fuel c e pv sg "fn" _ _ _ IExpect Match)as[IMatch FMatch].
 apply bind_inv in Run as(eout&sProv&Provision&Run).
 assert(Oprov:ok(provided_at(set_fn e sid sg)(KyFn sid)g(tymap(set_fn e sid sg))false)).
 {unfold provided_at,provision_app;okt. }
 destruct(Oprov _ _ _ IMatch Provision)as[IProv FProv].
 apply bind_inv in Run as(umeta&sEvent&Event&Run);injection Run as <- <- <- <-.
 match type of Event with ?op _ = _=>assert(Oevent:ok op)by(unfold record_reify_event;apply ok_reify_update)end.
 destruct(Oevent _ _ _ IProv Event)as[_ FEvent].
 pose proof(later_back w P R Fl _ _ (fr_trans _ _ _ FProv FEvent)L)as LMatch.
 pose proof(later_back w P R Fl _ _ (fr_trans _ _ _ FExpect FMatch)LMatch)as LSig.
 assert(Wsig:st_w sSig=w)by(rewrite(fr_w _ _ FSig);exact Hw).
 assert(Iisig:InfoInv sSig)by(eapply info_step;[exact Ii|exact(frl_of_fr _ _ FSig)|exact(ni_fn_sig _ _ _ _ _ _ _ SigRun)|wrapper_cache_same]).
 pose proof(named_defined_provider w (P:=P) R Fl b Hload Hworld Finj fuel fuel c e F C pg pv sg "fn"
   sSig sExpect sm0 sMatch u pn pps prt pbody Expect Match Provider DeclP ISig Iisig Hc LMatch)as FP.
 pose proof(load_world_ids b w Hload sid g Hg)as Gid.
 assert(Loaded:nget(w_syms w)(gs_id g)=Some g)by(now rewrite Gid).
 pose proof(signature_loaded_kind w R Fl b Hload _ _ Loaded)as GKind.
 unfold KindOK in GKind;rewrite Kind in GKind;cbn[kconv]in GKind.
 assert(Decl:exists n ps rt body,gs_decl g=ADFn RPlain n ps rt body).
 {destruct(gs_decl g);cbn[A.kind_of]in GKind;try discriminate.
  destruct f;try discriminate;repeat eexists;reflexivity. }
 destruct Decl as(n&ps&rt&body&Decl).
 pose proof(fn_sig_canonical w (P:=P) R Fl b Hload c e F C g None s sg sSig RPlain n ps rt body
   SigRun Hi Hw Hc Loaded Decl I LSig)as Signature.
 destruct(fn_sig_body_properties w (P:=P) R Fl b Hload c e F C g s sg sSig
   SigRun Hi Hw Loaded Hc LSig)as[Clean Support].
 unfold provided_at in Provision;apply bind_inv in Provision as(a&sa&AppRun&Provision).
 apply bind_inv in Provision as(stamp&st&Stamp&Provision);injection Provision as <- <-.
 assert(Oa:ok(provision_app g(tymap(set_fn e sid sg))false))by(unfold provision_app;okt).
 destruct(Oa _ _ _ IMatch AppRun)as[IA FA].
 pose proof(later_back w P R Fl _ _ (proj2(ok_fresh _ _ _ IA Stamp))
   (later_back w P R Fl _ _ FEvent L))as LApp.
 pose proof(provision_app_canonical w R Fl c e F C g sMatch a sa AppRun IMatch Hc LApp
   ltac:(rewrite Gid,KindF;discriminate))as App.
 rewrite Gid in Signature,App.
 set(C':=A.provide C(IFn sid)(bind_inst Fl e a)(A.set_fn C sid(canonical_sig Fl e sg))).
 split;[reflexivity|]. exists C';split.
 - apply(cxrel_function w R Fl c e F C sid sg _ _ stamp Hc Clean Support);[reflexivity|].
   exists(A.c_ty C);split;[exact Signature|exact App].
 - cbn[fold_stmts];eapply A.BFnBind;[exact Target|exact KindF|exact Signature|exact FP|exact App].
Qed.
End Bind.
