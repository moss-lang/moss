(* The actual named abstract-provider branch builds a fresh typed forwarder. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy BodyTr CtxWalk
 CanonTypes InfoPreserve LoaderOrigin ForwarderTrace ForwarderHistory FinalForwarders
 FnProviderAbstractTrace FnProviderForwardSignature FnProviderForwardExec FnSnapshotAlign ProviderSig SignatureLeavesExec.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section Abstract.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Notation WW:=(BodySoundCore.W w R Fl).
Theorem abstract_provider_sound:forall expectfuel matchfuel c e F C pa target provider wanted kind s mid sm final n ps rt,
 nget(w_syms w)(gs_id target)=Some target->gs_kind target=KFn->gs_decl target=ADFn RPlain n ps rt None->
 resolve_sym(cx_mod c)pa s=Good(Some target,s)->
 A.resolve_sym WW(A.fr_mod F)pa=Some(gs_id target)->
 expect_fn_provider expectfuel c e(AEPath pa None)s=Good(provider,mid)->
 match_fn_sig matchfuel c e provider wanted kind mid=Good(sm,final)->
 Inv s->InfoInv s->FwdInv s->st_w s=w->CxRel w R Fl c e F C->Later w P R Fl final->
 A.FnProvider WW P F C(AEPath pa None)(canonical_sig Fl e wanted)None(gs_id provider)sm.
Proof.
 intros expectfuel matchfuel c e F C pa target provider wanted kind s mid sm final n ps rt
 Loaded Kind Decl Resolve Path Expect Match Hi Ii FI Hw Hc L.
 destruct(ok_expect_fn_provider expectfuel c e(AEPath pa None)_ _ _ Hi Expect)as[Im Fexpect].
 destruct(ok_match_fn_sig matchfuel c e provider wanted kind _ _ _ Im Match)as[Ifinal Fmatch].
 assert(Lmid:Later w P R Fl mid)by exact(later_back w P R Fl _ _ Fmatch L).
 assert(Hwm:st_w mid=w)by(rewrite(fr_w _ _ Fexpect);exact Hw).
 assert(Iim:InfoInv mid)by(exact(proj2(okI_expect_fn_provider expectfuel c e(AEPath pa None))_ _ _ Hi Ii Expect)).
 assert(NoBody:fn_body target=None)by(unfold fn_body;rewrite Decl;reflexivity).
 destruct(expect_abstract_provider_trace expectfuel c e pa s provider mid target Hi FI Resolve Kind NoBody Expect)
 as[Available[Origin Lookup]].
 destruct(abstract_provider_final_facts w P expectfuel c e pa s provider mid target Hi FI
 (lt_forwarders _ _ _ _ _ Lmid)Resolve Kind NoBody Expect)as[_[Nonzero[tg[LoadedTg[Id[Copied[Home[Recv Body]]]]]]]].
 destruct LoadedTg as[LoadedTg Rest].
 assert(Same:tg=target)by(rewrite Id in LoadedTg;congruence);subst tg.
 destruct(match_forwarder_captured_signature w (P:=P) R Fl b Hload Hworld matchfuel c e F C provider target wanted kind mid sm final n ps rt
 Lookup Origin Loaded Kind Decl Copied Home Match Im Iim Hwm Hc L)as[Table Sm].
 assert(Absent:nget(w_syms w)(gs_id provider)=None).
 {destruct(nget(w_syms w)(gs_id provider))as[old|]eqn:Old;[|reflexivity].
  pose proof(iv_loader mid Im b ltac:(rewrite Hwm;exact Hload)(gs_id provider)old
  ltac:(rewrite Hwm;exact Old))as InState.
  assert(old=provider)by congruence;subst old.
  pose proof(load_world_source b w Hload _ _ Old)as Source;rewrite Origin in Source;discriminate. }
 assert(Fresh:A.Fresh WW(gs_id provider)).
 {split;[cbn[A.w_decl BodySoundCore.W W_read];rewrite Absent;reflexivity|exact Nonzero]. }
 assert(Ast:A.ast_of WW(gs_id target)=Some(ADFn RPlain n ps rt None)).
 {change(option_map A.d_ast(A.w_decl(W_read w R Fl)(gs_id target))=Some(ADFn RPlain n ps rt None)).
  rewrite(w_decl_read w R Fl _ _ Loaded);cbn[option_map A.d_ast];rewrite Decl;reflexivity. }
 pose proof(match_forwarder_target_signature w (P:=P) R Fl b Hload matchfuel c e F C provider target wanted kind mid sm final n ps rt
 Loaded Decl Copied Home Match Im Hwm Hc L)as Signature.
 pose proof(signature_parameter_arity WW(gs_id target)(A.c_ty C)None(canonical_sig Fl e wanted)RPlain n ps rt None Signature Ast)as Arity.
 assert(Count:List.length(fn_params provider)=List.length(A.sg_params(canonical_sig Fl e wanted))).
 {rewrite Arity;unfold fn_params at 1;rewrite Copied;unfold forwarder_decl;unfold fn_params;rewrite Decl.
  cbn[gs_decl];rewrite combine_length,!map_length,seq_length,Nat.min_id;reflexivity. }
 subst sm;eapply A.FPForward with(g:=gs_id target)(n:=List.length(fn_params provider)).
 - exact Path.
 - rewrite(kind_sym_rd w R Fl b Hload _ _ Loaded),Kind;reflexivity.
 - exists n,ps,rt;exact Ast.
 - exact Table.
 - exact Count.
 - exact Fresh.
 - exact Body.
Qed.
Theorem named_abstract_provider:forall expectfuel matchfuel c e F C pa provider wanted kind s mid sm final u n ps rt,
 expect_fn_provider expectfuel c e(AEPath pa None)s=Good(provider,mid)->
 match_fn_sig matchfuel c e provider wanted kind mid=Good(sm,final)->
 A.resolve_sym WW(A.fr_mod F)pa=Some u->A.ast_of WW u=Some(ADFn RPlain n ps rt None)->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Later w P R Fl final->
 A.FnProvider WW P F C(AEPath pa None)(canonical_sig Fl e wanted)None(gs_id provider)sm.
Proof.
 intros expectfuel matchfuel c e F C pa provider wanted kind s mid sm final u n ps rt
 Expect Match Path Ast Hi Ii FI Hc L.
 destruct(ok_expect_fn_provider expectfuel c e(AEPath pa None)_ _ _ Hi Expect)as[Im Fe].
 destruct(ok_match_fn_sig matchfuel c e provider wanted kind _ _ _ Im Match)as[Ifinal Fm].
 assert(Hw:st_w s=w)by(rewrite <-(fr_w _ _ Fe),<-(fr_w _ _ Fm);exact(lt_w _ _ _ _ _ L)).
 pose proof Expect as Trace;destruct expectfuel as[|expectfuel];[discriminate|].
 cbn[expect_fn_provider]in Trace;apply bind_inv in Trace as[found[sf[Resolve Trace]]].
 destruct(esym_rel w R Fl b Hload(cx_mod c)pa s found sf Hi Hw(cr_mod_in _ _ _ _ _ _ _ Hc)Resolve)
 as[-> Read].
 destruct found as[target|].
 2:{rewrite(cr_mod _ _ _ _ _ _ _ Hc),Read in Path;discriminate. }
 destruct Read as[PathTarget Loaded].
 assert(Id:u=gs_id target)by(rewrite(cr_mod _ _ _ _ _ _ _ Hc),PathTarget in Path;congruence).
 subst u.
 assert(WD:A.w_decl WW(gs_id target)=Some(A.Decl(gm_path(get_mod w(gs_mod target)))(gs_decl target)(gs_assumes target)))
 by exact(w_decl_read w R Fl _ _ Loaded).
 assert(Decl:gs_decl target=ADFn RPlain n ps rt None).
 {unfold A.ast_of in Ast;rewrite WD in Ast;injection Ast as <-;reflexivity. }
 assert(Kind:gs_kind target=KFn).
 {pose proof(signature_loaded_kind w R Fl b Hload _ _ Loaded)as K;unfold KindOK in K;rewrite Decl in K;
  destruct(gs_kind target);cbn[A.kind_of kconv]in K;congruence. }
 eapply abstract_provider_sound;[exact Loaded|exact Kind|exact Decl|exact Resolve|exact Path|exact Expect|exact Match|
 exact Hi|exact Ii|exact FI|exact Hw|exact Hc|exact L].
Qed.
End Abstract.
