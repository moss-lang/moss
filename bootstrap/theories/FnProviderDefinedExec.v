(* Named defined providers use actual context satisfaction and signature checks. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame InfoPreserve
 BodySoundCore BodyTr BodyTy Merge CanonTypes ProviderSig SourceSatisfiesGaps CtxMerge SignatureLeavesExec.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Ltac provider_noerr H:=exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(? &? &? &H)])].
Section Defined.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Notation WW:=(BodySoundCore.W w R Fl).
Lemma canonical_type_fixed:forall c e F C t,CxRel w R Fl c e F C ->
 A.canon C(tr Fl(canon e t))=tr Fl(canon e t).
Proof.
 intros c e F C t Hc;unfold A.canon;rewrite(cr_sub _ _ _ _ _ _ _ Hc).
 rewrite tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
 rewrite canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc));reflexivity.
Qed.
Lemma canonical_signature_fixed:forall c e F C sg,CxRel w R Fl c e F C ->
 A.FSig(map(A.canon C)(A.sg_params(canonical_sig Fl e sg)))
       (A.canon C(A.sg_ret(canonical_sig Fl e sg)))=canonical_sig Fl e sg.
Proof.
 intros c e F C sg Hc;unfold canonical_sig;cbn[A.sg_params A.sg_ret].
 rewrite map_map;f_equal;[apply map_ext;intro p|];apply(canonical_type_fixed c e F C _ Hc).
Qed.
Theorem match_defined_provider:forall fuel c e F C pa g n ps rt body wanted kind s sm s',
 nget(w_syms w)(gs_id g)=Some g ->gs_decl g=ADFn RPlain n ps rt(Some body) ->
 A.resolve_sym WW(A.fr_mod F)pa=Some(gs_id g) ->
 match_fn_sig fuel c e g wanted kind s=Good(sm,s') ->
 Inv s ->InfoInv s ->CxRel w R Fl c e F C ->Later w P R Fl s' ->
 A.FnProvider WW P F C(AEPath pa None)(canonical_sig Fl e wanted)None(gs_id g)sm.
Proof.
 intros fuel c e F C pa g n ps rt body wanted kind s sm s' Hg Decl Path Run Hi Ii Hc L.
 pose proof Run as Whole.
 destruct(ok_match_fn_sig fuel c e g wanted kind _ _ _ Hi Run)as[_ AllFr].
 assert(Hw:st_w s=w)by(rewrite <-(fr_w _ _ AllFr);exact(lt_w _ _ _ _ _ L)).
 destruct fuel as[|fuel];[discriminate Run|]. cbn[match_fn_sig]in Run.
 apply bind_inv in Run as(info&si&Info&Run).
 destruct(ok_fn_info_frame _ _ _ _ Hi Info)as[Ii1 Fi].
 apply bind_inv in Run as(pairs&sn&Needs&Run).
 assert(On:ok(require_needs(cx_mod c)e g info))by(unfold require_needs,require_merges;okt).
 destruct(On _ _ _ Ii1 Needs)as[In Fn].
 apply bind_inv in Run as(got&ssig&SigRun&Run).
 assert(Osig:ok(fn_sig g(tymap e)e None))by(unfold fn_sig;okt).
 destruct(Osig _ _ _ In SigRun)as[Isig Fsig].
 destruct(sig_eqb_in e got e wanted)eqn:Equal;[|provider_noerr Run].
 apply bind_inv in Run as(u&sevent&Event&Run).
 assert(Oevent:ok(record_reify_event(REProvider ety key(gs_id g)(fi_needs info)(reify_sig e got)(reify_arguments e))))
 by(unfold record_reify_event;apply ok_reify_update).
 destruct(Oevent _ _ _ Isig Event)as[Ievent Fevent].
 destruct(fn_info_satisfaction_gaps_any w (P:=P) R Fl b Hload Finj Hworld g s info si si
    (cx_mod c)e pairs sn sevent sm s' c F C Hg Info(fr_refl si)Needs
    (fr_trans _ _ _ Fsig Fevent)Run Hi Ii Hw Ii1 Ievent L Hc)as(Cs&Ctx&Sat).
 pose proof(match_fn_sig_signature w (P:=P) R Fl b Hload (S fuel)c e F C g wanted kind s sm s'
    RPlain n ps rt(Some body)Whole Hi Hw Hc Hg Decl L)as Signature.
 assert(WD:A.w_decl WW(gs_id g)=Some(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)))
 by exact(w_decl_read w R Fl _ _ Hg).
 eapply A.FPDefined with(Cg:=Cs)(ns:=map(key_tr Fl)(fi_needs info)).
 - exact Path.
 - unfold A.kind_sym;rewrite WD;cbn;rewrite Decl;reflexivity.
 - exists n,ps,rt,body;unfold A.ast_of;rewrite WD;cbn;now rewrite Decl.
 - exact Ctx.
 - exact Sat.
 - rewrite(canonical_signature_fixed c e F C wanted Hc);exact Signature.
Qed.
End Defined.

Section Named.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Notation WW:=(BodySoundCore.W w R Fl).
Theorem expect_named_defined:forall fuel c e pa s pv s' F C u n ps rt body,
 expect_fn_provider fuel c e(AEPath pa None)s=Good(pv,s') ->
 Inv s->st_w s=w->CxRel w R Fl c e F C ->
 A.resolve_sym WW(A.fr_mod F)pa=Some u ->
 A.ast_of WW u=Some(ADFn RPlain n ps rt(Some body)) ->
 s'=s /\nget(w_syms w)(gs_id pv)=Some pv /\gs_id pv=u /\
 gs_decl pv=ADFn RPlain n ps rt(Some body).
Proof.
 intros[|fuel] c e pa s pv s' F C u n ps rt body E Hi Hw Hc Path Ast;
 cbn[expect_fn_provider]in E;[discriminate|].
 apply bind_inv in E as(found&sf&Resolve&E).
 destruct(esym_rel w R Fl b Hload(cx_mod c)pa s found sf Hi Hw(cr_mod_in _ _ _ _ _ _ _ Hc)Resolve)
 as[-> Read].
 destruct found as[g|].
 2:{rewrite(cr_mod _ _ _ _ _ _ _ Hc),Read in Path;discriminate. }
 destruct Read as[PathG Hg].
 assert(Id:u=gs_id g)by(rewrite(cr_mod _ _ _ _ _ _ _ Hc),PathG in Path;congruence).
 subst u.
 assert(WD:A.w_decl WW(gs_id g)=Some(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)))
 by exact(w_decl_read w R Fl _ _ Hg).
 assert(Decl:gs_decl g=ADFn RPlain n ps rt(Some body)).
 {unfold A.ast_of in Ast;rewrite WD in Ast;injection Ast as <-;reflexivity. }
 assert(Kind:gs_kind g=KFn).
 {pose proof(signature_loaded_kind w R Fl b Hload _ _ Hg)as K;unfold KindOK in K;rewrite Decl in K;
  destruct(gs_kind g);cbn[A.kind_of kconv]in K;congruence. }
 rewrite Kind in E;unfold fn_body in E;rewrite Decl in E;cbn[kind_eqb is_some negb andb]in E.
 unfold expect_defined_fn in E;apply bind_inv in E as(again&s2&Again&E).
 rewrite Resolve in Again;injection Again as <- <-.
 unfold is_defined_fn,fn_body in E;rewrite Kind,Decl in E;
 cbn[kind_eqb is_some andb]in E;injection E as <- <-.
 auto.
Qed.
Theorem named_defined_provider:forall fuel_match fuel_expect c e F C pa pv wanted kind s se sm s' u n ps rt body,
 expect_fn_provider fuel_expect c e(AEPath pa None)s=Good(pv,se) ->
 match_fn_sig fuel_match c e pv wanted kind se=Good(sm,s') ->
 A.resolve_sym WW(A.fr_mod F)pa=Some u ->
 A.ast_of WW u=Some(ADFn RPlain n ps rt(Some body)) ->
 Inv s->InfoInv s->CxRel w R Fl c e F C->Later w P R Fl s' ->
 A.FnProvider WW P F C(AEPath pa None)(canonical_sig Fl e wanted)None(gs_id pv)sm.
Proof.
 intros fuel_match fuel_expect c e F C pa pv wanted kind s se sm s' u n ps rt body Expect Match Path Ast Hi Ii Hc L.
 destruct(ok_expect_fn_provider fuel_expect c e(AEPath pa None)_ _ _ Hi Expect)as[_ Prefix].
 assert(Hw:st_w s=w).
 {destruct(ok_match_fn_sig fuel_match c e pv wanted kind _ _ _
   (proj1(ok_expect_fn_provider _ _ _ _ _ _ _ Hi Expect))Match)as[_ Tail].
  rewrite <-(fr_w _ _ Prefix),<-(fr_w _ _ Tail);exact(lt_w _ _ _ _ _ L). }
 destruct(expect_named_defined fuel_expect c e pa s pv se F C u n ps rt body Expect Hi Hw Hc Path Ast)
 as[->[Loaded[Id Decl]]].
 eapply(match_defined_provider w (P:=P) R Fl b Hload Hworld Finj fuel_match c e F C pa pv n ps rt body wanted kind s sm s');
 try eassumption;now rewrite Id.
Qed.
End Named.
