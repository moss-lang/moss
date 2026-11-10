(* Actual scanner leaves correspond to entries in a completed semantic result read. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy
 MethodLookup MethodLookupNone MethodKind FamilySelectTrace FamilyOutputLeaf FamilyLeafScan.
From NM Require Import AuditFacts.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Lemma family_output_context_inv:forall W p ob tm pa a c d n its es tm',
 A.resolve_sym W p pa=Some c->A.w_decl W c=Some d->A.d_ast d=ADCtx n its->
 A.OutputItemEntries W p ob tm(Spec(Some pa)None a)es tm'->
 exists bs,A.OutputBrackets W p(ob++tm)a bs/\A.OutputItemsEntries W(A.d_mod d)(bs++ob)tm(ctx_specs its)es tm'.
Proof.
 intros W p ob tm pa a c d n its es tm' Res Decl Ast Read.
 inversion Read as[p0 ob0 tm0 pa0 a0 bs t R K B|
  p0 ob0 tm0 pa0 a0 bs u k R K B Cases|
  p0 ob0 tm0 a0 pa0 u du vn vt ty bs ap R D V B Ty App|
  p0 ob0 tm0 a0 pa0 u sg bs ap du rcv fn ps rt R K D F B Sig App|
  p0 ob0 tm0 a0 bs pa0 u du fn ps args rs fx R D F B Available Fixed|
  |p0 ob0 tm0 a0 pa0 u du cn specs sps bs entries tm1 R D F Es B Child];subst.
 - assert(t=c)by congruence;subst t;unfold A.kind_sym in K;rewrite Decl,Ast in K;discriminate K.
 - assert(u=c)by congruence;subst u;unfold A.kind_sym in K;rewrite Decl,Ast in K;injection K as <-.
   destruct Cases as[Bad|[Bad|[Bad|[[Bad Rest]|[Bad Rest]]]]];discriminate Bad.
 - assert(u=c)by congruence;subst u;assert(du=d)by congruence;subst du;congruence.
 - assert(u=c)by congruence;subst u;unfold A.kind_sym in K;rewrite Decl,Ast in K;discriminate K.
 - assert(u=c)by congruence;subst u;assert(du=d)by congruence;subst du;congruence.
 - assert(u=c)by congruence;subst u;assert(du=d)by congruence;subst du.
   assert(Ei:map CISpec sps=its)by congruence;subst its;rewrite ctx_specs_map;exists bs;split;assumption.
Qed.
Lemma family_output_method_inv:forall W p ob tm pa dn a es tm',
 A.OutputItemEntries W p ob tm(Spec(Some pa)(Some dn)a)es tm'->
 exists r mid rt sg bs ap,A.resolve_sym W p pa=Some r/\A.MethodNamed W p r dn mid/\
 A.OutputBrackets W p(ob++tm)a bs/\A.RecvTy W r(bs++ob++tm)rt/\es=[A.NMth rt mid sg ap].
Proof.
 intros;inversion H;subst;do 6 eexists.
 split;[eassumption|];split;[eassumption|];split;[eassumption|];split;[eassumption|reflexivity].
Qed.
Section Complete.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Definition FamilyReadComplete(e:env)(mi:nat)(outer:list(nat*ety))(sp:spec)(leaves:list family_leaf):Prop:=
 forall ob tm es tm',A.OutputItemEntries W(gm_path(get_mod w mi))ob tm sp es tm'->
 forall leaf,In leaf leaves->exists rt mid sg ap,
 In(A.NMth rt mid sg ap)es/\FamilyScanRoute w (P:=P) R Fl e mi outer ob tm sp rt mid leaf.
Lemma family_complete_chunks:forall e mi outer specs chunks,
 Forall2(FamilyReadComplete e mi outer)specs chunks->
 forall ob tm es tm',A.OutputItemsEntries W(gm_path(get_mod w mi))ob tm specs es tm'->
 forall leaf,In leaf(List.concat chunks)->exists sp rt mid sg ap,
 In sp specs/\In(A.NMth rt mid sg ap)es/\FamilyScanRoute w (P:=P) R Fl e mi outer ob tm sp rt mid leaf.
Proof.
 intros e mi outer specs chunks Rel;induction Rel as[|sp chunk specs chunks Head Rel IH];
 intros ob tm es tm' Read leaf Hin;[contradiction|].
 inversion Read as[|p0 ob0 tm0 sp0 rest es1 tm1 es2 tm2 R1 R2];subst.
 cbn[List.concat]in Hin;apply in_app_iff in Hin as[First|Rest].
 - destruct(Head ob tm es1 tm1 R1 leaf First)as(rt&mid&sg&ap&Member&Route).
   exists sp,rt,mid,sg,ap;split;[left;reflexivity|];split;[apply in_app_iff;left;exact Member|exact Route].
 - destruct(proj1 family_output_leaf_origin W _ _ _ _ _ _ R1)as[Same _];subst tm1.
   destruct(IH ob tm es2 tm' R2 leaf Rest)as(sp0&rt&mid&sg&ap&Member&InE&Route).
   exists sp0,rt,mid,sg,ap;split;[right;exact Member|];split;[apply in_app_iff;right;exact InE|exact Route].
Qed.
Theorem family_leaves_results_complete:forall fuel e mi specs outer s leaves sf,
 family_leaves fuel e mi specs outer s=Good(leaves,sf)->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sf->
 forall ob tm es tm',A.OutputItemsEntries W(gm_path(get_mod w mi))ob tm specs es tm'->
 forall leaf,In leaf leaves->exists sp rt mid sg ap,
 In sp specs/\In(A.NMth rt mid sg ap)es/\FamilyScanRoute w (P:=P) R Fl e mi outer ob tm sp rt mid leaf.
Proof.
 induction fuel as[|fuel IH];intros e mi specs outer s leaves sf Run Hi Hw Hmi L;
 [exfalso;eapply err_not_good;exact Run|].
 change((let!chunks:=mmap(family_spec_scan fuel e mi outer)specs in ret(List.concat chunks))s=Good(leaves,sf))in Run.
 apply bind_inv in Run as(chunks&sr&Map&Ret);injection Ret as <- <-.
 assert(Rel:Forall2(FamilyReadComplete e mi outer)specs chunks).
 {eapply mmap_ok_rel;[apply family_spec_scan_ok| |exact Map|exact Hi|exact Hw|exact L].
  intros[pa dn a]Hsp si chunk ss E Ii Wi Li ob tm es tm' Reading leaf Hin.
  destruct pa as[pa|],dn as[dn|];try solve[inversion Reading].
  - destruct(family_output_method_inv _ _ _ _ _ _ _ _ _ Reading)
    as(r&mid&rt&sg&bs&ap&Res&Named&Br&Recv&->).
    unfold family_spec_scan in E;apply bind_inv in E as(sub&sx&Sub&E).
    assert(OS:ok(match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end))by(destruct a;okt).
    destruct(OS _ _ _ Ii Sub)as[Ix Fx];pose proof(fr_w' w _ _ Fx Wi)as Wx.
    apply bind_inv in E as(rg&sy&Resolve&E).
    destruct(rts_spec w R Fl b Hload mi pa sx rg sy Ix Wx Hmi Resolve)as[-> [Rr[Hr Kr]]].
    assert(Id:gs_id rg=r)by congruence;subst r.
    apply bind_inv in E as(mo&sz&Find&E);destruct mo as[mg|].
    2:{injection E as <- <-;contradiction. }
    destruct(MethodLookupNone.find_method_decl_loaded b w R Fl mi rg dn sx mg sz Hload Hmi Hr Ix Wx Find)
    as[-> [Hm Mdecl]].
    destruct(find_method_decl_named b w R Fl mi rg dn sx mg sx Hload Hmi Hr Ix Wx Find)as[_ Nmg].
    assert(Mid:mid=gs_id mg)by(eapply method_named_unique;[exact Named|exact Nmg]);subst mid.
    injection E as <- <-;destruct Hin as[Eq|[]];subst leaf.
    exists rt,(gs_id mg),sg,ap;split;[left;reflexivity|].
    eapply FSRMethod;try eassumption;try reflexivity.
    exists si,sx;split;[exact Ii|];split;[exact Wi|];split;assumption.
  - unfold family_spec_scan in E;apply bind_inv in E as(sub&sx&Sub&E).
    assert(OS:ok(match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end))by(destruct a;okt).
    destruct(OS _ _ _ Ii Sub)as[Ix Fx];pose proof(fr_w' w _ _ Fx Wi)as Wx.
    apply bind_inv in E as(go&sy&Resolve&E).
    destruct(esym_rel w R Fl b Hload mi pa sx go sy Ix Wx Hmi Resolve)as[-> RelG].
    destruct go as[cg|];[|injection E as <- <-;contradiction].
    destruct RelG as[Rg Hg].
    destruct(gs_kind cg)eqn:K;try solve[injection E as <- <-;contradiction].
    destruct(gs_decl cg)eqn:D;try solve[injection E as <- <-;contradiction].
    destruct(family_output_context_inv W(gm_path(get_mod w mi))ob tm pa a(gs_id cg)
      (A.Decl(gm_path(get_mod w(gs_mod cg)))(gs_decl cg)(gs_assumes cg))n l es tm'
      Rg(w_decl_read w R Fl _ _ Hg)D Reading)as(bs&Br&Children).
    assert(Hcm:In(gs_mod cg)(w_order w))by exact(loader_mod_in w R Fl b Hload _ _ Hg).
    destruct(IH e(gs_mod cg)(ctx_specs l)sub sx chunk ss E Ix Wx Hcm Li(bs++ob)tm es tm' Children leaf Hin)
    as(child&rt&mid&sg&ap&Hchild&Member&Route).
    apply in_ctx_specs in Hchild.
    destruct(ok_family_leaves fuel e(gs_mod cg)(ctx_specs l)sub _ _ _ Ix E)as[_ Ft].
    exists rt,mid,sg,ap;split;[exact Member|].
    eapply FSRContext with(d:=A.Decl(gm_path(get_mod w(gs_mod cg)))(gs_decl cg)(gs_assumes cg))(n:=n);
    try eassumption;try reflexivity.
    + exists si,sx;split;[exact Ii|];split;[exact Wi|];split;[exact(later_back w P R Fl _ _ Ft Li)|exact Sub].
    + exact(w_decl_read w R Fl _ _ Hg). }
 intros ob tm es tm' Read leaf Hin;eapply family_complete_chunks;[exact Rel|exact Read|exact Hin].
Qed.
End Complete.
