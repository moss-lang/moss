(* The actual scanner retains the semantic leaf's bracket/context route. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTr BodyTy
 MethodLookup MethodLookupNone MethodKind FamilySelectTrace FamilyOutputLeaf.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition family_spec_scan(f:nat)(e:env)(mi:nat)(outer:list(nat*ety))(sp:spec):M(list family_leaf):=
 match sp with Spec p d a=>
 let! sub:=match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end in
 match d,p with
 |Some dn,Some pp=>let!r:=resolve_type_symbol mi pp in let!mo:=find_method_decl mi r dn in
   match mo with Some m=>ret[(mi,r,m,sub)]|None=>ret[]end
 |None,Some pp=>let!go:=resolve_sym mi pp in
   match go with Some g=>match gs_kind g,gs_decl g with KCtx,ADCtx _ its=>family_leaves f e(gs_mod g)its sub|_,_=>ret[]end
   |None=>ret[]end
 |_,_=>ret[]end end.
Lemma family_spec_scan_ok:forall f e mi outer sp,ok(family_spec_scan f e mi outer sp).
Proof. intros f e mi outer[pa dn a];unfold family_spec_scan;okt. Qed.
Section Trace.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Definition FamilyBracketReceipt(e:env)(mi:nat)(a:sapp)(outer sub:list(nat*ety)):Prop:=
 exists si sf,Inv si/\st_w si=w/\Later w P R Fl sf/\
 (match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end)si=Good(sub,sf).
Inductive FamilyScanRoute(e:env):nat->list(nat*ety)->list(nat*ty)->list(nat*ty)->spec->ty->nat->family_leaf->Prop:=
|FSRMethod:forall mi outer ob tm pa dn a r mid rt bs sub rg mg,
 FamilyBracketReceipt e mi a outer sub->
 A.resolve_sym W(gm_path(get_mod w mi))pa=Some r->A.MethodNamed W(gm_path(get_mod w mi))r dn mid->
 A.OutputBrackets W(gm_path(get_mod w mi))(ob++tm)a bs->A.RecvTy W r(bs++ob++tm)rt->
 nget(w_syms w)(gs_id rg)=Some rg->gs_id rg=r->
 nget(w_syms w)(gs_id mg)=Some mg->gs_id mg=mid->
 FamilyScanRoute e mi outer ob tm(Spec(Some pa)(Some dn)a)rt mid(mi,rg,mg,sub)
|FSRContext:forall mi outer ob tm pa a c d n its bs sp rt mid sub cg leaf,
 FamilyBracketReceipt e mi a outer sub->
 A.resolve_sym W(gm_path(get_mod w mi))pa=Some c->A.w_decl W c=Some d->A.d_ast d=ADCtx n its->
 A.OutputBrackets W(gm_path(get_mod w mi))(ob++tm)a bs->In sp its->
 nget(w_syms w)(gs_id cg)=Some cg->gs_id cg=c->A.d_mod d=gm_path(get_mod w(gs_mod cg))->
 FamilyScanRoute e(gs_mod cg)sub(bs++ob)tm sp rt mid leaf->
 FamilyScanRoute e mi outer ob tm(Spec(Some pa)None a)rt mid leaf.
Lemma family_forall2_pick:forall X Y(Q:X->Y->Prop)xs ys,
 Forall2 Q xs ys->forall x,In x xs->exists y,In y ys/\Q x y.
Proof.
 intros X Y Q xs ys H;induction H;intros z Hin;[contradiction|].
 destruct Hin as[<-|Hin];[exists y;split;[left;reflexivity|exact H]|].
 destruct(IHForall2 z Hin)as(v&Hv&Qv);exists v;split;[right;exact Hv|exact Qv].
Qed.
Definition FamilyScanned(e:env)(mi:nat)(outer:list(nat*ety))(sp:spec)(leaves:list family_leaf):Prop:=
 forall ob tm rt mid,FamilyOutputLeaf W(gm_path(get_mod w mi))ob tm sp rt mid->
 exists leaf,In leaf leaves/\FamilyScanRoute e mi outer ob tm sp rt mid leaf.
Theorem family_leaves_semantic_route:forall fuel e mi specs outer s leaves sf,
 family_leaves fuel e mi specs outer s=Good(leaves,sf)->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sf->
 forall ob tm sp rt mid,In sp specs->FamilyOutputLeaf W(gm_path(get_mod w mi))ob tm sp rt mid->
 exists leaf,In leaf leaves/\FamilyScanRoute e mi outer ob tm sp rt mid leaf.
Proof.
 induction fuel as[|fuel IH];intros e mi specs outer s leaves sf Run Hi Hw Hmi L;
 [exfalso;eapply err_not_good;exact Run|].
 change((let!chunks:=mmap(family_spec_scan fuel e mi outer)specs in ret(List.concat chunks))s=Good(leaves,sf))in Run.
 apply bind_inv in Run as(chunks&sr&Map&Ret);injection Ret as <- <-.
 assert(Rel:Forall2(FamilyScanned e mi outer)specs chunks).
 {eapply mmap_ok_rel;[apply family_spec_scan_ok| |exact Map|exact Hi|exact Hw|exact L].
  intros sp Hsp si chunk ss E Ii Wi Li ob tm rt mid Leaf.
  inversion Leaf as[p0 ob0 tm0 pa dn a r mid0 rt0 bs Res Named Br Recv|
   p0 ob0 tm0 pa a ctx d n its bs child rt0 mid0 Res Decl Ast Br Hchild Child];subst.
  - unfold family_spec_scan in E;apply bind_inv in E as(sub&sx&Sub&E).
    assert(OS:ok(match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end))by(destruct a;okt).
    destruct(OS _ _ _ Ii Sub)as[Ix Fx];pose proof(fr_w' w _ _ Fx Wi)as Wx.
    apply bind_inv in E as(rg&sy&Resolve&E).
    destruct(rts_spec w R Fl b Hload mi pa sx rg sy Ix Wx Hmi Resolve)as[-> [Rr[Hr Kr]]].
    assert(Id:gs_id rg=r)by congruence;subst r.
    apply bind_inv in E as(mo&sz&Find&E);destruct mo as[mg|].
    2:{destruct(find_method_decl_none b w R Fl mi rg dn sx sz Hload Hmi Hr Wx Find)as[_ Absent];
      exfalso;exact(Absent mid Named). }
    destruct(MethodLookupNone.find_method_decl_loaded b w R Fl mi rg dn sx mg sz Hload Hmi Hr Ix Wx Find)
    as[-> [Hm Mdecl]].
    destruct(find_method_decl_named b w R Fl mi rg dn sx mg sx Hload Hmi Hr Ix Wx Find)as[_ Nmg].
    assert(Mid:gs_id mg=mid)by(eapply method_named_unique;[exact Nmg|exact Named]).
    injection E as <- <-.
    exists(mi,rg,mg,sub);split;[left;reflexivity|].
    eapply FSRMethod;try eassumption;try reflexivity.
    exists si,sx;split;[exact Ii|];split;[exact Wi|];split;assumption.
  - unfold family_spec_scan in E;apply bind_inv in E as(sub&sx&Sub&E).
    assert(OS:ok(match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end))by(destruct a;okt).
    destruct(OS _ _ _ Ii Sub)as[Ix Fx];pose proof(fr_w' w _ _ Fx Wi)as Wx.
    apply bind_inv in E as(go&sy&Resolve&E).
    destruct(esym_rel w R Fl b Hload mi pa sx go sy Ix Wx Hmi Resolve)as[-> RelG].
    destruct go as[cg|];[|cbn in RelG;congruence].
    destruct RelG as[Rg Hg];assert(Id:ctx=gs_id cg)by congruence;subst ctx.
    change(A.w_decl(W_read w R Fl)(gs_id cg)=Some d)in Decl.
    rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
    change(gs_decl cg=ADCtx n its)in Ast.
    pose proof(kind_sym_read b w R Fl Hload _ _ Hg)as Kind.
    unfold A.kind_sym in Kind;rewrite(w_decl_read w R Fl _ _ Hg)in Kind;cbn[A.d_ast]in Kind.
    rewrite Ast in Kind;cbn[A.kind_of]in Kind;injection Kind as Kind.
    assert(K:gs_kind cg=KCtx)by(apply kconv_ctx;symmetry;exact Kind).
    rewrite K,Ast in E.
    assert(Hcm:In(gs_mod cg)(w_order w))by exact(loader_mod_in w R Fl b Hload _ _ Hg).
    destruct(IH e(gs_mod cg)its sub sx chunk ss E Ix Wx Hcm Li(bs++ob)tm child rt mid Hchild Child)
    as(leaf&Hin&Route).
    destruct(ok_family_leaves fuel e(gs_mod cg)its sub _ _ _ Ix E)as[_ Ft].
    exists leaf;split;[exact Hin|].
    eapply FSRContext with(d:=A.Decl(gm_path(get_mod w(gs_mod cg)))(gs_decl cg)(gs_assumes cg))(n:=n);try eassumption;try reflexivity.
    exists si,sx;split;[exact Ii|];split;[exact Wi|];split;[exact(later_back w P R Fl _ _ Ft Li)|exact Sub].
    exact(w_decl_read w R Fl _ _ Hg). }
 intros ob tm sp rt mid Hin Leaf.
 destruct(family_forall2_pick _ _ _ _ _ Rel sp Hin)as(chunk&Hchunk&Found).
 destruct(Found ob tm rt mid Leaf)as(leaf&Hleaf&Route).
 exists leaf;split;[apply in_concat;exists chunk;split;assumption|exact Route].
Qed.
Lemma family_scan_route_selected:forall e mi outer ob tm sp target mid leaf,
 FamilyScanRoute e mi outer ob tm sp target mid leaf->In mi(w_order w)->
 let '(lmi,rg,mg,sub):=leaf in
 nget(w_syms w)(gs_id rg)=Some rg/\gs_id mg=mid/\In lmi(w_order w).
Proof.
 intros e mi outer ob tm sp target mid leaf Route;induction Route;intro Hmi;cbn.
 - split;[exact H4|];split;assumption.
 - apply IHRoute;exact(loader_mod_in w R Fl b Hload _ _ H5).
Qed.
End Trace.
