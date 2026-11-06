(* Completeness of the executable mutually recursive fixed-input visitor. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTy
 FreeVisitorCore ResultMentionExec ResultMentionsFold FixedInputsStep NestingTypesExec MethodLookup MethodLookupNone.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements free_type_syms result_mentions_go functor_fixed_go].
Module A:=Audit.Elaboration.
Definition mention_here(f self mi:nat)(d:option string)(g:gsym):M(list nat):=
 match d with
 | Some dn=>let! mo:=find_method_decl mi g dn in
   let! rr:=if kind_eqb(gs_kind g)KType then ret[gs_id g]else reqs(gs_id g)in
   let! ms:=match mo with Some m=>let! rs:=reqs(gs_id m)in signature_type_syms m rs|None=>ret[]end in
   ret(rr++ms)
 | None=>match gs_kind g,gs_decl g with
   | KCtx,ADCtx _ its=>result_mentions_go f self(gs_mod g)its[]
   | KVal,_|KFn,_=>reqs(gs_id g)
   | KFunctor,ADFunctor _ _ _ _ None=>if Nat.eqb(gs_id g)self then ret[]else functor_fixed_go f g
   | _,_=>ret[]end
 end.
Lemma ok_mention_here:forall f self mi d g,ok(mention_here f self mi d g).
Proof. intros;unfold mention_here;destruct d;okt. Qed.
Section Covers.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).
Definition FixedCovers(f:nat):Prop:=forall g n names args res s found s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n(Some names)args res None->
 functor_fixed_go f g s=Good(found,s')->
 Inv s->st_w s=w->Later w P R Fl s'->forall t,A.Fixed WW(gs_id g)t->In t found.
Lemma bound_notin:forall mi (bs:list(path*spec)) bl s s' t,
 mmap(fun b0=>let! l:=Elab.resolve_sym mi(fst b0)in ret(match l with Some x=>[gs_id x]|None=>[]end))bs s=Good(bl,s')->
 Inv s->st_w s=w->In mi(w_order w)->
 ~(exists b0,In b0 bs/\A.resolve_sym WW(gm_path(get_mod w mi))(fst b0)=Some t)->
 ~In t(List.concat bl).
Proof.
 intros mi bs bl s s' t H Hi Hw Hmi N Hin.
 apply N. exact(proj1(proj2(signature_bound w R Fl b Hload mi bs s bl s' H Hi Hw Hmi)t)Hin).
Qed.
Lemma here_valfn:forall f self mi g s found s' t,
 nget(w_syms w)(gs_id g)=Some g->
 (A.kind_sym WW(gs_id g)=Some A.DKVal\/A.kind_sym WW(gs_id g)=Some A.DKFn)->
 mention_here f self mi None g s=Good(found,s')->Inv s->Later w P R Fl s'->
 In t(R(gs_id g))->In t found.
Proof.
 intros f self mi g s found s' t Hg K H Hi L Ht.
 rewrite(kind_sym_rd w R Fl b Hload _ _ Hg)in K.
 unfold mention_here in H. destruct(gs_kind g);try(solve[destruct K;discriminate]);
 destruct(gs_decl g);try(exact(eq_ind _ (fun rs=>In t rs) Ht _ (reqs_R w R Fl _ _ _ _ Hi H L))).
Qed.
Lemma here_receiver:forall f self mi dn g s found s' t,
 nget(w_syms w)(gs_id g)=Some g->
 (t=gs_id g/\A.kind_sym WW(gs_id g)=Some A.DKType\/
 A.kind_sym WW(gs_id g)<>Some A.DKType/\In t(R(gs_id g)))->
 mention_here f self mi(Some dn)g s=Good(found,s')->Inv s->st_w s=w->Later w P R Fl s'->In t found.
Proof.
 intros f self mi dn g s found s' t Hg K H Hi Hw L.
 unfold mention_here in H.
 apply bind_inv in H as(mo&s1&Hm&H).
 assert(Om:ok(find_method_decl mi g dn))by okt.
 destruct(Om s mo s1 Hi Hm)as[I1 F1].
 apply bind_inv in H as(rr&s2&Hr&H).
 assert(Or:ok(if kind_eqb(gs_kind g)KType then ret[gs_id g]else reqs(gs_id g)))by okt.
 destruct(Or _ _ _ I1 Hr)as[I2 F2].
 apply bind_inv in H as(ms&s3&Hs&H). injection H as <- <-.
 assert(Os:ok(match mo with Some m=>let! rs:=reqs(gs_id m)in signature_type_syms m rs|None=>ret[]end))by(destruct mo;okt).
 destruct(Os _ _ _ I2 Hs)as[_ Fs].
 apply in_or_app;left.
 destruct(kind_eqb(gs_kind g)KType)eqn:Kt.
 - injection Hr as <- <-. apply kind_eqb_spec in Kt.
   rewrite(kind_sym_rd w R Fl b Hload _ _ Hg),Kt in K.
   destruct K as[ [-> _]|[N _]];[left;reflexivity|contradiction N;reflexivity].
 - rewrite(reqs_R w R Fl _ _ _ _ I1 Hr(later_back w P R Fl _ _ Fs L))in K.
   destruct K as[[-> K]|[_ K]];[|exact K].
   rewrite(kind_sym_rd w R Fl b Hload _ _ Hg)in K.
   destruct(gs_kind g);discriminate.
Qed.
Lemma here_context:forall f self mi g n its dc s found s' t sp,
 MentionCovers w (P:=P) R Fl f->nget(w_syms w)(gs_id g)=Some g->
 A.w_decl WW(gs_id g)=Some dc->A.d_ast dc=ADCtx n its->
 mention_here f self mi None g s=Good(found,s')->Inv s->st_w s=w->Later w P R Fl s'->
 In sp its->A.ResultMentions WW self(A.d_mod dc)sp t->In t found.
Proof.
 intros f self mi g n its dc s found s' t sp IH Hg Hd D H Hi Hw L Hin Hm.
 rewrite(w_decl_rd w R Fl _ _ Hg)in Hd;injection Hd as <-.
 cbn[A.d_ast]in D. cbn[A.d_mod]in Hm.
 assert(K:gs_kind g=KCtx).
 { pose proof(load_world_kind b w Hload _ _ Hg)as K. unfold KindOK in K.
   rewrite D in K. destruct(gs_kind g);simpl in K;congruence. }
 unfold mention_here in H;rewrite K,D in H.
 exact(IH self(gs_mod g)its[] s found s' H Hi Hw(loader_mod_in w R Fl b Hload _ _ Hg)L sp t Hin Hm).
Qed.
Lemma here_functor:forall f self mi g dc n names args res sp s found s' t,
 FixedCovers f->nget(w_syms w)(gs_id g)=Some g->A.w_decl WW(gs_id g)=Some dc->
 A.d_ast dc=ADFunctor n(Some names)args res None->gs_id g<>self->
 In sp res->A.ResultMentions WW(gs_id g)(A.d_mod dc)sp t->
 ~A.FunOutputs WW(gs_id g)t->
 ~(exists name,In name names/\A.resolve_sym WW(A.d_mod dc)[name]=Some t)->
 mention_here f self mi None g s=Good(found,s')->Inv s->st_w s=w->Later w P R Fl s'->In t found.
Proof.
 intros f self mi g dc n names args res sp s found s' t IH Hg Hd D N Hin Hm No Nnames H Hi Hw L.
 rewrite(w_decl_rd w R Fl _ _ Hg)in Hd;injection Hd as <-.
 cbn[A.d_ast]in D. cbn[A.d_mod]in Hm,Nnames.
 assert(K:gs_kind g=KFunctor).
 { pose proof(load_world_kind b w Hload _ _ Hg)as K. unfold KindOK in K.
   rewrite D in K. destruct(gs_kind g);simpl in K;congruence. }
 unfold mention_here in H;rewrite K,D in H.
 destruct(Nat.eqb(gs_id g)self)eqn:E;[apply Nat.eqb_eq in E;contradiction|].
 apply(IH g n names args res s found s' Hg D H Hi Hw L t).
 apply(proj2(fixed_named_iff w R Fl g n names args res t Hg D)).
 split;[exists sp;split;assumption|split;assumption].
Qed.
Lemma here_method:forall f self mi dn g m s found s' t,
 nget(w_syms w)(gs_id g)=Some g->In mi(w_order w)->
 A.MethodNamed WW(gm_path(get_mod w mi))(gs_id g)dn m->In t(R m)->A.SigMentions WW m t->
 mention_here f self mi(Some dn)g s=Good(found,s')->Inv s->st_w s=w->Later w P R Fl s'->In t found.
Proof.
 intros f self mi dn g m s found s' t Hg Hmi HM Ht Hsig H Hi Hw L.
 unfold mention_here in H. apply bind_inv in H as(mo&s1&Hm&H).
 destruct mo as[mg|].
 2:{destruct(find_method_decl_none b w R Fl mi g dn s s1 Hload Hmi Hg Hw Hm)as[_ N];exfalso;exact(N m HM). }
 destruct(find_method_decl_loaded b w R Fl mi g dn s mg s1 Hload Hmi Hg Hi Hw Hm)
  as[->[Hmg(rcv&nm&ps&rt&body&D)]].
 destruct(find_method_decl_named b w R Fl mi g dn s mg s Hload Hmi Hg Hi Hw Hm)as[_ Hnamed].
 pose proof(method_named_unique _ _ _ _ _ _ HM Hnamed)as Eq;subst m.
 apply bind_inv in H as(rr&s2&Hr&H).
 assert(Or:ok(if kind_eqb(gs_kind g)KType then ret[gs_id g]else reqs(gs_id g)))by okt.
 destruct(Or _ _ _ Hi Hr)as[I2 F2].
 assert(W2:st_w s2=w)by(rewrite(fr_w _ _ F2);exact Hw).
 apply bind_inv in H as(ms&s3&Hs&H);injection H as <- <-.
 apply in_or_app;right.
 apply bind_inv in Hs as(rs&s4&Hrs&Hs).
 destruct(ok_reqs(gs_id mg)s2 rs s4 I2 Hrs)as[I4 F4].
 assert(W4:st_w s4=w)by(rewrite(fr_w _ _ F4);exact W2).
 destruct(signature_type_syms_filter w (P:=P) R Fl b Hload mg rcv nm ps rt body rs s4 ms s3 Hmg D Hs I4 W4 L)
  as[test[-> Htest]].
 apply filter_In;split.
 - rewrite<-(reqs_R w R Fl _ _ _ _ I2 Hrs(later_back w P R Fl _ _(proj2(ok_signature_syms_frame mg rs _ _ _ I4 Hs))L));exact Ht.
 - apply Htest;exact Hsig.
Qed.
Lemma mention_item_covers:forall f,
 MentionCovers w (P:=P) R Fl f->FixedCovers f->
 forall self mi sp out s found s',
 mention_item f self mi out sp s=Good(found,s')->Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->
 forall t,In t out\/A.ResultMentions WW self(gm_path(get_mod w mi))sp t->In t found.
Proof.
 intros f IH IF self mi [pa d a]out s found s' H Hi Hw Hmi L t Ht.
 destruct pa as[pa|].
 2:{cbn[mention_item]in H;injection H as <- <-;destruct Ht as[Ht|Ht];[exact Ht|inversion Ht]. }
 unfold mention_item in H.
 apply bind_inv in H as(bl&s1&Hb&H).
 destruct(signature_bound w R Fl b Hload mi(opt_default[]a)s bl s1 Hb Hi Hw Hmi)as[-> B].
 apply bind_inv in H as(rhs&s2&Hrhs&H).
 assert(Orhs:ok(mfold(fun o b0=>match snd b0 with Spec(Some sp')_ sa=>free_type_syms 200(ATRef sp' sa)mi o|_=>ret o end)[](opt_default[]a)))by okt.
 destruct(Orhs _ _ _ Hi Hrhs)as[I2 F2].
 assert(W2:st_w s2=w)by(rewrite(fr_w _ _ F2);exact Hw).
 apply bind_inv in H as(tg&s3&Htg&H).
 destruct(esym_rel w R Fl b Hload mi pa s2 tg s3 I2 W2 Hmi Htg)as[-> Rt].
 destruct tg as[g|].
 2:{injection H as <- <-;destruct Ht as[Ht|Ht];[exact Ht|inversion Ht;congruence]. }
 destruct Rt as[Rt Hg].
 fold(mention_here f self mi d g)in H.
 apply bind_inv in H as(here&s4&Hhere&H);injection H as <- <-.
 destruct(ok_mention_here f self mi d g s2 here s4 I2 Hhere)as[_ Fh].
 pose proof(result_rhs_rel w (P:=P) R Fl b Hload mi(opt_default[]a)[]s rhs s2 Hrhs Hi Hw Hmi(later_back w P R Fl _ _ Fh L))as RHS.
 assert(Keep:forall x,~(exists b0,In b0(opt_default[]a)/\A.resolve_sym WW(gm_path(get_mod w mi))(fst b0)=Some x)->
 negb(nmem x(List.concat bl))=true).
 {intros x N;apply negb_true_iff;apply Bool.not_true_is_false;intro E;apply nmem_in in E;apply N;apply B;exact E. }
 unfold mentions_add;rewrite mentions_addl.
 destruct Ht as[Ht|Ht];[left;exact Ht|right].
 inversion Ht as
 [self0 p pa0 a0 v x Hr K Hin N
 |self0 p pa0 dn a0 r x Hr K N
 |self0 p pa0 dn a0 r m x Hr HM Hin Hsig N
 |self0 p pa0 d0 a0 bp sp0 dr sa x Hr Hin Hfree
 |self0 p pa0 a0 c dc nm its sp0 x Hr Hd D Hin HM N
 |self0 p pa0 a0 u du nm names args res sp0 x Hr Hd D Nself Hin HM Nout Nnames N];subst;
 try(rewrite Rt in Hr;injection Hr as <-).
 - apply in_or_app;left;apply filter_In;split;[eapply(here_valfn f self mi);eassumption|apply Keep;exact N].
 - apply in_or_app;left;apply filter_In;split;[eapply(here_receiver f self mi);eassumption|apply Keep;exact N].
 - apply in_or_app;left;apply filter_In;split;[eapply(here_method f self mi);eassumption|apply Keep;exact N].
 - apply in_or_app;right;apply RHS;right;exists(bp,Spec(Some sp0)dr sa);split;[exact Hin|exact Hfree].
 - apply in_or_app;left;apply filter_In;split;[eapply(here_context f self mi);eassumption|apply Keep;exact N].
 - apply in_or_app;left;apply filter_In;split;[eapply(here_functor f self mi);eassumption|apply Keep;exact N].
Qed.
Theorem visitor_covers:forall f,MentionCovers w (P:=P) R Fl f/\FixedCovers f.
Proof.
 induction f as[|f[IH IF]].
 - split.
   + unfold MentionCovers;intros self mi specs out s found s' H;cbn[result_mentions_go]in H;discriminate H.
   + unfold FixedCovers;intros g n names args res s found s' Hg D H;cbn[functor_fixed_go]in H;discriminate H.
 - split.
   + unfold MentionCovers;intros self mi specs out s found s' H Hi Hw Hmi L sp t Hin HM.
     rewrite result_mentions_view in H.
     eapply(mentions_fold w (P:=P) R Fl spec(mention_item f self mi)
      (fun sp t=>A.ResultMentions WW self(gm_path(get_mod w mi))sp t)specs).
     * intros;apply ok_mention_item.
     * intros q Hq o s0 ys s0' E I0 L0 x Hx.
       destruct(ok_mention_item f self mi o q s0 ys s0' I0 E)as[_ F].
       apply(mention_item_covers f IH IF self mi q o s0 ys s0' E I0
        (lt_w _ _ _ _ _(later_back w P R Fl _ _ F L0))Hmi L0 x Hx).
     * exact H.
     * exact Hi.
     * exact L.
     * right;exists sp;split;assumption.
   + unfold FixedCovers;intros g n names args res s found s' Hg D H Hi Hw L t HF.
     exact(fixed_go_step w (P:=P) R Fl b Hload f IH g n names args res s found s' Hg D H Hi Hw L t HF).
Qed.
Corollary result_mentions_covers:forall f,MentionCovers w (P:=P) R Fl f.
Proof. intro f;exact(proj1(visitor_covers f)). Qed.
Corollary abstract_fixed_go_covers:forall f g n names args res s found s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 functor_fixed_go f g s=Good(found,s')->Inv s->st_w s=w->Later w P R Fl s'->
 forall t,A.Fixed WW(gs_id g)t->In t found.
Proof.
 intros f g n names args res s found s' Hg D H Hi Hw L t HF.
 destruct names as[names|].
 - exact(proj2(visitor_covers f)g n names args res s found s' Hg D H Hi Hw L t HF).
 - exfalso;exact(fixed_unbracketed_false w R Fl g n args res t Hg D HF).
Qed.
Theorem functor_fixed_covers:forall g n names args res s found s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 functor_fixed g s=Good(found,s')->Inv s->st_w s=w->Later w P R Fl s'->
 forall t,A.Fixed WW(gs_id g)t->In t found.
Proof.
 intros g n names args res s found s' Hg D H Hi Hw L t HF.
 unfold functor_fixed in H;apply bind_inv in H as(f&s1&Hf&H).
 assert(Of:ok walk_fuel)by okt.
 destruct(Of _ _ _ Hi Hf)as[I1 F1].
 eapply abstract_fixed_go_covers;[exact Hg|exact D|exact H|exact I1| |exact L|exact HF].
 rewrite(fr_w _ _ F1);exact Hw.
Qed.
End Covers.
