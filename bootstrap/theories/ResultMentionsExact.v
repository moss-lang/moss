(* Soundness of the executable mutually recursive result/fixed visitor. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTy
 FreeVisitorCore ResultMentionExec ResultMentionsFold ResultMentionsCover FixedInputsStep NestingTypesExec MethodLookup MethodLookupNone.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements free_type_syms result_mentions_go functor_fixed_go].
Module A:=ExecutionModel.Elaboration.
Section Exact.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Definition MentionSound(f:nat):Prop:=forall self mi specs out s found s',
 result_mentions_go f self mi specs out s=Good(found,s')->Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->
 forall t,In t found->In t out\/exists sp,In sp specs/\A.ResultMentions WW self(gm_path(get_mod w mi))sp t.
Definition FixedSound(f:nat):Prop:=forall g n names args res s found s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 functor_fixed_go f g s=Good(found,s')->Inv s->st_w s=w->Later w P R Fl s'->
 forall t,In t found->A.Fixed WW(gs_id g)t.
Lemma receiver_reqs_sound:forall g s rs s',
 nget(w_syms w)(gs_id g)=Some g->
 (if kind_eqb(gs_kind g)KType then ret[gs_id g]else reqs(gs_id g))s=Good(rs,s')->
 Inv s->Later w P R Fl s'->forall t,In t rs->
 (t=gs_id g/\A.kind_sym WW(gs_id g)=Some A.DKType\/
 A.kind_sym WW(gs_id g)<>Some A.DKType/\In t(R(gs_id g))).
Proof.
 intros g s rs s' Hg H Hi L t Hin.
 destruct(kind_eqb(gs_kind g)KType)eqn:K.
 - injection H as <- <-. destruct Hin as[<-|[]]. left;split;[reflexivity|].
   rewrite(kind_sym_rd w R Fl b Hload _ _ Hg). apply kind_eqb_spec in K;rewrite K;reflexivity.
 - right;split.
   + rewrite(kind_sym_rd w R Fl b Hload _ _ Hg). intro E.
     destruct(gs_kind g);try discriminate E;discriminate K.
   + rewrite(reqs_R w R Fl _ _ _ _ Hi H L);exact Hin.
Qed.
Lemma method_sig_reqs_sound:forall mi g dn mo s0 s rs s' t,
 In mi(w_order w)->nget(w_syms w)(gs_id g)=Some g->
 find_method_decl mi g dn s0=Good(mo,s0)->Inv s0->st_w s0=w->
 (match mo with Some m=>let! rr:=reqs(gs_id m)in signature_type_syms m rr|None=>ret[]end)s=Good(rs,s')->
 Inv s->st_w s=w->Later w P R Fl s'->In t rs->
 exists m,A.MethodNamed WW(gm_path(get_mod w mi))(gs_id g)dn m/\A.SigMentions WW m t.
Proof.
 intros mi g dn mo s0 s rs s' t Hmi Hg Hm I0 W0 H Hi Hw L Hin.
 destruct mo as[m|];[|injection H as <- <-;destruct Hin].
 destruct(find_method_decl_loaded b w R Fl mi g dn s0 m s0 Hload Hmi Hg I0 W0 Hm)
 as[_[Hmload(rcv&nm&ps&rt&body&D)]].
 destruct(find_method_decl_named b w R Fl mi g dn s0 m s0 Hload Hmi Hg I0 W0 Hm)as[_ HM].
 apply bind_inv in H as(rr&s1&Hr&H).
 destruct(ok_reqs(gs_id m)s rr s1 Hi Hr)as[I1 F1].
 assert(W1:st_w s1=w)by(rewrite(fr_w _ _ F1);exact Hw).
 destruct(signature_type_syms_filter w (P:=P) R Fl b Hload m rcv nm ps rt body rr s1 rs s' Hmload D H I1 W1 L)
 as(test&extra&->&Htest&_&Hextra).
 exists(gs_id m);split;[exact HM|].
 apply in_app_or in Hin as[Hin|Hin].
 - apply filter_In in Hin as[_ Test]. apply Htest;exact Test.
 - apply Hextra in Hin as[Hs _]. exact Hs.
Qed.
Lemma here_sound:forall f,MentionSound f->FixedSound f->
 forall self mi d g pa a s found s',
 nget(w_syms w)(gs_id g)=Some g->A.resolve_sym WW(gm_path(get_mod w mi))pa=Some(gs_id g)->
 mention_here f self mi d g s=Good(found,s')->Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->
 forall t,In t found->
 ~(exists q,In q(opt_default[]a)/\A.resolve_sym WW(gm_path(get_mod w mi))(fst q)=Some t)->
 A.ResultMentions WW self(gm_path(get_mod w mi))(Spec(Some pa)d a)t.
Proof.
 intros f IH IF self mi d g pa a s found s' Hg Resolve H Hi Hw Hmi L t Hin N.
 destruct d as[dn|].
 - unfold mention_here in H.
   apply bind_inv in H as(mo&s1&Hm&H).
   assert(Om:ok(find_method_decl mi g dn))by okt.
   destruct(Om _ _ _ Hi Hm)as[I1 F1].
   assert(W1:st_w s1=w)by(rewrite(fr_w _ _ F1);exact Hw).
   assert(Em:s1=s).
   {destruct mo as[m|].
    - exact(proj1(find_method_decl_named b w R Fl mi g dn s m s1 Hload Hmi Hg Hi Hw Hm)).
    - exact(proj1(find_method_decl_none b w R Fl mi g dn s s1 Hload Hmi Hg Hw Hm)). }
   subst s1.
   apply bind_inv in H as(rr&s2&Hr&H).
   assert(Or:ok(if kind_eqb(gs_kind g)KType then ret[gs_id g]else reqs(gs_id g)))by okt.
   destruct(Or _ _ _ Hi Hr)as[I2 F2].
   assert(W2:st_w s2=w)by(rewrite(fr_w _ _ F2);exact Hw).
   apply bind_inv in H as(ms&s3&Hs&H);injection H as <- <-.
   assert(Os:ok(match mo with Some m=>let! rs:=reqs(gs_id m)in signature_type_syms m rs|None=>ret[]end))by(destruct mo;okt).
   destruct(Os _ _ _ I2 Hs)as[_ Fs].
   apply in_app_or in Hin as[Hin|Hin].
   + eapply A.RMReceiver;[exact Resolve| |exact N].
     exact(receiver_reqs_sound g s rr s2 Hg Hr Hi(later_back w P R Fl _ _ Fs L)t Hin).
   + destruct(method_sig_reqs_sound mi g dn mo s s2 ms s3 t Hmi Hg Hm Hi Hw Hs I2 W2 L Hin)as[m[HM HS]].
     eapply A.RMMeth;[exact Resolve|exact HM|exact HS|exact N].
 - unfold mention_here in H.
   destruct(gs_kind g)eqn:K;destruct(gs_decl g)as[asm decls|tn|an aliasTy|tag pars pay|un|vn vt init|rcv fnn ps rt body|cn its|fgn names args0 res body]eqn:D;
   try solve[injection H as <- <-;destruct Hin];
   try solve[eapply A.RMValFn;[exact Resolve|rewrite(kind_sym_rd w R Fl b Hload _ _ Hg),K;cbn;tauto|change(In t(R(gs_id g)));rewrite(reqs_R w R Fl _ _ _ _ Hi H L);exact Hin|exact N]].
   + destruct(IH self(gs_mod g)its[]s found s' H Hi Hw(loader_mod_in w R Fl b Hload _ _ Hg)L t Hin)as[[]|(sp&Hsp&HM)].
     eapply A.RMCtx with(dc:=A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)).
     * exact Resolve.
     * exact(w_decl_rd w R Fl _ _ Hg).
     * cbn[A.d_ast];exact D.
     * exact Hsp.
     * exact HM.
     * exact N.
   + destruct body as[body|];[injection H as <- <-;destruct Hin|].
     destruct(Nat.eqb(gs_id g)self)eqn:E;[injection H as <- <-;destruct Hin|].
     apply Nat.eqb_neq in E.
     pose proof(IF g fgn names args0 res s found s' Hg D H Hi Hw L t Hin)as HF.
     destruct names as[names|].
     2:{exfalso;exact(fixed_unbracketed_false w R Fl g fgn args0 res t Hg D HF). }
     destruct(proj1(fixed_named_iff w R Fl g fgn names args0 res t Hg D)HF)as[(sp&Hsp&HM)[Hout Hnames]].
     eapply A.RMFunctor with(du:=A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)).
     * exact Resolve.
     * exact(w_decl_rd w R Fl _ _ Hg).
     * cbn[A.d_ast];exact D.
     * exact E.
     * exact Hsp.
     * exact HM.
     * exact Hout.
     * exact Hnames.
     * exact N.
Qed.
Lemma mention_item_sound:forall f,MentionSound f->FixedSound f->
 forall self mi sp out s found s',
 mention_item f self mi out sp s=Good(found,s')->Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->
 forall t,In t found->In t out\/A.ResultMentions WW self(gm_path(get_mod w mi))sp t.
Proof.
 intros f IH IF self mi [pa d a]out s found s' H Hi Hw Hmi L t Hin.
 destruct pa as[pa|].
 2:{cbn[mention_item]in H;injection H as <- <-;left;exact Hin. }
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
 2:{injection H as <- <-;left;exact Hin. }
 destruct Rt as[Rt Hg].
 fold(mention_here f self mi d g)in H.
 apply bind_inv in H as(here&s4&Hhere&H);injection H as <- <-.
 destruct(ok_mention_here f self mi d g s2 here s4 I2 Hhere)as[_ Fh].
 pose proof(result_rhs_rel w (P:=P) R Fl b Hload mi(opt_default[]a)[]s rhs s2 Hrhs Hi Hw Hmi(later_back w P R Fl _ _ Fh L))as RHS.
 unfold mentions_add in Hin;apply mentions_addl in Hin.
 destruct Hin as[Hin|Hin];[left;exact Hin|right].
 apply in_app_or in Hin as[Hin|Hin].
 - apply filter_In in Hin as[Hin Keep].
   apply(here_sound f IH IF self mi d g pa a s2 here s4 Hg Rt Hhere I2 W2 Hmi L t Hin).
   intros(q&Hq&Hr).
   assert(Member:In t(List.concat bl))by(apply B;exists q;split;assumption).
   apply nmem_in in Member;rewrite Member in Keep;discriminate Keep.
 - apply RHS in Hin as[[]|(q&Hq&Hfree)].
   destruct q as[bp[pa0 dr sa]];destruct pa0 as[pa0|].
   + eapply A.RMBracket;[rewrite Rt;discriminate|exact Hq|exact Hfree].
   + cbn[rhs_type]in Hfree;inversion Hfree.
Qed.
Lemma mention_fold_sound:forall X(step:list nat->X->M(list nat))(Pred:X->nat->Prop)xs,
 (forall out x,ok(step out x))->
 (forall x out s found s',step out x s=Good(found,s')->Inv s->Later w P R Fl s'->
  forall t,In t found->In t out\/Pred x t)->
 forall out s found s',mfold step out xs s=Good(found,s')->Inv s->Later w P R Fl s'->
 forall t,In t found->In t out\/exists x,In x xs/\Pred x t.
Proof.
 intros X step Pred xs O HS;induction xs as[|x rest IH];intros out s found s' H Hi L t Hin.
 - cbn[mfold]in H;injection H as <- <-;left;exact Hin.
 - cbn[mfold]in H;apply bind_inv in H as(mid&s1&H1&H).
   destruct(O out x s mid s1 Hi H1)as[I1 F1].
   destruct(ok_mfold _ _ _ rest mid(fun o a=>O o a)s1 found s' I1 H)as[_ Ft].
   destruct(IH mid s1 found s' H I1 L t Hin)as[Hmid|(a&Ha&Hp)].
   + destruct(HS x out s mid s1 H1 Hi(later_back w P R Fl _ _ Ft L)t Hmid)as[Hout|Hp].
     * left;exact Hout.
     * right;exists x;split;[left;reflexivity|exact Hp].
   + right;exists a;split;[right;exact Ha|exact Hp].
Qed.
Lemma names_keys_member:forall p names keys t,
 Forall2(fun name key=>A.resolve_sym WW p[name]=Some key/\A.kind_sym WW key=Some A.DKType)names keys->
 (exists name,In name names/\A.resolve_sym WW p[name]=Some t)->In t keys.
Proof.
 intros p names keys t Rel;induction Rel as[|n k names keys[Hr Hk]Rel IH];intros(name&Hin&Hres).
 - destruct Hin.
 - destruct Hin as[<-|Hin].
   + rewrite Hr in Hres;injection Hres as <-;left;reflexivity.
   + right;apply IH;exists name;split;assumption.
Qed.
Theorem visitor_sound:forall f,MentionSound f/\FixedSound f.
Proof.
 induction f as[|f[IH IF]].
 - split.
   + unfold MentionSound;intros self mi specs out s found s' H;cbn[result_mentions_go]in H;discriminate H.
   + unfold FixedSound;intros g n names args res s found s' Hg D H;cbn[functor_fixed_go]in H;discriminate H.
 - split.
   + unfold MentionSound;intros self mi specs out s found s' H Hi Hw Hmi L t Hin.
     rewrite result_mentions_view in H.
     eapply(mention_fold_sound spec(mention_item f self mi)
      (fun sp t=>A.ResultMentions WW self(gm_path(get_mod w mi))sp t)specs).
     * intros;apply ok_mention_item.
     * intros q o s0 ys s0' E I0 L0 x Hx.
       destruct(ok_mention_item f self mi o q s0 ys s0' I0 E)as[_ F].
       exact(mention_item_sound f IH IF self mi q o s0 ys s0' E I0
        (lt_w _ _ _ _ _(later_back w P R Fl _ _ F L0))Hmi L0 x Hx).
     * exact H.
     * exact Hi.
     * exact L.
     * exact Hin.
   + unfold FixedSound;intros g n names args res s found s' Hg D H Hi Hw L t Hin.
     cbn[functor_fixed_go]in H;apply bind_inv in H as(ps&s1&Hp&H).
     destruct ps as[keys|];[|injection H as <- <-;destruct Hin].
     destruct names as[names|].
     2:{unfold functor_params,functor_brackets in Hp;rewrite D in Hp;discriminate Hp. }
     destruct(functor_params_names w R Fl b Hload g n names args res None s keys s1 Hg D Hp Hi Hw)as[-> Pnames].
     unfold functor_parts in H;rewrite D in H;cbn beta iota zeta in H.
     apply bind_inv in H as(ms&s2&Hm&H);apply bind_inv in H as(rts&s3&Ho&H);injection H as <- <-.
     destruct(ok_result_mentions_go f(gs_id g)(gs_mod g)res[]s ms s2 Hi Hm)as[I2 F2].
     pose proof(fr_w' w _ _ F2 Hw)as W2.
     destruct(result_types_rel w R Fl b Hload g n(Some names)args res None s2 rts s3 Hg D Ho I2 W2)as[->[_ Outs]].
     apply filter_In in Hin as[Hin Masks];apply andb_true_iff in Masks as[Nkeys Nouts].
     destruct(IH(gs_id g)(gs_mod g)res[]s ms s2 Hm Hi Hw(loader_mod_in w R Fl b Hload _ _ Hg)L t Hin)as[[]|(sp&Hsp&HM)].
     apply(proj2(fixed_named_iff w R Fl g n names args res t Hg D));split;[exists sp;split;assumption|split].
     * intro Hout;apply(proj2(Outs t))in Hout;apply nmem_in in Hout;rewrite Hout in Nouts;discriminate Nouts.
     * intro Hex;apply(names_keys_member _ _ _ t Pnames)in Hex;apply nmem_in in Hex;rewrite Hex in Nkeys;discriminate Nkeys.
Qed.
Theorem result_mentions_go_sound:forall f,MentionSound f.
Proof. intro f;exact(proj1(visitor_sound f)). Qed.
Theorem functor_fixed_go_sound:forall f,FixedSound f.
Proof. intro f;exact(proj2(visitor_sound f)). Qed.
Theorem result_mentions_exact:forall g n names args res body s found s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res body->
 result_mentions g s=Good(found,s')->Inv s->st_w s=w->Later w P R Fl s'->
 forall t,In t found<->exists sp,In sp res/\A.ResultMentions WW(gs_id g)(gm_path(get_mod w(gs_mod g)))sp t.
Proof.
 intros g n names args res body s found s' Hg D H Hi Hw L t.
 unfold result_mentions in H;apply bind_inv in H as(f&s1&Hf&H).
 unfold functor_parts in H;rewrite D in H;cbn beta iota zeta in H.
 assert(Of:ok walk_fuel)by okt.
 destruct(Of _ _ _ Hi Hf)as[I1 F1].
 assert(W1:st_w s1=w)by(rewrite(fr_w _ _ F1);exact Hw).
 split.
 - intro Hin;destruct(result_mentions_go_sound(2*f)(gs_id g)(gs_mod g)res[]s1 found s' H I1 W1
   (loader_mod_in w R Fl b Hload _ _ Hg)L t Hin)as[[]|M];exact M.
 - intros(sp&Hsp&HM).
   exact(result_mentions_covers w (P:=P) R Fl b Hload(2*f)(gs_id g)(gs_mod g)res[]s1 found s' H I1 W1
    (loader_mod_in w R Fl b Hload _ _ Hg)L sp t Hsp HM).
Qed.
Theorem functor_fixed_exact:forall g n names args res s found s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 functor_fixed g s=Good(found,s')->Inv s->st_w s=w->Later w P R Fl s'->
 forall t,In t found<->A.Fixed WW(gs_id g)t.
Proof.
 intros g n names args res s found s' Hg D H Hi Hw L t;split.
 - unfold functor_fixed in H;apply bind_inv in H as(f&s1&Hf&H).
   assert(Of:ok walk_fuel)by okt;destruct(Of _ _ _ Hi Hf)as[I1 F1].
   exact(functor_fixed_go_sound(2*f)g n names args res s1 found s' Hg D H I1
     ltac:(rewrite(fr_w _ _ F1);exact Hw)L t).
 - exact(functor_fixed_covers w (P:=P) R Fl b Hload g n names args res s found s' Hg D H Hi Hw L t).
Qed.
End Exact.
