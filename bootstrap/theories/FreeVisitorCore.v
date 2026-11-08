(* The actual signature visitor, read against the final fixed requirements. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsStable ElabFrameCore BodySoundCore BodyTy.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section Visitor.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Notation path_of mi:=(gm_path(get_mod w mi)).
Definition FreeStep(step:list nat->M(list nat))(mi:nat)(t:aty):Prop:=
 forall out s found s',step out s=Good(found,s')->Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->
 forall x,In x found<->In x out\/A.FreeIn WW R(path_of mi)t x.
Lemma free_fold:forall X(step:X->list nat->M(list nat))(T:X->aty)mi xs,
 (forall x out,ok(step x out))->(forall x,In x xs->FreeStep(step x)mi(T x))->
 forall out s found s',mfold(fun out x=>step x out)out xs s=Good(found,s')->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->
 forall y,In y found<->In y out\/(exists x,In x xs/\A.FreeIn WW R(path_of mi)(T x)y).
Proof.
 intros X step T mi xs Ok Hs. induction xs as[|x rest IH];intros out s found s' H Hi Hw Hmi L.
 - injection H as <- <-. intro y. cbn[In]. split;[intro Hy;left;exact Hy|intros[Hy|(x&[]&_)];exact Hy].
 - cbn[mfold]in H. apply bind_inv in H as(mid&s1&H1&H).
   destruct(Ok x out s mid s1 Hi H1)as[I1 F1].
   destruct(ok_mfold _ _ _ rest mid(fun o a=>Ok a o)s1 found s' I1 H)as[_ Ftail].
   pose proof(Hs x(or_introl eq_refl)out s mid s1 H1 Hi Hw Hmi(later_back w P R Fl _ _ Ftail L))as Rh.
   pose proof(IH(fun a Ha=>Hs a(or_intror Ha))mid s1 found s' H I1 ltac:(rewrite(fr_w _ _ F1);exact Hw)Hmi L)as Rt.
   intro y. specialize(Rh y). specialize(Rt y). clear -Rh Rt. rewrite Rt,Rh. cbn[In]. firstorder congruence.
Qed.
Lemma signature_bound:forall mi (bs:list(path*spec)) s bl s',
 mmap(fun b0=>let! l:=Elab.resolve_sym mi(fst b0)in ret(match l with Some x=>[gs_id x]|None=>[]end))bs s=Good(bl,s')->
 Inv s->st_w s=w->In mi(w_order w)->s'=s/\
 forall y,In y(List.concat bl)<->exists b0,In b0 bs/\A.resolve_sym WW(path_of mi)(fst b0)=Some y.
Proof.
 intros mi bs. induction bs as[|b0 rest IH];intros s bl s' H Hi Hw Hmi;cbn[mmap]in H.
 - injection H as <- <-. split;[reflexivity|]. intro y. cbn[In List.concat]. split;[intros[]|intros(b0&[]&_)].
 - apply bind_inv in H as(y0&s1&H1&H). apply bind_inv in H1 as(l&s2&H2&H1).
   destruct(esym_rel w R Fl b Hload mi(fst b0)s l s2 Hi Hw Hmi H2)as[-> Hl]. injection H1 as <- <-.
   apply bind_inv in H as(ys&s3&H3&H). destruct(IH s ys s3 H3 Hi Hw Hmi)as[-> Hys].
   injection H as <- <-. split;[reflexivity|]. intro y. cbn[List.concat]. rewrite in_app_iff,Hys. split.
   + intros[Hy|(b1&Hb1&Hr)];[|exists b1;split;[right;exact Hb1|exact Hr]].
     destruct l as[g|];[|destruct Hy]. destruct Hy as[<-|[]]. exists b0. split;[left;reflexivity|exact(proj1 Hl)].
   + intros(b1&[<-|Hb1]&Hr);[|right;exists b1;split;assumption]. left.
     destruct l as[g|];[destruct Hl as[Hr' _];rewrite Hr in Hr';injection Hr' as ->;left;reflexivity|rewrite Hl in Hr;discriminate].
Qed.
Lemma free_add_one:forall bound out r x,
 In x(if nmem r bound||nmem r out then out else out++[r])<->In x out\/(x=r/\~In x bound).
Proof.
 intros bound out r x. clear Hload w P R Fl b. change(In x(fold_left(fun o r=>if nmem r bound||nmem r o then o else o++[r])[r]out)<->In x out\/(x=r/\~In x bound)).
 rewrite fold_add_spec. cbn[In]. firstorder congruence.
Qed.
Theorem free_type_syms_rel:forall fuel t mi,FreeStep(fun out=>free_type_syms fuel t mi out)mi t.
Proof.
 induction fuel as[|f IH];intros t mi out s found s' H Hi Hw Hmi L;[discriminate H|].
 cbn[free_type_syms]in H. destruct t as[| | |fs|ms|pa a].
 1-3:injection H as <- <-;intro x;split;[intro Hy;left;exact Hy|intros[Hy|Hy];[exact Hy|inversion Hy]].
 - pose proof(free_fold _ (fun p out=>free_type_syms f(snd p)mi out)snd mi fs
    (fun p out=>ok_free_type_syms f(snd p)mi out)(fun p _=>IH(snd p)mi)
    out s found s' H Hi Hw Hmi L)as Rel.
   intro y. rewrite Rel. split.
   + intros[Hy|([n t]&Hin&Hy)];[left;exact Hy|right;eapply A.FRec;eassumption].
   + intros[Hy|Hy];[left;exact Hy|]. inversion Hy;subst. right;eexists;split;eassumption.
 - pose proof(free_fold _ (fun t out=>free_type_syms f t mi out)(fun t=>t)mi ms
    (fun t out=>ok_free_type_syms f t mi out)(fun t _=>IH t mi)
    out s found s' H Hi Hw Hmi L)as Rel.
   intro y. rewrite Rel. split.
   + intros[Hy|(t&Hin&Hy)];[left;exact Hy|right;eapply A.FUnion;eassumption].
   + intros[Hy|Hy];[left;exact Hy|]. inversion Hy;subst. right;eexists;split;eassumption.
 - apply bind_inv in H as(tg&s1&H1&H).
   destruct(esym_rel w R Fl b Hload mi pa s tg s1 Hi Hw Hmi H1)as[-> Hr].
   destruct tg as[g|].
   2:{ injection H as <- <-. intro y. split;[intro Hy;left;exact Hy|intros[Hy|Hy];[exact Hy|inversion Hy;subst;congruence]]. }
   destruct Hr as[Hr Hg]. apply bind_inv in H as(bl&s2&H2&H).
   destruct(signature_bound mi(opt_default [] a)s bl s2 H2 Hi Hw Hmi)as[-> Hbound].
   cbn beta zeta in H. apply bind_inv in H as(o1&s3&H3&H).
   set(Tb:=fun b0:path*spec=>match snd b0 with Spec(Some sp)_ sa=>ATRef sp sa|_=>ATUnit end).
   assert(Okb:forall (b0:path*spec)out,
    ok(match snd b0 with Spec(Some sp)_ sa=>free_type_syms f(ATRef sp sa)mi out|_=>ret out end)).
   { intros[bp[[sp|]d sa]]o;[apply ok_free_type_syms|apply ok_ret]. }
   assert(Sb:forall b0,In b0(opt_default [] a)->
    FreeStep(fun out=>match snd b0 with Spec(Some sp)_ sa=>free_type_syms f(ATRef sp sa)mi out|_=>ret out end)mi(Tb b0)).
   { intros[bp[[sp|]d sa]]_;[exact(IH(ATRef sp sa)mi)|].
     intros o si r sf E Ii Wi M Li. injection E as <- <-. intro y.
     split;[intro Hy;left;exact Hy|intros[Hy|Hy];[exact Hy|inversion Hy]]. }
   destruct(ok_mfold _ _ _ (opt_default [] a)out (fun o b0=>Okb b0 o)s o1 s3 Hi H3)as[I3 F3].
   assert(Otail:ok(if kind_eqb(gs_kind g)KType then
      ret(if nmem(gs_id g)(List.concat bl)||nmem(gs_id g)o1 then o1 else o1++[gs_id g])else
      let! rq:=type_requirements f(gs_id g)in
      ret(fold_left(fun o r=>if nmem r(List.concat bl)||nmem r o then o else o++[r])rq o1))).
   { destruct(kind_eqb(gs_kind g)KType);okt. }
   destruct(Otail _ _ _ I3 H)as[_ Ft].
   pose proof(free_fold _ (fun b0 out=>match snd b0 with Spec(Some sp)_ sa=>free_type_syms f(ATRef sp sa)mi out|_=>ret out end)
     Tb mi(opt_default [] a)Okb Sb out s o1 s3 H3 Hi Hw Hmi(later_back w P R Fl _ _ Ft L))as Br.
   destruct(kind_eqb(gs_kind g)KType)eqn:Kt.
   + injection H as <- <-. apply kind_eqb_spec in Kt.
     assert(Kw:A.kind_sym WW(gs_id g)=Some A.DKType).
     { rewrite(kind_sym_rd w R Fl b Hload _ _ Hg),Kt;reflexivity. }
     intro y. rewrite free_add_one,Br. split.
     * intros[[Hy|([bp[[sp|]d sa]]&Hin&Hy)]|[-> Hy]];[left;exact Hy| |inversion Hy|].
       -- right;eapply A.FBracket;[rewrite Hr;discriminate|exact Hin|exact Hy].
       -- right;apply A.FType;[exact Hr|exact Kw|]. intro Hex;apply Hy,Hbound;exact Hex.
     * intros[Hy|Hy];[left;left;exact Hy|].
       inversion Hy as[| |pa' a' t' Hr2 Hk2 Hn2|pa' a' g' x Hr2 Hk2 Hx2 Hn2|pa' a' bp sp d sa x Hresolved Hin Hsub];subst.
       -- rewrite Hr in Hr2;injection Hr2 as <-. right;split;[reflexivity|intro E;apply Hn2,Hbound,E].
       -- rewrite Hr in Hr2;injection Hr2 as <-;contradiction.
       -- left;right;exists(bp,Spec(Some sp)d sa);split;assumption.
   + apply bind_inv in H as(rq&s4&H4&H). injection H as <- <-.
     assert(Er:R(gs_id g)=rq).
     { apply(lt_reqs _ _ _ _ _ L). eapply(reqs_cached f);[intros g0 G0;exact(iv_ids _ I3 _ _ G0)|exact H4]. }
     assert(Kw:A.kind_sym WW(gs_id g)<>Some A.DKType).
     { rewrite(kind_sym_rd w R Fl b Hload _ _ Hg). destruct(gs_kind g);cbn[kconv kind_eqb]in Kt|-*;congruence. }
     intro y. rewrite fold_add_spec,Br. split.
     * intros[[Hy|([bp[[sp|]d sa]]&Hin&Hy)]|[Hy Hb]];[left;exact Hy| |inversion Hy|].
       -- right;eapply A.FBracket;[rewrite Hr;discriminate|exact Hin|exact Hy].
       -- right;eapply A.FNom;[exact Hr|exact Kw|rewrite Er;exact Hy|]. intro Hex;apply Hb,Hbound;exact Hex.
     * intros[Hy|Hy];[left;left;exact Hy|].
       inversion Hy as[| |pa' a' t' Hr2 Hk2 Hn2|pa' a' g' x Hr2 Hk2 Hx2 Hn2|pa' a' bp sp d sa x Hresolved Hin Hsub];subst.
       -- rewrite Hr in Hr2;injection Hr2 as <-;contradiction.
       -- rewrite Hr in Hr2;injection Hr2 as <-. right;split;[exact Hx2|intro E;apply Hn2,Hbound,E].
       -- left;right;exists(bp,Spec(Some sp)d sa);split;assumption.
Qed.

Lemma signature_mentions_fn:forall g rcv n ps rt body x,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn rcv n ps rt body->
 A.SigMentions WW(gs_id g)x<->
 ((exists pt,In pt ps/\A.FreeIn WW R(path_of(gs_mod g))(snd pt)x)\/
  exists t,rt=Some t/\A.FreeIn WW R(path_of(gs_mod g))t x).
Proof.
 intros g rcv n ps rt body x Hg D. unfold A.SigMentions. split.
 - intros(d&rcv'&n'&ps'&rt'&body'&Hd&D'&Hf).
   rewrite(w_decl_rd w R Fl _ _ Hg)in Hd;injection Hd as <-.
   cbn[A.d_ast]in D'. rewrite D in D'. injection D' as <- <- <- <- <-. exact Hf.
 - intro Hf. exists(A.Decl(path_of(gs_mod g))(gs_decl g)(gs_assumes g)),rcv,n,ps,rt,body.
   split;[exact(w_decl_rd w R Fl _ _ Hg)|]. split;[exact D|exact Hf].
Qed.
Lemma later_sigsyms:forall f s,Later w P R Fl(s_sigsyms f s)->Later w P R Fl s.
Proof. intros f s[L1 L2 L3 L4 L5 L6 L7 L8 L9]. constructor;assumption. Qed.
Theorem signature_type_syms_filter:forall g rcv n ps rt body rs s result s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn rcv n ps rt body->
 signature_type_syms g rs s=Good(result,s')->Inv s->st_w s=w->Later w P R Fl s'->
 exists test:nat->bool,result=filter test rs/\forall x,test x=true<->A.SigMentions WW(gs_id g)x.
Proof.
 intros g rcv n ps rt body rs s result s' Hg D H Hi Hw L.
 unfold signature_type_syms in H. rewrite D in H.
 apply bind_inv in H as(vis&sv&Hv&H). apply bind_inv in H as(u&su&Hu&H).
 unfold upd in Hu. injection Hu as <- <-. injection H as <- <-.
 apply later_sigsyms in L.
 apply bind_inv in Hv as(o&s1&Hfold&Hr).
 destruct(ok_mfold _ _ _ ps [] (fun out p=>ok_free_type_syms 200(snd p)(gs_mod g)out)s o s1 Hi Hfold)as[I1 F1].
 assert(Hw1:st_w s1=w)by(rewrite(fr_w _ _ F1);exact Hw).
 assert(Hmi:In(gs_mod g)(w_order w))by exact(loader_mod_in w R Fl b Hload _ _ Hg).
 assert(Or:ok(match rt with Some t=>free_type_syms 200 t(gs_mod g)o|None=>ret o end)).
 { destruct rt;[apply ok_free_type_syms|apply ok_ret]. }
 destruct(Or _ _ _ I1 Hr)as[_ Fr].
 pose proof(free_fold _ (fun p out=>free_type_syms 200(snd p)(gs_mod g)out)snd(gs_mod g)ps
  (fun p out=>ok_free_type_syms 200(snd p)(gs_mod g)out)(fun p _=>free_type_syms_rel 200(snd p)(gs_mod g))
  [] s o s1 Hfold Hi Hw Hmi(later_back w P R Fl _ _ Fr L))as Rfold.
 assert(M:forall x,In x vis<->A.SigMentions WW(gs_id g)x).
 { intro x. rewrite(signature_mentions_fn g rcv n ps rt body x Hg D).
   specialize(Rfold x). destruct rt as[t|].
   - pose proof(free_type_syms_rel 200 t(gs_mod g)o s1 vis sv Hr I1 Hw1 Hmi L x)as V.
     rewrite V,Rfold. cbn[In]. split.
     + intros[[[]|Hp]|Ht];[left;exact Hp|right;exists t;split;[reflexivity|exact Ht]].
     + intros[Hp|(t'&Et&Ht)];[left;right;exact Hp|]. injection Et as <-. right;exact Ht.
   - injection Hr as <- <-. rewrite Rfold. cbn[In]. split.
     + intros[[]|Hp]. left;exact Hp.
     + intros[Hp|(t'&E&_)];[right;exact Hp|discriminate E]. }
 exists(fun x=>nmem x vis). split;[reflexivity|]. intro x. rewrite nmem_in. exact(M x).
Qed.

Lemma filter_mask:forall(test:nat->bool)rs,
 map snd(filter fst(combine(map test rs)rs))=filter test rs.
Proof.
 intros test rs. induction rs as[|r rs IH];[reflexivity|].
 cbn[map combine filter fst snd]. destruct(test r);cbn[map];rewrite IH;reflexivity.
Qed.
Lemma ok_signature_syms_frame:forall g rs,ok(signature_type_syms g rs).
Proof. intros. unfold signature_type_syms;okt. Qed.
End Visitor.
