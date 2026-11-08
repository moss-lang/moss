(* Receiver instantiation installs meanings without changing canonical types. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame InfoPreserve WorldSound BodySoundCore BodyTr BodyTy
 BindType CanonTypes Merge NominalSupport TyOfRestrict ProviderSig.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL canon].
Definition receiver_args(e:env)(r:ety):list(nat*ety):=
 match canon e r with YNom _ xs=>xs|_=>[]end.
Definition receiver_types(xs:list(nat*ety))(e:env):M env:=
 mfold(fun en q=>let! stamp:=fresh in ret(set_tn en(fst q)(snd q)stamp))e xs.
Definition receiver_setup(e:env)(r:ety):M env:=
 match canon e r with
 |YNom _ ((_::_)as xs)=>receiver_types xs e
 |_=>ret e end.
Lemma receiver_setup_view:forall e r,receiver_setup e r=receiver_types(receiver_args e r)e.
Proof. intros e r;unfold receiver_setup,receiver_args;destruct(canon e r);try reflexivity;destruct l;reflexivity. Qed.
Lemma ok_receiver_types:forall xs e,ok(receiver_types xs e).
Proof. intros;unfold receiver_types;okt. Qed.
Lemma ni_receiver_types:forall xs e,NI(receiver_types xs e).
Proof. intros;unfold receiver_types;nit. Qed.
Section Receiver.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Notation WW:=(BodySoundCore.W w R Fl).
Definition recv_sem(xs:list(nat*ety)):=map(fun p=>(fst p,tr Fl(snd p)))xs.
Lemma receiver_types_context:forall xs c e F C s e2 s',
 receiver_types xs e s=Good(e2,s')->CxRel w R Fl c e F C->
 Forall(fun p=>WFty(snd p)/\Support WW(snd p)/\Canon(en_uf e)(snd p))xs->
 en_uf e2=en_uf e/\
 CxRel w R Fl c e2 F(fold_left(fun C p=>A.set_ty C(fst p)(tr Fl(snd p)))xs C).
Proof.
 induction xs as[|[k y]rest IH];intros c e F C s e2 s' H Hc Props.
 - cbn[receiver_types mfold]in H;injection H as <- <-;split;[reflexivity|exact Hc].
 - inversion Props as[|p ps[Wy[Sy Cy]]Rest];subst.
   unfold receiver_types in H;cbn[mfold]in H.
   apply bind_inv in H as(en&s1&Hs&H).
   apply bind_inv in Hs as(stamp&s2&Hstamp&Hret);injection Hret as <- <-.
   assert(Fix:canon e y=y)by(rewrite canon_res;unfold Res;apply canon_fix;exact Cy).
   pose proof(cxrel_type w R Fl c e F C k y stamp Hc Wy Sy)as C1;rewrite Fix in C1.
   destruct(IH c(set_tn e k y stamp)F(A.set_ty C k(tr Fl y))s2 e2 s' H C1 Rest)as[U HC].
   split;[exact U|exact HC].
Qed.
Lemma receiver_args_properties:forall c e F C r,
 CxRel w R Fl c e F C->WFty r->Support WW r->
 Forall(fun p=>WFty(snd p)/\Support WW(snd p)/\Canon(en_uf e)(snd p))(receiver_args e r).
Proof.
 intros c e F C r Hc Wr Sr.
 pose proof(canon_wf e r(proj2(cr_uf _ _ _ _ _ _ _ Hc))Wr)as Wc.
 pose proof(@canon_support WW e r(@es_uf WW e(cr_support _ _ _ _ _ _ _ Hc))Sr)as Sc.
 pose proof(canon_tri e r(proj1(cr_uf _ _ _ _ _ _ _ Hc)))as Can.
 unfold receiver_args;destruct(canon e r)as[| |g|n xs|ms|fs];try constructor.
 destruct(@support_nominal WW n xs Sc)as[_[_ Sub]].
 pose proof(canon_nom_parts e n xs Can)as Cs.
 apply Forall_forall;intros p Hin;split.
 - unfold WFty in Wc;cbn[ety_wf]in Wc;rewrite forallb_forall in Wc;exact(Wc p Hin).
 - split;[exact(proj1(Forall_forall _ _)Sub p Hin)|].
   exact(proj1(Forall_forall _ _)Cs(snd p)(in_map snd _ _ Hin)).
Qed.
Lemma receiver_args_nodup:forall c e F C r,
 CxRel w R Fl c e F C->Support WW r->(forall n,NoDup(A.w_reqs WW n))->
 NoDup(map fst(receiver_args e r)).
Proof.
 intros c e F C r Hc Sr Nd.
 pose proof(@canon_support WW e r(@es_uf WW e(cr_support _ _ _ _ _ _ _ Hc))Sr)as Sc.
 unfold receiver_args;destruct(canon e r)as[| |g|n xs|ms|fs];try constructor.
 destruct(@support_nominal WW n xs Sc)as[_[Keys _]];rewrite Keys;apply Nd.
Qed.
Lemma nlook_key_missing:forall k(l:list(nat*ty)),~In k(map fst l)->A.nlook k l=None.
Proof.
 intros k l;induction l as[|[a v]l IH];intro N;[reflexivity|].
 cbn[A.nlook];destruct(Nat.eqb k a)eqn:E.
 - apply Nat.eqb_eq in E;subst a;exfalso;apply N;left;reflexivity.
 - apply IH;intro Hin;apply N;right;exact Hin.
Qed.
Lemma nlook_rev_keys:forall(l:list(nat*ty)),NoDup(map fst l)->forall k,A.nlook k(rev l)=A.nlook k l.
Proof.
 intros l;induction l as[|[a v]rest IH];intros Nd k;[reflexivity|].
 inversion Nd as[|a0 keys N Nd0];subst.
 cbn[rev A.nlook];rewrite nlook_app,IH by exact Nd0.
 destruct(Nat.eqb k a)eqn:E.
 - apply Nat.eqb_eq in E;subst k;rewrite(nlook_key_missing a rest N);cbn[A.nlook];rewrite Nat.eqb_refl;reflexivity.
 - destruct(A.nlook k rest);[reflexivity|cbn[A.nlook];rewrite E;reflexivity].
Qed.
Lemma fold_set_types_map:forall xs C,
 A.c_ty(fold_left(fun C p=>A.set_ty C(fst p)(tr Fl(snd p)))xs C)=rev(recv_sem xs)++A.c_ty C.
Proof.
 induction xs as[|[k y]rest IH];intro C;[reflexivity|].
 cbn[fold_left];rewrite IH;unfold recv_sem;cbn[map rev A.set_ty A.c_ty].
 rewrite<-app_assoc;reflexivity.
Qed.
Lemma receiver_tymap_lookup:forall xs C,
 NoDup(map fst xs)->forall k,
 A.nlook k(A.c_ty(fold_left(fun C p=>A.set_ty C(fst p)(tr Fl(snd p)))xs C))=
 A.nlook k(recv_sem xs++A.c_ty C).
Proof.
 intros xs C Nd k;rewrite fold_set_types_map,!nlook_app,nlook_rev_keys;[reflexivity|].
 unfold recv_sem;rewrite map_map;change(NoDup(map fst xs));exact Nd.
Qed.
Lemma sigof_lookup_ext:forall W, A.RelevanceClosed W(A.w_reqs W)->forall u tm tm' this sg,
 (forall k,A.nlook k tm'=A.nlook k tm)->A.SigOf W u tm this sg->A.SigOf W u tm' this sg.
Proof.
 intros W Closed u tm tm' this sg Eq(d&rcv&n&ps&rt&body&Hd&D&Params&Ret).
 exists d,rcv,n,ps,rt,body;split;[exact Hd|split;[exact D|split]].
 - assert(Gen:forall(ps:list(name*aty))(ts:list ty),
    Forall2(fun pt tau=>A.TyOf W(A.d_mod d)tm this(snd pt)tau)ps ts->
    Forall2(fun pt tau=>A.TyOf W(A.d_mod d)tm' this(snd pt)tau)ps ts).
   {intros ps0 ts H;induction H;constructor;[|exact IHForall2].
    eapply(@tyof_restrict W Closed _ _ _ _ _ H tm');intros;apply Eq. }
   exact(Gen _ _ Params).
 - destruct rt;[|exact Ret].
   eapply(@tyof_restrict W Closed _ _ _ _ _ Ret tm');intros;apply Eq.
Qed.
Lemma canonical_sig_uf:forall e e' sg,en_uf e'=en_uf e->canonical_sig Fl e' sg=canonical_sig Fl e sg.
Proof.
 intros e e' sg U;unfold canonical_sig;f_equal.
 - apply map_ext;intro p;rewrite(canon_uf e e' U);reflexivity.
 - rewrite(canon_uf e e' U);reflexivity.
Qed.
Lemma fold_receiver_sem:forall xs C,
 fold_left(fun C p=>A.set_ty C(fst p)(snd p))(recv_sem xs)C=
 fold_left(fun C p=>A.set_ty C(fst p)(tr Fl(snd p)))xs C.
Proof. induction xs as[|[k y]rest IH];intro C;[reflexivity|]. cbn[recv_sem fold_left map fst snd];apply IH. Qed.
Lemma receiver_args_head:forall e r h,head(canon e r)=Some h->
 recv_sem(receiver_args e r)=match tr Fl(canon e r)with TNom _ xs=>xs|_=>[]end.
Proof.
 intros e r h H;unfold receiver_args;destruct(canon e r);cbn[head]in H;try discriminate H;reflexivity.
Qed.
End Receiver.
