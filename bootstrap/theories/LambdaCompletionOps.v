(* Monadic composition of real new-forwarder completion histories. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv FnReadonly
 FnBound FnBoundEmit FnPresentStable FnPresentEmit ForwarderHistory ForwarderTrace LambdaCompletion.
Import ListNotations.
Set Implicit Arguments.
Definition CH{A}(m:M A):Prop:=PF m /\BF m /\forall b w s a final,
 load_world b=Good w->Inv s->FnBound s->st_w s=w->m s=Good(a,final)->NewLambdas s final.
Definition CH_at(owner:nat){A}(m:M A):Prop:=PF_except owner m /\BF_at owner m /\forall b w s a final,
 load_world b=Good w->Inv s->FnBound s->st_w s=w->owner<st_next s->m s=Good(a,final)->NewLambdas s final.
Arguments CH:simpl never.
Arguments CH_at:simpl never.
Lemma ch_keeps:forall A(m:M A),ok m->keeps st_fns m->keeps st_syms m->CH m.
Proof.
 intros A m O F S;split;[apply pf_keeps;assumption|]. split;[apply bf_keeps;assumption|].
 intros b w s a final Load I B W E;apply new_lambdas_same_symbols;exact(S _ _ _ E).
Qed.
Lemma ch_ret:forall A(a:A),CH(ret a).
Proof. intros;apply ch_keeps;[apply ok_ret|apply keeps_ret|apply keeps_ret]. Qed.
Lemma ch_bind:forall A B(m:M A)(k:A->M B),CH m->(forall a,CH(k a))->CH(mbind m k).
Proof.
 intros A B m k[PM[BM HM]]K;split;[apply pf_bind;[exact PM|intro;exact(proj1(K a))]|].
 split;[apply bf_bind;[exact BM|intro;exact(proj1(proj2(K a)))]|].
 intros b w s value final Load I Bound W E;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(proj1 PM _ _ _ I E1)as[IM F1].
 assert(BM':FnBound mid)by(exact(proj2 BM _ _ _ I Bound E1)).
 assert(WM:st_w mid=w)by(rewrite(fr_w _ _ F1);exact W).
 destruct(proj1(proj1(K a))_ _ _ IM E2)as[IF F2].
 eapply new_lambdas_trans;[exact IM|exact F1|exact F2|
 exact(proj2(proj1(K a))_ _ _ IM E2)|exact(HM b w _ _ _ Load I Bound W E1)|
 exact(proj2(proj2(K a))b w _ _ _ Load IM BM' WM E2)].
Qed.
Lemma ch_at_of_ch:forall A owner(m:M A),CH m->CH_at owner m.
Proof.
 intros A owner m[P[B H]];split;[apply pfe_of_pf;exact P|]. split;[apply bfa_of_bf;exact B|].
 intros b w s a final Load I Bound W Below E;exact(H b w _ _ _ Load I Bound W E).
Qed.
Lemma cha_bind:forall A B owner(m:M A)(k:A->M B),CH_at owner m->(forall a,CH_at owner(k a))->CH_at owner(mbind m k).
Proof.
 intros A B owner m k[PM[BM HM]]K;split;[apply pfe_bind;[exact PM|intro;exact(proj1(K a))]|].
 split;[apply bfa_bind;[exact BM|intro;exact(proj1(proj2(K a)))]|].
 intros b w s value final Load I Bound W Below E;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(proj1 PM _ _ _ I E1)as[IM F1].
 assert(BM':FnBound mid)by(exact(proj2 BM _ _ _ I Bound Below E1)).
 assert(WM:st_w mid=w)by(rewrite(fr_w _ _ F1);exact W).
 assert(BelowMid:owner<st_next mid)by(pose proof(fr_next _ _ F1);lia).
 destruct(proj1(proj1(K a))_ _ _ IM E2)as[IF F2].
 eapply new_lambdas_trans_except with(owner:=owner);[exact IM|exact F1|exact F2|exact Below|
 exact(proj2(proj1(K a))_ _ _ IM E2)|exact(HM b w _ _ _ Load I Bound W Below E1)|
 exact(proj2(proj2(K a))b w _ _ _ Load IM BM' WM BelowMid E2)].
Qed.
Lemma cha_emit:forall owner body,CH_at owner(emit_fn owner body).
Proof.
 intros owner body;split;[apply pfe_emit|]. split;[apply bfa_emit|].
 intros b w s a final Load I Bound W Below E;unfold emit_fn,upd in E;injection E as <- <-.
 apply new_lambdas_same_symbols;reflexivity.
Qed.
Lemma ch_mmap:forall A B(f:A->M B)xs,(forall x,CH(f x))->CH(mmap f xs).
Proof. intros A B f xs F;induction xs;cbn[mmap];[apply ch_ret|apply ch_bind;[apply F|intro;apply ch_bind;[exact IHxs|intro;apply ch_ret]]]. Qed.
Lemma ch_miter:forall A(f:A->M unit)xs,(forall x,CH(f x))->CH(miter f xs).
Proof. intros A f xs F;induction xs;cbn[miter];[apply ch_ret|apply ch_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma ch_mfold:forall A B(f:A->B->M A)xs acc,(forall a x,CH(f a x))->CH(mfold f acc xs).
Proof. intros A B f xs;induction xs;intros acc F;cbn[mfold];[apply ch_ret|apply ch_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma cha_mmap:forall A B owner(f:A->M B)xs,(forall x,CH_at owner(f x))->CH_at owner(mmap f xs).
Proof. intros A B owner f xs F;induction xs;cbn[mmap];[apply ch_at_of_ch,ch_ret|apply cha_bind;[apply F|intro;apply cha_bind;[exact IHxs|intro;apply ch_at_of_ch,ch_ret]]]. Qed.
Lemma cha_miter:forall A owner(f:A->M unit)xs,(forall x,CH_at owner(f x))->CH_at owner(miter f xs).
Proof. intros A owner f xs F;induction xs;cbn[miter];[apply ch_at_of_ch,ch_ret|apply cha_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma cha_mfold:forall A B owner(f:A->B->M A)xs acc,(forall a x,CH_at owner(f a x))->CH_at owner(mfold f acc xs).
Proof. intros A B owner f xs;induction xs;intros acc F;cbn[mfold];[apply ch_at_of_ch,ch_ret|apply cha_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma completion_new_sym_other_symbols:forall mi name kind decl orig s id final key,
 new_sym mi name kind decl orig s=Good(id,final)->key<>id->
 nget(st_syms final)key=nget(st_syms s)key.
Proof.
 intros mi name kind decl orig s id final key E N.
 cbn[new_sym mbind fresh upd ret]in E;injection E as <- <-.
 cbn[s_syms st_syms];rewrite nget_nset_other;[reflexivity|exact N].
Qed.
Lemma ch_new_sym_bind:forall A mi name kind decl orig(k:nat->M A),
 orig<>OSource->kind<>KTag /\kind<>KUnit->orig<>OLambda->
 (forall id,CH_at id(k id))->CH(mbind(new_sym mi name kind decl orig)k).
Proof.
 intros A mi name kind decl orig k Source Kind NotFwd K.
 split;[apply pf_new_sym_bind;[exact Source|exact Kind|intro;exact(proj1(K id))]|].
 split;[apply bf_new_sym_bind;[exact Source|exact Kind|intro;exact(proj1(proj2(K id)))]|].
 intros b w s value final Load I Bound W E;apply bind_inv in E as[id[mid[New Rest]]].
 destruct(ok_new_sym mi name kind decl orig Source Kind _ _ _ I New)as[IM FM].
 destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id[Next Allocated]].
 assert(BM:FnBound mid).
 {eapply fn_bound_keeps;[exact Bound|exact FM|]. assert(F:keeps st_fns(new_sym mi name kind decl orig))by(unfold new_sym;keepst). exact(F _ _ _ New). }
 assert(WM:st_w mid=w)by(rewrite(fr_w _ _ FM);exact W).
 assert(Below:id<st_next mid)by(rewrite Next;lia).
 pose proof(proj2(proj2(K id))b w _ _ _ Load IM BM WM Below Rest)as Hist.
 destruct(proj1(proj1(K id))_ _ _ IM Rest)as[IF FF].
 intros key g Lookup Origin Missing.
 destruct(Nat.eq_dec key id)as[Same|Different].
 - subst key;pose proof(fr_keep _ _ FF _ _ Allocated)as Same;rewrite Lookup in Same;injection Same as Eq;subst g.
   cbn[gs_origin]in Origin;exfalso;exact(NotFwd Origin).
 - eapply Hist;[exact Lookup|exact Origin|]. rewrite(@completion_new_sym_other_symbols _ _ _ _ _ _ _ _ _ New Different);exact Missing.
Qed.
Lemma ch_with_vars:forall A init(m:M A),CH m->CH(with_vars init m).
Proof.
 intros A init m[P[B H]];split;[apply pf_with_vars;exact P|]. split;[apply bf_with_vars;exact B|].
 intros b w s a final Load I Bound W E;unfold with_vars in E.
 apply bind_inv in E as[s0[s1[R0 E]]];injection R0 as <- <-.
 apply bind_inv in E as[u[s2[R1 E]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in E as[r[s3[Body E]]].
 apply bind_inv in E as[u'[s4[R2 E]]];unfold upd in R2;injection R2 as <- <-;injection E as <- <-.
 assert(IM:Inv(s_vars init(List.length init)s))by(destruct I;constructor;assumption).
 exact(H b w _ _ _ Load IM Bound W Body).
Qed.
Lemma cha_with_vars:forall A owner init(m:M A),CH_at owner m->CH_at owner(with_vars init m).
Proof.
 intros A owner init m[P[B H]];split;[apply pfe_with_vars;exact P|]. split;[apply bfa_with_vars;exact B|].
 intros b w s a final Load I Bound W Below E;unfold with_vars in E.
 apply bind_inv in E as[s0[s1[R0 E]]];injection R0 as <- <-.
 apply bind_inv in E as[u[s2[R1 E]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in E as[r[s3[Body E]]].
 apply bind_inv in E as[u'[s4[R2 E]]];unfold upd in R2;injection R2 as <- <-;injection E as <- <-.
 assert(IM:Inv(s_vars init(List.length init)s))by(destruct I;constructor;assumption).
 exact(H b w _ _ _ Load IM Bound W Below Body).
Qed.
Lemma cha_bind_sym:forall A owner id(k:gsym->M A),
 (forall g,gs_id g=id->CH_at owner(k g))->CH_at owner(mbind(sym id)k).
Proof.
 intros A owner id k K;split.
 - split.
   + intros s value final I E;apply bind_inv in E as[g[mid[Read Rest]]].
     unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate]. injection Read as <- <-.
     exact(proj1(proj1(K got(iv_ids s I id got Lookup)))_ _ _ I Rest).
   + intros s value final I E;apply bind_inv in E as[g[mid[Read Rest]]].
     unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate]. injection Read as <- <-.
     exact(proj2(proj1(K got(iv_ids s I id got Lookup)))_ _ _ I Rest).
 - split;[apply bfa_bind_sym;intro;intro;exact(proj1(proj2(K g H)))|].
   intros b w s value final Load I Bound W Below E;apply bind_inv in E as[g[mid[Read Rest]]].
   unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate]. injection Read as <- <-.
   exact(proj2(proj2(K got(iv_ids s I id got Lookup)))b w _ _ _ Load I Bound W Below Rest).
Qed.
