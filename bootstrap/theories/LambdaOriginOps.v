(* Composition of actual generated-code origins and header preservation. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv FnReadonly
 FnBound FnBoundEmit FnPresentStable FnPresentEmit ReifyHeaderStable ForwarderHistory ForwarderTrace LambdaOrigin ReifyHeader ReifyHeaderStable.
Import ListNotations.
Set Implicit Arguments.
Definition LH{A}(m:M A):Prop:=HS m /\BF m /\forall b w s a final,
 load_world b=Good w->Inv s->FnBound s->st_w s=w->m s=Good(a,final)->NewLambdaOrigins s final.
Definition LH_at(owner:nat){A}(m:M A):Prop:=HSE owner m /\BF_at owner m /\forall b w s a final,
 load_world b=Good w->Inv s->FnBound s->st_w s=w->owner<st_next s->m s=Good(a,final)->NewLambdaOrigins s final.
Arguments LH:simpl never.
Arguments LH_at:simpl never.
Lemma lh_keeps:forall A(m:M A),ok m->keeps st_fns m->keeps st_syms m->keeps function_headers m->LH m.
Proof.
 intros A m O F S H;split.
 - apply hs_keeps;[apply pf_keeps;assumption|apply header_keeps_of_rows;exact H].
 - split;[apply bf_keeps;assumption|].
   intros b w s a final Load I B W E;apply new_lambda_origins_same_symbols;exact(S _ _ _ E).
Qed.
Lemma lh_ret:forall A(a:A),LH(ret a).
Proof. intros;apply lh_keeps;[apply ok_ret|apply keeps_ret|apply keeps_ret|apply keeps_ret]. Qed.
Lemma lh_bind:forall A B(m:M A)(k:A->M B),LH m->(forall a,LH(k a))->LH(mbind m k).
Proof.
 intros A B m k[PM[BM HM]]K;split;[apply hs_bind;[exact PM|intro;exact(proj1(K a))]|].
 split;[apply bf_bind;[exact BM|intro;exact(proj1(proj2(K a)))]|].
 intros b w s value final Load I Bound W E;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(proj1(proj1 PM)_ _ _ I E1)as[IM F1].
 assert(BM':FnBound mid)by(exact(proj2 BM _ _ _ I Bound E1)).
 assert(WM:st_w mid=w)by(rewrite(fr_w _ _ F1);exact W).
 destruct(proj1(proj1(proj1(K a)))_ _ _ IM E2)as[IF F2].
 eapply new_lambda_origins_trans;[exact IM|exact F1|exact F2|
 exact(proj2(proj1(proj1(K a)))_ _ _ IM E2)| |exact(HM b w _ _ _ Load I Bound W E1)|
 exact(proj2(proj2(K a))b w _ _ _ Load IM BM' WM E2)].
 intros key body Below Present;apply(proj2(proj1(K a))_ _ _ IM E2 key Below).
 left;exists body;exact Present.
Qed.
Lemma lh_at_of_lh:forall A owner(m:M A),LH m->LH_at owner m.
Proof.
 intros A owner m[P[B H]];split;[apply hse_of_hs;exact P|]. split;[apply bfa_of_bf;exact B|].
 intros b w s a final Load I Bound W Below E;exact(H b w _ _ _ Load I Bound W E).
Qed.
Lemma lha_bind:forall A B owner(m:M A)(k:A->M B),LH_at owner m->(forall a,LH_at owner(k a))->LH_at owner(mbind m k).
Proof.
 intros A B owner m k[PM[BM HM]]K;split;[apply hse_bind;[exact PM|intro;exact(proj1(K a))]|].
 split;[apply bfa_bind;[exact BM|intro;exact(proj1(proj2(K a)))]|].
 intros b w s value final Load I Bound W Below E;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(proj1(proj1 PM)_ _ _ I E1)as[IM F1].
 assert(BM':FnBound mid)by(exact(proj2 BM _ _ _ I Bound Below E1)).
 assert(WM:st_w mid=w)by(rewrite(fr_w _ _ F1);exact W).
 assert(BelowMid:owner<st_next mid)by(pose proof(fr_next _ _ F1);lia).
 destruct(proj1(proj1(proj1(K a)))_ _ _ IM E2)as[IF F2].
 eapply new_lambda_origins_trans_except with(excluded:=owner);[exact IM|exact F1|exact F2|exact Below|
 exact(proj2(proj1(proj1(K a)))_ _ _ IM E2)| |exact(HM b w _ _ _ Load I Bound W Below E1)|
 exact(proj2(proj2(K a))b w _ _ _ Load IM BM' WM BelowMid E2)].
 intros key body Other KeyBound Present;apply(proj2(proj1(K a))_ _ _ IM E2 key Other KeyBound).
 left;exists body;exact Present.
Qed.
Lemma lha_emit:forall owner body,LH_at owner(emit_fn owner body).
Proof.
 intros owner body;split;[apply hse_emit|]. split;[apply bfa_emit|].
 intros b w s a final Load I Bound W Below E;unfold emit_fn,upd in E;injection E as <- <-.
 apply new_lambda_origins_same_symbols;reflexivity.
Qed.
Lemma lh_mmap:forall A B(f:A->M B)xs,(forall x,LH(f x))->LH(mmap f xs).
Proof. intros A B f xs F;induction xs;cbn[mmap];[apply lh_ret|apply lh_bind;[apply F|intro;apply lh_bind;[exact IHxs|intro;apply lh_ret]]]. Qed.
Lemma lh_miter:forall A(f:A->M unit)xs,(forall x,LH(f x))->LH(miter f xs).
Proof. intros A f xs F;induction xs;cbn[miter];[apply lh_ret|apply lh_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma lh_mfold:forall A B(f:A->B->M A)xs acc,(forall a x,LH(f a x))->LH(mfold f acc xs).
Proof. intros A B f xs;induction xs;intros acc F;cbn[mfold];[apply lh_ret|apply lh_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma lha_mmap:forall A B owner(f:A->M B)xs,(forall x,LH_at owner(f x))->LH_at owner(mmap f xs).
Proof. intros A B owner f xs F;induction xs;cbn[mmap];[apply lh_at_of_lh,lh_ret|apply lha_bind;[apply F|intro;apply lha_bind;[exact IHxs|intro;apply lh_at_of_lh,lh_ret]]]. Qed.
Lemma lha_miter:forall A owner(f:A->M unit)xs,(forall x,LH_at owner(f x))->LH_at owner(miter f xs).
Proof. intros A owner f xs F;induction xs;cbn[miter];[apply lh_at_of_lh,lh_ret|apply lha_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma lha_mfold:forall A B owner(f:A->B->M A)xs acc,(forall a x,LH_at owner(f a x))->LH_at owner(mfold f acc xs).
Proof. intros A B owner f xs;induction xs;intros acc F;cbn[mfold];[apply lh_at_of_lh,lh_ret|apply lha_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma completion_new_sym_other_symbols:forall mi name kind decl orig s id final key,
 new_sym mi name kind decl orig s=Good(id,final)->key<>id->
 nget(st_syms final)key=nget(st_syms s)key.
Proof.
 intros mi name kind decl orig s id final key E N.
 cbn[new_sym mbind fresh upd ret]in E;injection E as <- <-.
 cbn[s_syms st_syms];rewrite nget_nset_other;[reflexivity|exact N].
Qed.
Lemma lh_new_sym_bind:forall A mi name kind decl orig(k:nat->M A),
 orig<>OSource->kind<>KTag /\kind<>KUnit->(orig<>OLambda/\forall target,orig<>OForwarder target)->
 (forall id,LH_at id(k id))->LH(mbind(new_sym mi name kind decl orig)k).
Proof.
 intros A mi name kind decl orig k Source Kind NotFwd K.
 split;[apply hs_new_sym_bind;[exact Source|exact Kind|intro;exact(proj1(K id))]|].
 split;[apply bf_new_sym_bind;[exact Source|exact Kind|intro;exact(proj1(proj2(K id)))]|].
 intros b w s value final Load I Bound W E;apply bind_inv in E as[id[mid[New Rest]]].
 destruct(ok_new_sym mi name kind decl orig Source Kind _ _ _ I New)as[IM FM].
 destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id[Next Allocated]].
 assert(BM:FnBound mid).
 {eapply fn_bound_keeps;[exact Bound|exact FM|]. assert(F:keeps st_fns(new_sym mi name kind decl orig))by(unfold new_sym;keepst). exact(F _ _ _ New). }
 assert(WM:st_w mid=w)by(rewrite(fr_w _ _ FM);exact W).
 assert(Below:id<st_next mid)by(rewrite Next;lia).
 pose proof(proj2(proj2(K id))b w _ _ _ Load IM BM WM Below Rest)as Hist.
 destruct(proj1(proj1(proj1(K id)))_ _ _ IM Rest)as[IF FF].
 intros key g Lookup Origin Missing.
 destruct(Nat.eq_dec key id)as[Same|Different].
 - subst key;pose proof(fr_keep _ _ FF _ _ Allocated)as Same;rewrite Lookup in Same;injection Same as Eq;subst g.
   cbn[gs_origin]in Origin;exfalso;destruct Origin as[Lam|[target Fwd]];[exact(proj1 NotFwd Lam)|exact(proj2 NotFwd target Fwd)].
 - eapply Hist;[exact Lookup|exact Origin|]. rewrite(@completion_new_sym_other_symbols _ _ _ _ _ _ _ _ _ New Different);exact Missing.
Qed.
Lemma lh_with_vars:forall A init(m:M A),LH m->LH(with_vars init m).
Proof.
 intros A init m[P[B H]];split;[apply hs_with_vars;exact P|]. split;[apply bf_with_vars;exact B|].
 intros b w s a final Load I Bound W E;unfold with_vars in E.
 apply bind_inv in E as[s0[s1[R0 E]]];injection R0 as <- <-.
 apply bind_inv in E as[u[s2[R1 E]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in E as[r[s3[Body E]]].
 apply bind_inv in E as[u'[s4[R2 E]]];unfold upd in R2;injection R2 as <- <-;injection E as <- <-.
 assert(IM:Inv(s_vars init(List.length init)s))by(destruct I;constructor;assumption).
 exact(H b w _ _ _ Load IM Bound W Body).
Qed.
Lemma lha_with_vars:forall A owner init(m:M A),LH_at owner m->LH_at owner(with_vars init m).
Proof.
 intros A owner init m[P[B H]];split;[apply hse_with_vars;exact P|]. split;[apply bfa_with_vars;exact B|].
 intros b w s a final Load I Bound W Below E;unfold with_vars in E.
 apply bind_inv in E as[s0[s1[R0 E]]];injection R0 as <- <-.
 apply bind_inv in E as[u[s2[R1 E]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in E as[r[s3[Body E]]].
 apply bind_inv in E as[u'[s4[R2 E]]];unfold upd in R2;injection R2 as <- <-;injection E as <- <-.
 assert(IM:Inv(s_vars init(List.length init)s))by(destruct I;constructor;assumption).
 exact(H b w _ _ _ Load IM Bound W Below Body).
Qed.
Lemma lha_bind_sym:forall A owner id(k:gsym->M A),
 (forall g,gs_id g=id->LH_at owner(k g))->LH_at owner(mbind(sym id)k).
Proof.
 intros A owner id k K;split.
 - apply hse_bind_sym;intros g Id;exact(proj1(K g Id)).
 - split;[apply bfa_bind_sym;intro;intro;exact(proj1(proj2(K g H)))|].
   intros b w s value final Load I Bound W Below E;apply bind_inv in E as[g[mid[Read Rest]]].
   unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate]. injection Read as <- <-.
   exact(proj2(proj2(K got(iv_ids s I id got Lookup)))b w _ _ _ Load I Bound W Below Rest).
Qed.
(* At an actual allocation, the enclosing real invocation supplies its own
   origin. The recursive continuation supplies all other new lambdas. *)
Lemma lambda_origin_created_run:forall A mi name kind decl orig(k:nat->M A)
 b w initial before id mid value final,
 orig<>OSource->kind<>KTag/\kind<>KUnit->(forall key,LH_at key(k key))->
 load_world b=Good w->Inv before->FnBound before->st_w before=w->Fr initial before->
 NewLambdaOrigins initial before->
 new_sym mi name kind decl orig before=Good(id,mid)->k id mid=Good(value,final)->
 (exists body,nassoc id(st_fns final)=Some body/\LambdaOrigin id body(function_header final id)orig)->
 NewLambdaOrigins initial final.
Proof.
 intros A mi name kind decl orig k b w initial before id mid value final Source Kind K Load I Bound World Frame Prefix New Rest Own.
 assert(Whole:HS(mbind(new_sym mi name kind decl orig)k)).
 {apply hs_new_sym_bind;[exact Source|exact Kind|intro;exact(proj1(K id0))]. }
 assert(Run:mbind(new_sym mi name kind decl orig)k before=Good(value,final)).
 {unfold mbind;rewrite New;exact Rest. }
 destruct(proj1(proj1 Whole)_ _ _ I Run)as[If Fwhole].
 destruct(ok_new_sym mi name kind decl orig Source Kind _ _ _ I New)as[Im Fnew].
 destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id[Next Allocated]].
 assert(Bmid:FnBound mid).
 {eapply fn_bound_keeps;[exact Bound|exact Fnew|].
  assert(Keep:keeps st_fns(new_sym mi name kind decl orig))by(unfold new_sym;keepst).
  exact(Keep _ _ _ New). }
 pose proof(proj2(proj2(K id))b w _ _ _ Load Im Bmid ltac:(rewrite(fr_w _ _ Fnew);exact World)
  ltac:(rewrite Next;lia)Rest)as Nested.
 destruct(proj1(proj1(proj1(K id)))_ _ _ Im Rest)as[IfTail FTail].
 assert(Tail:NewLambdaOrigins before final).
 {intros key owner At Origin Missing;destruct(Nat.eq_dec key id)as[->|Other].
  - pose proof(fr_keep _ _ FTail _ _ Allocated)as Kept.
    rewrite At in Kept;injection Kept as Same;subst owner;exact Own.
  -
  eapply Nested;[exact At|exact Origin|].
  rewrite(@completion_new_sym_other_symbols _ _ _ _ _ _ _ _ _ New Other);exact Missing. }
 eapply new_lambda_origins_trans;[exact I|exact Frame|exact Fwhole|
 exact(proj2(proj1 Whole)_ _ _ I Run)| |exact Prefix|exact Tail].
 intros key body Below Present;apply(proj2 Whole _ _ _ I Run key Below).
 left;exists body;exact Present.
Qed.
