(* Actual preservation of the historical lambda-body invariant. A pending
   fresh owner may emit its first body once, after compiling nested code. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly FnBound FnBoundEmit
 LambdaReadonly LambdaRows ForwarderTrace ForwarderHistory.
Import ListNotations.
Set Implicit Arguments.
Definition LC{A}(m:M A):Prop:=BF m/\forall s a final,Inv s->FnBound s->LambdaRows s->m s=Good(a,final)->LambdaRows final.
Definition LCP(owner:nat){A}(m:M A):Prop:=BF_at owner m/\forall s a final,
 Inv s->FnBound s->LambdaRows s->
 (exists g,nget(st_syms s)owner=Some g/\gs_origin g=OLambda)->
 nassoc owner(st_fns s)=None->owner<st_next s->m s=Good(a,final)->LambdaRows final.
Arguments LC:simpl never.
Arguments LCP:simpl never.
Lemma lc_keeps:forall A(m:M A),BF m->keeps st_fns m->LC m.
Proof. intros A m B K;split;[exact B|]. intros s a final I Bound Rows E.
 exact(lambda_rows_kept s final Bound Rows(proj2(proj1 B _ _ _ I E))(K _ _ _ E)). Qed.
Lemma lc_ret:forall A(a:A),LC(ret a).
Proof. intros;apply lc_keeps;[apply bf_ret|apply keeps_ret]. Qed.
Lemma lc_bind:forall A B(m:M A)(k:A->M B),LC m->(forall a,LC(k a))->LC(mbind m k).
Proof.
 intros A B m k[Bm M]K;split;[apply bf_bind;[exact Bm|intro;exact(proj1(K a))]|].
 intros s value final I Bound Rows E;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(proj1 Bm _ _ _ I E1)as[Im Frame].
 exact(proj2(K a)_ _ _ Im(proj2 Bm _ _ _ I Bound E1)(M _ _ _ I Bound Rows E1)E2).
Qed.
Lemma lc_mmap:forall A B(f:A->M B)xs,(forall x,LC(f x))->LC(mmap f xs).
Proof. intros A B f xs F;induction xs;cbn[mmap];[apply lc_ret|apply lc_bind;[apply F|intro;apply lc_bind;[exact IHxs|intro;apply lc_ret]]]. Qed.
Lemma lc_miter:forall A(f:A->M unit)xs,(forall x,LC(f x))->LC(miter f xs).
Proof. intros A f xs F;induction xs;cbn[miter];[apply lc_ret|apply lc_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma lc_mfold:forall A B(f:A->B->M A)xs a,(forall a x,LC(f a x))->LC(mfold f a xs).
Proof. intros A B f xs;induction xs;intros acc F;cbn[mfold];[apply lc_ret|apply lc_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma lcp_of_lc:forall A owner(m:M A),LC m->LCP owner m.
Proof.
 intros A owner m[B M];split;[apply bfa_of_bf;exact B|].
 intros s a final I Bound Rows Symbol Empty Below E;exact(M _ _ _ I Bound Rows E).
Qed.
Lemma lcp_emit:forall owner body,LCP owner(emit_fn owner body).
Proof.
 intros owner body;split;[apply bfa_emit|]. intros s a final I Bound Rows Symbol Empty Below E.
 unfold emit_fn,upd in E;injection E as <- <-;apply lambda_rows_emit_fresh;assumption.
Qed.
Lemma lcp_bind_before:forall A B owner(m:M A)(k:A->M B),
 LC m->LR m->(forall a,LCP owner(k a))->LCP owner(mbind m k).
Proof.
 intros A B owner m k[Bm M][Om Stable]K;split.
 - apply bfa_bind;[apply bfa_of_bf;exact Bm|intro;exact(proj1(K a))].
 - intros s value final I Bound Rows[g[Lookup Origin]]Empty Below E.
   apply bind_inv in E as[a[mid[E1 E2]]];destruct(Om _ _ _ I E1)as[Im Frame].
   apply(proj2(K a)mid value final Im(proj2 Bm _ _ _ I Bound E1)(M _ _ _ I Bound Rows E1)).
   + exists g;split;[exact(fr_keep _ _ Frame _ _ Lookup)|exact Origin].
   + rewrite(lambda_rows_stable_lookup s mid(Stable _ _ _ I E1)owner g Lookup Origin);exact Empty.
   + pose proof(fr_next _ _ Frame);lia.
   + exact E2.
Qed.
Lemma lcp_bind_after:forall A B owner(m:M A)(k:A->M B),
 LCP owner m->(forall a,LC(k a))->LCP owner(mbind m k).
Proof.
 intros A B owner m k[Bm M]K;split.
 - apply bfa_bind;[exact Bm|intro;apply bfa_of_bf;exact(proj1(K a))].
 - intros s value final I Bound Rows Symbol Empty Below E;apply bind_inv in E as[a[mid[E1 E2]]].
   destruct(proj1 Bm _ _ _ I E1)as[Im Frame].
   exact(proj2(K a)_ _ _ Im(proj2 Bm _ _ _ I Bound Below E1)(M _ _ _ I Bound Rows Symbol Empty Below E1)E2).
Qed.
Lemma lc_new_lambda_bind:forall A mi name kind decl(k:nat->M A),
 kind<>KTag/\kind<>KUnit->(forall id,LCP id(k id))->
 LC(mbind(new_sym mi name kind decl OLambda)k).
Proof.
 intros A mi name kind decl k Kind K;split.
 - apply bf_new_sym_bind;[discriminate|exact Kind|intro;exact(proj1(K id))].
 - intros s value final I Bound Rows E;apply bind_inv in E as[id[mid[New Rest]]].
   destruct(ok_new_sym mi name kind decl OLambda ltac:(discriminate)Kind _ _ _ I New)as[Im Frame].
   destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id[Next Symbol]].
   assert(Kept:st_fns mid=st_fns s).
   {assert(Knew:keeps st_fns(new_sym mi name kind decl OLambda))by(unfold new_sym;keepst).
    exact(Knew _ _ _ New). }
   apply(proj2(K id)mid value final Im).
   + eapply fn_bound_keeps;[exact Bound|exact Frame|exact Kept].
   + exact(lambda_rows_kept s mid Bound Rows Frame Kept).
   + exists(GSym id mi name kind decl None[]OLambda);split;[exact Symbol|reflexivity].
   + rewrite Kept;apply(fn_bound_fresh_absent Bound);lia.
   + lia.
   + exact Rest.
Qed.
Lemma lambda_rows_readonly_run:forall A(m:M A)s value final,
 ok m->keeps st_fns m->Inv s->FnBound s->LambdaRows s->m s=Good(value,final)->
 Inv final/\FnBound final/\LambdaRows final/\Fr s final.
Proof.
 intros A m s value final O K I Bound Rows E;destruct(O _ _ _ I E)as[Ifinal Frame].
 assert(Kept:st_fns final=st_fns s)by exact(K _ _ _ E).
 split;[exact Ifinal|]. split;[exact(fn_bound_keeps Bound Frame Kept)|].
 split;[exact(lambda_rows_kept s final Bound Rows Frame Kept)|exact Frame].
Qed.
Lemma lambda_rows_emit_other_run:forall s value final key body owner,
 Inv s->FnBound s->LambdaRows s->nget(st_syms s)key=Some owner->gs_origin owner<>OLambda->
 emit_fn key body s=Good(value,final)->
 Inv final/\FnBound final/\LambdaRows final/\Fr s final.
Proof.
 intros s value final key body owner I Bound Rows Symbol Origin Run.
 destruct(ok_emit_fn key body _ _ _ I Run)as[Ifinal Frame].
 assert(Below:key<st_next s)by exact(@symbol_below_next s key owner I Symbol).
 assert(Bfinal:FnBound final)by exact(proj2(bfa_emit key body)_ _ _ I Bound Below Run).
 split;[exact Ifinal|]. split;[exact Bfinal|]. split;[|exact Frame].
 unfold emit_fn,upd in Run;injection Run as <- <-;exact(lambda_rows_emit_other s key body owner Rows Symbol Origin).
Qed.
