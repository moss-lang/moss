(* Allocation bounds for function rows, established from the real empty table. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv FnReadonly ForwarderTrace.
Import ListNotations.
Set Implicit Arguments.
Definition FnBound(s:st):Prop:=forall key body,In(key,body)(st_fns s)->key<st_next s.
Lemma fn_bound_init:forall w,FnBound(s_init w).
Proof. intros w key body E;contradiction. Qed.
Lemma fn_bound_fresh_absent:forall s key,FnBound s ->st_next s<=key ->nassoc key(st_fns s)=None.
Proof.
 intros s key Bound Above;destruct(nassoc key(st_fns s))as[body|]eqn:E;[|reflexivity].
 assert(In(key,body)(st_fns s)).
 {clear Bound Above;unfold nassoc in E;induction(st_fns s)as[|[k v]xs IH];cbn[assoc]in E;[discriminate|].
  destruct(Nat.eqb key k)eqn:Eq;[apply Nat.eqb_eq in Eq;injection E as <-;subst;left;reflexivity|right;apply IH;exact E]. }
 pose proof(Bound _ _ H);lia.
Qed.
Lemma fn_bound_keeps:forall s s',FnBound s ->Fr s s' ->st_fns s'=st_fns s ->FnBound s'.
Proof. intros s s' B F Eq key body InRow;rewrite Eq in InRow;pose proof(B _ _ InRow);pose proof(fr_next _ _ F);lia. Qed.
Definition BF{A}(m:M A):Prop:=ok m /\forall s a s',Inv s ->FnBound s ->m s=Good(a,s')->FnBound s'.
Definition BF_at(key:nat){A}(m:M A):Prop:=ok m /\forall s a s',Inv s ->FnBound s ->key<st_next s ->m s=Good(a,s')->FnBound s'.
Arguments BF:simpl never.
Arguments BF_at:simpl never.
Lemma bf_keeps:forall A(m:M A),ok m ->keeps st_fns m ->BF m.
Proof. intros A m O K;split;[exact O|]. intros s a s' I B E;eapply fn_bound_keeps;[exact B|exact(proj2(O _ _ _ I E))|exact(K _ _ _ E)]. Qed.
Lemma bf_ret:forall A(a:A),BF(ret a).
Proof. intros;apply bf_keeps;[apply ok_ret|apply keeps_ret]. Qed.
Lemma bf_bind:forall A B(m:M A)(k:A->M B),BF m ->(forall a,BF(k a))->BF(mbind m k).
Proof.
 intros A B m k[O M]K;split;[apply ok_bind;[exact O|intro;exact(proj1(K a))]|].
 intros s value final I Bnd E;apply bind_inv in E as[a[mid[E1 E2]]].
 exact(proj2(K a)_ _ _(proj1(O _ _ _ I E1))(M _ _ _ I Bnd E1)E2).
Qed.
Lemma bf_mmap:forall A B(f:A->M B)xs,(forall x,BF(f x))->BF(mmap f xs).
Proof. intros A B f xs F;induction xs;cbn[mmap];[apply bf_ret|apply bf_bind;[apply F|intro;apply bf_bind;[exact IHxs|intro;apply bf_ret]]]. Qed.
Lemma bf_miter:forall A(f:A->M unit)xs,(forall x,BF(f x))->BF(miter f xs).
Proof. intros A f xs F;induction xs;cbn[miter];[apply bf_ret|apply bf_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma bf_mfold:forall A B(f:A->B->M A)xs acc,(forall a x,BF(f a x))->BF(mfold f acc xs).
Proof. intros A B f xs;induction xs;intros acc F;cbn[mfold];[apply bf_ret|apply bf_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma bfa_of_bf:forall A key(m:M A),BF m ->BF_at key m.
Proof. intros A key m[O B];split;[exact O|]. intros s a s' I Bnd K E;exact(B _ _ _ I Bnd E). Qed.
Lemma bfa_emit:forall key body,BF_at key(emit_fn key body).
Proof.
 intros key body;split;[apply ok_emit_fn|]. intros s a s' I Bnd K E.
 unfold emit_fn,upd in E;injection E as <- <-;intros k v InRow.
 cbn[s_fns st_fns]in InRow;destruct InRow as[Eq|InRow];[injection Eq as <- <-;exact K|exact(Bnd _ _ InRow)].
Qed.
Lemma bfa_bind:forall A B key(m:M A)(k:A->M B),BF_at key m ->(forall a,BF_at key(k a))->BF_at key(mbind m k).
Proof.
 intros A B key m k[O M]K;split;[apply ok_bind;[exact O|intro;exact(proj1(K a))]|].
 intros s value final I Bnd Bound E;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(O _ _ _ I E1)as[IM F]. apply(proj2(K a)mid value final IM(M _ _ _ I Bnd Bound E1));[pose proof(fr_next _ _ F);lia|exact E2].
Qed.
Lemma bfa_mmap:forall A B key(f:A->M B)xs,(forall x,BF_at key(f x))->BF_at key(mmap f xs).
Proof. intros A B key f xs F;induction xs;cbn[mmap];[apply bfa_of_bf,bf_ret|apply bfa_bind;[apply F|intro;apply bfa_bind;[exact IHxs|intro;apply bfa_of_bf,bf_ret]]]. Qed.
Lemma bfa_miter:forall A key(f:A->M unit)xs,(forall x,BF_at key(f x))->BF_at key(miter f xs).
Proof. intros A key f xs F;induction xs;cbn[miter];[apply bfa_of_bf,bf_ret|apply bfa_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma bfa_mfold:forall A B key(f:A->B->M A)xs acc,(forall a x,BF_at key(f a x))->BF_at key(mfold f acc xs).
Proof. intros A B key f xs;induction xs;intros acc F;cbn[mfold];[apply bfa_of_bf,bf_ret|apply bfa_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma bf_new_sym_bind:forall A mi name kind decl orig(k:nat->M A),
 orig<>OSource ->kind<>KTag /\kind<>KUnit ->(forall id,BF_at id(k id))->BF(mbind(new_sym mi name kind decl orig)k).
Proof.
 intros A mi name kind decl orig k Origin Kind K;split;[apply ok_bind;[apply ok_new_sym;assumption|intro;exact(proj1(K a))]|].
 intros s value final I Bnd E;apply bind_inv in E as[id[mid[New Rest]]].
 destruct(ok_new_sym mi name kind decl orig Origin Kind _ _ _ I New)as[IM F].
 destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id[Next Lookup]].
 assert(BM:FnBound mid).
 {eapply fn_bound_keeps;[exact Bnd|exact F|]. assert(Kf:keeps st_fns(new_sym mi name kind decl orig))by(unfold new_sym;keepst). exact(Kf _ _ _ New). }
 apply(proj2(K id)mid value final IM BM);[rewrite Next;lia|exact Rest].
Qed.
Lemma bfa_bind_sym:forall A owner id(k:gsym->M A),
 (forall g,gs_id g=id ->BF_at owner(k g))->BF_at owner(mbind(sym id)k).
Proof.
 intros A owner id k K;split.
 - intros s value final I E;apply bind_inv in E as[g[mid[Read Rest]]].
   unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate]. injection Read as <- <-.
   exact(proj1(K got(iv_ids s I id got Lookup))_ _ _ I Rest).
 - intros s value final I Bnd Bound E;apply bind_inv in E as[g[mid[Read Rest]]].
   unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate]. injection Read as <- <-.
   exact(proj2(K got(iv_ids s I id got Lookup))_ _ _ I Bnd Bound Rest).
Qed.
