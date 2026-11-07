(* Structural provenance for the cache of abstract-function forwarders. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore ElabInv FwdReadonly ForwarderTrace.
Import ListNotations.
Set Implicit Arguments.
Definition FwdInv(s:st):Prop:=forall target provider,nget(st_fwd s)target=Some provider ->
 exists g,nget(st_syms s)provider=Some g /\gs_origin g=OForwarder target.
Lemma fwd_inv_init:forall w,FwdInv(s_init w).
Proof. intros w target provider E;discriminate E. Qed.
Lemma fwd_inv_frame:forall s s',FwdInv s ->Fr s s' ->st_fwd s'=st_fwd s ->FwdInv s'.
Proof.
 intros s s' I F Eq target provider E;rewrite Eq in E.
 destruct(I _ _ E)as[g[Lookup Origin]]. exists g;split;[exact(fr_keep _ _ F _ _ Lookup)|exact Origin].
Qed.
Lemma fwd_inv_install:forall s target provider g,
 FwdInv s ->nget(st_syms s)provider=Some g ->gs_origin g=OForwarder target ->
 FwdInv(s_fwd(fun m=>nset m target provider)s).
Proof.
 intros s target provider g I Lookup Origin t p E;cbn[s_fwd st_fwd]in E;rewrite nget_nset in E.
 destruct(Nat.eqb t target)eqn:Eq.
 - apply Nat.eqb_eq in Eq;subst t;injection E as <-;exists g;split;assumption.
 - exact(I _ _ E).
Qed.
Definition okF{A}(m:M A):Prop:=ok m /\forall s a s',Inv s ->FwdInv s ->m s=Good(a,s')->FwdInv s'.
Arguments okF:simpl never.
Lemma okF_readonly:forall A(m:M A),ok m ->keeps st_fwd m ->okF m.
Proof.
 intros A m O K;split;[exact O|]. intros s a s' I FI E.
 eapply fwd_inv_frame;[exact FI|exact(proj2(O _ _ _ I E))|exact(K _ _ _ E)].
Qed.
Lemma okF_ret:forall A(a:A),okF(ret a).
Proof. intros;apply okF_readonly;[apply ok_ret|apply keeps_ret]. Qed.
Lemma okF_bind:forall A B(m:M A)(k:A->M B),okF m ->(forall a,okF(k a))->okF(mbind m k).
Proof.
 intros A B m k[O M]K;split;[apply ok_bind;[exact O|intro;exact(proj1(K a))]|].
 intros s value final I FI E;apply bind_inv in E as[a[mid[E1 E2]]].
 exact(proj2(K a)_ _ _(proj1(O _ _ _ I E1))(M _ _ _ I FI E1)E2).
Qed.
Lemma okF_mmap:forall A B(f:A->M B)xs,(forall x,okF(f x))->okF(mmap f xs).
Proof. intros A B f xs F;induction xs;cbn[mmap];[apply okF_ret|apply okF_bind;[apply F|intro;apply okF_bind;[exact IHxs|intro;apply okF_ret]]]. Qed.
Lemma okF_miter:forall A(f:A->M unit)xs,(forall x,okF(f x))->okF(miter f xs).
Proof. intros A f xs F;induction xs;cbn[miter];[apply okF_ret|apply okF_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma okF_mfold:forall A B(f:A->B->M A)xs acc,(forall a x,okF(f a x))->okF(mfold f acc xs).
Proof. intros A B f xs;induction xs;intros acc F;cbn[mfold];[apply okF_ret|apply okF_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma okF_with_vars:forall A init(m:M A),okF m ->okF(with_vars init m).
Proof.
 intros A init m[O K];split;[apply ok_with_vars;exact O|].
 intros s a final I FI E;unfold with_vars in E.
 apply bind_inv in E as[s0[s1[R0 E]]];injection R0 as <- <-.
 apply bind_inv in E as[u[s2[R1 E]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in E as[r[s3[Body E]]].
 apply bind_inv in E as[u'[s4[R2 E]]];unfold upd in R2;injection R2 as <- <-.
 injection E as <- <-.
 assert(IM:Inv(s_vars init(List.length init)s))by(destruct I;constructor;assumption).
 exact(K _ _ _ IM FI Body).
Qed.
