(* Link cache preservation with persistent physical input symbols. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabKeeps
 InfoInvariant InfoPreserve FwdInvariant FwdSynthInv LinkCacheInvariant ProviderPhysical.
Import ListNotations.
Definition LinkStable(p:st->Prop):Prop:=forall s final,Fr s final->p s->p final.
Definition okLP {A}(p:st->Prop)(m:M A):Prop:=
 okI m /\ okF m /\ forall s a final,
 Inv s ->InfoInv s ->FwdInv s ->LinkInv s ->p s ->m s=Good(a,final)->LinkInv final.
Arguments okLP:simpl never.
Lemma okLP_of_okL : forall A p(m:M A),okL m ->okLP p m.
Proof. intros A p m[Info[Forward Linked]];split;[exact Info|];split;[exact Forward|].
 intros;eapply Linked;eassumption. Qed.
Lemma okLP_bind : forall A B p(m:M A)(k:A->M B),
 LinkStable p ->okLP p m ->(forall a,okLP p(k a))->okLP p(mbind m k).
Proof.
 intros A B p m k Stable[Info[Forward Linked]] K;split.
 - apply okI_bind;[exact Info|intro;exact(proj1(K a))].
 - split;[apply okF_bind;[exact Forward|intro;exact(proj1(proj2(K a)))]|].
   intros s b final Valid Cached Forwarded Links Pre Run.
   apply bind_inv in Run as[a[mid[First Last]]].
   destruct(proj1 Info _ _ _ Valid First)as[VM Frame].
   exact(proj2(proj2(K a))mid b final VM(proj2 Info _ _ _ Valid Cached First)
    (proj2 Forward _ _ _ Valid Forwarded First)
    (Linked _ _ _ Valid Cached Forwarded Links Pre First)(Stable _ _ Frame Pre)Last).
Qed.
Lemma okLP_mmap : forall A B p(f:A->M B)xs,LinkStable p ->
 (forall x,okLP p(f x))->okLP p(mmap f xs).
Proof.
 intros A B p f xs Stable Each;induction xs as[|x xs IH];cbn[mmap].
 - apply okLP_of_okL;apply okL_ret.
 - apply okLP_bind;[exact Stable|apply Each|intro;apply okLP_bind;
   [exact Stable|exact IH|intro;apply okLP_of_okL;apply okL_ret]].
Qed.
Lemma okLP_miter : forall A p(f:A->M unit)xs,LinkStable p ->
 (forall x,okLP p(f x))->okLP p(miter f xs).
Proof.
 intros A p f xs Stable Each;induction xs as[|x xs IH];cbn[miter].
 - apply okLP_of_okL;apply okL_ret.
 - apply okLP_bind;[exact Stable|apply Each|intro;exact IH].
Qed.
Lemma okLP_mfold : forall A B p(f:A->B->M A)xs acc,LinkStable p ->
 (forall a x,okLP p(f a x))->okLP p(mfold f acc xs).
Proof.
 intros A B p f xs;induction xs as[|x xs IH];intros acc Stable Each;cbn[mfold].
 - apply okLP_of_okL;apply okL_ret.
 - apply okLP_bind;[exact Stable|apply Each|intro;apply IH;assumption].
Qed.
Definition LinkPhysical(fg bg:gsym)(s:st):Prop:=PhysicalProvider s fg /\PhysicalProvider s bg.
Lemma link_physical_stable : forall fg bg,LinkStable(LinkPhysical fg bg).
Proof.
 intros fg bg s final Frame[Target Provider];split;unfold PhysicalProvider in *.
 - exact(fr_keep s final Frame _ _ Target).
 - exact(fr_keep s final Frame _ _ Provider).
Qed.
Definition LinkBodyPreserves(fuel:nat)(fg bg:gsym):Prop:=
 forall s h final,Inv s ->InfoInv s ->FwdInv s ->LinkInv s ->
 PhysicalProvider s fg ->PhysicalProvider s bg ->
 link_body fuel fg bg s=Good(h,final)->LinkInv final.
Lemma okLP_link_body : forall fuel fg bg,LinkBodyPreserves fuel fg bg ->
 okLP(LinkPhysical fg bg)(link_body fuel fg bg).
Proof.
 intros fuel fg bg Linked;split;[apply okI_link_body|];split;[apply okF_link_body|].
 intros s h final Valid Cached Forwarded Links[Target Provider]Run.
 exact(Linked _ _ _ Valid Cached Forwarded Links Target Provider Run).
Qed.
