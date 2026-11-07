(* Headers of emitted owners survive even a later top-level lowering pass. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame ElabInv
 FnReadonly FnPresentStable FnPresentEmit FnPresentSynth LowerOne
 ReifyHeader ReifyHeaderReadonly ReifyHeaderStable ReifyHeaderSynth.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Definition HP{A}(m:M A):Prop:=PF m/\forall s value final,
 Inv s->m s=Good(value,final)->forall key body,
 key<st_next s->nassoc key(st_fns s)=Some body->
 function_header final key=function_header s key.
Arguments HP:simpl never.
Lemma hp_of_hs:forall A(m:M A),HS m->HP m.
Proof.
 intros A m[PFm Keep];split;[exact PFm|].
 intros s value final I Run key body Bound Present;apply(Keep _ _ _ I Run key Bound).
 left;exists body;exact Present.
Qed.
Lemma hp_bind:forall A B(m:M A)(k:A->M B),HP m->(forall a,HP(k a))->HP(mbind m k).
Proof.
 intros A B m k[PFm Keep]K;split;[apply pf_bind;[exact PFm|intro;exact(proj1(K a))]|].
 intros s value final I Run key body Bound Present;apply bind_inv in Run as[a[mid[First Last]]].
 destruct(proj1 PFm _ _ _ I First)as[Im Frame].
 rewrite(proj2(K a)_ _ _ Im Last key body ltac:(pose proof(fr_next _ _ Frame);lia)
  (proj2 PFm _ _ _ I First key body Bound Present)).
 exact(Keep _ _ _ I First key body Bound Present).
Qed.
Lemma hp_miter:forall A(f:A->M unit)xs,(forall x,HP(f x))->HP(miter f xs).
Proof. intros A f xs K;induction xs;cbn[miter];[apply hp_of_hs,hs_ret|].
 apply hp_bind;[apply K|intro;exact IHxs]. Qed.
Lemma hp_lower_body:forall fuel g,HP(lower_body fuel g).
Proof.
 intros[|fuel]g;[apply hp_of_hs;apply hs_keeps;[apply pf_lower_body|unfold lower_body;intros s value final Run;discriminate]|].
 split;[apply pf_lower_body|]. intros s value final I Run key body Bound Present.
 destruct(Nat.eq_dec key(gs_id g))as[Same|Other].
 - subst key;cbn[lower_body]in Run;apply bind_inv in Run as[done[mid[Read Run]]].
   cbn[lowered mbind get ret]in Read;rewrite Present in Read;cbn[is_some]in Read;injection Read as <- <-.
   injection Run as <- <-;reflexivity.
 - apply(proj2(hse_lower_body(S fuel)g)_ _ _ I Run key Other Bound).
   left;exists body;exact Present.
Qed.
Lemma hp_lower_one:forall fuel id,HP(lower_one fuel id).
Proof.
 intros fuel id;unfold lower_one;apply hp_bind.
 - apply hp_of_hs,hs_keeps.
   + apply pf_keeps;[okt|keepst].
   + apply header_keeps_of_rows;keepst.
 - intro g;destruct(is_some(fn_body g)&&(kind_eqb(gs_kind g)KFn||kind_eqb(gs_kind g)KMethod));
   [apply hp_lower_body|apply hp_of_hs,hs_ret].
Qed.
Lemma hp_lower_all:HP lower_all.
Proof.
 unfold lower_all;apply hp_bind.
 - apply hp_of_hs,hs_keeps.
   + apply pf_keeps;[okt|keepst].
   + apply header_keeps_of_rows;keepst.
 - intro s;apply hp_miter;intro mi;apply hp_miter;intro id;apply hp_lower_one.
Qed.
