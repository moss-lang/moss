(* Positive generated-symbol lookups can only come from the initial state. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore ElabKeeps SymReadonly ProgramTail.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Definition generated_origin (o:origin) : Prop := o=OLambda \/ exists target,o=OForwarder target.
Definition GP{A}(m:M A):Prop:=forall s a final,
 m s=Good(a,final)->forall key g,nget(st_syms final)key=Some g->
 generated_origin(gs_origin g)->nget(st_syms s)key=Some g.
Arguments GP:simpl never.
Lemma gp_ret:forall A(a:A),GP(ret a).
Proof. intros A a s v final Run;injection Run as <- <-;auto. Qed.
Lemma gp_bind:forall A B(m:M A)(k:A->M B),GP m->(forall a,GP(k a))->GP(mbind m k).
Proof.
 intros A B m k M K s value final Run key g Lookup Origin.
 apply bind_inv in Run as[a[mid[First Last]]].
 exact(M _ _ _ First _ _ (K a _ _ _ Last _ _ Lookup Origin) Origin).
Qed.
Lemma gp_mmap:forall A B(f:A->M B)xs,(forall x,GP(f x))->GP(mmap f xs).
Proof. intros A B f xs F;induction xs;cbn[mmap];[apply gp_ret|apply gp_bind;[apply F|intro;apply gp_bind;[exact IHxs|intro;apply gp_ret]]]. Qed.
Lemma gp_mfold:forall A B(f:A->B->M A)xs acc,(forall a x,GP(f a x))->GP(mfold f acc xs).
Proof. intros A B f xs;induction xs;intros acc F;cbn[mfold];[apply gp_ret|apply gp_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma gp_miter:forall A(f:A->M unit)xs,(forall x,GP(f x))->GP(miter f xs).
Proof. intros A f xs F;induction xs;cbn[miter];[apply gp_ret|apply gp_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma gp_readonly:forall A(m:M A),keeps st_syms m->GP m.
Proof. intros A m K s a final Run key g Lookup Origin;rewrite(K _ _ _ Run)in Lookup;exact Lookup. Qed.
Lemma gp_new_sym:forall mi name kind decl orig,
 ~generated_origin orig->GP(new_sym mi name kind decl orig).
Proof.
 intros mi name kind decl orig Not s value final Run key g Lookup Origin.
 unfold new_sym in Run;apply bind_inv in Run as[id[s1[Fresh Run]]].
 unfold fresh in Fresh;injection Fresh as <- <-.
 apply bind_inv in Run as[u[s2[Update Run]]];unfold upd in Update;injection Update as <- <-.
 injection Run as <- <-;cbn[s_syms st_syms]in Lookup;rewrite nget_nset in Lookup.
 destruct(Nat.eqb key(st_next s)).
 - injection Lookup as <-;exact(False_rect _ (Not Origin)).
 - exact Lookup.
Qed.
Local Lemma ks_assumed_requirements:forall mi e k,keeps st_syms(check_assumed_requirements mi e k).
Proof. intros;unfold check_assumed_requirements;keepst. Qed.
#[local]Hint Resolve ks_assumed_requirements:keepsdb.
Local Lemma ks_assumes:keeps st_syms check_assumes.
Proof. unfold check_assumes;keepst. Qed.
Local Lemma ks_entry_zero:forall fuel e t,keeps st_syms(entry_zero_width fuel e t).
Proof. induction fuel;fintros;cbn[entry_zero_width];keepst. Qed.
#[local]Hint Resolve ks_entry_zero:keepsdb.
Local Lemma ks_main_signature:forall g info,keeps st_syms(check_main_signature g info).
Proof. intros;unfold check_main_signature;keepst. Qed.
#[local]Hint Resolve ks_assumes ks_main_signature:keepsdb.
Local Ltac gpt:=repeat(fintros;lazymatch goal with
| |-GP(ret _)=>apply gp_ret
| |-GP(mbind _ _)=>apply gp_bind
| |-GP(mmap _ _)=>apply gp_mmap
| |-GP(mfold _ _ _)=>apply gp_mfold
| |-GP(miter _ _)=>apply gp_miter
| |-GP(new_sym _ _ _ _ _)=>apply gp_new_sym;intros[Bad|[target Bad]];discriminate
| |-GP ?m=>first[solve[apply gp_readonly;auto with keepsdb]|lazymatch m with
  |upd _=>apply gp_readonly;solve[keepst]
  |(fun s=>_)=>apply gp_readonly;solve[keepst]
  |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|let h:=keeps_head m in progress unfold h]
  end]
end).
Lemma program_tail_generated:GP program_tail.
Proof. unfold program_tail;gpt. Qed.
