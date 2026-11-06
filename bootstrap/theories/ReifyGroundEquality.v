(* Checked immutable contexts are systems of actual type equations. Their
   meaning does not require the written equation list to be idempotent. *)
From Stdlib Require Import List Arith.
From NM Require Import CoreMoss Specialization.
Import ListNotations.
Definition grounded_type_equal(Phi:ctx)(a b:ty):Prop:=
 forall g,ctx_grounded g Phi->app_subst g a=app_subst g b.
Lemma grounded_type_equal_refl : forall Phi tau,grounded_type_equal Phi tau tau.
Proof. intros Phi tau g H;reflexivity. Qed.
Lemma grounded_type_equal_sym : forall Phi a b,
 grounded_type_equal Phi a b->grounded_type_equal Phi b a.
Proof. intros Phi a b H g Ground;symmetry;apply H;exact Ground. Qed.
Lemma grounded_type_equal_trans : forall Phi a b c,
 grounded_type_equal Phi a b->grounded_type_equal Phi b c->grounded_type_equal Phi a c.
Proof. intros Phi a b c A B g Ground;rewrite(A g Ground);apply B;exact Ground. Qed.
Lemma context_substitution_ground_equal : forall Phi tau,
 grounded_type_equal Phi(app_subst(cS Phi)tau)tau.
Proof. intros Phi tau g[_[Absorb _]];apply absorbs_all;exact Absorb. Qed.
Lemma context_pin_ground_equal : forall Phi t tau,
 lookup_subst t(cS Phi)=Some tau->grounded_type_equal Phi(TAbs t)tau.
Proof.
 intros Phi t tau Pin g[_[Absorb _]].
 specialize(Absorb _ _ Pin);cbn;rewrite Absorb;reflexivity.
Qed.
Fixpoint context_type_read(n:nat)(Phi:ctx)(tau:ty):ty:=
 match n with 0=>tau|S k=>app_subst(cS Phi)(context_type_read k Phi tau)end.
Lemma context_type_reads_ground_equal : forall n Phi tau,
 grounded_type_equal Phi(context_type_read n Phi tau)tau.
Proof.
 induction n as[|n IH];intros Phi tau;[apply grounded_type_equal_refl|].
 cbn;eapply grounded_type_equal_trans;[apply context_substitution_ground_equal|apply IH].
Qed.
Lemma context_type_reads_equivalent : forall n m Phi tau,
 grounded_type_equal Phi(context_type_read n Phi tau)(context_type_read m Phi tau).
Proof.
 intros n m Phi tau;eapply grounded_type_equal_trans;
   [apply context_type_reads_ground_equal|apply grounded_type_equal_sym,context_type_reads_ground_equal].
Qed.
Lemma grounded_substitution_images : forall Phi first second tau,
 (forall t,In t(ty_frees tau)->grounded_type_equal Phi(app_subst first(TAbs t))(app_subst second(TAbs t)))->
 grounded_type_equal Phi(app_subst first tau)(app_subst second tau).
Proof.
 intros Phi first second tau Atoms g Ground;rewrite !app_subst_compose.
 apply dentry_agree_retype_aux1;intros t Member.
 rewrite <-!app_subst_compose;apply Atoms;[exact Member|exact Ground].
Qed.

Definition pin_chain_context:ctx:=mkCtx[ITy 114;ITy 115;ITy 116]
 [(114,TAbs 115);(115,TAbs 116);(116,TNom 10[])].
Example written_pins_need_not_be_idempotent :
 app_subst(cS pin_chain_context)(app_subst(cS pin_chain_context)(TAbs 114))<>
 app_subst(cS pin_chain_context)(TAbs 114).
Proof. discriminate. Qed.
Example actual_pin_chain_has_one_ground_meaning :
 grounded_type_equal pin_chain_context(TAbs 114)(TNom 10[]).
Proof.
 apply grounded_type_equal_sym.
 change(grounded_type_equal pin_chain_context(context_type_read 3 pin_chain_context(TAbs 114))(TAbs 114)).
 apply context_type_reads_ground_equal.
Qed.
