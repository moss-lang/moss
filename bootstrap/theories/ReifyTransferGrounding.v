(* Complete maps construct callee type meanings from actual caller slots;
   existence of a callee grounding is not an opaque contract assumption. *)
From Stdlib Require Import List Arith Bool.
From NM Require Import CoreMoss Specialization ReifyMaps.
Import ListNotations.
Definition ground_type_transfer(g:subst)(bindings:list(tsym*tsym)):subst:=
 map(fun p=>(fst p,app_subst g(TAbs(snd p))))bindings.
Definition type_transfer_items(bindings:list(tsym*tsym)):smap:=
 map(fun p=>(ITy(fst p),ITy(snd p)))bindings.
Lemma ground_type_transfer_domain : forall g bindings,
 map fst(ground_type_transfer g bindings)=map fst bindings.
Proof. intros;unfold ground_type_transfer;rewrite map_map;reflexivity. Qed.
Lemma ground_type_transfer_lookup : forall bindings g formal actual,
 NoDup(map fst bindings)->In(formal,actual)bindings->
 lookup_subst formal(ground_type_transfer g bindings)=Some(app_subst g(TAbs actual)).
Proof.
 induction bindings as[|[first value]rest IH];intros g formal actual Unique Member;[contradiction|].
 inversion Unique as[|head tail Absent Rest];subst;cbn[ground_type_transfer map lookup_subst fst snd].
 destruct Member as[Same|Member].
 - inversion Same;subst;rewrite Nat.eqb_refl;reflexivity.
 - destruct(Nat.eqb formal first)eqn:Equal.
   + apply Nat.eqb_eq in Equal;subst formal;exfalso;apply Absent.
     apply in_map_iff;exists(first,actual);auto.
   + eapply IH;eassumption.
Qed.
Lemma covered_type_is_closed : forall g Phi t,
 ctx_grounded g Phi->In t(tyreqs(cA Phi))->closed_ty(app_subst g(TAbs t)).
Proof.
 intros g Phi t[Ground[Absorb[Covered Rest]]]Member.
 destruct(Covered _ Member)as[tau Read];cbn;rewrite Read.
 eapply grounding_lookup_closed;eauto.
Qed.
Lemma actual_transfer_is_grounding : forall g Phi bindings,
 ctx_grounded g Phi->Forall(fun p=>In(snd p)(tyreqs(cA Phi)))bindings->
 grounding(ground_type_transfer g bindings).
Proof.
 intros g Phi bindings Ground Available;unfold grounding,ground_type_transfer.
 apply Forall_map;eapply Forall_impl;[|exact Available].
 intros[formal actual]Member;cbn;eapply covered_type_is_closed;eauto.
Qed.
Theorem actual_transfer_context_grounded : forall g Phi D bindings,
 ctx_grounded g Phi->NoDup(map fst bindings)->tyreqs D=map fst bindings->
 ~In this_sym(map fst bindings)->
 Forall(fun p=>In(snd p)(tyreqs(cA Phi)))bindings->
 ctx_grounded(ground_type_transfer g bindings)(mkCtx D[])/\
 map_type_agreement(ground_type_transfer g bindings)g(type_transfer_items bindings).
Proof.
 intros g Phi D bindings Ground Unique Domain Nonzero Available;split.
 - split;[eapply actual_transfer_is_grounding;eauto|].
   split;[intros t tau Impossible;discriminate|].
   split.
   + intros t Member;change(In t(tyreqs D))in Member;rewrite Domain in Member.
     apply in_map_iff in Member as[[formal actual][Same Present]];cbn in Same;subst formal.
     exists(app_subst g(TAbs actual));eapply ground_type_transfer_lookup;eauto.
   + split.
     * split;[intros t Impossible;contradiction|constructor].
     * change(~In this_sym(tyreqs D));rewrite Domain;exact Nonzero.
 - intros formal actual Member;unfold type_transfer_items in Member.
   apply in_map_iff in Member as[[f a][Same Present]].
   injection Same as <- <-;cbn;rewrite(ground_type_transfer_lookup _ _ _ _ Unique Present);reflexivity.
Qed.
Lemma actual_transfer_type_interpretation : forall bindings g tau,
 NoDup(map fst bindings)->incl(ty_frees tau)(map fst bindings)->
 app_subst(ground_type_transfer g bindings)tau=
 app_subst g(app_subst(ground_type_transfer [] bindings)tau).
Proof.
 intros bindings g tau Unique Covered;rewrite app_subst_compose.
 apply dentry_agree_retype_aux1;intros t Member.
 apply Covered in Member;apply in_map_iff in Member as[[formal actual][Same Present]].
 cbn in Same;subst formal;rewrite <-app_subst_compose;cbn.
 rewrite(ground_type_transfer_lookup _ _ _ _ Unique Present),
   (ground_type_transfer_lookup bindings [] t actual Unique Present);reflexivity.
Qed.
Corollary actual_transfer_signature_interpretation : forall bindings g signature,
 NoDup(map fst bindings)->incl(msig_frees signature)(map fst bindings)->
 grounded_call_signature(ground_type_transfer g bindings)signature=
 grounded_call_signature g(mkSig
   (map(app_subst(ground_type_transfer [] bindings))(fs_params signature))
   (app_subst(ground_type_transfer [] bindings)(fs_ret signature))).
Proof.
 intros bindings g signature Unique Covered;unfold grounded_call_signature;cbn;f_equal.
 - rewrite map_map;apply map_ext_in;intros tau Member.
   apply actual_transfer_type_interpretation;[exact Unique|].
   intros t Free;apply Covered;unfold msig_frees;apply in_flat_map;exists tau;split;
     [apply in_or_app;left;exact Member|exact Free].
 - apply actual_transfer_type_interpretation;[exact Unique|].
   intros t Free;apply Covered;unfold msig_frees;apply in_flat_map;exists(fs_ret signature);split;
     [apply in_or_app;right;left;reflexivity|exact Free].
Qed.
