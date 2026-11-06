(* Structural input matching only extends immutable generation solutions.
   Existing frozen meanings cannot be overwritten by later argument rows. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import CoreMoss Reify ReifyTransfers.
Import ListNotations.
Definition solution_extends (before after:subst):Prop:=
 forall t tau,lookup_subst t before=Some tau->lookup_subst t after=Some tau.
Lemma solution_extends_refl : forall s,solution_extends s s.
Proof. intros s t tau H;exact H. Qed.
Lemma solution_extends_trans : forall a b c,
 solution_extends a b->solution_extends b c->solution_extends a c.
Proof. intros a b c A B t tau H;apply B,A,H. Qed.
Lemma add_generation_solution_extends : forall t tau before after,
 add_generation_solution t tau before=Some after->solution_extends before after.
Proof.
 intros t tau before after H;unfold add_generation_solution in H.
 destruct(lookup_subst t before)as[previous|]eqn:E.
 - destruct(ty_eqb previous tau);[|discriminate]. inversion H;apply solution_extends_refl.
 - inversion H;subst;intros key value Old;cbn.
   destruct(Nat.eqb key t)eqn:K;[|exact Old].
   apply Nat.eqb_eq in K;subst;rewrite E in Old;discriminate.
Qed.

Lemma match_generation_pattern_extends : forall pattern variables actual before after,
 match_generation_pattern variables pattern actual before=Some after->solution_extends before after.
Proof.
 induction pattern as[|t|n ps IH|ps IH]using ty_ind';intros variables actual before after E.
 - destruct actual;cbn[match_generation_pattern]in E;try discriminate.
   inversion E;apply solution_extends_refl.
 - cbn[match_generation_pattern]in E.
   destruct(existsb(Nat.eqb t)variables).
   + eapply add_generation_solution_extends;exact E.
   + destruct(ty_eqb(TAbs t)actual);[|discriminate]. inversion E;apply solution_extends_refl.
 - destruct actual as[|u|m xs|xs];cbn[match_generation_pattern]in E;try discriminate.
   destruct(Nat.eqb n m);[|discriminate].
   revert xs before after E;induction IH as[|[key p]rest Head Tail Inner];
     intros xs before after E;destruct xs as[|[actual_key a]xs];cbn in E;try discriminate.
   + inversion E;apply solution_extends_refl.
   + destruct(Nat.eqb key actual_key);[|discriminate].
     destruct(match_generation_pattern variables p a before)as[middle|]eqn:M;[|discriminate].
     eapply solution_extends_trans;[eapply Head;exact M|eapply Inner;exact E].
 - destruct actual as[|u|m xs|xs];cbn[match_generation_pattern]in E;try discriminate.
   revert xs before after E;induction IH as[|p rest Head Tail Inner];
     intros xs before after E;destruct xs as[|a xs];cbn in E;try discriminate.
   + inversion E;apply solution_extends_refl.
   + destruct(match_generation_pattern variables p a before)as[middle|]eqn:M;[|discriminate].
     eapply solution_extends_trans;[eapply Head;exact M|eapply Inner;exact E].
Qed.

Definition solution_keys_within (variables:list tsym)(solutions:subst):Prop:=
 forall t tau,lookup_subst t solutions=Some tau->In t variables.
Lemma add_generation_solution_keys : forall variables t tau before after,
 In t variables->solution_keys_within variables before->
 add_generation_solution t tau before=Some after->solution_keys_within variables after.
Proof.
 intros variables t tau before after Member Before E.
 unfold add_generation_solution in E;destruct(lookup_subst t before)as[previous|].
 - destruct(ty_eqb previous tau);[|discriminate]. inversion E;subst;exact Before.
 - inversion E;subst;intros key value H;cbn in H.
   destruct(Nat.eqb key t)eqn:K.
   + apply Nat.eqb_eq in K;subst;exact Member.
   + eapply Before;exact H.
Qed.

Lemma match_generation_pattern_invariant : forall pattern variables (I:subst->Prop),
 (forall t tau before after,In t variables->I before->
   add_generation_solution t tau before=Some after->I after)->
 forall actual before after,I before->
 match_generation_pattern variables pattern actual before=Some after->I after.
Proof.
 induction pattern as[|t|n ps IH|ps IH]using ty_ind';
   intros variables I Add actual before after Initial E.
 - destruct actual;cbn[match_generation_pattern]in E;try discriminate.
   inversion E;subst;exact Initial.
 - cbn[match_generation_pattern]in E.
   destruct(existsb(Nat.eqb t)variables)eqn:Member.
   + apply existsb_exists in Member as[key[Hin Eq]]. apply Nat.eqb_eq in Eq;subst.
     eapply Add;[exact Hin|exact Initial|exact E].
   + destruct(ty_eqb(TAbs t)actual);[|discriminate]. inversion E;subst;exact Initial.
 - destruct actual as[|u|m xs|xs];cbn[match_generation_pattern]in E;try discriminate.
   destruct(Nat.eqb n m);[|discriminate].
   revert xs before after Initial E;induction IH as[|[key p]rest Head Tail Inner];
     intros xs before after Initial E;destruct xs as[|[actual_key a]xs];cbn in E;try discriminate.
   + inversion E;subst;exact Initial.
   + destruct(Nat.eqb key actual_key);[|discriminate].
     destruct(match_generation_pattern variables p a before)as[middle|]eqn:M;[|discriminate].
     eapply Inner;[eapply Head;[exact Add|exact Initial|exact M]|exact E].
 - destruct actual as[|u|m xs|xs];cbn[match_generation_pattern]in E;try discriminate.
   revert xs before after Initial E;induction IH as[|p rest Head Tail Inner];
     intros xs before after Initial E;destruct xs as[|a xs];cbn in E;try discriminate.
   + inversion E;subst;exact Initial.
   + destruct(match_generation_pattern variables p a before)as[middle|]eqn:M;[|discriminate].
     eapply Inner;[eapply Head;[exact Add|exact Initial|exact M]|exact E].
Qed.

Lemma match_generation_pattern_keys : forall pattern variables actual before after,
 solution_keys_within variables before->
 match_generation_pattern variables pattern actual before=Some after->solution_keys_within variables after.
Proof.
 intros pattern variables actual before after Initial E.
 eapply match_generation_pattern_invariant;[|exact Initial|exact E].
 intros;eapply add_generation_solution_keys;eassumption.
Qed.

Lemma generation_argument_solutions_extend : forall patterns variables arguments runtime before after,
 solve_generation_arguments variables patterns arguments runtime before=Some after->
 solution_extends before after.
Proof.
 induction patterns as[|[name pattern]rest IH];intros variables arguments runtime before after E;cbn in E.
 - inversion E;apply solution_extends_refl.
 - destruct(lookup_subst name arguments)as[raw|].
   + destruct(match_generation_pattern variables pattern(app_subst(rf_generations runtime)raw)before)
       as[middle|]eqn:M;[|discriminate].
     eapply solution_extends_trans;[eapply match_generation_pattern_extends;exact M|eapply IH;exact E].
   + eapply IH;exact E.
Qed.
Lemma generation_argument_solution_keys : forall patterns variables arguments runtime before after,
 solution_keys_within variables before->
 solve_generation_arguments variables patterns arguments runtime before=Some after->
 solution_keys_within variables after.
Proof.
 induction patterns as[|[name pattern]rest IH];intros variables arguments runtime before after Initial E;cbn in E.
 - inversion E;subst;exact Initial.
 - destruct(lookup_subst name arguments)as[raw|].
   + destruct(match_generation_pattern variables pattern(app_subst(rf_generations runtime)raw)before)
       as[middle|]eqn:M;[|discriminate].
     eapply IH;[eapply match_generation_pattern_keys;[exact Initial|exact M]|exact E].
   + eapply IH;[exact Initial|exact E].
Qed.

Lemma added_generation_has_meaning : forall t tau before after,
 add_generation_solution t tau before=Some after->lookup_subst t after=Some tau.
Proof.
 intros t tau before after E;unfold add_generation_solution in E.
 destruct(lookup_subst t before)as[previous|]eqn:Old.
 - destruct(ty_eqb previous tau)eqn:Eq;[|discriminate].
   apply ty_eqb_eq in Eq;subst;inversion E;subst;exact Old.
 - inversion E;subst;cbn;rewrite Nat.eqb_refl;reflexivity.
Qed.

Lemma matched_variables_have_meanings : forall pattern variables actual before after,
 match_generation_pattern variables pattern actual before=Some after->
 forall t,In t variables->In t(ty_frees pattern)->exists tau,lookup_subst t after=Some tau.
Proof.
 induction pattern as[|key|n ps IH|ps IH]using ty_ind';
   intros variables actual before after E t InVars Occurs.
 - contradiction.
 - cbn in Occurs;destruct Occurs as[<-|[]]. cbn[match_generation_pattern]in E.
   assert(Member:existsb(Nat.eqb key)variables=true).
   { apply existsb_exists;exists key;split;[exact InVars|apply Nat.eqb_refl]. }
   rewrite Member in E;exists actual;eapply added_generation_has_meaning;exact E.
 - destruct actual as[|u|m xs|xs];cbn[match_generation_pattern]in E;try discriminate.
   destruct(Nat.eqb n m);[|discriminate].
   revert xs before after E Occurs;induction IH as[|[key p]rest Head Tail Inner];
     intros xs before after E Occurs;destruct xs as[|[actual_key a]xs];cbn in E;try discriminate.
   + contradiction.
   + destruct(Nat.eqb key actual_key);[|discriminate].
     destruct(match_generation_pattern variables p a before)as[middle|]eqn:M;[|discriminate].
     cbn in Occurs;apply in_app_or in Occurs as[First|Rest].
     * destruct(Head variables a before middle M t InVars First)as[tau Meaning].
       exists tau;apply(match_generation_pattern_extends(TNom 0 rest)variables(TNom 0 xs)middle after).
       -- cbn[match_generation_pattern];exact E.
       -- exact Meaning.
     * eapply Inner;[exact E|exact Rest].
 - destruct actual as[|u|m xs|xs];cbn[match_generation_pattern]in E;try discriminate.
   revert xs before after E Occurs;induction IH as[|p rest Head Tail Inner];
     intros xs before after E Occurs;destruct xs as[|a xs];cbn in E;try discriminate.
   + contradiction.
   + destruct(match_generation_pattern variables p a before)as[middle|]eqn:M;[|discriminate].
     cbn in Occurs;apply in_app_or in Occurs as[First|Rest].
     * destruct(Head variables a before middle M t InVars First)as[tau Meaning].
       exists tau;apply(match_generation_pattern_extends(TUnion rest)variables(TUnion xs)middle after).
       -- cbn[match_generation_pattern];exact E.
       -- exact Meaning.
     * eapply Inner;[exact E|exact Rest].
Qed.

Lemma solved_pattern_stable : forall variables pattern before after,
 solution_keys_within variables before->solution_keys_within variables after->
 solution_extends before after->
 (forall t,In t variables->In t(ty_frees pattern)->exists tau,lookup_subst t before=Some tau)->
 app_subst before pattern=app_subst after pattern.
Proof.
 intros variables pattern before after Before After Ext Solved.
 apply app_subst_frees_agree;intros t Occurs.
 destruct(in_dec Nat.eq_dec t variables)as[Member|Outside].
 - destruct(Solved t Member Occurs)as[tau E].
   specialize(Ext t tau E);cbn;rewrite E,Ext;reflexivity.
 - assert(A:lookup_subst t before=None).
   { destruct(lookup_subst t before)as[tau|]eqn:E;[|reflexivity].
     exfalso;apply Outside;eapply Before;exact E. }
   assert(B:lookup_subst t after=None).
   { destruct(lookup_subst t after)as[tau|]eqn:E;[|reflexivity].
     exfalso;apply Outside;eapply After;exact E. }
   cbn;rewrite A,B;reflexivity.
Qed.

Lemma matched_pattern_interprets : forall pattern variables actual before after,
 solution_keys_within variables before->
 match_generation_pattern variables pattern actual before=Some after->
 app_subst after pattern=actual.
Proof.
 induction pattern as[|key|n ps IH|ps IH]using ty_ind';intros variables actual before after Initial E.
 - destruct actual;cbn[match_generation_pattern]in E;try discriminate.
   inversion E;reflexivity.
 - cbn[match_generation_pattern]in E.
   destruct(existsb(Nat.eqb key)variables)eqn:Member.
   + pose proof(added_generation_has_meaning _ _ _ _ E)as Meaning.
     cbn;rewrite Meaning;reflexivity.
   + destruct(ty_eqb(TAbs key)actual)eqn:Eq;[|discriminate].
     apply ty_eqb_eq in Eq;subst actual;inversion E;subst after.
     assert(No:lookup_subst key before=None).
     { destruct(lookup_subst key before)as[tau|]eqn:Lookup;[|reflexivity].
       assert(In key variables)by(eapply Initial;exact Lookup).
       assert(existsb(Nat.eqb key)variables=true)by
         (apply existsb_exists;exists key;split;[assumption|apply Nat.eqb_refl]). congruence. }
     cbn;rewrite No;reflexivity.
 - destruct actual as[|u|m xs|xs];cbn[match_generation_pattern]in E;try discriminate.
   destruct(Nat.eqb n m)eqn:Heads;[|discriminate]. apply Nat.eqb_eq in Heads;subst m.
   cbn;f_equal.
   revert xs before after Initial E;induction IH as[|[key p]rest Head Tail Inner];
     intros xs before after Initial E;destruct xs as[|[actual_key a]xs];cbn in E;try discriminate.
   + reflexivity.
   + destruct(Nat.eqb key actual_key)eqn:Keys;[|discriminate].
     apply Nat.eqb_eq in Keys;subst actual_key.
     destruct(match_generation_pattern variables p a before)as[middle|]eqn:M;[|discriminate].
     assert(Middle:solution_keys_within variables middle)by(eapply match_generation_pattern_keys;eassumption).
     assert(TailMatch:match_generation_pattern variables(TNom 0 rest)(TNom 0 xs)middle=Some after)
       by(cbn[match_generation_pattern];exact E).
     assert(Final:solution_keys_within variables after)by(eapply match_generation_pattern_keys;[exact Middle|exact TailMatch]).
     assert(Stable:app_subst middle p=app_subst after p).
     { eapply solved_pattern_stable;[exact Middle|exact Final|
         eapply match_generation_pattern_extends;exact TailMatch|].
       intros t Member Occurs;eapply matched_variables_have_meanings;[exact M|exact Member|exact Occurs]. }
     pose proof(Head variables a before middle Initial M)as Actual;cbn in Actual.
     cbn;rewrite <-Stable,Actual.
     f_equal;eapply Inner;[exact Middle|exact E].
 - destruct actual as[|u|m xs|xs];cbn[match_generation_pattern]in E;try discriminate.
   cbn;f_equal.
   revert xs before after Initial E;induction IH as[|p rest Head Tail Inner];
     intros xs before after Initial E;destruct xs as[|a xs];cbn in E;try discriminate.
   + reflexivity.
   + destruct(match_generation_pattern variables p a before)as[middle|]eqn:M;[|discriminate].
     assert(Middle:solution_keys_within variables middle)by(eapply match_generation_pattern_keys;eassumption).
     assert(TailMatch:match_generation_pattern variables(TUnion rest)(TUnion xs)middle=Some after)
       by(cbn[match_generation_pattern];exact E).
     assert(Final:solution_keys_within variables after)by(eapply match_generation_pattern_keys;[exact Middle|exact TailMatch]).
     assert(Stable:app_subst middle p=app_subst after p).
     { eapply solved_pattern_stable;[exact Middle|exact Final|
         eapply match_generation_pattern_extends;exact TailMatch|].
       intros t Member Occurs;eapply matched_variables_have_meanings;[exact M|exact Member|exact Occurs]. }
     pose proof(Head variables a before middle Initial M)as Actual;cbn in Actual.
     cbn;rewrite <-Stable,Actual.
     f_equal;eapply Inner;[exact Middle|exact E].
Qed.

Definition solution_compatible (desired solutions:subst):Prop:=
 forall t tau,lookup_subst t solutions=Some tau->app_subst desired(TAbs t)=tau.
Lemma add_generation_solution_complete : forall desired t before,
 solution_compatible desired before->exists after,
 add_generation_solution t(app_subst desired(TAbs t))before=Some after /\
 solution_compatible desired after.
Proof.
 intros desired t before Compatible;unfold add_generation_solution.
 destruct(lookup_subst t before)as[previous|]eqn:E.
 - pose proof(Compatible _ _ E)as Equal;rewrite Equal.
   rewrite(proj2(ty_eqb_eq previous previous)eq_refl).
   exists before;split;[reflexivity|exact Compatible].
 - exists((t,app_subst desired(TAbs t))::before);split;[reflexivity|].
   intros key tau Lookup;cbn in Lookup;destruct(Nat.eqb key t)eqn:Key.
   + apply Nat.eqb_eq in Key;subst;inversion Lookup;reflexivity.
   + apply Compatible;exact Lookup.
Qed.

Lemma match_generation_pattern_complete : forall pattern variables desired before,
 solution_keys_within variables desired->solution_compatible desired before->
 exists after,match_generation_pattern variables pattern(app_subst desired pattern)before=Some after /\
 solution_compatible desired after.
Proof.
 induction pattern as[|key|n ps IH|ps IH]using ty_ind';intros variables desired before Within Compatible.
 - exists before;split;[reflexivity|exact Compatible].
 - cbn[match_generation_pattern]. destruct(existsb(Nat.eqb key)variables)eqn:Member.
   + apply add_generation_solution_complete;exact Compatible.
   + assert(No:lookup_subst key desired=None).
     { destruct(lookup_subst key desired)as[tau|]eqn:Lookup;[|reflexivity].
       assert(In key variables)by(eapply Within;exact Lookup).
       assert(existsb(Nat.eqb key)variables=true)by
         (apply existsb_exists;exists key;split;[assumption|apply Nat.eqb_refl]). congruence. }
     cbn;rewrite No;cbn;rewrite Nat.eqb_refl.
     exists before;split;[reflexivity|exact Compatible].
 - cbn[app_subst match_generation_pattern];rewrite Nat.eqb_refl.
   revert before Compatible;induction IH as[|[key p]rest Head Tail Inner];intros before Compatible.
   + exists before;split;[reflexivity|exact Compatible].
   + cbn;rewrite Nat.eqb_refl.
     destruct(Head variables desired before Within Compatible)as[middle[Match Middle]].
     cbn in Match;rewrite Match.
     destruct(Inner middle Middle)as[after[Rest Final]].
     exists after;split;[exact Rest|exact Final].
 - cbn[app_subst match_generation_pattern].
   revert before Compatible;induction IH as[|p rest Head Tail Inner];intros before Compatible.
   + exists before;split;[reflexivity|exact Compatible].
   + cbn;destruct(Head variables desired before Within Compatible)as[middle[Match Middle]].
     rewrite Match;destruct(Inner middle Middle)as[after[Rest Final]].
     exists after;split;[exact Rest|exact Final].
Qed.
