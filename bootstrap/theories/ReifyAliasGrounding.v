(* The walker's written alias expansion is justified by actual target
   context pins. No idempotence of the pin list is required. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import CoreMoss Specialization Reify ReifyWalkData ReifyGroundEquality.
Import ListNotations.
Definition aliases_model_pins(sigma aliases:subst):Prop:=
 forall g,absorbs g sigma->forall t tau,lookup_subst t aliases=Some tau->
 app_subst g(TAbs t)=app_subst g tau.
Lemma aliases_model_types : forall sigma aliases g,
 aliases_model_pins sigma aliases->absorbs g sigma->forall tau,
 app_subst g(app_subst aliases tau)=app_subst g tau.
Proof.
 intros sigma aliases g Model Absorb tau;rewrite app_subst_compose.
 apply dentry_agree_retype_aux1;intros t _;rewrite <-app_subst_compose.
 cbn[app_subst];destruct(lookup_subst t aliases)as[value|]eqn:Read;[|reflexivity].
 symmetry;apply(Model g Absorb t value Read).
Qed.
Lemma absorbs_bind_old : forall g sigma t tau,
 ~In t(map fst sigma)->absorbs g((t,tau)::sigma)->absorbs g sigma.
Proof.
 intros g sigma t tau Fresh Absorb key value Read.
 apply Absorb;cbn;destruct(Nat.eqb key t)eqn:Equal;[|exact Read].
 apply Nat.eqb_eq in Equal;subst key;exfalso;apply Fresh.
 apply in_map_iff;exists(t,value);split;[reflexivity|].
 eapply callee_env_agree_aux1;exact Read.
Qed.
Lemma alias_models_actual_pin : forall Phi slot meaning aliases,
 ~In slot(map fst(cS Phi))->aliases_model_pins(cS Phi)aliases->
 aliases_model_pins(cS(bind_ty slot meaning Phi))
   ((slot,app_subst aliases meaning)::aliases).
Proof.
 intros Phi slot meaning aliases Fresh Old g Absorb key value Read.
 assert(OldAbsorb:absorbs g(cS Phi)).
 { eapply absorbs_bind_old;[exact Fresh|exact Absorb]. }
 cbn[lookup_subst]in Read;destruct(Nat.eqb key slot)eqn:Same.
 - apply Nat.eqb_eq in Same;subst key;injection Read as <-.
   specialize(Absorb slot(app_subst(cS Phi)meaning)ltac:(cbn;rewrite Nat.eqb_refl;reflexivity)).
   cbn[app_subst];rewrite Absorb.
   rewrite(aliases_model_types _ _ _ Old OldAbsorb meaning).
   apply absorbs_all;exact OldAbsorb.
 - eapply Old;[exact OldAbsorb|exact Read].
Qed.
Theorem local_alias_models_actual_bind : forall Phi name raw before slot meaning after,
 local_type_alias name raw before=(slot,meaning,after)->
 ~In slot(map fst(cS Phi))->
 aliases_model_pins(cS Phi)(rl_aliases before)->
 aliases_model_pins(cS(bind_ty slot meaning Phi))(rl_aliases after).
Proof.
 intros Phi name raw before slot meaning after Run Fresh Old.
 pose proof(local_type_alias_context _ _ _ _ _ _ Run)as[Aliases _].
 rewrite Aliases;apply alias_models_actual_pin;assumption.
Qed.

Definition type_temporary_context(bindings:subst)(Phi:ctx):ctx:=
 fold_left(fun current p=>bind_ty(fst p)(snd p)current)bindings Phi.
Theorem type_temporaries_model_actual_binds : forall bindings Phi aliases,
 NoDup(map fst bindings)->
 (forall slot,In slot(map fst bindings)->~In slot(map fst(cS Phi)))->
 aliases_model_pins(cS Phi)aliases->
 aliases_model_pins(cS(type_temporary_context bindings Phi))
   (aliases_after_temporaries bindings aliases).
Proof.
 induction bindings as[|[slot meaning]rest IH];intros Phi aliases Unique Fresh Model.
 - exact Model.
 - inversion Unique as[|head tail Absent Rest];subst.
   change(aliases_model_pins(cS(type_temporary_context rest(bind_ty slot meaning Phi)))
     (aliases_after_temporaries rest((slot,app_subst aliases meaning)::aliases))).
   apply IH.
   + exact Rest.
   + intros next Member;cbn[cS bind_ty map fst];intros[Same|Old].
     * subst next;apply Absent;exact Member.
     * apply(Fresh next);[right;exact Member|exact Old].
   + apply alias_models_actual_pin;[apply Fresh;left;reflexivity|exact Model].
Qed.
Lemma aliases_empty_model : forall sigma,aliases_model_pins sigma[].
Proof. intros sigma g Absorb t tau Impossible;discriminate. Qed.
Lemma aliases_model_ground_equal : forall Phi aliases tau,
 aliases_model_pins(cS Phi)aliases->grounded_type_equal Phi(app_subst aliases tau)tau.
Proof. intros Phi aliases tau Model g[_[Absorb _]];eapply aliases_model_types;eauto. Qed.
