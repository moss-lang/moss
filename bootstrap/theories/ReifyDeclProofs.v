(* Fresh allocation makes every emitted declaration visible in the final
   finite signature, including after later bodies allocate more slots. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import CoreMoss ReifyData Reify ReifySig ReifyFresh.
Import ListNotations.

Lemma unique_slot_declaration_lookup : forall declarations key declaration,
 NoDup(map(fun p=>reify_item_slot(fst p))declarations)->
 In(key,declaration)declarations->lookup_reify_decl key declarations=Some declaration.
Proof.
 induction declarations as[|[first value]rest IH];intros key declaration Unique Member;[contradiction|].
 inversion Unique as[|slot slots Absent Rest];subst;cbn in Member |- *.
 destruct Member as[Equal|Member].
 - inversion Equal;subst;rewrite(proj2(item_eqb_eq key key)eq_refl);reflexivity.
 - destruct(item_eqb key first)eqn:Equal.
   + apply item_eqb_eq in Equal;subst first;exfalso;apply Absent.
     apply in_map_iff;exists(key,declaration);split;[reflexivity|exact Member].
   + eapply IH;[exact Rest|exact Member].
Qed.

Lemma allocated_value_signature : forall base state value inputs meaning,
 allocated_declarations state->In(IVal value,RDValue inputs meaning)(rf_declarations state)->
 g_val(reify_signature_overlay base(rf_declarations state))value=Some(inputs,meaning).
Proof.
 intros base state value inputs meaning[Unique Bounded]Member;cbn.
 rewrite(unique_slot_declaration_lookup _ _ _ Unique Member);reflexivity.
Qed.
Lemma allocated_function_signature : forall base state function inputs signature,
 allocated_declarations state->In(IFn function,RDFunction inputs signature)(rf_declarations state)->
 g_fn(reify_signature_overlay base(rf_declarations state))function=Some(inputs,signature,None).
Proof.
 intros base state function inputs signature[Unique Bounded]Member.
 apply reify_signature_function;eapply unique_slot_declaration_lookup;eassumption.
Qed.
Lemma allocated_functor_signature : forall base state functor schema,
 allocated_declarations state->In(IFun functor,RDFunctor schema)(rf_declarations state)->
 g_fun(reify_signature_overlay base(rf_declarations state))functor=
   Some(rfd_inputs schema,rfd_outputs schema,None).
Proof.
 intros base state functor schema[Unique Bounded]Member.
 apply reify_signature_functor;unfold lookup_reify_functor.
 rewrite(unique_slot_declaration_lookup _ _ _ Unique Member);reflexivity.
Qed.

Lemma unique_slot_method_lookup : forall declarations receiver method theta inputs signature,
 NoDup(map(fun p=>reify_item_slot(fst p))declarations)->
 In(IMth receiver method theta,RDMethod inputs signature)declarations->
 lookup_reify_method method declarations=Some(inputs,signature).
Proof.
 induction declarations as[|[first value]rest IH];
   intros receiver method theta inputs signature Unique Member;[contradiction|].
 inversion Unique as[|slot slots Absent Rest];subst;cbn in Member.
 destruct Member as[Equal|Member].
 - inversion Equal;subst;cbn[lookup_reify_method];rewrite Nat.eqb_refl;reflexivity.
 - destruct first as[t|v|f|r n th|u];
     destruct value as[|Dv tau|Df Sf|Dm Sm|schema|original];cbn[lookup_reify_method];
     try(eapply IH;[exact Rest|exact Member]).
   destruct(Nat.eqb method n)eqn:Same.
   + apply Nat.eqb_eq in Same;subst n;exfalso;apply Absent.
     apply in_map_iff;exists(IMth receiver method theta,RDMethod inputs signature).
     split;[reflexivity|exact Member].
   + eapply IH;[exact Rest|exact Member].
Qed.
Lemma allocated_method_signature : forall base state receiver method theta inputs signature,
 allocated_declarations state->
 In(IMth receiver method theta,RDMethod inputs signature)(rf_declarations state)->
 g_mth(reify_signature_overlay base(rf_declarations state))method=Some(inputs,signature).
Proof.
 intros base state receiver method theta inputs signature[Unique Bounded]Member;cbn.
 rewrite(unique_slot_method_lookup _ _ _ _ _ _ Unique Member);reflexivity.
Qed.
