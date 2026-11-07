(* Fresh physical type roles are genuine declared type inputs, even after
   later scopes and provider bodies extend the finite declaration table. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import CoreMoss ReifyData Reify ReifyRoles ReifyFresh ReifySig ReifyDeclProofs.
Import ListNotations.
Lemma allocated_type_signature : forall base final role,
 allocated_declarations final->In(ITy role,RDType)(rf_declarations final)->
 g_ty(reify_signature_overlay base(rf_declarations final))role=true.
Proof.
 intros base final role[Unique Bounded]Member;cbn.
 rewrite(unique_slot_declaration_lookup _ _ _ Unique Member);reflexivity.
Qed.
Lemma allocated_roles_declarations : forall names before roles sm after,
 allocate_type_roles names before=(roles,sm,after)->
 rf_declarations after=
   rev(map(fun t=>(ITy t,RDType))(seq(rf_next before)(length names)))++rf_declarations before.
Proof.
 induction names as[|name rest IH];intros before roles sm after Run.
 - inversion Run;reflexivity.
 - cbn in Run.
   destruct(allocate_type_roles rest
     (ReifyState(S(rf_next before))(rf_names before)(rf_generations before)(rf_items before)
       ((ITy(rf_next before),RDType)::rf_declarations before)))as[[tail maps]final]eqn:Tail.
   injection Run as <- <- <-. rewrite(IH _ _ _ _ Tail);cbn[rf_next rf_declarations length seq map rev].
   rewrite <-app_assoc;reflexivity.
Qed.
Lemma allocated_role_telescope_formed : forall names before roles sm after base final,
 allocate_type_roles names before=(roles,sm,after)->
 allocated_declarations final->incl(rf_declarations after)(rf_declarations final)->
 this_sym<rf_next before->
 wf_tele(reify_signature_overlay base(rf_declarations final))empty_ctx
   (map ITy(seq(rf_next before)(length names)))
   (mkCtx(map ITy(seq(rf_next before)(length names)))[]).
Proof.
 intros names before roles sm after base final Run Allocated Contains Positive.
 pose proof(allocated_roles_declarations _ _ _ _ _ Run)as Declarations.
 change(wf_tele(reify_signature_overlay base(rf_declarations final))(mkCtx[][])
   (map ITy(seq(rf_next before)(length names)))
   (mkCtx([]++map ITy(seq(rf_next before)(length names)))[])).
 apply formed_distinct_slot_telescope.
 - cbn;rewrite map_map;change(NoDup(map(fun t=>t)(seq(rf_next before)(length names)))).
   rewrite map_id;apply seq_NoDup.
 - intros prefix item suffix Split.
   assert(Member:In item(map ITy(seq(rf_next before)(length names)))).
   { rewrite Split;apply in_or_app;right;left;reflexivity. }
   apply in_map_iff in Member as[role[<- Within]].
   constructor.
   + eapply allocated_type_signature;[exact Allocated|].
     apply Contains;rewrite Declarations;apply in_or_app;left;apply (proj1(in_rev _ _)).
     apply in_map_iff;exists role;split;[reflexivity|exact Within].
   + apply in_seq in Within;lia.
Qed.
