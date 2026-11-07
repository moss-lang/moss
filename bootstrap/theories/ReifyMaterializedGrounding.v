(* Actual argument materialization supplies every callee type slot. Hidden
   default inputs can be retained as an explicit prefix of the same map. *)
From Stdlib Require Import List Arith Bool Lia.
From NM Require Import CoreMoss ReifyMaps Reify ReifyAliasGrounding ReifyTransferGrounding.
Import ListNotations.
Lemma combine_exact_domains : forall X Y(xs:list X)(ys:list Y),
 length xs=length ys->map fst(combine xs ys)=xs/\map snd(combine xs ys)=ys.
Proof.
 intros X Y xs;induction xs as[|x rest IH];intros[|y tail]Length;try discriminate;cbn;[auto|].
 destruct(IH tail ltac:(cbn in Length;lia))as[First Second];split;f_equal;assumption.
Qed.
Lemma type_transfer_combine : forall formals actuals,
 type_transfer_items(combine formals actuals)=combine(map ITy formals)(map ITy actuals).
Proof.
 induction formals as[|formal rest IH];intros[|actual tail];cbn[type_transfer_items combine map];
 try reflexivity;f_equal;apply IH.
Qed.
Lemma materialized_argument_pairs : forall arguments before bindings sm after,
 materialize_type_arguments arguments before=(bindings,sm,after)->
 exists pairs,sm=type_transfer_items pairs/\map fst pairs=map fst arguments/\
 map snd pairs=map fst bindings.
Proof.
 intros arguments before bindings sm after Run.
 destruct(materialize_type_arguments_structure _ _ _ _ _ Run)as[Values[Items Next]].
 exists(combine(map fst arguments)(seq(rf_next before)(length arguments))).
 assert(EqualLengths:length(map fst arguments)=length(seq(rf_next before)(length arguments))).
 { rewrite map_length,seq_length;reflexivity. }
 destruct(combine_exact_domains _ _ _ _ EqualLengths)as[Formals Actuals].
 split.
 - rewrite Items,type_transfer_combine,map_map;reflexivity.
 - split;[exact Formals|].
   etransitivity;[exact Actuals|];rewrite Values.
   symmetry;apply(proj1(combine_exact_domains _ _ _ _ ltac:(rewrite seq_length,map_length;reflexivity))).
Qed.
Lemma temporary_context_keeps_type_keys : forall bindings Phi,
 incl(tyreqs(cA Phi))(tyreqs(cA(type_temporary_context bindings Phi))).
Proof.
 induction bindings as[|[slot meaning]rest IH];intros Phi key Member;[exact Member|].
 change(In key(tyreqs(cA(type_temporary_context rest(bind_ty slot meaning Phi))))).
 apply IH;cbn;right;exact Member.
Qed.
Lemma temporary_context_contains_bindings : forall bindings Phi,
 incl(map fst bindings)(tyreqs(cA(type_temporary_context bindings Phi))).
Proof.
 induction bindings as[|[slot meaning]rest IH];intros Phi key Member;[contradiction|].
 change(In key(tyreqs(cA(type_temporary_context rest(bind_ty slot meaning Phi))))).
 destruct Member as[Same|Member].
 - subst key;apply temporary_context_keeps_type_keys;cbn;left;reflexivity.
 - apply IH;exact Member.
Qed.
Theorem actual_materialized_map_grounding : forall arguments before bindings sm after prefix Phi D g,
 materialize_type_arguments arguments before=(bindings,sm,after)->
 NoDup(map fst prefix++map fst arguments)->
 tyreqs D=map fst prefix++map fst arguments->
 ~In this_sym(map fst prefix++map fst arguments)->
 Forall(fun p=>In(snd p)(tyreqs(cA Phi)))prefix->
 ctx_grounded g(type_temporary_context bindings Phi)->
 exists target,ctx_grounded target(mkCtx D[])/\
 map_type_agreement target g(type_transfer_items prefix++sm).
Proof.
 intros arguments before bindings sm after prefix Phi D g Run Unique Domain Nonzero Prefix Ground.
 destruct(materialized_argument_pairs _ _ _ _ _ Run)as[pairs[Items[Formals Actuals]]].
 assert(Available:Forall(fun p=>In(snd p)(tyreqs(cA(type_temporary_context bindings Phi))))(prefix++pairs)).
 { apply Forall_app;split.
   - eapply Forall_impl;[|exact Prefix];intros[formal actual]Member.
     apply temporary_context_keeps_type_keys;exact Member.
   - apply Forall_forall;intros[formal actual]Member.
     apply temporary_context_contains_bindings.
     rewrite <-Actuals;apply in_map_iff;exists(formal,actual);auto. }
 assert(AllUnique:NoDup(map fst(prefix++pairs))).
 { rewrite map_app,Formals;exact Unique. }
 assert(AllDomain:tyreqs D=map fst(prefix++pairs)).
 { rewrite map_app,Formals;exact Domain. }
 assert(AllNonzero:~In this_sym(map fst(prefix++pairs))).
 { rewrite map_app,Formals;exact Nonzero. }
 destruct(actual_transfer_context_grounded _ _ _ _ Ground AllUnique AllDomain AllNonzero Available)
   as[Target Agreement].
 exists(ground_type_transfer g(prefix++pairs));split;[exact Target|].
 unfold type_transfer_items in Agreement;rewrite map_app in Agreement.
 rewrite Items;exact Agreement.
Qed.
