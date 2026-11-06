(* One counter separates every immutable declaration namespace. This is a
   structural invariant of the allocator, independent of source typing. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import CoreMoss ReifyData Reify.
Import ListNotations.
Definition reify_item_slot (i:item):nat:=
 match i with ITy t=>t|IVal v=>v|IFn f=>f|IMth _ m _=>m|IFun u=>u end.
Definition allocated_declarations (s:reify_state):Prop:=
 NoDup(map(fun p=>reify_item_slot(fst p))(rf_declarations s)) /\
 Forall(fun p=>reify_item_slot(fst p)<rf_next s)(rf_declarations s).

Lemma fresh_declaration_extension : forall before names generations items key declaration,
 allocated_declarations before->reify_item_slot key=rf_next before->
 allocated_declarations(ReifyState(S(rf_next before))names generations items
   ((key,declaration)::rf_declarations before)).
Proof.
 intros before names generations items key declaration[Unique Bounded]Key.
 split;cbn.
 - constructor;[|exact Unique]. intro Hin;apply in_map_iff in Hin as[[old d][E Hin]].
   apply Forall_forall with(x:=(old,d))in Bounded;[|exact Hin]. cbn in E,Bounded;lia.
 - constructor;[cbn;lia|]. eapply Forall_impl;[|exact Bounded]. intros[old d]H;cbn in *;lia.
Qed.

Lemma type_alias_preserves_allocation : forall before name rhs slot meaning after,
 allocated_declarations before->lower_type_alias name rhs before=(slot,meaning,after)->
 allocated_declarations after.
Proof.
 intros before name rhs slot meaning after H E;inversion E;subst.
 eapply fresh_declaration_extension;[exact H|reflexivity].
Qed.
Lemma type_temporary_preserves_allocation : forall before meaning slot after,
 allocated_declarations before->lower_type_temporary meaning before=(slot,after)->
 allocated_declarations after.
Proof.
 intros before meaning slot after H E;inversion E;subst.
 eapply fresh_declaration_extension;[exact H|reflexivity].
Qed.
Lemma materialized_arguments_preserve_allocation : forall arguments before bindings sm after,
 allocated_declarations before->materialize_type_arguments arguments before=(bindings,sm,after)->
 allocated_declarations after.
Proof.
 induction arguments as[|[formal tau]rest IH]; intros before bindings sm after H E.
 - inversion E;subst;exact H.
 - cbn[materialize_type_arguments]in E.
   destruct(lower_type_temporary tau before)as[slot middle]eqn:T.
   destruct(materialize_type_arguments rest middle)as[[tail maps]final]eqn:M.
   inversion E;subst;eapply IH;[eapply type_temporary_preserves_allocation;eassumption|exact M].
Qed.

Lemma same_key_has_same_slot : forall sigma left right,
 same_key sigma left right=true->reify_item_slot left=reify_item_slot right.
Proof.
 intros sigma left right H;destruct left;destruct right;cbn in H |- *;try discriminate;
   try(apply Nat.eqb_eq;exact H).
 apply andb_true_iff in H as[H _];apply Nat.eqb_eq;exact H.
Qed.
Lemma find_same_key_absent_slot : forall sigma items wanted,
 ~In(reify_item_slot wanted)(map reify_item_slot items)->
 find_same_key sigma items wanted=None.
Proof.
 intros sigma items;induction items as[|item rest IH];intros wanted Outside;[reflexivity|].
 cbn;destruct(same_key sigma item wanted)eqn:Same.
 - exfalso;apply Outside;left;eapply same_key_has_same_slot;exact Same.
 - apply IH;intro Hin;apply Outside;right;exact Hin.
Qed.
Lemma merge_distinct_slots : forall tail before,
 NoDup(map reify_item_slot(before++tail))->
 merge_tele(mkCtx before[])tail=Some(mkCtx(before++tail)[]).
Proof.
 induction tail as[|item rest IH];intros before Unique.
 - cbn;rewrite app_nil_r;reflexivity.
 - assert(Outside:~In(reify_item_slot item)(map reify_item_slot before)).
   { rewrite map_app in Unique;cbn in Unique.
     pose proof(NoDup_remove_2(map reify_item_slot before)(map reify_item_slot rest)
       (reify_item_slot item)Unique)as Absent.
     intro Hin;apply Absent;apply in_or_app;left;exact Hin. }
   cbn[merge_tele];unfold merge_item;cbn[cA cS].
   rewrite(find_same_key_absent_slot[]before item Outside).
   replace(before++item::rest)with((before++[item])++rest)by(now rewrite <-app_assoc).
   apply IH;rewrite <-app_assoc;cbn;exact Unique.
Qed.
Lemma distinct_slot_telescope_context : forall telescope,
 NoDup(map reify_item_slot telescope)->tele_ctx telescope=Some(mkCtx telescope[]).
Proof.
 intros telescope Unique;unfold tele_ctx,empty_ctx.
 exact(merge_distinct_slots telescope[]Unique).
Qed.

Lemma output_generation_preserves_allocation : forall before name generation slot after,
 allocated_declarations before->lower_output_generation name generation before=(slot,after)->
 allocated_declarations after.
Proof.
 intros before name generation slot after H E;inversion E;subst.
 eapply fresh_declaration_extension;[exact H|reflexivity].
Qed.
Lemma value_provision_preserves_allocation : forall before name inputs meaning slot after,
 allocated_declarations before->lower_value_provision name inputs meaning before=(slot,after)->
 allocated_declarations after.
Proof.
 intros before name inputs meaning slot after H E;inversion E;subst.
 eapply fresh_declaration_extension;[exact H|reflexivity].
Qed.
Lemma function_provision_preserves_allocation : forall before name inputs signature slot after,
 allocated_declarations before->lower_function_provision name inputs signature before=(slot,after)->
 allocated_declarations after.
Proof.
 intros before name inputs signature slot after H E;inversion E;subst.
 eapply fresh_declaration_extension;[exact H|reflexivity].
Qed.
Lemma method_provision_preserves_allocation : forall before original receiver inputs signature target after,
 allocated_declarations before->lower_method_provision original receiver inputs signature before=(target,after)->
 allocated_declarations after.
Proof.
 intros before original receiver inputs signature target after H E;inversion E;subst.
 eapply fresh_declaration_extension;[exact H|reflexivity].
Qed.
Lemma functor_copy_preserves_allocation : forall before name schema slot after,
 allocated_declarations before->lower_functor_copy name schema before=(slot,after)->
 allocated_declarations after.
Proof.
 intros before name schema slot after H E;inversion E;subst.
 eapply fresh_declaration_extension;[exact H|reflexivity].
Qed.

Lemma formed_distinct_slot_telescope : forall Sg telescope before,
 NoDup(map reify_item_slot(before++telescope))->
 (forall prefix item suffix,telescope=prefix++item::suffix->
   item_ok Sg(mkCtx(before++prefix)[])item)->
 wf_tele Sg(mkCtx before[])telescope(mkCtx(before++telescope)[]).
Proof.
 intros Sg telescope;induction telescope as[|item rest IH];intros before Unique Formed.
 - rewrite app_nil_r;constructor.
 - assert(Outside:~In(reify_item_slot item)(map reify_item_slot before)).
   { rewrite map_app in Unique;cbn in Unique.
     pose proof(NoDup_remove_2(map reify_item_slot before)(map reify_item_slot rest)
       (reify_item_slot item)Unique)as Absent.
     intro Hin;apply Absent;apply in_or_app;left;exact Hin. }
   replace(before++item::rest)with((before++[item])++rest)by(now rewrite <-app_assoc).
   eapply WCons.
   + specialize(Formed[]item rest eq_refl);cbn in Formed;rewrite app_nil_r in Formed;exact Formed.
   + unfold merge_item;cbn[cA cS];rewrite(find_same_key_absent_slot[]before item Outside);reflexivity.
   + apply IH.
     * rewrite <-app_assoc;cbn;exact Unique.
     * intros prefix next suffix Equal.
       specialize(Formed(item::prefix)next suffix).
       assert(rest=prefix++next::suffix)by exact Equal.
       specialize(Formed(f_equal(cons item)H));cbn in Formed.
       rewrite <-app_assoc;cbn;exact Formed.
Qed.
