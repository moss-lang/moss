(* Whole checked scopes allocate fresh declaration slots, even when ordered
   source output occurrences repeat the same source name. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import CoreMoss Elab ReifyData Reify ReifyRoles ReifySource ReifyInterfaces ReifyFresh.
Import ListNotations.
Lemma allocated_state_same_tables : forall before after,
 rf_next after=rf_next before->rf_declarations after=rf_declarations before->
 allocated_declarations before->allocated_declarations after.
Proof. intros before after N D H;unfold allocated_declarations in *;rewrite N,D;exact H. Qed.
Lemma allocated_type_roles : forall names before roles sm after,
 allocated_declarations before->allocate_type_roles names before=(roles,sm,after)->
 allocated_declarations after.
Proof.
 induction names as[|name rest IH];intros before roles sm after Initial E.
 - inversion E;subst;exact Initial.
 - cbn[allocate_type_roles]in E.
   destruct(lower_type_temporary TUnit before)as[slot middle]eqn:M.
   destruct(allocate_type_roles rest middle)as[[tail maps]final]eqn:T.
   inversion E;subst;eapply IH;[eapply type_temporary_preserves_allocation;eassumption|exact T].
Qed.
Lemma allocated_source_type_interface : forall source scope before,
 allocated_declarations before->allocated_declarations(sti_state(lower_source_type_interface source scope before)).
Proof.
 intros source scope before Initial;unfold lower_source_type_interface.
 destruct(allocate_type_roles(source_scope_type_keys scope)before)as[[roles sm]after]eqn:E.
 eapply allocated_state_same_tables with(before:=after);[reflexivity|reflexivity|].
 eapply allocated_type_roles;eassumption.
Qed.
Lemma allocated_scope_values : forall source scope values before outputs after,
 allocated_declarations before->lower_scope_values source scope values before=(outputs,after)->
 allocated_declarations after.
Proof.
 intros source scope values;induction values as[|[name tau]rest IH];intros before outputs after Initial E.
 - inversion E;subst;exact Initial.
 - cbn[lower_scope_values]in E.
   destruct(lower_value_provision name
     (value_type_inputs(Audit.app_subst(rf_generations before)(reify_snapshot_type source scope tau)))
     (reify_snapshot_type source scope tau)before)as[slot middle]eqn:M.
   destruct(lower_scope_values source scope rest middle)as[tail final]eqn:T.
   inversion E;subst;eapply IH;[eapply value_provision_preserves_allocation;eassumption|exact T].
Qed.
Lemma allocated_scope_functions : forall source scope functions before outputs after,
 allocated_declarations before->lower_scope_functions source scope functions before=(outputs,after)->
 allocated_declarations after.
Proof.
 intros source scope functions;induction functions as[|[name signature]rest IH];intros before outputs after Initial E.
 - inversion E;subst;exact Initial.
 - cbn[lower_scope_functions]in E.
   destruct(lower_function_provision name
     (signature_type_inputs(lower_fn_signature before(snapshot_signature source scope signature)))
     (snapshot_signature source scope signature)before)as[slot middle]eqn:M.
   destruct(lower_scope_functions source scope rest middle)as[tail final]eqn:T.
   inversion E;subst;eapply IH;[eapply function_provision_preserves_allocation;eassumption|exact T].
Qed.
Lemma allocated_scope_methods : forall source scope methods before outputs after,
 allocated_declarations before->lower_scope_methods source scope methods before=(outputs,after)->
 allocated_declarations after.
Proof.
 intros source scope methods;induction methods as[|[[receiver name]signature]rest IH];intros before outputs after Initial E.
 - inversion E;subst;exact Initial.
 - cbn[lower_scope_methods]in E.
   destruct(lower_method_provision(reify_snapshot_item source scope(KyMth receiver name))
     (reify_snapshot_type source scope receiver)
     (signature_type_inputs(lower_fn_signature before(snapshot_signature source scope signature)))
     (snapshot_signature source scope signature)before)as[slot middle]eqn:M.
   destruct(lower_scope_methods source scope rest middle)as[tail final]eqn:T.
   inversion E;subst;eapply IH;[eapply method_provision_preserves_allocation;eassumption|exact T].
Qed.
Lemma allocated_scope_first_order : forall source scope before outputs after,
 allocated_declarations before->lower_scope_first_order source scope before=(outputs,after)->
 allocated_declarations after.
Proof.
 intros source scope before outputs after Initial E;unfold lower_scope_first_order in E.
 destruct(lower_scope_values source scope(rs_values ety key scope)before)as[values one]eqn:V.
 destruct(lower_scope_functions source scope(rs_functions ety key scope)one)as[functions two]eqn:F.
 destruct(lower_scope_methods source scope(rs_methods ety key scope)two)as[methods three]eqn:M.
 inversion E;subst;eapply allocated_scope_methods;[|exact M].
 eapply allocated_scope_functions;[|exact F]. eapply allocated_scope_values;eassumption.
Qed.

Lemma scope_values_slot_sequence : forall source scope values before outputs after,
 lower_scope_values source scope values before=(outputs,after)->
 map reify_item_slot outputs=seq(rf_next before)(List.length values).
Proof.
 intros source scope values;induction values as[|[name tau]rest IH];intros before outputs after E.
 - inversion E;reflexivity.
 - cbn[lower_scope_values lower_value_provision]in E.
   destruct(lower_scope_values source scope rest
     (ReifyState(S(rf_next before))(rf_names before)(rf_generations before)
       ((IVal name,IVal(rf_next before))::rf_items before)
       ((IVal(rf_next before),RDValue(value_type_inputs(Audit.app_subst(rf_generations before)
          (reify_snapshot_type source scope tau)))(Audit.app_subst(rf_generations before)
          (reify_snapshot_type source scope tau)))::rf_declarations before)))
     as[tail final]eqn:T.
   inversion E;subst;pose proof(IH _ _ _ T)as Tail;cbn in Tail |- *;f_equal;exact Tail.
Qed.
Lemma scope_functions_slot_sequence : forall source scope functions before outputs after,
 lower_scope_functions source scope functions before=(outputs,after)->
 map reify_item_slot outputs=seq(rf_next before)(List.length functions).
Proof.
 intros source scope functions;induction functions as[|[name signature]rest IH];intros before outputs after E.
 - inversion E;reflexivity.
 - cbn[lower_scope_functions lower_function_provision]in E.
   destruct(lower_scope_functions source scope rest
     (ReifyState(S(rf_next before))(rf_names before)(rf_generations before)
       ((IFn name,IFn(rf_next before))::rf_items before)
       ((IFn(rf_next before),RDFunction(signature_type_inputs(lower_fn_signature before
          (snapshot_signature source scope signature)))(lower_fn_signature before
          (snapshot_signature source scope signature)))::rf_declarations before)))
     as[tail final]eqn:T.
   inversion E;subst;pose proof(IH _ _ _ T)as Tail;cbn in Tail |- *;f_equal;exact Tail.
Qed.
Lemma scope_methods_slot_sequence : forall source scope methods before outputs after,
 lower_scope_methods source scope methods before=(outputs,after)->
 map reify_item_slot outputs=seq(rf_next before)(List.length methods).
Proof.
 intros source scope methods;induction methods as[|[[receiver name]signature]rest IH];intros before outputs after E.
 - inversion E;reflexivity.
 - cbn[lower_scope_methods]in E.
   destruct(lower_method_provision(reify_snapshot_item source scope(KyMth receiver name))
     (reify_snapshot_type source scope receiver)
     (signature_type_inputs(lower_fn_signature before(snapshot_signature source scope signature)))
     (snapshot_signature source scope signature)before)as[target middle]eqn:M.
   destruct(lower_scope_methods source scope rest middle)as[tail final]eqn:T.
   inversion E;subst;pose proof(IH _ _ _ T)as Tail.
   inversion M;subst;cbn in Tail |- *;f_equal;exact Tail.
Qed.

Lemma scope_functions_counter : forall source scope functions before outputs after,
 lower_scope_functions source scope functions before=(outputs,after)->
 rf_next after=rf_next before+List.length functions.
Proof.
 intros source scope functions;induction functions as[|[name signature]rest IH];intros before outputs after E.
 - inversion E;cbn;lia.
 - cbn[lower_scope_functions]in E.
   destruct(lower_function_provision name
     (signature_type_inputs(lower_fn_signature before(snapshot_signature source scope signature)))
     (snapshot_signature source scope signature)before)as[slot middle]eqn:M.
   destruct(lower_scope_functions source scope rest middle)as[tail final]eqn:T.
   inversion E;subst;pose proof(IH _ _ _ T)as N.
   inversion M;subst;cbn in N |- *;lia.
Qed.

Lemma scope_first_order_slot_sequence : forall source scope before outputs after,
 lower_scope_first_order source scope before=(outputs,after)->
 map reify_item_slot outputs=seq(rf_next before)
   (List.length(rs_values ety key scope)+List.length(rs_functions ety key scope)+
     List.length(rs_methods ety key scope)).
Proof.
 intros source scope before outputs after E;unfold lower_scope_first_order in E.
 destruct(lower_scope_values source scope(rs_values ety key scope)before)as[values one]eqn:V.
 destruct(lower_scope_functions source scope(rs_functions ety key scope)one)as[functions two]eqn:F.
 destruct(lower_scope_methods source scope(rs_methods ety key scope)two)as[methods three]eqn:M.
 inversion E;subst;rewrite !map_app,
   (scope_values_slot_sequence _ _ _ _ _ _ V),
   (scope_functions_slot_sequence _ _ _ _ _ _ F),
   (scope_methods_slot_sequence _ _ _ _ _ _ M).
 destruct(lower_scope_values_preserves_types _ _ _ _ _ _ V)as[_[_[_ One]]].
 pose proof(scope_functions_counter _ _ _ _ _ _ F)as Two.
 rewrite Two,One.
 rewrite app_assoc,<-seq_app.
 rewrite(seq_app(List.length(rs_values ety key scope)+List.length(rs_functions ety key scope))
   (List.length(rs_methods ety key scope))(rf_next before)).
 now rewrite Nat.add_assoc.
Qed.
Lemma scope_first_order_slots_unique : forall source scope before outputs after,
 lower_scope_first_order source scope before=(outputs,after)->NoDup(map reify_item_slot outputs).
Proof. intros;rewrite(scope_first_order_slot_sequence _ _ _ _ _ H);apply seq_NoDup. Qed.
