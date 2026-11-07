(* Actual post-body source keys determine immutable exports. Output role
   placeholders are interpreted before the final checker generation map. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss Elab ReifyData Reify ReifySource
  ReifyFunctors ReifyProviders ReifyFunctorProfiles.
Import ListNotations.

Definition provider_result_replacements (profile : reify_exports ety key)
    (fixed_names : list tsym) : list(tsym*ety) :=
  map(fun p=>(snd p,scope_type_meaning(rx_actual ety key profile)(fst p)))
    (rx_outputs ety key profile)++
  map(fun name=>(name,scope_type_meaning(rx_actual ety key profile)name))fixed_names.
Definition provider_actual_result_key (profile : reify_exports ety key)
    (fixed_names : list tsym) (promised : key) : key :=
  match promised with
  | KyMth receiver method=>
      let replaced:=instantiate(env_with_edges(provider_result_replacements profile fixed_names)[])receiver in
      KyMth(instantiate(env_with_edges(rs_generations ety key(rx_actual ety key profile))[])replaced)method
  | _=>promised end.

Fixpoint provider_type_exports (roles : subst) (runtime : reify_state) : option smap :=
  match roles with
  | []=>Some[]
  | (name,TAbs formal)::rest=>match lookup_subst name(rf_names runtime),provider_type_exports rest runtime with
      | Some(TAbs actual),Some tail=>Some((ITy formal,ITy actual)::tail)
      | _,_=>None end
  | _::_=>None end.
Fixpoint provider_item_exports (source : st) (profile : reify_exports ety key)
    (fixed_names : list tsym) (keys : list key) (formals : tele)
    (runtime : reify_state) : option smap :=
  match keys,formals with
  | [],[]=>Some[]
  | source_key::keys,formal::formals=>
      let actual_key:=provider_actual_result_key profile fixed_names source_key in
      let source_item:=reify_snapshot_item source(rx_actual ety key profile)actual_key in
      match lookup_smap source_item(rf_items runtime),
        provider_item_exports source profile fixed_names keys formals runtime with
      | Some actual,Some tail=>if same_result_kind actual formal
          then Some((formal,actual)::tail)else None
      | _,_=>None end
  | _,_=>None end.
Definition first_order_result_keys (keys : list key) : list key :=
  filter(fun key=>match key with KyVal _|KyFn _|KyMth _ _=>true|_=>false end)keys.
Definition functor_result_keys (keys : list key) : list key :=
  filter(fun key=>match key with KyFun _=>true|_=>false end)keys.
Definition provider_body_exports (source : st) (profile : reify_exports ety key)
    (fixed_names : list tsym) (view : reify_functor_view) (runtime : reify_state) : option smap :=
  match provider_type_exports(rfv_output_types view)runtime,
    provider_item_exports source profile fixed_names(first_order_result_keys(rx_items ety key profile))
      (first_order_result_items(rfd_outputs(rfv_schema view)))runtime,
    provider_item_exports source profile fixed_names(functor_result_keys(rx_items ety key profile))
      (functor_result_items(rfd_outputs(rfv_schema view)))runtime with
  | Some types,Some items,Some functors=>Some(types++items++functors)
  | _,_,_=>None end.

Lemma provider_item_exports_domain : forall source profile fixed_names keys formals runtime exports,
  provider_item_exports source profile fixed_names keys formals runtime=Some exports->
  map fst exports=formals.
Proof.
 intros source profile fixed_names keys;induction keys as[|key keys IH];
   intros formals runtime exports H;destruct formals as[|formal formals];cbn in H;try discriminate.
 - inversion H;reflexivity.
 - destruct(lookup_smap _ _)as[actual|];[|discriminate].
   destruct(provider_item_exports _ _ _ _ _ _)as[tail|]eqn:E;[|discriminate].
   destruct(same_result_kind actual formal);[|discriminate].
   inversion H;subst;cbn;f_equal;eapply IH;exact E.
Qed.

Fixpoint provider_smap_same (a b : smap) : bool :=
  match a,b with
  | [],[]=>true
  | (x,y)::xs,(u,v)::ys=>item_eqb x u&&item_eqb y v&&provider_smap_same xs ys
  | _,_=>false end.
Definition provider_promised_result_item (source : st) (profile : reify_exports ety key)
    (promised : key) : item :=
  output_role_item(rx_outputs ety key profile)
    (reify_snapshot_item source(rx_promised ety key profile)promised).
Definition provider_method_alias_of (source : st) (profile : reify_exports ety key)
    (fixed_names : list tsym) (previous current : xbind) : bool :=
  match previous,current with
  | XBMth receiver method theta provider sm,XBMth alias_receiver alias_method alias_theta alias_provider alias_sm=>
      Nat.eqb provider alias_provider&&provider_smap_same sm alias_sm&&
      negb(item_eqb(IMth receiver method theta)(IMth alias_receiver alias_method alias_theta))&&
      existsb(fun promised=>match promised with
        | KyMth _ _=>
            let actual:=provider_actual_result_key profile fixed_names promised in
            match reify_snapshot_item source(rx_actual ety key profile)actual with
            | IMth actual_receiver actual_method _=>
                ty_eqb receiver actual_receiver&&Nat.eqb method actual_method&&
                item_eqb(IMth alias_receiver alias_method alias_theta)
                  (provider_promised_result_item source profile promised)
            | _=>false end
        | _=>false end)(rx_items ety key profile)
  | _,_=>false end.
