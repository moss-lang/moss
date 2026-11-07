(* A generated adapter's future input roles must not capture a same-number
   type variable from the surrounding caller. Quote those meanings into fresh
   captured roles before constructing the adapter's local schemas. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss ReifyData Reify ReifyRoles ReifyFunctors ReifyProviders.
Import ListNotations.

Definition effective_fixed_interpretation (schema : reify_functor_decl) : option subst :=
  provider_fixed_capture_arguments(rfd_fixed schema)(rfd_fixed_inst schema).
Definition normalized_fixed_table (aliases : subst) (schema : reify_functor_decl) : subst :=
  map_fixed_interpretation(rfd_fixed schema)aliases(rfd_fixed_inst schema).
Definition effective_fixed_values (fixed : list tsym) (table : subst) : option(list ty) :=
  match provider_fixed_capture_arguments fixed table with
  | Some arguments=>Some(map snd arguments)|None=>None end.
Definition fixed_capture_variables (left_fixed right_fixed : list tsym)
    (left_table right_table : subst) : option(list tsym) :=
  match effective_fixed_values left_fixed left_table,effective_fixed_values right_fixed right_table with
  | Some left_values,Some right_values=>Some(nodup Nat.eq_dec(flat_map ty_frees(left_values++right_values)))
  | _,_=>None end.
Definition quote_capture_arguments (quotes : subst) : subst :=
  flat_map(fun p=>match snd p with
    | TAbs captured=>[(captured,TAbs(fst p))]|_=>[]end)quotes.
Definition quote_fixed_schema (aliases quotes : subst) (schema : reify_functor_decl)
    : reify_functor_decl :=
  ReifyFunctorDecl(rfd_inputs schema)(rfd_outputs schema)(rfd_fixed schema)
    (map_fixed_interpretation(rfd_fixed schema)quotes(normalized_fixed_table aliases schema))
    (rfd_poly schema)[][].

Lemma quoted_schema_keeps_formals : forall aliases quotes schema,
  rfd_inputs(quote_fixed_schema aliases quotes schema)=rfd_inputs schema /\
  rfd_outputs(quote_fixed_schema aliases quotes schema)=rfd_outputs schema /\
  rfd_fixed(quote_fixed_schema aliases quotes schema)=rfd_fixed schema /\
  rfd_poly(quote_fixed_schema aliases quotes schema)=rfd_poly schema.
Proof. intros;repeat split;reflexivity. Qed.

(* Future-call equations are not lexical captures, including recursive
   same-original interfaces whose formal IDs also occur in an outer scope. *)
Lemma quoted_schema_preserves_poly_equation : forall aliases quotes schema key,
  existsb(Nat.eqb key)(rfd_fixed schema)=false->
  lookup_subst key(rfd_fixed_inst(quote_fixed_schema aliases quotes schema))=
    lookup_subst key(rfd_fixed_inst schema).
Proof.
 intros aliases quotes schema key H;cbn[rfd_fixed_inst quote_fixed_schema].
 rewrite map_fixed_interpretation_lookup,H.
 unfold normalized_fixed_table;rewrite map_fixed_interpretation_lookup,H.
 destruct(lookup_subst key(rfd_fixed_inst schema));reflexivity.
Qed.
