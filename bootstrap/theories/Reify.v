(* Immutable slot allocation for the certified source reifier. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import CoreMoss ReifyData.
Import ListNotations.

Record reify_state : Type := ReifyState {
  rf_next : nat;
  rf_names : subst;
  rf_generations : subst;
  rf_items : smap;
  rf_declarations : list (item * reify_decl)
}.

(* RHSs mention immutable generations.  Rebinding a source name updates only
   the name map; it never overwrites the interpretation of an old generation. *)
Definition lower_type_alias (name : tsym) (rhs : ty) (s : reify_state)
    : tsym * ty * reify_state :=
  (rf_next s, app_subst (rf_generations s) rhs,
   ReifyState (S (rf_next s)) ((name, TAbs (rf_next s)) :: rf_names s)
              (rf_generations s) (rf_items s)
              ((ITy (rf_next s),RDType) :: rf_declarations s)).

Lemma lower_type_alias_preserves_generations : forall name rhs s slot tau s',
    lower_type_alias name rhs s = (slot,tau,s') ->
    rf_generations s' = rf_generations s.
Proof. intros. inversion H. reflexivity. Qed.

Lemma lower_type_alias_fresh : forall name rhs s slot tau s',
    lower_type_alias name rhs s = (slot,tau,s') ->
    slot = rf_next s /\ rf_next s' = S slot /\
    rf_names s' = (name,TAbs slot) :: rf_names s.
Proof. intros. inversion H. repeat split; reflexivity. Qed.

(* Generative application outputs additionally introduce a new source
   generation.  The mapping is persistent, so old source generations retain
   their meanings.  The success proof supplies freshness of [generation]. *)
Definition lower_output_generation (name generation : tsym) (s : reify_state)
    : tsym * reify_state :=
  (rf_next s, ReifyState (S (rf_next s))
    ((name,TAbs (rf_next s)) :: rf_names s)
    ((generation,TAbs (rf_next s)) :: rf_generations s) (rf_items s)
    ((ITy (rf_next s),RDType) :: rf_declarations s)).

Lemma lower_output_generation_preserves_old : forall name generation s slot s' t,
    lower_output_generation name generation s = (slot,s') ->
    t <> generation ->
    lookup_subst t (rf_generations s') = lookup_subst t (rf_generations s).
Proof.
  intros name generation s slot s' t H Hne. inversion H; subst.
  cbn. destruct (Nat.eqb t generation) eqn:E; [|reflexivity].
  apply Nat.eqb_eq in E. contradiction.
Qed.

(* New value provisions get their own immutable declaration slot, so rebinding
   the same source value at a different instantiated type cannot change the
   type of an older provision. *)
Definition lower_value_provision (name : vsym) (inputs : tele) (tau : ty)
    (s : reify_state) : vsym * reify_state :=
  let slot := rf_next s in
  (slot, ReifyState (S slot) (rf_names s) (rf_generations s)
    ((IVal name,IVal slot) :: rf_items s)
    ((IVal slot,RDValue inputs (app_subst (rf_generations s) tau))
       :: rf_declarations s)).

Lemma lower_value_provision_keeps_types : forall name inputs tau s slot s',
    lower_value_provision name inputs tau s = (slot,s') ->
    rf_names s' = rf_names s /\ rf_generations s' = rf_generations s.
Proof. intros. inversion H. split; reflexivity. Qed.

Definition lower_fn_signature (s : reify_state) (signature : fnsig) : fnsig :=
  mkSig (map (app_subst (rf_generations s)) (fs_params signature))
        (app_subst (rf_generations s) (fs_ret signature)).

Definition lower_function_provision (name : fsym) (inputs : tele)
    (signature : fnsig) (s : reify_state) : fsym * reify_state :=
  let slot := rf_next s in
  (slot, ReifyState (S slot) (rf_names s) (rf_generations s)
    ((IFn name,IFn slot) :: rf_items s)
    ((IFn slot,RDFunction inputs (lower_fn_signature s signature))
       :: rf_declarations s)).

(* A fresh method declaration retains only formal parameters used by its
   signature.  Their identity substitution satisfies Core method saturation
   without adding unrelated source context dependencies to the runtime key. *)
Definition lower_method_theta (inputs : tele) (signature : fnsig) : subst :=
  map (fun t => (t,TAbs t))
    (nodup Nat.eq_dec (filter (fun t => existsb (Nat.eqb t) (tyreqs inputs))
      (msig_frees signature))).

Lemma lower_method_theta_domain : forall inputs signature t,
    In t (map fst (lower_method_theta inputs signature)) <->
    In t (msig_frees signature) /\ In t (tyreqs inputs).
Proof.
  intros inputs signature t. unfold lower_method_theta. rewrite map_map. cbn. rewrite map_id.
  change (In t (nodup Nat.eq_dec
    (filter (fun t => existsb (Nat.eqb t) (tyreqs inputs)) (msig_frees signature))) <->
      In t (msig_frees signature) /\ In t (tyreqs inputs)).
  rewrite nodup_In,filter_In. split; intros [Ha Hb]; split; auto.
  - apply existsb_exists in Hb as [x [Hx Heq]]. apply Nat.eqb_eq in Heq. subst. exact Hx.
  - apply existsb_exists. exists t. split; [assumption|apply Nat.eqb_refl].
Qed.
Definition lower_method_provision (original : item) (receiver : ty)
    (inputs : tele) (signature : fnsig) (s : reify_state)
    : item * reify_state :=
  let signature' := lower_fn_signature s signature in
  let target := IMth (app_subst (rf_generations s) receiver) (rf_next s)
    (lower_method_theta inputs signature') in
  (target, ReifyState (S (rf_next s)) (rf_names s) (rf_generations s)
    ((original,target) :: rf_items s)
    ((target,RDMethod inputs (lower_fn_signature s signature))
       :: rf_declarations s)).

(* A plain copied functor retains the provider's complete formal interface;
   changing it would require adapting the actual runtime link chain. *)
Definition lower_functor_copy (name : usym) (schema : reify_functor_decl)
    (s : reify_state) : usym * reify_state :=
  let slot := rf_next s in
  (slot, ReifyState (S slot) (rf_names s) (rf_generations s)
    ((IFun name,IFun slot) :: rf_items s)
    ((IFun slot,RDFunctor schema) :: rf_declarations s)).


Record reify_result : Type := ReifyResult {
  rr_program : xprogram;
  rr_declarations : list (item * reify_decl)
}.

Definition lower_type_temporary (tau : ty) (s : reify_state)
    : tsym * reify_state :=
  (rf_next s, ReifyState (S (rf_next s)) (rf_names s) (rf_generations s)
    (rf_items s) ((ITy (rf_next s),RDType) :: rf_declarations s)).

(* Explicit source theta has exactly one runtime transfer path: fresh typed
   entries in the complete map.  Reified XCallD therefore uses theta=[]. *)
Fixpoint materialize_type_arguments (arguments : subst) (s : reify_state)
    : list (tsym * ty) * smap * reify_state :=
  match arguments with
  | [] => ([],[],s)
  | (formal,tau) :: rest =>
      let '(slot,s1) := lower_type_temporary tau s in
      let '(bindings,sm,s2) := materialize_type_arguments rest s1 in
      ((slot,tau) :: bindings,(ITy formal,ITy slot) :: sm,s2)
  end.
Definition bind_type_temporaries (bindings : list (tsym * ty)) (e : xexpr) : xexpr :=
  fold_right (fun p body => XBindTy (fst p) (snd p) body) e bindings.

Lemma materialize_type_arguments_keeps_scope : forall args s bindings sm s',
    materialize_type_arguments args s = (bindings,sm,s') ->
    rf_names s' = rf_names s /\ rf_generations s' = rf_generations s /\
    rf_items s' = rf_items s.
Proof.
  induction args as [|[formal tau] rest IH]; intros s bindings sm s' H.
  - inversion H. repeat split; reflexivity.
  - cbn in H. destruct (materialize_type_arguments rest
      (ReifyState (S (rf_next s)) (rf_names s) (rf_generations s)
        (rf_items s) ((ITy (rf_next s),RDType) :: rf_declarations s)))
      as [[bindings' sm'] s2] eqn:E.
    inversion H; subst. exact (IH _ _ _ _ E).
Qed.

Definition restore_reify_scope (before after : reify_state) : reify_state :=
  ReifyState (rf_next after) (rf_names before) (rf_generations before)
    (rf_items before) (rf_declarations after).

(* A raw eta wrapper may be reused at different method receiver types. The
   immutable program gives each checked provision its own typed code slot;
   arg0 is the method receiver and the original plain function ignores it. *)
Definition lower_method_eta_wrapper (receiver : ty) (inputs : tele)
    (signature : fnsig) (callee : fsym) (callee_map : smap) (s : reify_state)
    : fsym * xexpr * reify_state :=
  let code := rf_next s in
  let signature' := lower_fn_signature s signature in
  let receiver' := app_subst (rf_generations s) receiver in
  (code, XCallD callee [] callee_map
       (map XVar (seq 1 (length (fs_params signature')))),
   ReifyState (S code) (rf_names s) (rf_generations s) (rf_items s)
     ((IFn code,RDFunction inputs
        (mkSig (receiver' :: fs_params signature') (fs_ret signature')))
        :: rf_declarations s)).

Lemma lower_method_eta_wrapper_arity : forall receiver inputs signature callee sm s code body s',
    lower_method_eta_wrapper receiver inputs signature callee sm s = (code,body,s') ->
    body = XCallD callee [] sm (map XVar (seq 1 (length (fs_params signature)))) /\
    code = rf_next s /\ rf_next s' = S code.
Proof.
  intros. inversion H; subst. unfold lower_fn_signature. cbn.
  rewrite map_length. repeat split; reflexivity.
Qed.

Lemma materialize_type_arguments_structure : forall arguments before bindings sm after,
  materialize_type_arguments arguments before=(bindings,sm,after)->
  bindings=combine(seq(rf_next before)(List.length arguments))(map snd arguments) /\
  sm=combine(map(fun p=>ITy(fst p))arguments)
    (map ITy(seq(rf_next before)(List.length arguments))) /\
  rf_next after=rf_next before+List.length arguments.
Proof.
 induction arguments as[|[formal tau]rest IH];intros before bindings sm after H.
 - cbn in H;inversion H;cbn;repeat split;lia.
 - cbn[materialize_type_arguments lower_type_temporary]in H.
   destruct(materialize_type_arguments rest
     (ReifyState(S(rf_next before))(rf_names before)(rf_generations before)
       (rf_items before)((ITy(rf_next before),RDType)::rf_declarations before)))
     as[[tail tailmap]final]eqn:E.
   inversion H;subst.
   destruct(IH _ _ _ _ E)as[B[M N]]. cbn in B,M,N.
   cbn[List.length map seq combine]. split;[now rewrite B|].
   split;[now rewrite M|lia].
Qed.

Lemma materialized_types_are_fresh : forall arguments before bindings sm after slot tau,
  materialize_type_arguments arguments before=(bindings,sm,after)->
  In(slot,tau)bindings->rf_next before<=slot<rf_next after.
Proof.
 intros arguments before bindings sm after slot tau H Hin.
 destruct(materialize_type_arguments_structure _ _ _ _ _ H)as[B[_ N]].
 rewrite B in Hin;apply in_combine_l in Hin;apply in_seq in Hin;lia.
Qed.
