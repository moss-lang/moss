(* Actual immutable expression traversal. The first-order cases use complete
   environment transfers and reusable checked source annotations. Functor
   builders are integrated separately; no raw fallback is executed here. *)
From Stdlib Require Import List Arith.
From NM Require Import ExecutionModel CoreMoss Elab ReifyData Reify ReifySource ReifyPlan
  ReifyAnnotations ReifyWalkData ReifyBindings ReifyFunctors ReifyPrimitive ReifyProviders ReifyAdapters.
Import ListNotations.

Record walk_config : Type := WalkConfig {
  cfg_source : st;
  cfg_defaults : subst;
  cfg_frames : list reify_function_frame;
  cfg_code : reify_code ety key
}.
Record walk_cursor : Type := WalkCursor {
  walk_local : reify_local;
  walk_functors : functor_work;
  walk_auxiliary : list (fsym * xexpr)
}.
Definition set_walk_local (local : reify_local) (cursor : walk_cursor) : walk_cursor :=
  WalkCursor local (walk_functors cursor) (walk_auxiliary cursor).
Definition restore_walk_scope (before after : walk_cursor) : walk_cursor :=
  WalkCursor (restore_reify_local (walk_local before) (walk_local after))
    (walk_functors after) (walk_auxiliary after).
Definition finish_walk (before : walk_cursor) (body : xexpr) (after : walk_cursor)
    : option (xexpr * walk_cursor) := Some (body,restore_walk_scope before after).
Definition walk_theta (before : walk_cursor) (theta : subst) : subst :=
  map (fun p => (fst p,ExecutionModel.app_subst
    (rf_generations (rl_runtime (walk_local before))) (snd p))) theta.

Definition bind_expression (binding : xbind) (body : xexpr) : xexpr :=
  match binding with
  | XBTy t tau => XBindTy t tau body
  | XBVal v e => XBindVal v e body
  | XBFn f code sm => XBindFn f code sm body
  | XBMth rt m th code sm => XBindMth rt m th code sm body
  | XBFun u h sm smc => XBindFun u h sm smc body
  | XBFunA u w sm => XBindFunA u w sm body
  | XBApply u th sm => XApply u th sm body
  | XBApplyR u inputs outputs => XApplyR u inputs outputs body
  | XBFunSym u h => XBindFunSym u h body
  | XBFunCopy u w => XBindFunCopy u w body
  | XBApplyBr u branches => XApplyBr u branches body
  | XBPrim item primitive => XBindPrim item primitive body
  end.
Definition bind_expressions (bindings : list xbind) (body : xexpr) : xexpr :=
  fold_right bind_expression body bindings.

Definition walk_option_bind {A B} (a : option A) (f : A -> option B) : option B :=
  match a with Some x => f x | None => None end.
Local Notation "'let?' x ':=' m 'in' k" := (walk_option_bind m (fun x => k))
  (at level 200,x pattern,m at level 100,k at level 200).
Fixpoint map_walk {A B} (step : A -> walk_cursor -> option (B * walk_cursor))
    (xs : list A) (before : walk_cursor) : option (list B * walk_cursor) :=
  match xs with
  | [] => Some ([],before)
  | x :: rest =>
      let? (y,middle) := step x before in
      let? (ys,after) := map_walk step rest middle in Some (y::ys,after)
  end.

(* A cached initializer carries its own explicit source occurrence marker.
   Its source checker starts in empty_env, independently of the reader's
   mutable names. Original generations retain their global default roles. *)
Definition constant_walk_scope (defaults : subst) (before : walk_cursor) : walk_cursor :=
  let runtime := rl_runtime (walk_local before) in
  set_walk_local (ReifyLocal
    (ReifyState (rf_next runtime) [] defaults [] (rf_declarations runtime)) []) before.
Lemma constant_scope_has_only_global_generations : forall defaults before,
    rf_names (rl_runtime (walk_local (constant_walk_scope defaults before))) = [] /\
    rf_generations (rl_runtime (walk_local (constant_walk_scope defaults before))) = defaults /\
    rf_items (rl_runtime (walk_local (constant_walk_scope defaults before))) = [] /\
    rl_aliases (walk_local (constant_walk_scope defaults before)) = [].
Proof. intros. repeat split; reflexivity. Qed.
Definition constant_walk_config (config : walk_config) (id : nat) : option walk_config :=
  match assoc Nat.eqb id (rm_constants (st_reify (cfg_source config))) with
  | None => None
  | Some events => Some (WalkConfig (cfg_source config) (cfg_defaults config)
      (cfg_frames config) (ReifyCode ety key (rc_owner ety key (cfg_code config))
        (ReifyScope ety key [] [] [] [] [] [] []) [] None None events))
  end.

Fixpoint reify_expression (config : walk_config) (e : xexpr) (before : walk_cursor)
    {struct e} : option (xexpr * walk_cursor) :=
  match e with
  | XConst id initializer =>
      let? own := constant_walk_config config id in
      let? (lowered,after) := reify_expression own initializer
        (constant_walk_scope (cfg_defaults config) before) in
      finish_walk before lowered after
  | XVar v => Some (XVar v,before)
  | XUnit => Some (XUnit,before)
  | XBreak => Some (XBreak,before)
  | XVl v => match lookup_smap (IVal v) (rf_items (rl_runtime (walk_local before))) with
      | Some (IVal slot) => Some (XVl slot,before) | _ => None end
  | XTag n theta payload =>
      let? (payload',after) := reify_expression config payload before in
      finish_walk before (XTag n (walk_theta before theta) payload') after
  | XCallA f args =>
      let? (args',after) := map_walk (reify_expression config) args before in
      match lookup_smap (IFn f) (rf_items (rl_runtime (walk_local before))) with
      | Some (IFn slot) => finish_walk before (XCallA slot args') after
      | _ => None end
  | XCallD f theta sm args =>
      let? (args',after) := map_walk (reify_expression config) args before in
      match lookup_function_frame f (cfg_frames config) with
      | None => None
      | Some frame => match rff_pending_functors frame with
          | _::_ => None
          | [] => let? (call,allocated) := lower_frame_call (cfg_source config) (cfg_defaults config) frame
              sm theta args' (rl_runtime (walk_local after)) in
              finish_walk before call (set_walk_local
                (ReifyLocal allocated (rl_aliases (walk_local after))) after)
          end
      end
  | XMeth receiver _ item args =>
      let? (receiver',middle) := reify_expression config receiver before in
      let? (args',after) := map_walk (reify_expression config) args middle in
      match lookup_smap item (rf_items (rl_runtime (walk_local before))) with
      | Some (IMth rt method theta) =>
          finish_walk before (XMeth receiver' method (IMth rt method theta) args') after
      | _ => None end
  | XLet v value body =>
      let? (value',middle) := reify_expression config value before in
      let? (body',after) := reify_expression config body middle in
      finish_walk before (XLet v value' body') after
  | XBindTy name raw body =>
      let '(slot,meaning,local) := local_type_alias name raw (walk_local before) in
      let? (body',after) := reify_expression config body (set_walk_local local before) in
      finish_walk before (XBindTy slot meaning body') after
  | XBindVal name value body =>
      let? (value',middle) := reify_expression config value before in
      let? (slot,local) := prepare_value_binding (cfg_source config)
          (rc_events ety key (cfg_code config)) name (walk_local middle) in
      let? (body',after) := reify_expression config body (set_walk_local local middle) in
      finish_walk before (XBindVal slot value' body') after
  | XBindFn name provider sm body =>
      let? (bindings,local) := prepare_function_binding (cfg_source config) (cfg_defaults config)
          (cfg_frames config) (rc_events ety key (cfg_code config)) name provider sm (walk_local before) in
      let? (body',after) := reify_expression config body (set_walk_local local before) in
      finish_walk before (bind_expressions bindings body') after
  | XBindMth receiver method theta provider sm body =>
      let? (bindings,local,auxiliary) := prepare_method_binding (cfg_source config) (cfg_defaults config)
          (cfg_frames config) (rc_events ety key (cfg_code config))
          receiver method theta provider sm (walk_local before) in
      let entered := WalkCursor local (walk_functors before) (auxiliary ++ walk_auxiliary before) in
      let? (body',after) := reify_expression config body entered in
      finish_walk before (bind_expressions bindings body') after
  | XBindFun name provider linked captured body =>
      let? (bindings,local,work) := lower_defined_functor (cfg_source config)
          (cfg_code config) (cfg_defaults config) name provider linked captured
          (walk_local before) (walk_functors before) in
      let entered := WalkCursor local work (walk_auxiliary before) in
      let? (body',after) := reify_expression config body entered in
      finish_walk before (bind_expressions bindings body') after
  | XBindFunA name provider sm body =>
      let? (bindings,local,work) := lower_abstract_functor (cfg_source config)
          (cfg_code config) (cfg_defaults config) name provider sm
          (walk_local before) (walk_functors before) in
      let entered := WalkCursor local work (walk_auxiliary before) in
      let? (body',after) := reify_expression config body entered in
      finish_walk before (bind_expressions bindings body') after
  | XApply name theta sm body =>
      let? (bindings,local,work) := lower_functor_application (cfg_source config)
          (cfg_code config) (cfg_defaults config) name theta sm
          (walk_local before) (walk_functors before) in
      let entered := WalkCursor local work (walk_auxiliary before) in
      let? (body',after) := reify_expression config body entered in
      finish_walk before (bind_expressions bindings body') after
  | XBindPrim item primitive body =>
      let? (binding,local) := prepare_primitive_binding (cfg_source config)
          (cfg_code config) item primitive (walk_local before) in
      let? (body',after) := reify_expression config body (set_walk_local local before) in
      finish_walk before (bind_expression binding body') after
  | XSeq a b =>
      let? (a',middle) := reify_expression config a before in
      let? (b',after) := reify_expression config b middle in
      finish_walk before (XSeq a' b') after
  | XIf condition yes no =>
      let? (condition',s1) := reify_expression config condition before in
      let? (yes',s2) := reify_expression config yes s1 in
      let? (no',after) := reify_expression config no s2 in
      finish_walk before (XIf condition' yes' no') after
  | XWhile condition body =>
      let? (condition',middle) := reify_expression config condition before in
      let? (body',after) := reify_expression config body middle in
      finish_walk before (XWhile condition' body') after
  | XLoop body => let? (body',after) := reify_expression config body before in
      finish_walk before (XLoop body') after
  | XReturn value => let? (value',after) := reify_expression config value before in
      finish_walk before (XReturn value') after
  | XAssign v value => let? (value',after) := reify_expression config value before in
      finish_walk before (XAssign v value') after
  | XField value field => let? (value',after) := reify_expression config value before in
      finish_walk before (XField value' field) after
  | XRec fields =>
      let? (fields',after) := map_walk (fun p current =>
          let? (body,next) := reify_expression config (snd p) current in
          Some ((fst p,body),next)) fields before in
      finish_walk before (XRec fields') after
  | XMatch scrutinee arms =>
      let? (scrutinee',middle) := reify_expression config scrutinee before in
      let? (arms',after) := map_walk (fun arm current =>
          let '(tag,v,body) := arm in
          let? (body',next) := reify_expression config body current in
          Some ((tag,v,body'),next)) arms middle in
      finish_walk before (XMatch scrutinee' arms') after
  | XMatchP scrutinee arms =>
      let? (scrutinee',middle) := reify_expression config scrutinee before in
      let? (arms',after) := map_walk (fun arm current =>
          let? (body',next) := reify_expression config (snd arm) current in
          Some ((fst arm,body'),next)) arms middle in
      finish_walk before (XMatchP scrutinee' arms') after
  (* Remaining source functor/method cases use their actual frame/view
     builders. Optimizer-only syntax and already lowered R applications
     are not raw source input to this stage. *)
  | XApplyR _ _ _ _ | XBindFunSym _ _ _
  | XBindFunCopy _ _ _ | XApplyBr _ _ _ => None
  end.

Definition walk_scope_same (before after : walk_cursor) : Prop :=
  rf_names (rl_runtime (walk_local after)) = rf_names (rl_runtime (walk_local before)) /\
  rf_generations (rl_runtime (walk_local after)) = rf_generations (rl_runtime (walk_local before)) /\
  rf_items (rl_runtime (walk_local after)) = rf_items (rl_runtime (walk_local before)) /\
  rl_aliases (walk_local after) = rl_aliases (walk_local before).
Lemma restored_walk_scope_same : forall before after,
    walk_scope_same before (restore_walk_scope before after).
Proof. intros. repeat split; reflexivity. Qed.

Lemma reify_expression_preserves_lexical_scope : forall config e before result after,
    reify_expression config e before = Some (result,after) ->
    walk_scope_same before after.
Proof.
  intros config e before result after H. destruct e;
    cbn [reify_expression] in H;
    unfold finish_walk in H;
    repeat first [
      match type of H with
      | walk_option_bind ?operation _ = _ =>
          destruct operation; cbn [walk_option_bind] in H; try discriminate
      end |
      match type of H with
      | (match ?operation with _ => _ end) = _ =>
          destruct operation; cbn [walk_option_bind] in H; try discriminate
      end ];
    try discriminate;
    inversion H; subst; repeat split; reflexivity.
Qed.

Example type_and_value_rebind_have_distinct_immutable_slots : forall source,
  let scope := ReifyScope ety key [] [] [] [] [] [] [] in
  let code := ReifyCode ety key (ROFunction 1) scope [] None None
    [REValue ety key 8 (YNom 7 []) [(1,YNom 7 [])]] in
  let config := WalkConfig source [] [] code in
  let before := WalkCursor (ReifyLocal (ReifyState 100 [] [] [] []) [])
    empty_functor_work [] in
  reify_expression config
    (XBindTy 1 (TNom 7 []) (XBindVal 8 (XTag 7 [] XUnit) (XVl 8))) before =
  Some (XBindTy 100 (TNom 7 []) (XBindVal 101 (XTag 7 [] XUnit) (XVl 101)),
    WalkCursor (ReifyLocal
      (ReifyState 102 [] [] []
        [(IVal 101,RDValue [] (TNom 7 []));(ITy 100,RDType)]) [])
      empty_functor_work []).
Proof. intro source. reflexivity. Qed.


Definition reify_binding (config : walk_config) (binding : xbind) (before : walk_cursor)
    : option (list xbind * walk_cursor) :=
  match binding with
  | XBTy name raw =>
      let '(slot,meaning,local) := local_type_alias name raw (walk_local before) in
      Some ([XBTy slot meaning],set_walk_local local before)
  | XBVal name value =>
      let? (value',middle) := reify_expression config value before in
      let? (slot,local) := prepare_value_binding (cfg_source config)
          (rc_events ety key (cfg_code config)) name (walk_local middle) in
      Some ([XBVal slot value'],set_walk_local local middle)
  | XBFn name provider sm =>
      let? (bindings,local) := prepare_function_binding (cfg_source config) (cfg_defaults config)
          (cfg_frames config) (rc_events ety key (cfg_code config)) name provider sm (walk_local before) in
      Some (bindings,set_walk_local local before)
  | XBMth receiver method theta provider sm =>
      let? (bindings,local,auxiliary) := prepare_method_binding (cfg_source config) (cfg_defaults config)
          (cfg_frames config) (rc_events ety key (cfg_code config))
          receiver method theta provider sm (walk_local before) in
      Some (bindings,WalkCursor local (walk_functors before) (auxiliary ++ walk_auxiliary before))
  | XBFun name provider linked captured =>
      let? (bindings,local,work) := lower_defined_functor (cfg_source config)
          (cfg_code config) (cfg_defaults config) name provider linked captured
          (walk_local before) (walk_functors before) in
      Some (bindings,WalkCursor local work (walk_auxiliary before))
  | XBFunA name provider sm =>
      let? (bindings,local,work) := lower_abstract_functor (cfg_source config)
          (cfg_code config) (cfg_defaults config) name provider sm
          (walk_local before) (walk_functors before) in
      Some (bindings,WalkCursor local work (walk_auxiliary before))
  | XBApply name theta sm =>
      let? (bindings,local,work) := lower_functor_application (cfg_source config)
          (cfg_code config) (cfg_defaults config) name theta sm
          (walk_local before) (walk_functors before) in
      Some (bindings,WalkCursor local work (walk_auxiliary before))
  | XBPrim item primitive =>
      let? (binding,local) := prepare_primitive_binding (cfg_source config)
          (cfg_code config) item primitive (walk_local before) in
      Some ([binding],set_walk_local local before)
  | XBApplyR _ _ _
  | XBFunSym _ _ | XBFunCopy _ _ | XBApplyBr _ _ => None
  end.
Fixpoint reify_bindings (config : walk_config) (bindings : list xbind) (before : walk_cursor)
    : option (list xbind * walk_cursor) :=
  match bindings with
  | [] => Some ([],before)
  | binding :: rest =>
      let? (prefix,middle) := reify_binding config binding before in
      let? (suffix,after) := reify_bindings config rest middle in
      Some (prefix ++ suffix,after)
  end.

Example constant_marker_ignores_reader_extra_type_name : forall source,
  let signature := ReifySignature ety [] YUnit None in
  let scope := ReifyScope ety key [] [] [] [] [] [] [] in
  let frame := ReifyFunctionFrame 43 100 scope [] [] [] (mkSig [] TUnit) []
    (ReifyState 250 [] [] [] []) in
  let own_events := [REFunction ety key 1 43 signature []] in
  let annotated := s_reify (fun metadata => ReifyMetadata
    (rm_owner metadata) (rm_codes metadata) (rm_declarations metadata)
    [(8,own_events)]) source in
  let code := ReifyCode ety key (ROFunction 2) scope [] None None [] in
  let config := WalkConfig annotated [] [frame] code in
  let before := WalkCursor
    (ReifyLocal (ReifyState 300 [(9,TNom 7 [])] [(9,TAbs 40)] [] []) [])
    empty_functor_work [] in
  option_map fst (reify_expression config
    (XConst 8 (XBindFn 1 43 [] XUnit)) before) =
    Some (XBindFn 300 100 [] XUnit).
Proof. intro source. reflexivity. Qed.
