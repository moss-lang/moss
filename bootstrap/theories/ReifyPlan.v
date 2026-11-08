(* Code-frame allocation precedes body traversal, so mutually recursive
   calls refer to already reserved immutable code and context slots. The
   remaining functor items are explicit work for schema/view preparation. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import ExecutionModel CoreFragment Elab ReifyData Reify ReifySource ReifyInterfaces ReifyRoles ReifyReach ReifyInitial ReifyMaps ReifyTransfers.
Import ListNotations.

Record reify_function_frame : Type := ReifyFunctionFrame {
  rff_source : fsym;
  rff_code : fsym;
  rff_source_scope : reify_scope ety key;
  rff_type_roles : subst;
  rff_item_roles : smap;
  rff_inputs : tele;
  rff_signature : fnsig;
  rff_pending_functors : list key;
  rff_scope : reify_state
}.

Definition type_role_telescope (roles : subst) : tele :=
  flat_map (fun p => match snd p with TAbs role => [ITy role] | _ => [] end) roles.
Definition source_functor_items (scope : reify_scope ety key) : list key :=
  filter (fun k => match k with KyFun _ => true | _ => false end) (rs_items ety key scope).

Definition prepare_function_frame (source : st) (original : fsym)
    (scope : reify_scope ety key) (signature : reify_signature ety)
    (global : reify_state) : reify_function_frame :=
  let code := rf_next global in
  let blank := ReifyState (S code) [] [] [] (rf_declarations global) in
  let types := lower_source_type_interface source scope blank in
  let '(items,local) := lower_scope_first_order source scope (sti_state types) in
  ReifyFunctionFrame original code scope (sti_roles types) (rev (rf_items local))
    (type_role_telescope (sti_roles types) ++ items)
    (reify_source_code_signature source local signature)
    (source_functor_items scope) local.

(* A frame's declaration is installed only after its pending functor views
   are resolved. They append actual dictionary items, not assumptions. *)
Definition complete_function_frame (frame : reify_function_frame)
    (functor_items : tele) (local : reify_state) : reify_function_frame :=
  ReifyFunctionFrame (rff_source frame) (rff_code frame) (rff_source_scope frame)
    (rff_type_roles frame) (rev (rf_items local)) (rff_inputs frame ++ functor_items)
    (rff_signature frame) [] local.
Definition declare_function_frame (frame : reify_function_frame) (global : reify_state) : reify_state :=
  ReifyState (rf_next global) (rf_names global) (rf_generations global) (rf_items global)
    ((IFn (rff_code frame),RDFunction (rff_inputs frame) (rff_signature frame)) :: rf_declarations global).
Definition enter_function_frame (frame : reify_function_frame) (global : reify_state) : reify_state :=
  restore_reify_scope (rff_scope frame) global.

Fixpoint lookup_function_frame (source : fsym) (frames : list reify_function_frame)
    : option reify_function_frame :=
  match frames with
  | [] => None
  | frame :: rest => if Nat.eqb source (rff_source frame) then Some frame
      else lookup_function_frame source rest
  end.

Lemma enter_function_frame_retains_global_allocation : forall frame global,
    rf_next (enter_function_frame frame global) = rf_next global /\
    rf_declarations (enter_function_frame frame global) = rf_declarations global /\
    rf_generations (enter_function_frame frame global) = rf_generations (rff_scope frame) /\
    rf_names (enter_function_frame frame global) = rf_names (rff_scope frame).
Proof. intros. repeat split; reflexivity. Qed.

Lemma completed_function_frame_inputs : forall frame functors local,
    rff_inputs (complete_function_frame frame functors local) = rff_inputs frame ++ functors /\
    rff_pending_functors (complete_function_frame frame functors local) = [].
Proof. intros. split; reflexivity. Qed.

(* Reserve every code/context frame before rewriting any body.  Recursive
   calls then consult this complete table rather than allocating a second
   meaning for the callee when it is first encountered. *)
Fixpoint prepare_function_frames (source : st)
    (specifications : list (fsym * reify_scope ety key * reify_signature ety))
    (global : reify_state) : list reify_function_frame * reify_state :=
  match specifications with
  | [] => ([],global)
  | (original,scope,signature) :: rest =>
      let frame := prepare_function_frame source original scope signature global in
      let next := restore_reify_scope global (rff_scope frame) in
      let '(frames,after) := prepare_function_frames source rest next in
      (frame :: frames,after)
  end.

Lemma prepare_function_frames_sources : forall source specifications global frames after,
    prepare_function_frames source specifications global = (frames,after) ->
    map rff_source frames = map (fun p => let '(f,_,_) := p in f) specifications.
Proof.
  induction specifications as [|[[original scope] signature] rest IH];
    intros global frames after H; cbn in H.
  - inversion H. reflexivity.
  - destruct (prepare_function_frames source rest _) as [tail final] eqn:E.
    inversion H; subst. cbn. f_equal.
    + unfold prepare_function_frame. destruct (lower_scope_first_order _ _ _). reflexivity.
    + eapply IH. exact E.
Qed.

Lemma prepare_function_frames_preserves_outer_scope : forall source specifications global frames after,
    prepare_function_frames source specifications global = (frames,after) ->
    rf_names after = rf_names global /\
    rf_generations after = rf_generations global /\ rf_items after = rf_items global.
Proof.
  induction specifications as [|[[original scope] signature] rest IH];
    intros global frames after H; cbn in H.
  - inversion H. repeat split; reflexivity.
  - destruct (prepare_function_frames source rest _) as [tail final] eqn:E.
    inversion H; subst. exact (IH _ _ _ E).
Qed.

(* Hidden default roles are physical input entries, separate from source
   formals. Their transfer is identity, never an application of source theta. *)
Definition attach_function_defaults (defaults : subst) (frame : reify_function_frame)
    : reify_function_frame :=
  ReifyFunctionFrame (rff_source frame) (rff_code frame) (rff_source_scope frame)
    (rff_type_roles frame) (rff_item_roles frame)
    (type_role_telescope defaults ++ rff_inputs frame) (rff_signature frame)
    (rff_pending_functors frame) (install_generation_defaults defaults (rff_scope frame)).
Definition global_default_transfer (defaults : subst) : smap :=
  identity_smap (type_role_telescope defaults).

Lemma attached_defaults_preserve_formals : forall defaults frame,
    rff_type_roles (attach_function_defaults defaults frame) = rff_type_roles frame /\
    rff_item_roles (attach_function_defaults defaults frame) = rff_item_roles frame /\
    rf_names (rff_scope (attach_function_defaults defaults frame)) = rf_names (rff_scope frame).
Proof. intros. repeat split; reflexivity. Qed.

Definition frame_name_patterns (source : st) (frame : reify_function_frame) : subst :=
  map (fun p => (fst p,reify_snapshot_type source (rff_source_scope frame) (snd p)))
    (rs_names ety key (rff_source_scope frame)).
Definition prepare_frame_transfer (source : st) (frame : reify_function_frame)
    (source_map : smap) (arguments : subst) (before : reify_state) :=
  prepare_pattern_transfer (frame_name_patterns source frame) (rff_type_roles frame)
    (rff_item_roles frame) source_map arguments before.

Definition lower_frame_call (source : st) (defaults : subst) (callee : reify_function_frame)
    (source_map : smap) (arguments : subst) (args : list xexpr)
    (before : reify_state) : option (xexpr * reify_state) :=
  match prepare_frame_transfer source callee source_map arguments before with
  | Some (bindings,sm,after) =>
      Some (bind_type_temporaries bindings
        (XCallD (rff_code callee) [] (global_default_transfer defaults ++ sm) args),after)
  | None => None end.

Lemma frame_call_defaults_are_not_instantiated : forall source defaults callee source_map arguments args before e after,
    lower_frame_call source defaults callee source_map arguments args before = Some (e,after) ->
    exists bindings sm,
      e = bind_type_temporaries bindings
        (XCallD (rff_code callee) [] (global_default_transfer defaults ++ sm) args) /\
      rf_names after = rf_names before /\
      rf_generations after = rf_generations before /\ rf_items after = rf_items before.
Proof.
  intros source defaults callee source_map arguments args before e after H.
  unfold lower_frame_call, prepare_frame_transfer in H.
  destruct (prepare_pattern_transfer _ _ _ _ _ _) as [[[bindings sm] middle]|] eqn:E;
    [|discriminate].
  inversion H; subst. exists bindings,sm. split; [reflexivity|].
  exact (prepare_pattern_transfer_preserves_scope _ _ _ _ _ _ _ _ _ E).
Qed.

Lemma prepared_function_item_order : forall source original scope signature global,
    let frame := prepare_function_frame source original scope signature global in
    exists items,
      rff_inputs frame = type_role_telescope (rff_type_roles frame) ++ items /\
      map snd (rff_item_roles frame) = items.
Proof.
  intros source original scope signature global. unfold prepare_function_frame.
  destruct (lower_scope_first_order source scope _) as [items local] eqn:E.
  exists items. split; [reflexivity|]. cbn [rff_item_roles].
  rewrite (lower_scope_first_order_item_order _ _ _ _ _ E),
    lower_source_type_interface_preserves_items. reflexivity.
Qed.
