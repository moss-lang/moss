(* Binding recipes capture the OLD environment before installing the new
   immutable provision. This matters when a provider reads the very source
   function name being rebound. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel CoreMoss Elab BodyTr ReifyData Reify ReifySource
  ReifyInterfaces ReifyTransfers ReifyPlan ReifyAnnotations ReifyWalkData ReifyEta.
Import ListNotations.

Definition annotation_raw_signature (source : st) (signature : reify_signature ety) : fnsig :=
  mkSig (map (tr (reify_field_number source)) (rsg_params ety signature))
    (tr (reify_field_number source) (rsg_result ety signature)).
Definition annotation_raw_arguments (source : st) (arguments : list (tsym * ety)) : subst :=
  map (fun p => (fst p,tr (reify_field_number source) (snd p))) arguments.

Definition prepare_value_binding (source : st) (events : list (reify_event ety key))
    (name : vsym) (before : reify_local) : option (vsym * reify_local) :=
  match find_value_annotation source (rl_aliases before) (rl_runtime before) name events with
  | None => None
  | Some raw =>
      let tau := tr (reify_field_number source) raw in
      let deps := value_type_inputs (ExecutionModel.app_subst (rf_generations (rl_runtime before)) tau) in
      let '(slot,after) := lower_value_provision name deps tau (rl_runtime before) in
      Some (slot,ReifyLocal after (rl_aliases before))
  end.

Definition prepare_function_binding (source : st) (defaults : subst)
    (frames : list reify_function_frame) (events : list (reify_event ety key))
    (name provider : fsym) (source_map : smap) (before : reify_local)
    : option (list xbind * reify_local) :=
  match find_reify_annotation
      (function_annotation_matches source (rl_aliases before) (rl_runtime before) name provider) events,
      lookup_function_frame provider frames with
  | Some (REFunction _ _ _ _ signature arguments),Some frame =>
      match rff_pending_functors frame with
      | _ :: _ => None
      | [] =>
          match prepare_frame_transfer source frame
              source_map (annotation_raw_arguments source arguments) (rl_runtime before) with
          | None => None
          | Some (temporaries,sm,allocated) =>
              let raw := annotation_raw_signature source signature in
              let deps := signature_type_inputs (lower_fn_signature allocated raw) in
              let '(slot,after) := lower_function_provision name deps raw allocated in
              Some (map (fun p => XBTy (fst p) (snd p)) temporaries ++
                [XBFn slot (rff_code frame) (global_default_transfer defaults ++ sm)],
                ReifyLocal after (aliases_after_temporaries temporaries (rl_aliases before)))
          end
      end
  | _,_ => None end.

Lemma prepared_value_preserves_type_view : forall source events name before slot after,
    prepare_value_binding source events name before = Some (slot,after) ->
    rl_aliases after = rl_aliases before /\
    rf_names (rl_runtime after) = rf_names (rl_runtime before) /\
    rf_generations (rl_runtime after) = rf_generations (rl_runtime before).
Proof.
  intros source events name before slot after H. unfold prepare_value_binding in H.
  destruct (find_value_annotation _ _ _ _ _) as [raw|]; [|discriminate].
  inversion H; subst. repeat split; reflexivity.
Qed.

Example function_binding_captures_old_provision : forall source,
  let scope := ReifyScope ety key [] [] [] [] [] [] [] in
  let signature := ReifySignature ety [] YUnit None in
  let frame := ReifyFunctionFrame 43 100 scope [] [(IFn 1,IFn 200)]
    [IFn 200] (mkSig [] TUnit) [] (ReifyState 250 [] [] [] []) in
  let before := ReifyLocal (ReifyState 300 [] [] [(IFn 1,IFn 50)] []) [] in
  prepare_function_binding source [] [frame]
    [REFunction ety key 1 43 signature []] 1 43 [(IFn 1,IFn 1)] before =
    Some ([XBFn 300 100 [(IFn 200,IFn 50)]],
      ReifyLocal (ReifyState 301 [] [] [(IFn 1,IFn 300);(IFn 1,IFn 50)]
        [(IFn 300,RDFunction [] (mkSig [] TUnit))]) []).
Proof. intro source. reflexivity. Qed.

Definition prepare_direct_method_binding (source : st) (defaults : subst)
    (frames : list reify_function_frame) (events : list (reify_event ety key))
    (receiver : ty) (method : msym) (theta : subst) (provider : fsym)
    (source_map : smap) (before : reify_local) : option (list xbind * reify_local) :=
  match find_reify_annotation
      (method_annotation_matches source (rl_aliases before) (rl_runtime before)
        receiver method theta provider) events,
      lookup_function_frame provider frames with
  | Some (REMethod _ _ _ _ _ signature arguments),Some frame =>
      match rff_pending_functors frame with
      | _::_ => None
      | [] => match prepare_frame_transfer source frame
            source_map (annotation_raw_arguments source arguments) (rl_runtime before) with
          | None => None
          | Some (temporaries,sm,allocated) =>
              let raw := annotation_raw_signature source signature in
              let deps := signature_type_inputs (lower_fn_signature allocated raw) in
              let '(target,after) := lower_method_provision (IMth receiver method theta)
                receiver deps raw allocated in
              match target with
              | IMth actual_receiver slot actual_theta =>
                  Some (map (fun p => XBTy (fst p) (snd p)) temporaries ++
                    [XBMth actual_receiver slot actual_theta (rff_code frame)
                      (global_default_transfer defaults ++ sm)],
                    ReifyLocal after (aliases_after_temporaries temporaries (rl_aliases before)))
              | _ => None end
          end
      end
  | _,_ => None end.

Definition prepare_eta_method_binding (source : st) (defaults : subst)
    (frames : list reify_function_frame) (events : list (reify_event ety key))
    (receiver : ty) (method : msym) (theta : subst) (provider : fsym)
    (source_map : smap) (before : reify_local)
    : option (list xbind * reify_local * list (fsym * xexpr)) :=
  match find_reify_annotation
      (method_annotation_matches source (rl_aliases before) (rl_runtime before)
        receiver method theta provider) events,source_eta_callee source provider with
  | Some (REMethod _ _ _ _ _ signature arguments),Some (callee,eta_args) =>
      match lookup_function_frame callee frames with
      | None => None
      | Some frame => match rff_pending_functors frame with
          | _::_ => None
          | [] =>
              if Nat.eqb (length eta_args) (length (fs_params (rff_signature frame))) &&
                 eta_arguments_from 1 eta_args then
                match prepare_frame_transfer source frame
                    source_map (annotation_raw_arguments source arguments) (rl_runtime before) with
                | None => None
                | Some (temporaries,sm,allocated) =>
                    let wrapper := reserve_eta_wrapper receiver frame allocated in
                    let raw := annotation_raw_signature source signature in
                    let deps := signature_type_inputs (lower_fn_signature (ewp_after wrapper) raw) in
                    let '(target,after) := lower_method_provision (IMth receiver method theta)
                        receiver deps raw (ewp_after wrapper) in
                    match target with
                    | IMth actual_receiver slot actual_theta =>
                        Some (map (fun p => XBTy (fst p) (snd p)) temporaries ++
                          [XBMth actual_receiver slot actual_theta (ewp_code wrapper)
                            (ewp_receiver_map wrapper ++ global_default_transfer defaults ++ sm)],
                          ReifyLocal after (aliases_after_temporaries temporaries (rl_aliases before)),
                          [(ewp_code wrapper,ewp_body wrapper)])
                    | _ => None end
                end
              else None
          end
      end
  | _,_ => None end.

Definition prepare_method_binding (source : st) (defaults : subst)
    (frames : list reify_function_frame) (events : list (reify_event ety key))
    (receiver : ty) (method : msym) (theta : subst) (provider : fsym)
    (source_map : smap) (before : reify_local)
    : option (list xbind * reify_local * list (fsym * xexpr)) :=
  match prepare_direct_method_binding source defaults frames events
      receiver method theta provider source_map before with
  | Some (bindings,after) => Some (bindings,after,[])
  | None => prepare_eta_method_binding source defaults frames events
      receiver method theta provider source_map before end.
