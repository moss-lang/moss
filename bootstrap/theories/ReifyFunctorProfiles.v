(* Canonical interfaces are prepared before provider bodies are traversed.
   A copied abstract functor obtains its own checked generic profile from
   the actual copy annotation, without borrowing the provider's schema. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel CoreMoss Sx Elab ReifyData Reify ReifySource
  ReifyInterfaces ReifyRoles ReifyFunctors ReifyPlan.
Import ListNotations.

Definition owner_functor_target (owner : reify_owner) : option usym :=
  match owner with ROPlatform target | ROLink target _ => Some target
  | ROFunction _ => None end.
Definition event_functor_profile (event : reify_event ety key)
    : list (usym * reify_exports ety key) :=
  match event with REFunctor _ _ target _ _ _ (Some profile) => [(target,profile)]
  | _ => [] end.
Definition code_functor_profiles (code : reify_code ety key)
    : list (usym * reify_exports ety key) :=
  (match owner_functor_target (rc_owner ety key code),rc_exports ety key code with
   | Some target,Some profile => [(target,profile)] | _,_ => [] end) ++
  flat_map event_functor_profile (rc_events ety key code).
Definition source_functor_profiles (source : st)
    : list (usym * reify_exports ety key) :=
  flat_map (fun p => code_functor_profiles (snd p)) (rm_codes (st_reify source)).

Definition export_name_roles (outputs : list(tsym*tsym)) (generation_roles : subst) : subst :=
  flat_map (fun p => match lookup_subst (snd p) generation_roles with
    | Some role => [(fst p,role)] | None => [] end) outputs.
Definition prepare_functor_export_interfaces (source : st) (original : usym)
    (profile : reify_exports ety key) (defaults : subst) (before : reify_state)
    : option (reify_functor_interfaces * reify_state) :=
  match prepare_functor_interfaces source original (rx_inputs ety key profile)
      (rx_promised ety key profile) (map snd (rx_outputs ety key profile))
      (rx_items ety key profile) defaults before with
  | Some (draft,after) =>
      Some (ReifyFunctorInterfaces (rfi_source draft) (rfi_slot draft)
        (rfi_input_scope draft) (rfi_output_scope draft) (rfi_input_types draft)
        (export_name_roles (rx_outputs ety key profile) (rfi_output_types draft))
        (rfi_input_items draft) (rfi_output_items draft) (rfi_inputs draft)
        (rfi_outputs draft) (rfi_pending_inputs draft) (rfi_pending_outputs draft)
        (rfi_input_runtime draft) (rfi_output_runtime draft),after)
  | None => None end.

Record reify_functor_profile : Type := ReifyFunctorProfile {
  rfp_draft : reify_functor_interfaces;
  rfp_fixed_names : list tsym
}.
Fixpoint lookup_functor_profile (original : usym) (profiles : list reify_functor_profile)
    : option reify_functor_profile :=
  match profiles with
  | [] => None
  | profile :: rest => if Nat.eqb original (rfi_source (rfp_draft profile))
      then Some profile else lookup_functor_profile original rest end.
Definition source_functor_fixed (source : st) (original : usym) : option (list tsym) :=
  match sym original source with
  | Good (symbol,state) => match functor_fixed symbol state with
      | Good (fixed,_) => Some fixed | Bad _ => None end
  | Bad _ => None end.

Fixpoint prepare_functor_profiles (source : st) (defaults : subst)
    (todo : list(usym * reify_exports ety key)) (profiles : list reify_functor_profile)
    (before : reify_state) : option(list reify_functor_profile * reify_state) :=
  match todo with
  | [] => Some(profiles,before)
  | (original,exports)::rest =>
      match lookup_functor_profile original profiles with
      | Some _ => prepare_functor_profiles source defaults rest profiles before
      | None => match source_functor_fixed source original,
          prepare_functor_export_interfaces source original exports defaults before with
          | Some fixed,Some(draft,middle) =>
              prepare_functor_profiles source defaults rest
                (ReifyFunctorProfile draft fixed::profiles) middle
          | _,_ => None end
      end
  end.

Lemma export_name_roles_exact : forall outputs roles name role,
  In(name,role)(export_name_roles outputs roles) <->
  exists generation,In(name,generation)outputs /\lookup_subst generation roles=Some role.
Proof.
 intros outputs roles name role;unfold export_name_roles;rewrite in_flat_map;split.
 - intros[[n g][Hin H]];cbn in H.
   destruct(lookup_subst g roles)as[r|]eqn:E;cbn in H;[|contradiction].
   destruct H as[Same|[]];injection Same as SameName SameRole;subst n r.
   exists g;split;assumption.
 - intros[g[Hin E]];exists(name,g);split;[exact Hin|].
   cbn;rewrite E;cbn;left;reflexivity.
Qed.

Lemma prepared_export_roles_use_output_names : forall source original profile defaults before draft after,
  prepare_functor_export_interfaces source original profile defaults before=Some(draft,after)->
  forall name role,In(name,role)(rfi_output_types draft)->exists generation,In(name,generation)(rx_outputs ety key profile).
Proof.
 intros source original profile defaults before draft after H name role Hin.
 unfold prepare_functor_export_interfaces in H.
 destruct(prepare_functor_interfaces _ _ _ _ _ _ _ _)as[[raw final]|];[|discriminate].
 inversion H;subst;cbn in Hin;apply export_name_roles_exact in Hin as[g[Hpair _]].
 eauto.
Qed.

Record reserved_functor_profile : Type := ReservedFunctorProfile {
  rsp_profile : reify_functor_profile;
  rsp_input_functors : list(usym*usym);
  rsp_output_functors : list(usym*usym)
}.
Fixpoint reserve_functor_slots (keys : list key) (before : reify_state)
    : option(list(usym*usym)*reify_state) :=
  match keys with
  | [] => Some([],before)
  | KyFun original::rest =>
      let slot:=rf_next before in
      let middle:=ReifyState(S slot)(rf_names before)(rf_generations before)
        (rf_items before)(rf_declarations before) in
      match reserve_functor_slots rest middle with
      | Some(slots,after)=>Some((original,slot)::slots,after)|None=>None end
  | _::_ => None end.
Fixpoint reserve_nested_functor_profiles (profiles : list reify_functor_profile)
    (before : reify_state) : option(list reserved_functor_profile*reify_state) :=
  match profiles with
  | [] => Some([],before)
  | profile::rest =>
      match reserve_functor_slots(rfi_pending_inputs(rfp_draft profile))before with
      | None=>None
      | Some(inputs,middle)=>
          match reserve_functor_slots(rfi_pending_outputs(rfp_draft profile))middle with
          | None=>None
          | Some(outputs,middle)=>
              match reserve_nested_functor_profiles rest middle with
              | Some(profiles,after)=>Some(ReservedFunctorProfile profile inputs outputs::profiles,after)
              | None=>None end
          end
      end
  end.
Fixpoint lookup_reserved_functor (original : usym) (profiles : list reserved_functor_profile)
    : option reserved_functor_profile :=
  match profiles with
  | []=>None
  | profile::rest => if Nat.eqb original(rfi_source(rfp_draft(rsp_profile profile)))
      then Some profile else lookup_reserved_functor original rest end.
Definition reserved_input_items (profile : reserved_functor_profile) : smap :=
  rfi_input_items(rfp_draft(rsp_profile profile)) ++
  map(fun p=>(IFun(fst p),IFun(snd p)))(rsp_input_functors profile).
Definition reserved_output_items (profile : reserved_functor_profile) : smap :=
  rfi_output_items(rfp_draft(rsp_profile profile)) ++
  map(fun p=>(IFun(fst p),IFun(snd p)))(rsp_output_functors profile).
Definition reserved_inputs (profile : reserved_functor_profile) : tele :=
  rfi_inputs(rfp_draft(rsp_profile profile))++map(fun p=>IFun(snd p))(rsp_input_functors profile).
Definition reserved_outputs (profile : reserved_functor_profile) : tele :=
  rfi_outputs(rfp_draft(rsp_profile profile))++map(fun p=>IFun(snd p))(rsp_output_functors profile).
Definition profile_fixed_generations (profile : reify_functor_profile) : list tsym :=
  nodup Nat.eq_dec(flat_map(fun name=>ety_gens(scope_type_meaning
    (rfi_input_scope(rfp_draft profile))name))(rfp_fixed_names profile)).
Definition profile_fixed_roles (profile : reify_functor_profile) (defaults : subst) : list tsym :=
  nodup Nat.eq_dec(tyreqs(type_role_telescope defaults) ++
    flat_map(fun p=>match snd p with
      | TAbs formal=>if existsb(Nat.eqb(fst p))(profile_fixed_generations profile)
          then[formal]else[] | _=>[] end)(rfi_input_types(rfp_draft profile))).
Definition reserved_schema (source : st) (defaults : subst) (profile : reserved_functor_profile)
    (fixed_interpretation : subst) : reify_functor_decl :=
  let draft:=rfp_draft(rsp_profile profile) in
  let fixed:=profile_fixed_roles(rsp_profile profile)defaults in
  ReifyFunctorDecl(reserved_inputs profile)(reserved_outputs profile)fixed
    (fixed_interpretation++source_role_equations source(rfi_input_scope draft)(rfi_input_types draft))
    (filter(fun t=>negb(existsb(Nat.eqb t)fixed))(tyreqs(reserved_inputs profile)))[][].
Definition reserved_view (source : st) (defaults : subst) (profile : reserved_functor_profile)
    (slot : usym) (fixed_interpretation : subst) : reify_functor_view :=
  let draft:=rfp_draft(rsp_profile profile) in
  ReifyFunctorView(rfi_source draft)slot(rfi_input_scope draft)(rfi_output_scope draft)
    (rfi_input_types draft)(reserved_input_items profile)(rfi_output_types draft)
    (reserved_output_items profile)(reserved_schema source defaults profile fixed_interpretation).

Fixpoint all_captured_types (arguments : list(tsym*option ety)) : option(list(tsym*ety)):=
  match arguments with
  | []=>Some[]
  | (name,Some meaning)::rest=>match all_captured_types rest with
      | Some tail=>Some((name,meaning)::tail)|None=>None end
  | (_,None)::rest=>all_captured_types rest end.
Definition nested_captured_interpretation (source : st) (defaults : subst)
    (parent_scope : reify_scope ety key) (parent_runtime : reify_state)
    (child : reserved_functor_profile) : option subst :=
  let original:=rfi_source(rfp_draft(rsp_profile child)) in
  match assoc key_eqb(KyFun original)(rs_provisions ety key parent_scope)with
  | Some(arguments,_)=>match all_captured_types arguments with
      | Some arguments=>match instantiate_functor_fixed_types source
          (rfi_input_scope(rfp_draft(rsp_profile child)))
          (rfi_input_types(rfp_draft(rsp_profile child)))
          (rfp_fixed_names(rsp_profile child))arguments parent_runtime with
          | Some captured=>Some(captured++map(fun t=>(t,TAbs t))(tyreqs(type_role_telescope defaults)))
          | None=>None end
      | None=>None end
  | None=>None end.
Fixpoint nested_reserved_views (source : st) (defaults : subst)
    (profiles : list reserved_functor_profile) (scope : reify_scope ety key)
    (runtime : reify_state) (slots : list(usym*usym)) : option(list reify_functor_view):=
  match slots with
  | []=>Some[]
  | (original,slot)::rest=>match lookup_reserved_functor original profiles with
      | Some child=>match nested_captured_interpretation source defaults scope runtime child,
          nested_reserved_views source defaults profiles scope runtime rest with
          | Some captured,Some views=>Some(reserved_view source defaults child slot captured::views)
          | _,_=>None end
      | None=>None end
  end.
Fixpoint complete_reserved_views (source : st) (defaults : subst)
    (all todo : list reserved_functor_profile) : option(list reify_functor_view):=
  match todo with
  | []=>Some[]
  | profile::rest=>let draft:=rfp_draft(rsp_profile profile) in
      match nested_reserved_views source defaults all(rfi_input_scope draft)(rfi_input_runtime draft)
          (rsp_input_functors profile),
          nested_reserved_views source defaults all(rfi_output_scope draft)(rfi_output_runtime draft)
          (rsp_output_functors profile),complete_reserved_views source defaults all rest with
      | Some inputs,Some outputs,Some others=>
          let fixed:=profile_fixed_roles(rsp_profile profile)defaults in
          Some(reserved_view source defaults profile(rfi_slot draft)
            (map(fun t=>(t,TAbs t))fixed)::inputs++outputs++others)
      | _,_,_=>None end
  end.
Definition prepare_functor_templates_for_profiles (source : st) (defaults : subst)
    (profiles : list(usym*reify_exports ety key))
    (before : reify_state) : option(functor_work*reify_state):=
  match prepare_functor_profiles source defaults profiles[]before with
  | None=>None
  | Some(profiles,middle)=>match reserve_nested_functor_profiles profiles middle with
      | None=>None
      | Some(reserved,after)=>match complete_reserved_views source defaults reserved reserved with
          | None=>None
          | Some views=>Some(FunctorWork views[][],
              ReifyState(rf_next after)(rf_names before)(rf_generations before)(rf_items before)
                (map(fun view=>(IFun(rfv_slot view),RDFunctor(rfv_schema view)))views++rf_declarations after))
          end
      end
  end.

Definition prepare_source_functor_templates (source : st) (defaults : subst)
    (before : reify_state) : option(functor_work*reify_state):=
  prepare_functor_templates_for_profiles source defaults(source_functor_profiles source)before.

Lemma reserved_schema_keeps_child_fixed_keys : forall source defaults profile interpretation,
  rfd_fixed(reserved_schema source defaults profile interpretation)=
  profile_fixed_roles(rsp_profile profile)defaults.
Proof. reflexivity. Qed.

Example uncaptured_nonfixed_parameter_is_not_a_capture :
  all_captured_types [(1,None);(2,Some YUnit)] = Some [(2,YUnit)].
Proof. reflexivity. Qed.
Example uncaptured_required_fixed_parameter_still_fails : forall source runtime,
  match all_captured_types [(1,None);(2,Some YUnit)] with
  | Some captured => instantiate_functor_fixed_types source
      (ReifyScope ety key [] [] [] [] [] [] [])
      [(1,TAbs 100)] [1] captured runtime
  | None => None end = None.
Proof. reflexivity. Qed.
