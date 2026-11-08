(* Provider code has one immutable frame per checked raw generated symbol.
   Its private type captures never reuse a polymorphic formal role by ID. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import ExecutionModel CoreMoss Elab ReifyData Reify ReifySource
  ReifyInterfaces ReifyRoles ReifyPlan ReifyInitial ReifyFunctors ReifyFunctorProfiles BodyTr ReifyReach ReifyWalkData ReifyAnnotations ReifyTransfers ReifyMaps.
Import ListNotations.

Fixpoint lookup_provider_job (original : usym) (jobs : list reify_functor_job)
    : option reify_functor_job :=
  match jobs with
  | []=>None
  | job::rest=>if Nat.eqb original(rfj_original job)then Some job
      else lookup_provider_job original rest
  end.
Definition remember_provider_job (job : reify_functor_job) (work : functor_work) : functor_work :=
  FunctorWork(fw_views work)(job::fw_jobs work)(fw_adapters work).
Lemma remembered_provider_job : forall job work,
  lookup_provider_job(rfj_original job)(fw_jobs(remember_provider_job job work))=Some job.
Proof. intros;cbn;rewrite Nat.eqb_refl;reflexivity. Qed.

Definition canonical_input_meaning (source : st) (defaults : subst)
    (target : reify_functor_view) (raw : ety) : ty :=
  ExecutionModel.app_subst(rfv_input_types target++defaults)
    (reify_snapshot_type source(rfv_input_scope target)raw).

(* A frozen old generation is not the mutable name which later replaced it.
   A manifest or composite G name also imposes no equation on an unused F
   polymorphic input. Only G's surviving representative parameter is linked. *)
Definition provider_linked_generation (source : st) (defaults : subst)
    (target : reify_functor_view) (scope : reify_scope ety key) (generation : tsym)
    : option ty :=
  match assoc Nat.eqb generation(rs_names ety key scope),
    assoc Nat.eqb generation(rs_names ety key(rfv_input_scope target))with
  | Some own,Some supplied=>
      if ty_eqb(reify_snapshot_type source scope own)(TAbs generation)
      then Some(canonical_input_meaning source defaults target supplied)
      else None
  | _,_=>None end.
Definition provider_linked_generations (source : st) (defaults : subst)
    (target : reify_functor_view) (scope : reify_scope ety key) : subst :=
  flat_map(fun generation=>match provider_linked_generation source defaults target scope generation with
    | Some meaning=>[(generation,meaning)]|None=>[]end)(source_scope_type_keys scope).

Record provider_type_frame : Type := ProviderTypeFrame {
  ptf_linked : subst;
  ptf_captures : subst;
  ptf_runtime : reify_state
}.
Definition prepare_provider_types (source : st) (defaults : subst)
    (target : reify_functor_view) (scope : reify_scope ety key)
    (before : reify_state) : provider_type_frame :=
  let linked:=provider_linked_generations source defaults target scope in
  let private:=filter(fun generation=>match lookup_subst generation linked with
      Some _=>false|None=>true end)(source_scope_type_keys scope) in
  let blank:=ReifyState(rf_next before)[][][](rf_declarations before) in
  let '(captures,_,allocated):=allocate_type_roles private blank in
  let typed:=ReifyState(rf_next allocated)[](linked++captures++defaults)[]
    (rf_declarations allocated) in
  ProviderTypeFrame linked captures
    (enter_source_names source scope(enter_source_generations source scope typed)).

Lemma provider_capture_keys_are_private : forall source defaults target scope before generation role,
  In(generation,role)(ptf_captures(prepare_provider_types source defaults target scope before))->
  lookup_subst generation(provider_linked_generations source defaults target scope)=None.
Proof.
 intros source defaults target scope before generation role H.
 unfold prepare_provider_types in H.
 destruct(allocate_type_roles _ _)as[[captures sm]allocated]eqn:E;cbn in H.
 pose proof(allocate_type_roles_domain _ _ _ _ _ E)as[Hd _].
 assert(Hin:In generation(map fst captures))by(apply in_map with(f:=@fst tsym ty)in H;exact H).
 rewrite Hd in Hin;apply filter_In in Hin as[_ Hprivate].
 destruct(lookup_subst generation(provider_linked_generations source defaults target scope));
   [discriminate|reflexivity].
Qed.

Example provider_manifest_name_does_not_constrain_unused_input : forall source,
  let empty:=ReifyScope ety key [][][][][][][] in
  let target_scope:=ReifyScope ety key[(1,YAbs 1)][(1,YAbs 1)][][][][][] in
  let schema:=ReifyFunctorDecl[ITy 100][][][][][][] in
  let target:=ReifyFunctorView 9 90 target_scope empty[(1,TAbs 100)][][][]schema in
  let provider_scope:=ReifyScope ety key[(1,YNom 7 [])][(1,YAbs 1)][][][][][] in
  provider_linked_generation source[]target provider_scope 1=None.
Proof. intros;reflexivity. Qed.

Definition provider_capture_meaning (source : st) (scope : reify_scope ety key)
    (arguments : list(tsym*ety)) (caller : reify_state) (generation : tsym) : option ty :=
  let own:=assoc Nat.eqb generation(rs_names ety key scope) in
  match own with
  | Some own=>
      if ty_eqb(reify_snapshot_type source scope own)(TAbs generation) then
        match assoc Nat.eqb generation arguments with
        | Some actual=>Some(ExecutionModel.app_subst(rf_generations caller)
            (tr(reify_field_number source)actual))
        | None=>lookup_subst generation(rf_generations caller)end
      else lookup_subst generation(rf_generations caller)
  | None=>lookup_subst generation(rf_generations caller)
  end.
Fixpoint provider_capture_arguments (source : st) (scope : reify_scope ety key)
    (roles : subst) (arguments : list(tsym*ety)) (caller : reify_state) : option subst :=
  match roles with
  | []=>Some[]
  | (generation,TAbs formal)::rest=>
      match provider_capture_meaning source scope arguments caller generation,
        provider_capture_arguments source scope rest arguments caller with
      | Some meaning,Some tail=>Some((formal,meaning)::tail)
      | _,_=>None end
  | _::_=>None end.

(* The post-body profile, not a replay of source RHS names at the bind site,
   determines fixed manifest equations. Fresh body outputs remain explicit
   in this computation and cannot masquerade as old caller type names. *)
Definition provider_fixed_pins (source : st) (target : reify_functor_view)
    (fixed_names : list tsym) (profile : reify_exports ety key)
    (runtime : reify_state) : option subst :=
  instantiate_functor_fixed_types source(rfv_input_scope target)(rfv_input_types target)
    fixed_names(map(fun name=>(name,scope_type_meaning(rx_actual ety key profile)name))fixed_names)runtime.

Lemma provider_capture_arguments_domain : forall source scope roles arguments caller supplied,
  provider_capture_arguments source scope roles arguments caller=Some supplied->
  map fst supplied=flat_map(fun p=>match snd p with TAbs formal=>[formal]|_=>[]end)roles.
Proof.
 intros source scope roles;induction roles as[|[generation role]rest IH];
   intros arguments caller supplied H;cbn in H.
 - inversion H;reflexivity.
 - destruct role as[|formal|n th|members];try discriminate.
   destruct(provider_capture_meaning source scope arguments caller generation);[|discriminate].
   destruct(provider_capture_arguments source scope rest arguments caller)as[tail|]eqn:E;[|discriminate].
   inversion H;subst;cbn;f_equal;eapply IH;exact E.
Qed.

Definition selected_provider_scope (source : st) (scope : reify_scope ety key)
    (selected : tele) : reify_scope ety key :=
  let chosen k:=existsb(item_eqb(reify_snapshot_item source scope k))selected in
  ReifyScope ety key(rs_names ety key scope)(rs_generations ety key scope)
    (filter chosen(rs_items ety key scope))
    (filter(fun p=>chosen(KyVal(fst p)))(rs_values ety key scope))
    (filter(fun p=>chosen(KyFn(fst p)))(rs_functions ety key scope))
    (filter(fun p=>chosen(KyMth(fst(fst p))(snd(fst p))))(rs_methods ety key scope))
    (rs_provisions ety key scope).
Fixpoint provider_linked_items (target : reify_functor_view) (source_map : smap)
    : option smap :=
  match source_map with
  | []=>Some[]
  | (source,promised)::rest=>
      match lookup_smap promised(rfv_input_items target),provider_linked_items target rest with
      | Some formal,Some tail=>Some((source,formal)::tail)
      | _,_=>None end
  end.

(* Private abstract functor slots preserve their own canonical formal
   interfaces; their fixed interpretations are read through G's fresh frame. *)
Fixpoint allocate_provider_functor_captures (source : st) (defaults : subst)
    (scope : reify_scope ety key) (todo : list key) (before : reify_state)
    (work : functor_work) : option(tele*reify_state*functor_work) :=
  match todo with
  | []=>Some([],before,work)
  | KyFun original::rest=>
      match lookup_functor_template original(fw_views work),
        assoc key_eqb(KyFun original)(rs_provisions ety key scope),source_functor_fixed source original with
      | Some template,Some(arguments,_),Some fixed=>
          let arguments:=flat_map(fun p=>match snd p with
            Some meaning=>[(fst p,meaning)]|None=>[]end)arguments in
          match instantiate_functor_fixed_types source(rfv_input_scope template)
            (rfv_input_types template)fixed arguments before with
          | Some interpretation=>
              let hidden:=map(fun t=>(t,TAbs t))(tyreqs(type_role_telescope defaults)) in
              let '(view,middle):=instantiate_functor_template template(interpretation++hidden++source_role_equations source
                (rfv_input_scope template)(rfv_input_types template))[][]before in
              match allocate_provider_functor_captures source defaults scope rest middle
                (remember_functor_view view work)with
              | Some(items,after,after_work)=>Some(IFun(rfv_slot view)::items,after,after_work)
              | None=>None end
          | None=>None end
      | _,_,_=>None end
  | _::_=>None end.

Definition provider_platform_pins (source : st) (target : reify_functor_view) : subst :=
  flat_map(fun p=>match lookup_subst(fst p)(rfv_input_types target)with
    | Some(TAbs role)=>[(role,snd p)]|_=>[]end)
    (runtime_machines(reify_runtime_schema source)(st_next source)).

Record provider_body_frame : Type := ProviderBodyFrame {
  pbf_types : provider_type_frame;
  pbf_captures : tele;
  pbf_capture_items : smap;
  pbf_pins : subst;
  pbf_local : reify_local;
  pbf_work : functor_work
}.
Definition prepare_provider_body_frame (source : st) (defaults : subst)
    (target : reify_functor_view) (code : reify_code ety key)
    (fixed_names : list tsym) (linked captured : smap)
    (before : reify_state) (work : functor_work) : option provider_body_frame :=
  let scope:=rc_scope ety key code in
  let types:=prepare_provider_types source defaults target scope before in
  let private:=selected_provider_scope source scope(map fst captured) in
  let '(first_order,middle):=lower_scope_first_order source private(ptf_runtime types) in
  match provider_linked_items target linked,
    allocate_provider_functor_captures source defaults private(source_functor_items private)middle work,
    rc_exports ety key code with
  | Some input_items,Some(functors,local,after_work),Some profile=>
      match provider_fixed_pins source target fixed_names profile local with
      | None=>None
      | Some pins=>
          let platform:=match rc_owner ety key code with
            | ROPlatform _=>provider_platform_pins source target|_=>[]end in
          let fixed_types:=map ITy(rfd_fixed(rfv_schema target)) in
          let captured_types:=type_role_telescope(ptf_captures types) in
          let body:=ReifyState(rf_next local)(rf_names local)(rf_generations local)
            (input_items++rf_items local)(rf_declarations local) in
          Some(ProviderBodyFrame types
            (fixed_types++captured_types++first_order++functors)(rev(rf_items local))
            (platform++pins)(ReifyLocal body(platform++pins))after_work)
      end
  | _,_,_=>None end.

Definition provider_code_schema (source : st) (target : reify_functor_view)
    (frame : provider_body_frame) : reify_functor_decl :=
  let base:=rfv_schema target in
  ReifyFunctorDecl(rfd_inputs base)(rfd_outputs base)(rfd_fixed base)
    (map(fun t=>(t,TAbs t))(rfd_fixed base)++
      source_role_equations source(rfv_input_scope target)(rfv_input_types target))
    (rfd_poly base)(pbf_captures frame)(pbf_pins frame).
Definition provider_code_view (source : st) (target : reify_functor_view)
    (code : usym) (frame : provider_body_frame) : reify_functor_view :=
  ReifyFunctorView(rfv_source target)code(rfv_input_scope target)(rfv_output_scope target)
    (rfv_input_types target)(rfv_input_items target)(rfv_output_types target)(rfv_output_items target)
    (provider_code_schema source target frame).

Definition reserve_provider_job (source : st) (defaults : subst)
    (raw_code : usym) (target : reify_functor_view) (fixed_names : list tsym)
    (linked captured : smap) (before : reify_state) (work : functor_work)
    : option(reify_functor_job*reify_state*functor_work) :=
  match lookup_provider_job raw_code(fw_jobs work)with
  | Some job=>Some(job,before,work)
  | None=>
      match reify_code_for_symbol source raw_code,
        reify_lookup_functor raw_code(st_funs source)with
      | Some metadata,Some(_,body)=>
          let code:=rf_next before in
          let reserved:=ReifyState(S code)(rf_names before)(rf_generations before)
            (rf_items before)(rf_declarations before) in
          match prepare_provider_body_frame source defaults target metadata fixed_names
            linked captured reserved work with
          | None=>None
          | Some frame=>
              let view:=provider_code_view source target code frame in
              let runtime:=rl_runtime(pbf_local frame) in
              let declared:=ReifyState(rf_next runtime)(rf_names runtime)(rf_generations runtime)
                (rf_items runtime)((IFun code,RDFunctor(rfv_schema view))::rf_declarations runtime) in
              let local:=ReifyLocal declared(rl_aliases(pbf_local frame)) in
              let job:=ReifyFunctorJob raw_code code view local body
                (ptf_captures(pbf_types frame))(pbf_capture_items frame)None in
              Some(job,restore_reify_scope before declared,
                remember_provider_job job(remember_functor_view view(pbf_work frame)))
          end
      | _,_=>None end
  end.

Lemma provider_reservation_reuses_existing_job : forall source defaults raw_code target fixed_names linked captured before work job,
  lookup_provider_job raw_code(fw_jobs work)=Some job->
  reserve_provider_job source defaults raw_code target fixed_names linked captured before work=
    Some(job,before,work).
Proof. intros;unfold reserve_provider_job;rewrite H;reflexivity. Qed.

Fixpoint provider_fixed_capture_arguments (fixed : list tsym) (interpretation : subst)
    : option subst :=
  match fixed with
  | []=>Some[]
  | formal::rest=>match lookup_subst formal interpretation,
      provider_fixed_capture_arguments rest interpretation with
      | Some meaning,Some tail=>Some((formal,meaning)::tail)
      | _,_=>None end
  end.
Definition lower_defined_functor (source : st) (code : reify_code ety key)
    (defaults : subst) (name raw_code : usym) (linked captured : smap)
    (before : reify_local) (work : functor_work)
    : option(list xbind*reify_local*functor_work) :=
  let caller:=rl_runtime before in
  match find_reify_annotation
      (functor_annotation_matches source(rl_aliases before)caller name raw_code)
      (rc_events ety key code),
    lookup_functor_template name(fw_views work),source_functor_fixed source name with
  | Some(REFunctor _ _ _ _ _ arguments None),Some template,Some fixed_names=>
      match instantiate_functor_fixed_types source(rfv_input_scope template)
        (rfv_input_types template)fixed_names arguments caller,
        reserve_provider_job source defaults raw_code template fixed_names linked captured caller work with
      | Some fixed,Some(job,reserved,reserved_work)=>
          let hidden:=map(fun t=>(t,TAbs t))(tyreqs(type_role_telescope defaults)) in
          let interpretation:=fixed++hidden++source_role_equations source
            (rfv_input_scope template)(rfv_input_types template) in
          match reify_code_for_symbol source raw_code with
          | None=>None
          | Some metadata=>
              match provider_fixed_capture_arguments(rfd_fixed(rfv_schema(rfj_view job)))interpretation,
                provider_capture_arguments source(rc_scope ety key metadata)
                  (rfj_type_captures job)arguments caller,
                resolve_item_transfers(rfj_item_captures job)captured caller with
              | Some fixed_args,Some private_args,Some item_map=>
                  let '(temporaries,type_map,allocated):=
                    materialize_type_arguments(fixed_args++private_args)reserved in
                  let '(view,after):=instantiate_functor_template template interpretation[][]allocated in
                  let binding:=XBFun(rfv_slot view)(rfj_code job)
                    (identity_smap(rfd_inputs(rfv_schema(rfj_view job))))(type_map++item_map) in
                  Some(map(fun p=>XBTy(fst p)(snd p))temporaries++[binding],
                    ReifyLocal after(aliases_after_temporaries temporaries(rl_aliases before)),
                    remember_functor_view view reserved_work)
              | _,_,_=>None end
          end
      | _,_=>None end
  | _,_,_=>None end.
