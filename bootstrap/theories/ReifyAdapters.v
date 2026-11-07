(* An abstract copy with different canonical interfaces is implemented by
   an actual provider body, applying the captured old value through complete
   input/output maps. Ambient fixed meanings are quoted before that body. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss Elab ReifyData Reify ReifySource ReifySig
  ReifyFunctors ReifyProviders ReifyFunctorProfiles ReifyQuotes ReifyMaps
  ReifyRoles ReifyTransfers ReifyInterfaces ReifyPlan ReifyWalkData ReifyAnnotations ReifyExports.
Import ListNotations.

Definition adapter_item_types (declarations : list(item*reify_decl)) (entry : item)
    : option(list ty) :=
  match entry with
  | IVal value=>match lookup_reify_decl(IVal value)declarations with
      | Some(RDValue _ meaning)=>Some[meaning]|_=>None end
  | IFn function=>match lookup_reify_decl(IFn function)declarations with
      | Some(RDFunction _ signature)=>Some(fs_ret signature::fs_params signature)|_=>None end
  | IMth receiver method theta=>match lookup_reify_method method declarations with
      | Some(_,signature)=>let '(parameters,result):=method_call_signature[]receiver theta signature in
          Some(result::parameters)
      | None=>None end
  | IFun functor=>match lookup_reify_functor functor declarations with
      | Some schema=>effective_fixed_values(rfd_fixed schema)(rfd_fixed_inst schema)
      | None=>None end
  | ITy _=>None end.
Fixpoint adapter_match_types (variables : list tsym) (patterns meanings : list ty)
    (solutions : subst) : option subst :=
  match patterns,meanings with
  | [],[]=>Some solutions
  | pattern::patterns,meaning::meanings=>
      match match_generation_pattern variables pattern meaning solutions with
      | Some next=>adapter_match_types variables patterns meanings next
      | None=>None end
  | _,_=>None end.
Fixpoint adapter_match_input_items (declarations : list(item*reify_decl))
    (variables : list tsym) (pins : subst) (provider target : reify_functor_view)
    (source_map : smap) (solutions : subst) : option subst :=
  match source_map with
  | []=>Some solutions
  | (source,target_source)::rest=>
      match lookup_smap source(rfv_input_items provider),lookup_smap target_source(rfv_input_items target)with
      | Some actual,Some supplied=>
          match adapter_item_types declarations actual,adapter_item_types declarations supplied with
          | Some patterns,Some meanings=>
              match adapter_match_types variables patterns(map(Audit.app_subst pins)meanings)solutions with
              | Some next=>adapter_match_input_items declarations variables pins provider target rest next
              | None=>None end
          | _,_=>None end
      | _,_=>None end
  end.
Definition adapter_name_arguments (source : st) (defaults pins : subst)
    (target : reify_functor_view) : subst :=
  map(fun p=>(fst p,Audit.app_subst pins(canonical_input_meaning source defaults target(snd p))))
    (rs_names ety key(rfv_input_scope target)).
Definition adapter_name_patterns (source : st) (provider : reify_functor_view) : subst :=
  map(fun p=>(fst p,reify_snapshot_type source(rfv_input_scope provider)(snd p)))
    (rs_names ety key(rfv_input_scope provider)).
Definition adapter_formal_patterns (source : st) (defaults : subst) (provider : reify_functor_view) : subst :=
  map(fun p=>(fst p,Audit.app_subst(rfv_input_types provider++defaults)(snd p)))
    (adapter_name_patterns source provider).
Definition adapter_input_solutions (source : st) (defaults pins : subst)
    (target provider : reify_functor_view) (source_map : smap)
    (declarations : list(item*reify_decl)) (fixed_solutions : subst) : option subst :=
  let variables:=tyreqs(rfd_inputs(rfv_schema provider)) in
  let arguments:=adapter_name_arguments source defaults pins target in
  let empty:=ReifyState 0[][][][] in
  match solve_generation_arguments variables(adapter_formal_patterns source defaults provider)arguments empty fixed_solutions with
  | None=>None
  | Some solutions=>adapter_match_input_items declarations variables pins provider target source_map solutions
  end.

Fixpoint adapter_input_arguments (formals : list tsym) (solutions defaults : subst)
    (provider_roles : subst) : option subst :=
  match formals with
  | []=>Some[]
  | formal::rest=>
      let meaning:=match lookup_subst formal solutions with
        | Some known=>Some known
        | None=>match find(fun p=>ty_eqb(snd p)(TAbs formal))provider_roles with
            | Some(generation,_)=>lookup_subst generation defaults
            | None=>if existsb(fun p=>ty_eqb(snd p)(TAbs formal))defaults
                then Some(TAbs formal)else None end end in
      match meaning,adapter_input_arguments rest solutions defaults provider_roles with
      | Some meaning,Some tail=>Some((formal,meaning)::tail)
      | _,_=>None end
  end.

Fixpoint adapter_output_placeholders (provider target : reify_functor_view)
    (roles : subst) : option subst :=
  match roles with
  | []=>Some[]
  | (name,_)::rest=>
      match scope_type_meaning(rfv_output_scope provider)name,
        lookup_subst name(rfv_output_types target),adapter_output_placeholders provider target rest with
      | YAbs placeholder,Some meaning,Some tail=>Some((placeholder,meaning)::tail)
      | _,_,_=>None end
  end.
Definition adapter_input_source_meanings (defaults solutions : subst)
    (provider : reify_functor_view) : subst :=
  map(fun p=>(fst p,Audit.app_subst solutions(snd p)))(rfv_input_types provider)++defaults.
Definition adapter_render_source_type (source : st) (pins outputs input_meanings : subst)
    (scope : reify_scope ety key) (meaning : ty) : ty :=
  Audit.app_subst pins(Audit.app_subst input_meanings
    (Audit.app_subst(result_generation_equations source scope(map fst outputs))
      (Audit.app_subst outputs meaning))).
Definition adapter_render_source_item (render : ty->ty) (entry : item) : item :=
  match entry with
  | IMth receiver method theta=>IMth(render receiver)method
      (map(fun p=>(fst p,render(snd p)))theta)
  | _=>entry end.
Fixpoint adapter_type_outputs (target provider : subst) : option smap :=
  match target with
  | []=>Some[]
  | (name,TAbs formal)::rest=>match lookup_subst name provider,adapter_type_outputs rest provider with
      | Some(TAbs actual),Some tail=>Some((ITy formal,ITy actual)::tail)
      | _,_=>None end
  | _::_=>None end.
Fixpoint adapter_item_outputs (target_render provider_render : ty->ty)
    (target provider : smap) : option smap :=
  match target with
  | []=>Some[]
  | (source,formal)::rest=>
      match find(fun candidate=>item_eqb(adapter_render_source_item target_render source)
          (adapter_render_source_item provider_render(fst candidate)))(rev provider),
        adapter_item_outputs target_render provider_render rest provider with
      | Some(_,actual),Some tail=>Some((formal,actual)::tail)
      | _,_=>None end
  end.
Definition adapter_output_map (source : st) (defaults pins solutions : subst)
    (target provider : reify_functor_view) : option smap :=
  match adapter_output_placeholders target target(rfv_output_types target),
    adapter_output_placeholders provider target(rfv_output_types provider),
    adapter_type_outputs(rfv_output_types target)(rfv_output_types provider)with
  | Some target_outputs,Some provider_outputs,Some types=>
      let target_input:=rfv_input_types target++defaults in
      let provider_input:=adapter_input_source_meanings defaults solutions provider in
      let target_render:=adapter_render_source_type source pins target_outputs target_input(rfv_output_scope target) in
      let provider_render:=adapter_render_source_type source pins provider_outputs provider_input(rfv_output_scope provider) in
      match adapter_item_outputs target_render provider_render(rfv_output_items target)(rfv_output_items provider)with
      | Some items=>Some(types++items)
      | None=>None end
  | _,_,_=>None end.

Lemma adapter_item_outputs_domain : forall target_render provider_render target provider outputs,
  adapter_item_outputs target_render provider_render target provider=Some outputs->
  map fst outputs=map snd target.
Proof.
 intros target_render provider_render target;induction target as[|[source formal]rest IH];
   intros provider outputs H;cbn in H.
 - inversion H;reflexivity.
 - destruct(find _ _)as[[actual_source actual]|];[|discriminate].
   destruct(adapter_item_outputs _ _ _ _)as[tail|]eqn:E;[|discriminate].
   inversion H;subst;cbn;f_equal;eapply IH;exact E.
Qed.

Definition adapter_code_view (source : st) (target : reify_functor_view)
    (code : usym) (captures : tele) (pins : subst) : reify_functor_view :=
  let base:=rfv_schema target in
  let schema:=ReifyFunctorDecl(rfd_inputs base)(rfd_outputs base)(rfd_fixed base)
    (map(fun t=>(t,TAbs t))(rfd_fixed base)++source_role_equations source
      (rfv_input_scope target)(rfv_input_types target))
    (rfd_poly base)captures pins in
  ReifyFunctorView(rfv_source target)code(rfv_input_scope target)(rfv_output_scope target)
    (rfv_input_types target)(rfv_input_items target)(rfv_output_types target)(rfv_output_items target)schema.

Definition build_copy_adapter (source : st) (defaults interpretation : subst)
    (target provider : reify_functor_view) (source_map : smap)
    (before : reify_local) (work : functor_work)
    : option(list xbind*reify_local*functor_work) :=
  let caller:=rl_runtime before in
  let target_fixed:=rfd_fixed(rfv_schema target) in
  let provider_schema:=rfv_schema provider in
  let actual_target:=map_fixed_interpretation target_fixed(rl_aliases before)interpretation in
  let actual_provider:=normalized_fixed_table(rl_aliases before)provider_schema in
  match fixed_capture_variables target_fixed(rfd_fixed provider_schema)actual_target actual_provider with
  | None=>None
  | Some variables=>
      let code:=rf_next caller in
      let reserved:=ReifyState(S code)(rf_names caller)(rf_generations caller)
        (rf_items caller)(rf_declarations caller) in
      let '(quotes,_,quoted):=allocate_type_roles variables reserved in
      let quoted_provider:=quote_fixed_schema(rl_aliases before)quotes provider_schema in
      let '(captured_view,captured_state):=instantiate_functor_template provider
        (rfd_fixed_inst quoted_provider)[][]quoted in
      let captured:=restore_reify_scope caller captured_state in
      match provider_fixed_capture_arguments target_fixed actual_target,
        provider_fixed_capture_arguments target_fixed
          (map_fixed_interpretation target_fixed quotes actual_target),
        provider_fixed_capture_arguments(rfd_fixed quoted_provider)(rfd_fixed_inst quoted_provider)with
      | Some fixed_arguments,Some pins,Some provider_fixed=>
          match adapter_input_solutions source defaults pins target captured_view source_map
              (rf_declarations captured)provider_fixed with
          | None=>None
          | Some solutions=>
              match adapter_input_arguments(tyreqs(rfd_inputs provider_schema))solutions defaults
                  (rfv_input_types provider),
                adapter_output_map source defaults pins solutions target captured_view with
              | Some arguments,Some output_map=>
                  let body_state:=ReifyState(rf_next captured)[][]
                    (rfv_input_items target)(rf_declarations captured) in
                  match resolve_item_transfers(rfv_input_items provider)source_map body_state with
                  | None=>None
                  | Some item_map=>
                      let '(body_temps,type_map,body_after):=materialize_type_arguments arguments body_state in
                      let body:=map(fun p=>XBTy(fst p)(snd p))body_temps++
                        [XBApplyR(rfv_slot captured_view)(type_map++item_map)output_map] in
                      let captures:=map ITy target_fixed++type_role_telescope quotes++[IFun(rfv_slot captured_view)] in
                      let code_view:=adapter_code_view source target code captures pins in
                      let declared:=ReifyState(rf_next body_after)(rf_names caller)(rf_generations caller)
                        (rf_items caller)((IFun code,RDFunctor(rfv_schema code_view))::rf_declarations body_after) in
                      let '(capture_temps,capture_types,allocated):=materialize_type_arguments
                        (fixed_arguments++quote_capture_arguments quotes)declared in
                      let '(new_view,after):=instantiate_functor_template target interpretation[][]allocated in
                      let binding:=XBFun(rfv_slot new_view)code(identity_smap(rfd_inputs(rfv_schema target)))
                        (capture_types++[(IFun(rfv_slot captured_view),IFun(rfv_slot provider))]) in
                      let views:=new_view::code_view::captured_view::fw_views work in
                      Some(map(fun p=>XBTy(fst p)(snd p))capture_temps++[binding],
                        ReifyLocal after(aliases_after_temporaries capture_temps(rl_aliases before)),
                        FunctorWork views(fw_jobs work)((code,body)::fw_adapters work))
                  end
              | _,_=>None end
          end
      | _,_,_=>None end
  end.

Definition copy_annotation_has_checked_interface(name provider:usym)
 (profile:option(reify_exports ety key)):bool:=
 match profile with Some _=>true|None=>Nat.eqb name provider end.
Lemma missing_copy_annotation_is_identity : forall name provider,
 copy_annotation_has_checked_interface name provider None=true->name=provider.
Proof. intros name provider H;apply Nat.eqb_eq;exact H. Qed.

Definition lower_abstract_functor (source : st) (code : reify_code ety key)
    (defaults : subst) (name provider : usym) (source_map : smap)
    (before : reify_local) (work : functor_work)
    : option(list xbind*reify_local*functor_work) :=
  let caller:=rl_runtime before in
  match find_reify_annotation(functor_annotation_matches source(rl_aliases before)caller name provider)
      (rc_events ety key code),lookup_functor_template name(fw_views work),
      source_functor_fixed source name,lookup_smap(IFun provider)(rf_items caller)with
  | Some(REFunctor _ _ _ _ _ arguments profile),Some target,Some fixed,Some(IFun slot)=>
      if copy_annotation_has_checked_interface name provider profile then match lookup_functor_view slot(fw_views work),instantiate_functor_fixed_types source
        (rfv_input_scope target)(rfv_input_types target)fixed arguments caller with
      | Some previous,Some interpretation=>
          let hidden:=map(fun t=>(t,TAbs t))(tyreqs(type_role_telescope defaults)) in
          build_copy_adapter source defaults(interpretation++hidden++source_role_equations source
            (rfv_input_scope target)(rfv_input_types target))target previous source_map before work
      | _,_=>None end else None
  | _,_,_,_=>None end.
