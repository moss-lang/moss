(* Canonical functor views retain distinct input and existential output roles. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import ExecutionModel CoreMoss Elab ReifyData Reify ReifySource ReifyInterfaces ReifyRoles ReifyMaps ReifyTransfers ReifyWalkData BodyTr ReifyPlan ReifyInitial ReifyAnnotations.
Import ListNotations.

Record reify_functor_view : Type := ReifyFunctorView {
  rfv_source : usym;
  rfv_slot : usym;
  rfv_input_scope : reify_scope ety key;
  rfv_output_scope : reify_scope ety key;
  rfv_input_types : subst;
  rfv_input_items : smap;
  rfv_output_types : subst;
  rfv_output_items : smap;
  rfv_schema : reify_functor_decl
}.
Record reify_functor_job : Type := ReifyFunctorJob {
  rfj_original : usym;
  rfj_code : usym;
  rfj_view : reify_functor_view;
  rfj_body_scope : reify_local;
  rfj_body : list xbind;
  rfj_type_captures : subst;
  rfj_item_captures : smap;
  rfj_exports : option smap
}.
Record functor_work : Type := FunctorWork {
  fw_views : list reify_functor_view;
  fw_jobs : list reify_functor_job;
  fw_adapters : list (usym * list xbind)
}.
Definition empty_functor_work : functor_work := FunctorWork [] [] [].
Fixpoint lookup_functor_view (slot : usym) (views : list reify_functor_view)
    : option reify_functor_view :=
  match views with
  | [] => None
  | view :: rest => if Nat.eqb slot (rfv_slot view) then Some view
      else lookup_functor_view slot rest
  end.
Definition remember_functor_view (view : reify_functor_view) (work : functor_work) : functor_work :=
  FunctorWork (view :: fw_views work) (fw_jobs work) (fw_adapters work).

Lemma remember_functor_view_lookup : forall view work,
  lookup_functor_view (rfv_slot view) (fw_views (remember_functor_view view work)) = Some view.
Proof. intros;cbn[remember_functor_view fw_views lookup_functor_view];rewrite Nat.eqb_refl;reflexivity. Qed.

(* A result binder overrides the old canonical equation before it is read.
   Other input generations keep their existing interpretation. *)
Definition result_generation_equations (source : st) (scope : reify_scope ety key)
    (outputs : list tsym) : subst :=
  filter (fun p => negb (existsb (Nat.eqb (fst p)) outputs)) (reify_snapshot_subst source scope).
Definition render_functor_result_type (source : st) (scope : reify_scope ety key)
    (outputs : subst) (runtime : reify_state) (tau : ety) : ty :=
  ExecutionModel.app_subst (rf_generations runtime)
    (ExecutionModel.app_subst (result_generation_equations source scope (map fst outputs))
      (ExecutionModel.app_subst outputs (tr (reify_field_number source) tau))).

Example result_generation_precedes_old_merge : forall source,
  let scope := ReifyScope ety key [(1,YAbs 1);(2,YAbs 2)]
    [(1,YAbs 1);(2,YAbs 1)] [] [] [] [] [] in
  let runtime := ReifyState 101 [] [(1,TAbs 90);(2,TAbs 90)] [] [] in
  render_functor_result_type source scope [(2,TAbs 100)] runtime (YAbs 2) = TAbs 100 /\
  render_functor_result_type source scope [(2,TAbs 100)] runtime (YAbs 1) = TAbs 90.
Proof. intros;split;reflexivity. Qed.

Definition result_signature (render : ety -> ty) (signature : reify_signature ety) : fnsig :=
  mkSig (map render (rsg_params ety signature)) (render (rsg_result ety signature)).
Fixpoint lookup_result_method (receiver : ety) (method : msym)
    (methods : list ((ety * msym) * reify_signature ety)) : option (reify_signature ety) :=
  match methods with
  | [] => None
  | ((other,name),signature) :: rest =>
      if ety_eqb receiver other && Nat.eqb method name then Some signature
      else lookup_result_method receiver method rest
  end.

(* Read the final table at every written occurrence. Repeated result keys
   receive separate immutable slots, all with their final effective reading. *)
Fixpoint lower_result_first_order (source : st) (scope : reify_scope ety key)
    (render : ety -> ty) (keys : list key) (before : reify_state)
    : option (tele * reify_state) :=
  match keys with
  | [] => Some ([],before)
  | current_key :: rest =>
      let current := match current_key with
      | KyVal v => match nassoc v (rs_values ety key scope) with
          | Some tau =>
              let meaning := render tau in
              let '(slot,after) := lower_value_provision v (value_type_inputs meaning) meaning before in
              Some (Some (IVal slot),after)
          | None => None end
      | KyFn f => match nassoc f (rs_functions ety key scope) with
          | Some signature =>
              let meaning := result_signature render signature in
              let '(slot,after) := lower_function_provision f (signature_type_inputs meaning) meaning before in
              Some (Some (IFn slot),after)
          | None => None end
      | KyMth receiver method =>
          match lookup_result_method receiver method (rs_methods ety key scope) with
          | Some signature =>
              let meaning := result_signature render signature in
              let '(slot,after) := lower_method_provision
                (reify_snapshot_item source scope current_key) (render receiver)
                (signature_type_inputs meaning) meaning before in
              Some (Some slot,after)
          | None => None end
      | KyFun _ => Some (None,before)
      end in
      match current with
      | Some (slot,middle) =>
          match lower_result_first_order source scope render rest middle with
          | Some (tail,after) => Some (match slot with Some i => i :: tail | None => tail end,after)
          | None => None end
      | None => None end
  end.

Lemma lower_result_first_order_scope : forall source scope render keys before outputs after,
  lower_result_first_order source scope render keys before = Some (outputs,after) ->
  rf_names after = rf_names before /\ rf_generations after = rf_generations before /\
  rf_next after = rf_next before + length outputs.
Proof.
 intros source scope render keys;induction keys as[|current rest IH];
 intros before outputs after H;cbn[lower_result_first_order]in H.
 - inversion H;subst;cbn;repeat split;try reflexivity;lia.
 - destruct current as[v|f|receiver method|u];
   cbn[lower_result_first_order]in H.
   all:repeat match type of H with
   |context[nassoc ?name ?table]=>destruct(nassoc name table)eqn:Lookup
   |context[lookup_result_method ?r ?m ?table]=>destruct(lookup_result_method r m table)eqn:Lookup
   end;try discriminate.
   all:cbn[lower_value_provision lower_function_provision lower_method_provision]in H.
   all:match type of H with
   | context[lower_result_first_order ?src ?sc ?rendering ?remaining ?middle]=>
     destruct(lower_result_first_order src sc rendering remaining middle)as[[tail final]|]eqn:E
   end;try discriminate.
   all:inversion H;subst;destruct(IH _ _ _ E)as[Names[Generations Next]];
     cbn in *;repeat split;try congruence;lia.
Qed.

Record reify_functor_interfaces : Type := ReifyFunctorInterfaces {
  rfi_source : usym;
  rfi_slot : usym;
  rfi_input_scope : reify_scope ety key;
  rfi_output_scope : reify_scope ety key;
  rfi_input_types : subst;
  rfi_output_types : subst;
  rfi_input_items : smap;
  rfi_output_items : smap;
  rfi_inputs : tele;
  rfi_outputs : tele;
  rfi_pending_inputs : list key;
  rfi_pending_outputs : list key;
  rfi_input_runtime : reify_state;
  rfi_output_runtime : reify_state
}.
Definition functor_keys (keys : list key) : list key :=
  filter (fun k => match k with KyFun _ => true | _ => false end) keys.
Definition enter_result_names (source : st) (scope : reify_scope ety key)
    (roles : subst) (input : reify_state) (allocated : reify_state) : reify_state :=
  let render := render_functor_result_type source scope roles input in
  ReifyState (rf_next allocated)
    (map (fun p => (fst p,render (snd p))) (rs_names ety key scope))
    (roles ++ rf_generations input) (rf_items allocated) (rf_declarations allocated).

(* [output_generations] are the explicit checked existential placeholders,
   not all generations in the final snapshot: the latter also contains inputs. *)
Definition prepare_functor_interfaces (source : st) (original : usym)
    (input_scope output_scope : reify_scope ety key)
    (output_generations : list tsym) (result_keys : list key) (defaults : subst)
    (before : reify_state) : option (reify_functor_interfaces * reify_state) :=
  let slot := rf_next before in
  let blank := ReifyState (S slot) [] [] [] (rf_declarations before) in
  let input_types := lower_source_type_interface source input_scope blank in
  let '(first_inputs,input_allocated) := lower_scope_first_order source input_scope (sti_state input_types) in
  let input := install_generation_defaults defaults input_allocated in
  let '(output_types,_,output_allocated) := allocate_type_roles (nodup Nat.eq_dec output_generations) input in
  let render := render_functor_result_type source output_scope output_types input in
  match lower_result_first_order source output_scope render result_keys output_allocated with
  | None => None
  | Some (first_outputs,output_allocated) =>
      let output := enter_result_names source output_scope output_types input output_allocated in
      Some (ReifyFunctorInterfaces original slot input_scope output_scope
        (sti_roles input_types) output_types (rev (rf_items input))
        (rev (firstn (length first_outputs) (rf_items output)))
        (type_role_telescope defaults ++ type_role_telescope (sti_roles input_types) ++ first_inputs)
        (type_role_telescope output_types ++ first_outputs)
        (functor_keys (rs_items ety key input_scope)) (functor_keys result_keys)
        input output,restore_reify_scope before output)
  end.

Lemma prepared_functor_interfaces_restore_outer_scope : forall source original inputs outputs generations keys defaults before interfaces after,
  prepare_functor_interfaces source original inputs outputs generations keys defaults before = Some (interfaces,after) ->
  rf_names after=rf_names before /\rf_generations after=rf_generations before /\rf_items after=rf_items before.
Proof.
 intros source original inputs outputs generations keys defaults before interfaces after H.
 unfold prepare_functor_interfaces in H.
 destruct(lower_scope_first_order _ _ _)as[first_inputs input_allocated].
 destruct(allocate_type_roles _ _)as[[out_types out_map]out_allocated].
 destruct(lower_result_first_order _ _ _ _ _)as[[first_outputs final]|];[|discriminate].
 inversion H;subst;repeat split;reflexivity.
Qed.

Lemma lower_result_first_order_item_stack : forall source scope render keys before outputs after,
  lower_result_first_order source scope render keys before = Some (outputs,after) ->
  map snd (rf_items after) = rev outputs ++ map snd (rf_items before).
Proof.
 intros source scope render keys;induction keys as[|current rest IH];
 intros before outputs after H;cbn[lower_result_first_order]in H.
 - inversion H;reflexivity.
 - destruct current as[v|f|receiver method|u];cbn[lower_result_first_order]in H.
   all:repeat match type of H with
   |context[nassoc ?name ?table]=>destruct(nassoc name table)eqn:Lookup
   |context[lookup_result_method ?r ?m ?table]=>destruct(lookup_result_method r m table)eqn:Lookup
   end;try discriminate.
   all:cbn[lower_value_provision lower_function_provision lower_method_provision]in H.
   all:match type of H with
   |context[lower_result_first_order ?src ?sc ?rendering ?remaining ?middle]=>
     destruct(lower_result_first_order src sc rendering remaining middle)as[[tail final]|]eqn:E
   end;try discriminate.
   all:inversion H;subst;rewrite(IH _ _ _ E);cbn;try rewrite <-app_assoc;reflexivity.
Qed.
Lemma lower_result_first_order_map_order : forall source scope render keys before outputs after,
  lower_result_first_order source scope render keys before = Some (outputs,after) ->
  map snd (rev (firstn (length outputs) (rf_items after))) = outputs.
Proof.
 intros source scope render keys before outputs after H.
 rewrite map_rev,<-firstn_map,(lower_result_first_order_item_stack _ _ _ _ _ _ _ H).
 rewrite firstn_app,length_rev,Nat.sub_diag,firstn_O,app_nil_r.
 rewrite firstn_all2;[apply rev_involutive|rewrite length_rev;lia].
Qed.

Example functor_interfaces_split_merged_input_and_new_output : forall source,
  let input_scope := ReifyScope ety key [(1,YAbs 1);(2,YAbs 1)]
    [(1,YAbs 1);(2,YAbs 1)] [] [] [] [] [] in
  let output_scope := ReifyScope ety key [(1,YAbs 1);(2,YAbs 2)]
    [(1,YAbs 1);(2,YAbs 1)] [KyVal 7] [(7,YAbs 2)] [] [] [] in
  let before := ReifyState 100 [] [] [] [] in
  match prepare_functor_interfaces source 9 input_scope output_scope [2] [KyVal 7] [] before with
  | Some (interfaces,after) =>
      rfi_inputs interfaces = [ITy 101] /\
      rfi_outputs interfaces = [ITy 102;IVal 103] /\
      rfi_output_items interfaces = [(IVal 7,IVal 103)] /\
      rf_names after = [] /\ rf_next after = 104
  | None => False end.
Proof. intros;repeat split;reflexivity. Qed.

(* All instances of one checked source interface reuse its formal keys.
   Captured interpretations vary per immutable slot, never per formal key. *)
Fixpoint lookup_functor_template (original : usym) (views : list reify_functor_view)
    : option reify_functor_view :=
  match views with
  | [] => None
  | view :: rest => if Nat.eqb original (rfv_source view) then Some view
      else lookup_functor_template original rest
  end.
Definition install_functor_view (view : reify_functor_view) (before : reify_state)
    : reify_state :=
  ReifyState (rf_next before) (rf_names before) (rf_generations before)
    ((IFun (rfv_source view),IFun (rfv_slot view)) :: rf_items before)
    ((IFun (rfv_slot view),RDFunctor (rfv_schema view)) :: rf_declarations before).
Definition instantiate_functor_template (template : reify_functor_view)
    (fixed_inst : subst) (captures : tele) (pins : subst) (before : reify_state)
    : reify_functor_view * reify_state :=
  let base := rfv_schema template in
  let schema := ReifyFunctorDecl (rfd_inputs base) (rfd_outputs base)
    (rfd_fixed base) fixed_inst (rfd_poly base) captures pins in
  let '(slot,after) := lower_functor_copy (rfv_source template) schema before in
  (ReifyFunctorView (rfv_source template) slot (rfv_input_scope template)
    (rfv_output_scope template) (rfv_input_types template) (rfv_input_items template)
    (rfv_output_types template) (rfv_output_items template) schema,after).

Lemma instantiated_functor_template_interface : forall template fixed captures pins before view after,
  instantiate_functor_template template fixed captures pins before=(view,after) ->
  rfd_inputs(rfv_schema view)=rfd_inputs(rfv_schema template) /\
  rfd_outputs(rfv_schema view)=rfd_outputs(rfv_schema template) /\
  rfd_fixed_inst(rfv_schema view)=fixed /\
  rfv_input_items view=rfv_input_items template /\
  rfv_output_items view=rfv_output_items template /\
  rfv_slot view=rf_next before /\rf_next after=S(rf_next before).
Proof. intros;inversion H;subst;repeat split;reflexivity. Qed.

Fixpoint allocate_application_type_outputs (roles : subst)
    (arguments : list (tsym * ety)) (before : reify_state)
    : option (smap * subst * reify_state) :=
  match roles with
  | [] => Some ([],[],before)
  | (name,TAbs formal) :: rest =>
      match nassoc name arguments with
      | Some (YAbs generation) =>
          let '(slot,middle) := lower_output_generation name generation before in
          match allocate_application_type_outputs rest arguments middle with
          | Some (outputs,types,after) =>
              Some ((ITy slot,ITy formal)::outputs,(formal,TAbs slot)::types,after)
          | None => None end
      | _ => None end
  | _ :: _ => None
  end.
Definition type_transfer_subst (inputs : smap) : subst :=
  flat_map (fun p => match p with
    | (ITy formal,ITy actual) => [(formal,TAbs actual)]
    | _ => [] end) inputs.
Definition first_order_result_items (items : tele) : tele :=
  filter (fun i => match i with IVal _|IFn _|IMth _ _ _=>true|_=>false end) items.
Definition functor_result_items (items : tele) : tele :=
  filter (fun i => match i with IFun _=>true|_=>false end) items.
Definition same_result_kind (actual formal : item) : bool :=
  match actual,formal with
  | ITy _,ITy _|IVal _,IVal _|IFn _,IFn _|IMth _ _ _,IMth _ _ _|IFun _,IFun _=>true
  | _,_=>false end.
Fixpoint pair_result_items (actual formal : tele) : option smap :=
  match actual,formal with
  | [],[]=>Some []
  | a::actual,f::formal =>
      if same_result_kind a f then match pair_result_items actual formal with
        | Some rest=>Some((a,f)::rest)|None=>None end else None
  | _,_=>None end.

(* Lexical substitutions act on captured fixed images. Nonfixed rows are
   equations on a future invocation's own formal inputs and retain scope. *)
Definition map_fixed_interpretation (fixed : list tsym) (sigma interpretation : subst) : subst :=
  map(fun p=>(fst p,if existsb(Nat.eqb(fst p))fixed
      then ExecutionModel.app_subst sigma(snd p)else snd p))interpretation.
Lemma map_fixed_interpretation_lookup : forall fixed sigma interpretation key,
  lookup_subst key(map_fixed_interpretation fixed sigma interpretation)=
  option_map(fun meaning=>if existsb(Nat.eqb key)fixed
    then ExecutionModel.app_subst sigma meaning else meaning)(lookup_subst key interpretation).
Proof.
 intros fixed sigma interpretation;induction interpretation as[|[name meaning]rest IH];intro key;cbn.
 - reflexivity.
 - destruct(Nat.eqb key name)eqn:E.
   + apply Nat.eqb_eq in E;subst;reflexivity.
   + exact(IH key).
Qed.

Fixpoint allocate_application_functor_outputs (keys : list key) (formal : tele)
    (parent_types : subst) (before : reify_state) (work : functor_work)
    : option (smap * reify_state * functor_work) :=
  match keys,formal with
  | [],[]=>Some([],before,work)
  | KyFun name::keys,IFun slot::formal =>
      match lookup_functor_view slot (fw_views work) with
      | None=>None
      | Some template=>
          if Nat.eqb name (rfv_source template) then
            let schema:=rfv_schema template in
            let fixed:=map_fixed_interpretation(rfd_fixed schema)parent_types(rfd_fixed_inst schema) in
            let '(view,middle):=instantiate_functor_template template fixed
              (rfd_captures schema) (rfd_pins schema) before in
            let middle_work:=remember_functor_view view work in
            match allocate_application_functor_outputs keys formal parent_types middle middle_work with
            | Some(outputs,after,after_work)=>Some((IFun(rfv_slot view),IFun slot)::outputs,after,after_work)
            | None=>None end
          else None
      end
  | _,_=>None end.

Lemma pair_result_items_domain : forall actual formal outputs,
  pair_result_items actual formal=Some outputs ->map fst outputs=actual /\map snd outputs=formal.
Proof.
 induction actual as[|a actual IH];intros[|f formal]outputs H;cbn in H;try discriminate.
 - injection H as <-;split;reflexivity.
 - destruct(same_result_kind a f);[|discriminate].
   destruct(pair_result_items actual formal)as[rest|]eqn:E;[|discriminate].
   injection H as <-;destruct(IH formal rest E)as[A B];cbn;split;f_equal;assumption.
Qed.

Fixpoint resolve_functor_role_arguments (roles : subst) (schema : reify_functor_decl)
    (arguments : subst) (runtime : reify_state) : option subst :=
  match roles with
  | []=>Some []
  | (generation,TAbs formal)::rest=>
      let meaning:=if existsb(Nat.eqb formal)(rfd_fixed schema)
        then lookup_subst formal(rfd_fixed_inst schema)
        else match lookup_subst generation arguments with
          | Some meaning=>Some meaning
          | None=>lookup_subst generation(rf_generations runtime)end in
      match meaning,resolve_functor_role_arguments rest schema arguments runtime with
      | Some tau,Some tail=>Some((formal,tau)::tail)|_,_=>None end
  | _::_=>None end.
Definition prepare_functor_input_transfer (source : st) (view : reify_functor_view)
    (source_map : smap) (arguments : subst) (before : reify_state)
    : option(list(tsym*ty)*smap*reify_state):=
  let scope:=rfv_input_scope view in
  let patterns:=map(fun p=>(fst p,reify_snapshot_type source scope(snd p)))
    (rs_names ety key scope) in
  match solve_generation_arguments(map fst(rfv_input_types view))patterns arguments before[] with
  | None=>None
  | Some solved=>
      match resolve_functor_role_arguments(rfv_input_types view)(rfv_schema view)solved before,
          resolve_item_transfers(rfv_input_items view)source_map before with
      | Some types,Some items=>
          let '(bindings,type_map,after):=materialize_type_arguments types before in
          Some(bindings,type_map++items,after)
      | _,_=>None end
  end.

Definition lower_checked_functor_application (source : st) (defaults : subst)
    (name : usym) (source_map : smap) (input_arguments output_arguments : list (tsym * ety))
    (output_keys : list key) (output_scope : reify_scope ety key)
    (before : reify_local) (work : functor_work)
    : option (list xbind * reify_local * functor_work) :=
  match lookup_smap (IFun name) (rf_items (rl_runtime before)) with
  | Some (IFun slot) =>
      match lookup_functor_view slot (fw_views work) with
      | None => None
      | Some view =>
          let arguments:=map(fun p=>(fst p,tr(reify_field_number source)(snd p))) input_arguments in
          match prepare_functor_input_transfer source view source_map arguments (rl_runtime before) with
          | None=>None
          | Some(temporaries,inputs,input_runtime)=>
              let inputs:=global_default_transfer defaults++inputs in
              match allocate_application_type_outputs (rfv_output_types view) output_arguments input_runtime with
              | None=>None
              | Some(type_outputs,output_types,typed_runtime)=>
                  let render:=fun tau=>ExecutionModel.app_subst(rf_generations typed_runtime)
                    (reify_snapshot_type source output_scope tau) in
                  match lower_result_first_order source output_scope render output_keys typed_runtime with
                  | None=>None
                  | Some(first_outputs,first_runtime)=>
                      match pair_result_items first_outputs (first_order_result_items(rfd_outputs(rfv_schema view))) with
                      | None=>None
                      | Some first_map=>
                          let parent_types:=output_types++type_transfer_subst inputs in
                          match allocate_application_functor_outputs (functor_keys output_keys)
                              (functor_result_items(rfd_outputs(rfv_schema view))) parent_types first_runtime work with
                          | None=>None
                          | Some(functor_map,after,after_work)=>
                              Some(map(fun p=>XBTy(fst p)(snd p))temporaries++
                                [XBApplyR slot inputs (type_outputs++first_map++functor_map)],
                                ReifyLocal after (aliases_after_temporaries temporaries(rl_aliases before)),after_work)
                          end
                      end
                  end
              end
          end
      end
  | _=>None end.

Definition lower_functor_application (source : st) (code : reify_code ety key)
    (defaults : subst) (name : usym) (theta : subst) (source_map : smap)
    (before : reify_local) (work : functor_work)
    : option(list xbind*reify_local*functor_work):=
  match find_reify_annotation(application_annotation_matches source name theta)
      (rc_events ety key code)with
  | Some(REApply _ _ _ _ output_keys input_arguments output_arguments output_scope)=>
      lower_checked_functor_application source defaults name source_map input_arguments output_arguments
        output_keys output_scope before work
  | _=>None end.

Example application_uses_captured_fixed_type_and_explicit_output_generation : forall source,
  let input_scope:=ReifyScope ety key [] [] [] [] [] [] [] in
  let output_scope:=ReifyScope ety key [(2,YAbs 9)] [(9,YAbs 9)]
    [KyVal 8] [(8,YAbs 9)] [] [] [] in
  let schema:=ReifyFunctorDecl [ITy 10] [ITy 20;IVal 21]
    [10] [(10,TNom 7 [])] [] [] [] in
  let view:=ReifyFunctorView 4 50 input_scope output_scope [(1,TAbs 10)] []
    [(2,TAbs 20)] [(IVal 8,IVal 21)] schema in
  let before:=ReifyLocal(ReifyState 100 [(1,TNom 7 [])] [(1,TNom 3 [])]
    [(IFun 4,IFun 50)] []) [] in
  let work:=remember_functor_view view empty_functor_work in
  match lower_checked_functor_application source [] 4 [] [] [(2,YAbs 9)]
      [KyVal 8] output_scope before work with
  | Some(bindings,after,_)=>
      bindings=[XBTy 100 (TNom 7 []);
        XBApplyR 50 [(ITy 10,ITy 100)] [(ITy 101,ITy 20);(IVal 102,IVal 21)]] /\
      lookup_subst 1(rf_generations(rl_runtime after))=Some(TNom 3 []) /\
      lookup_subst 9(rf_generations(rl_runtime after))=Some(TAbs 101) /\
      lookup_smap(IVal 8)(rf_items(rl_runtime after))=Some(IVal 102)
  | None=>False end.
Proof. intros;repeat split;reflexivity. Qed.

Definition add_fixed_type_scope (scope : reify_scope ety key) (fixed : list tsym)
    : reify_scope ety key :=
  ReifyScope ety key
    (rs_names ety key scope ++ map(fun t=>(t,YAbs t))fixed)
    (rs_generations ety key scope ++ map(fun t=>(t,YAbs t))fixed)
    (rs_items ety key scope) (rs_values ety key scope) (rs_functions ety key scope)
    (rs_methods ety key scope) (rs_provisions ety key scope).
Definition scope_type_meaning (scope : reify_scope ety key) (name : tsym) : ety :=
  instantiate(env_with_edges(rs_generations ety key scope)[])
    (opt_default(YAbs name)(nassoc name(rs_names ety key scope))).
Definition solve_captured_fixed_types (scope : reify_scope ety key) (variables : list tsym)
    (fixed : list tsym) (arguments : list(tsym*ety)) : option(list(tsym*ety)):=
  fold_left(fun solution name=>match solution,nassoc name arguments with
    | Some solved,Some actual=>copy_match_type variables(scope_type_meaning scope name)actual solved
    | _,_=>None end)fixed(Some []).
Definition captured_fixed_interpretation (source : st) (roles : subst)
    (solution : list(tsym*ety)) (caller : reify_state) : subst :=
  flat_map(fun p=>match snd p,nassoc(fst p)solution with
    | TAbs formal,Some actual=>[(formal,ExecutionModel.app_subst(rf_generations caller)
        (tr(reify_field_number source)actual))]
    | _,_=>[] end)roles.
Definition instantiate_functor_fixed_types (source : st) (scope : reify_scope ety key)
    (roles : subst) (fixed : list tsym) (arguments : list(tsym*ety)) (caller : reify_state)
    : option subst :=
  match solve_captured_fixed_types scope (map fst roles) fixed arguments with
  | Some solution=>Some(captured_fixed_interpretation source roles solution caller)
  | None=>None end.

Lemma captured_fixed_interpretation_formals : forall source roles solution caller formal tau,
  In(formal,tau)(captured_fixed_interpretation source roles solution caller)->
  exists generation,In(generation,TAbs formal)roles.
Proof.
 intros source roles solution caller formal tau H.
 unfold captured_fixed_interpretation in H;apply in_flat_map in H as[[generation role][Hin H]].
 destruct role as[|formal_role|head args|members].
 - cbn in H;contradiction.
 - change (In (formal,tau) (match nassoc generation solution with
     | Some actual => [(formal_role,ExecutionModel.app_subst(rf_generations caller)
         (tr(reify_field_number source)actual))] | None => [] end)) in H.
   destruct(nassoc generation solution)as[actual|]eqn:E;[|contradiction].
   destruct H as[H|[]];injection H as <- <-;exists generation;exact Hin.
 - cbn in H;contradiction.
 - cbn in H;contradiction.
Qed.
