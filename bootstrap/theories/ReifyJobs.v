(* Provider jobs are reserved before their bodies are traversed. Recursive
   source bindings therefore reuse the reserved code frame. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel CoreMoss Elab ReifyData Reify ReifySource
  ReifyFunctors ReifyProviders ReifyFunctorProfiles ReifyExports ReifyPlan ReifySig ReifyMaps
  ReifyWalkData ReifyWalk ReifyProgramWalk ReifyAnnotations ReifyOpaqueFunctors.
Import ListNotations.

Definition provider_binding_has_annotation (config : walk_config) (binding : xbind)
    (before : walk_cursor) : bool :=
  match binding with
  | XBMth receiver method theta provider _=>
      existsb(method_annotation_matches(cfg_source config)
        (rl_aliases(walk_local before))(rl_runtime(walk_local before))
        receiver method theta provider)(rc_events ety key(cfg_code config))
  | _=>true end.
Fixpoint walk_provider_bindings (config : walk_config) (profile : reify_exports ety key)
    (fixed_names : list tsym) (previous : option xbind) (bindings : list xbind)
    (before : walk_cursor) : option(list xbind*walk_cursor) :=
  match bindings with
  | []=>Some([],before)
  | binding::rest=>
      let alias:=match previous with
        | Some original=>provider_method_alias_of(cfg_source config)profile fixed_names original binding
        | None=>false end in
      if alias&&negb(provider_binding_has_annotation config binding before)then
        walk_provider_bindings config profile fixed_names previous rest before
      else match reify_binding config binding before with
        | None=>None
        | Some(lowered,middle)=>
            match walk_provider_bindings config profile fixed_names(Some binding)rest middle with
            | Some(tail,after)=>Some(lowered++tail,after)
            | None=>None end
        end
  end.

Definition finish_provider_job (job : reify_functor_job) (exports : smap) : reify_functor_job :=
  ReifyFunctorJob(rfj_original job)(rfj_code job)(rfj_view job)(rfj_body_scope job)
    (rfj_body job)(rfj_type_captures job)(rfj_item_captures job)(Some exports).
Fixpoint replace_provider_job (job : reify_functor_job) (jobs : list reify_functor_job)
    : list reify_functor_job :=
  match jobs with
  | []=>[]
  | previous::rest=>(if Nat.eqb(rfj_original job)(rfj_original previous)then job else previous)
      ::replace_provider_job job rest
  end.
Definition complete_provider_work (job : reify_functor_job) (work : functor_work) : functor_work :=
  FunctorWork(fw_views work)(replace_provider_job job(fw_jobs work))(fw_adapters work).

Record compiled_provider_step : Type := CompiledProviderStep {
  cps_code : usym;
  cps_declaration : tele;
  cps_body : list xbind;
  cps_exports : smap;
  cps_functions : list(fsym*xexpr);
  cps_global : reify_state;
  cps_work : functor_work
}.
Definition compile_provider_job_reachable (source : st) (defaults : subst)
    (frames : list reify_function_frame) (job : reify_functor_job)
    (global : reify_state) (work : functor_work) : option compiled_provider_step :=
  match reify_code_for_symbol source(rfj_original job),
    source_functor_fixed source(rfv_source(rfj_view job))with
  | Some metadata,Some fixed_names=>match rc_exports ety key metadata with
      | None=>None
      | Some profile=>
          let local:=ReifyLocal
            (restore_reify_scope(rl_runtime(rfj_body_scope job))global)
            (rl_aliases(rfj_body_scope job)) in
          let config:=WalkConfig source defaults frames metadata in
          match walk_provider_bindings config profile fixed_names None(rfj_body job)
            (WalkCursor local work[])with
          | None=>None
          | Some(body,after)=>
              match provider_body_exports source profile fixed_names(rfj_view job)
                (rl_runtime(walk_local after))with
              | None=>None
              | Some exports=>Some(CompiledProviderStep(rfj_code job)(map snd exports)body exports
                  (walk_auxiliary after)
                  (restore_reify_scope global(rl_runtime(walk_local after)))
                  (complete_provider_work(finish_provider_job job exports)(walk_functors after)))
              end
          end
      end
  | _,_=>None end.

Definition compile_provider_job(source:st)(defaults:subst)
 (frames:list reify_function_frame)(job:reify_functor_job)
 (global:reify_state)(work:functor_work):option compiled_provider_step:=
 let schema:=rfv_schema(rfj_view job)in
 if scope_has_opaque_functor(rfd_inputs schema++rfd_captures schema)(rf_declarations global)
 then let outputs:=rfd_outputs schema in
      let exports:=map(fun i=>(i,i))outputs in
      Some(CompiledProviderStep(rfj_code job)outputs[]exports[]global
        (complete_provider_work(finish_provider_job job exports)work))
 else compile_provider_job_reachable source defaults frames job global work.

Definition next_provider_job (work : functor_work) : option reify_functor_job :=
  find(fun job=>match rfj_exports job with None=>true|Some _=>false end)(fw_jobs work).
Record compiled_provider_jobs : Type := CompiledProviderJobs {
  cpj_functors : list(usym*(tele*list xbind));
  cpj_exports : list(usym*smap);
  cpj_functions : list(fsym*xexpr);
  cpj_global : reify_state;
  cpj_work : functor_work
}.
Fixpoint compile_provider_jobs (remaining : nat) (source : st) (defaults : subst)
    (frames : list reify_function_frame) (global : reify_state) (work : functor_work)
    : option compiled_provider_jobs :=
  match next_provider_job work with
  | None=>Some(CompiledProviderJobs[][][]global work)
  | Some job=>match remaining with
      | 0=>None
      | S rest=>match compile_provider_job source defaults frames job global work with
          | None=>None
          | Some step=>match compile_provider_jobs rest source defaults frames(cps_global step)(cps_work step)with
              | None=>None
              | Some tail=>Some(CompiledProviderJobs
                  ((cps_code step,(cps_declaration step,cps_body step))::cpj_functors tail)
                  ((cps_code step,cps_exports step)::cpj_exports tail)
                  (cps_functions step++cpj_functions tail)(cpj_global tail)(cpj_work tail))
              end
          end
      end
  end.

Definition compile_all_provider_jobs (source : st) (defaults : subst)
    (frames : list reify_function_frame) (global : reify_state) (work : functor_work) :=
  compile_provider_jobs(List.length(st_funs source))source defaults frames global work.

Lemma next_provider_job_pending : forall work job,
  next_provider_job work=Some job->In job(fw_jobs work)/\rfj_exports job=None.
Proof.
 intros work job H;apply find_some in H as[Hin Hpending];split;[exact Hin|].
 destruct(rfj_exports job);[discriminate|reflexivity].
Qed.

(* Adapter bodies are already immutable. Their actual generated declaration
   supplies the canonical output telescope and input type keys. *)
Fixpoint compile_adapter_tables (declarations : list(item*reify_decl))
    (adapters : list(usym*list xbind))
    : option(list(usym*(tele*list xbind))*list(usym*smap)*list(usym*list tsym)) :=
  match adapters with
  | []=>Some([],[],[])
  | (code,body)::rest=>match lookup_reify_functor code declarations,
      compile_adapter_tables declarations rest with
      | Some schema,Some(functions,exports,inputs)=>
          Some((code,(rfd_outputs schema,body))::functions,
            (code,identity_smap(rfd_outputs schema))::exports,
            (code,tyreqs(rfd_inputs schema))::inputs)
      | _,_=>None end
  end.

Lemma compiled_jobs_have_no_pending_body : forall remaining source defaults frames global work result,
  compile_provider_jobs remaining source defaults frames global work=Some result->
  next_provider_job(cpj_work result)=None.
Proof.
 induction remaining as[|remaining IH];intros source defaults frames global work result H;
   cbn[compile_provider_jobs]in H;
   destruct(next_provider_job work)as[job|]eqn:E.
 - discriminate.
 - inversion H;subst;exact E.
 - destruct(compile_provider_job source defaults frames job global work)as[step|]eqn:S;[|discriminate].
   destruct(compile_provider_jobs remaining source defaults frames(cps_global step)(cps_work step))
     as[tail|]eqn:T;[|discriminate].
   inversion H;subst;cbn;eapply IH;exact T.
 - inversion H;subst;exact E.
Qed.
