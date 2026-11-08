(* Finite source-code frames are allocated before any body walk. Every
   recursive call therefore sees the same immutable formal interface. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel CoreMoss Elab ReifyData Reify ReifySource
  ReifyInitial ReifyPlan ReifyReach ReifyFunctors ReifyFunctorProfiles
  ReifyProfileSupport ReifyOpaqueFunctors ReifyWalkData ReifyWalk ReifyEta ReifyTransfers ReifyInterfaces.
Import ListNotations.

Fixpoint function_frame_specifications_nodes (source : st) (nodes : list reify_node)
    : option(list(fsym * reify_scope ety key * reify_signature ety)) :=
  match nodes with
  | []=>Some[]
  | RnFunctor _::rest=>function_frame_specifications_nodes source rest
  | RnFunction f::rest=>
      match assoc reify_owner_eqb (ROFunction f) (rm_codes(st_reify source)),
        function_frame_specifications_nodes source rest with
      | Some code,Some tail=>match rc_signature ety key code with
          | Some signature=>Some((f,rc_scope ety key code,signature)::tail)
          | None=>match source_eta_callee source f with Some _=>Some tail|None=>None end end
      | _,_=>None end
  end.
Definition function_frame_specifications (source : st) (raw : xprogram)
    : option(list(fsym * reify_scope ety key * reify_signature ety)) :=
  function_frame_specifications_nodes source(reify_reachable_nodes raw).

Definition function_frame_node_ready (source : st) (node : reify_node) : Prop :=
  match node with
  | RnFunctor _=>True
  | RnFunction f=>(exists code,
      assoc reify_owner_eqb(ROFunction f)(rm_codes(st_reify source))=Some code /\
      (exists signature,rc_signature ety key code=Some signature)) \/
      (exists code original,assoc reify_owner_eqb(ROFunction f)(rm_codes(st_reify source))=Some code /\
        rc_signature ety key code=None /\source_eta_callee source f=Some original)
  end.

Lemma function_frame_specifications_nodes_total : forall source nodes,
  Forall(function_frame_node_ready source)nodes->
  exists result,function_frame_specifications_nodes source nodes=Some result.
Proof.
 intros source nodes H;induction H as[|node rest Ready Rest IH].
 - exists[];reflexivity.
 - destruct IH as[tail Tail]. destruct node as[f|u];cbn in Ready |- *.
   + destruct Ready as[[code[Code[signature Sig]]]|[code[original[Code[Sig Eta]]]]];
       rewrite Code,Tail,Sig.
     * eexists;reflexivity.
     * rewrite Eta;eexists;reflexivity.
   + exists tail;exact Tail.
Qed.

Definition present_captured_types (arguments : list(tsym*option ety)) : list(tsym*ety) :=
  flat_map(fun p=>match snd p with Some meaning=>[(fst p,meaning)]|None=>[]end)arguments.

Fixpoint install_function_functor_items (source : st) (defaults : subst)
    (scope : reify_scope ety key) (todo : list key)
    (before : reify_state) (work : functor_work)
    : option(tele * reify_state * functor_work) :=
  match todo with
  | []=>Some([],before,work)
  | KyFun original::rest=>
      match lookup_functor_template original(fw_views work),
        assoc key_eqb(KyFun original)(rs_provisions ety key scope),
        source_functor_fixed source original with
      | Some template,Some(arguments,_),Some fixed=>
          let arguments:=present_captured_types arguments in
              match instantiate_functor_fixed_types source(rfv_input_scope template)
                (rfv_input_types template)fixed arguments before with
              | Some interpretation=>
                  let hidden:=map(fun t=>(t,TAbs t))(tyreqs(type_role_telescope defaults)) in
                  let '(view,middle):=instantiate_functor_template template
                    (interpretation++hidden++source_role_equations source
                      (rfv_input_scope template)(rfv_input_types template))[][]before in
                  let middle_work:=remember_functor_view view work in
                  match install_function_functor_items source defaults scope rest middle middle_work with
                  | Some(items,after,after_work)=>Some(IFun(rfv_slot view)::items,after,after_work)
                  | None=>None end
              | None=>None end
      | None,_,_=>if has_opaque_original original(rf_declarations before)then
          let '(slot,middle):=install_opaque_functor original before in
          match install_function_functor_items source defaults scope rest middle work with
          | Some(items,after,after_work)=>Some(IFun slot::items,after,after_work)
          | None=>None end
        else None
      | _,_,_=>None end
  | _::_=>None
  end.

Fixpoint complete_function_frames (source : st) (defaults : subst)
    (todo : list reify_function_frame) (global : reify_state) (work : functor_work)
    : option(list reify_function_frame * reify_state * functor_work) :=
  match todo with
  | []=>Some([],global,work)
  | frame::rest=>
      match install_function_functor_items source defaults(rff_source_scope frame)
        (rff_pending_functors frame)(enter_function_frame frame global)work with
      | None=>None
      | Some(items,local,middle_work)=>
          let completed:=complete_function_frame frame items local in
          let middle:=declare_function_frame completed(restore_reify_scope global local) in
          match complete_function_frames source defaults rest middle middle_work with
          | Some(frames,after,after_work)=>Some(completed::frames,after,after_work)
          | None=>None end
      end
  end.

Record reified_function_bodies : Type := ReifiedFunctionBodies {
  rfb_functions : list(fsym*xexpr);
  rfb_global : reify_state;
  rfb_functors : functor_work
}.
Definition reify_function_body(config:walk_config)(frame:reify_function_frame)
 (body:xexpr)(before:walk_cursor):option(xexpr*walk_cursor):=
 if scope_has_opaque_functor(rff_inputs frame)(rf_declarations(rl_runtime(walk_local before)))
 then Some(XUnit,before)else reify_expression config body before.

Fixpoint walk_function_bodies (source : st) (raw : xprogram) (defaults : subst)
    (all todo : list reify_function_frame) (global : reify_state) (work : functor_work)
    : option reified_function_bodies :=
  match todo with
  | []=>Some(ReifiedFunctionBodies[]global work)
  | frame::rest=>
      match assoc Nat.eqb(rff_source frame)(xo_fns raw),
        assoc reify_owner_eqb(ROFunction(rff_source frame))(rm_codes(st_reify source)) with
      | Some body,Some code=>
          let config:=WalkConfig source defaults all code in
          let before:=WalkCursor(ReifyLocal(enter_function_frame frame global)[])work[] in
          match reify_function_body config frame body before with
          | None=>None
          | Some(lowered,after)=>
              let middle:=restore_reify_scope global(rl_runtime(walk_local after)) in
              match walk_function_bodies source raw defaults all rest middle(walk_functors after)with
              | None=>None
              | Some result=>Some(ReifiedFunctionBodies
                  ((rff_code frame,lowered)::walk_auxiliary after++rfb_functions result)
                  (rfb_global result)(rfb_functors result))
              end
          end
      | _,_=>None end
  end.

Lemma complete_function_frames_sources : forall source defaults todo global work frames after after_work,
  complete_function_frames source defaults todo global work=Some(frames,after,after_work)->
  map rff_source frames=map rff_source todo.
Proof.
 intros source defaults todo;induction todo as[|frame rest IH];
   intros global work frames after after_work H;cbn in H.
 - inversion H;reflexivity.
 - destruct(install_function_functor_items source defaults(rff_source_scope frame)
     (rff_pending_functors frame)(enter_function_frame frame global)work)
     as[[[items local]middle_work]|]eqn:E;[|discriminate].
   destruct(complete_function_frames source defaults rest
     (declare_function_frame(complete_function_frame frame items local)
       (restore_reify_scope global local))middle_work)
     as[[[tail final]final_work]|]eqn:F;[|discriminate].
   inversion H;subst;cbn;f_equal;eapply IH;exact F.
Qed.

Lemma function_body_walk_keeps_outer_scope : forall source raw defaults all todo global work result,
  walk_function_bodies source raw defaults all todo global work=Some result->
  rf_names(rfb_global result)=rf_names global /\
  rf_generations(rfb_global result)=rf_generations global /\
  rf_items(rfb_global result)=rf_items global.
Proof.
 intros source raw defaults all todo;induction todo as[|frame rest IH];
   intros global work result H;cbn in H.
 - inversion H;repeat split;reflexivity.
 - destruct(assoc Nat.eqb(rff_source frame)(xo_fns raw))as[body|];[|discriminate].
   destruct(assoc reify_owner_eqb(ROFunction(rff_source frame))(rm_codes(st_reify source)))as[code|];[|discriminate].
   destruct(reify_function_body(WalkConfig source defaults all code)frame body
     (WalkCursor(ReifyLocal(enter_function_frame frame global)[])work[]))
     as[[lowered after]|]eqn:E;[|discriminate].
   destruct(walk_function_bodies source raw defaults all rest
     (restore_reify_scope global(rl_runtime(walk_local after)))(walk_functors after))
     as[tail|]eqn:F;[|discriminate].
   inversion H;subst;exact(IH _ _ _ F).
Qed.

(* Main's source type-name rows are not immutable generation IDs either.
   Solve the checked main profile against the actual raw platform type rows,
   then replace unresolved original generations by their closed descriptors. *)
Definition initial_frame_type_entries (source : st) (raw : xprogram)
    (frame : reify_function_frame) (descriptors : subst) : option xdenv :=
  let runtime:=ReifyState 0 [] descriptors [] [] in
  match solve_generation_arguments(map fst(rff_type_roles frame))
    (frame_name_patterns source frame)(xdtypes(xo_env raw))runtime[]with
  | None=>None
  | Some solutions=>match resolve_role_solutions(rff_type_roles frame)solutions runtime with
      | Some arguments=>Some(map(fun p=>(ITy(fst p),XDType(snd p)))arguments)
      | None=>None end
  end.

Record prepared_function_stage : Type := PreparedFunctionStage {
  pfs_initial : reified_initial_types;
  pfs_defaults : subst;
  pfs_frames : list reify_function_frame;
  pfs_main : reify_function_frame;
  pfs_global : reify_state;
  pfs_functors : functor_work
}.
Definition prepare_function_stage (source : st) (raw : xprogram)
    : option prepared_function_stage :=
  let initial:=lower_global_defaults source in
  let defaults:=sti_roles(rit_interface initial) in
  match function_frame_specifications source raw,
    prepare_supported_functor_templates source raw defaults(sti_state(rit_interface initial))with
  | Some specifications,Some(work,global)=>
      let '(frames,middle):=prepare_function_frames source specifications global in
      let attached:=map(attach_function_defaults defaults)frames in
      match complete_function_frames source defaults attached middle work with
      | Some(completed,after,after_work)=>
          match lookup_function_frame(xo_main raw)completed with
          | Some main=>Some(PreparedFunctionStage initial defaults completed main after after_work)
          | None=>None end
      | None=>None end
  | _,_=>None end.
Definition walk_prepared_functions (source : st) (raw : xprogram)
    (prepared : prepared_function_stage) : option reified_function_bodies :=
  walk_function_bodies source raw(pfs_defaults prepared)(pfs_frames prepared)
    (pfs_frames prepared)(pfs_global prepared)(pfs_functors prepared).
