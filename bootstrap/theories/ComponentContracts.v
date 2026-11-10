(* Component obligations and milestones, separate from the five public
   build/run obligations in Audit.v. These contracts concern ExecutionModel. The old oracle bound them in
   Certified.v, deleted when Audit.v's implementation became the reference. *)
From Stdlib Require Import List String PrimInt63.
From NM Require Export ExecutionModel CoreFragment.
From NM Require Sx.
Import ListNotations.
(* D1 as a property of the relation: which callee a method call means is
   determined by the context and the key — the case analysis of
   MethCall/Provided has at most one applicable case, and each reads a
   function (a scope, the attached table, the context's table at the key).
   A family application's θ is not part of the choice (DRAFT NOTE 7). *)
Inductive choice := CDirect (f : nat) | CProv (i : item) | CFamily (u : nat) (i : item).

(* The provision chosen is its identity, the key without θ: θ is where the
   provision was made (Part V, note 28), and for a family application the
   application's own θ (DRAFT NOTE 7). *)

Definition provision_id (it : item) : item := Elaboration.provision_id it.

Fixpoint choice_of (x : xexpr) : option choice :=
  match x with
  | XCallD f _ _ _ => Some (CDirect f)
  | XMeth _ _ it _ => Some (CProv (provision_id it))
  | XApply u _ _ (XMeth _ _ it _) => Some (CFamily u (provision_id it))
  | XApply _ _ _ rest => choice_of rest
  | _ => None
  end.

(* What the old oracle's Certified.v bound.  Read the comment on each. *)
(* The lexer agrees with Lexes on the bytes it receives. The parser
   agrees with Derives, which includes source normalization before lexing. *)
Module Type FRONT_END.
Import Stdlib.Strings.Byte.
  Parameter lex : list byte -> option (list Syntax.token).
  Axiom lexer_sound : forall bytes ts, lex bytes = Some ts -> Syntax.Lexes 0 bytes ts.
  Axiom lexer_complete : forall bytes ts, Syntax.Lexes 0 bytes ts -> lex bytes = Some ts.

  Parameter parse : string -> Sx.res Input.afile.
  Axiom parser_sound : forall s f, parse s = Sx.Good f -> Syntax.Derives (list_byte_of_string s) f.
  Axiom parser_complete : forall s f, Syntax.Derives (list_byte_of_string s) f -> parse s = Sx.Good f.
End FRONT_END.

(* The loader's contract: a bundle loaded from a file system `read` is
   what Loader.Bundled says (Part I.3): every module a file whose bytes
   derive its AST, each once, closed under imports and the prelude.  It
   is soundness only: a loader that fails on every input meets it (that
   one which reads and derives every file succeeds is not stated). *)
Module Type LOADER.
  Parameter load : (string -> option string) -> string -> string -> string -> Sx.res Input.bundle.
  Axiom load_sound : forall read root prelude entry b,
    load read root prelude entry = Sx.Good b -> Loader.Bundled read root prelude entry b.
End LOADER.

Module Type PROVED_NOW.
  Parameter elaborate : Input.bundle -> Sx.res xprogram.

  Parameter interp : nat -> gsig -> denv -> venv -> expr -> res.

  (* Core Moss's dynamics: big-step evaluation is deterministic, and the
     fueled interpreter is adequate for it (both directions, on Ok). *)
  Parameter eval_deterministic : forall Sg d g e w1 w2,
    eval Sg d g e w1 -> eval Sg d g e w2 -> w1 = w2.
  Parameter interp_sound : forall fuel Sg d g e w,
    interp fuel Sg d g e = Ok w -> eval Sg d g e w.
  Parameter interp_complete : forall Sg d g e w,
    eval Sg d g e w -> exists fuel, interp fuel Sg d g e = Ok w.

  (* Specialization: substituting a grounding into an elaborated term's
     type annotations does not change what it evaluates to.  (The stronger
     theorems of Specialization.v — specialized code is monomorphically
     well-typed — are about the typing judgments, which are proof devices
     and stay out of this file.) *)
  Parameter spec_eval : forall Sg d ge g e w,
    eval Sg d ge (spec g e) w <-> eval Sg d ge e w.

  (* MossX, over the primitive semantics the implementation supplies. *)
  Declare Module Prims : PRIM_SEMANTICS.
  Module X := XSem Prims.

  (* On embedded core programs MossX computes exactly what Core Moss
     does, for every fuel (Ok, Err, OutOfFuel), touching neither the
     locals nor the machine. *)
  Parameter xinterp_conservative : forall fuel Sg d g e st,
    res_agree (interp fuel Sg d g e)
              (X.xr_out (X.xinterp fuel (emb_prog Sg) (emb_denv d) (emb_venv g) st (emb_expr e)))
    /\ X.xr_g (X.xinterp fuel (emb_prog Sg) (emb_denv d) (emb_venv g) st (emb_expr e)) = emb_venv g
    /\ X.xr_st (X.xinterp fuel (emb_prog Sg) (emb_denv d) (emb_venv g) st (emb_expr e)) = st.

  (* The primitives' law. *)
  Parameter prim_law : forall P p ps r ws st,
    prim_sig p = Some (ps, r) -> Forall2 (xvty P) ws ps ->
    match fst (Prims.run_prim P p ws st) with
    | XOk w => xvty P w r
    | XTrap _ | XExit _ => True
    | _ => False
    end.

  (* Track B: the same law at the memory primitives' method signatures. *)
  Parameter prim_mlaw : forall P p ps r ws st,
    prim_msig p = Some (ps, r) -> Forall2 (xvty P) ws ps ->
    match fst (Prims.run_prim P p ws st) with
    | XOk w => xvty P w r
    | XTrap _ | XExit _ => True
    | _ => False
    end.

  (* Track B: a primitive the model does not implement stops the program
     (XUns), whatever its arguments. *)
  Parameter prim_ulaw : forall P c ws st,
    fst (Prims.run_prim P (PUnsupported c) ws st) = XUns c.

  (* Core safety, through a typing predicate the implementation chooses.
     `Typed` is a proof device: an auditor need not read its definition.
     What makes these two obligations worth anything is (a) the pending
     `elaborated_typed` of COMPLETE, which ties `Typed` to elaboration, and
     (b) `typed_inhabited`, which rules out `Typed := False`: a program with
     functors and an existential result is Typed.  By themselves they say
     nothing about any Moss program. *)
  Parameter Typed : gsig -> fsym -> Prop.
  Parameter typed_safe : forall Sg main body fuel,
    Typed Sg main -> g_fn Sg main = Some ([], mkSig [] TUnit, Some body) ->
    interp fuel Sg [] [] body <> Err.
  Parameter typed_inhabited : exists Sg main u,
    Typed Sg main /\ g_fun Sg u <> None.

  (* D1, search-free resolution, as a property of the elaboration relation:
     two elaborations of one method call in one context choose one callee. *)
  Parameter method_choice_unique : forall b W P F C ox ot pa args x1 t1 x2 t2,
    Elaboration.WorldOf b W ->
    Elaboration.MethCall W P F C ox ot pa args x1 t1 ->
    Elaboration.MethCall W P F C ox ot pa args x2 t2 ->
    choice_of x1 = choice_of x2.
End PROVED_NOW.

(* Store typing and the laws that the concrete GC primitives satisfy. *)


Module SourceReading.
Local Unset Implicit Arguments.
Import Input Elaboration.

(* The fields of WorldOf about the loader: everything but requirements and
   field numbering. *)
Record LoaderReading (b : bundle) (W : world) : Prop := {
  lr_acyclic : exists rank : string -> nat, forall p m t imp,
      Loaded b p -> module_of b p = Some m -> In (t, imp) (imports_of b m) ->
      module_of b t <> None /\ rank t < rank p;
  lr_decl_sound : forall s d, w_decl W s = Some d -> s <> 0 /\ Declared b d /\ kind_of (d_ast d) <> None;
  lr_decl_complete : forall d, Declared b d -> kind_of (d_ast d) <> None ->
      exists s, w_decl W s = Some d /\ forall s', w_decl W s' = Some d -> s' = s;
  lr_exports : forall p ns n e, Loaded b p -> (w_exp W p ns n = Some e <-> Exports b W p ns n e);
  lr_scope : forall p ns n e, Loaded b p -> (w_scope W p ns n = Some e <-> InScope b W p ns n e);
  lr_attached : forall r n s, w_attached W r n = Some s <-> AttachedDecl W r n s;
  lr_library : w_library W = Loader.library_dir (b_prelude b)
}.

(* The rest: requirements ([D71]/[D73]) and field numbering. *)
Record ReqsReading (W : world) : Prop := {
  rr_nodup : forall s, NoDup (w_reqs W s);
  rr_plain : forall s k, kind_sym W s = Some k ->
      k <> DKTag -> k <> DKAlias -> k <> DKType -> k <> DKUnit ->
      forall t, In t (w_reqs W s) <-> AssumeTypes W s t;
  rr_none : forall s, kind_sym W s = Some DKType \/ kind_sym W s = Some DKUnit -> w_reqs W s = [];
  rr_closed : RelevanceClosed W (w_reqs W);
  rr_least : forall R, RelevanceClosed W R ->
      (forall s, kind_sym W s <> Some DKTag -> kind_sym W s <> Some DKAlias -> R s = w_reqs W s) ->
      forall g, (kind_sym W g = Some DKTag \/ kind_sym W g = Some DKAlias) -> incl (w_reqs W g) (R g);
  rr_listed : forall g d ps t n, w_decl W g = Some d -> nominal_body (d_ast d) = Some (ps, t) -> In n ps ->
      exists x, resolve_sym W (d_mod d) [n] = Some x /\ AssumeTypes W g x
}.

End SourceReading.

(* Checked milestones toward whole-source elaboration and runtime typing.
   Metadata and frame specifications are artifacts of the implementation. *)
Module Type SOURCE_PROGRESS (C : PROVED_NOW).
  Parameter metadata frame_specs loaded_world : Type.
  Parameter load_world : Input.bundle -> Sx.res loaded_world.
  Parameter read_world :
    loaded_world -> (nat -> list nat) -> (string -> nat) -> Elaboration.world.
  Parameter module_ids : loaded_world -> list nat.
  Parameter module_path : loaded_world -> nat -> string.
  Parameter resolve_path :
    loaded_world -> nat -> Input.path -> Sx.res (option Elaboration.ent).
  Parameter cached_requirements : metadata -> nat -> option (list nat).
  Parameter elaborate_with_metadata :
    Input.bundle -> Sx.res (xprogram * metadata).
  Parameter function_frame_specifications :
    metadata -> xprogram -> option frame_specs.
  Parameter lower_runtime : metadata -> xprogram -> option xprogram.

  Axiom elaborate_metadata_projects : forall b,
    C.elaborate b =
      match elaborate_with_metadata b with
      | Sx.Good (p, _) => Sx.Good p
      | Sx.Bad msg => Sx.Bad msg
      end.
  Axiom loader_reads : forall b w requirements fields,
    load_world b = Sx.Good w ->
    SourceReading.LoaderReading b (read_world w requirements fields).
  Axiom world_of_reading : forall b W,
    SourceReading.LoaderReading b W -> SourceReading.ReqsReading W ->
    (forall a c, Elaboration.w_fld W a = Elaboration.w_fld W c -> a = c) ->
    Elaboration.WorldOf b W.
  Axiom world_resolves : forall b w requirements fields i path entity,
    load_world b = Sx.Good w -> In i (module_ids w) ->
    resolve_path w i path = Sx.Good entity ->
    Elaboration.resolve (read_world w requirements fields) (module_path w i) path = entity.
  Axiom elaborate_world_of : forall b p,
    C.elaborate b = Sx.Good p ->
    exists w source requirements,
      load_world b = Sx.Good w /\
      elaborate_with_metadata b = Sx.Good (p, source) /\
      (forall x l, cached_requirements source x = Some l -> requirements x = l) /\
      forall fields, (forall a c, fields a = fields c -> a = c) ->
      Elaboration.WorldOf b (read_world w requirements fields).
  Axiom context_type_meanings : forall W s Ctx needs,
    Elaboration.CtxOf W s Ctx needs -> forall t x,
    Elaboration.nlook t (Elaboration.c_ty Ctx) = Some x ->
    x = Elaboration.canon Ctx (TAbs t).
  Axiom function_frames_total : forall b p source,
    elaborate_with_metadata b = Sx.Good (p, source) ->
    exists specs, function_frame_specifications source p = Some specs.
End SOURCE_PROGRESS.

(* The reference run, with the entry point and initial provisions supplied
   by elaboration. Both the safety and agreement contracts refer to it. *)
Module ReferenceRun (C : PROVED_NOW).
  Definition run p argv read fuel st :=
    C.X.run_main fuel (program_of p argv read) (xo_main p) (xo_env p) st.
End ReferenceRun.

(* Typing judgments supplied by the runtime proof. The contracts below
   spell out how they constrain an initial configuration and its outcome. *)
Module Type RUNTIME_TYPING (C : PROVED_NOW).
  Parameter representation context locals store : Type.
  Definition representations := nsym -> option representation.
  Definition functors := usym -> option (tele * tele * subst).
  Parameter context_items : context -> tele.
  Parameter context_subst : context -> subst.
  Parameter empty_store : store.
  Parameter well_formed : gsig -> representations -> xprog -> functors -> Prop.
  Parameter typed : gsig -> representations -> xprog -> functors ->
    option ty -> bool -> context -> locals -> xexpr -> ty -> Prop.
  Parameter grounded : subst -> context -> Prop.
  Parameter provides : gsig -> representations -> xprog -> functors ->
    store -> subst -> tele -> xdenv -> Prop.
  Parameter locals_ok : gsig -> representations -> xprog ->
    store -> subst -> tele -> locals -> xvenv -> Prop.
  Parameter inputs_form : gsig -> tele -> context -> Prop.
  Parameter store_ok : gsig -> representations -> xprog -> store -> C.Prims.mstate -> Prop.
  Parameter store_extends : store -> store -> Prop.
  Parameter value_typed : gsig -> representations -> xprog -> store -> xvalue -> ty -> Prop.
  Parameter HeapNonempty : C.Prims.mstate -> Prop.
End RUNTIME_TYPING.

Module RuntimeSpec (C : PROVED_NOW) (Source : SOURCE_PROGRESS C)
    (Import T : RUNTIME_TYPING C).
  Module Ref := ReferenceRun C.

  Record expression_typing := ExpressionTyping {
    signature : gsig;
    reps : representations;
    funs : functors;
    return_type : option ty;
    in_loop : bool;
    provisions : context;
    local_types : locals;
    result_type : ty;
    grounding : subst;
    store_typing : store
  }.

  Definition ReadyExpression t P d ge st e : Prop :=
    let Sg := signature t in let Rp := reps t in let Fb := funs t in
    let Phi := provisions t in let H := store_typing t in let g := grounding t in
    well_formed Sg Rp P Fb /\
    typed Sg Rp P Fb (return_type t) (in_loop t) Phi (local_types t) e (result_type t) /\
    grounded g Phi /\
    provides Sg Rp P Fb H g (context_items Phi) d /\
    locals_ok Sg Rp P H g (context_items Phi) (local_types t) ge /\
    store_ok Sg Rp P H st.

  Definition SafeResult t P (r : C.X.xres) : Prop :=
    let Sg := signature t in let Rp := reps t in
    let Phi := provisions t in let g := grounding t in
    exists H', store_extends (store_typing t) H' /\ store_ok Sg Rp P H' (C.X.xr_st r) /\
    let locals_preserved := locals_ok Sg Rp P H' g (context_items Phi) (local_types t) (C.X.xr_g r) in
    match C.X.xr_out r with
    | XOk w =>
        value_typed Sg Rp P H' w (app_subst g (app_subst (context_subst Phi) (result_type t))) /\
        locals_preserved
    | XRet w => exists rt, return_type t = Some rt /\ value_typed Sg Rp P H' w (app_subst g rt)
    | XBrk => in_loop t = true /\ locals_preserved
    | XErr _ => False
    | _ => True
    end.

  Definition ReadyMain P main d : Prop :=
    exists Sg Rp Fb Dm Sm body Phi ground,
      well_formed Sg Rp P Fb /\
      g_fn Sg main = Some (Dm, Sm, None) /\ fs_params Sm = [] /\
      xp_fn P main = Some body /\ inputs_form Sg Dm Phi /\
      grounded ground Phi /\
      provides Sg Rp P Fb empty_store ground (context_items Phi) d.

  Definition TypedRuntime p : Prop :=
    forall argv read,
    let P := program_of p argv read in
    let main := xo_main p in
    exists Sg Rp Fb inputs body Phi ground,
      well_formed Sg Rp P Fb /\
      g_fn Sg main = Some (inputs, mkSig [] TUnit, None) /\
      xp_fn P main = Some body /\ inputs_form Sg inputs Phi /\
      grounded ground Phi /\
      provides Sg Rp P Fb empty_store ground (context_items Phi) (xo_env p).

  Definition ReificationCertificate source raw result : Prop :=
    Source.lower_runtime source raw = Some result /\ TypedRuntime result.
End RuntimeSpec.

Module Type RUNTIME_SAFETY (C : PROVED_NOW) (Source : SOURCE_PROGRESS C)
    (T : RUNTIME_TYPING C).
  Module Spec := RuntimeSpec C Source T.
  Import Spec T.

  Axiom expression_safe : forall typing P d g st e,
    ReadyExpression typing P d g st e ->
    forall fuel, SafeResult typing P (C.X.xinterp fuel P d g st e).
  Axiom main_safe : forall P main d,
    ReadyMain P main d ->
    forall fuel st c, C.X.xr_out (C.X.run_main fuel P main d st) <> XErr c.
  Axiom main_typing_inhabited : exists P main d, ReadyMain P main d.
  Axiom heap_example : exists P main d st,
    ReadyMain P main d /\ C.X.xr_out (C.X.run_main 30 P main d st) = XOk XVUnit /\
    HeapNonempty (C.X.xr_st (C.X.run_main 30 P main d st)).
  Axiom typed_runtime_never_err : forall p,
    TypedRuntime p ->
    forall fuel argv read st c, C.X.xr_out (Ref.run p argv read fuel st) <> XErr c.
  Axiom certified_reification_never_err : forall source raw result,
    ReificationCertificate source raw result ->
    forall fuel argv read st c, C.X.xr_out (Ref.run result argv read fuel st) <> XErr c.
End RUNTIME_SAFETY.

(* Pending bridge from accepted source to the runtime safety contract.
   This is an obligation, not a checked implementation. *)
Module Type SOURCE_RUNTIME_COMPLETE (C : PROVED_NOW)
    (Source : SOURCE_PROGRESS C) (T : RUNTIME_TYPING C).
  Module Spec := RuntimeSpec C Source T.

  Parameter source_runtime_certificate : forall b raw source,
    Source.elaborate_with_metadata b = Sx.Good (raw, source) ->
    exists runtime, Spec.ReificationCertificate source raw runtime.
End SOURCE_RUNTIME_COMPLETE.

(* The default interpreter compiles MossX bodies directly to closures.
   The table equations describe the OCaml driver's trusted preparation. *)
Module Type ORACLE_PIPELINE (C : PROVED_NOW).
  Parameter code bind_code : Type.
  Definition function_table := fsym -> option code.
  Definition functor_table := usym -> option (tele * bind_code).
  Parameter compile_expr : xprog -> function_table -> functor_table -> xexpr -> code.
  Parameter compile_binds : xprog -> function_table -> functor_table -> list xbind -> bind_code.
  Parameter engine_run : function_table -> nat -> fsym -> xdenv -> C.Prims.mstate -> C.X.xres.
  Parameter fast_program prune_program : xprogram -> xprogram.
End ORACLE_PIPELINE.

Module OracleSpec (C : PROVED_NOW) (Import Engine : ORACLE_PIPELINE C).
  Module Ref := ReferenceRun C.
  Definition CompiledTables P fns funs : Prop :=
    (forall f, fns f = option_map (compile_expr P fns funs) (xp_fn P f)) /\
    (forall h, funs h = option_map (fun '(Dr,bs) => (Dr,compile_binds P fns funs bs)) (xp_fun P h)).
  Definition Prepared p argv read fns funs : Prop :=
    CompiledTables (program_of (prune_program p) argv read) fns funs.
  Definition out_of_fuel (r : C.X.xres) := C.X.xr_out r = XFuel.
  Definition stuck (r : C.X.xres) := exists c, C.X.xr_out r = XErr c.
End OracleSpec.

Module Type ORACLE_AGREEMENT (C : PROVED_NOW) (Engine : ORACLE_PIPELINE C).
  Module Spec := OracleSpec C Engine.
  Import Spec Engine.
  (* With the driver's compiled tables, the default engine agrees with
     the reference run whenever that run avoids fuel exhaustion and XErr. *)
  Axiom interpreter_correct : forall p argv read fuel st fns funs,
    Prepared (fast_program p) argv read fns funs ->
    out_of_fuel (Ref.run p argv read fuel st) \/
    stuck (Ref.run p argv read fuel st) \/
    engine_run fns fuel (xo_main (fast_program p)) (xo_env (fast_program p)) st =
      Ref.run p argv read fuel st.
End ORACLE_AGREEMENT.

(* Pending end-to-end source obligations. The checked component contracts
   above were bound in the old oracle's Certified.v. *)
Module Type COMPLETE.
  Include PROVED_NOW.

  (* The elaborator is sound and complete for the (DRAFT) relation. *)
  Parameter elaborate_sound : forall b p,
    elaborate b = Sx.Good p -> Elaboration.Elaborates b p.
  Parameter elaborate_complete : forall b,
    (exists p, Elaboration.Elaborates b p) -> exists p, elaborate b = Sx.Good p.

  (* Accepted programs never go wrong: whatever the command line, the file
     system, the initial machine and the fuel, running an accepted
     program's main never gets stuck (XErr).  It may trap, exit, run out of
     fuel, or hit an unmodelled primitive (XUns). *)
  Parameter accepted_never_err : forall b p,
    elaborate b = Sx.Good p ->
    forall fuel argv rd st0 c,
      X.xr_out (X.run_main fuel (program_of p argv rd) (xo_main p) (xo_env p) st0) <> XErr c.

  (* The bridge the proof of accepted_never_err is expected to go through
     on the core fragment: an elaborated program whose bodies are all core
     terms is Typed. *)
  Parameter elaborated_typed : forall b p,
    elaborate b = Sx.Good p ->
    (forall f x, In (f, x) (xo_fns p) -> exists e, x = emb_expr e) ->
    exists Sg, Typed Sg (xo_main p) /\
      forall f x, Elaboration.nlook f (xo_fns p) = Some x ->
        exists D S e, g_fn Sg f = Some (D, S, Some e) /\ emb_expr e = x.
End COMPLETE.

Module HeapContracts.
Import ExecutionModel.Heap.
Module Type HEAP_LAWS (Import Prims : PRIM_SEMANTICS).
  Parameter heap_ok : hty -> mstate -> Prop.

  (* the invariant holds of fewer addresses, and of weaker predicates *)
  Axiom heap_mono : forall Psi Psi' st,
    heap_ok Psi st ->
    (forall a k Q', Psi' a = Some (k, Q') ->
       exists Q, Psi a = Some (k, Q) /\ forall v, Q v -> Q' v) ->
    heap_ok Psi' st.

  (* of no address at all, it holds of any machine *)
  Axiom heap_empty : forall st, heap_ok (fun _ => None) st.

  (* every other primitive keeps it *)
  Axiom heap_other : forall P p ws st Psi,
    is_gc p = false -> heap_ok Psi st -> heap_ok Psi (snd (run_prim P p ws st)).

  (* the GC primitives: a value as gc_post says, or a trap *)
  Axiom heap_gc : forall P p ws st Psi Q,
    heap_ok Psi st -> gc_pre Psi p ws Q ->
    match run_prim P p ws st with
    | (XOk w, st') => exists Psi', heap_ok Psi' st' /\ gc_post Psi Psi' p Q w
    | (XTrap _, st') => heap_ok Psi st'
    | _ => False
    end.
End HEAP_LAWS.
End HeapContracts.
