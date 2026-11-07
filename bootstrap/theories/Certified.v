(* ------------------------------------------------------------------------ *)
(* Certified.v binds the checked component contracts in
   ComponentContracts.v. The five end-to-end obligations in
   Audit.Public.CORRECT remain pending. *)

From Stdlib Require Import List.
From NM Require Import ComponentContracts.
From NM Require Import Audit CoreMoss MossX Sx Elab Specialization RelFacts.
From NM Require FastPath WorldSound ElabFrame ReqsTotal MethodLookup XBridgeCtx.
From NM Require XTyping XSafety XExample XHeapC.
From NM Require Parse FrontEnd FrontEndComplete FrontEndLoader DumpRoundTrip.
From NM Require EarlyCertified DefaultRuntime.
Import ListNotations.

Module Certified := EarlyCertified.Certified.

(* The fixed public platform uses the same primitives and heap invariant as
   the executable. The generic build/run contract remains unimplemented. *)
Module PublicRuntime := DefaultRuntime.Model.
Definition default_interface := DefaultRuntime.default_interface.
Definition default_interface_laws := DefaultRuntime.default_interface_laws.
Definition default_compatible := DefaultRuntime.default_compatible.

(* Toward ComponentContracts.COMPLETE.elaborate_sound, step 1: the static
   world the elaborator's loader reads a bundle as satisfies every condition
   of Part V's WorldOf about modules, imports, declarations, D44 exports and
   scopes and D61 attached methods (WorldSound.LoaderReading), whatever the
   requirements and field numbering; with those two (ReqsReading, an
   injective numbering) it is a WorldOf (world_of_reading).  The remaining
   conditions are WorldSound.v's section world_todo. *)
Definition world_reads := WorldSound.load_world_reads.
Definition elaborate_world_reads := WorldSound.elaborate_world_reads.
Definition world_of_reading := WorldSound.worldof_of_reading.
(* and every path the elaborator resolves in a module of that world, Part
   V's `resolve` resolves to the same entity *)
Definition world_resolves := WorldSound.load_world_resolves.
Definition context_method_named := MethodLookup.find_method_decl_named.
Definition context_type_meanings := XBridgeCtx.ctx_tys_self.
Definition dump_reads := DumpRoundTrip.dump_reads.
(* and what elaboration keeps of that world: its symbols, unchanged; only
   fresh, non-source symbols made; record fields numbered injectively; and
   the requirement lists it caches for tags and aliases are closed and least
   (toward ReqsReading's rr_closed, rr_least and rr_listed) *)
Definition elaboration_frame := ElabFrame.elaborate_frame.
(* the requirements the elaborator computes for a val, fn, method, context
   or functor are Part V's AssumeTypes (toward ReqsReading's rr_plain) *)
Definition assume_types_exact := WorldSound.load_world_assume_types.
(* and, steps 1-3 together: whenever the elaborator accepts a bundle, the
   loader's world with the requirement lists the elaborator cached (the
   least relevance-closed ones, and AssumeTypes, for the symbols it never
   asked about) and any injective field numbering is a WorldOf of it *)
Definition elaborate_world_of := ReqsTotal.elaborate_world_of.
(* FRONT_END: bind the successful lexer and parser results to the audit. *)
Module FrontEndCertified <: FRONT_END.
  Definition lex bytes :=
    match Parse.lex bytes with
    | Parse.LOk ts => Some ts
    | Parse.LErr _ _ => None
    end.
  Lemma lexer_sound : forall bytes ts, lex bytes = Some ts -> Syntax.Lexes 0 bytes ts.
  Proof.
    intros bytes ts H. unfold lex in H.
    destruct (Parse.lex bytes) eqn:E; [|discriminate].
    injection H as <-. exact (FrontEnd.lex_sound bytes _ E).
  Qed.
  Lemma lexer_complete : forall bytes ts, Syntax.Lexes 0 bytes ts -> lex bytes = Some ts.
  Proof. intros bytes ts H. unfold lex. now rewrite (FrontEnd.lex_complete bytes ts H). Qed.

  Definition parse := Parse.parse_moss.
  Definition parser_sound : forall s f, parse s = Sx.Good f -> Syntax.Derives (String.list_byte_of_string s) f :=
    FrontEnd.parse_moss_sound.
  Definition parser_complete : forall s f, Syntax.Derives (String.list_byte_of_string s) f -> parse s = Sx.Good f :=
    FrontEndComplete.parse_moss_complete.
End FrontEndCertified.

(* LOADER: a bundle the loader loads (Parse.load: load_bundle's modules,
   their dumps read back) is what Audit.Loader.Bundled says
   (FrontEndLoader.v). *)
Module LoaderCertified <: LOADER.
  Definition load := Parse.load.
  Definition load_sound := FrontEndLoader.load_sound.
End LoaderCertified.

(* The dump adapter used by extraction and its agreement with the typed
   elaborator. *)
Definition elaborate_sx := Elab.elaborate_sx.
Lemma elaborate_sx_reads : forall x,
  elaborate_sx x = match Sx.read_bundle x with
                   | Sx.Good b => Certified.elaborate b
                   | Sx.Bad m => Sx.Bad m
                   end.
Proof. intro x. reflexivity. Qed.

(* The other executable root. *)
Definition run_main := MossX.XS.run_main.

From NM Require FunctionFrameProgram ReifyProgramWalk ReifyProgram Reify XElabTyped
  XProvision XEnv CachedClo CachedCloFacts SlotX FoldX Prune.

Module SourceCertified <: SOURCE_PROGRESS Certified.
  Definition metadata := Elab.st.
  Definition loaded_world := Elab.world.
  Definition load_world := Elab.load_world.
  Definition read_world := WorldSound.W_read.
  Definition module_ids := Elab.w_order.
  Definition module_path w i := Elab.gm_path (Elab.get_mod w i).
  Definition resolve_path w i path :=
    match Elab.resolve_path w i path with
    | Sx.Good entity => Sx.Good (option_map (WorldSound.ent_of w) entity)
    | Sx.Bad msg => Sx.Bad msg
    end.
  Definition cached_requirements source x := NMap.nget (Elab.st_reqs source) x.
  Definition frame_specs :=
    list (fsym * ReifyData.reify_scope Elab.ety Elab.key * ReifyData.reify_signature Elab.ety).
  Definition elaborate_with_metadata := Elab.elaborate_with_metadata.
  Definition function_frame_specifications := ReifyProgramWalk.function_frame_specifications.
  Definition lower_runtime source raw :=
    option_map Reify.rr_program (ReifyProgram.reify_program source raw).

  Definition elaborate_metadata_projects := Elab.elaborate_with_metadata_project.

  Definition loader_reads := WorldSound.load_world_reads.
  Definition world_of_reading := WorldSound.worldof_of_reading.
  Definition context_type_meanings := XBridgeCtx.ctx_tys_self.

  Lemma world_resolves : forall b w requirements fields i path entity,
    load_world b = Sx.Good w -> In i (module_ids w) ->
    resolve_path w i path = Sx.Good entity ->
    Elaboration.resolve (read_world w requirements fields) (module_path w i) path = entity.
  Proof.
    intros b w R F i path entity Load Member Resolve.
    unfold resolve_path in Resolve.
    destruct (Elab.resolve_path w i path) as [found|msg] eqn:E; [|discriminate].
    injection Resolve as <-.
    exact (WorldSound.load_world_resolves b w R F Load i path found Member E).
  Qed.

  Lemma elaborate_world_of : forall b p,
    Certified.elaborate b = Sx.Good p ->
    exists w source requirements,
      load_world b = Sx.Good w /\
      elaborate_with_metadata b = Sx.Good (p, source) /\
      (forall x l, cached_requirements source x = Some l -> requirements x = l) /\
      forall fields, (forall a c, fields a = fields c -> a = c) ->
      Elaboration.WorldOf b (read_world w requirements fields).
  Proof.
    intros b p H.
    destruct (ReqsTotal.elaborate_world_of _ _ H) as (w & s & R & Load & Run & Reqs & World).
    exists w, s, R. split; [exact Load|]. split; [|exact (conj Reqs World)].
    unfold elaborate_with_metadata, Elab.elaborate_with_metadata, Sx.rbind.
    now rewrite Load.
  Qed.

  Lemma function_frames_total : forall b p source,
    elaborate_with_metadata b = Sx.Good (p, source) ->
    exists specs, function_frame_specifications source p = Some specs.
  Proof.
    intros b p source H. unfold elaborate_with_metadata, Elab.elaborate_with_metadata, Sx.rbind in H.
    destruct (Elab.load_world b) as [w|msg] eqn:E; [|discriminate].
    exact (@FunctionFrameProgram.actual_function_frame_specifications_total b w p source E H).
  Qed.
End SourceCertified.

Module RuntimeTyping <: RUNTIME_TYPING Certified.
  Definition representation := XTyping.rep.
  Definition context := ctx.
  Definition locals := tenv.
  Definition store := XTyping.sty.
  Definition representations := nsym -> option representation.
  Definition functors := usym -> option (tele * tele * subst).
  Definition context_items := cA.
  Definition context_subst := cS.
  Definition empty_store : store := fun _ => None.
  Definition well_formed := XTyping.xwf.
  Definition typed := XTyping.xhas.
  Definition grounded := ctx_grounded.
  Definition provides := XProvision.provision_environment.
  Definition locals_ok := XEnv.venv_ok.
  Definition inputs_form Sg := wf_tele Sg empty_ctx.
  Definition store_ok Sg Rp P H := XHeapC.ConcreteHeap.heap_ok (XSafety.XSafetyC.PsiH Sg Rp P H).
  Definition store_extends := XTyping.sext.
  Definition value_typed := XTyping.xvt.
  Definition HeapNonempty st := MossX.ms_heap st <> HTrie.TE.
End RuntimeTyping.

Module RuntimeCertified <: RUNTIME_SAFETY Certified SourceCertified RuntimeTyping.
  Module Spec := RuntimeSpec Certified SourceCertified RuntimeTyping.
  Import Spec RuntimeTyping.

  Lemma expression_safe : forall t P d g st e,
    ReadyExpression t P d g st e ->
    forall fuel, SafeResult t P (Certified.X.xinterp fuel P d g st e).
  Proof.
    intros t P d g st e (W & T & G & D & V & H) fuel.
    eapply XSafety.mossx_typed_safe; eassumption.
  Qed.

  Lemma main_safe : forall P main d,
    ReadyMain P main d ->
    forall fuel st c, Certified.X.xr_out (Certified.X.run_main fuel P main d st) <> XErr c.
  Proof.
    intros P main d (Sg & Rp & Fb & Dm & Sm & body & Phi & ground & W & M & A & B & T & G & E).
    intros fuel st c. eapply XSafety.mossx_run_main_safe; eassumption.
  Qed.

  Lemma ref_main_ready : ReadyMain XExample.XExampleRef.P XExample.XExampleRef.fMain nil.
  Proof.
    Import XExample.XExampleRef.
    exists Sg, Rp, Fb, nil, (mkSig nil TUnit), main_body, empty_ctx, nil.
    split; [exact ref_wf|].
    split; [reflexivity|]. split; [reflexivity|]. split; [reflexivity|].
    split; [constructor|]. split.
    - split; [constructor|]. split; [intros t tau Ht; discriminate Ht|].
      split; [intros t []|]. split; [split; [intros x [] | constructor] | intros []].
    - split; [split; [constructor | split; [intros i en [] | constructor]]|intros Reify t []].
  Qed.

  Lemma main_typing_inhabited : exists P main d, ReadyMain P main d.
  Proof. do 3 eexists. exact ref_main_ready. Qed.

  Lemma heap_example : exists P main d st,
    ReadyMain P main d /\ Certified.X.xr_out (Certified.X.run_main 30 P main d st) = XOk XVUnit /\
    HeapNonempty (Certified.X.xr_st (Certified.X.run_main 30 P main d st)).
  Proof.
    exists XExample.XExampleRef.P, XExample.XExampleRef.fMain, nil, XExample.XExampleRef.st0.
    split; [exact ref_main_ready|].
    destruct XExample.XExampleRef.ref_runs as [Out Heap]. split; [exact Out|].
    unfold HeapNonempty. rewrite Heap. discriminate.
  Qed.

  Definition typed_runtime_never_err := XElabTyped.typed_runtime_never_err.

  Lemma certified_reification_never_err : forall source raw result,
    ReificationCertificate source raw result ->
    forall fuel argv read st c, Certified.X.xr_out (Ref.run result argv read fuel st) <> XErr c.
  Proof. intros source raw result [_ T]. exact (typed_runtime_never_err result T). Qed.
End RuntimeCertified.

From NM Require LazyOracle.

Module OraclePipeline := LazyOracle.Pipeline.

Module OracleCertified := LazyOracle.Certified.
