(* The body-safety induction for source bodies (T02 slice). Evaluating
   the code of any Ex/Blk/BlkE/Branch/Cond derivation under a realizing
   state produces an Outcome: never Stuck, a Breaking only where the source
   breaks inside a loop, typed values and stores otherwise.

   The proof is by induction on fuel, inverting derivations. Constructors
   outside the slice (contextual vals, calls, methods, operators, literals
   and bind statements) and two static facts (tag construction and match
   arm selection) are explicit Section hypotheses; each is a named
   remaining obligation, not an assumption about accepted programs. *)
From NM Require AuditFacts.
From NM Require Import LegacyBodyJudgments.
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import AuditBase.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBoolTyping PublicBodySafety PublicCallable PublicTypeFacts PublicArms PublicTagCheck PublicContextFacts
  PublicCallRealize PublicRuntimeKeys.
From NM Require AllocationFacts PublicFormation PublicScoping PublicWorldFormation PublicOrientation PublicMergePresent PublicMerge.
From NM Require PublicCopySubst PublicLinkAnchors.
Import Audit.Input AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.
Local Open Scope string_scope.

Module BodySlice (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module AR := Arms Prims I0.
Module B := BoolTruthy Prims I0.
Module TC := Checks Prims I0.
Module PC := PublicCallable.Callable Prims I0.

Section Slice.
Variables (W : A.world) (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Let P := program_of Prog argv read.
Variable state_ok : heap_types -> Prims.mstate -> Prop.

Hypothesis Layouts : Platform.Layouts W.
(* HYPOTHESES (T29): context formation's own premises (PublicFormation). *)
Hypothesis mentions_typed : PublicFormation.MentionsTyped W.
Hypothesis family_sig_ok : PublicFormation.FamilySigReadOffOk W.
Hypothesis Bools : BoolSymbols W Prog.

Definition Absorbs (C : A.cx) (d : environment) : Prop :=
  ReadsMerge C (ground d).

(* The canonical readings still used by the transitional rules. *)
Definition AbsorbsCanon (C : A.cx) (d : environment) : Prop :=
  forall t, ground d (A.canon C t) = ground d t.

Lemma absorbs_canon_iff : forall C d, CxSection C -> Absorbs C d <-> AbsorbsCanon C d.
Proof.
  intros C d Section. split.
  - intros Abs t. symmetry. apply Abs. unfold A.canon.
    apply PublicMerge.merge_eq_read. exact (proj1 Section).
  - intros Abs t u Equal. apply (context_canon_iff_mergeeq C Section) in Equal.
    rewrite <- (Abs t), <- (Abs u), Equal. reflexivity.
Qed.

(* Compatibility names keep the existing proof chain at its original
   module paths; the contracts themselves live in the small generic module. *)
Definition CallOutcome := PC.CallOutcome W P state_ok.
Definition PrimOutcome := PC.PrimOutcome W P state_ok.
Definition CallableOk := PC.CallableOk W P state_ok.

(* ---- functor closures (T15) ---------------------------------------------- *)

(* An environment that only grounds types. *)
Definition denv (gm : subst) : environment := EnvironmentWithAliases gm gm [].

Lemma existsb_item_in : forall k l, existsb (item_eqb k) l = true <-> In k l.
Proof.
  intros k l. split.
  - intros E. apply existsb_exists in E as [k' [I E]]. apply item_eqb_spec in E. subst. exact I.
  - intros I. apply existsb_exists. exists k. split; [exact I|apply item_eqb_spec; reflexivity].
Qed.

Lemma captured_spec : forall target needs k,
  In k (A.captured_needs target needs) <-> In k needs /\ target k = None.
Proof.
  intros target needs k. unfold A.captured_needs. rewrite filter_In.
  unfold A.is_linked. destruct (target k); cbn; split; intuition congruence.
Qed.

Lemma linked_spec : forall target needs k,
  In k (A.linked_needs target needs) <-> In k needs /\ target k <> None.
Proof.
  intros target needs k. unfold A.linked_needs. rewrite filter_In.
  unfold A.is_linked. destruct (target k); cbn; split; intuition congruence.
Qed.

Lemma linked_targets_in : forall u Cu nsu Cg nsg target dc gfx,
  ClosureBinding W u Cu nsu Cg nsg target dc gfx -> forall k,
  In k (A.linked_needs target nsg) -> In (A.linked_target target k) nsu.
Proof.
  intros u Cu nsu Cg nsg target dc gfx Binding k Member.
  apply linked_spec in Member as [Source Linked]. unfold A.linked_target.
  destruct (target k) as [key|] eqn:Target; [|contradiction].
  exact (closure_bind_targets _ _ _ _ _ _ _ _ _ Binding k key Source Target).
Qed.

Lemma sig_gens_ret : forall g K, incl (A.sig_gens g) K -> incl (A.gens (A.sg_ret g)) K.
Proof. intros g K S y Iy. apply S. unfold A.sig_gens. cbn [flat_map]. apply in_or_app. left. exact Iy. Qed.
Lemma sig_gens_param : forall g K t, incl (A.sig_gens g) K -> In t (A.sg_params g) -> incl (A.gens t) K.
Proof.
  intros g K t S It y Iy. apply S. unfold A.sig_gens. cbn [flat_map]. apply in_or_app. right.
  apply in_flat_map. exists t. split; assumption.
Qed.
(* The target keys selected for a copy belong to F's needs. *)
Definition NeedsAmong (target : item -> item) (nsh nsu : list item) : Prop :=
  forall k, In k nsh -> In (target k) nsu.
Definition captured_funs (Cg : A.cx) (caps : list item) : list (nat * subst) :=
  flat_map (fun k => match k with
    | IFun u => match A.nlook u (A.c_funs Cg) with Some fx => [(u, fx)] | None => [] end
    | _ => [] end) caps.
Definition ground_fixed (gm : subst) (fx : subst) : subst :=
  map (fun q => (fst q, app_subst gm (snd q))) fx.

(* The meanings an application reads a linked body's types at: u's fixed
   inputs as gfx says, its captured inputs at the closure's meanings. (Its
   other linked inputs are never mentioned by what it captures, D84 3b.) *)
Definition AppMeanings (u : nat) (Cu Cg : A.cx) (dc : environment) (gfx gm : subst) : Prop :=
  (forall x, A.Fixed W u x -> In x (map fst (A.c_ty Cg)) -> lookup_subst x gm = lookup_subst x gfx) /\
  (forall x, In x (map fst (A.c_ty Cg)) -> ~ In x (map fst (A.c_ty Cu)) ->
    output_meaning dc x <> None -> lookup_subst x gm = Some (cmeaning dc x)).

(* An abstract functor's context lists every symbol it mentions. *)
Lemma fixed_in_functx : forall u Cu ns x, (exists n ps a r, A.ast_of W u = Some (ADFunctor n ps a r None)) ->
  A.FunCtx W u Cu ns -> A.Fixed W u x -> In x (map fst (A.c_ty Cu)).
Proof.
  intros u Cu ns x [n [ps [a [r Ast]]]] (d & ps' & a' & r' & b & es & tm & tm' & σ & classes & implicit & D & FP & _ & Imp & -> & _ & _ & _ & -> & _ & _)
    [Mn _].
  unfold A.ast_of in Ast. rewrite D in Ast. injection Ast as Ast. rewrite Ast in FP. injection FP as _ _ _ <-.
  cbn. rewrite map_map. cbn. rewrite map_app, map_map. cbn. apply in_or_app.
  destruct (A.nlook x tm) as [v|] eqn:L.
  - right. apply nlook_in in L. change x with (fst (x, v)). apply in_map, L.
  - left. rewrite map_id. apply Imp. split; [reflexivity|split; [exact Mn|exact L]].
Qed.

(* A platform family's fixed machine-type inputs mean the machine types
   (the platform's provision exists only there, lib/wasm.moss). *)
Definition PlatformFixed (u : nat) (gfx : subst) : Prop :=
  forall x, In x (map fst (program_machines Prog)) ->
    A.Fixed W u x -> lookup_subst x gfx = Some (TRigid x).

(* PlatformFixed syntactically: the context the family was provided in
   (main's) records each fixed machine-type input as itself, and gfx is
   that record read by some meanings. *)
Definition PlatformRead (u : nat) (gfx : subst) : Prop :=
  exists fx gm, gfx = ground_fixed gm fx /\
    (forall x, A.Fixed W u x -> exists tx, In (x, tx) fx) /\
    (forall x t t', In (x, t) fx -> In (x, t') fx -> t = t') /\
    (forall x tx, In x (map fst (program_machines Prog)) ->
       In (x, tx) fx -> tx = TAbs x).

(* A copy `bind u = h;` at its site C1, syntactically (BFunCopy): fx is
   u's fixed readings in C1 (what set_fun records), CopyInterpretation compares
   at them read canonically, gfx is them read by d (which realizes C1);
   h's recorded readings fxh in C1 agree with C1's (FixedAgreement) and
   mean in d what h's closure takes them to (gfh). *)
Definition CopySite (u h : nat) (Cu : A.cx) (nsu nsh : list item) (Ch : A.cx) (rm : list nat) (target : item -> item) (gfx gfh : subst) : Prop :=
  exists C1 fx fxh d,
    PublicFormation.FormedCx W C1 /\ Absorbs C1 d /\
    CxSection C1 /\
    (forall x tx, In (x, tx) fx <-> A.Fixed W u x /\ A.nlook x (A.c_ty C1) = Some tx) /\
    A.CopyInterpretation W (map (fun q => (fst q, A.canon C1 (snd q))) fx) u h Cu nsu nsh Ch rm target /\
    gfx = ground_fixed (type_meanings d) fx /\
    A.nlook h (A.c_funs C1) = Some fxh /\ A.FixedAgreement W C1 h /\
    (forall x, A.Fixed W h x -> exists tx, In (x, tx) fxh) /\
    (forall x tx, In (x, tx) fxh -> lookup_subst x gfh = Some (ground d tx)) /\
    (* u is abstract (only an abstract functor is bound) *)
    (exists n ps a r, A.ast_of W u = Some (ADFunctor n ps a r None)).

(* Q6: a linked body's pins of fixed inputs (each a type over G's inputs:
   fixed inputs of u, read as gfx says, and captured ones, LinkBody) mean,
   at any application's meanings, what the closure's fixed readings give
   the pinned input. *)
Definition PinsAgree (u : nat) (Cu Cg : A.cx) (dc : environment) (gfx pins : subst) : Prop :=
  forall gm, AppMeanings u Cu Cg dc gfx gm ->
  forall x tx, In (x, tx) pins -> lookup_subst x gfx = Some (app_subst gm tx).

(* A functor closure is applicable (structurally: its captured functors
   are subterms). ClosureOk n H0 u gfx pv: pv, provided for u with its
   fixed inputs meaning what gfx says, is a body linked for u whose
   captures are realized at n and H0 under any application's meanings, a
   copy of an applicable closure, or a platform family. *)
Inductive ClosureOk (n : nat) (H0 : heap_types) : nat -> subst -> provision_value -> Prop :=
| CLink : forall u gg h Cu nsu Cg nsg linktarget dc gfx pins,
    LegacyBodyJudgments.LinkBody W Prog u gg h pins -> ClosureBinding W u Cu nsu Cg nsg linktarget dc gfx ->
    A.FunCtx W u Cu nsu -> A.FunCtx W gg Cg nsg ->
    (forall x, A.Fixed W u x -> In x (map fst (A.c_ty Cu))) ->
    (* u is abstract (BFunClosure binds only an abstract functor) *)
    (exists n ps a r, A.ast_of W u = Some (ADFunctor n ps a r None)) ->
    (forall gm, AppMeanings u Cu Cg dc gfx gm ->
      (forall v t, In (IVal v) (A.captured_needs linktarget nsg) -> A.nlook v (A.c_vals Cg) = Some t -> exists w,
         lookup_provision (IVal v) dc = Some (ValueProvision w) /\ value_typed W P H0 w (app_subst gm t)) /\
      (forall f g, In (IFn f) (A.captured_needs linktarget nsg) -> A.nlook f (A.c_fns Cg) = Some g -> exists pv,
         lookup_provision (IFn f) dc = Some pv /\ CallableOk n H0 (denv gm) (A.sg_params g) (A.sg_ret g) pv) /\
      (forall r m a g, In (IMth r m a) (A.captured_needs linktarget nsg) -> A.mlook r m (A.c_mths Cg) = Some g -> exists pv,
         lookup_provision (A.rkey Cg r m) dc = Some pv /\ CallableOk n H0 (denv gm) (r :: A.sg_params g) (A.sg_ret g) pv)) ->
    (forall gm, AppMeanings u Cu Cg dc gfx gm ->
      CapturedFuns n H0 gm dc (captured_funs Cg (A.captured_needs linktarget nsg))) ->
    (* Q6: each pin of a fixed input means what u's fixed reading says *)
    PinsAgree u Cu Cg dc gfx pins ->
    ClosureOk n H0 u gfx (FunctorClosure h
      (Some (A.link_map Cu Cg linktarget nsg)) dc None)
| CCopy : forall u h Cu nsu Ch nsh target code sms dc rms rm gfx gfh,
    ClosureOk n H0 h gfh (FunctorClosure code sms dc rms) ->
    A.FunCtx W u Cu nsu -> A.FunCtx W h Ch nsh -> NeedsAmong target nsh nsu ->
    (forall x, A.Fixed W h x -> A.Fixed W u x /\ lookup_subst x gfh = lookup_subst x gfx) ->
    (* H's inputs, needs and result read as u's, u's fixed inputs read as
       the copy site did *)
    (exists sg m, A.CopyInterpretation W sg u h Cu nsu nsh Ch rm target /\ gfx = ground_fixed m sg) ->
    (forall x tx, A.nlook x (A.c_ty Ch) = Some tx -> A.nlook x (A.c_ty Cu) <> None) ->
    (* H's result is u's, leaf by leaf (so the same type outputs:
       same_result_outputs) *)
    A.SameResult W u h ->
    CopySite u h Cu nsu nsh Ch rm target gfx gfh ->
    ClosureOk n H0 u gfx (FunctorClosure code
      (compose_adaptation sms (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu (target k))) nsh)) dc (compose_positions rms rm))
| CPlatform : forall u code gfx, Platform.Family W Prog u code -> PlatformFixed u gfx -> PlatformRead u gfx ->
    ClosureOk n H0 u gfx (FunctorClosure code None (EnvironmentWithAliases [] [] []) None)
with CapturedFuns (n : nat) (H0 : heap_types) : subst -> environment -> list (nat * subst) -> Prop :=
| CFNil : forall gm dc, CapturedFuns n H0 gm dc []
| CFCons : forall gm dc u fx pv gfx rest,
    lookup_provision (IFun u) dc = Some pv -> ClosureOk n H0 u gfx pv ->
    (forall x tx, In (x, tx) fx -> lookup_subst x gfx = Some (app_subst gm tx)) ->
    CapturedFuns n H0 gm dc rest -> CapturedFuns n H0 gm dc ((u, fx) :: rest).

Lemma lookup_ground_fixed : forall gm fx x tx, (forall x t t', In (x, t) fx -> In (x, t') fx -> t = t') ->
  In (x, tx) fx -> lookup_subst x (ground_fixed gm fx) = Some (app_subst gm tx).
Proof.
  intros gm fx x tx Fun. induction fx as [|[y t] rest IH]; intros I; [contradiction|]. cbn.
  destruct (Nat.eqb_spec x y) as [->|Ne].
  - destruct I as [E|I]; [injection E as <-; reflexivity|].
    rewrite (Fun y t tx (or_introl eq_refl) (or_intror I)). reflexivity.
  - destruct I as [E|I]; [injection E as E; congruence|].
    apply IH; [intros a b c I1 I2; exact (Fun a b c (or_intror I1) (or_intror I2))|exact I].
Qed.

(* A functor's fixed inputs, as a context records them (fx), read in d. *)
Definition FixedMeans (u : nat) (d : environment) (fx gfx : subst) : Prop :=
  (forall x, A.Fixed W u x -> exists tx, In (x, tx) fx) /\
  (forall x tx, In (x, tx) fx -> lookup_subst x gfx = Some (ground d tx)).

Scheme closure_ok_ind' := Induction for ClosureOk Sort Prop
with captured_funs_ind' := Induction for CapturedFuns Sort Prop.
Combined Scheme closure_ok_mut from closure_ok_ind', captured_funs_ind'.

(* The static provisions, realized under their runtime keys (T31). *)
Definition EnvOkWith (rk : item -> item) (n : nat) (C : A.cx) (d : environment) (H : heap_types) : Prop :=
  (forall v t, A.nlook v (A.c_vals C) = Some t -> exists w,
     lookup_provision (rk (IVal v)) d = Some (ValueProvision w) /\ value_typed W P H w (ground d t)) /\
  (forall f g, A.nlook f (A.c_fns C) = Some g -> exists pv,
     lookup_provision (rk (IFn f)) d = Some pv /\ CallableOk n H d (A.sg_params g) (A.sg_ret g) pv) /\
  (forall r m g, A.mlook r m (A.c_mths C) = Some g -> exists pv,
     lookup_provision (rk (IMth r m [])) d = Some pv /\ CallableOk n H d (r :: A.sg_params g) (A.sg_ret g) pv) /\
  (* functor closures, applicable with their fixed inputs read here *)
  (forall u fx, A.nlook u (A.c_funs C) = Some fx -> exists pv gfx,
     lookup_provision (rk (IFun u)) d = Some pv /\ ClosureOk n H u gfx pv /\ FixedMeans u d fx gfx).

(* Expand the key cases so existing users keep their concrete lookup keys. *)
Definition EnvOk (n : nat) (C : A.cx) (d : environment) (H : heap_types) : Prop :=
  EnvOkWith (fun k => match k with
    | IMth r m _ => A.rkey C r m
    | _ => k
    end) n C d H.

Lemma recorded_fixed_meaning : forall u C fx d gfx T x t,
  A.FixedReadings W u (A.c_ty C) fx -> AbsorbsCanon C d ->
  gfx = ground_fixed (type_meanings d) fx ->
  (forall y, A.Fixed W u y -> lookup_subst y gfx = Some (app_subst T (TAbs y))) ->
  In (x,t) (map (fun q => (fst q, A.canon C (snd q))) fx) ->
  A.Fixed W u x /\ app_subst (type_meanings d) t = app_subst T (TAbs x).
Proof.
  intros u C fx d gfx T x t Fixed Abs Recorded Read Member.
  assert (Function : forall y v w, In (y,v) fx -> In (y,w) fx -> v = w).
  { intros y v w First Second.
    destruct (proj1 (Fixed y v) First) as [_ FirstRead].
    destruct (proj1 (Fixed y w) Second) as [_ SecondRead]. congruence. }
  apply in_map_iff in Member as [[y tx] [Equal InFx]]. cbn in Equal. injection Equal as -> <-.
  destruct (proj1 (Fixed x tx) InFx) as [Fx _]. split; [exact Fx|].
  pose proof (Read x Fx) as Fg. rewrite Recorded, (lookup_ground_fixed _ fx x tx Function InFx) in Fg.
  injection Fg as Fg. change (ground d (A.canon C tx) = app_subst T (TAbs x)).
  rewrite (Abs tx). exact Fg.
Qed.

Lemma envok_concrete_iff : forall n C d H,
  EnvOk n C d H <-> EnvOkWith (A.rkey_of C) n C d H.
Proof. intros; reflexivity. Qed.

(* Only keys of provisions present in this context need to agree. *)
Lemma envok_with_keys : forall rk rk' n C d H,
  (forall v t, A.nlook v (A.c_vals C) = Some t -> rk (IVal v) = rk' (IVal v)) ->
  (forall f g, A.nlook f (A.c_fns C) = Some g -> rk (IFn f) = rk' (IFn f)) ->
  (forall r m g, A.mlook r m (A.c_mths C) = Some g -> rk (IMth r m []) = rk' (IMth r m [])) ->
  (forall u fx, A.nlook u (A.c_funs C) = Some fx -> rk (IFun u) = rk' (IFun u)) ->
  EnvOkWith rk n C d H -> EnvOkWith rk' n C d H.
Proof.
  intros rk rk' n C d H V F M U [EV [EF [EM EU]]].
  split; [|split; [|split]].
  - intros v t L. rewrite <- (V v t L). exact (EV v t L).
  - intros f g L. rewrite <- (F f g L). exact (EF f g L).
  - intros r m g L. rewrite <- (M r m g L). exact (EM r m g L).
  - intros u fx L. rewrite <- (U u fx L). exact (EU u fx L).
Qed.

Lemma envok_normalized_iff : forall n C d H,
  (forall r m g, A.mlook r m (A.c_mths C) = Some g -> A.canon C r = r) ->
  EnvOk n C d H <-> EnvOkWith (normalized_rkey C) n C d H.
Proof.
  intros n C d H Stored.
  assert (Methods : forall r m g, A.mlook r m (A.c_mths C) = Some g ->
    normalized_rkey C (A.mkey r m) = A.rkey_of C (A.mkey r m)).
  { intros r m g L. apply normalized_rkey_stored.
    change (A.mkey (A.canon C r) m = A.mkey r m). now rewrite (Stored r m g L). }
  rewrite envok_concrete_iff.
  split; intro Env; eapply envok_with_keys; try exact Env;
    intros; try reflexivity.
  - symmetry. eapply Methods; eassumption.
  - eapply Methods; eassumption.
Qed.

Lemma envok_with_method_keyeq : forall C (keys : RuntimeKeys C) n d H r m g k,
  EnvOkWith (runtime_key C keys) n C d H ->
  A.mlook r m (A.c_mths C) = Some g ->
  A.KeyEq (A.c_merge C) (A.mkey r m) k ->
  exists pv, runtime_lookup C keys k d = Some pv /\
    CallableOk n H d (r :: A.sg_params g) (A.sg_ret g) pv.
Proof.
  intros C keys n d H r m g k [_ [_ [Methods _]]] Found Equal.
  destruct (Methods r m g Found) as [pv [Lookup Callable]].
  exists pv. split; [|exact Callable].
  rewrite <- (runtime_lookup_proper C keys d (A.mkey r m) k Equal). exact Lookup.
Qed.

(* Every provision is filed under a key of its own kind. *)
Definition KeysMatch (C : A.cx) : Prop := PublicCallRealize.KeysMatch (A.c_provisions C).

(* Static facts of a context in force (T29). *)
Definition StaticOk (C : A.cx) : Prop :=
  CxSection C /\ KeysMatch C.

Lemma static_ok_canonical_iff : forall C,
  StaticOk C <-> (canonical_context C /\ CxSection C) /\ KeysMatch C.
Proof.
  intro C. split.
  - intros [Section Keys]. exact (conj (conj (section_canonical C Section) Section) Keys).
  - intros [[_ Section] Keys]. exact (conj Section Keys).
Qed.

Lemma envok_runtime_iff : forall C (Section : CxSection C) n d H,
  PublicScoping.MethsRead W C -> KeysMatch C ->
  EnvOk n C d H <-> EnvOkWith (runtime_key C (canonical_runtime_keys C Section)) n C d H.
Proof.
  intros C Section n d H Methods Keys. apply envok_normalized_iff.
  intros r m g Found.
  exact (PublicScoping.methsread_mlook_canonical W C r m g Methods Keys Found).
Qed.

Lemma envok_runtime_method : forall C (Section : CxSection C) n d H r m g k,
  PublicScoping.MethsRead W C -> KeysMatch C -> EnvOk n C d H ->
  A.mlook r m (A.c_mths C) = Some g ->
  A.KeyEq (A.c_merge C) (A.mkey r m) k ->
  exists pv, lookup_provision (normalized_rkey C k) d = Some pv /\
    CallableOk n H d (r :: A.sg_params g) (A.sg_ret g) pv.
Proof.
  intros C Section n d H r m g k Methods Keys Env Found Equal.
  apply (envok_runtime_iff C Section n d H Methods Keys) in Env.
  exact (envok_with_method_keyeq C (canonical_runtime_keys C Section)
    n d H r m g k Env Found Equal).
Qed.

(* A realization exposes its runtime key function. The current rules still
   observe concrete keys, while formed contexts admit the proper key interface. *)
Definition RealizesWith (rk : item -> item) (n : nat) (C : A.cx)
    (d : environment) (H : heap_types) : Prop :=
  Absorbs C d /\ EnvOkWith rk n C d H /\ StaticOk C.

Definition Realizes (n : nat) (C : A.cx) (d : environment) (H : heap_types) : Prop :=
  RealizesWith (fun k => match k with
    | IMth r m _ => A.rkey C r m
    | _ => k
    end) n C d H.

Lemma realizes_runtime_iff : forall C (Section : CxSection C) n d H,
  PublicScoping.MethsRead W C ->
  Realizes n C d H <->
  RealizesWith (runtime_key C (canonical_runtime_keys C Section)) n C d H.
Proof.
  intros C Section n d H Methods. split.
  - intros [Abs [Env Static]]. pose proof Static as [_ Keys].
    split; [exact Abs|]. split; [|exact Static].
    apply (envok_runtime_iff C Section n d H Methods Keys). exact Env.
  - intros [Abs [Env Static]]. pose proof Static as [_ Keys].
    split; [exact Abs|]. split; [|exact Static].
    apply (envok_runtime_iff C Section n d H Methods Keys) in Env. exact Env.
Qed.

Lemma realizes_with_method_keyeq : forall C (keys : RuntimeKeys C) n d H r m g k,
  RealizesWith (runtime_key C keys) n C d H ->
  A.mlook r m (A.c_mths C) = Some g ->
  A.KeyEq (A.c_merge C) (A.mkey r m) k ->
  exists pv, runtime_lookup C keys k d = Some pv /\
    CallableOk n H d (r :: A.sg_params g) (A.sg_ret g) pv.
Proof.
  intros C keys n d H r m g k [_ [Env _]] Found Equal.
  exact (envok_with_method_keyeq C keys n d H r m g k Env Found Equal).
Qed.

Lemma realizes_absorbs_canon : forall n C d H, Realizes n C d H -> AbsorbsCanon C d.
Proof.
  intros n C d H [Abs [_ [Section _]]]. apply (absorbs_canon_iff C d Section). exact Abs.
Qed.

(* The context and frame are formed (T29). *)
Definition Formed (F : A.frame) (C : A.cx) : Prop :=
  PublicFormation.FormedCx W C /\ PublicFormation.FrameFormed W F /\ PublicScoping.CxScoped W F C.

Definition Pre n F C d types H g st : Prop :=
  FrameOk F /\ CtxLocals F C d types /\ locals_typed W P H g types /\ state_ok H st /\ Realizes n C d H /\ Formed F C.

Lemma pre_absorbs_canon : forall n F C d types H g st,
  Pre n F C d types H g st -> AbsorbsCanon C d.
Proof. intros n F C d types H g st (_ & _ & _ & _ & Real & _). eapply realizes_absorbs_canon; eassumption. Qed.

Lemma pre_method_provision : forall n F C d types H locals state r m g,
  Pre n F C d types H locals state ->
  A.mlook r m (A.c_mths C) = Some g ->
  exists pv, lookup_provision (A.rkey C r m) d = Some pv /\
    CallableOk n H d (r :: A.sg_params g) (A.sg_ret g) pv.
Proof.
  intros n F C d types H locals state r m g
    (_ & _ & _ & _ & Real & _ & _ & Scoped) Found.
  pose proof Real as [_ [_ [Section Keys]]].
  pose proof Scoped as [_ [_ [_ Methods]]].
  apply (realizes_runtime_iff C Section n d H Methods) in Real.
  destruct (realizes_with_method_keyeq C (canonical_runtime_keys C Section)
    n d H r m g (A.mkey r m) Real Found (conj eq_refl (A.MERefl _ _)))
    as [pv [Lookup Callable]].
  exists pv. split; [|exact Callable].
  pose proof (PublicScoping.methsread_mlook_canonical W C r m g Methods Keys Found) as Stored.
  change (lookup_provision (normalized_rkey C (A.mkey r m)) d = Some pv) in Lookup.
  rewrite (normalized_rkey_stored C (A.mkey r m)) in Lookup.
  - exact Lookup.
  - change (A.mkey (A.canon C r) m = A.mkey r m). now rewrite Stored.
Qed.

(* The context code runs in after a demand: the type table of C0, where
   the code is, and the provisions the families added. The demand itself
   may read the table differently (an attached method's receiver). *)
Definition with_ty (C0 C : A.cx) : A.cx := A.Cx (A.c_ty C0) (A.c_sub C) (A.c_merge C) (A.c_provisions C) (A.c_locals C).

Notation Outcome := (AR.O.Outcome W P state_ok).

Definition SafeEx k : Prop := forall F C a exp x t, LegacyBodyJudgments.Ex W Prog F C a exp x t ->
  forall d types H g st, Pre k F C d types H g st ->
  Outcome F d H types t (A.Breaks W (A.fr_mod F) a) (AR.O.I.eval k P d g st x).
Definition SafeBlk k : Prop := forall F C ss tl exp x t made, LegacyBodyJudgments.Blk W Prog F C ss tl exp x t made ->
  (forall g, In g made -> ~ In g (A.gens t)) -> forall d types H g st, Pre k F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks W (A.fr_mod F) (ABlock ss tl)) (AR.O.I.eval k P d g st x).
Definition SafeBlkE k : Prop := forall F C b exp x t, LegacyBodyJudgments.BlkE W Prog F C b exp x t ->
  forall d types H g st, Pre k F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks W (A.fr_mod F) b) (AR.O.I.eval k P d g st x).
Definition SafeBranch k : Prop := forall F C y exp x t, LegacyBodyJudgments.Branch W Prog F C y exp x t ->
  forall d types H g st, Pre k F C d types H g st ->
  Outcome F d H types t (A.Breaks W (A.fr_mod F) y) (AR.O.I.eval k P d g st x).
Definition SafeCond k : Prop := forall F C c x, LegacyBodyJudgments.Cond W Prog F C c x ->
  forall d types H g st, Pre k F C d types H g st ->
  exists b, A.LangSym W "bool" A.NSName "Bool" b /\
    Outcome F d H types (TNom b []) (A.Breaks W (A.fr_mod F) c) (AR.O.I.eval k P d g st x).
Definition Safe k : Prop := SafeEx k /\ SafeBlk k /\ SafeBlkE k /\ SafeBranch k /\ SafeCond k.
Definition Below k : Prop := forall j, j <= k -> Safe j.

(* ---- obligations outside the slice -------------------------------------- *)

Definition StepSafe (k : nat) F C t (breaks : Prop) (x : expr) : Prop :=
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t breaks (AR.O.I.eval (S k) P d g st x).

(* A method a family supplies at the call: a functor application (T15/T16). *)
Hypothesis Reps : Platform.Representations W Prog.

Lemma forall2_keys : forall (reqs : list nat) (xs : list (nat * ty)) (R : nat -> ty -> Prop),
  Forall2 (fun r ra => fst ra = r /\ R r (snd ra)) reqs xs -> map fst xs = reqs.
Proof. intros reqs xs R F; induction F as [|r ra rs ras [E _] _ IH]; cbn; [reflexivity|congruence]. Qed.


(* ---- generic outcome facts ------------------------------------------------ *)

Lemma extends_trans : forall H1 H2 H3, heap_extends H1 H2 -> heap_extends H2 H3 -> heap_extends H1 H3.
Proof. unfold heap_extends; auto. Qed.

Lemma extends_refl : forall H, heap_extends H H.
Proof. unfold heap_extends; auto. Qed.

Lemma outcome_heap : forall F d H H' types t breaks r,
  heap_extends H H' -> Outcome F d H' types t breaks r -> Outcome F d H types t breaks r.
Proof.
  intros F d H H' types t breaks [[] g st] Ext Out; cbn in *; auto.
  - destruct Out as [H2 [E Rest]]; exists H2; split; [eapply extends_trans; eassumption|exact Rest].
  - destruct Out as [H2 [ret [Ret [E Rest]]]]; exists H2, ret; split; [exact Ret|].
    split; [eapply extends_trans; eassumption|exact Rest].
  - destruct Out as [Br [Loop [H2 [E Rest]]]]; split; [exact Br|]; split; [exact Loop|].
    exists H2; split; [eapply extends_trans; eassumption|exact Rest].
Qed.

Lemma callable_lower : forall n m H0 d params ret pv, m <= n ->
  CallableOk n H0 d params ret pv -> CallableOk m H0 d params ret pv.
Proof. apply PC.callable_lower. Qed.

Lemma callable_extends : forall n H H' d params ret pv, heap_extends H H' ->
  CallableOk n H d params ret pv -> CallableOk n H' d params ret pv.
Proof. apply PC.callable_extends. Qed.

Lemma callable_lower_extends : forall n m H0 H1 d params ret pv, m <= n -> heap_extends H0 H1 ->
  CallableOk n H0 d params ret pv -> CallableOk m H1 d params ret pv.
Proof. apply PC.callable_lower_extends. Qed.

(* Applicable closures stay applicable below their index, on a larger heap. *)
Lemma closure_lower : forall n m H0 H1, m <= n -> heap_extends H0 H1 ->
  (forall u gfx pv, ClosureOk n H0 u gfx pv -> ClosureOk m H1 u gfx pv) /\
  (forall gm dc l, CapturedFuns n H0 gm dc l -> CapturedFuns m H1 gm dc l).
Proof.
  intros n m H0 H1 Le Ext.
  apply (closure_ok_mut n H0 (fun u gfx pv _ => ClosureOk m H1 u gfx pv) (fun gm dc l _ => CapturedFuns m H1 gm dc l)).
  - intros u gg h Cu nsu Cg nsg linktarget dc gfx pins LB Binding FCu FCg FxIn AbU Caps _ IH PinsOk.
    apply (CLink m H1 u gg h Cu nsu Cg nsg linktarget dc gfx pins LB Binding FCu FCg FxIn AbU); [|exact IH|exact PinsOk].
    intros gm Am. destruct (Caps gm Am) as [Vals [Fns Mths]]. split; [|split].
    + intros v t I L. destruct (Vals v t I L) as [w [Look T]]. exists w. split; [exact Look|].
      eapply value_typed_extends; eassumption.
    + intros f g I L. destruct (Fns f g I L) as [pv [Look Ok]]. exists pv. split; [exact Look|].
      exact (callable_lower_extends _ _ _ _ _ _ _ _ Le Ext Ok).
    + intros r m' a g I L. destruct (Mths r m' a g I L) as [pv [Look Ok]]. exists pv. split; [exact Look|].
      exact (callable_lower_extends _ _ _ _ _ _ _ _ Le Ext Ok).
  - intros u h Cu nsu Ch nsh target code sms dc rms rm gfx gfh _ IH FCu FCh Incl Fx CO Sub SO Site.
    exact (CCopy m H1 u h Cu nsu Ch nsh target code sms dc rms rm gfx gfh IH FCu FCh Incl Fx CO Sub SO Site).
  - intros u code gfx Fam Mach PR. exact (CPlatform m H1 u code gfx Fam Mach PR).
  - intros gm dc. constructor.
  - intros gm dc u fx pv gfx rest Look _ IH1 Fx _ IH2. exact (CFCons m H1 gm dc u fx pv gfx rest Look IH1 Fx IH2).
Qed.

Lemma realizes_with_lower : forall rk n m C d H,
  m <= n -> RealizesWith rk n C d H -> RealizesWith rk m C d H.
Proof.
  intros rk n m C d H Le [Abs [[Vals [Fns [Mths Funs]]] St]]. split; [exact Abs|]. split; [|exact St].
  split; [exact Vals|]. split; [|split; [|intros u fx Found; destruct (Funs u fx Found) as [pv [gfx [Look [Ok Fm]]]];
    exists pv, gfx; split; [exact Look|split; [exact (proj1 (closure_lower n m H H Le (fun a c L => L)) _ _ _ Ok)|exact Fm]]]].
  - intros f g Found. destruct (Fns f g Found) as [pv [Look Ok]].
    exists pv; split; [exact Look|exact (callable_lower n m _ _ _ _ _ Le Ok)].
  - intros r m' g Found. destruct (Mths r m' g Found) as [pv [Look Ok]].
    exists pv; split; [exact Look|exact (callable_lower n m _ _ _ _ _ Le Ok)].
Qed.


Lemma realizes_lower : forall n m C d H, m <= n -> Realizes n C d H -> Realizes m C d H.
Proof. intros n m C d H. exact (realizes_with_lower (A.rkey_of C) n m C d H). Qed.

Lemma realizes_with_extends : forall rk n C d H H',
  heap_extends H H' -> RealizesWith rk n C d H -> RealizesWith rk n C d H'.
Proof.
  intros rk n C d H H' Ext [Abs [[Vals [Fns [Mths Funs]]] St]]. split; [exact Abs|].
  split; [|exact St]. split; [|split; [|split]].
  - intros v t Found. destruct (Vals v t Found) as [w [Look T]].
    exists w; split; [exact Look|eapply value_typed_extends; eassumption].
  - intros f g Found. destruct (Fns f g Found) as [pv [Look Ok]].
    exists pv; split; [exact Look|exact (callable_extends _ _ _ _ _ _ _ Ext Ok)].
  - intros r m g Found. destruct (Mths r m g Found) as [pv [Look Ok]].
    exists pv; split; [exact Look|exact (callable_extends _ _ _ _ _ _ _ Ext Ok)].
  - intros u fx Found. destruct (Funs u fx Found) as [pv [gfx [Look [Ok Fm]]]].
    exists pv, gfx; split; [exact Look|split; [exact (proj1 (closure_lower n n H H' (le_n n) Ext) _ _ _ Ok)|exact Fm]].
Qed.

Lemma realizes_extends : forall n C d H H',
  heap_extends H H' -> Realizes n C d H -> Realizes n C d H'.
Proof. intros n C d H H'. exact (realizes_with_extends (A.rkey_of C) n C d H H'). Qed.

Lemma realizes_same : forall n C C' d H,
  A.c_ty C' = A.c_ty C -> A.c_sub C' = A.c_sub C -> A.c_merge C' = A.c_merge C ->
  A.c_provisions C' = A.c_provisions C ->
  Realizes n C d H -> Realizes n C' d H.
Proof.
  intros n [ty sub classes provs locs] [ty' sub' classes' provs' locs'] d H; cbn; intros -> -> -> -> Real; exact Real.
Qed.

Lemma pre_lower : forall n F C d types H g st, Pre (S n) F C d types H g st -> Pre n F C d types H g st.
Proof.
  intros n F C d types H g st [Frame [Ctx [L [St [Real Fmd]]]]].
  exact (conj Frame (conj Ctx (conj L (conj St (conj (realizes_lower (S n) n C d H (le_S _ _ (le_n n)) Real) Fmd))))).
Qed.

Arguments pre_lower {n F C d types H g st} _.

Lemma pre_le : forall n m F C d types H g st, m <= n -> Pre n F C d types H g st -> Pre m F C d types H g st.
Proof.
  intros n m F C d types H g st Le [Frame [Ctx [L [St [Real Fmd]]]]].
  exact (conj Frame (conj Ctx (conj L (conj St (conj (realizes_lower n m C d H Le Real) Fmd))))).
Qed.

Lemma pre_after : forall n F C d types H g st H' g' st',
  Pre n F C d types H g st -> heap_extends H H' -> state_ok H' st' ->
  locals_typed W P H' g' types -> Pre n F C d types H' g' st'.
Proof.
  intros n F C d types H g st H' g' st' [Frame [Ctx [_ [_ [Real Fmd]]]]] Ext State Locals.
  exact (conj Frame (conj Ctx (conj Locals (conj State (conj (realizes_extends n C d H H' Ext Real) Fmd))))).
Qed.

(* A non-Normal result passes through unchanged. *)
Lemma outcome_pass : forall F d H types t t' (breaks breaks' : Prop) r,
  (breaks -> breaks') ->
  (forall value, AR.O.I.evaluation_signal r <> Normal value) ->
  Outcome F d H types t breaks r -> Outcome F d H types t' breaks' r.
Proof.
  intros F d H types t t' breaks breaks' [[] g st] Weaken NotNormal Out; cbn in *; auto.
  - exfalso; exact (NotNormal v eq_refl).
  - destruct Out as [Br Rest]; auto.
Qed.

Lemma outcome_meanings : forall F d d' H types t (B : Prop) r, type_meanings d' = type_meanings d ->
  Outcome F d' H types t B r -> Outcome F d H types t B r.
Proof.
  intros F d d' H types t B [[] g st] E Out; cbn in *; auto.
  - destruct Out as [H' [Ex [S [L T]]]]. exists H'. unfold ground in *. rewrite E in T. auto.
  - destruct Out as [H' [ret [R [Ex [S [L T]]]]]]. exists H', ret. unfold ground in *. rewrite E in T. auto.
Qed.

(* A state with the same meanings and aliases, a realized context and a
   larger heap keeps Pre. *)
Lemma pre_keep : forall r F C d d' types H H' g st st',
  Pre r F C d types H g st -> type_meanings d' = type_meanings d -> type_aliases d' = type_aliases d ->
  Realizes r C d' H' -> heap_extends H H' -> state_ok H' st' -> Pre r F C d' types H' g st'.
Proof.
  intros r F C d d' types H H' g st st' [Frame [[Lo [Re Al]] [L [St [Real Fm]]]]] Em Ea Real' Ext St'.
  assert (G : forall t, ground d' t = ground d t) by (intros t; unfold ground; rewrite Em; reflexivity).
  split; [exact Frame|]. split; [split; [|split]|split; [exact (locals_extends W P H H' g types Ext L)|split; [exact St'|split; [exact Real'|exact Fm]]]].
  - intros n t mu Found Nm. rewrite G. exact (Lo n t mu Found Nm).
  - intros t Th. rewrite G. exact (Re t Th).
  - intros x tx Lx. unfold cmeaning, output_meaning. rewrite Ea, Em. rewrite G. exact (Al x tx Lx).
Qed.

Lemma eval_bind_eq0 : forall j d g st b x,
  AR.O.I.eval (S j) P d g st (EBind b x) =
  match AR.O.I.eval_bindings j P d g st [b] with
  | inl (d1, g1, st1) => AR.O.I.eval j P d1 g1 st1 x
  | inr r => r
  end.
Proof. reflexivity. Qed.

(* The demand's context C reads its table in the code's scope (C0's):
   each entry ok and mentioning only C0's generations. *)
Definition FamilyTableIn (C C0 : A.cx) : Prop :=
  forall x v, In (x, v) (A.c_ty C) ->
    PublicFormation.TyOk W (A.canon C v) /\ PublicFormation.ScopedTy W C0 (A.canon C v).

(* A context's method provisions agree with a family's new provision at
   the same receiver, method and θ (so at the same run-time key). *)
Definition FamilyCoherent (Cx Cr : A.cx) (ks : list item) : Prop :=
  forall r m g a g' a', In (A.mkey r m) ks ->
  In (A.mkey r m, A.ProvidedMethod g a) (A.c_provisions Cx) ->
  A.ilook (A.mkey r m) (A.c_provisions Cr) = Some (A.ProvidedMethod g' a') ->
  A.theta_of Cx a = A.theta_of Cr a' -> g = g'.

(* HYPOTHESIS (T15/T16). A family application, as one binding: it
   completes with the families' provisions added (the code's own context,
   and any narrower one whose methods agree with the family's at a shared
   run-time key, stays realized), or it stops. Its matching is read off a
   demanded key whose receiver is ok and in the code's scope (proved:
   PublicFamilyStep.family_step_ok). *)
Definition FamilyStepOk : Prop := forall k, Below k -> forall C0 C u matching th sm Cr ks ot m,
  A.c_sub C = A.c_sub C0 -> A.c_merge C = A.c_merge C0 ->
  A.c_provisions C = A.c_provisions C0 -> A.c_locals C = A.c_locals C0 ->
  AuditFacts.FamilyApplication W C u matching th sm Cr ks -> A.FamilyReadOff W C u ot m matching ->
  PublicFormation.TyOk W ot -> PublicFormation.ScopedTy W C0 ot -> FamilyTableIn C C0 -> PublicFormation.TysScoped W C ->
  forall F rr j d types H g st, rr <= S k -> S j <= rr -> Pre rr F C0 d types H g st -> forall rest,
  (exists d' st' H', heap_extends H H' /\ type_meanings d' = type_meanings d /\ type_aliases d' = type_aliases d /\
     Pre rr F (with_ty C0 Cr) d' types H' g st' /\
     (forall Cx, incl (A.c_provisions Cx) (A.c_provisions C0) -> A.c_sub Cx = A.c_sub C0 -> FamilyCoherent Cx Cr ks ->
        Realizes rr Cx d H -> Realizes rr Cx d' H') /\
     AR.O.I.eval_bindings (S j) P d g st (BApply u th [] sm (map (A.rkey_of Cr) ks) :: rest) = AR.O.I.eval_bindings j P d' g st' rest) \/
  (exists res, AR.O.I.eval_bindings (S j) P d g st (BApply u th [] sm (map (A.rkey_of Cr) ks) :: rest) = inr res /\
     (forall v, AR.O.I.evaluation_signal res <> Normal v) /\ Outcome F d H types TUnit False res).
Hypothesis family_step : FamilyStepOk.

(* HYPOTHESIS (T29). The code's own context, formed, agrees with any
   family applied around it where they share a run-time key. Both are the
   method's declared signature read at θ; a formed context carries that
   for its method provisions (PublicScoping.MethsRead, in CxScoped).
   Proved: PublicFamilyCoherent.family_coherent. *)
Definition FamilyCoherentOk : Prop := forall F C, Formed F C ->
  forall C1 u matching th sm Cr ks, A.c_sub C1 = A.c_sub C -> incl (A.c_provisions C) (A.c_provisions C1) ->
  AuditFacts.FamilyApplication W C1 u matching th sm Cr ks -> FamilyCoherent C Cr ks.
Hypothesis family_coherent : FamilyCoherentOk.

(* HYPOTHESES (T29). A demanded family key's receiver is in the code's
   scope (PublicFormation.ScopedTy): an
   elaborated expression's or literal's type is scoped, and so is a
   callee's need read through θ at the call site (at a table X with the
   code's merge; rule B puts the callee's requirements in force there).
   Proved: PublicScoping (ex_scoped, literal_scoped, demand_at_th_scoped). *)
Definition ExScopedOk : Prop := forall F C a x t, LegacyBodyJudgments.Ex W Prog F C a None x t -> Formed F C ->
  PublicFormation.ScopedTy W C t.
Definition LitScopedOk : Prop := forall F C tree x t, LegacyBodyJudgments.Literal W C tree x t -> Formed F C ->
  PublicFormation.ScopedTy W C t.
Definition DemandScopedOk : Prop := forall F C X f Cf ns th r m, Formed F C -> FamilyTableIn X C ->
  A.c_sub X = A.c_sub C -> CxSection X -> A.CtxOf W f Cf ns -> A.CallTheta W X Cf f th -> A.RequirementsInForce X Cf ->
  In (A.mkey r m) (map (A.at_th X th) ns) ->
  PublicFormation.TyOk W r /\ PublicFormation.ScopedTy W C r.
Hypothesis ex_scoped : ExScopedOk.
Hypothesis lit_scoped : LitScopedOk.
Hypothesis demand_scoped : DemandScopedOk.

Lemma table_in_self : forall F C, Formed F C -> FamilyTableIn C C.
Proof.
  intros F C [Fc _] x v Iq. split.
  - exact (PublicFormation.canon_ok W C v (proj1 Fc) (PublicFormation.cf_ty W C (proj1 Fc) x v Iq)).
  - intros y Iy. exact (PublicFormation.scope_ty_canon C x v y Iq Iy).
Qed.

(* Code after a family application runs with the families' provisions
   and the code's own context both realized. *)
Lemma family_then : forall k, Below k -> forall C u matching th sm Cr ks ot m F t (B : Prop) x,
  AuditFacts.FamilyApplication W C u matching th sm Cr ks -> A.FamilyReadOff W C u ot m matching ->
  (Formed F C -> PublicFormation.TyOk W ot /\ PublicFormation.ScopedTy W C ot) ->
  (forall j, j <= k -> forall d types H g st, Pre j F (with_ty C Cr) d types H g st -> Pre j F C d types H g st ->
     Outcome F d H types t B (AR.O.I.eval j P d g st x)) ->
  StepSafe k F C t B (EBind (BApply u th [] sm (map (A.rkey_of Cr) ks)) x).
Proof.
  intros k Bel C u matching th sm Cr ks ot m F t B x FA Ro Rcv Cont d types H g st Pre0.
  rewrite eval_bind_eq0. destruct k as [|k1]; [cbn; exact Logic.I|].
  pose proof Pre0 as [_ [_ [_ [_ [Real0 Fm0]]]]].
  destruct (Rcv Fm0) as [Ok Sc].
  destruct (family_step (S k1) Bel C C u matching th sm Cr ks ot m eq_refl eq_refl eq_refl eq_refl FA Ro Ok Sc (table_in_self F C Fm0) (proj2 (proj2 (proj1 Fm0)))
    F (S (S k1)) k1 d types H g st (le_n _) ltac:(lia) Pre0 []) as [[d' [st' [H' [E [Em [Ea [PreN [Keep Run]]]]]]]]|[res [Run [NN Out]]]].
  - rewrite Run. destruct k1 as [|k2]; [cbn; exact Logic.I|].
    assert (Enil : AR.O.I.eval_bindings (S k2) P d' g st' [] = inl (d', g, st')) by reflexivity. rewrite Enil.
    pose proof PreN as [_ [_ [_ [StN _]]]].
    assert (Coh : FamilyCoherent C Cr ks) by exact (family_coherent F C Fm0 C u matching th sm Cr ks eq_refl (fun q I => I) FA).
    pose proof (pre_keep _ _ _ _ _ _ _ _ _ _ _ Pre0 Em Ea (Keep C (fun q I => I) eq_refl Coh Real0) E StN) as PreC.
    eapply outcome_heap; [exact E|]. apply (outcome_meanings F d d' H' types t B _ Em).
    exact (Cont (S (S k2)) ltac:(lia) d' types H' g st' (pre_lower PreN) (pre_lower PreC)).
  - rewrite Run. eapply outcome_pass; [|exact NN|exact Out]. intros [].
Qed.

Lemma safe_zero : Safe 0.
Proof.
  split; [red; intros; exact Logic.I|]. split; [red; intros; exact Logic.I|]. split; [red; intros; exact Logic.I|].
  split; [red; intros; exact Logic.I|].
  red; intros F C c x D. destruct D as [? ? ? ? ? b Lang ? ?]. intros.
  exists b; split; [exact Lang|exact Logic.I].
Qed.

Lemma outcome_now : forall n F C d types H g st t (breaks : Prop) w,
  Pre n F C d types H g st -> value_typed W P H w (ground d t) ->
  Outcome F d H types t breaks (AR.O.I.Evaluation (Normal w) g st).
Proof.
  intros n F C d types H g st t breaks w [_ [_ [Locals [State _]]]] Typed.
  exists H; repeat split; auto. intros a c E; exact E.
Qed.

Lemma ground_never : forall d H w, ~ value_typed W P H w (ground d A.never).
Proof. intros d H w Typed. exact (empty_union_uninhabited W P H w Typed). Qed.

Lemma outcome_pop : forall F d H types x tau t breaks r,
  Outcome F d H ((x, tau) :: types) t breaks r -> Outcome F d H types t breaks (AR.O.I.pop1 r).
Proof.
  intros F d H types x tau t breaks [[] g st] Out; cbn in *; auto.
  - destruct Out as [H' [E [S [L T]]]]. exists H'; repeat split; auto.
    exact (locals_pop W P H' g _ L).
  - destruct Out as [H' [ret [R [E [S [L T]]]]]]. exists H', ret; repeat split; auto.
    exact (locals_pop W P H' g _ L).
  - destruct Out as [B [Loop [H' [E [S L]]]]]. split; [exact B|]. split; [exact Loop|].
    exists H'; repeat split; auto. exact (locals_pop W P H' g _ L).
Qed.

Lemma skipn_app_length : forall X (l m : list X), skipn (List.length l) (l ++ m) = m.
Proof. intros X l m; induction l; cbn; auto. Qed.

Lemma outcome_popn : forall F d H types front n t breaks r,
  List.length front = n ->
  Outcome F d H (front ++ types) t breaks r -> Outcome F d H types t breaks (AR.O.I.popn n r).
Proof.
  intros F d H types front n t breaks [[] g st] Len Out; subst n; cbn in *; auto.
  - destruct Out as [H' [E [S [L T]]]]. exists H'; repeat split; auto.
    rewrite <- (skipn_app_length _ front types). exact (locals_popn W P _ H' g _ L).
  - destruct Out as [H' [ret [R [E [S [L T]]]]]]. exists H', ret; repeat split; auto.
    rewrite <- (skipn_app_length _ front types). exact (locals_popn W P _ H' g _ L).
  - destruct Out as [B [Loop [H' [E [S L]]]]]. split; [exact B|]. split; [exact Loop|].
    exists H'; repeat split; auto.
    rewrite <- (skipn_app_length _ front types). exact (locals_popn W P _ H' g _ L).
Qed.

Lemma pre_loop : forall n F C d types H g st,
  Pre n F C d types H g st -> Pre n (A.with_loop F) C d types H g st.
Proof. intros; assumption. Qed.

(* A loop body's outcome, other than Breaking, is its function's. *)
Lemma outcome_unloop : forall F d H types t breaks r,
  AR.O.I.evaluation_signal r <> Breaking ->
  Outcome (A.with_loop F) d H types t breaks r -> Outcome F d H types t breaks r.
Proof. intros F d H types t breaks [[] g st] NotBreak Out; cbn in *; auto. congruence. Qed.

(* An inlined constant's frame neither returns nor breaks. *)
Lemma outcome_const : forall F0 F d H types t (breaks breaks' : Prop) r,
  A.fr_ret F0 = None -> A.fr_loop F0 = false ->
  Outcome F0 d H types t breaks r -> Outcome F d H types t breaks' r.
Proof.
  intros F0 F d H types t breaks breaks' [[] g st] Ret Loop Out; cbn in *; auto.
  - destruct Out as [H' [ret [R _]]]; congruence.
  - destruct Out as [_ [L _]]; congruence.
Qed.

(* D89: what can break in a bracketed call's expansion is one of its
   arguments; the binds and the `%arg` paths cannot. *)
Lemma call_block_breaks : forall md pa bs args,
  A.Breaks W md (Audit.Syntax.call_block pa bs args) -> A.Breaks W md (AECall pa (Some bs) args).
Proof.
  intros md pa bs args B. unfold Audit.Syntax.call_block in B.
  inversion B; subst; clear B.
  match goal with BB : A.BlockBreaks _ _ _ |- _ => inversion BB; subst; clear BB end.
  - match goal with InS : In _ (List.app _ _) |- _ => apply in_app_or in InS as [InL|InB] end.
    + apply in_map_iff in InL as [[nm y] [Eq InC]]. subst. cbn in *.
      match goal with SB : A.StmtBreaks _ _ _ |- _ => inversion SB; subst end.
      eapply A.BkCall; [eapply in_combine_r; exact InC|eassumption].
    + destruct InB as [Eq|[]]. subst.
      match goal with SB : A.StmtBreaks _ _ _ |- _ => inversion SB; subst end.
      match goal with InI : In _ (map _ _) |- _ => apply in_map_iff in InI as [b0 [Eq _]] end.
      subst. destruct b0 as [bp [[rp|] rk ra]]; cbn in *;
        match goal with IB : A.ItemBreaks _ _ _ |- _ => inversion IB; subst end;
        match goal with By : A.Breaks _ _ _ |- _ => inversion By end.
  - match goal with BT : A.Breaks _ _ (AECall _ _ _) |- _ => inversion BT; subst end.
    match goal with InY : In _ (map _ _) |- _ => apply in_map_iff in InY as [n0 [Eq _]] end.
    subst. match goal with By : A.Breaks _ _ (AEPath _ _) |- _ => inversion By end.
Qed.

Lemma block_breaks_cons : forall md s rest tl,
  A.BlockBreaks W md (ABlock rest tl) -> A.BlockBreaks W md (ABlock (s :: rest) tl).
Proof.
  intros md s rest tl B. inversion B; subst.
  - eapply A.BBStmt; [right; eassumption|eassumption].
  - apply A.BBTail; assumption.
Qed.

Lemma stmt_breaks_block : forall md s rest tl,
  A.StmtBreaks W md s -> A.BlockBreaks W md (ABlock (s :: rest) tl).
Proof. intros md s rest tl B. eapply A.BBStmt; [left; reflexivity|exact B]. Qed.

Lemma path_never_breaks : forall md pa a, ~ A.Breaks W md (AEPath pa a).
Proof. intros md pa a B; inversion B. Qed.

Lemma assoc_slook : forall f (fs : list (fld * value)), AR.O.I.assoc_fld f fs = A.slook f fs.
Proof. intros f fs; induction fs as [|[g v] r IH]; cbn; [reflexivity|rewrite IH; reflexivity]. Qed.

(* A block without a tail yields () or diverges. *)
Lemma tailless_block : forall F C ss tl exp x t made,
  LegacyBodyJudgments.Blk W Prog F C ss tl exp x t made -> tl = None -> t = TUnit \/ t = A.never.
Proof.
  intros F C ss tl exp x t made D. induction D; intros Tail; try discriminate; auto.
  destruct (ty_eqb vt A.never); auto.
Qed.

Lemma blke_branch_ty : forall F C b exp x t,
  LegacyBodyJudgments.BlkE W Prog F C b exp x t -> A.fits t (A.branch_ty b t) /\ A.opt_fits t exp.
Proof.
  intros F C b exp x t D. destruct D as [F C ss tl exp x t made Blk _ Fits]. split; [|exact Fits].
  destruct tl; cbn; [apply fits_refl|].
  destruct (tailless_block _ _ _ _ _ _ _ _ Blk eq_refl) as [->| ->]; [apply fits_refl|apply fits_never].
Qed.

Lemma truthy_same : forall w, AR.O.I.truthy P w = B.I.truthy P w.
Proof. reflexivity. Qed.

Lemma below_le : forall k j, Below k -> j <= k -> Safe j.
Proof. intros k j Bel Le; exact (Bel j Le). Qed.

Lemma typed_unit : forall d H, value_typed W P H VUnit (ground d TUnit).
Proof. intros; constructor. Qed.

Lemma while_safe : forall k, Below k -> forall j, j <= k ->
  forall F C c cx b bx bt d types H g st,
  LegacyBodyJudgments.Cond W Prog F C c cx -> LegacyBodyJudgments.BlkE W Prog (A.with_loop F) C b None bx bt ->
  Pre j F C d types H g st ->
  Outcome F d H types TUnit (A.Breaks W (A.fr_mod F) c) (AR.O.I.eval j P d g st (EWhile cx bx)).
Proof.
  intros k Bel j. induction j as [|j IH]; intros Le F C c cx b bx bt d types H g st Cond Body Pre0;
    [exact Logic.I|].
  destruct (Bel j ltac:(lia)) as [_ [_ [SBlkE [_ SCond]]]].
  destruct (SCond F C c cx Cond d types H g st (pre_lower Pre0)) as [bsym [Lang OutC]].
  cbn [AR.O.I.eval]. destruct (AR.O.I.eval j P d g st cx) as [sig g1 st1] eqn:EC.
  destruct sig; try (eapply outcome_pass with (t := TNom bsym []); [exact (fun x => x)|cbn; congruence|exact OutC]).
  destruct OutC as [H1 [E1 [S1 [L1 T1]]]].
  assert (Bool : value_typed W (program_of Prog argv read) H1 v (TNom bsym [])) by exact T1.
  destruct (B.truthy_typed W Prog argv read H1 bsym v Bools Lang Bool) as [bit Bit].
  replace (AR.O.I.truthy P v) with (Some bit) by (symmetry; exact Bit). destruct bit.
  - pose proof (pre_lower (pre_after _ _ _ _ _ _ _ _ H1 g1 st1 Pre0 E1 S1 L1)) as Pre1.
    pose proof (SBlkE (A.with_loop F) C b None bx bt Body d types H1 g1 st1 (pre_loop _ _ _ _ _ _ _ _ Pre1)) as OutB.
    destruct (AR.O.I.eval j P d g1 st1 bx) as [sig2 g2 st2] eqn:EB.
    destruct sig2; cbn in OutB |- *; auto.
    + destruct OutB as [H2 [E2 [S2 [L2 _]]]].
      eapply outcome_heap; [exact E1|]. eapply outcome_heap; [exact E2|].
      eapply IH; [lia|exact Cond|exact Body|].
      eapply pre_after; [exact Pre1|exact E2|exact S2|exact L2].
    + destruct OutB as [H2 [ret [R [E2 [S2 [L2 T2]]]]]].
      exists H2, ret; repeat split; auto. eapply extends_trans; eassumption.
    + destruct OutB as [_ [_ [H2 [E2 [S2 L2]]]]].
      exists H2; repeat split; auto; [eapply extends_trans; eassumption|constructor].
  - exists H1; repeat split; auto. constructor.
Qed.

Lemma loop_safe : forall k, Below k -> forall j, j <= k ->
  forall F C b bx bt d types H g st,
  LegacyBodyJudgments.BlkE W Prog (A.with_loop F) C b None bx bt ->
  Pre j F C d types H g st ->
  Outcome F d H types TUnit False (AR.O.I.eval j P d g st (ELoop bx)) /\
  (forall w, AR.O.I.evaluation_signal (AR.O.I.eval j P d g st (ELoop bx)) = Normal w -> A.BlockBreaks W (A.fr_mod F) b).
Proof.
  intros k Bel j. induction j as [|j IH]; intros Le F C b bx bt d types H g st Body Pre0;
    [split; [exact Logic.I|cbn; discriminate]|].
  destruct (Bel j ltac:(lia)) as [_ [_ [SBlkE _]]].
  pose proof (SBlkE (A.with_loop F) C b None bx bt Body d types H g st (pre_loop _ _ _ _ _ _ _ _ (pre_lower Pre0))) as OutB.
  cbn [AR.O.I.eval]. destruct (AR.O.I.eval j P d g st bx) as [sig g2 st2] eqn:EB.
  destruct sig; cbn in OutB |- *; try (split; [exact Logic.I|discriminate]).
  - destruct OutB as [H2 [E2 [S2 [L2 _]]]].
    destruct (IH ltac:(lia) F C b bx bt d types H2 g2 st2 Body
      (pre_lower (pre_after _ _ _ _ _ _ _ _ _ _ _ Pre0 E2 S2 L2))) as [Out Normal'].
    split; [eapply outcome_heap; eassumption|exact Normal'].
  - destruct OutB as [H2 [ret [R [E2 [S2 [L2 T2]]]]]].
    split; [|discriminate]. exists H2, ret; repeat split; auto.
  - destruct OutB as [Brk [_ [H2 [E2 [S2 L2]]]]].
    split; [exists H2; repeat split; auto; constructor|intros; exact Brk].
  - contradiction.
Qed.

(* Arguments evaluated in order: typed values, or a non-Normal outcome. *)
Definition ArgSafe F C (breaks : Prop) (e : expr) (t : ty) : Prop :=
  exists a exp t', LegacyBodyJudgments.Ex W Prog F C a exp e t' /\
    (A.fits t' t \/ A.fits (A.canon C t') (A.canon C t)) /\ (A.Breaks W (A.fr_mod F) a -> breaks).

Lemma outcome_conv : forall F d H types t t' breaks r,
  (forall H' w, value_typed W P H' w (ground d t) -> value_typed W P H' w (ground d t')) ->
  Outcome F d H types t breaks r -> Outcome F d H types t' breaks r.
Proof.
  intros F d H types t t' breaks [[] g st] Conv Out; cbn in *; auto.
  destruct Out as [H' [E [S [L T]]]]. exists H'; repeat split; auto.
Qed.

Lemma canon_fits_ground : forall C d H w t t', AbsorbsCanon C d ->
  (A.fits t t' \/ A.fits (A.canon C t) (A.canon C t')) ->
  value_typed W P H w (ground d t) -> value_typed W P H w (ground d t').
Proof.
  intros C d H w t t' Abs [Fits|Fits] T; [exact (fits_ground W P H d w t t' Fits T)|].
  rewrite <- Abs in T |- *. exact (fits_ground W P H d w _ _ Fits T).
Qed.

Lemma args_safe : forall k, Below k -> forall j, j <= k ->
  forall F C (breaks : Prop) es tys d types H g st,
  Forall2 (ArgSafe F C breaks) es tys -> Pre j F C d types H g st ->
  match AR.O.I.eval_arguments j P d g st es with
  | inl (ws, g', st') => exists H', heap_extends H H' /\ state_ok H' st' /\
      locals_typed W P H' g' types /\ Forall2 (fun w t => value_typed W P H' w (ground d t)) ws tys
  | inr r => Outcome F d H types TUnit breaks r /\ forall w, AR.O.I.evaluation_signal r <> Normal w
  end.
Proof.
  intros k Bel j. induction j as [|j IH]; intros Le F C breaks es tys d types H g st Args Pre0;
    [split; [exact Logic.I|cbn; discriminate]|].
  destruct Args as [|e t es tys [a [exp [t' [D [Fits Brk]]]]] Rest]; cbn [AR.O.I.eval_arguments].
  - destruct Pre0 as [_ [_ [L [S _]]]]. exists H; repeat split; auto. intros ? ? E; exact E.
  - destruct (Bel j ltac:(lia)) as [SEx _].
    pose proof (outcome_conv _ _ _ _ _ _ _ _
      (fun H' w => canon_fits_ground C d H' w t' t (pre_absorbs_canon _ _ _ _ _ _ _ _ Pre0) Fits)
      (SEx F C a exp e t' D d types H g st (pre_lower Pre0))) as Out.
    destruct (AR.O.I.eval j P d g st e) as [sig g1 st1] eqn:E.
    destruct sig; cbn in Out;
      try (split; [eapply outcome_pass; [exact Brk|cbn; congruence|exact Out]|cbn; congruence]).
    destruct Out as [H1 [E1 [S1 [L1 T1]]]].
    pose proof (IH ltac:(lia) F C breaks es tys d types H1 g1 st1 Rest
      (pre_lower (pre_after _ _ _ _ _ _ _ _ _ _ _ Pre0 E1 S1 L1))) as Next.
    destruct (AR.O.I.eval_arguments j P d g1 st1 es) as [[[ws g2] st2]|r].
    + destruct Next as [H2 [E2 [S2 [L2 T2]]]]. exists H2; repeat split; auto.
      * eapply extends_trans; eassumption.
      * constructor; [eapply value_typed_extends; eassumption|exact T2].
    + destruct Next as [Out NotNormal]. split; [eapply outcome_heap; eassumption|exact NotNormal].
Unshelve. all: exact TUnit.
Qed.

Lemma outcome_tag : forall F d H types s xs (breaks : Prop) sig g st,
  state_ok H st -> locals_typed W P H g types ->
  match sig with
  | Normal r => value_typed W P H r (ground d (TNom s xs))
  | Trap _ => True
  | _ => False
  end ->
  Outcome F d H types (TNom s xs) breaks (AR.O.I.Evaluation sig g st).
Proof.
  intros F d H types s xs breaks sig g st State Locals Result.
  destruct sig; cbn in *; try contradiction; auto.
  exists H; repeat split; auto. intros ? ? E; exact E.
Qed.

Lemma let_breaks : forall md st n ann y mu, A.LetStmt st n ann y mu -> A.Breaks W md y -> A.StmtBreaks W md st.
Proof. intros md st n ann y mu Let B; destruct Let; [apply A.SBLet|apply A.SBVar]; exact B. Qed.

Lemma unit_never_breaks : forall md, ~ A.Breaks W md AEUnit.
Proof. intros md B; inversion B. Qed.

Lemma unit_value : forall H w d, value_typed W P H w (ground d TUnit) -> w = VUnit.
Proof. intros H w d T; inversion T; reflexivity. Qed.

Lemma add_locals_ty : forall binds C,
  A.c_ty (fold_left (fun C q => A.add_local C (fst q) (snd q) A.LLet) binds C) = A.c_ty C.
Proof. induction binds as [|q r IH]; intros C; [reflexivity|cbn; rewrite IH; reflexivity]. Qed.

Lemma add_locals_provisions : forall binds C,
  A.c_provisions (fold_left (fun C q => A.add_local C (fst q) (snd q) A.LLet) binds C) = A.c_provisions C.
Proof. induction binds as [|q r IH]; intros C; [reflexivity|cbn; rewrite IH; reflexivity]. Qed.

Lemma add_locals_sub : forall binds C,
  A.c_sub (fold_left (fun C q => A.add_local C (fst q) (snd q) A.LLet) binds C) = A.c_sub C.
Proof. induction binds as [|q r IH]; intros C; [reflexivity|cbn; rewrite IH; reflexivity]. Qed.

Lemma add_locals_merge : forall binds C,
  A.c_merge (fold_left (fun C q => A.add_local C (fst q) (snd q) A.LLet) binds C) = A.c_merge C.
Proof. induction binds as [|q r IH]; intros C; [reflexivity|cbn; rewrite IH; reflexivity]. Qed.

Lemma record_args : forall F C pa a fs ftys xfs,
  Forall2 (fun ft xf => fst xf = fst ft /\
    exists n v vt, n = fst ft /\ In (n, v) fs /\
      LegacyBodyJudgments.Ex W Prog F C (match v with Some y => y | None => AEPath [n] None end) (Some (snd ft)) (snd xf) vt /\
      A.fits vt (snd ft)) ftys xfs ->
  Forall2 (ArgSafe F C (A.Breaks W (A.fr_mod F) (AERec pa a fs))) (map snd xfs) (map snd ftys) /\
  map fst xfs = map fst ftys.
Proof.
  intros F C pa a fs ftys xfs Fields. induction Fields as [|ft xf ftys xfs [Name [n [v [vt [-> [In' [D Fits]]]]]]] Rest [IHA IHN]];
    [split; constructor|].
  split; [|cbn; rewrite Name, IHN; reflexivity].
  constructor; [|exact IHA].
  exists (match v with Some y => y | None => AEPath [fst ft] None end), (Some (snd ft)), vt.
  split; [exact D|]. split; [left; exact Fits|].
  destruct v as [y|]; [intro B; eapply A.BkRec; [exact In'|exact B]|intro B; inversion B].
Qed.

Lemma fields_combine : forall H d ftys ws,
  Forall2 (fun w t => value_typed W P H w (ground d t)) ws (map snd ftys) ->
  fields_typed W P H (combine (map fst ftys) ws) (map (fun q => (fst q, ground d (snd q))) ftys).
Proof.
  intros H d ftys. induction ftys as [|[f t] r IH]; intros ws Typed; inversion Typed; subst; cbn;
    constructor; auto.
Qed.

Lemma record_typed : forall H d ftys (xfs : list (fld * expr)) ws,
  map fst xfs = map fst ftys ->
  Forall2 (fun w t => value_typed W P H w (ground d t)) ws (map snd ftys) ->
  value_typed W P H (VRecord (combine (map fst xfs) ws)) (ground d (TRecord ftys)).
Proof.
  intros H d ftys xfs ws Names T. rewrite ground_record, Names. constructor.
  apply fields_combine; exact T.
Qed.

Lemma record_eval : forall k, Below k ->
  forall F C pa a fs ftys xfs d types H g st,
  Forall2 (ArgSafe F C (A.Breaks W (A.fr_mod F) (AERec pa a fs))) (map snd xfs) (map snd ftys) ->
  map fst xfs = map fst ftys -> Pre k F C d types H g st ->
  Outcome F d H types (TRecord ftys) (A.Breaks W (A.fr_mod F) (AERec pa a fs)) (AR.O.I.eval k P d g st (ERecord xfs)).
Proof.
  intros [|k] Bel F C pa a fs ftys xfs d types H g st Args Names Pre0; [exact Logic.I|].
  cbn [AR.O.I.eval]. pose proof (args_safe (S k) Bel k ltac:(lia) _ _ _ _ _ d types H g st Args (pre_lower Pre0)) as Out.
  match goal with |- context [AR.O.I.eval_arguments ?j ?V ?d0 ?g0 ?s0 ?es] =>
    change (AR.O.I.eval_arguments j V d0 g0 s0 es) with (AR.O.I.eval_arguments k P d g st (map snd xfs)) end.
  destruct (AR.O.I.eval_arguments k P d g st (map snd xfs)) as [[[ws g'] st']|r]; cbn in Out; hnf.
  - destruct Out as [H' [E [S [L T]]]]. exists H'; repeat split; auto.
    match goal with |- value_typed _ _ _ _ ?T => change T with (ground d (TRecord ftys)) end.
    exact (record_typed _ _ _ _ _ Names T).
  - destruct Out as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
Qed.

Lemma assign_eval : forall k, Below k ->
  forall F C n y tv v vt d types H g st,
  A.slook n (A.c_locals C) = Some (tv, A.LVar) -> A.Named F n ->
  LegacyBodyJudgments.Ex W Prog F C y (Some tv) v vt -> A.fits vt tv -> Pre k F C d types H g st ->
  Outcome F d H types TUnit (A.Breaks W (A.fr_mod F) y) (AR.O.I.eval k P d g st (EAssign (A.var_of F n) v)).
Proof.
  intros [|k] Bel F C n y tv v vt d types H g st Found Named D Fits Pre0; [exact Logic.I|].
  destruct (Bel k ltac:(lia)) as [SEx _].
  pose proof (AR.O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits (SEx F C y _ v vt D d types H g st (pre_lower Pre0))) as Out.
  pose proof Pre0 as [_ [[Loc _] _]].
  cbn [AR.O.I.eval]. destruct (AR.O.I.eval k P d g st v) as [sig g1 st1] eqn:E.
  destruct sig; try (eapply outcome_pass with (t := tv); [exact (fun x => x)|cbn; congruence|exact Out]).
  destruct Out as [H1 [E1 [S1 [L1 T1]]]].
  destruct (locals_assign W P H1 g1 types (A.var_of F n) (ground d tv) v0 L1 (Loc n tv A.LVar Found Named) T1)
    as [g2 [Assign L2]].
  rewrite Assign. exists H1; repeat split; auto. constructor.
Qed.

Lemma args_argsafe : forall F C args ts xs, LegacyBodyJudgments.Args W Prog F C args ts xs ->
  forall (B : Prop), (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) -> Forall2 (ArgSafe F C B) xs ts.
Proof.
  intros F C args ts xs D. induction D as [F C|F C a r t ts x xs at' Dx Fits Rest IH]; intros B Brk; constructor.
  - exists a, (Some t), at'. split; [exact Dx|]. split; [right; exact Fits|]. intro Ba; exact (Brk a (or_introl eq_refl) Ba).
  - apply IH. intros a' In' Ba; exact (Brk a' (or_intror In') Ba).
Qed.

Lemma outcome_ret_call : forall F d H H1 types t (b : Prop) r g1,
  heap_extends H H1 -> locals_typed W P H1 g1 types -> CallOutcome H1 (ground d t) r ->
  Outcome F d H types t b (AR.O.I.ret_call g1 r).
Proof.
  intros F d H H1 types t b [[] l st] g1 E1 L1 Out; cbn in *; try contradiction; auto;
    destruct Out as [H' [E' [S' T']]]; exists H'; repeat split; auto;
    try (eapply extends_trans; eassumption); eapply locals_extends; eassumption.
Qed.

Lemma outcome_prim : forall F d H H1 types t (b : Prop) o st2 g1,
  heap_extends H H1 -> locals_typed W P H1 g1 types -> PrimOutcome H1 (ground d t) (o, st2) ->
  Outcome F d H types t b (AR.O.I.Evaluation o g1 st2).
Proof.
  intros F d H H1 types t b [] st2 g1 E1 L1 Out; cbn in *; try contradiction; auto.
  destruct Out as [H' [E' [S' T']]]; exists H'; repeat split; auto;
    [eapply extends_trans; eassumption|eapply locals_extends; eassumption].
Qed.

(* Sequencing: a Normal first result continues with a second evaluation. *)
Lemma outcome_seq : forall n F C d types H g st t1 t (b1 b : Prop) r1 (k2 : value -> locals -> Prims.mstate -> AR.O.I.evaluation),
  Pre n F C d types H g st -> (b1 -> b) ->
  Outcome F d H types t1 b1 r1 ->
  (forall H1 g1 st1 w, heap_extends H H1 -> Pre n F C d types H1 g1 st1 ->
     value_typed W P H1 w (ground d t1) -> Outcome F d H1 types t b (k2 w g1 st1)) ->
  Outcome F d H types t b
    (match r1 with
     | AR.O.I.Evaluation (Normal w) g1 st1 => k2 w g1 st1
     | AR.O.I.Evaluation (Returning v) g1 st1 => AR.O.I.Evaluation (Returning v) g1 st1
     | AR.O.I.Evaluation Breaking g1 st1 => AR.O.I.Evaluation Breaking g1 st1
     | AR.O.I.Evaluation (Stuck c) g1 st1 => AR.O.I.Evaluation (Stuck c) g1 st1
     | AR.O.I.Evaluation (Trap c) g1 st1 => AR.O.I.Evaluation (Trap c) g1 st1
     | AR.O.I.Evaluation (Exit c) g1 st1 => AR.O.I.Evaluation (Exit c) g1 st1
     | AR.O.I.Evaluation (UnsupportedOperation c) g1 st1 => AR.O.I.Evaluation (UnsupportedOperation c) g1 st1
     | AR.O.I.Evaluation OutOfFuel g1 st1 => AR.O.I.Evaluation OutOfFuel g1 st1
     end).
Proof.
  intros n F C d types H g st t1 t b1 b [[] g1 st1] k2 Pre0 Weaken Out Next;
    try (eapply outcome_pass; [exact Weaken|cbn; congruence|exact Out]).
  destruct Out as [H1 [E1 [S1 [L1 T1]]]].
  eapply outcome_heap; [exact E1|]. apply (Next H1 g1 st1 v E1); [|exact T1].
  eapply pre_after; eassumption.
Qed.

(* ---- code safe at every fuel up to n --------------------------------------- *)

Definition CodeSafe (n : nat) F C (t : ty) (B : Prop) (e : expr) : Prop :=
  forall j, j <= n -> forall d types H g st, Pre j F C d types H g st ->
  Outcome F d H types t B (AR.O.I.eval j P d g st e).

Lemma codesafe_le : forall n m F C t B e, m <= n -> CodeSafe n F C t B e -> CodeSafe m F C t B e.
Proof. intros n m F C t B e Le Safe j Lj; apply Safe; lia. Qed.

Lemma args_code : forall n j, j <= n ->
  forall F C (breaks : Prop) es tys d types H g st,
  Forall2 (fun e t => CodeSafe n F C t breaks e) es tys -> Pre j F C d types H g st ->
  match AR.O.I.eval_arguments j P d g st es with
  | inl (ws, g', st') => exists H', heap_extends H H' /\ state_ok H' st' /\
      locals_typed W P H' g' types /\ Forall2 (fun w t => value_typed W P H' w (ground d t)) ws tys
  | inr r => Outcome F d H types TUnit breaks r /\ forall w, AR.O.I.evaluation_signal r <> Normal w
  end.
Proof.
  intros n j. induction j as [|j IH]; intros Le F C breaks es tys d types H g st Args Pre0;
    [split; [exact Logic.I|cbn; discriminate]|].
  destruct Args as [|e t es tys Safe Rest]; cbn [AR.O.I.eval_arguments].
  - destruct Pre0 as [_ [_ [L [S _]]]]. exists H; repeat split; auto. intros ? ? E; exact E.
  - pose proof (Safe j ltac:(lia) d types H g st (pre_lower Pre0)) as Out.
    destruct (AR.O.I.eval j P d g st e) as [sig g1 st1] eqn:E.
    destruct sig; cbn in Out;
      try (split; [eapply outcome_pass; [exact (fun x => x)|cbn; congruence|exact Out]|cbn; congruence]).
    destruct Out as [H1 [E1 [S1 [L1 T1]]]].
    pose proof (IH ltac:(lia) F C breaks es tys d types H1 g1 st1 Rest
      (pre_lower (pre_after _ _ _ _ _ _ _ _ _ _ _ Pre0 E1 S1 L1))) as Next.
    destruct (AR.O.I.eval_arguments j P d g1 st1 es) as [[[ws g2] st2]|r].
    + destruct Next as [H2 [E2 [S2 [L2 T2]]]]. exists H2; repeat split; auto.
      * eapply extends_trans; eassumption.
      * constructor; [eapply value_typed_extends; eassumption|exact T2].
    + destruct Next as [Out NotNormal]. split; [eapply outcome_heap; eassumption|exact NotNormal].
Unshelve. all: exact TUnit.
Qed.

Lemma invoke_code : forall n F C (B : Prop) f g xs d types H lg lst,
  A.nlook f (A.c_fns C) = Some g ->
  Forall2 (fun e t => CodeSafe n F C t B e) xs (A.sg_params g) ->
  Pre (S n) F C d types H lg lst ->
  Outcome F d H types (A.sg_ret g) B (AR.O.I.eval (S n) P d lg lst (EInvoke f xs)).
Proof.
  intros n F C B f g xs d types H lg lst Found Args Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [[_ [[_ [Fns _]] _]] _]]]]]. destruct (Fns f g Found) as [pv [Look Ok]].
  pose proof (args_code n n (le_n n) F C B xs (A.sg_params g) d types H lg lst Args (pre_lower Pre0)) as AOut.
  cbn [AR.O.I.eval]. rewrite Look.
  destruct pv as [w|f' df|u sms dc|p held]; [destruct Ok| |destruct Ok|].
  - destruct Ok as [body [View Run]]. rewrite View.
    destruct (AR.O.I.eval_arguments n P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      exact (outcome_ret_call F d H H1 types _ _ _ g1 E1 L1 (Run n (Nat.lt_succ_diag_r n) H1 st1 ws E1 S1 T1)).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
  - destruct (AR.O.I.eval_arguments n P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      pose proof (Ok H1 st1 ws E1 S1 T1) as POut.
      destruct (AR.O.I.run_prim_at P p held ws st1) as [o st2] eqn:Run'.
      exact (outcome_prim F d H H1 types _ _ o st2 g1 E1 L1 POut).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
Unshelve. all: exact TUnit.
Qed.

Lemma method_code_at : forall n F C (B : Prop) ot m g ox xs d types H lg lst K pv,
  lookup_provision K d = Some pv -> CallableOk (S n) H d (ot :: A.sg_params g) (A.sg_ret g) pv ->
  CodeSafe n F C ot B ox ->
  Forall2 (fun e t => CodeSafe n F C t B e) xs (A.sg_params g) ->
  Pre (S n) F C d types H lg lst ->
  Outcome F d H types (A.sg_ret g) B (AR.O.I.eval (S n) P d lg lst (EMethod ox m K xs)).
Proof.
  intros n F C B ot m g ox xs d types H lg lst K pv Look Ok Recv Args Pre0.
  pose proof (Recv n (le_n n) d types H lg lst (pre_lower Pre0)) as ROut.
  cbn [AR.O.I.eval]. rewrite Look.
  destruct pv as [w|f' df|u sms dc|p held]; [destruct Ok| |destruct Ok|].
  - destruct Ok as [body [View Run]]. rewrite View.
    apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ (pre_lower Pre0) (fun b => b) ROut).
    intros H0 g0 st0 w0 E0 Pre1 T0.
    pose proof (args_code n n (le_n n) F C B xs (A.sg_params g) d types H0 g0 st0 Args Pre1) as AOut.
    destruct (AR.O.I.eval_arguments n P d g0 st0 xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      apply (outcome_ret_call F d H0 H1 types _ _ _ g1 E1 L1).
      apply (Run n (Nat.lt_succ_diag_r n) H1 st1 (w0 :: ws) (extends_trans _ _ _ E0 E1) S1). constructor; [|exact T1].
      eapply value_typed_extends; eassumption.
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
  - apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ (pre_lower Pre0) (fun b => b) ROut).
    intros H0 g0 st0 w0 E0 Pre1 T0.
    pose proof (args_code n n (le_n n) F C B xs (A.sg_params g) d types H0 g0 st0 Args Pre1) as AOut.
    destruct (AR.O.I.eval_arguments n P d g0 st0 xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      assert (Args' : Forall2 (fun w t => value_typed W P H1 w (ground d t)) (w0 :: ws) (ot :: A.sg_params g)).
      { constructor; [|exact T1]. eapply value_typed_extends; eassumption. }
      pose proof (Ok H1 st1 (w0 :: ws) (extends_trans _ _ _ E0 E1) S1 Args') as POut.
      destruct (AR.O.I.run_prim_at P p held (w0 :: ws) st1) as [o' st2] eqn:Run'.
      exact (outcome_prim F d H0 H1 types _ _ o' st2 g1 E1 L1 POut).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
Unshelve. all: exact TUnit.
Qed.

Lemma method_code : forall n F C (B : Prop) ot m g ox xs d types H lg lst,
  A.mlook ot m (A.c_mths C) = Some g ->
  CodeSafe n F C ot B ox ->
  Forall2 (fun e t => CodeSafe n F C t B e) xs (A.sg_params g) ->
  Pre (S n) F C d types H lg lst ->
  Outcome F d H types (A.sg_ret g) B (AR.O.I.eval (S n) P d lg lst (EMethod ox m (A.rkey C ot m) xs)).
Proof.
  intros n F C B ot m g ox xs d types H lg lst Found Recv Args Pre0.
  destruct (pre_method_provision _ _ _ _ _ _ _ _ _ _ _ Pre0 Found) as [pv [Look Ok]].
  exact (method_code_at n F C B ot m g ox xs d types H lg lst _ pv Look Ok Recv Args Pre0).
Qed.

(* Ex-derived code is safe below the induction's fuel, read at a fitting type. *)
Definition Conv (C : A.cx) (t' t : ty) : Prop :=
  A.fits t' t \/ A.fits (A.canon C t') (A.canon C t) \/ t = A.canon C t'.

Lemma conv_ground : forall C d H w t' t, AbsorbsCanon C d -> Conv C t' t ->
  value_typed W P H w (ground d t') -> value_typed W P H w (ground d t).
Proof.
  intros C d H w t' t Abs [Fits|[Fits| ->]] T.
  - exact (fits_ground W P H d w t' t Fits T).
  - rewrite <- Abs in T |- *. exact (fits_ground W P H d w _ _ Fits T).
  - rewrite Abs. exact T.
Qed.

Lemma ex_code : forall k, Below k -> forall F C a exp e t' t (B : Prop),
  LegacyBodyJudgments.Ex W Prog F C a exp e t' -> Conv C t' t -> (A.Breaks W (A.fr_mod F) a -> B) -> CodeSafe k F C t B e.
Proof.
  intros k Bel F C a exp e t' t B D Cv Brk j Le d types H g st Pre0.
  destruct (Bel j Le) as [SEx _].
  pose proof (SEx F C a exp e t' D d types H g st Pre0) as Out.
  pose proof (pre_absorbs_canon _ _ _ _ _ _ _ _ Pre0) as Abs.
  eapply AR.O.outcome_breaks; [exact Brk|].
  exact (outcome_conv _ _ _ _ _ _ _ _ (fun H' w => conv_ground C d H' w t' t Abs Cv) Out).
Qed.

Lemma args_code_of : forall k, Below k -> forall F C args ts xs, LegacyBodyJudgments.Args W Prog F C args ts xs ->
  forall (B : Prop), (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) -> Forall2 (fun e t => CodeSafe k F C t B e) xs ts.
Proof.
  intros k Bel F C args ts xs D. induction D as [F C|F C a r t ts x xs at' Dx Fits Rest IH]; intros B Brk; constructor.
  - eapply ex_code; [exact Bel|exact Dx|right; left; exact Fits|]. intro Ba; exact (Brk a (or_introl eq_refl) Ba).
  - apply IH. intros a' In' Ba; exact (Brk a' (or_intror In') Ba).
Qed.

Lemma codesafe_conv : forall n F C t' t B e,
  (forall d H w, AbsorbsCanon C d -> value_typed W P H w (ground d t') -> value_typed W P H w (ground d t)) ->
  CodeSafe n F C t' B e -> CodeSafe n F C t B e.
Proof.
  intros n F C t' t B e Conv Safe j Le d types H g st Pre0.
  pose proof (pre_absorbs_canon _ _ _ _ _ _ _ _ Pre0) as Abs.
  exact (outcome_conv _ _ _ _ _ _ _ _ (fun H' w => Conv d H' w Abs) (Safe j Le d types H g st Pre0)).
Qed.

(* Arithmetic a family supplies to a literal: a functor application (T15/T16). *)
Lemma family_method_case : forall k, Below k -> forall F C o ox ot0 m args u matching th sm C' ks g xs (B : Prop),
  LegacyBodyJudgments.Ex W Prog F C o None ox ot0 -> (A.Breaks W (A.fr_mod F) o -> B) -> (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) ->
  A.mlook (A.canon C ot0) m (A.c_mths C) = None ->
  AuditFacts.FamilyApplication W C u matching th sm C' ks -> A.FamilyReadOff W C u (A.canon C ot0) m matching ->
  (Formed F C -> PublicFormation.TyOk W (A.canon C ot0) /\ PublicFormation.ScopedTy W C (A.canon C ot0)) ->
  In (A.mkey (A.canon C ot0) m) ks -> A.OnlyFamily W C u (A.mkey (A.canon C ot0) m) ->
  A.mlook (A.canon C ot0) m (A.c_mths C') = Some g -> LegacyBodyJudgments.Args W Prog F C args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) B
    (apply_expression u th sm (map (A.rkey_of C') ks) (EMethod ox m (A.rkey C' (A.canon C ot0) m) xs)).
Proof.
  intros k Bel F C o ox ot0 m args u matching th sm C' ks g xs B Do BrkO BrkA None' FA Ro Rcv InK Only Found Args.
  unfold apply_expression. apply (family_then k Bel C u matching th sm C' ks _ m F _ B _ FA Ro Rcv).
  intros j Lj d types H lg st PreN PreC. destruct j as [|j1]; [exact Logic.I|].
  pose proof PreN as [_ [_ [_ [_ [[_ [[_ [_ [Mths _]]] _]] _]]]]].
  destruct (Mths (A.canon C ot0) m g Found) as [pv [Look Ok]].
  apply (method_code_at j1 F C B (A.canon C ot0) m g ox xs d types H lg st _ pv Look Ok).
  - apply (codesafe_le k); [lia|]. apply (ex_code k Bel F C o None ox ot0 _ B Do); [right; right; reflexivity|exact BrkO].
  - pose proof (args_code_of k Bel F C args _ xs Args B BrkA) as AC. clear -AC Lj.
    induction AC; constructor; [apply (codesafe_le k); [lia|assumption]|assumption].
  - exact PreC.
Qed.

Lemma literal_family_case : forall n, Below n -> forall F C t m ox oy u matching th sm C' ks,
  CodeSafe n F C t False ox -> CodeSafe n F C t False oy ->
  A.mlook t m (A.c_mths C) = None -> AuditFacts.FamilyApplication W C u matching th sm C' ks ->
  A.FamilyReadOff W C u t m matching ->
  (Formed F C -> PublicFormation.TyOk W t /\ PublicFormation.ScopedTy W C t) ->
  In (A.mkey t m) ks -> A.OnlyFamily W C u (A.mkey t m) -> A.mlook t m (A.c_mths C') = Some (A.FSig [t] t) ->
  StepSafe n F C t False (apply_expression u th sm (map (A.rkey_of C') ks) (EMethod ox m (A.rkey C' t m) [oy])).
Proof.
  intros n Bel F C t m ox oy u matching th sm C' ks Cx Cy None' FA Ro Rcv InK Only Found.
  unfold apply_expression. apply (family_then n Bel C u matching th sm C' ks t m F t False _ FA Ro Rcv).
  intros j Lj d types H lg st PreN PreC. destruct j as [|j1]; [exact Logic.I|].
  pose proof PreN as [_ [_ [_ [_ [[_ [[_ [_ [Mths _]]] _]] _]]]]].
  destruct (Mths t m _ Found) as [pv [Look Ok]].
  apply (method_code_at j1 F C False t m (A.FSig [t] t) ox [oy] d types H lg st _ pv Look Ok).
  - apply (codesafe_le n); [lia|exact Cx].
  - constructor; [apply (codesafe_le n); [lia|exact Cy]|constructor].
  - exact PreC.
Qed.

Lemma lit_step : forall n, Below n ->
  (forall F C tree x t, LegacyBodyJudgments.Literal W C tree x t -> CodeSafe n F C t False x) ->
  forall F C tree x t, LegacyBodyJudgments.Literal W C tree x t -> StepSafe n F C t False x.
Proof.
  intros n Bel IHc F C tree x t D d types H lg lst Pre0.
  assert (Op : forall a b x y result m,
    LegacyBodyJudgments.Literal W C a x t -> LegacyBodyJudgments.Literal W C b y t -> LegacyBodyJudgments.LitOp W C t m x y result ->
    Outcome F d H types t False (AR.O.I.eval (S n) P d lg lst result)).
  { intros a b x' y result m Da Db Lop. destruct Lop as [Found|u matching th formal needs C' ks None' Typed Ro InK Only Found].
    - refine (method_code n F C False t m (A.FSig [t] t) x' [y] d types H lg lst Found
        (IHc F C a x' t Da) _ Pre0).
      constructor; [exact (IHc F C b y t Db)|constructor].
    - pose (sm := A.satisfaction_map (A.family_context C matching) formal th needs).
      pose proof (AuditFacts.family_application_with_map Typed) as Fam.
      exact (literal_family_case n Bel F C t m x' y u matching th sm C' ks
        (IHc F C a x' t Da) (IHc F C b y t Db) None' Fam Ro
        (fun Fm => conj (PublicFormation.literal_ok W _ _ _ _ Da (proj1 (proj1 Fm))) (lit_scoped F C a x' t Da Fm))
        InK Only Found d types H lg lst Pre0). }
  destruct D as [z t Lang Val|o t Lang Val|a b x y result t m Da Db Lang Lop|a b x y result t m Da Db Lang Lop
                |a b x y result t m Da Db Lang Lop|a x ti tp t f Da Lang Fn Fits].
  - pose proof Pre0 as [_ [_ [_ [_ [[_ [[Vals _] _]] _]]]]]. destruct (Vals z t Val) as [w [Look T]].
    cbn [AR.O.I.eval]. rewrite Look. eapply outcome_now; eassumption.
  - pose proof Pre0 as [_ [_ [_ [_ [[_ [[Vals _] _]] _]]]]]. destruct (Vals o t Val) as [w [Look T]].
    cbn [AR.O.I.eval]. rewrite Look. eapply outcome_now; eassumption.
  - exact (Op a b x y result m Da Db Lop).
  - exact (Op a b x y result m Da Db Lop).
  - exact (Op a b x y result m Da Db Lop).
  - refine (invoke_code n F C False f (A.FSig [tp] t) [x] d types H lg lst Fn _ Pre0).
    constructor; [|constructor].
    apply (codesafe_conv n F C ti); [|exact (IHc F C a x ti Da)].
    intros d' H' w _ T. exact (fits_ground W P H' d' w ti tp Fits T).
Qed.

Lemma lit_code : forall n, Below n -> forall F C tree x t, LegacyBodyJudgments.Literal W C tree x t -> CodeSafe (S n) F C t False x.
Proof.
  induction n as [|n IH]; intros Bel F C tree x t D j Le d types H g st Pre0.
  - destruct j as [|j]; [exact Logic.I|]. assert (j = 0) as -> by lia.
    refine (lit_step 0 Bel _ F C tree x t D d types H g st Pre0).
    intros F' C' tree' x' t' D' j' Le' d' types' H' g' st' _. assert (j' = 0) as -> by lia. exact Logic.I.
  - assert (Bel' : Below n) by (intros i Li; apply Bel; lia).
    destruct (Nat.le_gt_cases j (S n)) as [Small|Large].
    + exact (IH Bel' F C tree x t D j Small d types H g st Pre0).
    + assert (j = S (S n)) as -> by lia.
      exact (lit_step (S n) Bel (fun F' C' tree' x' t' D' => IH Bel' F' C' tree' x' t' D') F C tree x t D d types H g st Pre0).
Qed.

(* ---- running a defined function's body ------------------------------------ *)

Hypothesis all_fn_elab : forall f dcl rcv n ps rt body,
  A.w_decl W f = Some dcl -> A.d_ast dcl = ADFn rcv n ps rt (Some body) -> LegacyBodyJudgments.FnElab W Prog f.

Lemma ctxof_locals : forall f C ns, A.CtxOf W f C ns -> A.c_locals C = [].
Proof.
  intros f C ns (d & es & tm & er & tmr & σ & classes & _ & _ & _ & _ & _ & _ & -> & _). reflexivity.
Qed.

Lemma slook_params : forall (names : list name) (ts : list ty) C n t mu,
  A.c_locals C = [] ->
  A.slook n (A.c_locals (fold_left (fun C' q => A.add_local C' (fst q) (snd q) A.LLet) (combine names ts) C)) = Some (t, mu) ->
  In (n, t) (combine names ts).
Proof.
  intros names ts C n t mu Empty.
  assert (General : forall l C, (forall t mu, A.slook n (A.c_locals C) = Some (t, mu) -> In (n, t) l) ->
    forall l', (forall q, In q l' -> In q l) ->
    A.slook n (A.c_locals (fold_left (fun C' q => A.add_local C' (fst q) (snd q) A.LLet) l' C)) = Some (t, mu) ->
    In (n, t) l).
  { intros l. intros C0 Base l'. revert C0 Base. induction l' as [|[m u] rest IH]; intros C0 Base Sub Found.
    - exact (Base t mu Found).
    - apply (IH (A.add_local C0 m u A.LLet)); [|intros q Iq; apply Sub; right; exact Iq|exact Found].
      intros t' mu' F'. cbn in F'. destruct (String.eqb_spec n m) as [->|_].
      + injection F' as <- _. apply Sub. left. reflexivity.
      + exact (Base t' mu' F'). }
  apply (General (combine names ts) C); [|intros q Iq; exact Iq].
  intros t' mu' F'. rewrite Empty in F'. discriminate.
Qed.

Lemma in_combine_nth : forall X Y (xs : list X) (ys : list Y) x y,
  In (x, y) (combine xs ys) -> exists i, nth_error xs i = Some x /\ nth_error ys i = Some y.
Proof.
  induction xs as [|a xs IH]; intros [|b ys] x y In'; cbn in In'; try contradiction.
  destruct In' as [E|In']; [injection E as -> ->; exists 0; auto|].
  destruct (IH ys x y In') as [i [E1 E2]]. exists (S i); auto.
Qed.

Lemma nlook_positions : forall (ts : list ty) i t start,
  nth_error ts i = Some t ->
  A.nlook (start + i) (combine (seq start (List.length ts)) ts) = Some t.
Proof.
  induction ts as [|u ts IH]; intros i t start Found; [destruct i; discriminate|].
  destruct i as [|i]; cbn in Found |- *.
  - injection Found as ->. rewrite Nat.add_0_r, Nat.eqb_refl. reflexivity.
  - destruct (Nat.eqb_spec (start + S i) start) as [E|_]; [lia|].
    replace (start + S i) with (S start + i) by lia. exact (IH i t (S start) Found).
Qed.

Lemma params_typed : forall H ws (ts : list ty) start,
  Forall2 (fun w t => value_typed W P H w t) ws ts ->
  locals_typed W P H (combine (seq start (List.length ws)) ws) (combine (seq start (List.length ts)) ts).
Proof.
  intros H ws ts start F. revert start. induction F as [|w t ws ts T Rest IH]; intros start; cbn;
    constructor; [split; [reflexivity|exact T]|apply IH].
Qed.


(* An environment whose aliases are its meanings reads a canonical type
   table in its aliases. *)
(* Every symbol of the type table has a meaning in d. *)
Definition KeysMeant (C : A.cx) (d : environment) : Prop :=
  forall k v, A.nlook k (A.c_ty C) = Some v -> lookup_subst k (type_meanings d) <> None.

Lemma alias_self : forall C d, type_aliases d = type_meanings d -> AbsorbsCanon C d ->
  (forall k v, A.nlook k (A.c_ty C) = Some v -> v = A.canon C (TAbs k)) -> KeysMeant C d -> AliasOk C d.
Proof.
  intros C d Al Abs Can Km x tx L. rewrite (Can x tx L). rewrite (Abs (TAbs x)).
  unfold output_meaning. rewrite Al. unfold ground. cbn.
  destruct (lookup_subst x (type_meanings d)) eqn:E; [reflexivity|exfalso; exact (Km x tx L E)].
Qed.

(* The body of a defined plain function, entered with a realized context and
   typed arguments, behaves as a call at the caller's types. *)
Lemma body_call : forall j, Safe j ->
  forall f dcl n ps rt b Cf ns de H st ws (d : environment) params ret,
  A.w_decl W f = Some dcl -> A.d_ast dcl = ADFn RPlain n ps rt (Some b) ->
  A.CtxOf W f Cf ns -> Realizes j Cf de H -> state_ok H st -> type_aliases de = type_meanings de -> KeysMeant Cf de ->
  (forall g0, A.SigOf W f (A.c_ty Cf) None g0 ->
     map (ground de) (A.sg_params g0) = map (ground d) params /\ ground de (A.sg_ret g0) = ground d ret) ->
  Forall2 (fun w t => value_typed W P H w (ground d t)) ws params ->
  exists body, view_function P f = Some body /\
    CallOutcome H (ground d ret) (AR.O.I.eval j P de (parameter_values ws) st body).
Proof.
  intros j Sj f dcl n ps rt b Cf ns de H st ws d params ret Decl Ast Ctx Real State Ali Km Agree Args.
  destruct (all_fn_elab f dcl RPlain n ps rt b Decl Ast dcl RPlain n ps rt b Decl Ast)
    as [C0 [ns0 [this [g0 [F0 [x [t0 [Ctx0 [This [Sig [_ [Ret [FThis [Loop [_ [_ [_ [Num [Body View]]]]]]]]]]]]]]]]]]].
  cbn in This. subst this.
  destruct (PublicContextFacts.function_context_unique W f C0 ns0 Cf ns Ctx0 Ctx) as [-> ->].
  exists x. split; [exact View|].
  destruct (Agree g0 Sig) as [Params Result].
  set (Cb := fold_left (fun C' q => A.add_local C' (fst q) (snd q) A.LLet) (combine (map fst ps) (A.sg_params g0)) Cf).
  set (types0 := combine (seq 0 (List.length (A.sg_params g0))) (map (ground de) (A.sg_params g0))).
  assert (Pre0 : Pre j F0 Cb de types0 H (parameter_values ws) st).
  { split; [apply (numbering_frame F0 (map fst ps)); rewrite FThis; exact Num|].
    split; [split; [|split; [|apply alias_self;
      [exact Ali|intros t; unfold Cb, A.canon; rewrite add_locals_sub; exact (realizes_absorbs_canon _ _ _ _ Real t)
      |intros k0 v0 L0; unfold Cb in L0 |- *; rewrite add_locals_ty in L0; unfold A.canon; rewrite add_locals_sub;
       exact (proj1 (ctxof_formed W _ Cf ns Ctx) k0 v0 L0)
      |intros k0 v0 L0; unfold Cb in L0; rewrite add_locals_ty in L0; exact (Km k0 v0 L0)]]]|].
    - intros pn t mu Found Named.
      apply slook_params in Found; [|exact (ctxof_locals f Cf ns Ctx)].
      apply in_combine_nth in Found as [i [Name Ty]].
      destruct Num as [_ [_ [_ Positions]]]. pose proof (Positions i pn Name) as Slot.
      unfold A.var_of. rewrite Slot. unfold types0.
      rewrite <- (map_length (ground de)).
      exact (nlook_positions (map (ground de) (A.sg_params g0)) i (ground de t) 0
        ltac:(rewrite nth_error_map, Ty; reflexivity)).
    - intros t' This'. congruence.
    - split; [|split; [exact State|]].
      + unfold parameter_values, types0. rewrite <- (map_length (ground de)).
        apply params_typed. rewrite Params.
        clear -Args. induction Args; constructor; auto.
      + split; [apply (realizes_same _ Cf); [apply add_locals_ty|apply add_locals_sub|apply add_locals_merge|apply add_locals_provisions|exact Real]|].
        destruct (PublicFormation.fn_body_formed W f Cf ns None g0 (map fst ps) mentions_typed Ctx Sig
          (PublicFormation.optok_none W)) as [Sg FBf].
        split; [exact FBf|]. split; [split; [rewrite Ret; apply PublicFormation.optok_some; exact (proj2 Sg)|rewrite FThis; apply PublicFormation.optok_none]|].
        exact (PublicScoping.fn_body_cxscoped W f Cf ns F0 None g0 (map fst ps) Ctx Sig FThis (PublicScoping.optscoped_none W Cf)). }
  destruct Sj as [_ [_ [SBlkE _]]].
  pose proof (SBlkE F0 Cb b (Some (A.sg_ret g0)) x t0 Body de types0 H (parameter_values ws) st Pre0) as Out.
  destruct (blke_branch_ty _ _ _ _ _ _ Body) as [_ Fits]. cbn in Fits.
  destruct (AR.O.I.eval j P de (parameter_values ws) st x) as [[] g' st']; cbn in Out |- *; auto.
  - destruct Out as [H' [E [S [_ T]]]]. exists H'; repeat split; auto.
    rewrite <- Result. exact (fits_ground W P H' de _ _ _ Fits T).
  - destruct Out as [H' [r [R [E [S [_ T]]]]]]. rewrite Ret in R. injection R as <-.
    exists H'; repeat split; auto. rewrite <- Result. exact T.
  - destruct Out as [_ [L _]]. rewrite Loop in L. discriminate.
Qed.

Lemma body_call_method : forall j, Safe j ->
  forall att dcl rn n ps rt b Cf ns de H st w0 ws (d : environment) recv params ret,
  A.w_decl W att = Some dcl -> A.d_ast dcl = ADFn (RAttached rn) n ps rt (Some b) ->
  A.CtxOf W att Cf ns -> Realizes j Cf de H -> state_ok H st -> type_aliases de = type_meanings de -> KeysMeant Cf de ->
  (forall r args g0, A.Attached W r n att ->
     Forall2 (fun x xa => fst xa = x /\ A.nlook x (A.c_ty Cf) = Some (snd xa)) (A.w_reqs W r) args ->
     A.SigOf W att (A.c_ty Cf) (Some (TNom r args)) g0 ->
     ground de (TNom r args) = ground d recv /\
     map (ground de) (A.sg_params g0) = map (ground d) params /\ ground de (A.sg_ret g0) = ground d ret) ->
  value_typed W P H w0 (ground d recv) ->
  Forall2 (fun w t => value_typed W P H w (ground d t)) ws params ->
  exists body, view_function P att = Some body /\
    CallOutcome H (ground d ret) (AR.O.I.eval j P de (parameter_values (w0 :: ws)) st body).
Proof.
  intros j Sj att dcl rn n ps rt b Cf ns de H st w0 ws d recv params ret Decl Ast Ctx Real State Ali Km Agree Recv Args.
  destruct (all_fn_elab att dcl (RAttached rn) n ps rt b Decl Ast dcl (RAttached rn) n ps rt b Decl Ast)
    as (C0 & ns0 & this & g0 & F0 & x & t0 & Ctx0 & Rcv & Sig & _ & Ret & FThis & Loop & _ & _ & _ & Num & Body & View).
  destruct Rcv as [r [args [Att [_ [-> Reqs]]]]].
  destruct (PublicContextFacts.function_context_unique W att C0 ns0 Cf ns Ctx0 Ctx) as [-> ->].
  exists x. split; [exact View|].
  destruct (Agree r args g0 Att Reqs Sig) as [This [Params Result]].
  set (Cb := fold_left (fun C' q => A.add_local C' (fst q) (snd q) A.LLet) (combine (map fst ps) (A.sg_params g0)) Cf).
  set (all := ground de (TNom r args) :: map (ground de) (A.sg_params g0)).
  set (types0 := combine (seq 0 (List.length all)) all).
  assert (Pre0 : Pre j F0 Cb de types0 H (parameter_values (w0 :: ws)) st).
  { split; [apply (numbering_frame F0 (map fst ps)); rewrite FThis; exact Num|].
    split; [split; [|split; [|apply alias_self;
      [exact Ali|intros t; unfold Cb, A.canon; rewrite add_locals_sub; exact (realizes_absorbs_canon _ _ _ _ Real t)
      |intros k0 v0 L0; unfold Cb in L0 |- *; rewrite add_locals_ty in L0; unfold A.canon; rewrite add_locals_sub;
       exact (proj1 (ctxof_formed W _ Cf ns Ctx) k0 v0 L0)
      |intros k0 v0 L0; unfold Cb in L0; rewrite add_locals_ty in L0; exact (Km k0 v0 L0)]]]|].
    - intros pn t mu Found Named.
      apply slook_params in Found; [|exact (ctxof_locals att Cf ns Ctx)].
      apply in_combine_nth in Found as [i [Name Ty]].
      destruct Num as [_ [_ [_ Positions]]]. pose proof (Positions i pn Name) as Slot.
      unfold A.var_of. rewrite Slot. unfold types0.
      exact (nlook_positions all (S i) (ground de t) 0 ltac:(cbn; rewrite nth_error_map, Ty; reflexivity)).
    - intros t' This'. rewrite FThis in This'. injection This' as <-. unfold types0.
      exact (nlook_positions all 0 _ 0 eq_refl).
    - split; [|split; [exact State|]].
      + unfold parameter_values, types0.
        apply params_typed. unfold all. constructor; [rewrite This; exact Recv|].
        rewrite Params. clear -Args. induction Args; constructor; auto.
      + split; [apply (realizes_same _ Cf); [apply add_locals_ty|apply add_locals_sub|apply add_locals_merge|apply add_locals_provisions|exact Real]|].
        pose proof (PublicFormation.ctxof_cx_formed W mentions_typed att Cf ns Ctx) as Fc.
        assert (Ot : PublicFormation.OptOk W (Some (TNom r args))).
        { apply PublicFormation.optok_some. exact (PublicFormation.attached_this_ok W Cf r args (proj1 Fc) Reqs). }
        destruct (PublicFormation.fn_body_formed W att Cf ns (Some (TNom r args)) g0 (map fst ps) mentions_typed Ctx Sig Ot)
          as [Sg FBf].
        split; [exact FBf|]. split; [split; [rewrite Ret; apply PublicFormation.optok_some; exact (proj2 Sg)|rewrite FThis; exact Ot]|].
        apply (PublicScoping.fn_body_cxscoped W att Cf ns F0 (Some (TNom r args)) g0 (map fst ps) Ctx Sig FThis).
        intros t E. injection E as <-. exact (PublicScoping.attached_this_scoped W Cf r args Reqs). }
  destruct Sj as [_ [_ [SBlkE _]]].
  pose proof (SBlkE F0 Cb b (Some (A.sg_ret g0)) x t0 Body de types0 H (parameter_values (w0 :: ws)) st Pre0) as Out.
  destruct (blke_branch_ty _ _ _ _ _ _ Body) as [_ Fits]. cbn in Fits.
  destruct (AR.O.I.eval j P de (parameter_values (w0 :: ws)) st x) as [[] g' st']; cbn in Out |- *; auto.
  - destruct Out as [H' [E [S [_ T]]]]. exists H'; repeat split; auto.
    rewrite <- Result. exact (fits_ground W P H' de _ _ _ Fits T).
  - destruct Out as [H' [r' [R [E [S [_ T]]]]]]. rewrite Ret in R. injection R as <-.
    exists H'; repeat split; auto. rewrite <- Result. exact T.
  - destruct Out as [_ [L _]]. rewrite Loop in L. discriminate.
Qed.


(* ---- direct calls of defined functions -------------------------------------- *)

(* The callee's environment: its formals read at the call, and the
   provisions the satisfaction map selects. *)
Definition callee_env (d : environment) (th : subst) (dc : environment) : environment :=
  EnvironmentWithAliases (map (fun q => (fst q, ground d (snd q))) th)
    (map (fun q => (fst q, ground d (snd q))) th) (provision_values dc).

Lemma keys_meant_map : forall (C : A.cx) (f : ty -> ty) (th : subst), map fst th = map fst (A.c_ty C) ->
  forall k v, A.nlook k (A.c_ty C) = Some v -> lookup_subst k (map (fun q => (fst q, f (snd q))) th) <> None.
Proof.
  intros C f th Keys k v L. apply lookup_subst_in.
  assert (E : map fst (map (fun q => (fst q, f (snd q))) th) = map fst th) by (rewrite map_map; apply map_ext; reflexivity).
  rewrite E, Keys.
  change k with (fst (k, v)). apply in_map. exact (nlook_in _ _ _ _ L).
Qed.

Lemma keys_meant_callee : forall C d th dc, map fst th = map fst (A.c_ty C) -> KeysMeant C (callee_env d th dc).
Proof. intros C d th dc Keys. exact (keys_meant_map C (ground d) th Keys). Qed.

Lemma ilook_in : forall k (provs : list (item * A.provision)) p, A.ilook k provs = Some p -> In (k, p) provs.
Proof.
  intros k provs p. induction provs as [|[k' p'] rest IH]; intros L; [discriminate|].
  cbn in L. destruct (item_eqb k k') eqn:E; [apply item_eqb_spec in E; subst k'; injection L as ->; left; reflexivity|].
  right. exact (IH L).
Qed.

Lemma forall2_map_eq : forall H (f g : ty -> ty) ws (ps ps' : list ty),
  map f ps = map g ps' -> Forall2 (fun w t => value_typed W P H w (g t)) ws ps' ->
  Forall2 (fun w t => value_typed W P H w (f t)) ws ps.
Proof.
  intros H f g ws ps ps' E F. revert ps E. induction F as [|w t ws ts T Rest IH]; intros [|p ps] E; cbn in E;
    try discriminate; constructor.
  - injection E as E1 _. rewrite E1. exact T.
  - injection E as _ E2. exact (IH ps E2).
Qed.

Lemma callable_transfer : forall n m H0 d d' ps ps' r r' pv, m <= n ->
  map (ground d) ps = map (ground d') ps' -> ground d r = ground d' r' ->
  CallableOk n H0 d ps r pv -> CallableOk m H0 d' ps' r' pv.
Proof.
  intros n m H0 d d' ps ps' r r' [w|f df|u sms dc|p held] Le Ps R Ok; cbn in Ok |- *; auto.
  - destruct Ok as [body [View Run]]. exists body. split; [exact View|].
    intros j Lj H st ws E S Args. rewrite <- R. apply (Run j ltac:(lia) H st ws E S).
    exact (forall2_map_eq H (ground d) (ground d') ws ps ps' Ps Args).
  - intros H st ws E S Args. rewrite <- R. apply (Ok H st ws E S).
    exact (forall2_map_eq H (ground d) (ground d') ws ps ps' Ps Args).
Qed.

Lemma ilook_apps_fun : forall C u fx, A.ilook (IFun u) (A.c_provisions C) = Some (A.ProvidedFunctor fx) ->
  A.ilook (IFun u) (A.c_apps C) = Some (A.fixed_app fx).
Proof.
  intros [ty sub classes provs locs] u fx. unfold A.c_apps. cbn. induction provs as [|[k p] rest IH]; intros L; [discriminate|].
  destruct k as [a|a|a|r m a|a]; cbn in L |- *; try exact (IH L).
  destruct (Nat.eqb u a); [injection L as ->; reflexivity|exact (IH L)].
Qed.

Lemma fixed_app_in : forall fx x t, In (x, t) fx -> In (x, Some t) (A.fixed_app fx).
Proof. intros fx x t I. unfold A.fixed_app. apply in_map_iff. exists (x, t). split; [reflexivity|exact I]. Qed.

Lemma fixed_app_out : forall fx x t, In (x, Some t) (A.fixed_app fx) -> In (x, t) fx.
Proof.
  intros fx x t I. unfold A.fixed_app in I. apply in_map_iff in I as [[y v] [E I]]. cbn in E. injection E as -> ->. exact I.
Qed.

(* What a call reads of the callee's context. *)
Definition CtxStatic (Cf : A.cx) (ns : list item) : Prop :=
  TableFormed Cf /\ StaticOk Cf /\
  NoDup ns /\ (forall k p, In (k, p) (A.c_provisions Cf) -> In k ns) /\ (forall k, In k ns -> NeedShape k) /\
  (forall k p, In (k, p) (A.c_provisions Cf) ->
     incl (provision_sig_gens p) (map fst (A.c_ty Cf)) /\ incl (key_gens k) (map fst (A.c_ty Cf))) /\
  (forall u fx, In (IFun u, A.ProvidedFunctor fx) (A.c_provisions Cf) ->
     (forall x, A.Fixed W u x -> exists t, In (x, t) fx) /\ (forall x t, In (x, t) fx -> incl (A.gens t) (map fst (A.c_ty Cf)))).

Lemma ctxof_static : forall f Cf ns, A.CtxOf W f Cf ns -> CtxStatic Cf ns.
Proof.
  intros f Cf ns Ctx. destruct (ctx_needs W f Cf ns Ctx) as [Nd [ProvNs Shape]].
  split; [exact (ctx_table_formed W f Cf ns Ctx)|].
  split; [split; [exact (proj2 (function_context_merge W f Cf ns Ctx)) | exact (ctx_keys_match W f Cf ns Ctx)] |].
  split; [exact Nd|]. split; [exact ProvNs|]. split; [exact Shape|].
  split; [exact (ctx_provision_scoped W f Cf ns Ctx)|].
  intros u fx I. destruct (ctx_fun_fixed W f Cf ns Ctx u fx I) as [Cov [Sc _]]. split; assumption.
Qed.

Lemma functx_static : forall u Cf ns, A.FunCtx W u Cf ns -> CtxStatic Cf ns.
Proof.
  intros u Cf ns FC. destruct (functx_scoped W u Cf ns FC) as [Can [Sc [KM [Nd [ProvNs [Shape Dom]]]]]].
  split; [split; [exact Can|exact Dom]|].
  split; [exact (conj (proj2 (functor_context_merge W u Cf ns FC)) KM)|].
  split; [exact Nd|]. split; [exact ProvNs|]. split; [exact Shape|].
  split; [intros k p I; destruct (Sc k p I) as [S1 [S2 _]]; split; assumption|].
  intros u' fx I. split.
  - exact (functx_fun_cover W u Cf ns FC u' fx I).
  - intros x t It. exact (proj2 (proj2 (Sc _ _ I)) x t It).
Qed.

(* T03/T04: a call's selected provisions realize the callee's context. *)
Lemma call_absorbs_merge : forall C Cf th d dc,
  TableFormed Cf -> CxSection Cf -> map fst th = map fst (A.c_ty Cf) ->
  A.Respects (A.c_merge Cf) th (A.c_merge C) -> Absorbs C d ->
  Absorbs Cf (callee_env d th dc).
Proof.
  intros C Cf th d dc [_ Support] Section Keys Respect Abs.
  apply (absorbs_canon_iff Cf _ Section). intro t.
  change (app_subst (callee_meanings d th) (app_subst (A.c_sub Cf) t) =
    app_subst (callee_meanings d th) t).
  rewrite <- (app_subst_nil t) at 2. apply subst2_agree. intros x _. rewrite app_subst_nil.
  change (app_subst (callee_meanings d th)
    (match lookup_subst x (A.c_sub Cf) with Some u => u | None => TAbs x end) =
    app_subst (callee_meanings d th) (TAbs x)).
  destruct (lookup_subst x (A.c_sub Cf)) as [u |] eqn:Lookup; [|reflexivity].
  destruct (Support x u (lookup_subst_in_pair _ _ _ Lookup)) as [Domain Range].
  rewrite (callee_ground d th u), (callee_ground d th (TAbs x)).
  - apply Abs, PublicMerge.merge_eq_substitute with (M := A.c_merge Cf).
    + exact Respect.
    + apply A.MESym. pose proof (proj1 Section x) as Read.
      cbn [app_subst] in Read. now rewrite Lookup in Read.
  - intros y [<- | []]. apply lookup_subst_in. now rewrite Keys.
  - intros y Y. apply lookup_subst_in. rewrite Keys. exact (Range y Y).
Qed.

Lemma static_realizes_le : forall n0 n C' d H f Cf ns sm th, n <= n0 ->
  Realizes n0 C' d H -> CtxStatic Cf ns -> A.Satisfies C' Cf th ns sm -> A.CallTheta W C' Cf f th ->
  exists dc, select_provisions d sm = Some dc /\
    Agree (type_meanings (callee_env d th dc)) (type_meanings d) (A.c_ty Cf) (A.c_ty C') /\
    Realizes n Cf (callee_env d th dc) H.
Proof.
  intros n0 n C d H f Cf ns sm th Le [AbsM [[Vals [Fns [Mths Funs]]] [Section KM]]] St Sat CT.
  destruct St as [TfC [[SectionC KMf] [NdC [ProvC [ShapeC [ScC FixC]]]]]].
  pose proof (proj1 (absorbs_canon_iff C d Section) AbsM) as Abs.

  destruct Sat as [SmEq [Prov [Coh [Types _]]]].
  pose proof NdC as Nd. pose proof ProvC as ProvNs. pose proof ShapeC as Shape.
  pose proof CT as [Keys [_ Respects]].
  (* the caller's provision of each need *)
  assert (Caller : forall k, In k ns -> exists pv, lookup_provision (A.rkey_of C (A.at_th C th k)) d = Some pv).
  { intros k Ik. pose proof (Prov k Ik) as Pr. pose proof (Shape k Ik) as Sh.
    destruct k as [t|v|f'|r m a|u]; cbn in Sh, Pr |- *; try contradiction.
    - destruct (A.nlook v (A.c_vals C)) as [t|] eqn:L; [|congruence]. destruct (Vals v t L) as [w [Look _]]. eauto.
    - destruct (A.nlook f' (A.c_fns C)) as [g|] eqn:L; [|congruence]. destruct (Fns f' g L) as [pv [Look _]]. eauto.
    - destruct (A.mlook _ m (A.c_mths C)) as [g|] eqn:L; [|congruence]. destruct (Mths _ m g L) as [pv [Look _]]. eauto.
    - destruct (A.nlook u (A.c_funs C)) as [fx|] eqn:L; [|congruence]. destruct (Funs u fx L) as [pv [gfx [Look _]]]. eauto. }
  destruct (select_ok d sm) as [dc Sel].
  { rewrite SmEq. intros q Iq. apply in_map_iff in Iq as [k [<- Ik]]. cbn. destruct (Caller k Ik) as [pv ->]. discriminate. }
  exists dc. split; [exact Sel|].
  set (de := callee_env d th dc).
  assert (Meanings : type_meanings de = callee_meanings d th) by reflexivity.
  (* each callee key finds the caller's provision *)
  assert (Find : forall k, In k ns -> lookup_provision (A.rkey_of Cf k) de = lookup_provision (A.rkey_of C (A.at_th C th k)) d).
  { intros k Ik. change (lookup_provision (A.rkey_of Cf k) de) with (lookup_provision (A.rkey_of Cf k) dc).
    rewrite (select_lookup d sm dc _ Sel), SmEq. unfold A.satisfaction_map.
    rewrite (sm_find_map (A.rkey_of Cf) (fun k' => A.rkey_of C (A.at_th C th k')) ns k Ik); [reflexivity|].
    intros k' Ik' E. exact (rkey_of_injective Cf k' k (Shape k' Ik') (Shape k Ik) E). }
  assert (Dom : forall t, incl (A.gens t) (map fst (A.c_ty Cf)) -> forall x, In x (A.gens t) -> lookup_subst x th <> None).
  { intros t Sc x Ix. apply lookup_subst_in. rewrite Keys. exact (Sc x Ix). }
  assert (GroundDe : forall t, incl (A.gens t) (map fst (A.c_ty Cf)) -> ground de t = ground d (A.canon C (app_subst th t))).
  { intros t Sc. unfold ground. rewrite Meanings, (callee_ground d th t (Dom t Sc)). symmetry. apply Abs. }
  assert (SigGround : forall gf gc, incl (A.sig_gens gf) (map fst (A.c_ty Cf)) ->
    A.canon_sig C gc = A.sig_at C th gf ->
    map (ground d) (A.sg_params gc) = map (ground de) (A.sg_params gf) /\ ground d (A.sg_ret gc) = ground de (A.sg_ret gf)).
  { intros [psf rf] [psc rc] Sc E. unfold A.canon_sig, A.sig_at in E. cbn [A.sg_params A.sg_ret] in E |- *. injection E as Ep Er.
    unfold A.sig_gens in Sc. cbn [A.sg_params A.sg_ret flat_map] in Sc. split.
    - transitivity (map (fun t => ground d (A.canon C t)) psc); [apply map_ext; intros t; symmetry; apply Abs|].
      rewrite <- map_map, Ep, map_map. apply map_ext_in. intros t It. symmetry. apply GroundDe.
      intros x Ix. apply Sc. apply in_or_app. right. apply in_flat_map. exists t. split; assumption.
    - rewrite <- Abs, Er. symmetry. apply GroundDe. intros x Ix. apply Sc. apply in_or_app. left. exact Ix. }
  split; [exact (call_agree_tf W f Cf C th d TfC SectionC CT Section AbsM)|].
  split; [exact (call_absorbs_merge C Cf th d dc TfC SectionC Keys Respects AbsM)|].
  split; [|exact (conj SectionC KMf)].
  split; [|split; [|split]].
  - (* vals *) intros v tf Found.
    destruct (vals_ilook _ v tf KMf Found) as [a Il]. pose proof (ilook_in _ _ _ Il) as InP.
    pose proof (ProvNs _ _ InP) as Ik.
    destruct (Types (IVal v) _ Ik Il) as [p' [Il' Sig]]. cbn in Il'.
    pose proof (KM _ _ (ilook_in _ _ _ Il')) as Kind.
    destruct p' as [tc a'|g a'|g a'|fx]; cbn in Kind; try contradiction.
    pose proof (ilook_vals _ v tc a' KM Il') as Lc. destruct (Vals v tc Lc) as [w [Look T]].
    exists w. pose proof (Find (IVal v) Ik) as F'; cbn [A.rkey_of A.at_th A.mkey] in F'; rewrite F'. split; [exact Look|].
    assert (Sig' : A.sg_ret (A.canon_sig C (A.FSig [] tc)) = A.sg_ret (A.sig_at C th (A.FSig [] tf)))
      by (cbn [option_map A.provision_signature] in Sig; congruence).
    change (A.canon C tc = A.canon C (app_subst th tf)) in Sig'. clear Sig. rename Sig' into Sig.
    destruct (ScC _ _ InP) as [Sc _]. unfold provision_sig_gens, A.sig_gens in Sc. cbn in Sc.
    rewrite GroundDe; [|intros x Ix; apply Sc; rewrite app_nil_r; exact Ix].
    rewrite <- Sig, Abs. exact T.
  - (* fns *) intros f' gf Found.
    destruct (fns_ilook _ f' gf KMf Found) as [a Il]. pose proof (ilook_in _ _ _ Il) as InP.
    pose proof (ProvNs _ _ InP) as Ik.
    destruct (Types (IFn f') _ Ik Il) as [p' [Il' Sig]]. cbn in Il'.
    pose proof (KM _ _ (ilook_in _ _ _ Il')) as Kind.
    destruct p' as [tc a'|gc a'|gc a'|fx]; cbn in Kind; try contradiction.
    pose proof (ilook_fns _ f' gc a' KM Il') as Lc. destruct (Fns f' gc Lc) as [pv [Look Ok]].
    exists pv. pose proof (Find (IFn f') Ik) as F'; cbn [A.rkey_of A.at_th A.mkey] in F'; rewrite F'. split; [exact Look|].
    assert (Sig' : A.canon_sig C gc = A.sig_at C th gf) by (cbn [option_map A.provision_signature] in Sig; congruence).
    clear Sig. rename Sig' into Sig.
    destruct (ScC _ _ InP) as [Sc _]. unfold provision_sig_gens in Sc. cbn in Sc.
    destruct (SigGround gf gc Sc Sig) as [Ps R].
    exact (callable_transfer n0 n H d de _ _ _ _ pv Le Ps R Ok).
  - (* methods *) intros r m gf Found.
    destruct (mths_ilook _ r m gf KMf Found) as [a Il]. pose proof (ilook_in _ _ _ Il) as InP.
    pose proof (ProvNs _ _ InP) as Ik.
    destruct (Types (A.mkey r m) _ Ik Il) as [p' [Il' Sig]]. cbn in Il'.
    pose proof (KM _ _ (ilook_in _ _ _ Il')) as Kind.
    destruct p' as [tc a'|gc a'|gc a'|fx]; cbn in Kind; try contradiction.
    pose proof (ilook_mths _ _ m gc a' KM Il') as Lc. destruct (Mths _ m gc Lc) as [pv [Look Ok]].
    exists pv. pose proof (Find (A.mkey r m) Ik) as F'; cbn [A.rkey_of A.at_th A.mkey] in F'; rewrite F'. split; [exact Look|].
    assert (Sig' : A.canon_sig C gc = A.sig_at C th gf) by (cbn [option_map A.provision_signature] in Sig; congruence).
    clear Sig. rename Sig' into Sig.
    destruct (ScC _ _ InP) as [Sc Kr]. unfold provision_sig_gens in Sc. cbn in Sc, Kr.
    destruct (SigGround gf gc Sc Sig) as [Ps R].
    apply (callable_transfer n0 n H d de (A.canon C (app_subst th r) :: A.sg_params gc) (r :: A.sg_params gf)
      (A.sg_ret gc) (A.sg_ret gf) pv Le); [|exact R|exact Ok].
    change (ground d (A.canon C (app_subst th r)) :: map (ground d) (A.sg_params gc) =
      ground de r :: map (ground de) (A.sg_params gf)).
    rewrite Ps, (GroundDe r Kr). reflexivity.
  - (* functors *) intros u fx Found.
    pose proof (funs_ilook _ u fx KMf Found) as Il. pose proof (ilook_in _ _ _ Il) as InP.
    pose proof (ProvNs _ _ InP) as Ik.
    pose proof (Prov (IFun u) Ik) as Pr. cbn in Pr.
    destruct (A.nlook u (A.c_funs C)) as [fx'|] eqn:L; [|congruence].
    destruct (Funs u fx' L) as [pv [gfx [Look' [Ok Fm]]]]. exists pv, gfx.
    pose proof (Find (IFun u) Ik) as F'; cbn [A.rkey_of A.at_th] in F'; rewrite F'. split; [exact Look'|].
    split; [exact (proj1 (closure_lower n0 n H H Le (fun a c L => L)) _ _ _ Ok)|].
    destruct (FixC u fx InP) as [Cov Sc]. split; [exact Cov|].
    intros x tf Ixt.
    assert (Ia : A.ilook (IFun u) (A.c_apps Cf) = Some (A.fixed_app fx)) by (apply ilook_apps_fun; exact Il).
    assert (Ic : A.ilook (IFun u) (A.c_apps C) = Some (A.fixed_app fx')) by (apply ilook_apps_fun; exact (funs_ilook _ u fx' KM L)).
    pose proof (Coh (IFun u) _ Ik Ia x tf (fixed_app_in _ _ _ Ixt)) as Have. cbn [A.at_th] in Have. rewrite Ic in Have.
    apply fixed_app_out in Have. destruct Fm as [_ Vm]. rewrite (Vm x _ Have).
    f_equal. symmetry. apply GroundDe. exact (Sc x tf Ixt).
Qed.

Lemma callee_realizes_le : forall n0 n C' d H f Cf ns sm th, n <= n0 ->
  Realizes n0 C' d H -> A.CtxOf W f Cf ns -> A.Satisfies C' Cf th ns sm -> A.CallTheta W C' Cf f th ->
  exists dc, select_provisions d sm = Some dc /\
    Agree (type_meanings (callee_env d th dc)) (type_meanings d) (A.c_ty Cf) (A.c_ty C') /\
    Realizes n Cf (callee_env d th dc) H.
Proof.
  intros n0 n C d H f Cf ns sm th Le Real Ctx. exact (static_realizes_le n0 n C d H f Cf ns sm th Le Real (ctxof_static f Cf ns Ctx)).
Qed.

Lemma callee_realizes : forall n C' d H f Cf ns sm th,
  Realizes (S n) C' d H -> A.CtxOf W f Cf ns -> A.Satisfies C' Cf th ns sm -> A.CallTheta W C' Cf f th ->
  exists dc, select_provisions d sm = Some dc /\
    Agree (type_meanings (callee_env d th dc)) (type_meanings d) (A.c_ty Cf) (A.c_ty C') /\
    Realizes n Cf (callee_env d th dc) H.
Proof. intros n C' d H f Cf ns sm th. exact (callee_realizes_le (S n) n C' d H f Cf ns sm th (le_S _ _ (le_n n))). Qed.

Lemma select_provisions_empty : forall d sm dc, select_provisions d sm = Some dc ->
  type_meanings dc = [] /\ type_aliases dc = [].
Proof.
  intros d sm. induction sm as [|[req s] rest IH]; intros dc Sel; cbn in Sel.
  - injection Sel as <-. split; reflexivity.
  - destruct (lookup_provision s d); [|discriminate].
    destruct (select_provisions d rest) as [d'|] eqn:Rest; [|discriminate].
    injection Sel as <-. exact (IH d' eq_refl).
Qed.

Lemma call_environment_callee : forall d th sm dc, select_provisions d sm = Some dc ->
  call_environment d th dc = callee_env d th dc.
Proof.
  intros d th sm dc Sel. destruct (select_provisions_empty d sm dc Sel) as [M Al].
  unfold call_environment, append_environment, instantiate, callee_env.
  destruct dc as [m al ps]; cbn in *. subst m al. rewrite !app_nil_r. reflexivity.
Qed.

(* The callee is read in C (after any demand), the arguments in CA (Q4:
   the caller's own context), both realized at the start. *)
Lemma call_core : forall j, Below j -> forall F C CA args f n0 ps0 rt0 b0 Cf ns sm th g xs (B : Prop),
  A.ast_of W f = Some (ADFn RPlain n0 ps0 rt0 (Some b0)) ->
  A.CtxOf W f Cf ns -> A.Satisfies C Cf th ns sm -> A.CallTheta W C Cf f th -> A.SigOf W f (A.c_ty C) None g ->
  LegacyBodyJudgments.Args W Prog F CA args (A.sg_params g) xs -> (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) ->
  forall d types H lg lst, Pre (S j) F C d types H lg lst -> Pre (S j) F CA d types H lg lst ->
  Outcome F d H types (A.sg_ret g) B (AR.O.I.eval (S j) P d lg lst (ECall f th sm xs)).
Proof.
  intros j Bel F C CA args f n0 ps0 rt0 b0 Cf ns sm th g xs B Ast Ctx Sat CT Sig Args Brk d types H lg lst Pre0 PreA.
  unfold A.ast_of in Ast. destruct (A.w_decl W f) as [dcl|] eqn:Decl; [|discriminate]. injection Ast as Ast.
  pose proof (all_fn_elab f dcl RPlain n0 ps0 rt0 b0 Decl Ast dcl RPlain n0 ps0 rt0 b0 Decl Ast)
    as (C0 & ns0 & this & g0 & F0 & x & t0 & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & View).
  pose proof (args_code j j (le_n j) F CA B xs (A.sg_params g) d types H lg lst
    (args_code_of j Bel F CA args _ xs Args B Brk) (pre_lower PreA)) as AOut.
  cbn [AR.O.I.eval]. change (view_function P f) with (A.body_of Prog f). rewrite View.
  destruct (AR.O.I.eval_arguments j P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
  2: { destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out]. }
  destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
  pose proof Pre0 as [_ [_ [_ [_ [Real _]]]]].
  destruct (callee_realizes j C d H1 f Cf ns sm th (realizes_extends _ _ _ _ _ E1 Real) Ctx Sat CT)
    as [dc [Sel [Ag RealF]]].
  rewrite Sel, (call_environment_callee d _ sm dc Sel).
  destruct (body_call j (Bel j (le_n j)) f dcl n0 ps0 rt0 b0 Cf ns _ H1 st1 ws d (A.sg_params g) (A.sg_ret g)
    Decl Ast Ctx RealF S1 eq_refl (keys_meant_callee Cf d th dc (proj1 CT))) as [body [View' Run]].
  - intros g' Sig'. exact (sig_rel W _ _ f _ _ None None g' g Sig' Sig Ag (fun _ _ E _ => ltac:(discriminate E))).
  - exact T1.
  - change (view_function P f) with (A.body_of Prog f) in View'. rewrite View in View'. injection View' as <-.
    exact (outcome_ret_call F d H H1 types _ _ _ g1 E1 L1 Run).
Qed.

(* Family applications a call demands wrap it in functor applications (T15/T16). *)
(* Code inside the wrapper may rely on both the demanding context and the
   context after the families: an attached call's receiver and arguments are
   elaborated before the families it demands. With no family the wrapper is
   the identity, so the inner code runs at the same fuel. *)

(* A family application keeps the context's provisions and merge. *)
Lemma family_cr : forall C u matching th sm Cr ks, AuditFacts.FamilyApplication W C u matching th sm Cr ks ->
  incl (A.c_provisions C) (A.c_provisions Cr) /\ A.c_sub Cr = A.c_sub C.
Proof.
  intros C u matching th sm Cr ks (formal & needs & (fixed & _ & _ & _ & _ & _ & _ & _ & _ & PR & _) & _).
  destruct PR as (d0 & ps & a & r & b & outs & es & tm' & _ & _ & _ & _ & _ & ->).
  destruct (PublicFormation.fold_set_ty_shape matching C) as [_ [Esub [Epr _]]].
  unfold A.output_cx, A.family_context in *. cbn [A.c_provisions A.c_sub]. split.
  - intros q I. apply in_or_app. right. rewrite Epr. exact I.
  - exact Esub.
Qed.

(* A demand's family applications, one after another, run the code after
   them with the families' provisions and the code's original context. *)
Lemma demand_gen : forall k, Below k -> forall D0 wanted used C wrap C', A.Demand W D0 wanted used C wrap C' ->
  forall F Cc Corig t (B : Prop) x,
  A.c_sub C = A.c_sub Cc -> A.c_merge C = A.c_merge Cc ->
  A.c_provisions C = A.c_provisions Cc -> A.c_locals C = A.c_locals Cc ->
  incl (A.c_provisions Corig) (A.c_provisions Cc) -> A.c_sub Corig = A.c_sub Cc ->
  FamilyTableIn C Cc -> PublicFormation.TysScoped W C ->
  (forall r m, In (A.mkey r m) wanted -> PublicFormation.TyOk W r /\ PublicFormation.ScopedTy W Cc r) ->
  (forall j, j <= S k -> forall d types H g st, Pre j F (with_ty Cc C') d types H g st -> Pre j F Corig d types H g st ->
     Outcome F d H types t B (AR.O.I.eval j P d g st x)) ->
  forall n, n <= S k -> forall d types H g st, Pre n F Cc d types H g st -> Pre n F Corig d types H g st ->
  Outcome F d H types t B (AR.O.I.eval n P d g st (A.translate_demands C wrap x)).
Proof.
  intros k Bel D0 wanted used C wrap C' Dm. induction Dm as [used C|used C u matching th formal needs Cr ks rest Cf Nin Step Rest IH];
    intros F Cc Corig t B x Es Emg Ep El Inc Eo Tin Ts Wk Cont n Ln d types H g st PreC PreO.
  - apply Cont; [exact Ln| |exact PreO].
    replace (with_ty Cc C) with Cc; [exact PreC|]. destruct Cc as [a0 b0 m0 c0 e0], C as [a1 b1 m1 c1 e1]; cbn in *; subst; reflexivity.
  - inversion Step as [C1 ot m u1 matching1 th1 formal1 needs1 Cr1 ks1 Iw Ml Of Typed Ro Ik Matched]; subst.
    pose (sm := A.satisfaction_map (A.family_context C matching) formal th needs).
    pose proof (AuditFacts.family_application_with_map Typed) as FA.
    change (Outcome F d H types t B (AR.O.I.eval n P d g st
      (EBind (BApply u th [] sm (map (A.rkey_of Cr) ks))
        (A.translate_demands (A.family_restore C Cr) rest x)))).
    destruct n as [|n1]; [exact Logic.I|]. rewrite eval_bind_eq0.
    destruct n1 as [|n2]; [cbn; exact Logic.I|].
    destruct (Wk ot m Iw) as [Ok Sc].
    destruct (family_step k Bel Cc C u matching th sm Cr ks ot m Es Emg Ep El FA Ro Ok Sc Tin Ts F (S (S n2)) n2 d types H g st Ln ltac:(lia) PreC [])
      as [[d' [st' [H' [E [Em [Ea [PreN [Keep Run]]]]]]]]|[res [Run [NN Out]]]].
    + rewrite Run. destruct n2 as [|n3]; [cbn; exact Logic.I|].
      assert (Enil : AR.O.I.eval_bindings (S n3) P d' g st' [] = inl (d', g, st')) by reflexivity. rewrite Enil.
      pose proof PreN as [_ [_ [_ [StN _]]]]. pose proof PreO as [_ [_ [_ [_ [RealO FmO]]]]].
      assert (Coh : FamilyCoherent Corig Cr ks).
      { apply (family_coherent F Corig FmO C u matching th sm Cr ks); [rewrite Es; symmetry; exact Eo| |exact FA].
        intros q I. rewrite Ep. exact (Inc q I). }
      pose proof (pre_keep _ _ _ _ _ _ _ _ _ _ _ PreO Em Ea (Keep Corig Inc Eo Coh RealO) E StN) as PreO'.
      destruct (family_cr _ _ _ _ _ _ _ FA) as [IncCr SubCr].
      assert (ScE : PublicFormation.scope_gens (with_ty Cc Cr) = PublicFormation.scope_gens Cc)
        by (apply PublicFormation.scope_gens_ext; [reflexivity|cbn; rewrite SubCr, Es; reflexivity]).
      eapply outcome_heap; [exact E|]. apply (outcome_meanings F d d' H' types t B _ Em).
      apply (IH F (with_ty Cc Cr) Corig t B x eq_refl eq_refl eq_refl eq_refl).
      * intros q I. apply IncCr. rewrite Ep. exact (Inc q I).
      * cbn. rewrite SubCr, Es. exact Eo.
      * intros x0 v Iq. cbn in Iq.
        replace (A.canon (A.family_restore C Cr) v) with (A.canon C v) by (unfold A.canon; cbn; rewrite SubCr; reflexivity).
        destruct (Tin x0 v Iq) as [O1 S1]. exact (conj O1 (PublicFormation.scoped_ext W _ _ _ (eq_sym ScE) S1)).
      * exact Ts.
      * intros r0 m0 I0. destruct (Wk r0 m0 I0) as [O1 S1]. exact (conj O1 (PublicFormation.scoped_ext W _ _ _ (eq_sym ScE) S1)).
      * exact Cont.
      * lia.
      * exact (pre_lower PreN).
      * exact (pre_lower PreO').
    + rewrite Run. eapply outcome_pass; [|exact NN|exact Out]. intros [].
Qed.

Lemma demand_case : forall k, Below k -> forall F C0 C wanted C' wrap t (B : Prop) x,
  A.c_sub C = A.c_sub C0 -> A.c_merge C = A.c_merge C0 ->
  A.c_provisions C = A.c_provisions C0 -> A.c_locals C = A.c_locals C0 ->
  A.Demand W C wanted [] C wrap C' ->
  (Formed F C0 -> FamilyTableIn C C0 /\ PublicFormation.TysScoped W C /\
     forall r m, In (A.mkey r m) wanted -> PublicFormation.TyOk W r /\ PublicFormation.ScopedTy W C0 r) ->
  (forall j, j <= S k -> forall d types H g st, Pre j F (with_ty C0 C') d types H g st -> Pre j F C0 d types H g st ->
     Outcome F d H types t B (AR.O.I.eval j P d g st x)) ->
  StepSafe k F C0 t B (A.translate_demands C wrap x).
Proof.
  intros k Bel F C0 C wanted C' wrap t B x Es Emg Ep El Dm Sco Cont d types H g st Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [_ Fm0]]]]]. destruct (Sco Fm0) as [Tin [Ts Wk]].
  exact (demand_gen k Bel C wanted [] C wrap C' Dm F C0 C0 t B x Es Emg Ep El (fun q I => I) eq_refl Tin Ts Wk Cont (S k) (le_n _)
    d types H g st Pre0 Pre0).
Qed.

Lemma call_code : forall k, Below k -> forall F C CA args f n0 ps0 rt0 b0 Cf ns sm th g xs (B : Prop),
  A.ast_of W f = Some (ADFn RPlain n0 ps0 rt0 (Some b0)) ->
  A.CtxOf W f Cf ns -> A.Satisfies C Cf th ns sm -> A.CallTheta W C Cf f th -> A.SigOf W f (A.c_ty C) None g ->
  LegacyBodyJudgments.Args W Prog F CA args (A.sg_params g) xs -> (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) ->
  forall j, j <= S k -> forall d types H lg lst, Pre j F C d types H lg lst -> Pre j F CA d types H lg lst ->
  Outcome F d H types (A.sg_ret g) B (AR.O.I.eval j P d lg lst (ECall f th sm xs)).
Proof.
  intros k Bel F C CA args f n0 ps0 rt0 b0 Cf ns sm th g xs B Ast Ctx Sat CT Sig Args Brk [|j] Le d types H lg lst Pre0 PreA;
    [exact Logic.I|].
  exact (call_core j (fun i Li => Bel i ltac:(lia)) F C CA args f n0 ps0 rt0 b0 Cf ns sm th g xs B
    Ast Ctx Sat CT Sig Args Brk d types H lg lst Pre0 PreA).
Qed.

(* ---- adding a provision -------------------------------------------------------- *)

Definition NewOk n C' d' H (k0 : item) (p0 : A.provision) : Prop :=
  match k0, p0 with
  | IVal v, A.ProvidedVal t _ => exists w, lookup_provision (IVal v) d' = Some (ValueProvision w) /\ value_typed W P H w (ground d' t)
  | IFn f, A.ProvidedFn g _ => exists pv, lookup_provision (IFn f) d' = Some pv /\ CallableOk n H d' (A.sg_params g) (A.sg_ret g) pv
  | IMth r m _, A.ProvidedMethod g _ => exists pv, lookup_provision (A.rkey C' r m) d' = Some pv /\ CallableOk n H d' (r :: A.sg_params g) (A.sg_ret g) pv
  | IFun u, A.ProvidedFunctor fx => exists pv gfx, lookup_provision (IFun u) d' = Some pv /\ ClosureOk n H u gfx pv /\ FixedMeans u d' fx gfx
  | _, _ => False
  end.

Definition set_cx (C : A.cx) (k0 : item) (p0 : A.provision) : A.cx :=
  A.Cx (A.c_ty C) (A.c_sub C) (A.c_merge C) (set_list k0 p0 (A.c_provisions C)) (A.c_locals C).

Lemma ilook_map_snd : forall X (f : A.provision -> X) k l,
  A.ilook k (map (fun q => (fst q, f (snd q))) l) = option_map f (A.ilook k l).
Proof. intros X f k l. induction l as [|[k' p] r IH]; [reflexivity|]. cbn. destruct (item_eqb k k'); [reflexivity|exact IH]. Qed.

Lemma capps_ilook : forall C k, A.ilook k (A.c_apps C) = option_map A.provision_inst (A.ilook k (A.c_provisions C)).
Proof. exact PublicContextFacts.capps_ilook. Qed.

Lemma rkey_set : forall C k0 p0 r m, item_eqb (A.mkey r m) k0 = false ->
  A.rkey (set_cx C k0 p0) r m = A.rkey C r m.
Proof.
  intros C k0 p0 r m Ne. unfold A.rkey. rewrite !capps_ilook.
  unfold set_cx. cbn [A.c_provisions]. rewrite ilook_set, Ne. reflexivity.
Qed.

Lemma rkey_of_kind : forall C k k', A.rkey_of C k = k' ->
  match k' with IVal v => k = IVal v | IFn f => k = IFn f | IFun u => k = IFun u
  | IMth r m _ => exists th, k = IMth r m th | ITy t => k = ITy t end.
Proof. intros C [t|v|f|r m th|u] k' <-; cbn; eauto. Qed.

Lemma realizes_set : forall n C d H k0 p0 d',
  Realizes n C d H -> PublicCallRealize.KeysMatch [(k0, p0)] ->
  type_meanings d' = type_meanings d ->
  (forall k, k <> A.rkey_of (set_cx C k0 p0) k0 -> lookup_provision k d' = lookup_provision k d) ->
  NewOk n (set_cx C k0 p0) d' H k0 p0 ->
  Realizes n (set_cx C k0 p0) d' H.
Proof.
  intros n C d H k0 p0 d' [Abs [[Vals [Fns [Mths Funs]]] [Section KM]]] K0 Meq Other New.
  assert (G : forall t, ground d' t = ground d t) by (intros t; unfold ground; rewrite Meq; reflexivity).
  assert (Gm : forall ps, map (ground d) ps = map (ground d') ps) by (intros ps; apply map_ext; intros; symmetry; apply G).
  pose proof (keys_match_set k0 p0 _ KM K0) as KM'.
  split; [intros t u E; rewrite !G; exact (Abs t u E)|]. split; [|exact (conj Section KM')].
  split; [|split; [|split]].
  - intros v t Found. change (A.c_vals (set_cx C k0 p0)) with (vals_of (set_list k0 p0 (A.c_provisions C))) in Found.
    rewrite vals_by_ilook in Found by exact KM'. rewrite ilook_set in Found.
    destruct (item_eqb (IVal v) k0) eqn:E.
    + apply item_eqb_spec in E. subst k0. destruct p0 as [t' a|g a|g a|fx]; try discriminate. injection Found as <-.
      exact New.
    + change (match A.ilook (IVal v) (A.c_provisions C) with Some (A.ProvidedVal t _) => Some t | _ => None end = Some t) in Found.
      rewrite <- vals_by_ilook in Found by exact KM. destruct (Vals v t Found) as [w [Look T]].
      exists w. rewrite G. split; [|exact T]. rewrite Other; [exact Look|].
      intro E'. symmetry in E'. pose proof (rkey_of_kind _ _ _ E') as Kd. cbn in Kd. subst k0.
      rewrite (proj2 (item_eqb_spec _ _) eq_refl) in E. discriminate.
  - intros f g Found. change (A.c_fns (set_cx C k0 p0)) with (fns_of (set_list k0 p0 (A.c_provisions C))) in Found.
    rewrite fns_by_ilook in Found by exact KM'. rewrite ilook_set in Found.
    destruct (item_eqb (IFn f) k0) eqn:E.
    + apply item_eqb_spec in E. subst k0. destruct p0 as [t' a|g' a|g' a|fx]; try discriminate. injection Found as <-.
      exact New.
    + change (match A.ilook (IFn f) (A.c_provisions C) with Some (A.ProvidedFn g _) => Some g | _ => None end = Some g) in Found.
      rewrite <- fns_by_ilook in Found by exact KM. destruct (Fns f g Found) as [pv [Look Ok]].
      exists pv. split; [rewrite Other; [exact Look|]|].
      * intro E'. symmetry in E'. pose proof (rkey_of_kind _ _ _ E') as Kd. cbn in Kd. subst k0.
        rewrite (proj2 (item_eqb_spec _ _) eq_refl) in E. discriminate.
      * exact (callable_transfer n n H d d' _ _ _ _ pv (le_n n) (Gm _) (eq_sym (G _)) Ok).
  - intros r m g Found. change (A.c_mths (set_cx C k0 p0)) with (mths_of (set_list k0 p0 (A.c_provisions C))) in Found.
    rewrite mths_by_ilook in Found by exact KM'. rewrite ilook_set in Found.
    destruct (item_eqb (A.mkey r m) k0) eqn:E.
    + apply item_eqb_spec in E. subst k0. destruct p0 as [t' a|g' a|g' a|fx]; try discriminate. injection Found as <-.
      exact New.
    + change (match A.ilook (A.mkey r m) (A.c_provisions C) with Some (A.ProvidedMethod g _) => Some g | _ => None end = Some g) in Found.
      rewrite <- mths_by_ilook in Found by exact KM. destruct (Mths r m g Found) as [pv [Look Ok]].
      exists pv. rewrite (rkey_set C k0 p0 r m E). split; [rewrite Other; [exact Look|]|].
      * intro E'. pose proof (rkey_of_kind (set_cx C k0 p0) k0 _ (eq_sym E')) as Kd. cbn in Kd.
        destruct Kd as [th0 ->]. pose proof (K0 _ _ (or_introl eq_refl)) as Kind. cbn in Kind.
        destruct th0; [|destruct p0; contradiction].
        rewrite (proj2 (item_eqb_spec _ _) eq_refl) in E. discriminate.
      * exact (callable_transfer n n H d d' _ _ _ _ pv (le_n n) (Gm _) (eq_sym (G _)) Ok).
  - intros u fx Found. change (A.c_funs (set_cx C k0 p0)) with (funs_of (set_list k0 p0 (A.c_provisions C))) in Found.
    rewrite funs_by_ilook in Found by exact KM'. rewrite ilook_set in Found.
    destruct (item_eqb (IFun u) k0) eqn:E.
    + apply item_eqb_spec in E. subst k0. destruct p0 as [t' a|g a|g a|fx']; try discriminate. injection Found as <-. exact New.
    + change (match A.ilook (IFun u) (A.c_provisions C) with Some (A.ProvidedFunctor fx) => Some fx | _ => None end = Some fx) in Found.
      rewrite <- funs_by_ilook in Found by exact KM. destruct (Funs u fx Found) as [pv [gfx [Look [Ok [Cov Vm]]]]].
      exists pv, gfx. split; [|split; [exact Ok|split; [exact Cov|intros x tx I; rewrite G; exact (Vm x tx I)]]].
      rewrite Other; [exact Look|].
      intro E'. symmetry in E'. pose proof (rkey_of_kind _ _ _ E') as Kd. cbn in Kd. subst k0.
      rewrite (proj2 (item_eqb_spec _ _) eq_refl) in E. discriminate.
Qed.

Lemma filter_idem : forall X (p : X -> bool) l, filter p (filter p l) = filter p l.
Proof.
  intros X p l. induction l as [|x r IH]; [reflexivity|]. cbn.
  destruct (p x) eqn:E; cbn; [rewrite E, IH; reflexivity|exact IH].
Qed.

Lemma provide_set : forall C k p0 a,
  (forall t a', p0 = A.ProvidedVal t a' -> exists v, k = IVal v) ->
  match p0 with A.ProvidedVal t _ => A.provide C k a (A.set_provision C k p0) = set_cx C k (A.ProvidedVal t a)
  | A.ProvidedFn g _ => A.provide C k a (A.set_provision C k p0) = set_cx C k (A.ProvidedFn g a)
  | A.ProvidedMethod g _ => A.provide C k a (A.set_provision C k p0) = set_cx C k (A.ProvidedMethod g a)
  | A.ProvidedFunctor _ => True end.
Proof.
  intros C k p0 a _. pose proof (proj2 (item_eqb_spec k k) eq_refl) as Kk.
  unfold A.provide, A.set_provision, set_cx, set_list. cbn [A.c_provisions A.ilook fst snd]. rewrite Kk.
  destruct p0 as [t a'|g a'|g a'|fx]; auto; cbn [A.c_provisions A.c_ty A.c_sub A.c_locals filter fst];
    rewrite Kk; cbn [negb]; rewrite filter_idem; reflexivity.
Qed.

(* ---- bind statements --------------------------------------------------------- *)

Definition ItemBreaks (F : A.frame) (it : spec * option aexpr) : Prop := A.ItemBreaks W (A.fr_mod F) it.
Definition ItemsBreak (F : A.frame) (its : list (spec * option aexpr)) : Prop :=
  exists it, In it its /\ ItemBreaks F it.

(* A bind's wrapper turns safe continuation code into safe code. *)
Definition WrapSafe n F C F' C' (Bi : Prop) (made : list nat) (w : expr -> expr) : Prop :=
  forall t (B : Prop) x, (forall g, In g made -> ~ In g (A.gens t)) ->
  CodeSafe n F' C' t B x -> CodeSafe n F C t (B \/ Bi) (w x).



Lemma wrap_breaks : forall n F C t (B B' : Prop) x, (B -> B') -> CodeSafe n F C t B x -> CodeSafe n F C t B' x.
Proof. intros n F C t B B' x Imp Safe j Le d types H g st Pre0. exact (AR.O.outcome_breaks W P state_ok _ _ _ _ _ _ _ _ Imp (Safe j Le d types H g st Pre0)). Qed.

Lemma eval_bind_eq : forall j d g st b x,
  AR.O.I.eval (S j) P d g st (EBind b x) =
  match AR.O.I.eval_bindings j P d g st [b] with
  | inl (d1, g1, st1) => AR.O.I.eval j P d1 g1 st1 x
  | inr r => r
  end.
Proof. reflexivity. Qed.

Lemma bindings_nil_eq : forall j d g st, AR.O.I.eval_bindings (S j) P d g st [] = inl (d, g, st).
Proof. reflexivity. Qed.

Lemma bindings_type_eq : forall j d g st t tau r,
  AR.O.I.eval_bindings (S j) P d g st (BType t tau :: r) = AR.O.I.eval_bindings j P (bind_type d t tau) g st r.
Proof. reflexivity. Qed.


Lemma bindings_value_eq : forall j d g st v e r,
  AR.O.I.eval_bindings (S j) P d g st (BValue v e :: r) =
  match AR.O.I.eval j P d g st e with
  | AR.O.I.Evaluation (Normal w1) g1 st1 => AR.O.I.eval_bindings j P (install_provision (IVal v) (ValueProvision w1) d) g1 st1 r
  | rr => inr rr
  end.
Proof. reflexivity. Qed.

Lemma lookup_install : forall k k0 pv d, lookup_provision k (install_provision k0 pv d) =
  if item_eqb k k0 then Some pv else lookup_provision k d.
Proof. reflexivity. Qed.

Lemma install_other : forall k0 pv d k, k <> k0 -> lookup_provision k (install_provision k0 pv d) = lookup_provision k d.
Proof.
  intros k0 pv d k Ne. rewrite lookup_install. destruct (item_eqb k k0) eqn:E; [|reflexivity].
  apply item_eqb_spec in E. contradiction.
Qed.

Lemma outcome_env : forall F d d' H types t (b : Prop) r, type_meanings d' = type_meanings d ->
  Outcome F d' H types t b r -> Outcome F d H types t b r.
Proof.
  intros F d d' H types t b r Meq Out. assert (G : forall u, ground d' u = ground d u) by (intros u; unfold ground; rewrite Meq; reflexivity).
  destruct r as [[] g st]; cbn in *; auto.
  - destruct Out as [H' [E [S [L T]]]]. exists H'; repeat split; auto. rewrite <- G. exact T.
  - destruct Out as [H' [ret [R [E [S [L T]]]]]]. exists H', ret; repeat split; auto. rewrite <- G. exact T.
Qed.


Lemma bindings_function_eq : forall j d g st f f' types sm r,
  AR.O.I.eval_bindings (S j) P d g st (BFunction f f' types sm :: r) =
  match select_provisions d sm with
  | Some dc => AR.O.I.eval_bindings j P (install_provision (IFn f) (FunctionClosure f' (typed_capture d types dc)) d) g st r
  | None => inr (AR.O.I.Evaluation (Stuck AR.O.I.e_compose) g st)
  end.
Proof. reflexivity. Qed.

Lemma calloutcome_ret : forall H t g r, CallOutcome H t r -> CallOutcome H t (AR.O.I.ret_call g r).
Proof. intros H t g [[] l st] Out; cbn in *; auto. Qed.

Lemma calloutcome_prim : forall H t o st g, PrimOutcome H t (o, st) -> CallOutcome H t (AR.O.I.Evaluation o g st).
Proof. intros H t [] st g Out; cbn in *; auto; contradiction. Qed.

Lemma lookup_params_from : forall ws base q, q < List.length ws ->
  lookup_local (base + q) (combine (seq base (List.length ws)) ws) = nth_error ws q.
Proof.
  induction ws as [|w rest IH]; intros base q Lq; cbn in Lq; [lia|].
  destruct q as [|q]; cbn [seq combine lookup_local nth_error List.length].
  - rewrite Nat.add_0_r, Nat.eqb_refl. reflexivity.
  - destruct (Nat.eqb_spec (base + S q) base) as [E|_]; [lia|].
    replace (base + S q) with (S base + q) by lia. apply IH. lia.
Qed.

(* Reading the parameters back as arguments. *)
Lemma vars_args : forall i d ws st,
  AR.O.I.eval_arguments i P d (parameter_values ws) st (map EVar (seq 0 (List.length ws))) = inl (ws, parameter_values ws, st) \/
  exists l, AR.O.I.eval_arguments i P d (parameter_values ws) st (map EVar (seq 0 (List.length ws))) = inr (AR.O.I.Evaluation OutOfFuel l st).
Proof.
  intros i d ws st.
  assert (Gen : forall i start rest,
    (forall q, q < List.length rest -> lookup_local (start + q) (parameter_values ws) = nth_error rest q) ->
    AR.O.I.eval_arguments i P d (parameter_values ws) st (map EVar (seq start (List.length rest))) = inl (rest, parameter_values ws, st) \/
    exists l, AR.O.I.eval_arguments i P d (parameter_values ws) st (map EVar (seq start (List.length rest))) = inr (AR.O.I.Evaluation OutOfFuel l st)).
  { clear i. intros i. induction i as [|i IH]; intros start rest Look; [right; exists (parameter_values ws); reflexivity|].
    destruct rest as [|w rest]; [left; reflexivity|]. cbn [List.length seq map AR.O.I.eval_arguments].
    destruct i as [|i]; [right; exists (parameter_values ws); reflexivity|].
    pose proof (Look 0 ltac:(cbn; lia)) as L0. rewrite Nat.add_0_r in L0. cbn [nth_error] in L0.
    cbn [AR.O.I.eval]. rewrite L0.
    destruct (IH (S start) rest) as [E|[l E]].
    - intros q Lq. replace (S start + q) with (start + S q) by lia. exact (Look (S q) ltac:(cbn; lia)).
    - rewrite E. left. reflexivity.
    - rewrite E. right. eauto. }
  apply (Gen i 0 ws). intros q Lq. unfold parameter_values. exact (lookup_params_from ws 0 q Lq).
Qed.

(* A closure bound from a provider is callable below the binding's fuel. *)
Lemma provider_callable : forall k, Below k -> forall jn, jn <= S k -> forall F C pa sg g types sm d H,
  LegacyBodyJudgments.FnProvider W Prog F C (AEPath pa None) sg None g types sm ->
  Realizes jn C d H ->
  exists dc, select_provisions d sm = Some dc /\
    CallableOk jn H d (A.sg_params sg) (A.sg_ret sg) (FunctionClosure g (typed_capture d types dc)).
Proof.
  intros k Bel jn Ljn F C pa sg g types sm d H Prov Real.
  pose proof Real as [_ [[_ [Fns _]] _]].
  remember (AEPath pa None) as src eqn:Src. remember (@None ty) as none eqn:NoThis.
  pose proof (realizes_absorbs_canon _ _ _ _ Real) as AbsRead.
  destruct Prov as [F C pa' g sg Cg ns sm th R K Def Ctx Sat CT SigG|F C pa' sg handle w Typed Translated].
  - destruct (callee_realizes_le jn jn C d H g Cg ns sm th (le_n jn) Real Ctx Sat CT) as [dc [Sel [Ag RealG]]].
    exists dc. split; [exact Sel|].
    destruct Def as [n0 [ps0 [rt0 [b0 Def]]]]. unfold A.ast_of in Def.
    destruct (A.w_decl W g) as [dcl|] eqn:Decl; [|discriminate]. injection Def as Ast.
    pose proof (all_fn_elab g dcl RPlain n0 ps0 rt0 b0 Decl Ast dcl RPlain n0 ps0 rt0 b0 Decl Ast)
      as (C0 & ns0 & this & g0 & F0 & x & t0 & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & View).
    cbn [CallableOk]. exists x. split; [change (view_function P g) with (A.body_of Prog g); exact View|].
    intros jj Ljj H' st ws E S Args.
    assert (RealJ : Realizes jj Cg (callee_env d th dc) H').
    { apply (realizes_extends _ _ _ H); [exact E|]. eapply realizes_lower; [|exact RealG]. lia. }
    destruct (body_call jj (Bel jj ltac:(lia)) g dcl n0 ps0 rt0 b0 Cg ns (callee_env d th dc) H' st ws d
      (A.sg_params sg) (A.sg_ret sg) Decl Ast Ctx RealJ S eq_refl (keys_meant_callee Cg d th dc (proj1 CT))) as [body [View' Run]].
    + intros g' Sig'. destruct (sig_rel W _ _ g _ _ None None g' _ Sig' SigG Ag (fun _ _ E' _ => ltac:(discriminate E')))
        as [Ps Rt]. cbn [A.sg_params A.sg_ret] in Ps, Rt. split.
      * change (map (app_subst (type_meanings (callee_env d th dc))) (A.sg_params g') =
                map (ground d) (A.sg_params sg)). rewrite Ps, map_map. apply map_ext. intros u. apply AbsRead.
      * change (app_subst (type_meanings (callee_env d th dc)) (A.sg_ret g') = ground d (A.sg_ret sg)).
        rewrite Rt. apply AbsRead.
    + exact Args.
    + change (view_function P g) with (A.body_of Prog g) in View'. rewrite View in View'. injection View' as <-.
      exact Run.
  - (* forwarder *)
    destruct handle as [g]. destruct Typed as (_ & _ & _ & Fn). destruct Translated as [_ Body].
    destruct (Fns g sg Fn) as [pv [Look Ok]].
    exists (install_provision (IFn g) pv empty_environment). split; [cbn; rewrite Look; reflexivity|].
    cbn [CallableOk]. exists (A.forwarder_body (A.ForwarderOf g) sg).
    split; [change (view_function P w) with (A.body_of Prog w); exact Body|].
    intros [|jj] Ljj H' st ws E S Args; [exact Logic.I|].
    assert (Len' : List.length ws = List.length (A.sg_params sg)) by (pose proof (Forall2_length Args); congruence).
    unfold A.forwarder_body. cbn [A.forwarder_source].
    rewrite <- Len'. cbn [AR.O.I.eval].
    assert (L0 : lookup_provision (IFn g) (typed_capture d (TypeCapture [] []) (install_provision (IFn g) pv empty_environment)) = Some pv)
      by (cbn; rewrite Nat.eqb_refl; reflexivity).
    rewrite L0.
    destruct pv as [w0|f' df|u sms dc|p held]; cbn [CallableOk] in Ok; try contradiction.
    + destruct Ok as [body [View Run]]. rewrite View.
      destruct (vars_args jj (typed_capture d (TypeCapture [] []) (install_provision (IFn g) (FunctionClosure f' df) empty_environment)) ws st)
        as [Ea|[l Ea]]; rewrite Ea; [|exact Logic.I].
      apply calloutcome_ret. exact (Run jj ltac:(lia) H' st ws E S Args).
    + destruct (vars_args jj (typed_capture d (TypeCapture [] []) (install_provision (IFn g) (PrimitiveClosure p held) empty_environment)) ws st)
        as [Ea|[l Ea]]; rewrite Ea; [|exact Logic.I].
      pose proof (Ok H' st ws E S Args) as POut.
      destruct (AR.O.I.run_prim_at P p held ws st) as [o st2]. exact (calloutcome_prim _ _ _ _ _ POut).
Qed.


(* ---- attached method calls ---------------------------------------------------- *)

Hypothesis World : exists b, A.WorldOf b W.

Lemma units_bare : PublicFormation.UnitsBare W.
Proof. destruct World as [b Wo]. exact (PublicFormation.units_bare_of W b Wo). Qed.

(* A bind item leaves a formed context (T29, PublicFormation). *)
Lemma item_formed : forall F C it w C' F' m, LegacyBodyJudgments.BindItem W Prog F C it w C' F' m -> Formed F C -> Formed F' C'.
Proof.
  intros F C it w C' F' m D [Fc [Ff Cs]].
  destruct (PublicFormation.binditem_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ _ D Fc Ff) as [Fc' Ff'].
  exact (conj Fc' (conj Ff' (proj1 (PublicScoping.binditem_scoped W Prog units_bare family_sig_ok _ _ _ _ _ _ _ D Fc Ff Cs)))).
Qed.

(* Fewer provisions keep a context formed. *)
Lemma formed_narrow : forall F C C', A.c_ty C' = A.c_ty C -> A.c_sub C' = A.c_sub C -> A.c_merge C' = A.c_merge C -> A.c_locals C' = A.c_locals C ->
  incl (A.c_provisions C') (A.c_provisions C) -> Formed F C -> Formed F C'.
Proof.
  intros F C C' Ety Esub Merge Eloc Inc [[[Ty Sub Sh Pr Lo] [Ks Ts]] [Ff Cs]].
  split; [|split; [exact Ff|exact (PublicScoping.cxscoped_narrow W F C C' Ety Esub Merge Eloc Inc Cs)]].
  assert (Sc : PublicFormation.scope_gens C' = PublicFormation.scope_gens C)
    by (unfold PublicFormation.scope_gens, A.canon; rewrite Ety, Esub; reflexivity).
  split; [constructor|split].
  - rewrite Ety. exact Ty.
  - rewrite Esub. exact Sub.
  - rewrite Merge. exact Sh.
  - intros k p I. exact (Pr k p (Inc _ I)).
  - rewrite Eloc. exact Lo.
  - intros r m th p I. exact (PublicFormation.scoped_ext W _ _ _ (eq_sym Sc) (Ks r m th p (Inc _ I))).
  - intros x Ix. unfold PublicFormation.TysScoped in *. rewrite Ety in Ix. exact (Ts x Ix).
Qed.


Lemma attached_unique : forall r r' n s, A.Attached W r n s -> A.Attached W r' n s -> r = r'.
Proof.
  intros r r' n s At At'. destruct World as [b Wo].
  apply (proj1 (A.wo_attached Wo r n s)) in At. apply (proj1 (A.wo_attached Wo r' n s)) in At'.
  destruct At as (d & rn & ps & rt & body & D & Ast & _ & Scope).
  destruct At' as (d' & rn' & ps' & rt' & body' & D' & Ast' & _ & Scope').
  rewrite D in D'. injection D' as <-. rewrite Ast in Ast'. injection Ast' as <- _ _ _.
  congruence.
Qed.

Lemma set_tys_fields : forall (rargs : list (nat * ty)) C,
  A.c_sub (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C) = A.c_sub C /\
  A.c_provisions (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C) = A.c_provisions C /\
  A.c_locals (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C) = A.c_locals C /\
  A.c_ty (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C) = (rev rargs ++ A.c_ty C)%list.
Proof.
  induction rargs as [|[x t] rest IH]; intros C; [repeat split; reflexivity|].
  cbn [fold_left fst snd]. destruct (IH (A.set_ty C x t)) as [E1 [E2 [E3 E4]]]. rewrite E1, E2, E3, E4.
  cbn. rewrite <- app_assoc. repeat split; reflexivity.
Qed.

Lemma demand_ty : forall C0 wanted used C wrap C', A.Demand W C0 wanted used C wrap C' -> A.c_ty C' = A.c_ty C.
Proof.
  intros C0 wanted used C wrap C' D. induction D as [used C|used C u matching th formal needs Cr ks rest Cf NotUsed Step Rest IH];
    [reflexivity|]. rewrite IH. destruct Step. reflexivity.
Qed.

Lemma demand_sub : forall C0 wanted used C wrap C', A.Demand W C0 wanted used C wrap C' -> A.c_sub C' = A.c_sub C.
Proof.
  intros C0 wanted used C wrap C' D. induction D as [used C|used C u matching th formal needs Cr ks rest Cf NotUsed Step Rest IH];
    [reflexivity|]. rewrite IH. destruct Step as [C ot m u matching th formal needs Cr ks _ _ _ Typed _ _ _]. cbn.
  pose (sm := A.satisfaction_map (A.family_context C matching) formal th needs).
  pose proof (AuditFacts.family_application_with_map Typed) as FA.
  exact (PublicFormation.family_application_sub W C u matching th sm Cr ks FA).
Qed.

Lemma demand_merge : forall C0 wanted used C wrap C', A.Demand W C0 wanted used C wrap C' -> A.c_merge C' = A.c_merge C.
Proof.
  intros C0 wanted used C wrap C' D. induction D as [used C|used C u matching th formal needs Cr ks rest Cf NotUsed Step Rest IH];
    [reflexivity|]. rewrite IH. destruct Step as [C ot m u matching th formal needs Cr ks _ _ _ Typed _ _ _]. cbn.
  pose (sm := A.satisfaction_map (A.family_context C matching) formal th needs).
  pose proof (AuditFacts.family_application_with_map Typed) as FA.
  exact (PublicContextFacts.family_application_merge W C u matching th sm Cr ks FA).
Qed.

(* CallTheta reads the caller only through its table and merge. *)
Lemma call_theta_same : forall C1 C2 Cs s th,
  A.c_ty C1 = A.c_ty C2 -> A.c_sub C1 = A.c_sub C2 -> A.c_merge C1 = A.c_merge C2 ->
  A.CallTheta W C1 Cs s th -> A.CallTheta W C2 Cs s th.
Proof.
  intros C1 C2 Cs s th Et Es Em CT.
  unfold A.CallTheta, A.canon in *. now rewrite <- Et, <- Es, <- Em.
Qed.

Lemma pre_with_ty : forall n F C0 C d types H g st, A.c_ty C = A.c_ty C0 ->
  Pre n F (with_ty C0 C) d types H g st -> Pre n F C d types H g st.
Proof. intros n F C0 [ty sub classes provs locs] d types H g st; cbn; intros -> Pre0. exact Pre0. Qed.

Lemma receiver_agree : forall att Ca ns Cd th d' r args xs (rest : list (nat * ty)),
  A.CtxOf W att Ca ns -> A.CallTheta W Cd Ca att th -> CxSection Cd -> AbsorbsCanon Cd d' ->
  NoDup (map fst xs) -> A.c_ty Cd = (rev xs ++ rest)%list ->
  Forall2 (fun x xa => fst xa = x /\ A.nlook x (A.c_ty Ca) = Some (snd xa)) (A.w_reqs W r) args ->
  map fst xs = A.w_reqs W r ->
  app_subst (callee_meanings d' th) (TNom r args) = ground d' (TNom r xs).
Proof.
  intros att Ca ns Cd th d' r args xs rest Ctx CT SectionD Abs Nd Cty Args Keys.
  pose proof (proj2 (absorbs_canon_iff Cd d' SectionD) Abs) as AbsM.
  destruct (ctxof_formed W att Ca ns Ctx) as [Form _].
  pose proof CT as [ThKeys [Bound _]].
  unfold ground. rewrite !subst_nom. f_equal.
  assert (Gen : forall reqs args' xs', Forall2 (fun x xa => fst xa = x /\ A.nlook x (A.c_ty Ca) = Some (snd xa)) reqs args' ->
    map fst xs' = reqs -> incl xs' xs ->
    PublicTypeRead.smap (callee_meanings d' th) args' = PublicTypeRead.smap (type_meanings d') xs').
  { intros reqs. induction reqs as [|x reqs IH]; intros args' xs' F Kx Sub; inversion F as [|? q ? args'' Hq Fr]; subst.
    - destruct xs'; [reflexivity|discriminate].
    - destruct q as [x' v]. destruct Hq as [Ex Lv].
      destruct xs' as [|[x0 u] xs'']; [discriminate|]. cbn in Kx, Ex. injection Kx as -> Kx'. subst x'.
      unfold PublicTypeRead.smap. cbn [map fst snd]. f_equal.
      + f_equal. cbn [snd] in Lv. rewrite (Form x v Lv).
        assert (Ix : In x (map fst (A.c_ty Ca))) by (apply nlook_in in Lv; change x with (fst (x, v)); apply in_map, Lv).
        rewrite (merged_meaning W att Ca ns Cd th d' x Ctx CT AbsM Ix).
        assert (Lu : A.nlook x (A.c_ty Cd) = Some u).
        { rewrite Cty. rewrite (nlook_rev_distinct _ xs rest x Nd). apply nlook_app_in; [exact Nd|]. apply Sub. left. reflexivity. }
        cbn [app_subst]. rewrite (Bound x Ix ltac:(congruence)). cbn [app_subst]. rewrite lookup_subst_nlook', Lu. apply Abs.
      + apply IH; [exact Fr|exact Kx'|intros q Iq; apply Sub; right; exact Iq]. }
  exact (Gen _ args xs Args Keys (fun q Iq => Iq)).
Qed.

(* ---- method binds ------------------------------------------------------------- *)


(* Realization does not read the type table. *)
Lemma realizes_ty_free : forall n C C' d H,
  A.c_sub C' = A.c_sub C -> A.c_merge C' = A.c_merge C -> A.c_provisions C' = A.c_provisions C ->
  Realizes n C d H -> Realizes n C' d H.
Proof.
  intros n [ty sub classes provs locs] [ty' sub' classes' provs' locs'] d H; cbn; intros -> -> -> Real; exact Real.
Qed.

(* An attached method bound at a key is entered at the key's receiver. *)
Lemma attached_provider_callable : forall k, Below k -> forall jn, jn <= S k ->
  forall C rg n att rt sg Ca ns sm th rargs d H,
  A.Attached W rg n att ->
  (exists rn ps r b, A.ast_of W att = Some (ADFn (RAttached rn) n ps r (Some b))) ->
  A.canon C rt = TNom rg rargs -> map fst rargs = A.w_reqs W rg ->
  A.CtxOf W att Ca ns ->
  A.Satisfies (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C) Ca th ns sm ->
  A.CallTheta W (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C) Ca att th ->
  A.SigOf W att (A.c_ty (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C)) (Some (A.canon C rt))
    (A.FSig (map (A.canon C) (A.sg_params sg)) (A.canon C (A.sg_ret sg))) ->
  Realizes jn C d H ->
  exists dc, select_provisions d sm = Some dc /\
    CallableOk jn H d (A.canon C rt :: A.sg_params sg) (A.sg_ret sg) (FunctionClosure att (typed_capture d (TypeCapture th th) dc)).
Proof.
  intros k Bel jn Ljn C rg n att rt sg Ca ns sm th rargs d H Att Def Canon Keys Ctx Sat CT Sig Real.
  set (Cd := fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C) in *.
  destruct (set_tys_fields rargs C) as [Es [Ep [El Et]]].
  pose proof (realizes_absorbs_canon _ _ _ _ Real) as Abs.
  pose proof (realizes_ty_free _ C Cd d H Es (PublicContextFacts.family_context_merge rargs C) Ep Real) as RealD.
  pose proof (realizes_absorbs_canon _ _ _ _ RealD) as AbsD.
  destruct (callee_realizes_le jn jn Cd d H att Ca ns sm th (le_n jn) RealD Ctx Sat CT) as [dc [Sel [Ag RealA]]].
  exists dc. split; [exact Sel|].
  destruct Def as [rn [ps [rt0 [b Def]]]]. unfold A.ast_of in Def.
  destruct (A.w_decl W att) as [dcl|] eqn:Decl; [|discriminate]. injection Def as Ast.
  pose proof (all_fn_elab att dcl (RAttached rn) n ps rt0 b Decl Ast dcl (RAttached rn) n ps rt0 b Decl Ast)
    as (C0 & ns0 & this & g0 & F0 & x0 & t0 & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & View).
  assert (Nd : NoDup (map fst rargs)) by (rewrite Keys; destruct World as [bd Wo]; exact (A.wo_reqs_nodup Wo rg)).
  cbn [CallableOk]. exists x0. split; [change (view_function P att) with (A.body_of Prog att); exact View|].
  intros jj Ljj H' st ws E S Args.
  inversion Args as [|w0 t0' ws' ts' Tw0 Tws]; subst.
  assert (RealJ : Realizes jj Ca (callee_env d th dc) H').
  { apply (realizes_extends _ _ _ H); [exact E|]. eapply realizes_lower; [|exact RealA]. lia. }
  destruct (body_call_method jj (Bel jj ltac:(lia)) att dcl rn n ps rt0 b Ca ns _ H' st w0 ws' d
    (A.canon C rt) (A.sg_params sg) (A.sg_ret sg) Decl Ast Ctx RealJ S eq_refl (keys_meant_callee Ca d th dc (proj1 CT))) as [body [View' Run]].
  - intros r' args' g0' Att' Reqs Sig0.
    pose proof (attached_unique rg r' n att Att Att') as <-.
    assert (Recv : app_subst (callee_meanings d th) (TNom rg args') = ground d (TNom rg rargs))
      by exact (receiver_agree att Ca ns Cd th d rg args' rargs (A.c_ty C) Ctx CT (proj1 (proj2 (proj2 RealD))) AbsD Nd Et Reqs Keys).
    destruct (sig_rel W _ _ att _ _ (Some (TNom rg args')) (Some (A.canon C rt)) g0' _ Sig0 Sig Ag
      (fun x1 x2 E1' E2' => ltac:(injection E1' as <-; injection E2' as <-; rewrite Canon; exact Recv))) as [Ps Rt].
    cbn [A.sg_params A.sg_ret] in Ps, Rt.
    split; [change (app_subst (callee_meanings d th) (TNom rg args') = ground d (A.canon C rt)); rewrite Canon; exact Recv|].
    split.
    + etransitivity; [exact Ps|]. rewrite map_map. apply map_ext. intros u. apply Abs.
    + etransitivity; [exact Rt|]. apply Abs.
  - exact Tw0.
  - exact Tws.
  - change (view_function P att) with (A.body_of Prog att) in View'. rewrite View in View'. injection View' as <-.
    exact Run.
Qed.

Lemma bindings_method_eq : forall j d g st r m th f' types sm rest,
  AR.O.I.eval_bindings (S j) P d g st (BMethod r m th f' types sm :: rest) =
  match select_provisions d sm with
  | Some dc => AR.O.I.eval_bindings j P (install_provision (IMth r m th) (FunctionClosure f' (typed_capture d types dc)) d) g st rest
  | None => inr (AR.O.I.Evaluation (Stuck AR.O.I.e_compose) g st)
  end.
Proof. reflexivity. Qed.

Lemma theta_of_sub : forall C C' a, A.c_sub C' = A.c_sub C -> A.theta_of C' a = A.theta_of C a.
Proof. intros C C' a E. unfold A.theta_of, A.canon. rewrite E. reflexivity. Qed.


(* ---- lambda closures ------------------------------------------------------- *)

(* A lambda's closure keeps the meanings of exactly its context's
   generations (merges included). *)
Definition capture_meanings (d : environment) (K : list nat) : subst :=
  map (fun g => (g, ground d (TAbs g))) K.

Lemma lookup_capture : forall d K y, In y K -> lookup_subst y (capture_meanings d K) = Some (ground d (TAbs y)).
Proof.
  intros d K y. induction K as [|g rest IH]; intros Iy; [contradiction|]. cbn.
  destruct (Nat.eqb_spec y g) as [->|Ne]; [reflexivity|].
  destruct Iy as [E|Iy]; [congruence|exact (IH Iy)].
Qed.

Lemma ground_capture : forall d K t, incl (A.gens t) K -> app_subst (capture_meanings d K) t = ground d t.
Proof.
  intros d K t Sc. unfold ground. apply subst_agree_on. intros x Ix.
  cbn [app_subst]. rewrite (lookup_capture d K x (Sc x Ix)). reflexivity.
Qed.

(* What a lambda captures the meanings of: its context's generations
   (Audit's lexical_capture). *)
Definition capture_gens (C : A.cx) : list nat := A.context_gens C.

Lemma cg_l : forall C, incl (A.context_gens C) (capture_gens C).
Proof. intros C x I. exact I. Qed.

(* A method key's receiver is among them in a formed context (rule B:
   PublicFormation.KeysScoped). *)
Lemma cg_key : forall C r m th p, PublicFormation.KeysScoped W C -> In (IMth r m th, p) (A.c_provisions C) ->
  incl (A.gens r) (capture_gens C).
Proof.
  intros C r m th p Ks I x Ix. pose proof (Ks r m th p I x Ix) as Sc. unfold capture_gens, A.context_gens.
  unfold PublicFormation.GenScoped, PublicFormation.scope_gens in Sc.
  apply in_app_or in Sc as [Sc|Sc]; apply in_or_app; [left; exact Sc|right; apply in_or_app; left; exact Sc].
Qed.

Lemma typed_capture_meanings : forall d C dc,
  type_meanings (typed_capture d (A.lexical_capture C) dc) = capture_meanings d (capture_gens C).
Proof. intros d C dc. cbn. unfold capture_meanings. rewrite map_map. reflexivity. Qed.

Lemma sub_context_gens : forall C x u, In (x, u) (A.c_sub C) ->
  In x (A.context_gens C) /\ incl (A.gens u) (A.context_gens C).
Proof.
  intros C x u I. unfold A.context_gens. split.
  - apply in_or_app. left. apply in_flat_map. exists (x, u). split; [exact I|left; reflexivity].
  - intros g Ig. apply in_or_app. left. apply in_flat_map. exists (x, u). split; [exact I|right; exact Ig].
Qed.

Lemma capture_absorbs : forall C d, AbsorbsCanon C d ->
  forall t, app_subst (capture_meanings d (capture_gens C)) (A.canon C t) = app_subst (capture_meanings d (capture_gens C)) t.
Proof.
  intros C d Abs t. unfold A.canon. rewrite <- (app_subst_nil t) at 2.
  apply subst2_agree. intros x _. cbn [app_subst lookup_subst].
  destruct (lookup_subst x (A.c_sub C)) as [u|] eqn:L; [|reflexivity].
  destruct (sub_context_gens C x u (lookup_subst_in_pair _ _ _ L)) as [Ix Su].
  rewrite (ground_capture d _ u (fun y I => cg_l C y (Su y I))), (lookup_capture d _ x (cg_l C x Ix)).
  pose proof (Abs (TAbs x)) as E. unfold A.canon in E. cbn [app_subst] in E. rewrite L in E. exact E.
Qed.

(* The lambda's own context: the captured vals provided, the captured
   locals in force, nothing else changed. *)
Definition cl_fold (C : A.cx) : A.cx -> name * nat -> A.cx :=
  fun C' q => let '(n, v) := q in
    match A.slook n (A.c_locals C) with
    | Some (t, _) => A.add_local (A.provide C' (IVal v) [] (A.set_val C' v t)) n t A.LCaptured
    | None => C' end.

Lemma cl_fold_eq : forall C B n v, cl_fold C B (n, v) =
  match A.slook n (A.c_locals C) with
  | Some (t, _) => A.add_local (A.provide B (IVal v) [] (A.set_val B v t)) n t A.LCaptured
  | None => B end.
Proof. reflexivity. Qed.

Lemma cl_step_eq : forall C' v t, A.provide C' (IVal v) [] (A.set_val C' v t) = set_cx C' (IVal v) (A.ProvidedVal t []).
Proof. intros C' v t. exact (provide_set C' (IVal v) (A.ProvidedVal t []) [] (fun _ _ _ => ex_intro _ v eq_refl)). Qed.

Lemma cl_fields : forall C qs B,
  A.c_ty (fold_left (cl_fold C) qs B) = A.c_ty B /\ A.c_sub (fold_left (cl_fold C) qs B) = A.c_sub B.
Proof.
  intros C qs. induction qs as [|[n v] rest IH]; intros B; [split; reflexivity|].
  cbn [fold_left]. rewrite cl_fold_eq. destruct (A.slook n (A.c_locals C)) as [[t mu]|]; [|apply IH].
  rewrite cl_step_eq. destruct (IH (A.add_local (set_cx B (IVal v) (A.ProvidedVal t [])) n t A.LCaptured)) as [E1 E2].
  rewrite E1, E2. split; reflexivity.
Qed.

Lemma cl_merge : forall C qs B,
  A.c_merge (fold_left (cl_fold C) qs B) = A.c_merge B.
Proof.
  intros C qs. induction qs as [|[n v] rest IH]; intros B; [reflexivity |].
  cbn [fold_left]. rewrite cl_fold_eq. destruct (A.slook n (A.c_locals C)) as [[t mu]|];
    [|apply IH].
  rewrite cl_step_eq, IH. reflexivity.
Qed.

Lemma cl_other : forall C qs B k, (forall q, In q qs -> k <> IVal (snd q)) ->
  A.ilook k (A.c_provisions (fold_left (cl_fold C) qs B)) = A.ilook k (A.c_provisions B).
Proof.
  intros C qs. induction qs as [|[n v] rest IH]; intros B k Out; [reflexivity|].
  cbn [fold_left]. rewrite cl_fold_eq. destruct (A.slook n (A.c_locals C)) as [[t mu]|];
    [|apply IH; intros q Iq; apply Out; right; exact Iq].
  rewrite cl_step_eq. rewrite IH by (intros q Iq; apply Out; right; exact Iq).
  cbn [A.add_local A.c_provisions set_cx]. rewrite ilook_set.
  destruct (item_eqb k (IVal v)) eqn:E; [|reflexivity].
  apply item_eqb_spec in E. exfalso. exact (Out (n, v) (or_introl eq_refl) E).
Qed.

Lemma cl_captured : forall C qs B n v t mu, NoDup (map snd qs) -> In (n, v) qs ->
  A.slook n (A.c_locals C) = Some (t, mu) ->
  A.ilook (IVal v) (A.c_provisions (fold_left (cl_fold C) qs B)) = Some (A.ProvidedVal t []).
Proof.
  intros C qs. induction qs as [|[n' v'] rest IH]; intros B n v t mu Nd Iq L; [contradiction|].
  inversion Nd as [|? ? NotIn Nd']; subst. cbn [fold_left]. destruct Iq as [E|Iq].
  - injection E as <- <-. rewrite cl_fold_eq. rewrite L. rewrite cl_step_eq.
    rewrite cl_other.
    + cbn [A.add_local A.c_provisions set_cx]. rewrite ilook_set, (proj2 (item_eqb_spec _ _) eq_refl). reflexivity.
    + intros q Iq E. injection E as E. apply NotIn. rewrite E. apply in_map. exact Iq.
  - apply (IH _ n v t mu Nd' Iq L).
Qed.

Lemma cl_keys : forall C qs B, PublicCallRealize.KeysMatch (A.c_provisions B) ->
  PublicCallRealize.KeysMatch (A.c_provisions (fold_left (cl_fold C) qs B)).
Proof.
  intros C qs. induction qs as [|[n v] rest IH]; intros B K; [exact K|].
  cbn [fold_left]. rewrite cl_fold_eq. destruct (A.slook n (A.c_locals C)) as [[t mu]|]; [|exact (IH B K)].
  rewrite cl_step_eq. apply IH. cbn [A.add_local A.c_provisions set_cx].
  apply keys_match_set; [exact K|]. intros k0 p0 [E|[]]. injection E as <- <-. exact Logic.I.
Qed.

Lemma cl_locals : forall C qs B n t mu, A.slook n (A.c_locals (fold_left (cl_fold C) qs B)) = Some (t, mu) ->
  (exists v mu0, In (n, v) qs /\ A.slook n (A.c_locals C) = Some (t, mu0)) \/ A.slook n (A.c_locals B) = Some (t, mu).
Proof.
  intros C qs. induction qs as [|[n' v'] rest IH]; intros B n t mu Found; [right; exact Found|].
  cbn [fold_left] in Found. rewrite cl_fold_eq in Found.
  destruct (A.slook n' (A.c_locals C)) as [[t' mu']|] eqn:L.
  - rewrite cl_step_eq in Found. destruct (IH _ n t mu Found) as [[v [mu0 [Iq L']]]|Here].
    + left. exists v, mu0. split; [right; exact Iq|exact L'].
    + cbn [A.add_local A.c_locals set_cx A.slook] in Here. destruct (String.eqb_spec n n') as [->|_].
      * injection Here as <- _. left. exists v', mu'. split; [left; reflexivity|exact L].
      * right. exact Here.
  - destruct (IH _ n t mu Found) as [[v [mu0 [Iq L']]]|Here]; [|right; exact Here].
    left. exists v, mu0. split; [right; exact Iq|exact L'].
Qed.

Lemma cl_rkey : forall C qs B r m, A.c_sub B = A.c_sub C ->
  (forall q, In q qs -> A.mkey r m <> IVal (snd q)) -> A.c_provisions B = A.c_provisions C ->
  A.rkey (fold_left (cl_fold C) qs B) r m = A.rkey C r m.
Proof.
  intros C qs B r m Sub Out Prov. unfold A.rkey. rewrite !capps_ilook. rewrite (cl_other C qs B _ Out), Prov.
  destruct (A.ilook (A.mkey r m) (A.c_provisions C)) as [p|]; [|reflexivity]. cbn [option_map].
  f_equal. apply theta_of_sub. rewrite (proj2 (cl_fields C qs B)). exact Sub.
Qed.

Lemma prov_context_gens : forall C k p, In (k, p) (A.c_provisions C) -> incl (A.provision_gens p) (A.context_gens C).
Proof.
  intros C k p I g Ig. unfold A.context_gens. apply in_or_app. right. apply in_or_app. right.
  apply in_or_app. left. apply in_flat_map. exists (k, p). split; [exact I|apply in_or_app; left; exact Ig].
Qed.

Lemma local_context_gens : forall C n t mu, A.slook n (A.c_locals C) = Some (t, mu) -> incl (A.gens t) (A.context_gens C).
Proof.
  intros C n t mu L g Ig. unfold A.context_gens. apply in_or_app. right. apply in_or_app. right.
  apply in_or_app. right. apply in_flat_map. exists (n, (t, mu)). split; [exact (slook_in _ _ _ _ L)|].
  apply in_or_app. left. exact Ig.
Qed.

Lemma sig_provision_gens : forall p g, A.provision_signature p = Some g ->
  incl (A.gens (A.sg_ret g)) (A.provision_gens p) /\ forall t, In t (A.sg_params g) -> incl (A.gens t) (A.provision_gens p).
Proof.
  intros p g Sig. unfold A.provision_gens. rewrite Sig. split.
  - intros x Ix. apply in_or_app. left. apply in_flat_map. exists (A.sg_ret g). split; [left; reflexivity|exact Ix].
  - intros t It x Ix. apply in_or_app. left. apply in_flat_map. exists t. split; [right; exact It|exact Ix].
Qed.

Lemma ground_params_capture : forall d K (ps : list ty), (forall t, In t ps -> incl (A.gens t) K) ->
  map (ground d) ps = map (app_subst (capture_meanings d K)) ps.
Proof.
  intros d K ps Sc. apply map_ext_in. intros t It. symmetry. exact (ground_capture d K t (Sc t It)).
Qed.

(* The closure realizes the lambda's context: what it captured is what
   the context provides, at the same meanings. *)
Lemma lambda_realizes_le : forall n0 n C qs Cl d1 H dc keys, n <= n0 ->
  Cl = fold_left (cl_fold C) qs (A.Cx (A.c_ty C) (A.c_sub C) (A.c_merge C) (A.c_provisions C) []) ->
  Realizes n0 C d1 H -> NoDup (map snd qs) ->
  (forall q, In q qs -> exists t mu w, A.slook (fst q) (A.c_locals C) = Some (t, mu) /\
     lookup_provision (IVal (snd q)) d1 = Some (ValueProvision w) /\ value_typed W P H w (ground d1 t)) ->
  (forall k, In k keys <-> (A.provides Cl k /\ (match k with ITy _ => False | _ => True end) /\ k = A.provision_id k)) ->
  select_provisions d1 (map (fun k => (A.rkey_of Cl k, A.rkey_of C k)) keys) = Some dc ->
  PublicFormation.KeysScoped W C ->
  Realizes n Cl (typed_capture d1 (A.lexical_capture C) dc) H.
Proof.
  intros n0 n C qs Cl d1 H dc keys Le ClE Real Nd Caps Keys Sel KsC.
  pose proof Real as [Abs [[Vals [Fns [Mths Funs]]] [Section KM]]].
  set (de := typed_capture d1 (A.lexical_capture C) dc).
  set (K := capture_gens C).
  assert (Meq : type_meanings de = capture_meanings d1 K) by apply typed_capture_meanings.
  assert (G : forall t, incl (A.gens t) (A.context_gens C) -> ground de t = ground d1 t).
  { intros t Sc. unfold ground at 1. rewrite Meq. exact (ground_capture d1 K t (fun y I => cg_l C y (Sc y I))). }
  destruct (cl_fields C qs (A.Cx (A.c_ty C) (A.c_sub C) (A.c_merge C) (A.c_provisions C) [])) as [_ Sub].
  rewrite <- ClE in Sub. cbn [A.c_sub] in Sub.
  assert (Canon : forall t, A.canon Cl t = A.canon C t) by (intros t; unfold A.canon; rewrite Sub; reflexivity).
  assert (KMl : PublicCallRealize.KeysMatch (A.c_provisions Cl)) by (rewrite ClE; apply cl_keys; exact KM).
  assert (Other : forall k, (forall q, In q qs -> k <> IVal (snd q)) -> A.ilook k (A.c_provisions Cl) = A.ilook k (A.c_provisions C)).
  { intros k Out. rewrite ClE. exact (cl_other C qs _ k Out). }
  assert (Find : forall k, In k keys -> lookup_provision (A.rkey_of Cl k) de = lookup_provision (A.rkey_of C k) d1).
  { intros k Ik. change (lookup_provision (A.rkey_of Cl k) de) with (lookup_provision (A.rkey_of Cl k) dc).
    rewrite (select_lookup d1 _ dc _ Sel).
    rewrite (sm_find_map (A.rkey_of Cl) (A.rkey_of C) keys k Ik); [reflexivity|].
    assert (Shape : forall k', In k' keys -> NeedShape k').
    { intros k' Ik'. apply Keys in Ik' as [_ [NotTy Id]]. destruct k' as [t|v|f|r m th|u]; cbn in *; auto.
      injection Id as E. exact E. }
    intros k' Ik' E. exact (rkey_of_injective Cl k' k (Shape k' Ik') (Shape k Ik) E). }
  assert (SectionCl : CxSection Cl).
  { eapply section_same_fields; [exact Sub | rewrite ClE, cl_merge; reflexivity | exact Section]. }
  split.
  { apply (absorbs_canon_iff Cl de SectionCl).
    intros t. unfold ground. rewrite Meq, Canon.
    exact (capture_absorbs C d1 (realizes_absorbs_canon _ _ _ _ Real) t). }
  split; [|exact (conj SectionCl KMl)].
  split; [|split; [|split]].
  - (* vals *) intros v t Found.
    assert (Ik : In (IVal v) keys) by (apply Keys; cbn; split; [congruence|auto]).
    change (A.c_vals Cl) with (vals_of (A.c_provisions Cl)) in Found. rewrite (vals_by_ilook _ v KMl) in Found.
    pose proof (Find _ Ik) as F'. cbn [A.rkey_of] in F'. rewrite F'.
    destruct (in_dec Nat.eq_dec v (map snd qs)) as [Iv|Ov].
    + apply in_map_iff in Iv as [[nm v'] [Ev Iq]]. cbn in Ev. subst v'.
      destruct (Caps _ Iq) as [t0 [mu [w [L [Look T]]]]]. cbn [fst snd] in L, Look.
      rewrite ClE, (cl_captured C qs _ nm v t0 mu Nd Iq L) in Found. injection Found as <-.
      exists w. split; [exact Look|]. rewrite G; [exact T|exact (local_context_gens C nm _ mu L)].
    + rewrite Other in Found by (intros q Iq E; injection E as E; apply Ov; rewrite E; apply in_map, Iq).
      destruct (A.ilook (IVal v) (A.c_provisions C)) as [[t' a|g a|g a|fx]|] eqn:Il; try discriminate.
      injection Found as <-.
      assert (Lv : A.nlook v (A.c_vals C) = Some t')
        by (change (A.c_vals C) with (vals_of (A.c_provisions C)); rewrite (vals_by_ilook _ v KM), Il; reflexivity).
      destruct (Vals v t' Lv) as [w [Look T]]. exists w. split; [exact Look|].
      rewrite G; [exact T|]. intros x Ix. apply (prov_context_gens C _ _ (ilook_in _ _ _ Il)).
      apply (proj1 (sig_provision_gens (A.ProvidedVal t' a) (A.FSig [] t') eq_refl)). exact Ix.
  - (* fns *) intros f g Found.
    assert (Ik : In (IFn f) keys) by (apply Keys; cbn; split; [congruence|auto]).
    change (A.c_fns Cl) with (fns_of (A.c_provisions Cl)) in Found. rewrite (fns_by_ilook _ f KMl) in Found.
    rewrite Other in Found by (intros q Iq E; discriminate).
    destruct (A.ilook (IFn f) (A.c_provisions C)) as [[t' a|g' a|g' a|fx]|] eqn:Il; try discriminate.
    injection Found as ->.
    assert (Lf : A.nlook f (A.c_fns C) = Some g)
      by (change (A.c_fns C) with (fns_of (A.c_provisions C)); rewrite (fns_by_ilook _ f KM), Il; reflexivity).
    destruct (Fns f g Lf) as [pv [Look Ok]]. exists pv.
    pose proof (Find _ Ik) as F'. cbn [A.rkey_of] in F'. rewrite F'. split; [exact Look|].
    destruct (sig_provision_gens (A.ProvidedFn g a) g eq_refl) as [Sr Sp].
    pose proof (prov_context_gens C _ _ (ilook_in _ _ _ Il)) as Sc.
    apply (callable_transfer n0 n H d1 de (A.sg_params g) (A.sg_params g) (A.sg_ret g) (A.sg_ret g) pv
      Le); [| |exact Ok].
    + apply map_ext_in. intros t It. symmetry. apply G. intros x Ix. exact (Sc x (Sp t It x Ix)).
    + symmetry. apply G. intros x Ix. exact (Sc x (Sr x Ix)).
  - (* methods *) intros r m g Found.
    assert (Ik : In (A.mkey r m) keys) by (apply Keys; cbn; split; [congruence|auto]).
    change (A.c_mths Cl) with (mths_of (A.c_provisions Cl)) in Found. rewrite (mths_by_ilook _ r m KMl) in Found.
    rewrite Other in Found by (intros q Iq E; discriminate).
    destruct (A.ilook (A.mkey r m) (A.c_provisions C)) as [[t' a|g' a|g' a|fx]|] eqn:Il; try discriminate.
    injection Found as ->.
    assert (Lm : A.mlook r m (A.c_mths C) = Some g)
      by (change (A.c_mths C) with (mths_of (A.c_provisions C)); rewrite (mths_by_ilook _ r m KM), Il; reflexivity).
    destruct (Mths r m g Lm) as [pv [Look Ok]]. exists pv.
    pose proof (Find _ Ik) as F'. cbn [A.rkey_of A.mkey] in F'. rewrite F'. split; [exact Look|].
    destruct (sig_provision_gens (A.ProvidedMethod g a) g eq_refl) as [Sr Sp].
    pose proof (prov_context_gens C _ _ (ilook_in _ _ _ Il)) as Sc.
    pose proof (cg_key C r m [] _ KsC (ilook_in _ _ _ Il)) as Kr.
    apply (callable_transfer n0 n H d1 de (r :: A.sg_params g) (r :: A.sg_params g) (A.sg_ret g) (A.sg_ret g) pv
      Le); [| |exact Ok].
    + apply map_ext_in. intros t [<-|It]; symmetry;
        [unfold ground at 1; rewrite Meq; exact (ground_capture d1 K r Kr)|apply G].
      intros x Ix. exact (Sc x (Sp t It x Ix)).
    + symmetry. apply G. intros x Ix. exact (Sc x (Sr x Ix)).
  - (* functors *) intros u fx Found.
    assert (Ik : In (IFun u) keys) by (apply Keys; cbn; split; [congruence|auto]).
    change (A.c_funs Cl) with (funs_of (A.c_provisions Cl)) in Found. rewrite (funs_by_ilook _ u KMl) in Found.
    rewrite Other in Found by (intros q Iq E; discriminate).
    assert (Lu : A.nlook u (A.c_funs C) = Some fx)
      by (change (A.c_funs C) with (funs_of (A.c_provisions C)); rewrite (funs_by_ilook _ u KM); exact Found).
    destruct (Funs u fx Lu) as [pv [gfx [Look [Ok [Cov Vm]]]]]. exists pv, gfx.
    pose proof (Find _ Ik) as F'. cbn [A.rkey_of] in F'. rewrite F'. split; [exact Look|].
    split; [exact (proj1 (closure_lower n0 n H H Le (fun a c L => L)) _ _ _ Ok)|]. split; [exact Cov|].
    pose proof (funs_ilook _ u fx KM Lu) as Il.
    pose proof (prov_context_gens C _ _ (ilook_in _ _ _ Il)) as Sc.
    intros x tx I. rewrite (Vm x tx I). f_equal. symmetry. apply G. intros y Iy. apply Sc. cbn.
    apply in_flat_map. exists (x, Some tx). split; [exact (fixed_app_in _ _ _ I)|exact Iy].
Qed.

Lemma lambda_realizes : forall n C qs Cl d1 H dc keys,
  Cl = fold_left (cl_fold C) qs (A.Cx (A.c_ty C) (A.c_sub C) (A.c_merge C) (A.c_provisions C) []) ->
  Realizes (S n) C d1 H -> NoDup (map snd qs) ->
  (forall q, In q qs -> exists t mu w, A.slook (fst q) (A.c_locals C) = Some (t, mu) /\
     lookup_provision (IVal (snd q)) d1 = Some (ValueProvision w) /\ value_typed W P H w (ground d1 t)) ->
  (forall k, In k keys <-> (A.provides Cl k /\ (match k with ITy _ => False | _ => True end) /\ k = A.provision_id k)) ->
  select_provisions d1 (map (fun k => (A.rkey_of Cl k, A.rkey_of C k)) keys) = Some dc ->
  PublicFormation.KeysScoped W C ->
  Realizes n Cl (typed_capture d1 (A.lexical_capture C) dc) H.
Proof. intros n C qs Cl d1 H dc keys. exact (lambda_realizes_le (S n) n C qs Cl d1 H dc keys (le_S _ _ (le_n n))). Qed.

(* ---- running a lambda's body ------------------------------------------------ *)

Definition capty (C : A.cx) (n : name) : ty :=
  match A.slook n (A.c_locals C) with Some (t, _) => t | None => TUnit end.
Definition this_list (o : option ty) : list ty := match o with Some r => [r] | None => [] end.

Lemma eval_let_eq : forall j d g st x e1 e2,
  AR.O.I.eval (S j) P d g st (ELet x e1 e2) =
  match AR.O.I.eval j P d g st e1 with
  | AR.O.I.Evaluation (Normal w1) g1 st1 => AR.O.I.pop1 (AR.O.I.eval j P d ((x, w1) :: g1) st1 e2)
  | r => r
  end.
Proof. reflexivity. Qed.

Lemma eval_val_eq : forall j d g st v,
  AR.O.I.eval (S j) P d g st (EVal v) =
  match lookup_provision (IVal v) d with
  | Some (ValueProvision w) => AR.O.I.Evaluation (Normal w) g st
  | _ => AR.O.I.Evaluation (Stuck AR.O.I.e_val) g st
  end.
Proof. reflexivity. Qed.

Lemma calloutcome_pop1 : forall H t r, CallOutcome H t r -> CallOutcome H t (AR.O.I.pop1 r).
Proof. intros H t [[] l st] Out; cbn in *; auto. Qed.

(* The captured vals are read into the lambda's locals, then its body runs. *)
Lemma lets_call : forall Fl C de H st ret lb qs j g types,
  locals_typed W P H g types ->
  (forall q, In q qs -> exists w, lookup_provision (IVal (snd q)) de = Some (ValueProvision w) /\
     value_typed W P H w (ground de (capty C (fst q)))) ->
  (forall j' g', j' <= j ->
     locals_typed W P H g' (rev (map (fun q => (A.var_of Fl (fst q), ground de (capty C (fst q)))) qs) ++ types) ->
     CallOutcome H ret (AR.O.I.eval j' P de g' st lb)) ->
  CallOutcome H ret (AR.O.I.eval j P de g st (fold_right (fun q x => ELet (A.var_of Fl (fst q)) (EVal (snd q)) x) lb qs)).
Proof.
  intros Fl C de H st ret lb qs. induction qs as [|q rest IH]; intros j g types L Caps Cont.
  - exact (Cont j g (le_n j) L).
  - destruct j as [|j]; [exact Logic.I|]. cbn [fold_right]. rewrite eval_let_eq.
    destruct j as [|j]; [exact Logic.I|]. rewrite eval_val_eq.
    destruct (Caps q (or_introl eq_refl)) as [w [Look T]]. rewrite Look.
    apply calloutcome_pop1.
    apply (IH (S j) ((A.var_of Fl (fst q), w) :: g) ((A.var_of Fl (fst q), ground de (capty C (fst q))) :: types)).
    + exact (locals_push W P H g types _ w _ L T).
    + intros q' Iq'. exact (Caps q' (or_intror Iq')).
    + intros j' g' Lj' L'. apply Cont; [lia|]. cbn [map rev]. rewrite <- app_assoc. exact L'.
Qed.

Lemma snd_combine : forall X Y (l : list X) (l' : list Y), List.length l = List.length l' -> map snd (combine l l') = l'.
Proof. intros X Y l. induction l as [|a r IH]; intros [|b r'] E; cbn in *; try discriminate; auto. f_equal. apply IH. lia. Qed.

Lemma fst_combine : forall X Y (l : list X) (l' : list Y), List.length l = List.length l' -> map fst (combine l l') = l.
Proof. intros X Y l. induction l as [|a r IH]; intros [|b r'] E; cbn in *; try discriminate; auto. f_equal. apply IH. lia. Qed.

Lemma nodup_map_on : forall X Y (f : X -> Y) l, NoDup l -> (forall x y, In x l -> In y l -> f x = f y -> x = y) -> NoDup (map f l).
Proof.
  intros X Y f l. induction l as [|a r IH]; intros Nd Inj; [constructor|]. inversion Nd as [|? ? Out Nd']; subst.
  cbn. constructor.
  - intro I. apply in_map_iff in I as [b [E Ib]]. apply Out. rewrite (Inj b a (or_intror Ib) (or_introl eq_refl) E) in Ib. exact Ib.
  - apply IH; [exact Nd'|]. intros x y Ix Iy. apply Inj; right; assumption.
Qed.

Lemma slook_add_locals : forall (l : list (name * ty)) C0 n t mu,
  A.slook n (A.c_locals (fold_left (fun C' q => A.add_local C' (fst q) (snd q) A.LLet) l C0)) = Some (t, mu) ->
  In (n, t) l \/ (~ In n (map fst l) /\ A.slook n (A.c_locals C0) = Some (t, mu)).
Proof.
  induction l as [|[m u] rest IH]; intros C0 n t mu Found; [right; split; [intros []|exact Found]|].
  cbn [fold_left fst snd] in Found. destruct (IH _ n t mu Found) as [Ir|[Out Here]]; [left; right; exact Ir|].
  cbn [A.add_local A.c_locals A.slook] in Here. destruct (String.eqb_spec n m) as [->|Ne].
  - injection Here as <- _. left. left. reflexivity.
  - right. split; [|exact Here]. intros [E|I]; [exact (Ne (eq_sym E))|exact (Out I)].
Qed.

Lemma tm_context_gens : forall C x, In x (tm_gens (A.c_ty C)) -> In x (A.context_gens C).
Proof.
  intros C x Ix. unfold tm_gens in Ix. apply in_flat_map in Ix as [q [Iq Ix]].
  unfold A.context_gens. apply in_or_app. right. apply in_or_app. left.
  apply in_flat_map. exists q. split; [exact Iq|apply in_or_app; left; exact Ix].
Qed.

Lemma lookup_map_snd : forall (f : ty -> ty) (l : list (nat * ty)) x,
  lookup_subst x (map (fun q => (fst q, f (snd q))) l) = option_map f (lookup_subst x l).
Proof. intros f l x. induction l as [|[a t] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb x a); [reflexivity|exact IH]. Qed.

(* A closure over C's lexical capture reads a symbol in force as the
   table does and one it mentions as its meaning: either way as d reads
   it canonically. *)
Lemma output_meaning_capture : forall d C dc x, AbsorbsCanon C d ->
  A.nlook x (A.c_ty C) <> None \/ In x (capture_gens C) ->
  output_meaning (typed_capture d (A.lexical_capture C) dc) x = Some (ground d (A.canon C (app_subst (A.c_ty C) (TAbs x)))).
Proof.
  intros d C dc x Abs Ix. unfold output_meaning.
  replace (type_aliases (typed_capture d (A.lexical_capture C) dc))
    with (map (fun q => (fst q, ground d (A.canon C (snd q)))) (A.c_ty C)) by (cbn; rewrite map_map; reflexivity).
  rewrite (lookup_map_snd (fun t => ground d (A.canon C t))). cbn [app_subst].
  destruct (lookup_subst x (A.c_ty C)) as [tx|] eqn:Lx; [reflexivity|]. cbn [option_map].
  rewrite typed_capture_meanings.
  destruct Ix as [Ix|Ix]; [exfalso; apply Ix; rewrite <- lookup_subst_nlook'; exact Lx|].
  rewrite (lookup_capture d _ x Ix). f_equal. symmetry. exact (Abs (TAbs x)).
Qed.


Lemma cmeaning_capture : forall d C dc x, AbsorbsCanon C d ->
  A.nlook x (A.c_ty C) <> None \/ In x (capture_gens C) ->
  cmeaning (typed_capture d (A.lexical_capture C) dc) x = ground d (A.canon C (app_subst (A.c_ty C) (TAbs x))).
Proof.
  intros d C dc x Abs Member. unfold cmeaning. now rewrite (output_meaning_capture d C dc x Abs Member).
Qed.

(* An environment without aliases reads a canonical table in its meanings. *)
Lemma alias_none : forall C d, type_aliases d = [] -> AbsorbsCanon C d ->
  (forall k v, A.nlook k (A.c_ty C) = Some v -> v = A.canon C (TAbs k)) -> KeysMeant C d -> AliasOk C d.
Proof.
  intros C d Al Abs Can Km x tx L. rewrite (Can x tx L). rewrite (Abs (TAbs x)).
  unfold output_meaning. rewrite Al. unfold ground. cbn.
  destruct (lookup_subst x (type_meanings d)) eqn:E; [reflexivity|exfalso; exact (Km x tx L E)].
Qed.

Lemma alias_ty : forall C C' d, A.c_ty C' = A.c_ty C -> AliasOk C d -> AliasOk C' d.
Proof. intros C C' d E Al x tx L. rewrite E in L. exact (Al x tx L). Qed.

(* A lambda's environment records the creating context's table in its
   aliases. *)
Lemma alias_capture : forall C d1 dc, AbsorbsCanon C d1 -> AliasOk C (typed_capture d1 (A.lexical_capture C) dc).
Proof.
  intros C d1 dc Abs x tx L. unfold output_meaning. cbn [type_aliases typed_capture A.lexical_capture capture_aliases].
  rewrite map_map. cbn [fst snd].
  rewrite (lookup_map_snd (fun t => ground d1 (A.canon C t))), lookup_subst_nlook', L. cbn [option_map]. f_equal.
  unfold ground at 2. rewrite typed_capture_meanings, ground_capture.
  - exact (Abs tx).
  - intros y Iy. apply cg_l. apply tm_context_gens. unfold tm_gens. apply in_flat_map. exists (x, tx). split; [|exact Iy].
    exact (nlook_in _ _ _ _ L).
Qed.

Lemma lambda_callable : forall k, Below k -> forall jn, jn <= S k ->
  forall C (ps : list (name * option aty)) sg this body pid caps capvals Cl Fl lbody ltype d1 H dc keys,
  List.length ps = List.length (A.sg_params sg) ->
  (forall n, In n caps -> A.slook n (A.c_locals C) <> None /\ ~ In n (map fst ps)) ->
  NoDup caps -> List.length capvals = List.length caps -> NoDup capvals ->
  Cl = fold_left (cl_fold C) (combine caps capvals) (A.Cx (A.c_ty C) (A.c_sub C) (A.c_merge C) (A.c_provisions C) []) ->
  A.fr_ret Fl = Some (A.sg_ret sg) -> A.fr_this Fl = this -> A.fr_loop Fl = false ->
  A.Numbering Fl this (map fst ps) -> Forall (A.Named Fl) caps ->
  LegacyBodyJudgments.BlkE W Prog Fl (fold_left (fun C' q => A.add_local C' (fst q) (snd q) A.LLet) (combine (map fst ps) (A.sg_params sg)) Cl)
       (ABlock [] (Some body)) (Some (A.sg_ret sg)) lbody ltype ->
  A.fits ltype (A.sg_ret sg) ->
  A.body_of Prog pid = Some (fold_right (fun q x => ELet (A.var_of Fl (fst q)) (EVal (snd q)) x) lbody (combine caps capvals)) ->
  (forall k, In k keys <-> (A.provides Cl k /\ (match k with ITy _ => False | _ => True end) /\ k = A.provision_id k)) ->
  select_provisions d1 (map (fun k => (A.rkey_of Cl k, A.rkey_of C k)) keys) = Some dc ->
  Realizes jn C d1 H ->
  (forall q, In q (combine caps capvals) -> exists w, lookup_provision (IVal (snd q)) d1 = Some (ValueProvision w) /\
     value_typed W P H w (ground d1 (capty C (fst q)))) ->
  (forall t, In t (this_list this ++ A.sg_ret sg :: A.sg_params sg)%list -> incl (A.gens t) (A.context_gens C)) ->
  PublicFormation.FormedCx W C -> PublicFormation.SigOk W sg -> PublicFormation.OptOk W this ->
  PublicScoping.LocalsScoped W C -> PublicScoping.ProvsScoped W C ->
  (forall t, In t (this_list this ++ A.sg_ret sg :: A.sg_params sg)%list -> PublicFormation.ScopedTy W C t) ->
  PublicScoping.MethsRead W C ->
  CallableOk jn H d1 (this_list this ++ A.sg_params sg)%list (A.sg_ret sg)
    (FunctionClosure pid (typed_capture d1 (A.lexical_capture C) dc)).
Proof.
  intros k Bel jn Ljn C ps sg this body pid caps capvals Cl Fl lbody ltype d1 H dc keys
    Len CapsIn NdC LenV NdV ClE Ret FThis Loop Num NamedC Blk Fits BodyOf Keys Sel Real Caps SigSc FcC SgOk ThOk LsC PsC SigS MrC.
  set (de := typed_capture d1 (A.lexical_capture C) dc).
  set (qs := combine caps capvals) in *.
  set (K := A.context_gens C).
  assert (G : forall t, incl (A.gens t) K -> ground de t = ground d1 t).
  { intros t Sc. unfold ground at 1. unfold de. rewrite typed_capture_meanings. exact (ground_capture d1 _ t (fun y I => cg_l C y (Sc y I))). }
  assert (Fq : map fst qs = caps) by (apply fst_combine; congruence).
  assert (Sq : map snd qs = capvals) by (apply snd_combine; congruence).
  assert (InCaps : forall q, In q qs -> In (fst q) caps) by (intros q Iq; rewrite <- Fq; apply in_map, Iq).
  assert (Capsr : forall q, In q qs -> exists t mu w, A.slook (fst q) (A.c_locals C) = Some (t, mu) /\
     lookup_provision (IVal (snd q)) d1 = Some (ValueProvision w) /\ value_typed W P H w (ground d1 t)).
  { intros q Iq. destruct (CapsIn _ (InCaps q Iq)) as [L _].
    destruct (A.slook (fst q) (A.c_locals C)) as [[t mu]|] eqn:Lq; [|congruence].
    destruct (Caps q Iq) as [w [Look T]]. unfold capty in T. rewrite Lq in T. exists t, mu, w. split; [reflexivity|split; [exact Look|exact T]]. }
  assert (Ndq : NoDup (map snd qs)) by (rewrite Sq; exact NdV).
  pose proof (lambda_realizes_le jn jn C qs Cl d1 H dc keys (le_n jn) ClE Real Ndq Capsr Keys Sel (proj1 (proj2 FcC))) as RealCl.
  fold de in RealCl.
  cbn [CallableOk]. eexists. split; [change (view_function P pid) with (A.body_of Prog pid); exact BodyOf|].
  intros j Lj H' st ws E Sok Args.
  set (all := (this_list this ++ A.sg_params sg)%list).
  set (types0 := combine (seq 0 (List.length (map (ground de) all))) (map (ground de) all)).
  assert (AllSc : forall t, In t all -> incl (A.gens t) K).
  { intros t It. apply SigSc. unfold all in It. apply in_app_or in It as [It|It]; apply in_or_app; [left; exact It|].
    right. right. exact It. }
  assert (L0 : locals_typed W P H' (parameter_values ws) types0).
  { unfold parameter_values, types0. apply params_typed.
    clear -Args AllSc G. unfold all in *. induction Args as [|w t ws ts T Rest IH]; constructor.
    - rewrite G; [exact T|apply AllSc; left; reflexivity].
    - apply IH. intros t' It'. apply AllSc. right. exact It'. }
  assert (CapsDe : forall q, In q qs -> exists w, lookup_provision (IVal (snd q)) de = Some (ValueProvision w) /\
     value_typed W P H' w (ground de (capty C (fst q)))).
  { intros q Iq. destruct (Capsr q Iq) as [t [mu [w [L [_ _]]]]].
    pose proof RealCl as [_ [[Vals _] _]].
    assert (Lv : A.nlook (snd q) (A.c_vals Cl) = Some t).
    { pose proof RealCl as [_ [_ [_ KMl]]].
      change (A.c_vals Cl) with (vals_of (A.c_provisions Cl)). rewrite (vals_by_ilook _ _ KMl).
      rewrite ClE, (cl_captured C qs _ (fst q) (snd q) t mu Ndq ltac:(destruct q; exact Iq) L).
      reflexivity. }
    destruct (Vals _ t Lv) as [w' [Look T]]. exists w'. split; [exact Look|].
    unfold capty. rewrite L. eapply value_typed_extends; eassumption. }
  apply (lets_call Fl C de H' st (ground d1 (A.sg_ret sg)) lbody qs j (parameter_values ws) types0 L0 CapsDe).
  intros j' g' Lj' Lg'.
  set (capsmap := map (fun q => (A.var_of Fl (fst q), ground de (capty C (fst q)))) qs) in *.
  set (Cb := fold_left (fun C' q => A.add_local C' (fst q) (snd q) A.LLet) (combine (map fst ps) (A.sg_params sg)) Cl) in *.
  assert (Frame : FrameOk Fl) by (apply (numbering_frame Fl (map fst ps)); rewrite FThis; exact Num).
  assert (NamedQ : forall q, In q qs -> A.Named Fl (fst q)) by (intros q Iq; rewrite Forall_forall in NamedC; exact (NamedC _ (InCaps q Iq))).
  assert (NdMap : NoDup (map fst capsmap)).
  { assert (Em : map fst capsmap = map (A.var_of Fl) caps) by (unfold capsmap; rewrite <- Fq, !map_map; reflexivity).
    rewrite Em. apply nodup_map_on; [exact NdC|]. intros x y Ix Iy Exy.
    rewrite Forall_forall in NamedC. exact (var_of_injective Fl x y Frame (NamedC x Ix) (NamedC y Iy) Exy). }
  assert (CapVar : forall x, In x (map fst capsmap) -> exists c, In c caps /\ x = A.var_of Fl c).
  { intros x Ix. unfold capsmap in Ix. rewrite map_map in Ix. apply in_map_iff in Ix as [q [<- Iq]].
    exists (fst q). split; [exact (InCaps q Iq)|reflexivity]. }
  assert (Slot : forall i pn, nth_error (map fst ps) i = Some pn ->
    A.var_of Fl pn = match this with Some _ => S i | None => i end /\ A.Named Fl pn).
  { intros i pn Nth. destruct Num as [_ [_ [_ Positions]]]. pose proof (Positions i pn Nth) as Sl.
    unfold A.var_of, A.Named. rewrite Sl. split; [reflexivity|discriminate]. }
  assert (Pre0 : Pre j' Fl Cb de (rev capsmap ++ types0)%list H' g' st).
  { split; [exact Frame|]. split; [split; [|split; [|apply (alias_ty C);
      [unfold Cb; rewrite add_locals_ty, ClE; exact (proj1 (cl_fields _ _ _))|exact (alias_capture C d1 dc (realizes_absorbs_canon _ _ _ _ Real))]]]|].
    - intros n t mu Found Named.
      destruct (slook_add_locals _ Cl n t mu Found) as [Ip|[Out Here]].
      + apply in_combine_nth in Ip as [i [Name Ty]]. destruct (Slot i n Name) as [Var _].
        rewrite nlook_app_out.
        * rewrite Var. unfold types0.
          assert (Nth : nth_error (map (ground de) all) (match this with Some _ => S i | None => i end) = Some (ground de t)).
          { rewrite nth_error_map. unfold all. destruct this; cbn; rewrite Ty; reflexivity. }
          exact (nlook_positions _ _ _ 0 Nth).
        * rewrite map_rev. intro Ir. apply in_rev in Ir. destruct (CapVar _ Ir) as [c [Ic Ec]].
          rewrite Forall_forall in NamedC.
          pose proof (var_of_injective Fl n c Frame Named (NamedC c Ic) Ec) as <-.
          apply (proj2 (CapsIn n Ic)). apply nth_error_In in Name. exact Name.
      + rewrite ClE in Here. destruct (cl_locals C qs _ n t mu Here) as [[v [mu0 [Iq L]]]|None0]; [|discriminate].
        rewrite nlook_rev_distinct by exact NdMap. apply nlook_app_in; [exact NdMap|].
        unfold capsmap. replace (ground de t) with (ground de (capty C (fst (n, v)))) by (cbn; unfold capty; rewrite L; reflexivity).
        exact (in_map (fun q => (A.var_of Fl (fst q), ground de (capty C (fst q)))) qs (n, v) Iq).
    - intros r This. rewrite nlook_app_out.
      + unfold types0. rewrite FThis in This. subst this.
        exact (nlook_positions (map (ground de) all) 0 (ground de r) 0 ltac:(unfold all; rewrite This; reflexivity)).
      + rewrite map_rev. intro Ir. apply in_rev in Ir. destruct (CapVar _ Ir) as [c [Ic Ec]].
        rewrite Forall_forall in NamedC.
        exact (var_of_not_receiver Fl c Frame ltac:(rewrite This; discriminate) (NamedC c Ic) (eq_sym Ec)).
    - split; [exact Lg'|]. split; [exact Sok|]. split.
      + apply (realizes_same _ Cl); [apply add_locals_ty|apply add_locals_sub|apply add_locals_merge|apply add_locals_provisions|].
        apply (realizes_extends _ _ _ H); [exact E|]. eapply realizes_lower; [|exact RealCl]. lia.
      + split.
        * unfold Cb. apply (PublicFormation.params_formed W Cl (map fst ps) (A.sg_params sg)); [|exact (proj1 SgOk)].
          rewrite ClE. exact (PublicFormation.lambda_cx_formed W C caps capvals FcC).
        * split; [split; [rewrite Ret; apply PublicFormation.optok_some; exact (proj2 SgOk)|rewrite FThis; exact ThOk]|].
          unfold Cb. rewrite ClE. apply (PublicScoping.lambda_cxscoped W C caps capvals Fl (map fst ps) (A.sg_params sg) LsC PsC).
          -- intros t Et. refine (SigS t _). rewrite <- FThis, Et. cbn. left. reflexivity.
          -- intros t It. exact (SigS t ltac:(apply in_or_app; right; right; exact It)).
          -- exact MrC. }
  destruct (Bel j' ltac:(lia)) as [_ [_ [SBlkE _]]].
  pose proof (SBlkE Fl Cb _ _ lbody ltype Blk de _ H' g' st Pre0) as Out.
  assert (Gr : ground de (A.sg_ret sg) = ground d1 (A.sg_ret sg)).
  { apply G. apply SigSc. apply in_or_app. right. left. reflexivity. }
  destruct (AR.O.I.eval j' P de g' st lbody) as [[] g2 st2]; cbn in Out |- *; auto.
  - destruct Out as [H2 [E2 [S2 [_ T]]]]. exists H2; repeat split; auto.
    rewrite <- Gr. exact (fits_ground W P H2 de _ _ _ Fits T).
  - destruct Out as [H2 [r [R [E2 [S2 [_ T]]]]]]. rewrite Ret in R. injection R as <-.
    exists H2; repeat split; auto. rewrite <- Gr. exact T.
  - destruct Out as [_ [L _]]. rewrite Loop in L. discriminate.
Qed.

(* ---- the captured-val prefix and the lambda bind ------------------------------ *)



Lemma Forall2_In_r : forall X Y (R : X -> Y -> Prop) l l' y, Forall2 R l l' -> In y l' -> exists x, In x l /\ R x y.
Proof.
  intros X Y R l l' y F. induction F as [|a b l l' Rab Rest IH]; intros Iy; [contradiction|].
  destruct Iy as [<-|Iy]; [exists a; split; [left; reflexivity|exact Rab]|].
  destruct (IH Iy) as [x [Ix Rx]]. exists x. split; [right; exact Ix|exact Rx].
Qed.

Lemma sigof_scoped : forall s C g, A.SigOf W s (A.c_ty C) None g ->
  forall t, In t (A.sg_ret g :: A.sg_params g) -> incl (A.gens t) (A.context_gens C).
Proof.
  intros s C g (dcl & rcv & n & ps & rt & body & _ & _ & Ps & R) t It x Ix. apply tm_context_gens.
  destruct It as [<-|It].
  - destruct rt as [r|]; [|rewrite R in Ix; contradiction].
    destruct (tyof_gens W _ _ _ _ _ R x Ix) as [Tm|[x0 [E _]]]; [exact Tm|discriminate].
  - destruct (Forall2_In_r _ _ _ _ _ _ Ps It) as [pt [_ T]].
    destruct (tyof_gens W _ _ _ _ _ T x Ix) as [Tm|[x0 [E _]]]; [exact Tm|discriminate].
Qed.

Lemma realizes_install_val : forall n C d H v pv, Realizes n C d H -> A.ilook (IVal v) (A.c_provisions C) = None ->
  Realizes n C (install_provision (IVal v) pv d) H.
Proof.
  intros n C d H v pv [Abs [[Vals [Fns [Mths Funs]]] [Section KM]]] Out.
  split; [exact Abs|]. split; [|exact (conj Section KM)]. split; [|split; [|split]].
  - intros v' t Found. destruct (Vals v' t Found) as [w [Look T]]. exists w. split; [|exact T].
    rewrite install_other; [exact Look|]. intros E. injection E as ->.
    change (A.c_vals C) with (vals_of (A.c_provisions C)) in Found. rewrite (vals_by_ilook _ v KM), Out in Found. discriminate.
  - intros f g Found. destruct (Fns f g Found) as [pv' [Look Ok]]. exists pv'.
    split; [rewrite install_other; [exact Look|discriminate]|exact Ok].
  - intros r m g Found. destruct (Mths r m g Found) as [pv' [Look Ok]]. exists pv'.
    split; [rewrite install_other; [exact Look|unfold A.rkey; discriminate]|exact Ok].
  - intros u fx Found. destruct (Funs u fx Found) as [pv' [gfx [Look Rest]]]. exists pv', gfx.
    split; [rewrite install_other; [exact Look|discriminate]|exact Rest].
Qed.

Definition Installed (C : A.cx) (qs : list (name * vsym)) (d : environment) (H : heap_types) : Prop :=
  forall q, In q qs -> exists w, lookup_provision (IVal (snd q)) d = Some (ValueProvision w) /\
    value_typed W P H w (ground d (capty C (fst q))).

Lemma eval_var_eq : forall j d g st x,
  AR.O.I.eval (S j) P d g st (EVar x) = match lookup_local x g with
    | Some w => AR.O.I.Evaluation (Normal w) g st | None => AR.O.I.Evaluation (Stuck AR.O.I.e_var) g st end.
Proof. reflexivity. Qed.

(* The prefix binds each captured local's value to its val. *)

(* Every key the lambda needs is found where it is bound. *)
Lemma lambda_select : forall n C (qs : list (name * vsym)) Cl d' H keys,
  Cl = fold_left (cl_fold C) qs (A.Cx (A.c_ty C) (A.c_sub C) (A.c_merge C) (A.c_provisions C) []) ->
  Realizes n C d' H -> Installed C qs d' H -> NoDup (map snd qs) ->
  (forall q, In q qs -> A.slook (fst q) (A.c_locals C) <> None) ->
  (forall k, In k keys <-> (A.provides Cl k /\ (match k with ITy _ => False | _ => True end) /\ k = A.provision_id k)) ->
  exists dc, select_provisions d' (map (fun k => (A.rkey_of Cl k, A.rkey_of C k)) keys) = Some dc.
Proof.
  intros n C qs Cl d' H keys ClE [_ [[Vals [Fns [Mths Funs]]] [_ KM]]] Inst Nd Loc Keys.
  apply select_ok. intros q Iq. apply in_map_iff in Iq as [k [<- Ik]]. cbn [snd].
  apply Keys in Ik as [Prov [NotTy Id]].
  assert (Other : forall k, (forall q, In q qs -> k <> IVal (snd q)) -> A.ilook k (A.c_provisions Cl) = A.ilook k (A.c_provisions C)).
  { intros k' Out. rewrite ClE. exact (cl_other C qs _ k' Out). }
  assert (KMl : PublicCallRealize.KeysMatch (A.c_provisions Cl)) by (rewrite ClE; apply cl_keys; exact KM).
  destruct k as [t|v|f|r m th|u]; cbn in Prov, NotTy |- *; try contradiction.
  - destruct (in_dec Nat.eq_dec v (map snd qs)) as [Iv|Ov].
    + apply in_map_iff in Iv as [q [<- Iq]]. destruct (Inst q Iq) as [w [Look _]]. congruence.
    + change (A.c_vals Cl) with (vals_of (A.c_provisions Cl)) in Prov. rewrite (vals_by_ilook _ v KMl) in Prov.
      rewrite Other in Prov by (intros q Iq E; injection E as E; apply Ov; rewrite E; apply in_map, Iq).
      destruct (A.ilook (IVal v) (A.c_provisions C)) as [[t a|g a|g a|fx]|] eqn:Il; try congruence.
      assert (Lv : A.nlook v (A.c_vals C) = Some t)
        by (change (A.c_vals C) with (vals_of (A.c_provisions C)); rewrite (vals_by_ilook _ v KM), Il; reflexivity).
      destruct (Vals v t Lv) as [w [Look _]]. congruence.
  - change (A.c_fns Cl) with (fns_of (A.c_provisions Cl)) in Prov. rewrite (fns_by_ilook _ f KMl) in Prov.
    rewrite Other in Prov by (intros q Iq E; discriminate).
    destruct (A.ilook (IFn f) (A.c_provisions C)) as [[t a|g a|g a|fx]|] eqn:Il; try congruence.
    assert (Lf : A.nlook f (A.c_fns C) = Some g)
      by (change (A.c_fns C) with (fns_of (A.c_provisions C)); rewrite (fns_by_ilook _ f KM), Il; reflexivity).
    destruct (Fns f g Lf) as [pv [Look _]]. congruence.
  - injection Id as ->.
    change (A.c_mths Cl) with (mths_of (A.c_provisions Cl)) in Prov. rewrite (mths_by_ilook _ r m KMl) in Prov.
    rewrite Other in Prov by (intros q Iq E; discriminate).
    destruct (A.ilook (A.mkey r m) (A.c_provisions C)) as [[t a|g a|g a|fx]|] eqn:Il; try congruence.
    assert (Lm : A.mlook r m (A.c_mths C) = Some g)
      by (change (A.c_mths C) with (mths_of (A.c_provisions C)); rewrite (mths_by_ilook _ r m KM), Il; reflexivity).
    destruct (Mths r m g Lm) as [pv [Look _]]. congruence.
  - change (A.c_funs Cl) with (funs_of (A.c_provisions Cl)) in Prov. rewrite (funs_by_ilook _ u KMl) in Prov.
    rewrite Other in Prov by (intros q Iq E; discriminate).
    destruct (A.ilook (IFun u) (A.c_provisions C)) as [[t a|g a|g a|fx]|] eqn:Il; try congruence.
    assert (Lu : A.nlook u (A.c_funs C) = Some fx)
      by (change (A.c_funs C) with (funs_of (A.c_provisions C)); rewrite (funs_by_ilook _ u KM), Il; reflexivity).
    destruct (Funs u fx Lu) as [pv [gfx [Look _]]]. congruence.
Qed.



Lemma method_sig_scoped : forall F C m a bs pa rt sg,
  A.Brackets W (A.fr_mod F) (A.c_ty C) m a bs ->
  A.TyOf W (A.fr_mod F) (bs ++ A.c_ty C)%list None (ATRef pa None) rt ->
  A.SigOf W m (bs ++ A.c_ty C)%list (Some rt) sg ->
  forall t, In t (rt :: A.sg_ret sg :: A.sg_params sg) -> incl (A.gens t) (A.context_gens C).
Proof.
  intros F C m a bs pa rt sg Br Ty Sig.
  assert (Tm : forall x, In x (tm_gens (bs ++ A.c_ty C)%list) -> In x (A.context_gens C)).
  { intros x Ix. apply tm_gens_app in Ix as [Ix|Ix]; apply tm_context_gens; [|exact Ix].
    exact (br_gens W _ _ _ _ _ Br x Ix). }
  assert (Rt : incl (A.gens rt) (A.context_gens C)).
  { intros x Ix. destruct (tyof_gens W _ _ _ _ _ Ty x Ix) as [I'|[x0 [E _]]]; [exact (Tm x I')|discriminate]. }
  destruct Sig as (dcl & rcv & n & ps & r & body & _ & _ & Ps & R).
  intros t [<-|[<-|It]] x Ix; [exact (Rt x Ix)| |].
  - destruct r as [r|]; [|rewrite R in Ix; contradiction].
    destruct (tyof_gens W _ _ _ _ _ R x Ix) as [I'|[x0 [E I']]]; [exact (Tm x I')|injection E as <-; exact (Rt x I')].
  - destruct (Forall2_In_r _ _ _ _ _ _ Ps It) as [pt [_ T]].
    destruct (tyof_gens W _ _ _ _ _ T x Ix) as [I'|[x0 [E I']]]; [exact (Tm x I')|injection E as <-; exact (Rt x I')].
Qed.


(* ---- wrapper providers ------------------------------------------------------ *)

Lemma lookup_keyed_in : forall (f : nat -> ty) (l : list (nat * ty)) x, In x (map fst l) ->
  lookup_subst x (map (fun q => (fst q, f (fst q))) l) = Some (f x).
Proof.
  intros f l x. induction l as [|[a t] r IH]; intros Ix; [contradiction|]. cbn.
  destruct (Nat.eqb_spec x a) as [->|Ne]; [reflexivity|]. destruct Ix as [E|Ix]; [cbn in E; congruence|exact (IH Ix)].
Qed.

Lemma lookup_keyed_out : forall (f : nat -> ty) (l : list (nat * ty)) x, ~ In x (map fst l) ->
  lookup_subst x (map (fun q => (fst q, f (fst q))) l) = None.
Proof.
  intros f l x. induction l as [|[a t] r IH]; intros Ox; [reflexivity|]. cbn.
  destruct (Nat.eqb_spec x a) as [->|Ne]; [exfalso; apply Ox; left; reflexivity|]. apply IH. intro I; apply Ox; right; exact I.
Qed.



(* Calling a symbol from inside its own context, at its own reading of
   each type, leaves every meaning as it was. *)
Lemma self_meanings : forall f Cg ns de, A.CtxOf W f Cg ns -> AbsorbsCanon Cg de ->
  (forall x, ~ In x (map fst (A.c_ty Cg)) -> lookup_subst x (type_meanings de) = None) ->
  forall t, app_subst (callee_meanings de (A.theta_ctx Cg)) t = ground de t.
Proof.
  intros f Cg ns de Ctx Abs Dom t. unfold ground. apply subst_agree_on. intros x _. cbn [app_subst].
  destruct (in_dec Nat.eq_dec x (map fst (A.c_ty Cg))) as [Ix|Ox].
  - assert (Lk : lookup_subst x (callee_meanings de (A.theta_ctx Cg)) =
                 Some (ground de (A.canon Cg (app_subst (A.c_ty Cg) (TAbs x))))).
    { unfold callee_meanings, A.theta_ctx. rewrite map_map.
      exact (lookup_keyed_in (fun x => ground de (A.canon Cg (app_subst (A.c_ty Cg) (TAbs x)))) _ x Ix). }
    rewrite Lk.
    destruct (ctxof_formed W f Cg ns Ctx) as [Form _].
    cbn [app_subst]. rewrite lookup_subst_nlook.
    destruct (keys_in (A.c_ty Cg) x Ix) as [v Lv]. rewrite Lv. rewrite (Form x v Lv).
    change (app_subst (A.c_sub Cg) (TAbs x)) with (A.canon Cg (TAbs x)). rewrite (Abs (A.canon Cg (TAbs x))), (Abs (TAbs x)).
    reflexivity.
  - assert (Lk : lookup_subst x (callee_meanings de (A.theta_ctx Cg)) = None).
    { unfold callee_meanings, A.theta_ctx. rewrite map_map.
      exact (lookup_keyed_out (fun x => ground de (A.canon Cg (app_subst (A.c_ty Cg) (TAbs x)))) _ x Ox). }
    rewrite Lk, (Dom x Ox). reflexivity.
Qed.

(* Realization carries over to an environment with the same meanings and
   the same provisions under the context's keys. *)
Lemma realizes_transfer : forall n C d d' H, Realizes n C d H ->
  (forall t, ground d' t = ground d t) ->
  (forall k p, In (k, p) (A.c_provisions C) -> lookup_provision (A.rkey_of C k) d' = lookup_provision (A.rkey_of C k) d) ->
  Realizes n C d' H.
Proof.
  intros n C d d' H [Abs [[Vals [Fns [Mths Funs]]] [Section KM]]] G Same.
  assert (Gm : forall ps, map (ground d) ps = map (ground d') ps) by (intros ps; apply map_ext; intros; symmetry; apply G).
  split; [intros t u E; rewrite !G; exact (Abs t u E)|]. split; [|exact (conj Section KM)]. split; [|split; [|split]].
  - intros v t Found. destruct (Vals v t Found) as [w [Look T]]. exists w. rewrite G. split; [|exact T].
    destruct (vals_ilook _ v t KM Found) as [a Il]. rewrite <- Look. exact (Same _ _ (ilook_in _ _ _ Il)).
  - intros f g Found. destruct (Fns f g Found) as [pv [Look Ok]]. exists pv. split.
    + destruct (fns_ilook _ f g KM Found) as [a Il]. rewrite <- Look. exact (Same _ _ (ilook_in _ _ _ Il)).
    + exact (callable_transfer n n H d d' _ _ _ _ pv (le_n n) (Gm _) (eq_sym (G _)) Ok).
  - intros r m g Found. destruct (Mths r m g Found) as [pv [Look Ok]]. exists pv. split.
    + destruct (mths_ilook _ r m g KM Found) as [a Il]. rewrite <- Look. exact (Same _ _ (ilook_in _ _ _ Il)).
    + exact (callable_transfer n n H d d' _ _ _ _ pv (le_n n) (Gm _) (eq_sym (G _)) Ok).
  - intros u fx Found. destruct (Funs u fx Found) as [pv [gfx [Look [Ok [Cov Vm]]]]]. exists pv, gfx.
    split; [|split; [exact Ok|split; [exact Cov|intros x tx I; rewrite G; exact (Vm x tx I)]]].
    pose proof (funs_ilook _ u fx KM Found) as Il. rewrite <- Look. exact (Same _ _ (ilook_in _ _ _ Il)).
Qed.

(* Each need of a callee is found in the environment a call builds. *)
Lemma callee_found : forall n C d H f Cf ns sm th dc,
  Realizes n C d H -> A.CtxOf W f Cf ns -> A.Satisfies C Cf th ns sm -> A.CallTheta W C Cf f th ->
  select_provisions d sm = Some dc ->
  forall k, In k ns -> lookup_provision (A.rkey_of Cf k) (callee_env d th dc) <> None.
Proof.
  intros n C d H f Cf ns sm th dc [Abs [[Vals [Fns [Mths Funs]]] [Section KM]]] Ctx Sat CT Sel k Ik.
  destruct Sat as [SmEq [Prov _]].
  destruct (ctx_needs W f Cf ns Ctx) as [Nd [ProvNs Shape]].
  change (lookup_provision (A.rkey_of Cf k) (callee_env d th dc)) with (lookup_provision (A.rkey_of Cf k) dc).
  rewrite (select_lookup d sm dc _ Sel), SmEq. unfold A.satisfaction_map.
  rewrite (sm_find_map (A.rkey_of Cf) (fun k' => A.rkey_of C (A.at_th C th k')) ns k Ik);
    [|intros k' Ik' E; exact (rkey_of_injective Cf k' k (Shape k' Ik') (Shape k Ik) E)].
  pose proof (Prov k Ik) as Pr. pose proof (Shape k Ik) as Sh.
  destruct k as [t|v|f'|r m a|u]; cbn in Sh, Pr |- *; try contradiction.
  - destruct (A.nlook v (A.c_vals C)) as [t|] eqn:L; [|congruence]. destruct (Vals v t L) as [w [Look _]]. congruence.
  - destruct (A.nlook f' (A.c_fns C)) as [g|] eqn:L; [|congruence]. destruct (Fns f' g L) as [pv [Look _]]. congruence.
  - destruct (A.mlook _ m (A.c_mths C)) as [g|] eqn:L; [|congruence]. destruct (Mths _ m g L) as [pv [Look _]]. congruence.
  - destruct (A.nlook u (A.c_funs C)) as [fx|] eqn:L; [|congruence]. destruct (Funs u fx L) as [pv [gfx [Look _]]]. congruence.
Qed.

Lemma vars_args_from : forall i d g st start rest,
  (forall q, q < List.length rest -> lookup_local (start + q) g = nth_error rest q) ->
  AR.O.I.eval_arguments i P d g st (map EVar (seq start (List.length rest))) = inl (rest, g, st) \/
  exists l, AR.O.I.eval_arguments i P d g st (map EVar (seq start (List.length rest))) = inr (AR.O.I.Evaluation OutOfFuel l st).
Proof.
  intros i d g st. induction i as [|i IH]; intros start rest Look; [right; exists g; reflexivity|].
  destruct rest as [|w rest]; [left; reflexivity|]. cbn [List.length seq map AR.O.I.eval_arguments].
  destruct i as [|i]; [right; exists g; reflexivity|].
  pose proof (Look 0 ltac:(cbn; lia)) as L0. rewrite Nat.add_0_r in L0. cbn [nth_error] in L0.
  cbn [AR.O.I.eval]. rewrite L0.
  destruct (IH (S start) rest) as [E|[l E]].
  - intros q Lq. replace (S start + q) with (start + S q) by lia. exact (Look (S q) ltac:(cbn; lia)).
  - rewrite E. left. reflexivity.
  - rewrite E. right. eauto.
Qed.

Lemma eval_call_eq : forall j d g st f th sm args,
  AR.O.I.eval (S j) P d g st (ECall f th sm args) =
  match view_function P f with
  | Some body =>
      match AR.O.I.eval_arguments j P d g st args with
      | inl (ws, g1, st1) =>
          match select_provisions d sm with
          | Some d' => AR.O.I.ret_call g1 (AR.O.I.eval j P (call_environment d th d') (parameter_values ws) st1 body)
          | None => AR.O.I.Evaluation (Stuck AR.O.I.e_compose) g1 st1
          end
      | inr r => r
      end
  | None => AR.O.I.Evaluation (Stuck AR.O.I.e_nobody) g st
  end.
Proof. reflexivity. Qed.

(* A wrapper's closure: the bound fn called from inside its own context. *)
Lemma wrapped_callable : forall k, Below k -> forall jn, jn <= S k ->
  forall C sg g th Cg ns sm w recv d H dc,
  (exists n ps rt b, A.ast_of W g = Some (ADFn RPlain n ps rt (Some b))) ->
  A.CtxOf W g Cg ns -> A.Satisfies C Cg th ns sm -> A.CallTheta W C Cg g th ->
  A.SigOf W g (A.c_ty C) None (A.FSig (map (A.canon C) (A.sg_params sg)) (A.canon C (A.sg_ret sg))) ->
  select_provisions d sm = Some dc ->
  A.body_of Prog w = Some (ECall g (A.theta_ctx Cg) (map (fun k => (A.rkey_of Cg k, A.rkey_of Cg k)) ns)
                                 (map EVar (seq 1 (List.length (A.sg_params sg))))) ->
  Realizes jn C d H ->
  CallableOk jn H d (recv :: A.sg_params sg) (A.sg_ret sg) (FunctionClosure w (typed_capture d (TypeCapture th th) dc)).
Proof.
  intros k Bel jn Ljn C sg g th Cg ns sm w recv d H dc Def Ctx Sat CT Sig Sel BodyW Real.
  pose proof (realizes_absorbs_canon _ _ _ _ Real) as Abs.
  destruct (callee_realizes_le jn jn C d H g Cg ns sm th (le_n jn) Real Ctx Sat CT) as [dc0 [Sel0 [Ag RealG]]].
  rewrite Sel in Sel0. injection Sel0 as <-.
  set (de := typed_capture d (TypeCapture th th) dc).
  assert (RealDe : Realizes jn Cg de H) by exact RealG.
  pose proof (realizes_absorbs_canon _ _ _ _ RealDe) as AbsDe.
  destruct Def as [n0 [ps0 [rt0 [b0 Def]]]]. unfold A.ast_of in Def.
  destruct (A.w_decl W g) as [dcl|] eqn:Decl; [|discriminate]. injection Def as Ast.
  pose proof (all_fn_elab g dcl RPlain n0 ps0 rt0 b0 Decl Ast dcl RPlain n0 ps0 rt0 b0 Decl Ast)
    as (C0 & ns0 & this & g0 & F0 & x & t0 & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & View).
  destruct (ctx_needs W g Cg ns Ctx) as [Nd [ProvNs Shape]].
  pose proof CT as [ThKeys _].
  cbn [CallableOk]. eexists. split; [change (view_function P w) with (A.body_of Prog w); exact BodyW|].
  intros j Lj H' st ws E Sok Args.
  inversion Args as [|w0 t0' ws' ts' Tw0 Tws]; subst.
  destruct j as [|j]; [exact Logic.I|]. rewrite eval_call_eq.
  change (view_function P g) with (A.body_of Prog g). rewrite View.
  assert (Len : List.length ws' = List.length (A.sg_params sg)) by exact (Forall2_length Tws).
  rewrite <- Len.
  destruct (vars_args_from j de (parameter_values (w0 :: ws')) st 1 ws') as [Ea|[l Ea]].
  { intros q Lq. unfold parameter_values.
    exact (lookup_params_from (w0 :: ws') 0 (S q) ltac:(cbn; lia)). }
  2: { rewrite Ea. exact Logic.I. }
  rewrite Ea.
  destruct (select_ok de (map (fun k => (A.rkey_of Cg k, A.rkey_of Cg k)) ns)) as [dc' Sel'].
  { intros q Iq. apply in_map_iff in Iq as [k' [<- Ik]]. cbn [snd].
    exact (callee_found jn C d H g Cg ns sm th dc Real Ctx Sat CT Sel k' Ik). }
  rewrite Sel', (call_environment_callee de _ _ dc' Sel').
  set (dn := callee_env de (A.theta_ctx Cg) dc').
  assert (Gn : forall t, ground dn t = ground de t).
  { intros t. apply (self_meanings g Cg ns de Ctx AbsDe). intros xo Ox.
    change (type_meanings de) with (callee_meanings d th). unfold callee_meanings.
    rewrite lookup_map_snd, lookup_subst_nlook, (keys_out th xo (fun Ix => Ox (eq_rect _ (fun l => In xo l) Ix _ ThKeys))). reflexivity. }
  assert (Same : forall k0 p, In (k0, p) (A.c_provisions Cg) -> lookup_provision (A.rkey_of Cg k0) dn = lookup_provision (A.rkey_of Cg k0) de).
  { intros k0 p I0. pose proof (ProvNs k0 p I0) as Ik.
    change (lookup_provision (A.rkey_of Cg k0) dn) with (lookup_provision (A.rkey_of Cg k0) dc').
    rewrite (select_lookup de _ dc' _ Sel').
    rewrite (sm_find_map (A.rkey_of Cg) (A.rkey_of Cg) ns k0 Ik); [reflexivity|].
    intros k' Ik' Ek. exact (rkey_of_injective Cg k' k0 (Shape k' Ik') (Shape k0 Ik) Ek). }
  assert (RealN : Realizes j Cg dn H').
  { apply (realizes_extends _ _ _ H); [exact E|]. apply (realizes_transfer j Cg de dn H); [|exact Gn|exact Same].
    eapply realizes_lower; [|exact RealDe]. lia. }
  destruct (body_call j (Bel j ltac:(lia)) g dcl n0 ps0 rt0 b0 Cg ns dn H' st ws' d (A.sg_params sg) (A.sg_ret sg)
    Decl Ast Ctx RealN Sok eq_refl (keys_meant_callee Cg de (A.theta_ctx Cg) dc' ltac:(unfold A.theta_ctx; rewrite map_map; reflexivity))) as [body [View' Run]].
  - intros g' Sig'. destruct (sig_rel W _ _ g _ _ None None g' _ Sig' Sig Ag (fun _ _ E' _ => ltac:(discriminate E')))
      as [Ps Rt]. cbn [A.sg_params A.sg_ret] in Ps, Rt. split.
    + transitivity (map (ground de) (A.sg_params g')); [apply map_ext; exact Gn|].
      etransitivity; [exact Ps|]. rewrite map_map. apply map_ext. intros u. apply Abs.
    + rewrite Gn. etransitivity; [exact Rt|]. apply Abs.
  - exact Tws.
  - change (view_function P g) with (A.body_of Prog g) in View'. rewrite View in View'. injection View' as <-.
    apply calloutcome_ret. exact Run.
Qed.



(* A bind item changes only the frame's fresh generations. *)
Lemma bind_item_frame : forall F C it w C' F' m, LegacyBodyJudgments.BindItem W Prog F C it w C' F' m ->
  A.fr_mod F' = A.fr_mod F /\ A.fr_ret F' = A.fr_ret F /\ A.fr_this F' = A.fr_this F /\
  A.fr_loop F' = A.fr_loop F /\ A.fr_var F' = A.fr_var F.
Proof. intros F C it w C' F' m D. destruct D; cbn; auto 10. Qed.

Lemma bind_items_frame : forall F C its w C' F' m, LegacyBodyJudgments.BindItems W Prog F C its w C' F' m ->
  A.fr_mod F' = A.fr_mod F /\ A.fr_ret F' = A.fr_ret F /\ A.fr_this F' = A.fr_this F /\
  A.fr_loop F' = A.fr_loop F /\ A.fr_var F' = A.fr_var F.
Proof.
  intros F C its w C' F' m D. induction D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 D1 Rest IH]; [auto 10|].
  destruct (bind_item_frame _ _ _ _ _ _ _ D1) as [E1 [E2 [E3 [E4 E5]]]].
  destruct IH as [I1 [I2 [I3 [I4 I5]]]]. repeat split; congruence.
Qed.

(* Code safety reads a frame only through its return type, receiver, loop
   flag and local numbering. *)
Lemma codesafe_frame : forall n F1 F2 C t (B : Prop) x,
  A.fr_ret F1 = A.fr_ret F2 -> A.fr_this F1 = A.fr_this F2 -> A.fr_loop F1 = A.fr_loop F2 -> A.fr_var F1 = A.fr_var F2 ->
  CodeSafe n F1 C t B x -> CodeSafe n F2 C t B x.
Proof.
  intros n [m1 r1 t1 l1 v1 f1 a1 p1] [m2 r2 t2 l2 v2 f2 a2 p2] C t B x; cbn; intros <- <- <- <- Safe.
  exact Safe.
Qed.

(* ---- binding-level steps -------------------------------------------------------- *)

(* Two frames that agree on what code safety reads. *)
Definition FrameSame (F F' : A.frame) : Prop :=
  A.fr_ret F' = A.fr_ret F /\ A.fr_this F' = A.fr_this F /\ A.fr_loop F' = A.fr_loop F /\ A.fr_var F' = A.fr_var F.

Lemma outcome_frame : forall F F' d H types t (b : Prop) r, FrameSame F F' ->
  Outcome F' d H types t b r -> Outcome F d H types t b r.
Proof.
  intros [m r1 t1 l1 v1 f1 a1 p1] [m' r2 t2 l2 v2 f2 a2 p2] d H types t b r [Er [Et [El Ev]]]; cbn in *.
  subst. intros Out. exact Out.
Qed.

Lemma pre_frame : forall n F F' C d types H g st, FrameSame F F' ->
  Pre n F C d types H g st -> Pre n F' C d types H g st.
Proof.
  intros n [m r1 t1 l1 v1 f1 a1 p1] [m' r2 t2 l2 v2 f2 a2 p2] C d types H g st [Er [Et [El Ev]]]; cbn in *.
  subst. intros Pre0. exact Pre0.
Qed.

(* The generations a step makes: the meanings grow only by them, and the
   frame's return and receiver types do not mention them. *)
Definition AvoidsTy (made : list nat) (t : ty) : Prop := forall g, In g made -> ~ In g (A.gens t).
Definition FrameAvoids (made : list nat) (F : A.frame) : Prop :=
  (forall t, A.fr_ret F = Some t -> AvoidsTy made t) /\ (forall t, A.fr_this F = Some t -> AvoidsTy made t).
Definition MeaningsGrow (made : list nat) (d d' : environment) : Prop :=
  exists writes, type_meanings d' = (writes ++ type_meanings d)%list /\ forall x, In x (map fst writes) -> In x made.

Lemma meanings_grow_refl : forall made d d', type_meanings d' = type_meanings d -> MeaningsGrow made d d'.
Proof. intros made d d' E. exists []. split; [exact E|intros x []]. Qed.

Lemma meanings_grow_trans : forall m1 m2 d1 d2 d3, MeaningsGrow m1 d1 d2 -> MeaningsGrow m2 d2 d3 ->
  MeaningsGrow (m1 ++ m2)%list d1 d3.
Proof.
  intros m1 m2 d1 d2 d3 [w1 [E1 I1]] [w2 [E2 I2]]. exists (w2 ++ w1)%list. split; [rewrite E2, E1, app_assoc; reflexivity|].
  intros x Ix. rewrite map_app in Ix. apply in_app_or in Ix as [Ix|Ix]; apply in_or_app; [right; exact (I2 x Ix)|left; exact (I1 x Ix)].
Qed.

Lemma ground_grow : forall made d d' t, MeaningsGrow made d d' -> AvoidsTy made t -> ground d' t = ground d t.
Proof.
  intros made d d' t [w [E In']] Av. unfold ground. rewrite E. apply subst_agree_on. intros x Ix. cbn [app_subst].
  rewrite !lookup_subst_nlook, nlook_app'. rewrite <- lookup_subst_nlook.
  destruct (lookup_subst x w) as [u|] eqn:L; [|reflexivity].
  exfalso. apply (Av x (In' x (in_map fst _ _ (lookup_subst_in_pair _ _ _ L))) Ix).
Qed.

Lemma outcome_grow : forall made F d d' H types t (b : Prop) r, MeaningsGrow made d d' -> AvoidsTy made t ->
  FrameAvoids made F -> Outcome F d' H types t b r -> Outcome F d H types t b r.
Proof.
  intros made F d d' H types t b r Grow Av [Ret _] Out. destruct r as [[] g st]; cbn in *; auto.
  - destruct Out as [H' [E [S [L T]]]]. exists H'; repeat split; auto. rewrite <- (ground_grow made d d' t Grow Av). exact T.
  - destruct Out as [H' [ret [R [E [S [L T]]]]]]. exists H', ret; repeat split; auto.
    rewrite <- (ground_grow made d d' ret Grow (Ret ret R)). exact T.
Qed.

(* One binding, run by eval_bindings at fuel at most r - 1, with the
   provisions realized at the fixed index r: either it completes, in the
   context after it, or it stops with an outcome of its own. *)
Definition BindStep (J r : nat) F C F' C' (Bi : Prop) (made : list nat) (b : binding) : Prop :=
  FrameSame F F' /\ FrameAvoids made F /\
  forall j d types H g st, S j <= r -> j < J -> Pre r F C d types H g st -> forall rest,
    (exists d' g' st' H', heap_extends H H' /\ MeaningsGrow made d d' /\
       Pre r F' C' d' types H' g' st' /\
       AR.O.I.eval_bindings (S j) P d g st (b :: rest) = AR.O.I.eval_bindings j P d' g' st' rest) \/
    (exists res, AR.O.I.eval_bindings (S j) P d g st (b :: rest) = inr res /\
       (forall v, AR.O.I.evaluation_signal res <> Normal v) /\ Outcome F d H types TUnit Bi res).

Lemma bindings_copy_eq : forall j d g st u h sm rm r,
  AR.O.I.eval_bindings (S j) P d g st (BCopyFunctor u h sm rm :: r) =
  match lookup_provision (IFun h) d with
  | Some (FunctorClosure c sms dc rms) =>
      AR.O.I.eval_bindings j P (install_provision (IFun u) (FunctorClosure c (compose_adaptation sms sm) dc (compose_positions rms rm)) d) g st r
  | _ => inr (AR.O.I.Evaluation (Stuck AR.O.I.e_fun) g st)
  end.
Proof. reflexivity. Qed.

(* A copy has its source's type outputs: they are the result's type leaves. *)
Lemma spectypes_leaf : forall p sp t, A.SpecTypes W p sp t -> A.ResultSpecLeaf W p sp (t, None).
Proof.
  intros p sp t D. induction D as [p pa a t R K|p pa a c d n its sp t R Dd Ast I D IH].
  - unfold A.kind_sym in K. destruct (A.w_decl W t) as [dt|] eqn:Dt; [|discriminate].
    eapply A.RSLPlain; [exact R|exact Dt| |].
    + intros n its E. rewrite E in K. discriminate.
    + destruct (A.d_ast dt); cbn in K |- *; try discriminate; try reflexivity.
      destruct f; discriminate.
  - eapply A.RSLContext; [exact R|exact Dd|exact Ast|exact I|exact IH].
Qed.

Lemma leaf_spectypes : forall p sp k, A.ResultSpecLeaf W p sp k -> forall t, k = (t, None) ->
  A.kind_sym W t = Some A.DKType -> A.SpecTypes W p sp t.
Proof.
  intros p sp k D. induction D as [p pa br s R|p pa br s d R Dd NC NA|p pa br c d n its sp k R Dd Ast I D IH];
    intros t E K; try discriminate.
  - injection E as <-. eapply A.STType; [exact R|exact K].
  - eapply A.STCtx; [exact R|exact Dd|exact Ast|exact I|exact (IH t E K)].
Qed.

Lemma same_result_outputs : forall u h, A.SameResult W u h -> forall t, A.FunOutputs W u t <-> A.FunOutputs W h t.
Proof.
  assert (One : forall u h, A.SameResult W u h -> forall t, A.FunOutputs W u t -> A.FunOutputs W h t).
  { intros u h Same t (d & ps & a & r & b & sp & D & FP & I & St).
    assert (K : A.kind_sym W t = Some A.DKType).
    { clear -St. induction St; [assumption|assumption]. }
    destruct (A.d_ast d) eqn:Ad; try discriminate. cbn in FP. injection FP as <- <- <- <-.
    assert (Lu : A.ResultLeaf W u (t, None)) by (eapply A.RL; [exact D|exact Ad|exact I|exact (spectypes_leaf _ _ _ St)]).
    pose proof (proj1 (Same (t, None)) Lu) as Lh.
    inversion Lh as [h' d' n' ps' a' r' b' sp' k' D' Ad' I' L']; subst.
    exists d', ps', a', r', b', sp'. split; [exact D'|]. rewrite Ad'. split; [reflexivity|]. split; [exact I'|].
    exact (leaf_spectypes _ _ _ L' t eq_refl K). }
  intros u h Same t. split; [apply One; exact Same|apply One; intros k; symmetry; apply Same].
Qed.

Lemma closure_shape : forall n H u gfx pv, ClosureOk n H u gfx pv -> exists c sms dc rms, pv = FunctorClosure c sms dc rms.
Proof. intros n H u gfx pv Ok. destruct Ok; eauto. Qed.

Lemma nlook_fixed_app : forall fx x, (exists t, In (x, t) fx) ->
  exists t, A.nlook x (A.fixed_app fx) = Some (Some t) /\ In (x, t) fx.
Proof.
  intros fx x [t0 I0]. induction fx as [|[y v] rest IH]; [contradiction|]. cbn.
  destruct (Nat.eqb_spec x y) as [->|Ne]; [exists v; split; [reflexivity|left; reflexivity]|].
  destruct I0 as [E|I0]; [injection E as E; congruence|]. destruct (IH I0) as [t [N I]]. exists t. split; [exact N|right; exact I].
Qed.

(* Item rules not yet proved: one hypothesis per rule, its premises verbatim. *)
Local Close Scope string_scope.
(* BApplyAbstract *)
(* The premises of an abstract application, together. *)
Definition AppPremises F C pa u fx Cu ns sm th gensof C' ks newgens outs : Prop :=
  (A.resolve_sym W (A.fr_mod F) pa = Some u) /\
  (A.nlook u (A.c_funs C) = Some fx) /\
  (A.FunCtx W u Cu ns) /\
  ((forall t tt, A.nlook t (A.c_ty Cu) = Some tt -> exists here, A.nlook t (A.c_ty C) = Some here)) /\
  (th = map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cu)) /\
  ((forall x tx, In (x, tx) fx -> exists here,
        A.nlook x (A.c_ty C) = Some here /\ A.canon C here = A.canon C tx)) /\
  (A.Satisfies C Cu th ns sm) /\
  (A.FreshFor W F C newgens) /\
  ((forall a b, A.FunOutputs W u a -> A.FunOutputs W u b -> gensof a = gensof b -> a = b)) /\
  ((forall t, A.FunOutputs W u t ->
         (A.nlook t (A.c_ty C) = None /\ gensof t = t /\
            ~ In t ((match A.fr_ret F with Some r => A.gens r | None => [] end) ++
                    (match A.fr_this F with Some r => A.gens r | None => [] end)) /\
            ~ In t (A.context_gens C ++ A.key_receiver_gens C)) \/
         ((A.nlook t (A.c_ty C) <> None \/ In t (A.context_gens C ++ A.key_receiver_gens C)) /\
            In (gensof t) newgens))) /\
  (NoDup outs) /\
  ((forall t, In t outs <-> A.FunOutputs W u t)) /\
  (A.ProvideResult W u C (fun t => TAbs (gensof t)) C' ks).

(* One step of a binding, from a given state: it completes or stops. *)
Definition StepOut r F F' C' (Bi : Prop) (made : list nat) (b : binding) j d types H g st : Prop :=
  forall rest,
    (exists d' g' st' H', heap_extends H H' /\ MeaningsGrow made d d' /\
       Pre r F' C' d' types H' g' st' /\
       AR.O.I.eval_bindings (S j) P d g st (b :: rest) = AR.O.I.eval_bindings j P d' g' st' rest) \/
    (exists res, AR.O.I.eval_bindings (S j) P d g st (b :: rest) = inr res /\
       (forall v, AR.O.I.evaluation_signal res <> Normal v) /\ Outcome F d H types TUnit Bi res).

(* A `bind F;` of an abstract F in force, as a step below the cap J. *)
Definition AppAt (J k : nat) : Prop := forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  A.resolve_sym W (A.fr_mod F) pa = Some u -> A.nlook u (A.c_funs C) = Some fx ->
      A.FunCtx W u Cu ns ->
      
      (forall t tt, A.nlook t (A.c_ty Cu) = Some tt -> exists here, A.nlook t (A.c_ty C) = Some here) ->
      th = map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cu) ->
      
      (forall x tx, In (x, tx) fx -> exists here,
        A.nlook x (A.c_ty C) = Some here /\ A.canon C here = A.canon C tx) ->
      A.Satisfies C Cu th ns sm ->
      A.FreshFor W F C newgens ->
      
      (forall a b, A.FunOutputs W u a -> A.FunOutputs W u b -> gensof a = gensof b -> a = b) ->
      
      (forall t, A.FunOutputs W u t ->
         (A.nlook t (A.c_ty C) = None /\ gensof t = t /\
            ~ In t ((match A.fr_ret F with Some r => A.gens r | None => [] end) ++
                    (match A.fr_this F with Some r => A.gens r | None => [] end)) /\
            ~ In t (A.context_gens C ++ A.key_receiver_gens C)) \/
         ((A.nlook t (A.c_ty C) <> None \/ In t (A.context_gens C ++ A.key_receiver_gens C)) /\
            In (gensof t) newgens)) ->
      NoDup outs -> (forall t, In t outs <-> A.FunOutputs W u t) ->
      A.ProvideResult W u C (fun t => TAbs (gensof t)) C' ks ->
  forall rr, rr <= S k -> BindStep J rr F C (A.with_fresh F newgens) C' False (map gensof outs)
      (BApply u th (map (fun t => (t, gensof t)) outs) sm (map (A.rkey_of C') ks)).


(* BFunCopy *)
Lemma case_BFunCopy : forall k, Below k -> forall F C pa u pg h Cu nsu Ch nsh target sm fx rm,
  A.resolve_sym W (A.fr_mod F) pa = Some u -> A.resolve_sym W (A.fr_mod F) pg = Some h ->
      (exists n ps a r, A.ast_of W u = Some (ADFunctor n ps a r None)) ->
      A.nlook h (A.c_funs C) <> None ->
      A.FunCtx W u Cu nsu -> A.FunCtx W h Ch nsh ->
      
      (forall x, A.Fixed W u x -> A.nlook x (A.c_ty C) <> None) ->
      (forall x, A.Fixed W h x -> A.Fixed W u x) ->
      A.FixedAgreement W C h ->
      A.SameResult W u h ->
      sm = map (fun k => (A.rkey_of Ch k, A.rkey_of Cu (target k))) nsh ->
      (forall x tx, In (x, tx) fx <-> A.Fixed W u x /\ A.nlook x (A.c_ty C) = Some tx) ->
      (forall x tx, A.nlook x (A.c_ty Ch) = Some tx -> A.nlook x (A.c_ty Cu) <> None) ->
      A.CopyInterpretation W (map (fun q => (fst q, A.canon C (snd q))) fx) u h Cu nsu nsh Ch rm target ->
  forall J rr, rr <= S k -> BindStep J rr F C F (A.set_fun C u fx) False [] (BCopyFunctor u h sm rm).
Proof.
  intros k _ F C pa u pg h Cu nsu Ch nsh target sm fx rm R1 R2 Abst Hf FCu FCh FixU FixH Agr Same Sm Fx Sub CO J rr _.
  assert (Incl : NeedsAmong target nsh nsu).
  { intros k0 Ik. destruct CO as (outs & gen & ys & cg & Cr & ks & Cr' & ks' & scope_after & _ & _ & _ & _ & Needs & _).
    destruct (Needs k0 Ik) as (p & _ & Match). exact (proj1 Match). }
  split; [repeat split|]. split; [split; intros t0 _ g0 []|].
  intros j d types H g st Lj LJ Pre0 rest. rewrite bindings_copy_eq.
  pose proof Pre0 as [Frame [Ctx [L [St [Real Fmd]]]]].
  pose proof Real as [AbsM [[_ [_ [_ Funs]]] [Section KM]]].
  pose proof (realizes_absorbs_canon _ _ _ _ Real) as Abs.
  destruct (A.nlook h (A.c_funs C)) as [fxh|] eqn:Lh; [|congruence].
  destruct (Funs h fxh Lh) as [pv [gfh [Look [Okh [Covh Vmh]]]]].
  destruct (closure_shape _ _ _ _ _ Okh) as [c [sms [dc [rms ->]]]]. rewrite Look. left.
  set (d' := install_provision (IFun u) (FunctorClosure c (compose_adaptation sms sm) dc (compose_positions rms rm)) d).
  assert (FunFx : forall x t t', In (x, t) fx -> In (x, t') fx -> t = t').
  { intros x t t' I I'. destruct (proj1 (Fx x t) I) as [_ E]. destruct (proj1 (Fx x t') I') as [_ E']. congruence. }
  exists d', g, st, H. split; [intros a0 c0 L0; exact L0|]. split; [apply meanings_grow_refl; reflexivity|]. split; [|reflexivity].
  change (A.set_fun C u fx) with (set_cx C (IFun u) (A.ProvidedFunctor fx)).
  split; [exact Frame|]. split; [exact Ctx|]. split; [exact L|]. split; [exact St|].
  split; [|split; [apply (PublicFormation.set_fun_formed W C u fx (proj1 Fmd)); intros x0 tx0 I0; exact (proj2 (proj1 (Fx x0 tx0) I0))
    |split; [exact (proj1 (proj2 Fmd))|exact (PublicScoping.set_fun_scoped W F C u fx (proj2 (proj2 Fmd)) (fun x0 tx0 I0 => proj2 (proj1 (Fx x0 tx0) I0)))]]].
  apply (realizes_set rr C d H (IFun u) (A.ProvidedFunctor fx)).
  - exact Real.
  - intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
  - reflexivity.
  - intros k0 Ne. apply install_other. exact Ne.
  - exists (FunctorClosure c (compose_adaptation sms sm) dc (compose_positions rms rm)), (ground_fixed (type_meanings d) fx).
    split; [cbn; rewrite Nat.eqb_refl; reflexivity|]. split.
    + subst sm. apply (CCopy rr H u h Cu nsu Ch nsh target c sms dc rms rm _ gfh Okh FCu FCh Incl); [| |exact Sub|exact Same|].
      3: { exists C, fx, fxh, d. split; [exact (proj1 Fmd)|]. split; [exact AbsM|]. split; [exact Section|].
           split; [exact Fx|]. split; [exact CO|]. split; [reflexivity|]. split; [exact Lh|]. split; [exact Agr|].
           split; [exact Covh|]. split; [exact Vmh|exact Abst]. }
      2: { exists (map (fun q => (fst q, A.canon C (snd q))) fx), (type_meanings d). split; [exact CO|].
           unfold ground_fixed. rewrite map_map. apply map_ext. intros [x tx]. cbn. f_equal. symmetry. exact (Abs tx). }
      intros x Fh. pose proof (FixH x Fh) as Fu. split; [exact Fu|].
      destruct (A.nlook x (A.c_ty C)) as [tx|] eqn:Lx; [|exfalso; exact (FixU x Fu Lx)].
      rewrite (lookup_ground_fixed _ fx x tx FunFx (proj2 (Fx x tx) (conj Fu Lx))).
      destruct (nlook_fixed_app fxh x (Covh x Fh)) as [was [Nw Iw]].
      rewrite (Vmh x was Iw). f_equal.
      pose proof (Agr x Fh) as Ag. rewrite (ilook_apps_fun C h fxh (funs_ilook _ h fxh KM Lh)), Lx, Nw in Ag.
      change (ground d was = ground d tx). exact (AbsM was tx Ag).
    + split.
      * intros x Fu. destruct (A.nlook x (A.c_ty C)) as [tx|] eqn:Lx; [|exfalso; exact (FixU x Fu Lx)].
        exists tx. exact (proj2 (Fx x tx) (conj Fu Lx)).
      * intros x tx I. exact (lookup_ground_fixed _ fx x tx FunFx I).
Qed.

(* BFunClosure *)
(* HYPOTHESIS (T29). What a closure captures from where it is bound
   mentions, among G's own inputs that F does not take, only types in
   force there or symbols the context mentions (which the closure's
   `lexical_capture` keeps the meanings of). Rule B for the closure puts
   each such input in force there through G's merge class; a member of
   its class may be in force without the input itself
   (tests/wasi/bind_merge_unbound.moss).
   Proved in PublicLinkMerges (`captured_in_force_ok`). *)
Definition CapturedInForceOk : Prop := forall C gg Cg nsg caps smc,
  A.FunCtx W gg Cg nsg -> PublicFormation.KeysScoped W C -> incl caps nsg ->
  A.Satisfies C Cg (map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cg))
    caps smc ->
  forall k p x, In k caps -> A.ilook (A.provision_id k) (A.c_provisions Cg) = Some p ->
  In x (A.provision_gens p ++ match k with IMth r _ _ => A.gens r | _ => [] end)%list ->
  In x (map fst (A.c_ty Cg)) ->
  A.nlook x (A.c_ty C) <> None \/ In x (capture_gens C).
Hypothesis captured_in_force : CapturedInForceOk.

Lemma bindings_functor_eq : forall j d g st u h types sm smc r,
  AR.O.I.eval_bindings (S j) P d g st (BFunctor u h types sm smc :: r) =
  match select_provisions d smc with
  | Some dc => AR.O.I.eval_bindings j P (install_provision (IFun u) (FunctorClosure h (Some sm) (typed_capture d types dc) None) d) g st r
  | None => inr (AR.O.I.Evaluation (Stuck AR.O.I.e_compose) g st)
  end.
Proof. reflexivity. Qed.

(* What an application reads of a linked body. *)
(* A linked body's result sources: position by position, the body's
   runtime key for each item of F's declared result (LinkBody). *)
Definition link_sources (Cb : A.cx) (outs : list nat) (gen : nat -> nat) (ks : list item) : list item :=
  map (fun k => A.rkey_of Cb (A.at_site Cb k)) ks.

(* The site a linked body's result keys are read at (A.link_site) has
   the body's provisions and merges, so it is realized where the body is,
   and files keys as the body does. *)
Lemma merge_link_site : forall Cg Cb pins outs gen,
  A.c_merge (A.link_site Cg Cb pins outs gen) = A.c_merge Cb.
Proof. reflexivity. Qed.

Lemma realizes_with_link_site : forall rk n Cg Cb pins outs gen d H,
  RealizesWith rk n Cb d H ->
  RealizesWith rk n (A.link_site Cg Cb pins outs gen) d H.
Proof. intros rk n Cg Cb pins outs gen d H R. exact R. Qed.

Lemma realizes_link_site : forall n Cg Cb pins outs gen d H, Realizes n Cb d H -> Realizes n (A.link_site Cg Cb pins outs gen) d H.
Proof. intros n Cg Cb pins outs gen d H. apply realizes_with_link_site. Qed.
Lemma rkey_of_link_site : forall Cg Cb pins outs gen k, A.rkey_of (A.link_site Cg Cb pins outs gen) k = A.rkey_of Cb k.
Proof. reflexivity. Qed.
Lemma canon_link_site : forall Cg Cb pins outs gen t, A.canon (A.link_site Cg Cb pins outs gen) t = A.canon Cb t.
Proof. reflexivity. Qed.

(* An application's selection pairs the caller's keys with the sources. *)
Lemma combine_in_l : forall (rs ss : list item) a b, In (a, b) (combine rs ss) -> In a rs.
Proof.
  induction rs as [|r rs IH]; intros ss a b I; [contradiction|]. destruct ss as [|s ss]; [contradiction|].
  destruct I as [E|I]; [injection E as <- _; left; reflexivity|right; exact (IH ss a b I)].
Qed.
Lemma combine_in_r : forall (rs ss : list item) a b, In (a, b) (combine rs ss) -> In b ss.
Proof.
  induction rs as [|r rs IH]; intros ss a b I; [contradiction|]. destruct ss as [|s ss]; [contradiction|].
  destruct I as [E|I]; [injection E as _ <-; left; reflexivity|right; exact (IH ss a b I)].
Qed.

Definition LinkData (u gg h : nat) dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0 pins : Prop :=
  A.FunCtx W u Cu nsu /\ A.FunCtx W gg Cg nsg /\
  A.w_decl W gg = Some dg /\ A.d_ast dg = ADFunctor n ps a r (Some bodies) /\
  A.fr_mod Fg = A.d_mod dg /\ A.fr_ret Fg = None /\ A.fr_this Fg = None /\ A.fr_loop Fg = false /\
  A.Numbering Fg None [] /\
  LegacyBodyJudgments.BindItems W Prog Fg Cg (List.concat bodies) wrap Cb Fb made /\
  (forall t, In t outs <-> A.FunOutputs W u t) /\
  (forall t, In t outs -> exists meaning, A.nlook t (A.c_ty Cb) = Some meaning) /\
  NoDup (map gen outs) /\ (forall t, In t outs -> A.Fresh W (gen t)) /\
  A.ProvideResult W u Cu (fun t => TAbs (gen t)) Cr ks /\
  A.ResultRealizes (A.link_site Cg Cb pins outs gen) Cr
    (A.role_reads (A.link_site Cg Cb pins outs gen) outs gen ++ A.pin_reads Cb pins) ks /\
  LegacyBodyJudgments.BindsOf (wrap EUnit) binds0 /\
  A.funbody_of Prog h = Some (FunctorCode (map fst (A.c_ty Cg))
      (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Cu))) (map fst (A.c_ty Cg))) (A.link_input_table Cu Cg)
      (map ITy outs) binds0
      (link_sources (A.link_site Cg Cb pins outs gen) outs gen ks)) /\
  (forall t, In t made -> A.nlook t (A.c_ty Cu) = None) /\
  (* Q6: the pins of fixed inputs, and what they mention *)
  (forall x tx, In (x, tx) pins <->
     A.Fixed W u x /\ A.nlook x (A.c_ty Cb) = Some tx /\
     (In x (map fst (A.c_ty Cg)) -> A.nlook x (A.c_ty Cg) <> Some tx)) /\
  (forall x tx y, In (x, tx) pins -> In y (A.gens tx) ->
     In y (map fst (A.c_ty Cg)) /\ lookup_subst y (A.c_sub Cg) = None /\ ~ In y made /\
     (In y (map fst (A.c_ty Cu)) -> A.Fixed W u y)).

Lemma link_facts : forall u gg h pins, LegacyBodyJudgments.LinkBody W Prog u gg h pins -> exists dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0,
  LinkData u gg h dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0 pins.
Proof.
  intros u gg h pins LB. destruct LB as [u gg h dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0 pins
    Dg Ast FCu FCg Mod Ret This Loop Num Pins Items MadeU Outputs _ RR BO Fam [_ [FB _]] PinsI PinG].
  destruct Outputs as (Outs & OutM & Allocated & PR).
  apply AllocationFacts.link_output_allocation_contract in Allocated as [NdG FrG].
  exists dg, n, ps, a, r, bodies, Cu, nsu, Cg, nsg, Fg, wrap, Cb, Fb, made, outs, gen, Cr, ks, binds0.
  unfold LinkData. repeat first [assumption | split].
Qed.

Lemma functx_unique : forall u C ns C' ns', A.FunCtx W u C ns -> A.FunCtx W u C' ns' -> C = C' /\ ns = ns'.
Proof.
  intros u C ns C' ns' (d & ps & a & r & b & es & tm & tm' & σ & classes & implicit & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & E)
    (d' & ps' & a' & r' & b' & es' & tm2 & tm2' & σ' & classes' & implicit' & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & E').
  rewrite E in E'. injection E' as -> ->. split; reflexivity.
Qed.

Lemma lookup_on_keys : forall (f : nat -> ty) (l : list (nat * ty)) x, In x (map fst l) ->
  lookup_subst x (map (fun q => (fst q, f (fst q))) l) = Some (f x).
Proof. exact PublicSubstFacts.lookup_on_keys. Qed.

Lemma lookup_on_keys_some : forall (f : nat -> ty) (l : list (nat * ty)) x, lookup_subst x (map (fun q => (fst q, f (fst q))) l) <> None <->
  In x (map fst l).
Proof.
  intros f l x. split.
  - induction l as [|[a t] r IH]; intros L; cbn in L |- *; [congruence|].
    destruct (Nat.eqb_spec x a) as [->|Ne]; [left; reflexivity|right; exact (IH L)].
  - intros I. rewrite (lookup_on_keys f l x I). discriminate.
Qed.

(* BFunClosure reads F's and G's contexts from the world; LinkBody states
   that they are their FunCtx. *)
Lemma link_body_functx : forall u gg h pins Cu nsu Cg nsg, LegacyBodyJudgments.LinkBody W Prog u gg h pins ->
  A.w_context W u = Some (Cu, nsu) -> A.w_context W gg = Some (Cg, nsg) ->
  A.FunCtx W u Cu nsu /\ A.FunCtx W gg Cg nsg.
Proof.
  intros u gg h pins Cu nsu Cg nsg LB Wu Wg.
  destruct LB as [u0 gg0 h0 dg n ps a r bodies Cu' nsu' Cg' nsg' Fg wrap Cb Fb made outs gen Cr ks binds0 pins0 Dg Ast FCu FCg].
  pose proof (PublicContextFacts.functor_context_record W _ _ _ FCu) as Wu'.
  pose proof (PublicContextFacts.functor_context_record W _ _ _ FCg) as Wg'.
  rewrite Wu in Wu'. rewrite Wg in Wg'. injection Wu' as E1 E2. injection Wg' as E3 E4. subst. split; assumption.
Qed.

Lemma case_BFunClosure : forall k, Below k -> forall F C pa u pg gg Cu nsu Cg nsg h pins linktarget sm smc fx,
  A.resolve_sym W (A.fr_mod F) pa = Some u -> A.resolve_sym W (A.fr_mod F) pg = Some gg ->
      (exists n ps a r, A.ast_of W u = Some (ADFunctor n ps a r None)) ->
      A.FunCtx W u Cu nsu -> A.FunCtx W gg Cg nsg ->
      
      (forall x, A.Fixed W u x -> A.nlook x (A.c_ty C) <> None) ->
      A.SameResult W u gg ->
      A.LinkBinding W u Cu nsu Cg nsg (map (fun q => (fst q, A.canon C (snd q))) fx) (A.link_private_readings C Cu Cg) (A.link_adopted_inputs C Cu Cg) linktarget -> LegacyBodyJudgments.LinkBody W Prog u gg h pins ->
      sm = A.link_map Cu Cg linktarget nsg ->
      A.Satisfies C Cg (map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cg))
        (A.captured_needs linktarget nsg) smc ->
      (forall x tx, In (x, tx) fx <-> A.Fixed W u x /\ A.nlook x (A.c_ty C) = Some tx) ->
      (forall x tx, In (x, tx) pins -> exists here, A.nlook x (A.c_ty C) = Some here /\
         (forall y, In y (A.gens tx) -> A.nlook y (A.c_ty C) <> None) /\
         A.canon C here = A.canon C (app_subst (map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cg)) tx)) ->
  forall J rr, rr <= S k -> BindStep J rr F C F (A.set_fun C u fx) False [] (BFunctor u h (A.lexical_capture C) sm smc).
Proof.
  intros k _ F C pa u pg gg Cu nsu Cg nsg h pins linktarget sm smc fx R1 R2 U FCu FCg FixU Same Binding LB Sm SatC Fx PinA J rr _.
  split; [repeat split|]. split; [split; intros t0 _ g0 []|].
  intros j d types H g st Lj LJ Pre0 rest. rewrite bindings_functor_eq.
  pose proof Pre0 as [Frame [Ctx [L [St [Real Fmd]]]]]. pose proof Ctx as [_ [_ Al]].
  pose proof Real as [_ [[Vals [Fns [Mths Funs]]] [Section KM]]].
  pose proof (realizes_absorbs_canon _ _ _ _ Real) as Abs.
  set (th := map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cg)) in *.
  pose proof SatC as SatC'. set (caps := A.captured_needs linktarget nsg) in *.
  destruct SatC as [SmEq [Prov [Coh [Types Merg]]]].
  destruct (functx_scoped W gg Cg nsg FCg) as [CanG [ScG [KMg [NdG [ProvNsG [ShapeG _]]]]]].
  assert (CapsIn : forall k0, In k0 caps -> In k0 nsg /\ linktarget k0 = None).
  { intros k0 I. exact (proj1 (captured_spec linktarget nsg k0) I). }
  pose proof (@A.link_bind_scope W u Cu nsu Cg nsg _ _ _ linktarget Binding) as B3.
  (* the caller provides every captured need *)
  assert (Caller : forall k0, In k0 caps -> exists pv, lookup_provision (A.rkey_of C (A.at_th C th k0)) d = Some pv).
  { intros k0 Ik. pose proof (Prov k0 Ik) as Pr. pose proof (ShapeG k0 (proj1 (CapsIn k0 Ik))) as Sh.
    destruct k0 as [t|v|f'|r m a|u']; cbn in Sh, Pr |- *; try contradiction.
    - destruct (A.nlook v (A.c_vals C)) as [t|] eqn:L0; [|congruence]. destruct (Vals v t L0) as [w [Look _]]. eauto.
    - destruct (A.nlook f' (A.c_fns C)) as [g0|] eqn:L0; [|congruence]. destruct (Fns f' g0 L0) as [pv [Look _]]. eauto.
    - destruct (A.mlook _ m (A.c_mths C)) as [g0|] eqn:L0; [|congruence]. destruct (Mths _ m g0 L0) as [pv [Look _]]. eauto.
    - destruct (A.nlook u' (A.c_funs C)) as [fx'|] eqn:L0; [|congruence]. destruct (Funs u' fx' L0) as [pv [gfx [Look _]]]. eauto. }
  destruct (select_ok d smc) as [dsel Sel].
  { rewrite SmEq. intros q Iq. apply in_map_iff in Iq as [k0 [<- Ik]]. cbn. destruct (Caller k0 Ik) as [pv ->]. discriminate. }
  rewrite Sel. left.
  set (dc := typed_capture d (A.lexical_capture C) dsel).
  destruct (select_provisions_empty _ _ _ Sel) as [Em Ea].
  assert (Find : forall k0, In k0 caps -> lookup_provision (A.rkey_of Cg k0) dc = lookup_provision (A.rkey_of C (A.at_th C th k0)) d).
  { intros k0 Ik. change (lookup_provision (A.rkey_of Cg k0) dc) with (lookup_provision (A.rkey_of Cg k0) dsel).
    rewrite (select_lookup d smc dsel _ Sel), SmEq. unfold A.satisfaction_map.
    rewrite (sm_find_map (A.rkey_of Cg) (fun k' => A.rkey_of C (A.at_th C th k')) caps k0 Ik); [reflexivity|].
    intros k' Ik' E. exact (rkey_of_injective Cg k' k0 (ShapeG k' (proj1 (CapsIn k' Ik'))) (ShapeG k0 (proj1 (CapsIn k0 Ik))) E). }
  set (gfx := ground_fixed (type_meanings d) fx).
  assert (FunFx : forall x t t', In (x, t) fx -> In (x, t') fx -> t = t').
  { intros x t t' I I'. destruct (proj1 (Fx x t) I) as [_ E]. destruct (proj1 (Fx x t') I') as [_ E']. congruence. }
  assert (Lth : forall x, In x (map fst (A.c_ty Cg)) -> lookup_subst x th = Some (A.canon C (app_subst (A.c_ty C) (TAbs x)))).
  { intros x Ix. exact (lookup_on_keys (fun x => A.canon C (app_subst (A.c_ty C) (TAbs x))) _ x Ix). }
  (* a captured provision's types, read at an application, are the caller's *)
  assert (GroundGm : forall gm, AppMeanings u Cu Cg dc gfx gm -> forall k0 p t, In k0 caps ->
    A.ilook (A.provision_id k0) (A.c_provisions Cg) = Some p ->
    incl (A.gens t) (A.provision_gens p ++ match k0 with IMth r _ _ => A.gens r | _ => [] end)%list ->
    incl (A.gens t) (map fst (A.c_ty Cg)) ->
    app_subst gm t = ground d (A.canon C (app_subst th t))).
  { intros gm [Am1 Am2] k0 p t Ik Il Gp Sc. rewrite (Abs _). unfold ground.
    rewrite <- (subst_through t (type_meanings d) th).
    2: { intros x Ix. rewrite (Lth x (Sc x Ix)). discriminate. }
    apply subst_agree_on. intros x Ix. cbn [app_subst].
    rewrite (lookup_map_snd (app_subst (type_meanings d))).
    rewrite (Lth x (Sc x Ix)). cbn [option_map].
    destruct (in_dec Nat.eq_dec x (map fst (A.c_ty Cu))) as [Iu|Ou].
    2: { pose proof (captured_in_force C gg Cg nsg caps smc FCg (proj1 (proj2 (proj1 Fmd)))
           (fun k I => proj1 (CapsIn k I)) SatC' k0 p x Ik Il (Gp x Ix) (Sc x Ix)) as Captured.
         assert (Present : output_meaning dc x <> None).
         { unfold dc. rewrite (output_meaning_capture d C dsel x Abs Captured). discriminate. }
         rewrite (Am2 x (Sc x Ix) Ou Present). unfold dc.
         now rewrite (cmeaning_capture d C dsel x Abs Captured). }
    assert (Fu : A.Fixed W u x).
    { apply (proj2 (B3 k0 p x (proj1 (CapsIn k0 Ik)) (proj2 (CapsIn k0 Ik)) Il (Gp x Ix))).
      destruct (keys_in _ x Iu) as [v Lv]. rewrite Lv. discriminate. }
    destruct (A.nlook x (A.c_ty C)) as [tx|] eqn:Lx; [|exfalso; exact (FixU x Fu Lx)].
    replace (app_subst (A.c_ty C) (TAbs x)) with tx by (cbn [app_subst]; rewrite (lookup_subst_nlook' x (A.c_ty C)), Lx; reflexivity).
    change (app_subst (type_meanings d) (A.canon C tx)) with (ground d (A.canon C tx)). rewrite (Abs tx).
    rewrite (Am1 x Fu (Sc x Ix)). unfold gfx. rewrite (lookup_ground_fixed _ fx x tx FunFx (proj2 (Fx x tx) (conj Fu Lx))).
    reflexivity. }
  (* Q6: a pin of a fixed input means, under the fixed readings, what the
     bind site's reading of the input does *)
  assert (PinsOk : PinsAgree u Cu Cg dc gfx pins).
  { destruct (link_facts _ _ _ _ LB) as (dg & n0 & ps0 & a0 & rs & bodies & Cu' & nsu' & Cg' & nsg' & Fg & wrap & Cb & Fb & made
      & outs & gen & Cr & ks & binds0 & LD).
    pose proof LD as (FCu2 & FCg2 & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & PinsI & PinG).
    destruct (functx_unique _ _ _ _ _ FCu FCu2) as [E1 E2]. destruct (functx_unique _ _ _ _ _ FCg FCg2) as [E3 E4]. subst Cu' nsu' Cg' nsg'.
    intros gm [Am1 Am2] x tx Ip.
    destruct (proj1 (PinsI x tx) Ip) as [Fx0 _].
    destruct (PinA x tx Ip) as [here [Lh [InF Eh]]].
    unfold gfx. rewrite (lookup_ground_fixed _ fx x here FunFx (proj2 (Fx x here) (conj Fx0 Lh))). f_equal.
    fold gfx. change (ground d here = app_subst gm tx). symmetry.
    rewrite <- (Abs here), Eh, (Abs _). unfold ground.
    rewrite <- (subst_through tx (type_meanings d) th).
    2: { intros y Iy. rewrite (Lth y (proj1 (PinG x tx y Ip Iy))). discriminate. }
    apply subst_agree_on. intros y Iy. cbn [app_subst].
    rewrite (lookup_map_snd (app_subst (type_meanings d))), (Lth y (proj1 (PinG x tx y Ip Iy))). cbn [option_map].
    destruct (PinG x tx y Ip Iy) as [Ig [_ [_ Fy]]].
    destruct (in_dec Nat.eq_dec y (map fst (A.c_ty Cu))) as [Iu|Ou].
    - pose proof (Fy Iu) as Fyy. rewrite (Am1 y Fyy Ig).
      destruct (A.nlook y (A.c_ty C)) as [ty|] eqn:Ly; [|exfalso; exact (FixU y Fyy Ly)].
      replace (app_subst (A.c_ty C) (TAbs y)) with ty by (cbn [app_subst]; rewrite (lookup_subst_nlook' y (A.c_ty C)), Ly; reflexivity).
      change (app_subst (type_meanings d) (A.canon C ty)) with (ground d (A.canon C ty)). rewrite (Abs ty).
      unfold gfx. rewrite (lookup_ground_fixed _ fx y ty FunFx (proj2 (Fx y ty) (conj Fyy Ly))). reflexivity.
    - assert (Present : output_meaning dc y <> None).
      { unfold dc. rewrite (output_meaning_capture d C dsel y Abs (or_introl (InF y Iy))). discriminate. }
      rewrite (Am2 y Ig Ou Present). unfold dc.
      rewrite (cmeaning_capture d C dsel y Abs (or_introl (InF y Iy))). reflexivity. }
  set (d' := install_provision (IFun u) (FunctorClosure h (Some sm) dc None) d).
  exists d', g, st, H. split; [intros a1 c1 L1; exact L1|]. split; [apply meanings_grow_refl; reflexivity|]. split; [|reflexivity].
  change (A.set_fun C u fx) with (set_cx C (IFun u) (A.ProvidedFunctor fx)).
  split; [exact Frame|]. split; [exact Ctx|]. split; [exact L|]. split; [exact St|].
  split; [|split; [apply (PublicFormation.set_fun_formed W C u fx (proj1 Fmd)); intros x0 tx0 I0; exact (proj2 (proj1 (Fx x0 tx0) I0))
    |split; [exact (proj1 (proj2 Fmd))|exact (PublicScoping.set_fun_scoped W F C u fx (proj2 (proj2 Fmd)) (fun x0 tx0 I0 => proj2 (proj1 (Fx x0 tx0) I0)))]]].
  apply (realizes_set rr C d H (IFun u) (A.ProvidedFunctor fx)).
  - exact Real.
  - intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
  - reflexivity.
  - intros k0 Ne. apply install_other. exact Ne.
  - exists (FunctorClosure h (Some sm) dc None), gfx. split; [cbn; rewrite Nat.eqb_refl; reflexivity|]. split.
    + assert (ReadBinding : ClosureBinding W u Cu nsu Cg nsg linktarget dc gfx).
      { set (private := A.link_private_readings C Cu Cg) in Binding.
        destruct Binding as [Scope (ys & cg & after & Allocated & Residual & Choices & Inputs)].
        pose proof (proj1 (AllocationFacts.copy_allocation_contract W _ [] (fun x => x) ys cg) (proj1 Allocated))
          as [Fresh [Avoid _]].
        cbn in Fresh, Avoid. rewrite flat_map_app in Avoid.
        assert (FixedAt : forall T, (forall x, A.Fixed W u x -> lookup_subst x gfx = Some (app_subst T (TAbs x))) ->
          forall x t, In (x,t) (map (fun q => (fst q, A.canon C (snd q))) fx) ->
          (forall z, In z (A.gens t) -> ~ In z (map cg ys)) /\
          app_subst (type_meanings d) t = app_subst T (TAbs x)).
        { intros T Fix x t Member. split.
          - intros z InT. apply Avoid, in_or_app. left. apply in_flat_map. exists (x,t). split; assumption.
          - exact (proj2 (recorded_fixed_meaning u C fx d gfx T x t Fx Abs eq_refl Fix Member)). }
        assert (RawInputs : forall T, ReadsMerge Cu (app_subst T) ->
          (forall x, A.Fixed W u x -> lookup_subst x gfx = Some (app_subst T (TAbs x))) ->
          forall x tx, A.nlook x (A.c_ty Cg) = Some tx -> A.nlook x (A.c_ty Cu) <> None ->
          app_subst T (TAbs x) = app_subst (link_base_reading Cu Cg T dc)
            (app_subst (A.link_adopted_inputs C Cu Cg) tx)).
        { intros T Reads Fix x tx Found Shared.
          destruct (Inputs x tx Found Shared) as [Supported Equal].
          set (G := link_base_reading Cu Cg T dc).
          set (reading := PublicCopySubst.cg_read ys cg T (type_meanings d)).
          assert (Separate : forall y, In y (map fst private) -> ~ In y (map fst (A.c_ty Cu))).
          { intros y Private Public. destruct (keys_in _ y Private) as [t Lookup].
            rewrite <- lookup_subst_nlook' in Lookup.
            destruct (PublicCopySubst.private_link_row C Cu Cg y t Lookup) as [_ [Absent _]].
            destruct (keys_in _ y Public) as [ty Present]. congruence. }
          assert (PrivateAt : forall y t, lookup_subst y private = Some t ->
            (forall z, In z (A.gens t) -> ~ In z (map cg ys)) /\ lookup_subst y G = Some (app_subst (type_meanings d) t)).
          { intros y t Lookup. split.
            - intros z Member. apply Avoid, in_or_app. right. apply in_flat_map.
              exists (y,t). split; [exact (lookup_subst_in_pair _ _ _ Lookup)|exact Member].
            - destruct (PublicCopySubst.private_link_row C Cu Cg y t Lookup) as [Ig [Absent [raw [Here ->]]]].
              unfold G. rewrite (link_base_reading_private Cu Cg T dc y); [| |exact Ig].
              + unfold dc. rewrite (cmeaning_capture d C dsel y Abs) by (left; rewrite Here; discriminate).
                cbn [app_subst]. rewrite lookup_subst_nlook', Here. reflexivity.
              + intros InCu. destruct (keys_in _ y InCu) as [ty Present]. congruence. }
          assert (Target : app_subst reading
            (app_subst (A.copy_subst (A.copy_fixed_readings Cu (map (fun q => (fst q, A.canon C (snd q))) fx)) ys cg)
              (A.canon Cu (TAbs x))) = app_subst T (TAbs x)).
          { transitivity (app_subst T (A.canon Cu (TAbs x))).
            - eapply PublicCopySubst.site_read_composition; [exact FCu|exact Reads|exact Fresh| |exact (FixedAt T Fix)|].
              + intros y Leaf Missing. apply Residual. exact (conj Leaf Missing).
              + intros y [<-|[]]. destruct (A.nlook x (A.c_ty Cu)) as [v|] eqn:Lookup; [|contradiction].
                exact (in_map fst _ (x,v) (PublicTypeRead.nlook_in _ _ _ _ Lookup)).
            - apply (proj1 (reads_merge_canon_iff Cu _ (proj2 (functor_context_merge W u Cu nsu FCu))) Reads). }
          rewrite <- Target, Equal. unfold G.
          eapply (PublicCopySubst.read_through_private_capture W u Cu nsu Cg); [exact FCu|exact Reads|exact Fresh| |exact (FixedAt T Fix)|exact Separate|exact PrivateAt| | |].
          + intros y Leaf Missing. apply Residual. exact (conj Leaf Missing).
          + intros y InCu InG. change (app_subst G (TAbs y) = app_subst T (TAbs y)).
            unfold G at 1. change ((match lookup_subst y (link_base_reading Cu Cg T dc) with
              | Some v => v | None => TAbs y end) = app_subst T (TAbs y)).
            now rewrite (link_base_reading_shared Cu Cg T dc y InCu InG).
          + apply PublicLinkAnchors.adoption_keeps_source_scope.
            intros y Member. pose proof (PublicCopySubst.canon_input_support W gg Cg nsg FCg x) as ScopeG.
            destruct (functx_scoped W gg Cg nsg FCg) as [Row _]. rewrite (Row x tx Found) in Member.
            apply ScopeG; [exact (in_map fst _ (x,tx) (PublicTypeRead.nlook_in _ _ _ _ Found))|exact Member].
          + intros y Member. apply in_or_app. destruct (Supported y Member) as [SharedY|PrivateY].
            * left. destruct (A.nlook y (A.c_ty Cu)) as [v|] eqn:Lookup; [|contradiction].
              exact (in_map fst _ (y,v) (PublicTypeRead.nlook_in _ _ _ _ Lookup)).
            * right. destruct (lookup_subst y private) as [v|] eqn:Lookup; [|contradiction].
              exact (in_map fst _ (y,v) (lookup_subst_in_pair _ _ _ Lookup)). }
        assert (ReadAfter : forall T, ReadsMerge Cu (app_subst T) ->
          (forall x, A.Fixed W u x -> lookup_subst x gfx = Some (app_subst T (TAbs x))) ->
          forall t, incl (A.gens t) (map fst (A.c_ty Cg)) ->
          app_subst (link_base_reading Cu Cg T dc) (app_subst (A.link_adopted_inputs C Cu Cg) t) =
          app_subst (link_reading Cu Cg T dc) t).
        { intros T Reads Fix. eapply PublicCopySubst.link_reads_after_adoption; [exact FCg|exact (RawInputs T Reads Fix)]. }
        constructor.
        - intros need key Member Chosen. destruct (Choices need Member) as (p & _ & Choice).
          rewrite Chosen in Choice. exact (proj1 (proj1 (proj2 (proj2 Choice)))).
        - intros T Reads Fix x tx Found Shared. rewrite (RawInputs T Reads Fix x tx Found Shared).
          apply ReadAfter; [exact Reads|exact Fix|].
          intros y Member. pose proof (PublicCopySubst.canon_input_support W gg Cg nsg FCg x) as ScopeG.
          rewrite (CanG x tx Found) in Member.
          apply ScopeG; [exact (in_map fst _ (x,tx) (PublicTypeRead.nlook_in _ _ _ _ Found))|exact Member].
        - intros T Reads Fix y InG Private Present.
          assert (NotAdopted : lookup_subst y (A.link_adopted_inputs C Cu Cg) = None).
          { destruct (lookup_subst y (A.link_adopted_inputs C Cu Cg)) as [value|] eqn:Adopted; [|reflexivity].
            exfalso. apply Present. unfold dc. eapply PublicLinkAnchors.lexical_capture_has_no_adopted_input; exact Adopted. }
          pose proof (ReadAfter T Reads Fix (TAbs y) ltac:(intros x [<-|[]]; exact InG)) as Reading.
          cbn [app_subst] in Reading. rewrite NotAdopted in Reading. cbn [app_subst] in Reading.
          rewrite (link_base_reading_private Cu Cg T dc y Private InG), (link_reading_lookup Cu Cg T dc y InG) in Reading.
          rewrite (link_reading_lookup Cu Cg T dc y InG). f_equal. symmetry. exact Reading.
        - intros T Reads Fix need p Member Lookup key Chosen.
          destruct (Choices need Member) as [p0 [Read Choice]]. rewrite Lookup in Read. injection Read as <-.
          set (adopted := A.link_adopted_inputs C Cu Cg).
          set (need' := app_subst_item adopted need). set (p' := A.provision_subst adopted p).
          fold adopted need' p' in Choice.
          rewrite Chosen in Choice. destruct Choice as [InU [InG [[_ [At _]] TargetScope]]].
          set (G := link_reading Cu Cg T dc).
          assert (Atom : forall y, In y (A.link_need_gens need' p') -> app_subst G (TAbs y) = app_subst T (TAbs y)).
          { intros y Occurs. unfold G. cbn [app_subst]. rewrite link_reading_shared; [reflexivity| |].
            - destruct (A.nlook y (A.c_ty Cu)) as [v|] eqn:Found; [|exfalso; exact (InU y Occurs Found)].
              exact (in_map fst _ (y,v) (PublicTypeRead.nlook_in _ _ _ _ Found)).
            - destruct (A.nlook y (A.c_ty Cg)) as [v|] eqn:Found; [|exfalso; exact (InG y Occurs Found)].
              exact (in_map fst _ (y,v) (PublicTypeRead.nlook_in _ _ _ _ Found)). }
          destruct (PublicCopySubst.site_need_read W u Cu nsu _ ys cg T (type_meanings d) FCu Reads Fresh
            (fun y Leaf Missing => proj2 (Residual y) (conj Leaf Missing)) (FixedAt T Fix) need' p' key InU At TargetScope)
            as [q [Q [Key Provision]]].
          exists q. split; [exact Q|]. split.
          + rewrite <- Key. unfold need'. destruct need as [t|v|f|r m a|h0]; try reflexivity.
            change (A.mkey (app_subst G r) m = A.mkey (app_subst T (app_subst adopted r)) m).
            f_equal. transitivity (app_subst G (app_subst adopted r)).
            * symmetry. unfold G, adopted. apply link_reading_adopted.
            * apply subst_agree_on. intros y Occurs. apply Atom, in_or_app. right. exact Occurs.
          + rewrite <- Provision. transitivity (A.provision_subst G p').
            * symmetry. unfold G, p', adopted. apply PublicCopySubst.link_provision_after_adoption.
            * rewrite <- (PublicCopySubst.provision_subst_nil p') at 1.
              apply PublicCopySubst.provision_agree. intros y Occurs. rewrite app_subst_nil.
              apply Atom, in_or_app. now left. }
      subst sm. apply (CLink rr H u gg h Cu nsu Cg nsg linktarget dc gfx pins LB ReadBinding FCu FCg).
      { intros x Fx'. exact (fixed_in_functx u Cu nsu x U FCu Fx'). }
      { exact U. }
      * intros gm Am. split; [|split].
        -- (* vals *) intros v t Ik Lt.
           destruct (vals_ilook _ v t KMg Lt) as [a Il]. pose proof (ilook_in _ _ _ Il) as InP.
           destruct (Types (IVal v) _ Ik Il) as [p' [Il' Sig]]. cbn in Il'.
           pose proof (KM _ _ (ilook_in _ _ _ Il')) as Kind.
           destruct p' as [tc a'|g' a'|g' a'|fx']; cbn in Kind; try contradiction.
           destruct (Vals v tc (ilook_vals _ v tc a' KM Il')) as [w [Look T]].
           exists w. split; [pose proof (Find (IVal v) Ik) as F'; cbn [A.rkey_of A.at_th] in F'; rewrite F'; exact Look|].
           assert (Sig' : A.sg_ret (A.canon_sig C (A.FSig [] tc)) = A.sg_ret (A.sig_at C th (A.FSig [] t)))
             by (cbn [option_map A.provision_signature] in Sig; congruence).
           change (A.canon C tc = A.canon C (app_subst th t)) in Sig'.
           rewrite (GroundGm gm Am (IVal v) _ t Ik Il).
           ++ rewrite <- Sig', (Abs tc). exact T.
           ++ intros x Ix. cbn. rewrite app_nil_r. apply in_or_app. left. rewrite app_nil_r. exact Ix.
           ++ intros x Ix. destruct (ScG _ _ InP) as [Sc _]. apply Sc. unfold provision_sig_gens, A.sig_gens. cbn. rewrite app_nil_r. exact Ix.
        -- (* fns *) intros f gf Ik Lf.
           destruct (fns_ilook _ f gf KMg Lf) as [a Il]. pose proof (ilook_in _ _ _ Il) as InP.
           destruct (Types (IFn f) _ Ik Il) as [p' [Il' Sig]]. cbn in Il'.
           pose proof (KM _ _ (ilook_in _ _ _ Il')) as Kind.
           destruct p' as [tc a'|gc a'|gc a'|fx']; cbn in Kind; try contradiction.
           destruct (Fns f gc (ilook_fns _ f gc a' KM Il')) as [pv [Look Ok]].
           exists pv. split; [pose proof (Find (IFn f) Ik) as F'; cbn [A.rkey_of A.at_th] in F'; rewrite F'; exact Look|].
           assert (Sig' : A.canon_sig C gc = A.sig_at C th gf) by (cbn [option_map A.provision_signature] in Sig; congruence).
           destruct (ScG _ _ InP) as [Sc _]. unfold provision_sig_gens in Sc. cbn in Sc.
           assert (Gt : forall t, In t (A.sg_ret gf :: A.sg_params gf) -> app_subst gm t = ground d (A.canon C (app_subst th t))).
           { intros t It. apply (GroundGm gm Am (IFn f) _ t Ik Il).
             - intros x Ix. cbn. rewrite app_nil_r. apply in_or_app. left.
               destruct It as [<-|It]; [apply in_or_app; left; exact Ix|].
               apply in_or_app. right. apply in_flat_map. exists t. split; [exact It|exact Ix].
             - intros x Ix. apply Sc. unfold A.sig_gens. destruct It as [<-|It]; [apply in_or_app; left; exact Ix|].
               apply in_or_app. right. apply in_flat_map. exists t. split; [exact It|exact Ix]. }
           destruct gc as [psc rc], gf as [psf rf]. unfold A.canon_sig, A.sig_at in Sig'. cbn in Sig', Gt. injection Sig' as Ep Er.
           apply (callable_transfer rr rr H d (denv gm) psc psf rc rf pv (le_n _)); [| |exact Ok].
           ++ transitivity (map (fun t => ground d (A.canon C t)) psc); [apply map_ext; intros t; symmetry; apply Abs|].
              rewrite <- map_map, Ep, map_map. apply map_ext_in. intros t It. symmetry. exact (Gt t (or_intror It)).
           ++ transitivity (ground d (A.canon C rc)); [symmetry; apply Abs|].
              change (A.canon C rc) with (app_subst (A.c_sub C) rc). rewrite Er. symmetry. exact (Gt rf (or_introl eq_refl)).
        -- (* methods *) intros r m a gf Ik Lm.
           destruct (mths_ilook _ r m gf KMg Lm) as [a0 Il]. pose proof (ilook_in _ _ _ Il) as InP.
           pose proof (ShapeG _ (proj1 (CapsIn _ Ik))) as Sh. cbn in Sh. subst a.
           destruct (Types (A.mkey r m) _ Ik Il) as [p' [Il' Sig]]. cbn in Il'.
           pose proof (KM _ _ (ilook_in _ _ _ Il')) as Kind.
           destruct p' as [tc a'|gc a'|gc a'|fx']; cbn in Kind; try contradiction.
           destruct (Mths _ m gc (ilook_mths _ _ m gc a' KM Il')) as [pv [Look Ok]].
           exists pv. split; [pose proof (Find (A.mkey r m) Ik) as F'; cbn [A.rkey_of A.at_th A.mkey] in F'; rewrite F'; exact Look|].
           assert (Sig' : A.canon_sig C gc = A.sig_at C th gf) by (cbn [option_map A.provision_signature] in Sig; congruence).
           destruct (ScG _ _ InP) as [Sc [Kr _]]. unfold provision_sig_gens in Sc. cbn in Sc, Kr.
           assert (Gt : forall t, In t (r :: A.sg_ret gf :: A.sg_params gf) -> app_subst gm t = ground d (A.canon C (app_subst th t))).
           { intros t It. apply (GroundGm gm Am (A.mkey r m) _ t Ik Il).
             - intros x Ix. cbn. destruct It as [<-|It]; [apply in_or_app; right; exact Ix|]. apply in_or_app. left.
               apply in_or_app. left. destruct It as [<-|It]; [apply in_or_app; left; exact Ix|].
               apply in_or_app. right. apply in_flat_map. exists t. split; [exact It|exact Ix].
             - intros x Ix. destruct It as [<-|It]; [exact (Kr x Ix)|]. apply Sc. unfold A.sig_gens.
               destruct It as [<-|It]; [apply in_or_app; left; exact Ix|].
               apply in_or_app. right. apply in_flat_map. exists t. split; [exact It|exact Ix]. }
           destruct gc as [psc rc], gf as [psf rf]. unfold A.canon_sig, A.sig_at in Sig'. cbn in Sig', Gt. injection Sig' as Ep Er.
           apply (callable_transfer rr rr H d (denv gm) (A.canon C (app_subst th r) :: psc) (r :: psf) rc rf pv (le_n _)); [| |exact Ok].
           ++ cbn [map]. f_equal; [symmetry; exact (Gt r (or_introl eq_refl))|].
              transitivity (map (fun t => ground d (A.canon C t)) psc); [apply map_ext; intros t; symmetry; apply Abs|].
              rewrite <- map_map, Ep, map_map. apply map_ext_in. intros t It. symmetry. exact (Gt t (or_intror (or_intror It))).
           ++ transitivity (ground d (A.canon C rc)); [symmetry; apply Abs|].
              change (A.canon C rc) with (app_subst (A.c_sub C) rc). rewrite Er. symmetry. exact (Gt rf (or_intror (or_introl eq_refl))).
      * intros gm Am.
        assert (Gen : forall l, incl l caps -> CapturedFuns rr H gm dc (captured_funs Cg l)).
        { induction l as [|k0 l IH]; intros Inc; [constructor|].
          assert (Ik : In k0 caps) by (apply Inc; left; reflexivity).
          specialize (IH (fun x Ix => Inc x (or_intror Ix))).
          destruct k0 as [t|v|f|r m a|u']; cbn [captured_funs flat_map]; try exact IH.
          destruct (A.nlook u' (A.c_funs Cg)) as [fx'|] eqn:Lu; [|exact IH]. cbn [app].
          pose proof (funs_ilook _ u' fx' KMg Lu) as Il. pose proof (ilook_in _ _ _ Il) as InP.
          pose proof (Prov (IFun u') Ik) as Pr. cbn in Pr.
          destruct (A.nlook u' (A.c_funs C)) as [fxc|] eqn:Lc; [|congruence].
          destruct (Funs u' fxc Lc) as [pv [gfc [Look [Ok [_ Vm]]]]].
          apply (CFCons rr H gm dc u' fx' pv gfc); [|exact Ok| |exact IH].
          - pose proof (Find (IFun u') Ik) as F'. cbn [A.rkey_of A.at_th] in F'. rewrite F'. exact Look.
          - intros x tx Ixt.
            assert (Ia : A.ilook (IFun u') (A.c_apps Cg) = Some (A.fixed_app fx')) by (apply ilook_apps_fun; exact Il).
            assert (Ic : A.ilook (IFun u') (A.c_apps C) = Some (A.fixed_app fxc)) by (apply ilook_apps_fun; exact (funs_ilook _ u' fxc KM Lc)).
            pose proof (Coh (IFun u') _ Ik Ia x tx (fixed_app_in _ _ _ Ixt)) as Have. cbn [A.at_th] in Have. rewrite Ic in Have.
            apply fixed_app_out in Have. rewrite (Vm x _ Have). f_equal. symmetry.
            change (ground d (A.canon C (app_subst th tx))) with (ground d (A.canon C (app_subst th tx))).
            apply (GroundGm gm Am (IFun u') _ tx Ik Il).
            + intros y Iy. cbn. rewrite app_nil_r. apply in_flat_map. exists (x, Some tx). split; [exact (fixed_app_in _ _ _ Ixt)|exact Iy].
            + destruct (ScG _ _ InP) as [_ [_ Sf]]. exact (Sf x tx Ixt). }
        apply Gen. intros x Ix. exact Ix.
      * exact PinsOk.
    + split.
      * intros x Fu. destruct (A.nlook x (A.c_ty C)) as [tx|] eqn:Lx; [|exfalso; exact (FixU x Fu Lx)].
        exists tx. exact (proj2 (Fx x tx) (conj Fu Lx)).
      * intros x tx I. exact (lookup_ground_fixed _ fx x tx FunFx I).
Qed.


Local Open Scope string_scope.

(* ---- applications: the body's entry ---------------------------------------- *)

Lemma lookup_append_env : forall k a b,
  lookup_provision k (append_environment a b) =
  match lookup_provision k a with Some v => Some v | None => lookup_provision k b end.
Proof.
  intros k [ma aa pa] b. unfold lookup_provision, append_environment. cbn.
  induction pa as [|[k' v'] r IH]; cbn; [reflexivity|]. destruct (item_eqb k k'); [reflexivity|exact IH].
Qed.

Lemma lookup_map_nat : forall (f : nat -> ty) l x, In x l -> lookup_subst x (map (fun s => (s, f s)) l) = Some (f x).
Proof.
  intros f l x. induction l as [|a r IH]; intros I; [contradiction|]. cbn.
  destruct (Nat.eqb_spec x a) as [->|Ne]; [reflexivity|]. destruct I as [E|I]; [congruence|exact (IH I)].
Qed.

Lemma captured_funs_in : forall Cg caps u fx, In (IFun u) caps -> A.nlook u (A.c_funs Cg) = Some fx ->
  In (u, fx) (captured_funs Cg caps).
Proof.
  intros Cg caps u fx I L. unfold captured_funs. apply in_flat_map. exists (IFun u). split; [exact I|]. rewrite L. left. reflexivity.
Qed.

Lemma captured_funs_find : forall n H gm dc l, CapturedFuns n H gm dc l -> forall u fx, In (u, fx) l ->
  exists pv gfx, lookup_provision (IFun u) dc = Some pv /\ ClosureOk n H u gfx pv /\
    (forall x tx, In (x, tx) fx -> lookup_subst x gfx = Some (app_subst gm tx)).
Proof.
  intros n H gm dc l CF. induction CF as [gm dc|gm dc u fx pv gfx rest Look Ok Fx Rest IH]; intros u' fx' I; [contradiction|].
  destruct I as [E|I]; [injection E as <- <-; exists pv, gfx; auto|exact (IH u' fx' I)].
Qed.

Lemma in_keys_dec : forall (k : item) (l : list item), {In k l} + {~ In k l}.
Proof.
  intros k l. destruct (existsb (item_eqb k) l) eqn:E.
  - left. apply existsb_exists in E as [k' [I E]]. apply item_eqb_spec in E. subst. exact I.
  - right. intros I. rewrite (proj2 (existsb_exists _ _) (ex_intro _ k (conj I (proj2 (item_eqb_spec _ _) eq_refl)))) in E. discriminate.
Qed.

Lemma subst_abs : forall s x, app_subst s (TAbs x) = match lookup_subst x s with Some t => t | None => TAbs x end.
Proof. reflexivity. Qed.

Lemma lookup_map_out : forall (f : nat -> ty) l x, ~ In x l -> lookup_subst x (map (fun s => (s, f s)) l) = None.
Proof.
  intros f l x. induction l as [|a r IH]; intros O; [reflexivity|]. cbn.
  destruct (Nat.eqb_spec x a) as [->|Ne]; [exfalso; apply O; left; reflexivity|]. apply IH. intros I. apply O. right. exact I.
Qed.

(* The types a provision mentions. *)
Definition prov_types (p : A.provision) : list ty :=
  match p with
  | A.ProvidedVal t _ => [t]
  | A.ProvidedFn g _ | A.ProvidedMethod g _ => A.sg_ret g :: A.sg_params g
  | A.ProvidedFunctor fx => map snd fx
  end.

(* A functor context's provisions are canonical: read through σ. *)
Lemma functx_canon_prov : forall u C ns, A.FunCtx W u C ns -> forall k p, In (k, p) (A.c_provisions C) ->
  (forall t, In t (prov_types p) -> A.canon C t = t) /\ (forall r m a, k = IMth r m a -> A.canon C r = r).
Proof.
  intros u C ns (dd & ps & a & r & b & es & tm & tm' & σ & classes & implicit & D & FP & IE & Imp & Etm & MGM & _ & _ & EC & _ & _) k p Ik.
  subst C. destruct MGM as [_ [Idem _]]. unfold A.canon. cbn [A.c_sub A.entries_cx A.c_provisions] in Ik |- *.
  apply in_flat_map in Ik as [e [_ Ik]].
  destruct e as [t0|v t0 a0|f g a0|r0 m g a0|u0 fx|eql eqr]; cbn [A.entry_provision In] in Ik; try contradiction;
    destruct Ik as [E|[]]; injection E as <- <-; split.
  - intros tq [<-|[]]. apply Idem.
  - intros r1 m1 a1 E. discriminate.
  - intros tq It. cbn [prov_types A.sig_subst A.sg_ret A.sg_params] in It.
    destruct It as [<-|It]; [apply Idem|]. apply in_map_iff in It as [t1 [<- _]]. apply Idem.
  - intros r1 m1 a1 E. discriminate.
  - intros tq It. cbn [prov_types A.sig_subst A.sg_ret A.sg_params] in It.
    destruct It as [<-|It]; [apply Idem|]. apply in_map_iff in It as [t1 [<- _]]. apply Idem.
  - intros r1 m1 a1 E. unfold A.mkey in E. injection E as <- _ _. apply Idem.
  - intros tq It. cbn [prov_types] in It. unfold A.fixed_subst in It.
    apply in_map_iff in It as [[x1 t1] [<- It]]. apply in_map_iff in It as [[x2 t2] [E _]].
    injection E as _ <-. apply Idem.
  - intros r1 m1 a1 E. discriminate.
Qed.

(* EnvOk depends on the environment only through its provisions and the
   meanings of the types the context's provisions mention. *)
Lemma envok_transfer : forall n C d d' H (Q : ty -> Prop),
  (forall k, lookup_provision k d' = lookup_provision k d) ->
  (forall t, Q t -> ground d' t = ground d t) ->
  (forall v t, A.nlook v (A.c_vals C) = Some t -> Q t) ->
  (forall f g, A.nlook f (A.c_fns C) = Some g -> forall t, In t (A.sg_ret g :: A.sg_params g) -> Q t) ->
  (forall r m g, A.mlook r m (A.c_mths C) = Some g -> forall t, In t (r :: A.sg_ret g :: A.sg_params g) -> Q t) ->
  (forall u fx, A.nlook u (A.c_funs C) = Some fx -> forall x t, In (x, t) fx -> Q t) ->
  EnvOk n C d H -> EnvOk n C d' H.
Proof.
  intros n C d d' H Q Lp G QV QF QM QU [Vals [Fns [Mths Funs]]]. split; [|split; [|split]].
  - intros v t L. destruct (Vals v t L) as [w [Look T]]. exists w. rewrite Lp, (G t (QV v t L)). split; assumption.
  - intros f g L. destruct (Fns f g L) as [pv [Look Ok]]. exists pv. rewrite Lp. split; [exact Look|].
    apply (callable_transfer n n H d d' (A.sg_params g) (A.sg_params g) (A.sg_ret g) (A.sg_ret g) pv (le_n _)); [| |exact Ok].
    + apply map_ext_in. intros t It. symmetry. exact (G t (QF f g L t (or_intror It))).
    + symmetry. exact (G _ (QF f g L _ (or_introl eq_refl))).
  - intros r m g L. destruct (Mths r m g L) as [pv [Look Ok]]. exists pv. rewrite Lp. split; [exact Look|].
    apply (callable_transfer n n H d d' (r :: A.sg_params g) (r :: A.sg_params g) (A.sg_ret g) (A.sg_ret g) pv (le_n _)); [| |exact Ok].
    + apply map_ext_in. intros t It. symmetry. apply G. apply (QM r m g L).
      destruct It as [<-|It]; [left; reflexivity|right; right; exact It].
    + symmetry. apply G. apply (QM r m g L). right. left. reflexivity.
  - intros u fx L. destruct (Funs u fx L) as [pv [gfx [Look [Ok [Cov Vm]]]]]. exists pv, gfx. rewrite Lp.
    split; [exact Look|]. split; [exact Ok|]. split; [exact Cov|].
    intros x t I. rewrite (Vm x t I), (G t (QU u fx L x t I)). reflexivity.
Qed.

(* An application's body environment realizes G's context: the needs F
   links come from F's realized context, the captured ones from the
   closure. The body reads each input through G's table (fc_table), so
   G's merges hold by construction. *)
Lemma link_entry : forall rr H d th d1 u gg h pins Cu nsu Cg nsg linktarget dc gfx d2,
  LegacyBodyJudgments.LinkBody W Prog u gg h pins -> ClosureBinding W u Cu nsu Cg nsg linktarget dc gfx -> A.FunCtx W u Cu nsu -> A.FunCtx W gg Cg nsg ->
  map fst th = map fst (A.c_ty Cu) ->
  (forall x, A.Fixed W u x -> In x (map fst (A.c_ty Cu))) ->
  Realizes rr Cu (callee_env d th d1) H ->
  (forall x, A.Fixed W u x -> lookup_subst x gfx = option_map (ground d) (lookup_subst x th)) ->
  (forall gm, AppMeanings u Cu Cg dc gfx gm ->
      (forall v t, In (IVal v) (A.captured_needs linktarget nsg) -> A.nlook v (A.c_vals Cg) = Some t -> exists w,
         lookup_provision (IVal v) dc = Some (ValueProvision w) /\ value_typed W P H w (app_subst gm t)) /\
      (forall f g, In (IFn f) (A.captured_needs linktarget nsg) -> A.nlook f (A.c_fns Cg) = Some g -> exists pv,
         lookup_provision (IFn f) dc = Some pv /\ CallableOk rr H (denv gm) (A.sg_params g) (A.sg_ret g) pv) /\
      (forall r m a g, In (IMth r m a) (A.captured_needs linktarget nsg) -> A.mlook r m (A.c_mths Cg) = Some g -> exists pv,
         lookup_provision (A.rkey Cg r m) dc = Some pv /\ CallableOk rr H (denv gm) (r :: A.sg_params g) (A.sg_ret g) pv)) ->
  (forall gm, AppMeanings u Cu Cg dc gfx gm -> CapturedFuns rr H gm dc (captured_funs Cg (A.captured_needs linktarget nsg))) ->
  select_provisions d1 (A.link_map Cu Cg linktarget nsg) = Some d2 ->
  Realizes rr Cg (application_environment P d h th (append_environment d2 dc)) H.
Proof.
  intros rr H d th d1 u gg h pins Cu nsu Cg nsg linktarget dc gfx d2 LB Binding FCu FCg Keys FxIn RealU Gfx Caps CF Sel.
  destruct (link_facts _ _ _ _ LB) as (dg & n0 & ps & a & rspec & bodies & Cu' & nsu' & Cg' & nsg' & Fg & wrap & Cb & Fb & made
    & outs & gen & Cr & ks & binds0 & FCu' & FCg' & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & FB & _).
  destruct (functx_unique _ _ _ _ _ FCu FCu') as [<- <-]. destruct (functx_unique _ _ _ _ _ FCg FCg') as [<- <-].
  destruct (functx_static _ _ _ FCg) as [[CanG DomG] [[SectionG KMg] [NdG [ProvNsG [ShapeG [ScG FixG]]]]]].
  destruct (functx_static _ _ _ FCu) as [[CanU DomU] [[SectionU KMu] [NdU [ProvNsU [ShapeU [ScU FixU]]]]]].
  set (ce := callee_env d th d1). set (de := application_environment P d h th (append_environment d2 dc)).
  (* The selected body reads its canonical leaves at this application. *)
  set (B := link_reading Cu Cg (type_meanings ce) dc).
  set (de0 := EnvironmentWithAliases B B (provision_values (append_environment d2 dc))).
  pose proof RealU as [ReadsU [[ValsU [FnsU [MthsU FunsU]]] [_ KMU']]].
  assert (Code : view_functor P h = A.funbody_of Prog h) by reflexivity.
  destruct (select_provisions_empty _ _ _ Sel) as [Em2 Ea2].
  set (caps := A.captured_needs linktarget nsg) in *.
  set (lks := A.linked_needs linktarget nsg) in *.
  assert (InKeys : forall k0 p, In (k0, p) (A.c_provisions Cg) -> In k0 nsg) by exact ProvNsG.
  assert (Th : forall x, In x (map fst (A.c_ty Cu)) -> exists v, lookup_subst x th = Some v).
  { intros x Ix. rewrite <- Keys in Ix. destruct (lookup_subst x th) as [v|] eqn:L; [eauto|].
    exfalso. apply in_map_iff in Ix as [[y v] [E Iq]]. cbn in E. subst y.
    clear -L Iq. induction th as [|[a0 t0] rest IH]; [contradiction|]. cbn in L, Iq.
    destruct (Nat.eqb_spec x a0) as [->|Ne]; [discriminate|]. destruct Iq as [E|Iq]; [injection E as E; congruence|apply IH; assumption]. }
  (* Provision selection changes no captured type meaning. *)
  assert (SameCapture : forall x, cmeaning (append_environment d2 dc) x = cmeaning dc x).
  { intro x. unfold cmeaning, output_meaning. cbn [type_aliases type_meanings append_environment].
    now rewrite Ea2, Em2. }
  assert (Mde : type_meanings de = map (fun source => (source, match lookup_subst source (A.c_ty Cg) with
    | Some t => app_subst B t | None => TAbs source end)) (map fst (A.c_ty Cg))).
  { unfold de. erewrite linked_application_table.
    - unfold B, ce. cbn [callee_env type_meanings]. now rewrite (link_reading_same_capture _ _ _ _ _ SameCapture).
    - rewrite Code. exact FB.
    - intros x Member. destruct (Th x Member) as [v Read]. congruence.
    - intros x t Read. rewrite lookup_subst_nlook' in Read.
      intros y Member. pose proof (PublicCopySubst.canon_input_support W gg Cg nsg FCg x) as Scope.
      rewrite (CanG x t Read) in Member. apply Scope;
        [exact (in_map fst _ (x,t) (PublicTypeRead.nlook_in _ _ _ _ Read))|exact Member]. }
  assert (FixAt : forall x, A.Fixed W u x -> lookup_subst x gfx = Some (app_subst (type_meanings ce) (TAbs x))).
  { intros x Fixed. rewrite (Gfx x Fixed). unfold ce, callee_env. cbn [type_meanings app_subst].
    rewrite lookup_map_snd. destruct (Th x (FxIn x Fixed)) as [v ->]. reflexivity. }
  assert (Am : AppMeanings u Cu Cg dc gfx B).
  { split.
    - intros x Fx Ig. unfold B. now rewrite (link_reading_shared _ _ _ _ _ (FxIn x Fx) Ig), FixAt.
    - intros x Ig Ou Present. unfold B. eapply closure_bind_captures; eassumption. }
  pose proof (closure_bind_provisions _ _ _ _ _ _ _ _ _ Binding (type_meanings ce) ReadsU FixAt) as Compare.
  fold B in Compare.
  assert (InLks : forall k0 key, In k0 nsg -> linktarget k0 = Some key -> In k0 lks).
  { intros k0 key Ig Selected. apply linked_spec. split; [exact Ig|congruence]. }
  assert (InCaps : forall k0, In k0 nsg -> linktarget k0 = None -> In k0 caps).
  { intros k0 Ig Captured. apply captured_spec. split; assumption. }
  assert (LksIn : forall k0, In k0 lks -> In k0 nsg /\ linktarget k0 <> None).
  { intros k0 I. exact (proj1 (linked_spec linktarget nsg k0) I). }
  assert (LookL : forall k0 key v, In k0 nsg -> linktarget k0 = Some key ->
    lookup_provision (A.rkey_of Cu key) ce = Some v -> lookup_provision (A.rkey_of Cg k0) de0 = Some v).
  { intros k0 key v Ig Selected L.
    pose proof (InLks k0 key Ig Selected) as Ik.
    change (lookup_provision (A.rkey_of Cg k0) de0) with (lookup_provision (A.rkey_of Cg k0) (append_environment d2 dc)).
    rewrite lookup_append_env, (select_lookup d1 _ d2 _ Sel). unfold A.link_map. fold lks.
    rewrite (sm_find_map (A.rkey_of Cg) (fun k => A.rkey_of Cu (A.linked_target linktarget k)) lks k0 Ik).
    - unfold A.linked_target. rewrite Selected. exact (f_equal (fun v => match v with Some p => Some p | None => lookup_provision (A.rkey_of Cg k0) dc end) L).
    - intros k' Ik' E. exact (rkey_of_injective Cg k' k0 (ShapeG k' (proj1 (LksIn k' Ik'))) (ShapeG k0 Ig) E). }
  assert (LookC : forall k0, In k0 caps -> lookup_provision (A.rkey_of Cg k0) de0 = lookup_provision (A.rkey_of Cg k0) dc).
  { intros k0 Ik. change (lookup_provision (A.rkey_of Cg k0) de0) with (lookup_provision (A.rkey_of Cg k0) (append_environment d2 dc)).
    rewrite lookup_append_env, (select_lookup d1 _ d2 _ Sel). unfold A.link_map. fold lks.
    destruct (proj1 (captured_spec linktarget nsg k0) Ik) as [Ig Captured].
    assert (Absent : sm_find (A.rkey_of Cg k0) (map (fun k => (A.rkey_of Cg k, A.rkey_of Cu (A.linked_target linktarget k))) lks) = None).
    { assert (All : forall k', In k' lks -> A.rkey_of Cg k' <> A.rkey_of Cg k0).
      { intros k' Ik' E. pose proof (rkey_of_injective Cg k' k0 (ShapeG k' (proj1 (LksIn k' Ik'))) (ShapeG k0 Ig) E) as ->.
        exact (proj2 (LksIn k0 Ik') Captured). }
      clear -All. induction lks as [|k' rest IH]; [reflexivity|]. cbn.
      destruct (item_eqb (A.rkey_of Cg k0) (A.rkey_of Cg k')) eqn:E.
      - apply item_eqb_spec in E. exfalso. exact (All k' (or_introl eq_refl) (eq_sym E)).
      - apply IH. intros k'' I. exact (All k'' (or_intror I)). }
    rewrite Absent. reflexivity. }
  destruct (Caps B Am) as [CVals [CFns CMths]].
  pose proof (CF B Am) as CFs.
  (* the body's meanings read G's table over B *)
  assert (GD : forall t, ground de t = app_subst B (A.canon Cg t)).
  { intros t. unfold ground, A.canon. rewrite Mde. rewrite <- (app_subst_nil t) at 1. apply subst2_agree.
    intros x _. rewrite app_subst_nil, !subst_abs.
    destruct (in_dec Nat.eq_dec x (map fst (A.c_ty Cg))) as [Ig|Og].
    - rewrite (lookup_map_nat _ _ x Ig). cbv beta iota. destruct (keys_in _ x Ig) as [v Lv].
      rewrite (lookup_subst_nlook' x (A.c_ty Cg)), Lv, (CanG x v Lv). reflexivity.
    - rewrite (lookup_map_out _ _ x Og).
      assert (Ls : lookup_subst x (A.c_sub Cg) = None).
      { destruct (lookup_subst x (A.c_sub Cg)) as [t0|] eqn:Ls; [|reflexivity].
        exfalso. exact (Og (proj1 (DomG x t0 (lookup_subst_in_pair _ _ _ Ls)))). }
      rewrite Ls. cbv iota. rewrite (subst_abs B). unfold B. now rewrite link_reading_out. }
  assert (Env0 : EnvOk rr Cg de0 H).
  { split; [|split; [|split]].
  - intros v t Found. destruct (vals_ilook _ v t KMg Found) as [inst Il].
    pose proof (ProvNsG _ _ (ilook_in _ _ _ Il)) as Ik.
    destruct (linktarget (IVal v)) as [key|] eqn:Target.
    + destruct (Compare (IVal v) _ Ik Il key Target) as [q [Lookup [Key Read]]].
      destruct key as [ty|v'|f|r m args|h']; cbn [A.provision_id app_subst_item] in Key; try discriminate.
      injection Key as <-. destruct q as [t' a'|g a'|g a'|fx]; cbn [A.provision_subst] in Read; try discriminate.
      destruct (ValsU v t' (ilook_vals _ _ _ _ KMu Lookup)) as [w [Look Typed]]. exists w.
      split; [exact (LookL (IVal v) _ _ Ik Target Look)|].
      assert (ReadTy : ground de0 t = ground ce t') by (change (app_subst B t = app_subst (type_meanings ce) t'); congruence).
      rewrite ReadTy. exact Typed.
    + destruct (CVals v t (InCaps _ Ik Target) Found) as [w [Look Typed]]. exists w.
      split; [pose proof (LookC (IVal v) (InCaps _ Ik Target)) as LC; cbn [A.rkey_of] in LC; now rewrite LC|exact Typed].
  - intros f g Found. destruct (fns_ilook _ f g KMg Found) as [inst Il].
    pose proof (ProvNsG _ _ (ilook_in _ _ _ Il)) as Ik.
    destruct (linktarget (IFn f)) as [key|] eqn:Target.
    + destruct (Compare (IFn f) _ Ik Il key Target) as [q [Lookup [Key Read]]].
      destruct key as [ty|v|f'|r m args|h']; cbn [A.provision_id app_subst_item] in Key; try discriminate.
      injection Key as <-. destruct q as [t' a'|g' a'|g' a'|fx]; cbn [A.provision_subst] in Read; try discriminate.
      destruct (FnsU f g' (ilook_fns _ _ _ _ KMu Lookup)) as [pv [Look Ok]]. exists pv.
      split; [exact (LookL (IFn f) _ _ Ik Target Look)|].
      assert (SigRead : A.sig_subst B g = A.sig_subst (type_meanings ce) g') by congruence.
      apply (callable_transfer rr rr H ce de0 (A.sg_params g') (A.sg_params g) (A.sg_ret g') (A.sg_ret g) pv (le_n _));
        [exact (eq_sym (f_equal A.sg_params SigRead))|exact (eq_sym (f_equal A.sg_ret SigRead))|exact Ok].
    + destruct (CFns f g (InCaps _ Ik Target) Found) as [pv [Look Ok]]. exists pv.
      split; [pose proof (LookC (IFn f) (InCaps _ Ik Target)) as LC; cbn [A.rkey_of] in LC; now rewrite LC|exact Ok].
  - intros r m g Found. destruct (mths_ilook _ r m g KMg Found) as [inst Il].
    pose proof (ProvNsG _ _ (ilook_in _ _ _ Il)) as Ik.
    destruct (linktarget (A.mkey r m)) as [key|] eqn:Target.
    + destruct (Compare (A.mkey r m) _ Ik Il key Target) as [q [Lookup [Key Read]]].
      destruct key as [ty|v|f|r' m' args|h']; cbn [A.provision_id A.mkey app_subst_item map_snd] in Key; try discriminate.
      assert (Name : m' = m) by congruence. subst m'.
      destruct q as [t' a'|g' a'|g' a'|fx]; cbn [A.provision_subst] in Read; try discriminate.
      destruct (MthsU r' m g' (ilook_mths _ _ _ _ _ KMu Lookup)) as [pv [Look Ok]]. exists pv.
      split; [exact (LookL (A.mkey r m) _ _ Ik Target Look)|].
      assert (SigRead : A.sig_subst B g = A.sig_subst (type_meanings ce) g') by congruence.
      assert (Receiver : app_subst B r = app_subst (type_meanings ce) r') by congruence.
      apply (callable_transfer rr rr H ce de0 (r' :: A.sg_params g') (r :: A.sg_params g) (A.sg_ret g') (A.sg_ret g) pv (le_n _)); [| |exact Ok].
      * cbn [map]. f_equal; [exact (eq_sym Receiver)|exact (eq_sym (f_equal A.sg_params SigRead))].
      * exact (eq_sym (f_equal A.sg_ret SigRead)).
    + destruct (CMths r m [] g (InCaps _ Ik Target) Found) as [pv [Look Ok]]. exists pv.
      split; [rewrite <- Look; exact (LookC (A.mkey r m) (InCaps _ Ik Target))|exact Ok].
  - intros u' fx Found. pose proof (funs_ilook _ u' fx KMg Found) as Il.
    pose proof (ProvNsG _ _ (ilook_in _ _ _ Il)) as Ik.
    destruct (FixG u' fx (ilook_in _ _ _ Il)) as [CovG ScFG].
    destruct (linktarget (IFun u')) as [key|] eqn:Target.
    + destruct (Compare (IFun u') _ Ik Il key Target) as [q [Lookup [Key Read]]].
      destruct key as [ty|v|f|r m args|h']; cbn [A.provision_id app_subst_item] in Key; try discriminate.
      injection Key as <-. destruct q as [t' a'|g' a'|g' a'|fx']; cbn [A.provision_subst] in Read; try discriminate.
      assert (Lu : A.nlook u' (A.c_funs Cu) = Some fx').
      { change (A.c_funs Cu) with (funs_of (A.c_provisions Cu)). cbn [A.provision_id] in Lookup. now rewrite (funs_by_ilook _ u' KMu), Lookup. }
      destruct (FunsU u' fx' Lu) as [pv [gfx' [Look [Ok [Cov Vm]]]]]. exists pv, gfx'.
      split; [exact (LookL (IFun u') _ _ Ik Target Look)|]. split; [exact Ok|]. split; [exact CovG|].
      intros x tx I. assert (FxRead : map (fun q => (fst q, app_subst B (snd q))) fx =
        map (fun q => (fst q, app_subst (type_meanings ce) (snd q))) fx') by congruence.
      assert (I' : In (x, app_subst B tx) (map (fun q => (fst q, app_subst (type_meanings ce) (snd q))) fx')).
      { rewrite <- FxRead. exact (in_map (fun q => (fst q, app_subst B (snd q))) fx (x,tx) I). }
      apply in_map_iff in I' as [[y value] [Equal InFx]]. cbn in Equal. injection Equal as -> Equal.
      rewrite (Vm x value InFx).
      change (Some (app_subst (type_meanings ce) value) = Some (app_subst B tx)).
      exact (f_equal Some Equal).
    + destruct (captured_funs_find _ _ _ _ _ CFs u' fx (captured_funs_in Cg caps u' fx (InCaps _ Ik Target) Found))
        as [pv [gfx' [Look [Ok Vm]]]].
      exists pv, gfx'. split; [pose proof (LookC (IFun u') (InCaps _ Ik Target)) as LC; cbn [A.rkey_of] in LC; now rewrite LC|].
      split; [exact Ok|]. split; [exact CovG|]. intros x tx I. exact (Vm x tx I).
}
  split; [|split; [|exact (conj SectionG KMg)]].
  - (* G's merges hold in the body's environment, by its table *)
    intros left right Equal. apply (context_canon_iff_mergeeq Cg SectionG) in Equal.
    now rewrite (GD left), (GD right), Equal.
  - apply (envok_transfer rr Cg de0 de H (fun t => A.canon Cg t = t)); [reflexivity| | | | | |exact Env0].
    + intros t Ct. rewrite GD, Ct. reflexivity.
    + intros v t L. destruct (vals_ilook _ v t KMg L) as [a0 Il].
      exact (proj1 (functx_canon_prov _ _ _ FCg _ _ (ilook_in _ _ _ Il)) t (or_introl eq_refl)).
    + intros f g L t It. destruct (fns_ilook _ f g KMg L) as [a0 Il].
      exact (proj1 (functx_canon_prov _ _ _ FCg _ _ (ilook_in _ _ _ Il)) t It).
    + intros r m g L t It. destruct (mths_ilook _ r m g KMg L) as [a0 Il].
      destruct (functx_canon_prov _ _ _ FCg _ _ (ilook_in _ _ _ Il)) as [Ct Crr].
      destruct It as [<-|It]; [exact (Crr r m [] eq_refl)|exact (Ct t It)].
    + intros u' fx L x t I. pose proof (funs_ilook _ u' fx KMg L) as Il.
      exact (proj1 (functx_canon_prov _ _ _ FCg _ _ (ilook_in _ _ _ Il)) t (in_map snd fx (x, t) I)).
Qed.
(* An application's θ reads F's inputs here, as a call's does. *)
Lemma apply_theta : forall C u Cu ns sm,
  A.FunCtx W u Cu ns -> CxSection C ->
  (forall t tt, A.nlook t (A.c_ty Cu) = Some tt -> exists here, A.nlook t (A.c_ty C) = Some here) ->
  A.Satisfies C Cu (map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cu)) ns sm ->
  A.CallTheta W C Cu u (map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cu)).
Proof.
  intros C u Cu ns sm FC SectionC Inp Sat. set (th := map _ (A.c_ty Cu)).
  pose proof (section_canonical C SectionC) as Idem.
  pose proof (proj2 (functor_context_merge W u Cu ns FC)) as SectionU.
  destruct (functx_scoped W u Cu ns FC) as [_ [_ [_ [_ [_ [_ Dom]]]]]].
  destruct Sat as [_ [_ [_ [_ Merg]]]].
  assert (Lth : forall x, In x (map fst (A.c_ty Cu)) -> lookup_subst x th = Some (A.canon C (app_subst (A.c_ty C) (TAbs x)))).
  { intros x Ix. exact (lookup_on_keys (fun x => A.canon C (app_subst (A.c_ty C) (TAbs x))) _ x Ix). }
  assert (Through : forall t, incl (A.gens t) (map fst (A.c_ty Cu)) ->
    A.canon C (app_subst th t) = A.canon C (app_subst (A.c_ty C) t)).
  { intros t Sc. transitivity (A.canon C (A.canon C (app_subst (A.c_ty C) t))); [|apply Idem]. f_equal.
    unfold A.canon. rewrite <- (subst_through t (A.c_sub C) (A.c_ty C)).
    - apply subst_agree_on. intros x Ix. cbn [app_subst]. rewrite (Lth x (Sc x Ix)).
      rewrite (lookup_map_snd (app_subst (A.c_sub C))). cbn [app_subst].
      destruct (keys_in _ x (Sc x Ix)) as [v Lv]. destruct (Inp x v Lv) as [here Lh].
      rewrite lookup_subst_nlook', Lh. reflexivity.
    - intros x Ix. destruct (keys_in _ x (Sc x Ix)) as [v Lv]. destruct (Inp x v Lv) as [here Lh].
      rewrite lookup_subst_nlook', Lh. discriminate. }
  split; [unfold th; rewrite map_map; reflexivity|]. split.
  - intros r Ir _. exact (Lth r Ir).
  - eapply PublicMerge.respects_section; [exact SectionU|].
    intros g t Ig. apply (context_canon_iff_mergeeq C SectionC).
    destruct (Dom g t Ig) as [Gg Gt].
    rewrite (Through (TAbs g)) by (intros y [<-|[]]; exact Gg). rewrite (Through t Gt).
    assert (Force : forall x, In x (map fst (A.c_ty Cu)) -> A.nlook x (A.c_ty C) <> None).
    { intros x Kx. destruct (keys_in _ x Kx) as [v Lv]. destruct (Inp x v Lv) as [here Lh]. rewrite Lh. discriminate. }
    apply (PublicOrientation.merges_by_pairs C Cu (map fst (A.c_ty Cu)) Dom
             (PublicOrientation.functx_sub_function W u Cu ns FC) Force Merg); [exact Ig|]. intros x Ix.
    assert (Kx : In x (map fst (A.c_ty Cu))) by (destruct Ix as [<-|Ix]; [exact Gg|exact (Gt x Ix)]).
    destruct (keys_in _ x Kx) as [v Lv]. destruct (Inp x v Lv) as [here Lh]. rewrite Lh. discriminate.
Qed.

Lemma bindings_apply_eq : forall j d g st u th outs sm rs r,
  AR.O.I.eval_bindings (S j) P d g st (BApply u th outs sm rs :: r) =
  match AR.O.I.apply_functor j P d u th outs sm rs st with
  | inl (dr, st1) => AR.O.I.eval_bindings j P (append_environment dr d) g st1 r
  | inr rr => inr (AR.O.I.with_g g rr)
  end.
Proof. reflexivity. Qed.

(* How a closure came to be applicable. *)
Lemma closure_cases : forall n H0 u gfx pv, ClosureOk n H0 u gfx pv ->
  (exists gg h Cu nsu Cg nsg linktarget dc,
     pv = FunctorClosure h (Some (A.link_map Cu Cg linktarget nsg)) dc None /\
     (exists pins, LegacyBodyJudgments.LinkBody W Prog u gg h pins /\ PinsAgree u Cu Cg dc gfx pins /\
       ClosureBinding W u Cu nsu Cg nsg linktarget dc gfx) /\ A.FunCtx W u Cu nsu /\ A.FunCtx W gg Cg nsg /\
     (forall x, A.Fixed W u x -> In x (map fst (A.c_ty Cu))) /\
     (forall gm, AppMeanings u Cu Cg dc gfx gm ->
        (forall v t, In (IVal v) (A.captured_needs linktarget nsg) -> A.nlook v (A.c_vals Cg) = Some t -> exists w,
           lookup_provision (IVal v) dc = Some (ValueProvision w) /\ value_typed W P H0 w (app_subst gm t)) /\
        (forall f g, In (IFn f) (A.captured_needs linktarget nsg) -> A.nlook f (A.c_fns Cg) = Some g -> exists pv,
           lookup_provision (IFn f) dc = Some pv /\ CallableOk n H0 (denv gm) (A.sg_params g) (A.sg_ret g) pv) /\
        (forall r m a g, In (IMth r m a) (A.captured_needs linktarget nsg) -> A.mlook r m (A.c_mths Cg) = Some g -> exists pv,
           lookup_provision (A.rkey Cg r m) dc = Some pv /\ CallableOk n H0 (denv gm) (r :: A.sg_params g) (A.sg_ret g) pv)) /\
     (forall gm, AppMeanings u Cu Cg dc gfx gm ->
        CapturedFuns n H0 gm dc (captured_funs Cg (A.captured_needs linktarget nsg)))) \/
  (exists h Cu nsu Ch nsh target code sms dc rms rm gfh,
     pv = FunctorClosure code (compose_adaptation sms (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu (target k))) nsh)) dc (compose_positions rms rm) /\
     ClosureOk n H0 h gfh (FunctorClosure code sms dc rms) /\
     A.FunCtx W u Cu nsu /\ A.FunCtx W h Ch nsh /\ NeedsAmong target nsh nsu /\
     (forall x, A.Fixed W h x -> A.Fixed W u x /\ lookup_subst x gfh = lookup_subst x gfx)) \/
  (exists code, pv = FunctorClosure code None (EnvironmentWithAliases [] [] []) None /\ Platform.Family W Prog u code /\
     PlatformFixed u gfx).
Proof.
  intros n H0 u gfx pv Ok. destruct Ok as [u gg h Cu nsu Cg nsg linktarget dc gfx pins LB Binding FCu FCg FxIn _ Caps CF PinsOk
    |u h Cu nsu Ch nsh target code sms dc rms rm gfx gfh Okh FCu FCh Incl Fx _ _|u code gfx Fam Mach].
  - left. exists gg, h, Cu, nsu, Cg, nsg, linktarget, dc. split; [reflexivity|].
    exact (conj (ex_intro _ pins (conj LB (conj PinsOk Binding))) (conj FCu (conj FCg (conj FxIn (conj Caps CF))))).
  - right; left. exists h, Cu, nsu, Ch, nsh, target, code, sms, dc, rms, rm, gfh. split; [reflexivity|].
    exact (conj Okh (conj FCu (conj FCh (conj Incl Fx)))).
  - right; right. exists code. split; [reflexivity|exact (conj Fam Mach)].
Qed.

(* A linked closure's functor is abstract (CLink). *)
Lemma closure_link_abstract : forall n H0 u gfx h sm dc, ClosureOk n H0 u gfx (FunctorClosure h (Some sm) dc None) ->
  exists n ps a r, A.ast_of W u = Some (ADFunctor n ps a r None).
Proof.
  intros n H0 u gfx h sm dc Ok. inversion Ok as [u' gg h' Cu nsu Cg nsg linktarget dc' gfx' pins LB Binding FCu FCg FxIn AbU Caps CF PinsOk
    |u' h' Cu nsu Ch nsh target code sms dc' rms rm gfx' gfh Okh FCu FCh Incl Fx CO InCu Same Site|u' code gfx' Fam Mach PR]; subst.
  exact AbU.
Qed.

Lemma ilook_some_of_in : forall X k (l : list (item * X)) p, In (k, p) l -> exists p', A.ilook k l = Some p'.
Proof.
  intros X k l p. induction l as [|[k' p'] r IH]; intros I; [contradiction|]. cbn.
  destruct (item_eqb k k') eqn:E; [eauto|]. destruct I as [Eq|I]; [injection Eq as -> ->; rewrite (proj2 (item_eqb_spec _ _) eq_refl) in E; discriminate|exact (IH I)].
Qed.

(* A realized context's provisions are found under their run-time keys. *)
Lemma realized_key : forall n C d H k p, Realizes n C d H -> In (k, p) (A.c_provisions C) -> NeedShape k ->
  exists pv, lookup_provision (A.rkey_of C k) d = Some pv.
Proof.
  intros n C d H k p [_ [[Vals [Fns [Mths Funs]]] [_ KM]]] I Sh.
  destruct (ilook_some_of_in _ k _ p I) as [p' Il]. pose proof (KM _ _ (ilook_in _ _ _ Il)) as Kind.
  destruct k as [t|v|f|r m th|u]; cbn in Sh |- *; try contradiction.
  - destruct p' as [t' a|g a|g a|fx]; cbn in Kind; try contradiction.
    destruct (Vals v t' (ilook_vals _ v t' a KM Il)) as [w [Look _]]. eauto.
  - destruct p' as [t' a|g a|g a|fx]; cbn in Kind; try contradiction.
    destruct (Fns f g (ilook_fns _ f g a KM Il)) as [pv [Look _]]. eauto.
  - subst th. destruct p' as [t' a|g a|g a|fx]; cbn in Kind; try contradiction.
    destruct (Mths r m g (ilook_mths _ r m g a KM Il)) as [pv [Look _]]. eauto.
  - destruct p' as [t' a|g a|g a|fx]; cbn in Kind; try contradiction.
    assert (L : A.nlook u (A.c_funs C) = Some fx).
    { change (A.c_funs C) with (funs_of (A.c_provisions C)). rewrite (funs_by_ilook _ u KM), Il. reflexivity. }
    destruct (Funs u fx L) as [pv [gfx [Look _]]]. eauto.
Qed.

Lemma bindsof_fold : forall bs l, LegacyBodyJudgments.BindsOf (fold_right (fun b x => EBind b x) EUnit bs) l -> l = bs.
Proof.
  induction bs as [|b r IH]; intros l D; cbn in D; inversion D; subst; [reflexivity|]. f_equal. apply IH. assumption.
Qed.

Lemma functx_locals : forall u C ns, A.FunCtx W u C ns -> A.c_locals C = [].
Proof. intros u C ns (d & ps & a & r & b & es & tm & tm' & σ & classes & implicit & _ & _ & _ & _ & _ & _ & _ & _ & -> & _ & _). reflexivity. Qed.

Lemma apply_functor_eq : forall j d u th outs sm rs st,
  AR.O.I.apply_functor (S j) P d u th outs sm rs st =
  match lookup_provision (IFun u) d with
  | Some (FunctorClosure h sms dc rms) =>
      match view_functor P h with
      | Some code =>
          match select_provisions d sm with
          | Some d1 =>
              match adapt_provisions d1 sms with
              | Some d2 =>
                  match AR.O.I.eval_bindings j P (application_environment P d h th (append_environment d2 dc)) [] st (fc_bindings code) with
                  | inl (d3, _, st3) =>
                      match select_provisions d3 (result_selection rs rms (fc_sources code)) with
                      | Some dr => match result_environment outs d3 (fc_outputs code) dr with
                          | Some result => inl (result, st3)
                          | None => inr (AR.O.I.Evaluation (Stuck AR.O.I.e_compose) [] st3) end
                      | None => inr (AR.O.I.Evaluation (Stuck AR.O.I.e_compose) [] st3)
                      end
                  | inr rr => inr rr
                  end
              | None => inr (AR.O.I.Evaluation (Stuck AR.O.I.e_chain) [] st)
              end
          | None => inr (AR.O.I.Evaluation (Stuck AR.O.I.e_compose) [] st)
          end
      | None => inr (AR.O.I.Evaluation (Stuck AR.O.I.e_nobody) [] st)
      end
  | _ => inr (AR.O.I.Evaluation (Stuck AR.O.I.e_fun) [] st)
  end.
Proof. reflexivity. Qed.

(* A body that neither returns nor loops stops only by trapping, exiting,
   an unsupported operation or running out of fuel: the caller's step
   stops the same way. *)
Lemma body_stop : forall Fb d' H' types' t (B Bi : Prop) F d H types g res,
  A.fr_ret Fb = None -> A.fr_loop Fb = false -> (forall v, AR.O.I.evaluation_signal res <> Normal v) ->
  Outcome Fb d' H' types' t B res ->
  (forall v, AR.O.I.evaluation_signal (AR.O.I.with_g g res) <> Normal v) /\ Outcome F d H types TUnit Bi (AR.O.I.with_g g res).
Proof.
  intros Fb d' H' types' t B Bi F d H types g [sig g' st] Ret Loop NN Out. cbn in NN, Out |- *.
  split; [exact NN|]. destruct sig; cbn in Out |- *; auto.
  - exfalso. exact (NN v eq_refl).
  - destruct Out as [H2 [ret [R _]]]. congruence.
  - destruct Out as [_ [L _]]. congruence.
Qed.

(* Chains of steps run below the eval-fuel cap J (a functor body runs
   below its application's fuel). *)
Section Cap.
Variable J : nat.

Inductive Chain (k : nat) : A.frame -> A.cx -> A.frame -> A.cx -> Prop -> list nat -> list binding -> Prop :=
| ChainNil : forall F F' C (B : Prop), FrameSame F F' -> Chain k F C F' C B [] []
| ChainCons : forall F C F1 C1 F2 C2 (B1 B2 B : Prop) m1 m2 b bs,
    (forall r, r <= S k -> BindStep J r F C F1 C1 B1 m1 b) -> Chain k F1 C1 F2 C2 B2 m2 bs ->
    (B1 \/ B2 -> B) -> Chain k F C F2 C2 B (m1 ++ m2)%list (b :: bs).

Lemma frame_same_refl : forall F, FrameSame F F.
Proof. intros F. repeat split. Qed.

Lemma frame_same_trans : forall F1 F2 F3, FrameSame F1 F2 -> FrameSame F2 F3 -> FrameSame F1 F3.
Proof. intros F1 F2 F3 [A1 [A2 [A3 A4]]] [B1 [B2 [B3 B4]]]. repeat split; congruence. Qed.

Lemma chain_frame : forall k F C F' C' B made bs, Chain k F C F' C' B made bs -> FrameSame F F'.
Proof.
  intros k F C F' C' B made bs D. induction D as [F F' C B Same|F C F1 C1 F2 C2 B1 B2 B m1 m2 b bs Step Rest IH Imp];
    [exact Same|].
  exact (frame_same_trans _ _ _ (proj1 (Step (S k) (le_n _))) IH).
Qed.

Lemma chain_weaken : forall k F C F' C' (B B' : Prop) made bs, Chain k F C F' C' B made bs -> (B -> B') ->
  Chain k F C F' C' B' made bs.
Proof.
  intros k F C F' C' B B' made bs D Imp. destruct D as [F F' C B Same|F C F1 C1 F2 C2 B1 B2 B m1 m2 b bs Step Rest Imp0];
    [constructor; exact Same|]. apply (ChainCons k F C F1 C1 F2 C2 B1 B2 B' m1 m2 b bs Step Rest). tauto.
Qed.

Lemma bstep_src : forall r F F1 C F2 C2 (B : Prop) m b, FrameSame F F1 ->
  BindStep J r F1 C F2 C2 B m b -> BindStep J r F C F2 C2 B m b.
Proof.
  intros r F F1 C F2 C2 B m b Same [Same12 [Av Run]]. split; [exact (frame_same_trans _ _ _ Same Same12)|].
  split.
  { destruct Same as [Er [Et _]]. destruct Av as [Ar At]. split; intros t E; [apply Ar|apply At]; congruence. }
  intros j d types H g st Lj LJ Pre0 rest.
  destruct (Run j d types H g st Lj LJ (pre_frame _ _ _ _ _ _ _ _ _ Same Pre0) rest)
    as [Done|[res [Eq [NN Out]]]]; [left; exact Done|right].
  exists res. split; [exact Eq|]. split; [exact NN|]. exact (outcome_frame F F1 d H types TUnit B res Same Out).
Qed.

Lemma chain_reframe_src : forall k F1 C F2 C2 (B : Prop) m bs F, Chain k F1 C F2 C2 B m bs -> FrameSame F F1 ->
  Chain k F C F2 C2 B m bs.
Proof.
  intros k F1 C F2 C2 B m bs F D Same. destruct D as [F1 F2 C B0 Same'|F1 C F3 C3 F2 C2 B1 B2 B0 m1 m2 b bs Step Rest Imp].
  - constructor. exact (frame_same_trans _ _ _ Same Same').
  - apply (ChainCons k F C F3 C3 F2 C2 B1 B2 B0 m1 m2 b bs); [|exact Rest|exact Imp].
    intros r Lr. exact (bstep_src r F F1 C F3 C3 B1 m1 b Same (Step r Lr)).
Qed.

Lemma bstep_tgt : forall r F C F2 C2 F3 (B : Prop) m b, FrameSame F2 F3 ->
  BindStep J r F C F2 C2 B m b -> BindStep J r F C F3 C2 B m b.
Proof.
  intros r F C F2 C2 F3 B m b Same [Same12 [Av Run]]. split; [exact (frame_same_trans _ _ _ Same12 Same)|].
  split; [exact Av|]. intros j d types H g st Lj LJ Pre0 rest.
  destruct (Run j d types H g st Lj LJ Pre0 rest) as [[d' [g' [st' [H' [E [M [Pre' Eq]]]]]]]|Stop]; [left|right; exact Stop].
  exists d', g', st', H'. split; [exact E|]. split; [exact M|]. split; [exact (pre_frame _ _ _ _ _ _ _ _ _ Same Pre')|exact Eq].
Qed.

Lemma chain_reframe_tgt : forall k F C F2 C2 (B : Prop) m bs F3, Chain k F C F2 C2 B m bs -> FrameSame F2 F3 ->
  Chain k F C F3 C2 B m bs.
Proof.
  intros k F C F2 C2 B m bs F3 D. induction D as [F F2 C B0 Same'|F C F1 C1 F2 C2 B1 B2 B0 m1 m2 b bs Step Rest IH Imp];
    intros Same.
  - constructor. exact (frame_same_trans _ _ _ Same' Same).
  - exact (ChainCons k F C F1 C1 F3 C2 B1 B2 B0 m1 m2 b bs Step (IH Same) Imp).
Qed.

Lemma chain_app : forall k F C F1 C1 F2 C2 (B1 B2 B : Prop) m1 m2 bs1 bs2,
  Chain k F C F1 C1 B1 m1 bs1 -> Chain k F1 C1 F2 C2 B2 m2 bs2 -> (B1 \/ B2 -> B) ->
  Chain k F C F2 C2 B (m1 ++ m2)%list (bs1 ++ bs2)%list.
Proof.
  intros k F C F1 C1 F2 C2 B1 B2 B m1 m2 bs1 bs2 D1. revert F2 C2 B2 B m2 bs2.
  induction D1 as [F F1 C B0 Same|F C F1 C1 F1' C1' Ba Bb B0 ma mb b bs Step Rest IH Imp]; intros F2 C2 B2 B m2 bs2 D2 Imp2.
  - cbn. apply (chain_weaken k F C F2 C2 B2 B m2 bs2 (chain_reframe_src k F1 C F2 C2 B2 m2 bs2 F D2 Same)). tauto.
  - cbn. rewrite <- app_assoc. apply (ChainCons k F C F1 C1 F2 C2 Ba (Bb \/ B2) B ma (mb ++ m2)%list b (bs ++ bs2)%list Step).
    + apply (IH F2 C2 B2 (Bb \/ B2) m2 bs2 D2). tauto.
    + tauto.
Qed.

Lemma avoids_app : forall m1 m2 t, AvoidsTy (m1 ++ m2)%list t -> AvoidsTy m1 t /\ AvoidsTy m2 t.
Proof. intros m1 m2 t Av. split; intros g Ig; apply Av; apply in_or_app; [left|right]; exact Ig. Qed.

(* A chain gives the expression-level wrapper: each binding in turn, then
   the continuation. *)
Lemma chain_wrap : forall k n F C F' C' (Bi : Prop) made bs, Chain k F C F' C' Bi made bs -> n <= S k -> n <= J ->
  WrapSafe n F C F' C' Bi made (fun x => fold_right (fun b x => EBind b x) x bs).
Proof.
  intros k n F C F' C' Bi made bs D Le LeJ. induction D as [F F' C B0 Same|F C F1 C1 F2 C2 B1 B2 B m1 m2 b bs Step Rest IH Imp].
  - intros t B x _ Safe. apply (wrap_breaks n F C t B); [tauto|].
    destruct Same as [Er [Et [El Ev]]]. exact (codesafe_frame n F' F C t B x Er Et El Ev Safe).
  - intros t B' x Av Safe. destruct (avoids_app m1 m2 t Av) as [Av1 Av2].
    pose proof (IH t B' x Av2 Safe) as Inner. cbn [fold_right].
    intros [|j] Lj d types H g st Pre0; [exact Logic.I|]. rewrite eval_bind_eq.
    destruct j as [|j]; [exact Logic.I|].
    destruct (Step (S (S j)) ltac:(lia)) as [Same [FAv Run]].
    destruct (Run j d types H g st ltac:(lia) ltac:(lia) Pre0 []) as [[d' [g' [st' [H' [E [Grow [Pre' Eq]]]]]]]|[res [Eq [NN Out]]]];
      rewrite Eq.
    + destruct j as [|j]; [exact Logic.I|]. rewrite bindings_nil_eq.
      pose proof (Inner (S (S j)) ltac:(lia) d' types H' g' st' (pre_lower Pre')) as Out.
      eapply outcome_heap; [exact E|]. apply (outcome_grow m1 F d d' H' types t _ _ Grow Av1 FAv).
      apply (outcome_frame F F1 d' H' types t _ _ Same).
      eapply AR.O.outcome_breaks; [|exact Out]. tauto.
    + eapply outcome_pass; [|exact NN|exact Out]. tauto.
Qed.

Lemma realizes_pre_lower : forall j r F C d types H g st, Pre r F C d types H g st -> j <= r ->
  Pre j F C d types H g st.
Proof.
  intros j r F C d types H g st [Frame [Ctx [L [St [Real Fmd]]]]] Le.
  exact (conj Frame (conj Ctx (conj L (conj St (conj (realizes_lower r j C d H Le Real) Fmd))))).
Qed.

Lemma heap_extends_trans : forall H1 H2 H3, heap_extends H1 H2 -> heap_extends H2 H3 -> heap_extends H1 H3.
Proof. intros H1 H2 H3 E1 E2 a c L. exact (E2 _ _ (E1 _ _ L)). Qed.

Lemma frame_avoids_same : forall made F F', FrameSame F F' -> FrameAvoids made F -> FrameAvoids made F'.
Proof. intros made F F' [Er [Et _]] [Ar At]. split; intros t E; [apply Ar|apply At]; congruence. Qed.

(* A chain run by eval_bindings: it completes, in the context after it,
   or stops with an outcome. *)
Lemma chain_run : forall k F C F' C' (Bi : Prop) made bs, Chain k F C F' C' Bi made bs ->
  forall r j d types H g st rest, r <= S k -> j < r -> j <= J -> Pre r F C d types H g st ->
  (exists d' g' st' H' j', j = j' + List.length bs /\ heap_extends H H' /\ MeaningsGrow made d d' /\
     Pre r F' C' d' types H' g' st' /\
     AR.O.I.eval_bindings j P d g st (bs ++ rest)%list = AR.O.I.eval_bindings j' P d' g' st' rest) \/
  (exists res, AR.O.I.eval_bindings j P d g st (bs ++ rest)%list = inr res /\
     (forall v, AR.O.I.evaluation_signal res <> Normal v) /\ Outcome F d H types TUnit Bi res).
Proof.
  intros k F C F' C' Bi made bs D. induction D as [F F' C B0 Same|F C F1 C1 F2 C2 B1 B2 B m1 m2 b bs Step Rest IH Imp];
    intros r j d types H g st rest Lr Lj LJ Pre0.
  - left. exists d, g, st, H, j. split; [cbn; lia|]. split; [intros a c L; exact L|].
    split; [apply meanings_grow_refl; reflexivity|]. split; [exact (pre_frame _ _ _ _ _ _ _ _ _ Same Pre0)|reflexivity].
  - destruct j as [|j]; [right; eexists; split; [reflexivity|]; split; [intros v E; discriminate|exact Logic.I]|].
    destruct (Step r Lr) as [Same [FAv Run]].
    destruct (Run j d types H g st ltac:(lia) ltac:(lia) Pre0 (bs ++ rest)%list)
      as [[d1 [g1 [st1 [H1 [E1 [M1 [Pre1 Eq1]]]]]]]|[res [Eq [NN Out]]]].
    + cbn [app]. rewrite Eq1.
      destruct (IH r j d1 types H1 g1 st1 rest Lr ltac:(lia) ltac:(lia) Pre1)
        as [[d' [g' [st' [H' [j' [Ej [E' [M' [Pre' Eq']]]]]]]]]|[res [Eq [NN Out]]]].
      * left. exists d', g', st', H', j'. split; [cbn; lia|]. split; [exact (heap_extends_trans _ _ _ E1 E')|].
        split; [exact (meanings_grow_trans _ _ _ _ _ M1 M')|]. split; [exact Pre'|exact Eq'].
      * right. exists res. split; [exact Eq|]. split; [exact NN|].
        eapply outcome_heap; [exact E1|]. apply (outcome_grow m1 F d d1 H1 types TUnit _ _ M1 (fun g _ I => I) FAv).
        apply (outcome_frame F F1 d1 H1 types TUnit _ _ Same).
        eapply AR.O.outcome_breaks; [|exact Out]. tauto.
    + right. exists res. cbn [app]. split; [exact Eq|]. split; [exact NN|].
      eapply AR.O.outcome_breaks; [|exact Out]. tauto.
Qed.

Lemma frame_avoids_nil : forall F, FrameAvoids [] F.
Proof. intros F. split; intros t _ g []. Qed.

Lemma type_bstep : forall r F C t0 tau, (Formed F C -> Formed F (A.set_ty C t0 (A.canon C tau))) ->
  BindStep J r F C F (A.set_ty C t0 (A.canon C tau)) False [] (BType t0 (A.canon C tau)).
Proof.
  intros r F C t0 tau FS. split; [apply frame_same_refl|]. split; [apply frame_avoids_nil|].
  intros j d types H g st Lj LJ Pre0 rest.
  left. exists (bind_type d t0 (A.canon C tau)), g, st, H. split; [intros a c L; exact L|].
  split; [apply meanings_grow_refl; reflexivity|]. split; [|reflexivity].
  destruct Pre0 as [Fr [[Lo [Re Al]] [L [St [Real Fmd]]]]].
  refine (conj Fr (conj (conj Lo (conj Re _)) (conj L (conj St (conj Real (FS Fmd)))))).
  intros x tx Lx. unfold cmeaning, output_meaning. cbn [type_aliases bind_type A.set_ty A.c_ty lookup_subst A.nlook] in Lx |- *.
  destruct (Nat.eqb_spec x t0) as [->|Ne]; [injection Lx as <-; reflexivity|]. exact (Al x tx Lx).
Qed.

Lemma val_bstep : forall k, Below k -> forall r, r <= S k -> forall F C sp v declared y x xt a,
  LegacyBodyJudgments.Ex W Prog F C y (Some declared) x xt -> A.fits xt declared ->
  (Formed F C -> Formed F (A.provide C (IVal v) a (A.set_val C v declared))) ->
  BindStep J r F C F (A.provide C (IVal v) a (A.set_val C v declared)) (ItemBreaks F (sp, Some y)) [] (BValue v x).
Proof.
  intros k Bel r Lr F C sp v declared y x xt a Dy Fits FS. split; [apply frame_same_refl|]. split; [apply frame_avoids_nil|].
  intros j d types H g st Lj LJ Pre0 rest. rewrite bindings_value_eq.
  destruct (Bel j ltac:(lia)) as [SEx _].
  pose proof (AR.O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits
    (SEx F C y _ x xt Dy d types H g st (realizes_pre_lower j r F C d types H g st Pre0 ltac:(lia)))) as Out.
  destruct (AR.O.I.eval j P d g st x) as [[w| | | | | | |] g1 st1] eqn:E;
    try (right; eexists; split; [reflexivity|]; split; [intros v' Ev; discriminate|];
         eapply outcome_pass; [exact (fun b => A.IBExpr sp b)|intros v' Ev; discriminate|exact Out]).
  destruct Out as [H1 [E1 [S1 [L1 T1]]]].
  left. exists (install_provision (IVal v) (ValueProvision w) d), g1, st1, H1.
  split; [exact E1|]. split; [apply meanings_grow_refl; reflexivity|]. split; [|reflexivity].
  pose proof Pre0 as [Frame [Ctx [_ [_ [Real Fmd]]]]]. pose proof (FS Fmd) as FmdP.
  unfold A.set_val in FmdP |- *. rewrite (provide_set C (IVal v) (A.ProvidedVal declared []) a ltac:(intros; eauto)) in FmdP |- *.
  split; [exact Frame|]. split; [exact Ctx|]. split; [exact L1|]. split; [exact S1|]. split; [|exact FmdP].
  apply (realizes_set r C d H1 (IVal v) (A.ProvidedVal declared a)).
  - exact (realizes_extends _ _ _ _ _ E1 Real).
  - intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
  - reflexivity.
  - intros k0 Ne. apply install_other. exact Ne.
  - exists w. split; [cbn; rewrite Nat.eqb_refl; reflexivity|exact T1].
Qed.

Lemma fn_bstep : forall k, Below k -> forall r, r <= S k -> forall F C sp f g sg pg types sm a,
  LegacyBodyJudgments.FnProvider W Prog F C (AEPath pg None) sg None g types sm ->
  (Formed F C -> Formed F (A.provide C (IFn f) a (A.set_fn C f sg))) ->
  BindStep J r F C F (A.provide C (IFn f) a (A.set_fn C f sg)) (ItemBreaks F (sp, Some (AEPath pg None)))
    [] (BFunction f g types sm).
Proof.
  intros k Bel r Lr F C sp f g sg pg types sm a Prov FS. split; [apply frame_same_refl|]. split; [apply frame_avoids_nil|].
  intros j d types0 H lg st Lj LJ Pre0 rest. rewrite bindings_function_eq.
  pose proof Pre0 as [Frame [Ctx [L [St [Real Fmd]]]]].
  destruct (provider_callable k Bel r Lr F C pg sg g types sm d H Prov Real) as [dc [Sel Ok]].
  rewrite Sel. left.
  set (d' := install_provision (IFn f) (FunctionClosure g (typed_capture d types dc)) d).
  exists d', lg, st, H. split; [intros a0 c L0; exact L0|]. split; [apply meanings_grow_refl; reflexivity|]. split; [|reflexivity].
  pose proof (FS Fmd) as FmdP.
  unfold A.set_fn in FmdP |- *. rewrite (provide_set C (IFn f) (A.ProvidedFn sg []) a ltac:(intros; discriminate)) in FmdP |- *.
  split; [exact Frame|]. split; [exact Ctx|]. split; [exact L|]. split; [exact St|]. split; [|exact FmdP].
  apply (realizes_set r C d H (IFn f) (A.ProvidedFn sg a)).
  - exact Real.
  - intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
  - reflexivity.
  - intros k0 Ne. apply install_other. exact Ne.
  - exists (FunctionClosure g (typed_capture d types dc)). split; [cbn; rewrite Nat.eqb_refl; reflexivity|].
    exact (callable_transfer r r H d d' _ _ _ _ _ (le_n _) eq_refl eq_refl Ok).
Qed.

(* ---- lambda prefixes as steps ------------------------------------------------- *)

Definition cap_cx (C : A.cx) (qs : list (name * vsym)) : A.cx :=
  fold_left (fun C' q => set_cx C' (IVal (snd q)) (A.ProvidedVal (capty C (fst q)) [])) qs C.

(* A narrower context that keeps every provision of C is realized too. *)
Lemma realizes_narrow : forall n C C2 d H, Realizes n C2 d H -> A.c_sub C2 = A.c_sub C -> A.c_merge C2 = A.c_merge C ->
  PublicCallRealize.KeysMatch (A.c_provisions C) ->
  (forall k, In k (map fst (A.c_provisions C)) -> A.ilook k (A.c_provisions C2) = A.ilook k (A.c_provisions C)) ->
  Realizes n C d H.
Proof.
  intros n C C2 d H [Abs [[Vals [Fns [Mths Funs]]] [Section KM2]]] Sub Merge KM Same.
  assert (Canon : forall t, A.canon C2 t = A.canon C t) by (intros t; unfold A.canon; rewrite Sub; reflexivity).
  assert (Key : forall k p, A.ilook k (A.c_provisions C) = Some p -> A.ilook k (A.c_provisions C2) = Some p).
  { intros k p L. rewrite Same; [exact L|]. change k with (fst (k, p)). apply in_map. exact (ilook_in _ _ _ L). }
  split; [intros t u E; apply Abs; now rewrite Merge |].
  split; [|split; [|exact KM]].
  2: { eapply PublicContextFacts.section_same_fields; [symmetry; exact Sub | symmetry; exact Merge | exact Section]. }
  split; [|split; [|split]].
  - intros v t Found. change (A.c_vals C) with (vals_of (A.c_provisions C)) in Found.
    rewrite (vals_by_ilook _ v KM) in Found.
    destruct (A.ilook (IVal v) (A.c_provisions C)) as [[t' a|g a|g a|fx]|] eqn:Il; try discriminate. injection Found as <-.
    apply Vals. change (A.c_vals C2) with (vals_of (A.c_provisions C2)). rewrite (vals_by_ilook _ v KM2), (Key _ _ Il).
    reflexivity.
  - intros f g Found. change (A.c_fns C) with (fns_of (A.c_provisions C)) in Found.
    rewrite (fns_by_ilook _ f KM) in Found.
    destruct (A.ilook (IFn f) (A.c_provisions C)) as [[t' a|g' a|g' a|fx]|] eqn:Il; try discriminate. injection Found as ->.
    apply Fns. change (A.c_fns C2) with (fns_of (A.c_provisions C2)). rewrite (fns_by_ilook _ f KM2), (Key _ _ Il).
    reflexivity.
  - intros r m g Found. change (A.c_mths C) with (mths_of (A.c_provisions C)) in Found.
    rewrite (mths_by_ilook _ r m KM) in Found.
    destruct (A.ilook (A.mkey r m) (A.c_provisions C)) as [[t' a|g' a|g' a|fx]|] eqn:Il; try discriminate. injection Found as ->.
    assert (L2 : A.mlook r m (A.c_mths C2) = Some g).
    { change (A.c_mths C2) with (mths_of (A.c_provisions C2)). rewrite (mths_by_ilook _ r m KM2), (Key _ _ Il). reflexivity. }
    destruct (Mths r m g L2) as [pv [Look Ok]]. exists pv. split; [|exact Ok].
    replace (A.rkey C r m) with (A.rkey C2 r m); [exact Look|].
    unfold A.rkey. rewrite !capps_ilook, (Key _ _ Il), Il. cbn [option_map].
    f_equal. apply theta_of_sub. exact Sub.
  - intros u fx Found. change (A.c_funs C) with (funs_of (A.c_provisions C)) in Found.
    rewrite (funs_by_ilook _ u KM) in Found.
    destruct (A.ilook (IFun u) (A.c_provisions C)) as [[t' a|g' a|g' a|fx']|] eqn:Il; try discriminate. injection Found as ->.
    apply (Funs u fx). change (A.c_funs C2) with (funs_of (A.c_provisions C2)). rewrite (funs_by_ilook _ u KM2), (Key _ _ Il).
    reflexivity.
Qed.

(* A captured local's value, bound to its val. *)
(* A captured local's val keeps the context formed. *)
Lemma cap_formed : forall F C n v, A.slook n (A.c_locals C) <> None -> Formed F C ->
  Formed F (set_cx C (IVal v) (A.ProvidedVal (capty C n) [])).
Proof.
  intros F C n v L [Fc [Ff Cs]].
  change (set_cx C (IVal v) (A.ProvidedVal (capty C n) [])) with (A.set_val C v (capty C n)).
  split; [|split; [exact Ff|]].
  2: { apply (PublicScoping.set_val_scoped W F C v _ Cs). unfold capty.
       destruct (A.slook n (A.c_locals C)) as [[t mu]|] eqn:Ls; [|congruence].
       exact (proj1 Cs n t mu (PublicFormation.slook_in _ _ _ _ Ls)). }
  apply PublicFormation.set_provision_formed; [exact Fc|exact Logic.I| |intros r m th E; discriminate].
  unfold capty. destruct (A.slook n (A.c_locals C)) as [[t mu]|] eqn:Ls; [|congruence].
  pose proof (PublicFormation.cf_locals W _ (proj1 Fc) n t mu (PublicFormation.slook_in _ _ _ _ Ls)) as Ot.
  split; [intros g E; injection E as <-; split; [constructor|exact Ot]|intros x t' []].
Qed.

Lemma cap_bstep : forall r F C n v, A.Named F n -> A.slook n (A.c_locals C) <> None ->
  A.ilook (IVal v) (A.c_provisions C) = None ->
  BindStep J r F C F (set_cx C (IVal v) (A.ProvidedVal (capty C n) [])) False [] (BValue v (EVar (A.var_of F n))).
Proof.
  intros r F C n v Named Loc Out. split; [apply frame_same_refl|]. split; [apply frame_avoids_nil|].
  intros j d types H g st Lj LJ Pre0 rest. rewrite bindings_value_eq.
  destruct j as [|j]; [right; eexists; split; [reflexivity|]; split; [intros v' E; discriminate|exact Logic.I]|].
  rewrite eval_var_eq.
  pose proof Pre0 as [Frame [[Locals Recv] [Lt [St [Real Fmd]]]]].
  destruct (A.slook n (A.c_locals C)) as [[t mu]|] eqn:L; [|congruence].
  destruct (locals_lookup W P H g types _ _ Lt (Locals _ t mu L Named)) as [w [Look T]]. rewrite Look.
  left. exists (install_provision (IVal v) (ValueProvision w) d), g, st, H.
  split; [intros a c L0; exact L0|]. split; [apply meanings_grow_refl; reflexivity|]. split; [|reflexivity].
  split; [exact Frame|]. split; [exact (conj Locals Recv)|]. split; [exact Lt|]. split; [exact St|].
  split; [|exact (cap_formed F C n v ltac:(rewrite L; discriminate) Fmd)].
  apply (realizes_set r C d H (IVal v) (A.ProvidedVal (capty C n) [])).
  - exact Real.
  - intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
  - reflexivity.
  - intros k0 Ne. apply install_other. exact Ne.
  - exists w. split; [cbn; rewrite Nat.eqb_refl; reflexivity|]. unfold capty. rewrite L. exact T.
Qed.

Lemma prefix_chain : forall k F C qs,
  (forall q, In q qs -> A.Named F (fst q) /\ A.slook (fst q) (A.c_locals C) <> None /\
     A.ilook (IVal (snd q)) (A.c_provisions C) = None) ->
  NoDup (map snd qs) ->
  Chain k F C F (cap_cx C qs) False [] (map (fun q => BValue (snd q) (EVar (A.var_of F (fst q)))) qs).
Proof.
  intros k F C qs. revert C. induction qs as [|[n v] rest IH]; intros C Ok Nd; [constructor; apply frame_same_refl|].
  inversion Nd as [|? ? Out Nd']; subst. cbn [map].
  destruct (Ok (n, v) (or_introl eq_refl)) as [Named [Loc Unp]].
  change ([] : list nat) with ([] ++ [] : list nat)%list.
  apply (ChainCons k F C F (set_cx C (IVal v) (A.ProvidedVal (capty C n) [])) F _ False False False [] [] _ _
    (fun r _ => cap_bstep r F C n v Named Loc Unp)); [|tauto].
  apply (IH (set_cx C (IVal v) (A.ProvidedVal (capty C n) []))); [|exact Nd'].
  intros q Iq. destruct (Ok q (or_intror Iq)) as [N [Lq Uq]]. split; [exact N|]. split; [exact Lq|].
  cbn [set_cx A.c_provisions]. rewrite ilook_set.
  destruct (item_eqb (IVal (snd q)) (IVal v)) eqn:E; [|exact Uq].
  apply item_eqb_spec in E. injection E as E. exfalso. apply Out. rewrite <- E. apply in_map. exact Iq.
Qed.

Lemma cap_cx_fields : forall C qs B,
  let Cm := fold_left (fun C' q => set_cx C' (IVal (snd q)) (A.ProvidedVal (capty C (fst q)) [])) qs B in
  A.c_ty Cm = A.c_ty B /\ A.c_sub Cm = A.c_sub B /\ A.c_locals Cm = A.c_locals B.
Proof.
  intros C qs. induction qs as [|q rest IH]; intros B; [repeat split|]. cbn [fold_left].
  destruct (IH (set_cx B (IVal (snd q)) (A.ProvidedVal (capty C (fst q)) []))) as [E1 [E2 E3]].
  cbn zeta in *. rewrite E1, E2, E3. repeat split.
Qed.

Lemma cap_cx_merge : forall C qs, A.c_merge (cap_cx C qs) = A.c_merge C.
Proof.
  intros C qs. unfold cap_cx.
  generalize C at 2 3. induction qs as [|q rest IH]; intros B; [reflexivity |].
  cbn [fold_left]. rewrite IH. reflexivity.
Qed.

Lemma cap_cx_other : forall C qs B k, (forall q, In q qs -> k <> IVal (snd q)) ->
  A.ilook k (A.c_provisions (fold_left (fun C' q => set_cx C' (IVal (snd q)) (A.ProvidedVal (capty C (fst q)) [])) qs B)) =
  A.ilook k (A.c_provisions B).
Proof.
  intros C qs. induction qs as [|q rest IH]; intros B k Out; [reflexivity|]. cbn [fold_left].
  rewrite IH by (intros q' Iq; apply Out; right; exact Iq). cbn [set_cx A.c_provisions]. rewrite ilook_set.
  destruct (item_eqb k (IVal (snd q))) eqn:E; [|reflexivity].
  apply item_eqb_spec in E. exfalso. exact (Out q (or_introl eq_refl) E).
Qed.

Lemma cap_cx_val : forall C qs B n v, NoDup (map snd qs) -> In (n, v) qs ->
  A.ilook (IVal v) (A.c_provisions (fold_left (fun C' q => set_cx C' (IVal (snd q)) (A.ProvidedVal (capty C (fst q)) [])) qs B)) =
  Some (A.ProvidedVal (capty C n) []).
Proof.
  intros C qs. induction qs as [|q rest IH]; intros B n v Nd Iq; [contradiction|].
  inversion Nd as [|? ? Out Nd']; subst. cbn [fold_left]. destruct Iq as [Eq|Iq]; [subst q|].
  - rewrite cap_cx_other.
    + cbn [set_cx A.c_provisions snd fst]. rewrite ilook_set, (proj2 (item_eqb_spec _ _) eq_refl). reflexivity.
    + intros q' Iq' E. injection E as E. apply Out. cbn. rewrite E. apply in_map. exact Iq'.
  - exact (IH _ n v Nd' Iq).
Qed.

Lemma cap_cx_incl : forall C qs B k p, In (k, p) (A.c_provisions B) -> (forall q, In q qs -> k <> IVal (snd q)) ->
  In (k, p) (A.c_provisions (fold_left (fun C' q => set_cx C' (IVal (snd q)) (A.ProvidedVal (capty C (fst q)) [])) qs B)).
Proof.
  intros C qs. induction qs as [|q rest IH]; intros B k p I Out; [exact I|]. cbn [fold_left].
  apply IH; [|intros q' Iq; apply Out; right; exact Iq].
  cbn [set_cx A.c_provisions set_list]. right. apply filter_In. split; [exact I|].
  cbn [fst]. destruct (item_eqb (IVal (snd q)) k) eqn:E; [|reflexivity].
  apply item_eqb_spec in E. exfalso. exact (Out q (or_introl eq_refl) (eq_sym E)).
Qed.

Lemma in_ilook : forall k p (l : list (item * A.provision)), In (k, p) l -> A.ilook k l <> None.
Proof.
  intros k p l. induction l as [|[k' p'] r IH]; intros I; [contradiction|]. cbn.
  destruct (item_eqb k k') eqn:E; [discriminate|]. destruct I as [Eq|I]; [|exact (IH I)].
  injection Eq as -> ->. rewrite (proj2 (item_eqb_spec _ _) eq_refl) in E. discriminate.
Qed.

Lemma fold_bind_values : forall F (qs : list (name * vsym)) x,
  fold_right (fun q x => bind_value_expression (snd q) (EVar (A.var_of F (fst q))) x) x qs =
  fold_right (fun b x => EBind b x) x (map (fun q => BValue (snd q) (EVar (A.var_of F (fst q)))) qs).
Proof. intros F qs x. induction qs as [|q r IH]; cbn; [reflexivity|]. rewrite IH. reflexivity. Qed.

(* A function lambda bind: its prefix of captured values, then the closure. *)
Lemma lambda_fn_chain : forall k, Below k -> forall F C sp f sg ps rt body pid pre sm a,
  A.SigOf W f (A.c_ty C) None sg ->
  LegacyBodyJudgments.LambdaProvider W Prog F C ps rt body sg None pid pre sm ->
  (Formed F C -> Formed F (A.provide C (IFn f) a (A.set_fn C f sg))) ->
  exists bs, (forall x, pre (bind_function_expression f pid (A.lexical_capture C) sm x) =
                        fold_right (fun b x => EBind b x) x bs) /\
    Chain k F C F (A.provide C (IFn f) a (A.set_fn C f sg)) (ItemBreaks F (sp, Some (AELam ps rt body))) [] bs.
Proof.
  intros k Bel F C sp f sg ps rt body pid pre sm a Sig LP FS.
  remember (@None ty) as none eqn:NoneE in LP.
  destruct LP as [F C ps rt body sg this pid caps capvals Cl Fl lbody ltype keys sm Len NdP Ann RtAnn Captures
    FreshP ClE FMod FRet FThis FLoop FScope Num NamedFl NamedF Blk Fits BodyOf KeysIff NdK SmE Origin].
  destruct Captures as (CapsIff & NdC & LenV & scope_after & Allocated).
  destruct (proj1 (AllocationFacts.capture_allocation_contract W C capvals) (proj1 Allocated)) as (NdV & FreshV & Unprov).
  subst sm this.
  assert (ClE' : Cl = fold_left (cl_fold C) (combine caps capvals) (A.Cx (A.c_ty C) (A.c_sub C) (A.c_merge C) (A.c_provisions C) []))
    by exact ClE.
  set (qs := combine caps capvals) in *.
  assert (Fq : map fst qs = caps) by (apply fst_combine; congruence).
  assert (Sq : map snd qs = capvals) by (apply snd_combine; congruence).
  assert (Ndq : NoDup (map snd qs)) by (rewrite Sq; exact NdV).
  assert (InCaps : forall q, In q qs -> In (fst q) caps) by (intros q Iq; rewrite <- Fq; apply in_map, Iq).
  assert (InVals : forall q, In q qs -> In (snd q) capvals) by (intros q Iq; rewrite <- Sq; apply in_map, Iq).
  assert (CapsIn : forall n, In n caps -> A.slook n (A.c_locals C) <> None /\ ~ In n (map fst ps))
    by (intros n0 I0; apply CapsIff in I0 as [_ [L [O _]]]; split; assumption).
  rewrite Forall_forall in NamedF, Unprov.
  assert (NotCap : forall k0 p0, In (k0, p0) (A.c_provisions C) -> forall q, In q qs -> k0 <> IVal (snd q)).
  { intros k0 p0 I q Iq E. subst k0. exact (in_ilook _ _ _ I (Unprov _ (InVals q Iq))). }
  exists (map (fun q => BValue (snd q) (EVar (A.var_of F (fst q)))) qs ++ [BFunction f pid (A.lexical_capture C)
    (map (fun k => (A.rkey_of Cl k, A.rkey_of C k)) keys)])%list.
  split.
  { intros x. unfold A.translate_lambda_captures. cbn beta. rewrite fold_bind_values, fold_right_app. reflexivity. }
  apply (chain_app k F C F (cap_cx C qs) F (A.provide C (IFn f) a (A.set_fn C f sg)) False
    (ItemBreaks F (sp, Some (AELam ps rt body))) (ItemBreaks F (sp, Some (AELam ps rt body))) [] []);
    [| |tauto].
  - apply prefix_chain; [|exact Ndq]. intros q Iq. split; [exact (NamedF _ (InCaps q Iq))|].
    split; [exact (proj1 (CapsIn _ (InCaps q Iq)))|exact (Unprov _ (InVals q Iq))].
  - apply (ChainCons k F (cap_cx C qs) F (A.provide C (IFn f) a (A.set_fn C f sg)) F (A.provide C (IFn f) a (A.set_fn C f sg)) (ItemBreaks F (sp, Some (AELam ps rt body))) False (ItemBreaks F (sp, Some (AELam ps rt body))) [] []);
      [|constructor; apply frame_same_refl|tauto].
    intros r Lr. split; [apply frame_same_refl|]. split; [apply frame_avoids_nil|].
    intros j d types H g st Lj LJ Pre0 rest. rewrite bindings_function_eq.
    pose proof Pre0 as [Frame [Ctx [L [St [RealM FmdM]]]]].
    destruct (cap_cx_fields C qs C) as [Ety [Esub Eloc]]. cbn zeta in Ety, Esub, Eloc.
    pose proof RealM as [_ [_ [_ KMm]]].
    assert (KMc : PublicCallRealize.KeysMatch (A.c_provisions C)).
    { intros k0 p0 I. apply KMm. exact (cap_cx_incl C qs C k0 p0 I (NotCap k0 p0 I)). }
    assert (RealC : Realizes r C d H).
    { apply (realizes_narrow r C (cap_cx C qs) d H RealM Esub (cap_cx_merge C qs) KMc).
      intros k0 Ik. apply in_map_iff in Ik as [[k1 p1] [<- I1]]. cbn [fst].
      exact (cap_cx_other C qs C k1 (NotCap k1 p1 I1)). }
    assert (Inst : Installed C qs d H).
    { intros q Iq. pose proof RealM as [_ [[Vals _] _]].
      assert (Lv : A.nlook (snd q) (A.c_vals (cap_cx C qs)) = Some (capty C (fst q))).
      { change (A.c_vals (cap_cx C qs)) with (vals_of (A.c_provisions (cap_cx C qs))).
        rewrite (vals_by_ilook _ _ KMm). unfold cap_cx. rewrite (cap_cx_val C qs C (fst q) (snd q) Ndq ltac:(destruct q; exact Iq)).
        reflexivity. }
      exact (Vals _ _ Lv). }
    destruct (lambda_select r C qs Cl d H keys ClE' RealC Inst Ndq
      (fun q Iq => proj1 (CapsIn _ (InCaps q Iq))) KeysIff) as [dc Sel].
    rewrite Sel. left.
    assert (FmdC : Formed F C).
    { apply (formed_narrow F (cap_cx C qs) C (eq_sym Ety) (eq_sym Esub) (eq_sym (cap_cx_merge C qs)) (eq_sym Eloc)); [|exact FmdM].
      intros [k1 p1] I1. exact (cap_cx_incl C qs C k1 p1 I1 (NotCap k1 p1 I1)). }
    pose proof (PublicFormation.sigof_ok W _ _ _ _ Sig (PublicFormation.cf_ty _ _ (proj1 (proj1 FmdC))) (PublicFormation.optok_none W)) as SgOk.
    pose proof (lambda_callable k Bel r Lr C ps sg None body pid caps capvals Cl Fl lbody ltype d H dc keys
      Len CapsIn NdC LenV NdV ClE' FRet FThis FLoop Num NamedFl Blk Fits BodyOf KeysIff Sel RealC Inst
      (sigof_scoped f C sg Sig) (proj1 FmdC) SgOk (PublicFormation.optok_none W)
      (proj1 (proj2 (proj2 FmdC))) (proj1 (proj2 (proj2 (proj2 FmdC))))
      (PublicScoping.sig_scoped_none W C f sg Sig) (proj2 (proj2 (proj2 (proj2 (proj2 FmdC)))))) as Ok.
    cbn [this_list app] in Ok.
    set (d'' := install_provision (IFn f) (FunctionClosure pid (typed_capture d (A.lexical_capture C) dc)) d).
    exists d'', g, st, H. split; [intros a0 c L0; exact L0|]. split; [apply meanings_grow_refl; reflexivity|].
    split; [|reflexivity].
    pose proof (FS FmdC) as FmdP.
    unfold A.set_fn in FmdP |- *. rewrite (provide_set C (IFn f) (A.ProvidedFn sg []) a ltac:(intros; discriminate)) in FmdP |- *.
    split; [exact Frame|]. split.
    { destruct Ctx as [Loc [Rcv Al]]. split; [|split; [exact Rcv|apply (alias_ty (cap_cx C qs)); [exact (eq_sym Ety)|exact Al]]].
      intros n t mu Found Nm. apply (Loc n t mu); [|exact Nm].
      change (A.c_locals (cap_cx C qs)) with (A.c_locals (fold_left (fun C' q => set_cx C' (IVal (snd q))
        (A.ProvidedVal (capty C (fst q)) [])) qs C)). rewrite Eloc. exact Found. }
    split; [exact L|]. split; [exact St|]. split; [|exact FmdP].
    apply (realizes_set r C d H (IFn f) (A.ProvidedFn sg a)).
    + exact RealC.
    + intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
    + reflexivity.
    + intros k0 Ne. apply install_other. exact Ne.
    + exists (FunctionClosure pid (typed_capture d (A.lexical_capture C) dc)).
      split; [cbn; rewrite Nat.eqb_refl; reflexivity|].
      exact (callable_transfer r r H d d'' _ _ _ _ _ (le_n _) eq_refl eq_refl Ok).
Qed.

(* Installing a callable method closure under a bound key. *)
Lemma meth_install : forall r F C d types H g st rt m sg ap pv,
  (Formed F C -> Formed F (A.provide C (A.mkey (A.canon C rt) m) ap (A.set_mth C (A.canon C rt) m sg))) ->
  Pre r F C d types H g st -> CallableOk r H d (A.canon C rt :: A.sg_params sg) (A.sg_ret sg) pv ->
  Pre r F (A.provide C (A.mkey (A.canon C rt) m) ap (A.set_mth C (A.canon C rt) m sg))
    (install_provision (IMth (A.canon C rt) m (A.theta_of C ap)) pv d) types H g st.
Proof.
  intros r F C d types H g st rt m sg ap pv FS [Frame [Ctx [L [St [Real Fmd]]]]] Ok.
  pose proof (FS Fmd) as FmdP. unfold A.set_mth in FmdP |- *.
  rewrite (provide_set C (A.mkey (A.canon C rt) m) (A.ProvidedMethod sg []) ap ltac:(intros; discriminate)) in FmdP |- *.
  assert (Key : A.rkey (set_cx C (A.mkey (A.canon C rt) m) (A.ProvidedMethod sg ap)) (A.canon C rt) m =
                IMth (A.canon C rt) m (A.theta_of C ap)).
  { unfold A.rkey. rewrite capps_ilook. unfold set_cx. cbn [A.c_provisions].
    rewrite ilook_set, (proj2 (item_eqb_spec _ _) eq_refl). cbn [option_map A.provision_inst].
    apply f_equal, theta_of_sub. reflexivity. }
  set (d' := install_provision (IMth (A.canon C rt) m (A.theta_of C ap)) pv d).
  split; [exact Frame|]. split; [exact Ctx|]. split; [exact L|]. split; [exact St|]. split; [|exact FmdP].
  apply (realizes_set r C d H (A.mkey (A.canon C rt) m) (A.ProvidedMethod sg ap)).
  - exact Real.
  - intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
  - reflexivity.
  - intros k0 Ne. apply install_other. intro E. apply Ne. rewrite E. unfold A.mkey. cbn [A.rkey_of].
    symmetry. exact Key.
  - cbn [NewOk]. exists pv.
    split; [transitivity (lookup_provision (IMth (A.canon C rt) m (A.theta_of C ap)) d'); [f_equal; exact Key|]|].
    { unfold d'. rewrite lookup_install, (proj2 (item_eqb_spec _ _) eq_refl). reflexivity. }
    exact (callable_transfer r r H d d' _ _ _ _ _ (le_n _) eq_refl eq_refl Ok).
Qed.

(* After a lambda's prefix: the enclosing context, and the captured vals. *)
Lemma pre_from_cap : forall r F C qs d types H g st,
  NoDup (map snd qs) -> (forall q, In q qs -> A.ilook (IVal (snd q)) (A.c_provisions C) = None) ->
  Pre r F (cap_cx C qs) d types H g st -> Pre r F C d types H g st /\ Installed C qs d H.
Proof.
  intros r F C qs d types H g st Ndq Unp Pre0.
  pose proof Pre0 as [Frame [Ctx [L [St [RealM FmdM]]]]].
  destruct (cap_cx_fields C qs C) as [Ety [Esub Eloc]]. cbn zeta in Ety, Esub, Eloc.
  pose proof RealM as [_ [_ [_ KMm]]].
  assert (NotCap : forall k0 p0, In (k0, p0) (A.c_provisions C) -> forall q, In q qs -> k0 <> IVal (snd q)).
  { intros k0 p0 I q Iq E. subst k0. exact (in_ilook _ _ _ I (Unp q Iq)). }
  assert (KMc : PublicCallRealize.KeysMatch (A.c_provisions C)).
  { intros k0 p0 I. apply KMm. exact (cap_cx_incl C qs C k0 p0 I (NotCap k0 p0 I)). }
  split.
  - split; [exact Frame|]. split.
    { destruct Ctx as [Loc [Rcv Al]]. split; [|split; [exact Rcv|apply (alias_ty (cap_cx C qs)); [exact (eq_sym Ety)|exact Al]]].
      intros n t mu Found Nm. apply (Loc n t mu); [|exact Nm].
      change (A.c_locals (cap_cx C qs)) with (A.c_locals (fold_left (fun C' q => set_cx C' (IVal (snd q))
        (A.ProvidedVal (capty C (fst q)) [])) qs C)). rewrite Eloc. exact Found. }
    split; [exact L|]. split; [exact St|]. split.
    + apply (realizes_narrow r C (cap_cx C qs) d H RealM Esub (cap_cx_merge C qs) KMc).
      intros k0 Ik. apply in_map_iff in Ik as [[k1 p1] [<- I1]]. cbn [fst].
      exact (cap_cx_other C qs C k1 (NotCap k1 p1 I1)).
    + apply (formed_narrow F (cap_cx C qs) C (eq_sym Ety) (eq_sym Esub) (eq_sym (cap_cx_merge C qs)) (eq_sym Eloc)); [|exact FmdM].
      intros [k1 p1] I1. exact (cap_cx_incl C qs C k1 p1 I1 (NotCap k1 p1 I1)).
  - intros q Iq. pose proof RealM as [_ [[Vals _] _]].
    assert (Lv : A.nlook (snd q) (A.c_vals (cap_cx C qs)) = Some (capty C (fst q))).
    { change (A.c_vals (cap_cx C qs)) with (vals_of (A.c_provisions (cap_cx C qs))).
      rewrite (vals_by_ilook _ _ KMm). unfold cap_cx. rewrite (cap_cx_val C qs C (fst q) (snd q) Ndq ltac:(destruct q; exact Iq)).
      reflexivity. }
    exact (Vals _ _ Lv).
Qed.

(* A method bind as a chain: an attached method, a wrapper, or a lambda
   (its prefix of captured values, then its closure). *)
Lemma meth_chain : forall k, Below k -> forall F C pa dn a y r m bs rt sg target pre sm ap types,
  A.resolve_sym W (A.fr_mod F) pa = Some r -> A.MethodNamed W (A.fr_mod F) r dn m ->
  A.Brackets W (A.fr_mod F) (A.c_ty C) m a bs ->
  A.TyOf W (A.fr_mod F) (bs ++ A.c_ty C)%list None (ATRef pa None) rt ->
  A.SigOf W m (bs ++ A.c_ty C)%list (Some rt) sg ->
  LegacyBodyJudgments.MethProvider W Prog F C y sg rt target types pre sm ->
  A.AppOf W m (bs ++ A.c_ty C)%list ap ->
  (Formed F C -> Formed F (A.provide C (A.mkey (A.canon C rt) m) ap (A.set_mth C (A.canon C rt) m sg))) ->
  exists binds, (forall x, pre (bind_method_expression (A.canon C rt) m (A.theta_of C ap) target types sm x) =
                           fold_right (fun b x => EBind b x) x binds) /\
    Chain k F C F (A.provide C (A.mkey (A.canon C rt) m) ap (A.set_mth C (A.canon C rt) m sg))
      (ItemBreaks F (Spec (Some pa) (Some dn) a, Some y)) [] binds.
Proof.
  intros k Bel F C pa dn a y r m bs rt sg target pre sm ap types R Named Br Ty Sig Prov App FS.
  pose proof (method_sig_scoped F C m a bs pa rt sg Br Ty Sig) as SigSc.
  set (Bi := ItemBreaks F (Spec (Some pa) (Some dn) a, Some y)).
  set (Cres := A.provide C (A.mkey (A.canon C rt) m) ap (A.set_mth C (A.canon C rt) m sg)).
  assert (Single : forall b, (forall rr, rr <= S k -> BindStep J rr F C F Cres Bi [] b) -> Chain k F C F Cres Bi [] [b]).
  { intros b Step. apply (ChainCons k F C F Cres F Cres Bi False Bi [] [] b []); [exact Step|constructor; apply frame_same_refl|tauto]. }
  destruct Prov as [F C rp n0 rg att rt sg Ca ns sm th rargs Cd Rp Att Def Canon -> Ctx Sat CT SigA RB
    |F C pw sg rt g sm w n0 Cg nsg types FP Ctx Len Fresh Body|F C ps r0 body sg rt pid pre sm LP].
  - (* attached *)
    exists [BMethod (A.canon C rt) m (A.theta_of C ap) att (TypeCapture th th) sm]. split; [reflexivity|].
    apply Single. intros rr Lr. split; [apply frame_same_refl|]. split; [apply frame_avoids_nil|].
    intros j d types0 H g st Lj LJ Pre0 rest. rewrite bindings_method_eq.
    assert (Keys : map fst rargs = A.w_reqs W rg).
    { destruct Def as [rn [ps [rt0 [b Def]]]]. unfold A.ast_of in Def.
      destruct (A.w_decl W att) as [dcl|] eqn:Decl; [|discriminate]. injection Def as Ast.
      pose proof (all_fn_elab att dcl (RAttached rn) n0 ps rt0 b Decl Ast dcl (RAttached rn) n0 ps rt0 b Decl Ast)
        as (C0 & ns0 & this & g0 & F0 & x0 & t0 & _ & Rcv & _).
      destruct Rcv as [r' [args0 [Att0 [Kind _]]]].
      pose proof (attached_unique rg r' n0 att Att Att0) as <-.
      pose proof Pre0 as [_ [_ [_ [_ [_ [FcB _]]]]]].
      assert (TmB : PublicFormation.TmOk W (bs ++ A.c_ty C)%list).
      { apply PublicFormation.tmok_app; [|exact (PublicFormation.cf_ty _ _ (proj1 FcB))].
        exact (PublicFormation.br_ok W _ _ _ _ _ Br (PublicFormation.cf_ty _ _ (proj1 FcB))). }
      pose proof (PublicFormation.canon_ok W C rt (proj1 FcB) (PublicFormation.tyof_ok W _ _ _ _ _ Ty TmB (PublicFormation.optok_none W))) as Oc.
      rewrite Canon in Oc. exact (proj1 (PublicFormation.nom_ok_elim W rg rargs Oc)). }
    pose proof Pre0 as [_ [_ [_ [_ [Real _]]]]].
    destruct (attached_provider_callable k Bel rr Lr C rg n0 att rt sg Ca ns sm th rargs d H
      Att Def Canon Keys Ctx Sat CT SigA Real) as [dc [Sel Ok]].
    rewrite Sel. left. eexists _, g, st, H. split; [intros a0 c L0; exact L0|].
    split; [|split; [exact (meth_install rr F C d types0 H g st rt m sg ap _ FS Pre0 Ok)|reflexivity]].
    apply meanings_grow_refl; reflexivity.
  - (* wrapper *)
    exists [BMethod (A.canon C rt) m (A.theta_of C ap) w types sm]. split; [reflexivity|].
    apply Single. intros rr Lr. split; [apply frame_same_refl|]. split; [apply frame_avoids_nil|].
    intros j d types0 H g0 st Lj LJ Pre0 rest. rewrite bindings_method_eq.
    remember (AEPath pw None) as src eqn:Src. remember (@None ty) as none eqn:NoneE.
    destruct FP as [F' C' pa' g' sg' Cg' ns' sm' th R' K Def Ctx' Sat CT SigG|F' C' pa' sg' handle w' Typed Translated].
    2: { exfalso. destruct Translated as [Fresh' _].
         destruct Ctx as (d0 & _ & _ & _ & _ & _ & _ & D & _). unfold A.Fresh in Fresh'. congruence. }
    destruct (PublicContextFacts.function_context_unique W g' Cg' ns' Cg nsg Ctx' Ctx) as [-> ->]. subst n0.
    pose proof Pre0 as [_ [_ [_ [_ [Real _]]]]].
    destruct (callee_realizes_le rr rr C' d H g' Cg nsg sm' th (le_n rr) Real Ctx Sat CT) as [dc [Sel _]].
    pose proof (wrapped_callable k Bel rr Lr C' sg' g' th Cg nsg sm' w (A.canon C' rt) d H dc
      Def Ctx Sat CT SigG Sel Body Real) as Ok.
    rewrite Sel. left. eexists _, g0, st, H. split; [intros a0 c L0; exact L0|].
    split; [|split; [exact (meth_install rr F' C' d types0 H g0 st rt m sg' ap _ FS Pre0 Ok)|reflexivity]].
    apply meanings_grow_refl; reflexivity.
  - (* lambda *)
      remember (Some rt) as recv eqn:RecvE in LP.
    destruct LP as [F C ps r0 body sg this pid caps capvals Cl Fl lbody ltype keys sm Len NdP Ann RtAnn Captures
      FreshP ClE FMod FRet FThis FLoop FScope Num NamedFl NamedF Blk Fits BodyOf KeysIff NdK SmE Origin].
    destruct Captures as (CapsIff & NdC & LenV & scope_after & Allocated).
    destruct (proj1 (AllocationFacts.capture_allocation_contract W C capvals) (proj1 Allocated)) as (NdV & FreshV & Unprov).
    subst sm this.
    assert (ClE' : Cl = fold_left (cl_fold C) (combine caps capvals) (A.Cx (A.c_ty C) (A.c_sub C) (A.c_merge C) (A.c_provisions C) []))
      by exact ClE.
    set (qs := combine caps capvals) in *.
    assert (Fq : map fst qs = caps) by (apply fst_combine; congruence).
    assert (Sq : map snd qs = capvals) by (apply snd_combine; congruence).
    assert (Ndq : NoDup (map snd qs)) by (rewrite Sq; exact NdV).
    assert (InCaps : forall q, In q qs -> In (fst q) caps) by (intros q Iq; rewrite <- Fq; apply in_map, Iq).
    assert (InVals : forall q, In q qs -> In (snd q) capvals) by (intros q Iq; rewrite <- Sq; apply in_map, Iq).
    assert (CapsIn : forall n, In n caps -> A.slook n (A.c_locals C) <> None /\ ~ In n (map fst ps))
      by (intros n0 I0; apply CapsIff in I0 as [_ [L [O _]]]; split; assumption).
    rewrite Forall_forall in NamedF, Unprov.
    set (bm := BMethod (A.canon C rt) m (A.theta_of C ap) pid (A.lexical_capture C)
      (map (fun k => (A.rkey_of Cl k, A.rkey_of C k)) keys)).
    exists (map (fun q => BValue (snd q) (EVar (A.var_of F (fst q)))) qs ++ [bm])%list.
    split; [intros x; unfold A.translate_lambda_captures; cbn beta; rewrite fold_bind_values, fold_right_app; reflexivity|].
    apply (chain_app k F C F (cap_cx C qs) F Cres False Bi Bi [] []); [| |tauto].
    + apply prefix_chain; [|exact Ndq]. intros q Iq. split; [exact (NamedF _ (InCaps q Iq))|].
      split; [exact (proj1 (CapsIn _ (InCaps q Iq)))|exact (Unprov _ (InVals q Iq))].
    + apply (ChainCons k F (cap_cx C qs) F Cres F Cres Bi False Bi [] [] bm []); [|constructor; apply frame_same_refl|tauto].
      intros rr Lr. split; [apply frame_same_refl|]. split; [apply frame_avoids_nil|].
      intros j d types0 H g st Lj LJ Pre0 rest. unfold bm. rewrite bindings_method_eq.
      destruct (pre_from_cap rr F C qs d types0 H g st Ndq (fun q Iq => Unprov _ (InVals q Iq)) Pre0) as [PreC Inst].
      pose proof PreC as [_ [_ [_ [_ [RealC _]]]]]. pose proof (realizes_absorbs_canon _ _ _ _ RealC) as Abs.
      destruct (lambda_select rr C qs Cl d H keys ClE' RealC Inst Ndq
        (fun q Iq => proj1 (CapsIn _ (InCaps q Iq))) KeysIff) as [dc Sel].
      rewrite Sel. left.
      pose proof PreC as [_ [_ [_ [_ [_ [FcC [_ CsC]]]]]]].
      assert (TmC : PublicFormation.TmOk W (bs ++ A.c_ty C)%list).
      { apply PublicFormation.tmok_app; [|exact (PublicFormation.cf_ty _ _ (proj1 FcC))].
        exact (PublicFormation.br_ok W _ _ _ _ _ Br (PublicFormation.cf_ty _ _ (proj1 FcC))). }
      pose proof (PublicFormation.tyof_ok W _ _ _ _ _ Ty TmC (PublicFormation.optok_none W)) as OkRt.
      pose proof (PublicFormation.sigof_ok W _ _ _ _ Sig TmC (PublicFormation.optok_some W _ OkRt)) as SgOk.
      pose proof (lambda_callable k Bel rr Lr C ps sg (Some rt) body pid caps capvals Cl Fl lbody ltype d H dc keys
        Len CapsIn NdC LenV NdV ClE' FRet FThis FLoop Num NamedFl Blk Fits BodyOf KeysIff Sel RealC Inst SigSc
        FcC SgOk (PublicFormation.optok_some W _ OkRt) (proj1 CsC) (proj1 (proj2 CsC))
        (PublicScoping.meth_sig_scoped W C _ _ m a bs _ rt sg Br Ty Sig) (proj2 (proj2 (proj2 CsC)))) as Ok.
      cbn [this_list app] in Ok.
      assert (Ok' : CallableOk rr H d (A.canon C rt :: A.sg_params sg) (A.sg_ret sg)
        (FunctionClosure pid (typed_capture d (A.lexical_capture C) dc))).
      { apply (callable_transfer rr rr H d d (rt :: A.sg_params sg) (A.canon C rt :: A.sg_params sg) (A.sg_ret sg) (A.sg_ret sg) _ (le_n _)); [|reflexivity|exact Ok].
        cbn [map]. f_equal. symmetry. exact (Abs rt). }
      eexists _, g, st, H. split; [intros a0 c L0; exact L0|].
      split; [|split; [exact (meth_install rr F C d types0 H g st rt m sg ap _ FS PreC Ok')|reflexivity]].
      apply meanings_grow_refl; reflexivity.
Qed.




Lemma fold_binds_app : forall bs1 bs2 x,
  fold_right (fun b x => EBind b x) (fold_right (fun b x => EBind b x) x bs2) bs1 =
  fold_right (fun b x => EBind b x) x (bs1 ++ bs2)%list.
Proof. intros bs1 bs2 x. rewrite fold_right_app. reflexivity. Qed.

Lemma single_chain : forall k F C F' C' (B : Prop) m b,
  (forall r, r <= S k -> BindStep J r F C F' C' B m b) -> Chain k F C F' C' B m [b].
Proof.
  intros k F C F' C' B m b Step. rewrite <- (app_nil_r m).
  apply (ChainCons k F C F' C' F' C' B False B m [] b []); [exact Step|constructor; apply frame_same_refl|tauto].
Qed.

(* Each bind item is a chain of bindings. *)
Fixpoint item_chain k (Bel : Below k) (Ap : AppAt J k) F C it w C' F' m (D : LegacyBodyJudgments.BindItem W Prog F C it w C' F' m) {struct D} :
  exists bs, (forall x, w x = fold_right (fun b x => EBind b x) x bs) /\ Chain k F C F' C' (ItemBreaks F it) m bs
with items_chain k (Bel : Below k) (Ap : AppAt J k) F C its w C' F' m (D : LegacyBodyJudgments.BindItems W Prog F C its w C' F' m) {struct D} :
  exists bs, (forall x, w x = fold_right (fun b x => EBind b x) x bs) /\ Chain k F C F' C' (ItemsBreak F its) m bs.
Proof.
  - pose proof (item_formed _ _ _ _ _ _ _ D) as FD. revert FD.
    destruct D as [F C pa t0 rp ra tau R K Pin Ty
                  |F C pa v d0 n0 t00 i declared y x xt a R Dd Ast Ty Dy Fits App
                  |F C pa f g sg pg sm a types R K Sig Prov App
                  |F C pa f sg ps rt body pid pre sm a R K Sig LP App
                  |F C pa dn a y r m bs rt sg target pre sm ap types R Named Br Ty Sig Prov App NoFam
                  |F C pa u dcl n0 ps a r bodies wrap C' F' made R Dcl Ast NotApp Ditems
                  |F C pa a Typed
                  |F C pa u pg h Cu nsu Ch nsh target sm fx rm R1 R2 Abst Hf FCu FCh FixU FixH Agr Same Sm Fx Sub CO
                  |F C pa u pg gg Cu nsu Cg nsg h pins linktarget sm smc fx R1 R2 U FCu FCg FixU Same Binding LB Sm SatC Fx CapF PinA]; intros FD.
    + exists [BType t0 (A.canon C tau)]. split; [reflexivity|].
      apply (chain_weaken k F C F _ False); [|tauto]. apply single_chain. intros r _. apply type_bstep. exact FD.
    + exists [BValue v x]. split; [reflexivity|].
      apply single_chain. intros r Lr. exact (val_bstep k Bel r Lr F C _ v declared y x xt a Dy Fits FD).
    + exists [BFunction f g types sm]. split; [reflexivity|].
      apply single_chain. intros r Lr. exact (fn_bstep k Bel r Lr F C _ f g sg pg types sm a Prov FD).
    + exact (lambda_fn_chain k Bel F C (Spec (Some pa) None None) f sg ps rt body pid pre sm a Sig LP FD).
    + exact (meth_chain k Bel F C pa dn a y r m bs rt sg target pre sm ap types R Named Br Ty Sig Prov App FD).
    + (* `bind F;` for F with a body: its binds, in place *)
      destruct (items_chain k Bel Ap _ _ _ _ _ _ _ Ditems) as [bs [Ew Ch]].
      exists bs. split; [exact Ew|].
      destruct (bind_items_frame _ _ _ _ _ _ _ Ditems) as [_ [Er [Et [El Ev]]]]. cbn in Er, Et, El, Ev.
      apply (chain_reframe_src k (A.with_mod F (A.d_mod dcl) u) C); [|repeat split].
      apply (chain_reframe_tgt k _ C F'); [|split; [|split; [|split]]; cbn; congruence].
      apply (chain_weaken k _ C F' C' (ItemsBreak (A.with_mod F (A.d_mod dcl) u) (List.concat bodies))); [exact Ch|].
      intros [it [Iit Bi]]. eapply A.IBApply; [exact R|exact Dcl|exact Ast|exact Iit|exact Bi].
    + destruct a as [u Cu ns th choices C' ks].
      pose (a := A.Application u Cu ns th choices C' ks).
      pose (gensof := A.app_identity a).
      pose (newgens := A.app_allocated a).
      pose (outs := A.app_outputs a).
      destruct Typed as (R & (fx & Fx & Fixed) & FC & Inp & Th & Needs & FreshG & Uniq & Outs & NdO & OutsIff & PR & _).
      pose (sm := A.satisfaction_map C Cu th ns).
      assert (Sat : A.Satisfies C Cu th ns sm) by (split; [reflexivity|exact Needs]).
      exists [BApply u th (map (fun t => (t, gensof t)) outs) sm (map (A.rkey_of C') ks)]. split; [reflexivity|].
      apply (chain_weaken k F C _ _ False); [|tauto]. apply single_chain.
      exact (Ap F C pa u fx Cu ns sm th gensof C' ks newgens outs R Fx FC Inp Th Fixed Sat FreshG
        Uniq (AllocationFacts.output_identity_conditions _ _ _ _ Outs) NdO OutsIff PR).
    + exists [BCopyFunctor u h sm rm]. split; [reflexivity|].
      apply (chain_weaken k F C _ _ False); [|tauto]. apply single_chain.
      exact (case_BFunCopy k Bel F C pa u pg h Cu nsu Ch nsh target sm fx rm R1 R2 Abst Hf FCu FCh FixU FixH Agr Same Sm Fx Sub CO J).
    + exists [BFunctor u h (A.lexical_capture C) sm smc]. split; [reflexivity|].
      apply (chain_weaken k F C _ _ False); [|tauto]. apply single_chain.
      destruct (link_body_functx u gg h pins Cu nsu Cg nsg LB FCu FCg) as [FCu' FCg'].
      exact (case_BFunClosure k Bel F C pa u pg gg Cu nsu Cg nsg h pins linktarget sm smc fx R1 R2 U FCu' FCg' FixU Same Binding LB Sm SatC Fx PinA J).
  - destruct D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 D1 Rest].
    + exists []. split; [reflexivity|]. constructor. apply frame_same_refl.
    + destruct (item_chain k Bel Ap _ _ _ _ _ _ _ D1) as [bs1 [E1 Ch1]].
      destruct (items_chain k Bel Ap _ _ _ _ _ _ _ Rest) as [bs2 [E2 Ch2]].
      exists (bs1 ++ bs2)%list. split; [intros x; rewrite E1, E2; apply fold_binds_app|].
      destruct (bind_item_frame _ _ _ _ _ _ _ D1) as [Mod _].
      apply (chain_app k F C F1 C1 F2 C2 (ItemBreaks F it) (ItemsBreak F1 r) _ m1 m2 bs1 bs2 Ch1 Ch2).
      unfold ItemsBreak, ItemBreaks. rewrite Mod. intros [b|[it' [I' Bi]]].
      * exists it. split; [left; reflexivity|exact b].
      * exists it'. split; [right; exact I'|exact Bi].
Qed.

End Cap.

(* ---- abstract applications ------------------------------------------------- *)

(* HYPOTHESIS (T15). A linked body that completes provides F's result at
   the application: its result provisions are selected under the caller's
   keys (through θ and the output roles), the output generations are
   fresh in the caller, and the caller's context after the application is
   realized. *)
Definition LinkResultsOk : Prop := forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  AppPremises F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st h dc gfx gg dg n ps a r bodies nsg Cg linktarget Fg wrap Cb Fb made outs' gen Cr ks' binds0 pins d1 d2 d3 g3 st3 H3 j',
  Pre rr F C d types H g st ->
  lookup_provision (IFun u) d = Some (FunctorClosure h (Some (A.link_map Cu Cg linktarget nsg)) dc None) ->
  ClosureOk rr H u gfx (FunctorClosure h (Some (A.link_map Cu Cg linktarget nsg)) dc None) ->
  FixedMeans u d fx gfx ->
  LinkData u gg h dg n ps a r bodies Cu ns Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 pins -> ClosureBinding W u Cu ns Cg nsg linktarget dc gfx -> PinsAgree u Cu Cg dc gfx pins ->
  select_provisions d sm = Some d1 ->
  select_provisions d1 (A.link_map Cu Cg linktarget nsg) = Some d2 ->
  AR.O.I.eval_bindings j' P (application_environment P d h th (append_environment d2 dc)) [] st binds0 = inl (d3, g3, st3) ->
  heap_extends H H3 -> Pre rr Fb Cb d3 [] H3 g3 st3 ->
  exists dr result,
    select_provisions d3 (result_selection (map (A.rkey_of C') ks) None (link_sources (A.link_site Cg Cb pins outs' gen) outs' gen ks')) = Some dr /\
    result_environment (map (fun t => (t, gensof t)) outs) d3 (map ITy outs') dr = Some result /\
    Pre rr (A.with_fresh F newgens) C' (append_environment result d) types H3 g st3 /\
    MeaningsGrow (map gensof outs) d (append_environment result d).
Hypothesis link_results : LinkResultsOk.

(* HYPOTHESIS (T15). Applying a copy runs its source at the source's
   interface, and applying a platform family installs its primitives. *)
Definition CopyApplyOk : Prop := forall k, Below k -> forall J, (forall J', J' < J -> AppAt J' k) ->
  forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  AppPremises F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr, rr <= S k -> forall j d types H g st, S j <= rr -> j < J -> Pre rr F C d types H g st ->
  forall gfx h Cu0 nsu Ch nsh target code sms dc rms rm gfh,
  lookup_provision (IFun u) d = Some (FunctorClosure code (compose_adaptation sms (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu0 (target k))) nsh)) dc (compose_positions rms rm)) ->
  ClosureOk rr H h gfh (FunctorClosure code sms dc rms) ->
  A.FunCtx W u Cu0 nsu -> A.FunCtx W h Ch nsh -> NeedsAmong target nsh nsu ->
  (forall x, A.Fixed W h x -> A.Fixed W u x /\ lookup_subst x gfh = lookup_subst x gfx) ->
  FixedMeans u d fx gfx ->
  StepOut rr F (A.with_fresh F newgens) C' False (map gensof outs) (BApply u th (map (fun t => (t, gensof t)) outs) sm (map (A.rkey_of C') ks)) j d types H g st.
Hypothesis copy_apply : CopyApplyOk.
Definition PlatformApplyOk : Prop := forall k, Below k -> forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  AppPremises F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr, rr <= S k -> forall j d types H g st, S j <= rr -> Pre rr F C d types H g st ->
  forall code, lookup_provision (IFun u) d = Some (FunctorClosure code None (EnvironmentWithAliases [] [] []) None) ->
  Platform.Family W Prog u code ->
  StepOut rr F (A.with_fresh F newgens) C' False (map gensof outs) (BApply u th (map (fun t => (t, gensof t)) outs) sm (map (A.rkey_of C') ks)) j d types H g st.
Hypothesis platform_apply : PlatformApplyOk.

(* An abstract application below the cap J, given those below it: the
   closure found for F is linked (run its body), a copy or a platform
   family. *)
Lemma app_step : forall k, Below k -> forall J, (forall J', J' < J -> AppAt J' k) -> AppAt J k.
Proof.
  intros k Bel J IH F C pa u fx Cu ns sm th gensof C' ks newgens outs R Fx FC Inp Th Fixed Sat FreshG Uniq Outs NdO OutsIff PR rr Lr.
  assert (AP : AppPremises F C pa u fx Cu ns sm th gensof C' ks newgens outs)
    by exact (conj R (conj Fx (conj FC (conj Inp (conj Th (conj Fixed (conj Sat (conj FreshG (conj Uniq (conj Outs (conj NdO (conj OutsIff PR)))))))))))).
  split; [repeat split|]. split.
  { assert (Av : forall g0, In g0 (map gensof outs) -> ~ In g0 ((match A.fr_ret F with Some r => A.gens r | None => [] end) ++
        (match A.fr_this F with Some r => A.gens r | None => [] end))).
    { intros g0 Ig. apply in_map_iff in Ig as [t [<- It]].
      destruct (Outs t (proj1 (OutsIff t) It)) as [[_ [E [Nf _]]]|[_ In']]; [rewrite E; exact Nf|].
      destruct FreshG as [_ Fr]. rewrite Forall_forall in Fr. destruct (Fr _ In') as [_ Out]. intros I. apply Out.
      apply in_or_app; right; apply in_or_app; right. exact I. }
    split; intros t E g0 Ig Ig'; apply (Av g0 Ig); rewrite E; apply in_or_app; [left|right]; exact Ig'. }
  intros j d types H g st Lj LJ Pre0. change (StepOut rr F (A.with_fresh F newgens) C' False (map gensof outs)
    (BApply u th (map (fun t => (t, gensof t)) outs) sm (map (A.rkey_of C') ks)) j d types H g st).
  pose proof Pre0 as [Frame [Ctx [Lt [St [Real Fmd]]]]].
  pose proof Real as [_ [[_ [_ [_ Funs]]] [Section KM]]].
  pose proof (PublicContextFacts.section_canonical C Section) as Idem.
  pose proof (realizes_absorbs_canon _ _ _ _ Real) as Abs.
  destruct (Funs u fx Fx) as [pv [gfx [Look [Ok Fm]]]].
  destruct (closure_cases _ _ _ _ _ Ok) as [CL|[CC|CP]].
  2: { destruct CC as (h & Cu0 & nsu & Ch & nsh & target & code & sms & dc & rms & rm & gfh & -> & Okh & FCu0 & FCh & Incl & FxC).
       exact (copy_apply k Bel J IH F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj LJ Pre0
         gfx h Cu0 nsu Ch nsh target code sms dc rms rm gfh Look Okh FCu0 FCh Incl FxC Fm). }
  2: { destruct CP as (code & -> & Fam & _).
       exact (platform_apply k Bel F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj Pre0 code Look Fam). }
  destruct CL as (gg & h & Cu0 & nsu & Cg & nsg & linktarget & dc & -> & [pins [LB [PinsOk Binding]]] & FCu0 & FCg & FxIn & Caps & CF).
  destruct (functx_unique _ _ _ _ _ FC FCu0) as [<- <-].
  intros rest. rewrite bindings_apply_eq.
  destruct j as [|j1].
  { right. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I]. }
  subst th. set (th := map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cu)) in *.
  destruct (static_realizes_le rr rr C d H u Cu ns sm th (le_n _) Real (functx_static _ _ _ FC) Sat
    (apply_theta C u Cu ns sm FC Section Inp Sat)) as [d1 [Sel [_ RealU]]].
  destruct (link_facts _ _ _ _ LB) as (dg & n0 & ps0 & a0 & rs & bodies & Cu' & nsu' & Cg' & nsg' & Fg & wrap & Cb & Fb & made
    & outs' & gen & Cr & ks' & binds0 & LD).
  pose proof LD as (FCu' & FCg' & Dg & Ast & Mod & Ret & This & Loop & Num & Items & OutsI & OutM & NdG & FrG & PR' & RR & BO & FB & _).
  destruct (functx_unique _ _ _ _ _ FC FCu') as [<- <-]. destruct (functx_unique _ _ _ _ _ FCg FCg') as [<- <-].
  (* the closure's own needs, from those the caller selected *)
  destruct (select_ok d1 (A.link_map Cu Cg linktarget nsg)) as [d2 Sel2].
  { intros q Iq. apply in_map_iff in Iq as [k0 [<- Ik]]. cbn [snd].
    pose proof (linked_targets_in u Cu ns Cg nsg linktarget _ _ Binding k0 Ik) as Iu.
    destruct (functx_needs_provided W u Cu ns FC (A.linked_target linktarget k0) Iu) as [p0 Ip].
    destruct (functx_static _ _ _ FC) as [_ [_ [_ [_ [ShU _]]]]].
    destruct (realized_key _ _ _ _ _ _ RealU Ip (ShU (A.linked_target linktarget k0) Iu)) as [pv0 L0].
    change (lookup_provision (A.rkey_of Cu (A.linked_target linktarget k0)) d1) with (lookup_provision (A.rkey_of Cu (A.linked_target linktarget k0)) (callee_env d th d1)). rewrite L0. discriminate. }
  assert (Keys : map fst th = map fst (A.c_ty Cu)) by (unfold th; rewrite map_map; reflexivity).
  assert (GfxA : forall x, A.Fixed W u x -> lookup_subst x gfx = option_map (ground d) (lookup_subst x th)).
  { intros x Fx'. destruct Fm as [Cov Vm]. destruct (Cov x Fx') as [tx Ixt]. rewrite (Vm x tx Ixt).
    destruct (Fixed x tx Ixt) as [here [Lh Ceq]].
    assert (Lx : lookup_subst x th = Some (A.canon C (app_subst (A.c_ty C) (TAbs x))))
      by (unfold th; exact (lookup_on_keys (fun x => A.canon C (app_subst (A.c_ty C) (TAbs x))) _ x (FxIn x Fx'))).
    rewrite Lx. cbn [option_map].
    replace (app_subst (A.c_ty C) (TAbs x)) with here by (cbn [app_subst]; rewrite lookup_subst_nlook', Lh; reflexivity).
    f_equal. rewrite <- (Abs tx), <- Ceq. reflexivity. }
  pose proof (link_entry rr H d th d1 u gg h pins Cu ns Cg nsg linktarget dc gfx d2 LB Binding FC FCg Keys FxIn RealU GfxA Caps CF Sel2) as RealG.
  set (de := application_environment P d h th (append_environment d2 dc)) in *.
  assert (PreB : Pre rr Fg Cg de [] H [] st).
  { split; [apply (numbering_frame Fg []); rewrite This; exact Num|].
    split; [split; [|split]|split; [apply Forall2_nil|split; [exact St|split; [exact RealG|]]]].
    4: { split; [exact (PublicFormation.functx_cx_formed W mentions_typed gg Cg nsg FCg)|].
         split; [split; [rewrite Ret; apply PublicFormation.optok_none|rewrite This; apply PublicFormation.optok_none]|].
         apply (PublicScoping.functx_cxscoped W gg Cg nsg Fg FCg). intros t0 E0. rewrite This in E0. discriminate. }
    - intros n1 t mu Found. rewrite (functx_locals _ _ _ FCg) in Found. discriminate.
    - intros t E. congruence.
    - apply alias_self; [reflexivity|exact (realizes_absorbs_canon _ _ _ _ RealG)| |].
      + destruct (functx_scoped W gg Cg nsg FCg) as [CanG _]. exact CanG.
      + intros k0 v0 L0. unfold de, application_environment. change (view_functor P h) with (A.funbody_of Prog h).
        rewrite FB. cbn [type_meanings fc_inputs]. rewrite lookup_map_nat; [discriminate|].
        change k0 with (fst (k0, v0)). apply in_map. exact (nlook_in _ _ _ _ L0). }
  destruct (items_chain (S j1) k Bel (IH (S j1) LJ) _ _ _ _ _ _ _ Items) as [bs [Ew Ch]].
  pose proof (Ew EUnit) as E0. rewrite E0 in BO. apply bindsof_fold in BO. subst binds0.
  rewrite apply_functor_eq, Look. change (view_functor P h) with (A.funbody_of Prog h). rewrite FB.
  cbn [fc_bindings fc_outputs fc_sources]. rewrite Sel. cbn [adapt_provisions]. rewrite Sel2. fold de.
  (* the body's binds, run as their chain does *)
  destruct (chain_run (S j1) k Fg Cg Fb Cb _ made bs Ch rr j1 de [] H [] st [] Lr ltac:(lia) ltac:(lia) PreB)
    as [[d3 [g3 [st3 [H3 [j' [_ [E3 [_ [Pre3 Run0]]]]]]]]]|[res [Run [NN Out]]]];
    [rewrite app_nil_r in Run0|rewrite app_nil_r in Run].
  - destruct j' as [|j'].
    { rewrite Run0. right. exists (AR.O.I.with_g g (AR.O.I.Evaluation OutOfFuel g3 st3)). split; [reflexivity|].
      exact (body_stop Fg de H [] TUnit False False F d H types g (AR.O.I.Evaluation OutOfFuel g3 st3) Ret Loop ltac:(intros v E; discriminate) Logic.I). }
    assert (Run : AR.O.I.eval_bindings j1 P de [] st bs = inl (d3, g3, st3)) by (rewrite Run0; reflexivity).
    rewrite Run.
    destruct (link_results F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st h dc gfx gg dg n0 ps0 a0 rs bodies
      nsg Cg linktarget Fg wrap Cb Fb made outs' gen Cr ks' bs pins d1 d2 d3 g3 st3 H3 j1 Pre0 Look Ok Fm LD Binding PinsOk Sel Sel2 Run E3 Pre3)
      as [dr [result [Sdr [Rres [PreC GrowC]]]]].
    rewrite Sdr, Rres. left. exists (append_environment result d), g, st3, H3.
    split; [exact E3|]. split; [exact GrowC|]. split; [exact PreC|reflexivity].
  - rewrite Run. right. exists (AR.O.I.with_g g res). split; [reflexivity|].
    exact (body_stop Fg de H [] TUnit _ False F d H types g res Ret Loop NN Out).
Qed.

Lemma case_BApplyAbstract : forall k, Below k -> forall J, AppAt J k.
Proof.
  intros k Bel J. induction J as [J IHJ] using lt_wf_ind. exact (app_step k Bel J IHJ).
Qed.

Lemma item_safe : forall k, Below k -> forall n F C it w C' F' m,
  LegacyBodyJudgments.BindItem W Prog F C it w C' F' m -> n <= S k -> WrapSafe n F C F' C' (ItemBreaks F it) m w.
Proof.
  intros k Bel n F C it w C' F' m D Le. destruct (item_chain (S k) k Bel (case_BApplyAbstract k Bel (S k)) _ _ _ _ _ _ _ D) as [bs [Ew Ch]].
  intros t B x Av Safe. rewrite Ew. exact (chain_wrap (S k) k n F C F' C' _ m bs Ch Le Le t B x Av Safe).
Qed.

Lemma items_safe : forall k, Below k -> forall n F C its w C' F' m,
  LegacyBodyJudgments.BindItems W Prog F C its w C' F' m -> n <= S k -> WrapSafe n F C F' C' (ItemsBreak F its) m w.
Proof.
  intros k Bel n F C its w C' F' m D Le. destruct (items_chain (S k) k Bel (case_BApplyAbstract k Bel (S k)) _ _ _ _ _ _ _ D) as [bs [Ew Ch]].
  intros t B x Av Safe. rewrite Ew. exact (chain_wrap (S k) k n F C F' C' _ m bs Ch Le Le t B x Av Safe).
Qed.

Section Step.
Variable k : nat.
Hypothesis Bel : Below k.

Lemma sex : SafeEx k. Proof. exact (proj1 (Bel k (le_n k))). Qed.
Lemma sblk : SafeBlk k. Proof. exact (proj1 (proj2 (Bel k (le_n k)))). Qed.
Lemma sblke : SafeBlkE k. Proof. exact (proj1 (proj2 (proj2 (Bel k (le_n k))))). Qed.
Lemma sbranch : SafeBranch k. Proof. exact (proj1 (proj2 (proj2 (proj2 (Bel k (le_n k)))))). Qed.
Lemma scond : SafeCond k. Proof. exact (proj2 (proj2 (proj2 (proj2 (Bel k (le_n k)))))). Qed.

Lemma unit_k : forall F C d types H g st, Pre k F C d types H g st ->
  Outcome F d H types TUnit False (AR.O.I.eval k P d g st EUnit).
Proof.
  intros F C d types H g st Pre0.
  eapply AR.O.outcome_breaks; [|exact (sex F C AEUnit None EUnit TUnit (LegacyBodyJudgments.ExUnit W Prog F C None) d types H g st Pre0)].
  exact (unit_never_breaks _).
Qed.



Lemma val_step : forall F C pa s t, A.nlook s (A.c_vals C) = Some t ->
  StepSafe k F C t (A.Breaks W (A.fr_mod F) (AEPath pa None)) (EVal s).
Proof.
  intros F C pa s t Found d types H lg lst Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [[_ [[Vals _] _]] _]]]]]. destruct (Vals s t Found) as [w [Look T]].
  cbn [AR.O.I.eval]. rewrite Look. eapply outcome_now; eassumption.
Qed.

Lemma invoke_step : forall F C pa args f g xs,
  A.nlook f (A.c_fns C) = Some g -> LegacyBodyJudgments.Args W Prog F C args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) (A.Breaks W (A.fr_mod F) (AECall pa None args)) (EInvoke f xs).
Proof.
  intros F C pa args f g xs Found Args d types H lg lst Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [[_ [[_ [Fns _]] _]] _]]]]]. destruct (Fns f g Found) as [pv [Look Ok]].
  pose proof (args_safe k Bel k (le_n k) F C (A.Breaks W (A.fr_mod F) (AECall pa None args)) xs (A.sg_params g) d types H lg lst
    (args_argsafe F C args _ xs Args _ (fun a In' B => A.BkCall pa None args In' B)) (pre_lower Pre0)) as AOut.
  cbn [AR.O.I.eval]. rewrite Look.
  destruct pv as [w|f' df|u sms dc|p held]; [destruct Ok| |destruct Ok|].
  - destruct Ok as [body [View Run]]. rewrite View.
    destruct (AR.O.I.eval_arguments k P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      exact (outcome_ret_call F d H H1 types _ _ _ g1 E1 L1 (Run k (Nat.lt_succ_diag_r k) H1 st1 ws E1 S1 T1)).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
  - destruct (AR.O.I.eval_arguments k P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      pose proof (Ok H1 st1 ws E1 S1 T1) as POut.
      destruct (AR.O.I.run_prim_at P p held ws st1) as [o st2] eqn:Run'.
      exact (outcome_prim F d H H1 types _ _ o st2 g1 E1 L1 POut).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
Unshelve. all: exact TUnit.
Qed.

Lemma attached_step : forall F C o ox ot0 ot n args h att Ca ns sm th g xs rargs wrap Cd (B : Prop),
  LegacyBodyJudgments.Ex W Prog F C o None ox ot0 -> (A.Breaks W (A.fr_mod F) o -> B) -> (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) ->
  ot = A.canon C ot0 ->
  A.head ot = Some h -> A.Attached W h n att ->
  (exists rn ps rt b, A.ast_of W att = Some (ADFn (RAttached rn) n ps rt (Some b))) ->
  rargs = (match ot with TNom _ xs => xs | _ => [] end) ->
  A.CtxOf W att Ca ns ->
  A.Demand W (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C)
    (map (A.at_th (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C) th) ns) []
    (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C) wrap Cd ->
  A.Satisfies Cd Ca th ns sm -> A.CallTheta W Cd Ca att th -> A.SigOf W att (A.c_ty Cd) (Some ot) g ->
  LegacyBodyJudgments.Args W Prog F C args (A.sg_params g) xs ->
  A.RequirementsInForce (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C) Ca ->
  StepSafe k F C (A.sg_ret g) B (A.translate_demands (A.family_context C rargs) wrap (ECall att th sm (ox :: xs))).
Proof.
  intros F C o ox ot0 ot n args h att Ca ns sm th g xs rargs wrap Cd B Do BrkO BrkA Ot Head Att Def Rargs Ctx Dem Sat CT Sig Args RB
    d types H lg lst Pre0.
  destruct Def as [rn [ps [rt [b Def]]]]. unfold A.ast_of in Def.
  destruct (A.w_decl W att) as [dcl|] eqn:Decl; [|discriminate]. injection Def as Ast.
  pose proof (all_fn_elab att dcl (RAttached rn) n ps rt b Decl Ast dcl (RAttached rn) n ps rt b Decl Ast)
    as (C0 & ns0 & this & g0 & F0 & x0 & t0 & _ & Rcv & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & View).
  destruct Rcv as [r [args0 [Att0 [Kind _]]]].
  pose proof (attached_unique h r n att Att Att0) as <-.
  pose proof Head as Head'. rewrite Ot in Head'.
  pose proof Pre0 as [_ [_ [_ [_ [_ [FcA [FfA _]]]]]]].
  destruct (PublicFormation.ex_nominal_formed' W Prog units_bare family_sig_ok F C o None ox ot0 Do FcA FfA
    (PublicFormation.optok_none W) h Head' Kind) as [xs0 [Eot Keys]].
  rewrite <- Ot in Eot. rewrite Eot in Ot. subst ot. cbn in Rargs. subst rargs.
  set (Cd0 := fold_left (fun C q => A.set_ty C (fst q) (snd q)) xs0 C) in *.
  destruct (set_tys_fields xs0 C) as [Es [Ep [El Et]]].
  assert (Nd : NoDup (map fst xs0)) by (rewrite Keys; destruct World as [bd Wo]; exact (A.wo_reqs_nodup Wo h)).
  assert (Cty : A.c_ty Cd = (rev xs0 ++ A.c_ty C)%list) by (rewrite (demand_ty _ _ _ _ _ _ Dem); exact Et).
  refine (demand_case k Bel F C Cd0 _ Cd wrap (A.sg_ret g) B _ Es (PublicContextFacts.family_context_merge xs0 C) Ep El Dem _ _ d types H lg lst Pre0).
  { intros Fm.
    assert (Rok : PublicFormation.TyOk W (TNom h xs0) /\ PublicFormation.ScopedTy W C (TNom h xs0)).
    { rewrite Ot. split.
      - exact (PublicFormation.canon_ok W C ot0 (proj1 (proj1 Fm))
          (PublicFormation.ex_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ Do (proj1 Fm) (proj1 (proj2 Fm)) (PublicFormation.optok_none W))).
      - exact (PublicFormation.canon_scoped W C ot0 (ex_scoped F C o ox ot0 Do Fm)). }
    assert (Tin : FamilyTableIn Cd0 C).
    { intros xq v Iq. replace (A.canon Cd0 v) with (A.canon C v) by (unfold A.canon, Cd0; rewrite Es; reflexivity).
      unfold Cd0 in Iq. rewrite Et in Iq. apply in_app_or in Iq as [Iq|Iq]; [|exact (table_in_self F C Fm xq v Iq)].
      apply (proj2 (in_rev _ _)) in Iq. destruct Rok as [Ok Sc].
      split; [exact (PublicFormation.canon_ok W C v (proj1 (proj1 Fm)) (proj2 (PublicFormation.nom_ok_elim W h xs0 Ok) xq v Iq))|].
      apply PublicFormation.canon_scoped. intros y Iy. apply Sc. rewrite PublicFormation.gens_nom.
      exact (PublicFormation.in_tm_gens _ _ _ _ Iq Iy). }
    split; [exact Tin|]. split.
    { intros xq Ix. unfold Cd0 in Ix. rewrite Et, map_app in Ix. apply in_app_or in Ix as [Ix|Ix].
      - rewrite map_rev in Ix. apply (proj2 (in_rev _ _)) in Ix.
        assert (Ir : In xq (A.w_reqs W h)) by (rewrite <- Keys; exact Ix).
        destruct World as [bd Wo]. apply (PublicWorldFormation.reqs_typed bd W Wo h xq); [|exact Ir].
        intros E. unfold A.kind_sym in Kind. rewrite E in Kind. destruct Kind; discriminate.
      - exact (proj2 (proj2 (proj1 Fm)) xq Ix). }
    pose proof (PublicContextFacts.family_context_merge xs0 C) as Emg.
    assert (CT0 : A.CallTheta W Cd0 Ca att th).
    { apply (call_theta_same Cd); [exact (demand_ty _ _ _ _ _ _ Dem)|exact (demand_sub _ _ _ _ _ _ Dem)|
        exact (demand_merge _ _ _ _ _ _ Dem)|exact CT]. }
    pose proof Pre0 as (_ & _ & _ & _ & (_ & _ & SectionC & _) & _).
    pose proof (section_same_fields C Cd0 Es Emg SectionC) as SectionD.
    intros r0 m0 I0. exact (demand_scoped F C Cd0 att Ca ns th r0 m0 Fm Tin Es SectionD Ctx CT0 RB I0). }
  intros j Lj d' types' H' g' st' PreD PreC.
  destruct j as [|j]; [exact Logic.I|].
  cbn [AR.O.I.eval]. change (view_function P att) with (A.body_of Prog att). rewrite View.
  assert (ArgsS : Forall2 (fun e t => CodeSafe k F C t B e) (ox :: xs) (TNom h xs0 :: A.sg_params g)).
  { constructor; [|exact (args_code_of k Bel F C args _ xs Args B BrkA)].
    apply (ex_code k Bel F C o None ox ot0 _ B Do); [right; right; exact Ot|exact BrkO]. }
  pose proof (args_code k j ltac:(lia) F C B (ox :: xs) _ d' types' H' g' st' ArgsS (pre_lower PreC)) as AOut.
  destruct (AR.O.I.eval_arguments j P d' g' st' (ox :: xs)) as [[[ws g1] st1]|rr]; cbn in AOut.
  2: { destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out]. }
  destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
  inversion T1 as [|w0 t0' ws' ts' Tw0 Tws]; subst.
  pose proof PreD as [_ [_ [_ [_ [RealD _]]]]]. apply (realizes_ty_free _ _ Cd) in RealD; [|reflexivity|reflexivity|reflexivity].
  destruct (callee_realizes j Cd d' H1 att Ca ns sm th (realizes_extends _ _ _ _ _ E1 RealD) Ctx Sat CT)
    as [dc [Sel [Ag RealA]]].
  rewrite Sel, (call_environment_callee d' _ sm dc Sel).
  pose proof (realizes_absorbs_canon _ _ _ _ RealD) as AbsD.
  destruct (body_call_method j (Bel j ltac:(lia)) att dcl rn n ps rt b Ca ns _ H1 st1 w0 ws' d'
    (TNom h xs0) (A.sg_params g) (A.sg_ret g) Decl Ast Ctx RealA S1 eq_refl (keys_meant_callee Ca d' th dc (proj1 CT))) as [body [View' Run]].
  - intros r' args' g0' Att' Reqs Sig0.
    pose proof (attached_unique h r' n att Att Att') as <-.
    assert (Recv : app_subst (callee_meanings d' th) (TNom h args') = ground d' (TNom h xs0))
      by exact (receiver_agree att Ca ns Cd th d' h args' xs0 (A.c_ty C) Ctx CT (proj1 (proj2 (proj2 RealD))) AbsD Nd Cty Reqs Keys).
    destruct (sig_rel W _ _ att _ _ (Some (TNom h args')) (Some (TNom h xs0)) g0' g Sig0 Sig Ag
      (fun x1 x2 E1' E2' => ltac:(injection E1' as <-; injection E2' as <-; exact Recv))) as [Ps Rt].
    split; [exact Recv|]. split; [exact Ps|exact Rt].
  - exact Tw0.
  - exact Tws.
  - change (view_function P att) with (A.body_of Prog att) in View'. rewrite View in View'. injection View' as <-.
    exact (outcome_ret_call F d' H' H1 types' _ _ _ g1 E1 L1 Run).
Unshelve. all: exact TUnit.
Qed.

Lemma provided_step : forall F C o ox ot0 m args x t (B : Prop),
  LegacyBodyJudgments.Ex W Prog F C o None ox ot0 -> (A.Breaks W (A.fr_mod F) o -> B) -> (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) ->
  LegacyBodyJudgments.Provided W Prog F C ox (A.canon C ot0) m args x t -> StepSafe k F C t B x.
Proof.
  intros F C o ox ot0 m args x t B Do BrkO BrkA Prov.
  remember (A.canon C ot0) as ot eqn:Ot.
  destruct Prov as [F C ox ot m args g xs Found Args|F C ox ot m args u matching th formal needs C' ks g xs None' Typed Ro InK Only Found Args].
  2: { subst ot.
       pose (sm := A.satisfaction_map (A.family_context C matching) formal th needs).
       pose proof (AuditFacts.family_application_with_map Typed) as Fam.
       exact (family_method_case k Bel F C o ox ot0 m args u matching th sm C' ks g xs B Do BrkO BrkA
          None' Fam Ro
          (fun Fm => conj (PublicFormation.canon_ok W C ot0 (proj1 (proj1 Fm))
              (PublicFormation.ex_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ Do (proj1 Fm) (proj1 (proj2 Fm)) (PublicFormation.optok_none W)))
            (PublicFormation.canon_scoped W C ot0 (ex_scoped F C o ox ot0 Do Fm)))
          InK Only Found Args). }
  intros d types H lg lst Pre0.
  pose proof (pre_absorbs_canon _ _ _ _ _ _ _ _ Pre0) as Abs.
  destruct (pre_method_provision _ _ _ _ _ _ _ _ _ _ _ Pre0 Found) as [pv [Look Ok]].
  pose proof (AR.O.outcome_breaks W P state_ok _ _ _ _ _ _ _ _ BrkO (sex F C o None ox ot0 Do d types H lg lst (pre_lower Pre0))) as ROut.
  cbn [AR.O.I.eval]. rewrite Look.
  destruct pv as [w|f' df|u sms dc|p held]; [destruct Ok| |destruct Ok|].
  - destruct Ok as [body [View Run]]. rewrite View.
    apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ (pre_lower Pre0) (fun b => b) ROut).
    intros H0 g0 st0 w0 E0 Pre1 T0.
    pose proof (args_safe k Bel k (le_n k) F C B xs (A.sg_params g) d types H0 g0 st0
      (args_argsafe F C args _ xs Args _ BrkA) Pre1) as AOut.
    destruct (AR.O.I.eval_arguments k P d g0 st0 xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      apply (outcome_ret_call F d H0 H1 types _ _ _ g1 E1 L1).
      apply (Run k (Nat.lt_succ_diag_r k) H1 st1 (w0 :: ws) (extends_trans _ _ _ E0 E1) S1). constructor; [|exact T1].
      subst ot. rewrite Abs. eapply value_typed_extends; eassumption.
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
  - apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ (pre_lower Pre0) (fun b => b) ROut).
    intros H0 g0 st0 w0 E0 Pre1 T0.
    pose proof (args_safe k Bel k (le_n k) F C B xs (A.sg_params g) d types H0 g0 st0
      (args_argsafe F C args _ xs Args _ BrkA) Pre1) as AOut.
    destruct (AR.O.I.eval_arguments k P d g0 st0 xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      assert (Args' : Forall2 (fun w t => value_typed W P H1 w (ground d t)) (w0 :: ws) (ot :: A.sg_params g)).
      { constructor; [|exact T1]. subst ot. rewrite Abs. eapply value_typed_extends; eassumption. }
      pose proof (Ok H1 st1 (w0 :: ws) (extends_trans _ _ _ E0 E1) S1 Args') as POut.
      destruct (AR.O.I.run_prim_at P p held (w0 :: ws) st1) as [o' st2] eqn:Run'.
      exact (outcome_prim F d H0 H1 types _ _ o' st2 g1 E1 L1 POut).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
Unshelve. all: exact TUnit.
Qed.

Lemma methcall_step : forall F C o ox ot0 pa args x t (B : Prop),
  LegacyBodyJudgments.Ex W Prog F C o None ox ot0 -> (A.Breaks W (A.fr_mod F) o -> B) -> (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) ->
  LegacyBodyJudgments.MethCall W Prog F C ox (A.canon C ot0) pa args x t -> StepSafe k F C t B x.
Proof.
  intros F C o ox ot0 pa args x t B Do BrkO BrkA Meth.
  remember (A.canon C ot0) as ot eqn:Ot.
  destruct Meth as [F C ox ot pa args m x t Long Det Prov
                   |F C ox ot n args h att m x t Head Att Scope Mth Prov
                   |F C ox ot n args h att Ca ns th g xs rargs wrap Cd Head Att NotWin Def Rargs Ctx Dem Needs CT Sig Args RB
                   |F C ox ot n args h att x t Head Att NotWin Abs Prov
                   |F C ox ot n args m x t NoAtt Scope Prov]; subst ot;
    try exact (provided_step F C o ox ot0 _ args x t B Do BrkO BrkA Prov).
  pose (sm := A.satisfaction_map Cd Ca th ns).
  assert (Sat : A.Satisfies Cd Ca th ns sm) by (split; [reflexivity|exact Needs]).
  exact (attached_step F C o ox ot0 _ n args h att Ca ns sm th g xs rargs wrap Cd B Do BrkO BrkA
    eq_refl Head Att Def Rargs Ctx Dem Sat CT Sig Args RB).
Qed.

Fixpoint ex_step F C a exp x t (D : LegacyBodyJudgments.Ex W Prog F C a exp x t) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t (A.Breaks W (A.fr_mod F) a) (AR.O.I.eval (S k) P d g st x)
with blk_step F C ss tl exp x t made (D : LegacyBodyJudgments.Blk W Prog F C ss tl exp x t made) {struct D} :
  (forall g, In g made -> ~ In g (A.gens t)) -> forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks W (A.fr_mod F) (ABlock ss tl)) (AR.O.I.eval (S k) P d g st x)
with blke_step F C b exp x t (D : LegacyBodyJudgments.BlkE W Prog F C b exp x t) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks W (A.fr_mod F) b) (AR.O.I.eval (S k) P d g st x)
with branch_step F C y exp x t (D : LegacyBodyJudgments.Branch W Prog F C y exp x t) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t (A.Breaks W (A.fr_mod F) y) (AR.O.I.eval (S k) P d g st x)
with cond_step F C c x (D : LegacyBodyJudgments.Cond W Prog F C c x) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  exists b, A.LangSym W "bool" A.NSName "Bool" b /\
    Outcome F d H types (TNom b []) (A.Breaks W (A.fr_mod F) c) (AR.O.I.eval (S k) P d g st x).
Proof.
  (* ---------------- Ex ---------------- *)
  - destruct D as
      [F C exp
      |F C exp t This
      |F C exp n t mu Found Named
      |F C exp pa s NotLocal R K
      |F C exp pa s dcl n t0 t NotLocal R Decl Ast Val
      |F C exp pa s dcl n t0 i t x NotLocal R Decl Ast NoAssume TyOf0 Init
      |F C exp pa a args s bs xs pay y v vty R K Br F2 Pay Args1 Dy Fits
      |F C exp pa args f Cf ns th sg xs wrap C' R K Def Ctx Dem Needs CT Sig Args RB
      |F C exp pa args f sg xs R K Abs Fn Args
      |F C exp pa bs args f x t R K Br Dx
      |F C exp o pa args ox ot x t Do Dm
      |F C exp op y lib n m ox ot x t Op Lang Dy Head Prov
      |F C exp op l r lib n m ox ot x t Op Lang Dl Head Prov
      |F C exp text x t Num Lit
      |F C exp c b cx' bx bt Dc Db FitsU Bt
      |F C exp c b y cx' bx bt ex' et t Dc Db Dy FitsE Join
      |F C exp sc arms sx st xarms t Ds Darms
      |F C exp b x t Db
      |F C exp Ret
      |F C exp e expected v actual Ret De Fits
      |F C exp Loop
      |F C exp pa a fs s bs xs pay ftys xfs R K Br F2 Pay PayEq Nd Len Fields
      |F C exp o n ox ot rcd t Do Rcd Fld];
      intros d types H lg lst Pre0.
    + (* unit *) cbn [AR.O.I.eval]. eapply outcome_now; [exact Pre0|constructor].
    + (* this *) cbn [AR.O.I.eval]. pose proof Pre0 as [_ [[_ [Rcv _]] [L _]]].
      destruct (locals_lookup W P H lg types 0 _ L (Rcv t This)) as [w [Look T]].
      rewrite Look. eapply outcome_now; eassumption.
    + (* local *) cbn [AR.O.I.eval]. pose proof Pre0 as [_ [[Loc _] [L _]]].
      destruct (locals_lookup W P H lg types _ _ L (Loc n t mu Found Named)) as [w [Look T]].
      rewrite Look. eapply outcome_now; eassumption.
    + (* unit tag *) cbn [AR.O.I.eval]. pose proof (unit_k F C d types H lg lst (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst TUnit _ False _ _ _ Pre0 (fun f => False_ind _ f) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      apply outcome_tag; [exact S1|exact L1|].
      exact (TC.tag_preserves W Prog argv read d H1 s [] TUnit w
        (or_intror (conj K (conj eq_refl eq_refl))) T1).
    + (* val *) exact (val_step F C pa s t Val d types H lg lst Pre0).
    + (* const *)
      destruct Init as [F0 [t' [Dx [Fits [Mod [Ret [This [Loop [Fresh [App [Pins Num]]]]]]]]]]].
      eapply (outcome_const F0); [exact Ret|exact Loop|].
      apply (AR.O.outcome_fits W P state_ok _ _ _ _ t'); [exact Fits|].
      apply (ex_step F0 (A.Cx [] [] [] [] []) i (Some t) x t' Dx).
      destruct Pre0 as [_ [_ [L [S Abs]]]]. split; [|split; [|split; [exact L|split; [exact S|split]]]].
      4: { split.
           - split; [constructor; [intros x9 u9 []|apply PublicFormation.tmok_read; intros x9 u9 []|intros c9 t9 []|intros k9 p9 []|intros n9 t9 mu9 []]|].
             split; [intros r9 m9 th9 p9 []|intros x9 []].
           - split; [split; [rewrite Ret; apply PublicFormation.optok_none|rewrite This; apply PublicFormation.optok_none]|].
             split; [intros n9 t9 mu9 []|split; [intros k9 p9 []|split; [intros t9 E9; rewrite This in E9; discriminate|]]].
             split; [exact PublicMergePresent.empty_merge_section|split; [intros x9 v9 []|intros r9 m9 g9 a9 []]]. }
      * apply (numbering_frame F0 []). rewrite This. exact Num.
      * split; [intros n' t1 mu Found; discriminate|split; [intros t1 This'; congruence|intros x1 tx1 Lx1; discriminate]].
      * split.
        { apply (absorbs_canon_iff (A.Cx [] [] [] [] []) d PublicMergePresent.empty_merge_section).
          intros t1. unfold A.canon. cbn. now rewrite app_subst_nil. }
        split; [split; [intros v' t1 Found; discriminate|]|].
        { split; [intros f g Found; discriminate|]. split; [intros r m g Found; discriminate|].
          intros u fx Found; discriminate. }
        split; [exact PublicMergePresent.empty_merge_section | intros k0 p0 []].
    + (* tag *) subst args. cbn [AR.O.I.eval].
      pose proof (AR.O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits (sex F C y _ v vty Dy d types H lg lst (pre_lower Pre0))) as Out.
      apply (outcome_seq _ F C d types H lg lst pay _ (A.Breaks W (A.fr_mod F) y) _ _ _ Pre0
        (fun B => A.BkCall pa a [y] (or_introl eq_refl) B) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      apply outcome_tag; [exact S1|exact L1|].
      exact (TC.tag_preserves W Prog argv read d H1 s xs pay w
        (or_introl (conj K (conj Pay (forall2_keys _ _ (fun r v => A.nlook r (List.app bs (A.c_ty C)) = Some v) F2)))) T1).
    + destruct Def as [n0 [ps0 [rt0 [b0 Def]]]].
      pose (sm := A.satisfaction_map C' Cf th ns).
      assert (Sat : A.Satisfies C' Cf th ns sm) by (split; [reflexivity|exact Needs]).
      pose proof Pre0 as (_ & _ & _ & _ & (_ & _ & SectionC & _) & _).
      exact (demand_case k Bel F C C _ C' wrap (A.sg_ret sg) _ _ eq_refl eq_refl eq_refl eq_refl Dem
        (fun Fm => conj (table_in_self F C Fm) (conj (proj2 (proj2 (proj1 Fm))) (fun r0 m0 I0 => demand_scoped F C C f Cf ns th r0 m0 Fm (table_in_self F C Fm) eq_refl
           SectionC Ctx
           (call_theta_same C' C Cf f th (demand_ty _ _ _ _ _ _ Dem) (demand_sub _ _ _ _ _ _ Dem)
             (demand_merge _ _ _ _ _ _ Dem) CT) RB I0)))
        (fun j Lj d' types' H' g' st' Pre' PreC => call_code k Bel F C' C args f n0 ps0 rt0 b0 Cf ns sm th sg xs _ Def Ctx Sat CT Sig Args
          (fun a In' B0 => A.BkCall pa None args In' B0) j Lj d' types' H' g' st'
          (pre_with_ty _ _ C C' _ _ _ _ _ (demand_ty _ _ _ _ _ _ Dem) Pre') PreC) d types H lg lst Pre0).
    + exact (invoke_step F C pa args f sg xs Fn Args d types H lg lst Pre0).
    + (* bracketed call (D89): its expansion *)
      eapply AR.O.outcome_breaks; [exact (call_block_breaks (A.fr_mod F) pa bs args)|].
      exact (ex_step F C _ None x t Dx d types H lg lst Pre0).
    + refine (methcall_step F C o ox ot pa args x t _ Do _ _ Dm d types H lg lst Pre0);
        [intro B0; apply A.BkMethRecv; exact B0|intros a' In' B0; eapply A.BkMethArg; [exact In'|exact B0]].
    + refine (provided_step F C y ox ot m [] x t _ Dy _ _ Prov d types H lg lst Pre0);
        [intro B0; apply A.BkUnary; exact B0|intros a' []].
    + refine (provided_step F C l ox ot m [r] x t _ Dl _ _ Prov d types H lg lst Pre0);
        [intro B0; apply A.BkBinL; exact B0|intros a' [<-|[]] B0; apply A.BkBinR; exact B0].
    + eapply AR.O.outcome_breaks; [|exact (lit_code k Bel F C _ x t Lit (S k) (le_n _) d types H lg lst Pre0)].
      intros [].
    + (* if-then *) cbn [AR.O.I.eval].
      destruct (scond F C c cx' Dc d types H lg lst (pre_lower Pre0)) as [bsym [Lang Out]].
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => A.BkIfC b None B) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      destruct (B.truthy_typed W Prog argv read H1 bsym w Bools Lang T1) as [bit Bit].
      replace (AR.O.I.truthy P w) with (Some bit) by (symmetry; exact Bit).
      destruct bit.
      * apply (AR.O.outcome_fits W P state_ok _ _ _ _ bt).
        { destruct Bt as [-> | ->]; [apply fits_refl|apply fits_never]. }
        eapply AR.O.outcome_breaks; [|exact (sblke F C b exp bx bt Db d types H1 g1 st1 (pre_lower Pre1))].
        intro B; apply A.BkIfB; exact B.
      * eapply AR.O.outcome_breaks; [|exact (unit_k F C d types H1 g1 st1 (pre_lower Pre1))]. intros [].
    + (* if-else *) cbn [AR.O.I.eval].
      destruct (scond F C c cx' Dc d types H lg lst (pre_lower Pre0)) as [bsym [Lang Out]].
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => A.BkIfC b (Some y) B) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      destruct (B.truthy_typed W Prog argv read H1 bsym w Bools Lang T1) as [bit Bit].
      replace (AR.O.I.truthy P w) with (Some bit) by (symmetry; exact Bit).
      destruct (blke_branch_ty _ _ _ _ _ _ Db) as [FitsA FitsBt].
      destruct (join_fits _ _ _ _ _ Join FitsA FitsBt FitsE) as [Left Right].
      destruct bit.
      * apply (AR.O.outcome_fits W P state_ok _ _ _ _ bt); [exact Left|].
        eapply AR.O.outcome_breaks; [|exact (sblke F C b exp bx bt Db d types H1 g1 st1 (pre_lower Pre1))].
        intro B; apply A.BkIfB; exact B.
      * apply (AR.O.outcome_fits W P state_ok _ _ _ _ et); [exact Right|].
        eapply AR.O.outcome_breaks; [|exact (sbranch F C y exp ex' et Dy d types H1 g1 st1 (pre_lower Pre1))].
        intro B; apply A.BkIfE; exact B.
    + (* match *) cbn [AR.O.I.eval].
      pose proof (sex F C sc None sx st Ds d types H lg lst (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => A.BkMatchS arms B) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      pose proof Pre1 as [Frame [Ctx [L1 [S1 [Real Fmd1]]]]].
      assert (T1' : value_typed W P H1 w (ground d (A.canon C st))) by (rewrite (realizes_absorbs_canon _ _ _ _ Real); exact T1).
      pose proof (PublicFormation.ex_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ Ds (proj1 Fmd1) (proj1 (proj2 Fmd1))
        (PublicFormation.optok_none W)) as OkSt.
      destruct (AR.arms_select W Prog P d H1 F C _ arms exp xarms t w Darms
        (PublicFormation.ex_formed' W Prog units_bare family_sig_ok _ _ _ _ _ _ Ds (proj1 Fmd1) (proj1 (proj2 Fmd1)) (PublicFormation.optok_none W)) T1')
        as [pat [body [bs [eb [binds [et [Sel [InArm [Dbr [FitsT [_ [[NamedB [Distinct LB]] [xp [hs [ca PO]]]]]]]]]]]]]]].
      rewrite Sel.
      assert (Len : List.length bs = List.length (map (fun q => (A.var_of F (fst q), ground d (snd q))) binds))
        by exact (Forall2_length LB).
      apply (outcome_popn F d H1 types _ _ t _ _ (eq_sym Len)).
      apply (AR.O.outcome_fits W P state_ok _ _ _ _ et); [exact FitsT|].
      eapply AR.O.outcome_breaks; [|apply (sbranch F _ body exp eb et Dbr d _ H1 (bs ++ g1)%list st1)].
      * intro B. eapply A.BkMatchA; [exact InArm|exact B].
      * split; [exact Frame|]. split; [exact (ctx_locals_push_distinct F binds C d types Frame Ctx NamedB Distinct)|].
        split; [apply Forall2_app; [exact LB|exact L1]|]. split; [exact S1|].
        apply (realizes_lower (S k) k C d H1 (le_S _ _ (le_n k))) in Real. split.
        -- refine (realizes_same _ C _ _ _ _ _ _ _ Real); [apply add_locals_ty|apply add_locals_sub|apply add_locals_merge|apply add_locals_provisions].
        -- split; [apply (PublicFormation.add_locals_formed W); [exact (proj1 Fmd1)|];
             exact (PublicFormation.patof_ok W _ _ _ _ _ _ _ PO (PublicFormation.canon_ok W C st (proj1 (proj1 Fmd1)) OkSt))|].
           split; [exact (proj1 (proj2 Fmd1))|].
           apply (PublicScoping.add_locals_scoped W F A.LLet binds C (proj2 (proj2 Fmd1))).
           exact (PublicScoping.patof_scoped W C F _ _ _ _ _ _ PO (PublicFormation.canon_scoped W C st (ex_scoped F C sc sx st Ds Fmd1))).
    + (* block *)
      destruct (blke_branch_ty _ _ _ _ _ _ Db) as [_ FitsExp].
      apply (AR.O.outcome_fits W P state_ok _ _ _ _ t).
      { destruct exp; [exact FitsExp|apply fits_refl]. }
      eapply AR.O.outcome_breaks; [|exact (blke_step F C b exp x t Db d types H lg lst Pre0)].
      intro B; apply A.BkBlock; exact B.
    + (* return () *) cbn [AR.O.I.eval].
      pose proof (unit_k F C d types H lg lst (pre_lower Pre0)) as Out.
      destruct (AR.O.I.eval k P d lg lst EUnit) as [[] g1 st1]; cbn in Out |- *; auto; try contradiction;
        [|destruct Out as [[] _]].
      destruct Out as [H1 [E1 [S1 [L1 T1]]]]. exists H1, TUnit; repeat split; auto.
    + (* return e *) cbn [AR.O.I.eval].
      pose proof (AR.O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits (sex F C e _ v actual De d types H lg lst (pre_lower Pre0))) as Out.
      destruct (AR.O.I.eval k P d lg lst v) as [[] g1 st1]; cbn in Out |- *; auto; try contradiction.
      * destruct Out as [H1 [E1 [S1 [L1 T1]]]]. exists H1, expected; repeat split; auto.
      * destruct Out as [B Rest]. split; [apply A.BkReturn; exact B|exact Rest].
    + (* break *) cbn [AR.O.I.eval]. pose proof Pre0 as [_ [_ [L [S _]]]].
      split; [apply A.BkBreak|]. split; [exact Loop|]. exists H; repeat split; auto. apply extends_refl.
    + (* record *) subst pay. cbn [AR.O.I.eval].
      destruct (record_args F C pa a fs ftys xfs Fields) as [Args Names].
      pose proof (record_eval k Bel F C pa a fs ftys xfs d types H lg lst Args Names (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => B) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      apply outcome_tag; [exact S1|exact L1|].
      exact (TC.tag_preserves W Prog argv read d H1 s xs (TRecord ftys) w
        (or_introl (conj K (conj Pay (forall2_keys _ _ (fun r v => A.nlook r (List.app bs (A.c_ty C)) = Some v) F2)))) T1).
    + (* field *) cbn [AR.O.I.eval].
      pose proof (sex F C o None ox ot Do d types H lg lst (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => A.BkField n B) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      destruct Fld as [ftys [-> Look]].
      assert (Rec : exists values, AR.O.I.get_field n w = A.slook n values /\
        fields_typed W P H1 values (map (fun q => (fst q, ground d (snd q))) ftys)).
      { destruct Rcd as [[s [xs [-> [K Pay]]]]|Same].
        - rewrite ground_nom in T1. destruct (nominal_value W P _ _ _ _ T1) as [inner ->].
          destruct (tagged_payload_typed W P H1 s _ inner K T1) as [payload [Pay' Inner]].
          pose proof (payload_det W _ _ _ _ Pay' (payload_subst W (type_meanings d) _ _ _ Pay)) as ->.
          change (app_subst (type_meanings d) (TRecord ftys)) with (ground d (TRecord ftys)) in Inner.
          rewrite ground_record in Inner.
          destruct (record_value W P _ _ _ Inner) as [values [-> Fs]].
          exists values; split; [cbn; apply assoc_slook|exact Fs].
        - rewrite <- Same, ground_record in T1. destruct (record_value W P _ _ _ T1) as [values [-> Fs]].
          exists values; split; [cbn; apply assoc_slook|exact Fs]. }
      destruct Rec as [values [Get Fs]].
      destruct (field_lookup_typed W P H1 values _ n (ground d t) Fs) as [value [Found TV]];
        [rewrite slook_ground, Look; reflexivity|].
      rewrite Get, Found. exists H1; repeat split; auto. intros ? ? E; exact E.
  (* ---------------- Blk ---------------- *)
  - destruct D as
      [F C y exp x t Dy
      |F C exp
      |F C st0 n ann y rest tl exp declared v vt t0 xr t made mu Let Ann Dy Decl Named Drest
      |F C n y rest tl exp tv v vt xr t made Found Dy Fits Drest Named
      |F C y exp v vt Dy
      |F C y rest tl exp v vt xr t made NonEmpty Dy Drest
      |F C c b rest tl exp cx' bx bt xr t made Dc Db Drest
      |F C b rest tl exp bx bt xr t made Cases Db Drest
      |F C b exp bx bt Db NotBreaks
      |F C its rest tl exp wrap C' F' xr t made made' Ditems Drest];
      intros Avoid d types H lg lst Pre0.
    + eapply AR.O.outcome_breaks; [|exact (ex_step F C y exp x t Dy d types H lg lst Pre0)].
      intro B; apply A.BBTail; exact B.
    + cbn [AR.O.I.eval]. eapply outcome_now; [exact Pre0|constructor].
    + (* let *) cbn [AR.O.I.eval].
      assert (Fits0 : A.fits vt t0).
      { destruct declared as [dt|]; cbn in Decl;
          [destruct Decl as [Fd E0]; subst t0; exact Fd|subst t0; apply fits_refl]. }
      pose proof (AR.O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits0 (sex F C y _ v vt Dy d types H lg lst (pre_lower Pre0))) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ st0 rest tl (let_breaks _ _ _ _ _ _ Let B)) Out).
      intros H1 g1 st1 w E1 [Frame [Ctx [L1 [S1 [Abs Fmd1]]]]] T1.
      eapply outcome_pop. eapply AR.O.outcome_breaks; [apply block_breaks_cons|].
      apply (sblk F _ rest tl exp xr t made Drest Avoid d _ H1 _ st1).
      split; [exact Frame|]. split; [exact (ctx_locals_push F C d types n t0 mu Frame Ctx Named)|].
      split; [exact (locals_push W P H1 g1 types _ w _ L1 T1)|]. split; [exact S1|split; [exact (realizes_same _ C _ _ _ eq_refl eq_refl eq_refl eq_refl (realizes_lower (S k) k C d H1 (le_S _ _ (le_n k)) Abs))|]].
      assert (T0 : PublicFormation.TyOk W t0 /\ PublicFormation.ScopedTy W C t0).
      { destruct declared as [dt|]; cbn in Decl.
        * destruct Decl as [_ E0]. subst t0. destruct ann as [a|]; cbn in Ann; [|discriminate].
          destruct Ann as [dd [Td E]]. injection E as <-.
          split; [exact (PublicFormation.tyof_ok W _ _ _ _ _ Td (PublicFormation.cf_ty _ _ (proj1 (proj1 Fmd1))) (proj2 (proj1 (proj2 Fmd1))))|].
          exact (PublicScoping.tyof_scoped_ctx W C _ _ _ _ Td (proj1 (proj2 (proj2 (proj2 (proj2 Fmd1)))))).
        * subst t0. split; [exact (PublicFormation.ex_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ Dy (proj1 Fmd1) (proj1 (proj2 Fmd1)) (PublicFormation.optok_none W))|].
          exact (ex_scoped F C y v vt Dy Fmd1). }
      split; [exact (PublicFormation.add_local_formed W C n t0 mu (proj1 Fmd1) (proj1 T0))|].
      split; [exact (proj1 (proj2 Fmd1))|exact (PublicScoping.add_local_scoped W F C n t0 mu (proj2 (proj2 Fmd1)) (proj2 T0))].
    + (* assign *) cbn [AR.O.I.eval].
      pose proof (assign_eval k Bel F C n y tv v vt d types H lg lst Found Named Dy Fits (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ _ rest tl (A.SBAssign n B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply AR.O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest Avoid d types H1 g1 st1 (pre_lower Pre1))].
    + (* last expression *) cbn [AR.O.I.eval].
      pose proof (sex F C y None v vt Dy d types H lg lst (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ _ [] None (A.SBExpr B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      destruct (ty_eqb vt A.never) eqn:Never.
      * apply PublicTypeFacts.ty_eqb_spec in Never. subst vt. exfalso; exact (ground_never d H1 w T1).
      * eapply AR.O.outcome_breaks; [|exact (unit_k F C d types H1 g1 st1 (pre_lower Pre1))]. intros [].
    + (* expression *) cbn [AR.O.I.eval].
      pose proof (sex F C y None v vt Dy d types H lg lst (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ _ rest tl (A.SBExpr B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply AR.O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest Avoid d types H1 g1 st1 (pre_lower Pre1))].
    + (* while *) cbn [AR.O.I.eval].
      pose proof (while_safe k Bel k (le_n k) F C c cx' b bx bt d types H lg lst Dc Db (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ _ rest tl (A.SBWhile b B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply AR.O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest Avoid d types H1 g1 st1 (pre_lower Pre1))].
    + (* loop *) cbn [AR.O.I.eval].
      pose proof (proj1 (loop_safe k Bel k (le_n k) F C b bx bt d types H lg lst Db (pre_lower Pre0))) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0 (fun f => False_ind _ f) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply AR.O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest Avoid d types H1 g1 st1 (pre_lower Pre1))].
    + (* last loop *) cbn [AR.O.I.eval].
      destruct (loop_safe k Bel k (le_n k) F C b bx bt d types H lg lst Db (pre_lower Pre0)) as [Out Normal'].
      destruct (AR.O.I.eval k P d lg lst (ELoop bx)) as [[] g1 st1]; cbn in Out, Normal' |- *; auto; try contradiction;
        try (destruct Out as [[] _]).
      exfalso. exact (NotBreaks (Normal' v eq_refl)).
    + (* bind *)
      assert (Av1 : forall g, In g made -> ~ In g (A.gens t)) by (intros g I; apply Avoid; apply in_or_app; left; exact I).
      assert (Av2 : forall g, In g made' -> ~ In g (A.gens t)) by (intros g I; apply Avoid; apply in_or_app; right; exact I).
      assert (XR : CodeSafe (S k) F' C' t (A.BlockBreaks W (A.fr_mod F) (ABlock rest tl)) xr).
      { destruct (bind_items_frame _ _ _ _ _ _ _ Ditems) as [ModI _].
        intros j Le d' types' H' g' st' Pre'. rewrite <- ModI. destruct (Nat.eq_dec j (S k)) as [->|Ne].
        - exact (blk_step F' C' rest tl exp xr t made' Drest Av2 d' types' H' g' st' Pre').
        - destruct (Bel j ltac:(lia)) as [_ [SBj _]]. exact (SBj F' C' rest tl exp xr t made' Drest Av2 d' types' H' g' st' Pre'). }
      pose proof (items_safe k Bel (S k) F C its wrap C' F' made Ditems (le_n _) t _ xr Av1 XR (S k) (le_n _)
        d types H lg lst Pre0) as Out.
      eapply AR.O.outcome_breaks; [|exact Out].
      intros [Rest|[[sp oy] [Iit Bi]]]; [apply block_breaks_cons; exact Rest|].
      apply stmt_breaks_block. eapply A.SBBind; [exact Iit|exact Bi].
  (* ---------------- BlkE ---------------- *)
  - destruct D as [F C ss tl exp x t made Dblk Escape Fits]. intros d types H lg lst Pre0.
    (* a nested block runs in the frame without pins, which code safety does not read *)
    assert (FS : FrameSame F (A.without_pins F)) by (repeat split).
    apply (outcome_frame F (A.without_pins F) d H types t _ _ FS).
    exact (blk_step (A.without_pins F) C ss tl exp x t made Dblk Escape d types H lg lst (pre_frame _ _ _ _ _ _ _ _ _ FS Pre0)).
  (* ---------------- Branch ---------------- *)
  - destruct D as [F C b exp x t Db|F C y exp x t NotBlock Dy]; intros d types H lg lst Pre0.
    + destruct (blke_branch_ty _ _ _ _ _ _ Db) as [Branch FitsExp].
      apply (AR.O.outcome_fits W P state_ok _ _ _ _ t).
      { destruct exp; [exact FitsExp|exact Branch]. }
      eapply AR.O.outcome_breaks; [|exact (blke_step F C b exp x t Db d types H lg lst Pre0)].
      intro B; apply A.BkBlock; exact B.
    + exact (ex_step F C y exp x t Dy d types H lg lst Pre0).
  (* ---------------- Cond ---------------- *)
  - destruct D as [F C c x t b Lang Dc Fits]; intros d types H lg lst Pre0.
    exists b; split; [exact Lang|].
    exact (AR.O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits (ex_step F C c _ x t Dc d types H lg lst Pre0)).
Qed.
End Step.

Theorem safe_all : forall k, Safe k.
Proof.
  assert (All : forall n k, k <= n -> Safe k).
  { induction n as [|n IH]; intros k Le.
    - inversion Le; subst. exact safe_zero.
    - inversion Le as [|? Le']; subst; [|exact (IH k Le')].
      assert (Bel : Below n) by (intros j Lj; exact (IH j Lj)).
      split; [exact (ex_step n Bel)|]. split; [exact (blk_step n Bel)|].
      split; [exact (blke_step n Bel)|]. split; [exact (branch_step n Bel)|exact (cond_step n Bel)]. }
  intros k; exact (All k k (le_n k)).
Qed.

End Slice.
End BodySlice.
