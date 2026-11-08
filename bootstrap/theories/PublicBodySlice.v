(* The body-safety induction for source bodies (T02 slice). Evaluating
   the code of any Ex/Blk/BlkE/Branch/Cond derivation under a realizing
   state produces an Outcome: never Stuck, a Breaking only where the source
   breaks inside a loop, typed values and stores otherwise.

   The proof is by induction on fuel, inverting derivations. Constructors
   outside the slice (contextual vals, calls, methods, operators, literals
   and bind statements) and two static facts (tag construction and match
   arm selection) are explicit Section hypotheses; each is a named
   remaining obligation, not an assumption about accepted programs. *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBoolTyping PublicBodySafety PublicTypeFacts PublicArms PublicTagCheck PublicContextFacts
  PublicCallRealize.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.
Local Open Scope string_scope.

Module BodySlice (Prims : PRIMITIVES).
Module AR := Arms Prims.
Module O := AR.O.
Module I := O.I.
Module B := BoolTruthy Prims.
Module TC := Checks Prims.

Section Slice.
Variables (W : A.world) (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Let P := program_of Prog argv read.
Variable state_ok : heap_types -> Prims.mstate -> Prop.

Hypothesis Layouts : Platform.Layouts W.
Hypothesis Bools : BoolSymbols W Prog.

Definition Absorbs (C : A.cx) (d : environment) : Prop :=
  forall t, ground d (A.canon C t) = ground d t.

(* What a call of a closure may produce, read at the caller's types. *)
Definition CallOutcome (H : heap_types) (t : ty) (r : I.evaluation) : Prop :=
  match r with
  | I.Evaluation signal _ st =>
      match signal with
      | Normal w | Returning w => exists H', heap_extends H H' /\ state_ok H' st /\ value_typed W P H' w t
      | Stuck _ | Breaking => False
      | _ => True
      end
  end.
Definition PrimOutcome (H : heap_types) (t : ty) (r : signal * Prims.mstate) : Prop :=
  match r with
  | (signal, st) =>
      match signal with
      | Normal w => exists H', heap_extends H H' /\ state_ok H' st /\ value_typed W P H' w t
      | Trap _ | Exit _ | UnsupportedOperation _ | OutOfFuel => True
      | _ => False
      end
  end.

(* A closure is callable below fuel n: fuel only decreases, so a closure
   available at fuel n is only ever invoked at a smaller fuel. The ghost
   heap only grows, so it is invoked at an extension of the heap H0 it was
   available at, where its captured values are typed. *)
Definition CallableOk (n : nat) (H0 : heap_types) (d : environment) (params : list ty) (ret : ty) (pv : provision_value) : Prop :=
  match pv with
  | FunctionClosure f df => exists body, view_function P f = Some body /\
      forall j, j < n -> forall H st ws, heap_extends H0 H -> state_ok H st ->
        Forall2 (fun w t => value_typed W P H w (ground d t)) ws params ->
        CallOutcome H (ground d ret) (I.eval j P df (parameter_values ws) st body)
  | PrimitiveClosure p held => forall H st ws, heap_extends H0 H -> state_ok H st ->
        Forall2 (fun w t => value_typed W P H w (ground d t)) ws params ->
        PrimOutcome H (ground d ret) (I.run_prim_at P p held ws st)
  | _ => False
  end.

(* The static provisions, realized under their runtime keys (T31). *)
Definition EnvOk (n : nat) (C : A.cx) (d : environment) (H : heap_types) : Prop :=
  (forall v t, A.nlook v (A.c_vals C) = Some t -> exists w,
     lookup_provision (IVal v) d = Some (ValueProvision w) /\ value_typed W P H w (ground d t)) /\
  (forall f g, A.nlook f (A.c_fns C) = Some g -> exists pv,
     lookup_provision (IFn f) d = Some pv /\ CallableOk n H d (A.sg_params g) (A.sg_ret g) pv) /\
  (forall r m g, A.mlook r m (A.c_mths C) = Some g -> exists pv,
     lookup_provision (A.rkey C r m) d = Some pv /\ CallableOk n H d (r :: A.sg_params g) (A.sg_ret g) pv) /\
  (* functor closures: their realization is T15's *)
  (forall u fx, A.nlook u (A.c_funs C) = Some fx -> exists h sms dc,
     lookup_provision (IFun u) d = Some (FunctorClosure h sms dc)).

(* Every provision is filed under a key of its own kind. *)
Definition KeysMatch (C : A.cx) : Prop := PublicCallRealize.KeysMatch (A.c_provisions C).

(* Static facts of a context in force (T29). *)
Definition StaticOk (C : A.cx) : Prop :=
  (forall t, A.canon C (A.canon C t) = A.canon C t) /\ KeysMatch C.

Definition Realizes (n : nat) (C : A.cx) (d : environment) (H : heap_types) : Prop :=
  Absorbs C d /\ EnvOk n C d H /\ StaticOk C.

Definition Pre n F C d types H g st : Prop :=
  FrameOk F /\ CtxLocals F C d types /\ locals_typed W P H g types /\ state_ok H st /\ Realizes n C d H.

Notation Outcome := (O.Outcome W P state_ok).

Definition SafeEx k : Prop := forall F C a exp x t, A.Ex W Prog F C a exp x t ->
  forall d types H g st, Pre k F C d types H g st ->
  Outcome F d H types t (A.Breaks a) (I.eval k P d g st x).
Definition SafeBlk k : Prop := forall F C ss tl exp x t made, A.Blk W Prog F C ss tl exp x t made ->
  forall d types H g st, Pre k F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks (ABlock ss tl)) (I.eval k P d g st x).
Definition SafeBlkE k : Prop := forall F C b exp x t, A.BlkE W Prog F C b exp x t ->
  forall d types H g st, Pre k F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks b) (I.eval k P d g st x).
Definition SafeBranch k : Prop := forall F C y exp x t, A.Branch W Prog F C y exp x t ->
  forall d types H g st, Pre k F C d types H g st ->
  Outcome F d H types t (A.Breaks y) (I.eval k P d g st x).
Definition SafeCond k : Prop := forall F C c x, A.Cond W Prog F C c x ->
  forall d types H g st, Pre k F C d types H g st ->
  exists b, A.LangSym W "bool" A.NSName "Bool" b /\
    Outcome F d H types (TNom b []) (A.Breaks c) (I.eval k P d g st x).
Definition Safe k : Prop := SafeEx k /\ SafeBlk k /\ SafeBlkE k /\ SafeBranch k /\ SafeCond k.
Definition Below k : Prop := forall j, j <= k -> Safe j.

(* ---- obligations outside the slice -------------------------------------- *)

Definition StepSafe (k : nat) F C t (breaks : Prop) (x : expr) : Prop :=
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t breaks (I.eval (S k) P d g st x).

(* A method a family supplies at the call: a functor application (T15/T16). *)
Hypothesis family_method_case : forall k, Below k -> forall F C o ox ot0 m args u matching th sm C' ks g xs (B : Prop),
  A.Ex W Prog F C o None ox ot0 -> (A.Breaks o -> B) -> (forall a, In a args -> A.Breaks a -> B) ->
  A.mlook (A.canon C ot0) m (A.c_mths C) = None ->
  A.FamilyApplication W C u matching th sm C' ks ->
  In (A.mkey (A.canon C ot0) m) ks -> A.OnlyFamily W C u (A.mkey (A.canon C ot0) m) ->
  A.mlook (A.canon C ot0) m (A.c_mths C') = Some g -> A.Args W Prog F C args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) B
    (apply_expression u th sm (EMethod ox m (A.rkey C' (A.canon C ot0) m) xs)).
(* A defined attached method: a direct call at the receiver's arguments (T03/T06/T14). *)
Hypothesis attached_defined_case : forall k, Below k -> forall F C o ox ot0 ot n args h att Ca ns sm th g xs rargs wrap Cd (B : Prop),
  A.Ex W Prog F C o None ox ot0 -> (A.Breaks o -> B) -> (forall a, In a args -> A.Breaks a -> B) ->
  ot = A.canon C ot0 ->
  A.head ot = Some h -> A.Attached W h n att ->
  ~ (exists m, A.w_scope W (A.fr_mod F) A.NSDetached n = Some (A.ESym m) /\ A.mlook ot m (A.c_mths C) <> None) ->
  (exists rn ps rt b, A.ast_of W att = Some (ADFn (RAttached rn) n ps rt (Some b))) ->
  rargs = (match ot with TNom _ xs => xs | _ => [] end) ->
  A.CtxOf W att Ca ns ->
  A.Demand W (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C)
    (map (A.at_site (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C)) ns) []
    (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C) wrap Cd ->
  A.Satisfies Cd Ca th ns sm -> A.CallTheta W Cd Ca att th -> A.SigOf W att (A.c_ty Cd) (Some ot) g ->
  A.Args W Prog F C args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) B (wrap (ECall att th sm (ox :: xs))).
Hypothesis Reps : Platform.Representations W Prog.

Lemma forall2_keys : forall (reqs : list nat) (xs : list (nat * ty)) (R : nat -> ty -> Prop),
  Forall2 (fun r ra => fst ra = r /\ R r (snd ra)) reqs xs -> map fst xs = reqs.
Proof. intros reqs xs R F; induction F as [|r ra rs ras [E _] _ IH]; cbn; [reflexivity|congruence]. Qed.

(* Elaborated types are formed: a union has nominal members with distinct
   heads, also after canonicalization (T29). *)
Hypothesis ex_formed : forall F C a exp x t, A.Ex W Prog F C a exp x t -> UnionFormed (A.canon C t).

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
Proof.
  intros n m H0 d params ret [| f df | |] Le Ok; cbn in *; auto.
  destruct Ok as [body [View Run]]. exists body; split; [exact View|].
  intros j Lt; apply Run; lia.
Qed.

Lemma realizes_lower : forall n m C d H, m <= n -> Realizes n C d H -> Realizes m C d H.
Proof.
  intros n m C d H Le [Abs [[Vals [Fns [Mths Funs]]] St]]. split; [exact Abs|]. split; [|exact St].
  split; [exact Vals|]. split; [|split; [|exact Funs]].
  - intros f g Found. destruct (Fns f g Found) as [pv [Look Ok]].
    exists pv; split; [exact Look|exact (callable_lower n m _ _ _ _ _ Le Ok)].
  - intros r m' g Found. destruct (Mths r m' g Found) as [pv [Look Ok]].
    exists pv; split; [exact Look|exact (callable_lower n m _ _ _ _ _ Le Ok)].
Qed.

Lemma callable_extends : forall n H H' d params ret pv, heap_extends H H' ->
  CallableOk n H d params ret pv -> CallableOk n H' d params ret pv.
Proof.
  intros n H H' d params ret [| f df | |] Ext Ok; cbn in *; auto.
  - destruct Ok as [body [View Run]]. exists body; split; [exact View|].
    intros j Lt Hc st ws E; apply Run; [exact Lt|exact (extends_trans _ _ _ Ext E)].
  - intros Hc st ws E; apply Ok; exact (extends_trans _ _ _ Ext E).
Qed.

Lemma realizes_extends : forall n C d H H', heap_extends H H' -> Realizes n C d H -> Realizes n C d H'.
Proof.
  intros n C d H H' Ext [Abs [[Vals [Fns [Mths Funs]]] St]]. split; [exact Abs|].
  split; [|exact St]. split; [|split; [|split; [|exact Funs]]].
  - intros v t Found. destruct (Vals v t Found) as [w [Look T]].
    exists w; split; [exact Look|eapply value_typed_extends; eassumption].
  - intros f g Found. destruct (Fns f g Found) as [pv [Look Ok]].
    exists pv; split; [exact Look|exact (callable_extends _ _ _ _ _ _ _ Ext Ok)].
  - intros r m g Found. destruct (Mths r m g Found) as [pv [Look Ok]].
    exists pv; split; [exact Look|exact (callable_extends _ _ _ _ _ _ _ Ext Ok)].
Qed.

Lemma realizes_same : forall n C C' d H,
  A.c_ty C' = A.c_ty C -> A.c_sub C' = A.c_sub C -> A.c_provisions C' = A.c_provisions C ->
  Realizes n C d H -> Realizes n C' d H.
Proof.
  intros n [ty sub provs locs] [ty' sub' provs' locs'] d H; cbn; intros -> -> -> Real; exact Real.
Qed.

Lemma pre_lower : forall n F C d types H g st, Pre (S n) F C d types H g st -> Pre n F C d types H g st.
Proof.
  intros n F C d types H g st [Frame [Ctx [L [St Real]]]].
  exact (conj Frame (conj Ctx (conj L (conj St (realizes_lower (S n) n C d H (le_S _ _ (le_n n)) Real))))).
Qed.

Arguments pre_lower {n F C d types H g st} _.

Lemma pre_le : forall n m F C d types H g st, m <= n -> Pre n F C d types H g st -> Pre m F C d types H g st.
Proof.
  intros n m F C d types H g st Le [Frame [Ctx [L [St Real]]]].
  exact (conj Frame (conj Ctx (conj L (conj St (realizes_lower n m C d H Le Real))))).
Qed.

Lemma pre_after : forall n F C d types H g st H' g' st',
  Pre n F C d types H g st -> heap_extends H H' -> state_ok H' st' ->
  locals_typed W P H' g' types -> Pre n F C d types H' g' st'.
Proof.
  intros n F C d types H g st H' g' st' [Frame [Ctx [_ [_ Real]]]] Ext State Locals.
  exact (conj Frame (conj Ctx (conj Locals (conj State (realizes_extends n C d H H' Ext Real))))).
Qed.

(* A non-Normal result passes through unchanged. *)
Lemma outcome_pass : forall F d H types t t' (breaks breaks' : Prop) r,
  (breaks -> breaks') ->
  (forall value, I.evaluation_signal r <> Normal value) ->
  Outcome F d H types t breaks r -> Outcome F d H types t' breaks' r.
Proof.
  intros F d H types t t' breaks breaks' [[] g st] Weaken NotNormal Out; cbn in *; auto.
  - exfalso; exact (NotNormal v eq_refl).
  - destruct Out as [Br Rest]; auto.
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
  Outcome F d H types t breaks (I.Evaluation (Normal w) g st).
Proof.
  intros n F C d types H g st t breaks w [_ [_ [Locals [State _]]]] Typed.
  exists H; repeat split; auto. intros a c E; exact E.
Qed.

Lemma ground_never : forall d H w, ~ value_typed W P H w (ground d A.never).
Proof. intros d H w Typed. exact (empty_union_uninhabited W P H w Typed). Qed.

Lemma outcome_pop : forall F d H types x tau t breaks r,
  Outcome F d H ((x, tau) :: types) t breaks r -> Outcome F d H types t breaks (I.pop1 r).
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
  Outcome F d H (front ++ types) t breaks r -> Outcome F d H types t breaks (I.popn n r).
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
  I.evaluation_signal r <> Breaking ->
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

Lemma block_breaks_cons : forall s rest tl,
  A.BlockBreaks (ABlock rest tl) -> A.BlockBreaks (ABlock (s :: rest) tl).
Proof.
  intros s rest tl B. inversion B; subst.
  - eapply A.BBStmt; [right; eassumption|eassumption].
  - apply A.BBTail; assumption.
Qed.

Lemma stmt_breaks_block : forall s rest tl,
  A.StmtBreaks s -> A.BlockBreaks (ABlock (s :: rest) tl).
Proof. intros s rest tl B. eapply A.BBStmt; [left; reflexivity|exact B]. Qed.

Lemma path_never_breaks : forall pa a, ~ A.Breaks (AEPath pa a).
Proof. intros pa a B; inversion B. Qed.

Lemma assoc_slook : forall f (fs : list (fld * value)), I.assoc_fld f fs = A.slook f fs.
Proof. intros f fs; induction fs as [|[g v] r IH]; cbn; [reflexivity|rewrite IH; reflexivity]. Qed.

(* A block without a tail yields () or diverges. *)
Lemma tailless_block : forall F C ss tl exp x t made,
  A.Blk W Prog F C ss tl exp x t made -> tl = None -> t = TUnit \/ t = A.never.
Proof.
  intros F C ss tl exp x t made D. induction D; intros Tail; try discriminate; auto.
  destruct (ty_eqb vt A.never); auto.
Qed.

Lemma blke_branch_ty : forall F C b exp x t,
  A.BlkE W Prog F C b exp x t -> A.fits t (A.branch_ty b t) /\ A.opt_fits t exp.
Proof.
  intros F C b exp x t D. destruct D as [F C ss tl exp x t made Blk _ Fits]. split; [|exact Fits].
  destruct tl; cbn; [apply fits_refl|].
  destruct (tailless_block _ _ _ _ _ _ _ _ Blk eq_refl) as [->| ->]; [apply fits_refl|apply fits_never].
Qed.

Lemma truthy_same : forall w, I.truthy P w = B.I.truthy P w.
Proof. reflexivity. Qed.

Lemma below_le : forall k j, Below k -> j <= k -> Safe j.
Proof. intros k j Bel Le; exact (Bel j Le). Qed.

Lemma typed_unit : forall d H, value_typed W P H VUnit (ground d TUnit).
Proof. intros; constructor. Qed.

Lemma while_safe : forall k, Below k -> forall j, j <= k ->
  forall F C c cx b bx bt d types H g st,
  A.Cond W Prog F C c cx -> A.BlkE W Prog (A.with_loop F) C b None bx bt ->
  Pre j F C d types H g st ->
  Outcome F d H types TUnit (A.Breaks c) (I.eval j P d g st (EWhile cx bx)).
Proof.
  intros k Bel j. induction j as [|j IH]; intros Le F C c cx b bx bt d types H g st Cond Body Pre0;
    [exact Logic.I|].
  destruct (Bel j ltac:(lia)) as [_ [_ [SBlkE [_ SCond]]]].
  destruct (SCond F C c cx Cond d types H g st (pre_lower Pre0)) as [bsym [Lang OutC]].
  cbn [I.eval]. destruct (I.eval j P d g st cx) as [sig g1 st1] eqn:EC.
  destruct sig; try (eapply outcome_pass with (t := TNom bsym []); [exact (fun x => x)|cbn; congruence|exact OutC]).
  destruct OutC as [H1 [E1 [S1 [L1 T1]]]].
  assert (Bool : value_typed W (program_of Prog argv read) H1 v (TNom bsym [])) by exact T1.
  destruct (B.truthy_typed W Prog argv read H1 bsym v Bools Layouts Lang Bool) as [bit Bit].
  replace (I.truthy P v) with (Some bit) by (rewrite truthy_same; symmetry; exact Bit). destruct bit.
  - pose proof (pre_lower (pre_after _ _ _ _ _ _ _ _ H1 g1 st1 Pre0 E1 S1 L1)) as Pre1.
    pose proof (SBlkE (A.with_loop F) C b None bx bt Body d types H1 g1 st1 (pre_loop _ _ _ _ _ _ _ _ Pre1)) as OutB.
    destruct (I.eval j P d g1 st1 bx) as [sig2 g2 st2] eqn:EB.
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
  A.BlkE W Prog (A.with_loop F) C b None bx bt ->
  Pre j F C d types H g st ->
  Outcome F d H types TUnit False (I.eval j P d g st (ELoop bx)) /\
  (forall w, I.evaluation_signal (I.eval j P d g st (ELoop bx)) = Normal w -> A.BlockBreaks b).
Proof.
  intros k Bel j. induction j as [|j IH]; intros Le F C b bx bt d types H g st Body Pre0;
    [split; [exact Logic.I|cbn; discriminate]|].
  destruct (Bel j ltac:(lia)) as [_ [_ [SBlkE _]]].
  pose proof (SBlkE (A.with_loop F) C b None bx bt Body d types H g st (pre_loop _ _ _ _ _ _ _ _ (pre_lower Pre0))) as OutB.
  cbn [I.eval]. destruct (I.eval j P d g st bx) as [sig g2 st2] eqn:EB.
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
  exists a exp t', A.Ex W Prog F C a exp e t' /\
    (A.fits t' t \/ A.fits (A.canon C t') (A.canon C t)) /\ (A.Breaks a -> breaks).

Lemma outcome_conv : forall F d H types t t' breaks r,
  (forall H' w, value_typed W P H' w (ground d t) -> value_typed W P H' w (ground d t')) ->
  Outcome F d H types t breaks r -> Outcome F d H types t' breaks r.
Proof.
  intros F d H types t t' breaks [[] g st] Conv Out; cbn in *; auto.
  destruct Out as [H' [E [S [L T]]]]. exists H'; repeat split; auto.
Qed.

Lemma canon_fits_ground : forall C d H w t t', Absorbs C d ->
  (A.fits t t' \/ A.fits (A.canon C t) (A.canon C t')) ->
  value_typed W P H w (ground d t) -> value_typed W P H w (ground d t').
Proof.
  intros C d H w t t' Abs [Fits|Fits] T; [exact (fits_ground W P H d w t t' Fits T)|].
  rewrite <- Abs in T |- *. exact (fits_ground W P H d w _ _ Fits T).
Qed.

Lemma args_safe : forall k, Below k -> forall j, j <= k ->
  forall F C (breaks : Prop) es tys d types H g st,
  Forall2 (ArgSafe F C breaks) es tys -> Pre j F C d types H g st ->
  match I.eval_arguments j P d g st es with
  | inl (ws, g', st') => exists H', heap_extends H H' /\ state_ok H' st' /\
      locals_typed W P H' g' types /\ Forall2 (fun w t => value_typed W P H' w (ground d t)) ws tys
  | inr r => Outcome F d H types TUnit breaks r /\ forall w, I.evaluation_signal r <> Normal w
  end.
Proof.
  intros k Bel j. induction j as [|j IH]; intros Le F C breaks es tys d types H g st Args Pre0;
    [split; [exact Logic.I|cbn; discriminate]|].
  destruct Args as [|e t es tys [a [exp [t' [D [Fits Brk]]]]] Rest]; cbn [I.eval_arguments].
  - destruct Pre0 as [_ [_ [L [S _]]]]. exists H; repeat split; auto. intros ? ? E; exact E.
  - destruct (Bel j ltac:(lia)) as [SEx _].
    pose proof (outcome_conv _ _ _ _ _ _ _ _
      (fun H' w => canon_fits_ground C d H' w t' t (proj1 (proj2 (proj2 (proj2 (proj2 Pre0))))) Fits)
      (SEx F C a exp e t' D d types H g st (pre_lower Pre0))) as Out.
    destruct (I.eval j P d g st e) as [sig g1 st1] eqn:E.
    destruct sig; cbn in Out;
      try (split; [eapply outcome_pass; [exact Brk|cbn; congruence|exact Out]|cbn; congruence]).
    destruct Out as [H1 [E1 [S1 [L1 T1]]]].
    pose proof (IH ltac:(lia) F C breaks es tys d types H1 g1 st1 Rest
      (pre_lower (pre_after _ _ _ _ _ _ _ _ _ _ _ Pre0 E1 S1 L1))) as Next.
    destruct (I.eval_arguments j P d g1 st1 es) as [[[ws g2] st2]|r].
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
  Outcome F d H types (TNom s xs) breaks (I.Evaluation sig g st).
Proof.
  intros F d H types s xs breaks sig g st State Locals Result.
  destruct sig; cbn in *; try contradiction; auto.
  exists H; repeat split; auto. intros ? ? E; exact E.
Qed.

Lemma let_breaks : forall st n ann y mu, A.LetStmt st n ann y mu -> A.Breaks y -> A.StmtBreaks st.
Proof. intros st n ann y mu Let B; destruct Let; [apply A.SBLet|apply A.SBVar]; exact B. Qed.

Lemma unit_never_breaks : ~ A.Breaks AEUnit.
Proof. intro B; inversion B. Qed.

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

Lemma record_args : forall F C pa a fs ftys xfs,
  Forall2 (fun ft xf => fst xf = fst ft /\
    exists n v vt, n = fst ft /\ In (n, v) fs /\
      A.Ex W Prog F C (match v with Some y => y | None => AEPath [n] None end) (Some (snd ft)) (snd xf) vt /\
      A.fits vt (snd ft)) ftys xfs ->
  Forall2 (ArgSafe F C (A.Breaks (AERec pa a fs))) (map snd xfs) (map snd ftys) /\
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
  Forall2 (ArgSafe F C (A.Breaks (AERec pa a fs))) (map snd xfs) (map snd ftys) ->
  map fst xfs = map fst ftys -> Pre k F C d types H g st ->
  Outcome F d H types (TRecord ftys) (A.Breaks (AERec pa a fs)) (I.eval k P d g st (ERecord xfs)).
Proof.
  intros [|k] Bel F C pa a fs ftys xfs d types H g st Args Names Pre0; [exact Logic.I|].
  cbn [I.eval]. pose proof (args_safe (S k) Bel k ltac:(lia) _ _ _ _ _ d types H g st Args (pre_lower Pre0)) as Out.
  match goal with |- context [I.eval_arguments ?j ?V ?d0 ?g0 ?s0 ?es] =>
    change (I.eval_arguments j V d0 g0 s0 es) with (I.eval_arguments k P d g st (map snd xfs)) end.
  destruct (I.eval_arguments k P d g st (map snd xfs)) as [[[ws g'] st']|r]; cbn in Out; hnf.
  - destruct Out as [H' [E [S [L T]]]]. exists H'; repeat split; auto.
    match goal with |- value_typed _ _ _ _ ?T => change T with (ground d (TRecord ftys)) end.
    exact (record_typed _ _ _ _ _ Names T).
  - destruct Out as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
Qed.

Lemma assign_eval : forall k, Below k ->
  forall F C n y tv v vt d types H g st,
  A.slook n (A.c_locals C) = Some (tv, A.LVar) -> A.Named F n ->
  A.Ex W Prog F C y (Some tv) v vt -> A.fits vt tv -> Pre k F C d types H g st ->
  Outcome F d H types TUnit (A.Breaks y) (I.eval k P d g st (EAssign (A.var_of F n) v)).
Proof.
  intros [|k] Bel F C n y tv v vt d types H g st Found Named D Fits Pre0; [exact Logic.I|].
  destruct (Bel k ltac:(lia)) as [SEx _].
  pose proof (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits (SEx F C y _ v vt D d types H g st (pre_lower Pre0))) as Out.
  pose proof Pre0 as [_ [[Loc _] _]].
  cbn [I.eval]. destruct (I.eval k P d g st v) as [sig g1 st1] eqn:E.
  destruct sig; try (eapply outcome_pass with (t := tv); [exact (fun x => x)|cbn; congruence|exact Out]).
  destruct Out as [H1 [E1 [S1 [L1 T1]]]].
  destruct (locals_assign W P H1 g1 types (A.var_of F n) (ground d tv) v0 L1 (Loc n tv A.LVar Found Named) T1)
    as [g2 [Assign L2]].
  rewrite Assign. exists H1; repeat split; auto. constructor.
Qed.

Lemma args_argsafe : forall F C args ts xs, A.Args W Prog F C args ts xs ->
  forall (B : Prop), (forall a, In a args -> A.Breaks a -> B) -> Forall2 (ArgSafe F C B) xs ts.
Proof.
  intros F C args ts xs D. induction D as [F C|F C a r t ts x xs at' Dx Fits Rest IH]; intros B Brk; constructor.
  - exists a, (Some t), at'. split; [exact Dx|]. split; [right; exact Fits|]. intro Ba; exact (Brk a (or_introl eq_refl) Ba).
  - apply IH. intros a' In' Ba; exact (Brk a' (or_intror In') Ba).
Qed.

Lemma outcome_ret_call : forall F d H H1 types t (b : Prop) r g1,
  heap_extends H H1 -> locals_typed W P H1 g1 types -> CallOutcome H1 (ground d t) r ->
  Outcome F d H types t b (I.ret_call g1 r).
Proof.
  intros F d H H1 types t b [[] l st] g1 E1 L1 Out; cbn in *; try contradiction; auto;
    destruct Out as [H' [E' [S' T']]]; exists H'; repeat split; auto;
    try (eapply extends_trans; eassumption); eapply locals_extends; eassumption.
Qed.

Lemma outcome_prim : forall F d H H1 types t (b : Prop) o st2 g1,
  heap_extends H H1 -> locals_typed W P H1 g1 types -> PrimOutcome H1 (ground d t) (o, st2) ->
  Outcome F d H types t b (I.Evaluation o g1 st2).
Proof.
  intros F d H H1 types t b [] st2 g1 E1 L1 Out; cbn in *; try contradiction; auto.
  destruct Out as [H' [E' [S' T']]]; exists H'; repeat split; auto;
    [eapply extends_trans; eassumption|eapply locals_extends; eassumption].
Qed.

(* Sequencing: a Normal first result continues with a second evaluation. *)
Lemma outcome_seq : forall n F C d types H g st t1 t (b1 b : Prop) r1 (k2 : value -> locals -> Prims.mstate -> I.evaluation),
  Pre n F C d types H g st -> (b1 -> b) ->
  Outcome F d H types t1 b1 r1 ->
  (forall H1 g1 st1 w, heap_extends H H1 -> Pre n F C d types H1 g1 st1 ->
     value_typed W P H1 w (ground d t1) -> Outcome F d H1 types t b (k2 w g1 st1)) ->
  Outcome F d H types t b
    (match r1 with
     | I.Evaluation (Normal w) g1 st1 => k2 w g1 st1
     | I.Evaluation (Returning v) g1 st1 => I.Evaluation (Returning v) g1 st1
     | I.Evaluation Breaking g1 st1 => I.Evaluation Breaking g1 st1
     | I.Evaluation (Stuck c) g1 st1 => I.Evaluation (Stuck c) g1 st1
     | I.Evaluation (Trap c) g1 st1 => I.Evaluation (Trap c) g1 st1
     | I.Evaluation (Exit c) g1 st1 => I.Evaluation (Exit c) g1 st1
     | I.Evaluation (UnsupportedOperation c) g1 st1 => I.Evaluation (UnsupportedOperation c) g1 st1
     | I.Evaluation OutOfFuel g1 st1 => I.Evaluation OutOfFuel g1 st1
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
  Outcome F d H types t B (I.eval j P d g st e).

Lemma codesafe_le : forall n m F C t B e, m <= n -> CodeSafe n F C t B e -> CodeSafe m F C t B e.
Proof. intros n m F C t B e Le Safe j Lj; apply Safe; lia. Qed.

Lemma args_code : forall n j, j <= n ->
  forall F C (breaks : Prop) es tys d types H g st,
  Forall2 (fun e t => CodeSafe n F C t breaks e) es tys -> Pre j F C d types H g st ->
  match I.eval_arguments j P d g st es with
  | inl (ws, g', st') => exists H', heap_extends H H' /\ state_ok H' st' /\
      locals_typed W P H' g' types /\ Forall2 (fun w t => value_typed W P H' w (ground d t)) ws tys
  | inr r => Outcome F d H types TUnit breaks r /\ forall w, I.evaluation_signal r <> Normal w
  end.
Proof.
  intros n j. induction j as [|j IH]; intros Le F C breaks es tys d types H g st Args Pre0;
    [split; [exact Logic.I|cbn; discriminate]|].
  destruct Args as [|e t es tys Safe Rest]; cbn [I.eval_arguments].
  - destruct Pre0 as [_ [_ [L [S _]]]]. exists H; repeat split; auto. intros ? ? E; exact E.
  - pose proof (Safe j ltac:(lia) d types H g st (pre_lower Pre0)) as Out.
    destruct (I.eval j P d g st e) as [sig g1 st1] eqn:E.
    destruct sig; cbn in Out;
      try (split; [eapply outcome_pass; [exact (fun x => x)|cbn; congruence|exact Out]|cbn; congruence]).
    destruct Out as [H1 [E1 [S1 [L1 T1]]]].
    pose proof (IH ltac:(lia) F C breaks es tys d types H1 g1 st1 Rest
      (pre_lower (pre_after _ _ _ _ _ _ _ _ _ _ _ Pre0 E1 S1 L1))) as Next.
    destruct (I.eval_arguments j P d g1 st1 es) as [[[ws g2] st2]|r].
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
  Outcome F d H types (A.sg_ret g) B (I.eval (S n) P d lg lst (EInvoke f xs)).
Proof.
  intros n F C B f g xs d types H lg lst Found Args Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [_ [[_ [Fns _]] _]]]]]]. destruct (Fns f g Found) as [pv [Look Ok]].
  pose proof (args_code n n (le_n n) F C B xs (A.sg_params g) d types H lg lst Args (pre_lower Pre0)) as AOut.
  cbn [I.eval]. rewrite Look.
  destruct pv as [w|f' df|u sms dc|p held]; [destruct Ok| |destruct Ok|].
  - destruct Ok as [body [View Run]]. rewrite View.
    destruct (I.eval_arguments n P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      exact (outcome_ret_call F d H H1 types _ _ _ g1 E1 L1 (Run n (Nat.lt_succ_diag_r n) H1 st1 ws E1 S1 T1)).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
  - destruct (I.eval_arguments n P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      pose proof (Ok H1 st1 ws E1 S1 T1) as POut.
      destruct (I.run_prim_at P p held ws st1) as [o st2] eqn:Run'.
      exact (outcome_prim F d H H1 types _ _ o st2 g1 E1 L1 POut).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
Unshelve. all: exact TUnit.
Qed.

Lemma method_code : forall n F C (B : Prop) ot m g ox xs d types H lg lst,
  A.mlook ot m (A.c_mths C) = Some g ->
  CodeSafe n F C ot B ox ->
  Forall2 (fun e t => CodeSafe n F C t B e) xs (A.sg_params g) ->
  Pre (S n) F C d types H lg lst ->
  Outcome F d H types (A.sg_ret g) B (I.eval (S n) P d lg lst (EMethod ox m (A.rkey C ot m) xs)).
Proof.
  intros n F C B ot m g ox xs d types H lg lst Found Recv Args Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [Abs [[_ [_ [Mths _]]] _]]]]]]. destruct (Mths ot m g Found) as [pv [Look Ok]].
  pose proof (Recv n (le_n n) d types H lg lst (pre_lower Pre0)) as ROut.
  cbn [I.eval]. rewrite Look.
  destruct pv as [w|f' df|u sms dc|p held]; [destruct Ok| |destruct Ok|].
  - destruct Ok as [body [View Run]]. rewrite View.
    apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ (pre_lower Pre0) (fun b => b) ROut).
    intros H0 g0 st0 w0 E0 Pre1 T0.
    pose proof (args_code n n (le_n n) F C B xs (A.sg_params g) d types H0 g0 st0 Args Pre1) as AOut.
    destruct (I.eval_arguments n P d g0 st0 xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      apply (outcome_ret_call F d H0 H1 types _ _ _ g1 E1 L1).
      apply (Run n (Nat.lt_succ_diag_r n) H1 st1 (w0 :: ws) (extends_trans _ _ _ E0 E1) S1). constructor; [|exact T1].
      eapply value_typed_extends; eassumption.
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
  - apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ (pre_lower Pre0) (fun b => b) ROut).
    intros H0 g0 st0 w0 E0 Pre1 T0.
    pose proof (args_code n n (le_n n) F C B xs (A.sg_params g) d types H0 g0 st0 Args Pre1) as AOut.
    destruct (I.eval_arguments n P d g0 st0 xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      assert (Args' : Forall2 (fun w t => value_typed W P H1 w (ground d t)) (w0 :: ws) (ot :: A.sg_params g)).
      { constructor; [|exact T1]. eapply value_typed_extends; eassumption. }
      pose proof (Ok H1 st1 (w0 :: ws) (extends_trans _ _ _ E0 E1) S1 Args') as POut.
      destruct (I.run_prim_at P p held (w0 :: ws) st1) as [o' st2] eqn:Run'.
      exact (outcome_prim F d H0 H1 types _ _ o' st2 g1 E1 L1 POut).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
Unshelve. all: exact TUnit.
Qed.

(* Ex-derived code is safe below the induction's fuel, read at a fitting type. *)
Definition Conv (C : A.cx) (t' t : ty) : Prop :=
  A.fits t' t \/ A.fits (A.canon C t') (A.canon C t) \/ t = A.canon C t'.

Lemma conv_ground : forall C d H w t' t, Absorbs C d -> Conv C t' t ->
  value_typed W P H w (ground d t') -> value_typed W P H w (ground d t).
Proof.
  intros C d H w t' t Abs [Fits|[Fits| ->]] T.
  - exact (fits_ground W P H d w t' t Fits T).
  - rewrite <- Abs in T |- *. exact (fits_ground W P H d w _ _ Fits T).
  - rewrite Abs. exact T.
Qed.

Lemma ex_code : forall k, Below k -> forall F C a exp e t' t (B : Prop),
  A.Ex W Prog F C a exp e t' -> Conv C t' t -> (A.Breaks a -> B) -> CodeSafe k F C t B e.
Proof.
  intros k Bel F C a exp e t' t B D Cv Brk j Le d types H g st Pre0.
  destruct (Bel j Le) as [SEx _].
  pose proof (SEx F C a exp e t' D d types H g st Pre0) as Out.
  pose proof Pre0 as [_ [_ [_ [_ [Abs _]]]]].
  eapply O.outcome_breaks; [exact Brk|].
  exact (outcome_conv _ _ _ _ _ _ _ _ (fun H' w => conv_ground C d H' w t' t Abs Cv) Out).
Qed.

Lemma args_code_of : forall k, Below k -> forall F C args ts xs, A.Args W Prog F C args ts xs ->
  forall (B : Prop), (forall a, In a args -> A.Breaks a -> B) -> Forall2 (fun e t => CodeSafe k F C t B e) xs ts.
Proof.
  intros k Bel F C args ts xs D. induction D as [F C|F C a r t ts x xs at' Dx Fits Rest IH]; intros B Brk; constructor.
  - eapply ex_code; [exact Bel|exact Dx|right; left; exact Fits|]. intro Ba; exact (Brk a (or_introl eq_refl) Ba).
  - apply IH. intros a' In' Ba; exact (Brk a' (or_intror In') Ba).
Qed.

Lemma codesafe_conv : forall n F C t' t B e,
  (forall d H w, Absorbs C d -> value_typed W P H w (ground d t') -> value_typed W P H w (ground d t)) ->
  CodeSafe n F C t' B e -> CodeSafe n F C t B e.
Proof.
  intros n F C t' t B e Conv Safe j Le d types H g st Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [Abs _]]]]].
  exact (outcome_conv _ _ _ _ _ _ _ _ (fun H' w => Conv d H' w Abs) (Safe j Le d types H g st Pre0)).
Qed.

(* Arithmetic a family supplies to a literal: a functor application (T15/T16). *)
Hypothesis literal_family_case : forall n, Below n -> forall F C t m ox oy u matching th sm C' ks,
  CodeSafe n F C t False ox -> CodeSafe n F C t False oy ->
  A.mlook t m (A.c_mths C) = None -> A.FamilyApplication W C u matching th sm C' ks ->
  In (A.mkey t m) ks -> A.OnlyFamily W C u (A.mkey t m) -> A.mlook t m (A.c_mths C') = Some (A.FSig [t] t) ->
  StepSafe n F C t False (apply_expression u th sm (EMethod ox m (A.rkey C' t m) [oy])).

Lemma lit_step : forall n, Below n ->
  (forall F C tree x t, A.Literal W C tree x t -> CodeSafe n F C t False x) ->
  forall F C tree x t, A.Literal W C tree x t -> StepSafe n F C t False x.
Proof.
  intros n Bel IHc F C tree x t D d types H lg lst Pre0.
  assert (Op : forall a b x y result m,
    A.Literal W C a x t -> A.Literal W C b y t -> A.LitOp W C t m x y result ->
    Outcome F d H types t False (I.eval (S n) P d lg lst result)).
  { intros a b x' y result m Da Db Lop. destruct Lop as [Found|u matching th sm C' ks None' Fam InK Only Found].
    - refine (method_code n F C False t m (A.FSig [t] t) x' [y] d types H lg lst Found
        (IHc F C a x' t Da) _ Pre0).
      constructor; [exact (IHc F C b y t Db)|constructor].
    - exact (literal_family_case n Bel F C t m x' y u matching th sm C' ks
        (IHc F C a x' t Da) (IHc F C b y t Db) None' Fam InK Only Found d types H lg lst Pre0). }
  destruct D as [z t Lang Val|o t Lang Val|a b x y result t m Da Db Lang Lop|a b x y result t m Da Db Lang Lop
                |a b x y result t m Da Db Lang Lop|a x ti tp t f Da Lang Fn Fits].
  - pose proof Pre0 as [_ [_ [_ [_ [_ [[Vals _] _]]]]]]. destruct (Vals z t Val) as [w [Look T]].
    cbn [I.eval]. rewrite Look. eapply outcome_now; eassumption.
  - pose proof Pre0 as [_ [_ [_ [_ [_ [[Vals _] _]]]]]]. destruct (Vals o t Val) as [w [Look T]].
    cbn [I.eval]. rewrite Look. eapply outcome_now; eassumption.
  - exact (Op a b x y result m Da Db Lop).
  - exact (Op a b x y result m Da Db Lop).
  - exact (Op a b x y result m Da Db Lop).
  - refine (invoke_code n F C False f (A.FSig [tp] t) [x] d types H lg lst Fn _ Pre0).
    constructor; [|constructor].
    apply (codesafe_conv n F C ti); [|exact (IHc F C a x ti Da)].
    intros d' H' w _ T. exact (fits_ground W P H' d' w ti tp Fits T).
Qed.

Lemma lit_code : forall n, Below n -> forall F C tree x t, A.Literal W C tree x t -> CodeSafe (S n) F C t False x.
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
  A.w_decl W f = Some dcl -> A.d_ast dcl = ADFn rcv n ps rt (Some body) -> A.FnElab W Prog f.

Lemma ctxof_locals : forall f C ns, A.CtxOf W f C ns -> A.c_locals C = [].
Proof.
  intros f C ns [d [es [tm [er [tmr [σ [_ [_ [_ [_ [-> _]]]]]]]]]]]. reflexivity.
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


(* The body of a defined plain function, entered with a realized context and
   typed arguments, behaves as a call at the caller's types. *)
Lemma body_call : forall j, Safe j ->
  forall f dcl n ps rt b Cf ns de H st ws (d : environment) params ret,
  A.w_decl W f = Some dcl -> A.d_ast dcl = ADFn RPlain n ps rt (Some b) ->
  A.CtxOf W f Cf ns -> Realizes j Cf de H -> state_ok H st ->
  (forall g0, A.SigOf W f (A.c_ty Cf) None g0 ->
     map (ground de) (A.sg_params g0) = map (ground d) params /\ ground de (A.sg_ret g0) = ground d ret) ->
  Forall2 (fun w t => value_typed W P H w (ground d t)) ws params ->
  exists body, view_function P f = Some body /\
    CallOutcome H (ground d ret) (I.eval j P de (parameter_values ws) st body).
Proof.
  intros j Sj f dcl n ps rt b Cf ns de H st ws d params ret Decl Ast Ctx Real State Agree Args.
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
    split; [split|].
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
      + apply (realizes_same _ Cf); [apply add_locals_ty|apply add_locals_sub|apply add_locals_provisions|exact Real]. }
  destruct Sj as [_ [_ [SBlkE _]]].
  pose proof (SBlkE F0 Cb b (Some (A.sg_ret g0)) x t0 Body de types0 H (parameter_values ws) st Pre0) as Out.
  destruct (blke_branch_ty _ _ _ _ _ _ Body) as [_ Fits]. cbn in Fits.
  destruct (I.eval j P de (parameter_values ws) st x) as [[] g' st']; cbn in Out |- *; auto.
  - destruct Out as [H' [E [S [_ T]]]]. exists H'; repeat split; auto.
    rewrite <- Result. exact (fits_ground W P H' de _ _ _ Fits T).
  - destruct Out as [H' [r [R [E [S [_ T]]]]]]. rewrite Ret in R. injection R as <-.
    exists H'; repeat split; auto. rewrite <- Result. exact T.
  - destruct Out as [_ [L _]]. rewrite Loop in L. discriminate.
Qed.

(* ---- direct calls of defined functions -------------------------------------- *)

(* The callee's environment: its formals read at the call, and the
   provisions the satisfaction map selects. *)
Definition callee_env (d : environment) (th : subst) (dc : environment) : environment :=
  EnvironmentWithAliases (map (fun q => (fst q, ground d (snd q))) th)
    (map (fun q => (fst q, ground d (snd q))) th) (provision_values dc).

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

(* T03/T04: a call's selected provisions realize the callee's context. *)
Lemma callee_realizes : forall n C' d H f Cf ns sm th,
  Realizes (S n) C' d H -> A.CtxOf W f Cf ns -> A.Satisfies C' Cf th ns sm -> A.CallTheta W C' Cf f th ->
  exists dc, select_provisions d sm = Some dc /\
    Agree (type_meanings (callee_env d th dc)) (type_meanings d) (A.c_ty Cf) (A.c_ty C') /\
    Realizes n Cf (callee_env d th dc) H.
Proof.
  intros n C d H f Cf ns sm th [Abs [[Vals [Fns [Mths Funs]]] [Idem KM]]] Ctx Sat CT.
  destruct Sat as [SmEq [Prov [_ [Types _]]]].
  destruct (ctx_needs W f Cf ns Ctx) as [Nd [ProvNs Shape]].
  pose proof (ctx_keys_match W f Cf ns Ctx) as KMf.
  pose proof CT as [Keys [_ _]].
  (* the caller's provision of each need *)
  assert (Caller : forall k, In k ns -> exists pv, lookup_provision (A.rkey_of C (A.at_th C th k)) d = Some pv).
  { intros k Ik. pose proof (Prov k Ik) as Pr. pose proof (Shape k Ik) as Sh.
    destruct k as [t|v|f'|r m a|u]; cbn in Sh, Pr |- *; try contradiction.
    - destruct (A.nlook v (A.c_vals C)) as [t|] eqn:L; [|congruence]. destruct (Vals v t L) as [w [Look _]]. eauto.
    - destruct (A.nlook f' (A.c_fns C)) as [g|] eqn:L; [|congruence]. destruct (Fns f' g L) as [pv [Look _]]. eauto.
    - destruct (A.mlook _ m (A.c_mths C)) as [g|] eqn:L; [|congruence]. destruct (Mths _ m g L) as [pv [Look _]]. eauto.
    - destruct (A.nlook u (A.c_funs C)) as [fx|] eqn:L; [|congruence]. destruct (Funs u fx L) as [h [sms [dc Look]]]. eauto. }
  destruct (select_ok d sm) as [dc Sel].
  { rewrite SmEq. intros q Iq. apply in_map_iff in Iq as [k [<- Ik]]. cbn. destruct (Caller k Ik) as [pv ->]. discriminate. }
  exists dc. split; [exact Sel|].
  set (de := callee_env d th dc).
  assert (Meanings : type_meanings de = callee_meanings d th) by reflexivity.
  (* each callee key finds the caller's provision *)
  assert (Find : forall k, In k ns -> lookup_provision (A.rkey_of Cf k) de = lookup_provision (A.rkey_of C (A.at_th C th k)) d).
  { intros k Ik. change (lookup_provision (A.rkey_of Cf k) de) with (lookup_provision (A.rkey_of Cf k) dc).
    rewrite (select_lookup d sm dc _ Sel), SmEq.
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
  split; [exact (call_agree W f Cf ns C th d Ctx CT Abs Idem)|].
  split; [intros t; unfold ground; rewrite Meanings; exact (call_absorbs W f Cf ns C th d Ctx CT Abs t)|].
  split; [|split; [exact (PublicContextFacts.function_context_canonical W f Cf ns Ctx)|exact KMf]].
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
    destruct (ctx_provision_scoped W f Cf ns Ctx _ _ InP) as [Sc _]. unfold provision_sig_gens, A.sig_gens in Sc. cbn in Sc.
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
    destruct (ctx_provision_scoped W f Cf ns Ctx _ _ InP) as [Sc _]. unfold provision_sig_gens in Sc. cbn in Sc.
    destruct (SigGround gf gc Sc Sig) as [Ps R].
    exact (callable_transfer (S n) n H d de _ _ _ _ pv (le_S _ _ (le_n n)) Ps R Ok).
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
    destruct (ctx_provision_scoped W f Cf ns Ctx _ _ InP) as [Sc Kr]. unfold provision_sig_gens in Sc. cbn in Sc, Kr.
    destruct (SigGround gf gc Sc Sig) as [Ps R].
    apply (callable_transfer (S n) n H d de (A.canon C (app_subst th r) :: A.sg_params gc) (r :: A.sg_params gf)
      (A.sg_ret gc) (A.sg_ret gf) pv (le_S _ _ (le_n n))); [|exact R|exact Ok].
    change (ground d (A.canon C (app_subst th r)) :: map (ground d) (A.sg_params gc) =
      ground de r :: map (ground de) (A.sg_params gf)).
    rewrite Ps, (GroundDe r Kr). reflexivity.
  - (* functors *) intros u fx Found.
    pose proof (funs_ilook _ u fx KMf Found) as Il. pose proof (ilook_in _ _ _ Il) as InP.
    pose proof (ProvNs _ _ InP) as Ik.
    pose proof (Prov (IFun u) Ik) as Pr. cbn in Pr.
    destruct (A.nlook u (A.c_funs C)) as [fx'|] eqn:L; [|congruence].
    destruct (Funs u fx' L) as [h [sms [dc' Look']]]. exists h, sms, dc'.
    pose proof (Find (IFun u) Ik) as F'; cbn [A.rkey_of A.at_th] in F'; rewrite F'. exact Look'.
Qed.

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

Lemma call_core : forall j, Below j -> forall F C args f n0 ps0 rt0 b0 Cf ns sm th g xs (B : Prop),
  A.ast_of W f = Some (ADFn RPlain n0 ps0 rt0 (Some b0)) ->
  A.CtxOf W f Cf ns -> A.Satisfies C Cf th ns sm -> A.CallTheta W C Cf f th -> A.SigOf W f (A.c_ty C) None g ->
  A.Args W Prog F C args (A.sg_params g) xs -> (forall a, In a args -> A.Breaks a -> B) ->
  StepSafe j F C (A.sg_ret g) B (ECall f th sm xs).
Proof.
  intros j Bel F C args f n0 ps0 rt0 b0 Cf ns sm th g xs B Ast Ctx Sat CT Sig Args Brk d types H lg lst Pre0.
  unfold A.ast_of in Ast. destruct (A.w_decl W f) as [dcl|] eqn:Decl; [|discriminate]. injection Ast as Ast.
  pose proof (all_fn_elab f dcl RPlain n0 ps0 rt0 b0 Decl Ast dcl RPlain n0 ps0 rt0 b0 Decl Ast)
    as (C0 & ns0 & this & g0 & F0 & x & t0 & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & View).
  pose proof (args_code j j (le_n j) F C B xs (A.sg_params g) d types H lg lst
    (args_code_of j Bel F C args _ xs Args B Brk) (pre_lower Pre0)) as AOut.
  cbn [I.eval]. change (view_function P f) with (A.body_of Prog f). rewrite View.
  destruct (I.eval_arguments j P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
  2: { destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out]. }
  destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
  pose proof Pre0 as [_ [_ [_ [_ Real]]]].
  destruct (callee_realizes j C d H1 f Cf ns sm th (realizes_extends _ _ _ _ _ E1 Real) Ctx Sat CT)
    as [dc [Sel [Ag RealF]]].
  rewrite Sel, (call_environment_callee d _ sm dc Sel).
  destruct (body_call j (Bel j (le_n j)) f dcl n0 ps0 rt0 b0 Cf ns _ H1 st1 ws d (A.sg_params g) (A.sg_ret g)
    Decl Ast Ctx RealF S1) as [body [View' Run]].
  - intros g' Sig'. exact (sig_rel W _ _ f _ _ None None g' g Sig' Sig Ag (fun _ _ E _ => ltac:(discriminate E))).
  - exact T1.
  - change (view_function P f) with (A.body_of Prog f) in View'. rewrite View in View'. injection View' as <-.
    exact (outcome_ret_call F d H H1 types _ _ _ g1 E1 L1 Run).
Qed.

(* Family applications a call demands wrap it in functor applications (T15/T16). *)
Hypothesis demand_case : forall k, Below k -> forall F C wanted C' wrap t (B : Prop) x,
  A.Demand W C wanted [] C wrap C' -> CodeSafe k F C' t B x -> StepSafe k F C t B (wrap x).

Lemma call_code : forall k, Below k -> forall F C args f n0 ps0 rt0 b0 Cf ns sm th g xs (B : Prop),
  A.ast_of W f = Some (ADFn RPlain n0 ps0 rt0 (Some b0)) ->
  A.CtxOf W f Cf ns -> A.Satisfies C Cf th ns sm -> A.CallTheta W C Cf f th -> A.SigOf W f (A.c_ty C) None g ->
  A.Args W Prog F C args (A.sg_params g) xs -> (forall a, In a args -> A.Breaks a -> B) ->
  CodeSafe k F C (A.sg_ret g) B (ECall f th sm xs).
Proof.
  intros k Bel F C args f n0 ps0 rt0 b0 Cf ns sm th g xs B Ast Ctx Sat CT Sig Args Brk [|j] Le d types H lg lst Pre0;
    [exact Logic.I|].
  exact (call_core j (fun i Li => Bel i ltac:(lia)) F C args f n0 ps0 rt0 b0 Cf ns sm th g xs B
    Ast Ctx Sat CT Sig Args Brk d types H lg lst Pre0).
Qed.

(* ---- adding a provision -------------------------------------------------------- *)

Definition NewOk n C' d' H (k0 : item) (p0 : A.provision) : Prop :=
  match k0, p0 with
  | IVal v, A.ProvidedVal t _ => exists w, lookup_provision (IVal v) d' = Some (ValueProvision w) /\ value_typed W P H w (ground d' t)
  | IFn f, A.ProvidedFn g _ => exists pv, lookup_provision (IFn f) d' = Some pv /\ CallableOk n H d' (A.sg_params g) (A.sg_ret g) pv
  | IMth r m _, A.ProvidedMethod g _ => exists pv, lookup_provision (A.rkey C' r m) d' = Some pv /\ CallableOk n H d' (r :: A.sg_params g) (A.sg_ret g) pv
  | IFun u, A.ProvidedFunctor _ => exists h sms dc, lookup_provision (IFun u) d' = Some (FunctorClosure h sms dc)
  | _, _ => False
  end.

Definition set_cx (C : A.cx) (k0 : item) (p0 : A.provision) : A.cx :=
  A.Cx (A.c_ty C) (A.c_sub C) (set_list k0 p0 (A.c_provisions C)) (A.c_locals C).

Lemma ilook_map_snd : forall X (f : A.provision -> X) k l,
  A.ilook k (map (fun q => (fst q, f (snd q))) l) = option_map f (A.ilook k l).
Proof. intros X f k l. induction l as [|[k' p] r IH]; [reflexivity|]. cbn. destruct (item_eqb k k'); [reflexivity|exact IH]. Qed.

Lemma capps_ilook : forall C k, A.ilook k (A.c_apps C) = option_map A.provision_inst (A.ilook k (A.c_provisions C)).
Proof.
  intros [ty sub provs locs] k. unfold A.c_apps. cbn [A.c_provisions].
  induction provs as [|[k' p] r IH]; [reflexivity|]. cbn. destruct (item_eqb k k'); [|exact IH].
  destruct p; reflexivity.
Qed.

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
  intros n C d H k0 p0 d' [Abs [[Vals [Fns [Mths Funs]]] [Idem KM]]] K0 Meq Other New.
  assert (G : forall t, ground d' t = ground d t) by (intros t; unfold ground; rewrite Meq; reflexivity).
  assert (Gm : forall ps, map (ground d) ps = map (ground d') ps) by (intros ps; apply map_ext; intros; symmetry; apply G).
  pose proof (keys_match_set k0 p0 _ KM K0) as KM'.
  split; [intros t; rewrite !G; exact (Abs t)|]. split; [|split; [exact Idem|exact KM']].
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
    + apply item_eqb_spec in E. subst k0. destruct p0 as [t' a|g a|g a|fx']; try discriminate. exact New.
    + change (match A.ilook (IFun u) (A.c_provisions C) with Some (A.ProvidedFunctor fx) => Some fx | _ => None end = Some fx) in Found.
      rewrite <- funs_by_ilook in Found by exact KM. destruct (Funs u fx Found) as [h [sms [dc Look]]].
      exists h, sms, dc. rewrite Other; [exact Look|].
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

Definition ItemBreaks (it : spec * option aexpr) : Prop :=
  match snd it with Some y => A.Breaks y | None => False end.
Definition ItemsBreak (its : list (spec * option aexpr)) : Prop :=
  exists it, In it its /\ ItemBreaks it.

(* A bind's wrapper turns safe continuation code into safe code. *)
Definition WrapSafe n F C F' C' (Bi : Prop) (w : expr -> expr) : Prop :=
  forall t (B : Prop) x, CodeSafe n F' C' t B x -> CodeSafe n F C t (B \/ Bi) (w x).

Definition ItemWrap n F C (it : spec * option aexpr) (w : expr -> expr) (C' : A.cx) (F' : A.frame) (made : list nat) : Prop :=
  WrapSafe n F C F' C' (ItemBreaks it) w.

(* Item rules not yet proved: one hypothesis per rule, its premises verbatim. *)
Local Close Scope string_scope.
(* BFnLambda *)
Hypothesis case_BFnLambda : forall k, Below k -> forall F C pa f sg ps rt body pid pre sm a,
  A.resolve_sym W (A.fr_mod F) pa = Some f -> A.kind_sym W f = Some A.DKFn ->
      A.SigOf W f (A.c_ty C) None sg ->
      A.LambdaProvider W Prog F C ps rt body sg None pid pre sm ->
      A.AppOf W f (A.c_ty C) a ->
  forall nf, nf <= S k -> ItemWrap nf F C (Spec (Some pa) None None, Some (AELam ps rt body)) (fun x => pre (bind_function_expression f pid (A.lexical_capture C) sm x))
             (A.provide C (IFn f) a (A.set_fn C f sg)) F [].

(* BMethBind *)
Hypothesis case_BMethBind : forall k, Below k -> forall F C pa dn a y r m bs rt sg target pre sm ap types,
  A.resolve_sym W (A.fr_mod F) pa = Some r -> A.MethodNamed W (A.fr_mod F) r dn m ->
      A.Brackets W (A.fr_mod F) (A.c_ty C) m a bs ->
      A.TyOf W (A.fr_mod F) (bs ++ A.c_ty C) None (ATRef pa None) rt ->
      A.SigOf W m (bs ++ A.c_ty C) (Some rt) sg ->
      A.MethProvider W Prog F C y sg rt target types pre sm ->
      A.AppOf W m (bs ++ A.c_ty C) ap ->
  forall nf, nf <= S k -> ItemWrap nf F C (Spec (Some pa) (Some dn) a, Some y)
             (fun x => pre (bind_method_expression (A.canon C rt) m (A.theta_of C ap) target types sm x))
             (A.provide C (A.mkey (A.canon C rt) m) ap (A.set_mth C (A.canon C rt) m sg)) F [].

(* BApplyBody *)
Hypothesis case_BApplyBody : forall k, Below k -> forall F C pa u d n ps a r bodies wrap C' F' made,
  A.resolve_sym W (A.fr_mod F) pa = Some u -> A.w_decl W u = Some d ->
      A.d_ast d = ADFunctor n ps a r (Some bodies) -> ~ In u (A.fr_applying F) ->
      A.BindItems W Prog (A.with_mod F (A.d_mod d) u) C (List.concat bodies) wrap C' F' made ->
  forall nf, nf <= S k -> ItemWrap nf F C (Spec (Some pa) None None, None) wrap C'
             (A.Frame (A.fr_mod F) (A.fr_ret F) (A.fr_this F) (A.fr_loop F) (A.fr_var F) (A.fr_fresh F') (A.fr_applying F) (A.fr_pins F)) made.

(* BApplyAbstract *)
Hypothesis case_BApplyAbstract : forall k, Below k -> forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
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
         (A.nlook t (A.c_ty C) = None /\ gensof t = t) \/
         (A.nlook t (A.c_ty C) <> None /\ A.Fresh W (gensof t) /\ In (gensof t) newgens)) ->
      NoDup outs -> (forall t, In t outs <-> A.FunOutputs W u t) ->
      A.ProvideResult W u C (fun t => TAbs (gensof t)) C' ks ->
  forall nf, nf <= S k -> ItemWrap nf F C (Spec (Some pa) None None, None)
      (apply_output_expression u th (map (fun t => (t, gensof t)) outs) sm)
      C' (A.with_fresh F newgens) newgens.

(* BFunCopy *)
Hypothesis case_BFunCopy : forall k, Below k -> forall F C pa u pg h Cu nsu Ch nsh sm fx,
  A.resolve_sym W (A.fr_mod F) pa = Some u -> A.resolve_sym W (A.fr_mod F) pg = Some h ->
      A.nlook h (A.c_funs C) <> None ->
      A.FunCtx W u Cu nsu -> A.FunCtx W h Ch nsh -> incl nsh nsu ->
      
      (forall x, A.Fixed W u x -> A.nlook x (A.c_ty C) <> None) ->
      (forall x, A.Fixed W h x -> A.Fixed W u x) ->
      A.FixedAgreement W C h ->
      A.SameResult W u h ->
      sm = map (fun k => (A.rkey_of Ch k, A.rkey_of Cu k)) nsh ->
      (forall x tx, In (x, tx) fx <-> A.Fixed W u x /\ A.nlook x (A.c_ty C) = Some tx) ->
  forall nf, nf <= S k -> ItemWrap nf F C (Spec (Some pa) None None, Some (AEPath pg None)) (copy_functor_expression u h sm) (A.set_fun C u fx) F [].

(* BFunClosure *)
Hypothesis case_BFunClosure : forall k, Below k -> forall F C pa u pg gg Cu nsu Cg nsg h sm smc fx,
  A.resolve_sym W (A.fr_mod F) pa = Some u -> A.resolve_sym W (A.fr_mod F) pg = Some gg ->
      (exists n ps a r, A.ast_of W u = Some (ADFunctor n ps a r None)) ->
      (exists n ps a r b, A.ast_of W gg = Some (ADFunctor n ps a r (Some b))) ->
      A.FunCtx W u Cu nsu -> A.FunCtx W gg Cg nsg ->
      
      (forall x, A.Fixed W u x -> A.nlook x (A.c_ty C) <> None) ->
      A.SameResult W u gg ->
      A.LinkBody W Prog u gg h ->
      sm = map (fun k => (A.rkey_of Cg k, A.rkey_of Cu k)) (filter (fun k => existsb (item_eqb k) nsu) nsg) ->
      smc = map (fun k => (A.rkey_of Cg k, A.rkey_of C (A.at_site C k))) (filter (fun k => negb (existsb (item_eqb k) nsu)) nsg) ->
      (forall k k', In (k, k') smc -> A.provides C k') ->
      (forall x tx, In (x, tx) fx <-> A.Fixed W u x /\ A.nlook x (A.c_ty C) = Some tx) ->
  forall nf, nf <= S k -> ItemWrap nf F C (Spec (Some pa) None None, Some (AEPath pg None)) (bind_functor_expression u h sm smc) (A.set_fun C u fx) F [].


Local Open Scope string_scope.

Lemma wrap_breaks : forall n F C t (B B' : Prop) x, (B -> B') -> CodeSafe n F C t B x -> CodeSafe n F C t B' x.
Proof. intros n F C t B B' x Imp Safe j Le d types H g st Pre0. exact (O.outcome_breaks W P state_ok _ _ _ _ _ _ _ _ Imp (Safe j Le d types H g st Pre0)). Qed.

Lemma eval_bind_eq : forall j d g st b x,
  I.eval (S j) P d g st (EBind b x) =
  match I.eval_bindings j P d g st [b] with
  | inl (d1, g1, st1) => I.eval j P d1 g1 st1 x
  | inr r => r
  end.
Proof. reflexivity. Qed.

Lemma bindings_nil_eq : forall j d g st, I.eval_bindings (S j) P d g st [] = inl (d, g, st).
Proof. reflexivity. Qed.

Lemma bindings_type_eq : forall j d g st t tau r,
  I.eval_bindings (S j) P d g st (BType t tau :: r) = I.eval_bindings j P (bind_type d t tau) g st r.
Proof. reflexivity. Qed.

Lemma type_bind_safe : forall n F C t0 tau,
  WrapSafe n F C F (A.set_ty C t0 (A.canon C tau)) False (bind_type_expression t0 (A.canon C tau)).
Proof.
  intros n F C t0 tau t B x Safe [|[|[|i]]] Le d types H g st Pre0; [exact Logic.I| | |];
    unfold bind_type_expression; rewrite eval_bind_eq; [reflexivity|..].
  - rewrite bindings_type_eq. exact Logic.I.
  - rewrite bindings_type_eq, bindings_nil_eq.
    eapply O.outcome_breaks; [intro b; left; exact b|].
    exact (Safe (S (S i)) ltac:(lia) (bind_type d t0 (A.canon C tau)) types H g st (pre_lower Pre0)).
Qed.

Lemma bindings_value_eq : forall j d g st v e r,
  I.eval_bindings (S j) P d g st (BValue v e :: r) =
  match I.eval j P d g st e with
  | I.Evaluation (Normal w1) g1 st1 => I.eval_bindings j P (install_provision (IVal v) (ValueProvision w1) d) g1 st1 r
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

Lemma val_bind_safe : forall k, Below k -> forall n, n <= S k -> forall F C sp v declared y x xt a,
  A.Ex W Prog F C y (Some declared) x xt -> A.fits xt declared ->
  ItemWrap n F C (sp, Some y) (bind_value_expression v x) (A.provide C (IVal v) a (A.set_val C v declared)) F [].
Proof.
  intros k Bel n Le F C sp v declared y x xt a Dy Fits t B cont Safe [|j] Lj d types H g st Pre0; [exact Logic.I|].
  unfold bind_value_expression. rewrite eval_bind_eq.
  destruct j as [|j]; [exact Logic.I|]. rewrite bindings_value_eq.
  destruct (Bel j ltac:(lia)) as [SEx _].
  pose proof (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits
    (SEx F C y _ x xt Dy d types H g st (pre_lower (pre_lower Pre0)))) as Out.
  destruct (I.eval j P d g st x) as [[] g1 st1] eqn:E;
    try (lazymatch goal with |- O.Outcome _ _ _ _ _ _ _ _ _ ?r => let r' := eval cbn in r in change r with r' end;
         refine (outcome_pass F d H types declared t (A.Breaks y) _ _ (fun b => or_intror b) _ Out);
         cbn; congruence).
  destruct Out as [H1 [E1 [S1 [L1 T1]]]].
  destruct j as [|j]; [exact Logic.I|]. rewrite bindings_nil_eq.
  unfold A.set_val in Safe. rewrite (provide_set C (IVal v) (A.ProvidedVal declared []) a ltac:(intros; eauto)) in Safe.
  eapply outcome_heap; [exact E1|].
  eapply O.outcome_breaks; [intro b; left; exact b|].
  apply (outcome_env F d (install_provision (IVal v) (ValueProvision v0) d) H1 types t B _ eq_refl).
  apply (Safe (S (S j)) ltac:(lia) _ types H1 g1 st1).
  pose proof (pre_after _ _ _ _ _ _ _ _ _ _ _ Pre0 E1 S1 L1) as [Frame [Ctx [_ [_ Real]]]].
  split; [exact Frame|]. split; [exact Ctx|]. split; [exact L1|]. split; [exact S1|].
  apply (realizes_set (S (S j)) C d H1 (IVal v) (A.ProvidedVal declared a)).
  - eapply realizes_lower; [|exact Real]. lia.
  - intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
  - reflexivity.
  - intros k0 Ne. apply install_other. exact Ne.
  - exists v0. split; [cbn; rewrite Nat.eqb_refl; reflexivity|exact T1].
Qed.

Lemma bindings_function_eq : forall j d g st f f' types sm r,
  I.eval_bindings (S j) P d g st (BFunction f f' types sm :: r) =
  match select_provisions d sm with
  | Some dc => I.eval_bindings j P (install_provision (IFn f) (FunctionClosure f' (typed_capture d types dc)) d) g st r
  | None => inr (I.Evaluation (Stuck I.e_compose) g st)
  end.
Proof. reflexivity. Qed.

Lemma calloutcome_ret : forall H t g r, CallOutcome H t r -> CallOutcome H t (I.ret_call g r).
Proof. intros H t g [[] l st] Out; cbn in *; auto. Qed.

Lemma calloutcome_prim : forall H t o st g, PrimOutcome H t (o, st) -> CallOutcome H t (I.Evaluation o g st).
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
  I.eval_arguments i P d (parameter_values ws) st (map EVar (seq 0 (List.length ws))) = inl (ws, parameter_values ws, st) \/
  exists l, I.eval_arguments i P d (parameter_values ws) st (map EVar (seq 0 (List.length ws))) = inr (I.Evaluation OutOfFuel l st).
Proof.
  intros i d ws st.
  assert (Gen : forall i start rest,
    (forall q, q < List.length rest -> lookup_local (start + q) (parameter_values ws) = nth_error rest q) ->
    I.eval_arguments i P d (parameter_values ws) st (map EVar (seq start (List.length rest))) = inl (rest, parameter_values ws, st) \/
    exists l, I.eval_arguments i P d (parameter_values ws) st (map EVar (seq start (List.length rest))) = inr (I.Evaluation OutOfFuel l st)).
  { clear i. intros i. induction i as [|i IH]; intros start rest Look; [right; exists (parameter_values ws); reflexivity|].
    destruct rest as [|w rest]; [left; reflexivity|]. cbn [List.length seq map I.eval_arguments].
    destruct i as [|i]; [right; exists (parameter_values ws); reflexivity|].
    pose proof (Look 0 ltac:(cbn; lia)) as L0. rewrite Nat.add_0_r in L0. cbn [nth_error] in L0.
    cbn [I.eval]. rewrite L0.
    destruct (IH (S start) rest) as [E|[l E]].
    - intros q Lq. replace (S start + q) with (start + S q) by lia. exact (Look (S q) ltac:(cbn; lia)).
    - rewrite E. left. reflexivity.
    - rewrite E. right. eauto. }
  apply (Gen i 0 ws). intros q Lq. unfold parameter_values. exact (lookup_params_from ws 0 q Lq).
Qed.

(* A closure bound from a provider is callable below the binding's fuel. *)
Lemma provider_callable : forall k, Below k -> forall jn, jn <= k -> forall F C pa sg g types sm d H,
  A.FnProvider W Prog F C (AEPath pa None) sg None g types sm ->
  Realizes (S jn) C d H ->
  exists dc, select_provisions d sm = Some dc /\
    CallableOk jn H d (A.sg_params sg) (A.sg_ret sg) (FunctionClosure g (typed_capture d types dc)).
Proof.
  intros k Bel jn Ljn F C pa sg g types sm d H Prov Real.
  pose proof Real as [Abs [[Vals [Fns [Mths Funs]]] [Idem KM]]].
  remember (AEPath pa None) as src eqn:Src. remember (@None ty) as none eqn:NoThis.
  destruct Prov as [F C pa' g sg Cg ns sm th R K Def Ctx Sat CT SigG|F C pa' g sg w n R K Abs' Fn Len Fresh Body].
  - destruct (callee_realizes jn C d H g Cg ns sm th Real Ctx Sat CT) as [dc [Sel [Ag RealG]]].
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
      (A.sg_params sg) (A.sg_ret sg) Decl Ast Ctx RealJ S) as [body [View' Run]].
    + intros g' Sig'. destruct (sig_rel W _ _ g _ _ None None g' _ Sig' SigG Ag (fun _ _ E' _ => ltac:(discriminate E')))
        as [Ps Rt]. cbn [A.sg_params A.sg_ret] in Ps, Rt. split.
      * change (map (app_subst (type_meanings (callee_env d th dc))) (A.sg_params g') =
                map (ground d) (A.sg_params sg)). rewrite Ps, map_map. apply map_ext. intros u. apply Abs.
      * change (app_subst (type_meanings (callee_env d th dc)) (A.sg_ret g') = ground d (A.sg_ret sg)).
        rewrite Rt. apply Abs.
    + exact Args.
    + change (view_function P g) with (A.body_of Prog g) in View'. rewrite View in View'. injection View' as <-.
      exact Run.
  - (* forwarder *)
    destruct (Fns g sg Fn) as [pv [Look Ok]].
    exists (install_provision (IFn g) pv empty_environment). split; [cbn; rewrite Look; reflexivity|].
    cbn [CallableOk]. exists (EInvoke g (map EVar (seq 0 n))). split; [change (view_function P w) with (A.body_of Prog w); exact Body|].
    intros [|jj] Ljj H' st ws E S Args; [exact Logic.I|].
    assert (Len' : List.length ws = n) by (pose proof (Forall2_length Args); congruence).
    rewrite <- Len'. cbn [I.eval].
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
      destruct (I.run_prim_at P p held ws st) as [o st2]. exact (calloutcome_prim _ _ _ _ _ POut).
Qed.

Lemma fn_bind_safe : forall k, Below k -> forall n, n <= S k -> forall F C sp f g sg pg types sm a,
  A.FnProvider W Prog F C (AEPath pg None) sg None g types sm ->
  ItemWrap n F C (sp, Some (AEPath pg None)) (bind_function_expression f g types sm)
    (A.provide C (IFn f) a (A.set_fn C f sg)) F [].
Proof.
  intros k Bel n Le F C sp f g sg pg types sm a Prov t B cont Safe [|j] Lj d types0 H lg st Pre0; [exact Logic.I|].
  unfold bind_function_expression. rewrite eval_bind_eq.
  destruct j as [|j]; [exact Logic.I|]. rewrite bindings_function_eq.
  pose proof Pre0 as [Frame [Ctx [L [St Real]]]].
  destruct (provider_callable k Bel (S j) ltac:(lia) F C pg sg g types sm d H Prov
    ltac:(eapply realizes_lower; [|exact Real]; lia)) as [dc [Sel Ok]].
  rewrite Sel. destruct j as [|j]; [exact Logic.I|]. rewrite bindings_nil_eq.
  unfold A.set_fn in Safe. rewrite (provide_set C (IFn f) (A.ProvidedFn sg []) a ltac:(intros; discriminate)) in Safe.
  set (d' := install_provision (IFn f) (FunctionClosure g (typed_capture d types dc)) d).
  apply (outcome_env F d d' H types0 t _ _ eq_refl).
  eapply O.outcome_breaks; [intro b; left; exact b|].
  apply (Safe (S (S j)) ltac:(lia) d' types0 H lg st).
  split; [exact Frame|]. split; [exact Ctx|]. split; [exact L|]. split; [exact St|].
  apply (realizes_set (S (S j)) C d H (IFn f) (A.ProvidedFn sg a)).
  - eapply realizes_lower; [|exact Real]. lia.
  - intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
  - reflexivity.
  - intros k0 Ne. apply install_other. exact Ne.
  - exists (FunctionClosure g (typed_capture d types dc)). split; [cbn; rewrite Nat.eqb_refl; reflexivity|].
    exact (callable_transfer (S (S j)) (S (S j)) H d d' _ _ _ _ _ (le_n _) eq_refl eq_refl Ok).
Qed.

Lemma item_safe : forall k, Below k -> forall n F C it w C' F' m,
  A.BindItem W Prog F C it w C' F' m -> n <= S k -> WrapSafe n F C F' C' (ItemBreaks it) w.
Proof.
  intros k Bel n F C it w C' F' m D Le.
  destruct D.
  - intros t' B x Safe. apply (wrap_breaks n F C t' (B \/ False)); [tauto|]. apply type_bind_safe. exact Safe.
  - eapply val_bind_safe; eassumption.
  - eapply fn_bind_safe; eassumption.
  - eapply case_BFnLambda; eassumption.
  - eapply case_BMethBind; eassumption.
  - eapply case_BApplyBody; eassumption.
  - eapply case_BApplyAbstract; eassumption.
  - eapply case_BFunCopy; eassumption.
  - eapply case_BFunClosure; eassumption.
Qed.

Lemma items_safe : forall k, Below k -> forall n F C its w C' F' m,
  A.BindItems W Prog F C its w C' F' m -> n <= S k -> WrapSafe n F C F' C' (ItemsBreak its) w.
Proof.
  intros k Bel n F C its w C' F' m D Le. induction D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 D1 Rest IH].
  - intros t B x Safe. apply (wrap_breaks n F C t B); [tauto|exact Safe].
  - intros t B x Safe.
    pose proof (IH t B x Safe) as S2.
    pose proof (item_safe k Bel n F C it w1 C1 F1 m1 D1 Le t _ (w2 x) S2) as S1.
    apply (wrap_breaks n F C t ((B \/ ItemsBreak r) \/ ItemBreaks it) _ (w1 (w2 x))); [|exact S1].
    intros [[b|[it' [I' Bi]]]|b]; [left; exact b|right; exists it'; split; [right; exact I'|exact Bi]|].
    right. exists it. split; [left; reflexivity|exact b].
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
  Outcome F d H types TUnit False (I.eval k P d g st EUnit).
Proof.
  intros F C d types H g st Pre0.
  eapply O.outcome_breaks; [|exact (sex F C AEUnit None EUnit TUnit (A.ExUnit W Prog F C None) d types H g st Pre0)].
  exact unit_never_breaks.
Qed.



Lemma val_step : forall F C pa s t, A.nlook s (A.c_vals C) = Some t ->
  StepSafe k F C t (A.Breaks (AEPath pa None)) (EVal s).
Proof.
  intros F C pa s t Found d types H lg lst Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [_ [[Vals _] _]]]]]]. destruct (Vals s t Found) as [w [Look T]].
  cbn [I.eval]. rewrite Look. eapply outcome_now; eassumption.
Qed.

Lemma invoke_step : forall F C pa args f g xs,
  A.nlook f (A.c_fns C) = Some g -> A.Args W Prog F C args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) (A.Breaks (AECall pa None args)) (EInvoke f xs).
Proof.
  intros F C pa args f g xs Found Args d types H lg lst Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [_ [[_ [Fns _]] _]]]]]]. destruct (Fns f g Found) as [pv [Look Ok]].
  pose proof (args_safe k Bel k (le_n k) F C (A.Breaks (AECall pa None args)) xs (A.sg_params g) d types H lg lst
    (args_argsafe F C args _ xs Args _ (fun a In' B => A.BkCall pa None args In' B)) (pre_lower Pre0)) as AOut.
  cbn [I.eval]. rewrite Look.
  destruct pv as [w|f' df|u sms dc|p held]; [destruct Ok| |destruct Ok|].
  - destruct Ok as [body [View Run]]. rewrite View.
    destruct (I.eval_arguments k P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      exact (outcome_ret_call F d H H1 types _ _ _ g1 E1 L1 (Run k (Nat.lt_succ_diag_r k) H1 st1 ws E1 S1 T1)).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
  - destruct (I.eval_arguments k P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      pose proof (Ok H1 st1 ws E1 S1 T1) as POut.
      destruct (I.run_prim_at P p held ws st1) as [o st2] eqn:Run'.
      exact (outcome_prim F d H H1 types _ _ o st2 g1 E1 L1 POut).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
Unshelve. all: exact TUnit.
Qed.

Lemma provided_step : forall F C o ox ot0 m args x t (B : Prop),
  A.Ex W Prog F C o None ox ot0 -> (A.Breaks o -> B) -> (forall a, In a args -> A.Breaks a -> B) ->
  A.Provided W Prog F C ox (A.canon C ot0) m args x t -> StepSafe k F C t B x.
Proof.
  intros F C o ox ot0 m args x t B Do BrkO BrkA Prov.
  remember (A.canon C ot0) as ot eqn:Ot.
  destruct Prov as [F C ox ot m args g xs Found Args|F C ox ot m args u matching th sm C' ks g xs None' Fam InK Only Found Args].
  2: { subst ot. exact (family_method_case k Bel F C o ox ot0 m args u matching th sm C' ks g xs B Do BrkO BrkA
          None' Fam InK Only Found Args). }
  intros d types H lg lst Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [Abs [[_ [_ [Mths _]]] _]]]]]]. destruct (Mths ot m g Found) as [pv [Look Ok]].
  pose proof (O.outcome_breaks W P state_ok _ _ _ _ _ _ _ _ BrkO (sex F C o None ox ot0 Do d types H lg lst (pre_lower Pre0))) as ROut.
  cbn [I.eval]. rewrite Look.
  destruct pv as [w|f' df|u sms dc|p held]; [destruct Ok| |destruct Ok|].
  - destruct Ok as [body [View Run]]. rewrite View.
    apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ (pre_lower Pre0) (fun b => b) ROut).
    intros H0 g0 st0 w0 E0 Pre1 T0.
    pose proof (args_safe k Bel k (le_n k) F C B xs (A.sg_params g) d types H0 g0 st0
      (args_argsafe F C args _ xs Args _ BrkA) Pre1) as AOut.
    destruct (I.eval_arguments k P d g0 st0 xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      apply (outcome_ret_call F d H0 H1 types _ _ _ g1 E1 L1).
      apply (Run k (Nat.lt_succ_diag_r k) H1 st1 (w0 :: ws) (extends_trans _ _ _ E0 E1) S1). constructor; [|exact T1].
      subst ot. rewrite Abs. eapply value_typed_extends; eassumption.
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
  - apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ (pre_lower Pre0) (fun b => b) ROut).
    intros H0 g0 st0 w0 E0 Pre1 T0.
    pose proof (args_safe k Bel k (le_n k) F C B xs (A.sg_params g) d types H0 g0 st0
      (args_argsafe F C args _ xs Args _ BrkA) Pre1) as AOut.
    destruct (I.eval_arguments k P d g0 st0 xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      assert (Args' : Forall2 (fun w t => value_typed W P H1 w (ground d t)) (w0 :: ws) (ot :: A.sg_params g)).
      { constructor; [|exact T1]. subst ot. rewrite Abs. eapply value_typed_extends; eassumption. }
      pose proof (Ok H1 st1 (w0 :: ws) (extends_trans _ _ _ E0 E1) S1 Args') as POut.
      destruct (I.run_prim_at P p held (w0 :: ws) st1) as [o' st2] eqn:Run'.
      exact (outcome_prim F d H0 H1 types _ _ o' st2 g1 E1 L1 POut).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
Unshelve. all: exact TUnit.
Qed.

Lemma methcall_step : forall F C o ox ot0 pa args x t (B : Prop),
  A.Ex W Prog F C o None ox ot0 -> (A.Breaks o -> B) -> (forall a, In a args -> A.Breaks a -> B) ->
  A.MethCall W Prog F C ox (A.canon C ot0) pa args x t -> StepSafe k F C t B x.
Proof.
  intros F C o ox ot0 pa args x t B Do BrkO BrkA Meth.
  remember (A.canon C ot0) as ot eqn:Ot.
  destruct Meth as [F C ox ot pa args m x t Long Det Prov
                   |F C ox ot n args h att m x t Head Att Scope Mth Prov
                   |F C ox ot n args h att Ca ns sm th g xs rargs wrap Cd Head Att NotWin Def Rargs Ctx Dem Sat CT Sig Args
                   |F C ox ot n args h att x t Head Att NotWin Abs Prov
                   |F C ox ot n args m x t NoAtt Scope Prov]; subst ot;
    try exact (provided_step F C o ox ot0 _ args x t B Do BrkO BrkA Prov).
  exact (attached_defined_case k Bel F C o ox ot0 _ n args h att Ca ns sm th g xs rargs wrap Cd B Do BrkO BrkA
    eq_refl Head Att NotWin Def Rargs Ctx Dem Sat CT Sig Args).
Qed.

Fixpoint ex_step F C a exp x t (D : A.Ex W Prog F C a exp x t) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t (A.Breaks a) (I.eval (S k) P d g st x)
with blk_step F C ss tl exp x t made (D : A.Blk W Prog F C ss tl exp x t made) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks (ABlock ss tl)) (I.eval (S k) P d g st x)
with blke_step F C b exp x t (D : A.BlkE W Prog F C b exp x t) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks b) (I.eval (S k) P d g st x)
with branch_step F C y exp x t (D : A.Branch W Prog F C y exp x t) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t (A.Breaks y) (I.eval (S k) P d g st x)
with cond_step F C c x (D : A.Cond W Prog F C c x) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  exists b, A.LangSym W "bool" A.NSName "Bool" b /\
    Outcome F d H types (TNom b []) (A.Breaks c) (I.eval (S k) P d g st x).
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
      |F C exp pa args f Cf ns sm th sg xs wrap C' R K Def Ctx Dem Sat CT Sig Args
      |F C exp pa args f sg xs R K Abs Fn Args
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
    + (* unit *) cbn [I.eval]. eapply outcome_now; [exact Pre0|constructor].
    + (* this *) cbn [I.eval]. pose proof Pre0 as [_ [[_ Rcv] [L _]]].
      destruct (locals_lookup W P H lg types 0 _ L (Rcv t This)) as [w [Look T]].
      rewrite Look. eapply outcome_now; eassumption.
    + (* local *) cbn [I.eval]. pose proof Pre0 as [_ [[Loc _] [L _]]].
      destruct (locals_lookup W P H lg types _ _ L (Loc n t mu Found Named)) as [w [Look T]].
      rewrite Look. eapply outcome_now; eassumption.
    + (* unit tag *) cbn [I.eval]. pose proof (unit_k F C d types H lg lst (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst TUnit _ False _ _ _ Pre0 (fun f => False_ind _ f) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      apply outcome_tag; [exact S1|exact L1|].
      exact (TC.tag_check W Prog Layouts Reps argv read d H1 s [] TUnit w
        (or_intror (conj K (conj eq_refl eq_refl))) T1).
    + (* val *) exact (val_step F C pa s t Val d types H lg lst Pre0).
    + (* const *)
      destruct Init as [F0 [t' [Dx [Fits [Mod [Ret [This [Loop [Fresh [App [Pins Num]]]]]]]]]]].
      eapply (outcome_const F0); [exact Ret|exact Loop|].
      apply (O.outcome_fits W P state_ok _ _ _ _ t'); [exact Fits|].
      apply (ex_step F0 (A.Cx [] [] [] []) i (Some t) x t' Dx).
      destruct Pre0 as [_ [_ [L [S Abs]]]]. split; [|split; [|split; [exact L|split; [exact S|]]]].
      * apply (numbering_frame F0 []). rewrite This. exact Num.
      * split; [intros n' t1 mu Found; discriminate|intros t1 This'; congruence].
      * split; [intros t1; unfold A.canon; cbn; rewrite app_subst_nil; reflexivity|].
        split; [split; [intros v' t1 Found; discriminate|]|].
        { split; [intros f g Found; discriminate|]. split; [intros r m g Found; discriminate|].
          intros u fx Found; discriminate. }
        split; [intros t1; unfold A.canon; cbn; rewrite !app_subst_nil; reflexivity|intros k0 p0 []].
    + (* tag *) subst args. cbn [I.eval].
      pose proof (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits (sex F C y _ v vty Dy d types H lg lst (pre_lower Pre0))) as Out.
      apply (outcome_seq _ F C d types H lg lst pay _ (A.Breaks y) _ _ _ Pre0
        (fun B => A.BkCall pa a [y] (or_introl eq_refl) B) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      apply outcome_tag; [exact S1|exact L1|].
      exact (TC.tag_check W Prog Layouts Reps argv read d H1 s xs pay w
        (or_introl (conj K (conj Pay (forall2_keys _ _ (fun r v => A.nlook r (List.app bs (A.c_ty C)) = Some v) F2)))) T1).
    + destruct Def as [n0 [ps0 [rt0 [b0 Def]]]].
      exact (demand_case k Bel F C _ C' wrap (A.sg_ret sg) _ _ Dem
        (call_code k Bel F C' args f n0 ps0 rt0 b0 Cf ns sm th sg xs _ Def Ctx Sat CT Sig Args
          (fun a In' B0 => A.BkCall pa None args In' B0)) d types H lg lst Pre0).
    + exact (invoke_step F C pa args f sg xs Fn Args d types H lg lst Pre0).
    + refine (methcall_step F C o ox ot pa args x t _ Do _ _ Dm d types H lg lst Pre0);
        [intro B0; apply A.BkMethRecv; exact B0|intros a' In' B0; eapply A.BkMethArg; [exact In'|exact B0]].
    + refine (provided_step F C y ox ot m [] x t _ Dy _ _ Prov d types H lg lst Pre0);
        [intro B0; apply A.BkUnary; exact B0|intros a' []].
    + refine (provided_step F C l ox ot m [r] x t _ Dl _ _ Prov d types H lg lst Pre0);
        [intro B0; apply A.BkBinL; exact B0|intros a' [<-|[]] B0; apply A.BkBinR; exact B0].
    + eapply O.outcome_breaks; [|exact (lit_code k Bel F C _ x t Lit (S k) (le_n _) d types H lg lst Pre0)].
      intros [].
    + (* if-then *) cbn [I.eval].
      destruct (scond F C c cx' Dc d types H lg lst (pre_lower Pre0)) as [bsym [Lang Out]].
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => A.BkIfC b None B) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      destruct (B.truthy_typed W Prog argv read H1 bsym w Bools Layouts Lang T1) as [bit Bit].
      replace (I.truthy P w) with (Some bit) by (rewrite truthy_same; symmetry; exact Bit).
      destruct bit.
      * apply (O.outcome_fits W P state_ok _ _ _ _ bt).
        { destruct Bt as [-> | ->]; [apply fits_refl|apply fits_never]. }
        eapply O.outcome_breaks; [|exact (sblke F C b exp bx bt Db d types H1 g1 st1 (pre_lower Pre1))].
        intro B; apply A.BkIfB; exact B.
      * eapply O.outcome_breaks; [|exact (unit_k F C d types H1 g1 st1 (pre_lower Pre1))]. intros [].
    + (* if-else *) cbn [I.eval].
      destruct (scond F C c cx' Dc d types H lg lst (pre_lower Pre0)) as [bsym [Lang Out]].
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => A.BkIfC b (Some y) B) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      destruct (B.truthy_typed W Prog argv read H1 bsym w Bools Layouts Lang T1) as [bit Bit].
      replace (I.truthy P w) with (Some bit) by (rewrite truthy_same; symmetry; exact Bit).
      destruct (blke_branch_ty _ _ _ _ _ _ Db) as [FitsA FitsBt].
      destruct (join_fits _ _ _ _ _ Join FitsA FitsBt FitsE) as [Left Right].
      destruct bit.
      * apply (O.outcome_fits W P state_ok _ _ _ _ bt); [exact Left|].
        eapply O.outcome_breaks; [|exact (sblke F C b exp bx bt Db d types H1 g1 st1 (pre_lower Pre1))].
        intro B; apply A.BkIfB; exact B.
      * apply (O.outcome_fits W P state_ok _ _ _ _ et); [exact Right|].
        eapply O.outcome_breaks; [|exact (sbranch F C y exp ex' et Dy d types H1 g1 st1 (pre_lower Pre1))].
        intro B; apply A.BkIfE; exact B.
    + (* match *) cbn [I.eval].
      pose proof (sex F C sc None sx st Ds d types H lg lst (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => A.BkMatchS arms B) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      pose proof Pre1 as [Frame [Ctx [L1 [S1 Real]]]].
      assert (T1' : value_typed W P H1 w (ground d (A.canon C st))) by (rewrite (proj1 Real); exact T1).
      destruct (AR.arms_select W Prog P d H1 F C _ arms exp xarms t w Darms (ex_formed _ _ _ _ _ _ Ds) T1')
        as [pat [body [bs [eb [binds [et [Sel [InArm [Dbr [FitsT [_ [NamedB [Distinct LB]]]]]]]]]]]]].
      rewrite Sel.
      assert (Len : List.length bs = List.length (map (fun q => (A.var_of F (fst q), ground d (snd q))) binds))
        by exact (Forall2_length LB).
      apply (outcome_popn F d H1 types _ _ t _ _ (eq_sym Len)).
      apply (O.outcome_fits W P state_ok _ _ _ _ et); [exact FitsT|].
      eapply O.outcome_breaks; [|apply (sbranch F _ body exp eb et Dbr d _ H1 (bs ++ g1)%list st1)].
      * intro B. eapply A.BkMatchA; [exact InArm|exact B].
      * split; [exact Frame|]. split; [exact (ctx_locals_push_distinct F binds C d types Frame Ctx NamedB Distinct)|].
        split; [apply Forall2_app; [exact LB|exact L1]|]. split; [exact S1|].
        apply (realizes_lower (S k) k C d H1 (le_S _ _ (le_n k))) in Real.
        refine (realizes_same _ C _ _ _ _ _ _ Real); [apply add_locals_ty|apply add_locals_sub|apply add_locals_provisions].
    + (* block *)
      destruct (blke_branch_ty _ _ _ _ _ _ Db) as [_ FitsExp].
      apply (O.outcome_fits W P state_ok _ _ _ _ t).
      { destruct exp; [exact FitsExp|apply fits_refl]. }
      eapply O.outcome_breaks; [|exact (blke_step F C b exp x t Db d types H lg lst Pre0)].
      intro B; apply A.BkBlock; exact B.
    + (* return () *) cbn [I.eval].
      pose proof (unit_k F C d types H lg lst (pre_lower Pre0)) as Out.
      destruct (I.eval k P d lg lst EUnit) as [[] g1 st1]; cbn in Out |- *; auto; try contradiction;
        [|destruct Out as [[] _]].
      destruct Out as [H1 [E1 [S1 [L1 T1]]]]. exists H1, TUnit; repeat split; auto.
    + (* return e *) cbn [I.eval].
      pose proof (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits (sex F C e _ v actual De d types H lg lst (pre_lower Pre0))) as Out.
      destruct (I.eval k P d lg lst v) as [[] g1 st1]; cbn in Out |- *; auto; try contradiction.
      * destruct Out as [H1 [E1 [S1 [L1 T1]]]]. exists H1, expected; repeat split; auto.
      * destruct Out as [B Rest]. split; [apply A.BkReturn; exact B|exact Rest].
    + (* break *) cbn [I.eval]. pose proof Pre0 as [_ [_ [L [S _]]]].
      split; [apply A.BkBreak|]. split; [exact Loop|]. exists H; repeat split; auto. apply extends_refl.
    + (* record *) subst pay. cbn [I.eval].
      destruct (record_args F C pa a fs ftys xfs Fields) as [Args Names].
      pose proof (record_eval k Bel F C pa a fs ftys xfs d types H lg lst Args Names (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => B) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      apply outcome_tag; [exact S1|exact L1|].
      exact (TC.tag_check W Prog Layouts Reps argv read d H1 s xs (TRecord ftys) w
        (or_introl (conj K (conj Pay (forall2_keys _ _ (fun r v => A.nlook r (List.app bs (A.c_ty C)) = Some v) F2)))) T1).
    + (* field *) cbn [I.eval].
      pose proof (sex F C o None ox ot Do d types H lg lst (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => A.BkField n B) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      destruct Fld as [ftys [-> Look]].
      assert (Rec : exists values, I.get_field n w = A.slook n values /\
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
      intros d types H lg lst Pre0.
    + eapply O.outcome_breaks; [|exact (ex_step F C y exp x t Dy d types H lg lst Pre0)].
      intro B; apply A.BBTail; exact B.
    + cbn [I.eval]. eapply outcome_now; [exact Pre0|constructor].
    + (* let *) cbn [I.eval].
      assert (Fits0 : A.fits vt t0).
      { destruct declared as [dt|]; cbn in Decl;
          [destruct Decl as [Fd E0]; subst t0; exact Fd|subst t0; apply fits_refl]. }
      pose proof (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits0 (sex F C y _ v vt Dy d types H lg lst (pre_lower Pre0))) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block st0 rest tl (let_breaks _ _ _ _ _ Let B)) Out).
      intros H1 g1 st1 w E1 [Frame [Ctx [L1 [S1 Abs]]]] T1.
      eapply outcome_pop. eapply O.outcome_breaks; [apply block_breaks_cons|].
      apply (sblk F _ rest tl exp xr t made Drest d _ H1 _ st1).
      split; [exact Frame|]. split; [exact (ctx_locals_push F C d types n t0 mu Frame Ctx Named)|].
      split; [exact (locals_push W P H1 g1 types _ w _ L1 T1)|]. split; [exact S1|exact (realizes_same _ C _ _ _ eq_refl eq_refl eq_refl (realizes_lower (S k) k C d H1 (le_S _ _ (le_n k)) Abs))].
    + (* assign *) cbn [I.eval].
      pose proof (assign_eval k Bel F C n y tv v vt d types H lg lst Found Named Dy Fits (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ rest tl (A.SBAssign n B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest d types H1 g1 st1 (pre_lower Pre1))].
    + (* last expression *) cbn [I.eval].
      pose proof (sex F C y None v vt Dy d types H lg lst (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ [] None (A.SBExpr B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      destruct (ty_eqb vt A.never) eqn:Never.
      * apply PublicTypeFacts.ty_eqb_spec in Never. subst vt. exfalso; exact (ground_never d H1 w T1).
      * eapply O.outcome_breaks; [|exact (unit_k F C d types H1 g1 st1 (pre_lower Pre1))]. intros [].
    + (* expression *) cbn [I.eval].
      pose proof (sex F C y None v vt Dy d types H lg lst (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ rest tl (A.SBExpr B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest d types H1 g1 st1 (pre_lower Pre1))].
    + (* while *) cbn [I.eval].
      pose proof (while_safe k Bel k (le_n k) F C c cx' b bx bt d types H lg lst Dc Db (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ rest tl (A.SBWhile b B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest d types H1 g1 st1 (pre_lower Pre1))].
    + (* loop *) cbn [I.eval].
      pose proof (proj1 (loop_safe k Bel k (le_n k) F C b bx bt d types H lg lst Db (pre_lower Pre0))) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0 (fun f => False_ind _ f) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest d types H1 g1 st1 (pre_lower Pre1))].
    + (* last loop *) cbn [I.eval].
      destruct (loop_safe k Bel k (le_n k) F C b bx bt d types H lg lst Db (pre_lower Pre0)) as [Out Normal'].
      destruct (I.eval k P d lg lst (ELoop bx)) as [[] g1 st1]; cbn in Out, Normal' |- *; auto; try contradiction;
        try (destruct Out as [[] _]).
      exfalso. exact (NotBreaks (Normal' v eq_refl)).
    + (* bind *)
      assert (XR : CodeSafe (S k) F' C' t (A.BlockBreaks (ABlock rest tl)) xr).
      { intros j Le d' types' H' g' st' Pre'. destruct (Nat.eq_dec j (S k)) as [->|Ne].
        - exact (blk_step F' C' rest tl exp xr t made' Drest d' types' H' g' st' Pre').
        - destruct (Bel j ltac:(lia)) as [_ [SBj _]]. exact (SBj F' C' rest tl exp xr t made' Drest d' types' H' g' st' Pre'). }
      pose proof (items_safe k Bel (S k) F C its wrap C' F' made Ditems (le_n _) t _ xr XR (S k) (le_n _)
        d types H lg lst Pre0) as Out.
      eapply O.outcome_breaks; [|exact Out].
      intros [Rest|[[sp oy] [Iit Bi]]]; [apply block_breaks_cons; exact Rest|].
      destruct oy as [y|]; [|contradiction]. apply stmt_breaks_block. eapply A.SBBind; [exact Iit|exact Bi].
  (* ---------------- BlkE ---------------- *)
  - destruct D as [F C ss tl exp x t made Dblk Escape Fits]. exact (blk_step F C ss tl exp x t made Dblk).
  (* ---------------- Branch ---------------- *)
  - destruct D as [F C b exp x t Db|F C y exp x t NotBlock Dy]; intros d types H lg lst Pre0.
    + destruct (blke_branch_ty _ _ _ _ _ _ Db) as [Branch FitsExp].
      apply (O.outcome_fits W P state_ok _ _ _ _ t).
      { destruct exp; [exact FitsExp|exact Branch]. }
      eapply O.outcome_breaks; [|exact (blke_step F C b exp x t Db d types H lg lst Pre0)].
      intro B; apply A.BkBlock; exact B.
    + exact (ex_step F C y exp x t Dy d types H lg lst Pre0).
  (* ---------------- Cond ---------------- *)
  - destruct D as [F C c x t b Lang Dc Fits]; intros d types H lg lst Pre0.
    exists b; split; [exact Lang|].
    exact (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits (ex_step F C c _ x t Dc d types H lg lst Pre0)).
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
