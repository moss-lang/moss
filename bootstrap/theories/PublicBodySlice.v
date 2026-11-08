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
  PublicBoolTyping PublicBodySafety PublicTypeFacts PublicArms PublicTagCheck PublicContextFacts.
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
   available at fuel n is only ever invoked at a smaller fuel. *)
Definition CallableOk (n : nat) (d : environment) (params : list ty) (ret : ty) (pv : provision_value) : Prop :=
  match pv with
  | FunctionClosure f df => exists body, view_function P f = Some body /\
      forall j, j < n -> forall H st ws, state_ok H st ->
        Forall2 (fun w t => value_typed W P H w (ground d t)) ws params ->
        CallOutcome H (ground d ret) (I.eval j P df (parameter_values ws) st body)
  | PrimitiveClosure p held => forall H st ws, state_ok H st ->
        Forall2 (fun w t => value_typed W P H w (ground d t)) ws params ->
        PrimOutcome H (ground d ret) (I.run_prim_at P p held ws st)
  | _ => False
  end.

(* The static provisions, realized under their runtime keys (T31). *)
Definition EnvOk (n : nat) (C : A.cx) (d : environment) (H : heap_types) : Prop :=
  (forall v t, A.nlook v (A.c_vals C) = Some t -> exists w,
     lookup_provision (IVal v) d = Some (ValueProvision w) /\ value_typed W P H w (ground d t)) /\
  (forall f g, A.nlook f (A.c_fns C) = Some g -> exists pv,
     lookup_provision (IFn f) d = Some pv /\ CallableOk n d (A.sg_params g) (A.sg_ret g) pv) /\
  (forall r m g, A.mlook r m (A.c_mths C) = Some g -> exists pv,
     lookup_provision (A.rkey C r m) d = Some pv /\ CallableOk n d (r :: A.sg_params g) (A.sg_ret g) pv).

Definition Realizes (n : nat) (C : A.cx) (d : environment) (H : heap_types) : Prop :=
  Absorbs C d /\ EnvOk n C d H.

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

Hypothesis call_defined_case : forall k, Below k -> forall F C pa args f Cf ns sm g xs wrap C',
  A.resolve_sym W (A.fr_mod F) pa = Some f -> A.kind_sym W f = Some A.DKFn ->
  (exists n ps rt b, A.ast_of W f = Some (ADFn RPlain n ps rt (Some b))) ->
  A.CtxOf W f Cf ns ->
  A.Demand W C (map (A.at_site C) ns) [] C wrap C' ->
  A.Satisfies C' Cf ns sm -> A.SigOf W f (A.c_ty C') None g ->
  A.Args W Prog F C' args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) (A.Breaks (AECall pa None args))
    (wrap (ECall f (A.theta_at W C' f) sm xs)).
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
Hypothesis attached_defined_case : forall k, Below k -> forall F C o ox ot0 ot n args h att Ca ns sm g xs rargs wrap Cd (B : Prop),
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
  A.Satisfies Cd Ca ns sm -> A.SigOf W att (A.c_ty Cd) (Some ot) g ->
  A.Args W Prog F C args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) B (wrap (ECall att (A.theta_at W Cd att) sm (ox :: xs))).
Hypothesis bind_case : forall k, Below k -> forall F C its rest tl exp wrap C' F' xr t made made',
  A.BindItems W Prog F C its wrap C' F' made ->
  A.Blk W Prog F' C' rest tl exp xr t made' ->
  StepSafe k F C t (A.BlockBreaks (ABlock (ASBind its :: rest) tl)) (wrap xr).

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

Lemma callable_lower : forall n m d params ret pv, m <= n ->
  CallableOk n d params ret pv -> CallableOk m d params ret pv.
Proof.
  intros n m d params ret [| f df | |] Le Ok; cbn in *; auto.
  destruct Ok as [body [View Run]]. exists body; split; [exact View|].
  intros j Lt; apply Run; lia.
Qed.

Lemma realizes_lower : forall n m C d H, m <= n -> Realizes n C d H -> Realizes m C d H.
Proof.
  intros n m C d H Le [Abs [Vals [Fns Mths]]]. split; [exact Abs|]. split; [exact Vals|]. split.
  - intros f g Found. destruct (Fns f g Found) as [pv [Look Ok]].
    exists pv; split; [exact Look|exact (callable_lower n m _ _ _ _ Le Ok)].
  - intros r m' g Found. destruct (Mths r m' g Found) as [pv [Look Ok]].
    exists pv; split; [exact Look|exact (callable_lower n m _ _ _ _ Le Ok)].
Qed.

Lemma realizes_extends : forall n C d H H', heap_extends H H' -> Realizes n C d H -> Realizes n C d H'.
Proof.
  intros n C d H H' Ext [Abs [Vals Rest]]. split; [exact Abs|]. split; [|exact Rest].
  intros v t Found. destruct (Vals v t Found) as [w [Look T]].
  exists w; split; [exact Look|eapply value_typed_extends; eassumption].
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
  pose proof Pre0 as [_ [_ [_ [_ [_ [_ [Fns _]]]]]]]. destruct (Fns f g Found) as [pv [Look Ok]].
  pose proof (args_code n n (le_n n) F C B xs (A.sg_params g) d types H lg lst Args (pre_lower Pre0)) as AOut.
  cbn [I.eval]. rewrite Look.
  destruct pv as [w|f' df|u sms dc|p held]; [destruct Ok| |destruct Ok|].
  - destruct Ok as [body [View Run]]. rewrite View.
    destruct (I.eval_arguments n P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      exact (outcome_ret_call F d H H1 types _ _ _ g1 E1 L1 (Run n (Nat.lt_succ_diag_r n) H1 st1 ws S1 T1)).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
  - destruct (I.eval_arguments n P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      pose proof (Ok H1 st1 ws S1 T1) as POut.
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
  pose proof Pre0 as [_ [_ [_ [_ [Abs [_ [_ Mths]]]]]]]. destruct (Mths ot m g Found) as [pv [Look Ok]].
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
      apply (Run n (Nat.lt_succ_diag_r n) H1 st1 (w0 :: ws) S1). constructor; [|exact T1].
      eapply value_typed_extends; eassumption.
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
  - apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ (pre_lower Pre0) (fun b => b) ROut).
    intros H0 g0 st0 w0 E0 Pre1 T0.
    pose proof (args_code n n (le_n n) F C B xs (A.sg_params g) d types H0 g0 st0 Args Pre1) as AOut.
    destruct (I.eval_arguments n P d g0 st0 xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      assert (Args' : Forall2 (fun w t => value_typed W P H1 w (ground d t)) (w0 :: ws) (ot :: A.sg_params g)).
      { constructor; [|exact T1]. eapply value_typed_extends; eassumption. }
      pose proof (Ok H1 st1 (w0 :: ws) S1 Args') as POut.
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
  - pose proof Pre0 as [_ [_ [_ [_ [_ [Vals _]]]]]]. destruct (Vals z t Val) as [w [Look T]].
    cbn [I.eval]. rewrite Look. eapply outcome_now; eassumption.
  - pose proof Pre0 as [_ [_ [_ [_ [_ [Vals _]]]]]]. destruct (Vals o t Val) as [w [Look T]].
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
  pose proof Pre0 as [_ [_ [_ [_ [_ [Vals _]]]]]]. destruct (Vals s t Found) as [w [Look T]].
  cbn [I.eval]. rewrite Look. eapply outcome_now; eassumption.
Qed.

Lemma invoke_step : forall F C pa args f g xs,
  A.nlook f (A.c_fns C) = Some g -> A.Args W Prog F C args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) (A.Breaks (AECall pa None args)) (EInvoke f xs).
Proof.
  intros F C pa args f g xs Found Args d types H lg lst Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [_ [_ [Fns _]]]]]]]. destruct (Fns f g Found) as [pv [Look Ok]].
  pose proof (args_safe k Bel k (le_n k) F C (A.Breaks (AECall pa None args)) xs (A.sg_params g) d types H lg lst
    (args_argsafe F C args _ xs Args _ (fun a In' B => A.BkCall pa None args In' B)) (pre_lower Pre0)) as AOut.
  cbn [I.eval]. rewrite Look.
  destruct pv as [w|f' df|u sms dc|p held]; [destruct Ok| |destruct Ok|].
  - destruct Ok as [body [View Run]]. rewrite View.
    destruct (I.eval_arguments k P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      exact (outcome_ret_call F d H H1 types _ _ _ g1 E1 L1 (Run k (Nat.lt_succ_diag_r k) H1 st1 ws S1 T1)).
    + destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out].
  - destruct (I.eval_arguments k P d lg lst xs) as [[[ws g1] st1]|r]; cbn in AOut.
    + destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
      pose proof (Ok H1 st1 ws S1 T1) as POut.
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
  pose proof Pre0 as [_ [_ [_ [_ [Abs [_ [_ Mths]]]]]]]. destruct (Mths ot m g Found) as [pv [Look Ok]].
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
      apply (Run k (Nat.lt_succ_diag_r k) H1 st1 (w0 :: ws) S1). constructor; [|exact T1].
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
      pose proof (Ok H1 st1 (w0 :: ws) S1 Args') as POut.
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
                   |F C ox ot n args h att Ca ns sm g xs rargs wrap Cd Head Att NotWin Def Rargs Ctx Dem Sat Sig Args
                   |F C ox ot n args h att x t Head Att NotWin Abs Prov
                   |F C ox ot n args m x t NoAtt Scope Prov]; subst ot;
    try exact (provided_step F C o ox ot0 _ args x t B Do BrkO BrkA Prov).
  exact (attached_defined_case k Bel F C o ox ot0 _ n args h att Ca ns sm g xs rargs wrap Cd B Do BrkO BrkA
    eq_refl Head Att NotWin Def Rargs Ctx Dem Sat Sig Args).
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
      |F C exp pa args f Cf ns sm sg xs wrap C' R K Def Ctx Dem Sat Sig Args
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
        split; [intros v' t1 Found; discriminate|].
        split; [intros f g Found; discriminate|intros r m g Found; discriminate].
    + (* tag *) subst args. cbn [I.eval].
      pose proof (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits (sex F C y _ v vty Dy d types H lg lst (pre_lower Pre0))) as Out.
      apply (outcome_seq _ F C d types H lg lst pay _ (A.Breaks y) _ _ _ Pre0
        (fun B => A.BkCall pa a [y] (or_introl eq_refl) B) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      apply outcome_tag; [exact S1|exact L1|].
      exact (TC.tag_check W Prog Layouts Reps argv read d H1 s xs pay w
        (or_introl (conj K (conj Pay (forall2_keys _ _ (fun r v => A.nlook r (List.app bs (A.c_ty C)) = Some v) F2)))) T1).
    + exact (call_defined_case k Bel F C pa args f Cf ns sm sg xs wrap C' R K Def Ctx Dem Sat Sig Args d types H lg lst Pre0).
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
    + exact (bind_case k Bel F C its rest tl exp wrap C' F' xr t made made' Ditems Drest d types H lg lst Pre0).
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
