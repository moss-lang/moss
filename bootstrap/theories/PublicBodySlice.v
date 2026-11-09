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

Module BodySlice (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module AR := Arms Prims I0.
Module O := AR.O.
Module I := O.I.
Module B := BoolTruthy Prims I0.
Module TC := Checks Prims I0.

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
  Outcome F d H types t (A.Breaks W (A.fr_mod F) a) (I.eval k P d g st x).
Definition SafeBlk k : Prop := forall F C ss tl exp x t made, A.Blk W Prog F C ss tl exp x t made ->
  forall d types H g st, Pre k F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks W (A.fr_mod F) (ABlock ss tl)) (I.eval k P d g st x).
Definition SafeBlkE k : Prop := forall F C b exp x t, A.BlkE W Prog F C b exp x t ->
  forall d types H g st, Pre k F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks W (A.fr_mod F) b) (I.eval k P d g st x).
Definition SafeBranch k : Prop := forall F C y exp x t, A.Branch W Prog F C y exp x t ->
  forall d types H g st, Pre k F C d types H g st ->
  Outcome F d H types t (A.Breaks W (A.fr_mod F) y) (I.eval k P d g st x).
Definition SafeCond k : Prop := forall F C c x, A.Cond W Prog F C c x ->
  forall d types H g st, Pre k F C d types H g st ->
  exists b, A.LangSym W "bool" A.NSName "Bool" b /\
    Outcome F d H types (TNom b []) (A.Breaks W (A.fr_mod F) c) (I.eval k P d g st x).
Definition Safe k : Prop := SafeEx k /\ SafeBlk k /\ SafeBlkE k /\ SafeBranch k /\ SafeCond k.
Definition Below k : Prop := forall j, j <= k -> Safe j.

(* ---- obligations outside the slice -------------------------------------- *)

Definition StepSafe (k : nat) F C t (breaks : Prop) (x : expr) : Prop :=
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t breaks (I.eval (S k) P d g st x).

(* A method a family supplies at the call: a functor application (T15/T16). *)
Hypothesis family_method_case : forall k, Below k -> forall F C o ox ot0 m args u matching th sm C' ks g xs (B : Prop),
  A.Ex W Prog F C o None ox ot0 -> (A.Breaks W (A.fr_mod F) o -> B) -> (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) ->
  A.mlook (A.canon C ot0) m (A.c_mths C) = None ->
  A.FamilyApplication W C u matching th sm C' ks ->
  In (A.mkey (A.canon C ot0) m) ks -> A.OnlyFamily W C u (A.mkey (A.canon C ot0) m) ->
  A.mlook (A.canon C ot0) m (A.c_mths C') = Some g -> A.Args W Prog F C args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) B
    (apply_expression u th sm (EMethod ox m (A.rkey C' (A.canon C ot0) m) xs)).
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
  Outcome F d H types TUnit (A.Breaks W (A.fr_mod F) c) (I.eval j P d g st (EWhile cx bx)).
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
  replace (I.truthy P v) with (Some bit) by (symmetry; exact Bit). destruct bit.
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
  (forall w, I.evaluation_signal (I.eval j P d g st (ELoop bx)) = Normal w -> A.BlockBreaks W (A.fr_mod F) b).
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
    (A.fits t' t \/ A.fits (A.canon C t') (A.canon C t)) /\ (A.Breaks W (A.fr_mod F) a -> breaks).

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

Lemma record_args : forall F C pa a fs ftys xfs,
  Forall2 (fun ft xf => fst xf = fst ft /\
    exists n v vt, n = fst ft /\ In (n, v) fs /\
      A.Ex W Prog F C (match v with Some y => y | None => AEPath [n] None end) (Some (snd ft)) (snd xf) vt /\
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
  Outcome F d H types (TRecord ftys) (A.Breaks W (A.fr_mod F) (AERec pa a fs)) (I.eval k P d g st (ERecord xfs)).
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
  Outcome F d H types TUnit (A.Breaks W (A.fr_mod F) y) (I.eval k P d g st (EAssign (A.var_of F n) v)).
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
  forall (B : Prop), (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) -> Forall2 (ArgSafe F C B) xs ts.
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
  A.Ex W Prog F C a exp e t' -> Conv C t' t -> (A.Breaks W (A.fr_mod F) a -> B) -> CodeSafe k F C t B e.
Proof.
  intros k Bel F C a exp e t' t B D Cv Brk j Le d types H g st Pre0.
  destruct (Bel j Le) as [SEx _].
  pose proof (SEx F C a exp e t' D d types H g st Pre0) as Out.
  pose proof Pre0 as [_ [_ [_ [_ [Abs _]]]]].
  eapply O.outcome_breaks; [exact Brk|].
  exact (outcome_conv _ _ _ _ _ _ _ _ (fun H' w => conv_ground C d H' w t' t Abs Cv) Out).
Qed.

Lemma args_code_of : forall k, Below k -> forall F C args ts xs, A.Args W Prog F C args ts xs ->
  forall (B : Prop), (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) -> Forall2 (fun e t => CodeSafe k F C t B e) xs ts.
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

Lemma body_call_method : forall j, Safe j ->
  forall att dcl rn n ps rt b Cf ns de H st w0 ws (d : environment) recv params ret,
  A.w_decl W att = Some dcl -> A.d_ast dcl = ADFn (RAttached rn) n ps rt (Some b) ->
  A.CtxOf W att Cf ns -> Realizes j Cf de H -> state_ok H st ->
  (forall r args g0, A.Attached W r n att ->
     Forall2 (fun x xa => fst xa = x /\ A.nlook x (A.c_ty Cf) = Some (snd xa)) (A.w_reqs W r) args ->
     A.SigOf W att (A.c_ty Cf) (Some (TNom r args)) g0 ->
     ground de (TNom r args) = ground d recv /\
     map (ground de) (A.sg_params g0) = map (ground d) params /\ ground de (A.sg_ret g0) = ground d ret) ->
  value_typed W P H w0 (ground d recv) ->
  Forall2 (fun w t => value_typed W P H w (ground d t)) ws params ->
  exists body, view_function P att = Some body /\
    CallOutcome H (ground d ret) (I.eval j P de (parameter_values (w0 :: ws)) st body).
Proof.
  intros j Sj att dcl rn n ps rt b Cf ns de H st w0 ws d recv params ret Decl Ast Ctx Real State Agree Recv Args.
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
    split; [split|].
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
      + apply (realizes_same _ Cf); [apply add_locals_ty|apply add_locals_sub|apply add_locals_provisions|exact Real]. }
  destruct Sj as [_ [_ [SBlkE _]]].
  pose proof (SBlkE F0 Cb b (Some (A.sg_ret g0)) x t0 Body de types0 H (parameter_values (w0 :: ws)) st Pre0) as Out.
  destruct (blke_branch_ty _ _ _ _ _ _ Body) as [_ Fits]. cbn in Fits.
  destruct (I.eval j P de (parameter_values (w0 :: ws)) st x) as [[] g' st']; cbn in Out |- *; auto.
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
Lemma callee_realizes_le : forall n0 n C' d H f Cf ns sm th, n <= n0 ->
  Realizes n0 C' d H -> A.CtxOf W f Cf ns -> A.Satisfies C' Cf th ns sm -> A.CallTheta W C' Cf f th ->
  exists dc, select_provisions d sm = Some dc /\
    Agree (type_meanings (callee_env d th dc)) (type_meanings d) (A.c_ty Cf) (A.c_ty C') /\
    Realizes n Cf (callee_env d th dc) H.
Proof.
  intros n0 n C d H f Cf ns sm th Le [Abs [[Vals [Fns [Mths Funs]]] [Idem KM]]] Ctx Sat CT.
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
    destruct (ctx_provision_scoped W f Cf ns Ctx _ _ InP) as [Sc Kr]. unfold provision_sig_gens in Sc. cbn in Sc, Kr.
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
    destruct (Funs u fx' L) as [h [sms [dc' Look']]]. exists h, sms, dc'.
    pose proof (Find (IFun u) Ik) as F'; cbn [A.rkey_of A.at_th] in F'; rewrite F'. exact Look'.
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

Lemma call_core : forall j, Below j -> forall F C args f n0 ps0 rt0 b0 Cf ns sm th g xs (B : Prop),
  A.ast_of W f = Some (ADFn RPlain n0 ps0 rt0 (Some b0)) ->
  A.CtxOf W f Cf ns -> A.Satisfies C Cf th ns sm -> A.CallTheta W C Cf f th -> A.SigOf W f (A.c_ty C) None g ->
  A.Args W Prog F C args (A.sg_params g) xs -> (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) ->
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
(* Code inside the wrapper may rely on both the demanding context and the
   context after the families: an attached call's receiver and arguments are
   elaborated before the families it demands. With no family the wrapper is
   the identity, so the inner code runs at the same fuel. *)
Hypothesis demand_case : forall k, Below k -> forall F C wanted C' wrap t (B : Prop) x,
  A.Demand W C wanted [] C wrap C' ->
  (forall j, j <= S k -> forall d types H g st, Pre j F C' d types H g st -> Pre j F C d types H g st ->
     Outcome F d H types t B (I.eval j P d g st x)) ->
  StepSafe k F C t B (wrap x).

Lemma call_code : forall k, Below k -> forall F C args f n0 ps0 rt0 b0 Cf ns sm th g xs (B : Prop),
  A.ast_of W f = Some (ADFn RPlain n0 ps0 rt0 (Some b0)) ->
  A.CtxOf W f Cf ns -> A.Satisfies C Cf th ns sm -> A.CallTheta W C Cf f th -> A.SigOf W f (A.c_ty C) None g ->
  A.Args W Prog F C args (A.sg_params g) xs -> (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) ->
  CodeSafe (S k) F C (A.sg_ret g) B (ECall f th sm xs).
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

Definition ItemBreaks (F : A.frame) (it : spec * option aexpr) : Prop := A.ItemBreaks W (A.fr_mod F) it.
Definition ItemsBreak (F : A.frame) (its : list (spec * option aexpr)) : Prop :=
  exists it, In it its /\ ItemBreaks F it.

(* A bind's wrapper turns safe continuation code into safe code. *)
Definition WrapSafe n F C F' C' (Bi : Prop) (w : expr -> expr) : Prop :=
  forall t (B : Prop) x, CodeSafe n F' C' t B x -> CodeSafe n F C t (B \/ Bi) (w x).

Definition ItemWrap n F C (it : spec * option aexpr) (w : expr -> expr) (C' : A.cx) (F' : A.frame) (made : list nat) : Prop :=
  WrapSafe n F C F' C' (ItemBreaks F it) w.

(* Item rules not yet proved: one hypothesis per rule, its premises verbatim. *)
Local Close Scope string_scope.
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
         refine (outcome_pass F d H types declared t (A.Breaks W (A.fr_mod F) y) _ _ (fun b => or_intror (A.IBExpr sp b)) _ Out);
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
Lemma provider_callable : forall k, Below k -> forall jn, jn <= S k -> forall F C pa sg g types sm d H,
  A.FnProvider W Prog F C (AEPath pa None) sg None g types sm ->
  Realizes jn C d H ->
  exists dc, select_provisions d sm = Some dc /\
    CallableOk jn H d (A.sg_params sg) (A.sg_ret sg) (FunctionClosure g (typed_capture d types dc)).
Proof.
  intros k Bel jn Ljn F C pa sg g types sm d H Prov Real.
  pose proof Real as [Abs [[Vals [Fns [Mths Funs]]] [Idem KM]]].
  remember (AEPath pa None) as src eqn:Src. remember (@None ty) as none eqn:NoThis.
  destruct Prov as [F C pa' g sg Cg ns sm th R K Def Ctx Sat CT SigG|F C pa' g sg w n R K Abs' Fn Len Fresh Body].
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

(* ---- attached method calls ---------------------------------------------------- *)

Hypothesis World : exists b, A.WorldOf b W.

(* Receiver types are formed: a nominal's arguments are keyed by its
   requirements (T29). *)
Hypothesis ex_nominal_formed : forall F C a exp x t, A.Ex W Prog F C a exp x t ->
  forall h, A.head (A.canon C t) = Some h -> (A.kind_sym W h = Some A.DKTag \/ A.kind_sym W h = Some A.DKUnit) ->
  exists xs, A.canon C t = TNom h xs /\ map fst xs = A.w_reqs W h.

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
  intros C0 wanted used C wrap C' D. induction D as [used C|used C u matching th sm Cm wrap Cf NotUsed Step Rest IH];
    [reflexivity|]. rewrite IH. destruct Step. reflexivity.
Qed.

(* Pre does not read the type table. *)
Lemma pre_ty_free : forall n F C C' d types H g st,
  A.c_sub C' = A.c_sub C -> A.c_provisions C' = A.c_provisions C -> A.c_locals C' = A.c_locals C ->
  Pre n F C d types H g st -> Pre n F C' d types H g st.
Proof.
  intros n F [ty sub provs locs] [ty' sub' provs' locs'] d types H g st; cbn; intros -> -> -> Pre0. exact Pre0.
Qed.

Lemma receiver_agree : forall att Ca ns Cd th d' r args xs (rest : list (nat * ty)),
  A.CtxOf W att Ca ns -> A.CallTheta W Cd Ca att th -> Absorbs Cd d' ->
  NoDup (map fst xs) -> A.c_ty Cd = (rev xs ++ rest)%list ->
  Forall2 (fun x xa => fst xa = x /\ A.nlook x (A.c_ty Ca) = Some (snd xa)) (A.w_reqs W r) args ->
  map fst xs = A.w_reqs W r ->
  app_subst (callee_meanings d' th) (TNom r args) = ground d' (TNom r xs).
Proof.
  intros att Ca ns Cd th d' r args xs rest Ctx CT Abs Nd Cty Args Keys.
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
        rewrite (merged_meaning W att Ca ns Cd th d' x Ctx CT Abs Ix).
        assert (Lu : A.nlook x (A.c_ty Cd) = Some u).
        { rewrite Cty. rewrite (nlook_rev_distinct _ xs rest x Nd). apply nlook_app_in; [exact Nd|]. apply Sub. left. reflexivity. }
        cbn [app_subst]. rewrite (Bound x Ix ltac:(congruence)). cbn [app_subst]. rewrite lookup_subst_nlook', Lu. apply Abs.
      + apply IH; [exact Fr|exact Kx'|intros q Iq; apply Sub; right; exact Iq]. }
  exact (Gen _ args xs Args Keys (fun q Iq => Iq)).
Qed.

(* ---- method binds ------------------------------------------------------------- *)

(* The bound key's receiver is formed: its arguments are keyed by its
   head's requirements (T29). *)
Hypothesis bind_nominal_formed : forall F C pa a bs m rt rg rargs,
  A.Brackets W (A.fr_mod F) (A.c_ty C) m a bs ->
  A.TyOf W (A.fr_mod F) (bs ++ A.c_ty C)%list None (ATRef pa None) rt ->
  A.canon C rt = TNom rg rargs -> (A.kind_sym W rg = Some A.DKTag \/ A.kind_sym W rg = Some A.DKUnit) ->
  map fst rargs = A.w_reqs W rg.

(* Realization does not read the type table. *)
Lemma realizes_ty_free : forall n C C' d H,
  A.c_sub C' = A.c_sub C -> A.c_provisions C' = A.c_provisions C ->
  Realizes n C d H -> Realizes n C' d H.
Proof.
  intros n [ty sub provs locs] [ty' sub' provs' locs'] d H; cbn; intros -> -> Real; exact Real.
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
  pose proof Real as [Abs _].
  pose proof (realizes_ty_free _ C Cd d H Es Ep Real) as RealD.
  pose proof RealD as [AbsD _].
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
    (A.canon C rt) (A.sg_params sg) (A.sg_ret sg) Decl Ast Ctx RealJ S) as [body [View' Run]].
  - intros r' args' g0' Att' Reqs Sig0.
    pose proof (attached_unique rg r' n att Att Att') as <-.
    assert (Recv : app_subst (callee_meanings d th) (TNom rg args') = ground d (TNom rg rargs))
      by exact (receiver_agree att Ca ns Cd th d rg args' rargs (A.c_ty C) Ctx CT AbsD Nd Et Reqs Keys).
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
  I.eval_bindings (S j) P d g st (BMethod r m th f' types sm :: rest) =
  match select_provisions d sm with
  | Some dc => I.eval_bindings j P (install_provision (IMth r m th) (FunctionClosure f' (typed_capture d types dc)) d) g st rest
  | None => inr (I.Evaluation (Stuck I.e_compose) g st)
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

Lemma typed_capture_meanings : forall d C dc,
  type_meanings (typed_capture d (A.lexical_capture C) dc) = capture_meanings d (A.context_gens C).
Proof. intros d C dc. cbn. unfold capture_meanings. rewrite map_map. reflexivity. Qed.

Lemma sub_context_gens : forall C x u, In (x, u) (A.c_sub C) ->
  In x (A.context_gens C) /\ incl (A.gens u) (A.context_gens C).
Proof.
  intros C x u I. unfold A.context_gens. split.
  - apply in_or_app. left. apply in_flat_map. exists (x, u). split; [exact I|left; reflexivity].
  - intros g Ig. apply in_or_app. left. apply in_flat_map. exists (x, u). split; [exact I|right; exact Ig].
Qed.

Lemma capture_absorbs : forall C d, Absorbs C d ->
  forall t, app_subst (capture_meanings d (A.context_gens C)) (A.canon C t) = app_subst (capture_meanings d (A.context_gens C)) t.
Proof.
  intros C d Abs t. unfold A.canon. rewrite <- (app_subst_nil t) at 2.
  apply subst2_agree. intros x _. cbn [app_subst lookup_subst].
  destruct (lookup_subst x (A.c_sub C)) as [u|] eqn:L; [|reflexivity].
  destruct (sub_context_gens C x u (lookup_subst_in_pair _ _ _ L)) as [Ix Su].
  rewrite (ground_capture d _ u Su), (lookup_capture d _ x Ix).
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
  Cl = fold_left (cl_fold C) qs (A.Cx (A.c_ty C) (A.c_sub C) (A.c_provisions C) []) ->
  Realizes n0 C d1 H -> NoDup (map snd qs) ->
  (forall q, In q qs -> exists t mu w, A.slook (fst q) (A.c_locals C) = Some (t, mu) /\
     lookup_provision (IVal (snd q)) d1 = Some (ValueProvision w) /\ value_typed W P H w (ground d1 t)) ->
  (forall r m th p, In (IMth r m th, p) (A.c_provisions C) -> incl (A.gens r) (A.context_gens C)) ->
  (forall k, In k keys <-> (A.provides Cl k /\ (match k with ITy _ => False | _ => True end) /\ k = A.provision_id k)) ->
  select_provisions d1 (map (fun k => (A.rkey_of Cl k, A.rkey_of C k)) keys) = Some dc ->
  Realizes n Cl (typed_capture d1 (A.lexical_capture C) dc) H.
Proof.
  intros n0 n C qs Cl d1 H dc keys Le ClE Real Nd Caps KScoped Keys Sel.
  pose proof Real as [Abs [[Vals [Fns [Mths Funs]]] [Idem KM]]].
  set (de := typed_capture d1 (A.lexical_capture C) dc).
  set (K := A.context_gens C).
  assert (Meq : type_meanings de = capture_meanings d1 K) by apply typed_capture_meanings.
  assert (G : forall t, incl (A.gens t) K -> ground de t = ground d1 t).
  { intros t Sc. unfold ground at 1. rewrite Meq. exact (ground_capture d1 K t Sc). }
  destruct (cl_fields C qs (A.Cx (A.c_ty C) (A.c_sub C) (A.c_provisions C) [])) as [_ Sub].
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
  split; [intros t; unfold ground; rewrite Meq, Canon; exact (capture_absorbs C d1 Abs t)|].
  split; [|split; [intros t; rewrite !Canon; exact (Idem t)|exact KMl]].
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
    pose proof (KScoped r m [] _ (ilook_in _ _ _ Il)) as Kr.
    apply (callable_transfer n0 n H d1 de (r :: A.sg_params g) (r :: A.sg_params g) (A.sg_ret g) (A.sg_ret g) pv
      Le); [| |exact Ok].
    + apply map_ext_in. intros t [<-|It]; symmetry; apply G; [exact Kr|].
      intros x Ix. exact (Sc x (Sp t It x Ix)).
    + symmetry. apply G. intros x Ix. exact (Sc x (Sr x Ix)).
  - (* functors *) intros u fx Found.
    assert (Ik : In (IFun u) keys) by (apply Keys; cbn; split; [congruence|auto]).
    change (A.c_funs Cl) with (funs_of (A.c_provisions Cl)) in Found. rewrite (funs_by_ilook _ u KMl) in Found.
    rewrite Other in Found by (intros q Iq E; discriminate).
    assert (Lu : A.nlook u (A.c_funs C) = Some fx)
      by (change (A.c_funs C) with (funs_of (A.c_provisions C)); rewrite (funs_by_ilook _ u KM); exact Found).
    destruct (Funs u fx Lu) as [h [sms [dc' Look]]]. exists h, sms, dc'.
    pose proof (Find _ Ik) as F'. cbn [A.rkey_of] in F'. rewrite F'. exact Look.
Qed.

Lemma lambda_realizes : forall n C qs Cl d1 H dc keys,
  Cl = fold_left (cl_fold C) qs (A.Cx (A.c_ty C) (A.c_sub C) (A.c_provisions C) []) ->
  Realizes (S n) C d1 H -> NoDup (map snd qs) ->
  (forall q, In q qs -> exists t mu w, A.slook (fst q) (A.c_locals C) = Some (t, mu) /\
     lookup_provision (IVal (snd q)) d1 = Some (ValueProvision w) /\ value_typed W P H w (ground d1 t)) ->
  (forall r m th p, In (IMth r m th, p) (A.c_provisions C) -> incl (A.gens r) (A.context_gens C)) ->
  (forall k, In k keys <-> (A.provides Cl k /\ (match k with ITy _ => False | _ => True end) /\ k = A.provision_id k)) ->
  select_provisions d1 (map (fun k => (A.rkey_of Cl k, A.rkey_of C k)) keys) = Some dc ->
  Realizes n Cl (typed_capture d1 (A.lexical_capture C) dc) H.
Proof. intros n C qs Cl d1 H dc keys. exact (lambda_realizes_le (S n) n C qs Cl d1 H dc keys (le_S _ _ (le_n n))). Qed.

(* ---- running a lambda's body ------------------------------------------------ *)

Definition capty (C : A.cx) (n : name) : ty :=
  match A.slook n (A.c_locals C) with Some (t, _) => t | None => TUnit end.
Definition this_list (o : option ty) : list ty := match o with Some r => [r] | None => [] end.

Lemma eval_let_eq : forall j d g st x e1 e2,
  I.eval (S j) P d g st (ELet x e1 e2) =
  match I.eval j P d g st e1 with
  | I.Evaluation (Normal w1) g1 st1 => I.pop1 (I.eval j P d ((x, w1) :: g1) st1 e2)
  | r => r
  end.
Proof. reflexivity. Qed.

Lemma eval_val_eq : forall j d g st v,
  I.eval (S j) P d g st (EVal v) =
  match lookup_provision (IVal v) d with
  | Some (ValueProvision w) => I.Evaluation (Normal w) g st
  | _ => I.Evaluation (Stuck I.e_val) g st
  end.
Proof. reflexivity. Qed.

Lemma calloutcome_pop1 : forall H t r, CallOutcome H t r -> CallOutcome H t (I.pop1 r).
Proof. intros H t [[] l st] Out; cbn in *; auto. Qed.

(* The captured vals are read into the lambda's locals, then its body runs. *)
Lemma lets_call : forall Fl C de H st ret lb qs j g types,
  locals_typed W P H g types ->
  (forall q, In q qs -> exists w, lookup_provision (IVal (snd q)) de = Some (ValueProvision w) /\
     value_typed W P H w (ground de (capty C (fst q)))) ->
  (forall j' g', j' <= j ->
     locals_typed W P H g' (rev (map (fun q => (A.var_of Fl (fst q), ground de (capty C (fst q)))) qs) ++ types) ->
     CallOutcome H ret (I.eval j' P de g' st lb)) ->
  CallOutcome H ret (I.eval j P de g st (fold_right (fun q x => ELet (A.var_of Fl (fst q)) (EVal (snd q)) x) lb qs)).
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

Lemma lambda_callable : forall k, Below k -> forall jn, jn <= S k ->
  forall C (ps : list (name * option aty)) sg this body pid caps capvals Cl Fl lbody ltype d1 H dc keys,
  List.length ps = List.length (A.sg_params sg) ->
  (forall n, In n caps -> A.slook n (A.c_locals C) <> None /\ ~ In n (map fst ps)) ->
  NoDup caps -> List.length capvals = List.length caps -> NoDup capvals ->
  Cl = fold_left (cl_fold C) (combine caps capvals) (A.Cx (A.c_ty C) (A.c_sub C) (A.c_provisions C) []) ->
  A.fr_ret Fl = Some (A.sg_ret sg) -> A.fr_this Fl = this -> A.fr_loop Fl = false ->
  A.Numbering Fl this (map fst ps) -> Forall (A.Named Fl) caps ->
  A.BlkE W Prog Fl (fold_left (fun C' q => A.add_local C' (fst q) (snd q) A.LLet) (combine (map fst ps) (A.sg_params sg)) Cl)
       (ABlock [] (Some body)) (Some (A.sg_ret sg)) lbody ltype ->
  A.fits ltype (A.sg_ret sg) ->
  A.body_of Prog pid = Some (fold_right (fun q x => ELet (A.var_of Fl (fst q)) (EVal (snd q)) x) lbody (combine caps capvals)) ->
  (forall k, In k keys <-> (A.provides Cl k /\ (match k with ITy _ => False | _ => True end) /\ k = A.provision_id k)) ->
  select_provisions d1 (map (fun k => (A.rkey_of Cl k, A.rkey_of C k)) keys) = Some dc ->
  Realizes jn C d1 H ->
  (forall q, In q (combine caps capvals) -> exists w, lookup_provision (IVal (snd q)) d1 = Some (ValueProvision w) /\
     value_typed W P H w (ground d1 (capty C (fst q)))) ->
  (forall r m th p, In (IMth r m th, p) (A.c_provisions C) -> incl (A.gens r) (A.context_gens C)) ->
  (forall t, In t (this_list this ++ A.sg_ret sg :: A.sg_params sg)%list -> incl (A.gens t) (A.context_gens C)) ->
  CallableOk jn H d1 (this_list this ++ A.sg_params sg)%list (A.sg_ret sg)
    (FunctionClosure pid (typed_capture d1 (A.lexical_capture C) dc)).
Proof.
  intros k Bel jn Ljn C ps sg this body pid caps capvals Cl Fl lbody ltype d1 H dc keys
    Len CapsIn NdC LenV NdV ClE Ret FThis Loop Num NamedC Blk Fits BodyOf Keys Sel Real Caps KS SigSc.
  set (de := typed_capture d1 (A.lexical_capture C) dc).
  set (qs := combine caps capvals) in *.
  set (K := A.context_gens C).
  assert (G : forall t, incl (A.gens t) K -> ground de t = ground d1 t).
  { intros t Sc. unfold ground at 1. unfold de. rewrite typed_capture_meanings. exact (ground_capture d1 K t Sc). }
  assert (Fq : map fst qs = caps) by (apply fst_combine; congruence).
  assert (Sq : map snd qs = capvals) by (apply snd_combine; congruence).
  assert (InCaps : forall q, In q qs -> In (fst q) caps) by (intros q Iq; rewrite <- Fq; apply in_map, Iq).
  assert (Capsr : forall q, In q qs -> exists t mu w, A.slook (fst q) (A.c_locals C) = Some (t, mu) /\
     lookup_provision (IVal (snd q)) d1 = Some (ValueProvision w) /\ value_typed W P H w (ground d1 t)).
  { intros q Iq. destruct (CapsIn _ (InCaps q Iq)) as [L _].
    destruct (A.slook (fst q) (A.c_locals C)) as [[t mu]|] eqn:Lq; [|congruence].
    destruct (Caps q Iq) as [w [Look T]]. unfold capty in T. rewrite Lq in T. exists t, mu, w. split; [reflexivity|split; [exact Look|exact T]]. }
  assert (Ndq : NoDup (map snd qs)) by (rewrite Sq; exact NdV).
  pose proof (lambda_realizes_le jn jn C qs Cl d1 H dc keys (le_n jn) ClE Real Ndq Capsr KS Keys Sel) as RealCl.
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
  { split; [exact Frame|]. split; [split|].
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
    - split; [exact Lg'|]. split; [exact Sok|].
      apply (realizes_same _ Cl); [apply add_locals_ty|apply add_locals_sub|apply add_locals_provisions|].
      apply (realizes_extends _ _ _ H); [exact E|]. eapply realizes_lower; [|exact RealCl]. lia. }
  destruct (Bel j' ltac:(lia)) as [_ [_ [SBlkE _]]].
  pose proof (SBlkE Fl Cb _ _ lbody ltype Blk de _ H' g' st Pre0) as Out.
  assert (Gr : ground de (A.sg_ret sg) = ground d1 (A.sg_ret sg)).
  { apply G. apply SigSc. apply in_or_app. right. left. reflexivity. }
  destruct (I.eval j' P de g' st lbody) as [[] g2 st2]; cbn in Out |- *; auto.
  - destruct Out as [H2 [E2 [S2 [_ T]]]]. exists H2; repeat split; auto.
    rewrite <- Gr. exact (fits_ground W P H2 de _ _ _ Fits T).
  - destruct Out as [H2 [r [R [E2 [S2 [_ T]]]]]]. rewrite Ret in R. injection R as <-.
    exists H2; repeat split; auto. rewrite <- Gr. exact T.
  - destruct Out as [_ [L _]]. rewrite Loop in L. discriminate.
Qed.

(* ---- the captured-val prefix and the lambda bind ------------------------------ *)

Lemma tm_context_gens : forall C x, In x (tm_gens (A.c_ty C)) -> In x (A.context_gens C).
Proof.
  intros C x Ix. unfold tm_gens in Ix. apply in_flat_map in Ix as [q [Iq Ix]].
  unfold A.context_gens. apply in_or_app. right. apply in_or_app. left.
  apply in_flat_map. exists q. split; [exact Iq|apply in_or_app; left; exact Ix].
Qed.

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
  intros n C d H v pv [Abs [[Vals [Fns [Mths Funs]]] [Idem KM]]] Out.
  split; [exact Abs|]. split; [|exact (conj Idem KM)]. split; [|split; [|split]].
  - intros v' t Found. destruct (Vals v' t Found) as [w [Look T]]. exists w. split; [|exact T].
    rewrite install_other; [exact Look|]. intros E. injection E as ->.
    change (A.c_vals C) with (vals_of (A.c_provisions C)) in Found. rewrite (vals_by_ilook _ v KM), Out in Found. discriminate.
  - intros f g Found. destruct (Fns f g Found) as [pv' [Look Ok]]. exists pv'.
    split; [rewrite install_other; [exact Look|discriminate]|exact Ok].
  - intros r m g Found. destruct (Mths r m g Found) as [pv' [Look Ok]]. exists pv'.
    split; [rewrite install_other; [exact Look|unfold A.rkey; discriminate]|exact Ok].
  - intros u fx Found. destruct (Funs u fx Found) as [h [sms [dc Look]]]. exists h, sms, dc.
    rewrite install_other; [exact Look|discriminate].
Qed.

Definition Installed (C : A.cx) (qs : list (name * vsym)) (d : environment) (H : heap_types) : Prop :=
  forall q, In q qs -> exists w, lookup_provision (IVal (snd q)) d = Some (ValueProvision w) /\
    value_typed W P H w (ground d (capty C (fst q))).

Lemma eval_var_eq : forall j d g st x,
  I.eval (S j) P d g st (EVar x) = match lookup_local x g with
    | Some w => I.Evaluation (Normal w) g st | None => I.Evaluation (Stuck I.e_var) g st end.
Proof. reflexivity. Qed.

(* The prefix binds each captured local's value to its val. *)
Lemma prefix_run : forall F C rest acc j d types H g st t (B : Prop) x,
  (forall q, In q (acc ++ rest)%list -> A.Named F (fst q) /\ A.slook (fst q) (A.c_locals C) <> None /\
     A.ilook (IVal (snd q)) (A.c_provisions C) = None) ->
  NoDup (map snd (acc ++ rest)%list) ->
  Pre j F C d types H g st -> Installed C acc d H ->
  (forall j' d', j' <= j -> type_meanings d' = type_meanings d -> Pre j' F C d' types H g st ->
     Installed C (acc ++ rest)%list d' H -> Outcome F d H types t B (I.eval j' P d' g st x)) ->
  Outcome F d H types t B (I.eval j P d g st
    (fold_right (fun q x => bind_value_expression (snd q) (EVar (A.var_of F (fst q))) x) x rest)).
Proof.
  intros F C rest. induction rest as [|q rest IH]; intros acc j d types H g st t B x Ok Nd Pre0 Inst Cont.
  - rewrite app_nil_r in Cont. exact (Cont j d (le_n j) eq_refl Pre0 Inst).
  - destruct j as [|j]; [exact Logic.I|]. cbn [fold_right]. unfold bind_value_expression at 1. rewrite eval_bind_eq.
    destruct j as [|j]; [exact Logic.I|]. rewrite bindings_value_eq.
    destruct j as [|j]; [exact Logic.I|]. rewrite eval_var_eq.
    destruct (Ok q ltac:(apply in_or_app; right; left; reflexivity)) as [Named [Loc Out]].
    destruct (A.slook (fst q) (A.c_locals C)) as [[t0 mu]|] eqn:L; [|congruence].
    pose proof Pre0 as [Frame [[Locals _] [Lt [St Real]]]].
    destruct (locals_lookup W P H g types _ _ Lt (Locals _ t0 mu L Named)) as [w [Look T]].
    rewrite Look. rewrite bindings_nil_eq.
    set (d' := install_provision (IVal (snd q)) (ValueProvision w) d).
    apply (outcome_env F d d' H types t B _ eq_refl).
    apply (IH (acc ++ [q])%list (S (S j)) d' types H g st t B x).
    + rewrite <- app_assoc. exact Ok.
    + rewrite <- app_assoc. exact Nd.
    + split; [exact Frame|]. split; [exact (proj1 (proj2 Pre0))|]. split; [exact Lt|]. split; [exact St|].
      apply realizes_install_val; [eapply realizes_lower; [|exact Real]; lia|exact Out].
    + intros q' Iq'. apply in_app_or in Iq' as [Iq'|[<-|[]]].
      * destruct (Inst q' Iq') as [w' [Look' T']]. exists w'. split; [|exact T'].
        unfold d'. rewrite install_other; [exact Look'|]. intros E. injection E as E.
        rewrite map_app in Nd. cbn in Nd. apply NoDup_remove_2 in Nd. apply Nd. apply in_or_app. left.
        rewrite <- E. apply in_map. exact Iq'.
      * exists w. split; [unfold d'; rewrite lookup_install, (proj2 (item_eqb_spec _ _) eq_refl); reflexivity|].
        unfold capty. rewrite L. exact T.
    + intros j' d'' Lj' Meq Pre' Inst'. rewrite <- app_assoc in Inst'. cbn in Inst'.
      apply (outcome_env F d' d H types t B _ eq_refl). exact (Cont j' d'' ltac:(lia) Meq Pre' Inst').
Qed.

(* Every key the lambda needs is found where it is bound. *)
Lemma lambda_select : forall n C (qs : list (name * vsym)) Cl d' H keys,
  Cl = fold_left (cl_fold C) qs (A.Cx (A.c_ty C) (A.c_sub C) (A.c_provisions C) []) ->
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
    destruct (Funs u fx Lu) as [h [sms [dc Look]]]. congruence.
Qed.

(* The method keys a lambda's context lists have receivers over its
   generations (T29). *)
Hypothesis lambda_keys_scoped : forall F C ps rt body sg this pid pre sm,
  A.LambdaProvider W Prog F C ps rt body sg this pid pre sm ->
  forall r m th p, In (IMth r m th, p) (A.c_provisions C) -> incl (A.gens r) (A.context_gens C).

Lemma lambda_fn_bind_safe : forall k, Below k -> forall n, n <= S k -> forall F C sp f sg ps rt body pid pre sm a,
  A.SigOf W f (A.c_ty C) None sg ->
  A.LambdaProvider W Prog F C ps rt body sg None pid pre sm ->
  ItemWrap n F C (sp, Some (AELam ps rt body)) (fun x => pre (bind_function_expression f pid (A.lexical_capture C) sm x))
    (A.provide C (IFn f) a (A.set_fn C f sg)) F [].
Proof.
  intros k Bel n Le F C sp f sg ps rt body pid pre sm a Sig LP.
  pose proof (lambda_keys_scoped _ _ _ _ _ _ _ _ _ _ LP) as KS.
  remember (@None ty) as none eqn:NoneE in LP.
  destruct LP as [F C ps rt body sg this pid caps capvals Cl Fl lbody ltype keys sm Len NdP Ann RtAnn CapsIff NdC LenV
    FreshV NdV Unprov FreshP ClE FMod FRet FThis FLoop FScope Num NamedFl NamedF Blk Fits BodyOf KeysIff NdK SmE].
  subst sm this.
  assert (ClE' : Cl = fold_left (cl_fold C) (combine caps capvals) (A.Cx (A.c_ty C) (A.c_sub C) (A.c_provisions C) []))
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
  intros t B cont Safe J LJ d types H g st Pre0. cbn beta.
  apply (prefix_run F C qs [] J d types H g st t _ _).
  - intros q Iq. split; [exact (NamedF _ (InCaps q Iq))|]. split; [exact (proj1 (CapsIn _ (InCaps q Iq)))|].
    exact (Unprov _ (InVals q Iq)).
  - exact Ndq.
  - exact Pre0.
  - intros q [].
  - intros j' d' Lj' Meq Pre' Inst'. cbn [app] in Inst'.
    destruct j' as [|j']; [exact Logic.I|]. unfold bind_function_expression. rewrite eval_bind_eq.
    destruct j' as [|j']; [exact Logic.I|]. rewrite bindings_function_eq.
    pose proof Pre' as [Frame [Ctx [L [St Real]]]].
    destruct (lambda_select _ C qs Cl d' H keys ClE' Real Inst' Ndq
      (fun q Iq => proj1 (CapsIn _ (InCaps q Iq))) KeysIff) as [dc Sel].
    rewrite Sel. destruct j' as [|j']; [exact Logic.I|]. rewrite bindings_nil_eq.
    pose proof (lambda_callable k Bel (S (S j')) ltac:(lia) C ps sg None body pid caps capvals Cl Fl lbody ltype d' H dc keys
      Len CapsIn NdC LenV NdV ClE' FRet FThis FLoop Num NamedFl Blk Fits BodyOf KeysIff Sel
      ltac:(eapply realizes_lower; [|exact Real]; lia) Inst' KS (sigof_scoped f C sg Sig)) as Ok.
    cbn [this_list app] in Ok.
    unfold A.set_fn in Safe. rewrite (provide_set C (IFn f) (A.ProvidedFn sg []) a ltac:(intros; discriminate)) in Safe.
    set (d'' := install_provision (IFn f) (FunctionClosure pid (typed_capture d' (A.lexical_capture C) dc)) d').
    apply (outcome_env F d d'' H types t _ _ Meq).
    eapply O.outcome_breaks; [intro b; left; exact b|].
    apply (Safe (S (S j')) ltac:(lia) d'' types H g st).
    split; [exact Frame|]. split; [exact Ctx|]. split; [exact L|]. split; [exact St|].
    apply (realizes_set (S (S j')) C d' H (IFn f) (A.ProvidedFn sg a)).
    + eapply realizes_lower; [|exact Real]. lia.
    + intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
    + reflexivity.
    + intros k0 Ne. apply install_other. exact Ne.
    + exists (FunctionClosure pid (typed_capture d' (A.lexical_capture C) dc)).
      split; [cbn; rewrite Nat.eqb_refl; reflexivity|].
      exact (callable_transfer (S (S j')) (S (S j')) H d' d'' _ _ _ _ _ (le_n _) eq_refl eq_refl Ok).
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

Lemma lambda_meth_bind_safe : forall k, Below k -> forall n, n <= S k -> forall F C sp m sg rt ps r body pid pre sm ap,
  A.LambdaProvider W Prog F C ps r body sg (Some rt) pid pre sm ->
  (forall t, In t (rt :: A.sg_ret sg :: A.sg_params sg) -> incl (A.gens t) (A.context_gens C)) ->
  ItemWrap n F C (sp, Some (AELam ps r body))
    (fun x => pre (bind_method_expression (A.canon C rt) m (A.theta_of C ap) pid (A.lexical_capture C) sm x))
    (A.provide C (A.mkey (A.canon C rt) m) ap (A.set_mth C (A.canon C rt) m sg)) F [].
Proof.
  intros k Bel n Le F C sp m sg rt ps r body pid pre sm ap LP SigSc.
  pose proof (lambda_keys_scoped _ _ _ _ _ _ _ _ _ _ LP) as KS.
  remember (Some rt) as recv eqn:RecvE in LP.
  destruct LP as [F C ps r body sg this pid caps capvals Cl Fl lbody ltype keys sm Len NdP Ann RtAnn CapsIff NdC LenV
    FreshV NdV Unprov FreshP ClE FMod FRet FThis FLoop FScope Num NamedFl NamedF Blk Fits BodyOf KeysIff NdK SmE].
  subst sm this.
  assert (ClE' : Cl = fold_left (cl_fold C) (combine caps capvals) (A.Cx (A.c_ty C) (A.c_sub C) (A.c_provisions C) []))
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
  intros t B cont Safe J LJ d types H g st Pre0. cbn beta.
  apply (prefix_run F C qs [] J d types H g st t _ _).
  - intros q Iq. split; [exact (NamedF _ (InCaps q Iq))|]. split; [exact (proj1 (CapsIn _ (InCaps q Iq)))|].
    exact (Unprov _ (InVals q Iq)).
  - exact Ndq.
  - exact Pre0.
  - intros q [].
  - intros j' d' Lj' Meq Pre' Inst'. cbn [app] in Inst'.
    destruct j' as [|j']; [exact Logic.I|]. unfold bind_method_expression. rewrite eval_bind_eq.
    destruct j' as [|j']; [exact Logic.I|]. rewrite bindings_method_eq.
    pose proof Pre' as [Frame [Ctx [L [St Real]]]].
    pose proof Real as [Abs _].
    destruct (lambda_select _ C qs Cl d' H keys ClE' Real Inst' Ndq
      (fun q Iq => proj1 (CapsIn _ (InCaps q Iq))) KeysIff) as [dc Sel].
    rewrite Sel. destruct j' as [|j']; [exact Logic.I|]. rewrite bindings_nil_eq.
    pose proof (lambda_callable k Bel (S (S j')) ltac:(lia) C ps sg (Some rt) body pid caps capvals Cl Fl lbody ltype d' H dc keys
      Len CapsIn NdC LenV NdV ClE' FRet FThis FLoop Num NamedFl Blk Fits BodyOf KeysIff Sel
      ltac:(eapply realizes_lower; [|exact Real]; lia) Inst' KS SigSc) as Ok.
    cbn [this_list app] in Ok.
    unfold A.set_mth in Safe.
    rewrite (provide_set C (A.mkey (A.canon C rt) m) (A.ProvidedMethod sg []) ap ltac:(intros; discriminate)) in Safe.
    assert (Key : A.rkey (set_cx C (A.mkey (A.canon C rt) m) (A.ProvidedMethod sg ap)) (A.canon C rt) m =
                  IMth (A.canon C rt) m (A.theta_of C ap)).
    { unfold A.rkey. rewrite capps_ilook. unfold set_cx. cbn [A.c_provisions].
      rewrite ilook_set, (proj2 (item_eqb_spec _ _) eq_refl). cbn [option_map A.provision_inst].
      apply f_equal, theta_of_sub. reflexivity. }
    set (d'' := install_provision (IMth (A.canon C rt) m (A.theta_of C ap))
      (FunctionClosure pid (typed_capture d' (A.lexical_capture C) dc)) d').
    apply (outcome_env F d d'' H types t _ _ Meq).
    eapply O.outcome_breaks; [intro b; left; exact b|].
    apply (Safe (S (S j')) ltac:(lia) d'' types H g st).
    split; [exact Frame|]. split; [exact Ctx|]. split; [exact L|]. split; [exact St|].
    apply (realizes_set (S (S j')) C d' H (A.mkey (A.canon C rt) m) (A.ProvidedMethod sg ap)).
    + eapply realizes_lower; [|exact Real]. lia.
    + intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
    + reflexivity.
    + intros k0 Ne. apply install_other. intro E. apply Ne. rewrite E. unfold A.mkey. cbn [A.rkey_of].
      symmetry. exact Key.
    + cbn [NewOk]. exists (FunctionClosure pid (typed_capture d' (A.lexical_capture C) dc)).
      split; [transitivity (lookup_provision (IMth (A.canon C rt) m (A.theta_of C ap)) d''); [f_equal; exact Key|]|].
      { unfold d''. rewrite lookup_install, (proj2 (item_eqb_spec _ _) eq_refl). reflexivity. }
      apply (callable_transfer (S (S j')) (S (S j')) H d' d'' (rt :: A.sg_params sg) (A.canon C rt :: A.sg_params sg) (A.sg_ret sg) (A.sg_ret sg) _ (le_n _)); [|reflexivity|exact Ok].
      cbn [map]. f_equal. symmetry. exact (Abs rt).
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

Lemma lookup_map_snd : forall (f : ty -> ty) (l : list (nat * ty)) x,
  lookup_subst x (map (fun q => (fst q, f (snd q))) l) = option_map f (lookup_subst x l).
Proof. intros f l x. induction l as [|[a t] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb x a); [reflexivity|exact IH]. Qed.

(* Calling a symbol from inside its own context, at its own reading of
   each type, leaves every meaning as it was. *)
Lemma self_meanings : forall f Cg ns de, A.CtxOf W f Cg ns -> Absorbs Cg de ->
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
  intros n C d d' H [Abs [[Vals [Fns [Mths Funs]]] [Idem KM]]] G Same.
  assert (Gm : forall ps, map (ground d) ps = map (ground d') ps) by (intros ps; apply map_ext; intros; symmetry; apply G).
  split; [intros t; rewrite !G; exact (Abs t)|]. split; [|exact (conj Idem KM)]. split; [|split; [|split]].
  - intros v t Found. destruct (Vals v t Found) as [w [Look T]]. exists w. rewrite G. split; [|exact T].
    destruct (vals_ilook _ v t KM Found) as [a Il]. rewrite <- Look. exact (Same _ _ (ilook_in _ _ _ Il)).
  - intros f g Found. destruct (Fns f g Found) as [pv [Look Ok]]. exists pv. split.
    + destruct (fns_ilook _ f g KM Found) as [a Il]. rewrite <- Look. exact (Same _ _ (ilook_in _ _ _ Il)).
    + exact (callable_transfer n n H d d' _ _ _ _ pv (le_n n) (Gm _) (eq_sym (G _)) Ok).
  - intros r m g Found. destruct (Mths r m g Found) as [pv [Look Ok]]. exists pv. split.
    + destruct (mths_ilook _ r m g KM Found) as [a Il]. rewrite <- Look. exact (Same _ _ (ilook_in _ _ _ Il)).
    + exact (callable_transfer n n H d d' _ _ _ _ pv (le_n n) (Gm _) (eq_sym (G _)) Ok).
  - intros u fx Found. destruct (Funs u fx Found) as [h [sms [dc Look]]]. exists h, sms, dc.
    pose proof (funs_ilook _ u fx KM Found) as Il. rewrite <- Look. exact (Same _ _ (ilook_in _ _ _ Il)).
Qed.

(* Each need of a callee is found in the environment a call builds. *)
Lemma callee_found : forall n C d H f Cf ns sm th dc,
  Realizes n C d H -> A.CtxOf W f Cf ns -> A.Satisfies C Cf th ns sm -> A.CallTheta W C Cf f th ->
  select_provisions d sm = Some dc ->
  forall k, In k ns -> lookup_provision (A.rkey_of Cf k) (callee_env d th dc) <> None.
Proof.
  intros n C d H f Cf ns sm th dc [Abs [[Vals [Fns [Mths Funs]]] [Idem KM]]] Ctx Sat CT Sel k Ik.
  destruct Sat as [SmEq [Prov _]].
  destruct (ctx_needs W f Cf ns Ctx) as [Nd [ProvNs Shape]].
  change (lookup_provision (A.rkey_of Cf k) (callee_env d th dc)) with (lookup_provision (A.rkey_of Cf k) dc).
  rewrite (select_lookup d sm dc _ Sel), SmEq.
  rewrite (sm_find_map (A.rkey_of Cf) (fun k' => A.rkey_of C (A.at_th C th k')) ns k Ik);
    [|intros k' Ik' E; exact (rkey_of_injective Cf k' k (Shape k' Ik') (Shape k Ik) E)].
  pose proof (Prov k Ik) as Pr. pose proof (Shape k Ik) as Sh.
  destruct k as [t|v|f'|r m a|u]; cbn in Sh, Pr |- *; try contradiction.
  - destruct (A.nlook v (A.c_vals C)) as [t|] eqn:L; [|congruence]. destruct (Vals v t L) as [w [Look _]]. congruence.
  - destruct (A.nlook f' (A.c_fns C)) as [g|] eqn:L; [|congruence]. destruct (Fns f' g L) as [pv [Look _]]. congruence.
  - destruct (A.mlook _ m (A.c_mths C)) as [g|] eqn:L; [|congruence]. destruct (Mths _ m g L) as [pv [Look _]]. congruence.
  - destruct (A.nlook u (A.c_funs C)) as [fx|] eqn:L; [|congruence]. destruct (Funs u fx L) as [h [sms [dc' Look]]]. congruence.
Qed.

Lemma vars_args_from : forall i d g st start rest,
  (forall q, q < List.length rest -> lookup_local (start + q) g = nth_error rest q) ->
  I.eval_arguments i P d g st (map EVar (seq start (List.length rest))) = inl (rest, g, st) \/
  exists l, I.eval_arguments i P d g st (map EVar (seq start (List.length rest))) = inr (I.Evaluation OutOfFuel l st).
Proof.
  intros i d g st. induction i as [|i IH]; intros start rest Look; [right; exists g; reflexivity|].
  destruct rest as [|w rest]; [left; reflexivity|]. cbn [List.length seq map I.eval_arguments].
  destruct i as [|i]; [right; exists g; reflexivity|].
  pose proof (Look 0 ltac:(cbn; lia)) as L0. rewrite Nat.add_0_r in L0. cbn [nth_error] in L0.
  cbn [I.eval]. rewrite L0.
  destruct (IH (S start) rest) as [E|[l E]].
  - intros q Lq. replace (S start + q) with (start + S q) by lia. exact (Look (S q) ltac:(cbn; lia)).
  - rewrite E. left. reflexivity.
  - rewrite E. right. eauto.
Qed.

Lemma eval_call_eq : forall j d g st f th sm args,
  I.eval (S j) P d g st (ECall f th sm args) =
  match view_function P f with
  | Some body =>
      match I.eval_arguments j P d g st args with
      | inl (ws, g1, st1) =>
          match select_provisions d sm with
          | Some d' => I.ret_call g1 (I.eval j P (call_environment d th d') (parameter_values ws) st1 body)
          | None => I.Evaluation (Stuck I.e_compose) g1 st1
          end
      | inr r => r
      end
  | None => I.Evaluation (Stuck I.e_nobody) g st
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
  pose proof Real as [Abs _].
  destruct (callee_realizes_le jn jn C d H g Cg ns sm th (le_n jn) Real Ctx Sat CT) as [dc0 [Sel0 [Ag RealG]]].
  rewrite Sel in Sel0. injection Sel0 as <-.
  set (de := typed_capture d (TypeCapture th th) dc).
  assert (RealDe : Realizes jn Cg de H) by exact RealG.
  pose proof RealDe as [AbsDe _].
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
    Decl Ast Ctx RealN Sok) as [body [View' Run]].
  - intros g' Sig'. destruct (sig_rel W _ _ g _ _ None None g' _ Sig' Sig Ag (fun _ _ E' _ => ltac:(discriminate E')))
      as [Ps Rt]. cbn [A.sg_params A.sg_ret] in Ps, Rt. split.
    + transitivity (map (ground de) (A.sg_params g')); [apply map_ext; exact Gn|].
      etransitivity; [exact Ps|]. rewrite map_map. apply map_ext. intros u. apply Abs.
    + rewrite Gn. etransitivity; [exact Rt|]. apply Abs.
  - exact Tws.
  - change (view_function P g) with (A.body_of Prog g) in View'. rewrite View in View'. injection View' as <-.
    apply calloutcome_ret. exact Run.
Qed.

Lemma wrapped_meth_bind_safe : forall k, Below k -> forall n, n <= S k ->
  forall F C sp pa m sg rt g types sm w n0 Cg nsg ap,
  A.FnProvider W Prog F C (AEPath pa None) sg None g types sm ->
  A.CtxOf W g Cg nsg -> n0 = List.length (A.sg_params sg) ->
  A.body_of Prog w = Some (ECall g (A.theta_ctx Cg) (map (fun k => (A.rkey_of Cg k, A.rkey_of Cg k)) nsg) (map EVar (seq 1 n0))) ->
  ItemWrap n F C (sp, Some (AEPath pa None))
    (fun x => bind_method_expression (A.canon C rt) m (A.theta_of C ap) w types sm x)
    (A.provide C (A.mkey (A.canon C rt) m) ap (A.set_mth C (A.canon C rt) m sg)) F [].
Proof.
  intros k Bel n Le F C sp pa m sg rt g types sm w n0 Cg nsg ap FP Ctx Len BodyW.
  remember (AEPath pa None) as src eqn:Src. remember (@None ty) as none eqn:NoneE.
  destruct FP as [F' C' pa' g' sg' Cg' ns' sm' th R K Def Ctx' Sat CT SigG|F' C' pa' g' sg' w' n' R K Abs' Fn Len' Fresh' Body'].
  2: { exfalso. destruct Ctx as (d0 & _ & _ & _ & _ & _ & D & _). unfold A.Fresh in Fresh'. congruence. }
  destruct (PublicContextFacts.function_context_unique W g' Cg' ns' Cg nsg Ctx' Ctx) as [-> ->].
  subst n0.
  intros t B cont Safe [|j] Lj d types0 H lg st Pre0; [exact Logic.I|].
  unfold bind_method_expression. rewrite eval_bind_eq.
  destruct j as [|j]; [exact Logic.I|]. rewrite bindings_method_eq.
  pose proof Pre0 as [Frame [Ctx0 [L [St Real]]]].
  destruct (callee_realizes (S j) C' d H g' Cg nsg sm' th ltac:(eapply realizes_lower; [|exact Real]; lia) Ctx Sat CT)
    as [dc [Sel _]].
  pose proof (wrapped_callable k Bel (S j) ltac:(lia) C' sg' g' th Cg nsg sm' w (A.canon C' rt) d H dc
    Def Ctx Sat CT SigG Sel BodyW ltac:(eapply realizes_lower; [|exact Real]; lia)) as Ok.
  rewrite Sel. destruct j as [|j]; [exact Logic.I|]. rewrite bindings_nil_eq.
  unfold A.set_mth in Safe.
  rewrite (provide_set C' (A.mkey (A.canon C' rt) m) (A.ProvidedMethod sg' []) ap ltac:(intros; discriminate)) in Safe.
  assert (Key : A.rkey (set_cx C' (A.mkey (A.canon C' rt) m) (A.ProvidedMethod sg' ap)) (A.canon C' rt) m =
                IMth (A.canon C' rt) m (A.theta_of C' ap)).
  { unfold A.rkey. rewrite capps_ilook. unfold set_cx. cbn [A.c_provisions].
    rewrite ilook_set, (proj2 (item_eqb_spec _ _) eq_refl). cbn [option_map A.provision_inst].
    apply f_equal, theta_of_sub. reflexivity. }
  set (d' := install_provision (IMth (A.canon C' rt) m (A.theta_of C' ap))
    (FunctionClosure w (typed_capture d (TypeCapture th th) dc)) d).
  apply (outcome_env F' d d' H types0 t _ _ eq_refl).
  eapply O.outcome_breaks; [intro b; left; exact b|].
  apply (Safe (S (S j)) ltac:(lia) d' types0 H lg st).
  split; [exact Frame|]. split; [exact Ctx0|]. split; [exact L|]. split; [exact St|].
  apply (realizes_set (S (S j)) C' d H (A.mkey (A.canon C' rt) m) (A.ProvidedMethod sg' ap)).
  - eapply realizes_lower; [|exact Real]. lia.
  - intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
  - reflexivity.
  - intros k0 Ne. apply install_other. intro E. apply Ne. rewrite E. unfold A.mkey. cbn [A.rkey_of].
    symmetry. exact Key.
  - cbn [NewOk]. exists (FunctionClosure w (typed_capture d (TypeCapture th th) dc)).
    split; [transitivity (lookup_provision (IMth (A.canon C' rt) m (A.theta_of C' ap)) d'); [f_equal; exact Key|]|].
    { unfold d'. rewrite lookup_install, (proj2 (item_eqb_spec _ _) eq_refl). reflexivity. }
    exact (callable_transfer (S (S j)) (S (S j)) H d d' _ _ _ _ _ (le_n _) eq_refl eq_refl Ok).
Qed.

Lemma meth_bind_safe : forall k, Below k -> forall n F C pa dn a y r m bs rt sg target pre sm ap types,
  A.resolve_sym W (A.fr_mod F) pa = Some r -> A.MethodNamed W (A.fr_mod F) r dn m ->
  A.Brackets W (A.fr_mod F) (A.c_ty C) m a bs ->
  A.TyOf W (A.fr_mod F) (bs ++ A.c_ty C)%list None (ATRef pa None) rt ->
  A.SigOf W m (bs ++ A.c_ty C)%list (Some rt) sg ->
  A.MethProvider W Prog F C y sg rt target types pre sm ->
  A.AppOf W m (bs ++ A.c_ty C)%list ap ->
  n <= S k -> ItemWrap n F C (Spec (Some pa) (Some dn) a, Some y)
    (fun x => pre (bind_method_expression (A.canon C rt) m (A.theta_of C ap) target types sm x))
    (A.provide C (A.mkey (A.canon C rt) m) ap (A.set_mth C (A.canon C rt) m sg)) F [].
Proof.
  intros k Bel n F C pa dn a y r m bs rt sg target pre sm ap types R Named Br Ty Sig Prov App Le.
  pose proof (method_sig_scoped F C m a bs pa rt sg Br Ty Sig) as SigSc.
  destruct Prov as [F C rp n0 rg att rt sg Ca ns sm th rargs Cd Rp Att Def Canon -> Ctx Sat CT SigA
    |F C pw sg rt g sm w n0 Cg nsg types FP Ctx Len Fresh Body|F C ps r0 body sg rt pid pre sm LP];
    [|exact (wrapped_meth_bind_safe k Bel n Le F C _ pw m sg rt g types sm w n0 Cg nsg ap FP Ctx Len Body)
     |exact (lambda_meth_bind_safe k Bel n Le F C _ m sg rt ps r0 body pid pre sm ap LP SigSc)].
  intros t B cont Safe [|j] Lj d types0 H lg st Pre0; [exact Logic.I|].
  unfold bind_method_expression. rewrite eval_bind_eq.
  destruct j as [|j]; [exact Logic.I|]. rewrite bindings_method_eq.
  pose proof Pre0 as [Frame [Ctx0 [L [St Real]]]].
  assert (Keys : map fst rargs = A.w_reqs W rg).
  { destruct Def as [rn [ps [rt0 [b Def]]]]. unfold A.ast_of in Def.
    destruct (A.w_decl W att) as [dcl|] eqn:Decl; [|discriminate]. injection Def as Ast.
    pose proof (all_fn_elab att dcl (RAttached rn) n0 ps rt0 b Decl Ast dcl (RAttached rn) n0 ps rt0 b Decl Ast)
      as (C0 & ns0 & this & g0 & F0 & x0 & t0 & _ & Rcv & _).
    destruct Rcv as [r' [args0 [Att0 [Kind _]]]].
    pose proof (attached_unique rg r' n0 att Att Att0) as <-.
    exact (bind_nominal_formed F C pa a bs m rt rg rargs Br Ty Canon Kind). }
  destruct (attached_provider_callable k Bel (S j) ltac:(lia) C rg n0 att rt sg Ca ns sm th rargs d H
    Att Def Canon Keys Ctx Sat CT SigA ltac:(eapply realizes_lower; [|exact Real]; lia)) as [dc [Sel Ok]].
  rewrite Sel. destruct j as [|j]; [exact Logic.I|]. rewrite bindings_nil_eq.
  unfold A.set_mth in Safe.
  rewrite (provide_set C (A.mkey (A.canon C rt) m) (A.ProvidedMethod sg []) ap ltac:(intros; discriminate)) in Safe.
  set (C' := set_cx C (A.mkey (A.canon C rt) m) (A.ProvidedMethod sg ap)) in *.
  assert (Key : A.rkey C' (A.canon C rt) m = IMth (A.canon C rt) m (A.theta_of C ap)).
  { unfold A.rkey. rewrite capps_ilook. unfold C', set_cx. cbn [A.c_provisions].
    rewrite ilook_set, (proj2 (item_eqb_spec _ _) eq_refl). cbn [option_map A.provision_inst].
    apply f_equal, theta_of_sub. reflexivity. }
  unfold C' in Key.
  set (d' := install_provision (IMth (A.canon C rt) m (A.theta_of C ap))
    (FunctionClosure att (typed_capture d (TypeCapture th th) dc)) d).
  apply (outcome_env F d d' H types0 t _ _ eq_refl).
  eapply O.outcome_breaks; [intro b; left; exact b|].
  apply (Safe (S (S j)) ltac:(lia) d' types0 H lg st).
  split; [exact Frame|]. split; [exact Ctx0|]. split; [exact L|]. split; [exact St|].
  apply (realizes_set (S (S j)) C d H (A.mkey (A.canon C rt) m) (A.ProvidedMethod sg ap)).
  - eapply realizes_lower; [|exact Real]. lia.
  - intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
  - reflexivity.
  - intros k0 Ne. apply install_other. intro E. apply Ne. rewrite E. unfold A.mkey. cbn [A.rkey_of].
    symmetry. exact Key.
  - cbn [NewOk]. exists (FunctionClosure att (typed_capture d (TypeCapture th th) dc)).
    split; [transitivity (lookup_provision (IMth (A.canon C rt) m (A.theta_of C ap)) d'); [f_equal; exact Key|]|].
    { unfold d'. rewrite lookup_install, (proj2 (item_eqb_spec _ _) eq_refl). reflexivity. }
    exact (callable_transfer (S (S j)) (S (S j)) H d d' _ _ _ _ _ (le_n _) eq_refl eq_refl Ok).
Qed.

(* A bind item changes only the frame's fresh generations. *)
Lemma bind_item_frame : forall F C it w C' F' m, A.BindItem W Prog F C it w C' F' m ->
  A.fr_mod F' = A.fr_mod F /\ A.fr_ret F' = A.fr_ret F /\ A.fr_this F' = A.fr_this F /\
  A.fr_loop F' = A.fr_loop F /\ A.fr_var F' = A.fr_var F.
Proof. intros F C it w C' F' m D. destruct D; cbn; auto 10. Qed.

Lemma bind_items_frame : forall F C its w C' F' m, A.BindItems W Prog F C its w C' F' m ->
  A.fr_mod F' = A.fr_mod F /\ A.fr_ret F' = A.fr_ret F /\ A.fr_this F' = A.fr_this F /\
  A.fr_loop F' = A.fr_loop F /\ A.fr_var F' = A.fr_var F.
Proof.
  intros F C its w C' F' m D. induction D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 D1 Rest IH]; [auto 10|].
  destruct (bind_item_frame _ _ _ _ _ _ _ D1) as [E1 [E2 [E3 [E4 E5]]]].
  destruct IH as [I1 [I2 [I3 [I4 I5]]]]. repeat split; congruence.
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

(* One binding, run by eval_bindings at fuel at most r - 1, with the
   provisions realized at the fixed index r: either it completes, in the
   context after it, or it stops with an outcome of its own. *)
Definition BindStep r F C F' C' (Bi : Prop) (b : binding) : Prop :=
  FrameSame F F' /\
  forall j d types H g st, S j <= r -> Pre r F C d types H g st -> forall rest,
    (exists d' g' st' H', heap_extends H H' /\ type_meanings d' = type_meanings d /\
       Pre r F' C' d' types H' g' st' /\
       I.eval_bindings (S j) P d g st (b :: rest) = I.eval_bindings j P d' g' st' rest) \/
    (exists res, I.eval_bindings (S j) P d g st (b :: rest) = inr res /\
       (forall v, I.evaluation_signal res <> Normal v) /\ Outcome F d H types TUnit Bi res).

Inductive Chain (k : nat) : A.frame -> A.cx -> A.frame -> A.cx -> Prop -> list binding -> Prop :=
| ChainNil : forall F C (B : Prop), Chain k F C F C B []
| ChainCons : forall F C F1 C1 F2 C2 (B1 B2 B : Prop) b bs,
    (forall r, r <= S k -> BindStep r F C F1 C1 B1 b) -> Chain k F1 C1 F2 C2 B2 bs ->
    (B1 \/ B2 -> B) -> Chain k F C F2 C2 B (b :: bs).

Lemma frame_same_refl : forall F, FrameSame F F.
Proof. intros F. repeat split. Qed.

Lemma frame_same_trans : forall F1 F2 F3, FrameSame F1 F2 -> FrameSame F2 F3 -> FrameSame F1 F3.
Proof. intros F1 F2 F3 [A1 [A2 [A3 A4]]] [B1 [B2 [B3 B4]]]. repeat split; congruence. Qed.

Lemma chain_frame : forall k F C F' C' B bs, Chain k F C F' C' B bs -> FrameSame F F'.
Proof.
  intros k F C F' C' B bs D. induction D as [F C B|F C F1 C1 F2 C2 B1 B2 B b bs Step Rest IH Imp];
    [apply frame_same_refl|].
  exact (frame_same_trans _ _ _ (proj1 (Step (S k) (le_n _))) IH).
Qed.

Lemma chain_weaken : forall k F C F' C' (B B' : Prop) bs, Chain k F C F' C' B bs -> (B -> B') -> Chain k F C F' C' B' bs.
Proof.
  intros k F C F' C' B B' bs D Imp. destruct D as [F C B|F C F1 C1 F2 C2 B1 B2 B b bs Step Rest Imp0];
    [constructor|]. apply (ChainCons k F C F1 C1 F2 C2 B1 B2 B' b bs Step Rest). tauto.
Qed.

Lemma chain_app : forall k F C F1 C1 F2 C2 (B1 B2 B : Prop) bs1 bs2,
  Chain k F C F1 C1 B1 bs1 -> Chain k F1 C1 F2 C2 B2 bs2 -> (B1 \/ B2 -> B) -> Chain k F C F2 C2 B (bs1 ++ bs2)%list.
Proof.
  intros k F C F1 C1 F2 C2 B1 B2 B bs1 bs2 D1. revert F2 C2 B2 B bs2.
  induction D1 as [F C B0|F C F1 C1 F1' C1' Ba Bb B0 b bs Step Rest IH Imp]; intros F2 C2 B2 B bs2 D2 Imp2.
  - cbn. apply (chain_weaken k F C F2 C2 B2 B bs2 D2). tauto.
  - cbn. apply (ChainCons k F C F1 C1 F2 C2 Ba (Bb \/ B2) B b (bs ++ bs2)%list Step).
    + apply (IH F2 C2 B2 (Bb \/ B2) bs2 D2). tauto.
    + tauto.
Qed.

(* A chain gives the expression-level wrapper: each binding in turn, then
   the continuation. *)
Lemma chain_wrap : forall k n F C F' C' (Bi : Prop) bs, Chain k F C F' C' Bi bs -> n <= S k ->
  WrapSafe n F C F' C' Bi (fun x => fold_right (fun b x => EBind b x) x bs).
Proof.
  intros k n F C F' C' Bi bs D Le. induction D as [F C B0|F C F1 C1 F2 C2 B1 B2 B b bs Step Rest IH Imp].
  - intros t B x Safe. apply (wrap_breaks n F C t B); [tauto|exact Safe].
  - intros t B' x Safe. pose proof (IH t B' x Safe) as Inner. cbn [fold_right].
    intros [|j] Lj d types H g st Pre0; [exact Logic.I|]. rewrite eval_bind_eq.
    destruct j as [|j]; [exact Logic.I|].
    destruct (Step (S (S j)) ltac:(lia)) as [Same Run].
    destruct (Run j d types H g st ltac:(lia) Pre0 []) as [[d' [g' [st' [H' [E [Meq [Pre' Eq]]]]]]]|[res [Eq [NN Out]]]];
      rewrite Eq.
    + destruct j as [|j]; [exact Logic.I|]. rewrite bindings_nil_eq.
      pose proof (Inner (S (S j)) ltac:(lia) d' types H' g' st' (pre_lower Pre')) as Out.
      eapply outcome_heap; [exact E|]. apply (outcome_env F d d' H' types t _ _ Meq).
      apply (outcome_frame F F1 d' H' types t _ _ Same).
      eapply O.outcome_breaks; [|exact Out]. tauto.
    + eapply outcome_pass; [|exact NN|exact Out]. tauto.
Qed.

Lemma realizes_pre_lower : forall j r F C d types H g st, Pre r F C d types H g st -> j <= r ->
  Pre j F C d types H g st.
Proof.
  intros j r F C d types H g st [Frame [Ctx [L [St Real]]]] Le.
  exact (conj Frame (conj Ctx (conj L (conj St (realizes_lower r j C d H Le Real))))).
Qed.

Lemma heap_extends_trans : forall H1 H2 H3, heap_extends H1 H2 -> heap_extends H2 H3 -> heap_extends H1 H3.
Proof. intros H1 H2 H3 E1 E2 a c L. exact (E2 _ _ (E1 _ _ L)). Qed.

(* A chain run by eval_bindings: it completes, in the context after it,
   or stops with an outcome. *)
Lemma chain_run : forall k F C F' C' (Bi : Prop) bs, Chain k F C F' C' Bi bs ->
  forall r j d types H g st rest, r <= S k -> j < r -> Pre r F C d types H g st ->
  (exists d' g' st' H' j', j = j' + List.length bs /\ heap_extends H H' /\ type_meanings d' = type_meanings d /\
     Pre r F' C' d' types H' g' st' /\
     I.eval_bindings j P d g st (bs ++ rest)%list = I.eval_bindings j' P d' g' st' rest) \/
  (exists res, I.eval_bindings j P d g st (bs ++ rest)%list = inr res /\
     (forall v, I.evaluation_signal res <> Normal v) /\ Outcome F d H types TUnit Bi res).
Proof.
  intros k F C F' C' Bi bs D. induction D as [F C B0|F C F1 C1 F2 C2 B1 B2 B b bs Step Rest IH Imp];
    intros r j d types H g st rest Lr Lj Pre0.
  - left. exists d, g, st, H, j. split; [cbn; lia|]. split; [intros a c L; exact L|].
    split; [reflexivity|]. split; [exact Pre0|reflexivity].
  - destruct j as [|j]; [right; eexists; split; [reflexivity|]; split; [intros v E; discriminate|exact Logic.I]|].
    destruct (Step r Lr) as [Same Run].
    destruct (Run j d types H g st ltac:(lia) Pre0 (bs ++ rest)%list)
      as [[d1 [g1 [st1 [H1 [E1 [M1 [Pre1 Eq1]]]]]]]|[res [Eq [NN Out]]]].
    + cbn [app]. rewrite Eq1.
      destruct (IH r j d1 types H1 g1 st1 rest Lr ltac:(lia) Pre1)
        as [[d' [g' [st' [H' [j' [Ej [E' [M' [Pre' Eq']]]]]]]]]|[res [Eq [NN Out]]]].
      * left. exists d', g', st', H', j'. split; [cbn; lia|]. split; [exact (heap_extends_trans _ _ _ E1 E')|].
        split; [congruence|]. split; [exact Pre'|exact Eq'].
      * right. exists res. split; [exact Eq|]. split; [exact NN|].
        eapply outcome_heap; [exact E1|]. apply (outcome_env F d d1 H1 types TUnit _ _ M1).
        apply (outcome_frame F F1 d1 H1 types TUnit _ _ Same).
        eapply O.outcome_breaks; [|exact Out]. tauto.
    + right. exists res. cbn [app]. split; [exact Eq|]. split; [exact NN|].
      eapply O.outcome_breaks; [|exact Out]. tauto.
Qed.

Lemma type_bstep : forall r F C t0 tau,
  BindStep r F C F (A.set_ty C t0 (A.canon C tau)) False (BType t0 (A.canon C tau)).
Proof.
  intros r F C t0 tau. split; [apply frame_same_refl|]. intros j d types H g st Lj Pre0 rest.
  left. exists (bind_type d t0 (A.canon C tau)), g, st, H. split; [intros a c L; exact L|].
  split; [reflexivity|]. split; [exact Pre0|]. reflexivity.
Qed.

Lemma val_bstep : forall k, Below k -> forall r, r <= S k -> forall F C sp v declared y x xt a,
  A.Ex W Prog F C y (Some declared) x xt -> A.fits xt declared ->
  BindStep r F C F (A.provide C (IVal v) a (A.set_val C v declared)) (ItemBreaks F (sp, Some y)) (BValue v x).
Proof.
  intros k Bel r Lr F C sp v declared y x xt a Dy Fits. split; [apply frame_same_refl|].
  intros j d types H g st Lj Pre0 rest. rewrite bindings_value_eq.
  destruct (Bel j ltac:(lia)) as [SEx _].
  pose proof (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits
    (SEx F C y _ x xt Dy d types H g st (realizes_pre_lower j r F C d types H g st Pre0 ltac:(lia)))) as Out.
  destruct (I.eval j P d g st x) as [[w| | | | | | |] g1 st1] eqn:E;
    try (right; eexists; split; [reflexivity|]; split; [intros v' Ev; discriminate|];
         eapply outcome_pass; [exact (fun b => A.IBExpr sp b)|intros v' Ev; discriminate|exact Out]).
  destruct Out as [H1 [E1 [S1 [L1 T1]]]].
  left. exists (install_provision (IVal v) (ValueProvision w) d), g1, st1, H1.
  split; [exact E1|]. split; [reflexivity|]. split; [|reflexivity].
  pose proof Pre0 as [Frame [Ctx [_ [_ Real]]]].
  unfold A.set_val. rewrite (provide_set C (IVal v) (A.ProvidedVal declared []) a ltac:(intros; eauto)).
  split; [exact Frame|]. split; [exact Ctx|]. split; [exact L1|]. split; [exact S1|].
  apply (realizes_set r C d H1 (IVal v) (A.ProvidedVal declared a)).
  - exact (realizes_extends _ _ _ _ _ E1 Real).
  - intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
  - reflexivity.
  - intros k0 Ne. apply install_other. exact Ne.
  - exists w. split; [cbn; rewrite Nat.eqb_refl; reflexivity|exact T1].
Qed.

Lemma fn_bstep : forall k, Below k -> forall r, r <= S k -> forall F C sp f g sg pg types sm a,
  A.FnProvider W Prog F C (AEPath pg None) sg None g types sm ->
  BindStep r F C F (A.provide C (IFn f) a (A.set_fn C f sg)) (ItemBreaks F (sp, Some (AEPath pg None)))
    (BFunction f g types sm).
Proof.
  intros k Bel r Lr F C sp f g sg pg types sm a Prov. split; [apply frame_same_refl|].
  intros j d types0 H lg st Lj Pre0 rest. rewrite bindings_function_eq.
  pose proof Pre0 as [Frame [Ctx [L [St Real]]]].
  destruct (provider_callable k Bel r Lr F C pg sg g types sm d H Prov Real) as [dc [Sel Ok]].
  rewrite Sel. left.
  set (d' := install_provision (IFn f) (FunctionClosure g (typed_capture d types dc)) d).
  exists d', lg, st, H. split; [intros a0 c L0; exact L0|]. split; [reflexivity|]. split; [|reflexivity].
  unfold A.set_fn. rewrite (provide_set C (IFn f) (A.ProvidedFn sg []) a ltac:(intros; discriminate)).
  split; [exact Frame|]. split; [exact Ctx|]. split; [exact L|]. split; [exact St|].
  apply (realizes_set r C d H (IFn f) (A.ProvidedFn sg a)).
  - exact Real.
  - intros k0 p0 [Eq|[]]. injection Eq as <- <-. exact Logic.I.
  - reflexivity.
  - intros k0 Ne. apply install_other. exact Ne.
  - exists (FunctionClosure g (typed_capture d types dc)). split; [cbn; rewrite Nat.eqb_refl; reflexivity|].
    exact (callable_transfer r r H d d' _ _ _ _ _ (le_n _) eq_refl eq_refl Ok).
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

Fixpoint item_safe k (Bel : Below k) n F C it w C' F' m (D : A.BindItem W Prog F C it w C' F' m) (Le : n <= S k)
  {struct D} : WrapSafe n F C F' C' (ItemBreaks F it) w
with items_safe k (Bel : Below k) n F C its w C' F' m (D : A.BindItems W Prog F C its w C' F' m) (Le : n <= S k)
  {struct D} : WrapSafe n F C F' C' (ItemsBreak F its) w.
Proof.
  - destruct D as [| | | | |F C pa u dcl n0 ps a r bodies wrap C' F' made R Dcl Ast NotApp Ditems| | |].
    + intros t' B x Safe. apply (wrap_breaks n F C t' (B \/ False)); [tauto|]. apply type_bind_safe. exact Safe.
    + eapply val_bind_safe; eassumption.
    + eapply fn_bind_safe; eassumption.
    + eapply lambda_fn_bind_safe; eassumption.
    + eapply meth_bind_safe; eassumption.
    + (* `bind F;` for F with a body: its binds, in place *)
      pose proof (items_safe k Bel n (A.with_mod F (A.d_mod dcl) u) C (List.concat bodies) wrap C' F' made Ditems Le) as Inner.
      destruct (bind_items_frame _ _ _ _ _ _ _ Ditems) as [_ [Er [Et [El Ev]]]]. cbn in Er, Et, El, Ev.
      intros t B x Safe.
      assert (Safe' : CodeSafe n F' C' t B x) by (apply (codesafe_frame n (A.Frame (A.fr_mod F) (A.fr_ret F) (A.fr_this F) (A.fr_loop F) (A.fr_var F) (A.fr_fresh F') (A.fr_applying F) (A.fr_pins F)) F' C' t B x); cbn; try congruence; exact Safe).
      pose proof (Inner t B x Safe') as Out.
      apply (codesafe_frame n (A.with_mod F (A.d_mod dcl) u) F C t _ (wrap x)); [reflexivity..|].
      apply (wrap_breaks n _ C t (B \/ ItemsBreak (A.with_mod F (A.d_mod dcl) u) (List.concat bodies))); [|exact Out].
      intros [b|[it [Iit Bi]]]; [left; exact b|right].
      eapply A.IBApply; [exact R|exact Dcl|exact Ast|exact Iit|exact Bi].
    + eapply case_BApplyAbstract; eassumption.
    + eapply case_BFunCopy; eassumption.
    + eapply case_BFunClosure; eassumption.
  - destruct D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 D1 Rest].
    + intros t B x Safe. apply (wrap_breaks n F C t B); [tauto|exact Safe].
    + intros t B x Safe.
      pose proof (items_safe k Bel n F1 C1 r w2 C2 F2 m2 Rest Le t B x Safe) as S2.
      pose proof (item_safe k Bel n F C it w1 C1 F1 m1 D1 Le t _ (w2 x) S2) as S1.
      destruct (bind_item_frame _ _ _ _ _ _ _ D1) as [Mod _].
      unfold ItemsBreak, ItemBreaks in S1. rewrite Mod in S1. fold (ItemBreaks F it) in S1.
      apply (wrap_breaks n F C t ((B \/ ItemsBreak F r) \/ ItemBreaks F it) _ (w1 (w2 x))); [|exact S1].
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
  exact (unit_never_breaks _).
Qed.



Lemma val_step : forall F C pa s t, A.nlook s (A.c_vals C) = Some t ->
  StepSafe k F C t (A.Breaks W (A.fr_mod F) (AEPath pa None)) (EVal s).
Proof.
  intros F C pa s t Found d types H lg lst Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [_ [[Vals _] _]]]]]]. destruct (Vals s t Found) as [w [Look T]].
  cbn [I.eval]. rewrite Look. eapply outcome_now; eassumption.
Qed.

Lemma invoke_step : forall F C pa args f g xs,
  A.nlook f (A.c_fns C) = Some g -> A.Args W Prog F C args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) (A.Breaks W (A.fr_mod F) (AECall pa None args)) (EInvoke f xs).
Proof.
  intros F C pa args f g xs Found Args d types H lg lst Pre0.
  pose proof Pre0 as [_ [_ [_ [_ [_ [[_ [Fns _]] _]]]]]]. destruct (Fns f g Found) as [pv [Look Ok]].
  pose proof (args_safe k Bel k (le_n k) F C (A.Breaks W (A.fr_mod F) (AECall pa None args)) xs (A.sg_params g) d types H lg lst
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

Lemma attached_step : forall F C o ox ot0 ot n args h att Ca ns sm th g xs rargs wrap Cd (B : Prop),
  A.Ex W Prog F C o None ox ot0 -> (A.Breaks W (A.fr_mod F) o -> B) -> (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) ->
  ot = A.canon C ot0 ->
  A.head ot = Some h -> A.Attached W h n att ->
  (exists rn ps rt b, A.ast_of W att = Some (ADFn (RAttached rn) n ps rt (Some b))) ->
  rargs = (match ot with TNom _ xs => xs | _ => [] end) ->
  A.CtxOf W att Ca ns ->
  A.Demand W (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C)
    (map (A.at_site (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C)) ns) []
    (fold_left (fun C q => A.set_ty C (fst q) (snd q)) rargs C) wrap Cd ->
  A.Satisfies Cd Ca th ns sm -> A.CallTheta W Cd Ca att th -> A.SigOf W att (A.c_ty Cd) (Some ot) g ->
  A.Args W Prog F C args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) B (wrap (ECall att th sm (ox :: xs))).
Proof.
  intros F C o ox ot0 ot n args h att Ca ns sm th g xs rargs wrap Cd B Do BrkO BrkA Ot Head Att Def Rargs Ctx Dem Sat CT Sig Args
    d types H lg lst Pre0.
  destruct Def as [rn [ps [rt [b Def]]]]. unfold A.ast_of in Def.
  destruct (A.w_decl W att) as [dcl|] eqn:Decl; [|discriminate]. injection Def as Ast.
  pose proof (all_fn_elab att dcl (RAttached rn) n ps rt b Decl Ast dcl (RAttached rn) n ps rt b Decl Ast)
    as (C0 & ns0 & this & g0 & F0 & x0 & t0 & _ & Rcv & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & View).
  destruct Rcv as [r [args0 [Att0 [Kind _]]]].
  pose proof (attached_unique h r n att Att Att0) as <-.
  pose proof Head as Head'. rewrite Ot in Head'.
  destruct (ex_nominal_formed F C o None ox ot0 Do h Head' Kind) as [xs0 [Eot Keys]].
  rewrite <- Ot in Eot. rewrite Eot in Ot. subst ot. cbn in Rargs. subst rargs.
  set (Cd0 := fold_left (fun C q => A.set_ty C (fst q) (snd q)) xs0 C) in *.
  destruct (set_tys_fields xs0 C) as [Es [Ep [El Et]]].
  assert (Nd : NoDup (map fst xs0)) by (rewrite Keys; destruct World as [bd Wo]; exact (A.wo_reqs_nodup Wo h)).
  assert (Cty : A.c_ty Cd = (rev xs0 ++ A.c_ty C)%list) by (rewrite (demand_ty _ _ _ _ _ _ Dem); exact Et).
  refine (demand_case k Bel F Cd0 _ Cd wrap (A.sg_ret g) B _ Dem _ d types H lg lst
    (pre_ty_free _ _ C Cd0 _ _ _ _ _ Es Ep El Pre0)).
  intros j Lj d' types' H' g' st' PreD PreD0.
  assert (PreC : Pre j F C d' types' H' g' st')
    by exact (pre_ty_free _ _ Cd0 C _ _ _ _ _ (eq_sym Es) (eq_sym Ep) (eq_sym El) PreD0).
  destruct j as [|j]; [exact Logic.I|].
  cbn [I.eval]. change (view_function P att) with (A.body_of Prog att). rewrite View.
  assert (ArgsS : Forall2 (fun e t => CodeSafe k F C t B e) (ox :: xs) (TNom h xs0 :: A.sg_params g)).
  { constructor; [|exact (args_code_of k Bel F C args _ xs Args B BrkA)].
    apply (ex_code k Bel F C o None ox ot0 _ B Do); [right; right; exact Ot|exact BrkO]. }
  pose proof (args_code k j ltac:(lia) F C B (ox :: xs) _ d' types' H' g' st' ArgsS (pre_lower PreC)) as AOut.
  destruct (I.eval_arguments j P d' g' st' (ox :: xs)) as [[[ws g1] st1]|rr]; cbn in AOut.
  2: { destruct AOut as [Out NotNormal]. eapply outcome_pass; [exact (fun x => x)|exact NotNormal|exact Out]. }
  destruct AOut as [H1 [E1 [S1 [L1 T1]]]].
  inversion T1 as [|w0 t0' ws' ts' Tw0 Tws]; subst.
  pose proof PreD as [_ [_ [_ [_ RealD]]]].
  destruct (callee_realizes j Cd d' H1 att Ca ns sm th (realizes_extends _ _ _ _ _ E1 RealD) Ctx Sat CT)
    as [dc [Sel [Ag RealA]]].
  rewrite Sel, (call_environment_callee d' _ sm dc Sel).
  pose proof RealD as [AbsD _].
  destruct (body_call_method j (Bel j ltac:(lia)) att dcl rn n ps rt b Ca ns _ H1 st1 w0 ws' d'
    (TNom h xs0) (A.sg_params g) (A.sg_ret g) Decl Ast Ctx RealA S1) as [body [View' Run]].
  - intros r' args' g0' Att' Reqs Sig0.
    pose proof (attached_unique h r' n att Att Att') as <-.
    assert (Recv : app_subst (callee_meanings d' th) (TNom h args') = ground d' (TNom h xs0))
      by exact (receiver_agree att Ca ns Cd th d' h args' xs0 (A.c_ty C) Ctx CT AbsD Nd Cty Reqs Keys).
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
  A.Ex W Prog F C o None ox ot0 -> (A.Breaks W (A.fr_mod F) o -> B) -> (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) ->
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
  A.Ex W Prog F C o None ox ot0 -> (A.Breaks W (A.fr_mod F) o -> B) -> (forall a, In a args -> A.Breaks W (A.fr_mod F) a -> B) ->
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
  exact (attached_step F C o ox ot0 _ n args h att Ca ns sm th g xs rargs wrap Cd B Do BrkO BrkA
    eq_refl Head Att Def Rargs Ctx Dem Sat CT Sig Args).
Qed.

Fixpoint ex_step F C a exp x t (D : A.Ex W Prog F C a exp x t) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t (A.Breaks W (A.fr_mod F) a) (I.eval (S k) P d g st x)
with blk_step F C ss tl exp x t made (D : A.Blk W Prog F C ss tl exp x t made) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks W (A.fr_mod F) (ABlock ss tl)) (I.eval (S k) P d g st x)
with blke_step F C b exp x t (D : A.BlkE W Prog F C b exp x t) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks W (A.fr_mod F) b) (I.eval (S k) P d g st x)
with branch_step F C y exp x t (D : A.Branch W Prog F C y exp x t) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  Outcome F d H types t (A.Breaks W (A.fr_mod F) y) (I.eval (S k) P d g st x)
with cond_step F C c x (D : A.Cond W Prog F C c x) {struct D} :
  forall d types H g st, Pre (S k) F C d types H g st ->
  exists b, A.LangSym W "bool" A.NSName "Bool" b /\
    Outcome F d H types (TNom b []) (A.Breaks W (A.fr_mod F) c) (I.eval (S k) P d g st x).
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
      apply (outcome_seq _ F C d types H lg lst pay _ (A.Breaks W (A.fr_mod F) y) _ _ _ Pre0
        (fun B => A.BkCall pa a [y] (or_introl eq_refl) B) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      apply outcome_tag; [exact S1|exact L1|].
      exact (TC.tag_check W Prog Layouts Reps argv read d H1 s xs pay w
        (or_introl (conj K (conj Pay (forall2_keys _ _ (fun r v => A.nlook r (List.app bs (A.c_ty C)) = Some v) F2)))) T1).
    + destruct Def as [n0 [ps0 [rt0 [b0 Def]]]].
      exact (demand_case k Bel F C _ C' wrap (A.sg_ret sg) _ _ Dem
        (fun j Lj d' types' H' g' st' Pre' _ => call_code k Bel F C' args f n0 ps0 rt0 b0 Cf ns sm th sg xs _ Def Ctx Sat CT Sig Args
          (fun a In' B0 => A.BkCall pa None args In' B0) j Lj d' types' H' g' st' Pre') d types H lg lst Pre0).
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
      replace (I.truthy P w) with (Some bit) by (symmetry; exact Bit).
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
      replace (I.truthy P w) with (Some bit) by (symmetry; exact Bit).
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
        (fun B => stmt_breaks_block _ st0 rest tl (let_breaks _ _ _ _ _ _ Let B)) Out).
      intros H1 g1 st1 w E1 [Frame [Ctx [L1 [S1 Abs]]]] T1.
      eapply outcome_pop. eapply O.outcome_breaks; [apply block_breaks_cons|].
      apply (sblk F _ rest tl exp xr t made Drest d _ H1 _ st1).
      split; [exact Frame|]. split; [exact (ctx_locals_push F C d types n t0 mu Frame Ctx Named)|].
      split; [exact (locals_push W P H1 g1 types _ w _ L1 T1)|]. split; [exact S1|exact (realizes_same _ C _ _ _ eq_refl eq_refl eq_refl (realizes_lower (S k) k C d H1 (le_S _ _ (le_n k)) Abs))].
    + (* assign *) cbn [I.eval].
      pose proof (assign_eval k Bel F C n y tv v vt d types H lg lst Found Named Dy Fits (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ _ rest tl (A.SBAssign n B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest d types H1 g1 st1 (pre_lower Pre1))].
    + (* last expression *) cbn [I.eval].
      pose proof (sex F C y None v vt Dy d types H lg lst (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ _ [] None (A.SBExpr B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      destruct (ty_eqb vt A.never) eqn:Never.
      * apply PublicTypeFacts.ty_eqb_spec in Never. subst vt. exfalso; exact (ground_never d H1 w T1).
      * eapply O.outcome_breaks; [|exact (unit_k F C d types H1 g1 st1 (pre_lower Pre1))]. intros [].
    + (* expression *) cbn [I.eval].
      pose proof (sex F C y None v vt Dy d types H lg lst (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ _ rest tl (A.SBExpr B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest d types H1 g1 st1 (pre_lower Pre1))].
    + (* while *) cbn [I.eval].
      pose proof (while_safe k Bel k (le_n k) F C c cx' b bx bt d types H lg lst Dc Db (pre_lower Pre0)) as Out.
      apply (outcome_seq _ F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ _ rest tl (A.SBWhile b B)) Out).
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
      assert (XR : CodeSafe (S k) F' C' t (A.BlockBreaks W (A.fr_mod F) (ABlock rest tl)) xr).
      { destruct (bind_items_frame _ _ _ _ _ _ _ Ditems) as [ModI _].
        intros j Le d' types' H' g' st' Pre'. rewrite <- ModI. destruct (Nat.eq_dec j (S k)) as [->|Ne].
        - exact (blk_step F' C' rest tl exp xr t made' Drest d' types' H' g' st' Pre').
        - destruct (Bel j ltac:(lia)) as [_ [SBj _]]. exact (SBj F' C' rest tl exp xr t made' Drest d' types' H' g' st' Pre'). }
      pose proof (items_safe k Bel (S k) F C its wrap C' F' made Ditems (le_n _) t _ xr XR (S k) (le_n _)
        d types H lg lst Pre0) as Out.
      eapply O.outcome_breaks; [|exact Out].
      intros [Rest|[[sp oy] [Iit Bi]]]; [apply block_breaks_cons; exact Rest|].
      apply stmt_breaks_block. eapply A.SBBind; [exact Iit|exact Bi].
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
