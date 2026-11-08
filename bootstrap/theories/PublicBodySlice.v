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
  PublicBoolTyping PublicBodySafety PublicTypeFacts.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.
Local Open Scope string_scope.

Module BodySlice (Prims : PRIMITIVES).
Module O := Outcomes Prims.
Module I := O.I.
Module B := BoolTruthy Prims.

Section Slice.
Variables (W : A.world) (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Let P := program_of Prog argv read.
Variable state_ok : heap_types -> Prims.mstate -> Prop.

Hypothesis Layouts : Platform.Layouts W.
Hypothesis Bools : BoolSymbols W Prog.

Definition Absorbs (C : A.cx) (d : environment) : Prop :=
  forall t, ground d (A.canon C t) = ground d t.

Definition Pre F C d types H g st : Prop :=
  FrameOk F /\ CtxLocals F C d types /\ locals_typed W P H g types /\ state_ok H st /\ Absorbs C d.

Notation Outcome := (O.Outcome W P state_ok).

Definition SafeEx k : Prop := forall F C a exp x t, A.Ex W Prog F C a exp x t ->
  forall d types H g st, Pre F C d types H g st ->
  Outcome F d H types t (A.Breaks a) (I.eval k P d g st x).
Definition SafeBlk k : Prop := forall F C ss tl exp x t made, A.Blk W Prog F C ss tl exp x t made ->
  forall d types H g st, Pre F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks (ABlock ss tl)) (I.eval k P d g st x).
Definition SafeBlkE k : Prop := forall F C b exp x t, A.BlkE W Prog F C b exp x t ->
  forall d types H g st, Pre F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks b) (I.eval k P d g st x).
Definition SafeBranch k : Prop := forall F C y exp x t, A.Branch W Prog F C y exp x t ->
  forall d types H g st, Pre F C d types H g st ->
  Outcome F d H types t (A.Breaks y) (I.eval k P d g st x).
Definition SafeCond k : Prop := forall F C c x, A.Cond W Prog F C c x ->
  forall d types H g st, Pre F C d types H g st ->
  exists b, A.LangSym W "bool" A.NSName "Bool" b /\
    Outcome F d H types (TNom b []) (A.Breaks c) (I.eval k P d g st x).
Definition Safe k : Prop := SafeEx k /\ SafeBlk k /\ SafeBlkE k /\ SafeBranch k /\ SafeCond k.
Definition Below k : Prop := forall j, j <= k -> Safe j.

(* ---- obligations outside the slice -------------------------------------- *)

Definition StepSafe (k : nat) F C t (breaks : Prop) (x : expr) : Prop :=
  forall d types H g st, Pre F C d types H g st ->
  Outcome F d H types t breaks (I.eval (S k) P d g st x).

Hypothesis val_case : forall k, Below k -> forall F C pa s dcl n t0 t,
  (forall n', pa = [n'] -> A.slook n' (A.c_locals C) = None) ->
  A.resolve_sym W (A.fr_mod F) pa = Some s -> A.w_decl W s = Some dcl ->
  A.d_ast dcl = ADVal n t0 None -> A.nlook s (A.c_vals C) = Some t ->
  StepSafe k F C t (A.Breaks (AEPath pa None)) (EVal s).
Hypothesis call_defined_case : forall k, Below k -> forall F C pa args f Cf ns sm g xs wrap C',
  A.resolve_sym W (A.fr_mod F) pa = Some f -> A.kind_sym W f = Some A.DKFn ->
  (exists n ps rt b, A.ast_of W f = Some (ADFn RPlain n ps rt (Some b))) ->
  A.CtxOf W f Cf ns ->
  A.Demand W C (map (A.at_site C) ns) [] C wrap C' ->
  A.Satisfies C' Cf ns sm -> A.SigOf W f (A.c_ty C') None g ->
  A.Args W Prog F C' args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) (A.Breaks (AECall pa None args))
    (wrap (ECall f (A.theta_at W C' f) sm xs)).
Hypothesis call_abstract_case : forall k, Below k -> forall F C pa args f g xs,
  A.resolve_sym W (A.fr_mod F) pa = Some f -> A.kind_sym W f = Some A.DKFn ->
  (exists n ps rt, A.ast_of W f = Some (ADFn RPlain n ps rt None)) ->
  A.nlook f (A.c_fns C) = Some g ->
  A.Args W Prog F C args (A.sg_params g) xs ->
  StepSafe k F C (A.sg_ret g) (A.Breaks (AECall pa None args)) (EInvoke f xs).
Hypothesis method_case : forall k, Below k -> forall F C o pa args ox ot x t,
  A.Ex W Prog F C o None ox ot ->
  A.MethCall W Prog F C ox (A.canon C ot) pa args x t ->
  StepSafe k F C t (A.Breaks (AEMeth o pa args)) x.
Hypothesis unary_case : forall k, Below k -> forall F C op y lib n m ox ot x t,
  A.operator_method op true = Some (lib, n) -> A.LangSym W lib A.NSDetached n m ->
  A.Ex W Prog F C y None ox ot -> A.head (A.canon C ot) <> None ->
  A.Provided W Prog F C ox (A.canon C ot) m [] x t ->
  StepSafe k F C t (A.Breaks (AEUnary op y)) x.
Hypothesis binary_case : forall k, Below k -> forall F C op l r lib n m ox ot x t,
  A.operator_method op false = Some (lib, n) -> A.LangSym W lib A.NSDetached n m ->
  A.Ex W Prog F C l None ox ot -> A.head (A.canon C ot) <> None ->
  A.Provided W Prog F C ox (A.canon C ot) m [r] x t ->
  StepSafe k F C t (A.Breaks (AEBinary op l r)) x.
Hypothesis literal_case : forall k, Below k -> forall F C text x t,
  A.NumericText text -> A.Literal W C (A.literal_tree_of text) x t ->
  StepSafe k F C t (A.Breaks (AELit text)) x.
Hypothesis bind_case : forall k, Below k -> forall F C its rest tl exp wrap C' F' xr t made made',
  A.BindItems W Prog F C its wrap C' F' made ->
  A.Blk W Prog F' C' rest tl exp xr t made' ->
  StepSafe k F C t (A.BlockBreaks (ABlock (ASBind its :: rest) tl)) (wrap xr).

(* Checked tag construction from a typed payload (T24's premises). *)
Hypothesis tag_check : forall d H s xs pay value,
  (A.kind_sym W s = Some A.DKTag /\ A.PayloadOf W s xs pay) \/
  (A.kind_sym W s = Some A.DKUnit /\ xs = [] /\ pay = TUnit) ->
  value_typed W P H value (ground d pay) ->
  match I.check_tag P d s xs value with
  | Normal result => value_typed W P H result (ground d (TNom s xs))
  | Trap _ => True
  | _ => False
  end.

(* Match arm selection for a typed scrutinee (static pattern typing). *)
Hypothesis arms_select : forall F C st arms exp xarms t d H w,
  A.Arms W Prog F C st arms exp xarms t -> FrameOk F ->
  value_typed W P H w (ground d st) ->
  exists pat body bs eb binds et,
    I.select_arm xarms w = Some (bs, eb) /\ In (pat, body) arms /\
    A.Branch W Prog F (fold_left (fun C q => A.add_local C (fst q) (snd q) A.LLet) binds C) body exp eb et /\
    A.fits et t /\ Forall (fun q => A.Named F (fst q)) binds /\ NoDup (map fst binds) /\
    locals_typed W P H bs (map (fun q => (A.var_of F (fst q), ground d (snd q))) binds).

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

Lemma pre_after : forall F C d types H g st H' g' st',
  Pre F C d types H g st -> heap_extends H H' -> state_ok H' st' ->
  locals_typed W P H' g' types -> Pre F C d types H' g' st'.
Proof.
  intros F C d types H g st H' g' st' [Frame [Ctx [_ [_ Abs]]]] _ State Locals.
  exact (conj Frame (conj Ctx (conj Locals (conj State Abs)))).
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

Lemma outcome_now : forall F C d types H g st t (breaks : Prop) w,
  Pre F C d types H g st -> value_typed W P H w (ground d t) ->
  Outcome F d H types t breaks (I.Evaluation (Normal w) g st).
Proof.
  intros F C d types H g st t breaks w [_ [_ [Locals [State _]]]] Typed.
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

Lemma pre_loop : forall F C d types H g st,
  Pre F C d types H g st -> Pre (A.with_loop F) C d types H g st.
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

Lemma slook_ground : forall d n (fs : list (string * ty)),
  A.slook n (map (fun q => (fst q, ground d (snd q))) fs) = option_map (ground d) (A.slook n fs).
Proof. intros d n fs; induction fs as [|[g t] r IH]; cbn; [reflexivity|]. destruct (String.eqb n g); auto. Qed.

Lemma ground_record : forall d fs, ground d (TRecord fs) = TRecord (map (fun q => (fst q, ground d (snd q))) fs).
Proof. intros d fs; unfold ground; apply subst_record. Qed.

Lemma ground_nom : forall d n xs, ground d (TNom n xs) = TNom n (PublicTypeRead.smap (type_meanings d) xs).
Proof. intros d n xs; unfold ground; apply subst_nom. Qed.

Lemma nominal_value : forall H w n xs, value_typed W P H w (TNom n xs) -> exists inner, w = VTag n inner.
Proof. intros H w n xs Typed; inversion Typed; subst; eauto. Qed.

Lemma record_value : forall H w fs, value_typed W P H w (TRecord fs) ->
  exists values, w = VRecord values /\ fields_typed W P H values fs.
Proof. intros H w fs Typed; inversion Typed; subst; eauto. Qed.

(* A block without a tail yields () or diverges. *)
Lemma tailless_block : forall F C ss tl exp x t made,
  A.Blk W Prog F C ss tl exp x t made -> tl = None -> t = TUnit \/ t = A.never.
Proof.
  intros F C ss tl exp x t made D. induction D; intros Tail; try discriminate; auto.
  destruct (ty_eqb vt A.never); auto.
Qed.

Lemma fits_refl : forall t, A.fits t t.
Proof. left; reflexivity. Qed.

Lemma fits_never : forall t, A.fits A.never t.
Proof. right; left; reflexivity. Qed.

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
  Pre F C d types H g st ->
  Outcome F d H types TUnit (A.Breaks c) (I.eval j P d g st (EWhile cx bx)).
Proof.
  intros k Bel j. induction j as [|j IH]; intros Le F C c cx b bx bt d types H g st Cond Body Pre0;
    [exact Logic.I|].
  destruct (Bel j ltac:(lia)) as [_ [_ [SBlkE [_ SCond]]]].
  destruct (SCond F C c cx Cond d types H g st Pre0) as [bsym [Lang OutC]].
  cbn [I.eval]. destruct (I.eval j P d g st cx) as [sig g1 st1] eqn:EC.
  destruct sig; try (eapply outcome_pass with (t := TNom bsym []); [exact (fun x => x)|cbn; congruence|exact OutC]).
  destruct OutC as [H1 [E1 [S1 [L1 T1]]]].
  assert (Bool : value_typed W (program_of Prog argv read) H1 v (TNom bsym [])) by exact T1.
  destruct (B.truthy_typed W Prog argv read H1 bsym v Bools Layouts Lang Bool) as [bit Bit].
  replace (I.truthy P v) with (Some bit) by (rewrite truthy_same; symmetry; exact Bit). destruct bit.
  - pose proof (pre_after _ _ _ _ _ _ _ H1 g1 st1 Pre0 E1 S1 L1) as Pre1.
    pose proof (SBlkE (A.with_loop F) C b None bx bt Body d types H1 g1 st1 (pre_loop _ _ _ _ _ _ _ Pre1)) as OutB.
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
  Pre F C d types H g st ->
  Outcome F d H types TUnit False (I.eval j P d g st (ELoop bx)) /\
  (forall w, I.evaluation_signal (I.eval j P d g st (ELoop bx)) = Normal w -> A.BlockBreaks b).
Proof.
  intros k Bel j. induction j as [|j IH]; intros Le F C b bx bt d types H g st Body Pre0;
    [split; [exact Logic.I|cbn; discriminate]|].
  destruct (Bel j ltac:(lia)) as [_ [_ [SBlkE _]]].
  pose proof (SBlkE (A.with_loop F) C b None bx bt Body d types H g st (pre_loop _ _ _ _ _ _ _ Pre0)) as OutB.
  cbn [I.eval]. destruct (I.eval j P d g st bx) as [sig g2 st2] eqn:EB.
  destruct sig; cbn in OutB |- *; try (split; [exact Logic.I|discriminate]).
  - destruct OutB as [H2 [E2 [S2 [L2 _]]]].
    destruct (IH ltac:(lia) F C b bx bt d types H2 g2 st2 Body
      (pre_after _ _ _ _ _ _ _ _ _ _ Pre0 E2 S2 L2)) as [Out Normal'].
    split; [eapply outcome_heap; eassumption|exact Normal'].
  - destruct OutB as [H2 [ret [R [E2 [S2 [L2 T2]]]]]].
    split; [|discriminate]. exists H2, ret; repeat split; auto.
  - destruct OutB as [Brk [_ [H2 [E2 [S2 L2]]]]].
    split; [exists H2; repeat split; auto; constructor|intros; exact Brk].
  - contradiction.
Qed.

(* Arguments evaluated in order: typed values, or a non-Normal outcome. *)
Definition ArgSafe F C (breaks : Prop) (e : expr) (t : ty) : Prop :=
  exists a exp t', A.Ex W Prog F C a exp e t' /\ A.fits t' t /\ (A.Breaks a -> breaks).

Lemma args_safe : forall k, Below k -> forall j, j <= k ->
  forall F C (breaks : Prop) es tys d types H g st,
  Forall2 (ArgSafe F C breaks) es tys -> Pre F C d types H g st ->
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
    pose proof (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits
      (SEx F C a exp e t' D d types H g st Pre0)) as Out.
    destruct (I.eval j P d g st e) as [sig g1 st1] eqn:E.
    destruct sig; cbn in Out;
      try (split; [eapply outcome_pass; [exact Brk|cbn; congruence|exact Out]|cbn; congruence]).
    destruct Out as [H1 [E1 [S1 [L1 T1]]]].
    pose proof (IH ltac:(lia) F C breaks es tys d types H1 g1 st1 Rest
      (pre_after _ _ _ _ _ _ _ _ _ _ Pre0 E1 S1 L1)) as Next.
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

Lemma fits_into_never : forall t, A.fits t A.never -> t = A.never.
Proof.
  intros t [E|[E|[ms [Ms Incl]]]]; auto.
  unfold A.never in Ms. injection Ms as <-.
  destruct t as [m|fs| |g|n xs|parts]; cbn in Incl;
    try (exfalso; apply (Incl _ (or_introl eq_refl))).
  destruct parts as [|p ps]; [reflexivity|exfalso; apply (Incl p); left; reflexivity].
Qed.

Lemma join_fits : forall a b exp t bt,
  A.Join a b exp t -> A.fits bt a -> A.opt_fits bt exp -> A.opt_fits b exp ->
  A.fits bt t /\ A.fits b t.
Proof.
  intros a b exp t bt Join FitsA FitsBt FitsB. destruct Join as [a b e|b|a b [Never|Same] NotNever].
  - split; assumption.
  - rewrite (fits_into_never _ FitsA). split; [apply fits_never|apply fits_refl].
  - subst b. split; [exact FitsA|apply fits_never].
  - subst b. split; [exact FitsA|apply fits_refl].
Qed.

Lemma let_breaks : forall st n ann y mu, A.LetStmt st n ann y mu -> A.Breaks y -> A.StmtBreaks st.
Proof. intros st n ann y mu Let B; destruct Let; [apply A.SBLet|apply A.SBVar]; exact B. Qed.

Lemma unit_never_breaks : ~ A.Breaks AEUnit.
Proof. intro B; inversion B. Qed.

Lemma unit_value : forall H w d, value_typed W P H w (ground d TUnit) -> w = VUnit.
Proof. intros H w d T; inversion T; reflexivity. Qed.

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
  split; [exact D|]. split; [exact Fits|].
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
  map fst xfs = map fst ftys -> Pre F C d types H g st ->
  Outcome F d H types (TRecord ftys) (A.Breaks (AERec pa a fs)) (I.eval k P d g st (ERecord xfs)).
Proof.
  intros [|k] Bel F C pa a fs ftys xfs d types H g st Args Names Pre0; [exact Logic.I|].
  cbn [I.eval]. pose proof (args_safe (S k) Bel k ltac:(lia) _ _ _ _ _ d types H g st Args Pre0) as Out.
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
  A.Ex W Prog F C y (Some tv) v vt -> A.fits vt tv -> Pre F C d types H g st ->
  Outcome F d H types TUnit (A.Breaks y) (I.eval k P d g st (EAssign (A.var_of F n) v)).
Proof.
  intros [|k] Bel F C n y tv v vt d types H g st Found Named D Fits Pre0; [exact Logic.I|].
  destruct (Bel k ltac:(lia)) as [SEx _].
  pose proof (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits (SEx F C y _ v vt D d types H g st Pre0)) as Out.
  pose proof Pre0 as [_ [[Loc _] _]].
  cbn [I.eval]. destruct (I.eval k P d g st v) as [sig g1 st1] eqn:E.
  destruct sig; try (eapply outcome_pass with (t := tv); [exact (fun x => x)|cbn; congruence|exact Out]).
  destruct Out as [H1 [E1 [S1 [L1 T1]]]].
  destruct (locals_assign W P H1 g1 types (A.var_of F n) (ground d tv) v0 L1 (Loc n tv A.LVar Found Named) T1)
    as [g2 [Assign L2]].
  rewrite Assign. exists H1; repeat split; auto. constructor.
Qed.

Section Step.
Variable k : nat.
Hypothesis Bel : Below k.

Lemma sex : SafeEx k. Proof. exact (proj1 (Bel k (le_n k))). Qed.
Lemma sblk : SafeBlk k. Proof. exact (proj1 (proj2 (Bel k (le_n k)))). Qed.
Lemma sblke : SafeBlkE k. Proof. exact (proj1 (proj2 (proj2 (Bel k (le_n k))))). Qed.
Lemma sbranch : SafeBranch k. Proof. exact (proj1 (proj2 (proj2 (proj2 (Bel k (le_n k)))))). Qed.
Lemma scond : SafeCond k. Proof. exact (proj2 (proj2 (proj2 (proj2 (Bel k (le_n k)))))). Qed.

Lemma unit_k : forall F C d types H g st, Pre F C d types H g st ->
  Outcome F d H types TUnit False (I.eval k P d g st EUnit).
Proof.
  intros F C d types H g st Pre0.
  eapply O.outcome_breaks; [|exact (sex F C AEUnit None EUnit TUnit (A.ExUnit W Prog F C None) d types H g st Pre0)].
  exact unit_never_breaks.
Qed.

(* Sequencing: a Normal first result continues with a second evaluation. *)
Lemma outcome_seq : forall F C d types H g st t1 t (b1 b : Prop) r1 (k2 : value -> locals -> Prims.mstate -> I.evaluation),
  Pre F C d types H g st -> (b1 -> b) ->
  Outcome F d H types t1 b1 r1 ->
  (forall H1 g1 st1 w, heap_extends H H1 -> Pre F C d types H1 g1 st1 ->
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
  intros F C d types H g st t1 t b1 b [[] g1 st1] k2 Pre0 Weaken Out Next;
    try (eapply outcome_pass; [exact Weaken|cbn; congruence|exact Out]).
  destruct Out as [H1 [E1 [S1 [L1 T1]]]].
  eapply outcome_heap; [exact E1|]. apply (Next H1 g1 st1 v E1); [|exact T1].
  eapply pre_after; eassumption.
Qed.

Fixpoint ex_step F C a exp x t (D : A.Ex W Prog F C a exp x t) {struct D} :
  forall d types H g st, Pre F C d types H g st ->
  Outcome F d H types t (A.Breaks a) (I.eval (S k) P d g st x)
with blk_step F C ss tl exp x t made (D : A.Blk W Prog F C ss tl exp x t made) {struct D} :
  forall d types H g st, Pre F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks (ABlock ss tl)) (I.eval (S k) P d g st x)
with blke_step F C b exp x t (D : A.BlkE W Prog F C b exp x t) {struct D} :
  forall d types H g st, Pre F C d types H g st ->
  Outcome F d H types t (A.BlockBreaks b) (I.eval (S k) P d g st x)
with branch_step F C y exp x t (D : A.Branch W Prog F C y exp x t) {struct D} :
  forall d types H g st, Pre F C d types H g st ->
  Outcome F d H types t (A.Breaks y) (I.eval (S k) P d g st x)
with cond_step F C c x (D : A.Cond W Prog F C c x) {struct D} :
  forall d types H g st, Pre F C d types H g st ->
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
    + (* unit tag *) cbn [I.eval]. pose proof (unit_k F C d types H lg lst Pre0) as Out.
      apply (outcome_seq F C d types H lg lst TUnit _ False _ _ _ Pre0 (fun f => False_ind _ f) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      apply outcome_tag; [exact S1|exact L1|].
      apply (tag_check d H1 s [] TUnit w); [right; repeat split; exact K|exact T1].
    + (* val *) exact (val_case k Bel F C pa s dcl n t0 t NotLocal R Decl Ast Val d types H lg lst Pre0).
    + (* const *)
      destruct Init as [F0 [t' [Dx [Fits [Mod [Ret [This [Loop [Fresh [App [Pins Num]]]]]]]]]]].
      eapply (outcome_const F0); [exact Ret|exact Loop|].
      apply (O.outcome_fits W P state_ok _ _ _ _ t'); [exact Fits|].
      apply (ex_step F0 (A.Cx [] [] [] []) i (Some t) x t' Dx).
      destruct Pre0 as [_ [_ [L [S Abs]]]]. split; [|split; [|split; [exact L|split; [exact S|]]]].
      * apply (numbering_frame F0 []). rewrite This. exact Num.
      * split; [intros n' t1 mu Found; discriminate|intros t1 This'; congruence].
      * intros t1. unfold A.canon. cbn. rewrite app_subst_nil. reflexivity.
    + (* tag *) subst args. cbn [I.eval].
      pose proof (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits (sex F C y _ v vty Dy d types H lg lst Pre0)) as Out.
      apply (outcome_seq F C d types H lg lst pay _ (A.Breaks y) _ _ _ Pre0
        (fun B => A.BkCall pa a [y] (or_introl eq_refl) B) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      apply outcome_tag; [exact S1|exact L1|].
      apply (tag_check d H1 s xs pay w); [left; split; [exact K|exact Pay]|exact T1].
    + exact (call_defined_case k Bel F C pa args f Cf ns sm sg xs wrap C' R K Def Ctx Dem Sat Sig Args d types H lg lst Pre0).
    + exact (call_abstract_case k Bel F C pa args f sg xs R K Abs Fn Args d types H lg lst Pre0).
    + exact (method_case k Bel F C o pa args ox ot x t Do Dm d types H lg lst Pre0).
    + exact (unary_case k Bel F C op y lib n m ox ot x t Op Lang Dy Head Prov d types H lg lst Pre0).
    + exact (binary_case k Bel F C op l r lib n m ox ot x t Op Lang Dl Head Prov d types H lg lst Pre0).
    + exact (literal_case k Bel F C text x t Num Lit d types H lg lst Pre0).
    + (* if-then *) cbn [I.eval].
      destruct (scond F C c cx' Dc d types H lg lst Pre0) as [bsym [Lang Out]].
      apply (outcome_seq F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => A.BkIfC b None B) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      destruct (B.truthy_typed W Prog argv read H1 bsym w Bools Layouts Lang T1) as [bit Bit].
      replace (I.truthy P w) with (Some bit) by (rewrite truthy_same; symmetry; exact Bit).
      destruct bit.
      * apply (O.outcome_fits W P state_ok _ _ _ _ bt).
        { destruct Bt as [-> | ->]; [apply fits_refl|apply fits_never]. }
        eapply O.outcome_breaks; [|exact (sblke F C b exp bx bt Db d types H1 g1 st1 Pre1)].
        intro B; apply A.BkIfB; exact B.
      * eapply O.outcome_breaks; [|exact (unit_k F C d types H1 g1 st1 Pre1)]. intros [].
    + (* if-else *) cbn [I.eval].
      destruct (scond F C c cx' Dc d types H lg lst Pre0) as [bsym [Lang Out]].
      apply (outcome_seq F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => A.BkIfC b (Some y) B) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      destruct (B.truthy_typed W Prog argv read H1 bsym w Bools Layouts Lang T1) as [bit Bit].
      replace (I.truthy P w) with (Some bit) by (rewrite truthy_same; symmetry; exact Bit).
      destruct (blke_branch_ty _ _ _ _ _ _ Db) as [FitsA FitsBt].
      destruct (join_fits _ _ _ _ _ Join FitsA FitsBt FitsE) as [Left Right].
      destruct bit.
      * apply (O.outcome_fits W P state_ok _ _ _ _ bt); [exact Left|].
        eapply O.outcome_breaks; [|exact (sblke F C b exp bx bt Db d types H1 g1 st1 Pre1)].
        intro B; apply A.BkIfB; exact B.
      * apply (O.outcome_fits W P state_ok _ _ _ _ et); [exact Right|].
        eapply O.outcome_breaks; [|exact (sbranch F C y exp ex' et Dy d types H1 g1 st1 Pre1)].
        intro B; apply A.BkIfE; exact B.
    + (* match *) cbn [I.eval].
      pose proof (sex F C sc None sx st Ds d types H lg lst Pre0) as Out.
      apply (outcome_seq F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => A.BkMatchS arms B) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      pose proof Pre1 as [Frame [Ctx [L1 [S1 Abs]]]].
      assert (T1' : value_typed W P H1 w (ground d (A.canon C st))) by (rewrite Abs; exact T1).
      destruct (arms_select F C _ arms exp xarms t d H1 w Darms Frame T1')
        as [pat [body [bs [eb [binds [et [Sel [InArm [Dbr [FitsT [NamedB [Distinct LB]]]]]]]]]]]].
      rewrite Sel.
      assert (Len : List.length bs = List.length (map (fun q => (A.var_of F (fst q), ground d (snd q))) binds))
        by exact (Forall2_length LB).
      apply (outcome_popn F d H1 types _ _ t _ _ (eq_sym Len)).
      apply (O.outcome_fits W P state_ok _ _ _ _ et); [exact FitsT|].
      eapply O.outcome_breaks; [|apply (sbranch F _ body exp eb et Dbr d _ H1 (bs ++ g1)%list st1)].
      * intro B. eapply A.BkMatchA; [exact InArm|exact B].
      * split; [exact Frame|]. split; [exact (ctx_locals_push_distinct F binds C d types Frame Ctx NamedB Distinct)|].
        split; [apply Forall2_app; [exact LB|exact L1]|]. split; [exact S1|].
        intros t1. unfold A.canon. rewrite add_locals_sub. exact (Abs t1).
    + (* block *)
      destruct (blke_branch_ty _ _ _ _ _ _ Db) as [_ FitsExp].
      apply (O.outcome_fits W P state_ok _ _ _ _ t).
      { destruct exp; [exact FitsExp|apply fits_refl]. }
      eapply O.outcome_breaks; [|exact (blke_step F C b exp x t Db d types H lg lst Pre0)].
      intro B; apply A.BkBlock; exact B.
    + (* return () *) cbn [I.eval].
      pose proof (unit_k F C d types H lg lst Pre0) as Out.
      destruct (I.eval k P d lg lst EUnit) as [[] g1 st1]; cbn in Out |- *; auto; try contradiction;
        [|destruct Out as [[] _]].
      destruct Out as [H1 [E1 [S1 [L1 T1]]]]. exists H1, TUnit; repeat split; auto.
    + (* return e *) cbn [I.eval].
      pose proof (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits (sex F C e _ v actual De d types H lg lst Pre0)) as Out.
      destruct (I.eval k P d lg lst v) as [[] g1 st1]; cbn in Out |- *; auto; try contradiction.
      * destruct Out as [H1 [E1 [S1 [L1 T1]]]]. exists H1, expected; repeat split; auto.
      * destruct Out as [B Rest]. split; [apply A.BkReturn; exact B|exact Rest].
    + (* break *) cbn [I.eval]. pose proof Pre0 as [_ [_ [L [S _]]]].
      split; [apply A.BkBreak|]. split; [exact Loop|]. exists H; repeat split; auto. apply extends_refl.
    + (* record *) subst pay. cbn [I.eval].
      destruct (record_args F C pa a fs ftys xfs Fields) as [Args Names].
      pose proof (record_eval k Bel F C pa a fs ftys xfs d types H lg lst Args Names Pre0) as Out.
      apply (outcome_seq F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => B) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      apply outcome_tag; [exact S1|exact L1|].
      apply (tag_check d H1 s xs (TRecord ftys) w); [left; split; [exact K|exact Pay]|exact T1].
    + (* field *) cbn [I.eval].
      pose proof (sex F C o None ox ot Do d types H lg lst Pre0) as Out.
      apply (outcome_seq F C d types H lg lst _ _ _ _ _ _ Pre0 (fun B => A.BkField n B) Out).
      intros H1 g1 st1 w E1 [_ [_ [L1 [S1 _]]]] T1.
      destruct Fld as [ftys [-> Look]].
      assert (Rec : exists values, I.get_field n w = A.slook n values /\
        fields_typed W P H1 values (map (fun q => (fst q, ground d (snd q))) ftys)).
      { destruct Rcd as [[s [xs [-> [K Pay]]]]|Same].
        - rewrite ground_nom in T1. destruct (nominal_value _ _ _ _ T1) as [inner ->].
          destruct (tagged_payload_typed W P H1 s _ inner K T1) as [payload [Pay' Inner]].
          pose proof (payload_det W _ _ _ _ Pay' (payload_subst W (type_meanings d) _ _ _ Pay)) as ->.
          change (app_subst (type_meanings d) (TRecord ftys)) with (ground d (TRecord ftys)) in Inner.
          rewrite ground_record in Inner.
          destruct (record_value _ _ _ Inner) as [values [-> Fs]].
          exists values; split; [cbn; apply assoc_slook|exact Fs].
        - rewrite <- Same, ground_record in T1. destruct (record_value _ _ _ T1) as [values [-> Fs]].
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
      pose proof (O.outcome_fits W P state_ok _ _ _ _ _ _ _ _ Fits0 (sex F C y _ v vt Dy d types H lg lst Pre0)) as Out.
      apply (outcome_seq F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block st0 rest tl (let_breaks _ _ _ _ _ Let B)) Out).
      intros H1 g1 st1 w E1 [Frame [Ctx [L1 [S1 Abs]]]] T1.
      eapply outcome_pop. eapply O.outcome_breaks; [apply block_breaks_cons|].
      apply (sblk F _ rest tl exp xr t made Drest d _ H1 _ st1).
      split; [exact Frame|]. split; [exact (ctx_locals_push F C d types n t0 mu Frame Ctx Named)|].
      split; [exact (locals_push W P H1 g1 types _ w _ L1 T1)|]. split; [exact S1|exact Abs].
    + (* assign *) cbn [I.eval].
      pose proof (assign_eval k Bel F C n y tv v vt d types H lg lst Found Named Dy Fits Pre0) as Out.
      apply (outcome_seq F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ rest tl (A.SBAssign n B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest d types H1 g1 st1 Pre1)].
    + (* last expression *) cbn [I.eval].
      pose proof (sex F C y None v vt Dy d types H lg lst Pre0) as Out.
      apply (outcome_seq F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ [] None (A.SBExpr B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      destruct (ty_eqb vt A.never) eqn:Never.
      * apply PublicTypeFacts.ty_eqb_spec in Never. subst vt. exfalso; exact (ground_never d H1 w T1).
      * eapply O.outcome_breaks; [|exact (unit_k F C d types H1 g1 st1 Pre1)]. intros [].
    + (* expression *) cbn [I.eval].
      pose proof (sex F C y None v vt Dy d types H lg lst Pre0) as Out.
      apply (outcome_seq F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ rest tl (A.SBExpr B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest d types H1 g1 st1 Pre1)].
    + (* while *) cbn [I.eval].
      pose proof (while_safe k Bel k (le_n k) F C c cx' b bx bt d types H lg lst Dc Db Pre0) as Out.
      apply (outcome_seq F C d types H lg lst _ _ _ _ _ _ Pre0
        (fun B => stmt_breaks_block _ rest tl (A.SBWhile b B)) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest d types H1 g1 st1 Pre1)].
    + (* loop *) cbn [I.eval].
      pose proof (proj1 (loop_safe k Bel k (le_n k) F C b bx bt d types H lg lst Db Pre0)) as Out.
      apply (outcome_seq F C d types H lg lst _ _ _ _ _ _ Pre0 (fun f => False_ind _ f) Out).
      intros H1 g1 st1 w E1 Pre1 T1.
      eapply O.outcome_breaks; [apply block_breaks_cons|exact (sblk F C rest tl exp xr t made Drest d types H1 g1 st1 Pre1)].
    + (* last loop *) cbn [I.eval].
      destruct (loop_safe k Bel k (le_n k) F C b bx bt d types H lg lst Db Pre0) as [Out Normal'].
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
