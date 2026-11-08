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
  PublicBoolTyping PublicBodySafety.
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
  destruct sig; try (eapply outcome_pass; [exact (fun x => x)|cbn; congruence|exact OutC]).
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

End Slice.
End BodySlice.
