(* ------------------------------------------------------------------------ *)
(* BodySoundCore.v — definitions and basic facts for body judgments      *)
(* (Track A, elaborate_sound step 2).                                        *)
(*                                                                           *)
(* The architecture.  Elaboration is one run of the state monad from the    *)
(* initial state to a final state sF, whose program P and field table are   *)
(* what the relation reads (W = W_read w R Fl, ReqsTotal.elaborate_world_of).*)
(* A derivation made for a call that ran from s to s' may use only what     *)
(* the final state still holds of s': `Later s'` (the world, field numbers, *)
(* emitted bodies of fresh symbols, what was reported unsupported).  Each   *)
(* synth-group function is related to its judgment under                     *)
(*                                                                           *)
(*   CxRel c e F C : the elaborator's checker context c and environment e   *)
(*                   correspond to the relation's frame F and context C      *)
(*   VarsIn s' F   : the locals numbered so far (st_vars) are F's numbering *)
(*                                                                           *)
(* where F's numbering is the one the body ends with: local_var only appends *)
(* to st_vars, and with_vars restores it, so a number given is never       *)
(* changed (vars_prefix).  Types translate by `tr Fl` (ety_ty computes the  *)
(* same, ety_ty_tr).                                                         *)
(*                                                                           *)
(* Proved (bodies_sound): for every fuel, synth, synth_block, check_stmts, *)
(* check_stmt, check_bool, synth_if and synth_path are related to Ex, BlkE, *)
(* Blk, Cond and Ex of `if`/paths, given `Pending`, which lists exactly the *)
(* constructs not yet proved, each as a step from the hypothesis at fuel f  *)
(* to fuel f+1:                                                             *)
(*   pd_synth  calls, fields, method calls, records, tags, match,           *)
(*             operators, literals;                                         *)
(*   pd_stmt   bind statements (and so functors, lambdas, providers);       *)
(*   pd_const  a defined val's inlined initializer (ExConst);               *)
(*   pd_ty     elab_type against TyOf.                                      *)
(* Proved outright: unit, `this`, locals, units, vals in force, `return`,   *)
(* `break`, blocks (with the escape check), `let`/`var`, assignment, `while`,*)
(* `loop` (BlkLoopLast), expression statements, `if` with and without      *)
(* `else` (Branch, Join), conditions, with canonical merge readings. The   *)
(* section hypothesis Hbool (lib/bool.moss's `Bool` is                      *)
(* its own declaration) is still to be discharged.                          *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ReqsSolve ElabFrameCore LambdaLocals.
From NM Require Export BodyTr.
From NM Require Import Merge CanonTypes NominalSupport NominalMergeSupport FunctorDecl.
From NM Require CoreMoss.
From NM Require Import InfoInvariant.
From NM Require Export ElabInv FwdInvariant FinalForwarders FinalLambdas FnSnapshot.
Import ListNotations.
Local Open Scope list_scope.

Module A := Audit.Elaboration.

Local Strategy opaque [Nat.of_num_uint FUEL].

(* ======================================================================== *)
(* 1. Elaborated types as Part V's types                                     *)
(* ======================================================================== *)

Lemma skipn_len_app : forall A (l m : list A), skipn (List.length l) (l ++ m) = m.
Proof. intros A l m. induction l; [reflexivity | exact IHl]. Qed.

Lemma slook_sassoc : forall A n (l : list (string * A)), A.slook n l = sassoc n l.
Proof. intros A n l. induction l as [|[k v] r IH]; [reflexivity|]. cbn. rewrite IH. reflexivity. Qed.

Lemma nlook_nassoc : forall A n (l : list (nat * A)), A.nlook n l = nassoc n l.
Proof. intros A n l. induction l as [|[k v] r IH]; [reflexivity|]. cbn. rewrite IH. reflexivity. Qed.

Lemma sassoc_aset : forall V n k (v : V) l,
  sassoc n (aset String.eqb k v l) = if String.eqb n k then Some v else sassoc n l.
Proof.
  intros V n k v l. induction l as [|[k' v'] r IH]; cbn.
  - destruct (String.eqb n k); reflexivity.
  - destruct (String.eqb_spec k k') as [<-|Hne]; cbn.
    + destruct (String.eqb n k); reflexivity.
    + destruct (String.eqb_spec n k') as [->|]; [|exact IH].
      destruct (String.eqb_spec k' k); [congruence | reflexivity].
Qed.

Lemma sassoc_app_some : forall V n (l l' : list (string * V)) v, sassoc n l = Some v -> sassoc n (l ++ l') = Some v.
Proof. intros V n l l' v H. rewrite sassoc_app, H. reflexivity. Qed.

(* ======================================================================== *)
(* 1a. Source syntax: induction through nested lists; `break`               *)
(* ======================================================================== *)

Section AInd.
  Variables (Pe : aexpr -> Prop) (Pb : ablock -> Prop) (Ps : astmt -> Prop).
  Definition Po (o : option aexpr) : Prop := match o with Some y => Pe y | None => True end.
  Hypothesis HUnit : Pe AEUnit.
  Hypothesis HThis : Pe AEThis.
  Hypothesis HPath : forall p a, Pe (AEPath p a).
  Hypothesis HCall : forall p a args, Forall Pe args -> Pe (AECall p a args).
  Hypothesis HField : forall o n, Pe o -> Pe (AEField o n).
  Hypothesis HMeth : forall o p args, Pe o -> Forall Pe args -> Pe (AEMeth o p args).
  Hypothesis HRec : forall p a fs, Forall (fun q => Po (snd q)) fs -> Pe (AERec p a fs).
  Hypothesis HIf : forall c bl el, Pe c -> Pb bl -> Po el -> Pe (AEIf c bl el).
  Hypothesis HMatch : forall sc arms, Pe sc -> Forall (fun q => Pe (snd q)) arms -> Pe (AEMatch sc arms).
  Hypothesis HRet : forall o, Po o -> Pe (AEReturn o).
  Hypothesis HBreak : Pe AEBreak.
  Hypothesis HBlock : forall bl, Pb bl -> Pe (AEBlock bl).
  Hypothesis HUn : forall op y, Pe y -> Pe (AEUnary op y).
  Hypothesis HBin : forall op l r, Pe l -> Pe r -> Pe (AEBinary op l r).
  Hypothesis HLit : forall t, Pe (AELit t).
  Hypothesis HLam : forall ps r y, Pe y -> Pe (AELam ps r y).
  Hypothesis HBlk : forall ss tl, Forall Ps ss -> Po tl -> Pb (ABlock ss tl).
  Hypothesis HLet : forall n t y, Pe y -> Ps (ASLet n t y).
  Hypothesis HVar : forall n t y, Pe y -> Ps (ASVar n t y).
  Hypothesis HAssign : forall n y, Pe y -> Ps (ASAssign n y).
  Hypothesis HBind : forall its, Forall (fun q => Po (snd q)) its -> Ps (ASBind its).
  Hypothesis HWhile : forall c bl, Pe c -> Pb bl -> Ps (ASWhile c bl).
  Hypothesis HLoop : forall bl, Pb bl -> Ps (ASLoop bl).
  Hypothesis HExpr : forall y, Pe y -> Ps (ASExpr y).

  Fixpoint aexpr_ind' (x : aexpr) : Pe x :=
    let po (o : option aexpr) : Po o := match o as o0 return Po o0 with Some y => aexpr_ind' y | None => I end in
    match x with
    | AEUnit => HUnit
    | AEThis => HThis
    | AEPath p a => HPath p a
    | AECall p a args => HCall p a args ((fix go (l : list aexpr) : Forall Pe l :=
        match l with [] => Forall_nil _ | y :: r => Forall_cons _ (aexpr_ind' y) (go r) end) args)
    | AEField o n => HField o n (aexpr_ind' o)
    | AEMeth o p args => HMeth o p args (aexpr_ind' o) ((fix go (l : list aexpr) : Forall Pe l :=
        match l with [] => Forall_nil _ | y :: r => Forall_cons _ (aexpr_ind' y) (go r) end) args)
    | AERec p a fs => HRec p a fs ((fix go (l : list (name * option aexpr)) : Forall (fun q => Po (snd q)) l :=
        match l with [] => Forall_nil _ | q :: r => @Forall_cons _ (fun q => Po (snd q)) q r (po (snd q)) (go r) end) fs)
    | AEIf c bl el => HIf c bl el (aexpr_ind' c) (ablock_ind' bl) (po el)
    | AEMatch sc arms => HMatch sc arms (aexpr_ind' sc) ((fix go (l : list (apat * aexpr)) : Forall (fun q => Pe (snd q)) l :=
        match l with [] => Forall_nil _ | q :: r => @Forall_cons _ (fun q => Pe (snd q)) q r (aexpr_ind' (snd q)) (go r) end) arms)
    | AEReturn o => HRet o (po o)
    | AEBreak => HBreak
    | AEBlock bl => HBlock bl (ablock_ind' bl)
    | AEUnary op y => HUn op y (aexpr_ind' y)
    | AEBinary op l r => HBin op l r (aexpr_ind' l) (aexpr_ind' r)
    | AELit t => HLit t
    | AELam ps r y => HLam ps r y (aexpr_ind' y)
    end
  with ablock_ind' (bl : ablock) : Pb bl :=
    match bl with
    | ABlock ss tl => HBlk ss tl ((fix go (l : list astmt) : Forall Ps l :=
        match l with [] => Forall_nil _ | q :: r => Forall_cons _ (astmt_ind' q) (go r) end) ss)
        (match tl as o0 return Po o0 with Some y => aexpr_ind' y | None => I end)
    end
  with astmt_ind' (st0 : astmt) : Ps st0 :=
    match st0 with
    | ASLet n t y => HLet n t y (aexpr_ind' y)
    | ASVar n t y => HVar n t y (aexpr_ind' y)
    | ASAssign n y => HAssign n y (aexpr_ind' y)
    | ASBind its => HBind its ((fix go (l : list (spec * option aexpr)) : Forall (fun q => Po (snd q)) l :=
        match l with [] => Forall_nil _ | q :: r =>
          @Forall_cons _ (fun q => Po (snd q)) q r (match snd q as o0 return Po o0 with Some y => aexpr_ind' y | None => I end) (go r) end) its)
    | ASWhile c bl => HWhile c bl (aexpr_ind' c) (ablock_ind' bl)
    | ASLoop bl => HLoop bl (ablock_ind' bl)
    | ASExpr y => HExpr y (aexpr_ind' y)
    end.
End AInd.

(* The elaborator's has_break is BlockBreaks (Audit note 42). *)
Lemma existsb_iff : forall A (f : A -> bool) (P : A -> Prop) l,
  Forall (fun x => f x = true <-> P x) l -> (existsb f l = true <-> exists x, In x l /\ P x).
Proof.
  intros A f P l H. induction H as [|x r Hx Hr IH]; cbn.
  - split; [discriminate | intros (y & [] & _)].
  - rewrite orb_true_iff, IH, Hx. split.
    + intros [Hp|(y & Hy & Hp)]; [exists x; auto | exists y; auto].
    + intros (y & [<-|Hy] & Hp); [left; exact Hp | right; exists y; auto].
Qed.

Lemma has_break_spec :
  (forall x, has_break_e x = true <-> A.Breaks x) /\
  (forall bl, has_break_b bl = true <-> A.BlockBreaks bl) /\
  (forall st0, has_break_s st0 = true <-> A.StmtBreaks st0).
Proof.
  assert (Lgo : forall (l : list aexpr), Forall (fun y => has_break_e y = true <-> A.Breaks y) l ->
            ((fix go (l : list aexpr) := match l with [] => false | y :: r => has_break_e y || go r end) l = true
             <-> exists y, In y l /\ A.Breaks y)).
  { intros l H. rewrite <- (existsb_iff _ has_break_e A.Breaks l H). induction l; reflexivity. }
  assert (Lopt : forall (l : list (name * option aexpr)), Forall (fun q => Po (fun y => has_break_e y = true <-> A.Breaks y) (snd q)) l ->
            ((fix go (l : list (name * option aexpr)) :=
               match l with [] => false | (_, Some v) :: r => has_break_e v || go r | (_, None) :: r => go r end) l = true
             <-> exists n y, In (n, Some y) l /\ A.Breaks y)).
  { intros l H. induction H as [|[n [y|]] r Hq Hr IH]; cbn in *.
    - split; [discriminate | intros (? & ? & [] & _)].
    - rewrite orb_true_iff, IH, Hq. split.
      + intros [Hb|(n' & y' & Hin & Hb)]; [exists n, y; auto | exists n', y'; auto].
      + intros (n' & y' & [E|Hin] & Hb); [injection E as -> ->; left; exact Hb | right; exists n', y'; auto].
    - rewrite IH. split; intros (n' & y' & Hin & Hb); exists n', y'; [auto|].
      destruct Hin as [E|Hin]; [discriminate | auto]. }
  assert (Lspec : forall (l : list (spec * option aexpr)), Forall (fun q => Po (fun y => has_break_e y = true <-> A.Breaks y) (snd q)) l ->
            ((fix go (l : list (spec * option aexpr)) :=
               match l with [] => false | (_, Some y) :: r => has_break_e y || go r | (_, None) :: r => go r end) l = true
             <-> exists sp y, In (sp, Some y) l /\ A.Breaks y)).
  { intros l H. induction H as [|[n [y|]] r Hq Hr IH]; cbn in *.
    - split; [discriminate | intros (? & ? & [] & _)].
    - rewrite orb_true_iff, IH, Hq. split.
      + intros [Hb|(n' & y' & Hin & Hb)]; [exists n, y; auto | exists n', y'; auto].
      + intros (n' & y' & [E|Hin] & Hb); [injection E as -> ->; left; exact Hb | right; exists n', y'; auto].
    - rewrite IH. split; intros (n' & y' & Hin & Hb); exists n', y'; [auto|].
      destruct Hin as [E|Hin]; [discriminate | auto]. }
  assert (Larm : forall (l : list (apat * aexpr)), Forall (fun q => has_break_e (snd q) = true <-> A.Breaks (snd q)) l ->
            ((fix go (l : list (apat * aexpr)) := match l with [] => false | (_, y) :: r => has_break_e y || go r end) l = true
             <-> exists pt y, In (pt, y) l /\ A.Breaks y)).
  { intros l H. induction H as [|[pt y] r Hq Hr IH]; cbn in *.
    - split; [discriminate | intros (? & ? & [] & _)].
    - rewrite orb_true_iff, IH, Hq. split.
      + intros [Hb|(n' & y' & Hin & Hb)]; [exists pt, y; auto | exists n', y'; auto].
      + intros (n' & y' & [E|Hin] & Hb); [injection E as -> ->; left; exact Hb | right; exists n', y'; auto]. }
  assert (Lst : forall (l : list astmt), Forall (fun q => has_break_s q = true <-> A.StmtBreaks q) l ->
            ((fix go (l : list astmt) := match l with [] => false | q :: r => has_break_s q || go r end) l = true
             <-> exists q, In q l /\ A.StmtBreaks q)).
  { intros l H. rewrite <- (existsb_iff _ has_break_s A.StmtBreaks l H). induction l; reflexivity. }
  set (Pe := fun x => has_break_e x = true <-> A.Breaks x).
  set (Pb := fun bl => has_break_b bl = true <-> A.BlockBreaks bl).
  set (Ps := fun st0 => has_break_s st0 = true <-> A.StmtBreaks st0).
  Ltac nob := split; [discriminate | intro Hb; inversion Hb].
  assert (H1 : Pe AEUnit) by (unfold Pe; cbn; nob).
  assert (H2 : Pe AEThis) by (unfold Pe; cbn; nob).
  assert (H3 : forall p a, Pe (AEPath p a)) by (intros; unfold Pe; cbn; nob).
  assert (H4 : forall p a args, Forall Pe args -> Pe (AECall p a args)).
  { intros p a args Hf. unfold Pe. cbn. rewrite (Lgo args Hf). split.
    - intros (y & Hy & Hb). eapply A.BkCall; eassumption.
    - intro Hb. inversion Hb; subst. eauto. }
  assert (H5 : forall o n, Pe o -> Pe (AEField o n)).
  { intros o n Ho. unfold Pe in *. cbn. rewrite Ho. split; [intro; constructor; assumption | intro Hb; inversion Hb; assumption]. }
  assert (H6 : forall o p args, Pe o -> Forall Pe args -> Pe (AEMeth o p args)).
  { intros o p args Ho Hf. unfold Pe in *. cbn. rewrite orb_true_iff, Ho, (Lgo args Hf). split.
    - intros [Hb|(y & Hy & Hb)]; [eapply A.BkMethRecv; eassumption | eapply A.BkMethArg; eassumption].
    - intro Hb. inversion Hb; subst; eauto. }
  assert (H7 : forall p a fs, Forall (fun q => Po Pe (snd q)) fs -> Pe (AERec p a fs)).
  { intros p a fs Hf. unfold Pe. cbn. rewrite (Lopt fs Hf). split.
    - intros (n & y & Hy & Hb). eapply A.BkRec; eassumption.
    - intro Hb. inversion Hb; subst. eauto. }
  assert (H8 : forall c bl el, Pe c -> Pb bl -> Po Pe el -> Pe (AEIf c bl el)).
  { intros c bl el Hc Hbl Hel. unfold Pe, Pb in *. cbn. rewrite !orb_true_iff, Hc, Hbl. split.
    - intros [[Hb|Hb]|Hb]; [eapply A.BkIfC; eassumption | eapply A.BkIfB; eassumption|].
      destruct el as [y|]; [|discriminate]. cbn in Hel. apply Hel in Hb. eapply A.BkIfE; eassumption.
    - intro Hb. inversion Hb; subst; auto. right. cbn in Hel. apply Hel. assumption. }
  assert (H9 : forall sc arms, Pe sc -> Forall (fun q => Pe (snd q)) arms -> Pe (AEMatch sc arms)).
  { intros sc arms Hs Hf. unfold Pe in *. cbn. rewrite orb_true_iff, Hs, (Larm arms Hf). split.
    - intros [Hb|(pt & y & Hy & Hb)]; [eapply A.BkMatchS; eassumption | eapply A.BkMatchA; eassumption].
    - intro Hb. inversion Hb; subst; eauto. }
  assert (H10 : forall o, Po Pe o -> Pe (AEReturn o)).
  { intros [y|] Ho; unfold Pe in *; cbn in *; [rewrite Ho; split; [intro; constructor; assumption | intro Hb; inversion Hb; assumption] | nob]. }
  assert (H11 : Pe AEBreak) by (unfold Pe; cbn; split; [intros _; constructor | reflexivity]).
  assert (H12 : forall bl, Pb bl -> Pe (AEBlock bl)).
  { intros bl Hbl. unfold Pe, Pb in *. cbn. rewrite Hbl. split; [intro; constructor; assumption | intro Hb; inversion Hb; assumption]. }
  assert (H13 : forall op y, Pe y -> Pe (AEUnary op y)).
  { intros op y Hy. unfold Pe in *. cbn. rewrite Hy. split; [intro; constructor; assumption | intro Hb; inversion Hb; assumption]. }
  assert (H14 : forall op l r, Pe l -> Pe r -> Pe (AEBinary op l r)).
  { intros op l r Hl Hr. unfold Pe in *. cbn. rewrite orb_true_iff, Hl, Hr. split.
    - intros [Hb|Hb]; [eapply A.BkBinL; eassumption | eapply A.BkBinR; eassumption].
    - intro Hb. inversion Hb; subst; auto. }
  assert (H15 : forall t, Pe (AELit t)) by (intros; unfold Pe; cbn; nob).
  assert (H16 : forall ps r y, Pe y -> Pe (AELam ps r y)).
  { intros ps r y Hy. unfold Pe in *. cbn. rewrite Hy. split; [intro; constructor; assumption | intro Hb; inversion Hb; assumption]. }
  assert (H17 : forall ss tl, Forall Ps ss -> Po Pe tl -> Pb (ABlock ss tl)).
  { intros ss tl Hf Ht. unfold Pb. cbn. rewrite orb_true_iff, (Lst ss Hf). split.
    - intros [(q & Hq & Hb)|Hb]; [eapply A.BBStmt; eassumption|].
      destruct tl as [y|]; [|discriminate]. cbn in Ht. apply Ht in Hb. eapply A.BBTail; eassumption.
    - intro Hb. inversion Hb; subst; [left; eauto | right; cbn in Ht; apply Ht; assumption]. }
  assert (H18 : forall n t y, Pe y -> Ps (ASLet n t y)).
  { intros n t y Hy. unfold Pe, Ps in *. cbn. rewrite Hy. split; [intro; constructor; assumption | intro Hb; inversion Hb; assumption]. }
  assert (H19 : forall n t y, Pe y -> Ps (ASVar n t y)).
  { intros n t y Hy. unfold Pe, Ps in *. cbn. rewrite Hy. split; [intro; constructor; assumption | intro Hb; inversion Hb; assumption]. }
  assert (H20 : forall n y, Pe y -> Ps (ASAssign n y)).
  { intros n y Hy. unfold Pe, Ps in *. cbn. rewrite Hy. split; [intro; constructor; assumption | intro Hb; inversion Hb; assumption]. }
  assert (H21 : forall its, Forall (fun q => Po Pe (snd q)) its -> Ps (ASBind its)).
  { intros its Hf. unfold Ps. cbn. rewrite (Lspec its Hf). split.
    - intros (sp & y & Hy & Hb). eapply A.SBBind; eassumption.
    - intro Hb. inversion Hb; subst. eauto. }
  assert (H22 : forall c bl, Pe c -> Pb bl -> Ps (ASWhile c bl)) by (intros; unfold Ps; cbn; nob).
  assert (H23 : forall bl, Pb bl -> Ps (ASLoop bl)) by (intros; unfold Ps; cbn; nob).
  assert (H24 : forall y, Pe y -> Ps (ASExpr y)).
  { intros y Hy. unfold Pe, Ps in *. cbn. rewrite Hy. split; [intro; constructor; assumption | intro Hb; inversion Hb; assumption]. }
  exact (conj (aexpr_ind' Pe Pb Ps H1 H2 H3 H4 H5 H6 H7 H8 H9 H10 H11 H12 H13 H14 H15 H16 H17 H18 H19 H20 H21 H22 H23 H24)
         (conj (ablock_ind' Pe Pb Ps H1 H2 H3 H4 H5 H6 H7 H8 H9 H10 H11 H12 H13 H14 H15 H16 H17 H18 H19 H20 H21 H22 H23 H24)
               (astmt_ind' Pe Pb Ps H1 H2 H3 H4 H5 H6 H7 H8 H9 H10 H11 H12 H13 H14 H15 H16 H17 H18 H19 H20 H21 H22 H23 H24))).
Qed.

(* ======================================================================== *)
(* 1b. The loader's world, as the relation reads it                          *)
(* ======================================================================== *)

Lemma load_world_kind : forall b w, load_world b = Good w ->
  forall i g, nget (w_syms w) i = Some g -> KindOK g.
Proof.
  intros b w H i g Hg. unfold load_world in H.
  apply rbind_good in H as ([e st] & Hl & H). injection H as <-.
  destruct (load_spec b _ _ _ _ _ _ Hl (linv_init b)) as (L & _).
  exact (li_kind _ _ L i g Hg).
Qed.

Lemma load_world_resolved_sym : forall b w R F, load_world b = Good w ->
  forall q pa x, A.resolve (W_read w R F) q pa = Some (A.ESym x) -> exists g, nget (w_syms w) x = Some g.
Proof.
  intros b w R F H q pa x Hr. unfold load_world in H.
  apply rbind_good in H as ([e st] & Hl & H). injection H as <-.
  destruct (load_spec b _ _ _ _ _ _ Hl (linv_init b)) as (L & _).
  exact (resolved_loader b st e L R F q pa x Hr).
Qed.

(* the declaration W_read gives a loader symbol *)
Lemma w_decl_read : forall w R F x g, nget (w_syms w) x = Some g ->
  A.w_decl (W_read w R F) x = Some (A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g)).
Proof. intros w R F x g H. cbn [A.w_decl W_read]. rewrite H. reflexivity. Qed.

Lemma kind_sym_read : forall b w R F, load_world b = Good w -> forall x g, nget (w_syms w) x = Some g ->
  A.kind_sym (W_read w R F) x = Some (kconv (gs_kind g)).
Proof.
  intros b w R F H x g Hg. unfold A.kind_sym. rewrite (w_decl_read w R F x g Hg). cbn [A.d_ast].
  exact (load_world_kind b w H x g Hg).
Qed.

(* ======================================================================== *)
(* 2. The frame of a run: what the final state keeps                         *)
(* ======================================================================== *)

Definition opt_support (W : A.world) (o : option ety) : Prop := match o with Some t => Support W t | None => True end.

Section Bodies.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).

Definition W : A.world := W_read w R Fl.
Notation trF := (tr Fl).

(* What ExConst asks of a defined val k's inlined initializer, elaborated
   to x at type t (the const_val cache's entries: ConstOK, Later). *)
Definition ConstFact (k : nat) (x : xexpr) (t : ety) : Prop :=
  forall d n t0 i, A.w_decl W k = Some d -> A.d_ast d = ADVal n t0 (Some i) ->
    A.d_assumes d = [] /\ A.TyOf W (A.d_mod d) [] None t0 (trF t) /\ WFty t /\ Support W t /\
    exists F0 t', A.Ex W P F0 (A.Cx [] [] [] [] [] [] [] []) i (Some (trF t)) x t' /\ A.fits t' (trF t) /\
                  A.fr_mod F0 = A.d_mod d.

(* What the final state still holds of a state s. *)
Record Later (s : st) : Prop := {
  lt_w : st_w s = w;
  lt_flds : forall a n, sassoc a (st_flds s) = Some n -> Fl a = n;
  lt_unsup : st_unsup s = [];
  lt_lang : st_lang s = lang_modules w;
  (* the requirement lists cached so far are the reading's *)
  lt_reqs : forall x l, nget (st_reqs s) x = Some l -> R x = l;
  (* every defined val the cache holds is its initializer (an obligation
     of the whole run: the run's last state's entries, each made by
     const_val from a state before it) *)
  lt_consts : forall k x t, nget (st_consts s) k = Some (x, t) -> ConstFact k x t;
  lt_reqclosed : A.RelevanceClosed W (A.w_reqs W);
  lt_reqnone : forall s, A.kind_sym W s = Some A.DKType \/ A.kind_sym W s = Some A.DKUnit -> A.w_reqs W s = [];
  lt_bool_support : forall bi x, sassoc "bool"%string (lang_modules w) = Some bi ->
    sassoc "Bool"%string (gm_names (get_mod w bi)) = Some x -> Support W (YNom x []);
  lt_forwarders : FinalForwarders w P s;
  lt_lambdas : FinalLambdas P s;
  lt_contexts : exists sc, check_contexts sc = Good (tt, sc) /\ st_w sc = w
}.

Lemma later_back : forall s s', Fr s s' -> Later s' -> Later s.
Proof.
  intros s s' Fs [Lw Lf Lu Ll Lr Lc Lclosed Lnone Lbool Lforwarders Llambdas Lcontexts].
  pose proof(final_lambdas_back P _ _ Fs Llambdas)as LlambdasBack. pose proof(fr_keep _ _ Fs)as Fkeep. destruct Fs as [[Fw _ _ _ _ Ffl [_ _ _ _ (u & Eu) Elang Ecs]] _ Fx]. constructor.
  - congruence.
  - intros a n H. exact (Lf a n (Ffl a n H)).
  - rewrite Lu in Eu. destruct (st_unsup s); [reflexivity | discriminate].
  - congruence.
  - intros x l H. exact (Lr x l (Fx x l H)).
  - intros k x t H. exact (Lc k x t (Ecs k (x, t) H)).
  - exact Lclosed.
  - exact Lnone.
  - exact Lbool.
  - eapply final_forwarders_keep;[exact Fkeep|exact Lforwarders].
  - exact LlambdasBack.
  - exact Lcontexts.
Qed.

(* The locals numbered so far are the frame's. *)
Definition VarsIn (s : st) (F : A.frame) : Prop :=
  forall n k, sassoc n (st_vars s) = Some k -> A.slook n (A.fr_var F) = Some k.

Lemma varsin_frame : forall s F F', A.fr_var F' = A.fr_var F -> VarsIn s F -> VarsIn s F'.
Proof. intros s F F' E H n k Hk. rewrite E. exact (H n k Hk). Qed.

Lemma varsin_back : forall s s' F, Fr s s' -> VarsIn s' F -> VarsIn s F.
Proof.
  intros s s' F Fs H n k Hk. apply H. destruct Fs as [[_ _ _ _ _ _ [(l & El) _ _ _ _ _]] _].
  rewrite El. exact (sassoc_app_some _ _ _ _ _ Hk).
Qed.

(* The checker context and environment, against a frame and a context.
   Without merges for now: en_uf = [] and c_sub = [].  The tables are the
   context's: types, vals, fns and methods in force. *)
Definition opt_wf (o : option ety) : Prop := match o with Some t => WFty t | None => True end.

Record CxRel (c : cx) (e : env) (F : A.frame) (C : A.cx) : Prop := {
  cr_mod_in : In (cx_mod c) (w_order w);
  cr_mod : A.fr_mod F = gm_path (get_mod w (cx_mod c));
  cr_ret : A.fr_ret F = (fun t => trF (canon e t)) (sg_ret (cx_sig c));
  cr_ret_wf : WFty (sg_ret (cx_sig c));
  cr_this : A.fr_this F = option_map (fun t => trF (canon e t)) (cx_this c);
  cr_this_wf : opt_wf (cx_this c);
  cr_loop : A.fr_loop F = negb (Nat.eqb (cx_loop c) 0);
  cr_uf : Tri (en_uf e) /\ UfWF (en_uf e);
  cr_sub : A.c_sub C = suf Fl (en_uf e);
  cr_locals : forall n, A.slook n (A.c_locals C) =
                option_map (fun p => ((fun t => trF (canon e t)) (fst p), tr_mut (snd p))) (sassoc n (en_locals e));
  cr_locals_wf : forall n p, sassoc n (en_locals e) = Some p -> WFty (fst p);
  cr_vals : forall v, A.nlook v (A.c_vals C) = option_map (fun t => trF (canon e t)) (nget (en_vals e) v);
  cr_vals_wf : forall v t, nget (en_vals e) v = Some t -> WFty t;
  cr_ty : forall t, A.nlook t (A.c_ty C) = option_map (fun t => trF (canon e t)) (tn_get e t);
  cr_ty_wf : forall t y, tn_get e t = Some y -> WFty y;
  (* the fns and methods in force (a method at a receiver key tr reads
     apart), as the context's tables hold them *)
  cr_fns : forall f, A.nlook f (A.c_fns C) =
             option_map (fun g => A.FSig (map (fun p => (fun t => trF (canon e t)) (snd p)) (sg_params g)) ((fun t => trF (canon e t)) (sg_ret g))) (nget (en_fns e) f);
  cr_mths : forall k m, ety_ok (canon e k) = true -> A.mlook ((fun t => trF (canon e t)) k) m (A.c_mths C) =
             option_map (fun g => A.FSig (map (fun p => (fun t => trF (canon e t)) (snd p)) (sg_params g)) ((fun t => trF (canon e t)) (sg_ret g))) (get_mth e (canon e k) m);
  cr_fns_clean : forall f sg, nget (en_fns e) f = Some sg -> SigOK sg;
  cr_mths_clean : forall r m sg, get_mth e r m = Some sg -> SigOK sg;
  cr_support : EnvSupport W e;
  cr_ret_support : Support W (sg_ret (cx_sig c));
  cr_this_support : opt_support W (cx_this c);
  cr_apps : forall r m, ety_ok r = true ->
    A.ilook (A.mkey (trF r) m) (A.c_apps C) =
    option_map (fun p => map (fun q => (fst q, option_map trF (snd q))) (fst p))
      (get_app e (KyMth r m));
  cr_vapps : forall v, A.ilook (IVal v) (A.c_apps C) =
    option_map (fun p => map (fun q => (fst q, option_map trF (snd q))) (fst p)) (get_app e (KyVal v));
  cr_fapps : forall f, A.ilook (IFn f) (A.c_apps C) =
    option_map (fun p => map (fun q => (fst q, option_map trF (snd q))) (fst p)) (get_app e (KyFn f));
  cr_funapps : forall u, A.ilook (IFun u) (A.c_apps C) =
    option_map (fun p => map (fun q => (fst q, option_map trF (snd q))) (fst p)) (get_app e (KyFun u));
  cr_funspresent : forall u, is_some (A.nlook u (A.c_funs C)) = is_some (nget (en_funs e) u);
  cr_funfixed : forall u fx x tx,
    A.nlook u (A.c_funs C) = Some fx -> In (x, tx) fx ->
    A.Fixed W u x /\ exists a stamp raw,
      get_app e (KyFun u) = Some (a, stamp) /\
      assoc Nat.eqb x a = Some (Some raw) /\ tx = trF raw;
  cr_pins : A.fr_pins F = cx_pins c;
  cr_applying : A.fr_applying F = cx_applying c;
  cr_fresh : A.fr_fresh F = cx_fresh c;
  cr_fun_decls : forall u fx, A.nlook u (A.c_funs C) = Some fx -> FunctorDecl W u;
  cr_fn_sigs : FnSnapshots W C;
  cr_locals_nodup : NoDup(map fst(en_locals e));
  cr_mth_rows : forall m rows,nget(en_mths e)m=Some rows ->
    NoDup(map fst rows)/\Forall(fun q=>ety_ok(fst q)=true/\canon e(fst q)=fst q)rows;
  cr_mth_domain : forall r m sg,A.mlook r m(A.c_mths C)=Some sg ->
    exists raw,ety_ok raw=true/\canon e raw=raw/\trF raw=r
}.

(* adding a local *)
Lemma cxrel_local : forall c e F C n t mu, CxRel c e F C -> WFty t -> Support W t ->
  CxRel c (set_local e n t mu) F (A.add_local C n (trF (canon e t)) (tr_mut mu)).
Proof.
  intros c e F C n t mu H Wt St. destruct H. constructor; auto.
  - intro x. cbn [A.add_local A.c_locals A.slook set_local en_locals]. rewrite sassoc_aset.
    destruct (String.eqb x n); [reflexivity|]. exact (cr_locals0 x).
  - intros x p Hp. cbn [set_local en_locals] in Hp. rewrite sassoc_aset in Hp.
    destruct (String.eqb x n); [injection Hp as <-; exact Wt | exact (cr_locals_wf0 x p Hp)].
  - exact (env_support_local n mu cr_support0 St).
  - apply set_local_names_nodup;exact cr_locals_nodup0.
Qed.

Lemma cxrel_loop : forall c e F C, CxRel c e F C -> CxRel (cx_in_loop c) e (A.with_loop F) C.
Proof. intros c e F C H. destruct H. constructor; cbn; auto. Qed.

(* ======================================================================== *)
(* 3. Steps                                                                  *)
(* ======================================================================== *)

Lemma okstep : forall A (m : M A) s a s', ok m -> Inv s -> m s = Good (a, s') -> Inv s' /\ Fr s s'.
Proof. intros A m s a s' Hm Hi H. exact (Hm s a s' Hi H). Qed.

Lemma ok_inject_check : forall e t exp, ok (inject_check e t exp).
Proof. intros. unfold inject_check. okt. Qed.
Lemma ok_bool_type : forall mi, ok (bool_type mi).
Proof. intros. unfold bool_type. okt. Qed.
Lemma ok_ety_of : forall t mi tm e this, ok (ety_of t mi tm e this).
Proof. intros. unfold ety_of. apply ok_elab_type. Qed.
Lemma ok_show : forall t, ok (show t).
Proof. intros. unfold show. apply ok_show_go. Qed.

Lemma local_var_spec : forall n s k s', local_var n s = Good (k, s') -> sassoc n (st_vars s') = Some k.
Proof.
  intros n s k s' H. unfold local_var in H. apply bind_inv in H as (s0 & s1 & E0 & H). injection E0 as <- <-.
  destruct (sassoc n (st_vars s)) as [k0|] eqn:E.
  - injection H as <- <-. exact E.
  - apply bind_inv in H as (u & s2 & E1 & H). unfold upd in E1. injection E1 as <- <-. injection H as <- <-.
    cbn. apply sassoc_snoc_new. exact E.
Qed.

Definition Post (s : st) (F : A.frame) : Prop := Later s /\ VarsIn s F.

Lemma post_frame : forall s F F', A.fr_var F' = A.fr_var F -> Post s F -> Post s F'.
Proof. intros s F F' E [L V]. split; [exact L | exact (varsin_frame s F F' E V)]. Qed.

Lemma post_back : forall s s' F, Fr s s' -> Post s' F -> Post s F.
Proof. intros s s' F Fs [L V]. split; [exact (later_back s s' Fs L) | exact (varsin_back s s' F Fs V)]. Qed.

Lemma fold_stmts_app : forall xs ys tx, fold_stmts (xs ++ ys) tx = fold_stmts xs (fold_stmts ys tx).
Proof. intros xs ys tx. induction xs as [|x r IH]; [reflexivity|]. cbn. rewrite IH. reflexivity. Qed.

Variable b : bundle.
Hypothesis Hload : load_world b = Good w.

(* [D45]: the Bool the elaborator reads is lib/bool.moss's own (an open
   obligation of the library's shape: see the report of this file) *)
Hypothesis Hbool : forall bi x, sassoc "bool"%string (lang_modules w) = Some bi ->
  sassoc "Bool"%string (gm_names (get_mod w bi)) = Some x -> A.LangSym W "bool"%string A.NSName "Bool"%string x.

(* a path the elaborator resolves, as the relation does *)
Lemma rpath_rel : forall mi p s r s', st_w s = w -> In mi (w_order w) -> rpath mi p s = Good (r, s') ->
  s' = s /\ A.resolve W (gm_path (get_mod w mi)) p = option_map (ent_of w) r.
Proof.
  intros mi p s r s' Hw Hmi H. unfold rpath in H. rewrite Hw in H.
  destruct (resolve_path w mi p) as [r0|m] eqn:E; [|discriminate]. injection H as <- <-. split; [reflexivity|].
  exact (load_world_resolves b w R Fl Hload mi p r0 Hmi E).
Qed.

(* a symbol read from a resolved path is the loader's *)
Lemma sym_loader : forall q pa x s g s', Inv s -> st_w s = w ->
  A.resolve W q pa = Some (A.ESym x) -> sym x s = Good (g, s') ->
  s' = s /\ nget (w_syms w) x = Some g.
Proof.
  intros q pa x s g s' Hi Hw Hr H. unfold sym in H.
  destruct (load_world_resolved_sym b w R Fl Hload q pa x Hr) as (g0 & Hg0).
  pose proof (iv_loader _ Hi b ltac:(rewrite Hw; exact Hload) x g0 ltac:(rewrite Hw; exact Hg0)) as E.
  rewrite E in H. injection H as <- <-. auto.
Qed.

(* ======================================================================== *)
(* 4. The judgments, function by function                                    *)
(* ======================================================================== *)

Definition SynthOK (f : nat) : Prop :=
  forall c e x exp s v t s', synth f c e x exp s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel c e F C -> opt_wf exp -> opt_support W exp -> Post s' F ->
  A.Ex W P F C x (option_map (fun t => trF (canon e t)) exp) v (trF (canon e t)) /\ WFty t /\ Support W t.

Definition block_bt (bl : ablock) (t : ety) : ety :=
  match bl with ABlock _ None => YUnit | ABlock _ (Some _) => t end.

Definition BlockOK (f : nat) : Prop :=
  forall c e bl exp s v t bt s', synth_block f c e bl exp s = Good ((v, t, bt), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel c e F C -> opt_wf exp -> opt_support W exp -> Post s' F ->
  A.BlkE W P F C bl (option_map (fun t => trF (canon e t)) exp) v (trF (canon e t)) /\ WFty t /\ Support W t /\ bt = block_bt bl t /\
  match bl with ABlock _ None => t = YUnit \/ t = YNever | _ => True end.

(* A statement: what follows it is elaborated in the frame and context it
   leaves; `made` are the generations its binds made. *)
Definition StmtConcl (F : A.frame) (C : A.cx) (st0 : astmt) (xs : list xstmt) (c c1 : cx) (e1 : env) (d : bool) : Prop :=
  exists F1 C1 made, CxRel c1 e1 F1 C1 /\ A.fr_var F1 = A.fr_var F /\ cx_fresh c1 = cx_fresh c ++ made /\
    (forall rest tl exp xr t made2, (rest <> [] \/ tl <> None) -> A.Blk W P F1 C1 rest tl exp xr t made2 ->
       A.Blk W P F C (st0 :: rest) tl exp (fold_stmts xs xr) t (made ++ made2)) /\
    (forall exp, A.Blk W P F C [st0] None exp (fold_stmts xs XUnit) (if d then A.never else TUnit) made).

Definition StmtOK (f : nat) : Prop :=
  forall c e st0 s xs e1 c1 d s', check_stmt f c e st0 s = Good ((xs, e1, c1, d), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel c e F C -> Post s' F -> StmtConcl F C st0 xs c c1 e1 d.

Definition StmtsOK (f : nat) : Prop :=
  forall c e ss s xs e1 c1 d s', check_stmts f c e ss s = Good ((xs, e1, c1, d), s') -> ss <> [] ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel c e F C -> Post s' F ->
  exists F1 C1 made, CxRel c1 e1 F1 C1 /\ A.fr_var F1 = A.fr_var F /\ cx_fresh c1 = cx_fresh c ++ made /\
    (forall y exp tx tt, A.Ex W P F1 C1 y exp tx tt -> A.Blk W P F C ss (Some y) exp (fold_stmts xs tx) tt made) /\
    (forall exp, A.Blk W P F C ss None exp (fold_stmts xs XUnit) (if d then A.never else TUnit) made).

Definition CondOK (f : nat) : Prop :=
  forall c e y s v s', check_bool f c e y s = Good (v, s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel c e F C -> Post s' F -> A.Cond W P F C y v.

Definition IfOK (f : nat) : Prop :=
  forall c e x exp s v t s', synth_if f c e x exp s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel c e F C -> opt_wf exp -> opt_support W exp -> Post s' F ->
  A.Ex W P F C x (option_map (fun t => trF (canon e t)) exp) v (trF (canon e t)) /\ WFty t /\ Support W t.

Definition PathOK (f : nat) : Prop :=
  forall c e p a s v t s', synth_path f c e p a s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel c e F C -> Post s' F ->
  forall exp, A.Ex W P F C (AEPath p a) exp v (trF (canon e t)) /\ WFty t /\ Support W t.

(* What ExConst asks of a defined val's inlined initializer. *)
Definition ConstOK (f : nat) : Prop :=
  forall g s v t s', const_val f g s = Good ((v, t), s') -> Inv s -> InfoInv s -> FwdInv s -> Later s' ->
  nget (w_syms w) (gs_id g) = Some g ->
  forall d n t0 i, A.w_decl W (gs_id g) = Some d -> A.d_ast d = ADVal n t0 (Some i) ->
    A.d_assumes d = [] /\ A.TyOf W (A.d_mod d) [] None t0 (trF t) /\ WFty t /\ Support W t /\
    exists F0 t', A.Ex W P F0 (A.Cx [] [] [] [] [] [] [] []) i (Some (trF t)) v t' /\ A.fits t' (trF t) /\
                  A.fr_mod F0 = A.d_mod d.

Record OK (f : nat) : Prop := {
  bs_synth : SynthOK f;
  bs_block : BlockOK f;
  bs_stmts : StmtsOK f;
  bs_stmt : StmtOK f;
  bs_cond : CondOK f;
  bs_if : IfOK f;
  bs_path : PathOK f;
  bs_const : ConstOK f
}.

(* The constructs this file does not prove yet. *)
Definition pending_expr (x : aexpr) : bool :=
  match x with
  | AECall _ _ _ | AEField _ _ | AEMeth _ _ _ | AERec _ _ _ | AEMatch _ _
  | AEUnary _ _ | AEBinary _ _ _ | AELit _ => true
  | _ => false
  end.

Definition pending_stmt (st0 : astmt) : bool :=
  match st0 with ASBind _ => true | _ => false end.

Record Pending : Prop := {
  pd_synth : forall f, OK f -> forall c e x exp s v t s', pending_expr x = true ->
    synth (S f) c e x exp s = Good ((v, t), s') ->
    forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel c e F C -> opt_wf exp -> opt_support W exp -> Post s' F ->
    A.Ex W P F C x (option_map (fun t => trF (canon e t)) exp) v (trF (canon e t)) /\ WFty t /\ Support W t;
  pd_stmt : forall f, OK f -> forall c e st0 s xs e1 c1 d s', pending_stmt st0 = true ->
    check_stmt (S f) c e st0 s = Good ((xs, e1, c1, d), s') ->
    forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel c e F C -> Post s' F -> StmtConcl F C st0 xs c c1 e1 d;
  pd_const : forall f, OK f -> ConstOK (S f);
  (* type expressions (V.2): elab_type against TyOf *)
  pd_ty : forall ty c e s d s', ety_of ty (cx_mod c) (tymap e) e (cx_this c) s = Good (d, s') ->
    forall F C, Inv s -> CxRel c e F C -> Later s' ->
    A.TyOf W (A.fr_mod F) (A.c_ty C) (A.fr_this F) ty (trF (canon e d)) /\ WFty d /\ Support W d
}.

Lemma ok_zero : OK 0.
Proof.
  constructor; intros until 1; match goal with H : _ = Good _ |- _ => cbn in H; discriminate H end.
Qed.

(* ======================================================================== *)
(* 5. Paths: locals, units, vals in force, defined vals                       *)
(* ======================================================================== *)

Tactic Notation "binv" hyp(H) "as" ident(a) ident(s1) ident(H1) :=
  apply bind_inv in H as (a & s1 & H1 & H).

Ltac noerr H := exfalso;
  solve [repeat (cbn beta iota zeta in H;
                 first [ exact (err_not_good _ _ _ _ _ _ H) | discriminate H
                       | apply bind_inv in H as (? & ? & ? & H) ])].




Lemma join_tr : forall mi e a b0 exp s t s', join mi e a b0 exp s = Good (t,s') ->
  Tri (en_uf e) /\ UfWF (en_uf e) -> WFty a -> WFty b0 -> opt_wf exp ->
  A.Join (trF (canon e a)) (trF (canon e b0))
    (option_map (fun t => trF (canon e t)) exp) (trF (canon e t)) /\ WFty t.
Proof. intros mi e a b0 exp s t s' H [_ U] Wa Wb Wx. exact (join_canon_tr Fl mi e a b0 exp s t s' H U Wa Wb Wx). Qed.

Lemma fits_tr : forall e t x, Tri (en_uf e) /\ UfWF (en_uf e) -> fits e t x = true ->
  A.fits (trF (canon e t)) (trF (canon e x)).
Proof. intros e t x _ H. exact (fits_canon_tr Fl e t x H). Qed.

Lemma opt_fits_lift : forall a c, A.fits a c -> A.opt_fits a (Some c).
Proof. intros a c H. exact H. Qed.

Lemma elab_opt_fits_tr : forall e t exp, Tri (en_uf e) /\ UfWF (en_uf e) -> Elab.opt_fits e t exp = true ->
  A.opt_fits (trF (canon e t)) (option_map (fun t => trF (canon e t)) exp).
Proof. intros e t exp _ H. exact (opt_fits_canon_tr Fl e t exp H). Qed.

Lemma local_named : forall n s k s' s'' F, local_var n s = Good (k, s') -> Fr s' s'' -> Post s'' F ->
  A.Named F n /\ A.var_of F n = k.
Proof.
  intros n s k s' s'' F H Fs Hp. destruct (post_back _ _ _ Fs Hp) as [_ V].
  pose proof (V n k (local_var_spec _ _ _ _ H)) as E. unfold A.Named, A.var_of. rewrite E.
  split; [discriminate | reflexivity].
Qed.

Lemma ok_join : forall mi e a b0 exp, ok (join mi e a b0 exp).
Proof. intros. unfold join. okt. Qed.

Lemma bool_type_spec : forall mi s bt s', bool_type mi s = Good (bt, s') -> Later s ->
  s' = s /\ exists bi x, sassoc "bool"%string (lang_modules w) = Some bi /\
    sassoc "Bool"%string (gm_names (get_mod w bi)) = Some x /\ bt = YNom x [].
Proof.
  intros mi s bt s' H [Lw _ _ Ll]. unfold bool_type in H. binv H as bm s3 Hl. unfold lang in Hl.
  binv Hl as st0 s4 Hg. injection Hg as E1 E2. subst st0 s4. injection Hl as E1 E2. subst bm s3.
  rewrite Ll in H. destruct (sassoc "bool"%string (lang_modules w)) as [bi|] eqn:Eb; [|noerr H].
  binv H as m s5 Hm. unfold the_mod in Hm. injection Hm as E1 E2. subst m s5.
  rewrite Lw in H. destruct (sassoc "Bool"%string (gm_names (get_mod w bi))) as [x|] eqn:Ex; [|noerr H].
  injection H as E1 E2. subst bt s'. split; [reflexivity|]. exists bi, x. auto.
Qed.

Lemma branch_tr : forall bl t, trF (block_bt bl t) = A.branch_ty bl (trF t).
Proof. intros [ss [y|]] t; reflexivity. Qed.

Lemma branch_canon_tr : forall e bl t, trF (canon e (block_bt bl t)) = A.branch_ty bl (trF (canon e t)).
Proof. intros e [ss [y|]] t; reflexivity. Qed.

Lemma block_bt_wf : forall bl t, WFty t -> WFty (block_bt bl t).
Proof. intros [ss [y|]] t W0; cbn; [exact W0 | reflexivity]. Qed.

Lemma block_bt_support : forall bl t, Support W t -> Support W(block_bt bl t).
Proof. intros [ss [y|]] t S; [exact S|constructor]. Qed.

End Bodies.
