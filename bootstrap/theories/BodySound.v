(* ------------------------------------------------------------------------ *)
(* BodySound.v — the elaborator's bodies against Part V's V.6 judgments      *)
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
(* `else` (Branch, Join), conditions.  Scope: environments without merges  *)
(* (en_uf = []); the section hypothesis Hbool (lib/bool.moss's `Bool` is   *)
(* its own declaration) is still to be discharged.                          *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ReqsSolve ElabFrame.
From NM Require CoreMoss.
Import ListNotations.
Local Open Scope list_scope.

Module A := Audit.Elaboration.

Local Strategy opaque [Nat.of_num_uint FUEL].

(* ======================================================================== *)
(* 1. Elaborated types as Part V's types                                     *)
(* ======================================================================== *)

(* An induction principle for ety through its nested lists. *)
Section EtyInd.
  Variable Q : ety -> Prop.
  Hypothesis HU : Q YUnit.
  Hypothesis HN : Q YNever.
  Hypothesis HA : forall g, Q (YAbs g).
  Hypothesis HNom : forall n xs, Forall (fun p => Q (snd p)) xs -> Q (YNom n xs).
  Hypothesis HUn : forall ms, Forall Q ms -> Q (YUnion ms).
  Hypothesis HRec : forall fs, Forall (fun p => Q (snd p)) fs -> Q (YRec fs).

  Fixpoint ety_ind' (t : ety) : Q t :=
    match t with
    | YUnit => HU
    | YNever => HN
    | YAbs g => HA g
    | YNom n xs => HNom n xs ((fix go (l : list (nat * ety)) : Forall (fun p => Q (snd p)) l :=
                                 match l with [] => Forall_nil _ | p :: r => Forall_cons _ (ety_ind' (snd p)) (go r) end) xs)
    | YUnion ms => HUn ms ((fix go (l : list ety) : Forall Q l :=
                             match l with [] => Forall_nil _ | p :: r => Forall_cons _ (ety_ind' p) (go r) end) ms)
    | YRec fs => HRec fs ((fix go (l : list (string * ety)) : Forall (fun p => Q (snd p)) l :=
                            match l with [] => Forall_nil _ | p :: r => Forall_cons _ (ety_ind' (snd p)) (go r) end) fs)
    end.
End EtyInd.

Section Tr.
Variable Fl : string -> nat.

Fixpoint tr (t : ety) : ty :=
  match t with
  | YUnit => TUnit
  | YNever => TUnion []
  | YAbs g => TAbs g
  | YNom n xs => TNom n (map (fun p => (fst p, tr (snd p))) xs)
  | YUnion ms => TUnion (map tr ms)
  | YRec fs => TNom A.rec_head (map (fun p => (Fl (fst p), tr (snd p))) fs)
  end.

Lemma ety_eqb_true : forall a b, ety_eqb a b = true -> a = b.
Proof.
  induction a as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; intros [| |g'|n' ys|ms'|fs'] H; cbn in H;
    try discriminate; try reflexivity.
  - apply Nat.eqb_eq in H. subst. reflexivity.
  - apply andb_true_iff in H as [En H]. apply Nat.eqb_eq in En. subst n'. f_equal.
    revert ys H. induction IH as [|[s t] r Ht Hr IHr]; intros [|[s' t'] ys] H; cbn in H; try (cbn in Ht); try discriminate; [reflexivity|].
    apply andb_true_iff in H as [H Hr']. apply andb_true_iff in H as [Es Et].
    apply Nat.eqb_eq in Es. subst. rewrite (Ht t' Et). f_equal. exact (IHr ys Hr').
  - f_equal. revert ms' H. induction IH as [|t r Ht Hr IHr]; intros [|t' ms'] H; cbn in H; try discriminate; [reflexivity|].
    apply andb_true_iff in H as [Et Hr']. rewrite (Ht t' Et). f_equal. exact (IHr ms' Hr').
  - f_equal. revert fs' H. induction IH as [|[s t] r Ht Hr IHr]; intros [|[s' t'] fs'] H; cbn in H; try (cbn in Ht); try discriminate; [reflexivity|].
    apply andb_true_iff in H as [H Hr']. apply andb_true_iff in H as [Es Et].
    apply String.eqb_eq in Es. subst. rewrite (Ht t' Et). f_equal. exact (IHr fs' Hr').
Qed.

Lemma ety_mem_in : forall t l, ety_mem t l = true -> In t l.
Proof.
  intros t l. induction l as [|x r IH]; cbn; [discriminate|].
  intro H. apply orb_true_iff in H as [H|H]; [left; symmetry; exact (ety_eqb_true _ _ H) | right; exact (IH H)].
Qed.

Definition tr_mut (m : Elab.lmut) : A.lmut :=
  match m with Elab.LLet => A.LLet | Elab.LVar => A.LVar | Elab.LCaptured => A.LCaptured end.

Lemma members_tr : forall t, A.members (tr t) = map tr (Elab.members t).
Proof. intros [| | | | |]; reflexivity. Qed.

(* The types the elaborator makes.  `|` as it writes it: no type it makes
   is a union of nothing, anywhere in it (elab_type, Audit note 42(d)), so
   is_never and the relation's never agree on its types and on the types
   inside them (a record's fields, a tag's arguments: BodyData.v); and a
   union's members are tag or unit applications ([D15]: elab_type, and
   substitution keeps them so), so a `match` reads its heads as the
   relation does (BodyData.v). *)
Fixpoint ety_wf (t : ety) : bool :=
  match t with
  | YUnion [] => false
  | YUnion ms => forallb (fun m => match m with YNom _ _ => ety_wf m | _ => false end) ms
  | YNom _ xs => forallb (fun p => ety_wf (snd p)) xs
  | YRec fs => forallb (fun p => ety_wf (snd p)) fs
  | _ => true
  end.

Definition WFty (t : ety) : Prop := ety_wf t = true.

Lemma never_tr : forall t, WFty t -> (tr t = A.never <-> t = YNever).
Proof.
  intros t W. unfold A.never. split; intro H; [|subst; reflexivity].
  destruct t as [| | |n xs|ms|fs]; cbn in H; try discriminate; [reflexivity|].
  destruct ms as [|m r]; [discriminate W | discriminate].
Qed.

Lemma is_never_tr : forall t, WFty t -> is_never t = ty_eqb (tr t) A.never.
Proof.
  intros t W. destruct (is_never t) eqn:E1, (ty_eqb (tr t) A.never) eqn:E2; try reflexivity.
  - exfalso. destruct t; try discriminate; cbn in E2; discriminate.
  - exfalso. apply CoreMoss.ty_eqb_eq in E2. apply (never_tr t W) in E2. subst. discriminate.
Qed.

(* D17: what the elaborator's fits accepts (without merges), the relation's
   fits accepts. *)
Lemma fits_raw_tr : forall t x, fits_raw t x = true -> A.fits (tr t) (tr x).
Proof.
  intros t x H. unfold fits_raw in H. apply orb_true_iff in H as [H|H]; [apply orb_true_iff in H as [H|H]|].
  - left. rewrite (ety_eqb_true _ _ H). reflexivity.
  - right; left. destruct t; try discriminate. reflexivity.
  - destruct x as [| | | |ms|]; try discriminate. right; right. exists (map tr ms). split; [reflexivity|].
    intros m Hm. rewrite members_tr in Hm. apply in_map_iff in Hm as (m0 & <- & Hm0).
    rewrite forallb_forall in H. apply in_map. apply ety_mem_in. exact (H m0 Hm0).
Qed.

Lemma opt_fits_tr : forall t exp, match exp with Some x => fits_raw t x | None => true end = true ->
  A.opt_fits (tr t) (option_map tr exp).
Proof. intros t [x|] H; cbn; [exact (fits_raw_tr t x H) | exact I]. Qed.

End Tr.

(* A type whose reading by tr no other such type shares (tr_inj, CtxMerge.v):
   no nominal headed by the records' reserved 0, no union of nothing.  The
   receivers methods are filed under are such types (CtxWalk.WS). *)
Fixpoint ety_ok (t : ety) : bool :=
  match t with
  | YNom h xs => negb (Nat.eqb h 0) && forallb (fun p => ety_ok (snd p)) xs
  | YUnion ms => match ms with [] => false | _ => forallb ety_ok ms end
  | YRec fs => forallb (fun p => ety_ok (snd p)) fs
  | _ => true
  end.

(* canon without merges is the identity *)
Lemma canon_go_nil : forall fuel t, canon_go fuel [] t = t.
Proof.
  intros [|f] t; [reflexivity|]. cbn [canon_go].
  induction t as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; try reflexivity.
  - f_equal. induction IH as [|[a y] r Hy Hr IHr]; [reflexivity|]. cbn [map fst snd] in *. rewrite Hy. f_equal. exact IHr.
  - f_equal. induction IH as [|y r Hy Hr IHr]; [reflexivity|]. cbn [map] in *. rewrite Hy. f_equal. exact IHr.
  - f_equal. induction IH as [|[a y] r Hy Hr IHr]; [reflexivity|]. cbn [map fst snd] in *. rewrite Hy. f_equal. exact IHr.
Qed.

Lemma canon_nil : forall e t, en_uf e = [] -> canon e t = t.
Proof. intros e t H. unfold canon. rewrite H. apply canon_go_nil. Qed.

Lemma fits_nil : forall e t x, en_uf e = [] -> fits e t x = fits_raw t x.
Proof. intros e t x H. unfold fits. rewrite !canon_nil by exact H. reflexivity. Qed.

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

Section Bodies.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).

Definition W : A.world := W_read w R Fl.
Notation trF := (tr Fl).

(* What ExConst asks of a defined val k's inlined initializer, elaborated
   to x at type t (the const_val cache's entries: ConstOK, Later). *)
Definition ConstFact (k : nat) (x : xexpr) (t : ety) : Prop :=
  forall d n t0 i, A.w_decl W k = Some d -> A.d_ast d = ADVal n t0 (Some i) ->
    A.d_assumes d = [] /\ A.TyOf W (A.d_mod d) [] None t0 (trF t) /\ WFty t /\
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
  lt_consts : forall k x t, nget (st_consts s) k = Some (x, t) -> ConstFact k x t
}.

Lemma later_back : forall s s', Fr s s' -> Later s' -> Later s.
Proof.
  intros s s' Fs [Lw Lf Lu Ll Lr Lc]. destruct Fs as [[Fw _ _ _ _ Ffl [_ _ _ _ (u & Eu) Elang Ecs]] _ Fx]. constructor.
  - congruence.
  - intros a n H. exact (Lf a n (Ffl a n H)).
  - rewrite Lu in Eu. destruct (st_unsup s); [reflexivity | discriminate].
  - congruence.
  - intros x l H. exact (Lr x l (Fx x l H)).
  - intros k x t H. exact (Lc k x t (Ecs k (x, t) H)).
Qed.

(* The locals numbered so far are the frame's. *)
Definition VarsIn (s : st) (F : A.frame) : Prop :=
  forall n k, sassoc n (st_vars s) = Some k -> A.slook n (A.fr_var F) = Some k.

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
  cr_ret : A.fr_ret F = trF (sg_ret (cx_sig c));
  cr_ret_wf : WFty (sg_ret (cx_sig c));
  cr_this : A.fr_this F = option_map trF (cx_this c);
  cr_this_wf : opt_wf (cx_this c);
  cr_loop : A.fr_loop F = negb (Nat.eqb (cx_loop c) 0);
  cr_uf : en_uf e = [];
  cr_sub : A.c_sub C = [];
  cr_locals : forall n, A.slook n (A.c_locals C) =
                option_map (fun p => (trF (fst p), tr_mut (snd p))) (sassoc n (en_locals e));
  cr_locals_wf : forall n p, sassoc n (en_locals e) = Some p -> WFty (fst p);
  cr_vals : forall v, A.nlook v (A.c_vals C) = option_map trF (nget (en_vals e) v);
  cr_vals_wf : forall v t, nget (en_vals e) v = Some t -> WFty t;
  cr_ty : forall t, A.nlook t (A.c_ty C) = option_map trF (tn_get e t);
  cr_ty_wf : forall t y, tn_get e t = Some y -> WFty y;
  (* the fns and methods in force (a method at a receiver key tr reads
     apart), as the context's tables hold them *)
  cr_fns : forall f, A.nlook f (A.c_fns C) =
             option_map (fun g => A.FSig (map (fun p => trF (snd p)) (sg_params g)) (trF (sg_ret g))) (nget (en_fns e) f);
  cr_mths : forall k m, ety_ok k = true -> A.mlook (trF k) m (A.c_mths C) =
             option_map (fun g => A.FSig (map (fun p => trF (snd p)) (sg_params g)) (trF (sg_ret g))) (get_mth e k m)
}.

(* adding a local *)
Lemma cxrel_local : forall c e F C n t mu, CxRel c e F C -> WFty t ->
  CxRel c (set_local e n t mu) F (A.add_local C n (trF t) (tr_mut mu)).
Proof.
  intros c e F C n t mu H Wt. destruct H. constructor; auto.
  - intro x. cbn [A.add_local A.c_locals A.slook set_local en_locals]. rewrite sassoc_aset.
    destruct (String.eqb x n); [reflexivity|]. exact (cr_locals0 x).
  - intros x p Hp. cbn [set_local en_locals] in Hp. rewrite sassoc_aset in Hp.
    destruct (String.eqb x n); [injection Hp as <-; exact Wt | exact (cr_locals_wf0 x p Hp)].
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

Lemma post_back : forall s s' F, Fr s s' -> Post s' F -> Post s F.
Proof. intros s s' F Fs [L V]. split; [exact (later_back s s' Fs L) | exact (varsin_back s s' F Fs V)]. Qed.

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
  forall F C, Inv s -> CxRel c e F C -> opt_wf exp -> Post s' F ->
  A.Ex W P F C x (option_map trF exp) v (trF t) /\ WFty t.

Definition block_bt (bl : ablock) (t : ety) : ety :=
  match bl with ABlock _ None => YUnit | ABlock _ (Some _) => t end.

Definition BlockOK (f : nat) : Prop :=
  forall c e bl exp s v t bt s', synth_block f c e bl exp s = Good ((v, t, bt), s') ->
  forall F C, Inv s -> CxRel c e F C -> opt_wf exp -> Post s' F ->
  A.BlkE W P F C bl (option_map trF exp) v (trF t) /\ WFty t /\ bt = block_bt bl t /\
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
  forall F C, Inv s -> CxRel c e F C -> Post s' F -> StmtConcl F C st0 xs c c1 e1 d.

Definition StmtsOK (f : nat) : Prop :=
  forall c e ss s xs e1 c1 d s', check_stmts f c e ss s = Good ((xs, e1, c1, d), s') -> ss <> [] ->
  forall F C, Inv s -> CxRel c e F C -> Post s' F ->
  exists F1 C1 made, CxRel c1 e1 F1 C1 /\ A.fr_var F1 = A.fr_var F /\ cx_fresh c1 = cx_fresh c ++ made /\
    (forall y exp tx tt, A.Ex W P F1 C1 y exp tx tt -> A.Blk W P F C ss (Some y) exp (fold_stmts xs tx) tt made) /\
    (forall exp, A.Blk W P F C ss None exp (fold_stmts xs XUnit) (if d then A.never else TUnit) made).

Definition CondOK (f : nat) : Prop :=
  forall c e y s v s', check_bool f c e y s = Good (v, s') ->
  forall F C, Inv s -> CxRel c e F C -> Post s' F -> A.Cond W P F C y v.

Definition IfOK (f : nat) : Prop :=
  forall c e x exp s v t s', synth_if f c e x exp s = Good ((v, t), s') ->
  forall F C, Inv s -> CxRel c e F C -> opt_wf exp -> Post s' F ->
  A.Ex W P F C x (option_map trF exp) v (trF t) /\ WFty t.

Definition PathOK (f : nat) : Prop :=
  forall c e p a s v t s', synth_path f c e p a s = Good ((v, t), s') ->
  forall F C, Inv s -> CxRel c e F C -> Post s' F ->
  forall exp, A.Ex W P F C (AEPath p a) exp v (trF t) /\ WFty t.

(* What ExConst asks of a defined val's inlined initializer. *)
Definition ConstOK (f : nat) : Prop :=
  forall g s v t s', const_val f g s = Good ((v, t), s') -> Inv s -> Later s' ->
  nget (w_syms w) (gs_id g) = Some g ->
  forall d n t0 i, A.w_decl W (gs_id g) = Some d -> A.d_ast d = ADVal n t0 (Some i) ->
    A.d_assumes d = [] /\ A.TyOf W (A.d_mod d) [] None t0 (trF t) /\ WFty t /\
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
    forall F C, Inv s -> CxRel c e F C -> opt_wf exp -> Post s' F ->
    A.Ex W P F C x (option_map trF exp) v (trF t) /\ WFty t;
  pd_stmt : forall f, OK f -> forall c e st0 s xs e1 c1 d s', pending_stmt st0 = true ->
    check_stmt (S f) c e st0 s = Good ((xs, e1, c1, d), s') ->
    forall F C, Inv s -> CxRel c e F C -> Post s' F -> StmtConcl F C st0 xs c c1 e1 d;
  pd_const : forall f, OK f -> ConstOK (S f);
  (* type expressions (V.2): elab_type against TyOf *)
  pd_ty : forall ty c e s d s', ety_of ty (cx_mod c) (tymap e) e (cx_this c) s = Good (d, s') ->
    forall F C, Inv s -> CxRel c e F C -> Later s' ->
    A.TyOf W (A.fr_mod F) (A.c_ty C) (A.fr_this F) ty (trF d) /\ WFty d
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

Lemma path_step : forall f, ConstOK f -> PathOK (S f).
Proof.
  intros f HC c e p a s v t s' H F C Hi Hc [Hl Hv] exp.
  assert (Fs : Fr s s') by exact (proj2 (ok_synth_path (S f) c e p a s _ s' Hi H)).
  assert (Hw : st_w s = w) by (rewrite <- (lt_w _ Hl); symmetry; exact (fr_w _ _ Fs)).
  cbn [synth_path] in H.
  (* the part after the local lookup *)
  assert (Global : forall s0, Inv s0 -> st_w s0 = w -> Fr s0 s' ->
            (forall n', p = [n'] -> sassoc n' (en_locals e) = None) -> a = None ->
            (let! tg := rpath (cx_mod c) p in
             match tg with
             | None => err (cx_mod c) ("`" ++ show_path p ++ "` is not in scope")%string
             | Some (EMod _) => err (cx_mod c) ("`" ++ show_path p ++ "` is a module, not a value")%string
             | Some (ESym s) =>
                 let! g := sym s in
                 match gs_kind g, gs_decl g with
                 | KUnit, _ => ret (XTag s [] XUnit, YNom s [])
                 | KVal, ADVal _ _ (Some _) => const_val f g
                 | KVal, _ => match nget (en_vals e) s with
                              | Some t => ret (XVl s, t)
                              | None => err (cx_mod c) ("`" ++ gs_name g ++ "` is not available in the context here (assume or bind it)")%string
                              end
                 | KFn, _ => err (cx_mod c) ("`" ++ gs_name g ++ "` is a function; call it (no first-class fns yet)")%string
                 | KTag, _ => err (cx_mod c) ("`" ++ gs_name g ++ "` is a tag; construct it with a payload")%string
                 | _, _ => err (cx_mod c) ("`" ++ gs_name g ++ "` is not a value")%string
                 end
             end) s0 = Good ((v, t), s') ->
            A.Ex W P F C (AEPath p a) exp v (trF t) /\ WFty t).
  { intros s0 Hi0 Hw0 Fs0 Hnl -> H0. binv H0 as r s1 Hr. destruct (rpath_rel _ _ _ _ _ Hw0 (cr_mod_in _ _ _ _ Hc) Hr) as [-> Hres].
    assert (Nl : forall n', p = [n'] -> A.slook n' (A.c_locals C) = None)
      by (intros n' E; rewrite (cr_locals _ _ _ _ Hc), (Hnl n' E); reflexivity).
    destruct r as [[x|j]|]; [|noerr H0|noerr H0].
    cbn [option_map ent_of] in Hres. binv H0 as a0 s2 Hs.
    destruct (sym_loader _ _ _ _ _ _ Hi0 Hw0 Hres Hs) as [-> Hg].
    assert (Hrs : A.resolve_sym W (A.fr_mod F) p = Some x)
      by (unfold A.resolve_sym; rewrite (cr_mod _ _ _ _ Hc), Hres; reflexivity).
    pose proof (kind_sym_read b w R Fl Hload x a0 Hg) as Hk. pose proof (load_world_kind b w Hload x a0 Hg) as KO.
    unfold KindOK in KO.
    destruct (gs_kind a0) eqn:Kg; try noerr H0.
    - (* a unit *)
      injection H0 as <- <- <-. split; [|reflexivity].
      eapply A.ExUnitTag; [exact Nl | exact Hrs | unfold W; rewrite Hk; reflexivity].
    - (* a val *)
      destruct (gs_decl a0) as [| | | | |n0 t0 [i|]| | |] eqn:Dg; cbn in KO;
        try (repeat match type of KO with context [match ?x with _ => _ end] => destruct x end; discriminate KO).
      + (* defined: inlined *)
        pose proof (load_world_ids b w Hload x a0 Hg) as Eid.
        assert (Hd : A.w_decl W (gs_id a0) = Some (A.Decl (gm_path (get_mod w (gs_mod a0))) (gs_decl a0) (gs_assumes a0)))
          by (rewrite Eid; exact (w_decl_read w R Fl x a0 Hg)).
        assert (Hg' : nget (w_syms w) (gs_id a0) = Some a0) by (rewrite Eid; exact Hg).
        destruct (HC a0 s0 v t s' H0 Hi0 Hl Hg' _ n0 t0 i Hd ltac:(cbn; exact Dg)) as (Ha & Hty & Wt & F0 & t' & Hx & Hf & Hm).
        split; [|exact Wt].
        eapply A.ExConst; [exact Nl | exact Hrs | rewrite <- Eid; exact Hd | cbn; exact Dg | exact Ha | exact Hty |].
        exists F0, t'. auto.
      + (* in force *)
        destruct (nget (en_vals e) x) as [tv|] eqn:Ev; [|noerr H0]. injection H0 as <- <- <-.
        split; [|exact (cr_vals_wf _ _ _ _ Hc x tv Ev)].
        eapply A.ExVal; [exact Nl | exact Hrs | unfold W; exact (w_decl_read w R Fl x a0 Hg) | cbn; exact Dg |].
        rewrite (cr_vals _ _ _ _ Hc), Ev. reflexivity. }
  destruct a as [bs|].
  - destruct p as [|n [|n2 r]]; cbn in H; noerr H.
  - destruct p as [|n [|n2 r]].
    + apply (Global s Hi Hw Fs); [intros n' E; discriminate E | reflexivity | exact H].
    + destruct (sassoc n (en_locals e)) as [[tl mu]|] eqn:El.
      * (* a local *)
        binv H as a s1 H0. injection H as <- <- <-.
        pose proof (local_var_spec _ _ _ _ H0) as Hk.
        split; [|exact (cr_locals_wf _ _ _ _ Hc n _ El)].
        replace (XVar a) with (XVar (A.var_of F n)).
        -- eapply A.ExLocal; [rewrite (cr_locals _ _ _ _ Hc), El; reflexivity|].
           unfold A.Named. rewrite (Hv n a Hk). discriminate.
        -- unfold A.var_of. rewrite (Hv n a Hk). reflexivity.
      * apply (Global s Hi Hw Fs); [intros n' E; injection E as <-; exact El | reflexivity | exact H].
    + apply (Global s Hi Hw Fs); [intros n' E; discriminate E | reflexivity | exact H].
Qed.

(* ======================================================================== *)
(* 6. Expressions, blocks, statements                                        *)
(* ======================================================================== *)

Lemma gens_tr : forall t, A.gens (trF t) = ety_gens t.
Proof.
  induction t as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; try reflexivity.
  - cbn. induction IH as [|[a y] r Hy Hr IHr]; [reflexivity|]. cbn in *. rewrite Hy. f_equal. exact IHr.
  - cbn. induction IH as [|y r Hy Hr IHr]; [reflexivity|]. cbn in *. rewrite Hy. f_equal. exact IHr.
  - cbn. induction IH as [|[a y] r Hy Hr IHr]; [reflexivity|]. cbn in *. rewrite Hy. f_equal. exact IHr.
Qed.

Lemma nmem_in : forall x l, nmem x l = true <-> In x l.
Proof.
  intros x l. induction l as [|y r IH]; cbn; [split; [discriminate | intros []]|].
  rewrite orb_true_iff, IH, Nat.eqb_eq. split; intros [H|H]; auto.
Qed.

Lemma join_tr : forall mi a b0 exp s t s', join mi a b0 exp s = Good (t, s') -> WFty a -> WFty b0 -> opt_wf exp ->
  A.Join (trF a) (trF b0) (option_map trF exp) (trF t) /\ WFty t.
Proof.
  intros mi a b0 exp s t s' H Wa Wb Wx. unfold join in H. destruct exp as [x|].
  - injection H as <- <-. split; [constructor | exact Wx].
  - destruct (is_never a) eqn:Ea.
    + injection H as <- <-. destruct a; try discriminate. split; [constructor | exact Wb].
    + destruct (is_never b0 || ety_eqb a b0) eqn:Eb; [|noerr H]. injection H as <- <-. split; [|exact Wa].
      apply A.JoinSame.
      * apply orb_true_iff in Eb as [Eb|Eb].
        -- left. destruct b0; try discriminate. reflexivity.
        -- right. rewrite (ety_eqb_true _ _ Eb). reflexivity.
      * intro E. apply (never_tr Fl a Wa) in E. subst. discriminate.
Qed.

Lemma fits_tr : forall e t x, en_uf e = [] -> fits e t x = true -> A.fits (trF t) (trF x).
Proof. intros e t x U H. rewrite fits_nil in H by exact U. exact (fits_raw_tr Fl t x H). Qed.

Lemma opt_fits_lift : forall a c, A.fits a c -> A.opt_fits a (Some c).
Proof. intros a c H. exact H. Qed.

Lemma elab_opt_fits_tr : forall e t exp, en_uf e = [] -> Elab.opt_fits e t exp = true ->
  A.opt_fits (trF t) (option_map trF exp).
Proof.
  intros e t [x|] U H; [|exact I]. unfold Elab.opt_fits in H. cbn beta iota in H.
  apply opt_fits_lift. exact (fits_tr e t x U H).
Qed.

Lemma varsin_frame : forall s F F', A.fr_var F' = A.fr_var F -> VarsIn s F -> VarsIn s F'.
Proof. intros s F F' E H n k Hk. rewrite E. exact (H n k Hk). Qed.

Lemma post_frame : forall s F F', A.fr_var F' = A.fr_var F -> Post s F -> Post s F'.
Proof. intros s F F' E [L V]. split; [exact L | exact (varsin_frame s F F' E V)]. Qed.

(* the local a statement or path names, as the frame numbers it *)
Lemma local_named : forall n s k s' s'' F, local_var n s = Good (k, s') -> Fr s' s'' -> Post s'' F ->
  A.Named F n /\ A.var_of F n = k.
Proof.
  intros n s k s' s'' F H Fs Hp. destruct (post_back _ _ _ Fs Hp) as [_ V].
  pose proof (V n k (local_var_spec _ _ _ _ H)) as E. unfold A.Named, A.var_of. rewrite E.
  split; [discriminate | reflexivity].
Qed.

Ltac stepok H L Hi Hi' Fs' := destruct (okstep _ _ _ _ _ L Hi H) as [Hi' Fs'].

Lemma synth_step : forall f, OK f -> Pending -> SynthOK (S f).
Proof.
  intros f IH PD c e x exp s v t s' H F C Hi Hc Hx Hp.
  destruct (pending_expr x) eqn:Pe; [exact (pd_synth PD f IH c e x exp s v t s' Pe H F C Hi Hc Hx Hp)|].
  cbn [synth] in H. binv H as vt0 s1 H1. destruct vt0 as [v0 t0].
  binv H as u s2 Hinj. injection H as <- <- <-.
  assert (Fin : forall s0, Inv s0 -> Fr s s0 -> s0 = s1 -> Post s1 F).
  { intros s0 Hi0 _ ->. stepok Hinj (ok_inject_check e t0 exp) Hi0 Hi2 Fs2. exact (post_back _ _ _ Fs2 Hp). }
  destruct x; cbn in Pe; try discriminate Pe; cbn beta iota zeta in H1.
  - (* () *) injection H1 as <- <- <-. split; [apply A.ExUnit | reflexivity].
  - (* this *) destruct (cx_this c) as [th|] eqn:Et; [|noerr H1]. injection H1 as <- <- <-.
    split; [apply A.ExThis; rewrite (cr_this _ _ _ _ Hc), Et; reflexivity|].
    pose proof (cr_this_wf _ _ _ _ Hc) as W0. rewrite Et in W0. exact W0.
  - (* a path *) stepok H1 (ok_synth_path f c e p s0) Hi Hi1 Fs1.
    destruct f as [|f']; [cbn [synth_path] in H1; discriminate H1|].
    exact (bs_path _ IH c e p s0 s v0 t0 s1 H1 F C Hi Hc (Fin s1 Hi1 Fs1 eq_refl) _).
  - (* if *) stepok H1 (ok_synth_if f c e (AEIf x a o) exp) Hi Hi1 Fs1.
    exact (bs_if _ IH c e _ exp s v0 t0 s1 H1 F C Hi Hc Hx (Fin s1 Hi1 Fs1 eq_refl)).
  - (* return *) destruct o as [y|].
    + binv H1 as vt s3 Hy. destruct vt as [vy ty].
      stepok Hy (ok_synth f c e y (Some (sg_ret (cx_sig c)))) Hi Hi3 Fs3.
      destruct (fits e ty (sg_ret (cx_sig c))) eqn:Ef; [|noerr H1]. injection H1 as <- <- <-.
      destruct (bs_synth _ IH c e y _ s vy ty s3 Hy F C Hi Hc (cr_ret_wf _ _ _ _ Hc) (Fin s3 Hi3 Fs3 eq_refl)) as [Ey Wy].
      split; [|reflexivity]. eapply A.ExReturn.
      * rewrite (cr_ret _ _ _ _ Hc). exact Ey.
      * rewrite (cr_ret _ _ _ _ Hc). exact (fits_tr e ty _ (cr_uf _ _ _ _ Hc) Ef).
    + destruct (is_unit_or_never (sg_ret (cx_sig c))) eqn:Eu; [|noerr H1]. injection H1 as <- <- <-.
      split; [|reflexivity]. apply A.ExReturnUnit. rewrite (cr_ret _ _ _ _ Hc).
      destruct (sg_ret (cx_sig c)); try discriminate Eu; [left | right]; reflexivity.
  - (* break *) destruct (Nat.eqb (cx_loop c) 0) eqn:El; [noerr H1|]. injection H1 as <- <- <-.
    split; [|reflexivity]. apply A.ExBreak. rewrite (cr_loop _ _ _ _ Hc), El. reflexivity.
  - (* a block *) binv H1 as r3 s3 Hb. destruct r3 as [[vb tb] bt]. injection H1 as <- <- <-.
    stepok Hb (ok_synth_block f c e a exp) Hi Hi3 Fs3.
    destruct (bs_block _ IH c e a exp s vb tb bt s3 Hb F C Hi Hc Hx (Fin s3 Hi3 Fs3 eq_refl)) as (Eb & Wb & _).
    split; [|destruct exp; [exact Hx | exact Wb]].
    destruct exp as [x|]; cbn; apply A.ExBlock in Eb; exact Eb.
  - (* an anonymous function outside a bind *) noerr H1.
Qed.

Lemma block_step : forall f, OK f -> BlockOK (S f).
Proof.
  intros f IH c e bl exp s v t bt s' H F C Hi Hc Hx Hp.
  destruct bl as [ss tl]. cbn [synth_block] in H.
  binv H as r1 s1 H1. destruct r1 as [[[xs e1] c1] d].
  stepok H1 (ok_check_stmts f c e ss) Hi Hi1 Fs1.
  binv H as r2 s2 H2. destruct r2 as [tx tty].
  assert (Fs2 : Fr s1 s2 /\ Inv s2).
  { destruct tl as [y|].
    - stepok H2 (ok_synth f c1 e1 y exp) Hi1 Hi2 Fs2. auto.
    - injection H2 as _ _ <-. split; [apply fr_refl | exact Hi1]. }
  destruct Fs2 as [Fs2 Hi2].
  cbn zeta in H.
  destruct (filter _ _) as [|g gone] eqn:Eg; [|noerr H].
  destruct (Elab.opt_fits e1 tty exp) eqn:Eo; [|noerr H].
  injection H as <- <- <-. subst s'.
  assert (Hp1 : Post s1 F) by exact (post_back _ _ _ Fs2 Hp).
  (* the statements *)
  assert (Hst : exists F1 C1 made, CxRel c1 e1 F1 C1 /\ A.fr_var F1 = A.fr_var F /\ cx_fresh c1 = cx_fresh c ++ made /\
    (forall y exp tx tt, A.Ex W P F1 C1 y exp tx tt -> A.Blk W P F C ss (Some y) exp (fold_stmts xs tx) tt made) /\
    (forall exp, A.Blk W P F C ss None exp (fold_stmts xs XUnit) (if d then A.never else TUnit) made)).
  { destruct ss as [|s0 r].
    - destruct f as [|f']; [discriminate H1|]. cbn [check_stmts] in H1. injection H1 as <- <- <- <- <-.
      exists F, C, []. split; [exact Hc|]. split; [reflexivity|]. split; [rewrite app_nil_r; reflexivity|].
      split; [intros; apply A.BlkTail; assumption | intro; apply A.BlkEmpty].
    - exact (bs_stmts _ IH c e (s0 :: r) s xs e1 c1 d s1 H1 ltac:(discriminate) F C Hi Hc Hp1). }
  destruct Hst as (F1 & C1 & made & Hc1 & Fv & Ef & Kt & Ln).
  assert (Hmade : skipn (List.length (cx_fresh c)) (cx_fresh c1) = made)
    by (rewrite Ef; apply skipn_len_app).
  assert (Hesc : forall g0, In g0 made -> ~ In g0 (A.gens (trF tty))).
  { intros g0 Hg Hin. rewrite gens_tr in Hin. rewrite canon_nil in Eg by exact (cr_uf _ _ _ _ Hc1).
    rewrite Hmade in Eg. assert (In g0 (filter (fun g => nmem g made) (ety_gens tty)))
      by (apply filter_In; split; [exact Hin | apply nmem_in; exact Hg]).
    rewrite Eg in H. destruct H. }
  assert (Hof : A.opt_fits (trF tty) (option_map trF exp)) by exact (elab_opt_fits_tr e1 tty exp (cr_uf _ _ _ _ Hc1) Eo).
  destruct tl as [y|].
  - stepok H2 (ok_synth f c1 e1 y exp) Hi1 Hi2' Fs2'.
    destruct (bs_synth _ IH c1 e1 y exp s1 tx tty s2 H2 F1 C1 Hi1 Hc1 Hx (post_frame _ _ _ Fv Hp)) as [Ey Wy].
    split; [eapply A.BlkECheck; [exact (Kt _ _ _ _ Ey) | exact Hesc | exact Hof]|].
    split; [exact Wy|]. split; reflexivity.
  - injection H2 as <- <- <-.
    split.
    + eapply A.BlkECheck; [| exact Hesc | exact Hof].
      replace (trF (if d then YNever else YUnit)) with (if d then A.never else TUnit) by (destruct d; reflexivity).
      apply Ln.
    + split; [destruct d; reflexivity|]. split; [reflexivity|]. destruct d; [right | left]; reflexivity.
Qed.

Lemma fold_stmts_app : forall xs ys tx, fold_stmts (xs ++ ys) tx = fold_stmts xs (fold_stmts ys tx).
Proof. intros xs ys tx. induction xs as [|x r IH]; [reflexivity|]. cbn. rewrite IH. reflexivity. Qed.

Lemma stmts_step : forall f, OK f -> StmtsOK (S f).
Proof.
  intros f IH c e ss s xs e1 c1 d s' H Hne F C Hi Hc Hp.
  destruct ss as [|s0 rest]; [exfalso; exact (Hne eq_refl)|]. cbn [check_stmts] in H.
  binv H as r1 s1 H1. destruct r1 as [[[xs1 e2] c2] d1].
  stepok H1 (ok_check_stmt f c e s0) Hi Hi1 Fs1.
  destruct rest as [|s5 rest5].
  - injection H as -> -> -> -> ->.
    destruct (bs_stmt _ IH c e s0 s xs e1 c1 d s' H1 F C Hi Hc Hp) as (F1 & C1 & made & Hc1 & Fv & Ef & K & L).
    exists F1, C1, made. split; [exact Hc1|]. split; [exact Fv|]. split; [exact Ef|]. split; [|exact L].
    intros y exp tx tt Ey. rewrite <- (app_nil_r made). apply K; [right; discriminate | apply A.BlkTail; exact Ey].
  - binv H as r2 s2 H2. destruct r2 as [[[ys e3] c3] d2]. injection H as <- -> -> -> ->.
    stepok H2 (ok_check_stmts f c2 e2 (s5 :: rest5)) Hi1 Hi2 Fs2.
    destruct (bs_stmt _ IH c e s0 s xs1 e2 c2 d1 s1 H1 F C Hi Hc (post_back _ _ _ Fs2 Hp)) as (F1 & C1 & made1 & Hc1 & Fv1 & Ef1 & K1 & L1).
    destruct (bs_stmts _ IH c2 e2 (s5 :: rest5) s1 ys e1 c1 d s' H2 ltac:(discriminate) F1 C1 Hi1 Hc1
                (post_frame _ _ _ Fv1 Hp)) as (F2 & C2 & made2 & Hc2 & Fv2 & Ef2 & K2 & L2).
    exists F2, C2, (made1 ++ made2). split; [exact Hc2|]. split; [congruence|].
    split; [rewrite Ef2, Ef1, app_assoc; reflexivity|].
    split.
    + intros y exp tx tt Ey. rewrite fold_stmts_app. apply K1; [left; discriminate | exact (K2 _ _ _ _ Ey)].
    + intro exp. rewrite fold_stmts_app. apply K1; [left; discriminate | apply L2].
Qed.



Lemma stmt_step : forall f, OK f -> Pending -> StmtOK (S f).
Proof.
  intros f IH PD c e st0 s xs e1 c1 d s' H F C Hi Hc Hp.
  destruct (pending_stmt st0) eqn:Ps; [exact (pd_stmt PD f IH c e st0 s xs e1 c1 d s' Ps H F C Hi Hc Hp)|].
  destruct st0 as [n ann y|n ann y|n y|its|cnd bl|bl|y]; cbn in Ps; try discriminate Ps; cbn [check_stmt] in H.
  - (* LSLet *) binv H as decl s1 H1.
    binv H as vt s2 H2; destruct vt as [v vt].
    binv H as ty s3 H3.
    binv H as x s4 H4.
    injection H as <- <- <- <- <-.
    assert (Hd : Inv s1 /\ Fr s s1) by
      (destruct ann as [a|];
       [ binv H1 as d0 s0 Ht; injection H1 as <- <-; exact (okstep _ _ _ _ _ (ok_ety_of a (cx_mod c) (tymap e) e (cx_this c)) Hi Ht)
       | injection H1 as <- <-; split; [exact Hi | apply fr_refl] ]);
    destruct Hd as [Hi1 Fs1].
    stepok H2 (ok_synth f c e y decl) Hi1 Hi2 Fs2.
    assert (E3 : s3 = s2) by (destruct decl as [d0|]; [destruct (fits _ vt d0); [injection H3 as _ <-; reflexivity | noerr H3] | injection H3 as _ <-; reflexivity]).
    subst s3.
    stepok H4 (ok_local_var n) Hi2 Hi4 Fs4.
    pose proof (post_back _ _ _ Fs4 Hp) as Hp2.
    pose proof (post_back _ _ _ Fs2 Hp2) as Hp1.
    destruct (local_named n s2 x s4 s4 F H4 (fr_refl _) Hp) as [Nn Vn].
    (* the declared type *)
    assert (HA : match ann with
                 | Some a => exists d0, A.TyOf W (A.fr_mod F) (A.c_ty C) (A.fr_this F) a d0 /\ option_map trF decl = Some d0
                 | None => option_map trF decl = None
                 end /\ opt_wf decl) by
      (destruct ann as [a|];
       [ binv H1 as d0 s0 Ht; injection H1 as <- <-;
         destruct (pd_ty PD a c e s d0 s0 Ht F C Hi Hc (proj1 Hp1)) as [Ty Wd];
         split; [exists (trF d0); split; [exact Ty | reflexivity] | exact Wd]
       | injection H1 as <- <-; split; [reflexivity | exact I] ]);
    destruct HA as [HA Wdecl].
    destruct (bs_synth _ IH c e y decl s1 v vt s2 H2 F C Hi1 Hc Wdecl Hp2) as [Ey Wy].
    assert (HT : match option_map trF decl with Some d0 => A.fits (trF vt) d0 /\ trF ty = d0 | None => trF ty = trF vt end /\ WFty ty) by
      (destruct decl as [d0|];
       [ cbn in Wdecl |- *; destruct (fits e vt d0) eqn:Ef; [injection H3 as E; subst ty; split; [split; [exact (fits_tr e vt d0 (cr_uf _ _ _ _ Hc) Ef) | reflexivity] | exact Wdecl] | noerr H3]
       | injection H3 as E; subst ty; split; [reflexivity | exact Wy] ]);
    destruct HT as [HT Wty].
    exists F, (A.add_local C n (trF ty) (tr_mut Elab.LLet)), [].
    split; [exact (cxrel_local _ _ _ _ _ _ _ Hc Wty)|].
    split; [reflexivity|]; split; [rewrite app_nil_r; reflexivity|].
    split.
    + intros rest tl exp xr t made2 _ Hb. cbn [fold_stmts app]. rewrite <- Vn.
      eapply A.BlkLet; [apply A.LSLet | exact HA | exact Ey | exact HT | exact Nn | exact Hb].
    + intro exp. cbn [fold_stmts]. rewrite <- Vn.
      eapply A.BlkLet; [apply A.LSLet | exact HA | exact Ey | exact HT | exact Nn | apply A.BlkEmpty].
  - (* LSVar *) binv H as decl s1 H1.
    binv H as vt s2 H2; destruct vt as [v vt].
    binv H as ty s3 H3.
    binv H as x s4 H4.
    injection H as <- <- <- <- <-.
    assert (Hd : Inv s1 /\ Fr s s1) by
      (destruct ann as [a|];
       [ binv H1 as d0 s0 Ht; injection H1 as <- <-; exact (okstep _ _ _ _ _ (ok_ety_of a (cx_mod c) (tymap e) e (cx_this c)) Hi Ht)
       | injection H1 as <- <-; split; [exact Hi | apply fr_refl] ]);
    destruct Hd as [Hi1 Fs1].
    stepok H2 (ok_synth f c e y decl) Hi1 Hi2 Fs2.
    assert (E3 : s3 = s2) by (destruct decl as [d0|]; [destruct (fits _ vt d0); [injection H3 as _ <-; reflexivity | noerr H3] | injection H3 as _ <-; reflexivity]).
    subst s3.
    stepok H4 (ok_local_var n) Hi2 Hi4 Fs4.
    pose proof (post_back _ _ _ Fs4 Hp) as Hp2.
    pose proof (post_back _ _ _ Fs2 Hp2) as Hp1.
    destruct (local_named n s2 x s4 s4 F H4 (fr_refl _) Hp) as [Nn Vn].
    (* the declared type *)
    assert (HA : match ann with
                 | Some a => exists d0, A.TyOf W (A.fr_mod F) (A.c_ty C) (A.fr_this F) a d0 /\ option_map trF decl = Some d0
                 | None => option_map trF decl = None
                 end /\ opt_wf decl) by
      (destruct ann as [a|];
       [ binv H1 as d0 s0 Ht; injection H1 as <- <-;
         destruct (pd_ty PD a c e s d0 s0 Ht F C Hi Hc (proj1 Hp1)) as [Ty Wd];
         split; [exists (trF d0); split; [exact Ty | reflexivity] | exact Wd]
       | injection H1 as <- <-; split; [reflexivity | exact I] ]);
    destruct HA as [HA Wdecl].
    destruct (bs_synth _ IH c e y decl s1 v vt s2 H2 F C Hi1 Hc Wdecl Hp2) as [Ey Wy].
    assert (HT : match option_map trF decl with Some d0 => A.fits (trF vt) d0 /\ trF ty = d0 | None => trF ty = trF vt end /\ WFty ty) by
      (destruct decl as [d0|];
       [ cbn in Wdecl |- *; destruct (fits e vt d0) eqn:Ef; [injection H3 as E; subst ty; split; [split; [exact (fits_tr e vt d0 (cr_uf _ _ _ _ Hc) Ef) | reflexivity] | exact Wdecl] | noerr H3]
       | injection H3 as E; subst ty; split; [reflexivity | exact Wy] ]);
    destruct HT as [HT Wty].
    exists F, (A.add_local C n (trF ty) (tr_mut Elab.LVar)), [].
    split; [exact (cxrel_local _ _ _ _ _ _ _ Hc Wty)|].
    split; [reflexivity|]; split; [rewrite app_nil_r; reflexivity|].
    split.
    + intros rest tl exp xr t made2 _ Hb. cbn [fold_stmts app]. rewrite <- Vn.
      eapply A.BlkLet; [apply A.LSVar | exact HA | exact Ey | exact HT | exact Nn | exact Hb].
    + intro exp. cbn [fold_stmts]. rewrite <- Vn.
      eapply A.BlkLet; [apply A.LSVar | exact HA | exact Ey | exact HT | exact Nn | apply A.BlkEmpty].
  - (* assignment *)
    destruct (sassoc n (en_locals e)) as [[tv mu]|] eqn:El; [|noerr H].
    destruct mu; try noerr H.
    binv H as vt s2 H2. destruct vt as [v vt].
    stepok H2 (ok_synth f c e y (Some tv)) Hi Hi2 Fs2.
    destruct (fits e vt tv) eqn:Ef; [|noerr H].
    binv H as x s4 H4. injection H as <- <- <- <- <-.
    stepok H4 (ok_local_var n) Hi2 Hi4 Fs4.
    pose proof (post_back _ _ _ Fs4 Hp) as Hp2.
    destruct (local_named n s2 x s4 s4 F H4 (fr_refl _) Hp) as [Nn Vn].
    destruct (bs_synth _ IH c e y (Some tv) s v vt s2 H2 F C Hi Hc (cr_locals_wf _ _ _ _ Hc n _ El) Hp2) as [Ey Wy].
    assert (Hl : A.slook n (A.c_locals C) = Some (trF tv, A.LVar)) by (rewrite (cr_locals _ _ _ _ Hc), El; reflexivity).
    exists F, C, []. split; [exact Hc|]. split; [reflexivity|]. split; [rewrite app_nil_r; reflexivity|].
    split.
    + intros rest tl exp xr t made2 _ Hb. cbn [fold_stmts app]. rewrite <- Vn.
      eapply A.BlkAssign; [exact Hl | exact Ey | exact (fits_tr e vt tv (cr_uf _ _ _ _ Hc) Ef) | exact Hb | exact Nn].
    + intro exp. cbn [fold_stmts]. rewrite <- Vn.
      eapply A.BlkAssign; [exact Hl | exact Ey | exact (fits_tr e vt tv (cr_uf _ _ _ _ Hc) Ef) | apply A.BlkEmpty | exact Nn].
  - (* while *)
    binv H as cond s1 H1. binv H as r2 s2 H2. destruct r2 as [[body bt0] bt1]. injection H as <- <- <- <- <-.
    stepok H1 (ok_check_bool f c e cnd) Hi Hi1 Fs1.
    stepok H2 (ok_synth_block f (cx_in_loop c) e bl None) Hi1 Hi2 Fs2.
    pose proof (post_back _ _ _ Fs2 Hp) as Hp1.
    pose proof (bs_cond _ IH c e cnd s cond s1 H1 F C Hi Hc Hp1) as Hcd.
    destruct (bs_block _ IH (cx_in_loop c) e bl None s1 body bt0 bt1 s2 H2 (A.with_loop F) C Hi1 (cxrel_loop _ _ _ _ Hc) I
                (post_frame _ _ _ eq_refl Hp)) as (Eb & _).
    exists F, C, []. split; [exact Hc|]. split; [reflexivity|]. split; [rewrite app_nil_r; reflexivity|].
    split.
    + intros rest tl exp xr t made2 _ Hb. cbn [fold_stmts app]. eapply A.BlkWhile; [exact Hcd | exact Eb | exact Hb].
    + intro exp. cbn [fold_stmts]. eapply A.BlkWhile; [exact Hcd | exact Eb | apply A.BlkEmpty].
  - (* loop *)
    binv H as r2 s2 H2. destruct r2 as [[body bt0] bt1]. injection H as <- <- <- <- <-.
    destruct (bs_block _ IH (cx_in_loop c) e bl None s body bt0 bt1 s2 H2 (A.with_loop F) C Hi (cxrel_loop _ _ _ _ Hc) I
                (post_frame _ _ _ eq_refl Hp)) as (Eb & _).
    exists F, C, []. split; [exact Hc|]. split; [reflexivity|]. split; [rewrite app_nil_r; reflexivity|].
    split.
    + intros rest tl exp xr t made2 Hne Hb. cbn [fold_stmts app].
      eapply A.BlkLoop; [destruct Hne as [Hn|Hn]; [left | right; left]; exact Hn | exact Eb | exact Hb].
    + intro exp. cbn [fold_stmts]. destruct (has_break_b bl) eqn:Hk; cbn [negb].
      * eapply A.BlkLoop; [right; right; apply (proj1 (proj2 has_break_spec)); exact Hk | exact Eb | apply A.BlkEmpty].
      * eapply A.BlkLoopLast; [exact Eb|]. intro Hb. apply (proj1 (proj2 has_break_spec)) in Hb. congruence.
  - (* an expression *)
    binv H as r2 s2 H2. destruct r2 as [v vt]. injection H as <- <- <- <- <-.
    destruct (bs_synth _ IH c e y None s v vt s2 H2 F C Hi Hc I Hp) as [Ey Wy].
    exists F, C, []. split; [exact Hc|]. split; [reflexivity|]. split; [rewrite app_nil_r; reflexivity|].
    split.
    + intros rest tl exp xr t made2 Hne Hb. cbn [fold_stmts app]. eapply A.BlkExpr; [exact Hne | exact Ey | exact Hb].
    + intro exp. cbn [fold_stmts]. rewrite (is_never_tr Fl vt Wy). apply A.BlkExprLast. exact Ey.
Qed.

Lemma ok_join : forall mi a b0 exp, ok (join mi a b0 exp).
Proof. intros. unfold join. okt. Qed.

Lemma bool_type_spec : forall mi s bt s', bool_type mi s = Good (bt, s') -> Later s ->
  s' = s /\ exists bi x, sassoc "bool"%string (lang_modules w) = Some bi /\
    sassoc "Bool"%string (gm_names (get_mod w bi)) = Some x /\ bt = YNom x [].
Proof.
  intros mi s bt s' H [Lw _ _ Ll]. unfold bool_type in H. binv H as bm s3 Hl. unfold lang in Hl.
  binv Hl as st0 s4 Hg. injection Hg as E1 E2. subst. injection Hl as E1 E2. subst.
  rewrite Ll in H. destruct (sassoc "bool"%string (lang_modules w)) as [bi|] eqn:Eb; [|noerr H].
  binv H as m s5 Hm. unfold the_mod in Hm. injection Hm as E1 E2. subst.
  rewrite Lw in H. destruct (sassoc "Bool"%string (gm_names (get_mod w bi))) as [x|] eqn:Ex; [|noerr H].
  injection H as E1 E2. subst. split; [reflexivity|]. exists bi, x. auto.
Qed.

Lemma cond_step : forall f, OK f -> CondOK (S f).
Proof.
  intros f IH c e y s v s' H F C Hi Hc Hp.
  assert (Fs : Fr s s') by exact (proj2 (ok_check_bool (S f) c e y s v s' Hi H)).
  destruct (post_back _ _ _ Fs Hp) as [Ls _].
  cbn [check_bool] in H. binv H as bt s1 H1. binv H as r s2 H2. destruct r as [v0 t0].
  destruct (bool_type_spec _ _ _ _ H1 Ls) as [-> (bi & x & Eb & Ex & ->)].
  destruct (fits e t0 (YNom x [])) eqn:Ef; [|noerr H]. injection H as E1 E2. subst.
  destruct (bs_synth _ IH c e y (Some (YNom x [])) s v t0 s' H2 F C Hi Hc eq_refl Hp) as [Ey _].
  eapply A.CondBool; [exact (Hbool bi x Eb Ex) | exact Ey | exact (fits_tr e t0 _ (cr_uf _ _ _ _ Hc) Ef)].
Qed.

Lemma branch_tr : forall bl t, trF (block_bt bl t) = A.branch_ty bl (trF t).
Proof. intros [ss [y|]] t; reflexivity. Qed.

Lemma block_bt_wf : forall bl t, WFty t -> WFty (block_bt bl t).
Proof. intros [ss [y|]] t W0; cbn; [exact W0 | reflexivity]. Qed.

Lemma if_step : forall f, OK f -> IfOK (S f).
Proof.
  intros f IH c e x exp s v t s' H F C Hi Hc Hx Hp.
  destruct x as [| | | | | | |c0 thn els| | | | | | | | ]; cbn [synth_if] in H; try discriminate H.
  binv H as cond s1 H1. binv H as r s2 H2. destruct r as [[tx t1] tty].
  stepok H1 (ok_check_bool f c e c0) Hi Hi1 Fs1.
  stepok H2 (ok_synth_block f c e thn exp) Hi1 Hi2 Fs2.
  destruct els as [y|]; cycle 1.
  - destruct (negb (Elab.opt_fits e YUnit exp)) eqn:E1; [noerr H|].
    destruct (negb (is_unit_or_never tty)) eqn:E2; [noerr H|]. injection H as <- <- <-.
    pose proof (bs_cond _ IH c e c0 s cond s1 H1 F C Hi Hc (post_back _ _ _ Fs2 Hp)) as Hcd.
    destruct (bs_block _ IH c e thn exp s1 tx t1 tty s2 H2 F C Hi1 Hc Hx Hp) as (Eb & Wb & Et & Tl).
    split; [|reflexivity].
    eapply A.ExIfThen; [exact Hcd | exact Eb | |].
    + apply negb_false_iff in E1. exact (elab_opt_fits_tr e YUnit exp (cr_uf _ _ _ _ Hc) E1).
    + apply negb_false_iff in E2. destruct thn as [ss [y0|]]; cbn in Et, Tl; subst tty.
      * destruct t1; try discriminate E2; [left | right]; reflexivity.
      * destruct Tl as [-> | ->]; [left | right]; reflexivity.
  - binv H as r3 s3 H3. destruct r3 as [ex et]. binv H as tj s4 H4. injection H as <- <- <-.
    assert (Hbr : Inv s3 /\ Fr s2 s3 /\ forall F', Post s3 F' -> CxRel c e F' C ->
              (exists et', A.Branch W P F' C y (option_map trF exp) ex et' /\ (exp = None -> et' = trF et)) /\ WFty et).
    { destruct y as [| | | | | | |c5 b5 e5| | | |b5| | | | ]; cbn beta iota in H3;
        try (destruct (okstep _ _ _ _ _ (ok_synth _ _ _ _ _) Hi2 H3) as [Hi3 Fs3]; split; [exact Hi3|]; split; [exact Fs3|];
             intros F' Hp' Hc'; destruct (bs_synth _ IH c e _ exp s2 ex et s3 H3 F' C Hi2 Hc' Hx Hp') as [Ey Wy];
             split; [exists (trF et); split; [apply A.BranchExpr; [intros b6 E6; discriminate E6 | exact Ey] | reflexivity] | exact Wy]).
      - stepok H3 (ok_synth_if f c e (AEIf c5 b5 e5) exp) Hi2 Hi3 Fs3. split; [exact Hi3|]. split; [exact Fs3|].
        intros F' Hp' Hc'. destruct (bs_if _ IH c e _ exp s2 ex et s3 H3 F' C Hi2 Hc' Hx Hp') as [Ey Wy].
        split; [exists (trF et); split; [apply A.BranchExpr; [intros b6 E6; discriminate E6 | exact Ey] | reflexivity] | exact Wy].
      - binv H3 as r6 s6 H6. destruct r6 as [[v6 t6] bt6]. injection H3 as <- <- <-.
        stepok H6 (ok_synth_block f c e b5 exp) Hi2 Hi3 Fs3. split; [exact Hi3|]. split; [exact Fs3|].
        intros F' Hp' Hc'. destruct (bs_block _ IH c e b5 exp s2 v6 t6 bt6 s6 H6 F' C Hi2 Hc' Hx Hp') as (Eb & Wb & Et & _).
        split; [|subst bt6; apply block_bt_wf; exact Wb].
        eexists. split; [apply A.BranchBlock; exact Eb|]. intros ->. subst bt6. symmetry. apply branch_tr. }
    destruct Hbr as (Hi3 & Fs3 & Hbr).
    stepok H4 (ok_join (cx_mod c) tty et exp) Hi3 Hi4' Fs4'.
    assert (Hp3 : Post s3 F) by exact (post_back _ _ _ Fs4' Hp).
    destruct (Hbr F Hp3 Hc) as ((et' & Hb' & Ee) & We).
    pose proof (bs_cond _ IH c e c0 s cond s1 H1 F C Hi Hc (post_back _ _ _ Fs2 (post_back _ _ _ Fs3 Hp3))) as Hcd.
    destruct (bs_block _ IH c e thn exp s1 tx t1 tty s2 H2 F C Hi1 Hc Hx (post_back _ _ _ Fs3 Hp3)) as (Eb & Wb & Et & _).
    assert (Wt : WFty tty) by (subst tty; apply block_bt_wf; exact Wb).
    destruct exp as [xe|].
    + unfold join in H4. injection H4 as <- <-. split; [|exact Hx].
      eapply A.ExIfElse; [exact Hcd | exact Eb | exact Hb' | apply A.JoinExp].
    + destruct (join_tr _ _ _ _ _ _ _ H4 Wt We I) as [J Wj]. split; [|exact Wj].
      eapply A.ExIfElse; [exact Hcd | exact Eb | exact Hb' |].
      rewrite (Ee eq_refl). subst tty. rewrite branch_tr in J. exact J.
Qed.

Lemma ok_step : forall f, OK f -> Pending -> OK (S f).
Proof.
  intros f IH PD. constructor.
  - exact (synth_step f IH PD).
  - exact (block_step f IH).
  - exact (stmts_step f IH).
  - exact (stmt_step f IH PD).
  - exact (cond_step f IH).
  - exact (if_step f IH).
  - exact (path_step f (bs_const _ IH)).
  - exact (pd_const PD f IH).
Qed.

(* The fragment, at every fuel, modulo the pending constructs. *)
Theorem bodies_sound : Pending -> forall f, OK f.
Proof. intros PD f. induction f as [|f IH]; [exact ok_zero | exact (ok_step f IH PD)]. Qed.

End Bodies.
