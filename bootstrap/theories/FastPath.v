(* ------------------------------------------------------------------------ *)
(* FastPath.v — applying a platform family keeping only the members used   *)
(* (m4, speed; Audit.v DRAFT NOTE 24).                                       *)
(*                                                                           *)
(* Under D84 rule 7, `XApply F θ sm e` runs F's body and composes its whole *)
(* result at θ (inst_smap θ Dr) at every application, though e reads one   *)
(* or two members: for the platform's `Memories` (27 members) at every      *)
(* memory access, about two thirds of the 5ea4e2e compiler's interpreted    *)
(* time.  This file is the speed-up, as a pass on the elaborated program    *)
(* (the elaborator itself is exactly rule 7 as drafted).                    *)
(*                                                                           *)
(* The pass is general: it applies to every functor F that main's root      *)
(* environment provides as a platform functor (XDFun h [] []) whose body is *)
(* primitives only (`bind` of each member to a primitive), such as          *)
(* `Memories`.  For such an F, given that nothing in the program binds F,   *)
(* copies it, or files it under another key (the syntactic check `ok_e`),  *)
(* the entry in force for F is always the root's, and the pass             *)
(*   - files F in the root environment as XDFunSym h;                       *)
(*   - turns each `XApply F θ [] e` into the bootstrap IR's `bind F;`       *)
(*     branch for h, `XApplyBr F [(h, bs', ps)] e`, where ps is the pairs  *)
(*     of inst_smap θ Dr whose requirement e looks up (`uses e`), and bs'  *)
(*     the primitive binds those pairs read;                                *)
(*   - turns each `XBApply F θ []` (in a functor body) into the same branch *)
(*     with all of the pairs.                                               *)
(*                                                                           *)
(* Proved (`fast_run`): for every fuel, the run of the transformed program *)
(* is exactly the run of the original — unless the original runs out of   *)
(* fuel (the transformed one runs fewer binds).  `fast_engine_agrees`       *)
(* composes it with CloX.engine_agrees: what the oracle runs by default.   *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia PrimInt63.
From NM Require Import Audit CoreMoss MossX FastX Prune SlotX FoldX CloX.
From NM Require Import InputGenerationExports.
Import ListNotations.

Set Implicit Arguments.
Local Open Scope nat_scope.

(* ===================== 1. What an expression looks up ==================== *)

(* Every item `e` may look up in the environment it starts in (an
   over-approximation: the lookups of every subexpression, as Prune's
   checker counts them, with every source of every satisfaction map). *)
Definition srcs (sm : smap) : list item := map snd sm.

Fixpoint uses (e : xexpr) : list item :=
  match e with
  | XVar _ | XUnit | XBreak => []
  | XVl v => [IVal v]
  | XTag _ _ e1 | XBindTy _ _ e1 | XAssign _ e1 | XLoop e1 | XReturn e1 | XField e1 _
  | XBindFunSym _ _ e1 | XBindPrim _ _ e1 | XConst _ e1 => uses e1
  | XCallD _ _ sm args => srcs sm ++ flat_map uses args
  | XCallA f args => IFn f :: flat_map uses args
  | XMeth e0 _ it args => it :: uses e0 ++ flat_map uses args
  | XMatch e0 arms => uses e0 ++ flat_map (fun a => match a with (_, _, b) => uses b end) arms
  | XLet _ a b | XBindVal _ a b | XSeq a b | XWhile a b => uses a ++ uses b
  | XBindFn _ _ sm e1 | XBindMth _ _ _ _ sm e1 => srcs sm ++ uses e1
  | XBindFun _ _ _ smc e1 => srcs smc ++ uses e1
  | XBindFunA _ u' _ e1 | XBindFunCopy _ u' e1 => IFun u' :: uses e1
  | XApply u _ sm e1 => IFun u :: srcs sm ++ uses e1
  | XApplyR u inputs _ e1 => IFun u :: srcs inputs ++uses e1
  | XIf a b c => uses a ++ uses b ++ uses c
  | XRec fs => flat_map (fun p => uses (snd p)) fs
  | XMatchP e0 arms => uses e0 ++ flat_map (fun p => uses (snd p)) arms
  | XApplyBr u brs e1 =>
      IFun u :: flat_map (fun br => match br with (_, bs, pr) => flat_map buses bs ++ srcs pr end) brs
             ++ uses e1
  end
with buses (b : xbind) : list item :=
  match b with
  | XBTy _ _ | XBFunSym _ _ | XBPrim _ _ => []
  | XBVal _ e1 => uses e1
  | XBFn _ _ sm | XBMth _ _ _ _ sm => srcs sm
  | XBFun _ _ _ smc => srcs smc
  | XBFunA _ u' _ | XBFunCopy _ u' => [IFun u']
  | XBApply u _ sm => IFun u :: srcs sm
  | XBApplyR u inputs _ => IFun u::srcs inputs
  | XBApplyBr u brs =>
      IFun u :: flat_map (fun br => match br with (_, bs, pr) => flat_map buses bs ++ srcs pr end) brs
  end.

(* ===================== 2. The pass, for one functor ====================== *)

Section Pass.
(* F's symbol, the platform functor h its root entry is, and h's body *)
Variables (U H : usym) (Dr : tele) (bsH : list xbind).

Definition isU (i : item) : bool := item_eqb i (IFun U).
Definition notU (u : usym) : bool := negb (Nat.eqb u U).

(* A satisfaction map never moves F's entry to another key, nor another
   entry to F's. *)
Definition sm_ok (sm : smap) : bool :=
  forallb (fun p => Bool.eqb (isU (fst p)) (isU (snd p))) sm.

Definition brs_ok (okb : xbind -> bool) (brs : list (usym * list xbind * smap)) : bool :=
  forallb (fun br => match br with (_, bs, pr) => forallb okb bs && sm_ok pr end) brs.

(* The check: F is never bound, copied, branched on or applied with a
   satisfaction map, and every satisfaction map is sm_ok. *)
Fixpoint ok_e (e : xexpr) : bool :=
  match e with
  | XVar _ | XUnit | XBreak | XVl _ => true
  | XTag _ _ e1 | XBindTy _ _ e1 | XAssign _ e1 | XLoop e1 | XReturn e1 | XField e1 _ => ok_e e1
  | XBindFunSym u _ e1 => notU u && ok_e e1
  | XBindPrim i _ e1 => negb (isU i) && ok_e e1
  | XConst _ e1 => ok_e e1
  | XCallD _ _ sm args => sm_ok sm && forallb ok_e args
  | XCallA _ args => forallb ok_e args
  | XMeth e0 _ _ args => ok_e e0 && forallb ok_e args
  | XMatch e0 arms => ok_e e0 && forallb (fun a => match a with (_, _, b) => ok_e b end) arms
  | XLet _ a b | XBindVal _ a b | XSeq a b | XWhile a b => ok_e a && ok_e b
  | XBindFn _ _ sm e1 | XBindMth _ _ _ _ sm e1 => sm_ok sm && ok_e e1
  | XBindFun u _ sm smc e1 => notU u && sm_ok sm && sm_ok smc && ok_e e1
  | XBindFunA u u' sm e1 => notU u && notU u' && sm_ok sm && ok_e e1
  | XBindFunCopy u u' e1 => notU u && notU u' && ok_e e1
  | XApply u _ sm e1 => sm_ok sm && (notU u || match sm with [] => true | _ => false end) && ok_e e1
  | XApplyR _ _ _ _ => false
  | XIf a b c => ok_e a && ok_e b && ok_e c
  | XRec fs => forallb (fun p => ok_e (snd p)) fs
  | XMatchP e0 arms => ok_e e0 && forallb (fun p => ok_e (snd p)) arms
  | XApplyBr u brs e1 =>
      notU u
      && forallb (fun br => match br with (_, bs, pr) => forallb ok_b bs && sm_ok pr end) brs
      && ok_e e1
  end
with ok_b (b : xbind) : bool :=
  match b with
  | XBTy _ _ => true
  | XBVal _ e1 => ok_e e1
  | XBFn _ _ sm | XBMth _ _ _ _ sm => sm_ok sm
  | XBFun u _ sm smc => notU u && sm_ok sm && sm_ok smc
  | XBFunA u u' sm => notU u && notU u' && sm_ok sm
  | XBFunCopy u u' => notU u && notU u'
  | XBFunSym u _ => notU u
  | XBApply u _ sm => sm_ok sm && (notU u || match sm with [] => true | _ => false end)
  | XBApplyR _ _ _ => false
  | XBApplyBr u brs =>
      notU u && forallb (fun br => match br with (_, bs, pr) => forallb ok_b bs && sm_ok pr end) brs
  | XBPrim i _ => negb (isU i)
  end.

(* h's body: primitives only, none filed under F, and one for every member *)
Definition prim_bind (b : xbind) : bool :=
  match b with XBPrim _ (PBox _) | XBPrim _ (PArrNew _) => false
  | XBPrim i _ => negb (isU i) | _ => false end.
Definition binds_item (i : item) (b : xbind) : bool :=
  match b with XBPrim j _ => item_eqb j i | _ => false end.
Definition body_ok : bool :=
  forallb prim_bind bsH
  && forallb (fun i => is_ty_item i || existsb (binds_item i) bsH) Dr.

(* Main's root environment: F's entries are the platform's, and no other
   entry holds an environment. Runtime type entries remain unchanged. *)
Definition plain (a : xdentry) : bool :=
  match a with
  | XDVal _ | XDPrim _ | XDFunSym _ | XDType _ => true
  | XDClo _ [] => true
  | XDFun _ sms [] => forallb sm_ok sms
  | _ => false
  end.
Definition root_ok (env : xdenv) : bool :=
  forallb (fun ie => if isU (fst ie)
                     then match snd ie with XDFun h [] [] => Nat.eqb h H | _ => false end
                     else plain (snd ie)) env.

(* The application at θ, keeping the pairs whose requirement satisfies
   `keep`, and of h's body the binds those pairs read. *)
Definition narrow (th : subst) (keep : item -> bool) : smap :=
  filter (fun p => keep (fst p)) (inst_smap th Dr).
Definition read_by (N : smap) (b : xbind) : bool :=
  match b with XBPrim i _ => existsb (fun q => item_eqb (snd q) i) N | _ => false end.
Definition branch (th : subst) (keep : item -> bool) : list (usym * list xbind * smap) :=
  let N := narrow th keep in [(H, filter (read_by N) bsH, N)].

Definition inW (W : list item) (i : item) : bool := existsb (item_eqb i) W.

Fixpoint fp_e (e : xexpr) : xexpr :=
  match e with
  | XVar x => XVar x
  | XUnit => XUnit
  | XVl v => XVl v
  | XTag n th e1 => XTag n th (fp_e e1)
  | XCallD f th sm args => XCallD f th sm (map fp_e args)
  | XCallA f args => XCallA f (map fp_e args)
  | XMeth e0 m it args => XMeth (fp_e e0) m it (map fp_e args)
  | XMatch e0 arms =>
      XMatch (fp_e e0) (map (fun a => match a with (n, x, b) => (n, x, fp_e b) end) arms)
  | XLet x e1 e2 => XLet x (fp_e e1) (fp_e e2)
  | XBindTy t tau e1 => XBindTy t tau (fp_e e1)
  | XBindVal v e1 e2 => XBindVal v (fp_e e1) (fp_e e2)
  | XBindFn f f' sm e1 => XBindFn f f' sm (fp_e e1)
  | XBindMth tk m th f' sm e1 => XBindMth tk m th f' sm (fp_e e1)
  | XBindFun u h sm smc e1 => XBindFun u h sm smc (fp_e e1)
  | XBindFunA u u' sm e1 => XBindFunA u u' sm (fp_e e1)
  | XApply u th sm e1 =>
      if Nat.eqb u U then XApplyBr U (branch th (inW (uses e1))) (fp_e e1)
      else XApply u th sm (fp_e e1)
  | XApplyR u inputs outputs e1 => XApplyR u inputs outputs (fp_e e1)
  | XSeq e1 e2 => XSeq (fp_e e1) (fp_e e2)
  | XAssign x e1 => XAssign x (fp_e e1)
  | XIf c e1 e2 => XIf (fp_e c) (fp_e e1) (fp_e e2)
  | XWhile c b => XWhile (fp_e c) (fp_e b)
  | XLoop b => XLoop (fp_e b)
  | XReturn e1 => XReturn (fp_e e1)
  | XBreak => XBreak
  | XRec fs => XRec (map (fun p => (fst p, fp_e (snd p))) fs)
  | XField e1 f => XField (fp_e e1) f
  | XMatchP e0 arms => XMatchP (fp_e e0) (map (fun p => (fst p, fp_e (snd p))) arms)
  | XBindFunSym u h e1 => XBindFunSym u h (fp_e e1)
  | XBindFunCopy u u' e1 => XBindFunCopy u u' (fp_e e1)
  | XApplyBr u brs e1 =>
      XApplyBr u (map (fun br => match br with (h, bs, pr) => (h, map fp_b bs, pr) end) brs)
               (fp_e e1)
  | XBindPrim i p e1 => XBindPrim i p (fp_e e1)
  | XConst v e1 => XConst v (fp_e e1)
  end
with fp_b (b : xbind) : xbind :=
  match b with
  | XBVal v e1 => XBVal v (fp_e e1)
  | XBApply u th sm => if Nat.eqb u U then XBApplyBr U (branch th (fun _ => true)) else XBApply u th sm
  | XBApplyBr u brs =>
      XBApplyBr u (map (fun br => match br with (h, bs, pr) => (h, map fp_b bs, pr) end) brs)
  | b => b
  end.

Definition fp_env (env : xdenv) : xdenv :=
  map (fun ie => if isU (fst ie) then (fst ie, XDFunSym H) else ie) env.

End Pass.

(* Primitive-only families with no type results cannot expose their runtime
   type instantiation. Other calls and checked casts retain their types. *)
Definition ok_program (U H : usym) (p : xprogram) : bool :=
  match Elaboration.nlook H (xo_funs p) with
  | Some (Dr, bsH) =>
      forallb (fun i => negb (is_ty_item i)) Dr && body_ok U Dr bsH
      && forallb (fun fb => ok_e U (snd fb)) (xo_fns p)
      && forallb (fun ub => forallb (ok_b U) (snd (snd ub))) (xo_funs p)
      && root_ok U H (xo_env p)
  | None => false
  end.

Definition fp_program (U H : usym) (p : xprogram) : xprogram :=
  match Elaboration.nlook H (xo_funs p) with
  | Some (Dr, bsH) =>
      XProgram (map (fun fb => (fst fb, fp_e U H Dr bsH (snd fb))) (xo_fns p))
               (map (fun ub => (fst ub, (fst (snd ub), map (fp_b U H Dr bsH) (snd (snd ub))))) (xo_funs p))
               (xo_bool p) (xo_true p) (xo_false p) (xo_main p)
               (fp_env U H (xo_env p)) (xo_prims p) (xo_cast p) (xo_inputs p) (xo_exports p) (xo_immutable p) (xo_fixed_inst p) (xo_fixed_keys p)
  | None => p
  end.

(* The candidates: main's root entries for platform functors. *)
Definition candidates (p : xprogram) : list (usym * usym) :=
  flat_map (fun ie => match ie with
                      | (IFun u, XDFun h [] []) => [(u, h)]
                      | _ => []
                      end) (xo_env p).

(* What the oracle runs: each candidate the check accepts, transformed. *)
Definition fast_program (p : xprogram) : xprogram :=
  fold_left (fun p uh => if ok_program (fst uh) (snd uh) p then fp_program (fst uh) (snd uh) p else p)
            (candidates p) p.

(* ===================== 3. Environments ================================== *)

Lemma item_eqb_refl : forall i, item_eqb i i = true.
Proof. intro i. apply item_eqb_eq. reflexivity. Qed.

Lemma lookup_app : forall i l d,
  lookup_xdenv i (l ++ d) = match lookup_xdenv i l with Some a => Some a | None => lookup_xdenv i d end.
Proof.
  induction l as [|[j a] r IH]; intros d; simpl; auto. destruct (item_eqb i j); auto.
Qed.

Lemma lookup_In : forall i l a, lookup_xdenv i l = Some a -> In (i, a) l.
Proof.
  induction l as [|[j b] r IH]; simpl; intros a H; [discriminate|].
  destruct (item_eqb i j) eqn:E; [apply item_eqb_eq in E; injection H as <-; subst; auto|auto].
Qed.

Lemma isU_subst : forall U th i, isU U (app_subst_item th i) = isU U i.
Proof. intros U th []; reflexivity. Qed.

Lemma isU_fun : forall U u, isU U (IFun u) = Nat.eqb u U.
Proof. reflexivity. Qed.

Lemma notU_isU : forall U u, notU U u = true -> isU U (IFun u) = false.
Proof. unfold notU. intros U u H. rewrite isU_fun. destruct (Nat.eqb u U); auto. Qed.

Lemma sm_ok_inst : forall U th D, sm_ok U (inst_smap th D) = true.
Proof.
  intros U th D. unfold sm_ok, inst_smap. apply forallb_forall. intros p Hp.
  apply in_map_iff in Hp as [i [<- _]]. simpl. rewrite isU_subst. apply eqb_reflx.
Qed.

Section Rel.
Variables U H : usym.

(* Entries agree when equal up to their captured environments, except that
   F's entry is the platform's in the original and XDFunSym h in the
   transformed program. *)
Inductive ent : xdentry -> xdentry -> Prop :=
| EVal : forall w, ent (XDVal w) (XDVal w)
| EPrim : forall p, ent (XDPrim p) (XDPrim p)
| EPrimAt : forall p tau, ent (XDPrimAt p tau) (XDPrimAt p tau)
| ESym : forall h, ent (XDFunSym h) (XDFunSym h)
| EClo : forall f d d', lr d d' -> ent (XDClo f d) (XDClo f d')
| EFun : forall h sms d d', forallb (sm_ok U) sms = true -> lr d d' -> ent (XDFun h sms d) (XDFun h sms d')
| EType : forall tau, ent (XDType tau) (XDType tau)
with kent : item -> xdentry -> xdentry -> Prop :=
| KU : forall i, isU U i = true -> kent i (XDFun H [] []) (XDFunSym H)
| KO : forall i a a', isU U i = false -> ent a a' -> kent i a a'
with lr : xdenv -> xdenv -> Prop :=
| LNil : lr [] []
| LCons : forall i a a' d d', kent i a a' -> lr d d' -> lr ((i, a) :: d) ((i, a') :: d').

Lemma ent_prim_entry : forall i p, ent (xprim_entry [] i p) (xprim_entry [] i p).
Proof. intros i p. destruct p; destruct i; constructor. Qed.

Definition orel (i : item) (o o' : option xdentry) : Prop :=
  match o, o' with
  | Some a, Some a' => kent i a a'
  | None, None => True
  | _, _ => False
  end.

(* The environments agree on S and retain exactly the same runtime type
   stream, even when some unused primitive provisions have been removed. *)
Record agx (S : item -> Prop) (d d' : xdenv) : Prop := Agx {
  agx_lookup : forall i, S i -> orel i (lookup_xdenv i d) (lookup_xdenv i d');
  agx_types : xdtypes d = xdtypes d'
}.
Coercion agx_lookup : agx >-> Funclass.

Lemma kent_types : forall i a a', kent i a a' ->
  xdtypes [(i,a)] = xdtypes [(i,a')].
Proof.
  intros i a a' K. inversion K; subst; [destruct i; reflexivity|].
  inversion H1; subst; destruct i; reflexivity.
Qed.
Lemma lr_types : forall d d', lr d d' -> xdtypes d = xdtypes d'.
Proof.
  intros d d' L. induction L; [reflexivity|].
  change (xdtypes ([(i,a)] ++ d) = xdtypes ([(i,a')] ++ d')).
  rewrite !xdtypes_app, (kent_types H0), IHL. reflexivity.
Qed.

Definition lk (S : item -> Prop) (l : list item) : Prop := forall i, In i l -> S i.

Lemma lr_lookup : forall d d', lr d d' -> forall i, orel i (lookup_xdenv i d) (lookup_xdenv i d').
Proof.
  intros d d' Hd i. induction Hd; simpl; auto.
  destruct (item_eqb i i0) eqn:E; auto. apply item_eqb_eq in E. subst. assumption.
Qed.

Lemma lr_agx : forall S d d', lr d d' -> agx S d d'.
Proof.
  intros S d d' Hd. constructor; [intros i _; apply lr_lookup; exact Hd|apply lr_types; exact Hd].
Qed.

Lemma agx_cons : forall S d d' i a a', kent i a a' -> agx S d d' -> agx S ((i, a) :: d) ((i, a') :: d').
Proof.
  intros S d d' i a a' Ha Hd. constructor.
  - intros j Hj. simpl. destruct (item_eqb j i) eqn:E;
      [apply item_eqb_eq in E; subst; exact Ha|apply Hd; auto].
  - change (xdtypes ([(i,a)] ++ d) = xdtypes ([(i,a')] ++ d')).
    rewrite !xdtypes_app, (kent_types Ha), (agx_types Hd). reflexivity.
Qed.

Lemma agx_app : forall S dr dr' d d', lr dr dr' -> agx S d d' -> agx S (dr ++ d) (dr' ++ d').
Proof. intros S dr dr' d d' Hr Hd. induction Hr; simpl; auto. apply agx_cons; auto. Qed.

Lemma lr_app : forall a a' b b', lr a a' -> lr b b' -> lr (a ++ b) (a' ++ b').
Proof. intros a a' b b' Ha Hb. induction Ha; simpl; auto. constructor; auto. Qed.

Lemma lr_type_rows : forall types, lr (ftype_pairs types) (ftype_pairs types).
Proof.
  induction types as [|[t tau] types IH]; constructor; auto.
  apply KO; [reflexivity|constructor].
Qed.
Lemma lr_close : forall S d d' dc dc', agx S d d' -> lr dc dc' -> lr (xclose d dc) (xclose d' dc').
Proof.
  intros S d d' dc dc' Hd Hc. unfold xclose.
  rewrite !xtype_env_pairs, (agx_types Hd).
  apply lr_app; [exact Hc|apply lr_type_rows].
Qed.
Lemma agx_ground : forall S d d' tau, agx S d d' -> xground d tau = xground d' tau.
Proof. intros S d d' tau Hd; unfold xground; rewrite (agx_types Hd); reflexivity. Qed.
Lemma lr_inst : forall S d d' th, agx S d d' -> lr (xinst d th) (xinst d' th).
Proof.
  intros S d d' th Hd. rewrite !xinst_map.
  induction th as [|[t tau] th IH]; cbn [map fst snd]; constructor; auto.
  rewrite (agx_ground tau Hd). apply KO; [reflexivity|constructor].
Qed.
Lemma lr_call : forall P P' S d d' th dc dc',
  xreify P' = xreify P -> agx S d d' -> lr dc dc' ->
  lr (xcall_env P d th dc) (xcall_env P' d' th dc').
Proof.
  intros P P' S d d' th dc dc' Mode Hd Hc. unfold xcall_env.
  rewrite Mode. destruct (xreify P); [|exact Hc].
  apply lr_app; [apply lr_inst with S; exact Hd|eapply lr_close; eauto].
Qed.
Lemma lr_result : forall P P' S d d' th Dr dr dr',
  xreify P' = xreify P -> agx S d d' -> lr dr dr' ->
  lr (xresult_env P th d Dr dr) (xresult_env P' th d' Dr dr').
Proof.
  intros P P' S d d' th Dr dr dr' Mode Hd Hr. unfold xresult_env.
  rewrite Mode. destruct (xreify P); [|exact Hr].
  unfold xresult_types. rewrite !xtype_env_pairs, (agx_types Hd).
  apply lr_app; [exact Hr|].
  apply lr_app; [apply lr_inst with S;exact Hd|].
  generalize (xdtypes d'). intro types. induction types as [|[t tau] types IH]; cbn [ftype_pairs map filter fst].
  - constructor.
  - destruct (existsb (item_eqb (ITy t)) Dr); auto. constructor; auto.
    apply KO; [reflexivity|constructor].
Qed.
Lemma agx_bind_ty : forall P P' S d d' t tau,
  xreify P' = xreify P -> agx S d d' ->
  agx S (xbind_ty P d t tau) (xbind_ty P' d' t tau).
Proof.
  intros P P' S d d' t tau Mode Hd. unfold xbind_ty.
  rewrite Mode. destruct (xreify P); [|exact Hd].
  rewrite (agx_ground tau Hd). apply agx_cons; [apply KO; [reflexivity|constructor]|exact Hd].
Qed.
Lemma agx_prim : forall S d d' i p, agx S d d' -> ent (xprim_entry d i p) (xprim_entry d' i p).
Proof.
  intros S d d' i p Hd. unfold xprim_entry; destruct p; destruct i; try constructor;
    rewrite (agx_ground _ Hd); constructor.
Qed.

Lemma kent_move : forall s req a a', kent s a a' -> Bool.eqb (isU U req) (isU U s) = true -> kent req a a'.
Proof.
  intros s req a a' Ha Hq. apply eqb_prop in Hq. inversion Ha; subst.
  - apply KU. congruence.
  - apply KO; auto. congruence.
Qed.

Lemma kent_ent : forall i a a', isU U i = false -> kent i a a' -> ent a a'.
Proof. intros i a a' Hi Ha. inversion Ha; subst; auto. congruence. Qed.

Lemma compose_agx : forall S d d' sm, agx S d d' -> lk S (srcs sm) -> sm_ok U sm = true ->
  match xcompose d sm, xcompose d' sm with
  | Some dc, Some dc' => lr dc dc'
  | None, None => True
  | _, _ => False
  end.
Proof.
  intros S d d' sm Hd. induction sm as [|[req s] r IH]; simpl; intros Hs Hok; [constructor|].
  apply andb_true_iff in Hok as [Hq Hok].
  assert (Hr : lk S (srcs r)) by (intros x Hx; apply Hs; simpl; auto).
  specialize (IH Hr Hok). pose proof (Hd s (Hs s (or_introl eq_refl))) as Ho.
  destruct (lookup_xdenv s d) as [a|], (lookup_xdenv s d') as [a'|]; simpl in Ho; try contradiction;
    destruct (xcompose d r) as [dr|], (xcompose d' r) as [dr'|]; try contradiction; auto.
  constructor; auto. simpl in Hq. eapply kent_move; eauto.
Qed.

Lemma chain_lr : forall sms d d', lr d d' -> forallb (sm_ok U) sms = true ->
  match xrun_chain d sms, xrun_chain d' sms with
  | Some d2, Some d2' => lr d2 d2'
  | None, None => True
  | _, _ => False
  end.
Proof.
  induction sms as [|sm r IH]; simpl; intros d d' Hd Hok; auto.
  apply andb_true_iff in Hok as [Hsm Hok]. specialize (IH d d' Hd Hok).
  destruct (xrun_chain d r) as [dm|], (xrun_chain d' r) as [dm'|]; try contradiction; auto.
  apply (compose_agx (S := fun _ => True)); auto. apply lr_agx; auto. intros ? ?; exact I.
Qed.

(* Environments of primitives at keys other than F's *)
Definition prim_env (dr : xdenv) : Prop :=
  Forall (fun e => isU U (fst e) = false /\ exists q, snd e = XDPrim q) dr.

Lemma lr_prims : forall dr, prim_env dr -> lr dr dr.
Proof.
  induction 1 as [|[i a] r [Hi [q Hq]] _ IH]; constructor; auto. simpl in *. subst.
  apply KO; auto. constructor.
Qed.

Definition inkeys (i : item) (dr : xdenv) : bool := existsb (fun p => item_eqb i (fst p)) dr.

Lemma lookup_inkeys : forall i dr, inkeys i dr = false -> lookup_xdenv i dr = None.
Proof.
  induction dr as [|[j a] r IH]; simpl; auto. intros E. apply orb_false_iff in E as [E1 E2].
  rewrite E1. auto.
Qed.

Lemma lookup_filter_keep : forall (keep : item -> bool) i dr, keep i = true ->
  lookup_xdenv i (filter (fun p => keep (fst p)) dr) = lookup_xdenv i dr.
Proof.
  induction dr as [|[j a] r IH]; simpl; intros Hk; auto.
  destruct (item_eqb i j) eqn:E.
  - apply item_eqb_eq in E. subst. rewrite Hk. simpl. rewrite item_eqb_refl. auto.
  - destruct (keep j); simpl; [rewrite E|]; auto.
Qed.

Lemma lookup_filter_none : forall (f : item * xdentry -> bool) i dr, lookup_xdenv i dr = None ->
  lookup_xdenv i (filter f dr) = None.
Proof.
  induction dr as [|[j a] r IH]; simpl; intros Hl; auto.
  destruct (item_eqb i j) eqn:E; [discriminate|].
  destruct (f (j, a)); simpl; [rewrite E|]; auto.
Qed.

(* After a narrowed application: agreement on the items the result keeps,
   and on S elsewhere *)
Lemma prim_env_types : forall dr, prim_env dr -> xdtypes dr = [].
Proof.
  intros dr Hp. induction Hp as [|[i a] r Hp Hr IH]; [reflexivity|].
  destruct Hp as [Hu [q Eq]]. cbn in Eq. subst a. destruct i; cbn [xdtypes]; exact IH.
Qed.
Lemma prim_env_filter : forall dr (f : item * xdentry -> bool), prim_env dr -> prim_env (filter f dr).
Proof. intros dr f Hp. apply Forall_forall. intros x E. apply filter_In in E as [E _]. exact ((proj1 (Forall_forall _ _) Hp) x E). Qed.

Lemma agx_narrow : forall S (keep : item -> bool) dr d d', prim_env dr -> agx S d d' ->
  agx (fun i => if inkeys i dr then keep i = true else S i)
      (dr ++ d) (filter (fun p => keep (fst p)) dr ++ d').
Proof.
  intros S keep dr d d' Hp Hd. constructor.
  - intro i. rewrite !lookup_app.
  destruct (inkeys i dr) eqn:E; intros Hi.
  + rewrite lookup_filter_keep by auto.
    destruct (lookup_xdenv i dr) as [a|] eqn:El.
    * apply lookup_In in El. unfold prim_env in Hp. rewrite Forall_forall in Hp. destruct (Hp _ El) as [Hu [q Hq]].
      simpl in *. subst. apply KO; auto. constructor.
    * exfalso. unfold inkeys in E. apply existsb_exists in E as [[j a] [Hin Hj]].
      simpl in Hj. apply item_eqb_eq in Hj. subst.
      clear -El Hin. induction dr as [|[k b] r IH]; simpl in *; [contradiction|].
      destruct (item_eqb j k) eqn:E; [discriminate|]. destruct Hin as [Hin|Hin]; auto.
      injection Hin as -> ->. rewrite item_eqb_refl in E. discriminate.
  + rewrite lookup_inkeys by auto. rewrite lookup_filter_none by (apply lookup_inkeys; auto).
    apply Hd; auto.
  - rewrite !xdtypes_app, (prim_env_types Hp), (prim_env_types (prim_env_filter _ Hp)), (agx_types Hd). reflexivity.
Qed.

End Rel.

(* ===================== 4. Primitive bodies =============================== *)

Definition bent (b : xbind) : item * xdentry :=
  match b with XBPrim i p => (i, XDPrim p) | _ => (ITy 0, XDVal XVUnit) end.

Lemma binds_prims : forall U P bs k d g st, forallb (prim_bind U) bs = true ->
  xinterp_binds k P d g st bs
  = if Nat.ltb (length bs) k then inl (rev (map bent bs) ++ d, g, st) else inr (XR XFuel g st).
Proof.
  intros U P. induction bs as [|b r IH]; intros [|k] d g st Hp; simpl; auto.
  destruct b; simpl in Hp; try discriminate. apply andb_true_iff in Hp as [Hpi Hp].
  assert (Ep : xprim_entry d i p = XDPrim p) by (destruct p; try discriminate Hpi; destruct i; reflexivity).
  rewrite Ep. rewrite IH by auto. rewrite <- app_assoc. reflexivity.
Qed.

Lemma prims_shape : forall U bs, forallb (prim_bind U) bs = true -> prim_env U (rev (map bent bs)).
Proof.
  intros U bs Hp. unfold prim_env. apply Forall_rev. induction bs as [|b r IH]; simpl; constructor.
  - destruct b; simpl in Hp; try discriminate. apply andb_true_iff in Hp as [Hi _].
    simpl. split; [apply negb_true_iff; destruct p; simpl in Hi; try discriminate; exact Hi|eauto].
  - apply IH. destruct b; simpl in Hp; try discriminate. apply andb_true_iff in Hp; tauto.
Qed.

Lemma lookup_prims_filter : forall U i (f : xbind -> bool) bs, forallb (prim_bind U) bs = true ->
  (forall b, In b bs -> binds_item i b = true -> f b = true) ->
  lookup_xdenv i (rev (map bent (filter f bs))) = lookup_xdenv i (rev (map bent bs)).
Proof.
  intros U i f. induction bs as [|b r IH]; simpl; intros Hp Hf; auto.
  apply andb_true_iff in Hp as [Hb Hp].
  assert (IH' := IH Hp (fun b' Hin => Hf b' (or_intror Hin))).
  destruct (f b) eqn:Ef; simpl; rewrite !lookup_app, IH'; auto.
  destruct (lookup_xdenv i (rev (map bent r))); auto.
  destruct b; simpl in Hb; try discriminate. simpl.
  destruct (item_eqb i i0) eqn:E; auto.
  apply item_eqb_eq in E. subst. rewrite (Hf _ (or_introl eq_refl)) in Ef; [discriminate|].
  simpl. apply item_eqb_refl.
Qed.

Lemma lookup_prims_some : forall U i bs, forallb (prim_bind U) bs = true ->
  existsb (binds_item i) bs = true -> exists a, lookup_xdenv i (rev (map bent bs)) = Some a.
Proof.
  intros U i. induction bs as [|b r IH]; simpl; intros Hp He; [discriminate|].
  apply andb_true_iff in Hp as [Hb Hp]. rewrite lookup_app.
  destruct (lookup_xdenv i (rev (map bent r))) as [a|]; eauto.
  apply orb_true_iff in He as [He|He].
  - destruct b; simpl in He; try discriminate. simpl. apply item_eqb_eq in He. subst.
    rewrite item_eqb_refl. eauto.
  - destruct (IH Hp He) as [a Ha]. discriminate.
Qed.

Lemma compose_ext : forall A B sm, (forall x, In x (srcs sm) -> lookup_xdenv x A = lookup_xdenv x B) ->
  xcompose A sm = xcompose B sm.
Proof.
  induction sm as [|[req s] r IH]; simpl; intros Hx; auto.
  rewrite Hx by auto. rewrite IH by auto. reflexivity.
Qed.

Lemma compose_filter : forall A sm dr (f : item -> bool), xcompose A sm = Some dr ->
  xcompose A (filter (fun p => f (fst p)) sm) = Some (filter (fun p => f (fst p)) dr).
Proof.
  induction sm as [|[req s] r IH]; simpl; intros dr f Hc; [injection Hc as <-; auto|].
  destruct (lookup_xdenv s A) as [a|] eqn:El; [|discriminate].
  destruct (xcompose A r) as [dr0|] eqn:Er; [|discriminate]. injection Hc as <-.
  simpl. destruct (f req); simpl; rewrite (IH dr0 f eq_refl); [rewrite El|]; auto.
Qed.

Lemma compose_some : forall A sm, (forall x, In x (srcs sm) -> exists a, lookup_xdenv x A = Some a) ->
  exists dr, xcompose A sm = Some dr.
Proof.
  induction sm as [|[req s] r IH]; simpl; intros Hx; eauto.
  destruct (Hx s (or_introl eq_refl)) as [a ->]. destruct IH as [dr ->]; eauto.
Qed.

Lemma compose_shape : forall (K : item -> Prop) (Q : xdentry -> Prop) A sm dr,
  xcompose A sm = Some dr -> (forall x a, lookup_xdenv x A = Some a -> Q a) ->
  (forall p, In p sm -> K (fst p)) -> Forall (fun e => K (fst e) /\ Q (snd e)) dr.
Proof.
  intros K Q A. induction sm as [|[req s] r IH]; simpl; intros dr Hc HQ HK; [injection Hc as <-; auto|].
  destruct (lookup_xdenv s A) as [a|] eqn:El; [|discriminate].
  destruct (xcompose A r) as [dr0|] eqn:Er; [|discriminate]. injection Hc as <-.
  constructor; [split; [apply (HK _ (or_introl eq_refl))|eauto]|].
  apply IH; auto.
Qed.

Lemma in_inst : forall th D q, In q (inst_smap th D) ->
  exists i, q = (app_subst_item th i, i) /\ In i D /\ is_ty_item i = false.
Proof.
  intros th D q Hq. unfold inst_smap in Hq. apply in_map_iff in Hq as [i [<- Hi]].
  apply filter_In in Hi as [Hi Ht]. exists i. split; auto. split; auto.
  apply negb_true_iff; auto.
Qed.

Lemma filter_true : forall A (l : list A), filter (fun _ => true) l = l.
Proof. induction l; simpl; f_equal; auto. Qed.

(* ===================== 5. Simulation ===================================== *)

Definition fo (r : xres) : Prop := xr_out r = XFuel.
Definition fol {A} (r : A + xres) : Prop := match r with inr r => fo r | inl _ => False end.

Section Sim.
Variables (U H : usym) (Dr : tele) (bsH : list xbind).
Variables P P' : xprog.
Notation T := (fp_e U H Dr bsH).
Notation Tb := (fp_b U H Dr bsH).
Notation Tbrs := (map (fun br : usym * list xbind * smap =>
                         match br with (h, bs, pr) => (h, map Tb bs, pr) end)).
Hypothesis Hfn : forall f, xp_fn P' f = option_map T (xp_fn P f).
Hypothesis Hfun : forall u, xp_fun P' u = option_map (fun p => (fst p, map Tb (snd p))) (xp_fun P u).
Hypothesis Hprim : forall p ws st, run_prim P' p ws st = run_prim P p ws st.
Hypothesis Htrue : xp_true P' = xp_true P.
Hypothesis Hmode : xreify P' = xreify P.
Hypothesis Hcasts : xp_cast P' = xp_cast P.
Hypothesis Hinputs : forall h, xp_inputs P' h = xp_inputs P h.
Hypothesis Hokf : forall f b, xp_fn P f = Some b -> ok_e U b = true.
Hypothesis Hoku : forall u D bs, xp_fun P u = Some (D, bs) -> forallb (ok_b U) bs = true.
Hypothesis HH : xp_fun P H = Some (Dr, bsH).
Hypothesis Hbody : body_ok U Dr bsH = true.
Hypothesis Hresults : forallb (fun i => negb (is_ty_item i)) Dr = true.

Notation agx := (agx U H).
Notation lr := (lr U H).
Notation lk := lk.

Lemma cast_eq : forall S d d' n th w, agx S d d' -> xcast P' d' n th w = xcast P d n th w.
Proof.
  intros S d d' n th w Hd. unfold xcast, xground.
  rewrite Hcasts, (agx_types Hd). reflexivity.
Qed.
Lemma apply_lr : forall S d d' h th dc dc', agx S d d' -> lr dc dc' ->
  lr (xapply_env P d h th dc) (xapply_env P' d' h th dc').
Proof.
  intros S d d' h th dc dc' Hd Hc. unfold xapply_env.
  rewrite Hinputs. eapply lr_call; eauto.
Qed.
Lemma result_plain : forall th d dr, xresult_env P th d Dr dr = dr.
Proof.
  intros th d dr. unfold xresult_env, xresult_types. destruct (xreify P); [|reflexivity].
  assert (E : filter (fun p => existsb (item_eqb (fst p)) Dr) (xtype_env d) = []).
  { clear -Hresults. induction d as [|[i a] d IH]; [reflexivity|].
    destruct i,a; cbn [xtype_env filter fst]; try exact IH.
    destruct (existsb (item_eqb (ITy t)) Dr) eqn:InTy; [|exact IH].
    apply existsb_exists in InTy as [j [In Eq]]. apply item_eqb_eq in Eq. subst j.
    rewrite forallb_forall in Hresults. specialize (Hresults _ In). discriminate. }
  rewrite E,(xresult_generations_typefree th Dr Hresults);cbn[xinst];rewrite app_nil_r;reflexivity.
Qed.
Lemma prim_at_eq : forall p tau ws st, run_prim_at P' p tau ws st = run_prim_at P p tau ws st.
Proof. intros. unfold run_prim_at. rewrite Hprim. reflexivity. Qed.

(* An application of F: the original runs h's whole body and composes the
   whole result; the branch runs the binds the kept pairs read and composes
   those pairs. *)
Lemma uapp : forall k S d d' g st th keep, S (IFun U) -> agx S d d' ->
  fol (xinterp_apply k P d U th [] st)
  \/ match xinterp_apply k P d U th [] st, xinterp_branch k P' d' g st U (branch H Dr bsH th keep) with
     | inl (dr, st1), inl (dr', g1, st1') =>
         g1 = g /\ st1' = st1 /\ dr' = filter (fun p => keep (fst p)) dr /\ prim_env U dr
     | inr r, inr r' => r' = with_g g r
     | _, _ => False
     end.
Proof.
  intros k S d d' g st th keep HS Hd. destruct k as [|k]; [left; reflexivity|].
  unfold body_ok in Hbody. apply andb_true_iff in Hbody as [Hp Hcov].
  simpl. pose proof (Hd _ HS) as Ho.
  destruct (lookup_xdenv (IFun U) d) as [a|], (lookup_xdenv (IFun U) d') as [a'|];
    simpl in Ho; try contradiction; [|right; reflexivity].
  inversion Ho; subst; [|rewrite isU_fun, Nat.eqb_refl in H0; discriminate].
  rewrite HH. simpl. rewrite Nat.eqb_refl.
  set (N := narrow Dr th keep).
  assert (Hpf : forallb (prim_bind U) (filter (read_by N) bsH) = true).
  { apply forallb_forall. intros b Hb. apply filter_In in Hb as [Hb _].
    rewrite forallb_forall in Hp. auto. }
  rewrite (binds_prims U P bsH k (xapply_env P d H th []) [] st Hp), (binds_prims U P' _ k d' g st Hpf).
  destruct (Nat.ltb (length bsH) k) eqn:Ek; [|left; reflexivity].
  assert (Ek' : Nat.ltb (length (filter (read_by N) bsH)) k = true).
  { apply Nat.ltb_lt. apply Nat.ltb_lt in Ek. pose proof (filter_length_le (read_by N) bsH). lia. }
  rewrite Ek'.
  (* every member of the result has its primitive *)
  assert (Hsrc : forall q, In q (inst_smap th Dr) -> exists a, lookup_xdenv (snd q) (rev (map bent bsH)) = Some a).
  { intros q Hq. destruct (in_inst _ _ _ Hq) as [i [-> [Hi Ht]]]. simpl.
    rewrite forallb_forall in Hcov. specialize (Hcov i Hi). rewrite Ht in Hcov. simpl in Hcov.
    eapply lookup_prims_some; eauto. }
  destruct (compose_some (rev (map bent bsH)) (inst_smap th Dr)) as [dr Hdr].
  { intros x Hx. unfold srcs in Hx. apply in_map_iff in Hx as [q [<- Hq]]. auto. }
  assert (HdrBase : xcompose (rev (map bent bsH) ++ xapply_env P d H th []) (inst_smap th Dr) = Some dr).
  { rewrite <- Hdr. apply compose_ext. intros x Hx.
    apply in_map_iff in Hx as [q [<- Hq]]. destruct (Hsrc q Hq) as [a Ha].
    rewrite lookup_app, Ha. reflexivity. }
  rewrite HdrBase, result_plain.
  assert (Hc' : xcompose (rev (map bent (filter (read_by N) bsH)) ++ d') N = Some (filter (fun p => keep (fst p)) dr)).
  { rewrite <- (compose_filter _ _ keep Hdr). apply compose_ext.
    intros x Hx. unfold srcs in Hx. apply in_map_iff in Hx as [q [<- Hq]].
    assert (HqI : In q (inst_smap th Dr)) by (unfold N, narrow in Hq; apply filter_In in Hq; tauto).
    destruct (Hsrc q HqI) as [a Ha].
    rewrite lookup_app. rewrite (lookup_prims_filter U (snd q) (read_by N) bsH Hp).
    - rewrite Ha. reflexivity.
    - intros b _ Hb. destruct b; simpl in Hb |- *; try discriminate.
      apply item_eqb_eq in Hb. subst. apply existsb_exists. exists q. split; auto.
      apply item_eqb_refl. }
  rewrite Hc'. right. split; auto. split; auto. split; auto.
  unfold prim_env. apply (compose_shape (fun i => isU U i = false) (fun a => exists q, a = XDPrim q) _ _ Hdr).
  - intros x a Hl. apply lookup_In in Hl. pose proof (prims_shape U bsH Hp) as Hs.
    unfold prim_env in Hs. rewrite Forall_forall in Hs. destruct (Hs _ Hl) as [_ Hq]. exact Hq.
  - intros q Hq. destruct (in_inst _ _ _ Hq) as [i [-> [Hi Ht]]]. simpl. rewrite isU_subst.
    rewrite forallb_forall in Hcov. specialize (Hcov i Hi). rewrite Ht in Hcov. simpl in Hcov.
    apply existsb_exists in Hcov as [b [Hb Hbi]]. rewrite forallb_forall in Hp. specialize (Hp b Hb).
    destruct b; simpl in Hp, Hbi; try discriminate. apply item_eqb_eq in Hbi. subst.
    apply negb_true_iff; destruct p; simpl in Hp; try discriminate; exact Hp.
Qed.


Definition brel (S : item -> Prop) (a b : (xdenv * xvenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (d', g', st'), inl (d, g, st) => agx S d d' /\ g' = g /\ st' = st
  | inr r', inr r => r' = r
  | _, _ => False
  end.

Definition arel (a b : (xdenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (d', st'), inl (d, st) => lr d d' /\ st' = st
  | inr r', inr r => r' = r
  | _, _ => False
  end.

Definition rrel (a b : (xdenv * xvenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (d', g', st'), inl (d, g, st) => lr d d' /\ g' = g /\ st' = st
  | inr r', inr r => r' = r
  | _, _ => False
  end.

Definition brs_uses (brs : list (usym * list xbind * smap)) : list item :=
  flat_map (fun br : usym * list xbind * smap => match br with (_, bs, pr) => flat_map buses bs ++ srcs pr end) brs.
Definition brs_okb (brs : list (usym * list xbind * smap)) : bool :=
  forallb (fun br : usym * list xbind * smap => match br with (_, bs, pr) => forallb (ok_b U) bs && sm_ok U pr end) brs.

Definition SIM (k : nat) : Prop :=
  (forall S d d' g st e, ok_e U e = true -> lk S (uses e) -> agx S d d' ->
     fo (xinterp k P d g st e) \/ xinterp k P' d' g st (T e) = xinterp k P d g st e)
  /\ (forall S d d' g st l, forallb (ok_e U) l = true -> lk S (flat_map uses l) -> agx S d d' ->
     fol (xinterp_list k P d g st l) \/ xinterp_list k P' d' g st (map T l) = xinterp_list k P d g st l)
  /\ (forall S d d' g st bs, forallb (ok_b U) bs = true -> lk S (flat_map buses bs) -> agx S d d' ->
     fol (xinterp_binds k P d g st bs)
     \/ brel S (xinterp_binds k P' d' g st (map Tb bs)) (xinterp_binds k P d g st bs))
  /\ (forall S d d' u th sm st, notU U u = true -> S (IFun u) -> lk S (srcs sm) -> sm_ok U sm = true ->
     agx S d d' ->
     fol (xinterp_apply k P d u th sm st)
     \/ arel (xinterp_apply k P' d' u th sm st) (xinterp_apply k P d u th sm st))
  /\ (forall S d d' g st u brs, notU U u = true -> S (IFun u) -> brs_okb brs = true -> lk S (brs_uses brs) ->
     agx S d d' ->
     fol (xinterp_branch k P d g st u brs)
     \/ rrel (xinterp_branch k P' d' g st u (Tbrs brs)) (xinterp_branch k P d g st u brs)).

Lemma lk_incl : forall S l l', lk S l -> incl l' l -> lk S l'.
Proof. intros S l l' Hl Hi x Hx. apply Hl, Hi, Hx. Qed.

Lemma look_nonU : forall S d d' i, agx S d d' -> S i -> isU U i = false ->
  match lookup_xdenv i d, lookup_xdenv i d' with
  | Some a, Some a' => ent U H a a'
  | None, None => True
  | _, _ => False
  end.
Proof.
  intros S d d' i Hd Hi Hu. pose proof (Hd i Hi) as Ho.
  destruct (lookup_xdenv i d), (lookup_xdenv i d'); simpl in Ho; auto.
  eapply kent_ent; eauto.
Qed.

Lemma find_xarm_map : forall n arms,
  find_xarm n (map (fun a => match a with (n, x, b) => (n, x, T b) end) arms)
  = option_map (fun p => (fst p, T (snd p))) (find_xarm n arms).
Proof. induction arms as [|[[n' x] b] r IH]; simpl; auto. destruct (Nat.eqb n n'); auto. Qed.

Lemma find_xarm_in : forall n arms x b, find_xarm n arms = Some (x, b) -> exists n', In (n', x, b) arms.
Proof.
  induction arms as [|[[n' x'] b'] r IH]; simpl; intros x b Hf; [discriminate|].
  destruct (Nat.eqb n n'); [injection Hf as <- <-; eauto|].
  destruct (IH _ _ Hf) as [m Hm]; eauto.
Qed.

Lemma select_arm_map : forall arms w,
  select_arm (map (fun p => (fst p, T (snd p))) arms) w
  = option_map (fun p => (fst p, T (snd p))) (select_arm arms w).
Proof.
  induction arms as [|[p b] r IH]; intros w; simpl; auto. destruct (match_pat p w); simpl; auto.
Qed.

Lemma select_arm_in : forall arms w bs b, select_arm arms w = Some (bs, b) -> exists p, In (p, b) arms.
Proof.
  induction arms as [|[p b'] r IH]; simpl; intros w bs b Hs; [discriminate|].
  destruct (match_pat p w); [injection Hs as <- <-; eauto| |discriminate].
  destruct (IH _ _ _ Hs) as [q Hq]; eauto.
Qed.

Lemma find_branch_map : forall h brs,
  find_branch h (Tbrs brs) = option_map (fun p => (map Tb (fst p), snd p)) (find_branch h brs).
Proof. induction brs as [|[[h' bs] pr] r IH]; simpl; auto. destruct (Nat.eqb h h'); auto. Qed.

Lemma find_branch_in : forall h brs bs pr, find_branch h brs = Some (bs, pr) -> exists h', In (h', bs, pr) brs.
Proof.
  induction brs as [|[[h' bs'] pr'] r IH]; simpl; intros bs pr Hf; [discriminate|].
  destruct (Nat.eqb h h'); [injection Hf as <- <-; eauto|].
  destruct (IH _ _ Hf) as [m Hm]; eauto.
Qed.

Lemma forallb_snd_map : forall (f : xexpr -> bool) (fs : list (fld * xexpr)),
  forallb (fun p => f (snd p)) fs = forallb f (map snd fs).
Proof. induction fs as [|p r IH]; simpl; auto. rewrite IH. auto. Qed.

Lemma truthy_eq : forall w, truthy P' w = truthy P w.
Proof. intros. unfold truthy. rewrite Htrue. reflexivity. Qed.

Ltac andb_split :=
  repeat match goal with Hx : (_ && _) = true |- _ => apply andb_true_iff in Hx as [? ?] end.

(* If a sub-evaluation of the original runs out of fuel, so does the whole. *)
Ltac fout :=
  match goal with
  | Hs : fo ?r |- _ =>
      left; revert Hs; destruct r as [?o ?gg ?ss]; unfold fo; simpl; intros ->; reflexivity
  | Hs : fol ?r |- _ =>
      left; revert Hs; destruct r as [?|[?o ?gg ?ss]]; simpl; [intros []|];
      unfold fo; simpl; intros ->; reflexivity
  end.

Ltac err := right; reflexivity.
Ltac sub Hlk := apply (lk_incl Hlk); intros ?x ?Hx; simpl; rewrite ?in_app_iff; tauto.

Lemma sim_step : forall k, SIM k -> SIM (S k).
Proof.
  intros k [IHe [IHl [IHb [IHa IHr]]]].
  split; [|split; [|split; [|split]]].
  - intros S d d' g st e Hok Hlk Hd.
    destruct e as [x| |v|n th e1|f th sm args|f args|e0 m it args|e0 arms|x e1 e2|t tau e1|v e1 e2
                  |f f' sm e1|tk m th f' sm e1|u h sm smc e1|u u' sm e1|u th sm e1|u inputs outputs e1|e1 e2|x e1|c e1 e2
                  |c b|b|e1| |fs|e1 fl|e0 arms|u h e1|u u' e1|u brs e1|i p e1|id e1];
      simpl in Hok |- *; try discriminate; andb_split.
    + (* XVar *) err.
    + (* XUnit *) err.
    + (* XVl *)
      pose proof (look_nonU (IVal v) Hd (Hlk _ (or_introl eq_refl)) eq_refl) as Ho.
      destruct (lookup_xdenv (IVal v) d) as [a|], (lookup_xdenv (IVal v) d') as [a'|];
        try contradiction; [|err].
      inversion Ho; subst; err.
    + (* XTag *)
      destruct (IHe S d d' g st e1 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
      destruct (xinterp k P d g st e1) as [[]]; try err. rewrite (cast_eq n th x Hd). err.
    + (* XCallD *)
      rewrite Hfn. destruct (xp_fn P f) as [body|] eqn:Ef; simpl; [|err].
      destruct (IHl S d d' g st args ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
      destruct (xinterp_list k P d g st args) as [[[ws g1] st1]|]; [|err].
      pose proof (@compose_agx U H S d d' sm Hd ltac:(sub Hlk) ltac:(assumption)) as Hc.
      destruct (xcompose d sm) as [dc|], (xcompose d' sm) as [dc'|]; try contradiction; [|err].
      destruct (IHe (fun _ => True) (xcall_env P d th dc) (xcall_env P' d' th dc') (mk_xvenv ws) st1 body (Hokf f Ef) (fun _ _ => I)
                    (lr_agx _ (lr_call P P' th Hmode Hd Hc))) as [Hs| ->]; [fout|err].
    + (* XCallA *)
      pose proof (look_nonU (IFn f) Hd (Hlk _ (or_introl eq_refl)) eq_refl) as Ho.
      destruct (lookup_xdenv (IFn f) d) as [a|], (lookup_xdenv (IFn f) d') as [a'|];
        try contradiction; [|err].
      inversion Ho as [w|q|q tau0|hh|f0 dc dc' Hdc|hh sms dc dc' Hsms Hdc|tau]; subst; try err.
      * destruct (IHl S d d' g st args ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
        destruct (xinterp_list k P d g st args) as [[[ws g1] st1]|]; [|err].
        rewrite prim_at_eq. err.
      * destruct (IHl S d d' g st args ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
        destruct (xinterp_list k P d g st args) as [[[ws g1] st1]|]; [|err].
        rewrite prim_at_eq. err.
      * rewrite Hfn. destruct (xp_fn P f0) as [body|] eqn:Ef; simpl; [|err].
        destruct (IHl S d d' g st args ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
        destruct (xinterp_list k P d g st args) as [[[ws g1] st1]|]; [|err].
        destruct (IHe (fun _ => True) dc dc' (mk_xvenv ws) st1 body (Hokf f0 Ef) (fun _ _ => I)
                      (lr_agx _ Hdc)) as [Hs| ->]; [fout|err].
    + (* XMeth *)
      pose proof (Hd it (Hlk it (or_introl eq_refl))) as Ho.
      destruct (lookup_xdenv it d) as [a|], (lookup_xdenv it d') as [a'|]; simpl in Ho;
        try contradiction; [|err].
      inversion Ho as [? Hu|? ? ? Hu He]; subst; [err|].
      inversion He as [w|q|q tau0|hh|f0 dc dc' Hdc|hh sms dc dc' Hsms Hdc|tau]; subst; try err.
      * destruct (IHe S d d' g st e0 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
        destruct (xinterp k P d g st e0) as [[w0| | | | | | |] g0 st0]; try err.
        destruct (IHl S d d' g0 st0 args ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
        destruct (xinterp_list k P d g0 st0 args) as [[[ws g1] st1]|]; [|err].
        rewrite prim_at_eq. err.
      * destruct (IHe S d d' g st e0 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
        destruct (xinterp k P d g st e0) as [[w0| | | | | | |] g0 st0]; try err.
        destruct (IHl S d d' g0 st0 args ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
        destruct (xinterp_list k P d g0 st0 args) as [[[ws g1] st1]|]; [|err].
        rewrite prim_at_eq. err.
      * rewrite Hfn. destruct (xp_fn P f0) as [body|] eqn:Ef; simpl; [|err].
        destruct (IHe S d d' g st e0 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
        destruct (xinterp k P d g st e0) as [[w0| | | | | | |] g0 st0]; try err.
        destruct (IHl S d d' g0 st0 args ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
        destruct (xinterp_list k P d g0 st0 args) as [[[ws g1] st1]|]; [|err].
        destruct (IHe (fun _ => True) dc dc' (mk_xvenv (w0 :: ws)) st1 body (Hokf f0 Ef) (fun _ _ => I)
                      (lr_agx _ Hdc)) as [Hs| ->]; [fout|err].
    + (* XMatch *)
      destruct (IHe S d d' g st e0 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
      destruct (xinterp k P d g st e0) as [[w0| | | | | | |] g0 st0]; try err.
      destruct w0; try err.
      rewrite find_xarm_map. destruct (find_xarm n arms) as [[x eb]|] eqn:Ea; simpl; [|err].
      destruct (find_xarm_in _ _ Ea) as [n' Hin].
      assert (Hb : ok_e U eb = true).
      { match goal with Hf : forallb _ arms = true |- _ => rewrite forallb_forall in Hf; exact (Hf _ Hin) end. }
      assert (Hl : lk S (uses eb)).
      { apply (lk_incl Hlk). intros y Hy. simpl. apply in_app_iff. right.
        apply in_flat_map. exists (n', x, eb). auto. }
      destruct (IHe S d d' ((x, w0) :: g0) st0 eb Hb Hl Hd) as [Hs| ->]; [fout|err].
    + (* XLet *)
      destruct (IHe S d d' g st e1 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; try err.
      destruct (IHe S d d' ((x, w0) :: g0) st0 e2 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->];
        [fout|err].
    + (* XBindTy *) apply (IHe S); [assumption|sub Hlk|eapply agx_bind_ty; eauto].
    + (* XBindVal *)
      destruct (IHe S d d' g st e1 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; try err.
      apply (IHe S); [assumption|sub Hlk|]. apply agx_cons; auto. apply KO; [reflexivity|constructor].
    + (* XBindFn *)
      pose proof (@compose_agx U H S d d' sm Hd ltac:(sub Hlk) ltac:(assumption)) as Hc.
      destruct (xcompose d sm) as [dc|], (xcompose d' sm) as [dc'|]; try contradiction; [|err].
      apply (IHe S); [assumption|sub Hlk|]. apply agx_cons; auto. apply KO; [reflexivity|constructor; eapply lr_close; eauto].
    + (* XBindMth *)
      pose proof (@compose_agx U H S d d' sm Hd ltac:(sub Hlk) ltac:(assumption)) as Hc.
      destruct (xcompose d sm) as [dc|], (xcompose d' sm) as [dc'|]; try contradiction; [|err].
      apply (IHe S); [assumption|sub Hlk|]. apply agx_cons; auto. apply KO; [reflexivity|constructor; eapply lr_close; eauto].
    + (* XBindFun *)
      pose proof (@compose_agx U H S d d' smc Hd ltac:(sub Hlk) ltac:(assumption)) as Hc.
      destruct (xcompose d smc) as [dc|], (xcompose d' smc) as [dc'|]; try contradiction; [|err].
      apply (IHe S); [assumption|sub Hlk|]. apply agx_cons; auto.
      apply KO; [apply notU_isU; auto|constructor; [simpl; rewrite andb_true_r; assumption|eapply lr_close; eauto]].
    + (* XBindFunA *)
      pose proof (look_nonU (IFun u') Hd (Hlk _ (or_introl eq_refl)) (notU_isU _ _ ltac:(eassumption))) as Ho.
      destruct (lookup_xdenv (IFun u') d) as [a|], (lookup_xdenv (IFun u') d') as [a'|];
        try contradiction; [|err].
      inversion Ho as [| | | | |h0 sms dc dc' Hsms Hdc|tau]; subst; try err.
      apply (IHe S); [assumption|sub Hlk|]. apply agx_cons; auto.
      apply KO; [apply notU_isU; auto|constructor; auto]. rewrite forallb_app. simpl.
      rewrite Hsms. simpl. rewrite andb_true_r. auto.
    + (* XApply *)
      destruct (Nat.eqb u U) eqn:Eu; simpl.
      * apply Nat.eqb_eq in Eu. subst u.
        match goal with Hx : (notU U U || _) = true |- _ => unfold notU in Hx; rewrite Nat.eqb_refl in Hx;
          simpl in Hx; destruct sm; [|discriminate] end.
        destruct (@uapp k S d d' g st th (inW (uses e1)) (Hlk _ (or_introl eq_refl)) Hd) as [Hs|Hu]; [fout|].
        destruct (xinterp_apply k P d U th [] st) as [[dr st1]|r1],
                 (xinterp_branch k P' d' g st U _) as [[[dr' g1] st1']|r1']; try contradiction.
        -- destruct Hu as [-> [-> [-> Hpe]]].
           apply (IHe (fun i => if inkeys i dr then inW (uses e1) i = true else S i)); auto.
           ++ intros i Hi. destruct (inkeys i dr).
              ** apply existsb_exists. exists i. split; auto. apply item_eqb_refl.
              ** apply Hlk. simpl. auto.
           ++ apply agx_narrow; auto.
        -- subst. err.
      * assert (Hn : notU U u = true) by (unfold notU; rewrite Eu; reflexivity).
        destruct (IHa S d d' u th sm st Hn (Hlk _ (or_introl eq_refl)) ltac:(sub Hlk) ltac:(assumption) Hd)
          as [Hs|Ha]; [fout|].
        destruct (xinterp_apply k P' d' u th sm st) as [[dr' st1']|r1'],
                 (xinterp_apply k P d u th sm st) as [[dr st1]|r1]; simpl in Ha; try contradiction.
        -- destruct Ha as [Hdr ->]. apply (IHe S); [assumption|sub Hlk|]. apply agx_app; auto.
        -- subst. err.
    + (* XSeq *)
      destruct (IHe S d d' g st e1 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; try err.
      apply (IHe S); [assumption|sub Hlk|assumption].
    + (* XAssign *)
      destruct (IHe S d d' g st e1 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|err].
    + (* XIf *)
      destruct (IHe S d d' g st c ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
      destruct (xinterp k P d g st c) as [[w0| | | | | | |] g0 st0]; try err.
      rewrite truthy_eq. destruct (truthy P w0) as [[]|]; try err; apply (IHe S); auto; sub Hlk.
    + (* XWhile *)
      destruct (IHe S d d' g st c ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
      destruct (xinterp k P d g st c) as [[w0| | | | | | |] g0 st0]; try err.
      rewrite truthy_eq. destruct (truthy P w0) as [[]|]; try err.
      destruct (IHe S d d' g0 st0 b ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
      destruct (xinterp k P d g0 st0 b) as [[w1| | | | | | |] g1 st1]; try err.
      change (XWhile (T c) (T b)) with (T (XWhile c b)).
      apply (IHe S); simpl; auto. apply andb_true_iff; split; assumption.
    + (* XLoop *)
      destruct (IHe S d d' g st b ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
      destruct (xinterp k P d g st b) as [[w0| | | | | | |] g0 st0]; try err.
      change (XLoop (T b)) with (T (XLoop b)).
      apply (IHe S); simpl; auto.
    + (* XReturn *)
      destruct (IHe S d d' g st e1 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|err].
    + (* XBreak *) err.
    + (* XRec *)
      rewrite !map_map. simpl.
      replace (map (fun x => T (snd x)) fs) with (map T (map snd fs)) by (rewrite map_map; reflexivity).
      assert (Hok' : forallb (ok_e U) (map snd fs) = true).
      { rewrite <- forallb_snd_map. assumption. }
      assert (Hl : lk S (flat_map uses (map snd fs))).
      { apply (lk_incl Hlk). intros y Hy. apply in_flat_map in Hy as [e [He Hy]].
        apply in_map_iff in He as [q [<- Hq]]. simpl. apply in_flat_map. eauto. }
      destruct (IHl S d d' g st (map snd fs) Hok' Hl Hd) as [Hs| ->]; [fout|err].
    + (* XField *)
      destruct (IHe S d d' g st e1 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|err].
    + (* XMatchP *)
      destruct (IHe S d d' g st e0 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
      destruct (xinterp k P d g st e0) as [[w0| | | | | | |] g0 st0]; try err.
      rewrite select_arm_map. destruct (select_arm arms w0) as [[bs eb]|] eqn:Ea; simpl; [|err].
      destruct (select_arm_in _ _ Ea) as [q Hin].
      assert (Hb : ok_e U eb = true).
      { match goal with Hf : forallb _ arms = true |- _ => rewrite forallb_forall in Hf; exact (Hf _ Hin) end. }
      assert (Hl : lk S (uses eb)).
      { apply (lk_incl Hlk). intros y Hy. simpl. apply in_app_iff. right.
        apply in_flat_map. exists (q, eb). auto. }
      destruct (IHe S d d' (bs ++ g0) st0 eb Hb Hl Hd) as [Hs| ->]; [fout|err].
    + (* XBindFunSym *)
      apply (IHe S); [assumption|sub Hlk|]. apply agx_cons; auto.
      apply KO; [apply notU_isU; auto|constructor].
    + (* XBindFunCopy *)
      pose proof (look_nonU (IFun u') Hd (Hlk _ (or_introl eq_refl)) (notU_isU _ _ ltac:(eassumption))) as Ho.
      destruct (lookup_xdenv (IFun u') d) as [a|], (lookup_xdenv (IFun u') d') as [a'|];
        try contradiction; [|err].
      apply (IHe S); [assumption|sub Hlk|]. apply agx_cons; auto. apply KO; auto. apply notU_isU; auto.
    + (* XApplyBr *)
      destruct (IHr S d d' g st u brs ltac:(assumption) (Hlk _ (or_introl eq_refl)) ltac:(assumption)
                  ltac:(unfold brs_uses; sub Hlk) Hd) as [Hs|Hr]; [fout|].
      destruct (xinterp_branch k P' d' g st u _) as [[[dr' g1'] st1']|r1'],
               (xinterp_branch k P d g st u brs) as [[[dr g1] st1]|r1]; simpl in Hr; try contradiction.
      * destruct Hr as [Hdr [-> ->]]. apply (IHe S); [assumption|sub Hlk|]. apply agx_app; auto.
      * subst. err.
    + (* XBindPrim *)
      apply (IHe S); [assumption|sub Hlk|]. apply agx_cons; auto.
      apply KO; [apply negb_true_iff; auto|].
      eapply agx_prim; exact Hd.
    + (* XConst *) destruct k;[err|apply(IHe S);[assumption|sub Hlk|exact Hd]].
  - intros S d d' g st l Hok Hlk Hd. destruct l as [|e r]; simpl in Hok |- *; [err|]. andb_split.
    destruct (IHe S d d' g st e ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
    destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; try err.
    destruct (IHl S d d' g0 st0 r ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [|err].
    left. revert Hs. destruct (xinterp_list k P d g0 st0 r) as [?|[o0 g1 sx2]]; simpl;
      [intros []|]. intros Hx; exact Hx.
  - intros S d d' g st bs Hok Hlk Hd. destruct bs as [|b r]; simpl in Hok |- *; [right; simpl; auto|].
    andb_split.
    destruct b as [t tau|v e1|f f' sm|tk m th f' sm|u h sm smc|u u' sm|u th sm|u inputs outputs|u h|u u'|u brs|i p];
      match goal with Hb : ok_b _ _ = true |- _ => simpl in Hb |- * end; try discriminate; andb_split.
    + (* XBTy *) apply (IHb S); [assumption|sub Hlk|eapply agx_bind_ty; eauto].
    + (* XBVal *)
      destruct (IHe S d d' g st e1 ltac:(assumption) ltac:(sub Hlk) Hd) as [Hs| ->]; [fout|].
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; try (right; reflexivity).
      apply (IHb S); [assumption|sub Hlk|]. apply agx_cons; auto. apply KO; [reflexivity|constructor].
    + (* XBFn *)
      pose proof (@compose_agx U H S d d' sm Hd ltac:(sub Hlk) ltac:(assumption)) as Hc.
      destruct (xcompose d sm) as [dc|], (xcompose d' sm) as [dc'|]; try contradiction;
        [|right; reflexivity].
      apply (IHb S); [assumption|sub Hlk|]. apply agx_cons; auto. apply KO; [reflexivity|constructor; eapply lr_close; eauto].
    + (* XBMth *)
      pose proof (@compose_agx U H S d d' sm Hd ltac:(sub Hlk) ltac:(assumption)) as Hc.
      destruct (xcompose d sm) as [dc|], (xcompose d' sm) as [dc'|]; try contradiction;
        [|right; reflexivity].
      apply (IHb S); [assumption|sub Hlk|]. apply agx_cons; auto. apply KO; [reflexivity|constructor; eapply lr_close; eauto].
    + (* XBFun *)
      pose proof (@compose_agx U H S d d' smc Hd ltac:(sub Hlk) ltac:(assumption)) as Hc.
      destruct (xcompose d smc) as [dc|], (xcompose d' smc) as [dc'|]; try contradiction;
        [|right; reflexivity].
      apply (IHb S); [assumption|sub Hlk|]. apply agx_cons; auto.
      apply KO; [apply notU_isU; auto|constructor; [simpl; rewrite andb_true_r; assumption|eapply lr_close; eauto]].
    + (* XBFunA *)
      pose proof (look_nonU (IFun u') Hd (Hlk _ (or_introl eq_refl)) (notU_isU _ _ ltac:(eassumption))) as Ho.
      destruct (lookup_xdenv (IFun u') d) as [a|], (lookup_xdenv (IFun u') d') as [a'|];
        try contradiction; [|right; reflexivity].
      inversion Ho as [| | | | |h0 sms dc dc' Hsms Hdc|tau]; subst; try (right; reflexivity).
      apply (IHb S); [assumption|sub Hlk|]. apply agx_cons; auto.
      apply KO; [apply notU_isU; auto|constructor; auto]. rewrite forallb_app. simpl.
      rewrite Hsms. simpl. rewrite andb_true_r. auto.
    + (* XBApply *)
      destruct (Nat.eqb u U) eqn:Eu; simpl.
      * apply Nat.eqb_eq in Eu. subst u.
        match goal with Hx : (notU U U || _) = true |- _ => unfold notU in Hx; rewrite Nat.eqb_refl in Hx;
          simpl in Hx; destruct sm; [|discriminate] end.
        destruct (@uapp k S d d' g st th (fun _ => true) (Hlk _ (or_introl eq_refl)) Hd) as [Hs|Hu]; [fout|].
        destruct (xinterp_apply k P d U th [] st) as [[dr st1]|r1],
                 (xinterp_branch k P' d' g st U _) as [[[dr' g1] st1']|r1']; try contradiction.
        -- destruct Hu as [-> [-> [-> Hpe]]]. rewrite filter_true.
           apply (IHb S); [assumption|sub Hlk|]. apply agx_app; auto. apply lr_prims; auto.
        -- subst. right. reflexivity.
      * assert (Hn : notU U u = true) by (unfold notU; rewrite Eu; reflexivity).
        destruct (IHa S d d' u th sm st Hn (Hlk _ (or_introl eq_refl)) ltac:(sub Hlk) ltac:(assumption) Hd)
          as [Hs|Ha]; [fout|].
        destruct (xinterp_apply k P' d' u th sm st) as [[dr' st1']|r1'],
                 (xinterp_apply k P d u th sm st) as [[dr st1]|r1]; simpl in Ha; try contradiction.
        -- destruct Ha as [Hdr ->]. apply (IHb S); [assumption|sub Hlk|]. apply agx_app; auto.
        -- subst. right. reflexivity.
    + (* XBFunSym *)
      apply (IHb S); [assumption|sub Hlk|]. apply agx_cons; auto.
      apply KO; [apply notU_isU; auto|constructor].
    + (* XBFunCopy *)
      pose proof (look_nonU (IFun u') Hd (Hlk _ (or_introl eq_refl)) (notU_isU _ _ ltac:(eassumption))) as Ho.
      destruct (lookup_xdenv (IFun u') d) as [a|], (lookup_xdenv (IFun u') d') as [a'|];
        try contradiction; [|right; reflexivity].
      apply (IHb S); [assumption|sub Hlk|]. apply agx_cons; auto. apply KO; auto. apply notU_isU; auto.
    + (* XBApplyBr *)
      destruct (IHr S d d' g st u brs ltac:(assumption) (Hlk _ (or_introl eq_refl)) ltac:(assumption)
                  ltac:(unfold brs_uses; sub Hlk) Hd) as [Hs|Hr]; [fout|].
      destruct (xinterp_branch k P' d' g st u _) as [[[dr' g1'] st1']|r1'],
               (xinterp_branch k P d g st u brs) as [[[dr g1] st1]|r1]; simpl in Hr; try contradiction.
      * destruct Hr as [Hdr [-> ->]]. apply (IHb S); [assumption|sub Hlk|]. apply agx_app; auto.
      * subst. right. reflexivity.
    + (* XBPrim *)
      apply (IHb S); [assumption|sub Hlk|]. apply agx_cons; auto.
      apply KO; [apply negb_true_iff; auto|].
      eapply agx_prim; exact Hd.
  - intros S d d' u th sm st Hn Hu Hlk Hsm Hd. simpl.
    pose proof (look_nonU (IFun u) Hd Hu (notU_isU _ _ Hn)) as Ho.
    destruct (lookup_xdenv (IFun u) d) as [a|], (lookup_xdenv (IFun u) d') as [a'|];
      try contradiction; [|right; reflexivity].
    inversion Ho as [| | | | |h sms dc dc' Hsms Hdc|tau]; subst; try (right; reflexivity).
    rewrite Hfun. destruct (xp_fun P h) as [[D bs]|] eqn:Ef; simpl; [|right; reflexivity].
    pose proof (@compose_agx U H S d d' sm Hd Hlk Hsm) as Hc.
    destruct (xcompose d sm) as [d1|], (xcompose d' sm) as [d1'|]; try contradiction;
      [|right; reflexivity].
    pose proof (@chain_lr U H sms d1 d1' Hc Hsms) as Hch.
    destruct (xrun_chain d1 sms) as [d2|], (xrun_chain d1' sms) as [d2'|]; try contradiction;
      [|right; reflexivity].
    destruct (IHb (fun _ => True) (xapply_env P d h th (d2 ++ dc)) (xapply_env P' d' h th (d2' ++ dc')) [] st bs (Hoku h Ef) (fun _ _ => I)
                  (lr_agx _ (apply_lr h th Hd (lr_app Hch Hdc)))) as [Hs|Hb].
    + left. revert Hs. destruct (xinterp_binds k P (xapply_env P d h th (d2 ++ dc)) [] st bs) as [?|[o0 g0 sx1]]; simpl;
        [intros []|]. intros Hx; exact Hx.
    + destruct (xinterp_binds k P' (xapply_env P' d' h th (d2' ++ dc')) [] st (map Tb bs)) as [[[d3' g3'] st3']|r3'],
               (xinterp_binds k P (xapply_env P d h th (d2 ++ dc)) [] st bs) as [[[d3 g3] st3]|r3]; simpl in Hb;
        try contradiction.
      * destruct Hb as [Hd3 [-> ->]].
        pose proof (@compose_agx U H (fun _ => True) d3 d3' (inst_smap th D) Hd3 (fun _ _ => I)
                      (sm_ok_inst U th D)) as Hc3.
        destruct (xcompose d3 (inst_smap th D)) as [dr|], (xcompose d3' (inst_smap th D)) as [dr'|];
          try contradiction; right; simpl; auto.
        split; [eapply lr_result; eauto|reflexivity].
      * subst. right. simpl. auto.
  - intros S d d' g st u brs Hn Hu Hb Hlk Hd. simpl.
    pose proof (look_nonU (IFun u) Hd Hu (notU_isU _ _ Hn)) as Ho.
    destruct (lookup_xdenv (IFun u) d) as [a|], (lookup_xdenv (IFun u) d') as [a'|];
      try contradiction; [|right; reflexivity].
    inversion Ho; subst; try (right; reflexivity).
    rewrite find_branch_map. destruct (find_branch h brs) as [[bs pr]|] eqn:Ef; simpl;
      [|right; reflexivity].
    destruct (find_branch_in _ _ Ef) as [h' Hin].
    unfold brs_okb in Hb. rewrite forallb_forall in Hb. specialize (Hb _ Hin). simpl in Hb.
    apply andb_true_iff in Hb as [Hbs Hpr].
    assert (Hl : lk S (flat_map buses bs ++ srcs pr)).
    { apply (lk_incl Hlk). intros y Hy. unfold brs_uses. apply in_flat_map. exists (h', bs, pr). auto. }
    destruct (IHb S d d' g st bs Hbs (lk_incl Hl (fun y Hy => in_or_app _ _ _ (or_introl Hy))) Hd)
      as [Hs|Hb'].
    + left. revert Hs. destruct (xinterp_binds k P d g st bs) as [?|[o0 g0 sx1]]; simpl;
        [intros []|]. intros Hx; exact Hx.
    + destruct (xinterp_binds k P' d' g st (map Tb bs)) as [[[d3' g3'] st3']|r3'],
               (xinterp_binds k P d g st bs) as [[[d3 g3] st3]|r3]; simpl in Hb';
        try contradiction.
      * destruct Hb' as [Hd3 [-> ->]].
        pose proof (@compose_agx U H S d3 d3' pr Hd3
                      (lk_incl Hl (fun y Hy => in_or_app _ _ _ (or_intror Hy))) Hpr) as Hc3.
        destruct (xcompose d3 pr) as [dr|], (xcompose d3' pr) as [dr'|];
          try contradiction; right; simpl; auto.
      * subst. right. simpl. auto.
Qed.

Lemma sim_all : forall k, SIM k.
Proof.
  induction k as [|k IH].
  - repeat split; intros; left; reflexivity.
  - apply sim_step, IH.
Qed.

End Sim.

(* ===================== 6. The transformed program ======================== *)

Lemma lr_root : forall U H env, root_ok U H env = true -> lr U H env (fp_env U H env).
Proof.
  intros U H. induction env as [|[i a] r IH]; simpl; intros Hr; [constructor|].
  apply andb_true_iff in Hr as [Ha Hr]. simpl in Ha.
  destruct (isU U i) eqn:Ei; simpl; constructor; auto.
  - destruct a as [w|f dc|h sms dc|p|h|tau|p tau]; try discriminate.
    destruct sms; try discriminate. destruct dc; try discriminate.
    apply Nat.eqb_eq in Ha. subst. apply KU; auto.
  - apply KO; auto.
    destruct a as [w|f dc|h sms dc|p|h|tau|p tau]; simpl in Ha; try discriminate.
    + apply EVal.
    + destruct dc; [apply EClo; constructor|discriminate].
    + destruct dc; [apply EFun; [exact Ha|constructor]|discriminate].
    + apply EPrim.
    + apply ESym.
    + apply EType.
Qed.

Lemma fo_ret_call : forall g r, fo r -> fo (ret_call g r).
Proof. intros g [o g0 s] Hs. unfold fo in *. simpl in *. subst. reflexivity. Qed.

Definition run_of (p : xprogram) (argv : list (list int)) (rd : list int -> option (list int))
    (fuel : nat) (st0 : mstate) : xres :=
  run_main fuel (program_of p argv rd) (xo_main p) (xo_env p) st0.

(* One functor: the transformed program runs exactly as the original,
   unless the original runs out of fuel. *)
Theorem fp_run : forall U H p argv rd fuel st0, ok_program U H p = true ->
  fo (run_of p argv rd fuel st0) \/ run_of (fp_program U H p) argv rd fuel st0 = run_of p argv rd fuel st0.
Proof.
  intros U H p argv rd fuel st0 Hok. unfold ok_program in Hok.
  unfold fp_program, run_of.
  destruct (Elaboration.nlook H (xo_funs p)) as [[Dr bsH]|] eqn:EH; [|discriminate].
  apply andb_true_iff in Hok as [Hok Hroot]. apply andb_true_iff in Hok as [Hok Hfuns].
  apply andb_true_iff in Hok as [Hbody Hfns].
  apply andb_true_iff in Hbody as [Hresults Hbody]. cbn [xo_main xo_env].
  set (P := program_of p argv rd).
  set (P' := program_of (XProgram (map (fun fb => (fst fb, fp_e U H Dr bsH (snd fb))) (xo_fns p))
                (map (fun ub => (fst ub, (fst (snd ub), map (fp_b U H Dr bsH) (snd (snd ub))))) (xo_funs p))
                (xo_bool p) (xo_true p) (xo_false p) (xo_main p) (fp_env U H (xo_env p)) (xo_prims p) (xo_cast p) (xo_inputs p) (xo_exports p) (xo_immutable p) (xo_fixed_inst p) (xo_fixed_keys p))
              argv rd).
  assert (Hfn : forall f, xp_fn P' f = option_map (fp_e U H Dr bsH) (xp_fn P f)).
  { intro f. unfold P, P', program_of. simpl. apply nlook_map. }
  assert (Hfun : forall u, xp_fun P' u
            = option_map (fun q => (fst q, map (fp_b U H Dr bsH) (snd q))) (xp_fun P u)).
  { intro u. unfold P, P', program_of. simpl.
    rewrite (nlook_map (fun q : tele * list xbind => (fst q, map (fp_b U H Dr bsH) (snd q)))).
    reflexivity. }
  assert (Hprim : forall q ws st, run_prim P' q ws st = run_prim P q ws st)
    by (intros; apply run_prim_code).
  assert (Htrue : xp_true P' = xp_true P) by reflexivity.
  assert (Hmode : xreify P' = xreify P) by reflexivity.
  assert (Hcasts : xp_cast P' = xp_cast P) by reflexivity.
  assert (Hinputs : forall h, xp_inputs P' h = xp_inputs P h) by reflexivity.
  assert (Hokf : forall f b, xp_fn P f = Some b -> ok_e U b = true).
  { intros f b Hf. apply nlook_in in Hf. rewrite forallb_forall in Hfns. exact (Hfns (f, b) Hf). }
  assert (Hoku : forall u D bs, xp_fun P u = Some (D, bs) -> forallb (ok_b U) bs = true).
  { intros u D bs Hf. apply nlook_in in Hf. rewrite forallb_forall in Hfuns. exact (Hfuns (u, (D, bs)) Hf). }
  assert (HH : xp_fun P H = Some (Dr, bsH)) by exact EH.
  destruct (@sim_all U H Dr bsH P P' Hfn Hfun Hprim Htrue Hmode Hcasts Hinputs Hokf Hoku HH Hbody Hresults fuel) as [Hsim _].
  unfold run_main. rewrite Hfn. destruct (xp_fn P (xo_main p)) as [body|] eqn:Em; simpl;
    [|right; reflexivity].
  destruct (Hsim (fun _ => True) (xo_env p) (fp_env U H (xo_env p)) [] st0 body (Hokf _ _ Em)
              (fun _ _ => I) (lr_agx _ (lr_root _ _ _ Hroot))) as [Hs| ->];
    [left; apply fo_ret_call; exact Hs|right; reflexivity].
Qed.

(* What the oracle runs: every accepted functor transformed. *)
Theorem fast_run : forall p argv rd fuel st0,
  fo (run_of p argv rd fuel st0) \/ run_of (fast_program p) argv rd fuel st0 = run_of p argv rd fuel st0.
Proof.
  intros p argv rd fuel st0. unfold fast_program. generalize (candidates p) as cs. intro cs. revert p.
  induction cs as [|[u h] r IH]; intros p; simpl; [right; reflexivity|].
  set (q := if ok_program u h p then fp_program u h p else p).
  assert (Hq : fo (run_of p argv rd fuel st0) \/ run_of q argv rd fuel st0 = run_of p argv rd fuel st0).
  { unfold q. destruct (ok_program u h p) eqn:E; [apply fp_run; auto|right; reflexivity]. }
  destruct Hq as [Hq|Hq]; [left; exact Hq|].
  destruct (IH q) as [Hs|Hs]; [left; rewrite <- Hq; exact Hs|right; rewrite Hs; exact Hq].
Qed.

(* The default engine on the transformed, pruned program computes Audit's
   semantics of the elaborated program — unless that run runs out of fuel
   or gets stuck (Prune's caveat, CloX.engine_agrees). *)
Theorem fast_engine_agrees : forall p argv rd fuel st0 tb M N R' tbl ftbl,
  let q := fast_program p in
  let R := rprog_of (oracle_program q) argv rd in
  (forall f bs, tb f = Some bs -> exists bd, rp_fn R f = Some (bs, bd)) ->
  rp_x R' = rp_x R ->
  (forall f, rp_fn R' f =
     match rp_fn R f with Some (bs, bd) => Some (guard_fold_e (rp_x R) tb M N bs, guard_fold_e (rp_x R) tb M N bd) | None => None end) ->
  (forall h, rp_fun R' h =
     match rp_fun R h with Some (Dr, bs) => Some (Dr, map (guard_fold_b (rp_x R) tb M N) bs) | None => None end) ->
  tbl_ok R' tbl ftbl ->
  let P := program_of p argv rd in
  fo (run_main fuel P (xo_main p) (xo_env p) st0)
  \/ stuck (run_main fuel P (xo_main p) (xo_env p) st0)
  \/ crun_main tbl fuel (xo_main q) (xo_env q) st0 = run_main fuel P (xo_main p) (xo_env p) st0.
Proof.
  intros p argv rd fuel st0 tb M N R' tbl ftbl q R Htb Hx Hfn Hfun Hok P.
  destruct (fast_run p argv rd fuel st0) as [Hf|Hf]; [left; exact Hf|]. unfold run_of in Hf.
  destruct (@engine_agrees q argv rd fuel st0 tb M N R' tbl ftbl Htb Hx Hfn Hfun Hok) as [Hs|He];
    unfold P; rewrite <- Hf.
  - right; left; exact Hs.
  - right; right; exact He.
Qed.
