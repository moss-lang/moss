(* ------------------------------------------------------------------------ *)
(* Prune.v — read-set pruning of satisfaction maps (milestone 3, speed      *)
(* step 2).                                                                  *)
(*                                                                           *)
(* The elaborator passes each callee its whole context: a call's            *)
(* satisfaction map has ~150–400 entries (the whole of Std), and the        *)
(* interpreter composes all of them at every call.  A callee reads few of   *)
(* them.  Given a read set R(f) for every fn f, `prune_e` keeps, at each    *)
(* call of f (and each closure of f made by a bind), only the pairs whose   *)
(* requirement is in R(f).                                                   *)
(*                                                                           *)
(* The read sets are computed by an untrusted iteration (`compute_R`); what *)
(* is trusted is the checker `check R p`: every fn body reads only items in *)
(* its own read set, where a call of g reads the sources of the pairs kept *)
(* for g.  If the check fails the oracle runs the unpruned program.         *)
(*                                                                           *)
(* Proved (`run_main_pruned`): if `check R p` holds, running the pruned     *)
(* program gives exactly the result of running p — unless running p gets   *)
(* stuck (XErr).  A stuck reference run may become unstuck when pruned (a   *)
(* dropped pair whose source was missing is no longer looked up).  Audit's  *)
(* pending obligation `accepted_never_err` says accepted programs never get *)
(* stuck; with it the agreement is unconditional on accepted programs.      *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit CoreMoss MossX FastX Elab.
Import ListNotations.

Set Implicit Arguments.
Local Open Scope nat_scope.

(* ===================== 1. Read sets ===================================== *)

(* Sets of items, as FastX's map-keyed environments (the entry is unused). *)
Definition iset := fenv.
Definition imem (i : item) (s : iset) : bool :=
  match flookup i s with Some _ => true | None => false end.
Definition iadd (i : item) (s : iset) : iset := if imem i s then s else fadd i (FFunSym 0) s.

Definition rsets := tmap iset.
Definition inR (R : rsets) (f : fsym) (i : item) : bool :=
  match tget R f with Some s => imem i s | None => false end.

Definition keep (R : rsets) (g : fsym) (sm : smap) : smap :=
  filter (fun p => inR R g (fst p)) sm.

Fixpoint prune_e (R : rsets) (e : xexpr) : xexpr :=
  match e with
  | XVar x => XVar x
  | XUnit => XUnit
  | XVl v => XVl v
  | XTag n th e1 => XTag n th (prune_e R e1)
  | XCallD f th sm args => XCallD f th (keep R f sm) (map (prune_e R) args)
  | XCallA f args => XCallA f (map (prune_e R) args)
  | XMeth e0 m it args => XMeth (prune_e R e0) m it (map (prune_e R) args)
  | XMatch e0 arms =>
      XMatch (prune_e R e0) (map (fun a => match a with (n, x, b) => (n, x, prune_e R b) end) arms)
  | XLet x e1 e2 => XLet x (prune_e R e1) (prune_e R e2)
  | XBindTy t tau e1 => XBindTy t tau (prune_e R e1)
  | XBindVal v e1 e2 => XBindVal v (prune_e R e1) (prune_e R e2)
  | XBindFn f f' sm e1 => XBindFn f f' (keep R f' sm) (prune_e R e1)
  | XBindMth tk m th f' sm e1 => XBindMth tk m th f' (keep R f' sm) (prune_e R e1)
  | XBindFun u h sm smc e1 => XBindFun u h sm smc (prune_e R e1)
  | XBindFunA u u' sm e1 => XBindFunA u u' sm (prune_e R e1)
  | XApply u th sm e1 => XApply u th sm (prune_e R e1)
  | XSeq e1 e2 => XSeq (prune_e R e1) (prune_e R e2)
  | XAssign x e1 => XAssign x (prune_e R e1)
  | XIf c e1 e2 => XIf (prune_e R c) (prune_e R e1) (prune_e R e2)
  | XWhile c b => XWhile (prune_e R c) (prune_e R b)
  | XLoop b => XLoop (prune_e R b)
  | XReturn e1 => XReturn (prune_e R e1)
  | XBreak => XBreak
  | XRec fs => XRec (map (fun p => (fst p, prune_e R (snd p))) fs)
  | XField e1 f => XField (prune_e R e1) f
  | XMatchP e0 arms => XMatchP (prune_e R e0) (map (fun p => (fst p, prune_e R (snd p))) arms)
  | XBindFunSym u h e1 => XBindFunSym u h (prune_e R e1)
  | XBindFunCopy u u' e1 => XBindFunCopy u u' (prune_e R e1)
  | XApplyBr u brs e1 =>
      XApplyBr u (map (fun br => match br with (h, bs, pr) => (h, map (prune_b R) bs, pr) end) brs)
               (prune_e R e1)
  | XBindPrim i p e1 => XBindPrim i p (prune_e R e1)
  end
with prune_b (R : rsets) (b : xbind) : xbind :=
  match b with
  | XBVal v e1 => XBVal v (prune_e R e1)
  | XBFn f f' sm => XBFn f f' (keep R f' sm)
  | XBMth tk m th f' sm => XBMth tk m th f' (keep R f' sm)
  | XBApplyBr u brs =>
      XBApplyBr u (map (fun br => match br with (h, bs, pr) => (h, map (prune_b R) bs, pr) end) brs)
  | b => b
  end.

(* The checker: every item `e` looks up, in an environment that agrees
   with the unpruned one on `inS`, is in `inS`. *)
Section Check.
Variable R : rsets.
Variable inS : item -> bool.
Definition srcs_ok (g : fsym) (sm : smap) : bool :=
  forallb (fun p => negb (inR R g (fst p)) || inS (snd p)) sm.
Definition all_ok (sm : smap) : bool := forallb (fun p => inS (snd p)) sm.

Fixpoint nchk (e : xexpr) : bool :=
  match e with
  | XVar _ | XUnit | XBreak => true
  | XVl v => inS (IVal v)
  | XTag _ _ e1 | XBindTy _ _ e1 | XAssign _ e1 | XLoop e1 | XReturn e1 | XField e1 _
  | XBindFunSym _ _ e1 | XBindPrim _ _ e1 => nchk e1
  | XCallD f _ sm args => srcs_ok f sm && forallb nchk args
  | XCallA f args => inS (IFn f) && forallb nchk args
  | XMeth e0 _ it args => inS it && nchk e0 && forallb nchk args
  | XMatch e0 arms => nchk e0 && forallb (fun a => match a with (_, _, b) => nchk b end) arms
  | XLet _ a b | XBindVal _ a b | XSeq a b | XWhile a b => nchk a && nchk b
  | XBindFn _ f' sm e1 | XBindMth _ _ _ f' sm e1 => srcs_ok f' sm && nchk e1
  | XBindFun _ _ _ smc e1 => all_ok smc && nchk e1
  | XBindFunA _ u' _ e1 | XBindFunCopy _ u' e1 => inS (IFun u') && nchk e1
  | XApply u _ sm e1 => inS (IFun u) && all_ok sm && nchk e1
  | XIf a b c => nchk a && nchk b && nchk c
  | XRec fs => forallb (fun p => nchk (snd p)) fs
  | XMatchP e0 arms => nchk e0 && forallb (fun p => nchk (snd p)) arms
  | XApplyBr u brs e1 =>
      inS (IFun u)
      && forallb (fun br => match br with (_, bs, pr) => forallb bchk bs && all_ok pr end) brs
      && nchk e1
  end
with bchk (b : xbind) : bool :=
  match b with
  | XBTy _ _ | XBFunSym _ _ | XBPrim _ _ => true
  | XBVal _ e1 => nchk e1
  | XBFn _ f' sm | XBMth _ _ _ f' sm => srcs_ok f' sm
  | XBFun _ _ _ smc => all_ok smc
  | XBFunA _ u' _ | XBFunCopy _ u' => inS (IFun u')
  | XBApply u _ sm => inS (IFun u) && all_ok sm
  | XBApplyBr u brs =>
      inS (IFun u)
      && forallb (fun br => match br with (_, bs, pr) => forallb bchk bs && all_ok pr end) brs
  end.
End Check.

Definition full (_ : item) : bool := true.

(* ===================== 2. Computing read sets (untrusted) ================ *)

Section Compute.
Variable R : rsets.
Definition add_srcs (g : fsym) (sm : smap) (s : iset) : iset :=
  fold_left (fun acc p => if inR R g (fst p) then iadd (snd p) acc else acc) sm s.
Definition add_all (sm : smap) (s : iset) : iset :=
  fold_left (fun acc p => iadd (snd p) acc) sm s.

Fixpoint reads (e : xexpr) (s : iset) : iset :=
  match e with
  | XVar _ | XUnit | XBreak => s
  | XVl v => iadd (IVal v) s
  | XTag _ _ e1 | XBindTy _ _ e1 | XAssign _ e1 | XLoop e1 | XReturn e1 | XField e1 _
  | XBindFunSym _ _ e1 | XBindPrim _ _ e1 => reads e1 s
  | XCallD f _ sm args => fold_left (fun acc a => reads a acc) args (add_srcs f sm s)
  | XCallA f args => fold_left (fun acc a => reads a acc) args (iadd (IFn f) s)
  | XMeth e0 _ it args => fold_left (fun acc a => reads a acc) args (reads e0 (iadd it s))
  | XMatch e0 arms => fold_left (fun acc a => match a with (_, _, b) => reads b acc end) arms (reads e0 s)
  | XLet _ a b | XBindVal _ a b | XSeq a b | XWhile a b => reads b (reads a s)
  | XBindFn _ f' sm e1 | XBindMth _ _ _ f' sm e1 => reads e1 (add_srcs f' sm s)
  | XBindFun _ _ _ smc e1 => reads e1 (add_all smc s)
  | XBindFunA _ u' _ e1 | XBindFunCopy _ u' e1 => reads e1 (iadd (IFun u') s)
  | XApply u _ sm e1 => reads e1 (add_all sm (iadd (IFun u) s))
  | XIf a b c => reads c (reads b (reads a s))
  | XRec fs => fold_left (fun acc p => reads (snd p) acc) fs s
  | XMatchP e0 arms => fold_left (fun acc p => reads (snd p) acc) arms (reads e0 s)
  | XApplyBr u brs e1 =>
      reads e1 (fold_left (fun acc br => match br with (_, bs, pr) =>
                             add_all pr (fold_left (fun a b => breads b a) bs acc) end)
                          brs (iadd (IFun u) s))
  end
with breads (b : xbind) (s : iset) : iset :=
  match b with
  | XBTy _ _ | XBFunSym _ _ | XBPrim _ _ => s
  | XBVal _ e1 => reads e1 s
  | XBFn _ f' sm | XBMth _ _ _ f' sm => add_srcs f' sm s
  | XBFun _ _ _ smc => add_all smc s
  | XBFunA _ u' _ | XBFunCopy _ u' => iadd (IFun u') s
  | XBApply u _ sm => add_all sm (iadd (IFun u) s)
  | XBApplyBr u brs =>
      fold_left (fun acc br => match br with (_, bs, pr) =>
                   add_all pr (fold_left (fun a b => breads b a) bs acc) end)
                brs (iadd (IFun u) s)
  end.
End Compute.

(* One round: each fn's reads under the current sets (for a fn defined
   twice, the first body wins, as Elaboration.nlook). *)
Definition round (fns : list (nat * xexpr)) (R : rsets) : rsets :=
  fold_right (fun p acc => tset acc (fst p) (reads R (snd p) fempty)) TE fns.

Definition check (R : rsets) (p : xprogram) : bool :=
  forallb (fun fb => nchk R (inR R (fst fb)) (snd fb)) (xo_fns p)
  && forallb (fun ub => forallb (bchk R full) (snd (snd ub))) (xo_funs p).

Fixpoint compute_R (fuel : nat) (p : xprogram) (R : rsets) : option rsets :=
  if check R p then Some R
  else match fuel with
       | 0 => None
       | S k => compute_R k p (round (xo_fns p) R)
       end.

Definition prune_program (R : rsets) (p : xprogram) : xprogram :=
  XProgram (map (fun fb => (fst fb, prune_e R (snd fb))) (xo_fns p))
           (map (fun ub => (fst ub, (fst (snd ub), map (prune_b R) (snd (snd ub))))) (xo_funs p))
           (xo_bool p) (xo_true p) (xo_false p) (xo_main p) (xo_env p) (xo_prims p).

(* What the oracle runs. *)
Definition oracle_program (p : xprogram) : xprogram :=
  match compute_R 64 p TE with
  | Some R => prune_program R p
  | None => p
  end.

(* ===================== 3. Agreement ===================================== *)

Section Agree.
Variable R : rsets.

(* Entries agree when equal up to their captured environments; a closure's
   environment needs to agree only on its fn's read set, a functor
   binding's (which is never pruned) entry for entry. *)
Inductive prel : xdentry -> xdentry -> Prop :=
| PVal : forall w, prel (XDVal w) (XDVal w)
| PClo : forall f d d', agree (inR R f) d d' -> prel (XDClo f d) (XDClo f d')
| PFun : forall h sms d d', lrel2 d d' -> prel (XDFun h sms d) (XDFun h sms d')
| PPrim : forall p, prel (XDPrim p) (XDPrim p)
| PFunSym : forall h, prel (XDFunSym h) (XDFunSym h)
(* every lookup of an item in S that succeeds in the reference environment
   succeeds in the pruned one, with an agreeing entry *)
with agree : (item -> bool) -> xdenv -> xdenv -> Prop :=
| Agree : forall S d d',
    (forall i a, S i = true -> lookup_xdenv i d = Some a ->
       exists a', lookup_xdenv i d' = Some a' /\ prel a a') -> agree S d d'
(* pointwise: same keys, agreeing entries *)
with lrel2 : xdenv -> xdenv -> Prop :=
| L2Nil : lrel2 [] []
| L2Cons : forall i a a' d d', prel a a' -> lrel2 d d' -> lrel2 ((i, a) :: d) ((i, a') :: d').

Lemma agree_lookup : forall S d d', agree S d d' -> forall i a, S i = true -> lookup_xdenv i d = Some a ->
  exists a', lookup_xdenv i d' = Some a' /\ prel a a'.
Proof. intros S d d' H. inversion H; subst; eauto. Qed.

Lemma agree_cons : forall S d d' i a a', prel a a' -> agree S d d' -> agree S ((i, a) :: d) ((i, a') :: d').
Proof.
  intros S d d' i a a' Ha H. constructor. intros j b Hj Hl. simpl in *.
  destruct (item_eqb j i); [injection Hl as <-; eauto|].
  eapply agree_lookup; eauto.
Qed.

Lemma lrel2_lookup : forall d d' i, lrel2 d d' ->
  match lookup_xdenv i d, lookup_xdenv i d' with
  | Some a, Some a' => prel a a'
  | None, None => True
  | _, _ => False
  end.
Proof.
  intros d d' i H. induction H; simpl; auto.
  destruct (item_eqb i i0); auto.
Qed.

Lemma agree_app : forall S dr dr' d d', lrel2 dr dr' -> agree S d d' -> agree S (dr ++ d) (dr' ++ d').
Proof.
  intros S dr dr' d d' H Hd. induction H; simpl; auto. apply agree_cons; auto.
Qed.

Lemma lrel2_app : forall a a' b b', lrel2 a a' -> lrel2 b b' -> lrel2 (a ++ b) (a' ++ b').
Proof. intros a a' b b' H Hb. induction H; simpl; auto. constructor; auto. Qed.

Lemma lrel2_agree : forall d d', lrel2 d d' -> agree full d d'.
Proof.
  intros d d' H. constructor. intros i a _ Hl. pose proof (lrel2_lookup i H) as Hx.
  rewrite Hl in Hx. destruct (lookup_xdenv i d'); [eauto|contradiction].
Qed.

Lemma agree_full_compose : forall S d d' sm dc, agree S d d' -> all_ok S sm = true ->
  xcompose d sm = Some dc -> exists dc', xcompose d' sm = Some dc' /\ lrel2 dc dc'.
Proof.
  intros S d d' sm. induction sm as [|[req s] r IH]; simpl; intros dc H Hok Hc.
  - injection Hc as <-. eauto using L2Nil.
  - apply andb_true_iff in Hok as [Hs Hok].
    destruct (lookup_xdenv s d) as [a|] eqn:El; [|discriminate].
    destruct (xcompose d r) as [dr|] eqn:Er; [|discriminate]. injection Hc as <-.
    destruct (agree_lookup H _ Hs El) as [a' [-> Ha]].
    destruct (IH dr H Hok eq_refl) as [dr' [-> Hr]]. eauto using L2Cons.
Qed.

Lemma keep_compose : forall S d d' g sm dc, agree S d d' -> srcs_ok R S g sm = true ->
  xcompose d sm = Some dc ->
  exists dc', xcompose d' (keep R g sm) = Some dc' /\ agree (inR R g) dc dc'.
Proof.
  intros S d d' g sm. induction sm as [|[req s] r IH]; simpl; intros dc H Hok Hc.
  - injection Hc as <-. exists []. split; auto. constructor. intros; discriminate.
  - apply andb_true_iff in Hok as [Hs Hok].
    destruct (lookup_xdenv s d) as [a|] eqn:El; [|discriminate].
    destruct (xcompose d r) as [dr|] eqn:Er; [|discriminate]. injection Hc as <-.
    destruct (IH dr H Hok eq_refl) as [dr' [Hc' Hr]].
    destruct (inR R g req) eqn:Ein; simpl.
    + simpl in Hs. destruct (agree_lookup H _ Hs El) as [a' [Hl' Ha]].
      rewrite Hl', Hc'. eexists; split; [reflexivity|].
      apply agree_cons; auto.
    + rewrite Hc'. eexists; split; [reflexivity|].
      constructor. intros i b Hi Hl. simpl in Hl.
      destruct (item_eqb i req) eqn:Eq.
      * apply item_eqb_eq in Eq; subst. rewrite Ein in Hi. discriminate.
      * eapply agree_lookup; eauto.
Qed.

Lemma lrel2_compose : forall d d' sm dc, lrel2 d d' -> xcompose d sm = Some dc ->
  exists dc', xcompose d' sm = Some dc' /\ lrel2 dc dc'.
Proof.
  intros. eapply agree_full_compose; eauto using lrel2_agree.
  unfold all_ok. apply forallb_forall. reflexivity.
Qed.

Lemma lrel2_chain : forall sms d d' d2, lrel2 d d' -> xrun_chain d sms = Some d2 ->
  exists d2', xrun_chain d' sms = Some d2' /\ lrel2 d2 d2'.
Proof.
  induction sms as [|sm r IH]; simpl; intros d d' d2 H Hc.
  - injection Hc as <-. eauto.
  - destruct (xrun_chain d r) as [dm|] eqn:E; [|discriminate].
    destruct (IH d d' dm H E) as [dm' [-> Hm]].
    eapply lrel2_compose; eauto.
Qed.

(* Reference entries agree with themselves. *)
Lemma prel_refl : forall a, prel a a.
Proof.
  fix IH 1. intros [w|f d|h sms d|p|h].
  - constructor.
  - constructor. constructor. intros i a _ Hl. exists a. split; [exact Hl|].
    revert a Hl. induction d as [|[j e] r IHd]; intros a Hl; simpl in Hl; [discriminate|].
    destruct (item_eqb i j); [injection Hl as <-; apply IH|exact (IHd a Hl)].
  - constructor. induction d as [|[j e] r IHd]; constructor; [apply IH|exact IHd].
  - constructor.
  - constructor.
Qed.

Lemma agree_refl : forall S d, agree S d d.
Proof. intros S d. constructor. intros i a _ Hl. exists a. split; auto using prel_refl. Qed.

End Agree.

Arguments agree_lookup {R S d d'} _ i {a}.
Arguments agree_cons {R S d d' i a a'}.
Arguments agree_app {R S dr dr' d d'}.
Arguments lrel2_app {R a a' b b'}.
Arguments lrel2_agree {R d d'}.
Arguments agree_full_compose {R S d d' sm dc}.
Arguments keep_compose {R S d d' g sm dc}.
Arguments lrel2_chain {R} sms {d d' d2}.

(* ===================== 4. Simulation ===================================== *)

Section Sim.
Variable R : rsets.
Variables P P' : xprog.
Hypothesis Hfn : forall f, xp_fn P' f = option_map (prune_e R) (xp_fn P f).
Hypothesis Hfun : forall u,
  xp_fun P' u = option_map (fun p => (fst p, map (prune_b R) (snd p))) (xp_fun P u).
Hypothesis Hprim : forall p ws st, run_prim P' p ws st = run_prim P p ws st.
Hypothesis Htrue : xp_true P' = xp_true P.
Hypothesis Hclosed : forall f b, xp_fn P f = Some b -> nchk R (inR R f) b = true.
Hypothesis Hfunchk : forall u Dr bs, xp_fun P u = Some (Dr, bs) -> forallb (bchk R full) bs = true.

Definition stuck (r : xres) : Prop := exists c, xr_out r = XErr c.
Definition stuckl {A} (r : A + xres) : Prop := match r with inr r => stuck r | inl _ => False end.

Definition brel2 (S : item -> bool) (a : (xdenv * xvenv * mstate) + xres)
    (b : (xdenv * xvenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (d', g', st'), inl (d, g, st) => agree R S d d' /\ g' = g /\ st' = st
  | inr r', inr r => r' = r
  | _, _ => False
  end.

Definition arel2 (a : (xdenv * mstate) + xres) (b : (xdenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (d', st'), inl (d, st) => lrel2 R d d' /\ st' = st
  | inr r', inr r => r' = r
  | _, _ => False
  end.

Definition rrel2 (a : (xdenv * xvenv * mstate) + xres) (b : (xdenv * xvenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (d', g', st'), inl (d, g, st) => lrel2 R d d' /\ g' = g /\ st' = st
  | inr r', inr r => r' = r
  | _, _ => False
  end.

Definition brchk (S : item -> bool) (brs : list (usym * list xbind * smap)) : bool :=
  forallb (fun br => match br with (_, bs, pr) => forallb (bchk R S) bs && all_ok S pr end) brs.

Definition SIM (k : nat) : Prop :=
  (forall S d d' g st e, nchk R S e = true -> agree R S d d' ->
     stuck (xinterp k P d g st e) \/ xinterp k P' d' g st (prune_e R e) = xinterp k P d g st e)
  /\ (forall S d d' g st l, forallb (nchk R S) l = true -> agree R S d d' ->
     stuckl (xinterp_list k P d g st l)
     \/ xinterp_list k P' d' g st (map (prune_e R) l) = xinterp_list k P d g st l)
  /\ (forall S d d' g st bs, forallb (bchk R S) bs = true -> agree R S d d' ->
     stuckl (xinterp_binds k P d g st bs)
     \/ brel2 S (xinterp_binds k P' d' g st (map (prune_b R) bs)) (xinterp_binds k P d g st bs))
  /\ (forall S d d' u th sm st, S (IFun u) = true -> all_ok S sm = true -> agree R S d d' ->
     stuckl (xinterp_apply k P d u th sm st)
     \/ arel2 (xinterp_apply k P' d' u th sm st) (xinterp_apply k P d u th sm st))
  /\ (forall S d d' g st u brs, S (IFun u) = true -> brchk S brs = true -> agree R S d d' ->
     stuckl (xinterp_branch k P d g st u brs)
     \/ rrel2 (xinterp_branch k P' d' g st u (map (fun br => match br with (h, bs, pr) =>
                                                (h, map (prune_b R) bs, pr) end) brs))
              (xinterp_branch k P d g st u brs)).

Lemma find_xarm_prune : forall n arms,
  find_xarm n (map (fun a => match a with (n, x, b) => (n, x, prune_e R b) end) arms)
  = option_map (fun p => (fst p, prune_e R (snd p))) (find_xarm n arms).
Proof.
  induction arms as [|[[n' x] b] r IH]; simpl; auto. destruct (Nat.eqb n n'); auto.
Qed.

Lemma find_xarm_chk : forall S n arms x b,
  forallb (fun a => match a with (_, _, b) => nchk R S b end) arms = true ->
  find_xarm n arms = Some (x, b) -> nchk R S b = true.
Proof.
  induction arms as [|[[n' x'] b'] r IH]; simpl; intros x b Hc Hf; [discriminate|].
  apply andb_true_iff in Hc as [H1 H2].
  destruct (Nat.eqb n n'); [injection Hf as <- <-; auto|eauto].
Qed.

Arguments find_xarm_chk {S n arms x b}.

Lemma select_arm_prune : forall arms w,
  select_arm (map (fun p => (fst p, prune_e R (snd p))) arms) w
  = option_map (fun p => (fst p, prune_e R (snd p))) (select_arm arms w).
Proof.
  induction arms as [|[p b] r IH]; intros w; simpl; auto.
  destruct (match_pat p w); simpl; auto.
Qed.

Lemma select_arm_chk : forall S arms w bs b,
  forallb (fun p => nchk R S (snd p)) arms = true -> select_arm arms w = Some (bs, b) ->
  nchk R S b = true.
Proof.
  induction arms as [|[p b'] r IH]; simpl; intros w bs b Hc Hs; [discriminate|].
  apply andb_true_iff in Hc as [H1 H2].
  destruct (match_pat p w); [injection Hs as <- <-; auto|eauto|discriminate].
Qed.

Arguments select_arm_chk {S arms w bs b}.

Lemma find_branch_prune : forall h brs,
  find_branch h (map (fun br => match br with (h, bs, pr) => (h, map (prune_b R) bs, pr) end) brs)
  = option_map (fun p => (map (prune_b R) (fst p), snd p)) (find_branch h brs).
Proof.
  induction brs as [|[[h' bs] pr] r IH]; simpl; auto. destruct (Nat.eqb h h'); auto.
Qed.

Lemma find_branch_chk : forall S h brs bs pr,
  brchk S brs = true -> find_branch h brs = Some (bs, pr) ->
  forallb (bchk R S) bs = true /\ all_ok S pr = true.
Proof.
  unfold brchk. induction brs as [|[[h' bs'] pr'] r IH]; simpl; intros bs pr Hc Hf; [discriminate|].
  apply andb_true_iff in Hc as [H1 H2]. apply andb_true_iff in H1.
  destruct (Nat.eqb h h'); [injection Hf as <- <-; auto|eauto].
Qed.

Arguments find_branch_chk {S h brs bs pr}.

Lemma map_snd_prune : forall (fs : list (fld * xexpr)),
  map snd (map (fun p => (fst p, prune_e R (snd p))) fs) = map (prune_e R) (map snd fs).
Proof. intros. rewrite !map_map. reflexivity. Qed.

Lemma map_fst_prune : forall (fs : list (fld * xexpr)),
  map fst (map (fun p => (fst p, prune_e R (snd p))) fs) = map fst fs.
Proof. intros. rewrite map_map. reflexivity. Qed.

Lemma forallb_snd : forall S (fs : list (fld * xexpr)),
  forallb (fun p => nchk R S (snd p)) fs = forallb (nchk R S) (map snd fs).
Proof. induction fs; simpl; auto. rewrite IHfs. auto. Qed.

Lemma truthy_eq : forall w, truthy P' w = truthy P w.
Proof. intros. unfold truthy. rewrite Htrue. reflexivity. Qed.

(* If a sub-evaluation of the reference gets stuck, so does the whole. *)
Ltac stk :=
  match goal with
  | Hs : stuck ?r |- _ =>
      let c := fresh "c" in let Hx := fresh "Hx" in
      let o := fresh "o" in let gg := fresh "gg" in let ss := fresh "ss" in
      left; destruct Hs as [c Hx]; revert Hx; destruct r as [o gg ss]; simpl; intros ->;
      simpl; eexists; reflexivity
  | Hs : stuckl ?r |- _ =>
      let c := fresh "c" in
      let o := fresh "o" in let gg := fresh "gg" in let ss := fresh "ss" in
      let Hx := fresh "Hx" in
      left; revert Hs; destruct r as [?|[o gg ss]]; simpl; [intros []|];
      intros [c Hx]; simpl in Hx; subst o; simpl; eexists; reflexivity
  end.

Ltac andb_split :=
  repeat match goal with H : (_ && _) = true |- _ => apply andb_true_iff in H as [? ?] end.

Ltac err := right; reflexivity.

Lemma sim_step : forall k, SIM k -> SIM (S k).
Proof.
  intros k [IHe [IHl [IHb [IHa IHr]]]].
  split; [|split; [|split; [|split]]].
  - intros S d d' g st e Hc H. destruct e; simpl in Hc |- *; andb_split.
    + (* XVar *) err.
    + err.
    + (* XVl *)
      destruct (lookup_xdenv (IVal v) d) as [a|] eqn:El; [|left; eexists; reflexivity].
      destruct (agree_lookup H _ Hc El) as [a' [-> Ha]].
      inversion Ha; subst; try (left; eexists; reflexivity). err.
    + (* XTag *)
      destruct (IHe S d d' g st e Hc H) as [Hs| ->]; [stk|].
      destruct (xinterp k P d g st e) as [[]]; err.
    + (* XCallD *)
      rewrite Hfn. destruct (xp_fn P f) as [body|] eqn:Ef; simpl; [|err].
      destruct (IHl S d d' g st l ltac:(eassumption) H) as [Hs| ->]; [stk|].
      destruct (xinterp_list k P d g st l) as [[[ws g1] st1]|]; [|err].
      destruct (xcompose d s0) as [dc|] eqn:Ec; [|left; eexists; reflexivity].
      destruct (keep_compose H ltac:(eassumption) Ec) as [dc' [-> Hdc]].
      destruct (IHe _ dc dc' (mk_xvenv ws) st1 body (Hclosed f Ef) Hdc) as [Hs| ->];
        [|err].
      left. destruct (xinterp k P dc (mk_xvenv ws) st1 body) as [o0 g0 sx0].
      destruct Hs as [c Hc']. simpl in Hc'. subst. simpl. eexists; reflexivity.
    + (* XCallA *)
      destruct (lookup_xdenv (IFn f) d) as [a|] eqn:El; [|left; eexists; reflexivity].
      destruct (agree_lookup H _ ltac:(eassumption) El) as [a' [-> Ha]].
      inversion Ha; subst; try (left; eexists; reflexivity).
      * rewrite Hfn. destruct (xp_fn P f0) as [body|] eqn:Ef; simpl; [|err].
        destruct (IHl S d d' g st l ltac:(eassumption) H) as [Hs| ->]; [stk|].
        destruct (xinterp_list k P d g st l) as [[[ws g1] st1]|]; [|err].
        destruct (IHe _ d0 d'0 (mk_xvenv ws) st1 body (Hclosed f0 Ef) ltac:(eassumption)) as [Hs| ->]; [|err].
        left. destruct (xinterp k P d0 (mk_xvenv ws) st1 body) as [o0 g0 sx0].
        destruct Hs as [c Hc']. simpl in Hc'. subst. simpl. eexists; reflexivity.
      * destruct (IHl S d d' g st l ltac:(eassumption) H) as [Hs| ->]; [stk|].
        destruct (xinterp_list k P d g st l) as [[[ws g1] st1]|]; [|err].
        rewrite Hprim. err.
    + (* XMeth *)
      destruct (lookup_xdenv i d) as [a|] eqn:El; [|left; eexists; reflexivity].
      destruct (agree_lookup H _ ltac:(eassumption) El) as [a' [-> Ha]].
      inversion Ha; subst; try (left; eexists; reflexivity).
      * rewrite Hfn. destruct (xp_fn P f) as [body|] eqn:Ef; simpl; [|err].
        destruct (IHe S d d' g st e ltac:(eassumption) H) as [Hs| ->]; [stk|].
        destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; try err.
        destruct (IHl S d d' g0 st0 l ltac:(eassumption) H) as [Hs| ->]; [stk|].
        destruct (xinterp_list k P d g0 st0 l) as [[[ws g1] st1]|]; [|err].
        destruct (IHe _ d0 d'0 (mk_xvenv (w0 :: ws)) st1 body (Hclosed f Ef) ltac:(eassumption)) as [Hs| ->]; [|err].
        left. destruct (xinterp k P d0 (mk_xvenv (w0 :: ws)) st1 body) as [o0 g2 s0].
        destruct Hs as [c Hc']. simpl in Hc'. subst. simpl. eexists; reflexivity.
      * destruct (IHe S d d' g st e ltac:(eassumption) H) as [Hs| ->]; [stk|].
        destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; try err.
        destruct (IHl S d d' g0 st0 l ltac:(eassumption) H) as [Hs| ->]; [stk|].
        destruct (xinterp_list k P d g0 st0 l) as [[[ws g1] st1]|]; [|err].
        rewrite Hprim. err.
    + (* XMatch *)
      destruct (IHe S d d' g st e ltac:(eassumption) H) as [Hs| ->]; [stk|].
      destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; try err.
      destruct w0; try err.
      rewrite find_xarm_prune. destruct (find_xarm n l) as [[x eb]|] eqn:Ea; simpl; [|err].
      pose proof (find_xarm_chk ltac:(eassumption) Ea) as Hb.
      destruct (IHe S d d' ((x, w0) :: g0) st0 eb Hb H) as [Hs| ->]; [stk|err].
    + (* XLet *)
      destruct (IHe S d d' g st e1 ltac:(eassumption) H) as [Hs| ->]; [stk|].
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; try err.
      destruct (IHe S d d' ((v, w0) :: g0) st0 e2 ltac:(eassumption) H) as [Hs| ->]; [stk|err].
    + (* XBindTy *) apply (IHe S d d' g st e Hc H).
    + (* XBindVal *)
      destruct (IHe S d d' g st e1 ltac:(eassumption) H) as [Hs| ->]; [stk|].
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; try err.
      apply (IHe S); auto. apply agree_cons; auto. constructor.
    + (* XBindFn *)
      destruct (xcompose d s) as [dc|] eqn:Ec; [|left; eexists; reflexivity].
      destruct (keep_compose H ltac:(eassumption) Ec) as [dc' [-> Hdc]].
      apply (IHe S); auto. apply agree_cons; auto. constructor; auto.
    + (* XBindMth *)
      destruct (xcompose d s0) as [dc|] eqn:Ec; [|left; eexists; reflexivity].
      destruct (keep_compose H ltac:(eassumption) Ec) as [dc' [-> Hdc]].
      apply (IHe S); auto. apply agree_cons; auto. constructor; auto.
    + (* XBindFun *)
      destruct (xcompose d s0) as [dc|] eqn:Ec; [|left; eexists; reflexivity].
      destruct (agree_full_compose H ltac:(eassumption) Ec) as [dc' [-> Hdc]].
      apply (IHe S); auto. apply agree_cons; auto. constructor; auto.
    + (* XBindFunA *)
      destruct (lookup_xdenv (IFun u0) d) as [a|] eqn:El; [|left; eexists; reflexivity].
      destruct (agree_lookup H _ ltac:(eassumption) El) as [a' [-> Ha]].
      inversion Ha; subst; try (left; eexists; reflexivity).
      apply (IHe S); auto. apply agree_cons; auto. constructor; auto.
    + (* XApply *)
      destruct (IHa S d d' u s s0 st ltac:(eassumption) ltac:(eassumption) H) as [Hs|Ha]; [stk|].
      destruct (xinterp_apply k P' d' u s s0 st) as [[dr' st1']|r1'],
               (xinterp_apply k P d u s s0 st) as [[dr st1]|r1]; simpl in Ha;
        try contradiction.
      * destruct Ha as [Hdr ->]. apply (IHe S); auto. apply agree_app; auto.
      * subst. err.
    + (* XSeq *)
      destruct (IHe S d d' g st e1 ltac:(eassumption) H) as [Hs| ->]; [stk|].
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; try err.
      apply (IHe S); auto.
    + (* XAssign *)
      destruct (IHe S d d' g st e Hc H) as [Hs| ->]; [stk|err].
    + (* XIf *)
      destruct (IHe S d d' g st e1 ltac:(eassumption) H) as [Hs| ->]; [stk|].
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; try err.
      rewrite truthy_eq. destruct (truthy P w0) as [[]|]; try err; apply (IHe S); auto.
    + (* XWhile *)
      destruct (IHe S d d' g st e1 ltac:(eassumption) H) as [Hs| ->]; [stk|].
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; try err.
      rewrite truthy_eq. destruct (truthy P w0) as [[]|]; try err.
      destruct (IHe S d d' g0 st0 e2 ltac:(eassumption) H) as [Hs| ->]; [stk|].
      destruct (xinterp k P d g0 st0 e2) as [[w1| | | | | | |] g1 st1]; try err.
      change (XWhile (prune_e R e1) (prune_e R e2)) with (prune_e R (XWhile e1 e2)).
      apply (IHe S); simpl; auto. apply andb_true_iff; split; assumption.
    + (* XLoop *)
      destruct (IHe S d d' g st e Hc H) as [Hs| ->]; [stk|].
      destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; try err.
      change (XLoop (prune_e R e)) with (prune_e R (XLoop e)).
      apply (IHe S); simpl; auto.
    + (* XReturn *)
      destruct (IHe S d d' g st e Hc H) as [Hs| ->]; [stk|err].
    + (* XBreak *) err.
    + (* XRec *)
      rewrite map_snd_prune, map_fst_prune. rewrite forallb_snd in Hc.
      destruct (IHl S d d' g st (map snd l) Hc H) as [Hs| ->]; [stk|err].
    + (* XField *)
      destruct (IHe S d d' g st e Hc H) as [Hs| ->]; [stk|err].
    + (* XMatchP *)
      destruct (IHe S d d' g st e ltac:(eassumption) H) as [Hs| ->]; [stk|].
      destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; try err.
      rewrite select_arm_prune. destruct (select_arm l w0) as [[bs eb]|] eqn:Ea; simpl; [|err].
      pose proof (select_arm_chk ltac:(eassumption) Ea) as Hb.
      destruct (IHe S d d' (bs ++ g0) st0 eb Hb H) as [Hs| ->]; [stk|err].
    + (* XBindFunSym *)
      apply (IHe S); auto. apply agree_cons; auto. constructor.
    + (* XBindFunCopy *)
      destruct (lookup_xdenv (IFun u0) d) as [a|] eqn:El; [|left; eexists; reflexivity].
      destruct (agree_lookup H _ ltac:(eassumption) El) as [a' [-> Ha]].
      apply (IHe S); auto. apply agree_cons; auto.
    + (* XApplyBr *)
      destruct (IHr S d d' g st u l ltac:(eassumption) ltac:(eassumption) H) as [Hs|Hr]; [stk|].
      destruct (xinterp_branch k P' d' g st u _) as [[[dr' g1'] st1']|r1'],
               (xinterp_branch k P d g st u l) as [[[dr g1] st1]|r1]; simpl in Hr;
        try contradiction.
      * destruct Hr as [Hdr [-> ->]]. apply (IHe S); auto. apply agree_app; auto.
      * subst. err.
    + (* XBindPrim *)
      apply (IHe S); auto. apply agree_cons; auto. constructor.
  - intros S d d' g st l Hc H. destruct l as [|e r]; simpl in Hc |- *; [err|]. andb_split.
    destruct (IHe S d d' g st e ltac:(eassumption) H) as [Hs| ->]; [stk|].
    destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; try err.
    destruct (IHl S d d' g0 st0 r ltac:(eassumption) H) as [Hs| ->]; [|err].
    left. revert Hs. destruct (xinterp_list k P d g0 st0 r) as [?|[o0 g1 sx2]]; simpl;
      [intros []|]. intros Hx; exact Hx.
  - intros S d d' g st bs Hc H. destruct bs as [|b r]; simpl in Hc |- *; [right; simpl; auto|].
    andb_split. destruct b;
      match goal with Hb : bchk _ _ _ = true |- _ => simpl in Hb |- * end; andb_split.
    + apply (IHb S); auto.
    + destruct (IHe S d d' g st x ltac:(eassumption) H) as [Hs| ->].
      * left. destruct (xinterp k P d g st x) as [o0 g0 sx0]. destruct Hs as [c Hc'].
        simpl in Hc'. subst. simpl. eexists; reflexivity.
      * destruct (xinterp k P d g st x) as [[w0| | | | | | |] g0 st0]; try (right; reflexivity).
        apply (IHb S); auto. apply agree_cons; auto. constructor.
    + destruct (xcompose d s) as [dc|] eqn:Ec; [|left; eexists; reflexivity].
      destruct (keep_compose H ltac:(eassumption) Ec) as [dc' [-> Hdc]].
      apply (IHb S); auto. apply agree_cons; auto. constructor; auto.
    + destruct (xcompose d s0) as [dc|] eqn:Ec; [|left; eexists; reflexivity].
      destruct (keep_compose H ltac:(eassumption) Ec) as [dc' [-> Hdc]].
      apply (IHb S); auto. apply agree_cons; auto. constructor; auto.
    + destruct (xcompose d s0) as [dc|] eqn:Ec; [|left; eexists; reflexivity].
      destruct (agree_full_compose H ltac:(eassumption) Ec) as [dc' [-> Hdc]].
      apply (IHb S); auto. apply agree_cons; auto. constructor; auto.
    + destruct (lookup_xdenv (IFun u0) d) as [a|] eqn:El; [|left; eexists; reflexivity].
      destruct (agree_lookup H _ ltac:(eassumption) El) as [a' [-> Ha]].
      inversion Ha; subst; try (left; eexists; reflexivity).
      apply (IHb S); auto. apply agree_cons; auto. constructor; auto.
    + destruct (IHa S d d' u s s0 st ltac:(eassumption) ltac:(eassumption) H) as [Hs|Ha].
      * left. destruct (xinterp_apply k P d u s s0 st) as [?|[o0 g0 sx1]]; simpl in Hs;
          [contradiction|]. destruct Hs as [c Hc']. simpl in Hc'. subst. simpl.
        eexists; reflexivity.
      * destruct (xinterp_apply k P' d' u s s0 st) as [[dr' st1']|r1'],
                 (xinterp_apply k P d u s s0 st) as [[dr st1]|r1]; simpl in Ha;
          try contradiction.
        -- destruct Ha as [Hdr ->]. apply (IHb S); auto. apply agree_app; auto.
        -- subst. right. reflexivity.
    + apply (IHb S); auto. apply agree_cons; auto. constructor.
    + destruct (lookup_xdenv (IFun u0) d) as [a|] eqn:El; [|left; eexists; reflexivity].
      destruct (agree_lookup H _ ltac:(eassumption) El) as [a' [-> Ha]].
      apply (IHb S); auto. apply agree_cons; auto.
    + destruct (IHr S d d' g st u l ltac:(eassumption) ltac:(eassumption) H) as [Hs|Hr].
      * left. destruct (xinterp_branch k P d g st u l) as [?|[o0 g0 sx1]]; simpl in Hs;
          [contradiction|]. destruct Hs as [c Hc']. simpl in Hc'. subst. simpl.
        eexists; reflexivity.
      * destruct (xinterp_branch k P' d' g st u _) as [[[dr' g1'] st1']|r1'],
                 (xinterp_branch k P d g st u l) as [[[dr g1] st1]|r1]; simpl in Hr;
          try contradiction.
        -- destruct Hr as [Hdr [-> ->]]. apply (IHb S); auto. apply agree_app; auto.
        -- subst. right. reflexivity.
    + apply (IHb S); auto. apply agree_cons; auto. constructor.
  - intros S d d' u th sm st Hu Hsm H. simpl.
    destruct (lookup_xdenv (IFun u) d) as [a|] eqn:El; [|left; eexists; reflexivity].
    destruct (agree_lookup H _ Hu El) as [a' [-> Ha]].
    inversion Ha; subst; try (left; eexists; reflexivity).
    rewrite Hfun. destruct (xp_fun P h) as [[Dr bs]|] eqn:Ef; simpl; [|right; reflexivity].
    destruct (xcompose d sm) as [d1|] eqn:Ec; [|left; eexists; reflexivity].
    destruct (agree_full_compose H Hsm Ec) as [d1' [-> Hd1]].
    destruct (xrun_chain d1 sms) as [d2|] eqn:Ech; [|left; eexists; reflexivity].
    destruct (lrel2_chain sms Hd1 Ech) as [d2' [-> Hd2]].
    assert (Hfull : agree R full (d2 ++ d0) (d2' ++ d'0)) by (apply lrel2_agree, lrel2_app; assumption).
    destruct (IHb full (d2 ++ d0) (d2' ++ d'0) [] st bs (Hfunchk h Ef) Hfull) as [Hs|Hb].
    + left. destruct (xinterp_binds k P (d2 ++ d0) [] st bs) as [?|[o0 g0 sx1]]; simpl in Hs;
        [contradiction|]. destruct Hs as [c Hc']. simpl in Hc'. subst. simpl. eexists; reflexivity.
    + destruct (xinterp_binds k P' (d2' ++ d'0) [] st (map (prune_b R) bs)) as [[[d3' g3'] st3']|r3'],
               (xinterp_binds k P (d2 ++ d0) [] st bs) as [[[d3 g3] st3]|r3]; simpl in Hb;
        try contradiction.
      * destruct Hb as [Hd3 [-> ->]].
        destruct (xcompose d3 (inst_smap th Dr)) as [dr|] eqn:Ec3; [|left; eexists; reflexivity].
        assert (Hok : all_ok full (inst_smap th Dr) = true)
          by (unfold all_ok; apply forallb_forall; reflexivity).
        destruct (agree_full_compose Hd3 Hok Ec3) as [dr' [-> Hdr]].
        right. simpl. auto.
      * subst. right. simpl. auto.
  - intros S d d' g st u brs Hu Hb H. simpl.
    destruct (lookup_xdenv (IFun u) d) as [a|] eqn:El; [|left; eexists; reflexivity].
    destruct (agree_lookup H _ Hu El) as [a' [-> Ha]].
    inversion Ha; subst; try (left; eexists; reflexivity).
    rewrite find_branch_prune. destruct (find_branch h brs) as [[bs pr]|] eqn:Ef; simpl;
      [|right; reflexivity].
    destruct (find_branch_chk Hb Ef) as [Hbs Hpr].
    destruct (IHb S d d' g st bs Hbs H) as [Hs|Hb'].
    + left. destruct (xinterp_binds k P d g st bs) as [?|[o0 g0 sx1]]; simpl in Hs;
        [contradiction|]. destruct Hs as [c Hc']. simpl in Hc'. subst. simpl. eexists; reflexivity.
    + destruct (xinterp_binds k P' d' g st (map (prune_b R) bs)) as [[[d3' g3'] st3']|r3'],
               (xinterp_binds k P d g st bs) as [[[d3 g3] st3]|r3]; simpl in Hb';
        try contradiction.
      * destruct Hb' as [Hd3 [-> ->]].
        destruct (xcompose d3 pr) as [dr|] eqn:Ec3; [|left; eexists; reflexivity].
        destruct (agree_full_compose Hd3 Hpr Ec3) as [dr' [-> Hdr]].
        right. simpl. auto.
      * subst. right. simpl. auto.
Qed.

Lemma sim_all : forall k, SIM k.
Proof.
  induction k as [|k IH].
  - repeat split; intros; right; simpl; auto.
  - apply sim_step, IH.
Qed.

End Sim.

(* ===================== 5. The pruned program ============================= *)

Lemma nlook_map : forall A B (f : A -> B) k (l : list (nat * A)),
  Elaboration.nlook k (map (fun p => (fst p, f (snd p))) l) = option_map f (Elaboration.nlook k l).
Proof.
  induction l as [|[k' a] r IH]; simpl; auto. destruct (Nat.eqb k k'); auto.
Qed.

Lemma nlook_in : forall A k (l : list (nat * A)) a, Elaboration.nlook k l = Some a -> In (k, a) l.
Proof.
  induction l as [|[k' a'] r IH]; simpl; intros a H; [discriminate|].
  destruct (Nat.eqb k k') eqn:E; [apply Nat.eqb_eq in E; injection H as ->; subst; auto|].
  right. auto.
Qed.

(* The concrete primitives read the program only through argv, the file
   system and Bool's symbols. *)
Lemma run_prim_code : forall fns funs fns' funs' b t f argv rd p ws st,
  run_prim (mkXProg fns' funs' b t f argv rd) p ws st = run_prim (mkXProg fns funs b t f argv rd) p ws st.
Proof. reflexivity. Qed.

Theorem run_main_pruned : forall R p argv rd fuel st0,
  check R p = true ->
  let P := program_of p argv rd in
  let P' := program_of (prune_program R p) argv rd in
  stuck (run_main fuel P (xo_main p) (xo_env p) st0)
  \/ run_main fuel P' (xo_main p) (xo_env p) st0 = run_main fuel P (xo_main p) (xo_env p) st0.
Proof.
  intros R p argv rd fuel st0 Hchk P P'.
  unfold check in Hchk. apply andb_true_iff in Hchk as [Hc1 Hc2].
  assert (Hfn : forall f, xp_fn P' f = option_map (prune_e R) (xp_fn P f)).
  { intro f. unfold P, P', program_of, prune_program. simpl. apply nlook_map. }
  assert (Hfun : forall u, xp_fun P' u
            = option_map (fun p => (fst p, map (prune_b R) (snd p))) (xp_fun P u)).
  { intro u. unfold P, P', program_of, prune_program. simpl.
    rewrite (nlook_map (fun q : tele * list xbind => (fst q, map (prune_b R) (snd q)))).
    reflexivity. }
  assert (Hprim : forall q ws st, run_prim P' q ws st = run_prim P q ws st)
    by (intros; apply run_prim_code).
  assert (Htrue : xp_true P' = xp_true P) by reflexivity.
  assert (Hclosed : forall f b, xp_fn P f = Some b -> nchk R (inR R f) b = true).
  { intros f b Hf. apply nlook_in in Hf. rewrite forallb_forall in Hc1.
    exact (Hc1 (f, b) Hf). }
  assert (Hfunchk : forall u Dr bs, xp_fun P u = Some (Dr, bs) -> forallb (bchk R full) bs = true).
  { intros u Dr bs Hf. apply nlook_in in Hf. rewrite forallb_forall in Hc2.
    exact (Hc2 (u, (Dr, bs)) Hf). }
  destruct (@sim_all R P P' Hfn Hfun Hprim Htrue Hclosed Hfunchk fuel) as [Hsim _].
  unfold run_main. rewrite Hfn. destruct (xp_fn P (xo_main p)) as [body|] eqn:Em; simpl;
    [|right; reflexivity].
  destruct (Hsim _ (xo_env p) (xo_env p) [] st0 body (Hclosed _ _ Em)
              (agree_refl R _ (xo_env p))) as [Hs| ->]; [|right; reflexivity].
  left. destruct (xinterp fuel P (xo_env p) [] st0 body) as [o g s]. destruct Hs as [c Hc].
  simpl in Hc. subst. simpl. eexists; reflexivity.
Qed.

(* ===================== 6. The oracle ===================================== *)

Lemma compute_R_check : forall n p R0 R, compute_R n p R0 = Some R -> check R p = true.
Proof.
  induction n as [|n IH]; intros p R0 R H; simpl in H;
    destruct (check R0 p) eqn:E; try (injection H as <-; exact E); try discriminate; eauto.
Qed.

(* What the extracted oracle computes (FastX's interpreter on the pruned
   program) is Audit's semantics of the elaborated program — unless the
   reference run gets stuck. *)
Theorem oracle_agrees : forall p argv rd fuel st0,
  let P := program_of p argv rd in
  let P' := program_of (oracle_program p) argv rd in
  stuck (run_main fuel P (xo_main p) (xo_env p) st0)
  \/ frun_main fuel P' (xo_main p) (xo_env p) st0 = run_main fuel P (xo_main p) (xo_env p) st0.
Proof.
  intros p argv rd fuel st0 P P'. unfold P'. rewrite frun_main_eq.
  unfold oracle_program. destruct (compute_R 64 p TE) as [R|] eqn:E.
  - apply run_main_pruned. eapply compute_R_check; eauto.
  - right. reflexivity.
Qed.
