(* ------------------------------------------------------------------------ *)
(* ProgramPrune.v — read-set pruning of satisfaction maps, for Audit's        *)
(* programs (Prune.v's pass, ported from ExecutionModel's XIR).              *)
(*                                                                           *)
(* The elaborator passes each callee its whole context: a call's            *)
(* satisfaction map has hundreds of entries, and Audit's interpreter        *)
(* selects every one of them (select_provisions, a linear lookup each) at   *)
(* every call.  A callee reads few of them.  Given a read set R(f) for      *)
(* every fn f, `prune_e` keeps, at each call of f (ECall) and each closure  *)
(* of f made by a binding (BFunction, BMethod, also inside functor bodies), *)
(* only the pairs whose requirement is in R(f).  Main's initial environment *)
(* is cut to R(main) the same way.  Functor bindings, copies and            *)
(* applications are untouched: a functor body reads its whole environment  *)
(* (fc_sources/result_selection read the body's final environment under    *)
(* the caller's keys).                                                       *)
(*                                                                           *)
(* The read sets are computed by an untrusted iteration (`compute_R`); what *)
(* is trusted is the checker `check R p`: every fn body reads only items in *)
(* its own read set, where a call of g reads the sources of the pairs kept *)
(* for g.  If the check fails, `prune` returns the program unchanged.       *)
(*                                                                           *)
(* Proved:                                                                   *)
(* - Sim.run_main_pruned: for every fuel, running the pruned program gives  *)
(*   exactly the reference result, unless the reference run is Stuck (a    *)
(*   dropped pair whose source was missing is no longer looked up).         *)
(* - PruneRuns.observe_pruned / runs_pruned: for Public's Runs, if the      *)
(*   program never runs to Wrong (public_safety gives this for admissible, *)
(*   compatible programs), the pruned program's observation at every fuel   *)
(*   is the original's; so Runs agree.  The interface's primitives must not *)
(*   read the program's code (`code_blind`): Audit passes the program view *)
(*   to primitives, and the view's fn bodies are what pruning changes.     *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.
From NM Require Import BaseFacts.
From NM Require Import AuditFacts.
From NM Require Import Audit PublicTypeFacts PublicValueTyping.
From NM Require NatTrie.
Import Audit.Semantics ListNotations.

Set Implicit Arguments.
Local Open Scope nat_scope.

(* ===================== 0. Item equality ================================== *)

Lemma subst_eqb_true : forall a b, subst_eqb a b = true <-> a = b.
Proof.
  induction a as [|[t tau] r IH]; intros [|[t' tau'] r']; cbn; try (split; congruence).
  rewrite !andb_true_iff, Nat.eqb_eq, ty_eqb_spec, IH.
  split; [intros [[-> ->] ->]; reflexivity|intros E; inversion E; auto].
Qed.

Lemma item_eqb_true : forall a b, item_eqb a b = true <-> a = b.
Proof.
  intros [t|v|f|r m th|u] [t'|v'|f'|r' m' th'|u']; cbn; try (split; congruence);
    try (rewrite Nat.eqb_eq; split; congruence).
  rewrite !andb_true_iff, ty_eqb_spec, Nat.eqb_eq, subst_eqb_true.
  split; [intros [[-> ->] ->]; reflexivity|intros E; inversion E; auto].
Qed.

(* ===================== 1. Read sets ===================================== *)

(* Sets of items: buckets of a nat trie, keyed by kind and symbol. Only the
   untrusted computation and the checker read them; soundness holds for any
   membership predicate. *)
Definition item_slot (i : item) : nat :=
  match i with
  | ITy t => 5 * t
  | IVal v => 5 * v + 1
  | IFn f => 5 * f + 2
  | IMth _ m _ => 5 * m + 3
  | IFun u => 5 * u + 4
  end.

Definition iset := NatTrie.tmap (list item).
Definition ibucket (i : item) (s : iset) : list item :=
  match NatTrie.tget s (item_slot i) with Some l => l | None => [] end.
Definition imem (i : item) (s : iset) : bool := existsb (item_eqb i) (ibucket i s).
Definition iadd (i : item) (s : iset) : iset :=
  if imem i s then s else NatTrie.tset s (item_slot i) (i :: ibucket i s).

Definition rsets := NatTrie.tmap iset.
Definition inR (R : rsets) (f : fsym) (i : item) : bool :=
  match i with
  | ITy _ => true
  | _ => match NatTrie.tget R f with Some s => imem i s | None => false end
  end.

Definition keep (R : rsets) (g : fsym) (sm : smap) : smap :=
  filter (fun p => inR R g (fst p)) sm.

Fixpoint prune_e (R : rsets) (e : expr) : expr :=
  match e with
  | EVar x => EVar x
  | EUnit => EUnit
  | EVal v => EVal v
  | ETag n th e1 => ETag n th (prune_e R e1)
  | ECall f th sm args => ECall f th (keep R f sm) (map (prune_e R) args)
  | EInvoke f args => EInvoke f (map (prune_e R) args)
  | EMethod e0 m it args => EMethod (prune_e R e0) m it (map (prune_e R) args)
  | ELet x e1 e2 => ELet x (prune_e R e1) (prune_e R e2)
  | EBind b e1 => EBind (prune_b R b) (prune_e R e1)
  | ESeq e1 e2 => ESeq (prune_e R e1) (prune_e R e2)
  | EAssign x e1 => EAssign x (prune_e R e1)
  | EIf c e1 e2 => EIf (prune_e R c) (prune_e R e1) (prune_e R e2)
  | EWhile c b => EWhile (prune_e R c) (prune_e R b)
  | ELoop b => ELoop (prune_e R b)
  | EReturn e1 => EReturn (prune_e R e1)
  | EBreak => EBreak
  | ERecord fs => ERecord (map (fun p => (fst p, prune_e R (snd p))) fs)
  | EField e1 f => EField (prune_e R e1) f
  | EMatch e0 arms => EMatch (prune_e R e0) (map (fun p => (fst p, prune_e R (snd p))) arms)
  end
with prune_b (R : rsets) (b : binding) : binding :=
  match b with
  | BValue v e1 => BValue v (prune_e R e1)
  | BFunction f f' types sm => BFunction f f' types (keep R f' sm)
  | BMethod tk m th f' types sm => BMethod tk m th f' types (keep R f' sm)
  | b => b
  end.

Definition prune_code (R : rsets) (c : functor_code) : functor_code :=
  FunctorCode (fc_inputs c) (fc_linked c) (fc_table c) (fc_outputs c)
    (map (prune_b R) (fc_bindings c)) (fc_sources c).

(* The checker: every item `e` looks up, in an environment that agrees
   with the unpruned one on `inS`, is in `inS`. *)
Section Check.
Variable R : rsets.
Variable inS : item -> bool.
Definition srcs_ok (g : fsym) (sm : smap) : bool :=
  forallb (fun p => negb (inR R g (fst p)) || inS (snd p)) sm.
Definition all_ok (sm : smap) : bool := forallb (fun p => inS (snd p)) sm.

Fixpoint nchk (e : expr) : bool :=
  match e with
  | EVar _ | EUnit | EBreak => true
  | EVal v => inS (IVal v)
  | ETag _ _ e1 | EAssign _ e1 | ELoop e1 | EReturn e1 | EField e1 _ => nchk e1
  | ECall f _ sm args => srcs_ok f sm && forallb nchk args
  | EInvoke f args => inS (IFn f) && forallb nchk args
  | EMethod e0 _ it args => inS it && nchk e0 && forallb nchk args
  | ELet _ a b | ESeq a b | EWhile a b => nchk a && nchk b
  | EBind b e1 => bchk b && nchk e1
  | EIf a b c => nchk a && nchk b && nchk c
  | ERecord fs => forallb (fun p => nchk (snd p)) fs
  | EMatch e0 arms => nchk e0 && forallb (fun p => nchk (snd p)) arms
  end
with bchk (b : binding) : bool :=
  match b with
  | BType _ _ | BPrimitive _ _ => true
  | BValue _ e1 => nchk e1
  | BFunction _ f' _ sm | BMethod _ _ _ f' _ sm => srcs_ok f' sm
  | BFunctor _ _ _ _ smc => all_ok smc
  | BCopyFunctor _ u' _ _ => inS (IFun u')
  | BApply u _ _ sm _ => inS (IFun u) && all_ok sm
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

Fixpoint reads (e : expr) (s : iset) : iset :=
  match e with
  | EVar _ | EUnit | EBreak => s
  | EVal v => iadd (IVal v) s
  | ETag _ _ e1 | EAssign _ e1 | ELoop e1 | EReturn e1 | EField e1 _ => reads e1 s
  | ECall f _ sm args => fold_left (fun acc a => reads a acc) args (add_srcs f sm s)
  | EInvoke f args => fold_left (fun acc a => reads a acc) args (iadd (IFn f) s)
  | EMethod e0 _ it args => fold_left (fun acc a => reads a acc) args (reads e0 (iadd it s))
  | ELet _ a b | ESeq a b | EWhile a b => reads b (reads a s)
  | EBind b e1 => reads e1 (breads b s)
  | EIf a b c => reads c (reads b (reads a s))
  | ERecord fs => fold_left (fun acc p => reads (snd p) acc) fs s
  | EMatch e0 arms => fold_left (fun acc p => reads (snd p) acc) arms (reads e0 s)
  end
with breads (b : binding) (s : iset) : iset :=
  match b with
  | BType _ _ | BPrimitive _ _ => s
  | BValue _ e1 => reads e1 s
  | BFunction _ f' _ sm | BMethod _ _ _ f' _ sm => add_srcs f' sm s
  | BFunctor _ _ _ _ smc => add_all smc s
  | BCopyFunctor _ u' _ _ => iadd (IFun u') s
  | BApply u _ _ sm _ => add_all sm (iadd (IFun u) s)
  end.
End Compute.

(* One round: each fn's reads under the current sets (for a fn defined
   twice, the first body wins, as Elaboration.nlook). *)
Definition round (fns : list (nat * expr)) (R : rsets) : rsets :=
  fold_right (fun p acc => NatTrie.tset acc (fst p) (reads R (snd p) NatTrie.TE)) NatTrie.TE fns.

(* Functor bodies are checked against the full set, which always passes;
   checking them keeps the simulation free of a nested induction. *)
Definition check (R : rsets) (p : program) : bool :=
  forallb (fun fb => nchk R (inR R (fst fb)) (snd fb)) (program_functions p)
  && forallb (fun ub => forallb (bchk R full) (fc_bindings (snd ub))) (program_functors p).

Fixpoint compute_R (fuel : nat) (p : program) (R : rsets) : option rsets :=
  if check R p then Some R
  else match fuel with
       | 0 => None
       | S k => compute_R k p (round (program_functions p) R)
       end.

Definition prune_env (R : rsets) (main : fsym) (d : environment) : environment :=
  EnvironmentWithAliases (type_meanings d) (type_aliases d)
    (filter (fun q => inR R main (fst q)) (provision_values d)).

Definition prune_program (R : rsets) (p : program) : program :=
  Program (map (fun fb => (fst fb, prune_e R (snd fb))) (program_functions p))
          (map (fun ub => (fst ub, prune_code R (snd ub))) (program_functors p))
          (program_bool p) (program_true p) (program_false p) (program_main p)
          (prune_env R (program_main p) (program_environment p))
          (program_unsupported p) (program_casts p) (program_machines p).

(* The pass. *)
Definition prune (p : program) : program :=
  match compute_R 64 p NatTrie.TE with
  | Some R => prune_program R p
  | None => p
  end.

(* ===================== 3. Agreement ===================================== *)

Lemma lookup_entry_app : forall i l1 l2, lookup_entry i (l1 ++ l2) =
  match lookup_entry i l1 with Some a => Some a | None => lookup_entry i l2 end.
Proof.
  intros i l1 l2. induction l1 as [|[k v] r IH]; simpl; auto. destruct (item_eqb i k); auto.
Qed.

Lemma lookup_entry_filter : forall (p : item -> bool) i l, p i = true ->
  lookup_entry i (filter (fun q => p (fst q)) l) = lookup_entry i l.
Proof.
  intros p i l Hp. induction l as [|[k v] r IH]; simpl; auto.
  destruct (p k) eqn:Ek; simpl; destruct (item_eqb i k) eqn:E; auto.
  apply item_eqb_true in E; subst. congruence.
Qed.

Section Agree.
Variable R : rsets.

(* Entries agree when equal up to their captured environments; a closure's
   environment needs to agree only on its fn's read set, a functor
   closure's (never pruned) entry for entry. Types are never pruned. *)
Inductive prel : provision_value -> provision_value -> Prop :=
| PVal : forall w, prel (ValueProvision w) (ValueProvision w)
| PClo : forall f d d', agree (inR R f) d d' -> prel (FunctionClosure f d) (FunctionClosure f d')
| PFun : forall h sms d d' rms, lrel d d' ->
    prel (FunctorClosure h sms d rms) (FunctorClosure h sms d' rms)
| PPrim : forall p ot, prel (PrimitiveClosure p ot) (PrimitiveClosure p ot)
(* every lookup of an item in S that succeeds in the reference environment
   succeeds in the pruned one, with an agreeing entry *)
with agree : (item -> bool) -> environment -> environment -> Prop :=
| Agree : forall S d d',
    type_meanings d = type_meanings d' -> type_aliases d = type_aliases d' ->
    (forall i a, S i = true -> lookup_provision i d = Some a ->
       exists a', lookup_provision i d' = Some a' /\ prel a a') ->
    agree S d d'
(* pointwise: same types, same keys, agreeing entries *)
with lrel : environment -> environment -> Prop :=
| LRel : forall d d', type_meanings d = type_meanings d' -> type_aliases d = type_aliases d' ->
    lrel2 (provision_values d) (provision_values d') -> lrel d d'
with lrel2 : list (item * provision_value) -> list (item * provision_value) -> Prop :=
| L2Nil : lrel2 [] []
| L2Cons : forall i a a' l l', prel a a' -> lrel2 l l' -> lrel2 ((i, a) :: l) ((i, a') :: l').

Lemma agree_lookup : forall S d d', agree S d d' -> forall i a, S i = true ->
  lookup_provision i d = Some a -> exists a', lookup_provision i d' = Some a' /\ prel a a'.
Proof. intros S d d' H. inversion H; subst; eauto. Qed.
Lemma agree_tm : forall S d d', agree S d d' -> type_meanings d = type_meanings d'.
Proof. intros S d d' H. inversion H; assumption. Qed.
Lemma agree_ta : forall S d d', agree S d d' -> type_aliases d = type_aliases d'.
Proof. intros S d d' H. inversion H; assumption. Qed.
Lemma lrel_tm : forall d d', lrel d d' -> type_meanings d = type_meanings d'.
Proof. intros d d' H. inversion H; assumption. Qed.
Lemma lrel_ta : forall d d', lrel d d' -> type_aliases d = type_aliases d'.
Proof. intros d d' H. inversion H; assumption. Qed.
Lemma lrel_pv : forall d d', lrel d d' -> lrel2 (provision_values d) (provision_values d').
Proof. intros d d' H. inversion H; assumption. Qed.

Lemma lrel2_lookup : forall l l' i, lrel2 l l' ->
  match lookup_entry i l, lookup_entry i l' with
  | Some a, Some a' => prel a a'
  | None, None => True
  | _, _ => False
  end.
Proof.
  intros l l' i H. induction H; simpl; auto.
  destruct (item_eqb i i0); auto.
Qed.

Lemma lrel_agree : forall S d d', lrel d d' -> agree S d d'.
Proof.
  intros S d d' H. inversion H as [? ? Tm Ta Pv]; subst. constructor; auto.
  intros i a _ Hl. pose proof (lrel2_lookup i Pv) as Q. unfold lookup_provision in *.
  rewrite Hl in Q. destruct (lookup_entry i (provision_values d')); [eauto|contradiction].
Qed.

(* Rebuilding an environment's type parts over the same provisions. *)
Lemma agree_make : forall S d d' ts als, agree S d d' ->
  agree S (EnvironmentWithAliases ts als (provision_values d))
          (EnvironmentWithAliases ts als (provision_values d')).
Proof.
  intros S d d' ts als H. constructor; auto. intros i a Hi Hl. exact (agree_lookup H i Hi Hl).
Qed.
Lemma lrel_make : forall d d' ts als, lrel d d' ->
  lrel (EnvironmentWithAliases ts als (provision_values d))
       (EnvironmentWithAliases ts als (provision_values d')).
Proof. intros d d' ts als H. constructor; auto. exact (lrel_pv H). Qed.

Lemma agree_install : forall S d d' i a a', prel a a' -> agree S d d' ->
  agree S (install_provision i a d) (install_provision i a' d').
Proof.
  intros S d d' i a a' Ha H. constructor; simpl; [exact (agree_tm H)|exact (agree_ta H)|].
  intros j b Hj Hl. unfold lookup_provision in *. simpl in *.
  destruct (item_eqb j i); [injection Hl as <-; eauto|exact (agree_lookup H j Hj Hl)].
Qed.
Lemma lrel_install : forall d d' i a a', prel a a' -> lrel d d' ->
  lrel (install_provision i a d) (install_provision i a' d').
Proof.
  intros d d' i a a' Ha H. constructor; simpl; [exact (lrel_tm H)|exact (lrel_ta H)|].
  constructor; [exact Ha|exact (lrel_pv H)].
Qed.

Lemma lrel2_app : forall a a' b b', lrel2 a a' -> lrel2 b b' -> lrel2 (a ++ b) (a' ++ b').
Proof. intros a a' b b' H Hb. induction H; simpl; auto. constructor; auto. Qed.

Lemma agree_append : forall S a a' b b', lrel a a' -> agree S b b' ->
  agree S (append_environment a b) (append_environment a' b').
Proof.
  intros S a a' b b' Ha Hb. constructor; simpl;
    [rewrite (lrel_tm Ha), (agree_tm Hb)|rewrite (lrel_ta Ha), (agree_ta Hb)|]; auto.
  intros i x Hi Hl. unfold lookup_provision in *. simpl in *.
  rewrite lookup_entry_app in Hl |- *. pose proof (lrel2_lookup i (lrel_pv Ha)) as Q.
  destruct (lookup_entry i (provision_values a)) as [y|];
    destruct (lookup_entry i (provision_values a')) as [y'|]; try contradiction.
  - injection Hl as <-. eauto.
  - exact (agree_lookup Hb i Hi Hl).
Qed.
Lemma lrel_append : forall a a' b b', lrel a a' -> lrel b b' ->
  lrel (append_environment a b) (append_environment a' b').
Proof.
  intros a a' b b' Ha Hb. constructor; simpl;
    [rewrite (lrel_tm Ha), (lrel_tm Hb)|rewrite (lrel_ta Ha), (lrel_ta Hb)|]; auto.
  apply lrel2_app; apply lrel_pv; assumption.
Qed.

Lemma lrel_empty : lrel empty_environment empty_environment.
Proof. constructor; auto. constructor. Qed.

Lemma all_select : forall S d d' sm x, agree S d d' -> all_ok S sm = true ->
  select_provisions d sm = Some x -> exists x', select_provisions d' sm = Some x' /\ lrel x x'.
Proof.
  intros S d d' sm. induction sm as [|[req s] r IH]; simpl; intros x H Hok Hc.
  - injection Hc as <-. eauto using lrel_empty.
  - apply andb_true_iff in Hok as [Hs Hok].
    destruct (lookup_provision s d) as [a|] eqn:El; [|discriminate].
    destruct (select_provisions d r) as [dr|] eqn:Er; [|discriminate]. injection Hc as <-.
    destruct (agree_lookup H s Hs El) as [a' [-> Ha]].
    destruct (IH dr H Hok eq_refl) as [dr' [-> Hr]]. eauto using lrel_install.
Qed.

Lemma all_ok_full : forall sm, all_ok full sm = true.
Proof. intro sm. unfold all_ok. apply forallb_forall. reflexivity. Qed.

Lemma lrel_select : forall d d' sm x, lrel d d' ->
  select_provisions d sm = Some x -> exists x', select_provisions d' sm = Some x' /\ lrel x x'.
Proof. intros. eapply all_select; eauto using all_ok_full. apply (lrel_agree full); assumption. Qed.

Lemma lrel_adapt : forall sms d d' x, lrel d d' ->
  adapt_provisions d sms = Some x -> exists x', adapt_provisions d' sms = Some x' /\ lrel x x'.
Proof.
  intros [sm|] d d' x H Hc; simpl in *.
  - eapply lrel_select; eauto.
  - injection Hc as <-. eauto.
Qed.

Lemma keep_select : forall S d d' g sm x, agree S d d' -> srcs_ok R S g sm = true ->
  select_provisions d sm = Some x ->
  exists x', select_provisions d' (keep R g sm) = Some x' /\ agree (inR R g) x x'.
Proof.
  intros S d d' g sm. induction sm as [|[req s] r IH]; simpl; intros x H Hok Hc.
  - injection Hc as <-. exists empty_environment. split; auto. apply lrel_agree, lrel_empty.
  - apply andb_true_iff in Hok as [Hs Hok].
    destruct (lookup_provision s d) as [a|] eqn:El; [|discriminate].
    destruct (select_provisions d r) as [dr|] eqn:Er; [|discriminate]. injection Hc as <-.
    destruct (IH dr H Hok eq_refl) as [dr' [Hc' Hr]].
    destruct (inR R g req) eqn:Ein; simpl.
    + simpl in Hs. destruct (agree_lookup H s Hs El) as [a' [Hl' Ha]].
      rewrite Hl', Hc'. eexists; split; [reflexivity|].
      apply agree_install; auto.
    + rewrite Hc'. eexists; split; [reflexivity|].
      constructor; simpl; [exact (agree_tm Hr)|exact (agree_ta Hr)|].
      intros i b Hi Hl. unfold lookup_provision in Hl. simpl in Hl.
      destruct (item_eqb i req) eqn:Eq.
      * apply item_eqb_true in Eq; subst. rewrite Ein in Hi. discriminate.
      * exact (agree_lookup Hr i Hi Hl).
Qed.

(* Reference entries agree with themselves. *)
Lemma prel_refl : forall a, prel a a.
Proof.
  fix IH 1. intros [w|f [ts als ps]|h sms [ts als ps] rms|p ot]; try constructor.
  - assert (Hd : forall i a, lookup_entry i ps = Some a -> prel a a).
    { intros i. induction ps as [|[j en] r IR]; intros a E; [discriminate|].
      simpl in E. destruct (item_eqb i j); [injection E as <-; apply IH|apply IR, E]. }
    constructor; auto. intros i a _ E. exists a. split; [exact E|apply Hd with i; exact E].
  - constructor; auto. simpl. induction ps as [|[j en] r IR]; constructor; [apply IH|exact IR].
Qed.

Lemma agree_refl : forall S d, agree S d d.
Proof. intros S d. constructor; auto. intros i a _ Hl. exists a. split; auto using prel_refl. Qed.

Lemma agree_prune_env : forall main d, agree (inR R main) d (prune_env R main d).
Proof.
  intros main d. constructor; auto. intros i a Hi Hl. exists a. split; [|apply prel_refl].
  unfold lookup_provision, prune_env in *. simpl. rewrite lookup_entry_filter; auto.
Qed.

Lemma agree_ground : forall S d d', agree S d d' -> forall tau, ground d tau = ground d' tau.
Proof. intros S d d' H tau. unfold ground. rewrite (agree_tm H). reflexivity. Qed.

Lemma agree_capture : forall S T d d' types dc dc', agree T d d' -> agree S dc dc' ->
  agree S (typed_capture d types dc) (typed_capture d' types dc').
Proof.
  intros S T d d' types dc dc' H Hc. unfold typed_capture.
  replace (ground d') with (ground d)
    by (unfold ground; rewrite (agree_tm H); reflexivity).
  apply agree_make; exact Hc.
Qed.
Lemma lrel_capture : forall T d d' types dc dc', agree T d d' -> lrel dc dc' ->
  lrel (typed_capture d types dc) (typed_capture d' types dc').
Proof.
  intros T d d' types dc dc' H Hc. unfold typed_capture.
  replace (ground d') with (ground d)
    by (unfold ground; rewrite (agree_tm H); reflexivity).
  apply lrel_make; exact Hc.
Qed.

Lemma agree_call : forall S T d d' th dc dc', agree T d d' -> agree S dc dc' ->
  agree S (call_environment d th dc) (call_environment d' th dc').
Proof.
  intros S T d d' th dc dc' H Hc. unfold call_environment, append_environment, instantiate.
  simpl. replace (ground d') with (ground d)
    by (unfold ground; rewrite (agree_tm H); reflexivity).
  rewrite (agree_tm Hc), (agree_ta Hc). apply agree_make; exact Hc.
Qed.

Lemma agree_bind_type : forall S d d' t tau, agree S d d' ->
  agree S (bind_type d t tau) (bind_type d' t tau).
Proof.
  intros S d d' t tau H. unfold bind_type, ground. rewrite (agree_tm H), (agree_ta H).
  apply agree_make; exact H.
Qed.

Lemma prim_closure_eq : forall S d d' i p, agree S d d' ->
  primitive_closure d i p = primitive_closure d' i p.
Proof. intros S d d' i p H. unfold primitive_closure, ground. rewrite (agree_tm H). reflexivity. Qed.

Lemma output_meaning_eq : forall d d', type_meanings d = type_meanings d' ->
  type_aliases d = type_aliases d' -> forall s, output_meaning d s = output_meaning d' s.
Proof. intros d d' Tm Ta s. unfold output_meaning. rewrite Tm, Ta. reflexivity. Qed.

Lemma read_result_types_eq : forall d d', type_meanings d = type_meanings d' ->
  type_aliases d = type_aliases d' -> forall Dr outputs,
  read_result_types d Dr outputs = read_result_types d' Dr outputs.
Proof.
  intros d d' Tm Ta Dr. induction outputs as [|[s t] r IH]; simpl; auto.
  rewrite (output_meaning_eq _ _ Tm Ta s), IH. reflexivity.
Qed.

Lemma result_env_rel : forall S outputs d3 d3' Dr dr dr', agree S d3 d3' -> lrel dr dr' ->
  match result_environment outputs d3 Dr dr, result_environment outputs d3' Dr dr' with
  | Some x, Some x' => lrel x x'
  | None, None => True
  | _, _ => False
  end.
Proof.
  intros S outputs d3 d3' Dr dr dr' H Hd. unfold result_environment.
  rewrite (read_result_types_eq _ _ (agree_tm H) (agree_ta H)).
  destruct (read_result_types d3' Dr outputs) as [[w a]|]; auto.
  constructor; auto. exact (lrel_pv Hd).
Qed.

End Agree.

Arguments agree_lookup {R S d d'} _ i {a}.
Arguments lrel_agree {R} S {d d'}.
Arguments keep_select {R S d d' g sm x}.
Arguments all_select {R S d d' sm x}.

(* ===================== 4. Simulation ===================================== *)

Module Sim (Prims : PRIMITIVES) (I : INTERP Prims).
Import Prims I.

(* A state invariant the primitives keep (Public's: the interface carried in
   the machine state never changes) is kept by every evaluation. *)
Section Inv.
Variable P : program_view.
Variable ok : mstate -> Prop.
Hypothesis Hok : forall q site ws st, ok st -> ok (snd (run_prim P q site ws st)).

Definition okr (r : evaluation) : Prop := ok (evaluation_state r).
Definition okl {A : Type} (r : (A * locals * mstate) + evaluation) : Prop :=
  match r with inl (_, _, st) => ok st | inr r => okr r end.
Definition oka (r : (environment * mstate) + evaluation) : Prop :=
  match r with inl (_, st) => ok st | inr r => okr r end.

Definition INV (k : nat) : Prop :=
  (forall d g st e, ok st -> okr (eval k P d g st e))
  /\ (forall d g st l, ok st -> okl (eval_arguments k P d g st l))
  /\ (forall d g st bs, ok st -> okl (eval_bindings k P d g st bs))
  /\ (forall d u th outputs sm rs st, ok st -> oka (apply_functor k P d u th outputs sm rs st)).

Lemma inv_all : forall k, INV k.
Proof.
  induction k as [|k [IHe [IHl [IHb IHa]]]].
  { repeat split; intros; simpl; assumption. }
  unfold INV, okr, okl, oka in *.
  repeat split.
  all: intros; simpl.
  all: repeat (unfold run_prim_at, ret_call, pop1, popn, with_g in *; simpl in *;
         match goal with
         | H : ok ?st |- context [eval ?k' P ?d ?g ?st ?e] =>
             let Hn := fresh "Hn" in pose proof (IHe d g st e H) as Hn;
             destruct (eval k' P d g st e) eqn:?; simpl in Hn
         | H : ok ?st |- context [eval_arguments ?k' P ?d ?g ?st ?l] =>
             let Hn := fresh "Hn" in pose proof (IHl d g st l H) as Hn;
             destruct (eval_arguments k' P d g st l) as [[[? ?] ?]|] eqn:?; simpl in Hn
         | H : ok ?st |- context [eval_bindings ?k' P ?d ?g ?st ?l] =>
             let Hn := fresh "Hn" in pose proof (IHb d g st l H) as Hn;
             destruct (eval_bindings k' P d g st l) as [[[? ?] ?]|] eqn:?; simpl in Hn
         | H : ok ?st |- context [apply_functor ?k' P ?d ?u ?th ?o ?sm ?rs ?st] =>
             let Hn := fresh "Hn" in pose proof (IHa d u th o sm rs st H) as Hn;
             destruct (apply_functor k' P d u th o sm rs st) as [[? ?]|] eqn:?; simpl in Hn
         | |- context [run_prim ?PP ?q ?site ?ws ?st] =>
             let Hn := fresh "Hn" in pose proof (@Hok q site ws st ltac:(assumption)) as Hn;
             destruct (run_prim PP q site ws st) eqn:?; simpl in Hn
         | |- context [match ?x with _ => _ end] =>
             lazymatch x with
             | context [match _ with _ => _ end] => fail
             | context [run_prim] => fail
             | context [eval] => fail
             | context [eval_arguments] => fail
             | context [eval_bindings] => fail
             | context [apply_functor] => fail
             | _ => destruct x eqn:?
             end
         end); try assumption.
  all: unfold okr in *; simpl in *; try assumption.
Qed.
End Inv.

Arguments okl {ok A} r.
Arguments oka {ok} r.

Section SimSec.
Variable R : rsets.
Variables P P' : program_view.
Variable ok : mstate -> Prop.
Hypothesis Hfn : forall f, view_function P' f = option_map (prune_e R) (view_function P f).
Hypothesis Hfun : forall h, view_functor P' h = option_map (prune_code R) (view_functor P h).
Hypothesis Hprim : forall q site ws st, ok st -> run_prim P' q site ws st = run_prim P q site ws st.
Hypothesis Hok : forall q site ws st, ok st -> ok (snd (run_prim P q site ws st)).
Hypothesis Hbool : view_bool P' = view_bool P.
Hypothesis Htrue : view_true P' = view_true P.
Hypothesis Hfalse : view_false P' = view_false P.
Hypothesis Hcasts : view_casts P' = view_casts P.
Hypothesis Hclosed : forall f b, view_function P f = Some b -> nchk R (inR R f) b = true.
Hypothesis Hfunchk : forall h c, view_functor P h = Some c ->
  forallb (bchk R full) (fc_bindings c) = true.

Definition stuck (r : evaluation) : Prop := exists c, evaluation_signal r = Stuck c.
Definition stuckl {A} (r : A + evaluation) : Prop :=
  match r with inr r => stuck r | inl _ => False end.

Definition brel (S : item -> bool) (a b : (environment * locals * mstate) + evaluation) : Prop :=
  match a, b with
  | inl (d', g', st'), inl (d, g, st) => agree R S d d' /\ g' = g /\ st' = st
  | inr r', inr r => r' = r
  | _, _ => False
  end.

Definition arel (a b : (environment * mstate) + evaluation) : Prop :=
  match a, b with
  | inl (d', st'), inl (d, st) => lrel R d d' /\ st' = st
  | inr r', inr r => r' = r
  | _, _ => False
  end.

Definition SIM (k : nat) : Prop :=
  (forall S d d' g st e, nchk R S e = true -> agree R S d d' -> ok st ->
     stuck (eval k P d g st e) \/ eval k P' d' g st (prune_e R e) = eval k P d g st e)
  /\ (forall S d d' g st l, forallb (nchk R S) l = true -> agree R S d d' -> ok st ->
     stuckl (eval_arguments k P d g st l)
     \/ eval_arguments k P' d' g st (map (prune_e R) l) = eval_arguments k P d g st l)
  /\ (forall S d d' g st bs, forallb (bchk R S) bs = true -> agree R S d d' -> ok st ->
     stuckl (eval_bindings k P d g st bs)
     \/ brel S (eval_bindings k P' d' g st (map (prune_b R) bs)) (eval_bindings k P d g st bs))
  /\ (forall S d d' u th outputs sm rs st, S (IFun u) = true -> all_ok S sm = true ->
     agree R S d d' -> ok st ->
     stuckl (apply_functor k P d u th outputs sm rs st)
     \/ arel (apply_functor k P' d' u th outputs sm rs st) (apply_functor k P d u th outputs sm rs st)).

Lemma inv_e : forall k d g st e, ok st -> ok (evaluation_state (eval k P d g st e)).
Proof. intros k. exact (proj1 (@inv_all P ok Hok k)). Qed.
Lemma inv_l : forall k d g st l, ok st -> okl (ok := ok) (eval_arguments k P d g st l).
Proof. intros k. exact (proj1 (proj2 (@inv_all P ok Hok k))). Qed.
Lemma inv_b : forall k d g st bs, ok st -> okl (ok := ok) (eval_bindings k P d g st bs).
Proof. intros k. exact (proj1 (proj2 (proj2 (@inv_all P ok Hok k)))). Qed.

Lemma select_arm_prune : forall arms w,
  select_arm (map (fun p => (fst p, prune_e R (snd p))) arms) w
  = option_map (fun q => (fst q, prune_e R (snd q))) (select_arm arms w).
Proof.
  induction arms as [|[pt eb] r IH]; intros w; simpl; auto.
  destruct (match_pat pt w); simpl; auto.
Qed.
Lemma select_arm_chk : forall S arms w bs eb,
  forallb (fun p => nchk R S (snd p)) arms = true -> select_arm arms w = Some (bs, eb) ->
  nchk R S eb = true.
Proof.
  intros S. induction arms as [|[pt b] r IH]; intros w bs eb Hc Ha; simpl in *; [discriminate|].
  apply andb_true_iff in Hc as [Hb Hr].
  destruct (match_pat pt w); [injection Ha as <- <-; exact Hb|eauto|discriminate].
Qed.
Lemma map_snd_prune : forall (fs : list (fld * expr)),
  map snd (map (fun p => (fst p, prune_e R (snd p))) fs) = map (prune_e R) (map snd fs).
Proof. intros. rewrite !map_map. reflexivity. Qed.
Lemma map_fst_prune : forall (fs : list (fld * expr)),
  map fst (map (fun p => (fst p, prune_e R (snd p))) fs) = map fst fs.
Proof. intros. rewrite map_map. reflexivity. Qed.
Lemma forallb_snd : forall S (fs : list (fld * expr)),
  forallb (fun p => nchk R S (snd p)) fs = forallb (nchk R S) (map snd fs).
Proof. intros. induction fs; simpl; congruence. Qed.

Lemma truthy_eq : forall w, truthy P' w = truthy P w.
Proof. intros. unfold truthy. rewrite Hbool, Htrue, Hfalse. reflexivity. Qed.
Lemma check_tag_eq : forall S d d' n th w, agree R S d d' ->
  check_tag P' d' n th w = check_tag P d n th w.
Proof. intros S d d' n th w H. unfold check_tag, ground. rewrite Hcasts, (agree_tm H). reflexivity. Qed.
Lemma prim_at_eq : forall p ot ws st, ok st -> run_prim_at P' p ot ws st = run_prim_at P p ot ws st.
Proof. intros. unfold run_prim_at. rewrite Hprim by assumption. reflexivity. Qed.

Lemma app_env_rel : forall T d d' h th dc dc', agree R T d d' -> lrel R dc dc' ->
  lrel R (application_environment P d h th dc) (application_environment P' d' h th dc').
Proof.
  intros T d d' h th dc dc' H Hc. unfold application_environment. rewrite Hfun.
  replace (ground d') with (ground d) by (unfold ground; rewrite (agree_tm H); reflexivity).
  replace (output_meaning dc') with (output_meaning dc).
  2:{ unfold output_meaning. rewrite (lrel_tm Hc), (lrel_ta Hc). reflexivity. }
  destruct (view_functor P h) as [code|]; simpl; apply lrel_make; exact Hc.
Qed.

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

Ltac err := right; simpl; reflexivity.
Ltac stuck_here := left; simpl; eexists; reflexivity.

Lemma sim_step : forall k, SIM k -> SIM (S k).
Proof.
  intros k [IHe [IHl [IHb IHa]]].
  split; [|split; [|split]].
  - intros S d d' g st e Hc H Hst.
    destruct e as [x| |v|n th e1|f th sm args|f args|e0 m it args|x e1 e2|bd e1|e1 e2|x e1
                  |c e1 e2|c e1|e1|e1| |fs|e1 fl|e0 arms];
      simpl in Hc |- *; andb_split.
    + (* EVar *) err.
    + (* EUnit *) err.
    + (* EVal *)
      destruct (lookup_provision (IVal v) d) as [a|] eqn:El; [|stuck_here].
      destruct (agree_lookup H _ Hc El) as [a' [-> Ha]].
      inversion Ha; subst; try stuck_here. err.
    + (* ETag *)
      destruct (IHe S d d' g st e1 Hc H Hst) as [Hs| ->]; [stk|].
      destruct (eval k P d g st e1) as [[w| | | | | | |] g1 st1]; try err.
      rewrite (check_tag_eq n th w H). err.
    + (* ECall *)
      rewrite Hfn. destruct (view_function P f) as [body|] eqn:Ef; simpl; [|err].
      destruct (IHl S d d' g st args ltac:(eassumption) H Hst) as [Hs| ->]; [stk|].
      pose proof (@inv_l k d g st args Hst) as Hn.
      destruct (eval_arguments k P d g st args) as [[[ws g1] st1]|]; simpl in Hn; [|err].
      destruct (select_provisions d sm) as [dc|] eqn:Ec; [|stuck_here].
      destruct (keep_select H ltac:(eassumption) Ec) as [dc' [-> Hdc]].
      destruct (IHe _ (call_environment d th dc) (call_environment d' th dc') (parameter_values ws)
                  st1 body (Hclosed f Ef) (agree_call th H Hdc) Hn) as [Hs| ->]; [|err].
      left. destruct (eval k P (call_environment d th dc) (parameter_values ws) st1 body) as [o0 g0 s0].
      destruct Hs as [c Hc']. simpl in Hc'. subst. simpl. eexists; reflexivity.
    + (* EInvoke *)
      destruct (lookup_provision (IFn f) d) as [a|] eqn:El; [|stuck_here].
      destruct (agree_lookup H _ ltac:(eassumption) El) as [a' [-> Ha]].
      inversion Ha as [|f' df df' Hdf| |]; subst; try stuck_here.
      * rewrite Hfn. destruct (view_function P f') as [body|] eqn:Ef; simpl; [|err].
        destruct (IHl S d d' g st args ltac:(eassumption) H Hst) as [Hs| ->]; [stk|].
        pose proof (@inv_l k d g st args Hst) as Hn.
        destruct (eval_arguments k P d g st args) as [[[ws g1] st1]|]; simpl in Hn; [|err].
        destruct (IHe _ df df' (parameter_values ws) st1 body (Hclosed f' Ef) Hdf Hn) as [Hs| ->]; [|err].
        left. destruct (eval k P df (parameter_values ws) st1 body) as [o0 g0 s0].
        destruct Hs as [c Hc']. simpl in Hc'. subst. simpl. eexists; reflexivity.
      * destruct (IHl S d d' g st args ltac:(eassumption) H Hst) as [Hs| ->]; [stk|].
        pose proof (@inv_l k d g st args Hst) as Hn.
        destruct (eval_arguments k P d g st args) as [[[ws g1] st1]|]; simpl in Hn; [|err].
        rewrite prim_at_eq by exact Hn. err.
    + (* EMethod *)
      destruct (lookup_provision it d) as [a|] eqn:El; [|stuck_here].
      destruct (agree_lookup H _ ltac:(eassumption) El) as [a' [-> Ha]].
      inversion Ha as [|f' df df' Hdf| |]; subst; try stuck_here.
      * rewrite Hfn. destruct (view_function P f') as [body|] eqn:Ef; simpl; [|err].
        destruct (IHe S d d' g st e0 ltac:(eassumption) H Hst) as [Hs| ->]; [stk|].
        pose proof (@inv_e k d g st e0 Hst) as Hn0.
        destruct (eval k P d g st e0) as [[w0| | | | | | |] g0 st0]; simpl in Hn0; try err.
        destruct (IHl S d d' g0 st0 args ltac:(eassumption) H Hn0) as [Hs| ->]; [stk|].
        pose proof (@inv_l k d g0 st0 args Hn0) as Hn.
        destruct (eval_arguments k P d g0 st0 args) as [[[ws g1] st1]|]; simpl in Hn; [|err].
        destruct (IHe _ df df' (parameter_values (w0 :: ws)) st1 body (Hclosed f' Ef) Hdf Hn)
          as [Hs| ->]; [|err].
        left. destruct (eval k P df (parameter_values (w0 :: ws)) st1 body) as [o0 g2 s0].
        destruct Hs as [c Hc']. simpl in Hc'. subst. simpl. eexists; reflexivity.
      * destruct (IHe S d d' g st e0 ltac:(eassumption) H Hst) as [Hs| ->]; [stk|].
        pose proof (@inv_e k d g st e0 Hst) as Hn0.
        destruct (eval k P d g st e0) as [[w0| | | | | | |] g0 st0]; simpl in Hn0; try err.
        destruct (IHl S d d' g0 st0 args ltac:(eassumption) H Hn0) as [Hs| ->]; [stk|].
        pose proof (@inv_l k d g0 st0 args Hn0) as Hn.
        destruct (eval_arguments k P d g0 st0 args) as [[[ws g1] st1]|]; simpl in Hn; [|err].
        rewrite prim_at_eq by exact Hn. err.
    + (* ELet *)
      destruct (IHe S d d' g st e1 ltac:(eassumption) H Hst) as [Hs| ->]; [stk|].
      pose proof (@inv_e k d g st e1 Hst) as Hn.
      destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      destruct (IHe S d d' ((x, w0) :: g0) st0 e2 ltac:(eassumption) H Hn) as [Hs| ->]; [stk|err].
    + (* EBind *)
      destruct (IHb S d d' g st [bd] ltac:(simpl; rewrite andb_true_r; assumption) H Hst)
        as [Hs|Hb]; [stk|].
      pose proof (@inv_b k d g st [bd] Hst) as Hn. simpl in Hb.
      destruct (eval_bindings k P' d' g st [prune_b R bd]) as [[[d1' g1'] st1']|r1'],
               (eval_bindings k P d g st [bd]) as [[[d1 g1] st1]|r1]; simpl in Hb, Hn;
        try contradiction.
      * destruct Hb as [Hd [-> ->]]. exact (IHe S d1 d1' g1 st1 e1 ltac:(assumption) Hd Hn).
      * subst. err.
    + (* ESeq *)
      destruct (IHe S d d' g st e1 ltac:(eassumption) H Hst) as [Hs| ->]; [stk|].
      pose proof (@inv_e k d g st e1 Hst) as Hn.
      destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      apply (IHe S); auto.
    + (* EAssign *)
      destruct (IHe S d d' g st e1 Hc H Hst) as [Hs| ->]; [stk|err].
    + (* EIf *)
      destruct (IHe S d d' g st c ltac:(eassumption) H Hst) as [Hs| ->]; [stk|].
      pose proof (@inv_e k d g st c Hst) as Hn.
      destruct (eval k P d g st c) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      rewrite truthy_eq. destruct (truthy P w0) as [[]|]; try err; apply (IHe S); auto.
    + (* EWhile *)
      destruct (IHe S d d' g st c ltac:(eassumption) H Hst) as [Hs| ->]; [stk|].
      pose proof (@inv_e k d g st c Hst) as Hn.
      destruct (eval k P d g st c) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      rewrite truthy_eq. destruct (truthy P w0) as [[]|]; try err.
      destruct (IHe S d d' g0 st0 e1 ltac:(eassumption) H Hn) as [Hs| ->]; [stk|].
      pose proof (@inv_e k d g0 st0 e1 Hn) as Hn1.
      destruct (eval k P d g0 st0 e1) as [[w1| | | | | | |] g1 st1]; simpl in Hn1; try err.
      change (EWhile (prune_e R c) (prune_e R e1)) with (prune_e R (EWhile c e1)).
      apply (IHe S); simpl; auto. apply andb_true_iff; split; assumption.
    + (* ELoop *)
      destruct (IHe S d d' g st e1 Hc H Hst) as [Hs| ->]; [stk|].
      pose proof (@inv_e k d g st e1 Hst) as Hn.
      destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      change (ELoop (prune_e R e1)) with (prune_e R (ELoop e1)).
      apply (IHe S); simpl; auto.
    + (* EReturn *)
      destruct (IHe S d d' g st e1 Hc H Hst) as [Hs| ->]; [stk|err].
    + (* EBreak *) err.
    + (* ERecord *)
      rewrite map_snd_prune, map_fst_prune. rewrite forallb_snd in Hc.
      destruct (IHl S d d' g st (map snd fs) Hc H Hst) as [Hs| ->]; [stk|err].
    + (* EField *)
      destruct (IHe S d d' g st e1 Hc H Hst) as [Hs| ->]; [stk|err].
    + (* EMatch *)
      destruct (IHe S d d' g st e0 ltac:(eassumption) H Hst) as [Hs| ->]; [stk|].
      pose proof (@inv_e k d g st e0 Hst) as Hn.
      destruct (eval k P d g st e0) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      rewrite select_arm_prune. destruct (select_arm arms w0) as [[bs eb]|] eqn:Ea; simpl; [|err].
      assert (Hb : nchk R S eb = true) by (eapply select_arm_chk; eassumption).
      destruct (IHe S d d' (bs ++ g0) st0 eb Hb H Hn) as [Hs| ->]; [stk|err].
  - (* eval_arguments *)
    intros S d d' g st l Hc H Hst. destruct l as [|e1 r]; simpl in Hc |- *; [err|]. andb_split.
    destruct (IHe S d d' g st e1 ltac:(eassumption) H Hst) as [Hs| ->]; [stk|].
    pose proof (@inv_e k d g st e1 Hst) as Hn.
    destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
    destruct (IHl S d d' g0 st0 r ltac:(eassumption) H Hn) as [Hs| ->]; [stk|].
    destruct (eval_arguments k P d g0 st0 r) as [[[ws g1] st1]|]; err.
  - (* eval_bindings *)
    intros S d d' g st bs Hc H Hst. destruct bs as [|b r]; simpl in Hc |- *.
    { right. simpl. auto. }
    andb_split.
    destruct b as [t tau|v e1|f f' types sm|tk m th f' types sm|u h types sm smc|u u' sm rm
                  |u th outputs sm rs|i p]; simpl in *; andb_split.
    + (* BType *) exact (IHb S _ _ g st r ltac:(assumption) (agree_bind_type t tau H) Hst).
    + (* BValue *)
      destruct (IHe S d d' g st e1 ltac:(eassumption) H Hst) as [Hs| ->]; [stk|].
      pose proof (@inv_e k d g st e1 Hst) as Hn.
      destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try err.
      apply (IHb S); auto. apply agree_install; auto. constructor.
    + (* BFunction *)
      destruct (select_provisions d sm) as [dc|] eqn:Ec; [|stuck_here].
      destruct (keep_select H ltac:(eassumption) Ec) as [dc' [-> Hdc]].
      apply (IHb S); auto. apply agree_install; auto. constructor. eapply agree_capture; eauto.
    + (* BMethod *)
      destruct (select_provisions d sm) as [dc|] eqn:Ec; [|stuck_here].
      destruct (keep_select H ltac:(eassumption) Ec) as [dc' [-> Hdc]].
      apply (IHb S); auto. apply agree_install; auto. constructor. eapply agree_capture; eauto.
    + (* BFunctor *)
      destruct (select_provisions d smc) as [dc|] eqn:Ec; [|stuck_here].
      destruct (all_select H ltac:(eassumption) Ec) as [dc' [-> Hdc]].
      apply (IHb S); auto. apply agree_install; auto. constructor. eapply lrel_capture; eauto.
    + (* BCopyFunctor *)
      destruct (lookup_provision (IFun u') d) as [a|] eqn:El; [|stuck_here].
      destruct (agree_lookup H _ ltac:(eassumption) El) as [a' [-> Ha]].
      inversion Ha as [| |h sms dc dc' rms Hdc|]; subst; try stuck_here.
      apply (IHb S); auto. apply agree_install; auto. constructor. exact Hdc.
    + (* BApply *)
      destruct (IHa S d d' u th outputs sm rs st ltac:(eassumption) ltac:(eassumption) H Hst)
        as [Hs|Ha]; [stk|].
      pose proof (proj2 (proj2 (proj2 (@inv_all P ok Hok k))) d u th outputs sm rs st Hst) as Hn.
      destruct (apply_functor k P' d' u th outputs sm rs st) as [[dr' st1']|r1'],
               (apply_functor k P d u th outputs sm rs st) as [[dr st1]|r1]; simpl in Ha, Hn;
        try contradiction.
      * destruct Ha as [Hdr ->]. apply (IHb S); auto. apply agree_append; auto.
      * subst. err.
    + (* BPrimitive *)
      rewrite <- (prim_closure_eq i p H).
      apply (IHb S); auto. apply agree_install; auto. apply prel_refl.
  - (* apply_functor *)
    intros S d d' u th outputs sm rs st Hu Hsm H Hst. simpl.
    destruct (lookup_provision (IFun u) d) as [a|] eqn:El; [|stuck_here].
    destruct (agree_lookup H (IFun u) Hu El) as [a' [-> Ha]].
    inversion Ha as [| |h sms dc dc' rms Hdc|]; subst; try err.
    rewrite Hfun. destruct (view_functor P h) as [code|] eqn:Eh; simpl; [|err].
    destruct (select_provisions d sm) as [d1|] eqn:E1; [|stuck_here].
    destruct (all_select H Hsm E1) as [d1' [-> Hd1]].
    destruct (adapt_provisions d1 sms) as [d2|] eqn:E2; [|stuck_here].
    destruct (lrel_adapt sms Hd1 E2) as [d2' [-> Hd2]].
    assert (Henv : lrel R (application_environment P d h th (append_environment d2 dc))
                          (application_environment P' d' h th (append_environment d2' dc'))).
    { eapply app_env_rel; [exact H|]. apply lrel_append; assumption. }
    destruct (IHb full _ _ [] st (fc_bindings code) (Hfunchk h Eh) (lrel_agree full Henv) Hst)
      as [Hs|Hb]; [stk|].
    pose proof (@inv_b k (application_environment P d h th (append_environment d2 dc)) []
                  st (fc_bindings code) Hst) as Hn.
    destruct (eval_bindings k P' (application_environment P' d' h th (append_environment d2' dc'))
                [] st (map (prune_b R) (fc_bindings code))) as [[[d3' g3'] st3']|r1'],
             (eval_bindings k P (application_environment P d h th (append_environment d2 dc))
                [] st (fc_bindings code)) as [[[d3 g3] st3]|r1]; simpl in Hb, Hn;
      try contradiction.
    + destruct Hb as [Hd3 [_ ->]].
      destruct (select_provisions d3 (result_selection rs rms (fc_sources code))) as [dr|] eqn:Er;
        [|stuck_here].
      destruct (all_select Hd3 (all_ok_full _) Er) as [dr' [-> Hdr]].
      pose proof (result_env_rel outputs (fc_outputs code) Hd3 Hdr) as Q.
      destruct (result_environment outputs d3 (fc_outputs code) dr),
               (result_environment outputs d3' (fc_outputs code) dr'); try contradiction.
      * right. simpl. auto.
      * err.
    + subst. err.
Qed.

Lemma sim_all : forall k, SIM k.
Proof.
  induction k as [|k IH]; [|exact (sim_step IH)].
  repeat split; intros; right; reflexivity.
Qed.

End SimSec.

Lemma nlook_map : forall A B (f : A -> B) k (l : list (nat * A)),
  Elaboration.nlook k (map (fun p => (fst p, f (snd p))) l) = option_map f (Elaboration.nlook k l).
Proof.
  intros A B f k l. induction l as [|[k' a] r IH]; simpl; auto.
  destruct (Nat.eqb k k'); auto.
Qed.

Lemma nlook_in : forall A k (l : list (nat * A)) a, Elaboration.nlook k l = Some a -> In (k, a) l.
Proof.
  induction l as [|[k' a'] r IH]; simpl; intros a H; [discriminate|].
  destruct (Nat.eqb k k') eqn:E; [apply Nat.eqb_eq in E; injection H as ->; subst; auto|].
  right. auto.
Qed.

(* Running `main` of the pruned program gives the reference result, unless
   the reference run is stuck, for any primitives that ignore the code of
   the program view (on states satisfying an invariant they keep). *)
Theorem run_main_pruned : forall R p argv rd fuel st0 (ok : mstate -> Prop),
  check R p = true ->
  let P := program_of p argv rd in
  let P' := program_of (prune_program R p) argv rd in
  (forall q site ws st, ok st -> run_prim P' q site ws st = run_prim P q site ws st) ->
  (forall q site ws st, ok st -> ok (snd (run_prim P q site ws st))) ->
  ok st0 ->
  stuck (run_main fuel P (program_main p) (program_environment p) st0)
  \/ run_main fuel P' (program_main (prune_program R p)) (program_environment (prune_program R p)) st0
     = run_main fuel P (program_main p) (program_environment p) st0.
Proof.
  intros R p argv rd fuel st0 ok Hchk P P' Hprim Hok Hst.
  unfold check in Hchk. apply andb_true_iff in Hchk as [Hc1 Hc2].
  assert (Hfn : forall f, view_function P' f = option_map (prune_e R) (view_function P f)).
  { intro f. unfold P, P', program_of, prune_program. simpl. apply nlook_map. }
  assert (Hfun : forall h, view_functor P' h = option_map (prune_code R) (view_functor P h)).
  { intro h. unfold P, P', program_of, prune_program. simpl. apply nlook_map. }
  assert (Hclosed : forall f b, view_function P f = Some b -> nchk R (inR R f) b = true).
  { intros f b Hf. apply nlook_in in Hf. rewrite forallb_forall in Hc1. exact (Hc1 (f, b) Hf). }
  assert (Hfunchk : forall h c, view_functor P h = Some c ->
            forallb (bchk R full) (fc_bindings c) = true).
  { intros h c Hf. apply nlook_in in Hf. rewrite forallb_forall in Hc2. exact (Hc2 (h, c) Hf). }
  destruct (@sim_all R P P' ok Hfn Hfun Hprim Hok eq_refl eq_refl eq_refl eq_refl Hclosed Hfunchk fuel)
    as [Hsim _].
  unfold run_main. simpl program_main. rewrite Hfn.
  destruct (view_function P (program_main p)) as [body|] eqn:Em; simpl; [|right; reflexivity].
  destruct (Hsim _ (program_environment p) (prune_env R (program_main p) (program_environment p))
              [] st0 body (Hclosed _ _ Em) (agree_prune_env R _ _) Hst) as [Hs| ->];
    [|right; reflexivity].
  left. destruct (eval fuel P (program_environment p) [] st0 body) as [o g s].
  destruct Hs as [c Hc]. simpl in Hc. subst. simpl. eexists; reflexivity.
Qed.

Lemma compute_R_check : forall n p R0 R, compute_R n p R0 = Some R -> check R p = true.
Proof.
  induction n as [|n IH]; intros p R0 R H; simpl in H;
    destruct (check R0 p) eqn:E; try (injection H as <-; exact E); try discriminate; eauto.
Qed.

(* The pass itself: no premise on the program. *)
Theorem prune_sound : forall p argv rd fuel st0 (ok : mstate -> Prop),
  let P := program_of p argv rd in
  let P' := program_of (prune p) argv rd in
  (forall q site ws st, ok st -> run_prim P' q site ws st = run_prim P q site ws st) ->
  (forall q site ws st, ok st -> ok (snd (run_prim P q site ws st))) ->
  ok st0 ->
  stuck (run_main fuel P (program_main p) (program_environment p) st0)
  \/ run_main fuel P' (program_main (prune p)) (program_environment (prune p)) st0
     = run_main fuel P (program_main p) (program_environment p) st0.
Proof.
  intros p argv rd fuel st0 ok P P'. unfold P', prune.
  destruct (compute_R 64 p NatTrie.TE) as [R|] eqn:E.
  - apply run_main_pruned. eapply compute_R_check; eauto.
  - intros. right. reflexivity.
Qed.

End Sim.

(* The pass keeps everything but code and main's environment. *)
Lemma prune_view : forall p argv rd,
  let P' := program_of (prune p) argv rd in let P := program_of p argv rd in
  view_bool P' = view_bool P /\ view_true P' = view_true P /\ view_false P' = view_false P
  /\ view_arguments P' = view_arguments P /\ view_read P' = view_read P
  /\ view_casts P' = view_casts P.
Proof. intros p argv rd P' P. unfold P', P, prune. destruct (compute_R 64 p NatTrie.TE); simpl; repeat split. Qed.

(* ===================== 5. Public runs ==================================== *)

Module PruneRuns (Host : HOST) (M : PUBLIC Host).
Module PS := Sim M.ReferenceHost M.Reference.

(* Audit passes the program view to primitives; pruning changes the view's
   code, so the interface must not read it (the concrete hosts read only
   argv, the file snapshot and Bool's symbols). *)
Definition code_blind (interface : M.interface) : Prop :=
  forall P P', view_bool P' = view_bool P -> view_true P' = view_true P ->
    view_false P' = view_false P -> view_arguments P' = view_arguments P ->
    view_read P' = view_read P -> view_casts P' = view_casts P ->
    forall q ws s, M.primitive interface P' q ws s = M.primitive interface P q ws s.

(* For a program that never runs to Wrong (public_safety: any admissible
   program on a compatible interface), the pruned program's observation at
   every fuel is the original's. *)
Theorem observe_pruned : forall program interface initial,
  code_blind interface ->
  (forall code final, ~ M.Runs program interface initial (M.Result (M.Wrong code) final)) ->
  forall fuel,
    M.observe (M.Reference.run_main fuel (M.runtime (prune program) initial)
      (program_main (prune program)) (program_environment (prune program)) (interface, initial))
    = M.observe (M.Reference.run_main fuel (M.runtime program initial)
      (program_main program) (program_environment program) (interface, initial)).
Proof.
  intros program interface initial Hblind Hsafe fuel. unfold M.runtime.
  destruct (@PS.prune_sound program (M.arguments initial) (M.read_file initial) fuel (interface, initial)
              (fun st => fst st = interface)) as [Hs|E].
  - intros q site ws [i s] Hi. simpl in Hi. subst i. unfold M.ReferenceHost.run_prim, Base.call, BaseCall.call, M.ReferenceHost.raw_primitive.
    destruct (prune_view program (M.arguments initial) (M.read_file initial))
      as [B [T [F [A [Rd C]]]]].
    rewrite (Hblind _ _ B T F A Rd C). reflexivity.
  - intros q site ws st Hi. unfold M.ReferenceHost.run_prim.
    eapply base_call_preserves; [|exact Hi].
    intros operation arguments [i s] Valid. unfold M.ReferenceHost.raw_primitive.
    destruct (M.supports i operation); [|exact Valid].
    destruct (M.primitive i _ operation arguments (M.external s)); exact Valid.
  - reflexivity.
  - exfalso. destruct Hs as [c Hc].
    apply (Hsafe c (snd (M.Reference.evaluation_state (M.Reference.run_main fuel
      (program_of program (M.arguments initial) (M.read_file initial))
      (program_main program) (program_environment program) (interface, initial))))).
    exists fuel. unfold M.runtime, M.observe. rewrite Hc. reflexivity.
  - rewrite E. reflexivity.
Qed.

Theorem runs_pruned : forall program interface initial result,
  code_blind interface ->
  (forall code final, ~ M.Runs program interface initial (M.Result (M.Wrong code) final)) ->
  M.Runs (prune program) interface initial result <-> M.Runs program interface initial result.
Proof.
  intros program interface initial result Hblind Hsafe. unfold M.Runs.
  split; intros [fuel E]; exists fuel;
    rewrite <- E; [symmetry|]; apply observe_pruned; assumption.
Qed.

End PruneRuns.
