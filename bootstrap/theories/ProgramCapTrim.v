(* ------------------------------------------------------------------------ *)
(* ProgramCapTrim.v — trimming the type captures of fn and method closures,  *)
(* for Audit's programs.                                                    *)
(*                                                                           *)
(* Audit's elaboration gives every lambda, method provider and closure a    *)
(* type capture over every generation its context mentions                  *)
(* (`lexical_capture`): thousands of entries for a large program.  Making   *)
(* the closure grounds each one in the enclosing environment               *)
(* (`typed_capture`, a linear lookup each), and every type the closure's    *)
(* code grounds is then looked up in that long list.  The code reads few   *)
(* of them.                                                                  *)
(*                                                                           *)
(* `captrim` rewrites each `BFunction`/`BMethod` binding (in fn bodies and  *)
(* in functor bodies) so that its capture keeps, of the first entry for    *)
(* each generation g, only those                                            *)
(*   - for a g the closure's code can ground (the read set G(f') of its    *)
(*     code f'), and                                                         *)
(*   - unless the entry is (g, TAbs g) and no activation of the enclosing  *)
(*     fn can give g a meaning (g is outside its domain D(f)): grounding it *)
(*     then gives TAbs g, which is what grounding an absent g gives;       *)
(* and drops the closure's source aliases (only functor closures' aliases  *)
(* are ever read, by output_meaning).                                       *)
(*                                                                           *)
(* G and D are computed by an untrusted iteration (`compute_T`); what is   *)
(* trusted is the checker `check T p`: every fn body grounds only types    *)
(* over G(f), installs result types only at keys in D(f), passes each      *)
(* callee g only keys in D(g), and every kept capture entry is grounded    *)
(* over G(f) and keyed in D(f'); main's initial environment is keyed in    *)
(* D(main), and so are the closures it holds.  If the check fails, the     *)
(* program is returned unchanged.  Functor bodies use "everything" for G  *)
(* and D (the trim there drops only unread entries).                       *)
(*                                                                           *)
(* Proved:                                                                   *)
(* - Sim.run_main_trimmed / captrim_sound: for every fuel, running the     *)
(*   trimmed program gives exactly the reference result (no stuck escape:  *)
(*   no lookup of a provision changes), for primitives that ignore the     *)
(*   program view's code.                                                    *)
(* - TrimRuns.runs_trimmed: the same Runs, for code-blind interfaces.      *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit PublicTypeFacts PublicValueTyping.
From NM Require NatTrie ProgramPrune.
Import Audit.Semantics ListNotations.

Set Implicit Arguments.
Unset Strict Implicit.
Local Open Scope nat_scope.

(* ===================== 1. Generation sets and tables ===================== *)

Definition gset := NatTrie.tmap unit.
Definition gmem (s : gset) (g : nat) : bool :=
  match NatTrie.tget s g with Some _ => true | None => false end.
Definition gadd (g : nat) (s : gset) : gset := if gmem s g then s else NatTrie.tset s g tt.

(* For each fn: (G, D), the generations its code grounds and those its
   activations can hold a meaning for. A fn absent from the table reads
   and holds everything. *)
Definition tbl := NatTrie.tmap (gset * gset).
Definition Gt (T : tbl) (f : fsym) (g : nat) : bool :=
  match NatTrie.tget T f with Some (a, _) => gmem a g | None => true end.
Definition Dt (T : tbl) (f : fsym) (g : nat) : bool :=
  match NatTrie.tget T f with Some (_, b) => gmem b g | None => true end.
Definition all_gens (_ : nat) : bool := true.

Definition is_id (g : nat) (t : ty) : bool :=
  match t with TAbs h => Nat.eqb g h | _ => false end.

(* The trimmed capture: the first entry of each read generation, kept
   unless it is an identity the enclosing activation cannot give a
   meaning (enc g = false). *)
Fixpoint trim_caps (rd enc : nat -> bool) (seen : list nat) (l : subst) : subst :=
  match l with
  | [] => []
  | (g, t) :: r =>
      if rd g && negb (existsb (Nat.eqb g) seen) then
        if negb (is_id g t) || enc g then (g, t) :: trim_caps rd enc (g :: seen) r
        else trim_caps rd enc (g :: seen) r
      else trim_caps rd enc seen r
  end.

Definition trim_tc (T : tbl) (enc : nat -> bool) (f' : fsym) (tc : type_capture) : type_capture :=
  TypeCapture (trim_caps (Gt T f') enc [] (capture_generations tc)) [].

Fixpoint trim_e (T : tbl) (enc : nat -> bool) (e : expr) : expr :=
  match e with
  | EVar x => EVar x
  | EUnit => EUnit
  | EVal v => EVal v
  | ETag n th e1 => ETag n th (trim_e T enc e1)
  | ECall f th sm args => ECall f th sm (map (trim_e T enc) args)
  | EInvoke f args => EInvoke f (map (trim_e T enc) args)
  | EMethod e0 m it args => EMethod (trim_e T enc e0) m it (map (trim_e T enc) args)
  | ELet x e1 e2 => ELet x (trim_e T enc e1) (trim_e T enc e2)
  | EBind b e1 => EBind (trim_b T enc b) (trim_e T enc e1)
  | ESeq e1 e2 => ESeq (trim_e T enc e1) (trim_e T enc e2)
  | EAssign x e1 => EAssign x (trim_e T enc e1)
  | EIf c e1 e2 => EIf (trim_e T enc c) (trim_e T enc e1) (trim_e T enc e2)
  | EWhile c b => EWhile (trim_e T enc c) (trim_e T enc b)
  | ELoop b => ELoop (trim_e T enc b)
  | EReturn e1 => EReturn (trim_e T enc e1)
  | EBreak => EBreak
  | ERecord fs => ERecord (map (fun p => (fst p, trim_e T enc (snd p))) fs)
  | EField e1 f => EField (trim_e T enc e1) f
  | EMatch e0 arms => EMatch (trim_e T enc e0) (map (fun p => (fst p, trim_e T enc (snd p))) arms)
  end
with trim_b (T : tbl) (enc : nat -> bool) (b : binding) : binding :=
  match b with
  | BValue v e1 => BValue v (trim_e T enc e1)
  | BFunction f f' types sm => BFunction f f' (trim_tc T enc f' types) sm
  | BMethod tk m th f' types sm => BMethod tk m th f' (trim_tc T enc f' types) sm
  | b => b
  end.

Definition trim_code (T : tbl) (c : functor_code) : functor_code :=
  FunctorCode (fc_inputs c) (fc_linked c) (fc_table c) (fc_outputs c)
    (map (trim_b T all_gens) (fc_bindings c)) (fc_sources c).

Definition trim_program (T : tbl) (p : program) : program :=
  Program (map (fun fb => (fst fb, trim_e T (Dt T (fst fb)) (snd fb))) (program_functions p))
          (map (fun ub => (fst ub, trim_code T (snd ub))) (program_functors p))
          (program_bool p) (program_true p) (program_false p) (program_main p)
          (program_environment p) (program_unsupported p) (program_casts p) (program_machines p).

(* ===================== 2. The checker ==================================== *)

Definition cast_gens (p : program) : list nat :=
  flat_map (fun q => match cast_handle (snd q) with Some (h, _) => [h] | None => [] end)
    (program_casts p).

Section Check.
Variable T : tbl.
Variable casts : list nat.
(* the enclosing activation's G and D *)
Variables S Dm : nat -> bool.
Definition gok (t : ty) : bool := forallb S (Elaboration.gens t).
Definition sok (th : subst) : bool := forallb (fun q => gok (snd q)) th.
Definition cap_ok (f' : fsym) (l : subst) : bool :=
  forallb (fun q => negb (Gt T f' (fst q)) ||
    (if is_id (fst q) (snd q) && negb (Dm (fst q)) then S (fst q)
     else gok (snd q) && Dt T f' (fst q))) l.

Fixpoint nchk (e : expr) : bool :=
  match e with
  | EVar _ | EUnit | EBreak | EVal _ => true
  | ETag _ th e1 => sok th && forallb S casts && nchk e1
  | EAssign _ e1 | ELoop e1 | EReturn e1 | EField e1 _ => nchk e1
  | ECall f th _ args => sok th && forallb (fun q => Dt T f (fst q)) th && forallb nchk args
  | EInvoke _ args => forallb nchk args
  | EMethod e0 _ _ args => nchk e0 && forallb nchk args
  | ELet _ a b | ESeq a b | EWhile a b => nchk a && nchk b
  | EBind b e1 => bchk b && nchk e1
  | EIf a b c => nchk a && nchk b && nchk c
  | ERecord fs => forallb (fun p => nchk (snd p)) fs
  | EMatch e0 arms => nchk e0 && forallb (fun p => nchk (snd p)) arms
  end
with bchk (b : binding) : bool :=
  match b with
  | BType _ tau => gok tau
  | BValue _ e1 => nchk e1
  | BFunction _ f' tc _ | BMethod _ _ _ f' tc _ => cap_ok f' (capture_generations tc)
  | BFunctor _ _ tc _ _ => sok (capture_generations tc) && sok (capture_aliases tc)
  | BCopyFunctor _ _ _ _ => true
  | BApply _ th outputs _ _ => sok th && forallb (fun q => Dm (snd q)) outputs
  | BPrimitive i _ => match i with IMth tau _ _ => gok tau | _ => true end
  end.
End Check.

(* Closures in main's initial environment hold meanings only in their
   fn's domain. *)
Fixpoint pv_ok (T : tbl) (a : provision_value) : bool :=
  match a with
  | FunctionClosure f d =>
      match d with EnvironmentWithAliases ts _ ps =>
        forallb (fun q => Dt T f (fst q)) ts && forallb (fun q => pv_ok T (snd q)) ps end
  | FunctorClosure _ _ d _ =>
      match d with EnvironmentWithAliases _ _ ps => forallb (fun q => pv_ok T (snd q)) ps end
  | _ => true
  end.

Definition check (T : tbl) (p : program) : bool :=
  let cs := cast_gens p in
  forallb (fun fb => nchk T cs (Gt T (fst fb)) (Dt T (fst fb)) (snd fb)) (program_functions p)
  && forallb (fun ub => forallb (bchk T cs all_gens all_gens) (fc_bindings (snd ub))) (program_functors p)
  && forallb (fun q => Dt T (program_main p) (fst q)) (type_meanings (program_environment p))
  && forallb (fun q => pv_ok T (snd q)) (provision_values (program_environment p)).

(* ===================== 3. Computing the tables (untrusted) =============== *)

Section Compute.
Variable T : tbl.   (* the previous round *)
Variable casts : list nat.

Definition tpair (A : tbl) (f : fsym) : gset * gset :=
  match NatTrie.tget A f with Some q => q | None => (NatTrie.TE, NatTrie.TE) end.
Definition addG (cur : option fsym) (gs : list nat) (A : tbl) : tbl :=
  match cur with
  | None => A
  | Some f => let '(a, b) := tpair A f in NatTrie.tset A f (fold_left (fun s g => gadd g s) gs a, b)
  end.
Definition addD (f : fsym) (gs : list nat) (A : tbl) : tbl :=
  let '(a, b) := tpair A f in NatTrie.tset A f (a, fold_left (fun s g => gadd g s) gs b).
Definition sgens (th : subst) : list nat := flat_map (fun q => Elaboration.gens (snd q)) th.

(* A closure site: the generations the enclosing fn must read, and the
   keys the closure's activations hold. *)
Definition site (cur : option fsym) (Dm : nat -> bool) (f' : fsym) (l : subst) (A : tbl) : tbl :=
  fold_left (fun A q =>
    if Gt T f' (fst q) then
      addD f' [fst q] (if is_id (fst q) (snd q) && negb (Dm (fst q)) then addG cur [fst q] A
                       else addG cur (Elaboration.gens (snd q)) A)
    else A) l A.

Fixpoint walk_e (cur : option fsym) (Dm : nat -> bool) (e : expr) (A : tbl) : tbl :=
  match e with
  | EVar _ | EUnit | EBreak | EVal _ => A
  | ETag _ th e1 => walk_e cur Dm e1 (addG cur (sgens th ++ casts) A)
  | EAssign _ e1 | ELoop e1 | EReturn e1 | EField e1 _ => walk_e cur Dm e1 A
  | ECall f th _ args =>
      fold_left (fun A a => walk_e cur Dm a A) args (addD f (map fst th) (addG cur (sgens th) A))
  | EInvoke _ args => fold_left (fun A a => walk_e cur Dm a A) args A
  | EMethod e0 _ _ args => fold_left (fun A a => walk_e cur Dm a A) args (walk_e cur Dm e0 A)
  | ELet _ a b | ESeq a b | EWhile a b => walk_e cur Dm b (walk_e cur Dm a A)
  | EBind b e1 => walk_e cur Dm e1 (walk_b cur Dm b A)
  | EIf a b c => walk_e cur Dm c (walk_e cur Dm b (walk_e cur Dm a A))
  | ERecord fs => fold_left (fun A p => walk_e cur Dm (snd p) A) fs A
  | EMatch e0 arms => fold_left (fun A p => walk_e cur Dm (snd p) A) arms (walk_e cur Dm e0 A)
  end
with walk_b (cur : option fsym) (Dm : nat -> bool) (b : binding) (A : tbl) : tbl :=
  match b with
  | BType _ tau => addG cur (Elaboration.gens tau) A
  | BValue _ e1 => walk_e cur Dm e1 A
  | BFunction _ f' tc _ | BMethod _ _ _ f' tc _ => site cur Dm f' (capture_generations tc) A
  | BFunctor _ _ tc _ _ => addG cur (sgens (capture_generations tc) ++ sgens (capture_aliases tc)) A
  | BCopyFunctor _ _ _ _ => A
  | BApply _ th outputs _ _ =>
      match cur with
      | Some f => addD f (map snd outputs) (addG cur (sgens th) A)
      | None => A
      end
  | BPrimitive i _ => match i with IMth tau _ _ => addG cur (Elaboration.gens tau) A | _ => A end
  end.

Fixpoint env_walk (fuel : nat) (owner : option fsym) (d : environment) (A : tbl) : tbl :=
  match fuel with
  | 0 => A
  | S k =>
      let A := match owner with Some f => addD f (map fst (type_meanings d)) A | None => A end in
      fold_left (fun A q => match snd q with
        | FunctionClosure f df => env_walk k (Some f) df A
        | FunctorClosure _ _ dc _ => env_walk k None dc A
        | _ => A end) (provision_values d) A
  end.
End Compute.

Definition round (p : program) (T : tbl) : tbl :=
  let cs := cast_gens p in
  let A := fold_left (fun A fb => walk_e T cs (Some (fst fb)) (Dt T (fst fb)) (snd fb) A)
             (program_functions p) T in
  let A := fold_left (fun A ub => fold_left (fun A b => walk_b T cs None all_gens b A)
             (fc_bindings (snd ub)) A) (program_functors p) A in
  env_walk 64 (Some (program_main p)) (program_environment p) A.

Definition init_tbl (p : program) : tbl :=
  fold_left (fun A fb => NatTrie.tset A (fst fb) (NatTrie.TE, NatTrie.TE)) (program_functions p) NatTrie.TE.

Fixpoint compute_T (fuel : nat) (p : program) (T : tbl) : option tbl :=
  if check T p then Some T
  else match fuel with
       | 0 => None
       | S k => compute_T k p (round p T)
       end.

(* The pass. *)
Definition captrim (p : program) : program :=
  match compute_T 64 p (init_tbl p) with
  | Some T => trim_program T p
  | None => p
  end.

(* ===================== 4. Meanings and agreement ========================= *)

(* What grounding reads for a generation: its meaning, or itself. *)
Definition mean (s : subst) (g : nat) : ty :=
  match lookup_subst g s with Some t => t | None => TAbs g end.

Lemma ground_agree : forall s1 s2 tau,
  (forall g, In g (Elaboration.gens tau) -> mean s1 g = mean s2 g) -> app_subst s1 tau = app_subst s2 tau.
Proof.
  fix IH 3. intros s1 s2 [m|fs| |t|n xs|ms] H; simpl in *.
  - reflexivity.
  - f_equal. induction fs as [|[f t] r IHr]; simpl in *; [reflexivity|].
    f_equal; [f_equal; apply IH; intros g Hg; apply H, in_or_app; left; exact Hg|].
    apply IHr. intros g Hg. apply H, in_or_app. right. exact Hg.
  - reflexivity.
  - exact (H t (or_introl eq_refl)).
  - f_equal. induction xs as [|[x t] r IHr]; simpl in *; [reflexivity|].
    f_equal; [f_equal; apply IH; intros g Hg; apply H, in_or_app; left; exact Hg|].
    apply IHr. intros g Hg. apply H, in_or_app. right. exact Hg.
  - f_equal. induction ms as [|t r IHr]; simpl in *; [reflexivity|].
    f_equal; [apply IH; intros g Hg; apply H, in_or_app; left; exact Hg|].
    apply IHr. intros g Hg. apply H, in_or_app. right. exact Hg.
Qed.

Lemma lookup_subst_map : forall (f : ty -> ty) g (l : subst),
  lookup_subst g (map (fun q => (fst q, f (snd q))) l) = option_map f (lookup_subst g l).
Proof.
  intros f g l. induction l as [|[h t] r IH]; simpl; auto. destruct (Nat.eqb g h); auto.
Qed.

Lemma lookup_subst_in : forall g (l : subst) t, lookup_subst g l = Some t -> In (g, t) l.
Proof.
  intros g l. induction l as [|[h u] r IH]; simpl; intros t H; [discriminate|].
  destruct (Nat.eqb g h) eqn:E; [apply Nat.eqb_eq in E; injection H as <-; subst; auto|auto].
Qed.

Lemma lookup_subst_app : forall g (a b : subst),
  lookup_subst g (a ++ b) = match lookup_subst g a with Some t => Some t | None => lookup_subst g b end.
Proof. intros g a b. induction a as [|[h t] r IH]; simpl; auto. destruct (Nat.eqb g h); auto. Qed.

Lemma lookup_subst_none : forall g (l : subst), (forall q, In q l -> fst q <> g) -> lookup_subst g l = None.
Proof.
  intros g l. induction l as [|[h t] r IH]; simpl; intros H; auto.
  destruct (Nat.eqb g h) eqn:E.
  - apply Nat.eqb_eq in E. subst. exfalso. exact (H (h, t) (or_introl eq_refl) eq_refl).
  - apply IH. intros q Hq. apply H. right. exact Hq.
Qed.

Lemma trim_caps_lookup : forall rd enc l seen g, rd g = true ->
  lookup_subst g (trim_caps rd enc seen l) =
  if existsb (Nat.eqb g) seen then None
  else match lookup_subst g l with
       | Some t => if negb (is_id g t) || enc g then Some t else None
       | None => None
       end.
Proof.
  intros rd enc l. induction l as [|[h t] r IH]; intros seen g Hg; simpl.
  - destruct (existsb (Nat.eqb g) seen); reflexivity.
  - destruct (Nat.eqb g h) eqn:E.
    + apply Nat.eqb_eq in E. subst h. rewrite Hg. simpl.
      destruct (existsb (Nat.eqb g) seen) eqn:Es; simpl.
      * rewrite IH by exact Hg. rewrite Es. reflexivity.
      * destruct (negb (is_id g t) || enc g); simpl.
        -- rewrite Nat.eqb_refl. reflexivity.
        -- rewrite IH by exact Hg. simpl. rewrite Nat.eqb_refl. reflexivity.
    + destruct (rd h && negb (existsb (Nat.eqb h) seen)).
      * destruct (negb (is_id h t) || enc h); simpl; [rewrite E|];
          rewrite IH by exact Hg; simpl; rewrite E; reflexivity.
      * apply IH. exact Hg.
Qed.

Lemma trim_caps_in : forall rd enc l seen g t, In (g, t) (trim_caps rd enc seen l) ->
  In (g, t) l /\ rd g = true /\ (negb (is_id g t) || enc g) = true.
Proof.
  intros rd enc l. induction l as [|[h u] r IH]; intros seen g t H; simpl in H; [contradiction|].
  destruct (rd h && negb (existsb (Nat.eqb h) seen)) eqn:E1.
  - apply andb_true_iff in E1 as [E1 _].
    destruct (negb (is_id h u) || enc h) eqn:E2.
    + destruct H as [H|H].
      * injection H as <- <-. simpl. auto.
      * destruct (IH _ _ _ H) as [A B]. simpl. auto.
    + destruct (IH _ _ _ H) as [A B]. simpl. auto.
  - destruct (IH _ _ _ H) as [A B]. simpl. auto.
Qed.

Section Agree.
Variable T : tbl.

Definition tagree (S : nat -> bool) (a b : subst) : Prop :=
  forall g, S g = true -> mean a g = mean b g.
Definition tdom (Dm : nat -> bool) (b : subst) : Prop :=
  forall g, Dm g = false -> mean b g = TAbs g.

(* Entries agree up to their captured environments: a fn closure's needs
   to agree on its fn's G and hold meanings only in its D (trimmed side);
   a functor closure's is equal in its types. *)
Inductive prel : provision_value -> provision_value -> Prop :=
| PVal : forall w, prel (ValueProvision w) (ValueProvision w)
| PClo : forall f d d', lrel2 (provision_values d) (provision_values d') ->
    tagree (Gt T f) (type_meanings d) (type_meanings d') -> tdom (Dt T f) (type_meanings d') ->
    prel (FunctionClosure f d) (FunctionClosure f d')
| PFun : forall h sms d d' rms, lrel2 (provision_values d) (provision_values d') ->
    type_meanings d = type_meanings d' -> type_aliases d = type_aliases d' ->
    prel (FunctorClosure h sms d rms) (FunctorClosure h sms d' rms)
| PPrim : forall p ot, prel (PrimitiveClosure p ot) (PrimitiveClosure p ot)
with lrel2 : list (item * provision_value) -> list (item * provision_value) -> Prop :=
| L2Nil : lrel2 [] []
| L2Cons : forall i a a' l l', prel a a' -> lrel2 l l' -> lrel2 ((i, a) :: l) ((i, a') :: l').

(* Activation environments: pointwise provisions; the types agree on S;
   the trimmed side holds meanings only in Dm; with x, the types are equal
   (functor bodies, which read aliases and meanings directly). *)
Definition envrel (S Dm : nat -> bool) (x : bool) (d d' : environment) : Prop :=
  lrel2 (provision_values d) (provision_values d')
  /\ tagree S (type_meanings d) (type_meanings d')
  /\ tdom Dm (type_meanings d')
  /\ (x = true -> type_meanings d = type_meanings d' /\ type_aliases d = type_aliases d').

(* The relation for result environments and selections: equal types. *)
Definition xrel (d d' : environment) : Prop :=
  lrel2 (provision_values d) (provision_values d')
  /\ type_meanings d = type_meanings d' /\ type_aliases d = type_aliases d'.

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

Lemma lrel2_app : forall a a' b b', lrel2 a a' -> lrel2 b b' -> lrel2 (a ++ b) (a' ++ b').
Proof. intros a a' b b' H Hb. induction H; simpl; auto. constructor; auto. Qed.

Lemma envrel_lookup : forall S Dm x d d' i a, envrel S Dm x d d' ->
  lookup_provision i d = Some a -> exists a', lookup_provision i d' = Some a' /\ prel a a'.
Proof.
  intros S Dm x d d' i a [H _] Hl. pose proof (lrel2_lookup i H) as Q. unfold lookup_provision in *.
  rewrite Hl in Q. destruct (lookup_entry i (provision_values d')); [eauto|contradiction].
Qed.

Lemma envrel_ground : forall S Dm x d d' tau, envrel S Dm x d d' -> gok S tau = true ->
  ground d tau = ground d' tau.
Proof.
  intros S Dm x d d' tau [_ [Ha _]] Hg. unfold ground. apply ground_agree.
  intros g Hin. apply Ha. unfold gok in Hg. rewrite forallb_forall in Hg. exact (Hg g Hin).
Qed.

Lemma envrel_ground_subst : forall S Dm x d d' th, envrel S Dm x d d' -> sok S th = true ->
  map (fun q => (fst q, ground d (snd q))) th = map (fun q => (fst q, ground d' (snd q))) th.
Proof.
  intros S Dm x d d' th H Hs. induction th as [|[t tau] r IH]; simpl in *; auto.
  apply andb_true_iff in Hs as [H1 H2]. rewrite (envrel_ground H H1), IH by exact H2. reflexivity.
Qed.

Lemma envrel_install : forall S Dm x d d' i a a', prel a a' -> envrel S Dm x d d' ->
  envrel S Dm x (install_provision i a d) (install_provision i a' d').
Proof.
  intros S Dm x d d' i a a' Ha [H1 [H2 [H3 H4]]]. split; [|split; [|split]]; simpl; auto.
  constructor; auto.
Qed.

Lemma envrel_bind_type : forall S Dm x d d' t tau, envrel S Dm x d d' -> gok S tau = true ->
  envrel S Dm x (bind_type d t tau) (bind_type d' t tau).
Proof.
  intros S Dm x d d' t tau H Hg. pose proof H as [H1 [H2 [H3 H4]]].
  unfold bind_type. split; [|split; [|split]]; simpl; auto.
  intro Hx. destruct (H4 Hx) as [E1 E2]. rewrite E1, E2, (envrel_ground H Hg). split; reflexivity.
Qed.

Lemma xrel_empty : xrel empty_environment empty_environment.
Proof. repeat split; constructor. Qed.

Lemma xrel_select : forall S Dm x d d' sm r, envrel S Dm x d d' ->
  select_provisions d sm = Some r -> exists r', select_provisions d' sm = Some r' /\ xrel r r'.
Proof.
  intros S Dm x d d' sm. induction sm as [|[req s] rest IH]; simpl; intros r H Hc.
  - injection Hc as <-. eauto using xrel_empty.
  - destruct (lookup_provision s d) as [a|] eqn:El; [|discriminate].
    destruct (select_provisions d rest) as [dr|] eqn:Er; [|discriminate]. injection Hc as <-.
    destruct (envrel_lookup H El) as [a' [-> Ha]].
    destruct (IH dr H eq_refl) as [dr' [-> [P1 [P2 P3]]]].
    eexists; split; [reflexivity|]. repeat split; simpl; auto. constructor; auto.
Qed.

Lemma xrel_envrel : forall d d', xrel d d' -> forall S, envrel S all_gens true d d'.
Proof.
  intros d d' [H1 [H2 H3]] S. split; [|split; [|split]]; auto.
  - intros g _. rewrite H2. reflexivity.
  - intros g Hg. discriminate.
Qed.

Lemma envrel_xrel : forall S Dm d d', envrel S Dm true d d' -> xrel d d'.
Proof. intros S Dm d d' [H1 [_ [_ H4]]]. destruct (H4 eq_refl). repeat split; auto. Qed.

Lemma envrel_weaken : forall S Dm d d', envrel S Dm true d d' -> envrel all_gens all_gens true d d'.
Proof. intros S Dm d d' H. apply xrel_envrel. eapply envrel_xrel; eauto. Qed.

Lemma xrel_adapt : forall sms d d' r, xrel d d' ->
  adapt_provisions d sms = Some r -> exists r', adapt_provisions d' sms = Some r' /\ xrel r r'.
Proof.
  intros [sm|] d d' r H Hc; simpl in *.
  - exact (xrel_select (xrel_envrel H all_gens) Hc).
  - injection Hc as <-. eauto.
Qed.

Lemma xrel_append : forall a a' b b', xrel a a' -> xrel b b' ->
  xrel (append_environment a b) (append_environment a' b').
Proof.
  intros a a' b b' [A1 [A2 A3]] [B1 [B2 B3]]. repeat split; simpl.
  - apply lrel2_app; assumption.
  - rewrite A2, B2. reflexivity.
  - rewrite A3, B3. reflexivity.
Qed.

(* The callee's activation (call_environment): agree on everything θ
   defines; θ's keys are in the callee's D. *)
Lemma envrel_call : forall S Dm x d d' th dc dc' g, envrel S Dm x d d' -> xrel dc dc' ->
  sok S th = true -> forallb (fun q => Dt T g (fst q)) th = true ->
  type_meanings dc' = [] ->
  envrel (Gt T g) (Dt T g) false (call_environment d th dc) (call_environment d' th dc').
Proof.
  intros S Dm x d d' th dc dc' g H [C1 [C2 C3]] Hs Hd Hdc.
  unfold call_environment, append_environment, instantiate. simpl.
  rewrite (envrel_ground_subst H Hs). rewrite C2. split; [|split; [|split]]; simpl; auto; [| |discriminate].
  - intros h _. reflexivity.
  - intros h Hh. rewrite Hdc, app_nil_r. unfold mean. rewrite lookup_subst_none; [reflexivity|].
    intros [t tau] Hin E. simpl in E. subst t.
    apply in_map_iff in Hin as [[t' tau'] [Eq Hin]]. simpl in Eq. injection Eq as -> _.
    rewrite forallb_forall in Hd. specialize (Hd _ Hin). simpl in Hd. congruence.
Qed.

Lemma select_empty_types : forall d sm r, select_provisions d sm = Some r -> type_meanings r = [].
Proof.
  intros d sm. induction sm as [|[req s] rest IH]; simpl; intros r Hc.
  - injection Hc as <-. reflexivity.
  - destruct (lookup_provision s d); [|discriminate].
    destruct (select_provisions d rest) as [dr|] eqn:Er; [|discriminate]. injection Hc as <-.
    simpl. exact (IH dr eq_refl).
Qed.

(* A closure made with a trimmed capture. *)
Lemma envrel_capture : forall S Dm x d d' f' l dc dc' als als',
  envrel S Dm x d d' -> xrel dc dc' -> cap_ok T S Dm f' l = true ->
  let E := typed_capture d (TypeCapture l als) dc in
  let E' := typed_capture d' (TypeCapture (trim_caps (Gt T f') Dm [] l) als') dc' in
  prel (FunctionClosure f' E) (FunctionClosure f' E').
Proof.
  intros S Dm x d d' f' l dc dc' als als' H [C1 _] Hc E E'.
  pose proof H as [_ [Ha [Hd _]]].
  unfold cap_ok in Hc. rewrite forallb_forall in Hc.
  constructor; unfold E, E', typed_capture; simpl; [exact C1| |].
  - intros g Hg. unfold mean. rewrite !lookup_subst_map, trim_caps_lookup by exact Hg. simpl.
    destruct (lookup_subst g l) as [t|] eqn:El; simpl; [|reflexivity].
    specialize (Hc _ (lookup_subst_in El)). simpl in Hc. rewrite Hg in Hc. simpl in Hc.
    destruct (is_id g t) eqn:Ei; destruct (Dm g) eqn:Ed; simpl in *.
    + apply andb_true_iff in Hc as [Hc _]. exact (envrel_ground H Hc).
    + (* a dropped identity: both read TAbs g *)
      destruct t as [| | |h| |]; try discriminate. simpl in Ei. apply Nat.eqb_eq in Ei. subst h.
      unfold ground. simpl. fold (mean (type_meanings d) g). rewrite (Ha g Hc). exact (Hd g Ed).
    + apply andb_true_iff in Hc as [Hc _]. exact (envrel_ground H Hc).
    + apply andb_true_iff in Hc as [Hc _]. exact (envrel_ground H Hc).
  - intros g Hg. unfold mean.
    destruct (lookup_subst g (map (fun q => (fst q, ground d' (snd q))) (trim_caps (Gt T f') Dm [] l)))
      as [t|] eqn:El; [|reflexivity].
    exfalso. apply lookup_subst_in in El. apply in_map_iff in El as [[g' t'] [Eq Hin]].
    simpl in Eq. injection Eq as -> _.
    destruct (trim_caps_in Hin) as [Hin' [Hr Hk]].
    specialize (Hc _ Hin'). simpl in Hc. rewrite Hr in Hc. simpl in Hc.
    destruct (is_id g t') eqn:Ei; destruct (Dm g) eqn:Ed; simpl in *; try discriminate;
      apply andb_true_iff in Hc as [_ Hc]; congruence.
Qed.

Lemma output_meaning_eq : forall d d', type_meanings d = type_meanings d' ->
  type_aliases d = type_aliases d' -> forall s, output_meaning d s = output_meaning d' s.
Proof. intros d d' Tm Ta s. unfold output_meaning. rewrite Tm, Ta. reflexivity. Qed.

Lemma read_result_types_eq : forall d d', type_meanings d = type_meanings d' ->
  type_aliases d = type_aliases d' -> forall Dr outputs,
  read_result_types d Dr outputs = read_result_types d' Dr outputs.
Proof.
  intros d d' Tm Ta Dr. induction outputs as [|[s t] r IH]; simpl; auto.
  rewrite (output_meaning_eq Tm Ta s), IH. reflexivity.
Qed.

Lemma read_result_types_keys : forall d Dr outputs w a,
  read_result_types d Dr outputs = Some (w, a) -> forall q, In q w -> In (fst q) (map snd outputs).
Proof.
  intros d Dr. induction outputs as [|[s t] r IH]; simpl; intros w a H q Hq.
  - injection H as <- <-. contradiction.
  - destruct (existsb (item_eqb (ITy s)) Dr); [|discriminate].
    destruct (output_meaning d s); [|discriminate].
    destruct (read_result_types d Dr r) as [[w' a']|] eqn:E; [|discriminate].
    injection H as <- <-. destruct Hq as [<-|Hq]; [left; reflexivity|right; exact (IH _ _ eq_refl q Hq)].
Qed.

Lemma result_env_rel : forall outputs d3 d3' Dr dr dr', xrel d3 d3' -> xrel dr dr' ->
  match result_environment outputs d3 Dr dr, result_environment outputs d3' Dr dr' with
  | Some a, Some a' => xrel a a' /\ (forall q, In q (type_meanings a') -> In (fst q) (map snd outputs))
  | None, None => True
  | _, _ => False
  end.
Proof.
  intros outputs d3 d3' Dr dr dr' [X1 [X2 X3]] [R1 _]. unfold result_environment.
  rewrite <- (read_result_types_eq X2 X3).
  destruct (read_result_types d3 Dr outputs) as [[w a]|] eqn:Ew; [|exact I].
  split; [repeat split; simpl; auto|]. simpl. exact (read_result_types_keys Ew).
Qed.

(* Installing an application's result in the caller. *)
Lemma envrel_append_keys : forall S Dm x d d' (outputs : list (tsym * tsym)) a a',
  envrel S Dm x d d' -> xrel a a' ->
  (forall q, In q (type_meanings a') -> In (fst q) (map snd outputs)) ->
  forallb (fun q => Dm (snd q)) outputs = true ->
  envrel S Dm x (append_environment a d) (append_environment a' d').
Proof.
  intros S Dm x d d' outputs a a' H [A1 [A2 A3]] Hk Ho.
  destruct H as [H1 [H2 [H3 H4]]]. unfold append_environment. split; [|split; [|split]]; simpl.
  - apply lrel2_app; assumption.
  - intros g Hg. unfold mean. rewrite !lookup_subst_app, A2.
    destruct (lookup_subst g (type_meanings a')); [reflexivity|]. exact (H2 g Hg).
  - intros g Hg. unfold mean. rewrite lookup_subst_app.
    rewrite lookup_subst_none; [exact (H3 g Hg)|].
    intros q Hq E. subst g. pose proof (Hk q Hq) as K.
    rewrite forallb_forall in Ho. apply in_map_iff in K as [o [Eo Ko]].
    specialize (Ho o Ko). destruct o as [o1 o2]; simpl in *. subst o2. congruence.
  - intro Hx. destruct (H4 Hx) as [E1 E2]. rewrite A2, A3, E1, E2. split; reflexivity.
Qed.

Lemma lookup_none : forall S Dm x d d' i, envrel S Dm x d d' ->
  lookup_provision i d = None -> lookup_provision i d' = None.
Proof.
  intros S Dm x d d' i [H1 _] El. pose proof (lrel2_lookup i H1) as Q.
  unfold lookup_provision in *. rewrite El in Q.
  destruct (lookup_entry i (provision_values d')); [contradiction|reflexivity].
Qed.

Lemma select_none : forall S Dm x d d' sm, envrel S Dm x d d' ->
  select_provisions d sm = None -> select_provisions d' sm = None.
Proof.
  intros S Dm x d d' sm H. induction sm as [|[req s] rest IH]; simpl; intros Ec; [discriminate|].
  destruct (lookup_provision s d) as [a|] eqn:El.
  - destruct (envrel_lookup H El) as [a' [-> _]].
    destruct (select_provisions d rest); [discriminate|]. rewrite IH by reflexivity. reflexivity.
  - rewrite (lookup_none H El). reflexivity.
Qed.

Lemma adapt_none : forall sms d d', xrel d d' ->
  adapt_provisions d sms = None -> adapt_provisions d' sms = None.
Proof.
  intros [sm|] d d' H Hc; simpl in *; [|discriminate].
  exact (select_none (xrel_envrel H all_gens) Hc).
Qed.

End Agree.

(* ===================== 5. Simulation ===================================== *)

Module Sim (Prims : PRIMITIVES) (I : INTERP Prims).
Import Prims I.
Module PS := ProgramPrune.Sim Prims I.

Ltac andb_split :=
  repeat match goal with H : (_ && _) = true |- _ => apply andb_true_iff in H as [? ?] end.

Section SimSec.
Variable T : tbl.
Variable casts : list nat.
Variables P P' : program_view.
Variable ok : mstate -> Prop.
Hypothesis Hfn : forall f, view_function P' f = option_map (trim_e T (Dt T f)) (view_function P f).
Hypothesis Hfun : forall h, view_functor P' h = option_map (trim_code T) (view_functor P h).
Hypothesis Hprim : forall q ws st, ok st -> run_prim P' q ws st = run_prim P q ws st.
Hypothesis Hok : forall q ws st, ok st -> ok (snd (run_prim P q ws st)).
Hypothesis Hbool : view_bool P' = view_bool P.
Hypothesis Htrue : view_true P' = view_true P.
Hypothesis Hfalse : view_false P' = view_false P.
Hypothesis Hcasts : view_casts P' = view_casts P.
Hypothesis Hcastg : forall n c h t, lookup_cast n (view_casts P) = Some c ->
  cast_handle c = Some (h, t) -> In h casts.
Hypothesis Hclosed : forall f b, view_function P f = Some b ->
  nchk T casts (Gt T f) (Dt T f) b = true.
Hypothesis Hfunchk : forall h c, view_functor P h = Some c ->
  forallb (bchk T casts all_gens all_gens) (fc_bindings c) = true.

Definition brel (S Dm : nat -> bool) (x : bool) (a b : (environment * locals * mstate) + evaluation) : Prop :=
  match a, b with
  | inl (d', g', st'), inl (d, g, st) => envrel T S Dm x d d' /\ g' = g /\ st' = st
  | inr r', inr r => r' = r
  | _, _ => False
  end.

Definition arel (outputs : list (tsym * tsym)) (a b : (environment * mstate) + evaluation) : Prop :=
  match a, b with
  | inl (d', st'), inl (d, st) => xrel T d d'
      /\ (forall q, In q (type_meanings d') -> In (fst q) (map snd outputs)) /\ st' = st
  | inr r', inr r => r' = r
  | _, _ => False
  end.

Definition SIM (k : nat) : Prop :=
  (forall S Dm x d d' g st e, nchk T casts S Dm e = true -> envrel T S Dm x d d' -> ok st ->
     eval k P' d' g st (trim_e T Dm e) = eval k P d g st e)
  /\ (forall S Dm x d d' g st l, forallb (nchk T casts S Dm) l = true -> envrel T S Dm x d d' -> ok st ->
     eval_arguments k P' d' g st (map (trim_e T Dm) l) = eval_arguments k P d g st l)
  /\ (forall S Dm x d d' g st bs, forallb (bchk T casts S Dm) bs = true -> envrel T S Dm x d d' -> ok st ->
     brel S Dm x (eval_bindings k P' d' g st (map (trim_b T Dm) bs)) (eval_bindings k P d g st bs))
  /\ (forall S Dm x d d' u th outputs sm rs st, sok S th = true -> envrel T S Dm x d d' -> ok st ->
     arel outputs (apply_functor k P' d' u th outputs sm rs st) (apply_functor k P d u th outputs sm rs st)).

Lemma inv_e : forall k d g st e, ok st -> ok (evaluation_state (eval k P d g st e)).
Proof. intros k. exact (proj1 (@PS.inv_all P ok Hok k)). Qed.
Lemma inv_l : forall k d g st l, ok st -> PS.okl (ok := ok) (eval_arguments k P d g st l).
Proof. intros k. exact (proj1 (proj2 (@PS.inv_all P ok Hok k))). Qed.
Lemma inv_b : forall k d g st bs, ok st -> PS.okl (ok := ok) (eval_bindings k P d g st bs).
Proof. intros k. exact (proj1 (proj2 (proj2 (@PS.inv_all P ok Hok k)))). Qed.
Lemma inv_a : forall k d u th outputs sm rs st, ok st ->
  PS.oka (ok := ok) (apply_functor k P d u th outputs sm rs st).
Proof. intros k. exact (proj2 (proj2 (proj2 (@PS.inv_all P ok Hok k)))). Qed.

Lemma select_arm_trim : forall Dm arms w,
  select_arm (map (fun p => (fst p, trim_e T Dm (snd p))) arms) w
  = option_map (fun q => (fst q, trim_e T Dm (snd q))) (select_arm arms w).
Proof.
  intros Dm. induction arms as [|[pt eb] r IH]; intros w; simpl; auto.
  destruct (match_pat pt w); simpl; auto.
Qed.
Lemma select_arm_chk : forall S Dm arms w bs eb,
  forallb (fun p => nchk T casts S Dm (snd p)) arms = true -> select_arm arms w = Some (bs, eb) ->
  nchk T casts S Dm eb = true.
Proof.
  intros S Dm. induction arms as [|[pt b] r IH]; intros w bs eb Hc Ha; simpl in *; [discriminate|].
  apply andb_true_iff in Hc as [Hb Hr].
  destruct (match_pat pt w); [injection Ha as <- <-; exact Hb|eauto|discriminate].
Qed.
Lemma map_snd_trim : forall Dm (fs : list (fld * expr)),
  map snd (map (fun p => (fst p, trim_e T Dm (snd p))) fs) = map (trim_e T Dm) (map snd fs).
Proof. intros. rewrite !map_map. reflexivity. Qed.
Lemma map_fst_trim : forall Dm (fs : list (fld * expr)),
  map fst (map (fun p => (fst p, trim_e T Dm (snd p))) fs) = map fst fs.
Proof. intros. rewrite map_map. reflexivity. Qed.
Lemma forallb_snd : forall S Dm (fs : list (fld * expr)),
  forallb (fun p => nchk T casts S Dm (snd p)) fs = forallb (nchk T casts S Dm) (map snd fs).
Proof. intros. induction fs; simpl; congruence. Qed.

Lemma truthy_eq : forall w, truthy P' w = truthy P w.
Proof. intros. unfold truthy. rewrite Hbool, Htrue, Hfalse. reflexivity. Qed.

Lemma check_tag_eq : forall S Dm x d d' n th w, envrel T S Dm x d d' ->
  sok S th = true -> forallb S casts = true ->
  check_tag P' d' n th w = check_tag P d n th w.
Proof.
  intros S Dm x d d' n th w H Hs Hc. unfold check_tag. rewrite Hcasts.
  destruct (lookup_cast n (view_casts P)) as [spec|] eqn:Ecast; [|reflexivity].
  assert (Hth : forall h t, lookup_subst h th = Some t -> ground d t = ground d' t).
  { intros h t Hl. apply (envrel_ground H). apply lookup_subst_in in Hl.
    unfold sok in Hs. rewrite forallb_forall in Hs. exact (Hs _ Hl). }
  assert (E1 : (match cast_handle spec with
                | None => true
                | Some (h, expected) => ty_eqb (ground d' (match lookup_subst h th with Some t => t | None => TAbs h end)) expected
                end) =
               (match cast_handle spec with
                | None => true
                | Some (h, expected) => ty_eqb (ground d (match lookup_subst h th with Some t => t | None => TAbs h end)) expected
                end)).
  { destruct (cast_handle spec) as [[h expected]|] eqn:Eh; [|reflexivity].
    destruct (lookup_subst h th) as [t|] eqn:El; [rewrite (Hth h t El); reflexivity|].
    rewrite (envrel_ground H); [reflexivity|]. unfold gok. simpl. rewrite andb_true_r.
    rewrite forallb_forall in Hc. apply Hc. exact (Hcastg Ecast Eh). }
  rewrite E1. destruct (lookup_subst (cast_held spec) th) as [tau|] eqn:El;
    [rewrite (Hth _ _ El)|]; reflexivity.
Qed.

Lemma prim_at_eq : forall p ot ws st, ok st -> run_prim_at P' p ot ws st = run_prim_at P p ot ws st.
Proof. intros. unfold run_prim_at. rewrite Hprim by assumption. reflexivity. Qed.

Lemma prim_closure_eq : forall S Dm x d d' i p, envrel T S Dm x d d' ->
  match i with IMth tau _ _ => gok S tau | _ => true end = true ->
  primitive_closure d i p = primitive_closure d' i p.
Proof.
  intros S Dm x d d' i p H Hi. unfold primitive_closure.
  destruct p; try reflexivity; destruct i; try reflexivity; rewrite (envrel_ground H Hi); reflexivity.
Qed.

Lemma app_env_rel : forall S Dm x d d' h th dc dc', envrel T S Dm x d d' -> sok S th = true ->
  xrel T dc dc' ->
  xrel T (application_environment P d h th dc) (application_environment P' d' h th dc').
Proof.
  intros S Dm x d d' h th dc dc' H Hs [C1 [C2 C3]]. unfold application_environment. rewrite Hfun.
  assert (Hb : forall source,
    match (if existsb (Nat.eqb source) (match option_map (trim_code T) (view_functor P h) with Some code => fc_linked code | None => [] end)
           then lookup_subst source th else None) with
    | Some meaning => ground d' meaning
    | None => match output_meaning dc' source with Some meaning => meaning | None => TAbs source end
    end =
    match (if existsb (Nat.eqb source) (match view_functor P h with Some code => fc_linked code | None => [] end)
           then lookup_subst source th else None) with
    | Some meaning => ground d meaning
    | None => match output_meaning dc source with Some meaning => meaning | None => TAbs source end
    end).
  { intro source. destruct (view_functor P h) as [code|]; simpl.
    - destruct (existsb (Nat.eqb source) (fc_linked code)).
      + destruct (lookup_subst source th) as [t|] eqn:El.
        * symmetry. apply (envrel_ground H). apply lookup_subst_in in El.
          unfold sok in Hs. rewrite forallb_forall in Hs. exact (Hs _ El).
        * rewrite (output_meaning_eq C2 C3). reflexivity.
      + rewrite (output_meaning_eq C2 C3). reflexivity.
    - rewrite (output_meaning_eq C2 C3). reflexivity. }
  destruct (view_functor P h) as [code|] eqn:Ev; simpl in *.
  - split; [exact C1|]. simpl.
    split; apply map_ext; intro source; f_equal;
      (destruct (lookup_subst source (fc_table code)); [f_equal; apply map_ext; intro s0; f_equal|]);
      symmetry; apply Hb.
  - split; [exact C1|]. simpl. split; reflexivity.
Qed.


Lemma sim_step : forall k, SIM k -> SIM (S k).
Proof.
  intros k [IHe [IHl [IHb IHa]]].
  split; [|split; [|split]].
  - intros S Dm x d d' g st e Hc H Hst.
    destruct e as [x0| |v|n th e1|f th sm args|f args|e0 m it args|x0 e1 e2|bd e1|e1 e2|x0 e1
                  |c e1 e2|c e1|e1|e1| |fs|e1 fl|e0 arms];
      simpl in Hc |- *; andb_split.
    + (* EVar *) reflexivity.
    + (* EUnit *) reflexivity.
    + (* EVal *)
      destruct (lookup_provision (IVal v) d) as [a|] eqn:El.
      * destruct (envrel_lookup H El) as [a' [-> Ha]]. inversion Ha; subst; reflexivity.
      * rewrite (lookup_none H El). reflexivity.
    + (* ETag *)
      rewrite (IHe S Dm x d d' g st e1 ltac:(assumption) H Hst).
      destruct (eval k P d g st e1) as [[w| | | | | | |] g1 st1]; try reflexivity.
      rewrite (@check_tag_eq S Dm x d d' n th w H ltac:(assumption) ltac:(assumption)). reflexivity.
    + (* ECall *)
      rewrite Hfn. destruct (view_function P f) as [body|] eqn:Ef; simpl; [|reflexivity].
      rewrite (IHl S Dm x d d' g st args ltac:(assumption) H Hst).
      pose proof (@inv_l k d g st args Hst) as Hn.
      destruct (eval_arguments k P d g st args) as [[[ws g1] st1]|]; simpl in Hn; [|reflexivity].
      destruct (select_provisions d sm) as [dc|] eqn:Ec.
      * destruct (xrel_select H Ec) as [dc' [-> Hdc]].
        rewrite (IHe _ _ false (call_environment d th dc) (call_environment d' th dc') (parameter_values ws)
                  st1 body (Hclosed Ef)
                  (@envrel_call T S Dm x d d' th dc dc' f H Hdc H0 H2 (eq_trans (eq_sym (proj1 (proj2 Hdc))) (select_empty_types Ec)))
                  Hn).
        reflexivity.
      * rewrite (select_none H Ec). reflexivity.
    + (* EInvoke *)
      destruct (lookup_provision (IFn f) d) as [a|] eqn:El.
      * destruct (envrel_lookup H El) as [a' [-> Ha]].
        inversion Ha as [|f' df df' Hp Hag Hdm| |]; subst; try reflexivity.
        -- rewrite Hfn. destruct (view_function P f') as [body|] eqn:Ef; simpl; [|reflexivity].
           rewrite (IHl S Dm x d d' g st args ltac:(assumption) H Hst).
           pose proof (@inv_l k d g st args Hst) as Hn.
           destruct (eval_arguments k P d g st args) as [[[ws g1] st1]|]; simpl in Hn; [|reflexivity].
           rewrite (IHe _ _ false df df' (parameter_values ws) st1 body (Hclosed Ef)
                      (conj Hp (conj Hag (conj Hdm (fun E => False_ind _ (diff_false_true E))))) Hn).
           reflexivity.
        -- rewrite (IHl S Dm x d d' g st args ltac:(assumption) H Hst).
           pose proof (@inv_l k d g st args Hst) as Hn.
           destruct (eval_arguments k P d g st args) as [[[ws g1] st1]|]; simpl in Hn; [|reflexivity].
           rewrite prim_at_eq by exact Hn. reflexivity.
      * rewrite (lookup_none H El). reflexivity.
    + (* EMethod *)
      destruct (lookup_provision it d) as [a|] eqn:El.
      * destruct (envrel_lookup H El) as [a' [-> Ha]].
        inversion Ha as [|f' df df' Hp Hag Hdm| |]; subst; try reflexivity.
        -- rewrite Hfn. destruct (view_function P f') as [body|] eqn:Ef; simpl; [|reflexivity].
           rewrite (IHe S Dm x d d' g st e0 ltac:(assumption) H Hst).
           pose proof (@inv_e k d g st e0 Hst) as Hn0.
           destruct (eval k P d g st e0) as [[w0| | | | | | |] g0 st0]; simpl in Hn0; try reflexivity.
           rewrite (IHl S Dm x d d' g0 st0 args ltac:(assumption) H Hn0).
           pose proof (@inv_l k d g0 st0 args Hn0) as Hn.
           destruct (eval_arguments k P d g0 st0 args) as [[[ws g1] st1]|]; simpl in Hn; [|reflexivity].
           rewrite (IHe _ _ false df df' (parameter_values (w0 :: ws)) st1 body (Hclosed Ef)
                      (conj Hp (conj Hag (conj Hdm (fun E => False_ind _ (diff_false_true E))))) Hn).
           reflexivity.
        -- rewrite (IHe S Dm x d d' g st e0 ltac:(assumption) H Hst).
           pose proof (@inv_e k d g st e0 Hst) as Hn0.
           destruct (eval k P d g st e0) as [[w0| | | | | | |] g0 st0]; simpl in Hn0; try reflexivity.
           rewrite (IHl S Dm x d d' g0 st0 args ltac:(assumption) H Hn0).
           pose proof (@inv_l k d g0 st0 args Hn0) as Hn.
           destruct (eval_arguments k P d g0 st0 args) as [[[ws g1] st1]|]; simpl in Hn; [|reflexivity].
           rewrite prim_at_eq by exact Hn. reflexivity.
      * rewrite (lookup_none H El). reflexivity.
    + (* ELet *)
      rewrite (IHe S Dm x d d' g st e1 ltac:(assumption) H Hst).
      pose proof (@inv_e k d g st e1 Hst) as Hn.
      destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try reflexivity.
      rewrite (IHe S Dm x d d' ((x0, w0) :: g0) st0 e2 ltac:(assumption) H Hn). reflexivity.
    + (* EBind *)
      pose proof (IHb S Dm x d d' g st [bd] ltac:(simpl; rewrite ?andb_true_r; assumption) H Hst) as Hb.
      pose proof (@inv_b k d g st [bd] Hst) as Hn. simpl in Hb.
      destruct (eval_bindings k P' d' g st [trim_b T Dm bd]) as [[[d1' g1'] st1']|r1'],
               (eval_bindings k P d g st [bd]) as [[[d1 g1] st1]|r1]; simpl in Hb, Hn;
        try contradiction.
      * destruct Hb as [Hd [-> ->]]. exact (IHe S Dm x d1 d1' g1 st1 e1 ltac:(assumption) Hd Hn).
      * subst. reflexivity.
    + (* ESeq *)
      rewrite (IHe S Dm x d d' g st e1 ltac:(assumption) H Hst).
      pose proof (@inv_e k d g st e1 Hst) as Hn.
      destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try reflexivity.
      apply (IHe S Dm x); auto.
    + (* EAssign *)
      rewrite (IHe S Dm x d d' g st e1 Hc H Hst). reflexivity.
    + (* EIf *)
      rewrite (IHe S Dm x d d' g st c ltac:(assumption) H Hst).
      pose proof (@inv_e k d g st c Hst) as Hn.
      destruct (eval k P d g st c) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try reflexivity.
      rewrite truthy_eq. destruct (truthy P w0) as [[]|]; try reflexivity; apply (IHe S Dm x); auto.
    + (* EWhile *)
      rewrite (IHe S Dm x d d' g st c ltac:(assumption) H Hst).
      pose proof (@inv_e k d g st c Hst) as Hn.
      destruct (eval k P d g st c) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try reflexivity.
      rewrite truthy_eq. destruct (truthy P w0) as [[]|]; try reflexivity.
      rewrite (IHe S Dm x d d' g0 st0 e1 ltac:(assumption) H Hn).
      pose proof (@inv_e k d g0 st0 e1 Hn) as Hn1.
      destruct (eval k P d g0 st0 e1) as [[w1| | | | | | |] g1 st1]; simpl in Hn1; try reflexivity.
      change (EWhile (trim_e T Dm c) (trim_e T Dm e1)) with (trim_e T Dm (EWhile c e1)).
      apply (IHe S Dm x); simpl; auto. apply andb_true_iff; split; assumption.
    + (* ELoop *)
      rewrite (IHe S Dm x d d' g st e1 Hc H Hst).
      pose proof (@inv_e k d g st e1 Hst) as Hn.
      destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try reflexivity.
      change (ELoop (trim_e T Dm e1)) with (trim_e T Dm (ELoop e1)).
      apply (IHe S Dm x); simpl; auto.
    + (* EReturn *)
      rewrite (IHe S Dm x d d' g st e1 Hc H Hst). reflexivity.
    + (* EBreak *) reflexivity.
    + (* ERecord *)
      rewrite map_snd_trim, map_fst_trim. rewrite forallb_snd in Hc.
      rewrite (IHl S Dm x d d' g st (map snd fs) Hc H Hst). reflexivity.
    + (* EField *)
      rewrite (IHe S Dm x d d' g st e1 Hc H Hst). reflexivity.
    + (* EMatch *)
      rewrite (IHe S Dm x d d' g st e0 ltac:(assumption) H Hst).
      pose proof (@inv_e k d g st e0 Hst) as Hn.
      destruct (eval k P d g st e0) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try reflexivity.
      rewrite select_arm_trim. destruct (select_arm arms w0) as [[bs eb]|] eqn:Ea; simpl; [|reflexivity].
      assert (Hb : nchk T casts S Dm eb = true) by (eapply select_arm_chk; eassumption).
      rewrite (IHe S Dm x d d' (bs ++ g0) st0 eb Hb H Hn). reflexivity.
  - (* eval_arguments *)
    intros S Dm x d d' g st l Hc H Hst. destruct l as [|e1 r]; simpl in Hc |- *; [reflexivity|]. andb_split.
    rewrite (IHe S Dm x d d' g st e1 ltac:(assumption) H Hst).
    pose proof (@inv_e k d g st e1 Hst) as Hn.
    destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try reflexivity.
    rewrite (IHl S Dm x d d' g0 st0 r ltac:(assumption) H Hn). reflexivity.
  - (* eval_bindings *)
    intros S Dm x d d' g st bs Hc H Hst. destruct bs as [|b r]; simpl in Hc |- *.
    { simpl. auto. }
    andb_split.
    destruct b as [t tau|v e1|f f' types sm|tk m th f' types sm|u h types sm smc|u u' sm rm
                  |u th outputs sm rs|i p]; simpl in *; andb_split.
    + (* BType *) exact (IHb S Dm x _ _ g st r ltac:(assumption) (@envrel_bind_type T S Dm x d d' t tau H ltac:(assumption)) Hst).
    + (* BValue *)
      rewrite (IHe S Dm x d d' g st e1 ltac:(eassumption) H Hst).
      pose proof (@inv_e k d g st e1 Hst) as Hn.
      destruct (eval k P d g st e1) as [[w0| | | | | | |] g0 st0]; simpl in Hn; try reflexivity.
      apply (IHb S Dm x); auto. apply envrel_install; auto. constructor.
    + (* BFunction *)
      destruct (select_provisions d sm) as [dc|] eqn:Ec.
      * destruct (xrel_select H Ec) as [dc' [-> Hdc]].
        apply (IHb S Dm x); auto. apply envrel_install; auto.
        destruct types as [l als]. unfold trim_tc. simpl in *.
        exact (@envrel_capture T S Dm x d d' f' l dc dc' als [] H Hdc ltac:(eassumption)).
      * rewrite (select_none H Ec). reflexivity.
    + (* BMethod *)
      destruct (select_provisions d sm) as [dc|] eqn:Ec.
      * destruct (xrel_select H Ec) as [dc' [-> Hdc]].
        apply (IHb S Dm x); auto. apply envrel_install; auto.
        destruct types as [l als]. unfold trim_tc. simpl in *.
        exact (@envrel_capture T S Dm x d d' f' l dc dc' als [] H Hdc ltac:(eassumption)).
      * rewrite (select_none H Ec). reflexivity.
    + (* BFunctor *)
      destruct (select_provisions d smc) as [dc|] eqn:Ec.
      * destruct (xrel_select H Ec) as [dc' [-> [C1 [C2 C3]]]].
        apply (IHb S Dm x); auto. apply envrel_install; auto.
        destruct types as [lg la]. simpl in *. constructor; simpl; auto.
        -- apply (envrel_ground_subst H); assumption.
        -- apply (envrel_ground_subst H); assumption.
      * rewrite (select_none H Ec). reflexivity.
    + (* BCopyFunctor *)
      destruct (lookup_provision (IFun u') d) as [a|] eqn:El.
      * destruct (envrel_lookup H El) as [a' [-> Ha]].
        inversion Ha as [| |h sms dc dc' rms Hp Htm Hta|]; subst; try reflexivity.
        apply (IHb S Dm x); auto. apply envrel_install; auto. constructor; auto.
      * rewrite (lookup_none H El). reflexivity.
    + (* BApply *)
      pose proof (IHa S Dm x d d' u th outputs sm rs st ltac:(eassumption) H Hst) as Ha.
      pose proof (@inv_a k d u th outputs sm rs st Hst) as Hn.
      destruct (apply_functor k P' d' u th outputs sm rs st) as [[dr' st1']|r1'],
               (apply_functor k P d u th outputs sm rs st) as [[dr st1]|r1]; simpl in Ha, Hn;
        try contradiction.
      * destruct Ha as [Hx [Hk ->]]. apply (IHb S Dm x); auto.
        exact (envrel_append_keys H Hx Hk ltac:(eassumption)).
      * subst. reflexivity.
    + (* BPrimitive *)
      rewrite <- (@prim_closure_eq S Dm x d d' i p H H0).
      apply (IHb S Dm x); auto. apply envrel_install; auto.
      unfold primitive_closure. constructor.
  - (* apply_functor *)
    intros S Dm x d d' u th outputs sm rs st Hs H Hst. simpl.
    destruct (lookup_provision (IFun u) d) as [a|] eqn:El;
      [|rewrite (lookup_none H El); reflexivity].
    destruct (envrel_lookup H El) as [a' [-> Ha]].
    inversion Ha as [| |h sms dc dc' rms Hp Htm Hta|]; subst; try reflexivity.
    rewrite Hfun. destruct (view_functor P h) as [code|] eqn:Eh; simpl; [|reflexivity].
    destruct (select_provisions d sm) as [d1|] eqn:E1; [|rewrite (select_none H E1); reflexivity].
    destruct (xrel_select H E1) as [d1' [-> Hd1]].
    destruct (adapt_provisions d1 sms) as [d2|] eqn:E2; [|rewrite (adapt_none Hd1 E2); reflexivity].
    destruct (xrel_adapt Hd1 E2) as [d2' [-> Hd2]].
    assert (Henv : xrel T (application_environment P d h th (append_environment d2 dc))
                          (application_environment P' d' h th (append_environment d2' dc'))).
    { eapply app_env_rel; [exact H|exact Hs|]. apply xrel_append; [exact Hd2|]. repeat split; assumption. }
    pose proof (IHb all_gens all_gens true _ _ [] st (fc_bindings code) (Hfunchk Eh)
                  (xrel_envrel Henv all_gens) Hst) as Hb.
    pose proof (@inv_b k (application_environment P d h th (append_environment d2 dc)) []
                  st (fc_bindings code) Hst) as Hn.
    destruct (eval_bindings k P' (application_environment P' d' h th (append_environment d2' dc'))
                [] st (map (trim_b T all_gens) (fc_bindings code))) as [[[d3' g3'] st3']|r1'],
             (eval_bindings k P (application_environment P d h th (append_environment d2 dc))
                [] st (fc_bindings code)) as [[[d3 g3] st3]|r1]; simpl in Hb, Hn;
      try contradiction.
    + destruct Hb as [Hd3 [_ ->]].
      destruct (select_provisions d3 (result_selection rs rms (fc_sources code))) as [dr|] eqn:Er;
        [|rewrite (select_none Hd3 Er); reflexivity].
      destruct (xrel_select Hd3 Er) as [dr' [-> Hdr]].
      pose proof (result_env_rel outputs (fc_outputs code) (envrel_xrel Hd3) Hdr) as Q.
      destruct (result_environment outputs d3 (fc_outputs code) dr),
               (result_environment outputs d3' (fc_outputs code) dr'); try contradiction;
        simpl; [destruct Q; auto|reflexivity].
    + subst. reflexivity.
Qed.

Lemma sim_all : forall k, SIM k.
Proof.
  induction k as [|k IH]; [|exact (sim_step IH)].
  repeat split; intros; simpl; try reflexivity; auto.
Qed.

End SimSec.

Lemma nlook_map_dep : forall A B (F : nat -> A -> B) k (l : list (nat * A)),
  Elaboration.nlook k (map (fun p => (fst p, F (fst p) (snd p))) l) = option_map (F k) (Elaboration.nlook k l).
Proof.
  intros A B F k l. induction l as [|[k' a] r IH]; simpl; auto.
  destruct (Nat.eqb k k') eqn:E; auto. apply Nat.eqb_eq in E. subst. reflexivity.
Qed.

Lemma nlook_map : forall A B (f : A -> B) k (l : list (nat * A)),
  Elaboration.nlook k (map (fun p => (fst p, f (snd p))) l) = option_map f (Elaboration.nlook k l).
Proof. intros. exact (nlook_map_dep (fun _ => f) k l). Qed.

Lemma nlook_in : forall A k (l : list (nat * A)) a, Elaboration.nlook k l = Some a -> In (k, a) l.
Proof.
  induction l as [|[k' a'] r IH]; simpl; intros a H; [discriminate|].
  destruct (Nat.eqb k k') eqn:E; [apply Nat.eqb_eq in E; injection H as ->; subst; auto|].
  right. auto.
Qed.

Lemma lookup_cast_in : forall n cs c, lookup_cast n cs = Some c -> In (n, c) cs.
Proof.
  intros n cs. induction cs as [|[n' c'] r IH]; simpl; intros c H; [discriminate|].
  destruct (Nat.eqb n n') eqn:E; [apply Nat.eqb_eq in E; injection H as ->; subst; auto|auto].
Qed.

Lemma pv_ok_refl : forall T a, pv_ok T a = true -> prel T a a.
Proof.
  intros T. fix IH 1. intros [w|f [ts als ps]|h sms [ts als ps] rms|p ot] H; simpl in H.
  - constructor.
  - apply andb_true_iff in H as [H1 H2]. constructor; simpl.
    + clear H1. induction ps as [|[i a] r IHr]; simpl in *; constructor;
        apply andb_true_iff in H2 as [Ha Hr]; [apply IH; exact Ha|apply IHr; exact Hr].
    + intros g _. reflexivity.
    + intros g Hg. unfold mean. rewrite lookup_subst_none; [reflexivity|].
      intros [g' t] Hin E. simpl in E. subst g'. rewrite forallb_forall in H1.
      specialize (H1 _ Hin). simpl in H1. congruence.
  - constructor; simpl; auto.
    induction ps as [|[i a] r IHr]; simpl in *; constructor;
      apply andb_true_iff in H as [Ha Hr]; [apply IH; exact Ha|apply IHr; exact Hr].
  - constructor.
Qed.

Lemma lrel2_refl_ok : forall T ps, forallb (fun q => pv_ok T (snd q)) ps = true -> lrel2 T ps ps.
Proof.
  intros T ps. induction ps as [|[i a] r IH]; simpl; intros H; constructor;
    apply andb_true_iff in H as [Ha Hr]; [apply pv_ok_refl; exact Ha|apply IH; exact Hr].
Qed.

(* Running `main` of the trimmed program gives exactly the reference
   result, for any primitives that ignore the code of the program view (on
   states satisfying an invariant they keep). *)
Theorem run_main_trimmed : forall T p argv rd fuel st0 (ok : mstate -> Prop),
  check T p = true ->
  let P := program_of p argv rd in
  let P' := program_of (trim_program T p) argv rd in
  (forall q ws st, ok st -> run_prim P' q ws st = run_prim P q ws st) ->
  (forall q ws st, ok st -> ok (snd (run_prim P q ws st))) ->
  ok st0 ->
  run_main fuel P' (program_main (trim_program T p)) (program_environment (trim_program T p)) st0
  = run_main fuel P (program_main p) (program_environment p) st0.
Proof.
  intros T p argv rd fuel st0 ok Hchk P P' Hprim Hok Hst.
  unfold check in Hchk. andb_split. rename H into Hc1, H2 into Hc2, H1 into Hc3, H0 into Hc4.
  assert (Hfn : forall f, view_function P' f = option_map (trim_e T (Dt T f)) (view_function P f)).
  { intro f. unfold P, P', program_of, trim_program. simpl.
    exact (nlook_map_dep (fun f e => trim_e T (Dt T f) e) f (program_functions p)). }
  assert (Hfun : forall h, view_functor P' h = option_map (trim_code T) (view_functor P h)).
  { intro h. unfold P, P', program_of, trim_program. simpl. apply nlook_map. }
  assert (Hcastg : forall n c h t, lookup_cast n (view_casts P) = Some c ->
            cast_handle c = Some (h, t) -> In h (cast_gens p)).
  { intros n c h t Hl Hh. unfold P, program_of in Hl. simpl in Hl. apply lookup_cast_in in Hl.
    unfold cast_gens. apply in_flat_map. exists (n, c). split; [exact Hl|]. simpl. rewrite Hh. left. reflexivity. }
  assert (Hclosed : forall f b, view_function P f = Some b ->
            nchk T (cast_gens p) (Gt T f) (Dt T f) b = true).
  { intros f b Hf. apply nlook_in in Hf. rewrite forallb_forall in Hc1. exact (Hc1 (f, b) Hf). }
  assert (Hfunchk : forall h c, view_functor P h = Some c ->
            forallb (bchk T (cast_gens p) all_gens all_gens) (fc_bindings c) = true).
  { intros h c Hf. apply nlook_in in Hf. rewrite forallb_forall in Hc2. exact (Hc2 (h, c) Hf). }
  destruct (@sim_all T (cast_gens p) P P' ok Hfn Hfun Hprim Hok eq_refl eq_refl eq_refl eq_refl
              Hcastg Hclosed Hfunchk fuel) as [Hsim _].
  unfold run_main. simpl program_main. simpl program_environment. rewrite Hfn.
  destruct (view_function P (program_main p)) as [body|] eqn:Em; simpl; [|reflexivity].
  assert (Henv : envrel T (Gt T (program_main p)) (Dt T (program_main p)) false
                   (program_environment p) (program_environment p)).
  { split; [apply lrel2_refl_ok; exact Hc4|]. split; [intros g _; reflexivity|].
    split; [|discriminate].
    intros g Hg. unfold mean. rewrite lookup_subst_none; [reflexivity|].
    intros [g' t] Hin E. simpl in E. subst g'. rewrite forallb_forall in Hc3.
    specialize (Hc3 _ Hin). simpl in Hc3. congruence. }
  rewrite (Hsim _ _ false _ _ [] st0 body (Hclosed _ _ Em) Henv Hst). reflexivity.
Qed.

Lemma compute_T_check : forall n p T0 T, compute_T n p T0 = Some T -> check T p = true.
Proof.
  induction n as [|n IH]; intros p T0 T H; simpl in H;
    destruct (check T0 p) eqn:E; try (injection H as <-; exact E); try discriminate; eauto.
Qed.

(* The pass itself: no premise on the program. *)
Theorem captrim_sound : forall p argv rd fuel st0 (ok : mstate -> Prop),
  let P := program_of p argv rd in
  let P' := program_of (captrim p) argv rd in
  (forall q ws st, ok st -> run_prim P' q ws st = run_prim P q ws st) ->
  (forall q ws st, ok st -> ok (snd (run_prim P q ws st))) ->
  ok st0 ->
  run_main fuel P' (program_main (captrim p)) (program_environment (captrim p)) st0
  = run_main fuel P (program_main p) (program_environment p) st0.
Proof.
  intros p argv rd fuel st0 ok P P'. unfold P', captrim.
  destruct (compute_T 64 p (init_tbl p)) as [T|] eqn:E.
  - apply run_main_trimmed. eapply compute_T_check; eauto.
  - intros. reflexivity.
Qed.

End Sim.

(* The pass keeps everything but code. *)
Lemma captrim_view : forall p argv rd,
  let P' := program_of (captrim p) argv rd in let P := program_of p argv rd in
  view_bool P' = view_bool P /\ view_true P' = view_true P /\ view_false P' = view_false P
  /\ view_arguments P' = view_arguments P /\ view_read P' = view_read P
  /\ view_casts P' = view_casts P.
Proof. intros p argv rd P' P. unfold P', P, captrim. destruct (compute_T 64 p (init_tbl p)); simpl; repeat split. Qed.

(* ===================== 6. Public runs ==================================== *)

Module TrimRuns (Host : HOST) (M : PUBLIC Host).
Module PS := Sim M.ReferenceHost M.Reference.

(* As ProgramPrune.PruneRuns.code_blind: the interface's primitives do not
   read the program view's code. *)
Definition code_blind (interface : M.interface) : Prop :=
  forall P P', view_bool P' = view_bool P -> view_true P' = view_true P ->
    view_false P' = view_false P -> view_arguments P' = view_arguments P ->
    view_read P' = view_read P -> view_casts P' = view_casts P ->
    forall q ws s, M.primitive interface P' q ws s = M.primitive interface P q ws s.

(* No safety premise: the trimmed run is the reference run. *)
Theorem observe_trimmed : forall program interface initial,
  code_blind interface ->
  forall fuel,
    M.observe (M.Reference.run_main fuel (M.runtime (captrim program) initial)
      (program_main (captrim program)) (program_environment (captrim program)) (interface, initial))
    = M.observe (M.Reference.run_main fuel (M.runtime program initial)
      (program_main program) (program_environment program) (interface, initial)).
Proof.
  intros program interface initial Hblind fuel. unfold M.runtime.
  rewrite (@PS.captrim_sound program (M.arguments initial) (M.read_file initial) fuel (interface, initial)
              (fun st => fst st = interface)); [reflexivity| | |reflexivity].
  - intros q ws [i s] Hi. simpl in Hi. subst i. unfold M.ReferenceHost.run_prim.
    destruct (captrim_view program (M.arguments initial) (M.read_file initial))
      as [B [T [F [A [Rd C]]]]].
    rewrite (Hblind _ _ B T F A Rd C). reflexivity.
  - intros q ws [i s] Hi. simpl in Hi. subst i. unfold M.ReferenceHost.run_prim.
    destruct (M.supports interface q); [|reflexivity].
    destruct (M.primitive interface _ q ws (M.external s)). reflexivity.
Qed.

Theorem runs_trimmed : forall program interface initial result,
  code_blind interface ->
  M.Runs (captrim program) interface initial result <-> M.Runs program interface initial result.
Proof.
  intros program interface initial result Hblind. unfold M.Runs.
  split; intros [fuel E]; exists fuel;
    rewrite <- E; [symmetry|]; apply observe_trimmed; assumption.
Qed.

End TrimRuns.
