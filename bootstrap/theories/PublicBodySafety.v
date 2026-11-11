(* Foundations of the body-level safety invariant (T02), for the slice of
   source bodies without calls, provisions or binds. The local store is
   typed positionally by a runtime type list; the static context's locals
   are realized in it through the frame's injective numbering. Outcome is
   what an evaluation may produce: never Stuck, a Breaking only where the
   source breaks inside a loop, and typed values otherwise. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

(* ---- fits is sound for runtime types --------------------------------- *)

Section Fits.
Variables (W : A.world) (P : program_view).

Lemma ground_union : forall d ms, ground d (TUnion ms) = TUnion (map (ground d) ms).
Proof. intros d ms; unfold ground; apply subst_union. Qed.

Theorem fits_ground : forall H d value t expected,
  A.fits t expected -> value_typed W P H value (ground d t) ->
  value_typed W P H value (ground d expected).
Proof.
  intros H d value t expected Fits Typed.
  destruct Fits as [<-|[->|[members [-> Included]]]]; [exact Typed| |].
  - exfalso. exact (empty_union_uninhabited W P H value Typed).
  - rewrite ground_union. destruct t as [m|fs| |g|n xs|parts];
      try (eapply TypedUnion; [exact Typed|apply in_map, Included; left; reflexivity]).
    rewrite ground_union in Typed. inversion Typed as [| | | | |? member ? Member In'| |]; subst.
    apply in_map_iff in In' as [part [<- Part]].
    eapply TypedUnion; [exact Member|apply in_map, Included, Part].
Qed.
Lemma nominal_value : forall H w n xs, value_typed W P H w (TNom n xs) -> exists inner, w = VTag n inner.
Proof. intros H w n xs Typed; inversion Typed; subst; eauto. Qed.
Lemma record_value : forall H w fs, value_typed W P H w (TRecord fs) ->
  exists values, w = VRecord values /\ fields_typed W P H values fs.
Proof. intros H w fs Typed; inversion Typed; subst; eauto. Qed.
End Fits.

Lemma slook_ground : forall d n (fs : list (string * ty)),
  A.slook n (map (fun q => (fst q, ground d (snd q))) fs) = option_map (ground d) (A.slook n fs).
Proof. intros d n fs; induction fs as [|[g t] r IH]; cbn; [reflexivity|]. destruct (String.eqb n g); auto. Qed.
Lemma ground_record : forall d fs, ground d (TRecord fs) = TRecord (map (fun q => (fst q, ground d (snd q))) fs).
Proof. intros d fs; unfold ground; apply subst_record. Qed.
Lemma ground_nom : forall d n xs, ground d (TNom n xs) = TNom n (PublicTypeRead.smap (type_meanings d) xs).
Proof. intros d n xs; unfold ground; apply subst_nom. Qed.

Lemma fits_refl : forall t, A.fits t t.
Proof. left; reflexivity. Qed.

Lemma fits_never : forall t, A.fits A.never t.
Proof. right; left; reflexivity. Qed.

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

(* ---- frames and local stores ------------------------------------------ *)

Definition FrameOk (F : A.frame) : Prop :=
  NoDup (map fst (A.fr_var F)) /\ NoDup (map snd (A.fr_var F)) /\
  (A.fr_this F <> None -> forall n, A.slook n (A.fr_var F) <> Some 0).

Lemma numbering_frame : forall F params,
  A.Numbering F (A.fr_this F) params -> FrameOk F.
Proof.
  intros F params [Names [Numbers [Reserved _]]]. split; [exact Names|]. split; [exact Numbers|].
  intros Receiver n. destruct (A.fr_this F); [exact (Reserved n)|contradiction].
Qed.

Lemma slook_in : forall X (l : list (string * X)) n x, A.slook n l = Some x -> In (n, x) l.
Proof.
  induction l as [|[m y] r IH]; intros n x Found; cbn in Found; [discriminate|].
  destruct (String.eqb_spec n m) as [->|_]; [injection Found as <-; left; reflexivity|right; auto].
Qed.

Lemma snd_injective : forall (l : list (string * nat)) a b k,
  NoDup (map snd l) -> In (a, k) l -> In (b, k) l -> a = b.
Proof.
  induction l as [|[m j] r IH]; intros a b k Distinct InA InB; [contradiction|].
  cbn in Distinct. inversion Distinct as [|? ? Absent Rest]; subst.
  destruct InA as [Ea|InA]; destruct InB as [Eb|InB].
  - congruence.
  - injection Ea as -> ->. exfalso. apply Absent. change k with (snd (b, k)). apply in_map, InB.
  - injection Eb as -> ->. exfalso. apply Absent. change k with (snd (a, k)). apply in_map, InA.
  - exact (IH a b k Rest InA InB).
Qed.

Lemma var_of_injective : forall F n m,
  FrameOk F -> A.Named F n -> A.Named F m -> A.var_of F n = A.var_of F m -> n = m.
Proof.
  intros F n m [_ [Numbers _]] Hn Hm Same. unfold A.Named, A.var_of in *.
  destruct (A.slook n (A.fr_var F)) as [k|] eqn:Kn; [|contradiction].
  destruct (A.slook m (A.fr_var F)) as [j|] eqn:Km; [|contradiction]. subst j.
  exact (snd_injective _ _ _ _ Numbers (slook_in _ _ _ _ Kn) (slook_in _ _ _ _ Km)).
Qed.

Lemma var_of_not_receiver : forall F n,
  FrameOk F -> A.fr_this F <> None -> A.Named F n -> A.var_of F n <> 0.
Proof.
  intros F n [_ [_ Reserved]] Receiver Hn Zero. unfold A.Named, A.var_of in *.
  destruct (A.slook n (A.fr_var F)) as [k|] eqn:Kn; [|contradiction]. subst k.
  exact (Reserved Receiver n Kn).
Qed.

(* What a closure's application reads an input as: its alias, else its
   meaning, else itself. *)
Definition cmeaning (d : environment) (x : nat) : ty :=
  match output_meaning d x with Some t => t | None => TAbs x end.

(* The type table, as the environment's aliases record it. *)
Definition AliasOk (C : A.cx) (d : environment) : Prop :=
  forall x tx, A.nlook x (A.c_ty C) = Some tx -> output_meaning d x = Some (ground d tx).

Lemma alias_cmeaning : forall C d x tx, AliasOk C d -> A.nlook x (A.c_ty C) = Some tx -> cmeaning d x = ground d tx.
Proof. intros C d x tx Al L. unfold cmeaning. rewrite (Al x tx L). reflexivity. Qed.

(* The static locals and receiver, realized in a positional type list,
   and the type table in the environment's aliases. *)
Definition CtxLocals (F : A.frame) (C : A.cx) (d : environment) (types : list (var * ty)) : Prop :=
  (forall n t mu, A.slook n (A.c_locals C) = Some (t, mu) -> A.Named F n ->
     A.nlook (A.var_of F n) types = Some (ground d t)) /\
  (forall t, A.fr_this F = Some t -> A.nlook 0 types = Some (ground d t)) /\
  AliasOk C d.

Lemma ctx_locals_push : forall F C d types n t mu,
  FrameOk F -> CtxLocals F C d types -> A.Named F n ->
  CtxLocals F (A.add_local C n t mu) d ((A.var_of F n, ground d t) :: types).
Proof.
  intros F C d types n t mu Frame [Locals [Receiver Al]] Named. split; [|split; [|exact Al]].
  - intros m tm mum Found Hm. cbn in Found |- *.
    destruct (String.eqb_spec m n) as [->|Different].
    + injection Found as <- _. rewrite Nat.eqb_refl. reflexivity.
    + destruct (Nat.eqb_spec (A.var_of F m) (A.var_of F n)) as [Same|_].
      * exfalso. exact (Different (var_of_injective F m n Frame Hm Named Same)).
      * exact (Locals m tm mum Found Hm).
  - intros tr This. cbn [A.nlook].
    destruct (Nat.eqb_spec 0 (A.var_of F n)) as [Zero|_].
    + exfalso. apply (var_of_not_receiver F n Frame); [congruence|exact Named|auto].
    + exact (Receiver tr This).
Qed.

Lemma ctx_locals_push_all : forall F binds C d types,
  FrameOk F -> CtxLocals F C d types -> Forall (fun q => A.Named F (fst q)) binds ->
  CtxLocals F (fold_left (fun C q => A.add_local C (fst q) (snd q) A.LLet) binds C) d
    (rev (map (fun q => (A.var_of F (fst q), ground d (snd q))) binds) ++ types).
Proof.
  intros F binds. induction binds as [|[n t] rest IH]; intros C d types Frame Ctx Named; [exact Ctx|].
  inversion Named as [|? ? Here Rest]; subst. cbn [fold_left map rev].
  rewrite <- app_assoc. apply IH; [exact Frame| |exact Rest].
  apply ctx_locals_push; assumption.
Qed.

(* ---- outcomes ------------------------------------------------------------ *)

Module Outcomes (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module I := I0.

Section Outcome.
Variables (W : A.world) (P : program_view).
Variable state_ok : heap_types -> Prims.mstate -> Prop.

(* What survives every evaluation: a typed store over the same positional
   types, a valid state for an extended ghost heap. *)
Definition Post (H : heap_types) (types : list (var * ty)) (g : locals) (st : Prims.mstate) : Prop :=
  exists H', heap_extends H H' /\ state_ok H' st /\ locals_typed W P H' g types /\
    (forall value tau, value_typed W P H value tau -> value_typed W P H' value tau).

Definition Outcome (F : A.frame) (d : environment) (H : heap_types) (types : list (var * ty))
    (t : ty) (breaks : Prop) (result : I.evaluation) : Prop :=
  match result with
  | I.Evaluation signal g st =>
      match signal with
      | Normal value => exists H', heap_extends H H' /\ state_ok H' st /\
          locals_typed W P H' g types /\ value_typed W P H' value (ground d t)
      | Returning value => exists H' ret, A.fr_ret F = Some ret /\ heap_extends H H' /\
          state_ok H' st /\ locals_typed W P H' g types /\ value_typed W P H' value (ground d ret)
      | Breaking => breaks /\ A.fr_loop F = true /\ exists H', heap_extends H H' /\
          state_ok H' st /\ locals_typed W P H' g types
      | Stuck _ => False
      | _ => True
      end
  end.

Lemma outcome_fits : forall F d H types t expected breaks result,
  A.fits t expected -> Outcome F d H types t breaks result ->
  Outcome F d H types expected breaks result.
Proof.
  intros F d H types t expected breaks [[] g st] Fits Out; cbn in *; auto.
  destruct Out as [H' [Extends [State [Locals Typed]]]].
  exists H'. repeat split; auto. eapply fits_ground; eassumption.
Qed.

Lemma outcome_breaks : forall F d H types t (breaks breaks' : Prop) result,
  (breaks -> breaks') -> Outcome F d H types t breaks result ->
  Outcome F d H types t breaks' result.
Proof.
  intros F d H types t breaks breaks' [[] g st] Weaken Out; cbn in *; auto.
  destruct Out as [Breaks Rest]; auto.
Qed.
End Outcome.
End Outcomes.

(* ---- distinct binders: order is irrelevant --------------------------------- *)

Lemma nlook_app_in : forall X (l m : list (nat * X)) k v,
  NoDup (map fst l) -> In (k, v) l -> A.nlook k (l ++ m) = Some v.
Proof.
  induction l as [|[a x] r IH]; intros m k v Distinct Found; [contradiction|].
  cbn in Distinct |- *. inversion Distinct as [|? ? Absent Rest]; subst.
  destruct Found as [Here|There].
  - injection Here as -> ->. rewrite Nat.eqb_refl. reflexivity.
  - destruct (Nat.eqb_spec k a) as [->|_]; [|exact (IH m k v Rest There)].
    exfalso. apply Absent. change a with (fst (a, v)). apply in_map, There.
Qed.

Lemma nlook_app_out : forall X (l m : list (nat * X)) k,
  ~ In k (map fst l) -> A.nlook k (l ++ m) = A.nlook k m.
Proof.
  induction l as [|[a x] r IH]; intros m k Absent; [reflexivity|].
  cbn in Absent |- *. destruct (Nat.eqb_spec k a) as [->|_]; [exfalso; auto|].
  apply IH. intro; auto.
Qed.

Lemma nlook_rev_distinct : forall X (l m : list (nat * X)) k,
  NoDup (map fst l) -> A.nlook k (rev l ++ m) = A.nlook k (l ++ m).
Proof.
  intros X l m k Distinct.
  assert (RevDistinct : NoDup (map fst (rev l))) by (rewrite map_rev; apply NoDup_rev; exact Distinct).
  destruct (in_dec Nat.eq_dec k (map fst l)) as [Present|Absent].
  - apply in_map_iff in Present as [[k' v] [Key Found]]; cbn in Key; subst k'.
    rewrite (nlook_app_in _ _ _ _ _ Distinct Found).
    apply nlook_app_in; [exact RevDistinct|apply in_rev; rewrite rev_involutive; exact Found].
  - rewrite (nlook_app_out _ _ _ _ Absent). apply nlook_app_out.
    rewrite map_rev. intro In'; apply Absent; apply in_rev; exact In'.
Qed.

Lemma ctx_locals_push_distinct : forall F binds C d types,
  FrameOk F -> CtxLocals F C d types -> Forall (fun q => A.Named F (fst q)) binds ->
  NoDup (map fst binds) ->
  CtxLocals F (fold_left (fun C q => A.add_local C (fst q) (snd q) A.LLet) binds C) d
    (map (fun q => (A.var_of F (fst q), ground d (snd q))) binds ++ types).
Proof.
  intros F binds C d types Frame Ctx Named Distinct.
  pose proof (ctx_locals_push_all F binds C d types Frame Ctx Named) as [Locals [Receiver Al]].
  set (l := map (fun q => (A.var_of F (fst q), ground d (snd q))) binds) in *.
  assert (Vars : NoDup (map fst l)).
  { subst l. rewrite map_map. cbn. clear Ctx Locals Receiver Al.
    induction binds as [|[n t] rest IH]; cbn; [constructor|].
    inversion Named as [|? ? Here Rest]; inversion Distinct as [|? ? Absent Others]; subst.
    constructor; [|exact (IH Rest Others)].
    intro In'. apply in_map_iff in In' as [[m u] [Same Found]]. cbn in Same.
    apply Absent. replace n with m; [change m with (fst (m, u)); apply in_map, Found|].
    apply (var_of_injective F); [exact Frame| |exact Here|exact Same].
    rewrite Forall_forall in Rest. exact (Rest (m, u) Found). }
  split; [|split; [|exact Al]].
  - intros n t mu Found Hn. rewrite <- nlook_rev_distinct by exact Vars.
    exact (Locals n t mu Found Hn).
  - intros t This. rewrite <- nlook_rev_distinct by exact Vars.
    exact (Receiver t This).
Qed.
