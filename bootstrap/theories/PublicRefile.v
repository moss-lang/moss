(* Refiled method installs in a linked functor body (T15).

   LinkBody files a linked body's binds as `flat_map (refile pairs) binds0`:
   after each method bind, `refile` installs the same method again under
   each key F declares it by (`A.output_renaming outs gen (A.rkey_of Cr k)`,
   receiver and θ in F's terms). The slice's obligation `RefileRunOk` says
   that this refiled run behaves as the body's chain does.

   An extra install changes the environment at its key. It leaves the
   context after the method bind realized exactly when no provision of
   that context is filed under the same runtime key (`NoShadow`). Under
   that condition, a chain runs refiled as it runs plain
   (`refile_rchain_run`), so `RefileRunOk` follows from `RefileShadowOk`:
   every chain of a linked body meets `NoShadow` after each method bind.

   `RefileShadowOk` is not true of every accepted program today:
   examples/refile-stale-shadow.moss binds `T.out` while T is still G's
   input, then `Int.out` (refiled under F's `T.out`), so the refiled install
   overwrites the stale `T.out` the context still lists, and a later call
   through it runs `Int.out` on a Bool (the reference interpreter stops
   Stuck; native prints 7). See scratch/orch/refile-STATUS.md. *)
From Stdlib Require Import List Arith Lia Bool PrimInt63.
From NM Require Import Audit PublicValueTyping PublicBodySafety PublicBodySlice.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Module Refile (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module S := PublicBodySlice.BodySlice Prims I0.
Module I := S.I.

Section Refiled.
Variables (W : A.world) (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Notation P := (program_of Prog argv read).
Variable state_ok : heap_types -> Prims.mstate -> Prop.

Notation Pre := (S.Pre W Prog argv read state_ok).
Notation Realizes := (S.Realizes W Prog argv read state_ok).
Notation BindStep := (S.BindStep W Prog argv read state_ok).
Notation Chain := (S.Chain W Prog argv read state_ok).
Notation Outcome := (S.O.Outcome W P state_ok).

(* ---- the extra installs ---------------------------------------------------- *)

(* What refile adds after a binding: for a method bind, the same method
   under each other key a pair names for it. *)
Definition refile_extras (pairs : list (item * item)) (b : binding) : list binding :=
  match b with
  | BMethod t m th f types sm =>
      flat_map (fun p => match p with
                         | (IMth t' m' _, IMth t0 _ th0) =>
                             if ty_eqb t t' && Nat.eqb m m' && negb (item_eqb (IMth t0 m th0) (IMth t m th))
                             then [BMethod t0 m th0 f types sm] else []
                         | _ => [] end) pairs
  | _ => []
  end.

Lemma refile_split : forall pairs b, A.refile pairs b = b :: refile_extras pairs b.
Proof. intros pairs []; reflexivity. Qed.

Lemma extras_shape : forall pairs t m th f types sm b',
  In b' (refile_extras pairs (BMethod t m th f types sm)) -> exists t0 th0, b' = BMethod t0 m th0 f types sm.
Proof.
  intros pairs t m th f types sm b' I. cbn [refile_extras] in I. apply in_flat_map in I as [[k1 k2] [_ I]].
  destruct k1; try contradiction. destruct k2; try contradiction.
  destruct (_ && _ && _); [destruct I as [<-|[]]; eauto|contradiction].
Qed.

Lemma extras_other : forall pairs b, (forall t m th f types sm, b <> BMethod t m th f types sm) ->
  refile_extras pairs b = [].
Proof. intros pairs [] N; try reflexivity. exfalso. eapply N. reflexivity. Qed.

(* No provision of C is filed under runtime key k (a method key, so values,
   fns and functors are filed elsewhere anyway). *)
Definition FreeKey (C : A.cx) (k : item) : Prop :=
  forall r m g, A.mlook r m (A.c_mths C) = Some g -> A.rkey C r m <> k.

(* After the method bind b, in the context C1 it leaves, none of refile's
   extra keys is the runtime key of a provision. *)
Definition NoShadow (pairs : list (item * item)) (b : binding) (C1 : A.cx) : Prop :=
  forall t0 m0 th0 f0 ty0 sm0, In (BMethod t0 m0 th0 f0 ty0 sm0) (refile_extras pairs b) ->
  FreeKey C1 (IMth t0 m0 th0).

Lemma realizes_install_free : forall n C d H t0 m0 th0 pv, Realizes n C d H -> FreeKey C (IMth t0 m0 th0) ->
  Realizes n C (install_provision (IMth t0 m0 th0) pv d) H.
Proof.
  intros n C d H t0 m0 th0 pv [Abs [[Vals [Fns [Mths Funs]]] St]] Free.
  split; [exact Abs|]. split; [|exact St]. split; [|split; [|split]].
  - intros v t Found. destruct (Vals v t Found) as [w [Look T]]. exists w.
    split; [rewrite S.install_other; [exact Look|discriminate]|exact T].
  - intros f g Found. destruct (Fns f g Found) as [pv' [Look Ok]]. exists pv'.
    split; [rewrite S.install_other; [exact Look|discriminate]|exact Ok].
  - intros r m g Found. destruct (Mths r m g Found) as [pv' [Look Ok]]. exists pv'.
    split; [rewrite S.install_other; [exact Look|exact (Free r m g Found)]|exact Ok].
  - intros u fx Found. destruct (Funs u fx Found) as [pv' [gfx [Look Rest]]]. exists pv', gfx.
    split; [rewrite S.install_other; [exact Look|discriminate]|exact Rest].
Qed.

Lemma pre_install_free : forall r F C d types H g st t0 m0 th0 pv,
  Pre r F C d types H g st -> FreeKey C (IMth t0 m0 th0) ->
  Pre r F C (install_provision (IMth t0 m0 th0) pv d) types H g st.
Proof.
  intros r F C d types H g st t0 m0 th0 pv Pre0 Free.
  pose proof Pre0 as [_ [_ [_ [St [Real _]]]]].
  exact (S.pre_keep W Prog argv read state_ok r F C d (install_provision (IMth t0 m0 th0) pv d) types H H g st st Pre0 eq_refl eq_refl
    (realizes_install_free r C d H t0 m0 th0 pv Real Free) (fun a c L => L) St).
Qed.

Lemma select_install : forall sm d k pv dc, select_provisions d sm = Some dc ->
  exists dc', select_provisions (install_provision k pv d) sm = Some dc'.
Proof.
  induction sm as [|[req s] rest IH]; intros d k pv dc Sel; [eexists; reflexivity|].
  cbn [select_provisions] in Sel |- *.
  destruct (lookup_provision s d) as [en|] eqn:L; [|discriminate].
  destruct (select_provisions d rest) as [d'|] eqn:R; [|discriminate].
  destruct (IH d k pv d' R) as [d'' R']. rewrite R'.
  unfold lookup_provision at 1. cbn [install_provision provision_values lookup_entry].
  destruct (item_eqb s k); [eexists; reflexivity|]. change (lookup_entry s (provision_values d)) with (lookup_provision s d).
  rewrite L. eexists; reflexivity.
Qed.

(* A run of extra installs of one method, each under a free key: it keeps
   the context realized, or runs out of fuel. *)
Lemma extras_run : forall r F C types H f ty sm es,
  (forall b', In b' es -> exists t0 m0 th0, b' = BMethod t0 m0 th0 f ty sm /\ FreeKey C (IMth t0 m0 th0)) ->
  forall j d g st rest, Pre r F C d types H g st -> (exists dc, select_provisions d sm = Some dc) ->
  (exists d' j', j = j' + List.length es /\ type_meanings d' = type_meanings d /\ Pre r F C d' types H g st /\
     I.eval_bindings j P d g st (es ++ rest) = I.eval_bindings j' P d' g st rest) \/
  I.eval_bindings j P d g st (es ++ rest) = inr (I.Evaluation OutOfFuel g st).
Proof.
  intros r F C types H f ty sm es. induction es as [|b' es IH]; intros Sh j d g st rest Pre0 [dc Sel].
  - left. exists d, j. split; [cbn; lia|]. split; [reflexivity|]. split; [exact Pre0|reflexivity].
  - destruct j as [|j]; [right; reflexivity|].
    destruct (Sh b' (or_introl eq_refl)) as [t0 [m0 [th0 [-> Free]]]].
    cbn [app]. rewrite (S.bindings_method_eq Prog argv read), Sel.
    set (d1 := install_provision (IMth t0 m0 th0) (FunctionClosure f (typed_capture d ty dc)) d).
    destruct (IH (fun b I => Sh b (or_intror I)) j d1 g st rest (pre_install_free _ _ _ _ _ _ _ _ _ _ _ _ Pre0 Free)
      (select_install sm d _ _ dc Sel)) as [[d' [j' [Ej [Em [Pre' Eq]]]]]|Out]; [left|right; exact Out].
    exists d', j'. split; [cbn; lia|]. split; [exact Em|]. split; [exact Pre'|exact Eq].
Qed.

(* ---- chains whose method binds do not shadow ------------------------------- *)

(* A chain (S.Chain) whose every step leaves a context where the step's
   refiled keys are free. *)
Inductive RChain (J k : nat) (pairs : list (item * item)) :
  A.frame -> A.cx -> A.frame -> A.cx -> Prop -> list nat -> list binding -> Prop :=
| RChainNil : forall F F' C (B : Prop), S.FrameSame F F' -> RChain J k pairs F C F' C B [] []
| RChainCons : forall F C F1 C1 F2 C2 (B1 B2 B : Prop) m1 m2 b bs,
    (forall r, r <= S k -> BindStep J r F C F1 C1 B1 m1 b) -> NoShadow pairs b C1 ->
    RChain J k pairs F1 C1 F2 C2 B2 m2 bs ->
    (B1 \/ B2 -> B) -> RChain J k pairs F C F2 C2 B (m1 ++ m2)%list (b :: bs).

Lemma rchain_chain : forall J k pairs F C F' C' B made bs, RChain J k pairs F C F' C' B made bs ->
  Chain J k F C F' C' B made bs.
Proof.
  intros J k pairs F C F' C' B made bs D.
  induction D as [F F' C B Same|F C F1 C1 F2 C2 B1 B2 B m1 m2 b bs Step _ _ IH Imp];
    [constructor; exact Same|].
  exact (S.ChainCons W Prog argv read state_ok J k F C F1 C1 F2 C2 B1 B2 B m1 m2 b bs Step IH Imp).
Qed.

(* A stopped run's outcome, after one step of the chain. *)
Lemma outcome_after : forall F F1 d d1 H H1 types m1 (B1 B2 B : Prop) res,
  S.FrameSame F F1 -> S.FrameAvoids m1 F -> heap_extends H H1 -> S.MeaningsGrow m1 d d1 -> (B1 \/ B2 -> B) ->
  Outcome F1 d1 H1 types TUnit B2 res -> Outcome F d H types TUnit B res.
Proof.
  intros F F1 d d1 H H1 types m1 B1 B2 B res Same FAv E M Imp Out.
  eapply (S.outcome_heap W Prog argv read state_ok); [exact E|].
  apply (S.outcome_grow W Prog argv read state_ok m1 F d d1 H1 types TUnit _ _ M (fun g _ I => I) FAv).
  apply (S.outcome_frame W Prog argv read state_ok F F1 d1 H1 types TUnit _ _ Same).
  eapply S.O.outcome_breaks; [|exact Out]. tauto.
Qed.

Lemma method_or_not : forall b, (exists t m th f types sm, b = BMethod t m th f types sm) \/
  (forall t m th f types sm, b <> BMethod t m th f types sm).
Proof. intros []; try (right; intros; discriminate). left. eauto 7. Qed.

(* chain_run, for the refiled list. *)
Lemma refile_rchain_run : forall J k pairs F C F' C' (Bi : Prop) made bs, RChain J k pairs F C F' C' Bi made bs ->
  forall r j d types H g st rest, r <= S k -> j < r -> j <= J -> Pre r F C d types H g st ->
  (exists d' g' st' H' j', j' <= j /\ heap_extends H H' /\ S.MeaningsGrow made d d' /\
     Pre r F' C' d' types H' g' st' /\
     I.eval_bindings j P d g st (flat_map (A.refile pairs) bs ++ rest)%list = I.eval_bindings j' P d' g' st' rest) \/
  (exists res, I.eval_bindings j P d g st (flat_map (A.refile pairs) bs ++ rest)%list = inr res /\
     (forall v, I.evaluation_signal res <> Normal v) /\ Outcome F d H types TUnit Bi res).
Proof.
  intros J k pairs F C F' C' Bi made bs D.
  induction D as [F F' C B0 Same|F C F1 C1 F2 C2 B1 B2 B m1 m2 b bs Step NoSh Rest IH Imp];
    intros r j d types H g st rest Lr Lj LJ Pre0.
  - left. exists d, g, st, H, j. split; [lia|]. split; [intros a c L; exact L|].
    split; [apply S.meanings_grow_refl; reflexivity|].
    split; [exact (S.pre_frame W Prog argv read state_ok _ _ _ _ _ _ _ _ _ Same Pre0)|reflexivity].
  - (* out of fuel before the step *)
    assert (Fuel0 : forall j0 d0 g0 st0 xs, j0 = 0 -> exists res, I.eval_bindings j0 P d0 g0 st0 xs = inr res /\
              (forall v, I.evaluation_signal res <> Normal v) /\ Outcome F d H types TUnit B res).
    { intros j0 d0 g0 st0 xs ->. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I]. }
    destruct j as [|j]; [right; exact (Fuel0 0 d g st _ eq_refl)|].
    destruct (Step r Lr) as [Same [FAv Run]].
    cbn [flat_map]. rewrite refile_split, <- app_assoc. cbn [app].
    (* the rest of the chain, from a state realizing C1 *)
    assert (Cont : forall j1 d1 H1 g1 st1, j1 <= j -> heap_extends H H1 -> S.MeaningsGrow m1 d d1 ->
              Pre r F1 C1 d1 types H1 g1 st1 ->
              (exists d' g' st' H' j', j' <= S j /\ heap_extends H H' /\ S.MeaningsGrow (m1 ++ m2)%list d d' /\
                 Pre r F2 C2 d' types H' g' st' /\
                 I.eval_bindings j1 P d1 g1 st1 (flat_map (A.refile pairs) bs ++ rest)%list = I.eval_bindings j' P d' g' st' rest) \/
              (exists res, I.eval_bindings j1 P d1 g1 st1 (flat_map (A.refile pairs) bs ++ rest)%list = inr res /\
                 (forall v, I.evaluation_signal res <> Normal v) /\ Outcome F d H types TUnit B res)).
    { intros j1 d1 H1 g1 st1 Lj1 E1 M1 Pre1.
      destruct (IH r j1 d1 types H1 g1 st1 rest Lr ltac:(lia) ltac:(lia) Pre1)
        as [[d' [g' [st' [H' [j' [Lj' [E' [M' [Pre' Eq']]]]]]]]]|[res [Eq [NN Out]]]].
      - left. exists d', g', st', H', j'. split; [lia|]. split; [exact (S.heap_extends_trans _ _ _ E1 E')|].
        split; [exact (S.meanings_grow_trans _ _ _ _ _ M1 M')|]. split; [exact Pre'|exact Eq'].
      - right. exists res. split; [exact Eq|]. split; [exact NN|].
        exact (outcome_after F F1 d d1 H H1 types m1 B1 B2 B res Same FAv E1 M1 Imp Out). }
    destruct (method_or_not b) as [[t [m [th [f [ty [sm ->]]]]]]|NotM].
    2: { rewrite (extras_other pairs b NotM). cbn [app].
         destruct (Run j d types H g st ltac:(lia) ltac:(lia) Pre0 (flat_map (A.refile pairs) bs ++ rest)%list)
           as [[d1 [g1 [st1 [H1 [E1 [M1 [Pre1 Eq1]]]]]]]|[res [Eq [NN Out]]]].
         - rewrite Eq1. exact (Cont j d1 H1 g1 st1 (le_n _) E1 M1 Pre1).
         - right. exists res. split; [exact Eq|]. split; [exact NN|].
           eapply S.O.outcome_breaks; [|exact Out]. tauto. }
    set (es := refile_extras pairs (BMethod t m th f ty sm)).
    (* the method bind: its capture is selected, or the step stops *)
    destruct (select_provisions d sm) as [dc|] eqn:Sel.
    2: { destruct (Run j d types H g st ltac:(lia) ltac:(lia) Pre0 (es ++ flat_map (A.refile pairs) bs ++ rest)%list)
           as [[d1 [g1 [st1 [H1 [E1 [M1 [Pre1 Eq1]]]]]]]|[res [Eq [NN Out]]]].
         - (* a completed step from a stuck bind: impossible with the run's own rest *)
           destruct (Run j d types H g st ltac:(lia) ltac:(lia) Pre0 [])
             as [[d2 [g2 [st2 [H2 [_ [_ [_ Eq2]]]]]]]|[res [Eq [_ Out]]]].
           + rewrite (S.bindings_method_eq Prog argv read), Sel in Eq2.
             destruct j; cbn in Eq2; discriminate.
           + rewrite (S.bindings_method_eq Prog argv read), Sel in Eq. injection Eq as <-. contradiction.
         - right. exists res. split; [exact Eq|]. split; [exact NN|].
           eapply S.O.outcome_breaks; [|exact Out]. tauto. }
    rewrite (S.bindings_method_eq Prog argv read), Sel.
    set (dI := install_provision (IMth t m th) (FunctionClosure f (typed_capture d ty dc)) d).
    destruct j as [|j]; [right; exact (Fuel0 0 dI g st _ eq_refl)|].
    (* the realized state after the bind, read off its run with nothing after *)
    destruct (Run (S j) d types H g st ltac:(lia) ltac:(lia) Pre0 [])
      as [[d1 [g1 [st1 [H1 [E1 [M1 [Pre1 Eq1]]]]]]]|[res [Eq [_ Out]]]].
    2: { rewrite (S.bindings_method_eq Prog argv read), Sel in Eq. discriminate. }
    rewrite (S.bindings_method_eq Prog argv read), Sel in Eq1. cbn in Eq1. injection Eq1 as <- <- <-.
    assert (Shape : forall b', In b' es -> exists t0 m0 th0, b' = BMethod t0 m0 th0 f ty sm /\ FreeKey C1 (IMth t0 m0 th0)).
    { intros b' Ib. destruct (extras_shape pairs t m th f ty sm b' Ib) as [t0 [th0 ->]].
      exists t0, m, th0. split; [reflexivity|]. exact (NoSh t0 m th0 f ty sm Ib). }
    destruct (extras_run r F1 C1 types H1 f ty sm es Shape (S j) dI g st (flat_map (A.refile pairs) bs ++ rest)%list
      Pre1 (select_install sm d _ _ dc Sel)) as [[d2 [j2 [Ej [Em [Pre2 Eq2]]]]]|Out].
    + rewrite Eq2. apply (Cont j2 d2 H1 g st ltac:(lia) E1); [|exact Pre2].
      destruct M1 as [w [Ew Iw]]. exists w. split; [rewrite Em; exact Ew|exact Iw].
    + right. rewrite Out. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I].
Qed.

(* ---- the obligation -------------------------------------------------------- *)

Definition refile_pairs (Cb : A.cx) (outs : list nat) (gen : nat -> nat) (Cr : A.cx) (ks : list item) :=
  map (fun k0 => (A.at_site Cb (A.output_renaming outs gen k0), A.output_renaming outs gen (A.rkey_of Cr k0))) ks.

(* REMAINING (T15). A linked body's chain never shadows with refile: after
   each method bind, no provision of the context it leaves is filed under
   one of the keys refile installs that method again under. *)
Definition RefileShadowOk : Prop := forall J' k u gg h dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0 B,
  S.LinkData W Prog u gg h dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0 ->
  Chain J' k Fg Cg Fb Cb B made binds0 ->
  RChain J' k (refile_pairs Cb outs gen Cr ks) Fg Cg Fb Cb B made binds0.

Theorem refile_run_ok : RefileShadowOk -> S.RefileRunOk W Prog argv read state_ok.
Proof.
  intros Sh J' k u gg h dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0 B LD Ch
    rr j' d H st Lr Lj LJ Pre0.
  pose proof (refile_rchain_run _ _ _ _ _ _ _ _ _ _ (Sh J' k u gg h dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0 B LD Ch)
    rr j' d [] H [] st [] Lr Lj LJ Pre0) as Run.
  unfold refile_pairs in Run. rewrite app_nil_r in Run.
  destruct Run as [[d' [g' [st' [H' [j0 [_ [E [_ [Pre' Eq]]]]]]]]]|[res [Eq [NN Out]]]].
  - destruct j0 as [|j0].
    + right. rewrite Eq. eexists. split; [reflexivity|]. split; [intros v E0; discriminate|exact Logic.I].
    + left. exists d', g', st', H'. split; [exact E|]. split; [exact Pre'|]. rewrite Eq. reflexivity.
  - right. exists res. split; [exact Eq|]. split; [exact NN|exact Out].
Qed.

End Refiled.
End Refile.
