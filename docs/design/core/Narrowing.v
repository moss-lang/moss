(* ------------------------------------------------------------------------ *)
(* Narrowing.v — a nominal's identity is its relevant parameters [D71].     *)
(*                                                                          *)
(* CoreMoss.v, following D21, makes TNom n θ total over every type          *)
(* requirement of n's telescope.  The language (D71) and both compilers     *)
(* (bootstrap/mossc/lower.py `type_requirements`, src/types.moss `reqs_of`) *)
(* parameterize a tag only by its *relevant* type symbols — those its       *)
(* payload mentions, closed through the nominals it mentions, as a least    *)
(* fixpoint; a unit by none — and make binding any other symbol an error.   *)
(* This file shows that narrowing is sound for the calculus:                *)
(*                                                                          *)
(*  Part 1. Relevance R, the normalizer `narrow R` that drops irrelevant    *)
(*          entries, and the semantic heart: value typing cannot tell two   *)
(*          types with one normal form apart (vty_narrow_invariant), given  *)
(*          that R is closed (what a payload mentions, after narrowing, is  *)
(*          relevant).                                                      *)
(*  Part 2. The narrowed calculus *is* the calculus, at a signature whose   *)
(*          tag telescopes are exactly their relevant type symbols: in such *)
(*          a signature every well-formed type is already in normal form    *)
(*          (wf_ty_narrowed), and type soundness is an instance of the      *)
(*          existing soundness_main (narrowed_soundness).                   *)
(*                                                                          *)
(* What it depends on is in KeyOnly.v: the core is protected because a      *)
(* method item carries the method's own application θ, and availability/ς   *)
(* compare it.  Matching method requirements by the key (receiver, method)  *)
(* alone — the rule both compilers used before D71 — is unsound: a concrete *)
(* program is well-typed under key-only matching and evaluates to Err       *)
(* (keyonly_counterexample); the real core rejects it                       *)
(* (counterexample_rejected_by_core).  Under D21 (unnarrowed) identity the  *)
(* receiver key alone tells the two instantiations apart                    *)
(* (d21_key_distinguishes); under narrowing it does not                     *)
(* (narrowed_key_conflates) — which is why the compilers check coherence    *)
(* on (receiver, method, θ).                                                *)
(*                                                                          *)
(* Built by the flake check core-calculus-rocq after CoreMoss.v, with the   *)
(* logical prefix NM; its Print Assumptions lines are audited as            *)
(* CoreMoss.v's §13 are.                                                    *)
(* ------------------------------------------------------------------------ *)

From NM Require Import CoreMoss.
From Stdlib Require Import List Bool Arith.
Import ListNotations.

Set Implicit Arguments.

(* ===================== Part 1. Relevance and normal forms ================ *)

Definition relmap := nsym -> list tsym.

Definition memb (t : tsym) (l : list tsym) : bool := existsb (Nat.eqb t) l.

(* Drop every entry of a nominal reference that is not relevant, recursively. *)
Fixpoint narrow (R : relmap) (tau : ty) {struct tau} : ty :=
  match tau with
  | TUnit => TUnit
  | TAbs t => TAbs t
  | TNom n th =>
      TNom n ((fix go (l : list (tsym * ty)) : list (tsym * ty) :=
                 match l with
                 | [] => []
                 | p :: r => if memb (fst p) (R n)
                             then (fst p, narrow R (snd p)) :: go r
                             else go r
                 end) th)
  | TUnion ms =>
      TUnion ((fix go (l : list ty) : list ty :=
                 match l with
                 | [] => []
                 | t' :: r => narrow R t' :: go r
                 end) ms)
  end.

Definition keep (R : relmap) (n : nsym) (th : subst) : subst :=
  filter (fun p => memb (fst p) (R n)) th.

Lemma narrow_nom : forall R n th,
  narrow R (TNom n th) = TNom n (map_snd (narrow R) (keep R n th)).
Proof.
  intros R n th. simpl. f_equal. unfold keep, map_snd.
  induction th as [| [t tau] r IH]; simpl; [reflexivity |].
  destruct (memb t (R n)); simpl; rewrite IH; reflexivity.
Qed.

Lemma narrow_union : forall R ms,
  narrow R (TUnion ms) = TUnion (map (narrow R) ms).
Proof.
  intros R ms. simpl. f_equal.
Qed.

(* Two types are identified by the narrowed reading iff they have one
   normal form. *)
Definition narrow_eq (R : relmap) (t1 t2 : ty) : Prop := narrow R t1 = narrow R t2.

(* Narrowing is a substitution homomorphism. *)
Lemma narrow_app_subst : forall R s tau,
  narrow R (app_subst s tau) = app_subst (map_snd (narrow R) s) (narrow R tau).
Proof.
  intros R s tau. induction tau using ty_ind'.
  - reflexivity.
  - simpl. rewrite lookup_map_snd. destruct (lookup_subst t s); reflexivity.
  - rewrite app_subst_nom, !narrow_nom, app_subst_nom. f_equal.
    unfold keep, map_snd. induction H as [| [t tau] r Hp Hr IH]; simpl;
      [reflexivity |].
    simpl in Hp. destruct (memb t (R n)); simpl.
    + rewrite Hp. f_equal. exact IH.
    + exact IH.
  - rewrite app_subst_union, !narrow_union, app_subst_union. f_equal.
    induction H as [| t r Hp Hr IH]; simpl; [reflexivity |].
    rewrite Hp. f_equal. exact IH.
Qed.

(* R is closed under the signature: whatever a tag's payload mentions,
   read with nested references narrowed, is relevant to that tag.  (The
   least such R is the intended "symbols the payload mentions" — note it is
   a fixpoint through nested and recursive references.) *)
Definition rel_closed (Sg : gsig) (R : relmap) : Prop :=
  forall n D taup, g_tag Sg n = Some (D, taup) ->
    forall t, In t (ty_frees (narrow R taup)) -> In t (R n).

Lemma lookup_keep : forall R n th t,
  memb t (R n) = true ->
  lookup_subst t (keep R n th) = lookup_subst t th.
Proof.
  intros R n th t Hm. unfold keep.
  induction th as [| [t' tau] r IH]; simpl; [reflexivity |].
  destruct (Nat.eqb t t') eqn:E.
  - apply Nat.eqb_eq in E; subst t'. rewrite Hm. simpl.
    rewrite Nat.eqb_refl. reflexivity.
  - destruct (memb t' (R n)); simpl; [rewrite E |]; exact IH.
Qed.

Lemma memb_In : forall t l, memb t l = true <-> In t l.
Proof.
  intros t l. unfold memb. rewrite existsb_exists. split.
  - intros [x [Hx E]]. apply Nat.eqb_eq in E. subst. exact Hx.
  - intros H. exists t. split; [exact H | apply Nat.eqb_refl].
Qed.

(* The semantic heart: value typing respects the narrowed identification.
   A tag value's typing consults its reference only through the payload,
   and the payload mentions only relevant symbols. *)
Theorem vty_narrow_invariant : forall Sg R,
  rel_closed Sg R ->
  forall w t1, vty Sg w t1 -> forall t2, narrow_eq R t1 t2 -> vty Sg w t2.
Proof.
  intros Sg R Hcl w t1 H. unfold narrow_eq.
  induction H as [| n D taup th w Htag Hw IH | w tau' ms Hw IH Hin];
    intros t2 Heq.
  - destruct t2; simpl in Heq; try discriminate. constructor.
  - destruct t2 as [| | n2 th2 | ms2];
      rewrite narrow_nom in Heq; try (simpl in Heq; discriminate).
    rewrite narrow_nom in Heq. injection Heq as En Eth. subst n2.
    econstructor; [exact Htag |]. apply IH.
    rewrite !narrow_app_subst. apply app_subst_frees_agree.
    intros t Ht. pose proof (Hcl _ _ _ Htag t Ht) as HR.
    apply memb_In in HR.
    rewrite !lookup_map_snd, <- (@lookup_keep R n th t HR), <- (@lookup_keep R n th2 t HR).
    rewrite <- !lookup_map_snd, Eth. reflexivity.
  - destruct t2 as [| | n2 th2 | ms2];
      rewrite narrow_union in Heq; try (simpl in Heq; discriminate).
    rewrite narrow_union in Heq. injection Heq as Ems.
    assert (Hm : In (narrow R tau') (map (narrow R) ms2))
      by (rewrite <- Ems; apply in_map; exact Hin).
    apply in_map_iff in Hm as [tau'' [E Hin2]].
    econstructor; [| exact Hin2]. apply IH. symmetry. exact E.
Qed.

(* Consequently the semantic types of a closed reference depend only on its
   relevant entries: identifying TNom n θ₁ with TNom n θ₂ when they agree
   on R n (up to ≈) loses nothing at the level of values. *)
Corollary vty_nom_relevant_only : forall Sg R n th1 th2 w,
  rel_closed Sg R ->
  map_snd (narrow R) (keep R n th1) = map_snd (narrow R) (keep R n th2) ->
  vty Sg w (TNom n th1) -> vty Sg w (TNom n th2).
Proof.
  intros Sg R n th1 th2 w Hcl E H. eapply vty_narrow_invariant; [exact Hcl | exact H |].
  unfold narrow_eq. rewrite !narrow_nom, E. reflexivity.
Qed.

(* ===================== Part 2. The narrowed calculus ===================== *)

(* A narrowed signature: every tag's telescope is exactly its relevant type
   symbols (units: none), without duplicates, and closed. *)
Definition narrowed_sig (Sg : gsig) (R : relmap) : Prop :=
  rel_closed Sg R /\
  forall n D taup, g_tag Sg n = Some (D, taup) ->
    D = map ITy (R n) /\ NoDup (R n).

(* A telescope of distinct type items merges to itself. *)
Lemma tele_ctx_tys : forall ts,
  NoDup ts ->
  tele_ctx (map ITy ts) = Some (mkCtx (map ITy ts) []).
Proof.
  intros ts Hnd. unfold tele_ctx.
  assert (G : forall pre, NoDup (pre ++ ts) ->
            merge_tele (mkCtx (map ITy pre) []) (map ITy ts)
            = Some (mkCtx (map ITy (pre ++ ts)) [])).
  { clear Hnd. induction ts as [| t r IH]; intros pre Hp; simpl.
    - rewrite app_nil_r. reflexivity.
    - unfold merge_item. simpl.
      assert (Hf : find_same_key [] (map ITy pre) (ITy t) = None).
      { assert (Hni : ~ In t pre).
        { intros Hin. apply NoDup_remove_2 in Hp. apply Hp.
          apply in_or_app. left. exact Hin. }
        clear -Hni. induction pre as [| u pre' IHp]; simpl; [reflexivity |].
        destruct (Nat.eqb u t) eqn:E.
        - apply Nat.eqb_eq in E. subst. exfalso. apply Hni. left. reflexivity.
        - apply IHp. intros H. apply Hni. right. exact H. }
      rewrite Hf. simpl.
      replace (map ITy pre ++ [ITy t]) with (map ITy (pre ++ [t]))
        by (rewrite map_app; reflexivity).
      rewrite (IH (pre ++ [t])).
      + rewrite <- app_assoc. reflexivity.
      + rewrite <- app_assoc. exact Hp. }
  exact (G [] Hnd).
Qed.

Lemma tyreqs_map_ITy : forall ts, tyreqs (map ITy ts) = ts.
Proof. induction ts as [| t r IH]; simpl; [reflexivity | rewrite IH; reflexivity]. Qed.

(* In a narrowed signature, the satisfaction of a tag's telescope forces the
   reference to list exactly the relevant symbols — so every well-formed
   type is its own normal form: the calculus at a narrowed signature *is*
   the narrowed reading, with TNom n θ₁ and TNom n θ₂ identified exactly
   when they are one type. *)
Lemma keep_all : forall R n th,
  map fst th = R n -> keep R n th = th.
Proof.
  intros R n th E. unfold keep. apply forallb_filter_id.
  apply forallb_forall. intros p Hp. rewrite memb_In, <- E.
  apply in_map. exact Hp.
Qed.

Theorem wf_ty_narrowed : forall Sg R Phi tau,
  narrowed_sig Sg R -> wf_ty Sg Phi tau -> narrow R tau = tau.
Proof.
  intros Sg R Phi tau [_ HN] H.
  apply (@wf_ty_ind' Sg Phi (fun t => narrow R t = t)); [| | | | exact H].
  - reflexivity.
  - reflexivity.
  - intros n D tau0 th Htag Hsat Hwf IH.
    destruct (HN _ _ _ Htag) as [ED Hnd]. subst D.
    destruct Hsat as [PhiD [Hctx [Hdom _]]].
    rewrite (tele_ctx_tys Hnd) in Hctx. injection Hctx as <-. simpl in Hdom.
    rewrite tyreqs_map_ITy in Hdom.
    rewrite narrow_nom, (keep_all R n th Hdom). f_equal.
    unfold map_snd. clear -IH.
    induction IH as [| [t ty0] r Hp Hr IHr]; simpl; [reflexivity |].
    simpl in Hp. rewrite Hp. f_equal. exact IHr.
  - intros ms Hwf IH Hnom Hnd. rewrite narrow_union. f_equal.
    clear -IH. induction IH as [| t r Hp Hr IHr]; simpl; [reflexivity |].
    rewrite Hp. f_equal. exact IHr.
Qed.

(* Type soundness of the narrowed calculus: an instance of soundness_main,
   which already quantifies over every signature.  Nothing in the
   metatheory needs a tag's telescope to be its full assume nesting. *)
Theorem narrowed_soundness :
  forall Sg R main body fuel,
    narrowed_sig Sg R ->
    wf_prog Sg main ->
    g_fn Sg main = Some ([], mkSig [] TUnit, Some body) ->
    interp fuel Sg [] [] body <> Err
    /\ (forall w, interp fuel Sg [] [] body = Ok w -> value_has_unit_ty w).
Proof.
  intros Sg R main body fuel _ Hwf Hmain. exact (soundness_main fuel Hwf Hmain).
Qed.

Print Assumptions vty_narrow_invariant.
Print Assumptions vty_nom_relevant_only.
Print Assumptions wf_ty_narrowed.
Print Assumptions narrowed_soundness.
