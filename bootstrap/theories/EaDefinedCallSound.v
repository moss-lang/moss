(* Both defined functions and defined attached methods use this checked
   call preparation. Argument elaboration takes place afterwards. *)
From Stdlib Require Import List String Bool.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaCall EaFamily EaExpr EaTypes EaTypesSound.
From NM Require EaCallSound EaSatisfiesSound PublicCallRealize PublicContextFacts.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Lemma ilist_eqb_exact : forall a b, ilist_eqb a b = true <-> a = b.
Proof.
  induction a as [|x a IH]; intros [|y b]; cbn [ilist_eqb]; try (split; congruence).
  rewrite andb_true_iff, PublicCallRealize.item_eqb_spec, IH. split; intuition congruence.
Qed.

Definition DemandSound (E : env) : Prop :=
  forall C wanted wrapper called,
  PublicContextFacts.CxSection C ->
  demand E C wanted = Ok (wrapper, called) ->
  exists steps, Demand (e_W E) C wanted [] C steps called /\
    PublicContextFacts.CxSection called /\
    forall body, wrapper body = translate_demands C steps body.

Theorem check_defined_call_sound : forall E f C this checked,
  DemandSound E ->
  PublicContextFacts.CxSection C ->
  (forall formal needs, w_context (e_W E) f = Some (formal, needs) ->
    CtxOf (e_W E) f formal needs) ->
  check_defined_call E f C this = Ok checked ->
  exists formal needs steps called,
    CtxOf (e_W E) f formal needs /\
    RequirementsInForce C formal /\
    Demand (e_W E) C (map (at_th C (prepared_types checked)) needs) [] C steps called /\
    CallTheta (e_W E) called formal f (prepared_types checked) /\
    NeedsSatisfied called formal (prepared_types checked) needs /\
    SigOf (e_W E) f (c_ty called) this (prepared_signature checked) /\
    forall args, emit_defined_call f checked args =
      translate_demands C steps (translate_call called formal f (prepared_types checked) needs args).
Proof.
  intros E f C this checked DemandCheck Caller Context Run.
  unfold check_defined_call in Run. cbv zeta in Run.
  apply rbind_ok in Run as ([formal needs] & Read & Run).
  destruct (w_context (e_W E) f) as [[Cf ns]|] eqn:Found; [|discriminate Read].
  injection Read as <- <-.
  pose proof (Context _ _ eq_refl) as Callee.
  destruct (PublicContextFacts.function_context_merge _ _ _ _ Callee) as [Solved Section].
  apply rbind_ok in Run as ([] & Rule & Run).
  apply rbind_ok in Run as (initial & Initial & Run).
  apply rbind_ok in Run as ([wrapper called] & DemandRun & Run).
  apply rbind_ok in Run as (types & Types & Run).
  apply rbind_ok in Run as (agreed & Agree & Run).
  destruct (ilist_eqb (map (at_th C initial) ns) (map (at_th C types) ns)) eqn:Wanted;
    [|discriminate Agree].
  apply ilist_eqb_exact in Wanted.
  apply rbind_ok in Run as (sm & CheckedNeeds & Run).
  apply rbind_ok in Run as (sig & Signature & Run).
  injection Run as <-.
  destruct (DemandCheck _ _ _ _ Caller DemandRun) as (steps & Demands & Called & Wrapper).
  apply EaSatisfiesSound.satisfies_sound in CheckedNeeds;
    [|eapply EaSatisfiesSound.ctxof_subst_functional; exact Callee].
  destruct CheckedNeeds as [Provisions Satisfied].
  exists Cf, ns, steps, called. cbn [prepared_types prepared_signature].
  split; [exact Callee|split; [eapply EaCallSound.rule_b_sound; eassumption|]].
  split; [now rewrite <- Wanted|split; [eapply EaCallSound.call_theta_sound; eassumption|]].
  split; [exact Satisfied|split; [eapply sig_of_sound; exact Signature|]].
  intro args. unfold emit_defined_call, translate_call. cbn [prepared_demands prepared_types prepared_provisions].
  rewrite Provisions. apply Wrapper.
Qed.
