(* A used generic tag payload may become nominal only after instantiation. *)
From Stdlib Require Import List Arith Bool.
From NM Require Import CoreMoss.
Import ListNotations.

Module ChoicePayload.
Definition Sg : gsig := mkGsig
  (fun t => Nat.eqb t 3)
  (fun n => if Nat.eqb n 4 then Some ([ITy 3],TUnion [TAbs 3;TNom 1 []])
            else if Nat.eqb n 1 || Nat.eqb n 2 then Some ([],TUnit) else None)
  (fun _ => None) (fun _ => None) (fun _ => None) (fun _ => None)
  (fun _ => []) (fun _ => []) (fun _ => ([],[])).

Lemma template_scoped :
    incl (ty_frees (TUnion [TAbs 3;TNom 1 []])) (tyreqs [ITy 3]).
Proof. apply incl_refl. Qed.

Lemma signature_well_formed : wf_gsig Sg.
Proof.
  constructor.
  - intros n D tau H. cbn in H.
    destruct (Nat.eqb n 4); [injection H as <- <-|].
    + exists (mkCtx [ITy 3] []). split.
      * eapply WCons; [constructor; [reflexivity|discriminate]|reflexivity|constructor].
      * exact template_scoped.
    + destruct (Nat.eqb n 1 || Nat.eqb n 2); [injection H as <- <-|discriminate].
      exists empty_ctx. split; [constructor|apply incl_refl].
  - intros v D tau H. discriminate H.
  - intros f D signature body H. discriminate H.
  - intros m D signature H. discriminate H.
  - intros u Di Do body H. discriminate H.
  - intros n D Impossible;discriminate Impossible.
Qed.

Lemma template_not_instance_well_formed :
    ~ wf_ty Sg (mkCtx [ITy 3] []) (TUnion [TAbs 3;TNom 1 []]).
Proof.
  intro H. inversion H; subst.
  match goal with Hnom : Forall is_nom _ |- _ =>
    inversion Hnom; subst
  end.
  match goal with Hbad : is_nom (TAbs 3) |- _ =>
    destruct Hbad as [n [args E]]; discriminate E
  end.
Qed.
End ChoicePayload.
