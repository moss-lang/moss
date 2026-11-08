(* Regression controls for the lexical name visitor used by lambda capture. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel Sx.
Import ListNotations.
Local Open Scope string_scope.
Module A:=ExecutionModel.Elaboration.
Lemma bracketed_path_is_not_local_mention:forall n brackets,
 ~A.Mentions_name(AEPath[n](Some brackets))n.
Proof. intros n brackets H;inversion H;subst;inversion H0. Qed.
Example if_initializer_mentions_outer:
 A.Mentions_name
 (AEIf(AEPath["condition"]None)(ABlock[ASLet "local" None(AEPath["outer"]None)]None)None)"outer".
Proof.
 eapply A.MNSub;[apply A.SubIfB;left;exists(ASLet "local" None(AEPath["outer"]None));split;[left;reflexivity|apply A.SSLet]|apply A.MNPath].
Qed.
