(* The body of an anonymous function can infer a strict member of its
   checked result union; its frame retains the declared result type. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel Sx.
Import ListNotations.
Local Open Scope string_scope.
Module A:=ExecutionModel.Elaboration.
Lemma lambda_union_member_fits:forall left right,
 A.fits(TNom left[])(TUnion[TNom left[];TNom right[]]).
Proof.
 intros;right;right;exists[TNom left[];TNom right[]];split;[reflexivity|].
 intros m[Eq|[]];subst m;left;reflexivity.
Qed.
Example lambda_union_member_body:forall W P F C left right,
 A.resolve_sym W(A.fr_mod F)["Left"]=Some left->A.kind_sym W left=Some A.DKUnit->
 A.slook "Left"(A.c_locals C)=None->
 A.BlkE W P F C(ABlock[](Some(AEPath["Left"]None)))
  (Some(TUnion[TNom left[];TNom right[]]))(XTag left[]XUnit)(TNom left[]).
Proof.
 intros W P F C left right Res Kind Local.
 eapply A.BlkECheck with(made:=[]).
 - apply A.BlkTail;eapply A.ExUnitTag;[|exact Res|exact Kind].
   intros n Eq;injection Eq as <-;exact Local.
 - intros g[].
 - apply lambda_union_member_fits.
Qed.
Example lambda_union_member_is_not_declared_union:forall left right,
 TNom left[]<>TUnion[TNom left[];TNom right[]].
Proof. intros;discriminate. Qed.
