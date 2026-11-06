(* Family demands restore types and preserve the caller's merge substitution. *)
From Stdlib Require Import List.
From NM Require Import Audit.
Module A:=Audit.Elaboration.
Lemma family_step_types:forall W C0 wanted C u matching th sm C',
 A.FamilyStep W C0 wanted C u matching th sm C'->A.c_ty C'=A.c_ty C/\A.c_sub C'=A.c_sub C.
Proof.
 intros W C0 wanted C u matching th sm C' H;destruct H;split;[reflexivity|].
 match goal with R:A.ProvideResult _ _ _ _ _ _|-_=>destruct R as(d&ps&args&res&body&outs&es&tm&Decl&Parts&Out&Read&Keys&Final);rewrite Final end.
 change(A.c_sub(fold_left(fun C q=>A.set_ty C(fst q)(snd q))matching C)=A.c_sub C).
 clear;revert C;induction matching as[|q rest IH];intro C;[reflexivity|cbn[fold_left];rewrite IH;reflexivity].
Qed.
Lemma demand_types:forall W C0 wanted used C wrap C',
 A.Demand W C0 wanted used C wrap C'->A.c_ty C'=A.c_ty C/\A.c_sub C'=A.c_sub C.
Proof.
 intros W C0 wanted used C wrap C' H;induction H;[split;reflexivity|].
 destruct(family_step_types _ _ _ _ _ _ _ _ _ H0)as[T S];destruct IHDemand as[T' S'];split;congruence.
Qed.
