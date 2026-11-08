(* Pure substitution of a declared signature reading. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel TyOfSubst.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Lemma sigof_subst:forall W sb f tm th sg,
 A.SigOf W f tm th sg ->forall tm',SubRel sb tm tm' ->
 A.SigOf W f tm'(option_map(ExecutionModel.app_subst sb)th)
   (A.FSig(map(ExecutionModel.app_subst sb)(A.sg_params sg))(ExecutionModel.app_subst sb(A.sg_ret sg))).
Proof.
 intros W sb f tm th sg(d &recv &nm &ps &rt &body &Hd &Da &Fp &Fr)tm' SR.
 exists d,recv,nm,ps,rt,body. split;[exact Hd|]. split;[exact Da|]. split.
 - cbn[A.sg_params]. clear Fr Da. induction Fp as[|p t ps ts Ty Fp IH];cbn[map].
   + constructor.
   + constructor;[exact(tyof_subst W sb _ _ _ _ _ Ty tm' SR)|exact IH].
 - cbn[A.sg_ret]. destruct rt;[exact(tyof_subst W sb _ _ _ _ _ Fr tm' SR)|rewrite Fr;reflexivity].
Qed.
