(* Lambda capture lists enumerate provision identities; their method
   instantiations live in the satisfaction map, not in the key set. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel Sx Elab CtxMerge.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Lemma lambda_source_key_identity:forall Fl key,
 key_tr Fl key=ExecutionModel.Elaboration.provision_id(key_tr Fl key).
Proof. intros Fl key;destruct key;reflexivity. Qed.
Lemma lambda_source_keys_identity:forall Fl keys,
 Forall(fun key=>key=ExecutionModel.Elaboration.provision_id key)(map(key_tr Fl)keys).
Proof. intros Fl keys;apply Forall_map,Forall_forall;intros key Hit;apply lambda_source_key_identity. Qed.
Example method_provision_identity:forall receiver mid theta,
 IMth receiver mid theta=ExecutionModel.Elaboration.provision_id(IMth receiver mid theta)<->theta=[].
Proof. intros;split;[intro E;injection E;auto|intro E;subst theta;reflexivity]. Qed.
Example method_argument_is_not_another_provision:
 let C:=A.Cx[][][][][((TUnit,7),A.FSig[]TUnit)][][][]in
 A.provides C(IMth TUnit 7[(1,TUnit)])/\
 IMth TUnit 7[(1,TUnit)]<>ExecutionModel.Elaboration.provision_id(IMth TUnit 7[(1,TUnit)]).
Proof. split;[discriminate|discriminate]. Qed.
