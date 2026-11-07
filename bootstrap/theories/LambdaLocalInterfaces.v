(* Local binding order cannot alter the provision interface or its runtime keys. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab LambdaTrace LambdaCapturedLocals LambdaCaptureTables
 LambdaBodyContext LambdaLocalOrder.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma lambda_local_rows_keys:forall rows e,
 all_keys(fold_left(fun en q=>set_local en(fst q)(fst(snd q))(snd(snd q)))rows e)=all_keys e.
Proof. intro rows;induction rows as[|[n[t mu]]rows IH];intro e;[reflexivity|cbn[fold_left fst snd];rewrite IH;reflexivity]. Qed.
Lemma lambda_captured_keys:forall(lets:list(string*nat*ety))le,
 all_keys(fold_left(fun en q=>let '(n,_,t) :=q in set_local en n t LCaptured)lets le)=all_keys le.
Proof. intro lets;induction lets as[|[[n v]t]lets IH];intro le;[reflexivity|cbn[fold_left];rewrite IH;reflexivity]. Qed.
Lemma lambda_own_env_keys:forall ps sg this le lets,
 all_keys(lambda_own_env ps sg this le lets)=all_keys le.
Proof. intros;rewrite lambda_own_env_rows;rewrite !lambda_local_rows_keys;reflexivity. Qed.
Lemma lambda_added_locals_key:forall rows C key,
 A.rkey_of(lambda_add_locals rows C)key=A.rkey_of C key.
Proof. intros;rewrite lambda_locals_shape;destruct key;reflexivity. Qed.
Lemma lambda_parameters_key:forall Fl original ps sg C key,
 A.rkey_of(lambda_parameter_model Fl original ps sg C)key=A.rkey_of C key.
Proof.
 intros;rewrite <-(lambda_typed_parameter_model Fl original original ps sg C eq_refl).
 apply lambda_added_locals_key.
Qed.
