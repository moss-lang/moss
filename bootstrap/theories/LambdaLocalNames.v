(* The real lambda frame-name check protects the implicit receiver slot. *)
From Stdlib Require Import List String Bool.
From NM Require Import ReifyData Audit NMap Sx Elab.
Import ListNotations.
Local Open Scope string_scope.
Lemma lambda_duplicate_none_nodup:forall names,
 duplicate_local_name names=None->NoDup names.
Proof.
 induction names as[|n names IH];intro Empty;[constructor|].
 cbn[duplicate_local_name]in Empty;destruct(smem n names)eqn:Present;[discriminate|].
 constructor;[|apply IH;exact Empty].
 clear IH Empty;induction names as[|x names IH];[intros[]|].
 cbn[smem]in Present;apply orb_false_iff in Present as[Ne Rest].
 intros[Eq|Member];[subst x;rewrite String.eqb_refl in Ne;discriminate|exact(IH Rest Member)].
Qed.
Lemma lambda_local_names_checked:forall mi names s value final,
 check_lambda_local_names mi names s=Good(value,final)->final=s/\NoDup names.
Proof.
 intros mi names s value final Run;unfold check_lambda_local_names in Run.
 destruct(duplicate_local_name names)as[bad|]eqn:Names;[discriminate|].
 injection Run as <- <-;split;[reflexivity|apply lambda_duplicate_none_nodup;exact Names].
Qed.
Example explicit_parameter_cannot_alias_receiver:
 forall s,check_lambda_local_names 0["this";"this"]s=
 err 0 "anonymous function local `this` occurs twice among its receiver, parameters or captures (D70)" s.
Proof. reflexivity. Qed.
Example ordinary_receiver_parameter_names:forall s,
 check_lambda_local_names 0["this";"argument"]s=Good(tt,s).
Proof. reflexivity. Qed.
Example plain_lambda_parameter_names:forall s,
 check_lambda_local_names 0["argument"]s=Good(tt,s).
Proof. reflexivity. Qed.
Definition lambda_name_control_world:world:=World NE[]0 NE 1 NE None.
Definition lambda_name_control_state:st:=
 St lambda_name_control_world NE 1 NE NE NE[][]NE[][]NE NE[]0 false false[][][]
 (empty_reify_metadata ety key).
Definition lambda_name_control_sig:fsig:=FSig[("argument",YUnit)]YUnit(Some YUnit).
Definition lambda_name_control_context:cx:=Cx 0 lambda_name_control_sig None 0[]""[][].
Definition lambda_name_control_bound:gsym:=
 GSym 0 0 "method" KMethod(ADFn RDetached "method"[("argument",ATUnit)]None None)None[]OSource.
Example raw_lambda_receiver_parameter_rejected:
 exists message,
 lambda_provider 8 lambda_name_control_context empty_env[("this",None)]None
  (AEPath["this"]None)lambda_name_control_bound lambda_name_control_sig(Some YUnit)"control"
  lambda_name_control_state=Bad message.
Proof. vm_compute;eexists;reflexivity. Qed.
Example ordinary_raw_lambda_receiver_accepted:
 exists pid pre sm final,
 lambda_provider 8 lambda_name_control_context empty_env[("argument",None)]None
  AEUnit lambda_name_control_bound lambda_name_control_sig(Some YUnit)"control"
  lambda_name_control_state=Good((pid,pre,sm),final).
Proof. vm_compute;eauto. Qed.
Example raw_captured_local_cannot_alias_receiver:
 exists message,
 lambda_provider 8 lambda_name_control_context(set_local empty_env "this" YUnit LLet)[]None
  (AEPath["this"]None)(GSym 0 0 "method0" KMethod(ADFn RDetached "method0"[]None None)None[]OSource)(FSig[]YUnit(Some YUnit))(Some YUnit)"control"
  lambda_name_control_state=Bad message.
Proof. vm_compute;eexists;reflexivity. Qed.
Example ordinary_plain_raw_lambda_accepted:
 exists pid pre sm final,
 lambda_provider 8 lambda_name_control_context empty_env[("argument",None)]None
  AEUnit lambda_name_control_bound(FSig[("argument",YUnit)]YUnit None)None"control"
  lambda_name_control_state=Good((pid,pre,sm),final).
Proof. vm_compute;eauto. Qed.
