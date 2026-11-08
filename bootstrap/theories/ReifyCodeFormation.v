(* Physical code signature formation comes from the actual checked source
   signature, including the receiver, under immutable generation meanings. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel CoreMoss Elab BodyTr ReifyData Reify ReifySource
 ReifySourceTypeFormation ReifySigFormation.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition source_checked_signature(source:st)(signature:reify_signature ety):A.fsig:=
 A.FSig(map(tr(reify_field_number source))(rsg_params ety signature))
   (tr(reify_field_number source)(rsg_result ety signature)).
Theorem checked_source_code_signature_formed : forall W Sg source runtime signature
    owner meanings Phi,
 source_nominal_schemas W Sg->(forall n,NoDup(A.w_reqs W n))->g_tag Sg rec_nom=None->
 A.SigOf W owner meanings
   (option_map(tr(reify_field_number source))(rsg_receiver ety signature))
   (source_checked_signature source signature)->
 source_meanings_formed Sg(rf_generations runtime)Phi meanings->
 (forall receiver,rsg_receiver ety signature=Some receiver->
   wf_ty Sg Phi(reify_source_type source runtime receiver))->
 Forall(wf_ty Sg Phi)(fs_params(reify_source_code_signature source runtime signature))/\
 wf_ty Sg Phi(fs_ret(reify_source_code_signature source runtime signature)).
Proof.
 intros W Sg source runtime signature owner meanings Phi Schemas Unique RecordHead
   Checked Meanings Receiver.
 destruct Checked as[d[rcv[name[parameters[result[body[Decl[Ast[Parameters Result]]]]]]]]].
 assert(Pars:Forall(wf_ty Sg Phi)
   (map(reify_source_type source runtime)(rsg_params ety signature))).
 { unfold reify_source_type;rewrite <-map_map.
   cbn[source_checked_signature A.sg_params]in Parameters.
   remember(map(tr(reify_field_number source))(rsg_params ety signature))as types eqn:Types in Parameters|-*.
   clear Types Ast Decl. induction Parameters as[|parameter tau parameters taus Typed Rest IH].
   - constructor.
   - cbn;constructor;[|exact IH].
     eapply(@source_type_formed W Sg(rf_generations runtime)Schemas Unique RecordHead);
       [exact Typed|exact Meanings|].
     intros t HasReceiver.
     destruct(rsg_receiver ety signature)as[raw|]eqn:E;[|discriminate].
     injection HasReceiver as <-;apply Receiver;reflexivity. }
 assert(Ret:wf_ty Sg Phi(reify_source_type source runtime(rsg_result ety signature))).
 { destruct result as[syntax|].
   - eapply(@source_type_formed W Sg(rf_generations runtime)Schemas Unique RecordHead);
       [exact Result|exact Meanings|].
     intros t HasReceiver.
     destruct(rsg_receiver ety signature)as[raw|]eqn:E;[|discriminate].
     injection HasReceiver as <-;apply Receiver;reflexivity.
   - change(tr(reify_field_number source)(rsg_result ety signature)=TUnit)in Result.
     unfold reify_source_type;rewrite Result;constructor. }
 split;[|exact Ret].
 cbn[reify_source_code_signature reify_source_signature fs_params].
 apply Forall_app;split;[|exact Pars].
 destruct(rsg_receiver ety signature)as[raw|]eqn:E;constructor;[apply Receiver;reflexivity|constructor].
Qed.

Lemma registered_signature_is_the_checked_canonical_signature : forall source environment signature,
 source_checked_signature source(reify_sig environment signature)=
 A.FSig(map(fun p=>tr(reify_field_number source)(canon environment(snd p)))(sg_params signature))
   (tr(reify_field_number source)(canon environment(sg_ret signature))).
Proof.
 intros;unfold source_checked_signature,reify_sig;cbn[rsg_params rsg_result].
 rewrite map_map;reflexivity.
Qed.

Theorem checked_source_function_row_formed : forall W Sg source runtime signature
    owner meanings inputs Phi (code:fsym),
 source_nominal_schemas W Sg->(forall n,NoDup(A.w_reqs W n))->g_tag Sg rec_nom=None->
 wf_tele Sg empty_ctx inputs Phi->
 A.SigOf W owner meanings
   (option_map(tr(reify_field_number source))(rsg_receiver ety signature))
   (source_checked_signature source signature)->
 source_meanings_formed Sg(rf_generations runtime)Phi meanings->
 (forall receiver,rsg_receiver ety signature=Some receiver->
   wf_ty Sg Phi(reify_source_type source runtime receiver))->
 formed_reify_declaration Sg
   (IFn code,RDFunction inputs(reify_source_code_signature source runtime signature)).
Proof.
 intros W Sg source runtime signature owner meanings inputs Phi code Schemas Unique RecordHead
   Inputs Checked Meanings Receiver.
 exists Phi;split;[exact Inputs|].
 eapply checked_source_code_signature_formed;eassumption.
Qed.
