(* Abstract provision signatures carry only the type dependencies they
   actually read. Formation narrows to that real dependency telescope. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import CoreMoss MergeWf ReifyFormation ReifyInterfaces ReifySigFormation ReifyData ReifySig ReifySignatureSource ReifyNominalWorldSource.
Import ListNotations.
Lemma nominal_argument_frees_in : forall head arguments pair,
 In pair arguments->incl(ty_frees(snd pair))(ty_frees(TNom head arguments)).
Proof.
 intros head arguments;induction arguments as[|first rest IH];intros pair Member t Free;[contradiction|].
 cbn;destruct Member as[<-|Member];apply in_or_app;[left;exact Free|right;eapply IH;eauto].
Qed.
Lemma union_member_frees_in : forall members member,
 In member members->incl(ty_frees member)(ty_frees(TUnion members)).
Proof.
 intros members;induction members as[|first rest IH];intros member Member t Free;[contradiction|].
 cbn;destruct Member as[<-|Member];apply in_or_app;[left;exact Free|right;eapply IH;eauto].
Qed.
Theorem formed_type_on_free_scope : forall Sg Phi tau,
 tags_typeonly Sg->wf_ty Sg Phi tau->forall target,
 (forall t,In t(ty_frees tau)->avail target(ITy t))->wf_ty Sg target tau.
Proof.
 intros Sg Phi tau Tags Well;revert tau Well.
 apply(@wf_ty_ind' Sg Phi(fun tau=>forall target,
   (forall t,In t(ty_frees tau)->avail target(ITy t))->wf_ty Sg target tau)).
 - intros target Covered;constructor.
 - intros t Available target Covered;constructor;apply Covered;left;reflexivity.
 - intros n D payload args Tag Satisfied Values IH target Covered.
   eapply WfTag;[exact Tag| |].
   + eapply sat_typeonly;[eapply Tags;exact Tag|exact Satisfied|reflexivity].
   + apply Forall_forall;intros pair Member;apply Forall_forall with(x:=pair)in IH;[|exact Member].
     apply IH;intros t Free;apply Covered;eapply nominal_argument_frees_in;eauto.
 - intros members Values IH Nominals Unique target Covered.
   apply WfUnion;[|exact Nominals|].
   + apply Forall_forall;intros member Member;apply Forall_forall with(x:=member)in IH;[|exact Member].
     apply IH;intros t Free;apply Covered;eapply union_member_frees_in;eauto.
   + rewrite(heads_of_nom(cS target)members Nominals).
     rewrite(heads_of_nom(cS Phi)members Nominals)in Unique;exact Unique.
 - intros fields Record Values IH target Covered;eapply WfRec;[exact Record|].
   apply Forall_forall;intros pair Member;apply Forall_forall with(x:=pair)in IH;[|exact Member].
   apply IH;intros t Free;apply Covered;eapply nominal_argument_frees_in;eauto.
 - intros n D args NoPayload Opaque Satisfied Only Values IH target Covered.
   eapply WfOpaque;[exact NoPayload|exact Opaque| |exact Only|].
   + eapply sat_typeonly;[exact Only|exact Satisfied|reflexivity].
   + apply Forall_forall;intros pair Member;apply Forall_forall with(x:=pair)in IH;[|exact Member].
     apply IH;intros t Free;apply Covered;eapply nominal_argument_frees_in;eauto.
Qed.
Lemma value_type_on_dependency_scope : forall Sg Phi tau,
 tags_typeonly Sg->wf_ty Sg Phi tau->
 wf_ty Sg(mkCtx(value_type_inputs tau)[])tau.
Proof.
 intros Sg Phi tau Tags Typed;eapply formed_type_on_free_scope;[exact Tags|exact Typed|].
 intros t Free;apply context_type_is_available.
 unfold value_type_inputs;cbn;rewrite type_telescope_requirements;apply nodup_In;exact Free.
Qed.
Lemma signature_on_dependency_scope : forall Sg Phi signature,
 tags_typeonly Sg->Forall(wf_ty Sg Phi)(fs_params signature)->wf_ty Sg Phi(fs_ret signature)->
 Forall(wf_ty Sg(mkCtx(signature_type_inputs signature)[]))(fs_params signature)/\
 wf_ty Sg(mkCtx(signature_type_inputs signature)[])(fs_ret signature).
Proof.
 intros Sg Phi signature Tags Params Result.
 assert(Covered:forall tau,In tau(fs_params signature++[fs_ret signature])->
   forall t,In t(ty_frees tau)->avail(mkCtx(signature_type_inputs signature)[])(ITy t)).
 { intros tau Member t Free;apply context_type_is_available.
   unfold signature_type_inputs;cbn;rewrite type_telescope_requirements;apply nodup_In.
   unfold msig_frees;apply in_flat_map;exists tau;auto. }
 split.
 - apply Forall_forall;intros tau Member.
   apply Forall_forall with(x:=tau)in Params;[|exact Member].
   eapply formed_type_on_free_scope;[exact Tags|exact Params|].
   apply Covered;apply in_or_app;left;exact Member.
 - eapply formed_type_on_free_scope;[exact Tags|exact Result|].
   apply Covered;apply in_or_app;right;left;reflexivity.
Qed.
Theorem actual_value_row_formed : forall Sg Phi value tau,
 tags_typeonly Sg->wf_ty Sg Phi tau->
 (forall t,In t(ty_frees tau)->g_ty Sg t=true/\t<>this_sym)->
 formed_reify_declaration Sg(IVal value,RDValue(value_type_inputs tau)tau).
Proof.
 intros Sg Phi value tau Tags Typed Declared.
 exists(mkCtx(value_type_inputs tau)[]);split.
 - unfold value_type_inputs;apply formed_type_telescope;[apply NoDup_nodup|].
   apply Forall_forall;intros t Member;apply Declared;apply nodup_In in Member;exact Member.
 - eapply value_type_on_dependency_scope;eauto.
Qed.
Theorem actual_function_row_formed : forall Sg Phi function signature,
 tags_typeonly Sg->Forall(wf_ty Sg Phi)(fs_params signature)->wf_ty Sg Phi(fs_ret signature)->
 (forall t,In t(msig_frees signature)->g_ty Sg t=true/\t<>this_sym)->
 formed_reify_declaration Sg(IFn function,RDFunction(signature_type_inputs signature)signature).
Proof.
 intros Sg Phi function signature Tags Params Result Declared.
 exists(mkCtx(signature_type_inputs signature)[]);split.
 - unfold signature_type_inputs;apply formed_type_telescope;[apply NoDup_nodup|].
   apply Forall_forall;intros t Member;apply Declared;apply nodup_In in Member;exact Member.
 - eapply signature_on_dependency_scope;eauto.
Qed.

Lemma actual_source_nominals_type_only : forall W source declarations,
 tags_typeonly(reify_signature_overlay(source_world_nominal_base W source)declarations).
Proof. intros W source declarations n D payload Read;exact(source_world_tag_type_only W source n D payload Read). Qed.
