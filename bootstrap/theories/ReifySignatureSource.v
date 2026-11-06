(* The actual completed nominal base remains formed when physical callable
   and value declarations are added. Only the genuinely emitted finite rows
   remain as declaration-formation obligations. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss NMap Elab ReifyData ReifySig ReifySigFormation
  ReifyNominal ReifyNominalWorld ReifyNominalWorldSource ReifySourceTypeFormation.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma reified_signature_type_extension : forall base declarations t,
 g_ty base t=true->g_ty(reify_signature_overlay base declarations)t=true.
Proof.
 intros base declarations t Declared;cbn.
 destruct(lookup_reify_decl(ITy t)declarations)as[value|];[destruct value|];auto.
Qed.
Lemma type_only_telescope_signature_extension : forall original target Phi D after,
 (forall t,g_ty original t=true->g_ty target t=true)->
 Forall(fun i=>is_ty_item i=true)D->wf_tele original Phi D after->wf_tele target Phi D after.
Proof.
 intros original target Phi D after Types Only Formed;induction Formed.
 - constructor.
 - inversion Only;subst.
   destruct i;cbn in H3;try discriminate.
   inversion H;subst;eapply WCons;[constructor;[apply Types;assumption|assumption]|exact H0|].
   apply IHFormed;assumption.
Qed.
Lemma type_only_parameter_telescope : forall types,
 Forall(fun i=>is_ty_item i=true)(map ITy types).
Proof. intros;apply Forall_forall;intros i Member;apply in_map_iff in Member as[t[<- _]];reflexivity. Qed.
Lemma source_world_tag_type_only : forall W source n D payload,
 g_tag(source_world_nominal_base W source)n=Some(D,payload)->
 Forall(fun i=>is_ty_item i=true)D.
Proof.
 intros W source n D payload Read.
 change(match cached_nominal_interface source n with Some(inputs,Some field)=>Some(inputs,field)|_=>None end=Some(D,payload))in Read.
 destruct(cached_nominal_interface source n)as[[inputs[field|]]|]eqn:Interface;try discriminate.
 inversion Read;subst.
 eapply nominal_interface_type_only;eapply cached_interface_is_actual;exact Interface.
Qed.
Lemma source_world_opaque_type_only : forall W source n D,
 g_opaque(source_world_nominal_base W source)n=Some D->
 Forall(fun i=>is_ty_item i=true)D.
Proof.
 intros W source n D Read.
 destruct(nominal_completion_opaque_origin _ _ _ _ Read)as[[k[Kind[Nominal ->]]]|Known].
 - apply type_only_parameter_telescope.
 - change(match cached_nominal_interface source n with Some(inputs,None)=>Some inputs|_=>None end=Some D)in Known.
   destruct(cached_nominal_interface source n)as[[inputs[field|]]|]eqn:Interface;try discriminate.
   inversion Known;subst.
   eapply nominal_interface_type_only;eapply cached_interface_is_actual;exact Interface.
Qed.
Theorem finite_rows_extend_formed_nominal_base : forall base declarations,
 wf_gsig base->
 (forall v,g_val base v=None)->(forall f,g_fn base f=None)->
 (forall m,g_mth base m=None)->(forall u,g_fun base u=None)->
 (forall n D payload,g_tag base n=Some(D,payload)->Forall(fun i=>is_ty_item i=true)D)->
 (forall n D,g_opaque base n=Some D->Forall(fun i=>is_ty_item i=true)D)->
 Forall(formed_reify_declaration(reify_signature_overlay base declarations))declarations->
 wf_gsig(reify_signature_overlay base declarations).
Proof.
 intros base declarations BaseWell NoValues NoFunctions NoMethods NoFunctors TagOnly OpaqueOnly Rows.
 apply finite_reified_signature_formed;try assumption.
 - intros n D payload Read.
   destruct(@wf_tag_decl base BaseWell n D payload Read)as[Phi[Tele Scoped]].
   exists Phi;split;[|exact Scoped].
   eapply type_only_telescope_signature_extension;[apply reified_signature_type_extension|eapply TagOnly;exact Read|exact Tele].
 - intros n D Read.
   destruct(@wf_opaque_decl base BaseWell n D Read)as[Phi Tele].
   exists Phi;eapply type_only_telescope_signature_extension;
     [apply reified_signature_type_extension|eapply OpaqueOnly;exact Read|exact Tele].
Qed.
Theorem source_world_signature_formed : forall W source declarations,
 (forall n parameters,nget(st_reqs source)n=Some parameters->A.w_reqs W n=parameters)->
 (forall n symbol,nget(st_syms source)n=Some symbol->gs_kind symbol=KTag->A.kind_sym W n=Some A.DKTag)->
 (forall n,NoDup(A.w_reqs W n))->
 (forall t,A.kind_sym W t=Some A.DKType->exists symbol,
   nget(st_syms source)t=Some symbol/\gs_kind symbol=KType)->
 (forall n k,A.kind_sym W n=Some k->(k=A.DKTag\/k=A.DKUnit)->
   Forall(fun t=>A.kind_sym W t=Some A.DKType/\t<>this_sym)(A.w_reqs W n))->
 Forall(formed_reify_declaration(reify_signature_overlay(source_world_nominal_base W source)declarations))declarations->
 wf_gsig(reify_signature_overlay(source_world_nominal_base W source)declarations).
Proof.
 intros W source declarations Cache Tags Unique Types Requirements Rows.
 apply finite_rows_extend_formed_nominal_base.
 - eapply source_world_nominal_base_formed;eauto.
 - reflexivity.
 - reflexivity.
 - reflexivity.
 - reflexivity.
 - apply source_world_tag_type_only.
 - apply source_world_opaque_type_only.
 - exact Rows.
Qed.
