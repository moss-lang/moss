(* Final-state cache receipts connect real source interfaces to the source
   world's requirement reading. Unobserved heads are completed opaquely. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import ExecutionModel CoreMoss NMap Sx Elab ElabFrame ReifyNominal
  ReifyNominalSource ReifyPayload ReifyNominalWorld ReifySourceTypeFormation
  ReifyPayloadScope ReifySource ReifyFormation BodyTr.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[FUEL ELAB_FUEL reqs skeleton_tag_payload].
Definition cached_nominal_interface(source:st)(head:nsym):option(tele*option ty):=
 match nget(st_syms source)head with
 | Some symbol=>match gs_kind symbol with
   | KTag=>match nget(st_reqs source)head with
     | None=>None|Some _=>source_nominal_interface source head end
   | _=>source_nominal_interface source head end
 | None=>source_nominal_interface source head end.
Definition cached_nominal_base(source:st):gsig:=
 let base:=reify_nominal_base source in
 mkGsigInterfaces(g_ty base)
 (fun n=>match cached_nominal_interface source n with
   Some(D,Some payload)=>Some(D,payload)|_=>None end)
 (g_val base)(g_fn base)(g_mth base)(g_fun base)(g_fixed base)(g_poly base)(g_cap base)
 (fun n=>match cached_nominal_interface source n with Some(D,None)=>Some D|_=>None end) (g_opaque_fun base).
Definition source_world_nominal_base(W:A.world)(source:st):gsig:=
 complete_nominal_signature W(cached_nominal_base source).
Lemma declared_head_not_machine : forall source head symbol,
 Inv source->nget(st_syms source)head=Some symbol->reify_machine_head source head=false.
Proof.
 intros source head symbol Invariant Read;apply Bool.not_true_is_false;intro Machine.
 unfold reify_machine_head in Machine;apply existsb_exists in Machine as[candidate[Within Same]].
 apply Nat.eqb_eq in Same;subst candidate;apply in_seq in Within.
 pose proof(iv_above _ Invariant head ltac:(lia))as Absent;congruence.
Qed.
Lemma cached_tag_interface_requirements : forall source head symbol D payload,
 Inv source->head<>rec_nom->nget(st_syms source)head=Some symbol->gs_kind symbol=KTag->
 cached_nominal_interface source head=Some(D,payload)->
 exists parameters,nget(st_reqs source)head=Some parameters/\D=map ITy parameters.
Proof.
 intros source head symbol D payload Invariant NonRecord Symbol Kind Read.
 pose proof(declared_head_not_machine _ _ _ Invariant Symbol)as NonMachine.
 unfold cached_nominal_interface in Read;rewrite Symbol,Kind in Read.
 destruct(nget(st_reqs source)head)as[parameters|]eqn:Cached;[|discriminate].
 exists parameters;split;[reflexivity|].
 unfold source_nominal_interface in Read;apply Nat.eqb_neq in NonRecord.
 rewrite NonRecord,NonMachine,Symbol,Kind,(actual_cached_requirements_read _ _ _ Cached)in Read.
 destruct(skeleton_tag_payload head source)as[[body final]|];inversion Read;reflexivity.
Qed.
Lemma cached_unit_interface : forall source head symbol,
 Inv source->head<>rec_nom->nget(st_syms source)head=Some symbol->gs_kind symbol=KUnit->
 cached_nominal_interface source head=Some([],Some TUnit).
Proof.
 intros source head symbol Invariant NonRecord Symbol Kind.
 pose proof(declared_head_not_machine _ _ _ Invariant Symbol)as NonMachine.
 unfold cached_nominal_interface;rewrite Symbol,Kind;unfold source_nominal_interface.
 apply Nat.eqb_neq in NonRecord;rewrite NonRecord,NonMachine,Symbol,Kind;reflexivity.
Qed.
Lemma cached_interface_is_actual : forall source head interface,
 cached_nominal_interface source head=Some interface->
 source_nominal_interface source head=Some interface.
Proof.
 intros source head interface Read;unfold cached_nominal_interface in Read.
 destruct(nget(st_syms source)head)as[symbol|];[|exact Read].
 destruct(gs_kind symbol);try exact Read.
 destruct(nget(st_reqs source)head);[exact Read|discriminate].
Qed.
Lemma cached_opaque_origin : forall source head D,
 g_opaque(cached_nominal_base source)head=Some D->
 D=[]\/exists symbol parameters,
 nget(st_syms source)head=Some symbol/\gs_kind symbol=KTag/\
 nget(st_reqs source)head=Some parameters/\D=map ITy parameters.
Proof.
 intros source head D Read.
 change(match cached_nominal_interface source head with Some(inputs,None)=>Some inputs|_=>None end=Some D)in Read.
 destruct(cached_nominal_interface source head)as[[inputs[payload|]]|]eqn:Interface;try discriminate.
 inversion Read;subst.
 destruct(nominal_opaque_interface_origin _ _ _ (cached_interface_is_actual _ _ _ Interface))
   as[Empty|[symbol[parameters[after[Symbol[Kind[Requirements Domain]]]]]]].
 - left;exact Empty.
 - right;unfold cached_nominal_interface in Interface;rewrite Symbol,Kind in Interface.
   destruct(nget(st_reqs source)head)as[existing|]eqn:Cached;[|discriminate].
   rewrite(actual_cached_requirements_read _ _ _ Cached)in Requirements;inversion Requirements;subst.
   exists symbol,parameters;repeat split;assumption.
Qed.

Lemma source_type_symbol_declared : forall source t symbol,
 t<>this_sym->nget(st_syms source)t=Some symbol->gs_kind symbol=KType->
 g_ty(cached_nominal_base source)t=true.
Proof.
 intros source t symbol Nonzero Read Kind.
 change(negb(Nat.eqb t this_sym)&&match nget(st_syms source)t with
   Some symbol=>kind_eqb(gs_kind symbol)KType|None=>false end=true).
 rewrite Read,Kind;cbn[kind_eqb];rewrite andb_true_r.
 apply Bool.negb_true_iff;apply Nat.eqb_neq;exact Nonzero.
Qed.

Lemma cached_known_payload_origin : forall source head D payload,
 g_tag(cached_nominal_base source)head=Some(D,payload)->
 (D=[]/\payload=TUnit)\/exists symbol parameters raw final,
 nget(st_syms source)head=Some symbol/\gs_kind symbol=KTag/\
 nget(st_reqs source)head=Some parameters/\D=map ITy parameters/\
 skeleton_tag_payload head source=Good(raw,final)/\
 payload=tr(reify_field_number source)raw.
Proof.
 intros source head D payload Read.
 change(match cached_nominal_interface source head with
   Some(inputs,Some field)=>Some(inputs,field)|_=>None end=Some(D,payload))in Read.
 destruct(cached_nominal_interface source head)as[[inputs[field|]]|]eqn:Cached;try discriminate.
 inversion Read;subst.
 pose proof(cached_interface_is_actual _ _ _ Cached)as Actual.
 unfold source_nominal_interface in Actual.
 destruct(Nat.eqb head rec_nom);[discriminate|].
 destruct(reify_machine_head source head);[discriminate|].
 destruct(nget(st_syms source)head)as[symbol|]eqn:Symbol;[|discriminate].
 destruct(gs_kind symbol)eqn:Kind;try discriminate.
 - inversion Actual;subst;left;split;reflexivity.
 - unfold cached_nominal_interface in Cached;rewrite Symbol,Kind in Cached.
   destruct(nget(st_reqs source)head)as[parameters|]eqn:Requirements;[|discriminate].
   rewrite(actual_cached_requirements_read _ _ _ Requirements)in Actual.
   destruct(skeleton_tag_payload head source)as[[raw final]|]eqn:Payload;[|discriminate].
   inversion Actual;subst;right;exists symbol,parameters,raw,final;repeat split;assumption.
Qed.
Theorem cached_nominal_payload_scoped : forall source head D payload,
 g_tag(cached_nominal_base source)head=Some(D,payload)->
 incl(ty_frees payload)(tyreqs D).
Proof.
 intros source head D payload Read.
 destruct(cached_known_payload_origin _ _ _ _ Read)
   as[[-> ->]|[symbol[parameters[raw[final[Symbol[Kind[Cached[-> [Payload ->]]]]]]]]]].
 - intros t Impossible;contradiction.
 - rewrite type_telescope_requirements;eapply cached_skeleton_payload_scoped;eauto.
Qed.

Section Correspondence.
Variables(W:A.world)(source:st).
Hypothesis Invariant:Inv source.
Hypothesis Positive:forall n k,A.kind_sym W n=Some k->(k=A.DKTag\/k=A.DKUnit)->n<>rec_nom.
Hypothesis TagSymbols:forall n,A.kind_sym W n=Some A.DKTag->
 exists symbol,nget(st_syms source)n=Some symbol/\gs_kind symbol=KTag.
Hypothesis UnitSymbols:forall n,A.kind_sym W n=Some A.DKUnit->
 exists symbol,nget(st_syms source)n=Some symbol/\gs_kind symbol=KUnit.
Hypothesis Cache:forall n parameters,nget(st_reqs source)n=Some parameters->A.w_reqs W n=parameters.
Hypothesis UnitRequirements:forall n,A.kind_sym W n=Some A.DKUnit->A.w_reqs W n=[].
Theorem source_world_nominal_schemas : source_nominal_schemas W(source_world_nominal_base W source).
Proof.
 apply nominal_completion_schemas;[exact Positive| |].
 - intros n k D payload Kind Nominal Read.
   change(match cached_nominal_interface source n with Some(inputs,Some field)=>Some(inputs,field)|_=>None end=Some(D,payload))in Read.
   destruct(cached_nominal_interface source n)as[[inputs[field|]]|]eqn:Interface;try discriminate.
   inversion Read;subst;destruct Nominal as[Same|Same];subst k.
   + destruct(TagSymbols _ Kind)as[symbol[Symbol IsTag]].
     destruct(cached_tag_interface_requirements _ _ _ _ _ Invariant
       (Positive _ _ Kind(or_introl eq_refl))Symbol IsTag Interface)as[parameters[Cached ->]].
     rewrite(Cache _ _ Cached);reflexivity.
   + destruct(UnitSymbols _ Kind)as[symbol[Symbol IsUnit]].
     rewrite(cached_unit_interface _ _ _ Invariant(Positive _ _ Kind(or_intror eq_refl))Symbol IsUnit)in Interface.
     inversion Interface;subst;rewrite(UnitRequirements _ Kind);reflexivity.
 - intros n k D Kind Nominal Read.
   change(match cached_nominal_interface source n with Some(inputs,None)=>Some inputs|_=>None end=Some D)in Read.
   destruct(cached_nominal_interface source n)as[[inputs[field|]]|]eqn:Interface;try discriminate.
   inversion Read;subst;destruct Nominal as[Same|Same];subst k.
   + destruct(TagSymbols _ Kind)as[symbol[Symbol IsTag]].
     destruct(cached_tag_interface_requirements _ _ _ _ _ Invariant
       (Positive _ _ Kind(or_introl eq_refl))Symbol IsTag Interface)as[parameters[Cached ->]].
     rewrite(Cache _ _ Cached);reflexivity.
   + destruct(UnitSymbols _ Kind)as[symbol[Symbol IsUnit]].
     rewrite(cached_unit_interface _ _ _ Invariant(Positive _ _ Kind(or_intror eq_refl))Symbol IsUnit)in Interface;discriminate.
Qed.
Hypothesis SourceTags:forall n symbol,nget(st_syms source)n=Some symbol->gs_kind symbol=KTag->
 A.kind_sym W n=Some A.DKTag.
Hypothesis RequirementsUnique:forall n,NoDup(A.w_reqs W n).
Hypothesis TypeSymbols:forall t,A.kind_sym W t=Some A.DKType->
 exists symbol,nget(st_syms source)t=Some symbol/\gs_kind symbol=KType.
Hypothesis RequirementKinds:forall n k,A.kind_sym W n=Some k->(k=A.DKTag\/k=A.DKUnit)->
 Forall(fun t=>A.kind_sym W t=Some A.DKType/\t<>this_sym)(A.w_reqs W n).
Lemma source_nominal_requirement_types : forall n k,A.kind_sym W n=Some k->(k=A.DKTag\/k=A.DKUnit)->
 Forall(fun t=>g_ty(cached_nominal_base source)t=true/\t<>this_sym)(A.w_reqs W n).
Proof.
 intros n k Kind Nominal;pose proof(RequirementKinds n k Kind Nominal)as Types.
 eapply Forall_impl;[|exact Types];intros t[TypeKind Nonzero];split;[|exact Nonzero].
 destruct(TypeSymbols t TypeKind)as[symbol[Read IsType]];eapply source_type_symbol_declared;eauto.
Qed.
Theorem source_world_opaque_telescope_formed : forall n D,
 g_opaque(source_world_nominal_base W source)n=Some D->
 exists Phi,wf_tele(source_world_nominal_base W source)empty_ctx D Phi.
Proof.
 unfold source_world_nominal_base;apply nominal_completion_opaque_formed;
 [exact RequirementsUnique|exact source_nominal_requirement_types|].
 intros n D Read;destruct(cached_opaque_origin _ _ _ Read)
   as[Empty|[symbol[parameters[Symbol[Kind[Cached Domain]]]]]].
 - exists[];subst D;repeat split;constructor.
 - exists parameters;split;[exact Domain|];rewrite <-(Cache _ _ Cached);split.
   + apply RequirementsUnique.
   + apply(source_nominal_requirement_types n A.DKTag);[eapply SourceTags;eauto|left;reflexivity].
Qed.
Theorem source_world_nominal_base_formed : wf_gsig(source_world_nominal_base W source).
Proof.
 constructor.
 - intros n D payload Read.
   change(g_tag(cached_nominal_base source)n=Some(D,payload))in Read.
   pose proof(cached_nominal_payload_scoped _ _ _ _ Read)as Scoped.
   destruct(cached_known_payload_origin _ _ _ _ Read)
     as[[-> ->]|[symbol[parameters[raw[final[Symbol[Kind[Cached[-> [Payload Meaning]]]]]]]]]].
   + exists empty_ctx;split;[constructor|exact Scoped].
   + exists(mkCtx(map ITy parameters)[]);split;[|exact Scoped].
     apply formed_type_telescope.
     * rewrite <-(Cache _ _ Cached);apply RequirementsUnique.
     * change(Forall(fun t=>g_ty(cached_nominal_base source)t=true/\t<>this_sym)parameters).
       rewrite <-(Cache _ _ Cached);apply(source_nominal_requirement_types n A.DKTag);
         [eapply SourceTags;eauto|left;reflexivity].
 - intros v D tau Impossible;discriminate.
 - intros f D signature body Impossible;discriminate.
 - intros m D signature Impossible;discriminate.
 - intros u inputs outputs body Impossible;discriminate.
 - exact source_world_opaque_telescope_formed.
Qed.
End Correspondence.
