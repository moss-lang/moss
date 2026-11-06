(* Source execution facts for real nominal interfaces.  In particular the
   opaque fallback retains the checked requirements without inventing a
   telescope or assuming that its formation is automatic. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit CoreMoss NMap Sx Elab ElabFrame ReqsStable ReifyPayload ReifyNominal ReifyFormation ReifyTypeTransport.
Import ListNotations.
Local Strategy opaque[FUEL ELAB_FUEL reqs skeleton_tag_payload].
Lemma actual_reifier_requirements_unique : forall b source head parameters after,
 Inv source->load_world b=Good(st_w source)->
 reqs head source=Good(parameters,after)->NoDup parameters.
Proof.
 intros b source head parameters after Invariant Loaded Run.
 destruct(ok_reqs_frame head source parameters after Invariant Run)as[After Frame].
 assert(Cached:nget(st_reqs after)head=Some parameters).
 { eapply(reqs_cached 200 head source parameters after).
   - intros symbol Read;exact(iv_ids _ Invariant head symbol Read).
   - rewrite reqs_frame_eq in Run;exact Run. }
 destruct(iv_reqs _ After b ltac:(rewrite(fr_w _ _ (fr_0 _ _ Frame));exact Loaded)
   head parameters Cached)as[Unique _];exact Unique.
Qed.
Lemma actual_nominal_interface_unique : forall b source head telescope payload,
 Inv source->load_world b=Good(st_w source)->
 source_nominal_interface source head=Some(telescope,payload)->
 NoDup(tyreqs telescope).
Proof.
 intros b source head telescope payload Invariant Loaded Read.
 unfold source_nominal_interface in Read.
 destruct(Nat.eqb head rec_nom);[discriminate|].
 destruct(reify_machine_head source head);[inversion Read;constructor|].
 destruct(nget(st_syms source)head)as[symbol|];[|discriminate].
 destruct(gs_kind symbol);try discriminate.
 - inversion Read;constructor.
 - destruct(reqs head source)as[[parameters after]|]eqn:Requirements;[|discriminate].
   destruct(skeleton_tag_payload head source)as[[body final]|];inversion Read;subst;
   rewrite type_telescope_requirements; eapply actual_reifier_requirements_unique; eauto.
Qed.

Lemma actual_nominal_base_parameters : forall b source,
 Inv source->load_world b=Good(st_w source)->
 nominal_type_parameters(reify_nominal_base source).
Proof.
 intros b source Invariant Loaded head telescope payload Read.
 change(match source_nominal_interface source head with
   Some(D,Some field)=>Some(D,field)|_=>None end=Some(telescope,payload))in Read.
 destruct(source_nominal_interface source head)as[[inputs[field|]]|]eqn:Interface;try discriminate.
 inversion Read;subst.
 pose proof(nominal_interface_type_only source head telescope (Some payload) Interface)as Only.
 exists(tyreqs telescope);split.
 - clear -Only. induction Only as[|i rest TypeOnly Rest IH];cbn;[reflexivity|].
   destruct i;cbn in TypeOnly;try discriminate;cbn;f_equal;exact IH.
 - eapply actual_nominal_interface_unique;eauto.
Qed.

Lemma actual_machine_interface : forall source offset,
 0<st_next source->offset<6->
 source_nominal_interface source(st_next source+offset)=Some([],None).
Proof.
 intros source offset Positive Bound;unfold source_nominal_interface.
 assert(NotRecord:Nat.eqb(st_next source+offset)rec_nom=false).
 { apply Nat.eqb_neq;unfold rec_nom;lia. }
 rewrite NotRecord;unfold reify_machine_head.
 assert(Present:existsb(Nat.eqb(st_next source+offset))(seq(st_next source)6)=true).
 { apply existsb_exists;exists(st_next source+offset);split.
   - apply in_seq;lia.
   - apply Nat.eqb_refl. }
 rewrite Present;reflexivity.
Qed.
Lemma actual_machine_type_formed : forall source offset Phi,
 0<st_next source->offset<6->
 wf_ty(reify_nominal_base source)Phi(TNom(st_next source+offset)[]).
Proof.
 intros source offset Phi Positive Bound.
 pose proof(actual_machine_interface source offset Positive Bound)as Interface.
 eapply WfOpaque with(D:=[]).
 - change(match source_nominal_interface source(st_next source+offset)with
     Some(D,Some payload)=>Some(D,payload)|_=>None end=None).
   rewrite Interface;reflexivity.
 - change(match source_nominal_interface source(st_next source+offset)with
     Some(D,None)=>Some D|_=>None end=Some[]).
   rewrite Interface;reflexivity.
 - apply(type_telescope_sat _ _ [] []);constructor.
 - constructor.
 - constructor.
Qed.

Lemma actual_cached_type_requirements_read : forall fuel source head parameters,
 fuel<>0->nget(st_reqs source)head=Some parameters->
 type_requirements fuel head source=Good(parameters,source).
Proof.
 intros [|fuel] source head parameters Nonzero Cached;[contradiction|].
  cbn[type_requirements cached_reqs mbind get ret].
  rewrite Cached;reflexivity.
Qed.
Local Strategy opaque[type_requirements].
Local Strategy 1[reqs].
Lemma actual_cached_requirements_read : forall source head parameters,
 nget(st_reqs source)head=Some parameters->reqs head source=Good(parameters,source).
Proof.
 intros source head parameters Cached;unfold reqs.
 exact(actual_cached_type_requirements_read 200 source head parameters ltac:(discriminate) Cached).
Qed.
Local Strategy opaque[reqs].
Lemma cached_tag_nominal_interface : forall source head symbol parameters,
 head<>rec_nom->reify_machine_head source head=false->
 nget(st_syms source)head=Some symbol->gs_kind symbol=KTag->
 nget(st_reqs source)head=Some parameters->
 exists payload,source_nominal_interface source head=Some(map ITy parameters,payload).
Proof.
 intros source head symbol parameters NonRecord NonMachine Symbol Kind Cached.
 unfold source_nominal_interface;apply Nat.eqb_neq in NonRecord.
 rewrite NonRecord,NonMachine,Symbol,Kind,(actual_cached_requirements_read _ _ _ Cached).
 destruct(skeleton_tag_payload head source)as[[payload final]|];eexists;reflexivity.
Qed.

Lemma nominal_opaque_interface_origin : forall source head D,
 source_nominal_interface source head=Some(D,None)->
 D=[]\/exists symbol parameters after,
   nget(st_syms source)head=Some symbol/\gs_kind symbol=KTag/\
   reqs head source=Good(parameters,after)/\D=map ITy parameters.
Proof.
 intros source head D Read;unfold source_nominal_interface in Read.
 destruct(Nat.eqb head rec_nom);[discriminate|].
 destruct(reify_machine_head source head);[inversion Read;left;reflexivity|].
 destruct(nget(st_syms source)head)as[symbol|]eqn:Symbol;[|discriminate].
 destruct(gs_kind symbol)eqn:Kind;try discriminate.
 destruct(reqs head source)as[[parameters after]|]eqn:Requirements;[|discriminate].
 destruct(skeleton_tag_payload head source)as[[payload final]|];[discriminate|].
 inversion Read;subst;right;exists symbol,parameters,after;repeat split;assumption.
Qed.
