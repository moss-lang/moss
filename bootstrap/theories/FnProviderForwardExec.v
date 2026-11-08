(* Forwarder signature matching uses actual prototype and captured app coherence. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy BodyTr
 CanonTypes CtxWalk FnSnapshot FnSnapshotAlign FnProviderForwardSignature ForwarderTrace
 InfoInvariant InfoPreserve InfoCache ForwarderPrototype NeedMapExec InstantiateTr ProviderSig TyOfSubst.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Ltac forward_exec_noerr H:=exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(? &? &? &H)])].
Section Forward.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Notation WW:=(BodySoundCore.W w R Fl).
Theorem match_forwarder_captured_signature:forall fuel c e F C provider target wanted kind s sm final n ps rt,
 nget(st_syms s)(gs_id provider)=Some provider->gs_origin provider=OForwarder(gs_id target)->
 nget(w_syms w)(gs_id target)=Some target->gs_kind target=KFn->
 gs_decl target=ADFn RPlain n ps rt None->
 gs_decl provider=forwarder_decl target->gs_mod provider=gs_mod target->
 match_fn_sig fuel c e provider wanted kind s=Good(sm,final)->
 Inv s->InfoInv s->st_w s=w->CxRel w R Fl c e F C->Later w P R Fl final->
 A.nlook(gs_id target)(A.c_fns C)=Some(canonical_sig Fl e wanted)/\
 sm=[(IFn(gs_id target),IFn(gs_id target))].
Proof.
 intros fuel c e F C provider target wanted kind s sm final n ps rt Provider Origin Loaded Kind Decl Copied Home Whole Hi Ii Hw Hc L.
 pose proof(match_forwarder_target_signature w (P:=P) R Fl b Hload fuel c e F C provider target wanted kind s sm final n ps rt
 Loaded Decl Copied Home Whole Hi Hw Hc L)as Current.
 destruct fuel as[|fuel];[discriminate Whole|]. pose proof Whole as Run;cbn[match_fn_sig]in Run.
 apply bind_inv in Run as[info[s1[Info Run]]].
 destruct(ok_fn_info_frame _ _ _ _ Hi Info)as[I1 F1].
 apply bind_inv in Run as[pairs[s2[Needs Run]]].
 assert(On:ok(require_needs(cx_mod c)e provider info))by(unfold require_needs;okt).
 destruct(On _ _ _ I1 Needs)as[I2 F2].
 apply bind_inv in Run as[got[s3[Read Run]]].
 assert(Os:ok(fn_sig provider(tymap e)e None))by(unfold fn_sig;okt).
 destruct(Os _ _ _ I2 Read)as[I3 F3].
 destruct(sig_eqb_in e got e wanted)eqn:Equal;[|forward_exec_noerr Run].
 apply bind_inv in Run as[u[se[Event Map]]].
 assert(Oevent:ok(record_reify_event(REProvider ety key(gs_id provider)(fi_needs info)(reify_sig e got)(reify_arguments e))))
 by(unfold record_reify_event;apply ok_reify_update).
 destruct(Oevent _ _ _ I3 Event)as[Ie Fe].
 assert(Om:ok(smap_at(fi_env info)e pairs))by(unfold smap_at;okt).
 destruct(Om _ _ _ Ie Map)as[Ifinal Fm].
 assert(L1:Later w P R Fl s1)by(exact(later_back w P R Fl _ _ (fr_trans _ _ _ F2(fr_trans _ _ _ F3(fr_trans _ _ _ Fe Fm)))L)).
 destruct(fn_info_forwarder_prototype w (P:=P) R Fl b Hload provider provider(gs_id target)target s info s1 n ps rt None
 Provider Origin Loaded Kind Decl(fn_req_types w R Fl b Hload _ _ Hworld Loaded Kind)Info Hi Ii Hw L1)
 as[[prototype_sig Prototype][NeedList[Applications Merges]]].
 destruct(require_needs_all _ _ _ _ _ _ _ I1 Needs)as[Pairs[Present Coherent]].
 rewrite NeedList in Pairs,Present,Coherent;cbn[map site_key]in Pairs;subst pairs.
 pose proof(Forall_inv Present)as TargetPresent;pose proof(Forall_inv Coherent)as TargetCoherent.
 cbn[site_key key_present]in TargetPresent;cbn[site_key]in TargetCoherent.
 destruct(nget(en_fns e)(gs_id target))as[stored|]eqn:Stored;[|discriminate].
 assert(Table:A.nlook(gs_id target)(A.c_fns C)=Some(canonical_sig Fl e stored)).
 {rewrite(cr_fns _ _ _ _ _ _ _ Hc),Stored;reflexivity. }
 destruct(cr_fn_sigs _ _ _ _ _ _ _ Hc _ _ Table)as[snapshot[recorded[Captured[Recorded Lookup]]]].
 rewrite(cr_fapps _ _ _ _ _ _ _ Hc)in Lookup.
 destruct(get_app e(KyFn(gs_id target)))as[[have stamp]|]eqn:Have;[|discriminate].
 injection Lookup as <-.
 assert(NotMethod:A.kind_sym WW(gs_id target)<>Some A.DKMeth).
 {rewrite(kind_sym_rd w R Fl b Hload _ _ Loaded),Kind;discriminate. }
 assert(Agree:canonical_sig Fl e wanted=canonical_sig Fl e stored).
 {eapply(captured_signature_matches_current WW(gs_id target)(R(gs_id target))(A.c_ty C)(A.c_sub C)snapshot
 (map(fun q=>(fst q,option_map(tr Fl)(snd q)))have)prototype_sig).
  - exact NotMethod.
  - exact Recorded.
  - intros; eapply caller_type_readings_fixed;eassumption.
  - intros key value LookupType.
    rewrite !nlook_smap,requirement_snapshot_lookup in LookupType.
    destruct(existsb(Nat.eqb key)(R(gs_id target)))eqn:Member;[|discriminate].
    cbn[option_map]in LookupType;injection LookupType as <-.
    apply existsb_exists in Member as[r[InR Eq]];apply Nat.eqb_eq in Eq;subst r.
    eapply(incoherent_semantic w R Fl e info(KyFn(gs_id target))(KyFn(gs_id target))have stamp c F C Hc TargetCoherent Have key(TAbs key)).
    rewrite Applications;cbn[assoc key_eqb];rewrite Nat.eqb_refl;cbn[opt_default].
    apply in_map_iff;exists(key,Some(YAbs key));split;[reflexivity|].
    apply in_map_iff;exists key;split;[reflexivity|exact InR].
  - exact Prototype.
  - exact Current.
  - exact Captured. }
 split;[rewrite Agree;exact Table|].
 unfold smap_at in Map;cbn[mmap fst snd]in Map.
 cbn[key_item_at mbind ret]in Map;injection Map as <- _;reflexivity.
Qed.
End Forward.
