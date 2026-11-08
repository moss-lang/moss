(* A forwarder's renamed parameters retain the checked target signature. *)
From Stdlib Require Import List String.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodyTr BodySoundCore CanonTypes Merge ProviderSig FnSigParallel ForwarderTrace.
Import ListNotations.
Lemma canonical_signature_parameter_names:forall Fl e sg target,
 map snd(sg_params sg)=map snd(sg_params target)->sg_ret sg=sg_ret target->
 canonical_sig Fl e sg=canonical_sig Fl e target.
Proof.
 intros Fl e sg target Params Ret;unfold canonical_sig.
 f_equal;[|rewrite Ret;reflexivity].
 replace(map(fun p=>tr Fl(canon e(snd p)))(sg_params sg))
 with(map(fun t=>tr Fl(canon e t))(map snd(sg_params sg)))by(rewrite map_map;reflexivity).
 replace(map(fun p=>tr Fl(canon e(snd p)))(sg_params target))
 with(map(fun t=>tr Fl(canon e t))(map snd(sg_params target)))by(rewrite map_map;reflexivity).
 rewrite Params;reflexivity.
Qed.
Theorem forwarder_signature_read:forall Fl target provider e s sg final,
 gs_decl provider=forwarder_decl target->gs_mod provider=gs_mod target->
 fn_sig provider(tymap e)e None s=Good(sg,final)->exists target_sig,
 fn_sig target(tymap e)e None s=Good(target_sig,final)/\
 canonical_sig Fl e sg=canonical_sig Fl e target_sig.
Proof.
 intros Fl target provider e s sg final Decl Home Run.
 destruct(fn_sig_forwarder_parallel target provider(tymap e)e None s sg final Decl Home Run)
 as[target_sig[Read[Params[Ret This]]]].
 exists target_sig;split;[exact Read|]. apply canonical_signature_parameter_names;assumption.
Qed.

Lemma caller_type_readings_fixed:forall w R Fl c e F C key value,
 CxRel w R Fl c e F C->ExecutionModel.Elaboration.nlook key(ExecutionModel.Elaboration.c_ty C)=Some value->
 ExecutionModel.app_subst(ExecutionModel.Elaboration.c_sub C)value=value.
Proof.
 intros w R Fl c e F C key value Hc Read.
 rewrite(cr_ty _ _ _ _ _ _ _ Hc)in Read.
 destruct(tn_get e key)as[raw|]eqn:Raw;[|discriminate].
 injection Read as <-.
 rewrite(cr_sub _ _ _ _ _ _ _ Hc),tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
 rewrite canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc));reflexivity.
Qed.

Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Ltac forward_signature_noerr H:=exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(? &? &? &H)])].
Section CheckedForwarder.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Theorem match_forwarder_target_signature:forall fuel c e F C provider target wanted kind s sm final n ps rt,
 nget(w_syms w)(gs_id target)=Some target->gs_decl target=ADFn RPlain n ps rt None->
 gs_decl provider=forwarder_decl target->gs_mod provider=gs_mod target->
 match_fn_sig fuel c e provider wanted kind s=Good(sm,final)->
 Inv s->st_w s=w->CxRel w R Fl c e F C->Later w P R Fl final->
 ExecutionModel.Elaboration.SigOf(BodySoundCore.W w R Fl)(gs_id target)(ExecutionModel.Elaboration.c_ty C)None(canonical_sig Fl e wanted).
Proof.
 intros[|fuel]c e F C provider target wanted kind s sm final n ps rt Loaded Decl Copied Home Run Hi Hw Hc L;
 [discriminate|]. cbn[match_fn_sig]in Run.
 apply bind_inv in Run as[info[s1[Info Run]]].
 destruct(ok_fn_info_frame _ _ _ _ Hi Info)as[I1 F1].
 apply bind_inv in Run as[pairs[s2[Needs Run]]].
 assert(On:ok(require_needs(cx_mod c)e provider info))by(unfold require_needs;okt).
 destruct(On _ _ _ I1 Needs)as[I2 F2].
 apply bind_inv in Run as[got[s3[Read Run]]].
 assert(Os:ok(fn_sig provider(tymap e)e None))by(unfold fn_sig;okt).
 destruct(Os _ _ _ I2 Read)as[I3 F3].
 destruct(sig_eqb_in e got e wanted)eqn:Equal;[|forward_signature_noerr Run].
 apply bind_inv in Run as[u[se[Event Map]]].
 assert(Oevent:ok(record_reify_event(REProvider ety key(gs_id provider)(fi_needs info)(reify_sig e got)(reify_arguments e))))
 by(unfold record_reify_event;apply ok_reify_update).
 destruct(Oevent _ _ _ I3 Event)as[Ie Fe].
 assert(Om:ok(smap_at(fi_env info)e pairs))by(unfold smap_at;okt).
 destruct(Om _ _ _ Ie Map)as[Ifinal Fm].
 destruct(forwarder_signature_read Fl target provider e s2 got s3 Copied Home Read)as[target_sig[Target Same]].
 pose proof(fn_sig_canonical w (P:=P) R Fl b Hload c e F C target None s2 target_sig s3 RPlain n ps rt None
 Target I2 ltac:(rewrite(fr_w _ _ F2),(fr_w _ _ F1);exact Hw)Hc Loaded Decl I
 (later_back w P R Fl _ _ (fr_trans _ _ _ Fe Fm)L))as Sig.
 rewrite <-Same,(sig_eqb_in_canonical Fl e got e wanted Equal)in Sig;exact Sig.
Qed.
End CheckedForwarder.
