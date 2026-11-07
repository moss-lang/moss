(* Callee provenance remains usable after a genuine caller setup prefix. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr
 Merge CtxWalk CtxMerge CtxMethodExec InfoCache ContextFormationSupport NeedInfoExec InfoShape NeedSatisfies MethodKeyTr LoaderOrigin MainRoot.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section Source.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Theorem fn_info_satisfaction_after:forall g si info sf sr mi e ps sn sm s' c F C,
 nget(w_syms w)(gs_id g)=Some g->~A.IsMainFn(BodySoundCore.W w R Fl)(gs_id g)->
 fn_info g si=Good(info,sf)->Fr sf sr->
 require_needs mi e g info sr=Good(ps,sn)->smap_at(fi_env info)e ps sn=Good(sm,s')->
 Inv si->InfoInv si->st_w si=w->Inv sr->Later w P R Fl s'->CxRel w R Fl c e F C->
 exists Cs,A.CtxOf(BodySoundCore.W w R Fl)(gs_id g)Cs(map(key_tr Fl)(fi_needs info))/\
 A.Satisfies C Cs(map(key_tr Fl)(fi_needs info))sm.
Proof.
 intros g si info sf sr mi e ps sn sm s' c F C Hg Nm Ei Prefix En Es Hi Ii Hw Ir L Hc.
 pose proof(loaded_no_forwarder b w g Hload Hg)as Nf.
 pose proof(not_main w R Fl b Hload g Hg Nm)as Nm'.
 destruct(ok_fn_info_frame _ _ _ _ Hi Ei)as[Is Fs].
 assert(On:ok(require_needs mi e g info))by(unfold require_needs,require_merges;okt).
 destruct(On _ _ _ Ir En)as[In Fn].
 assert(Os:ok(smap_at(fi_env info)e ps)).
 { unfold smap_at;apply ok_mmap;intros[x y];apply ok_bind;[apply ok_key_item_at_frame|intro].
   apply ok_bind;[apply ok_key_item_at_frame|intro;apply ok_ret]. }
 destruct(Os _ _ _ In Es)as[Iout Fsm].
 pose proof(later_back w P R Fl _ _(fr_trans _ _ _ Prefix(fr_trans _ _ _ Fn Fsm))L)as Li.
 pose proof(fn_info_ctx_any w R Fl b Hload Finj(meth_step_holds w R Fl b Hload)
   _ _ _ _ Hg Nf Nm Nm' Ei Hi Ii Hw Li)as Fc.
 destruct Fc as[Cs[Ctx[Sub[Merges[Ty[Val[FnLook[Mth[Fun[Apps[Rows[Canonical Domain]]]]]]]]]]]].
 destruct(fn_info_valid_any_nonroot w R Fl b Hload _ _ _ _ Hg Nf Nm' Ei Hi Ii Hw Li)as[_[Ti _]].
 pose proof(fn_info_needs_ok_any_nonroot w R Fl b Hload _ _ _ _ Hg Nf Nm' Ei Hi Ii Hw Li)as Keys.
 pose proof(fn_info_apps_shape _ _ _ _ _ _ Hload Hw Hg Ii Ei)as Shape.
 exists Cs;split;[apply ctx_of_ie_complete;exact Ctx|].
 eapply need_satisfies;[exact Hload|exact En|exact Es|exact Ir|exact L|exact Hc|exact Ctx|exact Sub|exact Merges|exact Ti|exact Shape|exact Keys|exact Apps].
Qed.
End Source.
