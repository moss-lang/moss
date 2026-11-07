(* The real collection and sequential family applications construct Demand. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy
 CtxMerge NominalSupport NeedReceiverFormation FamilyDemandTrace FamilyDemandCollect
 FamilyDemandDistinct FamilyDemandGroup FamilyMatchingCtx.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma family_applications_frame:forall f c e s groups pre e' sf,
 FamilyApplications f c e s groups pre e' sf->Inv s->Inv sf/\Fr s sf.
Proof.
 intros f c e s groups pre e' sf Trace;induction Trace;intro Hi.
 - split;[exact Hi|apply fr_refl].
 - destruct(ok_apply_family f c e fg th wanted _ _ _ Hi H)as[I1 F1].
   destruct(IHTrace I1)as[I2 F2];split;[exact I2|eapply fr_trans;eassumption].
Qed.
Section Demand.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall x y,Fl x=Fl y->x=y.
Notation W:=(BodySoundCore.W w R Fl).
Theorem family_applications_demand:forall f c e0 F C0 wanted sc e s groups pre e' sf,
 A.WorldOf b W->check_contexts sc=Good(tt,sc)->st_w sc=w->
 CxRel w R Fl c e0 F C0->
 FamilyApplications f c e s groups pre e' sf->
 Inv s->st_w s=w->Later w P R Fl sf->
 Forall(FamilyGroupSeed w (P:=P)R Fl c e0 wanted)groups->
 NoDup(map(family_group_sem Fl e0)groups)->
 forall C used,CxRel w R Fl c e F C->A.c_ty C=A.c_ty C0->A.c_sub C=A.c_sub C0->A.c_funs C=A.c_funs C0->
 en_uf e=en_uf e0->
 (forall q,In q(map(family_group_sem Fl e0)groups)->~In q used)->
 exists C',A.Demand W C0 wanted used C(fun x=>fold_stmts pre x)C'/\CxRel w R Fl c e' F C'.
Proof.
 intros f c e0 F C0 wanted sc e s groups pre e' sf Wo Checked Wsc HC0 Trace.
 induction Trace as[e s|e s fg th rawWanted rest full sm e1 s1 pre e2 s2 App Tail IH];
 intros Hi Hw L Seeds Distinct C used HC Ty Sub Funs Uf Apart.
 - exists C;split;[constructor|exact HC].
 - apply Forall_cons_iff in Seeds as[Seed Rest];cbn[map]in Distinct;apply NoDup_cons_iff in Distinct as[Absent Drest].
   destruct Seed as(r&mid&ss&se&Select&Is&Ws&Ls&Wr&Sr&Can&Miss&Wanted).
   destruct(ok_apply_family f c e fg th rawWanted _ _ _ Hi App)as[I1 F1].
   destruct(family_applications_frame _ _ _ _ _ _ _ _ Tail I1)as[I2 F2].
   pose proof(later_back w P R Fl _ _ F2 L)as L1.
   destruct(family_demand_group w R Fl b Hload Finj f c e0 F C0 e C(cx_mod c)r mid ss fg th se s
   [SApply(gs_id fg)full sm]e1 s1 rawWanted wanted sc Wo Checked Wsc Select Is Ws Ls HC0 Wr Sr Can Miss Wanted App Hi Hw L1 HC Ty Sub Funs Uf)
   as(full0&sm0&C1&Pre&Step&HC1&Ty1&Sub1&Fu1&Uf1).
   injection Pre as Full Sm;subst full0 sm0.
   destruct(IH I1(fr_w' w _ _ F1 Hw)L Rest Drest C1
   ((gs_id fg,family_matching Fl e0 th)::used)HC1 Ty1 Sub1 Fu1 Uf1)as(C2&Demand&HC2).
   + intros q Hin Hused;destruct Hused as[Eq|Hused].
     * subst q;apply Absent;exact Hin.
     * eapply(Apart q);[right;exact Hin|exact Hused].
   + exists C2;split;[|exact HC2].
     change(A.Demand W C0 wanted used C(fun x=>XApply(gs_id fg)full sm(fold_stmts pre x))C2).
     eapply A.DemandApply;[apply Apart;left;reflexivity|exact Step|exact Demand].
Qed.
Theorem demand_needs_exec:forall f c e F C info s pre e' sf sc,
 A.WorldOf b W->check_contexts sc=Good(tt,sc)->st_w sc=w->
 demand_needs f c e info s=Good((pre,e'),sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->NeedReceivers w R Fl info->
 exists C',A.Demand W C(map(A.at_site C)(map(key_tr Fl)(fi_needs info)))[]C
 (fun x=>fold_stmts pre x)C'/\CxRel w R Fl c e' F C'.
Proof.
 intros[|f]c e F C info s pre e' sf sc Wo Checked Wsc Run Hi Hw L HC Form.
 - cbn[demand_needs deep fail]in Run;discriminate Run.
 - destruct(demand_needs_trace f c e info s pre e' sf Run)as(groups&si&Collect&Apps).
   assert(Ocollect:ok(demand_collect c e info)).
   {change(ok(mfold(family_collect_step c e)[](fi_needs info)));apply ok_mfold;intros;apply family_collect_ok. }
   destruct(Ocollect _ _ _ Hi Collect)as[Ii Fi].
   destruct(family_applications_frame _ _ _ _ _ _ _ _ Apps Ii)as[If Fa].
   pose proof(later_back w P R Fl _ _ Fa L)as Li.
   eapply(family_applications_demand f c e F C(map(A.at_site C)(map(key_tr Fl)(fi_needs info)))sc e si groups pre e' sf);
   [exact Wo|exact Checked|exact Wsc|exact HC|exact Apps|exact Ii|exact(fr_w' w _ _ Fi Hw)|exact L|
   eapply demand_collect_seeds|eapply demand_collect_distinct|exact HC|reflexivity|reflexivity|reflexivity|reflexivity|];try eassumption.
   intros q Hin Hnil;contradiction.
Qed.
End Demand.
