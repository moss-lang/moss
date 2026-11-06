(* Source grouping never repeats an inferred substitution. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTr BodyTy
 NominalSupport TySupport SourceSupport CtxMerge Merge CanonTypes TypeReadingInj PMatchComplete PMatchExec
 InstantiateTr NeedReceiverFormation FamilyDemandTrace FamilyDemandCollect FamilySelectTrace
 FamilyMatchCanonical FamilyMatchingCtx MethodKeyTr FamilySearchCompetitor.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition family_group_raw(q:demand_group):nat*list(nat*ety):=(gs_id(fst(fst q)),snd(fst q)).
Definition family_group_sem(Fl:string->nat)(e:env)(q:demand_group):nat*subst:=
 (gs_id(fst(fst q)),family_matching Fl e(snd(fst q))).
Definition family_group_same(fg:gsym)(th:list(nat*ety))(q:demand_group):bool:=
 Nat.eqb(gs_id(fst(fst q)))(gs_id fg)&&Nat.eqb(List.length(snd(fst q)))(List.length th)&&
 forallb(fun p=>match nassoc(fst p)(snd(fst q))with Some y=>ety_eqb y(snd p)|None=>false end)th.
Lemma family_same_complete:forall fg th q,NoDup(map fst th)->
 family_group_raw q=(gs_id fg,th)->family_group_same fg th q=true.
Proof.
 intros fg th[[g th0]wanted]Nd Eq;unfold family_group_raw in Eq;cbn[fst snd]in Eq;injection Eq as Id ->.
 unfold family_group_same;cbn[fst snd];rewrite Id,!Nat.eqb_refl;cbn[andb].
 apply forallb_forall;intros[k y]Hin;cbn[fst snd].
 rewrite(match_nassoc_member _ th k y Nd Hin);apply ety_eqb_refl.
Qed.
Lemma family_raw_update:forall fg th itk before,
 NoDup(map fst th)->NoDup(map family_group_raw before)->
 NoDup(map family_group_raw
 (if existsb(family_group_same fg th)before
 then map(fun q=>if family_group_same fg th q then(fst q,snd q++[itk])else q)before
 else before++[((fg,th),[itk])])).
Proof.
 intros fg th itk before Nd Old;destruct(existsb(family_group_same fg th)before)eqn:E.
 - assert(Same:map family_group_raw(map(fun q=>if family_group_same fg th q then(fst q,snd q++[itk])else q)before)=map family_group_raw before).
   {rewrite map_map;apply map_ext;intro q;destruct(family_group_same fg th q);reflexivity. }
   rewrite Same;exact Old.
 - rewrite map_app;cbn[map family_group_raw fst snd];apply NoDup_app;[exact Old|constructor;[intro H;contradiction|constructor]|].
   intros q Hin Hnew;destruct Hnew as[Eq|[]];subst q.
   apply in_map_iff in Hin as(old&Eq&Hin).
   assert(True:family_group_same fg th old=true)by(eapply family_same_complete;eassumption).
   assert(Exists:existsb(family_group_same fg th)before=true)by(apply existsb_exists;exists old;auto).
   congruence.
Qed.
Section Distinct.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Lemma family_collect_distinct_step:forall c e F C info before need s after sf,
 family_collect_step c e before need s=Good(after,sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->
 NeedReceivers w R Fl info->In need(fi_needs info)->
 NoDup(map family_group_raw before)->NoDup(map family_group_raw after).
Proof.
 intros c e F C info before need s after sf Run Hi Hw L HC Form Hin Old.
 destruct need as[v|fn|r mid|u];try(injection Run as <- <-;exact Old).
 unfold family_collect_step in Run;apply bind_inv in Run as(mg&s1&Sym&Run).
 assert(Osy:ok(sym mid))by(unfold sym;okt);destruct(Osy _ _ _ Hi Sym)as[I1 F1].
 pose proof(family_sym_id _ _ _ _ Sym Hi)as Id.
 apply bind_inv in Run as(k&s2&Key&Run);destruct(@method_key_value _ _ _ _ _ _ _ Key)as[-> ->];rewrite Id in Run.
 destruct(key_present e(KyMth(canon e(instantiate e r))mid));[injection Run as <- <-;exact Old|].
 apply bind_inv in Run as(fm&s3&Select&Run);destruct fm as[[fg th]|];[|injection Run as <- <-;exact Old].
 assert(Os:ok(family_member(cx_mod c)e(canon e(instantiate e r))mid))by(unfold family_member;okt).
 destruct(Os _ _ _ I1 Select)as[I3 F3].
 apply bind_inv in Run as(itk&s4&Item&Run).
 assert(Oi:ok(key_item(KyMth(canon e(instantiate e r))mid)))by(unfold key_item;okt).
 destruct(Oi _ _ _ I3 Item)as[I4 F4].
 assert(Last:sf=s4)by(destruct existsb;injection Run;auto);subst sf.
 destruct(need_site_receiver_formed w R Fl b Hload c e F C info r mid HC Form Hin)as[WF[Supp Can]].
 destruct(family_member_substitution w R Fl b Hload c e F C(cx_mod c)(canon e(instantiate e r))mid s1 fg th s3
 HC Select I1(fr_w' w _ _ F1 Hw)(later_back w P R Fl _ _ F4 L)WF Supp)
 as[Loaded[Wth[Sth[Nd Poly]]]].
 assert(Shape:after=if existsb(family_group_same fg th)before then
 map(fun q=>if family_group_same fg th q then(fst q,snd q++[itk])else q)before
 else before++[((fg,th),[itk])]).
 {change((if existsb(family_group_same fg th)before then
 ret(map(fun q=>if family_group_same fg th q then(fst q,snd q++[itk])else q)before)
 else ret(before++[((fg,th),[itk])]))s4=Good(after,s4))in Run.
 destruct existsb;injection Run;auto. }
 rewrite Shape;apply family_raw_update;assumption.
Qed.
Lemma family_collect_distinct_fold:forall c e F C info needs before s after sf,
 mfold(family_collect_step c e)before needs s=Good(after,sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->
 NeedReceivers w R Fl info->(forall k,In k needs->In k(fi_needs info))->
 NoDup(map family_group_raw before)->NoDup(map family_group_raw after).
Proof.
 intros c e F C info needs;induction needs as[|need rest IH];intros before s after sf Run Hi Hw L HC Form Sub Old.
 - injection Run as <- <-;exact Old.
 - cbn[mfold]in Run;apply bind_inv in Run as(next&sn&Step&Tail).
   destruct(family_collect_ok c e before need _ _ _ Hi Step)as[Inext Fstep].
   assert(Otail:ok(mfold(family_collect_step c e)next rest))by(apply ok_mfold;intros;apply family_collect_ok).
   destruct(Otail _ _ _ Inext Tail)as[If Ftail].
   eapply IH;[exact Tail|exact Inext|exact(fr_w' w _ _ Fstep Hw)|exact L|exact HC|exact Form| |].
   + intros k Hin;apply Sub;right;exact Hin.
   + eapply family_collect_distinct_step;[exact Step|exact Hi|exact Hw|exact(later_back w P R Fl _ _ Ftail L)|exact HC|exact Form|apply Sub;left;reflexivity|exact Old].
Qed.
Theorem demand_collect_raw_distinct:forall c e F C info s groups sf,
 demand_collect c e info s=Good(groups,sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->NeedReceivers w R Fl info->
 NoDup(map family_group_raw groups).
Proof.
 intros c e F C info s groups sf Run Hi Hw L HC Form.
 change(mfold(family_collect_step c e)[](fi_needs info)s=Good(groups,sf))in Run.
 eapply family_collect_distinct_fold;[exact Run|exact Hi|exact Hw|exact L|exact HC|exact Form|auto|constructor].
Qed.
Hypothesis Finj:forall x y,Fl x=Fl y->x=y.
Lemma family_seed_readings:forall c e F C wanted q,
 CxRel w R Fl c e F C->FamilyGroupSeed w (P:=P)R Fl c e wanted q->
 Forall(fun z=>canon e(snd z)=snd z)(snd(fst q))/\
 Forall(fun z=>ety_ok(snd z)=true)(snd(fst q)).
Proof.
 intros c e F C wanted q HC(r&mid&s&sf&Select&Hi&Hw&L&Wr&Sr&Can&Miss&Wanted).
 split;[eapply family_member_matching_canonical;[exact Select|exact(proj1(cr_uf _ _ _ _ _ _ _ HC))|exact Can]|].
 destruct(family_member_substitution w R Fl b Hload c e F C(cx_mod c)r mid s(fst(fst q))(snd(fst q))sf HC Select Hi Hw L Wr Sr)
 as[Loaded[Wm[Sm[Nd Poly]]]].
 rewrite Forall_forall in Wm,Sm|-*;intros z Hin.
 exact(support_clean(loaded_record_unowned w R Fl b Hload)(Sm z Hin)(Wm z Hin)).
Qed.
Lemma family_matching_inj:forall e xs ys,
 Forall(fun z=>canon e(snd z)=snd z)xs->Forall(fun z=>canon e(snd z)=snd z)ys->
 Forall(fun z=>ety_ok(snd z)=true)xs->Forall(fun z=>ety_ok(snd z)=true)ys->
 family_matching Fl e xs=family_matching Fl e ys->xs=ys.
Proof.
 intros e xs;induction xs as[|[k x]xs IH];intros[|[j y]ys]Cx Cy Ox Oy Eq;cbn[family_matching map]in Eq;try discriminate;[reflexivity|].
 inversion Cx as[|z zs CanX Cxs];subst;inversion Cy as[|z zs CanY Cys];subst.
 inversion Ox as[|z zs OkX Oxs];subst;inversion Oy as[|z zs OkY Oys];subst.
 cbn[fst snd]in Eq,CanX,CanY,OkX,OkY.
 injection Eq as Kj Eq Rest;subst j;rewrite CanX,CanY in Eq.
 assert(x=y)by(eapply tr_inj;eassumption);subst y;f_equal.
 eapply IH;try eassumption;exact Rest.
Qed.
Lemma family_group_sem_inj:forall c e F C wanted q q',
 CxRel w R Fl c e F C->FamilyGroupSeed w (P:=P)R Fl c e wanted q->FamilyGroupSeed w (P:=P)R Fl c e wanted q'->
 family_group_sem Fl e q=family_group_sem Fl e q'->family_group_raw q=family_group_raw q'.
Proof.
 intros c e F C wanted q q' HC Hq Hq' Eq.
 destruct(family_seed_readings c e F C wanted q HC Hq)as[Cq Oq].
 destruct(family_seed_readings c e F C wanted q' HC Hq')as[Cq' Oq'].
 unfold family_group_sem in Eq;injection Eq as Id Match;unfold family_group_raw;rewrite Id;f_equal.
 eapply family_matching_inj;eassumption.
Qed.
Lemma family_groups_sem_distinct:forall c e F C wanted groups,
 CxRel w R Fl c e F C->Forall(FamilyGroupSeed w (P:=P)R Fl c e wanted)groups->
 NoDup(map family_group_raw groups)->NoDup(map(family_group_sem Fl e)groups).
Proof.
 intros c e F C wanted groups HC Seeds;induction Seeds as[|q groups Hq Rest IH];intro Nd;[constructor|].
 inversion Nd as[|raw raws Absent Tail];subst;cbn[map];constructor;[|apply IH;exact Tail].
 intro Hin;apply in_map_iff in Hin as(q'&Eq&Hin).
 assert(Same:family_group_raw q'=family_group_raw q).
 {eapply family_group_sem_inj;[exact HC| |exact Hq|exact Eq].
 rewrite Forall_forall in Rest;exact(Rest q' Hin). }
 apply Absent;apply in_map_iff;exists q';split;assumption.
Qed.
Theorem demand_collect_distinct:forall c e F C info s groups sf,
 demand_collect c e info s=Good(groups,sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->NeedReceivers w R Fl info->
 NoDup(map(family_group_sem Fl e)groups).
Proof.
 intros c e F C info s groups sf Run Hi Hw L HC Form.
 eapply family_groups_sem_distinct;[exact HC|eapply demand_collect_seeds|eapply demand_collect_raw_distinct];eassumption.
Qed.
End Distinct.
