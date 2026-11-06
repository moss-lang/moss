(* Every collected group retains a genuine selection made in the original environment. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTr BodyTy
 NominalSupport Merge CtxMerge CanonTypes InstantiateTr NeedReceiverFormation FamilyDemandTrace FamilySearchCompetitor MethodKeyTr.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition family_collect_step(c:cx)(e:env)(apps:list demand_group)(need:key):M(list demand_group):=
 let mi:=cx_mod c in

      match need with
      | KyMth r m =>
          let! mg := sym m in
          let! k := method_key e mi (instantiate e r) mg in
          if key_present e k then ret apps else
          match k with
          | KyMth r' _ =>
              let! fm := family_member mi e r' m in
              match fm with
              | Some (fg, th) =>
                  let! itk := key_item k in
                  let same q := Nat.eqb (gs_id (fst (fst q))) (gs_id fg) &&
                                Nat.eqb (List.length (snd (fst q))) (List.length th) &&
                                forallb (fun p => match nassoc (fst p) (snd (fst q)) with
                                                  | Some y => ety_eqb y (snd p) | None => false end) th in
                  if existsb same apps
                  then ret (map (fun q => if same q then (fst q, snd q ++ [itk]) else q) apps)
                  else ret (apps ++ [((fg, th), [itk])])
              | None => ret apps
              end
          | _ => ret apps
          end
      | _ => ret apps
      end.
Lemma family_collect_ok:forall c e before need,ok(family_collect_step c e before need).
Proof. intros;unfold family_collect_step;destruct need;try apply ok_ret.
 apply ok_bind;[unfold sym;okt|intro mg];apply ok_bind;[unfold method_key;okt|intro k].
 destruct(key_present e k);[apply ok_ret|];destruct k;try apply ok_ret.
 apply ok_bind;[unfold family_member;okt|intros[[g th]|]];[|apply ok_ret].
 apply ok_bind;[unfold key_item;okt|intro itk];destruct existsb;apply ok_ret.
Qed.
Section Collect.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySound.W w R Fl).
Definition FamilyGroupSeed(c:cx)(e:env)(wanted:list item)(q:demand_group):Prop:=
 exists r mid s sf,
 family_member(cx_mod c)e r mid s=Good(Some(fst(fst q),snd(fst q)),sf)/\
 Inv s/\st_w s=w/\Later w P R Fl sf/\WFty r/\Support W r/\canon e r=r/\
 get_mth e r mid=None/\In(A.mkey(tr Fl r)mid)wanted.
Lemma family_seed_first:forall c e wanted q q',fst q=fst q'->
 FamilyGroupSeed c e wanted q->FamilyGroupSeed c e wanted q'.
Proof. intros c e wanted q q' Eq H;unfold FamilyGroupSeed in *;rewrite <-Eq;exact H. Qed.
Lemma family_collect_seed_step:forall c e F C info before need s after sf,
 family_collect_step c e before need s=Good(after,sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->
 NeedReceivers w R Fl info->In need(fi_needs info)->
 Forall(FamilyGroupSeed c e(map(A.at_site C)(map(key_tr Fl)(fi_needs info))))before->
 Forall(FamilyGroupSeed c e(map(A.at_site C)(map(key_tr Fl)(fi_needs info))))after.
Proof.
 intros c e F C info before need s after sf Run Hi Hw L HC Form Hin Old.
 destruct need as[v|fn|r mid|u];try(injection Run as <- <-;exact Old).
 unfold family_collect_step in Run;apply bind_inv in Run as(mg&s1&Sym&Run).
 assert(Osy:ok(sym mid))by(unfold sym;okt).
 destruct(Osy _ _ _ Hi Sym)as[I1 F1].
 pose proof(family_sym_id _ _ _ _ Sym Hi)as Id.
 apply bind_inv in Run as(k&s2&Key&Run).
 destruct(@method_key_value _ _ _ _ _ _ _ Key)as[-> ->];rewrite Id in Run.
 destruct(key_present e(KyMth(canon e(instantiate e r))mid))eqn:Present;
 [injection Run as <- <-;exact Old|].
 apply bind_inv in Run as(fm&s3&Select&Run);destruct fm as[[fg th]|];[|injection Run as <- <-;exact Old].
 assert(Os:ok(family_member(cx_mod c)e(canon e(instantiate e r))mid))by(unfold family_member;okt).
 destruct(Os _ _ _ I1 Select)as[I3 F3].
 apply bind_inv in Run as(itk&s4&Item&Run).
 assert(Oi:ok(key_item(KyMth(canon e(instantiate e r))mid)))by(unfold key_item;okt).
 destruct(Oi _ _ _ I3 Item)as[I4 F4].
 assert(Last:sf=s4)by(destruct existsb;injection Run;auto);subst sf.
 pose proof(later_back w P R Fl _ _ F4 L)as L3.
 destruct(need_site_receiver_formed w R Fl b Hload c e F C info r mid HC Form Hin)as[WF[Support Can]].
 assert(Seed:forall wanted,FamilyGroupSeed c e(map(A.at_site C)(map(key_tr Fl)(fi_needs info)))((fg,th),wanted)).
 {intro wanted;exists(canon e(instantiate e r)),mid,s1,s3;cbn[fst snd].
  split;[exact Select|];split;[exact I1|];split;[exact(fr_w' w _ _ F1 Hw)|].
  split;[exact L3|];split;[exact WF|];split;[exact Support|];split;[exact Can|];split.
  - unfold key_present in Present;destruct(get_mth e(canon e(instantiate e r))mid);[discriminate Present|reflexivity].
  - rewrite map_map;apply in_map_iff;exists(KyMth r mid);split;[|exact Hin].
    rewrite <-(key_tr_site w R Fl c e F C(KyMth r mid)HC);reflexivity. }
 destruct existsb eqn:Existing;injection Run as <-.
 - apply Forall_map;rewrite Forall_forall in Old|-*;intros q Hq;destruct q as[[g th0]wanted];cbn[fst snd].
   destruct andb;[eapply family_seed_first with(q:=((g,th0),wanted));[reflexivity|apply Old;exact Hq]|apply Old;exact Hq].
 - apply Forall_app;split;[exact Old|constructor;[apply Seed|constructor]].
Qed.
Lemma family_collect_seed_fold:forall c e F C info needs before s after sf,
 mfold(family_collect_step c e)before needs s=Good(after,sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->
 NeedReceivers w R Fl info->(forall k,In k needs->In k(fi_needs info))->
 Forall(FamilyGroupSeed c e(map(A.at_site C)(map(key_tr Fl)(fi_needs info))))before->
 Forall(FamilyGroupSeed c e(map(A.at_site C)(map(key_tr Fl)(fi_needs info))))after.
Proof.
 intros c e F C info needs;induction needs as[|need rest IH];intros before s after sf Run Hi Hw L HC Form Sub Old.
 - injection Run as <- <-;exact Old.
 - cbn[mfold]in Run;apply bind_inv in Run as(next&sn&Step&Tail).
   destruct(family_collect_ok c e before need _ _ _ Hi Step)as[Inext Fstep].
   assert(Otail:ok(mfold(family_collect_step c e)next rest))by(apply ok_mfold;intros;apply family_collect_ok).
   destruct(Otail _ _ _ Inext Tail)as[If Ftail].
   eapply IH;[exact Tail|exact Inext|exact(fr_w' w _ _ Fstep Hw)|exact L|exact HC|exact Form| |].
   + intros k Hin;apply Sub;right;exact Hin.
   + eapply family_collect_seed_step;[exact Step|exact Hi|exact Hw|exact(later_back w P R Fl _ _ Ftail L)|exact HC|exact Form|apply Sub;left;reflexivity|exact Old].
Qed.
Theorem demand_collect_seeds:forall c e F C info s groups sf,
 demand_collect c e info s=Good(groups,sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->NeedReceivers w R Fl info->
 Forall(FamilyGroupSeed c e(map(A.at_site C)(map(key_tr Fl)(fi_needs info))))groups.
Proof.
 intros c e F C info s groups sf Run Hi Hw L HC Form.
 change(mfold(family_collect_step c e)[](fi_needs info)s=Good(groups,sf))in Run.
 eapply family_collect_seed_fold;[exact Run|exact Hi|exact Hw|exact L|exact HC|exact Form|auto|constructor].
Qed.
End Collect.
