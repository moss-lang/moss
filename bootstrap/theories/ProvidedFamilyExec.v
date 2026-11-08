From NM Require Import WrapperKeyReadonly.
(* The actual on-demand method branch, including original-context arguments. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabMono BodySoundCore BodyTr BodyTy
 InfoPreserve FwdInvariant FwdReadonly FwdPreserve FwdSynthInv CanonTypes NominalSupport TySupport SourceSupport MethodKeyTr EtyTyExec CallArgs ProvidedDirect
 ProvideResultContext FamilySelectTrace FamilyMatchingCtx FamilyApplyRestore FamilySelectedApply.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Lemma provided_family_aa_phases:forall fuel c e ox r m args s x t sf,
 get_mth e(canon e r)(gs_id m)=None->
 provided_call(S fuel)c e ox r m(map AA args)s=Good((x,t),sf)->
 exists g matching s1 wanted s2 pre e2 s3 sg xs s4 it s5 unit,
 family_member(cx_mod c)e(canon e r)(gs_id m)s=Good(Some(g,matching),s1)/\
 key_item(KyMth(canon e r)(gs_id m))s1=Good(wanted,s2)/\
 apply_family fuel c e g matching[wanted]s2=Good((pre,e2),s3)/\
 get_mth e2(canon e r)(gs_id m)=Some sg/\
 check_args fuel c e(gs_name m)sg(map AA args)s3=Good(xs,s4)/\
 key_item_at e2(KyMth(canon e r)(gs_id m))s4=Good(it,s5)/\
 layout_ok e(sg_ret sg)s5=Good(unit,sf)/\
 x=fold_stmts pre(XMeth ox(gs_id m)it xs)/\t=sg_ret sg.
Proof.
 intros fuel c e ox r m args s x t sf Miss Run;cbn[provided_call]in Run.
 apply bind_inv in Run as(k&sk&Key&Run).
 destruct(@method_key_value e(cx_mod c)r m s k sk Key)as[-> ->].
 rewrite Miss in Run;apply bind_inv in Run as(fam&s1&Select&Run).
 destruct fam as[[g matching]|].
 2:{apply bind_inv in Run as(sh&ss&Show&Run);destruct(filter _ _)as[|q qs];
    [exfalso;eapply err_not_good;exact Run|].
    apply bind_inv in Run as(ws&sw&Many&Run);exfalso;eapply err_not_good;exact Run. }
 apply bind_inv in Run as(wanted&s2&Want&Run).
 apply bind_inv in Run as([pre e2]&s3&App&Run).
 destruct(get_mth e2(canon e r)(gs_id m))as[sg|]eqn:Found.
 2:{apply bind_inv in Run as(sh&ss&Show&Run);exfalso;eapply err_not_good;exact Run. }
 apply bind_inv in Run as([]&sl&Lit&Run).
 unfold check_literal_sig in Lit;rewrite literal_sig_ok_aa in Lit;injection Lit as <-.
 apply bind_inv in Run as(xs&s4&Args&Run);apply bind_inv in Run as(it&s5&Item&Run).
 apply bind_inv in Run as(unit&s6&Layout&Ret);injection Ret as <- <- <-.
 destruct args as[|a[|a' rest]];cbn[map]in *;
 exists g,matching,s1,wanted,s2,pre,e2,s3,sg,xs,s4,it,s5,unit;repeat split;assumption.
Qed.
Lemma provided_select_info:forall mi e r mid,okI(family_member mi e r mid).
Proof. intros;apply okI_ni;[unfold family_member;okt|unfold family_member;nit|unfold family_member;keepst]. Qed.
Lemma provided_select_fwd:forall mi e r mid,okF(family_member mi e r mid).
Proof. intros;apply okF_readonly;[unfold family_member;okt|unfold family_member;keepst]. Qed.
Lemma provided_wanted_info:forall k,okI(key_item k).
Proof. intros;apply okI_ni;[unfold key_item;okt|unfold key_item;nit|unfold key_item;keepst]. Qed.
Lemma provided_wanted_fwd:forall k,okF(key_item k).
Proof. intros;apply okF_readonly;[unfold key_item;okt|unfold key_item;keepst]. Qed.
Section Family.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall x y,Fl x=Fl y->x=y.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Variable checked:st.
Hypothesis Hcontexts:check_contexts checked=Good(tt,checked).
Hypothesis Hchecked:st_w checked=w.
Notation W:=(BodySoundCore.W w R Fl).
Lemma family_sig_uf:forall e e2 sg,en_uf e2=en_uf e->output_sig Fl e2 sg=output_sig Fl e sg.
Proof.
 intros e e2 sg Uf;unfold output_sig;f_equal.
 - apply map_ext;intro p;rewrite(canon_uf e e2 Uf);reflexivity.
 - f_equal;apply canon_uf;exact Uf.
Qed.
Theorem provided_family_exec:forall fuel,OK w P R Fl fuel->ProvidedFamilyStep w P R Fl fuel.
Proof.
 intros fuel IH c e ox r m args s x t sf Run F C Hi Ii Ifwd HC Hp Wr Sr Miss.
 pose proof(mono_group fuel)as Mon;do 15(apply proj2 in Mon);apply proj1 in Mon.
 pose proof(Mon c e ox r m(map AA args)s((x,t),sf)Run)as Up.
 destruct(provided_family_aa_phases _ _ _ _ _ _ _ _ _ _ _ Miss Up)
 as(g&matching&s1&wanted&s2&pre&e2&s3&sg&xs&s4&it&s5&unit&Select&Want&App&Found&Args&Item&Layout&->&->).
 assert(Os:ok(family_member(cx_mod c)e(canon e r)(gs_id m)))by(unfold family_member;okt).
 destruct(Os _ _ _ Hi Select)as[I1 F1].
 assert(Owant:ok(key_item(KyMth(canon e r)(gs_id m))))by(unfold key_item;okt).
 destruct(Owant _ _ _ I1 Want)as[I2 F2].
 destruct(ok_apply_family fuel c e g matching[wanted]_ _ _ I2 App)as[I3 F3].
 destruct(ok_check_args fuel c e(gs_name m)sg(map AA args)_ _ _ I3 Args)as[I4 F4].
 destruct(@ok_key_item_at_frame e2(KyMth(canon e r)(gs_id m))_ _ _ I4 Item)as[I5 F5].
 destruct(ok_layout_ok e(sg_ret sg)_ _ _ I5 Layout)as[_ F6].
 pose proof(post_back w P R Fl _ _ F F6 Hp)as Hp5.
 pose proof(post_back w P R Fl _ _ F F5 Hp5)as Hp4.
 pose proof(post_back w P R Fl _ _ F F4 Hp4)as Hp3.
 pose proof(later_back w P R Fl _ _ F3(proj1 Hp3))as L2.
 pose proof(later_back w P R Fl _ _ F2 L2)as L1.
 pose proof(later_back w P R Fl _ _ F1 L1)as L0.
 assert(Hw:st_w s=w)by exact(@lt_w w P R Fl s L0).
 assert(Wrc:WFty(canon e r))by exact(canon_wf e r(proj2(cr_uf _ _ _ _ _ _ _ HC))Wr).
 assert(Src:Support W(canon e r))by exact(@canon_support W e r (es_uf(cr_support _ _ _ _ _ _ _ HC))Sr).
 assert(Can:canon e(canon e r)=canon e r)by(apply canon_idem;exact(proj1(cr_uf _ _ _ _ _ _ _ HC))).
 destruct(family_selected_application w R Fl b Hload Finj fuel c e F C(cx_mod c)(canon e r)(gs_id m)
 s g matching s1 s2[wanted]pre e2 s3 sg checked Hworld Hcontexts Hchecked Select F2 App Hi Hw
 I2(proj1 Hp3)HC Wrc Src Can Miss Found)
 as(fx&emitted&sm&Cu&ns&Cr&ks&Pre&Fu&Fam&Poly&FC&Cover&Reads&Agree&Sat&Result&Inkey&Only&HR).
 assert(Uf:en_uf e2=en_uf e).
 { destruct(family_member_substitution w R Fl b Hload c e F C(cx_mod c)(canon e r)(gs_id m)
    s g matching s1 HC Select Hi Hw L1 Wrc Src)as[[Hg Fam0]Rest].
   destruct(proj1 Fam0)as(d&n&names&as0&rs&Decl&D&Shape).
   change(A.w_decl(W_read w R Fl)(gs_id g)=Some d)in Decl.
   rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
   change(gs_decl g=ADFunctor n names as0 rs None)in D.
   eapply (apply_family_uf w R Fl b Hload fuel c e g n names as0 rs matching[wanted]s2 pre e2 s3);[exact Hg|exact D|exact Fam|exact App|exact I2|].
   exact(fr_w' w _ _ F2(fr_w' w _ _ F1 Hw)). }
 assert(Clean:ety_ok(canon e r)=true)by exact(support_clean(loaded_record_unowned w R Fl b Hload)Src Wrc).
 assert(Can2:canon e2(canon e r)=canon e r)by(rewrite(canon_uf e e2 Uf);exact Can).
 destruct(cr_mths_clean _ _ _ _ _ _ _ HR _ _ _ Found)as[Ps[Rt This]].
 destruct(@es_mths W e2(cr_support _ _ _ _ _ _ _ HR)_ _ _ Found)as[_[Supps[SuppRet SuppThis]]].
 assert(Wret:WFty(sg_ret sg))by(apply ety_ok_wf;exact Rt).
 assert(Both:Forall(fun q=>WFty(snd q)/\Support W(snd q))(sg_params sg)).
 {rewrite Forall_forall in Ps,Supps|-*;intros q Hq;split.
  - apply ety_ok_wf;apply Ps;apply in_map;exact Hq.
  - exact(Supps q Hq). }
 pose proof(proj2(provided_select_info(cx_mod c)e(canon e r)(gs_id m))_ _ _ Hi Ii Select)as Info1.
 pose proof(proj2(provided_wanted_info(KyMth(canon e r)(gs_id m)))_ _ _ I1 Info1 Want)as Info2.
 pose proof(proj2(okI_apply_family fuel c e g matching[wanted])_ _ _ I2 Info2 App)as Info3.
 pose proof(proj2(provided_select_fwd(cx_mod c)e(canon e r)(gs_id m))_ _ _ Hi Ifwd Select)as Fwd1.
 pose proof(proj2(provided_wanted_fwd(KyMth(canon e r)(gs_id m)))_ _ _ I1 Fwd1 Want)as Fwd2.
 pose proof(proj2(okF_apply_family fuel c e g matching[wanted])_ _ _ I2 Fwd2 App)as Fwd3.
 pose proof(@check_args_aa_ok w P R Fl fuel IH c e(gs_name m)sg args s3 xs s4 F C
 Both Args I3 Info3 Fwd3 HC Hp4)as ArgRel.
 pose proof(@key_item_at_tr w P R Fl e2(A.family_restore C Cr)(canon e r)(gs_id m)s4 it s5
 Item I4(proj1 Hp5)(cr_sub _ _ _ _ _ _ _ HR)(proj1(cr_uf _ _ _ _ _ _ _ HR))
 (cr_apps _ _ _ _ _ _ _ HR(canon e r)(gs_id m)Clean))as ItemRel.
 change(it=A.rkey Cr(tr Fl(canon e r))(gs_id m))in ItemRel.
 assert(Missing:A.mlook(tr Fl(canon e r))(gs_id m)(A.c_mths C)=None).
 {pose proof(cr_mths _ _ _ _ _ _ _ HC r(gs_id m)Clean)as M;cbn beta in M;rewrite Miss in M;exact M. }
 assert(At:A.mlook(tr Fl(canon e r))(gs_id m)(A.c_mths Cr)=Some(output_sig Fl e sg)).
 {pose proof(cr_mths _ _ _ _ _ _ _ HR(canon e r)(gs_id m)ltac:(rewrite Can2;exact Clean))as M.
  cbn beta in M;rewrite Can2,Found in M;change(A.mlook(tr Fl(canon e r))(gs_id m)(A.c_mths Cr)=Some(output_sig Fl e2 sg))in M.
  rewrite(family_sig_uf e e2 sg Uf)in M;exact M. }
 split;[|split;assumption].
 rewrite Pre;cbn[fold_stmts];rewrite ItemRel.
 eapply A.PrFamily with(u:=gs_id g)(fx:=fx)(matching:=family_matching Fl e matching)
  (th:=emitted)(sm:=sm)(Cu:=Cu)(nsu:=ns)(C':=Cr)(ks:=ks)(g:=output_sig Fl e sg);
 try eassumption.
Qed.
End Family.
