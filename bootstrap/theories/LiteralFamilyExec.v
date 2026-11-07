(* Literal arithmetic uses the same actual on-demand family application. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabMono BodySoundCore BodyTr BodyTy
 CanonTypes NominalSupport TySupport SourceSupport MethodKeyTr EtyTyExec BodyLit LiteralDirect
 ProvideResultContext FamilySelectTrace FamilyMatchingCtx FamilyApplyRestore FamilySelectedApply ProvidedFamilyExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma provided_family_ab_phases:forall fuel c e ox T oy m s x t sf,
 get_mth e(canon e T)(gs_id m)=None->
 provided_call(S fuel)c e ox T m[AB oy T]s=Good((x,t),sf)->
 exists g matching s1 wanted s2 pre e2 s3 sg ul sl xs s4 it s5 unit,
 family_member(cx_mod c)e(canon e T)(gs_id m)s=Good(Some(g,matching),s1)/\
 key_item(KyMth(canon e T)(gs_id m))s1=Good(wanted,s2)/\
 apply_family fuel c e g matching[wanted]s2=Good((pre,e2),s3)/\
 get_mth e2(canon e T)(gs_id m)=Some sg/\
 check_literal_sig e(cx_mod c)m T[AB oy T]sg s3=Good(ul,sl)/\
 check_args fuel c e(gs_name m)sg[AB oy T]sl=Good(xs,s4)/\
 key_item_at e2(KyMth(canon e T)(gs_id m))s4=Good(it,s5)/\
 layout_ok e(sg_ret sg)s5=Good(unit,sf)/\
 x=fold_stmts pre(XMeth ox(gs_id m)it xs)/\t=T.
Proof.
 intros fuel c e ox T oy m s x t sf Miss Run;cbn[provided_call]in Run.
 apply bind_inv in Run as(k&sk&Key&Run).
 destruct(@method_key_value e(cx_mod c)T m s k sk Key)as[-> ->].
 rewrite Miss in Run;apply bind_inv in Run as(fam&s1&Select&Run).
 destruct fam as[[g matching]|].
 2:{apply bind_inv in Run as(sh&ss&Show&Run);destruct(filter _ _)as[|q qs];
    [exfalso;eapply err_not_good;exact Run|].
    apply bind_inv in Run as(ws&sw&Many&Run);exfalso;eapply err_not_good;exact Run. }
 apply bind_inv in Run as(wanted&s2&Want&Run).
 apply bind_inv in Run as([pre e2]&s3&App&Run).
 destruct(get_mth e2(canon e T)(gs_id m))as[sg|]eqn:Found.
 2:{apply bind_inv in Run as(sh&ss&Show&Run);exfalso;eapply err_not_good;exact Run. }
 apply bind_inv in Run as(ul&sl&Lit&Run);apply bind_inv in Run as(xs&s4&Args&Run).
 apply bind_inv in Run as(it&s5&Item&Run);apply bind_inv in Run as(unit&s6&Layout&Ret).
 injection Ret as <- <- <-.
 exists g,matching,s1,wanted,s2,pre,e2,s3,sg,ul,sl,xs,s4,it,s5,unit;repeat split;assumption.
Qed.
Section Literal.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall x y,Fl x=Fl y->x=y.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Variable checked:st.
Hypothesis Hcontexts:check_contexts checked=Good(tt,checked).
Hypothesis Hchecked:st_w checked=w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem literal_family_exec:forall fuel,LiteralFamilyStep w P R Fl fuel.
Proof.
 intros fuel c e ox T oy m s x t sf Run F C Hi Ii Ifwd HC Hp WT ST Miss.
 pose proof(mono_group fuel)as Mon;do 15(apply proj2 in Mon);apply proj1 in Mon.
 pose proof(Mon c e ox T m[AB oy T]s((x,t),sf)Run)as Up.
 destruct(provided_family_ab_phases _ _ _ _ _ _ _ _ _ _ _ Miss Up)
 as(g&matching&s1&wanted&s2&pre&e2&s3&sg&ul&sl&xs&s4&it&s5&unit&Select&Want&App&Found&Lit&Args&Item&Layout&->&->).
 assert(Os:ok(family_member(cx_mod c)e(canon e T)(gs_id m)))by(unfold family_member;okt).
 destruct(Os _ _ _ Hi Select)as[I1 F1].
 assert(Owant:ok(key_item(KyMth(canon e T)(gs_id m))))by(unfold key_item;okt).
 destruct(Owant _ _ _ I1 Want)as[I2 F2].
 destruct(ok_apply_family fuel c e g matching[wanted]_ _ _ I2 App)as[I3 F3].
 destruct(ok_check_literal_sig e(cx_mod c)m T[AB oy T]sg _ _ _ I3 Lit)as[Il Flit].
 destruct(ok_check_args fuel c e(gs_name m)sg[AB oy T]_ _ _ Il Args)as[I4 F4].
 destruct(@ok_key_item_at_frame e2(KyMth(canon e T)(gs_id m))_ _ _ I4 Item)as[I5 F5].
 destruct(ok_layout_ok e(sg_ret sg)_ _ _ I5 Layout)as[_ F6].
 pose proof(post_back w P R Fl _ _ F F6 Hp)as Hp5.
 pose proof(post_back w P R Fl _ _ F F5 Hp5)as Hp4.
 pose proof(post_back w P R Fl _ _ F F4 Hp4)as Hpl.
 pose proof(post_back w P R Fl _ _ F Flit Hpl)as Hp3.
 pose proof(later_back w P R Fl _ _ F3(proj1 Hp3))as L2.
 pose proof(later_back w P R Fl _ _ F2 L2)as L1.
 pose proof(later_back w P R Fl _ _ F1 L1)as L0.
 assert(Hw:st_w s=w)by exact(@lt_w w P R Fl s L0).
 assert(Wrc:WFty(canon e T))by exact(canon_wf e T(proj2(cr_uf _ _ _ _ _ _ _ HC))WT).
 assert(Src:Support W(canon e T))by exact(@canon_support W e T(es_uf(cr_support _ _ _ _ _ _ _ HC))ST).
 assert(Can:canon e(canon e T)=canon e T)by(apply canon_idem;exact(proj1(cr_uf _ _ _ _ _ _ _ HC))).
 destruct(family_selected_application w R Fl b Hload Finj fuel c e F C(cx_mod c)(canon e T)(gs_id m)
 s g matching s1 s2[wanted]pre e2 s3 sg checked Hworld Hcontexts Hchecked Select F2 App Hi Hw
 I2(proj1 Hp3)HC Wrc Src Can Miss Found)
 as(fx&emitted&sm&Cu&ns&Cr&ks&Pre&Fu&Fam&Poly&FC&Cover&Reads&Agree&Sat&Result&Inkey&Only&HR).
 assert(Uf:en_uf e2=en_uf e).
 {destruct(family_member_classified w R Fl b Hload c e F C(cx_mod c)(canon e T)(gs_id m)
   s g matching s1 HC Select Hi Hw L1)as[Hg Fam0].
  destruct(proj1 Fam0)as(d&n&names&as0&rs&Decl&D&Shape).
  change(A.w_decl(W_read w R Fl)(gs_id g)=Some d)in Decl.
  rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
  change(gs_decl g=ADFunctor n names as0 rs None)in D.
  eapply (apply_family_uf w R Fl b Hload fuel c e g n names as0 rs matching[wanted]s2 pre e2 s3);
   [exact Hg|exact D|exact Fam|exact App|exact I2|].
  exact(fr_w' w _ _ F2(fr_w' w _ _ F1 Hw)). }
 assert(Clean:ety_ok(canon e T)=true)by exact(support_clean(loaded_record_unowned w R Fl b Hload)Src Wrc).
 assert(Can2:canon e2(canon e T)=canon e T)by(rewrite(canon_uf e e2 Uf);exact Can).
 destruct(@literal_sig_ab_info e(cx_mod c)m T oy sg s3 ul sl Lit)as(pn&pty&Ps&EqP&EqR).
 pose proof(@key_item_at_tr w P R Fl e2(A.family_restore C Cr)(canon e T)(gs_id m)s4 it s5
 Item I4(proj1 Hp5)(cr_sub _ _ _ _ _ _ _ HR)(proj1(cr_uf _ _ _ _ _ _ _ HR))
 (cr_apps _ _ _ _ _ _ _ HR(canon e T)(gs_id m)Clean))as ItemRel.
 change(it=A.rkey Cr(tr Fl(canon e T))(gs_id m))in ItemRel.
 assert(Missing:A.mlook(tr Fl(canon e T))(gs_id m)(A.c_mths C)=None).
 {pose proof(cr_mths _ _ _ _ _ _ _ HC T(gs_id m)Clean)as M;cbn beta in M;rewrite Miss in M;exact M. }
 assert(At:A.mlook(tr Fl(canon e T))(gs_id m)(A.c_mths Cr)=Some(A.FSig[tr Fl(canon e T)](tr Fl(canon e T)))).
 {pose proof(cr_mths _ _ _ _ _ _ _ HR(canon e T)(gs_id m)ltac:(rewrite Can2;exact Clean))as M.
  cbn beta in M;rewrite Can2,Found in M;change(A.mlook(tr Fl(canon e T))(gs_id m)(A.c_mths Cr)=Some(output_sig Fl e2 sg))in M.
  rewrite(family_sig_uf Fl e e2 sg Uf)in M.
  unfold output_sig in M;rewrite Ps in M;cbn[map snd]in M;rewrite EqP,EqR in M;exact M. }
 rewrite(check_args_ab fuel c e(gs_name m)sg oy T sl xs s4 Args),Pre;cbn[fold_stmts];rewrite ItemRel.
 eapply A.LitOpFamily with(u:=gs_id g)(fx:=fx)(matching:=family_matching Fl e matching)
 (th:=emitted)(sm:=sm)(Cu:=Cu)(nsu:=ns)(C':=Cr)(ks:=ks);eassumption.
Qed.
End Literal.
