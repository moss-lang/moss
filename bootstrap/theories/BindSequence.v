From NM Require Import BodyProofTools.
(* Source bind-item sequencing, with actual context and continuation updates. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore
 BodyDataHelpers BindType BindValue.
From NM Require Import InfoPreserve.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition bind_one(f:nat)(c:cx)(e:env)(it:spec*option aexpr):M(list xstmt*env*cx):=
 match it with(sp,None)=>apply_functor f c e sp|(sp,Some y)=>check_bind f c e sp y end.
Definition bind_fold_step(f:nat)(acc:list xstmt*env*cx)(it:spec*option aexpr):M(list xstmt*env*cx):=
 let '(xs,e,c):=acc in
 let! (ys,e',c'):=bind_one f c e it in ret(xs++ys,e',c').
Lemma ok_bind_one:forall f c e it,ok(bind_one f c e it).
Proof. intros f c e[sp[y|]];[apply ok_check_bind|apply ok_apply_functor]. Qed.
Lemma ok_bind_fold_step:forall f acc it,ok(bind_fold_step f acc it).
Proof.
 intros f[[xs e]c]it. unfold bind_fold_step. apply ok_bind;[apply ok_bind_one|].
 intros[[ys e']c'];apply ok_ret.
Qed.
Section Sequence.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Notation WW:=(BodySoundCore.W w R Fl).
Definition BindResult(c:cx)(e:env)(F:A.frame)(C:A.cx)(it:spec*option aexpr)
 (xs:list xstmt)(e':env)(c':cx):Prop:=
 exists F1 C1 made wrap,CxRel w R Fl c' e' F1 C1 /\A.fr_var F1=A.fr_var F /\
 cx_fresh c'=cx_fresh c++made /\A.BindItem WW P F C it wrap C1 F1 made /\
 forall tail,wrap tail=fold_stmts xs tail.
Definition OneStep(f:nat):Prop:=forall c e it s xs e' c' s',
 bind_one f c e it s=Good((xs,e',c'),s')->forall F C,
 Inv s->InfoInv s-> FwdInv s ->CxRel w R Fl c e F C->Post w P R Fl s' F->BindResult c e F C it xs e' c'.

Lemma bind_result_same:forall c e F C it xs e' c' C',
 c'=c->CxRel w R Fl c' e' F C'->
 A.BindItem WW P F C it(fun tail=>fold_stmts xs tail)C' F[] ->
 BindResult c e F C it xs e' c'.
Proof.
 intros c e F C it xs e' c' C' Ec Hc B. exists F,C',[],(fun tail=>fold_stmts xs tail).
 split;[exact Hc|]. split;[reflexivity|]. split;[rewrite app_nil_r,Ec;reflexivity|]. auto.
Qed.

Lemma bind_items_exec:forall f,OneStep f->forall its xs0 c e s xs e' c' s' F C,
 mfold(bind_fold_step f)(xs0,e,c)its s=Good((xs,e',c'),s')->
 Inv s->InfoInv s-> FwdInv s ->CxRel w R Fl c e F C->Post w P R Fl s' F->
 exists ys F1 C1 made wrap,xs=xs0++ys /\CxRel w R Fl c' e' F1 C1 /\
 A.fr_var F1=A.fr_var F /\cx_fresh c'=cx_fresh c++made /\
 A.BindItems WW P F C its wrap C1 F1 made /\forall tail,wrap tail=fold_stmts ys tail.
Proof.
 intros f Step its. induction its as[|it its IH];intros xs0 c e s xs e' c' s' F C E Hi Ii Ifwd Hc Hp.
 - cbn[mfold]in E. injection E as <- <- <- <-.
   exists [],F,C,[],(fun x=>x). split;[rewrite app_nil_r;reflexivity|].
   split;[exact Hc|]. split;[reflexivity|]. split;[rewrite app_nil_r;reflexivity|].
   split;[constructor|intro;reflexivity].
 - cbn[mfold]in E. apply bind_inv in E as(acc & s1 & Eone & E).
   unfold bind_fold_step in Eone. apply bind_inv in Eone as(res & sx & Eit & Eone).
   destruct res as[[ys e1]c1]. injection Eone as <- <-.
   destruct(ok_bind_one f c e it _ _ _ Hi Eit)as[I1 F1].
   destruct(ok_mfold _ _ _ its _ (ok_bind_fold_step f)_ _ _ I1 E)as[_ Ft].
   destruct(Step _ _ _ _ _ _ _ _ Eit F C Hi Ii Ifwd Hc(post_back w P R Fl _ _ F Ft Hp))
     as(Fa &Ca &ma &wa &Hca &Va &FreshA &Ba &Ea).
   destruct(IH (xs0++ys)c1 e1 sx xs e' c' s' Fa Ca E I1 ltac:(cache_inv) ltac:(fwd_inv) Hca(post_frame w P R Fl _ _ _ Va Hp))
     as(zs &Fb &Cb &mb &wb &Ex &Hcb &Vb &FreshB &Bb &Eb).
   exists(ys++zs),Fb,Cb,(ma++mb),(fun x=>wa(wb x)). split.
   + rewrite Ex,app_assoc. reflexivity.
   + split;[exact Hcb|]. split;[rewrite Vb;exact Va|]. split.
     * rewrite FreshB,FreshA,app_assoc;reflexivity.
     * split;[eapply A.BICons;[exact Ba|exact Bb]|]. intro tail.
       rewrite Ea,Eb,fold_stmts_app. reflexivity.
Qed.

Theorem bind_stmt_from_items:forall f,OneStep f->forall c e its s xs e' c' d s',
 check_stmt(S f)c e(ASBind its)s=Good((xs,e',c',d),s')->
 forall F C,Inv s->InfoInv s-> FwdInv s ->CxRel w R Fl c e F C->Post w P R Fl s' F->
 StmtConcl w P R Fl F C(ASBind its)xs c c' e' d.
Proof.
 intros f Step c e its s xs e' c' d s' E F C Hi Ii Ifwd Hc Hp.
 cbn[check_stmt]in E. apply bind_inv in E as(res &s1 &Fold &E).
 destruct res as[[ys ey]cy]. injection E as EYs EEy ECy ED ES. subst ys ey cy d s1.
 change(mfold(bind_fold_step f)([],e,c)its s=Good((xs,e',c'),s'))in Fold.
 destruct(bind_items_exec f Step _ _ _ _ _ _ _ _ _ _ _ Fold Hi Ii Ifwd Hc Hp)
   as(ys &F1 &C1 &made &wrap &Ex &H1 &V1 &Fresh &B &Eq).
 cbn[app]in Ex. subst xs. exists F1,C1,made. split;[exact H1|]. split;[exact V1|]. split;[exact Fresh|]. split.
 - intros rest tl exp xr t made2 _ Tail. rewrite <-Eq. eapply A.BlkBind;[exact B|exact Tail].
 - intro exp. pose proof(@A.BlkBind WW P F C its [] None exp wrap C1 F1 XUnit TUnit made [] B(A.BlkEmpty WW P F1 C1 exp))as Last.
   rewrite app_nil_r,Eq in Last. exact Last.
Qed.
End Sequence.

Definition simple_bind(W:A.world)(F:A.frame)(sp:spec)(y:aexpr):nat:=
 match sp with Spec(Some pp)None None=>
 match A.resolve_sym W(A.fr_mod F)pp with Some v=>
 match A.kind_sym W v with Some A.DKType=>match y with AEPath _ _=>1|_=>0 end
 |Some A.DKVal=>2|_=>0 end|None=>0 end|_=>0 end.
Section Dispatch.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Definition ResidualBindStep(f:nat):Prop:=forall c e sp y s xs e' c' s',
 check_bind f c e sp y s=Good((xs,e',c'),s')->forall F C,
 simple_bind WW F sp y=0 ->Inv s->InfoInv s-> FwdInv s ->CxRel w R Fl c e F C->Post w P R Fl s' F->
 BindResult w P R Fl c e F C(sp,Some y)xs e' c'.
Definition ApplyBindStep(f:nat):Prop:=forall c e sp s xs e' c' s',
 apply_functor f c e sp s=Good((xs,e',c'),s')->forall F C,
 Inv s->InfoInv s-> FwdInv s ->CxRel w R Fl c e F C->Post w P R Fl s' F->
 BindResult w P R Fl c e F C(sp,None)xs e' c'.

Lemma one_from_residual:forall f,OK w P R Fl f->ResidualBindStep f->ApplyBindStep f->OneStep w P R Fl f.
Proof.
 intros f IH Res Apply c e[sp[y|]]s xs e' c' s' E F C Hi Ii Ifwd Hc Hp;
 cbn[bind_one]in E;[|exact(Apply _ _ _ _ _ _ _ _ E F C Hi Ii Ifwd Hc Hp)].
 destruct sp as[[pp|][dn|][bs|]];
 try solve[eapply Res;[exact E|reflexivity|exact Hi|exact Ii|exact Ifwd|exact Hc|exact Hp]].
 destruct(A.resolve_sym WW(A.fr_mod F)pp)as[v|]eqn:Hr;
 try solve[eapply Res;[exact E|cbn[simple_bind];rewrite Hr;reflexivity|exact Hi|exact Ii|exact Ifwd|exact Hc|exact Hp]].
 destruct(A.kind_sym WW v)as[k|]eqn:Hk;
 try solve[eapply Res;[exact E|cbn[simple_bind];rewrite Hr,Hk;reflexivity|exact Hi|exact Ii|exact Ifwd|exact Hc|exact Hp]].
 destruct k;
 try solve[eapply Res;[exact E|cbn[simple_bind];rewrite Hr,Hk;reflexivity|exact Hi|exact Ii|exact Ifwd|exact Hc|exact Hp]].
 - destruct y;
   try solve[eapply Res;[exact E|cbn[simple_bind];rewrite Hr,Hk;reflexivity|exact Hi|exact Ii|exact Ifwd|exact Hc|exact Hp]].
   match type of E with check_bind _ _ _ (Spec(Some ?lhs)None None)(AEPath ?rp ?ra)_ = _ =>
    destruct(@check_bind_type_sound_full w P R Fl b Hload f c e lhs rp ra s xs e' c' s' F C E Hi Hc(proj1 Hp)
     (ex_intro _ v(conj Hr Hk)))as[Ec[C1[H1 B]]]end.
   eapply bind_result_same;[exact Ec|exact H1|exact B].
 - destruct f as[|f];[discriminate E|].
   destruct(@check_bind_value_sound_full w P R Fl b Hload f(ok_down w P R Fl f IH)
     c e pp y s xs e' c' s' F C E Hi Ii Ifwd Hc Hp(ex_intro _ v(conj Hr Hk)))as[Ec[C1[H1 B]]].
   eapply bind_result_same;[exact Ec|exact H1|exact B].
Qed.

Theorem bind_stmt_step:forall f,OK w P R Fl f->ResidualBindStep f->ApplyBindStep f->
 forall c e its s xs e' c' d s',check_stmt(S f)c e(ASBind its)s=Good((xs,e',c',d),s')->
 forall F C,Inv s->InfoInv s-> FwdInv s ->CxRel w R Fl c e F C->Post w P R Fl s' F->
 StmtConcl w P R Fl F C(ASBind its)xs c c' e' d.
Proof.
 intros f IH Res App. apply bind_stmt_from_items. exact(one_from_residual f IH Res App).
Qed.
End Dispatch.
