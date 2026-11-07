(* Actual context update used by a source type binding. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore
 NatAssoc CanonBodyTy SourceSupport EtyTyExec NominalSupport NominalMergeSupport CanonTypes.
Import ListNotations.
Module A:=Audit.Elaboration.
Section TypeBinding.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat).
Notation WW:=(BodySoundCore.W w R Fl).
Lemma cxrel_type : forall c e F C t ty stamp,
 CxRel w R Fl c e F C ->WFty ty ->Support WW ty ->
 CxRel w R Fl c(set_tn e t ty stamp)F(A.set_ty C t(tr Fl(canon e ty))).
Proof.
 intros c e F C t ty stamp H Wt St.
 pose proof(cr_ty _ _ _ _ _ _ _ H)as Tlook.
 pose proof(cr_ty_wf _ _ _ _ _ _ _ H)as Wlook.
 pose proof(cr_support _ _ _ _ _ _ _ H)as Eenv.
 destruct H. constructor;auto.
 - intro x. cbn[A.set_ty A.c_ty A.nlook]. unfold Elab.tn_get,Elab.set_tn. cbn[en_tn].
   rewrite nassoc_aset. destruct(Nat.eqb x t);cbn[option_map fst];[reflexivity|exact(Tlook x)].
 - intros x y Ey. unfold Elab.tn_get,Elab.set_tn in Ey. cbn[en_tn]in Ey.
   rewrite nassoc_aset in Ey. destruct(Nat.eqb x t);cbn[option_map fst]in Ey.
   + injection Ey as <-. exact Wt.
   + exact(Wlook x y Ey).
 - exact(env_support_type t stamp Eenv St).
Qed.
End TypeBinding.

Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].
Ltac noerr_bind H := exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(? &? &? &H)])].
Section Execution.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Theorem check_bind_type_sound_full : forall fuel c e pp rp ra s xs e' c' s' F C,
 check_bind fuel c e (Spec(Some pp)None None)(AEPath rp ra)s=Good((xs,e',c'),s') ->
 Inv s ->CxRel w R Fl c e F C ->Later w P R Fl s' ->
 (exists t,A.resolve_sym WW(A.fr_mod F)pp=Some t /\A.kind_sym WW t=Some A.DKType) ->
 c'=c /\ exists C',CxRel w R Fl c' e' F C' /\
 A.BindItem WW P F C (Spec(Some pp)None None,Some(AEPath rp ra))
   (fun rest=>fold_stmts xs rest)C' F [].
Proof.
 intros fuel c e pp rp ra s xs e' c' s' F C E Hi Hc L(t0 & Hrs & Hkt).
 destruct(ok_check_bind fuel c e (Spec(Some pp)None None)(AEPath rp ra)_ _ _ Hi E)as[_ Fs].
 assert(Hw:st_w s=w)by(rewrite <-(fr_w _ _ Fs);exact(lt_w _ _ _ _ _ L)).
 destruct fuel as[|fuel];[discriminate E|]. cbn[check_bind]in E.
 apply bind_inv in E as(r & s1 & Er & E).
 destruct(rpath_rel w R Fl b Hload _ _ _ _ _ Hw(cr_mod_in _ _ _ _ _ _ _ Hc)Er)as[-> Hres].
 destruct r as[[t|mi]|];[|noerr_bind E|noerr_bind E].
 cbn[option_map ent_of]in Hres. apply bind_inv in E as(g & s2 & Eg & E).
 destruct(sym_loader w R Fl b Hload _ _ _ _ _ _ Hi Hw Hres Eg)as[-> Hg].
 assert(Et:t0=t).
 { unfold A.resolve_sym in Hrs. rewrite(cr_mod _ _ _ _ _ _ _ Hc),Hres in Hrs. injection Hrs as <-;reflexivity. }
 subst t0. pose proof(kind_sym_read b w R Fl Hload t g Hg)as Kd.
 change(A.kind_sym WW t=Some(kconv(gs_kind g)))in Kd.
 assert(Kg:gs_kind g=KType)by(rewrite Kd in Hkt;destruct(gs_kind g);try discriminate Hkt;reflexivity).
 rewrite Kg in E. apply bind_inv in E as(u & s1 & Epin & E).
 destruct(nmem t(cx_pins c))eqn:Pin;[noerr_bind Epin|]. injection Epin as _ <-.
 apply bind_inv in E as(ty & s2 & Et & E).
 destruct(ok_ety_of _ _ _ _ _ _ _ _ Hi Et)as[I2 F2].
 apply bind_inv in E as(ms & s3 & Ems & E).
 assert(Om:ok memory_sym)by(unfold memory_sym;okt).
 destruct(Om _ _ _ I2 Ems)as[I3 F3].
 apply bind_inv in E as(um & s4 & Emem & E).
 match type of Emem with ?op _ = _ =>assert(Omemory:ok op)by(destruct ms as[m|];[destruct(Nat.eqb m t)|];okt)end.
 destruct(Omemory _ _ _ I3 Emem)as[I4 F4].
 apply bind_inv in E as(stamp & s5 & Efresh & E).
 destruct(ok_fresh _ _ _ I4 Efresh)as[I5 F5].
 apply bind_inv in E as(code & s6 & Ecode & E).
 apply bind_inv in E as(umeta & se & Eevent & E). injection E as <- <- <- <-.
 destruct(ok_ety_ty _ _ _ _ I5 Ecode)as[I6 F6].
 match type of Eevent with ?op _ = _=>assert(Oevent:ok op)by(unfold record_reify_event;apply ok_reify_update)end.
 destruct(Oevent _ _ _ I6 Eevent)as[_ Fevent].
 pose proof(later_back w P R Fl _ _ Fevent L)as Lcode.
 pose proof(later_back w P R Fl _ _ (fr_trans _ _ _ F3(fr_trans _ _ _ F4(fr_trans _ _ _ F5 F6)))Lcode)as L2.
 destruct(ety_of_canonical w R Fl b Hload _ _ _ _ _ _ _ _ Et Hi Hw
   (cr_mod_in _ _ _ _ _ _ _ Hc)(proj1(cr_uf _ _ _ _ _ _ _ Hc))
   (cr_ty_wf _ _ _ _ _ _ _ Hc)I(cr_ty _ _ _ _ _ _ _ Hc)L2)as[Ty Wty].
 assert(Sty:Support WW ty).
 { eapply ety_of_support_at;[exact Hload|exact Et|exact Hi|exact Hw|exact(cr_mod_in _ _ _ _ _ _ _ Hc)| | |exact(cr_support _ _ _ _ _ _ _ Hc)|exact(cr_ty_wf _ _ _ _ _ _ _ Hc)|exact I|exact L2].
   - intros k y Ey. rewrite nassoc_tymap in Ey. exact(@tn_get_support WW e k y(cr_support _ _ _ _ _ _ _ Hc)Ey).
   - intros k y Ey. rewrite nassoc_tymap in Ey. exact(cr_ty_wf _ _ _ _ _ _ _ Hc k y Ey). }
 pose proof(ety_ty_tr w P R Fl _ _ _ _ Ecode I5 Lcode)as Ec. subst code.
 assert(Can:A.canon C(tr Fl(canon e ty))=tr Fl(canon e ty)).
 { unfold A.canon. rewrite(cr_sub _ _ _ _ _ _ _ Hc).
   rewrite (tr_canon Fl e (canon e ty)(proj1(cr_uf _ _ _ _ _ _ _ Hc))).
   rewrite canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)). reflexivity. }
 split;[reflexivity|].
 exists(A.set_ty C t(tr Fl(canon e ty))). split.
 - pose proof(cxrel_type w R Fl _ _ _ _ t(canon e ty)stamp Hc
    (canon_wf e ty(proj2(cr_uf _ _ _ _ _ _ _ Hc))Wty)
    (@canon_support WW e ty(@es_uf WW e(cr_support _ _ _ _ _ _ _ Hc))Sty))as Hnew.
   rewrite canon_idem in Hnew by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)). exact Hnew.
 - cbn[fold_stmts]. rewrite <-Can. eapply A.BTypeBind;
   [exact Hrs|exact Hkt| |rewrite(cr_mod _ _ _ _ _ _ _ Hc);exact Ty].
   rewrite(cr_pins _ _ _ _ _ _ _ Hc). intro Hin. pose proof(proj2(nmem_in t(cx_pins c))Hin)as Htrue.
   rewrite Pin in Htrue;discriminate.
Qed.
Theorem check_bind_type_sound : forall fuel c e pp rp ra s xs e' c' s' F C,
 check_bind fuel c e (Spec(Some pp)None None)(AEPath rp ra)s=Good((xs,e',c'),s') ->
 Inv s ->CxRel w R Fl c e F C ->Later w P R Fl s' ->
 (exists t,A.resolve_sym WW(A.fr_mod F)pp=Some t /\A.kind_sym WW t=Some A.DKType) ->
 exists C',CxRel w R Fl c' e' F C' /\
 A.BindItem WW P F C (Spec(Some pp)None None,Some(AEPath rp ra))
   (fun rest=>fold_stmts xs rest)C' F [].
Proof.
 intros fuel c e pp rp ra s xs e1 c1 s1 F C E Hi Hc L Hk.
 exact(proj2(check_bind_type_sound_full fuel c e pp rp ra s xs e1 c1 s1 F C E Hi Hc L Hk)).
Qed.
End Execution.

Section Shape.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Theorem check_bind_type_rhs:forall fuel c e pp y s xs e' c' s' F C,
 check_bind fuel c e(Spec(Some pp)None None)y s=Good((xs,e',c'),s')->
 Inv s->CxRel w R Fl c e F C->Later w P R Fl s'->
 (exists t,A.resolve_sym(BodySoundCore.W w R Fl)(A.fr_mod F)pp=Some t /\
  A.kind_sym(BodySoundCore.W w R Fl)t=Some A.DKType)->
 exists rp ra,y=AEPath rp ra.
Proof.
 intros fuel c e pp y s xs e' c' s' F C E Hi Hc L(t0 &Hrs &Hkt).
 destruct(ok_check_bind fuel c e(Spec(Some pp)None None)y _ _ _ Hi E)as[_ Fs].
 assert(Hw:st_w s=w)by(rewrite <-(fr_w _ _ Fs);exact(lt_w _ _ _ _ _ L)).
 destruct fuel as[|fuel];[discriminate E|]. cbn[check_bind]in E.
 apply bind_inv in E as(r &s1 &Er &E).
 destruct(rpath_rel w R Fl b Hload _ _ _ _ _ Hw(cr_mod_in _ _ _ _ _ _ _ Hc)Er)as[-> Hres].
 destruct r as[[t|mi]|];[|noerr_bind E|noerr_bind E].
 cbn[option_map ent_of]in Hres. apply bind_inv in E as(g &s2 &Eg &E).
 destruct(sym_loader w R Fl b Hload _ _ _ _ _ _ Hi Hw Hres Eg)as[-> Hg].
 assert(Et:t0=t).
 { unfold A.resolve_sym in Hrs. rewrite(cr_mod _ _ _ _ _ _ _ Hc),Hres in Hrs. injection Hrs as <-;reflexivity. }
 subst t0. pose proof(kind_sym_read b w R Fl Hload t g Hg)as Kd.
 change(A.kind_sym(BodySoundCore.W w R Fl)t=Some(kconv(gs_kind g)))in Kd.
 assert(Kg:gs_kind g=KType)by(rewrite Kd in Hkt;destruct(gs_kind g);try discriminate Hkt;reflexivity).
 rewrite Kg in E. destruct y;try noerr_bind E. eexists;eexists;reflexivity.
Qed.
End Shape.
