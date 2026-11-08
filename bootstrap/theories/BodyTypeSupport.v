(* The body checker reads source types with their actual support invariant. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy
 CanonBodyTy NominalSupport SourceSupport.
Module A:=ExecutionModel.Elaboration.
Section BodyType.
Variable w:Elab.world.
Context {P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Theorem pd_ty_supported : forall t c e s d s',
 ety_of t(cx_mod c)(tymap e)e(cx_this c)s=Good(d,s') ->
 forall F C,Inv s -> CxRel w R Fl c e F C -> Later w P R Fl s' ->
 A.TyOf(BodySoundCore.W w R Fl)(A.fr_mod F)(A.c_ty C)(A.fr_this F)t(tr Fl(canon e d)) /\
 WFty d /\ Support(BodySoundCore.W w R Fl)d.
Proof.
 intros t c e s d s' E F C Hi Hc L.
 destruct(pd_ty_holds w R Fl b Hload _ _ _ _ _ _ E F C Hi Hc L)as[Ty Wd].
 split;[exact Ty|]. split;[exact Wd|].
 destruct(ok_ety_of _ _ _ _ _ _ _ _ Hi E)as[_ Fs].
 assert(Hw:st_w s=w)by(rewrite <-(fr_w _ _ Fs);exact(lt_w _ _ _ _ _ L)).
 eapply ety_of_support_at;[exact Hload|exact E|exact Hi|exact Hw|exact(cr_mod_in _ _ _ _ _ _ _ Hc)| | |exact(cr_support _ _ _ _ _ _ _ Hc)|exact(cr_ty_wf _ _ _ _ _ _ _ Hc)| |exact L].
 - intros k y Ey. rewrite nassoc_tymap in Ey. exact(@tn_get_support _ e k y(cr_support _ _ _ _ _ _ _ Hc)Ey).
 - intros k y Ey. rewrite nassoc_tymap in Ey. exact(cr_ty_wf _ _ _ _ _ _ _ Hc k y Ey).
 - pose proof(cr_this_support _ _ _ _ _ _ _ Hc)as St.
   pose proof(cr_this_wf _ _ _ _ _ _ _ Hc)as Wt.
   destruct(cx_this c);[split;assumption|exact I].
Qed.
End BodyType.
