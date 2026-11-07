(* Enter a defined functor's module and restore the caller control frame. *)
From Stdlib Require Import List String.
From NM Require Import Audit NMap Sx Elab BodySoundCore BodyTr CanonTypes.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition body_exit_frame(F inner:A.frame):A.frame:=
 A.Frame(A.fr_mod F)(A.fr_ret F)(A.fr_this F)(A.fr_loop F)(A.fr_var F)(A.fr_fresh inner)(A.fr_applying F)(A.fr_pins F).
Section Context.
Variable w:Elab.world. Variables(R:nat->list nat)(Fl:string->nat).
Lemma cxrel_enter_functor:forall c e F C g,
 CxRel w R Fl c e F C->In(gs_mod g)(w_order w)->
 CxRel w R Fl(cx_applying_in c g)e(A.with_mod F(gm_path(get_mod w(gs_mod g)))(gs_id g))C.
Proof.
 intros c e F C g HC Mod;destruct HC;constructor;cbn[cx_applying_in A.with_mod]in *;auto.
 change(gs_id g::A.fr_applying F=gs_id g::cx_applying c);now rewrite cr_applying.
Qed.
Lemma cxrel_exit_functor:forall c e F C ci ei Fi Ci,
 CxRel w R Fl c e F C->CxRel w R Fl ci ei Fi Ci->en_uf ei=en_uf e->
 CxRel w R Fl(cx_restore c ci)ei(body_exit_frame F Fi)Ci.
Proof.
 intros c e F C ci ei Fi Ci HC HI Uf.
 assert(Ret:A.fr_ret F=tr Fl(canon ei(sg_ret(cx_sig c)))).
 {rewrite(canon_uf e ei Uf);exact(cr_ret _ _ _ _ _ _ _ HC). }
 assert(This:A.fr_this F=option_map(fun t=>tr Fl(canon ei t))(cx_this c)).
 {rewrite(cr_this _ _ _ _ _ _ _ HC);destruct(cx_this c);cbn;[rewrite(canon_uf e ei Uf)|];reflexivity. }
 pose proof(cr_mod_in _ _ _ _ _ _ _ HC)as Mod.
 pose proof(cr_mod _ _ _ _ _ _ _ HC)as Module.
 pose proof(cr_ret_wf _ _ _ _ _ _ _ HC)as RetW.
 pose proof(cr_this_wf _ _ _ _ _ _ _ HC)as ThisW.
 pose proof(cr_loop _ _ _ _ _ _ _ HC)as Loop.
 pose proof(cr_ret_support _ _ _ _ _ _ _ HC)as RetS.
 pose proof(cr_this_support _ _ _ _ _ _ _ HC)as ThisS.
 pose proof(cr_pins _ _ _ _ _ _ _ HC)as Pins.
 pose proof(cr_applying _ _ _ _ _ _ _ HC)as Applying.
 destruct HI;constructor;cbn[cx_restore cx_with_fresh body_exit_frame]in *;auto.
Qed.
End Context.
