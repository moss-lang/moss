(* Lambda annotations read in the enclosing type context, with their
   explicit receiver rather than the enclosing function's receiver. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy BodyTr
 CanonTypes Merge CanonBodyTy SigSound TyOfSubst.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section Read.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Theorem lambda_annotation_read:forall c e F C this syntax s actual final,
 ety_of syntax(cx_mod c)(tymap e)e this s=Good(actual,final)->
 Inv s->CxRel w R Fl c e F C->opt_wf this->Later w P R Fl final->
 A.TyOf WW(A.fr_mod F)(A.c_ty C)(option_map(fun y=>tr Fl(canon e y))this)syntax(tr Fl(canon e actual))/\WFty actual.
Proof.
 intros c e F C this syntax s actual final Run Hi Hc Wthis L.
 destruct(ok_ety_of _ _ _ _ _ _ _ _ Hi Run)as[Ifinal Frame].
 assert(Hw:st_w s=w)by(rewrite <-(fr_w _ _ Frame);exact(lt_w _ _ _ _ _ L)).
 assert(TR:TmRel Fl(tymap e)(map(trp Fl)(tymap e))e).
 {apply tmrel_tymap;[exact(cr_ty_wf _ _ _ _ _ _ _ Hc)|]. intro k;rewrite nlook_trp,nassoc_tymap;reflexivity. }
 destruct(ety_of_rel_at w (P:=P) R Fl b Hload syntax(cx_mod c)(tymap e)(map(trp Fl)(tymap e))e this s actual final
 Run Hi Hw(cr_mod_in _ _ _ _ _ _ _ Hc)TR Wthis L)as[Ty WF].
 split;[|exact WF].
 assert(SR:SubRel(suf Fl(en_uf e))(map(trp Fl)(tymap e))(A.c_ty C)).
 {intro k;rewrite(cr_ty _ _ _ _ _ _ _ Hc),nlook_trp,nassoc_tymap.
  destruct(tn_get e k);cbn[option_map];[rewrite tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc))|];reflexivity. }
 pose proof(tyof_subst WW(suf Fl(en_uf e))_ _ _ _ _ Ty _ SR)as Substituted.
 rewrite tr_canon in Substituted by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
 assert(This:option_map(Audit.app_subst(suf Fl(en_uf e)))(option_map(tr Fl)this)=
 option_map(fun y=>tr Fl(canon e y))this).
 {destruct this;cbn[option_map];[rewrite tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc))|];reflexivity. }
 rewrite This,<-(cr_mod _ _ _ _ _ _ _ Hc)in Substituted;exact Substituted.
Qed.
End Read.
