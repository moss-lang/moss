(* Actual signature reads with an explicit method receiver retain validity/support. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTy BodyTr CanonTypes
 CanonBodyTy NominalSupport TySupport SourceSupport SourceClean NeedMapExec FnSigExec.
Import ListNotations.
Section Receiver.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).
Theorem fn_sig_receiver_properties:forall c e F C g r s sg s',
 fn_sig g(tymap e)e(Some r)s=Good(sg,s')->Inv s->st_w s=w->
 nget(w_syms w)(gs_id g)=Some g->CxRel w R Fl c e F C->WFty r->Support WW r->Later w P R Fl s'->
 SigOK sg/\SigSupport WW sg.
Proof.
 intros c e F C g r s sg s' E Hi Hw Hg Hc Wr Sr L.
 destruct(caller_types_ok w R Fl b Hload c e F C Hc)as[U T].
 assert(Tc:TmClean(tymap e)).
 {intros k y Ey;rewrite nassoc_tymap in Ey;exact(T _ _ Ey). }
 assert(Cr:ety_ok r=true)by exact(support_clean(loaded_record_unowned w R Fl b Hload)Sr Wr).
 split.
 - exact(@fn_sig_clean_at w P R Fl b Hload g(tymap e)e(Some r)s sg s' E Hi Hw Hg Tc Tc Cr L).
 - eapply(@fn_sig_support_at w P R Fl b Hload);[exact E|exact Hi|exact Hw|exact(loader_mod_in w R Fl b Hload _ _ Hg)| | |exact(cr_support _ _ _ _ _ _ _ Hc)|exact(cr_ty_wf _ _ _ _ _ _ _ Hc)|split;assumption|exact L].
   + intros k y Ey;rewrite nassoc_tymap in Ey;exact(@tn_get_support WW e k y(cr_support _ _ _ _ _ _ _ Hc)Ey).
   + intros k y Ey;rewrite nassoc_tymap in Ey;exact(cr_ty_wf _ _ _ _ _ _ _ Hc _ _ Ey).
Qed.
End Receiver.
