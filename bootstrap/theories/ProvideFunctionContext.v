(* Function outputs retain the shared signature/application snapshot at their
   actual declaration read, then freeze both meanings canonically. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy
 Merge CanonTypes TySupport SourceSupport SourceClean NominalSupport CanonBodyTy FnSigExec
 ProvideResultReading ProvideResultFunction ProvideResultCanonical ProvideResultContext.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[fn_sig ety_of reqs type_requirements FUEL].
Section Function.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem provide_function_context_read:forall c e F C g tm tmr out s e' keys s' rcv n ps rt body,
 provide_function_code e g tm out s=Good((e',keys),s')->
 Inv s->st_w s=w->nget(w_syms w)(gs_id g)=Some g->
 gs_decl g=ADFn rcv n ps rt body->Later w P R Fl s'->
 CxRel w R Fl c e F C->TmRel Fl tm(map(trp Fl)tm)e->
 (forall k y,nassoc k tm=Some y->Support W y)->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))->
 A.kind_sym W(gs_id g)<>Some A.DKMeth->
 exists sg app stamp,
 e'=set_app(set_fn e(gs_id g)sg)(KyFn(gs_id g))(freeze_app e app)stamp /\
 keys=out++[KyFn(gs_id g)] /\
 A.SigOf W(gs_id g)tmr None(output_sig Fl e sg) /\
 A.AppOf W(gs_id g)tmr(output_inst Fl(freeze_app e app)) /\
 CxRel w R Fl c e' F
 (A.provide C(IFn(gs_id g))(output_inst Fl(freeze_app e app))
   (A.set_fn C(gs_id g)(output_sig Fl e sg))).
Proof.
 intros c e F C g tm tmr out s e' keys s' rcv n ps rt body Run Hi Hw Hg D L HC TR St M K.
 unfold provide_function_code in Run.
 apply bind_inv in Run as(sg&sy&Sig&Run).
 apply bind_inv in Run as(ep&se&Ep&Run);injection Run as <- <- <-.
 unfold provided_at in Ep;apply bind_inv in Ep as(app&sa&Ea&Ep).
 apply bind_inv in Ep as(stamp&ss&Es&Ep);injection Ep as <- <-.
 destruct(output_signature_ok g tm e None _ _ _ Hi Sig)as[Iy Fy].
 destruct(output_provision_ok g tm false _ _ _ Iy Ea)as[Ia Fa].
 destruct(ok_fresh _ _ _ Ia Es)as[_ Fs].
 pose proof(later_back w P R Fl _ _ Fs L)as La.
 pose proof(later_back w P R Fl _ _ Fa La)as Ly.
 assert(Tri:Tri(en_uf e))by exact(proj1(cr_uf _ _ _ _ _ _ _ HC)).
 pose proof(output_signature_canonical w R Fl b Hload g tm tmr e None s sg sy rcv n ps rt body
  Sig Hi Hw Hg D Ly Tri TR I M)as Sg.
 pose proof(output_provision_canonical w R Fl g tm tmr e sy app sa
  Ea Iy La Tri TR M K)as App.
 assert(Owned:A.kind_sym W A.rec_head=None)by exact(loaded_record_unowned w R Fl b Hload).
 assert(Wt:forall k y,nassoc k tm=Some y->WFty y)by exact(proj1(proj2 TR)).
 assert(Tc:TmClean tm)by(intros k y Ey;exact(support_clean Owned(St _ _ Ey)(Wt _ _ Ey))).
 assert(Tnc:TmClean(tymap e)).
 {intros k y Ey;rewrite CanonBodyTy.nassoc_tymap in Ey.
  exact(support_clean Owned(@tn_get_support W e k y(cr_support _ _ _ _ _ _ _ HC)Ey)
   (cr_ty_wf _ _ _ _ _ _ _ HC k y Ey)). }
 assert(Csg:SigOK sg).
 {eapply(fn_sig_clean_at w R Fl b Hload);[exact Sig|exact Hi|exact Hw|exact Hg|exact Tc|exact Tnc|exact I|exact Ly]. }
 assert(Ssg:SigSupport W sg).
 {eapply(fn_sig_support_at w R Fl b Hload);[exact Sig|exact Hi|exact Hw|exact(loader_mod_in w R Fl b Hload _ _ Hg)|exact St|exact Wt|exact(cr_support _ _ _ _ _ _ _ HC)|exact(cr_ty_wf _ _ _ _ _ _ _ HC)|exact I|exact Ly]. }
 exists sg,app,stamp;split;[reflexivity|].
 split;[reflexivity|].
 split;[exact Sg|].
 split;[exact App|].
 eapply(cxrel_function w R Fl);[exact HC|exact Csg|exact Ssg|reflexivity|].
 exists tmr;split;assumption.
Qed.
End Function.
