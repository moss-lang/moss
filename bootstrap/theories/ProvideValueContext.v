(* Actual value result installation, including canonical declaration reading
   and the full outgoing context relation. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySound BodyTy
 Merge CanonTypes BindValue SourceSupport NominalSupport ProvideResultReading ProvideResultCanonical
 ProvideResultSubst TyOfSubst.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[ety_of reqs type_requirements FUEL].
Section Value.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySound.W w R Fl).
Theorem provide_value_context_read:forall c e F C g t tm tmr out s e' keys s',
 provide_value_code e g t tm out s=Good((e',keys),s')->
 Inv s->st_w s=w->In(gs_mod g)(w_order w)->Later w P R Fl s'->
 CxRel w R Fl c e F C->TmRel Fl tm(map(trp Fl)tm)e->
 (forall k y,nassoc k tm=Some y->Support W y)->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))->
 A.kind_sym W(gs_id g)<>Some A.DKMeth->
 exists y app stamp,
 e'=set_app(set_val e(gs_id g)y)(KyVal(gs_id g))(freeze_app e app)stamp /\
 keys=out++[KyVal(gs_id g)] /\
 A.TyOf W(gm_path(get_mod w(gs_mod g)))tmr None t(tr Fl(canon e y)) /\
 A.AppOf W(gs_id g)tmr(output_inst Fl(freeze_app e app)) /\
 CxRel w R Fl c e' F
 (A.provide C(IVal(gs_id g))(output_inst Fl(freeze_app e app))
   (A.set_val C(gs_id g)(tr Fl(canon e y)))).
Proof.
 intros c e F C g t tm tmr out s e' keys s' Run Hi Hw Hmi L HC TR St M K.
 unfold provide_value_code in Run.
 apply bind_inv in Run as(y&sy&Et&Run).
 apply bind_inv in Run as(ep&se&Ep&Run);injection Run as <- <- <-.
 pose proof Ep as Provided.
 unfold provided_at in Ep;apply bind_inv in Ep as(app&sa&Ea&Ep).
 apply bind_inv in Ep as(stamp&ss&Es&Ep);injection Ep as <- <-.
 destruct(ok_ety_of t(gs_mod g)tm e None _ _ _ Hi Et)as[Iy Fy].
 destruct(output_provision_ok g tm false _ _ _ Iy Ea)as[Ia Fa].
 destruct(ok_fresh _ _ _ Ia Es)as[_ Fs].
 pose proof(later_back w P R Fl _ _ Fs L)as La.
 pose proof(later_back w P R Fl _ _ Fa La)as Ly.
 assert(Tri:Tri(en_uf e))by exact(proj1(cr_uf _ _ _ _ _ _ _ HC)).
 destruct(output_type_canonical w R Fl b Hload t(gs_mod g)tm e None s y sy tmr
  Et Hi Hw Hmi Ly Tri TR I M)as[Ty Wy].
 assert(Sy:Support W y).
 {eapply(ety_of_support_at w R Fl b Hload);[exact Et|exact Hi|exact Hw|exact Hmi|exact St|exact(proj1(proj2 TR))|exact(cr_support _ _ _ _ _ _ _ HC)|exact(cr_ty_wf _ _ _ _ _ _ _ HC)|exact I|exact Ly]. }
 pose proof(output_provision_canonical w R Fl g tm tmr e sy app sa
  Ea Iy La Tri TR M K)as App.
 exists y,app,stamp;split;[reflexivity|].
 split;[reflexivity|].
 split;[exact Ty|].
 split;[exact App|].
 eapply(cxrel_value w R Fl);[exact HC|exact Wy|exact Sy|reflexivity].
Qed.
End Value.
