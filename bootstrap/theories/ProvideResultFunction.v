(* Actual function output installation reads its declaration and captures the
   instantiation at that read, before installing the next output occurrence. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore
 BodyTy CtxWalk SigSound ProvideResultReading.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Definition provide_function_code(e:env)(g:gsym)(tm:list(nat*ety))(out:list key)
 :M(env*list key):=
 let!sg:=fn_sig g tm e None in
 let!ep:=provided_at(set_fn e(gs_id g)sg)(KyFn(gs_id g))g tm false in
 ret(ep,out++[KyFn(gs_id g)]).
Lemma output_signature_ok:forall g tm e this,ok(fn_sig g tm e this).
Proof. intros;unfold fn_sig;okt. Qed.
Section Function.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Local Opaque reqs type_requirements provision_app fn_sig ety_of.
Theorem provide_function_read:forall e g tm tmr out s e' keys s' rcv n ps rt body,
 provide_function_code e g tm out s=Good((e',keys),s')->
 Inv s->st_w s=w->nget(w_syms w)(gs_id g)=Some g->
 gs_decl g=ADFn rcv n ps rt body->Later w P R Fl s'->
 TmRel Fl tm tmr e->A.kind_sym WW(gs_id g)<>Some A.DKMeth->
 exists sg a stamp,
 A.SigOf WW(gs_id g)tmr None(tr_sig Fl sg) /\ sg_this sg=None /\
 A.AppOf WW(gs_id g)tmr(output_inst Fl a) /\
 e'=set_app(set_fn e(gs_id g)sg)(KyFn(gs_id g))(freeze_app(set_fn e(gs_id g)sg)a)stamp /\
 keys=out++[KyFn(gs_id g)].
Proof.
 intros e g tm tmr out s e' keys s' rcv n ps rt body E Hi Hw Hg D L T K.
 unfold provide_function_code in E;apply bind_inv in E as(sg&sy&Et&E).
 apply bind_inv in E as(ep&se&Ep&E);injection E as <- <- <-.
 unfold provided_at in Ep;apply bind_inv in Ep as(a&sa&Ea&Ep).
 apply bind_inv in Ep as(stamp&ss&Es&Ep);injection Ep as <- <-.
 pose proof(output_signature_ok g tm e None)as Ot.
 destruct(Ot _ _ _ Hi Et)as[Iy Fy].
 pose proof(output_provision_ok g tm false)as Oa.
 destruct(Oa _ _ _ Iy Ea)as[Ia Fa].
 destruct(ok_fresh _ _ _ Ia Es)as[_ Fs].
 pose proof(later_back w P R Fl _ _ Fs L)as La.
 pose proof(later_back w P R Fl _ _ Fa La)as Ly.
 destruct(fn_sig_rel_at w R Fl b Hload g tm tmr e None s sg sy rcv n ps rt body
   Et Hi Hw Hg D T I Ly)as[Sg Th].
 exists sg,a,stamp;repeat split;try exact Sg;try exact Th;try reflexivity.
 eapply ordinary_provision_rel;[exact Ea|exact Iy|exact La|exact T|exact K].
Qed.
End Function.
