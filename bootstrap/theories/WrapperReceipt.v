(* Cached receiver adapters retain genuine emitted lambda provenance. *)
From Stdlib Require Import List String Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame WorldSound
 InfoInvariant FinalLambdas ForwarderTrace.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Lemma wrapper_entry_body:forall s p id P,
 WrapperEntry s p id->FinalLambdas P s->
 exists pv info th sm,
 gs_id pv=p /\nget(st_syms s)p=Some pv /\InfoEntry s p info /\
 A.body_of P id=Some(XCallD p th sm(map XVar(seq 1(List.length(fn_params pv))))).
Proof.
 intros s p id P Entry Final.
 destruct Entry as(pv&info&th&sm&sms&sme&ths&the&news&newe&Id&Physical&Cache&Is&Map&Fs&It&Theta&Ft&Inw&Phnew&New&Fn&Symbol&Row).
 exists pv,info,th,sm;repeat split;try assumption.
 eapply Final;[exact Row|exact Symbol|reflexivity].
Qed.
Lemma wrapper_entry_fresh:forall b w R Fl s p id,
 load_world b=Good w->st_w s=w->WrapperEntry s p id->A.Fresh(W_read w R Fl)id.
Proof.
 intros b w R Fl s p id Load World Entry.
 destruct Entry as(pv&info&th&sm&sms&sme&ths&the&news&newe&Id&Physical&Cache&Is&Map&Fs&It&Theta&Ft&Inw&Phnew&New&Fn&Symbol&Row).
 pose proof(new_sym_shape _ _ _ _ _ _ _ _ New)as[Allocated _].
 assert(O:ok(new_sym(gs_mod pv)(gs_name pv++"$recv")%string KFn(gs_decl pv)OLambda))by okt.
 destruct(O _ _ _ Inw New)as[_ Frame].
 assert(Wnew:st_w news=w)by(rewrite <-(fr_w _ _ Frame),<-(fl_w _ _ Fn);exact World).
 subst id;split.
 - change(match nget(w_syms w)(st_next news)with Some g=>Some(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g))|None=>None end=None).
   destruct(nget(w_syms w)(st_next news))as[g|]eqn:Lookup;[|reflexivity].
   pose proof(iv_loader _ Inw b ltac:(rewrite Wnew;exact Load)_ _ ltac:(rewrite Wnew;exact Lookup))as Has.
   rewrite(iv_above _ Inw _ (le_n _))in Has;discriminate.
 - assert(~st_next news<=p)by(intro Bound;rewrite(iv_above _ Inw _ Bound)in Phnew;discriminate).
   lia.
Qed.
