(* The selector's real receiver reader has the canonical semantic receiver meaning. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTr BodyTy
 SigSound Merge CanonTypes TyOfSubst ProvideResultSubst.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Receiver.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Theorem family_receiver_canonical:forall e mi rg sub s pat sf tmr,
 receiver_at e mi rg sub s=Good(pat,sf)->Inv s->st_w s=w->
 nget(w_syms w)(gs_id rg)=Some rg->Later w P R Fl sf->Tri(en_uf e)->UfWF(en_uf e)->
 TmRel Fl(sub++tymap e)(map(trp Fl)(sub++tymap e))e->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k(sub++tymap e)))->
 A.RecvTy(BodySoundCore.W w R Fl)(gs_id rg)tmr(tr Fl pat)/\WFty pat/\canon e pat=pat.
Proof.
 intros e mi rg sub s pat sf tmr Run Hi Hw Hg L T UF TR Mean.
 unfold receiver_at in Run;apply bind_inv in Run as(raw&sr&Read&Ret);injection Ret as <- <-.
 destruct(recvty_exec w R Fl b Hload mi rg(sub++tymap e)(map(trp Fl)(sub++tymap e))e s raw sr
 Read Hi Hw Hg TR L)as[Recv Wraw].
 assert(SR:SubRel(suf Fl(en_uf e))(map(trp Fl)(sub++tymap e))tmr).
 {intro k;rewrite nlook_trp,Mean;destruct(nassoc k(sub++tymap e));cbn[option_map];[rewrite tr_canon by exact T|];reflexivity. }
 pose proof(output_receiver_subst(BodySoundCore.W w R Fl)(suf Fl(en_uf e))_ _ _ Recv tmr
 SR)as CanonRecv.
 rewrite tr_canon in CanonRecv by exact T;split;[exact CanonRecv|];split.
 - exact(canon_wf _ _ UF Wraw).
 - apply canon_idem;exact T.
Qed.
End Receiver.
