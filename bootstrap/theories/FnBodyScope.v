From NM Require Import BodyProofTools.
(* Recover the real body run and its preserved invariants inside scoped state. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound ElabFrame ReqsSolve ReqsStable InfoInvariant InfoPreserve BodySoundCore BodyConst.
Import ListNotations.
Lemma info_reify_state:forall change s,InfoInv s->InfoInv(s_reify change s).
Proof.
 intros change s I;eapply info_step;[exact I| |reflexivity|reflexivity].
 constructor;cbn;auto;[exists[];rewrite app_nil_r;reflexivity|apply ext_refl].
Qed.
Lemma later_reify_state:forall w P R Fl change s,
 Later w P R Fl(s_reify change s)<->Later w P R Fl s.
Proof. intros;split;intro L;destruct L;constructor;assumption. Qed.
Section Scope.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Lemma in_reify_checked_scope:forall A owner(m:M A)s a final,
 Inv s->InfoInv s->FwdInv s->in_reify_code owner m s=Good(a,final)->Later w P R Fl final->
 exists start last,m start=Good(a,last)/\Inv start/\InfoInv start/\FwdInv start/\
 Later w P R Fl last/\st_vars start=st_vars s/\st_nvar start=st_nvar s/\st_w start=st_w s.
Proof.
 intros A owner m s a final Inv Cache Fwd E Late;unfold in_reify_code in E.
 apply bind_inv in E as[saved[s0[Get E]]];injection Get as <- <-.
 apply bind_inv in E as[u[s1[Change E]]];unfold upd in Change;injection Change as <- <-.
 apply bind_inv in E as[value[last[Body E]]].
 apply bind_inv in E as[u'[s2[Restore E]]];unfold upd in Restore;injection Restore as <- <-.
 injection E as <- <-.
 eexists;exists last;split;[exact Body|];split.
 - destruct Inv;constructor;assumption.
 - split;[apply info_reify_state;exact Cache|];split;[exact Fwd|].
   split;[apply(proj1(later_reify_state _ _ _ _ _ _)Late)|];repeat split;reflexivity.
Qed.
Lemma with_vars_checked_scope:forall A init(m:M A)s a final,
 Inv s->InfoInv s->FwdInv s->ok m->with_vars init m s=Good(a,final)->Later w P R Fl final->
 VShape(s_vars init(List.length init)s)->
 exists last,m(s_vars init(List.length init)s)=Good(a,last)/\
 Inv(s_vars init(List.length init)s)/\InfoInv(s_vars init(List.length init)s)/\FwdInv(s_vars init(List.length init)s)/\
 Later w P R Fl last/\VShape last/\Fr(s_vars init(List.length init)s)last.
Proof.
 intros A init m s a final Inv Cache Fwd OK E Late Shape;unfold with_vars in E.
 apply bind_inv in E as[saved[s0[Get E]]];injection Get as <- <-.
 apply bind_inv in E as[u[s1[Change E]]];unfold upd in Change;injection Change as <- <-.
 apply bind_inv in E as[value[last[Body E]]].
 apply bind_inv in E as[u'[s2[Restore E]]];unfold upd in Restore;injection Restore as <- <-.
 injection E as <- <-.
 assert(InvStart:ElabFrame.Inv(s_vars init(List.length init)s))by(destruct Inv;constructor;assumption).
 destruct(OK _ _ _ InvStart Body)as[InvLast Frame].
 exists last;split;[exact Body|];split;[exact InvStart|];split;[apply info_vars;exact Cache|];split;[exact Fwd|].
 split;[apply(proj1(later_vars w P R Fl _ _ _)Late)|].
 split;[exact(fx_vshape _ _(fr_x _ _ Frame)Shape)|exact Frame].
Qed.
End Scope.
