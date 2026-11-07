(* A direct call records the complete canonical caller instantiation. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy EtyTyExec InstantiateTr.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL canon instantiate ety_ty reqs type_requirements].
Definition call_theta_step(e:env)(r:nat):M(nat*ty):=
 let! y:=ety_ty(canon e(instantiate e(YAbs r)))in ret(r,y).
Lemma ok_call_theta_step : forall e r,ok(call_theta_step e r).
Proof. intros;unfold call_theta_step;apply ok_bind;[apply ok_ety_ty|intro;apply ok_ret]. Qed.
Lemma ok_call_theta : forall e g,ok(call_theta e g).
Proof.
 intros e g;unfold call_theta. apply ok_bind;[apply ok_reqs_frame|intro rs].
 apply ok_mmap;intro r. apply ok_bind;[apply ok_ety_ty|intro;apply ok_ret].
Qed.
Section Read.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat).
Lemma call_theta_loop : forall rs c e F C s th s',
 mmap(fun r=>let! y:=ety_ty(canon e(instantiate e(YAbs r)))in ret(r,y))rs s=Good(th,s')->Inv s->Later w P R Fl s'->CxRel w R Fl c e F C->
 th=map(fun r=>(r,A.canon C(Audit.app_subst(A.c_ty C)(TAbs r))))rs.
Proof.
 intros rs;induction rs as[|r rs IH];intros c e F C s th s' E Hi L Hc.
 - injection E as <- _;reflexivity.
 - cbn[mmap]in E. apply bind_inv in E as[p[sp[Ep E]]].
   apply bind_inv in E as[rest[sr[Er E]]]. injection E as <- <-.
   destruct(ok_call_theta_step e r _ _ _ Hi Ep)as[Ip Fp].
   destruct(ok_mmap _ _ _ _ (ok_call_theta_step e)_ _ _ Ip Er)as[Ir Fr].
   apply bind_inv in Ep as[y[sy[Ey Ep]]]. injection Ep as <- <-.
   rewrite(ety_ty_tr w P R Fl _ _ _ _ Ey Hi(later_back w P R Fl _ _ Fr L)).
   rewrite(instantiate_canonical w R Fl c e F C Hc(YAbs r)).
   cbn[map]. f_equal. exact(IH _ _ _ _ _ _ _ Er Ip L Hc).
Qed.
Theorem call_theta_tr : forall c e F C g s th s',
 call_theta e g s=Good(th,s')->Inv s->Later w P R Fl s'->CxRel w R Fl c e F C->
 th=A.theta_at(BodySoundCore.W w R Fl)C(gs_id g).
Proof.
 intros c e F C g s th s' E Hi L Hc. unfold call_theta in E.
 apply bind_inv in E as[rs[sr[Er E]]].
 assert(Om:ok(mmap(fun r=>let! y:=ety_ty(canon e(instantiate e(YAbs r)))in ret(r,y))rs))by(apply ok_mmap;apply ok_call_theta_step).
 destruct(ok_reqs(gs_id g)_ _ _ Hi Er)as[Ir Fr].
 destruct(Om _ _ _ Ir E)as[_ Fm].
 pose proof(reqs_R w R Fl _ _ _ _ Hi Er(later_back w P R Fl _ _ Fm L))as Req.
 change(th=map(fun r=>(r,A.canon C(Audit.app_subst(A.c_ty C)(TAbs r))))(R(gs_id g))).
 rewrite Req. exact(call_theta_loop _ _ _ _ _ _ _ _ E Ir L Hc).
Qed.
End Read.
