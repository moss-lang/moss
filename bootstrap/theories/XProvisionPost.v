(* Compositional execution posts for the shared environment invariant. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel CoreMoss XTyping XEnv ReifyContracts XProvision XSafety XHeapC MossX.
Import ListNotations.
Set Implicit Arguments.
Section Posts.
Variables(Sg:gsig)(Rp:nsym->option rep)(P:xprog)(Fb:usym->option(tele*tele*subst)).

(* Only meanings reachable from the caller's scope must be retained.  This
   covers immutable exports without constraining irrelevant substitution rows. *)
Definition provision_binds_post(H:sty)(Phi Phi':ctx)(g:subst)
 (r:(xdenv*xvenv*MossX.mstate)+MossX.XS.xres):Prop:=
 match r with
 | inl(d',ge',st')=>ge'=[] /\exists H' g',sext H H' /\
     XHeapC.ConcreteHeap.heap_ok(XSafetyC.PsiH Sg Rp P H')st' /\
     ctx_grounded g' Phi' /\provision_environment Sg Rp P Fb H' g'(cA Phi')d' /\
     meanings_on(cA Phi)g' g
 | inr rr=>XSafetyC.ok_ctl Sg Rp P H rr
 end.
Definition provision_apply_post(H:sty)(Phi:ctx)(Dr:tele)(g:subst)(d:xdenv)
 (r:(xdenv*MossX.mstate)+MossX.XS.xres):Prop:=
 match r with
 | inl(dr,st')=>exists H' g',sext H H' /\
     XHeapC.ConcreteHeap.heap_ok(XSafetyC.PsiH Sg Rp P H')st' /\
     ctx_grounded g'(open_res Dr Phi) /\
     provision_environment Sg Rp P Fb H' g'(cA(open_res Dr Phi))(dr++d) /\
     meanings_on(cA Phi)g' g
 | inr rr=>XSafetyC.ok_ctl Sg Rp P H rr
 end.

Lemma provision_binds_post_step : forall H H1 Phi Phi1 Phi' g g1 r,
 provision_binds_post H1 Phi1 Phi' g1 r ->sext H H1 ->
 incl(tyreqs(cA Phi))(tyreqs(cA Phi1)) ->meanings_on(cA Phi)g1 g ->
 provision_binds_post H Phi Phi' g r.
Proof.
 intros H H1 Phi Phi1 Phi' g g1 [[ [d' ge'] st']|rr] Extends Extend Sub Eq.
 - destruct Extends as[Ge[H'[g'[Ext[Heap[Ground[Env Mean]]]]]]].
   split;[exact Ge|].
   exists H',g';split;[eapply sext_trans;eassumption|].
   split;[exact Heap|].
   split;[exact Ground|].
   split;[exact Env|].
   intros t Hin;rewrite(Mean t(Sub t Hin));apply Eq;exact Hin.
 - eapply XSafetyC.ok_ctl_ext;eassumption.
Qed.

Lemma provision_apply_post_ext : forall H H1 Phi Dr g d r,
 sext H H1 ->provision_apply_post H1 Phi Dr g d r ->
 provision_apply_post H Phi Dr g d r.
Proof.
 intros H H1 Phi Dr g d [[dr st']|rr] Extend Post.
 - destruct Post as[H'[g'[Ext[Heap[Ground[Env Mean]]]]]].
   exists H',g';split;[eapply sext_trans;eassumption|].
   split;[exact Heap|].
   split;[exact Ground|].
   split;assumption.
 - eapply XSafetyC.ok_ctl_ext;eassumption.
Qed.

Lemma provision_control_not_error : forall H r c,
 XSafetyC.ok_ctl Sg Rp P H r ->MossX.XS.xr_out r<>XErr c.
Proof.
 intros H [out ge st] c Safe;destruct out;cbn in Safe|-*;try contradiction;discriminate.
Qed.
End Posts.
