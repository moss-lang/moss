(* A defined functor executes its real bind sequence in its declaring module. *)
From Stdlib Require Import List String Bool.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTr BodyTy
 InfoPreserve BindSequence BodyUf ApplyBodyContext.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Body.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySound.W w R Fl).
Theorem apply_body_from_items:forall f,OneStep w P R Fl f->
 forall c e F C pa g n names args res bodies s xs e' c' sf,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res(Some bodies)->
 A.resolve_sym W(A.fr_mod F)pa=Some(gs_id g)->
 apply_body(S f)c e g s=Good((xs,e',c'),sf)->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl sf F->
 BindResult w P R Fl c e F C(Spec(Some pa)None None,None)xs e' c'.
Proof.
 intros f Items c e F C pa g n names args res bodies s xs e' c' sf Hg D Resolve Run Hi Ii Fi HC Post.
 pose proof(uf_apply_body _ (body_uf(S f))c e g s((xs,e',c'))sf Run)as Uf;cbn[Env3]in Uf.
 cbn[apply_body]in Run;destruct(nmem(gs_id g)(cx_applying c))eqn:Active.
 1:{exfalso;eapply err_not_good;exact Run. }
 unfold functor_parts in Run;rewrite D in Run;cbn beta iota in Run.
 apply bind_inv in Run as([[ys ey]cy]&sy&Walk&Ret);injection Ret as EqXs EqEnv EqCx EqState;subst ys ey c' sy.
 change(mfold(bind_fold_step f)([],e,cx_applying_in c g)(List.concat bodies)s=Good((xs,e',cy),sf))in Walk.
 assert(Hinner:CxRel w R Fl(cx_applying_in c g)e(A.with_mod F(gm_path(get_mod w(gs_mod g)))(gs_id g))C).
 {apply cxrel_enter_functor;[exact HC|exact(loader_mod_in w R Fl b Hload _ _ Hg)]. }
 destruct(bind_items_exec w P R Fl f Items(List.concat bodies)[](cx_applying_in c g)e s xs e' cy sf
 (A.with_mod F(gm_path(get_mod w(gs_mod g)))(gs_id g))C Walk Hi Ii Fi Hinner
 (post_frame w P R Fl sf F _ eq_refl Post))as(zs&F1&C1&made&wrap&Eq&HC1&Var&Fresh&Typed&Wrap).
 cbn[List.app]in Eq;subst zs.
 exists(body_exit_frame F F1),C1,made,wrap.
 split;[apply(cxrel_exit_functor w R Fl c e F C cy e' F1 C1);assumption|].
 split;[reflexivity|];split;[exact Fresh|];split;[|exact Wrap].
 eapply A.BApplyBody with(u:=gs_id g)(d:=A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g))
 (n:=n)(ps:=names)(a:=args)(r:=res)(bodies:=bodies)(F':=F1).
 - exact Resolve.
 - exact(w_decl_read w R Fl _ _ Hg).
 - cbn[A.d_ast];exact D.
 - intro Hin;rewrite(cr_applying _ _ _ _ _ _ _ HC)in Hin.
   assert(nmem(gs_id g)(cx_applying c)=true)by(apply nmem_in;exact Hin);congruence.
 - exact Typed.
Qed.
End Body.
