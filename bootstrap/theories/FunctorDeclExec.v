(* A real functor environment key resolves to its loaded abstract declaration. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound FunctorDecl.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Loaded.
Variable w:Elab.world. Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySound.W w R Fl).
Theorem cxrel_functor_loaded:forall c e F C u stamp,
 CxRel w R Fl c e F C->nget(en_funs e)u=Some stamp->
 exists g n names args res,nget(w_syms w)u=Some g/\gs_id g=u/\
 gs_decl g=ADFunctor n names args res None.
Proof.
 intros c e F C u stamp HC Have.
 pose proof(cr_funspresent _ _ _ _ _ _ _ HC u)as Present.
 rewrite Have in Present;cbn[is_some]in Present.
 destruct(A.nlook u(A.c_funs C))as[fx|]eqn:Fu;[|discriminate Present].
 destruct(cr_fun_decls _ _ _ _ _ _ _ HC u fx Fu)as(d&n&names&args&res&Decl&Ast).
 unfold BodySound.W,W_read in Decl;cbn[A.w_decl]in Decl.
 destruct(nget(w_syms w)u)as[g|]eqn:Hg;[|discriminate Decl].
 injection Decl as <-;cbn[A.d_ast]in Ast.
 exists g,n,names,args,res;split;[reflexivity|];split;[exact(load_world_ids b w Hload u g Hg)|exact Ast].
Qed.
Theorem cxrel_functor_sym:forall c e F C u stamp si g sf,
 CxRel w R Fl c e F C->nget(en_funs e)u=Some stamp->
 sym u si=Good(g,sf)->Inv si->st_w si=w->
 sf=si/\nget(w_syms w)(gs_id g)=Some g/\
 exists n names args res,gs_decl g=ADFunctor n names args res None.
Proof.
 intros c e F C u stamp si g sf HC Have E Hi Hw.
 destruct(cxrel_functor_loaded _ _ _ _ _ _ HC Have)as(g0&n&names&args&res&Hg&Id&D).
 unfold sym in E.
 rewrite(iv_loader _ Hi b ltac:(rewrite Hw;exact Hload)u g0 ltac:(rewrite Hw;exact Hg))in E.
 injection E as <- <-;split;[reflexivity|];split;[rewrite Id;exact Hg|].
 exists n,names,args,res;exact D.
Qed.
End Loaded.
