(* Recover the final actual walk invariant without needing merge principality. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySoundCore Merge CanonTypes ContextClean CtxWalk CtxMerge NeedKeys.
Import ListNotations.
Section Settled.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(walk:env->M(env*list key)).
Hypothesis Hok:forall e,ok(walk e).
Hypothesis Hround:forall U s e needs s',Tri U->UfOK U->Inv s->st_w s=w->Later w P R Fl s'->
 walk(env_with_edges [] U)s=Good((e,needs),s')->exists tm les,WS Fl U e tm les needs.
Lemma settled_go_ws:forall mi n U s e needs s',
 mw_go walk mi n [] U s=Good((e,needs),s')->Tri U->UfOK U->Inv s->st_w s=w->Later w P R Fl s'->
 exists tm les,WS Fl(en_uf e)e tm les needs.
Proof.
 intros mi n. induction n as[|n IH];intros U s e needs s' H T O Hi Hw L;
 cbn[mw_go]in H;apply bind_inv in H as([e1 n1]&s1&H1&H2);
 destruct(Hok _ _ _ _ Hi H1)as[I1 F1];cbn beta iota zeta in H2.
 - destruct(edges_eqb _ _)eqn:E;[|exfalso;exact(err_not_good _ _ _ _ _ _ H2)].
   injection H2 as -> -> ->.
   destruct(Hround U s e needs s' T O Hi Hw L H1)as(tm&les&W).
   rewrite(tne_nil _ _ _ _ _ _ W)in E. destruct(ws_ext _ _ _ _ _ _ W)as(nw&En).
   rewrite En in E. pose proof(edges_settled _ _ E)as ->. rewrite app_nil_r in En.
   exists tm,les;rewrite En;exact W.
 - destruct(edges_eqb _ _)eqn:E.
   + injection H2 as -> -> ->.
     destruct(Hround U s e needs s' T O Hi Hw L H1)as(tm&les&W).
     rewrite(tne_nil _ _ _ _ _ _ W)in E. destruct(ws_ext _ _ _ _ _ _ W)as(nw&En).
     rewrite En in E. pose proof(edges_settled _ _ E)as ->. rewrite app_nil_r in En.
     exists tm,les;rewrite En;exact W.
   + destruct(ok_mw_go walk Hok mi n _ _ _ _ _ I1 H2)as[_ F2].
     destruct(Hround U s e1 n1 s1 T O Hi Hw(later_back w P R Fl _ _ F2 L)H1)as(tm&les&W).
     rewrite(tne_nil _ _ _ _ _ _ W)in H2.
     exact(IH(en_uf e1)s1 e needs s' H2(ws_tri _ _ _ _ _ _ W)(cc_uf _(ws_clean _ _ _ _ _ _ W))
       I1 ltac:(rewrite(fr_w _ _ F1);exact Hw)L).
Qed.
Theorem merged_walk_ws:forall mi s e needs s',
 merged_walk mi walk s=Good((e,needs),s')->Inv s->st_w s=w->Later w P R Fl s'->
 exists tm les,WS Fl(en_uf e)e tm les needs.
Proof.
 intros mi s e needs s' H Hi Hw L. rewrite(mw_go_eq walk mi)in H.
 exact(settled_go_ws mi 8 [] s e needs s' H TriNil(fun g t Hin=>match Hin with end)Hi Hw L).
Qed.
End Settled.
