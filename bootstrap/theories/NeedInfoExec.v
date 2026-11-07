(* Structurally clean needs of actual callee contexts, including cache hits. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy
 Merge CanonTypes ContextClean CtxWalk CtxMerge InfoCache CtxMethodExec NeedKeys SettledWalk.
Import ListNotations.
Section Needs.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Lemma assumes_merged_needs_ok:forall g s e needs s',
 (forall t,gs_origin g<>OForwarder t)->
 (forall s0 m s1,st_w s0=w->is_main g s0=Good(m,s1)->m=false)->
 In(gs_mod g)(w_order w)->merged_walk(gs_mod g)(fi_walk g)s=Good((e,needs),s')->
 Inv s->st_w s=w->Later w P R Fl s'->Forall(fun k=>key_ok k=true)needs.
Proof.
 intros g s e needs s' Nf Nm Hmi H Hi Hw L.
 assert(Ew:forall e0,fi_walk g e0=
 (let! acc:=mfold(fun acc it=>add_assume_item(fst acc)(gs_mod g)it(snd acc))(e0,[])(gs_assumes g)in
 let! main:=is_main g in if main then add_root_context(fst acc)(gs_mod g)(snd acc)else ret acc)).
 { intro e0. unfold fi_walk. destruct(gs_origin g)eqn:O;try reflexivity. exfalso;exact(Nf _ eq_refl). }
 assert(Final:exists tm les,WS Fl(en_uf e)e tm les needs).
 { eapply(merged_walk_ws w(P:=P)R Fl(fi_walk g))with(mi:=gs_mod g)(s:=s)(s':=s');try assumption.
   - intro e0. rewrite Ew. apply ok_bind.
     + apply ok_mfold;intros;apply ok_add_assume_item.
     + intro acc. apply ok_bind;[apply ok_is_main|intro m;destruct m;[unfold add_root_context;okt|apply ok_ret]].
   - intros U si eo ns sf T O Ii Wi Li E. rewrite Ew in E.
     apply bind_inv in E as([e1 n1]&s1&E1&E).
     destruct(ok_mfold _ _ _ (gs_assumes g)(env_with_edges [] U,[])
      (fun acc it=>ok_add_assume_item(fst acc)(gs_mod g)it(snd acc))si _ s1 Ii E1)as[I1 F1].
     apply bind_inv in E as(m&s2&E2&E). pose proof(Nm s1 m s2 ltac:(rewrite(fr_w _ _ F1);exact Wi)E2)as Em. subst m.
     injection E as -> -> ->. destruct(ok_is_main g _ _ _ I1 E2)as[_ F2].
     destruct(assumes_walk w R Fl b Hload(@meth_step_holds w P R Fl b Hload)
      (gs_mod g)(gs_assumes g)(env_with_edges [] U)[] si eo ns s1 U [] [] E1 Ii Wi Hmi
      (later_back w P R Fl _ _ F2 Li)(ws_start Fl U T O))as(les&es&tm&_&_&W).
     exists tm,les. exact W. }
 destruct Final as(tm&les&W). exact(ws_needs_ok Fl(en_uf e)e tm les needs W eq_refl).
Qed.
Theorem fn_info_needs_ok_any_nonroot:forall g s i s',
 nget(w_syms w)(gs_id g)=Some g->(forall t,gs_origin g<>OForwarder t)->
 (forall s0 m s1,st_w s0=w->is_main g s0=Good(m,s1)->m=false)->
 fn_info g s=Good(i,s')->Inv s->InfoInv s->st_w s=w->Later w P R Fl s'->
 Forall(fun k=>key_ok k=true)(fi_needs i).
Proof.
 intros g s i s' Hg Nf Nm H Hi I Hw L.
 assert(Hb:load_world b=Good(st_w s))by(rewrite Hw;exact Hload).
 assert(Gs:nget(st_syms s)(gs_id g)=Some g)by(apply(iv_loader _ Hi b Hb);rewrite Hw;exact Hg).
 destruct(nget(st_info s)(gs_id g))as[j|]eqn:Ej.
 - pose proof H as H0. unfold fn_info in H0. apply bind_inv in H0 as(x&s2&E&H0). injection E as <- <-.
   rewrite Ej in H0. injection H0 as <- <-.
   destruct(I _ _ Ej)as[(g'&s0&e&needs&s1&G0&Hi0&H1&->&F)|N].
   + destruct(ok_merged_fi g' s0 _ s1 Hi0 H1)as[_ F1].
     assert(W0:st_w s0=w)by(rewrite <-Hw,(fl_w _ _ F),(fr_w _ _ F1);reflexivity).
     assert(g'=g).
     { assert(Hb0:load_world b=Good(st_w s0))by(rewrite W0;exact Hload).
       pose proof(iv_loader _ Hi0 b Hb0(gs_id g)g ltac:(rewrite W0;exact Hg))as E0. congruence. }
     subst g'. exact(assumes_merged_needs_ok g s0 e needs s1 Nf Nm
       (loader_mod_in w R Fl b Hload _ _ Hg)H1 Hi0 W0(later_fromL w R Fl _ _ F L)).
   + exfalso. pose proof(proj1 N b Hb)as N0. rewrite Hw in N0. congruence.
 - destruct(fn_info_miss g s i s' H Ej)as(g'&e&needs&s1&Eg&H1&->&->).
   rewrite Gs in Eg;injection Eg as <-.
   exact(assumes_merged_needs_ok g s e needs s1 Nf Nm(loader_mod_in w R Fl b Hload _ _ Hg)H1 Hi Hw(later_info w R Fl s1 _ L)).
Qed.
End Needs.
