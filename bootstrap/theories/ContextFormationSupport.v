(* Actual ordinary declaration contexts, including every merge rebuild. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTy
 Merge CanonTypes SourceClean ContextClean CtxWalk CtxMerge NominalSupport
 ContextSupport ContextWalkSupport ContextMergeSupport InfoCache.
Import ListNotations.
Section Formation.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).
Theorem assumes_merged_support:forall g s e needs s',
 (forall t,gs_origin g<>OForwarder t)->
 (forall s0 m s1,st_w s0=w->is_main g s0=Good(m,s1)->m=false)->
 In(gs_mod g)(w_order w)->merged_walk(gs_mod g)(fi_walk g)s=Good((e,needs),s')->
 Inv s->st_w s=w->Later w P R Fl s'->
 ContextClean e /\ Tri(en_uf e) /\ EnvSupport WW e.
Proof.
 intros g s e needs s' Nf Nm Hmi H Hi Hw L.
 assert(Ew:forall e0,fi_walk g e0=
 (let! acc:=mfold(fun acc it=>add_assume_item(fst acc)(gs_mod g)it(snd acc))(e0,[])(gs_assumes g)in
  let! main:=is_main g in if main then add_root_context(fst acc)(gs_mod g)(snd acc)else ret acc)).
 { intro e0. unfold fi_walk. destruct(gs_origin g)eqn:O;try reflexivity. exfalso;exact(Nf _ eq_refl). }
 apply(merged_walk_support w (P:=P) R Fl(fi_walk g))with(mi:=gs_mod g)(s:=s)(needs:=needs)(s':=s');try assumption.
 - intro e0. rewrite Ew. apply ok_bind.
   + apply ok_mfold;intros;apply ok_add_assume_item.
   + intro acc. apply ok_bind;[apply ok_is_main|intro m;destruct m;[unfold add_root_context;okt|apply ok_ret]].
 - intros U s0 e0 ns sf T O SU Ii Wi Li E. rewrite Ew in E.
   apply bind_inv in E as([e1 n1]&s1&E1&E).
   destruct(ok_mfold _ _ _ (gs_assumes g)(env_with_edges [] U,[])
    (fun acc it=>ok_add_assume_item(fst acc)(gs_mod g)it(snd acc))s0 _ s1 Ii E1)as[I1 F1].
   apply bind_inv in E as(m&s2&E2&E).
   pose proof(Nm s1 m s2 ltac:(rewrite(fr_w _ _ F1);exact Wi)E2)as Em. subst m.
   injection E as -> -> ->.
   destruct(ok_is_main g _ _ _ I1 E2)as[_ F2].
   destruct(support_edges_start WW U O T SU)as[C0[T0 S0]].
   exact(assume_items_context_support w R Fl b Hload(gs_mod g)(gs_assumes g)(env_with_edges [] U)[]
    s0 e0 ns s1 E1 Ii Wi Hmi C0 T0 S0(later_back w P R Fl _ _ F2 Li)).
Qed.
Theorem fn_info_support_nonroot:forall g s i s',
 nget(w_syms w)(gs_id g)=Some g->(forall t,gs_origin g<>OForwarder t)->
 (forall s0 m s1,st_w s0=w->is_main g s0=Good(m,s1)->m=false)->
 fn_info g s=Good(i,s')->nget(st_info s)(gs_id g)=None->
 Inv s->st_w s=w->Later w P R Fl s'->EnvSupport WW(fi_env i).
Proof.
 intros g s i s' Hg Nf Nm H N Hi Hw L.
 destruct(fn_info_miss g s i s' H N)as(g'&e&needs&s1&Eg&H1&->&->).
 rewrite(iv_loader _ Hi b ltac:(rewrite Hw;exact Hload)_ _ ltac:(rewrite Hw;exact Hg))in Eg.
 injection Eg as <-. apply later_info in L.
 apply freeze_all_apps_support.
 exact(proj2(proj2(assumes_merged_support g s e needs s1 Nf Nm
  (loader_mod_in w R Fl b Hload _ _ Hg)H1 Hi Hw L))).
Qed.
Theorem fn_info_support_any_nonroot:forall g s i s',
 nget(w_syms w)(gs_id g)=Some g->(forall t,gs_origin g<>OForwarder t)->
 (forall s0 m s1,st_w s0=w->is_main g s0=Good(m,s1)->m=false)->
 fn_info g s=Good(i,s')->Inv s->InfoInv s->st_w s=w->Later w P R Fl s'->
 EnvSupport WW(fi_env i).
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
     subst g'. apply freeze_all_apps_support.
     exact(proj2(proj2(assumes_merged_support g s0 e needs s1 Nf Nm
      (loader_mod_in w R Fl b Hload _ _ Hg)H1 Hi0 W0(later_fromL w R Fl _ _ F L)))).
   + exfalso. pose proof(proj1 N b Hb)as N0. rewrite Hw in N0. congruence.
 - exact(fn_info_support_nonroot g s i s' Hg Nf Nm H Ej Hi Hw L).
Qed.
Theorem fn_info_valid_nonroot:forall g s i s',
 nget(w_syms w)(gs_id g)=Some g->(forall t,gs_origin g<>OForwarder t)->
 (forall s0 m s1,st_w s0=w->is_main g s0=Good(m,s1)->m=false)->
 fn_info g s=Good(i,s')->nget(st_info s)(gs_id g)=None->
 Inv s->st_w s=w->Later w P R Fl s'->
 ContextClean(fi_env i) /\ Tri(en_uf(fi_env i)) /\ EnvSupport WW(fi_env i).
Proof.
 intros g s i s' Hg Nf Nm H N Hi Hw L.
 destruct(fn_info_miss g s i s' H N)as(g'&e&needs&s1&Eg&H1&->&->).
 rewrite(iv_loader _ Hi b ltac:(rewrite Hw;exact Hload)_ _ ltac:(rewrite Hw;exact Hg))in Eg.
 injection Eg as <-. apply later_info in L.
 destruct(assumes_merged_support g s e needs s1 Nf Nm
  (loader_mod_in w R Fl b Hload _ _ Hg)H1 Hi Hw L)as[C[T S]].
 exact(freeze_all_apps_valid WW e C T S).
Qed.
Theorem fn_info_valid_any_nonroot:forall g s i s',
 nget(w_syms w)(gs_id g)=Some g->(forall t,gs_origin g<>OForwarder t)->
 (forall s0 m s1,st_w s0=w->is_main g s0=Good(m,s1)->m=false)->
 fn_info g s=Good(i,s')->Inv s->InfoInv s->st_w s=w->Later w P R Fl s'->
 ContextClean(fi_env i) /\ Tri(en_uf(fi_env i)) /\ EnvSupport WW(fi_env i).
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
     subst g'. destruct(assumes_merged_support g s0 e needs s1 Nf Nm
      (loader_mod_in w R Fl b Hload _ _ Hg)H1 Hi0 W0(later_fromL w R Fl _ _ F L))as[C[T S]].
     exact(freeze_all_apps_valid WW e C T S).
   + exfalso. pose proof(proj1 N b Hb)as N0. rewrite Hw in N0. congruence.
 - exact(fn_info_valid_nonroot g s i s' Hg Nf Nm H Ej Hi Hw L).
Qed.
End Formation.
