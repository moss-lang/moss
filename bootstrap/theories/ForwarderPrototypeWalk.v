(* A forwarder settles in its first context-walk round. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy BodyTr CtxWalk
 InfoPreserve InfoCache FnInfoCore ForwarderPrototypeSeed ForwarderPrototypeFn FnSnapshotAlign.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section Walk.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Theorem forwarder_first_walk:forall g t target s e needs final n ps rt body,
 gs_origin g=OForwarder t->nget(w_syms w)t=Some target->gs_kind target=KFn->
 gs_decl target=ADFn RPlain n ps rt body->ReqTypes w R t->
 Inv s->st_w s=w->Later w P R Fl final->fi_walk g empty_env s=Good((e,needs),final)->
 exists seed sg stamp,PrototypeSeed seed(R t)/\
 e=set_app(set_fn seed t sg)(KyFn t)(map(fun r=>(r,Some(YAbs r)))(R t))stamp /\
 A.SigOf WW t(requirement_snapshot(R t))None(tr_sig Fl sg)/\needs=[KyFn t].
Proof.
 intros g t target s e needs final n ps rt body Origin Loaded Kind Decl Types Hi Hw L Run.
 unfold fi_walk in Run;rewrite Origin in Run.
 apply bind_inv in Run as[actual[s1[Symbol Run]]].
 destruct(sym_loader_st w b Hload _ _ _ _ _ Hi Hw Loaded Symbol)as[-> ->].
 destruct(bstep _ _ _ _ _ _ _ (ok_reqs t)Hi Run)as[rs[s2[Req[Run2[I2 F2]]]]].
 destruct(bstep _ _ _ _ _ _ _ ok_ctx_fuel I2 Run2)as[cf[s3[Fuel[Run3[I3 F3]]]]].
 assert(Ofold:forall acc r,ok(let! rg:=sym r in add_symbol_item cf(fst acc)(gs_mod g)rg[](snd acc)))
 by(intros;apply ok_bind;[unfold sym;okt|intro;apply ok_add_symbol_item]).
 destruct(bstep _ _ _ _ _ _ _ (ok_mfold _ _ _ rs(empty_env,[])Ofold)I3 Run3)
 as[[seed ns][s4[Fold[Add[I4 F4]]]]].
 destruct(ok_add_symbol_item cf seed(gs_mod g)target[]ns _ _ _ I4 Add)as[I5 F5].
 assert(Rs:R t=rs).
 {eapply(reqs_R w (P:=P) R Fl);[exact Hi|exact Req|].
  exact(later_back w P R Fl _ _ (fr_trans _ _ _ F3(fr_trans _ _ _ F4 F5))L). }
 subst rs.
 destruct(forwarder_requirements_seed w R b Hload cf(gs_mod g)(R t)[]empty_env[]s3 seed ns s4
 seed_empty I3 ltac:(rewrite(fr_w _ _ F3),(fr_w _ _ F2);exact Hw)Types Fold)as[Seed ->].
 rewrite app_nil_r in Seed.
 assert(Id:gs_id target=t)by exact(load_world_ids b w Hload t target Loaded).
 destruct(add_forwarder_function w (P:=P) R Fl b Hload cf seed(gs_mod g)target[]s4 e needs final n ps rt body
 ltac:(rewrite Id;exact Seed)ltac:(rewrite Id;exact Loaded)Kind Decl I4
 ltac:(rewrite(fr_w _ _ F4),(fr_w _ _ F3),(fr_w _ _ F2);exact Hw)L Add)
 as[sg[stamp[Shape[Sig Needs]]]].
 rewrite Id in Shape,Sig,Needs.
 exists seed,sg,stamp;split;[exact Seed|]. split;[exact Shape|]. split;[exact Sig|exact Needs].
Qed.
Lemma seed_no_type_edges:forall seed keys,PrototypeSeed seed keys->
 filter(fun p=>negb(ety_eqb(snd p)(YAbs(fst p))))(tymap seed)=[].
Proof.
 intros seed keys Seed;unfold tymap;pose proof(seed_identity _ _ Seed)as Self.
 induction(en_tn seed)as[|[k[y stamp]]rest IH];[reflexivity|].
 inversion Self;subst;cbn[map fst snd filter].
 cbn[fst snd]in H1;rewrite H1,ety_eqb_refl;cbn[negb];apply IH;assumption.
Qed.
Theorem forwarder_merged_walk:forall g t target s e needs final n ps rt body,
 gs_origin g=OForwarder t->nget(w_syms w)t=Some target->gs_kind target=KFn->
 gs_decl target=ADFn RPlain n ps rt body->ReqTypes w R t->
 Inv s->st_w s=w->Later w P R Fl final->
 merged_walk(gs_mod g)(fi_walk g)s=Good((e,needs),final)->
 exists seed sg stamp,PrototypeSeed seed(R t)/\
 e=set_app(set_fn seed t sg)(KyFn t)(map(fun r=>(r,Some(YAbs r)))(R t))stamp /\
 A.SigOf WW t(requirement_snapshot(R t))None(tr_sig Fl sg)/\needs=[KyFn t].
Proof.
 intros g t target s e needs final n ps rt body Origin Loaded Kind Decl Types Hi Hw L Run.
 rewrite(mw_go_eq(fi_walk g))in Run;cbn[mw_go]in Run.
 apply bind_inv in Run as[[first ns][mid[Walk Rest]]].
 destruct(ok_fi_walk g empty_env _ _ _ Hi Walk)as[Im Fm].
 assert(Tail:Fr mid final).
 {destruct(edges_eqb _ _)eqn:Edges.
  - injection Rest as <- <- <-;apply fr_refl.
  - exact(proj2(ok_mw_go(fi_walk g)(ok_fi_walk g)(gs_mod g)7 _ _ mid _ final Im Rest)). }
 destruct(forwarder_first_walk g t target s first ns mid n ps rt body Origin Loaded Kind Decl Types Hi Hw
 (later_back w P R Fl _ _ Tail L)Walk)as[seed[sg[stamp[Seed[Shape[Sig Ns]]]]]].
 subst first;cbn[set_app set_fn en_uf en_tn tymap]in Rest.
 replace(tymap(set_app(set_fn seed t sg)(KyFn t)(map(fun r=>(r,Some(YAbs r)))(R t))stamp))
 with(tymap seed)in Rest by reflexivity.
 rewrite(seed_no_type_edges seed(R t)Seed),(seed_uf _ _ Seed)in Rest.
 cbn[app edges_eqb length Nat.eqb forallb andb mbind ret]in Rest.
 injection Rest as <- <- <-.
 exists seed,sg,stamp;split;[exact Seed|]. split;[reflexivity|]. split;[exact Sig|exact Ns].
Qed.
End Walk.
