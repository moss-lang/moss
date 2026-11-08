(* A real bound semantic competitor is retained by its actual scanner visit. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTr BodyTy
 CanonBodyTy Merge CanonTypes TyOfSubst SourceReadingExtend FunctorDeclExec
 FamilySelectTrace FamilyClassifierReverse FamilyMarkerContext FamilyMarkerSpecialize FamilyMarkedSubst
 FamilyCandidateComplete FamilySemanticLeaf FamilyLeafScan FamilyScanSpecialize FamilyCandidateRoute FamilySearchRetains FamilyFoldExec.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
#[local]Hint Resolve ok_family_receiver ok_family_params ok_family_classify ok_family_candidate_step:okdb.
Local Strategy opaque[family_candidate_step receiver_at family_params is_family].
Lemma family_sym_id:forall u s g sf,sym u s=Good(g,sf)->Inv s->gs_id g=u.
Proof.
 intros u s g sf E Hi;unfold sym in E;destruct(nget(st_syms s)u)as[g0|]eqn:Read;
 [injection E as <- <-;exact(iv_ids _ Hi _ _ Read)|discriminate E].
Qed.
Section Competitor.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Ltac competitor_prefix H Hi Hw L a si E Ii Wi Li:=
 match type of H with mbind ?m ?k ?s=Good(?v,?sf)=>
 let O:=fresh"O"in assert(O:ok m)by okt;
 apply bind_inv in H as(a&si&E&H);
 let Fr:=fresh"Fr"in destruct(O _ _ _ Hi E)as[Ii Fr];
 pose proof(fr_w' w _ _ Fr Hw)as Wi;
 let Ot:=fresh"Ot"in assert(Ot:ok(k a))by okt;
 let It:=fresh"It"in let Ft:=fresh"Ft"in destruct(Ot _ _ _ Ii H)as[It Ft];
 pose proof(later_back w P R Fl _ _ Ft L)as Li;clear O Ot It Ft end.
Lemma family_marked_raw_subrel:forall c em F Cm sigma completed,
 CxRel w R Fl c em F Cm->SubRel sigma(A.c_ty Cm)completed->
 SubRel sigma(family_norm_types Fl em(tymap em))completed.
Proof.
 intros c em F Cm sigma completed HC SR k;rewrite(SR k).
 rewrite family_norm_lookup,nassoc_tymap,(cr_ty _ _ _ _ _ _ _ HC);reflexivity.
Qed.
Theorem family_search_step_competitor:forall c e F C mi r mid found u s found' sf sc,
 (forall x y,Fl x=Fl y->x=y)->A.WorldOf b W->
 check_contexts sc=Good(tt,sc)->st_w sc=w->CxRel w R Fl c e F C->
 A.FamilyProduces W C u(A.mkey(tr Fl r)mid)->
 family_search_step mi e r mid found u s=Good(found',sf)->
 Inv s->st_w s=w->Later w P R Fl sf->WFty r->NominalSupport.Support W r->canon e r=r->
 exists other th,In(other,th)found'/\gs_id other=u.
Proof.
 intros c e F C mi r mid found u s found' sf sc Finj Wo Checked Wsc HC Produces Run Hi Hw L Wr Sr Can.
 destruct(family_produces_semantic_leaf w R Fl c e F C u(tr Fl r)mid HC Produces)
 as(fx&matching&d&n&names&args&results&leafsp&Have&Fam&Poly&Decl&Ast&InLeaf&Leaf).
 pose proof(cr_funspresent _ _ _ _ _ _ _ HC u)as Present.
 rewrite Have in Present;cbn[is_some]in Present.
 destruct(nget(en_funs e)u)as[stamp|]eqn:En;[|discriminate Present].
 unfold family_search_step in Run;apply bind_inv in Run as(fg&ss&Sym&Run).
 pose proof(family_sym_id _ _ _ _ Sym Hi)as Id.
 destruct(cxrel_functor_sym w R Fl b Hload c e F C u stamp s fg ss HC En Sym Hi Hw)
 as[->[Hg DeclShape]].
 rewrite <-Id in Decl,Fam,Poly.
 change(A.w_decl(W_read w R Fl)(gs_id fg)=Some d)in Decl.
 rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
 change(gs_decl fg=ADFunctor n names args results None)in Ast.
 competitor_prefix Run Hi Hw L fam sc0 Class Ic Wc Lc.
 pose proof(is_family_classifies w R Fl b Hload fg s fam sc0 sc Hg Class Checked Wsc Hi Hw Lc Fam)as ->.
 cbn[negb]in Run.
 competitor_prefix Run Ic Wc L ps sp Params Ip Wp Lp.
 competitor_prefix Run Ip Wp L marks sm Alloc Im Wm Lm.
 competitor_prefix Run Im Wm L em se Install Ie We Le.
 change(mfold family_mark_env_step e marks sm=Good(em,se))in Install.
 destruct(family_marked_context w R Fl c e r F C ps sp marks sm em se Alloc Install HC)as[Uf Hmarked].
 unfold functor_parts in Run;rewrite Ast in Run;cbn beta iota zeta in Run.
 competitor_prefix Run Ie We L fuel sn Fuel Ifuel Wfuel Lfuel.
 competitor_prefix Run Ifuel Wfuel L leaves sl Leaves Il Wl Ll.
 assert(Hfgmod:In(gs_mod fg)(w_order w))by exact(loader_mod_in w R Fl b Hload _ _ Hg).
 cbn[A.d_mod]in Leaf.
 destruct(family_leaves_semantic_route w R Fl b Hload fuel em(gs_mod fg)results[]sn leaves sl
 Leaves Ifuel Wfuel Hfgmod Ll [] (family_semantic_target C matching)leafsp(tr Fl r)mid InLeaf Leaf)
 as(rawleaf&RawIn&Route).
 pose proof(family_marker_specializes w R Fl b Hload c e r F C fg n names args results matching sc0 ps sp marks sm
 Hg Ast Fam Params Alloc Ic Wc Lm HC Poly)as Special.
 pose proof(family_marker_completion_types w R Fl b Hload c e r F C fg n names args results sc0 ps sp marks sm
 (family_semantic_target C matching)Wo Hg Ast Params Alloc Ic Wc Lm HC)as Extend.
 assert(RawSR:SubRel(family_marker_subst marks(family_semantic_target C matching))
  (family_norm_types Fl em[]++family_norm_types Fl em(tymap em))
  (overlay_missing(family_marker_types marks)(family_semantic_target C matching))).
 {cbn[family_norm_types map List.app];eapply family_marked_raw_subrel;[exact Hmarked|exact Special]. }
 destruct(family_fold_visit _ _ (family_candidate_step mi e em r mid u fg marks(family_marker_back marks))
 (fun before lf=>ok_family_candidate_step _ _ _ _ _ _ _ _ _ _ _)leaves found sl found' sf Run Il rawleaf RawIn)
 as(prefix&suffix&before&after&sb&sa&Eq&Pre&One&Post&Ib).
 destruct(ok_mfold _ _ _ prefix found (fun before lf=>ok_family_candidate_step _ _ _ _ _ _ _ _ _ _ _)
 _ _ _ Il Pre)as[_ Fpre].
 assert(Wb:st_w sb=w)by exact(fr_w' w _ _ Fpre Wl).
 assert(Oone:ok(family_candidate_step mi e em r mid u fg marks(family_marker_back marks)before rawleaf))by apply ok_family_candidate_step.
 destruct(Oone _ _ _ Ib One)as[Ia Fa].
 destruct(ok_mfold _ _ _ suffix after (fun before lf=>ok_family_candidate_step _ _ _ _ _ _ _ _ _ _ _)
 _ _ _ Ia Post)as[_ Fpost].
 destruct rawleaf as[[[lmi rg]mg]sub].
 destruct(family_candidate_route_retains w R Fl b Hload c e em r F
  (fold_left(fun C q=>A.set_ty C(fst q)(TAbs(snd q)))marks C)
  mi mid u fg marks before lmi rg mg sub sb after sa ps sp sm(family_semantic_target C matching)
  (gs_mod fg)[] [] (family_semantic_target C matching)leafsp
  (overlay_missing(family_marker_types marks)(family_semantic_target C matching))
  Finj Wo Alloc One Route Hmarked Hfgmod(Forall_nil _) (Forall_nil _)RawSR Extend Ib Wb
  (later_back w P R Fl _ _ Fpost L)Wr Sr Can Id)as(other&th&Hit&Oid).
 exists other,th;split;[eapply family_candidates_retain_old;[exact Post|exact Hit]|congruence].
Qed.
End Competitor.
