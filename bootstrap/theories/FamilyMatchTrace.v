(* A selected family carries an actual leaf and the successful reconstruction check. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame BodySoundCore BodyTr BodyTy
 FamilySelectTrace FamilyMarkerContext FamilyCandidateComplete FamilyCandidateWitness FamilyFoldExec FamilySearchRetains.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Trace.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat).
Inductive FamilyMatchTrace(c:cx)(e:env)(F:A.frame)(C:A.cx)(r:ety)(mid:nat)(g:gsym)(matching:list(nat*ety)):Prop:=
| FMT:forall
 (fmt_ps:list nat)
 (fmt_marks:list(nat*nat))
 (fmt_em:env)
 (fmt_results:list spec)
 (fmt_fuel:nat)
 (fmt_class_state:st)
 (fmt_params_state:st)
 (fmt_marks_state:st)
 (fmt_install_state:st)
 (fmt_scan_state:st)
 (fmt_leaves_state:st)
 (fmt_read_state:st)
 (fmt_read_end:st)
 (fmt_leaves:list family_leaf)
 (fmt_leaf:family_leaf)
 (fmt_pattern:ety)
 (fmt_parts:snd(fst(functor_parts g))=fmt_results)
 (fmt_params:family_params g fmt_class_state=Good(fmt_ps,fmt_params_state))
 (fmt_alloc:mmap(fun p=>let!k:=fresh_marker_for e r in ret(p,k))fmt_ps fmt_params_state=Good(fmt_marks,fmt_marks_state))
 (fmt_install:mfold family_mark_env_step e fmt_marks fmt_marks_state=Good(fmt_em,fmt_install_state))
 (fmt_context:CxRel w R Fl c fmt_em F(fold_left(fun C q=>A.set_ty C(fst q)(TAbs(snd q)))fmt_marks C))
 (fmt_uf:en_uf fmt_em=en_uf e)
 (fmt_params_inv:Inv fmt_class_state)
 (fmt_params_world:st_w fmt_class_state=w)
 (fmt_marks_later:Later w P R Fl fmt_marks_state)
 (fmt_scan:family_leaves fmt_fuel fmt_em(gs_mod g)fmt_results[]fmt_scan_state=Good(fmt_leaves,fmt_leaves_state))
 (fmt_scan_inv:Inv fmt_scan_state)
 (fmt_scan_world:st_w fmt_scan_state=w)
 (fmt_scan_later:Later w P R Fl fmt_leaves_state)
 (fmt_member:In fmt_leaf fmt_leaves)
 (fmt_read:receiver_at fmt_em(let '(lmi,_,_,_) :=fmt_leaf in lmi)(let '(_,rg,_,_) :=fmt_leaf in rg)
   (let '(_,_,_,sub):=fmt_leaf in sub)fmt_read_state=Good(fmt_pattern,fmt_read_end))
 (fmt_read_inv:Inv fmt_read_state)
 (fmt_read_world:st_w fmt_read_state=w)
 (fmt_read_later:Later w P R Fl fmt_read_end)
 (fmt_matching:matching=pmatch_full(family_marker_back fmt_marks)fmt_pattern r[])
 (fmt_mid:let '(_,_,mg,_):=fmt_leaf in gs_id mg=mid)
 (fmt_mentioned:forallb(fun q=>is_some(nassoc(fst q)matching))
  (filter(fun q=>nmem(snd q)(ety_gens fmt_pattern))fmt_marks)=true)
 (fmt_reconstructed:canon e(ety_subst_full(family_raw_inst fmt_marks matching)fmt_pattern)=r)
,FamilyMatchTrace c e F C r mid g matching.
#[local]Hint Resolve ok_family_receiver ok_family_params ok_family_classify ok_family_candidate_step:okdb.
Local Strategy opaque[family_candidate_step receiver_at family_params is_family].
Ltac trace_prefix H Hi Hw L a si E Ii Wi Li:=
 match type of H with mbind ?m ?k ?s=Good(?v,?sf)=>
 let O:=fresh"O"in assert(O:ok m)by okt;
 apply bind_inv in H as(a&si&E&H);
 let Fr:=fresh"Fr"in destruct(O _ _ _ Hi E)as[Ii Fr];pose proof(fr_w' w _ _ Fr Hw)as Wi;
 let Ot:=fresh"Ot"in assert(Ot:ok(k a))by okt;
 let It:=fresh"It"in let Ft:=fresh"Ft"in destruct(Ot _ _ _ Ii H)as[It Ft];
 pose proof(later_back w P R Fl _ _ Ft L)as Li;clear O Ot It Ft end.
Theorem family_member_trace:forall c e F C mi r mid s g matching sf,
 family_member mi e r mid s=Good(Some(g,matching),sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->FamilyMatchTrace c e F C r mid g matching.
Proof.
 intros c e F C mi r mid s g matching sf Run Hi Hw L HC.
 pose proof(family_member_search _ _ _ _ _ _ _ _ Run)as Search;unfold family_search in Search.
 destruct(family_fold_new_id_visit _ _ (fun q:family_candidate=>gs_id(fst q))(family_search_step mi e r mid)
 (fun before u=>ok_family_search_step _ _ _ _ _ _) _ [] s[(g,matching)]sf(gs_id g)Search Hi
 ltac:(intros[])ltac:(left;reflexivity))
 as(prefix&u&suffix&before&after&sb&sa&Eq&Pre&One&Post&Ib&Absent&Present).
 destruct(ok_mfold _ _ _ prefix [] (fun before u=>ok_family_search_step _ _ _ _ _ _)_ _ _ Hi Pre)as[_ Fpre].
 assert(Wb:st_w sb=w)by exact(fr_w' w _ _ Fpre Hw).
 destruct(ok_family_search_step mi e r mid before u _ _ _ Ib One)as[Ia Fa].
 destruct(ok_mfold _ _ _ suffix after (fun before u=>ok_family_search_step _ _ _ _ _ _)_ _ _ Ia Post)as[_ Fpost].
 pose proof(later_back w P R Fl _ _ Fpost L)as La.
 unfold family_search_step in One;apply bind_inv in One as(fg&sy&Sym&One).
 assert(Same:sy=sb)by(unfold sym in Sym;destruct(nget(st_syms sb)u);[injection Sym;auto|discriminate Sym]);subst sy.
 trace_prefix One Ib Wb La fam sc Class Ic Wc Lc.
 destruct(negb fam);[injection One as <- <-;contradiction|].
 trace_prefix One Ic Wc La ps sp Params Ip Wp Lp.
 trace_prefix One Ip Wp La marks sm Alloc Im Wm Lm.
 trace_prefix One Im Wm La em se Install Ie We Le.
 change(mfold family_mark_env_step e marks sm=Good(em,se))in Install.
 destruct(family_marked_context w R Fl c e r F C ps sp marks sm em se Alloc Install HC)as[Uf Hmarked].
 destruct(functor_parts fg)as[[args results]body]eqn:Parts;cbn beta iota zeta in One.
 trace_prefix One Ie We La fuel sn Fuel Ifuel Wfuel Lfuel.
 trace_prefix One Ifuel Wfuel La leaves sl Leaves Il Wl Ll.
 destruct(family_fold_new_id_visit _ _ (fun q:family_candidate=>gs_id(fst q))
 (family_candidate_step mi e em r mid u fg marks(family_marker_back marks))
 (fun before lf=>ok_family_candidate_step _ _ _ _ _ _ _ _ _ _ _) _ before sl after sa(gs_id g)One Il Absent Present)
 as(lp&lf&ls&first&next&sr&spat&LEq&LPre&Visit&LPost&Ir&LAbsent&LPresent).
 assert(Lin:In lf leaves)by(rewrite LEq;apply in_app_iff;right;left;reflexivity).
 destruct(ok_mfold _ _ _ lp before(fun before lf=>ok_family_candidate_step _ _ _ _ _ _ _ _ _ _ _)_ _ _ Il LPre)as[_ Flpre].
 assert(Wr:st_w sr=w)by exact(fr_w' w _ _ Flpre Wl).
 destruct(ok_family_candidate_step mi e em r mid u fg marks(family_marker_back marks)first lf _ _ _ Ir Visit)as[Inext Fnext].
 destruct(ok_mfold _ _ _ ls next(fun before lf=>ok_family_candidate_step _ _ _ _ _ _ _ _ _ _ _)_ _ _ Inext LPost)as[_ Flpost].
 pose proof(later_back w P R Fl _ _ Flpost La)as Lpat.
 destruct lf as[[[lmi rg]mg]sub].
 destruct(family_candidate_new_id mi e em r mid u fg marks(family_marker_back marks)first lmi rg mg sub sr next spat(gs_id g)Visit LAbsent LPresent)
 as(pat&spat0&Read&Next&End&Target&Mid&Mentioned&Reconstruct).
 assert(Added:In(fg,pmatch_full(family_marker_back marks)pat r[])next).
 {rewrite Next;apply in_app_iff;right;left;reflexivity. }
 pose proof(family_candidates_retain_old mi e em r mid u fg marks(family_marker_back marks)ls next spat after sa LPost _ Added)as Inafter.
 pose proof(family_search_retain_old mi e r mid suffix after sa[(g,matching)]sf Post _ Inafter)as Final.
 destruct Final as[Pair|[]];injection Pair as Efg Ematching;subst fg matching.
 subst spat0.
 eapply FMT with(fmt_ps:=ps)(fmt_marks:=marks)(fmt_em:=em)(fmt_results:=results)(fmt_fuel:=fuel)
 (fmt_class_state:=sc)(fmt_params_state:=sp)(fmt_marks_state:=sm)(fmt_install_state:=se)
 (fmt_scan_state:=sn)(fmt_leaves_state:=sl)(fmt_read_state:=sr)(fmt_read_end:=spat)
 (fmt_leaves:=leaves)(fmt_leaf:=(lmi,rg,mg,sub))(fmt_pattern:=pat);try eassumption;try reflexivity.
 rewrite Parts;reflexivity.
Qed.
End Trace.
