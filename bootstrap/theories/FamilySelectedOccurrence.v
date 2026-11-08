(* Successful selection identifies a written method occurrence in every complete output reading. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy NominalSupport Merge
 CanonTypes CanonBodyTy TyOfSubst SourceReadingExtend TySupport FamilySelectTrace FamilyMatchTrace
 FamilySelectedLeaf FamilyMatchCanonical FamilyCandidateWitness FamilyMatchingCtx FamilyMarkerSpecialize
 FamilyMarkedSubst FamilyMarkersExec FamilyScanSpecialize FamilyProducedKey.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Occurrence.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem family_selected_occurrence:forall c e F C mi r mid s g matching sf es,
 A.WorldOf b W->family_member mi e r mid s=Good(Some(g,matching),sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->
 WFty r->Support W r->canon e r=r->
 A.OutputItemsEntries W(gm_path(get_mod w(gs_mod g)))[]
 (family_semantic_target C(family_matching Fl e matching))(snd(fst(functor_parts g)))es
 (family_semantic_target C(family_matching Fl e matching))->
 Forall(fun ent=>match ent with A.NMth rt _ _ _=>TySupport W rt|_=>True end)es->
 exists sg ap,In(A.NMth(tr Fl r)mid sg ap)es.
Proof.
 intros c e F C mi r mid s g matching sf es Wo Select Hi Hw L HC Wr Sr Can Reading Supported.
 destruct(family_member_substitution w R Fl b Hload c e F C mi r mid s g matching sf HC Select Hi Hw L Wr Sr)
 as[[Hg Fam][Wm[Sm[Unique Poly]]]].
 destruct(proj1 Fam)as(d&n&names&args&res&Decl&D&Shape).
 change(A.w_decl(W_read w R Fl)(gs_id g)=Some d)in Decl.
 rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
 change(gs_decl g=ADFunctor n names args res None)in D.
 pose proof(family_member_matching_canonical _ _ _ _ _ _ _ _ Select(proj1(cr_uf _ _ _ _ _ _ _ HC))Can)as Values.
 destruct(family_member_trace w R Fl c e F C mi r mid s g matching sf Select Hi Hw L HC)
 as[ps marks em results fuel sc sp sm si ss sl sr se leaves leaf pat Parts Params Alloc Install HCM Uf
 Ic Wc Lm Scan Is Ws Ls Member Read Ir Wr0 Lr Matching Mid Mentioned Checked].
 set(target:=family_semantic_target C(family_matching Fl e matching)).
 set(sigma:=family_marker_subst marks target).
 set(completed:=overlay_missing(family_marker_types marks)target).
 assert(SRel:SubRel sigma(family_norm_types Fl em(tymap em))completed).
 {pose proof(family_marker_specializes w R Fl b Hload c e r F C g n names args res
 (family_matching Fl e matching)sc ps sp marks sm Hg D Fam Params Alloc Ic Wc Lm HC
 (family_matching_poly W Fl e(gs_id g)matching Poly))as Rel.
  intros k;rewrite family_norm_lookup,nassoc_tymap.
  specialize(Rel k);rewrite(cr_ty _ _ _ _ _ _ _ HCM k)in Rel.
  exact Rel. }
 assert(TE:@TypeExtension W target completed).
 {eapply(family_marker_completion_types w R Fl b Hload);eassumption. }
 assert(Pat:canon e pat=pat).
 {assert(Canonical:canon em pat=pat).
  {unfold receiver_at in Read;apply bind_inv in Read as(raw&sq&Raw&Ret);injection Ret as <- <-.
   apply canon_idem;exact(proj1(cr_uf _ _ _ _ _ _ _ HCM)). }
  rewrite(canon_uf e em Uf)in Canonical;exact Canonical. }
 assert(Nd:NoDup(map snd marks))by exact(proj1(family_marker_allocation _ _ _ _ _ _ Alloc)).
 assert(Entry:exists sg ap,In(A.NMth(tr Fl r)(let '(_,_,mg,_) := leaf in gs_id mg)sg ap)es).
 {eapply(family_selected_leaf_occurs w R Fl b Hload)with(c:=c)(e:=e)(F:=F)(C:=C)(marks:=marks)(matching:=matching)(sigma:=sigma)(completed:=completed);
 [exact Scan|exact Is|exact Ws|exact(loader_mod_in w R Fl b Hload _ _ Hg)|exact Ls|exact Member|
 rewrite Parts in Reading;exact Reading|exact Supported|exact Read|exact Ir|exact Wr0|exact Lr|exact Wo|exact HC|
 exact(proj1(cr_uf _ _ _ _ _ _ _ HCM))|exact(proj2(cr_uf _ _ _ _ _ _ _ HCM))|
 exact(cr_ty_wf _ _ _ _ _ _ _ HCM)|exact SRel|exact TE|reflexivity|exact Nd|exact Unique|exact Values|exact Pat|exact Mentioned|exact Checked]. }
 destruct leaf as[[[lmi rg]mg]sub];cbn in Mid,Entry;rewrite Mid in Entry;exact Entry.
Qed.
End Occurrence.
