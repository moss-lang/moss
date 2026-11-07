(* A selected scanner leaf remains an output occurrence, even when already present. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv BodySoundCore BodyTr Merge CanonTypes
 TyOfSubst SourceReadingExtend TySupport FamilyMatchReconstruct FamilyCandidateWitness FamilyMarkedSubst
 FamilyMatchingCtx FamilyMarkerSpecialize FamilyLeafComplete FamilyScanSpecialize FamilyLeafScan.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Selected.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem family_selected_leaf_occurs:forall fuel c e F C em mi specs si leaves sf leaf sr pat se
 marks matching r ob tm es tm' sigma completed,
 family_leaves fuel em mi specs [] si=Good(leaves,sf)->
 Inv si->st_w si=w->In mi(w_order w)->Later w P R Fl sf->
 In leaf leaves->
 A.OutputItemsEntries W(gm_path(get_mod w mi))ob tm specs es tm'->
 Forall(fun ent=>match ent with A.NMth rt _ _ _=>TySupport W rt|_=>True end)es->
 receiver_at em(let '(lmi,_,_,_) := leaf in lmi)(let '(_,rg,_,_) := leaf in rg)
 (let '(_,_,_,sub) := leaf in sub)sr=Good(pat,se)->
 Inv sr->st_w sr=w->Later w P R Fl se->
 A.WorldOf b W->CxRel w R Fl c e F C->Tri(en_uf em)->UfWF(en_uf em)->
 (forall k y,tn_get em k=Some y->WFty y)->
 SubRel sigma(family_norm_types Fl em(tymap em))completed->
 @TypeExtension W(ob++tm)completed->
 sigma=family_marker_subst marks(family_semantic_target C(family_matching Fl e matching))->
 NoDup(map snd marks)->NoDup(map fst matching)->
 Forall(fun q=>canon e(snd q)=snd q)matching->canon e pat=pat->
 forallb(fun q=>is_some(nassoc(fst q)matching))(filter(fun q=>nmem(snd q)(ety_gens pat))marks)=true->
 canon e(ety_subst_full(family_raw_inst marks matching)pat)=r->
 exists sg ap,In(A.NMth(tr Fl r)(let '(_,_,mg,_) := leaf in gs_id mg)sg ap)es.
Proof.
 intros fuel c e F C em mi specs si leaves sf leaf sr pat se marks matching r ob tm es tm' sigma completed
 Scan Hi Hw Hmi L Member Reading Supported Read Ir Wr Lr Wo HC Tri UF Types SR TE Sigma Nd Unique Values Pat Mentioned Checked.
 destruct(family_leaves_results_complete w R Fl b Hload fuel em mi specs [] si leaves sf Scan Hi Hw Hmi L
 ob tm es tm' Reading leaf Member)as(sp&rt&mid&sg&ap&Hsp&Entry&Route).
 assert(Support:TySupport W rt).
 {rewrite Forall_forall in Supported;exact(Supported _ Entry). }
 assert(Identity:Audit.app_subst sigma(tr Fl pat)=rt).
 {eapply(family_scan_specializes w R Fl b Hload);[exact Route|exact Wo|exact Tri|exact UF|exact Types|
 exact Hmi|constructor|exact Support|exact SR|exact TE|exact Read|exact Ir|exact Wr|exact Lr]. }
 assert(Receiver:rt=tr Fl r).
 {rewrite <-Identity,Sigma;eapply family_match_reconstructs;eassumption. }
 assert(Method:(let '(_,_,mg,_) := leaf in gs_id mg)=mid).
 {clear -Route;induction Route;cbn;[assumption|exact IHRoute]. }
 exists sg,ap;rewrite Method,<-Receiver;exact Entry.
Qed.
End Selected.
