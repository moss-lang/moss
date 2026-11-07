(* A semantic competitor's real scanned leaf cannot be skipped by matching. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore BodySoundCore BodyTr BodyTy Merge CanonTypes
 NominalSupport TySupport SourceSupport SourceReadingExtend TyOfSubst PMatchComplete
 FamilySelectTrace FamilyRawSolution FamilyMarkedSubst FamilyCandidateComplete
 FamilyLeafScan FamilyScanSpecialize FamilyScanSupport.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma family_candidate_receiver_read:forall mi e em r mid u fg marks back found lmi rg mg sub s found' sf,
 family_candidate_step mi e em r mid u fg marks back found(lmi,rg,mg,sub)s=Good(found',sf)->
 Inv s->gs_id mg=mid->exists pat sp,
 receiver_at em lmi rg sub s=Good(pat,sp)/\Inv sp/\Fr s sp/\Fr sp sf.
Proof.
 intros mi e em r mid u fg marks back found lmi rg mg sub s found' sf Run Hi Mid.
 unfold family_candidate_step in Run;cbn beta iota zeta in Run;rewrite Mid,Nat.eqb_refl in Run;cbn[negb]in Run.
 apply bind_inv in Run as(pat&sp&Read&Tail).
 destruct(ok_family_receiver em lmi rg sub _ _ _ Hi Read)as[Ip Fp].
 match type of Tail with ?m sp=Good(_,sf)=>
 assert(Om:ok m)by okt;destruct(Om _ _ _ Ip Tail)as[_ Ft]end.
 exists pat,sp;split;[exact Read|];split;[exact Ip|];split;assumption.
Qed.
Section Retention.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem family_candidate_route_retains:forall c e em r F C mi mid u fg marks found lmi rg mg sub s found' sf
 ps seed sm table origin outer ob tm sp completed,
 (forall x y,Fl x=Fl y->x=y)->A.WorldOf b W->
 mmap(fun p=>let!k:=fresh_marker_for e r in ret(p,k))ps seed=Good(marks,sm)->
 family_candidate_step mi e em r mid u fg marks(family_marker_back marks)found(lmi,rg,mg,sub)s=Good(found',sf)->
 FamilyScanRoute w (P:=P) R Fl em origin outer ob tm sp(tr Fl r)mid(lmi,rg,mg,sub)->
 CxRel w R Fl c em F C->In origin(w_order w)->Forall(fun q=>WFty(snd q))outer->
 Forall(fun q=>Support W(snd q))outer->
 SubRel(family_marker_subst marks table)
  (family_norm_types Fl em outer++family_norm_types Fl em(tymap em))completed->
 @TypeExtension W(ob++tm)completed->
 Inv s->st_w s=w->Later w P R Fl sf->WFty r->Support W r->canon e r=r->gs_id fg=u->
 exists other th,In(other,th)found'/\gs_id other=gs_id fg.
Proof.
 intros c e em r F C mi mid u fg marks found lmi rg mg sub s found' sf ps seed sm table origin outer ob tm sp completed
 Finj Wo Alloc Step Route HC Horigin WFouter Souter SR TE Hi Hw L Wr Supp Can Id.
 destruct(family_scan_route_selected w R Fl b Hload _ _ _ _ _ _ _ _ _ Route Horigin)as[Hg[Mid Hmi]].
 destruct(family_candidate_receiver_read _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ Step Hi Mid)
 as(pat&spat&Read&Ip&Fp&Ft).
 assert(Wp:st_w spat=w)by exact(fr_w' w _ _ Fp Hw).
 pose proof(later_back w P R Fl _ _ Ft L)as Lp.
 assert(T:Tri(en_uf em))by exact(proj1(cr_uf _ _ _ _ _ _ _ HC)).
 assert(UF:UfWF(en_uf em))by exact(proj2(cr_uf _ _ _ _ _ _ _ HC)).
 assert(Types:forall k y,tn_get em k=Some y->WFty y)by exact(cr_ty_wf _ _ _ _ _ _ _ HC).
 pose proof(family_scan_specializes w R Fl b Hload _ _ _ _ _ _ _ _ _ Route Wo T UF Types Horigin WFouter
  (support_tr Fl Supp)(family_marker_subst marks table)completed SR TE s pat spat Read Hi Hw Lp)as Eq.
 pose proof(family_scan_receiver_support w R Fl b Hload _ _ _ _ _ _ _ _ _ Route T
  (cr_support _ _ _ _ _ _ _ HC)Types Horigin WFouter Souter s pat spat Read Hi Hw Lp)as Spat.
 pose proof(family_scan_receiver_wf w R Fl b Hload _ _ _ _ _ _ _ _ _ Route T UF Types Horigin WFouter
  s pat spat Read Hi Hw Lp)as Wpat.
 assert(Cpat:ety_ok pat=true)by exact(support_clean(loaded_record_unowned w R Fl b Hload)Spat Wpat).
 assert(Cr:ety_ok r=true)by exact(support_clean(loaded_record_unowned w R Fl b Hload)Supp Wr).
 assert(Unique:MatchUnique pat)by exact(support_match_unique W(@A.wo_reqs_nodup b W Wo)pat Spat).
 eapply family_candidate_semantic_complete;[exact Finj|exact Cpat|exact Cr|exact Alloc|exact Step|exact Read|
 exact Mid|exact Id|exact Unique|exact Can|exact Eq].
Qed.
End Retention.
