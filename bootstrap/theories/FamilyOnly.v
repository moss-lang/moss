(* Actual successful unique-family selection excludes every semantic competitor. *)
From Stdlib Require Import List String.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore BodySoundCore BodyTr BodyTy NominalSupport NMapEnum
 FamilySelectTrace FamilySearchCompetitor FamilySearchRetains FamilyFoldExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Only.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem family_member_only:forall c e F C mi r mid s fg th sf sc,
 (forall x y,Fl x=Fl y->x=y)->A.WorldOf b W->
 check_contexts sc=Good(tt,sc)->st_w sc=w->CxRel w R Fl c e F C->
 family_member mi e r mid s=Good(Some(fg,th),sf)->
 Inv s->st_w s=w->Later w P R Fl sf->WFty r->Support W r->canon e r=r->
 A.OnlyFamily W C(gs_id fg)(A.mkey(tr Fl r)mid).
Proof.
 intros c e F C mi r mid s fg th sf sc Finj Wo Checked Wsc HC Run Hi Hw L Wr Sr Can.
 pose proof(family_member_search _ _ _ _ _ _ _ _ Run)as Search;unfold family_search in Search.
 intros u Ne Produces.
 destruct Produces as[fx[matching[Cr[keys[Have[Fam[Poly[Provide Key]]]]]]]]eqn:Parts.
 assert(Producer:A.FamilyProduces W C u(A.mkey(tr Fl r)mid)).
 {exists fx,matching,Cr,keys;split;[exact Have|];split;[exact Fam|];split;[exact Poly|];split;assumption. }
 pose proof(cr_funspresent _ _ _ _ _ _ _ HC u)as Present;rewrite Have in Present;cbn[is_some]in Present.
 destruct(nget(en_funs e)u)as[stamp|]eqn:En;[|discriminate Present].
 assert(Uin:In u(map fst(elems(en_funs e)))).
 {apply in_map_iff;exists(u,stamp);split;[reflexivity|apply elems_lookup;exact En]. }
 destruct(family_fold_visit _ _ (family_search_step mi e r mid)
 (fun before v=>ok_family_search_step _ _ _ _ _ _) _ [] s[(fg,th)]sf Search Hi u Uin)
 as(prefix&suffix&before&after&sb&sa&Eq&Pre&One&Post&Ib).
 destruct(ok_mfold _ _ _ prefix [] (fun before v=>ok_family_search_step _ _ _ _ _ _)
 _ _ _ Hi Pre)as[_ Fpre].
 assert(Wb:st_w sb=w)by exact(fr_w' w _ _ Fpre Hw).
 destruct(ok_family_search_step mi e r mid before u _ _ _ Ib One)as[Ia Fa].
 destruct(ok_mfold _ _ _ suffix after (fun before v=>ok_family_search_step _ _ _ _ _ _)
 _ _ _ Ia Post)as[_ Fpost].
 destruct(family_search_step_competitor w R Fl b Hload c e F C mi r mid before u sb after sa sc
 Finj Wo Checked Wsc HC Producer One Ib Wb(later_back w P R Fl _ _ Fpost L)Wr Sr Can)
 as(other&theta&Hit&Id).
 pose proof(family_search_retain_old mi e r mid suffix after sa[(fg,th)]sf Post(other,theta)Hit)as Final.
 destruct Final as[EqPair|[]];injection EqPair as <- <-;congruence.
Qed.
End Only.
