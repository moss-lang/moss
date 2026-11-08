(* Successful candidate scanning never drops a family already found. *)
From Stdlib Require Import List Bool.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound FamilySelectTrace.
Import ListNotations.
Lemma family_candidate_retains_old:forall mi e em r mid u fg marks back found lf s found' sf,
 family_candidate_step mi e em r mid u fg marks back found lf s=Good(found',sf)->
 forall candidate,In candidate found->In candidate found'.
Proof.
 intros mi e em r mid u fg marks back found [[[lmi rg]m]sub]s found' sf E candidate Old.
 unfold family_candidate_step in E;cbn beta iota zeta in E.
 destruct(negb(Nat.eqb(gs_id m)mid));[injection E as <- <-;exact Old|].
 apply bind_inv in E as(pat&sp&Read&E).
 destruct(negb(forallb(fun q=>is_some(nassoc(fst q)(pmatch_full back pat r[])))
 (filter(fun q=>nmem(snd q)(ety_gens pat))marks)));[injection E as <- <-;exact Old|].
 destruct(negb(ety_eqb(canon e(ety_subst_full(map(fun q=>(snd q,opt_default(YAbs(snd q))
 (nassoc(fst q)(pmatch_full back pat r[]))))marks)pat))r));[injection E as <- <-;exact Old|].
 destruct(filter(fun q=>Nat.eqb(gs_id(fst q))u)found)as[|[other th]rest].
 - injection E as <- <-;apply in_app_iff;left;exact Old.
 - destruct(forallb(fun q=>match nassoc(fst q)th with Some y=>ety_eqb y(snd q)|None=>false end)
 (pmatch_full back pat r[])&&Nat.eqb(length(pmatch_full back pat r[]))(length th)).
   + injection E as <- <-;exact Old.
   + apply bind_inv in E as(text&ss&Show&E);exfalso;eapply err_not_good;exact E.
Qed.
Lemma family_candidates_retain_old:forall mi e em r mid u fg marks back leaves found s found' sf,
 mfold(family_candidate_step mi e em r mid u fg marks back)found leaves s=Good(found',sf)->
 forall candidate,In candidate found->In candidate found'.
Proof.
 intros mi e em r mid u fg marks back leaves;induction leaves as[|leaf leaves IH];
 intros found s found' sf E candidate Old;[injection E as <- <-;exact Old|].
 cbn[mfold]in E;apply bind_inv in E as(next&sn&Step&Tail).
 eapply IH;[exact Tail|eapply family_candidate_retains_old;[exact Step|exact Old]].
Qed.
Lemma family_search_step_retains_old:forall mi e r mid found u s found' sf,
 family_search_step mi e r mid found u s=Good(found',sf)->
 forall candidate,In candidate found->In candidate found'.
Proof.
 intros mi e r mid found u s found' sf E candidate Old;unfold family_search_step in E.
 apply bind_inv in E as(fg&sg&Sym&E);apply bind_inv in E as(fam&sc&Class&E).
 destruct(negb fam);[injection E as <- <-;exact Old|].
 apply bind_inv in E as(ps&sp&Params&E);apply bind_inv in E as(marks&sm&Markers&E).
 apply bind_inv in E as(em&se&Env&E).
 destruct(functor_parts fg)as[[args results]body];cbn beta iota zeta in E.
 apply bind_inv in E as(fuel&sn&Fuel&E);apply bind_inv in E as(leaves&sl&Leaves&E).
 eapply family_candidates_retain_old;[exact E|exact Old].
Qed.
Lemma family_search_retain_old:forall mi e r mid ids found s found' sf,
 mfold(family_search_step mi e r mid)found ids s=Good(found',sf)->
 forall candidate,In candidate found->In candidate found'.
Proof.
 intros mi e r mid ids;induction ids as[|u ids IH];intros found s found' sf E candidate Old;
 [injection E as <- <-;exact Old|].
 cbn[mfold]in E;apply bind_inv in E as(next&sn&Step&Tail).
 eapply IH;[exact Tail|eapply family_search_step_retains_old;[exact Step|exact Old]].
Qed.
