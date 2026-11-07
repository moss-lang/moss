(* Real bracket and receiver executions preserve the supported nominal domains. *)
From Stdlib Require Import List String.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore BodySoundCore BodyTr BodyTy
 Merge CanonTypes CanonBodyTy NominalSupport SourceSupport FamilyLeafScan FamilyScanSpecialize FamilyReceiverExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma family_prefix_lookup:forall(Q:ety->Prop)e outer,
 Forall(fun q=>Q(snd q))outer->(forall k y,tn_get e k=Some y->Q y)->
 forall k y,nassoc k(outer++tymap e)=Some y->Q y.
Proof.
 intros Q e outer Parts Types k y Read;rewrite nassoc_app in Read.
 destruct(nassoc k outer)as[v|]eqn:Found.
 - injection Read as <-;rewrite Forall_forall in Parts.
   exact(Parts(k,v)(Merge.nassoc_in _ _ _ _ Found)).
 - rewrite nassoc_tymap in Read;exact(Types k y Read).
Qed.
Section Support.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Lemma family_receipt_support:forall e mi a outer sub,
 FamilyBracketReceipt w (P:=P) R Fl e mi a outer sub->In mi(w_order w)->
 EnvSupport W e->(forall k y,tn_get e k=Some y->WFty y)->
 Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 Forall(fun q=>Support W(snd q))sub.
Proof.
 intros e mi a outer sub[si[sf[Hi[Hw[L Run]]]]]Hmi Env Types WF Parts.
 destruct a as[bs|];[|injection Run as <- <-;exact Parts].
 apply bind_inv in Run as(xs&sx&Read&Ret);injection Ret as <- <-.
 apply Forall_app;split;[|exact Parts].
 exact(app_subst_support w R Fl b Hload 200 e mi bs outer si xs sx Read Hi Hw Hmi Parts WF Env Types L).
Qed.
Lemma family_receiver_support:forall e mi rg sub sr pat sf,
 receiver_at e mi rg sub sr=Good(pat,sf)->Inv sr->st_w sr=w->
 nget(w_syms w)(gs_id rg)=Some rg->Later w P R Fl sf->
 EnvSupport W e->(forall k y,tn_get e k=Some y->WFty y)->
 Forall(fun q=>WFty(snd q))sub->Forall(fun q=>Support W(snd q))sub->Support W pat.
Proof.
 intros e mi rg sub sr pat sf Run Hi Hw Hg L Env Types WF Parts.
 unfold receiver_at in Run;apply bind_inv in Run as(raw&ss&Read&Ret);injection Ret as <- <-.
 apply canon_support;[exact(es_uf Env)|].
 destruct(nassoc(gs_id rg)(sub++tymap e))as[y|]eqn:Found.
 - injection Read as <- <-;eapply family_prefix_lookup;[exact Parts| |exact Found].
   intros k v V;eapply tn_get_support;eassumption.
 - eapply (symbol_type_support_at w R Fl b Hload 200 mi rg(sub++tymap e)e sr raw ss);[exact Read|exact Hi|exact Hw|exact Hg| | |exact Env|exact Types|exact L].
   + eapply family_prefix_lookup;[exact Parts|];intros k v V;eapply tn_get_support;eassumption.
   + eapply family_prefix_lookup;[exact WF|exact Types].
Qed.
Theorem family_scan_receiver_support:forall e mi outer ob tm sp target mid leaf,
 FamilyScanRoute w (P:=P) R Fl e mi outer ob tm sp target mid leaf->
 Tri(en_uf e)->EnvSupport W e->(forall k y,tn_get e k=Some y->WFty y)->In mi(w_order w)->
 Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 forall sr pat sf,
 receiver_at e(let '(lmi,_,_,_) := leaf in lmi)(let '(_,rg,_,_) := leaf in rg)
  (let '(_,_,_,sub) := leaf in sub)sr=Good(pat,sf)->
 Inv sr->st_w sr=w->Later w P R Fl sf->Support W pat.
Proof.
 intros e mi outer ob tm sp target mid leaf Route;induction Route;
 intros T Env Types Hmi WF Parts sr pat sf Read Hi Hw L.
 - pose proof(family_receipt_support e mi a outer sub H Hmi Env Types WF Parts)as Ssub.
   destruct(family_receipt_canonical w R Fl b Hload e mi a outer sub H Hmi T Types WF)as(bm&WFsub&Rest).
   cbn in Read;eapply family_receiver_support;eassumption.
 - pose proof(family_receipt_support e mi a outer sub H Hmi Env Types WF Parts)as Ssub.
   destruct(family_receipt_canonical w R Fl b Hload e mi a outer sub H Hmi T Types WF)as(bm&WFsub&Rest).
   eapply IHRoute;try eassumption.
   exact(loader_mod_in w R Fl b Hload _ _ H5).
Qed.
Theorem family_scan_receiver_wf:forall e mi outer ob tm sp target mid leaf,
 FamilyScanRoute w (P:=P) R Fl e mi outer ob tm sp target mid leaf->
 Tri(en_uf e)->UfWF(en_uf e)->(forall k y,tn_get e k=Some y->WFty y)->In mi(w_order w)->
 Forall(fun q=>WFty(snd q))outer->forall sr pat sf,
 receiver_at e(let '(lmi,_,_,_) := leaf in lmi)(let '(_,rg,_,_) := leaf in rg)
  (let '(_,_,_,sub) := leaf in sub)sr=Good(pat,sf)->
 Inv sr->st_w sr=w->Later w P R Fl sf->WFty pat.
Proof.
 intros e mi outer ob tm sp target mid leaf Route;induction Route;
 intros T UF Types Hmi WF sr pat sf Read Hi Hw L.
 - destruct(family_receipt_canonical w R Fl b Hload e mi a outer sub H Hmi T Types WF)as(bm&WFsub&Rest).
   cbn in Read.
   assert(Mean:forall k,A.nlook k(family_norm_types Fl e(sub++tymap e))=
    option_map(fun y=>tr Fl(canon e y))(nassoc k(sub++tymap e)))by(apply family_norm_lookup).
   destruct(family_receiver_canonical w R Fl b Hload e mi rg sub sr pat sf
    (family_norm_types Fl e(sub++tymap e))Read Hi Hw H4 L T UF
    (family_raw_tmrel Fl e sub Types WFsub)Mean)as[_[Good _]];exact Good.
 - destruct(family_receipt_canonical w R Fl b Hload e mi a outer sub H Hmi T Types WF)as(bm&WFsub&Rest).
   eapply IHRoute;try eassumption;exact(loader_mod_in w R Fl b Hload _ _ H5).
Qed.
End Support.
