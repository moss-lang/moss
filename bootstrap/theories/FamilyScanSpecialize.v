(* Recorded actual context brackets transport the matching substitution to the receiver. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrameCore ElabInv BodySoundCore BodyTr BodyTy Merge CanonTypes CanonBodyTy
 TyOfSubst SourceReadingExtend TySupport ProvideResultCanonical FamilyLeafScan FamilyReceiverExec FamilyReceiverTransfer.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition family_norm_types(Fl:string->nat)(e:env)(tm:list(nat*ety)):=
 map(fun q=>(fst q,tr Fl(canon e(snd q))))tm.
Lemma family_norm_lookup:forall Fl e tm k,
 A.nlook k(family_norm_types Fl e tm)=option_map(fun y=>tr Fl(canon e y))(nassoc k tm).
Proof.
 intros Fl e tm;induction tm as[|[p y]tm IH];intro k;[reflexivity|].
 cbn[family_norm_types A.nlook nassoc assoc fst snd map];destruct(Nat.eqb k p);[reflexivity|apply IH].
Qed.
Lemma family_norm_app:forall Fl e xs ys,
 family_norm_types Fl e(xs++ys)=family_norm_types Fl e xs++family_norm_types Fl e ys.
Proof. intros;apply map_app. Qed.
Section Specialize.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Lemma family_raw_tmrel:forall e outer,
 (forall k y,tn_get e k=Some y->WFty y)->Forall(fun q=>WFty(snd q))outer->
 TmRel Fl(outer++tymap e)(map(trp Fl)(outer++tymap e))e.
Proof.
 intros e outer Types Parts;rewrite map_app;apply tmrel_app;[|reflexivity|exact Parts].
 apply tmrel_tymap;[exact Types|intro k;rewrite nlook_trp,nassoc_tymap;reflexivity].
Qed.
Lemma family_receipt_canonical:forall e mi a outer sub,
 FamilyBracketReceipt w (P:=P) R Fl e mi a outer sub->In mi(w_order w)->Tri(en_uf e)->
 (forall k y,tn_get e k=Some y->WFty y)->Forall(fun q=>WFty(snd q))outer->
 exists bm,Forall(fun q=>WFty(snd q))sub/\
 family_norm_types Fl e sub=bm++family_norm_types Fl e outer/\
 A.OutputBrackets W(gm_path(get_mod w mi))
 (family_norm_types Fl e outer++family_norm_types Fl e(tymap e))a bm.
Proof.
 intros e mi a outer sub[si[sf[Hi[Hw[L Run]]]]]Hmi T Types Parts.
 destruct a as[bs|].
 - apply bind_inv in Run as(xs&sx&Read&Ret);injection Ret as <- <-.
   destruct(output_brackets_canonical w R Fl b Hload e mi bs outer si xs sx
    (family_norm_types Fl e(outer++tymap e))Read Hi Hw Hmi L T
    (family_raw_tmrel e outer Types Parts)(family_norm_lookup Fl e(outer++tymap e)))as[Br WF].
   exists(family_norm_types Fl e xs);split;[apply Forall_app;split;assumption|];split.
   + apply family_norm_app.
   + rewrite family_norm_app in Br;exact Br.
 - injection Run as <- <-;exists[];split;[exact Parts|];split;[reflexivity|constructor].
Qed.
Theorem family_scan_specializes:forall e mi outer ob tm sp target mid leaf,
 FamilyScanRoute w (P:=P) R Fl e mi outer ob tm sp target mid leaf->
 A.WorldOf b W->Tri(en_uf e)->UfWF(en_uf e)->
 (forall k y,tn_get e k=Some y->WFty y)->In mi(w_order w)->
 Forall(fun q=>WFty(snd q))outer->TySupport W target->
 forall sigma completed,
 SubRel sigma(family_norm_types Fl e outer++family_norm_types Fl e(tymap e))completed->
 @TypeExtension W(ob++tm)completed->
 forall sr pat sf,
 receiver_at e(let '(lmi,_,_,_) := leaf in lmi)(let '(_,rg,_,_) := leaf in rg)
  (let '(_,_,_,sub) := leaf in sub)sr=Good(pat,sf)->
 Inv sr->st_w sr=w->Later w P R Fl sf->
 ExecutionModel.app_subst sigma(tr Fl pat)=target.
Proof.
 intros e mi outer ob tm sp target mid leaf Route;induction Route;
 intros Wo T UF Types Hmi Parts Supported sigma completed SR TE sr pat sf Read Hi Hw L.
 - destruct(family_receipt_canonical e mi a outer sub H Hmi T Types Parts)as(bm&WFsub&Norm&Brm).
   destruct(family_brackets_transfer W sigma(gm_path(get_mod w mi))
    (family_norm_types Fl e outer++family_norm_types Fl e(tymap e))(ob++tm)completed a bm bs
    SR TE Brm H2)as[Eq[SR' TE']].
   cbn in Read.
   assert(Mean:forall k,A.nlook k(family_norm_types Fl e sub++family_norm_types Fl e(tymap e))=
    option_map(fun y=>tr Fl(canon e y))(nassoc k(sub++tymap e))).
   {intro k;rewrite <-family_norm_app;apply family_norm_lookup. }
   destruct(family_receiver_canonical w R Fl b Hload e mi rg sub sr pat sf
    (family_norm_types Fl e sub++family_norm_types Fl e(tymap e))Read Hi Hw H4 L T UF
    (family_raw_tmrel e sub Types WFsub)Mean)as[Recv Rest].
   eapply family_receiver_transfer with(marked:=family_norm_types Fl e sub++family_norm_types Fl e(tymap e));[exact Wo|exact Supported| |exact TE'| |exact H3].
   + rewrite Norm,<-app_assoc;exact SR'.
   + rewrite H5 in Recv;exact Recv.
 - destruct(family_receipt_canonical e mi a outer sub H Hmi T Types Parts)as(bm&WFsub&Norm&Brm).
   destruct(family_brackets_transfer W sigma(gm_path(get_mod w mi))
    (family_norm_types Fl e outer++family_norm_types Fl e(tymap e))(ob++tm)completed a bm bs
    SR TE Brm H3)as[Eq[SR' TE']].
   eapply IHRoute with(sigma:=sigma)(completed:=bs++completed);try eassumption.
   + exact(loader_mod_in w R Fl b Hload _ _ H5).
   + rewrite Norm,<-app_assoc;exact SR'.
   + rewrite <-app_assoc;exact TE'.
Qed.
End Specialize.
