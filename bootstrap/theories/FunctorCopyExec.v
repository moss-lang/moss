(* Actual abstract-to-abstract functor binding. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy
 Merge CanonTypes CtxWalk CtxMerge FunctorContext ResultMentionsExact
 SignatureLeavesExec CopyFixedExec FunctorCopyContext FunctorCopyMap ProvideResultFixed.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Lemma bind_fixed_present_total:forall mi e g xs,
 (forall x,In x xs->tn_get e x<>None)->forall s,bind_fixed_present mi e g xs s=Good(tt,s).
Proof.
 intros mi e g xs;induction xs as[|x xs IH];intros Present s;[reflexivity|].
 unfold bind_fixed_present;cbn[miter].
 destruct(tn_get e x)eqn:Look;[|exfalso;exact(Present x(or_introl eq_refl)Look)].
 cbn[is_some mbind ret];apply IH;intros y Iny;apply Present;right;exact Iny.
Qed.
Section Copy.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Ltac copy_frame E Hi NewI NewF :=
 lazymatch type of E with ?m ?s=Good(?a,?s')=>
 let O:=fresh"O"in assert(O:ok m)by okt;
 destruct(O s a s' Hi E)as[NewI NewF];clear O end.
Ltac copy_worlds := repeat multimatch goal with
 | F:Fr ?s ?s',W:st_w ?s=w|-_=>
   tryif(match goal with _:st_w s'=w|-_=>idtac end) then fail 0 else
     let N:=fresh"World"in assert(N:st_w s'=w)by(rewrite(fr_w _ _ F);exact W)
 end.
Ltac copy_laters := repeat multimatch goal with
 | F:Fr ?s ?s',L:Later w P R Fl ?s'|-_=>
   tryif(match goal with _:Later w P R Fl s|-_=>idtac end) then fail 0 else
     let N:=fresh"Late"in assert(N:Later w P R Fl s)by exact(later_back w P R Fl _ _ F L)
 end.
Theorem abstract_copy_exec_code:forall fuel c e fg bg fname fps fargs fres gname gps gargs gres pa pg s xs e' s' F C,
 A.WorldOf b WW ->
 nget(w_syms w)(gs_id fg)=Some fg ->gs_decl fg=ADFunctor fname fps fargs fres None ->
 nget(w_syms w)(gs_id bg)=Some bg ->gs_decl bg=ADFunctor gname gps gargs gres None ->
 Elab.resolve_sym(cx_mod c)pg s=Good(Some bg,s) ->
 A.resolve_sym WW(A.fr_mod F)pa=Some(gs_id fg) ->
 bind_functor fuel c e fg(AEPath pg None)s=Good((xs,e'),s') ->
 Inv s ->st_w s=w ->Later w P R Fl s' ->CxRel w R Fl c e F C ->
 exists C' wrap,A.BindItem WW P F C(Spec(Some pa)None None,Some(AEPath pg None))wrap C' F[] /\
 CxRel w R Fl c e' F C' /\(forall tail,wrap tail=fold_stmts xs tail) /\
 exists sm,xs=[SBindFunA(gs_id fg)(gs_id bg)sm] /\wrap=XBindFunA(gs_id fg)(gs_id bg)sm.
Proof.
 intros[|fuel] c e fg bg fname fps fargs fres gname gps gargs gres pa pg s xs e' s' F C
  Wo Fg Fdecl Bg Bdecl ResTarget PathTarget Run Hi Hw L Hc;
 cbn[bind_functor]in Run;[discriminate|].
 unfold functor_parts in Run;rewrite Fdecl in Run.
 apply bind_inv in Run as(found&s0&Resolve&Run).
 rewrite ResTarget in Resolve;injection Resolve as <- <-.
 assert(Kind:gs_kind bg=KFunctor).
 {pose proof(signature_loaded_kind w R Fl b Hload _ _ Bg)as K.
  unfold KindOK in K;rewrite Bdecl in K;destruct(gs_kind bg);cbn[A.kind_of kconv]in K;congruence. }
 rewrite Kind in Run;cbn[kind_eqb negb]in Run;rewrite Bdecl in Run.
 apply bind_inv in Run as(u&s0&Avail&Run).
 destruct(nget(en_funs e)(gs_id bg))eqn:Here;[injection Avail as <- <-|discriminate].
 apply bind_inv in Run as(fitems&sFItems&FItems&Run);copy_frame FItems Hi IFItems FFItems.
 apply bind_inv in Run as(gitems&sGItems&GItems&Run);copy_frame GItems IFItems IGItems FGItems.
 apply bind_inv in Run as(u&sGuard&LeafGuard&Run).
 destruct(filter(fun q=>negb(existsb(fun r=>leaf_eqb(fst q)(fst r))gitems))fitems)as[|q qs]eqn:Missing in LeafGuard;[|discriminate].
 destruct(filter(fun q=>negb(existsb(fun r=>leaf_eqb(fst q)(fst r))fitems))gitems)as[|q qs]eqn:More in LeafGuard;[injection LeafGuard as <- <-|discriminate].
 apply bind_inv in Run as([ef fneeds]&sF&EF&Run);copy_frame EF IGItems IF FF.
 apply bind_inv in Run as([eg gneeds]&sG&EG&Run);copy_frame EG IF IG FG.
 apply bind_inv in Run as(xf0&sX0&XF0&Run);copy_frame XF0 IG IX0 FX0.
 apply bind_inv in Run as(uPresent&sPresent&Present&Run);destruct uPresent;copy_frame Present IX0 IPresent FPresent.
 apply bind_inv in Run as(stamp&sStamp&Stamp&Run);copy_frame Stamp IPresent IStamp FStamp.
 apply bind_inv in Run as(uNeeds&sNeeds&Needs&Run);destruct uNeeds;change(copy_needs_guard(cx_mod c)fg bg fneeds gneeds sStamp=Good(tt,sNeeds))in Needs;
 copy_frame Needs IStamp INeeds FNeeds.
 apply bind_inv in Run as(xh&sXH&XH&Run);copy_frame XH INeeds IXH FXH.
 apply bind_inv in Run as(uCurrent&sCurrent&Current&Run);destruct uCurrent;copy_frame Current IXH ICurrent FCurrent.
 apply bind_inv in Run as(xf&sXF&XF&Run);copy_frame XF ICurrent IXF FXF.
 apply bind_inv in Run as(uSubset&sSubset&Subset&Run);destruct uSubset;change(copy_fixed_subset_guard(cx_mod c)fg bg xf xh sXF=Good(tt,sSubset))in Subset;
 copy_frame Subset IXF ISubset FSubset.
 apply bind_inv in Run as(profile&sProfile&Profile&Run);copy_frame Profile ISubset IProfile FProfile.
 apply bind_inv in Run as(sm&sSm&Sm&Run);copy_frame Sm IProfile ISm FSm.
 apply bind_inv in Run as(sa&sSa&Sa&Run);copy_frame Sa ISm ISa FSa.
 apply bind_inv in Run as(uEvent&sEvent&Event&Run);destruct uEvent;copy_frame Event ISa IEvent FEvent.
 injection Run as <- <- <-.
 copy_worlds;copy_laters.
 destruct(fun_env_context_valid w (P:=P) R Fl b Hload fg fname fps fargs fres sGItems ef fneeds sF
   Wo Fg Fdecl EF IGItems ltac:(eassumption) ltac:(eassumption))
   as(CF&FC&FT&TF&KF&MF).
 destruct(fun_env_context_valid w (P:=P) R Fl b Hload bg gname gps gargs gres sF eg gneeds sG
   Wo Bg Bdecl EG IF ltac:(eassumption) ltac:(eassumption))
   as(CG&GC&GT&TG&KG&MG).
 pose proof(functor_fixed_exact w (P:=P) R Fl b Hload fg fname fps fargs fres sG xf0 sX0
   Fg Fdecl XF0 IG ltac:(eassumption) ltac:(eassumption))as Dom0.
 pose proof(functor_fixed_exact w (P:=P) R Fl b Hload fg fname fps fargs fres sCurrent xf sXF
   Fg Fdecl XF ICurrent ltac:(eassumption) ltac:(eassumption))as DomF.
 pose proof(functor_fixed_exact w (P:=P) R Fl b Hload bg gname gps gargs gres sNeeds xh sXH
   Bg Bdecl XH INeeds ltac:(eassumption) ltac:(eassumption))as DomH.
 destruct(bind_fixed_present_success(cx_mod c)e fg xf0 sX0 sPresent Present)as[_ Av0].
 assert(Av:forall x,In x xf->tn_get e x<>None).
 {intros x InX;apply Av0;apply Dom0;apply DomF;exact InX. }
 set(fx:=output_fixed_values(A.c_ty C)xf).
 destruct(actual_functor_snapshot w (P:=P) R Fl b Hload fg fname fps fargs fres(cx_mod c)e c F C
   sCurrent xf sXF sXF stamp sa Fg Fdecl XF
   (bind_fixed_present_total(cx_mod c)e fg xf Av sXF) ICurrent
   ltac:(eassumption) ltac:(eassumption) Hc)as[Snapshot Out].
 assert(Same:A.SameResult WW(gs_id fg)(gs_id bg)).
 {destruct(signature_functor_leaves w R Fl b Hload fg fname fps fargs fres None
   s fitems sFItems Fg Fdecl FItems Hi Hw)as[EqF FRel].
  subst sFItems.
  destruct(signature_functor_leaves w R Fl b Hload bg gname gps gargs gres None
   s gitems sGItems Bg Bdecl GItems IFItems Hw)as[EqG GRel].
  intro k;rewrite <-FRel,<-GRel;exact(checked_leaf_sets_equal fitems gitems Missing More k). }
 assert(Agreement:A.FixedAgreement WW C(gs_id bg)).
 {eapply(actual_copy_fixed_agreement w (P:=P) R Fl b Hload bg gname gps gargs gres(cx_mod c)e c F C
   sNeeds xh sXH sCurrent);eassumption. }
 assert(SubsetSemantic:forall x,A.Fixed WW(gs_id bg)x->A.Fixed WW(gs_id fg)x).
 {pose proof(copy_fixed_guard_subset _ _ _ _ _ _ _ _ Subset)as Incl.
  intros x Fix;apply DomF;apply Incl;apply DomH;exact Fix. }
 assert(Available:forall x,A.Fixed WW(gs_id fg)x->A.nlook x(A.c_ty C)<>None).
 {intros x Fix;rewrite(cr_ty _ _ _ _ _ _ _ Hc).
  destruct(tn_get e x)eqn:Look;[discriminate|].
  exfalso;exact(Av x(proj2(DomF x)Fix)Look). }
 assert(Held:A.nlook(gs_id bg)(A.c_funs C)<>None).
 {intro MissingH;pose proof(cr_funspresent _ _ _ _ _ _ _ Hc(gs_id bg))as Lookup.
  rewrite MissingH,Here in Lookup;discriminate. }
 assert(ProviderPath:A.resolve_sym WW(A.fr_mod F)pg=Some(gs_id bg)).
 {rewrite(cr_mod _ _ _ _ _ _ _ Hc).
  destruct(esym_rel w R Fl b Hload(cx_mod c)pg s(Some bg)s Hi Hw
    (cr_mod_in _ _ _ _ _ _ _ Hc)ResTarget)as[_[Path _]];exact Path. }
 assert(Map:sm=map(fun k=>(A.rkey_of CG(key_tr Fl k),A.rkey_of CF(key_tr Fl k)))gneeds).
 {eapply(copy_map_from_tables w (P:=P) R Fl eg ef CG CF gneeds sProfile sm sSm);eassumption. }
 exists(A.set_fun C(gs_id fg)fx),(XBindFunA(gs_id fg)(gs_id bg)sm).
 split.
 - eapply A.BFunCopy with(Cu:=CF)(nsu:=map(key_tr Fl)fneeds)(Ch:=CG)(nsh:=map(key_tr Fl)gneeds).
   + exact PathTarget.
   + exact ProviderPath.
   + exact Held.
   + exact FC.
   + exact GC.
   + apply incl_map;exact(copy_needs_guard_subset _ _ _ _ _ _ _ _ Needs).
   + exact Available.
   + exact SubsetSemantic.
   + exact Agreement.
   + exact Same.
   + rewrite map_map;exact Map.
   + exact Snapshot.
 - split;[exact Out|]. split;[intro;reflexivity|exists sm;split;reflexivity].
Qed.
Theorem abstract_copy_exec:forall fuel c e fg bg fname fps fargs fres gname gps gargs gres pa pg s xs e' s' F C,
 A.WorldOf b WW ->
 nget(w_syms w)(gs_id fg)=Some fg ->gs_decl fg=ADFunctor fname fps fargs fres None ->
 nget(w_syms w)(gs_id bg)=Some bg ->gs_decl bg=ADFunctor gname gps gargs gres None ->
 Elab.resolve_sym(cx_mod c)pg s=Good(Some bg,s) ->
 A.resolve_sym WW(A.fr_mod F)pa=Some(gs_id fg) ->
 bind_functor fuel c e fg(AEPath pg None)s=Good((xs,e'),s') ->
 Inv s ->st_w s=w ->Later w P R Fl s' ->CxRel w R Fl c e F C ->
 exists C' wrap,A.BindItem WW P F C(Spec(Some pa)None None,Some(AEPath pg None))wrap C' F[] /\
 CxRel w R Fl c e' F C' /\forall tail,wrap tail=fold_stmts xs tail.
Proof.
 intros fuel c e fg bg fname fps fargs fres gname gps gargs gres pa pg s xs e' s' F C
  Wo Fg Fdecl Bg Bdecl Resolve Target Run Hi Hw L Hc.
 destruct(abstract_copy_exec_code fuel c e fg bg fname fps fargs fres gname gps gargs gres
   pa pg s xs e' s' F C Wo Fg Fdecl Bg Bdecl Resolve Target Run Hi Hw L Hc)
 as(C'&wrap&Bind&Out&Fold&Code).
 exists C',wrap;auto.
Qed.
End Copy.
