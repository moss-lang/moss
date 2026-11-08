(* Actual defined-provider closure binding, apart from the separately checked linked body. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy
 Merge CanonTypes CtxWalk CtxMerge FunctorContext ResultMentionsExact
 SignatureLeavesExec CopyFixedExec FunctorCopyContext FunctorCopyMap ProvideResultFixed FunctorDefinedContext FunctorClosureCapture FunctorClosureTrace.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
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
Theorem defined_closure_exec_code:forall fuel c e fg bg fname fps fargs fres gname gps gargs gres bodies pa pg s xs e' s' F C,
 A.WorldOf b WW ->
 nget(w_syms w)(gs_id fg)=Some fg ->gs_decl fg=ADFunctor fname fps fargs fres None ->
 nget(w_syms w)(gs_id bg)=Some bg ->gs_decl bg=ADFunctor gname gps gargs gres (Some bodies) ->
 Elab.resolve_sym(cx_mod c)pg s=Good(Some bg,s) ->
 A.resolve_sym WW(A.fr_mod F)pa=Some(gs_id fg) ->
 bind_functor fuel c e fg(AEPath pg None)s=Good((xs,e'),s') ->
 Inv s ->st_w s=w ->Later w P R Fl s' ->CxRel w R Fl c e F C ->
 exists C' h before after,
 link_body (pred fuel) fg bg before=Good(h,after) /\ Inv before /\ Later w P R Fl after /\
 CxRel w R Fl c e' F C' /\
 (A.LinkBody WW P(gs_id fg)(gs_id bg)h ->
 A.BindItem WW P F C(Spec(Some pa)None None,Some(AEPath pg None))
 (fun tail=>fold_stmts xs tail) C' F[]).
Proof.
 intros[|fuel] c e fg bg fname fps fargs fres gname gps gargs gres bodies pa pg s xs e' s' F C
  Wo Fg Fdecl Bg Bdecl ResTarget PathTarget Run Hi Hw L Hc;
 cbn[bind_functor]in Run;[discriminate|].
 unfold functor_parts in Run;rewrite Fdecl in Run.
 apply bind_inv in Run as(found&s0&Resolve&Run).
 rewrite ResTarget in Resolve;injection Resolve as <- <-.
 assert(Kind:gs_kind bg=KFunctor).
 {pose proof(signature_loaded_kind w R Fl b Hload _ _ Bg)as K.
  unfold KindOK in K;rewrite Bdecl in K;destruct(gs_kind bg);cbn[A.kind_of kconv]in K;congruence. }
 rewrite Kind in Run;cbn[kind_eqb negb]in Run;rewrite Bdecl in Run.
 apply bind_inv in Run as(u&s0&Avail&Run);injection Avail as <- <-.
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
 change(defined_functor_binding fuel c e fg bg (Some bodies) ef eg fneeds gneeds xf0 stamp sStamp=Good((xs,e'),s'))in Run.
 unfold defined_functor_binding in Run.
 apply bind_inv in Run as(uInputs&sInputs&Inputs&Run);copy_frame Inputs IStamp IInputs FInputs.
 apply bind_inv in Run as(uTotal&sTotal&Total&Run);copy_frame Total IInputs ITotal FTotal.
 apply bind_inv in Run as(h&sLink&Link&Run);copy_frame Link ITotal ILink FLink.
 apply bind_inv in Run as(uCaptured&sCaptured&Captured&Run);destruct uCaptured.
 change(closure_capture_guard(cx_mod c)e fg bg(closure_captured fneeds gneeds)sLink=Good(tt,sCaptured))in Captured.
 copy_frame Captured ILink ICaptured FCaptured.
 apply bind_inv in Run as(uRead&sRead&Read&Run);copy_frame Read ICaptured IRead FRead.
 apply bind_inv in Run as(sm&sSm&Sm&Run);copy_frame Sm IRead ISm FSm.
 apply bind_inv in Run as(smc&sSmc&Smc&Run);copy_frame Smc ISm ISmc FSmc.
 apply bind_inv in Run as(fam&sFam&Fam&Run);copy_frame Fam ISmc IFam FFam.
 apply bind_inv in Run as(uFamily&sFamily&Family&Run);copy_frame Family IFam IFamily FFamily.
 apply bind_inv in Run as(xf&sXF&XF&Run);copy_frame XF IFamily IXF FXF.
 apply bind_inv in Run as(ep&sPins&Pins&Run);copy_frame Pins IXF IPins FPins.
 apply bind_inv in Run as(uCurrent&sCurrent&Current&Run);copy_frame Current IPins ICurrent FCurrent.
 apply bind_inv in Run as(sa&sSa&Sa&Run);copy_frame Sa ICurrent ISa FSa.
 apply bind_inv in Run as(uEvent&sEvent&Event&Run);copy_frame Event ISa IEvent FEvent.
 injection Run as <- <- <-.
 copy_worlds;copy_laters.
 destruct(fun_env_context_valid w (P:=P) R Fl b Hload fg fname fps fargs fres sGItems ef fneeds sF
   Wo Fg Fdecl EF IGItems ltac:(eassumption) ltac:(eassumption))
   as(CF&FC&FT&TF&KF&MF).
 destruct(defined_fun_env_context_valid w P R Fl b Hload bg gname gps gargs gres bodies sF eg gneeds sG
   Wo Bg Bdecl EG IF ltac:(eassumption) ltac:(eassumption))
   as(CG&GC&GT&TG&KG&MG).
 pose proof(functor_fixed_exact w (P:=P) R Fl b Hload fg fname fps fargs fres sG xf0 sX0
   Fg Fdecl XF0 IG ltac:(eassumption) ltac:(eassumption))as Dom0.
 pose proof(functor_fixed_exact w (P:=P) R Fl b Hload fg fname fps fargs fres sFamily xf sXF
   Fg Fdecl XF IFamily ltac:(eassumption) ltac:(eassumption))as DomF.
 destruct(bind_fixed_present_success(cx_mod c)e fg xf0 sX0 sPresent Present)as[_ Av0].
 assert(Av:forall x,In x xf->tn_get e x<>None).
 {intros x InX;apply Av0;apply Dom0;apply DomF;exact InX. }
 set(fx:=output_fixed_values(A.c_ty C)xf).
 destruct(actual_functor_snapshot w (P:=P) R Fl b Hload fg fname fps fargs fres(cx_mod c)e c F C
   sFamily xf sXF sXF stamp sa Fg Fdecl XF
   (bind_fixed_present_total(cx_mod c)e fg xf Av sXF) IFamily
   ltac:(eassumption) ltac:(eassumption) Hc)as[Snapshot Out].
 assert(Same:A.SameResult WW(gs_id fg)(gs_id bg)).
 {destruct(signature_functor_leaves w R Fl b Hload fg fname fps fargs fres None
   s fitems sFItems Fg Fdecl FItems Hi Hw)as[EqF FRel].
  subst sFItems.
  destruct(signature_functor_leaves w R Fl b Hload bg gname gps gargs gres (Some bodies)
   s gitems sGItems Bg Bdecl GItems IFItems Hw)as[EqG GRel].
  intro k;rewrite <-FRel,<-GRel;exact(checked_leaf_sets_equal fitems gitems Missing More k). }
 assert(Available:forall x,A.Fixed WW(gs_id fg)x->A.nlook x(A.c_ty C)<>None).
 {intros x Fix;rewrite(cr_ty _ _ _ _ _ _ _ Hc).
  destruct(tn_get e x)eqn:Look;[discriminate|].
  exfalso;exact(Av x(proj2(DomF x)Fix)Look). }
 assert(ProviderPath:A.resolve_sym WW(A.fr_mod F)pg=Some(gs_id bg)).
 {rewrite(cr_mod _ _ _ _ _ _ _ Hc).
  destruct(esym_rel w R Fl b Hload(cx_mod c)pg s(Some bg)s Hi Hw
    (cr_mod_in _ _ _ _ _ _ _ Hc)ResTarget)as[_[Path _]];exact Path. }
 assert(Finj:forall a b,Fl a=Fl b->a=b)by exact(A.wo_fld Wo).
 assert(Map:sm=map(fun k=>(A.rkey_of CG k,A.rkey_of CF k))
   (filter(fun k=>existsb(item_eqb k)(map(key_tr Fl)fneeds))(map(key_tr Fl)gneeds))).
 {eapply(closure_linked_map w P R Fl Finj);eassumption. }
 assert(CapturedMap:smc=map(fun k=>(A.rkey_of CG k,A.rkey_of C(A.at_site C k)))
   (filter(fun k=>negb(existsb(item_eqb k)(map(key_tr Fl)fneeds)))(map(key_tr Fl)gneeds))).
 {eapply(closure_captured_split_map w P R Fl b Hload Finj);eassumption. }
 assert(Provided:forall k k',In(k,k')smc->A.provides C k').
 {eapply(closure_captured_available w P R Fl b Hload c e F C eg CG
    (closure_captured fneeds gneeds)(cx_mod c)fg bg sLink tt sCaptured sSm smc sSmc);try eassumption.
  apply closure_filter_clean;exact KG. }
 exists(A.set_fun C(gs_id fg)fx),h,sTotal,sLink.
 split;[exact Link|];split;[exact ITotal|];split;[assumption|];split;[exact Out|].
 intro Linked.
 change(A.BindItem WW P F C(Spec(Some pa)None None,Some(AEPath pg None))
   (XBindFun(gs_id fg)h sm smc)(A.set_fun C(gs_id fg)fx)F[]).
 eapply A.BFunClosure with(Cu:=CF)(nsu:=map(key_tr Fl)fneeds)(Cg:=CG)(nsg:=map(key_tr Fl)gneeds).
 - exact PathTarget.
 - exact ProviderPath.
 - exists fname,fps,fargs,fres;change(A.ast_of(W_read w R Fl)(gs_id fg)=Some(ADFunctor fname fps fargs fres None));unfold A.ast_of;rewrite(w_decl_read w R Fl _ _ Fg);cbn;now rewrite Fdecl.
 - exists gname,gps,gargs,gres,bodies;change(A.ast_of(W_read w R Fl)(gs_id bg)=Some(ADFunctor gname gps gargs gres (Some bodies)));unfold A.ast_of;rewrite(w_decl_read w R Fl _ _ Bg);cbn;now rewrite Bdecl.
 - exact FC.
 - exact GC.
 - exact Available.
 - exact Same.
 - exact Linked.
 - exact Map.
 - exact CapturedMap.
 - exact Provided.
 - exact Snapshot.
Qed.
End Copy.
