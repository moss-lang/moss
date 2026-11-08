(* Nominal support through the platform's actual root-context visitor. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame BodySoundCore BodyTy BodyTr
 WorldSound CanonTypes CtxWalk CtxMerge CtxMethodExec RootContextExec ContextWalkSupport
 ContextClean ContextSupport NominalSupport LibraryFacts ResultMentionsExact MethodCallCore XBridge.
Import ListNotations.
Local Open Scope string_scope.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements functor_fixed].
Section Root.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Notation WW:=(BodySoundCore.W w R Fl).
#[local] Hint Resolve ok_functor_fixed ok_ctx_fuel : okdb.
Lemma root_type_support:forall cf mi xs e needs s e' needs' s' U tm les,
 mfold(fun acc t=>let! tg:=sym t in add_symbol_item cf(fst acc)mi tg[](snd acc))(e,needs)xs s=
   Good((e',needs'),s')->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->WS Fl U e tm les needs->
 Forall(fun x=>A.kind_sym WW x=Some A.DKType)xs->EnvSupport WW e->EnvSupport WW e'.
Proof.
 intros cf mi xs;induction xs as[|x rest IH];intros e needs s e' needs' s' U tm les Run Inv Hw Mi Late W Types Support.
 - cbn[mfold]in Run;injection Run as <- <- <-;exact Support.
 - inversion Types as[|x0 rest0 HeadType Rest];subst x0 rest0.
   cbn[mfold]in Run;apply bind_inv in Run as[[e1 needs1][s1[Head Tail]]].
   apply bind_inv in Head as[tg[mid[Read Add]]].
   destruct(sym_declared_read w R Fl b Hload x A.DKType s tg mid HeadType Read Inv Hw)as[->[Id Loaded]].
   destruct(ok_add_symbol_item cf e mi tg[]needs s _ s1 Inv Add)as[Inv1 Frame].
   assert(Otail:ok(mfold(fun acc t=>let! tg:=sym t in add_symbol_item cf(fst acc)mi tg[](snd acc))(e1,needs1)rest))by okt.
   destruct(Otail _ _ _ Inv1 Tail)as[_ TailFrame].
   pose proof(later_back w P R Fl _ _ TailFrame Late)as Late1.
   destruct(proj1(walk_all w(P:=P)R Fl b Hload(meth_step_holds w R Fl b Hload)cf)
     e mi tg[]needs s e1 needs1 s1 U tm les[] Add Inv Hw Mi Loaded Late1 W eq_refl(Forall_nil _))
     as(added&es&tm1&Entry&Map&W1).
   assert(S1:EnvSupport WW e1).
   { eapply(proj1(context_walk_support w(P:=P)R Fl b Hload cf));
       [exact Add|exact Inv|exact Hw|exact Mi|exact Loaded| |constructor|constructor|
        exact(ws_clean _ _ _ _ _ _ W)|exact(ws_tri _ _ _ _ _ _ W)|exact Support|exact Late1].
     intros k y Bad;discriminate. }
   eapply IH;[exact Tail|exact Inv1|rewrite(fr_w _ _ Frame);exact Hw|exact Mi|exact Late|
     exact W1|exact Rest|exact S1].
Qed.

Lemma root_item_support:forall mi gi e needs sp s e' needs' s' U tm les,
 root_item_step mi gi(e,needs)sp s=Good((e',needs'),s')->
 Inv s->st_w s=w->In mi(w_order w)->In gi(w_order w)->Later w P R Fl s'->
 WS Fl U e tm les needs->EnvSupport WW e->EnvSupport WW e'.
Proof.
 intros mi gi e needs[pa dn brackets]s e' needs' s' U tm les Run Inv Hw Mi Gi Late W Support.
 unfold root_item_step in Run;cbn beta iota zeta in Run.
 destruct pa as[path|],dn as[name|];try(injection Run as <- <- <-;exact Support).
 apply bind_inv in Run as[found[mid[Resolve Run]]].
 destruct(esym_rel w R Fl b Hload gi path s found mid Inv Hw Gi Resolve)as[-> Resolved].
 destruct found as[fg|];[|injection Run as <- <- <-;exact Support].
 destruct Resolved as[Res Loaded].
 destruct(negb(kind_eqb(gs_kind fg)KFunctor)||is_some(nget(en_funs e)(gs_id fg)))eqn:Choice;
   [injection Run as <- <- <-;exact Support|].
 apply orb_false_iff in Choice as[Kind Missing];apply negb_false_iff in Kind;apply kind_eqb_spec in Kind.
 apply bind_inv in Run as[xs[s1[Fixed Run]]];apply bind_inv in Run as[cf[s2[Fuel Run]]].
 apply bind_inv in Run as[[e1 needs1][s3[Types Apply]]].
 destruct(assumed_functor_abstract cf e1 mi fg needs1 s3 e' needs' s' Kind Apply)as(n&names&args&res&Decl).
 destruct(ok_functor_fixed fg s xs s1 Inv Fixed)as[Inv1 Frame1].
 destruct(ok_ctx_fuel s1 cf s2 Inv1 Fuel)as[Inv2 Frame2].
 assert(Otypes:ok(mfold(fun acc t=>let! tg:=sym t in add_symbol_item cf(fst acc)mi tg[](snd acc))(e,needs)xs))by okt.
 destruct(Otypes _ _ _ Inv2 Types)as[Inv3 Frame3].
 destruct(ok_add_symbol_item cf e1 mi fg[]needs1 s3 _ s' Inv3 Apply)as[_ Frame4].
 assert(Late1:Later w P R Fl s1)by(eapply later_back;[exact(fr_trans _ _ _ Frame2(fr_trans _ _ _ Frame3 Frame4))|exact Late]).
 pose proof(functor_fixed_exact w(P:=P)R Fl b Hload fg n names args res s xs s1 Loaded Decl Fixed Inv Hw Late1)as Exact.
 assert(Typed:Forall(fun x=>A.kind_sym WW x=Some A.DKType)xs).
 { apply Forall_forall;intros x Hin;exact(fixed_type b WW Hworld(gs_id fg)x(proj1(Exact x)Hin)). }
 assert(Hw2:st_w s2=w)by(rewrite(fr_w _ _ Frame2),(fr_w _ _ Frame1);exact Hw).
 assert(Late3:Later w P R Fl s3)by exact(later_back w P R Fl _ _ Frame4 Late).
 destruct(root_type_walk w(P:=P)R Fl b Hload cf mi xs e needs s2 e1 needs1 s3 U tm les
   Types Inv2 Hw2 Mi Late3 W Typed)as[added[Map W1]].
 pose proof(root_type_support cf mi xs e needs s2 e1 needs1 s3 U tm les
   Types Inv2 Hw2 Mi Late3 W Typed Support)as Support1.
 eapply(proj1(context_walk_support w(P:=P)R Fl b Hload cf));
   [exact Apply|exact Inv3|rewrite(fr_w _ _ Frame3);exact Hw2|exact Mi|exact Loaded|
    |constructor|constructor|exact(ws_clean _ _ _ _ _ _ W1)|exact(ws_tri _ _ _ _ _ _ W1)|
    exact Support1|exact Late].
 intros k y Bad;discriminate.
Qed.
Lemma root_items_support:forall mi gi specs e needs s e' needs' s' U tm les,
 mfold(root_item_step mi gi)(e,needs)specs s=Good((e',needs'),s')->
 Inv s->st_w s=w->In mi(w_order w)->In gi(w_order w)->Later w P R Fl s'->
 WS Fl U e tm les needs->EnvSupport WW e->EnvSupport WW e'.
Proof.
 intros mi gi specs;induction specs as[|sp rest IH];intros e needs s e' needs' s' U tm les Run Inv Hw Mi Gi Late W Support.
 - cbn[mfold]in Run;injection Run as <- <- <-;exact Support.
 - cbn[mfold]in Run;apply bind_inv in Run as[[e1 needs1][s1[Head Tail]]].
   destruct(ok_root_item_step mi gi(e,needs)sp s _ s1 Inv Head)as[Inv1 Frame].
   assert(Otail:ok(mfold(root_item_step mi gi)(e1,needs1)rest)).
   {apply ok_mfold;intros;apply ok_root_item_step. }
   destruct(Otail _ _ _ Inv1 Tail)as[_ TailFrame].
   pose proof(later_back w P R Fl _ _ TailFrame Late)as Late1.
   destruct(root_item_exec w(P:=P)R Fl b Hload Hworld mi gi e needs sp s e1 needs1 s1 U tm les
     Head Inv Hw Mi Gi Late1 W)as(added&es&tm1&Entry&Map&W1).
   pose proof(root_item_support mi gi e needs sp s e1 needs1 s1 U tm les
     Head Inv Hw Mi Gi Late1 W Support)as Support1.
   eapply IH;[exact Tail|exact Inv1|rewrite(fr_w _ _ Frame);exact Hw|exact Mi|exact Gi|
     exact Late|exact W1|exact Support1].
Qed.

Theorem add_root_context_support:forall e mi needs s e' needs' s' U tm les,
 add_root_context e mi needs s=Good((e',needs'),s')->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->
 WS Fl U e tm les needs->EnvSupport WW e->EnvSupport WW e'.
Proof.
 intros e mi needs s e' needs' s' U tm les Run Inv Hw Mi Late W Support.
 assert(O:ok(add_root_context e mi needs))by(unfold add_root_context;okt).
 destruct(O _ _ _ Inv Run)as[_ Frame].
 pose proof(later_back w P R Fl _ _ Frame Late)as Late0.
 unfold add_root_context in Run;apply bind_inv in Run as[wm0[s0[Lang Run]]].
 unfold lang in Lang;apply bind_inv in Lang as[snapshot[mid[Read Lang]]].
 unfold get in Read;injection Read as <- <-;injection Lang as <- <-.
 rewrite(lt_lang _ _ _ _ _ Late0)in Run.
 destruct(sassoc "wasm"(lang_modules w))as[wm|]eqn:Wasm;
   [|injection Run as <- <- <-;exact Support].
 apply bind_inv in Run as[gm[mid[Mod Run]]];unfold the_mod in Mod;injection Mod as <- <-;rewrite Hw in Run.
 destruct(sassoc "Gc"(gm_names(get_mod w wm)))as[x|]eqn:Gc;
   [|injection Run as <- <- <-;exact Support].
 apply bind_inv in Run as[gc[mid[Symbol Run]]].
 pose proof(LibraryFacts.lang_symbol_scope b w R Fl Hload "wasm" false "Gc" wm x Wasm Gc)as Lib.
 destruct Lib as(d&path&Decl&Library&Scope).
 destruct(sym_world_decl_read w R Fl b Hload x d s gc mid Decl Symbol Inv Hw)as[->[Id Loaded]].
 destruct(gs_kind gc);try(injection Run as <- <- <-;exact Support).
 destruct(gs_decl gc)as[asm declarations|tn|an tau|tag parameters payload|unit|vn vt init|receiver name ps rt body|cn its|fnm names args res body];
   try(injection Run as <- <- <-;exact Support).
 change(mfold(root_item_step mi(gs_mod gc))(e,needs)its s=Good((e',needs'),s'))in Run.
 eapply root_items_support;[exact Run|exact Inv|exact Hw|exact Mi|
   exact(loader_mod_in w R Fl b Hload _ _ Loaded)|exact Late|exact W|exact Support].
Qed.
End Root.
