(* Adding the type-only implicit inputs of a functor preserves walk tables. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore Merge CanonTypes
 SourceClean ContextClean CtxWalk.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma ws_add_type:forall Fl U e tm les needs n stamp,
 WS Fl U e tm les needs->
 WS Fl U(set_tn e n(YAbs n)stamp)((n,TAbs n)::tm)(les++[LTy n])needs.
Proof.
 intros Fl U e tm les needs n stamp Hws.
 apply ws_les_nokey;[|reflexivity|discriminate|discriminate|discriminate].
 pose proof(context_clean_type e n stamp(ws_clean _ _ _ _ _ _ Hws))as Cout.
 destruct Hws as[T1 T2 T3 T4 T5 T6 T7 T8 T9 Q1 Q2 T10 T11 T12 T13 T14 T15 Urows Crows].
 assert(Ee:forall k,tn_get(set_tn e n(YAbs n)stamp)k=if Nat.eqb k n then Some(YAbs n)else tn_get e k)
  by apply tn_get_set_tn.
 constructor;cbn[set_tn en_uf en_vals en_fns].
 - intro k;rewrite Ee;cbn[A.nlook];destruct(Nat.eqb_spec k n)as[->|];[reflexivity|apply T1].
 - intros k p Hk;apply in_aset in Hk as[Hk|Hk];[injection Hk as -> Ep;subst p;reflexivity|exact(T2 k p Hk)].
 - exact T3.
 - exact T4.
 - exact T5.
 - exact T6.
 - exact T7.
 - exact T8.
 - exact T9.
 - exact Q1.
 - exact Q2.
 - intros E k;rewrite(T10 E k);reflexivity.
 - exact T11.
 - exact T12.
 - intros E k Hk;rewrite(lfirst_same e(set_tn e n(YAbs n)stamp)k les eq_refl);exact(T13 E k Hk).
 - exact T14.
 - exact Cout.
 - exact Urows.
 - intro Settled;exact(Crows Settled).
Qed.

Definition implicit_type_step(f mi:nat)(outs:list nat)(acc:env*list key)(t:nat):M(env*list key):=
 if nmem t outs||is_some(tn_get(fst acc)t)then ret acc else
 let! tg:=sym t in
 if kind_eqb(gs_kind tg)KType then add_symbol_item f(fst acc)mi tg[](snd acc)else ret acc.
Lemma ok_implicit_type_step:forall f mi outs acc t,ok(implicit_type_step f mi outs acc t).
Proof. intros;unfold implicit_type_step;okt. Qed.
Lemma implicit_type_step_shape:forall f mi outs e needs t s e' needs' s' g,
 nget(st_syms s)t=Some g->gs_id g=t->gs_kind g=KType->
 implicit_type_step f mi outs(e,needs)t s=Good((e',needs'),s')->
 needs'=needs/\
 if nmem t outs||is_some(tn_get e t)then e'=e else
 exists stamp,e'=set_tn e t(YAbs t)stamp.
Proof.
 intros f mi outs e needs t s e' needs' s' g Hg Gi Gk H.
 unfold implicit_type_step in H;cbn[fst snd]in H.
 destruct(nmem t outs||is_some(tn_get e t))eqn:C.
 - injection H as <- <- <-;split;reflexivity.
 - apply bind_inv in H as(tg&s1&Hsym&H).
   unfold sym in Hsym;rewrite Hg in Hsym;injection Hsym as <- <-.
   rewrite Gk in H;cbn[kind_eqb]in H.
   destruct f as[|f];[discriminate H|].
   cbn[add_symbol_item check_totality]in H;rewrite Gk,Gi in H.
   assert(N:tn_get e t=None).
   { apply orb_false_iff in C as[_ C]. destruct(tn_get e t);[discriminate C|reflexivity]. }
   rewrite N in H;apply bind_inv in H as(u&s0&Eu&H);injection Eu as <- <-.
   apply bind_inv in H as(stamp&s2&Hfresh&H);injection H as <- <- <-.
   split;[reflexivity|]. exists stamp;reflexivity.
Qed.
Lemma tn_set_missing:forall e n stamp k,
 tn_get(set_tn e n(YAbs n)stamp)k=None<->tn_get e k=None/\k<>n.
Proof.
 intros e n stamp k;rewrite tn_get_set_tn;destruct(Nat.eqb_spec k n)as[->|N];
 split;[discriminate|intros[_ N'];contradiction N';reflexivity|intro H;split;assumption|intros[H _];exact H].
Qed.
Lemma type_symbol_at:forall b w R Fl s t,
 load_world b=Good w->Inv s->st_w s=w->A.kind_sym(W_read w R Fl)t=Some A.DKType->
 exists g,nget(st_syms s)t=Some g/\gs_id g=t/\gs_kind g=KType.
Proof.
 intros b w R Fl s t Hl Hi Hw K.
 unfold A.kind_sym in K;cbn[A.w_decl W_read]in K.
 destruct(nget(w_syms w)t)as[g|]eqn:Hg;[|discriminate K].
 cbn[A.d_ast]in K.
 pose proof(load_world_kind b w Hl t g Hg)as KG;unfold KindOK in KG;rewrite KG in K;injection K as K.
 pose proof(iv_loader _ Hi b ltac:(rewrite Hw;exact Hl)t g ltac:(rewrite Hw;exact Hg))as Hkeep.
 exists g;split;[exact Hkeep|]. split;[exact(iv_ids _ Hi t g Hkeep)|].
 apply kconv_type;exact K.
Qed.
Theorem implicit_types_walk:forall b w R Fl f mi outs ms e needs s e' needs' s' U tm les,
 load_world b=Good w->
 (forall t,In t ms->A.kind_sym(W_read w R Fl)t=Some A.DKType)->
 mfold(implicit_type_step f mi outs)(e,needs)ms s=Good((e',needs'),s')->
 Inv s->st_w s=w->WS Fl U e tm les needs->
 exists added,needs'=needs/\en_uf e'=en_uf e/\
 WS Fl U e' (map(fun t=>(t,TAbs t))(rev added)++tm)(les++map LTy added)needs/\
 (forall t,In t added<->In t ms/\~In t outs/\tn_get e t=None)/\
 (forall t,tn_get e' t=None<->tn_get e t=None/\~In t added).
Proof.
 intros b w R Fl f mi outs ms;induction ms as[|t rest IH];intros e needs s e' needs' s' U tm les Hl Types H Hi Hw WS0.
 - cbn[mfold]in H;injection H as <- <- <-;exists[];cbn;split;[reflexivity|].
   split;[reflexivity|]. split;[rewrite app_nil_r;exact WS0|]. split;intro k;cbn[In];tauto.
 - cbn[mfold]in H;apply bind_inv in H as([e1 n1]&s1&E1&H).
   destruct(ok_implicit_type_step f mi outs(e,needs)t _ _ _ Hi E1)as[I1 F1].
   assert(W1:st_w s1=w)by(rewrite(fr_w _ _ F1);exact Hw).
   destruct(type_symbol_at b w R Fl s t Hl Hi Hw(Types t(or_introl eq_refl)))as(g&Hg&Gi&Gk).
   destruct(implicit_type_step_shape f mi outs e needs t s e1 n1 s1 g Hg Gi Gk E1)as[-> Shape].
   destruct(nmem t outs||is_some(tn_get e t))eqn:C.
   + subst e1.
     destruct(IH e needs s1 e' needs' s' U tm les Hl(fun k Hk=>Types k(or_intror Hk))H I1 W1 WS0)
      as(added&N&Uf&Tables&Members&Missing).
     exists added;split;[exact N|]. split;[exact Uf|]. split;[exact Tables|]. split;[|exact Missing].
     assert(NoNew:~(~In t outs/\tn_get e t=None)).
     { intros[NoOut Nt];rewrite Nt in C;cbn[is_some]in C;rewrite Bool.orb_false_r in C;apply nmem_in in C;exact(NoOut C). }
     intro k;rewrite Members;cbn[In];split.
     * intros[Hr[No Nt]];split;[right;exact Hr|split;assumption].
     * intros[[E|Hr][No Nt]];[subst k;exfalso;apply NoNew;split;assumption|repeat split;assumption].
   + destruct Shape as(stamp&->).
     assert(Nt:tn_get e t=None).
     { apply orb_false_iff in C as[_ C];destruct(tn_get e t);[discriminate C|reflexivity]. }
     assert(NoOut:~In t outs).
     { intro Hin;apply nmem_in in Hin;rewrite Hin in C;discriminate C. }
     pose proof(ws_add_type Fl U e tm les needs t stamp WS0)as WS1.
     destruct(IH _ needs s1 e' needs' s' U ((t,TAbs t)::tm)(les++[LTy t])Hl
       (fun k Hk=>Types k(or_intror Hk))H I1 W1 WS1)as(added&N&Uf&Tables&Members&Missing).
     exists(t::added);split;[exact N|]. split;[exact Uf|]. split.
     * cbn[rev map];rewrite map_app;cbn[map].
       repeat rewrite<-app_assoc in Tables|-*. exact Tables.
     * split;intro k.
       -- cbn[In];rewrite Members,tn_set_missing;clear -Nt NoOut.
          destruct(Nat.eq_dec t k)as[E|NE];[subst k|];firstorder congruence.
       -- rewrite Missing,tn_set_missing;cbn[In];clear -k t added e;firstorder congruence.
Qed.
