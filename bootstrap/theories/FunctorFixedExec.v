(* Fixed inputs are present in an abstract functor's actual input context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySound BodyTr BodyTy CanonTypes
 CtxMerge FunctorContext XBridgeCtx NeedExec BindFunctorCtx.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma nlook_app_present:forall X k (a b:list(nat*X)),
 A.nlook k a<>None->A.nlook k(a++b)<>None.
Proof.
 intros X k a;induction a as[|[v x]a IH];intros b H;[contradiction H;reflexivity|].
 cbn in *;destruct(Nat.eqb k v);[discriminate|auto].
Qed.
Lemma nlook_app_missing:forall X k(a b:list(nat*X)),
 A.nlook k a=None->A.nlook k(a++b)=A.nlook k b.
Proof.
 intros X k a;induction a as[|[v x]a IH];intros b H;[reflexivity|].
 cbn in *;destruct(Nat.eqb k v);[discriminate|auto].
Qed.
Lemma self_map_present:forall ts t,In t ts->A.nlook t(map(fun x=>(x,TAbs x))ts)<>None.
Proof.
 intros ts;induction ts as[|v ts IH];intros t H;[contradiction|].
 cbn. destruct(Nat.eqb t v)eqn:E;[discriminate|].
 destruct H as[<-|H];[rewrite Nat.eqb_refl in E;discriminate|auto].
Qed.
Lemma functx_abstract_mentions_present:forall W u C ns d n names args res,
 A.w_decl W u=Some d->A.d_ast d=ADFunctor n names args res None->
 A.FunCtx W u C ns->forall t,A.Mentions W u t->A.nlook t(A.c_ty C)<>None.
Proof.
 intros W u C ns d n names args res Hd D
 (d'&ps&a&r&body&es&tm&tm'&sigma&implicit&Hde&Hparts&IE&M&T&MG&->&N)t Men.
 rewrite Hd in Hde;injection Hde as <-. unfold A.functor_parts in Hparts;rewrite D in Hparts.
 injection Hparts as <- <- <- <-. cbn[A.entries_cx A.c_ty]. rewrite nlook_map_meanings.
 assert(Present:A.nlook t tm'<>None).
 { rewrite T. destruct(A.nlook t tm)eqn:E.
   - destruct(A.nlook t(map(fun x=>(x,TAbs x))implicit))eqn:Ei.
     + apply nlook_app_present;rewrite Ei;discriminate.
     + rewrite nlook_app_missing by exact Ei;rewrite E;discriminate.
   - apply nlook_app_present. apply self_map_present. apply M;auto. }
 destruct(A.nlook t tm');[discriminate|contradiction].
Qed.
Lemma functx_abstract_fixed_present:forall W u C ns d n names args res,
 A.w_decl W u=Some d->A.d_ast d=ADFunctor n names args res None->
 A.FunCtx W u C ns->forall t,A.Fixed W u t->A.nlook t(A.c_ty C)<>None.
Proof.
 intros W u C ns d n names args res Hd D FC t[Men Poly].
 eapply functx_abstract_mentions_present;eauto.
Qed.

Section Recorded.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat).
Lemma recorded_fixed_agreement:forall c e F C u fx,
 CxRel w R Fl c e F C->A.nlook u(A.c_funs C)=Some fx->
 (forall x,A.Fixed(BodySound.W w R Fl)u x->tn_get e x<>None)->
 (forall x raw now,A.Fixed(BodySound.W w R Fl)u x->
  (exists a stamp,get_app e(KyFun u)=Some(a,stamp)/\assoc Nat.eqb x a=Some(Some raw))->
  tn_get e x=Some now->canon e raw=canon e now)->
 forall x tx,In(x,tx)fx->exists here,
 A.nlook x(A.c_ty C)=Some here/\A.canon C here=A.canon C tx.
Proof.
 intros c e F C u fx Hc Fu Present Check x tx Hin.
 destruct(cr_funfixed _ _ _ _ _ _ _ Hc u fx x tx Fu Hin)as[Fix[a[stamp[raw[App[At ->]]]]]].
 destruct(tn_get e x)as[now|]eqn:Now;[|exfalso;exact(Present x Fix Now)].
 exists(tr Fl(canon e now)). split.
 - rewrite(cr_ty _ _ _ _ _ _ _ Hc),Now;reflexivity.
 - unfold A.canon. rewrite(cr_sub _ _ _ _ _ _ _ Hc).
   rewrite !tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
   rewrite canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
   symmetry;f_equal;apply(Check x raw now Fix);[exists a,stamp;auto|exact Now].
Qed.
End Recorded.

Definition fixed_guard (mi:nat)(e:env)(fg:gsym)(recorded:inst)(x:nat):M unit:=
 match assoc Nat.eqb x recorded,tn_get e x with
 |Some(Some was),Some now=>
  if ety_eqb(canon e was)(canon e now)then ret tt else
  let! xg:=sym x in let! a:=show(canon e now)in let! b:=show(canon e was)in
  err mi ("applying `"++gs_name fg++"` here reads `"++gs_name xg++"` as `"++a++
    "`, but `"++gs_name fg++"` is fixed in it and was provided where it was `"++b++
    "` (D84 rule 5, D72)")%string
 |_,_=>ret tt
 end.
Lemma fixed_guard_checked:forall mi e fg recorded xf s u s' x raw now,
 miter(fixed_guard mi e fg recorded)xf s=Good(u,s')->In x xf->
 assoc Nat.eqb x recorded=Some(Some raw)->tn_get e x=Some now->
 canon e raw=canon e now.
Proof.
 intros mi e fg recorded xf s u s' x raw now E Hin At Now.
 destruct(NeedExec.miter_checked _ _ _ _ _ _ E _ Hin)as[sx[ux[sx' Ex]]].
 unfold fixed_guard in Ex;rewrite At,Now in Ex.
 destruct(ety_eqb(canon e raw)(canon e now))eqn:Eq;[exact(ety_eqb_true _ _ Eq)|].
 apply bind_inv in Ex as(xg&sa&Eg&Ex).
 apply bind_inv in Ex as(a&sb&Ea&Ex).
 apply bind_inv in Ex as(b&sc&Eb&Ex).
 discriminate Ex.
Qed.

Lemma tn_get_key:forall e x y,tn_get e x=Some y->In x(map fst(en_tn e)).
Proof.
 intros e x y;unfold tn_get,nassoc. induction(en_tn e)as[|[k[t stamp]]ls IH];[discriminate|].
 cbn[assoc option_map fst snd]. destruct(Nat.eqb x k)eqn:E.
 - intros _;left;apply Nat.eqb_eq in E;auto.
 - intro H;right;exact(IH H).
Qed.
Definition input_guard(mi:nat)(e:env)(fg:gsym)(t:nat):M unit:=
 if is_some(tn_get e t)then ret tt else let! tgg:=sym t in
 err mi("applying `"++gs_name fg++"` needs `"++gs_name tgg++"`, which is not available in the context here")%string.
Lemma input_guard_checked:forall mi e fg ts s u s' t,
 miter(input_guard mi e fg)ts s=Good(u,s')->In t ts->tn_get e t<>None.
Proof.
 intros mi e fg ts s u s' t E Hin.
 destruct(NeedExec.miter_checked _ _ _ _ _ _ E _ Hin)as[sx[ux[sx' Ex]]].
 unfold input_guard in Ex. destruct(tn_get e t);[discriminate|].
 cbn[is_some]in Ex. apply bind_inv in Ex as(tgg&sa&Eg&Ex);discriminate Ex.
Qed.

Lemma ok_fixed_guard:forall mi e fg recorded x,ok(fixed_guard mi e fg recorded x).
Proof. intros;unfold fixed_guard;okt. Qed.
Lemma ok_input_guard:forall mi e fg t,ok(input_guard mi e fg t).
Proof. intros;unfold input_guard;okt. Qed.
Section Actual.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Let WW:=BodySound.W w R Fl.
Theorem actual_application_fixed:forall g n names args res s ef needs sf xf sx mi e c F C fx sy sz,
 A.WorldOf b WW->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 fun_env g s=Good((ef,needs),sf)->functor_fixed g sf=Good(xf,sx)->
 miter(fixed_guard mi e g(match get_app e(KyFun(gs_id g))with Some(a,_)=>a|None=>[]end))xf sx=Good(tt,sy)->
 miter(input_guard mi e g)(map fst(en_tn ef))sy=Good(tt,sz)->
 Inv s->st_w s=w->Later w P R Fl sz->CxRel w R Fl c e F C->
 A.nlook(gs_id g)(A.c_funs C)=Some fx->
 forall x tx,In(x,tx)fx->exists here,
 A.nlook x(A.c_ty C)=Some here/\A.canon C here=A.canon C tx.
Proof.
 intros g n names args res s ef needs sf xf sx mi e c F C fx sy sz Wo Hg D E Ex G I Hi Hw L Hc Fu.
 assert(Oe:ok(fun_env g))by(unfold fun_env;okt).
 destruct(Oe _ _ _ Hi E)as[If Ff].
 assert(Of:ok(functor_fixed g))by(unfold functor_fixed;okt).
 destruct(Of _ _ _ If Ex)as[Ix Fx].
 destruct(ok_miter _ _ _ (ok_fixed_guard mi e g _) _ _ _ Ix G)as[Iy Fy].
 destruct(ok_miter _ _ _ (ok_input_guard mi e g) _ _ _ Iy I)as[Iz Fz].
 assert(Lx:Later w P R Fl sx)by exact(later_back w P R Fl _ _ Fy(later_back w P R Fl _ _ Fz L)).
 assert(Lf:Later w P R Fl sf)by exact(later_back w P R Fl _ _ Fx Lx).
 destruct(fun_env_context w R Fl b Hload g n names args res s ef needs sf Wo Hg D E Hi Hw Lf)as(Cf&FC&Tables).
 pose proof(ResultMentionsExact.functor_fixed_exact w (P:=P) R Fl b Hload g n names args res sf xf sx
  Hg D Ex If ltac:(rewrite(fr_w _ _ Ff);exact Hw)Lx)as Fixed.
 apply(recorded_fixed_agreement w R Fl c e F C(gs_id g)fx Hc Fu).
 - intros x HF. assert(Present:A.nlook x(A.c_ty Cf)<>None).
   { eapply functx_abstract_fixed_present;[exact(BodyTy.w_decl_rd w R Fl _ _ Hg)|exact D|exact FC|exact HF]. }
   rewrite(fet_ty _ _ _ Tables)in Present.
   destruct(tn_get ef x)as[y|]eqn:Y;[|contradiction].
   eapply input_guard_checked;[exact I|exact(tn_get_key ef x y Y)].
 - intros x raw now HF[a[stamp[App At]]]Now.
   eapply fixed_guard_checked;[exact G|apply Fixed;exact HF| |exact Now].
   rewrite App;exact At.
Qed.
End Actual.

Section RecordedAgreement.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Let W:=BodySound.W w R Fl.
Theorem actual_recorded_fixed_agreement:forall g n names args res s xf sf mi e c F C ss,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 functor_fixed g s=Good(xf,sf)->
 miter(fixed_guard mi e g(match get_app e(KyFun(gs_id g))with Some(a,_)=>a|None=>[]end))xf sf=Good(tt,ss)->
 Inv s->st_w s=w->Later w P R Fl ss->CxRel w R Fl c e F C->
 A.FixedAgreement W C(gs_id g).
Proof.
 intros g n names args res s xf sf mi e c F C ss Hg D E G Hi Hw L Hc.
 assert(O:ok(functor_fixed g))by(unfold functor_fixed;okt).
 destruct(O _ _ _ Hi E)as[If Ff].
 destruct(ok_miter _ _ _ (ok_fixed_guard mi e g _) _ _ _ If G)as[_ Fg].
 pose proof(ResultMentionsExact.functor_fixed_exact w (P:=P) R Fl b Hload g n names args res s xf sf
  Hg D E Hi Hw(later_back w P R Fl _ _ Fg L))as Fixed.
 unfold A.FixedAgreement;intros x HF.
 rewrite(cr_funapps _ _ _ _ _ _ _ Hc),(cr_ty _ _ _ _ _ _ _ Hc).
 destruct(get_app e(KyFun(gs_id g)))as[[a stamp]|]eqn:App;cbn[option_map fst];[|exact I].
 destruct(tn_get e x)as[now|]eqn:Now;cbn[option_map];[|exact I].
 change(match A.nlook x(fun_tr_inst Fl a)with
 |Some(Some was)=>A.canon C was=A.canon C(tr Fl(canon e now))|_=>True end).
 assert(Look:forall (aa:Elab.inst) k,A.nlook k(fun_tr_inst Fl aa)=option_map(option_map(tr Fl))(nassoc k aa)).
 { intros aa;induction aa as[|[k v]aa IH];intro j;[reflexivity|].
   cbn[A.nlook fun_tr_inst nassoc assoc map fst snd];destruct(Nat.eqb j k);[reflexivity|apply IH]. }
 rewrite Look.
 destruct(nassoc x a)as[[raw|]|]eqn:At;cbn[option_map];try exact I.
 unfold A.canon;rewrite(cr_sub _ _ _ _ _ _ _ Hc).
 rewrite !tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
 rewrite canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)).
 f_equal;eapply fixed_guard_checked;[exact G|apply Fixed;exact HF|exact At|exact Now].
Qed.
End RecordedAgreement.
