(* The lambda's identity need list reads the actual recorded instantiations
   on both sides; the type arguments are not part of its provision key set. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySound CtxMerge
 MethodKeyTr LambdaKeyTranslation.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma ok_lambda_smap:forall ei eo pairs,ok(smap_at ei eo pairs).
Proof.
 intros;unfold smap_at;apply ok_mmap;intros[a b].
 apply ok_bind;[apply ok_key_item_at_frame|intro];apply ok_bind;[apply ok_key_item_at_frame|intro;apply ok_ret].
Qed.
Section Keys.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Lemma lambda_key_item_at:forall c e F C key s it sf,
 key_item_at e key s=Good(it,sf)->Inv s->Later w P R Fl sf->
 CxRel w R Fl c e F C->LambdaKeyValid key->it=A.rkey_of C(key_tr Fl key).
Proof.
 intros c e F C key s it sf Run Hi L HC Valid;destruct key as[v|f|r mid|u].
 - injection Run as <- <-;reflexivity.
 - injection Run as <- <-;reflexivity.
 - eapply(@key_item_at_tr w P R Fl);[exact Run|exact Hi|exact L|exact(cr_sub _ _ _ _ _ _ _ HC)|exact(proj1(cr_uf _ _ _ _ _ _ _ HC))|].
   exact(cr_apps _ _ _ _ _ _ _ HC r mid Valid).
 - injection Run as <- <-;reflexivity.
Qed.
Theorem lambda_smap_identity:forall keys ci ei Fi Ci co eo Fo Co s sm sf,
 smap_at ei eo(map(fun k=>(k,k))keys)s=Good(sm,sf)->Inv s->Later w P R Fl sf->
 CxRel w R Fl ci ei Fi Ci->CxRel w R Fl co eo Fo Co->Forall LambdaKeyValid keys->
 sm=map(fun k=>(A.rkey_of Ci k,A.rkey_of Co k))(map(key_tr Fl)keys).
Proof.
 intro keys;induction keys as[|key keys IH];intros ci ei Fi Ci co eo Fo Co s sm sf Run Hi L HCi HCo Valid.
 - injection Run as <- <-;reflexivity.
 - inversion Valid as[|k ks Gkey Rest];subst.
   unfold smap_at in Run;cbn[map mmap]in Run.
   apply bind_inv in Run as(pair&sp&Head&Run).
   apply bind_inv in Run as(tail&last&Tail&Run);injection Run as <- <-.
   change(smap_at ei eo(map(fun k=>(k,k))keys)sp=Good(tail,last))in Tail.
   assert(Oh:ok(let!a:=key_item_at ei key in let!b:=key_item_at eo key in ret(a,b))).
   {apply ok_bind;[apply ok_key_item_at_frame|intro];apply ok_bind;[apply ok_key_item_at_frame|intro;apply ok_ret]. }
   destruct(Oh _ _ _ Hi Head)as[Ip Fp].
   destruct(ok_lambda_smap ei eo _ _ _ _ Ip Tail)as[_ Ft].
   pose proof(later_back w P R Fl _ _ Ft L)as Lp.
   apply bind_inv in Head as(a&sa&Left&Head).
   apply bind_inv in Head as(b&sb&Right&Head);injection Head as <- <-.
   destruct(@ok_key_item_at_frame ei key s a sa Hi Left)as[Ia Fa].
   destruct(@ok_key_item_at_frame eo key sa b sb Ia Right)as[_ Fb].
   pose proof(lambda_key_item_at ci ei Fi Ci key s a sa Left Hi(later_back w P R Fl _ _ Fb Lp)HCi Gkey)as Ea.
   pose proof(lambda_key_item_at co eo Fo Co key sa b sb Right Ia Lp HCo Gkey)as Eb.
   pose proof(IH ci ei Fi Ci co eo Fo Co sb tail last Tail Ip L HCi HCo Rest)as Et.
   cbn[map];rewrite Ea,Eb,Et;reflexivity.
Qed.
End Keys.
