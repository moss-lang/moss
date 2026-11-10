(* Method-table extension, including duplicate-signature unification. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTy
 Merge CanonTypes SourceClean ContextClean CtxWalk CtxMethod MethodRows.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Table.
Variable Fl:string->nat.
Lemma ws_mth:forall U e e0 e1 e2 tm les needs r m sg a mi0 nm0 s1 s1' s0 s0',
 WS Fl U e tm les needs->sg_this sg=Some r->ety_ok r=true->
 ((get_mth e(canon e r)m=None/\e0=set_mth e(canon e r)m sg)\/
  exists old mi nm s s',get_mth e(canon e r)m=Some old/\unify_sig e mi old sg nm s=Good(e0,s'))->
 unify_app e0 mi0(KyMth(canon e r)m)a nm0 s1=Good(e1,s1')->
 setdefault_app e1(KyMth(canon e r)m)a s0=Good(e2,s0')->ContextClean e2->
 WS Fl U e2 tm(les++[LMth r m sg a])(add_need needs(KyMth(canon e r)m)).
Proof.
 intros U e e0 e1 e2 tm les needs r m sg a mi0 nm0 s1 s1' s0 s0' W Hthis Or C0 Hua Hsd Clean.
 set(R:=canon e r)in *.
 destruct(setdefault_env _ _ _ _ _ _ Hsd)as(D1&D2&D3&D4&D5&D6).
 assert(Cnorm:(get_mth e R m=None/\e0=set_mth e R m sg)\/
  exists old,get_mth e R m=Some old/\UStep e e0/\
   List.length(sg_params old)=List.length(sg_params sg)/\
   (forall sb,RespE Fl sb(en_uf e)->SigResp Fl sb old sg->RespE Fl sb(en_uf e0))/\
   (en_uf e0=en_uf e->Forall(fun p=>canon e(fst p)=canon e(snd p))
     (combine(map snd(sg_params old))(map snd(sg_params sg)))/\canon e(sg_ret old)=canon e(sg_ret sg))).
 { destruct C0 as[C|(old&mi&nm&s&s'&O&H)];[left;exact C|].
   destruct(unify_sig_spec Fl e mi old sg nm s e0 s'(ws_tri _ _ _ _ _ _ W)H)as(_&Us&Len&Re&Ce).
   right;exists old;auto. }
 clear C0.
 assert(Ea0:en_apps e0=en_apps e/\Tri(en_uf e0)/\exists n,en_uf e0=en_uf e++n).
 { destruct Cnorm as[[N ->]|(old&O&U1&_)].
   - split;[reflexivity|]. split;[exact(ws_tri _ _ _ _ _ _ W)|]. exists[]. rewrite app_nil_r. reflexivity.
   - split;[rewrite(us_same _ _ U1);reflexivity|]. split;[exact(us_tri _ _ U1)|exact(us_ext _ _ U1)]. }
 destruct Ea0 as(Ea0&Tr0&Ex0).
 destruct(compose_app Fl e e0 e1(KyMth R m)a mi0 nm0 s1 s1' Tr0 Ex0 Ea0 Hua)as(Uu&Nn&Ru&Cu).
 assert(C:(get_mth e R m=None/\e1=set_mth e R m sg)\/
  exists old,get_mth e R m=Some old/\UStep e e1/\
   List.length(sg_params old)=List.length(sg_params sg)/\
   (forall sb,RespE Fl sb(en_uf e)->SigResp Fl sb old sg->
      (forall a0 st,get_app e(KyMth R m)=Some(a0,st)->InstResp Fl sb a0 a)->RespE Fl sb(en_uf e1))/\
   (en_uf e1=en_uf e->Forall(fun p=>canon e(fst p)=canon e(snd p))
     (combine(map snd(sg_params old))(map snd(sg_params sg)))/\canon e(sg_ret old)=canon e(sg_ret sg))).
 { destruct Cnorm as[[N ->]|(old&O&U1&L1&R1&C1)].
   - left. split;[exact N|]. apply Nn.
     destruct(get_app e(KyMth R m))as[[a0 st]|]eqn:G;[|reflexivity]. exfalso.
     destruct(appin_of_get Fl e _ a0 st les(ws_apps_in _ _ _ _ _ _ W)G)as(_&Gm). exact(Gm N).
   - right. exists old. split;[exact O|]. split;[exact(ustep_trans _ _ _ U1 Uu)|]. split;[exact L1|]. split.
     + intros sb Hr E Hi. apply Ru;[exact(R1 sb Hr E)|exact Hi].
     + intro E. exact(C1(Cu E)). }
 assert(Data:en_tn e1=en_tn e/\en_vals e1=en_vals e/\en_fns e1=en_fns e/\
  en_funs e1=en_funs e/\en_apps e1=en_apps e/\(exists nw,en_uf e1=en_uf e++nw)/\Tri(en_uf e1)).
 { destruct C as[[N ->]|(old&O&Us&_)].
   - repeat split;try reflexivity.
     + exists[]. rewrite app_nil_r;reflexivity.
     + exact(ws_tri _ _ _ _ _ _ W).
   - rewrite(us_same _ _ Us). cbn[with_uf en_tn en_vals en_fns en_funs en_apps en_uf].
     repeat split;try reflexivity;[exact(us_ext _ _ Us)|exact(us_tri _ _ Us)]. }
 destruct Data as(Tn&Tv&Tf&Tu&Ta&Ex1&T1).
 assert(Ex2:exists nw,en_uf e2=en_uf e++nw)by(rewrite D2;exact Ex1).
 assert(Hu:en_uf e2=U->en_uf e=U/\en_uf e2=en_uf e).
 { intro E. exact(uf_squeeze U e e2(ws_ext _ _ _ _ _ _ W)Ex2 E). }
 assert(Back:forall sb,RespE Fl sb(en_uf e2)->RespE Fl sb(en_uf e)).
 { destruct Ex2 as(nw&E). intros sb H. rewrite E in H. exact(resp_app Fl _ _ _ H). }
 assert(Q1:forall rq mq,get_mth e1 rq mq=
  if Nat.eqb mq m&&ety_eqb rq R then Some(match get_mth e R m with Some old=>old|None=>sg end)else get_mth e rq mq).
 { intros rq mq. destruct C as[[N ->]|(old&O&Us&_)].
   - rewrite N. apply get_mth_set_case.
   - rewrite(us_same _ _ Us). change(get_mth e rq mq=if Nat.eqb mq m&&ety_eqb rq R then Some(match get_mth e R m with Some old=>old|None=>sg end)else get_mth e rq mq). rewrite O.
     destruct(Nat.eqb mq m&&ety_eqb rq R)eqn:B;[|reflexivity].
     apply andb_true_iff in B as[Bm Br]. apply Nat.eqb_eq in Bm. apply ety_eqb_true in Br. subst mq rq. exact O. }
 assert(Q:forall rq mq,get_mth e2 rq mq=
  if Nat.eqb mq m&&ety_eqb rq R then Some(match get_mth e R m with Some old=>old|None=>sg end)else get_mth e rq mq).
 { intros rq mq. unfold get_mth at 1. rewrite D5. apply Q1. }
 assert(Preserve:forall rq mq g0,get_mth e rq mq=Some g0->get_mth e2 rq mq=Some g0).
 { intros rq mq g0 G. rewrite Q. destruct(Nat.eqb mq m&&ety_eqb rq R)eqn:B;[|exact G].
   apply andb_true_iff in B as[Bm Br]. apply Nat.eqb_eq in Bm. apply ety_eqb_true in Br. subst mq rq. rewrite G. reflexivity. }
 assert(At:get_mth e2 R m=Some(match get_mth e R m with Some old=>old|None=>sg end)).
 { rewrite Q,Nat.eqb_refl,ety_eqb_refl;reflexivity. }
 eapply(@ws_method_common Fl U e e2 tm les needs r m sg a);try exact W;try exact Clean;try exact Hthis.
 - rewrite D1;exact Tn.
 - rewrite D3;exact Tv.
 - rewrite D4;exact Tf.
 - rewrite D6;exact Tu.
 - exact Ex2.
 - rewrite D2;exact T1.
 - intros sb Hr Hun. rewrite D2. rewrite map_app in Hun.
   assert(R0:RespE Fl sb(en_uf e)).
   { apply(ws_mg _ _ _ _ _ _ W sb Hr). exact(unifies_app _ _ _ Hun). }
   assert(Hi:forall a0 st,get_app e(KyMth R m)=Some(a0,st)->InstResp Fl sb a0 a).
   { intros a0 st G. destruct(appin_of_get Fl e _ a0 st les(ws_apps_in _ _ _ _ _ _ W)G)as((r0&g0&Hin&Eq)&_).
     assert(Er:asub sb(tr Fl r0)=asub sb(tr Fl r)).
     { rewrite(Eq sb R0). unfold R,canon. apply(resp_canon_go Fl);exact R0. }
     assert(Ei:Some(A.inst_read sb(tr_inst Fl a0))=Some(A.inst_read sb(tr_inst Fl a))).
     { refine(proj2(proj1 Hun(tr_ent Fl(LMth r0 m g0 a0))(tr_ent Fl(LMth r m sg a))(A.mkey(asub sb(tr Fl r))m)_ _ _ _)).
       - apply in_or_app;left;apply in_map;exact Hin.
       - apply in_or_app;right;left;reflexivity.
       - cbn[tr_ent A.entry_key]. rewrite Er;reflexivity.
       - reflexivity. }
     injection Ei as Ei. exact(instresp_of_read Fl sb a0 a Ei). }
   destruct C as[[N ->]|(old&O&Us&Len&Re&Ce)];[exact R0|]. apply(Re sb R0);[|exact Hi].
   destruct(ws_mths_in _ _ _ _ _ _ W R m old O)as(r0&a0&Ho&Equiv).
   pose proof(ws_mthis _ _ _ _ _ _ W r0 m old a0 Ho)as Hto.
   assert(Er:asub sb(tr Fl r0)=asub sb(tr Fl r)).
   { rewrite(Equiv sb R0). unfold R,canon. apply(resp_canon_go Fl);exact R0. }
   assert(Sg:Some(A.sig_subst sb(tr_sig Fl old))=Some(A.sig_subst sb(tr_sig Fl sg))).
   { refine(proj1(proj1 Hun(tr_ent Fl(LMth r0 m old a0))(tr_ent Fl(LMth r m sg a))(A.mkey(asub sb(tr Fl r))m)_ _ _ _)).
     - apply in_or_app;left;apply in_map;exact Ho.
     - apply in_or_app;right;left;reflexivity.
     - cbn[tr_ent A.entry_key]. rewrite Er;reflexivity.
     - reflexivity. }
   injection Sg as Sp Sr. unfold SigResp. rewrite Hto,Hthis. split;[|split;[exact Sr|exact Er]].
   rewrite !map_map in Sp. rewrite <-(map_map snd(fun y=>asub sb(tr Fl y)))in Sp.
   rewrite <-(map_map snd(fun y=>asub sb(tr Fl y))(sg_params sg))in Sp.
   exact(Merge.map_combine(fun y=>asub sb(tr Fl y))_ _ Sp).
 - intros E rq mq gq aq Hin. destruct(Hu E)as[E0 E01].
   apply in_app_iff in Hin as[Hin|[Eq|[]]].
   + destruct(ws_mths _ _ _ _ _ _ W E0 rq mq gq aq Hin)as[Ok(g0&G&Sg)].
     split;[rewrite(canon_same e e2 rq E01);exact Ok|]. exists g0. split.
     * rewrite(canon_same e e2 rq E01). apply Preserve;exact G.
     * exact(sigcanon_same e e2 gq g0 E01 Sg).
   + injection Eq as <- <- <- <-. split;[apply canon_ok;[exact(cc_uf _ Clean)|exact Or]|].
     rewrite(canon_same e e2 r E01). fold R.
     destruct C as[[N ->]|(old&O&Us&Len&Re&Ce)].
     * exists sg. split;[rewrite N in At;exact At|split;reflexivity].
     * exists old. split;[rewrite O in At;exact At|].
       destruct(Ce(eq_trans(eq_sym D2)E01))as[Pc Rc]. apply(sigcanon_same e e2 sg old E01).
       split;[|symmetry;exact Rc]. symmetry. rewrite <-!(map_map snd(canon e)).
       apply combine_map_eq;[rewrite !length_map;exact Len|exact Pc].
 - assert(Old:forall rq mq g0,get_mth e rq mq=Some g0->exists r0 a0,
    In(LMth r0 mq g0 a0)(les++[LMth r m sg a])/\
    forall sb,RespE Fl sb(en_uf e2)->asub sb(tr Fl r0)=asub sb(tr Fl rq)).
   { intros rq mq g0 G. destruct(ws_mths_in _ _ _ _ _ _ W _ _ _ G)as(r0&a0&Hin&Eq).
     exists r0,a0. split;[apply in_or_app;left;exact Hin|intros sb Hr;exact(Eq sb(Back sb Hr))]. }
   intros rq mq g0 G. rewrite Q in G. destruct(Nat.eqb mq m&&ety_eqb rq R)eqn:B.
   + apply andb_true_iff in B as[Bm Br]. apply Nat.eqb_eq in Bm. apply ety_eqb_true in Br. subst mq rq.
     destruct(get_mth e R m)as[old|]eqn:O.
     * injection G as <-. exact(Old R m old O).
     * injection G as <-. exists r,a. split;[apply in_or_app;right;left;reflexivity|].
       intros sb Hr. symmetry. unfold R,canon. apply(resp_canon_go Fl);exact(Back sb Hr).
   + exact(Old rq mq g0 G).
 - exact(apps_step U e e1 e2 les(LMth r m sg a)(KyMth R m)a s0 s0'
    (ws_apps _ _ _ _ _ _ W)Ta(ws_ext _ _ _ _ _ _ W)Ex1 eq_refl Hsd).
 - intros mid rows AtRows. rewrite D5 in AtRows.
   destruct C as[[N ->]|(old&O&Us&_)].
   + unfold set_mth in AtRows;cbn[en_mths]in AtRows;rewrite nget_nset in AtRows.
     destruct(Nat.eqb mid m).
     * injection AtRows as <-;apply method_aset_keys_unique.
       destruct(nget(en_mths e)m)as[rs|]eqn:Get;[exact(ws_method_unique _ _ _ _ _ _ W _ _ Get)|constructor].
     * exact(ws_method_unique _ _ _ _ _ _ W _ _ AtRows).
   + rewrite(us_same _ _ Us)in AtRows. exact(ws_method_unique _ _ _ _ _ _ W _ _ AtRows).
 - intro E. destruct(Hu E)as[E0 E01].
   assert(C1:en_uf e1=en_uf e)by(congruence).
   assert(Rgood:ety_ok R=true)by(unfold R;apply canon_ok;[exact(cc_uf _ (ws_clean _ _ _ _ _ _ W))|exact Or]).
   assert(Rcanon:canon e R=R)by(unfold R;apply canon_idem;exact(ws_tri _ _ _ _ _ _ W)).
   intros mid rows AtRows. rewrite D5 in AtRows.
   destruct C as[[N ->]|(old&O&Us&_)].
   + pose proof(proj2(method_rows_set e R m sg (ws_method_unique _ _ _ _ _ _ W)
       (ws_method_canonical _ _ _ _ _ _ W E0) Rgood Rcanon) _ _ AtRows)as Rows.
     unfold canon in *;rewrite E01;exact Rows.
   + rewrite(us_same _ _ Us)in AtRows.
     pose proof(ws_method_canonical _ _ _ _ _ _ W E0 _ _ AtRows)as Rows.
     unfold canon in *;rewrite E01;exact Rows.
 - intros E k a' Hk Hi'. destruct(Hu E)as[E0 E01]. cbn[lkey linst]in Hk,Hi'.
   rewrite(canon_same e e2 r E01)in Hk. injection Hk as <-. injection Hi' as <-.
   apply(insts_new Fl U e e1 e2 les(LMth r m sg a)(KyMth R m)a mi0 nm0 s1 s1' s0 s0'(ws_apps _ _ _ _ _ _ W)e0 Ea0 Ex0
     (ws_ext _ _ _ _ _ _ W)Hua Tr0 Hsd);[|exact E].
   cbn[lapp]. rewrite(canon_same e e2 r E01). reflexivity.
 - assert(Mm:forall rq mq,get_mth e rq mq<>None->get_mth e1 rq mq<>None).
   { intros rq mq H. rewrite Q1. destruct(Nat.eqb mq m&&ety_eqb rq R);[discriminate|exact H]. }
   assert(Mk:forall rq mq,KyMth R m=KyMth rq mq->get_mth e1 rq mq<>None).
   { intros rq mq Eq. injection Eq as <- <-. rewrite Q1,Nat.eqb_refl,ety_eqb_refl. discriminate. }
   exact(appin_step Fl e e1 e2 les(LMth r m sg a)(KyMth R m)a s0 s0'(ws_apps_in _ _ _ _ _ _ W)Ta Ex1 Mm eq_refl Mk Hsd).
Qed.
End Table.
