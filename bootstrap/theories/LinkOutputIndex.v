(* The emitted result telescope reads the actual completed output context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame BodySoundCore BodyTr BodyTy
 CtxMerge EtyTyExec MethodKeyTr LambdaKeyTranslation LambdaSmapIdentity LinkBodyTrace LinkOutputRefile.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL nat_to_string nat_to_string_go canon instantiate ety_ty key_item_at tr].
Module A:=ExecutionModel.Elaboration.
Section Index.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Lemma link_output_index_context:forall keys roles c e F C s indexed final,
 mmap(fun k=>let!it:=key_item_at e k in ret(output_role_item roles it))keys s=Good(indexed,final)->
 Inv s->Later w P R Fl final->CxRel w R Fl c e F C->Forall LambdaKeyValid keys->
 indexed=map(fun k=>output_role_item roles(A.rkey_of C(key_tr Fl k)))keys.
Proof using w P R Fl.
 intros keys;induction keys as[|k keys IH];intros roles c e F C s indexed final Run Hi L HC Valid.
 - injection Run as <- _;reflexivity.
 - inversion Valid as[|key rest Gkey Grest];subst.
   cbn[mmap]in Run;apply bind_inv in Run as[it[step[Head Run]]].
   apply bind_inv in Run as[tail[last[Tail Run]]];injection Run as <- <-.
   apply bind_inv in Head as[raw[read[Read Ret]]];injection Ret as <- <-.
   destruct(@ok_key_item_at_frame e k s raw read Hi Read)as[Ir Fr].
   assert(Otail:ok(mmap(fun key=>let!value:=key_item_at e key in ret(output_role_item roles value))keys)).
   {apply ok_mmap;intro key;apply ok_bind;[apply ok_key_item_at_frame|intro;apply ok_ret]. }
   destruct(Otail _ _ _ Ir Tail)as[It Ft].
   pose proof(lambda_key_item_at w P R Fl c e F C k s raw read Read Hi(later_back w P R Fl _ _ Ft L)HC Gkey)as Eq.
   cbn[map];rewrite Eq;f_equal;exact(IH roles c e F C read tail last Tail Ir L HC Grest).
Qed.
Definition link_output_context_pair(roles:list(nat*nat))(eb:env)(tys:list(nat*ety))(C:A.cx)(k:key):item*item:=
 (match k with
  |KyMth r m=>IMth(tr Fl(canon eb(instantiate(env_with_edges tys[])r)))m[]
  |_=>key_tr Fl k end,
  output_role_item roles(A.rkey_of C(key_tr Fl k))).
Definition link_output_rename_step(e eb:env)(roles:list(nat*nat))(tys:list(nat*ety))(k:key):M(list(ty*nat*(ty*subst))):=
 match k,link_trace_body_key eb tys k with
 |KyMth r m,KyMth r' _=>let!receiver:=ety_ty r' in
 let!checked:=key_item_at e k in
 match output_role_item roles checked with IMth t0 _ th0=>ret[(receiver,m,(t0,th0))]|_=>ret[]end
 |_,_=>ret[]end.
Lemma link_output_rename_step_ok:forall e eb roles tys k,ok(link_output_rename_step e eb roles tys k).
Proof.
 intros;unfold link_output_rename_step;destruct k;cbn[link_trace_body_key];try apply ok_ret.
 apply ok_bind;[apply ok_ety_ty|intro];apply ok_bind;[apply ok_key_item_at_frame|intro it].
 destruct(output_role_item roles it);apply ok_ret.
Qed.
Lemma link_output_rename_step_context:forall k roles eb tys c e F C s aliases final,
 link_output_rename_step e eb roles tys k s=Good(aliases,final)->
 Inv s->Later w P R Fl final->CxRel w R Fl c e F C->LambdaKeyValid k->
 aliases=link_output_rename_pairs[link_output_context_pair roles eb tys C k].
Proof using w P R Fl.
 intros k roles eb tys c e F C s aliases final Run Hi Last HC Valid.
 destruct k as[v|f|r mid|u];cbn[link_output_rename_step link_trace_body_key]in Run.
 all:try(injection Run as <- <-;reflexivity).
 apply bind_inv in Run as[receiver[converted[Convert Run]]].
 apply bind_inv in Run as[checked[indexed[Index Run]]].
 destruct(ok_ety_ty _ _ _ _ Hi Convert)as[Ic Fc].
 destruct(@ok_key_item_at_frame e(KyMth r mid)converted checked indexed Ic Index)as[Ii Fi].
 assert(Li:Later w P R Fl indexed).
 {destruct(output_role_item roles checked);injection Run as <- <-;exact Last. }
 pose proof(lambda_key_item_at w P R Fl c e F C(KyMth r mid)converted checked indexed Index Ic Li HC Valid)as Eq.
 rewrite Eq in Run;cbn[key_tr A.rkey_of A.rkey output_role_item ExecutionModel.app_subst_item]in Run.
 injection Run as <- <-.
 rewrite(ety_ty_tr w P R Fl _ _ _ _ Convert Hi(later_back w P R Fl _ _ Fi Li)).
 cbn[link_output_rename_pairs link_output_context_pair flat_map key_tr A.rkey_of A.rkey output_role_item ExecutionModel.app_subst_item];reflexivity.
Qed.
Lemma link_output_renames_context:forall keys roles eb tys c e F C s renamed final,
 link_trace_renames e eb roles tys keys s=Good(renamed,final)->
 Inv s->st_w s=w->Later w P R Fl final->CxRel w R Fl c e F C->Forall LambdaKeyValid keys->
 List.concat renamed=link_output_rename_pairs(map(link_output_context_pair roles eb tys C)keys).
Proof using w P R Fl.
 intros keys roles eb tys c e F C s renamed final Run Hi Hw Last HC Valid.
 change(mmap(link_output_rename_step e eb roles tys)keys s=Good(renamed,final))in Run.
 assert(Rel:Forall2(fun k aliases=>aliases=link_output_rename_pairs[link_output_context_pair roles eb tys C k])keys renamed).
 {eapply(BodyTy.mmap_ok_rel w (P:=P) R Fl);[apply link_output_rename_step_ok| |exact Run|exact Hi|exact Hw|exact Last].
  intros k Hit si aliases sf Step Is Ws Ls.
  eapply link_output_rename_step_context;try eassumption.
  rewrite Forall_forall in Valid;exact(Valid k Hit). }
 clear Run Hi Hw Last HC Valid.
 induction Rel as[|k aliases keys rest Eq Rel IH];[reflexivity|].
 subst aliases;cbn[List.concat map];rewrite IH.
 unfold link_output_rename_pairs;rewrite <-flat_map_app;reflexivity.
Qed.
End Index.
