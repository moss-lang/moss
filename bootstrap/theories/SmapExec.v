(* Runtime satisfaction keys are read from the two actual contexts. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore BodySoundCore BodyTr Merge CtxMerge MethodKeyTr.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Keys.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat).
Definition key_apps(e:env)(C:A.cx)(k:key):Prop:=match k with
 |KyMth r m=>A.ilook(A.mkey(tr Fl r)m)(A.c_apps C)=
 option_map(fun p=>map(fun q=>(fst q,option_map(tr Fl)(snd q)))(fst p))(get_app e k)
 |_=>True end.
Lemma key_item_at_context : forall e C k s it s',
 key_item_at e k s=Good(it,s')->Inv s->Later w P R Fl s'->
 A.c_sub C=suf Fl(en_uf e)->Tri(en_uf e)->key_apps e C k->
 it=A.rkey_of C(key_tr Fl k).
Proof.
 intros e C k s it s' E Hi L Sub T App. destruct k;try(injection E as <- _;reflexivity).
 exact(@key_item_at_tr w P R Fl e C e0 n s it s' E Hi L Sub T App).
Qed.
Lemma smap_at_context : forall ei eo Cs C ps s sm s',
 smap_at ei eo ps s=Good(sm,s')->Inv s->Later w P R Fl s'->
 A.c_sub Cs=suf Fl(en_uf ei)->Tri(en_uf ei)->
 A.c_sub C=suf Fl(en_uf eo)->Tri(en_uf eo)->
 Forall(fun p=>key_apps ei Cs(fst p)/\key_apps eo C(snd p))ps->
 sm=map(fun p=>(A.rkey_of Cs(key_tr Fl(fst p)),A.rkey_of C(key_tr Fl(snd p))))ps.
Proof.
 intros ei eo Cs C ps;induction ps as[|[a b]ps IH];intros s sm s' E Hi L Si Ti So To Apps.
 - injection E as <- _;reflexivity.
 - unfold smap_at in E. cbn[mmap]in E.
   apply bind_inv in E as[ab[sab[Eab E]]].
   apply bind_inv in E as[rest[sr[Er E]]]. injection E as <- <-.
   inversion Apps as[|p ps0 Hab Hrest];subst. cbn[fst snd]in Hab.
   apply bind_inv in Eab as[ka[sa[Ea Eab]]].
   apply bind_inv in Eab as[kb[sb[Eb Eab]]]. injection Eab as <- <-.
   destruct(@ok_key_item_at_frame ei a s ka sa Hi Ea)as[Ia Fa].
   destruct(@ok_key_item_at_frame eo b sa kb sb Ia Eb)as[Ib Fb].
   assert(Or:ok(smap_at ei eo ps)).
   { unfold smap_at. apply ok_mmap. intros[x y]. apply ok_bind;[apply ok_key_item_at_frame|intro].
     apply ok_bind;[apply ok_key_item_at_frame|intro;apply ok_ret]. }
   destruct(Or _ _ _ Ib Er)as[Ir Fr].
   rewrite(key_item_at_context _ _ _ _ _ _ Ea Hi(later_back w P R Fl _ _ (fr_trans _ _ _ Fb Fr)L)Si Ti(proj1 Hab)).
   rewrite(key_item_at_context _ _ _ _ _ _ Eb Ia(later_back w P R Fl _ _ Fr L)So To(proj2 Hab)).
   cbn[map fst snd]. f_equal. exact(IH _ _ _ Er Ib L Si Ti So To Hrest).
Qed.
End Keys.
