(* Support is carried through every actual context-rebuild round. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore
 BodyTy Merge CanonTypes SourceClean ContextClean NominalSupport ContextSupport.
Import ListNotations.
Lemma clean_tne_nil : forall e, ContextClean e ->
 filter(fun p=>negb(ety_eqb(snd p)(YAbs(fst p))))(tymap e)=[].
Proof.
 intros e C. pose proof(cc_types _ C)as T. unfold SourceClean.EnvClean in T. unfold tymap.
 induction(en_tn e)as[|[k p]r IH];[reflexivity|]. cbn[map filter fst snd].
 rewrite(T k p(or_introl eq_refl)),ety_eqb_refl. cbn[negb].
 apply IH. intros k' p' H. exact(T k' p'(or_intror H)).
Qed.
Definition support_go (walk:env->M(env*list key))(mi:nat) :=
 fix go(n:nat)(tne ufe:list(nat*ety)):M(env*list key):=
 let! (e,needs):=walk(env_with_edges tne ufe)in
 let tne':=filter(fun p=>negb(ety_eqb(snd p)(YAbs(fst p))))(tymap e)in
 let ufe':=en_uf e in
 if edges_eqb(tne'++ufe')(tne++ufe)then ret(e,needs)else
 match n with 0=>err mi "merging the context does not settle in 8 rounds (D43)"|S n'=>go n' tne' ufe' end.
Section Rounds.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(walk:env->M(env*list key)).
Notation WW:=(BodySoundCore.W w R Fl).
Hypothesis Hok:forall e,ok(walk e).
Hypothesis Hround:forall U s e needs s',
 Tri U->UfOK U->UfSupport WW U->Inv s->st_w s=w->Later w P R Fl s'->
 walk(env_with_edges [] U)s=Good((e,needs),s')->
 ContextClean e /\ Tri(en_uf e) /\ EnvSupport WW e.
Lemma support_go_ok:forall mi n tne ufe,ok(support_go walk mi n tne ufe).
Proof.
 intros mi n. induction n as[|n IH];intros tne ufe;cbn[support_go];
 apply ok_bind;[apply Hok| |apply Hok|];intros[e needs];cbn beta iota zeta;
 destruct(edges_eqb _ _);try apply ok_ret;[unfold err;okt|apply IH].
Qed.
Lemma rounds_support:forall mi n U s e needs s',
 support_go walk mi n [] U s=Good((e,needs),s')->
 Tri U->UfOK U->UfSupport WW U->Inv s->st_w s=w->Later w P R Fl s'->
 ContextClean e /\ Tri(en_uf e) /\ EnvSupport WW e.
Proof.
 intros mi n. induction n as[|n IH];intros U s e needs s' H T O S Hi Hw L;
 cbn[support_go]in H;apply bind_inv in H as([e1 n1]&s1&H1&H2);
 destruct(Hok _ _ _ _ Hi H1)as[Hi1 F1];cbn beta iota zeta in H2.
 - destruct(edges_eqb _ _)eqn:E;[|exfalso;exact(err_not_good _ _ _ _ _ _ H2)].
   injection H2 as -> -> ->. exact(Hround U s e needs s' T O S Hi Hw L H1).
 - destruct(edges_eqb _ _)eqn:E.
   + injection H2 as -> -> ->. exact(Hround U s e needs s' T O S Hi Hw L H1).
   + destruct(support_go_ok mi n _ _ _ _ _ Hi1 H2)as[_ F2].
     destruct(Hround U s e1 n1 s1 T O S Hi Hw(later_back w P R Fl _ _ F2 L)H1)as[C1[T1 S1]].
     rewrite(clean_tne_nil e1 C1)in H2.
     exact(IH(en_uf e1)s1 e needs s' H2 T1(cc_uf _ C1)(@es_uf WW e1 S1)Hi1(fr_w' w _ _ F1 Hw)L).
Qed.
Theorem merged_walk_support:forall mi s e needs s',
 merged_walk mi walk s=Good((e,needs),s')->Inv s->st_w s=w->Later w P R Fl s'->
 ContextClean e /\ Tri(en_uf e) /\ EnvSupport WW e.
Proof.
 intros mi s e needs s' H Hi Hw L.
 exact(rounds_support mi 8 [] s e needs s' H TriNil(fun g t H=>match H with end)(fun g t H=>match H with end)Hi Hw L).
Qed.
End Rounds.

Lemma support_edges_start:forall W U,
 UfOK U->Tri U->UfSupport W U->
 ContextClean(env_with_edges [] U) /\ Tri(en_uf(env_with_edges [] U)) /\ EnvSupport W(env_with_edges [] U).
Proof.
 intros W U O T S. split.
 - constructor;cbn;auto;try solve[intros;discriminate]. intros k p [].
 - split;[exact T|]. constructor;cbn;auto;try solve[intros;discriminate]. intros k p [].
Qed.
Lemma freeze_all_apps_support:forall W e,
 EnvSupport W e->EnvSupport W(freeze_all_apps e).
Proof. intros W e[T U V F M L]. constructor;try assumption. intros n p Bad;discriminate. Qed.
Lemma freeze_all_apps_valid:forall W e,
 ContextClean e->Tri(en_uf e)->EnvSupport W e->
 ContextClean(freeze_all_apps e) /\ Tri(en_uf(freeze_all_apps e)) /\ EnvSupport W(freeze_all_apps e).
Proof.
 intros W e C T S. split.
 - destruct C as[A B C D E]. constructor;assumption.
 - split;[exact T|apply freeze_all_apps_support;exact S].
Qed.
