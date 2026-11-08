(* The executable result/fixed visitors collect each key once. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ResultMentionExec WorldSound.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements free_type_syms result_mentions_go functor_fixed_go].
Lemma mentions_add_nodup:forall xs out,NoDup out->NoDup(mentions_add xs out).
Proof.
 intros xs;induction xs as[|x rest IH];intros out Nd;[exact Nd|].
 cbn[mentions_add fold_left];destruct(nmem x out)eqn:E;apply IH;[exact Nd|].
 apply NoDup_app;[exact Nd| |].
 - constructor;[intros[]|constructor].
 - intros y Hy [<-|[]];apply nmem_in in Hy;rewrite Hy in E;discriminate E.
Qed.
Lemma mention_item_nodup:forall f self mi out sp s found s',
 mention_item f self mi out sp s=Good(found,s')->NoDup out->NoDup found.
Proof.
 intros f self mi out[pa d a]s found s' H Nd;destruct pa;[|injection H as <- <-;exact Nd].
 unfold mention_item in H.
 apply bind_inv in H as(bl&s1&Hb&H);apply bind_inv in H as(rhs&s2&Hr&H).
 apply bind_inv in H as(tg&s3&Hg&H);destruct tg as[g|];[|injection H as <- <-;exact Nd].
 apply bind_inv in H as(here&s4&Hh&H);injection H as <- <-.
 apply mentions_add_nodup;exact Nd.
Qed.
Lemma mention_fold_nodup:forall X(step:list nat->X->M(list nat))xs,
 (forall out x s found s',step out x s=Good(found,s')->NoDup out->NoDup found)->
 forall out s found s',mfold step out xs s=Good(found,s')->NoDup out->NoDup found.
Proof.
 intros X step xs HS;induction xs as[|x rest IH];intros out s found s' H Nd.
 - injection H as <- <-;exact Nd.
 - cbn[mfold]in H;apply bind_inv in H as(mid&s1&H1&H).
   exact(IH mid s1 found s' H(HS out x s mid s1 H1 Nd)).
Qed.
Theorem result_mentions_go_nodup:forall f self mi specs out s found s',
 result_mentions_go f self mi specs out s=Good(found,s')->NoDup out->NoDup found.
Proof.
 intros[|f]self mi specs out s found s' H Nd;[discriminate H|].
 rewrite result_mentions_view in H.
 exact(mention_fold_nodup spec(mention_item f self mi)specs(mention_item_nodup f self mi)out s found s' H Nd).
Qed.
Theorem functor_fixed_go_nodup:forall f g s found s',
 functor_fixed_go f g s=Good(found,s')->NoDup found.
Proof.
 intros[|f]g s found s' H;[discriminate H|].
 cbn[functor_fixed_go]in H;apply bind_inv in H as(ps&s1&Hp&H);destruct ps as[keys|].
 2:{injection H as <- <-;constructor. }
 destruct(functor_parts g)as[[args res]body].
 apply bind_inv in H as(ms&s2&Hm&H);apply bind_inv in H as(rts&s3&Hr&H);injection H as <- <-.
 apply NoDup_filter;eapply result_mentions_go_nodup;[exact Hm|constructor].
Qed.
Theorem functor_fixed_nodup:forall g s found s',functor_fixed g s=Good(found,s')->NoDup found.
Proof.
 intros g s found s' H;unfold functor_fixed in H;apply bind_inv in H as(f&s1&Hf&H).
 exact(functor_fixed_go_nodup _ _ _ _ _ H).
Qed.
