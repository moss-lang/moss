(* The actual marker list is injective and disjoint from all caller readings. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound FreshMarkerExec.
Import ListNotations.
Lemma family_marker_allocation:forall e receiver (ps:list nat) s marks sf,
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps s=Good(marks,sf)->
 NoDup(map snd marks)/\st_next s<=st_next sf/\
 Forall(fun q=>st_next s<=snd q<st_next sf/\
   forall g,In g(marker_generation_ids e)->g<snd q)marks.
Proof.
 intros e receiver ps;induction ps as[|p ps IH];intros s marks sf E.
 - injection E as <- <-;split;[constructor|];split;[lia|constructor].
 - cbn[mmap]in E;apply bind_inv in E as(q&sq&Head&E).
   apply bind_inv in Head as(k&sk&Alloc&Ret);injection Ret as <- <-.
   apply bind_inv in E as(rest&sr&Tail&Ret);injection Ret as <- <-.
   destruct(IH sk rest sr Tail)as[Nd[Next Bounds]].
   destruct(proj1(fresh_marker_for_above _ _ _ _ _ Alloc))as[Lo[At Avoid]].
   split.
   + cbn[map snd];constructor;[|exact Nd].
     intro Bad;apply in_map_iff in Bad as([pk g]&Eq&Hin);cbn[snd]in Eq;subst g.
     rewrite Forall_forall in Bounds;destruct(Bounds(pk,k)Hin)as[[Low High]Av];cbn[snd]in Low;lia.
   + split;[lia|];constructor.
     * cbn[snd];split;[lia|exact Avoid].
     * eapply Forall_impl;[|exact Bounds];intros[pk g][[Low High]Av];cbn[snd]in *.
       split;[lia|exact Av].
Qed.
Lemma family_marker_keys:forall e receiver (ps:list nat) s marks sf,
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps s=Good(marks,sf)->map fst marks=ps.
Proof.
 intros e receiver ps;induction ps as[|p ps IH];intros s marks sf E.
 - injection E as <- <-;reflexivity.
 - cbn[mmap]in E;apply bind_inv in E as(q&sq&Head&E).
   apply bind_inv in Head as(k&sk&Alloc&Ret);injection Ret as <- <-.
   apply bind_inv in E as(rest&sr&Tail&Ret);injection Ret as <- <-.
   cbn[map fst snd];f_equal;eapply IH;exact Tail.
Qed.
Theorem family_markers_avoid_caller:forall e receiver (ps:list nat) s marks sf,
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps s=Good(marks,sf)->
 forall p k,In(p,k)marks->
 (forall t,In t(reify_scope_types e)->~In k(ety_gens t))/\
 nassoc k(en_uf e)=None/\canon e(YAbs k)=YAbs k.
Proof.
 intros e receiver ps s marks sf E p k Hin.
 destruct(family_marker_allocation _ _ _ _ _ _ E)as[Nd[Next Bounds]].
 rewrite Forall_forall in Bounds;destruct(Bounds(p,k)Hin)as[Range Avoid];cbn[snd]in Avoid.
 assert(Absent:nassoc k(en_uf e)=None).
 {destruct(nassoc k(en_uf e))as[y|]eqn:Look;[|reflexivity].
  exfalso;assert(Key:In k(map fst(en_uf e))).
  {apply in_map_iff;exists(k,y);split;[reflexivity|].
   clear Avoid Range Bounds Hin E Nd Next.
   induction(en_uf e)as[|[j v]rest IH];[discriminate Look|].
   cbn[nassoc assoc]in Look;destruct(Nat.eqb k j)eqn:J.
   - apply Nat.eqb_eq in J;subst j;injection Look as <-;left;reflexivity.
   - right;apply IH;exact Look. }
  pose proof(Avoid k(marker_uf_key e k Key));lia. }
 split.
 - intros t Ht Bad;pose proof(Avoid k(marker_scope_type e t k Ht Bad));lia.
 - split;[exact Absent|]. unfold canon;cbn[canon_go];rewrite Absent;reflexivity.
Qed.
From NM Require Import CanonGenerations.
Lemma reify_scope_uf_value:forall e h y,In(h,y)(en_uf e)->In y(reify_scope_types e).
Proof.
 intros e h y Hy;unfold reify_scope_types;apply in_app_iff;right;apply in_app_iff;left.
 apply in_map_iff;exists(h,y);split;[reflexivity|exact Hy].
Qed.
Theorem family_markers_avoid_canonical_caller:forall e receiver (ps:list nat) s marks sf,
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps s=Good(marks,sf)->
 forall p k,In(p,k)marks->forall t,In t(reify_scope_types e)->
 forall g,In g(ety_gens(canon e t))->g<k.
Proof.
 intros e receiver ps s marks sf E p k Hin t Ht.
 destruct(family_marker_allocation _ _ _ _ _ _ E)as[Nd[Next Bounds]].
 rewrite Forall_forall in Bounds;destruct(Bounds(p,k)Hin)as[Range Avoid];cbn[snd]in Avoid.
 eapply canon_generations.
 - intros h y Hy g Hg;apply Avoid;eapply marker_scope_type;[exact(reify_scope_uf_value e h y Hy)|exact Hg].
 - intros g Hg;apply Avoid;eapply marker_scope_type;[exact Ht|exact Hg].
Qed.
Theorem family_markers_receiver_bounds:forall e receiver (ps:list nat) s marks sf,
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps s=Good(marks,sf)->
 Forall(fun q=>forall g,In g(ety_gens receiver)->g<snd q)marks.
Proof.
 intros e receiver ps;induction ps as[|p ps IH];intros s marks sf E;[injection E as <- <-;constructor|].
 cbn[mmap]in E;apply bind_inv in E as(q&sq&Head&E).
 apply bind_inv in Head as(k&sk&Alloc&Ret);injection Ret as <- <-.
 apply bind_inv in E as(rest&sr&Tail&Ret);injection Ret as <- <-.
 constructor;[exact(proj2(fresh_marker_for_above _ _ _ _ _ Alloc))|exact(IH _ _ _ Tail)].
Qed.
Corollary family_markers_avoid_receiver:forall e receiver (ps:list nat) s marks sf p k,
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps s=Good(marks,sf)->
 In(p,k)marks->~In k(ety_gens receiver).
Proof.
 intros e receiver ps s marks sf p k E Hin Bad.
 pose proof(family_markers_receiver_bounds _ _ _ _ _ _ E)as Bounds;rewrite Forall_forall in Bounds.
 pose proof(Bounds(p,k)Hin k Bad);cbn[snd]in H;lia.
Qed.
