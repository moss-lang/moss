(* Full finite-trie enumeration agrees with lookup, at every key depth. *)
From Stdlib Require Import List Arith Lia Bool.
From NM Require Import NMap Elab.
Import ListNotations.
Lemma enum_even_double:forall k,is_even_nat(2*k)=true.
Proof. intro k;unfold is_even_nat;rewrite Nat.div2_double;rewrite Nat.sub_diag;reflexivity. Qed.
Lemma enum_odd_double:forall k,is_even_nat(S(2*k))=false.
Proof. intro k;unfold is_even_nat;rewrite Nat.div2_succ_double;replace(S(2*k)-2*k)with 1 by lia;reflexivity. Qed.
Lemma enum_even_index:forall k,is_even_nat k=true->k=2*Nat.div2 k.
Proof.
 intros k H;unfold is_even_nat in H;apply Nat.eqb_eq in H.
 assert(Lo:2*Nat.div2 k<=k)by(rewrite Nat.div2_div;apply Nat.mul_div_le;lia).
 lia.
Qed.
Lemma enum_odd_index:forall k,is_even_nat k=false->k=2*Nat.div2 k+1.
Proof.
 intros k H;unfold is_even_nat in H;apply Nat.eqb_neq in H.
 pose proof(Nat.div_mod k 2 ltac:(lia))as D.
 pose proof(Nat.mod_upper_bound k 2 ltac:(lia))as M.
 rewrite<-Nat.div2_div in D;lia.
Qed.
Theorem nmap_elems_member:forall X(m:nmap X)base step k v,
 In(k,v)(nmap_elems m base step)->exists q,nget m q=Some v/\k=base+step*q.
Proof.
 intros X m;induction m as[|l IHl root r IHr];intros base step k v H;[contradiction|].
 cbn[nmap_elems]in H;apply in_app_or in H as[Hroot|Hrest].
 - destruct root as[x|];[|contradiction]. destruct Hroot as[Eq|[]];injection Eq as <- <-.
   exists 0;split;[reflexivity|lia].
 - apply in_app_or in Hrest as[Hl|Hr].
   + destruct(IHl _ _ _ _ Hl)as(q&Lookup&Eq).
     exists(S(2*q));split.
     * cbn[nget];rewrite enum_even_double,Nat.div2_double;exact Lookup.
     * rewrite Eq;nia.
   + destruct(IHr _ _ _ _ Hr)as(q&Lookup&Eq).
     exists(S(S(2*q)));split.
     * cbn[nget];rewrite enum_odd_double,Nat.div2_succ_double;exact Lookup.
     * rewrite Eq;nia.
Qed.
Theorem nmap_elems_complete:forall X(m:nmap X)base step k v,
 nget m k=Some v->In(base+step*k,v)(nmap_elems m base step).
Proof.
 intros X m;induction m as[|l IHl root r IHr];intros base step k v H;[discriminate H|].
 destruct k as[|k].
 - cbn[nget]in H;destruct root as[x|];[injection H as <-|discriminate H].
   replace(base+step*0)with base by lia;cbn[nmap_elems];left;reflexivity.
 - cbn[nget]in H;destruct(is_even_nat k)eqn:Even.
   + assert(K:k=2*Nat.div2 k)by(apply enum_even_index;exact Even).
     replace(base+step*S k)with((base+step)+(2*step)*Nat.div2 k)by nia.
     cbn[nmap_elems];apply in_app_iff;right;apply in_app_iff;left;apply IHl;exact H.
   + assert(K:k=2*Nat.div2 k+1)by(apply enum_odd_index;exact Even).
     replace(base+step*S k)with((base+2*step)+(2*step)*Nat.div2 k)by nia.
     cbn[nmap_elems];apply in_app_iff;right;apply in_app_iff;right;apply IHr;exact H.
Qed.
Theorem elems_lookup:forall X(m:nmap X)k v,In(k,v)(elems m)<->nget m k=Some v.
Proof.
 intros X m k v;split.
 - intro H;destruct(nmap_elems_member X m 0 1 k v H)as(q&Lookup&Eq);replace q with k in Lookup by lia;exact Lookup.
 - intro H;pose proof(nmap_elems_complete X m 0 1 k v H)as Hin;replace(0+1*k)with k in Hin by lia;exact Hin.
Qed.
Fixpoint enum_deep(depth:nat):nmap unit:=
 match depth with 0=>NN NE(Some tt)NE|S d=>NN(enum_deep d)None NE end.
Lemma enum_deep_count:forall depth base step,List.length(nmap_elems(enum_deep depth)base step)=1.
Proof.
 induction depth as[|d IH];intros base step;cbn[enum_deep nmap_elems List.length List.app];[reflexivity|].
 rewrite app_length,IH;reflexivity.
Qed.
Example elems_below_old_depth_limit:List.length(elems(enum_deep 70))=1.
Proof. apply enum_deep_count. Qed.
