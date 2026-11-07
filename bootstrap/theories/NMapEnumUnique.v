(* Finite-trie enumeration has no duplicate keys, independently of values. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import NMap Elab NMapEnum.
Import ListNotations.
Local Open Scope list_scope.
Lemma enum_key_index:forall X(m:nmap X)base step k,
 In k(map fst(nmap_elems m base step))->exists q,k=base+step*q.
Proof.
 intros X m base step k Hit;apply in_map_iff in Hit as([x v]&Eq&Hit);cbn[fst]in Eq;subst x.
 destruct(nmap_elems_member X m base step k v Hit)as(q&Lookup&Index);exists q;exact Index.
Qed.
Theorem nmap_elems_keys_nodup:forall X(m:nmap X)base step,
 0<step->NoDup(map fst(nmap_elems m base step)).
Proof.
 intros X m;induction m as[|l IHl v r IHr];intros base step Positive;[constructor|].
 assert(Left:NoDup(map fst(nmap_elems l(base+step)(2*step))))by(apply IHl;lia).
 assert(Right:NoDup(map fst(nmap_elems r(base+2*step)(2*step))))by(apply IHr;lia).
 assert(Separate:forall k,In k(map fst(nmap_elems l(base+step)(2*step)))->
  ~In k(map fst(nmap_elems r(base+2*step)(2*step)))).
 {intros k Hl Hr;destruct(enum_key_index _ _ _ _ _ Hl)as[ql EqL].
  destruct(enum_key_index _ _ _ _ _ Hr)as[qr EqR ].
  assert(E:step*(1+2*ql)=step*(2+2*qr))by nia.
  apply Nat.mul_cancel_l in E;lia. }
 assert(Both:NoDup(map fst(nmap_elems l(base+step)(2*step))++map fst(nmap_elems r(base+2*step)(2*step))))
 by(apply NoDup_app;auto).
 cbn[nmap_elems];rewrite !map_app;destruct v;cbn[map List.app];[|exact Both].
 constructor;[|exact Both];intro Hit;apply in_app_or in Hit as[Hl|Hr].
 - destruct(enum_key_index _ _ _ _ _ Hl)as[q Eq];cbn[fst]in Eq;lia.
 - destruct(enum_key_index _ _ _ _ _ Hr)as[q Eq];cbn[fst]in Eq;lia.
Qed.
Theorem elems_keys_nodup:forall X(m:nmap X),NoDup(map fst(elems m)).
Proof. intros X m;unfold elems;apply nmap_elems_keys_nodup;lia. Qed.
