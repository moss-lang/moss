(* Raw local names are unique under the actual local setters and capture filter. *)
From Stdlib Require Import List String Bool.
From NM Require Import ExecutionModel Sx Elab.
Import ListNotations.
Lemma local_keys_aset_member:forall X name(value:X)rows key,
 In key(map fst(aset String.eqb name value rows))->key=name\/In key(map fst rows).
Proof.
 intros X name value rows;induction rows as[|[n old]rows IH];intros key Member.
 - cbn[aset map fst]in Member;destruct Member as[Eq|[]];left;symmetry;exact Eq.
 - cbn[aset]in Member;destruct(String.eqb_spec name n)as[->|Ne].
   + cbn[map fst]in Member;destruct Member as[Eq|Member];[left;symmetry;exact Eq|right;right;exact Member].
   + cbn[map fst]in Member;destruct Member as[Eq|Member];[right;left;exact Eq|].
     destruct(IH _ Member)as[Eq|Old];[left;exact Eq|right;right;exact Old].
Qed.
Lemma local_keys_aset_nodup:forall X name(value:X)rows,
 NoDup(map fst rows)->NoDup(map fst(aset String.eqb name value rows)).
Proof.
 intros X name value rows;induction rows as[|[n old]rows IH];intro Unique.
 - cbn[aset map fst];constructor;[intros[]|constructor].
 - inversion Unique;subst;cbn[aset];destruct(String.eqb_spec name n)as[->|Ne].
   + exact Unique.
   + cbn[map fst];constructor;[|apply IH;assumption].
     intro Member;destruct(local_keys_aset_member X name value rows n Member)as[Eq|Old];[congruence|contradiction].
Qed.
Lemma set_local_names_nodup:forall e name ty mut,
 NoDup(map fst(en_locals e))->NoDup(map fst(en_locals(set_local e name ty mut))).
Proof. intros;apply local_keys_aset_nodup;assumption. Qed.
Lemma filtered_names_nodup:forall X(test:string*X->bool)rows,
 NoDup(map fst rows)->NoDup(map fst(filter test rows)).
Proof.
 intros X test rows;induction rows as[|[n value]rows IH];intro Unique;[constructor|].
 inversion Unique;subst;cbn[filter];destruct(test(n,value));[|apply IH;assumption].
 cbn[map fst];constructor;[|apply IH;assumption].
 intro Member;apply in_map_iff in Member as[[key v][Eq InFiltered]];cbn[fst]in Eq;subst key.
 apply filter_In in InFiltered as[InOld _].
 match goal with Ne:~In n(map fst rows)|-_=>apply Ne end.
 apply in_map_iff;exists(n,v);split;[reflexivity|exact InOld].
Qed.
