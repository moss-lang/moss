(* The actual lambda capture filter has exactly the lexical names the rule
   requests, and preserves the real source local table's uniqueness. *)
From Stdlib Require Import List String Bool.
From NM Require Import ExecutionModel Sx Elab LambdaNameRead LambdaLocals.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition lambda_source_captures(c:cx)(e:env)(ps:list(name*option aty))(body:aexpr):=
 filter(fun q=>smem(fst q)(free_names_e 10000 body)&&negb(smem(fst q)(map fst ps)))
 (match cx_applying c with[]=>en_locals e|_=>[]end).
Lemma capture_smem_true:forall n names,smem n names=true<->In n names.
Proof.
 intros n names;induction names as[|x names IH];cbn[smem In];[split;[discriminate|contradiction]|].
 rewrite orb_true_iff,String.eqb_eq,IH;split;intros[Eq|Member];[left;symmetry;exact Eq|right;exact Member|left;symmetry;exact Eq|right;exact Member].
Qed.
Lemma capture_smem_false:forall n names,smem n names=false<->~In n names.
Proof.
 intros n names;destruct(smem n names)eqn:E.
 - split;[discriminate|intro Ne;exfalso;apply Ne;apply capture_smem_true;exact E].
 - split;[intros _ Member;apply capture_smem_true in Member;congruence|intro;reflexivity].
Qed.
Lemma capture_local_lookup:forall X n(rows:list(string*X))value,
 NoDup(map fst rows)->In(n,value)rows->sassoc n rows=Some value.
Proof.
 intros X n rows value Unique Member;induction rows as[|[k old]rows IH];[contradiction|].
 inversion Unique as[|? ? NotIn Rest];subst;cbn[sassoc assoc].
 destruct Member as[Eq|Member].
 - injection Eq as <- <-;rewrite String.eqb_refl;reflexivity.
 - destruct(String.eqb_spec n k)as[->|Ne];[|apply IH;assumption].
   exfalso;apply NotIn;apply in_map_iff;exists(k,value);split;[reflexivity|exact Member].
Qed.
Lemma capture_lookup_member:forall X n(rows:list(string*X))value,
 sassoc n rows=Some value->In(n,value)rows.
Proof.
 intros X n rows;induction rows as[|[k old]rows IH];intros value Read;[discriminate|].
 cbn[sassoc assoc]in Read;destruct(String.eqb_spec n k)as[->|Ne].
 - injection Read as <-;left;reflexivity.
 - right;exact(IH _ Read).
Qed.
Theorem lambda_capture_names_nodup:forall c e ps body,
 NoDup(map fst(en_locals e))->NoDup(map fst(lambda_source_captures c e ps body)).
Proof.
 intros c e ps body Unique;unfold lambda_source_captures;destruct(cx_applying c);[apply filtered_names_nodup;exact Unique|constructor].
Qed.
Theorem lambda_capture_names_exact:forall c e ps body,
 NoDup(map fst(en_locals e))->forall n,
 In n(map fst(lambda_source_captures c e ps body))<->
 A.Mentions_name body n/\sassoc n(en_locals e)<>None/\~In n(map fst ps)/\cx_applying c=[].
Proof.
 intros c e ps body Unique n;unfold lambda_source_captures.
 destruct(cx_applying c)as[|u rest]eqn:Applying.
 - split.
   + intro Member;apply in_map_iff in Member as[[key value][Eq Member]];cbn[fst]in Eq;subst key.
     apply filter_In in Member as[Row Test];apply andb_true_iff in Test as[Named NotParam].
     apply capture_smem_true in Named;apply negb_true_iff in NotParam;apply capture_smem_false in NotParam.
     split;[apply(free_names_exact 10000 body n);exact Named|].
     split;[rewrite(capture_local_lookup _ _ _ _ Unique Row);discriminate|].
     split;[exact NotParam|reflexivity].
   + intros[Mention[Present[NotParam _]]].
     destruct(sassoc n(en_locals e))as[value|]eqn:Read;[|contradiction].
     apply in_map_iff;exists(n,value);split;[reflexivity|].
     apply filter_In;split;[apply capture_lookup_member;exact Read|].
     apply andb_true_iff;split;[apply capture_smem_true;apply(free_names_exact 10000 body n);exact Mention|].
     apply negb_true_iff;apply capture_smem_false;exact NotParam.
 - cbn[filter map];split;[contradiction|intros[_[_[_ Eq]]];discriminate].
Qed.
