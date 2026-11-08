(* Historical lambda rows never disagree with their visible body. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame FnBound LambdaReadonly.
Import ListNotations.
Definition LambdaRows(s:st):Prop:=forall key body owner,
 In(key,body)(st_fns s)->nget(st_syms s)key=Some owner->gs_origin owner=OLambda->
 nassoc key(st_fns s)=Some body.
Lemma lambda_rows_init:forall w,LambdaRows(s_init w).
Proof. intros w key body owner Row;contradiction. Qed.
Lemma lookup_none_no_row:forall X key(xs:list(nat*X)),nassoc key xs=None->forall value,~In(key,value)xs.
Proof.
 intros X key xs;induction xs as[|[k v]xs IH];intros Empty value Member;[contradiction|].
 cbn[nassoc assoc]in Empty;destruct(Nat.eqb_spec key k)as[->|Ne];[discriminate|].
 destruct Member as[Eq|Member];[injection Eq;congruence|exact(IH Empty _ Member)].
Qed.
Lemma lambda_rows_kept:forall s final,FnBound s->LambdaRows s->Fr s final->st_fns final=st_fns s->LambdaRows final.
Proof.
 intros s final Bound Rows Frame Same key body owner Row Lookup Origin;rewrite Same in Row|-*.
 destruct(nget(st_syms s)key)as[old|]eqn:Old.
 - pose proof(fr_keep _ _ Frame _ _ Old)as Kept;rewrite Lookup in Kept;injection Kept as <-.
   exact(Rows _ _ _ Row Old Origin).
 - pose proof(fr_new_above _ _ Frame _ _ Old Lookup);pose proof(Bound _ _ Row);lia.
Qed.
Lemma lambda_rows_emit_fresh:forall s key body,
 LambdaRows s->nassoc key(st_fns s)=None->LambdaRows(s_fns(fun rows=>(key,body)::rows)s).
Proof.
 intros s key body Rows Empty k old owner Row Lookup Origin.
 cbn[s_fns st_fns]in Row|-*;destruct Row as[Eq|Row].
 - injection Eq as <- <-;cbn[nassoc assoc];rewrite Nat.eqb_refl;reflexivity.
 - destruct(Nat.eq_dec k key)as[->|Ne].
   + exfalso;exact(lookup_none_no_row _ _ _ Empty _ Row).
   + cbn[nassoc assoc];destruct(Nat.eqb_spec k key)as[Eq|];[contradiction|].
     exact(Rows _ _ _ Row Lookup Origin).
Qed.
Lemma lambda_rows_emit_other:forall s key body writer,
 LambdaRows s->nget(st_syms s)key=Some writer->gs_origin writer<>OLambda->
 LambdaRows(s_fns(fun rows=>(key,body)::rows)s).
Proof.
 intros s key body writer Rows Writer NotLambda k old owner Row Lookup Origin.
 assert(Ne:k<>key).
 {intro Eq;subst k;cbn[s_fns st_syms]in Lookup;rewrite Writer in Lookup;injection Lookup as <-;contradiction. }
 cbn[s_fns st_fns]in Row|-*;destruct Row as[Eq|Row];[injection Eq;congruence|].
 cbn[nassoc assoc];destruct(Nat.eqb_spec k key)as[Eq|];[contradiction|].
 exact(Rows _ _ _ Row Lookup Origin).
Qed.
Lemma lookup_filtered_owner:forall X key(rows:list(nat*X)),
 nassoc key(filter(fun p=>Nat.eqb key(fst p))rows)=nassoc key rows.
Proof.
 intros X key rows;induction rows as[|[k value]rows IH];[reflexivity|].
 cbn[filter fst];destruct(Nat.eqb key k)eqn:Eq;cbn[nassoc assoc];rewrite ?Eq;[reflexivity|exact IH].
Qed.
Lemma lambda_rows_stable_lookup:forall s final,
 lambda_rows_stable s final->forall key owner,nget(st_syms s)key=Some owner->gs_origin owner=OLambda->
 nassoc key(st_fns final)=nassoc key(st_fns s).
Proof.
 intros s final Stable key owner Lookup Origin;pose proof(Stable _ _ Lookup Origin)as Eq.
 apply(f_equal(nassoc key))in Eq;unfold lambda_owner_rows in Eq;rewrite !lookup_filtered_owner in Eq;exact Eq.
Qed.
