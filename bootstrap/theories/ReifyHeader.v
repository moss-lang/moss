(* Annotation events and exports cannot replace a code owner's entry scope. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab.
Import ListNotations.
Definition code_header(code:reify_code ety key):=
 (rc_owner ety key code,rc_scope ety key code,rc_inputs ety key code,rc_signature ety key code).
Definition function_header(s:st)(f:nat):=
 option_map code_header(assoc reify_owner_eqb(ROFunction f)(rm_codes(st_reify s))).
Lemma code_header_map:forall change rows owner,
 (forall row,fst(change row)=fst row/\code_header(snd(change row))=code_header(snd row))->
 option_map code_header(assoc reify_owner_eqb owner(map change rows))=
 option_map code_header(assoc reify_owner_eqb owner rows).
Proof.
 intros change rows;induction rows as[|[key code]rows IH];intros owner Keep;[reflexivity|].
 pose proof(Keep(key,code))as[Key Header].
 destruct(change(key,code))as[key' code']eqn:Eq;cbn[fst snd]in Key,Header;subst key'.
 cbn[map assoc];rewrite Eq;cbn[fst snd].
 destruct(reify_owner_eqb owner key);[cbn[option_map];rewrite Header;reflexivity|apply IH;exact Keep].
Qed.
Lemma function_header_event:forall event s f,
 function_header(s_reify(fun r=>
 let codes:=match rm_owner r with
 |None=>rm_codes r
 |Some owner=>map(fun p=>if reify_owner_eqb(fst p)owner then
  let code:=snd p in(fst p,ReifyCode ety key(rc_owner ety key code)(rc_scope ety key code)
   (rc_inputs ety key code)(rc_signature ety key code)(rc_exports ety key code)
   (event::rc_events ety key code))else p)(rm_codes r)end in
 ReifyMetadata(rm_owner r)codes(rm_declarations r)(rm_constants r))s)f=function_header s f.
Proof.
 intros event s f;unfold function_header;cbn[s_reify st_reify].
 destruct(rm_owner(st_reify s));[|reflexivity].
 apply code_header_map;intros[owner code];cbn[fst snd];destruct(reify_owner_eqb owner r);split;reflexivity.
Qed.
Lemma record_event_header:forall event s final,
 record_reify_event event s=Good(tt,final)->forall f,function_header final f=function_header s f.
Proof. intros event s final Run f;unfold record_reify_event,upd in Run;injection Run as <-;apply function_header_event. Qed.
Lemma record_exports_header:forall owner types items inputs promised actual roles s final,
 record_reify_exports_at owner types items inputs promised actual roles s=Good(tt,final)->
 forall f,function_header final f=function_header s f.
Proof.
 intros owner types items inputs promised actual roles s final Run f.
 unfold record_reify_exports_at,upd in Run;injection Run as <-.
 unfold function_header;cbn[s_reify st_reify];apply code_header_map.
 intros[other code];cbn[fst snd];destruct(reify_owner_eqb other owner);split;reflexivity.
Qed.
Lemma register_other_header:forall owner e inputs sg s final,
 register_reify_code owner e inputs sg s=Good(tt,final)->forall f,
 reify_owner_eqb(ROFunction f)owner=false->function_header final f=function_header s f.
Proof.
 intros owner e inputs sg s final Run f Other;unfold register_reify_code,upd in Run;injection Run as <-.
 unfold function_header;cbn[s_reify st_reify rm_codes assoc];rewrite Other;reflexivity.
Qed.
Lemma register_function_header:forall f e inputs sg s final,
 register_reify_code(ROFunction f)e inputs(Some sg)s=Good(tt,final)->
 function_header final f=Some(ROFunction f,reify_scope_of e,inputs,Some(reify_sig e sg)).
Proof.
 intros f e inputs sg s final Run;unfold register_reify_code,upd in Run;injection Run as <-.
 unfold function_header;cbn[s_reify st_reify rm_codes assoc reify_owner_eqb];rewrite Nat.eqb_refl;reflexivity.
Qed.
Definition header_keeps{A}(m:M A):Prop:=forall s a final,m s=Good(a,final)->
 forall f,function_header final f=function_header s f.
Lemma header_keeps_ret:forall A(a:A),header_keeps(ret a).
Proof. intros A a s result final E;injection E as <- <-;reflexivity. Qed.
Lemma header_keeps_bind:forall A B(m:M A)(k:A->M B),
 header_keeps m->(forall a,header_keeps(k a))->header_keeps(mbind m k).
Proof.
 intros A B m k First Last s result final E f;unfold mbind in E.
 destruct(m s)as[[a mid]|error]eqn:Run;[|discriminate E].
 rewrite(Last a _ _ _ E f);exact(First _ _ _ Run f).
Qed.
Lemma header_keeps_mmap:forall A B(f:A->M B)xs,
 (forall a,header_keeps(f a))->header_keeps(mmap f xs).
Proof. intros A B f xs Keep;induction xs;cbn[mmap];[apply header_keeps_ret|].
 apply header_keeps_bind;[apply Keep|intro value;apply header_keeps_bind;[exact IHxs|intro;apply header_keeps_ret]]. Qed.
Lemma header_keeps_mfold:forall A B(f:A->B->M A)xs init,
 (forall a b,header_keeps(f a b))->header_keeps(mfold f init xs).
Proof. intros A B f xs;induction xs;intros init Keep;cbn[mfold];[apply header_keeps_ret|].
 apply header_keeps_bind;[apply Keep|intro;apply IHxs;exact Keep]. Qed.
Definition code_header_rows(rows:list(reify_owner*reify_code ety key)):=
 flat_map(fun row=>match fst row with ROFunction f=>[(f,code_header(snd row))]|_=>[]end)rows.
Definition function_headers(s:st):=code_header_rows(rm_codes(st_reify s)).
Lemma function_header_lookup:forall s f,function_header s f=nassoc f(function_headers s).
Proof.
 intros s f;unfold function_header,function_headers;generalize(rm_codes(st_reify s));intro rows.
 induction rows as[|[owner code]rows IH];[reflexivity|].
 destruct owner as[id|target|target provider];cbn[code_header_rows flat_map fst snd List.app assoc reify_owner_eqb nassoc];
 try exact IH;destruct(Nat.eqb f id);[reflexivity|exact IH].
Qed.
Lemma code_header_rows_map:forall change rows,
 (forall row,fst(change row)=fst row/\code_header(snd(change row))=code_header(snd row))->
 code_header_rows(map change rows)=code_header_rows rows.
Proof.
 intros change rows Keep;induction rows as[|[owner code]rows IH];[reflexivity|].
 pose proof(Keep(owner,code))as[Owner Header];destruct(change(owner,code))as[owner' code']eqn:Eq.
 cbn[fst snd]in Owner,Header;subst owner';cbn[map code_header_rows flat_map fst snd];rewrite Eq.
 cbn[fst snd];destruct owner;cbn[List.app];try exact IH.
 f_equal;[f_equal;exact Header|exact IH].
Qed.
Lemma record_event_header_rows:forall event s final,
 record_reify_event event s=Good(tt,final)->function_headers final=function_headers s.
Proof.
 intros event s final Run;unfold record_reify_event,upd in Run;injection Run as <-.
 unfold function_headers;cbn[s_reify st_reify];destruct(rm_owner(st_reify s));[|reflexivity].
 apply code_header_rows_map;intros[owner code];cbn[fst snd];destruct(reify_owner_eqb owner r);split;reflexivity.
Qed.
Lemma record_exports_header_rows:forall owner types items inputs promised actual roles s final,
 record_reify_exports_at owner types items inputs promised actual roles s=Good(tt,final)->
 function_headers final=function_headers s.
Proof.
 intros owner types items inputs promised actual roles s final Run.
 unfold record_reify_exports_at,upd in Run;injection Run as <-.
 unfold function_headers;cbn[s_reify st_reify];apply code_header_rows_map.
 intros[other code];cbn[fst snd];destruct(reify_owner_eqb other owner);split;reflexivity.
Qed.
