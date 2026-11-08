(* Existing function scopes survive body compilation, including pending lambdas. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv ReifyHeader
 FnPresentStable FnPresentEmit.
Import ListNotations.
Set Implicit Arguments.
Definition header_protected(s:st)(f:nat):Prop:=
 (exists body,nassoc f(st_fns s)=Some body)\/
 (exists symbol,nget(st_syms s)f=Some symbol/\gs_origin symbol=OLambda).
Lemma header_protected_frame:forall s final,Fr s final->fn_present_stable s final->
 forall f,f<st_next s->header_protected s f->header_protected final f.
Proof.
 intros s final Frame Functions f Bound[[body Present]|[symbol[At Origin]]].
 - left;exists body;exact(Functions _ _ Bound Present).
 - right;exists symbol;split;[exact(fr_keep _ _ Frame _ _ At)|exact Origin].
Qed.
Definition HS{A}(m:M A):Prop:=PF m/\forall s a final,
 Inv s->m s=Good(a,final)->forall f,f<st_next s->header_protected s f->
 function_header final f=function_header s f.
Definition HSE(owner:nat){A}(m:M A):Prop:=PF_except owner m/\forall s a final,
 Inv s->m s=Good(a,final)->forall f,f<>owner->f<st_next s->header_protected s f->
 function_header final f=function_header s f.
Arguments HS:simpl never.
Arguments HSE:simpl never.
Lemma hs_ret:forall A(a:A),HS(ret a).
Proof. intros A a;split;[apply pf_ret|]. intros s value final I Run;injection Run as <- <-;reflexivity. Qed.
Lemma hs_keeps:forall A(m:M A),PF m->header_keeps m->HS m.
Proof. intros A m PFm Keep;split;[exact PFm|];intros s a final I Run f Bound Protected;exact(Keep _ _ _ Run f). Qed.
Lemma hs_bind:forall A B(m:M A)(k:A->M B),HS m->(forall a,HS(k a))->HS(mbind m k).
Proof.
 intros A B m k[PFm Hm]K;split;[apply pf_bind;[exact PFm|intro;exact(proj1(K a))]|].
 intros s value final I Run f Bound Protected;apply bind_inv in Run as[a[mid[First Last]]].
 destruct(proj1 PFm _ _ _ I First)as[Im Frame].
 rewrite(proj2(K a)_ _ _ Im Last f ltac:(pose proof(fr_next _ _ Frame);lia)
  (header_protected_frame Frame(proj2 PFm _ _ _ I First)Bound Protected)).
 exact(Hm _ _ _ I First f Bound Protected).
Qed.
Lemma hs_mmap:forall A B(f:A->M B)xs,(forall x,HS(f x))->HS(mmap f xs).
Proof. intros A B f xs K;induction xs;cbn[mmap];[apply hs_ret|].
 apply hs_bind;[apply K|intro value;apply hs_bind;[exact IHxs|intro;apply hs_ret]]. Qed.
Lemma hs_miter:forall A(f:A->M unit)xs,(forall x,HS(f x))->HS(miter f xs).
Proof. intros A f xs K;induction xs;cbn[miter];[apply hs_ret|].
 apply hs_bind;[apply K|intro;exact IHxs]. Qed.
Lemma hs_mfold:forall A B(f:A->B->M A)xs initial,(forall a x,HS(f a x))->HS(mfold f initial xs).
Proof. intros A B f xs;induction xs;intros initial K;cbn[mfold];[apply hs_ret|].
 apply hs_bind;[apply K|intro;apply IHxs;exact K]. Qed.
Lemma hse_of_hs:forall A owner(m:M A),HS m->HSE owner m.
Proof. intros A owner m[PFm Keep];split;[apply pfe_of_pf;exact PFm|];intros;eapply Keep;eassumption. Qed.
Lemma hse_bind:forall A B owner(m:M A)(k:A->M B),HSE owner m->
 (forall a,HSE owner(k a))->HSE owner(mbind m k).
Proof.
 intros A B owner m k[PFm Hm]K;split;[apply pfe_bind;[exact PFm|intro;exact(proj1(K a))]|].
 intros s value final I Run f Other Bound Protected;apply bind_inv in Run as[a[mid[First Last]]].
 destruct(proj1 PFm _ _ _ I First)as[Im Frame].
 assert(Bmid:f<st_next mid)by(pose proof(fr_next _ _ Frame);lia).
 assert(Pmid:header_protected mid f).
 {destruct Protected as[[body Present]|[symbol[At Origin]]].
  - left;exists body;exact(proj2 PFm _ _ _ I First f body Other Bound Present).
  - right;exists symbol;split;[exact(fr_keep _ _ Frame _ _ At)|exact Origin]. }
 rewrite(proj2(K a)_ _ _ Im Last f Other Bmid Pmid);exact(Hm _ _ _ I First f Other Bound Protected).
Qed.
Lemma header_new_sym:forall mi name kind decl origin,header_keeps(new_sym mi name kind decl origin).
Proof.
 intros mi name kind decl origin s id final Run f;unfold new_sym in Run.
 apply bind_inv in Run as[n[s1[Fresh Run]]];unfold fresh in Fresh;injection Fresh as <- <-.
 apply bind_inv in Run as[u[s2[Update Run]]];unfold upd in Update;injection Update as <- <-.
 injection Run as <- <-;reflexivity.
Qed.
Lemma hs_new_sym_bind:forall A mi name kind decl origin(k:nat->M A),
 origin<>OSource->kind<>KTag/\kind<>KUnit->
 (forall id,HSE id(k id))->HS(mbind(new_sym mi name kind decl origin)k).
Proof.
 intros A mi name kind decl origin k Origin Kind K;split.
 - apply pf_new_sym_bind;[exact Origin|exact Kind|intro;exact(proj1(K id))].
 - intros s value final I Run f Bound Protected;apply bind_inv in Run as[id[mid[New Rest]]].
   destruct(ok_new_sym mi name kind decl origin Origin Kind _ _ _ I New)as[Im Frame].
   assert(Id:id=st_next s).
   {unfold new_sym in New;apply bind_inv in New as[n[s1[Fresh New]]];unfold fresh in Fresh;injection Fresh as <- <-.
    apply bind_inv in New as[u[s2[Update New]]];injection New as <- _;reflexivity. }
   assert(PFnew:PF(new_sym mi name kind decl origin)).
   {apply pf_keeps;[apply ok_new_sym;assumption|unfold new_sym;keepst]. }
   rewrite(proj2(K id)_ _ _ Im Rest f ltac:(lia) ltac:(pose proof(fr_next _ _ Frame);lia)
    (header_protected_frame Frame(proj2 PFnew _ _ _ I New)Bound Protected)).
   exact(@header_new_sym mi name kind decl origin s id mid New f).
Qed.

Lemma header_keeps_of_rows:forall A(m:M A),keeps function_headers m->header_keeps m.
Proof.
 intros A m Keep s value final Run f;repeat rewrite function_header_lookup.
 rewrite (Keep _ _ _ Run);reflexivity.
Qed.
Lemma hs_with_vars:forall A init(m:M A),HS m->HS(with_vars init m).
Proof.
 intros A init m[PFm Keep];split;[apply pf_with_vars;exact PFm|].
 intros s value final I Run f Bound Protected;unfold with_vars in Run.
 apply bind_inv in Run as[s0[s1[R0 Run]]];injection R0 as <- <-.
 apply bind_inv in Run as[u[s2[R1 Run]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in Run as[r[s3[Body Run]]].
 apply bind_inv in Run as[u'[s4[R2 Run]]];unfold upd in R2;injection R2 as <- <-.
 injection Run as <- <-.
 assert(Im:Inv(s_vars init(List.length init)s))by(destruct I;constructor;assumption).
 exact(Keep _ _ _ Im Body f Bound Protected).
Qed.
Lemma hse_with_vars:forall A owner init(m:M A),HSE owner m->HSE owner(with_vars init m).
Proof.
 intros A owner init m[PFm Keep];split;[apply pfe_with_vars;exact PFm|].
 intros s value final I Run f Other Bound Protected;unfold with_vars in Run.
 apply bind_inv in Run as[s0[s1[R0 Run]]];injection R0 as <- <-.
 apply bind_inv in Run as[u[s2[R1 Run]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in Run as[r[s3[Body Run]]].
 apply bind_inv in Run as[u'[s4[R2 Run]]];unfold upd in R2;injection R2 as <- <-.
 injection Run as <- <-.
 assert(Im:Inv(s_vars init(List.length init)s))by(destruct I;constructor;assumption).
 exact(Keep _ _ _ Im Body f Other Bound Protected).
Qed.
Lemma hse_emit:forall owner body,HSE owner(emit_fn owner body).
Proof.
 intros;split;[apply pfe_emit|]. intros s value final I Run f Other Bound Protected.
 unfold emit_fn,upd in Run;injection Run as <- <-;reflexivity.
Qed.
Lemma hse_register:forall owner e inputs sg,
 HSE owner(register_reify_code(ROFunction owner)e inputs sg).
Proof.
 intros;split.
 - apply pfe_of_pf;apply pf_keeps;[unfold register_reify_code;okt|unfold register_reify_code;keepst].
 - intros s value final I Run f Other Bound Protected;destruct value.
   eapply register_other_header;[exact Run|];cbn[reify_owner_eqb];apply Nat.eqb_neq;exact Other.
Qed.
Lemma hse_mmap:forall A B owner(f:A->M B)xs,(forall x,HSE owner(f x))->HSE owner(mmap f xs).
Proof. intros A B owner f xs K;induction xs;cbn[mmap];[apply hse_of_hs,hs_ret|].
 apply hse_bind;[apply K|intro;apply hse_bind;[exact IHxs|intro;apply hse_of_hs,hs_ret]]. Qed.
Lemma hse_miter:forall A owner(f:A->M unit)xs,(forall x,HSE owner(f x))->HSE owner(miter f xs).
Proof. intros A owner f xs K;induction xs;cbn[miter];[apply hse_of_hs,hs_ret|].
 apply hse_bind;[apply K|intro;exact IHxs]. Qed.
Lemma hse_mfold:forall A B owner(f:A->B->M A)xs initial,(forall a x,HSE owner(f a x))->HSE owner(mfold f initial xs).
Proof. intros A B owner f xs;induction xs;intros initial K;cbn[mfold];[apply hse_of_hs,hs_ret|].
 apply hse_bind;[apply K|intro;apply IHxs;exact K]. Qed.
Lemma hse_bind_sym:forall A owner id(k:gsym->M A),
 (forall g,gs_id g=id->HSE owner(k g))->HSE owner(mbind(sym id)k).
Proof.
 intros A owner id k K;split;[split|].
 - intros s value final I Run;apply bind_inv in Run as[g[mid[Read Rest]]].
   unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:At;[|discriminate].
   injection Read as <- <-;exact(proj1(proj1(K got(iv_ids s I id got At)))_ _ _ I Rest).
 - intros s value final I Run key body Other Bound Present;apply bind_inv in Run as[g[mid[Read Rest]]].
   unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:At;[|discriminate].
   injection Read as <- <-;exact(proj2(proj1(K got(iv_ids s I id got At)))_ _ _ I Rest key body Other Bound Present).
 - intros s value final I Run key Other Bound Protected;apply bind_inv in Run as[g[mid[Read Rest]]].
   unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:At;[|discriminate].
   injection Read as <- <-;exact(proj2(K got(iv_ids s I id got At))_ _ _ I Rest key Other Bound Protected).
Qed.

Lemma hs_in_reify_code:forall A owner(m:M A),HS m->HS(in_reify_code owner m).
Proof.
 intros A owner m[PFm Keep];split.
 - split;[apply ok_reify_code;exact(proj1 PFm)|].
   intros s value final I Run key body Bound Present;unfold in_reify_code in Run.
   apply bind_inv in Run as[s0[s1[R0 Run]]];injection R0 as <- <-.
   apply bind_inv in Run as[u[s2[R1 Run]]];unfold upd in R1;injection R1 as <- <-.
   apply bind_inv in Run as[r[s3[Body Run]]].
   apply bind_inv in Run as[u'[s4[R2 Run]]];unfold upd in R2;injection R2 as <- <-.
   injection Run as <- <-.
   change(nassoc key(st_fns s3)=Some body).
   assert(Im:Inv(s_reify(fun r=>ReifyMetadata(Some owner)(rm_codes r)(rm_declarations r)(rm_constants r))s))
    by(destruct I;constructor;assumption).
   exact(proj2 PFm _ _ _ Im Body key body Bound Present).
 - intros s value final I Run key Bound Protected;unfold in_reify_code in Run.
   apply bind_inv in Run as[s0[s1[R0 Run]]];injection R0 as <- <-.
   apply bind_inv in Run as[u[s2[R1 Run]]];unfold upd in R1;injection R1 as <- <-.
   apply bind_inv in Run as[r[s3[Body Run]]].
   apply bind_inv in Run as[u'[s4[R2 Run]]];unfold upd in R2;injection R2 as <- <-.
   injection Run as <- <-.
   change(function_header s3 key=function_header s key).
   assert(Im:Inv(s_reify(fun r=>ReifyMetadata(Some owner)(rm_codes r)(rm_declarations r)(rm_constants r))s))
    by(destruct I;constructor;assumption).
   exact(Keep _ _ _ Im Body key Bound Protected).
Qed.
