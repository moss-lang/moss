(* At top-level boundaries every generated lambda is complete. Thus any
   lambda reached by lower_all is already lowered and cannot get a marker. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv LoaderOrigin
 FnReadonly SymReadonly FnBound FnBoundEmit FnBoundSynth FnBoundProgram
 LambdaReadonly LambdaReadonlyEmit LambdaReadonlySynth FnPresentStable FnPresentSynth
 LambdaRows LambdaRowsPreserve LambdaRowsEmit LambdaRowsLower LambdaRowsSynth
 LambdaCompletion LambdaCompletionOps LambdaCompletionEmit LambdaCompletionSynth LambdaCompletionProgram.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Definition LCR{A}(m:M A):Prop:=CH m/\forall b w s a final,
 load_world b=Good w->Inv s->FnBound s->LambdaRows s->LambdaComplete s->st_w s=w->
 m s=Good(a,final)->LambdaRows final.
Arguments LCR:simpl never.
Lemma lcr_of_lc_ch:forall A(m:M A),LC m->CH m->LCR m.
Proof. intros A m L C;split;[exact C|]. intros b w s a final Load I Bound Rows Complete W E;exact(proj2 L _ _ _ I Bound Rows E). Qed.
Lemma lcr_ret:forall A(a:A),LCR(ret a).
Proof. intros;apply lcr_of_lc_ch;[apply lc_ret|apply ch_ret]. Qed.
Lemma lcr_bind:forall A B(m:M A)(k:A->M B),LCR m->(forall a,LCR(k a))->LCR(mbind m k).
Proof.
 intros A B m k[C M]K;split;[apply ch_bind;[exact C|intro;exact(proj1(K a))]|].
 intros b w s value final Load I Bound Rows Complete W E;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(proj1(proj1 C)_ _ _ I E1)as[Im Frame].
 assert(Bmid:FnBound mid)by exact(proj2(proj1(proj2 C))_ _ _ I Bound E1).
 assert(Rmid:LambdaRows mid)by exact(M b w _ _ _ Load I Bound Rows Complete W E1).
 assert(Cmid:LambdaComplete mid).
 {eapply lambda_complete_preserved;[exact I|exact Frame|exact(proj2(proj1 C)_ _ _ I E1)|
 exact(proj2(proj2 C)b w _ _ _ Load I Bound W E1)|exact Complete]. }
 exact(proj2(K a)b w mid value final Load Im Bmid Rmid Cmid ltac:(rewrite(fr_w _ _ Frame);exact W)E2).
Qed.
Lemma lcr_mmap:forall A B(f:A->M B)xs,(forall x,LCR(f x))->LCR(mmap f xs).
Proof. intros A B f xs F;induction xs;cbn[mmap];[apply lcr_ret|apply lcr_bind;[apply F|intro;apply lcr_bind;[exact IHxs|intro;apply lcr_ret]]]. Qed.
Lemma lcr_miter:forall A(f:A->M unit)xs,(forall x,LCR(f x))->LCR(miter f xs).
Proof. intros A f xs F;induction xs;cbn[miter];[apply lcr_ret|apply lcr_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma lcr_mfold:forall A B(f:A->B->M A)xs a,(forall a x,LCR(f a x))->LCR(mfold f a xs).
Proof. intros A B f xs;induction xs;intros acc F;cbn[mfold];[apply lcr_ret|apply lcr_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma lower_body_present_state:forall fuel g s value final body,
 nassoc(gs_id g)(st_fns s)=Some body->lower_body fuel g s=Good(value,final)->final=s.
Proof.
 intros[|fuel]g s value final body Present Run;[discriminate|].
 cbn[lower_body]in Run;apply bind_inv in Run as[done[mid[Read Run]]].
 cbn[lowered mbind get ret]in Read;rewrite Present in Read;cbn[is_some]in Read;injection Read as <- <-.
 injection Run;auto.
Qed.
Definition lambda_lower_one(fuel id:nat):M unit:=let! g:=sym id in
 if is_some(fn_body g)&&(kind_eqb(gs_kind g)KFn||kind_eqb(gs_kind g)KMethod)
 then lower_body fuel g else ret tt.
Lemma lcr_lower_one:forall fuel id,LCR(lambda_lower_one fuel id).
Proof.
 intros fuel id;split.
 - unfold lambda_lower_one;apply ch_bind_symbol;intros g Id.
   destruct(is_some(fn_body g)&&(kind_eqb(gs_kind g)KFn||kind_eqb(gs_kind g)KMethod));split.
   + apply pf_lower_body.
   + apply ch_lower_body.
   + apply pf_ret.
   + apply ch_at_of_ch,ch_ret.
 - intros b w s value final Load I Bound Rows Complete W Run;unfold lambda_lower_one in Run.
   apply bind_inv in Run as[g[mid[Symbol Run]]];unfold sym in Symbol.
   destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[injection Symbol as <- <-|discriminate].
   assert(Id:gs_id got=id)by exact(iv_ids s I id got Lookup).
   destruct(is_some(fn_body got)&&(kind_eqb(gs_kind got)KFn||kind_eqb(gs_kind got)KMethod));[|injection Run as <- <-;exact Rows].
   destruct(gs_origin got)eqn:Origin;
   try solve[eapply(proj2(lc_lower_body fuel got));[exact I|exact Bound|exact Rows|rewrite Id;exact Lookup|rewrite Origin;discriminate|exact Run]].
   destruct(Complete id got Lookup Origin)as[body Present];rewrite <-Id in Present.
   pose proof(@lower_body_present_state fuel got s value final body Present Run)as Eq;subst final;exact Rows.
Qed.
Lemma lcr_lower_all:LCR lower_all.
Proof.
 split;[exact ch_lower_all|]. intros b w s value final Load I Bound Rows Complete W Run;unfold lower_all in Run.
 apply bind_inv in Run as[snapshot[mid[Read Run]]];injection Read as <- <-.
 change(miter(fun mi=>miter(lambda_lower_one ELAB_FUEL)(module_function_ids(get_mod(st_w s)mi)))(w_order(st_w s))s=Good(value,final))in Run.
 assert(Loop:LCR(miter(fun mi=>miter(lambda_lower_one ELAB_FUEL)(module_function_ids(get_mod(st_w s)mi)))(w_order(st_w s)))).
 {apply lcr_miter;intro;apply lcr_miter;intro;apply lcr_lower_one. }
 exact(proj2 Loop b w _ _ _ Load I Bound Rows Complete W Run).
Qed.
#[local] Hint Resolve LambdaCompletionProgram.kf_entry_zero_width_history
 LambdaCompletionProgram.ks_entry_zero_width_history
 LambdaCompletionProgram.kf_check_main_signature_history
 LambdaCompletionProgram.ks_check_main_signature_history : keepsdb.
Create HintDb lcrdb.
#[local]Hint Resolve lcr_ret lcr_lower_all:lcrdb.
Ltac lcrt:=repeat(fintros;first[solve[auto with lcrdb]|lazymatch goal with
 | |-LCR(mbind _ _)=>apply lcr_bind
 | |-LCR(mmap _ _)=>apply lcr_mmap
 | |-LCR(miter _ _)=>apply lcr_miter
 | |-LCR(mfold _ _ _)=>apply lcr_mfold
 | |-LCR ?m=>first[solve[apply lcr_of_lc_ch;[lct|cht]]|
    match m with context[match ?x with _=>_ end]=>destruct x end|
    let h:=head_of m in progress unfold h]
 end]).
Theorem lcr_elab_program:LCR elab_program.
Proof. unfold elab_program;lcrt. Qed.
