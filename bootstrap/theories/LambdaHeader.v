(* Anonymous functions retain their real checked code-entry metadata. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv
 LambdaTrace FnReadonly FnPresentStable FnPresentEmit FnPresentSynth
 ReifyHeader ReifyHeaderReadonly ReifyHeaderStable ReifyHeaderEmit ReifyHeaderSynth.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma hs_lambda_generated_body:forall fuel c e ps body sg this pid lets,
 HS(lambda_generated_body fuel c e ps body sg this pid lets).
Proof.
 intros;unfold lambda_generated_body;apply hs_in_reify_code,hs_with_vars.
 apply hs_bind.
 - apply hs_mmap;intros[[n v]t];apply hs_bind.
   + apply hs_keeps;[apply pf_keeps;[okt|keepst]|apply header_keeps_of_rows;keepst].
   + intro;apply hs_ret.
 - intro locals;apply hs_bind;[apply hs_synth_block|intros[[compiled t]bt];apply hs_ret].
Qed.
Lemma new_lambda_header_protected:forall mi name decl s pid after e needs sg registered,
 new_sym mi name KFn decl OLambda s=Good(pid,after)->
 register_reify_code(ROFunction pid)e needs(Some sg)after=Good(tt,registered)->
 header_protected registered pid/\pid<st_next registered.
Proof.
 intros mi name decl s pid after e needs sg registered New Register.
 unfold register_reify_code,upd in Register;injection Register as <-.
 unfold new_sym in New;apply bind_inv in New as[id[s1[Fresh New]]].
 unfold fresh in Fresh;injection Fresh as <- <-.
 apply bind_inv in New as[u[s2[Update New]]];unfold upd in Update;injection Update as <- <-.
 injection New as <- <-;split.
 - right;exists(GSym(st_next s)mi name KFn decl None[]OLambda);split.
   + cbn[s_reify st_syms s_syms];apply nget_nset_same.
   + reflexivity.
 - cbn[s_reify st_next s_syms];lia.
Qed.
Theorem lambda_provider_header:forall fuel c e ps rt body bound sg this what s pid pre sm final,
 lambda_provider fuel c e ps rt body bound sg this what s=Good((pid,pre,sm),final)->Inv s->
 exists le lets compiled,
 function_header final pid=Some(ROFunction pid,reify_scope_of(lambda_own_env ps sg this le lets),
 all_keys le,Some(reify_sig(lambda_own_env ps sg this le lets)(lambda_own_signature ps sg this)))/\
 nassoc pid(st_fns final)=Some compiled.
Proof.
 intros fuel c e ps rt body bound sg this what s pid pre sm final Run I.
 destruct(lambda_provider_stages _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ Run I)
 as(f&le&lets&captureStart&beforePid&afterPid&registered&compiled&bodyFinal&emitted&cached&
 Fuel&Capture&Ic&Fc&New&Register&Body&Emit&Cache&Transfer&Ip&Ir).
 destruct(@new_lambda_header_protected _ _ _ _ _ _ _ _ _ _ New Register)as[Protected Bound].
 pose proof(proj2(@hs_lambda_generated_body f c _ ps body sg this pid lets)
  _ _ _ Ir Body pid Bound Protected)as Header.
 assert(Kept:keeps function_headers(smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le)))).
 {unfold smap_at;keepst. }
 assert(Functions:keeps st_fns(smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le)))).
 {unfold smap_at;keepst. }
 exists le,lets,compiled;split.
 - rewrite(function_header_lookup final pid),(Kept _ _ _ Transfer),<-function_header_lookup.
   unfold upd in Cache;injection Cache as <-.
   unfold emit_fn,upd in Emit;injection Emit as <-.
   rewrite <-(register_function_header _ _ _ _ _ _ Register);exact Header.
 - rewrite(Functions _ _ _ Transfer);unfold upd in Cache;injection Cache as <-.
   unfold emit_fn,upd in Emit;injection Emit as <-.
   cbn[s_info st_fns s_fns nassoc assoc];rewrite Nat.eqb_refl;reflexivity.
Qed.
