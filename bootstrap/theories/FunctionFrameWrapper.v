(* The real eta writer creates one function row with a usable own frame. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame ElabInv
 FnReadonly SymReadonly FnBound FnBoundEmit FnBoundSynth FnPresentStable
 ReifyHeader ReifyHeaderReadonly ReifyHeaderStable ReifyHeaderEmit ReifyHeaderSynth
 ReifyHeaderPresent LambdaOrigin EtaHeader ReifyFrameReady FunctionFrameHistory FunctionFrameOps.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL fn_info].
Lemma ff_wrapper:forall fuel provider,FF(wrapper fuel provider).
Proof.
 intros[|fuel]provider;[apply ff_keeps;unfold wrapper;[okt|keepst|keepst]|].
 split;[apply hp_of_hs,hs_wrapper|];split;[apply bf_wrapper|].
 intros initial value final I Bound Run;pose proof Run as WholeRun.
 cbn[wrapper wrapper_cached]in Run;apply bind_inv in Run as[s0[s1[Read Run]]];injection Read as <- <-.
 destruct(nget(st_wrappers initial)(gs_id provider))as[old|]eqn:Found.
 {injection Run as <- <-;intros f body Present Missing;congruence. }
 apply bind_inv in Run as[physical[physicalState[Physical Run]]].
 unfold sym in Physical;destruct(nget(st_syms initial)(gs_id provider))as[stored|];[|discriminate].
 injection Physical as <- <-;unfold wrapper_create in Run.
 apply bind_inv in Run as[info[s2[Info Run]]].
 apply bind_inv in Run as[sm[s3[Transfer Run]]].
 apply bind_inv in Run as[id[s4[New Run]]].
 apply bind_inv in Run as[th[s5[Theta Run]]].
 apply bind_inv in Run as[u[s6[Register Run]]];destruct u.
 apply bind_inv in Run as[u[s7[Emit Run]]];destruct u.
 apply bind_inv in Run as[u[s8[Cache Run]]];destruct u;injection Run as <- <-.
 assert(Ksm:keeps st_fns(smap_at(fi_env info)(fi_env info)(map(fun k=>(k,k))(fi_needs info))))by(unfold smap_at;keepst).
 assert(Knew:keeps st_fns(new_sym(gs_mod stored)(gs_name stored++"$recv")KFn(gs_decl stored)OLambda))by(unfold new_sym;keepst).
 assert(Ktheta:keeps st_fns(call_theta(fi_env info)stored))by(unfold call_theta;keepst).
 assert(Kregister:keeps st_fns(register_reify_code(ROFunction id)(fi_env info)(fi_needs info)None))by(unfold register_reify_code;keepst).
 assert(Rows:st_fns s6=st_fns initial).
 {rewrite(Kregister _ _ _ Register),(Ktheta _ _ _ Theta),(Knew _ _ _ New),(Ksm _ _ _ Transfer),(@kf_fn_info_frame _ _ _ _ Info);reflexivity. }
 destruct(wrapper_created_header _ _ _ _ _ Found WholeRun)as(pv&inf&theta&transfer&Header&Own&Eta).
 intros f body Present Missing;destruct(Nat.eq_dec f id)as[->|Other].
 - split.
   + exists OLambda;right;left;exists(S fuel),provider,initial,s8.
     repeat first[assumption|reflexivity|lazymatch goal with |- _ /\ _=>split end].
   + eapply function_header_eta_ready;[exact Header|exact Eta].
 - unfold upd in Cache;injection Cache as <-.
   unfold emit_fn,upd in Emit;injection Emit as <-.
   cbn[s_wrappers s_fns st_fns nassoc assoc]in Present.
   assert(Eq:Nat.eqb f id=false)by(apply Nat.eqb_neq;exact Other).
   rewrite Eq,Rows,Missing in Present;discriminate.
Qed.
