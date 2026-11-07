(* Fresh receiver adapters have exactly the recorded eta-call shape. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv
 FnReadonly SymReadonly ReifyHeader ReifyHeaderReadonly ReifyEta.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL fn_info smap_at].
Lemma new_sym_lookup:forall mi name kind decl origin s id final,
 new_sym mi name kind decl origin s=Good(id,final)->
 nget(st_syms final)id=Some(GSym id mi name kind decl None[]origin).
Proof.
 intros mi name kind decl origin s id final Run;unfold new_sym in Run.
 apply bind_inv in Run as[value[mid[Fresh Run]]];unfold fresh in Fresh;injection Fresh as <- <-.
 apply bind_inv in Run as[u[next[Update Run]]];unfold upd in Update;injection Update as <- <-.
 injection Run as <- <-;cbn[s_syms st_syms];apply nget_nset_same.
Qed.
Lemma source_eta_callee_header:forall source f owner scope inputs symbol callee th sm args,
 function_header source f=Some(owner,scope,inputs,None)->
 nget(st_syms source)f=Some symbol->gs_origin symbol=OLambda->
 nassoc f(st_fns source)=Some(XCallD callee th sm args)->
 source_eta_callee source f=Some(callee,args).
Proof.
 intros source f owner scope inputs symbol callee th sm args Header Symbol Origin Body.
 unfold function_header in Header.
 destruct(assoc reify_owner_eqb(ROFunction f)(rm_codes(st_reify source)))as[code|]eqn:Code;[|discriminate].
 cbn[option_map code_header]in Header;injection Header as Owner Scope Inputs Signature.
 unfold nassoc in Body;unfold source_eta_callee;rewrite Code,Symbol,Body,Signature,Origin;reflexivity.
Qed.
Theorem wrapper_create_header:forall provider s id final,
 wrapper_create provider s=Good(id,final)->
 exists info th sm,
 function_header final id=Some(ROFunction id,reify_scope_of(fi_env info),fi_needs info,None)/\
 nassoc id(st_fns final)=Some(XCallD(gs_id provider)th sm(map XVar(seq 1(List.length(fn_params provider)))))/\
 source_eta_callee final id=Some(gs_id provider,map XVar(seq 1(List.length(fn_params provider)))).
Proof.
 intros provider s id final Run;unfold wrapper_create in Run.
 apply bind_inv in Run as[info[s2[Info Run]]].
 apply bind_inv in Run as[sm[s3[Transfer Run]]].
 apply bind_inv in Run as[key[s4[New Run]]].
 apply bind_inv in Run as[th[s5[Theta Run]]].
 apply bind_inv in Run as[u[s6[Register Run]]];destruct u.
 apply bind_inv in Run as[u[s7[Emit Run]]];destruct u.
 apply bind_inv in Run as[u[s8[Cache Run]]];destruct u.
 injection Run as <- <-.
 assert(Symbol:nget(st_syms s4)key=Some(GSym key(gs_mod provider)(gs_name provider++"$recv")KFn(gs_decl provider)None[]OLambda))
 by(exact(new_sym_lookup _ _ _ _ _ _ _ _ New)).
 assert(ThetaKeeps:keeps st_syms(call_theta(fi_env info)provider)).
 {unfold call_theta;keepst. }
 assert(Symbol5:nget(st_syms s5)key=Some(GSym key(gs_mod provider)(gs_name provider++"$recv")KFn(gs_decl provider)None[]OLambda)).
 {rewrite(ThetaKeeps _ _ _ Theta);exact Symbol. }
 unfold upd in Cache;injection Cache as <-.
 unfold emit_fn,upd in Emit;injection Emit as <-.
 assert(Header:function_header s6 key=Some(ROFunction key,reify_scope_of(fi_env info),fi_needs info,None)).
 {unfold register_reify_code,upd in Register;injection Register as <-.
  unfold function_header;cbn[s_reify st_reify assoc rm_codes reify_owner_eqb];rewrite Nat.eqb_refl;reflexivity. }
 exists info,th,sm;split;[exact Header|];split.
 - cbn[s_wrappers st_fns s_fns nassoc assoc];rewrite Nat.eqb_refl;reflexivity.
 - eapply source_eta_callee_header with(symbol:=GSym key(gs_mod provider)(gs_name provider++"$recv")KFn(gs_decl provider)None[]OLambda);[exact Header| |reflexivity|].
   + unfold register_reify_code,upd in Register;injection Register as <-;exact Symbol5.
   + cbn[s_wrappers st_fns s_fns nassoc assoc];rewrite Nat.eqb_refl;reflexivity.
Qed.

Theorem wrapper_created_header:forall fuel provider s id final,
 nget(st_wrappers s)(gs_id provider)=None->wrapper fuel provider s=Good(id,final)->
 exists physical info th sm,
 function_header final id=Some(ROFunction id,reify_scope_of(fi_env info),fi_needs info,None)/\
 nassoc id(st_fns final)=Some(XCallD(gs_id physical)th sm(map XVar(seq 1(List.length(fn_params physical)))))/\
 source_eta_callee final id=Some(gs_id physical,map XVar(seq 1(List.length(fn_params physical)))).
Proof.
 intros[|fuel]provider s id final Absent Run;[discriminate|].
 cbn[wrapper wrapper_cached]in Run.
 apply bind_inv in Run as[s0[s1[Read Run]]];injection Read as <- <-;rewrite Absent in Run.
 apply bind_inv in Run as[physical[mid[Physical Run]]].
 unfold sym in Physical;destruct(nget(st_syms s)(gs_id provider))as[stored|];[|discriminate].
 injection Physical as <- <-.
 destruct(wrapper_create_header _ _ _ _ Run)as(info&th&sm&Header&Body&Eta).
 exists stored,info,th,sm;auto.
Qed.
