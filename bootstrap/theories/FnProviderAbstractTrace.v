(* The actual abstract-provider branch selects its available target and
   returns its proven forwarder; no signature/body callback is assumed. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame FwdInvariant
 ForwarderSymbol FinalForwarders ForwarderHistory.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Theorem expect_abstract_provider_trace:forall fuel c e pa s provider final target,
 Inv s->FwdInv s->resolve_sym(cx_mod c)pa s=Good(Some target,s)->
 gs_kind target=KFn->fn_body target=None->
 expect_fn_provider fuel c e(AEPath pa None)s=Good(provider,final)->
 (exists sg,nget(en_fns e)(gs_id target)=Some sg)/\
 gs_origin provider=OForwarder(gs_id target)/\
 nget(st_syms final)(gs_id provider)=Some provider.
Proof.
 intros[|f]c e pa s provider final target I FI Resolve Kind NoBody E;[discriminate|].
 cbn[expect_fn_provider]in E;apply bind_inv in E as[found[mid[Read E]]].
 rewrite Resolve in Read;injection Read as <- <-;rewrite Kind,NoBody in E.
 cbn[kind_eqb is_some andb negb]in E.
 destruct(nget(en_fns e)(gs_id target))as[sg|]eqn:Available;[|discriminate E].
 split;[exists sg;reflexivity|]. exact(forwarder_return_symbol _ _ _ _ _ I FI E).
Qed.
Theorem abstract_provider_final_facts:forall w P fuel c e pa s provider final target,
 Inv s->FwdInv s->FinalForwarders w P final->
 resolve_sym(cx_mod c)pa s=Good(Some target,s)->gs_kind target=KFn->fn_body target=None->
 expect_fn_provider fuel c e(AEPath pa None)s=Good(provider,final)->
 (exists sg,nget(en_fns e)(gs_id target)=Some sg)/\gs_id provider<>0/\
 exists tg,LoadedAbstract w tg /\gs_id tg=gs_id target/\
 gs_decl provider=ForwarderTrace.forwarder_decl tg/\gs_mod provider=gs_mod tg/\gs_recv provider=None/\
 Audit.Elaboration.body_of P(gs_id provider)=
 Some(XCallA(gs_id target)(map XVar(seq 0(List.length(fn_params provider))))).
Proof.
 intros w P fuel c e pa s provider final target I FI Final Resolve Kind NoBody E.
 destruct(expect_abstract_provider_trace _ _ _ _ _ _ _ _ I FI Resolve Kind NoBody E)as[Available[Origin Lookup]].
 split;[exact Available|]. exact(Final _ _ _ Lookup Origin).
Qed.
