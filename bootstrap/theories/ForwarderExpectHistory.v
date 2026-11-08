(* Real provider resolution supplies the loaded-target premise of GHF. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv
 FnReadonly SymReadonly FnBound FnBoundSynth FnPresentStable FnPresentSynth
 ResolveLoadedAny ForwarderHistory ForwarderHistoryOps ForwarderHistoryStep.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Lemma ks_expect_defined_provider:forall mi y,keeps st_syms(expect_defined_fn mi y).
Proof. intros;unfold expect_defined_fn;keepst. Qed.
Lemma gh_expect_fn_provider_step:forall fuel,(forall tg,GHF fuel tg)->forall c e y,GH(expect_fn_provider(S fuel)c e y).
Proof.
 intros fuel Forward c e y;split;[apply pf_expect_fn_provider|]. split;[apply bf_expect_fn_provider|].
 intros b w s g final Load I Bound World E.
 destruct y;cbn[expect_fn_provider]in E;
 try solve[apply forwarder_history_same_symbols;eapply ks_expect_defined_provider;exact E].
 match goal with a:sapp|-_=>destruct a end;
 try solve[apply forwarder_history_same_symbols;eapply ks_expect_defined_provider;exact E].
 apply bind_inv in E as[found[mid[Resolve E]]].
 destruct found as[tg|].
 - destruct(resolve_sym_loaded_any _ _ _ _ _ _ _ Load I World Resolve)as[-> Loaded].
   destruct(kind_eqb(gs_kind tg)KFn&&negb(is_some(fn_body tg)))eqn:Abstract.
   + apply andb_true_iff in Abstract as[Kind NoBody]. apply kind_eqb_spec in Kind.
     apply negb_true_iff in NoBody;destruct(fn_body tg)eqn:Body;[discriminate|].
     destruct(is_some(nget(en_fns e)(gs_id tg)))eqn:Available;[|discriminate E].
     assert(Target:LoadedAbstract w tg).
     {split;[exact Loaded|]. split;[exact Kind|]. eapply loaded_abstract_from_kind;eassumption. }
     exact(proj2(proj2(Forward tg))b w _ _ _ Load I Bound World Target E).
   + apply forwarder_history_same_symbols;eapply ks_expect_defined_provider;exact E.
 - assert(Readonly:mid=s).
   {assert(K:keeps(fun s=>s)(resolve_sym(cx_mod c)p))by(unfold resolve_sym,rpath;keepst).
    exact(K _ _ _ Resolve). }
   subst mid;apply forwarder_history_same_symbols;eapply ks_expect_defined_provider;exact E.
Qed.
