(* The real plain-function-to-method provider branch, including cached eta. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame WorldSound
 InfoPreserve BodySound BodyTy BodyTr ProviderSig FnProviderDefinedExec
 WrapperEtaExec WrapperArity.
Import ListNotations.
Local Open Scope string_scope.
Module A:=ExecutionModel.Elaboration.
Section Method.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySound.W w R Fl).
Theorem plain_method_provider:forall expectc e F C pa rt pv wanted n ps ret body
    matchfuel wrapfuel s sp sm matched id final,
 expect_provider expectc e(AEPath pa None)rt s=Good(pv,sp)->
 gs_decl pv=ADFn RPlain n ps ret(Some body)->
 match_fn_sig matchfuel expectc e pv wanted "method" sp=Good(sm,matched)->
 wrapper wrapfuel pv matched=Good(id,final)->
 Inv s->InfoInv s->st_w s=w->CxRel w R Fl expectc e F C->Later w P R Fl final->
 A.MethProvider(BodySound.W w R Fl)P F C(AEPath pa None)
   (canonical_sig Fl e wanted)(tr Fl(canon e rt))id(fun x=>x)sm.
Proof.
 intros c e F C pa rt pv wanted n ps ret body mf wf s sp sm matched id final
   Expect Decl Match Wrap Iv Cache Hw Caller Last.
 unfold expect_provider,expect_defined_fn in Expect.
 apply bind_inv in Expect as(found&sr&Resolve&Expect).
 destruct(esym_rel w R Fl b Hload(cx_mod c)pa s found sr Iv Hw
   (cr_mod_in _ _ _ _ _ _ _ Caller)Resolve)as[-> Reading].
 destruct found as[g|];[|exfalso;exact(err_not_good _ _ _ _ _ _ Expect)].
 destruct(is_defined_fn g)eqn:Defined;[|exfalso;exact(err_not_good _ _ _ _ _ _ Expect)].
 injection Expect as SameProvider SameState;subst g sp.
 destruct Reading as[Path Loaded].
 assert(SemPath:A.resolve_sym(BodySound.W w R Fl)(A.fr_mod F)pa=Some(gs_id pv)).
 {rewrite(cr_mod _ _ _ _ _ _ _ Caller);exact Path. }
 destruct(proj1(okI_match_fn_sig mf c e pv wanted "method")_ _ _ Iv Match)as[Im Fm].
 pose proof(proj2(okI_match_fn_sig mf c e pv wanted "method")_ _ _ Iv Cache Match)as Cm.
 destruct(proj1(okI_wrapper wf pv)_ _ _ Im Wrap)as[If Fw].
 pose proof(later_back w P R Fl _ _ Fw Last)as Mid.
 pose proof(match_defined_provider w R Fl b Hload Hworld(@A.wo_fld b _ Hworld)
   mf c e F C pa pv n ps ret body wanted "method" s sm matched Loaded Decl SemPath
   Match Iv Cache Caller Mid)as Provider.
 destruct(actual_wrapper_eta w R Fl b Hload Hworld wf pv matched id final Loaded Wrap Im Cm
   ltac:(rewrite(fr_w _ _ Fm);exact Hw)Last)as[Cg[ns[Context[Fresh Body]]]].
 eapply A.MPWrapped with(g:=gs_id pv)(Cg:=Cg)(nsg:=ns)(n:=List.length(fn_params pv));
   [exact Provider|exact Context| |exact Fresh|exact Body].
 unfold canonical_sig;cbn[A.sg_params];rewrite map_length.
 exact(match_fn_sig_arity _ _ _ _ _ _ _ _ _ Match).
Qed.
End Method.
