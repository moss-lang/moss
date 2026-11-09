(* ChainNewOk (PublicChainCore). Its former first clause (every key the
   result selection dr fills is the run-time key of one of C''s new
   provisions) is true by construction since the caller-keyed install
   (Audit.result_selection; PublicChainCore.selection_news), so the former
   static premise ChainKeysOk is gone. What remains is the realization of
   the new provisions after the application, ChainRealizeOk. *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBodySafety PublicTypeFacts PublicCallRealize PublicBodySlice PublicCopyApply PublicPlatformApply
  PublicLinkResults PublicFamilyAgree PublicChainCore.
From NM Require PublicFormation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Module ChainNew (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module CC := PublicChainCore.ChainCore Prims I0.
Module LR := CC.LR.
Module CA := CC.CA.
Module S := CC.S.

Section New.
Variable W : A.world.
Variables (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Variable state_ok : heap_types -> Prims.mstate -> Prop.
Let P := program_of Prog argv read.

Notation ClosureOk := (S.ClosureOk W Prog argv read state_ok).

(* PREMISE (T15/T16): the new provisions of the caller's result realized
   after the application. At an application of F (AppPremises) whose
   closure is a copy chain (positions rms) down to a linked root h0, the
   selection files each position of F's declared result under the
   caller's key for it (ks), read from the root body's source through the
   chain's positions; the caller's new provisions are then realized. *)
Definition ChainRealizeOk : Prop := forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st code sms dc rms gfx h0 gf0 gg dg n ps a r bodies Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0
    d1 d2 d3 g3 st3 H3 j',
  S.Pre W Prog argv read state_ok rr F C d types H g st ->
  lookup_provision (IFun u) d = Some (FunctorClosure code sms dc rms) ->
  ClosureOk rr H u gfx (FunctorClosure code sms dc rms) ->
  S.FixedMeans W u d fx gfx ->
  CA.CopyChain W u gfx sms rms h0 gf0 [map (fun k => (A.rkey_of Cg k, A.rkey_of Ch0 k)) (S.linked_needs nsh0 nsg)] [] ->
  ClosureOk rr H h0 gf0 (FunctorClosure code [map (fun k => (A.rkey_of Cg k, A.rkey_of Ch0 k)) (S.linked_needs nsh0 nsg)] dc []) ->
  S.LinkData W Prog h0 gg code dg n ps a r bodies Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 ->
  select_provisions d sm = Some d1 ->
  adapt_provisions d1 sms = Some d2 ->
  S.I.eval_bindings j' P (application_environment P d code th (append_environment d2 dc)) [] st binds0 = inl (d3, g3, st3) ->
  heap_extends H H3 -> S.Pre W Prog argv read state_ok rr Fb Cb d3 [] H3 g3 st3 ->
  forall dr, select_provisions d3 (result_selection (map (A.rkey_of C') ks) rms (S.link_sources (A.link_site Cg Cb outs') outs' gen ks')) = Some dr ->
  forall news, A.c_provisions C' = (news ++ A.c_provisions C)%list ->
  CC.EnvOkOn W Prog argv read state_ok (fun k => In k (map fst news)) rr C' (append_environment (LR.result_of d3 outs gensof dr) d) H3.

Theorem chain_new_ok : ChainRealizeOk -> CC.ChainNewOk W Prog argv read state_ok.
Proof. intros Real. exact Real. Qed.

End New.
End ChainNew.
