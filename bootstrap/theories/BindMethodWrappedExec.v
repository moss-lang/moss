(* A named plain function supplies a method through the actual receiver adapter. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame
 InfoPreserve BodySoundCore BodyTy BodyTr MainRoot SignatureLeavesExec
 ProviderSig MethodEtaProvider BindMethodTrace.
Import ListNotations.
Module A := ExecutionModel.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].
Section Wrapped.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Hypothesis World : A.WorldOf b (BodySoundCore.W w R Fl).
Notation W := (BodySoundCore.W w R Fl).

Lemma defined_plain_provider_declaration : forall pv,
 nget (w_syms w) (gs_id pv) = Some pv -> is_defined_fn pv = true ->
 gs_recv pv = None /\
 exists name params result body, gs_decl pv = ADFn RPlain name params result (Some body).
Proof.
 intros pv Loaded Defined.
 unfold is_defined_fn in Defined; apply andb_true_iff in Defined as [Kind Body].
 assert (K : gs_kind pv = KFn) by (destruct (gs_kind pv); cbn [kind_eqb] in Kind; congruence).
 split.
 - destruct (load_world_recv b w Load _ _ Loaded) as [NoReceiver|Method]; [exact NoReceiver|congruence].
 - pose proof (signature_loaded_kind w R Fl b Load _ _ Loaded) as Model.
   unfold KindOK in Model; rewrite K in Model; cbn [kconv] in Model.
   destruct (gs_decl pv) as [tn|un|an aliasTy|tag tps pay|items ds|vn vt init|
     receiver name params result fnbody|cn items|fgn tps args outs bodies] eqn:Decl;
     cbn [A.kind_of] in Model; try discriminate Model.
   destruct receiver; cbn in Model; try discriminate Model.
   unfold fn_body in Body; rewrite Decl in Body.
   destruct fnbody as [body|]; [|discriminate Body].
   exists name,params,result,body; reflexivity.
Qed.

Theorem method_bind_wrapped_provider : forall fuel c e F C pa this m wanted ts s target pre sm final,
 method_bind_provider fuel c e (AEPath pa None) this m wanted ts s = Good ((target,pre,sm),final) ->
 Inv s -> InfoInv s -> st_w s = w -> CxRel w R Fl c e F C -> Later w P R Fl final ->
 A.MethProvider W P F C (AEPath pa None) (canonical_sig Fl e wanted)
   (tr Fl (canon e this)) target (fold_stmts pre) sm.
Proof.
 intros fuel c e F C pa this m wanted ts s target pre sm final Run Valid Cache Hw Caller Last.
 cbn [method_bind_provider] in Run.
 apply bind_inv in Run as [pv [expected [Expect Run]]].
 pose proof Expect as OriginalExpect.
 unfold expect_provider, expect_defined_fn in Expect.
 apply bind_inv in Expect as [found [resolved [Resolve Expect]]].
 destruct (esym_rel w R Fl b Load (cx_mod c) pa s found resolved Valid Hw
   (cr_mod_in _ _ _ _ _ _ _ Caller) Resolve) as [Same Read].
 subst resolved.
 destruct found as [g|]; [|exfalso;exact (err_not_good _ _ _ _ _ _ Expect)].
 destruct Read as [Path Loaded].
 destruct (is_defined_fn g) eqn:Defined; [|exfalso;exact (err_not_good _ _ _ _ _ _ Expect)].
 injection Expect as SameProvider SameState; subst g expected.
 destruct (defined_plain_provider_declaration pv Loaded Defined) as [NoReceiver [name [params [result [body Decl]]]]].
 apply bind_inv in Run as [mapping [matched [Match Run]]].
 apply bind_inv in Run as [adapter [adapted [Wrap Run]]].
 rewrite NoReceiver in Wrap; cbn [is_some] in Wrap.
 injection Run as SameTarget SamePre SameMap SameFinal; subst adapter pre mapping adapted.
 change (A.MethProvider W P F C (AEPath pa None) (canonical_sig Fl e wanted)
   (tr Fl (canon e this)) target (fun x => x) sm).
 eapply (plain_method_provider w (P:=P) R Fl b Load World c e F C pa this pv wanted name params result body
   fuel fuel s s sm matched target final);
   eassumption.
Qed.
End Wrapped.
