(* Actual attached-method providers preserve their source identity and context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound WorldSound ElabFrame InfoPreserve
 BodySoundCore BodyTr BodyTy CtxMerge Merge CanonTypes ProviderSig SourceSatisfiesGaps FnProviderDefinedExec
 MethodSelectExec MethodKind LoaderReceiver BindMethodTrace.
Import ListNotations.
Local Open Scope string_scope.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].
Module A := Audit.Elaboration.
Ltac attached_noerr H := exfalso; solve [repeat (cbn beta iota zeta in H;
 first [exact (err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as (? & ? & ? & H)])].
Section Attached.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Notation W := (BodySoundCore.W w R Fl).
Theorem expect_attached_provider : forall c e F C rp name this s pv final,
 expect_provider c e (AEField (AEPath rp None) name) this s = Good (pv,final) ->
 Inv s -> st_w s = w -> CxRel w R Fl c e F C ->
 final = s /\ exists receiver rn params result body,
 A.resolve_sym W (A.fr_mod F) rp = Some receiver /\
 A.Attached W receiver name (gs_id pv) /\ nget (w_syms w) (gs_id pv) = Some pv /\
 gs_decl pv = ADFn (RAttached rn) name params result (Some body) /\
 A.head (tr Fl (canon e this)) = Some receiver.
Proof using w R Fl b Load.
 intros c e F C rp name this s pv final Run Valid World Caller.
 unfold expect_provider in Run.
 apply bind_inv in Run as [found [resolved [Resolve Run]]].
 destruct (esym_rel w R Fl b Load (cx_mod c) rp s found resolved Valid World
   (cr_mod_in _ _ _ _ _ _ _ Caller) Resolve) as [Same Reading].
 subst resolved; destruct found as [rg|]; [|unfold expect_defined_fn in Run;discriminate Run].
 destruct Reading as [Path LoadedReceiver].
 destruct (kind_eqb (gs_kind rg) KUnit || kind_eqb (gs_kind rg) KTag) eqn:Kind;
   [|unfold expect_defined_fn in Run;discriminate Run].
 apply bind_inv in Run as [home [sh [Home Run]]]; unfold the_mod in Home; injection Home as <- <-.
 apply bind_inv in Run as [here [sl [Here Run]]]; unfold the_mod in Here; injection Here as <- <-.
 rewrite World in Run.
 apply bind_inv in Run as [attached [sa [Attachment Run]]].
 destruct (match assoc attached_eqb (gs_id rg,name) (gm_attached (get_mod w (gs_mod rg))) with
   | Some id => Some id | None => assoc attached_eqb (gs_id rg,name) (gm_attached (get_mod w (cx_mod c))) end)
   as [id|] eqn:Candidate in Attachment.
 2:{injection Attachment as <- <-;attached_noerr Run. }
 apply bind_inv in Attachment as [actual [ss [Symbol Attachment]]].
 injection Attachment as <- <-.
 pose proof Symbol as SymbolSaved.
 unfold sym in Symbol; destruct (nget (st_syms s) id) as [physical|] eqn:Dynamic;[|discriminate Symbol].
 injection Symbol as <- <-.
 destruct (fn_body physical) as [body|] eqn:Body;cbn [is_some] in Run;[|attached_noerr Run].
 destruct (Elab.head (canon e this)) as [head|] eqn:Head;[|attached_noerr Run].
 destruct (Nat.eqb head (gs_id rg)) eqn:Matches;[|attached_noerr Run].
 apply Nat.eqb_eq in Matches;subst head; injection Run as <- <-.
 assert (ReceiverDynamic : nget (st_syms s) (gs_id rg) = Some rg).
 { apply (iv_loader _ Valid b); [now rewrite World|now rewrite World]. }
 assert (Lookup : attached_call_lookup (cx_mod c) (gs_id rg) name s = Good (Some physical,s)).
 { unfold attached_call_lookup, the_mod, get, mbind, ret; cbv beta iota zeta.
   unfold sym; rewrite ReceiverDynamic; cbv beta iota zeta delta [ret]; rewrite World.
   destruct (assoc attached_eqb (gs_id rg,name) (gm_attached (get_mod w (gs_mod rg)))) as [owner|] eqn:Owner.
   - cbn in Candidate; injection Candidate as <-; cbv beta iota zeta delta [ret]; rewrite Dynamic; reflexivity.
   - destruct (assoc attached_eqb (gs_id rg,name) (gm_attached (get_mod w (cx_mod c)))) as [local|] eqn:Local;
       cbn in Candidate; [|discriminate Candidate].
     injection Candidate as <-; cbv beta iota zeta delta [ret]; rewrite Dynamic; reflexivity. }
 destruct (attached_call_lookup_exact w R Fl b Load (cx_mod c) (gs_id rg) rg name s (Some physical) s
   (cr_mod_in _ _ _ _ _ _ _ Caller) LoadedReceiver Valid World Lookup) as [_ [Attached Loaded]].
 pose proof (proj1 (lr_attached _ _ (load_world_reads b w R Fl Load) (gs_id rg) name (gs_id physical)) Attached)
   as (decl & rn & params & result & optionbody & Declared & Ast & Rest).
 change (A.w_decl W (gs_id physical) = Some decl) in Declared.
 assert (ReadDecl : A.w_decl W (gs_id physical) =
   Some (A.Decl (gm_path (get_mod w (gs_mod physical))) (gs_decl physical) (gs_assumes physical)))
   by exact (w_decl_read w R Fl _ _ Loaded).
 rewrite ReadDecl in Declared; injection Declared as <-.
 cbn [A.d_ast] in Ast.
 assert (Concrete : gs_decl physical = ADFn (RAttached rn) name params result (Some body)).
 { unfold fn_body in Body; rewrite Ast in Body; cbn in Body; subst optionbody;exact Ast. }
 split;[reflexivity|]. exists (gs_id rg),rn,params,result,body.
 split;[rewrite (cr_mod _ _ _ _ _ _ _ Caller);exact Path|].
 split;[exact Attached|]. split;[exact Loaded|]. split;[exact Concrete|].
 destruct (canon e this);cbn [Elab.head] in Head;try discriminate Head;
 injection Head as <-;reflexivity.
Qed.


Hypothesis CompleteWorld : A.WorldOf b W.
Hypothesis Finj : forall a b, Fl a = Fl b -> a = b.
Theorem match_attached_provider : forall fuel c e F C rp name receiver this pv rn params result body
 wanted s sm final,
 nget (w_syms w) (gs_id pv) = Some pv ->
 gs_decl pv = ADFn (RAttached rn) name params result (Some body) ->
 A.resolve_sym W (A.fr_mod F) rp = Some receiver ->
 A.Attached W receiver name (gs_id pv) ->
 A.head (tr Fl (canon e this)) = Some receiver ->
 match_fn_sig fuel c e pv wanted "method" s = Good (sm,final) ->
 Inv s -> InfoInv s -> st_w s = w -> CxRel w R Fl c e F C -> Later w P R Fl final ->
 A.MethProvider W P F C (AEField (AEPath rp None) name) (canonical_sig Fl e wanted)
   (tr Fl (canon e this)) (gs_id pv) (fun x => x) sm.
Proof using w P R Fl b Load CompleteWorld Finj.
 intros fuel c e F C rp name receiver this pv rn params result body wanted s sm final
 Loaded Decl Path Attached Head Run Valid Cache CallerWorld Caller Last.
 pose proof Run as Whole.
 destruct fuel as [|fuel]; [discriminate Run|]. cbn [match_fn_sig] in Run.
 apply bind_inv in Run as [info [si [Info Run]]].
 destruct (ok_fn_info_frame _ _ _ _ Valid Info) as [Ii Fi].
 apply bind_inv in Run as [pairs [sn [Needs Run]]].
 assert (On : ok (require_needs (cx_mod c) e pv info)) by (unfold require_needs,require_merges; okt).
 destruct (On _ _ _ Ii Needs) as [In Fn].
 apply bind_inv in Run as [got [signed [SignatureRun Run]]].
 assert (Osig : ok (fn_sig pv (tymap e) e None)) by (unfold fn_sig; okt).
 destruct (Osig _ _ _ In SignatureRun) as [Isig Fsig].
 destruct (sig_eqb_in e got e wanted) eqn:Equal; [|attached_noerr Run].
 apply bind_inv in Run as [u [event [Event Map]]].
 assert (Oevent : ok (record_reify_event (REProvider ety key (gs_id pv) (fi_needs info)
   (reify_sig e got) (reify_arguments e)))) by (unfold record_reify_event; apply ok_reify_update).
 destruct (Oevent _ _ _ Isig Event) as [Ie Fe].
 destruct (fn_info_satisfaction_gaps_any w (P:=P) R Fl b Load Finj CompleteWorld pv s info si si
   (cx_mod c) e pairs sn event sm final c F C Loaded Info (fr_refl si) Needs
   (fr_trans _ _ _ Fsig Fe) Map Valid Cache CallerWorld Ii Ie Last Caller) as [Cs [Context Satisfaction]].
 pose proof (match_fn_sig_signature w (P:=P) R Fl b Load (S fuel) c e F C pv wanted "method" s sm final
   (RAttached rn) name params result (Some body) Whole Valid CallerWorld Caller Loaded Decl Last) as Signature.
 assert (ReadDecl : A.w_decl W (gs_id pv) =
   Some (A.Decl (gm_path (get_mod w (gs_mod pv))) (gs_decl pv) (gs_assumes pv)))
   by exact (w_decl_read w R Fl _ _ Loaded).
 eapply A.MPAttached with (rg:=receiver) (att:=gs_id pv) (Ca:=Cs) (ns:=map (key_tr Fl) (fi_needs info)).
 - exact Path.
 - exact Attached.
 - exists rn,params,result,body; unfold A.ast_of; rewrite ReadDecl; cbn [A.d_ast]; now rewrite Decl.
 - exact Head.
 - exact Context.
 - exact Satisfaction.
 - rewrite (canonical_signature_fixed w R Fl c e F C wanted Caller); exact Signature.
Qed.

Theorem method_bind_attached_provider : forall fuel c e F C rp name this m wanted ts s target pre sm final,
 method_bind_provider fuel c e (AEField (AEPath rp None) name) this m wanted ts s =
   Good ((target,pre,sm),final) ->
 Inv s -> InfoInv s -> st_w s = w -> CxRel w R Fl c e F C -> Later w P R Fl final ->
 A.MethProvider W P F C (AEField (AEPath rp None) name) (canonical_sig Fl e wanted)
   (tr Fl (canon e this)) target (fold_stmts pre) sm.
Proof using w P R Fl b Load CompleteWorld Finj.
 intros fuel c e F C rp name this m wanted ts s target pre sm final Run Valid Cache World Caller Last.
 unfold method_bind_provider in Run.
 apply bind_inv in Run as [pv [selected [Expect Run]]].
 destruct (expect_attached_provider c e F C rp name this s pv selected Expect Valid World Caller)
   as [Same [receiver [rn [params [result [body [Path [Attached [Loaded [Decl Head]]]]]]]]]].
 subst selected.
 apply bind_inv in Run as [pairs [matched [Match Run]]].
 destruct (loaded_attached_receiver b w R Fl (gs_id pv) pv rn name params result (Some body) Load Loaded Decl)
   as [recv [gr [Receiver Metadata]]].
 rewrite Receiver in Run; cbn [is_some] in Run.
 apply bind_inv in Run as [id [supplied [Direct Return]]].
 injection Direct as <- <-; injection Return as <- <- <- <-.
 eapply match_attached_provider;
   [exact Loaded|exact Decl|exact Path|exact Attached|exact Head|exact Match|
    exact Valid|exact Cache|exact World|exact Caller|exact Last].
Qed.
End Attached.
