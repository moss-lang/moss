(* Every remaining successful ordinary binding is an abstract-to-defined
   functor closure. Other syntax and kinds are rejected or already proved. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore
 BodyTy SignatureLeavesExec BindSequence BindCopyDispatch BindFnDispatch BindLambdaDispatch BindMethodAllDispatch InfoPreserve.
Import ListNotations.
Local Open Scope list_scope.
Module A := Audit.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements nat_to_string nat_to_string_go].
Ltac residual_noerr H := exfalso; solve [repeat (cbn beta iota zeta in H;
 first [exact (err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as (? & ? & ? & H)])].
Section Remaining.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Hypothesis World : A.WorldOf b (BodySoundCore.W w R Fl).
Hypothesis Finj : forall a b, Fl a = Fl b -> a = b.
Notation W := (BodySoundCore.W w R Fl).

Lemma loaded_plain_function : forall g,
 nget (w_syms w) (gs_id g) = Some g -> gs_kind g = KFn ->
 exists name params result body, gs_decl g = ADFn RPlain name params result body.
Proof.
 intros g Loaded Kind.
 pose proof (signature_loaded_kind w R Fl b Load _ _ Loaded) as Model.
 unfold KindOK in Model; rewrite Kind in Model; cbn [kconv] in Model.
 destruct (gs_decl g) as [tn|un|an aliasTy|tag tps pay|items ds|vn vt init|
   receiver name params result fnbody|cn items|fgn tps args outs bodies] eqn:Decl;
   cbn [A.kind_of] in Model; try discriminate Model.
 destruct receiver; cbn in Model; try discriminate Model.
 exists name,params,result,fnbody; reflexivity.
Qed.

Lemma expect_fn_provider_source : forall fuel c e F C y s pv final,
 expect_fn_provider fuel c e y s = Good (pv,final) ->
 Inv s -> st_w s = w -> CxRel w R Fl c e F C ->
 exists pg u name params result body,
 y = AEPath pg None /\ A.resolve_sym W (A.fr_mod F) pg = Some u /\
 A.ast_of W u = Some (ADFn RPlain name params result body).
Proof.
 intros [|fuel] c e F C y s pv final Run Valid Hw Caller; [discriminate Run|].
 destruct y; cbn [expect_fn_provider] in Run;
 try (unfold expect_defined_fn in Run; discriminate Run).
 match goal with a:sapp |- _ => destruct a end;
 try (unfold expect_defined_fn in Run; discriminate Run).
 apply bind_inv in Run as [found [resolved [Resolve Run]]].
 destruct (esym_rel w R Fl b Load (cx_mod c) p s found resolved Valid Hw
   (cr_mod_in _ _ _ _ _ _ _ Caller) Resolve) as [Same Reading].
 subst resolved.
 destruct found as [g|].
 2:{ unfold expect_defined_fn in Run; apply bind_inv in Run as [again [sr [Again Run]]].
     rewrite Resolve in Again; injection Again as <- <-; residual_noerr Run. }
 destruct Reading as [Path Loaded].
 assert (Kind : gs_kind g = KFn).
 { destruct (kind_eqb (gs_kind g) KFn && negb (is_some (fn_body g))) eqn:Abstract.
   - apply andb_true_iff in Abstract as [K _].
     destruct (gs_kind g); cbn [kind_eqb] in K; congruence.
   - unfold expect_defined_fn in Run; apply bind_inv in Run as [again [sr [Again Run]]].
     rewrite Resolve in Again; injection Again as <- <-.
     destruct (is_defined_fn g) eqn:Defined; [|residual_noerr Run].
     unfold is_defined_fn in Defined; apply andb_true_iff in Defined as [K _].
     destruct (gs_kind g); cbn [kind_eqb] in K; congruence. }
 destruct (loaded_plain_function g Loaded Kind) as (name & params & result & body & Decl).
 exists p,(gs_id g),name,params,result,body; split; [reflexivity|].
 split; [rewrite (cr_mod _ _ _ _ _ _ _ Caller);exact Path|].
 change (A.ast_of (W_read w R Fl) (gs_id g) = Some (ADFn RPlain name params result body)).
 unfold A.ast_of; rewrite (w_decl_read w R Fl _ _ Loaded); cbn [option_map A.d_ast].
 now rewrite Decl.
Qed.


Lemma nonmethod_binding_bare : forall fuel c e sp y s xs e' c' final,
 check_bind (S fuel) c e sp y s = Good ((xs,e',c'),final) ->
 method_bind sp = false -> exists pa, sp = Spec (Some pa) None None.
Proof.
 intros fuel c e sp y s xs e' c' final Run NonMethod.
 destruct sp as [[pa|] [dn|] [app|]]; cbn [method_bind] in NonMethod; try discriminate NonMethod;
 cbn [check_bind] in Run; try (residual_noerr Run); eexists; reflexivity.
Qed.

Lemma loaded_ast : forall g,
 nget (w_syms w) (gs_id g) = Some g -> A.ast_of W (gs_id g) = Some (gs_decl g).
Proof.
 intros g Loaded; change (A.ast_of (W_read w R Fl) (gs_id g) = Some (gs_decl g)); unfold A.ast_of; rewrite (w_decl_read w R Fl _ _ Loaded);reflexivity.
Qed.

Definition FunctorClosureStep (fuel : nat) : Prop :=
 forall c e pa pg s xs e' c' final F C u g target_name target_names target_args target_result
 provider_name provider_names provider_args provider_result bodies,
 check_bind fuel c e (Spec (Some pa) None None) (AEPath pg None) s = Good ((xs,e',c'),final) ->
 A.resolve_sym W (A.fr_mod F) pa = Some u -> A.resolve_sym W (A.fr_mod F) pg = Some g ->
 A.ast_of W u = Some (ADFunctor target_name target_names target_args target_result None) ->
 A.ast_of W g = Some (ADFunctor provider_name provider_names provider_args provider_result (Some bodies)) ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl final F ->
 BindResult w P R Fl c e F C (Spec (Some pa) None None,Some (AEPath pg None)) xs e' c'.


Theorem remaining_bindings_are_functor_closures : forall fuel,
 FunctorClosureStep (S fuel) -> NonMethodRemainingBindStep w P R Fl (S fuel).
Proof.
 intros fuel Closure c e sp y s xs e' c' final Run F C Simple Copy Named Lambda NonMethod Valid Cache Forward Caller Last.
 destruct (nonmethod_binding_bare fuel c e sp y s xs e' c' final Run NonMethod) as [pa Same]; subst sp.
 pose proof Run as Whole.
 cbn [check_bind] in Run; apply bind_inv in Run as [found [resolved [Resolve Run]]].
 assert (Hw : st_w s = w).
 { destruct (ok_check_bind (S fuel) c e (Spec (Some pa) None None) y _ _ _ Valid Whole) as [_ Frame].
   rewrite <- (fr_w _ _ Frame); exact (lt_w _ _ _ _ _ (proj1 Last)). }
 destruct (rpath_rel w R Fl b Load _ _ _ _ _ Hw (cr_mod_in _ _ _ _ _ _ _ Caller) Resolve) as [Same Reading].
 subst resolved; destruct found as [[id|mi]|]; [|residual_noerr Run|residual_noerr Run].
 cbn [option_map ent_of] in Reading.
 apply bind_inv in Run as [g [targeted [Symbol Run]]].
 destruct (sym_loader w R Fl b Load _ _ _ _ _ _ Valid Hw Reading Symbol) as [Same Loaded].
 subst targeted.
 pose proof (load_world_ids b w Load _ _ Loaded) as Id; subst id.
 assert (Target : A.resolve_sym W (A.fr_mod F) pa = Some (gs_id g)).
 { unfold A.resolve_sym; rewrite (cr_mod _ _ _ _ _ _ _ Caller), Reading; reflexivity. }
 pose proof (kind_sym_read b w R Fl Load _ _ Loaded) as SemanticKind.
 change (A.kind_sym W (gs_id g) = Some (kconv (gs_kind g))) in SemanticKind.
 destruct (gs_kind g) eqn:Kind; cbn [kconv] in SemanticKind; try (residual_noerr Run).
 - destruct y; try (residual_noerr Run).
   cbn [simple_bind] in Simple; rewrite Target,SemanticKind in Simple; discriminate Simple.
 - cbn [simple_bind] in Simple; rewrite Target,SemanticKind in Simple; discriminate Simple.
 - apply bind_inv in Run as [wanted [signed [Signature Run]]].
   assert (O : ok (fn_sig g (tymap e) e None)) by (unfold fn_sig;okt).
   destruct (O _ _ _ Valid Signature) as [Is Fs].
   assert (Ws : st_w signed = w) by (rewrite (fr_w _ _ Fs); exact Hw).
   apply bind_inv in Run as [[[provider pre] mapping] [provided [Provider Run]]].
   destruct y;
     try solve [cbn [function_lambda_bind] in Lambda; rewrite Target,SemanticKind in Lambda; discriminate Lambda].
   all: apply bind_inv in Provider as [pv [expected [Expect Provider]]].
   all: destruct (expect_fn_provider_source fuel c e F C _ signed pv expected Expect Is Ws Caller) as
     (pg & u & name & params & result & body & Source & ProviderPath & ProviderDecl);
     try discriminate Source.
   injection Source as PathEq AppEq; subst pg.
   match type of AppEq with ?app = None => subst app | None = ?app => subst app end.
   cbn [named_function_bind] in Named; rewrite Target,ProviderPath,SemanticKind,ProviderDecl in Named; discriminate Named.
 - apply bind_inv in Run as [[statements out] [bound [Bind Return]]].
   injection Return as <- <- <- <-.
   destruct (signature_functor_decl g (signature_loaded_kind w R Fl b Load _ _ Loaded) Kind)
     as (target_name & target_names & target_args & target_result & target_body & TargetDecl).
   destruct fuel as [|fuel]; [discriminate Bind|].
   cbn [bind_functor] in Bind; unfold functor_parts in Bind; rewrite TargetDecl in Bind.
   destruct target_body as [target_bodies|]; [residual_noerr Bind|].
   destruct y; try (residual_noerr Bind).
   match goal with app:sapp |- _ => destruct app end; [residual_noerr Bind|].
   apply bind_inv in Bind as [provider [provided [ProviderResolve Bind]]].
   destruct (esym_rel w R Fl b Load (cx_mod c) p s provider provided Valid Hw
     (cr_mod_in _ _ _ _ _ _ _ Caller) ProviderResolve) as [Same ProviderReading].
   subst provided; destruct provider as [bg|]; [|residual_noerr Bind].
   destruct ProviderReading as [ProviderPath ProviderLoaded].
   destruct (kind_eqb (gs_kind bg) KFunctor) eqn:ProviderKind; cbn [negb] in Bind; [|residual_noerr Bind].
   assert (BgKind : gs_kind bg = KFunctor) by (destruct (gs_kind bg); cbn [kind_eqb] in ProviderKind; congruence).
   destruct (signature_functor_decl bg (signature_loaded_kind w R Fl b Load _ _ ProviderLoaded) BgKind)
     as (provider_name & provider_names & provider_args & provider_result & provider_body & ProviderDecl).
   assert (TargetAst : A.ast_of W (gs_id g) = Some (ADFunctor target_name target_names target_args target_result None)).
   { rewrite (loaded_ast g Loaded),TargetDecl;reflexivity. }
   assert (ProviderAst : A.ast_of W (gs_id bg) = Some (ADFunctor provider_name provider_names provider_args provider_result provider_body)).
   { rewrite (loaded_ast bg ProviderLoaded),ProviderDecl;reflexivity. }
   assert (ProviderSemanticPath : A.resolve_sym W (A.fr_mod F) p = Some (gs_id bg)).
   { rewrite (cr_mod _ _ _ _ _ _ _ Caller);exact ProviderPath. }
   destruct provider_body as [bodies|].
   + eapply (Closure c e pa p s statements out c bound F C (gs_id g) (gs_id bg)
       target_name target_names target_args target_result provider_name provider_names provider_args provider_result bodies);
       eassumption.
   + unfold abstract_copy_bind,abstract_functor_id in Copy.
     rewrite Target,TargetAst,ProviderSemanticPath,ProviderAst in Copy;discriminate Copy.
Qed.
End Remaining.
