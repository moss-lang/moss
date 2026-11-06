(* ------------------------------------------------------------------------ *)
(* BodyPending.v — BodySound.Pending from the construct lemmas (Track A,   *)
(* elaborate_sound step 2).                                                 *)
(*                                                                           *)
(* call_step is pd_synth for every AECall: a call the elaborator accepts   *)
(* names a tag (BodyTag.tag_step) or a fn (Hfn, the other half of Track A),*)
(* read from the loader as the relation's kind_sym does.  pending is       *)
(* Pending, assembled from BodyData (fields, records, match), BodyOps      *)
(* (operators), BodyLit (literals), BodyConst (defined vals) and call_step,*)
(* Actual library lookup, ordinary calls, attached calls, method families    *)
(* and literal families are discharged here. The remaining source callbacks *)
(* is RemainingBindStep (the unclosed ordinary provider binding cases).    *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabMono BodySound BodyData BodyOps
  BodyTag BodyConst BodyLit CallAbstract ProvidedDirect LiteralDirect BindSequence MethodCallRoutes AttachedCallNoDemand CallDefinedAny BindCopyDispatch BindFnDispatch BindLambdaDispatch LibraryFacts FamilyCallbacks CallDefinedDemand AttachedCallDemand FunctorApplyExec.
From NM Require Import InfoPreserve.
Import ListNotations.
Local Open Scope list_scope.
Local Open Scope string_scope.

From NM Require Import BodyTypeSupport NominalSupport.

Module A := Audit.Elaboration.

Local Strategy opaque [Nat.of_num_uint FUEL].

Tactic Notation "bi" hyp(H) "as" ident(a) ident(s1) ident(H1) :=
  apply bind_inv in H as (a & s1 & H1 & H).

Ltac nogood H := exfalso;
  solve [repeat (cbn beta iota zeta in H;
                 first [ exact (err_not_good _ _ _ _ _ _ H) | discriminate H
                       | apply bind_inv in H as (? & ? & ? & H) ])].

Section Pend.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation WW := (BodySound.W w R Fl).
Notation trF := (tr Fl).
Notation trx := (BodyData.trx Fl).
Variable b : bundle.
Hypothesis Hload : load_world b = Good w.
Hypothesis Hworld : A.WorldOf b WW.

(* the section hypotheses of BodyData, BodyOps, BodyTag, BodyLit, BodyConst *)
Hypothesis HFl : forall a c, Fl a = Fl c -> a = c.
Lemma Hlang : forall (short : string) (det : bool) (n : string) (li x : nat), sassoc short (lang_modules w) = Some li ->
  sassoc n (if det then gm_detached (get_mod w li) else gm_names (get_mod w li)) = Some x ->
  A.LangSym WW short (if det then A.NSDetached else A.NSName) n x.
Proof. exact(lang_symbol_scope b w R Fl Hload). Qed.
Lemma Hfamily : forall f, OK w P R Fl f -> ProvidedFamilyStep w P R Fl f.
Proof. exact(@provided_family_step w P R Fl b Hload HFl Hworld). Qed.
Lemma Hprovided : forall f, OK w P R Fl f -> forall c e ox oty m args s x t s',
  provided_call f c e ox oty m (map AA args) s = Good ((x, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F -> WFty oty ->Support WW oty ->
  A.Provided WW P F C ox ((fun t => trF (canon e t)) oty) (gs_id m) args x ((fun t => trF (canon e t)) t) /\ WFty t /\ Support WW t.
Proof.
 intros f IH; exact (@provided_step w P R Fl b Hload f IH (Hfamily f IH)).
Qed.
Lemma Hlitfamily : forall f, OK w P R Fl f -> LiteralFamilyStep w P R Fl f.
Proof. intros f IH;exact(@literal_family_step w P R Fl b Hload HFl Hworld f). Qed.
Lemma HcallAB : forall f, OK w P R Fl f -> forall c e ox T oy m s x t s',
  provided_call f c e ox T m [AB oy T] s = Good ((x, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->WFty T ->Support WW T ->
  A.LitOp WW C ((fun t => trF (canon e t)) T) (gs_id m) ox oy x.
Proof.
 intros f IH; exact (@literal_step w P R Fl b Hload f (Hlitfamily f IH)).
Qed.
Definition Hty := @pd_ty_supported w P R Fl b Hload.

(* [calls and methods, the other half of Track A] *)
Lemma Hcalldemand : forall f, OK w P R Fl f -> @DefinedDemandStep w P R Fl f.
Proof. exact(@defined_demand_step w P R Fl b Hload Hworld HFl). Qed.
Lemma Hdefined : forall f, OK w P R Fl f -> DefinedFnStep w P R Fl f.
Proof.
 intros f IH;exact(@defined_fn_step w P R Fl b Hload Hworld HFl f IH(Hcalldemand f IH)).
Qed.
Lemma Hmethdemand : forall f, OK w P R Fl f -> @AttachedDemandStep w P R Fl f.
Proof. exact(@attached_demand_step w P R Fl b Hload HFl Hworld). Qed.
Lemma Hmethdefined : forall f, OK w P R Fl f -> AttachedDefinedStep w P R Fl f.
Proof.
 intros f IH;exact(@attached_defined_step w P R Fl b Hload HFl
  (@A.wo_reqs_nodup b WW Hworld)f IH(Hmethdemand f IH)).
Qed.
Lemma Hmeth : forall f, OK w P R Fl f -> forall c e o pa args exp s v t s',
  synth (S f) c e (AEMeth o pa args) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> opt_wf exp ->opt_support WW exp -> Post w P R Fl s' F ->
  A.Ex WW P F C (AEMeth o pa args) (option_map (fun t => trF (canon e t)) exp) v ((fun t => trF (canon e t)) t) /\ WFty t /\ Support WW t.
Proof.
 intros f IH c e o pa args exp s v t s' H F C Hi Ii Ifwd Hc Wexp Sexp Hp.
 exact(@method_sound w P R Fl b Hload f IH(Hfamily f IH)(Hmethdefined f IH)
  c e o pa args exp s v t s' F C H Hi Ii Ifwd Hc Hp).
Qed.
Hypothesis Hlambda_remainingbind : forall f, OK w P R Fl f -> LambdaRemainingBindStep w P R Fl f.
Lemma Hremainingbind : forall f, OK w P R Fl f -> RemainingBindStep w P R Fl f.
Proof.
 intros [|f] IH.
 - intros c e sp y s xs e1 c1 sf Run;discriminate Run.
 - exact(@residual_after_function_lambda w P R Fl b Hload HFl f
    (ok_down w P R Fl f IH)(Hlambda_remainingbind(S f)IH)).
Qed.
Lemma Hbinditem : forall f, OK w P R Fl f -> ResidualBindStep w P R Fl f.
Proof.
 intros f IH;exact(@residual_after_abstract_copy w P R Fl b Hload Hworld f(@residual_after_named_function w P R Fl b Hload Hworld HFl f(Hremainingbind f IH))).
Qed.
Lemma Happlybind : forall f, OK w P R Fl f -> ApplyBindStep w P R Fl f.
Proof. exact(@apply_bind_step w P R Fl b Hload Hworld HFl Hremainingbind). Qed.
Lemma Hbind : forall f, OK w P R Fl f -> forall c e its s xs e1 c1 d s',
  check_stmt (S f) c e (ASBind its) s = Good ((xs, e1, c1, d), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F -> StmtConcl w P R Fl F C (ASBind its) xs c c1 e1 d.

Proof.
 intros f IH; exact (@bind_stmt_step w P R Fl b Hload f IH (Hbinditem f IH) (Happlybind f IH)).
Qed.

(* A call the elaborator accepts names a tag or a fn. *)
Lemma call_head : forall f c e pa a args exp s v t s',
  synth (S f) c e (AECall pa a args) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->
  exists x, A.resolve_sym WW (A.fr_mod F) pa = Some x /\
    (A.kind_sym WW x = Some A.DKTag \/ A.kind_sym WW x = Some A.DKFn).
Proof.
  intros f c e pa a args exp s v t s' H F C Hi Ii Ifwd Hc [Hl _].
  cbn [synth] in H. destruct (synth_wrap e exp s v t s' _ H (ok_synth_call f c e pa a args) Hi) as (s1 & H1 & _ & Fs1 & Fs1').
  assert (Hw : st_w s = w) by (rewrite <- (lt_w _ _ _ _ _ Hl); symmetry; exact (fr_w _ _ (fr_trans _ _ _ Fs1 Fs1'))).
  destruct f as [|f]; [discriminate H1|]. cbn [synth_call] in H1. bi H1 as r s2 Hr.
  destruct (rpath_rel w R Fl b Hload _ _ _ _ _ Hw (cr_mod_in _ _ _ _ _ _ _ Hc) Hr) as [-> Hres].
  destruct r as [[x|j]|]; [|nogood H1|nogood H1].
  cbn [option_map ent_of] in Hres. bi H1 as g s3 Hs.
  destruct (sym_loader w R Fl b Hload _ _ _ _ _ _ Hi Hw Hres Hs) as [-> Hg].
  exists x. split; [unfold A.resolve_sym; rewrite (cr_mod _ _ _ _ _ _ _ Hc), Hres; reflexivity|].
  unfold BodySound.W. rewrite (kind_sym_read b w R Fl Hload x g Hg).
  destruct (gs_kind g); try nogood H1; [left | right]; reflexivity.
Qed.

Lemma call_step : forall f, OK w P R Fl f -> forall c e pa a args exp s v t s',
  synth (S f) c e (AECall pa a args) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> opt_wf exp ->opt_support WW exp -> Post w P R Fl s' F ->
  A.Ex WW P F C (AECall pa a args) (option_map (fun t => trF (canon e t)) exp) v ((fun t => trF (canon e t)) t) /\ WFty t /\ Support WW t.
Proof.
  intros f IH c e pa a args exp s v t s' H F C Hi Ii Ifwd Hc Hx Hxs Hp.
  destruct (call_head f c e pa a args exp s v t s' H F C Hi Ii Ifwd Hc Hp) as (x & Hr & [Hk|Hk]).
  - eapply tag_step; try eassumption. exists x. exact (conj Hr Hk).
  - exact (@fn_call_step w P R Fl b Hload f IH (Hdefined f IH) c e pa a args exp s v t s' H F C Hi Ii Ifwd Hc Hx Hxs Hp (ex_intro _ x (conj Hr Hk))).
Qed.

Theorem pending : Pending w P R Fl.
Proof.
  pose proof Hlang as Hlang_fact.
  pose proof Hprovided as Hprovided_fact.
  pose proof HcallAB as Hliteral_fact.
  constructor.
  - intros f IH c e x exp s v t s' Hpe H F C Hi Ii Ifwd Hc Hx Hxs Hp.
    destruct x; try discriminate Hpe.
    + eapply call_step; eassumption.
    + eapply field_step; eassumption.
    + eapply Hmeth; eassumption.
    + eapply record_step; eassumption.
    + eapply match_step; eassumption.
    + eapply unary_step; eassumption.
    + eapply binary_step; eassumption.
    + eapply lit_step; eassumption.
  - intros f IH c e st0 s xs e1 c1 d s' Hpe H F C Hi Ii Ifwd Hc Hp.
    destruct st0; try discriminate Hpe. eapply Hbind; eassumption.
  - exact (const_step w P R Fl b Hload).
  - exact Hty.
Qed.

End Pend.
