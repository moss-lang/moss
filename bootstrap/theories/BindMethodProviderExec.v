(* Every successful provider selection denotes the matching method provider. *)
From NM Require Import BodyProofTools.
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame
 BodySoundCore BodyTy NominalSupport CanonTypes ProviderSig BindMethodTrace LambdaProviderExec
 BindMethodWrappedExec BindMethodAttachedExec.
Import ListNotations.
Module A := Audit.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].

Lemma ok_method_bind_provider : forall fuel c e y this m sg ts,
 ok (method_bind_provider fuel c e y this m sg ts).
Proof.
 intros; unfold method_bind_provider; destruct y;
 try solve [apply ok_lambda_provider].
 all: apply ok_bind; [unfold expect_provider,expect_defined_fn;okt|intro pv];
   apply ok_bind; [apply ok_match_fn_sig|intro mapping];
   apply ok_bind; [destruct (is_some (gs_recv pv));[apply ok_ret|apply ok_wrapper]|intro target];
   apply ok_ret.
Qed.

Ltac invalid_method_provider Run :=
 unfold method_bind_provider in Run;
 apply bind_inv in Run as (? & ? & Expect & Run);
 cbn [expect_provider expect_defined_fn] in Expect;
 exfalso; exact (err_not_good _ _ _ _ _ _ Expect).

Section Provider.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Load : load_world b = Good w.
Hypothesis World : A.WorldOf b (BodySoundCore.W w R Fl).
Hypothesis Finj : forall a b, Fl a = Fl b -> a = b.
Notation W := (BodySoundCore.W w R Fl).
Theorem method_bind_provider_sound : forall fuel c e F C y this m wanted ts s target pre sm final,
 OK w P R Fl fuel ->
 method_bind_provider fuel c e y this m wanted ts s = Good ((target,pre,sm),final) ->
 Inv s -> InfoInv s -> FwdInv s -> st_w s = w ->
 nget (w_syms w) (gs_id m) = Some m -> CxRel w R Fl c e F C ->
 SigOK wanted -> SigSupport W wanted -> WFty this -> Support W this ->
 Post w P R Fl final F ->
 A.MethProvider W P F C y (canonical_sig Fl e wanted) (tr Fl (canon e this))
   target (fold_stmts pre) sm.
Proof.
 intros fuel c e F C y this m wanted ts s target pre sm final IH Run Valid Cache Fwd Hw Loaded Caller
   SignatureWellformed SignatureSupported ReceiverWellformed ReceiverSupported Last.
 destruct y; try solve [invalid_method_provider Run].
 - match goal with app:sapp |- _ => destruct app end; [invalid_method_provider Run|].
   eapply (method_bind_wrapped_provider w P R Fl b Load World);try eassumption.
   exact (proj1 Last).
 - match goal with obj:aexpr |- _ => destruct obj end; try solve [invalid_method_provider Run].
   match goal with app:sapp |- _ => destruct app end; [invalid_method_provider Run|].
   eapply (method_bind_attached_provider w P R Fl b Load World Finj);try eassumption.
   exact (proj1 Last).
 - apply A.MPLambda.
   cbn [method_bind_provider] in Run.
   match type of Run with
   | lambda_provider ?f ?ctx ?env ?ps ?rt ?body ?bound ?sg ?receiver ?what ?initial = Good _ =>
     exact (lambda_provider_sound w P R Fl b Load Finj f ctx env ps rt body bound sg receiver what
       initial target pre sm final IH Run F C Valid Cache Fwd Hw Loaded Caller
       SignatureWellformed SignatureSupported ReceiverWellformed ReceiverSupported Last)
   end.
Qed.
End Provider.
