(* The executable function checker forms its source context and signature,
   numbers the frame and records exactly the translated body. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit EaBase EaTypes EaTypesSound EaControlSound.
From NM Require EaMain EaNumbering EaContextTableSound EaCodeAllocationSound WorldSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Local Lemma read_receiver_arguments : forall W receiver C,
  find (fun x => negb (is_some (nlook x (c_ty C)))) (w_reqs W receiver) = None ->
  Forall2 (fun x xa => fst xa = x /\ nlook x (c_ty C) = Some (snd xa))
    (w_reqs W receiver)
    (flat_map (fun x => match nlook x (c_ty C) with Some t => [(x, t)] | None => [] end)
      (w_reqs W receiver)).
Proof.
  intros W receiver C. induction (w_reqs W receiver) as [|x xs IH]; intro Found; [constructor|].
  cbn [find] in Found. cbn [flat_map].
  destruct (nlook x (c_ty C)) as [t|] eqn:Meaning; [|discriminate].
  cbn. constructor; [split; [reflexivity|exact Meaning]|now apply IH].
Qed.

Theorem elab_fn_read : forall E f d rcv n ps rt b before after,
  EaContextTableSound.ContextTableSound (e_W E) ->
  w_decl (e_W E) f = Some d -> d_ast d = ADFn rcv n ps rt (Some b) ->
  EaMain.elab_fn E f before = Ok (tt, after) ->
  exists C ns this g F x t started emitted,
    CtxOf (e_W E) f C ns /\ FunctionReceiver (e_W E) f rcv n C this /\
    SigOf (e_W E) f (c_ty C) this g /\
    fr_mod F = d_mod d /\ fr_ret F = Some (sg_ret g) /\ fr_this F = this /\
    fr_loop F = false /\ fr_fresh F = [] /\ fr_applying F = [] /\ fr_pins F = [] /\
    Numbering F this (map fst ps) /\
    started = before /\
    r_blke (EaMain.R0 E) F
      (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet)
        (combine (map fst ps) (sg_params g)) C)
      b (Some (sg_ret g)) started = Ok ((x, t), emitted) /\
    set_fn_body f x emitted = Ok (tt, after).
Proof.
  intros E f d rcv n ps rt b before after Contexts Declared Ast Run.
  unfold EaMain.elab_fn in Run.
  apply mbind_ok in Run as (declaration & after_decl & ReadDecl & Run).
  rewrite Declared in ReadDecl. injection ReadDecl as <- <-.
  rewrite Ast in Run.
  apply mbind_ok in Run as ([context needs] & after_context & ReadContext & Run).
  destruct (w_context (e_W E) f) as [[C ns]|] eqn:Context; [|discriminate ReadContext].
  injection ReadContext as <- <- <-.
  apply mbind_ok in Run as (this & started & Receiver & Run).
  assert (Reading : FunctionReceiver (e_W E) f rcv n C this /\ started = before).
  {
    destruct rcv as [ | | rn ]; try (injection Receiver as <- <-; split; reflexivity).
    cbn [FunctionReceiver] in *.
    destruct (w_scope (e_W E) (d_mod d) NSName rn) as [[r|module_path]|] eqn:ReceiverName;
      try discriminate Receiver.
    cbn [mbind ret fail] in Receiver.
    destruct (w_attached (e_W E) r n) as [f'|] eqn:AttachedMethod; [|discriminate Receiver].
    destruct (Nat.eqb f f') eqn:Method; [|discriminate Receiver].
    apply Nat.eqb_eq in Method. subst f'.
    destruct (kind_is (e_W E) r DKTag || kind_is (e_W E) r DKUnit) eqn:Kind;
      [|discriminate Receiver].
    destruct (find (fun x => negb (is_some (nlook x (c_ty C)))) (w_reqs (e_W E) r))
      eqn:Args; [discriminate Receiver|].
    injection Receiver as <- <-. split; [|reflexivity].
    exists r. eexists. split; [exact AttachedMethod|].
    split; [|split; [reflexivity|now apply read_receiver_arguments]].
    apply orb_true_iff in Kind. now rewrite !kind_is_spec in Kind.
  }
  destruct Reading as [Reading ->].
  assert (ReadCtx : CtxOf (e_W E) f C ns).
  {
    specialize (Contexts f C ns Context).
    unfold kind_sym in Contexts. rewrite Declared, Ast in Contexts.
    destruct rcv; exact Contexts.
  }
  apply mbind_ok in Run as (g & after_sig & SignatureRun & Run).
  unfold pure in SignatureRun.
  destruct (sig_of (e_W E) (e_fuel E) f (c_ty C) this) as [signature|message] eqn:Signature;
    [|discriminate SignatureRun].
  injection SignatureRun as <- <-.
  apply mbind_ok in Run as (distinct & after_names & DistinctRun & Run).
  destruct (suniq_dup ((if is_some this then ["this"] else []) ++ map fst ps)) eqn:Distinct;
    [discriminate DistinctRun|].
  assert (Parameters : NoDup (map fst ps)).
  {
    apply EaNumbering.checked_parameters_distinct in Distinct.
    destruct this; cbn [is_some app] in Distinct; [inversion Distinct; assumption|exact Distinct].
  }
  injection DistinctRun as <- <-.
  apply mbind_ok in Run as ([x t] & emitted & Body & Write).
  lazymatch type of Body with
  | r_blke _ ?F _ _ _ ?state = _ => exists C, ns, this, signature, F, x, t, state, emitted
  end.
  split; [exact ReadCtx|split; [exact Reading|]].
  split; [eapply sig_of_sound; exact Signature|].
  repeat (split; [reflexivity|]).
  split.
  - eapply EaNumbering.mk_frame_vars_numbering; [reflexivity|exact Parameters].
  - split; [reflexivity|split; [exact Body|exact Write]].
Qed.

Lemma miter_member_run : forall A (action : A -> M unit) xs before after,
  miter action xs before = Ok (tt, after) ->
  forall x, In x xs -> exists started emitted, action x started = Ok (tt, emitted).
Proof.
  intros A action xs. induction xs as [|x xs IH]; intros before after Run y Member;
    [contradiction|].
  cbn [miter] in Run.
  apply mbind_ok in Run as ([] & middle & Step & Rest).
  destruct Member as [<-|Member]; [exists before, middle; exact Step|eapply IH; eassumption].
Qed.

Theorem assembled_fn_was_checked : forall E bound entry program before after,
  EaMain.assemble E bound entry before = Ok (program, after) ->
  forall f d rcv n ps rt b,
  f < bound -> w_decl (e_W E) f = Some d -> d_ast d = ADFn rcv n ps rt (Some b) ->
  exists started emitted, EaMain.elab_fn E f started = Ok (tt, emitted).
Proof.
  intros E bound entry program before after Run f d rcv n ps rt b Bounded Declared Ast.
  unfold EaMain.assemble in Run.
  apply mbind_ok in Run as ([] & stage & Functions & _).
  assert (Present : In f (seq 0 bound)) by (apply in_seq; lia).
  destruct (miter_member_run _ _ _ _ _ Functions _ Present) as (started & emitted & Checked).
  exists started, emitted. unfold EaMain.elaborate_function, EaMain.defined_fn in Checked. rewrite Declared, Ast in Checked.
  unfold with_msg in Checked.
  destruct (EaMain.elab_fn E f started) as [[value state]|message] eqn:Body; [|discriminate].
  injection Checked as <- <-. reflexivity.
Qed.
