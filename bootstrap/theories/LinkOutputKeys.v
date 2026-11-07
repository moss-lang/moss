(* Returned output method keys retain real method slots. Their validity follows
   from the completed executable context, independently of source ordering. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore BodySoundCore
 ElabKeeps CtxWalk CtxMethod MethodKeyTr ProvideResultExec LambdaKeyTranslation.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL provision_app fresh].
Definition output_method_slots (e e' : env) : Prop :=
 forall r m, get_mth e r m <> None -> get_mth e' r m <> None.
Definition output_key_slot (e : env) (k : key) : Prop :=
 match k with KyMth r m => get_mth e r m <> None | _ => True end.
Definition output_keys_emitted (e : env) (out : list key) (e' : env) (keys : list key) : Prop :=
 output_method_slots e e' /\
 (Forall (output_key_slot e) out -> Forall (output_key_slot e') keys).
Lemma output_slots_same : forall e e', en_mths e' = en_mths e -> output_method_slots e e'.
Proof. intros e e' Same r m;rewrite (get_mth_same e e' r m Same);tauto. Qed.
Lemma output_slots_trans : forall e1 e2 e3,
 output_method_slots e1 e2 -> output_method_slots e2 e3 -> output_method_slots e1 e3.
Proof. unfold output_method_slots;auto. Qed.
Lemma output_slots_keys : forall e e' keys,
 output_method_slots e e' -> Forall (output_key_slot e) keys -> Forall (output_key_slot e') keys.
Proof. intros e e' keys Slots Valid;induction Valid;constructor;auto.
 destruct x;cbn [output_key_slot] in *;auto. Qed.
Lemma output_slots_set_method : forall e r m sg, output_method_slots e (set_mth e r m sg).
Proof. intros e r m sg q n;rewrite get_mth_set_case;destruct (Nat.eqb n m && ety_eqb q r);discriminate||tauto. Qed.
Lemma output_method_slot_set : forall e r m sg, output_key_slot (set_mth e r m sg) (KyMth r m).
Proof. intros;cbn [output_key_slot];rewrite get_mth_set_case,Nat.eqb_refl,ety_eqb_refl;discriminate. Qed.
Lemma output_emitted_same : forall e e' keys,
 en_mths e' = en_mths e -> output_keys_emitted e keys e' keys.
Proof. intros;split;[apply output_slots_same;assumption|apply output_slots_keys;apply output_slots_same;assumption]. Qed.
Lemma output_emitted_plain : forall e e' keys key,
 en_mths e' = en_mths e -> output_key_slot e' key ->
 output_keys_emitted e keys e' (keys++[key]).
Proof.
 intros e e' keys key Same New;split;[apply output_slots_same;exact Same|].
 intro Old;apply Forall_app;split;[eapply output_slots_keys;[apply output_slots_same;exact Same|exact Old]|].
 constructor;[exact New|constructor].
Qed.
Lemma output_emitted_compose : forall e1 e2 e3 keys1 keys2 keys3,
 output_keys_emitted e1 keys1 e2 keys2 -> output_keys_emitted e2 keys2 e3 keys3 ->
 output_keys_emitted e1 keys1 e3 keys3.
Proof. intros e1 e2 e3 keys1 keys2 keys3 [First Keys1] [Last Keys2];split;[eapply output_slots_trans;eassumption|auto]. Qed.
Lemma output_emitted_fold : forall X (step : env * list key -> X -> M (env * list key)) its,
 (forall e out x s e' keys final,step (e,out) x s=Good((e',keys),final)->output_keys_emitted e out e' keys) ->
 forall e out s e' keys final,mfold step (e,out) its s=Good((e',keys),final)->output_keys_emitted e out e' keys.
Proof.
 intros X step its Each;induction its as [|x xs IH];intros e out s e' keys final Run.
 - injection Run as <- <- <-;apply output_emitted_same;reflexivity.
 - cbn [mfold] in Run;apply bind_inv in Run as [[mid outputs] [state [First Rest]]].
   eapply output_emitted_compose;[eapply Each;exact First|eapply IH;exact Rest].
Qed.
Lemma provided_at_methods : forall e k g tm meth s e' final,
 provided_at e k g tm meth s=Good(e',final)->en_mths e'=en_mths e.
Proof.
 intros e k g tm meth s e' final Run;unfold provided_at in Run.
 apply bind_inv in Run as [app [state [App Run]]].
 apply bind_inv in Run as [stamp [last [Stamp Return]]].
 injection Return as <- <-;reflexivity.
Qed.

Local Opaque app_subst resolve_type_symbol find_method_decl symbol_type fn_sig
 method_key provided_at resolve_sym already_provided ety_of fresh functor_fixed.
Theorem provide_spec_keys_slots : forall fuel e fg mi sp outer out s e' keys final,
 provide_spec fuel e fg mi sp outer out s=Good((e',keys),final)->
 output_keys_emitted e out e' keys.
Proof.
 induction fuel as [|f IH];intros e fg mi [p d a] outer out s e' keys final Run;[discriminate Run|].
 cbn [provide_spec] in Run;apply bind_inv in Run as [sub [sx [Sub Run]]].
 destruct d as [dn|],p as [pp|];try discriminate Run.
 - apply bind_inv in Run as [r [sr [Recv Run]]].
   apply bind_inv in Run as [mo [sm [Method Run]]];destruct mo as [m|];[|discriminate Run].
   apply bind_inv in Run as [this [st [This Run]]].
   apply bind_inv in Run as [sg [ss [Signature Run]]].
   apply bind_inv in Run as [k [sk [Key Run]]].
   destruct (@method_key_value e mi this m ss k sk Key) as [Eq State];subst k sk.
   apply bind_inv in Run as [ep [se [Provide Return]]];injection Return as <- <- <-.
   assert (Same : en_mths ep = en_mths (set_mth e (canon e this) (gs_id m) sg)) by (eapply provided_at_methods;exact Provide).
   split.
   + eapply output_slots_trans;[apply output_slots_set_method|apply output_slots_same;exact Same].
   + intro Old;apply Forall_app;split.
     * eapply output_slots_keys;[eapply output_slots_trans;[apply output_slots_set_method|apply output_slots_same;exact Same]|exact Old].
     * constructor;[|constructor].
       cbn [output_key_slot];rewrite (get_mth_same _ _ _ _ Same).
       apply output_method_slot_set.
 - apply bind_inv in Run as [found [sg [Symbol Run]]];destruct found as [g|];[|discriminate Run].
   destruct (gs_kind g) eqn:Kind;destruct (gs_decl g);cbn beta iota zeta in Run;
    try (destruct (already_provided g);[injection Run as <- <- <-;apply output_emitted_same;reflexivity|]);
    try discriminate Run.
   all: try first [
    match type of Run with mfold ?step ?acc ?its _ = _ =>
      eapply (output_emitted_fold _ step its);[intros;eapply IH;eassumption|exact Run]
    end
    | apply bind_inv in Run as [value [state [Read Run]]];
      apply bind_inv in Run as [ep [se [Provide Return]]];injection Return as <- <- <-;
      apply output_emitted_plain;[pose proof (provided_at_methods _ _ _ _ _ _ _ _ Provide) as Eq;exact Eq|exact I]
    | injection Run as <- <- <-;apply output_emitted_same;reflexivity].
   destruct o0;[discriminate Run|].
   repeat apply bind_inv in Run as [? [? [? Run]]].
   injection Run as <- <- <-;apply output_emitted_plain;[reflexivity|exact I].
Qed.

Theorem provide_result_keys_slots : forall e fg gens s e' keys final,
 provide_result e fg gens s=Good((e',keys),final)->Forall (output_key_slot e') keys.
Proof.
 intros e fg gens s e' keys final Run.
 destruct (provide_result_phases e fg gens s e' keys final Run)
  as (ts&st&typed&se&fuel&sn&Types&Install&Fuel&Walk).
 assert (Emitted : output_keys_emitted typed [] e' keys).
 { match type of Walk with mfold ?step ?initial ?items _ = _ =>
     eapply (output_emitted_fold _ step items);[|exact Walk]
   end.
   intros e0 out sp before after outputs last Step.
   exact (provide_spec_keys_slots fuel e0 fg (gs_mod fg) sp [] out before after outputs last Step). }
 destruct Emitted as [_ Keys];apply Keys;constructor.
Qed.
Lemma method_assoc_valid : forall rows r sg,
 Forall (fun p : ety * Elab.fsig => ety_ok (fst p) = true) rows ->
 assoc ety_eqb r rows = Some sg -> ety_ok r = true.
Proof.
 intros rows;induction rows as [|[receiver signature] rest IH];intros r sg Valid Read;[discriminate Read|].
 inversion Valid as [|first tail Head Tail];subst.
 cbn [assoc fst] in Read,Head;destruct (ety_eqb r receiver) eqn:Same.
 - apply ety_eqb_true in Same;subst r;exact Head.
 - eapply IH;eassumption.
Qed.
Theorem output_slots_valid : forall w R Fl c e F C keys,
 CxRel w R Fl c e F C -> Forall (output_key_slot e) keys -> Forall LambdaKeyValid keys.
Proof.
 intros w R Fl c e F C keys Caller Slots;rewrite Forall_forall in Slots|-*.
 intros k Member;destruct k as [v|fn|r m|u];try exact I.
 specialize (Slots (KyMth r m) Member);cbn [output_key_slot] in Slots.
 unfold get_mth in Slots;destruct (nget (en_mths e) m) as [rows|] eqn:Rows;[|contradiction Slots;reflexivity].
 destruct (assoc ety_eqb r rows) as [sg|] eqn:Found;[|contradiction Slots;reflexivity].
 destruct (cr_mth_rows _ _ _ _ _ _ _ Caller m rows Rows) as [_ Good].
 rewrite Forall_forall in Good.
 eapply method_assoc_valid;[|exact Found].
 apply Forall_forall;intros [receiver signature] Here;exact (proj1 (Good (receiver,signature) Here)).
Qed.
