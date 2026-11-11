(* Lambda typing follows the actual capture allocation, nested body
   elaboration, and registration in the final program. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit EaBase EaCodeAllocationSound EaCodeEffects EaLambdaKeys EaLinkBodySound EaRecSound NMap.
From NM Require EaAllocationSound EaBind EaControlSound EaMain EaNumbering EaSatisfiesSound
  EaTypes EaTypesSound PublicCallRealize PublicContextFacts PublicTypeFacts WorldSound.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Import EaBind.
Local Open Scope list_scope.

Local Lemma fresh_n_keeps_origins : forall count before values after,
  fresh_n count before = Ok (values, after) ->
  es_lamcx after = es_lamcx before /\ es_link after = es_link before /\ es_pins after = es_pins before.
Proof.
  induction count as [|count IH]; intros before values after Run; cbn [fresh_n] in Run.
  - inversion Run. repeat split.
  - apply EaControlSound.mbind_ok in Run as (name & middle & Fresh & Run).
    apply EaControlSound.mbind_ok in Run as (tail & finish & Tail & Run).
    injection Run as _ <-. specialize (IH _ _ _ Tail).
    unfold fresh in Fresh. injection Fresh as _ <-. exact IH.
Qed.

Local Lemma reserve_keeps_origins : forall table before slot after,
  reserve_code table before = Ok (slot, after) ->
  es_lamcx after = es_lamcx before /\ es_link after = es_link before /\ es_pins after = es_pins before.
Proof.
  intros table before slot after Run. unfold reserve_code, mbind, fresh, ret in Run.
  injection Run as _ <-. repeat split.
Qed.

Theorem write_lambda_keeps_generated_post : forall bound W P slot body C before written after,
  read_code before FunctionTable (slot_index slot) = None ->
  read_origin before FunctionTable (slot_index slot) = None ->
  write_fn_code slot body before = Ok (tt, written) ->
  put_lamcx slot C written = Ok (tt, after) ->
  GeneratedPost bound after before /\
  read_code after FunctionTable (slot_index slot) = Some body /\
  read_origin after FunctionTable (slot_index slot) = Some (c_sub C) /\
  (TypedLinkCache W P before -> TypedLinkCache W P after).
Proof.
  intros bound W P slot body C before written after NoCode NoOrigin Write Note.
  unfold write_fn_code, set_fn_body in Write. injection Write as <-.
  unfold put_lamcx in Note. injection Note as <-.
  split.
  - constructor; [apply Nat.le_refl| |].
    + intros [] n found _ Read; [|exact Read].
      cbn [read_code es_fns]. destruct (Nat.eq_dec n (slot_index slot)) as [->|Other].
      * rewrite NoCode in Read. discriminate.
      * now rewrite WorldSound.nget_nset_other.
    + intros [] n found _ Read; [|exact Read].
      cbn [read_origin es_lamcx]. destruct (Nat.eq_dec n (slot_index slot)) as [->|Other].
      * rewrite NoOrigin in Read. discriminate.
      * now rewrite WorldSound.nget_nset_other.
  - split.
    + cbn [read_code es_fns]. apply WorldSound.nget_nset_same.
    + split.
      * cbn [read_origin es_lamcx]. now rewrite WorldSound.nget_nset_same.
      * unfold TypedLinkCache, link_pins_at. now cbn [es_link es_pins].
Qed.

Local Definition LambdaAnnotation (W : world) (F : frame) (C : cx) (this : option ty)
    (expected : ty) (annotation : option aty) : Prop :=
  match annotation with
  | Some syntax => exists actual,
      TyOf W (fr_mod F) (c_ty C) this syntax actual /\ MergeEq (c_merge C) actual expected
  | None => True
  end.

Local Lemma check_lambda_annotation_sound : forall E F C this expected annotation message before value after,
  PublicContextFacts.CxSection C ->
  check_lambda_annotation E F C this expected annotation message before = Ok (value, after) ->
  after = before /\ LambdaAnnotation (e_W E) F C this expected annotation.
Proof.
  intros E F C this expected [syntax|] message before value after Section Run.
  - unfold check_lambda_annotation in Run.
    apply EaControlSound.mbind_ok in Run as (actual & middle & Read & Run).
    unfold pure in Read.
    destruct (EaTypes.ty_of (e_W E) (e_fuel E) (fr_mod F) (c_ty C) this syntax)
      as [found|error] eqn:TypeRead; [|discriminate].
    injection Read as <- <-.
    destruct (ty_eqb (canon C found) (canon C expected)) eqn:Agree; [|discriminate].
    injection Run as _ <-. split; [reflexivity|].
    exists found. split; [eapply EaTypesSound.ty_of_sound; exact TypeRead|].
    apply (PublicContextFacts.context_canon_iff_mergeeq C Section).
    now apply PublicTypeFacts.ty_eqb_spec.
  - injection Run as _ <-. split; [reflexivity|exact I].
Qed.

Local Lemma check_lambda_parameters_sound : forall E F C this ps types
    (message : name -> ty -> ty -> string) before value after,
  PublicContextFacts.CxSection C ->
  List.length ps = List.length types ->
  miter (fun q => check_lambda_annotation E F C this (snd q) (snd (fst q))
      (fun actual => message (fst (fst q)) actual (snd q))) (combine ps types) before = Ok (value, after) ->
  after = before /\ Forall2 (fun q expected => LambdaAnnotation (e_W E) F C this expected (snd q)) ps types.
Proof.
  intros E F C this ps. induction ps as [|[name ann] ps IH];
    intros [|ty types] message before value after Section Length Run; try discriminate Length.
  - injection Run as _ <-. split; [reflexivity|constructor].
  - injection Length as Length. cbn [combine miter] in Run.
    apply EaControlSound.mbind_ok in Run as (checked & middle & Check & Run).
    apply check_lambda_annotation_sound in Check as [-> Annotation]; [|exact Section].
    destruct (IH types message before value after Section Length Run) as [-> Remaining].
    split; [reflexivity|now constructor].
Qed.

Local Lemma numbered_names_lookup : forall names first n,
  In n names -> exists slot, slook n (number_from first names) = Some slot.
Proof.
  induction names as [|name names IH]; intros first n Member; [contradiction|].
  cbn [number_from slook]. destruct (String.eqb_spec n name) as [->|Different].
  - exists first. reflexivity.
  - destruct Member as [Equal|Member]; [congruence|]. now apply IH.
Qed.

Local Lemma mk_frame_vars_named : forall F receiver parameters names n,
  fr_var F = mk_frame_vars receiver parameters names ->
  In n (parameters ++ names) -> Named F n.
Proof.
  intros F receiver parameters names n Vars Member. unfold Named. rewrite Vars.
  unfold mk_frame_vars.
  destruct (EaAllocationSound.suniq_exact (parameters ++ names)) as [_ Members].
  apply Members in Member.
  destruct (numbered_names_lookup _ (if receiver then 1 else 0) _ Member) as [slot ->].
  discriminate.
Qed.

Local Lemma map_err_success : forall A message (action : M A) before value after,
  map_err message action before = Ok (value, after) -> action before = Ok (value, after).
Proof.
  intros A message action before value after Run. unfold map_err in Run.
  destruct (action before) as [[found finish]|error]; [exact Run|discriminate].
Qed.

Section Lambda.
Variables (E : env) (R : rec) (bound : nat) (entry : string) (P : program) (final : est).
Let W := e_W E.
Hypotheses
  (Declared : forall n d, w_decl W n = Some d -> n < bound)
  (Assembled : EaMain.assemble E bound entry (init_state bound) = Ok (P, final))
  (NestedEffect : forall F C block expected, PreservesOldCode (r_blke R F C block expected)).

Theorem lambda_provider_sound : forall what F C ps annotation body sg this before pid prefix needs after,
  PublicContextFacts.CxSection C ->
  PublicCallRealize.KeysMatch (c_provisions C) ->
  bound <= es_next before -> CodeTablesBound before ->
  (forall n, es_next before <= n -> read_origin before FunctionTable n = None) ->
  (forall v, In v (value_provision_keys (c_provisions C)) -> v < es_next before) ->
  TypedLinkCache W P before ->
  GeneratedPost bound final after ->
  (forall caps capvals captured slot start code result finish,
    fresh_n (List.length caps) before = Ok (capvals, captured) ->
    reserve_code FunctionTable captured = Ok (slot, start) ->
    TypesLambdaCaptures W F C (map fst ps) body caps capvals ->
    TypedLinkCache W P start -> GeneratedPost bound final finish ->
    let Fl := lambda_body_frame E F C (map fst ps) sg this body caps in
    let Cl := lambda_parameter_context (EaBind.captured_cx C caps capvals) (map fst ps) (sg_params sg) in
    r_blke R Fl Cl (ABlock [] (Some body)) (Some (sg_ret sg)) start = Ok ((code, result), finish) ->
    BlkE W P Fl Cl (ABlock [] (Some body)) (Some (sg_ret sg)) code result /\ TypedLinkCache W P finish) ->
  EaBind.lambda_provider E R what F C ps annotation body sg this before = Ok ((pid, prefix, needs), after) ->
  LambdaProvider W P F C ps annotation body sg this pid prefix needs /\ TypedLinkCache W P after.
Proof.
  intros what F C ps annotation body sg this before pid prefix needs after
    Section Matching Floor Tables Future Values Cache Post NestedTyping Run.
  unfold EaBind.lambda_provider in Run. fold W in Run.
  apply EaControlSound.mbind_ok in Run as (checked_length & s1 & Length & Run).
  apply successful_link_check in Length as [Length ->]. apply Nat.eqb_eq in Length.
  apply EaControlSound.mbind_ok in Run as (checked_names & s2 & Names & Run).
  destruct (suniq_dup (map fst ps)) eqn:Distinct; [discriminate Names|].
  injection Names as _ <-. apply EaNumbering.checked_parameters_distinct in Distinct.
  apply EaControlSound.mbind_ok in Run as (checked_parameters & s3 & Parameters & Run).
  destruct (check_lambda_parameters_sound E F C this ps (sg_params sg)
    (lambda_parameter_message E what) before checked_parameters s3 Section Length Parameters)
    as [-> ParametersTyped].
  apply EaControlSound.mbind_ok in Run as (checked_result & s4 & Annotation & Run).
  destruct (check_lambda_annotation_sound E F C this (sg_ret sg) annotation
    (lambda_return_message E what (sg_ret sg)) before checked_result s4 Section Annotation)
    as [-> AnnotationTyped].
  set (parameters := map fst ps) in *.
  set (caps := EaBind.lambda_capture_names E F C parameters body) in *.
  apply EaControlSound.mbind_ok in Run as (capvals & captured & Captures & Run).
  assert (FreshSource : forall n, bound <= n -> Fresh W n).
  { intros n Above. unfold Fresh. destruct (w_decl W n) as [d|] eqn:Found; [|reflexivity].
    pose proof (Declared _ _ Found). lia. }
  destruct (EaAllocationSound.lambda_captures_from_counter E F C parameters body before
    ltac:(intros; apply FreshSource; lia) Values) as (allocated & next & Exact & Counter & CaptureTypes).
  change (fresh_n (List.length caps) before = Ok (allocated, next)) in Exact.
  rewrite Captures in Exact. injection Exact as <- <-.
  pose proof (code_effect_fresh_n _ _ _ _ Captures) as CaptureEffect.
  pose proof (code_bound_preserved _ _ CaptureEffect Tables) as CapturedTables.
  destruct (fresh_n_keeps_origins _ _ _ _ Captures) as (CaptureOrigins & CaptureLinks & CapturePins).
  apply EaControlSound.mbind_ok in Run as (slot & reserved & Reserve & Run).
  destruct (reserve_code_spec W bound FunctionTable captured ltac:(lia) FreshSource CapturedTables)
    as (found & next & Reserved & Index & Next & _ & _ & Vacant & _).
  rewrite Reserve in Reserved. injection Reserved as <- <-.
  destruct (reserve_keeps_origins _ _ _ _ Reserve) as (ReserveOrigins & ReserveLinks & ReservePins).
  set (Cl := EaBind.captured_cx C caps capvals) in *.
  set (Fl := lambda_body_frame E F C parameters sg this body caps) in *.
  set (inner := lambda_parameter_context Cl parameters (sg_params sg)) in *.
  apply EaControlSound.mbind_ok in Run as (checked_captures & s5 & CapturedNames & Run).
  apply successful_link_check in CapturedNames as [CapturedNames ->].
  apply EaControlSound.mbind_ok in Run as ([code result] & elaborated & Body & Run).
  apply map_err_success in Body.
  pose proof (NestedEffect _ _ _ _ _ _ _ Body) as BodyEffect.
  assert (Issued : slot_index slot < es_next reserved) by lia.
  pose proof (code_transition_preserves_reservation _ _ _ BodyEffect Issued Vacant FunctionTable) as NoCode.
  assert (NoOrigin : read_origin elaborated FunctionTable (slot_index slot) = None).
  { rewrite (old_origins_preserved _ _ BodyEffect FunctionTable _ Issued). cbn [read_origin].
    rewrite ReserveOrigins, CaptureOrigins. apply Future. lia. }
  apply EaControlSound.mbind_ok in Run as (checked_fit & s6 & Fit & Run).
  apply successful_link_check in Fit as [Fit ->]. apply EaControlSound.fitsb_exact in Fit.
  apply EaControlSound.mbind_ok in Run as (wrote & written & Write & Run).
  apply EaControlSound.mbind_ok in Run as (noted & finish & Note & Run).
  destruct wrote, noted. injection Run as <- <- <- <-.
  destruct (write_lambda_keeps_generated_post bound W P _ _ _ _ _ _ NoCode NoOrigin Write Note)
    as (PostBody & Stored & Origin & KeepCache).
  pose proof (generated_post_compose _ _ _ _ PostBody Post) as NestedPost.
  assert (ReservedCache : TypedLinkCache W P reserved).
  { unfold TypedLinkCache, link_pins_at in *. rewrite ReserveLinks, CaptureLinks, ReservePins, CapturePins.
    exact Cache. }
  destruct (NestedTyping caps capvals _ _ _ _ _ _ Captures Reserve
    CaptureTypes ReservedCache NestedPost Body)
    as [TypedBody BodyCache].
  assert (Owned : bound <= slot_index slot < es_next finish).
  { pose proof (counter_grows _ _ BodyEffect).
    unfold put_lamcx in Note. injection Note as <-.
    unfold write_fn_code, set_fn_body in Write. injection Write as <-.
    cbn [es_next]. lia. }
  pose proof (generated_code_in_assembly _ _ _ _ _ _ Assembled Post FunctionTable _ _ Owned Stored) as CodeInProgram.
  pose proof (generated_lambda_origin_in_assembly _ _ _ _ _ _ _ _ Assembled Post Owned Origin) as OriginInProgram.
  set (keys := EaBind.lambda_need_keys Cl (PIdx (pindex (c_provisions Cl)))) in *.
  destruct (lambda_need_keys_exact Cl (captured_cx_keys_match _ _ _ Matching)) as [KeysDistinct Keys].
  split; [|exact (KeepCache BodyCache)].
  eapply (@LP W P F C ps annotation body sg this (slot_index slot) caps capvals Cl Fl code result keys
    (map (fun k => (rkof_i Cl (PIdx (pindex (c_provisions Cl))) k,
                   rkof_i C (PIdx (pindex (c_provisions C))) k)) keys));
    try reflexivity; try eassumption.
  - apply FreshSource. lia.
  - apply incl_refl.
  - eapply EaNumbering.mk_frame_vars_numbering; [reflexivity|exact Distinct].
  - apply Forall_forall. intros n Member.
    eapply mk_frame_vars_named; [reflexivity|].
    apply in_or_app. right. apply in_or_app. now left.
  - apply Forall_forall. intros n Member. unfold Named.
    pose proof (proj1 (forallb_forall _ _) CapturedNames n Member) as Present.
    cbn beta in Present. destruct (slook n (fr_var F)); [discriminate|discriminate Present].
  - apply map_ext. intro k.
    change ((rkof_i Cl (pix_of true (c_provisions Cl)) k, rkof_i C (pix_of true (c_provisions C)) k) =
      (rkey_of Cl k, rkey_of C k)).
    now rewrite !EaSatisfiesSound.rkof_i_exact.
Qed.

End Lambda.
