(* Frame core and the shared synthesis traversal instance. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Export ElabFrameCore.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsStable ElabFrameAtoms SynthTraversal.
Include ElabFrameCore.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint reqs fn_info check_assumed_requirements check_assumes].
Lemma ok_synth_group : forall fuel,
  (forall c e x exp, ok (synth fuel c e x exp)) /\
  (forall c e a exp, ok (synth_arg fuel c e a exp)) /\
  (forall c e b exp, ok (synth_block fuel c e b exp)) /\
  (forall c e ss, ok (check_stmts fuel c e ss)) /\
  (forall c e s, ok (check_stmt fuel c e s)) /\
  (forall c e y, ok (check_bool fuel c e y)) /\
  (forall c e x exp, ok (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, ok (synth_match fuel c e s arms exp)) /\
  (forall g, ok (const_val fuel g)) /\
  (forall c e p a, ok (synth_path fuel c e p a)) /\
  (forall c e nm sg args, ok (check_args fuel c e nm sg args)) /\
  (forall c e p a args, ok (synth_call fuel c e p a args)) /\
  (forall c e p a fs, ok (synth_record fuel c e p a fs)) /\
  (forall c e o n, ok (synth_field fuel c e o n)) /\
  (forall c e o p args, ok (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, ok (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, ok (apply_family fuel c e fg th want)) /\
  (forall c e info, ok (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, ok (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, ok (lang_call fuel c e o n args why)) /\
  (forall c e ds why, ok (int_literal fuel c e ds why)) /\
  (forall c e v why, ok (float_of fuel c e v why)) /\
  (forall c e text, ok (synth_literal fuel c e text)) /\
  (forall c e sp y, ok (check_bind fuel c e sp y)) /\
  (forall c e y, ok (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, ok (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, ok (wrapper fuel pv)) /\
  (forall tg, ok (forwarder fuel tg)) /\
  (forall g, ok (lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, ok (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, ok (apply_functor fuel c e sp)) /\
  (forall c e g, ok (apply_body fuel c e g)) /\
  (forall c e fg, ok (apply_abstract fuel c e fg)) /\
  (forall c e fg y, ok (bind_functor fuel c e fg y)) /\
  (forall fg bg, ok (link_body fuel fg bg)).
Proof.
  refine (@synth_preservation_group (@ok)
    (fun fuel g => ok (lower_body fuel g)) _
    (@ok_ret) (@ok_bind) (@ok_mmap) (@ok_mfold) (@ok_miter)
    (@ok_with_vars) (fun A m F W H O => O)
    (@ok_wrapper_atom) (@ok_forwarder_atom) (@ok_lower_body_atom)
    (@ok_lambda_provider_atom)).
  intro g; cbn [lower_body]; okt.
Qed.

Lemma ok_synth : forall fuel c e x exp, ok (synth fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (ok_synth_group fuel)) c e x exp). Qed.
#[export] Hint Resolve ok_synth : okdb.

Lemma ok_synth_arg : forall fuel c e a exp, ok (synth_arg fuel c e a exp).
Proof. intros fuel c e a exp. exact ((proj1 (proj2 (ok_synth_group fuel))) c e a exp). Qed.
#[export] Hint Resolve ok_synth_arg : okdb.

Lemma ok_synth_block : forall fuel c e b exp, ok (synth_block fuel c e b exp).
Proof. intros fuel c e b exp. exact ((proj1 (proj2 (proj2 (ok_synth_group fuel)))) c e b exp). Qed.
#[export] Hint Resolve ok_synth_block : okdb.

Lemma ok_check_stmts : forall fuel c e ss, ok (check_stmts fuel c e ss).
Proof. intros fuel c e ss. exact ((proj1 (proj2 (proj2 (proj2 (ok_synth_group fuel))))) c e ss). Qed.
#[export] Hint Resolve ok_check_stmts : okdb.

Lemma ok_check_stmt : forall fuel c e s, ok (check_stmt fuel c e s).
Proof. intros fuel c e s. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))) c e s). Qed.
#[export] Hint Resolve ok_check_stmt : okdb.

Lemma ok_check_bool : forall fuel c e y, ok (check_bool fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))) c e y). Qed.
#[export] Hint Resolve ok_check_bool : okdb.

Lemma ok_synth_if : forall fuel c e x exp, ok (synth_if fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))) c e x exp). Qed.
#[export] Hint Resolve ok_synth_if : okdb.

Lemma ok_synth_match : forall fuel c e s arms exp, ok (synth_match fuel c e s arms exp).
Proof. intros fuel c e s arms exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))) c e s arms exp). Qed.
#[export] Hint Resolve ok_synth_match : okdb.

Lemma ok_const_val : forall fuel g, ok (const_val fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))) g). Qed.
#[export] Hint Resolve ok_const_val : okdb.

Lemma ok_synth_path : forall fuel c e p a, ok (synth_path fuel c e p a).
Proof. intros fuel c e p a. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))) c e p a). Qed.
#[export] Hint Resolve ok_synth_path : okdb.

Lemma ok_check_args : forall fuel c e nm sg args, ok (check_args fuel c e nm sg args).
Proof. intros fuel c e nm sg args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))) c e nm sg args). Qed.
#[export] Hint Resolve ok_check_args : okdb.

Lemma ok_synth_call : forall fuel c e p a args, ok (synth_call fuel c e p a args).
Proof. intros fuel c e p a args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))) c e p a args). Qed.
#[export] Hint Resolve ok_synth_call : okdb.

Lemma ok_synth_record : forall fuel c e p a fs, ok (synth_record fuel c e p a fs).
Proof. intros fuel c e p a fs. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))) c e p a fs). Qed.
#[export] Hint Resolve ok_synth_record : okdb.

Lemma ok_synth_field : forall fuel c e o n, ok (synth_field fuel c e o n).
Proof. intros fuel c e o n. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))) c e o n). Qed.
#[export] Hint Resolve ok_synth_field : okdb.

Lemma ok_synth_method : forall fuel c e o p args, ok (synth_method fuel c e o p args).
Proof. intros fuel c e o p args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))) c e o p args). Qed.
#[export] Hint Resolve ok_synth_method : okdb.

Lemma ok_provided_call : forall fuel c e ox oty m args, ok (provided_call fuel c e ox oty m args).
Proof. intros fuel c e ox oty m args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))) c e ox oty m args). Qed.
#[export] Hint Resolve ok_provided_call : okdb.

Lemma ok_apply_family : forall fuel c e fg th want, ok (apply_family fuel c e fg th want).
Proof. intros fuel c e fg th want. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))) c e fg th want). Qed.
#[export] Hint Resolve ok_apply_family : okdb.

Lemma ok_demand_needs : forall fuel c e info, ok (demand_needs fuel c e info).
Proof. intros fuel c e info. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))) c e info). Qed.
#[export] Hint Resolve ok_demand_needs : okdb.

Lemma ok_synth_operator : forall fuel c e lm n op operand args, ok (synth_operator fuel c e lm n op operand args).
Proof. intros fuel c e lm n op operand args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))) c e lm n op operand args). Qed.
#[export] Hint Resolve ok_synth_operator : okdb.

Lemma ok_lang_call : forall fuel c e o n args why, ok (lang_call fuel c e o n args why).
Proof. intros fuel c e o n args why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))) c e o n args why). Qed.
#[export] Hint Resolve ok_lang_call : okdb.

Lemma ok_int_literal : forall fuel c e ds why, ok (int_literal fuel c e ds why).
Proof. intros fuel c e ds why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))) c e ds why). Qed.
#[export] Hint Resolve ok_int_literal : okdb.

Lemma ok_float_of : forall fuel c e v why, ok (float_of fuel c e v why).
Proof. intros fuel c e v why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))) c e v why). Qed.
#[export] Hint Resolve ok_float_of : okdb.

Lemma ok_synth_literal : forall fuel c e text, ok (synth_literal fuel c e text).
Proof. intros fuel c e text. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))))) c e text). Qed.
#[export] Hint Resolve ok_synth_literal : okdb.

Lemma ok_check_bind : forall fuel c e sp y, ok (check_bind fuel c e sp y).
Proof. intros fuel c e sp y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))) c e sp y). Qed.
#[export] Hint Resolve ok_check_bind : okdb.

Lemma ok_expect_fn_provider : forall fuel c e y, ok (expect_fn_provider fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))))))) c e y). Qed.
#[export] Hint Resolve ok_expect_fn_provider : okdb.

Lemma ok_match_fn_sig : forall fuel c e pv wanted kind, ok (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel c e pv wanted kind. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))))) c e pv wanted kind). Qed.
#[export] Hint Resolve ok_match_fn_sig : okdb.

Lemma ok_wrapper : forall fuel pv, ok (wrapper fuel pv).
Proof. intros fuel pv. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))))))))) pv). Qed.
#[export] Hint Resolve ok_wrapper : okdb.

Lemma ok_forwarder : forall fuel tg, ok (forwarder fuel tg).
Proof. intros fuel tg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))))))) tg). Qed.
#[export] Hint Resolve ok_forwarder : okdb.

Lemma ok_lower_body : forall fuel g, ok (lower_body fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))))))))))) g). Qed.
#[export] Hint Resolve ok_lower_body : okdb.

Lemma ok_lambda_provider : forall fuel c e ps rt body bound sg this what, ok (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel c e ps rt body bound sg this what. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))))))))) c e ps rt body bound sg this what). Qed.
#[export] Hint Resolve ok_lambda_provider : okdb.

Lemma ok_apply_functor : forall fuel c e sp, ok (apply_functor fuel c e sp).
Proof. intros fuel c e sp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))))))))))))) c e sp). Qed.
#[export] Hint Resolve ok_apply_functor : okdb.

Lemma ok_apply_body : forall fuel c e g, ok (apply_body fuel c e g).
Proof. intros fuel c e g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))))))))))) c e g). Qed.
#[export] Hint Resolve ok_apply_body : okdb.

Lemma ok_apply_abstract : forall fuel c e fg, ok (apply_abstract fuel c e fg).
Proof. intros fuel c e fg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))))))))))))))) c e fg). Qed.
#[export] Hint Resolve ok_apply_abstract : okdb.

Lemma ok_bind_functor : forall fuel c e fg y, ok (bind_functor fuel c e fg y).
Proof. intros fuel c e fg y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))))))))))))) c e fg y). Qed.
#[export] Hint Resolve ok_bind_functor : okdb.

Lemma ok_link_body : forall fuel fg bg, ok (link_body fuel fg bg).
Proof. intros fuel fg bg. exact ((proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))))))))))))) fg bg). Qed.
#[export] Hint Resolve ok_link_body : okdb.


Lemma ok_entry_zero_width : forall fuel e t, ok (entry_zero_width fuel e t).
Proof. induction fuel; intros e t; cbn [entry_zero_width]; okt. Qed.
#[export] Hint Resolve ok_entry_zero_width : okdb.

Lemma ok_check_main_arity : forall g, ok (check_main_arity g).
Proof. intros g. unfold check_main_arity. okt. Qed.
#[export] Hint Resolve ok_check_main_arity : okdb.

Lemma ok_check_main_signature : forall g info, ok (check_main_signature g info).
Proof. intros g info. unfold check_main_signature. okt. Qed.
#[export] Hint Resolve ok_check_main_signature : okdb.

Lemma ok_check_platform_entry_view : forall e k entry,
    ok (check_platform_entry_view e k entry).
Proof. intros. unfold check_platform_entry_view. okt. Qed.
#[export] Hint Resolve ok_check_platform_entry_view : okdb.

Lemma ok_elab_program : ok elab_program.
Proof. unfold elab_program. okt. Qed.

(* ======================================================================== *)
(* The frame of a whole elaboration (WorldSound.v's TODO_syms_frame and      *)
(* TODO_fld_injective)                                                       *)
(* ======================================================================== *)

Lemma load_world_syms : forall b w, load_world b = Good w ->
  0 < w_nsyms w /\ forall i, w_nsyms w <= i -> nget (w_syms w) i = None.
Proof.
  intros b w H. unfold load_world in H. apply rbind_good in H as ([e st0] & Hl & H). injection H as <-.
  destruct (load_spec b _ _ _ _ _ _ Hl (linv_init b)) as (L & _).
  split; [exact (li_pos _ _ L) | exact (li_above _ _ L)].
Qed.

Theorem elab_frame : forall b w p s, load_world b = Good w -> elab_program (s_init w) = Good (p, s) ->
  st_w s = w /\
  (* the loader's symbols are kept, unchanged *)
  (forall i g, nget (w_syms w) i = Some g -> nget (st_syms s) i = Some g) /\
  (* every symbol elaboration makes is no source symbol, and is fresh in the
     reading of the bundle (Audit.v Part V's `Fresh`) *)
  (forall i g, nget (w_syms w) i = None -> nget (st_syms s) i = Some g ->
     gs_origin g <> OSource /\ forall R F, A.Fresh (W_read w R F) i) /\
  (* record fields are numbered 1, 2, …, so fld_ext of them is injective *)
  Numbered (st_flds s) /\
  (forall a c, fld_ext (st_flds s) (List.length (st_flds s)) a = fld_ext (st_flds s) (List.length (st_flds s)) c -> a = c) /\
  (* every requirement list it cached is without repeats, of a symbol, and
     for a val, fn, method, context or functor Part V's AssumeTypes, for a
     type or unit none *)
  ReqOK s /\
  (* the cached lists of tags and aliases are closed and least, and each
     listed parameter is a type its assume nesting requires (ReqsSolve.v) *)
  RC (W0r w) (st_reqs s) /\ RL (W0r w) (st_reqs s).
Proof.
  intros b w p s Hw He. destruct (load_world_syms b w Hw) as [Hpos Habove].
  assert (I0 : Inv (s_init w)).
  { constructor; cbn [st_next st_syms st_flds st_w st_reqs s_init].
    - exact Habove.
    - split; [constructor | reflexivity].
    - exact (load_world_ids b w Hw).
    - intros b' Hb' i g Hg. exact Hg.
    - intros b' Hb' i g Hn Hg. rewrite Hn in Hg. discriminate.
    - intros b' Hb' x l Hx. destruct x; discriminate Hx.
    - intros b' Hb' i g Hn Hg. rewrite Hn in Hg. discriminate. }
  destruct (ok_elab_program _ _ _ I0 He) as [[_ Nf _ _ _ Ir _] [[Fw _ Fk Fn Fa _] Frel]].
  cbn [st_w st_syms st_next s_init] in Fw, Fk, Fn, Fa.
  split; [exact Fw|]. split; [exact Fk|]. split.
  - intros i g Hn Hg. split; [exact (Fn i g Hn Hg)|]. intros R F. split.
    + cbn [A.w_decl W_read]. rewrite Hn. reflexivity.
    + pose proof (Fa i g Hn Hg). lia.
  - split; [exact Nf|]. split; [destruct (numbered_inj _ Nf) as [Ninj Nb]; exact (fld_ext_inj _ _ Ninj Nb)|].
    split; [apply (Ir b); rewrite Fw; exact Hw|].
    assert (R0 : RelOK (s_init w)) by (intros b' _; split; [apply RC_empty | apply RL_empty]).
    rewrite <- Fw. apply (Frel R0 b). rewrite Fw. exact Hw.
Qed.

Corollary elaborate_frame : forall b p, elaborate b = Good p ->
  exists w s, load_world b = Good w /\ elab_program (s_init w) = Good (p, s) /\
  st_w s = w /\
  (forall i g, nget (w_syms w) i = Some g -> nget (st_syms s) i = Some g) /\
  (forall i g, nget (w_syms w) i = None -> nget (st_syms s) i = Some g ->
     gs_origin g <> OSource /\ forall R F, A.Fresh (W_read w R F) i) /\
  Numbered (st_flds s) /\
  (forall a c, fld_ext (st_flds s) (List.length (st_flds s)) a = fld_ext (st_flds s) (List.length (st_flds s)) c -> a = c) /\
  ReqOK s /\ RC (W0r w) (st_reqs s) /\ RL (W0r w) (st_reqs s).
Proof.
  intros b p H. unfold elaborate in H. destruct (load_world b) as [w|m] eqn:Hw; [|discriminate].
  cbn [rbind] in H. change (match elab_program (s_init w) with Good (p, _) => Good p | Bad m => Bad m end = Good p) in H.
  destruct (elab_program (s_init w)) as [[p' s]|m] eqn:He; [|discriminate].
  injection H as <-. exists w, s. split; [reflexivity|]. split; [exact He|].
  exact (elab_frame b w p' s Hw He).
Qed.
