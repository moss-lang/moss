(* ------------------------------------------------------------------------ *)
(* ElabConsts.v — every entry of const_val's cache, in a run's last state,  *)
(* is its val's ExConst premises (Track A; Audit note 66, BodySound's      *)
(* Later.lt_consts).                                                        *)
(*                                                                           *)
(* ElabInv.v's machinery, for st_consts: the cache's invariant is ConstInv   *)
(* (ConstInv: every entry is a ConstFact), and what the last state gives back is     *)
(* Later0, BodySound's Later without lt_consts.  Every helper leaves the   *)
(* cache alone (keeps st_consts, section 1); every function of the synth   *)
(* group keeps ConstInv (section 3): the one write, const_val's fresh case, *)
(* is BodyConst.const_fresh, from the state after the initializer's        *)
(* synthesis, whose cache is valid by the induction.  So a run from the     *)
(* empty cache ends with a valid one (elab_consts), and Later of its last *)
(* state is Later0 of it (later_run).                                       *)
(*                                                                           *)
(* What this file assumes (section hypotheses):                             *)
(*   HOK  BodySound.bodies_sound's conclusion (OK at every fuel), which    *)
(*        Pending gives;                                                    *)
(*   Hty  Pending's pd_ty, as BodyConst.v.                                   *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv BodySound BodyConst.
Import ListNotations.
Local Open Scope list_scope.

Module A := Audit.Elaboration.

Local Strategy opaque [Nat.of_num_uint FUEL].

(* ======================================================================== *)
(* 1. The helpers leave the cache alone                                      *)
(* ======================================================================== *)

#[export] Hint Extern 1 (keeps st_consts (ret _)) => apply keeps_ret : keepsdb.

Lemma kc_mentioned_nominals : forall fuel t mi, keeps st_consts (mentioned_nominals fuel t mi).
Proof. induction fuel; fintros; cbn [mentioned_nominals]; keepst. Qed.
#[export] Hint Resolve kc_mentioned_nominals : keepsdb.

Lemma kc_walk_specs : forall fuel mi sp out, keeps st_consts (walk_specs fuel mi sp out).
Proof. induction fuel; fintros; cbn [walk_specs]; keepst. Qed.
#[export] Hint Resolve kc_walk_specs : keepsdb.

Lemma kc_free_type_syms_group : forall fuel,
  (forall t mi out, keeps st_consts (free_type_syms fuel t mi out)) /\
  (forall s, keeps st_consts (type_requirements fuel s)) /\
  (forall root, keeps st_consts (solve_requirements fuel root)) /\
  (forall g, keeps st_consts (listed_params fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ _))); fintros; cbn [free_type_syms type_requirements solve_requirements listed_params]; keepst. }
  destruct IH as (I0 & I1 & I2 & I3).
  refine (conj _ (conj _ (conj _ _))); fintros; cbn [free_type_syms type_requirements solve_requirements listed_params]; keepst.
Qed.

Lemma kc_free_type_syms : forall fuel t mi out, keeps st_consts (free_type_syms fuel t mi out).
Proof. intros. apply kc_free_type_syms_group. Qed.
Lemma kc_type_requirements : forall fuel s, keeps st_consts (type_requirements fuel s).
Proof. intros. apply kc_free_type_syms_group. Qed.
Lemma kc_solve_requirements : forall fuel root, keeps st_consts (solve_requirements fuel root).
Proof. intros. apply kc_free_type_syms_group. Qed.
Lemma kc_listed_params : forall fuel g, keeps st_consts (listed_params fuel g).
Proof. intros. apply kc_free_type_syms_group. Qed.
#[export] Hint Resolve kc_free_type_syms kc_type_requirements kc_solve_requirements kc_listed_params : keepsdb.

Lemma kc_show_go : forall fuel t, keeps st_consts (show_go fuel t).
Proof. induction fuel; fintros; cbn [show_go]; keepst. Qed.
#[export] Hint Resolve kc_show_go : keepsdb.

Lemma kc_unify : forall fuel e mi a b why, keeps st_consts (unify fuel e mi a b why).
Proof. induction fuel; fintros; cbn [unify]; keepst. Qed.
#[export] Hint Resolve kc_unify : keepsdb.

Lemma kc_elab_type_group : forall fuel,
  (forall t mi tm e this, keeps st_consts (elab_type fuel t mi tm e this)) /\
  (forall e mi bs outer, keeps st_consts (app_subst fuel e mi bs outer)) /\
  (forall mi g tm e, keeps st_consts (symbol_type fuel mi g tm e)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ _)); fintros; cbn [elab_type app_subst symbol_type]; keepst. }
  destruct IH as (I0 & I1 & I2).
  refine (conj _ (conj _ _)); fintros; cbn [elab_type app_subst symbol_type]; keepst.
Qed.

Lemma kc_elab_type : forall fuel t mi tm e this, keeps st_consts (elab_type fuel t mi tm e this).
Proof. intros. apply kc_elab_type_group. Qed.
Lemma kc_app_subst : forall fuel e mi bs outer, keeps st_consts (app_subst fuel e mi bs outer).
Proof. intros. apply kc_elab_type_group. Qed.
Lemma kc_symbol_type : forall fuel mi g tm e, keeps st_consts (symbol_type fuel mi g tm e).
Proof. intros. apply kc_elab_type_group. Qed.
#[export] Hint Resolve kc_elab_type kc_app_subst kc_symbol_type : keepsdb.

Lemma kc_result_mentions_go_group : forall fuel,
  (forall self mi specs out, keeps st_consts (result_mentions_go fuel self mi specs out)) /\
  (forall g, keeps st_consts (functor_fixed_go fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); fintros; cbn [result_mentions_go functor_fixed_go]; keepst. }
  destruct IH as (I0 & I1).
  refine (conj _ _); fintros; cbn [result_mentions_go functor_fixed_go]; keepst.
Qed.

Lemma kc_result_mentions_go : forall fuel self mi specs out, keeps st_consts (result_mentions_go fuel self mi specs out).
Proof. intros. apply kc_result_mentions_go_group. Qed.
Lemma kc_functor_fixed_go : forall fuel g, keeps st_consts (functor_fixed_go fuel g).
Proof. intros. apply kc_result_mentions_go_group. Qed.
#[export] Hint Resolve kc_result_mentions_go kc_functor_fixed_go : keepsdb.

Lemma kc_add_symbol_item_group : forall fuel,
  (forall e mi g subst needs, keeps st_consts (add_symbol_item fuel e mi g subst needs)) /\
  (forall e mi sp outer needs, keeps st_consts (add_spec fuel e mi sp outer needs)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); fintros; cbn [add_symbol_item add_spec]; keepst. }
  destruct IH as (I0 & I1).
  refine (conj _ _); fintros; cbn [add_symbol_item add_spec]; keepst.
Qed.

Lemma kc_add_symbol_item : forall fuel e mi g subst needs, keeps st_consts (add_symbol_item fuel e mi g subst needs).
Proof. intros. apply kc_add_symbol_item_group. Qed.
Lemma kc_add_spec : forall fuel e mi sp outer needs, keeps st_consts (add_spec fuel e mi sp outer needs).
Proof. intros. apply kc_add_symbol_item_group. Qed.
#[export] Hint Resolve kc_add_symbol_item kc_add_spec : keepsdb.

Lemma kc_ety_ty_go : forall fuel t, keeps st_consts (ety_ty_go fuel t).
Proof. induction fuel; fintros; cbn [ety_ty_go]; keepst. Qed.
#[export] Hint Resolve kc_ety_ty_go : keepsdb.

Lemma kc_slots_go : forall fuel e t visiting, keeps st_consts (slots_go fuel e t visiting).
Proof. induction fuel; fintros; cbn [slots_go]; keepst. Qed.
#[export] Hint Resolve kc_slots_go : keepsdb.

Lemma kc_expand_spec : forall fuel mi sp out, keeps st_consts (expand_spec fuel mi sp out).
Proof. induction fuel; fintros; cbn [expand_spec]; keepst. Qed.
#[export] Hint Resolve kc_expand_spec : keepsdb.

Lemma kc_functor_total : forall fuel g, keeps st_consts (functor_total fuel g).
Proof. induction fuel; fintros; cbn [functor_total]; keepst. Qed.
#[export] Hint Resolve kc_functor_total : keepsdb.

Lemma kc_provide_spec : forall fuel e fg mi sp outer out, keeps st_consts (provide_spec fuel e fg mi sp outer out).
Proof. induction fuel; fintros; cbn [provide_spec]; keepst. Qed.
#[export] Hint Resolve kc_provide_spec : keepsdb.

Lemma kc_family_leaves_ok : forall fuel mi ps specs, keeps st_consts (family_leaves_ok fuel mi ps specs).
Proof. induction fuel; fintros; cbn [family_leaves_ok]; keepst. Qed.
#[export] Hint Resolve kc_family_leaves_ok : keepsdb.

Lemma kc_family_leaves : forall fuel e mi specs outer, keeps st_consts (family_leaves fuel e mi specs outer).
Proof. induction fuel; fintros; cbn [family_leaves]; keepst. Qed.
#[export] Hint Resolve kc_family_leaves : keepsdb.

(* ======================================================================== *)
(* 2. The cache, its invariant, and what the last state gives back           *)
(* ======================================================================== *)

Tactic Notation "bi" hyp(H) "as" ident(a) ident(s1) ident(H1) :=
  apply bind_inv in H as (a & s1 & H1 & H).

Ltac nogood H := exfalso;
  solve [repeat (cbn beta iota zeta in H;
                 first [ exact (err_not_good _ _ _ _ _ _ H) | discriminate H
                       | apply bind_inv in H as (? & ? & ? & H) ])].

Section Run.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation trF := (tr Fl).
Variable b : bundle.
Hypothesis Hload : load_world b = Good w.

(* [Pending's pd_ty, as BodyConst.v] *)
Hypothesis Hty : forall ty c e s d s', ety_of ty (cx_mod c) (tymap e) e (cx_this c) s = Good (d, s') ->
  forall F C, Inv s -> CxRel w Fl c e F C -> Later w P R Fl s' ->
  A.TyOf (W w R Fl) (A.fr_mod F) (A.c_ty C) (A.fr_this F) ty (trF d) /\ WFty d.

(* [BodySound.bodies_sound, given Pending] *)
Hypothesis HOK : forall f, OK w P R Fl f.

(* BodySound's Later without the cache *)
Record Later0 (s : st) : Prop := {
  l0_w : st_w s = w;
  l0_flds : forall a n, sassoc a (st_flds s) = Some n -> Fl a = n;
  l0_unsup : st_unsup s = [];
  l0_lang : st_lang s = lang_modules w;
  l0_reqs : forall x l, nget (st_reqs s) x = Some l -> R x = l
}.

Lemma later0_back : forall s s', Fr s s' -> Later0 s' -> Later0 s.
Proof.
  intros s s' Fs [Lw Lf Lu Ll Lr]. destruct Fs as [[Fw _ _ _ _ Ffl [_ _ _ _ (u & Eu) Elang _]] _ Fx]. constructor.
  - congruence.
  - intros a n H. exact (Lf a n (Ffl a n H)).
  - rewrite Lu in Eu. destruct (st_unsup s); [reflexivity | discriminate].
  - congruence.
  - intros x l H. exact (Lr x l (Fx x l H)).
Qed.

Lemma later0_vars : forall vs n s, Later0 (s_vars vs n s) -> Later0 s.
Proof. intros vs n s [L1 L2 L3 L4 L5]. constructor; assumption. Qed.

Definition ConstInv (s : st) : Prop :=
  forall k x t, nget (st_consts s) k = Some (x, t) -> ConstFact w P R Fl k x t.

Lemma later_of : forall s, Later0 s -> ConstInv s -> Later w P R Fl s.
Proof. intros s [L1 L2 L3 L4 L5] Q. constructor; assumption. Qed.

Definition ConstCache : Cache := {|
  ca_proj := st_consts;
  ca_L := Later0;
  ca_Q := ConstInv;
  ca_L_back := later0_back;
  ca_L_vars := later0_vars;
  ca_Q_vars := fun vs n s Q => Q;
  ca_Q_keep := fun s s' _ _ E _ Q k x t H => Q k x t (eq_ind _ (fun m => nget m k = Some (x, t)) H _ E)
|}.

Notation okc := (okq ConstCache).

(* A symbol const_val is given: the loader's at its number, if any (when
   none, ConstFact asks nothing of the entry). *)
Definition GoodG (g : gsym) : Prop := forall g', nget (w_syms w) (gs_id g) = Some g' -> g' = g.

(* a symbol read from the state is one *)
Lemma okc_bind_sym : forall x B (K : gsym -> M B), (forall g, ok (K g)) ->
  (forall g, GoodG g -> gs_id g = x -> okc (K g)) -> okc (mbind (sym x) K).
Proof.
  intros x B K Ok Hk. split; [apply ok_bind; [unfold sym; okt | exact Ok]|].
  intros s a s' Hi H L Q. bi H as g s1 H1. unfold sym in H1.
  destruct (nget (st_syms s) x) as [g0|] eqn:Ex; [|discriminate]. injection H1 as -> <-.
  destruct (Ok g s a s' Hi H) as [_ F]. pose proof (later0_back _ _ F L) as L1.
  assert (Eid : gs_id g = x) by exact (iv_ids _ Hi x g Ex).
  assert (G : GoodG g).
  { intros g' Hg'. rewrite Eid in Hg'.
    assert (Hb : load_world b = Good (st_w s)) by (rewrite (l0_w _ L1); exact Hload).
    pose proof (iv_loader _ Hi b Hb x g' ltac:(rewrite (l0_w _ L1); exact Hg')) as E. congruence. }
  exact (proj2 (Hk g G Eid) s a s' Hi H L Q).
Qed.

Ltac sym_ext := idtac; lazymatch goal with
  | |- okq _ (mbind (sym _) _) => apply okc_bind_sym; [intro; solve [okt] | ]
  end.

(* ======================================================================== *)
(* 3. Every function of the elaborator keeps the cache valid                 *)
(* ======================================================================== *)

(* the write *)
Lemma okc_const_val_step : forall f, (forall c e x exp, okc (synth f c e x exp)) ->
  forall g, GoodG g -> okc (const_val (S f) g).
Proof.
  intros f IH g G. split; [apply ok_const_val|].
  intros s a s' Hi H L Q. cbn [const_val] in H.
  bi H as s0 s1 Hget. unfold get in Hget. injection Hget as <- <-.
  destruct (nget (st_consts s) (gs_id g)) as [r|] eqn:Ec.
  { injection H as _ <-. exact Q. }
  destruct (gs_assumes g) as [|a0 r0] eqn:Eas; [|nogood H].
  destruct (gs_decl g) as [| | | | |n t0 [i|]| | |] eqn:Ed; try nogood H.
  bi H as declared s2 Hdecl.
  bi H as r3 s3 Hwv. destruct r3 as [v0 vt].
  destruct (fits empty_env vt declared) eqn:Efit; [|nogood H].
  bi H as u s4 Hupd. injection H as _ <-.
  (* the frames *)
  destruct (ok_ety_of t0 (gs_mod g) [] empty_env None s declared s2 Hi Hdecl) as [Hi2 Fs2].
  destruct (ok_with_vars _ _ _ (ok_synth _ _ _ _ _) _ _ _ Hi2 Hwv) as [Hi3 Fs3].
  destruct (ok_consts_add (gs_id g) (v0, declared) _ _ _ Hi3 Hupd) as [_ Fs4].
  pose proof (later0_back _ _ Fs4 L) as L3.
  (* the cache, through the declared type and the initializer *)
  assert (Oe : okc (ety_of t0 (gs_mod g) [] empty_env None)) by (unfold ety_of; okqt).
  pose proof (proj2 Oe s declared s2 Hi Hdecl (later0_back _ _ Fs3 L3) Q) as Q2.
  pose proof (proj2 (okq_with_vars ConstCache _ [] _ (IH _ _ _ _)) s2 (v0, vt) s3 Hi2 Hwv L3 Q2) as Q3.
  change (ConstInv s3) in Q3.
  unfold upd in Hupd. injection Hupd as _ <-.
  unfold ConstCache; cbn [ca_Q]. intros k x t Hk. unfold s_consts in Hk. cbn [st_consts] in Hk.
  destruct (nget (st_consts s3) (gs_id g)) as [y|] eqn:E3; [exact (Q3 k x t Hk)|].
  rewrite nget_nset in Hk. destruct (Nat.eqb_spec k (gs_id g)) as [->|]; [|exact (Q3 k x t Hk)].
  injection Hk as <- <-.
  (* the new entry: the loader's val, or no declaration to read *)
  destruct (nget (w_syms w) (gs_id g)) as [g'|] eqn:Ew.
  2: { intros d n' t0' i' Hd. unfold W in Hd. cbn [A.w_decl W_read] in Hd. rewrite Ew in Hd. discriminate Hd. }
  pose proof (G g' Ew) as Eg. subst g'.
  (* the state after the initializer's synthesis *)
  unfold with_vars in Hwv. bi Hwv as s5 s6 Hg5. unfold get in Hg5. injection Hg5 as <- <-.
  bi Hwv as u5 s7 Hu5. unfold upd in Hu5. injection Hu5 as <- <-.
  bi Hwv as r8 s8 Hsyn. bi Hwv as u8 s9 Hu8. unfold upd in Hu8. injection Hu8 as <- <-. injection Hwv as -> <-.
  assert (L8 : Later0 s8) by exact (later0_vars _ _ _ L3).
  assert (Q8 : ConstInv s8) by exact Q3.
  exact (const_fresh w P R Fl b Hload Hty f (HOK f) g n t0 i s declared s2 v0 vt s8 Ew Eas Ed Hi Hdecl Hsyn Efit
           (later_of _ L8 Q8)).
Qed.

(* the one read of a symbol for const_val *)
Lemma okc_synth_path_step : forall f, (forall g, GoodG g -> okc (const_val f g)) ->
  forall c e p a, okc (synth_path (S f) c e p a).
Proof. intros f IH c e p a. cbn [synth_path]. okqt_with sym_ext. Qed.

Lemma okc_synth_group : forall fuel,
  (forall c e x exp, okc (synth fuel c e x exp)) /\
  (forall c e a exp, okc (synth_arg fuel c e a exp)) /\
  (forall c e b exp, okc (synth_block fuel c e b exp)) /\
  (forall c e ss, okc (check_stmts fuel c e ss)) /\
  (forall c e s, okc (check_stmt fuel c e s)) /\
  (forall c e y, okc (check_bool fuel c e y)) /\
  (forall c e x exp, okc (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, okc (synth_match fuel c e s arms exp)) /\
  (forall g, GoodG g -> okc (const_val fuel g)) /\
  (forall c e p a, okc (synth_path fuel c e p a)) /\
  (forall c e nm sg args, okc (check_args fuel c e nm sg args)) /\
  (forall c e p a args, okc (synth_call fuel c e p a args)) /\
  (forall c e p a fs, okc (synth_record fuel c e p a fs)) /\
  (forall c e o n, okc (synth_field fuel c e o n)) /\
  (forall c e o p args, okc (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, okc (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, okc (apply_family fuel c e fg th want)) /\
  (forall c e info, okc (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, okc (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, okc (lang_call fuel c e o n args why)) /\
  (forall c e ds why, okc (int_literal fuel c e ds why)) /\
  (forall c e v why, okc (float_of fuel c e v why)) /\
  (forall c e text, okc (synth_literal fuel c e text)) /\
  (forall c e sp y, okc (check_bind fuel c e sp y)) /\
  (forall c e y, okc (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, okc (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, okc (wrapper fuel pv)) /\
  (forall tg, okc (forwarder fuel tg)) /\
  (forall g, okc (lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, okc (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, okc (apply_functor fuel c e sp)) /\
  (forall c e g, okc (apply_body fuel c e g)) /\
  (forall c e fg, okc (apply_abstract fuel c e fg)) /\
  (forall c e fg y, okc (bind_functor fuel c e fg y)) /\
  (forall fg bg, okc (link_body fuel fg bg)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))); fintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; okqt. }
  destruct IH as (I0 & I1 & I2 & I3 & I4 & I5 & I6 & I7 & I8 & I9 & I10 & I11 & I12 & I13 & I14 & I15 & I16 & I17 & I18 & I19 & I20 & I21 & I22 & I23 & I24 & I25 & I26 & I27 & I28 & I29 & I30 & I31 & I32 & I33 & I34).
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))).
  10: exact (okc_synth_path_step fuel I8).
  9: exact (okc_const_val_step fuel I0).
  all: fintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; okqt.
Qed.

Lemma okc_synth : forall fuel c e x exp, okc (synth fuel c e x exp).
Proof. intros fuel. exact (proj1 (okc_synth_group fuel)). Qed.
#[local] Hint Resolve okc_synth : okqdb.

Lemma okc_synth_arg : forall fuel c e a exp, okc (synth_arg fuel c e a exp).
Proof. intros fuel. exact (proj1 (proj2 (okc_synth_group fuel))). Qed.
#[local] Hint Resolve okc_synth_arg : okqdb.

Lemma okc_synth_block : forall fuel c e b exp, okc (synth_block fuel c e b exp).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (okc_synth_group fuel)))). Qed.
#[local] Hint Resolve okc_synth_block : okqdb.

Lemma okc_check_stmts : forall fuel c e ss, okc (check_stmts fuel c e ss).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (okc_synth_group fuel))))). Qed.
#[local] Hint Resolve okc_check_stmts : okqdb.

Lemma okc_check_stmt : forall fuel c e s, okc (check_stmt fuel c e s).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))). Qed.
#[local] Hint Resolve okc_check_stmt : okqdb.

Lemma okc_check_bool : forall fuel c e y, okc (check_bool fuel c e y).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))). Qed.
#[local] Hint Resolve okc_check_bool : okqdb.

Lemma okc_synth_if : forall fuel c e x exp, okc (synth_if fuel c e x exp).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))). Qed.
#[local] Hint Resolve okc_synth_if : okqdb.

Lemma okc_synth_match : forall fuel c e s arms exp, okc (synth_match fuel c e s arms exp).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))). Qed.
#[local] Hint Resolve okc_synth_match : okqdb.

Lemma okc_const_val : forall fuel g, GoodG g -> okc (const_val fuel g).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))). Qed.

Lemma okc_synth_path : forall fuel c e p a, okc (synth_path fuel c e p a).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))). Qed.
#[local] Hint Resolve okc_synth_path : okqdb.

Lemma okc_check_args : forall fuel c e nm sg args, okc (check_args fuel c e nm sg args).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))). Qed.
#[local] Hint Resolve okc_check_args : okqdb.

Lemma okc_synth_call : forall fuel c e p a args, okc (synth_call fuel c e p a args).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))). Qed.
#[local] Hint Resolve okc_synth_call : okqdb.

Lemma okc_synth_record : forall fuel c e p a fs, okc (synth_record fuel c e p a fs).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))). Qed.
#[local] Hint Resolve okc_synth_record : okqdb.

Lemma okc_synth_field : forall fuel c e o n, okc (synth_field fuel c e o n).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))). Qed.
#[local] Hint Resolve okc_synth_field : okqdb.

Lemma okc_synth_method : forall fuel c e o p args, okc (synth_method fuel c e o p args).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))). Qed.
#[local] Hint Resolve okc_synth_method : okqdb.

Lemma okc_provided_call : forall fuel c e ox oty m args, okc (provided_call fuel c e ox oty m args).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))). Qed.
#[local] Hint Resolve okc_provided_call : okqdb.

Lemma okc_apply_family : forall fuel c e fg th want, okc (apply_family fuel c e fg th want).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))). Qed.
#[local] Hint Resolve okc_apply_family : okqdb.

Lemma okc_demand_needs : forall fuel c e info, okc (demand_needs fuel c e info).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))). Qed.
#[local] Hint Resolve okc_demand_needs : okqdb.

Lemma okc_synth_operator : forall fuel c e lm n op operand args, okc (synth_operator fuel c e lm n op operand args).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))). Qed.
#[local] Hint Resolve okc_synth_operator : okqdb.

Lemma okc_lang_call : forall fuel c e o n args why, okc (lang_call fuel c e o n args why).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_lang_call : okqdb.

Lemma okc_int_literal : forall fuel c e ds why, okc (int_literal fuel c e ds why).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_int_literal : okqdb.

Lemma okc_float_of : forall fuel c e v why, okc (float_of fuel c e v why).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_float_of : okqdb.

Lemma okc_synth_literal : forall fuel c e text, okc (synth_literal fuel c e text).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_synth_literal : okqdb.

Lemma okc_check_bind : forall fuel c e sp y, okc (check_bind fuel c e sp y).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_check_bind : okqdb.

Lemma okc_expect_fn_provider : forall fuel c e y, okc (expect_fn_provider fuel c e y).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_expect_fn_provider : okqdb.

Lemma okc_match_fn_sig : forall fuel c e pv wanted kind, okc (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_match_fn_sig : okqdb.

Lemma okc_wrapper : forall fuel pv, okc (wrapper fuel pv).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_wrapper : okqdb.

Lemma okc_forwarder : forall fuel tg, okc (forwarder fuel tg).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_forwarder : okqdb.

Lemma okc_lower_body : forall fuel g, okc (lower_body fuel g).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_lower_body : okqdb.

Lemma okc_lambda_provider : forall fuel c e ps rt body bound sg this what, okc (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_lambda_provider : okqdb.

Lemma okc_apply_functor : forall fuel c e sp, okc (apply_functor fuel c e sp).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_apply_functor : okqdb.

Lemma okc_apply_body : forall fuel c e g, okc (apply_body fuel c e g).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_apply_body : okqdb.

Lemma okc_apply_abstract : forall fuel c e fg, okc (apply_abstract fuel c e fg).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_apply_abstract : okqdb.

Lemma okc_bind_functor : forall fuel c e fg y, okc (bind_functor fuel c e fg y).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_bind_functor : okqdb.

Lemma okc_link_body : forall fuel fg bg, okc (link_body fuel fg bg).
Proof. intros fuel. exact (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_link_body : okqdb.

(* ======================================================================== *)
(* 4. A run                                                                  *)
(* ======================================================================== *)

Lemma okc_elab_program : okc elab_program.
Proof. unfold elab_program. okqt. Qed.

Lemma consts_init : ConstInv (s_init w).
Proof. intros k x t H. discriminate H. Qed.

(* an accepted bundle's run ends with a valid cache: BodySound's Later of
   its last state is Later0 of it *)
Theorem elab_consts : forall p s, elab_program (s_init w) = Good (p, s) -> Inv (s_init w) ->
  Later0 s -> ConstInv s.
Proof. intros p s H Hi L. exact (proj2 okc_elab_program _ _ _ Hi H L consts_init). Qed.

Corollary later_run : forall p s, elab_program (s_init w) = Good (p, s) -> Inv (s_init w) ->
  Later0 s -> Later w P R Fl s.
Proof. intros p s H Hi L. exact (later_of s L (elab_consts p s H Hi L)). Qed.

End Run.
