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
(*   Declaration type support follows from BodyTypeSupport.               *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import FinalLambdas Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv BodySound BodyConst InfoPreserve ElabConstInput ElabProgramInput FinalForwarders FwdReadonly FwdInvariant FwdPreserve FwdSynthInv FwdProgramInv ForwarderTrace.
Import ListNotations.
Local Open Scope list_scope.

Module A := Audit.Elaboration.

Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].

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

Lemma kc_reqs_frame : forall x, keeps st_consts (reqs x).
Proof. intro x. rewrite reqs_frame_eq. apply kc_type_requirements. Qed.
#[export] Hint Resolve kc_reqs_frame : keepsdb.
Local Strategy opaque [reqs type_requirements].

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

Lemma kc_check_literal_sig : forall e mi m oty args sg,
  keeps st_consts (check_literal_sig e mi m oty args sg).
Proof.
  intros. unfold check_literal_sig. destruct (literal_sig_ok e oty args sg); keepst.
Qed.
#[export] Hint Resolve kc_check_literal_sig : keepsdb.

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

Lemma kc_merged_walk_frame : forall mi walk,
 (forall e, keeps st_consts (walk e)) -> keeps st_consts (merged_walk mi walk).
Proof. intros mi walk H. unfold merged_walk. keepst. Qed.
#[export] Hint Resolve kc_merged_walk_frame : keepsdb.
Local Strategy opaque [merged_walk].
Lemma kc_fun_env_input:forall g,keeps st_consts(fun_env g).
Proof. intros;unfold fun_env;keepst. all:apply kc_merged_walk_frame;intros;keepst. Qed.
#[export]Hint Resolve kc_fun_env_input:keepsdb.
Local Strategy opaque [fun_env].
Lemma kc_fn_info_frame : forall g, keeps st_consts (fn_info g).
Proof. intro g. unfold fn_info. keepst. all: apply kc_merged_walk_frame; intros; keepst. Qed.
#[export] Hint Resolve kc_fn_info_frame : keepsdb.
Local Strategy opaque [fn_info].

Lemma kc_ety_ty_go : forall fuel t, keeps st_consts (ety_ty_go fuel t).
Proof. induction fuel; fintros; cbn [ety_ty_go]; keepst. Qed.
#[export] Hint Resolve kc_ety_ty_go : keepsdb.

Lemma kc_slots_go : forall fuel e t visiting, keeps st_consts (slots_go fuel e t visiting).
Proof. induction fuel; fintros; cbn [slots_go]; keepst. Qed.
#[export] Hint Resolve kc_slots_go : keepsdb.
Local Strategy opaque [slots_go].

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

Lemma kc_check_constant_effects:forall mi x,keeps st_consts(check_constant_effects mi x).
Proof. intros;unfold check_constant_effects;keepst. Qed.
Lemma kc_check_provider_effects:forall mi bs,keeps st_consts(check_provider_effects mi bs).
Proof. intros;unfold check_provider_effects;keepst. Qed.
#[local]Hint Resolve kc_check_constant_effects kc_check_provider_effects:keepsdb.

Section Run.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation trF := (tr Fl).
Variable b : bundle.
Hypothesis Hload : load_world b = Good w.

(* [BodySound.bodies_sound, given Pending] *)
Hypothesis HOK : forall f, OK w P R Fl f.

(* BodySound's Later without the cache *)
Record Later0 (s : st) : Prop := {
  l0_w : st_w s = w;
  l0_flds : forall a n, sassoc a (st_flds s) = Some n -> Fl a = n;
  l0_unsup : st_unsup s = [];
  l0_lang : st_lang s = lang_modules w;
  l0_reqs : forall x l, nget (st_reqs s) x = Some l -> R x = l;
  l0_reqclosed : A.RelevanceClosed (BodySound.W w R Fl) (A.w_reqs (BodySound.W w R Fl));
  l0_reqnone : forall x, A.kind_sym (BodySound.W w R Fl) x = Some A.DKType \/ A.kind_sym (BodySound.W w R Fl) x = Some A.DKUnit -> A.w_reqs (BodySound.W w R Fl) x = [];
  l0_bool_support : forall bi x, sassoc "bool"%string (lang_modules w) = Some bi -> sassoc "Bool"%string (gm_names (get_mod w bi)) = Some x -> NominalSupport.Support (BodySound.W w R Fl) (YNom x []);
  l0_forwarders : FinalForwarders w P s;
  l0_lambdas : FinalLambdas P s;
  l0_contexts : exists sc, check_contexts sc=Good(tt,sc) /\ st_w sc=w
}.

Lemma later0_back : forall s s', Fr s s' -> Later0 s' -> Later0 s.
Proof.
  intros s s' Fs [Lw Lf Lu Ll Lr Lclosed Lnone Lbool Lforwarders Llambdas Lcontexts].
  pose proof(final_lambdas_back P s s' Fs Llambdas)as LL.
  pose proof(final_forwarders_back w P s s' Fs Lforwarders)as LF. destruct Fs as [[Fw _ _ _ _ Ffl [_ _ _ _ (u & Eu) Elang _]] _ Fx]. constructor.
  - congruence.
  - intros a n H. exact (Lf a n (Ffl a n H)).
  - rewrite Lu in Eu. destruct (st_unsup s); [reflexivity | discriminate].
  - congruence.
  - intros x l H. exact (Lr x l (Fx x l H)).
  - exact Lclosed.
  - exact Lnone.
  - exact Lbool.
  - exact LF.
  - exact LL.
  - exact Lcontexts.
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

Lemma kc_compile_pattern : forall mi e p sty heads, keeps st_consts (compile_pattern mi e p sty heads).
Proof. intros;unfold compile_pattern;keepst. Qed.
Lemma ok_compile_pattern_frame:forall mi e p sty heads,ok(compile_pattern mi e p sty heads).
Proof. intros;exact(proj1(info_compile_pattern mi e p sty heads)). Qed.
#[local]Hint Resolve kc_compile_pattern:keepsdb.
#[local]Hint Resolve ok_compile_pattern_frame:okdb.
Local Strategy opaque [compile_pattern].
Lemma kc_payload_type_input:forall t e,keeps st_consts(payload_type t e).
Proof. intros;unfold payload_type;keepst. Qed.
#[local]Hint Resolve kc_payload_type_input:keepsdb.
Lemma kc_check_relevant_input:forall mi g a,keeps st_consts(check_relevant mi g a).
Proof. intros;unfold check_relevant;keepst. Qed.
#[local]Hint Resolve kc_check_relevant_input:keepsdb.
Lemma kc_fn_sig_input:forall g tm e this,keeps st_consts(fn_sig g tm e this).
Proof. intros;unfold fn_sig;keepst. Qed.
#[local]Hint Resolve kc_fn_sig_input:keepsdb.
Lemma kc_ety_of_input:forall t mi tm e this,keeps st_consts(ety_of t mi tm e this).
Proof. intros;unfold ety_of;keepst. Qed.
#[local]Hint Resolve kc_ety_of_input:keepsdb.
Local Strategy opaque [payload_type check_relevant fn_sig].
Lemma kc_check_captured_reading : forall mi site prototype formal actual,
 keeps st_consts(check_captured_reading mi site prototype formal actual).
Proof. intros;unfold check_captured_reading;keepst. Qed.
Lemma kc_check_link_fixed_current : forall mi site fg bg fixed,
 keeps st_consts(check_link_fixed_current mi site fg bg fixed).
Proof. intros;unfold check_link_fixed_current;keepst. Qed.
Lemma kc_fresh_marker : forall e,keeps st_consts(fresh_marker e).
Proof. intros;unfold fresh_marker;keepst. Qed.
Lemma kc_fresh_marker_for : forall e r,keeps st_consts(fresh_marker_for e r).
Proof. intros;unfold fresh_marker_for;keepst. Qed.
#[local] Hint Resolve kc_check_captured_reading kc_check_link_fixed_current kc_fresh_marker kc_fresh_marker_for : keepsdb.

Lemma cache_fwd_ety_of:forall t mi tm e this,okF(ety_of t mi tm e this).
Proof. intros;apply okF_readonly;[apply ok_ety_of|apply kw_ety_of_input]. Qed.
#[local]Hint Resolve cache_fwd_ety_of:inputfwdb.
Notation okc := (okqi ConstCache).
Lemma okc_ety_of:forall t mi tm e this,okc(ety_of t mi tm e this).
Proof. intros;apply okqi_keeps;[apply ok_ety_of|apply kc_ety_of_input]. Qed.
#[local]Hint Resolve okc_ety_of:okqidb.

(* A symbol const_val is given: the loader's at its number, if any (when
   none, ConstFact asks nothing of the entry). *)
Definition GoodG (g : gsym) : Prop := forall g', nget (w_syms w) (gs_id g) = Some g' -> g' = g.

(* a symbol read from the state is one *)
Lemma okc_bind_sym : forall x B (K : gsym -> M B), (forall g, ok (K g)) ->
  (forall g, GoodG g -> gs_id g = x -> okc (K g)) -> okc (mbind (sym x) K).
Proof.
  intros x B K Ok Hk. split; [apply ok_bind; [unfold sym; okt | exact Ok]|].
  intros s a s' Hi Ii Fi H L Q. bi H as g s1 H1. unfold sym in H1.
  destruct (nget (st_syms s) x) as [g0|] eqn:Ex; [|discriminate]. injection H1 as -> <-.
  destruct (Ok g s a s' Hi H) as [_ F]. pose proof (later0_back _ _ F L) as L1.
  assert (Eid : gs_id g = x) by exact (iv_ids _ Hi x g Ex).
  assert (G : GoodG g).
  { intros g' Hg'. rewrite Eid in Hg'.
    assert (Hb : load_world b = Good (st_w s)) by (rewrite (l0_w _ L1); exact Hload).
    pose proof (iv_loader _ Hi b Hb x g' ltac:(rewrite (l0_w _ L1); exact Hg')) as E. congruence. }
  exact (proj2 (Hk g G Eid) s a s' Hi Ii Fi H L Q).
Qed.

Ltac sym_ext := idtac; lazymatch goal with
  | |- okqi _ (mbind (sym _) _) => apply okc_bind_sym; [intro; solve [okt] | ]
  end.

(* ======================================================================== *)
(* 3. Every function of the elaborator keeps the cache valid                 *)
(* ======================================================================== *)

Lemma kc_replay_reify_constant:forall id,keeps st_consts(replay_reify_constant id).
Proof. intros;unfold replay_reify_constant;keepst. Qed.
Lemma kc_remember_reify_constant:forall id start,keeps st_consts(remember_reify_constant id start).
Proof. intros;unfold remember_reify_constant;keepst. Qed.
#[local]Hint Resolve kc_replay_reify_constant kc_remember_reify_constant:keepsdb.

(* the write *)
Lemma okc_const_val_step : forall f, (forall c e x exp, okc (synth f c e x exp)) ->
  forall g, GoodG g -> okc (const_val (S f) g).
Proof.
  intros f IH g G. split; [apply ok_const_val|].
  intros s a s' Hi Ii Fi H L Q. cbn [const_val] in H.
  bi H as s0 s1 Hget. unfold get in Hget. injection Hget as <- <-.
  destruct (nget (st_consts s) (gs_id g)) as [r|] eqn:Ec.
  { bi H as replayed sr Hreplay. injection H as _ <-.
    change(ConstInv sr);intros k x t Lookup;apply Q.
    rewrite <-(kc_replay_reify_constant _ _ _ _ Hreplay);exact Lookup. }
  destruct (gs_assumes g) as [|a0 r0] eqn:Eas; [|nogood H].
  destruct (gs_decl g) as [| | | | |n t0 [i|]| | |] eqn:Ed; try nogood H.
  bi H as declared s2 Hdecl.
  bi H as r3 s3 Hwv. destruct r3 as [v0 vt].
  destruct (fits empty_env vt declared) eqn:Efit; [|nogood H].
  bi H as remembered sr Hremember.
  bi H as u s4 Hupd. injection H as _ <-.
  (* the frames *)
  destruct (ok_ety_of t0 (gs_mod g) [] empty_env None s declared s2 Hi Hdecl) as [Hi2 Fs2].
  destruct (ok_with_vars _ _ _ (ok_synth _ _ _ _ _) _ _ _ Hi2 Hwv) as [Hi3 Fs3].
  destruct (ok_remember_reify_constant (gs_id g) _ _ _ _ Hi3 Hremember) as [Hir Fremember].
  destruct (ok_consts_add (gs_id g) (v0, declared) _ _ _ Hir Hupd) as [_ Fs4].
  pose proof (later0_back _ _ Fs4 L) as Lremember.
  pose proof (later0_back _ _ Fremember Lremember) as L3.
  assert(Econst:st_consts sr=st_consts s3)by(eapply kc_remember_reify_constant;exact Hremember).
  (* the cache, through the declared type and the initializer *)
  assert (Oe : okc (ety_of t0 (gs_mod g) [] empty_env None)) by (apply okqi_keeps; [apply ok_ety_of | apply kc_ety_of_input]).
  pose proof (proj2 Oe s declared s2 Hi Ii Fi Hdecl (later0_back _ _ Fs3 L3) Q) as Q2.
  assert (OIe : okI (ety_of t0 (gs_mod g) [] empty_env None)) by (apply okI_annotation_type).
  pose proof (proj2 OIe _ _ _ Hi Ii Hdecl) as Ii2.
  assert(OFdecl:okF(ety_of t0(gs_mod g)[]empty_env None))by input_fwd.
  pose proof(proj2 OFdecl _ _ _ Hi Fi Hdecl)as Fi2.
  pose proof (proj2 (okqi_with_vars ConstCache _ [] _ (IH _ _ _ _)) s2 (v0, vt) s3 Hi2 Ii2 Fi2 Hwv L3 Q2) as Q3.
  change (ConstInv s3) in Q3.
  unfold upd in Hupd. injection Hupd as _ <-.
  unfold ConstCache; cbn [ca_Q]. intros k x t Hk. unfold s_consts in Hk. cbn [st_consts] in Hk. rewrite Econst in Hk.
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
  exact (const_fresh w P R Fl b Hload f (HOK f) g n t0 i s declared s2 v0 vt s8 Ew Eas Ed Hi Ii Fi Hdecl Hsyn Efit
           (later_of _ L8 Q8)).
Qed.

Local Strategy opaque [ety_of].

(* the one read of a symbol for const_val *)
Lemma okc_synth_path_step : forall f, (forall g, GoodG g -> okc (const_val f g)) ->
  forall c e p a, okc (synth_path (S f) c e p a).
Proof. intros f IH c e p a. cbn [synth_path]. okqit_with sym_ext. Qed.

Lemma okc_forwarder_step:forall f,(forall g,okc(lower_body f g))->forall tg,okc(forwarder(S f)tg).
Proof.
 intros f Lower tg;split;[apply ok_forwarder|].
 intros s got final Hi Ii Fi Run L Q.
 cbn[forwarder]in Run;apply bind_inv in Run as(initial&s0&Read&Run);injection Read as <- <-.
 destruct(nget(st_fwd s)(gs_id tg))as[id|]eqn:Old.
 - unfold sym in Run;destruct(nget(st_syms s)id);[injection Run as <- <-;exact Q|discriminate Run].
 - apply bind_inv in Run as(id&allocated&New&Run).
   apply bind_inv in Run as([]&cached&Cache&Run).
   apply bind_inv in Run as(provider&ready&Sym&Run).
   apply bind_inv in Run as([]&last&Body&Ret);injection Ret as <- <-.
   assert(On:ok(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by okt.
   destruct(On _ _ _ Hi New)as[Ia Fa].
   assert(NInew:NI(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by(unfold new_sym;nit).
   pose proof(proj2(okI_ni _ _ On NInew ltac:(keepst))_ _ _ Hi Ii New)as IaInfo.
   assert(KeepFwd:keeps st_fwd(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by(unfold new_sym;keepst).
   assert(IaFwd:FwdInv allocated)by(eapply fwd_inv_frame;[exact Fi|exact Fa|exact(KeepFwd _ _ _ New)]).
   destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id[Next Lookup]].
   assert(Oc:ok(upd(s_fwd(fun m=>nset m(gs_id tg)id))))by okt.
   destruct(Oc _ _ _ Ia Cache)as[Ic Fc].
   assert(NIc:NI(upd(s_fwd(fun m=>nset m(gs_id tg)id))))by(nistate).
   pose proof(proj2(okI_ni _ _ Oc NIc ltac:(keepst))_ _ _ Ia IaInfo Cache)as IcInfo.
   assert(Cached:cached=s_fwd(fun m=>nset m(gs_id tg)id)allocated).
   {unfold upd in Cache;injection Cache;auto. }
   assert(IcFwd:FwdInv cached).
   {rewrite Cached;eapply fwd_inv_install;[exact IaFwd|exact Lookup|reflexivity]. }
   assert(Os:ok(sym id))by okt;destruct(Os _ _ _ Ic Sym)as[Ir Fr].
   assert(NIsym:NI(sym id))by(unfold sym;nit).
   pose proof(proj2(okI_ni _ _ Os NIsym ltac:(keepst))_ _ _ Ic IcInfo Sym)as IrInfo.
   assert(Same:ready=cached).
   {unfold sym in Sym;destruct(nget(st_syms cached)id);[injection Sym;auto|discriminate Sym]. }
   assert(IrFwd:FwdInv ready)by(rewrite Same;exact IcFwd).
   assert(Knew:keeps st_consts(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by(unfold new_sym;keepst).
   assert(Qr:ConstInv ready).
   {intros k x ty Have;apply Q;rewrite Same,Cached in Have;cbn[s_fwd st_consts]in Have.
    rewrite(Knew _ _ _ New)in Have;exact Have. }
   exact(proj2(Lower provider)_ _ _ Ir IrInfo IrFwd Body L Qr).
Qed.

Lemma cache_fwd_family_member:forall mi e r mid,okF(family_member mi e r mid).
Proof. intros;apply okF_readonly;[unfold family_member;okt|unfold family_member;keepst]. Qed.
Lemma cache_fwd_require_needs:forall mi e g i,okF(require_needs mi e g i).
Proof. intros;apply okF_readonly;[unfold require_needs;okt|unfold require_needs;keepst]. Qed.
#[local]Hint Resolve cache_fwd_family_member cache_fwd_require_needs:inputfwdb.

(* Walk each monadic subtree once while establishing the three invariants
   needed by cache-preserving bind.  Proving them independently at every bind
   repeatedly traverses overlapping prefixes. *)
Local Definition cache_step (C : Cache) {A} (m : M A) := okI m /\ okF m /\ okqi C m.
Arguments cache_step : simpl never.
Local Lemma cache_step_intro : forall C A (m : M A),
  okqi C m -> okI m -> okF m -> cache_step C m.
Proof. intros C A m Q I F; exact (conj I (conj F Q)). Qed.
Local Lemma cache_step_cache : forall C A (m : M A), cache_step C m -> okqi C m.
Proof. intros C A m [_ [_ Q]]; exact Q. Qed.
Local Lemma cache_step_bind : forall C A B (m : M A) (k : A -> M B),
  cache_step C m -> (forall a, cache_step C (k a)) -> cache_step C (mbind m k).
Proof.
  intros C A B m k [Im [Fm Qm]] K. split.
  - apply okI_bind; [exact Im | intro a; exact (proj1 (K a))].
  - split.
    + apply okF_bind; [exact Fm | intro a; exact (proj1 (proj2 (K a)))].
    + apply okqi_bind; [exact Im | exact Fm | exact Qm | intro a; exact (proj2 (proj2 (K a)))].
Qed.
Local Lemma cache_step_ret : forall C A (a : A), cache_step C (ret a).
Proof. intros; split; [apply okI_ret | split; [apply okF_ret | apply okqi_ret]]. Qed.
Local Lemma cache_step_mmap : forall C A B (f : A -> M B) xs,
  (forall x, cache_step C (f x)) -> cache_step C (mmap f xs).
Proof.
  intros C A B f xs F. split.
  - apply okI_mmap; intro x; exact (proj1 (F x)).
  - split.
    + apply okF_mmap; intro x; exact (proj1 (proj2 (F x))).
    + apply okqi_mmap; intro x; [exact (proj1 (F x)) | exact (proj1 (proj2 (F x))) | exact (proj2 (proj2 (F x)))].
Qed.
Local Lemma cache_step_mfold : forall C A B (f : A -> B -> M A) xs acc,
  (forall a x, cache_step C (f a x)) -> cache_step C (mfold f acc xs).
Proof.
  intros C A B f xs acc F. split.
  - apply okI_mfold; intros a x; exact (proj1 (F a x)).
  - split.
    + apply okF_mfold; intros a x; exact (proj1 (proj2 (F a x))).
    + apply okqi_mfold; intros a x; [exact (proj1 (F a x)) | exact (proj1 (proj2 (F a x))) | exact (proj2 (proj2 (F a x)))].
Qed.
Local Lemma cache_step_miter : forall C A (f : A -> M unit) xs,
  (forall x, cache_step C (f x)) -> cache_step C (miter f xs).
Proof.
  intros C A f xs F. split.
  - apply okI_miter; intro x; exact (proj1 (F x)).
  - split.
    + apply okF_miter; intro x; exact (proj1 (proj2 (F x))).
    + apply okqi_miter; intro x; [exact (proj1 (F x)) | exact (proj1 (proj2 (F x))) | exact (proj2 (proj2 (F x)))].
Qed.
Local Lemma cache_step_with_vars : forall C A initial (m : M A),
  cache_step C m -> cache_step C (with_vars initial m).
Proof.
  intros C A initial m [I [F Q]]. split.
  - apply okI_with_vars; exact I.
  - split; [apply okF_with_vars; exact F | apply okqi_with_vars; exact Q].
Qed.
Local Ltac cache_step_tac := repeat (fintros; lazymatch goal with
    | |- cache_step _ (ret _) => apply cache_step_ret
    (* Allocation and initialization must remain one atomic proof step. *)
    | |- cache_step _ (mbind (new_sym _ _ _ _ _) _) =>
        apply cache_step_intro; [solve [okqit] | solve [input_info] | solve [input_fwd]]
    | |- cache_step _ (mbind _ _) => apply cache_step_bind
    | |- cache_step _ (mmap _ _) => apply cache_step_mmap
    | |- cache_step _ (mfold _ _ _) => apply cache_step_mfold
    | |- cache_step _ (miter _ _) => apply cache_step_miter
    | |- cache_step _ (with_vars _ _) => apply cache_step_with_vars
    | |- cache_step _ (upd _) =>
        apply cache_step_intro; [solve [okqi_keeps_leaf] | solve [input_info] | solve [input_fwd]]
    | |- cache_step _ (fun s => _) =>
        apply cache_step_intro; [solve [okqi_keeps_leaf] | solve [input_info] | solve [input_fwd]]
    | |- cache_step _ (match ?x with _ => _ end) => destruct x
    | |- cache_step _ ?m => first [
        solve [apply cache_step_intro; [solve [auto with okqidb] | input_info | input_fwd]]
        | solve [apply cache_step_intro; [okqi_keeps_lemma | input_info | input_fwd]]
        | match m with context[match ?x with _ => _ end] => destruct x end
        | let h := head_of m in progress unfold h]
    end).

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
  { refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))); fintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; okqit. }
  destruct IH as (I0 & I1 & I2 & I3 & I4 & I5 & I6 & I7 & I8 & I9 & I10 & I11 & I12 & I13 & I14 & I15 & I16 & I17 & I18 & I19 & I20 & I21 & I22 & I23 & I24 & I25 & I26 & I27 & I28 & I29 & I30 & I31 & I32 & I33 & I34).
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))).
  10: exact (okc_synth_path_step fuel I8).
  9: exact (okc_const_val_step fuel I0).
  26: exact (okc_forwarder_step fuel I28).
  (* Some operations, such as wrapper, need only the weaker cache property. *)
  all: first [solve [fintros; apply cache_step_cache; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; cache_step_tac] | solve [fintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; okqit]].
Qed.

Lemma okc_synth : forall fuel c e x exp, okc (synth fuel c e x exp).
Proof. intros fuel. exact (proj1 (okc_synth_group fuel)). Qed.
#[local] Hint Resolve okc_synth : okqidb.

Lemma okc_synth_arg : forall fuel c e a exp, okc (synth_arg fuel c e a exp).
Proof. intros fuel. exact (proj1 (proj2 (okc_synth_group fuel))). Qed.
#[local] Hint Resolve okc_synth_arg : okqidb.

Lemma okc_synth_block : forall fuel c e b exp, okc (synth_block fuel c e b exp).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (okc_synth_group fuel)))). Qed.
#[local] Hint Resolve okc_synth_block : okqidb.

Lemma okc_check_stmts : forall fuel c e ss, okc (check_stmts fuel c e ss).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (okc_synth_group fuel))))). Qed.
#[local] Hint Resolve okc_check_stmts : okqidb.

Lemma okc_check_stmt : forall fuel c e s, okc (check_stmt fuel c e s).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))). Qed.
#[local] Hint Resolve okc_check_stmt : okqidb.

Lemma okc_check_bool : forall fuel c e y, okc (check_bool fuel c e y).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))). Qed.
#[local] Hint Resolve okc_check_bool : okqidb.

Lemma okc_synth_if : forall fuel c e x exp, okc (synth_if fuel c e x exp).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))). Qed.
#[local] Hint Resolve okc_synth_if : okqidb.

Lemma okc_synth_match : forall fuel c e s arms exp, okc (synth_match fuel c e s arms exp).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))). Qed.
#[local] Hint Resolve okc_synth_match : okqidb.

Lemma okc_const_val : forall fuel g, GoodG g -> okc (const_val fuel g).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))). Qed.

Lemma okc_synth_path : forall fuel c e p a, okc (synth_path fuel c e p a).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))). Qed.
#[local] Hint Resolve okc_synth_path : okqidb.

Lemma okc_check_args : forall fuel c e nm sg args, okc (check_args fuel c e nm sg args).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))). Qed.
#[local] Hint Resolve okc_check_args : okqidb.

Lemma okc_synth_call : forall fuel c e p a args, okc (synth_call fuel c e p a args).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))). Qed.
#[local] Hint Resolve okc_synth_call : okqidb.

Lemma okc_synth_record : forall fuel c e p a fs, okc (synth_record fuel c e p a fs).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))). Qed.
#[local] Hint Resolve okc_synth_record : okqidb.

Lemma okc_synth_field : forall fuel c e o n, okc (synth_field fuel c e o n).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))). Qed.
#[local] Hint Resolve okc_synth_field : okqidb.

Lemma okc_synth_method : forall fuel c e o p args, okc (synth_method fuel c e o p args).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))). Qed.
#[local] Hint Resolve okc_synth_method : okqidb.

Lemma okc_provided_call : forall fuel c e ox oty m args, okc (provided_call fuel c e ox oty m args).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))). Qed.
#[local] Hint Resolve okc_provided_call : okqidb.

Lemma okc_apply_family : forall fuel c e fg th want, okc (apply_family fuel c e fg th want).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))). Qed.
#[local] Hint Resolve okc_apply_family : okqidb.

Lemma okc_demand_needs : forall fuel c e info, okc (demand_needs fuel c e info).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))). Qed.
#[local] Hint Resolve okc_demand_needs : okqidb.

Lemma okc_synth_operator : forall fuel c e lm n op operand args, okc (synth_operator fuel c e lm n op operand args).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))). Qed.
#[local] Hint Resolve okc_synth_operator : okqidb.

Lemma okc_lang_call : forall fuel c e o n args why, okc (lang_call fuel c e o n args why).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_lang_call : okqidb.

Lemma okc_int_literal : forall fuel c e ds why, okc (int_literal fuel c e ds why).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_int_literal : okqidb.

Lemma okc_float_of : forall fuel c e v why, okc (float_of fuel c e v why).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_float_of : okqidb.

Lemma okc_synth_literal : forall fuel c e text, okc (synth_literal fuel c e text).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_synth_literal : okqidb.

Lemma okc_check_bind : forall fuel c e sp y, okc (check_bind fuel c e sp y).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_check_bind : okqidb.

Lemma okc_expect_fn_provider : forall fuel c e y, okc (expect_fn_provider fuel c e y).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_expect_fn_provider : okqidb.

Lemma okc_match_fn_sig : forall fuel c e pv wanted kind, okc (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_match_fn_sig : okqidb.

Lemma okc_wrapper : forall fuel pv, okc (wrapper fuel pv).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_wrapper : okqidb.

Lemma okc_forwarder : forall fuel tg, okc (forwarder fuel tg).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_forwarder : okqidb.

Lemma okc_lower_body : forall fuel g, okc (lower_body fuel g).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_lower_body : okqidb.

Lemma okc_lambda_provider : forall fuel c e ps rt body bound sg this what, okc (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_lambda_provider : okqidb.

Lemma okc_apply_functor : forall fuel c e sp, okc (apply_functor fuel c e sp).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_apply_functor : okqidb.

Lemma okc_apply_body : forall fuel c e g, okc (apply_body fuel c e g).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_apply_body : okqidb.

Lemma okc_apply_abstract : forall fuel c e fg, okc (apply_abstract fuel c e fg).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel)))))))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_apply_abstract : okqidb.

Lemma okc_bind_functor : forall fuel c e fg y, okc (bind_functor fuel c e fg y).
Proof. intros fuel. exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_bind_functor : okqidb.

Lemma okc_link_body : forall fuel fg bg, okc (link_body fuel fg bg).
Proof. intros fuel. exact (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okc_synth_group fuel))))))))))))))))))))))))))))))))))). Qed.
#[local] Hint Resolve okc_link_body : okqidb.

(* ======================================================================== *)
(* 4. A run                                                                  *)
(* ======================================================================== *)

Local Strategy opaque [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body].

Lemma okc_check_listed_phase:okc check_listed.
Proof. apply okqi_keeps;[exact(proj1 program_info_listed)|unfold check_listed;keepst]. Qed.
#[local]Hint Resolve okc_check_listed_phase:okqidb.
Lemma okc_check_contexts_phase:okc check_contexts.
Proof. apply okqi_keeps;[exact(proj1 program_info_contexts)|unfold check_contexts;keepst]. Qed.
#[local]Hint Resolve okc_check_contexts_phase:okqidb.
Lemma okc_check_runtime_library_phase:okc check_runtime_library.
Proof. apply okqi_keeps;[exact(proj1 program_info_runtime)|unfold check_runtime_library;keepst]. Qed.
#[local]Hint Resolve okc_check_runtime_library_phase:okqidb.
Lemma okc_check_assumes_phase:okc check_assumes.
Proof. apply okqi_keeps;[exact(proj1 program_info_assumes)|unfold check_assumes;keepst]. Qed.
#[local]Hint Resolve okc_check_assumes_phase:okqidb.
Lemma okc_platform_phase:forall k p,okc(platform_entry k p).
Proof. intros;apply okqi_keeps;[exact(proj1(program_info_platform k p))|unfold platform_entry;keepst]. Qed.
#[local]Hint Resolve okc_platform_phase:okqidb.
Lemma cache_fwd_listed:okF check_listed.
Proof. apply okF_readonly;[exact(proj1 program_info_listed)|unfold check_listed;keepst]. Qed.
Lemma cache_fwd_contexts:okF check_contexts.
Proof. apply okF_readonly;[exact(proj1 program_info_contexts)|unfold check_contexts;keepst]. Qed.
Lemma cache_fwd_runtime:okF check_runtime_library.
Proof. apply okF_readonly;[exact(proj1 program_info_runtime)|unfold check_runtime_library;keepst]. Qed.
Lemma cache_fwd_assumes:okF check_assumes.
Proof. apply okF_readonly;[exact(proj1 program_info_assumes)|unfold check_assumes;keepst]. Qed.
#[local]Hint Resolve cache_fwd_listed cache_fwd_contexts cache_fwd_runtime cache_fwd_assumes:inputfwdb.
Lemma okc_lower_phase:okc lower_all.
Proof. unfold lower_all;program_qit. Qed.
#[local]Hint Resolve okc_lower_phase:okqidb.
Local Strategy opaque [check_listed check_contexts check_runtime_library check_assumes platform_entry lower_all].
(* Do not unfold the literal entry-width fuel in whole-program cache
   automation: prove preservation once for arbitrary fuel instead. *)
Lemma kc_entry_zero_width : forall fuel e t,keeps st_consts(entry_zero_width fuel e t).
Proof. induction fuel;intros e t;cbn[entry_zero_width];keepst. Qed.
#[local]Hint Resolve kc_entry_zero_width:keepsdb.
Lemma kc_check_main_arity : forall g,keeps st_consts(check_main_arity g).
Proof. intros;unfold check_main_arity;keepst. Qed.
#[local]Hint Resolve kc_check_main_arity:keepsdb.
Lemma kc_check_main_signature : forall g info,keeps st_consts(check_main_signature g info).
Proof. intros;unfold check_main_signature;keepst. Qed.
#[local]Hint Resolve kc_check_main_signature:keepsdb.
Lemma okc_runtime_inputs_phase:forall h,okc(runtime_provider_inputs h).
Proof. intros;apply okqi_keeps;[exact(proj1(program_info_runtime_inputs h))|unfold runtime_provider_inputs;keepst]. Qed.
#[local]Hint Resolve okc_runtime_inputs_phase:okqidb.
Local Strategy opaque [runtime_provider_inputs].
Lemma cache_fwd_platform:forall k p,okF(platform_entry k p).
Proof. intros;apply okF_readonly;[exact(proj1(program_info_platform k p))|apply kw_platform_entry]. Qed.
Lemma cache_fwd_runtime_inputs:forall h,okF(runtime_provider_inputs h).
Proof. intros;apply okF_readonly;[exact(proj1(program_info_runtime_inputs h))|apply kw_runtime_provider_inputs]. Qed.
Lemma cache_fwd_platform_view:forall e k entry,okF(check_platform_entry_view e k entry).
Proof. intros;apply okF_readonly;[apply ok_check_platform_entry_view|apply kw_check_platform_entry_view]. Qed.
Lemma cache_fwd_ety_ty:forall t,okF(ety_ty t).
Proof. intros;apply okF_readonly;[unfold ety_ty;apply ok_ety_ty_go|unfold ety_ty;apply kw_ety_ty_go]. Qed.
#[local]Hint Resolve cache_fwd_platform cache_fwd_runtime_inputs cache_fwd_platform_view cache_fwd_ety_ty:inputfwdb.
Lemma cache_fwd_main_signature:forall g info,okF(check_main_signature g info).
Proof. intros;apply okF_readonly;[apply ok_check_main_signature|apply kw_check_main_signature_present]. Qed.
#[local]Hint Resolve cache_fwd_main_signature:inputfwdb.
Lemma okc_elab_program : okc elab_program.
Proof. unfold elab_program. program_qit. Qed.

Lemma consts_init : ConstInv (s_init w).
Proof. intros k x t H. discriminate H. Qed.

(* an accepted bundle's run ends with a valid cache: BodySound's Later of
   its last state is Later0 of it *)
Theorem elab_consts : forall p s, elab_program (s_init w) = Good (p, s) -> Inv (s_init w) ->
  Later0 s -> ConstInv s.
Proof. intros p s H Hi L. exact (proj2 okc_elab_program _ _ _ Hi (info_init w) (@fwd_inv_init w) H L consts_init). Qed.

Corollary later_run : forall p s, elab_program (s_init w) = Good (p, s) -> Inv (s_init w) ->
  Later0 s -> Later w P R Fl s.
Proof. intros p s H Hi L. exact (later_of s L (elab_consts p s H Hi L)). Qed.

End Run.
