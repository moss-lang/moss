(* Actual cache preservation for source execution, independent of body semantics. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsStable ElabFrame ReqsTotal.
From NM Require Export InfoInvariant.
From NM Require Import ElabInv ProviderPhysical FnInfoResult ForwarderTrace ReifyData.
From NM Require Export ElabInv WrapperReadonly.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque[Nat.of_num_uint FUEL].
Definition NI {A} (m : M A) : Prop := forall s a s', m s = Good (a, s') -> st_info s' = st_info s.
Arguments NI : simpl never.

Lemma ni_ret : forall A (a : A), NI (ret a).
Proof. intros A a s x s' H. injection H as _ <-. reflexivity. Qed.

Lemma ni_bind : forall A B (m : M A) (k : A -> M B), NI m -> (forall a, NI (k a)) -> NI (mbind m k).
Proof.
  intros A B m k Hm Hk s b s'' H. apply bind_inv in H as (a & s' & H1 & H2).
  rewrite (Hk a _ _ _ H2). exact (Hm _ _ _ H1).
Qed.

Lemma ni_fail : forall A msg, @NI A (fail msg).
Proof. intros A msg s a s' H. discriminate H. Qed.

Lemma ni_mmap : forall A B (f : A -> M B) l, (forall x, NI (f x)) -> NI (mmap f l).
Proof.
  intros A B f l Hf. induction l as [|x r IH]; cbn [mmap]; [apply ni_ret|].
  apply ni_bind; [apply Hf | intro y; apply ni_bind; [exact IH | intro; apply ni_ret]].
Qed.

Lemma ni_mfold : forall A B (f : A -> B -> M A) l a, (forall a x, NI (f a x)) -> NI (mfold f a l).
Proof.
  intros A B f l. induction l as [|x r IH]; intros a Hf; cbn [mfold]; [apply ni_ret|].
  apply ni_bind; [apply Hf | intro a'; apply IH; exact Hf].
Qed.

Lemma ni_miter : forall A (f : A -> M unit) l, (forall x, NI (f x)) -> NI (miter f l).
Proof.
  intros A f l Hf. induction l as [|x r IH]; cbn [miter]; [apply ni_ret|].
  apply ni_bind; [apply Hf | intro; exact IH].
Qed.

Create HintDb nidb.
#[export] Hint Resolve ni_ret ni_fail : nidb.

(* a state function: its cases each give back a state with the same cache *)
Ltac nistate :=
  let s := fresh "s" in let a := fresh "a" in let s' := fresh "s'" in let H := fresh "H" in
  intros s a s' H;
  repeat (match type of H with context [match ?x with _ => _ end] => destruct x end);
  try discriminate H; injection H; intros; subst; reflexivity.

Ltac nifixind1 :=
  lazymatch goal with |- NI (?F ?N) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N as n; intro n; induction n as [|n IHn]; cbn beta iota end.
Ltac nifixind3 :=
  lazymatch goal with |- NI (?F ?N ?a ?b) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N, a, b; intro n; induction n as [|n IHn]; intros; cbn beta iota end.

Ltac nit :=
  repeat (fintros; lazymatch goal with
  | |- NI (mbind _ _) => apply ni_bind
  | |- NI (mmap _ _) => apply ni_mmap
  | |- NI (mfold _ _ _) => apply ni_mfold
  | |- NI (miter _ _) => apply ni_miter
  | |- NI (upd _) => unfold upd; nistate
  | |- NI (fun s => _) => nistate
  | |- NI ?t =>
      first [ solve [auto with nidb]
            | (lazymatch t with ?F _ => is_fix F end; nifixind1)
            | (lazymatch t with ?F _ _ _ => is_fix F end; nifixind3)
            | match t with context [match ?x with _ => _ end] => destruct x end
            | let h := head_of t in progress unfold h ]
  end).

Lemma ni_fresh_marker:forall e,NI(fresh_marker e).
Proof. intros;unfold fresh_marker;nistate. Qed.
Lemma ni_fresh_marker_for:forall e r,NI(fresh_marker_for e r).
Proof. intros;unfold fresh_marker_for;nistate. Qed.
#[export]Hint Resolve ni_fresh_marker ni_fresh_marker_for:nidb.

(* every function of Elab.v before fn_info, and those after it that never
   reach it, leave the cache alone *)

Lemma ni_mentioned_nominals_group : forall fuel,
  (forall t mi, NI (mentioned_nominals fuel t mi)).
Proof.
  induction fuel as [|fuel IH].
  { fintros; cbn [mentioned_nominals]; nit. }
  pose proof IH as I0. fintros; cbn [mentioned_nominals]; nit.
Qed.

Lemma ni_mentioned_nominals : forall fuel t mi, NI (mentioned_nominals fuel t mi).
Proof. intros fuel t mi. exact ((ni_mentioned_nominals_group fuel) t mi). Qed.
#[export] Hint Resolve ni_mentioned_nominals : nidb.

Lemma ni_walk_specs_group : forall fuel,
  (forall mi sp out, NI (walk_specs fuel mi sp out)).
Proof.
  induction fuel as [|fuel IH].
  { fintros; cbn [walk_specs]; nit. }
  pose proof IH as I0. fintros; cbn [walk_specs]; nit.
Qed.

Lemma ni_walk_specs : forall fuel mi sp out, NI (walk_specs fuel mi sp out).
Proof. intros fuel mi sp out. exact ((ni_walk_specs_group fuel) mi sp out). Qed.
#[export] Hint Resolve ni_walk_specs : nidb.

Lemma ni_free_type_syms_group : forall fuel,
  (forall t mi out, NI (free_type_syms fuel t mi out)) /\
  (forall s, NI (type_requirements fuel s)) /\
  (forall root, NI (solve_requirements fuel root)) /\
  (forall g, NI (listed_params fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ _))); fintros; cbn [free_type_syms type_requirements solve_requirements listed_params]; nit. }
  destruct IH as (I0 & I1 & I2 & I3).
  refine (conj _ (conj _ (conj _ _))); fintros; cbn [free_type_syms type_requirements solve_requirements listed_params]; nit.
Qed.

Lemma ni_free_type_syms : forall fuel t mi out, NI (free_type_syms fuel t mi out).
Proof. intros fuel t mi out. exact ((proj1 (ni_free_type_syms_group fuel)) t mi out). Qed.
#[export] Hint Resolve ni_free_type_syms : nidb.

Lemma ni_type_requirements : forall fuel s, NI (type_requirements fuel s).
Proof. intros fuel s. exact ((proj1 (proj2 (ni_free_type_syms_group fuel))) s). Qed.
#[export] Hint Resolve ni_type_requirements : nidb.

Lemma ni_solve_requirements : forall fuel root, NI (solve_requirements fuel root).
Proof. intros fuel root. exact ((proj1 (proj2 (proj2 (ni_free_type_syms_group fuel)))) root). Qed.
#[export] Hint Resolve ni_solve_requirements : nidb.

Lemma ni_listed_params : forall fuel g, NI (listed_params fuel g).
Proof. intros fuel g. exact ((proj2 (proj2 (proj2 (ni_free_type_syms_group fuel)))) g). Qed.
#[export] Hint Resolve ni_listed_params : nidb.

Lemma ni_show_go_group : forall fuel,
  (forall t, NI (show_go fuel t)).
Proof.
  induction fuel as [|fuel IH].
  { fintros; cbn [show_go]; nit. }
  pose proof IH as I0. fintros; cbn [show_go]; nit.
Qed.

Lemma ni_show_go : forall fuel t, NI (show_go fuel t).
Proof. intros fuel t. exact ((ni_show_go_group fuel) t). Qed.
#[export] Hint Resolve ni_show_go : nidb.

Lemma ni_unify_group : forall fuel,
  (forall e mi a b why, NI (unify fuel e mi a b why)).
Proof.
  induction fuel as [|fuel IH].
  { fintros; cbn [unify]; nit. }
  pose proof IH as I0. fintros; cbn [unify]; nit.
Qed.

Lemma ni_unify : forall fuel e mi a b why, NI (unify fuel e mi a b why).
Proof. intros fuel e mi a b why. exact ((ni_unify_group fuel) e mi a b why). Qed.
#[export] Hint Resolve ni_unify : nidb.

Lemma ni_elab_type_group : forall fuel,
  (forall t mi tm e this, NI (elab_type fuel t mi tm e this)) /\
  (forall e mi bs outer, NI (app_subst fuel e mi bs outer)) /\
  (forall mi g tm e, NI (symbol_type fuel mi g tm e)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ _)); fintros; cbn [elab_type app_subst symbol_type]; nit. }
  destruct IH as (I0 & I1 & I2).
  refine (conj _ (conj _ _)); fintros; cbn [elab_type app_subst symbol_type]; nit.
Qed.

Lemma ni_elab_type : forall fuel t mi tm e this, NI (elab_type fuel t mi tm e this).
Proof. intros fuel t mi tm e this. exact ((proj1 (ni_elab_type_group fuel)) t mi tm e this). Qed.
#[export] Hint Resolve ni_elab_type : nidb.

Lemma ni_app_subst : forall fuel e mi bs outer, NI (app_subst fuel e mi bs outer).
Proof. intros fuel e mi bs outer. exact ((proj1 (proj2 (ni_elab_type_group fuel))) e mi bs outer). Qed.
#[export] Hint Resolve ni_app_subst : nidb.

Lemma ni_symbol_type : forall fuel mi g tm e, NI (symbol_type fuel mi g tm e).
Proof. intros fuel mi g tm e. exact ((proj2 (proj2 (ni_elab_type_group fuel))) mi g tm e). Qed.
#[export] Hint Resolve ni_symbol_type : nidb.

Lemma ni_result_mentions_go_group : forall fuel,
  (forall self mi specs out, NI (result_mentions_go fuel self mi specs out)) /\
  (forall g, NI (functor_fixed_go fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); fintros; cbn [result_mentions_go functor_fixed_go]; nit. }
  destruct IH as (I0 & I1).
  refine (conj _ _); fintros; cbn [result_mentions_go functor_fixed_go]; nit.
Qed.

Lemma ni_result_mentions_go : forall fuel self mi specs out, NI (result_mentions_go fuel self mi specs out).
Proof. intros fuel self mi specs out. exact ((proj1 (ni_result_mentions_go_group fuel)) self mi specs out). Qed.
#[export] Hint Resolve ni_result_mentions_go : nidb.

Lemma ni_functor_fixed_go : forall fuel g, NI (functor_fixed_go fuel g).
Proof. intros fuel g. exact ((proj2 (ni_result_mentions_go_group fuel)) g). Qed.
#[export] Hint Resolve ni_functor_fixed_go : nidb.

Lemma ni_check_literal_sig : forall e mi m oty args sg,
  NI (check_literal_sig e mi m oty args sg).
Proof.
  intros. unfold check_literal_sig. destruct (literal_sig_ok e oty args sg); nit.
Qed.
#[export] Hint Resolve ni_check_literal_sig : nidb.

Lemma ni_add_symbol_item_group : forall fuel,
  (forall e mi g subst needs, NI (add_symbol_item fuel e mi g subst needs)) /\
  (forall e mi sp outer needs, NI (add_spec fuel e mi sp outer needs)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); fintros; cbn [add_symbol_item add_spec]; nit. }
  destruct IH as (I0 & I1).
  refine (conj _ _); fintros; cbn [add_symbol_item add_spec]; nit.
Qed.

Lemma ni_add_symbol_item : forall fuel e mi g subst needs, NI (add_symbol_item fuel e mi g subst needs).
Proof. intros fuel e mi g subst needs. exact ((proj1 (ni_add_symbol_item_group fuel)) e mi g subst needs). Qed.
#[export] Hint Resolve ni_add_symbol_item : nidb.

Lemma ni_add_spec : forall fuel e mi sp outer needs, NI (add_spec fuel e mi sp outer needs).
Proof. intros fuel e mi sp outer needs. exact ((proj2 (ni_add_symbol_item_group fuel)) e mi sp outer needs). Qed.
#[export] Hint Resolve ni_add_spec : nidb.

Lemma ni_ety_ty_go_group : forall fuel,
  (forall t, NI (ety_ty_go fuel t)).
Proof.
  induction fuel as [|fuel IH].
  { fintros; cbn [ety_ty_go]; nit. }
  pose proof IH as I0. fintros; cbn [ety_ty_go]; nit.
Qed.

Lemma ni_ety_ty_go : forall fuel t, NI (ety_ty_go fuel t).
Proof. intros fuel t. exact ((ni_ety_ty_go_group fuel) t). Qed.
#[export] Hint Resolve ni_ety_ty_go : nidb.

Lemma ni_slots_go_group : forall fuel,
  (forall e t visiting, NI (slots_go fuel e t visiting)).
Proof.
  induction fuel as [|fuel IH].
  { fintros; cbn [slots_go]; nit. }
  pose proof IH as I0. fintros; cbn [slots_go]; nit.
Qed.

Lemma ni_slots_go : forall fuel e t visiting, NI (slots_go fuel e t visiting).
Proof. intros fuel e t visiting. exact ((ni_slots_go_group fuel) e t visiting). Qed.
#[export] Hint Resolve ni_slots_go : nidb.

Lemma ni_expand_spec_group : forall fuel,
  (forall mi sp out, NI (expand_spec fuel mi sp out)).
Proof.
  induction fuel as [|fuel IH].
  { fintros; cbn [expand_spec]; nit. }
  pose proof IH as I0. fintros; cbn [expand_spec]; nit.
Qed.

Lemma ni_expand_spec : forall fuel mi sp out, NI (expand_spec fuel mi sp out).
Proof. intros fuel mi sp out. exact ((ni_expand_spec_group fuel) mi sp out). Qed.
#[export] Hint Resolve ni_expand_spec : nidb.

Lemma ni_functor_total_group : forall fuel,
  (forall g, NI (functor_total fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { fintros; cbn [functor_total]; nit. }
  pose proof IH as I0. fintros; cbn [functor_total]; nit.
Qed.

Lemma ni_functor_total : forall fuel g, NI (functor_total fuel g).
Proof. intros fuel g. exact ((ni_functor_total_group fuel) g). Qed.
#[export] Hint Resolve ni_functor_total : nidb.

Lemma ni_provide_spec_group : forall fuel,
  (forall e fg mi sp outer out, NI (provide_spec fuel e fg mi sp outer out)).
Proof.
  induction fuel as [|fuel IH].
  { fintros; cbn [provide_spec]; nit. }
  pose proof IH as I0. fintros; cbn [provide_spec]; nit.
Qed.

Lemma ni_provide_spec : forall fuel e fg mi sp outer out, NI (provide_spec fuel e fg mi sp outer out).
Proof. intros fuel e fg mi sp outer out. exact ((ni_provide_spec_group fuel) e fg mi sp outer out). Qed.
#[export] Hint Resolve ni_provide_spec : nidb.

Lemma ni_provide_result : forall e fg roles, NI (provide_result e fg roles).
Proof. intros; unfold provide_result; nit. Qed.
#[export] Hint Resolve ni_provide_result : nidb.

Lemma ni_fresh_target_output_view : forall e fg, NI (fresh_target_output_view e fg).
Proof. intros; unfold fresh_target_output_view; nit. Qed.
#[export] Hint Resolve ni_fresh_target_output_view : nidb.

Lemma ni_bind_fixed_present : forall mi e fg xs, NI (bind_fixed_present mi e fg xs).
Proof. intros; unfold bind_fixed_present; nit. Qed.
#[export] Hint Resolve ni_bind_fixed_present : nidb.

Lemma ni_copy_fixed_current : forall mi e fg xs, NI (copy_fixed_current mi e fg xs).
Proof. intros; unfold copy_fixed_current; nit. Qed.
#[export] Hint Resolve ni_copy_fixed_current : nidb.

Lemma ni_defined_link_inputs_compatible : forall mi site ef eg fg bg fixed fneeds gneeds,
  NI(defined_link_inputs_compatible mi site ef eg fg bg fixed fneeds gneeds).
Proof. intros;unfold defined_link_inputs_compatible;nit. Qed.
#[export]Hint Resolve ni_defined_link_inputs_compatible:nidb.

Lemma ni_check_link_fixed_current : forall mi site fg bg fixed,
  NI(check_link_fixed_current mi site fg bg fixed).
Proof. intros;unfold check_link_fixed_current;nit. Qed.
#[export] Hint Resolve ni_check_link_fixed_current : nidb.

Lemma ni_check_constant_effects : forall mi x, NI (check_constant_effects mi x).
Proof. intros; unfold check_constant_effects; nit. Qed.
#[export] Hint Resolve ni_check_constant_effects : nidb.

Lemma ni_check_provider_effects : forall mi bs, NI (check_provider_effects mi bs).
Proof. intros; unfold check_provider_effects; nit. Qed.
#[export] Hint Resolve ni_check_provider_effects : nidb.

Lemma ni_check_captured_reading : forall mi site prototype formal actual,
  NI(check_captured_reading mi site prototype formal actual).
Proof. intros;unfold check_captured_reading;nit. Qed.
#[export] Hint Resolve ni_check_captured_reading : nidb.

Lemma ni_copy_output_compatible : forall mi site ef eg fg bg fixed fneeds gneeds, NI (copy_output_compatible mi site ef eg fg bg fixed fneeds gneeds).
Proof. intros; unfold copy_output_compatible; nit. Qed.
#[export] Hint Resolve ni_copy_output_compatible : nidb.

Lemma ni_family_leaves_ok_group : forall fuel,
  (forall mi ps specs, NI (family_leaves_ok fuel mi ps specs)).
Proof.
  induction fuel as [|fuel IH].
  { fintros; cbn [family_leaves_ok]; nit. }
  pose proof IH as I0. fintros; cbn [family_leaves_ok]; nit.
Qed.

Lemma ni_family_leaves_ok : forall fuel mi ps specs, NI (family_leaves_ok fuel mi ps specs).
Proof. intros fuel mi ps specs. exact ((ni_family_leaves_ok_group fuel) mi ps specs). Qed.
#[export] Hint Resolve ni_family_leaves_ok : nidb.

Lemma ni_family_leaves_group : forall fuel,
  (forall e mi specs outer, NI (family_leaves fuel e mi specs outer)).
Proof.
  induction fuel as [|fuel IH].
  { fintros; cbn [family_leaves]; nit. }
  pose proof IH as I0. fintros; cbn [family_leaves]; nit.
Qed.

Lemma ni_family_leaves : forall fuel e mi specs outer, NI (family_leaves fuel e mi specs outer).
Proof. intros fuel e mi specs outer. exact ((ni_family_leaves_group fuel) e mi specs outer). Qed.
#[export] Hint Resolve ni_family_leaves : nidb.

(* ======================================================================== *)
(* 2. The cache's invariant                                                  *)
(* ======================================================================== *)

(* what of a state its successors keep that Later reads *)
Ltac wrapper_cache_same :=
 lazymatch goal with
 | H : ?m ?si=Good(?a,?sf) |- st_wrappers ?sf=st_wrappers ?si =>
   let K:=fresh "WrKeep" in assert(K:keeps st_wrappers m)by keepst;
   exact(K _ _ _ H)
 end.

Lemma info_step : forall s s', InfoInv s -> FrL s s' -> st_info s' = st_info s ->
 st_wrappers s'=st_wrappers s ->InfoInv s'.
Proof.
 intros s s' I F E Wr;constructor.
 - intros x i H;rewrite E in H;exact(info_entry_keep _ _ _ _ (I x i H)F).
 - intros p id H;rewrite Wr in H;exact(wrapper_entry_keep _ _ _ _ (info_wrappers _ I _ _ H)F).
Qed.

Definition okI {A} (m : M A) : Prop :=
  ok m /\ forall s a s', Inv s -> InfoInv s -> m s = Good (a, s') -> InfoInv s'.
Arguments okI : simpl never.

Lemma okI_ni : forall A (m : M A), ok m -> NI m ->keeps st_wrappers m ->okI m.
Proof.
  intros A m Ho Hn Hw. split; [exact Ho|]. intros s a s' Hi I H.
  destruct (Ho s a s' Hi H) as [_ F]. exact (info_step s s' I (frl_of_fr _ _ F) (Hn s a s' H)(Hw s a s' H)).
Qed.

Lemma okI_ret : forall A (a : A), okI (ret a).
Proof. intros. apply okI_ni; [apply ok_ret | apply ni_ret|unfold keeps;intros;injection H as _ <-;reflexivity]. Qed.

Lemma okI_get : okI get.
Proof. apply okI_ni; [apply ok_get | unfold get; nit | unfold get; keepst]. Qed.
Lemma okI_fail : forall A msg, @okI A (fail msg).
Proof. intros. apply okI_ni; [apply ok_fail | apply ni_fail | unfold fail; keepst]. Qed.
Lemma okI_sym : forall id, okI (sym id).
Proof. intros. apply okI_ni; [unfold sym; okt | unfold sym; nit | unfold sym; keepst]. Qed.

Lemma okI_bind : forall A B (m : M A) (k : A -> M B), okI m -> (forall a, okI (k a)) -> okI (mbind m k).
Proof.
  intros A B m k [Om Pm] Hk. split; [apply ok_bind; [exact Om | intro a; exact (proj1 (Hk a))]|].
  intros s b s'' Hi I H. apply bind_inv in H as (a & s' & H1 & H2).
  destruct (Om _ _ _ Hi H1) as [Hi' _]. exact (proj2 (Hk a) _ _ _ Hi' (Pm _ _ _ Hi I H1) H2).
Qed.

Lemma okI_mmap : forall A B (f : A -> M B) l, (forall x, okI (f x)) -> okI (mmap f l).
Proof.
  intros A B f l Hf. induction l as [|x r IH]; cbn [mmap]; [apply okI_ret|].
  apply okI_bind; [apply Hf | intro y; apply okI_bind; [exact IH | intro; apply okI_ret]].
Qed.

Lemma okI_mfold : forall A B (f : A -> B -> M A) l a, (forall a x, okI (f a x)) -> okI (mfold f a l).
Proof.
  intros A B f l. induction l as [|x r IH]; intros a Hf; cbn [mfold]; [apply okI_ret|].
  apply okI_bind; [apply Hf | intro a'; apply IH; exact Hf].
Qed.

Lemma okI_miter : forall A (f : A -> M unit) l, (forall x, okI (f x)) -> okI (miter f l).
Proof.
  intros A f l Hf. induction l as [|x r IH]; cbn [miter]; [apply okI_ret|].
  apply okI_bind; [apply Hf | intro; exact IH].
Qed.

Lemma okI_with_vars : forall A init (m : M A), okI m -> okI (with_vars init m).
Proof.
  intros A init m [Om Pm]. split; [apply ok_with_vars; exact Om|]. intros s a s' Hi I H. unfold with_vars in H.
  apply bind_inv in H as (s0 & s1 & E0 & H). injection E0 as <- <-.
  apply bind_inv in H as (u & s2 & E1 & H). unfold upd in E1. injection E1 as <- <-.
  apply bind_inv in H as (r & s3 & H3 & H).
  apply bind_inv in H as (u' & s4 & E2 & H). unfold upd in E2. injection E2 as <- <-.
  injection H as <- <-.
  assert (Hi2 : Inv (s_vars init (List.length init) s)) by (destruct Hi; constructor; assumption).
  assert (I2 : InfoInv (s_vars init (List.length init) s))
    by (apply (info_step s); [exact I | constructor; cbn; auto; [exists []; rewrite app_nil_r; reflexivity | apply ext_refl] | reflexivity|reflexivity]).
  pose proof (Pm _ _ _ Hi2 I2 H3) as I3.
  apply (info_step s3); [exact I3 | constructor; cbn; auto; [exists []; rewrite app_nil_r; reflexivity | apply ext_refl] | reflexivity|reflexivity].
Qed.

(* ---- fn_info writes the cache ---- *)

Lemma ok_fi_walk : forall g e, ok (fi_walk g e).
Proof. intros g e. unfold fi_walk. destruct (gs_origin g); okt. Qed.

Lemma ni_fi_walk : forall g e, NI (fi_walk g e).
Proof. intros g e. unfold fi_walk. destruct (gs_origin g); nit. Qed.

Lemma ok_merged_fi : forall g, ok (merged_walk (gs_mod g) (fi_walk g)).
Proof. intro g. rewrite (mw_go_eq (fi_walk g)). apply ok_mw_go. apply ok_fi_walk. Qed.

Lemma ni_merged_fi : forall g, NI (merged_walk (gs_mod g) (fi_walk g)).
Proof.
  intro g. rewrite (mw_go_eq (fi_walk g)).
  assert (G : forall n tne ufe, NI (mw_go (fi_walk g) (gs_mod g) n tne ufe)); [|apply G].
  intro n. induction n as [|n IH]; intros tne ufe; cbn [mw_go]; apply ni_bind; try apply ni_fi_walk;
    intros [e needs]; cbn beta iota zeta; destruct (edges_eqb _ _); try apply ni_ret; [unfold err; nit | apply IH].
Qed.

Lemma kwr_merged_fi:forall g,keeps st_wrappers(merged_walk(gs_mod g)(fi_walk g)).
Proof. intro g;apply kwr_merged_walk_frame;intro;unfold fi_walk;destruct(gs_origin g);keepst. Qed.
Local Strategy opaque[merged_walk].
Lemma okI_fn_info : forall g0, okI (fn_info g0).
Proof.
  intro g0. assert (Ok : ok (fn_info g0)) by apply ok_fn_info_frame. split; [exact Ok|].
  intros s a s' Hi I H. pose proof H as H0. unfold fn_info in H.
  apply bind_inv in H as (x & s2 & E & H). injection E as <- <-.
  destruct (nget (st_info s) (gs_id g0)) as [i|] eqn:Ei.
  { injection H as <- <-. exact I. }
  destruct (fn_info_miss g0 s a s' H0 Ei) as (g & e & needs & s1 & Eg & H1 & Ea & Es).
  destruct (ok_merged_fi g s _ s1 Hi H1) as [Hi1 F1].
  pose proof (ni_merged_fi g s _ s1 H1) as N1.
  assert (I1 : InfoInv s1) by exact (info_step s s1 I (frl_of_fr _ _ F1) N1 (kwr_merged_fi g _ _ _ H1)).
  assert (F2 : FrL s1 s') by (rewrite Es; constructor; cbn; auto; [exists []; rewrite app_nil_r; reflexivity | apply ext_refl]).
  constructor.
  2:{intros p id Hit;eapply wrapper_entry_keep;[apply(info_wrappers _ I1);rewrite Es in Hit;exact Hit|exact F2]. }
  intros y j Hy. rewrite Es in Hy. cbn [st_info] in Hy. rewrite nget_nset in Hy.
  destruct (Nat.eqb_spec y (gs_id g0)) as [->|Ne].
  - injection Hy as <-. left. exists g, s, e, needs, s1. auto 6.
  - destruct (I1 y j Hy) as [(g' & s0 & e' & n' & s3 & P1 & P2 & P3 & P4 & P5)|Nl].
    + left. exists g', s0, e', n', s3. repeat (split; [assumption|]). exact (frl_trans _ _ _ P5 F2).
    + right. exact (nlx_frl _ _ _ Nl F2).
Qed.
Local Strategy transparent[merged_walk].

(* ---- lambda_provider writes the cache, for the symbol it made ---- *)

Definition okIF {A} (pid : nat) (m : M A) : Prop :=
  ok m /\ forall s a s', Inv s -> InfoInv s -> NLx s pid -> m s = Good (a, s') -> InfoInv s'.

Lemma okIF_of_okI : forall A pid (m : M A), okI m -> okIF pid m.
Proof. intros A pid m [O P]. split; [exact O|]. intros s a s' Hi I _ H. exact (P s a s' Hi I H). Qed.

Lemma okIF_bind : forall A B pid (m : M A) (k : A -> M B), okIF pid m -> (forall a, okIF pid (k a)) -> okIF pid (mbind m k).
Proof.
  intros A B pid m k [Om Pm] Hk. split; [apply ok_bind; [exact Om | intro a; exact (proj1 (Hk a))]|].
  intros s b s'' Hi I N H. apply bind_inv in H as (a & s' & H1 & H2).
  destruct (Om _ _ _ Hi H1) as [Hi' F]. refine (proj2 (Hk a) s' b s'' Hi' (Pm _ _ _ Hi I N H1) _ H2).
  exact (nlx_frl _ _ _ N (frl_of_fr _ _ F)).
Qed.

Lemma okIF_with_vars : forall A pid init (m : M A), okIF pid m -> okIF pid (with_vars init m).
Proof.
  intros A pid init m [Om Pm]. split; [apply ok_with_vars; exact Om|]. intros s a s' Hi I N H. unfold with_vars in H.
  apply bind_inv in H as (s0 & s1 & E0 & H). injection E0 as <- <-.
  apply bind_inv in H as (u & s2 & E1 & H). unfold upd in E1. injection E1 as <- <-.
  apply bind_inv in H as (r & s3 & H3 & H).
  apply bind_inv in H as (u' & s4 & E2 & H). unfold upd in E2. injection E2 as <- <-.
  injection H as <- <-.
  assert (Hi2 : Inv (s_vars init (List.length init) s)) by (destruct Hi; constructor; assumption).
  assert (I2 : InfoInv (s_vars init (List.length init) s))
    by (apply (info_step s); [exact I | constructor; cbn; auto; [exists []; rewrite app_nil_r; reflexivity | apply ext_refl] | reflexivity|reflexivity]).
  pose proof (Pm _ _ _ Hi2 I2 N H3) as I3.
  apply (info_step s3); [exact I3 | constructor; cbn; auto; [exists []; rewrite app_nil_r; reflexivity | apply ext_refl] | reflexivity|reflexivity].
Qed.


Lemma frl_info : forall s f, FrL s (s_info f s).
Proof. intros s f. constructor; cbn; auto; [exists []; rewrite app_nil_r; reflexivity | apply ext_refl]. Qed.

Lemma entry_frl : forall s s' x i, InfoEntry s x i -> FrL s s' -> InfoEntry s' x i.
Proof.
  intros s s' x i [(g & s0 & e & needs & s1 & H1 & H2 & H3 & H4 & H5)|N] F.
  - left. exists g, s0, e, needs, s1. repeat (split; [assumption|]). exact (frl_trans _ _ _ H5 F).
  - right. exact (nlx_frl _ _ _ N F).
Qed.

Lemma okIF_write : forall pid info, okIF pid (upd (s_info (fun m => nset m pid info))).
Proof.
  intros pid info. split; [okt|]. intros s a s' Hi I N H. unfold upd in H. injection H as _ <-.
  constructor.
  2:{intros p id Hit;eapply wrapper_entry_keep;[apply(info_wrappers _ I);exact Hit|apply frl_info]. }
  intros y j Hy. cbn [s_info st_info] in Hy. rewrite nget_nset in Hy. destruct (Nat.eqb_spec y pid) as [->|Ne].
  - right. exact N.
  - exact (entry_frl _ _ _ _ (I y j Hy) (frl_info s _)).
Qed.

Lemma okI_new_sym_bind : forall A mi n k d o (K : nat -> M A), o <> OSource -> k <> KTag /\ k <> KUnit -> (forall t, o <> OForwarder t) ->
  (forall pid, okIF pid (K pid)) -> okI (mbind (new_sym mi n k d o) K).
Proof.
  intros A mi n k d o K Ho Hk Hf HK. split; [apply ok_bind; [apply ok_new_sym; [exact Ho | exact Hk] | intro pid; exact (proj1 (HK pid))]|].
  intros s a s'' Hi I H. apply bind_inv in H as (pid & s1 & H1 & H2).
  destruct (ok_new_sym mi n k d o Ho Hk s pid s1 Hi H1) as [Hi1 F1].
  assert (N1 : NI (new_sym mi n k d o)) by (unfold new_sym; nit).
  pose proof (info_step s s1 I (frl_of_fr _ _ F1) (N1 _ _ _ H1) ltac:(assert(WrKeep:keeps st_wrappers(new_sym mi n k d o))by(unfold new_sym;keepst);exact(WrKeep _ _ _ H1))) as I1.
  apply (proj2 (HK pid) s1 a s'' Hi1 I1); [|exact H2].
  unfold new_sym, fresh in H1.
  apply bind_inv in H1 as (id & s2 & E1 & H1). injection E1 as <- <-.
  apply bind_inv in H1 as (u & s3 & E2 & H1). unfold upd in E2. injection E2 as <- <-. injection H1 as <- <-.
  split.
  - intros b Hb. cbn in Hb |- *.
    destruct (nget (w_syms (st_w s)) (st_next s)) as [g|] eqn:Eg; [|reflexivity].
    pose proof (iv_loader _ Hi b Hb _ _ Eg) as Es. rewrite (iv_above _ Hi (st_next s) (le_n _)) in Es. discriminate.
  - eexists. cbn. split; [apply nget_nset_same | exact Hf].
Qed.

(* a forwarder's continuation never writes the cache under the forwarder *)
Lemma okI_new_sym_fwd : forall A mi n k d t (K : nat -> M A), k <> KTag /\ k <> KUnit ->
  (forall pid, okI (K pid)) -> okI (mbind (new_sym mi n k d (OForwarder t)) K).
Proof.
  intros A mi n k d t K Hk HK. apply okI_bind; [|exact HK].
  apply okI_ni; [apply ok_new_sym; [discriminate | exact Hk] | unfold new_sym; nit|unfold new_sym;keepst].
Qed.


Lemma ok_wrapper_create : forall pv,ok(wrapper_create pv).
Proof. intro pv;unfold wrapper_create;okt. Qed.

Lemma wrapper_create_receipt:forall pv s id sf,
 Inv s ->InfoInv s ->PhysicalProvider s pv ->
 wrapper_create pv s=Good(id,sf)->
 InfoInv sf.
Proof.
 intros pv s id sf Is I Physical Run.
 pose proof Run as Full;unfold wrapper_create in Run.
 apply bind_inv in Run as(info&s1&E1&Run).
 apply bind_inv in Run as(sm&s2&E2&Run).
 apply bind_inv in Run as(w&s3&E3&Run).
 apply bind_inv in Run as(th&s4&E4&Run).
 apply bind_inv in Run as(u&s5&E5&Run).
 apply bind_inv in Run as(v&s6&E6&Run).
 apply bind_inv in Run as(z&s7&E7&Run).
 injection Run as <- <-.
 destruct(proj1(okI_fn_info pv)s info s1 Is E1)as[Is1 F1].
 pose proof(proj2(okI_fn_info pv)s info s1 Is I E1)as I1.
 assert(O2:ok(smap_at(fi_env info)(fi_env info)(map(fun k=>(k,k))(fi_needs info))))by okt.
 destruct(O2 s1 sm s2 Is1 E2)as[Is2 F2].
 assert(O3:ok(new_sym(gs_mod pv)(gs_name pv++"$recv")%string KFn(gs_decl pv)OLambda))by okt.
 destruct(O3 s2 w s3 Is2 E3)as[Is3 F3].
 assert(O4:ok(call_theta(fi_env info)pv))by okt.
 destruct(O4 s3 th s4 Is3 E4)as[Is4 F4].
 assert(O5:ok(register_reify_code(ROFunction w)(fi_env info)(fi_needs info)None))by okt.
 destruct(O5 s4 u s5 Is4 E5)as[Is5 F5].
 assert(O6:ok(emit_fn w(XCallD(gs_id pv)th sm(map XVar(seq 1(List.length(fn_params pv)))))))by okt.
 destruct(O6 s5 v s6 Is5 E6)as[Is6 F6].
 assert(O7:ok(upd(s_wrappers(fun m=>nset m(gs_id pv)w))))by okt.
 destruct(O7 s6 z s7 Is6 E7)as[Is7 F7].
 assert(Wcache:st_wrappers s6=st_wrappers s).
 { assert(W1:keeps st_wrappers(fn_info pv))by keepst.
   assert(W2:keeps st_wrappers(smap_at(fi_env info)(fi_env info)(map(fun k=>(k,k))(fi_needs info))))by keepst.
   assert(W3:keeps st_wrappers(new_sym(gs_mod pv)(gs_name pv++"$recv")%string KFn(gs_decl pv)OLambda))by(unfold new_sym;keepst).
   assert(W4:keeps st_wrappers(call_theta(fi_env info)pv))by keepst.
   assert(W5:keeps st_wrappers(register_reify_code(ROFunction w)(fi_env info)(fi_needs info)None))by keepst.
   assert(W6:keeps st_wrappers(emit_fn w(XCallD(gs_id pv)th sm(map XVar(seq 1(List.length(fn_params pv)))))))by(unfold emit_fn;keepst).
   rewrite(W6 _ _ _ E6),(W5 _ _ _ E5),(W4 _ _ _ E4),(W3 _ _ _ E3),(W2 _ _ _ E2),(W1 _ _ _ E1);reflexivity. }
 assert(Fall:FrL s1 s7).
 { eapply frl_trans;[apply frl_of_fr;exact F2|];eapply frl_trans;[apply frl_of_fr;exact F3|];eapply frl_trans;[apply frl_of_fr;exact F4|];eapply frl_trans;[apply frl_of_fr;exact F5|];eapply frl_trans;[apply frl_of_fr;exact F6|];apply frl_of_fr;exact F7. }
 assert(Ncache:st_info s7=st_info s1).
 { assert(N2:NI(smap_at(fi_env info)(fi_env info)(map(fun k=>(k,k))(fi_needs info))))by nit.
   assert(N3:NI(new_sym(gs_mod pv)(gs_name pv++"$recv")%string KFn(gs_decl pv)OLambda))by(unfold new_sym;nit).
   assert(N4:NI(call_theta(fi_env info)pv))by nit.
   assert(N5:NI(register_reify_code(ROFunction w)(fi_env info)(fi_needs info)None))by nit.
   assert(N6:NI(emit_fn w(XCallD(gs_id pv)th sm(map XVar(seq 1(List.length(fn_params pv)))))))by(unfold emit_fn;nit).
   assert(N7:NI(upd(s_wrappers(fun m=>nset m(gs_id pv)w))))by nit.
   rewrite(N7 _ _ _ E7),(N6 _ _ _ E6),(N5 _ _ _ E5),(N4 _ _ _ E4),(N3 _ _ _ E3),(N2 _ _ _ E2);reflexivity. }
 constructor.
 - intros x j Hit;rewrite Ncache in Hit.
   eapply info_entry_keep;[exact(I1 x j Hit)|exact Fall].
 - intros p j Hit;unfold upd in E7;injection E7 as _ State;subst s7.
   cbn[s_wrappers st_wrappers]in Hit;rewrite nget_nset in Hit.
   destruct(Nat.eqb_spec p(gs_id pv))as[->|Ne].
   + injection Hit as <-.
     exists pv,info,th,sm,s1,s2,s3,s4,s2,s3.
     split;[reflexivity|].
     split;[eapply fl_keep;[eapply frl_trans;[apply frl_of_fr;exact F1|exact Fall]|exact Physical]|].
     split;[eapply info_entry_keep;[apply I1;apply fn_info_stored in E1;exact E1|exact Fall]|].
     split;[exact Is1|];split;[exact E2|].
     split;[eapply frl_trans;[apply frl_of_fr;exact F3|];eapply frl_trans;[apply frl_of_fr;exact F4|];eapply frl_trans;[apply frl_of_fr;exact F5|];eapply frl_trans;[apply frl_of_fr;exact F6|];apply frl_of_fr;exact F7|].
     split;[exact Is3|];split;[exact E4|].
     split;[eapply frl_trans;[apply frl_of_fr;exact F5|];eapply frl_trans;[apply frl_of_fr;exact F6|];apply frl_of_fr;exact F7|].
     split;[exact Is2|].
     split;[eapply fr_keep;[exact F2|];eapply fr_keep;[exact F1|exact Physical]|].
     split;[exact E3|].
     split;[eapply frl_trans;[apply frl_of_fr;exact F4|];eapply frl_trans;[apply frl_of_fr;exact F5|];eapply frl_trans;[apply frl_of_fr;exact F6|];apply frl_of_fr;exact F7|].
     split.
     * eapply fr_keep;[exact F7|];eapply fr_keep;[exact F6|];eapply fr_keep;[exact F5|];eapply fr_keep;[exact F4|].
       exact(proj2(proj2(new_sym_shape _ _ _ _ _ _ _ _ E3))).
     * unfold emit_fn,upd in E6;injection E6 as _ <-;cbn[s_wrappers st_fns s_fns];left;reflexivity.
   + eapply wrapper_entry_keep;[exact(info_wrappers _ I p j ltac:(rewrite Wcache in Hit;exact Hit))|].
     eapply frl_trans;[apply frl_of_fr;exact F1|exact Fall].
Qed.

Lemma okI_wrapper_cached:forall pv,okI(wrapper_cached pv).
Proof.
 intro pv;split.
 {unfold wrapper_cached;apply ok_bind;[okt|];intro ss.
  destruct(nget(st_wrappers ss)(gs_id pv));[okt|].
  apply ok_bind;[okt|];intro physical;apply ok_wrapper_create. }
 intros s id sf Is I Run;unfold wrapper_cached in Run.
 apply bind_inv in Run as(ss&sg&Get&Run);injection Get as <- <-.
 destruct(nget(st_wrappers s)(gs_id pv))as[cached|]eqn:Cache.
 - injection Run as <- <-;exact I.
 - apply bind_inv in Run as(physical&s1&Lookup&Create).
   destruct(sym_physical_provider _ _ _ _ Lookup Is)as[State Physical].
   subst s1;eapply wrapper_create_receipt;eassumption.
Qed.

(* ======================================================================== *)
(* 3. Every function of the elaborator keeps the cache's invariant           *)
(* ======================================================================== *)

Lemma okI_annotation_type:forall ty mi tm e this,okI(ety_of ty mi tm e this).
Proof. intros;apply okI_ni;[unfold ety_of;apply ok_elab_type|unfold ety_of;nit|apply kwr_ety_of_input]. Qed.

Lemma okI_reqs_input:forall x,okI(reqs x).
Proof. intros;unfold reqs;apply okI_ni;[apply ok_type_requirements|apply ni_type_requirements|apply kwr_type_requirements]. Qed.
Lemma okI_type_read:forall f t mi tm e this,okI(elab_type f t mi tm e this).
Proof. intros;apply okI_ni;[apply ok_elab_type|apply ni_elab_type|apply kwr_elab_type]. Qed.
Lemma okI_substitution_read:forall f e mi bs outer,okI(app_subst f e mi bs outer).
Proof. intros;apply okI_ni;[apply ok_app_subst|apply ni_app_subst|apply kwr_app_subst]. Qed.
Lemma okI_symbol_read:forall f mi g tm e,okI(symbol_type f mi g tm e).
Proof. intros;apply okI_ni;[apply ok_symbol_type|apply ni_symbol_type|apply kwr_symbol_type]. Qed.
Lemma ni_reqs_frame : forall x, NI (reqs x).
Proof. intro x. unfold reqs. apply ni_type_requirements. Qed.
#[export] Hint Resolve ni_reqs_frame : nidb.
Lemma ni_merged_walk : forall mi walk,
 (forall e, NI (walk e)) -> NI (merged_walk mi walk).
Proof.
 intros mi walk H;rewrite(mw_go_eq walk).
 assert(G:forall n tne ufe,NI(mw_go walk mi n tne ufe));[|apply G].
 intro n;induction n as[|n IH];intros tne ufe;cbn[mw_go];apply ni_bind;try apply H;
 intros[e needs];cbn beta iota zeta;destruct(edges_eqb _ _);try apply ni_ret;
 [unfold err;nit|apply IH].
Qed.
#[export] Hint Extern 1 (NI (merged_walk _ _)) => apply ni_merged_walk;intros;nit : nidb.
Local Strategy opaque [slots_go elab_type app_subst symbol_type reqs type_requirements merged_walk].
Lemma ok_fun_env_frame : forall g, ok (fun_env g).
Proof. intros;unfold fun_env;okt. all:apply ok_merged_walk_frame;intros;okt. Qed.
Lemma ni_fun_env_frame : forall g, NI (fun_env g).
Proof. intros;unfold fun_env;nit. Qed.
Lemma okI_fun_env : forall g, okI (fun_env g).
Proof. intro g. apply okI_ni;[apply ok_fun_env_frame|apply ni_fun_env_frame|apply kwr_fun_env_input]. Qed.


Create HintDb okIdb.
#[export] Hint Resolve okI_ret okI_get okI_fail okI_sym okI_fn_info : okIdb.
#[export] Hint Extern 5 (okI _) => (apply okI_ni; [solve [auto with okdb] | solve [nit]|solve[keepst]]) : okIdb.

Ltac okIfixind1 :=
  lazymatch goal with |- okI (?F ?N) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N as n; intro n; induction n as [|n IHn]; cbn beta iota end.
Ltac okIfixind3 :=
  lazymatch goal with |- okI (?F ?N ?a ?b) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N, a, b; intro n; induction n as [|n IHn]; intros; cbn beta iota end.

Local Lemma okI_pure_input : forall A (f : st -> A),
  okI (fun s => Good (f s, s)).
Proof.
  intros A f. split.
  - apply ok_reader. intros s a final Run. injection Run as _ <-. reflexivity.
  - intros s a final Is I Run. injection Run as _ <-. exact I.
Qed.

Ltac okIFt :=
  repeat (fintros; cbn zeta; lazymatch goal with
  | |- okIF _ (mbind _ _) => apply okIF_bind
  | |- okIF _ (with_vars _ _) => apply okIF_with_vars
  | |- okIF ?p (upd (s_info (fun m => nset m ?p _))) => apply okIF_write
  | |- okIF _ ?t =>
      first [ apply okIF_of_okI; solve [okIt]
            | match t with context [match ?x with _ => _ end] => destruct x end ]
  end)
with okIt :=
  repeat (fintros; lazymatch goal with
  | |- okI (wrapper_cached _) => apply okI_wrapper_cached
  | |- okI (fun_env _) => apply okI_fun_env
  | |- okI (ety_of _ _ _ _ _) => apply okI_annotation_type
  | |- okI (reqs _) => apply okI_reqs_input
  | |- okI (elab_type _ _ _ _ _ _) => apply okI_type_read
  | |- okI (app_subst _ _ _ _ _) => apply okI_substitution_read
  | |- okI (symbol_type _ _ _ _ _) => apply okI_symbol_read
  | |- okI (mbind (new_sym _ _ _ _ (OForwarder _)) _) => apply okI_new_sym_fwd; [split; discriminate|]; intro
  | |- okI (mbind (new_sym _ _ _ _ _) _) => apply okI_new_sym_bind; [discriminate | split; discriminate | intros ? ?; discriminate | intro; okIFt]
  | |- okI (mbind _ _) => apply okI_bind
  | |- okI (mmap _ _) => apply okI_mmap
  | |- okI (mfold _ _ _) => apply okI_mfold
  | |- okI (miter _ _) => apply okI_miter
  | |- okI (with_vars _ _) => apply okI_with_vars
  | |- okI (upd _) => apply okI_ni; [okt | nit|keepst]
  | |- okI (fun s => Good (_, s)) => apply okI_pure_input
  | |- okI (fun s => _) => apply okI_ni; [okt | nit|keepst]
  | |- okI ?t =>
      first [ (lazymatch t with match ?x with _ => _ end => destruct x end)
            | solve [auto with okIdb]
            | (lazymatch t with ?F _ => is_fix F end; okIfixind1)
            | (lazymatch t with ?F _ _ _ => is_fix F end; okIfixind3)
            | match t with context [match ?x with _ => _ end] => destruct x end
            | let h := head_of t in progress unfold h ]
  end).

Lemma okI_remember_reify_constant : forall id start, okI (remember_reify_constant id start).
Proof. intros. apply okI_ni; [apply ok_remember_reify_constant|unfold remember_reify_constant;nit|unfold remember_reify_constant;keepst]. Qed.
#[export] Hint Resolve okI_remember_reify_constant : okIdb.

Lemma okI_replay_reify_constant : forall id, okI (replay_reify_constant id).
Proof. intros. apply okI_ni; [apply ok_replay_reify_constant|unfold replay_reify_constant;nit|unfold replay_reify_constant;keepst]. Qed.
#[export] Hint Resolve okI_replay_reify_constant : okIdb.

Local Strategy opaque [ety_of slots_go elab_type app_subst symbol_type reqs type_requirements merged_walk].
Lemma okI_synth_group : forall fuel,
  (forall c e x exp, okI (synth fuel c e x exp)) /\
  (forall c e a exp, okI (synth_arg fuel c e a exp)) /\
  (forall c e b exp, okI (synth_block fuel c e b exp)) /\
  (forall c e ss, okI (check_stmts fuel c e ss)) /\
  (forall c e s, okI (check_stmt fuel c e s)) /\
  (forall c e y, okI (check_bool fuel c e y)) /\
  (forall c e x exp, okI (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, okI (synth_match fuel c e s arms exp)) /\
  (forall g, okI (const_val fuel g)) /\
  (forall c e p a, okI (synth_path fuel c e p a)) /\
  (forall c e nm sg args, okI (check_args fuel c e nm sg args)) /\
  (forall c e p a args, okI (synth_call fuel c e p a args)) /\
  (forall c e p a fs, okI (synth_record fuel c e p a fs)) /\
  (forall c e o n, okI (synth_field fuel c e o n)) /\
  (forall c e o p args, okI (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, okI (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, okI (apply_family fuel c e fg th want)) /\
  (forall c e info, okI (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, okI (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, okI (lang_call fuel c e o n args why)) /\
  (forall c e ds why, okI (int_literal fuel c e ds why)) /\
  (forall c e v why, okI (float_of fuel c e v why)) /\
  (forall c e text, okI (synth_literal fuel c e text)) /\
  (forall c e sp y, okI (check_bind fuel c e sp y)) /\
  (forall c e y, okI (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, okI (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, okI (wrapper fuel pv)) /\
  (forall tg, okI (forwarder fuel tg)) /\
  (forall g, okI (lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, okI (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, okI (apply_functor fuel c e sp)) /\
  (forall c e g, okI (apply_body fuel c e g)) /\
  (forall c e fg, okI (apply_abstract fuel c e fg)) /\
  (forall c e fg y, okI (bind_functor fuel c e fg y)) /\
  (forall fg bg, okI (link_body fuel fg bg)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))); fintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; okIt. }
  destruct IH as (I0 & I1 & I2 & I3 & I4 & I5 & I6 & I7 & I8 & I9 & I10 & I11 & I12 & I13 & I14 & I15 & I16 & I17 & I18 & I19 & I20 & I21 & I22 & I23 & I24 & I25 & I26 & I27 & I28 & I29 & I30 & I31 & I32 & I33 & I34).
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))); fintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; okIt.
Qed.

Lemma okI_synth : forall fuel c e x exp, okI (synth fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (okI_synth_group fuel)) c e x exp). Qed.
#[export] Hint Resolve okI_synth : okIdb.

Lemma okI_synth_arg : forall fuel c e a exp, okI (synth_arg fuel c e a exp).
Proof. intros fuel c e a exp. exact ((proj1 (proj2 (okI_synth_group fuel))) c e a exp). Qed.
#[export] Hint Resolve okI_synth_arg : okIdb.

Lemma okI_synth_block : forall fuel c e b exp, okI (synth_block fuel c e b exp).
Proof. intros fuel c e b exp. exact ((proj1 (proj2 (proj2 (okI_synth_group fuel)))) c e b exp). Qed.
#[export] Hint Resolve okI_synth_block : okIdb.

Lemma okI_check_stmts : forall fuel c e ss, okI (check_stmts fuel c e ss).
Proof. intros fuel c e ss. exact ((proj1 (proj2 (proj2 (proj2 (okI_synth_group fuel))))) c e ss). Qed.
#[export] Hint Resolve okI_check_stmts : okIdb.

Lemma okI_check_stmt : forall fuel c e s, okI (check_stmt fuel c e s).
Proof. intros fuel c e s. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))) c e s). Qed.
#[export] Hint Resolve okI_check_stmt : okIdb.

Lemma okI_check_bool : forall fuel c e y, okI (check_bool fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))) c e y). Qed.
#[export] Hint Resolve okI_check_bool : okIdb.

Lemma okI_synth_if : forall fuel c e x exp, okI (synth_if fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))) c e x exp). Qed.
#[export] Hint Resolve okI_synth_if : okIdb.

Lemma okI_synth_match : forall fuel c e s arms exp, okI (synth_match fuel c e s arms exp).
Proof. intros fuel c e s arms exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))) c e s arms exp). Qed.
#[export] Hint Resolve okI_synth_match : okIdb.

Lemma okI_const_val : forall fuel g, okI (const_val fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))) g). Qed.
#[export] Hint Resolve okI_const_val : okIdb.

Lemma okI_synth_path : forall fuel c e p a, okI (synth_path fuel c e p a).
Proof. intros fuel c e p a. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))) c e p a). Qed.
#[export] Hint Resolve okI_synth_path : okIdb.

Lemma okI_check_args : forall fuel c e nm sg args, okI (check_args fuel c e nm sg args).
Proof. intros fuel c e nm sg args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))))) c e nm sg args). Qed.
#[export] Hint Resolve okI_check_args : okIdb.

Lemma okI_synth_call : forall fuel c e p a args, okI (synth_call fuel c e p a args).
Proof. intros fuel c e p a args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))) c e p a args). Qed.
#[export] Hint Resolve okI_synth_call : okIdb.

Lemma okI_synth_record : forall fuel c e p a fs, okI (synth_record fuel c e p a fs).
Proof. intros fuel c e p a fs. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))))))) c e p a fs). Qed.
#[export] Hint Resolve okI_synth_record : okIdb.

Lemma okI_synth_field : forall fuel c e o n, okI (synth_field fuel c e o n).
Proof. intros fuel c e o n. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))))) c e o n). Qed.
#[export] Hint Resolve okI_synth_field : okIdb.

Lemma okI_synth_method : forall fuel c e o p args, okI (synth_method fuel c e o p args).
Proof. intros fuel c e o p args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))))))))) c e o p args). Qed.
#[export] Hint Resolve okI_synth_method : okIdb.

Lemma okI_provided_call : forall fuel c e ox oty m args, okI (provided_call fuel c e ox oty m args).
Proof. intros fuel c e ox oty m args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))))))) c e ox oty m args). Qed.
#[export] Hint Resolve okI_provided_call : okIdb.

Lemma okI_apply_family : forall fuel c e fg th want, okI (apply_family fuel c e fg th want).
Proof. intros fuel c e fg th want. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))))))))))) c e fg th want). Qed.
#[export] Hint Resolve okI_apply_family : okIdb.

Lemma okI_demand_needs : forall fuel c e info, okI (demand_needs fuel c e info).
Proof. intros fuel c e info. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))))))))) c e info). Qed.
#[export] Hint Resolve okI_demand_needs : okIdb.

Lemma okI_synth_operator : forall fuel c e lm n op operand args, okI (synth_operator fuel c e lm n op operand args).
Proof. intros fuel c e lm n op operand args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))))))))))))) c e lm n op operand args). Qed.
#[export] Hint Resolve okI_synth_operator : okIdb.

Lemma okI_lang_call : forall fuel c e o n args why, okI (lang_call fuel c e o n args why).
Proof. intros fuel c e o n args why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))))))))))) c e o n args why). Qed.
#[export] Hint Resolve okI_lang_call : okIdb.

Lemma okI_int_literal : forall fuel c e ds why, okI (int_literal fuel c e ds why).
Proof. intros fuel c e ds why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))))))))))))))) c e ds why). Qed.
#[export] Hint Resolve okI_int_literal : okIdb.

Lemma okI_float_of : forall fuel c e v why, okI (float_of fuel c e v why).
Proof. intros fuel c e v why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))))))))))))) c e v why). Qed.
#[export] Hint Resolve okI_float_of : okIdb.

Lemma okI_synth_literal : forall fuel c e text, okI (synth_literal fuel c e text).
Proof. intros fuel c e text. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))))))))))))))))) c e text). Qed.
#[export] Hint Resolve okI_synth_literal : okIdb.

Lemma okI_check_bind : forall fuel c e sp y, okI (check_bind fuel c e sp y).
Proof. intros fuel c e sp y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))))))))))))))) c e sp y). Qed.
#[export] Hint Resolve okI_check_bind : okIdb.

Lemma okI_expect_fn_provider : forall fuel c e y, okI (expect_fn_provider fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))))))))))))))))))) c e y). Qed.
#[export] Hint Resolve okI_expect_fn_provider : okIdb.

Lemma okI_match_fn_sig : forall fuel c e pv wanted kind, okI (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel c e pv wanted kind. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))))))))))))))))) c e pv wanted kind). Qed.
#[export] Hint Resolve okI_match_fn_sig : okIdb.

Lemma okI_wrapper : forall fuel pv, okI (wrapper fuel pv).
Proof. intros fuel pv. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))))))))))))))))))))) pv). Qed.
#[export] Hint Resolve okI_wrapper : okIdb.

Lemma okI_forwarder : forall fuel tg, okI (forwarder fuel tg).
Proof. intros fuel tg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))))))))))))))))))) tg). Qed.
#[export] Hint Resolve okI_forwarder : okIdb.

Lemma okI_lower_body : forall fuel g, okI (lower_body fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))))))))))))))))))))))) g). Qed.
#[export] Hint Resolve okI_lower_body : okIdb.

Lemma okI_lambda_provider : forall fuel c e ps rt body bound sg this what, okI (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel c e ps rt body bound sg this what. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))))))))))))))))))))) c e ps rt body bound sg this what). Qed.
#[export] Hint Resolve okI_lambda_provider : okIdb.

Lemma okI_apply_functor : forall fuel c e sp, okI (apply_functor fuel c e sp).
Proof. intros fuel c e sp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))))))))))))))))))))))))) c e sp). Qed.
#[export] Hint Resolve okI_apply_functor : okIdb.

Lemma okI_apply_body : forall fuel c e g, okI (apply_body fuel c e g).
Proof. intros fuel c e g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))))))))))))))))))))))) c e g). Qed.
#[export] Hint Resolve okI_apply_body : okIdb.

Lemma okI_apply_abstract : forall fuel c e fg, okI (apply_abstract fuel c e fg).
Proof. intros fuel c e fg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel)))))))))))))))))))))))))))))))))) c e fg). Qed.
#[export] Hint Resolve okI_apply_abstract : okIdb.

Lemma okI_bind_functor : forall fuel c e fg y, okI (bind_functor fuel c e fg y).
Proof. intros fuel c e fg y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))))))))))))))))))))))))) c e fg y). Qed.
#[export] Hint Resolve okI_bind_functor : okIdb.

Lemma okI_link_body : forall fuel fg bg, okI (link_body fuel fg bg).
Proof. intros fuel fg bg. exact ((proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okI_synth_group fuel))))))))))))))))))))))))))))))))))) fg bg). Qed.
#[export] Hint Resolve okI_link_body : okIdb.

Lemma okI_entry_zero_width : forall fuel e t, okI (entry_zero_width fuel e t).
Proof. induction fuel; intros e t; cbn [entry_zero_width]; okIt. Qed.
#[export] Hint Resolve okI_entry_zero_width : okIdb.

Lemma okI_check_main_arity : forall g, okI (check_main_arity g).
Proof. intros g. unfold check_main_arity. okIt. Qed.
#[export] Hint Resolve okI_check_main_arity : okIdb.

Lemma okI_check_main_signature : forall g info, okI (check_main_signature g info).
Proof. intros g info. unfold check_main_signature. okIt. Qed.
#[export] Hint Resolve okI_check_main_signature : okIdb.


Lemma okI_runtime_provider_inputs : forall h, okI (runtime_provider_inputs h).
Proof. intro h. unfold runtime_provider_inputs. okIt. Qed.
#[export] Hint Resolve okI_runtime_provider_inputs : okIdb.

Lemma okI_ety_ty : forall t, okI (ety_ty t).
Proof. intro t;unfold ety_ty;apply okI_ni;[apply ok_ety_ty_go|apply ni_ety_ty_go|keepst]. Qed.
#[export] Hint Resolve okI_ety_ty : okIdb.

Lemma okI_check_platform_entry_view : forall e k entry,
    okI (check_platform_entry_view e k entry).
Proof.
  intros. apply okI_ni; [apply ok_check_platform_entry_view|unfold check_platform_entry_view;nit|unfold check_platform_entry_view;keepst].
Qed.
#[export] Hint Resolve okI_check_platform_entry_view : okIdb.

(* Name the phase proofs once. Repeatedly asking okIt to discover the whole
   program proof speculates about read-only phases and reconstructs these
   proofs along several failing paths. The final proof follows each bind. *)
Lemma okI_check_listed : okI check_listed.
Proof. unfold check_listed. okIt. Qed.
Lemma okI_check_contexts : okI check_contexts.
Proof. unfold check_contexts. okIt. Qed.
Lemma okI_check_runtime_library : okI check_runtime_library.
Proof. unfold check_runtime_library. okIt. Qed.
Lemma okI_lower_all : okI lower_all.
Proof. unfold lower_all. okIt. Qed.
Lemma okI_check_assumed_requirements : forall mi e k, okI (check_assumed_requirements mi e k).
Proof. intros. unfold check_assumed_requirements. okIt. Qed.
#[local] Hint Resolve okI_check_assumed_requirements : okIdb.

Lemma okI_check_assumes : okI check_assumes.
Proof. unfold check_assumes. okIt. Qed.
Lemma okI_platform_entry : forall k prims, okI (platform_entry k prims).
Proof. intros. unfold platform_entry. okIt. Qed.
Lemma okI_runtime_symbols : okI runtime_symbols.
Proof. unfold runtime_symbols. okIt. Qed.

Lemma okI_elab_program : okI elab_program.
Proof.
  unfold elab_program.
  apply okI_bind; [exact okI_check_listed | intros _].
  apply okI_bind; [exact okI_check_contexts | intros _].
  apply okI_bind; [exact okI_check_runtime_library | intros _].
  apply okI_bind; [exact okI_lower_all | intros _].
  apply okI_bind; [exact okI_check_assumes | intros _].
  apply okI_bind; [exact okI_get | intro s].
  destruct (sassoc "main" (gm_names (get_mod (st_w s) (w_entry (st_w s))))) as [msym|];
    [| apply okI_fail].
  apply okI_bind; [apply okI_sym | intro mg].
  destruct (negb (is_defined_fn mg)); [apply okI_fail |].
  apply okI_bind; [apply okI_fn_info | intro info].
  apply okI_bind; [apply okI_check_main_signature | intros _].
  apply okI_bind.
  - apply okI_mfold. intros [env0 prims] k.
    apply okI_bind; [apply okI_platform_entry | intros [[en prims']|]].
    + apply okI_bind; [apply okI_check_platform_entry_view | intros _].
      apply okI_bind; [unfold key_item_at; okIt | intro it].
      apply okI_ret.
    + apply okI_bind; [unfold show_key; okIt | intro sk].
      apply okI_fail.
  - intros [env0 prims].
    apply okI_bind; [exact okI_get | intro s1].
    apply okI_bind.
    + destruct (st_memory_bound s1 && st_memory_used s1); [apply okI_fail | apply okI_ret].
    + intros _. apply okI_bind.
      * destruct (st_unsup s1); [apply okI_ret | apply okI_fail].
      * intros _. apply okI_bind; [exact okI_runtime_symbols | intro sc].
        destruct (runtime_bool_tuple sc) as [[b t] fl].
        apply okI_bind.
        -- apply okI_mmap. intro p.
           apply okI_bind; [apply okI_runtime_provider_inputs | intro ts].
           apply okI_ret.
        -- intro input_types. apply okI_bind; [exact okI_get | intro s2].
           apply okI_bind.
           ++ apply okI_mmap. intro q.
              apply okI_bind; [apply okI_ety_ty | intro tau]. apply okI_ret.
           ++ intro main_types. apply okI_ret.
Qed.

Lemma info_init : forall w, InfoInv (s_init w).
Proof. intro w;constructor;intros;discriminate. Qed.

(* an accepted bundle's run keeps the cache's invariant to its end *)
Theorem elab_info : forall w p s, elab_program (s_init w) = Good (p, s) -> Inv (s_init w) -> InfoInv s.
Proof. intros w p s H Hi. exact (proj2 okI_elab_program _ _ _ Hi (info_init w) H). Qed.

(* ======================================================================== *)
(* 4. fn_info, cached or not, forms the function's context                  *)
(* ======================================================================== *)
