(* Information-table stability of type/context helpers, independent of the synthesis traversal. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabKeeps.
Import ListNotations.
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
  repeat (keeps_intros; lazymatch goal with
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
            | let h := keeps_head t in progress unfold h ]
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
  { keeps_intros; cbn [mentioned_nominals]; nit. }
  pose proof IH as I0. keeps_intros; cbn [mentioned_nominals]; nit.
Qed.

Lemma ni_mentioned_nominals : forall fuel t mi, NI (mentioned_nominals fuel t mi).
Proof. intros fuel t mi. exact ((ni_mentioned_nominals_group fuel) t mi). Qed.
#[export] Hint Resolve ni_mentioned_nominals : nidb.

Lemma ni_walk_specs_group : forall fuel,
  (forall mi sp out, NI (walk_specs fuel mi sp out)).
Proof.
  induction fuel as [|fuel IH].
  { keeps_intros; cbn [walk_specs]; nit. }
  pose proof IH as I0. keeps_intros; cbn [walk_specs]; nit.
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
  { refine (conj _ (conj _ (conj _ _))); keeps_intros; cbn [free_type_syms type_requirements solve_requirements listed_params]; nit. }
  destruct IH as (I0 & I1 & I2 & I3).
  refine (conj _ (conj _ (conj _ _))); keeps_intros; cbn [free_type_syms type_requirements solve_requirements listed_params]; nit.
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
  { keeps_intros; cbn [show_go]; nit. }
  pose proof IH as I0. keeps_intros; cbn [show_go]; nit.
Qed.

Lemma ni_show_go : forall fuel t, NI (show_go fuel t).
Proof. intros fuel t. exact ((ni_show_go_group fuel) t). Qed.
#[export] Hint Resolve ni_show_go : nidb.

Lemma ni_unify_group : forall fuel,
  (forall e mi a b why, NI (unify fuel e mi a b why)).
Proof.
  induction fuel as [|fuel IH].
  { keeps_intros; cbn [unify]; nit. }
  pose proof IH as I0. keeps_intros; cbn [unify]; nit.
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
  { refine (conj _ (conj _ _)); keeps_intros; cbn [elab_type app_subst symbol_type]; nit. }
  destruct IH as (I0 & I1 & I2).
  refine (conj _ (conj _ _)); keeps_intros; cbn [elab_type app_subst symbol_type]; nit.
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
  { refine (conj _ _); keeps_intros; cbn [result_mentions_go functor_fixed_go]; nit. }
  destruct IH as (I0 & I1).
  refine (conj _ _); keeps_intros; cbn [result_mentions_go functor_fixed_go]; nit.
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
  { refine (conj _ _); keeps_intros; cbn [add_symbol_item add_spec]; nit. }
  destruct IH as (I0 & I1).
  refine (conj _ _); keeps_intros; cbn [add_symbol_item add_spec]; nit.
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
  { keeps_intros; cbn [ety_ty_go]; nit. }
  pose proof IH as I0. keeps_intros; cbn [ety_ty_go]; nit.
Qed.

Lemma ni_ety_ty_go : forall fuel t, NI (ety_ty_go fuel t).
Proof. intros fuel t. exact ((ni_ety_ty_go_group fuel) t). Qed.
#[export] Hint Resolve ni_ety_ty_go : nidb.

Lemma ni_slots_go_group : forall fuel,
  (forall e t visiting, NI (slots_go fuel e t visiting)).
Proof.
  induction fuel as [|fuel IH].
  { keeps_intros; cbn [slots_go]; nit. }
  pose proof IH as I0. keeps_intros; cbn [slots_go]; nit.
Qed.

Lemma ni_slots_go : forall fuel e t visiting, NI (slots_go fuel e t visiting).
Proof. intros fuel e t visiting. exact ((ni_slots_go_group fuel) e t visiting). Qed.
#[export] Hint Resolve ni_slots_go : nidb.

Lemma ni_expand_spec_group : forall fuel,
  (forall mi sp out, NI (expand_spec fuel mi sp out)).
Proof.
  induction fuel as [|fuel IH].
  { keeps_intros; cbn [expand_spec]; nit. }
  pose proof IH as I0. keeps_intros; cbn [expand_spec]; nit.
Qed.

Lemma ni_expand_spec : forall fuel mi sp out, NI (expand_spec fuel mi sp out).
Proof. intros fuel mi sp out. exact ((ni_expand_spec_group fuel) mi sp out). Qed.
#[export] Hint Resolve ni_expand_spec : nidb.

Lemma ni_functor_total_group : forall fuel,
  (forall g, NI (functor_total fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { keeps_intros; cbn [functor_total]; nit. }
  pose proof IH as I0. keeps_intros; cbn [functor_total]; nit.
Qed.

Lemma ni_functor_total : forall fuel g, NI (functor_total fuel g).
Proof. intros fuel g. exact ((ni_functor_total_group fuel) g). Qed.
#[export] Hint Resolve ni_functor_total : nidb.

Lemma ni_provide_spec_group : forall fuel,
  (forall e fg mi sp outer out, NI (provide_spec fuel e fg mi sp outer out)).
Proof.
  induction fuel as [|fuel IH].
  { keeps_intros; cbn [provide_spec]; nit. }
  pose proof IH as I0. keeps_intros; cbn [provide_spec]; nit.
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
  { keeps_intros; cbn [family_leaves_ok]; nit. }
  pose proof IH as I0. keeps_intros; cbn [family_leaves_ok]; nit.
Qed.

Lemma ni_family_leaves_ok : forall fuel mi ps specs, NI (family_leaves_ok fuel mi ps specs).
Proof. intros fuel mi ps specs. exact ((ni_family_leaves_ok_group fuel) mi ps specs). Qed.
#[export] Hint Resolve ni_family_leaves_ok : nidb.

Lemma ni_family_leaves_group : forall fuel,
  (forall e mi specs outer, NI (family_leaves fuel e mi specs outer)).
Proof.
  induction fuel as [|fuel IH].
  { keeps_intros; cbn [family_leaves]; nit. }
  pose proof IH as I0. keeps_intros; cbn [family_leaves]; nit.
Qed.

Lemma ni_family_leaves : forall fuel e mi specs outer, NI (family_leaves fuel e mi specs outer).
Proof. intros fuel e mi specs outer. exact ((ni_family_leaves_group fuel) e mi specs outer). Qed.
#[export] Hint Resolve ni_family_leaves : nidb.

