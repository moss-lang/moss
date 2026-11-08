(* ------------------------------------------------------------------------ *)
(* ElabMono.v — more fuel never changes what the elaborator returns (Track *)
(* A).  BodySound.v relates `synth (S f)` given the judgments at fuel f;   *)
(* a construct elaborated by a helper (synth_field f, synth_match f, …)    *)
(* calls synth at f - 1, and `fuel_mono` lifts that call to fuel f.        *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound.
Import ListNotations.

Local Strategy opaque [Nat.of_num_uint].

(* m2 returns whatever m1 returns *)
Definition le_m {A} (m1 m2 : M A) : Prop := forall s r, m1 s = Good r -> m2 s = Good r.

Arguments le_m : simpl never.

Lemma le_refl : forall A (m : M A), le_m m m.
Proof. intros A m s r H. exact H. Qed.

Lemma le_bind : forall A B (m1 m2 : M A) (k1 k2 : A -> M B),
  le_m m1 m2 -> (forall a, le_m (k1 a) (k2 a)) -> le_m (mbind m1 k1) (mbind m2 k2).
Proof.
  intros A B m1 m2 k1 k2 Hm Hk s [b s''] H. apply bind_inv in H as (a & s' & H1 & H2).
  unfold mbind. rewrite (Hm _ _ H1). exact (Hk a _ _ H2).
Qed.

Lemma le_mmap : forall A B (f1 f2 : A -> M B) l, (forall x, le_m (f1 x) (f2 x)) -> le_m (mmap f1 l) (mmap f2 l).
Proof.
  intros A B f1 f2 l Hf. induction l as [|x r IH]; cbn [mmap]; [apply le_refl|].
  apply le_bind; [apply Hf | intro y; apply le_bind; [exact IH | intro; apply le_refl]].
Qed.

Lemma le_mfold : forall A B (f1 f2 : A -> B -> M A) l a, (forall a x, le_m (f1 a x) (f2 a x)) ->
  le_m (mfold f1 a l) (mfold f2 a l).
Proof.
  intros A B f1 f2 l. induction l as [|x r IH]; intros a Hf; cbn [mfold]; [apply le_refl|].
  apply le_bind; [apply Hf | intro a'; apply IH; exact Hf].
Qed.

Lemma le_miter : forall A (f1 f2 : A -> M unit) l, (forall x, le_m (f1 x) (f2 x)) -> le_m (miter f1 l) (miter f2 l).
Proof.
  intros A f1 f2 l Hf. induction l as [|x r IH]; cbn [miter]; [apply le_refl|].
  apply le_bind; [apply Hf | intro; exact IH].
Qed.

Lemma le_with_vars : forall A init (m1 m2 : M A), le_m m1 m2 -> le_m (with_vars init m1) (with_vars init m2).
Proof.
  intros A init m1 m2 H. unfold with_vars. apply le_bind; [apply le_refl | intro s0].
  apply le_bind; [apply le_refl | intro u]. apply le_bind; [exact H | intro a]. apply le_refl.
Qed.

Lemma le_deep : forall A (m : M A), le_m deep m.
Proof. intros A m s r H. discriminate H. Qed.

(* the right side, up to conversion: cbn unfolds a call at fuel+1 on the
   right that the induction hypothesis states folded *)
Lemma le_conv : forall A (m1 m2 m3 : M A), le_m m1 m2 -> m2 = m3 -> le_m m1 m3.
Proof. intros A m1 m2 m3 H <-. exact H. Qed.

Ltac le_head t := lazymatch t with ?f _ => le_head f | _ => t end.
Ltac lintros := repeat (lazymatch goal with |- forall _, _ => intro end).

Ltac lfix1 :=
  lazymatch goal with |- le_m (?F ?N) (?G ?N) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N as n; intro n; induction n as [|n IHn]; cbn beta iota end.
Ltac lfix3 :=
  lazymatch goal with |- le_m (?F ?N ?a ?b) (?G ?N ?a ?b) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N, a, b; intro n; induction n as [|n IHn]; intros; cbn beta iota end.

Ltac mt_with previous :=
  repeat (lintros; first [solve [previous ()]|lazymatch goal with
  | |- le_m ?x ?x => apply le_refl
  | |- le_m deep _ => apply le_deep
  | |- le_m (mbind _ _) (mbind _ _) => apply le_bind
  | |- le_m (mmap _ _) (mmap _ _) => apply le_mmap
  | |- le_m (mfold _ _ _) (mfold _ _ _) => apply le_mfold
  | |- le_m (miter _ _) (miter _ _) => apply le_miter
  | |- le_m (with_vars _ _) (with_vars _ _) => apply le_with_vars
  | |- le_m ?t ?u =>
      first [ match goal with Hm : _ |- _ => solve [apply Hm] end
            | (lazymatch t with ?F _ => is_fix F end; lfix1)
            | (lazymatch t with ?F _ _ _ => is_fix F end; lfix3)
            | match t with context [match ?x with _ => _ end] => destruct x end
            | let h := le_head t in progress unfold h ]
  end]).
Ltac mt := mt_with ltac:(fun _ => fail).

Definition Grp (f1 f2 : nat) : Prop :=
  (forall c e x exp, le_m (synth f1 c e x exp) (synth f2 c e x exp)) /\
  (forall c e a exp, le_m (synth_arg f1 c e a exp) (synth_arg f2 c e a exp)) /\
  (forall c e b exp, le_m (synth_block f1 c e b exp) (synth_block f2 c e b exp)) /\
  (forall c e ss, le_m (check_stmts f1 c e ss) (check_stmts f2 c e ss)) /\
  (forall c e s, le_m (check_stmt f1 c e s) (check_stmt f2 c e s)) /\
  (forall c e y, le_m (check_bool f1 c e y) (check_bool f2 c e y)) /\
  (forall c e x exp, le_m (synth_if f1 c e x exp) (synth_if f2 c e x exp)) /\
  (forall c e s arms exp, le_m (synth_match f1 c e s arms exp) (synth_match f2 c e s arms exp)) /\
  (forall g, le_m (const_val f1 g) (const_val f2 g)) /\
  (forall c e p a, le_m (synth_path f1 c e p a) (synth_path f2 c e p a)) /\
  (forall c e nm sg args, le_m (check_args f1 c e nm sg args) (check_args f2 c e nm sg args)) /\
  (forall c e p a args, le_m (synth_call f1 c e p a args) (synth_call f2 c e p a args)) /\
  (forall c e p a fs, le_m (synth_record f1 c e p a fs) (synth_record f2 c e p a fs)) /\
  (forall c e o n, le_m (synth_field f1 c e o n) (synth_field f2 c e o n)) /\
  (forall c e o p args, le_m (synth_method f1 c e o p args) (synth_method f2 c e o p args)) /\
  (forall c e ox oty m args, le_m (provided_call f1 c e ox oty m args) (provided_call f2 c e ox oty m args)) /\
  (forall c e fg th want, le_m (apply_family f1 c e fg th want) (apply_family f2 c e fg th want)) /\
  (forall c e info, le_m (demand_needs f1 c e info) (demand_needs f2 c e info)) /\
  (forall c e lm n op operand args, le_m (synth_operator f1 c e lm n op operand args) (synth_operator f2 c e lm n op operand args)) /\
  (forall c e o n args why, le_m (lang_call f1 c e o n args why) (lang_call f2 c e o n args why)) /\
  (forall c e ds why, le_m (int_literal f1 c e ds why) (int_literal f2 c e ds why)) /\
  (forall c e v why, le_m (float_of f1 c e v why) (float_of f2 c e v why)) /\
  (forall c e text, le_m (synth_literal f1 c e text) (synth_literal f2 c e text)) /\
  (forall c e sp y, le_m (check_bind f1 c e sp y) (check_bind f2 c e sp y)) /\
  (forall c e y, le_m (expect_fn_provider f1 c e y) (expect_fn_provider f2 c e y)) /\
  (forall c e pv wanted kind, le_m (match_fn_sig f1 c e pv wanted kind) (match_fn_sig f2 c e pv wanted kind)) /\
  (forall pv, le_m (wrapper f1 pv) (wrapper f2 pv)) /\
  (forall tg, le_m (forwarder f1 tg) (forwarder f2 tg)) /\
  (forall g, le_m (lower_body f1 g) (lower_body f2 g)) /\
  (forall c e ps rt body bound sg this what, le_m (lambda_provider f1 c e ps rt body bound sg this what) (lambda_provider f2 c e ps rt body bound sg this what)) /\
  (forall c e sp, le_m (apply_functor f1 c e sp) (apply_functor f2 c e sp)) /\
  (forall c e g, le_m (apply_body f1 c e g) (apply_body f2 c e g)) /\
  (forall c e fg, le_m (apply_abstract f1 c e fg) (apply_abstract f2 c e fg)) /\
  (forall c e fg y, le_m (bind_functor f1 c e fg y) (bind_functor f2 c e fg y)) /\
  (forall fg bg, le_m (link_body f1 fg bg) (link_body f2 fg bg)).

(* one step: the bodies at f+1 and g+1 call the group at f and g *)
Lemma mono_step : forall f1 f2, Grp f1 f2 -> Grp (S f1) (S f2).
Proof.
  intros f1 f2 IH. destruct IH as (I0 & I1 & I2 & I3 & I4 & I5 & I6 & I7 & I8 & I9 & I10 & I11 & I12 & I13 & I14 & I15 & I16 & I17 & I18 & I19 & I20 & I21 & I22 & I23 & I24 & I25 & I26 & I27 & I28 & I29 & I30 & I31 & I32 & I33 & I34).
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (_))))))))))))))))))))))))))))))))))); lintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body];
  (* Select recursive calls directly from the previous-fuel hypotheses. *)
  mt_with ltac:(fun _ =>
    lazymatch goal with |- le_m ?left ?right =>
      let h := le_head left in lazymatch h with
      | synth => apply I0
      | synth_arg => apply I1
      | synth_block => apply I2
      | check_stmts => apply I3
      | check_stmt => apply I4
      | check_bool => apply I5
      | synth_if => apply I6
      | synth_match => apply I7
      | const_val => apply I8
      | synth_path => apply I9
      | check_args => apply I10
      | synth_call => apply I11
      | synth_record => apply I12
      | synth_field => apply I13
      | synth_method => apply I14
      | provided_call => apply I15
      | apply_family => apply I16
      | demand_needs => apply I17
      | synth_operator => apply I18
      | lang_call => apply I19
      | int_literal => apply I20
      | float_of => apply I21
      | synth_literal => apply I22
      | check_bind => apply I23
      | expect_fn_provider => apply I24
      | match_fn_sig => apply I25
      | wrapper => apply I26
      | forwarder => apply I27
      | lower_body => apply I28
      | lambda_provider => apply I29
      | apply_functor => apply I30
      | apply_body => apply I31
      | apply_abstract => apply I32
      | bind_functor => apply I33
      | link_body => apply I34
      end
    end).
Qed.

Lemma mono_group : forall fuel, Grp fuel (S fuel).
Proof.
  induction fuel as [|fuel IH]; [|exact (mono_step _ _ IH)].
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (_))))))))))))))))))))))))))))))))))); lintros; let H := fresh "H" in intros ? ? H; discriminate H.
Qed.
