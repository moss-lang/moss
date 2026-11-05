(* ------------------------------------------------------------------------ *)
(* InfoCache.v — fn_info's cache (st_info) against CtxOf (Track A).         *)
(*                                                                           *)
(* Every entry the cache holds was computed by fn_info: the merged walk of *)
(* the symbol table's entry for its key, run from a state with Inv, whose  *)
(* end state is framed (Fr) by the current one (InfoInv).  Every step of   *)
(* the elaborator keeps InfoInv (okI): only fn_info writes the cache, and  *)
(* no function before it in Elab.v touches it (NI).  So fn_info, whether   *)
(* its answer is cached or not, forms the function's context              *)
(* (fn_info_ctx_any).                                                      *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsStable ElabFrame ReqsTotal BodySound BodyTy Merge CtxWalk CtxMerge.
Import ListNotations.
Local Open Scope list_scope.

Module A := Audit.Elaboration.

Local Strategy opaque [Nat.of_num_uint FUEL].

(* ======================================================================== *)
(* 1. Steps that leave the cache alone                                       *)
(* ======================================================================== *)

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
  try discriminate; injection H; intros; subst; reflexivity.

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
Record FrL (s s' : st) : Prop := {
  fl_w : st_w s' = st_w s;
  fl_flds : forall f n, sassoc f (st_flds s) = Some n -> sassoc f (st_flds s') = Some n;
  fl_unsup : exists l, st_unsup s' = st_unsup s ++ l;
  fl_lang : st_lang s' = st_lang s;
  fl_reqs : Ext (st_reqs s) (st_reqs s');
  fl_keep : forall i g, nget (st_syms s) i = Some g -> nget (st_syms s') i = Some g
}.

Lemma frl_of_fr : forall s s', Fr s s' -> FrL s s'.
Proof.
  intros s s' [[Fw _ Fk _ _ Ffl [_ _ _ _ Fu Fl]] _ Fx]. constructor; assumption.
Qed.

Lemma frl_trans : forall s1 s2 s3, FrL s1 s2 -> FrL s2 s3 -> FrL s1 s3.
Proof.
  intros s1 s2 s3 [W1 F1 (u1 & U1) L1 X1 K1] [W2 F2 (u2 & U2) L2 X2 K2]. constructor.
  - congruence.
  - auto.
  - exists (u1 ++ u2). rewrite U2, U1, app_assoc. reflexivity.
  - congruence.
  - exact (ext_trans _ _ _ X1 X2).
  - auto.
Qed.

Lemma later_fromL : forall w R Fl s s', FrL s s' -> Later w R Fl s' -> Later w R Fl s.
Proof.
  intros w R Fl s s' [Fw Ffl (u & Eu) Fla Fx _] [Lw Lf Lu Ll Lr]. constructor.
  - congruence.
  - intros a n H. exact (Lf a n (Ffl a n H)).
  - rewrite Lu in Eu. destruct (st_unsup s); [reflexivity | discriminate].
  - congruence.
  - intros x l H. exact (Lr x l (Fx x l H)).
Qed.

(* x is no symbol of the loader's (a symbol the elaborator made) *)
Definition NL (s : st) (x : nat) : Prop :=
  forall b, load_world b = Good (st_w s) -> nget (w_syms (st_w s)) x = None.

(* x is a symbol the elaborator made that is not a forwarder (a lambda
   provider's) *)
Definition NLx (s : st) (x : nat) : Prop :=
  NL s x /\ exists g, nget (st_syms s) x = Some g /\ forall t, gs_origin g <> OForwarder t.

Lemma nlx_frl : forall s s' x, NLx s x -> FrL s s' -> NLx s' x.
Proof.
  intros s s' x [N (g & G & O)] F. split; [unfold NL; rewrite (fl_w _ _ F); exact N|].
  exists g. split; [exact (fl_keep _ _ F _ _ G) | exact O].
Qed.

(* an entry of the cache: fn_info's merged walk of the symbol table's entry,
   or (lambda_provider's) an entry for a symbol the elaborator made, not a
   forwarder *)
Definition InfoEntry (s : st) (x : nat) (i : fninfo) : Prop :=
  (exists g s0 e needs s1, nget (st_syms s0) x = Some g /\ Inv s0 /\
    merged_walk (gs_mod g) (fi_walk g) s0 = Good ((e, needs), s1) /\
    i = FInfo (freeze_all_apps e) needs
              (map (fun k => (k, match get_app (freeze_all_apps e) k with Some (a, _) => a | None => [] end)) needs)
              (merge_edges e) /\
    FrL s1 s) \/ NLx s x.

Definition InfoInv (s : st) : Prop := forall x i, nget (st_info s) x = Some i -> InfoEntry s x i.

Lemma info_step : forall s s', InfoInv s -> FrL s s' -> st_info s' = st_info s -> InfoInv s'.
Proof.
  intros s s' I F E x i H. rewrite E in H. destruct (I x i H) as [(g & s0 & e & needs & s1 & H1 & H2 & H3 & H4 & H5)|N].
  - left. exists g, s0, e, needs, s1. repeat (split; [assumption|]). exact (frl_trans _ _ _ H5 F).
  - right. exact (nlx_frl _ _ _ N F).
Qed.

Definition okI {A} (m : M A) : Prop :=
  ok m /\ forall s a s', Inv s -> InfoInv s -> m s = Good (a, s') -> InfoInv s'.
Arguments okI : simpl never.

Lemma okI_ni : forall A (m : M A), ok m -> NI m -> okI m.
Proof.
  intros A m Ho Hn. split; [exact Ho|]. intros s a s' Hi I H.
  destruct (Ho s a s' Hi H) as [_ F]. exact (info_step s s' I (frl_of_fr _ _ F) (Hn s a s' H)).
Qed.

Lemma okI_ret : forall A (a : A), okI (ret a).
Proof. intros. apply okI_ni; [apply ok_ret | apply ni_ret]. Qed.

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
    by (apply (info_step s); [exact I | constructor; cbn; auto; [exists []; rewrite app_nil_r; reflexivity | apply ext_refl] | reflexivity]).
  pose proof (Pm _ _ _ Hi2 I2 H3) as I3.
  apply (info_step s3); [exact I3 | constructor; cbn; auto; [exists []; rewrite app_nil_r; reflexivity | apply ext_refl] | reflexivity].
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

Lemma okI_fn_info : forall g0, okI (fn_info g0).
Proof.
  intro g0. assert (Ok : ok (fn_info g0)) by (unfold fn_info; okt). split; [exact Ok|].
  intros s a s' Hi I H. pose proof H as H0. unfold fn_info in H.
  apply bind_inv in H as (x & s2 & E & H). injection E as <- <-.
  destruct (nget (st_info s) (gs_id g0)) as [i|] eqn:Ei.
  { injection H as <- <-. exact I. }
  destruct (fn_info_miss g0 s a s' H0 Ei) as (g & e & needs & s1 & Eg & H1 & Ea & Es).
  destruct (ok_merged_fi g s _ s1 Hi H1) as [Hi1 F1].
  pose proof (ni_merged_fi g s _ s1 H1) as N1.
  assert (I1 : InfoInv s1) by exact (info_step s s1 I (frl_of_fr _ _ F1) N1).
  assert (F2 : FrL s1 s') by (rewrite Es; constructor; cbn; auto; [exists []; rewrite app_nil_r; reflexivity | apply ext_refl]).
  intros y j Hy. rewrite Es in Hy. cbn [st_info] in Hy. rewrite nget_nset in Hy.
  destruct (Nat.eqb_spec y (gs_id g0)) as [->|Ne].
  - injection Hy as <-. left. exists g, s, e, needs, s1. auto 6.
  - destruct (I1 y j Hy) as [(g' & s0 & e' & n' & s3 & P1 & P2 & P3 & P4 & P5)|Nl].
    + left. exists g', s0, e', n', s3. repeat (split; [assumption|]). exact (frl_trans _ _ _ P5 F2).
    + right. exact (nlx_frl _ _ _ Nl F2).
Qed.

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
    by (apply (info_step s); [exact I | constructor; cbn; auto; [exists []; rewrite app_nil_r; reflexivity | apply ext_refl] | reflexivity]).
  pose proof (Pm _ _ _ Hi2 I2 N H3) as I3.
  apply (info_step s3); [exact I3 | constructor; cbn; auto; [exists []; rewrite app_nil_r; reflexivity | apply ext_refl] | reflexivity].
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
  intros y j Hy. cbn [s_info st_info] in Hy. rewrite nget_nset in Hy. destruct (Nat.eqb_spec y pid) as [->|Ne].
  - right. exact N.
  - exact (entry_frl _ _ _ _ (I y j Hy) (frl_info s _)).
Qed.

Lemma okI_new_sym_bind : forall A mi n k d o (K : nat -> M A), o <> OSource -> (forall t, o <> OForwarder t) ->
  (forall pid, okIF pid (K pid)) -> okI (mbind (new_sym mi n k d o) K).
Proof.
  intros A mi n k d o K Ho Hf HK. split; [apply ok_bind; [apply ok_new_sym; exact Ho | intro pid; exact (proj1 (HK pid))]|].
  intros s a s'' Hi I H. apply bind_inv in H as (pid & s1 & H1 & H2).
  destruct (ok_new_sym mi n k d o Ho s pid s1 Hi H1) as [Hi1 F1].
  assert (N1 : NI (new_sym mi n k d o)) by (unfold new_sym; nit).
  pose proof (info_step s s1 I (frl_of_fr _ _ F1) (N1 _ _ _ H1)) as I1.
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
Lemma okI_new_sym_fwd : forall A mi n k d t (K : nat -> M A),
  (forall pid, okI (K pid)) -> okI (mbind (new_sym mi n k d (OForwarder t)) K).
Proof.
  intros A mi n k d t K HK. apply okI_bind; [|exact HK].
  apply okI_ni; [apply ok_new_sym; discriminate | unfold new_sym; nit].
Qed.


(* ======================================================================== *)
(* 3. Every function of the elaborator keeps the cache's invariant           *)
(* ======================================================================== *)

Create HintDb okIdb.
#[export] Hint Resolve okI_ret okI_fn_info : okIdb.
#[export] Hint Extern 5 (okI _) => (apply okI_ni; [solve [auto with okdb] | solve [nit]]) : okIdb.

Ltac okIfixind1 :=
  lazymatch goal with |- okI (?F ?N) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N as n; intro n; induction n as [|n IHn]; cbn beta iota end.
Ltac okIfixind3 :=
  lazymatch goal with |- okI (?F ?N ?a ?b) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N, a, b; intro n; induction n as [|n IHn]; intros; cbn beta iota end.

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
  | |- okI (mbind (new_sym _ _ _ _ (OForwarder _)) _) => apply okI_new_sym_fwd; intro
  | |- okI (mbind (new_sym _ _ _ _ _) _) => apply okI_new_sym_bind; [discriminate | intros ? ?; discriminate | intro; okIFt]
  | |- okI (mbind _ _) => apply okI_bind
  | |- okI (mmap _ _) => apply okI_mmap
  | |- okI (mfold _ _ _) => apply okI_mfold
  | |- okI (miter _ _) => apply okI_miter
  | |- okI (with_vars _ _) => apply okI_with_vars
  | |- okI (upd _) => apply okI_ni; [okt | nit]
  | |- okI (fun s => _) => apply okI_ni; [okt | nit]
  | |- okI ?t =>
      first [ solve [auto with okIdb]
            | (lazymatch t with ?F _ => is_fix F end; okIfixind1)
            | (lazymatch t with ?F _ _ _ => is_fix F end; okIfixind3)
            | match t with context [match ?x with _ => _ end] => destruct x end
            | let h := head_of t in progress unfold h ]
  end).

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

Lemma okI_elab_program : okI elab_program.
Proof. unfold elab_program. okIt. Qed.

Lemma info_init : forall w, InfoInv (s_init w).
Proof. intros w x i H. discriminate H. Qed.

(* an accepted bundle's run keeps the cache's invariant to its end *)
Theorem elab_info : forall w p s, elab_program (s_init w) = Good (p, s) -> Inv (s_init w) -> InfoInv s.
Proof. intros w p s H Hi. exact (proj2 okI_elab_program _ _ _ Hi (info_init w) H). Qed.

(* ======================================================================== *)
(* 4. fn_info, cached or not, forms the function's context                  *)
(* ======================================================================== *)

Section Use.
Variables (w : Elab.world) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Hypothesis Finj : forall a c, Fl a = Fl c -> a = c.
Hypothesis MS : MethStep w R Fl.
Notation WW := (BodySound.W w R Fl).
Notation trF := (tr Fl).

(* what fn_info_ctx concludes of an info i of g *)
Definition FnCtx (g : gsym) (i : fninfo) : Prop :=
  let e := fi_env i in
  exists C, CtxOfIE WW (gs_id g) C (map (key_tr Fl) (fi_needs i)) /\
    A.c_sub C = suf Fl (en_uf e) /\ fi_merges i = en_uf e /\
    (forall k, A.nlook k (A.c_ty C) = option_map (fun y => trF (canon e y)) (tn_get e k)) /\
    (forall v, A.nlook v (A.c_vals C) = option_map (fun y => trF (canon e y)) (nget (en_vals e) v)) /\
    (forall f, A.nlook f (A.c_fns C) = option_map (csig Fl e) (nget (en_fns e) f)) /\
    (forall k m, ety_ok k = true -> canon e k = k -> A.mlook (trF k) m (A.c_mths C) = option_map (csig Fl e) (get_mth e k m)) /\
    (forall u, A.nlook u (A.c_funs C) = option_map (fun _ => []) (nget (en_funs e) u)) /\
    (forall k, nofun k = true -> key_ok k = true ->
       A.ilook (key_tr Fl k) (A.c_apps C) = option_map (fun p => tr_inst Fl (fst p)) (get_app e k)).

Lemma walk_ctx : forall g s e needs s1,
  nget (w_syms w) (gs_id g) = Some g -> (forall t, gs_origin g <> OForwarder t) ->
  ~ A.IsMainFn WW (gs_id g) -> (forall s0 m s2, st_w s0 = w -> is_main g s0 = Good (m, s2) -> m = false) ->
  merged_walk (gs_mod g) (fi_walk g) s = Good ((e, needs), s1) -> Inv s -> st_w s = w -> Later w R Fl s1 ->
  FnCtx g (FInfo (freeze_all_apps e) needs
              (map (fun k => (k, match get_app (freeze_all_apps e) k with Some (a, _) => a | None => [] end)) needs)
              (merge_edges e)).
Proof.
  intros g s e needs s1 Hg Nf Nm Nm' H1 Hi Hw L.
  pose proof (loader_mod_in w R Fl b Hload _ _ Hg) as Hmi.
  destruct (assumes_merged w R Fl b Hload Finj MS g s e needs s1 Nf Nm' Hmi H1 Hi Hw L)
    as (les & tm & I & Hws & Mg & Nd).
  set (U := en_uf e) in *.
  exists (A.entries_cx (suf Fl U) tm (map (tr_ent Fl) les)). cbn [fi_env fi_needs fi_merges].
  split; [|split; [reflexivity|split]].
  - exists (A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g)), (map (tr_ent Fl) les), tm, [], tm, (suf Fl U).
    rewrite !app_nil_r. split; [exact (w_decl_rd w R Fl _ _ Hg)|]. split; [exact I|]. split; [|auto].
    apply A.RootNone. left. exact Nm.
  - unfold merge_edges. rewrite (tne_nil _ _ _ _ _ _ Hws). reflexivity.
  - split; [|split; [|split; [|split; [|split]]]].
    + intro k. exact (ctx_ty Fl U e tm les needs Hws eq_refl k).
    + intro v. exact (ctx_vals Fl U e tm les needs Hws eq_refl v).
    + intro f. exact (ctx_fns Fl U e tm les needs Hws eq_refl f).
    + intros k m Ok Ck. exact (ctx_mths Fl Finj U e tm les needs Hws eq_refl k m Ok Ck).
    + intro u. exact (ctx_funs Fl U e tm les needs Hws u).
    + intros k Nk Ok. exact (ctx_apps Fl Finj U e tm les needs Hws eq_refl k Nk Ok).
Qed.

(* fn_info of a function of the program, neither a forwarder nor `main`,
   from a state with the cache's invariant: its info is that function's
   context, whether it was cached or not *)
Theorem fn_info_ctx_any : forall g s i s',
  nget (w_syms w) (gs_id g) = Some g -> (forall t, gs_origin g <> OForwarder t) ->
  ~ A.IsMainFn WW (gs_id g) -> (forall s0 m s1, st_w s0 = w -> is_main g s0 = Good (m, s1) -> m = false) ->
  fn_info g s = Good (i, s') -> Inv s -> InfoInv s -> st_w s = w -> Later w R Fl s' ->
  FnCtx g i.
Proof.
  intros g s i s' Hg Nf Nm Nm' H Hi I Hw L.
  assert (Hb : load_world b = Good (st_w s)) by (rewrite Hw; exact Hload).
  assert (Gs : nget (st_syms s) (gs_id g) = Some g) by (apply (iv_loader _ Hi b Hb); rewrite Hw; exact Hg).
  destruct (nget (st_info s) (gs_id g)) as [j|] eqn:Ej.
  - (* cached *)
    pose proof H as H0. unfold fn_info in H0. apply bind_inv in H0 as (x & s2 & E & H0). injection E as <- <-.
    rewrite Ej in H0. injection H0 as <- <-.
    destruct (I _ _ Ej) as [(g' & s0 & e & needs & s1 & G0 & Hi0 & H1 & -> & F)|N].
    + destruct (ok_merged_fi g' s0 _ s1 Hi0 H1) as [_ F1].
      assert (W0 : st_w s0 = w) by (rewrite <- Hw, (fl_w _ _ F), (fr_w _ _ F1); reflexivity).
      assert (g' = g).
      { assert (Hb0 : load_world b = Good (st_w s0)) by (rewrite W0; exact Hload).
        pose proof (iv_loader _ Hi0 b Hb0 (gs_id g) g ltac:(rewrite W0; exact Hg)) as E0. congruence. }
      subst g'. exact (walk_ctx g s0 e needs s1 Hg Nf Nm Nm' H1 Hi0 W0 (later_fromL w R Fl _ _ F L)).
    + exfalso. pose proof (proj1 N b Hb) as N0. rewrite Hw in N0. congruence.
  - (* computed *)
    destruct (fn_info_miss g s i s' H Ej) as (g' & e & needs & s1 & Eg & H1 & -> & ->).
    rewrite Gs in Eg. injection Eg as <-.
    exact (walk_ctx g s e needs s1 Hg Nf Nm Nm' H1 Hi Hw (later_info w R Fl s1 _ L)).
Qed.

End Use.

(* ======================================================================== *)
(* 5. Forwarders: their needs                                                *)
(* ======================================================================== *)

(* The relation reads no CtxOf for a forwarder (FPForward: a Fresh symbol
   whose satisfaction map is [(IFn g, IFn g)]); what the elaborator owes is
   that a forwarder's needs are exactly its target's key.  Its walk assumes
   the target's requirements, type symbols (of a fn: its assume nesting's,
   WorldOf), which add no need, and then the target, a fn. *)

(* merged_walk answers with one of its rounds *)
Lemma mw_round : forall (walk : env -> M (env * list key)) (P : list key -> Prop) (Pre Post : st -> Prop) mi,
  (forall e, ok (walk e)) -> (forall s s', Fr s s' -> Pre s -> Pre s') -> (forall s s', Fr s s' -> Post s' -> Post s) ->
  (forall e0 s e needs s', Inv s -> Pre s -> Post s' -> walk e0 s = Good ((e, needs), s') -> P needs) ->
  forall n tne ufe s e needs s', mw_go walk mi n tne ufe s = Good ((e, needs), s') -> Inv s -> Pre s -> Post s' -> P needs.
Proof.
  intros walk P Pre Post mi Hok Pf Pb Hr n. induction n as [|n IH]; intros tne ufe s e needs s' H Hi Hp Hq;
    cbn [mw_go] in H; apply bind_inv in H as ([e1 n1] & s1 & H1 & H2); destruct (Hok _ s _ s1 Hi H1) as [Hi1 F1];
    cbn beta iota zeta in H2.
  - destruct (edges_eqb _ _); [|noerr H2]. injection H2; intros; subst. exact (Hr _ _ _ _ _ Hi Hp Hq H1).
  - destruct (edges_eqb _ _).
    + injection H2; intros; subst. exact (Hr _ _ _ _ _ Hi Hp Hq H1).
    + exact (IH _ _ s1 e needs s' H2 Hi1 (Pf _ _ F1 Hp) Hq).
Qed.

Lemma asi_ty_needs : forall f e mi g needs s e' n' s', add_symbol_item f e mi g [] needs s = Good ((e', n'), s') ->
  gs_kind g = KType -> n' = needs.
Proof.
  intros [|f] e mi g needs s e' n' s' H K; [discriminate|]. cbn [add_symbol_item] in H.
  apply bind_inv in H as (u & s1 & _ & H). cbn beta zeta in H. rewrite K in H.
  destruct (tn_get e (gs_id g)); [injection H as _ <- _; reflexivity|].
  apply bind_inv in H as (st0 & s2 & _ & H). injection H as _ <- _. reflexivity.
Qed.

Lemma asi_fn_needs : forall f e mi g needs s e' n' s', add_symbol_item f e mi g [] needs s = Good ((e', n'), s') ->
  gs_kind g = KFn -> n' = add_need needs (KyFn (gs_id g)).
Proof.
  intros [|f] e mi g needs s e' n' s' H K; [discriminate|]. cbn [add_symbol_item] in H.
  apply bind_inv in H as (u & s1 & _ & H). cbn beta zeta in H. rewrite K in H.
  apply bind_inv in H as (sg & s2 & _ & H).
  apply bind_inv in H as (e1 & s3 & _ & H).
  apply bind_inv in H as (a & s4 & _ & H).
  apply bind_inv in H as (mem & s5 & _ & H).
  apply bind_inv in H as (ms & s6 & _ & H). cbn beta zeta in H.
  apply bind_inv in H as (e2 & s7 & _ & H). injection H as _ <- _. reflexivity.
Qed.

Section Fwd.
Variables (w : Elab.world) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Notation WW := (BodySound.W w R Fl).

(* the target's requirements are type symbols of the loader's *)
Definition ReqTypes (t : nat) : Prop :=
  forall r, In r (R t) -> exists rg, nget (w_syms w) r = Some rg /\ gs_kind rg = KType.

Lemma spectypes_type : forall p sp r, A.SpecTypes WW p sp r -> A.kind_sym WW r = Some A.DKType.
Proof. intros p sp r H. induction H; assumption. Qed.

(* of a fn, they are (WorldOf: its assume nesting's type symbols) *)
Lemma fn_req_types : forall t tg, A.WorldOf b WW -> nget (w_syms w) t = Some tg -> gs_kind tg = KFn -> ReqTypes t.
Proof.
  intros t tg Wo Ht K r Hr.
  assert (Kt : A.kind_sym WW t = Some A.DKFn) by (rewrite (kind_sym_rd w R Fl b Hload _ _ Ht), K; reflexivity).
  apply (proj1 (A.wo_reqs_plain Wo t Kt ltac:(discriminate) ltac:(discriminate) ltac:(discriminate) ltac:(discriminate) r)) in Hr.
  destruct Hr as (d & pa & _ & _ & St). pose proof (spectypes_type _ _ _ St) as Kr.
  unfold A.kind_sym in Kr. destruct (A.w_decl WW r) eqn:Dr; [|discriminate].
  cbn [A.w_decl BodySound.W W_read] in Dr. destruct (nget (w_syms w) r) as [rg|] eqn:Er; [|discriminate].
  exists rg. split; [reflexivity|]. apply kconv_type.
  pose proof (kind_sym_rd w R Fl b Hload _ _ Er) as Kr'. unfold A.kind_sym in Kr'.
  cbn [A.w_decl BodySound.W W_read] in Kr'. rewrite Er in Kr'. injection Dr as <-. rewrite Kr in Kr'.
  injection Kr' as E. exact (eq_sym E).
Qed.

Lemma sym_loader_st : forall x g s g' s', Inv s -> st_w s = w -> nget (w_syms w) x = Some g -> sym x s = Good (g', s') ->
  g' = g /\ s' = s.
Proof.
  intros x g s g' s' Hi Hw Hg H. unfold sym in H.
  rewrite (iv_loader _ Hi b ltac:(rewrite Hw; exact Hload) x g ltac:(rewrite Hw; exact Hg)) in H.
  injection H as <- <-. auto.
Qed.

Lemma fwd_reqs_needs : forall cf mi rs acc s res s',
  mfold (fun acc r => let! rg := sym r in add_symbol_item cf (fst acc) mi rg [] (snd acc)) acc rs s = Good (res, s') ->
  Inv s -> st_w s = w -> (forall r, In r rs -> exists rg, nget (w_syms w) r = Some rg /\ gs_kind rg = KType) ->
  snd res = snd acc.
Proof.
  intros cf mi rs. induction rs as [|r rest IH]; intros acc s res s' H Hi Hw Ht; cbn [mfold] in H.
  - injection H as <- _. reflexivity.
  - apply bind_inv in H as (a1 & s1 & H1 & H2). apply bind_inv in H1 as (rg & s0 & Hs & H1).
    destruct (Ht r (or_introl eq_refl)) as (rg' & Er & Kr).
    destruct (sym_loader_st _ _ _ _ _ Hi Hw Er Hs) as [-> ->].
    destruct a1 as [e1 n1]. destruct (ok_add_symbol_item cf (fst acc) mi rg' [] (snd acc) s _ s1 Hi H1) as [Hi1 F1].
    rewrite (IH (e1, n1) s1 res s' H2 Hi1 (fr_w' w _ _ F1 Hw) (fun r0 H0 => Ht r0 (or_intror H0))).
    exact (asi_ty_needs _ _ _ _ _ _ _ _ _ H1 Kr).
Qed.

Lemma fwd_walk_needs : forall g t tg e0 s e needs s', gs_origin g = OForwarder t ->
  nget (w_syms w) t = Some tg -> gs_kind tg = KFn -> ReqTypes t ->
  fi_walk g e0 s = Good ((e, needs), s') -> Inv s -> st_w s = w -> Later w R Fl s' -> needs = [KyFn t].
Proof.
  intros g t tg e0 s e needs s' O Ht K Rt H Hi Hw L. unfold fi_walk in H. rewrite O in H.
  apply bind_inv in H as (tg' & s1 & H1 & H). destruct (sym_loader_st _ _ _ _ _ Hi Hw Ht H1) as [-> ->].
  destruct (bstep _ _ _ _ _ _ _ (ok_reqs t) Hi H) as (rs & s2 & H2 & H3 & Hi2 & F2).
  destruct (bstep _ _ _ _ _ _ _ ok_ctx_fuel Hi2 H3) as (cf & s3 & _ & H4 & Hi3 & F3).
  assert (Okf : forall acc r, ok (let! rg := sym r in add_symbol_item cf (fst acc) (gs_mod g) rg [] (snd acc)))
    by (intros; apply ok_bind; [unfold sym; okt | intro; apply ok_add_symbol_item]).
  destruct (bstep _ _ _ _ _ _ _ (ok_mfold _ _ _ rs (e0, []) Okf) Hi3 H4) as ([e4 n4] & s4 & H5 & H6 & Hi4 & F4).
  destruct (ok_add_symbol_item cf e4 (gs_mod g) tg [] n4 s4 _ s' Hi4 H6) as [_ F6].
  assert (Er : R t = rs) by (apply (reqs_R w R Fl _ s rs s2 Hi H2); exact (later_from w R Fl _ _ F3 (later_from w R Fl _ _ F4 (later_from w R Fl _ _ F6 L)))).
  pose proof (fwd_reqs_needs _ _ _ _ _ _ _ H5 Hi3 (fr_w' w _ _ F3 (fr_w' w _ _ F2 Hw)) ltac:(rewrite <- Er; exact Rt)) as N4.
  cbn [snd] in N4. subst n4.
  rewrite (asi_fn_needs _ _ _ _ _ _ _ _ _ H6 K), (load_world_ids b w Hload t tg Ht). reflexivity.
Qed.

(* fn_info of a forwarder (the symbol table's entry for its number), cached
   or not: its needs are its target's key *)
Theorem fn_info_fwd : forall g0 g t tg s i s',
  nget (st_syms s) (gs_id g0) = Some g -> gs_origin g = OForwarder t ->
  nget (w_syms w) t = Some tg -> gs_kind tg = KFn -> ReqTypes t ->
  fn_info g0 s = Good (i, s') -> Inv s -> InfoInv s -> st_w s = w -> Later w R Fl s' ->
  fi_needs i = [KyFn t].
Proof.
  intros g0 g t tg s i s' Gs O Ht K Rt H Hi I Hw L.
  assert (Gen : forall g' s0 e needs s1, nget (st_syms s0) (gs_id g0) = Some g' -> g' = g -> Inv s0 -> st_w s0 = w ->
            Later w R Fl s1 -> merged_walk (gs_mod g') (fi_walk g') s0 = Good ((e, needs), s1) -> needs = [KyFn t]).
  { intros g' s0 e needs s1 _ -> Hi0 Hw0 L1 H1. rewrite (mw_go_eq (fi_walk g)) in H1.
    exact (mw_round (fi_walk g) (fun n => n = [KyFn t]) (fun s2 => st_w s2 = w) (Later w R Fl) (gs_mod g)
             (ok_fi_walk g) (fun s2 s3 F E => fr_w' w s2 s3 F E) (fun s2 s3 F L2 => later_from w R Fl s2 s3 F L2)
             (fun e0 s2 e2 n2 s3 Hi2 Hw2 L3 H2 => fwd_walk_needs g t tg e0 s2 e2 n2 s3 O Ht K Rt H2 Hi2 Hw2 L3)
             8 [] [] s0 e needs s1 H1 Hi0 Hw0 L1). }
  destruct (nget (st_info s) (gs_id g0)) as [j|] eqn:Ej.
  - pose proof H as H0. unfold fn_info in H0. apply bind_inv in H0 as (x & s2 & E & H0). injection E as <- <-.
    rewrite Ej in H0. injection H0 as <- <-.
    destruct (I _ _ Ej) as [(g' & s0 & e & needs & s1 & G0 & Hi0 & H1 & -> & F)|[_ (g' & G' & Nf)]].
    + destruct (ok_merged_fi g' s0 _ s1 Hi0 H1) as [_ F1].
      assert (W0 : st_w s0 = w) by (rewrite <- Hw, (fl_w _ _ F), (fr_w _ _ F1); reflexivity).
      assert (Eg : g' = g) by (pose proof (fl_keep _ _ F _ _ (fr_keep _ _ F1 _ _ G0)) as G1; congruence).
      exact (Gen g' s0 e needs s1 G0 Eg Hi0 W0 (later_fromL w R Fl _ _ F L) H1).
    + exfalso. rewrite Gs in G'. injection G' as <-. exact (Nf t O).
  - destruct (fn_info_miss g0 s i s' H Ej) as (g' & e & needs & s1 & Eg & H1 & -> & ->).
    exact (Gen g' s e needs s1 Eg ltac:(congruence) Hi Hw (later_info w R Fl s1 _ L) H1).
Qed.

End Fwd.
