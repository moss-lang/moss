(* Declared signatures read in an explicit receiver context. Unlike the
   plain-function specialization, this also covers method context items. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame
  BodySound BodyTy CtxWalk.
Import ListNotations.
Local Open Scope list_scope.
Module A := Audit.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL].

Section Signature.
Variable w : Elab.world.
Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Notation WW := (BodySound.W w R Fl).
Notation trF := (tr Fl).
Notation path_of mi := (gm_path (get_mod w mi)).

Lemma ety_of_rel_at : forall t mi tm tmr e this s d s',
  ety_of t mi tm e this s = Good (d, s') ->
  Inv s -> st_w s = w -> In mi (w_order w) ->
  TmRel Fl tm tmr e -> opt_wf this -> Later w P R Fl s' ->
  A.TyOf WW (path_of mi) tmr (option_map trF this) t (trF d) /\ WFty d.
Proof.
  intros t mi tm tmr e this s d s' H Hi Hw Hmi Ht Wthis L.
  rewrite ety_of_eq in H.
  exact (proj1 (ty_all w R Fl b Hload 200) t mi tm e this s d s' tmr H Hi Hw Hmi Ht Wthis L).
Qed.

Theorem fn_sig_rel_at : forall g tm tmr e this s sg s' rcv n ps rt body,
  fn_sig g tm e this s = Good (sg, s') ->
  Inv s -> st_w s = w -> nget (w_syms w) (gs_id g) = Some g ->
  gs_decl g = ADFn rcv n ps rt body ->
  TmRel Fl tm tmr e -> opt_wf this -> Later w P R Fl s' ->
  A.SigOf WW (gs_id g) tmr (option_map trF this) (tr_sig Fl sg) /\ sg_this sg = this.
Proof.
  intros g tm tmr e this s sg s' rcv n ps rt body H Hi Hw Hg D Ht Wthis L.
  pose proof (loader_mod_in w R Fl b Hload _ _ Hg) as Hgm.
  unfold fn_sig in H.
  destruct (bstep _ _ _ _ _ _ _
    (ok_mmap _ _ _ _ (fun p => ok_bind _ _ _ _ (ok_ety_of _ _ _ _ _) (fun _ => ok_ret _ _))) Hi H)
    as (ps' & s1 & H1 & H2 & Hi1 & F1).
  apply bind_inv in H2 as (r & s2 & H3 & H4). injection H4 as <- <-.
  assert (Ok3 : ok (match fn_ret g with Some t => ety_of t (gs_mod g) tm e this | None => ret YUnit end))
    by (destruct (fn_ret g); [apply ok_ety_of | apply ok_ret]).
  destruct (Ok3 s1 r s2 Hi1 H3) as [_ F3].
  pose proof (fr_w' w _ _ F1 Hw) as Hw1.
  split; [|reflexivity].
  exists (A.Decl (path_of (gs_mod g)) (gs_decl g) (gs_assumes g)), rcv, n, ps, rt, body.
  split; [exact (w_decl_rd w R Fl _ _ Hg)|]. split; [exact D|].
  cbn [A.d_mod tr_sig A.sg_params A.sg_ret].
  unfold fn_params, fn_ret in *. rewrite D in H1, H3. split.
  - assert (Q : Forall2 (fun p q => fst q = fst p /\
      A.TyOf WW (path_of (gs_mod g)) tmr (option_map trF this) (snd p) (trF (snd q))) ps ps').
    { refine (mmap_ok_rel w R Fl _ _ _ _ ps s ps' s1 _ _ H1 Hi Hw
        (later_from w R Fl _ _ F3 L)).
      - intro p. apply ok_bind; [apply ok_ety_of | intro; apply ok_ret].
      - intros p _ s0 q s3 E Hi0 Hw0 L0.
        apply bind_inv in E as (y & s4 & E1 & E2). injection E2 as <- <-.
        split; [reflexivity|].
        exact (proj1 (ety_of_rel_at _ _ _ _ _ _ _ _ _ E1 Hi0 Hw0 Hgm Ht Wthis L0)). }
    clear -Q. induction Q as [|p q l l' [_ Hq] Hl IH]; constructor; [exact Hq | exact IH].
  - destruct rt as [t|]; [|injection H3 as <- _; reflexivity].
    exact (proj1 (ety_of_rel_at _ _ _ _ _ _ _ _ _ H3 Hi1 Hw1 Hgm Ht Wthis L)).
Qed.
End Signature.
