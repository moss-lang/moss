(* Context method lookup follows MethodNamed: an attached declaration in
   the receiver's file, otherwise a detached declaration in this scope. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame.
Import ListNotations.
Module A := Audit.Elaboration.

Lemma loaded_scope_table : forall b w R Fl,
  load_world b = Good w -> forall mi ns n, In mi (w_order w) ->
  A.w_scope (W_read w R Fl) (gm_path (get_mod w mi)) ns n =
  option_map (aent w ns) (sassoc n (tbl (get_mod w mi) false ns)).
Proof.
  intros b w R Fl H mi ns n Hi. unfold load_world in H.
  apply rbind_good in H as ([en st] & Hl & H). injection H as <-.
  destruct (load_spec b _ _ _ _ _ _ Hl (linv_init b)) as (L & _).
  cbn [w_order] in Hi.
  destruct (proj1 (li_order _ _ L mi) Hi) as (p & Hp).
  destruct (li_loaded _ _ L p mi Hp) as [_ (m & am & Em & Pm & _)].
  rewrite (fin_get_mod b st en mi m Em), Pm.
  exact (fin_scope b st en L R Fl p mi m ns n Hp Em).
Qed.

Theorem find_method_decl_named : forall b w R Fl mi recv n s m s',
  load_world b = Good w -> In mi (w_order w) ->
  nget (w_syms w) (gs_id recv) = Some recv ->
  Inv s -> st_w s = w ->
  find_method_decl mi recv n s = Good (Some m, s') ->
  s' = s /\ A.MethodNamed (W_read w R Fl)
    (gm_path (get_mod w mi)) (gs_id recv) n (gs_id m).
Proof.
  intros b w R Fl mi recv n s m s' Hl Hmi Hr Hi Hw H.
  unfold find_method_decl in H.
  apply bind_inv in H as (hm & s1 & E1 & H).
  unfold the_mod in E1. injection E1 as <- <-.
  apply bind_inv in H as (here & s1 & E1 & H).
  unfold the_mod in E1. injection E1 as <- <-. rewrite Hw in H.
  destruct (assoc attached_eqb (gs_id recv, n)
    (gm_attached (get_mod w (gs_mod recv)))) as [mid|] eqn:Ea.
  - apply bind_inv in H as (mg & s1 & Es & H).
    injection H as <- <-. unfold sym in Es.
    destruct (nget (st_syms s) mid) as [g|] eqn:Eg; [|discriminate].
    injection Es as <- <-. split; [reflexivity|].
    left. unfold A.Attached. cbn [A.w_attached W_read]. rewrite Hr.
    rewrite (iv_ids _ Hi mid _ Eg). exact Ea.
  - destruct (sassoc n (gm_detached (get_mod w mi))) as [mid|] eqn:Ed;
      [|discriminate].
    apply bind_inv in H as (mg & s1 & Es & H).
    injection H as <- <-. unfold sym in Es.
    destruct (nget (st_syms s) mid) as [g|] eqn:Eg; [|discriminate].
    injection Es as <- <-. split; [reflexivity|]. right. split.
    + intros mid' Ha. unfold A.Attached in Ha.
      cbn [A.w_attached W_read] in Ha. rewrite Hr, Ea in Ha. discriminate.
    + rewrite (loaded_scope_table b w R Fl Hl mi A.NSDetached n Hmi).
      cbn [tbl]. rewrite Ed. cbn [aent].
      rewrite (iv_ids _ Hi mid _ Eg). reflexivity.
Qed.
