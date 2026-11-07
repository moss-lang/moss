(* Chosen method declarations belong to the loaded source world. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore MethodLookup.
Import ListNotations.
Module A := Audit.Elaboration.

Lemma decl_has_loader : forall w R Fl mid d,
  A.w_decl (W_read w R Fl) mid = Some d ->
  exists g, nget (w_syms w) mid = Some g.
Proof.
  intros w R Fl mid d H. cbn [A.w_decl W_read] in H.
  destruct (nget (w_syms w) mid); [eauto|discriminate].
Qed.

Lemma scope_has_loader : forall b w R Fl p ns n mid,
  load_world b = Good w -> A.w_scope (W_read w R Fl) p ns n = Some (A.ESym mid) ->
  exists g, nget (w_syms w) mid = Some g.
Proof.
  intros b w R Fl p ns n mid H N. unfold load_world in H.
  apply rbind_good in H as ([en st] & Hl & H). injection H as <-.
  destruct (load_spec b _ _ _ _ _ _ Hl (linv_init b)) as (L & _).
  exact (table_esym b st en L _ false p ns n mid (fun _ _ _ => eq_refl) N).
Qed.

Lemma chosen_method_dynamic : forall recv mi n s m s',
  Inv s -> find_method_decl mi recv n s = Good (Some m,s') ->
  s'=s /\ nget (st_syms s) (gs_id m) = Some m.
Proof.
  intros recv mi n s m s' I H. unfold find_method_decl in H.
  apply bind_inv in H as (hm & s1 & H1 & H). unfold the_mod in H1. injection H1 as <- <-.
  apply bind_inv in H as (here & s1 & H1 & H). unfold the_mod in H1. injection H1 as <- <-.
  destruct (assoc attached_eqb (gs_id recv,n) (gm_attached (get_mod (st_w s) (gs_mod recv))));
    [|destruct (sassoc n (gm_detached (get_mod (st_w s) mi))); [|discriminate]].
  all: apply bind_inv in H as (g & s1 & H1 & H); injection H as <- <-;
    unfold sym in H1; destruct (nget (st_syms s) n0) eqn:E; [|discriminate];
    injection H1 as <- <-; split; [reflexivity|]; rewrite (iv_ids _ I _ _ E); exact E.
Qed.

Theorem find_method_decl_loaded : forall b w (R : nat -> list nat) (Fl : string -> nat) mi recv n s m s',
  load_world b = Good w -> In mi (w_order w) ->
  nget (w_syms w) (gs_id recv) = Some recv -> Inv s -> st_w s = w ->
  find_method_decl mi recv n s = Good (Some m,s') ->
  s'=s /\ nget (w_syms w) (gs_id m) = Some m.
Proof.
  intros b w R Fl mi recv n s m s' Hl Hmi Hr I Hw H.
  destruct (chosen_method_dynamic _ _ _ _ _ _ I H) as [-> D].
  destruct (find_method_decl_named b w R Fl mi recv n s m s Hl Hmi Hr I Hw H) as [_ N].
  assert (L : exists g, nget (w_syms w) (gs_id m) = Some g).
  { destruct N as [N|[_ N]].
    - apply (proj1 (lr_attached _ _ (load_world_reads b w R Fl Hl) _ _ _)) in N.
      destruct N as (d & rn & ps & rt & body & Hd & _).
      exact (decl_has_loader w R Fl _ _ Hd).
    - exact (scope_has_loader b w R Fl _ _ _ _ Hl N). }
  destruct L as [g Hg].
  pose proof (iv_loader _ I b ltac:(rewrite Hw; exact Hl) _ g ltac:(rewrite Hw; exact Hg)) as E.
  rewrite E in D. injection D as <-. auto.
Qed.
