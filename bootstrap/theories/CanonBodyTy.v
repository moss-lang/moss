(* Source type readings in a context carrying a nonempty merge. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTy SigSound Merge CanonTypes TyOfSubst.
Import ListNotations.
Module A := Audit.Elaboration.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].

Section CanonicalReading.
Variable w : Elab.world. Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Notation WW := (W_read w R Fl).
Notation trF := (tr Fl).

Lemma nassoc_tymap : forall e k, nassoc k (tymap e) = tn_get e k.
Proof.
  intros e k. unfold tymap, tn_get. induction (en_tn e) as [|[n [y st]] l IH]; cbn; [reflexivity|].
  destruct (Nat.eqb k n); [reflexivity|exact IH].
Qed.

Theorem ety_of_canonical : forall t mi e this s d s' tmr,
  ety_of t mi (tymap e) e this s = Good (d,s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> Tri (en_uf e) ->
  (forall k y, tn_get e k = Some y -> WFty y) -> opt_wf this ->
  (forall k, A.nlook k tmr = option_map (fun y => trF (canon e y)) (tn_get e k)) ->
  Later w P R Fl s' ->
  A.TyOf WW (gm_path (get_mod w mi)) tmr (option_map (fun y => trF (canon e y)) this) t (trF (canon e d)) /\ WFty d.
Proof.
  intros t mi e this s d s' tmr H Hi Hw Hmi T Wtn Wth Htm L.
  assert (TR : TmRel Fl (tymap e) (map (trp Fl) (tymap e)) e).
  { apply tmrel_tymap; [exact Wtn|]. intro k. rewrite nlook_trp. rewrite nassoc_tymap. reflexivity. }
  destruct (ety_of_rel_at w R Fl b Hload _ _ _ _ _ _ _ _ _ H Hi Hw Hmi TR Wth L) as [Ty Wd].
  split; [|exact Wd].
  pose proof (tyof_subst WW (suf Fl (en_uf e)) _ _ _ _ _ Ty tmr) as Ts.
  assert (SR : SubRel (suf Fl (en_uf e)) (map (trp Fl) (tymap e)) tmr).
  { intro k. rewrite Htm, nlook_trp, nassoc_tymap.
    destruct (tn_get e k) as [y|]; [|reflexivity]. cbn [option_map]. rewrite tr_canon by exact T. reflexivity. }
  specialize (Ts SR). rewrite tr_canon in Ts by exact T.
  assert (Ethis : option_map (Audit.app_subst (suf Fl (en_uf e))) (option_map trF this) =
    option_map (fun y => trF (canon e y)) this).
  { destruct this; cbn [option_map]; [rewrite tr_canon by exact T|]; reflexivity. }
  rewrite Ethis in Ts. exact Ts.
Qed.
End CanonicalReading.
