(* ------------------------------------------------------------------------ *)
(* BodyOps.v — operators against V.6 (Track A, elaborate_sound step 2;    *)
(* BodySound.v's Pending, pd_synth for AEUnary and AEBinary).              *)
(*                                                                           *)
(* An operator is its library's detached method on the (left) operand      *)
(* ([D68], and [D85]'s `for.` operators: Audit note 60), called as any     *)
(* provided method is.  What this file assumes (section hypotheses):       *)
(*   Hlang      the library modules the elaborator reads (lang_modules) are *)
(*              Part V's LangSym: the generalization of BodySound's Hbool; *)
(*   Hprovided  [the other half of Track A: calls and methods]              *)
(*              provided_call is Part V's Provided.                         *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabMono BodySound BodyDataHelpers Merge CanonTypes.
From NM Require Import InfoPreserve.
Import ListNotations.
Local Open Scope list_scope.

From NM Require Import NominalSupport.

Tactic Notation "bi" hyp(H) "as" ident(a) ident(s1) ident(H1) :=
  apply bind_inv in H as (a & s1 & H1 & H).

Ltac nogood H := exfalso;
  solve [repeat (cbn beta iota zeta in H;
                 first [ exact (err_not_good _ _ _ _ _ _ H) | discriminate H
                       | apply bind_inv in H as (? & ? & ? & H) ])].

Ltac okstep' H L Hi Hi' Fs' := destruct (L _ _ _ Hi H) as [Hi' Fs'].


Module A := Audit.Elaboration.

Local Strategy opaque [Nat.of_num_uint FUEL].

Section Ops.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation WW := (BodySound.W w R Fl).
Notation trF := (tr Fl).

(* [D45], [D68], [D85]: a declaration of a library module the elaborator
   reads (lang_modules) is the relation's LangSym (an obligation of the
   library's shape, as BodySound's Hbool) *)
Hypothesis Hlang : forall (short : string) (det : bool) (n : string) (li x : nat), sassoc short (lang_modules w) = Some li ->
  sassoc n (if det then gm_detached (get_mod w li) else gm_names (get_mod w li)) = Some x ->
  A.LangSym WW short (if det then A.NSDetached else A.NSName) n x.

(* [calls and methods, the other half of Track A] a call of a provided
   method, with source arguments, is Part V's Provided at the receiver's
   type *)
Hypothesis Hprovided : forall f, OK w P R Fl f -> forall c e ox oty m args s x t s',
  provided_call f c e ox oty m (map AA args) s = Good ((x, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F -> WFty oty -> Support WW oty ->
  A.Provided WW P F C ox (trF (canon e oty)) (gs_id m) args x (trF (canon e t)) /\ WFty t /\Support WW t.

Lemma lang_symbol_spec : forall short n det why mi s g s', lang_symbol short n det why mi s = Good (g, s') ->
  Inv s -> Later w P R Fl s -> s' = s /\ A.LangSym WW short (if det then A.NSDetached else A.NSName) n (gs_id g).
Proof.
  intros short n det why mi s g s' H Hi L. unfold lang_symbol in H.
  bi H as lm s1 Hl. unfold lang in Hl. bi Hl as s0 s2 Hg. injection Hg as <- <-. injection Hl as <- <-.
  rewrite (lt_lang _ _ _ _ _ L) in H.
  destruct (sassoc short (lang_modules w)) as [li|] eqn:El; [|nogood H].
  bi H as m s3 Hm. unfold the_mod in Hm. injection Hm as <- <-. rewrite (lt_w _ _ _ _ _ L) in H.
  destruct (sassoc n (if det then gm_detached (get_mod w li) else gm_names (get_mod w li))) as [x|] eqn:Ex;
    [|destruct det; nogood H].
  assert (Hs : sym x s = Good (g, s')) by (destruct det; exact H).
  unfold sym in Hs. destruct (nget (st_syms s) x) as [g0|] eqn:Eg; [|discriminate]. injection Hs as <- <-.
  split; [reflexivity|]. rewrite (iv_ids _ Hi x g0 Eg). exact (Hlang short det n li x El Ex).
Qed.

Lemma head_tr : forall t h, Elab.head t = Some h -> A.head (trF t) = Some h.
Proof. intros [| | | | |] h H; cbn in H; try discriminate; exact H. Qed.

Lemma operator_rel : forall op unary, Elab.operator_method op unary = A.operator_method op unary.
Proof. reflexivity. Qed.

Lemma operator_step : forall f, OK w P R Fl f -> forall c e op lm n operand args (ys : list aexpr) s v t s' unary,
  args = map AA ys -> Elab.operator_method op unary = Some (lm, n) ->
  synth_operator f c e lm n op operand args s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->
  exists m ox ot, A.LangSym WW lm A.NSDetached n m /\ A.Ex WW P F C operand None ox ot /\
    A.head (A.canon C ot) <> None /\ A.Provided WW P F C ox (A.canon C ot) m ys v (trF (canon e t)) /\ WFty t /\Support WW t.
Proof.
  intros f IH c e op lm n operand args ys s v t s' unary -> Eop H F C Hi Ii Ifwd Hc Hp.
  destruct f as [|f']; [discriminate H|]. pose proof (ok_down w P R Fl f' IH) as IH'.
  cbn [synth_operator] in H.
  pose proof (proj2 (ok_synth_operator (S f') c e lm n op operand (map AA ys) s _ s' Hi
                       ltac:(cbn [synth_operator]; exact H))) as Fs.
  destruct (post_back w P R Fl _ _ F Fs Hp) as [L0 _].
  bi H as m s1 Hm. destruct (lang_symbol_spec _ _ _ _ _ _ _ _ Hm Hi L0) as [-> Hls].
  bi H as r2 s2 Ho. destruct r2 as [ox oty]. okstep' Ho (ok_synth f' c e operand None) Hi Hi2 Fs2.
  destruct (Elab.head (canon e oty)) as [h|] eqn:Eh; [|nogood H].
  okstep' H (ok_provided_call f' c e ox oty m (map AA ys)) Hi2 Hi3 Fs3.
  pose proof (post_back w P R Fl _ _ F Fs3 Hp) as Hp2.
  destruct (bs_synth _ _ _ _ _ IH' c e operand None s ox oty s2 Ho F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc I I Hp2) as[Eo[Wo So]].
  destruct (Hprovided f' IH' c e ox oty m ys s2 v t s' H F C Hi2 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp Wo So) as[Hpr[Wt St]].
  exists (gs_id m), ox, (trF (canon e oty)).
  rewrite (context_canon_tr Fl e C (canon e oty) (cr_sub _ _ _ _ _ _ _ Hc) (proj1 (cr_uf _ _ _ _ _ _ _ Hc))).
  rewrite canon_idem by exact (proj1 (cr_uf _ _ _ _ _ _ _ Hc)).
  split; [exact Hls|]. split; [exact Eo|]. split; [rewrite (head_tr _ _ Eh); discriminate|]. auto.
Qed.

Lemma unary_step : forall f, OK w P R Fl f -> forall c e op y exp s v t s',
  synth (S f) c e (AEUnary op y) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> opt_wf exp ->opt_support WW exp -> Post w P R Fl s' F ->
  A.Ex WW P F C (AEUnary op y) (option_map (fun t => trF (canon e t)) exp) v (trF (canon e t)) /\ WFty t /\Support WW t.
Proof.
  intros f IH c e op y exp s v t s' H F C Hi Ii Ifwd Hc Hx HexpS Hp.
  cbn [synth] in H.
  destruct (Elab.operator_method op true) as [[lm n]|] eqn:Eop; [|cbn beta iota in H; nogood H].
  destruct (synth_wrap e exp s v t s' _ H (ok_synth_operator f c e lm n op y []) Hi) as (s1 & H1 & Hi1 & Fs1 & Fs1').
  pose proof (post_back w P R Fl _ _ F Fs1' Hp) as Hp1.
  destruct (operator_step f IH c e op lm n y [] [] s v t s1 true eq_refl Eop H1 F C Hi Ii Ifwd Hc Hp1)
    as (m & ox & ot & Hls & Eo & Hh & Hpr & Wt & St).
  split;[|split;[exact Wt|exact St]]. eapply A.ExUnary; [rewrite <- operator_rel; exact Eop | exact Hls | exact Eo | exact Hh | exact Hpr].
Qed.

Lemma binary_step : forall f, OK w P R Fl f -> forall c e op l r exp s v t s',
  synth (S f) c e (AEBinary op l r) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> opt_wf exp ->opt_support WW exp -> Post w P R Fl s' F ->
  A.Ex WW P F C (AEBinary op l r) (option_map (fun t => trF (canon e t)) exp) v (trF (canon e t)) /\ WFty t /\Support WW t.
Proof.
  intros f IH c e op l r exp s v t s' H F C Hi Ii Ifwd Hc Hx HexpS Hp.
  cbn [synth] in H.
  destruct (Elab.operator_method op false) as [[lm n]|] eqn:Eop; [|cbn beta iota in H; nogood H].
  destruct (synth_wrap e exp s v t s' _ H (ok_synth_operator f c e lm n op l [AA r]) Hi) as (s1 & H1 & Hi1 & Fs1 & Fs1').
  pose proof (post_back w P R Fl _ _ F Fs1' Hp) as Hp1.
  destruct (operator_step f IH c e op lm n l [AA r] [r] s v t s1 false eq_refl Eop H1 F C Hi Ii Ifwd Hc Hp1)
    as (m & ox & ot & Hls & Eo & Hh & Hpr & Wt & St).
  split;[|split;[exact Wt|exact St]]. eapply A.ExBinary; [rewrite <- operator_rel; exact Eop | exact Hls | exact Eo | exact Hh | exact Hpr].
Qed.

End Ops.
