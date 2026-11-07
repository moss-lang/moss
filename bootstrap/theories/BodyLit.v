(* ------------------------------------------------------------------------ *)
(* BodyLit.v — literals against V.6's ExLit (Track A, elaborate_sound step *)
(* 2; BodySound.v's Pending, pd_synth for AELit).                          *)
(*                                                                           *)
(* lit_step: the term the elaborator builds for a literal ([D68]: `zero`,   *)
(* `one`, num.moss's `.add`/`.mul`/`.div` provided at the literal's type,  *)
(* float.moss's `float`) is a LitTerm denoting lit_value of its text.      *)
(*                                                                           *)
(* What this file assumes (section hypotheses):                             *)
(*   Hlang     as BodyOps.v's (the library modules are LangSym);            *)
(*   HcallAB   [calls and methods] typed literal arithmetic follows either *)
(*             a provision in force or a uniquely selected family; the     *)
(*             elaborator checks the signature is `(T): T` at this use.    *)
(* Float conversion reads the actual signature and the argument fit from  *)
(* check_args; no library signature shape is assumed.                       *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Ascii String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabMono BodySound BodyDataHelpers BodyOps LiteralResult.
From NM Require Import InfoPreserve.
Import ListNotations.
Local Open Scope string_scope.
Local Open Scope list_scope.

From NM Require Import NominalSupport.

Module A := Audit.Elaboration.

Local Strategy opaque [Nat.of_num_uint FUEL].

Tactic Notation "bi" hyp(H) "as" ident(a) ident(s1) ident(H1) :=
  apply bind_inv in H as (a & s1 & H1 & H).

Ltac nogood H := exfalso;
  solve [repeat (cbn beta iota zeta in H;
                 first [ exact (err_not_good _ _ _ _ _ _ H) | discriminate H
                       | apply bind_inv in H as (? & ? & ? & H) ])].

Ltac okstep' H L Hi Hi' Fs' := destruct (L _ _ _ Hi H) as [Hi' Fs'].

(* ======================================================================== *)
(* 1. Arithmetic of the digits                                               *)
(* ======================================================================== *)

Definition dstep (v : nat) (c : ascii) : nat := 10 * v + (nat_of_ascii c - 48).

Lemma digits_val_fold : forall l v, A.digits_val l v = fold_left dstep l v.
Proof. induction l as [|c r IH]; intro v; [reflexivity | cbn; rewrite IH; reflexivity]. Qed.

Lemma parse_exp_fold : forall l v, fold_left (fun n c => 10 * n + digit_val c) l v = A.digits_val l v.
Proof. intros l v. rewrite digits_val_fold. reflexivity. Qed.

Lemma strip_zeros_val : forall l, A.digits_val (strip_zeros l) 0 = A.digits_val l 0.
Proof.
  induction l as [|c r IH]; [reflexivity|]. cbn [strip_zeros].
  destruct (ascii_dec c "0"%char) as [->|Hne]; [exact IH|].
  replace (match c with "0"%char => strip_zeros r | _ => c :: r end) with (c :: r); [reflexivity|].
  destruct c as [[] [] [] [] [] [] [] []]; try reflexivity; exfalso; exact (Hne eq_refl).
Qed.

Lemma strip_zeros_head : forall l d r, strip_zeros l = d :: r -> d <> "0"%char /\ incl (d :: r) l.
Proof.
  induction l as [|c l IH]; intros d r H; [discriminate|]. cbn [strip_zeros] in H.
  destruct (ascii_dec c "0"%char) as [->|Hne].
  - destruct (IH d r H) as [Hd Hi]. split; [exact Hd | intros x Hx; right; exact (Hi x Hx)].
  - assert (E : (match c with "0"%char => strip_zeros l | _ => c :: l end) = c :: l)
      by (destruct c as [[] [] [] [] [] [] [] []]; try reflexivity; exfalso; exact (Hne eq_refl)).
    rewrite E in H. injection H as <- <-. split; [exact Hne | intros x Hx; exact Hx].
Qed.

Lemma strip_zeros_nil : forall l, strip_zeros l = [] -> A.digits_val l 0 = 0.
Proof. intros l H. rewrite <- strip_zeros_val, H. reflexivity. Qed.

(* a digit other than `0` is worth at least one *)
Lemma digit_pos : forall d, is_digit_char d = true -> d <> "0"%char -> 1 <= digit_val d.
Proof.
  intros d H Hne. unfold is_digit_char in H. apply andb_true_iff in H as [H1 H2].
  apply Nat.leb_le in H1. apply Nat.leb_le in H2. unfold digit_val.
  destruct (Nat.eq_dec (nat_of_ascii d) 48) as [E|E]; [|lia].
  exfalso. apply Hne. rewrite <- (ascii_nat_embedding d), E. reflexivity.
Qed.

(* the literal's text, split as the relation splits it *)
Lemma split_at_e_eq : forall l acc, existsb (fun c => Ascii.eqb c "E"%char) l = false ->
  split_at_e l acc = A.split_at "e"%char l acc.
Proof.
  induction l as [|c r IH]; intros acc H; [reflexivity|]. cbn in H |- *. apply orb_false_iff in H as [H1 H2].
  rewrite H1, orb_false_r. destruct (Ascii.eqb c "e"%char); [reflexivity | exact (IH _ H2)].
Qed.

Lemma split_at_dot_eq : forall l acc, split_at_dot l acc = A.split_at "."%char l acc.
Proof. induction l as [|c r IH]; intro acc; [reflexivity|]. cbn. destruct (Ascii.eqb c "."%char); [reflexivity | apply IH]. Qed.

Lemma split_some_in : forall ch l acc m r, A.split_at ch l acc = (m, Some r) -> In ch l.
Proof.
  intros ch l. induction l as [|c l IH]; intros acc m r H; [discriminate|]. cbn in H.
  destruct (Ascii.eqb_spec c ch) as [->|]; [left; reflexivity | right; exact (IH _ _ _ H)].
Qed.

Lemma split_fst_incl : forall ch l acc m o, A.split_at ch l acc = (m, o) -> incl m (rev acc ++ l).
Proof.
  intros ch l. induction l as [|c l IH]; intros acc m o H.
  - cbn in H. injection H as <- _. rewrite app_nil_r. apply incl_refl.
  - cbn in H. destruct (Ascii.eqb c ch).
    + injection H as <- _. intros y Hy. apply in_app_iff. left. exact Hy.
    + intros y Hy. apply (IH _ _ _ H) in Hy. cbn in Hy. rewrite <- app_assoc in Hy. exact Hy.
Qed.

Lemma split_none : forall ch l acc m, A.split_at ch l acc = (m, None) -> m = rev acc ++ l.
Proof.
  intros ch l. induction l as [|c l IH]; intros acc m H.
  - cbn in H. injection H as <-. rewrite app_nil_r. reflexivity.
  - cbn in H. destruct (Ascii.eqb c ch); [discriminate|]. rewrite (IH _ _ H). cbn. rewrite <- app_assoc. reflexivity.
Qed.

(* the exponent, as lit_value reads it *)
Definition exp_of (ex : option (list ascii)) : bool * nat :=
  match ex with Some l => parse_exp l | None => (false, 0) end.

Lemma exp_of_val : forall ex, exp_of ex =
  match ex with
  | Some ("-"%char :: d) => (true, A.digits_val d 0)
  | Some ("+"%char :: d) => (false, A.digits_val d 0)
  | Some d => (false, A.digits_val d 0)
  | None => (false, 0) end.
Proof.
  intros [[|c r]|]; [reflexivity| |reflexivity]. unfold exp_of, parse_exp.
  destruct c as [[] [] [] [] [] [] [] []]; cbn [fst snd]; rewrite ?parse_exp_fold; reflexivity.
Qed.

(* lit_value's case analysis of the shift *)
Definition lv (n : nat) (eneg : bool) (emag lf : nat) : nat * nat :=
  if eneg then (n, 10 ^ (emag + lf))
  else if Nat.leb lf emag then (n * 10 ^ (emag - lf), 1) else (n, 10 ^ (lf - emag)).

Lemma lv_cases : forall (n : nat) (eneg : bool) (emag lf : nat) (sneg : bool) (smag : nat),
  (if eneg then (true, emag + lf) else if Nat.leb lf emag then (false, emag - lf) else (true, lf - emag)) = (sneg, smag) ->
  (smag = 0 -> lv n eneg emag lf = (n, 1)) /\
  (smag <> 0 -> sneg = true -> lv n eneg emag lf = (n * 1, 1 * 10 ^ smag)) /\
  (smag <> 0 -> sneg = false -> lv n eneg emag lf = (n * 10 ^ smag, 1 * 1)).
Proof.
  intros n eneg emag lf sneg smag H. unfold lv. destruct eneg.
  - injection H as <- <-. split; [|split]; intros Hs; [rewrite Hs; reflexivity | | discriminate].
    intros _. f_equal; lia.
  - destruct (Nat.leb lf emag) eqn:E; injection H as <- <-.
    + split; [|split]; intros Hs; [rewrite Hs; cbn; f_equal; lia | discriminate |]. intros _. f_equal; lia.
    + apply Nat.leb_gt in E. split; [|split]; intros Hs; [lia | | discriminate]. intros _. f_equal; lia.
Qed.

Lemma lit_value_lv : forall text mant ex whole frac,
  existsb (fun c => Ascii.eqb c "E"%char) (list_ascii_of_string text) = false ->
  split_at_e (list_ascii_of_string text) [] = (mant, ex) -> split_at_dot mant [] = (whole, frac) ->
  A.lit_value text = lv (A.digits_val (whole ++ opt_default [] frac) 0) (fst (exp_of ex)) (snd (exp_of ex))
                        (List.length (opt_default [] frac)).
Proof.
  intros text mant ex whole frac Hn Ee Ed. unfold A.lit_value.
  rewrite <- (split_at_e_eq _ _ Hn), Ee. cbn iota. rewrite <- split_at_dot_eq, Ed. cbn iota.
  rewrite exp_of_val. unfold lv.
  destruct (match ex with
            | Some ("-"%char :: d) => (true, A.digits_val d 0)
            | Some ("+"%char :: d) => (false, A.digits_val d 0)
            | Some d => (false, A.digits_val d 0)
            | None => (false, 0) end) as [eneg emag].
  destruct frac; reflexivity.
Qed.

(* ======================================================================== *)
(* 2. Folds whose accumulator stands for a number                            *)
(* ======================================================================== *)

Section Folds.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).

Lemma mfold_val : forall A B (g : A -> B -> M A) (Q : nat -> A -> Prop) (h : nat -> B -> nat) F l a v s r s',
  mfold g a l s = Good (r, s') -> Inv s -> InfoInv s -> FwdInv s -> Post w P R Fl s' F -> (forall a x, okI (g a x) /\okF(g a x)) ->
  (forall a x s1 a' s2 v, In x l -> Q v a -> Inv s1 -> InfoInv s1 -> FwdInv s1 -> g a x s1 = Good (a', s2) -> Post w P R Fl s2 F -> Q (h v x) a') ->
  Q v a -> Q (fold_left h l v) r.
Proof.
  intros A B g Q h F l. induction l as [|x l IH]; intros a v s r s' H Hi Ii Ifwd Hp Ok Hs Hq.
  - cbn in H. injection H as <- _. exact Hq.
  - cbn [mfold] in H. bi H as a1 s1 H1. pose proof(proj1(Ok a x))as Ocache. pose proof(proj2(Ok a x))as Oforward. okstep' H1 (proj1(proj1(Ok a x))) Hi Hi1 Fs1.
    pose proof (proj2 (ok_mfold _ _ _ l a1 (fun a x=>proj1(proj1(Ok a x))) s1 _ _ Hi1 H)) as Fr1.
    cbn [fold_left]. refine (IH a1 (h v x) s1 r s' H Hi1 ltac:(cache_inv) ltac:(fwd_inv) Hp Ok _ _).
    + intros a0 x0 s0 a0' s2 v0 Hin. exact (Hs a0 x0 s0 a0' s2 v0 (or_intror Hin)).
    + exact (Hs a x s a1 s1 v (or_introl eq_refl) Hq Hi Ii Ifwd H1 (post_back w P R Fl _ _ F Fr1 Hp)).
Qed.

End Folds.

Lemma fold_succ : forall k v, fold_left (fun n (_ : unit) => n + 1) (repeat tt k) v = v + k.
Proof. induction k as [|k IH]; intro v; cbn; [lia | rewrite IH; lia]. Qed.

Lemma fold_ten : forall k v, fold_left (fun n (_ : unit) => n * 10) (repeat tt k) v = v * 10 ^ k.
Proof. induction k as [|k IH]; intro v; cbn [fold_left repeat]; [cbn; lia | rewrite IH; cbn [Nat.pow]; lia]. Qed.

(* synth_literal's float case, as Elab writes it *)
Definition float_lit_m (f : nat) (c : cx) (e : env) (why : string) (whole : list ascii)
    (frac ex : option (list ascii)) : M (xexpr * ety) :=
      let fr := opt_default [] frac in
      let '(eneg, emag) := match ex with Some l => parse_exp l | None => (false, 0) end in
      let '(sneg, smag) := if eneg then (true, emag + List.length fr)
                           else if Nat.leb (List.length fr) emag then (false, emag - List.length fr)
                           else (true, List.length fr - emag) in
      let digits := strip_zeros (whole ++ fr) in
      let! mantissa :=
        match digits with
        | [] => let! z := int_literal f c e ["0"%char] why in float_of f c e z why
        | d0 :: rest =>
            let! i10 := int_literal f c e ["1"%char; "0"%char] why in
            let! ten := float_of f c e i10 why in
            let! i0 := int_literal f c e [d0] why in
            let! m0 := float_of f c e i0 why in
            mfold (fun acc d =>
                     let! acc := lang_call f c e acc "mul" [ten] why in
                     if Ascii.eqb d "0"%char then ret acc
                     else let! id := int_literal f c e [d] why in
                          let! fd := float_of f c e id why in
                          lang_call f c e acc "add" [fd] why) m0 rest
        end in
      match smag with
      | 0 => ret mantissa
      | _ =>
          let! i10 := int_literal f c e ["1"%char; "0"%char] why in
          let! ten := float_of f c e i10 why in
          let! scale := mfold (fun acc _ => lang_call f c e acc "mul" [ten] why) ten (repeat tt (smag - 1)) in
          lang_call f c e mantissa (if sneg then "div" else "mul") [scale] why
      end.

(* ======================================================================== *)
(* 3. The literal's terms                                                    *)
(* ======================================================================== *)

Section Lit.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation WW := (BodySound.W w R Fl).
Notation trF := (tr Fl).
Notation LT C t x a b := (A.LitTerm WW C t x a b).
Variable b : bundle.
Hypothesis Hload : load_world b = Good w.

Hypothesis Hlang : forall (short : string) (det : bool) (n : string) (li x : nat), sassoc short (lang_modules w) = Some li ->
  sassoc n (if det then gm_detached (get_mod w li) else gm_names (get_mod w li)) = Some x ->
  A.LangSym WW short (if det then A.NSDetached else A.NSName) n x.

(* [calls and methods] The typed-operand form of provided_call, including
   its family case. This is the same method-call obligation used by the
   body proof, specialized to literal arithmetic's checked signature. *)
Hypothesis HcallAB : forall f, OK w P R Fl f -> forall c e ox T oy m s x t s',
  provided_call f c e ox T m [AB oy T] s = Good ((x, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->WFty T ->Support WW T ->
  A.LitOp WW C ((fun t => trF (canon e t)) T) (gs_id m) ox oy x.

Lemma lt_eq : forall C t x a b0 a' b', LT C t x a b0 -> a = a' -> b0 = b' -> LT C t x a' b'.
Proof. intros. subst. assumption. Qed.

(* (the kernel otherwise unfolds `show` comparing the report's binds) *)
Local Strategy opaque [show].

Lemma no_provision_bad : forall mi e oty m r mid s a s', no_provision mi e oty m r mid s = Good (a, s') -> False.
Proof.
  intros mi e oty m r mid s a s' H. unfold no_provision in H. apply bind_inv in H as (so & s1 & Hs & H).
  destruct (filter _ _) as [|q qs]; [exact (err_not_good _ _ _ _ _ _ H)|].
  apply bind_inv in H as (ws & s2 & Hw & H). exact (err_not_good _ _ _ _ _ _ H).
Qed.

(* one arithmetic step: num.moss's method at T, applied to an argument at T *)
Lemma lang_call_step : forall f, OK w P R Fl f -> forall c e x1 T x2 n why s x t s' F C,
  lang_call f c e (x1, T) n [(x2, T)] why s = Good ((x, t), s') -> In n ["add"; "mul"; "div"]%string ->
  Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->WFty T ->Support WW T ->
  exists m, A.LangSym WW "num" A.NSDetached n m /\ t = T /\
    A.LitOp WW C ((fun t => trF (canon e t)) T) m x1 x2 x.
Proof.
  intros f IH0 c e x1 T x2 n why s x t s' F C H Hn Hi Ii Ifwd Hc Hp WT ST.
  pose proof (proj2 (ok_lang_call f c e (x1, T) n [(x2, T)] why s _ s' Hi H)) as Fs.
  destruct f as [|f]; [discriminate H|]. pose proof (ok_down w P R Fl f IH0) as IH.
  destruct (post_back w P R Fl _ _ F Fs Hp) as [L _].
  cbn [lang_call] in H.
  bi H as m s1 Hm.
  destruct (lang_symbol_spec w P R Fl Hlang _ _ _ _ _ _ _ _ Hm Hi L) as [-> Hls].
  pose proof (HcallAB f IH c e x1 T x2 m s x t s' H F C Hi Ii Ifwd Hc Hp WT ST) as Hop.
  pose proof (provided_call_literal_type f c e x1 T x2 m s x t s' H) as Et.
  exists (gs_id m). split; [exact Hls|]. split; [exact Et|exact Hop].
Qed.

(* the rest of a run keeps the frame: Post at the state a step left *)
Tactic Notation "post_rest" hyp(H) hyp(Hi) hyp(Hp) ident(Hp1) :=
  match type of H with ?m ?s1 = Good (_, ?s2) =>
    let Ok := fresh "Ok" in assert (Ok : ok m) by okt;
    pose proof (post_back w P R Fl _ _ _ (proj2 (Ok _ _ _ Hi H)) Hp) as Hp1; clear Ok
  end.

(* int.moss's `zero`/`one`, as the elaborator reads them *)
Definition IntTy (e : env) (t : ety) : Prop :=
  (exists o, A.LangSym WW "int" A.NSName "one" o /\ nget (en_vals e) o = Some t) \/
  (exists z, A.LangSym WW "int" A.NSName "zero" z /\ nget (en_vals e) z = Some t).

Lemma intty_support : forall c e F C t,CxRel w R Fl c e F C ->IntTy e t ->Support WW t.
Proof.
 intros c e F C t Hc [[o[Ho Ev]]|[o[Ho Ev]]];exact(@es_vals WW e(cr_support _ _ _ _ _ _ _ Hc)o t Ev).
Qed.

Lemma lang_val_step : forall c e n why s x t s' F C,
  (let! g := lang_symbol "int" n false why (cx_mod c) in
   match nget (en_vals e) (gs_id g) with
   | Some t => ret (XVl (gs_id g), t)
   | None => err (cx_mod c) (why ++ " means `" ++ n ++ "`, which is not available in the context here (assume or bind it)")%string
   end) s = Good ((x, t), s') ->
  Inv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->
  exists o, A.LangSym WW "int" A.NSName n o /\ nget (en_vals e) o = Some t /\ x = XVl o /\
    A.nlook o (A.c_vals C) = Some ((fun t => trF (canon e t)) t) /\ WFty t /\ s' = s.
Proof.
  intros c e n why s x t s' F C H Hi Hc [L _]. bi H as g s1 Hg.
  assert (Okl : ok (lang_symbol "int" n false why (cx_mod c))) by (unfold lang_symbol, lang, the_mod; okt).
  okstep' Hg Okl Hi Hi1 Fs1.
  destruct (nget (en_vals e) (gs_id g)) as [tv|] eqn:Ev; [|nogood H]. injection H as <- <- <-.
  destruct (lang_symbol_spec w P R Fl Hlang _ _ _ _ _ _ _ _ Hg Hi (later_back w P R Fl _ _ Fs1 L)) as [-> Hls].
  exists (gs_id g). split; [exact Hls|]. split; [exact Ev|]. split; [reflexivity|].
  split; [rewrite (cr_vals _ _ _ _ _ _ _ Hc), Ev; reflexivity|]. split; [exact (cr_vals_wf _ _ _ _ _ _ _ Hc _ _ Ev) | reflexivity].
Qed.

(* a step of a run, its frame, and Post where it ends *)
Tactic Notation "stepok" hyp(H) ident(a) ident(s1) ident(H1) hyp(Hi) ident(Hi1) ident(Fs1) :=
  apply bind_inv in H as (a & s1 & H1 & H);
  match type of H1 with ?m _ = _ =>
    let Ok := fresh "Ok" in assert (Ok : ok m) by okt; destruct (Ok _ _ _ Hi H1) as [Hi1 Fs1]; clear Ok
  end.

Lemma int_lit_step : forall f, OK w P R Fl f -> forall c e ds why s x t s' F C,
  int_literal f c e ds why s = Good ((x, t), s') -> forallb is_digit_char ds = true ->
  Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->
  IntTy e t /\ LT C ((fun t => trF (canon e t)) t) x (A.digits_val ds 0) 1 /\ WFty t.
Proof.
  intros f IH0 c e ds why s x t s' F C H Hd Hi Ii Ifwd Hc Hp.
  destruct f as [|f]; [discriminate H|]. pose proof (ok_down w P R Fl f IH0) as IH.
  cbn [int_literal] in H.
  destruct (strip_zeros ds) as [|d0 rest] eqn:Es.
  { (* zero *)
    destruct (lang_val_step c e "zero" why s x t s' F C H Hi Hc Hp) as (z & Hls & Ev & -> & Hn & Wt & _).
    split; [right; exists z; auto|]. split; [|exact Wt].
    rewrite (strip_zeros_nil ds Es). eapply A.LitZero; [exact Hls | exact Hn]. }
  destruct (strip_zeros_head ds d0 rest Es) as [Hd0 Hincl].
  assert (Dig : forall d, In d (d0 :: rest) -> is_digit_char d = true)
    by (intros d Hin; rewrite forallb_forall in Hd; exact (Hd d (Hincl d Hin))).
  stepok H one s1 Hone Hi Hi1 Fs1. post_rest H Hi1 Hp Hp1.
  destruct one as [xo T].
  destruct (lang_val_step c e "one" why s xo T s1 F C Hone Hi Hc Hp1) as (o & Hls1 & Ev1 & -> & Hn1 & WT & _).
  assert (L1 : LT C ((fun t => trF (canon e t)) T) (XVl o) 1 1) by (eapply A.LitOne; [exact Hls1 | exact Hn1]).
  (* `small k`: one, plus one k-1 times *)
  assert (Small : forall k s2 r s3, 1 <= k ->
            mfold (fun acc (_ : unit) => lang_call f c e acc "add" [(XVl o, T)] why) (XVl o, T) (repeat tt (k - 1)) s2 = Good (r, s3) ->
            Inv s2 -> InfoInv s2 -> FwdInv s2 -> Post w P R Fl s3 F -> snd r = T /\ LT C ((fun t => trF (canon e t)) T) (fst r) k 1).
  { intros k s2 r s3 Hk Hm Hi2 Ii2 Ifwd2 Hp3.
    set (Q := fun v (a : xexpr * ety) => snd a = T /\ LT C ((fun t => trF (canon e t)) T) (fst a) v 1).
    assert (QR : Q (fold_left (fun n (_ : unit) => n + 1) (repeat tt (k - 1)) 1) r).
    { refine (mfold_val w P R Fl _ _ _ Q (fun n _ => n + 1) F (repeat tt (k - 1)) (XVl o, T) 1 s2 r s3 Hm Hi2 ltac:(cache_inv) ltac:(fwd_inv) Hp3 _ _ _).
      - intros a0 u. split;[apply okI_lang_call|fwt].
      - intros [xa ta] u s4 [x' t'] s5 v _ [Eta Hv] Hi4 Icb Fcb Hs Hp5. cbn in Eta. subst ta.
        destruct (lang_call_step f IH c e xa T (XVl o) "add" why s4 x' t' s5 F C Hs ltac:(cbn; auto) Hi4 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp5 ltac:(assumption) ltac:(first[assumption|eapply intty_support;[exact Hc|left;exists o;split;assumption]|eapply floatty_support;[exact Hc|eassumption]]))
          as (m & Hm' & -> & Hml).
        split; [reflexivity|]. cbn [fst].
        eapply lt_eq; [eapply A.LitAddOp; [exact Hm' | exact Hml | exact Hv | exact L1] | lia | lia].
      - split; [reflexivity | exact L1]. }
    rewrite fold_succ in QR. replace (1 + (k - 1)) with k in QR by lia. exact QR. }
  stepok H acc s2 Hacc Hi1 Hi2 Fs2. post_rest H Hi2 Hp Hp2.
  destruct (Small (digit_val d0) s1 acc s2 (digit_pos d0 (Dig d0 (or_introl eq_refl)) Hd0) Hacc Hi1 ltac:(cache_inv) ltac:(fwd_inv) Hp2) as [Ea La].
  destruct acc as [xa ta]. cbn [fst snd] in Ea, La. subst ta.
  assert (Val : A.digits_val ds 0 = A.digits_val rest (digit_val d0))
    by (rewrite <- strip_zeros_val, Es; reflexivity).
  rewrite Val.
  destruct rest as [|d1 rest'].
  { bi H as ten s3 Hten. injection Hten as <- <-. cbn in H. injection H as <- <- <-.
    split; [left; exists o; auto|]. split; [exact La | exact WT]. }
  stepok H ten s3 Hten Hi2 Hi3 Fs3. post_rest H Hi3 Hp Hp3.
  destruct (Small 10 s2 ten s3 ltac:(lia) Hten Hi2 ltac:(cache_inv) ltac:(fwd_inv) Hp3) as [Et Lt]. destruct ten as [xt tt0]. cbn [fst snd] in Et, Lt. subst tt0.
  set (Q := fun v (a : xexpr * ety) => snd a = T /\ LT C ((fun t => trF (canon e t)) T) (fst a) v 1).
  assert (QR : Q (fold_left dstep (d1 :: rest') (digit_val d0)) (x, t)).
  { refine (mfold_val w P R Fl _ _ _ Q dstep F (d1 :: rest') (xa, T) (digit_val d0) s3 (x, t) s' H Hi3 ltac:(cache_inv) ltac:(fwd_inv) Hp _ _ _).
    - intros a0 d. split;[okIt|fwt].
    - intros [xa' ta] d s4 a' s5 v Hin [Eta Hv] Hi4 Icb Fcb Hs Hp5. cbn in Eta. subst ta. cbn beta in Hs.
      stepok Hs a1 s6 H1 Hi4 Hi6 Fs6. post_rest Hs Hi6 Hp5 Hp6.
      destruct a1 as [x1 t1].
      destruct (lang_call_step f IH c e xa' T xt "mul" why s4 x1 t1 s6 F C H1 ltac:(cbn; auto) Hi4 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp6 ltac:(assumption) ltac:(first[assumption|eapply intty_support;[exact Hc|left;exists o;split;assumption]|eapply floatty_support;[exact Hc|eassumption]]))
        as (m1 & Hm1 & -> & Hml1).
      assert (L2 : LT C ((fun t => trF (canon e t)) T) x1 (v * 10) (1 * 1)) by (eapply A.LitMulOp; [exact Hm1 | exact Hml1 | exact Hv | exact Lt]).
      destruct (Ascii.eqb d "0"%char) eqn:Ed.
      + injection Hs as <- _. apply Ascii.eqb_eq in Ed. subst d.
        split; [reflexivity|]. eapply lt_eq; [exact L2 | unfold dstep; cbn; lia | lia].
      + stepok Hs sd s7 H2 Hi6 Hi7 Fs7. post_rest Hs Hi7 Hp5 Hp7.
        assert (Hne : d <> "0"%char) by (intro E; subst d; discriminate Ed).
        destruct (Small (digit_val d) s6 sd s7 (digit_pos d (Dig d (or_intror Hin)) Hne) H2 Hi6 ltac:(cache_inv) ltac:(fwd_inv) Hp7) as [Es7 Ls].
        destruct sd as [xs ts]. cbn [fst snd] in Es7, Ls. subst ts. destruct a' as [x' t'].
        destruct (lang_call_step f IH c e x1 T xs "add" why s7 x' t' s5 F C Hs ltac:(cbn; auto) Hi7 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp5 ltac:(assumption) ltac:(first[assumption|eapply intty_support;[exact Hc|left;exists o;split;assumption]|eapply floatty_support;[exact Hc|eassumption]]))
          as (m2 & Hm2 & -> & Hml2).
        split; [reflexivity|]. cbn [fst].
        eapply lt_eq; [eapply A.LitAddOp; [exact Hm2 | exact Hml2 | exact L2 | exact Ls] | unfold dstep, digit_val; lia | lia].
    - split; [reflexivity | exact La]. }
  rewrite <- digits_val_fold in QR. destruct QR as [Et' Lq]. cbn [fst snd] in Et', Lq. subst t.
  split; [left; exists o; auto|]. split; [exact Lq | exact WT].
Qed.

Lemma lang_symbol_names : forall short n why mi s g s', lang_symbol short n false why mi s = Good (g, s') ->
  Inv s -> Later w P R Fl s ->
  s' = s /\ exists li, sassoc short (lang_modules w) = Some li /\ sassoc n (gm_names (get_mod w li)) = Some (gs_id g).
Proof.
  intros short n why mi s g s' H Hi L. unfold lang_symbol in H.
  bi H as lm s1 Hl. unfold lang in Hl. bi Hl as s0 s2 Hg. injection Hg as <- <-. injection Hl as <- <-.
  rewrite (lt_lang _ _ _ _ _ L) in H.
  destruct (sassoc short (lang_modules w)) as [li|] eqn:El; [|nogood H].
  bi H as m s3 Hm. unfold the_mod in Hm. injection Hm as <- <-. rewrite (lt_w _ _ _ _ _ L) in H.
  destruct (sassoc n (gm_names (get_mod w li))) as [x|] eqn:Ex; [|nogood H].
  unfold sym in H. destruct (nget (st_syms s) x) as [g0|] eqn:Eg; [|discriminate]. injection H as <- <-.
  split; [reflexivity|]. exists li. split; [reflexivity|]. rewrite (iv_ids _ Hi x g0 Eg). exact Ex.
Qed.

Lemma check_args_ab_info : forall f c e nm sg v t s xs s', check_args f c e nm sg [AB v t] s = Good (xs, s') -> exists pn pty,sg_params sg=[(pn,pty)] /\fits e t pty=true /\xs=[v].
Proof.
  intros f c e nm sg v t s xs s' H. destruct f as [|f]; [discriminate H|]. cbn [check_args] in H.
  destruct (negb (Nat.eqb (List.length [AB v t]) (List.length (sg_params sg)))) eqn:E; [nogood H|].
  apply negb_false_iff, Nat.eqb_eq in E.
  destruct (sg_params sg) as [|[pn pty] [|p2 ps]] eqn:Eps; cbn in E; try discriminate. cbn in H.
  bi H as y s1 H1. bi H1 as r s2 Hs. destruct r as [v1 t1].
  destruct f as [|f]; [discriminate Hs|]. cbn [synth_arg] in Hs. bi Hs as u s3 Hu. injection Hs as <- <- <-.
  destruct (fits e t pty)eqn:Fit; [injection H1 as <- <- | nogood H1].
  cbn in H. injection H as <- <-. exists pn,pty. auto.
Qed.

Lemma check_args_ab : forall f c e nm sg v t s xs s',check_args f c e nm sg [AB v t]s=Good(xs,s')->xs=[v].
Proof. intros. destruct(check_args_ab_info _ _ _ _ _ _ _ _ _ _ H)as(pn &pty &Ep &Ef &Ex). exact Ex. Qed.

(* float.moss's `float`, as the elaborator reads it: one type *)
Definition FloatTy (e : env) (t : ety) : Prop :=
  exists li x sg, sassoc "float" (lang_modules w) = Some li /\ sassoc "float" (gm_names (get_mod w li)) = Some x /\
    nget (en_fns e) x = Some sg /\ t = sg_ret sg.

Lemma floatty_support : forall c e F C t,CxRel w R Fl c e F C ->FloatTy e t ->Support WW t.
Proof.
 intros c e F C t Hc(li & x & sg & A1 & B1 & C1 & ->).
 exact(proj1(proj2(@es_fns WW e(cr_support _ _ _ _ _ _ _ Hc)x sg C1))).
Qed.

Lemma floatty_fun : forall e t1 t2, FloatTy e t1 -> FloatTy e t2 -> t1 = t2.
Proof. intros e t1 t2 (l1 & x1 & g1 & A1 & B1 & C1 & ->) (l2 & x2 & g2 & A2 & B2 & C2 & ->). congruence. Qed.

Lemma float_of_step : forall f, OK w P R Fl f -> forall c e x0 t0 why s x t s' F C,
  float_of f c e (x0, t0) why s = Good ((x, t), s') -> IntTy e t0 ->
  Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->
  FloatTy e t /\ WFty t /\ forall a b0, LT C ((fun t => trF (canon e t)) t0) x0 a b0 -> LT C ((fun t => trF (canon e t)) t) x a b0.
Proof.
  intros f IH c e x0 t0 why s x t s' F C H Hty Hi Ii Ifwd Hc Hp.
  pose proof (proj2 (ok_float_of f c e (x0, t0) why s _ s' Hi H)) as Fs.
  destruct (post_back w P R Fl _ _ F Fs Hp) as [L _].
  destruct f as [|f]; [discriminate H|]. cbn [float_of] in H.
  bi H as g s1 Hg. destruct (lang_symbol_names _ _ _ _ _ _ _ Hg Hi L) as [-> (li & Eli & Ex)].
  destruct (nget (en_fns e) (gs_id g)) as [sg|] eqn:Ef; [|nogood H].
  bi H as xs s2 Hxs. injection H as <- <- <-. destruct(check_args_ab_info _ _ _ _ _ _ _ _ _ _ Hxs)as(pn &tp &Eps &Fit &Exs). rewrite Exs.
  pose proof (Hlang "float" false "float" li (gs_id g) Eli Ex) as Hls.
  pose proof(cr_fns _ _ _ _ _ _ _ Hc(gs_id g))as Hn. rewrite Ef in Hn. cbn[option_map]in Hn.
  assert(Wr:WFty(sg_ret sg))by(apply ety_ok_wf;exact(proj1(proj2(cr_fns_clean _ _ _ _ _ _ _ Hc _ _ Ef)))).
  split; [exists li, (gs_id g), sg; auto|]. split; [exact Wr|].
  intros a b0 Hx. eapply A.LitFloat with(tp:=trF(canon e tp));
    [exact Hls| |exact(fits_tr Fl e t0 tp(cr_uf _ _ _ _ _ _ _ Hc)Fit)|exact Hx].
  rewrite Hn,Eps. reflexivity.
Qed.

Lemma ten_int : forall f, OK w P R Fl f -> forall c e why s x t s' F C,
  int_literal f c e ["1"%char; "0"%char] why s = Good ((x, t), s') ->
  Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F -> IntTy e t /\ LT C ((fun t => trF (canon e t)) t) x 10 1 /\ WFty t.
Proof.
  intros f IH c e why s x t s' F C H Hi Ii Ifwd Hc Hp.
  exact (int_lit_step f IH c e ["1"%char; "0"%char] why s x t s' F C H eq_refl Hi ltac:(cache_inv) ltac:(fwd_inv) Hc Hp).
Qed.

(* the mantissa: its digits' number, as a float *)
Lemma float_lit_step : forall f, OK w P R Fl f -> forall c e why whole frac ex s x t s' F C,
  float_lit_m f c e why whole frac ex s = Good ((x, t), s') ->
  forallb is_digit_char (whole ++ opt_default [] frac) = true ->
  Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->
  FloatTy e t /\ WFty t /\
  LT C ((fun t => trF (canon e t)) t) x (fst (lv (A.digits_val (whole ++ opt_default [] frac) 0) (fst (exp_of ex)) (snd (exp_of ex))
                         (List.length (opt_default [] frac))))
                 (snd (lv (A.digits_val (whole ++ opt_default [] frac) 0) (fst (exp_of ex)) (snd (exp_of ex))
                         (List.length (opt_default [] frac)))).
Proof.
  intros f IH c e why whole frac ex s x t s' F C H Hd Hi Ii Ifwd Hc Hp.
  unfold float_lit_m in H. cbv zeta in H.
  change (match ex with Some l => parse_exp l | None => (false, 0) end) with (exp_of ex) in H.
  destruct (exp_of ex) as [eneg emag] eqn:Eexp. cbn [fst snd].
  set (fr := opt_default [] frac) in *.
  destruct (if eneg then (true, emag + List.length fr) else if Nat.leb (List.length fr) emag then (false, emag - List.length fr)
            else (true, List.length fr - emag)) as [sneg smag] eqn:Esc.
  pose proof (lv_cases (A.digits_val (whole ++ fr) 0) eneg emag (List.length fr) sneg smag Esc) as (C0 & Cn & Cp).
  stepok H mant s1 Hm Hi Hi1 Fs1. post_rest H Hi1 Hp Hp1.
  (* the mantissa *)
  assert (M : FloatTy e (snd mant) /\ WFty (snd mant) /\
              LT C ((fun t => trF (canon e t)) (snd mant)) (fst mant) (A.digits_val (whole ++ fr) 0) 1).
  { destruct (strip_zeros (whole ++ fr)) as [|d0 rest] eqn:Ed.
    - stepok Hm z s2 Hz Hi Hi2 Fs2. post_rest Hm Hi2 Hp1 Hp2. destruct z as [xz tz].
      destruct (int_lit_step f IH c e ["0"%char] why s xz tz s2 F C Hz eq_refl Hi ltac:(cache_inv) ltac:(fwd_inv) Hc Hp2) as (Tz & Lz & Wz).
      destruct mant as [xm tm].
      destruct (float_of_step f IH c e xz tz why s2 xm tm s1 F C Hm Tz Hi2 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp1) as (Fm & Wm & Km).
      split; [exact Fm|]. split; [exact Wm|]. rewrite (strip_zeros_nil _ Ed). exact (Km _ _ Lz).
    - destruct (strip_zeros_head _ d0 rest Ed) as [Hd0 Hincl].
      assert (Dig : forall d, In d (d0 :: rest) -> is_digit_char d = true)
        by (intros d Hin; rewrite forallb_forall in Hd; exact (Hd d (Hincl d Hin))).
      stepok Hm i10 s2 H10 Hi Hi2 Fs2. post_rest Hm Hi2 Hp1 Hp2. destruct i10 as [x10 t10].
      destruct (ten_int f IH c e why s x10 t10 s2 F C H10 Hi ltac:(cache_inv) ltac:(fwd_inv) Hc Hp2) as (T10 & L10 & W10).
      stepok Hm ten s3 Hten Hi2 Hi3 Fs3. post_rest Hm Hi3 Hp1 Hp3. destruct ten as [xt tf].
      destruct (float_of_step f IH c e x10 t10 why s2 xt tf s3 F C Hten T10 Hi2 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp3) as (Ftf & Wtf & Kt).
      pose proof (Kt _ _ L10) as Lt.
      stepok Hm i0 s4 H0 Hi3 Hi4 Fs4. post_rest Hm Hi4 Hp1 Hp4. destruct i0 as [x0 t0].
      destruct (int_lit_step f IH c e [d0] why s3 x0 t0 s4 F C H0 ltac:(cbn; rewrite (Dig d0 (or_introl eq_refl)); reflexivity) Hi3 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp4)
        as (T0 & L0 & W0).
      stepok Hm m0 s5 Hm0 Hi4 Hi5 Fs5. post_rest Hm Hi5 Hp1 Hp5. destruct m0 as [xm0 tm0].
      destruct (float_of_step f IH c e x0 t0 why s4 xm0 tm0 s5 F C Hm0 T0 Hi4 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp5) as (Fm0 & Wm0 & Km0).
      pose proof (floatty_fun e tm0 tf Fm0 Ftf) as Etm; subst tm0. pose proof (Km0 _ _ L0) as Lm0.
      set (Q := fun v (a : xexpr * ety) => snd a = tf /\ LT C ((fun t => trF (canon e t)) tf) (fst a) v 1).
      assert (QR : Q (fold_left dstep rest (A.digits_val [d0] 0)) mant).
      { refine (mfold_val w P R Fl _ _ _ Q dstep F rest (xm0, tf) (A.digits_val [d0] 0) s5 mant s1 Hm Hi5 ltac:(cache_inv) ltac:(fwd_inv) Hp1 _ _ _).
        - intros a0 d. split;[okIt|fwt].
        - intros [xa ta] d s6 a' s7 v Hin [Eta Hv] Hi6 Icb Fcb Hs Hp7. cbn in Eta. subst ta. cbn beta in Hs.
          stepok Hs a1 s8 H1 Hi6 Hi8 Fs8. post_rest Hs Hi8 Hp7 Hp8. destruct a1 as [x1 t1].
          destruct (lang_call_step f IH c e xa tf xt "mul" why s6 x1 t1 s8 F C H1 ltac:(cbn; auto) Hi6 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp8 ltac:(assumption) ltac:(first[assumption|eapply intty_support;[exact Hc|left;exists o;split;assumption]|eapply floatty_support;[exact Hc|eassumption]]))
            as (m1 & Hm1 & -> & Hml1).
          assert (L2 : LT C ((fun t => trF (canon e t)) tf) x1 (v * 10) (1 * 1))
            by (eapply A.LitMulOp; [exact Hm1 | exact Hml1 | exact Hv | exact Lt]).
          destruct (Ascii.eqb d "0"%char) eqn:Ed0.
          + injection Hs as <- _. apply Ascii.eqb_eq in Ed0. subst d.
            split; [reflexivity|]. eapply lt_eq; [exact L2 | unfold dstep; cbn; lia | lia].
          + stepok Hs id s9 H9 Hi8 Hi9 Fs9. post_rest Hs Hi9 Hp7 Hp9. destruct id as [xi ti].
            destruct (int_lit_step f IH c e [d] why s8 xi ti s9 F C H9 ltac:(cbn; rewrite (Dig d (or_intror Hin)); reflexivity) Hi8 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp9)
              as (Ti & Li & Wi).
            stepok Hs fd s10 H10' Hi9 Hi10 Fs10. post_rest Hs Hi10 Hp7 Hp10. destruct fd as [xf tf'].
            destruct (float_of_step f IH c e xi ti why s9 xf tf' s10 F C H10' Ti Hi9 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp10) as (Ff & Wf & Kf).
            pose proof (floatty_fun e tf' tf Ff Ftf) as Etf'; subst tf'. pose proof (Kf _ _ Li) as Lf.
            destruct a' as [x' t'].
            destruct (lang_call_step f IH c e x1 tf xf "add" why s10 x' t' s7 F C Hs ltac:(cbn; auto) Hi10 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp7 ltac:(assumption) ltac:(first[assumption|eapply intty_support;[exact Hc|left;exists o;split;assumption]|eapply floatty_support;[exact Hc|eassumption]]))
              as (m2 & Hm2 & -> & Hml2).
            split; [reflexivity|]. cbn [fst].
            eapply lt_eq; [eapply A.LitAddOp; [exact Hm2 | exact Hml2 | exact L2 | exact Lf] | unfold dstep; cbn; lia | lia].
        - split; [reflexivity | exact Lm0]. }
      rewrite <- digits_val_fold in QR. destruct QR as [Em Lq].
      split; [rewrite Em; exact Ftf|]. split; [rewrite Em; exact Wtf|]. rewrite Em.
      rewrite <- strip_zeros_val, Ed. exact Lq. }
  destruct mant as [xm tm]. cbn [fst snd] in M. destruct M as (FT & WT & LM).
  destruct smag as [|k].
  { injection H as <- <-. split; [exact FT|]. split; [exact WT|]. rewrite (C0 eq_refl). exact LM. }
  stepok H i10 s2 H10 Hi1 Hi2 Fs2. post_rest H Hi2 Hp Hp2. destruct i10 as [x10 t10].
  destruct (ten_int f IH c e why s1 x10 t10 s2 F C H10 Hi1 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp2) as (T10 & L10 & W10).
  stepok H ten s3 Hten Hi2 Hi3 Fs3. post_rest H Hi3 Hp Hp3. destruct ten as [xt tf].
  destruct (float_of_step f IH c e x10 t10 why s2 xt tf s3 F C Hten T10 Hi2 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp3) as (Ftf & Wtf & Kt).
  pose proof (floatty_fun e tf tm Ftf FT) as Etf; subst tf. pose proof (Kt _ _ L10) as Lt.
  stepok H sc s4 Hsc Hi3 Hi4 Fs4. post_rest H Hi4 Hp Hp4.
  set (Q := fun v (a : xexpr * ety) => snd a = tm /\ LT C ((fun t => trF (canon e t)) tm) (fst a) v 1).
  assert (QS : Q (fold_left (fun n (_ : unit) => n * 10) (repeat tt (S k - 1)) 10) sc).
  { refine (mfold_val w P R Fl _ _ _ Q (fun n _ => n * 10) F (repeat tt (S k - 1)) (xt, tm) 10 s3 sc s4 Hsc Hi3 ltac:(cache_inv) ltac:(fwd_inv) Hp4 _ _ _).
    - intros a0 u. split;[apply okI_lang_call|fwt].
    - intros [xa ta] u s5 [x' t'] s6 v _ [Eta Hv] Hi5 Icb Fcb Hs Hp6. cbn in Eta. subst ta.
      destruct (lang_call_step f IH c e xa tm xt "mul" why s5 x' t' s6 F C Hs ltac:(cbn; auto) Hi5 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp6 ltac:(assumption) ltac:(first[assumption|eapply intty_support;[exact Hc|left;exists o;split;assumption]|eapply floatty_support;[exact Hc|eassumption]]))
        as (m & Hm' & -> & Hml).
      split; [reflexivity|]. cbn [fst].
      eapply lt_eq; [eapply A.LitMulOp; [exact Hm' | exact Hml | exact Hv | exact Lt] | lia | lia].
    - split; [reflexivity | exact Lt]. }
  rewrite fold_ten in QS. replace (10 * 10 ^ (S k - 1)) with (10 ^ S k) in QS
    by (cbn [Nat.pow]; replace (S k - 1) with k by lia; reflexivity).
  destruct sc as [xs ts]. destruct QS as [Ets Ls]. cbn [fst snd] in Ets, Ls. subst ts.
  destruct sneg.
  - destruct (lang_call_step f IH c e xm tm xs "div" why s4 x t s' F C H ltac:(cbn; auto) Hi4 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp ltac:(assumption) ltac:(first[assumption|eapply intty_support;[exact Hc|left;exists o;split;assumption]|eapply floatty_support;[exact Hc|eassumption]]))
      as (m & Hm' & -> & Hml).
    split; [exact FT|]. split; [exact WT|]. rewrite (Cn ltac:(discriminate) eq_refl).
    eapply A.LitDivOp; [exact Hm' | exact Hml | exact LM | exact Ls | apply Nat.pow_nonzero; discriminate].
  - destruct (lang_call_step f IH c e xm tm xs "mul" why s4 x t s' F C H ltac:(cbn; auto) Hi4 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp ltac:(assumption) ltac:(first[assumption|eapply intty_support;[exact Hc|left;exists o;split;assumption]|eapply floatty_support;[exact Hc|eassumption]]))
      as (m & Hm' & -> & Hml).
    split; [exact FT|]. split; [exact WT|]. rewrite (Cp ltac:(discriminate) eq_refl).
    eapply A.LitMulOp; [exact Hm' | exact Hml | exact LM | exact Ls].
Qed.

Lemma float_lit_text : forall text mant ex whole frac,
  existsb (fun c => Ascii.eqb c "E"%char) (list_ascii_of_string text) = false ->
  split_at_e (list_ascii_of_string text) [] = (mant, ex) -> split_at_dot mant [] = (whole, frac) ->
  (ex <> None \/ frac <> None) -> A.is_float_lit text = true.
Proof.
  intros text mant ex whole frac Hn Ee Ed Hs. unfold A.is_float_lit. apply existsb_exists.
  rewrite (split_at_e_eq _ _ Hn) in Ee. rewrite split_at_dot_eq in Ed.
  destruct Hs as [Hs|Hs].
  - destruct ex as [l|]; [|contradiction]. exists "e"%char. split; [exact (split_some_in _ _ _ _ _ Ee) | reflexivity].
  - destruct frac as [fr|]; [|contradiction]. exists "."%char. split; [|reflexivity].
    apply (split_fst_incl _ _ _ _ _ Ee). exact (split_some_in _ _ _ _ _ Ed).
Qed.

(* the literal's term denotes lit_value of its text, at one's, zero's or a
   float's type *)

Lemma lit_core : forall f, OK w P R Fl f -> forall c e text s x t s' F C,
  synth_literal f c e text s = Good ((x, t), s') ->
  Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->
  ((exists o, A.LangSym WW "int" A.NSName "one" o /\ A.nlook o (A.c_vals C) = Some ((fun t => trF (canon e t)) t)) \/
   (exists z, A.LangSym WW "int" A.NSName "zero" z /\ A.nlook z (A.c_vals C) = Some ((fun t => trF (canon e t)) t)) \/
   A.is_float_lit text = true) /\
  LT C ((fun t => trF (canon e t)) t) x (fst (A.lit_value text)) (snd (A.lit_value text)) /\ WFty t /\Support WW t.
Proof.
  intros f IH0 c e text s x t s' F C H Hi Ii Ifwd Hc Hp.
  destruct f as [|f]; [discriminate H|]. pose proof (ok_down w P R Fl f IH0) as IH.
  cbn [synth_literal] in H.
  destruct (lit_wf text) eqn:Ew; cbn [negb] in H; [|nogood H].
  unfold lit_wf in Ew. cbv zeta in Ew.
  apply andb_true_iff in Ew as [Hne Ew]. apply negb_true_iff in Hne.
  destruct (split_at_e (list_ascii_of_string text) []) as [mant ex] eqn:Ese.
  destruct (split_at_dot mant []) as [whole frac] eqn:Esd.
  apply andb_true_iff in Ew as [Ew Hex]. apply andb_true_iff in Ew as [Hwh Hfr].
  rewrite (lit_value_lv text mant ex whole frac Hne Ese Esd).
  assert (Dw : forallb is_digit_char (whole ++ opt_default [] frac) = true).
  { rewrite forallb_app. apply andb_true_iff. split.
    - destruct whole; [discriminate | exact Hwh].
    - destruct frac as [[|c0 fr]|]; [discriminate | exact Hfr | reflexivity]. }
  destruct ex as [l|]; [|destruct frac as [fr|]].
  - assert (Hf : float_lit_m f c e (String.append "the literal `" (String.append text "`")) whole frac (Some l) s = Good ((x, t), s'))
      by exact H.
    destruct (float_lit_step f IH c e _ whole frac (Some l) s x t s' F C Hf Dw Hi Ii Ifwd Hc Hp) as (FT & WT & L).
    assert (Hs : Some l <> None \/ frac <> None) by (left; discriminate).
    split; [right; right; exact (float_lit_text text mant (Some l) whole frac Hne Ese Esd Hs)|].
    split;[exact L|split;[exact WT|exact(floatty_support c e F C t Hc FT)]].
  - assert (Hf : float_lit_m f c e (String.append "the literal `" (String.append text "`")) whole (Some fr) None s = Good ((x, t), s'))
      by exact H.
    destruct (float_lit_step f IH c e _ whole (Some fr) None s x t s' F C Hf Dw Hi Ii Ifwd Hc Hp) as (FT & WT & L).
    assert (Hs : None <> @None (list ascii) \/ Some fr <> None) by (right; discriminate).
    split; [right; right; exact (float_lit_text text mant None whole (Some fr) Hne Ese Esd Hs)|].
    split;[exact L|split;[exact WT|exact(floatty_support c e F C t Hc FT)]].
  - rewrite split_at_dot_eq in Esd. pose proof (split_none _ _ _ _ Esd) as Em. cbn in Em. subst whole.
    cbn [opt_default] in Dw. rewrite app_nil_r in Dw.
    destruct (int_lit_step f IH c e mant _ s x t s' F C H Dw Hi ltac:(cache_inv) ltac:(fwd_inv) Hc Hp) as (Ty & L & Wt).
    pose proof(intty_support c e F C t Hc Ty)as St.
    split.
    + destruct Ty as [(o & Ho & Ev)|(o & Ho & Ev)]; [left|right; left]; exists o; split; try exact Ho;
        rewrite (cr_vals _ _ _ _ _ _ _ Hc), Ev; reflexivity.
    + split;[|split;[exact Wt|exact St]]. eapply lt_eq; [exact L | | ]; cbn [exp_of opt_default fst snd lv Nat.leb]; rewrite ?app_nil_r; cbn; lia.
Qed.

Lemma lit_step : forall f, OK w P R Fl f -> forall c e text exp s v t s',
  synth (S f) c e (AELit text) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> opt_wf exp ->opt_support WW exp -> Post w P R Fl s' F ->
  A.Ex WW P F C (AELit text) (option_map (fun t => trF (canon e t)) exp) v ((fun t => trF (canon e t)) t) /\ WFty t /\Support WW t.
Proof.
  intros f IH c e text exp s v t s' H F C Hi Ii Ifwd Hc Hx HexpS Hp.
  cbn [synth] in H. destruct (synth_wrap e exp s v t s' _ H (ok_synth_literal f c e text) Hi) as (s1 & H1 & Hi1 & Fs1 & Fs1').
  pose proof (post_back w P R Fl _ _ F Fs1' Hp) as Hp1.
  destruct (lit_core f IH c e text s v t s1 F C H1 Hi Ii Ifwd Hc Hp1) as (Ty & L & Wt & St).
  split;[|split;[exact Wt|exact St]]. eapply A.ExLit; [exact Ty | apply surjective_pairing | exact L].
Qed.

End Lit.
