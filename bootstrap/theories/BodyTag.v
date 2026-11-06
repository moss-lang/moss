(* ------------------------------------------------------------------------ *)
(* BodyTag.v — a tag applied to its payload against V.6's ExTag (Track A,  *)
(* elaborate_sound step 2; BodySound.v's Pending, pd_synth for AECall at a *)
(* tag).                                                                    *)
(*                                                                           *)
(* tag_step is pd_synth for `T(y)` / `T[..](y)` where `T` resolves to a     *)
(* tag: from OK f to synth at f+1.  It asks of the call that its head       *)
(* resolve to a tag (the relation's kind_sym), so that, with the lemma for  *)
(* a call of a fn (the other half of Track A), AECall is covered.           *)
(*                                                                           *)
(* What this file assumes (section hypotheses):                             *)
(*   Hpayload, Htagargs  as BodyData.v's (the other half of Track A);       *)
(* Library identity and scope facts follow from the loader and selected    *)
(* prelude directory (LibraryFacts.v), including reexports.                 *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import LibraryFacts Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabMono BodySound BodyData Merge CanonTypes PatternSubst.
From NM Require Import InfoPreserve.
Import ListNotations.
Local Open Scope list_scope.

From NM Require Import TagArgsExec NominalSupport.

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
(* 1. Strings: in_library's directory                                        *)
(* ======================================================================== *)

Lemma prefix_app : forall d p, String.prefix d p = true -> exists r, p = (d ++ r)%string.
Proof.
  induction d as [|a d IH]; intros p H; [exists p; reflexivity|].
  destruct p as [|a' p]; cbn in H; [discriminate|].
  destruct (Ascii.ascii_dec a a') as [<-|]; [|discriminate].
  destruct (IH p H) as [r ->]. exists r. reflexivity.
Qed.

Lemma substring_app : forall pre suf, substring 0 (String.length pre) (pre ++ suf) = pre.
Proof. induction pre as [|a pre IH]; intro suf; [destruct suf; reflexivity | cbn; rewrite IH; reflexivity]. Qed.

Lemma length_app_s : forall a c, String.length (a ++ c) = String.length a + String.length c.
Proof. induction a as [|x a IH]; intro c; [reflexivity | cbn; rewrite IH; reflexivity]. Qed.

Lemma append_assoc_s : forall a c d, (a ++ (c ++ d))%string = ((a ++ c) ++ d)%string.
Proof. induction a as [|x a IH]; intros c d; [reflexivity | cbn; rewrite IH; reflexivity]. Qed.

(* in_library's directory of a `…lib/wasm.moss` is its `…lib/` *)
Lemma wasm_dir : forall pre,
  let lp := (pre ++ "lib/wasm.moss")%string in
  substring 0 (String.length lp - String.length "wasm.moss") lp = (pre ++ "lib/")%string /\
  lp = ((pre ++ "lib/") ++ "wasm.moss")%string.
Proof.
  intro pre. cbn zeta.
  assert (E : (pre ++ "lib/wasm.moss")%string = ((pre ++ "lib/") ++ "wasm.moss")%string)
    by (rewrite <- append_assoc_s; reflexivity).
  split; [|exact E]. rewrite E, length_app_s.
  replace (String.length (pre ++ "lib/") + String.length "wasm.moss" - String.length "wasm.moss")
    with (String.length (pre ++ "lib/")) by lia.
  apply substring_app.
Qed.

(* ======================================================================== *)
(* 2. The tag step                                                           *)
(* ======================================================================== *)

Section Loader.
Variables (w : Elab.world) (b : bundle).
Hypothesis Hload : load_world b = Good w.

(* a loader symbol's module is loaded, and files the symbol under its name *)
Lemma load_world_symfacts : forall x g, nget (w_syms w) x = Some g ->
  In (gs_mod g) (w_order w) /\
  forall n, A.filed_name (gs_decl g) = Some (A.NSName, n) -> sassoc n (gm_names (get_mod w (gs_mod g))) = Some x.
Proof.
  intros x g Hg. pose proof Hload as H. unfold load_world in H.
  apply rbind_good in H as ([e st] & Hl & H). injection H as <-.
  destruct (load_spec b _ _ _ _ _ _ Hl (linv_init b)) as (L & _).
  cbn [w_syms w_order w_mods] in *. split.
  - destruct (li_syms _ _ L x g Hg) as (_ & _ & _ & p & Hp). apply (li_order _ _ L). exists p. exact Hp.
  - destruct (li_symfacts _ _ L x g Hg) as (m & p & am & Hm & _ & _ & _ & _ & Hn & _).
    intros n En. unfold get_mod. cbn [w_mods]. rewrite Hm. exact (Hn n En).
Qed.

End Loader.

Section Tag.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation WW := (BodySound.W w R Fl).
Notation trF := (tr Fl).
Notation trx := (BodyData.trx Fl).
Variable b : bundle.
Hypothesis Hload : load_world b = Good w.

(* as BodyData.v's *)
Definition Hpayload := @BodyData.Hpayload w P R Fl b Hload.
Definition Htagargs := @BodyData.Htagargs w P R Fl b Hload.

(* A platform reference tag is selected by wasm's scope, including a
   deliberate reexport. Its declaration may live in another module. *)

Lemma platform_ref : forall x g, nget (w_syms w) x = Some g -> A.PlatformRefTag WW x ->
  exists wm, sassoc "wasm" (lang_modules w) = Some wm /\
  A.LibraryModule WW "wasm" (gm_path (get_mod w wm)) /\
  (sassoc "Ref" (gm_names (get_mod w wm)) = Some x \/
   sassoc "Array" (gm_names (get_mod w wm)) = Some x).
Proof.
  intros x g Hg [Hp|Hp].
  all: destruct Hp as (d & p & Hd & Hpath & Hscope).
  all: destruct (selected_scope w R Fl p A.NSName _ x Hpath Hscope) as (wm & Ew & Ep & Hn).
  all: exists wm; split; [exact Ew|].
  all: split; [rewrite <- Ep; exact Hpath|].
  all: cbn [tbl] in Hn; auto.
Qed.

Lemma is_platform_true : forall n g s bo s' wm, is_platform_sym n g s = Good (bo, s') -> Later w P R Fl s ->
  sassoc "wasm" (lang_modules w) = Some wm ->
  sassoc n (gm_names (get_mod w wm)) = Some (gs_id g) -> bo = true.
Proof.
  intros n g s bo s' wm H L Ew En. unfold is_platform_sym in H.
  bi H as lm s1 Hl. unfold lang in Hl. bi Hl as s0 s2 Hg. injection Hg as <- <-. injection Hl as <- <-.
  rewrite (lt_lang _ _ _ _ _ L), Ew in H.
  bi H as m s3 Hm. unfold the_mod in Hm. injection Hm as <- <-.
  rewrite (lt_w _ _ _ _ _ L), En, Nat.eqb_refl in H.
  injection H as <- _. reflexivity.
Qed.

Lemma in_library_true : forall mi s s' wm, in_library mi s = Good (true, s') -> Later w P R Fl s ->
  sassoc "wasm" (lang_modules w) = Some wm -> A.LibraryModule WW "wasm" (gm_path (get_mod w wm)) ->
  A.InLibrary WW (gm_path (get_mod w mi)).
Proof.
  intros mi s s' wm H L Ew Ep. unfold in_library in H.
  bi H as s0 s1 Hg. injection Hg as <- <-.
  rewrite (lt_w _ _ _ _ _ L) in H.
  destruct (Elab.w_library w) as [dir|] eqn:Edir; [|discriminate].
  injection H as Hpre _. destruct (prefix_app _ _ Hpre) as [rest Er].
  exists dir, rest. split; [exact Edir|exact Er].
Qed.

(* reporting a construct unsupported leaves no run the final state keeps *)
Lemma unsupported_later : forall why s u s', unsupported why s = Good (u, s') -> Later w P R Fl s' -> False.
Proof.
  intros why s u s' H L. unfold unsupported in H. injection H as _ <-.
  pose proof (lt_unsup _ _ _ _ _ L) as E. cbn [st_unsup] in E. destruct (st_unsup s); discriminate E.
Qed.

Lemma ok_is_platform_sym : forall n g, ok (is_platform_sym n g).
Proof. intros. unfold is_platform_sym, lang, the_mod. okt. Qed.

Lemma ok_in_library : forall mi, ok (in_library mi).
Proof. intros. unfold in_library, lang, the_mod. okt. Qed.

Lemma tag_step : forall f, OK w P R Fl f -> forall c e pa a args exp s v t s',
  synth (S f) c e (AECall pa a args) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> opt_wf exp ->opt_support WW exp -> Post w P R Fl s' F ->
  (exists x, A.resolve_sym WW (A.fr_mod F) pa = Some x /\ A.kind_sym WW x = Some A.DKTag) ->
  A.Ex WW P F C (AECall pa a args) (option_map (fun t => trF (canon e t)) exp) v (trF (canon e t)) /\ WFty t /\Support WW t.
Proof.
  intros f IH c e pa a args exp s v t s' H F C Hi Ii Ifwd Hc Hx HexpS Hp (x0 & Hrs & Hk).
  cbn [synth] in H. destruct (synth_wrap e exp s v t s' _ H (ok_synth_call f c e pa a args) Hi) as (s1 & H1 & Hi1 & Fs1 & Fs1').
  pose proof (post_back w P R Fl _ _ F Fs1' Hp) as Hp1. clear H.
  destruct f as [|f']; [discriminate H1|]. pose proof (ok_down w P R Fl f' IH) as IH'.
  cbn [synth_call] in H1.
  pose proof (cr_mod_in _ _ _ _ _ _ _ Hc) as Hmi. pose proof (cr_mod _ _ _ _ _ _ _ Hc) as Hm.
  assert (Hw : st_w s = w).
  { destruct Hp1 as [L _]. rewrite <- (lt_w _ _ _ _ _ L). symmetry. exact (fr_w _ _ Fs1). }
  bi H1 as r s2 Hr. destruct (rpath_rel w R Fl b Hload _ pa s r s2 Hw Hmi Hr) as [-> Hres].
  destruct r as [[x|j]|]; [|nogood H1|nogood H1].
  cbn [option_map ent_of] in Hres.
  bi H1 as g s3 Hs. destruct (sym_loader w R Fl b Hload _ _ x s g s3 Hi Hw Hres Hs) as [-> Hg].
  assert (Ex0 : x0 = x).
  { unfold A.resolve_sym in Hrs. rewrite Hm in Hrs. rewrite Hres in Hrs. injection Hrs as <-. reflexivity. }
  subst x0.
  assert (Kg : gs_kind g = KTag).
  { rewrite (kind_read w R Fl b Hload x g Hg) in Hk. destruct (gs_kind g); try discriminate Hk; reflexivity. }
  rewrite Kg in H1.
  pose proof (load_world_ids b w Hload x g Hg) as Eid.
  assert (Hg' : nget (w_syms w) (gs_id g) = Some g) by (rewrite Eid; exact Hg).
  bi H1 as tm s4 Htm. bi H1 as xs s5 Hxs.
  assert (Hta : tag_args c e g a s = Good (xs, s5)).
  { unfold tag_args, TagArgsCore.tag_args, mbind at 1.
    match goal with |- match ?X with _ => _ end = _ => replace X with (Good (tm, s4)) by (symmetry; exact Htm) end.
    exact Hxs. }
  clear Htm Hxs. okstep' Hta (ok_tag_args c e g a) Hi Hi5 Fs5.
  bi H1 as pay s6 Hpay. okstep' Hpay (ok_payload_type (YNom x xs) e) Hi5 Hi6 Fs6.
  destruct args as [|y [|y2 r2]]; [nogood H1| |nogood H1].
  bi H1 as r7 s7 Hy. destruct r7 as [v0 vt]. okstep' Hy (ok_synth f' c e y (Some pay)) Hi6 Hi7 Fs7.
  destruct (negb (fits e vt pay)) eqn:Efit; [nogood H1|]. apply negb_false_iff in Efit.
  bi H1 as th s12 Hth. okstep' Hth (ok_tag_theta e xs) Hi7 Hi12 Fs12. injection H1 as <- <- <-.
  pose proof (post_back w P R Fl _ _ F Fs12 Hp1) as Hp7.
  pose proof (post_back w P R Fl _ _ F Fs7 Hp7) as Hp6.
  pose proof (post_back w P R Fl _ _ F Fs6 Hp6) as Hp5.
  destruct (Htagargs c e F C g a s xs s5 Hta Hi Hw Hc (proj1 Hp5) Hg' Kg) as (bs & Hbr & Hreq & Wxs).
  rewrite Eid in Hbr, Hreq.
  pose proof(tag_args_support w R Fl b Hload _ _ _ _ _ _ _ _ _ Hta Hi Hw Hc(proj1 Hp5)Hg' Kg)as Snom.
  rewrite Eid in Snom.
  assert (Hw5 : st_w s5 = w) by (rewrite (fr_w _ _ Fs5); exact Hw).
  destruct (Hpayload c e F C x xs s5 pay s6 Hpay Hi5 Hw5 Hc (proj1 Hp6) (ex_intro _ g (conj Hg Kg))(wf_nom _ _ Wxs)Snom) as[Hpo[Wp Sp]].
  pose proof (payload_subst WW (suf Fl (en_uf e)) _ _ _ Hpo) as HpoC.
  rewrite BodyData.smap_trx, tr_canon in HpoC by exact (proj1 (cr_uf _ _ _ _ _ _ _ Hc)).
  clear Hpo. rename HpoC into Hpo.
  destruct (bs_synth _ _ _ _ _ IH' c e y (Some pay) s6 v0 vt s7 Hy F C Hi6 ltac:(cache_inv) ltac:(fwd_inv) Hc Wp Sp Hp7) as[Ey[Wy Sy]].
  rewrite (tag_theta_tr w P R Fl e xs s7 th s12 Hth Hi7 (proj1 Hp1)).
  split; [|split;[exact(wf_nom _ _ Wxs)|exact Snom]].
  rewrite tr_canon_nom.
  eapply A.ExTag with (bs := bs) (pay := trF (canon e pay)) (y := y);
    [unfold A.resolve_sym; rewrite Hm, Hres; reflexivity | rewrite (kind_read w R Fl b Hload x g Hg), Kg; reflexivity
    | exact Hbr | exact Hreq | exact Hpo | reflexivity | exact Ey | exact (fits_tr Fl e vt pay (cr_uf _ _ _ _ _ _ _ Hc) Efit)].

Qed.

End Tag.
