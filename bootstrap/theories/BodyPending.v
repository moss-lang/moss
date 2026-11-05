(* ------------------------------------------------------------------------ *)
(* BodyPending.v — BodySound.Pending from the construct lemmas (Track A,   *)
(* elaborate_sound step 2).                                                 *)
(*                                                                           *)
(* call_step is pd_synth for every AECall: a call the elaborator accepts   *)
(* names a tag (BodyTag.tag_step) or a fn (Hfn, the other half of Track A),*)
(* read from the loader as the relation's kind_sym does.  pending is       *)
(* Pending, assembled from BodyData (fields, records, match), BodyOps      *)
(* (operators), BodyLit (literals), BodyConst (defined vals) and call_step,*)
(* under the union of their section hypotheses and, for what no lemma of   *)
(* this branch covers yet, these (the other half of Track A):              *)
(*   Hfn    a call whose head resolves to a fn (kind_sym DKFn);             *)
(*   Hmeth  a method call (AEMeth);                                          *)
(*   Hbind  a bind statement (pd_stmt);                                      *)
(*   Hty    elab_type against TyOf (pd_ty).                                  *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabMono BodySound BodyData BodyOps
  BodyTag BodyConst BodyLit.
Import ListNotations.
Local Open Scope list_scope.
Local Open Scope string_scope.

Module A := Audit.Elaboration.

Local Strategy opaque [Nat.of_num_uint FUEL].

Tactic Notation "bi" hyp(H) "as" ident(a) ident(s1) ident(H1) :=
  apply bind_inv in H as (a & s1 & H1 & H).

Ltac nogood H := exfalso;
  solve [repeat (cbn beta iota zeta in H;
                 first [ exact (err_not_good _ _ _ _ _ _ H) | discriminate H
                       | apply bind_inv in H as (? & ? & ? & H) ])].

Section Pend.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation WW := (BodySound.W w R Fl).
Notation trF := (tr Fl).
Notation trx := (BodyData.trx Fl).
Variable b : bundle.
Hypothesis Hload : load_world b = Good w.

(* the section hypotheses of BodyData, BodyOps, BodyTag, BodyLit, BodyConst *)
Hypothesis HFl : forall a c, Fl a = Fl c -> a = c.
Hypothesis Hpayload : forall c e F C x xs s pay s',
  payload_type (YNom x xs) e s = Good (pay, s') -> Inv s -> st_w s = w ->
  CxRel w Fl c e F C -> Later w P R Fl s' ->
  (exists g, nget (w_syms w) x = Some g /\ gs_kind g = KTag) ->
  A.PayloadOf WW x (trx xs) (trF pay) /\ WFty pay.
Hypothesis Htagargs : forall c e F C g a s xs s', tag_args c e g a s = Good (xs, s') -> Inv s -> st_w s = w ->
  CxRel w Fl c e F C -> Later w P R Fl s' -> nget (w_syms w) (gs_id g) = Some g -> gs_kind g = KTag ->
  exists bs, A.Brackets WW (A.fr_mod F) (A.c_ty C) (gs_id g) a bs /\
    Forall2 (fun r ra => fst ra = r /\ A.nlook r (bs ++ A.c_ty C) = Some (snd ra)) (A.w_reqs WW (gs_id g)) (trx xs) /\
    Forall (fun p => WFty (snd p)) xs.
Hypothesis Hwasm : forall mi, In mi (w_order w) -> A.ends_with "lib/wasm.moss" (gm_path (get_mod w mi)) ->
  sassoc "wasm" (lang_modules w) = Some mi.
Hypothesis Hlang : forall (short : string) (det : bool) (n : string) (li x : nat), sassoc short (lang_modules w) = Some li ->
  sassoc n (if det then gm_detached (get_mod w li) else gm_names (get_mod w li)) = Some x ->
  A.LangSym WW short (if det then A.NSDetached else A.NSName) n x.
Hypothesis Hprovided : forall f, OK w P R Fl f -> forall c e ox oty m args s x t s',
  provided_call f c e ox oty m (map AA args) s = Good ((x, t), s') ->
  forall F C, Inv s -> CxRel w Fl c e F C -> Post w P R Fl s' F -> WFty oty ->
  A.Provided WW P F C ox (trF oty) (gs_id m) args x (trF t) /\ WFty t.
Hypothesis Hdirect : forall f, OK w P R Fl f -> forall c e ox oty m args s x t s' sg,
  provided_call f c e ox oty m (map (fun a => AB (fst a) (snd a)) args) s = Good ((x, t), s') ->
  get_mth e (canon e oty) (gs_id m) = Some sg ->
  forall F C, Inv s -> CxRel w Fl c e F C -> Post w P R Fl s' F ->
  x = XMeth ox (gs_id m) (A.rkey C (trF oty) (gs_id m)) (map fst args) /\ t = sg_ret sg /\
  A.mlook (trF oty) (gs_id m) (A.c_mths C) = Some (A.FSig (map (fun p => trF (snd p)) (sg_params sg)) (trF (sg_ret sg))).
Hypothesis Hnumsig : forall c e F C r mid sg n, CxRel w Fl c e F C -> get_mth e r mid = Some sg ->
  A.LangSym WW "num" A.NSDetached n mid -> In n ["add"; "mul"; "div"]%string ->
  map snd (sg_params sg) = [r] /\ sg_ret sg = r.
Hypothesis Hfloat : forall c e F C x sg, CxRel w Fl c e F C -> A.LangSym WW "float" A.NSName "float" x ->
  nget (en_fns e) x = Some sg ->
  A.nlook x (A.c_fns C) = Some (A.FSig (map (fun p => trF (snd p)) (sg_params sg)) (trF (sg_ret sg))) /\
  WFty (sg_ret sg) /\
  forall o t, (A.LangSym WW "int" A.NSName "one" o \/ A.LangSym WW "int" A.NSName "zero" o) ->
    nget (en_vals e) o = Some t -> map snd (sg_params sg) = [t].
Hypothesis Hty : forall ty c e s d s', ety_of ty (cx_mod c) (tymap e) e (cx_this c) s = Good (d, s') ->
  forall F C, Inv s -> CxRel w Fl c e F C -> Later w P R Fl s' ->
  A.TyOf WW (A.fr_mod F) (A.c_ty C) (A.fr_this F) ty (trF d) /\ WFty d.

(* [calls and methods, the other half of Track A] *)
Hypothesis Hfn : forall f, OK w P R Fl f -> forall c e pa a args exp s v t s',
  synth (S f) c e (AECall pa a args) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> CxRel w Fl c e F C -> opt_wf exp -> Post w P R Fl s' F ->
  (exists x, A.resolve_sym WW (A.fr_mod F) pa = Some x /\ A.kind_sym WW x = Some A.DKFn) ->
  A.Ex WW P F C (AECall pa a args) (option_map trF exp) v (trF t) /\ WFty t.
Hypothesis Hmeth : forall f, OK w P R Fl f -> forall c e o pa args exp s v t s',
  synth (S f) c e (AEMeth o pa args) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> CxRel w Fl c e F C -> opt_wf exp -> Post w P R Fl s' F ->
  A.Ex WW P F C (AEMeth o pa args) (option_map trF exp) v (trF t) /\ WFty t.
Hypothesis Hbind : forall f, OK w P R Fl f -> forall c e its s xs e1 c1 d s',
  check_stmt (S f) c e (ASBind its) s = Good ((xs, e1, c1, d), s') ->
  forall F C, Inv s -> CxRel w Fl c e F C -> Post w P R Fl s' F -> StmtConcl w P R Fl F C (ASBind its) xs c c1 e1 d.

(* A call the elaborator accepts names a tag or a fn. *)
Lemma call_head : forall f c e pa a args exp s v t s',
  synth (S f) c e (AECall pa a args) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> CxRel w Fl c e F C -> Post w P R Fl s' F ->
  exists x, A.resolve_sym WW (A.fr_mod F) pa = Some x /\
    (A.kind_sym WW x = Some A.DKTag \/ A.kind_sym WW x = Some A.DKFn).
Proof.
  intros f c e pa a args exp s v t s' H F C Hi Hc [Hl _].
  cbn [synth] in H. destruct (synth_wrap e exp s v t s' _ H (ok_synth_call f c e pa a args) Hi) as (s1 & H1 & _ & Fs1 & Fs1').
  assert (Hw : st_w s = w) by (rewrite <- (lt_w _ _ _ _ _ Hl); symmetry; exact (fr_w _ _ (fr_trans _ _ _ Fs1 Fs1'))).
  destruct f as [|f]; [discriminate H1|]. cbn [synth_call] in H1. bi H1 as r s2 Hr.
  destruct (rpath_rel w R Fl b Hload _ _ _ _ _ Hw (cr_mod_in _ _ _ _ _ _ Hc) Hr) as [-> Hres].
  destruct r as [[x|j]|]; [|nogood H1|nogood H1].
  cbn [option_map ent_of] in Hres. bi H1 as g s3 Hs.
  destruct (sym_loader w R Fl b Hload _ _ _ _ _ _ Hi Hw Hres Hs) as [-> Hg].
  exists x. split; [unfold A.resolve_sym; rewrite (cr_mod _ _ _ _ _ _ Hc), Hres; reflexivity|].
  unfold BodySound.W. rewrite (kind_sym_read b w R Fl Hload x g Hg).
  destruct (gs_kind g); try nogood H1; [left | right]; reflexivity.
Qed.

Lemma call_step : forall f, OK w P R Fl f -> forall c e pa a args exp s v t s',
  synth (S f) c e (AECall pa a args) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> CxRel w Fl c e F C -> opt_wf exp -> Post w P R Fl s' F ->
  A.Ex WW P F C (AECall pa a args) (option_map trF exp) v (trF t) /\ WFty t.
Proof.
  intros f IH c e pa a args exp s v t s' H F C Hi Hc Hx Hp.
  destruct (call_head f c e pa a args exp s v t s' H F C Hi Hc Hp) as (x & Hr & [Hk|Hk]).
  - eapply tag_step; try eassumption. exists x. exact (conj Hr Hk).
  - exact (Hfn f IH c e pa a args exp s v t s' H F C Hi Hc Hx Hp (ex_intro _ x (conj Hr Hk))).
Qed.

Theorem pending : Pending w P R Fl.
Proof.
  constructor.
  - intros f IH c e x exp s v t s' Hpe H F C Hi Hc Hx Hp.
    destruct x; try discriminate Hpe.
    + eapply call_step; eassumption.
    + eapply field_step; eassumption.
    + eapply Hmeth; eassumption.
    + eapply record_step; eassumption.
    + eapply match_step; eassumption.
    + eapply unary_step; eassumption.
    + eapply binary_step; eassumption.
    + eapply lit_step; eassumption.
  - intros f IH c e st0 s xs e1 c1 d s' Hpe H F C Hi Hc Hp.
    destruct st0; try discriminate Hpe. eapply Hbind; eassumption.
  - exact (const_step w P R Fl b Hload Hty).
  - exact Hty.
Qed.

End Pend.
