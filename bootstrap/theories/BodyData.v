(* ------------------------------------------------------------------------ *)
(* BodyData.v — records, tags, fields and `match` against V.6 (Track A,    *)
(* elaborate_sound step 2; BodySound.v's Pending, the data constructs).    *)
(*                                                                           *)
(* Each lemma here is BodySound.Pending's pd_synth for one construct: from  *)
(* the judgments at fuel f (OK f) to synth at f+1.  The helpers the        *)
(* elaborator uses for a construct (synth_field, synth_match, …) run at    *)
(* fuel f and call synth at f-1; ElabMono's fuel monotonicity gives OK f-1  *)
(* from OK f (ok_down).                                                     *)
(*                                                                           *)
(* What this file assumes (section hypotheses), each a type-level fact of  *)
(* the other half of Track A (elab_type against TyOf):                      *)
(*   Hpayload  payload_type of a tag's application is its PayloadOf;       *)
(* and of the final field numbering, which elaborate_frame provides:       *)
(*   HFl       Fl is injective (WorldOf's wo_fld).                          *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabMono BodySound Merge CanonTypes PatternSubst.
From NM Require CoreMoss.
From NM Require Export EtyTyExec.
From NM Require Import TagArgsCore TagArgsExec PayloadExec NominalSupport NominalMergeSupport SourceSupport.
From NM Require Import InfoPreserve.
Import ListNotations.
Local Open Scope list_scope.

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
(* 1. More fuel                                                              *)
(* ======================================================================== *)

Lemma synth_up : forall f c e x exp s r, synth f c e x exp s = Good r -> synth (S f) c e x exp s = Good r.
Proof. intros. exact ((proj1 (mono_group f)) c e x exp s r H). Qed.

Section Down.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).

(* the judgments at fuel f+1 give them at f *)
Lemma ok_down : forall f, OK w P R Fl (S f) -> OK w P R Fl f.
Proof.
  intros f [S1 B1 Ss1 St1 C1 I1 P1 K1].
  destruct (mono_group f) as (M0 & M1 & M2 & M3 & M4 & M5 & M6 & M7 & M8 & M9 & _).
  constructor.
  - intros c e x exp s v t s' H. exact (S1 c e x exp s v t s' (M0 _ _ _ _ _ _ H)).
  - intros c e bl exp s v t bt s' H. exact (B1 c e bl exp s v t bt s' (M2 _ _ _ _ _ _ H)).
  - intros c e ss s xs e1 c1 d s' H. exact (Ss1 c e ss s xs e1 c1 d s' (M3 _ _ _ _ _ H)).
  - intros c e st0 s xs e1 c1 d s' H. exact (St1 c e st0 s xs e1 c1 d s' (M4 _ _ _ _ _ H)).
  - intros c e y s v s' H. exact (C1 c e y s v s' (M5 _ _ _ _ _ H)).
  - intros c e x exp s v t s' H. exact (I1 c e x exp s v t s' (M6 _ _ _ _ _ _ H)).
  - intros c e p a s v t s' H. exact (P1 c e p a s v t s' (M9 _ _ _ _ _ _ H)).
  - intros g s v t s' H. exact (K1 g s v t s' (M8 _ _ _ H)).
Qed.

End Down.

Definition mmap_pure := @EtyTyExec.mmap_pure.

(* ======================================================================== *)
(* 2. The data constructs                                                    *)
(* ======================================================================== *)

Section Data.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation WW := (BodySound.W w R Fl).
Notation trF := (tr Fl).
Variable b : bundle.
Hypothesis Hload : load_world b = Good w.

Definition trx (xs : list (nat * ety)) : list (nat * ty) := map (fun p => (fst p, trF (snd p))) xs.

(* Field numbers: distinct names, distinct numbers (WorldOf's wo_fld; the
   elaborator's final table is injective, ElabFrame.elab_frame). *)
Definition trxC (e : env) (xs : list (nat * ety)) : list (nat * ty) :=
  map (fun p => (fst p, trF (canon e (snd p)))) xs.

Hypothesis HFl : forall a c, Fl a = Fl c -> a = c.

(* [type lemma, the other half of Track A] the payload of a tag's
   application, as payload_type computes it, is PayloadOf's. *)
Lemma Hpayload : forall c e F C x xs s pay s',
 payload_type(YNom x xs)e s=Good(pay,s') -> Inv s ->st_w s=w ->
 CxRel w R Fl c e F C ->Later w P R Fl s' ->
 (exists g,nget(w_syms w)x=Some g /\gs_kind g=KTag) ->
 WFty(YNom x xs) ->Support WW(YNom x xs) ->
 A.PayloadOf WW x(trx xs)(trF pay) /\ WFty pay /\ Support WW pay.
Proof.
 intros c e F C x xs s pay s' E Hi Hw Hc L Hg Wt St.
 exact(payload_type_supported_all w R Fl b Hload _ _ _ _ _ _ _ _ _ E Hi Hw Hc L
   (cr_support _ _ _ _ _ _ _ Hc)St Wt Hg).
Qed.


Lemma smap_trx : forall e xs, Tri (en_uf e) -> TyOfSubst.smap (suf Fl (en_uf e)) (trx xs) = trxC e xs.
Proof.
  intros e xs T. unfold TyOfSubst.smap, trx, trxC. rewrite map_map. apply map_ext. intros [k y]. cbn [fst snd]. rewrite tr_canon by exact T. reflexivity.
Qed.

(* a tag or unit the elaborator reads is the loader's (Inv's iv_kind) *)
Lemma sym_nominal : forall s x g s', Inv s -> st_w s = w -> sym x s = Good (g, s') ->
  (gs_kind g = KTag \/ gs_kind g = KUnit) -> s' = s /\ nget (w_syms w) x = Some g.
Proof.
  intros s x g s' Hi Hw H Hk. unfold sym in H. destruct (nget (st_syms s) x) as [g0|] eqn:Ex; [|discriminate].
  injection H as <- <-. split; [reflexivity|].
  destruct (nget (w_syms w) x) as [g1|] eqn:Ew.
  - pose proof (iv_loader _ Hi b ltac:(rewrite Hw; exact Hload) x g1 ltac:(rewrite Hw; exact Ew)) as E.
    rewrite E in Ex. exact Ex.
  - destruct (iv_kind _ Hi b ltac:(rewrite Hw; exact Hload) x g0 ltac:(rewrite Hw; exact Ew) Ex) as [K1 K2].
    destruct Hk; contradiction.
Qed.

Lemma sym_same : forall x s g s', sym x s = Good (g, s') -> s' = s.
Proof. intros x s g s' H. unfold sym in H. destruct (nget (st_syms s) x); [injection H as _ <-; reflexivity | discriminate]. Qed.

(* the field number fld_id gives is the final one *)
Definition fld_id_final := @EtyTyExec.fld_id_final w P R Fl.

Lemma nlook_fields : forall n (ftys : list (string * ety)) t,
  sassoc n ftys = Some t -> A.nlook (Fl n) (map (fun p => (Fl (fst p), trF (snd p))) ftys) = Some (trF t).
Proof.
  intros n ftys t H. induction ftys as [|[k v] r IH]; [discriminate|]. cbn in H |- *.
  destruct (String.eqb_spec n k) as [<-|Hne].
  - injection H as <-. rewrite Nat.eqb_refl. reflexivity.
  - destruct (Nat.eqb_spec (Fl n) (Fl k)) as [E|]; [exfalso; exact (Hne (HFl _ _ E)) | exact (IH H)].
Qed.

Lemma nlook_fields_canon : forall e n (ftys : list (string * ety)) t,
  sassoc n ftys = Some t -> A.nlook (Fl n) (map (fun p => (Fl (fst p), trF (canon e (snd p)))) ftys) = Some (trF (canon e t)).
Proof.
  intros e n ftys t H. induction ftys as [|[k v] r IH]; [discriminate|]. cbn in H |- *.
  destruct (String.eqb_spec n k) as [<-|Hne].
  - injection H as <-. rewrite Nat.eqb_refl. reflexivity.
  - destruct (Nat.eqb_spec (Fl n) (Fl k)) as [E|]; [exfalso; exact (Hne (HFl _ _ E))|exact (IH H)].
Qed.

Definition ok_layout_ok := @EtyTyExec.ok_layout_ok.

Lemma ok_payload_type : forall t e, ok (payload_type t e).
Proof. intros. unfold payload_type. okt. Qed.

Lemma ok_compile_pattern : forall mi e p sty heads, ok (compile_pattern mi e p sty heads).
Proof. intros. unfold compile_pattern. okt. Qed.

(* a field of a well-formed record type is well formed *)
Lemma wf_field : forall ftys n t, WFty (YRec ftys) -> sassoc n ftys = Some t -> WFty t.
Proof.
  intros ftys n t W0 H. unfold WFty in W0. cbn [ety_wf] in W0. induction ftys as [|[k v] r IH]; [discriminate|].
  cbn in W0, H. apply andb_true_iff in W0 as [W1 W2]. destruct (String.eqb n k); [injection H as <-; exact W1 | exact (IH W2 H)].
Qed.

(* the synth wrapper: a construct's helper, then the check of the expected type *)
Lemma support_field : forall ftys n t, Support WW(YRec ftys) -> sassoc n ftys=Some t -> Support WW t.
Proof.
 intros ftys n t S E. inversion S;subst. match goal with H:Forall _ ftys|-_=>rename H into F end.
 clear -F E. induction F as[|[k y]ftys Sy F IH];cbn[sassoc assoc]in E;[discriminate|].
 destruct(String.eqb n k);[injection E as <-;exact Sy|exact(IH E)].
Qed.
Lemma support_member : forall sty m, Support WW sty -> In m(members sty) -> Support WW m.
Proof.
 intros sty m S H. destruct sty;cbn[members]in H;try contradiction;try(destruct H as[<-|[]];exact S).
 inversion S as[| | | |ms Supp|];subst. rewrite Forall_forall in Supp. exact(Supp m H).
Qed.

Lemma synth_wrap : forall e exp s v t s' (m : M (xexpr * ety)),
  (let! (v, t) := m in inject_check e t exp ;; ret (v, t)) s = Good ((v, t), s') ->
  ok m -> Inv s ->
  exists s1, m s = Good ((v, t), s1) /\ Inv s1 /\ Fr s s1 /\ Fr s1 s'.
Proof.
  intros e exp s v t s' m H Om Hi. apply bind_inv in H as ([v0 t0] & s1 & H1 & H).
  apply bind_inv in H as (u & s2 & Hinj & H). injection H as <- <- <-.
  destruct (Om _ _ _ Hi H1) as [Hi1 Fs1]. destruct (ok_inject_check e t0 exp _ _ _ Hi1 Hinj) as [_ Fs2].
  exists s1. auto.
Qed.

Lemma kind_read : forall x g, nget (w_syms w) x = Some g -> A.kind_sym WW x = Some (kconv (gs_kind g)).
Proof. intros x g Hg. exact (kind_sym_read b w R Fl Hload x g Hg). Qed.

(* ---- fields ------------------------------------------------------------ *)

Lemma field_step : forall f, OK w P R Fl f -> forall c e o n exp s v t s',
  synth (S f) c e (AEField o n) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> opt_wf exp -> opt_support WW exp -> Post w P R Fl s' F ->
  A.Ex WW P F C (AEField o n) (option_map (fun t => trF (canon e t)) exp) v (trF (canon e t)) /\ WFty t /\ Support WW t.
Proof.
  intros f IH c e o n exp s v t s' H F C Hi Ii Ifwd Hc Hx Hxs Hp.
  cbn [synth] in H. destruct (synth_wrap e exp s v t s' _ H (ok_synth_field f c e o n) Hi) as (s1 & H1 & Hi1 & Fs1 & Fs1').
  pose proof (post_back w P R Fl _ _ F Fs1' Hp) as Hp1. clear H.
  destruct f as [|f']; [discriminate H1|]. pose proof (ok_down w P R Fl f' IH) as IH'.
  cbn [synth_field] in H1.
  bi H1 as r3 s3 Ho. destruct r3 as [ox oty].
  okstep' Ho (ok_synth f' c e o None) Hi Hi3 Fs3.
  bi H1 as record s4 Hr.
  assert (Hw0 : st_w s = w) by (destruct Hp1 as [L _]; rewrite <- (lt_w _ _ _ _ _ L); symmetry; exact (fr_w _ _ Fs1)).
  assert (Hw : st_w s3 = w) by (rewrite (fr_w _ _ Fs3); exact Hw0).
  (* the receiver *)
  assert (Ok4 : ok (match oty with
                    | YNom s0 _ => let! g := sym s0 in if kind_eqb (gs_kind g) KTag then payload_type oty e else ret oty
                    | _ => ret oty end)) by (destruct oty; unfold payload_type; okt).
  okstep' Hr Ok4 Hi3 Hi4 Fs4.
  destruct record as [| | | | |ftys]; try nogood H1.
  bi H1 as u s5 Hlay. okstep' Hlay (ok_layout_ok e (YRec ftys)) Hi4 Hi5 Fs5.
  destruct (sassoc n ftys) as [tf|] eqn:En; [|nogood H1].
  bi H1 as i s6 Hf. injection H1 as <- <- <-.
  assert (Ei : Fl n = i) by (destruct Hp1 as [L _]; exact (fld_id_final n s5 i s6 s6 Hi5 Hf (fr_refl _) L)).
  pose proof (post_back w P R Fl _ _ F (proj2 (ok_fld_id n _ _ _ Hi5 Hf)) Hp1) as Hp5.
  pose proof (post_back w P R Fl _ _ F Fs5 Hp5) as Hp4.
  pose proof (post_back w P R Fl _ _ F Fs4 Hp4) as Hp3.
  destruct (bs_synth _ _ _ _ _ IH' c e o None s ox oty s3 Ho F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc I I Hp3) as [Eo [Wo So]].
  (* the record: a tag's payload, or the receiver itself *)
  assert (Hrec : ((exists x xs, trF (canon e oty) = TNom x xs /\ A.kind_sym WW x = Some A.DKTag /\
                                A.PayloadOf WW x xs (trF (canon e (YRec ftys)))) \/ trF (canon e (YRec ftys)) = trF (canon e oty)) /\
                 WFty (YRec ftys) /\ Support WW(YRec ftys)).
  { destruct oty as [| | |x xs|ms|fs]; try (injection Hr as E _; discriminate E).
    - bi Hr as g s7 Hs. destruct (kind_eqb (gs_kind g) KTag) eqn:Kg; [|injection Hr as E _; discriminate E].
      apply kind_eqb_spec in Kg.
      destruct (sym_nominal s3 x g s7 Hi3 Hw Hs (or_introl Kg)) as [-> Hg].
      destruct (Hpayload c e F C x xs s3 (YRec ftys) s4 Hr Hi3 Hw Hc (proj1 Hp4) (ex_intro _ g (conj Hg Kg)) Wo So) as [Hpo [Wp Sp]].
      pose proof (payload_subst WW (suf Fl (en_uf e)) _ _ _ Hpo) as HpoC.
      rewrite smap_trx, tr_canon in HpoC by exact (proj1 (cr_uf _ _ _ _ _ _ _ Hc)).
      clear Hpo. rename HpoC into Hpo.
      split; [|split;[exact Wp|exact Sp]]. left. exists x, (trxC e xs). split; [apply tr_canon_nom|]. split; [|exact Hpo].
      rewrite (kind_read x g Hg), Kg. reflexivity.
    - injection Hr as -> _. split; [right; reflexivity | split;[exact Wo|exact So]]. }
  destruct Hrec as [Hrec [Wr Sr]].
  split.
  - rewrite <- Ei. eapply A.ExField; [exact Eo | exact Hrec |].
    exists (map (fun p => (Fl (fst p), trF (canon e (snd p)))) ftys). split; [apply tr_canon_rec|]. exact (nlook_fields_canon e n ftys tf En).
  - split;[exact(wf_field ftys n tf Wr En)|exact(support_field ftys n tf Sr En)].
Qed.

(* ---- match: heads, patterns, arms ------------------------------------- *)

Definition hstep (hs : list (nat * ety)) (m : ety) : list (nat * ety) :=
  match head m with Some h => aset Nat.eqb h m hs | None => hs end.

Lemma nassoc_aset : forall V n k (v : V) l,
  nassoc n (aset Nat.eqb k v l) = if Nat.eqb n k then Some v else nassoc n l.
Proof.
  intros V n k v l. induction l as [|[k' v'] r IH]; cbn.
  - destruct (Nat.eqb n k); reflexivity.
  - destruct (Nat.eqb_spec k k') as [<-|Hne]; cbn.
    + destruct (Nat.eqb n k); reflexivity.
    + destruct (Nat.eqb_spec n k') as [->|]; [|exact IH].
      destruct (Nat.eqb_spec k' k); [congruence | reflexivity].
Qed.

Lemma heads_in : forall ms hs0 x m, nassoc x (fold_left hstep ms hs0) = Some m ->
  nassoc x hs0 = Some m \/ (In m ms /\ head m = Some x).
Proof.
  induction ms as [|m0 r IH]; intros hs0 x m H; [left; exact H|]. cbn [fold_left] in H.
  destruct (IH _ _ _ H) as [H1|[H1 H2]]; [|right; split; [right; exact H1 | exact H2]].
  unfold hstep in H1. destruct (head m0) as [h|] eqn:Eh; [|left; exact H1].
  rewrite nassoc_aset in H1. destruct (Nat.eqb_spec x h) as [->|]; [|left; exact H1].
  injection H1 as <-. right. split; [left; reflexivity | exact Eh].
Qed.

Lemma heads_keep : forall ms hs0 x, nassoc x hs0 <> None -> nassoc x (fold_left hstep ms hs0) <> None.
Proof.
  induction ms as [|m0 r IH]; intros hs0 x H; [exact H|]. cbn [fold_left]. apply IH.
  unfold hstep. destruct (head m0) as [h|]; [|exact H]. rewrite nassoc_aset.
  destruct (Nat.eqb x h); [discriminate | exact H].
Qed.

Lemma heads_cover : forall ms hs0 m x, In m ms -> head m = Some x -> nassoc x (fold_left hstep ms hs0) <> None.
Proof.
  induction ms as [|m0 r IH]; intros hs0 m x Hin Hh; [destruct Hin|]. cbn [fold_left].
  destruct Hin as [<-|Hin]; [|exact (IH _ _ _ Hin Hh)].
  apply heads_keep. unfold hstep. rewrite Hh, nassoc_aset, Nat.eqb_refl. discriminate.
Qed.

Lemma nassoc_in : forall V k (l : list (nat * V)) v, nassoc k l = Some v -> In (k, v) l.
Proof.
  intros V k l v H. induction l as [|[k' v'] r IH]; [discriminate|]. cbn in H.
  destruct (Nat.eqb_spec k k') as [<-|]; [injection H as <-; left; reflexivity | right; exact (IH H)].
Qed.

Definition trb (bs : list (string * ety)) : list (string * ty) := map (fun q => (fst q, trF (snd q))) bs.
Definition trbC (e : env) (bs : list (string * ety)) : list (string * ty) :=
  map (fun q => (fst q, trF (canon e (snd q)))) bs.

Lemma resolve_sym_rel : forall mi p s r s', st_w s = w -> In mi (w_order w) -> Inv s ->
  resolve_sym mi p s = Good (r, s') ->
  s' = s /\ match r with
            | Some g => A.resolve WW (gm_path (get_mod w mi)) p = Some (A.ESym (gs_id g)) /\
                        nget (w_syms w) (gs_id g) = Some g
            | None => True
            end.
Proof.
  intros mi p s r s' Hw Hmi Hi H. unfold resolve_sym in H. bi H as r0 s1 Hr.
  destruct (rpath_rel w R Fl b Hload mi p s r0 s1 Hw Hmi Hr) as [-> Hres].
  destruct r0 as [[x|j]|]; try (injection H as <- <-; split; [reflexivity | exact I]).
  bi H as g s2 Hs. injection H as <- <-. cbn [option_map ent_of] in Hres.
  destruct (sym_loader w R Fl b Hload _ _ x s g s2 Hi Hw Hres Hs) as [-> Hg].
  split; [reflexivity|]. rewrite (load_world_ids b w Hload x g Hg). auto.
Qed.

Lemma smap_trb : forall e bs, Tri (en_uf e) ->
  map (fun q => (fst q,Audit.app_subst (suf Fl (en_uf e)) (snd q))) (trb bs) = trbC e bs.
Proof.
  intros e bs T. unfold trb, trbC. rewrite map_map. apply map_ext. intros [n y]. cbn [fst snd]. rewrite tr_canon by exact T. reflexivity.
Qed.

Lemma binds_uf : forall bs e,
  en_uf (fold_left (fun e p => set_local e (fst p) (snd p) LLet) bs e) = en_uf e.
Proof. induction bs as [|[n y] bs IH]; intro e; [reflexivity|]. cbn [fold_left fst snd]. rewrite IH. reflexivity. Qed.

Lemma cxrel_binds : forall bs c e F C, CxRel w R Fl c e F C -> Forall (fun q => WFty (snd q)) bs -> Forall(fun q=>Support WW(snd q))bs ->
  CxRel w R Fl c (fold_left (fun e p => set_local e (fst p) (snd p) LLet) bs e) F
    (fold_left (fun C q => A.add_local C (fst q) (snd q) A.LLet) (trbC e bs) C).
Proof.
  induction bs as [|[n t] r IH]; intros c e F C H Hw Hs; [exact H|]. inversion Hw as [|? ? W1 Wr]; subst. inversion Hs as[|? ? S1 Sr];subst.
  cbn [fold_left trbC map fst snd]. apply IH; [|exact Wr|exact Sr].
  exact (cxrel_local w R Fl c e F C n t Elab.LLet H W1 S1).
Qed.


(* the pattern binder `y`: a name that is not in scope *)
Definition binder_name_m (mi : nat) (q : apat) : M (option string) :=
  match q with
  | APPath [n] => let! t := rpath mi [n] in ret (match t with None => Some n | Some _ => None end)
  | _ => ret None
  end.

Lemma binder_name_spec : forall mi q s n s', st_w s = w -> In mi (w_order w) ->
  binder_name_m mi q s = Good (Some n, s') -> s' = s /\ q = APPath [n] /\ A.resolve WW (gm_path (get_mod w mi)) [n] = None.
Proof.
  intros mi q s n s' Hw Hmi H. destruct q as [|[|n0 [|]]| |]; cbn [binder_name_m] in H; try (injection H as E _; discriminate E).
  bi H as r s1 Hr. destruct (rpath_rel w R Fl b Hload mi [n0] s r s1 Hw Hmi Hr) as [-> Hres].
  destruct r; injection H as E <-; [discriminate E|]. subst. auto.
Qed.

Lemma ok_binder_name_m : forall mi q, ok (binder_name_m mi q).
Proof. intros. unfold binder_name_m. okt. Qed.

(* The fields of a record pattern. *)
Definition fp_step (mi : nat) (gname : string) (ftys : list (string * ety))
    (acc : list (string * ety) * list (nat * nat)) (q : string * option apat)
    : M (list (string * ety) * list (nat * nat)) :=
  let '(bs, xs) := acc in
  let '(fname, sub) := q in
  match sassoc fname ftys with
  | None => err mi ("`" ++ gname ++ "` has no field `" ++ fname ++ "`")%string
  | Some fty =>
      let! fid := fld_id fname in
      match sub with
      | None => let! v := local_var fname in ret (bs ++ [(fname, fty)], xs ++ [(fid, v)])
      | Some APWild => ret (bs, xs)
      | Some s =>
          let! bn := binder_name_m mi s in
          match bn with
          | Some n => let! v := local_var n in ret (bs ++ [(n, fty)], xs ++ [(fid, v)])
          | None => err mi "v0 record patterns support binders only"
          end
      end
  end.

Lemma ok_fp_step : forall mi gname ftys acc q, ok (fp_step mi gname ftys acc q).
Proof. intros. unfold fp_step, binder_name_m. okt. Qed.

Lemma field_pats : forall mi gname (ftys : list (string * ety)) F fs bs0 xs0 s bs xs s',
  mfold (fp_step mi gname ftys) (bs0, xs0) fs s = Good ((bs, xs), s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> A.fr_mod F = gm_path (get_mod w mi) -> Post w P R Fl s' F ->
  WFty (YRec ftys) -> Support WW(YRec ftys) ->
  exists bs1 xs1, bs = bs0 ++ bs1 /\ xs = xs0 ++ xs1 /\ Forall (fun q => WFty (snd q)) bs1 /\ Forall(fun q=>Support WW(snd q))bs1 /\
    A.FieldPats WW F (map (fun p => (Fl (fst p), trF (snd p))) ftys) fs (trb bs1) xs1.
Proof.
  intros mi gname ftys F fs. induction fs as [|[fname sub] r IH]; intros bs0 xs0 s bs xs s' H Hi Hw Hmi Hm Hp Wr Sr.
  { cbn in H. injection H as <- <- <-. exists [], []. rewrite !app_nil_r. repeat split; constructor. }
  cbn [mfold] in H. bi H as acc s1 H1.
  okstep' H1 (ok_fp_step mi gname ftys (bs0, xs0) (fname, sub)) Hi Hi1 Fs1.
  destruct acc as [bs1 xs1].
  assert (Hw1 : st_w s1 = w) by (rewrite (fr_w _ _ Fs1); exact Hw).
  destruct (IH bs1 xs1 s1 bs xs s' H Hi1 Hw1 Hmi Hm Hp Wr Sr) as (bs2 & xs2 & -> & -> & Wb2 & Sb2 & Fp2).
  pose proof (post_back w P R Fl _ _ F (proj2 (ok_mfold _ _ _ r _ (ok_fp_step mi gname ftys) s1 _ _ Hi1 H)) Hp) as Hp1.
  unfold fp_step in H1. destruct (sassoc fname ftys) as [fty|] eqn:Ef; [|nogood H1].
  pose proof (wf_field ftys fname fty Wr Ef) as Wf.
  pose proof(support_field ftys fname fty Sr Ef)as Sf.
  pose proof (nlook_fields fname ftys fty Ef) as Hl.
  bi H1 as fid s2 Hfid. okstep' Hfid (ok_fld_id fname) Hi Hi2 Fs2.
  match type of H1 with ?m s2 = Good (_, _) =>
    assert (Ok2 : ok m) by (destruct sub as [[]|]; cbn beta iota; unfold binder_name_m; okt) end.
  okstep' H1 Ok2 Hi2 Hi3 Fs3.
  pose proof (fld_id_final fname s fid s2 s1 Hi Hfid Fs3 (proj1 Hp1)) as Efid.
  destruct sub as [q|].
  - destruct (match q with APWild => true | _ => false end) eqn:Eq.
    + destruct q; try discriminate Eq. injection H1 as <- <- <-.
      exists bs2, xs2. repeat split; [exact Wb2|exact Sb2|]. eapply A.FPWild; [exact Hl | exact Fp2].
    + assert (Hq : (let! bn := binder_name_m mi q in
                    match bn with
                    | Some n => let! v := local_var n in ret (bs0 ++ [(n, fty)], xs0 ++ [(fid, v)])
                    | None => err mi "v0 record patterns support binders only"
                    end) s2 = Good ((bs1, xs1), s1)) by (destruct q; try discriminate Eq; exact H1).
      clear H1. bi Hq as bn s3 Hb. destruct bn as [n|]; [|nogood Hq].
      destruct (binder_name_spec mi q s2 n s3 ltac:(rewrite (fr_w _ _ Fs2); exact Hw) Hmi Hb) as (-> & -> & Hres).
      bi Hq as v s4 Hv. injection Hq as E1 E2 E3. subst bs1 xs1 s4.
      destruct (local_named w P R Fl n s2 v s1 s' F Hv (proj2 (ok_mfold _ _ _ r _ (ok_fp_step mi gname ftys) s1 _ _ Hi1 H)) Hp) as [Nn Vn].
      exists ((n, fty) :: bs2), ((fid, v) :: xs2). rewrite <- !app_assoc. split; [reflexivity|]. split; [reflexivity|].
      split; [constructor; assumption|]. split;[constructor;assumption|]. cbn [trb map fst snd]. rewrite <- Vn, <- Efid.
      eapply A.FPBind; [exact Hl | rewrite Hm; exact Hres | exact Nn | exact Fp2].
  - bi H1 as v s4 Hv. injection H1 as E1 E2 E3. subst bs1 xs1 s4.
    destruct (local_named w P R Fl fname s2 v s1 s' F Hv (proj2 (ok_mfold _ _ _ r _ (ok_fp_step mi gname ftys) s1 _ _ Hi1 H)) Hp) as [Nn Vn].
    exists ((fname, fty) :: bs2), ((fid, v) :: xs2). rewrite <- !app_assoc. split; [reflexivity|]. split; [reflexivity|].
    split; [constructor; assumption|]. split;[constructor;assumption|]. cbn [trb map fst snd]. rewrite <- Vn, <- Efid.
    eapply A.FPPun; [exact Hl | exact Nn | exact Fp2].
Qed.

Lemma head_nom : forall ms x m, In m ms -> head m = Some x -> (match m with YNom _ _ => true | _ => false end) = true ->
  exists xs, m = YNom x xs.
Proof. intros ms x m _ Hh Hn. destruct m; try discriminate Hn. injection Hh as ->. eauto. Qed.

(* A pattern, against the scrutinee's type. *)
Lemma pat_step : forall c e F C p sty s pat bs ah ca s',
  compile_pattern (cx_mod c) e p sty (fold_left hstep (members sty) []) s = Good ((pat, bs, ah, ca), s') ->
  Inv s -> st_w s = w -> CxRel w R Fl c e F C -> Post w P R Fl s' F -> WFty sty -> Support WW sty ->
  A.PatOf WW F (trF sty) p pat (trb bs) ah ca /\ Forall (fun q => WFty (snd q)) bs /\ Forall(fun q=>Support WW(snd q))bs.
Proof.
  intros c e F C p sty s pat bs ah ca s' H Hi Hw Hc Hp Ws Ss.
  pose proof (cr_mod_in _ _ _ _ _ _ _ Hc) as Hmi. pose proof (cr_mod _ _ _ _ _ _ _ Hc) as Hm.
  set (heads := fold_left hstep (members sty) []) in H.
  (* a head the pattern names: a member of the scrutinee's type *)
  assert (Mem : forall x m, nassoc x heads = Some m -> (match m with YNom _ _ => true | _ => false end) = true ->
             exists xs, m = YNom x xs /\ In (TNom x (trx xs)) (A.members (trF sty)) /\ WFty m /\ Support WW m).
  { intros x m Hx Hn. destruct (heads_in _ _ _ _ Hx) as [E|[Hin Hh]]; [discriminate E|].
    destruct (head_nom _ _ _ Hin Hh Hn) as [xs ->]. exists xs. split; [reflexivity|].
    split;[rewrite members_tr;exact(in_map trF _ _ Hin)|]. split.
    - exact(proj1(Forall_forall _ _)(BodyTy.members_wf sty Ws) _ Hin).
    - exact(support_member sty (YNom x xs) Ss Hin). }
  destruct p as [|pp|pp sub|[pp|] fs]; unfold compile_pattern in H; cbn beta iota zeta in H.
  - (* _ *) injection H as <- <- <- <- <-. split; [apply A.PWild |split;constructor].
  - (* a binder, a unit or a tag *)
    bi H as r s1 Hr. destruct (rpath_rel w R Fl b Hload _ pp s r s1 Hw Hmi Hr) as [-> Hres].
    destruct r as [[x|j]|]; [| destruct pp as [|? [|]]; nogood H | destruct pp as [|n [|]]; try nogood H].
    + bi H as g s2 Hs.
      destruct (kind_eqb (gs_kind g) KUnit || kind_eqb (gs_kind g) KTag) eqn:Kg; [|nogood H].
      destruct (nassoc x heads) as [m|] eqn:Ex; [|nogood H].
      destruct (match m with YNom _ _ => true | _ => false end) eqn:Em; [|nogood H].
      injection H as <- <- <- <- <-.
      assert (Kg' : gs_kind g = KUnit \/ gs_kind g = KTag)
        by (apply orb_true_iff in Kg as [K|K]; apply kind_eqb_spec in K; auto).
      destruct (sym_nominal s x g s2 Hi Hw Hs ltac:(destruct Kg'; auto)) as [-> Hg].
      destruct (Mem x m Ex Em) as (xs & -> & Hin & Wnom & Snom).
      split; [|split;constructor].
      eapply A.PUnit with (xs := trx xs); [unfold A.resolve_sym; rewrite Hm; cbn [option_map ent_of] in Hres; rewrite Hres; reflexivity
                                          | exact (kind_read x g Hg) | destruct Kg' as [K|K]; rewrite K; [left | right]; reflexivity
                                          | exact Hin].
    + (* a binder *)
      bi H as v s2 Hv. injection H as <- <- <- <- ->.
      destruct (local_named w P R Fl n s v s' s' F Hv (fr_refl _) Hp) as [Nn Vn].
      split; [|split;[constructor;[exact Ws|constructor]|constructor;[exact Ss|constructor]]]. cbn [trb map fst snd]. rewrite <- Vn.
      apply A.PBinder; [rewrite Hm; exact Hres | exact Nn].
  - (* S(y) *)
    bi H as r s1 Hr. destruct (resolve_sym_rel _ pp s r s1 Hw Hmi Hi Hr) as [-> Hres].
    destruct r as [g|]; [|nogood H]. destruct Hres as [Hres Hg].
    destruct (negb (kind_eqb (gs_kind g) KTag)) eqn:Kg; [nogood H|]. apply negb_false_iff, kind_eqb_spec in Kg.
    destruct (nassoc (gs_id g) heads) as [m|] eqn:Ex; [|nogood H].
    destruct (match m with YNom _ _ => true | _ => false end) eqn:Em; [|destruct m; try discriminate Em; nogood H].
    destruct (Mem _ m Ex Em) as (xs & -> & Hin & Wnom & Snom).
    bi H as pay s2 Hpay. okstep' Hpay (ok_payload_type (YNom (gs_id g) xs) e) Hi Hi2 Fs2.
    bi H as bn s3 Hb. change (binder_name_m (cx_mod c) sub s2 = Good (bn, s3)) in Hb.
    destruct bn as [n|]; [|nogood H].
    destruct (binder_name_spec _ sub s2 n s3 ltac:(rewrite (fr_w _ _ Fs2); exact Hw) Hmi Hb) as (-> & -> & Hres').
    bi H as u s4 Hl. okstep' Hl (ok_layout_ok e pay) Hi2 Hi4 Fs4.
    bi H as v s5 Hv. injection H as <- <- <- <- ->.
    destruct (local_named w P R Fl n s4 v s' s' F Hv (fr_refl _) Hp) as [Nn Vn].
    pose proof (post_back w P R Fl _ _ F (proj2 (ok_local_var n _ _ _ Hi4 Hv)) Hp) as Hp4.
    pose proof (post_back w P R Fl _ _ F Fs4 Hp4) as Hp2.
    destruct (Hpayload c e F C (gs_id g) xs s (pay) s2 Hpay Hi Hw Hc (proj1 Hp2) (ex_intro _ g (conj Hg Kg)) Wnom Snom) as [Hpo [Wp Sp]].
    split; [|split;[constructor;[exact Wp|constructor]|constructor;[exact Sp|constructor]]]. cbn [trb map fst snd]. rewrite <- Vn.
    eapply A.PTagBind; [unfold A.resolve_sym; rewrite Hm, Hres; reflexivity | rewrite (kind_read _ g Hg), Kg; reflexivity
                       | exact Hin | exact Hpo | rewrite Hm; exact Hres' | exact Nn].
  - (* S{…} *)
    bi H as r s1 Hr. destruct (resolve_sym_rel _ pp s r s1 Hw Hmi Hi Hr) as [-> Hres].
    destruct r as [g|]; [|nogood H]. destruct Hres as [Hres Hg].
    destruct (negb (kind_eqb (gs_kind g) KTag)) eqn:Kg; [nogood H|]. apply negb_false_iff, kind_eqb_spec in Kg.
    destruct (nassoc (gs_id g) heads) as [m|] eqn:Ex; [|nogood H].
    destruct (match m with YNom _ _ => true | _ => false end) eqn:Em; [|destruct m; try discriminate Em; nogood H].
    destruct (Mem _ m Ex Em) as (xs & -> & Hin & Wnom & Snom).
    bi H as pay s2 Hpay. okstep' Hpay (ok_payload_type (YNom (gs_id g) xs) e) Hi Hi2 Fs2.
    destruct pay as [| | | | |ftys]; try nogood H.
    bi H as u s4 Hl. okstep' Hl (ok_layout_ok e (YRec ftys)) Hi2 Hi4 Fs4.
    bi H as acc s5 Hf. destruct acc as [bs0 xs0]. injection H as <- -> <- <- ->.
    change (mfold (fp_step (cx_mod c) (gs_name g) ftys) ([], []) fs s4 = Good ((bs, xs0), s')) in Hf.
    pose proof (post_back w P R Fl _ _ F (proj2 (ok_mfold _ _ _ fs _ (ok_fp_step (cx_mod c) (gs_name g) ftys) s4 _ _ Hi4 Hf)) Hp) as Hp4.
    pose proof (post_back w P R Fl _ _ F Fs4 Hp4) as Hp2.
    destruct (Hpayload c e F C (gs_id g) xs s (YRec ftys) s2 Hpay Hi Hw Hc (proj1 Hp2) (ex_intro _ g (conj Hg Kg)) Wnom Snom) as [Hpo [Wp Sp]].
    destruct (field_pats (cx_mod c) (gs_name g) ftys F fs [] [] s4 bs xs0 s' Hf Hi4
                ltac:(rewrite (fr_w _ _ Fs4), (fr_w _ _ Fs2); exact Hw) Hmi Hm Hp Wp Sp) as (bs1 & xs1 & -> & -> & Wb & Sb & Fp).
    split; [|split;[exact Wb|exact Sb]].
    eapply A.PRecord; [unfold A.resolve_sym; rewrite Hm, Hres; reflexivity | rewrite (kind_read _ g Hg), Kg; reflexivity
                      | exact Hin | exact Hpo | exact Fp].
  - (* `{…}` with no tag head *) nogood H.
Qed.

(* the elaborator's join, as a function *)
Definition jn (e : env) (exp : option ety) (a b0 : ety) : option ety :=
  match exp with Some t => Some t | None =>
    if is_never (canon e a) then Some b0
    else if is_never (canon e b0) || ety_eqb (canon e a) (canon e b0) then Some a else None end.

Lemma join_jn : forall mi e a b0 exp s t s', join mi e a b0 exp s = Good (t,s') ->
  jn e exp a b0 = Some t /\ s'=s.
Proof.
  intros mi e a b0 exp s t s' H. unfold join in H. unfold jn.
  destruct exp; [injection H as <- <-; auto|].
  destruct (is_never (canon e a)); [injection H as <- <-; auto|].
  destruct (is_never (canon e b0) || ety_eqb (canon e a) (canon e b0)); [injection H as <- <-; auto|nogood H].
Qed.

Lemma jn_support : forall e exp a b0 t, jn e exp a b0=Some t ->
 Support WW a ->Support WW b0 ->opt_support WW exp ->Support WW t.
Proof.
 intros e exp a b0 t E Sa Sb Sx. unfold jn in E. destruct exp;
 [injection E as <-;exact Sx|].
 destruct(is_never(canon e a));[injection E as <-;exact Sb|].
 destruct(is_never(canon e b0)||ety_eqb(canon e a)(canon e b0));[injection E as <-;exact Sa|discriminate].
Qed.

Lemma jn_assoc : forall e x a b0 c ab r, jn e x a b0 = Some ab -> jn e x ab c = Some r ->
  exists bc, jn e x b0 c = Some bc /\ jn e x a bc = Some r.
Proof.
  intros e x a b0 c ab r H1 H2. destruct x as [t|].
  - cbn in H1, H2 |- *. injection H1 as <-. injection H2 as <-. eauto.
  - unfold jn in H1. destruct (is_never (canon e a)) eqn:Na.
    + injection H1 as <-. exists r. split; [exact H2|]. unfold jn. rewrite Na. reflexivity.
    + destruct (is_never (canon e b0) || ety_eqb (canon e a) (canon e b0)) eqn:E1; [|discriminate]. injection H1 as <-.
      unfold jn in H2. rewrite Na in H2.
      destruct (is_never (canon e c) || ety_eqb (canon e a) (canon e c)) eqn:E2; [|discriminate]. injection H2 as <-.
      apply orb_true_iff in E1 as [Nb|Eab].
      * exists c. unfold jn. rewrite Nb, Na, E2. split; reflexivity.
      * apply ety_eqb_true in Eab. exists b0. unfold jn. rewrite <- Eab.
        rewrite Na, E2, ety_eqb_refl, orb_true_r. split; reflexivity.
Qed.

Lemma jn_tr : forall e exp a b0 t, jn e exp a b0 = Some t -> UfWF (en_uf e) -> WFty a -> WFty b0 -> opt_wf exp ->
  A.Join (trF (canon e a)) (trF (canon e b0)) (option_map (fun t => trF (canon e t)) exp) (trF (canon e t)) /\ WFty t.
Proof.
  intros e exp a b0 t H U Wa Wb Wx. destruct exp as [x|]; cbn [jn] in H.
  - injection H as <-. split; [constructor|exact Wx].
  - destruct (is_never (canon e a)) eqn:Ea.
    + injection H as <-. split; [rewrite (is_never_true_tr Fl _ Ea); constructor|exact Wb].
    + destruct (is_never (canon e b0) || ety_eqb (canon e a) (canon e b0)) eqn:Eb; [|discriminate]. injection H as <-.
      split; [|exact Wa]. apply A.JoinSame.
      * apply orb_true_iff in Eb as [Eb|Eb].
        -- left. exact (is_never_true_tr Fl _ Eb).
        -- right. rewrite (ety_eqb_true _ _ Eb). reflexivity.
      * intro E. apply (never_tr Fl _ (canon_wf e a U Wa)) in E. rewrite E in Ea. discriminate.
Qed.

Lemma canon_nil_c : forall C t, A.c_sub C = [] -> A.canon C t = t.
Proof. intros C t H. unfold A.canon. rewrite H. apply CoreMoss.app_subst_nil. Qed.

(* One arm of a match, as the elaborator checks it. *)
Definition am_step (f' : nat) (c : cx) (e : env) (exp : option ety) (sty : ety)
    (acc : list (xpat * xexpr) * list nat * bool * option ety) (arm : apat * aexpr)
    : M (list (xpat * xexpr) * list nat * bool * option ety) :=
  let '(xarms, covered, catch, result) := acc in
  let! (pat, binders, ah, ca) := compile_pattern (cx_mod c) e (fst arm) sty (fold_left hstep (members sty) []) in
  let e_arm := fold_left (fun e p => set_local e (fst p) (snd p) LLet) binders e in
  let! (bx, bty) :=
    match snd arm with
    | AEBlock b => let! (bx, _, bt) := synth_block f' c e_arm b exp in ret (bx, opt_default bt exp)
    | y => let! (bx, bt) := synth f' c e_arm y exp in
           if Elab.opt_fits e_arm bt exp then ret (bx, bt)
           else let! a := show bt in let! b := show (opt_default YUnit exp) in
                err (cx_mod c) ("match arm yields `" ++ a ++ "`, expected `" ++ b ++ "`")%string
    end in
  let! r := match result with None => ret bty | Some r => join (cx_mod c) e r bty exp end in
  ret (xarms ++ [(pat, bx)], covered ++ ah, catch || ca, Some r).

Lemma ok_am_step : forall f' c e exp sty acc arm, ok (am_step f' c e exp sty acc arm).
Proof. intros. unfold am_step, compile_pattern, join. okt. Qed.

Definition res_ok (e : env) (exp : option ety) (res0 : option ety) (T R : ety) : Prop :=
  match res0 with None => R = T | Some r0 => jn e exp r0 T = Some R end.

Lemma arms_loop : forall f' c e exp sty F C, OK w P R Fl f' -> forall arms xr0 cov0 ca0 res0 s xr cov ca res s',
  mfold (am_step f' c e exp sty) (xr0, cov0, ca0, res0) arms s = Good ((xr, cov, ca, res), s') ->
  Inv s -> InfoInv s -> FwdInv s -> st_w s = w -> CxRel w R Fl c e F C -> Post w P R Fl s' F -> WFty sty -> Support WW sty -> opt_wf exp ->opt_support WW exp ->
  match res0 with Some r0 => WFty r0 | None => True end ->
  match res0 with Some r0 => Support WW r0 | None => True end ->
  (arms = [] /\ xr = xr0 /\ cov = cov0 /\ ca = ca0 /\ res = res0) \/
  (exists xarms covd cad T R0, xr = xr0 ++ xarms /\ cov = cov0 ++ covd /\ ca = ca0 || cad /\ res = Some R0 /\
     A.ArmsList WW P F C (trF (canon e sty)) arms (option_map (fun t => trF (canon e t)) exp) xarms covd cad (trF (canon e T)) /\
     res_ok e exp res0 T R0 /\ WFty T /\ Support WW T /\ WFty R0 /\ Support WW R0).
Proof.
  intros f' c e exp sty F C IH arms. induction arms as [|arm rest IHr];
    intros xr0 cov0 ca0 res0 s xr cov ca res s' H Hi Ii Ifwd Hw Hc Hp Ws Ss Wx Sx Wr0 Sr0.
  { left. cbn in H. injection H as <- <- <- <- _. auto. }
  right. cbn [mfold] in H. bi H as acc1 s1 H1. okstep' H1 (ok_am_step f' c e exp sty (xr0, cov0, ca0, res0) arm) Hi Hi1 Fs1.
  pose proof (proj2 (ok_mfold _ _ _ rest _ (ok_am_step f' c e exp sty) s1 _ _ Hi1 H)) as Fr1.
  pose proof (post_back w P R Fl _ _ F Fr1 Hp) as Hp1.
  destruct arm as [pt body]. unfold am_step in H1. cbn [fst snd] in H1.
  bi H1 as r2 s2 Hpt. destruct r2 as [[[pat binders] ah] ca1].
  okstep' Hpt (ok_compile_pattern (cx_mod c) e pt sty (fold_left hstep (members sty) [])) Hi Hi2 Fs2.
  bi H1 as r3 s3 Hb. destruct r3 as [bx bty].
  set (e_arm := fold_left (fun e p => set_local e (fst p) (snd p) LLet) binders e) in Hb.
  match type of Hb with ?m s2 = Good _ => assert (Okb : ok m) by (destruct body; okt) end.
  okstep' Hb Okb Hi2 Hi3 Fs3.
  bi H1 as r s4 Hj.
  assert (Ej : match res0 with None => r = bty | Some r0 => jn e exp r0 bty = Some r end /\ s4 = s3).
  { destruct res0 as [r0|]; [exact (join_jn _ _ _ _ _ _ _ _ Hj) | injection Hj as <- <-; auto]. }
  destruct Ej as [Ej ->]. injection H1 as <- ->.
  pose proof (post_back w P R Fl _ _ F Fs3 Hp1) as Hp2.
  destruct (pat_step c e F C pt sty s pat binders ah ca1 s2 Hpt Hi Hw Hc Hp2 Ws Ss) as [Hpat [Wb Sb]].
  pose proof (pattern_subst WW F _ _ _ _ _ _ (suf Fl (en_uf e)) Hpat) as HpC.
  rewrite tr_canon, smap_trb in HpC by exact (proj1 (cr_uf _ _ _ _ _ _ _ Hc)).
  clear Hpat. rename HpC into Hpat.
  pose proof (cxrel_binds binders c e F C Hc Wb Sb) as Hca.
  assert (Earm : en_uf e_arm = en_uf e) by apply binds_uf.
  pose proof (canon_uf e e_arm Earm) as Ecanon.
  assert (Eexp : option_map (fun t => trF (canon e_arm t)) exp = option_map (fun t => trF (canon e t)) exp).
  { destruct exp; cbn [option_map]; [rewrite Ecanon|]; reflexivity. }
  assert (Ii2 : InfoInv s2) by cache_inv.
  assert (Ifwd2 : FwdInv s2) by fwd_inv.
  (* Every non-block constructor uses the same expression soundness fact. *)
  assert (ExprArm : forall y x ty last,
    synth f' c e_arm y exp s2 = Good ((x, ty), last) ->
    Elab.opt_fits e_arm ty exp = true -> Post w P R Fl last F ->
    (forall block, y <> AEBlock block) ->
    A.Branch WW P F (fold_left (fun C q => A.add_local C (fst q) (snd q) A.LLet) (trbC e binders) C)
      y (option_map (fun t => trF (canon e_arm t)) exp) x (trF (canon e_arm ty)) /\
    A.opt_fits (trF (canon e_arm ty)) (option_map (fun t => trF (canon e_arm t)) exp) /\
    WFty ty /\ Support WW ty).
  {
    intros y x ty last Run Fits Post Nonblock.
    destruct (bs_synth _ _ _ _ _ IH c e_arm y exp s2 x ty last Run F _ Hi2 Ii2 Ifwd2 Hca Wx Sx Post)
      as [Ex [Well Supported]].
    split; [apply A.BranchExpr; [exact Nonblock | exact Ex] |].
    split; [exact (elab_opt_fits_tr Fl e_arm ty exp (cr_uf _ _ _ _ _ _ _ Hca) Fits) |].
    exact (conj Well Supported).
  }
  (* the arm's body *)
  assert (Hbody : A.Branch WW P F (fold_left (fun C q => A.add_local C (fst q) (snd q) A.LLet) (trbC e binders) C)
                    body (option_map (fun t => trF (canon e_arm t)) exp) bx (trF (canon e_arm bty)) /\ A.opt_fits (trF (canon e_arm bty)) (option_map (fun t => trF (canon e_arm t)) exp) /\ WFty bty /\ Support WW bty).
  { destruct body; cbn beta iota in Hb;
      try (bi Hb as r5 s5 Hy; destruct r5 as [bx0 bt0];
           destruct (Elab.opt_fits e_arm bt0 exp) eqn:Eo; [|nogood Hb]; injection Hb as <- <- <-;
           exact (ExprArm _ _ _ _ Hy Eo Hp1 ltac:(intros block Equal; discriminate Equal))).
    bi Hb as r5 s5 Hy. destruct r5 as [[bx0 t0] bt0]. injection Hb as <- <- <-.
    destruct (bs_block _ _ _ _ _ IH c e_arm a exp s2 bx0 t0 bt0 s5 Hy F _ Hi2 Ii2 Ifwd2 Hca Wx Sx Hp1) as (Eb & Wt & St & -> & _).
    split; [|split].
    - replace (trF (canon e_arm (opt_default (block_bt a t0) exp))) with
        (match option_map (fun t => trF (canon e_arm t)) exp with Some x => x | None => A.branch_ty a (trF (canon e_arm t0)) end)
        by (destruct exp; [reflexivity | symmetry; apply branch_canon_tr]).
      apply A.BranchBlock. exact Eb.
    - destruct exp as [x|]; [left; reflexivity | exact I].
    - split;[destruct exp as[x|];[exact Wx|apply block_bt_wf;exact Wt]|destruct exp as[x|];[exact Sx|apply block_bt_support;exact St]]. }
  destruct Hbody as (Hbr & Hof & Wbt & Sbt).
  rewrite Ecanon, Eexp in Hbr, Hof.
  assert (Wr : WFty r) by (destruct res0 as [r0|]; [destruct (jn_tr _ _ _ _ _ Ej (proj2 (cr_uf _ _ _ _ _ _ _ Hc)) Wr0 Wbt Wx) as [_ W']; exact W' | subst r; exact Wbt]).
  assert(Sr:Support WW r)by(destruct res0 as[r0|];[exact(jn_support _ _ _ _ _ Ej Sr0 Sbt Sx)|subst r;exact Sbt]).
  destruct (IHr _ _ _ _ s1 xr cov ca res s' H Hi1 ltac:(cache_inv) ltac:(fwd_inv) ltac:(rewrite (fr_w _ _ Fs1); exact Hw) Hc Hp Ws Ss Wx Sx Wr Sr)
    as [(-> & -> & -> & -> & ->) | (xarms & covd & cad & T & R0 & -> & -> & -> & -> & Hl & Ho & WT & ST & WR & SR)].
  - (* the last arm *)
    exists [(pat, bx)], ah, ca1, bty, r. split; [reflexivity|]. split; [reflexivity|]. split; [reflexivity|].
    split; [reflexivity|]. split; [eapply A.ArmsOne; eassumption|].
    split; [destruct res0; exact Ej|]. auto.
  - (* more arms: the relation joins from the right *)
    cbn [res_ok] in Ho.
    assert (HT : exists T0, jn e exp bty T = Some T0 /\ res_ok e exp res0 T0 R0).
    { destruct res0 as [r0|].
      - destruct (jn_assoc _ _ _ _ _ _ _ Ej Ho) as (T0 & E1 & E2). eauto.
      - subst r. exists R0. split; [exact Ho | reflexivity]. }
    destruct HT as (T0 & E1 & E2). destruct (jn_tr _ _ _ _ _ E1 (proj2 (cr_uf _ _ _ _ _ _ _ Hc)) Wbt WT Wx) as [J WT0].
    pose proof(jn_support _ _ _ _ _ E1 Sbt ST Sx)as ST0.
    exists ((pat, bx) :: xarms), (ah ++ covd), (ca1 || cad), T0, R0.
    split; [rewrite <- app_assoc; reflexivity|]. split; [rewrite <- app_assoc; reflexivity|].
    split; [rewrite orb_assoc; reflexivity|]. split; [reflexivity|].
    split; [|auto].
    eapply A.ArmsMore; [| exact Hpat | exact Hbr | exact Hof | exact Hl | exact J].
    intro E. subst rest. inversion Hl.
Qed.

Lemma filter_nil_in : forall A (p : A -> bool) l x, filter p l = [] -> In x l -> p x = false.
Proof.
  intros A p l x H Hin. destruct (p x) eqn:E; [|reflexivity]. exfalso.
  assert (In x (filter p l)) by (apply filter_In; auto). rewrite H in H0. destruct H0.
Qed.

(* the heads of a well-formed type's members are the relation's *)
Lemma head_member_tr : forall sty m, WFty sty -> In m (members sty) -> (forall fs, sty <> YRec fs) ->
  A.head (trF m) = Elab.head m.
Proof.
  intros sty m W0 Hin Hr. destruct sty as [| | |n xs|ms|fs]; cbn [members] in Hin.
  - destruct Hin as [<-|[]]; reflexivity.
  - destruct Hin.
  - destruct Hin as [<-|[]]; reflexivity.
  - destruct Hin as [<-|[]]; reflexivity.
  - unfold WFty in W0. destruct ms as [|m0 r]; [discriminate W0|]. cbn [ety_wf] in W0.
    apply forallb_forall with (x := m) in W0; [|exact Hin]. destruct m; try discriminate W0. reflexivity.
  - exfalso. exact (Hr fs eq_refl).
Qed.

Lemma context_canon_tr : forall e C t,
  A.c_sub C = suf Fl (en_uf e) -> Tri (en_uf e) -> A.canon C (trF t) = trF (canon e t).
Proof. intros e C t H T. unfold A.canon. rewrite H. apply tr_canon. exact T. Qed.

Lemma match_step : forall f, OK w P R Fl f -> forall c e sc arms exp s v t s',
  synth (S f) c e (AEMatch sc arms) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> opt_wf exp -> opt_support WW exp -> Post w P R Fl s' F ->
  A.Ex WW P F C (AEMatch sc arms) (option_map (fun t => trF (canon e t)) exp) v (trF (canon e t)) /\ WFty t /\ Support WW t.
Proof.
  intros f IH c e sc arms exp s v t s' H F C Hi Ii Ifwd Hc Hx Hxs Hp.
  cbn [synth] in H. destruct (synth_wrap e exp s v t s' _ H (ok_synth_match f c e sc arms exp) Hi) as (s1 & H1 & Hi1 & Fs1 & Fs1').
  pose proof (post_back w P R Fl _ _ F Fs1' Hp) as Hp1. clear H.
  destruct f as [|f']; [discriminate H1|]. pose proof (ok_down w P R Fl f' IH) as IH'.
  cbn [synth_match] in H1.
  bi H1 as r3 s3 Hs. destruct r3 as [sx rawsty].
  set (sty := canon e rawsty) in H1.
  assert (StyFix : canon e sty = sty) by (apply canon_idem; exact (proj1 (cr_uf _ _ _ _ _ _ _ Hc))).
  okstep' Hs (ok_synth f' c e sc None) Hi Hi3 Fs3.
  bi H1 as acc s4 Ha. destruct acc as [[[xr cov] ca] res].
  change (mfold (am_step f' c e exp sty) ([], [], false, None) arms s3 = Good ((xr, cov, ca, res), s4)) in Ha.
  pose proof (proj2 (ok_mfold _ _ _ arms _ (ok_am_step f' c e exp sty) s3 _ _ Hi3 Ha)) as Fs4.
  assert (Hw : st_w s3 = w)
    by (rewrite (fr_w _ _ Fs3); destruct Hp1 as [L _]; rewrite <- (lt_w _ _ _ _ _ L); symmetry; exact (fr_w _ _ Fs1)).
  assert (Hp4 : Post w P R Fl s4 F).
  { destruct (is_never sty); [destruct arms; [injection H1 as _ _ <-; exact Hp1 | nogood H1]|].
    destruct (negb ca && _); [nogood H1 | injection H1 as _ _ <-; exact Hp1]. }
  pose proof (post_back w P R Fl _ _ F Fs4 Hp4) as Hp3.
  destruct (bs_synth _ _ _ _ _ IH' c e sc None s sx rawsty s3 Hs F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc I I Hp3) as [Es [Wraw Sraw]].
  assert (Ws : WFty sty) by (apply canon_wf; [exact (proj2 (cr_uf _ _ _ _ _ _ _ Hc))|exact Wraw]).
  assert(Ss:Support WW sty)by(apply canon_support;[exact(@es_uf WW e(cr_support _ _ _ _ _ _ _ Hc))|exact Sraw]).
  change (A.Ex WW P F C sc None sx (trF sty)) in Es.
  destruct (is_never sty) eqn:En.
  - (* a match on `|` *)
    destruct arms; [|nogood H1]. injection H1 as <- <- <-. assert (Esty : sty = YNever) by (destruct sty; try discriminate En; reflexivity).
    split; [|split;[destruct exp;[exact Hx|reflexivity]|destruct exp;[exact Hxs|constructor]]].
    eapply A.ExMatch; [exact Es|]. rewrite (context_canon_tr e C sty (cr_sub _ _ _ _ _ _ _ Hc) (proj1 (cr_uf _ _ _ _ _ _ _ Hc))), StyFix.
    rewrite Esty. destruct exp; apply A.ArmsEmptyNever.
  - match type of H1 with (if ?b0 then _ else _) _ = _ => destruct b0 eqn:Ex; [nogood H1|] end.
    change (negb ca && (negb (Nat.eqb (List.length (filter (fun h => negb (nmem (fst h) cov)) (fold_left hstep (members sty) []))) 0)
                           || Nat.eqb (List.length (fold_left hstep (members sty) [])) 0) = false) in Ex.
    injection H1 as <- <- <-.
    destruct (arms_loop f' c e exp sty F C IH' arms [] [] false None s3 xr cov ca res s4 Ha Hi3 ltac:(cache_inv) ltac:(fwd_inv) Hw Hc Hp4 Ws Ss Hx Hxs I I)
      as [(-> & -> & -> & -> & ->) | (xarms & covd & cad & T & R0 & -> & -> & -> & -> & Hl & Ho & WT & ST & WR & SR)].
    + exfalso. cbn -[sty hstep] in Ex. destruct (fold_left hstep (members sty) []); discriminate Ex.
    + rewrite StyFix in Hl. cbn [res_ok] in Ho. subst R0. cbn [app orb opt_default]. split; [|split;[exact WR|exact SR]].
      eapply A.ExMatch; [exact Es|]. rewrite (context_canon_tr e C sty (cr_sub _ _ _ _ _ _ _ Hc) (proj1 (cr_uf _ _ _ _ _ _ _ Hc))), StyFix.
      eapply A.ArmsSome; [| exact Hl |].
      * intro E. apply (never_tr Fl sty Ws) in E. rewrite E in En. discriminate En.
      * destruct cad; [left; reflexivity | right].
        cbn -[sty hstep] in Ex. apply orb_false_iff in Ex as [Em Eh]. apply negb_false_iff, Nat.eqb_eq in Em. apply Nat.eqb_neq in Eh.
        apply length_zero_iff_nil in Em.
        assert (NR : forall fs, sty <> YRec fs) by (intros fs ->; cbn in Eh; exact (Eh eq_refl)).
        split.
        -- rewrite members_tr. intro E. apply map_eq_nil in E. rewrite E in Eh. exact (Eh eq_refl).
        -- intros m h Hm Hh. rewrite members_tr in Hm. apply in_map_iff in Hm as (m0 & <- & Hm0).
           rewrite (head_member_tr sty m0 Ws Hm0 NR) in Hh.
           pose proof (heads_cover _ [] _ _ Hm0 Hh) as Hk.
           destruct (nassoc h (fold_left hstep (members sty) [])) as [mm|] eqn:Ek; [|exact (Hk eq_refl) || contradiction].
           pose proof (filter_nil_in _ _ _ _ Em (nassoc_in _ _ _ _ Ek)) as Hf. cbn in Hf.
           apply negb_false_iff, nmem_in in Hf. exact Hf.
Qed.

(* ety_ty computes `tr Fl` (the field numbers are the final ones) *)
Definition ety_ty_go_tr := @EtyTyExec.ety_ty_go_tr w P R Fl.
Definition ety_ty_eq := @EtyTyExec.ety_ty_eq.
Definition ety_ty_tr := @EtyTyExec.ety_ty_tr w P R Fl.
Definition ok_ety_ty := @EtyTyExec.ok_ety_ty.

Lemma ok_tag_theta : forall e xs, ok (tag_theta e xs).
Proof. intros e xs. unfold tag_theta. apply ok_mmap. intro x. apply ok_bind; [apply ok_ety_ty | intro; apply ok_ret]. Qed.

Lemma tag_theta_tr : forall e xs s th s', tag_theta e xs s = Good (th, s') -> Inv s -> Later w P R Fl s' -> th = trxC e xs.
Proof.
  intros e xs s th s' H Hi L. unfold tag_theta in H.
  refine (mmap_pure _ _ _ (fun p => (fst p, trF (canon e (snd p)))) (Later w P R Fl) xs s th s' H Hi L _ _ _).
  - intro x. apply ok_bind; [apply ok_ety_ty | intro; apply ok_ret].
  - intros x s0 y0 s2 _ Hi0 Hx L2. apply bind_inv in Hx as (y1 & s3 & Hy & Hx). injection Hx as <- ->.
    rewrite (ety_ty_tr _ _ _ _ Hy Hi0 L2). reflexivity.
  - intros s0 s2 Fs L2. exact (later_back w P R Fl _ _ Fs L2).
Qed.

(* ---- records ----------------------------------------------------------- *)

(* a tag's arguments, at the brackets written and the context's types *)
Definition tag_args := TagArgsCore.tag_args.

Lemma ok_tag_args : forall c e g a, ok (tag_args c e g a).
Proof. intros. unfold tag_args; apply TagArgsCore.ok_tag_args. Qed.

(* [type lemma, the other half of Track A] a tag's arguments, as the
   elaborator reads them, are V.2's: its brackets (Brackets), then each
   requirement in w_reqs order, as bound or in force. *)
Lemma Htagargs : forall c e F C g a s xs s', tag_args c e g a s = Good (xs, s') -> Inv s -> st_w s = w ->
  CxRel w R Fl c e F C -> Later w P R Fl s' -> nget (w_syms w) (gs_id g) = Some g -> gs_kind g = KTag ->
  exists bs, A.Brackets WW (A.fr_mod F) (A.c_ty C) (gs_id g) a bs /\
    Forall2 (fun r ra => fst ra = r /\ A.nlook r (bs ++ A.c_ty C) = Some (snd ra)) (A.w_reqs WW (gs_id g)) (trxC e xs) /\
    Forall (fun p => WFty (snd p)) xs.
Proof.
 intros c e F C g a s xs s' E Hi Hw Hc L Hg Kg.
 exact(tag_args_canonical w R Fl b Hload _ _ _ _ _ _ _ _ _ E Hi Hw Hc L).
Qed.

Lemma wf_nom : forall x xs, Forall (fun p => WFty (snd p)) xs -> WFty (YNom x xs).
Proof.
  intros x xs H. unfold WFty. cbn [ety_wf]. apply forallb_forall. intros p Hp.
  rewrite Forall_forall in H. exact (H p Hp).
Qed.

(* one field of a record literal *)
Definition rg_step (f' : nat) (c : cx) (e : env) (g : gsym) (ftys : list (string * ety))
    (given : list (string * xexpr)) (q : name * option aexpr) : M (list (string * xexpr)) :=
  let '(fname, val) := q in
  match sassoc fname ftys with
  | None => err (cx_mod c) ("`" ++ gs_name g ++ "` has no field `" ++ fname ++ "`")%string
  | Some fty =>
      if is_some (sassoc fname given) then err (cx_mod c) ("duplicate field `" ++ fname ++ "`")%string else
      let! (v, vt) := synth f' c e (match val with Some y => y | None => AEPath [fname] None end) (Some fty) in
      if fits e vt fty then ret (given ++ [(fname, v)])
      else let! x := show vt in let! z := show fty in
           err (cx_mod c) ("field `" ++ fname ++ "` is `" ++ x ++ "`, expected `" ++ z ++ "`")%string
  end.

Lemma ok_rg_step : forall f' c e g ftys given q, ok (rg_step f' c e g ftys given q).
Proof. intros. unfold rg_step. okt. Qed.

Lemma nodup_snoc : forall A (l : list A) x, NoDup l -> ~ In x l -> NoDup (l ++ [x]).
Proof.
  intros A l x. induction l as [|y r IH]; intros Nd Hx; cbn; [constructor; [intros [] | constructor]|].
  inversion Nd as [|? ? Hy Nr]; subst. constructor.
  - rewrite in_app_iff. intros [Hin|[E|[]]]; [exact (Hy Hin) | subst; apply Hx; left; reflexivity].
  - apply IH; [exact Nr | intro Hin; apply Hx; right; exact Hin].
Qed.

Lemma sassoc_none_notin : forall V n (l : list (string * V)), sassoc n l = None -> ~ In n (map fst l).
Proof.
  intros V n l H Hin. induction l as [|[k v] r IH]; [destruct Hin|]. cbn in H, Hin.
  destruct (String.eqb_spec n k); [discriminate | destruct Hin as [E|Hin]; [congruence | exact (IH H Hin)]].
Qed.

Definition FieldOK (f' : nat) (F : A.frame) (C : A.cx) (e : env) (ftys : list (string * ety))
    (fs : list (name * option aexpr)) (fname : string) (v : xexpr) : Prop :=
  exists val fty vt, In (fname, val) fs /\ sassoc fname ftys = Some fty /\
    A.Ex WW P F C (match val with Some y => y | None => AEPath [fname] None end) (Some (trF (canon e fty))) v (trF (canon e vt)) /\
    A.fits (trF (canon e vt)) (trF (canon e fty)).

Lemma given_loop : forall f' c e g ftys F C, OK w P R Fl f' -> forall fs given0 s given s',
  mfold (rg_step f' c e g ftys) given0 fs s = Good (given, s') ->
  Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F -> WFty (YRec ftys) -> Support WW(YRec ftys) -> NoDup (map fst given0) ->
  exists nw, given = given0 ++ nw /\ map fst nw = map fst fs /\ NoDup (map fst given) /\
    (forall fname v, In (fname, v) nw -> FieldOK f' F C e ftys fs fname v) /\
    (forall n, In n (map fst fs) -> sassoc n ftys <> None).
Proof.
  intros f' c e g ftys F C IH fs. induction fs as [|[fname val] r IHr]; intros given0 s given s' H Hi Ii Ifwd Hc Hp Wr Sr Nd.
  { cbn in H. injection H as <- _. exists []. rewrite app_nil_r. repeat split; auto; try (intros; contradiction). }
  cbn [mfold] in H. bi H as g1 s1 H1. okstep' H1 (ok_rg_step f' c e g ftys given0 (fname, val)) Hi Hi1 Fs1.
  pose proof (proj2 (ok_mfold _ _ _ r _ (ok_rg_step f' c e g ftys) s1 _ _ Hi1 H)) as Fr1.
  pose proof (post_back w P R Fl _ _ F Fr1 Hp) as Hp1.
  unfold rg_step in H1. destruct (sassoc fname ftys) as [fty|] eqn:Ef; [|nogood H1].
  destruct (sassoc fname given0) as [o|] eqn:Eg; [nogood H1|]. cbn [is_some] in H1.
  bi H1 as r2 s2 Hy. destruct r2 as [v vt].
  destruct (fits e vt fty) eqn:Efit; [|nogood H1]. injection H1 as <- ->.
  destruct (bs_synth _ _ _ _ _ IH c e _ (Some fty) s v vt s1 Hy F C Hi ltac:(cache_inv) ltac:(fwd_inv) Hc (wf_field ftys fname fty Wr Ef) (support_field ftys fname fty Sr Ef) Hp1) as [Ey [Wy Sy]].
  assert (Nd1 : NoDup (map fst (given0 ++ [(fname, v)]))).
  { rewrite map_app. cbn. exact (nodup_snoc _ _ _ Nd (sassoc_none_notin _ _ _ Eg)). }
  destruct (IHr _ s1 given s' H Hi1 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp Wr Sr Nd1) as (nw & -> & Em & Nd2 & Hf & Hn).
  exists ((fname, v) :: nw). rewrite <- app_assoc. split; [reflexivity|]. split; [cbn; f_equal; exact Em|].
  split; [rewrite app_assoc; exact Nd2|]. split.
  - intros fn v0 [E|Hin].
    + injection E as <- <-. exists val, fty, vt. split; [left; reflexivity|]. split; [exact Ef|].
      split; [exact Ey | exact (fits_tr Fl e vt fty (cr_uf _ _ _ _ _ _ _ Hc) Efit)].
    + destruct (Hf fn v0 Hin) as (val0 & fty0 & vt0 & Hin0 & R0). exists val0, fty0, vt0. split; [right; exact Hin0 | exact R0].
  - intros n [<-|Hin]; [cbn; rewrite Ef; discriminate | exact (Hn n Hin)].
Qed.

Lemma sassoc_nodup_in : forall V n (l : list (string * V)) v, NoDup (map fst l) -> In (n, v) l -> sassoc n l = Some v.
Proof.
  intros V n l v Nd Hin. induction l as [|[k x] r IH]; [destruct Hin|]. cbn in Nd |- *. inversion Nd as [|? ? Hk Nr]; subst.
  destruct Hin as [E|Hin].
  - injection E as <- <-. rewrite String.eqb_refl. reflexivity.
  - destruct (String.eqb_spec n k) as [<-|]; [exfalso; apply Hk; exact (in_map fst _ _ Hin) | exact (IH Nr Hin)].
Qed.

Lemma sassoc_some_in : forall V n (l : list (string * V)) v, sassoc n l = Some v -> In (n, v) l.
Proof.
  intros V n l v H. induction l as [|[k x] r IH]; [discriminate|]. cbn in H.
  destruct (String.eqb_spec n k) as [<-|]; [injection H as <-; left; reflexivity | right; exact (IH H)].
Qed.

Lemma Forall2_map_both : forall A B C0 (R0 : B -> C0 -> Prop) (h1 : A -> B) (h2 : A -> C0) l,
  (forall x, In x l -> R0 (h1 x) (h2 x)) -> Forall2 R0 (map h1 l) (map h2 l).
Proof. intros A B C0 R0 h1 h2 l H. induction l as [|x r IH]; constructor; [apply H; left; reflexivity | apply IH; intros; apply H; right; assumption]. Qed.

Lemma record_step : forall f, OK w P R Fl f -> forall c e pa a fs exp s v t s',
  synth (S f) c e (AERec pa a fs) exp s = Good ((v, t), s') ->
  forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> opt_wf exp -> opt_support WW exp -> Post w P R Fl s' F ->
  A.Ex WW P F C (AERec pa a fs) (option_map (fun t => trF (canon e t)) exp) v (trF (canon e t)) /\ WFty t /\ Support WW t.
Proof.
  intros f IH c e pa a fs exp s v t s' H F C Hi Ii Ifwd Hc Hx HexpS Hp.
  cbn [synth] in H. destruct (synth_wrap e exp s v t s' _ H (ok_synth_record f c e pa a fs) Hi) as (s1 & H1 & Hi1 & Fs1 & Fs1').
  pose proof (post_back w P R Fl _ _ F Fs1' Hp) as Hp1. clear H.
  destruct f as [|f']; [discriminate H1|]. pose proof (ok_down w P R Fl f' IH) as IH'.
  cbn [synth_record] in H1.
  pose proof (cr_mod_in _ _ _ _ _ _ _ Hc) as Hmi. pose proof (cr_mod _ _ _ _ _ _ _ Hc) as Hm.
  assert (Hw : st_w s = w).
  { destruct Hp1 as [L _]. rewrite <- (lt_w _ _ _ _ _ L). symmetry. exact (fr_w _ _ Fs1). }
  bi H1 as r s2 Hr. destruct (resolve_sym_rel _ pa s r s2 Hw Hmi Hi Hr) as [-> Hres].
  destruct r as [g|]; [|nogood H1]. destruct Hres as [Hres Hg].
  destruct (negb (kind_eqb (gs_kind g) KTag)) eqn:Kg; [nogood H1|]. apply negb_false_iff, kind_eqb_spec in Kg.
  bi H1 as tm s3 Htm. bi H1 as xs s4 Hxs.
  assert (Hta : tag_args c e g a s = Good (xs, s4)).
  { unfold tag_args, TagArgsCore.tag_args, mbind at 1.
    match goal with |- match ?X with _ => _ end = _ => replace X with (Good (tm, s3)) by (symmetry; exact Htm) end.
    exact Hxs. }
  clear Htm Hxs. okstep' Hta (ok_tag_args c e g a) Hi Hi4 Fs4.
  bi H1 as pay s5 Hpay. okstep' Hpay (ok_payload_type (YNom (gs_id g) xs) e) Hi4 Hi5 Fs5.
  destruct pay as [| | | | |ftys]; try nogood H1.
  bi H1 as given s6 Hgv.
  change (mfold (rg_step f' c e g ftys) [] fs s5 = Good (given, s6)) in Hgv.
  okstep' Hgv (ok_mfold _ _ _ fs [] (ok_rg_step f' c e g ftys)) Hi5 Hi6 Fs6.
  destruct (filter _ ftys) as [|q0 qs] eqn:Emiss; [|nogood H1].
  destruct (negb (Nat.eqb (List.length fs) (List.length ftys))) eqn:Elen; [nogood H1|].
  apply negb_false_iff, Nat.eqb_eq in Elen.
  bi H1 as xfs s7 Hxfs. match type of Hxfs with ?m s6 = _ => assert (Okx : ok m) by okt end.
  okstep' Hxfs Okx Hi6 Hi7 Fs7.
  bi H1 as th s8 Hth. okstep' Hth (ok_tag_theta e xs) Hi7 Hi8 Fs8. injection H1 as <- <- <-.
  pose proof (post_back w P R Fl _ _ F Fs8 Hp1) as Hp7. pose proof (post_back w P R Fl _ _ F Fs7 Hp7) as Hp6.
  pose proof (post_back w P R Fl _ _ F Fs6 Hp6) as Hp5. pose proof (post_back w P R Fl _ _ F Fs5 Hp5) as Hp4.
  destruct (Htagargs c e F C g a s xs s4 Hta Hi Hw Hc (proj1 Hp4) Hg Kg) as (bs & Hbr & Hreq & Wxs).
  pose proof(tag_args_support w R Fl b Hload _ _ _ _ _ _ _ _ _ Hta Hi Hw Hc (proj1 Hp4)Hg Kg)as Snom.
  assert (Hw4 : st_w s4 = w) by (rewrite (fr_w _ _ Fs4); exact Hw).
  destruct (Hpayload c e F C (gs_id g) xs s4 (YRec ftys) s5 Hpay Hi4 Hw4 Hc (proj1 Hp5) (ex_intro _ g (conj Hg Kg)) (wf_nom _ _ Wxs) Snom) as [Hpo [Wp Sp]].
  pose proof (payload_subst WW (suf Fl (en_uf e)) _ _ _ Hpo) as HpoC.
  rewrite smap_trx, tr_canon in HpoC by exact (proj1 (cr_uf _ _ _ _ _ _ _ Hc)).
  clear Hpo. rename HpoC into Hpo.
  destruct (given_loop f' c e g ftys F C IH' fs [] s5 given s6 Hgv Hi5 ltac:(cache_inv) ltac:(fwd_inv) Hc Hp6 Wp Sp (NoDup_nil _))
    as (nw & E0 & Enames & Ndg & Hok & Hin). cbn in E0. subst nw.
  (* every field of the payload is given *)
  assert (Hall : forall q, In q ftys -> exists v0, sassoc (fst q) given = Some v0).
  { intros q Hq. destruct (sassoc (fst q) given) as [v0|] eqn:E; [eauto|]. exfalso.
    assert (In q (filter (fun q => negb (is_some (sassoc (fst q) given))) ftys)) by (apply filter_In; rewrite E; auto).
    rewrite Emiss in H. destruct H. }
  (* so the payload names each field once *)
  assert (Ndf : NoDup (map fst ftys)).
  { apply (@NoDup_incl_NoDup _ (map fst fs)).
    - rewrite <- Enames. exact Ndg.
    - rewrite !length_map. lia.
    - intros n Hn. destruct (sassoc n ftys) as [ty|] eqn:E; [|exfalso; exact (Hin n Hn E)].
      exact (in_map fst _ _ (sassoc_some_in _ _ _ _ E)). }
  (* the fields, in the payload's order *)
  assert (Exf : xfs = map (fun q => (Fl (fst q), opt_default XUnit (sassoc (fst q) given))) ftys).
  { refine (mmap_pure _ _ _ _ (Later w P R Fl) ftys s6 xfs s7 Hxfs Hi6 (proj1 Hp7) _ _ _).
    - intro q. okt.
    - intros q s0 y0 s9 _ Hi0 Hq L9. bi Hq as i s10 Hf. injection Hq as <- <-.
      rewrite (fld_id_final _ s0 i s10 s10 Hi0 Hf (fr_refl _) L9). reflexivity.
    - intros s0 s9 Fs L9. exact (later_back w P R Fl _ _ Fs L9). }
  rewrite (tag_theta_tr e xs s7 th s8 Hth Hi7 (proj1 Hp1)).
  split; [|split;[exact(wf_nom _ _ Wxs)|exact Snom]].
  rewrite tr_canon_nom.
  refine (@A.ExRecord WW P F C _ pa a fs (gs_id g) bs (trxC e xs) (trF (canon e (YRec ftys)))
            (map (fun p => (Fl (fst p), trF (canon e (snd p)))) ftys) _ _ _ _ _ _ _ _ _ _);
    [unfold A.resolve_sym; rewrite Hm, Hres; reflexivity | rewrite (kind_read _ g Hg), Kg; reflexivity
    | exact Hbr | exact Hreq | exact Hpo | apply tr_canon_rec | rewrite Enames in Ndg; exact Ndg | rewrite length_map; exact Elen |].
  rewrite Exf. apply Forall2_map_both. intros [n ty] Hq. cbn [fst snd]. split; [reflexivity|].
  destruct (Hall _ Hq) as [v0 Ev]. cbn in Ev. rewrite Ev. cbn [opt_default].
  destruct (Hok n v0 (sassoc_some_in _ _ _ _ Ev)) as (val & fty & vt & Hinf & Ef & Ey & Hfit).
  rewrite (sassoc_nodup_in _ n ftys ty Ndf Hq) in Ef. injection Ef as ->.
  exists n, val, (trF (canon e vt)). split; [reflexivity|]. split; [exact Hinf|]. split; [exact Ey | exact Hfit].
Qed.

End Data.
