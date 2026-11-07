(* ------------------------------------------------------------------------ *)
(* BodyTy.v — elab_type against Part V's TyOf (V.2) (Track A, BodySound's   *)
(* pd_ty).                                                                   *)
(*                                                                           *)
(* elab_type, app_subst and symbol_type are related together, by induction  *)
(* on fuel, to TyOf and Brackets, for any type meanings `tm` that agree     *)
(* with the relation's `tmr` (TmRel: lookups agree under `tr`, meanings are *)
(* well formed, and tm covers the environment's own, which app_subst reads *)
(* behind it).  A nominal's arguments are its requirements in the reading's *)
(* order: the list `reqs` reads is the final state's (ReqsStable.v), which  *)
(* is the reading's (BodySoundCore.Later's lt_reqs).                            *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsStable ElabFrameCore BodySoundCore Merge CanonTypes TyOfSubst.
Import ListNotations.
Local Open Scope list_scope.

Module A := Audit.Elaboration.

Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].

Local Lemma miter_each : forall X (f : X -> M unit) l s u s',
  (forall x, In x l -> forall u s', f x s = Good (u, s') -> s' = s) ->
  miter f l s = Good (u, s') -> forall x, In x l -> exists u', f x s = Good (u', s).
Proof.
  intros X f l. induction l as [|x r IH]; intros s u s' Hr H y Hy; [destruct Hy|]. cbn [miter] in H.
  apply bind_inv in H as (u1 & s1 & H1 & H2). pose proof (Hr x (or_introl eq_refl) u1 s1 H1) as ->.
  destruct Hy as [<-|Hy]; [exists u1; exact H1|].
  exact (IH s u s' (fun z Hz => Hr z (or_intror Hz)) H2 y Hy).
Qed.

Section Ty.
Variable w : Elab.world. Context {P : xprogram}. Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.

Notation WW := (BodySoundCore.W w R Fl).
Notation trF := (tr Fl).
Notation path_of mi := (gm_path (get_mod w mi)).

Lemma loader_nonzero : forall x g, nget (w_syms w) x = Some g -> x <> 0.
Proof.
  intros x g H. destruct (load_world_reads b w R Fl Hload) as [_ L2 _ _ _ _].
  exact (proj1 (L2 x _ (w_decl_read w R Fl x g H))).
Qed.

Lemma loader_mod_in : forall x g, nget (w_syms w) x = Some g -> In (gs_mod g) (w_order w).
Proof.
  intros x g H. destruct (load_world_inv b w Hload) as (ls & e & HI & ->).
  exact (proj2 (proj2 (loader_decl b ls e HI R Fl x g H))).
Qed.

(* the elaborator's resolve_sym, as the relation's *)
Lemma esym_rel : forall mi pa s r s', Inv s -> st_w s = w -> In mi (w_order w) ->
  Elab.resolve_sym mi pa s = Good (r, s') ->
  s' = s /\ match r with
            | Some g => A.resolve_sym WW (path_of mi) pa = Some (gs_id g) /\ nget (w_syms w) (gs_id g) = Some g
            | None => A.resolve_sym WW (path_of mi) pa = None
            end.
Proof.
  intros mi pa s r s' Hi Hw Hmi H. unfold Elab.resolve_sym in H. apply bind_inv in H as (r0 & s1 & H1 & H).
  destruct (rpath_rel w R Fl b Hload mi pa s r0 s1 Hw Hmi H1) as [-> Hres].
  unfold A.resolve_sym. change (A.resolve (W_read w R Fl)) with (A.resolve WW). rewrite Hres.
  destruct r0 as [[x|j]|]; cbn [option_map ent_of].
  - apply bind_inv in H as (g & s2 & H2 & H). injection H as <- <-.
    destruct (sym_loader w R Fl b Hload _ _ x s g s2 Hi Hw Hres H2) as [-> Hg].
    rewrite (load_world_ids b w Hload x g Hg). auto.
  - injection H as <- <-. auto.
  - injection H as <- <-. auto.
Qed.

(* reqs, without letting the kernel unfold its fuel *)
Lemma reqs_eq : forall x, reqs x = type_requirements 200 x.
Proof. reflexivity. Qed.

Lemma ok_reqs : forall x, ok (reqs x).
Proof. intro x. rewrite reqs_eq. apply ok_type_requirements. Qed.

(* a requirement list read is the reading's *)
Lemma reqs_R : forall x s rs s', Inv s -> reqs x s = Good (rs, s') -> Later w P R Fl s' -> R x = rs.
Proof.
  intros x s rs s' Hi H L. apply (lt_reqs _ _ _ _ _ L).
  rewrite reqs_eq in H.
  revert H. generalize 200. intros n H.
  exact (reqs_cached n x s rs s' (fun g Hg => iv_ids _ Hi x g Hg) H).
Qed.

(* ---- the type meanings in force ---- *)

Definition TmRel (tm : list (nat * ety)) (tmr : list (nat * ty)) (e : env) : Prop :=
  (forall k, A.nlook k tmr = option_map trF (nassoc k tm)) /\
  (forall k y, nassoc k tm = Some y -> WFty y) /\
  (forall k, nassoc k tm = None -> tn_get e k = None).

Lemma nassoc_app : forall V k (l m : list (nat * V)), nassoc k (l ++ m) = match nassoc k l with Some v => Some v | None => nassoc k m end.
Proof. intros V k l m. induction l as [|[a v] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb k a); [reflexivity | exact IH]. Qed.

Lemma nlook_app : forall V k (l m : list (nat * V)), A.nlook k (l ++ m) = match A.nlook k l with Some v => Some v | None => A.nlook k m end.
Proof. intros V k l m. induction l as [|[a v] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb k a); [reflexivity | exact IH]. Qed.

Definition trp (q : nat * ety) : nat * ty := (fst q, trF (snd q)).

Lemma nlook_trp : forall k l, A.nlook k (map trp l) = option_map trF (nassoc k l).
Proof. intros k l. induction l as [|[a v] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb k a); [reflexivity | exact IH]. Qed.

Lemma tmrel_tymap : forall e, (forall t y, tn_get e t = Some y -> WFty y) ->
  forall tmr, (forall k, A.nlook k tmr = option_map trF (tn_get e k)) -> TmRel (tymap e) tmr e.
Proof.
  intros e Hw tmr Ht.
  assert (E : forall k, nassoc k (tymap e) = tn_get e k).
  { intro k. unfold tymap, tn_get. induction (en_tn e) as [|[a [y st0]] r IH]; [reflexivity|]. cbn.
    destruct (Nat.eqb k a); [reflexivity | exact IH]. }
  split; [intro k; rewrite E; apply Ht|]. split; [intros k y H; rewrite E in H; exact (Hw k y H)|].
  intros k H. rewrite E in H. exact H.
Qed.

Lemma tmrel_app : forall l lr tm tmr e, TmRel tm tmr e -> map trp l = lr -> Forall (fun q => WFty (snd q)) l ->
  TmRel (l ++ tm) (lr ++ tmr) e.
Proof.
  intros l lr tm tmr e [T1 [T2 T3]] El Wl. subst lr. split; [|split].
  - intro k. rewrite nlook_app, nassoc_app, nlook_trp. destruct (nassoc k l); [reflexivity | apply T1].
  - intros k y H. rewrite nassoc_app in H. destruct (nassoc k l) as [v|] eqn:E; [|exact (T2 k y H)].
    injection H as <-. clear -E Wl. induction Wl as [|[a v'] r Hv Hr IH]; [discriminate|]. cbn in E.
    destruct (Nat.eqb k a); [injection E as <-; exact Hv | exact (IH E)].
  - intros k H. rewrite nassoc_app in H. destruct (nassoc k l); [discriminate | exact (T3 k H)].
Qed.

(* the brackets' right sides read with the environment's meanings behind *)
Lemma tmrel_outer : forall tm tmr e, TmRel tm tmr e -> TmRel (tm ++ tymap e) tmr e.
Proof.
  intros tm tmr e [T1 [T2 T3]].
  assert (E : forall k, nassoc k (tm ++ tymap e) = nassoc k tm).
  { intro k. rewrite nassoc_app. destruct (nassoc k tm) eqn:E; [reflexivity|].
    rewrite <- (T3 k E). unfold tymap, tn_get. induction (en_tn e) as [|[a [y st0]] r IH]; [reflexivity|]. cbn.
    destruct (Nat.eqb k a); [reflexivity | exact IH]. }
  split; [intro k; rewrite E; apply T1|]. split; [intros k y H; rewrite E in H; exact (T2 k y H)|].
  intros k H. rewrite E in H. exact (T3 k H).
Qed.

(* ---- frames of the steps ---- *)

Lemma later_step : forall A (m : M A) s a s', ok m -> Inv s -> m s = Good (a, s') -> Inv s' /\ Fr s s'.
Proof. intros A m s a s' Hm Hi H. exact (Hm s a s' Hi H). Qed.

Lemma later_from : forall s s', Fr s s' -> Later w P R Fl s' -> Later w P R Fl s.
Proof. exact (later_back w P R Fl). Qed.


Ltac noerr' H := exfalso;
  solve [repeat (cbn beta iota zeta in H;
                 first [ exact (err_not_good _ _ _ _ _ _ H) | discriminate H
                       | apply bind_inv in H as (? & ? & ? & H) ])].

Lemma fr_w' : forall s s', Fr s s' -> st_w s = w -> st_w s' = w.
Proof. intros s s' F E. rewrite (fr_w _ _ F). exact E. Qed.

(* a list mapped by an `ok` step, related element by element *)
Lemma mmap_ok_rel : forall X Y (f : X -> M Y) (Q : X -> Y -> Prop) xs s ys s',
  (forall x, ok (f x)) ->
  (forall x, In x xs -> forall s y s1, f x s = Good (y, s1) -> Inv s -> st_w s = w -> Later w P R Fl s1 -> Q x y) ->
  mmap f xs s = Good (ys, s') -> Inv s -> st_w s = w -> Later w P R Fl s' -> Forall2 Q xs ys.
Proof.
  intros X Y f Q xs. induction xs as [|x r IH]; intros s ys s' Hok HQ H Hi Hw L; cbn [mmap] in H.
  - injection H as <- <-. constructor.
  - apply bind_inv in H as (y & s1 & H1 & H). destruct (Hok x s y s1 Hi H1) as [Hi1 F1].
    apply bind_inv in H as (ys' & s2 & H2 & H). injection H as <- <-.
    destruct (ok_mmap _ _ f r Hok s1 ys' s2 Hi1 H2) as [_ F2].
    constructor.
    + exact (HQ x (or_introl eq_refl) s y s1 H1 Hi Hw (later_from _ _ F2 L)).
    + exact (IH s1 ys' s2 Hok (fun z Hz => HQ z (or_intror Hz)) H2 Hi1 (fr_w' _ _ F1 Hw) L).
Qed.

Lemma rts_spec : forall mi p s g s', Inv s -> st_w s = w -> In mi (w_order w) ->
  resolve_type_symbol mi p s = Good (g, s') ->
  s' = s /\ A.resolve_sym WW (path_of mi) p = Some (gs_id g) /\ nget (w_syms w) (gs_id g) = Some g /\
  (gs_kind g = KType \/ gs_kind g = KUnit \/ gs_kind g = KTag \/ gs_kind g = KAlias).
Proof.
  intros mi p s g s' Hi Hw Hmi H. unfold resolve_type_symbol in H. apply bind_inv in H as (r & s1 & H1 & H).
  destruct (rpath_rel w R Fl b Hload mi p s r s1 Hw Hmi H1) as [-> Hres].
  destruct r as [[x|j]|]; [|noerr' H|noerr' H].
  apply bind_inv in H as (g0 & s2 & H2 & H). cbn [option_map ent_of] in Hres.
  destruct (sym_loader w R Fl b Hload _ _ x s g0 s2 Hi Hw Hres H2) as [-> Hg].
  pose proof (load_world_ids b w Hload x g0 Hg) as Eid.
  assert (Rs : A.resolve_sym WW (path_of mi) p = Some x)
    by (unfold A.resolve_sym; rewrite Hres; reflexivity).
  destruct (gs_kind g0) eqn:K; try noerr' H; injection H as <- <-;
    (split; [reflexivity|]; rewrite Eid; split; [exact Rs|]; split; [exact Hg|]; rewrite K; auto).
Qed.

Lemma kind_sym_rd : forall x g, nget (w_syms w) x = Some g -> A.kind_sym WW x = Some (kconv (gs_kind g)).
Proof. intros x g H. exact (kind_sym_read b w R Fl Hload x g H). Qed.

Lemma w_decl_rd : forall x g, nget (w_syms w) x = Some g ->
  A.w_decl WW x = Some (A.Decl (path_of (gs_mod g)) (gs_decl g) (gs_assumes g)).
Proof. intros x g H. exact (w_decl_read w R Fl x g H). Qed.

Lemma kind_type_of : forall x g, nget (w_syms w) x = Some g -> A.kind_sym WW x = Some A.DKType -> gs_kind g = KType.
Proof.
  intros x g Hg Hk. rewrite (kind_sym_rd x g Hg) in Hk. injection Hk as Hk. exact (kconv_type _ Hk).
Qed.

Lemma check_relevant_spec : forall mi g bs s u s', Inv s -> st_w s = w -> In mi (w_order w) ->
  check_relevant mi g bs s = Good (u, s') -> Later w P R Fl s' ->
  forall b0 x, In b0 bs -> A.resolve_sym WW (path_of mi) (fst b0) = Some x -> A.kind_sym WW x = Some A.DKType ->
  In x (R (gs_id g)).
Proof.
  intros mi g bs s u s' Hi Hw Hmi H L b0 x Hb Hr Hk. unfold check_relevant in H.
  apply bind_inv in H as (rs & s1 & H1 & H).
  destruct (ok_reqs _ s rs s1 Hi H1) as [Hi1 F1]. pose proof (fr_w' _ _ F1 Hw) as Hw1.
  match type of H with miter ?fn _ _ = _ =>
    assert (Rd : forall b1, In b1 bs -> forall u0 s2, fn b1 s1 = Good (u0, s2) -> s2 = s1) end.
  { intros b1 _ u0 s2 E. cbv beta in E. apply bind_inv in E as (l & s3 & E1 & E). rewrite (resolve_sym_reader _ _ _ _ _ E1) in E.
    destruct l as [lg|]; [|injection E as _ <-; reflexivity].
    destruct (kind_eqb (gs_kind lg) KType && negb (nmem (gs_id lg) rs)); [noerr' E | injection E as _ <-; reflexivity]. }
  pose proof (miter_each _ _ _ _ _ _ Rd H) as Each.
  assert (Es : s' = s1).
  { clear -Rd H. revert s1 u s' Rd H. induction bs as [|b1 r IH]; intros s1 u s' Rd H; cbn [miter] in H.
    - injection H as _ <-. reflexivity.
    - apply bind_inv in H as (u1 & s2 & H1 & H2). rewrite (Rd b1 (or_introl eq_refl) u1 s2 H1) in H2.
      exact (IH s1 u s' (fun z Hz => Rd z (or_intror Hz)) H2). }
  subst s'. rewrite (reqs_R (gs_id g) s rs s1 Hi H1 L).
  destruct (Each b0 Hb) as (u' & E). cbv beta in E. apply bind_inv in E as (l & s3 & E1 & E).
  destruct (esym_rel mi (fst b0) s1 l s3 Hi1 Hw1 Hmi E1) as [-> Hl].
  destruct l as [lg|]; [|rewrite Hl in Hr; discriminate].
  destruct Hl as [Hl Hg]. rewrite Hr in Hl. injection Hl as ->.
  rewrite (kind_type_of _ _ Hg Hk) in E. cbn [kind_eqb andb] in E.
  destruct (nmem (gs_id lg) rs) eqn:Em; [apply nmem_in; exact Em | noerr' E].
Qed.

(* ---- unions ---- *)

Definition UInv (out : list ety) : Prop :=
  Forall (fun x => exists h xs, x = YNom h xs /\ h <> 0) out /\ NoDup (map Elab.head out).

Lemma union_inner : forall mi l out s out' s', UInv out ->
  mfold (fun out x =>
           match x with
           | YAbs _ => let! s := show x in
                       err mi ("union member `" ++ s ++ "` is abstract and not constrained to a nominal type (D15)")
           | YNom h _ =>
               if Nat.eqb h 0 then err mi "union member is not nominal (D15)" else
               if ety_mem x out then let! s := show x in err mi ("duplicate union member `" ++ s ++ "`")
               else
                 match find (fun y => match y with YNom h' _ => Nat.eqb h h' | _ => false end) out with
                 | Some y => let! s := show x in let! s' := show y in
                             err mi ("union members `" ++ s' ++ "` and `" ++ s ++ "` share a head (D15)")
                 | None => ret (out ++ [x])
                 end
           | _ => let! s := show x in err mi ("union member `" ++ s ++ "` is not nominal (D15)")
           end) out l s = Good (out', s') ->
  out' = out ++ l /\ UInv out' /\ s' = s.
Proof.
  intros mi l. induction l as [|x r IH]; intros out s out' s' U H; cbn [mfold] in H.
  - injection H as <- <-. rewrite app_nil_r. split; [reflexivity|]. split; [exact U | reflexivity].
  - apply bind_inv in H as (o1 & s1 & H1 & H).
    destruct x as [| |g|h xs|ms|fs]; try noerr' H1.
    destruct (Nat.eqb_spec h 0) as [_|Hh]; [noerr' H1|].
    destruct (ety_mem (YNom h xs) out); [noerr' H1|].
    destruct (find _ out) as [y|] eqn:Ef; [noerr' H1|]. injection H1 as <- <-.
    assert (U1 : UInv (out ++ [YNom h xs])).
    { destruct U as [U1 U2]. split; [apply Forall_app; split; [exact U1 | constructor; [exists h, xs; auto | constructor]]|].
      rewrite map_app. cbn. apply NoDup_app; [exact U2 | constructor; [intros []|constructor] |].
      intros hd Hin [<-|[]]. apply in_map_iff in Hin as (y & Ey & Hy).
      rewrite Forall_forall in U1. destruct (U1 y Hy) as (h' & xs' & -> & _). cbn in Ey. injection Ey as ->.
      pose proof (find_none _ _ Ef (YNom h xs') Hy) as E. cbn in E. rewrite Nat.eqb_refl in E. discriminate. }
    destruct (IH _ _ _ _ U1 H) as (-> & U2 & ->). split; [rewrite <- app_assoc; reflexivity|]. split; [exact U2 | reflexivity].
Qed.

Lemma head_tr : forall x, (exists h xs, x = YNom h xs /\ h <> 0) -> A.head (trF x) = Elab.head x.
Proof. intros x (h & xs & -> & _). reflexivity. Qed.

(* a union the elaborator makes from well-formed members' members, each a
   tag or unit application, is well formed (BodySoundCore.ety_wf) *)
Lemma members_wf : forall y, WFty y -> Forall WFty (Elab.members y).
Proof.
  intros y Wy. destruct y as [| |n|n xs|ms|fs]; cbn [Elab.members]; try (constructor; [exact Wy | constructor]).
  - constructor.
  - unfold WFty in Wy. cbn [ety_wf] in Wy. destruct ms as [|m0 r]; [discriminate|].
    rewrite Forall_forall. intros m Hm. pose proof (proj1 (forallb_forall _ _) Wy m Hm) as Wm.
    destruct m; try discriminate Wm. exact Wm.
Qed.

Lemma union_wf : forall ys out, Forall WFty ys -> out = flat_map Elab.members ys ->
  Forall (fun x => exists h xs, x = YNom h xs /\ h <> 0) out ->
  WFty (match out with [] => YNever | _ => YUnion out end).
Proof.
  intros ys out Wy Eo U.
  assert (Wm : Forall WFty out).
  { subst out. clear U. induction Wy as [|y r Hy Hr IH]; [constructor|]. cbn [flat_map]. apply Forall_app.
    split; [exact (members_wf y Hy) | exact IH]. }
  destruct out as [|m0 r]; [reflexivity|].
  change (forallb (fun m => match m with YNom _ _ => ety_wf m | _ => false end) (m0 :: r) = true).
  apply forallb_forall. intros m Hm. rewrite Forall_forall in U, Wm.
  destruct (U m Hm) as (h & xs & -> & _). exact (Wm _ Hm).
Qed.

(* ---- the judgments ---- *)

Definition ETyOK (f : nat) : Prop :=
  forall t mi tm e this s d s' tmr, elab_type f t mi tm e this s = Good (d, s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> TmRel tm tmr e -> opt_wf this -> Later w P R Fl s' ->
  A.TyOf WW (path_of mi) tmr (option_map trF this) t (trF d) /\ WFty d.

Definition BrRel (p : string) (tmr : list (nat * ty)) (b0 : path * spec) (tb : nat * ty) : Prop :=
  exists sp sa, snd b0 = Spec (Some sp) None sa /\
    A.resolve_sym WW p (fst b0) = Some (fst tb) /\ A.kind_sym WW (fst tb) = Some A.DKType /\
    A.TyOf WW p tmr None (ATRef sp sa) (snd tb).

Definition SubstOK (f : nat) : Prop :=
  forall e mi bs outer s l s' tmr, app_subst f e mi bs outer s = Good (l, s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> TmRel (outer ++ tymap e) tmr e -> Later w P R Fl s' ->
  exists lr, Forall2 (BrRel (path_of mi) tmr) bs lr /\ map trp l = rev lr /\ Forall (fun q => WFty (snd q)) l.

Definition SymOK (f : nat) : Prop :=
  forall mi g tm e s d s' tmr, symbol_type f mi g tm e s = Good (d, s') ->
  Inv s -> st_w s = w -> nget (w_syms w) (gs_id g) = Some g -> TmRel tm tmr e -> Later w P R Fl s' ->
  WFty d /\
  match gs_kind g with
  | KType => A.nlook (gs_id g) tmr = Some (trF d)
  | KUnit | KTag => exists args, trF d = TNom (gs_id g) args /\
      Forall2 (fun r ra => fst ra = r /\ A.nlook r tmr = Some (snd ra)) (R (gs_id g)) args
  | KAlias => exists n body, gs_decl g = ADAlias n body /\ A.TyOf WW (path_of (gs_mod g)) tmr None body (trF d)
  | _ => False
  end.


Lemma ty_zero : ETyOK 0 /\ SubstOK 0 /\ SymOK 0.
Proof. split; [|split]; intros until 1; discriminate. Qed.

Lemma tr_map_trp : forall l, map (fun p => (fst p, trF (snd p))) l = map trp l.
Proof. reflexivity. Qed.

Lemma ty_step : forall f, ETyOK f -> SubstOK f -> SymOK f -> ETyOK (S f) /\ SubstOK (S f) /\ SymOK (S f).
Proof.
  intros f IE IS IY. split; [|split].
  - (* elab_type *)
    intros t mi tm e this s d s' tmr H Hi Hw Hmi Ht Wth L. cbn [elab_type] in H.
    destruct t as [| | |fs|ms|p a].
    + injection H as <- <-. split; [apply A.TyNever | reflexivity].
    + destruct this as [y|]; [|noerr' H]. injection H as <- <-. split; [apply A.TyThis | exact Wth].
    + injection H as <- <-. split; [apply A.TyUnit | reflexivity].
    + (* a record *)
      apply bind_inv in H as (fs' & s1 & H1 & H). injection H as <- <-.
      assert (Q : Forall2 (fun nt q => fst q = fst nt /\ A.TyOf WW (path_of mi) tmr (option_map trF this) (snd nt) (trF (snd q)) /\ WFty (snd q)) fs fs').
      { refine (mmap_ok_rel _ _ _ _ fs s fs' s1 _ _ H1 Hi Hw L).
        - intro x. apply ok_bind; [apply ok_elab_type | intro; apply ok_ret].
        - intros [n0 t0] _ s2 q s3 E Hi2 Hw2 L3. apply bind_inv in E as (y & s4 & E1 & E). injection E as <- <-.
          destruct (ok_elab_type _ _ _ _ _ _ s2 y s4 Hi2 E1) as [_ F4].
          split; [reflexivity|]. exact (IE _ _ _ _ _ _ _ _ _ E1 Hi2 Hw2 Hmi Ht Wth L3). }
      split.
      { cbn [tr]. apply A.TyRec. clear -Q. induction Q as [|[n0 t0] [n1 y] r r' [E1 [Ty _]] Hr IHr]; constructor; [|exact IHr].
        cbn in *. subst n1. split; [reflexivity | exact Ty]. }
      { unfold WFty. cbn [ety_wf]. apply forallb_forall. intros q Hq. clear -Q Hq.
        induction Q as [|nt q' r r' [_ [_ Wq]] Hr IHr]; [destruct Hq|].
        destruct Hq as [<-|Hq]; [exact Wq | exact (IHr Hq)]. }
    + (* a union *)
      apply bind_inv in H as (out & s1 & H1 & H). injection H as <- <-.
      match type of H1 with mfold ?fn _ _ _ = _ =>
      assert (Gen : forall ms1 out0 s0 out1 s2, mfold fn out0 ms1 s0 = Good (out1, s2) -> Inv s0 -> st_w s0 = w ->
                Later w P R Fl s2 -> UInv out0 ->
                exists ys, Forall2 (fun m y => A.TyOf WW (path_of mi) tmr (option_map trF this) m (trF y) /\ WFty y) ms1 ys /\
                           out1 = out0 ++ flat_map Elab.members ys /\ UInv out1) end.
      { induction ms1 as [|m r IH]; intros out0 s0 out1 s2 E Hi0 Hw0 L2 U0; cbn [mfold] in E.
        - injection E as <- <-. exists []. rewrite app_nil_r. auto.
        - apply bind_inv in E as (o1 & s3 & E1 & E). apply bind_inv in E1 as (y & s4 & E2 & E1).
          destruct (ok_elab_type _ _ _ _ _ _ s0 y s4 Hi0 E2) as [Hi4 F4].
          destruct (union_inner mi _ _ _ _ _ U0 E1) as (-> & U1 & ->).
          destruct (IH _ _ _ _ E Hi4 (fr_w' _ _ F4 Hw0) L2 U1) as (ys & Q & -> & U2).
          assert (L4 : Later w P R Fl s4).
          { refine (later_from _ _ _ L2). refine (proj2 (ok_mfold _ _ _ r _ _ _ _ _ Hi4 E)).
            intros a0 x. apply ok_bind; [apply ok_elab_type|]. intro y0. apply ok_mfold. intros a1 x1.
            repeat (match goal with |- ok (match ?x with _ => _ end) => destruct x end); okt. }
          exists (y :: ys). split; [constructor; [exact (IE _ _ _ _ _ _ _ _ _ E2 Hi0 Hw0 Hmi Ht Wth L4) | exact Q]|].
          split; [cbn; rewrite app_assoc; reflexivity | exact U2]. }
      destruct (Gen _ [] s out s1 H1 Hi Hw L (conj (Forall_nil _) (NoDup_nil _))) as (ys & Q & Eo & [U1 U2]).
      cbn [app] in Eo.
      assert (Etr : trF (match out with [] => YNever | _ => YUnion out end) = TUnion (map trF out)) by (destruct out; reflexivity).
      rewrite Etr. split; [|refine (union_wf ys out _ Eo U1); clear -Q; induction Q as [|m y r r' Hy Hr IHr]; constructor; [exact (proj2 Hy) | exact IHr]].
      apply A.TyUnion with (ts := map trF ys).
      * clear -Q. induction Q as [|m y r r' Hy Hr IHr]; constructor; [exact (proj1 Hy) | exact IHr].
      * subst out. clear. induction ys as [|y r IH]; [reflexivity|]. cbn [flat_map map]. rewrite map_app, IH, members_tr. reflexivity.
      * rewrite Forall_forall in U1 |- *. intros m Hm. apply in_map_iff in Hm as (x & <- & Hx).
        destruct (U1 x Hx) as (h & xs & -> & Hh). eexists h, _. split; [reflexivity | exact Hh].
      * rewrite map_map. erewrite map_ext_in; [exact U2|]. intros x Hx. apply head_tr. rewrite Forall_forall in U1. exact (U1 x Hx).
    + (* a reference *)
      apply bind_inv in H as (g & s1 & H1 & H).
      destruct (rts_spec mi p s g s1 Hi Hw Hmi H1) as (-> & Hr & Hg & Hk).
      pose proof (kind_sym_rd _ _ Hg) as Kd.
      assert (Fin : forall tmr' a', A.Brackets WW (path_of mi) tmr (gs_id g) a' tmr' -> WFty d ->
                (match gs_kind g with
                 | KType => A.nlook (gs_id g) (tmr' ++ tmr) = Some (trF d)
                 | KUnit | KTag => exists args, trF d = TNom (gs_id g) args /\
                     Forall2 (fun r ra => fst ra = r /\ A.nlook r (tmr' ++ tmr) = Some (snd ra)) (R (gs_id g)) args
                 | KAlias => exists n body, gs_decl g = ADAlias n body /\
                     A.TyOf WW (path_of (gs_mod g)) (tmr' ++ tmr) None body (trF d)
                 | _ => False
                 end) -> (a' = None \/ gs_kind g <> KType) ->
                A.TyOf WW (path_of mi) tmr (option_map trF this) (ATRef p a') (trF d) /\ WFty d).
      { intros tmr' a' Br Wd K Na. split; [|exact Wd].
        destruct Hk as [Kt|[Kt|[Kt|Kt]]]; rewrite Kt in K, Kd.
        - destruct Na as [->|Na]; [|exfalso; exact (Na Kt)].
          inversion Br; subst. eapply A.TyType; [exact Hr | exact Kd | exact K].
        - destruct K as (args & -> & K). eapply A.TyNom; [exact Hr | exact Kd | right; reflexivity | exact Br | exact K].
        - destruct K as (args & -> & K). eapply A.TyNom; [exact Hr | exact Kd | left; reflexivity | exact Br | exact K].
        - destruct K as (n & body & Db & K). eapply A.TyAlias; [exact Hr | exact (w_decl_rd _ _ Hg) | cbn; exact Db | exact Br | exact K]. }
      destruct a as [bs|].
      * (* brackets *)
        apply bind_inv in H as (u1 & s2 & H2 & H).
        apply bind_inv in H as (l & s3 & H3 & H).
        apply bind_inv in H as (u4 & s4 & H4 & H).
        destruct (kind_eqb (gs_kind g) KType) eqn:Kt; [noerr' H|].
        assert (O2 : ok (check_relevant mi g bs)) by (unfold check_relevant; apply ok_bind; [apply ok_reqs | intro; okt]).
        assert (O4 : ok (check_totality mi g (l ++ tm) e)) by (unfold check_totality; destruct (l ++ tm); [okt|];
                                                              apply ok_bind; [apply ok_reqs | intro; okt]).
        destruct (O2 _ _ _ Hi H2) as [Hi2 F2]. destruct (ok_app_subst _ _ _ _ _ _ _ _ Hi2 H3) as [Hi3 F3].
        destruct (O4 _ _ _ Hi3 H4) as [Hi4 F4]. destruct (ok_symbol_type _ _ _ _ _ _ _ _ Hi4 H) as [_ F5].
        pose proof (fr_w' _ _ F2 Hw) as Hw2. pose proof (fr_w' _ _ F3 Hw2) as Hw3. pose proof (fr_w' _ _ F4 Hw3) as Hw4.
        pose proof (later_from _ _ F5 L) as L4. pose proof (later_from _ _ F4 L4) as L3. pose proof (later_from _ _ F3 L3) as L2.
        destruct (IS _ _ _ _ _ _ _ tmr H3 Hi2 Hw2 Hmi (tmrel_outer _ _ _ Ht) L3) as (lr & Fa & El & Wl).
        destruct (IY _ _ _ _ _ _ _ (rev lr ++ tmr) H Hi4 Hw4 Hg (tmrel_app _ _ _ _ _ Ht El Wl) L) as [Wd K].
        assert (Br : A.Brackets WW (path_of mi) tmr (gs_id g) (Some bs) (rev lr)).
        { apply A.BrSome. assert (Gen : forall bs', incl bs' bs -> forall lr', Forall2 (BrRel (path_of mi) tmr) bs' lr' ->
              Forall2 (fun b0 tb => exists sp sa, snd b0 = Spec (Some sp) None sa /\
                 A.resolve_sym WW (path_of mi) (fst b0) = Some (fst tb) /\ A.kind_sym WW (fst tb) = Some A.DKType /\
                 (A.kind_sym WW (gs_id g) <> Some A.DKMeth -> In (fst tb) (A.w_reqs WW (gs_id g))) /\
                 A.TyOf WW (path_of mi) tmr None (ATRef sp sa) (snd tb)) bs' lr').
          { intros bs' Hin lr' F. induction F as [|b0 tb r r' Hb Hr' IHr]; constructor.
            - destruct Hb as (sp & sa & E1 & E2 & E3 & E4). exists sp, sa. split; [exact E1|]. split; [exact E2|].
              split; [exact E3|]. split; [|exact E4]. intros _.
              exact (check_relevant_spec mi g bs s u1 s2 Hi Hw Hmi H2 L2 b0 _ (Hin b0 (or_introl eq_refl)) E2 E3).
            - apply IHr. intros z Hz. apply Hin. right. exact Hz. }
          exact (Gen bs (incl_refl _) lr Fa). }
        apply (Fin _ _ Br Wd K). right. intro E. rewrite E in Kt. discriminate Kt.
      * destruct (IY _ _ _ _ _ _ _ tmr H Hi Hw Hg Ht L) as [Wd K].
        exact (Fin [] None (A.BrNone _ _ _ _) Wd K (or_introl eq_refl)).
  - (* app_subst *)
    intros e mi bs outer s l s' tmr H Hi Hw Hmi Ht L. cbn [app_subst] in H.
    apply bind_inv in H as (l0 & s1 & H1 & H). injection H as <- <-.
    assert (Q : Forall2 (fun b0 q => BrRel (path_of mi) tmr b0 (trp q) /\ WFty (snd q)) bs l0).
    { refine (mmap_ok_rel _ _ _ _ bs s l0 s1 _ _ H1 Hi Hw L).
      - intro x. apply ok_bind; [unfold rpath; apply ok_reader; intros s0 a0 s2 E; destruct (resolve_path _ _ _); [injection E as _ <-; reflexivity | discriminate E]|].
        intro tg. apply ok_bind; [destruct tg as [[x0|j]|]; okt|]. intro g. destruct (snd x) as [[sp|] [dn|] sa]; okt.
      - intros b0 _ s0 q s2 E Hi0 Hw0 L2. apply bind_inv in E as (tg & s3 & E1 & E).
        destruct (rpath_rel w R Fl b Hload mi (fst b0) s0 tg s3 Hw0 Hmi E1) as [-> Hres].
        apply bind_inv in E as (g & s4 & E2 & E).
        destruct tg as [[x|j]|]; [|noerr' E2|noerr' E2].
        apply bind_inv in E2 as (g0 & s5 & E5 & E2).
        destruct (sym_loader w R Fl b Hload _ _ x s0 g0 s5 Hi0 Hw0 Hres E5) as [-> Hg0].
        destruct (kind_eqb (gs_kind g0) KType) eqn:Kt; [|noerr' E2]. injection E2 as <- <-.
        destruct (snd b0) as [[sp|] [dn|] sa] eqn:Esb; try noerr' E.
        apply bind_inv in E as (y & s6 & E6 & E). injection E as <- <-.
        destruct (IE _ _ _ _ _ _ _ _ tmr E6 Hi0 Hw0 Hmi Ht I L2) as [Ty Wy].
        split; [|exact Wy]. exists sp, sa. split; [exact Esb|].
        pose proof (load_world_ids b w Hload x _ Hg0) as Eid. cbn [fst snd trp].
        split; [unfold A.resolve_sym; rewrite Hres; cbn; rewrite Eid; reflexivity|].
        split; [rewrite Eid, (kind_sym_rd _ _ Hg0); apply kind_eqb_spec in Kt; rewrite Kt; reflexivity|].
        exact Ty. }
    exists (map trp l0). split; [|split; [rewrite map_rev; reflexivity|]].
    + clear -Q. induction Q as [|b0 q r r' Hq Hr IHr]; constructor; [exact (proj1 Hq) | exact IHr].
    + apply Forall_rev. clear -Q. induction Q as [|b0 q r r' Hq Hr IHr]; constructor; [exact (proj2 Hq) | exact IHr].
  - (* symbol_type *)
    intros mi g tm e s d s' tmr H Hi Hw Hg Ht L. cbn [symbol_type] in H.
    destruct (gs_kind g) eqn:K; try noerr' H.
    + destruct (nassoc (gs_id g) tm) as [y|] eqn:Ey; [|noerr' H]. injection H as <- <-.
      split; [exact (proj1 (proj2 Ht) _ _ Ey)|]. rewrite (proj1 Ht), Ey. reflexivity.
    + apply bind_inv in H as (xs & s1 & H1 & H). injection H as <- <-.
      unfold nominal_args in H1. apply bind_inv in H1 as (rs & s2 & H2 & H1).
      match type of H1 with mmap ?fn _ _ = _ =>
        assert (MR : forall r, In r rs -> forall q s3, fn r s2 = Good (q, s3) -> s3 = s2 /\ (fst q = r /\ nassoc r tm = Some (snd q)))
          by (intros r _ q s3 E; cbv beta in E; destruct (nassoc r tm) as [y|] eqn:Ey; [unfold ret in E; injection E as <- <-; auto | noerr' E]) end.
      destruct (mmap_reader _ _ _ (fun r q => fst q = r /\ nassoc r tm = Some (snd q)) rs s2 xs s1 MR H1) as [-> PP].
      split.
      { unfold WFty. cbn [ety_wf]. apply forallb_forall. intros q Hq. clear -PP Ht Hq.
        induction PP as [|r q' rs' xs' [E1 E2] Pr IHr]; [destruct Hq|].
        destruct Hq as [<-|Hq]; [exact (proj1 (proj2 Ht) _ _ E2) | exact (IHr Hq)]. }
      rewrite (reqs_R _ _ _ _ Hi H2 L). exists (map trp xs). split; [reflexivity|].
      clear -PP Ht. induction PP as [|r q rs' xs' [E1 E2] Pr IHr]; constructor; [|exact IHr].
      split; [exact E1 | cbn; rewrite (proj1 Ht), E2; reflexivity].
    + destruct (gs_decl g) as [| |n0 t0| | | | | |] eqn:D; try noerr' H.
      destruct (IE _ _ _ _ _ _ _ _ tmr H Hi Hw (loader_mod_in _ _ Hg) Ht I L) as [Ty Wd].
      split; [exact Wd|]. exists n0, t0. auto.

    + apply bind_inv in H as (xs & s1 & H1 & H). injection H as <- <-.
      unfold nominal_args in H1. apply bind_inv in H1 as (rs & s2 & H2 & H1).
      match type of H1 with mmap ?fn _ _ = _ =>
        assert (MR : forall r, In r rs -> forall q s3, fn r s2 = Good (q, s3) -> s3 = s2 /\ (fst q = r /\ nassoc r tm = Some (snd q)))
          by (intros r _ q s3 E; cbv beta in E; destruct (nassoc r tm) as [y|] eqn:Ey; [unfold ret in E; injection E as <- <-; auto | noerr' E]) end.
      destruct (mmap_reader _ _ _ (fun r q => fst q = r /\ nassoc r tm = Some (snd q)) rs s2 xs s1 MR H1) as [-> PP].
      split.
      { unfold WFty. cbn [ety_wf]. apply forallb_forall. intros q Hq. clear -PP Ht Hq.
        induction PP as [|r q' rs' xs' [E1 E2] Pr IHr]; [destruct Hq|].
        destruct Hq as [<-|Hq]; [exact (proj1 (proj2 Ht) _ _ E2) | exact (IHr Hq)]. }
      rewrite (reqs_R _ _ _ _ Hi H2 L). exists (map trp xs). split; [reflexivity|].
      clear -PP Ht. induction PP as [|r q rs' xs' [E1 E2] Pr IHr]; constructor; [|exact IHr].
      split; [exact E1 | cbn; rewrite (proj1 Ht), E2; reflexivity].
Qed.

Lemma ty_all : forall f, ETyOK f /\ SubstOK f /\ SymOK f.
Proof.
  induction f as [|f [IE [IS IY]]]; [exact ty_zero | exact (ty_step f IE IS IY)].
Qed.

Lemma ety_of_eq : forall t mi tm e this, ety_of t mi tm e this = elab_type 200 t mi tm e this.
Proof. reflexivity. Qed.

(* BodySound's pd_ty *)
Theorem pd_ty_holds : forall ty c e s d s', ety_of ty (cx_mod c) (tymap e) e (cx_this c) s = Good (d, s') ->
  forall F C, Inv s -> CxRel w R Fl c e F C -> Later w P R Fl s' ->
  A.TyOf WW (A.fr_mod F) (A.c_ty C) (A.fr_this F) ty (trF (canon e d)) /\ WFty d.
Proof.
  intros ty c e s d s' H F C Hi Hc L.
  destruct (ok_ety_of ty (cx_mod c) (tymap e) e (cx_this c) s d s' Hi H) as [_ Fs].
  assert (Hw : st_w s = w) by (rewrite <- (fr_w _ _ Fs); exact (lt_w _ _ _ _ _ L)).
  assert (Etm : forall k, nassoc k (tymap e) = tn_get e k).
  { intro k. unfold tymap, tn_get. induction (en_tn e) as [|[n [y stamp]] l IH]; cbn; [reflexivity|].
    destruct (Nat.eqb k n); [reflexivity|exact IH]. }
  assert (TR : TmRel (tymap e) (map trp (tymap e)) e).
  { apply tmrel_tymap; [exact (cr_ty_wf _ _ _ _ _ _ _ Hc)|]. intro k. rewrite nlook_trp, Etm. reflexivity. }
  rewrite ety_of_eq in H.
  destruct (proj1 (ty_all 200) _ _ _ _ _ _ _ _ _ H Hi Hw (cr_mod_in _ _ _ _ _ _ _ Hc)
    TR (cr_this_wf _ _ _ _ _ _ _ Hc) L) as [Ty Wd]. split; [|exact Wd].
  pose proof (proj1 (cr_uf _ _ _ _ _ _ _ Hc)) as T.
  pose proof (tyof_subst WW (suf Fl (en_uf e)) _ _ _ _ _ Ty (A.c_ty C)) as Ts.
  assert (SR : SubRel (suf Fl (en_uf e)) (map trp (tymap e)) (A.c_ty C)).
  { intro k. rewrite (cr_ty _ _ _ _ _ _ _ Hc), nlook_trp, Etm.
    destruct (tn_get e k); cbn [option_map]; [rewrite tr_canon by exact T|]; reflexivity. }
  specialize (Ts SR). rewrite tr_canon in Ts by exact T.
  rewrite (cr_mod _ _ _ _ _ _ _ Hc), (cr_this _ _ _ _ _ _ _ Hc).
  assert (Ethis : option_map (Audit.app_subst (suf Fl (en_uf e))) (option_map trF (cx_this c)) =
    option_map (fun y => trF (canon e y)) (cx_this c)).
  { destruct (cx_this c); cbn [option_map]; [rewrite tr_canon by exact T|]; reflexivity. }
  rewrite Ethis in Ts. exact Ts.
Qed.

End Ty.
