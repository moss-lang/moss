(* ------------------------------------------------------------------------ *)
(* ReqsSolve.v — the elaborator's requirements of tags and aliases are the  *)
(* least relevance-closed sets (Track A, WorldSound.v's TODO_reqs_reading   *)
(* part (b)).                                                                *)
(*                                                                           *)
(* solve_requirements gathers the group of uncached tags and aliases a root *)
(* reaches, writes [] for each, and iterates free_type_syms over the group  *)
(* until a round changes nothing.  Proved here, over the requirement cache  *)
(* (st_reqs) read against the loader's world:                                *)
(*  - RC: every cached tag or alias is closed (FinalAt): its body's symbols *)
(*    are cached, its listed parameters and the FreeIn symbols of its body, *)
(*    read against the cache, are in its list, and (rr_listed) every listed *)
(*    parameter is a type its assume nesting requires;                       *)
(*  - RL: every cached tag's or alias's list is inside any relevance-closed *)
(*    R that contains the cached lists of everything else.                  *)
(* Both hold of the empty cache, and solve_requirements keeps them           *)
(* (solve_rel); every other write is of a symbol that is no tag or alias    *)
(* (rel_ext).  ElabFrame.v carries them through the whole elaboration.       *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound.
Import ListNotations.
Local Open Scope list_scope.

Module A := ExecutionModel.Elaboration.

Local Strategy opaque [Nat.of_num_uint].

(* ======================================================================== *)
(* 1. The relation's side: the symbols a type expression reads              *)
(* ======================================================================== *)

Definition cv (c : nmap (list nat)) (x : nat) : list nat := opt_default [] (nget c x).

Definition Ext (c c' : nmap (list nat)) : Prop := forall x l, nget c x = Some l -> nget c' x = Some l.

Lemma ext_refl : forall c, Ext c c.
Proof. intros c x l H. exact H. Qed.

Lemma ext_trans : forall c1 c2 c3, Ext c1 c2 -> Ext c2 c3 -> Ext c1 c3.
Proof. intros c1 c2 c3 H1 H2 x l H. exact (H2 x l (H1 x l H)). Qed.

Lemma ext_cached : forall c c' x, Ext c c' -> nget c x <> None -> nget c' x <> None.
Proof. intros c c' x H Hx. destruct (nget c x) as [l|] eqn:E; [|contradiction]. rewrite (H x l E). discriminate. Qed.

(* The symbols FreeIn reads R at: the heads of a type expression's
   references, and of its brackets' right sides under a head that resolves. *)
Inductive HeadIn (W : A.world) (p : string) : aty -> nat -> Prop :=
| HRec : forall fs n t h, In (n, t) fs -> HeadIn W p t h -> HeadIn W p (ATRec fs) h
| HUnion : forall ms t h, In t ms -> HeadIn W p t h -> HeadIn W p (ATUnion ms) h
| HHere : forall pa a h, A.resolve_sym W p pa = Some h -> HeadIn W p (ATRef pa a) h
| HBracket : forall pa a bpath sp d sa h, A.resolve_sym W p pa <> None ->
    In (bpath, Spec (Some sp) d sa) (match a with Some bs => bs | None => [] end) ->
    HeadIn W p (ATRef sp sa) h -> HeadIn W p (ATRef pa a) h.

Lemma FreeIn_mono : forall W Ra Rb p t y,
  (forall h, HeadIn W p t h -> A.kind_sym W h <> Some A.DKType -> incl (Ra h) (Rb h)) ->
  A.FreeIn W Ra p t y -> A.FreeIn W Rb p t y.
Proof.
  intros W Ra Rb p t y Hh H. revert Hh.
  induction H as [fs n t x Hin Hf IH | ms t x Hin Hf IH | pa a t Hr Hk Hb | pa a g x Hr Hk Hx Hb
                  | pa a bpath sp d sa x Hr Hin Hf IH]; intro Hh.
  - eapply A.FRec; [exact Hin|]. apply IH. intros h Hh' Hk. apply Hh; [eapply HRec; eauto | exact Hk].
  - eapply A.FUnion; [exact Hin|]. apply IH. intros h Hh' Hk. apply Hh; [eapply HUnion; eauto | exact Hk].
  - apply A.FType; assumption.
  - eapply A.FNom; [exact Hr | exact Hk | apply (Hh g (HHere _ _ _ _ _ Hr) Hk), Hx | exact Hb].
  - eapply A.FBracket; [exact Hr | exact Hin|]. apply IH. intros h Hh' Hk. apply Hh; [eapply HBracket; eauto | exact Hk].
Qed.

(* reading against a cache that only grows, where the heads are cached *)
Lemma FreeIn_stable : forall W c c' p t,
  (forall h, HeadIn W p t h -> A.kind_sym W h <> Some A.DKType -> nget c h <> None) -> Ext c c' ->
  forall y, A.FreeIn W (cv c) p t y <-> A.FreeIn W (cv c') p t y.
Proof.
  intros W c c' p t Hh He y.
  assert (E : forall h, HeadIn W p t h -> A.kind_sym W h <> Some A.DKType -> cv c h = cv c' h).
  { intros h H1 H2. pose proof (Hh h H1 H2) as Hc. unfold cv.
    destruct (nget c h) as [l|] eqn:El; [|contradiction]. rewrite (He h l El). reflexivity. }
  split; apply FreeIn_mono; intros h H1 H2; rewrite (E h H1 H2); apply incl_refl.
Qed.

(* the requirements of a tag or alias, as Part V's conditions read them *)
Definition NomW (W : A.world) (x : nat) : Prop :=
  A.kind_sym W x = Some A.DKTag \/ A.kind_sym W x = Some A.DKAlias.

Definition FinalAt (W : A.world) (c : nmap (list nat)) (g : nat) : Prop :=
  forall d ps t, A.w_decl W g = Some d -> A.nominal_body (A.d_ast d) = Some (ps, t) ->
    (forall h, HeadIn W (A.d_mod d) t h -> A.kind_sym W h <> Some A.DKType -> nget c h <> None) /\
    (forall n x, In n ps -> A.resolve_sym W (A.d_mod d) [n] = Some x -> In x (cv c g)) /\
    (forall x, A.FreeIn W (cv c) (A.d_mod d) t x -> In x (cv c g)) /\
    (forall n, In n ps -> exists x, A.resolve_sym W (A.d_mod d) [n] = Some x /\ A.AssumeTypes W g x).

(* closed: every cached tag or alias *)
Definition RC (W : A.world) (c : nmap (list nat)) : Prop :=
  forall g l, NomW W g -> nget c g = Some l -> FinalAt W c g.

(* least: inside every relevance-closed R that holds the cached lists of the
   symbols (of W) that are no tag or alias *)
Definition RL (W : A.world) (c : nmap (list nat)) : Prop :=
  forall R, A.RelevanceClosed W R ->
    (forall h l, A.kind_sym W h <> None -> ~ NomW W h -> nget c h = Some l -> incl l (R h)) ->
    forall g l, NomW W g -> nget c g = Some l -> incl l (R g).

Lemma FinalAt_stable : forall W c c' g, FinalAt W c g -> nget c g <> None -> Ext c c' -> FinalAt W c' g.
Proof.
  intros W c c' g Hf Hg He d ps t Hd Hb. destruct (Hf d ps t Hd Hb) as (H1 & H2 & H3 & H4).
  assert (Eg : cv c' g = cv c g).
  { unfold cv. destruct (nget c g) as [l|] eqn:El; [|contradiction]. rewrite (He g l El). reflexivity. }
  rewrite Eg. split; [intros h Hh Hk; exact (ext_cached c c' h He (H1 h Hh Hk))|].
  split; [exact H2|]. split; [|exact H4].
  intros x Hx. apply H3. apply (proj2 (FreeIn_stable W c c' _ t H1 He x)). exact Hx.
Qed.

Lemma RC_empty : forall W, RC W NE.
Proof. intros W g l _ H. rewrite nget_NE in H. discriminate. Qed.

Lemma RL_empty : forall W, RL W NE.
Proof. intros W R _ _ g l _ H. rewrite nget_NE in H. discriminate. Qed.

Definition nominal_k (k : kind) : bool := match k with KTag | KAlias => true | _ => false end.

Lemma nominal_body_ty : forall d ps t, A.nominal_body d = Some (ps, t) -> decl_ty d = Some t.
Proof. intros [] ps t H; cbn in H; try discriminate; injection H as <- <-; reflexivity. Qed.

Lemma nassoc_in : forall V x (l : list (nat * V)) v, nassoc x l = Some v -> In (x, v) l.
Proof.
  intros V x l v. induction l as [|[k u] r IH]; cbn; [discriminate|].
  destruct (Nat.eqb_spec x k) as [->|]; [intro H; injection H as <-; left; reflexivity | intro H; right; exact (IH H)].
Qed.

Lemma fold_add_spec : forall bound l o y,
  In y (fold_left (fun o r => if nmem r bound || nmem r o then o else o ++ [r]) l o) <->
  In y o \/ (In y l /\ ~ In y bound).
Proof.
  intros bound l. induction l as [|r rest IH]; intros o y; cbn [fold_left].
  - split; [intro H; left; exact H | intros [H|[[] _]]; exact H].
  - assert (Ho : forall z, In z (if nmem r bound || nmem r o then o else o ++ [r]) <-> In z o \/ (z = r /\ ~ In z bound)).
    { intro z. destruct (nmem r bound) eqn:Eb; [|destruct (nmem r o) eqn:Eo]; cbn [orb].
      - split; [intro H; left; exact H | intros [H|[-> H]]; [exact H | exfalso; apply H, nmem_in, Eb]].
      - split; [intro H; left; exact H | intros [H|[-> H]]; [exact H | apply nmem_in, Eo]].
      - rewrite in_app_iff. cbn [In]. split.
        + intros [H|[<-|[]]]; [left; exact H | right; split; [reflexivity | intro Hb; apply nmem_in in Hb; congruence]].
        + intros [H|[-> _]]; [left; exact H | right; left; reflexivity]. }
    rewrite IH, Ho. cbn [In]. split.
    + intros [[H|[-> H]]|[H1 H2]]; [left; exact H | right; split; [left; reflexivity | exact H] | right; split; [right; exact H1 | exact H2]].
    + intros [H|[[<-|H1] H2]]; [left; left; exact H | left; right; split; [reflexivity | exact H2] | right; split; [exact H1 | exact H2]].
Qed.

Lemma mfold_reader : forall X Y (f : X -> Y -> M X) xs a s r s',
  (forall a x s r s', f a x s = Good (r, s') -> s' = s) -> mfold f a xs s = Good (r, s') -> s' = s.
Proof.
  intros X Y f xs. induction xs as [|x rest IH]; intros a s r s' Hf H; cbn [mfold] in H.
  - injection H as _ <-. reflexivity.
  - apply bind_inv in H as (a1 & s1 & H1 & H2). rewrite (IH _ _ _ _ Hf H2). exact (Hf _ _ _ _ _ H1).
Qed.

Lemma resolve_sym_reader : forall mi pa s r s', Elab.resolve_sym mi pa s = Good (r, s') -> s' = s.
Proof.
  intros mi pa s r s' H. unfold Elab.resolve_sym in H. apply bind_inv in H as (r0 & s1 & H1 & H).
  cbv beta delta [rpath] in H1. destruct (LoadWorld.resolve_path (st_w s) mi pa) as [r1|m]; [|discriminate].
  injection H1 as -> <-.
  destruct r0 as [[x|j]|].
  - apply bind_inv in H as (g & s2 & H2 & H). unfold sym in H2.
    destruct (nget (st_syms s) x); [injection H2 as <- <-|discriminate]. injection H as <- <-. reflexivity.
  - injection H as <- <-; reflexivity.
  - injection H as <- <-; reflexivity.
Qed.

Lemma walk_specs_reader : forall fuel mi its acc s l s', walk_specs fuel mi its acc s = Good (l, s') -> s' = s.
Proof.
  induction fuel as [|f IH]; intros mi its acc s l s' H; [discriminate|]. cbn [walk_specs] in H.
  refine (mfold_reader _ _ _ its acc s l s' _ H).
  intros o [[p|] [n|] a] s0 r s0' H0; try (injection H0 as _ <-; reflexivity).
  apply bind_inv in H0 as (tg & s1 & H1 & H0). rewrite (resolve_sym_reader _ _ _ _ _ H1) in H0.
  destruct tg as [g|]; [|injection H0 as _ <-; reflexivity].
  destruct (kind_eqb (gs_kind g) KType); [injection H0 as _ <-; reflexivity|].
  destruct (kind_eqb (gs_kind g) KCtx); [|injection H0 as _ <-; reflexivity].
  destruct (nmem (gs_id g) (fst o)); [injection H0 as _ <-; reflexivity|].
  destruct (gs_decl g); try (injection H0 as _ <-; reflexivity). exact (IH _ _ _ _ _ _ H0).
Qed.

Lemma assume_type_syms_reader : forall g s l s', assume_type_syms g s = Good (l, s') -> s' = s.
Proof.
  intros g s l s' H. unfold assume_type_syms, nesting_types, walk_fuel in H.
  apply bind_inv in H as (n & s1 & H1 & H). apply bind_inv in H1 as (x & s2 & E1 & E2).
  injection E1 as <- <-. injection E2 as <- <-.
  apply bind_inv in H as (r & s3 & H3 & H). rewrite (walk_specs_reader _ _ _ _ _ _ _ H3) in H.
  injection H as _ <-. reflexivity.
Qed.

(* The group solve_requirements gathers, and its rounds (the local fixes of
   Elab.solve_requirements, named). *)
Definition collect_fn : nat -> list nat -> list nat -> M (list nat) :=
  fix collect (n : nat) (stack : list nat) (seen : list nat) : M (list nat) :=
        match n with
        | 0 => fail "relevance: too many declarations"
        | S n' =>
            match stack with
            | [] => ret (rev seen)
            | s :: rest =>
                let! c := cached_reqs s in
                if nmem s seen || is_some c then collect n' rest seen
                else
                  let! g := sym s in
                  let! more := match decl_ty (gs_decl g) with
                               | Some t => mentioned_nominals 200 t (gs_mod g)
                               | None => ret []
                               end in
                  collect n' (more ++ rest) (s :: seen)
            end
        end.

Definition round_step (f : nat) (listed : list (nat * list nat)) : bool -> nat -> M bool :=
  fun ch s =>
    let! g := sym s in
    let base := opt_default [] (nassoc s listed) in
    let! found := match decl_ty (gs_decl g) with
                  | Some t => free_type_syms f t (gs_mod g) base
                  | None => ret base
                  end in
    let! old := cached_reqs s in
    if list_eq_dec Nat.eq_dec found (opt_default [] old) then ret ch
    else set_nominal_reqs s found ;; ret true.

Definition iterate_fn (f : nat) (group : list nat) (listed : list (nat * list nat)) : nat -> M unit :=
  fix iterate (n : nat) : M unit :=
    match n with
    | 0 => fail "relevance does not converge"
    | S n' =>
        let! changed := mfold (round_step f listed) false group in
        if changed then iterate n' else ret tt
    end.

Lemma cached_reqs_inv : forall x s c s', cached_reqs x s = Good (c, s') -> s' = s /\ c = nget (st_reqs s) x.
Proof.
  intros x s c s' H. unfold cached_reqs in H. apply bind_inv in H as (s0 & s0' & E1 & E2).
  injection E1 as <- <-. injection E2 as <- <-. split; reflexivity.
Qed.

Lemma sym_inv : forall x s g s', sym x s = Good (g, s') -> s' = s /\ nget (st_syms s) x = Some g.
Proof.
  intros x s g s' H. unfold sym in H. destruct (nget (st_syms s) x) as [g'|]; [|discriminate].
  injection H as <- <-. split; reflexivity.
Qed.

Lemma set_nominal_inv : forall x v s u s', set_nominal_reqs x v s = Good (u, s') ->
  exists g, nget (st_syms s) x = Some g /\
    ((nominal_k (gs_kind g) = true /\ s' = s_reqs (fun m => nset m x (nodup Nat.eq_dec v)) s) \/
     (nominal_k (gs_kind g) = false /\ s' = s)).
Proof.
  intros x v s u s' H. unfold set_nominal_reqs in H. apply bind_inv in H as (g & s1 & H1 & H).
  apply sym_inv in H1 as [-> Hg]. exists g. split; [exact Hg|].
  destruct (gs_kind g); cbn in H |- *;
    [right | right | left | left | right | right | right | right | right];
    (unfold set_reqs, upd in H; injection H as _ <-; split; reflexivity).
Qed.

(* ======================================================================== *)
(* 2. Over the loader's world                                                *)
(* ======================================================================== *)

Section Solve.

Variable b : bundle.
Variable ls : load_st.
Variable e : nat.
Hypothesis HI : LInv b ls.
Variables (R0 : nat -> list nat) (F0 : string -> nat).

Let w := LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls) (ls_next_sym ls) (ls_files ls) (Loader.library_dir (b_prelude b)).
Let W := W_read w R0 F0.

Lemma loader_kind : forall x g, nget (ls_syms ls) x = Some g -> A.kind_sym W x = Some (kconv (gs_kind g)).
Proof. intros x g H. exact (proj1 (proj2 (loader_decl b ls e HI R0 F0 x g H))). Qed.

Lemma ldecl : forall x g, nget (ls_syms ls) x = Some g ->
  A.w_decl W x = Some (A.Decl (pathi b ls e (gs_mod g)) (gs_decl g) (gs_assumes g)) /\ In (gs_mod g) (ls_order ls).
Proof. intros x g H. destruct (loader_decl b ls e HI R0 F0 x g H) as (D & _ & O). split; [exact D | exact O]. Qed.

Lemma kind_none : forall x, nget (ls_syms ls) x = None -> A.kind_sym W x = None.
Proof.
  intros x H. unfold A.kind_sym.
  assert (E : A.w_decl W x = match nget (ls_syms ls) x with
                             | Some g => Some (A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g))
                             | None => None end) by exact (fin_decl b ls e R0 F0 x).
  rewrite E, H. reflexivity.
Qed.

Lemma decl_none : forall x, nget (ls_syms ls) x = None -> A.w_decl W x = None.
Proof.
  intros x H.
  assert (E : A.w_decl W x = match nget (ls_syms ls) x with
                             | Some g => Some (A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g))
                             | None => None end) by exact (fin_decl b ls e R0 F0 x).
  rewrite E, H. reflexivity.
Qed.

Lemma nom_loader : forall x, NomW W x -> exists g, nget (ls_syms ls) x = Some g /\ nominal_k (gs_kind g) = true.
Proof.
  intros x Hx. destruct (nget (ls_syms ls) x) as [g|] eqn:E.
  - exists g. split; [reflexivity|]. unfold NomW in Hx. rewrite (loader_kind x g E) in Hx.
    destruct (gs_kind g); cbn in Hx; destruct Hx as [H|H]; try discriminate; reflexivity.
  - unfold NomW in Hx. rewrite (kind_none x E) in Hx. destruct Hx as [H|H]; discriminate.
Qed.

Lemma nom_of_loader : forall x g, nget (ls_syms ls) x = Some g -> nominal_k (gs_kind g) = true -> NomW W x.
Proof.
  intros x g H Hk. unfold NomW. rewrite (loader_kind x g H). destruct (gs_kind g); cbn in Hk; try discriminate; auto.
Qed.

Lemma notnom_of_loader : forall x g, nget (ls_syms ls) x = Some g -> nominal_k (gs_kind g) = false -> ~ NomW W x.
Proof.
  intros x g H Hk Hn. destruct (nom_loader x Hn) as (g' & H' & Hk'). rewrite H in H'. injection H' as <-. congruence.
Qed.

Lemma visible_loader : forall x, A.kind_sym W x <> None -> exists g, nget (ls_syms ls) x = Some g.
Proof.
  intros x H. destruct (nget (ls_syms ls) x) as [g|] eqn:E; [exists g; reflexivity|]. exfalso. apply H, kind_none, E.
Qed.

Lemma rsym_loader : forall p pa x, A.resolve_sym W p pa = Some x -> exists g, nget (ls_syms ls) x = Some g.
Proof.
  intros p pa x H. unfold A.resolve_sym in H. destruct (A.resolve W p pa) as [[y|m]|] eqn:E; try discriminate.
  injection H as <-. exact (resolved_loader b ls e HI R0 F0 p pa y E).
Qed.

Lemma headin_loader : forall p t h, HeadIn W p t h -> exists g, nget (ls_syms ls) h = Some g.
Proof. intros p t h H. induction H; auto. exact (rsym_loader _ _ _ H). Qed.

Lemma headin_visible : forall p t h, HeadIn W p t h -> A.kind_sym W h <> None.
Proof.
  intros p t h H. destruct (headin_loader p t h H) as (g & Hg). rewrite (loader_kind h g Hg). discriminate.
Qed.

(* resolution in any module: a module the loader did not load resolves
   nothing *)
Lemma esym_any : forall mi pa s r s', GS b ls e s -> Elab.resolve_sym mi pa s = Good (r, s') ->
  s' = s /\ match r with
            | Some g => In mi (ls_order ls) /\ A.resolve_sym W (pathi b ls e mi) pa = Some (gs_id g) /\
                        nget (ls_syms ls) (gs_id g) = Some g
            | None => In mi (ls_order ls) -> A.resolve_sym W (pathi b ls e mi) pa = None
            end.
Proof.
  intros mi pa s r s' Hg H. destruct (in_dec Nat.eq_dec mi (ls_order ls)) as [Hmi|Hmi].
  - destruct (esym_resolve b ls e HI R0 F0 mi pa s r s' Hg Hmi H) as [-> Hr]. split; [reflexivity|].
    destruct r as [g|]; [exact (conj Hmi Hr) | intros _; exact Hr].
  - assert (Hm : nget (ls_mods ls) mi = None).
    { destruct (nget (ls_mods ls) mi) as [m|] eqn:E; [|reflexivity]. exfalso. apply Hmi.
      apply (proj2 (li_order _ _ HI mi)). exact (li_mods _ _ HI mi m E). }
    assert (E : LoadWorld.resolve_path (st_w s) mi pa = Good None).
    { destruct Hg as [Gw _]. rewrite Gw. unfold LoadWorld.resolve_path, get_mod. cbn [w_mods]. rewrite Hm.
      destruct pa; reflexivity. }
    unfold Elab.resolve_sym in H. apply bind_inv in H as (r0 & s1 & H1 & H).
    cbv beta delta [rpath] in H1. rewrite E in H1. injection H1 as <- <-. injection H as <- <-.
    split; [reflexivity|]. intro Hin. contradiction.
Qed.

(* ---- the frame of a call that solves nothing ---- *)

Definition NewPlain (c c' : nmap (list nat)) : Prop :=
  forall x l, nget c x = None -> nget c' x = Some l ->
    exists g, nget (ls_syms ls) x = Some g /\ nominal_k (gs_kind g) = false.

Definition FP (s s' : st) : Prop :=
  st_w s' = st_w s /\ st_syms s' = st_syms s /\ Ext (st_reqs s) (st_reqs s') /\ NewPlain (st_reqs s) (st_reqs s').

Lemma FP_refl : forall s, FP s s.
Proof. intro s. split; [reflexivity|]. split; [reflexivity|]. split; [apply ext_refl|]. intros x l H1 H2. congruence. Qed.

Lemma FP_trans : forall s1 s2 s3, FP s1 s2 -> FP s2 s3 -> FP s1 s3.
Proof.
  intros s1 s2 s3 (A1 & B1 & C1 & D1) (A2 & B2 & C2 & D2).
  split; [congruence|]. split; [congruence|]. split; [exact (ext_trans _ _ _ C1 C2)|].
  intros x l H1 H3. destruct (nget (st_reqs s2) x) as [l2|] eqn:E2.
  - exact (D1 x l2 H1 E2).
  - exact (D2 x l E2 H3).
Qed.

Lemma FP_GS : forall s s', GS b ls e s -> FP s s' -> GS b ls e s'.
Proof. intros s s' [Gw Gk] (A1 & B1 & _). split; [congruence|]. rewrite B1. exact Gk. Qed.

Lemma FP_set : forall s x l g, nget (st_reqs s) x = None -> nget (ls_syms ls) x = Some g ->
  nominal_k (gs_kind g) = false -> FP s (s_reqs (fun m => nset m x l) s).
Proof.
  intros s x l g Hx Hg Hk. unfold s_reqs. split; [reflexivity|]. split; [reflexivity|]. cbn [st_reqs]. split.
  - intros y l' Hy. rewrite nget_nset_other; [exact Hy|]. intros ->. congruence.
  - intros y l' Hy Hy'. rewrite nget_nset in Hy'. destruct (Nat.eqb_spec y x) as [->|]; [|congruence].
    exists g. split; assumption.
Qed.

(* the nominal heads of t are cached *)
Definition Pre (c : nmap (list nat)) (mi : nat) (t : aty) : Prop :=
  In mi (ls_order ls) -> forall h, HeadIn W (pathi b ls e mi) t h -> NomW W h -> nget c h <> None.

(* every head of t that is no type is cached *)
Definition HC (c : nmap (list nat)) (mi : nat) (t : aty) : Prop :=
  forall h, HeadIn W (pathi b ls e mi) t h -> A.kind_sym W h <> Some A.DKType -> nget c h <> None.

Lemma Pre_ext : forall c c' mi t, Ext c c' -> Pre c mi t -> Pre c' mi t.
Proof. intros c c' mi t He Hp Hmi h Hh Hn. exact (ext_cached _ _ _ He (Hp Hmi h Hh Hn)). Qed.

Lemma HC_ext : forall c c' mi t, Ext c c' -> HC c mi t -> HC c' mi t.
Proof. intros c c' mi t He Hp h Hh Hn. exact (ext_cached _ _ _ He (Hp h Hh Hn)). Qed.

(* What a step of free_type_syms computes: out, and the FreeIn symbols of t
   read against the cache it leaves. *)
Definition StepOK (step : list nat -> M (list nat)) (mi : nat) (t : aty) : Prop :=
  forall o s r s', GS b ls e s -> Pre (st_reqs s) mi t -> step o s = Good (r, s') ->
  FP s s' /\ incl o r /\
  (In mi (ls_order ls) -> HC (st_reqs s') mi t /\
     forall y, In y r <-> In y o \/ A.FreeIn W (cv (st_reqs s')) (pathi b ls e mi) t y).

Lemma fold_fts : forall X (step : list nat -> X -> M (list nat)) (T : X -> aty) mi xs,
  (forall x, In x xs -> StepOK (fun o => step o x) mi (T x)) ->
  forall out s r s', GS b ls e s -> (forall x, In x xs -> Pre (st_reqs s) mi (T x)) ->
  mfold step out xs s = Good (r, s') ->
  FP s s' /\ incl out r /\
  (In mi (ls_order ls) -> (forall x, In x xs -> HC (st_reqs s') mi (T x)) /\
     forall y, In y r <-> In y out \/ exists x, In x xs /\ A.FreeIn W (cv (st_reqs s')) (pathi b ls e mi) (T x) y).
Proof.
  intros X step T mi xs Hs. induction xs as [|x rest IH]; intros out s r s' Hg Hp H; cbn [mfold] in H.
  - injection H as <- <-. split; [apply FP_refl|]. split; [intros y Hy; exact Hy|]. intros _.
    split; [intros x []|]. intro y. split; [intro Hy; left; exact Hy | intros [Hy|(x & [] & _)]; exact Hy].
  - apply bind_inv in H as (o1 & s1 & H1 & H2).
    destruct (Hs x (or_introl eq_refl) out s o1 s1 Hg (Hp x (or_introl eq_refl)) H1) as (F1 & I1 & R1).
    pose proof (FP_GS _ _ Hg F1) as Hg1.
    destruct (IH (fun x' Hx' => Hs x' (or_intror Hx')) o1 s1 r s' Hg1
                 (fun x' Hx' => Pre_ext _ _ _ _ (proj1 (proj2 (proj2 F1))) (Hp x' (or_intror Hx'))) H2) as (F2 & I2 & R2).
    split; [exact (FP_trans _ _ _ F1 F2)|]. split; [intros y Hy; apply I2, I1, Hy|].
    intro Hmi. destruct (R1 Hmi) as [HC1 E1]. destruct (R2 Hmi) as [HC2 E2].
    pose proof (proj1 (proj2 (proj2 F2))) as X2.
    pose proof (FreeIn_stable W _ _ _ (T x) HC1 X2) as Hst.
    split.
    + intros x' [<-|Hx']; [exact (HC_ext _ _ _ _ X2 HC1) | exact (HC2 x' Hx')].
    + intro y. rewrite E2, E1, Hst. split.
      * intros [[Hy|Hy]|(x' & Hx' & Hy)];
          [left; exact Hy | right; exists x; split; [left; reflexivity|exact Hy] | right; exists x'; split; [right; exact Hx' | exact Hy]].
      * intros [Hy|(x' & [<-|Hx'] & Hy)]; [left; left; exact Hy | left; right; exact Hy | right; exists x'; split; assumption].
Qed.

Lemma bound_spec : forall mi (bs : list (path * spec)) s bl s', GS b ls e s ->
  mmap (fun b0 => let! l := Elab.resolve_sym mi (fst b0) in
                  ret (match l with Some x => [gs_id x] | None => [] end)) bs s = Good (bl, s') ->
  s' = s /\ (In mi (ls_order ls) -> forall y, In y (List.concat bl) <->
               exists b0, In b0 bs /\ A.resolve_sym W (pathi b ls e mi) (fst b0) = Some y).
Proof.
  intros mi bs. induction bs as [|b0 rest IH]; intros s bl s' Hg H; cbn [mmap] in H.
  - injection H as <- <-. split; [reflexivity|]. intros _ y. cbn. split; [intros []|intros (b0 & [] & _)].
  - apply bind_inv in H as (y0 & s1 & H1 & H). apply bind_inv in H1 as (l & s2 & H2 & H1).
    destruct (esym_any mi (fst b0) s l s2 Hg H2) as [-> Hl]. injection H1 as <- <-.
    apply bind_inv in H as (ys & s3 & H3 & H). destruct (IH s ys s3 Hg H3) as [-> Hys].
    injection H as <- <-. split; [reflexivity|]. intros Hmi y. cbn [List.concat]. rewrite in_app_iff, (Hys Hmi y).
    split.
    + intros [Hy|(b1 & Hb1 & Hr)]; [|exists b1; split; [right; exact Hb1 | exact Hr]].
      destruct l as [g|]; [|destruct Hy]. destruct Hy as [<-|[]]. exists b0. split; [left; reflexivity | exact (proj1 (proj2 Hl))].
    + intros (b1 & [<-|Hb1] & Hr); [|right; exists b1; split; assumption]. left.
      destruct l as [g|].
      * destruct Hl as (_ & Hr' & _). rewrite Hr in Hr'. injection Hr' as ->. left. reflexivity.
      * rewrite (Hl Hmi) in Hr. discriminate.
Qed.

(* type_requirements of a head: cached, or (no tag or alias) computed now *)
Lemma tr_spec : forall fuel g s rq s', GS b ls e s -> nget (ls_syms ls) (gs_id g) = Some g ->
  (nominal_k (gs_kind g) = true -> nget (st_reqs s) (gs_id g) <> None) ->
  type_requirements fuel (gs_id g) s = Good (rq, s') ->
  FP s s' /\ nget (st_reqs s') (gs_id g) = Some rq.
Proof.
  intros fuel g s rq s' Hg Hgl Hn H. destruct fuel as [|f]; [discriminate|]. cbn [type_requirements] in H.
  apply bind_inv in H as (c & s1 & H1 & H). apply cached_reqs_inv in H1 as [-> ->].
  destruct (nget (st_reqs s) (gs_id g)) as [l|] eqn:Hc.
  { injection H as <- <-. split; [apply FP_refl | exact Hc]. }
  apply bind_inv in H as (g' & s2 & H2 & H). apply sym_inv in H2 as [-> H2].
  destruct Hg as [Gw Gk]. rewrite (Gk _ _ Hgl) in H2. injection H2 as <-.
  assert (Hset : forall l s3, st_reqs s3 = st_reqs s -> st_w s3 = st_w s -> st_syms s3 = st_syms s ->
             nominal_k (gs_kind g) = false ->
             FP s (s_reqs (fun m => nset m (gs_id g) l) s3) /\
             nget (st_reqs (s_reqs (fun m => nset m (gs_id g) l) s3)) (gs_id g) = Some l).
  { intros l s3 E1 E2 E3 Hk. split; [|apply nget_nset_same].
    pose proof (FP_set s (gs_id g) l g Hc Hgl Hk) as (A1 & B1 & C1 & D1). unfold s_reqs in *. cbn [st_w st_syms st_reqs] in *.
    rewrite E1, E2, E3. split; [reflexivity|]. split; [reflexivity|]. split; assumption. }
  destruct (gs_kind g) eqn:Kg.
  1, 2: apply bind_inv in H as (u & s3 & H3 & H); unfold set_reqs, upd in H3; injection H3 as <- <-;
    injection H as <- <-; apply Hset; reflexivity.
  1, 2: exfalso; exact (Hn eq_refl eq_refl).
  all: apply bind_inv in H as (out & s3 & H3 & H); apply assume_type_syms_reader in H3; subst s3;
    apply bind_inv in H as (u & s4 & H4 & H); unfold set_reqs, upd in H4; injection H4 as <- <-;
    injection H as <- <-; apply Hset; reflexivity.
Qed.

Lemma kind_type : forall x g, nget (ls_syms ls) x = Some g -> kind_eqb (gs_kind g) KType = false ->
  A.kind_sym W x <> Some A.DKType.
Proof.
  intros x g H Hk. rewrite (loader_kind x g H). intro E. injection E as E. apply kconv_type in E.
  rewrite E in Hk. discriminate.
Qed.

(* free_type_syms, where every tag or alias it reads is cached: it solves
   nothing, caches only symbols that are no tag or alias, and computes out
   and the FreeIn symbols of t, read against the cache it leaves. *)
Lemma fts_spec : forall fuel t mi, StepOK (fun o => free_type_syms fuel t mi o) mi t.
Proof.
  induction fuel as [|f IH]; intros t mi o s r s' Hg Hp H; [discriminate|].
  cbn [free_type_syms] in H. destruct t as [| | |fs|ms|pa a].
  1-3: injection H as <- <-; (split; [apply FP_refl|]); (split; [intros y Hy; exact Hy|]); intros _;
    (split; [intros h Hh; inversion Hh | intro y; split; [intro Hy; left; exact Hy | intros [Hy|Hy]; [exact Hy | inversion Hy]]]).
  - (* a record *)
    destruct (fold_fts _ (fun o p => free_type_syms f (snd p) mi o) snd mi fs (fun x _ => IH (snd x) mi) o s r s' Hg
               (fun x Hx Hmi h Hh Hn => Hp Hmi h (HRec _ _ fs (fst x) (snd x) h (ltac:(destruct x; exact Hx)) Hh) Hn) H)
      as (F1 & I1 & R1).
    split; [exact F1|]. split; [exact I1|]. intro Hmi. destruct (R1 Hmi) as [HC1 E1]. split.
    + intros h Hh Hk. inversion Hh as [fs' n t h' Hin Hh'| | |]; subst. exact (HC1 (n, t) Hin h Hh' Hk).
    + intro y. rewrite E1. split.
      * intros [Hy|((n, t) & Hin & Hy)]; [left; exact Hy | right; eapply A.FRec; [exact Hin | exact Hy]].
      * intros [Hy|Hy]; [left; exact Hy|]. inversion Hy as [fs' n t y' Hin Hy'| | | |]; subst.
        right. exists (n, t). split; [exact Hin | exact Hy'].
  - (* a union *)
    destruct (fold_fts _ (fun o m => free_type_syms f m mi o) (fun m => m) mi ms (fun x _ => IH x mi) o s r s' Hg
               (fun x Hx Hmi h Hh Hn => Hp Hmi h (HUnion _ _ ms x h Hx Hh) Hn) H)
      as (F1 & I1 & R1).
    split; [exact F1|]. split; [exact I1|]. intro Hmi. destruct (R1 Hmi) as [HC1 E1]. split.
    + intros h Hh Hk. inversion Hh as [|ms' t h' Hin Hh'| |]; subst. exact (HC1 t Hin h Hh' Hk).
    + intro y. rewrite E1. split.
      * intros [Hy|(t & Hin & Hy)]; [left; exact Hy | right; eapply A.FUnion; [exact Hin | exact Hy]].
      * intros [Hy|Hy]; [left; exact Hy|]. inversion Hy as [|ms' t y' Hin Hy'| | |]; subst.
        right. exists t. split; [exact Hin | exact Hy'].
  - (* a reference *)
    apply bind_inv in H as (tg & s1 & H1 & H). destruct (esym_any mi pa s tg s1 Hg H1) as [-> Htg].
    destruct tg as [g|].
    2: { injection H as <- <-. split; [apply FP_refl|]. split; [intros y Hy; exact Hy|]. intro Hmi.
         pose proof (Htg Hmi) as Hr. split.
         - intros h Hh. inversion Hh; subst; congruence.
         - intro y. split; [intro Hy; left; exact Hy | intros [Hy|Hy]; [exact Hy | inversion Hy; subst; congruence]]. }
    destruct Htg as (Hmi & Hr & Hgl).
    apply bind_inv in H as (bl & s2 & H2 & H). destruct (bound_spec mi (opt_default [] a) s bl s2 Hg H2) as [-> Hbl].
    specialize (Hbl Hmi). cbv beta zeta in H.
    set (Tb := fun b0 : path * spec => match snd b0 with Spec (Some sp) _ sa => ATRef sp sa | _ => ATUnit end).
    apply bind_inv in H as (o1 & s3 & H3 & H).
    assert (Hb : forall x, In x (opt_default [] a) ->
              StepOK (fun o => (fun o b0 => match snd b0 with
                                            | Spec (Some sp) _ sa => free_type_syms f (ATRef sp sa) mi o
                                            | _ => ret o end) o x) mi (Tb x)).
    { intros [bp [[sp|] d sa]] _; [exact (IH (ATRef sp sa) mi)|].
      intros o' s0 r0 s0' _ _ H0. injection H0 as <- <-. split; [apply FP_refl|]. split; [intros y Hy; exact Hy|].
      intros _. split; [intros h Hh; inversion Hh|]. intro y. split; [intro Hy; left; exact Hy | intros [Hy|Hy]; [exact Hy | inversion Hy]]. }
    assert (Hpb : forall x, In x (opt_default [] a) -> Pre (st_reqs s) mi (Tb x)).
    { intros [bp [[sp|] d sa]] Hx Hmi' h Hh Hn; [|inversion Hh].
      apply (Hp Hmi' h); [|exact Hn]. eapply HBracket; [rewrite Hr; discriminate | exact Hx | exact Hh]. }
    destruct (fold_fts _ _ Tb mi (opt_default [] a) Hb o s o1 s3 Hg Hpb H3) as (F3 & I3 & R3).
    destruct (R3 Hmi) as [HC3 E3].
    (* the brackets, as FreeIn reads them *)
    assert (Hbr : forall c, Ext (st_reqs s3) c -> forall y,
              (exists x, In x (opt_default [] a) /\ A.FreeIn W (cv (st_reqs s3)) (pathi b ls e mi) (Tb x) y) <->
              (exists bpath sp d sa, In (bpath, Spec (Some sp) d sa) (opt_default [] a) /\
                                     A.FreeIn W (cv c) (pathi b ls e mi) (ATRef sp sa) y)).
    { intros c Hc y. split.
      - intros ([bp [[sp|] d sa]] & Hx & Hy); [|inversion Hy].
        exists bp, sp, d, sa. split; [exact Hx|]. exact (proj1 (FreeIn_stable W _ _ _ _ (HC3 _ Hx) Hc y) Hy).
      - intros (bp & sp & d & sa & Hx & Hy). exists (bp, Spec (Some sp) d sa). split; [exact Hx|].
        exact (proj2 (FreeIn_stable W _ _ _ _ (HC3 _ Hx) Hc y) Hy). }
    assert (Hbound : forall y, In y (List.concat bl) <-> exists b0, In b0 (match a with Some bs => bs | None => [] end) /\
                                                     A.resolve_sym W (pathi b ls e mi) (fst b0) = Some y)
      by exact Hbl.
    destruct (kind_eqb (gs_kind g) KType) eqn:Kt.
    + (* a type *)
      injection H as <- <-. apply kind_eqb_spec in Kt.
      assert (Kw : A.kind_sym W (gs_id g) = Some A.DKType) by (rewrite (loader_kind _ g Hgl), Kt; reflexivity).
      assert (Hr' : forall y, In y (if nmem (gs_id g) (List.concat bl) || nmem (gs_id g) o1 then o1 else o1 ++ [gs_id g]) <->
                              In y o1 \/ (y = gs_id g /\ ~ In y (List.concat bl))).
      { intro y. destruct (nmem (gs_id g) (List.concat bl)) eqn:Eb; [|destruct (nmem (gs_id g) o1) eqn:Eo]; cbn [orb].
        - split; [intro Hy; left; exact Hy | intros [Hy|[-> Hy]]; [exact Hy | exfalso; apply Hy, nmem_in, Eb]].
        - split; [intro Hy; left; exact Hy | intros [Hy|[-> _]]; [exact Hy | apply nmem_in, Eo]].
        - rewrite in_app_iff. cbn [In]. split.
          + intros [Hy|[<-|[]]]; [left; exact Hy | right; split; [reflexivity | intro Hb'; apply nmem_in in Hb'; congruence]].
          + intros [Hy|[-> _]]; [left; exact Hy | right; left; reflexivity]. }
      split; [exact F3|]. split; [intros y Hy; apply Hr'; left; apply I3, Hy|]. intros _. split.
      * intros h Hh Hk. inversion Hh as [| |pa' a' h' Hr2|pa' a' bp sp d sa h' _ Hx Hh']; subst.
        -- rewrite Hr in Hr2. injection Hr2 as <-. contradiction.
        -- exact (HC3 (bp, Spec (Some sp) d sa) Hx h Hh' Hk).
      * intro y. rewrite Hr', E3. split.
        -- intros [[Hy|Hy]|[-> Hy]]; [left; exact Hy| |].
           ++ right. destruct (proj1 (Hbr _ (ext_refl _) y) Hy) as (bp & sp & d & sa & Hx & Hy').
              eapply A.FBracket; [rewrite Hr; discriminate | exact Hx | exact Hy'].
           ++ right. apply A.FType; [exact Hr | exact Kw|]. intro Hex. apply Hy, Hbound. exact Hex.
        -- intros [Hy|Hy]; [left; left; exact Hy|].
           inversion Hy as [| |pa' a' t' Hr2 Hk2 Hn2|pa' a' g' x Hr2 Hk2 Hx2 Hn2|pa' a' bp sp d sa x _ Hx Hy']; subst.
           ++ rewrite Hr in Hr2. injection Hr2 as <-. right. split; [reflexivity|]. intro Hb'. apply Hn2, Hbound, Hb'.
           ++ rewrite Hr in Hr2. injection Hr2 as <-. contradiction.
           ++ left. right. apply (proj2 (Hbr _ (ext_refl _) y)). exists bp, sp, d, sa. split; assumption.
    + (* a tag, alias, val, fn, …: its requirements *)
      apply bind_inv in H as (rq & s4 & H4 & H). injection H as <- <-.
      pose proof (FP_GS _ _ Hg F3) as Hg3.
      assert (Hn : nominal_k (gs_kind g) = true -> nget (st_reqs s3) (gs_id g) <> None).
      { intro Hk. apply (ext_cached _ _ _ (proj1 (proj2 (proj2 F3)))). apply (Hp Hmi (gs_id g) (HHere _ _ _ _ _ Hr)).
        exact (nom_of_loader _ g Hgl Hk). }
      destruct (tr_spec f g s3 rq s4 Hg3 Hgl Hn H4) as (F4 & C4).
      pose proof (proj1 (proj2 (proj2 F4))) as X4.
      pose proof (kind_type _ g Hgl Kt) as Kw.
      split; [exact (FP_trans _ _ _ F3 F4)|]. split; [intros y Hy; apply fold_add_spec; left; apply I3, Hy|]. intros _. split.
      * intros h Hh Hk. inversion Hh as [| |pa' a' h' Hr2|pa' a' bp sp d sa h' _ Hx Hh']; subst.
        -- rewrite Hr in Hr2. injection Hr2 as <-. rewrite C4. discriminate.
        -- exact (ext_cached _ _ _ X4 (HC3 (bp, Spec (Some sp) d sa) Hx h Hh' Hk)).
      * intro y. rewrite fold_add_spec, E3.
        assert (Ec : cv (st_reqs s4) (gs_id g) = rq) by (unfold cv; rewrite C4; reflexivity).
        split.
        -- intros [[Hy|Hy]|[Hy Hb']]; [left; exact Hy| |].
           ++ right. destruct (proj1 (Hbr _ X4 y) Hy) as (bp & sp & d & sa & Hx & Hy').
              eapply A.FBracket; [rewrite Hr; discriminate | exact Hx | exact Hy'].
           ++ right. eapply A.FNom; [exact Hr | exact Kw | rewrite Ec; exact Hy|]. intro Hex. apply Hb', Hbound. exact Hex.
        -- intros [Hy|Hy]; [left; left; exact Hy|].
           inversion Hy as [| |pa' a' t' Hr2 Hk2 Hn2|pa' a' g' x Hr2 Hk2 Hx2 Hn2|pa' a' bp sp d sa x _ Hx Hy']; subst.
           ++ rewrite Hr in Hr2. injection Hr2 as <-. contradiction.
           ++ rewrite Hr in Hr2. injection Hr2 as <-. right. split; [exact Hx2|].
              intro Hb'. apply Hn2, Hbound, Hb'.
           ++ left. right. apply (proj2 (Hbr _ X4 y)). exists bp, sp, d, sa. split; assumption.
Qed.

(* ---- the group: what collect gathers ---- *)

Lemma mmap_reader : forall X Y (f : X -> M Y) (P : X -> Y -> Prop) xs s ys s',
  (forall x, In x xs -> forall y s1, f x s = Good (y, s1) -> s1 = s /\ P x y) ->
  mmap f xs s = Good (ys, s') -> s' = s /\ Forall2 P xs ys.
Proof.
  intros X Y f P xs. induction xs as [|x rest IH]; intros s ys s' Hf H; cbn [mmap] in H.
  - injection H as <- <-. split; [reflexivity | constructor].
  - apply bind_inv in H as (y & s1 & H1 & H). destruct (Hf x (or_introl eq_refl) y s1 H1) as [-> Py].
    apply bind_inv in H as (ys' & s2 & H2 & H).
    destruct (IH s ys' s2 (fun x' Hx' => Hf x' (or_intror Hx')) H2) as [-> Pys].
    injection H as <- <-. split; [reflexivity | constructor; assumption].
Qed.

(* mentioned_nominals finds every tag or alias FreeIn can read *)
Lemma mn_spec : forall fuel t mi s l s', GS b ls e s -> mentioned_nominals fuel t mi s = Good (l, s') ->
  s' = s /\ (In mi (ls_order ls) -> forall h, HeadIn W (pathi b ls e mi) t h -> NomW W h -> In h l).
Proof.
  induction fuel as [|f IH]; intros t mi s l s' Hg H; [discriminate|]. cbn [mentioned_nominals] in H.
  destruct t as [| | |fs|ms|pa a].
  1-3: injection H as <- <-; split; [reflexivity | intros _ h Hh; inversion Hh].
  - apply bind_inv in H as (lss & s1 & H1 & H). injection H as <- <-.
    destruct (mmap_reader _ _ _ (fun p l => In mi (ls_order ls) -> forall h, HeadIn W (pathi b ls e mi) (snd p) h -> NomW W h -> In h l)
                fs s lss s1 (fun x _ y s2 H2 => IH (snd x) mi s y s2 Hg H2) H1) as [-> Hf].
    split; [reflexivity|]. intros Hmi h Hh Hn. inversion Hh as [fs' n t h' Hin Hh'| | |]; subst.
    destruct (forall2_in_l _ _ _ _ _ Hf (n, t) Hin) as (l0 & Hl0 & P0).
    apply in_concat. exists l0. split; [exact Hl0 | exact (P0 Hmi h Hh' Hn)].
  - apply bind_inv in H as (lss & s1 & H1 & H). injection H as <- <-.
    destruct (mmap_reader _ _ _ (fun m l => In mi (ls_order ls) -> forall h, HeadIn W (pathi b ls e mi) m h -> NomW W h -> In h l)
                ms s lss s1 (fun x _ y s2 H2 => IH x mi s y s2 Hg H2) H1) as [-> Hf].
    split; [reflexivity|]. intros Hmi h Hh Hn. inversion Hh as [|ms' t h' Hin Hh'| |]; subst.
    destruct (forall2_in_l _ _ _ _ _ Hf t Hin) as (l0 & Hl0 & P0).
    apply in_concat. exists l0. split; [exact Hl0 | exact (P0 Hmi h Hh' Hn)].
  - apply bind_inv in H as (tg & s1 & H1 & H). destruct (esym_any mi pa s tg s1 Hg H1) as [-> Htg].
    cbv beta zeta in H.
    apply bind_inv in H as (more & s2 & H2 & H). injection H as <- <-.
    apply (mmap_reader _ _ _ (fun b0 l => match snd b0 with
                                          | Spec (Some sp) _ sa => In mi (ls_order ls) ->
                                              forall h, HeadIn W (pathi b ls e mi) (ATRef sp sa) h -> NomW W h -> In h l
                                          | _ => True end)) in H2 as [-> Hm].
    2: { intros [bp [[sp|] d sa]] _ y s3 H3; [exact (IH _ mi s y s3 Hg H3) | injection H3 as <- <-; split; [reflexivity | exact I]]. }
    split; [reflexivity|]. intros Hmi h Hh Hn. apply in_or_app.
    inversion Hh as [| |pa' a' h' Hr2|pa' a' bp sp d sa h' _ Hx Hh']; subst.
    + left. destruct tg as [g|]; [|rewrite (Htg Hmi) in Hr2; discriminate].
      destruct Htg as (_ & Hr & Hgl). rewrite Hr in Hr2. injection Hr2 as <-.
      destruct (nom_loader _ Hn) as (g' & Hg' & Hk). rewrite Hgl in Hg'. injection Hg' as <-.
      destruct (gs_kind g); cbn in Hk |- *; try discriminate; left; reflexivity.
    + right. apply in_concat. destruct (forall2_in_l _ _ _ _ _ Hm _ Hx) as (l0 & Hl0 & P0).
      exists l0. split; [exact Hl0|]. exact (P0 Hmi h Hh' Hn).
Qed.

Definition Ment (s : st) (x : nat) (l : list nat) : Prop :=
  exists g, nget (st_syms s) x = Some g /\
    match decl_ty (gs_decl g) with
    | Some t => mentioned_nominals 200 t (gs_mod g) s = Good (l, s)
    | None => l = []
    end.

(* the group is uncached, and closed: what its members mention is in it or
   cached *)
Lemma collect_spec : forall n stack seen s G s', GS b ls e s ->
  collect_fn n stack seen s = Good (G, s') ->
  (forall x, In x seen -> nget (st_reqs s) x = None /\ exists l, Ment s x l /\
     forall y, In y l -> In y seen \/ In y stack \/ nget (st_reqs s) y <> None) ->
  s' = s /\ forall x, In x G -> nget (st_reqs s) x = None /\ exists l, Ment s x l /\
     forall y, In y l -> In y G \/ nget (st_reqs s) y <> None.
Proof.
  induction n as [|n IH]; intros stack seen s G s' Hg H Hi; [discriminate|]. cbn [collect_fn] in H.
  destruct stack as [|x rest].
  - injection H as <- <-. split; [reflexivity|]. intros x Hx. apply in_rev in Hx.
    destruct (Hi x Hx) as (Hc & l & Hm & Hl). split; [exact Hc|]. exists l. split; [exact Hm|].
    intros y Hy. destruct (Hl y Hy) as [Hy'|[[]|Hy']]; [left; apply in_rev; rewrite rev_involutive; exact Hy' | right; exact Hy'].
  - apply bind_inv in H as (c & s1 & H1 & H). apply cached_reqs_inv in H1 as [-> ->].
    destruct (nmem x seen || is_some (nget (st_reqs s) x)) eqn:Ex.
    + apply (IH rest seen s G s' Hg H). intros z Hz. destruct (Hi z Hz) as (Hc & l & Hm & Hl).
      split; [exact Hc|]. exists l. split; [exact Hm|].
      intros y Hy. destruct (Hl y Hy) as [Hy'|[[<-|Hy']|Hy']]; [left; exact Hy' | | right; left; exact Hy' | right; right; exact Hy'].
      apply orb_true_iff in Ex as [Ex|Ex]; [left; apply nmem_in; exact Ex|].
      right; right. destruct (nget (st_reqs s) x); [discriminate | discriminate Ex].
    + apply orb_false_iff in Ex as [Ex1 Ex2].
      apply bind_inv in H as (g & s2 & H2 & H). apply sym_inv in H2 as [-> Hg2].
      apply bind_inv in H as (more & s3 & H3 & H).
      assert (Hm : Ment s x more /\ s3 = s).
      { destruct (decl_ty (gs_decl g)) as [t|] eqn:Dt.
        - destruct (mn_spec _ t (gs_mod g) s more s3 Hg H3) as [-> _]. split; [|reflexivity].
          exists g. split; [exact Hg2|]. rewrite Dt. exact H3.
        - injection H3 as <- <-. split; [|reflexivity]. exists g. split; [exact Hg2|]. rewrite Dt. reflexivity. }
      destruct Hm as [Hm ->].
      apply (IH (more ++ rest) (x :: seen) s G s' Hg H). intros z [<-|Hz].
      * split; [revert Ex2; destruct (nget (st_reqs s) x); cbn; intro Ex2; [discriminate | reflexivity]|].
        exists more. split; [exact Hm|]. intros y Hy. right; left; apply in_or_app; left; exact Hy.
      * destruct (Hi z Hz) as (Hc & l & Hm' & Hl). split; [exact Hc|]. exists l. split; [exact Hm'|]. intros y Hy.
        destruct (Hl y Hy) as [Hy'|[[<-|Hy']|Hy']];
          [left; right; exact Hy' | left; left; reflexivity | right; left; apply in_or_app; right; exact Hy' | right; right; exact Hy'].
Qed.

(* ---- the listed parameters ---- *)

Definition LOK (x : nat) (l : list nat) : Prop :=
  forall d ps t, A.w_decl W x = Some d -> A.nominal_body (A.d_ast d) = Some (ps, t) ->
    (forall y, In y l -> exists n, In n ps /\ A.resolve_sym W (A.d_mod d) [n] = Some y) /\
    (forall n, In n ps -> exists y, A.resolve_sym W (A.d_mod d) [n] = Some y /\ In y l /\ A.AssumeTypes W x y).

Lemma mfold_names : forall X (f : list nat -> X -> M (list nat)) (P : X -> nat -> Prop) xs out s r s',
  (forall o x r0 s0', In x xs -> f o x s = Good (r0, s0') ->
     s0' = s /\ incl o r0 /\ (forall y, In y r0 -> In y o \/ P x y) /\ exists y, P x y /\ In y r0) ->
  mfold f out xs s = Good (r, s') ->
  s' = s /\ incl out r /\ (forall y, In y r -> In y out \/ exists x, In x xs /\ P x y) /\
  (forall x, In x xs -> exists y, P x y /\ In y r).
Proof.
  intros X f P xs. induction xs as [|x rest IH]; intros out s r s' Hf H; cbn [mfold] in H.
  - injection H as <- <-. split; [reflexivity|]. split; [intros y Hy; exact Hy|].
    split; [intros y Hy; left; exact Hy | intros x []].
  - apply bind_inv in H as (o1 & s1 & H1 & H2).
    destruct (Hf out x o1 s1 (or_introl eq_refl) H1) as (-> & I1 & S1 & (y0 & P0 & Y0)).
    destruct (IH o1 s r s' (fun o x' r0 s0' Hx' => Hf o x' r0 s0' (or_intror Hx')) H2) as (-> & I2 & S2 & C2).
    split; [reflexivity|]. split; [intros y Hy; apply I2, I1, Hy|]. split.
    + intros y Hy. destruct (S2 y Hy) as [Ho|(x' & Hx' & Py)].
      * destruct (S1 y Ho) as [Ho'|Py]; [left; exact Ho' | right; exists x; split; [left; reflexivity | exact Py]].
      * right. exists x'. split; [right; exact Hx' | exact Py].
    + intros x' [<-|Hx']; [|exact (C2 x' Hx')]. exists y0. split; [exact P0 | apply I2, Y0].
Qed.

Lemma listed_spec : forall f g s l s', GS b ls e s -> listed_params f g s = Good (l, s') ->
  s' = s /\ (nget (ls_syms ls) (gs_id g) = Some g -> LOK (gs_id g) l).
Proof.
  intros f g s l s' Hg H.
  assert (E : listed_params f g = listed_params 0 g) by (destruct f; reflexivity).
  rewrite E in H. cbn [listed_params] in H.
  assert (Hdecl : nget (ls_syms ls) (gs_id g) = Some g -> forall d, A.w_decl W (gs_id g) = Some d ->
            d = A.Decl (pathi b ls e (gs_mod g)) (gs_decl g) (gs_assumes g) /\ In (gs_mod g) (ls_order ls)).
  { intros Hgl d Hd. destruct (ldecl _ g Hgl) as (D & Og).
    rewrite Hd in D. injection D as ->. split; [reflexivity | exact Og]. }
  destruct (gs_decl g) as [| | |tn [|n1 names] t| | | | |] eqn:Dg.
  all: try (injection H as <- <-; split; [reflexivity|]; intros Hgl d ps t' Hd Hb;
            destruct (Hdecl Hgl d Hd) as [-> _]; cbn [A.d_ast] in Hb; try rewrite Dg in Hb; cbn in Hb;
            first [discriminate Hb | injection Hb as <- <-; split; [intros ? [] | intros ? []]]).
  apply bind_inv in H as (nesting & s1 & H1 & H). pose proof (assume_type_syms_reader _ _ _ _ H1) as ->.
  set (P := fun n y => A.resolve_sym W (pathi b ls e (gs_mod g)) [n] = Some y /\ In y nesting).
  apply (mfold_names _ _ P) in H as (-> & _ & S & C).
  - split; [reflexivity|]. intros Hgl d ps t' Hd Hb. destruct (Hdecl Hgl d Hd) as [-> _].
    cbn [A.d_ast A.d_mod] in Hb |- *. try rewrite Dg in Hb. cbn in Hb. injection Hb as <- <-.
    destruct (assume_type_syms_exact b ls e HI R0 F0 g s nesting s Hg Hgl H1) as (_ & _ & Hex).
    split.
    + intros y Hy. destruct (S y Hy) as [[]|(n & Hn & Hr & _)]. exists n. split; [exact Hn | exact Hr].
    + intros n Hn. destruct (C n Hn) as (y & (Hr & Hy) & Hl). exists y. split; [exact Hr|]. split; [exact Hl|].
      apply Hex. exact Hy.
  - intros o n r0 s0' _ H0. apply bind_inv in H0 as (tg & s2 & H2 & H0).
    pose proof (resolve_sym_reader _ _ _ _ _ H2) as ->.
    destruct tg as [t'|]; [|exfalso; exact (err_not_good _ _ _ _ _ _ H0)].
    destruct (nmem (gs_id t') nesting) eqn:Em; [|exfalso; exact (err_not_good _ _ _ _ _ _ H0)].
    injection H0 as <- <-. split; [reflexivity|].
    assert (Hin : In (gs_id t') (if nmem (gs_id t') o then o else o ++ [gs_id t'])).
    { destruct (nmem (gs_id t') o) eqn:Eo; [apply nmem_in, Eo | apply in_or_app; right; left; reflexivity]. }
    split; [intros y Hy; destruct (nmem (gs_id t') o); [exact Hy | apply in_or_app; left; exact Hy]|].
    destruct (in_dec Nat.eq_dec (gs_mod g) (ls_order ls)) as [Hmi|Hmi].
    + destruct (esym_any _ _ s _ _ Hg H2) as [_ (_ & Hr & _)].
      split.
      * intros y Hy. destruct (nmem (gs_id t') o); [left; exact Hy|].
        apply in_app_or in Hy as [Hy|[<-|[]]]; [left; exact Hy | right; split; [exact Hr | apply nmem_in, Em]].
      * exists (gs_id t'). split; [split; [exact Hr | apply nmem_in, Em] | exact Hin].
    + exfalso. destruct (esym_any _ _ s _ _ Hg H2) as [_ (Hmi' & _)]. exact (Hmi Hmi').
Qed.

Lemma nassoc_some : forall V x (l : list (nat * V)) v, In (x, v) l -> exists v', nassoc x l = Some v'.
Proof.
  intros V x l v. induction l as [|[k u] r IH]; cbn; [intros []|].
  destruct (Nat.eqb_spec x k) as [->|Hne]; [intros _; exists u; reflexivity|].
  intros [E|H]; [injection E as -> _; contradiction | exact (IH H)].
Qed.

Lemma listed_all : forall f G s ll s', GS b ls e s ->
  mmap (fun x => let! g := sym x in let! l := listed_params f g in ret (x, l)) G s = Good (ll, s') ->
  s' = s /\ (forall x l, In (x, l) ll -> LOK x l) /\ (forall x, In x G -> exists l, nassoc x ll = Some l).
Proof.
  intros f G s ll s' Hg H.
  apply (mmap_reader _ _ _ (fun x p => fst p = x /\ LOK x (snd p))) in H as [-> Hf].
  - split; [reflexivity|]. split.
    + intros x l Hin. destruct (forall2_in_r _ _ _ _ _ Hf _ Hin) as (x0 & _ & E & Hl). cbn in E, Hl. subst x0. exact Hl.
    + intros x Hx. destruct (forall2_in_l _ _ _ _ _ Hf _ Hx) as ([x0 l] & Hin & E & _). cbn in E. subst x0.
      exact (nassoc_some _ _ _ _ Hin).
  - intros x _ p s1 H1. apply bind_inv in H1 as (g & s2 & H2 & H1). apply sym_inv in H2 as [-> Hgx].
    apply bind_inv in H1 as (l & s3 & H3 & H1). destruct (listed_spec f g s l s3 Hg H3) as [-> Hl].
    injection H1 as <- <-. split; [reflexivity|]. split; [reflexivity|]. cbn [snd].
    destruct (nget (ls_syms ls) x) as [g0|] eqn:E0.
    + destruct Hg as [_ Gk]. rewrite (Gk x g0 E0) in Hgx. injection Hgx as <-.
      destruct (li_syms _ _ HI x g0 E0) as (_ & _ & Hid & _). rewrite <- Hid in E0 |- *. exact (Hl E0).
    + intros d ps t Hd. rewrite (decl_none x E0) in Hd. discriminate.
Qed.

(* ---- the rounds ---- *)

Lemma NomW_dec : forall x, NomW W x \/ ~ NomW W x.
Proof.
  intro x. unfold NomW. destruct (A.kind_sym W x) as [[]|];
    try (left; auto; fail); right; intros [H|H]; discriminate.
Qed.

(* a loader tag or alias: its body *)
Lemma nominal_decl : forall x g, nget (ls_syms ls) x = Some g -> nominal_k (gs_kind g) = true ->
  exists ps t, A.nominal_body (gs_decl g) = Some (ps, t) /\ decl_ty (gs_decl g) = Some t.
Proof.
  intros x g Hg Hk. pose proof (li_kind _ _ HI x g Hg) as Kd. unfold KindOK in Kd. revert Hk Kd.
  destruct (gs_kind g), (gs_decl g) as [| | |tn ps t| | | [| |] ? ? ? ? | |]; cbn; intros Hk Kd;
    try discriminate; [exists [], a | exists ps, t]; split; reflexivity.
Qed.

Lemma RL_ext : forall c c', RL W c -> Ext c c' -> NewPlain c c' -> RL W c'.
Proof.
  intros c c' Hr He Hn R Hc Hh g l Hg Hl.
  destruct (nget c g) as [l0|] eqn:E.
  - rewrite (He g l0 E) in Hl. injection Hl as <-. apply (Hr R Hc); [|exact Hg | exact E].
    intros h l1 H1 H2 H3. exact (Hh h l1 H1 H2 (He h l1 H3)).
  - destruct (Hn g l E Hl) as (g' & Hg' & Hk). exfalso. exact (notnom_of_loader g g' Hg' Hk Hg).
Qed.

Lemma RL_set : forall c x v, RL W c ->
  (forall g, nget (ls_syms ls) x = Some g -> nominal_k (gs_kind g) = true) ->
  (NomW W x -> forall R, A.RelevanceClosed W R ->
     (forall h l, A.kind_sym W h <> None -> ~ NomW W h -> nget (nset c x v) h = Some l -> incl l (R h)) ->
     incl v (R x)) ->
  RL W (nset c x v).
Proof.
  intros c x v Hr Hx Hv R Hc Hh g l Hg Hl. rewrite nget_nset in Hl. destruct (Nat.eqb_spec g x) as [->|Hne].
  - injection Hl as <-. exact (Hv Hg R Hc Hh).
  - apply (Hr R Hc); [|exact Hg | exact Hl]. intros h l1 H1 H2 H3.
    destruct (Nat.eq_dec h x) as [->|Hhx].
    + exfalso. destruct (visible_loader x H1) as (g' & Hg'). exact (H2 (nom_of_loader x g' Hg' (Hx g' Hg'))).
    + apply (Hh h l1 H1 H2). rewrite nget_nset_other by exact Hhx. exact H3.
Qed.

Record K (s0 : st) (G : list nat) (s : st) : Prop := {
  k_gs : GS b ls e s;
  k_w : st_w s = st_w s0;
  k_syms : st_syms s = st_syms s0;
  k_ext : Ext (st_reqs s0) (st_reqs s);
  k_new : forall x l, nget (st_reqs s0) x = None -> nget (st_reqs s) x = Some l ->
            In x G \/ exists g, nget (ls_syms ls) x = Some g /\ nominal_k (gs_kind g) = false;
  k_rl : RL W (st_reqs s)
}.

(* every tag or alias of the group is cached *)
Definition Cov (G : list nat) (s : st) : Prop :=
  forall x g, In x G -> nget (st_syms s) x = Some g -> nominal_k (gs_kind g) = true -> nget (st_reqs s) x <> None.

Lemma K_FP : forall s0 G s s', K s0 G s -> FP s s' -> K s0 G s'.
Proof.
  intros s0 G s s' [Kg Kw Ks Ke Kn Kr] F. pose proof F as (A1 & B1 & C1 & D1). constructor.
  - exact (FP_GS _ _ Kg F).
  - congruence.
  - congruence.
  - exact (ext_trans _ _ _ Ke C1).
  - intros x l H0 H1. destruct (nget (st_reqs s) x) as [l'|] eqn:E.
    + exact (Kn x l' H0 E).
    + right. exact (D1 x l E H1).
  - exact (RL_ext _ _ Kr C1 D1).
Qed.

Lemma Cov_grow : forall G s s', st_syms s' = st_syms s ->
  (forall y, nget (st_reqs s) y <> None -> nget (st_reqs s') y <> None) -> Cov G s -> Cov G s'.
Proof. intros G s s' Es Hg Hc x g Hx Hgx Hk. rewrite Es in Hgx. exact (Hg x (Hc x g Hx Hgx Hk)). Qed.

Lemma K_set : forall s0 G s x g v, K s0 G s -> In x G -> nget (st_reqs s0) x = None ->
  nget (st_syms s) x = Some g -> nominal_k (gs_kind g) = true ->
  (NomW W x -> forall R, A.RelevanceClosed W R ->
     (forall h l, A.kind_sym W h <> None -> ~ NomW W h -> nget (nset (st_reqs s) x v) h = Some l -> incl l (R h)) ->
     incl v (R x)) ->
  K s0 G (s_reqs (fun m => nset m x v) s).
Proof.
  intros s0 G s x g v [Kg Kw Ks Ke Kn Kr] Hx H0 Hgx Hk Hv. unfold s_reqs. constructor; cbn [st_w st_syms st_reqs].
  - destruct Kg as [Gw Gk]. split; [exact Gw | exact Gk].
  - exact Kw.
  - exact Ks.
  - intros y l Hy. rewrite nget_nset_other; [exact (Ke y l Hy)|]. intros ->. congruence.
  - intros y l Hy Hy'. rewrite nget_nset in Hy'. destruct (Nat.eqb_spec y x) as [->|]; [left; exact Hx | exact (Kn y l Hy Hy')].
  - apply RL_set; [exact Kr | | exact Hv]. intros g' Hg'. destruct Kg as [_ Gk]. rewrite (Gk x g' Hg') in Hgx.
    injection Hgx as <-. exact Hk.
Qed.

Lemma miter_spec : forall s0 G L s u s', K s0 G s -> (forall x, In x L -> In x G) ->
  (forall x, In x G -> nget (st_reqs s0) x = None) ->
  miter (fun x => set_nominal_reqs x []) L s = Good (u, s') ->
  K s0 G s' /\ (forall y, nget (st_reqs s) y <> None -> nget (st_reqs s') y <> None) /\
  (forall x g, In x L -> nget (st_syms s') x = Some g -> nominal_k (gs_kind g) = true -> nget (st_reqs s') x <> None).
Proof.
  intros s0 G L. induction L as [|x rest IH]; intros s u s' Hk HL H0 H; cbn [miter] in H.
  - injection H as _ <-. split; [exact Hk|]. split; [auto | intros x g []].
  - apply bind_inv in H as (u1 & s1 & H1 & H2).
    assert (Hs1 : K s0 G s1 /\ (forall y, nget (st_reqs s) y <> None -> nget (st_reqs s1) y <> None) /\
                  (forall g, nget (st_syms s1) x = Some g -> nominal_k (gs_kind g) = true -> nget (st_reqs s1) x <> None)).
    { destruct (set_nominal_inv x [] s u1 s1 H1) as (g & Hg & [[Hkd ->]|[Hkd ->]]).
      - split; [apply (K_set s0 G s x g); [exact Hk | exact (HL x (or_introl eq_refl)) | exact (H0 x (HL x (or_introl eq_refl)))
                                           | exact Hg | exact Hkd | intros _ R _ _ y []]|].
        split; [intros y Hy; cbn [st_reqs s_reqs]; rewrite nget_nset; destruct (Nat.eqb y x); [discriminate | exact Hy]|].
        intros g' _ _. cbn [st_reqs s_reqs]. rewrite nget_nset_same. discriminate.
      - split; [exact Hk|]. split; [auto|]. intros g' Hg' Hk'. rewrite Hg in Hg'. injection Hg' as <-. congruence. }
    destruct Hs1 as (K1 & G1 & C1).
    destruct (IH s1 u s' K1 (fun y Hy => HL y (or_intror Hy)) H0 H2) as (K2 & G2 & C2).
    split; [exact K2|]. split; [intros y Hy; apply G2, G1, Hy|].
    intros y g [<-|Hy] Hg Hkd; [|exact (C2 y g Hy Hg Hkd)].
    apply G2. apply (C1 g); [|exact Hkd]. rewrite (k_syms _ _ _ K1), <- (k_syms _ _ _ K2). exact Hg.
Qed.

Record Grp (s0 : st) (G : list nat) (ll : list (nat * list nat)) : Prop := {
  gp_new : forall x, In x G -> nget (st_reqs s0) x = None;
  gp_closed : forall x, In x G -> exists l, Ment s0 x l /\ forall y, In y l -> In y G \/ nget (st_reqs s0) y <> None;
  gp_lok : forall x l, In (x, l) ll -> LOK x l;
  gp_ll : forall x, In x G -> exists l, nassoc x ll = Some l
}.

Lemma step_spec : forall s0 G ll f ch x s ch' s', Grp s0 G ll -> K s0 G s -> Cov G s -> In x G ->
  round_step f ll ch x s = Good (ch', s') ->
  K s0 G s' /\ Cov G s' /\
  (ch' = false -> ch = false /\ Ext (st_reqs s) (st_reqs s') /\ (NomW W x -> FinalAt W (st_reqs s') x)).
Proof.
  intros s0 G ll f ch x s ch' s' Hgp Hk Hc Hx H. unfold round_step in H.
  apply bind_inv in H as (g & s1 & H1 & H). apply sym_inv in H1 as [-> Hgx]. cbv beta zeta in H.
  apply bind_inv in H as (found & s2 & H2 & H).
  (* a loader tag or alias *)
  assert (Hnx : NomW W x -> nget (ls_syms ls) x = Some g /\ nominal_k (gs_kind g) = true).
  { intro Hn. destruct (nom_loader x Hn) as (gx & Hgxl & Hkx). destruct (k_gs _ _ _ Hk) as [_ Gk].
    rewrite (Gk x gx Hgxl) in Hgx. injection Hgx as <-. split; assumption. }
  assert (Hf : FP s s2 /\ incl (opt_default [] (nassoc x ll)) found /\
     (NomW W x -> exists t, decl_ty (gs_decl g) = Some t /\ In (gs_mod g) (ls_order ls) /\ HC (st_reqs s2) (gs_mod g) t /\
        forall y, In y found <-> In y (opt_default [] (nassoc x ll)) \/ A.FreeIn W (cv (st_reqs s2)) (pathi b ls e (gs_mod g)) t y)).
  { destruct (decl_ty (gs_decl g)) as [t|] eqn:Dt.
    - assert (Hp : Pre (st_reqs s) (gs_mod g) t).
      { intros Hmi h Hh Hn. destruct (gp_closed _ _ _ Hgp x Hx) as (l & (g0 & Hg0 & Hm) & Hl).
        rewrite <- (k_syms _ _ _ Hk), Hgx in Hg0. injection Hg0 as <-. rewrite Dt in Hm.
        assert (GS0 : GS b ls e s0).
        { destruct (k_gs _ _ _ Hk) as [Gw Gk]. split; [rewrite <- (k_w _ _ _ Hk); exact Gw | rewrite <- (k_syms _ _ _ Hk); exact Gk]. }
        destruct (mn_spec _ t (gs_mod g) s0 l s0 GS0 Hm) as [_ Hin]. pose proof (Hin Hmi h Hh Hn) as Hhl.
        destruct (Hl h Hhl) as [HG|Hc0].
        - destruct (nom_loader h Hn) as (gh & Hgh & Hkh). apply (Hc h gh HG); [|exact Hkh].
          destruct (k_gs _ _ _ Hk) as [_ Gk]. exact (Gk h gh Hgh).
        - exact (ext_cached _ _ _ (k_ext _ _ _ Hk) Hc0). }
      destruct (fts_spec f t (gs_mod g) _ s found s2 (k_gs _ _ _ Hk) Hp H2) as (F & I & R).
      split; [exact F|]. split; [exact I|]. intros Hn. exists t. split; [reflexivity|].
      destruct (Hnx Hn) as [Hgxl _]. destruct (ldecl x g Hgxl) as (_ & Og). split; [exact Og|]. exact (R Og).
    - injection H2 as <- <-. split; [apply FP_refl|]. split; [intros y Hy; exact Hy|]. intros Hn. exfalso.
      destruct (Hnx Hn) as [Hgxl Hkx]. destruct (nominal_decl x g Hgxl Hkx) as (ps & t & _ & Dt'). congruence. }
  apply bind_inv in H as (old & s3 & H3 & H). apply cached_reqs_inv in H3 as [-> ->].
  destruct Hf as (F2 & I2 & R2).
  pose proof (K_FP _ _ _ _ Hk F2) as K2.
  assert (C2 : Cov G s2).
  { apply (Cov_grow G s s2); [exact (proj1 (proj2 F2)) | intros y; apply ext_cached, (proj1 (proj2 (proj2 F2))) | exact Hc]. }
  (* the tag's or alias's body and listed parameters *)
  assert (Hbody : NomW W x -> exists ps t, A.w_decl W x = Some (A.Decl (pathi b ls e (gs_mod g)) (gs_decl g) (gs_assumes g)) /\
            A.nominal_body (gs_decl g) = Some (ps, t) /\ In (gs_mod g) (ls_order ls) /\ HC (st_reqs s2) (gs_mod g) t /\
            (forall y, In y found <-> In y (opt_default [] (nassoc x ll)) \/ A.FreeIn W (cv (st_reqs s2)) (pathi b ls e (gs_mod g)) t y) /\
            LOK x (opt_default [] (nassoc x ll))).
  { intro Hn. destruct (R2 Hn) as (t & Dt & Og & HC2 & E2). destruct (Hnx Hn) as [Hgxl Hkx].
    destruct (nominal_decl x g Hgxl Hkx) as (ps & t' & Hb & Dt'). rewrite Dt in Dt'. injection Dt' as <-.
    destruct (gp_ll _ _ _ Hgp x Hx) as (l0 & Hl0). pose proof (gp_lok _ _ _ Hgp x l0 (nassoc_in _ _ _ _ Hl0)) as Hlok.
    rewrite Hl0. exists ps, t. split; [exact (proj1 (ldecl x g Hgxl))|]. split; [exact Hb|]. split; [exact Og|].
    split; [exact HC2|]. split; [rewrite Hl0 in E2; exact E2 | exact Hlok]. }
  destruct (list_eq_dec Nat.eq_dec found (opt_default [] (nget (st_reqs s2) x))) as [Eq|Ne].
  - injection H as <- <-. split; [exact K2|]. split; [exact C2|]. intros ->. split; [reflexivity|].
    split; [exact (proj1 (proj2 (proj2 F2)))|].
    intros Hn. destruct (Hbody Hn) as (ps & t & Dg & Hb & Og & HC2 & E2 & Hlok).
    intros d ps' t' Hd Hb'. rewrite Dg in Hd. injection Hd as <-. cbn [A.d_ast A.d_mod] in Hb' |- *.
    rewrite Hb in Hb'. injection Hb' as <- <-.
    assert (Ecv : cv (st_reqs s2) x = found) by (unfold cv; symmetry; exact Eq). rewrite Ecv.
    destruct (Hlok _ ps t Dg Hb) as [L1 L2].
    split; [exact HC2|]. split.
    + intros n y Hn' Hr. destruct (L2 n Hn') as (y' & Hr' & Hy' & _). cbn [A.d_mod] in Hr'. rewrite Hr in Hr'.
      injection Hr' as <-. apply I2, Hy'.
    + split; [intros y Hy; apply E2; right; exact Hy|].
      intros n Hn'. destruct (L2 n Hn') as (y & Hr & _ & Ha). exists y. split; assumption.
  - apply bind_inv in H as (u & s4 & H4 & H). injection H as <- <-.
    destruct (set_nominal_inv x found s2 u s4 H4) as (g' & Hg' & [[Hkd ->]|[Hkd ->]]).
    + split.
      * apply (K_set s0 G s2 x g'); [exact K2 | exact Hx | exact (gp_new _ _ _ Hgp x Hx) | exact Hg' | exact Hkd|].
        intros Hn R Hcl Hh y Hy. apply nodup_In in Hy.
        destruct (Hbody Hn) as (ps & t & Dg & Hb & Og & HC2 & E2 & Hlok).
        destruct (Hcl x _ ps t Dg Hb) as [Cl1 Cl2]. cbn [A.d_mod] in Cl1, Cl2.
        destruct (Hlok _ ps t Dg Hb) as [L1 _]. cbn [A.d_mod] in L1.
        apply E2 in Hy as [Hy|Hy].
        -- destruct (L1 y Hy) as (n & Hn' & Hr). exact (Cl1 n y Hn' Hr).
        -- apply Cl2. apply (FreeIn_mono W (cv (st_reqs s2)) R _ t y); [|exact Hy].
           intros h Hhd Hk'. pose proof (HC2 h Hhd Hk') as Hch.
           destruct (nget (st_reqs s2) h) as [lh|] eqn:Eh; [|contradiction]. unfold cv. rewrite Eh. cbn [opt_default].
           destruct (NomW_dec h) as [Hnh|Hnh].
           ++ apply (k_rl _ _ _ K2 R Hcl); [|exact Hnh | exact Eh].
              intros h' l' H1' H2' H3'. apply (Hh h' l' H1' H2'). rewrite nget_nset_other; [exact H3'|].
              intros ->. exact (H2' Hn).
           ++ apply (Hh h lh (headin_visible _ _ _ Hhd) Hnh). rewrite nget_nset_other; [exact Eh|].
              intros ->. exact (Hnh Hn).
      * split; [|intro E; discriminate E].
        apply (Cov_grow G s2); [reflexivity | | exact C2].
        intros y Hy. cbn [st_reqs s_reqs]. rewrite nget_nset. destruct (Nat.eqb y x); [discriminate | exact Hy].
    + split; [exact K2|]. split; [exact C2|]. intro E; discriminate E.
Qed.

Lemma round_spec : forall s0 G ll f L ch s ch' s', Grp s0 G ll -> K s0 G s -> Cov G s ->
  (forall x, In x L -> In x G) ->
  mfold (round_step f ll) ch L s = Good (ch', s') ->
  K s0 G s' /\ Cov G s' /\
  (ch' = false -> ch = false /\ Ext (st_reqs s) (st_reqs s') /\ forall x, In x L -> NomW W x -> FinalAt W (st_reqs s') x).
Proof.
  intros s0 G ll f L. induction L as [|x rest IH]; intros ch s ch' s' Hgp Hk Hc HL H; cbn [mfold] in H.
  - injection H as <- <-. split; [exact Hk|]. split; [exact Hc|]. intros ->. split; [reflexivity|].
    split; [apply ext_refl | intros x []].
  - apply bind_inv in H as (ch1 & s1 & H1 & H2).
    destruct (step_spec s0 G ll f ch x s ch1 s1 Hgp Hk Hc (HL x (or_introl eq_refl)) H1) as (K1 & C1 & P1).
    destruct (IH ch1 s1 ch' s' Hgp K1 C1 (fun y Hy => HL y (or_intror Hy)) H2) as (K2 & C2 & P2).
    split; [exact K2|]. split; [exact C2|]. intros Hf. destruct (P2 Hf) as (E1 & X2 & F2). destruct (P1 E1) as (E0 & X1 & F1).
    split; [exact E0|]. split; [exact (ext_trans _ _ _ X1 X2)|].
    intros y [<-|Hy] Hn; [|exact (F2 y Hy Hn)].
    apply (FinalAt_stable W (st_reqs s1)); [exact (F1 Hn) | | exact X2].
    destruct (nom_loader x Hn) as (gy & Hgy & Hky). apply (C1 x gy (HL x (or_introl eq_refl))); [|exact Hky].
    destruct (k_gs _ _ _ K1) as [_ Gk]. exact (Gk x gy Hgy).
Qed.

Lemma iterate_spec : forall s0 G ll f n s u s', Grp s0 G ll -> K s0 G s -> Cov G s ->
  iterate_fn f G ll n s = Good (u, s') ->
  K s0 G s' /\ forall x, In x G -> NomW W x -> FinalAt W (st_reqs s') x.
Proof.
  intros s0 G ll f n. induction n as [|n IH]; intros s u s' Hgp Hk Hc H; [discriminate|].
  cbn [iterate_fn] in H. apply bind_inv in H as (ch & s1 & H1 & H).
  destruct (round_spec s0 G ll f G false s ch s1 Hgp Hk Hc (fun x Hx => Hx) H1) as (K1 & C1 & P1).
  destruct ch.
  - exact (IH s1 u s' Hgp K1 C1 H).
  - injection H as _ <-. split; [exact K1|]. exact (proj2 (proj2 (P1 eq_refl))).
Qed.

(* solve_requirements keeps the cache closed and least *)
Lemma solve_rel : forall f root s u s', GS b ls e s -> RC W (st_reqs s) -> RL W (st_reqs s) ->
  solve_requirements (S f) root s = Good (u, s') -> RC W (st_reqs s') /\ RL W (st_reqs s').
Proof.
  intros f root s u s' Hg Hrc Hrl H. cbn [solve_requirements] in H.
  apply bind_inv in H as (G & s1 & H1 & H).
  assert (H1' : collect_fn 100000 [gs_id root] [] s = Good (G, s1)) by exact H1.
  destruct (collect_spec _ [gs_id root] [] s G s1 Hg H1' (fun x Hx => match Hx with end)) as [-> HG].
  apply bind_inv in H as (u1 & s2 & H2 & H).
  assert (K0 : K s G s) by (constructor; [exact Hg | reflexivity | reflexivity | apply ext_refl | intros x l E1 E2; congruence | exact Hrl]).
  destruct (miter_spec s G G s u1 s2 K0 (fun x Hx => Hx) (fun x Hx => proj1 (HG x Hx)) H2) as (K2 & _ & C2).
  apply bind_inv in H as (ll & s3 & H3 & H).
  destruct (listed_all f G s2 ll s3 (k_gs _ _ _ K2) H3) as (-> & Hlok & Hll).
  assert (Hgp : Grp s G ll) by (constructor; [exact (fun x Hx => proj1 (HG x Hx)) | exact (fun x Hx => proj2 (HG x Hx))
                                             | exact Hlok | exact Hll]).
  assert (H' : iterate_fn f G ll 10000 s2 = Good (u, s')) by exact H.
  destruct (iterate_spec s G ll f _ s2 u s' Hgp K2 C2 H') as (K3 & Fin).
  split; [|exact (k_rl _ _ _ K3)].
  intros g l Hn Hl. destruct (in_dec Nat.eq_dec g G) as [HgG|HgG]; [exact (Fin g HgG Hn)|].
  destruct (nget (st_reqs s) g) as [l0|] eqn:E0.
  - apply (FinalAt_stable W (st_reqs s)); [exact (Hrc g l0 Hn E0) | rewrite E0; discriminate | exact (k_ext _ _ _ K3)].
  - exfalso. destruct (k_new _ _ _ K3 g l E0 Hl) as [Hin|(g' & Hg' & Hk)]; [exact (HgG Hin)|].
    exact (notnom_of_loader g g' Hg' Hk Hn).
Qed.

(* a write of a symbol that is no tag or alias, not yet cached *)
Lemma rel_set_plain : forall c x l, RC W c -> RL W c -> nget c x = None ->
  (forall g, nget (ls_syms ls) x = Some g -> nominal_k (gs_kind g) = false) ->
  RC W (nset c x l) /\ RL W (nset c x l).
Proof.
  intros c x l Hrc Hrl Hx Hk.
  assert (Hnx : ~ NomW W x) by (intro Hn; destruct (nom_loader x Hn) as (g & Hg & Hk'); rewrite (Hk g Hg) in Hk'; discriminate).
  assert (He : Ext c (nset c x l)) by (intros y l' Hy; rewrite nget_nset_other; [exact Hy | intros ->; congruence]).
  split.
  - intros g l' Hn Hl. rewrite nget_nset_other in Hl by (intros ->; exact (Hnx Hn)).
    apply (FinalAt_stable W c); [exact (Hrc g l' Hn Hl) | rewrite Hl; discriminate | exact He].
  - intros R Hc Hh g l' Hn Hl. rewrite nget_nset_other in Hl by (intros ->; exact (Hnx Hn)).
    apply (Hrl R Hc); [|exact Hn | exact Hl]. intros h l1 H1 H2 H3. exact (Hh h l1 H1 H2 (He h l1 H3)).
Qed.

End Solve.

(* ======================================================================== *)
(* 3. Over the world load_world reads                                        *)
(* ======================================================================== *)

Lemma load_world_inv : forall b w, load_world b = Good w ->
  exists ls e, LInv b ls /\ w = LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls) (ls_next_sym ls) (ls_files ls) (Loader.library_dir (b_prelude b)).
Proof.
  intros b w H. unfold load_world in H. apply rbind_good in H as ([e ls] & Hl & H). injection H as <-.
  destruct (load_spec b _ _ _ _ _ _ Hl (linv_init b)) as (L & _). exists ls, e. split; [exact L | reflexivity].
Qed.

Definition W0r (w : LoadWorld.world) : A.world := W_read w (fun _ => []) (fun _ => 0).

Theorem solve_rel_w : forall b w f root s u s', load_world b = Good w -> st_w s = w ->
  (forall i g, nget (w_syms w) i = Some g -> nget (st_syms s) i = Some g) ->
  RC (W0r w) (st_reqs s) -> RL (W0r w) (st_reqs s) ->
  solve_requirements (S f) root s = Good (u, s') -> RC (W0r w) (st_reqs s') /\ RL (W0r w) (st_reqs s').
Proof.
  intros b w f root s u s' Hw Hsw Hsk Hrc Hrl H. destruct (load_world_inv b w Hw) as (ls & e & HI & ->).
  exact (solve_rel b ls e HI _ _ f root s u s' (conj Hsw Hsk) Hrc Hrl H).
Qed.

Theorem rel_set_plain_w : forall b w c x l, load_world b = Good w ->
  RC (W0r w) c -> RL (W0r w) c -> nget c x = None ->
  (forall g, nget (w_syms w) x = Some g -> nominal_k (gs_kind g) = false) ->
  RC (W0r w) (nset c x l) /\ RL (W0r w) (nset c x l).
Proof.
  intros b w c x l Hw Hrc Hrl Hx Hk. destruct (load_world_inv b w Hw) as (ls & e & HI & ->).
  exact (rel_set_plain b ls e HI _ _ c x l Hrc Hrl Hx Hk).
Qed.
