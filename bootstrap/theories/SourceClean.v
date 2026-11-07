(* Source types cannot construct the reserved nominal record head. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ReqsSolve ElabFrameCore BodySoundCore BodyTy CanonTypes.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].
Module A := Audit.Elaboration.

Definition TmClean (tm : list (nat * ety)) : Prop :=
  forall k y, nassoc k tm = Some y -> ety_ok y = true.
Definition EnvClean (e : env) : Prop :=
  forall k p, In (k,p) (en_tn e) -> fst p = YAbs k.

Lemma tmclean_of_forall : forall tm,
  Forall (fun q => ety_ok (snd q) = true) tm -> TmClean tm.
Proof.
  intros tm H k y E. exact ((proj1 (Forall_forall _ _) H) (k,y) (Merge.nassoc_in _ _ _ _ E)).
Qed.
Lemma tmclean_app : forall a b, TmClean a -> TmClean b -> TmClean (a ++ b).
Proof.
  intros a b A0 B0 k y. rewrite nassoc_app. destruct (nassoc k a) eqn:E; [intro H; injection H as <-; exact (A0 _ _ E)|exact (B0 k y)].
Qed.

Section Clean.
Variable w : Elab.world. Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Notation path_of mi := (gm_path (get_mod w mi)).

Ltac noerr' H := exfalso;
  solve [repeat (cbn beta iota zeta in H;
    first [exact (err_not_good _ _ _ _ _ _ H) | discriminate H
          | apply bind_inv in H as (? & ? & ? & H)])].
Lemma clean_later : forall s s', Fr s s' -> Later w P R Fl s' -> Later w P R Fl s.
Proof. exact (later_back w P R Fl). Qed.

Definition ETyClean_at f : Prop := forall t mi tm e this s d s',
  elab_type f t mi tm e this s = Good (d,s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> TmClean tm -> TmClean (tymap e) ->
  (match this with Some y => ety_ok y = true | None => True end) ->
  Later w P R Fl s' -> ety_ok d = true.
Definition SubClean_at f : Prop := forall e mi bs outer s l s',
  app_subst f e mi bs outer s = Good (l,s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> TmClean outer -> TmClean (tymap e) ->
  Later w P R Fl s' -> Forall (fun q => ety_ok (snd q) = true) l.
Definition SymClean_at f : Prop := forall mi g tm e s d s',
  symbol_type f mi g tm e s = Good (d,s') ->
  Inv s -> st_w s = w -> nget (w_syms w) (gs_id g) = Some g ->
  TmClean tm -> TmClean (tymap e) -> Later w P R Fl s' -> ety_ok d = true.

Lemma clean_zero_at : ETyClean_at 0 /\ SubClean_at 0 /\ SymClean_at 0.
Proof. split; [|split]; intros until 1; discriminate. Qed.

Lemma clean_step_at : forall f, ETyClean_at f -> SubClean_at f -> SymClean_at f ->
  ETyClean_at (S f) /\ SubClean_at (S f) /\ SymClean_at (S f).
Proof.
  intros f IE IS IY. split; [|split].
  - intros t mi tm e this s d s' H Hi Hw Hmi T E Wth L.
    cbn [elab_type] in H. destruct t as [| | |fs|ms|p a].
    + injection H as <- <-. reflexivity.
    + destruct this; [|noerr' H]. injection H as <- <-. exact Wth.
    + injection H as <- <-. reflexivity.
    + apply bind_inv in H as (fs' & s1 & H1 & H). injection H as <- <-.
      assert (Q : Forall2 (fun nt q => ety_ok (snd q) = true) fs fs').
      { refine (mmap_ok_rel w R Fl _ _ _ _ fs s fs' s1 _ _ H1 Hi Hw L).
        - intro x. apply ok_bind; [apply ok_elab_type|intro; apply ok_ret].
        - intros [n0 t0] _ s2 q s3 H2 Hi2 Hw2 L3.
          apply bind_inv in H2 as (y & s4 & H3 & H2). injection H2 as <- <-.
          exact (IE _ _ _ _ _ _ _ _ H3 Hi2 Hw2 Hmi T E Wth L3). }
      cbn [ety_ok]. apply forallb_forall. intros q Hq.
      clear -Q Hq. induction Q; [contradiction|]. destruct Hq as [<-|Hq]; auto.
    + apply bind_inv in H as (out & s1 & H1 & H). injection H as <- <-.
      match type of H1 with mfold ?fn _ _ _ = _ =>
      assert (Gen : forall ms1 out0 s0 out1 s2,
        mfold fn out0 ms1 s0 = Good (out1,s2) -> Inv s0 -> st_w s0 = w ->
        Later w P R Fl s2 -> UInv out0 ->
        Forall (fun y => ety_ok y = true) out0 ->
        Forall (fun y => ety_ok y = true) out1 /\ UInv out1) end.
      { induction ms1 as [|m r IH]; intros out0 s0 out1 s2 H2 Hi0 Hw0 L2 U0 C0; cbn [mfold] in H2.
        - injection H2 as <- <-. exact (conj C0 U0).
        - apply bind_inv in H2 as (o1 & s3 & H3 & H2).
          apply bind_inv in H3 as (y & s4 & H4 & H3).
          destruct (ok_elab_type _ _ _ _ _ _ s0 y s4 Hi0 H4) as [Hi4 F4].
          destruct (union_inner mi _ _ _ _ _ U0 H3) as (-> & U1 & ->).
          assert (L4 : Later w P R Fl s4).
          { refine (clean_later _ _ _ L2). refine (proj2 (ok_mfold _ _ _ r _ _ _ _ _ Hi4 H2)).
            intros a0 x. apply ok_bind; [apply ok_elab_type|]. intro y0. apply ok_mfold. intros a1 x1.
            repeat (match goal with |- ok (match ?x with _ => _ end) => destruct x end); okt. }
          apply (IH _ _ _ _ H2 Hi4 (fr_w' w _ _ F4 Hw0) L2 U1).
          apply Forall_app. split; [exact C0|]. apply members_ok.
          exact (IE _ _ _ _ _ _ _ _ H4 Hi0 Hw0 Hmi T E Wth L4). }
      pose proof (Gen ms [] s out s1 H1 Hi Hw L (conj (Forall_nil _) (NoDup_nil _)) (Forall_nil _)) as [C [N Nd]].
      destruct out; [reflexivity|]. cbn [ety_ok]. apply forallb_forall. intros t Ht.
      rewrite Forall_forall in C,N. destruct (N t Ht) as (n & xs & -> & Hn). exact (C _ Ht).
    + apply bind_inv in H as (g & s1 & H1 & H).
      destruct (rts_spec w R Fl b Hload mi p s g s1 Hi Hw Hmi H1) as (-> & Hr & Hg & Hk).
      destruct a as [bs|].
      * apply bind_inv in H as (u1 & s2 & H2 & H).
        apply bind_inv in H as (l & s3 & H3 & H).
        apply bind_inv in H as (u4 & s4 & H4 & H).
        destruct (kind_eqb (gs_kind g) KType); [noerr' H|].
        assert (O2 : ok (check_relevant mi g bs)) by (unfold check_relevant; apply ok_bind; [apply ok_reqs|intro; okt]).
        assert (O4 : ok (check_totality mi g (l ++ tm) e)) by (unfold check_totality; destruct (l ++ tm); [okt|]; apply ok_bind; [apply ok_reqs|intro; okt]).
        destruct (O2 _ _ _ Hi H2) as [Hi2 F2]. destruct (ok_app_subst _ _ _ _ _ _ _ _ Hi2 H3) as [Hi3 F3].
        destruct (O4 _ _ _ Hi3 H4) as [Hi4 F4]. destruct (ok_symbol_type _ _ _ _ _ _ _ _ Hi4 H) as [_ F5].
        pose proof (fr_w' w _ _ F2 Hw) as Hw2. pose proof (fr_w' w _ _ F3 Hw2) as Hw3. pose proof (fr_w' w _ _ F4 Hw3) as Hw4.
        pose proof (clean_later _ _ F5 L) as L4. pose proof (clean_later _ _ F4 L4) as L3.
        pose proof (IS _ _ _ _ _ _ _ H3 Hi2 Hw2 Hmi T E L3) as Cl.
        eapply IY; [exact H|exact Hi4|exact Hw4|exact Hg| |exact E|exact L].
        apply tmclean_app; [apply tmclean_of_forall; exact Cl|exact T].
      * exact (IY _ _ _ _ _ _ _ H Hi Hw Hg T E L).
  - intros e mi bs outer s l s' H Hi Hw Hmi T E L. cbn [app_subst] in H.
    apply bind_inv in H as (l0 & s1 & H1 & H). injection H as <- <-.
    apply Forall_rev. assert (Q : Forall2 (fun b0 q => ety_ok (snd q) = true) bs l0).
    { refine (mmap_ok_rel w R Fl _ _ _ _ bs s l0 s1 _ _ H1 Hi Hw L).
      - intro x. apply ok_bind; [unfold rpath; apply ok_reader; intros s0 a0 s2 H2; destruct (resolve_path _ _ _); [injection H2 as _ <-; reflexivity|discriminate H2]|].
        intro tg. apply ok_bind; [destruct tg as [[x0|j]|]; okt|]. intro g. destruct (snd x) as [[sp|] [dn|] sa]; okt.
      - intros b0 _ s0 q s2 H2 Hi0 Hw0 L2.
        apply bind_inv in H2 as (tg & s3 & H3 & H2).
        destruct (rpath_rel w R Fl b Hload mi (fst b0) s0 tg s3 Hw0 Hmi H3) as [-> Hres].
        apply bind_inv in H2 as (g & s4 & H4 & H2).
        destruct tg as [[x|j]|]; [|noerr' H4|noerr' H4].
        apply bind_inv in H4 as (g0 & s5 & H5 & H4).
        destruct (sym_loader w R Fl b Hload _ _ x s0 g0 s5 Hi0 Hw0 Hres H5) as [-> Hg0].
        destruct (kind_eqb (gs_kind g0) KType); [|noerr' H4]. injection H4 as <- <-.
        destruct (snd b0) as [[sp|] [dn|] sa]; try noerr' H2.
        apply bind_inv in H2 as (y & s6 & H6 & H2). injection H2 as <- <-.
        eapply IE; [exact H6|exact Hi0|exact Hw0|exact Hmi| |exact E|exact I|exact L2].
        apply tmclean_app; [exact T|exact E]. }
    clear -Q. induction Q; constructor; assumption.
  - intros mi g tm e s d s' H Hi Hw Hg T E L. cbn [symbol_type] in H.
    destruct (gs_kind g); try noerr' H.
    + destruct (nassoc (gs_id g) tm) eqn:Ey; [|noerr' H]. injection H as <- <-. exact (T _ _ Ey).
    + apply bind_inv in H as (xs & s1 & H1 & H); injection H as <- <-;
        unfold nominal_args in H1; apply bind_inv in H1 as (rs & s2 & H2 & H1);
        match type of H1 with mmap ?fn _ _ = _ =>
        assert (MR : forall r, In r rs -> forall q s3, fn r s2 = Good (q,s3) -> s3=s2 /\ ety_ok (snd q)=true)
          by (intros r _ q s3 H3; cbv beta in H3; destruct (nassoc r tm) eqn:Ey; [injection H3 as <- <-; split; [reflexivity|exact (T _ _ Ey)]|noerr' H3]) end;
        destruct (mmap_reader _ _ _ (fun r q => ety_ok (snd q)=true) rs s2 xs s1 MR H1) as [-> PP];
        cbn [ety_ok]; apply andb_true_iff; split;
        [apply negb_true_iff; apply Nat.eqb_neq; exact (loader_nonzero w R Fl b Hload _ _ Hg)|
         apply forallb_forall; intros q Hq; clear -PP Hq; induction PP; [contradiction|]; destruct Hq as [<-|Hq]; auto].
    + destruct (gs_decl g) as [| |n0 t0| | | | | |]; try noerr' H.
      eapply IE; [exact H|exact Hi|exact Hw|exact (loader_mod_in w R Fl b Hload _ _ Hg)|exact T|exact E|exact I|exact L].
    + apply bind_inv in H as (xs & s1 & H1 & H); injection H as <- <-;
        unfold nominal_args in H1; apply bind_inv in H1 as (rs & s2 & H2 & H1);
        match type of H1 with mmap ?fn _ _ = _ =>
        assert (MR : forall r, In r rs -> forall q s3, fn r s2 = Good (q,s3) -> s3=s2 /\ ety_ok (snd q)=true)
          by (intros r _ q s3 H3; cbv beta in H3; destruct (nassoc r tm) eqn:Ey; [injection H3 as <- <-; split; [reflexivity|exact (T _ _ Ey)]|noerr' H3]) end;
        destruct (mmap_reader _ _ _ (fun r q => ety_ok (snd q)=true) rs s2 xs s1 MR H1) as [-> PP];
        cbn [ety_ok]; apply andb_true_iff; split;
        [apply negb_true_iff; apply Nat.eqb_neq; exact (loader_nonzero w R Fl b Hload _ _ Hg)|
         apply forallb_forall; intros q Hq; clear -PP Hq; induction PP; [contradiction|]; destruct Hq as [<-|Hq]; auto].
Qed.

Theorem source_clean_at : forall f, ETyClean_at f /\ SubClean_at f /\ SymClean_at f.
Proof. induction f as [|f [IE [IS IY]]]; [exact clean_zero_at|exact (clean_step_at f IE IS IY)]. Qed.

Lemma ety_of_clean_at : forall t mi tm e this s d s',
  ety_of t mi tm e this s = Good (d,s') -> Inv s -> st_w s = w ->
  In mi (w_order w) -> TmClean tm -> TmClean (tymap e) ->
  (match this with Some y => ety_ok y = true | None => True end) ->
  Later w P R Fl s' -> ety_ok d = true.
Proof.
  intros t mi tm e this s d s' H Hi Hw Hmi T E Wth L.
  rewrite ety_of_eq in H.
  exact (proj1 (source_clean_at 200) _ _ _ _ _ _ _ _ H Hi Hw Hmi T E Wth L).
Qed.

Definition ETyClean f : Prop := forall t mi tm e this s d s',
  elab_type f t mi tm e this s = Good (d,s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> TmClean tm -> EnvClean e ->
  (match this with Some y => ety_ok y = true | None => True end) ->
  Later w P R Fl s' -> ety_ok d = true.
Definition SubClean f : Prop := forall e mi bs outer s l s',
  app_subst f e mi bs outer s = Good (l,s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> TmClean outer -> EnvClean e ->
  Later w P R Fl s' -> Forall (fun q => ety_ok (snd q) = true) l.
Definition SymClean f : Prop := forall mi g tm e s d s',
  symbol_type f mi g tm e s = Good (d,s') ->
  Inv s -> st_w s = w -> nget (w_syms w) (gs_id g) = Some g ->
  TmClean tm -> EnvClean e -> Later w P R Fl s' -> ety_ok d = true.

Theorem source_clean : forall f, ETyClean f /\ SubClean f /\ SymClean f.
Proof.
 intro f. destruct(source_clean_at f)as[IE[IS IY]]. split; [|split].
 - intros t mi tm e this s d s' H Hi Hw Hmi T E Wth L.
   exact(IE _ _ _ _ _ _ _ _ H Hi Hw Hmi T (tmclean_of_forall _ (tymap_ok e E)) Wth L).
 - intros e mi bs outer s l s' H Hi Hw Hmi T E L.
   exact(IS _ _ _ _ _ _ _ H Hi Hw Hmi T (tmclean_of_forall _ (tymap_ok e E)) L).
 - intros mi g tm e s d s' H Hi Hw Hg T E L.
   exact(IY _ _ _ _ _ _ _ H Hi Hw Hg T (tmclean_of_forall _ (tymap_ok e E)) L).
Qed.
Lemma ety_of_clean : forall t mi tm e this s d s',
 ety_of t mi tm e this s=Good(d,s') -> Inv s -> st_w s=w ->
 In mi(w_order w) -> TmClean tm -> EnvClean e ->
 (match this with Some y=>ety_ok y=true|None=>True end) ->
 Later w P R Fl s' -> ety_ok d=true.
Proof.
 intros t mi tm e this s d s' H Hi Hw Hmi T E Wth L.
 exact(ety_of_clean_at _ _ _ _ _ _ _ _ H Hi Hw Hmi T (tmclean_of_forall _ (tymap_ok e E)) Wth L).
Qed.
Lemma fn_sig_clean : forall g tm e this s sg s',
  fn_sig g tm e this s = Good (sg,s') -> Inv s -> st_w s = w ->
  nget (w_syms w) (gs_id g) = Some g -> TmClean tm -> EnvClean e ->
  (match this with Some y => ety_ok y = true | None => True end) ->
  Later w P R Fl s' -> SigOK sg.
Proof.
  intros g tm e this s sg s' H Hi Hw Hg T E Wth L.
  unfold fn_sig in H. apply bind_inv in H as (ps & s1 & H1 & H).
  apply bind_inv in H as (r & s2 & H2 & H). injection H as <- <-.
  assert (Of : ok (match fn_ret g with Some t => ety_of t (gs_mod g) tm e this | None => ret YUnit end)).
  { destruct (fn_ret g); okt. }
  match type of H1 with ?m s = _ => assert (O1 : ok m) by (apply ok_mmap; intro p; apply ok_bind; [apply ok_ety_of|intro; apply ok_ret]) end.
  destruct (O1 _ _ _ Hi H1) as [Hi1 F1].
  destruct (Of _ _ _ Hi1 H2) as [Hi2 F2].
  assert (Q : Forall2 (fun p q => ety_ok (snd q) = true) (fn_params g) ps).
  { refine (mmap_ok_rel w R Fl _ _ _ _ _ s ps s1 _ _ H1 Hi Hw (clean_later _ _ F2 L)).
    - intro p. apply ok_bind; [apply ok_ety_of|intro; apply ok_ret].
    - intros p _ s0 q s3 H3 Hi0 Hw0 L3. apply bind_inv in H3 as (y & s4 & H4 & H3). injection H3 as <- <-.
      exact (ety_of_clean _ _ _ _ _ _ _ _ H4 Hi0 Hw0 (loader_mod_in w R Fl b Hload _ _ Hg) T E Wth L3). }
  unfold SigOK. cbn [sg_params sg_ret sg_this]. split.
  - apply Forall_forall. intros y Hy. apply in_map_iff in Hy as (q & <- & Hq).
    clear -Q Hq. induction Q; [contradiction|]. destruct Hq as [<-|Hq]; auto.
  - split; [|exact Wth]. destruct (fn_ret g).
    + exact (ety_of_clean _ _ _ _ _ _ _ _ H2 Hi1 (fr_w' w _ _ F1 Hw) (loader_mod_in w R Fl b Hload _ _ Hg) T E Wth L).
    + injection H2 as <- _. reflexivity.
Qed.
End Clean.
