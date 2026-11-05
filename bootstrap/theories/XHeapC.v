(* ------------------------------------------------------------------------ *)
(* XHeapC.v — MossX's concrete GC heap satisfies XHeap.HEAP_LAWS.            *)
(*                                                                           *)
(* The heap and a GC array's elements are tries keyed by bits (HTrie.v), so *)
(* every law is proved from the tries' get/set laws, without the kernel's   *)
(* array axioms: the certificate audits `ConcreteHeap`.                     *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool PrimInt63 Uint63.
From NM Require Import Audit HTrie MossX XHeap.
Import ListNotations.

(* loop_fuel is 2^34 as a nat: never compute it *)
Local Opaque loop_fuel.

(* A heap cell as a store typing says. *)
Definition all_elems (Q : xvalue -> Prop) (xs : garr) : Prop :=
  Q (fst xs) /\ forall kk v, tget_l (snd xs) kk = Some v -> Q v.

Definition cell_ok (o : option hobj) (k : bool) (Q : xvalue -> Prop) : Prop :=
  match o with
  | Some (HBox v) => k = false /\ Q v
  | Some (HArr xs _) => k = true /\ all_elems Q xs
  | _ => False
  end.

Definition heap_okc (Psi : hty) (st : mstate) : Prop :=
  forall kk k Q, Psi kk = Some (k, Q) -> cell_ok (tget_l (ms_heap st) kk) k Q.

(* ---- arrays ---- *)

Lemma garr_get_ok : forall Q xs i, all_elems Q xs -> Q (garr_get xs i).
Proof.
  intros Q [d m] i [Hd Hm]. unfold garr_get, all_elems in *. simpl in *. rewrite tget_key.
  destruct (tget_l m (key i)) eqn:E; [exact (Hm _ _ E) | exact Hd].
Qed.

Lemma garr_set_ok : forall Q xs i v, all_elems Q xs -> Q v -> all_elems Q (garr_set xs i v).
Proof.
  intros Q [d m] i v [Hd Hm] Hv. unfold all_elems, garr_set in *. simpl in *.
  split; [exact Hd |].
  intros kk w. rewrite tset_key.
  destruct (list_eq_dec bool_dec (key i) kk) as [<- | Hne].
  - rewrite tget_l_set_same. intros E; injection E as <-. exact Hv.
  - rewrite (tget_l_set_other _ _ _ _ _ Hne). apply Hm.
Qed.

Lemma int_loop_inv : forall A (I : A -> Prop) fuel i n f acc acc',
  int_loop fuel i n f acc = Some acc' -> I acc ->
  (forall j a a', I a -> f j a = Some a' -> I a') -> I acc'.
Proof.
  intros A I fuel. induction fuel as [|fuel IH]; intros i n f acc acc' H Ha Hf; simpl in H;
    [discriminate |].
  destruct (i <? n)%uint63.
  - destruct (f i acc) as [a'|] eqn:E; [| discriminate].
    exact (IH _ _ _ _ _ H (Hf _ _ _ Ha E) Hf).
  - injection H as <-. exact Ha.
Qed.

(* ---- the heap ---- *)

Lemma heap_set_ok : forall Psi st a o k Q,
  heap_okc Psi st -> Psi (key a) = Some (k, Q) -> cell_ok (Some o) k Q ->
  heap_okc Psi (heap_set st a o).
Proof.
  intros Psi st a o k Q H Ha Ho kk k' Q' Hk. unfold heap_set. simpl. rewrite tset_key.
  destruct (list_eq_dec bool_dec (key a) kk) as [<- | Hne].
  - rewrite tget_l_set_same. rewrite Ha in Hk. injection Hk as <- <-. exact Ho.
  - rewrite (tget_l_set_other _ _ _ _ _ Hne). exact (H _ _ _ Hk).
Qed.

Definition hupd (Psi : hty) (kk : list bool) (c : bool * (xvalue -> Prop)) : hty :=
  fun kk' => if list_eq_dec bool_dec kk kk' then Some c else Psi kk'.

Lemma heap_alloc_ok : forall Psi st o a s k Q,
  heap_okc Psi st -> heap_alloc st o = Some (a, s) -> cell_ok (Some o) k Q ->
  Psi (key a) = None /\ heap_okc (hupd Psi (key a) (k, Q)) s /\ hext Psi (hupd Psi (key a) (k, Q)).
Proof.
  intros Psi st o a s k Q H Hal Ho. unfold heap_alloc in Hal.
  destruct (heap_max <=? ms_hnext st)%uint63; [discriminate |].
  destruct (tget (ms_heap st) (ms_hnext st)) eqn:Et; [discriminate |].
  injection Hal as <- <-. rewrite tget_key in Et.
  assert (Hfresh : Psi (key (ms_hnext st)) = None).
  { destruct (Psi (key (ms_hnext st))) as [[k0 Q0]|] eqn:E; [| reflexivity].
    pose proof (H _ _ _ E) as C. rewrite Et in C. destruct C. }
  split; [exact Hfresh |]. split.
  - intros kk k' Q' Hk. simpl. rewrite tset_key. unfold hupd in Hk.
    destruct (list_eq_dec bool_dec (key (ms_hnext st)) kk) as [<- | Hne].
    + injection Hk as <- <-. rewrite tget_l_set_same. exact Ho.
    + rewrite (tget_l_set_other _ _ _ _ _ Hne). exact (H _ _ _ Hk).
  - intros kk k' Q' Hk. unfold hupd.
    destruct (list_eq_dec bool_dec (key (ms_hnext st)) kk) as [<- | Hne]; [congruence | exact Hk].
Qed.

Lemma hext_refl : forall Psi, hext Psi Psi.
Proof. intros Psi a k Q H. exact H. Qed.

Lemma ref_cell : forall Psi st k Q r,
  heap_okc Psi st -> href Psi k r Q ->
  exists n a, r = XVTag n (XVRef a) /\ Psi (key a) = Some (k, Q)
  /\ cell_ok (Some (heap_get st a)) k Q.
Proof.
  intros Psi st k Q r H [n [a [-> Ha]]]. exists n, a. split; [reflexivity |]. split; [exact Ha |].
  pose proof (H _ _ _ Ha) as C. unfold heap_get. rewrite tget_key.
  destruct (tget_l (ms_heap st) (key a)); [exact C | destruct C].
Qed.

(* A GC primitive runs as gc_prim. *)
Lemma run_gc : forall P p ws st, is_gc_prim p = true ->
  ConcretePrims.run_prim P p ws st = gc_prim p ws st.
Proof.
  intros P p ws st Hg. unfold ConcretePrims.run_prim, run_prim. rewrite Hg.
  destruct p; try discriminate Hg; reflexivity.
Qed.

Module ConcreteHeap <: HEAP_LAWS ConcretePrims.
  Definition heap_ok := heap_okc.

  Lemma heap_mono : forall Psi Psi' st,
    heap_ok Psi st ->
    (forall a k Q', Psi' a = Some (k, Q') ->
       exists Q, Psi a = Some (k, Q) /\ forall v, Q v -> Q' v) ->
    heap_ok Psi' st.
  Proof.
    intros Psi Psi' st H Hw kk k Q' Hk.
    destruct (Hw _ _ _ Hk) as [Q [E Himp]].
    pose proof (H _ _ _ E) as C.
    destruct (tget_l (ms_heap st) kk) as [[|v|xs n]|]; simpl in C |- *; try exact C.
    - destruct C as [Ek Hv]. split; [exact Ek | exact (Himp _ Hv)].
    - destruct C as [Ek [Hd Hm]]. split; [exact Ek |]. split; [exact (Himp _ Hd) |].
      intros kk' v Hv. exact (Himp _ (Hm _ _ Hv)).
  Qed.

  Lemma heap_empty : forall st, heap_ok (fun _ => None) st.
  Proof. intros st kk k Q E. discriminate E. Qed.

  Lemma heap_other : forall P p ws st Psi,
    is_gc p = false -> heap_ok Psi st -> heap_ok Psi (snd (ConcretePrims.run_prim P p ws st)).
  Proof.
    intros P p ws st Psi Hg H.
    assert (Hg' : is_gc_prim p = false) by (destruct p; simpl in Hg |- *; congruence).
    unfold ConcretePrims.run_prim, run_prim. rewrite Hg'.
    destruct (run_prim0 P p (mem_args p ws) st) as [o s]. exact H.
  Qed.

  Ltac post_refl := exists _; split; [eassumption | split; [apply hext_refl |]].

  Lemma heap_gc : forall P p ws st Psi Q,
    heap_ok Psi st -> gc_pre Psi p ws Q ->
    match ConcretePrims.run_prim P p ws st with
    | (XOk w, st') => exists Psi', heap_ok Psi' st' /\ gc_post Psi Psi' p Q w
    | (XTrap _, st') => heap_ok Psi st'
    | _ => False
    end.
  Proof.
    intros P p ws st Psi Q H Hpre. unfold heap_ok in *.
    assert (Hg : is_gc_prim p = true) by (destruct p; simpl in Hpre; try contradiction; reflexivity).
    rewrite (run_gc P p ws st Hg).
    destruct p; simpl in Hpre; try contradiction; clear Hg;
      repeat (match goal with
              | Hp : match ?ws with _ => _ end |- _ =>
                  destruct ws as [|?w ws]; try contradiction
              | Hp : match ?w with _ => _ end |- _ =>
                  destruct w; try contradiction
              end); cbn [gc_prim].
    - (* box *)
      destruct (heap_alloc st (HBox w)) as [[a s]|] eqn:Ea; [| exact H].
      destruct (@heap_alloc_ok Psi st (HBox w) a s false Q H Ea (conj eq_refl Hpre))
        as [Hf [Hok Hext]].
      exists (hupd Psi (key a) (false, Q)). split; [exact Hok |]. split; [exact Hext |].
      exists a. split; [reflexivity |]. split; [exact Hf |].
      unfold hupd. destruct (list_eq_dec bool_dec (key a) (key a)); [reflexivity | congruence].
    - (* deref *)
      destruct (@ref_cell Psi st false Q w H Hpre) as [n [a [-> [Ha C]]]]. cbn [ref_of].
      destruct (heap_get st a) as [|v|xs m]; simpl in C; try contradiction.
      + destruct C as [_ Hv]. exists Psi. split; [exact H |]. split; [apply hext_refl | exact Hv].
      + destruct C as [E _]. discriminate.
    - (* ref set *)
      destruct Hpre as [Hr Hv].
      destruct (@ref_cell Psi st false Q w H Hr) as [n [a [-> [Ha C]]]]. cbn [ref_of].
      destruct (heap_get st a) as [|v|xs m]; simpl in C; try contradiction.
      + exists Psi. split; [exact (@heap_set_ok Psi st a (HBox w0) false Q H Ha (conj eq_refl Hv)) |].
        split; [apply hext_refl | reflexivity].
      + destruct C as [E _]. discriminate.
    - (* array new *)
      destruct (heap_max <? i)%uint63; [exact H |].
      destruct (heap_alloc st (HArr (w, TE) i)) as [[a s]|] eqn:Ea; [| exact H].
      assert (Hc : cell_ok (Some (HArr (w, TE) i)) true Q).
      { split; [reflexivity |]. split; [exact Hpre | intros kk v E; discriminate E]. }
      destruct (@heap_alloc_ok Psi st _ a s true Q H Ea Hc) as [Hf [Hok Hext]].
      exists (hupd Psi (key a) (true, Q)). split; [exact Hok |]. split; [exact Hext |].
      exists a. split; [reflexivity |]. split; [exact Hf |].
      unfold hupd. destruct (list_eq_dec bool_dec (key a) (key a)); [reflexivity | congruence].
    - (* array length *)
      destruct (@ref_cell Psi st true Q w H Hpre) as [n [a [-> [Ha C]]]].
      unfold arr_of. cbn [ref_of].
      destruct (heap_get st a) as [|v|xs m]; simpl in C; try contradiction.
      + destruct C as [E _]. discriminate.
      + exists Psi. split; [exact H |]. split; [apply hext_refl | eexists; reflexivity].
    - (* array get *)
      destruct (@ref_cell Psi st true Q w H Hpre) as [n [a [-> [Ha C]]]].
      unfold arr_of. cbn [ref_of].
      destruct (heap_get st a) as [|v|xs m]; simpl in C; try contradiction.
      + destruct C as [E _]. discriminate.
      + destruct C as [_ Hall]. destruct (i <? m)%uint63; [| exact H].
        exists Psi. split; [exact H |]. split; [apply hext_refl | exact (garr_get_ok _ _ i Hall)].
    - (* array set *)
      destruct Hpre as [Hr Hv].
      destruct (@ref_cell Psi st true Q w H Hr) as [n [a [-> [Ha C]]]].
      unfold arr_of. cbn [ref_of].
      destruct (heap_get st a) as [|v|xs m]; simpl in C; try contradiction.
      + destruct C as [E _]. discriminate.
      + destruct C as [_ Hall]. destruct (i <? m)%uint63; [| exact H].
        exists Psi. split.
        * apply (@heap_set_ok Psi st a _ true Q H Ha).
          split; [reflexivity | exact (garr_set_ok _ _ i _ Hall Hv)].
        * split; [apply hext_refl | reflexivity].
    - (* array fill *)
      destruct Hpre as [Hr Hv].
      destruct (@ref_cell Psi st true Q w H Hr) as [n [a [-> [Ha C]]]].
      unfold arr_of. cbn [ref_of].
      destruct (heap_get st a) as [|v|xs m]; simpl in C; try contradiction.
      + destruct C as [E _]. discriminate.
      + destruct C as [_ Hall]. destruct (m <? i + i0)%uint63; [exact H |].
        destruct (int_loop loop_fuel 0 i0 _ xs) as [ys|] eqn:El; [| exact H].
        assert (Hys : all_elems Q ys).
        { refine (int_loop_inv _ (all_elems Q) _ _ _ _ _ _ El Hall _).
          intros j b b' Hb E. injection E as <-. exact (garr_set_ok _ _ _ _ Hb Hv). }
        exists Psi. split.
        * apply (@heap_set_ok Psi st a _ true Q H Ha). split; [reflexivity | exact Hys].
        * split; [apply hext_refl | reflexivity].
    - (* array copy *)
      destruct Hpre as [Hr [Q2 [Hr2 Himp]]].
      destruct (@ref_cell Psi st true Q w H Hr) as [n [a [-> [Ha C]]]].
      destruct (@ref_cell Psi st true Q2 w0 H Hr2) as [n2 [a2 [-> [Ha2 C2]]]].
      unfold arr_of. cbn [ref_of].
      destruct (heap_get st a) as [|v|xs m]; simpl in C; try contradiction;
        [destruct C as [E _]; discriminate |].
      destruct (heap_get st a2) as [|v2|ys m2]; simpl in C2; try contradiction;
        [destruct C2 as [E _]; discriminate | ].
      destruct C as [_ Hall]. destruct C2 as [_ Hall2].
      destruct ((m <? i + i1)%uint63 || (m2 <? i0 + i1)%uint63); [exact H |].
      destruct (int_loop loop_fuel 0 i1 _ xs) as [zs|] eqn:El; [| exact H].
      assert (Hzs : all_elems Q zs).
      { refine (int_loop_inv _ (all_elems Q) _ _ _ _ _ _ El Hall _).
        intros j b b' Hb E. injection E as <-.
        apply (garr_set_ok _ _ _ _ Hb). apply Himp. exact (garr_get_ok _ _ _ Hall2). }
      exists Psi. split.
      * apply (@heap_set_ok Psi st a _ true Q H Ha). split; [reflexivity | exact Hzs].
      * split; [apply hext_refl | reflexivity].
  Qed.
End ConcreteHeap.
