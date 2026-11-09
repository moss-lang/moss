(* Formation of types and contexts (T29). A formed type is built the way
   TyOf builds types: every nominal is applied at exactly its requirements,
   and every union has nominal members with distinct heads, at every depth.
   Substitution by formed types keeps types formed, and so do the readings
   TyOf, Brackets and SigOf over formed type tables. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead PublicCallRealize PublicArms.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Formation.
Variable W : A.world.

Fixpoint TyFormed (t : ty) : Prop :=
  match t with
  | TMachine _ | TUnit | TAbs _ => True
  | TRecord fs => (fix go (fs : list (string * ty)) := match fs with [] => True | (_, u) :: r => TyFormed u /\ go r end) fs
  | TNom n xs => map fst xs = A.w_reqs W n /\
      (fix go (xs : list (nat * ty)) := match xs with [] => True | (_, u) :: r => TyFormed u /\ go r end) xs
  | TUnion ms => Forall (fun m => exists n xs, m = TNom n xs) ms /\ NoDup (map A.head ms) /\
      (fix go (ms : list ty) := match ms with [] => True | u :: r => TyFormed u /\ go r end) ms
  end.

Definition TmFormed (tm : list (nat * ty)) : Prop := forall x u, In (x, u) tm -> TyFormed u.

Lemma record_formed : forall fs, TyFormed (TRecord fs) <-> forall n u, In (n, u) fs -> TyFormed u.
Proof.
  intros fs. cbn. induction fs as [|[n u] r IH]; split; intros H.
  - intros n u [].
  - exact I.
  - destruct H as [Hu Hr]. intros n' u' [E|I]; [injection E as -> ->; exact Hu|exact (proj1 IH Hr n' u' I)].
  - split; [exact (H n u (or_introl eq_refl))|]. apply IH. intros n' u' I. exact (H n' u' (or_intror I)).
Qed.

Lemma args_formed : forall (xs : list (nat * ty)),
  (fix go (xs : list (nat * ty)) := match xs with [] => True | (_, u) :: r => TyFormed u /\ go r end) xs <->
  TmFormed xs.
Proof.
  intros xs. induction xs as [|[n u] r IH]; split; intros H.
  - intros x v [].
  - exact I.
  - destruct H as [Hu Hr]. intros x v [E|I]; [injection E as -> ->; exact Hu|exact (proj1 IH Hr x v I)].
  - split; [exact (H n u (or_introl eq_refl))|]. apply IH. intros x v I. exact (H x v (or_intror I)).
Qed.

Lemma members_formed : forall (ms : list ty),
  (fix go (ms : list ty) := match ms with [] => True | u :: r => TyFormed u /\ go r end) ms <->
  Forall TyFormed ms.
Proof.
  intros ms. induction ms as [|u r IH]; split; intros H.
  - constructor.
  - exact I.
  - destruct H as [Hu Hr]. constructor; [exact Hu|exact (proj1 IH Hr)].
  - inversion H; subst. split; [assumption|apply IH; assumption].
Qed.

Lemma nom_formed : forall n xs, TyFormed (TNom n xs) <-> map fst xs = A.w_reqs W n /\ TmFormed xs.
Proof. intros n xs. cbn [TyFormed]. rewrite args_formed. reflexivity. Qed.

Lemma union_formed : forall ms, TyFormed (TUnion ms) <->
  Forall (fun m => exists n xs, m = TNom n xs) ms /\ NoDup (map A.head ms) /\ Forall TyFormed ms.
Proof. intros ms. cbn [TyFormed]. rewrite members_formed. reflexivity. Qed.

Definition SubstFormed (s : subst) : Prop := forall x u, lookup_subst x s = Some u -> TyFormed u.

Lemma subst_formed : forall t s, SubstFormed s -> TyFormed t -> TyFormed (app_subst s t).
Proof.
  fix IH 1. intros [m|fields| |g|n xs|ms] s Sf Ft.
  - exact I.
  - rewrite subst_record. cbn [TyFormed] in Ft |- *.
    induction fields as [|[f u] r Tail]; cbn in Ft |- *; [exact I|].
    destruct Ft as [Fu Fr]. split; [exact (IH u s Sf Fu)|exact (Tail Fr)].
  - exact I.
  - cbn [app_subst]. destruct (lookup_subst g s) as [u|] eqn:L; [exact (Sf g u L)|exact I].
  - rewrite subst_nom. cbn [TyFormed] in Ft |- *. destruct Ft as [Keys Args].
    split; [unfold PublicTypeRead.smap; rewrite map_map; exact Keys|]. clear Keys.
    unfold PublicTypeRead.smap. induction xs as [|[x u] r Tail]; cbn in Args |- *; [exact I|].
    destruct Args as [Fu Fr]. split; [exact (IH u s Sf Fu)|exact (Tail Fr)].
  - rewrite subst_union. cbn [TyFormed] in Ft |- *. destruct Ft as [Nom [Nd Fm]].
    split; [|split].
    + rewrite Forall_forall in Nom |- *. intros m' I. apply in_map_iff in I as [m [<- I]].
      destruct (Nom m I) as [n [xs ->]]. rewrite subst_nom. eauto.
    + replace (map A.head (map (app_subst s) ms)) with (map A.head ms); [exact Nd|].
      rewrite map_map. apply map_ext_in. intros m I. rewrite Forall_forall in Nom.
      destruct (Nom m I) as [n [xs ->]]. rewrite subst_nom. reflexivity.
    + clear Nom Nd. induction ms as [|u r Tail]; cbn in Fm |- *; [exact I|].
      destruct Fm as [Fu Fr]. split; [exact (IH u s Sf Fu)|exact (Tail Fr)].
Qed.

Lemma tm_app_formed : forall l m, TmFormed l -> TmFormed m -> TmFormed (l ++ m).
Proof. intros l m Fl Fm x u I. apply in_app_or in I as [I|I]; [exact (Fl x u I)|exact (Fm x u I)]. Qed.

Lemma members_of_formed : forall t, TyFormed t -> Forall TyFormed (A.members t).
Proof.
  intros [m|fs| |g|n xs|ms] Ft; cbn [A.members]; try (constructor; [exact Ft|constructor]).
  apply union_formed in Ft as [_ [_ Fm]]. exact Fm.
Qed.

Lemma keys_of : forall (reqs : list nat) (xs : list (nat * ty)) (R : nat -> ty -> Prop),
  Forall2 (fun r ra => fst ra = r /\ R r (snd ra)) reqs xs -> map fst xs = reqs.
Proof. intros reqs xs R F; induction F as [|r ra rs ras [E _] _ IH]; cbn; [reflexivity|congruence]. Qed.

Fixpoint tyof_formed p tm th a t (D : A.TyOf W p tm th a t) {struct D} :
  TmFormed tm -> (forall x, th = Some x -> TyFormed x) -> TyFormed t
with br_formed p tm h x l (D : A.Brackets W p tm h x l) {struct D} : TmFormed tm -> TmFormed l.
Proof.
  - destruct D as [p tm th|p tm th|p tm t|p tm th fs ts Nd F|p tm th ms ts flat F Ef Fm Nd
                  |p tm th pa t tau R K L|p tm th pa a g0 k bs args R K Kk B F|p tm th pa a g0 d n body bs tau R D A B T];
      intros Tm Th.
    + cbn. split; [constructor|split; [constructor|exact I]].
    + exact I.
    + exact (Th t eq_refl).
    + cbn [TyFormed]. clear Nd. induction F as [|nt ft fs' ts' [E1 Hft] Fr IH]; [exact I|].
      destruct ft as [f t]. split; [exact (tyof_formed _ _ _ _ _ Hft Tm Th)|exact IH].
    + apply union_formed. split; [exact Fm|split; [exact Nd|]]. subst flat. clear Fm Nd.
      induction F as [|a t' ms' ts' Ht Fr IH]; [constructor|]. cbn [flat_map].
      apply Forall_app. split; [|exact IH].
      apply members_of_formed. exact (tyof_formed _ _ _ _ _ Ht Tm Th).
    + exact (Tm t tau (nlook_in _ _ _ _ L)).
    + apply nom_formed. split; [exact (keys_of _ _ (fun r v => A.nlook r (bs ++ tm)%list = Some v) F)|].
      pose proof (tm_app_formed _ _ (br_formed _ _ _ _ _ B Tm) Tm) as Btm. clear B.
      intros x u I. clear Kk. induction F as [|r ra rs ras [E1 L1] Fr IH]; [contradiction|].
      destruct I as [E|I]; [subst ra; exact (Btm _ _ (nlook_in _ _ _ _ L1))|exact (IH I)].
    + apply (tyof_formed _ _ _ _ _ T); [exact (tm_app_formed _ _ (br_formed _ _ _ _ _ B Tm) Tm)|intros x E; discriminate].
  - destruct D as [p tm h|p tm h bs l F]; intros Tm; [intros x u []|].
    intros x u I. apply (proj2 (in_rev _ _)) in I.
    induction F as [|b0 tb bs' l' (sp & sa & E1 & R1 & K1 & Q1 & T1) Fr IH]; [contradiction|].
    destruct I as [E|I]; [|exact (IH I)]. destruct tb as [y v]. injection E as -> ->.
    exact (tyof_formed _ _ _ _ _ T1 Tm (fun x E => ltac:(discriminate E))).
Qed.

End Formation.

(* ======================================================================== *)
(* Formation of contexts (T29)                                               *)
(* ======================================================================== *)

Scheme oie_ind' := Induction for A.OutputItemEntries Sort Prop
with oies_ind' := Induction for A.OutputItemsEntries Sort Prop.
Combined Scheme oies_mut from oie_ind', oies_ind'.

Section Ok.
Variable W : A.world.
Import A.

(* A type is ok when it is formed and every generation it mentions is
   abstract: never a tag or unit symbol (so a head that is a tag or unit
   symbol is a nominal head). *)
Definition Abstract (g : nat) : Prop := kind_sym W g <> Some DKTag /\ kind_sym W g <> Some DKUnit.
Definition TyOk (t : ty) : Prop := TyFormed W t /\ forall g, In g (gens t) -> Abstract g.
Definition OptOk (o : option ty) : Prop := forall t, o = Some t -> TyOk t.
Definition TmOk (tm : list (nat * ty)) : Prop := forall x u, In (x, u) tm -> TyOk u.
Definition SigP (P : ty -> Prop) (g : fsig) : Prop := Forall P (sg_params g) /\ P (sg_ret g).
Definition InstP (P : ty -> Prop) (a : inst) : Prop := forall x t, In (x, Some t) a -> P t.
Definition SigOk := SigP TyOk.
Definition InstOk := InstP TyOk.
Definition ProvOk (p : provision) : Prop :=
  (forall g, provision_signature p = Some g -> SigOk g) /\ InstOk (provision_inst p).
Definition KeyOk (k : item) : Prop := match k with IMth r _ _ => TyOk r | _ => True end.
Definition EntryP (P : ty -> Prop) (e : entry) : Prop :=
  match e with
  | NTy _ => True
  | NVal _ t a => P t /\ InstP P a
  | NFn _ g a => SigP P g /\ InstP P a
  | NMth r _ g a => P r /\ SigP P g /\ InstP P a
  | NFun _ fx => forall x u, In (x, u) fx -> P u
  end.

(* ---- small facts ---------------------------------------------------------- *)

Lemma gens_nom : forall n xs, gens (TNom n xs) = tm_gens xs.
Proof. intros n xs. cbn. unfold tm_gens. induction xs as [|[x u] r IH]; cbn; [reflexivity|rewrite IH; reflexivity]. Qed.

Lemma gens_record : forall fs, gens (TRecord fs) = flat_map (fun q => gens (snd q)) fs.
Proof. intros fs. cbn. induction fs as [|[f u] r IH]; cbn; [reflexivity|rewrite IH; reflexivity]. Qed.

Lemma tm_gens_in : forall l g, In g (tm_gens l) -> exists x u, In (x, u) l /\ In g (gens u).
Proof. intros l g Ig. unfold tm_gens in Ig. apply in_flat_map in Ig as [[x u] [Iq Ig]]. eauto. Qed.

Lemma in_tm_gens : forall l g x u, In (x, u) l -> In g (gens u) -> In g (tm_gens l).
Proof. intros l g x u Iq Ig. unfold tm_gens. apply in_flat_map. exists (x, u). split; assumption. Qed.

Lemma slook_in : forall X n (l : list (string * X)) v, slook n l = Some v -> In (n, v) l.
Proof.
  intros X n l v. induction l as [|[a x] r IH]; intros Found; [discriminate|]. cbn in Found.
  destruct (String.eqb_spec n a) as [->|_]; [injection Found as <-; left; reflexivity|right; exact (IH Found)].
Qed.

Lemma ilook_in_key : forall X k (l : list (item * X)) v, ilook k l = Some v -> In (k, v) l.
Proof.
  intros X k l v. induction l as [|[k' x] r IH]; intros Found; [discriminate|]. cbn in Found.
  destruct (item_eqb k k') eqn:E.
  - apply item_eqb_spec in E. subst k'. injection Found as <-. left. reflexivity.
  - right. exact (IH Found).
Qed.

Lemma mlook_in : forall r m (l : list ((ty * nat) * fsig)) g, mlook r m l = Some g -> exists q, In (q, g) l.
Proof.
  intros r m l g. induction l as [|[[r' m'] g'] rest IH]; intros Found; [discriminate|]. cbn in Found.
  destruct (ty_eqb r r' && Nat.eqb m m').
  - injection Found as <-. eexists. left. reflexivity.
  - destruct (IH Found) as [q Iq]. exists q. right. exact Iq.
Qed.

Lemma gens_keep : forall t s y, In y (gens t) -> lookup_subst y s = None -> In y (gens (app_subst s t)).
Proof.
  fix IH 1. intros [m|fields| |g|n th|ms] s y Iy L.
  - contradiction.
  - rewrite subst_record. cbn in Iy |- *. induction fields as [|[f t] rest Tail]; [contradiction|].
    cbn in Iy |- *. apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; exact (IH t s y Iy L)|right; exact (Tail Iy)].
  - contradiction.
  - cbn in Iy. destruct Iy as [<-|[]]. cbn. rewrite L. left. reflexivity.
  - rewrite subst_nom. unfold PublicTypeRead.smap. cbn in Iy |- *. induction th as [|[x t] rest Tail]; [contradiction|].
    cbn in Iy |- *. apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; exact (IH t s y Iy L)|right; exact (Tail Iy)].
  - rewrite subst_union. cbn in Iy |- *. induction ms as [|t rest Tail]; [contradiction|].
    cbn in Iy |- *. apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; exact (IH t s y Iy L)|right; exact (Tail Iy)].
Qed.

(* ---- ok types --------------------------------------------------------------- *)

Lemma tyok_unit : TyOk TUnit.
Proof. split; [exact Logic.I|intros g []]. Qed.

Lemma never_ok : TyOk never.
Proof. split; [cbn; split; [constructor|split; [constructor|exact Logic.I]]|intros g []]. Qed.

Lemma tyok_abs : forall g, Abstract g -> TyOk (TAbs g).
Proof. intros g A0. split; [exact Logic.I|intros g' [<-|[]]; exact A0]. Qed.

Lemma typed_abstract : forall x, kind_sym W x = Some DKType -> Abstract x.
Proof. intros x K. unfold Abstract. rewrite K. split; discriminate. Qed.

Lemma fresh_abstract : forall x, Fresh W x -> Abstract x.
Proof. intros x Fr. unfold Abstract, kind_sym. unfold Fresh in Fr. rewrite Fr. split; discriminate. Qed.

Lemma optok_none : OptOk None.
Proof. intros t E. discriminate. Qed.

Lemma optok_some : forall t, TyOk t -> OptOk (Some t).
Proof. intros t Ok t' E. injection E as <-. exact Ok. Qed.

Lemma tmok_nil : TmOk [].
Proof. intros x u []. Qed.

Lemma tmok_app : forall l m, TmOk l -> TmOk m -> TmOk (l ++ m).
Proof. intros l m Hl Hm x u Iq. apply in_app_or in Iq as [Iq|Iq]; [exact (Hl x u Iq)|exact (Hm x u Iq)]. Qed.

Lemma tmok_formed : forall tm, TmOk tm -> TmFormed W tm.
Proof. intros tm Ok x u Iq. exact (proj1 (Ok x u Iq)). Qed.

Lemma tmok_gens : forall tm g, TmOk tm -> In g (tm_gens tm) -> Abstract g.
Proof. intros tm g Ok Ig. destruct (tm_gens_in tm g Ig) as [x [u [Iq Iu]]]. exact (proj2 (Ok x u Iq) g Iu). Qed.

Lemma tyok_subst : forall s t, TmOk s -> TyOk t -> TyOk (app_subst s t).
Proof.
  intros s t Hs [Ft Gt]. split.
  - apply subst_formed; [|exact Ft]. intros x u L. exact (proj1 (Hs x u (lookup_subst_in_pair _ _ _ L))).
  - intros y Iy. destruct (gens_subst t s y Iy) as [z [Iz [[_ ->]|[u [L Iu]]]]]; [exact (Gt z Iz)|].
    exact (proj2 (Hs z u (lookup_subst_in_pair _ _ _ L)) y Iu).
Qed.

Lemma sigok_subst : forall s g, TmOk s -> SigOk g -> SigOk (sig_subst s g).
Proof.
  intros s [ps r] Hs [Ps R]. split; cbn in *; [|exact (tyok_subst _ _ Hs R)].
  apply Forall_map. apply (Forall_impl _ (fun t Ok => tyok_subst s t Hs Ok)). exact Ps.
Qed.

Lemma nom_ok_intro : forall n xs, map fst xs = w_reqs W n -> TmOk xs -> TyOk (TNom n xs).
Proof.
  intros n xs K Tm. split.
  - apply nom_formed. split; [exact K|exact (tmok_formed _ Tm)].
  - intros g Ig. rewrite gens_nom in Ig. exact (tmok_gens _ _ Tm Ig).
Qed.

Lemma nom_ok_elim : forall n xs, TyOk (TNom n xs) -> map fst xs = w_reqs W n /\ TmOk xs.
Proof.
  intros n xs [Ft Gt]. apply nom_formed in Ft as [K Tm]. split; [exact K|].
  intros x u Iq. split; [exact (Tm x u Iq)|]. intros g Ig. apply Gt. rewrite gens_nom. exact (in_tm_gens _ _ _ _ Iq Ig).
Qed.

Lemma record_ok_elim : forall fs f u, TyOk (TRecord fs) -> In (f, u) fs -> TyOk u.
Proof.
  intros fs f u [Ft Gt] Iq. split; [exact (proj1 (record_formed W fs) Ft f u Iq)|].
  intros g Ig. apply Gt. rewrite gens_record. apply in_flat_map. exists (f, u). split; assumption.
Qed.

Lemma members_ok : forall t m, TyOk t -> In m (members t) -> TyOk m.
Proof.
  intros t m [Ft Gt] Im. split.
  - pose proof (members_of_formed W t Ft) as Fm. rewrite Forall_forall in Fm. exact (Fm m Im).
  - intros g Ig. exact (Gt g (member_gens _ _ _ Im Ig)).
Qed.

Lemma branch_ty_ok : forall b t, TyOk t -> TyOk (branch_ty b t).
Proof. intros [ss [y|]] t Ok; cbn; [exact Ok|exact tyok_unit]. Qed.

Lemma join_ok : forall a b exp t, Join a b exp t -> TyOk a -> TyOk b -> OptOk exp -> TyOk t.
Proof. intros a b exp t J Ha Hb He. destruct J; [exact (He _ eq_refl)|exact Hb|exact Ha]. Qed.

(* ---- readings over ok tables ------------------------------------------------ *)

Lemma tyof_ok : forall p tm th a t, TyOf W p tm th a t -> TmOk tm -> OptOk th -> TyOk t.
Proof.
  intros p tm th a t T Tm Th. split.
  - apply (tyof_formed W _ _ _ _ _ T); [exact (tmok_formed _ Tm)|intros x E; exact (proj1 (Th x E))].
  - intros g Ig. destruct (tyof_gens W _ _ _ _ _ T g Ig) as [In'|[x [E Ix]]];
      [exact (tmok_gens _ _ Tm In')|exact (proj2 (Th x E) g Ix)].
Qed.

Lemma tyof_none_gens : forall p tm a t, TyOf W p tm None a t -> incl (gens t) (tm_gens tm).
Proof.
  intros p tm a t T g Ig. destruct (tyof_gens W _ _ _ _ _ T g Ig) as [In'|[x [E _]]]; [exact In'|discriminate].
Qed.

Lemma br_ok : forall p tm h x l, Brackets W p tm h x l -> TmOk tm -> TmOk l.
Proof.
  intros p tm h x l B Tm y u Iq. split.
  - exact (br_formed W _ _ _ _ _ B (tmok_formed _ Tm) y u Iq).
  - intros g Ig. apply (tmok_gens tm g Tm). apply (br_gens W _ _ _ _ _ B g). exact (in_tm_gens _ _ _ _ Iq Ig).
Qed.

Lemma forall2_tyof_ok : forall p tm th (l : list (name * aty)) ts,
  Forall2 (fun pt t => TyOf W p tm th (snd pt) t) l ts -> TmOk tm -> OptOk th -> Forall TyOk ts.
Proof.
  intros p tm th l ts F Tm Th. induction F as [|x y l l' H Fr IH].
  - constructor.
  - constructor; [exact (tyof_ok _ _ _ _ _ H Tm Th)|exact IH].
Qed.

Lemma sigof_ok : forall s tm this g, SigOf W s tm this g -> TmOk tm -> OptOk this -> SigOk g.
Proof.
  intros s tm this g (d & rcv & n & args & rt & body & D & Ast & Ps & R) Tm Th. split.
  - exact (forall2_tyof_ok _ _ _ _ _ Ps Tm Th).
  - destruct rt as [a|]; [exact (tyof_ok _ _ _ _ _ R Tm Th)|rewrite R; exact tyok_unit].
Qed.

Lemma reqs_ok : forall reqs xs tm,
  Forall2 (fun x xa => fst xa = x /\ nlook x tm = Some (snd xa)) reqs xs -> TmOk tm ->
  map fst xs = reqs /\ TmOk xs.
Proof.
  intros reqs xs tm F Tm. split; [exact (keys_of reqs xs (fun x v => nlook x tm = Some v) F)|].
  intros y u Iq. induction F as [|x xa xs' xas [E L] Fr IH]; [contradiction|].
  destruct Iq as [->|Iq]; [exact (Tm _ _ (nlook_in _ _ _ _ L))|exact (IH Iq)].
Qed.

Lemma recv_ok : forall r tm rt, RecvTy W r tm rt -> TmOk tm -> TyOk rt.
Proof.
  intros r tm rt Recv Tm. unfold RecvTy in Recv.
  destruct (nlook r tm) as [t'|] eqn:Found; [subst rt; exact (Tm _ _ (nlook_in _ _ _ _ Found))|].
  assert (Nom : forall args, Forall2 (fun x xa => fst xa = x /\ nlook x tm = Some (snd xa)) (w_reqs W r) args ->
    TyOk (TNom r args)).
  { intros args F. destruct (reqs_ok _ _ _ F Tm) as [K A0]. exact (nom_ok_intro r args K A0). }
  destruct (w_decl W r) as [d|].
  - destruct (d_ast d); try (destruct Recv as [args [-> F]]; exact (Nom args F)).
    exact (tyof_ok _ _ _ _ _ Recv Tm optok_none).
  - destruct Recv as [args [-> F]]. exact (Nom args F).
Qed.

Lemma recv_gens : forall r tm rt, RecvTy W r tm rt -> incl (gens rt) (tm_gens tm).
Proof.
  intros r tm rt Recv g Ig.
  pose proof (recv_scoped W r tm (map (fun g => (g, TUnit)) (tm_gens tm)) rt Recv) as H.
  rewrite map_map in H. cbn in H. rewrite map_id in H. exact (H (fun g I => I) g Ig).
Qed.

Lemma appof_ok : forall s tm a, AppOf W s tm a -> TmOk tm -> InstOk a.
Proof.
  intros s tm a [rs [-> _]] Tm x t Iq. apply in_map_iff in Iq as [r [E _]]. injection E as -> E.
  exact (Tm _ _ (nlook_in _ _ _ _ E)).
Qed.

Lemma payload_ok : forall s xs pay, PayloadOf W s xs pay -> TmOk xs -> TyOk pay.
Proof. intros s xs pay [s0 xs0 d n ps t pay0 D Ast T] Ok. exact (tyof_ok _ _ _ _ _ T Ok optok_none). Qed.

Lemma fieldpats_ok : forall F ftys fs bs xs, FieldPats W F ftys fs bs xs ->
  (forall n t, In (n, t) ftys -> TyOk t) -> forall n t, In (n, t) bs -> TyOk t.
Proof.
  intros F ftys fs bs xs FP Ok. induction FP as [|n t r bs xs S Nm FP IH|n t r bs xs S FP IH|n y t r bs xs S R Nm FP IH];
    intros n' t' Ib; [contradiction| |exact (IH n' t' Ib)|].
  - destruct Ib as [E|Ib]; [injection E as <- <-; exact (Ok _ _ (slook_in _ _ _ _ S))|exact (IH _ _ Ib)].
  - destruct Ib as [E|Ib]; [injection E as <- <-; exact (Ok _ _ (slook_in _ _ _ _ S))|exact (IH _ _ Ib)].
Qed.

Lemma patof_ok : forall F st pat xp binds hs ca, PatOf W F st pat xp binds hs ca -> TyOk st ->
  forall n t, In (n, t) binds -> TyOk t.
Proof.
  intros F st pat xp binds hs ca Pt Ok.
  destruct Pt as [|n R Nm|pa s k xs R K Kk Im|pa s xs y pay R K Im Pay Ry Nm|pa s xs fs ftys bs fxs R K Im Pay FP Nd];
    intros n' t' Ib.
  - contradiction.
  - destruct Ib as [E|[]]. injection E as _ <-. exact Ok.
  - contradiction.
  - destruct Ib as [E|[]]. injection E as _ <-. destruct (nom_ok_elim _ _ (members_ok _ _ Ok Im)) as [_ Xs].
    exact (payload_ok _ _ _ Pay Xs).
  - destruct (nom_ok_elim _ _ (members_ok _ _ Ok Im)) as [_ Xs]. pose proof (payload_ok _ _ _ Pay Xs) as Rok.
    exact (fieldpats_ok _ _ _ _ _ FP (fun n t Iq => record_ok_elim _ _ _ Rok Iq) n' t' Ib).
Qed.

End Ok.

Section Contexts.
Variable W : A.world.
Import A.

(* The part of `context_gens` that the type tables determine: merge keys,
   merge values, and the type table's meanings and their canonical forms.
   Unlike all of `context_gens`, it never shrinks: binds only add meanings,
   and replacing a provision leaves it alone. *)
Definition scope_gens (C : cx) : list nat :=
  flat_map (fun q => fst q :: gens (snd q)) (c_sub C) ++
  flat_map (fun q => gens (snd q) ++ gens (canon C (snd q))) (c_ty C).

(* (a) formation *)
Record CxFormed (C : cx) : Prop := {
  cf_ty : TmOk W (c_ty C);
  cf_sub : TmOk W (c_sub C);
  cf_prov : forall k p, In (k, p) (c_provisions C) -> KeyOk W k /\ ProvOk W p;
  cf_locals : forall n t mu, In (n, (t, mu)) (c_locals C) -> TyOk W t
}.

(* A generation is in scope up to declared symbols: in [scope_gens], or a
   declared type symbol. A callee requirement the caller leaves unbound is
   read at the call site as itself (`theta_at`, `at_site`), so a family
   demand can install keys and signatures that mention a declared type
   symbol outside the caller's table (examples/family-unbound-need*.moss,
   accepted by the oracle and native). Lambda capture keeps the meanings of
   method-key receivers' generations (`lexical_capture`), so no run-time
   fact about such symbols is needed. *)
Definition GenScoped (C : cx) (y : nat) : Prop := In y (scope_gens C) \/ kind_sym W y = Some DKType.
Definition ScopedTy (C : cx) (t : ty) : Prop := forall y, In y (gens t) -> GenScoped C y.

(* (b) scoping: a method key's receiver mentions only generations in scope
   (up to declared symbols) *)
Definition KeysScoped (C : cx) : Prop :=
  forall r m th p, In (IMth r m th, p) (c_provisions C) -> ScopedTy C r.

(* (b') a type-table key is a declared type symbol: every key elaboration
   makes is one (`bind T = τ;`, an output, a requirement of a context or a
   functor), never a fresh generation (`bind T = τ;` may bind a type symbol
   not in force, so not every key is in `context_gens`) *)
Definition TysScoped (C : cx) : Prop :=
  forall x, In x (map fst (c_ty C)) -> kind_sym W x = Some DKType.

Definition FormedCx (C : cx) : Prop := CxFormed C /\ KeysScoped C /\ TysScoped C.

Definition FrameFormed (F : frame) : Prop := OptOk W (fr_ret F) /\ OptOk W (fr_this F).

(* ---- scope facts ------------------------------------------------------------ *)

Lemma scope_sub : forall C g u y, In (g, u) (c_sub C) -> In y (gens u) -> In y (scope_gens C).
Proof.
  intros C g u y Iq Iy. unfold scope_gens. apply in_or_app. left. apply in_flat_map. exists (g, u). split; [exact Iq|right; exact Iy].
Qed.

Lemma scope_sub_key : forall C g u, In (g, u) (c_sub C) -> In g (scope_gens C).
Proof.
  intros C g u Iq. unfold scope_gens. apply in_or_app. left. apply in_flat_map. exists (g, u). split; [exact Iq|left; reflexivity].
Qed.

Lemma scope_ty : forall C x v y, In (x, v) (c_ty C) -> In y (gens v) -> In y (scope_gens C).
Proof.
  intros C x v y Iq Iy. unfold scope_gens. apply in_or_app. right. apply in_flat_map. exists (x, v).
  split; [exact Iq|apply in_or_app; left; exact Iy].
Qed.

Lemma scope_ty_canon : forall C x v y, In (x, v) (c_ty C) -> In y (gens (canon C v)) -> In y (scope_gens C).
Proof.
  intros C x v y Iq Iy. unfold scope_gens. apply in_or_app. right. apply in_flat_map. exists (x, v).
  split; [exact Iq|apply in_or_app; right; exact Iy].
Qed.

Lemma canon_scope : forall C t, incl (gens t) (scope_gens C) -> incl (gens (canon C t)) (scope_gens C).
Proof.
  intros C t Sc y Iy. unfold canon in Iy. destruct (gens_subst t (c_sub C) y Iy) as [z [Iz [[_ ->]|[u [L Iu]]]]];
    [exact (Sc z Iz)|exact (scope_sub C z u y (lookup_subst_in_pair _ _ _ L) Iu)].
Qed.

(* A meaning v read into a table whose entry is v under the merge: v's
   generations are in scope (merged ones as merge keys). *)
Lemma table_scope : forall C x v, In (x, app_subst (c_sub C) v) (c_ty C) -> incl (gens v) (scope_gens C).
Proof.
  intros C x v Iq y Iy. destruct (lookup_subst y (c_sub C)) as [u|] eqn:L.
  - exact (scope_sub_key C y u (lookup_subst_in_pair _ _ _ L)).
  - exact (scope_ty C x _ y Iq (gens_keep _ _ _ Iy L)).
Qed.

Lemma scope_gens_ext : forall C1 C2, c_ty C1 = c_ty C2 -> c_sub C1 = c_sub C2 -> scope_gens C1 = scope_gens C2.
Proof. intros C1 C2 E1 E2. unfold scope_gens, canon. rewrite E1, E2. reflexivity. Qed.

Lemma ty_scope : forall C g, In g (tm_gens (c_ty C)) -> In g (scope_gens C).
Proof. intros C g Ig. destruct (tm_gens_in _ g Ig) as [x [u [Iq Iu]]]. exact (scope_ty C x u g Iq Iu). Qed.

Lemma inscope_mono : forall C C' y, incl (scope_gens C) (scope_gens C') -> GenScoped C y -> GenScoped C' y.
Proof. intros C C' y Sc [I|K]; [left; exact (Sc y I)|right; exact K]. Qed.

Lemma scoped_of_incl : forall C t, incl (gens t) (scope_gens C) -> ScopedTy C t.
Proof. intros C t Sc y Iy. left. exact (Sc y Iy). Qed.

Lemma scoped_ext : forall C1 C2 t, scope_gens C1 = scope_gens C2 -> ScopedTy C1 t -> ScopedTy C2 t.
Proof. intros C1 C2 t E Sc y Iy. unfold GenScoped. rewrite <- E. exact (Sc y Iy). Qed.

Lemma canon_scoped : forall C t, ScopedTy C t -> ScopedTy C (canon C t).
Proof.
  intros C t Sc y Iy. unfold canon in Iy. destruct (gens_subst t (c_sub C) y Iy) as [z [Iz [[_ ->]|[u [L Iu]]]]];
    [exact (Sc z Iz)|left; exact (scope_sub C z u y (lookup_subst_in_pair _ _ _ L) Iu)].
Qed.

Lemma canon_ok : forall C t, CxFormed C -> TyOk W t -> TyOk W (canon C t).
Proof. intros C t Cf Ok. exact (tyok_subst W _ _ (cf_sub _ Cf) Ok). Qed.

(* ---- context steps ---------------------------------------------------------- *)

Lemma add_local_formed : forall C n t mu, FormedCx C -> TyOk W t -> FormedCx (add_local C n t mu).
Proof.
  intros C n t mu [[Ty Sub Pr Lo] [Ks Ts]] Ok. split; [|split; [exact Ks|exact Ts]].
  constructor; cbn; try assumption.
  intros n' t' mu' [E|Iq]; [injection E as -> -> ->; exact Ok|exact (Lo _ _ _ Iq)].
Qed.

Lemma add_locals_formed : forall mu l C, FormedCx C -> (forall n t, In (n, t) l -> TyOk W t) ->
  FormedCx (fold_left (fun C q => add_local C (fst q) (snd q) mu) l C).
Proof.
  intros mu l. induction l as [|[n t] r IH]; intros C Fc Ok; cbn; [exact Fc|].
  apply IH; [apply add_local_formed; [exact Fc|exact (Ok n t (or_introl eq_refl))]|].
  intros n' t' Iq. exact (Ok n' t' (or_intror Iq)).
Qed.

Lemma params_formed : forall C names ts, FormedCx C -> Forall (TyOk W) ts ->
  FormedCx (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet) (combine names ts) C).
Proof.
  intros C names ts Fc Ts. apply add_locals_formed; [exact Fc|]. intros n t Iq. rewrite Forall_forall in Ts.
  exact (Ts t (in_combine_r _ _ _ _ Iq)).
Qed.

Lemma scope_set_ty : forall C t v, incl (scope_gens C) (scope_gens (set_ty C t v)).
Proof.
  intros C t v y Iy. unfold scope_gens in *. cbn [set_ty c_sub c_ty]. apply in_app_or in Iy as [Iy|Iy]; apply in_or_app;
    [left; exact Iy|right]. cbn [flat_map]. apply in_or_app. right. exact Iy.
Qed.

Lemma scope_fold_set_ty : forall l C, incl (scope_gens C) (scope_gens (fold_left (fun C q => set_ty C (fst q) (snd q)) l C)).
Proof.
  induction l as [|[x v] r IH]; intros C; cbn [fold_left]; [intros y I; exact I|].
  intros y I. apply IH. exact (scope_set_ty C x v y I).
Qed.

Lemma scope_app_ty : forall C C' l, c_sub C' = c_sub C -> c_ty C' = l ++ c_ty C -> incl (scope_gens C) (scope_gens C').
Proof.
  intros C C' l Es Et y Iy. unfold scope_gens, canon in *. rewrite Es, Et. rewrite flat_map_app.
  apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; exact Iy|right; apply in_or_app; right; exact Iy].
Qed.

Lemma set_ty_cxformed : forall C t v, CxFormed C -> TyOk W v -> CxFormed (set_ty C t v).
Proof.
  intros C t v [Ty Sub Pr Lo] Ok. constructor; cbn; try assumption.
  intros x u [E|Iq]; [injection E as -> <-; exact Ok|exact (Ty x u Iq)].
Qed.

Lemma set_ty_keys : forall C t v, KeysScoped C -> KeysScoped (set_ty C t v).
Proof. intros C t v Ks r m th p Iq y Iy. exact (inscope_mono _ _ y (scope_set_ty C t v) (Ks r m th p Iq y Iy)). Qed.

Lemma set_ty_formed : forall C t v, FormedCx C -> TyOk W v -> kind_sym W t = Some DKType -> FormedCx (set_ty C t v).
Proof.
  intros C t v [Cf [Ks Ts]] Ok K. split; [exact (set_ty_cxformed _ _ _ Cf Ok)|split; [exact (set_ty_keys _ _ _ Ks)|]].
  intros x [E|Ix]; [subst x; exact K|exact (Ts x Ix)].
Qed.

Lemma fold_set_ty_shape : forall l C,
  c_ty (fold_left (fun C q => set_ty C (fst q) (snd q)) l C) = rev l ++ c_ty C /\
  c_sub (fold_left (fun C q => set_ty C (fst q) (snd q)) l C) = c_sub C /\
  c_provisions (fold_left (fun C q => set_ty C (fst q) (snd q)) l C) = c_provisions C /\
  c_locals (fold_left (fun C q => set_ty C (fst q) (snd q)) l C) = c_locals C.
Proof.
  induction l as [|[x v] r IH]; intros C; [repeat split|]. cbn [fold_left].
  destruct (IH (set_ty C (fst (x, v)) (snd (x, v)))) as [E1 [E2 [E3 E4]]]. rewrite E1, E2, E3, E4. cbn.
  rewrite <- app_assoc. repeat split.
Qed.

Lemma fold_set_ty_formed : forall l C, CxFormed C -> KeysScoped C -> TmOk W l ->
  CxFormed (fold_left (fun C q => set_ty C (fst q) (snd q)) l C) /\
  KeysScoped (fold_left (fun C q => set_ty C (fst q) (snd q)) l C).
Proof.
  induction l as [|[x v] r IH]; intros C Cf Ks Ok; [split; assumption|]. cbn [fold_left].
  apply IH; [exact (set_ty_cxformed _ _ _ Cf (Ok x v (or_introl eq_refl)))|exact (set_ty_keys _ _ _ Ks)|].
  intros y u Iq. exact (Ok y u (or_intror Iq)).
Qed.

Lemma set_provision_formed : forall C k p, FormedCx C -> KeyOk W k -> ProvOk W p ->
  (forall r m th, k = IMth r m th -> ScopedTy C r) -> FormedCx (set_provision C k p).
Proof.
  intros C k p [[Ty Sub Pr Lo] [Ks Ts]] Kk Pk Sk. split; [|split; [|exact Ts]].
  - constructor; cbn; try assumption. intros k' p' [E|Iq]; [injection E as <- <-; split; assumption|].
    apply filter_In in Iq as [Iq _]. exact (Pr _ _ Iq).
  - intros r m th p' [E|Iq].
    + injection E as E1 E2. exact (Sk r m th E1).
    + apply filter_In in Iq as [Iq _]. exact (Ks r m th p' Iq).
Qed.

Lemma provide_formed : forall C k a C1, FormedCx C1 -> InstOk W a -> FormedCx (provide C k a C1).
Proof.
  intros C k a C1 Fc Ia. unfold provide.
  destruct (ilook k (c_provisions C1)) as [p0|] eqn:L; [|exact Fc].
  apply ilook_in_key in L. pose proof Fc as [Cf [Ks Ts]].
  destruct (cf_prov _ Cf k p0 L) as [Kk [Sg _]].
  assert (Sk : forall r m th, k = IMth r m th -> ScopedTy C1 r) by (intros r m th ->; exact (Ks r m th p0 L)).
  destruct p0 as [t a0|g a0|g a0|fx]; try exact Fc; apply set_provision_formed; try assumption; split; assumption.
Qed.

Lemma set_fun_formed : forall C u fx, FormedCx C -> (forall x tx, In (x, tx) fx -> nlook x (c_ty C) = Some tx) ->
  FormedCx (set_fun C u fx).
Proof.
  intros C u fx Fc Fx. unfold set_fun. apply set_provision_formed; [exact Fc|exact Logic.I| |intros r m th E; discriminate].
  split; [intros g E; discriminate|]. intros x t Iq. cbn in Iq. apply in_map_iff in Iq as [[y v] [E Iq]].
  injection E as -> ->. exact (cf_ty _ (proj1 Fc) _ _ (nlook_in _ _ _ _ (Fx _ _ Iq))).
Qed.

Lemma set_val_formed : forall C v t, FormedCx C -> TyOk W t -> FormedCx (set_val C v t).
Proof.
  intros C v t Fc Ok. unfold set_val. apply set_provision_formed; [exact Fc|exact Logic.I| |intros r m th E; discriminate].
  split; [intros g E; injection E as <-; split; [constructor|exact Ok]|intros x t' []].
Qed.

Lemma set_fn_formed : forall C f g, FormedCx C -> SigOk W g -> FormedCx (set_fn C f g).
Proof.
  intros C f g Fc Ok. unfold set_fn. apply set_provision_formed; [exact Fc|exact Logic.I| |intros r m th E; discriminate].
  split; [intros g' E; injection E as <-; exact Ok|intros x t' []].
Qed.

Lemma set_mth_formed : forall C r m g, FormedCx C -> TyOk W r -> incl (gens r) (scope_gens C) -> SigOk W g ->
  FormedCx (set_mth C r m g).
Proof.
  intros C r m g Fc Or Sr Og. unfold set_mth. apply set_provision_formed; [exact Fc|exact Or| |].
  - split; [intros g' E; injection E as <-; exact Og|intros x t' []].
  - intros r' m' th E. injection E as <- _ _. exact (scoped_of_incl C r Sr).
Qed.

(* ---- what a context provides ------------------------------------------------- *)

Lemma vals_ok : forall C v t, CxFormed C -> nlook v (c_vals C) = Some t -> TyOk W t.
Proof.
  intros C v t Cf L. apply nlook_in in L. unfold c_vals in L. apply in_flat_map in L as [[k p] [Iq Ik]].
  destruct (cf_prov _ Cf k p Iq) as [_ [Sg _]].
  destruct k, p; cbn in Ik; try contradiction; destruct Ik as [E|[]]; injection E as -> ->.
  exact (proj2 (Sg _ eq_refl)).
Qed.

Lemma fns_ok : forall C f g, CxFormed C -> nlook f (c_fns C) = Some g -> SigOk W g.
Proof.
  intros C f g Cf L. apply nlook_in in L. unfold c_fns in L. apply in_flat_map in L as [[k p] [Iq Ik]].
  destruct (cf_prov _ Cf k p Iq) as [_ [Sg _]].
  destruct k, p; cbn in Ik; try contradiction; destruct Ik as [E|[]]; injection E as -> ->.
  exact (Sg _ eq_refl).
Qed.

Lemma mths_ok : forall C r m g, CxFormed C -> mlook r m (c_mths C) = Some g -> SigOk W g.
Proof.
  intros C r m g Cf L. destruct (mlook_in _ _ _ _ L) as [q L']. unfold c_mths in L'. apply in_flat_map in L' as [[k p] [Iq Ik]].
  destruct (cf_prov _ Cf k p Iq) as [_ [Sg _]].
  destruct k, p; cbn in Ik; try contradiction; destruct Ik as [E|[]]; injection E as _ E2; subst.
  exact (Sg _ eq_refl).
Qed.

Lemma literal_ok : forall C tr x t, Literal W C tr x t -> CxFormed C -> TyOk W t.
Proof.
  intros C tr x t L Cf.
  induction L as [z t Lz Found|o t Lo Found|a b x y r t m La IHa Lb IHb Lm Op|a b x y r t m La IHa Lb IHb Lm Op
                 |a b x y r t m La IHa Lb IHb Lm Op|a x ti tp t f La IHa Lf Found Fits].
  - exact (vals_ok _ _ _ Cf Found).
  - exact (vals_ok _ _ _ Cf Found).
  - exact IHa.
  - exact IHa.
  - exact IHa.
  - exact (proj2 (fns_ok _ _ _ Cf Found)).
Qed.

End Contexts.

(* ---- initial contexts ---------------------------------------------------------- *)

Section Initial.
Variable W : A.world.
Import A.

(* HYPOTHESIS (merge formation). [MostGeneralMerge] characterizes a merge
   only as a most general idempotent unifier over the telescope's
   generations; it does not say that the merge's values are formed. A most
   general unifier of formed entries should map each generation to a formed
   type (every node of a value is forced by some formed entry type).
   Proved from most generality alone in
   PublicWorldFormation.merge_formed_of. *)
Definition MergeFormed : Prop := forall s es, MostGeneralMerge s es -> Forall (EntryP (TyFormed W)) es ->
  forall g t, In (g, t) s -> TyFormed W t.

(* HYPOTHESIS (mentioned inputs are types). The implicit inputs of a
   functor, and the fixed inputs main's root context puts in force, are
   symbols its result mentions: type symbols, never tags or units. This
   follows from [WorldOf] (requirements are assumed type symbols, closed
   through relevance), via the least relevance-closed reading:
   PublicWorldFormation.mentions_typed_of. *)
Definition MentionsTyped : Prop := forall u t, Mentions W u t -> kind_sym W t = Some DKType.

Hypothesis merge_formed : MergeFormed.
Hypothesis mentions_typed : MentionsTyped.

Definition KeysAbstract (tm : list (nat * ty)) : Prop := forall x, In x (map fst tm) -> Abstract W x.

Lemma idform_ok : forall tm, IdForm tm -> KeysAbstract tm -> TmOk W tm.
Proof.
  intros tm Id Ka x u Iq. unfold IdForm in Id. rewrite Forall_forall in Id. pose proof (Id _ Iq) as E. cbn in E. subst u.
  apply tyok_abs. apply Ka. change x with (fst (x, TAbs x)). apply in_map, Iq.
Qed.

Lemma entry_ok_formed : forall e, EntryP (TyOk W) e -> EntryP (TyFormed W) e.
Proof.
  assert (Sig : forall g, SigP (TyOk W) g -> SigP (TyFormed W) g).
  { intros g [Ps R]. split; [exact (Forall_impl _ (fun t H => proj1 H) Ps)|exact (proj1 R)]. }
  assert (Ins : forall a, InstP (TyOk W) a -> InstP (TyFormed W) a) by (intros a H x t Iq; exact (proj1 (H x t Iq))).
  intros [t|v t a|f g a|r m g a|u fx]; cbn; try tauto.
  - intros [H1 H2]. exact (conj (proj1 H1) (Ins a H2)).
  - intros [H1 H2]. exact (conj (Sig g H1) (Ins a H2)).
  - intros [H1 [H2 H3]]. exact (conj (proj1 H1) (conj (Sig g H2) (Ins a H3))).
  - intros H x u0 Iq. exact (proj1 (H x u0 Iq)).
Qed.

Lemma entries_ok :
  (forall p ob tm sp es tm', ItemEntries W p ob tm sp es tm' ->
     TmOk W ob -> IdForm tm -> KeysAbstract tm -> IdForm tm' /\ KeysAbstract tm' /\ Forall (EntryP (TyOk W)) es) /\
  (forall p ob tm sps es tm', ItemsEntries W p ob tm sps es tm' ->
     TmOk W ob -> IdForm tm -> KeysAbstract tm -> IdForm tm' /\ KeysAbstract tm' /\ Forall (EntryP (TyOk W)) es).
Proof.
  apply (ies_mut W
    (fun p ob tm sp es tm' _ => TmOk W ob -> IdForm tm -> KeysAbstract tm ->
       IdForm tm' /\ KeysAbstract tm' /\ Forall (EntryP (TyOk W)) es)
    (fun p ob tm sps es tm' _ => TmOk W ob -> IdForm tm -> KeysAbstract tm ->
       IdForm tm' /\ KeysAbstract tm' /\ Forall (EntryP (TyOk W)) es)).
  - (* type *) intros p ob tm pa t R K Ob Id Ka. split; [|split; [|repeat constructor]].
    + destruct (nlook t tm); [exact Id|constructor; [reflexivity|exact Id]].
    + destruct (nlook t tm); [exact Ka|]. intros x [<-|Ix]; [exact (typed_abstract W _ K)|exact (Ka x Ix)].
  - (* nothing *) intros p ob tm pa s k R K Kind Ob Id Ka. split; [exact Id|split; [exact Ka|constructor]].
  - (* val *) intros p ob tm a pa s d n t ty0 bs ap R D Ast B T App Ob Id Ka.
    pose proof (tmok_app W _ _ Ob (idform_ok _ Id Ka)) as Tbl. pose proof (br_ok W _ _ _ _ _ B Tbl) as Bs.
    pose proof (tmok_app W _ _ Bs Tbl) as Full.
    split; [exact Id|split; [exact Ka|]]. constructor; [|constructor]. cbn.
    split; [exact (tyof_ok W _ _ _ _ _ T Full (optok_none W))|exact (appof_ok W _ _ _ App Full)].
  - (* fn *) intros p ob tm a pa s g bs ap R K B Sig App Ob Id Ka.
    pose proof (tmok_app W _ _ Ob (idform_ok _ Id Ka)) as Tbl. pose proof (br_ok W _ _ _ _ _ B Tbl) as Bs.
    pose proof (tmok_app W _ _ Bs Tbl) as Full.
    split; [exact Id|split; [exact Ka|]]. constructor; [|constructor]. cbn.
    split; [exact (sigof_ok W _ _ _ _ Sig Full (optok_none W))|exact (appof_ok W _ _ _ App Full)].
  - (* functor *) intros p ob tm pa u d n ps args res fx R D Ast Fixed FR Ob Id Ka.
    split; [exact Id|split; [exact Ka|]]. constructor; [|constructor]. cbn. intros x tx Iq.
    destruct (proj1 (FR x tx) Iq) as [_ L]. exact (idform_ok _ Id Ka _ _ (nlook_in _ _ _ _ L)).
  - (* method *) intros p ob tm a pa dn r m rt sg bs ap R MN B Recv Sig App Ob Id Ka.
    pose proof (tmok_app W _ _ Ob (idform_ok _ Id Ka)) as Tbl. pose proof (br_ok W _ _ _ _ _ B Tbl) as Bs.
    pose proof (tmok_app W _ _ Bs Tbl) as Full. pose proof (recv_ok W _ _ _ Recv Full) as Rt.
    split; [exact Id|split; [exact Ka|]]. constructor; [|constructor]. cbn.
    split; [exact Rt|split; [exact (sigof_ok W _ _ _ _ Sig Full (optok_some W _ Rt))|exact (appof_ok W _ _ _ App Full)]].
  - (* context *) intros p ob tm a pa c d n its bs es tm' R D Ast B Rest IH Ob Id Ka.
    pose proof (tmok_app W _ _ Ob (idform_ok _ Id Ka)) as Tbl. pose proof (br_ok W _ _ _ _ _ B Tbl) as Bs.
    exact (IH (tmok_app W _ _ Bs Ob) Id Ka).
  - intros p ob tm Ob Id Ka. split; [exact Id|split; [exact Ka|constructor]].
  - intros p ob tm sp r es1 tm1 es2 tm2 H1 IH1 H2 IH2 Ob Id Ka.
    destruct (IH1 Ob Id Ka) as [Id1 [Ka1 E1]]. destruct (IH2 Ob Id1 Ka1) as [Id2 [Ka2 E2]].
    split; [exact Id2|split; [exact Ka2|apply Forall_app; split; assumption]].
Qed.

Lemma add_tys_keys : forall xs tm x, In x (map fst (add_tys xs tm)) -> In x xs \/ In x (map fst tm).
Proof.
  unfold add_tys. induction xs as [|y rest IH]; intros tm x Ix; cbn in Ix; [right; exact Ix|].
  destruct (nlook y tm) eqn:F.
  - destruct (IH tm x Ix) as [H|H]; [left; right; exact H|right; exact H].
  - destruct (IH _ x Ix) as [H|[<-|H]]; [left; right; exact H|left; left; reflexivity|right; exact H].
Qed.

Lemma root_ok : forall q es0 tm its es tm', RootItems W q es0 tm its es tm' ->
  IdForm tm -> KeysAbstract tm -> IdForm tm' /\ KeysAbstract tm' /\ Forall (EntryP (TyOk W)) es.
Proof.
  intros q es0 tm its es tm' Root. induction Root as
    [es0 tm|es0 tm p a rest u du n ps args res xs fx es tm' R D Ast Fresh Nd Xs FR Rest IH|es0 tm sp rest es tm' Skip Rest IH];
    intros Id Ka.
  - split; [exact Id|split; [exact Ka|constructor]].
  - destruct (add_tys_formed xs tm Id) as [IdA _].
    assert (KaA : KeysAbstract (add_tys xs tm)).
    { intros x Ix. destruct (add_tys_keys xs tm x Ix) as [Ix'|Ix']; [|exact (Ka x Ix')].
      apply typed_abstract. apply (mentions_typed u). exact (proj1 (proj1 (Xs x) Ix')). }
    destruct (IH IdA KaA) as [Id' [Ka' Es']]. split; [exact Id'|split; [exact Ka'|]].
    apply Forall_app. split; [apply Forall_forall; intros e Ie; apply in_map_iff in Ie as [x [<- _]]; exact Logic.I|].
    constructor; [|exact Es']. cbn. intros x tx Ixt. destruct (proj1 (FR x tx) Ixt) as [_ L].
    exact (idform_ok _ IdA KaA _ _ (nlook_in _ _ _ _ L)).
  - exact (IH Id Ka).
Qed.

Lemma entry_provision_ok : forall s freeze e k p, TmOk W s -> (forall a, InstOk W a -> InstOk W (freeze a)) ->
  EntryP (TyOk W) e -> In (k, p) (entry_provision s freeze e) -> KeyOk W k /\ ProvOk W p.
Proof.
  intros s freeze [t|v t a|f g a|r m g a|u fx] k p Ss Fr Ok Iq; cbn in Iq; try contradiction;
    destruct Iq as [E|[]]; injection E as <- <-; cbn in Ok.
  - destruct Ok as [Ot Oa]. split; [exact Logic.I|]. split; [|exact (Fr a Oa)].
    intros g E; injection E as <-; split; [constructor|exact (tyok_subst W _ _ Ss Ot)].
  - destruct Ok as [Og Oa]. split; [exact Logic.I|]. split; [|exact (Fr a Oa)].
    intros g' E; injection E as <-; exact (sigok_subst W _ _ Ss Og).
  - destruct Ok as [Or [Og Oa]]. split; [exact (tyok_subst W _ _ Ss Or)|]. split; [|exact (Fr a Oa)].
    intros g' E; injection E as <-; exact (sigok_subst W _ _ Ss Og).
  - split; [exact Logic.I|]. split; [intros g E; discriminate|]. intros x t Ix. cbn in Ix. unfold fixed_subst in Ix.
    rewrite map_map in Ix. apply in_map_iff in Ix as [[y v] [E Iq]]. cbn in E. injection E as -> <-.
    exact (tyok_subst W _ _ Ss (Ok _ _ Iq)).
Qed.

Lemma entry_provision_key : forall s freeze e r m th p, In (IMth r m th, p) (entry_provision s freeze e) ->
  exists r0 g a, e = NMth r0 m g a /\ r = app_subst s r0.
Proof.
  intros s freeze [t|v t a|f g a|r0 m0 g a|u fx] r m th p Iq; cbn in Iq; try contradiction;
    destruct Iq as [E|[]]; try discriminate.
  injection E as E1 E2 _ _. subst. exists r0, g, a. split; reflexivity.
Qed.

Lemma idform_scope : forall σ tm es x, IdForm tm -> In x (map fst tm) -> In x (scope_gens (entries_cx σ tm es)).
Proof.
  intros σ tm es x Id Ix. apply in_map_iff in Ix as [[y v] [E Iq]]. cbn in E. subst y.
  unfold IdForm in Id. rewrite Forall_forall in Id. pose proof (Id _ Iq) as Ev. cbn in Ev. subst v.
  apply (table_scope (entries_cx σ tm es) x (TAbs x)); [|left; reflexivity].
  cbn [entries_cx c_ty c_sub]. apply in_map_iff. exists (x, TAbs x). split; [reflexivity|exact Iq].
Qed.

(* A telescope's type keys are declared type symbols. *)
Definition KeysDecl (tm : list (nat * ty)) : Prop := forall x, In x (map fst tm) -> kind_sym W x = Some DKType.

Lemma entries_keys_decl :
  (forall p ob tm sp es tm', ItemEntries W p ob tm sp es tm' -> KeysDecl tm -> KeysDecl tm') /\
  (forall p ob tm sps es tm', ItemsEntries W p ob tm sps es tm' -> KeysDecl tm -> KeysDecl tm').
Proof.
  apply (ies_mut W (fun p ob tm sp es tm' _ => KeysDecl tm -> KeysDecl tm')
                   (fun p ob tm sps es tm' _ => KeysDecl tm -> KeysDecl tm'));
    intros; try assumption.
  - destruct (nlook t tm); [assumption|]. intros x [<-|Ix]; [assumption|auto].
  - auto.
  - auto.
Qed.

Lemma root_keys_decl : forall q es0 tm its es tm', RootItems W q es0 tm its es tm' -> KeysDecl tm -> KeysDecl tm'.
Proof.
  intros q es0 tm its es tm' Root. induction Root as
    [es0 tm|es0 tm p a rest u du n ps args res xs fx es tm' R D Ast Fresh Nd Xs FR Rest IH|es0 tm sp rest es tm' Skip Rest IH];
    intros Kt.
  - exact Kt.
  - apply IH. intros x Ix. destruct (add_tys_keys xs tm x Ix) as [Ix'|Ix']; [|exact (Kt x Ix')].
    apply (mentions_typed u). exact (proj1 (proj1 (Xs x) Ix')).
  - exact (IH Kt).
Qed.

Lemma entries_cx_formed : forall σ tm es,
  IdForm tm -> KeysAbstract tm -> KeysDecl tm -> Forall (EntryP (TyOk W)) es ->
  (forall e, In e es -> incl (entry_gens e) (map fst tm)) ->
  (forall g t, In (g, t) σ -> TyFormed W t /\ incl (gens t) (map fst tm)) ->
  FormedCx W (entries_cx σ tm es).
Proof.
  intros σ tm es Id Ka Kd Es Eg Sg.
  assert (So : TmOk W σ).
  { intros g t Iq. destruct (Sg g t Iq) as [Ft Gt]. split; [exact Ft|]. intros y Iy. exact (Ka y (Gt y Iy)). }
  pose proof (idform_ok _ Id Ka) as Tm.
  assert (Fr : forall a, InstOk W a -> InstOk W (map (fun q => (fst q, option_map (app_subst σ)
                 (match snd q with Some t => Some t | None => nlook (fst q) tm end))) a)).
  { intros a Ia x t Iq. apply in_map_iff in Iq as [[y o] [E Iq]]. cbn in E. injection E as -> E.
    destruct o as [t0|]; cbn in E.
    - injection E as <-. exact (tyok_subst W _ _ So (Ia _ _ Iq)).
    - destruct (nlook x tm) as [t0|] eqn:L; cbn in E; [|discriminate]. injection E as <-.
      exact (tyok_subst W _ _ So (Tm _ _ (nlook_in _ _ _ _ L))). }
  rewrite Forall_forall in Es. split; [|split].
  - constructor; cbn.
    + intros x u Iq. apply in_map_iff in Iq as [[y v] [E Iq]]. cbn in E. injection E as -> <-.
      exact (tyok_subst W _ _ So (Tm _ _ Iq)).
    + exact So.
    + intros k p Iq. apply in_flat_map in Iq as [e [Ie Iq]].
      exact (entry_provision_ok _ _ e k p So Fr (Es e Ie) Iq).
    + intros n t mu [].
  - intros r m th p Iq. cbn in Iq. apply in_flat_map in Iq as [e [Ie Iq]].
    destruct (entry_provision_key _ _ _ _ _ _ _ Iq) as [r0 [g [a [-> ->]]]].
    assert (Sc : incl (gens r0) (scope_gens (entries_cx σ tm es))).
    { intros y Iy. apply idform_scope; [exact Id|]. apply (Eg _ Ie). cbn. apply in_or_app. left. exact Iy. }
    exact (scoped_of_incl W _ _ (canon_scope (entries_cx σ tm es) r0 Sc)).
  - intros x Ix. cbn in Ix. rewrite map_map in Ix. cbn in Ix. exact (Kd x Ix).
Qed.

(* The context a fn or method's assumes form is formed. *)
Theorem ctxof_cx_formed : forall f C ns, CtxOf W f C ns -> FormedCx W C.
Proof.
  intros f C ns (d & es & tm & er & tmr & σ & D & IE & Root & MGM & -> & _ & _).
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT [_ EsT]].
  destruct (proj2 entries_ok _ _ _ _ _ _ IE (tmok_nil W) ltac:(constructor) ltac:(intros x [])) as [_ [KaT OkT]].
  assert (Tmr : IdForm tmr /\ incl (map fst tm) (map fst tmr) /\ Scoped tmr (flat_map entry_gens er) /\
                KeysAbstract tmr /\ Forall (EntryP (TyOk W)) er).
  { destruct Root as [_|gc d0 n0 its er' tmr' _ _ _ _ RI].
    - split; [exact IdT|split; [intros x Ix; exact Ix|split; [intros g []|split; [exact KaT|constructor]]]].
    - destruct (root_formed W _ _ _ _ _ _ RI IdT) as [A1 [A2 A3]]. destruct (root_ok _ _ _ _ _ _ RI IdT KaT) as [_ [B2 B3]].
      split; [exact A1|split; [exact A2|split; [exact A3|split; [exact B2|exact B3]]]]. }
  destruct Tmr as [IdR [SubR [ErR [KaR OkR]]]].
  assert (All : Scoped tmr (flat_map entry_gens (es ++ er))).
  { intros x Ix. rewrite flat_map_app in Ix. apply in_app_or in Ix as [Ix|Ix]; [exact (SubR x (EsT x Ix))|exact (ErR x Ix)]. }
  assert (Ok : Forall (EntryP (TyOk W)) (es ++ er)) by (apply Forall_app; split; assumption).
  assert (KdR : KeysDecl tmr).
  { pose proof (proj2 entries_keys_decl _ _ _ _ _ _ IE (fun y (Iy : In y []) => match Iy with end)) as Kt.
    destruct Root as [_|gc d0 n0 its er' tmr' _ _ _ _ RI]; [exact Kt|exact (root_keys_decl _ _ _ _ _ _ RI Kt)]. }
  apply entries_cx_formed; [exact IdR|exact KaR|exact KdR|exact Ok| |].
  - intros e Ie y Iy. apply All. apply in_flat_map. exists e. split; assumption.
  - intros g t Iq. split.
    + exact (merge_formed σ (es ++ er) MGM (Forall_impl _ entry_ok_formed Ok) g t Iq).
    + destruct MGM as [_ [_ [_ [Dom _]]]]. intros y Iy. exact (All y (proj2 (Dom g t Iq) y Iy)).
Qed.

(* The context a functor's argument telescope forms is formed. *)
Theorem functx_cx_formed : forall u C ns, FunCtx W u C ns -> FormedCx W C.
Proof.
  intros u C ns (d & ps & a & r & b & es & tm & tm' & σ & implicit & D & FP & IE & Imp & -> & MGM & -> & _ & _).
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT [_ EsT]].
  destruct (proj2 entries_ok _ _ _ _ _ _ IE (tmok_nil W) ltac:(constructor) ltac:(intros x [])) as [_ [KaT OkT]].
  assert (Id' : IdForm (map (fun t => (t, TAbs t)) implicit ++ tm)).
  { apply Forall_app. split; [|exact IdT]. apply Forall_forall. intros q Iq. apply in_map_iff in Iq as [t [<- _]]. reflexivity. }
  assert (Ka' : KeysAbstract (map (fun t => (t, TAbs t)) implicit ++ tm)).
  { intros x Ix. rewrite map_app, map_map in Ix. cbn in Ix. rewrite map_id in Ix. apply in_app_or in Ix as [Ix|Ix]; [|exact (KaT x Ix)].
    apply typed_abstract. apply (mentions_typed u). exact (proj1 (proj2 (proj1 (Imp x) Ix))). }
  assert (Sub : forall x, In x (map fst tm) -> In x (map fst (map (fun t => (t, TAbs t)) implicit ++ tm))).
  { intros x Ix. rewrite map_app. apply in_or_app. right. exact Ix. }
  assert (Kd' : KeysDecl (map (fun t => (t, TAbs t)) implicit ++ tm)).
  { intros x Ix. rewrite map_app, map_map in Ix. cbn in Ix. rewrite map_id in Ix. apply in_app_or in Ix as [Ix|Ix].
    - exact (mentions_typed u x (proj1 (proj2 (proj1 (Imp x) Ix)))).
    - exact (proj2 entries_keys_decl _ _ _ _ _ _ IE (fun y (Iy : In y []) => match Iy with end) x Ix). }
  apply entries_cx_formed; [exact Id'|exact Ka'|exact Kd'|exact OkT| |].
  - intros e Ie y Iy. apply Sub. apply EsT. apply in_flat_map. exists e. split; assumption.
  - intros g t Iq. split.
    + exact (merge_formed σ es MGM (Forall_impl _ entry_ok_formed OkT) g t Iq).
    + destruct MGM as [_ [_ [_ [Dom _]]]]. intros y Iy. exact (Sub y (EsT y (proj2 (Dom g t Iq) y Iy))).
Qed.

End Initial.

(* ---- functor results, families and demand ----------------------------------- *)

Section Results.
Variable W : A.world.
Import A.

Lemma obr_ok : forall p tm a bs, OutputBrackets W p tm a bs -> TmOk W tm -> TmOk W bs /\ incl (tm_gens bs) (tm_gens tm).
Proof.
  intros p tm a bs B Tm. destruct B as [|bs0 l F]; [split; [intros x u []|intros g []]|].
  assert (H : TmOk W l /\ incl (tm_gens l) (tm_gens tm)).
  { induction F as [|b tb bs' l' (sp & sa & E1 & R1 & K1 & T1) Fr IH]; [split; [intros x u []|intros g []]|].
    destruct IH as [Ok Sc]. destruct tb as [y v]. cbn in T1. split.
    - intros x u [E|Iq]; [injection E as -> <-; exact (tyof_ok W _ _ _ _ _ T1 Tm (optok_none W))|exact (Ok _ _ Iq)].
    - intros g Ig. cbn in Ig. apply in_app_or in Ig as [Ig|Ig]; [|exact (Sc g Ig)].
      exact (tyof_none_gens W _ _ _ _ T1 g Ig). }
  destruct H as [Ok Sc]. split.
  - intros x u Iq. apply (proj2 (in_rev _ _)) in Iq. exact (Ok _ _ Iq).
  - intros g Ig. apply Sc. destruct (tm_gens_in _ g Ig) as [x [u [Iq Iu]]].
    apply (proj2 (in_rev _ _)) in Iq. exact (in_tm_gens _ _ _ _ Iq Iu).
Qed.

Definition GensIn (K : nat -> Prop) (l : list nat) : Prop := forall y, In y l -> K y.

Lemma subst_within : forall (K : nat -> Prop) s t,
  (forall g u, In (g, u) s -> GensIn K (gens u)) -> GensIn K (gens t) -> GensIn K (gens (app_subst s t)).
Proof.
  intros K s t Range Sc x Ix. destruct (gens_subst t s x Ix) as [y [Iy [[_ ->]|[u [L Iu]]]]]; [exact (Sc y Iy)|].
  exact (Range y u (lookup_subst_in_pair _ _ _ L) x Iu).
Qed.

Lemma output_entries_ok : forall (K : nat -> Prop),
  (forall p ob tm sp es tm', OutputItemEntries W p ob tm sp es tm' ->
     TmOk W ob -> TmOk W tm -> GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) ->
     tm' = tm /\ Forall (EntryP (TyOk W)) es /\ (forall r m g a, In (NMth r m g a) es -> GensIn K (gens r))) /\
  (forall p ob tm sps es tm', OutputItemsEntries W p ob tm sps es tm' ->
     TmOk W ob -> TmOk W tm -> GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) ->
     tm' = tm /\ Forall (EntryP (TyOk W)) es /\ (forall r m g a, In (NMth r m g a) es -> GensIn K (gens r))).
Proof.
  intros K. apply (oies_mut W
    (fun p ob tm sp es tm' _ => TmOk W ob -> TmOk W tm -> GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) ->
       tm' = tm /\ Forall (EntryP (TyOk W)) es /\ (forall r m g a, In (NMth r m g a) es -> GensIn K (gens r)))
    (fun p ob tm sps es tm' _ => TmOk W ob -> TmOk W tm -> GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) ->
       tm' = tm /\ Forall (EntryP (TyOk W)) es /\ (forall r m g a, In (NMth r m g a) es -> GensIn K (gens r)))).
  - (* type *) intros p ob tm pa a bs t R Kd B Ob Tm SOb STm.
    split; [reflexivity|split; [repeat constructor|intros r m g a' [E|[]]; discriminate]].
  - (* nothing *) intros p ob tm pa a bs s k R Kd B Kind Ob Tm SOb STm.
    split; [reflexivity|split; [constructor|intros r m g a' []]].
  - (* val *) intros p ob tm a pa s d n t ty0 bs ap R D Ast B T App Ob Tm SOb STm.
    pose proof (tmok_app W _ _ Ob Tm) as Tbl. destruct (obr_ok _ _ _ _ B Tbl) as [Bs _].
    pose proof (tmok_app W _ _ Bs Tbl) as Full.
    split; [reflexivity|split; [|intros r m g a' [E|[]]; discriminate]]. constructor; [|constructor]. cbn.
    split; [exact (tyof_ok W _ _ _ _ _ T Full (optok_none W))|exact (appof_ok W _ _ _ App Full)].
  - (* fn *) intros p ob tm a pa s g bs ap d rcv n ps rt R Kd D Ast B Sig App Ob Tm SOb STm.
    pose proof (tmok_app W _ _ Ob Tm) as Tbl. destruct (obr_ok _ _ _ _ B Tbl) as [Bs _].
    pose proof (tmok_app W _ _ Bs Tbl) as Full.
    split; [reflexivity|split; [|intros r m g' a' [E|[]]; discriminate]]. constructor; [|constructor]. cbn.
    split; [exact (sigof_ok W _ _ _ _ Sig Full (optok_none W))|exact (appof_ok W _ _ _ App Full)].
  - (* functor *) intros p ob tm a bs pa u d n ps args res fx R D Ast B Fixd FR Ob Tm SOb STm.
    pose proof (tmok_app W _ _ Ob Tm) as Tbl. destruct (obr_ok _ _ _ _ B Tbl) as [Bs _].
    pose proof (tmok_app W _ _ Bs Tbl) as Full.
    split; [reflexivity|split; [|intros r m g a' [E|[]]; discriminate]]. constructor; [|constructor]. cbn.
    intros x tx Iq. destruct (proj1 (FR x tx) Iq) as [_ L]. exact (Full _ _ (nlook_in _ _ _ _ L)).
  - (* method *) intros p ob tm a pa dn r m rt g bs ap R MN B Recv Sig App Ob Tm SOb STm.
    pose proof (tmok_app W _ _ Ob Tm) as Tbl. destruct (obr_ok _ _ _ _ B Tbl) as [Bs SBs].
    pose proof (tmok_app W _ _ Bs Tbl) as Full. pose proof (recv_ok W _ _ _ Recv Full) as Rt.
    split; [reflexivity|split].
    + constructor; [|constructor]. cbn.
      split; [exact Rt|split; [exact (sigof_ok W _ _ _ _ Sig Full (optok_some W _ Rt))|exact (appof_ok W _ _ _ App Full)]].
    + intros r' m' g' a' [E|[]]. injection E as -> _ _ _. intros y Iy.
      pose proof (recv_gens W _ _ _ Recv y Iy) as Iy'. apply tm_gens_app in Iy' as [Iy'|Iy'].
      * apply SBs in Iy'. apply tm_gens_app in Iy' as [Iy'|Iy']; [exact (SOb y Iy')|exact (STm y Iy')].
      * apply tm_gens_app in Iy' as [Iy'|Iy']; [exact (SOb y Iy')|exact (STm y Iy')].
  - (* context *) intros p ob tm a pa c d n its bs es tm' R D Ast B Rest IH Ob Tm SOb STm.
    pose proof (tmok_app W _ _ Ob Tm) as Tbl. destruct (obr_ok _ _ _ _ B Tbl) as [Bs SBs].
    apply IH; [exact (tmok_app W _ _ Bs Ob)|exact Tm| |exact STm].
    intros y Iy. apply tm_gens_app in Iy as [Iy|Iy]; [|exact (SOb y Iy)].
    apply SBs in Iy. apply tm_gens_app in Iy as [Iy|Iy]; [exact (SOb y Iy)|exact (STm y Iy)].
  - intros p ob tm Ob Tm SOb STm. split; [reflexivity|split; [constructor|intros r m g a []]].
  - intros p ob tm sp rest es1 tm1 es2 tm2 H1 IH1 H2 IH2 Ob Tm SOb STm.
    destruct (IH1 Ob Tm SOb STm) as [-> [E1 M1]]. destruct (IH2 Ob Tm SOb STm) as [-> [E2 M2]].
    split; [reflexivity|split; [apply Forall_app; split; assumption|]].
    intros r m g a Iq. apply in_app_or in Iq as [Iq|Iq]; [exact (M1 _ _ _ _ Iq)|exact (M2 _ _ _ _ Iq)].
Qed.

Lemma output_entries_tm :
  (forall p ob tm sp es tm', OutputItemEntries W p ob tm sp es tm' -> tm' = tm) /\
  (forall p ob tm sps es tm', OutputItemsEntries W p ob tm sps es tm' -> tm' = tm).
Proof.
  apply (oies_mut W (fun p ob tm sp es tm' _ => tm' = tm) (fun p ob tm sps es tm' _ => tm' = tm)); intros; try reflexivity.
  - assumption.
  - subst. reflexivity.
Qed.

Lemma provide_result_shape : forall u C0 out C' ks, ProvideResult W u C0 out C' ks ->
  c_sub C' = c_sub C0 /\ c_locals C' = c_locals C0 /\
  exists outs, (forall t, In t outs <-> FunOutputs W u t) /\
    c_ty C' = map (fun q => (fst q, canon C0 (snd q))) (output_type_map C0 outs out).
Proof.
  intros u C0 out C' ks (d & ps & a & r & b & outs & es & tm' & D & FP & Outs & OIE & RK & ->).
  rewrite (proj2 output_entries_tm _ _ _ _ _ _ OIE).
  split; [reflexivity|split; [reflexivity|]]. exists outs. split; [exact Outs|reflexivity].
Qed.

(* A functor's result read in context C0 (each output type t as `out t`):
   every new provision is ok, and its key mentions only generations K. *)
Lemma provide_result_ok : forall u C0 out C' ks (K : nat -> Prop),
  ProvideResult W u C0 out C' ks ->
  TmOk W (c_sub C0) -> (forall g v, In (g, v) (c_sub C0) -> GensIn K (gens v)) ->
  (forall x v, In (x, v) (c_ty C0) -> TyOk W (canon C0 v) /\ GensIn K (gens (canon C0 v))) ->
  (forall t, FunOutputs W u t -> TyOk W (canon C0 (out t)) /\ GensIn K (gens (canon C0 (out t)))) ->
  c_sub C' = c_sub C0 /\ c_locals C' = c_locals C0 /\
  (exists outs, (forall t, In t outs <-> FunOutputs W u t) /\
     c_ty C' = map (fun q => (fst q, canon C0 (snd q))) (output_type_map C0 outs out)) /\
  (forall k p, In (k, p) (c_provisions C') -> In (k, p) (c_provisions C0) \/
     (KeyOk W k /\ ProvOk W p /\ forall r m th, k = IMth r m th -> GensIn K (gens r))).
Proof.
  intros u C0 out C' ks K (d & ps & a & r & b & outs & es & tm' & D & FP & Outs & OIE & RK & ->) Sub SubK Ty Out.
  assert (T0 : TmOk W (output_type_map C0 outs out) /\ GensIn K (tm_gens (output_type_map C0 outs out))).
  { assert (Each : forall x v, In (x, v) (output_type_map C0 outs out) -> TyOk W v /\ GensIn K (gens v)).
    { intros x v Iq. unfold output_type_map in Iq. apply in_map_iff in Iq as [[y w] [E Iq]]. cbn in E. injection E as -> <-.
      apply in_app_or in Iq as [Iq|Iq].
      - apply in_map_iff in Iq as [t [E It]]. injection E as <- <-. exact (Out t (proj1 (Outs t) It)).
      - exact (Ty _ _ Iq). }
    split; [intros x v Iq; exact (proj1 (Each x v Iq))|].
    intros g Ig. destruct (tm_gens_in _ g Ig) as [x [v [Iq Iv]]]. exact (proj2 (Each x v Iq) g Iv). }
  destruct T0 as [Ok0 Sc0].
  destruct (proj2 (output_entries_ok K) _ _ _ _ _ _ OIE (tmok_nil W) Ok0 ltac:(intros g []) Sc0) as [-> [Es Ms]].
  split; [reflexivity|split; [reflexivity|split; [exists outs; split; [exact Outs|reflexivity]|]]].
  intros k p Iq. cbn in Iq. apply in_app_or in Iq as [Iq|Iq]; [right|left; exact Iq].
  apply in_flat_map in Iq as [e [Ie Iq]]. apply (proj2 (in_rev _ _)) in Ie. rewrite Forall_forall in Es.
  destruct (entry_provision_ok W _ _ e k p Sub (fun a Ia => Ia) (Es e Ie) Iq) as [Kk Pk].
  split; [exact Kk|split; [exact Pk|]]. intros r0 m th ->.
  destruct (entry_provision_key _ _ _ _ _ _ _ Iq) as [r1 [g [a1 [-> ->]]]].
  exact (subst_within K _ r1 SubK (Ms _ _ _ _ Ie)).
Qed.

Lemma spec_types_typed : forall p sp t, SpecTypes W p sp t -> kind_sym W t = Some DKType.
Proof. intros p sp t ST. induction ST; assumption. Qed.

Lemma outputs_typed : forall u t, FunOutputs W u t -> kind_sym W t = Some DKType.
Proof. intros u t (d & ps & a & r & b & sp & D & FP & Isp & ST). exact (spec_types_typed _ _ _ ST). Qed.

Lemma family_result_spec : forall mi sp t, FamilyResult W mi sp -> SpecTypes W mi sp t -> False.
Proof.
  intros mi sp t FR ST. revert FR. induction ST as [p pa a t R K|p pa a c d n its sp t R D Ast Isp ST IH]; intros FR.
  - inversion FR as [|mi' pa' br' u d' n' its' R' D' Ast' Fa]; subst.
    rewrite R in R'. injection R' as <-. unfold kind_sym in K. rewrite D', Ast' in K. discriminate.
  - inversion FR as [|mi' pa' br' u d' n' its' R' D' Ast' Fa]; subst.
    rewrite R in R'. injection R' as <-. rewrite D in D'. injection D' as <-. rewrite Ast in Ast'.
    injection Ast' as <- <-. rewrite Forall_forall in Fa. exact (IH (Fa sp Isp)).
Qed.

Lemma family_no_outputs : forall u t, FamilyShape W u -> FunOutputs W u t -> False.
Proof.
  intros u t (d & n & ps & a & r & D & Ast & Fr & _) (d' & ps' & a' & r' & b' & sp & D' & FP & Isp & ST).
  rewrite D in D'. injection D' as <-. rewrite Ast in FP. cbn in FP. injection FP as _ _ <- _.
  rewrite Forall_forall in Fr. exact (family_result_spec _ _ _ (Fr sp Isp) ST).
Qed.

Lemma family_context_shape : forall m C,
  c_ty (family_context C m) = rev m ++ c_ty C /\ c_sub (family_context C m) = c_sub C /\
  c_provisions (family_context C m) = c_provisions C /\ c_locals (family_context C m) = c_locals C.
Proof. intros m C. exact (fold_set_ty_shape m C). Qed.

(* A family applied in C, with every matching value ok and in C's scope,
   adds only ok provisions keyed in C's scope. *)
Lemma family_application_ok : forall C u matching th sm Cr ks,
  FamilyApplication W C u matching th sm Cr ks -> CxFormed W C -> KeysScoped W C ->
  (forall x v, In (x, v) matching -> TyOk W (canon C v) /\ ScopedTy W C (canon C v)) ->
  c_sub Cr = c_sub C /\ c_locals Cr = c_locals C /\
  (forall k p, In (k, p) (c_provisions Cr) -> KeyOk W k /\ ProvOk W p /\
     forall r m th, k = IMth r m th -> ScopedTy W C r).
Proof.
  intros C u matching th sm Cr ks (fixed & formal & needs & Fx & Fam & Poly & FC & Args & InF & FA & Sat & PR & Nd) Cf Ks Mt.
  destruct (family_context_shape matching C) as [Ety [Esub [Epr Elo]]].
  assert (P1 : TmOk W (c_sub (family_context C matching))) by (rewrite Esub; exact (cf_sub _ _ Cf)).
  assert (P2 : forall g v, In (g, v) (c_sub (family_context C matching)) -> GensIn (GenScoped W C) (gens v)).
  { intros g v Iq y Iy. rewrite Esub in Iq. left. exact (scope_sub C g v y Iq Iy). }
  assert (P3 : forall x v, In (x, v) (c_ty (family_context C matching)) ->
     TyOk W (canon (family_context C matching) v) /\ GensIn (GenScoped W C) (gens (canon (family_context C matching) v))).
  { intros x v Iq. rewrite Ety in Iq. unfold canon. rewrite Esub. apply in_app_or in Iq as [Iq|Iq].
    - apply (proj2 (in_rev _ _)) in Iq. exact (Mt x v Iq).
    - split; [exact (canon_ok W C v Cf (cf_ty _ _ Cf _ _ Iq))|]. intros y Iy. left. exact (scope_ty_canon C x v y Iq Iy). }
  assert (P4 : forall t, FunOutputs W u t -> TyOk W (canon (family_context C matching) (TAbs t)) /\
     GensIn (GenScoped W C) (gens (canon (family_context C matching) (TAbs t)))).
  { intros t Ft. exfalso. exact (family_no_outputs u t (proj1 Fam) Ft). }
  destruct (provide_result_ok _ _ _ _ _ _ PR P1 P2 P3 P4) as [Sub' [Lo' [_ Pr']]].
  rewrite Sub', Esub, Lo', Elo. split; [reflexivity|split; [reflexivity|]].
  intros k p Iq. destruct (Pr' k p Iq) as [Iq'|H]; [rewrite Epr in Iq'|exact H].
  destruct (cf_prov _ _ Cf k p Iq') as [Kk Pk]. split; [exact Kk|split; [exact Pk|]].
  intros r m th' ->. exact (Ks r m th' p Iq').
Qed.

Lemma family_application_sub : forall C u matching th sm Cr ks,
  FamilyApplication W C u matching th sm Cr ks -> c_sub Cr = c_sub C.
Proof.
  intros C u matching th sm Cr ks (fixed & formal & needs & Fx & Fam & Poly & FC & Args & InF & FA & Sat & PR & Nd).
  destruct PR as (d & ps & a & r & b & outs & es & tm' & D & FP & Outs & OIE & RK & ->). cbn.
  exact (proj1 (proj2 (family_context_shape matching C))).
Qed.

(* Demand keeps the type table and the merge. *)
Lemma demand_shape : forall C0 wanted used C wrap Cf, Demand W C0 wanted used C wrap Cf ->
  c_ty Cf = c_ty C /\ c_sub Cf = c_sub C.
Proof.
  intros C0 wanted used C wrap Cf Dm. induction Dm as [used C|used C u matching th sm rs Cm wrap Cf Nin Step Rest IH];
    [split; reflexivity|].
  destruct IH as [E1 E2]. rewrite E1, E2. destruct Step as [C1 ot m u1 matching1 th1 sm1 Cr ks Iw Ml Of FAp Ro Ik].
  cbn. split; [reflexivity|exact (family_application_sub _ _ _ _ _ _ _ FAp)].
Qed.

(* A matching read off a key ([FamilyReadOff]): each value is part of the
   key's receiver, so it is ok and in scope when the receiver is. *)
Fixpoint aty_ind (P : ty -> Prop) (Hm : forall m, P (TMachine m))
  (Hr : forall fs, (forall f u, In (f, u) fs -> P u) -> P (TRecord fs)) (Hu : P TUnit)
  (Ha : forall g, P (TAbs g)) (Hn : forall n xs, (forall x u, In (x, u) xs -> P u) -> P (TNom n xs))
  (Hs : forall ms, (forall u, In u ms -> P u) -> P (TUnion ms)) (t : ty) {struct t} : P t.
Proof.
  destruct t as [m|fs| |g|n xs|ms].
  - apply Hm.
  - apply Hr. induction fs as [|[f0 u0] r Tail]; intros f u I; [destruct I|].
    destruct I as [E|I]; [injection E as _ <-; exact (aty_ind P Hm Hr Hu Ha Hn Hs u0)|exact (Tail f u I)].
  - apply Hu.
  - apply Ha.
  - apply Hn. induction xs as [|[x0 u0] r Tail]; intros x u I; [destruct I|].
    destruct I as [E|I]; [injection E as _ <-; exact (aty_ind P Hm Hr Hu Ha Hn Hs u0)|exact (Tail x u I)].
  - apply Hs. induction ms as [|u0 r Tail]; intros u I; [destruct I|].
    destruct I as [<-|I]; [exact (aty_ind P Hm Hr Hu Ha Hn Hs u0)|exact (Tail u I)].
Qed.

Inductive SubTy : ty -> ty -> Prop :=
| SubRefl : forall t, SubTy t t
| SubRec : forall a fs f u, In (f, u) fs -> SubTy a u -> SubTy a (TRecord fs)
| SubNom : forall a n xs x u, In (x, u) xs -> SubTy a u -> SubTy a (TNom n xs)
| SubUnion : forall a ms u, In u ms -> SubTy a u -> SubTy a (TUnion ms).

Lemma gens_record_flat : forall fs, gens (TRecord fs) = flat_map (fun p => gens (snd p)) fs.
Proof. induction fs as [|[f u] r IH]; cbn; [reflexivity|f_equal; exact IH]. Qed.
Lemma gens_nom_flat : forall n xs, gens (TNom n xs) = flat_map (fun p => gens (snd p)) xs.
Proof. intros n; induction xs as [|[x u] r IH]; cbn; [reflexivity|f_equal; exact IH]. Qed.
Lemma gens_union_flat : forall ms, gens (TUnion ms) = flat_map gens ms.
Proof. induction ms as [|u r IH]; cbn; [reflexivity|f_equal; exact IH]. Qed.

Lemma sub_read : forall rf s x v, In x (gens rf) -> lookup_subst x s = Some v -> SubTy v (app_subst s rf).
Proof.
  intros rf. induction rf as [m|fs IH| |g|n xs IH|ms IH] using aty_ind; intros s x v Ix L.
  - contradiction.
  - rewrite gens_record_flat in Ix. apply in_flat_map in Ix as [[f u] [Iu Ix]].
    rewrite subst_record. apply (SubRec _ _ f (app_subst s u)); [|exact (IH f u Iu s x v Ix L)].
    apply in_map_iff. exists (f, u). split; [reflexivity|exact Iu].
  - contradiction.
  - destruct Ix as [<-|[]]. cbn. rewrite L. constructor.
  - rewrite gens_nom_flat in Ix. apply in_flat_map in Ix as [[y u] [Iu Ix]].
    rewrite subst_nom. apply (SubNom _ _ _ y (app_subst s u)); [|exact (IH y u Iu s x v Ix L)].
    unfold PublicTypeRead.smap. apply in_map_iff. exists (y, u). split; [reflexivity|exact Iu].
  - rewrite gens_union_flat in Ix. apply in_flat_map in Ix as [u [Iu Ix]].
    rewrite subst_union. apply (SubUnion _ _ (app_subst s u)); [|exact (IH u Iu s x v Ix L)].
    apply in_map. exact Iu.
Qed.

Lemma sub_subst : forall c a b, SubTy a b -> SubTy (app_subst c a) (app_subst c b).
Proof.
  intros c a b Sb. induction Sb as [t|a fs f u I _ IH|a n xs x u I _ IH|a ms u I _ IH].
  - constructor.
  - rewrite subst_record. apply (SubRec _ _ f (app_subst c u)); [|exact IH].
    apply in_map_iff. exists (f, u). split; [reflexivity|exact I].
  - rewrite subst_nom. apply (SubNom _ _ _ x (app_subst c u)); [|exact IH].
    unfold PublicTypeRead.smap. apply in_map_iff. exists (x, u). split; [reflexivity|exact I].
  - rewrite subst_union. apply (SubUnion _ _ (app_subst c u)); [|exact IH]. apply in_map. exact I.
Qed.

Lemma sub_ok : forall a b, SubTy a b -> TyOk W b -> TyOk W a /\ incl (gens a) (gens b).
Proof.
  intros a b Sb. induction Sb as [t|a fs f u I _ IH|a n xs x u I _ IH|a ms u I _ IH]; intros [Fb Ab].
  - split; [split; assumption|intros g Ig; exact Ig].
  - assert (Iu : incl (gens u) (gens (TRecord fs))).
    { intros g Ig. rewrite gens_record_flat. apply in_flat_map. exists (f, u). split; assumption. }
    destruct IH as [Oa Ia]; [split; [exact (proj1 (record_formed W fs) Fb f u I)|intros g Ig; exact (Ab g (Iu g Ig))]|].
    split; [exact Oa|intros g Ig; exact (Iu g (Ia g Ig))].
  - assert (Iu : incl (gens u) (gens (TNom n xs))).
    { intros g Ig. rewrite gens_nom_flat. apply in_flat_map. exists (x, u). split; assumption. }
    destruct IH as [Oa Ia]; [split; [exact (proj2 (proj1 (nom_formed W n xs) Fb) x u I)|intros g Ig; exact (Ab g (Iu g Ig))]|].
    split; [exact Oa|intros g Ig; exact (Iu g (Ia g Ig))].
  - assert (Iu : incl (gens u) (gens (TUnion ms))).
    { intros g Ig. rewrite gens_union_flat. apply in_flat_map. exists u. split; assumption. }
    destruct (proj1 (union_formed W ms) Fb) as [_ [_ Fm]]. rewrite Forall_forall in Fm.
    destruct IH as [Oa Ia]; [split; [exact (Fm u I)|intros g Ig; exact (Ab g (Iu g Ig))]|].
    split; [exact Oa|intros g Ig; exact (Iu g (Ia g Ig))].
Qed.

Lemma lookup_subst_nodup : forall (s : subst) x v, NoDup (map fst s) -> In (x, v) s -> lookup_subst x s = Some v.
Proof.
  induction s as [|[y u] r IH]; intros x v Nd I; [contradiction|]. cbn in Nd |- *. inversion Nd as [|? ? Ny Nr]; subst.
  destruct I as [E|I].
  - injection E as -> ->. rewrite Nat.eqb_refl. reflexivity.
  - destruct (Nat.eqb_spec x y) as [->|_]; [exfalso; apply Ny; apply in_map_iff; exists (y, v); split; [reflexivity|exact I]|].
    exact (IH x v Nr I).
Qed.

(* Family matchings read off an ok, scoped receiver are ok and scoped. *)
Lemma family_matching_scoped : forall C u ot m matching,
  FamilyReadOff W C u ot m matching -> TyOk W ot -> ScopedTy W C ot ->
  forall x v, In (x, v) matching -> TyOk W (canon C v) /\ ScopedTy W C (canon C v).
Proof.
  intros C u ot m matching (marks & fr & fks & rf & a & Marks & PR & Ik & Dom & Nd & Ot) Ok Sc x v Iq.
  assert (Ix : In x (gens rf)) by (apply (proj2 (proj1 (Dom x) (in_map fst _ _ Iq)))).
  pose proof (sub_subst (c_sub C) _ _ (sub_read rf matching x v Ix (lookup_subst_nodup matching x v Nd Iq))) as Sb.
  change (SubTy (canon C v) (canon C (app_subst matching rf))) in Sb. rewrite Ot in Sb.
  destruct (sub_ok _ _ Sb Ok) as [Ov Iv]. split; [exact Ov|intros g Ig; exact (Sc g (Iv g Ig))].
Qed.

Lemma demand_formed : forall C0 wanted used C wrap Cf,
  Demand W C0 wanted used C wrap Cf -> CxFormed W C -> KeysScoped W C ->
  (forall r m, In (mkey r m) wanted -> TyOk W r /\ ScopedTy W C r) ->
  CxFormed W Cf /\ KeysScoped W Cf.
Proof.
  intros C0 wanted used C wrap Cf Dm. induction Dm as [used C|used C u matching th sm rs Cm wrap Cf Nin Step Rest IH];
    intros Cf0 Ks Wk; [split; assumption|].
  destruct Step as [C1 ot m u1 matching1 th1 sm1 Cr ks Iw Ml Of FAp Ro Ik].
  destruct (Wk ot m Iw) as [Ok Sco].
  destruct (family_application_ok _ _ _ _ _ _ _ FAp Cf0 Ks (family_matching_scoped _ _ _ _ _ Ro Ok Sco)) as [Sub [Lo Pr]].
  assert (Sc : scope_gens (family_restore C1 Cr) = scope_gens C1) by (apply scope_gens_ext; [reflexivity|exact Sub]).
  apply IH.
  - constructor.
    + exact (cf_ty _ _ Cf0).
    + change (TmOk W (c_sub Cr)). rewrite Sub. exact (cf_sub _ _ Cf0).
    + intros k p Iq. destruct (Pr k p Iq) as [Kk [Pk _]]. split; assumption.
    + change (forall n t mu, In (n, (t, mu)) (c_locals Cr) -> TyOk W t). rewrite Lo. exact (cf_locals _ _ Cf0).
  - intros r m' th' p Iq. apply (scoped_ext W _ _ _ (eq_sym Sc)). exact (proj2 (proj2 (Pr _ _ Iq)) r m' th' eq_refl).
  - intros r m' Iw'. destruct (Wk r m' Iw') as [O1 S1]. split; [exact O1|exact (scoped_ext W _ _ _ (eq_sym Sc) S1)].
Qed.

Lemma demand_tys : forall C0 wanted used C wrap Cf, Demand W C0 wanted used C wrap Cf -> TysScoped W C -> TysScoped W Cf.
Proof.
  intros C0 wanted used C wrap Cf Dm Ts x Ix. destruct (demand_shape _ _ _ _ _ _ Dm) as [E1 E2].
  rewrite E1 in Ix. exact (Ts x Ix).
Qed.

(* `bind F;` of an abstract F in force: the result context is formed. *)
Lemma apply_abstract_formed : forall C u gensof C' ks,
  FormedCx W C ->
  (forall t, FunOutputs W u t -> gensof t = t \/ Fresh W (gensof t)) ->
  ProvideResult W u C (fun t => TAbs (gensof t)) C' ks -> FormedCx W C'.
Proof.
  intros C u gensof C' ks [Cf [Ks Ts]] Outs PR.
  destruct (provide_result_shape _ _ _ _ _ PR) as [Esub [Elo [outs [OutsI Ety]]]].
  assert (InTy : forall x v, In (x, v) (output_type_map C outs (fun t => TAbs (gensof t))) ->
    In (x, app_subst (c_sub C') v) (c_ty C')).
  { intros x v Iq. rewrite Ety, Esub. apply in_map_iff. exists (x, v). split; [reflexivity|exact Iq]. }
  assert (OldIn : forall x v, In (x, v) (c_ty C) -> In (x, canon C v) (output_type_map C outs (fun t => TAbs (gensof t)))).
  { intros x v Iq. unfold output_type_map. apply in_map_iff. exists (x, v). split; [reflexivity|apply in_or_app; right; exact Iq]. }
  assert (OutIn : forall t, In t outs -> In (t, canon C (TAbs (gensof t))) (output_type_map C outs (fun t => TAbs (gensof t)))).
  { intros t It. unfold output_type_map. apply in_map_iff. exists (t, TAbs (gensof t)). split; [reflexivity|].
    apply in_or_app; left. apply in_map_iff. exists t. split; [reflexivity|exact It]. }
  assert (OutAbs : forall t, FunOutputs W u t -> Abstract W (gensof t)).
  { intros t Ft. destruct (Outs t Ft) as [E|Fr]; [rewrite E; exact (typed_abstract W _ (outputs_typed _ _ Ft))|exact (fresh_abstract W _ Fr)]. }
  assert (SubK : forall g v, In (g, v) (c_sub C) -> GensIn (fun y => In y (scope_gens C')) (gens v)).
  { intros g v Iq y Iy. rewrite <- Esub in Iq. exact (scope_sub C' g v y Iq Iy). }
  assert (TyK : forall x v, In (x, v) (c_ty C) -> TyOk W (canon C v) /\ GensIn (fun y => In y (scope_gens C')) (gens (canon C v))).
  { intros x v Iq. split; [exact (canon_ok W C v Cf (cf_ty _ _ Cf _ _ Iq))|].
    exact (table_scope C' x (canon C v) (InTy _ _ (OldIn _ _ Iq))). }
  assert (OutK : forall t, FunOutputs W u t ->
     TyOk W (canon C (TAbs (gensof t))) /\ GensIn (fun y => In y (scope_gens C')) (gens (canon C (TAbs (gensof t))))).
  { intros t Ft. split; [exact (canon_ok W C _ Cf (tyok_abs W _ (OutAbs t Ft)))|].
    exact (table_scope C' t _ (InTy _ _ (OutIn t (proj2 (OutsI t) Ft)))). }
  destruct (provide_result_ok _ _ _ _ _ (fun y => In y (scope_gens C')) PR (cf_sub _ _ Cf) SubK TyK OutK) as [_ [_ [_ Pr]]].
  assert (Scope : incl (scope_gens C) (scope_gens C')).
  { intros y Iy. unfold scope_gens in Iy. apply in_app_or in Iy as [Iy|Iy].
    - apply in_flat_map in Iy as [[g v] [Iq Iy]]. destruct Iy as [E|Iy].
      + cbn in E. subst y. rewrite <- Esub in Iq. exact (scope_sub_key C' _ _ Iq).
      + exact (SubK g v Iq y Iy).
    - apply in_flat_map in Iy as [[x v] [Iq Iy]]. cbn in Iy. apply in_app_or in Iy as [Iy|Iy].
      + destruct (lookup_subst y (c_sub C)) as [w|] eqn:L.
        * rewrite <- Esub in L. exact (scope_sub_key C' y w (lookup_subst_in_pair _ _ _ L)).
        * exact (proj2 (TyK x v Iq) y (gens_keep _ _ _ Iy L)).
      + exact (proj2 (TyK x v Iq) y Iy). }
  split; [|split].
  - constructor.
    + intros x w Iq. rewrite Ety in Iq. apply in_map_iff in Iq as [[y v] [E Iq]]. cbn in E. injection E as -> <-.
      unfold output_type_map in Iq. apply in_map_iff in Iq as [[z v0] [E Iq]]. cbn in E. injection E as -> <-.
      apply (canon_ok W C _ Cf). apply (canon_ok W C _ Cf). apply in_app_or in Iq as [Iq|Iq].
      * apply in_map_iff in Iq as [t [E It]]. injection E as <- <-. exact (tyok_abs W _ (OutAbs t (proj1 (OutsI t) It))).
      * exact (cf_ty _ _ Cf _ _ Iq).
    + change (TmOk W (c_sub C')). rewrite Esub. exact (cf_sub _ _ Cf).
    + intros k p Iq. destruct (Pr k p Iq) as [Iq'|[Kk [Pk _]]]; [exact (cf_prov _ _ Cf k p Iq')|split; assumption].
    + change (forall n t mu, In (n, (t, mu)) (c_locals C') -> TyOk W t). rewrite Elo. exact (cf_locals _ _ Cf).
  - intros r m th p Iq. destruct (Pr _ _ Iq) as [Iq'|[_ [_ S]]]; [|intros y Iy; left; exact (S r m th eq_refl y Iy)].
    intros y Iy. exact (inscope_mono W _ _ y Scope (Ks r m th p Iq' y Iy)).
  - intros x Ix. apply in_map_iff in Ix as [[x' w] [E Iq]]. cbn in E. subst x'.
    rewrite Ety in Iq. apply in_map_iff in Iq as [[y v] [E Iq]]. cbn in E. injection E as -> _.
    unfold output_type_map in Iq. apply in_map_iff in Iq as [[z v0] [E Iq]]. cbn in E. injection E as -> _.
    apply in_app_or in Iq as [Iq|Iq].
    + apply in_map_iff in Iq as [t [E It]]. injection E as <- _. exact (outputs_typed _ _ (proj1 (OutsI t) It)).
    + assert (Ix : In x (map fst (c_ty C))) by (change x with (fst (x, v0)); apply in_map, Iq).
      exact (Ts x Ix).
Qed.

End Results.

(* ---- elaboration preserves formation ------------------------------------------- *)

Section Elab.
Variable W : A.world.
Variable Prog : program.
Import A.

(* HYPOTHESIS (bare units). A unit has no requirements: `TNom s []` for a
   unit s is formed. This is [wo_reqs_none] of [WorldOf] (units_bare_of). *)
Definition UnitsBare : Prop := forall s, kind_sym W s = Some DKUnit -> w_reqs W s = [].

(* Family signatures, as first stated: a family applied to supply a method
   at an ok receiver supplies it at an ok signature. False as stated (see
   below); kept for reference, unused. *)
Definition FamilySigOk : Prop := forall C u matching th sm Cr ks ot m g,
  FamilyApplication W C u matching th sm Cr ks -> FormedCx W C -> TyOk W ot ->
  In (mkey ot m) ks -> mlook ot m (c_mths Cr) = Some g -> SigOk W g.

(* [FamilySigOk] is false as stated: FamilyApplication constrains the
   matching only to P(F) symbols, and a family whose result is a bracketed
   context leaf can carry a matching value into a signature its key does
   not determine (PublicFamilySig.v has the accepted program). The family
   case of [ex_ok] has the read-off of the matching from the demanded key
   (FamilyReadOff) in hand, and under it the signature is ok
   (PublicFamilySig.family_sig_ok_readoff), so that is what is assumed. *)
Definition FamilySigReadOffOk : Prop := forall C u matching th sm Cr ks ot m g,
  FamilyApplication W C u matching th sm Cr ks -> FamilyReadOff W C u ot m matching ->
  FormedCx W C -> TyOk W ot ->
  In (mkey ot m) ks -> mlook ot m (c_mths Cr) = Some g -> SigOk W g.

Hypothesis units_bare : UnitsBare.
Hypothesis family_sig_ok : FamilySigReadOffOk.

Lemma units_bare_of : forall b, WorldOf b W -> UnitsBare.
Proof. intros b Wo s K. apply (@wo_reqs_none b W Wo). right. exact K. Qed.

Lemma tmok_ctx_table : forall C bs, FormedCx W C -> TmOk W bs -> TmOk W (bs ++ c_ty C).
Proof. intros C bs Fc Bs. exact (tmok_app W _ _ Bs (cf_ty _ _ (proj1 Fc))). Qed.

Fixpoint ex_ok F C a exp x t (D : Ex W Prog F C a exp x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> OptOk W exp -> TyOk W t
with branch_ok F C a exp x t (D : Branch W Prog F C a exp x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> OptOk W exp -> TyOk W t
with arms_ok F C st arms exp xarms t (D : Arms W Prog F C st arms exp xarms t) {struct D} :
  FormedCx W C -> FrameFormed W F -> TyOk W st -> OptOk W exp -> TyOk W t
with armslist_ok F C st arms exp xarms hs ca t (D : ArmsList W Prog F C st arms exp xarms hs ca t) {struct D} :
  FormedCx W C -> FrameFormed W F -> TyOk W st -> OptOk W exp -> TyOk W t
with methcall_ok F C ox ot pa args x t (D : MethCall W Prog F C ox ot pa args x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> TyOk W ot -> TyOk W t
with provided_ok F C ox ot m args x t (D : Provided W Prog F C ox ot m args x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> TyOk W ot -> TyOk W t
with blke_ok F C b exp x t (D : BlkE W Prog F C b exp x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> OptOk W exp -> TyOk W t
with blk_ok F C ss tl exp x t made (D : Blk W Prog F C ss tl exp x t made) {struct D} :
  FormedCx W C -> FrameFormed W F -> OptOk W exp -> TyOk W t
with binditems_ok F C its wrap C' F' made (D : BindItems W Prog F C its wrap C' F' made) {struct D} :
  FormedCx W C -> FrameFormed W F -> FormedCx W C' /\ FrameFormed W F'
with binditem_ok F C it wrap C' F' made (D : BindItem W Prog F C it wrap C' F' made) {struct D} :
  FormedCx W C -> FrameFormed W F -> FormedCx W C' /\ FrameFormed W F'.
Proof.
  (* ---- Ex ---- *)
  - destruct D as [F C exp|F C exp t Th|F C exp n t mu L Nm|F C exp pa s Hl R K|F C exp pa s d n t0 t Hl R D' Ast L
                  |F C exp pa s d n t0 i t x Hl R D' Ast As T Ex0|F C exp pa a args s bs xs pay y v ty R K B Fx Pay Ea Ey Fi
                  |F C exp pa args f Cf ns sm th g xs wrap C' R K Def Ctx Dm Sat CT Sig As
                  |F C exp pa args f g xs R K Abs L As|F C exp o pa args ox ot x t Eo Mc
                  |F C exp op y lib n m ox ot x t Op Ls Ey Hd Pv|F C exp op l r lib n m ox ot x t Op Ls El Hd Pv
                  |F C exp text x t Nt Lt|F C exp c b cx' bx bt Cd Bb Of Bt|F C exp c b y cx' bx bt ex' et t Cd Bb Br Of J
                  |F C exp s arms sx st xarms t Es Ar|F C exp b x t Bb|F C exp Rt|F C exp e expected v actual Rt Ee Fi
                  |F C exp Lp|F C exp pa a fs s bs xs pay ftys xfs R K B Fx Pay Ep Nd Len F2
                  |F C exp o n ox ot rec t Eo Rc Ft];
      intros Fc Ff Oe.
    + exact (tyok_unit W).
    + exact (proj2 Ff t Th).
    + exact (cf_locals _ _ (proj1 Fc) n t mu (slook_in _ _ _ _ L)).
    + apply nom_ok_intro; [cbn; symmetry; exact (units_bare s K)|exact (tmok_nil W)].
    + exact (vals_ok W _ _ _ (proj1 Fc) L).
    + exact (tyof_ok W _ _ _ _ _ T (tmok_nil W) (optok_none W)).
    + destruct (reqs_ok W _ _ _ Fx (tmok_ctx_table C bs Fc (br_ok W _ _ _ _ _ B (cf_ty _ _ (proj1 Fc))))) as [Kx Ox].
      exact (nom_ok_intro W s xs Kx Ox).
    + destruct (demand_shape W _ _ _ _ _ _ Dm) as [Ety _].
      assert (Tm : TmOk W (c_ty C')) by (rewrite Ety; exact (cf_ty _ _ (proj1 Fc))).
      exact (proj2 (sigof_ok W _ _ _ _ Sig Tm (optok_none W))).
    + exact (proj2 (fns_ok W _ _ _ (proj1 Fc) L)).
    + pose proof (ex_ok _ _ _ _ _ _ Eo Fc Ff (optok_none W)) as Ot.
      exact (methcall_ok _ _ _ _ _ _ _ _ Mc Fc Ff (canon_ok W C ot (proj1 Fc) Ot)).
    + pose proof (ex_ok _ _ _ _ _ _ Ey Fc Ff (optok_none W)) as Ot.
      exact (provided_ok _ _ _ _ _ _ _ _ Pv Fc Ff (canon_ok W C ot (proj1 Fc) Ot)).
    + pose proof (ex_ok _ _ _ _ _ _ El Fc Ff (optok_none W)) as Ot.
      exact (provided_ok _ _ _ _ _ _ _ _ Pv Fc Ff (canon_ok W C ot (proj1 Fc) Ot)).
    + exact (literal_ok W _ _ _ _ Lt (proj1 Fc)).
    + exact (tyok_unit W).
    + pose proof (blke_ok _ _ _ _ _ _ Bb Fc Ff Oe) as Hb. pose proof (branch_ok _ _ _ _ _ _ Br Fc Ff Oe) as He.
      exact (join_ok W _ _ _ _ J (branch_ty_ok W b bt Hb) He Oe).
    + pose proof (ex_ok _ _ _ _ _ _ Es Fc Ff (optok_none W)) as Hs.
      exact (arms_ok _ _ _ _ _ _ _ Ar Fc Ff (canon_ok W C st (proj1 Fc) Hs) Oe).
    + destruct exp as [e|]; [exact (Oe e eq_refl)|exact (blke_ok _ _ _ _ _ _ Bb Fc Ff Oe)].
    + exact (never_ok W).
    + exact (never_ok W).
    + exact (never_ok W).
    + destruct (reqs_ok W _ _ _ Fx (tmok_ctx_table C bs Fc (br_ok W _ _ _ _ _ B (cf_ty _ _ (proj1 Fc))))) as [Kx Ox].
      exact (nom_ok_intro W s xs Kx Ox).
    + pose proof (ex_ok _ _ _ _ _ _ Eo Fc Ff (optok_none W)) as Ot.
      assert (Rok : TyOk W rec).
      { destruct Rc as [(s & xs & -> & Kt & Pay)| ->]; [|exact Ot].
        exact (payload_ok W _ _ _ Pay (proj2 (nom_ok_elim W _ _ Ot))). }
      destruct Ft as [ftys [-> S]]. exact (record_ok_elim W _ _ _ Rok (slook_in _ _ _ _ S)).
  (* ---- Branch ---- *)
  - destruct D as [F C b exp x t Bb|F C y exp x t Nb Ey]; intros Fc Ff Oe.
    + destruct exp as [e|]; [exact (Oe e eq_refl)|exact (branch_ty_ok W b t (blke_ok _ _ _ _ _ _ Bb Fc Ff Oe))].
    + exact (ex_ok _ _ _ _ _ _ Ey Fc Ff Oe).
  (* ---- Arms ---- *)
  - destruct D as [F C exp|F C st arms exp xarms covered catch t Nn Al Exh]; intros Fc Ff Os Oe.
    + destruct exp as [e|]; [exact (Oe e eq_refl)|exact (never_ok W)].
    + exact (armslist_ok _ _ _ _ _ _ _ _ _ Al Fc Ff Os Oe).
  (* ---- ArmsList ---- *)
  - destruct D as [F C st pat body exp xp binds hs ca bx bt Pt Br Of
                  |F C st pat body rest exp xp binds hs ca bx bt xr hr cr tr t Ne Pt Br Of Al J]; intros Fc Ff Os Oe.
    + exact (branch_ok _ _ _ _ _ _ Br (add_locals_formed W LLet binds C Fc (patof_ok W _ _ _ _ _ _ _ Pt Os)) Ff Oe).
    + pose proof (branch_ok _ _ _ _ _ _ Br (add_locals_formed W LLet binds C Fc (patof_ok W _ _ _ _ _ _ _ Pt Os)) Ff Oe) as Hb.
      pose proof (armslist_ok _ _ _ _ _ _ _ _ _ Al Fc Ff Os Oe) as Hr.
      exact (join_ok W _ _ _ _ J Hb Hr Oe).
  (* ---- MethCall ---- *)
  - destruct D as [F C ox ot pa args m x t Len Rd Pv|F C ox ot n args h att m x t Hd At Sc Ml Pv
                  |F C ox ot n args h att Ca ns sm th g xs rargs wrap Cd Hd At Nw Def Ra Ctx Dm Sat CT Sig As
                  |F C ox ot n args h att x t Hd At Nw Abs Pv|F C ox ot n args m x t Na Sc Pv]; intros Fc Ff Ot.
    + exact (provided_ok _ _ _ _ _ _ _ _ Pv Fc Ff Ot).
    + exact (provided_ok _ _ _ _ _ _ _ _ Pv Fc Ff Ot).
    + destruct (demand_shape W _ _ _ _ _ _ Dm) as [Ety _].
      destruct (fold_set_ty_shape rargs C) as [Ety' _].
      assert (Ra' : TmOk W rargs).
      { subst rargs. destruct ot; try exact (tmok_nil W). exact (proj2 (nom_ok_elim W _ _ Ot)). }
      assert (Tm : TmOk W (c_ty Cd)) by (rewrite Ety, Ety'; exact (tmok_app W _ _ (fun x u Iq => Ra' x u (proj2 (in_rev _ _) Iq)) (cf_ty _ _ (proj1 Fc)))).
      exact (proj2 (sigof_ok W _ _ _ _ Sig Tm (optok_some W _ Ot))).
    + exact (provided_ok _ _ _ _ _ _ _ _ Pv Fc Ff Ot).
    + exact (provided_ok _ _ _ _ _ _ _ _ Pv Fc Ff Ot).
  (* ---- Provided ---- *)
  - destruct D as [F C ox ot m args g xs Ml As|F C ox ot m args u matching th sm C' ks g xs Ml FAp Ro Ik Of Ml' As];
      intros Fc Ff Ot.
    + exact (proj2 (mths_ok W _ _ _ _ (proj1 Fc) Ml)).
    + exact (proj2 (family_sig_ok _ _ _ _ _ _ _ ot m g FAp Ro Fc Ot Ik Ml')).
  (* ---- BlkE ---- *)
  - destruct D as [F C ss tl exp x t made Bk Esc Of]; intros Fc Ff Oe.
    exact (blk_ok _ _ _ _ _ _ _ _ Bk Fc Ff Oe).
  (* ---- Blk ---- *)
  - destruct D as [F C y exp x t Ey|F C exp
                  |F C st n ann y rest tl exp declared v vt t0 xr t made mu Ls Ann Ey Dc Nm Rest
                  |F C n y rest tl exp tv v vt xr t made L Ey Fi Rest Nm|F C y exp v vt Ey
                  |F C y rest tl exp v vt xr t made Ne Ey Rest|F C c b rest tl exp cx' bx bt xr t made Cd Bb Rest
                  |F C b rest tl exp bx bt xr t made Ne Bb Rest|F C b exp bx bt Bb Nb
                  |F C its rest tl exp wrap C' F' xr t made made' Bi Rest]; intros Fc Ff Oe.
    + exact (ex_ok _ _ _ _ _ _ Ey Fc Ff Oe).
    + exact (tyok_unit W).
    + assert (Od : OptOk W declared).
      { destruct ann as [a|].
        - destruct Ann as [d [T ->]]. apply optok_some. exact (tyof_ok W _ _ _ _ _ T (cf_ty _ _ (proj1 Fc)) (proj2 Ff)).
        - rewrite Ann. exact (optok_none W). }
      pose proof (ex_ok _ _ _ _ _ _ Ey Fc Ff Od) as Hvt.
      assert (Ht0 : TyOk W t0).
      { destruct declared as [d|]; [destruct Dc as [_ ->]; exact (Od d eq_refl)|rewrite Dc; exact Hvt]. }
      exact (blk_ok _ _ _ _ _ _ _ _ Rest (add_local_formed W C n t0 mu Fc Ht0) Ff Oe).
    + exact (blk_ok _ _ _ _ _ _ _ _ Rest Fc Ff Oe).
    + destruct (ty_eqb vt never); [exact (never_ok W)|exact (tyok_unit W)].
    + exact (blk_ok _ _ _ _ _ _ _ _ Rest Fc Ff Oe).
    + exact (blk_ok _ _ _ _ _ _ _ _ Rest Fc Ff Oe).
    + exact (blk_ok _ _ _ _ _ _ _ _ Rest Fc Ff Oe).
    + exact (never_ok W).
    + destruct (binditems_ok _ _ _ _ _ _ _ Bi Fc Ff) as [Fc' Ff'].
      exact (blk_ok _ _ _ _ _ _ _ _ Rest Fc' Ff' Oe).
  (* ---- BindItems ---- *)
  - destruct D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 B1 B2]; intros Fc Ff; [split; assumption|].
    destruct (binditem_ok _ _ _ _ _ _ _ B1 Fc Ff) as [Fc1 Ff1]. exact (binditems_ok _ _ _ _ _ _ _ B2 Fc1 Ff1).
  (* ---- BindItem ---- *)
  - destruct D as [F C pa t rp ra tau R K Np T
                  |F C pa v d n t0 i declared y x xt a R D' Ast T Ey Fi App
                  |F C pa f g sg pg sm a types R K Sig FP App
                  |F C pa f sg ps rt body pid pre sm a R K Sig LP App
                  |F C pa dn a y r m bs rt sg target pre sm ap types R MN B T Sig MP App NoFam
                  |F C pa u d n ps a r bodies wrap C' F' made R D' Ast Na Bi
                  |F C pa u fx Cu ns sm th gensof C' ks newgens outs R Fx FC In' Th Fix Sat FF Inj Outs Nd OutsI PR
                  |F C pa u pg h Cu nsu Ch nsh sm fx
                  |F C pa u pg gg Cu nsu Cg nsg h sm smc fx]; intros Fc Ff.
    + pose proof (tyof_ok W _ _ _ _ _ T (cf_ty _ _ (proj1 Fc)) (optok_none W)) as Htau.
      exact (conj (set_ty_formed W C t (canon C tau) Fc (canon_ok W C tau (proj1 Fc) Htau) K) Ff).
    + pose proof (tyof_ok W _ _ _ _ _ T (cf_ty _ _ (proj1 Fc)) (optok_none W)) as Hd.
      split; [|exact Ff]. apply provide_formed; [exact (set_val_formed W C v declared Fc Hd)|].
      exact (appof_ok W _ _ _ App (cf_ty _ _ (proj1 Fc))).
    + pose proof (sigof_ok W _ _ _ _ Sig (cf_ty _ _ (proj1 Fc)) (optok_none W)) as Hs.
      split; [|exact Ff]. apply provide_formed; [exact (set_fn_formed W C f sg Fc Hs)|].
      exact (appof_ok W _ _ _ App (cf_ty _ _ (proj1 Fc))).
    + pose proof (sigof_ok W _ _ _ _ Sig (cf_ty _ _ (proj1 Fc)) (optok_none W)) as Hs.
      split; [|exact Ff]. apply provide_formed; [exact (set_fn_formed W C f sg Fc Hs)|].
      exact (appof_ok W _ _ _ App (cf_ty _ _ (proj1 Fc))).
    + pose proof (br_ok W _ _ _ _ _ B (cf_ty _ _ (proj1 Fc))) as Bs.
      pose proof (tmok_ctx_table C bs Fc Bs) as Full.
      pose proof (tyof_ok W _ _ _ _ _ T Full (optok_none W)) as Hrt.
      pose proof (sigof_ok W _ _ _ _ Sig Full (optok_some W _ Hrt)) as Hsg.
      assert (Sc : incl (gens rt) (scope_gens C)).
      { intros y' Iy. pose proof (tyof_none_gens W _ _ _ _ T y' Iy) as Iy'. apply ty_scope.
        apply tm_gens_app in Iy' as [Iy'|Iy']; [exact (br_gens W _ _ _ _ _ B y' Iy')|exact Iy']. }
      split; [|exact Ff]. apply provide_formed; [|exact (appof_ok W _ _ _ App Full)].
      exact (set_mth_formed W C (canon C rt) m sg Fc (canon_ok W C rt (proj1 Fc) Hrt) (canon_scope C rt Sc) Hsg).
    + destruct (binditems_ok _ _ _ _ _ _ _ Bi Fc Ff) as [Fc' _]. exact (conj Fc' Ff).
    + split; [|exact Ff].
      apply (apply_abstract_formed W C u gensof C' ks Fc); [|exact PR].
      intros t Ft. destruct (Outs t Ft) as [[_ [E _]]|[_ [Fr _]]]; [left; exact E|right; exact Fr].
    + match goal with Fx : forall x tx, In (x, tx) fx <-> _ |- _ =>
        split; [|exact Ff]; apply set_fun_formed; [exact Fc|intros x tx Iq; exact (proj2 (proj1 (Fx x tx) Iq))] end.
    + match goal with Fx : forall x tx, In (x, tx) fx <-> _ |- _ =>
        split; [|exact Ff]; apply set_fun_formed; [exact Fc|intros x tx Iq; exact (proj2 (proj1 (Fx x tx) Iq))] end.
Qed.

End Elab.

(* ---- inner contexts and corollaries --------------------------------------------- *)

Section Corollaries.
Variable W : A.world.
Variable Prog : program.
Import A.

(* A lambda provider's own context (LambdaProvider's Cl) is formed. *)
Lemma lambda_cx_formed : forall C caps capvals, FormedCx W C ->
  FormedCx W (fold_left (fun C' q => let '(n, v) := q in
                    match slook n (c_locals C) with
                    | Some (t, _) => add_local (provide C' (IVal v) [] (set_val C' v t)) n t LCaptured
                    | None => C' end)
                 (combine caps capvals)
                 (Cx (c_ty C) (c_sub C) (c_provisions C) [])).
Proof.
  intros C caps capvals Fc.
  assert (Base : FormedCx W (Cx (c_ty C) (c_sub C) (c_provisions C) [])).
  { destruct Fc as [[Ty Sub Pr Lo] [Ks Ts]]. split; [constructor; cbn; try assumption; intros n t mu []|split; assumption]. }
  generalize (combine caps capvals) as l. intros l. revert Base.
  generalize (Cx (c_ty C) (c_sub C) (c_provisions C) []) as C0.
  induction l as [|[n v] r IH]; intros C0 F0; cbn; [exact F0|]. apply IH.
  destruct (slook n (c_locals C)) as [[t mu]|] eqn:S; [|exact F0].
  pose proof (cf_locals _ _ (proj1 Fc) n t mu (slook_in _ _ _ _ S)) as Ot.
  apply add_local_formed; [|exact Ot]. apply provide_formed; [|intros x t' []].
  exact (set_val_formed W C0 v t F0 Ot).
Qed.

(* A fn or method body's context: the assumes' context with the parameters. *)
Lemma fn_body_formed : forall f C ns this g names,
  MergeFormed W -> MentionsTyped W ->
  CtxOf W f C ns -> SigOf W f (c_ty C) this g -> OptOk W this ->
  SigOk W g /\ FormedCx W (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet) (combine names (sg_params g)) C).
Proof.
  intros f C ns this g names Hm Ht Ctx Sig Th. pose proof (ctxof_cx_formed W Hm Ht f C ns Ctx) as Fc.
  pose proof (sigof_ok W _ _ _ _ Sig (cf_ty _ _ (proj1 Fc)) Th) as Sg.
  split; [exact Sg|exact (params_formed W C names _ Fc (proj1 Sg))].
Qed.

(* An attached method's receiver at its requirements' meanings is ok. *)
Lemma attached_this_ok : forall C r args, CxFormed W C ->
  Forall2 (fun x xa => fst xa = x /\ nlook x (c_ty C) = Some (snd xa)) (w_reqs W r) args -> TyOk W (TNom r args).
Proof.
  intros C r args Cf F. destruct (reqs_ok W _ _ _ F (cf_ty _ _ Cf)) as [K Ok]. exact (nom_ok_intro W r args K Ok).
Qed.

Hypothesis units_bare : UnitsBare W.
Hypothesis family_sig_ok : FamilySigReadOffOk W.

(* Corollary 1: elaborated types are formed, also canonically. *)
Theorem ex_formed' : forall F C a exp x t, Ex W Prog F C a exp x t ->
  FormedCx W C -> FrameFormed W F -> OptOk W exp -> UnionFormed (canon C t).
Proof.
  intros F C a exp x t D Fc Ff Oe.
  pose proof (canon_ok W C t (proj1 Fc) (ex_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ D Fc Ff Oe)) as [Fm _].
  intros ms E. rewrite E in Fm. apply union_formed in Fm as [Nom [Nd _]]. split; assumption.
Qed.

(* Corollary 2: an elaborated type headed by a tag or unit symbol is that
   nominal at exactly its requirements. *)
Theorem ex_nominal_formed' : forall F C a exp x t, Ex W Prog F C a exp x t ->
  FormedCx W C -> FrameFormed W F -> OptOk W exp ->
  forall h, head (canon C t) = Some h -> (kind_sym W h = Some DKTag \/ kind_sym W h = Some DKUnit) ->
  exists xs, canon C t = TNom h xs /\ map fst xs = w_reqs W h.
Proof.
  intros F C a exp x t D Fc Ff Oe h Hd Kd.
  pose proof (canon_ok W C t (proj1 Fc) (ex_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ D Fc Ff Oe)) as [Fm Gm].
  revert Fm Gm Hd. destruct (canon C t) as [m0|fs| |g|n xs|ms]; intros Fm Gm Hd; cbn in Hd; try discriminate.
  - injection Hd as ->. exfalso. destruct (Gm h (or_introl eq_refl)) as [N1 N2]. destruct Kd; contradiction.
  - injection Hd as ->. exists xs. split; [reflexivity|]. exact (proj1 (proj1 (nom_formed W h xs) Fm)).
Qed.

End Corollaries.

(* Corollary 3: a method key's receiver mentions only the context's generations. *)
Theorem keys_scoped : forall W C r m th p, FormedCx W C -> In (IMth r m th, p) (A.c_provisions C) ->
  forall y, In y (A.gens r) -> In y (scope_gens C) \/ A.kind_sym W y = Some A.DKType.
Proof. intros W C r m th p [_ [Ks _]] Iq y Iy. exact (Ks r m th p Iq y Iy). Qed.

Theorem keys_scoped_context : forall W C r m th p, FormedCx W C -> In (IMth r m th, p) (A.c_provisions C) ->
  forall y, In y (A.gens r) -> In y (A.context_gens C) \/ A.kind_sym W y = Some A.DKType.
Proof.
  intros W C r m th p [_ [Ks _]] Iq y Iy. destruct (Ks r m th p Iq y Iy) as [Sc|K]; [left|right; exact K].
  unfold scope_gens in Sc. unfold A.context_gens.
  apply in_app_or in Sc as [Sc|Sc]; apply in_or_app; [left; exact Sc|right; apply in_or_app; left; exact Sc].
Qed.

(* A type-table key is one of the context's generations or a declared type
   symbol (so never a fresh generation, which is undeclared). *)
Theorem table_keys_scoped : forall W C x, FormedCx W C -> In x (map fst (A.c_ty C)) ->
  In x (A.context_gens C) \/ A.kind_sym W x = Some A.DKType.
Proof.
  intros W C x [_ [_ Ts]] Ix. right. exact (Ts x Ix).
Qed.

(* The context a family demand leaves (ExCallD's and MCAttachedDefined's
   C'/Cd, FamilyStep's) is formed when the demanded keys' receivers are ok
   and in scope: each family's matching is read off one of them. *)
Theorem demand_formed_cx : forall W C0 wanted used C wrap Cf, A.Demand W C0 wanted used C wrap Cf ->
  FormedCx W C -> (forall r m, In (A.mkey r m) wanted -> TyOk W r /\ ScopedTy W C r) ->
  FormedCx W Cf.
Proof.
  intros W C0 wanted used C wrap Cf Dm [Cf0 [Ks Ts]] Wk.
  destruct (demand_formed W _ _ _ _ _ _ Dm Cf0 Ks Wk) as [Cf1 Ks1].
  exact (conj Cf1 (conj Ks1 (demand_tys W _ _ _ _ _ _ Dm Ts))).
Qed.

(* A key of a family context's table that F's inputs lack is a type symbol:
   the context's own keys are (TysScoped), and a matching key is a P(F)
   symbol, which F's inputs list unless F lists its polymorphic inputs,
   which are types (FamilyShape). *)
Theorem family_keys_typed : forall W C u matching th sm Cr ks, TysScoped W C ->
  A.FamilyApplication W C u matching th sm Cr ks -> forall Cu needs, A.FunCtx W u Cu needs ->
  forall x, A.nlook x (A.c_ty (A.family_context C matching)) <> None ->
  A.nlook x (A.c_ty Cu) = None -> A.kind_sym W x = Some A.DKType.
Proof.
  intros W C u matching th sm Cr ks Ts (fx & Cu0 & ns & Fx & [Shape _] & Poly & _) Cu needs FC x Lx Nu.
  destruct (family_context_shape matching C) as [Ety _].
  destruct (A.nlook x (A.c_ty (A.family_context C matching))) as [v|] eqn:L; [|congruence].
  apply nlook_in in L. rewrite Ety in L. apply in_app_or in L as [L|L].
  - apply in_rev in L. rewrite Forall_forall in Poly. pose proof (Poly _ L) as Px. cbn in Px.
    destruct Px as (d & n & ps & a & r & D & Ast & Pm).
    destruct Shape as (d' & n' & ps' & a' & r' & D' & Ast' & _ & Listed).
    rewrite D in D'. injection D' as <-. rewrite Ast in Ast'. injection Ast' as <- <- <- <-.
    destruct ps as [names|].
    + destruct Pm as [y [Iy Ry]]. exact (Listed names y x eq_refl Iy Ry).
    + exfalso. destruct FC as (d0 & ps0 & a0 & r0 & b0 & es & tm & tm' & σ & implicit & D0 & FP & _ & Imp & -> & _ & -> & _).
      rewrite D in D0. injection D0 as <-. rewrite Ast in FP. cbn in FP. injection FP as _ _ _ <-.
      assert (Ix : In x (map fst (A.c_ty (A.entries_cx σ (map (fun t => (t, TAbs t)) implicit ++ tm) es)))).
      { cbn. rewrite map_map. cbn. rewrite map_app, map_map. cbn. apply in_or_app.
        destruct (A.nlook x tm) as [w|] eqn:Lt.
        - right. apply nlook_in in Lt. change x with (fst (x, w)). apply in_map, Lt.
        - left. rewrite map_id. apply Imp. split; [reflexivity|split; [exact Pm|exact Lt]]. }
      apply in_map_iff in Ix as [[y w] [Ey Iw]]. cbn in Ey. subst y.
      destruct (A.nlook x (A.c_ty (A.entries_cx σ (map (fun t => (t, TAbs t)) implicit ++ tm) es))) eqn:N; [discriminate|].
      clear Nu. induction (A.c_ty (A.entries_cx σ (map (fun t => (t, TAbs t)) implicit ++ tm) es)) as [|[z u0] rest IH];
        [contradiction|]. cbn in N. destruct (Nat.eqb_spec x z) as [->|Ne]; [discriminate|].
      destruct Iw as [E|Iw]; [injection E as E _; congruence|exact (IH N Iw)].
  - apply Ts. change x with (fst (x, v)). apply in_map. exact L.
Qed.
