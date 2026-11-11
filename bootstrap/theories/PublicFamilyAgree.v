(* Two readings of a family's result (OutputItemsEntries) at two type
   tables related by substitutions s1 and s2, as PublicOutputSubst relates
   them, but without its DomGap escape: for a family u, every symbol the
   result mentions (ResultMentions) is a key of the first table, the first
   table's keys are keys of the second, and an extra key of the second
   that a path names is a type symbol. Then each method entry's receiver,
   signature and instantiation correspond exactly (EntryRelM): the
   instantiation's requirements are in force on both sides (InstSome), and
   the first reading mentions only generations in a given list K. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import AuditBase.
From NM Require Import Audit PublicTypeRead PublicCallRealize.
From NM Require PublicFormation PublicOutputSubst PublicCallerKeys PublicOrientation.
From NM Require Import AuditFacts.
Import Audit.Input AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.

Section FamilyRel.
Variable W : A.world.
Variables s1 s2 : subst.
Variable u : nat.
Variable K : list nat.
Import A.

Ltac kmis := exfalso;
  repeat match goal with
  | H1 : resolve_sym W ?p ?pa = Some ?x, H2 : resolve_sym W ?p ?pa = Some ?y |- _ =>
      assert_fails (constr_eq x y); assert (y = x) by congruence; subst y; clear H2
  end;
  repeat match goal with
  | H1 : w_decl W ?x = Some ?a, H2 : w_decl W ?x = Some ?b |- _ =>
      assert_fails (constr_eq a b); rewrite H1 in H2; injection H2 as H2; subst b
  end;
  unfold kind_sym in *;
  repeat match goal with H : w_decl W _ = Some _ |- _ => rewrite H in *; clear H end;
  repeat match goal with H : d_ast _ = _ |- _ => rewrite H in *; clear H end;
  cbn in *; intuition congruence.

Ltac flat := repeat match goal with
  | H : _ \/ _ |- _ => destruct H
  | H : _ /\ _ |- _ => destruct H
  | H : exists _, _ |- _ => destruct H end.

Notation Agree := (PublicTypeRead.Agree s1 s2).
Notation Rel := (PublicTypeRead.Rel s1 s2).
Notation TyRel := (PublicOutputSubst.TyRel s1 s2).
Notation SigRel := (PublicOutputSubst.SigRel s1 s2).
Notation InstRel := (PublicOutputSubst.InstRel s1 s2).

(* Every requirement in force on both sides, its meanings related. *)
Definition InstSome (a1 a2 : inst) : Prop :=
  Forall2 (fun q1 q2 => fst q1 = fst q2 /\ exists x1 x2, snd q1 = Some x1 /\ snd q2 = Some x2 /\ TyRel x1 x2) a1 a2.

Definition EntryRelM (e1 e2 : entry) : Prop :=
  match e1, e2 with
  | NMth r1 m1 g1 a1, NMth r2 m2 g2 a2 => m1 = m2 /\ TyRel r1 r2 /\ SigRel g1 g2 /\
      (kind_sym W m1 = Some DKMeth -> InstSome a1 a2) /\ incl (gens r1 ++ sig_gens g1 ++ inst_gens a1) K
  | NMth _ _ _ _, _ => False
  | _, NMth _ _ _ _ => False
  | _, _ => True
  end.

(* The second table has every key of the first; an extra key of the
   second that a path names is a type symbol. *)
Definition DomT (tm1 tm2 : list (nat * ty)) : Prop :=
  (forall x, nlook x tm1 <> None -> nlook x tm2 <> None) /\
  (forall q pa x, resolve_sym W q pa = Some x -> nlook x tm2 <> None -> nlook x tm1 = None ->
     kind_sym W x = Some DKType).

Definition KT : list (nat * ty) := map (fun g => (g, TUnit)) K.
Lemma kt_fst : map fst KT = K.
Proof. unfold KT. rewrite map_map. apply map_id. Qed.

Lemma obr_gens : forall p tm a bs, OutputBrackets W p tm a bs -> forall g, In g (tm_gens bs) -> In g (tm_gens tm).
Proof.
  intros p tm a bs D g Ig. destruct D as [|bs l F]; [contradiction|].
  apply in_flat_map in Ig as [q [Iq Ig]]. apply (proj2 (in_rev _ _)) in Iq.
  induction F as [|b0 tb bs' l' (sp & sa & E1 & R1 & K1 & T1) Fr IH]; [contradiction|].
  destruct Iq as [<-|Iq]; [|exact (IH Iq)].
  destruct (tyof_gens W _ _ _ _ _ T1 g Ig) as [In'|[x [E _]]]; [exact In'|discriminate].
Qed.

Lemma obr_keys : forall p tm a bs, OutputBrackets W p tm a bs ->
  forall b t, In b (match a with Some bs0 => bs0 | None => [] end) -> resolve_sym W p (fst b) = Some t ->
  nlook t bs <> None.
Proof.
  intros p tm a bs D b t Ib Rb. destruct D as [|bs l F]; [contradiction|].
  assert (It : In t (map fst l)).
  { clear -F Ib Rb. induction F as [|b0 tb bs' l' (sp & sa & E1 & R1 & K1 & T1) Fr IH]; [contradiction|].
    destruct Ib as [<-|Ib]; [left; congruence|right; exact (IH Ib)]. }
  assert (It' : In t (map fst (rev l))) by (rewrite map_rev; apply (proj1 (in_rev _ _)); exact It).
  destruct (nlook_some _ t (rev l) It') as [v L]. rewrite L. discriminate.
Qed.

Lemma classic_bound : forall (a : option (list (path * spec))) p t,
  (exists b, In b (match a with Some bs0 => bs0 | None => [] end) /\ resolve_sym W p (fst b) = Some t) \/
  ~ (exists b, In b (match a with Some bs0 => bs0 | None => [] end) /\ resolve_sym W p (fst b) = Some t).
Proof.
  intros a p t. generalize (match a with Some bs0 => bs0 | None => [] end) as l.
  induction l as [|b rest IH]; [right; intros [b [[] _]]|].
  destruct (resolve_sym W p (fst b)) as [x|] eqn:R.
  - destruct (Nat.eq_dec x t) as [->|Ne]; [left; exists b; split; [left; reflexivity|exact R]|].
    destruct IH as [[b' [I E]]|N]; [left; exists b'; split; [right; exact I|exact E]|].
    right. intros [b' [[<-|I] E]]; [congruence|exact (N (ex_intro _ b' (conj I E)))].
  - destruct IH as [[b' [I E]]|N]; [left; exists b'; split; [right; exact I|exact E]|].
    right. intros [b' [[<-|I] E]]; [congruence|exact (N (ex_intro _ b' (conj I E)))].
Qed.

Lemma select_facts : forall X (P : X -> Prop) l s, Select P l s -> forall x, In x s -> P x /\ In x l.
Proof.
  intros X P l s S. induction S as [|x xs sel Px S IH|x xs sel Nx S IH]; intros y Iy; [contradiction| |].
  - destruct Iy as [<-|Iy]; [split; [exact Px|left; reflexivity]|]. destruct (IH y Iy) as [Py Iy']. split; [exact Py|right; exact Iy'].
  - destruct (IH y Iy) as [Py Iy']. split; [exact Py|right; exact Iy'].
Qed.

(* Keys present in one full table are present in the other. *)
Lemma dom12 : forall l1 l2 tm1 tm2 x, Forall2 Rel l1 l2 -> DomT tm1 tm2 ->
  nlook x (l1 ++ tm1) <> None -> nlook x (l2 ++ tm2) <> None.
Proof.
  intros l1 l2 tm1 tm2 x F [D _] N. rewrite nlook_app' in N |- *. destruct (rel_nlook s1 s2 l1 l2 F x) as [S _].
  destruct (nlook x l1) eqn:L1; destruct (nlook x l2) eqn:L2; try discriminate.
  - exfalso. destruct S as [_ S]. discriminate (S eq_refl).
  - exact (D x N).
Qed.

Lemma inst_some_of : forall rq (T1 T2 : list (nat * ty)), (forall t, In t rq -> nlook t T1 <> None) ->
  (forall x, nlook x T1 <> None -> nlook x T2 <> None) -> Agree T1 T2 ->
  InstSome (map (fun r => (r, nlook r T1)) rq) (map (fun r => (r, nlook r T2)) rq).
Proof.
  intros rq T1 T2 Each D Ag. induction rq as [|t rest IH]; cbn; constructor.
  - split; [reflexivity|].
    pose proof (Each t (or_introl eq_refl)) as N1. pose proof (D t N1) as N2.
    destruct (nlook t T1) as [x1|] eqn:L1; [|exfalso; exact (N1 eq_refl)].
    destruct (nlook t T2) as [x2|] eqn:L2; [|exfalso; exact (N2 eq_refl)].
    exists x1, x2. split; [reflexivity|]. split; [reflexivity|]. exact (Ag t x1 x2 L1 L2).
  - apply IH. intros t' It'. exact (Each t' (or_intror It')).
Qed.

Theorem family_entries_rel :
  (forall p ob1 tm1 sp es1 tm1', OutputItemEntries W p ob1 tm1 sp es1 tm1' ->
     forall ob2 tm2 es2 tm2', OutputItemEntries W p ob2 tm2 sp es2 tm2' ->
     Forall2 Rel ob1 ob2 -> Agree tm1 tm2 -> DomT tm1 tm2 ->
     (forall t, ResultMentions W u p sp t -> nlook t (ob1 ++ tm1) <> None) ->
     incl (tm_gens (ob1 ++ tm1)) K -> Forall2 EntryRelM es1 es2) /\
  (forall p ob1 tm1 sps es1 tm1', OutputItemsEntries W p ob1 tm1 sps es1 tm1' ->
     forall ob2 tm2 es2 tm2', OutputItemsEntries W p ob2 tm2 sps es2 tm2' ->
     Forall2 Rel ob1 ob2 -> Agree tm1 tm2 -> DomT tm1 tm2 ->
     (forall sp t, In sp sps -> ResultMentions W u p sp t -> nlook t (ob1 ++ tm1) <> None) ->
     incl (tm_gens (ob1 ++ tm1)) K -> Forall2 EntryRelM es1 es2).
Proof.
  apply (PublicFormation.oies_mut W
    (fun p ob1 tm1 sp es1 tm1' _ => forall ob2 tm2 es2 tm2', OutputItemEntries W p ob2 tm2 sp es2 tm2' ->
       Forall2 Rel ob1 ob2 -> Agree tm1 tm2 -> DomT tm1 tm2 ->
       (forall t, ResultMentions W u p sp t -> nlook t (ob1 ++ tm1) <> None) ->
       incl (tm_gens (ob1 ++ tm1)) K -> Forall2 EntryRelM es1 es2)
    (fun p ob1 tm1 sps es1 tm1' _ => forall ob2 tm2 es2 tm2', OutputItemsEntries W p ob2 tm2 sps es2 tm2' ->
       Forall2 Rel ob1 ob2 -> Agree tm1 tm2 -> DomT tm1 tm2 ->
       (forall sp t, In sp sps -> ResultMentions W u p sp t -> nlook t (ob1 ++ tm1) <> None) ->
       incl (tm_gens (ob1 ++ tm1)) K -> Forall2 EntryRelM es1 es2)).
  - (* type *)
    intros p ob tm pa a bs t R Kd B ob2 tm2 es2 tm2' D2 Ob Ag Dm Mt Kg.
    inversion D2; subst; try (flat; kmis). repeat constructor.
  - (* nothing *)
    intros p ob tm pa a bs s k R Kd B Kk ob2 tm2 es2 tm2' D2 Ob Ag Dm Mt Kg.
    inversion D2; subst; first [apply Forall2_nil|flat; kmis].
  - (* val *)
    intros p ob tm a pa s d n t ty0 bs ap R Dd Ast B T App ob2 tm2 es2 tm2' D2 Ob Ag Dm Mt Kg.
    inversion D2; subst; try (flat; kmis). repeat constructor.
  - (* fn *)
    intros p ob tm a pa s g bs ap d rcv n ps rt R Kd Dd Ast B Sig App ob2 tm2 es2 tm2' D2 Ob Ag Dm Mt Kg.
    inversion D2; subst; try (flat; kmis). repeat constructor.
  - (* functor *)
    intros p ob tm a bs pa u' d n ps args res fx R Dd Ast B Fx Rd ob2 tm2 es2 tm2' D2 Ob Ag Dm Mt Kg.
    inversion D2; subst; try (flat; kmis). repeat constructor.
  - (* method *)
    intros p ob tm a pa dn r m rt g bs ap R Mn B Rv Sig App ob2 tm2 es2 tm2' D2 Ob Ag Dm Mt Kg.
    inversion D2; subst; try (flat; kmis).
    match goal with H : resolve_sym W p pa = Some ?s' |- _ => assert_fails (constr_eq s' r); assert (s' = r) by congruence; subst s' end.
    pose proof (agree_app s1 s2 _ _ _ _ Ob Ag) as Ag1.
    match goal with H : OutputBrackets W p (ob2 ++ _) a ?bs2 |- _ =>
      pose proof (PublicOutputSubst.obr_rel W s1 s2 _ _ _ _ _ _ B H Ag1) as Bs; rename bs2 into bsB end.
    pose proof (agree_app s1 s2 _ _ _ _ Bs Ag1) as Ag2.
    rename m into m1.
    match goal with H : MethodNamed W p r dn ?m2 |- _ => rename m2 into mm end.
    assert (Em : mm = m1).
    { match goal with H : MethodNamed W p r dn mm |- _ => destruct Mn as [A1|[N1 S1]]; destruct H as [A2|[N2 S2]] end.
      - unfold Attached in A1, A2. congruence.
      - exfalso. exact (N2 m1 A1).
      - exfalso. exact (N1 mm A2).
      - congruence. }
    subst mm.
    match type of Dm with DomT _ ?T => rename T into tm2 end.
    assert (FB : Forall2 Rel (bs ++ ob) (bsB ++ ob2)) by exact (Forall2_app Bs Ob).
    assert (Bound : forall t, (exists b, In b (match a with Some bs0 => bs0 | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
      nlook t (bs ++ ob ++ tm) <> None).
    { intros t [b [Ib Rb]]. rewrite nlook_app'. pose proof (obr_keys _ _ _ _ B b t Ib Rb) as Nb.
      destruct (nlook t bs); [discriminate|exfalso; exact (Nb eq_refl)]. }
    assert (Ext : forall t, nlook t (ob ++ tm) <> None -> nlook t (bs ++ ob ++ tm) <> None).
    { intros t N. rewrite nlook_app'. destruct (nlook t bs); [discriminate|exact N]. }
    assert (D12 : forall x, nlook x (bs ++ ob ++ tm) <> None -> nlook x (bsB ++ ob2 ++ tm2) <> None).
    { intros x N. rewrite app_assoc in N |- *. exact (dom12 _ _ _ _ x FB Dm N). }
    (* the receiver is in force on both sides, or on neither *)
    assert (Df : nlook r (bs ++ ob ++ tm) = None <-> nlook r (bsB ++ ob2 ++ tm2) = None).
    { destruct (nlook r (bs ++ ob ++ tm)) eqn:L1.
      - split; [discriminate|]. intros L2. exfalso. apply (D12 r); [rewrite L1; discriminate|exact L2].
      - split; [intros _|reflexivity]. pose proof L1 as L1'.
        rewrite app_assoc, nlook_app' in L1 |- *.
        destruct (rel_nlook s1 s2 _ _ FB r) as [S _].
        destruct (nlook r (bs ++ ob)) eqn:K1; [discriminate|]. rewrite (proj1 S eq_refl).
        destruct (nlook r tm2) eqn:K2; [|reflexivity]. exfalso.
        assert (Ty : kind_sym W r = Some DKType) by (apply (proj2 Dm p pa r R); [rewrite K2; discriminate|exact L1]).
        destruct (classic_bound a p r) as [Bd|NB].
        + exact (Bound r Bd L1').
        + exact (Ext r (Mt r (@RMReceiver W u p pa dn a r r R (or_introl (conj eq_refl Ty)) NB)) L1'). }
    assert (Sc : forall gq, In gq (tm_gens (bs ++ ob ++ tm)) -> In gq (map fst KT)).
    { intros gq Ig. rewrite kt_fst. apply tm_gens_app in Ig as [Ib|Io]; [exact (Kg gq (obr_gens _ _ _ _ B gq Ib))|exact (Kg gq Io)]. }
    constructor; [|constructor]. cbn. split; [reflexivity|].
    match goal with H : RecvTy W r _ ?rt2 |- _ => pose proof (PublicOutputSubst.recv_rel W s1 s2 _ _ _ _ _ Rv H Ag2 Df) as Rr end.
    split; [exact Rr|]. split.
    + match goal with H : SigOf W m1 _ (Some ?rt2) ?g2 |- _ =>
        exact (sig_rel W s1 s2 _ _ _ _ _ _ _ Sig H Ag2 (fun x1 x2 E1 E2 => ltac:(injection E1 as <-; injection E2 as <-; exact Rr))) end.
    + split.
      * intros Km. match goal with H : AppOf W m1 _ ?a2 |- _ => destruct App as [rq1 [E1 K1]]; destruct H as [rq2 [E2 K2]] end.
        assert (Rq : rq1 = rq2).
        { destruct K1 as [[M1 S1]|[M1 S1]]; destruct K2 as [[M2 S2]|[M2 S2]];
            [|congruence|congruence|congruence].
          destruct S1 as (sel1 & ex1 & S1 & O1 & ->). destruct S2 as (sel2 & ex2 & S2 & O2 & ->).
          assert (Ee : ex1 = ex2) by (eapply sig_outside_det; eassumption).
    rewrite (PublicOutputSubst.select_det _ _ _ _ _ S1 S2), Ee. reflexivity. }
        subst rq2. destruct K1 as [[_ (sel1 & ex1 & S1 & O1 & Eq1)]|[M1 _]]; [|congruence]. subst. unfold InstSome.
        assert (Each : forall t, In t (sel1 ++ ex1) -> nlook t (bs ++ ob ++ tm) <> None).
        { intros t It.
          assert (SM : SigMentions W m1 t).
          { apply in_app_or in It as [It|It];
              [exact (proj1 (select_facts _ _ _ _ S1 t It)) | exact (proj1 (proj1 (proj2 O1 t) It))]. }
          destruct (classic_bound a p t) as [Bd|NB]; [exact (Bound t Bd)|].
          apply Ext. exact (Mt t (@RMMeth W u p pa dn a r m1 t R Mn SM NB)). }
        exact (inst_some_of (sel1 ++ ex1) _ _ Each D12 Ag2).
      * intros gq Ig. rewrite <- kt_fst. apply in_app_or in Ig as [Ig|Ig]; [exact (recv_scoped W _ _ _ _ Rv Sc gq Ig)|].
        apply in_app_or in Ig as [Ig|Ig].
        -- apply (sigof_scoped W _ _ _ _ _ _ Sig Sc); [|exact Ig].
           intros x E g1 Ig1. injection E as <-. exact (recv_scoped W _ _ _ _ Rv Sc g1 Ig1).
        -- exact (Sc gq (appof_scoped W _ _ _ App gq Ig)).
  - (* context *)
    intros p ob tm a pa c d n its sps bs es tm' R Dd Ast Esps B Inner IH ob2 tm2 es2 tm2' D2 Ob Ag Dm Mt Kg.
    inversion D2; subst; try (flat; kmis).
    match goal with H : resolve_sym W p pa = Some ?s' |- _ => assert_fails (constr_eq s' c); assert (s' = c) by congruence; subst s' end.
    match goal with H : w_decl W c = Some ?d' |- _ => assert_fails (constr_eq d' d); assert (d' = d) by congruence; subst d' end.
    rewrite Ast in *. match goal with H : ADCtx _ _ = ADCtx _ _ |- _ => injection H as <- Ei end.
    apply map_CISpec_inj in Ei. subst.
    pose proof (agree_app s1 s2 _ _ _ _ Ob Ag) as Ag1.
    match goal with H : OutputBrackets W p (ob2 ++ _) a ?bs2 |- _ => pose proof (PublicOutputSubst.obr_rel W s1 s2 _ _ _ _ _ _ B H Ag1) as Bs end.
    match goal with H : OutputItemsEntries W _ (_ ++ ob2) _ _ ?es2 _ |- _ => apply (IH _ _ _ _ H (Forall2_app Bs Ob) Ag Dm) end.
    + intros sp t Isp Rm. rewrite <- app_assoc, nlook_app'.
      destruct (classic_bound a p t) as [Bd|NB].
      * destruct Bd as [b [Ib Rb]]. pose proof (obr_keys _ _ _ _ B b t Ib Rb) as Nb.
        destruct (nlook t bs); [discriminate|exfalso; exact (Nb eq_refl)].
      * destruct (nlook t bs); [discriminate|].
        exact (Mt t (@RMCtx W u p pa a c d n _ sp t R Dd Ast (in_map CISpec _ _ Isp) Rm NB)).
    + intros g0 Ig. rewrite <- app_assoc in Ig. apply tm_gens_app in Ig as [Ib|Io]; [exact (Kg g0 (obr_gens _ _ _ _ B g0 Ib))|exact (Kg g0 Io)].
  - (* nil *)
    intros p ob tm ob2 tm2 es2 tm2' D2 Ob Ag Dm Mt Kg. inversion D2; subst. constructor.
  - (* cons *)
    intros p ob tm sp rest es1 tm1 es2 tm2 D1 IH1 Ds IHs ob2 tm2x es2x tm2x' D2 Ob Ag Dm Mt Kg.
    inversion D2; subst.
    match goal with H1 : OutputItemEntries W p ob2 tm2x sp ?a ?b, H2 : OutputItemsEntries W p ob2 ?b rest ?c ?e |- _ =>
      pose proof (IH1 _ _ _ _ H1 Ob Ag Dm (fun t Rm => Mt sp t (or_introl eq_refl) Rm) Kg) as F1;
      rewrite (proj1 (PublicFormation.output_entries_tm W) _ _ _ _ _ _ H1) in H2;
      rewrite (proj1 (PublicFormation.output_entries_tm W) _ _ _ _ _ _ D1) in IHs;
      exact (Forall2_app F1 (IHs _ _ _ _ H2 Ob Ag Dm (fun sp' t Isp Rm => Mt sp' t (or_intror Isp) Rm) Kg)) end.
Qed.

End FamilyRel.

(* ---- the agreement a platform family's run needs ------------------------------ *)

(* A caller's signature type and a family's, read at θ: the family's
   type over the inputs of its code (Ch0's table). *)
Definition SigAgree (C Ch0 : A.cx) (th : subst) (tc tf : ty) : Prop :=
  incl (A.gens tf) (map fst (A.c_ty Ch0)) /\ A.canon C tc = A.canon C (app_subst th tf).

(* The new method provisions of a result Cr (New r m g: a new method r.m
   of Cr, with signature g) and a family's own items (outputs, keys, read
   at its inputs Ch0) agree at θ, read in the caller's context C: each new
   method is filed under the run-time key a family item gets at θ and read
   with that item's signature at θ, and every family item is one of them. *)
Definition ItemsAgree (C Cr Ch0 outputs : A.cx) (th : subst) (New : ty -> nat -> A.fsig -> Prop)
    (keys : list item) : Prop :=
  (forall r m g, New r m g ->
     (exists key, In key keys /\ app_subst_item th (A.rkey_of outputs key) = A.rkey Cr r m) /\
     (forall key rf a gf, In key keys -> key = IMth rf m a -> A.mlook rf m (A.c_mths outputs) = Some gf ->
        app_subst_item th (A.rkey_of outputs key) = A.rkey Cr r m ->
        Forall2 (SigAgree C Ch0 th) (A.sg_ret g :: r :: A.sg_params g) (A.sg_ret gf :: rf :: A.sg_params gf))) /\
  (forall key, In key keys -> exists r m g,
     New r m g /\ app_subst_item th (A.rkey_of outputs key) = A.rkey Cr r m).

(* ---- list facts ---------------------------------------------------------------- *)

Lemma forall2_pairs : forall X Y (R : X -> Y -> Prop) l1 l2, Forall2 R l1 l2 ->
  exists L, l1 = map fst L /\ l2 = map snd L /\ Forall (fun p => R (fst p) (snd p)) L.
Proof.
  intros X Y R l1 l2 F. induction F as [|x y l1 l2 Rxy F IH]; [exists []; repeat split; constructor|].
  destruct IH as [L [-> [-> FL]]]. exists ((x, y) :: L). split; [reflexivity|]. split; [reflexivity|].
  constructor; [exact Rxy|exact FL].
Qed.

Definition keys_of {X : Type} (f : X -> option item) (l : list X) : list item :=
  flat_map (fun e => match f e with Some k => [k] | None => [] end) l.

Lemma keys_of_in : forall X (f : X -> option item) l k, In k (keys_of f l) <-> exists e, In e l /\ f e = Some k.
Proof.
  intros X f l k. unfold keys_of. rewrite in_flat_map. split.
  - intros [e [Ie Ik]]. exists e. split; [exact Ie|]. destruct (f e); [destruct Ik as [<-|[]]; reflexivity|contradiction].
  - intros [e [Ie E]]. exists e. split; [exact Ie|]. rewrite E. left. reflexivity.
Qed.

Lemma keys_of_map : forall X Y (f : Y -> option item) (g : X -> Y) l, keys_of f (map g l) = keys_of (fun x => f (g x)) l.
Proof. intros X Y f g l. unfold keys_of. induction l as [|x r IH]; [reflexivity|]. cbn. rewrite IH. reflexivity. Qed.

(* Keys read off related pairs, position by position. *)
Lemma keys_of_pairs : forall X Y (L : list (X * Y)) (f1 : X -> option item) (f2 : Y -> option item) (R : item -> item -> Prop),
  (forall p, In p L -> (f1 (fst p) = None <-> f2 (snd p) = None) /\
     forall k1 k2, f1 (fst p) = Some k1 -> f2 (snd p) = Some k2 -> R k2 k1) ->
  Forall2 R (keys_of f2 (map snd L)) (keys_of f1 (map fst L)).
Proof.
  intros X Y L f1 f2 R. induction L as [|p L IH]; intros Ok; [constructor|]. cbn [map]. unfold keys_of. cbn [flat_map].
  fold (keys_of f2 (map snd L)) (keys_of f1 (map fst L)).
  destruct (Ok p (or_introl eq_refl)) as [Iff Rp].
  assert (IH' : Forall2 R (keys_of f2 (map snd L)) (keys_of f1 (map fst L))) by (apply IH; intros q Iq; exact (Ok q (or_intror Iq))).
  destruct (f1 (fst p)) as [k1|] eqn:E1; destruct (f2 (snd p)) as [k2|] eqn:E2.
  - constructor; [exact (Rp k1 k2 eq_refl eq_refl)|exact IH'].
  - exfalso. pose proof (proj2 Iff eq_refl). discriminate.
  - exfalso. pose proof (proj1 Iff eq_refl). discriminate.
  - exact IH'.
Qed.

Lemma keys_unique : forall X (f : X -> option item) l, NoDup (keys_of f l) ->
  forall e e' k, In e l -> In e' l -> f e = Some k -> f e' = Some k -> e = e'.
Proof.
  intros X f l. induction l as [|x r IH]; intros Nd e e' k Ie Ie' Fe Fe'; [contradiction|].
  cbn in Nd. assert (Out : forall y, In y r -> f y = Some k -> f x = Some k -> False).
  { intros y Iy Fy Fx. rewrite Fx in Nd. cbn in Nd. inversion Nd as [|? ? Nk _]. apply Nk.
    apply keys_of_in. exists y. split; assumption. }
  assert (Nd' : NoDup (keys_of f r)) by (destruct (f x); [inversion Nd; assumption|exact Nd]).
  destruct Ie as [<-|Ie]; destruct Ie' as [<-|Ie'].
  - reflexivity.
  - exfalso. exact (Out e' Ie' Fe' Fe).
  - exfalso. exact (Out e Ie Fe Fe').
  - exact (IH Nd' e e' k Ie Ie' Fe Fe').
Qed.

Lemma ilook_app' : forall X k (l1 l2 : list (item * X)),
  A.ilook k (l1 ++ l2) = match A.ilook k l1 with Some p => Some p | None => A.ilook k l2 end.
Proof.
  intros X k l1 l2. induction l1 as [|[k' p] r IH]; [reflexivity|]. cbn.
  destruct (item_eqb k k'); [reflexivity|exact IH].
Qed.

Lemma mlook_app' : forall r m (l1 l2 : list ((ty * nat) * A.fsig)),
  A.mlook r m (l1 ++ l2) = match A.mlook r m l1 with Some g => Some g | None => A.mlook r m l2 end.
Proof.
  intros r m l1 l2. induction l1 as [|[[r' m'] g] rest IH]; [reflexivity|]. cbn.
  destruct (ty_eqb r r' && Nat.eqb m m'); [reflexivity|exact IH].
Qed.

Lemma ilook_apps : forall C k, A.ilook k (A.c_apps C) = option_map A.provision_inst (A.ilook k (A.c_provisions C)).
Proof.
  intros C k. unfold A.c_apps. induction (A.c_provisions C) as [|[k' p] r IH]; [reflexivity|]. cbn.
  destruct (item_eqb k k'); [destruct p; reflexivity|exact IH].
Qed.

Lemma entry_provision_of : forall s (f : A.inst -> A.inst) e k, A.entry_key s e = Some k ->
  exists p, A.entry_provision s f e = [(k, p)].
Proof. intros s f [t|v t a|g0 sg a|r m g a|u fx|eql eqr] k E; cbn in E |- *; try discriminate; injection E as <-; eexists; reflexivity. Qed.

Lemma entry_provision_none : forall s (f : A.inst -> A.inst) e, A.entry_key s e = None -> A.entry_provision s f e = [].
Proof. intros s f [t|v t a|g0 sg a|r m g a|u fx|eql eqr] E; cbn in E |- *; try discriminate; reflexivity. Qed.

(* The provision a key unique among the entries' keys gets. *)
Lemma ilook_entries : forall s es e k, In e es -> A.entry_key s e = Some k ->
  (forall e', In e' es -> A.entry_key s e' = Some k -> e' = e) ->
  A.ilook k (flat_map (A.entry_provision s (fun a => a)) es) =
  match A.entry_provision s (fun a => a) e with [(_, p)] => Some p | _ => None end.
Proof.
  intros s es e k. induction es as [|x r IH]; intros Ie Ek Un; [contradiction|]. cbn [flat_map].
  rewrite ilook_app'. destruct (A.entry_key s x) as [kx|] eqn:Kx.
  - destruct (entry_provision_of s (fun a => a) x kx Kx) as [px Ex]. rewrite Ex. cbn.
    destruct (item_eqb k kx) eqn:Eq.
    + apply item_eqb_spec in Eq. subst kx. rewrite <- (Un x (or_introl eq_refl) Kx), Ex. reflexivity.
    + apply IH; [destruct Ie as [<-|Ie]; [|exact Ie]|exact Ek|intros e' Ie' E'; exact (Un e' (or_intror Ie') E')].
      exfalso. rewrite Ek in Kx. injection Kx as <-. rewrite (proj2 (item_eqb_spec k k) eq_refl) in Eq. discriminate.
  - rewrite (entry_provision_none s _ x Kx). cbn.
    apply IH; [destruct Ie as [<-|Ie]; [congruence|exact Ie]|exact Ek|intros e' Ie' E'; exact (Un e' (or_intror Ie') E')].
Qed.

Lemma keys_match_entries : forall s es, KeysMatch (flat_map (A.entry_provision s (fun a => a)) es).
Proof.
  intros s es k p I. apply in_flat_map in I as [e [_ I]].
  destruct e as [t|v t a|f g a|r m g a|u fx|eql eqr]; cbn in I; try contradiction; destruct I as [E|[]]; injection E as <- <-; cbn; auto.
Qed.

Lemma lookup_keys : forall (f : nat -> ty) (l : list (nat * ty)) x, In x (map fst l) ->
  lookup_subst x (map (fun q => (fst q, f (fst q))) l) = Some (f x).
Proof.
  intros f l x. induction l as [|[y v] r IH]; intros I; [contradiction|]. cbn.
  destruct (Nat.eqb_spec x y) as [->|Ne]; [reflexivity|]. apply IH. destruct I as [E|I]; [cbn in E; congruence|exact I].
Qed.

Lemma nlook_map_snd : forall (f : ty -> ty) (l : list (nat * ty)) k,
  A.nlook k (map (fun q => (fst q, f (snd q))) l) = option_map f (A.nlook k l).
Proof. intros f l k. induction l as [|[a v] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb k a); [reflexivity|exact IH]. Qed.

Lemma nlook_dom : forall X k (l : list (nat * X)), A.nlook k l <> None <-> In k (map fst l).
Proof.
  intros X k l. split.
  - intros N. destruct (A.nlook k l) as [v|] eqn:L; [|congruence]. apply nlook_in in L. exact (in_map fst _ _ L).
  - intros I. destruct (nlook_some X k l I) as [v L]. rewrite L. discriminate.
Qed.

(* ---- a platform family applied as a family: its result agrees ----------------- *)

(* Generally: u's result read in a context Cf (c_sub Cf = c_sub C) at
   θ = Cf's readings of u's inputs (Parts: a family application's facts,
   or an application's, AppPremises, with Cf = C), against its result at
   its own inputs. Uniq: the caller's keys are distinct (a family
   application checks it), or the family's are, with distinct methods
   (Platform.MethodsDistinct). *)
Section FamilyAgree.
Variable W : A.world.
Variables (C : A.cx) (u : nat) (th : subst) (Cf : A.cx) (out2 : nat -> ty) (Cr : A.cx) (ks : list item).
Variables (Cu : A.cx) (needs : list item) (outputs : A.cx) (keys : list item).
Import A.

Hypothesis SubCf0 : c_sub Cf = c_sub C.
Hypothesis Parts : th = map (fun q => (fst q, canon Cf (app_subst (c_ty Cf) (TAbs (fst q))))) (c_ty Cu) /\
  (forall t, In t (map fst (c_ty Cu)) -> nlook t (c_ty Cf) <> None) /\
  (forall g t, In (g, t) (c_sub Cu) -> (forall x, In x (g :: gens t) -> nlook x (c_ty Cf) <> None) ->
     canon Cf (app_subst (c_ty Cf) (TAbs g)) = canon Cf (app_subst (c_ty Cf) t)) /\
  (forall d ps a r b, w_decl W u = Some d -> functor_parts (d_ast d) = Some (ps, a, r, b) -> b = None) /\
  ProvideResult W u Cf out2 Cr ks.
Hypothesis Uniq : NoDup (map provision_id ks) \/ (NoDup (map provision_id keys) /\ Platform.MethodsDistinct W keys).
Hypothesis FCu : FunCtx W u Cu needs.
Hypothesis NoOut : forall t, ~ FunOutputs W u t.
Hypothesis PRf : ProvideResult W u Cu (fun t => TAbs t) outputs keys.
Hypothesis KeysM : forall key, In key keys -> exists r m a, key = IMth r m a /\ kind_sym W m = Some DKMeth.
Hypothesis Idem : forall t, canon C (canon C t) = canon C t.
(* An extra key of the family context's table that a path names is a type
   symbol (a table's keys are type symbols or generations). *)
Hypothesis KeysT : forall q pa x, resolve_sym W q pa = Some x ->
  nlook x (c_ty Cf) <> None -> nlook x (c_ty Cu) = None -> kind_sym W x = Some DKType.

Let σ := c_sub Cu.
Let cS := c_sub C.
Let Dom := map fst (c_ty Cu).

Lemma functx_uniq : forall C1 ns1 C2 ns2, FunCtx W u C1 ns1 -> FunCtx W u C2 ns2 -> C1 = C2 /\ ns1 = ns2.
Proof.
  intros C1 ns1 C2 ns2 (d & ps & a & r & b & es & tm & tm' & s & classes & imp & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & E1)
    (d' & ps' & a' & r' & b' & es' & tm2 & tm2' & s' & classes' & imp' & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & E2).
  rewrite E1 in E2. injection E2 as -> ->. split; reflexivity.
Qed.

Lemma fa_parts : th = map (fun q => (fst q, canon Cf (app_subst (c_ty Cf) (TAbs (fst q))))) (c_ty Cu) /\
  (forall t, In t Dom -> nlook t (c_ty Cf) <> None) /\
  (forall g t, In (g, t) (c_sub Cu) -> (forall x, In x (g :: gens t) -> nlook x (c_ty Cf) <> None) ->
     canon Cf (app_subst (c_ty Cf) (TAbs g)) = canon Cf (app_subst (c_ty Cf) t)) /\
  (forall d ps a r b, w_decl W u = Some d -> functor_parts (d_ast d) = Some (ps, a, r, b) -> b = None) /\
  ProvideResult W u Cf out2 Cr ks.
Proof. exact Parts. Qed.

Lemma sub_cf : c_sub Cf = cS.
Proof. exact SubCf0. Qed.

Lemma canon_f : forall t, canon Cf t = canon C t.
Proof. intros t. unfold canon. rewrite SubCf0. reflexivity. Qed.

Lemma th_lookup : forall x, In x Dom -> lookup_subst x th = Some (canon C (app_subst (c_ty Cf) (TAbs x))).
Proof.
  intros x Ix. pose proof (lookup_keys (fun y => canon Cf (app_subst (c_ty Cf) (TAbs y))) (c_ty Cu) x Ix) as L.
  rewrite (proj1 fa_parts). etransitivity; [exact L|]. f_equal. apply canon_f.
Qed.

Lemma th_on : forall t, incl (gens t) Dom -> app_subst th t = canon C (app_subst (c_ty Cf) t).
Proof.
  intros t Sc. rewrite <- (PublicTypeRead.app_subst_nil (app_subst th t)). unfold canon.
  apply subst2_agree. intros x Ix. cbn [app_subst]. rewrite (th_lookup x (Sc x Ix)). apply PublicTypeRead.app_subst_nil.
Qed.

Lemma sigma_range : forall g t, In (g, t) σ -> In g Dom /\ incl (gens t) Dom.
Proof.
  intros g t I. destruct (functx_scoped W u Cu needs FCu) as [_ [_ [_ [_ [_ [_ R]]]]]]. exact (R g t I).
Qed.

Lemma th_sigma : forall t, app_subst th (app_subst σ t) = app_subst th t.
Proof.
  intros t. rewrite <- (PublicTypeRead.app_subst_nil t) at 2. apply subst2_agree. intros x _.
  cbn [app_subst]. destruct (lookup_subst x σ) as [t'|] eqn:L; [|reflexivity].
  destruct (sigma_range x t' (lookup_subst_in_pair _ _ _ L)) as [Ix St].
  change (app_subst th t' = app_subst th (TAbs x)).
  rewrite (th_on t' St), (th_on (TAbs x) ltac:(intros y [<-|[]]; exact Ix)).
  destruct fa_parts as [_ [InF [Merge _]]].
  rewrite <- !canon_f. symmetry. apply Merge; [exact (lookup_subst_in_pair _ _ _ L)|].
  intros y [<-|Iy]; [exact (InF _ Ix)|exact (InF y (St y Iy))].
Qed.

Lemma tbl_cu : forall k v, nlook k (c_ty Cu) = Some v -> v = app_subst σ (TAbs k).
Proof. intros k v L. exact (proj1 (functx_scoped W u Cu needs FCu) k v L). Qed.

Lemma sigma_scoped : forall t, incl (gens t) Dom -> incl (gens (app_subst σ t)) Dom.
Proof. intros t Sc. apply subst_scoped; [intros g t' I; exact (proj2 (sigma_range g t' I))|exact Sc]. Qed.

Definition T1 := output_type_map Cu [] (fun t => TAbs t).
Definition T2 := output_type_map Cf [] (fun t => TAbs t).

Lemma t1_look : forall k, nlook k T1 = option_map (canon Cu) (nlook k (c_ty Cu)).
Proof. intros k. unfold T1, output_type_map. cbn [map app]. exact (nlook_map_snd (canon Cu) _ k). Qed.
Lemma t2_look : forall k, nlook k T2 = option_map (canon Cf) (nlook k (c_ty Cf)).
Proof. intros k. unfold T2, output_type_map. cbn [map app]. exact (nlook_map_snd (canon Cf) _ k). Qed.

Lemma agree_t : PublicTypeRead.Agree th cS T1 T2.
Proof.
  intros k v1 v2 L1 L2. rewrite t1_look in L1. rewrite t2_look in L2.
  destruct (nlook k (c_ty Cu)) as [w1|] eqn:K1; [|discriminate]. destruct (nlook k (c_ty Cf)) as [w2|] eqn:K2; [|discriminate].
  injection L1 as <-. injection L2 as <-. rewrite (tbl_cu k w1 K1).
  assert (Ik : In k Dom) by (apply nlook_dom; rewrite K1; discriminate).
  change (app_subst th (app_subst σ (app_subst σ (TAbs k))) = canon C (canon Cf w2)).
  rewrite !th_sigma, (th_on (TAbs k) ltac:(intros y [<-|[]]; exact Ik)), canon_f, Idem.
  cbn [app_subst]. rewrite lookup_subst_nlook', K2. reflexivity.
Qed.

Lemma dom_t : DomT W T1 T2.
Proof.
  split.
  - intros x N. rewrite t1_look in N. rewrite t2_look.
    destruct (nlook x (c_ty Cu)) eqn:K1; [|exfalso; exact (N eq_refl)].
    assert (Ix : In x Dom) by (apply nlook_dom; rewrite K1; discriminate).
    destruct fa_parts as [_ [InF _]]. pose proof (InF x Ix) as N2. destruct (nlook x (c_ty Cf)); [discriminate|exfalso; exact (N2 eq_refl)].
  - intros q pa x R N2 N1. rewrite t1_look in N1. rewrite t2_look in N2.
    apply (KeysT q pa x R); [destruct (nlook x (c_ty Cf)); [intros E; discriminate E|exfalso; exact (N2 eq_refl)]|].
    destruct (nlook x (c_ty Cu)); [discriminate|reflexivity].
Qed.

Lemma tbl_in : forall k v, In (k, v) (c_ty Cu) -> v = app_subst σ (TAbs k).
Proof.
  intros k v I. unfold σ. clear KeysT PRf KeysM.
  destruct FCu as (d & ps & a & r & b & es & tm & tm' & s & classes & imp & D & FP & IE & Imp & Etm & MGM & _ & _ & ECu & _).
  rewrite ECu in I |- *. subst tm'. cbn in I |- *.
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT _].
  apply in_map_iff in I as [[k' w] [E I]]. cbn in E. injection E as -> <-.
  apply in_app_or in I as [I|I].
  - apply in_map_iff in I as [t [E _]]. injection E as -> <-. reflexivity.
  - unfold IdForm in IdT. rewrite Forall_forall in IdT. pose proof (IdT _ I) as E'; cbn in E'; rewrite E'; reflexivity.
Qed.

Lemma kg_t : incl (tm_gens ([] ++ T1)) Dom.
Proof.
  intros g Ig. cbn [app] in Ig. unfold tm_gens, T1, output_type_map in Ig. cbn [map app] in Ig.
  apply in_flat_map in Ig as [[k v] [Iq Ig]]. apply in_map_iff in Iq as [[k' w] [E Iq]]. injection E as Ek Ev.
  subst k. rewrite <- Ev in Ig. cbn [snd] in Ig. rewrite (tbl_in k' w Iq) in Ig.
  assert (Ik : In k' Dom) by exact (in_map fst _ _ Iq).
  exact (sigma_scoped _ (sigma_scoped (TAbs k') ltac:(intros y [<-|[]]; exact Ik)) g Ig).
Qed.

Lemma family_shape_b : forall d ps a r b, w_decl W u = Some d -> functor_parts (d_ast d) = Some (ps, a, r, b) -> b = None.
Proof.
  intros d ps a r b D FP. exact (proj1 (proj2 (proj2 (proj2 fa_parts))) d ps a r b D FP).
Qed.

Lemma mentions_dom : forall t, Mentions W u t -> In t Dom.
Proof.
  intros t Mt. pose proof Mt as (d & ps & a & r & b & sp & D & FP & _).
  pose proof (family_shape_b d ps a r b D FP) as Eb.
  unfold Dom. clear KeysT PRf KeysM.
  destruct FCu as (d' & ps' & a' & r' & b' & es & tm & tm' & s & classes & imp & D' & FP' & IE & Imp & Etm & MGM & _ & _ & ECu & _).
  rewrite ECu. cbn [c_ty entries_cx]. rewrite map_map. cbn. subst tm'.
  rewrite D in D'. injection D' as <-. rewrite FP in FP'. injection FP' as <- <- <- <-.
  rewrite map_app. apply in_or_app. destruct (nlook t tm) eqn:L.
  - right. apply nlook_dom. rewrite L. discriminate.
  - left. rewrite map_map. cbn. rewrite map_id. apply Imp. exact (conj Eb (conj Mt L)).
Qed.

Lemma ment_in : forall d ps a r b, w_decl W u = Some d -> functor_parts (d_ast d) = Some (ps, a, r, b) ->
  forall sp t, In sp r -> ResultMentions W u (d_mod d) sp t -> nlook t ([] ++ T1) <> None.
Proof.
  intros d ps a r b D FP sp t Isp Rm. cbn [app]. rewrite t1_look.
  assert (It : In t Dom) by (apply mentions_dom; exists d, ps, a, r, b, sp; repeat split; first [assumption|apply NoOut]).
  apply (proj2 (nlook_dom _ t (c_ty Cu))) in It. destruct (nlook t (c_ty Cu)); [discriminate|exfalso; exact (It eq_refl)].
Qed.

Lemma no_outs : forall outs : list nat, (forall t, In t outs <-> FunOutputs W u t) -> outs = [].
Proof. intros [|t r] O; [reflexivity|]. exfalso. exact (NoOut t (proj1 (O t) (or_introl eq_refl))). Qed.

Lemma entries_rel : exists es1 es2,
  outputs = output_cx Cu T1 es1 /\ keys = keys_of (entry_key σ) es1 /\
  Cr = output_cx Cf T2 es2 /\ ks = keys_of (entry_key cS) es2 /\
  Forall2 (EntryRelM W th cS Dom) es1 es2.
Proof.
  destruct PRf as (d & ps & a & r & b & outs1 & es1 & tm1 & D & FP & O1 & E1 & K1 & Eo).
  destruct fa_parts as [_ [_ [_ [_ PR]]]].
  destruct PR as (d' & ps' & a' & r' & b' & outs2 & es2 & tm2 & D' & FP' & O2 & E2 & K2 & ECr).
  rewrite D in D'. injection D' as <-. rewrite FP in FP'. injection FP' as <- <- <- <-.
  rewrite (no_outs outs1 O1) in *. rewrite (no_outs outs2 O2) in *.
  rewrite (proj2 (PublicFormation.output_entries_tm W) _ _ _ _ _ _ E1) in *.
  rewrite (proj2 (PublicFormation.output_entries_tm W) _ _ _ _ _ _ E2) in *.
  exists es1, es2. split; [exact Eo|]. split; [exact K1|]. split; [exact ECr|]. split.
  - rewrite K2. change (keys_of (entry_key (c_sub Cf)) es2 = keys_of (entry_key cS) es2). rewrite sub_cf. reflexivity.
  - apply (proj2 (family_entries_rel W th cS u Dom) _ _ _ _ _ _ E1 _ _ _ _ E2 (Forall2_nil _) agree_t dom_t).
    + exact (ment_in d ps a r b D FP).
    + exact kg_t.
Qed.

Lemma known_rel : forall a1 a2, InstSome th cS a1 a2 ->
  map (fun q => (fst q, app_subst th (snd q))) (known a1) = map (fun q => (fst q, app_subst cS (snd q))) (known a2).
Proof.
  intros a1 a2 F. induction F as [|q1 q2 a1 a2 [Ef (x1 & x2 & E1 & E2 & R)] _ IH]; [reflexivity|].
  destruct q1 as [y1 o1], q2 as [y2 o2]. cbn in Ef, E1, E2 |- *. subst. cbn. f_equal; [rewrite R; reflexivity|exact IH].
Qed.

Lemma prov_id_keys : forall s es, map provision_id (keys_of (entry_key s) es) = keys_of (entry_key s) es.
Proof.
  intros s es. induction es as [|e r IH]; [reflexivity|]. unfold keys_of in *. cbn [flat_map].
  rewrite map_app, IH. destruct e; reflexivity.
Qed.

(* The provision a method entry gets in a result, when its key is unique
   among the result's keys. *)
Lemma result_lookup : forall (C0 : cx) tm es r m g a,
  In (NMth r m g a) es ->
  (forall e', In e' es -> entry_key (c_sub C0) e' = Some (mkey (app_subst (c_sub C0) r) m) -> e' = NMth r m g a) ->
  mlook (app_subst (c_sub C0) r) m (c_mths (output_cx C0 tm es)) = Some (sig_subst (c_sub C0) g) /\
  ilook (mkey (app_subst (c_sub C0) r) m) (c_apps (output_cx C0 tm es)) = Some a.
Proof.
  intros C0 tm es r m g a Ie Un.
  assert (L : ilook (mkey (app_subst (c_sub C0) r) m) (flat_map (entry_provision (c_sub C0) (fun a => a)) (rev es)) =
    Some (ProvidedMethod (sig_subst (c_sub C0) g) a)).
  { rewrite (ilook_entries (c_sub C0) (rev es) (NMth r m g a) _ (proj1 (in_rev _ _) Ie) eq_refl).
    - reflexivity.
    - intros e' Ie' E'. exact (Un e' (proj2 (in_rev _ _) Ie') E'). }
  split.
  - change (c_mths (output_cx C0 tm es)) with (mths_of (flat_map (entry_provision (c_sub C0) (fun a => a)) (rev es) ++ c_provisions C0)).
    unfold mths_of. rewrite flat_map_app. rewrite mlook_app'. fold (mths_of (flat_map (entry_provision (c_sub C0) (fun a => a)) (rev es))).
    rewrite mths_by_ilook by apply keys_match_entries. rewrite L. reflexivity.
  - rewrite ilook_apps. cbn [output_cx c_provisions]. rewrite ilook_app', L. reflexivity.
Qed.

Lemma sig_agree_of : forall t1 t2, app_subst th t1 = app_subst cS t2 -> incl (gens t1) Dom ->
  SigAgree Cf Cu th (app_subst cS t2) (app_subst σ t1).
Proof.
  intros t1 t2 R Sc. split; [exact (sigma_scoped t1 Sc)|].
  rewrite !canon_f, th_sigma, R. change (canon C (canon C t2) = canon C (canon C t2)). reflexivity.
Qed.

Theorem items_agree_gen : ItemsAgree Cf Cr Cu outputs th
  (fun r m g => In (mkey r m) ks /\ mlook r m (c_mths Cr) = Some g) keys.
Proof.
  destruct entries_rel as (es1 & es2 & Eo & EK1 & ECr & EK2 & Fr).
  destruct (forall2_pairs _ _ _ _ _ Fr) as (L & -> & -> & FL).
  rewrite Forall_forall in FL.
  assert (SubO : c_sub outputs = σ) by (rewrite Eo; reflexivity).
  assert (SubR : c_sub Cr = cS).
  { rewrite ECr. cbn. unfold cS. exact SubCf0. }
  assert (SubCf : c_sub Cf = cS) by exact SubCf0.
  (* a method pair *)
  assert (Pm : forall p r1 m g1 a1, In p L -> fst p = NMth r1 m g1 a1 -> exists r2 g2 a2, snd p = NMth r2 m g2 a2 /\
     app_subst th r1 = app_subst cS r2 /\ PublicOutputSubst.SigRel th cS g1 g2 /\
     (kind_sym W m = Some DKMeth -> InstSome th cS a1 a2) /\ incl (gens r1 ++ sig_gens g1 ++ inst_gens a1) Dom).
  { intros p r1 m g1 a1 Ip E. pose proof (FL p Ip) as R. rewrite E in R.
    destruct (snd p) as [?|?|?|r2 m2 g2 a2|?|? ?]; try contradiction. destruct R as (<- & Rr & Sg & Is & Sc).
    exists r2, g2, a2. split; [reflexivity|exact (conj Rr (conj Sg (conj Is Sc)))]. }
  assert (Pm2 : forall p r2 m g2 a2, In p L -> snd p = NMth r2 m g2 a2 -> exists r1 g1 a1, fst p = NMth r1 m g1 a1).
  { intros p r2 m g2 a2 Ip E. pose proof (FL p Ip) as R. rewrite E in R.
    destruct (fst p) as [?|?|?|r1 m1 g1 a1|?|? ?]; try contradiction. destruct R as (-> & _). eauto. }
  (* the pairs are determined by their caller method keys: the caller's
     keys are distinct, or a caller key's method names one family key *)
  assert (Upair : forall p p' r m, In p L -> In p' L -> entry_key cS (snd p) = Some (mkey r m) ->
    entry_key cS (snd p') = Some (mkey r m) -> p = p').
  { intros p p' r m Ip Ip' E E'. destruct Uniq as [Nd|[Nd1 MD]].
    - rewrite EK2, prov_id_keys, keys_of_map in Nd. exact (keys_unique _ _ L Nd p p' _ Ip Ip' E E').
    - rewrite EK1, prov_id_keys, keys_of_map in Nd1.
      destruct (snd p) as [?|?|?|r2 m2 g2 a2|?|? ?] eqn:S2; cbn in E; try discriminate. injection E as _ Em. subst m2.
      destruct (snd p') as [?|?|?|r2' m2' g2' a2'|?|? ?] eqn:S2'; cbn in E'; try discriminate. injection E' as _ Em'. subst m2'.
      destruct (Pm2 p r2 m g2 a2 Ip S2) as (r1 & g1 & a1 & F1).
      destruct (Pm2 p' r2' m g2' a2' Ip' S2') as (r1' & g1' & a1' & F1').
      assert (I1 : In (mkey (app_subst σ r1) m) (keys_of (entry_key σ) (map fst L))).
      { apply keys_of_in. exists (NMth r1 m g1 a1). split; [rewrite <- F1; exact (in_map fst _ _ Ip)|reflexivity]. }
      assert (I1' : In (mkey (app_subst σ r1') m) (keys_of (entry_key σ) (map fst L))).
      { apply keys_of_in. exists (NMth r1' m g1' a1'). split; [rewrite <- F1'; exact (in_map fst _ _ Ip')|reflexivity]. }
      rewrite <- EK1 in I1, I1'.
      pose proof (MD _ _ _ _ _ _ _ _ I1 I1' eq_refl eq_refl (or_introl eq_refl)) as Ek.
      apply (keys_unique _ _ L Nd1 p p' (mkey (app_subst σ r1) m) Ip Ip').
      + rewrite F1. reflexivity.
      + rewrite F1', Ek. reflexivity. }
  (* each side's method key is unique *)
  assert (Key2 : forall p r1 m g1 a1, In p L -> fst p = NMth r1 m g1 a1 ->
     entry_key cS (snd p) = Some (mkey (app_subst th (app_subst σ r1)) m)).
  { intros p r1 m g1 a1 Ip E. destruct (Pm p r1 m g1 a1 Ip E) as (r2 & g2 & a2 & E2 & Rr & _).
    rewrite E2. cbn. rewrite th_sigma, Rr. reflexivity. }
  assert (Un1 : forall p r1 m g1 a1, In p L -> fst p = NMth r1 m g1 a1 ->
     forall e', In e' (map fst L) -> entry_key σ e' = Some (mkey (app_subst σ r1) m) -> e' = NMth r1 m g1 a1).
  { intros p r1 m g1 a1 Ip E e' Ie' Ek. apply in_map_iff in Ie' as [p' [<- Ip']].
    destruct (fst p') as [?|?|?|r1' m' g1' a1'|?|? ?] eqn:E'; cbn in Ek; try discriminate.
    injection Ek as Er ->. pose proof (Key2 p' r1' m g1' a1' Ip' E') as K'. rewrite Er in K'.
    rewrite (Upair p' p _ _ Ip' Ip K' (Key2 p r1 m g1 a1 Ip E)) in E'. congruence. }
  assert (Un2 : forall p r2 m g2 a2, In p L -> snd p = NMth r2 m g2 a2 ->
     forall e', In e' (map snd L) -> entry_key cS e' = Some (mkey (app_subst cS r2) m) -> e' = NMth r2 m g2 a2).
  { intros p r2 m g2 a2 Ip E e' Ie' Ek. apply in_map_iff in Ie' as [p' [<- Ip']].
    assert (Ek0 : entry_key cS (snd p) = Some (mkey (app_subst cS r2) m)) by (rewrite E; reflexivity).
    rewrite (Upair p' p _ _ Ip' Ip Ek Ek0). exact E. }
  (* one pair: the family's run-time key at θ is the caller's *)
  assert (Core : forall p r1 m g1 a1, In p L -> fst p = NMth r1 m g1 a1 -> In (mkey (app_subst σ r1) m) keys ->
     exists r2 g2 a2, snd p = NMth r2 m g2 a2 /\
       In (mkey (app_subst cS r2) m) ks /\ mlook (app_subst cS r2) m (c_mths Cr) = Some (sig_subst cS g2) /\
       mlook (app_subst σ r1) m (c_mths outputs) = Some (sig_subst σ g1) /\
       app_subst_item th (rkey_of outputs (mkey (app_subst σ r1) m)) = rkey Cr (app_subst cS r2) m).
  { intros p r1 m g1 a1 Ip E Ik. destruct (Pm p r1 m g1 a1 Ip E) as (r2 & g2 & a2 & E2 & Rr & Sg & Is & Sc).
    assert (I1 : In (NMth r1 m g1 a1) (map fst L)) by (rewrite <- E; exact (in_map fst _ _ Ip)).
    assert (I2 : In (NMth r2 m g2 a2) (map snd L)) by (rewrite <- E2; exact (in_map snd _ _ Ip)).
    pose proof (result_lookup Cu T1 (map fst L) r1 m g1 a1 I1 (Un1 p r1 m g1 a1 Ip E)) as [M1 A1].
    pose proof (result_lookup Cf T2 (map snd L) r2 m g2 a2 I2) as R2. rewrite SubCf in R2.
    destruct (R2 (Un2 p r2 m g2 a2 Ip E2)) as [M2 A2]. clear R2.
    exists r2, g2, a2. split; [exact E2|]. split; [|split; [rewrite ECr; exact M2|split; [rewrite Eo; exact M1|]]].
    - rewrite EK2. apply keys_of_in. exists (NMth r2 m g2 a2). split; [exact I2|reflexivity].
    - assert (Km : kind_sym W m = Some DKMeth).
      { destruct (KeysM _ Ik) as (r0 & m0 & a0 & Ek & Km). injection Ek as _ <- _. exact Km. }
      change (rkey_of outputs (mkey (app_subst σ r1) m)) with (rkey outputs (app_subst σ r1) m). unfold rkey. rewrite <- Eo in A1. rewrite <- ECr in A2. change (c_sub Cu) with σ in A1. rewrite A1, A2.
      cbn [app_subst_item]. unfold theta_of, canon. rewrite SubO, SubR.
      rewrite th_sigma, Rr. f_equal. unfold map_snd. rewrite map_map. cbn.
      erewrite map_ext; [exact (known_rel a1 a2 (Is Km))|]. intros [y t]. cbn. rewrite th_sigma. reflexivity. }
  split.
  - intros r m g [Ik Mg]. rewrite EK2 in Ik. apply keys_of_in in Ik as [e2 [Ie2 Ek2]].
    apply in_map_iff in Ie2 as [p [Ep Ip]].
    destruct e2 as [?|?|?|r2 m2 g2 a2|?|? ?]; cbn in Ek2; try discriminate. injection Ek2 as Er ->.
    destruct (Pm2 p r2 m g2 a2 Ip Ep) as (r1 & g1 & a1 & E1).
    assert (Ik1 : In (mkey (app_subst σ r1) m) keys).
    { rewrite EK1. apply keys_of_in. exists (NMth r1 m g1 a1). split; [rewrite <- E1; exact (in_map fst _ _ Ip)|reflexivity]. }
    destruct (Core p r1 m g1 a1 Ip E1 Ik1) as (r2' & g2' & a2' & E2' & _ & M2 & M1 & Kr).
    rewrite Ep in E2'. injection E2' as <- <- <-. rewrite Er in M2, Kr. rewrite Mg in M2. injection M2 as ->.
    split; [exists (mkey (app_subst σ r1) m); split; [exact Ik1|exact Kr]|].
    intros key rf a gf Ik Ekey Mgf Kq.
    rewrite EK1 in Ik. apply keys_of_in in Ik as [e1 [Ie1 Ek1]]. apply in_map_iff in Ie1 as [p' [Ep' Ip']].
    rewrite Ekey in Ek1. destruct e1 as [?|?|?|r1' m1 g1' a1'|?|? ?]; cbn in Ek1; try discriminate.
    unfold mkey in Ek1. injection Ek1 as Erf Em <-. subst m1.
    assert (Ik1' : In (mkey (app_subst σ r1') m) keys).
    { rewrite EK1. apply keys_of_in. exists (NMth r1' m g1' a1'). split; [rewrite <- Ep'; exact (in_map fst _ _ Ip')|reflexivity]. }
    destruct (Core p' r1' m g1' a1' Ip' Ep' Ik1') as (r2' & g2' & a2' & E2' & _ & _ & M1' & Kr').
    subst key. rewrite <- Erf in Kq. change (mkey (app_subst σ r1') m) with (IMth (app_subst σ r1') m []) in Kr'. rewrite Kq in Kr'.
    assert (Er2 : app_subst cS r2' = r).
    { unfold rkey in Kr'. injection Kr' as Er2 _. symmetry. exact Er2. }
    assert (Pp : p' = p).
    { apply (Upair p' p r m Ip' Ip); [rewrite E2'; cbn; rewrite Er2; reflexivity|rewrite Ep; cbn; rewrite Er; reflexivity]. }
    subst p'. rewrite Ep' in E1. injection E1 as <- <- <-. rewrite <- Erf in Mgf. rewrite M1 in Mgf. injection Mgf as <-.
    destruct (Pm p r1' m g1' a1' Ip Ep') as (r2x & g2x & a2x & E2x & Rr & [Sp Sr] & _ & Sc).
    rewrite Ep in E2x. injection E2x as <- <- <-.
    assert (Sc' : forall t, In t (sg_ret g1' :: sg_params g1') -> incl (gens t) Dom).
    { intros t It y Iy. apply Sc. apply in_or_app. right. apply in_or_app. left. unfold sig_gens. apply in_flat_map. exists t. split; assumption. }
    rewrite <- Er, <- Erf. cbn [sig_subst sg_ret sg_params].
    constructor; [exact (sig_agree_of _ _ Sr (Sc' _ (or_introl eq_refl)))|].
    constructor; [exact (sig_agree_of _ _ Rr (fun y Iy => Sc y (in_or_app _ _ _ (or_introl Iy))))|].
    assert (ScP : forall t, In t (sg_params g1') -> incl (gens t) Dom) by (intros t It; exact (Sc' t (or_intror It))).
    revert Sp ScP. generalize (sg_params g1') as l1, (sg_params g2) as l2.
    induction l1 as [|t1 l1 IH]; intros [|t2 l2] Sp ScP; cbn in Sp; try discriminate; [constructor|].
    injection Sp as E Rest. constructor; [exact (sig_agree_of _ _ E (ScP t1 (or_introl eq_refl)))|].
    apply IH; [exact Rest|intros t It; exact (ScP t (or_intror It))].
  - intros key Ik. pose proof Ik as Ik0. rewrite EK1 in Ik. apply keys_of_in in Ik as [e1 [Ie1 Ek1]].
    apply in_map_iff in Ie1 as [p [Ep Ip]].
    destruct (KeysM key Ik0) as (r0 & m0 & a0 & -> & _).
    destruct e1 as [?|?|?|r1 m g1 a1|?|? ?]; cbn in Ek1; try discriminate. injection Ek1 as Erf <- Ea. subst a0 r0.
    destruct (Core p r1 m g1 a1 Ip Ep Ik0) as (r2 & g2 & a2 & _ & Ik2 & M2 & _ & Kr).
    exists (app_subst cS r2), m, (sig_subst cS g2). split; [split; assumption|exact Kr].
Qed.

(* Position by position, the family's run-time key at θ is the caller's
   (the caller-keyed install reads the family's item at position i for
   the caller's key at position i). *)
Theorem items_agree_pos : Forall2 (fun k key => app_subst_item th (rkey_of outputs key) = rkey_of Cr k) ks keys.
Proof.
  (* the caller's keys are methods: its reading has the family's entries' kinds *)
  assert (KsM : forall k, In k ks -> exists r m a, k = IMth r m a).
  { destruct fa_parts as [_ [_ [_ [_ PR]]]].
    destruct (PublicCallerKeys.provide_result_kinds W u Cf out2 Cr ks Cu (fun t => TAbs t) outputs keys PR PRf)
      as (e1 & e2 & E1 & E2 & Fk).
    pose proof (PublicCallerKeys.same_kind_keys (c_sub Cf) (c_sub Cu) _ _ Fk) as FK. rewrite <- E1, <- E2 in FK.
    intros k Ik. clear -FK Ik KeysM. revert k Ik KeysM. induction FK as [|x y l1 l2 Rxy _ IH]; intros k Ik KeysM; [contradiction|].
    destruct Ik as [<-|Ik]; [|exact (IH k Ik (fun key I => KeysM key (or_intror I)))].
    destruct (KeysM y (or_introl eq_refl)) as (r & m & a & -> & _). destruct x; try contradiction. eauto. }
  destruct entries_rel as (es1 & es2 & Eo & EK1 & ECr & EK2 & Fr).
  destruct (forall2_pairs _ _ _ _ _ Fr) as (L & -> & -> & FL).
  rewrite Forall_forall in FL.
  assert (SubO : c_sub outputs = σ) by (rewrite Eo; reflexivity).
  assert (SubR : c_sub Cr = cS).
  { rewrite ECr. cbn. unfold cS. exact SubCf0. }
  assert (SubCf : c_sub Cf = cS) by exact SubCf0.
  (* a method pair *)
  assert (Pm : forall p r1 m g1 a1, In p L -> fst p = NMth r1 m g1 a1 -> exists r2 g2 a2, snd p = NMth r2 m g2 a2 /\
     app_subst th r1 = app_subst cS r2 /\ PublicOutputSubst.SigRel th cS g1 g2 /\
     (kind_sym W m = Some DKMeth -> InstSome th cS a1 a2) /\ incl (gens r1 ++ sig_gens g1 ++ inst_gens a1) Dom).
  { intros p r1 m g1 a1 Ip E. pose proof (FL p Ip) as R. rewrite E in R.
    destruct (snd p) as [?|?|?|r2 m2 g2 a2|?|? ?]; try contradiction. destruct R as (<- & Rr & Sg & Is & Sc).
    exists r2, g2, a2. split; [reflexivity|exact (conj Rr (conj Sg (conj Is Sc)))]. }
  assert (Pm2 : forall p r2 m g2 a2, In p L -> snd p = NMth r2 m g2 a2 -> exists r1 g1 a1, fst p = NMth r1 m g1 a1).
  { intros p r2 m g2 a2 Ip E. pose proof (FL p Ip) as R. rewrite E in R.
    destruct (fst p) as [?|?|?|r1 m1 g1 a1|?|? ?]; try contradiction. destruct R as (-> & _). eauto. }
  (* the pairs are determined by their caller method keys: the caller's
     keys are distinct, or a caller key's method names one family key *)
  assert (Upair : forall p p' r m, In p L -> In p' L -> entry_key cS (snd p) = Some (mkey r m) ->
    entry_key cS (snd p') = Some (mkey r m) -> p = p').
  { intros p p' r m Ip Ip' E E'. destruct Uniq as [Nd|[Nd1 MD]].
    - rewrite EK2, prov_id_keys, keys_of_map in Nd. exact (keys_unique _ _ L Nd p p' _ Ip Ip' E E').
    - rewrite EK1, prov_id_keys, keys_of_map in Nd1.
      destruct (snd p) as [?|?|?|r2 m2 g2 a2|?|? ?] eqn:S2; cbn in E; try discriminate. injection E as _ Em. subst m2.
      destruct (snd p') as [?|?|?|r2' m2' g2' a2'|?|? ?] eqn:S2'; cbn in E'; try discriminate. injection E' as _ Em'. subst m2'.
      destruct (Pm2 p r2 m g2 a2 Ip S2) as (r1 & g1 & a1 & F1).
      destruct (Pm2 p' r2' m g2' a2' Ip' S2') as (r1' & g1' & a1' & F1').
      assert (I1 : In (mkey (app_subst σ r1) m) (keys_of (entry_key σ) (map fst L))).
      { apply keys_of_in. exists (NMth r1 m g1 a1). split; [rewrite <- F1; exact (in_map fst _ _ Ip)|reflexivity]. }
      assert (I1' : In (mkey (app_subst σ r1') m) (keys_of (entry_key σ) (map fst L))).
      { apply keys_of_in. exists (NMth r1' m g1' a1'). split; [rewrite <- F1'; exact (in_map fst _ _ Ip')|reflexivity]. }
      rewrite <- EK1 in I1, I1'.
      pose proof (MD _ _ _ _ _ _ _ _ I1 I1' eq_refl eq_refl (or_introl eq_refl)) as Ek.
      apply (keys_unique _ _ L Nd1 p p' (mkey (app_subst σ r1) m) Ip Ip').
      + rewrite F1. reflexivity.
      + rewrite F1', Ek. reflexivity. }
  (* each side's method key is unique *)
  assert (Key2 : forall p r1 m g1 a1, In p L -> fst p = NMth r1 m g1 a1 ->
     entry_key cS (snd p) = Some (mkey (app_subst th (app_subst σ r1)) m)).
  { intros p r1 m g1 a1 Ip E. destruct (Pm p r1 m g1 a1 Ip E) as (r2 & g2 & a2 & E2 & Rr & _).
    rewrite E2. cbn. rewrite th_sigma, Rr. reflexivity. }
  assert (Un1 : forall p r1 m g1 a1, In p L -> fst p = NMth r1 m g1 a1 ->
     forall e', In e' (map fst L) -> entry_key σ e' = Some (mkey (app_subst σ r1) m) -> e' = NMth r1 m g1 a1).
  { intros p r1 m g1 a1 Ip E e' Ie' Ek. apply in_map_iff in Ie' as [p' [<- Ip']].
    destruct (fst p') as [?|?|?|r1' m' g1' a1'|?|? ?] eqn:E'; cbn in Ek; try discriminate.
    injection Ek as Er ->. pose proof (Key2 p' r1' m g1' a1' Ip' E') as K'. rewrite Er in K'.
    rewrite (Upair p' p _ _ Ip' Ip K' (Key2 p r1 m g1 a1 Ip E)) in E'. congruence. }
  assert (Un2 : forall p r2 m g2 a2, In p L -> snd p = NMth r2 m g2 a2 ->
     forall e', In e' (map snd L) -> entry_key cS e' = Some (mkey (app_subst cS r2) m) -> e' = NMth r2 m g2 a2).
  { intros p r2 m g2 a2 Ip E e' Ie' Ek. apply in_map_iff in Ie' as [p' [<- Ip']].
    assert (Ek0 : entry_key cS (snd p) = Some (mkey (app_subst cS r2) m)) by (rewrite E; reflexivity).
    rewrite (Upair p' p _ _ Ip' Ip Ek Ek0). exact E. }
  (* one pair: the family's run-time key at θ is the caller's *)
  assert (Core : forall p r1 m g1 a1, In p L -> fst p = NMth r1 m g1 a1 -> In (mkey (app_subst σ r1) m) keys ->
     exists r2 g2 a2, snd p = NMth r2 m g2 a2 /\
       In (mkey (app_subst cS r2) m) ks /\ mlook (app_subst cS r2) m (c_mths Cr) = Some (sig_subst cS g2) /\
       mlook (app_subst σ r1) m (c_mths outputs) = Some (sig_subst σ g1) /\
       app_subst_item th (rkey_of outputs (mkey (app_subst σ r1) m)) = rkey Cr (app_subst cS r2) m).
  { intros p r1 m g1 a1 Ip E Ik. destruct (Pm p r1 m g1 a1 Ip E) as (r2 & g2 & a2 & E2 & Rr & Sg & Is & Sc).
    assert (I1 : In (NMth r1 m g1 a1) (map fst L)) by (rewrite <- E; exact (in_map fst _ _ Ip)).
    assert (I2 : In (NMth r2 m g2 a2) (map snd L)) by (rewrite <- E2; exact (in_map snd _ _ Ip)).
    pose proof (result_lookup Cu T1 (map fst L) r1 m g1 a1 I1 (Un1 p r1 m g1 a1 Ip E)) as [M1 A1].
    pose proof (result_lookup Cf T2 (map snd L) r2 m g2 a2 I2) as R2. rewrite SubCf in R2.
    destruct (R2 (Un2 p r2 m g2 a2 Ip E2)) as [M2 A2]. clear R2.
    exists r2, g2, a2. split; [exact E2|]. split; [|split; [rewrite ECr; exact M2|split; [rewrite Eo; exact M1|]]].
    - rewrite EK2. apply keys_of_in. exists (NMth r2 m g2 a2). split; [exact I2|reflexivity].
    - assert (Km : kind_sym W m = Some DKMeth).
      { destruct (KeysM _ Ik) as (r0 & m0 & a0 & Ek & Km). injection Ek as _ <- _. exact Km. }
      change (rkey_of outputs (mkey (app_subst σ r1) m)) with (rkey outputs (app_subst σ r1) m). unfold rkey. rewrite <- Eo in A1. rewrite <- ECr in A2. change (c_sub Cu) with σ in A1. rewrite A1, A2.
      cbn [app_subst_item]. unfold theta_of, canon. rewrite SubO, SubR.
      rewrite th_sigma, Rr. f_equal. unfold map_snd. rewrite map_map. cbn.
      erewrite map_ext; [exact (known_rel a1 a2 (Is Km))|]. intros [y t]. cbn. rewrite th_sigma. reflexivity. }
  rewrite EK1, EK2. apply keys_of_pairs. intros p Ip.
  destruct (fst p) as [t1|v1 t1 a1|f1 g1 a1|r1 m g1 a1|u1 fx1|eql eqr] eqn:E1.
  4: { destruct (Pm p r1 m g1 a1 Ip E1) as (r2 & g2 & a2 & E2 & _). rewrite E2. cbn [entry_key].
       split; [split; discriminate|]. intros k1 k2 K1 K2. injection K1 as <-. injection K2 as <-.
       assert (Ik1 : In (mkey (app_subst σ r1) m) keys).
       { rewrite EK1. apply keys_of_in. exists (NMth r1 m g1 a1). split; [rewrite <- E1; exact (in_map fst _ _ Ip)|reflexivity]. }
       destruct (Core p r1 m g1 a1 Ip E1 Ik1) as (r2' & g2' & a2' & E2' & _ & _ & _ & Kr).
       rewrite E2 in E2'. injection E2' as <- <- <-. exact Kr. }
  all: assert (N1 : entry_key σ (fst p) = None)
    by (destruct (entry_key σ (fst p)) as [k1|] eqn:K1; [|reflexivity];
        assert (Ik1 : In k1 keys) by (rewrite EK1; apply keys_of_in; exists (fst p); split; [exact (in_map fst _ _ Ip)|exact K1]);
        destruct (KeysM k1 Ik1) as (r & m & a & -> & _); rewrite E1 in K1; cbn in K1; discriminate).
  all: assert (N2 : entry_key cS (snd p) = None)
    by (pose proof (FL p Ip) as R0; rewrite E1 in R0;
        destruct (entry_key cS (snd p)) as [k2|] eqn:K2; [|reflexivity];
        assert (Ik2 : In k2 ks) by (rewrite EK2; apply keys_of_in; exists (snd p); split; [exact (in_map snd _ _ Ip)|exact K2]);
        destruct (KsM k2 Ik2) as (r & m & a & ->);
        destruct (snd p); cbn in R0, K2; try contradiction; discriminate).
  all: rewrite <- E1, N1, N2. all: split; [split; reflexivity|intros k1 k2 K1; discriminate].
Qed.

End FamilyAgree.

(* A family application (AuditFacts.FamilyApplication), as PlatformApply's
   family_result_agrees uses it. *)
Section FamilyAgreeFA.
Variable W : A.world.
Variables (C : A.cx) (u : nat) (matching th : subst) (sm : smap) (Cr : A.cx) (ks : list item).
Variables (Cu : A.cx) (needs : list item) (outputs : A.cx) (keys : list item).
Import A.

Hypothesis FA : AuditFacts.FamilyApplication W C u matching th sm Cr ks.
Hypothesis FCu : FunCtx W u Cu needs.
Hypothesis NoOut : forall t, ~ FunOutputs W u t.
Hypothesis PRf : ProvideResult W u Cu (fun t => TAbs t) outputs keys.
Hypothesis KeysM : forall key, In key keys -> exists r m a, key = IMth r m a /\ kind_sym W m = Some DKMeth.
Hypothesis Idem : forall t, canon C (canon C t) = canon C t.
(* An extra key of the family context's table that a path names is a type
   symbol (a table's keys are type symbols or generations). *)
Hypothesis KeysT : forall q pa x, resolve_sym W q pa = Some x ->
  nlook x (c_ty (family_context C matching)) <> None -> nlook x (c_ty Cu) = None -> kind_sym W x = Some DKType.

Theorem family_items_agree : ItemsAgree (family_context C matching) Cr Cu outputs th
  (fun r m g => In (mkey r m) ks /\ mlook r m (c_mths Cr) = Some g) keys.
Proof.
  destruct FA as (Cu' & ns & (fx & Fx & [Shape _] & Poly & FC & Th & InF & FAg & TypedNeeds & PR & Nd) & EmittedNeeds).
  pose proof (conj EmittedNeeds TypedNeeds) as Sat.
  destruct (functx_uniq W u _ _ _ _ FC FCu) as [-> ->].
  destruct Sat as [_ [_ [_ [_ Merge0]]]].
  pose proof (PublicOrientation.merges_by_pairs (family_context C matching) Cu (map fst (c_ty Cu))
    (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (functx_scoped W u Cu needs FCu)))))))
    (PublicOrientation.functx_sub_function W u Cu needs FCu) InF Merge0) as Merge.
  apply (items_agree_gen W C u th (family_context C matching) (fun t => TAbs t) Cr ks Cu needs outputs keys).
  - exact (proj1 (proj2 (PublicFormation.family_context_shape matching C))).
  - split; [exact Th|]. split; [exact InF|]. split; [exact Merge|]. split; [|exact PR].
    intros d ps a r b D FP. destruct Shape as (d' & n & ps' & a' & r' & D' & Ast & _).
    rewrite D in D'. injection D' as <-. rewrite Ast in FP. cbn in FP. injection FP as _ _ _ <-. reflexivity.
  - left. exact Nd.
  - exact FCu.
  - exact NoOut.
  - exact PRf.
  - exact KeysM.
  - exact Idem.
  - exact KeysT.
Qed.

(* The same, position by position. *)
Theorem family_items_pos : Forall2 (fun k key => app_subst_item th (rkey_of outputs key) = rkey_of Cr k) ks keys.
Proof.
  destruct FA as (Cu' & ns & (fx & Fx & [Shape _] & Poly & FC & Th & InF & FAg & TypedNeeds & PR & Nd) & EmittedNeeds).
  pose proof (conj EmittedNeeds TypedNeeds) as Sat.
  destruct (functx_uniq W u _ _ _ _ FC FCu) as [-> ->].
  destruct Sat as [_ [_ [_ [_ Merge0]]]].
  pose proof (PublicOrientation.merges_by_pairs (family_context C matching) Cu (map fst (c_ty Cu))
    (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (functx_scoped W u Cu needs FCu)))))))
    (PublicOrientation.functx_sub_function W u Cu needs FCu) InF Merge0) as Merge.
  apply (items_agree_pos W C u th (family_context C matching) (fun t => TAbs t) Cr ks Cu needs outputs keys).
  - exact (proj1 (proj2 (PublicFormation.family_context_shape matching C))).
  - split; [exact Th|]. split; [exact InF|]. split; [exact Merge|]. split; [|exact PR].
    intros d ps a r b D FP. destruct Shape as (d' & n & ps' & a' & r' & D' & Ast & _).
    rewrite D in D'. injection D' as <-. rewrite Ast in FP. cbn in FP. injection FP as _ _ _ <-. reflexivity.
  - left. exact Nd.
  - exact FCu.
  - exact NoOut.
  - exact PRf.
  - exact KeysM.
  - exact Idem.
  - exact KeysT.
Qed.

End FamilyAgreeFA.
