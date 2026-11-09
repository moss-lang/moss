(* The closure obligations of `bind F = G;` (BFunClosure, LinkBody).

   - `LinkMergesOk` and `BindMergesOk` (formerly of PublicBodySlice) were
     false for programs both implementations accept:
     examples/linked-merge-captured.moss (G merges the captured Y into the
     linked X) and examples/bind-merge-unbound.moss (G merges Y, not in
     force where G is bound, into Z). Audit's `application_environment`
     now reads G's inputs through G's table (`fc_table`), so G's merges
     hold by construction and both obligations are gone
     (scratch/orch/repair-AUDIT.md).
   - `CapturedInForceOk` (`captured_in_force_ok` below): a symbol a
     captured need mentions, among G's inputs F does not take, is in force
     where G is bound or mentioned by the context there
     (`captured_mentioned`). It may be a declared symbol not in force
     (examples/functor-captured-unbound.moss); the closure's
     `lexical_capture` keeps its meaning (scratch/orch/family4-AUDIT.md). *)
From Stdlib Require Import List Arith Bool.
From NM Require Import Audit PublicTypeRead PublicCallRealize PublicFormation.
From NM Require PublicValueTyping PublicBodySlice.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Static.
Variable W : A.world.
Import A.

(* A substitution's image of a mentioned symbol is mentioned. *)
Lemma gens_hit : forall t s y u, In y (gens t) -> lookup_subst y s = Some u -> incl (gens u) (gens (app_subst s t)).
Proof.
  fix IH 1. intros [m|fields| |g|n th|ms] s y u Iy L z Iz.
  - contradiction.
  - rewrite subst_record. cbn in Iy |- *. induction fields as [|[f t] rest Tail]; [contradiction|].
    cbn in Iy |- *. apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; exact (IH t s y u Iy L z Iz)|right; exact (Tail Iy)].
  - contradiction.
  - cbn in Iy. destruct Iy as [<-|[]]. cbn. rewrite L. exact Iz.
  - rewrite subst_nom. unfold PublicTypeRead.smap. cbn in Iy |- *. induction th as [|[x t] rest Tail]; [contradiction|].
    cbn in Iy |- *. apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; exact (IH t s y u Iy L z Iz)|right; exact (Tail Iy)].
  - rewrite subst_union. cbn in Iy |- *. induction ms as [|t rest Tail]; [contradiction|].
    cbn in Iy |- *. apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; exact (IH t s y u Iy L z Iz)|right; exact (Tail Iy)].
Qed.

Lemma ilook_in' : forall {X} k (l : list (item * X)) p, ilook k l = Some p -> In (k, p) l.
Proof.
  intros X k l p. induction l as [|[k' p'] rest IH]; intros L; [discriminate|].
  cbn in L. destruct (item_eqb k k') eqn:E; [apply item_eqb_spec in E; subst k'; injection L as ->; left; reflexivity|].
  right. exact (IH L).
Qed.

Lemma lookup_on_keys' : forall (f : nat -> ty) (l : list (nat * ty)) x, In x (map fst l) ->
  lookup_subst x (map (fun q => (fst q, f (fst q))) l) = Some (f x).
Proof.
  intros f l x. induction l as [|[a t] r IH]; intros I; [contradiction|]. cbn in I |- *.
  destruct (Nat.eqb_spec x a) as [->|Ne]; [reflexivity|]. destruct I as [E|I]; [congruence|exact (IH I)].
Qed.

Lemma ilook_c_apps : forall C k, ilook k (c_apps C) = option_map provision_inst (ilook k (c_provisions C)).
Proof.
  intros [ty sub provs locs] k. unfold c_apps. cbn. induction provs as [|[k' p] rest IH]; [reflexivity|].
  cbn. destruct (item_eqb k k'); [|exact IH]. destruct p; reflexivity.
Qed.

(* ---- the keys of a functor's context are declared types ---------------------- *)

Definition KeysTyped (tm : list (nat * ty)) : Prop := forall x, In x (map fst tm) -> kind_sym W x = Some DKType.

Lemma entries_keys_typed :
  (forall p ob tm sp es tm', ItemEntries W p ob tm sp es tm' -> KeysTyped tm -> KeysTyped tm') /\
  (forall p ob tm sps es tm', ItemsEntries W p ob tm sps es tm' -> KeysTyped tm -> KeysTyped tm').
Proof.
  apply (ies_mut W (fun p ob tm sp es tm' _ => KeysTyped tm -> KeysTyped tm')
                   (fun p ob tm sps es tm' _ => KeysTyped tm -> KeysTyped tm'));
    intros; try assumption.
  - (* type *) destruct (nlook t tm); [assumption|]. intros x [<-|Ix]; [assumption|auto].
  - (* context *) auto.
  - auto.
Qed.

Lemma functx_keys_typed : forall u C ns, MentionsTyped W -> FunCtx W u C ns ->
  forall x, In x (map fst (c_ty C)) -> kind_sym W x = Some DKType.
Proof.
  intros u C ns Mt (d & ps & a & r & b & es & tm & tm' & σ & implicit & D & FP & IE & Imp & -> & MGM & -> & _ & _) x Ix.
  cbn in Ix. rewrite map_map in Ix. cbn in Ix. rewrite map_app, map_map in Ix. cbn in Ix.
  apply in_app_or in Ix as [Ix|Ix].
  - rewrite map_id in Ix. destruct (proj1 (Imp x) Ix) as [_ [M _]]. exact (Mt u x M).
  - exact (proj2 entries_keys_typed _ _ _ _ _ _ IE (fun y (Iy : In y []) => match Iy with end) x Ix).
Qed.

Lemma kind_decl : forall x k, kind_sym W x = Some k -> w_decl W x <> None.
Proof. intros x k K E. unfold kind_sym in K. rewrite E in K. discriminate. Qed.

(* ---- CapturedInForceOk, at a bind site -------------------------------------- *)

Lemma context_gens_sub_key : forall C g u, In (g, u) (c_sub C) -> In g (context_gens C).
Proof.
  intros C g u Iq. unfold context_gens. apply in_or_app. left. apply in_flat_map. exists (g, u). split; [exact Iq|left; reflexivity].
Qed.

Lemma context_gens_sub : forall C g u y, In (g, u) (c_sub C) -> In y (gens u) -> In y (context_gens C).
Proof.
  intros C g u y Iq Iy. unfold context_gens. apply in_or_app. left. apply in_flat_map. exists (g, u). split; [exact Iq|right; exact Iy].
Qed.

Lemma context_gens_prov : forall C k p y, In (k, p) (c_provisions C) -> In y (provision_gens p) -> In y (context_gens C).
Proof.
  intros C k p y Iq Iy. unfold context_gens. apply in_or_app. right. apply in_or_app. right. apply in_or_app. left.
  apply in_flat_map. exists (k, p). split; [exact Iq|apply in_or_app; left; exact Iy].
Qed.

Lemma context_gens_scope : forall C y, In y (scope_gens C) -> In y (context_gens C).
Proof.
  intros C y Iy. unfold scope_gens in Iy. unfold context_gens. apply in_app_or in Iy as [Iy|Iy];
    apply in_or_app; [left; exact Iy|right; apply in_or_app; left; exact Iy].
Qed.

(* A type a provision of C has, read canonically, mentions only what C
   mentions. *)
Lemma canon_prov_gens : forall C k p t y, In (k, p) (c_provisions C) ->
  incl (gens t) (provision_gens p) -> In y (gens (canon C t)) -> In y (context_gens C).
Proof.
  intros C k p t y Iq Sc Iy. unfold canon in Iy. destruct (gens_subst t (c_sub C) y Iy) as [z [Iz [[_ ->]|[u [L Iu]]]]].
  - exact (context_gens_prov C k p z Iq (Sc z Iz)).
  - exact (context_gens_sub C z u y (lookup_subst_in_pair _ _ _ L) Iu).
Qed.

(* What a closure captures where it is bound (BFunClosure's 3b, read at
   G's inputs here): a symbol a captured need mentions, among G's inputs
   that F does not take, is in force here or mentioned by the context
   (its merge, provisions, or a method key's receiver). The closure's
   `lexical_capture` keeps both. *)
Theorem captured_mentioned : forall C gg Cu nsu Cg nsg smc,
  FunCtx W gg Cg nsg ->
  Satisfies C Cg (map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cg))
    (filter (fun k => negb (existsb (item_eqb (canon_item Cu k)) nsu)) nsg) smc ->
  forall k p x, In k (filter (fun k => negb (existsb (item_eqb (canon_item Cu k)) nsu)) nsg) -> ilook (provision_id k) (c_provisions Cg) = Some p ->
  In x (provision_gens p ++ match k with IMth r _ _ => gens r | _ => [] end)%list ->
  In x (map fst (c_ty Cg)) -> ~ In x (map fst (c_ty Cu)) ->
  nlook x (c_ty C) <> None \/ In x (context_gens C ++ key_receiver_gens C).
Proof.
  intros C gg Cu nsu Cg nsg smc FCg [_ [Prov [Coh [Types _]]]] k p x Ik Il Ix Ig _.
  destruct (nlook x (c_ty C)) as [tx|] eqn:Lx; [left; discriminate|right].
  set (th := map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cg)) in *.
  (* a merged-away x is mentioned by the merge *)
  destruct (lookup_subst x (c_sub C)) as [u|] eqn:Ns.
  { apply in_or_app. left. exact (context_gens_sub_key C x u (lookup_subst_in_pair _ _ _ Ns)). }
  (* so θ reads x as itself, and x survives a reading at θ *)
  assert (Th : lookup_subst x th = Some (TAbs x)).
  { pose proof (lookup_on_keys' (fun x => canon C (app_subst (c_ty C) (TAbs x))) _ x Ig) as E. cbn beta in E.
    unfold th. etransitivity; [exact E|]. cbn [app_subst]. rewrite lookup_subst_nlook', Lx. cbn.
    unfold canon. cbn. rewrite Ns. reflexivity. }
  assert (Keep : forall t, In x (gens t) -> In x (gens (canon C (app_subst th t)))).
  { intros t It. unfold canon. apply gens_keep; [|exact Ns]. exact (gens_hit t th x (TAbs x) It Th x (or_introl eq_refl)). }
  assert (Nk : In k nsg) by (apply filter_In in Ik as [I _]; exact I).
  destruct (functx_scoped W gg Cg nsg FCg) as [_ [_ [_ [_ [_ [ShapeG _]]]]]].
  assert (Pid : provision_id k = k) by (pose proof (ShapeG k Nk) as Sh; destruct k; cbn in Sh |- *; congruence).
  rewrite Pid in Il.
  destruct (Types k p Ik Il) as [p' [Il' Sig]].
  pose proof (ilook_in' _ _ _ Il') as Ip'.
  apply in_app_or in Ix as [Ix|Ix]; [apply in_or_app; left|].
  - unfold provision_gens in Ix. apply in_app_or in Ix as [Ix|Ix].
    + (* a type of its signature *)
      destruct (provision_signature p) as [g|] eqn:Sp; [|contradiction].
      destruct (provision_signature p') as [g'|] eqn:Sp'; [|discriminate].
      destruct g as [ps r], g' as [ps' r']. unfold canon_sig, sig_at in Sig. cbn in Sig, Ix.
      injection Sig as Eps Er.
      assert (Ix' : exists t, In t (r :: ps) /\ In x (gens t)).
      { apply in_app_or in Ix as [Ix|Ix]; [exists r; split; [left; reflexivity|exact Ix]|].
        apply in_flat_map in Ix as [t [It Ixt]]. exists t. split; [right; exact It|exact Ixt]. }
      destruct Ix' as [t [It Ixt]].
      assert (Ex : exists t', In t' (r' :: ps') /\ canon C t' = canon C (app_subst th t)).
      { destruct It as [<-|It]; [exists r'; split; [left; reflexivity|exact Er]|].
        assert (Im : In (canon C (app_subst th t)) (map (canon C) ps'))
          by (rewrite Eps; apply in_map_iff; exists t; split; [reflexivity|exact It]).
        apply in_map_iff in Im as [t' [E It']]. exists t'. split; [right; exact It'|exact E]. }
      destruct Ex as [t' [It' Et']].
      apply (canon_prov_gens C _ p' t' x Ip').
      * intros z Iz. unfold provision_gens. rewrite Sp'. apply in_or_app. left. cbn.
        destruct It' as [<-|It']; apply in_or_app; [left; exact Iz|right; apply in_flat_map; exists t'; split; assumption].
      * rewrite Et'. exact (Keep t Ixt).
    + (* its recorded instantiation *)
      apply in_flat_map in Ix as [[y [want|]] [Iy Ixw]]; [|contradiction]. cbn in Ixw.
      assert (Ia : ilook k (c_apps Cg) = Some (provision_inst p)) by (rewrite ilook_c_apps, Il; reflexivity).
      pose proof (Coh k _ Ik Ia y want Iy) as Have.
      rewrite ilook_c_apps, Il' in Have. cbn in Have.
      apply (context_gens_prov C _ p' x Ip'). unfold provision_gens. apply in_or_app. right.
      apply in_flat_map. exists (y, Some (canon C (app_subst th want))). split; [exact Have|exact (Keep want Ixw)].
  - (* a method's receiver *)
    destruct k as [t|v|f|r m a|u]; try contradiction. cbn in Il'.
    apply in_or_app. right. unfold key_receiver_gens. apply in_flat_map.
    eexists. split; [exact (ilook_in' _ _ _ Il')|]. cbn. exact (Keep r Ix).
Qed.

End Static.

(* ---- CapturedInForceOk ------------------------------------------------------ *)

Module LinkMerges (Prims : PRIMITIVES) (I0 : PublicValueTyping.INTERP Prims).
Module S := PublicBodySlice.BodySlice Prims I0.

Theorem captured_in_force_ok : forall W, S.CapturedInForceOk W.
Proof.
  intros W C gg Cu nsu Cg nsg smc FCg Sat k p x Ik Il Ix Ig Ou.
  exact (captured_mentioned W C gg Cu nsu Cg nsg smc FCg Sat k p x Ik Il Ix Ig Ou).
Qed.
End LinkMerges.
