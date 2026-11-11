(* Realizing a callee's context at a call (T03/T04). A function's context
   maps each of its type symbols to that symbol's canonical form, and its
   merge substitution and entries mention only those symbols. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead PublicTypeFacts.
From NM Require PublicSubstFacts PublicMerge.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Formation.
Variable W : A.world.
Import A.

Definition IdForm (tm : list (nat * ty)) : Prop := Forall (fun q => snd q = TAbs (fst q)) tm.

Lemma idform_gens : forall tm g, IdForm tm -> In g (tm_gens tm) -> In g (map fst tm).
Proof.
  intros tm g Id Ig. unfold tm_gens in Ig. apply in_flat_map in Ig as [[k v] [Iq Ig]].
  unfold IdForm in Id. rewrite Forall_forall in Id. pose proof (Id (k, v) Iq) as E. cbn in E, Ig. subst v.
  cbn in Ig. destruct Ig as [<-|[]]. change k with (fst (k, TAbs k)). apply in_map, Iq.
Qed.

Lemma idform_nlook : forall tm k v, IdForm tm -> nlook k tm = Some v -> v = TAbs k.
Proof.
  intros tm k v Id Found. unfold IdForm in Id. induction Id as [|[a u] rest E Rest IH]; [discriminate|].
  cbn in E, Found. destruct (Nat.eqb_spec k a) as [->|_]; [injection Found as <-; exact E|exact (IH Found)].
Qed.

Definition Scoped (tm : list (nat * ty)) (gs : list nat) : Prop := forall g, In g gs -> In g (map fst tm).

Scheme ie_ind' := Induction for ItemEntries Sort Prop
with ies_ind' := Induction for ItemsEntries Sort Prop.
Combined Scheme ies_mut from ie_ind', ies_ind'.

Lemma reading_scoped : forall ob tm g,
  IdForm tm -> Scoped tm (tm_gens ob) -> In g (tm_gens (ob ++ tm)) -> In g (map fst tm).
Proof.
  intros ob tm g Id Ob Ig. apply tm_gens_app in Ig as [In'|In']; [exact (Ob g In')|exact (idform_gens tm g Id In')].
Qed.

Lemma brackets_scoped : forall p ob tm s a bs,
  IdForm tm -> Scoped tm (tm_gens ob) -> Brackets W p (ob ++ tm) s a bs -> Scoped tm (tm_gens bs).
Proof.
  intros p ob tm s a bs Id Ob B g Ig. exact (reading_scoped ob tm g Id Ob (br_gens W _ _ _ _ _ B g Ig)).
Qed.

Lemma full_scoped : forall bs ob tm g,
  IdForm tm -> Scoped tm (tm_gens ob) -> Scoped tm (tm_gens bs) ->
  In g (tm_gens (bs ++ ob ++ tm)) -> In g (map fst tm).
Proof.
  intros bs ob tm g Id Ob Bs Ig. apply tm_gens_app in Ig as [In'|In']; [exact (Bs g In')|].
  exact (reading_scoped ob tm g Id Ob In').
Qed.

Lemma appof_scoped : forall s tm a, AppOf W s tm a -> forall g, In g (inst_gens a) -> In g (tm_gens tm).
Proof.
  intros s tm a [rs [-> _]] g Ig. unfold inst_gens in Ig. apply in_flat_map in Ig as [[r o] [Iq Ig]].
  apply in_map_iff in Iq as [r' [E _]]. injection E as -> <-. cbn in Ig.
  destruct (nlook r tm) eqn:Found; [|contradiction]. exact (nlook_gens _ _ _ _ Found Ig).
Qed.

Lemma tyof_scoped : forall p tm0 (tm : list (nat * ty)) a t g,
  TyOf W p tm0 None a t -> (forall g, In g (tm_gens tm0) -> In g (map fst tm)) ->
  In g (gens t) -> In g (map fst tm).
Proof.
  intros p tm0 tm a t g T Sc Ig. destruct (tyof_gens W _ _ _ _ _ T g Ig) as [In'|[x [E _]]]; [exact (Sc g In')|discriminate].
Qed.

Lemma sigof_scoped : forall s tm0 (tm : list (nat * ty)) this g sg,
  SigOf W s tm0 this sg -> (forall g, In g (tm_gens tm0) -> In g (map fst tm)) ->
  (forall x, this = Some x -> forall g, In g (gens x) -> In g (map fst tm)) ->
  In g (sig_gens sg) -> In g (map fst tm).
Proof.
  intros s tm0 tm this g [ps r] (d & rcv & n & args & rt & body & D & Ast & Ps & R) Sc This Ig.
  cbn [sg_ret sg_params] in Ps, R. unfold sig_gens in Ig. cbn [sg_ret sg_params flat_map] in Ig. apply in_app_or in Ig as [Ir|Ip].
  - destruct rt as [rta|]; cbn in R.
    + destruct (tyof_gens W _ _ _ _ _ R g Ir) as [In'|[x [E Ix]]]; [exact (Sc g In')|exact (This x E g Ix)].
    + subst r. cbn in Ir. contradiction.
  - apply in_flat_map in Ip as [t [It Ig]]. cbn in It.
    clear R D Ast. revert It. induction Ps as [|pt t' ps' ts' Ht Fr IH]; intros It; [contradiction|].
    destruct It as [<-|It]; [|exact (IH It)].
    destruct (tyof_gens W _ _ _ _ _ Ht g Ig) as [In'|[x [E Ix]]]; [exact (Sc g In')|exact (This x E g Ix)].
Qed.

Lemma recv_scoped : forall r tm0 (tm : list (nat * ty)) rt,
  RecvTy W r tm0 rt -> (forall g, In g (tm_gens tm0) -> In g (map fst tm)) ->
  forall g, In g (gens rt) -> In g (map fst tm).
Proof.
  intros r tm0 tm rt Recv Sc g Ig. unfold RecvTy in Recv.
  assert (Nom : forall args, Forall2 (fun x xa => fst xa = x /\ nlook x tm0 = Some (snd xa)) (w_reqs W r) args ->
    In g (gens (TNom r args)) -> In g (map fst tm)).
  { intros args F Ia. cbn in Ia. clear Recv Ig. induction F as [|x xa xs xas [_ L] Fr IH]; [contradiction|].
    cbn in Ia. apply in_app_or in Ia as [Here|There]; [exact (Sc g (nlook_gens _ _ _ _ L Here))|exact (IH There)]. }
  destruct (nlook r tm0) as [t'|] eqn:Found; [subst rt; exact (Sc g (nlook_gens _ _ _ _ Found Ig))|].
  destruct (w_decl W r) as [d|].
  - destruct (d_ast d); try (destruct Recv as [_ [args [-> F]]]; exact (Nom args F Ig)).
    exact (tyof_scoped _ _ _ _ _ _ Recv Sc Ig).
  - destruct Recv as [args [-> F]]. exact (Nom args F Ig).
Qed.

Lemma add_tys_formed : forall xs tm, IdForm tm ->
  IdForm (add_tys xs tm) /\ incl (map fst tm) (map fst (add_tys xs tm)) /\ incl xs (map fst (add_tys xs tm)).
Proof.
  induction xs as [|x rest IH]; intros tm Id; cbn.
  { split; [exact Id|]. split; [intros y Iy; exact Iy|intros y []]. }
  destruct (nlook x tm) as [v|] eqn:Found.
  - destruct (IH tm Id) as [Id' [Sub Xs]]. split; [exact Id'|]. split; [exact Sub|].
    intros y [E|Iy]; [subst y|exact (Xs y Iy)]. apply Sub. apply nlook_in in Found.
    change x with (fst (x, v)). apply in_map, Found.
  - assert (Id1 : IdForm ((x, TAbs x) :: tm)) by (constructor; [reflexivity|exact Id]).
    destruct (IH _ Id1) as [Id' [Sub Xs]]. split; [exact Id'|]. split.
    + intros y Iy. apply Sub. right. exact Iy.
    + intros y [<-|Iy]; [apply Sub; left; reflexivity|exact (Xs y Iy)].
Qed.

Lemma entries_formed :
  (forall p ob tm sp es tm', ItemEntries W p ob tm sp es tm' ->
     IdForm tm -> Scoped tm (tm_gens ob) ->
     IdForm tm' /\ incl (map fst tm) (map fst tm') /\ Scoped tm' (flat_map entry_gens es)) /\
  (forall p ob tm sps es tm', ItemsEntries W p ob tm sps es tm' ->
     IdForm tm -> Scoped tm (tm_gens ob) ->
     IdForm tm' /\ incl (map fst tm) (map fst tm') /\ Scoped tm' (flat_map entry_gens es)).
Proof.
  apply (ies_mut W
    (fun p ob tm sp es tm' _ => IdForm tm -> Scoped tm (tm_gens ob) ->
       IdForm tm' /\ incl (map fst tm) (map fst tm') /\ Scoped tm' (flat_map entry_gens es))
    (fun p ob tm sps es tm' _ => IdForm tm -> Scoped tm (tm_gens ob) ->
       IdForm tm' /\ incl (map fst tm) (map fst tm') /\ Scoped tm' (flat_map entry_gens es))).
  - (* type *) intros p ob tm pa t R K Id Ob.
    destruct (nlook t tm) as [v|] eqn:Found.
    + split; [exact Id|]. split; [intros x Ix; exact Ix|].
      intros g Ig. cbn in Ig. destruct Ig as [<-|[]]. apply nlook_in in Found.
      change t with (fst (t, v)). apply in_map, Found.
    + split; [constructor; [reflexivity|exact Id]|]. split; [intros x Ix; right; exact Ix|].
      intros g Ig. cbn in Ig. destruct Ig as [<-|[]]. left. reflexivity.
  - (* nothing *) intros p ob tm pa s k R K Kind Id Ob. split; [exact Id|]. split; [intros x Ix; exact Ix|].
    intros g []. 
  - (* val *) intros p ob tm a pa s d n t ty0 bs ap R D Ast B T App Id Ob.
    pose proof (brackets_scoped p ob tm s a bs Id Ob B) as Bs.
    split; [exact Id|]. split; [intros x Ix; exact Ix|].
    intros g Ig. cbn in Ig. rewrite app_nil_r in Ig. apply in_app_or in Ig as [Ig|Ig].
    + exact (tyof_scoped _ _ _ _ _ _ T (fun g' I' => full_scoped bs ob tm g' Id Ob Bs I') Ig).
    + exact (full_scoped bs ob tm g Id Ob Bs (appof_scoped _ _ _ App g Ig)).
  - (* fn *) intros p ob tm a pa s sg bs ap R K B Sig App Id Ob.
    pose proof (brackets_scoped p ob tm s a bs Id Ob B) as Bs.
    split; [exact Id|]. split; [intros x Ix; exact Ix|].
    intros g Ig. cbn in Ig. rewrite app_nil_r in Ig. apply in_app_or in Ig as [Ig|Ig].
    + exact (sigof_scoped _ _ _ _ _ _ Sig (fun g' I' => full_scoped bs ob tm g' Id Ob Bs I')
        (fun x E => ltac:(discriminate E)) Ig).
    + exact (full_scoped bs ob tm g Id Ob Bs (appof_scoped _ _ _ App g Ig)).
  - (* functor *) intros p ob tm pa u d n ps args res fx R D Ast Fixed FR Id Ob.
    split; [exact Id|]. split; [intros x Ix; exact Ix|].
    intros g Ig. cbn in Ig. rewrite app_nil_r in Ig. apply in_flat_map in Ig as [[x tx] [Iq Ig]].
    destruct (proj1 (FR x tx) Iq) as [_ L]. exact (idform_gens tm g Id (nlook_gens _ _ _ _ L Ig)).
  - (* method *) intros p ob tm a pa dn r m rt sg bs ap R MN B Recv Sig App Id Ob.
    pose proof (brackets_scoped p ob tm m a bs Id Ob B) as Bs.
    pose proof (fun g' I' => full_scoped bs ob tm g' Id Ob Bs I') as Sc.
    pose proof (recv_scoped r _ tm rt Recv Sc) as Rt.
    split; [exact Id|]. split; [intros x Ix; exact Ix|].
    intros g Ig. cbn in Ig. rewrite app_nil_r in Ig. apply in_app_or in Ig as [Ig|Ig]; [exact (Rt g Ig)|].
    apply in_app_or in Ig as [Ig|Ig].
    + exact (sigof_scoped _ _ _ _ _ _ Sig Sc (fun x E => ltac:(injection E as <-; exact Rt)) Ig).
    + exact (Sc g (appof_scoped _ _ _ App g Ig)).
  - (* context *) intros p ob tm a pa c d n its bs es tm' R D Ast B Rest IH Id Ob.
    pose proof (brackets_scoped p ob tm c a bs Id Ob B) as Bs.
    apply IH; [exact Id|]. intros g Ig. apply tm_gens_app in Ig as [Ig|Ig]; [exact (Bs g Ig)|exact (Ob g Ig)].
  - intros p ob tm Id Ob. split; [exact Id|]. split; [intros x Ix; exact Ix|]. intros g [].
  - intros p ob tm sp r es1 tm1 es2 tm2 H1 IH1 H2 IH2 Id Ob.
    destruct (IH1 Id Ob) as [Id1 [Sub1 Es1]].
    destruct (IH2 Id1 (fun g Ig => Sub1 g (Ob g Ig))) as [Id2 [Sub2 Es2]].
    split; [exact Id2|]. split; [intros x Ix; exact (Sub2 x (Sub1 x Ix))|].
    intros g Ig. rewrite flat_map_app in Ig. apply in_app_or in Ig as [Ig|Ig]; [exact (Sub2 g (Es1 g Ig))|exact (Es2 g Ig)].
  - (* equation *) intros p ob tm l r es1 tm1 tl tr rest es2 tm2 H1 IH1 Tl Tr H2 IH2 Id Ob.
    destruct (IH1 Id Ob) as [Id1 [Sub1 Es1]].
    assert (Ob1 : Scoped tm1 (tm_gens ob)) by (intros g Ig; exact (Sub1 g (Ob g Ig))).
    destruct (IH2 Id1 Ob1) as [Id2 [Sub2 Es2]].
    split; [exact Id2|]. split; [intros x Ix; exact (Sub2 x (Sub1 x Ix))|].
    intros g Ig. rewrite flat_map_app in Ig. apply in_app_or in Ig as [Ig|Ig]; [exact (Sub2 g (Es1 g Ig))|].
    cbn [flat_map entry_gens] in Ig. apply in_app_or in Ig as [Ig|Ig]; [|exact (Es2 g Ig)].
    apply in_app_or in Ig as [Ig|Ig]; apply Sub2.
    + exact (tyof_scoped _ _ _ _ _ _ Tl (fun g' I' => reading_scoped ob _ g' Id1 Ob1 I') Ig).
    + exact (tyof_scoped _ _ _ _ _ _ Tr (fun g' I' => reading_scoped ob _ g' Id1 Ob1 I') Ig).
Qed.
End Formation.

Section Contexts.
Variable W : A.world.
Import A.

Lemma root_formed : forall q es0 tm its es tm', RootItems W q es0 tm its es tm' ->
  IdForm tm -> IdForm tm' /\ incl (map fst tm) (map fst tm') /\ Scoped tm' (flat_map entry_gens es).
Proof.
  intros q es0 tm its es tm' Root. induction Root as
    [es0 tm|es0 tm p a rest u du n ps args res xs fx es tm' R D Ast Fresh Nd Xs FR Rest IH|es0 tm sp rest es tm' Skip Rest IH];
    intros Id.
  - split; [exact Id|]. split; [intros x Ix; exact Ix|]. intros g [].
  - destruct (add_tys_formed xs tm Id) as [IdA [SubA XsA]].
    destruct (IH IdA) as [Id' [Sub' Es']]. split; [exact Id'|]. split; [intros x Ix; exact (Sub' x (SubA x Ix))|].
    intros g Ig. rewrite flat_map_app in Ig. apply in_app_or in Ig as [Ig|Ig].
    + apply in_flat_map in Ig as [e [Ie Ig]]. apply in_map_iff in Ie as [x [<- Ix]]. cbn in Ig.
      destruct Ig as [E|[]]. subst g. exact (Sub' x (XsA x Ix)).
    + cbn in Ig. apply in_app_or in Ig as [Ig|Ig]; [|exact (Es' g Ig)].
      apply in_flat_map in Ig as [[x tx] [Iq Ig]].
      destruct (proj1 (FR x tx) Iq) as [_ L]. exact (Sub' g (idform_gens _ g IdA (nlook_gens _ _ _ _ L Ig))).
  - exact (IH Id).
Qed.

(* A function's context: each type symbol means its canonical self, and
   the merge maps only its type symbols, to types over them. *)
Theorem ctxof_formed : forall f Cf ns, CtxOf W f Cf ns ->
  (forall k v, nlook k (c_ty Cf) = Some v -> v = app_subst (c_sub Cf) (TAbs k)) /\
  (forall g t, In (g, t) (c_sub Cf) -> In g (map fst (c_ty Cf)) /\ incl (gens t) (map fst (c_ty Cf))).
Proof.
  intros f Cf ns (d & es & tm & er & tmr & σ & classes & D & IE & Root & MGM & Merge & Section & -> & _ & _).
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT [_ EsT]].
  assert (Tmr : IdForm tmr /\ incl (map fst tm) (map fst tmr) /\ Scoped tmr (flat_map entry_gens er)).
  { destruct Root as [_|gc d0 n0 its er' tmr' _ _ _ _ RI].
    - split; [exact IdT|]. split; [intros x Ix; exact Ix|]. intros g [].
    - exact (root_formed _ _ _ _ _ _ RI IdT). }
  destruct Tmr as [IdR [SubR ErR]].
  assert (Keys : map fst (c_ty (entries_cx σ classes tmr (es ++ er))) = map fst tmr).
  { cbn. rewrite map_map. reflexivity. }
  assert (All : Scoped tmr (flat_map entry_gens (es ++ er))).
  { intros x Ix. rewrite flat_map_app in Ix. apply in_app_or in Ix as [Ix|Ix]; [exact (SubR x (EsT x Ix))|exact (ErR x Ix)]. }
  assert (Classes : incl (merge_symbols classes) (map fst tmr)).
  { intros x Ix. apply All. exact (proj1 (proj2 (proj2 Merge)) x Ix). }
  split.
  - intros k v Found. cbn in Found. change (map _ tmr) with (PublicTypeRead.smap σ tmr) in Found.
    rewrite nlook_smap in Found. destruct (nlook k tmr) as [u|] eqn:L; [|discriminate].
    injection Found as <-. rewrite (idform_nlook _ _ _ IdR L). reflexivity.
  - intros g t Ig. rewrite Keys. cbn in Ig.
    pose proof (proj1 (proj1 (proj2 (proj2 (proj2 MGM))) g t Ig)) as Gg.
    assert (Inside : In g (map fst tmr)) by exact (All g Gg).
    split; [exact Inside|].
    assert (Read : app_subst σ (TAbs g) = t).
    { cbn [app_subst]. rewrite (proj2 (proj2 (proj2 (proj2 MGM))) g t Ig). reflexivity. }
    rewrite <- Read. apply (PublicMerge.section_gens_bound Section Classes).
    intros x [<-|[]]. exact Inside.
Qed.

End Contexts.

Section Calls.
Variable W : A.world.
Import A.

(* What the call lemmas read of a callee's context: its table is canonical
   self-reference and its merge maps only its keys, to types over them. *)
Definition TableFormed (Cf : cx) : Prop :=
  (forall k v, nlook k (c_ty Cf) = Some v -> v = app_subst (c_sub Cf) (TAbs k)) /\
  (forall g t, In (g, t) (c_sub Cf) -> In g (map fst (c_ty Cf)) /\ incl (gens t) (map fst (c_ty Cf))).


Lemma ctx_table_formed : forall f Cf ns, CtxOf W f Cf ns -> TableFormed Cf.
Proof. intros f Cf ns Ctx. exact (ctxof_formed W f Cf ns Ctx). Qed.

Definition callee_meanings (d : environment) (th : subst) : subst :=
  map (fun q => (fst q, ground d (snd q))) th.

Lemma lookup_subst_in : forall x (th : subst), In x (map fst th) -> lookup_subst x th <> None.
Proof. exact PublicSubstFacts.lookup_subst_in. Qed.

Lemma lookup_subst_nlook' : forall k (s : subst), lookup_subst k s = nlook k s.
Proof. intros k s; induction s as [|[a t] r IH]; cbn; [reflexivity|rewrite IH; reflexivity]. Qed.

Lemma lookup_subst_in_pair : forall x (s : subst) t, lookup_subst x s = Some t -> In (x, t) s.
Proof. intros x s t L. rewrite lookup_subst_nlook' in L. exact (nlook_in _ _ _ _ L). Qed.

(* The callee's view of a type over its own symbols is the caller's view
   of that type read at θ. *)
Lemma callee_ground : forall d th t,
  (forall x, In x (gens t) -> lookup_subst x th <> None) ->
  app_subst (callee_meanings d th) t = ground d (app_subst th t).
Proof. intros d th t Dom. exact (subst_through t (type_meanings d) th Dom). Qed.

Lemma merge_image_dom_tf : forall Cf th x,
  TableFormed Cf -> map fst th = map fst (c_ty Cf) ->
  In x (map fst (c_ty Cf)) -> forall y, In y (gens (app_subst (c_sub Cf) (TAbs x))) -> lookup_subst y th <> None.
Proof.
  intros Cf th x [_ Dom] Keys Ix y Iy. apply lookup_subst_in. rewrite Keys.
  cbn in Iy.
  destruct (lookup_subst x (c_sub Cf)) as [t|] eqn:L.
  - apply lookup_subst_in_pair in L. exact (proj2 (Dom x t L) y Iy).
  - destruct Iy as [<-|[]]. exact Ix.
Qed.

Lemma merged_meaning_tf : forall f Cf C th d x,
  TableFormed Cf -> CallTheta W C Cf f th ->
  (forall t, ground d (canon C t) = ground d t) ->
  In x (map fst (c_ty Cf)) ->
  app_subst (callee_meanings d th) (app_subst (c_sub Cf) (TAbs x)) = ground d (app_subst th (TAbs x)).
Proof.
  intros f Cf C th d x Tf [Keys [_ Merges]] Abs Ix.
  rewrite callee_ground by exact (merge_image_dom_tf Cf th x Tf Keys Ix).
  replace (app_subst (c_sub Cf) (TAbs x)) with
    (match lookup_subst x (c_sub Cf) with Some t => t | None => TAbs x end) by reflexivity.
  destruct (lookup_subst x (c_sub Cf)) as [t|] eqn:L; [|reflexivity].
  apply lookup_subst_in_pair in L. rewrite <- (Abs (app_subst th t)), <- (Merges x t L), Abs. reflexivity.
Qed.

Theorem call_agree_tf : forall f Cf C th d,
  TableFormed Cf -> CallTheta W C Cf f th ->
  (forall t, ground d (canon C t) = ground d t) ->
  Agree (callee_meanings d th) (type_meanings d) (c_ty Cf) (c_ty C).
Proof.
  intros f Cf C th d Tf CT Abs k v1 v2 L1 L2.
  pose proof Tf as [Form _]. rewrite (Form k v1 L1).
  assert (Ik : In k (map fst (c_ty Cf))) by (apply nlook_in in L1; change k with (fst (k, v1)); apply in_map, L1).
  rewrite (merged_meaning_tf f Cf C th d k Tf CT Abs Ik).
  destruct CT as [Keys [Bound _]].
  assert (Th : lookup_subst k th = Some (canon C v2)).
  { rewrite (Bound k Ik ltac:(congruence)). cbn. rewrite lookup_subst_nlook', L2. reflexivity. }
  cbn [app_subst]. rewrite Th. change (app_subst (type_meanings d) v2) with (ground d v2).
  rewrite <- (Abs v2). reflexivity.
Qed.

Lemma merge_image_dom : forall f Cf ns th x,
  CtxOf W f Cf ns -> map fst th = map fst (c_ty Cf) ->
  In x (map fst (c_ty Cf)) -> forall y, In y (gens (app_subst (c_sub Cf) (TAbs x))) -> lookup_subst y th <> None.
Proof. intros f Cf ns th x Ctx. exact (merge_image_dom_tf Cf th x (ctx_table_formed f Cf ns Ctx)). Qed.

Lemma merged_meaning : forall f Cf ns C th d x,
  CtxOf W f Cf ns -> CallTheta W C Cf f th ->
  (forall t, ground d (canon C t) = ground d t) ->
  In x (map fst (c_ty Cf)) ->
  app_subst (callee_meanings d th) (app_subst (c_sub Cf) (TAbs x)) = ground d (app_subst th (TAbs x)).
Proof. intros f Cf ns C th d x Ctx. exact (merged_meaning_tf f Cf C th d x (ctx_table_formed f Cf ns Ctx)). Qed.

Theorem call_agree : forall f Cf ns C th d,
  CtxOf W f Cf ns -> CallTheta W C Cf f th ->
  (forall t, ground d (canon C t) = ground d t) ->
  Agree (callee_meanings d th) (type_meanings d) (c_ty Cf) (c_ty C).
Proof. intros f Cf ns C th d Ctx. exact (call_agree_tf f Cf C th d (ctx_table_formed f Cf ns Ctx)). Qed.

End Calls.

(* ---- provision tables ------------------------------------------------------ *)

Lemma subst_eqb_spec : forall a b, subst_eqb a b = true <-> a = b.
Proof.
  induction a as [|[x t] r IH]; intros [|[y u] r']; cbn; try (split; congruence).
  rewrite !andb_true_iff, Nat.eqb_eq, ty_eqb_spec, IH.
  split; [intros [[E1 E2] E3]; subst; reflexivity|intros E; inversion E; subst; auto].
Qed.

Lemma item_eqb_spec : forall a b, item_eqb a b = true <-> a = b.
Proof.
  intros [t|v|f|r m th|u] [t'|v'|f'|r' m' th'|u']; cbn; try (split; congruence);
    try (rewrite Nat.eqb_eq; split; congruence).
  rewrite !andb_true_iff, ty_eqb_spec, Nat.eqb_eq, subst_eqb_spec.
  split; [intros [[E1 E2] E3]; subst; reflexivity|intros E; inversion E; subst; auto].
Qed.

Section Tables.
Import A.

(* Every provision is filed under a key of its own kind. *)
Definition KeysMatch (provs : list (item * provision)) : Prop :=
  forall k p, In (k, p) provs ->
  match k, p with
  | IVal _, ProvidedVal _ _ | IFn _, ProvidedFn _ _ | IFun _, ProvidedFunctor _ => True
  | IMth _ _ [], ProvidedMethod _ _ => True
  | _, _ => False
  end.

Lemma keys_match_tail : forall q provs, KeysMatch (q :: provs) -> KeysMatch provs.
Proof. intros q provs K k p I; exact (K k p (or_intror I)). Qed.

Lemma vals_ilook : forall provs v t, KeysMatch provs ->
  nlook v (flat_map (fun q => match q with (IVal v, ProvidedVal t _) => [(v,t)] | _ => [] end) provs) = Some t ->
  exists a, ilook (IVal v) provs = Some (ProvidedVal t a).
Proof.
  induction provs as [|[k p] rest IH]; intros v t K Found; [discriminate|].
  pose proof (K k p (or_introl eq_refl)) as Kind. specialize (IH v t (keys_match_tail _ _ K)).
  destruct k as [x|x|x|r m [|? ?]|x], p as [tv a|g a|g a|fx]; cbn in Kind; try contradiction; cbn in Found |- *; auto.
  destruct (Nat.eqb_spec v x) as [->|_]; [injection Found as <-; eauto|auto].
Qed.

Lemma ilook_vals : forall provs v t a, KeysMatch provs -> ilook (IVal v) provs = Some (ProvidedVal t a) ->
  nlook v (flat_map (fun q => match q with (IVal v, ProvidedVal t _) => [(v,t)] | _ => [] end) provs) = Some t.
Proof.
  induction provs as [|[k p] rest IH]; intros v t a K Found; [discriminate|].
  pose proof (K k p (or_introl eq_refl)) as Kind. specialize (IH v t a (keys_match_tail _ _ K)).
  destruct k as [x|x|x|r m [|? ?]|x], p as [tv a'|g a'|g a'|fx]; cbn in Kind; try contradiction; cbn in Found |- *; auto.
  destruct (Nat.eqb_spec v x) as [->|_]; [congruence|auto].
Qed.

Lemma fns_ilook : forall provs f g, KeysMatch provs ->
  nlook f (flat_map (fun q => match q with (IFn f, ProvidedFn g _) => [(f,g)] | _ => [] end) provs) = Some g ->
  exists a, ilook (IFn f) provs = Some (ProvidedFn g a).
Proof.
  induction provs as [|[k p] rest IH]; intros f g K Found; [discriminate|].
  pose proof (K k p (or_introl eq_refl)) as Kind. specialize (IH f g (keys_match_tail _ _ K)).
  destruct k as [x|x|x|r m [|? ?]|x], p as [tv a|g' a|g' a|fx]; cbn in Kind; try contradiction; cbn in Found |- *; auto.
  destruct (Nat.eqb_spec f x) as [->|_]; [injection Found as <-; eauto|auto].
Qed.

Lemma ilook_fns : forall provs f g a, KeysMatch provs -> ilook (IFn f) provs = Some (ProvidedFn g a) ->
  nlook f (flat_map (fun q => match q with (IFn f, ProvidedFn g _) => [(f,g)] | _ => [] end) provs) = Some g.
Proof.
  induction provs as [|[k p] rest IH]; intros f g a K Found; [discriminate|].
  pose proof (K k p (or_introl eq_refl)) as Kind. specialize (IH f g a (keys_match_tail _ _ K)).
  destruct k as [x|x|x|r m [|? ?]|x], p as [tv a'|g' a'|g' a'|fx]; cbn in Kind; try contradiction; cbn in Found |- *; auto.
  destruct (Nat.eqb_spec f x) as [->|_]; [congruence|auto].
Qed.

Lemma mths_ilook : forall provs r m g, KeysMatch provs ->
  mlook r m (flat_map (fun q => match q with (IMth r m _, ProvidedMethod g _) => [((r,m),g)] | _ => [] end) provs) = Some g ->
  exists a, ilook (mkey r m) provs = Some (ProvidedMethod g a).
Proof.
  induction provs as [|[k p] rest IH]; intros r m g K Found; [discriminate|].
  pose proof (K k p (or_introl eq_refl)) as Kind. specialize (IH r m g (keys_match_tail _ _ K)).
  destruct k as [x|x|x|r' m' [|? ?]|x], p as [tv a|g' a|g' a|fx]; cbn in Kind; try contradiction; cbn in Found |- *; auto.
  destruct (ty_eqb r r'); destruct (Nat.eqb m m'); cbn in Found |- *; try (injection Found as <-; eauto); auto.
Qed.

Lemma ilook_mths : forall provs r m g a, KeysMatch provs -> ilook (mkey r m) provs = Some (ProvidedMethod g a) ->
  mlook r m (flat_map (fun q => match q with (IMth r m _, ProvidedMethod g _) => [((r,m),g)] | _ => [] end) provs) = Some g.
Proof.
  induction provs as [|[k p] rest IH]; intros r m g a K Found; [discriminate|].
  pose proof (K k p (or_introl eq_refl)) as Kind. specialize (IH r m g a (keys_match_tail _ _ K)).
  destruct k as [x|x|x|r' m' [|? ?]|x], p as [tv a'|g' a'|g' a'|fx]; cbn in Kind; try contradiction; cbn in Found |- *; auto.
  destruct (ty_eqb r r'); destruct (Nat.eqb m m'); cbn in Found |- *; try congruence; auto.
Qed.

Lemma funs_ilook : forall provs u fx, KeysMatch provs ->
  nlook u (flat_map (fun q => match q with (IFun u, ProvidedFunctor fx) => [(u,fx)] | _ => [] end) provs) = Some fx ->
  ilook (IFun u) provs = Some (ProvidedFunctor fx).
Proof.
  induction provs as [|[k p] rest IH]; intros u fx K Found; [discriminate|].
  pose proof (K k p (or_introl eq_refl)) as Kind. specialize (IH u fx (keys_match_tail _ _ K)).
  destruct k as [x|x|x|r m [|? ?]|x], p as [tv a|g a|g a|fx']; cbn in Kind; try contradiction; cbn in Found |- *; auto.
  destruct (Nat.eqb_spec u x) as [->|_]; [congruence|auto].
Qed.
End Tables.

(* ---- a function context's provisions are its needs -------------------------- *)

Section Needs.
Variable W : A.world.
Import A.

Definition NeedShape (k : item) : Prop :=
  match k with ITy _ => False | IMth _ _ th => th = [] | _ => True end.

Lemma entry_provision_key : forall σ freeze e k p,
  In (k, p) (entry_provision σ freeze e) -> entry_key σ e = Some k.
Proof. intros σ freeze [t|v t a|f g a|r m g a|u fx|eql eqr] k p I; cbn in I; try contradiction; destruct I as [E|[]]; injection E as <- _; reflexivity. Qed.

Lemma entry_key_shape : forall σ e k, entry_key σ e = Some k -> NeedShape k.
Proof. intros σ [t|v t a|f g a|r m g a|u fx|eql eqr] k E; cbn in E; try discriminate; injection E as <-; cbn; auto. Qed.

Lemma ctx_keys_match : forall f Cf ns, CtxOf W f Cf ns -> KeysMatch (c_provisions Cf).
Proof.
  intros f Cf ns (d & es & tm & er & tmr & σ & classes & _ & _ & _ & _ & _ & _ & -> & _ & _) k p I. cbn in I.
  apply in_flat_map in I as [e [_ I]]. destruct e as [t|v t a|f' g a|r m g a|u fx|eql eqr]; cbn in I;
    try contradiction; destruct I as [E|[]]; injection E as <- <-; cbn; auto.
Qed.

Lemma ctx_needs : forall f Cf ns, CtxOf W f Cf ns ->
  NoDup ns /\ (forall k p, In (k, p) (c_provisions Cf) -> In k ns) /\ (forall k, In k ns -> NeedShape k).
Proof.
  intros f Cf ns (d & es & tm & er & tmr & σ & classes & _ & _ & _ & _ & _ & _ & -> & [Nd Keys] & _).
  split; [exact Nd|]. split.
  - intros k p I. cbn in I. apply in_flat_map in I as [e [Ie I]]. apply Keys. exists e. split; [exact Ie|].
    exact (entry_provision_key _ _ _ _ _ I).
  - intros k Ik. destruct (proj1 (Keys k) Ik) as [e [_ E]]. exact (entry_key_shape _ _ _ E).
Qed.
End Needs.

(* ---- selecting provisions --------------------------------------------------- *)

Fixpoint sm_find (req : item) (sm : smap) : option item :=
  match sm with
  | [] => None
  | (r, s) :: rest => if item_eqb req r then Some s else sm_find req rest
  end.

Lemma select_lookup : forall d sm dc req, select_provisions d sm = Some dc ->
  lookup_provision req dc = match sm_find req sm with Some s => lookup_provision s d | None => None end.
Proof.
  intros d sm. induction sm as [|[r s] rest IH]; intros dc req Sel; cbn in Sel.
  - injection Sel as <-. reflexivity.
  - destruct (lookup_provision s d) as [en|] eqn:L; [|discriminate].
    destruct (select_provisions d rest) as [d'|] eqn:Rest; [|discriminate]. injection Sel as <-.
    change (lookup_provision req (install_provision r en d')) with
      (if item_eqb req r then Some en else lookup_provision req d').
    cbn [sm_find]. destruct (item_eqb req r); [rewrite L; reflexivity|exact (IH d' req eq_refl)].
Qed.

Lemma select_ok : forall d sm, (forall q, In q sm -> lookup_provision (snd q) d <> None) ->
  exists dc, select_provisions d sm = Some dc.
Proof.
  intros d sm Ok. induction sm as [|q rest IH]; [eexists; reflexivity|].
  destruct q as [rq sv]. cbn [select_provisions].
  destruct (lookup_provision sv d) as [en|] eqn:L; [|exfalso; exact (Ok (rq, sv) (or_introl eq_refl) L)].
  assert (RestOk : forall q, In q rest -> lookup_provision (snd q) d <> None) by (intros q Iq; apply Ok; right; exact Iq).
  destruct (IH RestOk) as [d' E]. rewrite E. eauto.
Qed.

Lemma sm_find_map : forall (key : item -> item) (target : item -> item) ns k,
  In k ns -> (forall k', In k' ns -> key k' = key k -> k' = k) ->
  sm_find (key k) (map (fun k' => (key k', target k')) ns) = Some (target k).
Proof.
  intros key target ns k Ik Inj. induction ns as [|k' rest IH]; [contradiction|]. cbn.
  destruct (item_eqb (key k) (key k')) eqn:E.
  - apply item_eqb_spec in E. rewrite (Inj k' (or_introl eq_refl) (eq_sym E)). reflexivity.
  - destruct Ik as [<-|Ik].
    + rewrite (proj2 (item_eqb_spec _ _) eq_refl) in E. discriminate.
    + apply IH; [exact Ik|intros k'' I; exact (Inj k'' (or_intror I))].
Qed.

Lemma rkey_of_injective : forall C k k', NeedShape k -> NeedShape k' -> A.rkey_of C k = A.rkey_of C k' -> k = k'.
Proof.
  intros C [t|v|f|r m th|u] [t'|v'|f'|r' m' th'|u'] S S' E; cbn in *; try contradiction; try discriminate; try congruence.
  subst th th'. unfold A.rkey in E. injection E as -> -> _. reflexivity.
Qed.

(* ---- generations of a substituted type -------------------------------------- *)

Lemma gens_subst : forall t s x, In x (A.gens (app_subst s t)) ->
  exists y, In y (A.gens t) /\
    ((lookup_subst y s = None /\ x = y) \/ (exists u, lookup_subst y s = Some u /\ In x (A.gens u))).
Proof. exact PublicSubstFacts.gens_subst. Qed.

(* Every generation a substituted variable mentions occurs at each use of
   that variable in the template. Together with gens_subst this is exact. *)
Lemma gens_subst_hit : forall t s x y, In x (A.gens t) ->
  In y (A.gens (app_subst s (TAbs x))) -> In y (A.gens (app_subst s t)).
Proof. exact PublicSubstFacts.gens_subst_hit. Qed.

Lemma gens_subst_iff : forall t s y, In y (A.gens (app_subst s t)) <->
  exists x, In x (A.gens t) /\ In y (A.gens (app_subst s (TAbs x))).
Proof. exact PublicSubstFacts.gens_subst_iff. Qed.

Section Support2.
Variable W : A.world.
Import A.

Lemma subst_scoped : forall (K : list nat) s t,
  (forall g u, In (g, u) s -> incl (gens u) K) -> incl (gens t) K -> incl (gens (app_subst s t)) K.
Proof. exact PublicSubstFacts.subst_scoped. Qed.

Definition provision_sig_gens (p : provision) : list nat :=
  match provision_signature p with Some g => sig_gens g | None => [] end.
Definition key_gens (k : item) : list nat := match k with IMth r _ _ => gens r | _ => [] end.

Lemma sig_read_scoped : forall (K : list nat) s g,
  (forall t, incl (gens t) K -> incl (gens (app_subst s t)) K) ->
  incl (sig_gens g) K -> incl (sig_gens (sig_subst s g)) K.
Proof.
  intros K s [ps r] Read Sc x Ix. unfold sig_gens in *. cbn in Ix |- *. cbn in Sc.
  apply in_app_or in Ix as [Ix|Ix].
  - exact (Read r (fun y Iy => Sc y (in_or_app _ _ _ (or_introl Iy))) x Ix).
  - rewrite flat_map_concat_map, map_map in Ix. apply in_concat in Ix as [l [Il Ix]].
    apply in_map_iff in Il as [t [<- It]].
    apply (Read t); [|exact Ix].
    intros y Iy. apply Sc. apply in_or_app. right. apply in_flat_map. exists t. split; assumption.
Qed.

Lemma sig_subst_scoped : forall (K : list nat) s g,
  (forall x u, In (x, u) s -> incl (gens u) K) -> incl (sig_gens g) K -> incl (sig_gens (sig_subst s g)) K.
Proof.
  intros K s g Rows Source. apply (sig_read_scoped K s g); [|exact Source].
  intros t InT. exact (subst_scoped K s t Rows InT).
Qed.

Theorem ctx_provision_scoped : forall f Cf ns, CtxOf W f Cf ns ->
  forall k p, In (k, p) (c_provisions Cf) ->
  incl (provision_sig_gens p) (map fst (c_ty Cf)) /\ incl (key_gens k) (map fst (c_ty Cf)).
Proof.
  intros f Cf ns Ctx.
  destruct Ctx as (d & es & tm & er & tmr & σ & classes & D & IE & Root & MGM & Merge & Section & -> & _ & _).
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT [_ EsT]].
  assert (Tmr : IdForm tmr /\ incl (map fst tm) (map fst tmr) /\ Scoped tmr (flat_map entry_gens er)).
  { destruct Root as [_|gc d0 n0 its er' tmr' _ _ _ _ RI].
    - split; [exact IdT|]. split; [intros x Ix; exact Ix|]. intros g [].
    - exact (root_formed W _ _ _ _ _ _ RI IdT). }
  destruct Tmr as [IdR [SubR ErR]].
  assert (Keys : map fst (c_ty (entries_cx σ classes tmr (es ++ er))) = map fst tmr) by (cbn; rewrite map_map; reflexivity).
  rewrite Keys.
  assert (All : Scoped tmr (flat_map entry_gens (es ++ er))).
  { intros x Ix. rewrite flat_map_app in Ix. apply in_app_or in Ix as [Ix|Ix]; [exact (SubR x (EsT x Ix))|exact (ErR x Ix)]. }
  assert (Classes : incl (merge_symbols classes) (map fst tmr)).
  { intros x Ix. apply All. exact (proj1 (proj2 (proj2 Merge)) x Ix). }
  assert (Read : forall t, incl (gens t) (map fst tmr) -> incl (gens (app_subst σ t)) (map fst tmr))
    by exact (PublicMerge.section_gens_bound Section Classes).
  intros k p I. cbn in I. apply in_flat_map in I as [e [Ie I]].
  assert (Eg : incl (entry_gens e) (map fst tmr)).
  { intros x Ix. apply All. apply in_flat_map. exists e. split; assumption. }
  destruct e as [t|v t a|f' g a|r m g a|u fx|eql eqr]; cbn in I; try contradiction; destruct I as [E|[]]; injection E as <- <-;
    unfold provision_sig_gens, key_gens; cbn [provision_signature]; split; try (intros x []).
  - apply (sig_read_scoped _ σ (FSig [] t) Read). intros x Ix. apply Eg. cbn. unfold sig_gens in Ix. cbn in Ix.
    rewrite app_nil_r in Ix. apply in_or_app. left. exact Ix.
  - apply (sig_read_scoped _ σ g Read). intros x Ix. apply Eg. cbn. apply in_or_app. left. exact Ix.
  - apply (sig_read_scoped _ σ g Read). intros x Ix. apply Eg. cbn. apply in_or_app. right. apply in_or_app. left. exact Ix.
  - apply (Read r). intros x Ix. apply Eg. cbn. apply in_or_app. left. exact Ix.
Qed.
(* A functor need records every fixed input of its functor. *)
Definition FunCover (e : entry) : Prop :=
  match e with NFun u fx => (forall x, Fixed W u x -> exists t, In (x, t) fx) /\
    (forall x t t', In (x, t) fx -> In (x, t') fx -> t = t') | _ => True end.

Lemma nlook_some : forall X k (l : list (nat * X)), In k (map fst l) -> exists v, nlook k l = Some v.
Proof.
  intros X k l. induction l as [|[a v] r IH]; intros I; [contradiction|]. cbn in I |- *.
  destruct (Nat.eqb_spec k a) as [->|Ne]; [eauto|]. destruct I as [E|I]; [cbn in E; congruence|exact (IH I)].
Qed.

Lemma entries_cover :
  (forall p ob tm sp es tm', ItemEntries W p ob tm sp es tm' -> Forall FunCover es) /\
  (forall p ob tm sps es tm', ItemsEntries W p ob tm sps es tm' -> Forall FunCover es).
Proof.
  apply (ies_mut W (fun p ob tm sp es tm' _ => Forall FunCover es) (fun p ob tm sps es tm' _ => Forall FunCover es));
    intros; repeat constructor; try assumption; try (apply Forall_app; split; assumption).
  all: try solve [apply Forall_app; split; [assumption|constructor; [exact Logic.I|assumption]]].
  all: cbn; first
    [ intros x Fx; destruct (nlook x tm) as [t|] eqn:L; [exists t; apply f; split; [exact Fx|exact L]|exfalso; exact (n0 x Fx L)]
    | intros x t t' I I'; destruct (proj1 (f x t) I) as [_ L]; destruct (proj1 (f x t') I') as [_ L']; congruence ].
Qed.

Lemma root_cover : forall q es0 tm its es tm', RootItems W q es0 tm its es tm' -> IdForm tm -> Forall FunCover es.
Proof.
  intros q es0 tm its es tm' Root. induction Root as
    [es0 tm|es0 tm p a rest u du n ps args res xs fx es tm' R D Ast Fresh Nd Xs FR Rest IH|es0 tm sp rest es tm' Skip Rest IH];
    intros Id; [constructor| |exact (IH Id)].
  destruct (add_tys_formed xs tm Id) as [IdA [_ XsA]].
  apply Forall_app. split; [apply Forall_forall; intros e Ie; apply in_map_iff in Ie as [x [<- _]]; exact Logic.I|].
  constructor; [|exact (IH IdA)]. cbn. split.
  - intros x Fx. destruct (nlook_some _ x _ (XsA x (proj2 (Xs x) Fx))) as [t L]. exists t. apply FR. split; [exact Fx|exact L].
  - intros x t t' I I'. destruct (proj1 (FR x t) I) as [_ L]. destruct (proj1 (FR x t') I') as [_ L']. congruence.
Qed.

Theorem ctx_fun_fixed : forall f Cf ns, CtxOf W f Cf ns -> forall u fx, In (IFun u, ProvidedFunctor fx) (c_provisions Cf) ->
  (forall x, Fixed W u x -> exists t, In (x, t) fx) /\ (forall x t, In (x, t) fx -> incl (gens t) (map fst (c_ty Cf))) /\
  (forall x t t', In (x, t) fx -> In (x, t') fx -> t = t').
Proof.
  intros f Cf ns Ctx. pose proof (ctxof_formed W f Cf ns Ctx) as [_ Dom].
  destruct Ctx as (d & es & tm & er & tmr & σ & classes & D & IE & Root & MGM & _ & _ & -> & _ & _).
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT [_ EsT]].
  pose proof (proj2 entries_cover _ _ _ _ _ _ IE) as CovE.
  assert (Tmr : IdForm tmr /\ incl (map fst tm) (map fst tmr) /\ Scoped tmr (flat_map entry_gens er) /\ Forall FunCover er).
  { destruct Root as [_|gc d0 n0 its er' tmr' _ _ _ _ RI].
    - split; [exact IdT|]. split; [intros x Ix; exact Ix|]. split; [intros g []|constructor].
    - destruct (root_formed W _ _ _ _ _ _ RI IdT) as [A1 [A2 A3]]. split; [exact A1|]. split; [exact A2|].
      split; [exact A3|exact (root_cover _ _ _ _ _ _ RI IdT)]. }
  destruct Tmr as [IdR [SubR [ErR CovR]]].
  assert (Keys : map fst (c_ty (entries_cx σ classes tmr (es ++ er))) = map fst tmr) by (cbn; rewrite map_map; reflexivity).
  rewrite Keys in Dom |- *.
  assert (All : Scoped tmr (flat_map entry_gens (es ++ er))).
  { intros x Ix. rewrite flat_map_app in Ix. apply in_app_or in Ix as [Ix|Ix]; [exact (SubR x (EsT x Ix))|exact (ErR x Ix)]. }
  assert (Range : forall g u, In (g, u) σ -> incl (gens u) (map fst tmr)) by (intros g u I; exact (proj2 (Dom g u I))).
  intros u fx I. cbn in I. apply in_flat_map in I as [e [Ie I]].
  assert (Eg : incl (entry_gens e) (map fst tmr)).
  { intros x Ix. apply All. apply in_flat_map. exists e. split; assumption. }
  assert (Cov : FunCover e).
  { apply in_app_or in Ie as [Ie|Ie]; [exact (proj1 (Forall_forall _ _) CovE e Ie)|exact (proj1 (Forall_forall _ _) CovR e Ie)]. }
  destruct e as [t|v t a|f' g a|r m g a|u' fx'|eql eqr]; cbn in I; try contradiction; destruct I as [E|[]]; try discriminate.
  injection E as <- <-. cbn in Cov. destruct Cov as [Cov Fun]. split; [|split].
  - intros x Fx. destruct (Cov x Fx) as [t It]. exists (app_subst σ t). unfold fixed_subst.
    apply in_map_iff. exists (x, t). split; [reflexivity|exact It].
  - intros x t It. unfold fixed_subst in It. apply in_map_iff in It as [[y t0] [E It]]. cbn in E. injection E as -> <-.
    apply (subst_scoped _ σ t0 Range). intros z Iz. apply Eg. cbn. apply in_flat_map. exists (x, t0). split; assumption.
  - intros x t t' I I'. unfold fixed_subst in I, I'. apply in_map_iff in I as [[y a] [E I]]. apply in_map_iff in I' as [[y' a'] [E' I']].
    cbn in E, E'. injection E as -> <-. injection E' as -> <-. rewrite (Fun x a a' I I'). reflexivity.
Qed.
(* A functor's own context, as a function's: its table is canonical
   self-reference, its merge and provisions are over its keys, and its
   provisions are its needs. *)
(* MergeOf bounds the classes directly by the functor's input telescope. *)
Theorem functx_merge_symbols : forall u C ns, FunCtx W u C ns ->
  incl (merge_symbols (c_merge C)) (map fst (c_ty C)).
Proof.
  intros u C ns (d & ps & a & r & b & es & tm & tm' & sigma & classes & implicit &
    D & FP & IE & Imp & -> & MGM & Merge & Section & -> & Needs & Context).
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [_ [_ Scope]].
  destruct Merge as [_ [_ [Support _]]]. intros y InMerge.
  cbn [c_merge c_ty entries_cx]. rewrite map_map. cbn. rewrite map_app.
  apply in_or_app. right. exact (Scope y (Support y InMerge)).
Qed.

Theorem functx_scoped : forall u C ns, FunCtx W u C ns ->
  (forall k v, nlook k (c_ty C) = Some v -> v = app_subst (c_sub C) (TAbs k)) /\
  (forall k p, In (k, p) (c_provisions C) ->
     incl (provision_sig_gens p) (map fst (c_ty C)) /\ incl (key_gens k) (map fst (c_ty C)) /\
     match p with ProvidedFunctor fx => forall x t, In (x, t) fx -> incl (gens t) (map fst (c_ty C)) | _ => True end) /\
  KeysMatch (c_provisions C) /\
  NoDup ns /\ (forall k p, In (k, p) (c_provisions C) -> In k ns) /\ (forall k, In k ns -> NeedShape k) /\
  (forall g t, In (g, t) (c_sub C) -> In g (map fst (c_ty C)) /\ incl (gens t) (map fst (c_ty C))).
Proof.
  intros u C ns (d & ps & a & r & b & es & tm & tm' & σ & classes & implicit & D & FP & IE & Imp & -> & MGM & Merge & Section & -> & [Nd Keys] & _).
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT [_ EsT]].
  set (tmr := (map (fun t => (t, TAbs t)) implicit ++ tm)%list).
  assert (IdR : IdForm tmr).
  { apply Forall_app. split; [|exact IdT]. apply Forall_forall. intros q Iq. apply in_map_iff in Iq as [t [<- _]]. reflexivity. }
  assert (All : Scoped tmr (flat_map entry_gens es)).
  { intros x Ix. unfold tmr. rewrite map_app. apply in_or_app. right. exact (EsT x Ix). }
  assert (KeysE : map fst (c_ty (entries_cx σ classes tmr es)) = map fst tmr) by (cbn; rewrite map_map; reflexivity).
  assert (Classes : incl (merge_symbols classes) (map fst tmr)).
  { intros x Ix. apply All. exact (proj1 (proj2 (proj2 Merge)) x Ix). }
  assert (Read : forall t, incl (gens t) (map fst tmr) -> incl (gens (app_subst σ t)) (map fst tmr))
    by exact (PublicMerge.section_gens_bound Section Classes).
  split; [|split; [|split; [|split; [exact Nd|split; [|split]]]]].
  - intros k v Found. cbn in Found. change (map _ tmr) with (PublicTypeRead.smap σ tmr) in Found.
    rewrite nlook_smap in Found. destruct (nlook k tmr) as [v0|] eqn:L; [|discriminate].
    injection Found as <-. rewrite (idform_nlook _ _ _ IdR L). reflexivity.
  - rewrite KeysE. intros k p I. cbn in I. apply in_flat_map in I as [e [Ie I]].
    assert (Eg : incl (entry_gens e) (map fst tmr)).
    { intros x Ix. apply All. apply in_flat_map. exists e. split; assumption. }
    destruct e as [t|v t a0|f' g a0|r0 m g a0|u' fx|eql eqr]; cbn in I; try contradiction; destruct I as [E|[]]; injection E as <- <-;
      unfold provision_sig_gens, key_gens; cbn [provision_signature].
    + split; [|split; [intros x []|exact Logic.I]]. apply (sig_read_scoped _ σ (FSig [] t) Read). intros x Ix. apply Eg. cbn.
      unfold sig_gens in Ix. cbn in Ix. rewrite app_nil_r in Ix. apply in_or_app. left. exact Ix.
    + split; [|split; [intros x []|exact Logic.I]]. apply (sig_read_scoped _ σ g Read). intros x Ix. apply Eg. cbn.
      apply in_or_app. left. exact Ix.
    + split; [|split; [|exact Logic.I]].
      * apply (sig_read_scoped _ σ g Read). intros x Ix. apply Eg. cbn. apply in_or_app. right. apply in_or_app. left. exact Ix.
      * apply (Read r0). intros x Ix. apply Eg. cbn. apply in_or_app. left. exact Ix.
    + split; [intros x []|split; [intros x []|]].
      intros x t It. unfold fixed_subst in It. apply in_map_iff in It as [[y t0] [E It]]. cbn in E. injection E as -> <-.
      apply (Read t0). intros z Iz. apply Eg. cbn. apply in_flat_map. exists (x, t0). split; assumption.
  - intros k p I. cbn in I. apply in_flat_map in I as [e [_ I]].
    destruct e as [t|v t a0|f' g a0|r0 m g a0|u' fx|eql eqr]; cbn in I; try contradiction; destruct I as [E|[]]; injection E as <- <-; cbn; auto.
  - intros k p I. cbn in I. apply in_flat_map in I as [e [Ie I]]. apply Keys. exists e. split; [exact Ie|].
    exact (entry_provision_key _ _ _ _ _ I).
  - intros k Ik. destruct (proj1 (Keys k) Ik) as [e [_ E]]. exact (entry_key_shape _ _ _ E).
  - rewrite KeysE. intros g t I. cbn in I.
    pose proof (proj1 (proj1 (proj2 (proj2 (proj2 MGM))) g t I)) as Gg.
    assert (Inside : In g (map fst tmr)) by exact (All g Gg).
    split; [exact Inside|].
    assert (Equal : app_subst σ (TAbs g) = t).
    { cbn [app_subst]. rewrite (proj2 (proj2 (proj2 (proj2 MGM))) g t I). reflexivity. }
    rewrite <- Equal. apply (Read (TAbs g)). intros x [<-|[]]. exact Inside.
Qed.
Theorem functx_fun_cover : forall u C ns, FunCtx W u C ns -> forall u' fx, In (IFun u', ProvidedFunctor fx) (c_provisions C) ->
  forall x, Fixed W u' x -> exists t, In (x, t) fx.
Proof.
  intros u C ns (d & ps & a & r & b & es & tm & tm' & σ & classes & implicit & D & FP & IE & Imp & -> & MGM & _ & _ & -> & _ & _) u' fx I.
  pose proof (proj2 entries_cover _ _ _ _ _ _ IE) as Cov. cbn in I. apply in_flat_map in I as [e [Ie I]].
  pose proof (proj1 (Forall_forall _ _) Cov e Ie) as Ce.
  destruct e as [t|v t a0|f' g a0|r0 m g a0|u0 fx0|eql eqr]; cbn in I; try contradiction; destruct I as [E|[]]; try discriminate.
  injection E as <- <-. cbn in Ce. destruct Ce as [Cv _]. intros x Fx. destruct (Cv x Fx) as [t It].
  exists (app_subst σ t). unfold fixed_subst. apply in_map_iff. exists (x, t). split; [reflexivity|exact It].
Qed.
Theorem functx_needs_provided : forall u C ns, FunCtx W u C ns -> forall k, In k ns -> exists p, In (k, p) (c_provisions C).
Proof.
  intros u C ns (d & ps & a & r & b & es & tm & tm' & σ & classes & implicit & _ & _ & _ & _ & -> & _ & _ & _ & -> & [_ Keys] & _) k Ik.
  destruct (proj1 (Keys k) Ik) as [e [Ie E]]. cbn [c_provisions entries_cx].
  destruct e as [t|v t a0|f' g a0|r0 m g a0|u0 fx|eql eqr] eqn:Ee; cbn in E; try discriminate; injection E as <-;
    eexists; apply in_flat_map; exists e; (split; [rewrite Ee; exact Ie|]); rewrite Ee; cbn; left; reflexivity.
Qed.
End Support2.

(* ---- tables read through the provision list ------------------------------- *)

Section ByIlook.
Import A.

Definition vals_of (l : list (item * provision)) := flat_map (fun q => match q with (IVal v, ProvidedVal t _) => [(v,t)] | _ => [] end) l.
Definition fns_of (l : list (item * provision)) := flat_map (fun q => match q with (IFn f, ProvidedFn g _) => [(f,g)] | _ => [] end) l.
Definition mths_of (l : list (item * provision)) := flat_map (fun q => match q with (IMth r m _, ProvidedMethod g _) => [((r,m),g)] | _ => [] end) l.
Definition funs_of (l : list (item * provision)) := flat_map (fun q => match q with (IFun u, ProvidedFunctor fx) => [(u,fx)] | _ => [] end) l.

Lemma vals_by_ilook : forall l v, KeysMatch l ->
  nlook v (vals_of l) = match ilook (IVal v) l with Some (ProvidedVal t _) => Some t | _ => None end.
Proof.
  induction l as [|[k p] rest IH]; intros v K; [reflexivity|].
  pose proof (K k p (or_introl eq_refl)) as Kind. specialize (IH v (keys_match_tail _ _ K)).
  destruct k as [x|x|x|r m [|? ?]|x], p as [tv a|g a|g a|fx]; cbn in Kind; try contradiction;
    unfold vals_of in *; cbn in IH |- *; try exact IH.
  destruct (Nat.eqb v x); [reflexivity|exact IH].
Qed.

Lemma fns_by_ilook : forall l f, KeysMatch l ->
  nlook f (fns_of l) = match ilook (IFn f) l with Some (ProvidedFn g _) => Some g | _ => None end.
Proof.
  induction l as [|[k p] rest IH]; intros f K; [reflexivity|].
  pose proof (K k p (or_introl eq_refl)) as Kind. specialize (IH f (keys_match_tail _ _ K)).
  destruct k as [x|x|x|r m [|? ?]|x], p as [tv a|g a|g a|fx]; cbn in Kind; try contradiction;
    unfold fns_of in *; cbn in IH |- *; try exact IH.
  destruct (Nat.eqb f x); [reflexivity|exact IH].
Qed.

Lemma mths_by_ilook : forall l r m, KeysMatch l ->
  mlook r m (mths_of l) = match ilook (mkey r m) l with Some (ProvidedMethod g _) => Some g | _ => None end.
Proof.
  induction l as [|[k p] rest IH]; intros r m K; [reflexivity|].
  pose proof (K k p (or_introl eq_refl)) as Kind. specialize (IH r m (keys_match_tail _ _ K)).
  destruct k as [x|x|x|r' m' [|? ?]|x], p as [tv a|g a|g a|fx]; cbn in Kind; try contradiction;
    unfold mths_of in *; cbn in IH |- *; try exact IH.
  destruct (ty_eqb r r'); destruct (Nat.eqb m m'); cbn; first [reflexivity|exact IH].
Qed.

Lemma funs_by_ilook : forall l u, KeysMatch l ->
  nlook u (funs_of l) = match ilook (IFun u) l with Some (ProvidedFunctor fx) => Some fx | _ => None end.
Proof.
  induction l as [|[k p] rest IH]; intros u K; [reflexivity|].
  pose proof (K k p (or_introl eq_refl)) as Kind. specialize (IH u (keys_match_tail _ _ K)).
  destruct k as [x|x|x|r m [|? ?]|x], p as [tv a|g a|g a|fx]; cbn in Kind; try contradiction;
    unfold funs_of in *; cbn in IH |- *; try exact IH.
  destruct (Nat.eqb u x); [reflexivity|exact IH].
Qed.

Definition set_list (k0 : item) (p0 : provision) (l : list (item * provision)) :=
  (k0, p0) :: filter (fun q => negb (item_eqb k0 (fst q))) l.

Lemma ilook_set : forall k k0 p0 l,
  ilook k (set_list k0 p0 l) = if item_eqb k k0 then Some p0 else ilook k l.
Proof.
  intros k k0 p0 l. unfold set_list. cbn. destruct (item_eqb k k0) eqn:E; [reflexivity|].
  induction l as [|[k' p'] rest IH]; [reflexivity|]. cbn.
  destruct (item_eqb k0 k') eqn:E0; cbn.
  - apply item_eqb_spec in E0. subst k'. rewrite E. exact IH.
  - destruct (item_eqb k k'); [reflexivity|exact IH].
Qed.

Lemma keys_match_set : forall k0 p0 l, KeysMatch l -> KeysMatch [(k0, p0)] -> KeysMatch (set_list k0 p0 l).
Proof.
  intros k0 p0 l K K0 k p [E|I]; [injection E as E1 E2; subst; exact (K0 _ _ (or_introl eq_refl))|].
  apply filter_In in I as [I _]. exact (K k p I).
Qed.
End ByIlook.
