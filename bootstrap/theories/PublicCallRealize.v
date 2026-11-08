(* Realizing a callee's context at a call (T03/T04). A function's context
   maps each of its type symbols to that symbol's canonical form, and its
   merge substitution and entries mention only those symbols. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead.
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
  - destruct (d_ast d); try (destruct Recv as [args [-> F]]; exact (Nom args F Ig)).
    exact (tyof_scoped _ _ _ _ _ _ Recv Sc Ig).
  - destruct Recv as [args [-> F]]. exact (Nom args F Ig).
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
Qed.
End Formation.

Section Contexts.
Variable W : A.world.
Import A.

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
  intros f Cf ns (d & es & tm & er & tmr & σ & D & IE & Root & MGM & -> & _ & _).
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT [_ EsT]].
  assert (Tmr : IdForm tmr /\ incl (map fst tm) (map fst tmr) /\ Scoped tmr (flat_map entry_gens er)).
  { destruct Root as [_|gc d0 n0 its er' tmr' _ _ _ _ RI].
    - split; [exact IdT|]. split; [intros x Ix; exact Ix|]. intros g [].
    - exact (root_formed _ _ _ _ _ _ RI IdT). }
  destruct Tmr as [IdR [SubR ErR]].
  assert (Keys : map fst (c_ty (entries_cx σ tmr (es ++ er))) = map fst tmr).
  { cbn. rewrite map_map. reflexivity. }
  split.
  - intros k v Found. cbn in Found. change (map _ tmr) with (PublicTypeRead.smap σ tmr) in Found.
    rewrite nlook_smap in Found. destruct (nlook k tmr) as [u|] eqn:L; [|discriminate].
    injection Found as <-. rewrite (idform_nlook _ _ _ IdR L). reflexivity.
  - intros g t Ig. rewrite Keys. cbn in Ig. destruct MGM as [_ [_ [_ Dom]]]. destruct (Dom g t Ig) as [Gg Gt].
    assert (All : Scoped tmr (flat_map entry_gens (es ++ er))).
    { intros x Ix. rewrite flat_map_app in Ix. apply in_app_or in Ix as [Ix|Ix]; [exact (SubR x (EsT x Ix))|exact (ErR x Ix)]. }
    split; [exact (All g Gg)|intros x Ix; exact (All x (Gt x Ix))].
Qed.
End Contexts.

Section Calls.
Variable W : A.world.
Import A.

Definition callee_meanings (d : environment) (th : subst) : subst :=
  map (fun q => (fst q, ground d (snd q))) th.

Lemma lookup_subst_in : forall x (th : subst), In x (map fst th) -> lookup_subst x th <> None.
Proof.
  intros x th. induction th as [|[y u] rest IH]; intros In'; [contradiction|].
  cbn in *. destruct (Nat.eqb_spec x y); [discriminate|]. destruct In' as [E|In']; [congruence|exact (IH In')].
Qed.

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

Lemma merge_image_dom : forall f Cf ns th x,
  CtxOf W f Cf ns -> map fst th = map fst (c_ty Cf) ->
  In x (map fst (c_ty Cf)) -> forall y, In y (gens (app_subst (c_sub Cf) (TAbs x))) -> lookup_subst y th <> None.
Proof.
  intros f Cf ns th x Ctx Keys Ix y Iy. apply lookup_subst_in. rewrite Keys.
  destruct (ctxof_formed W f Cf ns Ctx) as [_ Dom]. cbn in Iy.
  destruct (lookup_subst x (c_sub Cf)) as [t|] eqn:L.
  - apply lookup_subst_in_pair in L. exact (proj2 (Dom x t L) y Iy).
  - destruct Iy as [<-|[]]. exact Ix.
Qed.

Lemma merged_meaning : forall f Cf ns C th d x,
  CtxOf W f Cf ns -> CallTheta W C Cf f th ->
  (forall t, ground d (canon C t) = ground d t) ->
  In x (map fst (c_ty Cf)) ->
  app_subst (callee_meanings d th) (app_subst (c_sub Cf) (TAbs x)) = ground d (app_subst th (TAbs x)).
Proof.
  intros f Cf ns C th d x Ctx [Keys [_ Merges]] Abs Ix.
  rewrite callee_ground by exact (merge_image_dom f Cf ns th x Ctx Keys Ix).
  replace (app_subst (c_sub Cf) (TAbs x)) with
    (match lookup_subst x (c_sub Cf) with Some t => t | None => TAbs x end) by reflexivity.
  destruct (lookup_subst x (c_sub Cf)) as [t|] eqn:L; [|reflexivity].
  apply lookup_subst_in_pair in L. rewrite <- (Abs (app_subst th t)), <- (Merges x t L), Abs. reflexivity.
Qed.

Theorem call_agree : forall f Cf ns C th d,
  CtxOf W f Cf ns -> CallTheta W C Cf f th ->
  (forall t, ground d (canon C t) = ground d t) -> (forall t, canon C (canon C t) = canon C t) ->
  Agree (callee_meanings d th) (type_meanings d) (c_ty Cf) (c_ty C).
Proof.
  intros f Cf ns C th d Ctx CT Abs Idem k v1 v2 L1 L2.
  destruct (ctxof_formed W f Cf ns Ctx) as [Form _]. rewrite (Form k v1 L1).
  assert (Ik : In k (map fst (c_ty Cf))) by (apply nlook_in in L1; change k with (fst (k, v1)); apply in_map, L1).
  rewrite (merged_meaning f Cf ns C th d k Ctx CT Abs Ik).
  destruct CT as [Keys [Bound _]].
  assert (Th : lookup_subst k th = Some (canon C v2)).
  { rewrite (Bound k Ik ltac:(congruence)). cbn. rewrite lookup_subst_nlook', L2. reflexivity. }
  cbn [app_subst]. rewrite Th. change (app_subst (type_meanings d) v2) with (ground d v2).
  rewrite <- (Abs v2). reflexivity.
Qed.

Theorem call_absorbs : forall f Cf ns C th d,
  CtxOf W f Cf ns -> CallTheta W C Cf f th ->
  (forall t, ground d (canon C t) = ground d t) ->
  forall t, app_subst (callee_meanings d th) (canon Cf t) = app_subst (callee_meanings d th) t.
Proof.
  intros f Cf ns C th d Ctx CT Abs t. unfold canon. rewrite <- (app_subst_nil t) at 2.
  apply subst2_agree. intros x Ix. rewrite app_subst_nil.
  destruct (lookup_subst x (c_sub Cf)) as [u|] eqn:L.
  2: { replace (app_subst (c_sub Cf) (TAbs x)) with (TAbs x) by (cbn; rewrite L; reflexivity). reflexivity. }
  pose proof (lookup_subst_in_pair _ _ _ L) as InL.
  destruct (ctxof_formed W f Cf ns Ctx) as [_ Dom]. destruct (Dom x u InL) as [Ix' _].
  pose proof (merged_meaning f Cf ns C th d x Ctx CT Abs Ix') as M.
  rewrite M. destruct CT as [Keys _].
  rewrite callee_ground; [reflexivity|]. intros y [<-|[]]. apply lookup_subst_in. rewrite Keys. exact Ix'.
Qed.
End Calls.
