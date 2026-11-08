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
