(* A checked fast path for unbracketed scalar function/value context items. *)
From Stdlib Require Import List String.
From NM Require Import Audit.
Import ListNotations Audit.Input Audit.Elaboration.

Definition scalar_ty (W : world) (p : string) (tm : list (nat * ty)) (a : aty) : option ty :=
  match a with
  | ATNever => Some never
  | ATUnit => Some TUnit
  | ATRef pa None =>
      match resolve_sym W p pa with
      | Some s => match kind_sym W s with Some DKType => nlook s tm | _ => None end
      | None => None
      end
  | _ => None
  end.

Lemma scalar_ty_sound : forall W p tm a t,
  scalar_ty W p tm a = Some t -> TyOf W p tm None a t.
Proof.
  intros W p tm a t; destruct a as [| | |fields|members|pa app];
    cbn [scalar_ty]; try discriminate;
    try (intro E; injection E as <-; constructor).
  destruct app; [discriminate|].
  destruct (resolve_sym W p pa) as [symbol|] eqn:R; [|discriminate].
  destruct (kind_sym W symbol) as [kind|] eqn:K; [|discriminate].
  destruct kind; try discriminate.
  intro E; eapply TyType; eauto.
Qed.

Fixpoint scalar_params W p tm (ps : list (name * aty)) : option (list ty) :=
  match ps with
  | [] => Some []
  | (_, a) :: ps => match scalar_ty W p tm a, scalar_params W p tm ps with
      | Some t, Some ts => Some (t :: ts) | _, _ => None end
  end.

Lemma scalar_params_sound : forall W p tm ps ts,
  scalar_params W p tm ps = Some ts ->
  Forall2 (fun pt t => TyOf W p tm None (snd pt) t) ps ts.
Proof.
  intros W p tm ps; induction ps as [|[n a] ps IH]; intros ts; cbn [scalar_params].
  - intro E; injection E as <-; constructor.
  - destruct (scalar_ty W p tm a) eqn:A; [|discriminate].
    destruct (scalar_params W p tm ps) eqn:P; [|discriminate].
    intro E; injection E as <-; constructor; [eapply scalar_ty_sound; eauto|eauto].
Qed.

Definition scalar_sig W s tm : option fsig :=
  match w_decl W s with
  | Some d => match d_ast d with
    | ADFn RPlain _ ps rt _ =>
      match scalar_params W (d_mod d) tm ps,
        match rt with Some a => scalar_ty W (d_mod d) tm a | None => Some TUnit end with
      | Some ts, Some t => Some (FSig ts t) | _, _ => None end
    | _ => None end
  | None => None end.

Lemma scalar_sig_sound : forall W s tm g,
  scalar_sig W s tm = Some g -> SigOf W s tm None g.
Proof.
  intros W s tm g; unfold scalar_sig.
  destruct (w_decl W s) as [d|] eqn:D; [|discriminate].
  destruct (d_ast d) eqn:A; try discriminate. destruct f; try discriminate.
  destruct (scalar_params W (d_mod d) tm l) eqn:P; [|discriminate].
  destruct o as [rt|].
  - destruct (scalar_ty W (d_mod d) tm rt) eqn:T; [|discriminate].
    intro E; injection E as <-. do 6 eexists; split; [exact D|]; split; [exact A|]; split.
    + eapply scalar_params_sound; exact P.
    + eapply scalar_ty_sound; exact T.
  - intro E; injection E as <-. do 6 eexists; split; [exact D|]; split; [exact A|]; split.
    + eapply scalar_params_sound; exact P.
    + reflexivity.
Qed.
Definition scalar_app W s (tm : list (nat * ty)) :=
  map (fun r => (r, nlook r tm)) (w_reqs W s).

Lemma scalar_app_sound : forall W s tm,
  kind_sym W s <> Some DKMeth -> AppOf W s tm (scalar_app W s tm).
Proof.
  intros W s tm K; exists (w_reqs W s); split; [apply incl_refl|].
  split; [reflexivity|]. split; [reflexivity|]. intros E; contradiction.
Qed.

Definition scalar_item W p tm (sp : spec) : option (entry * list (nat * ty)) :=
  match sp with
  | Spec (Some pa) None None => match resolve_sym W p pa with
    | Some s => match kind_sym W s with
      | Some DKType => Some (NTy s, match nlook s tm with Some _ => tm | None => (s,TAbs s)::tm end)
      | Some DKFn => match scalar_sig W s tm with
        | Some g => Some (NFn s g (scalar_app W s tm), tm) | None => None end
      | Some DKVal => match w_decl W s with
        | Some d => match d_ast d with
          | ADVal _ a None => match scalar_ty W (d_mod d) tm a with
            | Some t => Some (NVal s t (scalar_app W s tm), tm) | None => None end
          | _ => None end
        | None => None end
      | _ => None end
    | None => None end
  | _ => None end.

Lemma scalar_item_sound : forall W p tm sp e tm',
  scalar_item W p tm sp = Some (e, tm') -> ItemEntries W p [] tm sp [e] tm'.
Proof.
  intros W p tm [pa dn a] e tm'; unfold scalar_item.
  destruct pa as [pa|], dn, a; try discriminate.
  destruct (resolve_sym W p pa) as [s|] eqn:R; [|discriminate].
  destruct (kind_sym W s) as [k|] eqn:K; [|discriminate].
  destruct k; try discriminate.
  - intro E; injection E as <- <-; apply IEType; assumption.
  - destruct (w_decl W s) as [d|] eqn:D; [|discriminate].
    destruct (d_ast d) eqn:A; try discriminate. destruct o; [discriminate|].
    destruct (scalar_ty W (d_mod d) tm a) eqn:T; [|discriminate].
    intro E; injection E as <- <-. eapply IEVal; [exact R|exact D|exact A|constructor| |].
    + eapply scalar_ty_sound; exact T.
    + apply scalar_app_sound; congruence.
  - destruct (scalar_sig W s tm) as [g|] eqn:G; [|discriminate].
    intro E; injection E as <- <-. eapply IEFn; [exact R|exact K|constructor| |].
    + eapply scalar_sig_sound; exact G.
    + apply scalar_app_sound; congruence.
Qed.

Fixpoint scalar_items W p tm (specs : list spec) : option (list entry * list (nat * ty)) :=
  match specs with
  | [] => Some ([], tm)
  | sp :: specs => match scalar_item W p tm sp with
    | Some (e, tm1) => match scalar_items W p tm1 specs with
      | Some (es,tm2) => Some (e::es,tm2) | None => None end
    | None => None end end.

Lemma scalar_items_sound : forall W p specs tm es tm',
  scalar_items W p tm specs = Some (es,tm') -> ItemsEntries W p [] tm specs es tm'.
Proof.
  intros W p specs; induction specs as [|sp specs IH]; intros tm es tm'; cbn [scalar_items].
  - intro E; injection E as <- <-; constructor.
  - destruct (scalar_item W p tm sp) as [[e tm1]|] eqn:S; [|discriminate].
    destruct (scalar_items W p tm1 specs) as [[tail tm2]|] eqn:T; [|discriminate].
    intro E; injection E as <- <-.
    change (ItemsEntries W p [] tm (sp :: specs) ([e] ++ tail) tm2).
    eapply IsCons; [eapply scalar_item_sound; exact S|eapply IH; exact T].
Qed.

Fixpoint scalar_prefix W p tm (specs : list spec) : nat * list entry * list (nat * ty) :=
  match specs with
  | [] => (0, [], tm)
  | sp :: specs => match scalar_item W p tm sp with
    | Some (e, tm1) => let '(n,es,tm2) := scalar_prefix W p tm1 specs in (S n,e::es,tm2)
    | None => (0,[],tm) end end.

Lemma items_append : forall W p ob tm left right es1 tm1 es2 tm2,
  ItemsEntries W p ob tm left es1 tm1 ->
  ItemsEntries W p ob tm1 right es2 tm2 ->
  ItemsEntries W p ob tm (left ++ right) (es1 ++ es2) tm2.
Proof.
  intros W p ob tm left right es1 tm1 es2 tm2 H; induction H; simpl; intros Tail.
  - exact Tail.
  - rewrite <- app_assoc. eapply IsCons; eauto.
Qed.

(* This tactic only chooses witnesses. The VM equation below is checked
   through scalar_items_sound; unsupported items fall back to relation tactics. *)
Ltac scalar_items_prefix := match goal with
| |- ItemsEntries ?W ?p [] ?tm ?specs ?es ?tm' =>
  let result := eval vm_compute in (scalar_prefix W p tm specs) in
  match result with
  | (S ?n, ?entries, ?types) =>
    let count := constr:(S n) in
    let left := eval vm_compute in (firstn count specs) in
    let right := eval vm_compute in (skipn count specs) in
    change specs with (left ++ right);
    eapply items_append with (es1 := entries) (tm1 := types);
    [apply scalar_items_sound; vm_compute; reflexivity | idtac]
  end
end.
