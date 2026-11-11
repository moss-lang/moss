(* Eager copies (class D, unobservable): `bind F = H;` composes its smap
   and its result positions into H's closure at once (Audit.compose_adaptation,
   Audit.compose_positions), so a closure carries one of each. Before,
   it appended them, one list entry per copy, and an application chained
   through every entry (`adapt_chain`, `chain_sources` below, as they
   were in Audit.v).

   - Positions: reading the root's sources through the composition is
     reading them through each copy in turn, exactly, for every list of
     sources (`sources_compose`, `positions_chain`, `selection_chain`).
   - Needs: whenever the chained selection succeeds, the composed one
     succeeds with the same environment (`adapt_compose`, `adapt_chain_sound`).
     The converse fails only where the chain is stuck: a copy's smap entry
     that no later step reads still had to be found (`adapt_chain_strict`).
     An accepted program is never stuck (public_safety), so the two agree
     on every accepted program. *)
From Stdlib Require Import List Bool.
From NM Require AuditBase.
From NM Require Import Audit.
Import Audit.Input AuditBase.Semantics ListNotations.

(* The representation before: one smap and one position list per copy,
   the first made first. *)
Fixpoint adapt_chain (d : environment) (sms : list smap) : option environment :=
  match sms with
  | [] => Some d
  | sm :: r =>
      match adapt_chain d r with
      | Some d' => select_provisions d' sm
      | None => None
      end
  end.
Definition chain_sources (rms : list (list nat)) (sources : list item) : list item :=
  fold_left (fun s rm => map (fun j => nth j s (ITy 0)) rm) rms sources.

(* The composed form of a chain: what the copies made, one at a time,
   starting from the root's own (a link's smap is the chain's first). *)
Definition smap_of (sms : list smap) : option smap := fold_left compose_adaptation sms None.
Definition positions_of (rms : list (list nat)) : option (list (option nat)) :=
  fold_left compose_positions rms None.

(* ---- positions --------------------------------------------------------- *)

Lemma sources_compose : forall p rm sources,
  closure_sources (compose_positions p rm) sources =
  map (fun j => nth j (closure_sources p sources) (ITy 0)) rm.
Proof.
  intros [q|] rm sources; cbn [compose_positions closure_sources]; rewrite map_map; [|reflexivity].
  apply map_ext. intros j.
  set (f := fun o : option nat => match o with Some i => nth i sources (ITy 0) | None => ITy 0 end).
  change (f (nth j q None) = nth j (map f q) (f None)). symmetry. apply map_nth.
Qed.

Lemma positions_chain : forall rms sources, closure_sources (positions_of rms) sources = chain_sources rms sources.
Proof.
  intros rms sources. induction rms as [|rm rms IH] using rev_ind; [reflexivity|].
  unfold positions_of, chain_sources in *. rewrite !fold_left_app. cbn [fold_left].
  rewrite sources_compose, IH. reflexivity.
Qed.

(* The selection an application makes is the chained one. *)
Lemma selection_chain : forall rs rms sources,
  result_selection rs (positions_of rms) sources = rev (combine rs (chain_sources rms sources)).
Proof. intros rs rms sources. unfold result_selection. rewrite positions_chain. reflexivity. Qed.

(* ---- needs ------------------------------------------------------------- *)

Lemma select_lookup : forall sm d d' req, select_provisions d sm = Some d' ->
  lookup_provision req d' = match smap_source req sm with Some s => lookup_provision s d | None => None end.
Proof.
  induction sm as [|[r s] rest IH]; intros d d' req Sel; cbn [select_provisions] in Sel.
  - injection Sel as <-. reflexivity.
  - destruct (lookup_provision s d) as [en|] eqn:L; [|discriminate].
    destruct (select_provisions d rest) as [d0|] eqn:Sr; [|discriminate]. injection Sel as <-.
    unfold lookup_provision, install_provision. cbn [provision_values lookup_entry smap_source].
    destruct (item_eqb req r); [exact (eq_sym L)|exact (IH d d0 req Sr)].
Qed.

Lemma select_compose : forall sm d d' c e, select_provisions d sm = Some d' -> select_provisions d' c = Some e ->
  select_provisions d (map (fun q => (fst q, match smap_source (snd q) sm with Some s => s | None => snd q end)) c) = Some e.
Proof.
  intros sm d d' c. induction c as [|[r s] rest IH]; intros e Sel Sc; cbn [select_provisions map fst snd] in *; [exact Sc|].
  rewrite (select_lookup sm d d' s Sel) in Sc.
  destruct (smap_source s sm) as [s'|]; [|discriminate].
  destruct (lookup_provision s' d); [|discriminate].
  destruct (select_provisions d' rest) as [e0|] eqn:Sr; [|discriminate].
  rewrite (IH e0 Sel eq_refl). exact Sc.
Qed.

(* The composition lemma: a copy's selection then its source's adaptation
   is the composed adaptation. *)
Lemma adapt_compose : forall a sm d d' e, select_provisions d sm = Some d' -> adapt_provisions d' a = Some e ->
  adapt_provisions d (compose_adaptation a sm) = Some e.
Proof.
  intros [c|] sm d d' e Sel Ad; cbn [adapt_provisions compose_adaptation] in *.
  - exact (select_compose sm d d' c e Sel Ad).
  - injection Ad as <-. exact Sel.
Qed.

Lemma adapt_chain_snoc : forall d sms sm, adapt_chain d (sms ++ [sm]) =
  match select_provisions d sm with Some d' => adapt_chain d' sms | None => None end.
Proof.
  intros d sms sm. induction sms as [|s rest IH]; cbn [app adapt_chain].
  - destruct (select_provisions d sm); reflexivity.
  - rewrite IH. destruct (select_provisions d sm); reflexivity.
Qed.

Lemma adapt_chain_sound : forall sms d e, adapt_chain d sms = Some e -> adapt_provisions d (smap_of sms) = Some e.
Proof.
  intros sms. induction sms as [|sm sms IH] using rev_ind; intros d e Ch; [exact Ch|].
  unfold smap_of. rewrite fold_left_app. cbn [fold_left]. fold (smap_of sms).
  rewrite adapt_chain_snoc in Ch. destruct (select_provisions d sm) as [d'|] eqn:Sel; [|discriminate].
  exact (adapt_compose _ sm d d' e Sel (IH d' e Ch)).
Qed.

(* Only a stuck chain differs: H's smap reads a key x the application did
   not select, for a need of H that F's linked smap (the root's) does not
   read. The chain is stuck; the composition does not look for x. *)
Lemma adapt_chain_strict : exists d sms,
  adapt_chain d sms = None /\ adapt_provisions d (smap_of sms) = Some empty_environment.
Proof. exists empty_environment, [[]; [(IVal 0, IVal 0)]]. split; reflexivity. Qed.
