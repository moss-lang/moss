From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit EaBase EaTypes EaTypesSound.
From NM Require EaDependencyFuel EaExpand EaMain EaTypeScope PublicCallRealize PublicTypeRead PublicWorldFormation.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

(* Heads reached through fields and union members before following a tag's
   payload. A negative traversal must have visited every tag at such a head. *)
Inductive LayoutHead : ty -> nat -> subst -> Prop :=
| LHNom : forall tag args, LayoutHead (TNom tag args) tag args
| LHField : forall fields name t tag args,
    In (name, t) fields -> LayoutHead t tag args -> LayoutHead (TRecord fields) tag args
| LHMember : forall members t tag args,
    In t members -> LayoutHead t tag args -> LayoutHead (TUnion members) tag args.

Definition LayoutCovered (W : world) (target : nat) (seen : list (nat * subst)) (t : ty) : Prop :=
  forall tag args, LayoutHead t tag args ->
  tag <> target /\ (In (tag, args) seen \/ kind_sym W tag <> Some DKTag).

(* At the end of a negative traversal, a newly visited tag's payload is
   covered. This also accounts for back edges to ancestors on the stack. *)
Definition LayoutClosed (W : world) (target : nat) (before after : list (nat * subst)) : Prop :=
  forall tag args, In (tag, args) after ->
  In (tag, args) before \/
    forall payload, PayloadOf W tag args payload -> LayoutCovered W target after payload.

Local Lemma layout_cover_mono : forall W target before after t,
  incl before after -> LayoutCovered W target before t -> LayoutCovered W target after t.
Proof.
  intros W target before after t Included Covered tag args Head.
  destruct (Covered tag args Head) as [Different [Seen|NotTag]].
  - split; [exact Different|left; exact (Included _ Seen)].
  - split; [exact Different|right; exact NotTag].
Qed.

Local Lemma layout_closed_trans : forall W target a b c,
  incl b c -> LayoutClosed W target a b -> LayoutClosed W target b c ->
  LayoutClosed W target a c.
Proof.
  intros W target a b c Grown Before After tag args Present.
  destruct (After tag args Present) as [Old|Covered]; [|now right].
  destruct (Before tag args Old) as [Initial|Covered]; [now left|].
  right. intros payload Read. eapply layout_cover_mono; [exact Grown|exact (Covered _ Read)].
Qed.

Lemma covered_layout_avoids : forall W target seen t,
  LayoutCovered W target seen t -> LayoutClosed W target [] seen ->
  ~ LayoutReaches W t target.
Proof.
  intros W target seen t Covered Closed Reaches. revert Covered Closed.
  induction Reaches as [tag args|tag args payload target Payload Reaches IH|
    fields field t target Member Reaches IH|members t target Member Reaches IH];
    intros Covered Closed.
  - exact (proj1 (Covered tag args (LHNom tag args)) eq_refl).
  - destruct (Covered tag args (LHNom tag args)) as [_ [Seen|NotTag]].
    + destruct (Closed tag args Seen) as [[]|CoveredPayload].
      exact (IH (CoveredPayload payload Payload) Closed).
    + exact (NotTag (PublicTypeRead.payload_tag _ _ _ _ Payload)).
  - apply IH; [|exact Closed].
    intros tag args Head. eapply Covered. eapply LHField; eassumption.
  - apply IH; [|exact Closed].
    intros tag args Head. eapply Covered. eapply LHMember; eassumption.
Qed.

Local Fixpoint layout_list
    (step : list (nat * subst) -> ty -> er (bool * list (nat * subst)))
    (types : list ty) (seen : list (nat * subst)) : er (bool * list (nat * subst)) :=
  match types with
  | [] => Ok (false, seen)
  | t :: rest =>
      let! (found, after) := step seen t in
      if found then Ok (true, after) else layout_list step rest after
  end.

Local Lemma layout_list_unfolded : forall
    (step : list (nat * subst) -> ty -> er (bool * list (nat * subst))) types seen,
  (let fix visit (ts : list ty) (seen : list (nat * subst)) :=
     match ts with
     | [] => Ok (false, seen)
     | t :: rest =>
         let! (found, after) := step seen t in
         if found then Ok (true, after) else visit rest after
     end in visit types seen) = layout_list step types seen.
Proof.
  intros step types. induction types as [|t rest IH]; intro seen; [reflexivity|].
  cbn [layout_list]. unfold rbind at 1 2.
  destruct (step seen t) as [[found after]|message]; [destruct found|];
    try reflexivity. apply IH.
Qed.

Local Lemma layout_list_negative : forall W target step,
  (forall before t after, step before t = Ok (false, after) ->
    incl before after /\ LayoutCovered W target after t /\ LayoutClosed W target before after) ->
  forall types before after, layout_list step types before = Ok (false, after) ->
  incl before after /\ Forall (LayoutCovered W target after) types /\ LayoutClosed W target before after.
Proof.
  intros W target step Sound types. induction types as [|t rest IH]; intros before after Run.
  - injection Run as <-. split; [apply incl_refl|]. split; [constructor|].
    intros tag args Present. now left.
  - cbn [layout_list] in Run. apply rbind_ok in Run as ([found middle] & Head & Run).
    destruct found; [discriminate|].
    destruct (Sound _ _ _ Head) as (FirstGrowth & Covered & FirstClosed).
    destruct (IH _ _ Run) as (LastGrowth & Others & LastClosed).
    split; [eapply incl_tran; eassumption|].
    split.
    + constructor; [eapply layout_cover_mono; eassumption|exact Others].
    + eapply layout_closed_trans; eassumption.
Qed.

Local Lemma layout_negative_leaf : forall W target before t,
  (forall tag args, ~ LayoutHead t tag args) ->
  incl before before /\ LayoutCovered W target before t /\ LayoutClosed W target before before.
Proof.
  intros W target before t NoHead. split; [apply incl_refl|]. split.
  - intros tag args Head. contradiction (NoHead _ _ Head).
  - intros tag args Present. now left.
Qed.

Local Lemma nominal_covered : forall W target seen tag args,
  tag <> target ->
  (In (tag, args) seen \/ kind_sym W tag <> Some DKTag) ->
  LayoutCovered W target seen (TNom tag args).
Proof.
  intros W target seen tag args Different Visited u xs Head.
  inversion Head; subst. auto.
Qed.

Theorem layout_reaches_negative : forall W fuel k before target t after,
  EaExpand.layout_reaches W fuel k before target t = Ok (false, after) ->
  incl before after /\ LayoutCovered W target after t /\ LayoutClosed W target before after.
Proof.
  intros W fuel k. induction k as [|k IH]; intros before target t after Run; [discriminate|].
  destruct t as [machine|fields| |abstract|tag args|members]; cbn [EaExpand.layout_reaches] in Run;
    try (injection Run as <-; apply layout_negative_leaf; intros ? ? Head; inversion Head).
  - rewrite layout_list_unfolded in Run.
    destruct (layout_list_negative W target _ (fun start ty finish => IH start target ty finish)
      _ _ _ Run) as (Growth & Covered & Closed).
    split; [exact Growth|split; [|exact Closed]].
    intros u xs Head. inversion Head as [|? name t ? ? Present Inner|]; subst.
    apply Forall_forall with (x := t) in Covered.
    + exact (Covered _ _ Inner).
    + exact (in_map snd _ _ Present).
  - destruct (Nat.eqb tag target) eqn:Target; [discriminate|].
    apply Nat.eqb_neq in Target.
    destruct (existsb (fun q => Nat.eqb (fst q) tag && EaExpand.args_eqb (snd q) args) before)
      eqn:Seen.
    + injection Run as <-. split; [apply incl_refl|]. split.
      * apply nominal_covered; [exact Target|left].
        apply existsb_exists in Seen as [[seen_tag seen_args] [Member Equal]].
        cbn [fst snd] in Equal. apply andb_true_iff in Equal as [Key Arguments].
        apply Nat.eqb_eq in Key. change (subst_eqb seen_args args = true) in Arguments.
        apply PublicCallRealize.subst_eqb_spec in Arguments. now subst.
      * intros u xs Present. now left.
    + destruct (kind_is W tag DKTag) eqn:Tag.
      2: {
        injection Run as <-. split; [apply incl_refl|]. split.
        - apply nominal_covered; [exact Target|right].
          intro Kind. apply kind_is_spec in Kind. congruence.
        - intros u xs Present. now left.
      }
      destruct (payload_of W fuel tag args) as [payload|message] eqn:Read; [|discriminate].
      apply rbind_ok in Run as ([found finish] & Recursive & Run). inversion Run; subst found finish.
      destruct (IH _ _ _ _ Recursive) as (Growth & Covered & Closed).
      split.
      * intros row Present. apply Growth. now right.
      * split.
        -- apply nominal_covered; [exact Target|left]. apply Growth. now left.
        -- intros u xs Present.
           destruct (Closed u xs Present) as [[This|Old]|Checked].
           ++ right. intros other Derivation. injection This as <- <-.
              assert (Equal : payload = other).
              { eapply PublicTypeRead.payload_det; [eapply payload_of_sound; exact Read|exact Derivation]. }
              now rewrite <- Equal.
           ++ now left.
           ++ now right.
  - rewrite layout_list_unfolded in Run.
    destruct (layout_list_negative W target _ (fun start ty finish => IH start target ty finish)
      _ _ _ Run) as (Growth & Covered & Closed).
    split; [exact Growth|split; [|exact Closed]].
    intros u xs Head. inversion Head as [| |? t ? ? Present Inner]; subst.
    apply Forall_forall with (x := t) in Covered; [exact (Covered _ _ Inner)|exact Present].
Qed.

Theorem layout_reaches_false : forall W fuel k target t after,
  EaExpand.layout_reaches W fuel k [] target t = Ok (false, after) ->
  ~ LayoutReaches W t target.
Proof.
  intros W fuel k target t after Run.
  destruct (layout_reaches_negative _ _ _ _ _ _ _ Run) as (_ & Covered & Closed).
  eapply covered_layout_avoids; eassumption.
Qed.

Theorem finite_layouts_sound : forall W fuel bound,
  (forall tag payload, PayloadOf W tag (as_written (w_reqs W tag)) payload -> tag < bound) ->
  (forall tag payload, PayloadOf W tag (as_written (w_reqs W tag)) payload ->
    payload_of W fuel tag (as_written (w_reqs W tag)) = Ok payload) ->
  EaExpand.finite_layouts W fuel bound = Ok tt -> FiniteLayouts W.
Proof.
  intros W fuel bound Bounded Reads Run tag payload Payload.
  unfold EaExpand.finite_layouts in Run.
  apply rbind_ok in Run as (checked & Checks & _).
  assert (Present : In tag (seq 0 bound)) by
    (apply in_seq; pose proof (Bounded _ _ Payload); lia).
  destruct (EaDependencyFuel.rmap_member_read _ _ _ _ _ Checks _ Present)
    as (result & Checked).
  assert (IsTag : kind_is W tag DKTag = true) by
    (apply kind_is_spec; eapply PublicTypeRead.payload_tag; exact Payload).
  change (PayloadOf W tag (as_written (w_reqs W tag)) payload) in Payload.
  unfold as_written in Reads.
  rewrite IsTag, (Reads _ _ Payload) in Checked.
  apply rbind_ok in Checked as ([reaches seen] & Search & Check).
  destruct reaches; [discriminate|].
  eapply layout_reaches_false. exact Search.
Qed.

Local Lemma as_written_lookup : forall keys key, In key keys ->
  nlook key (as_written keys) = Some (TAbs key).
Proof.
  intros keys. induction keys as [|k keys IH]; intros key Present; [contradiction|].
  cbn [as_written map nlook]. destruct (Nat.eqb_spec key k) as [->|Different]; [reflexivity|].
  apply IH. destruct Present as [Equal|Present]; [congruence|exact Present].
Qed.

Local Lemma declarations_check_tag_payload : forall E bound tag d n params syntax,
  EaMain.decls_form E bound = Ok tt ->
  tag < bound -> w_decl (e_W E) tag = Some d -> d_ast d = ADTag n params syntax ->
  exists payload, payload_of (e_W E) (e_fuel E) tag (EaMain.self_all E bound) = Ok payload.
Proof.
  intros E bound tag d n params syntax Run Bounded Declared Ast.
  unfold EaMain.decls_form in Run. apply rbind_ok in Run as (checked & Checks & _).
  assert (Present : In tag (seq 0 bound)) by (apply in_seq; lia).
  destruct (EaDependencyFuel.rmap_member_read _ _ _ _ _ Checks _ Present) as (result & Check).
  destruct (EaMain.decl_forms E bound tag) as [formed|message] eqn:Formed; [|discriminate Check].
  unfold EaMain.decl_forms in Formed. rewrite Declared, Ast in Formed.
  apply rbind_ok in Formed as (fields & Fields & Formed).
  apply rbind_ok in Formed as (payload & Read & _).
  exists payload. unfold payload_of, decl_of. rewrite Declared. cbn [rbind].
  rewrite Ast, Fields. exact Read.
Qed.

Theorem declarations_cover_payload_reads : forall bundle E bound,
  WorldOf bundle (e_W E) ->
  (forall symbol d, w_decl (e_W E) symbol = Some d -> symbol < bound) ->
  EaMain.decls_form E bound = Ok tt ->
  forall tag payload, PayloadOf (e_W E) tag (as_written (w_reqs (e_W E) tag)) payload ->
  payload_of (e_W E) (e_fuel E) tag (as_written (w_reqs (e_W E) tag)) = Ok payload.
Proof.
  intros bundle E bound World Bounded Declarations tag payload Payload.
  inversion Payload as [s xs d n params syntax pay Declared Ast Typed]; subst.
  destruct (declarations_check_tag_payload E bound tag d n params syntax Declarations
    (Bounded _ _ Declared) Declared Ast) as (other & Read).
  unfold payload_of, decl_of in Read. rewrite Declared in Read. cbn [rbind] in Read.
  rewrite Ast in Read. apply rbind_ok in Read as (fields & Fields & Read).
  assert (Requirements : forall key, FreeIn (e_W E) (w_reqs (e_W E)) (d_mod d) syntax key ->
      In key (w_reqs (e_W E) tag)).
  { intros key Free. apply (proj2 (@wo_reqs_closed _ _ World tag d params syntax Declared
      ltac:(unfold nominal_body; now rewrite Ast))). exact Free. }
  assert (Same : EaTypeScope.ReadsSame (e_W E) (d_mod d) syntax
      (EaMain.self_all E bound) (as_written (w_reqs (e_W E) tag))).
  {
    intros key Free. pose proof (Requirements key Free) as Required.
    rewrite as_written_lookup by exact Required.
    change (nlook key (as_written (filter (fun t => kind_is (e_W E) t DKType) (seq 0 bound))) =
      Some (TAbs key)).
    apply as_written_lookup. apply filter_In.
    assert (Kind : kind_sym (e_W E) key = Some DKType).
    { eapply (PublicWorldFormation.reqs_typed bundle _ World tag); [rewrite Declared; discriminate|exact Required]. }
    split; [|now apply kind_is_spec].
    apply in_seq. unfold kind_sym in Kind.
    destruct (w_decl (e_W E) key) as [declaration|] eqn:Key; [|discriminate Kind].
    pose proof (Bounded _ _ Key). lia.
  }
  pose proof (proj1 (EaTypeScope.type_readers_narrow (e_W E)
    (@wo_reqs_closed _ _ World) (e_fuel E)) _ _ _ _ _ _ Same Read) as Narrow.
  assert (Equal : other = payload).
  { eapply PublicTypeRead.tyof_det; [eapply ty_of_sound; exact Narrow|exact Typed]. }
  subst other. unfold payload_of, decl_of. rewrite Declared. cbn [rbind].
  rewrite Ast, Fields. exact Narrow.
Qed.
