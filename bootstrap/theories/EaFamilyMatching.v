(* Receiver matching visits the formal type structurally. The generated
   substitution binds exactly the polymorphic variables it visited. *)
From Stdlib Require Import List String Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaFamily EaTypesSound.
From NM Require EaBudget EaDependencyFuel EaControlSound EaFamilyUseSound EaOutputSound EaResultBudget EaTypes PublicCallRealize PublicContextFacts PublicTypeFacts PublicTypeRead WorldSound.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Import EaBudget.
Local Open Scope list_scope.

Local Fixpoint ty_ind_all (P : ty -> Prop)
    (Unit : P TUnit)
    (Record : forall fields, Forall (fun q => P (snd q)) fields -> P (TRecord fields))
    (Machine : forall n, P (TMachine n))
    (Abstract : forall n, P (TAbs n))
    (Nominal : forall n args, Forall (fun q => P (snd q)) args -> P (TNom n args))
    (Union : forall members, Forall P members -> P (TUnion members))
    (t : ty) {struct t} : P t.
Proof.
  destruct t as [n|fields| |abstract|tag args|members].
  - apply Machine.
  - apply Record. induction fields as [|[label field] rest IH]; constructor; [exact (ty_ind_all P Unit Record Machine Abstract Nominal Union field)|exact IH].
  - exact Unit.
  - apply Abstract.
  - apply Nominal. induction args as [|[key arg] rest IH]; constructor; [exact (ty_ind_all P Unit Record Machine Abstract Nominal Union arg)|exact IH].
  - apply Union. induction members as [|member rest IH]; constructor; [exact (ty_ind_all P Unit Record Machine Abstract Nominal Union member)|exact IH].
Defined.

Definition MatchDomain (marks uses : list nat) (before after : subst) : Prop :=
  (NoDup (map fst before) -> NoDup (map fst after)) /\
  forall g, In g (map fst after) <-> In g (map fst before) \/ (In g marks /\ In g uses).

Local Lemma domain_empty : forall marks s, MatchDomain marks [] s s.
Proof. intros marks s. split; [auto|intros; cbn; tauto]. Qed.

Local Lemma domain_unchanged : forall marks uses s,
  (forall g, In g marks -> In g uses -> In g (map fst s)) ->
  MatchDomain marks uses s s.
Proof. intros marks uses s Known. split; [auto|]. intros g. specialize (Known g). tauto. Qed.

Local Lemma domain_compose : forall marks a b s m f,
  MatchDomain marks a s m -> MatchDomain marks b m f -> MatchDomain marks (a ++ b) s f.
Proof.
  intros marks a b s m f [DistinctA KeysA] [DistinctB KeysB]. split; [auto|].
  intros key. rewrite KeysB, KeysA, in_app_iff. tauto.
Qed.

Local Lemma lookup_subst_keys : forall key s,
  In key (map fst s) <-> exists t, lookup_subst key s = Some t.
Proof.
  intros key s. induction s as [|[k v] s IH]; cbn [map fst lookup_subst].
  - split; [contradiction|intros (? & Impossible); discriminate].
  - destruct (Nat.eqb_spec key k) as [->|Different]; [split; [intros; now exists v|intros; now left]|].
    rewrite <- IH. split; [intros [Equal|Present]; [congruence|exact Present]|now right].
Qed.

Local Lemma domain_add : forall marks key t s,
  In key marks -> lookup_subst key s = None -> MatchDomain marks [key] s (s ++ [(key, t)]).
Proof.
  intros marks key t s Mark Fresh. unfold MatchDomain. rewrite map_app. cbn [map fst].
  split.
  - intro Distinct. apply NoDup_app; [exact Distinct|repeat constructor; auto|].
    intros k Present [<-|[]].
    apply lookup_subst_keys in Present as [existing Found]. congruence.
  - intro k. rewrite in_app_iff. cbn. destruct (Nat.eq_dec k key) as [->|Different]; intuition congruence.
Qed.

Local Lemma rows_domain : forall A (step : A -> A -> subst -> option subst) (uses : A -> list nat)
    marks formal,
  Forall (fun row => forall actual s out, step row actual s = Some out -> MatchDomain marks (uses row) s out) formal ->
  forall actual s out, match_rows step formal actual s = Some out ->
  MatchDomain marks (flat_map uses formal) s out.
Proof.
  intros A step uses marks formal Typed. induction Typed as [|row rest Row Rest IH];
    intros [|actual actuals] s out Run; try discriminate.
  - injection Run as <-. apply domain_empty.
  - cbn [match_rows] in Run. destruct (step row actual s) as [middle|] eqn:Head; [|discriminate].
    cbn [flat_map]. eapply domain_compose; [eapply Row; exact Head|eapply IH; exact Run].
Qed.

Local Lemma gens_record : forall fields,
  gens (TRecord fields) = flat_map (fun q => gens (snd q)) fields.
Proof. induction fields as [|[label t] rest IH]; [reflexivity|]. cbn [gens flat_map snd] in *. now rewrite IH. Qed.

Local Lemma gens_nominal : forall tag args,
  gens (TNom tag args) = flat_map (fun q => gens (snd q)) args.
Proof. intros tag args. induction args as [|[key t] rest IH]; [reflexivity|]. cbn [gens flat_map snd] in *. now rewrite IH. Qed.

Local Lemma gens_union : forall members, gens (TUnion members) = flat_map gens members.
Proof. intros values. induction values as [|t rest IH]; [reflexivity|]. cbn [gens flat_map] in *. now rewrite IH. Qed.

Theorem match_ty_domain : forall C marks formal actual s out,
  match_ty C marks formal actual s = Some out -> MatchDomain marks (gens formal) s out.
Proof.
  intros C marks formal. induction formal as [|fields Fields|machine|key|tag args Args|members Members]
    using ty_ind_all; intros actual s out Run; cbn [match_ty] in Run.
  - destruct (ty_eqb TUnit actual); [injection Run as <-; apply domain_empty|discriminate].
  - destruct actual as [|actual| | | |]; try discriminate.
    rewrite gens_record. eapply rows_domain; [|exact Run].
    apply Forall_forall. intros row Present other before after Step.
    apply Forall_forall with (x := row) in Fields; [|exact Present].
    cbn beta in Step. destruct (String.eqb (fst row) (fst other)); [eapply Fields; exact Step|discriminate].
  - destruct (ty_eqb (TMachine machine) actual); [injection Run as <-; apply domain_empty|discriminate].
  - destruct (nmem key marks) eqn:Marker.
    + apply nmem_spec in Marker.
      destruct (lookup_subst key s) as [v|] eqn:Found.
      * destruct (ty_eqb (canon C v) actual); [|discriminate]. injection Run as <-.
        apply domain_unchanged. intros x _ [<-|[]]. apply lookup_subst_keys. now exists v.
      * injection Run as <-. now apply domain_add.
    + apply EaDependencyFuel.nmem_false_spec in Marker.
      destruct (ty_eqb (canon C (TAbs key)) actual); [|discriminate]. injection Run as <-.
      apply domain_unchanged. intros x Present [<-|[]]. contradiction.
  - destruct actual as [| | | |other actual|]; try discriminate.
    destruct (Nat.eqb tag other); [|discriminate].
    rewrite gens_nominal. eapply rows_domain; [|exact Run].
    apply Forall_forall. intros row Present other_row before after Step.
    apply Forall_forall with (x := row) in Args; [|exact Present].
    cbn beta in Step.
    lazymatch type of Step with
    | (if ?test then _ else _) = _ => destruct test; [eapply Args; exact Step|discriminate]
    end.
  - destruct actual as [| | | | |actual]; try discriminate.
    rewrite gens_union. eapply rows_domain; [|exact Run].
    exact Members.
Qed.

Theorem read_off_one_domain : forall C marks formal receiver matching,
  read_off_one C marks formal receiver = Some matching ->
  NoDup (map fst matching) /\
  (forall g, In g (map fst matching) <-> In g marks /\ In g (gens formal)) /\
  canon C (app_subst matching formal) = receiver.
Proof.
  intros C marks formal receiver matching Run. unfold read_off_one in Run.
  destruct (match_ty C marks formal receiver []) as [rows|] eqn:Match; [|discriminate].
  destruct (ty_eqb (canon C (app_subst rows formal)) receiver) eqn:Equation; [|discriminate].
  injection Run as <-.
  destruct (match_ty_domain _ _ _ _ _ _ Match) as [Distinct Domain].
  split; [apply Distinct; constructor|].
  split; [intro g; rewrite Domain; simpl; tauto|now apply PublicTypeFacts.ty_eqb_spec].
Qed.

Definition instantiated (C : cx) (rows : subst) (t : ty) : ty := canon C (app_subst rows t).

Local Lemma instantiated_record : forall C rows fields,
  instantiated C rows (TRecord fields) =
  TRecord (map (fun q => (fst q, instantiated C rows (snd q))) fields).
Proof.
  intros C rows fields. induction fields as [|[label t] rest IH]; [reflexivity|].
  unfold instantiated, canon in *. cbn [app_subst map fst snd] in *. congruence.
Qed.

Local Lemma instantiated_nominal : forall C rows tag fields,
  instantiated C rows (TNom tag fields) =
  TNom tag (map (fun q => (fst q, instantiated C rows (snd q))) fields).
Proof.
  intros C rows tag fields. induction fields as [|[label t] rest IH]; [reflexivity|].
  unfold instantiated, canon in *. cbn [app_subst map fst snd] in *. congruence.
Qed.

Local Lemma instantiated_union : forall C rows members,
  instantiated C rows (TUnion members) = TUnion (map (instantiated C rows) members).
Proof.
  intros C rows members. induction members as [|t rest IH]; [reflexivity|].
  unfold instantiated, canon in *. cbn [app_subst map] in *. congruence.
Qed.

Definition MatchingAgrees (C : cx) (expected actual : subst) : Prop :=
  forall key value, lookup_subst key actual = Some value ->
  exists meant, lookup_subst key expected = Some meant /\ canon C value = canon C meant.

Definition MarkerRows (marks : list nat) (rows : subst) : Prop :=
  forall key, In key (map fst rows) -> In key marks.

Definition CoversMarkers (marks : list nat) (formal : ty) (rows : subst) : Prop :=
  forall key, In key marks -> In key (gens formal) -> In key (map fst rows).

Local Lemma agree_add : forall C expected acc key t,
  PublicContextFacts.CxSection C ->
  MatchingAgrees C expected acc -> lookup_subst key expected = Some t ->
  MatchingAgrees C expected (acc ++ [(key, canon C t)]).
Proof.
  intros C expected acc key t Section Agree Read other value Lookup.
  rewrite PublicCallRealize.lookup_subst_nlook', PublicTypeRead.nlook_app' in Lookup.
  destruct (nlook other acc) as [old|] eqn:Existing.
  - injection Lookup as <-. apply Agree. now rewrite PublicCallRealize.lookup_subst_nlook'.
  - cbn [nlook] in Lookup. destruct (Nat.eqb_spec other key) as [->|Different]; [|discriminate].
    injection Lookup as <-. exists t. split; [exact Read|].
    apply PublicContextFacts.section_canonical. exact Section.
Qed.

Local Lemma match_mapped_rows : forall A (step : A -> A -> subst -> option subst)
    C expected (target : A -> A) formal,
  Forall (fun row => forall acc, MatchingAgrees C expected acc ->
    exists out, step row (target row) acc = Some out /\ MatchingAgrees C expected out) formal ->
  forall acc, MatchingAgrees C expected acc ->
  exists out, match_rows step formal (map target formal) acc = Some out /\ MatchingAgrees C expected out.
Proof.
  intros A step C expected target formal Rows. induction Rows as [|row rest Head Rest IH];
    intros acc Agree; [now exists acc|].
  destruct (Head _ Agree) as (middle & Step & Agrees).
  destruct (IH _ Agrees) as (out & Tail & Agreed).
  exists out. split; [cbn [map match_rows]; now rewrite Step|exact Agreed].
Qed.

Theorem match_ty_complete : forall C marks expected formal acc,
  PublicContextFacts.CxSection C -> MarkerRows marks expected ->
  CoversMarkers marks formal expected -> MatchingAgrees C expected acc ->
  exists out, match_ty C marks formal (instantiated C expected formal) acc = Some out /\
    MatchingAgrees C expected out.
Proof.
  intros C marks expected formal. induction formal as [|fields Fields|machine|key|tag args Args|members Members]
    using ty_ind_all; intros acc Section Marked Covers Agree.
  - exists acc. split; [reflexivity|exact Agree].
  - rewrite instantiated_record. cbn [match_ty]. apply match_mapped_rows; [|exact Agree].
    apply Forall_forall. intros [label field] Present acc' Agrees.
    apply Forall_forall with (x := (label, field)) in Fields; [|exact Present].
    cbn [fst snd] in *. rewrite String.eqb_refl.
    apply Fields; try assumption.
    intros x Mark Occurs. apply Covers; [exact Mark|].
    rewrite gens_record. apply in_flat_map. exists (label, field). auto.
  - exists acc. split; [cbn [match_ty instantiated canon app_subst]; rewrite (proj2 (PublicTypeFacts.ty_eqb_spec _ _) eq_refl); reflexivity|exact Agree].
  - unfold instantiated. cbn [app_subst match_ty].
    destruct (nmem key marks) eqn:Marker.
    + apply nmem_spec in Marker.
      destruct (proj1 (lookup_subst_keys key expected) (Covers key Marker (or_introl eq_refl)))
        as (meaning & Found).
      rewrite Found.
      destruct (lookup_subst key acc) as [old|] eqn:Existing.
      * destruct (Agree _ _ Existing) as (want & Read & Equal).
        rewrite Found in Read. injection Read as <-.
        exists acc. split; [rewrite Equal, (proj2 (PublicTypeFacts.ty_eqb_spec _ _) eq_refl); reflexivity|exact Agree].
      * exists (acc ++ [(key, canon C meaning)]). split; [reflexivity|].
        now apply agree_add.
    + apply EaDependencyFuel.nmem_false_spec in Marker.
      destruct (lookup_subst key expected) as [meaning|] eqn:Found.
      * exfalso. apply Marker, Marked, lookup_subst_keys. now exists meaning.
      * exists acc. split; [rewrite (proj2 (PublicTypeFacts.ty_eqb_spec _ _) eq_refl); reflexivity|exact Agree].
  - rewrite instantiated_nominal. cbn [match_ty]. rewrite Nat.eqb_refl.
    apply match_mapped_rows; [|exact Agree].
    apply Forall_forall. intros [label arg] Present acc' Agrees.
    apply Forall_forall with (x := (label, arg)) in Args; [|exact Present].
    cbn [fst snd] in *. rewrite Nat.eqb_refl.
    apply Args; try assumption.
    intros x Mark Occurs. apply Covers; [exact Mark|].
    rewrite gens_nominal. apply in_flat_map. exists (label, arg). auto.
  - rewrite instantiated_union. cbn [match_ty]. apply match_mapped_rows; [|exact Agree].
    apply Forall_forall. intros member Present acc' Agrees.
    apply Forall_forall with (x := member) in Members; [|exact Present].
    apply Members; try assumption.
    intros x Mark Occurs. apply Covers; [exact Mark|].
    rewrite gens_union. apply in_flat_map. exists member. auto.
Qed.

Definition CanonLookup (C : cx) (rows : subst) (key : nat) : option ty :=
  option_map (canon C) (lookup_subst key rows).

Local Lemma instantiated_agree : forall C a b formal,
  (forall key, In key (gens formal) -> CanonLookup C a key = CanonLookup C b key) ->
  instantiated C a formal = instantiated C b formal.
Proof.
  intros C a b formal. induction formal as [|fields Fields|machine|key|tag args Args|members Members]
    using ty_ind_all; intro Agree; try reflexivity.
  - rewrite !instantiated_record. f_equal. apply map_ext_in.
    intros [label t] Present. cbn [fst snd]. f_equal.
    apply Forall_forall with (x := (label, t)) in Fields; [|exact Present].
    apply Fields. intros g Occurs. apply Agree.
    rewrite gens_record. apply in_flat_map. exists (label, t). auto.
  - specialize (Agree key (or_introl eq_refl)). unfold instantiated, CanonLookup in *.
    cbn [app_subst].
    destruct (lookup_subst key a), (lookup_subst key b); cbn [option_map] in Agree;
      try reflexivity; congruence.
  - rewrite !instantiated_nominal. f_equal. apply map_ext_in.
    intros [label t] Present. cbn [fst snd]. f_equal.
    apply Forall_forall with (x := (label, t)) in Args; [|exact Present].
    apply Args. intros g Occurs. apply Agree.
    rewrite gens_nominal. apply in_flat_map. exists (label, t). auto.
  - rewrite !instantiated_union. f_equal. apply map_ext_in.
    intros t Present. apply Forall_forall with (x := t) in Members; [|exact Present].
    apply Members. intros g Occurs. apply Agree.
    rewrite gens_union. apply in_flat_map. exists t. auto.
Qed.

Theorem read_off_one_complete : forall C marks formal receiver expected,
  PublicContextFacts.CxSection C ->
  (forall g, In g (map fst expected) <-> In g marks /\ In g (gens formal)) ->
  instantiated C expected formal = receiver ->
  exists matching, read_off_one C marks formal receiver = Some matching /\
    forall g, CanonLookup C matching g = CanonLookup C expected g.
Proof.
  intros C marks formal receiver expected Section Exact <-.
  assert (Marked : MarkerRows marks expected).
  { intros key Present. now apply Exact in Present. }
  assert (Covers : CoversMarkers marks formal expected).
  { intros key Marker Occurs. apply Exact. auto. }
  assert (Empty : MatchingAgrees C expected []) by (intros ? ? Impossible; discriminate).
  destruct (match_ty_complete C marks expected formal [] Section Marked Covers Empty)
    as (rows & Matched & Agree).
  destruct (match_ty_domain _ _ _ _ _ _ Matched) as [_ Domain].
  assert (Readings : forall g, CanonLookup C rows g = CanonLookup C expected g).
  {
    intro g. unfold CanonLookup.
    destruct (lookup_subst g rows) as [actual|] eqn:Actual.
    - destruct (Agree _ _ Actual) as (meant & Meaning & Equal). rewrite Meaning. cbn [option_map]. now rewrite Equal.
    - destruct (lookup_subst g expected) as [meant|] eqn:Meaning; [|reflexivity].
      exfalso.
      assert (Present : In g (map fst expected)) by (apply lookup_subst_keys; now exists meant).
      apply Exact in Present. assert (Read : In g (map fst rows)) by (apply Domain; now right).
      apply lookup_subst_keys in Read as [value Found]. congruence.
  }
  exists rows. split; [|exact Readings].
  unfold read_off_one. rewrite Matched.
  change ((if ty_eqb (instantiated C rows formal) (instantiated C expected formal)
      then Some rows else None) = Some rows).
  rewrite (instantiated_agree C rows expected formal (fun key _ => Readings key)).
  now rewrite (proj2 (PublicTypeFacts.ty_eqb_spec _ _) eq_refl).
Qed.

Local Lemma chosen_candidate : forall A (first : A) rest index,
  In (nth index (first :: rest) first) (first :: rest).
Proof.
  intros A first rest index.
  destruct (Nat.lt_ge_cases index (List.length (first :: rest))) as [Found|Default].
  - now apply nth_In.
  - rewrite nth_overflow by exact Default. now left.
Qed.

Local Lemma subst_canon_agree_sound : forall C a b,
  subst_canon_agree C a b = true -> forall key, CanonLookup C a key = CanonLookup C b key.
Proof.
  intros C a b Checked key. unfold subst_canon_agree in Checked.
  destruct (in_dec Nat.eq_dec key (map fst a ++ map fst b)) as [Present|Absent].
  - apply forallb_forall with (x := key) in Checked; [|exact Present].
    now apply EaControlSound.oty_eqb_exact in Checked.
  - unfold CanonLookup.
    destruct (lookup_subst key a) as [a_value|] eqn:Left,
      (lookup_subst key b) as [b_value|] eqn:Right; try reflexivity;
      exfalso; apply Absent; apply in_or_app.
    all: first [left; apply lookup_subst_keys; now exists a_value|
      right; apply lookup_subst_keys; now exists b_value].
Qed.

Definition receiver_candidates (C : cx) (marks : list nat) (receiver : ty)
    (method : nat) (keys : list item) : list subst :=
  flat_map (fun key => match key with
    | IMth formal m _ =>
        if Nat.eqb method m then
          match read_off_one C marks formal receiver with Some rows => [rows] | None => [] end
        else []
    | _ => []
    end) keys.

Local Lemma candidate_read : forall C marks receiver method keys matching,
  In matching (receiver_candidates C marks receiver method keys) ->
  exists formal instantiation, In (IMth formal method instantiation) keys /\
    read_off_one C marks formal receiver = Some matching.
Proof.
  intros C marks receiver method keys matching Found.
  unfold receiver_candidates in Found.
  apply in_flat_map in Found as (key & Present & Read).
  destruct key as [| | |formal m instantiation|]; try contradiction.
  destruct (Nat.eqb_spec method m) as [->|Different]; [|contradiction].
  destruct (read_off_one C marks formal receiver) as [rows|] eqn:Rows; [|contradiction].
  destruct Read as [<-|[]]. exists formal, instantiation. auto.
Qed.

Local Lemma matching_is_candidate : forall C marks receiver method keys formal instantiation rows,
  In (IMth formal method instantiation) keys ->
  read_off_one C marks formal receiver = Some rows ->
  In rows (receiver_candidates C marks receiver method keys).
Proof.
  intros C marks receiver method keys formal instantiation rows Present Read.
  unfold receiver_candidates. apply in_flat_map.
  exists (IMth formal method instantiation). split; [exact Present|].
  now rewrite Nat.eqb_refl, Read; left.
Qed.

Theorem family_read_off_sound : forall E bound,
  (forall symbol d, w_decl (e_W E) symbol = Some d -> symbol < bound) ->
  metadata_budget (e_W E) bound < e_metadata_fuel E ->
  EaFamilyUseSound.FamilyMatchingSound E.
Proof.
  intros E bound Bounded Budget C u receiver method matching Section Run.
  unfold family_read_off, formal_matches in Run. cbv zeta in Run.
  set (marks := EaTypes.poly (e_W E) (e_metadata_fuel E) u) in Run.
  destruct (provide_result E u (marks_cx C marks) (fun t => TAbs t))
    as [[result keys]|message] eqn:Result; [|discriminate].
  cbn [rbind] in Run.
  fold (receiver_candidates C marks receiver method keys) in Run.
  destruct (receiver_candidates C marks receiver method keys) as [|first rest] eqn:Candidates;
    [discriminate|].
  set (selected := nth (s_pick (e_strat E) 0 (List.length (first :: rest))) (first :: rest) first) in *.
  apply rbind_ok in Run as (checked & Check & Run).
  destruct (forallb (subst_canon_agree C selected) (first :: rest)) eqn:Agreement; [|discriminate Check].
  injection Run as <-.
  assert (Present : In selected (receiver_candidates C marks receiver method keys)).
  { rewrite Candidates. unfold selected. apply chosen_candidate. }
  destruct (candidate_read _ _ _ _ _ _ Present) as (formal & inst & Site & Read).
  destruct (read_off_one_domain _ _ _ _ _ Read) as (Distinct & Domain & Equation).
  exists marks, result, keys, formal, inst.
  split.
  { unfold marks. symmetry. apply EaResultBudget.poly_exact with (bound := bound); assumption. }
  split.
  { unfold marks_cx in Result. eapply EaOutputSound.provide_result_sound_at_budget; eassumption. }
  split; [exact Site|split; [exact Domain|split; [exact Distinct|split; [exact Equation|]]]].
  intros other other_inst expected Other OtherDomain Eq key.
  destruct (read_off_one_complete C marks other receiver expected Section OtherDomain Eq)
    as (computed & Computed & Reads).
  pose proof (matching_is_candidate C marks receiver method keys other other_inst computed Other Computed)
    as InCandidates.
  rewrite Candidates in InCandidates.
  apply forallb_forall with (x := computed) in Agreement; [|exact InCandidates].
  change (CanonLookup C expected key = CanonLookup C selected key).
  rewrite <- (Reads key). symmetry. now apply subst_canon_agree_sound.
Qed.
