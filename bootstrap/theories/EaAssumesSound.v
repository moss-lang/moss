(* Fresh assumption readings and the declaration-level assumption check. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import EaBudget.
From NM Require Import Audit EaBase EaCall EaAssumptionAtoms EaCtx EaMain EaTypesSound EaContextTableSound.
From NM Require AllocationFacts PublicMerge PublicFormation PublicContextFacts PublicSubstFacts
  PublicTypeFacts PublicTypeRead PublicCallRealize EaMentionsSound EaRootSound EaDependencyFuel EaCallSound.
From NM Require EaWorldSound EaMainSymbols EaMetadataSound EaSatisfiesSound EaEntriesSound EaUnifySound ElabWorld NMap.
Import Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration ListNotations.
Local Open Scope string_scope.
Local Open Scope list_scope.

Lemma scope_floor_world : forall support bound, bound <= scope_floor bound support.
Proof.
  induction support as [|g support IH]; intro bound; [apply Nat.le_refl|].
  change (bound <= Nat.max (S g) (scope_floor bound support)).
  eapply Nat.le_trans; [apply IH|apply Nat.le_max_r].
Qed.

Lemma scope_floor_support : forall support bound g,
  In g support -> g < scope_floor bound support.
Proof.
  induction support as [|x support IH]; intros bound g Present; [contradiction|].
  change (g < Nat.max (S x) (scope_floor bound support)).
  destruct Present as [<-|Present].
  - apply Nat.le_max_l.
  - eapply Nat.lt_le_trans; [apply IH; exact Present|apply Nat.le_max_r].
Qed.

Lemma class_unread_spec : forall C Cs z,
  class_unread C Cs z = true <->
    (forall y, canon Cs (TAbs y) = TAbs z -> nlook y (c_ty C) = None).
Proof.
  intros C Cs z. unfold class_unread, has_bound_reading. rewrite negb_true_iff.
  split.
  - intros Absent y Equal. destruct (nlook y (c_ty C)) as [t|] eqn:Read; [exfalso|reflexivity].
    pose proof (PublicTypeRead.nlook_in _ _ _ _ Read) as Present.
    assert (Found : existsb (fun q => ty_eqb (canon Cs (TAbs (fst q))) (TAbs z)) (c_ty C) = true).
    { apply existsb_exists. exists (y, t). split; [exact Present|].
      cbn [fst]. apply PublicTypeFacts.ty_eqb_spec. exact Equal. }
    congruence.
  - intros Absent. destruct (existsb (fun q => ty_eqb (canon Cs (TAbs (fst q))) (TAbs z)) (c_ty C))
      eqn:Found; [exfalso|reflexivity].
    apply existsb_exists in Found as [[y t] [Present Equal]].
    apply PublicTypeFacts.ty_eqb_spec in Equal. cbn [fst] in Equal.
    specialize (Absent y Equal). apply (PublicSubstFacts.lookup_subst_in y (c_ty C)).
    + apply (in_map fst) in Present. exact Present.
    + rewrite PublicCallRealize.lookup_subst_nlook'. exact Absent.
Qed.

Lemma callee_leaves_spec : forall Cs z,
  In z (callee_leaves Cs) <->
  exists x, In x (map fst (c_ty Cs)) /\ In z (gens (canon Cs (TAbs x))).
Proof.
  intros Cs z. unfold callee_leaves. rewrite in_flat_map. split.
  - intros [[x t] [Present Leaf]]. exists x. split; [exact (in_map fst _ _ Present)|exact Leaf].
  - intros [x [Present Leaf]]. apply in_map_iff in Present as [[y t] [Equal Present]].
    cbn in Equal. subst y. exists (x, t). split; assumption.
Qed.

Lemma expanded_class_occurs : forall Cs r,
  Solved (c_merge Cs) -> PublicContextFacts.CxSection Cs ->
  (exists x z, In x (map fst (c_ty Cs)) /\ In z (gens (expand (c_merge Cs) (TAbs x))) /\
    MergeEq (c_merge Cs) (TAbs r) (TAbs z)) <->
  (exists x z, In x (map fst (c_ty Cs)) /\ In z (gens (canon Cs (TAbs x))) /\
    MergeEq (c_merge Cs) (TAbs r) (TAbs z)).
Proof.
  intros Cs r Solved Section.
  assert (Expanded : forall x, expand (c_merge Cs) (canon Cs (TAbs x)) = canon Cs (TAbs x)).
  { intros x. apply PublicFormation.section_output_expanded; assumption. }
  assert (Equivalent : forall x, MergeEq (c_merge Cs) (TAbs x) (canon Cs (TAbs x))).
  { exact (proj1 Section). }
  split; intros (x & z & Key & Leaf & Class).
  - destruct (@PublicMerge.expanded_generations_by_class (c_merge Cs) Solved
      (TAbs x) (canon Cs (TAbs x)) (Equivalent x) z Leaf) as [g [InG Eq]].
    rewrite Expanded in InG. exists x, g. repeat split; try assumption.
    eapply METrans; eassumption.
  - rewrite <- Expanded in Leaf.
    destruct (@PublicMerge.expanded_generations_by_class (c_merge Cs) Solved
      (canon Cs (TAbs x)) (TAbs x) (MESym (Equivalent x)) z Leaf) as [g [InG Eq]].
    exists x, g. repeat split; try assumption. eapply METrans; eassumption.
Qed.

Lemma absent_classes_spec : forall C Cs r,
  Solved (c_merge Cs) -> PublicContextFacts.CxSection Cs ->
  MissingAtomicClass C Cs r <->
    exists z, canon Cs (TAbs r) = TAbs z /\ In z (absent_classes C Cs).
Proof.
  intros C Cs r Solved Section.
  unfold MissingAtomicClass, absent_classes.
  rewrite (expanded_class_occurs Cs r Solved Section).
  split.
  - intros [(x & z & Present & Leaf & Class) Absent]. exists z.
    assert (Fixed : canon Cs (TAbs z) = TAbs z).
    { eapply PublicFormation.subst_fixed_gens; [|exact Leaf].
      apply PublicContextFacts.section_canonical. exact Section. }
    apply (PublicContextFacts.context_canon_iff_mergeeq Cs Section) in Class.
    rewrite Fixed in Class. split; [exact Class|].
    apply filter_In. split.
    + apply EaMentionsSound.sort_uniq_members, callee_leaves_spec. now exists x.
    + apply class_unread_spec. intros y Equal.
      destruct (nlook y (c_ty C)) as [value|] eqn:Read; [exfalso|reflexivity].
      apply Absent. exists y. split; [|congruence].
      apply (PublicContextFacts.context_canon_iff_mergeeq Cs Section). congruence.
  - intros (z & Class & Member). apply filter_In in Member as [Leaf Unread].
    apply EaMentionsSound.sort_uniq_members, callee_leaves_spec in Leaf as [x [Present Leaf]].
    assert (Fixed : canon Cs (TAbs z) = TAbs z).
    { eapply PublicFormation.subst_fixed_gens; [|exact Leaf].
      apply PublicContextFacts.section_canonical. exact Section. }
    split.
    + exists x, z. repeat split; try assumption.
      apply (PublicContextFacts.context_canon_iff_mergeeq Cs Section). congruence.
    + intros [y [Eq Bound]]. apply Bound.
      apply (proj1 (class_unread_spec C Cs z) Unread y).
      apply (PublicContextFacts.context_canon_iff_mergeeq Cs Section) in Eq. congruence.
Qed.

Lemma choose_atom_spec : forall C Cs bound r g,
  choose_assumption_atom C Cs bound r = Some g <->
  exists z, canon Cs (TAbs r) = TAbs z /\ In z (absent_classes C Cs) /\
    g = assumption_atom_start C Cs bound + z.
Proof.
  intros C Cs bound r g. unfold choose_assumption_atom.
  destruct (absent_classes C Cs) as [|first rest];
    destruct (canon Cs (TAbs r)) as [m|fs| |a|n args|ts] eqn:Read;
    try (split; [discriminate|intros (? & Bad & _); discriminate]).
  { split; [discriminate|intros (? & _ & [] & _)]. }
  destruct (nmem a (first :: rest)) eqn:Present.
  - apply EaTypesSound.nmem_spec in Present. split.
    + intro Equal. injection Equal as <-. now exists a.
    + intros (z & Equal & _ & ->). injection Equal as <-. reflexivity.
  - split; [discriminate|]. intros (z & Equal & Member & _).
    injection Equal as <-. apply EaTypesSound.nmem_spec in Member. congruence.
Qed.

Theorem choose_assumption_atom_sound : forall W C Cs bound,
  Solved (c_merge Cs) -> PublicContextFacts.CxSection Cs ->
  (forall g d, w_decl W g = Some d -> g < bound) ->
  AssumptionAllocation C Cs (assumption_scope W C Cs) (choose_assumption_atom C Cs bound).
Proof.
  intros W C Cs bound Solved Section WorldBound.
  set (support := context_type_support C ++ context_type_support Cs).
  set (floor := scope_floor bound support).
  set (classes := absent_classes C Cs).
  set (encode := fun z => floor + z).
  set (batch := map encode classes).
  set (before := assumption_scope W C Cs).
  exists batch, (extend_allocation_scope before batch).
  split.
  - split; [|reflexivity]. split.
    + apply AllocationFacts.allocation_rename_nodup.
      * unfold encode. lia.
      * unfold classes, absent_classes. apply NoDup_filter, EaRootSound.ascending_nodup,
          EaDependencyFuel.sort_uniq_ascending.
    + apply Forall_forall. intros g Member.
      apply in_map_iff in Member as [z [<- Present]]. split.
      * change (w_decl W (encode z) = None).
        destruct (w_decl W (encode z)) as [d|] eqn:Declared; [exfalso|reflexivity].
        pose proof (scope_floor_world support bound).
        pose proof (WorldBound _ _ Declared). unfold encode, floor in *. lia.
      * change (~ In (encode z) support). intro Reserved.
        pose proof (scope_floor_support support bound _ Reserved). unfold encode, floor in *. lia.
  - split.
    + intro r. rewrite (absent_classes_spec _ _ _ Solved Section). split.
      * intros [z [Class Member]]. exists (encode z). apply choose_atom_spec.
        exists z. repeat split; assumption || reflexivity.
      * intros [g Read]. apply choose_atom_spec in Read as (z & Class & Member & _).
        now exists z.
    + split.
      * intro g. split.
        -- intro Member. apply in_map_iff in Member as [z [<- Present]]. exists z.
           apply choose_atom_spec. exists z. repeat split; try assumption; try reflexivity.
           unfold classes, absent_classes in Present. apply filter_In in Present as [Leaf _].
           apply EaMentionsSound.sort_uniq_members, callee_leaves_spec in Leaf as [x [_ Leaf]].
           eapply PublicFormation.subst_fixed_gens; [|exact Leaf].
           apply PublicContextFacts.section_canonical. exact Section.
        -- intros [r Read]. apply choose_atom_spec in Read as (z & Class & Member & ->).
           unfold batch. apply in_map. exact Member.
      * intros r r' g g' Read Read'.
        apply choose_atom_spec in Read as (z & Class & Member & ->).
        apply choose_atom_spec in Read' as (z' & Class' & Member' & ->).
        rewrite <- (PublicContextFacts.context_canon_iff_mergeeq Cs Section).
        rewrite Class, Class'. split; intro Equal; [f_equal; lia|injection Equal as ->; reflexivity].
Qed.

Theorem assumption_theta_sound : forall W who C Cs bound s th,
  PublicContextFacts.CxSection C -> Solved (c_merge Cs) -> PublicContextFacts.CxSection Cs ->
  (forall g d, w_decl W g = Some d -> g < bound) ->
  assumption_theta W who C Cs bound = Ok th ->
  CallTheta W C Cs s th /\ AssumptionReadings W C Cs th.
Proof.
  intros W who C Cs bound s th Caller Solved Callee WorldBound Run.
  set (atom := choose_assumption_atom C Cs bound) in *.
  destruct (EaCallSound.call_theta_with_sound W who C Cs (assumption_class_read C Cs atom)
    s th Caller Callee Run) as [-> Theta].
  split; [exact Theta|]. exists atom.
  split; [apply choose_assumption_atom_sound; assumption|].
  intros r a Present Chosen. rewrite (PublicSubstFacts.lookup_on_keys _ _ _ Present).
  destruct (proj1 (choose_atom_spec C Cs bound r a) Chosen) as (z & Class & Member & Encoded).
  assert (Unread : nlook r (c_ty C) = None).
  { unfold absent_classes in Member. apply filter_In in Member as [_ Unread].
    exact (proj1 (class_unread_spec C Cs z) Unread r Class). }
  assert (Same : atom z = Some a).
  { apply choose_atom_spec. exists z. split; [|split; assumption].
    rewrite <- Class. apply PublicContextFacts.section_canonical. exact Callee. }
  unfold call_read, bound_in, assumption_class_read. now rewrite Unread, Class, Same.
Qed.

Lemma ctxof_class_reading : forall W s C ns, CtxOf W s C ns ->
  Solved (c_merge C) /\ PublicContextFacts.CxSection C.
Proof.
  intros W s C ns (d & es & tm & er & tmr & sigma & M & _ & _ & _ & _ & Merge & Section & -> & _).
  split; [exact (proj1 Merge)|exact Section].
Qed.

Lemma known_types_preserve_section : forall values C,
  PublicContextFacts.CxSection C ->
  PublicContextFacts.CxSection (fold_left (fun C q => set_ty C (fst q) (snd q)) values C).
Proof.
  induction values as [|[x t] values IH]; intros C Section; [exact Section|].
  apply IH. exact Section.
Qed.

Definition AssumedCallee (W : world) (s : nat) : Prop :=
  kind_sym W s = Some DKFn \/ kind_sym W s = Some DKMeth \/ kind_sym W s = Some DKVal.

Definition EntryCallee (W : world) (e : entry) : Prop :=
  match e with NVal s _ _ | NFn s _ _ | NMth _ s _ _ => AssumedCallee W s | _ => True end.

Scheme item_entries_callee_ind := Induction for ItemEntries Sort Prop
with items_entries_callee_ind := Induction for ItemsEntries Sort Prop.
Combined Scheme entries_callee_ind from item_entries_callee_ind, items_entries_callee_ind.

Lemma entries_callees : forall W,
  (forall p ob tm sp es tm', ItemEntries W p ob tm sp es tm' -> Forall (EntryCallee W) es) /\
  (forall p ob tm its es tm', ItemsEntries W p ob tm its es tm' -> Forall (EntryCallee W) es).
Proof.
  intro W. apply (entries_callee_ind W
    (fun _ _ _ _ es _ _ => Forall (EntryCallee W) es)
    (fun _ _ _ _ es _ _ => Forall (EntryCallee W) es)).
  - intros. constructor; [exact I|constructor].
  - intros. constructor.
  - intros. constructor; [|constructor]. cbn [EntryCallee]. right. right. unfold kind_sym.
    match goal with
    | Found : w_decl _ ?s = Some ?d, Ast : d_ast ?d = ADVal _ _ _ |- _ =>
        rewrite Found; cbn; now rewrite Ast
    end.
  - intros. constructor; [|constructor]. cbn [EntryCallee]. left. assumption.
  - intros. constructor; [exact I|constructor].
  - intros p ob tm a pa dn r m rt g bs ap _ _ _ _ Sig _.
    constructor; [|constructor]. cbn [EntryCallee].
    destruct Sig as (d & receiver & name & ps & result & body & Found & Ast & _).
    unfold AssumedCallee, kind_sym. rewrite Found. cbn. rewrite Ast.
    destruct receiver; cbn; auto.
  - intros. assumption.
  - intros. constructor.
  - intros. apply Forall_app. auto.
  - intros. apply Forall_app. split; [assumption|]. constructor; [exact I|assumption].
Qed.

Lemma assumed_application_callee : forall W e s a,
  EntryCallee W e -> assumed_application e = Some (s, a) -> AssumedCallee W s.
Proof.
  intros W [] target instantiation Kind App; cbn [assumed_application] in App; try discriminate;
    injection App as <- <-; exact Kind.
Qed.

Section Ordered.
Variable E : env.
Let W := e_W E.
Variable bound : nat.
Hypothesis Bounded : forall g d, w_decl W g = Some d -> g < bound.
Hypothesis Context : ContextTableSound W.
Hypothesis Enough : metadata_budget W bound < e_metadata_fuel E.

Lemma assumed_entry_sound : forall before tm e,
  EntryCallee W e ->
  assumed_entry E bound before tm e = Ok tt -> AssumedEntry W before tm e.
Proof.
  intros before tm e Kind Run s a App. unfold assumed_entry in Run. fold W in Run.
  rewrite App in Run.
  apply rbind_ok in Run as (checked & Present & Run).
  destruct (find (fun q => negb (is_some (snd q))) a) as [[g t]|] eqn:Missing; [discriminate|].
  split.
  { intros t o Member Equal. pose proof (find_none _ _ Missing (t, o) Member) as Absent.
    subst o. discriminate. }
  apply rbind_ok in Run as ([Cs needs] & Table & Run).
  destruct (w_context W s) as [[ReadCs ReadNeeds]|] eqn:Installed; [|discriminate].
  injection Table as <- <-. rename ReadCs into Cs. rename ReadNeeds into needs.
  assert (Formed : CtxOf W s Cs needs).
  { pose proof (Context _ _ _ Installed) as Reading.
    destruct (assumed_application_callee _ _ _ _ Kind App) as [K|[K|K]]; now rewrite K in Reading. }
  exists Cs, needs. split; [exact Formed|].
  destruct (direct_assumption Cs needs a) eqn:Direct.
  { left. unfold direct_assumption in Direct.
    destruct (c_sub Cs), needs; try discriminate.
    split; [reflexivity|]. split; [reflexivity|].
    intros t Required. apply in_map_iff in Required as (q & <- & Required).
    rewrite forallb_forall in Direct. specialize (Direct q Required).
    change (is_some (nlook (fst q) (known a)) = true) in Direct.
    intro MissingType. rewrite MissingType in Direct. discriminate. }
  right.
  all: apply rbind_ok in Run as ([sigma classes] & Merge & Run).
  all: destruct (EaUnifySound.merge_sound E _ _ _ Merge) as (MostGeneral & Classes & Section).
  all: set (C := entries_cx sigma classes tm before) in *.
  all: set (Cv := fold_left (fun C q => set_ty C (fst q) (snd q)) (known a) C) in *.
  all: destruct (ctxof_class_reading _ _ _ _ Formed) as [Solved Callee].
  all: apply rbind_ok in Run as (checked_types & TypeCheck & Run).
  all: assert (TypeForce : kind_sym W s <> Some DKMeth -> RequirementsInForce Cv Cs)
    by (intro NotMethod; destruct (EaTypes.kind_is W s DKMeth) eqn:Method;
        [apply kind_is_spec in Method; contradiction|];
        destruct checked_types; eapply EaCallSound.rule_b_sound; eassumption).
  all: apply rbind_ok in Run as (sm & Checked & _).
  all: set (who := String.append "assuming " (qs W s)) in *.
  all: lazymatch type of Checked with
    | resolve_assumption_error _ _ _ _ ?run = _ => set (check_needs := run) in Checked
    end.
  all: destruct check_needs as [selected|message] eqn:SatisfiesRun; [|discriminate].
  all: unfold check_needs in SatisfiesRun; injection Checked as <-.
  all: apply rbind_ok in SatisfiesRun as (th & Theta & Sat).
  all: destruct (assumption_theta_sound W who Cv Cs bound s th
    (known_types_preserve_section (known a) C Section) Solved Callee Bounded Theta) as [Call Reads].
  all: exists sigma, classes, th, selected; split; [exact MostGeneral|];
    split; [exact Classes|]; split; [exact Section|]; split; [exact TypeForce|];
    split; [exact Call|]; split; [|exact Reads].
  all: eapply EaSatisfiesSound.satisfies_sound; [|exact Sat];
    eapply EaSatisfiesSound.ctxof_subst_functional; exact Formed.
Qed.

Lemma ordered_entries_sound : forall es before tm,
  Forall (EntryCallee W) es ->
  ordered_entries E bound before tm es = Ok tt -> OrderedEntries W before tm es.
Proof.
  induction es as [|e rest IH]; intros before tm Kinds Run; [exact I|].
  cbn [ordered_entries] in Run. apply rbind_ok in Run as (checked & First & Rest).
  inversion Kinds; subst. split.
  - destruct checked. eapply assumed_entry_sound; eassumption.
  - eapply IH; eassumption.
Qed.

Lemma ordered_items_sound : forall p its,
  ordered_items E bound p its = Ok tt -> OrderedItems W p its.
Proof.
  intros p its Run. unfold ordered_items in Run.
  apply rbind_ok in Run as ([es tm] & Read & Checked).
  pose proof (proj2 (EaEntriesSound.entries_sound_at_budget E bound Bounded Enough (e_fuel E))
    _ _ _ _ _ _ _ Read) as Items.
  exists es, tm. split; [exact Items|].
  eapply ordered_entries_sound; [|exact Checked].
  eapply (proj2 (entries_callees W)); exact Items.
Qed.

Lemma same_assume_heads_sound : forall a b,
  same_assume_heads a b = true -> a = b.
Proof.
  induction a as [|a ar IH].
  - destruct b; [reflexivity|discriminate].
  - destruct b as [|b br]; [destruct a as [[p m]|]; discriminate|].
  destruct a as [[pa ma]|], b as [[pb mb]|]; cbn [same_assume_heads]; try discriminate.
  rewrite !andb_true_iff. intros [[Path Method] Rest].
  assert (pa = pb) as ->.
  { clear -Path. revert pb Path. induction pa as [|a ar IH]; destruct pb as [|b br];
      cbn [slist_eqb]; try discriminate; [reflexivity|].
    rewrite andb_true_iff, String.eqb_eq. intros [-> Tail]. f_equal. now apply IH. }
  assert (ma = mb) as -> by (destruct ma, mb; try discriminate; f_equal; now apply String.eqb_eq).
  now rewrite (IH _ Rest).
Qed.

Definition CheckedNesting (d : decl) : Prop :=
  OrderedItems W (d_mod d) (map assume_spec (d_assumes d)).

Lemma check_assume_nesting_sound : forall seen d next,
  Forall CheckedNesting seen ->
  check_assume_nesting E bound seen d = Ok next ->
  CheckedNesting d /\ Forall CheckedNesting next.
Proof.
  intros seen d next Seen Run. unfold check_assume_nesting in Run.
  destruct (existsb (same_assume_nesting d) seen) eqn:Reuse.
  - injection Run as <-. split; [|exact Seen].
    apply existsb_exists in Reuse as (other & Member & Same).
    unfold same_assume_nesting in Same. apply andb_true_iff in Same as [Module Heads].
    apply String.eqb_eq in Module. apply same_assume_heads_sound in Heads.
    unfold CheckedNesting. rewrite Module, Heads.
    apply Forall_forall with (x := other) in Seen; assumption.
  - apply rbind_ok in Run as (checked & New & Finish). injection Finish as <-.
    unfold assumption_module_error in New.
    destruct (ordered_items E bound (d_mod d) (map assume_spec (d_assumes d))) as [[]|msg] eqn:Order;
      [|discriminate].
    assert (NewScope : CheckedNesting d) by (unfold CheckedNesting; now apply ordered_items_sound).
    split; [exact NewScope|now constructor].
Qed.

Lemma check_assume_symbols_sound :
  (forall s C ns, assume_ctx E s = Ok (C, ns) -> AssumeCtx W s C ns) ->
  forall symbols seen, Forall CheckedNesting seen ->
  check_assume_symbols E bound seen symbols = Ok tt ->
  forall s k, In s symbols -> kind_sym W s = Some k ->
    (exists C ns, AssumeCtx W s C ns) /\
    forall d, w_decl W s = Some d ->
      CheckedNesting d /\
      match functor_parts (d_ast d) with
      | Some (_, inputs, _, _) => OrderedItems W (d_mod d) (map CISpec inputs)
      | None => True
      end.
Proof.
  intros Assumes symbols. induction symbols as [|first rest IH]; [intros ? ? ? ? ? []|].
  intros seen Seen Run s k Member Kind. cbn [check_assume_symbols] in Run. fold W in Run.
  destruct (w_decl W first) as [d|] eqn:Decl;
    [destruct (kind_of (d_ast d)) as [dk|] eqn:DKind|].
  all: try solve [destruct Member as [<-|Member];
    [unfold kind_sym in Kind; rewrite Decl in Kind; try rewrite DKind in Kind; discriminate
    |eapply IH; eassumption]].
  apply rbind_ok in Run as (formed & Form & Run).
  apply rbind_ok in Run as (next & Nesting & Run).
  apply rbind_ok in Run as (checked & Inputs & Tail).
  destruct (check_assume_nesting_sound _ _ _ Seen Nesting) as [Order SeenNext].
  destruct Member as [<-|Member]; [|eapply IH; eassumption].
  split.
  - unfold assumption_module_error in Form.
    destruct (assume_ctx E first) as [[C ns]|msg] eqn:Read; [|discriminate].
    exists C, ns. now apply Assumes.
  - intros d' Same. rewrite Decl in Same. injection Same as <-.
    split; [exact Order|].
    unfold check_functor_inputs, assumption_module_error in Inputs.
    destruct (functor_parts (d_ast d)) as [[[[ps args] outputs] body]|]; [|exact I].
    destruct (ordered_items E bound (d_mod d) (map CISpec args)) as [[]|msg] eqn:Read;
      [now apply ordered_items_sound|discriminate].
Qed.

Theorem assumes_complete_sound :
  (forall s C ns, assume_ctx E s = Ok (C, ns) -> AssumeCtx W s C ns) ->
  assumes_complete E bound = Ok tt -> AssumesComplete W.
Proof.
  intros Assumes Run s k Kind.
  eapply (check_assume_symbols_sound Assumes (seq 0 bound) []); [constructor|exact Run| |exact Kind].
  unfold kind_sym in Kind. destruct (w_decl W s) as [d|] eqn:Decl; [|discriminate].
  apply in_seq. cbn [Nat.add]. split; [apply Nat.le_0_l|eapply Bounded; exact Decl].
Qed.
End Ordered.

Theorem elaborate_with_assumes_complete : forall strategy b W program,
  elaborate_with strategy b = Ok (W, program) -> AssumesComplete W.
Proof.
  intros strategy b W program Run.
  unfold elaborate_with, elaborate_with_metadata in Run.
  destruct (bundle_world_r b) as [[W0 bound]|message] eqn:Loaded; [|discriminate].
  cbn zeta in Run.
  set (modules := map am_path (b_mods b)) in Run.
  set (mains := main_symbols W0 modules) in Run.
  set (metadata := metadata_fuel W0 bound) in Run.
  set (E0 := Env W0 strategy modules mains elab_fuel metadata) in Run.
  set (E := Env (ElabWorld.with_context W0 (NMap.nget (context_table E0 bound)))
    strategy modules mains elab_fuel metadata) in Run.
  destruct (assumes_complete E bound) as [[]|message] eqn:Checked; [|discriminate].
  repeat match type of Run with
  | context [match ?step with _ => _ end] => destruct step; try discriminate
  end.
  injection Run as <- <-. change (AssumesComplete (e_W E)).
  apply (assumes_complete_sound E bound).
  - exact (EaWorldSound.bundle_world_r_bound _ _ _ Loaded).
  - exact (bundle_world_r_context_table_sound _ _ _ Loaded strategy).
  - exact (EaMetadataSound.metadata_fuel_sufficient W0 bound).
  - apply assume_ctx_sound with (bound := bound).
    + exact (EaWorldSound.bundle_world_r_bound _ _ _ Loaded).
    + exact (EaMetadataSound.metadata_fuel_sufficient W0 bound).
    + exact (EaMainSymbols.bundle_world_r_main_symbols_exact _ _ _ Loaded).
  - exact Checked.
Qed.
