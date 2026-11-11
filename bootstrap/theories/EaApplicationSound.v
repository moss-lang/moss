From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit EaBase EaCall EaControlSound EaTypesSound.
From NM Require AllocationFacts EaBudget EaContextFuel EaContextTableSound EaDecide EaDependencyFuel
  EaFunctor EaOutputSound EaRootSound EaSatisfiesSound EaTypes ProgramPrune PublicFormation PublicTypeFacts PublicTypeRead.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Local Definition OutputChoice (C : cx) (mentioned protected : list nat)
    (role : nat) (row : nat * output_identity) : Prop :=
  fst row = role /\
  match snd row with
  | DeclaredOutput =>
      nlook role (c_ty C) = None /\ ~ In role protected /\ ~ In role mentioned
  | GeneratedOutput _ =>
      nlook role (c_ty C) <> None \/ In role mentioned
  end.

Local Lemma allocate_output_sound : forall E C u mentioned protected role before row after,
  EaFunctor.allocate_output E C u mentioned protected role before = Ok (row, after) ->
  OutputChoice C mentioned protected role row /\
  match snd row with
  | DeclaredOutput => es_next after = es_next before
  | GeneratedOutput g => g = es_next before /\ es_next after = S (es_next before)
  end.
Proof.
  intros E C u mentioned protected role before [key choice] after Run.
  unfold EaFunctor.allocate_output in Run.
  destruct (bound_in C role || nmem role mentioned) eqn:Allocate.
  - cbn [mbind fresh put_name ret] in Run. injection Run as <- <- <-.
    split; [|split; reflexivity]. split; [reflexivity|].
    apply orb_true_iff in Allocate as [InForce|Mentioned].
    + left. unfold bound_in in InForce. destruct (nlook role (c_ty C));
        [discriminate|discriminate InForce].
    + right. now apply nmem_spec.
  - destruct (nmem role protected) eqn:Protected; [discriminate Run|].
    injection Run as <- <- <-. split; [|reflexivity].
    apply orb_false_iff in Allocate as [Unbound Unmentioned].
    split; [reflexivity|]. split.
    + unfold bound_in in Unbound. destruct (nlook role (c_ty C));
        [discriminate Unbound|reflexivity].
    + split; now apply EaDependencyFuel.nmem_false_spec.
Qed.

Local Lemma output_batch_sound : forall E C u mentioned protected outs before rows after,
  mmap (EaFunctor.allocate_output E C u mentioned protected) outs before = Ok (rows, after) ->
  Forall2 (OutputChoice C mentioned protected) outs rows /\
  allocated_output_generations rows =
    seq (es_next before) (length (allocated_output_generations rows)) /\
  es_next after = es_next before + length (allocated_output_generations rows).
Proof.
  intros E C u mentioned protected outs. induction outs as [|role rest IH];
    intros before rows after Run.
  - injection Run as <- <-. cbn. split; [constructor|]. split; [reflexivity|lia].
  - cbn [mmap] in Run.
    apply mbind_ok in Run as ([key choice] & middle & Head & Run).
    apply mbind_ok in Run as (tail & finish & Rest & Run).
    injection Run as <- <-.
    destruct (allocate_output_sound _ _ _ _ _ _ _ _ _ Head) as [Choice Next].
    destruct (IH _ _ _ Rest) as (Choices & Range & End).
    split; [now constructor|].
    destruct choice as [|generation]; cbn [allocated_output_generations flat_map snd fst] in *.
    + cbn [app length seq]. split; [now rewrite <- Next|].
      now rewrite End, Next.
    + destruct Next as [<- Next]. cbn [app length seq].
      split; [f_equal; now rewrite <- Next|].
      rewrite End, Next. now rewrite Nat.add_succ_r.
Qed.

Lemma allocation_floor_sound : forall support before result after,
  EaFunctor.require_allocation_floor support before = Ok (result, after) ->
  before = after /\ forall g, In g support -> g < es_next before.
Proof.
  intros support before result after Read. unfold EaFunctor.require_allocation_floor in Read.
  destruct (forallb (fun g => Nat.ltb g (es_next before)) support) eqn:Checked; [|discriminate Read].
  injection Read as _ <-. split; [reflexivity|]. intros g Member.
  apply Nat.ltb_lt. now apply forallb_forall with (x := g) in Checked.
Qed.

Local Lemma application_output_batch : forall E F C u outs before rows after,
  EaFunctor.allocate_application_outputs E F C u outs before = Ok (rows, after) ->
  let protected := (match fr_ret F with Some t => gens t | None => [] end) ++
    match fr_this F with Some t => gens t | None => [] end in
  Forall2 (OutputChoice C (context_gens C ++ key_receiver_gens C) protected) outs rows /\
  allocated_output_generations rows =
    seq (es_next before) (length (allocated_output_generations rows)) /\
  es_next after = es_next before + length (allocated_output_generations rows) /\
  (outs = [] \/ forall g,
    In g (scope_allocated (body_allocation_scope (e_W E) F C) ++
      scope_support (body_allocation_scope (e_W E) F C)) ->
    g < es_next before).
Proof.
  intros E F C u outs before rows after Run. cbn zeta.
  unfold EaFunctor.allocate_application_outputs in Run. cbn zeta in Run.
  apply mbind_ok in Run as (checked & start & Check & Batch).
  assert (Floor : before = start /\ (outs = [] \/ forall g,
    In g (scope_allocated (body_allocation_scope (e_W E) F C) ++
      scope_support (body_allocation_scope (e_W E) F C)) -> g < es_next before)).
  {
    destruct outs as [|role rest].
    - injection Check as _ <-. auto.
    - apply allocation_floor_sound in Check as [-> Below]. split; [reflexivity|right].
      intros g Present. apply Below. cbn [body_allocation_scope scope_allocated scope_support] in Present.
      rewrite !in_app_iff in *. tauto.
  }
  destruct Floor as [<- Floor]. apply output_batch_sound in Batch as (Choices & Range & After).
  now split.
Qed.

Theorem allocate_application_outputs_range : forall E F C u outs before rows after,
  EaFunctor.allocate_application_outputs E F C u outs before = Ok (rows, after) ->
  let allocated := allocated_output_generations rows in
  allocated = seq (es_next before) (length allocated) /\
  es_next after = es_next before + length allocated.
Proof.
  intros E F C u outs before rows after Run. cbn zeta.
  apply application_output_batch in Run as (_ & Range & End & _). now split.
Qed.

Local Lemma output_roles : forall C mentioned protected outs rows,
  Forall2 (OutputChoice C mentioned protected) outs rows ->
  map fst rows = outs.
Proof.
  intros C mentioned protected outs rows Choices. induction Choices as [|role row outs rows Choice _ IH];
    [reflexivity|].
  cbn [map]. destruct Choice as [Role _]. now rewrite Role, IH.
Qed.

Local Lemma output_choice_for : forall C mentioned protected outs rows role,
  Forall2 (OutputChoice C mentioned protected) outs rows ->
  In role outs ->
  exists choice, In (role, choice) rows /\ OutputChoice C mentioned protected role (role, choice).
Proof.
  intros C mentioned protected outs rows role Choices.
  induction Choices as [|head [key choice] outs rows Chosen _ IH]; [contradiction|].
  intros [<-|Member].
  - pose proof (proj1 Chosen) as Key. cbn in Key. subst key.
    exists choice. split; [now left|exact Chosen].
  - destruct (IH Member) as (other & InRow & Evidence).
    exists other. split; [now right|exact Evidence].
Qed.

Local Lemma nlook_unique_row : forall A key (value : A) entries,
  NoDup (map fst entries) -> In (key, value) entries -> nlook key entries = Some value.
Proof.
  intros A key value entries. induction entries as [|[k v] rest IH];
    intros Unique Member; [contradiction|].
  inversion Unique as [|? ? Absent Tail]; subst.
  cbn [nlook]. destruct (Nat.eqb_spec key k) as [->|Different].
  - destruct Member as [Equal|Rest]; [now injection Equal as ->|].
    exfalso. apply Absent. exact (in_map fst _ _ Rest).
  - apply IH; [exact Tail|]. destruct Member as [Equal|Rest]; [congruence|exact Rest].
Qed.

Local Lemma allocated_roles_distinct : forall rows r r' g,
  NoDup (allocated_output_generations rows) ->
  In (r, GeneratedOutput g) rows -> In (r', GeneratedOutput g) rows -> r = r'.
Proof.
  induction rows as [|[role [|generation]] rows IH];
    intros r r' g Unique First Second; [contradiction| |].
  - destruct First as [Equal|First]; [discriminate Equal|].
    destruct Second as [Equal|Second]; [discriminate Equal|].
    exact (IH _ _ _ Unique First Second).
  - change (NoDup (generation :: allocated_output_generations rows)) in Unique.
    inversion Unique as [|? ? Absent Tail]; subst.
    destruct First as [Equal|First], Second as [Equal'|Second].
    + congruence.
    + inversion Equal; subst. exfalso. apply Absent. eapply AllocationFacts.generated_output_in_batch; exact Second.
    + inversion Equal'; subst. exfalso. apply Absent. eapply AllocationFacts.generated_output_in_batch; exact First.
    + exact (IH _ _ _ Tail First Second).
Qed.

Theorem application_output_identities : forall E F C a before after,
  EaFunctor.allocate_application_outputs E F C (app_functor a) (app_outputs a) before =
    Ok (app_output_choices a, after) ->
  NoDup (app_outputs a) ->
  (forall t, In t (app_outputs a) <-> FunOutputs (e_W E) (app_functor a) t) ->
  (forall g, es_next before <= g -> Fresh (e_W E) g) ->
  (forall t, In t (app_outputs a) -> t < es_next before) ->
  FreshFor (e_W E) F C (app_allocated a) /\
  (forall x y, FunOutputs (e_W E) (app_functor a) x ->
    FunOutputs (e_W E) (app_functor a) y -> app_identity a x = app_identity a y -> x = y) /\
  OutputIdentityAt (e_W E) F C a.
Proof.
  intros E F C a before after Run Unique Roles Fresh RolesBelow.
  apply application_output_batch in Run as (Choices & Range & _ & Floor).
  assert (Read : forall role, In role (app_outputs a) ->
    exists choice, In (role, choice) (app_output_choices a) /\
      OutputChoice C (context_gens C ++ key_receiver_gens C)
        ((match fr_ret F with Some r => gens r | None => [] end) ++
         (match fr_this F with Some r => gens r | None => [] end)) role (role, choice) /\
      output_choice_at (app_output_choices a) role = choice).
  {
    intros role Present.
    destruct (output_choice_for _ _ _ _ _ _ Choices Present) as (choice & Row & Evidence).
    exists choice. split; [exact Row|]. split; [exact Evidence|].
    unfold output_choice_at. rewrite (nlook_unique_row _ _ _ _ Unique Row). reflexivity.
  }
  split.
  {
    unfold FreshFor, app_allocated.
    destruct Floor as [Empty|Below].
    - rewrite Empty in Choices. inversion Choices. split; constructor.
    - rewrite Range. exact (proj1 (AllocationFacts.allocate_range
        (body_allocation_scope (e_W E) F C) _ _ Fresh Below)).
  }
  split.
  - intros x y X Y Equal. apply Roles in X, Y.
    destruct (Read x X) as (choice_x & Rx & _ & Ex).
    destruct (Read y Y) as (choice_y & Ry & _ & Ey).
    unfold app_identity, output_generation_at in Equal. rewrite Ex, Ey in Equal.
    destruct choice_x as [|gx], choice_y as [|gy]; cbn [encode_output_identity] in Equal.
    + exact Equal.
    + subst gy. pose proof (AllocationFacts.generated_output_in_batch _ _ _ Ry) as Member.
      rewrite Range in Member. apply in_seq in Member. specialize (RolesBelow x X). lia.
    + subst gx. pose proof (AllocationFacts.generated_output_in_batch _ _ _ Rx) as Member.
      rewrite Range in Member. apply in_seq in Member. specialize (RolesBelow y Y). lia.
    + subst gy. eapply allocated_roles_distinct; [rewrite Range; apply seq_NoDup|exact Rx|exact Ry].
  - intros role Role. apply Roles in Role.
    destruct (Read role Role) as (choice & Row & Evidence & Reads).
    rewrite Reads. exact (proj2 Evidence).
Qed.

Local Lemma indexed_member : forall A start (xs : list A) i x,
  nth_error xs i = Some x -> In (start + i, x) (combine (seq start (length xs)) xs).
Proof.
  intros A start xs. revert start.
  induction xs as [|head rest IH]; intros start [|i] x At; try discriminate.
  - injection At as <-. cbn [length seq combine]. left. now rewrite Nat.add_0_r.
  - cbn [length seq combine]. right. rewrite Nat.add_succ_r.
    exact (IH (S start) i x At).
Qed.

Theorem collides_only_as_sound : forall keys formal,
  EaDecide.collides_only_as keys formal = true -> CollidesOnlyAs keys formal.
Proof.
  intros keys formal Checked i j k k' At At' Collision.
  unfold EaDecide.collides_only_as in Checked. cbv zeta in Checked.
  rewrite forallb_forall in Checked.
  specialize (Checked (i, k) (indexed_member _ 0 _ _ _ At)).
  rewrite forallb_forall in Checked.
  specialize (Checked (j, k') (indexed_member _ 0 _ _ _ At')).
  cbn [fst snd] in Checked. rewrite Collision in Checked.
  rewrite (proj2 (ProgramPrune.item_eqb_true _ _) eq_refl) in Checked.
  destruct (nth_error formal i) as [f|], (nth_error formal j) as [f'|];
    try discriminate Checked.
  exists f, f'. repeat split. now apply ProgramPrune.item_eqb_true.
Qed.

Local Lemma pure_read : forall A (reader : er A) before value after,
  pure reader before = Ok (value, after) -> reader = Ok value /\ after = before.
Proof.
  intros A [answer|message] before value after Read; [|discriminate].
  injection Read as <- <-. split; reflexivity.
Qed.

Theorem apply_abstract_sound : forall E bound F C pa u before wrap result F' made after P,
  (forall g d, w_decl (e_W E) g = Some d -> g < bound) ->
  EaBudget.metadata_budget (e_W E) bound < e_metadata_fuel E ->
  EaContextTableSound.ContextTableSound (e_W E) ->
  resolve_sym (e_W E) (fr_mod F) pa = Some u ->
  kind_sym (e_W E) u = Some DKFunctor ->
  bound <= es_next before ->
  EaFunctor.apply_abstract E F C u before = Ok ((wrap, result, F', made), after) ->
  BindItem (e_W E) P F C (Spec (Some pa) None None, None) wrap result F' made.
Proof.
  intros E bound F C pa u before wrap result F' made after P
    Bounded Budget Contexts Resolved Kind Counter Run.
  unfold EaFunctor.apply_abstract in Run. cbv zeta in Run.
  apply mbind_ok in Run as (fx & s1 & Read & Run).
  destruct (cfun C u) as [fixed|] eqn:Available; [|discriminate Read].
  injection Read as <- <-.
  apply mbind_ok in Run as ([Cu ns] & s2 & Read & Run).
  destruct (w_context (e_W E) u) as [[formal needs]|] eqn:Context; [|discriminate Read].
  injection Read as <- <- <-.
  apply mbind_ok in Run as (checked & s3 & Read & Run).
  destruct (find (fun q => negb (bound_in C (fst q))) (c_ty formal))
    as [[missing missing_type]|] eqn:InForce; [discriminate Read|].
  injection Read as <- <-.
  apply mbind_ok in Run as (checked & s4 & Read & Run).
  destruct (find (fun q => match nlook (fst q) (c_ty C) with
    | Some here => negb (ty_eqb (canon C here) (canon C (snd q)))
    | None => true
    end) fixed) as [[bad wanted]|] eqn:Fixed; [discriminate Read|].
  injection Read as <- <-.
  set (th := map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty formal)) in *.
  apply mbind_ok in Run as (sm & s5 & Read & Run).
  apply pure_read in Read as [Satisfied ->].
  pose proof (Contexts _ _ _ Context) as Formal. rewrite Kind in Formal.
  apply EaSatisfiesSound.satisfies_sound in Satisfied;
    [|eapply EaSatisfiesSound.functx_subst_functional; exact Formal].
  destruct Satisfied as [-> Satisfied].
  set (outs := EaTypes.fun_outputs (e_W E) (e_metadata_fuel E) u) in *.
  apply mbind_ok in Run as (rows & s6 & Allocated & Run).
  set (generation := output_generation_at rows) in *.
  apply mbind_ok in Run as ([C' ks] & s7 & Read & Run).
  apply pure_read in Read as [Provided ->].
  apply mbind_ok in Run as ([Cf ksf] & s8 & Read & Run).
  apply pure_read in Read as [FormalResult ->].
  apply mbind_ok in Run as (checked & s9 & Read & Run).
  destruct (EaDecide.collides_only_as ks ksf) eqn:Collisions; [|discriminate Read].
  injection Run as <- <- <- <- _.
  assert (Unique : NoDup outs) by apply EaRootSound.fun_outputs_nodup.
  assert (Fuel : bound < e_metadata_fuel E).
  { assert (Depth : bound <= EaBudget.metadata_depth (e_W E) bound).
    { unfold EaBudget.metadata_depth. apply Nat.le_max_l. }
    unfold EaBudget.metadata_budget in Budget. lia. }
  assert (Roles : forall t, In t outs <-> FunOutputs (e_W E) u t).
  { apply (EaContextFuel.fun_outputs_exact _ bound Bounded _ u Fuel). }
  assert (Below : forall t, In t outs -> t < es_next before).
  {
    intros t Role. apply Roles in Role.
    pose proof (PublicFormation.outputs_typed _ _ _ Role) as Typed.
    unfold kind_sym in Typed.
    destruct (w_decl (e_W E) t) as [d|] eqn:Declaration; [|discriminate].
    pose proof (Bounded t d Declaration). lia.
  }
  assert (Unused : forall g, es_next before <= g -> Fresh (e_W E) g).
  { intros g Above. unfold Fresh. destruct (w_decl (e_W E) g) as [d|] eqn:Declaration;
      [pose proof (Bounded g d Declaration); lia|reflexivity]. }
  assert (RowRoles : map fst rows = outs).
  { apply application_output_batch in Allocated as (Choices & _). eapply output_roles; exact Choices. }
  rewrite <- RowRoles in Allocated, Unique, Roles, Below |- *.
  pose (a := Application u formal needs th rows C' ks).
  destruct (application_output_identities E F C a _ _ Allocated
    Unique Roles Unused Below) as (Fresh & Injective & Identities).
  assert (KeysRead : map (rkof C') ks = map (rkey_of C') ks).
  { apply map_ext. apply EaSatisfiesSound.rkof_exact. }
  rewrite KeysRead. apply (@BApplyAbstract _ _ F C pa a).
  unfold TypesApplication, a. cbn zeta.
  split; [exact Resolved|].
  split.
  { exists fixed. split; [now rewrite <- EaSatisfiesSound.cfun_exact|].
    intros x tx Member. pose proof (find_none _ _ Fixed (x, tx) Member) as Check.
    cbn [fst snd] in Check.
    destruct (nlook x (c_ty C)) as [here|]; [|discriminate Check].
    exists here. split; [reflexivity|].
    apply PublicTypeFacts.ty_eqb_spec, negb_false_iff. exact Check. }
  split; [exact Formal|].
  split.
  { intros t tt Present. apply PublicTypeRead.nlook_in in Present.
    pose proof (find_none _ _ InForce (t, tt) Present) as Check.
    apply negb_false_iff in Check. unfold bound_in in Check. cbn [fst] in Check.
    now apply EaSatisfiesSound.is_some_value in Check. }
  split; [reflexivity|].
  split; [exact Satisfied|].
  split; [exact Fresh|].
  split; [exact Injective|].
  split; [exact Identities|].
  split; [exact Unique|].
  split; [exact Roles|].
  split.
  - eapply EaOutputSound.provide_result_sound_at_budget; eassumption.
  - exists Cf, ksf. split.
    + eapply EaOutputSound.provide_result_sound_at_budget; eassumption.
    + now apply collides_only_as_sound.
Qed.
