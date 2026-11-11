From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit EaBase EaCall EaControlSound EaTypesSound.
From NM Require AllocationFacts EaBudget EaContextFuel EaContextTableSound EaDecide EaDependencyFuel
  EaFunctor EaOutputSound EaRootSound EaSatisfiesSound EaTypes ProgramPrune PublicFormation PublicTypeFacts PublicTypeRead.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Local Definition OutputChoice (C : cx) (mentioned protected : list nat)
    (role : nat) (row : nat * nat * bool) : Prop :=
  fst (fst row) = role /\
  if snd row then
    nlook role (c_ty C) <> None \/ In role mentioned
  else
    snd (fst row) = role /\ nlook role (c_ty C) = None /\
    ~ In role protected /\ ~ In role mentioned.

Local Lemma allocate_output_sound : forall E C u mentioned protected role before row after,
  EaFunctor.allocate_output E C u mentioned protected role before = Ok (row, after) ->
  OutputChoice C mentioned protected role row /\
  if snd row then snd (fst row) = es_next before /\ es_next after = S (es_next before)
  else es_next after = es_next before.
Proof.
  intros E C u mentioned protected role before [[key generation] allocated] after Run.
  unfold EaFunctor.allocate_output in Run.
  destruct (bound_in C role || nmem role mentioned) eqn:Allocate.
  - cbn [mbind fresh put_name ret] in Run. injection Run as <- <- <- <-.
    split; [|split; reflexivity]. split; [reflexivity|].
    apply orb_true_iff in Allocate as [InForce|Mentioned].
    + left. unfold bound_in in InForce. destruct (nlook role (c_ty C));
        [discriminate|discriminate InForce].
    + right. now apply nmem_spec.
  - destruct (nmem role protected) eqn:Protected; [discriminate Run|].
    injection Run as <- <- <- <-. split; [|reflexivity].
    apply orb_false_iff in Allocate as [Unbound Unmentioned].
    split; [reflexivity|]. split; [reflexivity|]. split.
    + unfold bound_in in Unbound. destruct (nlook role (c_ty C));
        [discriminate Unbound|reflexivity].
    + split; now apply EaDependencyFuel.nmem_false_spec.
Qed.

Local Lemma output_batch_sound : forall E C u mentioned protected outs before rows after,
  mmap (EaFunctor.allocate_output E C u mentioned protected) outs before = Ok (rows, after) ->
  Forall2 (OutputChoice C mentioned protected) outs rows /\
  EaFunctor.allocated_output_generations rows =
    seq (es_next before) (length (EaFunctor.allocated_output_generations rows)) /\
  es_next after = es_next before + length (EaFunctor.allocated_output_generations rows).
Proof.
  intros E C u mentioned protected outs. induction outs as [|role rest IH];
    intros before rows after Run.
  - injection Run as <- <-. cbn. split; [constructor|]. split; [reflexivity|lia].
  - cbn [mmap] in Run.
    apply mbind_ok in Run as ([[key generation] allocated] & middle & Head & Run).
    apply mbind_ok in Run as (tail & finish & Rest & Run).
    injection Run as <- <-.
    destruct (allocate_output_sound _ _ _ _ _ _ _ _ _ Head) as [Choice Next].
    destruct (IH _ _ _ Rest) as (Choices & Range & End).
    split; [now constructor|].
    cbn [EaFunctor.allocated_output_generations flat_map snd fst] in *.
    destruct allocated.
    + destruct Next as [<- Next]. cbn [app length seq].
      split; [f_equal; now rewrite <- Next|].
      rewrite End, Next. now rewrite Nat.add_succ_r.
    + cbn [app length seq]. split; [now rewrite <- Next|].
      now rewrite End, Next.
Qed.

Theorem allocate_application_outputs_range : forall E F C u outs before rows after,
  EaFunctor.allocate_application_outputs E F C u outs before = Ok (rows, after) ->
  let allocated := EaFunctor.allocated_output_generations rows in
  allocated = seq (es_next before) (length allocated) /\
  es_next after = es_next before + length allocated.
Proof.
  intros E F C u outs before rows after Run. cbn zeta.
  apply output_batch_sound in Run. exact (proj2 Run).
Qed.

Local Lemma output_roles : forall C mentioned protected outs rows,
  Forall2 (OutputChoice C mentioned protected) outs rows ->
  map fst (map fst rows) = outs.
Proof.
  intros C mentioned protected outs rows Choices. induction Choices as [|role row outs rows Choice _ IH];
    [reflexivity|].
  cbn [map]. destruct Choice as [Role _]. now rewrite Role, IH.
Qed.

Local Lemma output_choice_for : forall C mentioned protected outs rows role,
  Forall2 (OutputChoice C mentioned protected) outs rows ->
  In role outs ->
  exists generation allocated,
    In (role, generation, allocated) rows /\
    OutputChoice C mentioned protected role (role, generation, allocated).
Proof.
  intros C mentioned protected outs rows role Choices.
  induction Choices as [|head [[key generation] allocated] outs rows Choice _ IH]; [contradiction|].
  intros [<-|Member].
  - pose proof (proj1 Choice) as Key. cbn in Key. subst key.
    exists generation, allocated. split; [now left|exact Choice].
  - destruct (IH Member) as (g & a & InRow & Chosen).
    exists g, a. split; [now right|exact Chosen].
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

Local Lemma row_generation : forall outs rows C mentioned protected role generation allocated,
  Forall2 (OutputChoice C mentioned protected) outs rows ->
  NoDup outs ->
  In (role, generation, allocated) rows ->
  EaFunctor.gen_of (map fst rows) role = generation.
Proof.
  intros outs rows C mentioned protected role generation allocated Choices Unique Member.
  unfold EaFunctor.gen_of.
  assert (Found : nlook role (map fst rows) = Some generation).
  { apply nlook_unique_row.
    - rewrite (output_roles _ _ _ _ _ Choices). exact Unique.
    - exact (in_map fst _ _ Member). }
  now rewrite Found.
Qed.

Local Lemma allocated_row_member : forall rows role generation,
  In (role, generation, true) rows ->
  In generation (EaFunctor.allocated_output_generations rows).
Proof.
  intros rows role generation Member.
  unfold EaFunctor.allocated_output_generations. apply in_flat_map.
  exists (role, generation, true). split; [exact Member|now left].
Qed.

Local Lemma allocated_roles_distinct : forall rows r r' g,
  NoDup (EaFunctor.allocated_output_generations rows) ->
  In (r, g, true) rows -> In (r', g, true) rows -> r = r'.
Proof.
  induction rows as [|[[role generation] allocated] rows IH];
    intros r r' g Unique First Second; [contradiction|].
  destruct allocated.
  - change (NoDup (generation :: EaFunctor.allocated_output_generations rows)) in Unique.
    inversion Unique as [|? ? Absent Tail]; subst.
    destruct First as [Equal|First], Second as [Equal'|Second].
    + congruence.
    + inversion Equal; subst. exfalso. apply Absent. eapply allocated_row_member; exact Second.
    + inversion Equal'; subst. exfalso. apply Absent. eapply allocated_row_member; exact First.
    + exact (IH _ _ _ Tail First Second).
  - destruct First as [Equal|First]; [discriminate Equal|].
    destruct Second as [Equal|Second]; [discriminate Equal|].
    exact (IH _ _ _ Unique First Second).
Qed.

Theorem application_output_identities : forall E F C a before rows after,
  EaFunctor.allocate_application_outputs E F C (app_functor a)
    (app_outputs a) before = Ok (rows, after) ->
  app_allocated a = EaFunctor.allocated_output_generations rows ->
  app_identity a = EaFunctor.gen_of (map fst rows) ->
  NoDup (app_outputs a) ->
  (forall t, In t (app_outputs a) <-> FunOutputs (e_W E) (app_functor a) t) ->
  (forall g, es_next before <= g -> Fresh (e_W E) g) ->
  (forall g, In g (scope_allocated (body_allocation_scope (e_W E) F C) ++
      scope_support (body_allocation_scope (e_W E) F C)) -> g < es_next before) ->
  (forall t, In t (app_outputs a) -> t < es_next before) ->
  FreshFor (e_W E) F C (app_allocated a) /\
  (forall x y, FunOutputs (e_W E) (app_functor a) x ->
    FunOutputs (e_W E) (app_functor a) y -> app_identity a x = app_identity a y -> x = y) /\
  OutputIdentityAt (e_W E) F C a.
Proof.
  intros E F C a before rows after Run Allocated Id Unique Roles Fresh Below RolesBelow.
  unfold EaFunctor.allocate_application_outputs in Run. cbn zeta in Run.
  apply output_batch_sound in Run as (Choices & Range & _).
  assert (Read : forall role, In role (app_outputs a) ->
    exists g fresh, In (role, g, fresh) rows /\
      OutputChoice C (context_gens C ++ key_receiver_gens C)
        ((match fr_ret F with Some r => gens r | None => [] end) ++
         (match fr_this F with Some r => gens r | None => [] end)) role (role, g, fresh) /\
      app_identity a role = g).
  {
    intros role Present.
    destruct (output_choice_for _ _ _ _ _ _ Choices Present) as (g & fresh & Row & Choice).
    exists g, fresh. split; [exact Row|]. split; [exact Choice|].
    rewrite Id. eapply row_generation; eassumption.
  }
  split.
  { unfold FreshFor. rewrite Allocated, Range.
    exact (proj1 (AllocationFacts.allocate_range (body_allocation_scope (e_W E) F C) _ _ Fresh Below)). }
  split.
  - intros x y X Y Equal. apply Roles in X, Y.
    destruct (Read x X) as (gx & ax & Rx & Cx & Ex).
    destruct (Read y Y) as (gy & ay & Ry & Cy & Ey).
    rewrite Ex, Ey in Equal. revert Ry Cy. rewrite <- Equal. intros Ry Cy. clear Equal Ey gy.
    destruct ax, ay.
    + eapply allocated_roles_distinct; [rewrite Range; apply seq_NoDup|exact Rx|exact Ry].
    + pose proof (allocated_row_member _ _ _ Rx) as Member.
      rewrite Range in Member. apply in_seq in Member.
      cbn [OutputChoice fst snd] in Cy. destruct Cy as [_ [Retained _]].
      change (gx = y) in Retained. rewrite Retained in Member. exfalso. exact (proj1 (Nat.lt_nge _ _) (RolesBelow y Y) (proj1 Member)).
    + pose proof (allocated_row_member _ _ _ Ry) as Member.
      rewrite Range in Member. apply in_seq in Member.
      cbn [OutputChoice fst snd] in Cx. destruct Cx as [_ [Retained _]].
      change (gx = x) in Retained. rewrite Retained in Member. exfalso. exact (proj1 (Nat.lt_nge _ _) (RolesBelow x X) (proj1 Member)).
    + destruct Cx as [_ [KeepX _]]. destruct Cy as [_ [KeepY _]].
      change (gx = x) in KeepX. rewrite <- KeepX. exact KeepY.
  - intros role Role. apply Roles in Role.
    destruct (Read role Role) as (g & fresh & Row & Choice & Reads).
    destruct fresh; cbn [OutputChoice fst snd] in Choice.
    + right. split; [exact (proj2 Choice)|].
      rewrite Reads, Allocated. eapply allocated_row_member; exact Row.
    + left. destruct Choice as (_ & Retained & Unbound & Protected & Unmentioned).
      repeat split; try assumption. now rewrite Reads.
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

Local Lemma cfun_exact : forall C u, cfun C u = nlook u (c_funs C).
Proof.
  intros [types substitution classes entries locals] u. unfold cfun, c_funs.
  cbn [c_provisions]. induction entries as [|[key value] rest IH]; [reflexivity|].
  destruct key as [s|s|s|r m th|f], value;
    cbn [cfun_go flat_map app nlook]; try exact IH.
  destruct (Nat.eqb u f); [reflexivity|exact IH].
Qed.

Local Lemma outputs_nodup : forall W fuel u,
  NoDup (EaTypes.fun_outputs W fuel u).
Proof.
  intros W fuel u. unfold EaTypes.fun_outputs.
  destruct (EaTypes.functor_decl W u) as [[d [[[params inputs] outputs] body]]|];
    [|constructor].
  apply EaRootSound.ascending_nodup, EaDependencyFuel.sort_uniq_ascending.
Qed.

Local Lemma functx_subst_functional : forall W u C ns,
  FunCtx W u C ns ->
  forall g ty, In (g, ty) (c_sub C) -> lookup_subst g (c_sub C) = Some ty.
Proof.
  intros W u C ns (d & ps & a & r & b & es & tm & tm' & sigma & classes & implicit_types &
    _ & _ & _ & _ & _ & Merge & _ & _ & -> & _).
  exact (proj2 (proj2 (proj2 (proj2 Merge)))).
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
  (forall g, In g (scope_allocated (body_allocation_scope (e_W E) F C) ++
      scope_support (body_allocation_scope (e_W E) F C)) -> g < es_next before) ->
  EaFunctor.apply_abstract E F C u before = Ok ((wrap, result, F', made), after) ->
  BindItem (e_W E) P F C (Spec (Some pa) None None, None) wrap result F' made.
Proof.
  intros E bound F C pa u before wrap result F' made after P
    Bounded Budget Contexts Resolved Kind Counter Scope Run.
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
    [|eapply functx_subst_functional; exact Formal].
  destruct Satisfied as [-> Satisfied].
  set (outs := EaTypes.fun_outputs (e_W E) (e_metadata_fuel E) u) in *.
  apply mbind_ok in Run as (rows & s6 & Allocated & Run).
  set (generation := EaFunctor.gen_of (map fst rows)) in *.
  apply mbind_ok in Run as ([C' ks] & s7 & Read & Run).
  apply pure_read in Read as [Provided ->].
  apply mbind_ok in Run as ([Cf ksf] & s8 & Read & Run).
  apply pure_read in Read as [FormalResult ->].
  apply mbind_ok in Run as (checked & s9 & Read & Run).
  destruct (EaDecide.collides_only_as ks ksf) eqn:Collisions; [|discriminate Read].
  injection Run as <- <- <- <- _.
  assert (Unique : NoDup outs) by apply outputs_nodup.
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
  pose (a := Application u formal needs th outs generation (EaFunctor.allocated_output_generations rows) C' ks).
  destruct (application_output_identities E F C a _ _ _ Allocated eq_refl eq_refl
    Unique Roles Unused Scope Below) as (Fresh & Injective & Identities).
  assert (KeysRead : map (rkof C') ks = map (rkey_of C') ks).
  { apply map_ext. apply EaSatisfiesSound.rkof_exact. }
  rewrite KeysRead. apply (@BApplyAbstract _ _ F C pa a).
  unfold TypesApplication, a. cbn zeta.
  split; [exact Resolved|].
  split.
  { exists fixed. split; [now rewrite <- cfun_exact|].
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
