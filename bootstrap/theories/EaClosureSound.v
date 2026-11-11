(* Closure binding checks its source inputs and ties them to the linked
   body, preserving the cache that the linker established. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaCall EaTypes.
From NM Require EaBudget EaCallSound EaContextTableSound EaControlSound EaDeclarationSound
  EaFunctor EaLinkBindingSound EaLinkBodySound EaMain EaResultBudget EaSatisfiesSound EaTypesSound
  PublicContextFacts PublicTypeFacts.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Import EaLinkBodySound.
Local Open Scope list_scope.

Lemma fixed_here_exact : forall E bound C u x t,
  (forall n d, w_decl (e_W E) n = Some d -> n < bound) ->
  EaBudget.metadata_budget (e_W E) bound < e_metadata_fuel E ->
  In (x, t) (EaFunctor.fixed_here E u C) <-> Fixed (e_W E) u x /\ nlook x (c_ty C) = Some t.
Proof.
  intros E bound C u x t Bounded Budget. unfold EaFunctor.fixed_here. rewrite in_flat_map.
  rewrite <- (EaResultBudget.fixed_exact _ _ Bounded _ u Budget x). split.
  - intros [key [Fixed Member]]. destruct (nlook key (c_ty C)) as [found|] eqn:Read; [|contradiction].
    destruct Member as [Equal|[]]. now injection Equal as <- <-.
  - intros [Fixed Read]. exists x. split; [exact Fixed|]. rewrite Read. now left.
Qed.

Local Lemma link_inputs_in_force : forall W gg C Cu Cg needs,
  FunCtx W gg Cg needs ->
  find (fun q => negb (bound_in Cu (fst q) || in_force_through C Cg (fst q))) (c_ty Cg) = None ->
  forall t, In t (map fst (c_ty Cg)) -> ~ In t (map fst (c_ty Cu)) ->
    InForceThrough C Cg t.
Proof.
  intros W gg C Cu Cg needs Formal Checked t Input NotLinked.
  apply in_map_iff in Input as [[key value] [<- Input]].
  pose proof (find_none _ _ Checked (key, value) Input) as Present. cbn [fst] in Present.
  apply negb_false_iff in Present. apply orb_true_iff in Present as [Linked|Captured].
  - exfalso. apply NotLinked. now apply bound_in_key.
  - destruct (PublicContextFacts.functor_context_merge _ _ _ _ Formal) as [Solved Section].
    now apply (EaCallSound.in_force_through_spec _ _ _ Solved Section).
Qed.

Local Lemma link_pin_agrees : forall C pins thg,
  find (fun q => negb (
      forallb (bound_in C) (gens (snd q)) &&
      match nlook (fst q) (c_ty C) with
      | Some here => ty_eqb (canon C here) (canon C (app_subst thg (snd q)))
      | None => false end)) pins = None ->
  forall x tx, In (x, tx) pins -> exists here, nlook x (c_ty C) = Some here /\
    (forall y, In y (gens tx) -> nlook y (c_ty C) <> None) /\
    canon C here = canon C (app_subst thg tx).
Proof.
  intros C pins thg Checked x tx Pin.
  pose proof (find_none _ _ Checked (x, tx) Pin) as Present. cbn [fst snd] in Present.
  apply negb_false_iff in Present. apply andb_true_iff in Present as [Types Equal].
  destruct (nlook x (c_ty C)) as [here|]; [|discriminate].
  exists here. split; [reflexivity|]. split; [|now apply PublicTypeFacts.ty_eqb_spec].
  intros y Generation. pose proof (proj1 (forallb_forall _ _) Types y Generation) as InForce.
  unfold bound_in in InForce. now destruct (nlook y (c_ty C)).
Qed.

Local Lemma closure_pure_read : forall A (read : er A) before value after,
  pure read before = Ok (value, after) -> read = Ok value /\ after = before.
Proof. intros A [x|] before value after Run; [now injection Run as <- <-|discriminate]. Qed.

Section Closure.
Variables (E : env) (R : rec) (P : program) (bound : nat) (Post Extra : est -> Prop).
Let W := e_W E.
Hypotheses
  (Bounded : forall n d, w_decl W n = Some d -> n < bound)
  (Budget : EaBudget.metadata_budget W bound < e_metadata_fuel E)
  (Decls : EaMain.decls_form E bound = Ok tt)
  (Contexts : EaContextTableSound.ContextTableSound W).

Theorem fun_closure_sound : forall F C pa u pg gg before wrap C' after,
  resolve_sym W (fr_mod F) pa = Some u -> resolve_sym W (fr_mod F) pg = Some gg ->
  kind_sym W gg = Some DKFunctor ->
  bound <= es_next before ->
  Post after ->
  (forall h finish,
    kind_sym W u = Some DKFunctor -> Post finish ->
    EaFunctor.link_body E R u gg before = Ok (h, finish) ->
    LinkBody W P u gg (slot_index h) (link_pins_at finish (slot_index h)) /\
    TypedLinkCache W P finish /\ Extra finish) ->
  EaFunctor.fun_closure E R F C u gg before = Ok ((wrap, C'), after) ->
  BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) wrap C' F [] /\
  TypedLinkCache W P after /\ Extra after.
Proof.
  intros F C pa u pg gg before wrap C' after TargetName ProviderName ProviderKind Floor Final LinkBodyChecked Run.
  unfold EaFunctor.fun_closure in Run. fold W in Run.
  apply EaControlSound.mbind_ok in Run as (checked & s1 & Abstract & Run).
  apply successful_link_check in Abstract as [Abstract ->].
  unfold EaFunctor.abstract_functor, ast_of in Abstract. fold W in Abstract.
  destruct (w_decl W u) as [d|] eqn:Declaration; [|discriminate].
  cbn [option_map] in Abstract.
  destruct (d_ast d) as [| | | | | | | |name parameters arguments results body]
    eqn:Shape; try discriminate Abstract.
  destruct body; [discriminate Abstract|].
  assert (TargetKind : kind_sym W u = Some DKFunctor) by
    (unfold kind_sym; rewrite Declaration, Shape; reflexivity).
  assert (TargetAbstract : exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None)).
  { exists name, parameters, arguments, results. unfold ast_of. rewrite Declaration. cbn [option_map]. now rewrite Shape. }
  apply EaControlSound.mbind_ok in Run as ([Cu nsu] & s2 & Read & Run).
  apply successful_link_lift in Read as [ReadTarget ->].
  apply EaControlSound.mbind_ok in Run as ([Cg nsg] & s3 & Read & Run).
  apply successful_link_lift in Read as [ReadProvider ->].
  assert (FormalTarget : FunCtx W u Cu nsu).
  { pose proof (Contexts _ _ _ ReadTarget) as Valid. now rewrite TargetKind in Valid. }
  assert (FormalProvider : FunCtx W gg Cg nsg).
  { pose proof (Contexts _ _ _ ReadProvider) as Valid. now rewrite ProviderKind in Valid. }
  apply EaControlSound.mbind_ok in Run as (checked_fixed & s4 & Fixed & Run).
  destruct (find (fun x => negb (bound_in C x)) (fixed W (e_metadata_fuel E) u))
    eqn:FixedHere; [discriminate Fixed|].
  injection Fixed as _ <-.
  apply EaControlSound.mbind_ok in Run as (checked_result & s5 & Same & Run).
  apply successful_link_check in Same as [Same ->].
  pose proof (proj1 (EaDeclarationSound.same_result_exact_at_declarations E bound u gg Bounded Decls TargetKind ProviderKind)
    Same) as ResultsAgree.
  set (fx := EaFunctor.fixed_here E u C) in *.
  apply EaControlSound.mbind_ok in Run as (target & s6 & Binding & Run).
  apply (EaLinkBindingSound.check_link_binding_sound E bound Bounded Budget u gg Cu nsu Cg nsg _ _ _ _ _ target
    FormalTarget Floor) in Binding as [-> LinkBinding].
  apply EaControlSound.mbind_ok in Run as (h & s7 & Link & Run).
  set (sm := EaFunctor.translate_link_targets Cu Cg target nsg) in *.
  set (thg := map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cg)) in *.
  apply EaControlSound.mbind_ok in Run as (smc & s8 & Satisfied & Run).
  apply closure_pure_read in Satisfied as [Satisfied ->].
  apply EaControlSound.mbind_ok in Run as (checked_inputs & s9 & Inputs & Run).
  match type of Inputs with
  | context [match ?bad with Some _ => _ | None => _ end] =>
      destruct bad as [[bad_key bad_type]|] eqn:InputsHere; [discriminate Inputs|]
  end.
  injection Inputs as _ <-.
  apply EaControlSound.mbind_ok in Run as (pins & s10 & Pins & Run).
  unfold get_pins in Pins. injection Pins as <- <-.
  apply EaControlSound.mbind_ok in Run as (checked_pins & s11 & PinCheck & Run).
  match type of PinCheck with
  | context [match ?bad with Some _ => _ | None => _ end] =>
      destruct bad as [[bad_key bad_type]|] eqn:PinsAgree; [discriminate PinCheck|]
  end.
  injection PinCheck as _ <-.
  injection Run as <- <- <-.
  destruct (LinkBodyChecked _ _ TargetKind Final Link) as [Body Preserved].
  split; [|exact Preserved].
  eapply (@BFunClosure W P F C pa u pg gg Cu nsu Cg nsg (slot_index h)
      (link_pins_at s7 (slot_index h)) target sm smc fx); try eassumption.
  - intros x Fixed.
    apply (EaResultBudget.fixed_exact W bound Bounded _ u Budget x) in Fixed.
    pose proof (find_none _ _ FixedHere x Fixed) as Bound.
    apply negb_false_iff in Bound. unfold bound_in in Bound.
    now destruct (nlook x (c_ty C)).
  - apply EaLinkBindingSound.translate_link_targets_sound.
  - eapply EaSatisfiesSound.satisfies_gen_sound; [|exact Satisfied].
    exact (EaSatisfiesSound.functx_subst_functional _ _ _ _ FormalProvider).
  - intros x tx. unfold fx. eapply fixed_here_exact; eassumption.
  - eapply link_inputs_in_force; eassumption.
  - eapply link_pin_agrees. exact PinsAgree.
Qed.

End Closure.
