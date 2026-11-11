(* Context item formation consumes exact source-dependency readers. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit EaBase EaTypes EaCtx EaTypesSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Section ContextEntries.
Variable E : env.
Let W := e_W E.
Let fuel := e_fuel E.
Let metadata_fuel := e_metadata_fuel E.
Hypothesis fixed_exact : forall u x, In x (fixed W metadata_fuel u) <-> Fixed W u x.
Hypothesis readings_exact : forall u tm, FixedReadings W u tm (fixed_readings W metadata_fuel u tm).
Hypothesis applications_sound : forall symbol tm, AppOf W symbol tm (app_of W metadata_fuel symbol tm).

Lemma fixed_available : forall u (tm : list (nat * ty)),
  filter (fun x => negb (is_some (nlook x tm))) (fixed W metadata_fuel u) = [] ->
  forall x, Fixed W u x -> nlook x tm <> None.
Proof.
  intros u tm Empty x Fixed Missing.
  assert (Member : In x (filter (fun x => negb (is_some (nlook x tm))) (fixed W metadata_fuel u))).
  { apply filter_In. split; [apply fixed_exact; exact Fixed|rewrite Missing; reflexivity]. }
  rewrite Empty in Member. contradiction.
Qed.

Theorem entries_sound : forall n,
  (forall stack p ob tm syntax entries meanings,
    item_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
    ItemEntries W p ob tm syntax entries meanings) /\
  (forall stack p ob tm syntax entries meanings,
    items_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
    ItemsEntries W p ob tm syntax entries meanings).
Proof.
  induction n as [|n [ItemSound ItemsSound]].
  - split; intros; discriminate.
  - split.
    + intros stack p ob tm [[pa|] [dn|] application] entries meanings Run; try discriminate.
      * cbn [item_entries] in Run. fold W fuel metadata_fuel in Run.
        destruct (resolve_sym W p pa) as [receiver|] eqn:Resolve; [|discriminate]. cbn [rbind] in Run.
        destruct (method_named W p receiver dn) as [method|] eqn:Method; [|discriminate]. cbn [rbind] in Run.
        apply rbind_ok in Run as [bindings [Brackets Run]].
        apply rbind_ok in Run as [receiver_type [Receiver Run]].
        apply rbind_ok in Run as [signature [Signature Run]]. injection Run as <- <-.
        eapply IEMeth; [exact Resolve|eapply method_named_sound; exact Method|
          eapply brackets_sound; exact Brackets|eapply recv_ty_sound; exact Receiver|
          eapply sig_of_sound; exact Signature|apply applications_sound].
      * cbn [item_entries] in Run. fold W fuel metadata_fuel in Run.
        destruct (resolve_sym W p pa) as [symbol|] eqn:Resolve; [|discriminate]. cbn [rbind] in Run.
        apply rbind_ok in Run as [d [Declaration Run]]. apply decl_of_sound in Declaration.
        destruct (d_ast d) as [items declarations|name|name syntax|name parameters payload|name|name value_type initial|
          receiver name parameters result body|name items|name parameters arguments results body] eqn:Ast;
          try discriminate.
        -- destruct application; [discriminate|]. cbn [rbind] in Run. injection Run as <- <-.
           eapply IEType; [exact Resolve|]. unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
        -- destruct application; [discriminate|]. cbn [rbind] in Run. injection Run as <- <-.
           eapply IENothing; [exact Resolve| |right; right; left; reflexivity].
           unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
        -- destruct application; [discriminate|]. cbn [rbind] in Run. injection Run as <- <-.
           eapply IENothing; [exact Resolve| |right; left; reflexivity].
           unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
        -- destruct application; [discriminate|]. cbn [rbind] in Run. injection Run as <- <-.
           eapply IENothing; [exact Resolve| |left; reflexivity].
           unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
        -- destruct initial as [initial|].
           ++ destruct application; [discriminate|]. cbn [rbind] in Run. injection Run as <- <-.
              eapply IENothing; [exact Resolve| |right; right; right; split; [reflexivity|]].
              ** unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
              ** exists d, name, value_type, initial. split; assumption.
           ++ apply rbind_ok in Run as [bindings [Brackets Run]].
              apply rbind_ok in Run as [meaning [Meaning Run]]. injection Run as <- <-.
              eapply IEVal; [exact Resolve|exact Declaration|exact Ast|
                eapply brackets_sound; exact Brackets|eapply ty_of_sound; exact Meaning|apply applications_sound].
        -- destruct receiver; try discriminate.
           apply rbind_ok in Run as [bindings [Brackets Run]].
           apply rbind_ok in Run as [signature [Signature Run]]. injection Run as <- <-.
           eapply IEFn; [exact Resolve| |eapply brackets_sound; exact Brackets|
             eapply sig_of_sound; exact Signature|apply applications_sound].
           unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
        -- destruct (negb (nmem symbol stack)) eqn:Acyclic; [|discriminate]. cbn [rbind] in Run.
           apply rbind_ok in Run as [bindings [Brackets Run]].
           eapply IECtx; [exact Resolve|exact Declaration|exact Ast|
             eapply brackets_sound; exact Brackets|eapply ItemsSound; exact Run].
        -- destruct body as [body|]; [discriminate|].
           destruct application; [discriminate|]. cbn [rbind] in Run.
           destruct (filter (fun x => negb (is_some (nlook x tm))) (fixed W metadata_fuel symbol))
             as [|absent rest] eqn:Available; [|discriminate]. cbn [rbind] in Run. injection Run as <- <-.
           eapply IEFun; [exact Resolve|exact Declaration|exact Ast|apply fixed_available; exact Available|apply readings_exact].
    + intros stack p ob tm syntax entries meanings Run.
      destruct syntax as [|[syntax|left right] rest].
      * cbn [items_entries] in Run. injection Run as <- <-. constructor.
      * cbn [items_entries] in Run.
        apply rbind_ok in Run as [[head_entries head_meanings] [Head Run]].
        apply rbind_ok in Run as [[tail_entries tail_meanings] [Tail Run]]. injection Run as <- <-.
        eapply IsCons; [eapply ItemSound; exact Head|eapply ItemsSound; exact Tail].
      * cbn [items_entries] in Run.
        apply rbind_ok in Run as [[head_entries head_meanings] [Head Run]].
        apply rbind_ok in Run as [left_type [Left Run]].
        apply rbind_ok in Run as [right_type [Right Run]].
        apply rbind_ok in Run as [[tail_entries tail_meanings] [Tail Run]]. injection Run as <- <-.
        eapply IsEq; [eapply ItemsSound; exact Head|eapply ty_of_sound; exact Left|
          eapply ty_of_sound; exact Right|eapply ItemsSound; exact Tail].
Qed.

Corollary item_entries_sound : forall n stack p ob tm syntax entries meanings,
  item_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
  ItemEntries W p ob tm syntax entries meanings.
Proof. intro n. exact (proj1 (entries_sound n)). Qed.

Corollary items_entries_sound : forall n stack p ob tm syntax entries meanings,
  items_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
  ItemsEntries W p ob tm syntax entries meanings.
Proof. intro n. exact (proj2 (entries_sound n)). Qed.

End ContextEntries.

From NM Require Import EaDependencyFuel EaResultBudget EaMetadataSound EaMain EaWorldSound ElabWorld.

Theorem entries_sound_at_budget : forall E bound,
  (forall symbol d, w_decl (e_W E) symbol = Some d -> symbol < bound) ->
  metadata_budget (e_W E) bound < e_metadata_fuel E -> forall n,
  (forall stack p ob tm syntax entries meanings,
    item_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
    ItemEntries (e_W E) p ob tm syntax entries meanings) /\
  (forall stack p ob tm syntax entries meanings,
    items_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
    ItemsEntries (e_W E) p ob tm syntax entries meanings).
Proof.
  intros E bound Bounded Enough n. apply entries_sound.
  - intro symbol. apply (EaResultBudget.fixed_exact _ bound Bounded); exact Enough.
  - intros symbol tm. apply (fixed_readings_exact _ bound Bounded); exact Enough.
  - intros symbol tm. apply app_of_sound.
    pose proof (signatures_depth_bounded _ bound Bounded symbol).
    unfold metadata_budget in Enough. lia.
Qed.

Corollary bundle_world_r_entries_sound : forall bundle W bound,
  EaMain.bundle_world_r bundle = Ok (W, bound) ->
  forall contexts strategy modules mains n,
  let E := Env (ElabWorld.with_context W contexts) strategy modules mains
    EaMain.elab_fuel (EaMain.metadata_fuel W bound) in
  (forall stack p ob tm syntax entries meanings,
    item_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
    ItemEntries (e_W E) p ob tm syntax entries meanings) /\
  (forall stack p ob tm syntax entries meanings,
    items_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
    ItemsEntries (e_W E) p ob tm syntax entries meanings).
Proof.
  intros bundle W bound Run contexts strategy modules mains n. cbn zeta.
  apply (entries_sound_at_budget _ bound).
  - exact (EaWorldSound.bundle_world_r_bound _ _ _ Run).
  - exact (metadata_fuel_sufficient W bound).
Qed.
