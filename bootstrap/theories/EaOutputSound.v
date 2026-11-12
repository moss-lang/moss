(* Installation of a functor's result follows Audit's output telescope. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes EaFamily EaTypesSound.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope list_scope.

Lemma ctx_spec_items_sound : forall items specs,
  ctx_spec_items items = Some specs -> items = map CISpec specs.
Proof.
  intro items. induction items as [|[syntax|left right] items IH]; intros specs Run; try discriminate.
  - injection Run as <-. reflexivity.
  - cbn [ctx_spec_items] in Run. destruct (ctx_spec_items items) as [tail|] eqn:Tail; [|discriminate].
    injection Run as <-. cbn [map]. rewrite (IH _ eq_refl). reflexivity.
Qed.

Section OutputEntries.
Variable E : env.
Let W := e_W E.
Let fuel := e_fuel E.
Let metadata_fuel := e_metadata_fuel E.
Hypothesis fixed_exact : forall u x, In x (fixed W metadata_fuel u) <-> Fixed W u x.
Hypothesis readings_exact : forall u tm, FixedReadings W u tm (fixed_readings W metadata_fuel u tm).
Hypothesis applications_sound : forall symbol tm, AppOf W symbol tm (app_of W metadata_fuel symbol tm).
Hypothesis outputs_exact : forall symbol x, In x (fun_outputs W metadata_fuel symbol) <-> FunOutputs W symbol x.

Theorem output_brackets_sound : forall p tm syntax bindings,
  output_brackets E p tm syntax = Ok bindings -> OutputBrackets W p tm syntax bindings.
Proof.
  intros p tm [source|] bindings Run.
  - unfold output_brackets in Run. fold W fuel in Run.
    apply rbind_ok in Run as [values [Read Run]]. injection Run as <-.
    apply OBrSome. eapply rmap_sound; [|exact Read].
    intros [pa [[sp|] [detached|] application]] [symbol meaning] Member; try discriminate.
    cbn [fst snd] in Member.
    destruct (resolve_sym W p pa) as [target|] eqn:Resolve; [|discriminate]. cbn [rbind] in Member.
    destruct (kind_is W target DKType) eqn:Kind; [|discriminate]. cbn [rbind] in Member.
    apply rbind_ok in Member as [ty0 [Meaning Member]]. injection Member as <- <-.
    exists sp, application. split; [reflexivity|]. split; [exact Resolve|].
    split; [apply kind_is_spec; exact Kind|eapply ty_of_sound; exact Meaning].
  - unfold output_brackets in Run. injection Run as <-. constructor.
Qed.

Lemma output_fixed_available : forall u (tm : subst),
  forallb (fun x => is_some (nlook x tm)) (fixed W metadata_fuel u) = true ->
  forall x, Fixed W u x -> nlook x tm <> None.
Proof.
  intros u tm Complete x Fixed Missing.
  apply forallb_forall with (x:=x) in Complete; [|apply fixed_exact; exact Fixed].
  rewrite Missing in Complete. discriminate.
Qed.

Theorem output_entries_sound : forall n,
  (forall stack p ob tm syntax entries meanings,
    output_item_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
    OutputItemEntries W p ob tm syntax entries meanings) /\
  (forall stack p ob tm syntax entries meanings,
    output_items_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
    OutputItemsEntries W p ob tm syntax entries meanings).
Proof.
  induction n as [|n [ItemSound ItemsSound]].
  - split; intros; discriminate.
  - split.
    + intros stack p ob tm [[pa|] [dn|] application] entries meanings Run; try discriminate.
      * cbn [output_item_entries] in Run. fold W fuel metadata_fuel in Run.
        destruct (resolve_sym W p pa) as [receiver|] eqn:Resolve; [|discriminate]. cbn [rbind] in Run.
        destruct (method_named W p receiver dn) as [method|] eqn:Method; [|discriminate]. cbn [rbind] in Run.
        apply rbind_ok in Run as [bindings [Brackets Run]].
        apply rbind_ok in Run as [receiver_type [Receiver Run]].
        apply rbind_ok in Run as [signature [Signature Run]]. injection Run as <- <-.
        eapply OIEMeth; [exact Resolve|eapply method_named_sound; exact Method|
          eapply output_brackets_sound; exact Brackets|eapply recv_ty_sound; exact Receiver|
          eapply sig_of_sound; exact Signature|apply applications_sound].
      * cbn [output_item_entries] in Run. fold W fuel metadata_fuel in Run.
        destruct (resolve_sym W p pa) as [symbol|] eqn:Resolve; [|discriminate]. cbn [rbind] in Run.
        apply rbind_ok in Run as [d [Declaration Run]]. apply decl_of_sound in Declaration.
        apply rbind_ok in Run as [bindings [Brackets Run]].
        destruct (d_ast d) as [items declarations|name|name syntax|name parameters payload|name|name value_type initial|
          receiver name parameters result body|name items|name parameters arguments results body] eqn:Ast;
          try discriminate.
        -- injection Run as <- <-. eapply OIEType; [exact Resolve| |eapply output_brackets_sound; exact Brackets].
           unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
        -- injection Run as <- <-.
           eapply OIENothing; [exact Resolve| |eapply output_brackets_sound; exact Brackets|right; left; reflexivity].
           unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
        -- injection Run as <- <-.
           eapply OIENothing; [exact Resolve| |eapply output_brackets_sound; exact Brackets|left; reflexivity].
           unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
        -- injection Run as <- <-.
           eapply OIENothing; [exact Resolve| |eapply output_brackets_sound; exact Brackets|left; reflexivity].
           unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
        -- destruct initial as [initial|].
           ++ injection Run as <- <-.
              eapply OIENothing; [exact Resolve| |eapply output_brackets_sound; exact Brackets|
                right; right; left; split; [reflexivity|]].
              ** unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
              ** exists d, name, value_type, initial. split; assumption.
           ++ apply rbind_ok in Run as [meaning [Meaning Run]]. injection Run as <- <-.
              eapply OIEVal; [exact Resolve|exact Declaration|exact Ast|
                eapply output_brackets_sound; exact Brackets|eapply ty_of_sound; exact Meaning|apply applications_sound].
        -- destruct receiver; try discriminate. destruct body as [body|].
           ++ injection Run as <- <-.
              eapply OIENothing; [exact Resolve| |eapply output_brackets_sound; exact Brackets|
                right; right; right; split; [reflexivity|]].
              ** unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
              ** exists d, RPlain, name, parameters, result, body. split; assumption.
           ++ apply rbind_ok in Run as [signature [Signature Run]]. injection Run as <- <-.
              eapply OIEFn; [exact Resolve| |exact Declaration|exact Ast|
                eapply output_brackets_sound; exact Brackets|eapply sig_of_sound; exact Signature|apply applications_sound].
              unfold kind_sym. rewrite Declaration. cbn. rewrite Ast. reflexivity.
        -- destruct (negb (nmem symbol stack)) eqn:Acyclic; [|discriminate]. cbn [rbind] in Run.
           destruct (ctx_spec_items items) as [specs|] eqn:Specs; [|discriminate]. cbn [rbind] in Run.
           eapply OIECtx; [exact Resolve|exact Declaration|exact Ast|eapply ctx_spec_items_sound; exact Specs|
             eapply output_brackets_sound; exact Brackets|eapply ItemsSound; exact Run].
        -- destruct body as [body|]; [discriminate|].
           destruct (forallb (fun x => is_some (nlook x (bindings ++ ob ++ tm))) (fixed W metadata_fuel symbol))
             eqn:Available; [|discriminate]. cbn [rbind] in Run. injection Run as <- <-.
           eapply OIEFun; [exact Resolve|exact Declaration|exact Ast|eapply output_brackets_sound; exact Brackets|
             apply output_fixed_available; exact Available|apply readings_exact].
    + intros stack p ob tm syntax entries meanings Run. destruct syntax as [|syntax rest].
      * cbn [output_items_entries] in Run. injection Run as <- <-. constructor.
      * cbn [output_items_entries] in Run.
        apply rbind_ok in Run as [[head_entries head_meanings] [Head Run]].
        apply rbind_ok in Run as [[tail_entries tail_meanings] [Tail Run]]. injection Run as <- <-.
        eapply OIsCons; [eapply ItemSound; exact Head|eapply ItemsSound; exact Tail].
Qed.

Corollary output_item_entries_sound : forall n stack p ob tm syntax entries meanings,
  output_item_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
  OutputItemEntries W p ob tm syntax entries meanings.
Proof. intro n. exact (proj1 (output_entries_sound n)). Qed.

Corollary output_items_entries_sound : forall n stack p ob tm syntax entries meanings,
  output_items_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
  OutputItemsEntries W p ob tm syntax entries meanings.
Proof. intro n. exact (proj2 (output_entries_sound n)). Qed.

Theorem provide_result_sound : forall symbol context out result keys,
  provide_result E symbol context out = Ok (result, keys) -> ProvideResult W symbol context out result keys.
Proof.
  intros symbol context out result keys Run. unfold provide_result in Run. fold W fuel metadata_fuel in Run.
  destruct (functor_decl W symbol) as [[d [[[parameters arguments] results] body]]|] eqn:Read; [|discriminate].
  cbn [rbind] in Run.
  apply rbind_ok in Run as [[entries meanings] [Entries Run]]. injection Run as <- <-.
  unfold functor_decl in Read. destruct (w_decl W symbol) as [found|] eqn:Declaration; [|discriminate].
  destruct (functor_parts (d_ast found)) as [[[[ps args] outs] code]|] eqn:Parts; [|discriminate].
  injection Read as <- <- <- <- <-.
  exists found, ps, args, outs, code, (fun_outputs W metadata_fuel symbol), entries, meanings.
  split; [exact Declaration|]. split; [exact Parts|]. split; [apply outputs_exact|].
  split; [eapply (proj2 (output_entries_sound fuel)); exact Entries|].
  split; reflexivity.
Qed.

End OutputEntries.

From NM Require Import EaDependencyFuel EaContextFuel EaResultBudget EaMetadataSound EaMain EaWorldSound ElabWorld.

Theorem output_entries_sound_at_budget : forall E bound,
  (forall symbol d, w_decl (e_W E) symbol = Some d -> symbol < bound) ->
  metadata_budget (e_W E) bound < e_metadata_fuel E -> forall n,
  (forall stack p ob tm syntax entries meanings,
    output_item_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
    OutputItemEntries (e_W E) p ob tm syntax entries meanings) /\
  (forall stack p ob tm syntax entries meanings,
    output_items_entries E n stack p ob tm syntax = Ok (entries, meanings) ->
    OutputItemsEntries (e_W E) p ob tm syntax entries meanings).
Proof.
  intros E bound Bounded Enough n. apply output_entries_sound.
  - intro symbol. apply (EaResultBudget.fixed_exact _ bound Bounded); exact Enough.
  - intros symbol tm. apply (fixed_readings_exact _ bound Bounded); exact Enough.
  - intros symbol tm. apply app_of_sound.
    pose proof (signatures_depth_bounded _ bound Bounded symbol).
    unfold metadata_budget in Enough. lia.
Qed.

Theorem provide_result_sound_at_budget : forall E bound,
  (forall symbol d, w_decl (e_W E) symbol = Some d -> symbol < bound) ->
  metadata_budget (e_W E) bound < e_metadata_fuel E ->
  forall symbol context out result keys,
  provide_result E symbol context out = Ok (result, keys) ->
  ProvideResult (e_W E) symbol context out result keys.
Proof.
  intros E bound Bounded Enough. apply provide_result_sound.
  - intro symbol. apply (EaResultBudget.fixed_exact _ bound Bounded); exact Enough.
  - intros symbol tm. apply (fixed_readings_exact _ bound Bounded); exact Enough.
  - intros symbol tm. apply app_of_sound.
    pose proof (signatures_depth_bounded _ bound Bounded symbol).
    unfold metadata_budget in Enough. lia.
  - intro symbol. apply (fun_outputs_exact _ bound Bounded).
    unfold metadata_budget, metadata_depth in Enough.
    pose proof (Nat.le_max_l bound (largest (fun symbol => match w_decl (e_W E) symbol with
      Some d => declaration_depth (d_ast d) | None => 0 end) (seq 0 bound))). lia.
Qed.

Corollary bundle_world_r_provide_result_sound : forall bundle W bound,
  EaMain.bundle_world_r bundle = Ok (W, bound) ->
  forall contexts strategy modules mains,
  let E := Env (ElabWorld.with_context W contexts) strategy modules mains
    EaMain.elab_fuel (EaMain.metadata_fuel W bound) in
  forall symbol context out result keys,
  provide_result E symbol context out = Ok (result, keys) ->
  ProvideResult (e_W E) symbol context out result keys.
Proof.
  intros bundle W bound Run contexts strategy modules mains. cbn zeta.
  apply (provide_result_sound_at_budget _ bound).
  - exact (EaWorldSound.bundle_world_r_bound _ _ _ Run).
  - exact (metadata_fuel_sufficient W bound).
Qed.
