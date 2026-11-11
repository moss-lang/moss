(* Contextual method calls, including the family translation for a missing
   provision. Arguments are checked at the original caller. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit EaBase EaExpr EaFamily EaControlSound EaChecked.
From NM Require Import LegacyBodyJudgments.
From NM Require EaCallExpressionSound EaFamilyUseSound EaSatisfiesSound PublicContextFacts.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Local Lemma pure_read : forall A (read : er A) before value after,
  pure read before = Ok (value, after) -> read = Ok value /\ before = after.
Proof. intros A [found|] before value after Run; [now injection Run as -> ->|discriminate]. Qed.

Section Methods.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.

Hypothesis CallerSection : forall F C state, valid F C state -> PublicContextFacts.CxSection C.
Hypothesis Families : EaFamilyUseSound.FamilyUseSound E.

Definition ProvidedPostAt (n : nat) : Prop :=
  forall F C receiver t method arguments,
  Checked post (valid F C) (fun _ => valid F C) (provided E R n F C receiver t method arguments)
    (fun '(code, result) _ => Provided W P F C receiver t method arguments code result).

Theorem provided_post_sound : forall n,
  (forall depth, depth < n -> ExPostAt E P valid post R depth) ->
  ProvidedPostAt n.
Proof.
  intros [|n] Below F C receiver t method arguments before [code result] after Run; [discriminate|].
  cbn [provided] in Run.
  destruct (cmth C t method) as [signature|] eqn:Found.
  - apply mbind_ok in Run as (values & finish & Arguments & Run).
    injection Run as <- <- <-.
    destruct (args_post_sound E R P valid post n (fun depth Less => Below depth ltac:(lia))
      _ _ _ _ _ _ _ _ _ Arguments) as [Back Typed].
    split; [exact Back|]. intros Valid Post.
    destruct (Typed Valid Post) as [ValidAfter TypedArgs].
    split; [exact ValidAfter|].
    change (rk C t method) with (rkof C (mkey t method)).
    rewrite EaSatisfiesSound.rkof_exact.
    eapply PrIn; [now rewrite <- EaCallExpressionSound.cmth_exact|exact TypedArgs].
  - apply mbind_ok in Run as ([ [[[[u matching] types] sm] result_context] keys ] & family_state & Family & Run).
    apply pure_read in Family as [FamilyRun <-].
    apply mbind_ok in Run as (signature & start & Read & Run).
    destruct (cmth result_context t method) as [g|] eqn:Signature; [|discriminate Read].
    injection Read as <- <-.
    apply mbind_ok in Run as (values & finish & Arguments & Run).
    injection Run as <- <- <-.
    destruct (args_post_sound E R P valid post n (fun depth Less => Below depth ltac:(lia))
      _ _ _ _ _ _ _ _ _ Arguments) as [Back Typed].
    split; [exact Back|]. intros Valid Post.
    destruct (Typed Valid Post) as [ValidAfter TypedArgs].
    destruct (Families _ _ _ _ _ _ _ _ _ _
      (CallerSection _ _ _ Valid) (CallerSection _ _ _ Valid) FamilyRun)
      as (formal & needs & App & Match & Present & Only & InForce & Map).
    split; [exact ValidAfter|].
    change (rk result_context t method) with (rkof result_context (mkey t method)).
    rewrite Map, (map_ext _ _ (EaSatisfiesSound.rkof_exact result_context)), EaSatisfiesSound.rkof_exact.
    change (Provided W P F C receiver t method arguments
      (translate_family_use C (FamilyUse u matching types formal needs result_context keys)
        (EMethod receiver method (rkey result_context t method) values)) (sg_ret g)).
    eapply PrFamily; try eassumption; now rewrite <- EaCallExpressionSound.cmth_exact.
Qed.
End Methods.

From NM Require EaDefinedCallSound EaContextTableSound EaFamilySound EaTypesSound.

Section MethodResolution.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.
Hypothesis CallerSection : forall F C state, valid F C state -> PublicContextFacts.CxSection C.
Hypothesis Contexts : EaContextTableSound.ContextTableSound W.
Hypothesis DemandCheck : EaDefinedCallSound.DemandSound E.
Hypothesis Families : EaFamilyUseSound.FamilyUseSound E.
Hypothesis World : exists bundle, WorldOf bundle W.

Local Lemma attached_declaration_name : forall h name method d rn n params result body,
  Attached W h name method -> w_decl W method = Some d ->
  d_ast d = ADFn (RAttached rn) n params result body -> n = name.
Proof.
  intros h name method d rn n params result body Found Declared Ast.
  destruct World as [bundle Read].
  apply (proj1 (@wo_attached _ _ Read h name method)) in Found.
  destruct Found as (other & receiver & ps & rt & b & Declaration & Shape & _).
  rewrite Declared in Declaration. injection Declaration as <-.
  rewrite Ast in Shape. now inversion Shape.
Qed.

Local Lemma called_method_context : forall method d rn n ps rt b formal needs,
  w_decl W method = Some d -> d_ast d = ADFn (RAttached rn) n ps rt b ->
  w_context W method = Some (formal, needs) -> CtxOf W method formal needs.
Proof.
  intros method d rn n ps rt b formal needs Declared Ast Found.
  specialize (Contexts _ _ _ Found). unfold kind_sym in Contexts.
  now rewrite Declared, Ast in Contexts.
Qed.

Local Lemma attached_lookup : forall t name method,
  (match head t with Some h => w_attached W h name | None => None end) = Some method ->
  exists h, head t = Some h /\ Attached W h name method.
Proof.
  intros t name method Found. destruct (head t) as [h|] eqn:Head; [|discriminate].
  exists h. auto.
Qed.

Local Lemma attached_absent : forall t name,
  (match head t with Some h => w_attached W h name | None => None end) = None ->
  forall h method, head t = Some h -> ~ Attached W h name method.
Proof.
  intros t name NoneFound h method Head Found. unfold Attached in Found.
  rewrite Head, Found in NoneFound. discriminate.
Qed.

Definition MethodCallPostAt (n : nat) : Prop :=
  forall F C receiver t path arguments,
  Checked post (valid F C) (fun _ => valid F C)
    (meth_call E R n F C receiver t path arguments)
    (fun '(code, result) _ => MethCall W P F C receiver t path arguments code result).

Theorem method_call_post_sound : forall n,
  (forall depth, depth < n -> ExPostAt E P valid post R depth) ->
  MethodCallPostAt n.
Proof.
  intros [|n] Below F C receiver t path arguments before [code result] after Run; [discriminate|].
  pose proof (provided_post_sound E R P valid post CallerSection Families n
    (fun depth Less => Below depth ltac:(lia))) as ProvidedCall.
  cbn [meth_call] in Run. fold W in Run.
  destruct path as [|name [|qualified rest]].
  - discriminate.
  - set (detached := match w_scope W (fr_mod F) NSDetached name with
      | Some (ESym method) => Some method | _ => None end) in Run.
    set (attached := match head t with Some h => w_attached W h name | None => None end) in Run.
    destruct attached as [attached_method|] eqn:Attachment.
    2: {
      destruct detached as [method|] eqn:Detached.
      - destruct (ProvidedCall _ _ _ _ _ _ _ _ _ Run) as [Back Typed].
        split; [exact Back|]. intros Valid Post.
        destruct (Typed Valid Post) as [Final Proof].
        split; [exact Final|].
        eapply MCDetached; [eapply attached_absent; exact Attachment| |exact Proof].
        unfold detached in Detached.
        destruct (w_scope W (fr_mod F) NSDetached name) as [[symbol|module_path]|] eqn:Found;
          try discriminate.
        now injection Detached as <-.
      - destruct (head t); discriminate Run.
    }
    destruct (attached_lookup _ _ _ Attachment) as (h & Head & Attached).
    set (detached_available := match detached with
      | Some method => is_some (cmth C t method) | None => false end) in Run.
    destruct detached_available eqn:Available.
    + destruct detached as [method|] eqn:Detached; [|discriminate Run].
      destruct (ProvidedCall _ _ _ _ _ _ _ _ _ Run) as [Back Typed].
      split; [exact Back|]. intros Valid Post.
      destruct (Typed Valid Post) as [Final Proof]. split; [exact Final|].
      eapply MCProvisionWins; [exact Head|exact Attached| | |exact Proof].
      * unfold detached in Detached.
        destruct (w_scope W (fr_mod F) NSDetached name) as [[symbol|module_path]|];
          try discriminate. now injection Detached as <-.
      * unfold detached_available in Available.
        rewrite <- EaCallExpressionSound.cmth_exact.
        destruct (cmth C t method); [discriminate|discriminate Available].
    + assert (NoneDetached : ~ exists method,
          w_scope W (fr_mod F) NSDetached name = Some (ESym method) /\
          mlook t method (c_mths C) <> None).
      {
        intros (method & Scoped & Provided).
        unfold detached_available, detached in Available. rewrite Scoped in Available.
        rewrite <- EaCallExpressionSound.cmth_exact in Provided.
        destruct (cmth C t method); [discriminate Available|contradiction].
      }
      apply mbind_ok in Run as (d & start & Declaration & Run).
      destruct (w_decl W attached_method) as [declared|] eqn:Declared; [|discriminate Declaration].
      injection Declaration as <- <-.
      destruct (d_ast declared) as [| | | | | |rcv m ps rt body| |] eqn:Ast; try discriminate Run.
      destruct rcv as [ | | rn ]; try discriminate Run.
      pose proof (attached_declaration_name _ _ _ _ _ _ _ _ _ Attached Declared Ast) as <-.
      assert (ReadAst : ast_of W attached_method = Some (ADFn (RAttached rn) m ps rt body)).
      { unfold ast_of. rewrite Declared. cbn [option_map]. now rewrite Ast. }
      destruct body as [body|].
      * apply mbind_ok in Run as (prepared & start & Read & Run).
        apply pure_read in Read as [Prepared <-].
        apply mbind_ok in Run as (values & finish & Arguments & Run).
        injection Run as <- <- <-.
        destruct (args_post_sound E R P valid post n (fun depth Less => Below depth ltac:(lia))
          _ _ _ _ _ _ _ _ _ Arguments) as [Back Typed].
        split; [exact Back|]. intros Valid Post.
        destruct (Typed Valid Post) as [Final Proof].
        set (rargs := match t with TNom _ xs => xs | _ => [] end) in *.
        assert (Sectioned : PublicContextFacts.CxSection (family_context C rargs)) by
          (apply EaFamilySound.family_context_section; exact (CallerSection _ _ _ Valid)).
        destruct (EaDefinedCallSound.check_defined_call_sound E attached_method (family_context C rargs)
          (Some t) prepared DemandCheck Sectioned
          (fun formal needs => called_method_context _ _ _ _ _ _ _ formal needs Declared Ast)
          Prepared) as (formal & needs & steps & called & Formed & Force & Demands &
            Types & Satisfied & Signature & Emit).
        split; [exact Final|]. rewrite Emit.
        eapply MCAttachedDefined; [exact Head|exact Attached|exact NoneDetached|
          exists rn, ps, rt, body; exact ReadAst|reflexivity|exact Formed|exact Demands|
          exact Satisfied|exact Types|exact Signature|exact Proof|exact Force].
      * destruct (ProvidedCall _ _ _ _ _ _ _ _ _ Run) as [Back Typed].
        split; [exact Back|]. intros Valid Post.
        destruct (Typed Valid Post) as [Final Proof]. split; [exact Final|].
        eapply MCAttachedAbstract; [exact Head|exact Attached|exact NoneDetached|
          exists rn, ps, rt; exact ReadAst|exact Proof].
  - apply mbind_ok in Run as (method & start & Read & Run).
    destruct (resolve_detached W (fr_mod F) (name :: qualified :: rest)) as [chosen|] eqn:Resolved;
      [|discriminate Read].
    injection Read as <- <-.
    destruct (ProvidedCall _ _ _ _ _ _ _ _ _ Run) as [Back Typed].
    split; [exact Back|]. intros Valid Post.
    destruct (Typed Valid Post) as [Final Proof]. split; [exact Final|].
    eapply MCQualified; [cbn; lia|exact Resolved|exact Proof].
Qed.
End MethodResolution.
