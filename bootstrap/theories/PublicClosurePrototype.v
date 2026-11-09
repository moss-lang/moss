(* A deliberately partial closure compiler over the public runtime syntax.
   Unsupported expressions return None; there is no interpreter fallback.
   Functor bodies use an explicit compiled-table correctness premise, which
   the closed example below discharges for its concrete nonrecursive table.
   This is a boundary experiment, not an implementation of Public.CORRECT. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit.
Import Audit.Semantics ListNotations.

Module Compiler (Prims : PRIMITIVES).
Module R := Interpreter Prims.
Import R.

Definition code := nat -> environment -> locals -> Prims.mstate -> evaluation.
Definition bcode := nat -> environment -> locals -> Prims.mstate ->
  (environment * locals * Prims.mstate) + evaluation.
Definition table := usym -> option (tele * bcode).

Section Compile.
Variable P : program_view.
Variable bodies : table.

Fixpoint compile_atom (e : expr) : option code :=
  match e with
  | EUnit => Some (fun fuel d g st =>
      match fuel with 0 => Evaluation OutOfFuel g st | S _ => Evaluation (Normal VUnit) g st end)
  | EVar x => Some (fun fuel d g st =>
      match fuel with 0 => Evaluation OutOfFuel g st | S _ =>
        match lookup_local x g with
        | Some w => Evaluation (Normal w) g st
        | None => Evaluation (Stuck e_var) g st end end)
  | EVal v => Some (fun fuel d g st =>
      match fuel with 0 => Evaluation OutOfFuel g st | S _ =>
        match lookup_provision (IVal v) d with
        | Some (ValueProvision w) => Evaluation (Normal w) g st
        | _ => Evaluation (Stuck e_val) g st end end)
  | ETag n th child =>
      match compile_atom child with
      | Some c => Some (fun fuel d g st =>
          match fuel with 0 => Evaluation OutOfFuel g st | S k =>
            match c k d g st with
            | Evaluation (Normal w) g1 st1 => Evaluation (check_tag P d n th w) g1 st1
            | r => r end end)
      | None => None end
  | _ => None
  end.

Lemma atom_correct : forall e c, compile_atom e = Some c ->
  forall fuel d g st, c fuel d g st = eval fuel P d g st e.
Proof.
  fix IH 1. intros e c Compiled.
  destruct e; cbn [compile_atom] in Compiled; try discriminate.
  - injection Compiled as <-. intros [|fuel] d g st; reflexivity.
  - injection Compiled as <-. intros [|fuel] d g st; reflexivity.
  - injection Compiled as <-. intros [|fuel] d g st; reflexivity.
  - destruct (compile_atom e) as [child|] eqn:Child; [|discriminate].
    injection Compiled as <-. intros [|fuel] d g st; [reflexivity|].
    cbn [eval]. rewrite (IH e child Child); reflexivity.
Qed.

Definition bnil : bcode := fun fuel d g st =>
  match fuel with 0 => inr (Evaluation OutOfFuel g st) | S _ => inl (d,g,st) end.

Definition compiled_apply (fuel : nat) (d : environment) (u : usym) (th : subst)
    (destinations : list (tsym * tsym)) (sm : smap) (st : Prims.mstate) :
    (environment * Prims.mstate) + evaluation :=
  match fuel with
  | 0 => inr (Evaluation OutOfFuel [] st)
  | S k =>
      match lookup_provision (IFun u) d with
      | Some (FunctorClosure h sms dc) =>
          match bodies h with
          | Some (outputs, body) =>
              match select_provisions d sm with
              | Some d1 =>
                  match adapt_provisions d1 sms with
                  | Some d2 =>
                      match body k (application_environment P d h th (append_environment d2 dc)) [] st with
                      | inl (d3, _, st3) =>
                          match select_provisions d3 (result_smap (result_substitution th destinations) outputs
                                  (match view_functor P h with Some code => fc_sources code | None => [] end)) with
                          | Some dr =>
                              match result_environment d destinations d3 outputs dr with
                              | Some delta => inl (delta, st3)
                              | None => inr (Evaluation (Stuck e_compose) [] st3) end
                          | None => inr (Evaluation (Stuck e_compose) [] st3) end
                      | inr rr => inr rr end
                  | None => inr (Evaluation (Stuck e_chain) [] st) end
              | None => inr (Evaluation (Stuck e_compose) [] st) end
          | None => inr (Evaluation (Stuck e_nobody) [] st) end
      | _ => inr (Evaluation (Stuck e_fun) [] st) end
  end.

Definition tables_correct : Prop := forall h,
  match view_functor P h, bodies h with
  | None, None => True
  | Some body, Some (outputs, c) => outputs = fc_outputs body /\
      forall fuel d g st, c fuel d g st = eval_bindings fuel P d g st (fc_bindings body)
  | _, _ => False
  end.

Lemma apply_correct : tables_correct -> forall fuel d u th destinations sm st,
  compiled_apply fuel d u th destinations sm st = apply_functor fuel P d u th destinations sm st.
Proof.
  intros Tables [|fuel] d u th destinations sm st; [reflexivity|].
  cbn [compiled_apply apply_functor].
  destruct (lookup_provision (IFun u) d) as [entry|]; [|reflexivity].
  destruct entry as [value|function captured|h maps captured|operation held]; try reflexivity.
  specialize (Tables h).
  destruct (view_functor P h) as [body|] eqn:Source;
    destruct (bodies h) as [[outputs c]|] eqn:Cached; try contradiction; try reflexivity.
  destruct Tables as [-> Body].
  destruct (select_provisions d sm) as [d1|]; [|reflexivity].
  destruct (adapt_provisions d1 maps) as [d2|]; [|reflexivity].
  rewrite Body. reflexivity.
Qed.

Definition compile_step (binding : binding) : option bcode :=
  match binding with
  | BType t tau => Some (fun _ d g st => inl (bind_type d t tau, g, st))
  | BValue v e =>
      match compile_atom e with
      | None => None
      | Some c => Some (fun fuel d g st =>
          match c fuel d g st with
          | Evaluation (Normal w) g1 st1 => inl (install_provision (IVal v) (ValueProvision w) d, g1, st1)
          | rr => inr rr end) end
  | BFunction f body plan sm => Some (fun _ d g st =>
      match select_provisions d sm with
      | Some dc => inl (install_provision (IFn f) (FunctionClosure body (typed_capture d plan dc)) d,g,st)
      | None => inr (Evaluation (Stuck e_compose) g st) end)
  | BMethod tau m th body plan sm => Some (fun _ d g st =>
      match select_provisions d sm with
      | Some dc => inl (install_provision (IMth tau m th) (FunctionClosure body (typed_capture d plan dc)) d,g,st)
      | None => inr (Evaluation (Stuck e_compose) g st) end)
  | BFunctor u body linked captures => Some (fun _ d g st =>
      match select_provisions d captures with
      | Some dc => inl (install_provision (IFun u) (FunctorClosure body [linked] (capture_environment d dc)) d,g,st)
      | None => inr (Evaluation (Stuck e_compose) g st) end)
  | BCopyFunctor u provider sm => Some (fun _ d g st =>
      match lookup_provision (IFun provider) d with
      | Some (FunctorClosure body maps dc) =>
          inl (install_provision (IFun u) (FunctorClosure body (maps ++ [sm]) dc) d,g,st)
      | _ => inr (Evaluation (Stuck e_fun) g st) end)
  | BApply u th destinations sm => Some (fun fuel d g st =>
      match compiled_apply fuel d u th destinations sm st with
      | inl (dr, st1) => inl (append_environment dr d,g,st1)
      | inr rr => inr (with_g g rr) end)
  | BPrimitive key operation => Some (fun _ d g st =>
      inl (install_provision key (primitive_closure d key operation) d,g,st))
  end.

Definition bcons (c rest : bcode) : bcode := fun fuel d g st =>
  match fuel with
  | 0 => inr (Evaluation OutOfFuel g st)
  | S k => match c k d g st with
      | inl (d1,g1,st1) => rest k d1 g1 st1
      | inr rr => inr rr end
  end.

Fixpoint compile_bindings (bs : list binding) : option bcode :=
  match bs with
  | [] => Some bnil
  | b :: rest => match compile_step b, compile_bindings rest with
      | Some c, Some cr => Some (bcons c cr)
      | _, _ => None end
  end.

Lemma step_correct : tables_correct -> forall b c,
  compile_step b = Some c -> forall fuel rest d g st,
  eval_bindings (S fuel) P d g st (b :: rest) =
    match c fuel d g st with
    | inl (d1,g1,st1) => eval_bindings fuel P d1 g1 st1 rest
    | inr rr => inr rr end.
Proof.
  intros Tables b c Compiled.
  destruct b as [t tau|v e|f body plan inputs|tau method theta body plan inputs|
    u body inputs captures|u provider inputs|u theta destinations inputs|key operation];
    cbn [compile_step] in Compiled.
  - injection Compiled as <-. intros; reflexivity.
  - destruct (compile_atom e) as [atom|] eqn:Atom; [|discriminate].
    injection Compiled as <-. intros fuel rest d g st. cbn [eval_bindings].
    rewrite (atom_correct e atom Atom).
    destruct (eval fuel P d g st e) as [out g1 st1]; destruct out; reflexivity.
  - injection Compiled as <-. intros fuel rest d g st. cbn [eval_bindings].
    destruct (select_provisions d inputs); reflexivity.
  - injection Compiled as <-. intros fuel rest d g st. cbn [eval_bindings].
    destruct (select_provisions d inputs); reflexivity.
  - injection Compiled as <-. intros fuel rest d g st. cbn [eval_bindings].
    destruct (select_provisions d captures); reflexivity.
  - injection Compiled as <-. intros fuel rest d g st. cbn [eval_bindings].
    destruct (lookup_provision (IFun provider) d) as [entry|];
      [destruct entry|]; reflexivity.
  - injection Compiled as <-. intros fuel rest d g st. cbn [eval_bindings].
    rewrite (apply_correct Tables).
    destruct (apply_functor fuel P d u theta destinations inputs st) as [[dr st1]|rr]; reflexivity.
  - injection Compiled as <-. intros; reflexivity.
Qed.

Theorem bindings_correct : tables_correct -> forall bs c,
  compile_bindings bs = Some c -> forall fuel d g st,
  c fuel d g st = eval_bindings fuel P d g st bs.
Proof.
  intros Tables bs. induction bs as [|b rest IH]; intros c Compiled.
  - injection Compiled as <-. intros [|fuel] d g st; reflexivity.
  - cbn [compile_bindings] in Compiled.
    destruct (compile_step b) as [first|] eqn:First;
      destruct (compile_bindings rest) as [tail|] eqn:Tail; try discriminate.
    injection Compiled as <-. intros [|fuel] d g st; [reflexivity|].
    cbn [bcons]. rewrite (step_correct Tables b first First).
    destruct (first fuel d g st) as [[[d1 g1] st1]|rr]; [apply IH; reflexivity|reflexivity].
Qed.

Fixpoint compile_expr (e : expr) : option code :=
  match e with
  | EBind b rest =>
      match compile_bindings [b], compile_expr rest with
      | Some cb, Some cr => Some (fun fuel d g st =>
          match fuel with
          | 0 => Evaluation OutOfFuel g st
          | S k => match cb k d g st with
              | inl (d1,g1,st1) => cr k d1 g1 st1
              | inr rr => rr end end)
      | _, _ => None end
  | _ => compile_atom e
  end.

Theorem expression_correct : tables_correct -> forall e c,
  compile_expr e = Some c -> forall fuel d g st,
  c fuel d g st = eval fuel P d g st e.
Proof.
  intros Tables. fix IH 1. intros e c Compiled.
  destruct e; try (apply atom_correct; exact Compiled).
  cbn [compile_expr] in Compiled.
  destruct (compile_bindings [b]) as [binding_code|] eqn:Binding;
    destruct (compile_expr e) as [rest|] eqn:Rest; try discriminate.
  injection Compiled as <-. intros [|fuel] d g st; [reflexivity|].
  cbn [eval]. rewrite (bindings_correct Tables [b] binding_code Binding).
  destruct (eval_bindings fuel P d g st [b]) as [[[d1 g1] st1]|rr];
    [apply IH; exact Rest|reflexivity].
Qed.
End Compile.
End Compiler.
