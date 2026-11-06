(* One complete environment transfer carries each formal type exactly once.
   Materialized type entries are lexical temporaries; runtime direct calls
   use theta=[] so no second override path can erase these meanings. *)
From Stdlib Require Import List Arith.
From NM Require Import Audit CoreMoss Reify.
Import ListNotations.

(* Source argument keys name mutable aliases; role keys name immutable
   canonical generations. Solve the former's type patterns before transferring
   the latter. In particular a current name T=New must not replace a frozen
   captured old generation T merely because both carry the same numeric ID. *)
Definition add_generation_solution (generation : tsym) (meaning : ty)
    (solutions : subst) : option subst :=
  match lookup_subst generation solutions with
  | Some previous => if ty_eqb previous meaning then Some solutions else None
  | None => Some ((generation,meaning)::solutions)
  end.

Fixpoint match_generation_pattern (variables : list tsym)
    (pattern actual : ty) (solutions : subst) {struct pattern} : option subst :=
  match pattern,actual with
  | TAbs t,_ => if existsb (Nat.eqb t) variables
      then add_generation_solution t actual solutions
      else if ty_eqb pattern actual then Some solutions else None
  | TUnit,TUnit => Some solutions
  | TNom n ps,TNom m xs =>
      if Nat.eqb n m then
        (fix go (ps xs : subst) (acc : subst) : option subst :=
          match ps,xs with
          | [],[] => Some acc
          | (p,t)::pr,(x,u)::xr => if Nat.eqb p x then
              match match_generation_pattern variables t u acc with
              | Some next => go pr xr next | None => None end else None
          | _,_ => None end) ps xs solutions
      else None
  | TUnion ps,TUnion xs =>
      (fix go (ps xs : list ty) (acc : subst) : option subst :=
        match ps,xs with
        | [],[] => Some acc
        | p::pr,x::xr => match match_generation_pattern variables p x acc with
            | Some next => go pr xr next | None => None end
        | _,_ => None end) ps xs solutions
  | _,_ => None
  end.

Fixpoint solve_generation_arguments (variables : list tsym)
    (patterns arguments : subst) (runtime : reify_state) (solutions : subst)
    : option subst :=
  match patterns with
  | [] => Some solutions
  | (name,pattern)::rest =>
      match lookup_subst name arguments with
      | None => solve_generation_arguments variables rest arguments runtime solutions
      | Some raw =>
          match match_generation_pattern variables pattern
              (app_subst (rf_generations runtime) raw) solutions with
          | Some next => solve_generation_arguments variables rest arguments runtime next
          | None => None end
      end
  end.

Fixpoint resolve_role_arguments (roles : subst) (arguments : subst)
    (runtime : reify_state) : option subst :=
  match roles with
  | [] => Some []
  | (generation,TAbs formal) :: rest =>
      let meaning := match lookup_subst generation arguments with
        | Some raw => Some (app_subst (rf_generations runtime) raw)
        | None => lookup_subst generation (rf_generations runtime) end in
      match meaning,resolve_role_arguments rest arguments runtime with
      | Some tau,Some tail => Some ((formal,tau) :: tail)
      | _,_ => None end
  | _ :: _ => None
  end.

Fixpoint resolve_item_transfers (formals : smap) (source_map : smap)
    (runtime : reify_state) : option smap :=
  match formals with
  | [] => Some []
  | (source,formal) :: rest =>
      match lookup_smap source source_map with
      | None => None
      | Some supplied => match lookup_smap supplied (rf_items runtime),
          resolve_item_transfers rest source_map runtime with
        | Some actual,Some tail => Some ((formal,actual) :: tail)
        | _,_ => None end
      end
  end.

Definition prepare_complete_transfer (roles : subst) (formals source_map : smap)
    (arguments : subst) (before : reify_state)
    : option (list (tsym * ty) * smap * reify_state) :=
  match resolve_role_arguments roles arguments before,
        resolve_item_transfers formals source_map before with
  | Some type_arguments,Some item_map =>
      let '(bindings,type_map,after) := materialize_type_arguments type_arguments before in
      Some (bindings,type_map ++ item_map,after)
  | _,_ => None end.

Fixpoint resolve_role_solutions (roles solutions : subst) (runtime : reify_state)
    : option subst :=
  match roles with
  | [] => Some []
  | (generation,TAbs formal)::rest =>
      let meaning := match lookup_subst generation solutions with
        | Some tau => Some tau
        | None => lookup_subst generation (rf_generations runtime) end in
      match meaning,resolve_role_solutions rest solutions runtime with
      | Some tau,Some tail => Some ((formal,tau)::tail)
      | _,_ => None end
  | _::_ => None
  end.

(* A canonical source name can be closed by its declaration's equations.
   Raw call theta still contains fallback entries for absent names; those
   rows have no immutable generation to infer.  Source typing, rather than
   this inference pass, validates the declaration's closed equations. *)
Definition generation_inference_patterns (variables : list tsym) (patterns : subst) : subst :=
  filter (fun p => existsb (fun t => existsb (Nat.eqb t) variables)
    (ty_frees (snd p))) patterns.

Definition prepare_pattern_transfer (patterns roles : subst)
    (formals source_map : smap) (arguments : subst) (before : reify_state)
    : option (list (tsym * ty) * smap * reify_state) :=
  match solve_generation_arguments (map fst roles)
      (generation_inference_patterns (map fst roles) patterns) arguments before [] with
  | None => None
  | Some solutions =>
      match resolve_role_solutions roles solutions before,
            resolve_item_transfers formals source_map before with
      | Some type_arguments,Some item_map =>
          let '(bindings,type_map,after) := materialize_type_arguments type_arguments before in
          Some (bindings,type_map ++ item_map,after)
      | _,_ => None end
  end.

Lemma prepare_pattern_transfer_preserves_scope : forall patterns roles formals source_map arguments before bindings sm after,
    prepare_pattern_transfer patterns roles formals source_map arguments before = Some (bindings,sm,after) ->
    rf_names after = rf_names before /\ rf_generations after = rf_generations before /\
    rf_items after = rf_items before.
Proof.
  intros patterns roles formals source_map arguments before bindings sm after H.
  unfold prepare_pattern_transfer in H.
  destruct (solve_generation_arguments _ _ _ _ _) as [solutions|]; [|discriminate].
  destruct (resolve_role_solutions _ _ _) as [args|]; [|discriminate].
  destruct (resolve_item_transfers _ _ _) as [items|]; [|discriminate].
  destruct (materialize_type_arguments args before) as [[prefix type_map] state] eqn:E.
  inversion H; subst. exact (materialize_type_arguments_keeps_scope _ _ _ _ _ E).
Qed.

Example frozen_generation_ignores_current_name_argument :
    let runtime := ReifyState 100 [] [(1,TAbs 50)] [] [] in
    let patterns := [(1,TNom 7 [])] in
    solve_generation_arguments [1] patterns [(1,TNom 7 [])] runtime [] = Some [] /\
    resolve_role_solutions [(1,TAbs 80)] [] runtime = Some [(80,TAbs 50)].
Proof. split; reflexivity. Qed.

Example generic_generation_uses_actual_name_argument :
    solve_generation_arguments [1] [(1,TAbs 1)] [(1,TNom 7 [])]
      (ReifyState 100 [] [(1,TAbs 50)] [] []) [] = Some [(1,TNom 7 [])].
Proof. reflexivity. Qed.

Example composite_generation_argument :
    solve_generation_arguments [2] [(1,TNom 7 [(3,TAbs 2)])]
      [(1,TNom 7 [(3,TNom 9 [])])] (ReifyState 100 [] [] [] []) [] =
      Some [(2,TNom 9 [])].
Proof. reflexivity. Qed.

Lemma prepare_complete_transfer_preserves_scope : forall roles formals source_map arguments before bindings sm after,
    prepare_complete_transfer roles formals source_map arguments before = Some (bindings,sm,after) ->
    rf_names after = rf_names before /\ rf_generations after = rf_generations before /\
    rf_items after = rf_items before.
Proof.
  intros roles formals source_map arguments before bindings sm after H.
  unfold prepare_complete_transfer in H.
  destruct (resolve_role_arguments roles arguments before) as [args|]; [|discriminate].
  destruct (resolve_item_transfers formals source_map before) as [items|]; [|discriminate].
  destruct (materialize_type_arguments args before) as [[prefix type_map] state] eqn:E.
  inversion H; subst. exact (materialize_type_arguments_keeps_scope _ _ _ _ _ E).
Qed.

Lemma resolve_item_transfers_domain : forall formals source_map runtime sm,
    resolve_item_transfers formals source_map runtime = Some sm ->
    map fst sm = map snd formals.
Proof.
  induction formals as [|[source formal] rest IH]; intros source_map runtime sm H; cbn in H.
  - inversion H; reflexivity.
  - destruct (lookup_smap source source_map); [|discriminate].
    destruct (lookup_smap i (rf_items runtime)); [|discriminate].
    destruct (resolve_item_transfers rest source_map runtime) eqn:E; [|discriminate].
    inversion H; subst. cbn. f_equal. eapply IH. exact E.
Qed.

Definition lower_direct_call_transfer (code : fsym) (roles : subst)
    (formals source_map : smap) (arguments : subst) (args : list xexpr)
    (before : reify_state) : option (xexpr * reify_state) :=
  match prepare_complete_transfer roles formals source_map arguments before with
  | Some (bindings,sm,after) =>
      Some (bind_type_temporaries bindings (XCallD code [] sm args),after)
  | None => None end.

Definition lower_function_capture_transfer (name code : fsym) (roles : subst)
    (formals source_map : smap) (arguments : subst) (body : xexpr)
    (before : reify_state) : option (xexpr * reify_state) :=
  match prepare_complete_transfer roles formals source_map arguments before with
  | Some (bindings,sm,after) =>
      Some (bind_type_temporaries bindings (XBindFn name code sm body),after)
  | None => None end.

Definition lower_method_capture_transfer (receiver : ty) (method : msym) (theta : subst)
    (code : fsym) (roles : subst) (formals source_map : smap) (arguments : subst)
    (body : xexpr) (before : reify_state) : option (xexpr * reify_state) :=
  match prepare_complete_transfer roles formals source_map arguments before with
  | Some (bindings,sm,after) =>
      Some (bind_type_temporaries bindings (XBindMth receiver method theta code sm body),after)
  | None => None end.

Lemma lower_direct_call_has_no_theta_override : forall code roles formals source_map arguments args before e after,
    lower_direct_call_transfer code roles formals source_map arguments args before = Some (e,after) ->
    exists bindings sm, e = bind_type_temporaries bindings (XCallD code [] sm args).
Proof.
  intros. unfold lower_direct_call_transfer in H.
  destruct (prepare_complete_transfer roles formals source_map arguments before)
    as [[[bindings sm] state]|]; [|discriminate].
  inversion H; subst. eauto.
Qed.

Lemma resolve_item_transfers_total : forall formals source_map runtime,
  (forall source formal,In(source,formal)formals->exists supplied actual,
    lookup_smap source source_map=Some supplied /\
    lookup_smap supplied(rf_items runtime)=Some actual)->
  exists result,resolve_item_transfers formals source_map runtime=Some result.
Proof.
 induction formals as[|[source formal]rest IH];intros source_map runtime Available.
 - exists[];reflexivity.
 - destruct(Available source formal(or_introl eq_refl))as[supplied[actual[Hs Ha]]].
   destruct(IH source_map runtime)as[result Hresult].
   { intros s f Hin;exact(Available s f(or_intror Hin)). }
   exists((formal,actual)::result);cbn;rewrite Hs,Ha,Hresult;reflexivity.
Qed.

Lemma resolve_item_transfers_relational : forall (R:item->item->Prop) formals source_map runtime result,
  resolve_item_transfers formals source_map runtime=Some result->
  (forall source formal supplied actual,In(source,formal)formals->
    lookup_smap source source_map=Some supplied->
    lookup_smap supplied(rf_items runtime)=Some actual->R formal actual)->
  Forall(fun p=>R(fst p)(snd p))result.
Proof.
 intros R formals;induction formals as[|[source formal]rest IH];
   intros source_map runtime result H Related;cbn in H.
 - inversion H;constructor.
 - destruct(lookup_smap source source_map)as[supplied|]eqn:Hs;[|discriminate].
   destruct(lookup_smap supplied(rf_items runtime))as[actual|]eqn:Ha;[|discriminate].
   destruct(resolve_item_transfers rest source_map runtime)as[tail|]eqn:Ht;[|discriminate].
   inversion H;subst;constructor.
   + eapply Related;[left;reflexivity|exact Hs|exact Ha].
   + eapply IH;[exact Ht|]. intros s f a b Hin Hsa Hab.
     eapply Related;[right;exact Hin|exact Hsa|exact Hab].
Qed.
