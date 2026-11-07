(* Closed-program support: source elaboration checks every defined function,
   but immutable runtime declarations need only executable code reachable
   from main and the actual platform environment. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit.
Import ListNotations.

Inductive reify_node : Type := RnFunction (f : fsym) | RnFunctor (h : usym).
Definition reify_node_eqb (a b : reify_node) : bool :=
  match a,b with
  | RnFunction f,RnFunction g | RnFunctor f,RnFunctor g => Nat.eqb f g
  | _,_ => false end.
Definition reify_node_mem (n : reify_node) (nodes : list reify_node) : bool :=
  existsb (reify_node_eqb n) nodes.

Fixpoint expr_code_dependencies (e : xexpr) : list reify_node :=
  match e with
  | XVar _ | XUnit | XVl _ | XBreak => []
  | XTag _ _ b | XBindTy _ _ b | XAssign _ b | XLoop b | XReturn b | XField b _
  | XBindFunCopy _ _ b | XBindPrim _ _ b | XConst _ b => expr_code_dependencies b
  | XCallD code _ _ args => RnFunction code :: flat_map expr_code_dependencies args
  | XCallA _ args => flat_map expr_code_dependencies args
  | XMeth receiver _ _ args => expr_code_dependencies receiver ++ flat_map expr_code_dependencies args
  | XMatch scrutinee arms => expr_code_dependencies scrutinee ++
      flat_map (fun a => match a with (_,_,b) => expr_code_dependencies b end) arms
  | XLet _ a b | XBindVal _ a b | XSeq a b | XWhile a b =>
      expr_code_dependencies a ++ expr_code_dependencies b
  | XBindFn _ code _ b | XBindMth _ _ _ code _ b => RnFunction code :: expr_code_dependencies b
  | XBindFun _ h _ _ b | XBindFunSym _ h b => RnFunctor h :: expr_code_dependencies b
  | XBindFunA _ _ _ b | XApply _ _ _ b | XApplyR _ _ _ b => expr_code_dependencies b
  | XIf a b c => expr_code_dependencies a ++ expr_code_dependencies b ++ expr_code_dependencies c
  | XRec fields => flat_map (fun p => expr_code_dependencies (snd p)) fields
  | XMatchP scrutinee arms => expr_code_dependencies scrutinee ++
      flat_map (fun p => expr_code_dependencies (snd p)) arms
  | XApplyBr _ branches b => expr_code_dependencies b ++
      flat_map (fun branch => match branch with (h,bindings,_) =>
        RnFunctor h :: flat_map bind_code_dependencies bindings end) branches
  end
with bind_code_dependencies (b : xbind) : list reify_node :=
  match b with
  | XBTy _ _ | XBPrim _ _ | XBFunA _ _ _ | XBFunCopy _ _ | XBApply _ _ _ | XBApplyR _ _ _ => []
  | XBVal _ e => expr_code_dependencies e
  | XBFn _ code _ | XBMth _ _ _ code _ => [RnFunction code]
  | XBFun _ h _ _ | XBFunSym _ h => [RnFunctor h]
  | XBApplyBr _ branches => flat_map (fun branch => match branch with (h,bindings,_) =>
      RnFunctor h :: flat_map bind_code_dependencies bindings end) branches
  end.

Fixpoint entry_code_dependencies (entry : xdentry) : list reify_node :=
  match entry with
  | XDClo code captured => RnFunction code ::
      flat_map (fun p => entry_code_dependencies (snd p)) captured
  | XDFun h _ captured => RnFunctor h ::
      flat_map (fun p => entry_code_dependencies (snd p)) captured
  | XDFunSym h => [RnFunctor h]
  | _ => [] end.

Definition declared_reify_nodes (P : xprogram) : list reify_node :=
  map (fun p => RnFunction (fst p)) (xo_fns P) ++
  map (fun p => RnFunctor (fst p)) (xo_funs P).
Fixpoint reify_lookup_function (f : fsym) (functions : list (fsym * xexpr)) : option xexpr :=
  match functions with
  | [] => None
  | (g,body) :: rest => if Nat.eqb f g then Some body else reify_lookup_function f rest end.
Fixpoint reify_lookup_functor (h : usym) (functors : list (usym * (tele * list xbind)))
    : option (tele * list xbind) :=
  match functors with
  | [] => None
  | (g,body) :: rest => if Nat.eqb h g then Some body else reify_lookup_functor h rest end.
Definition reify_node_dependencies (P : xprogram) (node : reify_node) : list reify_node :=
  match node with
  | RnFunction f => match reify_lookup_function f (xo_fns P) with
      | Some e => expr_code_dependencies e | None => [] end
  | RnFunctor h => match reify_lookup_functor h (xo_funs P) with
      | Some (_,bindings) => flat_map bind_code_dependencies bindings | None => [] end
  end.
Definition reify_reach_step (P : xprogram) (seen : list reify_node) : list reify_node :=
  let requested := seen ++ flat_map (reify_node_dependencies P) seen in
  filter (fun node => reify_node_mem node requested) (declared_reify_nodes P).
Fixpoint reify_reach_rounds (fuel : nat) (P : xprogram) (seen : list reify_node) : list reify_node :=
  match fuel with
  | 0 => seen
  | S f => reify_reach_rounds f P (reify_reach_step P seen)
  end.
Definition reify_reachable_nodes (P : xprogram) : list reify_node :=
  reify_reach_rounds (S (length (declared_reify_nodes P))) P
    (RnFunction (xo_main P) :: flat_map (fun p => entry_code_dependencies (snd p)) (xo_env P)).

Lemma reify_reach_step_declared : forall P seen node,
    In node (reify_reach_step P seen) -> In node (declared_reify_nodes P).
Proof. intros P seen node H. apply filter_In in H. exact (proj1 H). Qed.
Lemma reify_reach_rounds_declared : forall fuel P seen,
    (forall node, In node seen -> In node (declared_reify_nodes P)) ->
    forall node, In node (reify_reach_rounds fuel P seen) -> In node (declared_reify_nodes P).
Proof.
  induction fuel; intros P seen Hseen node H; [exact (Hseen node H)|].
  apply (IHfuel P (reify_reach_step P seen)); [|exact H].
  intros n Hn. exact (reify_reach_step_declared P seen n Hn).
Qed.
Lemma reify_reachable_declared : forall P node,
    In node (reify_reachable_nodes P) -> In node (declared_reify_nodes P).
Proof.
  intros P node H. unfold reify_reachable_nodes in H. cbn [reify_reach_rounds] in H.
  eapply reify_reach_rounds_declared; [|exact H].
  intros n Hn. eapply reify_reach_step_declared. exact Hn.
Qed.

Definition reify_reach_closed (P : xprogram) (nodes : list reify_node) : bool :=
  reify_node_mem (RnFunction (xo_main P)) nodes &&
  forallb (fun node => reify_node_mem node nodes)
    (flat_map (fun p => entry_code_dependencies (snd p)) (xo_env P)) &&
  forallb (fun node => forallb (fun dependency => reify_node_mem dependency nodes)
    (reify_node_dependencies P node)) nodes.

Example reify_reach_omits_unused_code :
  let P := XProgram [(10,XUnit);(11,XApply 99 [] [] XUnit)] []
    0 0 0 10 [] [] [] [] [] false [] [] in
  reify_reachable_nodes P = [RnFunction 10] /\
    reify_reach_closed P (reify_reachable_nodes P) = true.
Proof. split; reflexivity. Qed.

Example reify_reach_follows_functor_closure :
  let P := XProgram [(10,XBindFun 20 30 [] [] XUnit);(40,XUnit)]
    [(30,([], [XBFn 21 40 []]))] 0 0 0 10 [] [] [] [] [] false [] [] in
  reify_reachable_nodes P = [RnFunction 10;RnFunction 40;RnFunctor 30] /\
    reify_reach_closed P (reify_reachable_nodes P) = true.
Proof. split; reflexivity. Qed.
