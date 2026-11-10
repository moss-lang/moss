(* ------------------------------------------------------------------------ *)
(* InterpreterCompiled.v: InterpreterFast's interpreter, compiled to closures.              *)
(*                                                                           *)
(* InterpreterFast.feval dispatches on the expression at every step. Here each   *)
(* expression is compiled once, before the run, into an OCaml closure      *)
(* (`code`) that runs it: the children's code is built first and captured, *)
(* so the dispatch happens at compile time. A fn body's code is found      *)
(* through `tbl` and a functor body's through `ftbl`, which the driver     *)
(* fills with the compiled bodies (closures that read the very tables they *)
(* are stored in); `tbl_ok` states what they must hold.                    *)
(*                                                                           *)
(* Proved, for every primitive implementation, every fuel and environment: *)
(*  - cc_agrees: under tbl_ok, the code of e returns exactly what           *)
(*    InterpreterFast.feval returns on e (likewise lists, bindings, applications);*)
(*  - crun_main_agrees: crun_main is InterpreterFast.frun_main, so (by            *)
(*    InterpreterFast.frun_main_agrees) Interpreter.run_main.                     *)
(*                                                                           *)
(* run/Extract.v extracts this file with the extraction's term             *)
(* optimizations off, which keeps each child's code built once, where it   *)
(* is bound, rather than at every evaluation.                              *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith String Lia.
From NM Require Import Audit NatTrie InterpreterLazy InterpreterFast.
Import Audit.Semantics ListNotations.
From NM Require ProgramPrune.

Local Open Scope nat_scope.

(* The scalar compiler uses these outside the interpreter functor so that
   extraction can inline them and return closures at their runtime arity. *)
Definition scalar_nil {D S : Type} : D -> locals -> S -> option (list value * S) :=
  fun _ _ st => Some ([], st).
Definition scalar_cons {D S : Type}
    (c : D -> locals -> S -> option (value * S))
    (rest : D -> locals -> S -> option (list value * S)) :
    D -> locals -> S -> option (list value * S) :=
  fun d g st =>
    match c d g st with
    | Some (w, st1) =>
        match rest d g st1 with
        | Some (ws, st2) => Some (w :: ws, st2)
        | None => None
        end
    | None => None
    end.

Definition push_arg {D S R : Type}
    (rest : nat -> D -> locals -> S -> (list value * locals * S) + R)
    (w : value) (m : nat) (d : D) (g : locals) (st : S) :=
  match rest m d g st with
  | inl (ws, g', st') => inl (w :: ws, g', st')
  | inr result => inr result
  end.

Module CloInterpreter (Import Prims : PRIMITIVES).
Module F := FastInterpreter Prims.
Module L := F.L.
Module I := F.I.

Definition code := nat -> fenv -> locals -> mstate -> I.evaluation.
Definition lcode := nat -> fenv -> locals -> mstate -> (list value * locals * mstate) + I.evaluation.
Definition bcode := nat -> fenv -> locals -> mstate -> (fenv * locals * mstate) + I.evaluation.
Definition acode := nat -> fenv -> mstate -> (fenv * mstate) + I.evaluation.

Section CC.
Variable P : program_view.
Variable tbl : fsym -> option code.
Variable ftbl : usym -> option bcode.

Definition lnil : lcode := fun m d g st =>
  match m with 0 => inr (I.Evaluation OutOfFuel g st) | S _ => inl ([], g, st) end.

(* Choose the immediate-argument cases before creating the runtime closure.
   The inner fuel is the one the argument and the rest both receive. *)
Fixpoint cc_list (f : expr -> code) (l : list expr) : lcode :=
  match l with
  | [] => lnil
  | e :: rest =>
      let cr := cc_list f rest in
      match e with
      | EVar x => fun m d g st =>
          match m with
          | S (S _ as k) =>
              match lookup_local x g with
              | Some w => push_arg cr w k d g st
              | None => inr (I.Evaluation (Stuck I.e_var) g st)
              end
          | _ => inr (I.Evaluation OutOfFuel g st)
          end
      | EVal v => fun m d g st =>
          match m with
          | S (S _ as k) =>
              match flookup_provision (IVal v) d with
              | Some (FValue w) => push_arg cr w k d g st
              | _ => inr (I.Evaluation (Stuck I.e_val) g st)
              end
          | _ => inr (I.Evaluation OutOfFuel g st)
          end
      | EUnit => fun m d g st =>
          match m with
          | S (S _ as k) => push_arg cr VUnit k d g st
          | _ => inr (I.Evaluation OutOfFuel g st)
          end
      | _ =>
          let c := f e in
          fun m d g st =>
            match m with
            | 0 => inr (I.Evaluation OutOfFuel g st)
            | S k =>
                match c k d g st with
                | I.Evaluation (Normal w) g1 st1 => push_arg cr w k d g1 st1
                | rr => inr rr
                end
            end
      end
  end.
Fixpoint cc_arms (f : expr -> code) (arms : list (pattern * expr)) : list (pattern * code) :=
  match arms with [] => [] | (p, e) :: r => (p, f e) :: cc_arms f r end.

Fixpoint select_carm (arms : list (pattern * code)) (w : value) : option (locals * code) :=
  match arms with
  | [] => None
  | (p, c) :: r =>
      match I.match_pat p w with
      | I.PMatch bs => Some (bs, c)
      | I.PNo => select_carm r w
      | I.PBad => None
      end
  end.

Definition bnil : bcode := fun m d g st =>
  match m with 0 => inr (I.Evaluation OutOfFuel g st) | S _ => inl (d, g, st) end.

(* fapply_functor, the body's bindings run by their code in ftbl. *)
Definition capply (u : usym) (th : subst) (outputs : list (tsym * tsym)) (sm : smap) (rs : list item) : acode :=
  fun m d st =>
  match m with
  | 0 => inr (I.Evaluation OutOfFuel [] st)
  | S k =>
      match flookup_provision (IFun u) d with
      | Some (FFunctor h a dc p) =>
          match view_functor P h, ftbl h with
          | Some code, Some cb =>
              let Dr := fc_outputs code in
              match fselect_provisions d sm with
              | Some d1 =>
                  match fadapt_provisions d1 a with
                  | Some d2 =>
                      match cb k (fapplication_environment P d h th (fappend d2 dc)) [] st with
                      | inl (d3, _, st3) =>
                          match fselect_provisions d3 (result_selection rs p (fc_sources code)) with
                          | Some dr => match fresult_environment outputs d3 Dr dr with
                              | Some result => inl (result, st3)
                              | None => inr (I.Evaluation (Stuck I.e_compose) [] st3) end
                          | None => inr (I.Evaluation (Stuck I.e_compose) [] st3)
                          end
                      | inr rr => inr rr
                      end
                  | None => inr (I.Evaluation (Stuck I.e_chain) [] st)
                  end
              | None => inr (I.Evaluation (Stuck I.e_compose) [] st)
              end
          | _, _ => inr (I.Evaluation (Stuck I.e_nobody) [] st)
          end
      | _ => inr (I.Evaluation (Stuck I.e_fun) [] st)
      end
  end.

(* One binding's step, before the rest's code `r`; `f` compiles a value's
   initializer. *)
Definition cc_bind (f : expr -> code) (b : binding) (r : bcode) : bcode :=
  match b with
  | BType t tau => fun m d g st =>
      match m with 0 => inr (I.Evaluation OutOfFuel g st) | S k => r k (fbind_type d t tau) g st end
  | BValue v e1 => let c1 := f e1 in fun m d g st =>
      match m with
      | 0 => inr (I.Evaluation OutOfFuel g st)
      | S k =>
          match c1 k d g st with
          | I.Evaluation (Normal w1) g1 st1 => r k (finstall (IVal v) (FValue w1) d) g1 st1
          | rr => inr rr
          end
      end
  | BFunction fn f' types sm => fun m d g st =>
      match m with
      | 0 => inr (I.Evaluation OutOfFuel g st)
      | S k =>
          match fselect_provisions d sm with
          | Some dc => r k (finstall (IFn fn) (FFunction f' (ftyped_capture d types dc)) d) g st
          | None => inr (I.Evaluation (Stuck I.e_compose) g st)
          end
      end
  | BMethod tauk mm th f' types sm => fun m d g st =>
      match m with
      | 0 => inr (I.Evaluation OutOfFuel g st)
      | S k =>
          match fselect_provisions d sm with
          | Some dc => r k (finstall (IMth tauk mm th) (FFunction f' (ftyped_capture d types dc)) d) g st
          | None => inr (I.Evaluation (Stuck I.e_compose) g st)
          end
      end
  | BFunctor u h types sm smc => fun m d g st =>
      match m with
      | 0 => inr (I.Evaluation OutOfFuel g st)
      | S k =>
          match fselect_provisions d smc with
          | Some dc => r k (finstall (IFun u) (FFunctor h (Some sm) (ftyped_capture d types dc) None) d) g st
          | None => inr (I.Evaluation (Stuck I.e_compose) g st)
          end
      end
  | BCopyFunctor u u' sm rm => fun m d g st =>
      match m with
      | 0 => inr (I.Evaluation OutOfFuel g st)
      | S k =>
          match flookup_provision (IFun u') d with
          | Some (FFunctor h a dc p) =>
              r k (finstall (IFun u) (FFunctor h (compose_adaptation a sm) dc (compose_positions p rm)) d) g st
          | _ => inr (I.Evaluation (Stuck I.e_fun) g st)
          end
      end
  | BApply u th outputs sm rs => let ca := capply u th outputs sm rs in fun m d g st =>
      match m with
      | 0 => inr (I.Evaluation OutOfFuel g st)
      | S k =>
          match ca k d st with
          | inl (dr, st1) => r k (fappend dr d) g st1
          | inr rr => inr (I.with_g g rr)
          end
      end
  | BPrimitive i p => fun m d g st =>
      match m with
      | 0 => inr (I.Evaluation OutOfFuel g st)
      | S k => r k (finstall_primitive d i p) g st
      end
  end.

(* ===================== Folding small direct calls (LazyFold) ============ *)

(* A direct call to a small fn is translated, to a bounded depth, into an
   `sexpr` over the caller's environment, every provision lookup remapped
   through the satisfaction maps into the caller's (a `plan`). Run on the
   caller's environment, it gives the call's result directly, when it
   gives one; otherwise, or when the fuel may not suffice, the call runs as
   compiled. fold_sound: the folded result is the call's result. *)
Definition lk := option item.
Definition plan := list (item * lk).
Definition read (d : fenv) (i : lk) : option fprov :=
  match i with Some key => flookup_provision key d | None => None end.
Fixpoint remap (pl : plan) (i : item) : lk :=
  match pl with [] => None | (req, src) :: rest => if item_eqb i req then src else remap rest i end.
Definition remapo (pl : option plan) (i : item) : lk :=
  match pl with None => Some i | Some ps => remap ps i end.
Definition remap_plan (pl : option plan) (sm : smap) : plan := map (fun p => (fst p, remapo pl (snd p))) sm.
Definition checks (d : fenv) (pl : plan) : bool :=
  forallb (fun p => match read d (snd p) with Some _ => true | None => false end) pl.

Inductive sexpr :=
| SVar (x : var) | SUnit | SVl (i : lk)
| STag (n : nsym) (body : sexpr)
| SPrim (i : lk) (args : list sexpr)
| SCall (guard : plan) (args : list sexpr) (body : sexpr)
| SMatch1 (scrutinee : sexpr) (pat : pattern) (body : sexpr).

(* m bounds the translation's structural recursion; n bounds call unfolding. *)
Fixpoint tos (m n : nat) (pl : option plan) (e : expr) {struct m} : option sexpr :=
  match m with
  | 0 => None
  | S m' =>
      match e with
      | EVar x => Some (SVar x)
      | EUnit => Some SUnit
      | EVal v => Some (SVl (remapo pl (IVal v)))
      | ETag t th e1 =>
          match lookup_cast t (view_casts P) with
          | Some _ => None
          | None => option_map (STag t) (tos m' n pl e1)
          end
      | EInvoke f args => option_map (SPrim (remapo pl (IFn f))) (tosl m' n pl args)
      | ECall f th sm args =>
          match n with
          | 0 => None
          | S n' =>
              match view_function P f with
              | None => None
              | Some body =>
                  let guard := remap_plan pl sm in
                  match tosl m' n pl args, tos m' n' (Some guard) body with
                  | Some ss, Some sb => Some (SCall guard ss sb)
                  | _, _ => None
                  end
              end
          end
      | EMatch e0 [(p, b)] =>
          match tos m' n pl e0, tos m' n pl b with
          | Some s, Some sb => Some (SMatch1 s p sb)
          | _, _ => None
          end
      | _ => None
      end
  end
with tosl (m n : nat) (pl : option plan) (es : list expr) {struct m} : option (list sexpr) :=
  match m with
  | 0 => None
  | S m' =>
      match es with
      | [] => Some []
      | e :: rest =>
          match tos m' n pl e, tosl m' n pl rest with
          | Some s, Some ss => Some (s :: ss)
          | _, _ => None
          end
      end
  end.

(* Fuel the original expression needs, its calls unfolded. *)
Fixpoint sdepth (s : sexpr) : nat :=
  match s with
  | SVar _ | SUnit | SVl _ => 1
  | STag _ s => S (sdepth s)
  | SPrim _ ss => S ((fix needed ss := match ss with [] => 1 | s :: ss => S (Nat.max (sdepth s) (needed ss)) end) ss)
  | SCall _ ss body =>
      S (Nat.max ((fix needed ss := match ss with [] => 1 | s :: ss => S (Nat.max (sdepth s) (needed ss)) end) ss)
           (sdepth body))
  | SMatch1 e _ b => S (Nat.max (sdepth e) (sdepth b))
  end.
Fixpoint needed (ss : list sexpr) : nat :=
  match ss with [] => 1 | s :: ss => S (Nat.max (sdepth s) (needed ss)) end.

Fixpoint sev (d : fenv) (g : locals) (st : mstate) (s : sexpr) {struct s} : option (value * mstate) :=
  match s with
  | SVar x => option_map (fun w => (w, st)) (lookup_local x g)
  | SUnit => Some (VUnit, st)
  | SVl i => match read d i with Some (FValue w) => Some (w, st) | _ => None end
  | STag n e => option_map (fun p => (VTag n (fst p), snd p)) (sev d g st e)
  | SPrim i args =>
      let ev := (fix ev args st := match args with
                 | [] => Some ([], st)
                 | s :: rest => match sev d g st s with
                     | Some (w, st1) => match ev rest st1 with Some (ws, st2) => Some (w :: ws, st2) | None => None end
                     | None => None end end) args st in
      match read d i with
      | Some (FPrimitive p held) =>
          match ev with
          | Some (ws, st1) => match I.run_prim_at P p held ws st1 with (Normal w, st2) => Some (w, st2) | _ => None end
          | None => None
          end
      | _ => None
      end
  | SCall guard args body =>
      if checks d guard then
        match (fix ev args st := match args with
               | [] => Some ([], st)
               | s :: rest => match sev d g st s with
                   | Some (w, st1) => match ev rest st1 with Some (ws, st2) => Some (w :: ws, st2) | None => None end
                   | None => None end end) args st with
        | Some (ws, st1) => sev d (fast_parameter_values ws) st1 body
        | None => None
        end
      else None
  | SMatch1 e pat b =>
      match sev d g st e with
      | Some (w, st1) => match I.match_pat pat w with I.PMatch bs => sev d (bs ++ g) st1 b | _ => None end
      | None => None
      end
  end.
Definition sevl (d : fenv) (g : locals) : list sexpr -> mstate -> option (list value * mstate) :=
  fix ev ss st := match ss with
  | [] => Some ([], st)
  | s :: rest => match sev d g st s with
      | Some (w, st1) => match ev rest st1 with Some (ws, st2) => Some (w :: ws, st2) | None => None end
      | None => None end end.

(* Gather each distinct key once per folded call, including guards for
   entries the scalar body never reads. The compiled code then reads slots. *)
Definition lk_eqb (a b : lk) : bool :=
  match a, b with
  | None, None => true
  | Some i, Some j => fast_item_eqb i j
  | _, _ => false
  end.
Fixpoint scalar_reads (s : sexpr) : list lk :=
  match s with
  | SVar _ | SUnit => []
  | SVl i => [i]
  | STag _ e => scalar_reads e
  | SPrim i args => i :: flat_map scalar_reads args
  | SCall guard args body => map snd guard ++ flat_map scalar_reads args ++ scalar_reads body
  | SMatch1 e _ body => scalar_reads e ++ scalar_reads body
  end.
Definition scalar_keys (s : sexpr) : list lk :=
  fold_right (fun i rest => if existsb (lk_eqb i) rest then rest else i :: rest) [] (scalar_reads s).
Fixpoint key_pos (i : lk) (keys : list lk) : option nat :=
  match keys with
  | [] => None
  | j :: rest => if lk_eqb i j then Some 0 else option_map S (key_pos i rest)
  end.
Definition slot_lookup (keys : list lk) (i : lk) : list fprov -> option fprov :=
  match key_pos i keys with Some n => fun slots => nth_error slots n | None => fun _ => None end.
Fixpoint gather (d : fenv) (keys : list lk) : option (list fprov) :=
  match keys with
  | [] => Some []
  | i :: rest =>
      match read d i with
      | Some entry => option_map (cons entry) (gather d rest)
      | None => None
      end
  end.

Definition scalar_code := list fprov -> locals -> mstate -> option (value * mstate).

Fixpoint scalar_compile (keys : list lk) (s : sexpr) {struct s} : scalar_code :=
  match s with
  | SVar x => fun _ g st => option_map (fun w => (w, st)) (lookup_local x g)
  | SUnit => fun _ _ st => Some (VUnit, st)
  | SVl i =>
      let get := slot_lookup keys i in
      fun slots _ st => match get slots with Some (FValue w) => Some (w, st) | _ => None end
  | STag n e =>
      let c := scalar_compile keys e in
      fun slots g st => option_map (fun p => (VTag n (fst p), snd p)) (c slots g st)
  | SPrim i args =>
      let get := slot_lookup keys i in
      let ca := fold_right (fun e cr => let c := scalar_compile keys e in scalar_cons c cr) scalar_nil args in
      fun slots g st =>
        match get slots with
        | Some (FPrimitive p held) =>
            match ca slots g st with
            | Some (ws, st1) =>
                match I.run_prim_at P p held ws st1 with (Normal w, st2) => Some (w, st2) | _ => None end
            | None => None
            end
        | _ => None
        end
  | SCall _ args body =>
      let ca := fold_right (fun e cr => let c := scalar_compile keys e in scalar_cons c cr) scalar_nil args in
      let cb := scalar_compile keys body in
      fun slots g st =>
        match ca slots g st with
        | Some (ws, st1) => cb slots (fast_parameter_values ws) st1
        | None => None
        end
  | SMatch1 e pat body =>
      let ce := scalar_compile keys e in
      let cb := scalar_compile keys body in
      fun slots g st =>
        match ce slots g st with
        | Some (w, st1) =>
            match I.match_pat pat w with I.PMatch bs => cb slots (bs ++ g) st1 | _ => None end
        | None => None
        end
  end.

(* A call's code, folded where its translation runs. *)
Definition fold_call (e : expr) (fallback : code) : code :=
  match tos 48 6 None e with
  | None => fallback
  | Some s =>
      let depth := sdepth s in
      let keys := scalar_keys s in
      let fast := scalar_compile keys s in
      fun m d g st =>
        if Nat.leb depth m then
          match gather d keys with
          | Some slots =>
              match fast slots g st with
              | Some (w, st1) => I.Evaluation (Normal w) g st1
              | None => fallback m d g st
              end
          | None => fallback m d g st
          end
        else fallback m d g st
  end.

(* The cast table is fixed for the program. Ordinary tags can construct
   their values directly, without reading a runtime type environment. *)
Definition cc_tag (n : nsym) (th : subst) (c : code) : code :=
  match lookup_cast n (view_casts P) with
  | None => fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match c k d g st with
          | I.Evaluation (Normal w) g1 st1 => I.Evaluation (Normal (VTag n w)) g1 st1
          | r => r
          end
      end
  | Some _ => fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match c k d g st with
          | I.Evaluation (Normal w) g1 st1 => I.Evaluation (L.lcheck_tag P (lshadow d) n th w) g1 st1
          | r => r
          end
      end
  end.

Fixpoint cc (e : expr) {struct e} : code :=
  match e with
  | EVar x => fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S _ => match lookup_local x g with
               | Some w => I.Evaluation (Normal w) g st
               | None => I.Evaluation (Stuck I.e_var) g st
               end
      end
  | EUnit => fun m d g st =>
      match m with 0 => I.Evaluation OutOfFuel g st | S _ => I.Evaluation (Normal VUnit) g st end
  | EVal v => fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S _ => match flookup_provision (IVal v) d with
               | Some (FValue w) => I.Evaluation (Normal w) g st
               | _ => I.Evaluation (Stuck I.e_val) g st
               end
      end
  | ETag n th e1 => cc_tag n th (cc e1)
  | ECall f th sm args => let ca := cc_list cc args in fold_call (ECall f th sm args) (fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match tbl f with
          | Some cf =>
              match ca k d g st with
              | inl (ws, g1, st1) =>
                  match fprepare_call d th sm with
                  | Some d' => I.ret_call g1 (cf k d' (fast_parameter_values ws) st1)
                  | None => I.Evaluation (Stuck I.e_compose) g1 st1
                  end
              | inr r => r
              end
          | None => I.Evaluation (Stuck I.e_nobody) g st
          end
      end)
  | EInvoke f args => let ca := cc_list cc args in fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match flookup_provision (IFn f) d with
          | Some (FFunction f' df) =>
              match tbl f' with
              | Some cf =>
                  match ca k d g st with
                  | inl (ws, g1, st1) => I.ret_call g1 (cf k df (fast_parameter_values ws) st1)
                  | inr r => r
                  end
              | None => I.Evaluation (Stuck I.e_nobody) g st
              end
          | Some (FPrimitive p held) =>
              match ca k d g st with
              | inl (ws, g1, st1) =>
                  let (o, st2) := I.run_prim_at P p held ws st1 in I.Evaluation o g1 st2
              | inr r => r
              end
          | _ => I.Evaluation (Stuck I.e_entry) g st
          end
      end
  | EMethod e0 mm prov args => let c0 := cc e0 in let ca := cc_list cc args in fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match flookup_provision prov d with
          | Some (FFunction f' d') =>
              match tbl f' with
              | Some cf =>
                  match c0 k d g st with
                  | I.Evaluation (Normal w0) g0 st0 =>
                      match ca k d g0 st0 with
                      | inl (ws, g1, st1) =>
                          I.ret_call g1 (cf k d' (fast_parameter_values (w0 :: ws)) st1)
                      | inr r => r
                      end
                  | r => r
                  end
              | None => I.Evaluation (Stuck I.e_nobody) g st
              end
          | Some (FPrimitive p held) =>
              match c0 k d g st with
              | I.Evaluation (Normal w0) g0 st0 =>
                  match ca k d g0 st0 with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := I.run_prim_at P p held (w0 :: ws) st1 in I.Evaluation o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | _ => I.Evaluation (Stuck I.e_entry) g st
          end
      end
  | ELet x e1 e2 => let c1 := cc e1 in let c2 := cc e2 in fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match c1 k d g st with
          | I.Evaluation (Normal w1) g1 st1 => I.pop1 (c2 k d ((x, w1) :: g1) st1)
          | r => r
          end
      end
  | EBind b e1 =>
      let c1 := cc e1 in
      match b with
      | BPrimitive i p => fun m d g st =>
          match m with
          (* The singleton binding and the empty tail each need fuel. *)
          | S (S (S _) as k) => c1 k (finstall_primitive d i p) g st
          | _ => I.Evaluation OutOfFuel g st
          end
      | _ =>
          let cb := cc_bind cc b bnil in
          fun m d g st =>
            match m with
            | 0 => I.Evaluation OutOfFuel g st
            | S k =>
                match cb k d g st with
                | inl (d1, g1, st1) => c1 k d1 g1 st1
                | inr result => result
                end
            end
      end
  | ESeq e1 e2 => let c1 := cc e1 in let c2 := cc e2 in fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match c1 k d g st with
          | I.Evaluation (Normal _) g1 st1 => c2 k d g1 st1
          | r => r
          end
      end
  | EAssign x e1 => let c1 := cc e1 in fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match c1 k d g st with
          | I.Evaluation (Normal w) g1 st1 =>
              match assign_local x w g1 with
              | Some g2 => I.Evaluation (Normal VUnit) g2 st1
              | None => I.Evaluation (Stuck I.e_assign) g1 st1
              end
          | r => r
          end
      end
  | EIf c e1 e2 => let cc0 := cc c in let c1 := cc e1 in let c2 := cc e2 in fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match cc0 k d g st with
          | I.Evaluation (Normal w) g1 st1 =>
              match I.truthy P w with
              | Some true => c1 k d g1 st1
              | Some false => c2 k d g1 st1
              | None => I.Evaluation (Stuck I.e_bool) g1 st1
              end
          | r => r
          end
      end
  | EWhile c b => let cc0 := cc c in let cb := cc b in
      fix loop (m : nat) (d : fenv) (g : locals) (st : mstate) {struct m} : I.evaluation :=
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match cc0 k d g st with
          | I.Evaluation (Normal w) g1 st1 =>
              match I.truthy P w with
              | Some true =>
                  match cb k d g1 st1 with
                  | I.Evaluation (Normal _) g2 st2 => loop k d g2 st2
                  | I.Evaluation Breaking g2 st2 => I.Evaluation (Normal VUnit) g2 st2
                  | r => r
                  end
              | Some false => I.Evaluation (Normal VUnit) g1 st1
              | None => I.Evaluation (Stuck I.e_bool) g1 st1
              end
          | r => r
          end
      end
  | ELoop b => let cb := cc b in
      fix loop (m : nat) (d : fenv) (g : locals) (st : mstate) {struct m} : I.evaluation :=
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match cb k d g st with
          | I.Evaluation (Normal _) g2 st2 => loop k d g2 st2
          | I.Evaluation Breaking g2 st2 => I.Evaluation (Normal VUnit) g2 st2
          | r => r
          end
      end
  | EReturn e1 => let c1 := cc e1 in fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match c1 k d g st with
          | I.Evaluation (Normal w) g1 st1 => I.Evaluation (Returning w) g1 st1
          | r => r
          end
      end
  | EBreak => fun m d g st =>
      match m with 0 => I.Evaluation OutOfFuel g st | S _ => I.Evaluation Breaking g st end
  | ERecord fs => let ca := cc_list cc (map snd fs) in let fields := map fst fs in fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match ca k d g st with
          | inl (ws, g1, st1) => I.Evaluation (Normal (VRecord (combine fields ws))) g1 st1
          | inr r => r
          end
      end
  | EField e1 f => let c1 := cc e1 in fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match c1 k d g st with
          | I.Evaluation (Normal w) g1 st1 =>
              match I.get_field f w with
              | Some v => I.Evaluation (Normal v) g1 st1
              | None => I.Evaluation (Stuck I.e_field) g1 st1
              end
          | r => r
          end
      end
  | EMatch e0 arms => let c0 := cc e0 in let carms := cc_arms cc arms in fun m d g st =>
      match m with
      | 0 => I.Evaluation OutOfFuel g st
      | S k =>
          match c0 k d g st with
          | I.Evaluation (Normal w) g1 st1 =>
              match select_carm carms w with
              | Some (bs, cb) => I.popn (List.length bs) (cb k d (bs ++ g1) st1)
              | None => I.Evaluation (Stuck I.e_pat) g1 st1
              end
          | r => r
          end
      end
  end.

Definition ccl (l : list expr) : lcode := cc_list cc l.
Definition ccbs (bs : list binding) : bcode := fold_right (cc_bind cc) bnil bs.

End CC.

Definition crun_main (P : program_view) (tbl : fsym -> option code) (fuel : nat) (main : fsym)
    (d0 : fenv) (st0 : mstate) : I.evaluation :=
  match tbl main with
  | Some c => I.ret_call [] (c fuel d0 [] st0)
  | None => I.Evaluation (Stuck I.e_nobody) [] st0
  end.

(* ===================== Agreement ========================================= *)

(* What the driver's tables hold: the code of the view's bodies. *)
Definition tbl_ok (P : program_view) (tbl : fsym -> option code) (ftbl : usym -> option bcode) : Prop :=
  (forall f, tbl f = option_map (cc P tbl ftbl) (view_function P f)) /\
  (forall h, ftbl h = option_map (fun code => ccbs P tbl ftbl (fc_bindings code)) (view_functor P h)).

Section Agreement.
Variables (P : program_view) (tbl : fsym -> option code) (ftbl : usym -> option bcode).
Hypothesis Tables : tbl_ok P tbl ftbl.

Lemma cc_tag_spec : forall n th c m d g st,
  cc_tag P n th c m d g st =
  match m with
  | 0 => I.Evaluation OutOfFuel g st
  | S k =>
      match c k d g st with
      | I.Evaluation (Normal w) g1 st1 => I.Evaluation (L.lcheck_tag P (lshadow d) n th w) g1 st1
      | r => r
      end
  end.
Proof.
  intros. unfold cc_tag. destruct (lookup_cast n (view_casts P)) eqn:Cast; [reflexivity|].
  destruct m as [|k]; [reflexivity|].
  destruct (c k d g st) as [[] g1 st1]; try reflexivity.
  unfold L.lcheck_tag. rewrite Cast. reflexivity.
Qed.

Lemma select_carm_cc : forall arms w,
  select_carm (cc_arms (cc P tbl ftbl) arms) w =
  option_map (fun q => (fst q, cc P tbl ftbl (snd q))) (I.select_arm arms w).
Proof.
  induction arms as [|[p e] r IH]; intro w; [reflexivity|]. cbn.
  destruct (I.match_pat p w); [reflexivity|apply IH|reflexivity].
Qed.

Lemma skipn_length_app : forall (A : Type) (bs g : list A), skipn (List.length bs) (bs ++ g) = g.
Proof. intros A bs g. induction bs as [|b bs IH]; [reflexivity|exact IH]. Qed.

Lemma read_remap_select : forall d de pl,
  (forall i, flookup_provision i de = read d (remapo pl i)) ->
  forall sm, checks d (remap_plan pl sm) = true ->
  exists d', fselect_provisions de sm = Some d' /\
    forall i, fget i (fstore_of d') = read d (remap (remap_plan pl sm) i).
Proof.
  intros d de pl H sm. induction sm as [|[req src] r IH]; intro C.
  - exists fempty. split; [reflexivity|]. intro i. apply fget_empty.
  - rewrite fselect_provisions_cons.
    cbn [remap_plan map checks forallb fst snd] in C |- *. apply andb_prop in C as [C1 C2].
    destruct (IH C2) as (d'' & E & Hd).
    rewrite H. destruct (read d (remapo pl src)) as [en|] eqn:R; [|discriminate C1].
    rewrite E. eexists. split; [reflexivity|]. intro i. cbn [finstall fstore_of remap fst snd].
    rewrite fget_fset. destruct (item_eqb i req); [symmetry; exact R|apply Hd].
Qed.

Lemma call_env_lookup : forall de th d' i,
  flookup_provision i (fcall_environment de th d') = fget i (fstore_of d').
Proof. reflexivity. Qed.

Lemma ret_call_normal : forall g w g' st, I.ret_call g (I.Evaluation (Normal w) g' st) = I.Evaluation (Normal w) g st.
Proof. reflexivity. Qed.

Lemma fold_sound : forall m,
  (forall n pl e s, tos P m n pl e = Some s ->
     forall d de, (forall i, flookup_provision i de = read d (remapo pl i)) ->
     forall g st w st', sev P d g st s = Some (w, st') ->
     forall fuel, sdepth s <= fuel -> F.feval fuel P de g st e = I.Evaluation (Normal w) g st') /\
  (forall n pl es ss, tosl P m n pl es = Some ss ->
     forall d de, (forall i, flookup_provision i de = read d (remapo pl i)) ->
     forall g st ws st', sevl P d g ss st = Some (ws, st') ->
     forall fuel, needed ss <= fuel -> F.feval_arguments fuel P de g st es = inl (ws, g, st')).
Proof.
  induction m as [|m [IHe IHl]]; [split; intros; discriminate|]. split.
  - intros n pl e s T d de H g st w st' Ev fuel Fu.
    destruct e; cbn [tos] in T; try discriminate.
    + (* EVar *) injection T as <-. cbn in Ev, Fu. destruct fuel as [|k]; [inversion Fu|]. cbn.
      match goal with |- context [lookup_local ?y g] => destruct (lookup_local y g) end;
        [|discriminate]. injection Ev as <- <-. reflexivity.
    + (* EUnit *) injection T as <-. cbn in Ev, Fu. destruct fuel as [|k]; [inversion Fu|].
      injection Ev as <- <-. reflexivity.
    + (* EVal *) injection T as <-. cbn [sev] in Ev. destruct fuel as [|k]; [inversion Fu|]. cbn [F.feval].
      rewrite H. destruct (read d (remapo pl (IVal v))) as [[]|]; try discriminate.
      injection Ev as <- <-. reflexivity.
    + (* ETag *) destruct (lookup_cast n0 (view_casts P)) eqn:Cast; [discriminate|].
      destruct (tos P m n pl e) as [s1|] eqn:T1; cbn [option_map] in T; [|discriminate]. injection T as <-.
      cbn [sev] in Ev. destruct (sev P d g st s1) as [[w1 st1]|] eqn:E1; cbn [option_map] in Ev; [|discriminate].
      injection Ev as <- <-. cbn [sdepth] in Fu. destruct fuel as [|k]; [inversion Fu|]. cbn [F.feval].
      rewrite (IHe n pl e s1 T1 d de H g st w1 st1 E1 k ltac:(lia)).
      unfold L.lcheck_tag. rewrite Cast. reflexivity.
    + (* ECall *) destruct n as [|n']; [discriminate|].
      destruct (view_function P f) as [body|] eqn:Vf; [|discriminate].
      destruct (tosl P m (S n') pl l) as [ss|] eqn:Ta; [|discriminate].
      destruct (tos P m n' (Some (remap_plan pl s1)) body) as [sb|] eqn:Tb; [|discriminate].
      injection T as <-. cbn [sev] in Ev.
      destruct (checks d (remap_plan pl s1)) eqn:C; [|discriminate].
      change ((fix ev (args : list sexpr) (st : mstate) {struct args} : option (list value * mstate) :=
        match args with
        | [] => Some ([], st)
        | s :: rest => match sev P d g st s with
            | Some (w, st1) => match ev rest st1 with Some (ws, st2) => Some (w :: ws, st2) | None => None end
            | None => None end end) ss st) with (sevl P d g ss st) in Ev.
      destruct (sevl P d g ss st) as [[ws st1]|] eqn:Ea; [|discriminate].
      fold needed in Fu. cbn [sdepth] in Fu. fold needed in Fu.
      destruct fuel as [|k]; [inversion Fu|]. cbn [F.feval]. rewrite Vf.
      rewrite (IHl (S n') pl l ss Ta d de H g st ws st1 Ea k ltac:(lia)).
      destruct (read_remap_select d de pl H s1 C) as (d' & Es & Hd').
      rewrite fprepare_call_spec, Es. cbn [option_map].
      rewrite (IHe n' (Some (remap_plan pl s1)) body sb Tb d (fcall_environment de s0 d')
        (fun i => eq_trans (call_env_lookup de s0 d' i) (Hd' i)) _ st1 w st' Ev k ltac:(lia)).
      reflexivity.
    + (* EInvoke *) destruct (tosl P m n pl l) as [ss|] eqn:Ta; cbn [option_map] in T; [|discriminate].
      injection T as <-. cbn [sev] in Ev.
      change ((fix ev (args : list sexpr) (st : mstate) {struct args} : option (list value * mstate) :=
        match args with
        | [] => Some ([], st)
        | s :: rest => match sev P d g st s with
            | Some (w, st1) => match ev rest st1 with Some (ws, st2) => Some (w :: ws, st2) | None => None end
            | None => None end end) ss st) with (sevl P d g ss st) in Ev.
      destruct (read d (remapo pl (IFn f))) as [[| | |p held]|] eqn:R; try discriminate.
      destruct (sevl P d g ss st) as [[ws st1]|] eqn:Ea; [|discriminate].
      destruct (I.run_prim_at P p held ws st1) as [[]  st2] eqn:Rp; try discriminate.
      injection Ev as <- <-. cbn [sdepth] in Fu. fold needed in Fu.
      destruct fuel as [|k]; [inversion Fu|]. cbn [F.feval]. rewrite H, R.
      rewrite (IHl n pl l ss Ta d de H g st ws st1 Ea k ltac:(lia)). rewrite Rp. reflexivity.
    + (* EMatch *) destruct l as [|[p b] [|]]; try discriminate.
      destruct (tos P m n pl e) as [s1|] eqn:T1; [|discriminate].
      destruct (tos P m n pl b) as [sb|] eqn:Tb; [|discriminate]. injection T as <-.
      cbn [sev] in Ev. destruct (sev P d g st s1) as [[w1 st1]|] eqn:E1; [|discriminate].
      destruct (I.match_pat p w1) as [bs| |] eqn:Mp; try discriminate.
      cbn [sdepth] in Fu. destruct fuel as [|k]; [inversion Fu|]. cbn [F.feval].
      rewrite (IHe n pl e s1 T1 d de H g st w1 st1 E1 k ltac:(lia)). cbn [I.select_arm]. rewrite Mp.
      rewrite (IHe n pl b sb Tb d de H (bs ++ g) st1 w st' Ev k ltac:(lia)).
      cbn. rewrite skipn_length_app. reflexivity.
  - intros n pl es ss T d de H g st ws st' Ev fuel Fu.
    destruct es as [|e r]; cbn [tosl] in T.
    + injection T as <-. cbn in Ev. injection Ev as <- <-. destruct fuel as [|k]; [inversion Fu|]. reflexivity.
    + destruct (tos P m n pl e) as [s1|] eqn:T1; [|discriminate].
      destruct (tosl P m n pl r) as [ss'|] eqn:T2; [|discriminate]. injection T as <-.
      cbn [sevl] in Ev. destruct (sev P d g st s1) as [[w1 st1]|] eqn:E1; [|discriminate].
      fold (sevl P d g ss' st1) in Ev.
      destruct (sevl P d g ss' st1) as [[ws' st2]|] eqn:E2; [|discriminate]. injection Ev as <- <-.
      cbn [needed] in Fu. destruct fuel as [|k]; [inversion Fu|]. cbn [F.feval_arguments].
      rewrite (IHe n pl e s1 T1 d de H g st w1 st1 E1 k ltac:(lia)).
      rewrite (IHl n pl r ss' T2 d de H g st1 ws' st2 E2 k ltac:(lia)). reflexivity.
Qed.

Lemma lk_eqb_true : forall i j, lk_eqb i j = true <-> i = j.
Proof.
  intros [i|] [j|]; cbn [lk_eqb]; try (split; congruence).
  rewrite fast_item_eqb_spec, ProgramPrune.item_eqb_true. split; congruence.
Qed.

Lemma gather_slots : forall keys d slots, gather d keys = Some slots ->
  forall i, In i keys -> exists entry, read d i = Some entry /\ slot_lookup keys i slots = Some entry.
Proof.
  induction keys as [|j rest IH]; intros d slots H i Hin; [contradiction|].
  cbn [gather] in H. destruct (read d j) as [entry|] eqn:Read; [|discriminate].
  destruct (gather d rest) as [tail|] eqn:Tail; cbn [option_map] in H; [|discriminate].
  injection H as <-. unfold slot_lookup. cbn [key_pos].
  destruct (lk_eqb i j) eqn:Equal.
  - apply lk_eqb_true in Equal. subst i. exists entry. split; [exact Read|reflexivity].
  - destruct Hin as [Same|Hin].
    + subst j. assert (lk_eqb i i = true) by (apply lk_eqb_true; reflexivity). congruence.
    + destruct (IH d tail Tail i Hin) as (en & Hread & Hslot).
      exists en. split; [exact Hread|].
      unfold slot_lookup in Hslot. destruct (key_pos i rest); exact Hslot.
Qed.

Lemma scalar_keys_in : forall s i, In i (scalar_reads s) -> In i (scalar_keys s).
Proof.
  intros s i. unfold scalar_keys. induction (scalar_reads s) as [|j rest IH]; [contradiction|].
  intro Hin. cbn [fold_right].
  set (keys := fold_right (fun k acc => if existsb (lk_eqb k) acc then acc else k :: acc) [] rest) in *.
  destruct (existsb (lk_eqb j) keys) eqn:Present.
  - destruct Hin as [<-|Hin]; [|apply IH; exact Hin].
    apply existsb_exists in Present as (k & Hk & Equal). apply lk_eqb_true in Equal. congruence.
  - destruct Hin as [<-|Hin]; [left; reflexivity|right; apply IH; exact Hin].
Qed.

Lemma present_checks : forall d pl,
  (forall i, In i (map snd pl) -> exists en, read d i = Some en) -> checks d pl = true.
Proof.
  intros d pl. induction pl as [|[req src] rest IH]; intro H; [reflexivity|].
  destruct (H src (or_introl eq_refl)) as [en Read].
  cbn [checks forallb snd]. rewrite Read. cbn. apply IH.
  intros i Hin. apply H. right. exact Hin.
Qed.

Definition scalar_arguments (keys : list lk) (args : list sexpr) :=
  fold_right (fun e cr => let c := scalar_compile P keys e in scalar_cons c cr) scalar_nil args.

Lemma sev_prim : forall d g st i args,
  sev P d g st (SPrim i args) =
  match read d i with
  | Some (FPrimitive p held) =>
      match sevl P d g args st with
      | Some (ws, st1) =>
          match I.run_prim_at P p held ws st1 with (Normal w, st2) => Some (w, st2) | _ => None end
      | None => None
      end
  | _ => None
  end.
Proof. reflexivity. Qed.

Lemma sev_call : forall d g st guard args body,
  sev P d g st (SCall guard args body) =
  if checks d guard then
    match sevl P d g args st with
    | Some (ws, st1) => sev P d (fast_parameter_values ws) st1 body
    | None => None
    end
  else None.
Proof. reflexivity. Qed.

Lemma scalar_compile_agrees : forall bound s keys slots d g st,
  sdepth s <= bound ->
  (forall i, In i (scalar_reads s) -> exists entry, read d i = Some entry /\ slot_lookup keys i slots = Some entry) ->
  scalar_compile P keys s slots g st = sev P d g st s.
Proof.
  induction bound as [|bound IH]; intros s keys slots d g st Depth Slots.
  - destruct s; cbn [sdepth] in Depth; lia.
  - assert (Args : forall args g' st', needed args <= S bound ->
        (forall i, In i (flat_map scalar_reads args) ->
          exists entry, read d i = Some entry /\ slot_lookup keys i slots = Some entry) ->
        scalar_arguments keys args slots g' st' = sevl P d g' args st').
    { induction args as [|arg rest IHr]; intros g' st' B H; [reflexivity|].
      cbn [needed] in B.
      change (scalar_arguments keys (arg :: rest)) with
        (scalar_cons (scalar_compile P keys arg) (scalar_arguments keys rest)).
      unfold scalar_cons. cbn [sevl].
      rewrite (IH arg keys slots d g' st') by
        (first [lia | intros i Hin; apply H; apply in_or_app; left; exact Hin]).
      destruct (sev P d g' st' arg) as [[w st1]|]; [|reflexivity].
      rewrite (IHr g' st1) by
        (first [lia | intros i Hin; apply H; apply in_or_app; right; exact Hin]).
      reflexivity. }
    destruct s as [x| |i|tag a|i args|guard args body|a pat body];
      cbn [scalar_reads] in Slots.
    + reflexivity.
    + reflexivity.
    + destruct (Slots i (or_introl eq_refl)) as (entry & Read & Slot).
      cbn [scalar_compile sev]. rewrite Read, Slot. reflexivity.
    + cbn [sdepth] in Depth. cbn [scalar_compile sev].
      rewrite (IH a keys slots d g st) by (assumption || lia). reflexivity.
    + change (S (needed args) <= S bound) in Depth.
      destruct (Slots i (or_introl eq_refl)) as (entry & Read & Slot).
      cbn [scalar_compile]. fold (scalar_arguments keys args).
      rewrite sev_prim, Read, Slot.
      rewrite (Args args g st) by (first [lia | intros j Hj; apply Slots; right; exact Hj]).
      reflexivity.
    + change (S (Nat.max (needed args) (sdepth body)) <= S bound) in Depth.
      cbn [scalar_compile]. fold (scalar_arguments keys args). rewrite sev_call.
      rewrite (present_checks d guard).
      2: { intros i Hin. destruct (Slots i ltac:(apply in_or_app; left; exact Hin)) as (en & Read & _).
           exists en. exact Read. }
      rewrite (Args args g st) by
        (first [lia | intros i Hin; apply Slots; apply in_or_app; right; apply in_or_app; left; exact Hin]).
      destruct (sevl P d g args st) as [[ws st1]|]; [|reflexivity].
      apply IH; [lia|]. intros i Hin. apply Slots. apply in_or_app; right. apply in_or_app; right. exact Hin.
    + cbn [sdepth] in Depth. cbn [scalar_compile sev].
      rewrite (IH a keys slots d g st) by
        (first [lia | intros i Hin; apply Slots; apply in_or_app; left; exact Hin]).
      destruct (sev P d g st a) as [[w st1]|]; [|reflexivity].
      destruct (I.match_pat pat w) as [bs| |]; try reflexivity.
      apply IH; [lia|]. intros i Hin. apply Slots. apply in_or_app; right. exact Hin.
Qed.

Lemma fold_call_ok : forall e fb n d g st,
  fb n d g st = F.feval n P d g st e -> fold_call P e fb n d g st = F.feval n P d g st e.
Proof.
  intros e fb n d g st Hfb. unfold fold_call.
  destruct (tos P 48 6 None e) as [s|] eqn:T; [|exact Hfb].
  destruct (Nat.leb (sdepth s) n) eqn:Le; [|exact Hfb].
  destruct (gather d (scalar_keys s)) as [slots|] eqn:Gather; [|exact Hfb].
  rewrite (scalar_compile_agrees (sdepth s) s (scalar_keys s) slots d g st).
  2: { lia. }
  2: { intros i Hin. eapply gather_slots; [exact Gather|apply scalar_keys_in; exact Hin]. }
  destruct (sev P d g st s) as [[w st1]|] eqn:Ev; [|exact Hfb].
  symmetry. apply Nat.leb_le in Le.
  exact (proj1 (fold_sound 48) 6 None e s T d d (fun i => eq_refl) g st w st1 Ev n Le).
Qed.

Definition agree (n : nat) : Prop :=
  (forall e d g st, cc P tbl ftbl e n d g st = F.feval n P d g st e) /\
  (forall l d g st, cc_list (cc P tbl ftbl) l n d g st = F.feval_arguments n P d g st l) /\
  (forall bs d g st, ccbs P tbl ftbl bs n d g st = F.feval_bindings n P d g st bs) /\
  (forall u th outputs sm rs d st,
     capply P ftbl u th outputs sm rs n d st = F.fapply_functor n P d u th outputs sm rs st).

Lemma agree_zero : agree 0.
Proof.
  repeat split.
  - intros e d g st. destruct e; try reflexivity; try (destruct b; reflexivity).
    + apply cc_tag_spec.
    + apply fold_call_ok. reflexivity.
  - intros l d g st. destruct l as [|e rest]; [reflexivity|].
    destruct e; reflexivity.
  - intros bs d g st. destruct bs as [|b bs]; [reflexivity|]. destruct b; reflexivity.
Qed.

Lemma agree_step : forall k, agree k -> agree (S k).
Proof.
  intros k [IHe [IHl [IHb IHa]]]. destruct Tables as [Ht Hf].
  repeat split.
  - intros e. destruct e; intros d g st; cbn [cc];
      lazymatch goal with
      | |- _ = F.feval _ _ _ _ _ (ECall _ _ _ _) => apply fold_call_ok; cbn [F.feval]
      | _ => cbn [F.feval]
      end.
    + reflexivity.
    + reflexivity.
    + reflexivity.
    + rewrite cc_tag_spec, IHe. reflexivity.
    + rewrite Ht. destruct (view_function P f) as [body|]; cbn [option_map]; [|reflexivity].
      rewrite IHl. destruct (F.feval_arguments k P d g st l) as [[[ws g1] st1]|r]; [|reflexivity].
      destruct (fprepare_call d s s0); [|reflexivity]. rewrite IHe. reflexivity.
    + destruct (flookup_provision (IFn f) d) as [[w|f' df|u a dc q|p o]|]; try reflexivity.
      * rewrite Ht. destruct (view_function P f') as [body|]; cbn [option_map]; [|reflexivity].
        rewrite IHl. destruct (F.feval_arguments k P d g st l) as [[[ws g1] st1]|r]; [|reflexivity].
        rewrite IHe. reflexivity.
      * rewrite IHl. reflexivity.
    + destruct (flookup_provision i d) as [[w|f' df|u a dc q|p o]|]; try reflexivity.
      * rewrite Ht. destruct (view_function P f') as [body|]; cbn [option_map]; [|reflexivity].
        rewrite IHe. destruct (F.feval k P d g st e) as [[] g0 st0]; try reflexivity.
        rewrite IHl. destruct (F.feval_arguments k P d g0 st0 l) as [[[ws g1] st1]|r]; [|reflexivity].
        rewrite IHe. reflexivity.
      * rewrite IHe. destruct (F.feval k P d g st e) as [[] g0 st0]; try reflexivity.
        rewrite IHl. reflexivity.
    + rewrite IHe. destruct (F.feval k P d g st e1) as [[] g1 st1]; try reflexivity.
      rewrite IHe. reflexivity.
    + destruct b; try (
        match goal with
        | |- context [cc_bind P ftbl (cc P tbl ftbl) ?b bnil] =>
            change (cc_bind P ftbl (cc P tbl ftbl) b bnil) with (ccbs P tbl ftbl [b]);
            rewrite IHb; destruct (F.feval_bindings k P d g st [b]) as [[[d1 g1] st1]|r];
              [apply IHe|reflexivity]
        end).
      destruct k as [|[|k']]; cbn [F.feval_bindings]; try reflexivity. apply IHe.
    + rewrite IHe. destruct (F.feval k P d g st e1) as [[] g1 st1]; try reflexivity. apply IHe.
    + rewrite IHe. reflexivity.
    + rewrite IHe. destruct (F.feval k P d g st e1) as [[] g1 st1]; try reflexivity.
      destruct (I.truthy P v) as [[]|]; try reflexivity; apply IHe.
    + (* while: the loop is the code of the same expression *)
      change ((fix loop (m : nat) (d0 : fenv) (g0 : locals) (st0 : mstate) {struct m} : I.evaluation :=
        match m with
        | 0 => I.Evaluation OutOfFuel g0 st0
        | S k0 =>
            match cc P tbl ftbl e1 k0 d0 g0 st0 with
            | I.Evaluation (Normal w) g1 st1 =>
                match I.truthy P w with
                | Some true =>
                    match cc P tbl ftbl e2 k0 d0 g1 st1 with
                    | I.Evaluation (Normal _) g2 st2 => loop k0 d0 g2 st2
                    | I.Evaluation Breaking g2 st2 => I.Evaluation (Normal VUnit) g2 st2
                    | r => r
                    end
                | Some false => I.Evaluation (Normal VUnit) g1 st1
                | None => I.Evaluation (Stuck I.e_bool) g1 st1
                end
            | r => r
            end
        end) k) with (cc P tbl ftbl (EWhile e1 e2) k).
      rewrite IHe. destruct (F.feval k P d g st e1) as [[] g1 st1]; try reflexivity.
      destruct (I.truthy P v) as [[]|]; try reflexivity.
      rewrite IHe. destruct (F.feval k P d g1 st1 e2) as [[] g2 st2]; try reflexivity.
      apply IHe.
    + change ((fix loop (m : nat) (d0 : fenv) (g0 : locals) (st0 : mstate) {struct m} : I.evaluation :=
        match m with
        | 0 => I.Evaluation OutOfFuel g0 st0
        | S k0 =>
            match cc P tbl ftbl e k0 d0 g0 st0 with
            | I.Evaluation (Normal _) g2 st2 => loop k0 d0 g2 st2
            | I.Evaluation Breaking g2 st2 => I.Evaluation (Normal VUnit) g2 st2
            | r => r
            end
        end) k) with (cc P tbl ftbl (ELoop e) k).
      rewrite IHe. destruct (F.feval k P d g st e) as [[] g2 st2]; try reflexivity. apply IHe.
    + rewrite IHe. reflexivity.
    + reflexivity.
    + rewrite IHl. reflexivity.
    + rewrite IHe. reflexivity.
    + rewrite IHe. destruct (F.feval k P d g st e) as [[] g1 st1]; try reflexivity.
      rewrite select_carm_cc. destruct (I.select_arm l v) as [[bs eb]|]; cbn [option_map fst snd]; [|reflexivity].
      rewrite IHe. reflexivity.
  - intros l d g st. destruct l as [|e1 r]; cbn [cc_list F.feval_arguments]; [reflexivity|].
    destruct e1; cbn [cc_list]; try match goal with
    | |- context [cc P tbl ftbl ?e k d g st] =>
        rewrite (IHe e d g st);
        destruct (F.feval k P d g st e) as [[] g1 st1]; try reflexivity;
        unfold push_arg; rewrite IHl; reflexivity
    end.
    all: destruct k as [|k']; [reflexivity|]; cbn [F.feval].
    all: try match goal with
      | |- context [lookup_local ?x ?env] => destruct (lookup_local x env)
      | |- context [flookup_provision (IVal ?v) ?env] => destruct (flookup_provision (IVal v) env) as [[]|]
      end.
    all: unfold push_arg; try rewrite IHl; reflexivity.
  - intros bs d g st. destruct bs as [|b bs]; [reflexivity|].
    change (ccbs P tbl ftbl (b :: bs)) with (cc_bind P ftbl (cc P tbl ftbl) b (ccbs P tbl ftbl bs)).
    destruct b; cbn [cc_bind F.feval_bindings]; try (rewrite IHb; reflexivity).
    + rewrite IHe. destruct (F.feval k P d g st e) as [[] g1 st1]; try reflexivity. apply IHb.
    + destruct (fselect_provisions d s); [apply IHb|reflexivity].
    + destruct (fselect_provisions d s0); [apply IHb|reflexivity].
    + destruct (fselect_provisions d s0); [apply IHb|reflexivity].
    + destruct (flookup_provision (IFun u0) d) as [[w|f' df|h a dc q|p o]|]; try reflexivity. apply IHb.
    + rewrite IHa. destruct (F.fapply_functor k P d u s l s0 l0 st) as [[dr st1]|r]; [apply IHb|reflexivity].
  - intros u th outputs sm rs d st. cbn [capply F.fapply_functor].
    destruct (flookup_provision (IFun u) d) as [[w|f' df|h a dc q|p o]|]; try reflexivity.
    rewrite Hf. destruct (view_functor P h) as [code|]; cbn [option_map]; [|reflexivity]. cbv zeta.
    destruct (fselect_provisions d sm) as [d1|]; [|reflexivity].
    destruct (fadapt_provisions d1 a) as [d2|]; [|reflexivity].
    rewrite IHb. reflexivity.
Qed.

Theorem cc_agrees : forall n e d g st, cc P tbl ftbl e n d g st = F.feval n P d g st e.
Proof.
  intros n. assert (H : agree n) by (induction n; [apply agree_zero|apply agree_step; assumption]).
  apply H.
Qed.

Theorem crun_main_agrees : forall fuel main d0 st0,
  crun_main P tbl fuel main d0 st0 = F.frun_main fuel P main d0 st0.
Proof.
  intros. unfold crun_main, F.frun_main. rewrite (proj1 Tables).
  destruct (view_function P main); cbn [option_map]; [|reflexivity]. rewrite cc_agrees. reflexivity.
Qed.

End Agreement.

(* The whole stack: under tbl_ok, the compiled run on main's converted
   environment is Audit's reference run. *)
Theorem crun_main_reference : forall P tbl ftbl fuel main d0 st0,
  tbl_ok P tbl ftbl ->
  crun_main P tbl fuel main (fast_env (lift_env d0)) st0 = I.run_main fuel P main d0 st0.
Proof.
  intros P tbl ftbl fuel main d0 st0 T. rewrite (crun_main_agrees P tbl ftbl T). apply F.frun_main_agrees.
Qed.

End CloInterpreter.
