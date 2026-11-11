(* ------------------------------------------------------------------------ *)
(* EaExpand.v — the expanding-cycle check (Base's NoExpandingCycle,       *)
(* D16/D30): TypesBounded and InstantiationsBounded, decided.              *)
(*                                                                           *)
(* Tarjan proposes levels by numbering the components in completion order. *)
(* The final pass checks each inequality directly against those levels.    *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith.
From NM Require Import AuditBase.
Import AuditBase.Compilation.
From NM Require Import Audit NMap EaBase EaTypes.
Import ListNotations.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* ---- a graph on (nat * nat) vertices ---- *)

Definition vkey := (nat * nat)%type.
Definition vmap (A : Type) := nmap (nmap A).
Definition vget {A} (m : vmap A) (v : vkey) : option A :=
  match nget m (fst v) with Some m' => nget m' (snd v) | None => None end.
Definition vset {A} (m : vmap A) (v : vkey) (a : A) : vmap A :=
  nset m (fst v) (nset (match nget m (fst v) with Some m' => m' | None => NE end) (snd v) a).

Record graph := Graph { g_adj : vmap (list (vkey * bool)); g_verts : list vkey }.
Definition gempty : graph := Graph NE [].
Definition gedge (g : graph) (a b : vkey) (s : bool) : graph :=
  let succ := match vget (g_adj g) a with Some l => l | None => [] end in
  let verts := match vget (g_adj g) a with Some _ => g_verts g | None => a :: g_verts g end in
  Graph (vset (g_adj g) a ((b, s) :: succ)) verts.
Definition succs (g : graph) (a : vkey) : list (vkey * bool) :=
  match vget (g_adj g) a with Some l => l | None => [] end.

(* Tarjan's strongly connected components *)
Record tst := TSt { t_next : nat; t_idx : vmap nat; t_low : vmap nat; t_on : vmap bool;
                    t_stack : list vkey; t_comp : vmap nat }.
Definition getn (m : vmap nat) (v : vkey) : nat := match vget m v with Some n => n | None => 0 end.

Fixpoint pop_comp (stack : list vkey) (v : vkey) (c : nat) (on : vmap bool) (comp : vmap nat)
  : list vkey * vmap bool * vmap nat :=
  match stack with
  | [] => ([], on, comp)
  | w :: r =>
      let on' := vset on w false in let comp' := vset comp w c in
      if Nat.eqb (fst w) (fst v) && Nat.eqb (snd w) (snd v) then (r, on', comp') else pop_comp r v c on' comp'
  end.

Fixpoint strong (fuel : nat) (g : graph) (v : vkey) (t : tst) {struct fuel} : tst :=
  match fuel with
  | 0 => t
  | S f =>
      let i := t_next t in
      let t := TSt (S i) (vset (t_idx t) v i) (vset (t_low t) v i) (vset (t_on t) v true) (v :: t_stack t) (t_comp t) in
      let t := fold_left (fun t ws =>
        let w := fst ws in
        match vget (t_idx t) w with
        | None =>
            let t := strong f g w t in
            TSt (t_next t) (t_idx t) (vset (t_low t) v (Nat.min (getn (t_low t) v) (getn (t_low t) w))) (t_on t) (t_stack t) (t_comp t)
        | Some wi =>
            if match vget (t_on t) w with Some b => b | None => false end
            then TSt (t_next t) (t_idx t) (vset (t_low t) v (Nat.min (getn (t_low t) v) wi)) (t_on t) (t_stack t) (t_comp t)
            else t
        end) (succs g v) t in
      if Nat.eqb (getn (t_low t) v) (getn (t_idx t) v) then
        let rank := t_next t in
        let '(stack, on, comp) := pop_comp (t_stack t) v rank (t_on t) (t_comp t) in
        TSt (S rank) (t_idx t) (t_low t) on stack comp
      else t
  end.

Definition component_ranks (g : graph) : vmap nat :=
  let fuel := S (List.length (g_verts g)) in
  let t := fold_left (fun t v => match vget (t_idx t) v with
       | Some _ => t | None => strong fuel g v t end)
       (g_verts g) (TSt 0 NE NE NE [] NE) in
  t_comp t.

Definition check_ranks (g : graph) (ranks : vmap nat) : bool :=
  forallb (fun a => forallb (fun (edge : vkey * bool) =>
    Nat.leb (getn ranks (fst edge) + (if snd edge then 1 else 0)) (getn ranks a))
    (succs g a)) (g_verts g).

Definition bounded (g : graph) : bool := check_ranks g (component_ranks g).

(* ---- TypesBounded ---- *)

Section Expand.
Variable W : world.
Variable fuel : nat.

Definition type_graph (bound : nat) : graph :=
  fold_left (fun g n =>
    if kind_is W n DKTag then
      match payload_of W fuel n (map (fun t => (t, TAbs t)) (w_reqs W n)) with
      | Ok pay =>
          fold_left (fun g na =>
            fold_left (fun g ytau =>
              fold_left (fun g xs => gedge g (n, fst xs) (fst na, fst ytau) (snd xs))
                        (ty_occurrences (snd ytau)) g) (snd na) g) (ty_nominals pay) g
      | Err _ => g
      end
    else g) (seq 0 bound) gempty.

(* ---- FiniteLayouts ([D58]/[D59], AUDIT-CHANGES #90) ---- *)

Fixpoint args_eqb (a b : list (nat * ty)) : bool :=
  match a, b with
  | [], [] => true
  | x :: r, y :: q => Nat.eqb (fst x) (fst y) && ty_eqb (snd x) (snd y) && args_eqb r q
  | _, _ => false end.

(* layout_reaches k seen n t = Ok (b, seen'): b = true iff LayoutReaches W t n
   (seen: the (tag, arguments) already explored, none of which reaches n;
   a tag's reach depends only on its arguments).  A tag whose payload does
   not form at its arguments, or exhausted fuel, is an error (reject). *)
Fixpoint layout_reaches (k : nat) (seen : list (nat * list (nat * ty))) (n : nat) (t : ty)
    : er (bool * list (nat * list (nat * ty))) :=
  match k with
  | 0 => Err "a layout too deep to decide (D58)"
  | S k' =>
      let fix all (ts : list ty) (seen : list (nat * list (nat * ty))) :=
        match ts with
        | [] => Ok (false, seen)
        | t :: r => let! (b, seen') := layout_reaches k' seen n t in
                    if b then Ok (true, seen') else all r seen'
        end in
      match t with
      | TNom m args =>
          if Nat.eqb m n then Ok (true, seen)
          else if existsb (fun q => Nat.eqb (fst q) m && args_eqb (snd q) args) seen then Ok (false, seen)
          else if kind_is W m DKTag then
            match payload_of W fuel m args with
            | Ok pay => let! (b, seen') := layout_reaches k' ((m, args) :: seen) n pay in Ok (b, seen')
            | Err e => Err ("a tag's payload does not form at its arguments (D58): " ++ e)
            end
          else Ok (false, seen)
      | TRecord fs => all (map snd fs) seen
      | TUnion ms => all ms seen
      | _ => Ok (false, seen)
      end
  end.

(* SOUND: finite_layouts bound = Ok tt -> FiniteLayouts W (every symbol
   below bound) *)
Definition finite_layouts (bound : nat) : er unit :=
  let! _ := rmap (fun n =>
    if kind_is W n DKTag then
      match payload_of W fuel n (map (fun t => (t, TAbs t)) (w_reqs W n)) with
      | Ok pay =>
          let! (b, _) := layout_reaches (S fuel * 64) [] n pay in
          rcheck (negb b) (mod_of W n ++ ": " ++ qs W n ++ " contains itself, so it has no finite layout (D58)")
      | Err _ => Ok tt
      end
    else Ok tt) (seq 0 bound) in
  Ok tt.

(* ---- InstantiationsBounded ---- *)

Definition node_id (n : code_node) : nat := match n with CodeFn f => 2 * f | CodeFunctor h => S (2 * h) end.

Variable P : program.
Variable fns : nmap expr.
Variable funs : nmap functor_code.

Definition sites_of (n : code_node) : list code_site :=
  match n with
  | CodeFn f => match nget fns f with Some e => expr_sites e | None => [] end
  | CodeFunctor h => match nget funs h with Some c => flat_map binding_sites (fc_bindings c) | None => [] end
  end.
Definition table_of (n : code_node) : subst :=
  match n with CodeFunctor h => match nget funs h with Some c => fc_table c | None => [] end | _ => [] end.

(* the one-step reads of a node: t -> (v, strict) *)
Definition read_steps (n : code_node) : list (nat * (nat * bool)) :=
  flat_map (fun q => map (fun vs => (fst q, vs)) (ty_occurrences (snd q))) (table_of n) ++
  flat_map (fun site => match site with
    | SiteApply th outputs =>
        flat_map (fun o => flat_map (fun q => map (fun vs => (snd o, (fst vs, true))) (ty_occurrences (snd q))) th) outputs
    | _ => [] end) (sites_of n).

Definition seen_read (acc : list (nat * bool)) (read : nat * bool) : bool :=
  existsb (fun seen => Nat.eqb (fst seen) (fst read) && Bool.eqb (snd seen) (snd read)) acc.

Definition next_reads (steps : list (nat * (nat * bool))) (frontier : list (nat * bool))
    : list (nat * bool) :=
  flat_map (fun current => flat_map (fun step =>
    if Nat.eqb (fst step) (fst current) then [(fst (snd step), snd current || snd (snd step))]
    else []) steps) frontier.

(* NodeReads n t: every (u, strict) t reads, keeping strictness along paths. *)
Fixpoint reads_closure (k : nat) (steps : list (nat * (nat * bool))) (frontier acc : list (nat * bool)) : list (nat * bool) :=
  match k with
  | 0 => acc
  | S k' =>
      let new := filter (fun read => negb (seen_read acc read)) (next_reads steps frontier) in
      match new with [] => acc | _ => reads_closure k' steps new (acc ++ new) end
  end.

Definition targets_of (site : code_site) : list (code_node * subst) :=
  match site with
  | SiteEnter n th => [(n, th)]
  | SiteApply th _ => map (fun q => (CodeFunctor (fst q), filter (fun p => existsb (Nat.eqb (fst p)) (fc_linked (snd q))) th))
                          (program_functors P)
  end.

Definition node_graph (g : graph) (n : code_node) : graph :=
  let steps := read_steps n in
  let reads v := reads_closure (S (2 * List.length steps)) steps [(v, false)] [(v, false)] in
  let a := node_id n in
  fold_left (fun g site =>
    fold_left (fun g mt =>
      fold_left (fun g xtau =>
        fold_left (fun g vs =>
          fold_left (fun g us => gedge g (a, fst us) (node_id (fst mt), fst xtau) (snd vs || snd us))
                    (reads (fst vs)) g) (ty_occurrences (snd xtau)) g) (snd mt) g) (targets_of site) g) (sites_of n) g.

Definition merge_graph (g : graph) (n : code_node) (section : subst) : graph :=
  let a := node_id n in
  fold_left (fun g q =>
    let g := fold_left (fun g vs => gedge g (a, fst vs) (a, fst q) (snd vs)) (ty_occurrences (app_subst section (TAbs (fst q)))) g in
    match app_subst section (TAbs (fst q)) with TAbs z => gedge g (a, fst q) (a, z) false | _ => g end) section g.

Definition code_nodes (next : nat) : list code_node :=
  flat_map (fun s => (match nget fns s with Some _ => [CodeFn s] | None => [] end) ++
                     (match nget funs s with Some _ => [CodeFunctor s] | None => [] end)) (seq 0 next).

Definition instantiation_graph (next : nat) : graph :=
  let nodes := code_nodes next in
  let g := fold_left node_graph nodes gempty in
  fold_left (fun g n => match code_section W P n with
    | Some section => merge_graph g n section
    | None => g
    end) nodes g.

Definition expansion_ok (bound next : nat) : bool :=
  bounded (type_graph bound) && bounded (instantiation_graph next).

End Expand.
