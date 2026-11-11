(* ------------------------------------------------------------------------ *)
(* EaMain.v — the knot, FnElab for every defined fn, and assembly:         *)
(* `elaborate : bundle -> er (world * program)`.                           *)
(*                                                                           *)
(* SOUND (the goal):                                                       *)
(*   elaborate b = Ok (W, P) -> ElaboratesIn b W P /\ Platform.Linked W P  *)
(* Pending pieces (elab-STATUS.md): AssumesComplete and NoExpandingCycle   *)
(* are not yet checked, and some Linked checks are TODO (EaPlatform).      *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith.
From NM Require Import AuditBase.
From NM Require Import Audit NMap EaBase EaTypes EaCtx EaCall EaFamily EaDecide EaExpr EaFunctor EaBind EaPlatform EaExpand EaAssumptionAtoms.
From NM Require Sx ElabWorld PublicWorld LoadListed.
From NM Require Import EaBudget.
Import ListNotations.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* ---- the knot ---- *)

Inductive query :=
| QEx (F : frame) (C : cx) (e : aexpr) (exp : option ty)
| QBlkE (F : frame) (C : cx) (b : ablock) (exp : option ty)
| QBind (F : frame) (C : cx) (its : list (spec * option aexpr)).
Inductive answer :=
| AExpr (r : expr * ty)
| AItems (r : wrapper * cx * frame * list nat).

Definition mkrec (call : query -> M answer) : rec :=
  Rec (fun F C e exp => let* a := call (QEx F C e exp) in
                        match a with AExpr r => ret r | _ => fail "knot" end)
      (fun F C b exp => let* a := call (QBlkE F C b exp) in
                        match a with AExpr r => ret r | _ => fail "knot" end)
      (fun F C its => let* a := call (QBind F C its) in
                      match a with AItems r => ret r | _ => fail "knot" end).

Section Main.
Variable E : env.
Let W := e_W E.
Let fuel := e_fuel E.

(* SOUND: by induction on n, RecSound E P (mkrec (run n)) given the
   per-file soundness theorems. *)
Fixpoint run (n : nat) (q : query) : M answer :=
  match n with
  | 0 => fail "elaboration: too deep"
  | S n' =>
      let R := mkrec (run n') in
      match q with
      | QEx F C e exp => let* r := ex E R fuel F C e exp in ret (AExpr r)
      | QBlkE F C b exp => let* r := blke E R fuel F C b exp in ret (AExpr r)
      | QBind F C its => let* r := bind_items E R F C its in ret (AItems r)
      end
  end.
Definition R0 : rec := mkrec (run fuel).

(* SOUND: elab_fn f = Ok tt (and body_of P f = the body recorded) -> FnElab W P f *)
Definition elab_fn (f : nat) : M unit :=
  let* d := lift (w_decl W f) "internal: unknown fn" in
  match d_ast d with
  | ADFn rcv nm ps rt (Some b) =>
      let* (C, ns) := lift (w_context W f) ("the context of " ++ qs W f ++ " does not form") in
      let* this := match rcv with
                   | RAttached rn =>
                       let* r := lift (match w_scope W (d_mod d) NSName rn with Some (ESym r) => Some r | _ => None end)
                                      ("attached method receiver `" ++ rn ++ "` is not in scope") in
                       let* _ := check (match w_attached W r nm with Some f' => Nat.eqb f f' | None => false end)
                                  ("duplicate definition of `" ++ rn ++ "." ++ nm ++ "`") in
                       let* _ := check (kind_is W r DKTag || kind_is W r DKUnit)
                                  ("attached method `" ++ rn ++ "." ++ nm ++ "` requires a nominal receiver (Q5); `" ++ rn ++ "` is abstract") in
                       let* args := match find (fun x => negb (is_some (nlook x (c_ty C)))) (w_reqs W r) with
                                    | Some x => fail ("type " ++ qs W x ++ " (required by " ++ qs W r ++ ") is not available in the context here")
                                    | None => ret (flat_map (fun x => match nlook x (c_ty C) with Some t => [(x, t)] | None => [] end) (w_reqs W r))
                                    end in
                       ret (Some (TNom r args))
                   | _ => ret None
                   end in
      let* g := pure (sig_of W fuel f (c_ty C) this) in
      let pnames := map fst ps in
      let* _ := match suniq_dup ((if is_some this then ["this"] else []) ++ pnames) with
                | Some n => fail ("parameter `" ++ n ++ "` of " ++ qs W f ++ " is declared twice")
                | None => ret tt end in
      let F := Frame (d_mod d) (Some (sg_ret g)) this false
                     (mk_frame_vars (is_some this) pnames (block_names W fuel (d_mod d) b)) [] [] [] in
      let C0 := fold_left (fun C' q => add_local C' (fst q) (snd q) LLet) (combine pnames (sg_params g)) C in
      let* (x, _) := r_blke R0 F C0 b (Some (sg_ret g)) in
      set_fn_body f x
  | _ => ret tt
  end.

Definition defined_fn (s : nat) : bool :=
  match w_decl W s with Some d => match d_ast d with ADFn _ _ _ _ (Some _) => true | _ => false end | None => false end.

(* every message names the module of the declaration it is about, as the
   old oracle's `err mi` did *)
Definition fn_label (s : nat) : string := mod_of W s ++ ": ".

Fixpoint collect {A} (m : nmap A) (n : nat) (acc : list (nat * A)) : list (nat * A) :=
  match n with
  | 0 => match nget m 0 with Some a => (0, a) :: acc | None => acc end
  | S k => collect m k (match nget m n with Some a => (n, a) :: acc | None => acc end)
  end.

(* SOUND: assemble bound entry = Ok P -> ElaboratesIn b W P /\ Linked W P
   (modulo the pending checks) *)
Definition assemble (bound : nat) (entry : string) : M program :=
  let* _ := miter (fun s => if defined_fn s then with_msg (fn_label s) (elab_fn s) else ret tt) (seq 0 bound) in
  let* _ := miter (fun s => match w_decl W s with
      | Some d => match d_ast d with
                  | ADVal nm _ (Some _) =>
                      with_msg (d_mod d ++ ": ")
                        (let* _ := r_ex R0 (Frame (d_mod d) None None false [] [] [] []) (Cx [] [] [] [] []) (AEPath [nm] None) None in ret tt)
                  | _ => ret tt end
      | None => ret tt end) (seq 0 bound) in
  let* main := lift (match w_scope W entry NSName "main" with Some (ESym s) => Some s | _ => None end) "unsupported: no main" in
  let* _ := check (match ast_of W main with Some (ADFn RPlain _ _ _ (Some _)) => true | _ => false end)
             "unsupported: no main" in
  let* _ := check (match ast_of W main with Some (ADFn _ _ [] _ _) => true | _ => false end)
             (mod_of W main ++ ": main must take no parameters (D39)") in
  let* env0 := main_environment E main in
  (* Platform.Linked: EntryPoint, Layouts, Representations *)
  let* (Cm, _) := lift (w_context W main) "the context of `main` does not form" in
  let* gm := pure (sig_of W fuel main (c_ty Cm) None) in
  let* _ := check (match sg_params gm with [] => true | _ => false end && entry_result E fuel (sg_ret gm))
             (mod_of W main ++ ": main must return a type with no runtime lanes (D39)") in
  let* _ := pure (layouts E) in
  (* ElaboratesIn: formation of the language's Bool item. *)
  let* _ := pure (lang_items_formed E) in
  let* _ := check (casts_ok E) "the platform's `Ref` or `Array` is declared without its held or raw type (Representations)" in
  (* [D58]/[D59] FiniteLayouts (#90) *)
  let* _ := pure (finite_layouts W fuel bound) in
  let* st := get in
  let '(bl, tr, fl) := bool_syms E in
  let links := map (fun q => (fst q, slot_index (snd q))) (es_link st) in
  let program := Program (collect (es_fns st) (es_next st) []) (collect (es_funs st) (es_next st) [])
                    bl tr fl main env0 (es_unsupported st) (casts E) (Platform.machine_table W) in
  let* _ := check (expansion_ok W fuel program (es_lamcx st) (es_fns st) (es_funs st) links bound (es_next st))
             "an expanding cycle (D16/D30, NoExpandingCycle)" in
  ret program.

End Main.

(* ---- AssumesComplete ([D20]) ---- *)

Section Assumes.
Variable E : env.
Let W := e_W E.

(* SOUND: assumed_complete bound C ns = Ok tt -> AssumedComplete W C ns *)
Definition assumed_complete (bound : nat) (C : cx) (ns : list item) : er unit :=
  let! _ := rmap (fun k =>
    match need_item k with
    | None => Ok tt
    | Some s =>
        let! a := ropt (capp C k) ("internal: assumed item without an instantiation: " ++ show_item W k) in
        let! _ := match find (fun q => negb (is_some (snd q))) a with
                  | Some (t, _) => Err ("assuming " ++ qs W s ++ " needs " ++ qs W t ++ " in force (D20)")
                  | None => Ok tt end in
        let Cv := fold_left (fun C q => set_ty C (fst q) (snd q)) (known a) C in
        let! (Cs, nss) := ropt (w_context W s) ("the context of " ++ qs W s ++ " does not form") in
        let! _ := resolve_assumption_error W Cv Cs bound
          (let! th := assumption_theta W ("assuming " ++ qs W s) Cv Cs bound in
           satisfies W ("calling " ++ qs W s) ("assuming " ++ qs W s) Cv Cs th nss) in
        Ok tt
    end) ns in
  Ok tt.

(* SOUND: assumes_complete bound = Ok tt -> AssumesComplete W (every
   symbol is below bound) *)
Definition assumes_complete (bound : nat) : er unit :=
  let! _ := rmap (fun s =>
    match kind_sym W s with
    | Some _ =>
        match assume_ctx E s with
        | Ok (C, ns) => match assumed_complete bound C ns with
                        | Ok _ => Ok tt
                        | Err m => Err (mod_of W s ++ ": " ++ m)
                        end
        | Err m => Err (mod_of W s ++ ": " ++ m)
        end
    | _ => Ok tt
    end) (seq 0 bound) in
  Ok tt.
End Assumes.


(* ---- DeclsForm (#74, #76, #77): every declaration forms as written ---- *)
Section Decls.
Variable E : env.
Let W := e_W E.
Let fuel := e_fuel E.

Definition self_all (bound : nat) : list (nat * ty) :=
  map (fun t => (t, TAbs t)) (filter (fun t => kind_is W t DKType) (seq 0 bound)).

Definition body_binds (p : string) (it : spec * option aexpr) : list (nat * option name) :=
  match it with
  | (Spec (Some pa) dn _, Some _) => match resolve_sym W p pa with Some s => [(s, dn)] | None => [] end
  | (Spec (Some pa) _ _, None) =>
      match resolve_sym W p pa with
      | Some g => if kind_is W g DKFunctor then result_leaves W fuel g else []
      | None => []
      end
  | _ => []
  end.

Definition decl_forms (bound : nat) (s : nat) : er unit :=
  let tm := self_all bound in
  match w_decl W s with
  | None => Ok tt
  | Some d =>
      match d_ast d with
      | ADCtx _ its =>
          let! _ := items_entries E fuel [s] (d_mod d) [] tm its in Ok tt
      | ADTag _ _ t | ADAlias _ t =>
          let! _ := match d_ast d, t with
                    | ADTag _ _ _, ATRec fs => rcheck (snodupb (map fst fs)) (qs W s ++ " names a field twice, so no record literal builds it")
                    | _, _ => Ok tt end in
          let! _ := ty_of W fuel (d_mod d) tm None t in Ok tt
      | ADVal _ t _ =>
          let! _ := ty_of W fuel (d_mod d) tm None t in Ok tt
      | ADFn RPlain _ _ _ _ =>
          let! _ := sig_of W fuel s tm None in Ok tt
      | ADFn _ _ _ _ _ =>
          let! _ := sig_of W fuel s tm (Some TUnit) in Ok tt
      | ADFunctor _ _ _ _ b =>
          let! (Cu, _) := match w_context W s with Some cn => Ok cn | None => fun_ctx E s end in
          let! _ := provide_result E s (Cx (c_ty Cu ++ tm) (c_sub Cu) (c_merge Cu) (c_provisions Cu) (c_locals Cu)) (fun t => TAbs t) in
          match b with
          | None => Ok tt
          | Some bodies =>
              let got := flat_map (body_binds (d_mod d)) (List.concat bodies) in
              let missing := filter (fun k => negb (existsb (leaf_eqb k) got)) (result_leaves W fuel s) in
              match missing with
              | [] => Ok tt
              | k :: _ => Err ("functor " ++ qs W s ++ " does not bind `" ++ sym_name W (fst k) ++
                               match snd k with Some dn => "." ++ dn | None => "" end ++ "`")
              end
          end
      | _ => Ok tt
      end
  end.

(* SOUND: decls_form bound = Ok tt -> DeclsForm W P, except ConstElab,
   which assemble checks (every defined val elaborated as ExConst reads it) *)
Definition decls_form (bound : nat) : er unit :=
  let! _ := rmap (fun s => match decl_forms bound s with
                           | Ok _ => Ok tt
                           | Err m => Err (mod_of W s ++ ": " ++ m)
                           end) (seq 0 bound) in
  Ok tt.
End Decls.

(* ---- the world ---- *)

(* the world, or the old oracle's report of why the bundle has none (its
   loader's and D73's messages, which name the culprit) *)
Definition bundle_world_r (b : bundle) : er (world * nat) :=
  match LoadWorld.load_world b with
  | Sx.Bad m => Err m
  | Sx.Good w =>
      match LoadListed.check w with
      | Sx.Bad m => Err m
      | Sx.Good _ =>
          match ElabWorld.world_requirements b w with
          | Some R => Ok (PublicWorld.world_read w R, LoadWorld.w_nsyms w)
          | None => Err "the bundle has no static reading (WorldOf)"
          end
      end
  end.
Definition bundle_world_n (b : bundle) : option (world * nat) :=
  match bundle_world_r b with Ok r => Some r | Err _ => None end.

Definition module_main (W : world) (p : string) : list nat :=
  match w_scope W p NSName "main" with
  | Some (ESym s) => match ast_of W s with
                    | Some (ADFn RPlain _ _ _ _) => [s]
                    | _ => [] end
  | _ => []
  end.

Definition main_symbols (W : world) (modules : list string) : list nat :=
  uniq (flat_map (module_main W) modules).

Definition elab_fuel : nat := 100000.

(* Full soundness still asks for ElaboratesIn b W P /\ Platform.Linked W P.
   EaWorldSound and EaContextTableSound certify the world and its contexts. *)
Definition elaborate_with_metadata (strat : strategy)
  (choose_metadata_fuel : world -> nat -> nat) (b : bundle) : er (world * program) :=
  match bundle_world_r b with
  | Err m => Err m
  | Ok (W0, bound) =>
      let mods := map am_path (b_mods b) in
      let mains := main_symbols W0 mods in
      let dependency_fuel := choose_metadata_fuel W0 bound in
      let E0 := Env W0 strat mods mains elab_fuel dependency_fuel in
      let T := context_table E0 bound in
      let W := ElabWorld.with_context W0 (nget T) in
      let E := Env W strat mods mains elab_fuel dependency_fuel in
      match assumes_complete E bound with
      | Err m => Err m
      | Ok _ =>
      match decls_form E bound with
      | Err m => Err m
      | Ok _ =>
      match assemble E bound (b_entry b) (init_state bound) with
      | Ok (P, _) => Ok (W, P)
      | Err m => Err (resolve_names NE m)
      end
      end
      end
  end.

(* Preserve the existing recursion limit. Only source dependency sets use the
   finite-world budget, computed once and shared before/after context formation. *)
Definition metadata_fuel (W : world) (bound : nat) : nat :=
  Nat.max elab_fuel (S (metadata_budget W bound)).

Definition elaborate_with (strat : strategy) (b : bundle) : er (world * program) :=
  elaborate_with_metadata strat metadata_fuel b.

Definition elaborate (b : bundle) : er (world * program) := elaborate_with default_strategy b.
