(* ------------------------------------------------------------------------ *)
(* EaPlatform.v — main's initial environment (PlatformEntry, InitialTypes),*)
(* the platform families' functor code (Platform.Family), the casts        *)
(* (Platform.Representations) and Bool's symbols.                          *)
(*                                                                           *)
(* TODO: Platform.Linked's remaining checks (EntryPoint, Layouts, AtKey,   *)
(* HandlesRead, MachinesFixed, MethodsDistinct, FixedMachinesRead) are not *)
(* yet decided here; see elab-STATUS.md.                                   *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith PrimInt63 Uint63.
From NM Require Import Audit EaBase EaTypes EaCall EaFamily.
Import ListNotations.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Section Platform.
Variable E : env.
Let W := e_W E.
Let fuel := e_fuel E.

Definition library_module (short p : string) : bool :=
  match w_library W with Some dir => String.eqb p (dir ++ (short ++ ".moss")) | None => false end.
Definition platform_modb (p : string) : bool :=
  library_module "wasm" p || library_module "wasip1" p || library_module "wasistd" p.

Definition wasm_sym (n : string) : option nat := lang_sym W "wasm" NSName n.
Definition opt_eqb (a : option nat) (b : nat) : bool := match a with Some x => Nat.eqb x b | None => false end.

(* SOUND: method_primitive k = Some op -> Platform.MethodPrimitive W k op
   (the op is determined by the key's method) *)
Definition method_primitive (k : item) : option prim :=
  match k with
  | IMth _ m _ =>
      match w_decl W m with
      | Some d =>
          match d_ast d with
          | ADFn RDetached nm _ _ _ =>
              if String.eqb nm "boxed" then option_map PBox (wasm_sym "Ref")
              else if String.eqb nm "arrayed" then option_map PArrNew (wasm_sym "Array")
              else None
          | ADFn (RAttached rp) nm _ _ _ =>
              match resolve_sym W (d_mod d) [rp] with
              | Some s =>
                  if opt_eqb (wasm_sym "Ref") s then
                    (if String.eqb nm "deref" then Some PDeref else if String.eqb nm "set" then Some PRefSet else None)
                  else if opt_eqb (wasm_sym "Array") s then
                    (if String.eqb nm "length" then Some PArrLen else if String.eqb nm "get" then Some PArrGet
                     else if String.eqb nm "set" then Some PArrSet else if String.eqb nm "fill" then Some PArrFill
                     else if String.eqb nm "copy" then Some PArrCopy else None)
                  else if opt_eqb (wasm_sym "Addr") s then prim_of_name nm
                  else None
              | None => None
              end
          | _ => None
          end
      | None => None
      end
  | _ => None
  end.


(* ---- Platform.Linked's checks, decided ---- *)

Definition machine_type (nm : string) (t : ty) : bool :=
  match wasm_sym nm, t with
  | Some sym, TAbs x => Nat.eqb sym x && kind_is W sym DKType
  | _, _ => false end.
Definition nominal_type (lib nm : string) (t : ty) : bool :=
  match lang_sym W lib NSName nm, t with Some sym, TNom n _ => Nat.eqb sym n | _, _ => false end.
(* SOUND: value_type t sh = true <-> Platform.ValueType W t sh *)
Definition value_type (t : ty) (sh : shape) : bool :=
  match sh with
  | ShapeI32 => machine_type "I32" t | ShapeI64 => machine_type "I64" t
  | ShapeF32 => machine_type "F32" t | ShapeF64 => machine_type "F64" t
  | ShapeUnit => ty_eqb t TUnit
  | ShapeBool => match lang_sym W "bool" NSName "Bool" with Some b => ty_eqb t (TNom b []) | None => false end
  | ShapeAddress => nominal_type "wasm" "Addr" t
  | ShapeTag => match t with TNom _ _ => true | _ => false end
  | ShapeNever => ty_eqb t never
  end.
Fixpoint forall2b {A B} (f : A -> B -> bool) (l : list A) (m : list B) : bool :=
  match l, m with [], [] => true | x :: r, y :: q => f x y && forall2b f r q | _, _ => false end.
Definition signatureb (params : list ty) (result : ty) (sg : option (list shape * shape)) : bool :=
  match sg with Some (ins, out) => forall2b value_type params ins && value_type result out | None => false end.
(* SOUND: reference_held a t = Some held <-> Platform.ReferenceType W a t held *)
Definition reference_held (array : bool) (t : ty) : option ty :=
  match wasm_sym (if array then "Array" else "Ref"), wasm_sym (if array then "Arrayed" else "Boxed"), t with
  | Some sym, Some par, TNom n args => if Nat.eqb sym n then nlook par args else None
  | _, _, _ => None end.
Definition is_ref (array : bool) (t : ty) : bool := is_some (reference_held array t).
Definition held_is (array : bool) (t h : ty) : bool :=
  match reference_held array t with Some x => ty_eqb x h | None => false end.
(* SOUND: gc_signature op ps r = true <-> Platform.GCSignature W op ps r *)
Definition gc_signature (op : prim) (ps : list ty) (r : ty) : bool :=
  let i32 := machine_type "I32" in
  match op, ps with
  | PBox sym, [h] => opt_eqb (wasm_sym "Ref") sym && held_is false r h
  | PDeref, [rf] => held_is false rf r
  | PRefSet, [rf; h] => held_is false rf h && ty_eqb r TUnit
  | PArrNew sym, [h; n] => opt_eqb (wasm_sym "Array") sym && i32 n && held_is true r h
  | PArrLen, [a] => i32 r && is_ref true a
  | PArrGet, [a; i] => i32 i && held_is true a r
  | PArrSet, [a; i; h] => i32 i && held_is true a h && ty_eqb r TUnit
  | PArrFill, [a; i; h; n] => i32 i && i32 n && held_is true a h && ty_eqb r TUnit
  | PArrCopy, [dst; di; src; si; n] => i32 di && i32 si && i32 n && ty_eqb r TUnit && is_ref true dst && is_ref true src
  | _, _ => false
  end.
Definition primitive_signature (op : prim) (ps : list ty) (r : ty) : bool :=
  match op with
  | PUnsupported _ => true
  | _ => signatureb ps r (prim_sig op) || signatureb ps r (prim_msig op) || gc_signature op ps r
  end.
(* SOUND: at_key C k op = true <-> Platform.AtKey W C k op *)
Definition at_key (C : cx) (k : item) (op : prim) : bool :=
  match k with
  | IFn f => match cfn C f with Some g => primitive_signature op (sg_params g) (sg_ret g) | None => false end
  | IMth r m _ => match cmth C r m with Some g => primitive_signature op (r :: sg_params g) (sg_ret g) | None => false end
  | _ => false end.
Definition raw_handle_read (t : ty) : bool :=
  forallb (fun nh => match wasm_sym (fst nh), wasm_sym (snd nh), t with
    | Some sym, Some raw, TNom n args => negb (Nat.eqb sym n) || oty_eqb (nlook raw args) (Some (TAbs raw))
    | _, _, _ => true end) Platform.raw_handles.
Definition handles_read (C : cx) (k : item) : bool :=
  match k with
  | IFn f => match cfn C f with Some g => forallb raw_handle_read (sg_ret g :: sg_params g) | None => true end
  | IMth r m _ => match cmth C r m with Some g => forallb raw_handle_read (sg_ret g :: r :: sg_params g) | None => true end
  | _ => true end.
Definition machines_fixed (u : nat) (inputs outputs : cx) (k : item) : bool :=
  match k with
  | IMth r m _ =>
      match cmth outputs r m with
      | Some g =>
          let gs := flat_map gens (sg_ret g :: r :: sg_params g) in
          forallb (fun nm => match wasm_sym (fst nm) with
            | Some sym => negb (nmem sym gs) || (nmem sym (fixed W fuel u) && bound_in inputs sym)
            | None => true end) initial_machines
      | None => true end
  | _ => true end.
Definition detached_method (m : nat) : bool :=
  match ast_of W m with Some (ADFn RDetached _ _ _ _) => true | _ => false end.
Definition methods_distinct (keys : list item) : bool :=
  forallb (fun k1 => forallb (fun k2 => match k1, k2 with
    | IMth _ m1 _, IMth _ m2 _ =>
        if Nat.eqb m1 m2 || (detached_method m1 && detached_method m2) then item_eqb k1 k2 else true
    | _, _ => true end) keys) keys.

(* SOUND: entry_result n t = true -> Platform.EntryResult W t *)
Fixpoint entry_result (n : nat) (t : ty) : bool :=
  match n with 0 => false | S n' =>
  match t with
  | TUnit => true
  | TUnion [] => true
  | TRecord fs => forallb (fun q => entry_result n' (snd q)) fs
  | TNom s args => match payload_of W fuel s args with Ok p => entry_result n' p | Err _ => false end
  | _ => false
  end end.

(* a message at a symbol's module, as the old oracle's `err (gs_mod g)` *)
Definition at_sym (s : nat) (m : string) : string := mod_of W s ++ ": " ++ m.
Definition tag_layout (nm par raw : string) : er unit :=
  match wasm_sym nm with
  | None => Ok tt
  | Some sym =>
      let params := ("platform tag `" ++ nm ++ "` has incompatible parameters")%string in
      match wasm_sym par, wasm_sym raw with
      | Some p, Some rw =>
          let! _ := rcheck (kind_is W sym DKTag &&
                            match w_reqs W sym with p' :: _ => Nat.eqb p p' | [] => false end) (at_sym sym params) in
          rcheck (match payload_of W fuel sym (map (fun t => (t, TAbs t)) (w_reqs W sym)) with
                  | Ok pay => ty_eqb pay (TAbs rw) | Err _ => false end)
                 (at_sym sym ("platform tag `" ++ nm ++ "` has an incompatible payload"))
      | _, _ => Err (at_sym sym params)
      end
  end.
(* SOUND: layouts = Ok tt <-> Platform.Layouts W *)
Definition layouts : er unit :=
  let present := flat_map (fun nm => match wasm_sym nm with Some s => [(nm, s)] | None => [] end) Platform.abstract_names in
  let! _ := match find (fun q => negb (kind_is W (snd q) DKType) ||
                                 negb (Nat.eqb (List.length (filter (Nat.eqb (snd q)) (map snd present))) 1)) present with
            | Some (nm, s) => Err (at_sym s ("platform type `" ++ nm ++ "` must be a distinct abstract type"))
            | None => Ok tt end in
  let! _ := tag_layout "Ref" "Boxed" "StructRef" in
  let! _ := tag_layout "Array" "Arrayed" "ArrayRef" in
  let! _ := tag_layout "Addr" "Memory" "I32" in
  match lang_sym W "bool" NSName "Bool" with
  | None => Ok tt
  | Some b =>
      let incompatible := at_sym b "platform Bool, True and False have incompatible declarations" in
      match lang_sym W "bool" NSName "True", lang_sym W "bool" NSName "False" with
      | Some tr, Some fl =>
          let! _ := rcheck (kind_is W b DKTag && kind_is W tr DKUnit && kind_is W fl DKUnit && negb (Nat.eqb tr fl) &&
                            match w_reqs W b with [] => true | _ => false end) incompatible in
          rcheck (match payload_of W fuel b [] with
                  | Ok pay => ty_eqb pay (TUnion [TNom tr []; TNom fl []]) || ty_eqb pay (TUnion [TNom fl []; TNom tr []])
                  | Err _ => false end)
                 (at_sym b "platform Bool must contain exactly True and False")
      | _, _ => Err incompatible
      end
  end.

(* casts: Platform.CastEntry has no derivation for a tag present with its
   held type or raw handle missing *)
Definition casts_ok : bool :=
  forallb (fun a : bool => match wasm_sym (if a then "Array" else "Ref") with
    | Some _ => is_some (wasm_sym (if a then "Arrayed" else "Boxed")) && is_some (wasm_sym (if a then "ArrayRef" else "StructRef"))
    | None => true end) [false; true].

(* The code of a platform family: its items' primitives, each under its
   run-time key, in its own inputs' terms.
   SOUND (with the Linked checks of the TODO): platform_code u = Ok code ->
     the code Platform.Family W P u h asks h to have. *)
Definition platform_code (u : nat) : er functor_code :=
  let nu := qs W u in
  let! (inputs, _) := ropt (w_context W u) (at_sym u ("the context of the platform functor " ++ nu ++ " does not form")) in
  let! _ := rcheck (match fun_outputs W fuel u with [] => true | _ => false end)
              (at_sym u ("the platform functor " ++ nu ++ " provides a type")) in
  let! (outputs, keys) := provide_result E u inputs (fun t => TAbs t) in
  let! _ := rcheck (inodupb (map provision_id keys)) (at_sym u ("the platform functor " ++ nu ++ " provides two items under one key")) in
  let! binds := rmap (fun k =>
      let! op := ropt (method_primitive k) (at_sym u ("the platform cannot provide " ++ qkey W k)) in
      let! _ := rcheck (at_key outputs k op) (at_sym u "platform primitive has an incompatible declared signature") in
      let! _ := rcheck (machines_fixed u inputs outputs k)
                  (at_sym u ("the platform functor " ++ nu ++ " provides " ++ qkey W k ++ ", which mentions a machine type " ++ nu ++ " is not fixed in")) in
      let! _ := rcheck (handles_read outputs k) (at_sym u "platform primitive has an incompatible declared signature") in
      Ok (BPrimitive (rkof outputs k) op)) keys in
  let! _ := rcheck (methods_distinct keys) (at_sym u ("the platform functor " ++ nu ++ " provides two items with one method")) in
  Ok (FunctorCode (map fst (c_ty inputs)) (map fst (c_ty inputs)) [] [] binds (map (rkof outputs) keys)).

(* SOUND: fixed_machines_read C u = true <-> Platform.FixedMachinesRead W C u *)
Definition fixed_machines_read (C : cx) (u : nat) : bool :=
  match cfun C u with
  | None => true
  | Some fx =>
      forallb (fun nm => match wasm_sym (fst nm) with
        | Some sym =>
            match lookup_subst sym fx with
            | Some t =>
                let c := canon C (TAbs sym) in
                ty_eqb (canon C t) c &&
                match c with TAbs _ => true | _ => false end &&
                forallb (Nat.eqb sym) (class_of C c)
            | None => true
            end
        | None => true end) initial_machines
  end.

(* main assumes an item no platform library provides: a construct the
   reference does not model (the old oracle's exit 65) *)
Definition unsupported_need (k : item) : string :=
  "unsupported: main assumes " ++ qkey W k ++ ", which no platform library provides".

(* SOUND: platform_entry k = Ok v -> PlatformEntry W P k v (P final: the
   unsupported names and the family code are recorded in the state) *)
Definition platform_entry (C : cx) (k : item) : M provision_value :=
  let decl_in s := match w_decl W s with
                   | Some d => if platform_modb (d_mod d) then Some d else None
                   | None => None end in
  match k with
  | IVal v =>
      let* d := lift (decl_in v) (unsupported_need (IVal v)) in
      (* Platform.Entry: the val's type in main's context is the machine
         type I32 (AUDIT-CHANGES #94: `val i32_one: ()` does not link) *)
      let* t := lift (cval C v) (unsupported_need (IVal v)) in
      let declared_ok := match d_ast d, w_context W v with
                         | ADVal _ a None, Some (Cv, _) =>
                             match ty_of W fuel (d_mod d) (c_ty Cv) None a with Ok t0 => machine_type "I32" t0 | Err _ => false end
                         | _, _ => false end in
      let* _ := check (machine_type "I32" t)
                 (at_sym v (if declared_ok then "platform I32 constant has an incompatible assumed type"
                            else "platform I32 constant has an incompatible declared type")) in
      match filed_name (d_ast d) with
      | Some (NSName, nm) =>
          if String.eqb nm "i32_zero" then ret (ValueProvision (VI32 0%uint63))
          else if String.eqb nm "i32_one" then ret (ValueProvision (VI32 1%uint63))
          else fail (unsupported_need (IVal v))
      | _ => fail (unsupported_need (IVal v))
      end
  | IFn f =>
      let* d := lift (decl_in f) (unsupported_need (IFn f)) in
      match d_ast d with
      | ADFn RPlain nm _ _ _ =>
          match prim_of_name nm with
          | Some pr =>
              let declared_ok := match w_context W f with
                                 | Some (Cf, _) => at_key Cf (IFn f) pr && handles_read Cf (IFn f)
                                 | None => false end in
              let* _ := check (at_key C (IFn f) pr && handles_read C (IFn f))
                         (at_sym f (if declared_ok then "platform primitive has an incompatible assumed signature"
                                    else "platform primitive has an incompatible declared signature")) in
              ret (PrimitiveClosure pr None)
          | None => let* i := unsupported_index nm in ret (PrimitiveClosure (PUnsupported i) None)
          end
      | _ => fail (unsupported_need (IFn f))
      end
  | IFun u =>
      let* d := lift (decl_in u) (unsupported_need (IFun u)) in
      match d_ast d with
      | ADFunctor _ _ _ _ None =>
          let* _ := check (fixed_machines_read C u) (at_sym u "platform functor has an incompatible assumed fixed type") in
          let* code := pure (platform_code u) in
          let* h := fresh in
          let* _ := set_fun_body h code in
          ret (FunctorClosure h None empty_environment None)
      | _ => fail (unsupported_need (IFun u))
      end
  | _ => fail (unsupported_need k)
  end.

(* SOUND: main_environment main = Ok env -> the ElaboratesIn clause on
   program_environment (CtxOf, InitialTypes, PlatformEntry for each need) *)
Definition main_environment (main : nat) : M environment :=
  let* (C, ns) := lift (w_context W main) "the context of `main` does not form" in
  let types := map (fun q => (fst q, app_subst (initial_machine_subst W) (snd q))) (c_ty C) in
  let* platform := mmap (fun k => let* v := platform_entry C (rkof C k) in ret (rkof C k, v)) ns in
  ret (Environment types platform).

(* SOUND: Platform.Representations W P when program_casts P = casts *)
Definition cast_entry (array : bool) : list (nsym * cast_spec) :=
  match wasm_sym (if array then "Array" else "Ref"),
        wasm_sym (if array then "Arrayed" else "Boxed"),
        wasm_sym (if array then "ArrayRef" else "StructRef") with
  | Some n, Some t, Some h => [(n, CastSpec array t (Some (h, TMachine (if array then MArrayRef else MStructRef))))]
  | _, _, _ => []
  end.
Definition casts : list (nsym * cast_spec) := cast_entry false ++ cast_entry true.

(* SOUND: the ElaboratesIn clause on Bool's symbols *)
Definition bool_syms : nat * nat * nat :=
  match lang_sym W "bool" NSName "Bool", lang_sym W "bool" NSName "True", lang_sym W "bool" NSName "False" with
  | Some b, Some t, Some f => (b, t, f)
  | _, _, _ => (0, 0, 0)
  end.

End Platform.
