(* The primitives Audit's interpreter runs with: MossX.run_prim (the
   machine model of ExecutionModel.v's primitives), at Audit's value type.
   UNVERIFIED glue (README.md): the two value types are declared alike,
   and the conversions below map them constructor for constructor. *)
open ExecutionModel
module A = Audit.Semantics

(* Field names: MossX's are numbers, Audit's are strings.  A decimal string
   is that number; any other name is interned to a negative number, so a
   value crossing the primitive boundary keeps its names both ways. *)
let fld_names : (string, int) Hashtbl.t = Hashtbl.create 64
let fld_ids : (int, string) Hashtbl.t = Hashtbl.create 64
let fld f = if f < 0 then Hashtbl.find fld_ids f else string_of_int f
let fld_back s = match int_of_string_opt s with
  | Some n -> n
  | None -> (match Hashtbl.find_opt fld_names s with
             | Some n -> n
             | None -> let n = - (Hashtbl.length fld_names + 1) in
                       Hashtbl.add fld_names s n; Hashtbl.add fld_ids n s; n)

(* Audit's machine and record types have no ExecutionModel form; they cross
   the primitive boundary (a checked handle's held type) encoded, as a
   negative symbol and a nominal -1000 over interned field names. *)
let rec ty (t : ExecutionModel.ty) : A.ty = match t with
  | TUnit -> A.TUnit
  | TAbs s when s < 0 -> A.TMachine (-1 - s)
  | TAbs s -> A.TAbs s
  | TNom (n, th) when n = -1000 -> A.TRecord (List.map (fun (f, t) -> (fld f, ty t)) th)
  | TNom (n, th) -> A.TNom (n, List.map (fun (s, t) -> (s, ty t)) th)
  | TUnion ts -> A.TUnion (List.map ty ts)
let rec ty_back (t : A.ty) : ExecutionModel.ty = match t with
  | A.TUnit -> TUnit | A.TAbs s -> TAbs s
  | A.TNom (n, th) -> TNom (n, List.map (fun (s, t) -> (s, ty_back t)) th)
  | A.TUnion ts -> TUnion (List.map ty_back ts)
  | A.TMachine s -> TAbs (-1 - s)
  | A.TRecord fs -> TNom (-1000, List.map (fun (f, t) -> (fld_back f, ty_back t)) fs)

(* The two prim types (and their operand enumerations) are declared
   constructor for constructor alike in ExecutionModel.v and Audit.v, so
   they have one representation. *)
let prim_back (p : A.prim) : ExecutionModel.prim = Obj.magic p

let rec value (w : xvalue) : A.value = match w with
  | XVUnit -> A.VUnit | XVTag (n, w) -> A.VTag (n, value w) | XVI32 i -> A.VI32 i
  | XVF64 f -> A.VF64 f | XVRec fs -> A.VRecord (List.map (fun (f, w) -> (fld f, value w)) fs)
  | XVF32 f -> A.VF32 f | XVI64 z -> A.VI64 z
  | XVRef a -> A.VHandle (snd (Uint63.to_int2 a))
  | XVRefAt (k, t, a) -> A.VCheckedHandle (k, ty t, snd (Uint63.to_int2 a))
let rec value_back (w : A.value) : xvalue = match w with
  | A.VUnit -> XVUnit | A.VTag (n, w) -> XVTag (n, value_back w) | A.VI32 i -> XVI32 i
  | A.VF64 f -> XVF64 f | A.VRecord fs -> XVRec (List.map (fun (f, w) -> (fld_back f, value_back w)) fs)
  | A.VF32 f -> XVF32 f | A.VI64 z -> XVI64 z
  | A.VHandle a -> XVRef (Uint63.of_int a)
  | A.VCheckedHandle (k, t, a) -> XVRefAt (k, ty_back t, Uint63.of_int a)

(* MossX.run_prim reads the host (argv, files, Bool's symbols) from an
   ExecutionModel xprog; the driver sets it before the run.  Its program
   tables are empty: no primitive reads them. *)
let host : xprog option ref = ref None

module P = struct
  type mstate = MossX.mstate
  let raw_primitive (_ : A.program_view) (p : A.prim) (ws : A.value list) (st : mstate) : A.signal * mstate =
    let xp = match !host with Some x -> x | None -> assert false in
    let (o, st') = MossX.run_prim_fast xp (prim_back p) (List.map value_back ws) st in
    ((match o with
      | XOk w -> A.Normal (value w) | XRet w -> A.Returning (value w) | XBrk -> A.Breaking
      | XErr n -> A.Stuck n | XTrap n -> A.Trap n | XExit c -> A.Exit c | XUns n -> A.UnsupportedOperation n
      | XFuel -> A.OutOfFuel), st')
  let run_prim program operation site values state =
    A.BaseCall.call raw_primitive program operation site values state
end
