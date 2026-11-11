(* The concrete Base platform: Wasm/WASI operations, source-library entry
   provisions, reference headers, heap laws and the public specialization.
   Audit.v defines the generic language and public proof obligations. *)
From Stdlib Require Import PrimInt63 ZArith String Ascii List Bool Arith.
From NM Require Import Audit.
Import Audit.Semantics ListNotations.
Set Implicit Arguments.
Local Open Scope nat_scope.

Module WasmOps.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* ===================== Primitives ======================================== *)

Inductive i32bin := IAdd | ISub | IMul | IDivS | IDivU | IRemS | IRemU
                  | IAnd | IOr | IXor | IShl | IShrS | IShrU | IRotl | IRotr.
Inductive i32cmp := CEq | CNe | CLtS | CLtU | CGtS | CGtU | CLeS | CLeU
                  | CGeS | CGeU.
Inductive i32un  := UEqz | UClz | UCtz | UPopcnt | UExt8 | UExt16.
Inductive f64bin := FAdd | FSub | FMul | FDiv | FMin | FMax | FCopysign.
Inductive f64un  := FAbs | FNeg | FCeil | FFloor | FTrunc | FNearest | FSqrt.
Inductive f64cmp := FEq | FNe | FLt | FGt | FLe | FGe.
Inductive conv   := CTruncS | CTruncU | CTruncSatS | CTruncSatU
                  | CConvS | CConvU.     (* i32 <-> f64 *)
Inductive loadk  := L32 | L8S | L8U | L16S | L16U.
Inductive storek := S32 | S8 | S16.
Inductive i64un  := U64Eqz | U64Clz | U64Ctz | U64Popcnt | U64Ext8 | U64Ext16 | U64Ext32.
Inductive load64k := L64 | L64_8S | L64_8U | L64_16S | L64_16U | L64_32S | L64_32U.
Inductive store64k := S64 | S64_8 | S64_16 | S64_32.
(* i64 <-> floats: trunc (s/u, sat or not) from f64/f32, convert to f64/f32 *)
Inductive conv64 := C64Trunc (from32 : bool) (signed : bool) (sat : bool)
                  | C64Conv (to32 : bool) (signed : bool).

Inductive wasm_prim : Type :=
| PI32Bin  : i32bin -> wasm_prim
| PI32Cmp  : i32cmp -> wasm_prim
| PI32Un   : i32un -> wasm_prim
| PF64Bin  : f64bin -> wasm_prim
| PF64Cmp  : f64cmp -> wasm_prim
| PF64Un   : f64un -> wasm_prim
| PConv    : conv -> wasm_prim
| PLoad    : loadk -> wasm_prim
| PStore   : storek -> wasm_prim
| PF64Load | PF64Store
| PMemSize | PMemGrow | PMemCopy | PMemFill
| PUnreachable
| PFdWrite | PProcExit | PArgsSizesGet | PArgsGet | PPathOpen | PFdRead | PFdClose
| PI32Bool
(* f32: every result rounded to binary32 *)
| PF32Bin  : f64bin -> wasm_prim
| PF32Cmp  : f64cmp -> wasm_prim
| PF32Un   : f64un -> wasm_prim
| PConv32  : conv -> wasm_prim               (* i32 <-> f32 *)
| PF32Load | PF32Store | PDemote | PPromote | PI32ReinterpF32 | PF32ReinterpI32
(* i64 *)
| PI64Bin  : i32bin -> wasm_prim
| PI64Cmp  : i32cmp -> wasm_prim
| PI64Un   : i64un -> wasm_prim
| PWrap | PExtendS | PExtendU
| PI64Load : load64k -> wasm_prim
| PI64Store : store64k -> wasm_prim
| PConv64  : conv64 -> wasm_prim
| PI64ReinterpF64 | PF64ReinterpI64
(* D75/D81: what `bind Heap;`, `bind Mutable;`, `bind Arrays;` provide;
   PBox and PArrNew carry the `Ref` and `Array` tags *)
| PBox : nsym -> wasm_prim | PDeref | PRefSet
| PArrNew : nsym -> wasm_prim | PArrLen | PArrGet | PArrSet | PArrFill | PArrCopy.

(* Which library fn is which primitive: the elaborator gives `main`'s need of
   a lib/wasm.moss or lib/wasip1.moss fn named n the primitive
   `prim_of_name n`.  An auditor checks this table
   against lib/wasm.moss's and lib/wasip1.moss's names. *)
Section PrimNames.
Local Open Scope string_scope.
Definition strip_prefix (pre s : string) : option string :=
  if String.prefix pre s then Some (substring (String.length pre) (String.length s - String.length pre) s) else None.

Definition i32bin_of (s : string) : option i32bin :=
  match s with
  | "add" => Some IAdd | "sub" => Some ISub | "mul" => Some IMul | "div_s" => Some IDivS
  | "div_u" => Some IDivU | "rem_s" => Some IRemS | "rem_u" => Some IRemU | "and" => Some IAnd
  | "or" => Some IOr | "xor" => Some IXor | "shl" => Some IShl | "shr_s" => Some IShrS
  | "shr_u" => Some IShrU | "rotl" => Some IRotl | "rotr" => Some IRotr | _ => None
  end.
Definition i32cmp_of (s : string) : option i32cmp :=
  match s with
  | "eq" => Some CEq | "ne" => Some CNe | "lt_s" => Some CLtS | "lt_u" => Some CLtU
  | "gt_s" => Some CGtS | "gt_u" => Some CGtU | "le_s" => Some CLeS | "le_u" => Some CLeU
  | "ge_s" => Some CGeS | "ge_u" => Some CGeU | _ => None
  end.
Definition i32un_of (s : string) : option i32un :=
  match s with
  | "eqz" => Some UEqz | "clz" => Some UClz | "ctz" => Some UCtz | "popcnt" => Some UPopcnt
  | "extend8_s" => Some UExt8 | "extend16_s" => Some UExt16 | _ => None
  end.
Definition f64bin_of (s : string) : option f64bin :=
  match s with
  | "add" => Some FAdd | "sub" => Some FSub | "mul" => Some FMul | "div" => Some FDiv
  | "min" => Some FMin | "max" => Some FMax | "copysign" => Some FCopysign | _ => None
  end.
Definition f64un_of (s : string) : option f64un :=
  match s with
  | "abs" => Some FAbs | "neg" => Some FNeg | "ceil" => Some FCeil | "floor" => Some FFloor
  | "trunc" => Some FTrunc | "nearest" => Some FNearest | "sqrt" => Some FSqrt | _ => None
  end.
Definition f64cmp_of (s : string) : option f64cmp :=
  match s with
  | "eq" => Some FEq | "ne" => Some FNe | "lt" => Some FLt | "gt" => Some FGt
  | "le" => Some FLe | "ge" => Some FGe | _ => None
  end.

Definition prim_other (s : string) : option wasm_prim :=
  match s with
  | "i32_load" => Some (PLoad L32) | "i32_load8_s" => Some (PLoad L8S) | "i32_load8_u" => Some (PLoad L8U)
  | "i32_load16_s" => Some (PLoad L16S) | "i32_load16_u" => Some (PLoad L16U)
  | "i32_store" => Some (PStore S32) | "i32_store8" => Some (PStore S8) | "i32_store16" => Some (PStore S16)
  | "f64_load" => Some PF64Load | "f64_store" => Some PF64Store
  | "memory_size" => Some PMemSize | "memory_grow" => Some PMemGrow
  | "memory_copy" => Some PMemCopy | "memory_fill" => Some PMemFill
  | "unreachable" => Some PUnreachable | "fd_write" => Some PFdWrite | "proc_exit" => Some PProcExit
  | "args_sizes_get" => Some PArgsSizesGet | "args_get" => Some PArgsGet
  | "path_open" => Some PPathOpen | "fd_read" => Some PFdRead | "fd_close" => Some PFdClose
  | "i32_bool" => Some PI32Bool
  | "i32_trunc_f64_s" => Some (PConv CTruncS) | "i32_trunc_f64_u" => Some (PConv CTruncU)
  | "i32_trunc_sat_f64_s" => Some (PConv CTruncSatS) | "i32_trunc_sat_f64_u" => Some (PConv CTruncSatU)
  | "f64_convert_i32_s" => Some (PConv CConvS) | "f64_convert_i32_u" => Some (PConv CConvU)
  | "i32_trunc_f32_s" => Some (PConv32 CTruncS) | "i32_trunc_f32_u" => Some (PConv32 CTruncU)
  | "i32_trunc_sat_f32_s" => Some (PConv32 CTruncSatS) | "i32_trunc_sat_f32_u" => Some (PConv32 CTruncSatU)
  | "f32_convert_i32_s" => Some (PConv32 CConvS) | "f32_convert_i32_u" => Some (PConv32 CConvU)
  | "f32_load" => Some PF32Load | "f32_store" => Some PF32Store | "f32_demote_f64" => Some PDemote
  | "f64_promote_f32" => Some PPromote | "i32_reinterpret_f32" => Some PI32ReinterpF32
  | "f32_reinterpret_i32" => Some PF32ReinterpI32
  | "i64_eqz" => Some (PI64Un U64Eqz) | "i64_clz" => Some (PI64Un U64Clz) | "i64_ctz" => Some (PI64Un U64Ctz)
  | "i64_popcnt" => Some (PI64Un U64Popcnt) | "i64_extend8_s" => Some (PI64Un U64Ext8)
  | "i64_extend16_s" => Some (PI64Un U64Ext16) | "i64_extend32_s" => Some (PI64Un U64Ext32)
  | "i32_wrap_i64" => Some PWrap | "i64_extend_i32_s" => Some PExtendS | "i64_extend_i32_u" => Some PExtendU
  | "i64_load" => Some (PI64Load L64) | "i64_load8_s" => Some (PI64Load L64_8S) | "i64_load8_u" => Some (PI64Load L64_8U)
  | "i64_load16_s" => Some (PI64Load L64_16S) | "i64_load16_u" => Some (PI64Load L64_16U)
  | "i64_load32_s" => Some (PI64Load L64_32S) | "i64_load32_u" => Some (PI64Load L64_32U)
  | "i64_store" => Some (PI64Store S64) | "i64_store8" => Some (PI64Store S64_8)
  | "i64_store16" => Some (PI64Store S64_16) | "i64_store32" => Some (PI64Store S64_32)
  | "i64_reinterpret_f64" => Some PI64ReinterpF64 | "f64_reinterpret_i64" => Some PF64ReinterpI64
  | "i64_trunc_f64_s" => Some (PConv64 (C64Trunc false true false))
  | "i64_trunc_f64_u" => Some (PConv64 (C64Trunc false false false))
  | "i64_trunc_f32_s" => Some (PConv64 (C64Trunc true true false))
  | "i64_trunc_f32_u" => Some (PConv64 (C64Trunc true false false))
  | "i64_trunc_sat_f64_s" => Some (PConv64 (C64Trunc false true true))
  | "i64_trunc_sat_f64_u" => Some (PConv64 (C64Trunc false false true))
  | "i64_trunc_sat_f32_s" => Some (PConv64 (C64Trunc true true true))
  | "i64_trunc_sat_f32_u" => Some (PConv64 (C64Trunc true false true))
  | "f64_convert_i64_s" => Some (PConv64 (C64Conv false true))
  | "f64_convert_i64_u" => Some (PConv64 (C64Conv false false))
  | "f32_convert_i64_s" => Some (PConv64 (C64Conv true true))
  | "f32_convert_i64_u" => Some (PConv64 (C64Conv true false))
  | _ => None
  end.

Definition prim_of_name (s : string) : option wasm_prim :=
  match prim_other s with
  | Some p => Some p
  | None =>
      let try_ {A} (pre : string) (f : string -> option A) (k : A -> wasm_prim) : option wasm_prim :=
        match strip_prefix pre s with
        | Some r => option_map k (f r)
        | None => None
        end in
      let first (l : list (option wasm_prim)) := fold_right (fun o acc => match o with Some _ => o | None => acc end) None l in
      first [try_ "i32_" i32bin_of PI32Bin; try_ "i32_" i32cmp_of PI32Cmp; try_ "i32_" i32un_of PI32Un;
             try_ "f64_" f64bin_of PF64Bin; try_ "f64_" f64cmp_of PF64Cmp; try_ "f64_" f64un_of PF64Un;
             try_ "f32_" f64bin_of PF32Bin; try_ "f32_" f64cmp_of PF32Cmp; try_ "f32_" f64un_of PF32Un;
             try_ "i64_" i32bin_of PI64Bin; try_ "i64_" i32cmp_of PI64Cmp]
  end.

End PrimNames.

Definition i32bin_name (p : i32bin) : string :=
  match p with
  | IAdd => "add"
  | ISub => "sub"
  | IMul => "mul"
  | IDivS => "div_s"
  | IDivU => "div_u"
  | IRemS => "rem_s"
  | IRemU => "rem_u"
  | IAnd => "and"
  | IOr => "or"
  | IXor => "xor"
  | IShl => "shl"
  | IShrS => "shr_s"
  | IShrU => "shr_u"
  | IRotl => "rotl"
  | IRotr => "rotr"
  end.

Definition i32cmp_name (p : i32cmp) : string :=
  match p with
  | CEq => "eq"
  | CNe => "ne"
  | CLtS => "lt_s"
  | CLtU => "lt_u"
  | CGtS => "gt_s"
  | CGtU => "gt_u"
  | CLeS => "le_s"
  | CLeU => "le_u"
  | CGeS => "ge_s"
  | CGeU => "ge_u"
  end.

Definition i32un_name (p : i32un) : string :=
  match p with
  | UEqz => "eqz"
  | UClz => "clz"
  | UCtz => "ctz"
  | UPopcnt => "popcnt"
  | UExt8 => "extend8_s"
  | UExt16 => "extend16_s"
  end.

Definition f64bin_name (p : f64bin) : string :=
  match p with
  | FAdd => "add"
  | FSub => "sub"
  | FMul => "mul"
  | FDiv => "div"
  | FMin => "min"
  | FMax => "max"
  | FCopysign => "copysign"
  end.

Definition f64cmp_name (p : f64cmp) : string :=
  match p with
  | FEq => "eq"
  | FNe => "ne"
  | FLt => "lt"
  | FGt => "gt"
  | FLe => "le"
  | FGe => "ge"
  end.

Definition f64un_name (p : f64un) : string :=
  match p with
  | FAbs => "abs"
  | FNeg => "neg"
  | FCeil => "ceil"
  | FFloor => "floor"
  | FTrunc => "trunc"
  | FNearest => "nearest"
  | FSqrt => "sqrt"
  end.

Definition primitive_name (p : wasm_prim) : string :=
  match p with
  | PI32Bin op => "i32_" ++ i32bin_name op
  | PI32Cmp op => "i32_" ++ i32cmp_name op
  | PI32Un op => "i32_" ++ i32un_name op
  | PI64Bin op => "i64_" ++ i32bin_name op
  | PI64Cmp op => "i64_" ++ i32cmp_name op
  | PF32Bin op => "f32_" ++ f64bin_name op
  | PF32Cmp op => "f32_" ++ f64cmp_name op
  | PF32Un op => "f32_" ++ f64un_name op
  | PF64Bin op => "f64_" ++ f64bin_name op
  | PF64Cmp op => "f64_" ++ f64cmp_name op
  | PF64Un op => "f64_" ++ f64un_name op
  | PLoad L32 => "i32_load"
  | PLoad L8S => "i32_load8_s"
  | PLoad L8U => "i32_load8_u"
  | PLoad L16S => "i32_load16_s"
  | PLoad L16U => "i32_load16_u"
  | PStore S32 => "i32_store"
  | PStore S8 => "i32_store8"
  | PStore S16 => "i32_store16"
  | PF64Load => "f64_load"
  | PF64Store => "f64_store"
  | PMemSize => "memory_size"
  | PMemGrow => "memory_grow"
  | PMemCopy => "memory_copy"
  | PMemFill => "memory_fill"
  | PUnreachable => "unreachable"
  | PFdWrite => "fd_write"
  | PProcExit => "proc_exit"
  | PArgsSizesGet => "args_sizes_get"
  | PArgsGet => "args_get"
  | PPathOpen => "path_open"
  | PFdRead => "fd_read"
  | PFdClose => "fd_close"
  | PI32Bool => "i32_bool"
  | PConv CTruncS => "i32_trunc_f64_s"
  | PConv CTruncU => "i32_trunc_f64_u"
  | PConv CTruncSatS => "i32_trunc_sat_f64_s"
  | PConv CTruncSatU => "i32_trunc_sat_f64_u"
  | PConv CConvS => "f64_convert_i32_s"
  | PConv CConvU => "f64_convert_i32_u"
  | PConv32 CTruncS => "i32_trunc_f32_s"
  | PConv32 CTruncU => "i32_trunc_f32_u"
  | PConv32 CTruncSatS => "i32_trunc_sat_f32_s"
  | PConv32 CTruncSatU => "i32_trunc_sat_f32_u"
  | PConv32 CConvS => "f32_convert_i32_s"
  | PConv32 CConvU => "f32_convert_i32_u"
  | PF32Load => "f32_load"
  | PF32Store => "f32_store"
  | PDemote => "f32_demote_f64"
  | PPromote => "f64_promote_f32"
  | PI32ReinterpF32 => "i32_reinterpret_f32"
  | PF32ReinterpI32 => "f32_reinterpret_i32"
  | PI64Un U64Eqz => "i64_eqz"
  | PI64Un U64Clz => "i64_clz"
  | PI64Un U64Ctz => "i64_ctz"
  | PI64Un U64Popcnt => "i64_popcnt"
  | PI64Un U64Ext8 => "i64_extend8_s"
  | PI64Un U64Ext16 => "i64_extend16_s"
  | PI64Un U64Ext32 => "i64_extend32_s"
  | PWrap => "i32_wrap_i64"
  | PExtendS => "i64_extend_i32_s"
  | PExtendU => "i64_extend_i32_u"
  | PI64Load L64 => "i64_load"
  | PI64Load L64_8S => "i64_load8_s"
  | PI64Load L64_8U => "i64_load8_u"
  | PI64Load L64_16S => "i64_load16_s"
  | PI64Load L64_16U => "i64_load16_u"
  | PI64Load L64_32S => "i64_load32_s"
  | PI64Load L64_32U => "i64_load32_u"
  | PI64Store S64 => "i64_store"
  | PI64Store S64_8 => "i64_store8"
  | PI64Store S64_16 => "i64_store16"
  | PI64Store S64_32 => "i64_store32"
  | PI64ReinterpF64 => "i64_reinterpret_f64"
  | PF64ReinterpI64 => "f64_reinterpret_i64"
  | PConv64 (C64Trunc false true false) => "i64_trunc_f64_s"
  | PConv64 (C64Trunc false false false) => "i64_trunc_f64_u"
  | PConv64 (C64Trunc true true false) => "i64_trunc_f32_s"
  | PConv64 (C64Trunc true false false) => "i64_trunc_f32_u"
  | PConv64 (C64Trunc false true true) => "i64_trunc_sat_f64_s"
  | PConv64 (C64Trunc false false true) => "i64_trunc_sat_f64_u"
  | PConv64 (C64Trunc true true true) => "i64_trunc_sat_f32_s"
  | PConv64 (C64Trunc true false true) => "i64_trunc_sat_f32_u"
  | PConv64 (C64Conv false true) => "f64_convert_i64_s"
  | PConv64 (C64Conv false false) => "f64_convert_i64_u"
  | PConv64 (C64Conv true true) => "f32_convert_i64_s"
  | PConv64 (C64Conv true false) => "f32_convert_i64_u"
  | PBox _ => "Ref.boxed" | PDeref => "Ref.deref" | PRefSet => "Ref.set"
  | PArrNew _ => "Array.arrayed" | PArrLen => "Array.length" | PArrGet => "Array.get"
  | PArrSet => "Array.set" | PArrFill => "Array.fill" | PArrCopy => "Array.copy"
  end.

Definition encode (p : wasm_prim) : op :=
  Op (primitive_name p) (match p with PBox n | PArrNew n => [n] | _ => [] end).

Definition gc_names : list (string * wasm_prim) :=
  [("Ref.deref", PDeref); ("Ref.set", PRefSet); ("Array.length", PArrLen);
   ("Array.get", PArrGet); ("Array.set", PArrSet); ("Array.fill", PArrFill);
   ("Array.copy", PArrCopy)].

Definition decode_candidate (operation : op) : option wasm_prim :=
  let name := op_name operation in
  match op_syms operation with
  | [tag] =>
      if String.eqb name "Ref.boxed" then Some (PBox tag)
      else if String.eqb name "Array.arrayed" then Some (PArrNew tag)
      else None
  | [] =>
      match find (fun entry => String.eqb name (fst entry)) gc_names with
      | Some (_, p) => Some p
      | None => prim_of_name name
      end
  | _ => None
  end.

Fixpoint operation_symbols_eqb (a b : list nat) : bool :=
  match a, b with
  | [], [] => true
  | x :: xs, y :: ys => Nat.eqb x y && operation_symbols_eqb xs ys
  | _, _ => false
  end.

Definition operation_eqb (a b : op) : bool :=
  String.eqb (op_name a) (op_name b) && operation_symbols_eqb (op_syms a) (op_syms b).

Definition decode_operation (operation : op) : option wasm_prim :=
  match decode_candidate operation with
  | Some p => if operation_eqb operation (encode p) then Some p else None
  | None => None
  end.

Definition decode := decode_operation.

(* Two operands reserve the Base encoding for an unsupported operation.
   Its first operand is the exact index in the program's diagnostic table. *)
Definition unsupported_operation (name : string) (index : nat) : op :=
  Op name [index; 0].
Definition unsupported_code (operation : op) : nat :=
  match op_syms operation with [index; 0] => index | _ => 0 end.

End WasmOps.
Export WasmOps.

(* Base's raw host calling convention. Source wrappers are unwrapped on
   input, and fresh references receive the allocation site's type header. *)
Module BaseCall.
Definition raw_receiver (w : value) : value :=
  match w with
  | VTag n (VRef a (Some _)) => VTag n (VRef a None)
  | _ => w
  end.
Definition primitive_arguments (p : wasm_prim) (ws : list value) : list value :=
  match p, ws with
  | PArrCopy, [r; i; s; j; count] => [raw_receiver r; i; raw_receiver s; j; count]
  | PDeref, r :: args | PRefSet, r :: args | PArrLen, r :: args
  | PArrGet, r :: args | PArrSet, r :: args | PArrFill, r :: args => raw_receiver r :: args
  | _, _ => ws
  end.
Definition memory_operation (p : wasm_prim) : bool :=
  match p with
  | PLoad _ | PStore _ | PF64Load | PF64Store | PF32Load | PF32Store
  | PI64Load _ | PI64Store _ | PMemSize | PMemGrow | PMemCopy | PMemFill => true
  | _ => false
  end.

Definition uses_site (p : wasm_prim) : bool :=
  match p with PBox _ | PArrNew _ => true | _ => memory_operation p end.

(* The Base raw host ABI prefixes memory calls with the resolved receiver
   type. A reference header carries the type through the existing value
   interface. This argument is consumed by the host and never dereferenced. *)
Definition memory_arguments (p : wasm_prim) (site : unit -> option ty)
    (values : list value) : list value :=
  if memory_operation p
  then VRef 0 (option_map (fun t => (0, t)) (site tt)) :: values
  else values.

Definition split_memory_arguments (p : wasm_prim) (values : list value) : option ty * list value :=
  if memory_operation p then
    match values with
    | VRef _ header :: rest => (option_map snd header, rest)
    | _ => (None, values)
    end
  else (None, values).

Definition arguments_at_site (p : wasm_prim) (site : unit -> option ty)
    (values : list value) : list value :=
  memory_arguments p site (primitive_arguments p values).

Definition allocation_result (p : wasm_prim) (tau : ty) (o : signal) : signal :=
  match p, o with
  | PBox _, Normal (VTag n (VRef a None)) => Normal (VTag n (VRef a (Some (0, tau))))
  | PArrNew _, Normal (VTag n (VRef a None)) => Normal (VTag n (VRef a (Some (1, tau))))
  | _, _ => o
  end.
Definition call {S : Type}
    (primitive : program_view -> op -> list value -> S -> signal * S)
    (P : program_view) (operation : op) (ot : option ty)
    (ws : list value) (st : S) : signal * S :=
  let decoded := decode operation in
  let arguments := match decoded with Some p => arguments_at_site p (fun _ => ot) ws | None => ws end in
  let '(o, st') := primitive P operation arguments st in
  (match ot, decoded with Some tau, Some p => allocation_result p tau o | _, _ => o end, st').

End BaseCall.

Module WasmSignatures.
Inductive shape : Type := ShapeI32 | ShapeI64 | ShapeF32 | ShapeF64 | ShapeUnit | ShapeBool
(* Track B: an address as `Memories`' members receive it (an `Addr`, a tag
   over an I32), and any tag (the memory a member is a method of) *)
| ShapeAddress | ShapeTag
(* no value: the result of an operation declared to return `|`, so the
   result law (`primitive_result`) forbids it to return normally *)
| ShapeNever.

Definition value_shape (P : program_view) (w : value) (t : shape) : Prop :=
  match t, w with
  | ShapeI32, VScalar 0 _ | ShapeI64, VScalar 1 _ | ShapeF32, VScalar 2 _ | ShapeF64, VScalar 3 _ => True
  | ShapeUnit, VUnit => True
  | ShapeBool, VTag b (VTag t' VUnit) => b = view_bool P /\ (t' = view_true P \/ t' = view_false P)
  | ShapeAddress, VTag _ (VScalar 0 _) => True
  | ShapeTag, VTag _ _ => True
  | ShapeNever, _ => False
  | _, _ => False
  end.

(* The signatures of lib/wasm.moss, lib/wasip1.moss and wasistd's i32_bool
   (Moss's I32 / I64 / F32 / F64 / () / Bool).  The GC primitives and the
   unmodelled ones have none here: their results depend on the heap. *)
Definition prim_sig (p : wasm_prim) : option (list shape * shape) :=
  match p with
  | PI32Bin _ | PI32Cmp _ => Some ([ShapeI32; ShapeI32], ShapeI32)
  | PI32Un _ => Some ([ShapeI32], ShapeI32)
  | PF64Bin _ => Some ([ShapeF64; ShapeF64], ShapeF64)
  | PF64Cmp _ => Some ([ShapeF64; ShapeF64], ShapeI32)
  | PF64Un _ => Some ([ShapeF64], ShapeF64)
  | PConv (CTruncS | CTruncU | CTruncSatS | CTruncSatU) => Some ([ShapeF64], ShapeI32)
  | PConv (CConvS | CConvU) => Some ([ShapeI32], ShapeF64)
  | PLoad _ => Some ([ShapeI32], ShapeI32)
  | PStore _ => Some ([ShapeI32; ShapeI32], ShapeUnit)
  | PF64Load => Some ([ShapeI32], ShapeF64)
  | PF64Store => Some ([ShapeI32; ShapeF64], ShapeUnit)
  | PMemSize => Some ([], ShapeI32)
  | PMemGrow => Some ([ShapeI32], ShapeI32)
  | PMemCopy | PMemFill => Some ([ShapeI32; ShapeI32; ShapeI32], ShapeUnit)
  | PUnreachable => Some ([], ShapeNever)
  | PFdWrite => Some ([ShapeI32; ShapeI32; ShapeI32; ShapeI32], ShapeI32)
  | PProcExit => Some ([ShapeI32], ShapeUnit)
  | PArgsSizesGet | PArgsGet => Some ([ShapeI32; ShapeI32], ShapeI32)
  | PPathOpen => Some ([ShapeI32; ShapeI32; ShapeI32; ShapeI32; ShapeI32; ShapeI64; ShapeI64; ShapeI32; ShapeI32], ShapeI32)
  | PFdRead => Some ([ShapeI32; ShapeI32; ShapeI32; ShapeI32], ShapeI32)
  | PFdClose => Some ([ShapeI32], ShapeI32)
  | PI32Bool => Some ([ShapeI32], ShapeBool)
  | PF32Bin _ => Some ([ShapeF32; ShapeF32], ShapeF32)
  | PF32Cmp _ => Some ([ShapeF32; ShapeF32], ShapeI32)
  | PF32Un _ => Some ([ShapeF32], ShapeF32)
  | PConv32 (CTruncS | CTruncU | CTruncSatS | CTruncSatU) => Some ([ShapeF32], ShapeI32)
  | PConv32 (CConvS | CConvU) => Some ([ShapeI32], ShapeF32)
  | PF32Load => Some ([ShapeI32], ShapeF32)
  | PF32Store => Some ([ShapeI32; ShapeF32], ShapeUnit)
  | PDemote => Some ([ShapeF64], ShapeF32)
  | PPromote => Some ([ShapeF32], ShapeF64)
  | PI32ReinterpF32 => Some ([ShapeF32], ShapeI32)
  | PF32ReinterpI32 => Some ([ShapeI32], ShapeF32)
  | PI64Bin _ => Some ([ShapeI64; ShapeI64], ShapeI64)
  | PI64Cmp _ => Some ([ShapeI64; ShapeI64], ShapeI32)
  | PI64Un U64Eqz => Some ([ShapeI64], ShapeI32)
  | PI64Un _ => Some ([ShapeI64], ShapeI64)
  | PWrap => Some ([ShapeI64], ShapeI32)
  | PExtendS | PExtendU => Some ([ShapeI32], ShapeI64)
  | PI64Load _ => Some ([ShapeI32], ShapeI64)
  | PI64Store _ => Some ([ShapeI32; ShapeI64], ShapeUnit)
  | PConv64 (C64Trunc f32 _ _) => Some ([if f32 then ShapeF32 else ShapeF64], ShapeI64)
  | PConv64 (C64Conv to32 _) => Some ([ShapeI64], if to32 then ShapeF32 else ShapeF64)
  | PI64ReinterpF64 => Some ([ShapeF64], ShapeI64)
  | PF64ReinterpI64 => Some ([ShapeI64], ShapeF64)
  | PBox _ | PDeref | PRefSet | PArrNew _ | PArrLen | PArrGet | PArrSet
  | PArrFill | PArrCopy => None
  end.

(* Track B.  The memory primitives as the platform provides them, as
   methods of `Memories` (D79): an address
   argument is an `Addr`, and memory.size and memory.grow receive the
   memory they are a method of. The host must implement these source
   calling conventions as well as the plain primitive signatures. *)
Definition prim_msig (p : wasm_prim) : option (list shape * shape) :=
  match p with
  | PLoad _ => Some ([ShapeAddress], ShapeI32)
  | PStore _ => Some ([ShapeAddress; ShapeI32], ShapeUnit)
  | PF64Load => Some ([ShapeAddress], ShapeF64)
  | PF64Store => Some ([ShapeAddress; ShapeF64], ShapeUnit)
  | PF32Load => Some ([ShapeAddress], ShapeF32)
  | PF32Store => Some ([ShapeAddress; ShapeF32], ShapeUnit)
  | PI64Load _ => Some ([ShapeAddress], ShapeI64)
  | PI64Store _ => Some ([ShapeAddress; ShapeI64], ShapeUnit)
  | PMemCopy => Some ([ShapeAddress; ShapeAddress; ShapeI32], ShapeUnit)
  | PMemFill => Some ([ShapeAddress; ShapeI32; ShapeI32], ShapeUnit)
  | PMemSize => Some ([ShapeTag], ShapeI32)
  | PMemGrow => Some ([ShapeTag; ShapeI32], ShapeI32)
  | _ => None
  end.
End WasmSignatures.
Export WasmSignatures.

Module Platform.
Import Input Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Definition i32_class := MScalar 0.
Definition i64_class := MScalar 1.
Definition f32_class := MScalar 2.
Definition f64_class := MScalar 3.
Definition ref_kind (array : bool) := if array then 1 else 0.
Definition ref_class (array : bool) := MReference (ref_kind array).

Definition initial_machines : list (name * machine_class) :=
  [("I32", i32_class); ("I64", i64_class); ("F32", f32_class); ("F64", f64_class);
   ("StructRef", ref_class false); ("ArrayRef", ref_class true)].
Definition machine_table (W : world) : list (tsym * machine_class) :=
  flat_map (fun q => match w_library W with
    | None => []
    | Some dir => match w_scope W (dir ++ "wasm.moss") NSName (fst q) with
      | Some (ESym symbol) => match w_decl W symbol with
        | Some _ => [(symbol, snd q)]
        | None => [] end
      | _ => [] end end) initial_machines.

Definition MachineType (W : world) (name : string) (t : ty) : Prop :=
  exists symbol, LangSym W "wasm" NSName name symbol /\
    kind_sym W symbol = Some DKType /\ t = TAbs symbol.
Definition NominalType (W : world) (library name : string) (t : ty) : Prop :=
  exists symbol args, LangSym W library NSName name symbol /\ t = TNom symbol args.

Definition ValueType (W : world) (t : ty) (shape : shape) : Prop :=
  match shape with
  | ShapeI32 => MachineType W "I32" t
  | ShapeI64 => MachineType W "I64" t
  | ShapeF32 => MachineType W "F32" t
  | ShapeF64 => MachineType W "F64" t
  | ShapeUnit => t = TUnit
  | ShapeBool => exists symbol, LangItem W LIBool symbol /\ t = TNom symbol []
  | ShapeAddress => NominalType W "wasm" "Addr" t
  | ShapeTag => exists symbol args, t = TNom symbol args
  | ShapeNever => t = never
  end.
Definition Signature (W : world) (params : list ty) (result : ty)
    (signature : option (list shape * shape)) : Prop :=
  exists inputs output, signature = Some (inputs, output) /\
    Forall2 (ValueType W) params inputs /\ ValueType W result output.

Definition ReferenceType (W : world) (array : bool) (t held : ty) : Prop :=
  exists symbol parameter args,
    LangSym W "wasm" NSName (if array then "Array" else "Ref") symbol /\
    LangSym W "wasm" NSName (if array then "Arrayed" else "Boxed") parameter /\
    t = TNom symbol args /\ nlook parameter args = Some held.

Definition GCSignature (W : world) (operation : wasm_prim) (params : list ty)
    (result : ty) : Prop :=
  match operation, params with
  | PBox symbol, [held] => LangSym W "wasm" NSName "Ref" symbol /\ ReferenceType W false result held
  | PDeref, [reference] => ReferenceType W false reference result
  | PRefSet, [reference; held] => ReferenceType W false reference held /\ result = TUnit
  | PArrNew symbol, [held; count] => LangSym W "wasm" NSName "Array" symbol /\
      MachineType W "I32" count /\ ReferenceType W true result held
  | PArrLen, [array] => MachineType W "I32" result /\ exists held, ReferenceType W true array held
  | PArrGet, [array; index] => MachineType W "I32" index /\ ReferenceType W true array result
  | PArrSet, [array; index; held] => MachineType W "I32" index /\
      ReferenceType W true array held /\ result = TUnit
  | PArrFill, [array; index; held; count] => MachineType W "I32" index /\
      MachineType W "I32" count /\ ReferenceType W true array held /\ result = TUnit
  | PArrCopy, [dst; di; src; si; count] => MachineType W "I32" di /\
      MachineType W "I32" si /\ MachineType W "I32" count /\ result = TUnit /\
      exists held, ReferenceType W true dst held /\ ReferenceType W true src held
  | _, _ => False
  end.

Definition WasmSignature (W : world) (operation : wasm_prim)
    (params : list ty) (result : ty) : Prop :=
  Signature W params result (prim_sig operation) \/
  Signature W params result (prim_msig operation) \/ GCSignature W operation params result.

Definition PrimitiveSignature (W : world) (operation : op)
    (params : list ty) (result : ty) : Prop :=
  match decode operation with
  | Some p => WasmSignature W p params result
  (* An unsupported operation never returns a value. *)
  | None => True
  end.

Definition AtKey (W : world) (C : cx) (key : item) (operation : op) : Prop :=
  match key with
  | IFn f => exists signature, nlook f (c_fns C) = Some signature /\
      PrimitiveSignature W operation (sg_params signature) (sg_ret signature)
  | IMth receiver method _ => exists signature,
      mlook receiver method (c_mths C) = Some signature /\
      PrimitiveSignature W operation (receiver :: sg_params signature) (sg_ret signature)
  | _ => False
  end.

(* The platform families provide GC methods or methods of Addr. *)
Definition MethodPrimitive (W : world) (key : item) (operation : op) : Prop :=
  exists receiver method theta d form name params result body,
    key = IMth receiver method theta /\ w_decl W method = Some d /\
    d_ast d = ADFn form name params result body /\
    match form with
    | RDetached =>
        (name = "boxed" /\ exists symbol, LangSym W "wasm" NSName "Ref" symbol /\ operation = encode (PBox symbol)) \/
        (name = "arrayed" /\ exists symbol, LangSym W "wasm" NSName "Array" symbol /\ operation = encode (PArrNew symbol))
    | RAttached path => exists symbol, resolve_sym W (d_mod d) [path] = Some symbol /\
        ((LangSym W "wasm" NSName "Ref" symbol /\
            ((name = "deref" /\ operation = encode PDeref) \/ (name = "set" /\ operation = encode PRefSet))) \/
         (LangSym W "wasm" NSName "Array" symbol /\
            ((name = "length" /\ operation = encode PArrLen) \/ (name = "get" /\ operation = encode PArrGet) \/
             (name = "set" /\ operation = encode PArrSet) \/ (name = "fill" /\ operation = encode PArrFill) \/
             (name = "copy" /\ operation = encode PArrCopy))) \/
         (LangSym W "wasm" NSName "Addr" symbol /\
            exists p, prim_of_name name = Some p /\ operation = encode p))
    | RPlain => False
    end.

(* The handle tags carry a raw machine handle (Ref a StructRef, Array an
   ArrayRef, Addr an I32), which the primitives read as that machine value.
   A platform fn's signature must read that requirement as itself: a
   context item can bracket it (`i64_store[I32 = Addr[I32 = I64]]`), which
   would type a handle whose payload is not the raw machine type. *)
Definition raw_handles : list (string * string) :=
  [("Ref", "StructRef"); ("Array", "ArrayRef"); ("Addr", "I32")].
Definition RawHandleRead (W : world) (t : ty) : Prop :=
  forall name handle symbol args, In (name, handle) raw_handles ->
    LangSym W "wasm" NSName name symbol -> t = TNom symbol args ->
    forall raw, LangSym W "wasm" NSName handle raw -> nlook raw args = Some (TAbs raw).
Definition HandlesRead (W : world) (C : cx) (key : item) : Prop :=
  match key with
  | IFn f => forall signature, nlook f (c_fns C) = Some signature ->
      Forall (RawHandleRead W) (sg_ret signature :: sg_params signature)
  (* a platform family's method item, its receiver among the parameters *)
  | IMth receiver method _ => forall signature, mlook receiver method (c_mths C) = Some signature ->
      Forall (RawHandleRead W) (sg_ret signature :: receiver :: sg_params signature)
  | _ => True
  end.

(* The machine types a platform item's signature mentions are fixed
   inputs of its functor, in its context: the platform's provision exists
   only at the machine types, so it may not be polymorphic in one
   (lib/wasm.moss: Heap, Mutable, Arrays and Memories bracket only Boxed,
   Arrayed and Memory). *)
Definition MachinesFixed (W : world) (P : program) (source : usym) (inputs outputs : cx) (key : item) : Prop :=
  match key with
  | IMth receiver method _ => forall signature, mlook receiver method (c_mths outputs) = Some signature ->
      forall symbol, In symbol (map fst (program_machines P)) ->
      In symbol (flat_map gens (sg_ret signature :: receiver :: sg_params signature)) ->
      Fixed W source symbol /\ In symbol (map fst (c_ty inputs))
  | _ => True
  end.

(* A platform family's items have pairwise distinct methods, and at most
   one of them is a detached method (lib/wasm.moss: Heap's `.boxed`,
   Arrays' `.arrayed`; every other item is an attached method of its own).
   A copy of the family (`bind F = H;`, BFunCopy) is checked one way only
   (CopyInterpretation: each of F's items is one of H's); with this, H's items
   are each one of F's too, since F has an item on each of H's leaves
   (SameResult), resolving to H's method there (an attached method is the
   receiver's, and the one detached method is the only one left). *)
Definition DetachedMethod (W : world) (m : nat) : Prop :=
  exists d n ps rt body, w_decl W m = Some d /\ d_ast d = ADFn RDetached n ps rt body.
Definition MethodsDistinct (W : world) (keys : list item) : Prop :=
  forall k1 k2 r1 r2 m1 m2 a1 a2, In k1 keys -> In k2 keys ->
    k1 = IMth r1 m1 a1 -> k2 = IMth r2 m2 a2 ->
    (m1 = m2 \/ (DetachedMethod W m1 /\ DetachedMethod W m2)) -> k1 = k2.

Definition Family (W : world) (P : program) (source code : usym) : Prop :=
  exists inputs needs outputs keys bindings,
    FunCtx W source inputs needs /\
    (* the platform provides method items only (Elab.v platform_fun): no
       type output, which its code could not return *)
    (forall t, ~ FunOutputs W source t) /\
    ProvideResult W source inputs (fun t => TAbs t) outputs keys /\
    (* each item is installed and returned under its run-time key, the
       instantiation it is made at (rkey_of), as a linked body's are *)
    funbody_of P code = Some (FunctorCode (map fst (c_ty inputs)) (map fst (c_ty inputs)) []
      [] bindings (map (rkey_of outputs) keys)) /\
    Forall2 (fun key binding => exists operation,
      binding = BPrimitive (rkey_of outputs key) operation /\ MethodPrimitive W key operation /\
      AtKey W outputs key operation /\ MachinesFixed W P source inputs outputs key /\
      HandlesRead W outputs key) keys bindings /\
    MethodsDistinct W keys /\
    (* an abstract functor (the oracle's platform_entry: KyFun with no
       body), whose items are distinct (lib/wasm.moss: each its own
       declaration) *)
    (forall d ps a r b, w_decl W source = Some d -> functor_parts (d_ast d) = Some (ps, a, r, b) -> b = None) /\
    NoDup (map provision_id keys).

(* main's context reads each machine type a platform functor fixes as
   itself (the oracle's check_platform_entry_view: "platform functor has an
   incompatible assumed fixed type"): the platform's provision exists only
   at the machine types, so a merge of main's context may not read one as
   another type.  Stated by class (criteria section 2, coherence): the
   fixed meaning t is in the machine type's merge class, and that class is
   the machine type alone, with no structure.  A class merging the machine
   type with another symbol is rejected whichever member represents it
   (both implementations reject it, in either order:
   examples/platform-fixed-merge-{xi,ix}.moss); before, the clause asked
   `canon C t = TAbs symbol`, which held exactly when the machine type
   was the representative.  The classes do not depend on that choice
   (PublicOrientation.mgm_classes_iff). *)
Definition FixedMachinesRead (P : program) (C : cx) (source : usym) : Prop :=
  forall fixed, nlook source (c_funs C) = Some fixed ->
  forall symbol t, In symbol (map fst (program_machines P)) -> In (symbol, t) fixed ->
    canon C t = canon C (TAbs symbol) /\ (exists z, canon C (TAbs symbol) = TAbs z) /\
    (forall y, canon C (TAbs y) = canon C (TAbs symbol) -> y = symbol).

(* [D79]/platform: main's initial environment.  Each need of main must be
   something the platform provides (ElaboratesIn keys the environment by
   main's needs; this is what each provision is, AUDIT-CHANGES #91):
   lib/wasm.moss's and lib/wasip1.moss's fns are primitives (`prim_of_name`, WasmOps), wasistd's `i32_zero`/
   `i32_one` the I32 constants, and an abstract functor of those modules a
   platform binding checked by Platform.Family. *)
Definition platform_mod (W : world) (p : string) : Prop :=
  LibraryModule W "wasm" p \/ LibraryModule W "wasip1" p \/ LibraryModule W "wasistd" p.

Inductive PlatformEntry (W : world) (P : program) : item -> provision_value -> Prop :=
| PEZero : forall v d, w_decl W v = Some d -> platform_mod W (d_mod d) ->
    filed_name (d_ast d) = Some (NSName, "i32_zero") -> PlatformEntry W P (IVal v) (ValueProvision (VScalar 0 0%Z))
| PEOne : forall v d, w_decl W v = Some d -> platform_mod W (d_mod d) ->
    filed_name (d_ast d) = Some (NSName, "i32_one") -> PlatformEntry W P (IVal v) (ValueProvision (VScalar 0 1%Z))
| PEPrim : forall f d n ps rt body pr, w_decl W f = Some d -> platform_mod W (d_mod d) ->
    d_ast d = ADFn RPlain n ps rt body -> prim_of_name n = Some pr ->
    PlatformEntry W P (IFn f) (PrimitiveClosure (encode pr) None)
| PEFun : forall u d n ps a r h, w_decl W u = Some d -> platform_mod W (d_mod d) ->
    d_ast d = ADFunctor n ps a r None -> funbody_of P h <> None ->
    PlatformEntry W P (IFun u) (FunctorClosure h None empty_environment None).

Definition Entry (W : world) (P : program) (C : cx)
    (key : item) (value : provision_value) : Prop :=
  match key, value with
  | IVal value, ValueProvision (VScalar 0 _) => exists t,
      nlook value (c_vals C) = Some t /\ MachineType W "I32" t
  | IFn _, PrimitiveClosure operation None => AtKey W C key operation /\ HandlesRead W C key
  | IFun source, FunctorClosure code None (EnvironmentWithAliases [] [] []) None =>
      Family W P source code /\ FixedMachinesRead P C source
  | _, _ => False
  end.

(* These source types have fixed runtime representations. Check their
   declarations as well as the signatures that mention them. *)
Definition abstract_names : list string :=
  ["I32"; "I64"; "F32"; "F64"; "StructRef"; "ArrayRef"; "Memory"; "Boxed"; "Arrayed"].
Definition TagLayout (W : world) (name parameter handle : string) : Prop :=
  forall symbol, LangSym W "wasm" NSName name symbol ->
  exists param raw rest,
    LangSym W "wasm" NSName parameter param /\
    LangSym W "wasm" NSName handle raw /\ kind_sym W symbol = Some DKTag /\
    w_reqs W symbol = param :: rest /\
    PayloadOf W symbol (map (fun t => (t, TAbs t)) (w_reqs W symbol)) (TAbs raw).
Definition Layouts (W : world) : Prop :=
  (forall name symbol, In name abstract_names -> LangSym W "wasm" NSName name symbol ->
    kind_sym W symbol = Some DKType) /\
  (forall left right symbol, In left abstract_names -> In right abstract_names ->
    LangSym W "wasm" NSName left symbol -> LangSym W "wasm" NSName right symbol -> left = right) /\
  TagLayout W "Ref" "Boxed" "StructRef" /\
  TagLayout W "Array" "Arrayed" "ArrayRef" /\
  TagLayout W "Addr" "Memory" "I32".

Definition CastEntry (W : world) (array : bool) (entry : option (nsym * cast_spec)) : Prop :=
  let tag := if array then "Array" else "Ref" in
  let held := if array then "Arrayed" else "Boxed" in
  let raw := if array then "ArrayRef" else "StructRef" in
  (exists n t h, LangSym W "wasm" NSName tag n /\
    LangSym W "wasm" NSName held t /\ LangSym W "wasm" NSName raw h /\
    entry = Some (n, CastSpec (ref_kind array) t
      (Some (h, TMachine h)))) \/
  ((forall n, ~ LangSym W "wasm" NSName tag n) /\ entry = None).
Definition RepresentationTables (W : world)
    (machines : list (tsym * machine_class)) (casts : list (nsym * cast_spec)) : Prop :=
  machines = machine_table W /\
  exists reference array, CastEntry W false reference /\ CastEntry W true array /\
    casts = (match reference with Some x => [x] | None => [] end) ++
                (match array with Some x => [x] | None => [] end).
Definition Representations (W : world) (P : program) : Prop :=
  RepresentationTables W (program_machines P) (program_casts P).

(* The native entry ABI carries no argument or result lanes. Nominal units
   and unions carry discriminators; ordinary tags carry their payload lanes. *)
Inductive EntryResult (W : world) : ty -> Prop :=
| EntryUnit : EntryResult W TUnit
| EntryNever : EntryResult W never
| EntryRecord : forall fields,
    Forall (fun field => EntryResult W (snd field)) fields ->
    EntryResult W (TRecord fields)
| EntryTag : forall n arguments payload,
    PayloadOf W n arguments payload -> EntryResult W payload ->
    EntryResult W (TNom n arguments).
Definition EntryPoint (W : world) (P : program) : Prop :=
  exists context needs signature,
    CtxOf W (program_main P) context needs /\
    SigOf W (program_main P) (c_ty context) None signature /\
    sg_params signature = [] /\ EntryResult W (sg_ret signature).

Definition Tables (W : world) (machines : list (tsym * machine_class))
    (casts : list (nsym * cast_spec)) : Prop :=
  Layouts W /\ RepresentationTables W machines casts.
Definition Provides (W : world) (P : program) (C : cx) (key : item) (value : provision_value) : Prop :=
  Entry W P C key value /\ PlatformEntry W P key value.
Definition EntryABI := EntryPoint.

Module Static <: STATIC_PLATFORM.
  Definition Tables := Tables.
  Definition Provides := Provides.
  Definition EntryABI := EntryABI.
End Static.
Module Link := Linking Static.
Definition Linked := Link.Linked.

Definition Ready := Link.Ready.
End Platform.

(* Addresses are semantic identities. A concrete heap supplies its own
   representation and proves that allocation, reading and writing refine these
   laws; no bit ordering or trie structure is part of the language. *)
Module Heap.
Local Unset Implicit Arguments.
(* A store typing: an address's kind (0: box, 1: array) and what its
   contents (a box's value, an array's elements) satisfy. Addresses are
   allocation identities, independent of the concrete heap representation. *)
Definition hty : Type := address -> option (nat * (value -> Prop)).

(* Ψ' extends Ψ: every address Ψ types, Ψ' types the same way. *)
Definition hext (Psi Psi' : hty) : Prop :=
  forall a k Q, Psi a = Some (k, Q) -> Psi' a = Some (k, Q).

(* w is a reference (a tag over a heap address) of kind k whose contents
   satisfy Q. *)
Definition href (Psi : hty) (k : nat) (w : value) (Q : value -> Prop) : Prop :=
  exists n a, w = VTag n (VRef a None) /\ Psi a = Some (k, Q).

(* What a GC primitive needs of its arguments, for contents satisfying Q. *)
Definition gc_pre (Psi : hty) (p : wasm_prim) (ws : list value) (Q : value -> Prop) : Prop :=
  match p, ws with
  | PBox _, [v] => Q v
  | PDeref, [r] => href Psi 0 r Q
  | PRefSet, [r; v] => href Psi 0 r Q /\ Q v
  | PArrNew _, [v; VScalar 0 _] => Q v
  | PArrLen, [r] => href Psi 1 r Q
  | PArrGet, [r; VScalar 0 _] => href Psi 1 r Q
  | PArrSet, [r; VScalar 0 _; v] => href Psi 1 r Q /\ Q v
  | PArrFill, [r; VScalar 0 _; v; VScalar 0 _] => href Psi 1 r Q /\ Q v
  | PArrCopy, [r; VScalar 0 _; r2; VScalar 0 _; VScalar 0 _] =>
      href Psi 1 r Q /\ exists Q2, href Psi 1 r2 Q2 /\ forall x, Q2 x -> Q x
  | _, _ => False
  end.

(* What it gives back, when it gives a value w, with Ψ' its store typing after. *)
Definition gc_post (Psi Psi' : hty) (p : wasm_prim) (Q : value -> Prop) (w : value) : Prop :=
  hext Psi Psi' /\
  match p with
  | PBox rf => exists a, w = VTag rf (VRef a None) /\ Psi a = None /\ Psi' a = Some (0, Q)
  | PArrNew ar => exists a, w = VTag ar (VRef a None) /\ Psi a = None /\ Psi' a = Some (1, Q)
  | PDeref | PArrGet => Q w
  | PArrLen => exists n, w = VScalar 0 n
  | _ => w = VUnit
  end.

Definition is_gc (p : wasm_prim) : bool :=
  match p with
  | PBox _ | PDeref | PRefSet | PArrNew _ | PArrLen | PArrGet | PArrSet
  | PArrFill | PArrCopy => true
  | _ => false
  end.


End Heap.

(* Base's host contract, applied by Public to an interface's fields. *)
Module BaseLaws.
Local Unset Implicit Arguments.

  Definition primitive_result P result (out : signal) : Prop :=
    match out with
    | Normal value => value_shape P value result
    | Trap _ | Exit _ => True
    | _ => False
    end.

  Definition laws (S : Type) (P : program_view) (supported : op -> bool)
      (primitive : program_view -> op -> list value -> S -> signal * S)
      (heap_valid : Heap.hty -> S -> Prop) : Prop :=
    let step := primitive P in
    let valid_heap := heap_valid in
    (forall p params result site values state,
      supported (encode p) = true -> prim_sig p = Some (params, result) ->
      Forall2 (value_shape P) values params ->
      primitive_result P result (fst (step (encode p) (BaseCall.memory_arguments p site values) state))) /\
    (forall p params result site values state,
      supported (encode p) = true -> prim_msig p = Some (params, result) ->
      Forall2 (value_shape P) values params ->
      primitive_result P result (fst (step (encode p) (BaseCall.memory_arguments p site values) state))) /\
    (forall operation values state, decode operation = None -> supported operation = true ->
      fst (step operation values state) = UnsupportedOperation (unsupported_code operation)) /\
    (forall typing weaker state,
      valid_heap typing state ->
      (forall address kind predicate,
        weaker address = Some (kind, predicate) ->
        exists original, typing address = Some (kind, original) /\
          forall value, original value -> predicate value) ->
      valid_heap weaker state) /\
    (forall state, valid_heap (fun _ => None) state) /\
    (forall p values state typing,
      supported p = true ->
      (match decode p with Some primitive => Heap.is_gc primitive = false | None => True end) ->
      valid_heap typing state -> valid_heap typing (snd (step p values state))) /\
    (forall p values state typing predicate,
      supported (encode p) = true ->
      valid_heap typing state -> Heap.gc_pre typing p values predicate ->
      match step (encode p) values state with
      | (Normal value, next) =>
          exists extended, valid_heap extended next /\
            Heap.gc_post typing extended p predicate value
      | (Trap _, next) => valid_heap typing next
      | _ => False
      end).

End BaseLaws.

Module Base <: PLATFORM.
  Include Platform.Static.
  Definition hty := Heap.hty.
  Definition call := @BaseCall.call.
  Definition laws := BaseLaws.laws.
End Base.

(* The existing public interface is the Base specialization. *)
Module Type HOST := HOST_FOR Base.
Module Public (Host : HOST) := PublicFor Base Host.
