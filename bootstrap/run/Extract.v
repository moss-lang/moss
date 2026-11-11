(* Extraction for the reference implementation's executable (run/dune,
   README.md): the Rocq front end (SnapshotLoader.load_from, and Parse.parse_moss with
   the AST dump for --dump), the elaborator (EaMain.elaborate), the proved
   passes on Audit programs (BootstrapRun.passes) and Audit.v's reference interpreter
   (Audit.Semantics.Interpreter, with program_of), run through
   InterpreterCompiled.CloInterpreter (the same interpreter with deferred type
   substitution, InterpreterLazy, trie-keyed provisions, InterpreterFast, and compiled
   to closures, InterpreterCompiled; proved equal to it: InterpreterCompiled.crun_main_reference),
   plus the primitives the
   driver runs it with (MossX.run_prim, MossX.init_state).
   assumptions/Certificate.v audits these roots. *)
From Stdlib Require Import PeanoNat Extraction ExtrOcamlBasic ExtrOcamlNatInt
  ExtrOcamlZBigInt ExtrOCamlInt63 ExtrOCamlFloats ExtrOCamlPArray ExtrOcamlNativeString.
From NM Require AuditBase.
From NM Require Import Audit ExecutionModel MossX Sx NatTrie Parse.
From NM Require SnapshotLoader EaMain BootstrapRun InterpreterLazy InterpreterFast InterpreterCompiled.
Extraction Language OCaml.
(* dune runs this in its build directory, where the extracted files go. *)
Set Warnings "-extraction-default-directory".
(* Audit's field names are strings: compare them natively. *)
Extract Inlined Constant String.eqb => "String.equal".
Extraction NoInline PrimArray.array.
(* Scalar payloads are Z in the model. In particular, an I32 at the
   primitive boundary should fit in one native integer. *)
Extract Inlined Constant Uint63.to_Z => "(fun x -> Big_int_Z.big_int_of_int64 (Uint63.to_int64 x))".
Extract Inlined Constant Uint63.of_Z => "(fun z -> Uint63.of_int64 (Big_int_Z.int64_of_big_int (Big_int_Z.extract_big_int z 0 63)))".
(* ExtrOcamlNatInt's nat_case, with its two branches inlined.  Unchanged in
   meaning; it only changes what the native code keeps alive.  Without the
   attributes, the successor branch `fun k -> ...` is a closure, and its
   environment holds every variable the branch mentions, for as long as the
   branch runs, such as the incoming machine state of every active frame:
   the whole PArray version chain of linear memory since the oldest frame's
   entry would stay reachable (17 GB on the self-compile).  Inlined, it
   dies at its last use.  Only flambda honours the attributes (dune). *)
Extract Inductive nat => int [ "0" "Stdlib.Int.succ" ]
 "(fun fO fS n -> if n=0 then (fO[@inlined]) () else (fS[@inlined]) (n-1))".
Extract Constant loop_fuel => "max_int".
(* nat equality as int equality, monomorphic: ExtrOcamlNatInt's `(=)` is
   OCaml's polymorphic compare wherever the extracted code's local helpers
   generalize, and caml_equal is then a large share of the run time. *)
Extract Inlined Constant Nat.eqb => "Int.equal".
Extract Inlined Constant PeanoNat.Nat.eqb => "Int.equal".
Extract Inlined Constant Nat.leb => "(<=)".
Extract Inlined Constant Nat.ltb => "(<)".
Extract Constant Nat.div => "fun n m -> if m = 0 then 0 else n / m".
Extract Constant Nat.modulo => "fun n m -> if m = 0 then n else n mod m".
(* NMap's and NatTrie's trie steps on OCaml ints (for every k >= 0):
   tleft k = evenb (k - 1), tdown k = div2 (k - 1). *)
Extract Inlined Constant NMap.is_even_nat => "(fun k -> k land 1 = 0)".
Extract Inlined Constant Nat.div2 => "(fun k -> k asr 1)".
Extract Inlined Constant tleft => "(fun k -> k = 0 || k land 1 = 1)".
Extract Inlined Constant tdown => "(fun k -> (k - 1) / 2)".
Extraction Inline InterpreterCompiled.scalar_nil InterpreterCompiled.scalar_cons InterpreterCompiled.push_arg.
(* The native operation has the same immutable name and operands.
   Its private memo is shared between the interpreter adapter and host. *)
Extract Inductive Audit.Semantics.op => "PrimCache.operation"
  ["PrimCache.operation"]
  "(fun f operation -> f (PrimCache.name operation) (PrimCache.symbols operation))".
Extract Inlined Constant Audit.Semantics.op_name => "PrimCache.name".
Extract Inlined Constant Audit.Semantics.op_syms => "PrimCache.symbols".
Extraction NoInline AuditBase.WasmOps.decode.
Extract Constant AuditBase.WasmOps.decode => "PrimCache.memoize decode_operation".
Extraction Blacklist String List Char Bool.
Separate Extraction AuditBase.BaseCall.call init_state run_prim_fast
  parse_moss Dump.dump_file print_sx SnapshotLoader.load_from
  EaMain.elaborate
  BootstrapRun.passes BootstrapRun.indexed_program_of
  AuditBase.WasmOps.decode_operation AuditBase.WasmOps.unsupported_code
  Audit.Semantics.required_primitives
  Audit.Semantics.Interpreter Audit.Semantics.program_of Audit.Semantics.Program
  InterpreterLazy.LazyInterpreter InterpreterLazy.lift_env
  InterpreterFast.FastInterpreter InterpreterFast.fast_env InterpreterCompiled.CloInterpreter.
(* InterpreterCompiled again, alone, with the extraction's term optimizations off
   (flag 0): this rewrites the InterpreterCompiled.ml written above, and must follow
   Separate Extraction. The closure compiler's speed depends on where its
   closures are built, and the optimizations may move them: inline
   `let c1 := cc e1 in fun m d g st => ..` (the child's code would then be
   rebuilt at every evaluation), or lift `fun m d g st` out of a match on
   the expression (a function matching on it at every call). Unchanged in
   meaning: the same terms, extracted literally. *)
Set Extraction Flag 0.
Extraction Library InterpreterCompiled.
