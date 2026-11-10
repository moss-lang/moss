(* Extraction for the reference implementation's executable (run/dune,
   README.md): the Rocq front end (Parse.load, and Parse.parse_moss with
   the AST dump for --dump), the elaborator (EaMain.elaborate), the proved
   passes on Audit programs (AuditFastPath.fastpath, AuditPrune.prune,
   AuditCapTrim.captrim) and Audit.v's reference interpreter
   (Audit.Semantics.Interpreter, with program_of), run through
   AuditFast.FastInterpreter (the same interpreter with deferred type
   substitution, AuditLazy, and trie-keyed provisions, proved equal to it:
   AuditLazy.lrun_main_agrees, AuditFast.frun_main_agrees), plus the primitives the
   driver runs it with (MossX.run_prim, MossX.init_state).
   assumptions/Certificate.v audits these roots. *)
From Stdlib Require Import PeanoNat Extraction ExtrOcamlBasic ExtrOcamlNatInt
  ExtrOCamlInt63 ExtrOCamlFloats ExtrOCamlPArray ExtrOcamlNativeString.
From NM Require Import Audit ExecutionModel MossX Sx NatTrie Parse.
From NM Require EaMain AuditPrune AuditFastPath AuditCapTrim AuditLazy AuditFast.
Extraction Language OCaml.
(* dune runs this in its build directory, where the extracted files go. *)
Set Warnings "-extraction-default-directory".
(* Audit's field names are strings: compare them natively. *)
Extract Inlined Constant String.eqb => "String.equal".
Extraction NoInline PrimArray.array.
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
Extraction Blacklist String List Char Bool.
Separate Extraction init_state run_prim
  parse_moss Dump.dump_file print_sx Parse.load
  EaMain.elaborate
  AuditFastPath.fastpath AuditPrune.prune AuditCapTrim.captrim
  Audit.Semantics.Interpreter Audit.Semantics.program_of Audit.Semantics.Program
  AuditLazy.LazyInterpreter AuditLazy.lift_env
  AuditFast.FastInterpreter AuditFast.fast_env.
