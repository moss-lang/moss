(* Extraction of the M2 oracle: the Gallina elaborator and MossX.run_main. *)
From Stdlib Require Import PeanoNat Extraction ExtrOcamlBasic ExtrOcamlNatInt
  ExtrOCamlInt63 ExtrOCamlFloats ExtrOCamlPArray ExtrOcamlNativeString.
From NM Require Import ExecutionModel CoreMoss MossX Sx Elab FastX Prune Parse SlotX FoldX FastPath CachedClo LazyX LazyClo.
Extraction Language OCaml.
(* dune runs this in its build directory, where the extracted files go. *)
Set Warnings "-extraction-default-directory".
Extraction NoInline PrimArray.array.
(* ExtrOcamlNatInt's nat_case, with its two branches inlined.  Unchanged in
   meaning; it only changes what the native code keeps alive.  Without the
   attributes, the successor branch `fun k -> ...` is a closure, and its
   environment holds every variable the branch mentions, for as long as the
   branch runs.  In XS.xinterp that includes the incoming machine state `st`
   of every active frame, so the whole PArray version chain of linear memory
   since the oldest frame's entry stays reachable (17 GB on the
   self-compile).  Inlined, `st` dies at its last use. *)
Extract Inductive nat => int [ "0" "Stdlib.Int.succ" ]
 "(fun fO fS n -> if n=0 then (fO[@inlined]) () else (fS[@inlined]) (n-1))".
Extract Constant loop_fuel => "max_int".
(* nat arithmetic the elaborator uses, on OCaml ints (as ExtrOcamlNatInt
   does for plus/minus/mult/eqb) *)
(* m4: nat equality as int equality, monomorphic.  ExtrOcamlNatInt's `(=)` is
   OCaml's polymorphic compare wherever the extracted code's local helpers
   generalize (ty_eqb's and lookup_subst's inner loops over (nat * ty)
   pairs), and caml_equal was a third of the run time. *)
Extract Inlined Constant Nat.eqb => "Int.equal".
Extract Inlined Constant PeanoNat.Nat.eqb => "Int.equal".
Extract Inlined Constant Nat.leb => "(<=)".
Extract Inlined Constant Nat.ltb => "(<)".
Extract Constant Nat.div => "fun n m -> if m = 0 then 0 else n / m".
Extract Constant Nat.modulo => "fun n m -> if m = 0 then n else n mod m".
(* FastX's trie steps on OCaml ints (for every k >= 0, including 0):
   tleft k = evenb (k - 1), tdown k = div2 (k - 1). *)
Extract Inlined Constant tleft => "(fun k -> k = 0 || k land 1 = 1)".
Extract Inlined Constant tdown => "(fun k -> (k - 1) / 2)".
(* The closure compiler's list, bind and functor steps are used partially applied
   (`lcons (cc e) cr`); as top-level 6-ary OCaml functions every call would
   go through caml_curry.  Inlined, each is a 4-ary closure built once, at
   closure-compile time.  Unchanged in meaning. *)
Extraction Inline CachedClo.lnil CachedClo.lcons CachedClo.lvar CachedClo.lval
  CachedClo.bnil CachedClo.capply CachedClo.capply_r CachedClo.cbranch csnil cscons
  LazyClo.lnil LazyClo.lcons LazyClo.lvar LazyClo.lval LazyClo.bnil
  LazyClo.capply LazyClo.capply_r LazyClo.cbranch
  LazyFold.knil LazyFold.kcons.
Extraction Blacklist String List Char Bool.
Separate Extraction run_main FastX.frun_main fast_program oracle_program init_state xinterp f64_bits f64_of_bits elaborate_sx
  parse_moss Dump.dump_file print_sx load_bundle bundle_sx print_bundle
  rrun_main shapes compile_fns compile_funs CachedClo.cc CachedClo.ccbs CachedClo.crun_main fold_e fold_b
  LazyX.frun_main LazyClo.cc LazyClo.ccbs LazyClo.crun_main.
(* CachedClo again, alone (Extraction Library), with the extraction's term
   optimizations off (flag 0); this rewrites the CachedClo.ml written above, and
   must follow Separate Extraction.  Its speed depends on where its closures are built,
   and the optimizations may move them: inline `let ca := <list code> in
   fun n d g st => ..` (the argument list's code would then be rebuilt at
   every evaluation), or lift `fun n d g st` out of `fun e cr => match e
   with RVar x => fun n d g st => ..` (a 6-ary function matching on e at
   every call).  Unchanged in meaning: the same terms, extracted
   literally. *)
Set Extraction Flag 0.
Extraction Library CloScalar.
Extraction Library CachedClo.
Extraction Library LazyX.
Extraction Library LazyClo.
(* Preserve the same child-code sharing in the scalar closure compiler. *)
Extraction Library LazyFold.
