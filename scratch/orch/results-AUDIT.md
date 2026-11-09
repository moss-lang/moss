# Audit changes by the "results" agent (LinkResultsOk)

## 1. BApplyAbstract: every output's destination is made (MadeFor)

**What.** `BApplyAbstract` required `FreshFor W F C newgens` and, for an
output `t` not in force here, only `gensof t = t`. Its `made` list (and the
frame's `fr_fresh`) was `newgens`, so an output that keeps its name was not
counted as made. Now the premise is `MadeFor W F C outs newgens` (new
definition next to `FreshFor`): `newgens` has no duplicates, and each of its
members is either `Fresh W` or an output not in force here, and is not in
`fr_fresh F`, `context_gens C` or the frame's return/receiver types. The
not-in-force output clause now also requires `In t newgens`.

**Why.**
- `LinkResultsOk` concludes `MeaningsGrow newgens d (append_environment result d)`,
  but `result_environment` writes a meaning for every output's destination,
  including a kept `t`. A kept `t` is a declared type symbol, so it was never
  in `newgens` (all `Fresh W`): the obligation was false for every
  application with an output not in force, e.g. the existing
  `bootstrap/examples/captured-output-scope.moss` (`bind F;` in `main`, T not
  in force, Maker's body binds T).
- Independently, Audit accepted a program both implementations reject, and
  which the reference interpreter runs ill-typed:
  `bootstrap/examples/kept-output-escape.moss`. `let v = { bind F; mk() };`
  lets a `T` (Bool inside the block) out; `bind G;` later makes `T` again (as
  Int), and `take(v)` runs `x + 1` on `true`. D78 (`BlkECheck`) only
  rejected the escape of `made` generations, and `made` lacked `T`.

**Evidence.**
- Oracle (`moss_m2.exe --elab-only`, built from this branch's Elab.v):
  "this block yields `T`, which mentions `T`, a type `bind F;` made in it: a
  value of that type cannot leave the block (D78)". Its `apply_abstract`
  appends `map snd gens` (every output's destination, kept ones included)
  to `cx_fresh`.
- Native (`moss run`, moss-safety's build, against its lib): "a type created by
  a binding cannot escape its block: `T`".
- The extra scope conditions on kept outputs (not in `fr_fresh F`,
  `context_gens C`, return/receiver types) are not checked by the oracle.
  They are needed at runtime anyway (`result_environment` is Stuck when the
  destination already has a meaning, and `outcome_grow` needs the frame's
  types to avoid it). An oracle variant that rejects a kept output found in
  `cx_fresh`, the type table's values, the merge table, the locals' types or
  the signature's return/receiver type gives the same verdict as the oracle
  on all 356 programs under tests/, examples/ and bootstrap/examples/
  (scratch/orch/results/cmp.sh, cmp.out; the variant adds the check in Elab.apply_abstract after `gens`).

**Alternatives considered.**
- Changing `made` to `newgens ++ kept` in the conclusion: mirrors the oracle
  textually but changes the statement of `LinkResultsOk`, `CopyApplyOk`
  and `PlatformApplyOk` (`with_fresh F newgens`, `MeaningsGrow newgens`).
  Folding the kept outputs into `newgens` keeps every obligation's text.
- Keeping Audit and weakening `MeaningsGrow` in the slice: does not fix the
  accepted ill-typed program above.

**Proofs fixed.** `PublicFormation` (one destruct pattern in the bind-item formation proof).
`PublicBodySlice.AppPremises`/`AppAt` restate the rule's premises.
