# Audit change by "copy": BFunCopy compares output interpretations

## What

`Audit.v`: new `CopyOutputs W sigma u h Cu` (next to `SameResult`), and a
new last premise of `BFunCopy`:

    CopyOutputs W (map (fun q => (fst q, canon C (snd q))) fx) u h Cu

Read at F's (u's) context Cu with the same fresh output roles, H's result
gives each key of F's result the provision F's result gives it (`ilook`, so
each key's last setter), after F's fixed inputs are read as they are in
force where F is bound (`fx`, canonical in C).

## Why (counterexample)

`bootstrap/examples/copy-output-order.moss`:

    functor F[]: -> out[P=U], out[P=V];
    functor H[]: -> out[P=V], out[P=U];
    functor G: -> out[P=V], out[P=U] { bind P = U; bind out = U; }
    fn main() { bind H = G; bind F = H; bind F; match out { V => () }; }

F and H have the same result leaves (`SameResult`), no inputs, no fixed
inputs, no needs, so every premise of the old `BFunCopy` held. Applying F
runs G's body, which provides `out` = the tag value `U`, but F's result
(`ProvideResult`, last setter) types `out` as `V` in the caller. The match
then has no arm for the value: stuck under Audit. So `CopyApplyOk` (the
caller's context after the application is realized) was false.

Evidence:
- oracle (`moss_m2.exe --prelude "" [--elab-only] FILE`): rejects, exit 64,
  "`H` cannot be copied to `F`: its effective output interpretation differs
  (D78)" (Elab.v `copy_output_compatible` / `copy_outputs_match`).
- native (`./target/release/moss run FILE` in moss-safety): rejects,
  "the functor does not satisfy the required signature: `out`" at `bind F = H;`.
- the same program is the native test
  `copied_functors_compare_final_output_interpretations`
  (crates/moss-tests/tests/suite/runtime_platform.rs); its accepted variant
  (F listing `out[P=V], out[P=U]`, matching `U`) runs under both, and
  satisfies the new premise.

## What it mirrors, and how it differs from the oracle

The oracle reads F's result in F's env and H's in H's env with the same
fresh roles (`fresh_target_output_view`, `provide_result`), and compares,
for each of F's keys from the last, the slot (application and signature)
of H's last setter of the corresponding key, under `generic`: F's formals
with fixed inputs solved to their site meanings and the rest fresh.
Audit's premise reads both results in Cu (H's inputs read as F's, which
BFunCopy already requires: `canon Cu (TAbs x) = canon Cu tx`), compares the
provisions under the same key spelling, and substitutes only F's fixed
inputs (by their site meanings, `fx`). Possible over-strictness: a method
key whose receiver is spelled differently in F and H but equal only after
the fixed inputs are read at the site, or an H result that mentions a type
outside Cu's table. No corpus program does either: the copies in the corpus
(`bootstrap/examples/copy-*.moss`, `tests/wasi/functor_*identity*copy*.moss`
(F = F), and the inline programs of runtime_platform.rs) spell F's and H's
results alike. A mechanized comparison with the oracle was not done
(ExecutionModel's own BFunCopy, used by FunctorCopyExec.v, is a separate
copy and is unchanged).

## Alternatives considered

- Requiring `ProvideResult` of u and h to coincide at every caller: much
  stronger than the oracle (rejects respellings with equal meaning).
- Comparing at the application instead (in `ClosureOk`/the obligation):
  the obligation's statement must not change, and the fact is static.

## Threading

`ClosureOk`'s `CCopy` records it (`exists sg m, A.CopyOutputs W sg u h Cu /\
gfx = ground_fixed m sg`); `case_BFunCopy` provides it (sg from fx, m the
type meanings, by merge absorption). `closure_cases` is unchanged.
