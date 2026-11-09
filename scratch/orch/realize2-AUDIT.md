# realize2: Audit change

## What

`BApplyAbstract` (Audit.v), kept-output case: an output that is not in
force and is installed as itself (`gensof t = t`) must also satisfy
`~ In t (fr_pins F)`. The mirror in PublicBodySlice (`AppPremises` and
`case_BApplyAbstract`'s premise list) gets the same conjunct, and
`PublicLinkResults.kept_outputs_unmentioned`'s destructuring is adjusted.

## Why

`CN.ChainRealizeOk` was false for an accepted program:
`bootstrap/examples/link-kept-output-pin.moss`. It is the observable form
of linknewpf's m7 (ob-linknewpf `scratch/linknewpf/m7-kept-output-receiver.moss`).
- F: T, Std -> T.m, done. G: Make, Std -> T.m, done. G does not take T.
- G's body runs `bind Make;`. Make's output T is not in force in G, so it
  was installed as itself, and at run time it means Char.
- The body binds `T.m` on it, and F's key `T.m` reads at
  `link_site Cg Cb []` as T.m.
- The caller (T = Bool) files the closure under Bool.m, and `true.m()`
  runs Char's `code` on a Bool.
- The pins M(F) exist to stop the body from changing F's result inputs
  (D84 3d). They stopped `bind T` only, not an application's kept output.

The proof needs exactly this: step 1 of realizepf's plan says that the
body's destinations (`binding_dests`) avoid F's inputs. A fresh
destination is `Fresh W` and so undeclared, while F's inputs are declared.
A kept destination now avoids M(F).

## Evidence

| Program | Oracle (ob-sigclose var moss_m2) | Native (ob-family, seed 1h1710x0) | Audit (new) |
|---|---|---|---|
| link-kept-output-pin.moss | elaborates; run stuck (`XErr 14`, exit 3); filed as proof-plan **E37** | rejects at `bind F = G` ("compilation failed: `T`") | rejects |
| link-kept-output-pin-control.moss (caller's T = Char) | prints 48, 10 | rejects (same) | rejects |

## Corpus comparison

- `scratch/orch/realize2-cmp.sh`, output in `scratch/orch/realize2-cmp.out`.
- A is the oracle as in safety. B is a variant (`scratch/realize2/variant-Elab.patch` applied to a copy of bootstrap/, built in scratch/realize2/varsrc)
  whose `check_link_pins` also rejects a pin that the linked body's final
  table has and G's own table lacks. That is a kept output installed at a
  pin, and is the oracle's closest analogue of the new Audit clause. (The
  oracle's pins are P(F) ∩ M(F), Elab.v `mf`; Audit's are M(F).)
- Over tests, examples, bootstrap/examples, scratch/copyfresh,
  scratch/realizepf and scratch/realize2: **398 same, 5 different**. All
  5 are the new programs: the two examples, plus the m7, m8 and control
  copies in scratch/realize2. No existing corpus program changes verdict.
- The control is a correct program (the meanings agree) that the oracle
  runs. The new clause rejects it, as native does. This is an
  incompleteness relative to the oracle only.

## Alternatives considered

- **Read F's key receiver at the body's table for kept outputs.** That
  is the old `at_site Cb`, which linknewpf's `link_site` replaced for
  sound reasons (merge-representative rebinds), and it would still need
  the caller's meaning to equal the body's.
- **Require G to take every input of F that F's result mentions.** This
  is too strong, since G may ignore an input.
- **Restrict only applications whose output appears in a result key.**
  This is finer, but neither implementation draws that line, and it
  complicates the walk.
