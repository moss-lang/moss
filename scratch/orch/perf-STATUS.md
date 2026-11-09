# perf: Rocq check build time (branch ob-perf, from safety at 9d1a25d)

Goal: clean `cd bootstrap && ./check.sh` (dev profile, default -j = 32) under
120 s without weakening anything. Result: **112 s** (from 914 s or more).

## Timings (clean `_build`, dev profile, default parallelism)

| run | state | wall | user | load at start -> end |
|---|---|---|---|---|
| reported | 9d1a25d | 914 s | ~= real | - |
| baseline (mine) | 9d1a25d | >1130 s: theories through PublicEntry in 456 s, then Certificate.v still running after 11 min (killed) | - | 0.68 -> 9 (other agents) |
| exp copy | chain de-aliased, old Certificate.v | 142 s (Audit etc. cached; concurrent with baseline) | - | 5.0 -> 18 |
| run1 | + one instance in Certificate.v | 123 s | 27m15 | 3.7 -> 15 |
| run2 | + roots as a list, statement tie | 117 s | 25m49 | 3.6 -> 13 |
| run3 | same, DUNE_JOBS=16 (experiment only) | 117 s | 19m22 | 7.5 -> 11 |
| run4 | + full paths in Certificate.v, PublicWorldRequirements fix (c04dd07) | **112 s** | 24m51 | 3.5 -> 15 |

Other agents' processes (moss interpreters, ~3 cores) were running throughout.

Per-file (from dune's trace): before, PublicChainRealize.vo was 85 MB,
PublicFamilyCopy.vo 155 MB, PublicEntry.vo 331 MB; PublicEntry alone took
3.5 min and Certificate.v more than 11 min. After, those files are 1.7-2.2 MB,
BodySlice through PublicEntry builds in ~15 s, and Certificate.v takes 12 s.

## What changed

1. a765982: no in-functor module abbreviations in the body-safety chain
   (PublicArms, PublicBodySlice, PublicCopyApply, PublicFamilyStep,
   PublicPlatformApply, PublicLinkResults, PublicChainCore, PublicLinkRun,
   PublicChainNew, PublicChainFwd, PublicFamilyLink, PublicChainSigs,
   PublicChainRealize, PublicFamilyCopy, PublicEntry). Inside a functor,
   `Module S := CC.S.` is a strengthened copy, not an alias, so each level
   copied every level below it, and every application copied the copies
   (`-time` on PublicChainRealize: 38 of its 42 s were the `Module` lines).
   Each use now names the module by full path through the one application
   (e.g. `CSg.FL.CF.CN.CC.S.foo`). Mechanical rewrite
   (scratch/perf/dealias.py); no statement or proof step changed. A comment
   at PublicEntry's `FCp` says why, so the abbreviations don't come back.
2. 4424251: Certificate.v takes every chain root from PublicEntryTop
   (public_safety's instance, which contains each chain functor exactly
   once), by full path, instead of applying ~15 chain functors separately.
   Even a top-level `Module X := PublicEntryTop.FCp.` is a copy (0.2-0.4 s
   each), so these are full paths too. The roots are a list of `{T & T}`
   instead of a nested tuple, whose term is quadratic in the number of roots
   (2 s to elaborate -> 0.08 s). Every root is still there, public_safety
   included. The assumption output is identical: the same multiset of lines
   as the old Certificate's log, only kernel primitives.
   Roots that used to be read at `PublicClosureExample.Prims` are now read
   at `ReferenceHost` (public_safety's instance). They are the same functor
   lemmas, and Print Assumptions on an instance covers the functor body,
   so the audit is just as strong. PublicHeaps and PublicLinkMergesI keep
   their own small instances.
3. 57ab96d: PublicWorldRequirements: `cbn in *` -> `cbn in` the four
   hypotheses it needs (12 s -> 0.6 s standalone, ~35 s -> ~1 s in a
   parallel build).
4. c04dd07 (coordinator's request): Certificate.v's functor
   `SafetyIsCorrect (C : PublicPublic.CORRECT)` reads the statement off
   `C.safety`'s type, abstracts it over `C.prelude` (Ltac `pattern`), and
   checks that `PublicEntryTop.public_safety` has exactly that type, both
   with `constr_eq` and by typing `public_safety_is_safety : statement`.
   The statement now lives only in Audit.v. Checked by hand: a variant with
   an extra premise is rejected by both checks.

## Where the time goes now; the floor

The critical path is now the Elab chain (ExecutionModel, CoreMoss,
WorldSound, ElabFrameCore, ..., FamilyResultWalk, ..., BodyPending,
~35 files) plus Certificate.v (12 s, of which Print Assumptions is 8 s,
5 s of that for BodyPending.pending's cone alone). That chain is ~98 s in
the build, but its files run 3-5x slower than standalone because 32 jobs
share 16 physical cores (FamilyResultWalk: 2.4 s alone, 11.8 s in the
build, user time ~= wall). So the remaining lever is total CPU (1794 s user).
The biggest remaining files are RelExampleData (a 21 s vm_compute),
LinkSynthCache, LinkBindPreserve (an 8.4 s `binding_tac`), FrontEndComplete
and RelNegative. With about 8 s of margin, more agents on the machine can
push a run back over 120 s.

Time spent: about 35 minutes.
