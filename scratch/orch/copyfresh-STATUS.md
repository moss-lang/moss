# copyfresh: status

Branch ob-copyfresh, created from safety at e69a7cd. The work took about 1h45m.

## Done

- **Audit change** (commit "Read a copy's non-fixed inputs as fresh
  symbols in CopyOutputs").
  - `Audit.CopyOutputs` compares F's and H's result provisions after
    `sigma ++ [y := cg y | y in ys]`. Here ys are the generations of F's
    input readings that sigma (F's fixed readings at the site) does not
    read. Each `cg y` is fresh, and they are distinct from each other and
    from the output roles. This is the oracle's `copy_output_compatible`
    with its `@copy` symbols.
  - Details, evidence and alternatives are in
    scratch/orch/copyfresh-AUDIT.md, plus a SAFETY-SITES Observations
    entry. The ChainRealizeOk and FamilyCopyRealizeOk table rows now note
    the counterexample and this repair.
  - New tests: bootstrap/examples/copy-bracket-nonfixed.moss and
    copy-fixed-reads-poly.moss. The oracle and native reject both.
  - Corpus (scratch/orch/copyfresh-cmp.out) compares this oracle with a
    variant that has the old rule (unsolved vars kept as themselves):
    389 same, 4 different. All 4 are hole witnesses that only the old
    rule accepts.
- **Proofs**: no statement changed. The only edit is the destructuring in
  `PublicCopyApply.chain_sources_range` (two more witnesses).
  `cd bootstrap && ./check.sh` passes (68s; the log was
  scratch/orch/copyfresh-check.log, not committed).
- No obligation was proved, and no new premises were added.

## Why ChainRealizeOk and FamilyCopyRealizeOk should now hold (copy part)

Take an application of F whose meanings are τ:
- a fixed x means gfx x = ground d (sigma x), with d realizing the copy
  site C1 (CopySite);
- a non-fixed y means θ y, which is arbitrary.

Let ρ send each `cg y` to θ y and every other symbol to its meaning in d.
1. Each `cg y` is `Fresh W` (undeclared). So it occurs in neither p, nor
   p', nor sigma's image, all of which are formed over declared symbols.
   It is also distinct from the output roles (NoDup).
2. Hence ρ ∘ (sigma ++ cg) restricted to p's symbols is exactly τ: x maps
   to d(sigma x), y maps to θ y, and the output roles are unchanged.
3. So CopyOutputs' equation gives p[τ] = p'[τ] at every application. The
   old equation gave this only when τ agreed with d on sigma's image.

Both counterexamples broke exactly that: a y in sigma's image (demo's I,
go's A) with θ y ≠ d y. A chain applies this step by step, since each copy
has its own CopyOutputs (CCopy).

The proof needs two lemmas, and neither is written yet:
- a composition lemma, `ground` after `app_subst` equals one substitution;
- freshness implies non-occurrence for formed provisions.
Consuming CopyOutputs needs the new witnesses (`ys`, `cg`, `Fresh`, NoDup),
which CopySite already exposes because it contains CopyOutputs.

## Re-hunt (scratch/copyfresh/; oracle = sigclose var, native = ob-family seed 1h1710x0)

| file | shape | oracle | native | old rule (variant) | new Audit |
|---|---|---|---|---|---|
| r1-copy-of-copy | t3 through F1 = H, then F = F1 | reject at F = F1 (D78) | reject (signature `get`) | accepts | rejects (I fresh) |
| r1b-copy-of-bracketed-copy | F2[]: get[I = Z], Z = Char, copied from H[I], then F3 = F2 | reject at F2 = H | reject | rejects | rejects |
| r2-bracket-poly-swapped | v3 swapped: F's A.m[U = Y], H's A.m[U = A], Y read as go's A | reject (D78) | reject (F unavailable) | accepts | rejects (A fresh) |
| r2b-two-fixed-same-reading | F's get[I = Z], H's get[I = Y], both fixed and read as demo's I | accepts, prints 97 | accepts, prints 97 | accepts | accepts (sound: H's body runs at Y = Char) |
| r3-site-merge | t3 with Z ≡ I by a merge in demo's context | reject (D78) | reject | rejects | rejects |
| r3b-target-merge | F's context merges non-fixed I with fixed Z, applied at I = Bool | copy accepted, application rejected (D43) | reject | same | copy accepted (canonical reps agree); application blocked by Satisfies' merge clause |
| r3c-target-merge-ok | r3b at I = Z = Char | prints 97 | reject (signature `I`) | same | accepts |
| r4-family-copy-of-copy | v3 through F1 = H, then F = F1 | reject at F = F1 (D78) | reject | elaborates, then stuck (XErr 6) | rejects |

**Verdict:** no new counterexample. Every shape that the old rule wrongly
accepted is now rejected at the copy, both directly and through a copy of
a copy. Every accepted shape is consistent across the oracle and Audit.

## Remaining

- The two lemmas above, and then the obligations themselves; ChainRealizeOk
  and FamilyCopyRealizeOk are still hypotheses.
- Known residual (copyfresh-AUDIT.md): when F's context merges a fixed
  input into a non-fixed representative, Audit can be stricter than the
  oracle, and it was before this change too. This is not a soundness
  issue.
