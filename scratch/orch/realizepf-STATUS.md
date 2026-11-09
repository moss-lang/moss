# realizepf: status

Branch ob-realizepf, from safety 460a720, merged up to safety 94d89ce
(linknewpf's link_site fix). About 1h15m.

## Stopped: CN.ChainRealizeOk is still false for an Audit-accepted program

`bootstrap/examples/copy-fresh-collide.moss` (the full argument is in its
header). The copyfresh repair is incomplete. CopyOutputs asks only that each
`cg y` is `Fresh W` (undeclared), distinct from the other `cg` symbols and
from the output roles. But sigma's image, which is F's fixed readings at the
copy site, can itself mention an undeclared generation.
- In `demo`, `bind Z = Bool; bind K;` rebinds K's output Z. At the copy site,
  Z therefore reads as a fresh application generation g.
- So sigma = [Z := TAbs g] and ys = [I], and the witness `cg I := g` is
  allowed.
- After the substitution, F's `get[I = Z]` and H's `get` are both
  `(): g [I := g]`. The copy is accepted, and F's input I is identified with
  the site's meaning of Z again: this is t3's hole.
- At `bind F;` the installed `get` returns Bool (`I = Bool`), while C' types
  it at g, which grounds to Char. `show(get())` then runs Char's `code` on a
  Bool.

Verdicts:
- **Oracle** (ob-sigclose var moss_m2): rejects at the copy (D78).
- **Native** (ob-family, seed 1h1710x0): rejects at the copy ("does not
  satisfy the required signature: `get`").
- **Control** (`scratch/realizepf/control.moss`, F[I] and no bracket): the
  oracle and native both accept it and print 7. So everything up to the
  copy is accepted, including the K rebind.

FS.FamilyCopyRealizeOk uses the same CopyOutputs, so it has the same hole
(v3 with the fixed Y read as a fresh generation). I did not build a separate
test for it.

A role variant is plausible but untested: an output role `gen t` chosen equal
to a generation in sigma's image.

**Suggested Audit repair (not made: the task says stop and report).**
CopyOutputs should also require
`forall y, In y ys -> ~ In (cg y) (flat_map (fun q => gens (snd q)) sigma)`,
and the same for each `gen t`. This mirrors the oracle's `@copy`/new_sym
symbols, which are fresh for everything. Accepted programs can always pick
such symbols, so the repair rejects nothing the oracle accepts. It still
needs the corpus comparison.

## Proved (bootstrap/theories/PublicCopyFresh.v, a Certificate root)

These are the two lemmas copyfresh-STATUS asked for, with no premises beyond
those stated.
- `ground_app_subst`: `ground d (app_subst s t) = app_subst (map (fun q =>
  (fst q, ground d (snd q))) s ++ type_meanings d) t`.
- `ty_agree`, `tys_agree`, `inst_agree`, `provision_agree`: a composition of
  two substitutions equals a third on a type or provision when they agree on
  each generation it mentions.
- The copy step (Section CopyRead), with `cg_map = map (fun y => (y, TAbs
  (cg y))) ys` (CopyOutputs' extension) and `cg_read = (cg y := tau y) ++
  rho0`.
  - `copy_outputs_at`: if `provision_subst (sigma ++ cg_map) p =
    provision_subst (sigma ++ cg_map) p'`, and every generation of p and p'
    satisfies `CopyGen`, then `provision_subst tau p = provision_subst tau
    p'`.
  - `CopyGen x` asks two things:
    - when x is fixed (sigma reads it as s), s avoids the cg symbols and
      `rho0 s = tau x`;
    - otherwise, when x is outside ys, x is not a cg symbol and
      `rho0 x = tau x`.
  - Also `copy_provision_read`, `copy_ty_read` and `copy_gen_read`. Only
    `NoDup (map cg ys)` is needed.
  - The avoidance clause in `CopyGen` is exactly what the suggested repair
    would supply. Without it, copy-fresh-collide.moss is a counterexample.
- `fresh_not_declared`, a trivial helper. The substantive half still to do
  is that FunCtx readings and Cu-formal provisions mention only declared
  generations and roles. I found no existing lemma for it, and it needs a
  walk over ItemsEntries and TyOf (see "Remaining").

check.sh passes (212s).

## Remaining

- The Audit repair above, with its own commit, AUDIT note, corpus
  comparison and SAFETY-SITES entry.
- Then ChainRealizeOk:
  1. Length-zero case (h0 = u, Ch0 = Cu): the correspondence between the
     caller's es (read at C, out = gensof) and the formal es' (read at Cu,
     out = gen), position by position. Candidates are PublicOutputSubst
     `provide_result_rel` and PublicFamilyAgree `family_entries_rel`. Last
     setters agree, because a caller key is a function of the formal key and
     both sides pick the last position.
  2. `body_provides` for the body's value at d3, re-read through
     `link_site` (`realizes_link_site` and `canon_link_site` from 94d89ce).
  3. The run fact that d3's meaning of each linked input x is
     `ground d (th x)`. It is still not threaded: `app_step` drops
     `MeaningsGrow made de d3`, and a static walk is needed showing that
     `made` avoids Cu's inputs.
  4. Then compose the copy steps with `copy_outputs_at`. Taking tau = the
     application's meanings and rho0 = d's meanings needs these facts:
     - FixedMeans gives gfx = ground d (sigma x);
     - Cu's gens and the roles are declared or fresh, and distinct from the
       cg symbols;
     - each copy's own CopyOutputs comes from CCopy/CopySite.
- FamilyCopyRealizeOk: the same plan, with no outputs.
