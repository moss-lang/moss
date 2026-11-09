# realizepf: Audit change

## What

`Audit.CopyOutputs W sigma u h Cu rm` gains one conjunct, placed right after
the NoDup conjunct:

    (forall z, In z (flat_map (fun q => gens (snd q)) sigma) ->
       ~ In z (map gen outs ++ map cg ys))

That is, the fresh symbols must also avoid the generations of sigma's image.
This covers both the readings `cg y` of F's non-fixed inputs and the output
roles `gen t`.

No statement that mentions CopyOutputs changed. The only proof edit is one
more `_` in `PublicCopyApply.chain_sources_range`'s destructuring.

## Why

copyfresh made each `cg y` `Fresh W`, meaning undeclared. But sigma's image
consists of F's fixed readings at the copy site, read canonically there, and
it can mention an undeclared generation. After `bind Z = Bool; bind K;`, the
output Z of K is in force as a fresh application generation g. Then:

- sigma = [Z := TAbs g] and ys = [I], so the witness `cg I := g` is allowed.
- F's `get[I = Z]` and H's `get` both read `(): g [I := g]`. The copy is
  accepted, and F's input I is identified with the site's meaning of Z.
  This is the t3 hole again (examples/copy-bracket-nonfixed.moss).
- At `bind F;` the body returns a Bool (I = Bool), while C' types `get` at
  g, which grounds to Char. `CN.ChainRealizeOk` fails.
- `FS.FamilyCopyRealizeOk` uses the same CopyOutputs, so it has the same
  hole.

**The role half.** I have not tested a program for it. The proof needs it.
In `PublicCopyFresh.copy_outputs_at`, the comparison is transferred to an
application's meanings tau through `cg_read`. `cg_read` must send each
generation in sigma's image to its site meaning, and each role `gen t` to
the role's meaning at the application. A role that is also a generation of
sigma's image would need both meanings at once.

## Mirrors the oracle

`Elab.copy_output_compatible` reads each unsolved generation as
`new_sym ... "@copy"`, which is fresh for everything. That includes the
site's generations, and so sigma's image. Output roles come from
`fresh_target_output_view`, which is fresh as well. So whenever the oracle
accepts a copy, its symbols witness the strengthened condition.

## Test and evidence

- **bootstrap/examples/copy-fresh-collide.moss** (the argument is in its
  header):
  - The oracle (this branch's Elab.v) rejects it at `bind F = H` with D78.
  - Native (ob-family, seed 1h1710x0) rejects it at the copy, with "the
    functor does not satisfy the required signature: `get`".
  - Old Audit accepts it, with `cg I := g`. New Audit rejects it: `cg I`
    must avoid g, so F's side reads `(): g` and H's side reads `(): cg I`.
- **scratch/realizepf/control.moss** (F[I], no bracket): the oracle and
  native both accept it and print 7. So the program is accepted right up
  to the copy, including the K rebind.
- **Corpus** (scratch/orch/realizepf-cmp.sh, output in
  scratch/orch/realizepf-cmp.out): run with --elab-only over every .moss
  under tests, examples, bootstrap/examples, scratch/copyfresh and
  scratch/realizepf.
  - A is this branch's oracle.
  - B is a variant that reads each unsolved generation as the first
    generation of the solved image that is not an input generation of F,
    when there is one. This is the collision the old rule allowed, and it
    is cruder than that rule.
  - Result: 397 the same, 1 different. The difference is
    copy-fresh-collide.moss, which B accepts and A rejects. No existing
    corpus file is affected.

## Alternatives considered

- **Require `cg y` to avoid `context_gens` of the copy site.** It is
  equivalent for soundness but needs the site context, which CopyOutputs
  does not take. sigma's image is the part that matters.
- **Require sigma's image to be over declared symbols.** This is stricter
  than the oracle, which accepts a fixed input read as a fresh generation
  (the control).
