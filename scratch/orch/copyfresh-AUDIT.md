# copyfresh: Audit change

## What

`Audit.CopyOutputs W sigma u h Cu rm` (BFunCopy's output check) gains two
witnesses, `ys` and `cg`:
- `ys` lists exactly the generations of F's input readings
  (`exists x tx, nlook x (c_ty Cu) = Some tx /\ In y (gens tx)`) that
  sigma does not read (`lookup_subst y sigma = None`): F's non-fixed
  inputs.
- `cg y` is a fresh symbol (`Fresh W`) for each, with
  `NoDup (map gen outs ++ map cg ys)`, so they are distinct from each
  other and from the output roles.

The provisions of F's and H's results are then compared after
`sigma ++ map (fun y => (y, TAbs (cg y))) ys` instead of sigma alone.
sigma is still F's fixed readings at the copy site, as before, and every
statement that mentions CopyOutputs (BFunCopy, `S.CopySite`, `S.CCopy`,
`case_BFunCopy`, `PublicCopyApply`) is unchanged. The only proof edit is
one destructuring pattern in PublicCopyApply.v (`chain_sources_range`),
which now names the two extra witnesses and skips their two conjuncts.

## Why

BFunCopy's sigma maps each fixed input x of F to its reading at the copy
site. That reading is in the site's terms and can name a symbol that is
also a non-fixed input of F, which each application supplies afresh.
Since Audit's symbols are global numbers, the comparison then
identified the site's meaning of that symbol with F's input.
- moss-safety scratch/hunt/hunt2-chain/t3-copy-bracket-nonfixed.moss
  (now bootstrap/examples/copy-bracket-nonfixed.moss) breaks
  `CN.ChainRealizeOk`. Here F[]'s `get[I = Z]` with Z read as demo's I
  equals H[I]'s `get` after `[Z := I]`. The application installs H's
  body at I = Bool under a key the caller reads at Char.
- moss-safety scratch/hunt/hunt2-family/v3-copy-fixed-reads-poly.moss
  (now bootstrap/examples/copy-fixed-reads-poly.moss) breaks
  `FS.FamilyCopyRealizeOk`. Here F's `A.m[U = A]` (A poly) equals H's
  `A.m[U = Y]` after `[Y := A]`.

## Mirrors the oracle

In Elab.v, `copy_output_compatible` collects `vars`, the generations of F's
formal readings. It solves F's fixed patterns against the site's readings
(`solved`), and sends each unsolved var to a fresh `@copy` symbol
(`new_sym`). `copy_slot_eq` then compares under that simultaneous
substitution. Audit's readings are identities (`FunCtx`: `TAbs t`), so a
fixed x's pattern is x, and `solved` is sigma. The oracle's fresh `@copy`
symbols are a witness for `ys`/`cg`. So whenever the oracle's
`copy_output_compatible` accepts, the new provision comparison holds too.
The key matching and positions (`rm`) are untouched.

## Test and evidence

- bootstrap/examples/copy-bracket-nonfixed.moss: the oracle (this branch's
  Elab.v, E35 fix included) rejects at `bind F = H` ("effective output
  interpretation differs (D78)"). Native (ob-family, seed 1h1710x0)
  rejects at the copy ("does not satisfy the required signature: `get`").
  The new Audit rejects: F's `get` becomes `(): I [I := I]` with the
  site's I, and H's becomes `(): cg I [I := cg I]`.
- bootstrap/examples/copy-fixed-reads-poly.moss: the oracle rejects the
  copy (D78). Native rejects at `use_it()` ("not available in the current
  context: `F`"). The new Audit rejects: `() -> cg A` against `() -> A`.
- Corpus: scratch/orch/copyfresh-cmp.sh compares two oracles. A is this
  branch's oracle. B is a variant whose unsolved vars map to themselves
  (`| None => ret (x,YAbs x)`), which is the old Audit rule. The run covers
  every .moss under tests, examples, bootstrap/examples and
  scratch/copyfresh, with --elab-only. The output,
  scratch/orch/copyfresh-cmp.out, has 389 same and 4 different. All 4 are
  hole witnesses that B accepts and A rejects: the two new examples, and
  the re-hunt shapes r1-copy-of-copy and r2-bracket-poly-swapped. No
  pre-existing corpus file is affected.
- The four named examples still satisfy the new CopyOutputs. The oracle
  elaborates all four (--elab-only), and the argument for each is below.
  - copy-extra-key: F's provisions equal H's syntactically (kk,
    `Box.m[T=Bool]`), so any substitution keeps them equal.
  - copy-fixed-bracket and linked-copy-bracket: the copy is
    `bind F = Arrays` in main. Here sigma reads I32 and Z (both fixed) as
    I32, and only Arrayed is renamed. Arrayed appears identically in F's
    and in Arrays' items, and `Array.length[I32 = Z]` against
    `[I32 := I32]` agrees after sigma.
  - family-copy-linked-bracket: sigma = [W := Concrete, Z := Concrete] and
    A is renamed. `A.len[W = Z]` and `A.len` both read
    `(): Concrete [W := Concrete]`, and A.wid is identical on both sides.

## Alternatives considered

- Requiring that sigma's image mention none of F's inputs is not
  equivalent, and is stricter than the oracle. copy-fixed-bracket reads
  its fixed Z as I32, which is itself an input of F (via Wasm).
  scratch/copyfresh/r2b-two-fixed-same-reading.moss reads two fixed
  inputs as demo's I, which is a non-fixed input of F, and the oracle and
  native both accept it and print 97.
- Requiring the image to avoid only F's non-fixed inputs is also
  stricter. It rejects r2b, where the substitution is harmless.

## Known residual (not introduced here)

When F's own context merges a fixed input x into a non-fixed
representative y, ProvideResult reads x's occurrences as y, and sigma's
entry for x is never used. The oracle instead solves y through x's
canonical pattern. On such shapes Audit can be stricter than the oracle,
and it was before this change too.
- scratch/copyfresh/r3c-target-merge-ok.moss: the oracle accepts and
  prints 97, while native rejects at the copy. Here both sides read
  `(): y`, so Audit accepts as well.
