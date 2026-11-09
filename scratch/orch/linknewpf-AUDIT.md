# linknewpf: Audit change

## What

`Audit.link_site Cg Cb outs` is a new definition. It is the context a
linked body's result keys are read at:
- its table is the outputs at the body's final table (`c_ty Cb`), then
  G's own table (`c_ty Cg`);
- its merges, provisions and locals are Cb's.

LinkBody now uses it in two places:
- its `ResultRealizes` (in place of `Cb`);
- the code's result sources, `rkey_of (link_site ..) (at_site (link_site ..) ..)`.

For outputs nothing changes. read_rename and at_site read an output role
at Cb's entry, which comes first. For inputs, a result key's receiver is
read at G's own readings, so a rebind in the body is ignored.

## Why

The counterexample is bootstrap/examples/link-merge-rep-rebind.moss
(scratch/orch/linknewpf-STATUS.md has the walk).
- **Setup.** F[X] and G merge X with T, with representative T (Audit's
  world is a witness and can pick it).
- **The rebind.** G binds T = Bool, then T.m, keyed Bool.m. That is
  allowed because the pins are M(F) = {X}.
- **The lookup.** The old `at_site Cb (T.m)` gave Bool.m. G's Bool.m
  closure was installed under the caller's Concrete.m, and `if this` ran
  on a Concrete.
- **Root cause.** An input's run-time meaning at an application is the
  caller's (`application_environment`), and `bind T` writes only the
  alias. The old reading followed the alias.

## Tests

- **link-merge-rep-rebind.moss** (= scratch/linknewpf/m2-XT.moss; m2-TX
  is the other context order).
  - Audit now rejects it: T.m read at Cg is T.m, which G does not bind.
  - Oracle: "functor `G` does not bind `X.m`" in both orders.
  - Native: "the functor does not bind every item its result promises".
- **scratch/linknewpf/m5-XT.moss and m5-TX.moss** (G also binds `X.m`,
  keyed T.m since X is pinned and reads T in Cg).
  - Audit accepts both, and the application selects G's `X.m` (T.m).
    Before, at representative T, it selected Bool.m and got stuck.
  - Oracle: rejects m5-XT, where its pins include the representative T
    ("`bind T` ... changes a type its result mentions (D84 3d)"; Elab.v's
    `mf` is the gens of F's keys and signatures in F's own, merged,
    terms). It accepts m5-TX, where its representative is X, and runs it
    (exit 0).
  - Native: accepts both and runs them (exit 0).
- **scratch/linknewpf/m3-rebind-unmentioned-input.moss** (control: G
  rebinds an unmentioned input, no merges). Accepted by the oracle,
  native and Audit, before and after.

## Alternatives considered

(a) Pin the merge representatives of M(F), as the oracle does.
- Minimal, but it rejects m5-XT, which native accepts and runs correctly.
- The coordinator's rule chose (b) because the oracle accepts m5-TX.

(c) Require that the body does not rebind an input in a key's receiver.
- Same native disagreement as (a).

(b) was taken. It agrees with native on m5, and Audit stays at least as
permissive as the oracle there.

## Corpus

The oracle's own check is by written leaf, and its D84 3d pins already
include the gens of F's key receivers in F's merged terms (Elab.v `mf`).
So no oracle code mirrors the change.

Instead I compared the oracle (A) with a variant (B) whose pins leave
those receiver gens out (scratch/linknewpf/cmp.sh). A file where A and B
differ has a linked body that changes a symbol of a result key's
receiver. Those are the only programs where `link_site` and
`at_site Cb` read a key differently.
- 426 files: tests, examples, bootstrap/examples, scratch/hunt,
  scratch/linknewpf.
- 423 are the same. The 3 that differ (scratch/linknewpf/cmp.out):
  - **link-merge-rep-rebind-both.moss and m5-XT.moss**: the new tests.
  - **tests/errors/functor_output_input_changed.moss**:
    - Provider (bound to Target[T]: T -> T.out, done) binds T.out and
      then applies Make, whose output T is in force, so the application
      gives T a fresh generation.
    - Old Audit rejected it: at_site Cb read T.out as T'.out, which is
      not bound.
    - New Audit accepts it: the key is read at G's T, where the body
      bound it.
    - Both implementations reject it (oracle: "a functor provider
      changes the polymorphic result input `T` (D84 3d)").
    - Audit's acceptance is sound. T's meaning in the body is the
      caller's; the application writes only the fresh generation and T's
      alias. The closure bound at T.out was typed at that meaning and is
      installed under the caller's θ(T).out.
    - This is a new, documented divergence: Audit is more permissive
      than both implementations here.

Apart from that file, no corpus program changes verdict. In particular
the change rejects nothing the implementations accept: in a program the
oracle accepts, no linked body changes a receiver symbol, so the two
readings agree there.

## Proof repair

- **PublicBodySlice.**
  - LinkData uses `A.ResultRealizes (A.link_site Cg Cb outs)` and
    `link_sources (A.link_site Cg Cb outs) outs gen ks`. link_sources'
    definition is unchanged.
  - New lemmas: `realizes_link_site` (Realizes at Cb gives it at the
    site, by conversion), `rkey_of_link_site`, `canon_link_site`.
- **Obligation statements.** The result selections in `ChainNewOk`,
  `FamilyLinkNewOk`, `ChainCoreOk`, `LinkResultsOk`,
  `FS.FamilyLinkResultsOk`, `CopyA.ChainResultsOk` and
  `CN.ChainRealizeOk` read `link_sources (A.link_site Cg Cb outs') ...`
  in place of `link_sources Cb ...`. This is the same selection Audit's
  code now makes, not a weakening.
- **PublicChainCore.** `chain_core_ok` and `family_link_results_ok`
  apply `chain_selection` at the site.
