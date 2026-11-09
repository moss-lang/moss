# results4: Audit change — CopyOutputs checks both directions

## What
`Audit.CopyOutputs` (used by `BFunCopy`, `bind F = H;` with H abstract)
gains a last conjunct: every key of H's result, read at F's inputs with
F's output roles, has the provision id of a key of F's result:

    (forall k', In k' ks' -> exists k, In k ks /\ provision_id k = provision_id k')

## Why
`SameResult` compares leaves (receiver symbol, method name), which drop
brackets, so H may have two items on one leaf where F has one. The
one-directional check (each of F's keys is one of H's, as the oracle's
`copy_outputs_match`) accepts that. An application of F runs the root
body and selects every key of the root's result (`fc_outputs`), so the
extra key overwrites the caller's own provision of it. Then the caller's
static context no longer describes its environment, and `ChainNewOk`'s
clause (a) ("every key dr fills is the run-time key of a new provision")
is false.

## Test
`bootstrap/examples/copy-extra-key.moss`: F's result is `kk,
Box.m[T=Bool]`, H's `kk, Box.m[T=Bool], Box.m[T=Char]`, G (H's body) binds
both. `demo` binds `Box.m[T=Char]` to 3, then `bind F;`, and prints
`Box[T=Char](a).m()` then `Box[T=Bool](true).m()`.

- Oracle (`moss_m2.exe`, safety 05a0bef): accepts, prints `21`: the extra
  key from G overwrote demo's binding. This is an oracle bug (filed as
  E32 in proof-plan.json).
- Native (`moss run`, ob-family build, seed mossc.wasm 1h1710x0...):
  accepts, prints `31`: it keeps exactly F's result, as
  docs/reference/semantics.md ("Functors": `bind F;` "keeps exactly F's
  result") says.

(Earlier attempt, `scratch/r4/copy-extra-type.moss`: making the extra
key's signature differ through an output type does not give type
confusion: the run-time key carries the instantiation (`theta_of`), so
the extra key and the caller's are different keys. Both print `a`. So
the effect is semantic (the wrong closure runs), not a stuck state,
but the static correspondence the proof relies on is false.)

## Evidence that it rejects nothing else
An oracle variant (extracted Elab.ml with the converse added to
`copy_outputs_match`, built in scratch/r4/variant) compared with the
oracle over tests/, examples/, bootstrap/examples/ (373 files,
`scratch/orch/results4-cmp.sh`): identical verdicts on all. The variant
rejects copy-extra-key.moss ("its effective output interpretation
differs (D78)").

Native accepts copy-extra-key.moss and behaves per the spec, so here
Audit (and the variant oracle) is stricter than native: an incompleteness
for a rare program shape, not a rejection of anything the oracle runs
correctly.

## Alternatives considered
1. Semantic: make Audit's application keep exactly F's result (native's
   behaviour). The closure only carries the root's code `h`; the
   application selects `fc_outputs` of h. Filtering needs F's keys at
   `BApply` (a new binding field) or in the closure (`BCopyFunctor`),
   changing the interpreter and many proofs. Too wide for now; it is the
   faithful alternative if the user prefers native's semantics.
2. Restrict `Platform.Family`/results to one item per leaf: rejects
   ordinary programs.
3. Tolerate the extra key semantically (callable at the old signature):
   needs a coherence invariant; the extra closure is still the wrong one.
