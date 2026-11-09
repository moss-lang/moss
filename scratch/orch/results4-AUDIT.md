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

# results4: Audit change 2 — LinkBody: no output symbol in a result key

## What
`LinkBody` gains a premise: for each result key k of F read at F's inputs
(`ProvideResult W u Cu (gen) Cr ks`), `rkey_of Cr k`'s receiver and
instantiation mention no output symbol t (raw `TAbs t`; output roles
`gen t` are fine).

## Why
Raw t in Cr is the *input* t. It arises only through F's merges: Cu's
table maps each input to itself under σ, and `output_type_map` puts the
roles first, so `T.m` with T an output reads `gen T`. A merge of X into T
(T representative) makes `X.m` read `T.m` with the input T. The code
files `output_renaming outs gen (rkey_of Cr k)`, which renames `gen T` to
T, so the input T and the output T become one symbol, and the
application's `result_substitution` (outputs first) reads it as the new
T. The caller keys the provision at X's meaning (old T), so the call
misses (e_entry). This was results3's "edge case" (an output that is also
an input, reached through a merge).

## Test
`bootstrap/examples/output-merge-key.moss`: oracle `moss_ref: stuck (XErr
6)`; native prints 7. Swapping the two readings (X the representative):
the oracle prints 7. Filed as E33.

## Evidence / scope
Native accepts and runs it, so this rejects a program native runs (an
incompleteness). The premise only bites when F's input context merges an
input onto an output symbol as representative *and* a result key
mentions it. NOT verified over the corpus (no oracle variant built for
this one); such a functor needs a merge in its input context between an
input and one of its outputs, which I expect to be rare. Where the
oracle accepts such a program, its program may be stuck anyway.

## Alternatives
Keep roles distinct: the code's keys would carry `gen t`, and the
application would map `gen t` to the destination. BApply's outputs do not
know `gen` (it is the closure's), so FunctorCode would need the role map:
an interpreter change, deferred to the user.
