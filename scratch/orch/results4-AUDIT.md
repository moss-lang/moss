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
mentions it.

**Corpus check (done later): it rejects a corpus program.** An oracle
variant (extracted Elab.ml; in link_body, after
`fresh_target_output_view ef fg`, fail if any method key of `dr` has a
receiver, or a recorded instantiation (`get_app ef'`), mentioning
`fst` of an output role) over tests/, examples/, bootstrap/examples/
(377 files, scratch/orch/results4-cmp-E33.out): 375 same, 2 differ:
- bootstrap/examples/output-merge-key.moss (intended);
- tests/regressions/functor_input_output_generations.moss: `Target[]:
  Right, Left -> T, made, T.out, U.out` with the contexts merging U into
  T through `seed[P=T]`, `seed[P=U]`. It is the same bug: the oracle
  elaborates it and the reference interpreter stops `stuck (XErr 6)`
  after printing `n`; native prints `yn`
  (crates/moss-tests/tests/suite/backend.rs:
  `native_functor_outputs_preserve_merged_input_generations` passes,
  `oracle_functor_outputs_preserve_merged_input_generations` is
  `#[ignore]`d as a known oracle generation-capture bug; proof-plan E06,
  S11 cite the test).
So the language intends to accept this shape, and the LinkBody premise
rejects a program native runs and the project tests. Reported to the
coordinator; the faithful fix is the "separate output roles" semantic
alternative below.

## Alternatives
Keep roles distinct: the code's keys would carry `gen t` (no
output_renaming), and the application would map `gen t` to the
destination and every input by θ. BApply's outputs do not know `gen` (it
is the closure's), so FunctorCode needs the role map (a new field, or
roles recorded in fc_outputs), apply_functor builds
`result_substitution th (map (gen t, dest t))` for the keys while
read_result_types still reads the body's meaning of t. Size estimate:
not small. FunctorCode is built by LinkBody and Platform.Family and
destructed in the interpreter and its proofs (PublicLinkResults,
PublicChainCore, PublicPlatformApply, PublicBodySlice app_step, the
refile/fc_sources proofs). The interpreter is also the oracle's
`moss_ref`, so the oracle's link_body must emit the roles too (fixes
E33/E06's case). Roughly half a day to a day including proof repair.

## Change 2 REVERTED (coordinator's decision)

The LinkBody premise is removed (it rejected the corpus regression
functor_input_output_generations.moss). Audit is again wrong for that
shape until change 3 lands; ChainKeysOk is false for it meanwhile.

# Change 3 (decided, NOT done): separate output roles — handoff

Plan (Audit):
- `functor_code` gains a last field `fc_roles : list (tsym * tsym)`
  (output symbol t, its role gen t). Platform.Family: `[]`.
- LinkBody: Dr keys `rkey_of Cr k` (no output_renaming); fc_sources'
  first components likewise; the body side stays
  `rkey_of Cb (at_site Cb (output_renaming outs gen k))`; fc_roles =
  `map (fun t => (t, gen t)) outs`.
- apply_functor: the selection uses
  `result_substitution th (role_outputs (fc_roles code) outputs)` with
  `role_outputs roles outputs := map (fun q => (snd q, dest of fst q in outputs)) roles`
  (raw t then reads by θ as the input it is; gen t maps to the
  destination). read_result_types is unchanged (it reads ITy t from d3).
- Statements to restate: LinkResultsOk (PublicBodySlice ~4649),
  ChainResultsOk (PublicCopyApply ~280), ChainCoreOk (PublicLinkResults
  ~346), ChainNewOk (PublicChainCore ~206), ChainKeysOk/ChainRealizeOk
  (PublicChainNew), FamilyLinkNewOk and FS's family link statements
  (PublicFamilyStep), LinkData (PublicBodySlice ~3246), plus the 13
  FunctorCode constructions in examples (AuditExamples,
  PublicClosureExample, PublicOutputTransferExample,
  PublicGenerationCallExample, PublicFunctorScopeExample) and
  PublicPlatformApply/PublicCopyApply/PublicEntry.

Important finding for the oracle side: the oracle's runtime is NOT
Audit's interpreter. `moss_ref` runs ExecutionModel's program (Elab.v
emits ExecutionModel code; the fast interpreters CachedClo/LazyX/CloX/
FastX/CoreFragment and their equivalence proofs consume it). So the
oracle half of change 3 means changing ExecutionModel's functor code and
application (and every fast interpreter plus its proofs), or an
oracle-only workaround in Elab.v's link_body. That is a large job; scope
it before starting (LinkOutputFresh.v's `link_output_renaming_actual`
and LinkBodySound.v relate the oracle's output_role_item to Audit's
output_renaming and must change with it).
