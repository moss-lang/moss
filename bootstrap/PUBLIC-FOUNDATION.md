# Public proof foundation: first checkpoint

Work started from `049546b` on 2026-11-08. The checkpoint is two hours of
active work; expensive bootstrap measurements run in the background with
an independent one-hour cap for each configuration.

## Finite source contract

`Audit.source` is now an explicit finite snapshot. Each entry contains a
canonical absolute `file_path` and its byte-string contents; entry path
names are unique. The entry order is an implementation detail, not import
precedence. `source_read` looks up a registered path and exposes its contents
only when they pass the existing UTF-8 check.

The verified build API receives this complete data value. The caller or
host adapter provides a finite inventory and its contents; the verified
function does not discover the domain of an arbitrary filesystem oracle.
The inventory may contain paths outside the entry directory. No project-root
restriction, module-count limit, or blanket validation of unrelated files
is added to the semantic `Builds` predicate. Missing imports fail because
the supplied snapshot does not contain them.

Filesystem snapshot collection is a host input boundary, like the existing
file-I/O boundary. The eventual CLI adapter must state how it supplies that
inventory; this change does not claim to verify OS enumeration or file I/O.
Runtime file reads remain part of `Public.state` and are separate from the
finite build snapshot.

[SourceSnapshot.v](theories/SourceSnapshot.v) proves exact lookup/read
membership, finite readable support and canonical readable paths. The
existing complete public build/run example uses the new snapshot directly.
`source_from_files` checks path uniqueness and constructs the snapshot;
its soundness/completeness lemmas require no caller-supplied uniqueness proof
for construction and prove acceptance of every unique canonical inventory.
These are input-contract facts, not loader or elaborator completeness.

## Consequences for loader completeness

A finite snapshot makes an input-derived traversal bound possible, but the
existing `Parse.walk` consumes fuel even when popping an already-seen path.
Therefore `length source_files` alone is not a sufficient bound: repeated
imports and prelude worklist entries must be counted too.

One concrete route is to parse/cache each registered file independently,
retaining parse failures as entries rather than rejecting unused files.
For each successfully parsed file, count explicit imports plus a possible
prelude edge. The initial entry plus all such edges bounds worklist pops;
an additional step handles the empty worklist. Alternatively, use a
well-founded worklist algorithm with the equivalent finite-inventory measure.
Both routes require proofs connecting registered paths, parse results,
visited modules and worklist multiplicity. The existing fixed `100000`
loader budget is not claimed to be justified by this change.

Finite files likewise do not automatically establish termination or
completeness of type checking, provider demand or elaboration. Those retain
their separate resource obligations in the roadmap.

## Runtime-boundary experiment

The public runtime syntax, type/provision environments and lexical captures
are the semantic target. The experiment should compile a representative
binding/application operation to closures over those same public structures,
prove its agreement with the public reference evaluator, and exercise saved
input meanings, fresh output meanings and checked GC casts. Any unsupported
cases or table/preparation premises must remain explicit.

This tests a local closure/lowering boundary. It does not require encoding
an entire public program back into the old source-elaboration model, and it
does not establish a complete executable elaborator/interpreter certificate.

The concrete proposal is now:

1. Keep the executable elaborator's useful metadata, but emit `Audit.ty`,
   `Audit.expr`, unified `Audit.binding`s and `Audit.program` directly.
   [PublicEmit.v](theories/PublicEmit.v) implements the type-metadata emitter
   and proves that it commutes with the existing input-bounded substitution.
   Record field names are preserved directly. Context meaning, machine
   initialization and whole expression/function emission are still open.
2. Compile public code into closures whose semantic environment contains
   the same separate public type meanings and provisions. The eventual fast
   representation may cache maps or defer substitutions, with local
   correspondence lemmas. It does not become a second source-language model.
3. Cache functor bodies as public output headers plus compiled binding
   closures. The prototype's table correctness condition relates those
   cached headers/bodies to the same public program. Recursive table
   preparation is a separate remaining obligation, not an assumed final
   implementation guarantee.

[PublicClosurePrototype.v](theories/PublicClosurePrototype.v) implements a
partial compiler for unit/local/provision/tag expressions and binding
chains. All binding forms are supported, but value initializers must be in
the supported expression fragment. Unsupported expressions return `None`;
there is no interpreter fallback. Its agreement theorem covers every fuel,
environment, locals, state and outcome, assuming the explicit compiled-table
condition. Function/method invocation, general control flow, collections,
fast environment caches and production integration remain to implement.

[PublicClosureExample.v](theories/PublicClosureExample.v) discharges the
table condition for a concrete nonrecursive runtime fixture. Its compiled
and reference executions agree at every fuel. The fixture captures an I32
checked handle, rebinds the caller's provision, starts with an F64 meaning
for an output name, then exports a fresh I32 generation and checks the cast
against that new generation. It returns the original captured handle.
The handle is supplied as runtime input: this is not a heap-allocation law
or a source `Builds` derivation.

An extracted microbenchmark varies the handle address across one million
executions to prevent a constant input/result from replacing the workload.
It uses the same flambda extraction conventions and preserves compiled child
code sharing. This measures one small operation, not compiler bootstrap
performance; the fully integrated pipeline must still pass X14.

For this fixture, three native runs of one million varying-address
executions took 0.162–0.175 seconds compiled and 0.184–0.193 seconds using
the reference evaluator. Allocations were 2,864 and 2,736 bytes per execution
respectively. These are small-fragment measurements, including input
construction; they do not measure the existing optimized engine or predict
full bootstrap behavior. The proof/extraction/probe sources and raw results
are retained in `scratch/closure-probe` and `scratch/PublicClosureProbe.v`.

## First direct semantic-safety component

[PublicCaptureFacts.v](theories/PublicCaptureFacts.v) proves that substitution
depends only on the generations a public type mentions. It then establishes
that adding a new caller behind a captured environment preserves grounding
of every type whose mentioned generations are present in that snapshot.
Captured provisions are likewise unchanged. These results refer directly
to Audit and require no executable elaborator, old-model representation
bridge, compiled table or concrete host implementation.

The completeness-of-captured-scope assumption is explicit. This does not yet
prove that every semantic binding constructs such a complete snapshot, or
that function application/heap operations preserve the full runtime typing
invariant. Those remain T02/T07/T12/T16 obligations. The next safety-design
step is to settle value/heap typing and callable/environment validity over
the public structures, using these capture facts as checked groundwork.

## Configuration measurements

`benchmark-configs.py` freezes one extracted oracle plus `src/`, `lib/` and
the Wasm feature policy before launching comparisons. It records source and
executable hashes, configuration settings, phase logs, full fixpoint results
and timeouts. Four configurations retain the lazy closure engine and vary
only the optional transformations:

| Configuration | Fast-path pass | Pruning pass |
| --- | --- | --- |
| `default` | On | On |
| `closure-only` | Off | Off |
| `prune-only` | Off | On |
| `fastpath-only` | On | Off |

Each successful run validates S1, optimizes it, runs it to produce S2 and
checks S1 = S2. Passing configurations must also produce the same compiler.
Measurements on the existing pipeline inform the configuration choice; they
do not establish the performance of the future fully ported public pipeline.

The supervisor's timeout path was exercised with a one-second run: it killed
the interpreter process group and recorded failure after 1.006 seconds.
Each measured configuration records its actual result; a timeout is not a
passing fixpoint or evidence that the configuration satisfies the hour limit.

### Results and selected path

The [pinned measurement record](benchmarks/foundation-2026-11-08.json) includes
the source/oracle hashes, configuration flags, tool versions and phase times.
The four configurations ran concurrently on the 32-CPU local x86 VM.

| Configuration | Result | Elapsed |
| --- | --- | --- |
| `default` | S1 validates and S1 = S2 | 38m08s |
| `closure-only` | Timed out during interpreted S1 generation | 60m |
| `prune-only` | Timed out during interpreted S1 generation | 60m |
| `fastpath-only` | Timed out during interpreted S1 generation | 60m |

The freshly checked oracle is byte-identical to the frozen benchmark oracle.
The source/compiler inputs were unchanged throughout the runs. Five-second
CPU samples found that the two configurations without pruning spent about
78–80% of cycles in two environment lookup functions; the pruning-only
sample spent about 39% in type equality/related traversal. These are local
samples, not whole-run profiles, but they explain why dropping the passes
is not currently an evidenced shortcut.

The selected initial route is therefore:

- Emit public programs/types/bindings directly from executable elaboration.
- Compile that public code to fast closures, with a local environment/cache
  relation and checked functor/function preparation. The semantic boundary
  uses public values and interface calls; any packed representation or host
  conversion is a local implementation obligation, not a second whole-source
  elaboration model or a public-to-old-program encoder.
- Retain the lazy environment/closure techniques and both optimization
  passes' performance effects for the initial port. Adapt their correctness
  arguments at the selected runtime boundary; current old-model theorems
  are not automatically public-model proofs. Omitted-pass configurations
  have not met the one-hour limit on the current pipeline.
- Prove semantic safety directly against Audit, independently of that port.

This closes the initial configuration decision, not production integration.
The exact lowered invariant belongs to L01/L03, the generic public host
adapter to H01, whole elaboration to E02/E11/E12, and agreement to X09/X10.
The integrated public-target implementation must still pass X14. A later
implementation may revisit optional passes when new measurements justify it.

## Next concrete frontier

The architectural experiment is complete ahead of the two-hour checkpoint.
The next independent design/proof task is T02: define public value/heap
typing and callable/environment validity, including complete captured type
scopes. Use the proved T22 capture facts; do not assume the fundamental
source-to-runtime typing theorem as a callback.

In parallel with that proof direction, E01 can relate executable metadata
to the public world using the verified type emitter, and F07/F08 can replace
the loader's arbitrary budget with the finite-inventory worklist argument.
Each task retains its own finish condition; all five public obligations and
the final integrated bootstrap gate remain open.
