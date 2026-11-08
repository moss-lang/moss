# Public safety and parallel frontier checkpoint

Work starts at `acc8662` on 2026-11-08, with a two-hour checkpoint beginning
at 15:01 UTC. [PROOFS.md](PROOFS.md) is the generated dependency DAG;
[proof-plan.json](proof-plan.json) records each task's exact finish condition.
All five public obligations remain open. The user approved a generation
handling correction in `Audit.v`; the production interpreter is unchanged.

## Checked safety groundwork

[PublicValueTyping.v](theories/PublicValueTyping.v) defines ground value,
record-field and handle typing. Machine representations are explicit;
record keys remain strings. A ghost heap maps allocation identities to
immutable `(array kind, held type)` pairs. The development proves:

- value/field typing survives heap extension;
- record lookup returns a value of the declared field type;
- the empty union has no values;
- checked handle typing identifies the corresponding ghost cell;
- a typed tag's contents have a suitable declared payload type.

[PublicLocalsTyping.v](theories/PublicLocalsTyping.v) proves local lookup,
assignment, scope push/pop, and heap-extension preservation for ground
local stores.

[PublicCastTyping.v](theories/PublicCastTyping.v) proves that checked tag
construction returns a typed tag or a defined trap, given correctly grounded
payload typing, the cast schema's handle shape, and complete guard arguments.
It uses public type-equality reflection in
[PublicTypeFacts.v](theories/PublicTypeFacts.v). These explicit premises
remain obligations of source typing; the theorem does not silently assume
that every accepted source derivation supplies them.

[PublicHeapTyping.v](theories/PublicHeapTyping.v) connects the ghost heap to
the predicates in the public host laws. Successful box and array allocation
extend the ghost heap, preserve its old type assignments, and establish the
extended heap predicate through the interface's weakening law. The host is
arbitrary subject to those laws, as required by semantic safety.

Callable/provision typing, immutable generation reading, source grounding,
and the full evaluation-preservation induction remain open. These data
lemmas are not a universal runtime certificate.

## A checked generation-reading mismatch

The following Moss sequence saves a type before its source name is rebound:

```moss
bind Saved = w::F64;
bind w::F64 = w::I32;
bind Boxed = Saved;
```

Statically, `Saved` still denotes the old `F64` identity. `BTypeBind` emits
that already-read identity as the right-hand side of the final binding.
Dynamically, `bind_type` grounds it through the current source-name entry,
which has become `I32`.

[PublicSavedTypeExample.v](theories/PublicSavedTypeExample.v) retains the
old mismatch as a legacy-helper calculation, derives the actual `BindItem`
instance, and checks the repaired behavior: saved `F64` remains `F64`, the
integer-handle wrapper traps, and output transfer preserves the old generation.

These are **local checked mismatches**, not a complete `WorldOf`/`Admissible`
counterexample to public safety. The larger source execution is separate
evidence against the existing implementation:

```moss
import "../../lib/wasm.moss" as w use Wasm, Gc, Boxed, StructRef, .boxed;
type Saved;
assume Wasm, Gc {
  fn main() {
    bind Boxed = w::I32;
    bind w::Heap;
    let integer_ref = w::i32_one.boxed();
    let raw = match integer_ref { w::Ref(raw) => raw };
    bind Boxed = w::F64;
    bind w::Heap;
    bind Saved = w::F64;
    bind w::F64 = w::I32;
    bind Boxed = Saved;
    let bad = w::Ref(raw);
    let y = bad.deref();
    w::f64_add(y, y);
  }
}
```

The existing reference interpreter accepts
[this source](examples/saved-type-rebinding.moss), then reports
`[moss_ref: stuck (XErr 0)]`. The bad wrapper succeeds; dereferencing returns
an integer that reaches floating-point addition. Preserving `Saved`'s old
meaning should instead make that wrapper a defined failed cast. The example
installs the floating-point dereference method before rebinding `F64`.

An [additional source](examples/saved-generic-rebinding.moss) uses generic
`T` and `U`, rather than rebinding a built-in machine name, and produces
the same reference error. Forbidding built-in rebindings alone would not
address that observation.

Reproduce from the repository root using a built oracle:

```sh
MOSS_ENGINE=ref MOSS_FASTPATH=0 MOSS_PRUNE=0 moss_m2 \
  --pages 2 --argv0 lib/prelude.moss bootstrap/examples/saved-type-rebinding.moss
```

A coherent repair must distinguish mutable source aliases, stable generation
interpretations within an activation, and output destinations. Entering a
generic call may instantiate its formals; rebinding a source name within
that activation must preserve previously emitted meanings. Fresh output
export must preserve older caller generations too. Erasing alias assignments
alone is insufficient because linked bodies currently export type outputs
through those assignments.

## Approved generation repair

The runtime environment now separates stable generation meanings, mutable
source/output aliases, and executable provisions. `bind_type` records its
already-read right-hand side in the alias map; it preserves grounding of
every type. A new activation receives its instantiated formals and lexical
capture, excluding unrelated ambient caller generations. Formal arguments
initialize both readers so stale captured aliases cannot shadow them.

`BApply` carries formal input meanings and output destinations separately.
`BApplyAbstract` specifies the exact input substitution rather than allowing
arbitrary extra pairs. Linked bodies must supply each type output. Output
transfer reads the completed body once, installs only fresh destination
generations, and separately updates caller source aliases. Missing outputs
and occupied/duplicate destinations retain an internal-error outcome; they
are not silently converted to traps to make safety easier.

Functions and methods carry an explicit `type_capture`: defined providers
capture their instantiated formal roles, lambdas capture the generations
and source readings their lexical context uses, and a forwarding wrapper
needs no type rows of its own. Functor activation reconstructs exactly
`fc_inputs`, preferring supplied arguments and otherwise using captured
source meanings. This both initializes private captured inputs and excludes
irrelevant captured generations from the new activation. The source patterns
[captured-output-scope.moss](examples/captured-output-scope.moss) and
[captured-type-input.moss](examples/captured-type-input.moss) are accepted by
the production elaborator; their public runtime fixtures are checked in
[PublicFunctorScopeExample.v](theories/PublicFunctorScopeExample.v).

[PublicCaptureScopeFacts.v](theories/PublicCaptureScopeFacts.v) proves exact
planned-row lookup, including absence of unplanned ambient rows. The lexical
capture domain and freshness checks use the same `context_gens`, covering
both stored types and their canonical readings. It also covers the merge
substitution's keys and the generations of their values, so a captured
context keeps the meaning of a merged-away key (its representative's), and
fresh generations avoid merge keys. (Earlier the merge keys were left out;
the safety proof needed them, see SAFETY-SITES.md.)

[PublicGenerationFacts.v](theories/PublicGenerationFacts.v) proves these
live helper properties, including preservation of existing generation
lookups and grounding of fully supported old types.
[PublicAliasContext.v](theories/PublicAliasContext.v) connects a real
`BTypeBind` transition to static/runtime alias agreement, with canonicalization
idempotence explicit. [Call regressions](theories/PublicGenerationCallExample.v)
check formal shadowing, lexical capture and activation isolation;
[output regressions](theories/PublicOutputTransferExample.v) check a caller
and callee independently applying the same output-producing functor.
[PublicContextFacts.v](theories/PublicContextFacts.v) derives idempotence
from actual function/functor context formation and preserves it through
type-binding and output-context updates.

The [review prototype](theories/PublicGenerationPrototype.v) is retained
as a small separate design calculation. The live helper theorems do not rely
on its definitions or assume refinement from it.

[PublicClosurePrototype.v](theories/PublicClosurePrototype.v) is updated
to the new helpers and retains all-fuel agreement with the reference.
Its [closed fixture](theories/PublicClosureExample.v) still captures the old
handle and exports a fresh type, now also preserving the caller's old type.
The complete public build/run example remains checked.

The safety statement is unchanged. Global source-formation, activation-domain
and call/export agreement still need proof. T26 closes the repaired helper
protocol; T02, T29, T30 and T31 retain the missing global invariant work.
This repair is not a universal safety proof or a port of the production executable.

## Direct finite-snapshot loader

[SnapshotLoaderBound.v](theories/SnapshotLoaderBound.v) proves a work
potential and an input-derived fuel bound, accounting for repeated worklist
pops as well as newly visited files. Its bound is two plus the number of
registered files times one more than their maximum import/prelude degree.
The maximum includes a conservative possible implicit-prelude edge.

[SnapshotLoader.v](theories/SnapshotLoader.v) implements direct typed-AST
loading, without a dump/read round trip or dump-name representability gate.
`load_sound` gives the public/shared `Bundled` predicate for successful
outputs. `load_complete` proves that any declarative `Bundled` witness
guarantees acceptance at the computed bound. Additional fuel stabilizes the
result. Invalid files outside the reachable import graph do not reject the
input snapshot.

Completeness returns some valid bundle. It does not promise equality with
an arbitrary witness containing unreachable extra modules.
[SnapshotLoaderWitness.v](theories/SnapshotLoaderWitness.v) proves that such
witnesses agree on reachable modules, and transports `WorldOf` and elaboration
to the actual loaded bundle while preserving exactly the same world and
program. Production CLI integration remains a separate engineering task.

## Public world metadata

[PublicWorld.v](theories/PublicWorld.v) reads executable loader metadata into
public worlds and proves declaration provenance/completeness and resolution
agreement. [PublicWorldScope.v](theories/PublicWorldScope.v) proves exact
public exports, scopes, attached methods, library selection and import
acyclicity, including prelude shadowing.

Its `load_world_world_of_from_requirements` theorem assembles `WorldOf`
conditionally on exactly its six requirements clauses.
[PublicWorldRequirements.v](theories/PublicWorldRequirements.v) discharges
those clauses for successful executable elaboration, including agreement
with every cached requirement list. It transports only shared source
requirement predicates and extracts the existing `ReqsReading` result before
the old model's field-numbering premise is introduced. Public `WorldOf`
therefore needs no field numbering or whole-program semantic refinement.
Context/code elaboration correctness remains separate.

## Next safety proof frontier

Use the public source derivations directly, with a small code-provenance
certificate for ordinary functions, lambda capture prologues, forwarders,
method adapters, linked bodies and primitive families. Inductive callable
and environment realization should use an external semantic grounding and
agree with physical type rows on the code's inspected support. In particular,
a forwarding wrapper has no type-inspecting code and needs no type rows
merely because abstract types appear in its signature.

After context/grounding formation, the next concrete lemmas are:

1. `Satisfies_select_ready`: satisfaction selects the exact required
   provisions and preserves their realization at public runtime keys.
2. `FnProvider_capture_ready`, then method/lambda variants: planned capture
   realizes the provider context and its grounded signature agrees.
3. `LinkBody_outputs_ready`: the source result/freshness premises ensure
   provision selection and output transfer succeed, extending caller
   realization while preserving old supported generations.

The evaluation proof can then induct on fuel, using global `FnElab` for
recursive source calls and finite inductive captured environments. This
avoids a second full expression-typing calculus. The DAG keeps formation
(T29), code provenance (T30), and callable/environment realization (T31)
visible prerequisites of the full invariant.

## Validation

The checked roots are included in
[assumptions/Certificate.v](assumptions/Certificate.v). Focused vos/vok
checks support iteration; a clean Nix oracle build checks every proof and
both certificate/closed-assumption audits. Measurements are recorded in
[the checkpoint record](benchmarks/safety-frontier-2026-11-08.json).

A quiet Dune trace identified `LinkSynthPreserve` on the critical path.
Four local cached preservation lemmas reduce its full kernel-checking time
from 25.38s to 13.58s. The theorem statements and assumptions are unchanged;
the cache shares repeated diagnostic/provider preservation proofs.
The final clean Nix build uses all 32 VM cores: Rocq compilation takes
104s and assumption audits 10s, for a complete 114s check. Including
extraction and installation, the Nix build takes 132.02s.

The production oracle's SHA-256 remains
`982ea335582de4749555434cc340b93f0889e41c07c779f51af05692748a8e23`,
identical to the measured foundation oracle. The prior default bootstrap
result of 38m08s therefore remains applicable; no changed production
interpreter is being declared verified by these new public proofs.
