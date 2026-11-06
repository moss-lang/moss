# Static array pooling

The [native pass](../../crates/moss-cli/native/static_bytes.cpp) normalizes
constant GC-array initialization and pools eligible byte contents. [Quoted literals](../reference/semantics.md)
desugar into ordinary contextual construction calls. The default
[GC string provider](../../lib/gcstring.moss) implements those calls;
[generic fixtures](../../tests/fixtures/array_literals/main.moss) also exercise
numeric rebinding without string-specific backend knowledge.

The concrete fixture represents text as a union of nominal literal and dynamic
variants backed by distinct GC arrays. The compiler keys single-scalar arrays
by their full Moss element type, and emits GC types in one recursive group,
so distinct nominal element tags give distinct backing types even when both
store `i32`. No string-specific backend annotation is involved.

## Two selectable targets

```sh
moss build -O3 --static-bytes memory program.moss -o memory.wasm
moss build -O3 --static-bytes globals program.moss -o globals.wasm
```

`memory` is the default at `-O2`, `-O3`, `-O4`, `-Os` and `-Oz`. Specifying
`--static-bytes` without `-O` implies `-O3`. `-O0` and `-O1` retain their ordinary
Binaryen pipelines and do not pool arrays.

| Property | `memory` | `globals` |
| --- | --- | --- |
| Literal evaluation | Unboxed i31 handle | Private GC-global reference |
| Payload storage | Active data in a private memory per eligible type | Passive data copied into deduplicated GC arrays at startup |
| Extra linear memory | Yes, at least one 64 KiB page for a nonempty pool | None |
| Startup GC allocations | None | One per unique payload |
| Work proportional to literal length when evaluated | None | None |
| Read implementation | Bounds-checked byte loads | Ordinary GC array operations |
| Nonempty pool requires | GC and multi-memory | GC and bulk-memory |

Both targets deduplicate equal contents within a backing type and retain
ordinary dynamic constructions when their contents are not constant. Empty
arrays need no data segment or additional linear memory. The globals target
uses an immutable empty-array global; the memory target uses an i31 sentinel.

The [globals target](../../crates/moss-cli/native/array_globals.h) creates a
module start wrapper that initializes its globals with `array.new_data`, drops
its private passive segment, then calls the original start. Active segments
have already been dropped by that point, so they cannot supply those bytes.
`array.new_fixed` is valid in global initializers but would encode every byte
as an instruction operand instead of using a data segment.

The current Moss array provider stores unpacked `i32` elements. The memory
pool compresses byte-valued elements to one byte; `array.new_data` must use four
little-endian bytes per unpacked element in the globals target. Native packed
`i8` arrays are supported by both targets, including signed reads and truncation.

## Initialization and eligibility proof

Preparation performs Binaryen's normal optimizations, inlining with a
20-instruction floor, GUFA, dead-argument elimination and local cleanup. It
preserves the distinction between literal and mutable backing types.

Our normalization recognizes a constant-sized `array.new`/`array.new_default`
assigned to a local, followed immediately by constant, in-bounds stores through
that local. It also recognizes an allocation in the first store's `local.tee`.
It crosses no reads, aliases, calls, effectful operands or control flow.
Complete store runs become `array.new_fixed`; empty/single-element fills can
also become fixed constructors. Larger fill allocations are not expanded just
because their size is known. Normalization has a 10,000-element allocation
limit and a per-function synthesis/scan budget, and only converts store runs
that pay for the fixed constructor's operand count.

The [copy normalizer](../../crates/moss-cli/native/array_copy.h) also exposes
bounded leaf writers and small GC copy helpers through ordinary inlining.
Constant selector arguments can then fold, including selectors over union or
record payloads. It tracks known fresh arrays
and recognizes a bounded, complete byte-for-byte copy loop into an unobserved
destination. Source and destination lengths, indices, stable bounds and
in-bounds accesses are checked before replacing construction with a fixed
snapshot. Unknown calls, escapes and unsupported control flow invalidate facts;
observable reads prevent rewriting the earlier destination allocation. Local
initialization effects are retained. After removing a copy, initialization
normalization runs again so ordinary cleanup can remove dead builders.
The production literal fixture exercises Unicode widths two through four and
all six typed prefix variants at every pooling optimization level.

Before pooling, every use of the candidate type is checked. Candidates are
final, unshared arrays with packed `i8` or unpacked `i32` storage. Private direct
calls, locals, tuples, structured control flow, null checks, non-null assertions,
reads and length queries are supported. Per-component checks prevent a tuple
from hiding reference erasure in one component behind a precise type in another.
Identity comparisons, casts/tests exposing the backing, post-initialization
writes, unsupported consumers and reference escapes prevent pooling. Related
source globals, tables, tags and public/imported ABIs remain conservative barriers.

A separate [initialization proof](../../crates/moss-cli/native/array_init.h)
admits a fresh destination assigned immediately before a pure initialization
loop, with no reassignment or escape during that phase. Such a destination remains GC backed, including when
its initial allocation is a fixed constructor. This lets runtime copies coexist
with pooled constants of the same type. Any later or unrecognized write still
blocks pooling for the type.

Only constant, byte-valued fixed constructors enter the pool. Runtime-valued
fixed constructors and runtime initialization loops retain GC backing. The
memory target widens affected value flows to `eqref` and uses typed read helpers; an all-static type has no dynamic
array branch. Null and out-of-bounds reads still trap. The globals target
preserves original value types during pooling, including exact-reference facts.

Pool data is bounded at 16 MiB per type, including unpacked i32 expansion in
the globals target. Generated memories, data, globals and helper functions are
checked against target limits before mutation. Normalization and pooling retain
effects and traps, under the ordinary compiler resource boundary that permits
allocation elimination. The globals target moves GC allocation and byte-copying
cost to instantiation; the memory target initializes the private memory instead.
Neither preserves GC allocation counts or allocation-exhaustion behavior.

## Type merging and union dispatch

Explicit type merging runs only after pooling in globals mode, and only when
imports/exports provide no reference-valued host boundary. Numeric WASI imports
meet that condition. Otherwise foreign modules could pass structurally matching
GC types through an abstract reference ABI, making closed-world type rewriting
observable. Pooling still works without merging in that case. A conservative
[dispatch simplifier](../../crates/moss-cli/native/array_dispatch.h) factors
identical array reads/length queries out of branches when moved expressions are
pure and nontrapping. Unselected null references must remain unobserved.

After merging, the [union coalescer](../../crates/moss-cli/native/array_union.h)
can contract a private tag/two-reference interface to one reference. It checks
that every use selects the same reference, and that local producers carry a
known tag with a null inactive slot. Argument evaluation order is preserved.
SSA and cleanup expose local aliases and function results between bounded
rounds. A private function that always returns one variant can also return
just its active reference: callers reconstruct the exact original constant tag
and null slot before cleanup. This preserves even explicit observations of those
components and handles effectful call operands. Other observed tags or inactive
slots, exposed interfaces, and unsupported producers retain their original
representation. Aggregate field layouts are not contracted by this pass.

The compiler now gives incompatible reference payloads separate typed union
slots, preserving precise backing identities without casts through `anyref`.
Reordered and widened unions remap those slots by nominal variant. Nested
records, function parameters/results and recursive boxes use the same physical
layout. Layout discovery compares semantic reference-lane keys before creating
GC types, avoiding incomplete recursive structs.

The memory target currently declines candidates stored in other GC heap
aggregates, because their reference fields would also need representation
rewriting. This is separate from unrelated mutable arrays, which do not block
literal pooling. The globals target admits private typed struct/array containers, checking
concrete reference flow through their fields and elements. Container identity
is preserved; leaf literal identity and mutation remain forbidden. Erasing or
exporting a related container still prevents pooling. Unused container type
declarations alone do not block either target.

## Build and checks

The C++20 pass uses internal headers from the exact Binaryen revision linked by
the Rust driver. The Nix shell sets `BINARYEN_INCLUDE_DIR`; see
[CONTRIBUTING](../../CONTRIBUTING.md). C++ exceptions never unwind across Rust;
a failed or invalid rewrite discards its module and reports a compiler error.
Binaryen's global options are protected by the driver's optimizer mutex.

[Native tests](../../crates/moss-cli/src/static_bytes_tests.rs) validate and run
source and transformed modules for both targets: initialization effects,
partial observations, bounds/null traps, identity/escape rejection, mutation,
packed/signed and wider values, deduplication, empty arrays, startup chaining,
feature/limit checks, tuple erasure and dispatch factoring.
[Integration tests](../../crates/moss-tests/tests/suite/static_bytes.rs) cover
both targets at every pooling optimization level, raw/oracle agreement,
runtime-selected literal/dynamic strings, numeric protocol rebinding, reordered
and widened unions, nested records, recursive boxes and the compiler fixpoint.

The default provider copies on finish so retaining a builder cannot mutate a
finished String. Constant copy construction is eligible for normalization;
runtime construction retains its ordinary GC backing. The
[explicit raw provider](../../lib/wasi.moss), `WasiRawStd`, remains available
for programs that intentionally use linear-memory string handles. WASI preview
1 output copies GC strings into temporary linear memory at the host boundary.

Remaining representation work includes contracting aggregate String fields
and extending memory-target lowering through typed heap containers. Unsupported
patterns keep their ordinary representation and semantics.
