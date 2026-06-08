# Lowering

The lowering phase (`crates/moss-core/src/lower.rs`) turns a parsed Moss program
into the static IR that codegen (`wasm.rs`) consumes. Because Moss resolves
types, functions, and values *contextually*, the bulk of lowering is **context
resolution**: figuring out, for every use site, which contextual definitions
satisfy its needs. This page records how that works, the design decisions behind
it, what currently lowers, and what's left.

## The IR in one paragraph

`IR.nodes` is a hash-consed arena of `Node`s representing types and contexts as
lambda-calculus terms over **de Bruijn _levels_**. A "lambda" abstracts not over
runtime values but over *contextual parameters* (`needs`). A `Need{Ty,Sig,Val,Ctx}def { level, def, param }`
is an occurrence of contextual definition `def`, supplied by the binder at
`level`, witnessed by `param`. A `Bind*` node binds such a need to something.
Top-level declarations each become a `Lambda` over their needs returning the
appropriate thing; crucially that returned thing differs by kind:

| Declaration | body returns | transparent? |
| --- | --- | --- |
| `type T;` (Tydef) | "nothing" (abstract — identity is nominal) | opaque |
| `type T U;` (Tagdef) | the inner type, but the type itself is nominal | opaque |
| `type T = U;` (Aliasdef) | the aliased type | **transparent** |
| `fn f(): R;` (Sigdef) | a signature | opaque |
| `fn f(): R { … }` (Fndef) | the function | **transparent** |
| `val v: T;` (Valdef) | the value's type | opaque |
| `context C = …;` (Ctxdef) | a context (list of members) | (composite) |

"Transparent" means reduction may substitute the body; "opaque" means it must
not (a sig's body is a *signature*, not a function; a type's is "nothing") — see
the `reduce` decision below.

## Pipeline

`prelude()` precompiles `lib/*.moss` (building `Base`, the table of
compiler-known definitions) → `lower()` lowers the user file
(`program`: `imports` → `decls` → `bodies`) → `wasm()` monomorphizes + emits
WebAssembly. Codegen is a **separate, later phase**; everything below is lowering.

## The resolution engine (key functions in `lower.rs`)

- `need` / `needs` — build `Need*` nodes for a declaration's contextual parameters.
- `invoke_need` — resolve a target lambda's needs against an available context,
  constructing providers (`resolve_need`) or propagating still-unmet needs.
- `extract` / `extract_lambda` / `try_extract_lambda` — find which context
  *slot* provides a needed definition, parameterized by a `DefKind { Ty | Sig | Val | Ctx }`
  tag (one unified matcher, not four copies). `extract_ctx` resolves a composite
  context member-wise.
- `synthesize` / `unify` — decide equivalence **and** solve for metavariables.
  `unify` is an env-threaded simultaneous walk: the `Renaming` relates `a`-side
  and `b`-side levels as it descends under binders, so equivalence-up-to-renaming
  is decided in the walk (there is no separate normalization pass). It reduces
  both sides to WHNF first. Asymmetric: the `b` side carries the metavariables.
- `reduce` — weak-head normal form: β-reduce `Apply`, project through `Get`
  (exploding composite contexts), unfold the **transparent** `Fndef`/`Aliasdef`,
  and leave the **opaque** `Tagdef`/`Need*`/`Bind*` heads in place. Applying a
  `Bind*` head projects out the bound value.
- `inline` — the β-step (substitute a lambda's needs with a construct).
- `explode` — turn a composite context into its member list.
- `invoke` / `invoke_force` — resolve a contextual lambda's needs against the
  ambient context, yielding the construct (argument list); contrast `reduce`,
  which evaluates to a value.
- `unique_option` + `at_least_as_specific` — coherence: among candidates, pick
  the most specific (see specificity decision below).
- `sig` (on `LowerBody`) — extract a function's `(param, result)` signature,
  reducing the applied callee and reading a contextual `Sigdef`'s signature via
  the scoped `signature` helper.

## Design decisions (the "why")

1. **Equivalence is decided by `unify` carrying a level `Renaming`, not by
   canonicalizing levels.** De Bruijn *levels* aren't stable under relocation, so
   a re-leveling "normalize" pass can't canonicalize the cases that matter (free
   variables across contexts) and is unsound if half-built. `unify` is already a
   simultaneous walk, so equivalence-modulo-binder-renaming belongs there, with
   the env as the explicit channel for the enclosing-context correspondence. The
   old `LevelSet`/`free`/`reduce`/`normalize` were removed.
2. **`reduce` distinguishes transparent vs opaque definitions.** It unfolds
   `Fndef`/`Aliasdef` but leaves nominal types and contextual needs opaque,
   because their declaration bodies are *metadata*, not substitutable values.
   Reading a contextual function's signature is therefore a *scoped* query
   (`signature`), not part of general reduction.
3. **Literals are contextual, not primitive values of contextual types.** A
   numeric literal desugars to Horner-form arithmetic over digit `val`s
   (`digit0..9`, `radix`) and `ops` `add`/`mul`; a string literal desugars via a
   string-builder triple over `char_from_codepoint`. See
   [`reference/literals.md`](../reference/literals.md). The only irreducible
   primitive is the literal token's data; every type and function stays
   contextual. (Folding to constants is a codegen optimization, not done yet.)
4. **Specificity is binding-aware.** When a need is provided both bound
   (`BindTydef{def}=V`) and abstract (`NeedTydef{def}`), the bound one wins. This
   is a *directional*, *scoped* relation (`at_least_as_specific`, used only in
   `unique_option`): it reduces candidate heads with a non-projecting
   `head_reduce` (so the binding stays visible) and rules `Bind{def}` ≥ `Need{def}`
   but not the reverse. It is deliberately **not** folded into general `unify`
   (doing so over-broadens and picks wrong winners). See
   [`reference/context.md`](../reference/context.md).
5. **`assume` is the base layer of the context.** A file's `assume` bindings are
   folded into the base of every declaration's needs (below the declaration's own
   `[needs]`, which are below any `bind`). The full context *stack* (frames,
   top-down search, intra-frame-only ambiguity) is not yet built — the current
   resolver treats the slot list as one frame and ranks by specificity.

## Current state

The engine — resolution, the env-threaded `unify`, the transparent/opaque
`reduce`, signature extraction, type-equality, the literal desugar, binding-aware
specificity, and `assume`-folding — lowers the **entire core standard library**
(`std.moss` and dependencies): declarations and function bodies, including
`println`'s string literal desugaring all the way through the contextual chain
(string builder → `char_from_codepoint` → `Uint` codepoint → Horner digits →
arithmetic ops). Precompilation continues through `wasip1.moss`/`wasm.moss` into
`wasi.moss`, whose `bootstrap` body is the current frontier.

The immediate next blocker is a **lowering** hole, not codegen: `invoke`'s
`Node::BindTydef` arm (`invoke` is the application engine that resolves a
contextual lambda's needs into a construct, and several of its arms are still
`todo!()`). It's reached via `numeric → digit → val_at → bind_tydef` while
desugaring a literal under a `Number=T` binding. `wasm.rs` codegen is entirely
**untouched**.

## Remaining work

- **Finish `invoke` (the application engine)** — the immediate frontier. Several
  `invoke` arms are still `todo!()`; the next is `Node::BindTydef` (peeling a
  type-binding wrapper to invoke its inner lambda). Filling these — by analogy to
  how `reduce` handles the same node kinds, but producing a construct rather than
  a reduced value — is what stands between here and the prelude lowering fully.
- **Codegen (`wasm.rs`)** — the big one, entirely unstarted: every `Expr` arm
  (`Call`/`Val`/`Param`/…), monomorphization of contextual values (Wasm globals
  vs extra params), string data segments, the realize/IO intrinsics, layouts.
- **`hello.moss`'s `main`** — lowers once the prelude fully precompiles.
- **Deferred lowering features:**
  - The full **context stack** (needed by `rebind.moss`/`context.moss`, which
    override context via `bind` — flat slots would make the rebound thing
    ambiguous with the original instead of shadowing it).
  - **Order-independent** declaration/member resolution (the `program`
    "declaration order shouldn't be significant" TODO; currently worked around by
    ordering members, e.g. in `wasi.moss`).
  - **Partial application of parametric composite contexts** (the
    `explode`/`try_extract_lambda` "partially-applied parametric composite
    contexts" TODOs).
  - **`static` / compile-time evaluation** for efficient literals of
    user-defined number types (see `reference/literals.md`); `static` was dropped
    for the prototype.
- **Owed reviews / hardening:**
  - `extract_ctx`'s level/needs threading (the member-wise synthesis).
  - The `NeedCtxdef` arm's direct `match_need` *plus* explode-and-search — confirm
    it can't produce spurious ambiguity in `unique_option`.
  - **Adversarial `unify`/`synthesize` tests.** Coherence rests on `synthesize`
    soundness (both false negatives and false positives); it's only smoke-tested
    by prelude lowering so far. See the probes under `tests/questions/`.
