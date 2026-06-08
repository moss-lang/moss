# Backend (Wasm codegen)

This page is a handoff for implementing the backend — the phase that turns the
lowered IR into a WebAssembly module. It lives in
[`crates/moss-core/src/wasm.rs`](/crates/moss-core/src/wasm.rs) and runs after
[lowering](#relationship-to-lowering).

## Status

As of this writing, **lowering is complete** for the examples that exercise it:
`hello`, `escape`, `params`, and `reassign` lower end-to-end (prelude + program)
and reach codegen. (`context`, `rebind`, `true` need contextual-value resolution
finished in lowering; `fizzbuzz`, `unchecked` need operator/method lowering — both
are lowering tasks, not backend tasks.)

Compilation currently panics in `Wasm::wasi_ctx`. Everything below the surface of
codegen is stubbed: the backend is a near-from-scratch build, not a few hole-fills.

## The pipeline

`wasm(ir, names, lib, main)` constructs a `Wasm` and calls `program()`, which:

1. `wasi_ctx(...)` — builds the **root context** `Object` that `main` runs against
   (the concrete `Std`/`Wasi` context, mapped to Wasm). **stub.**
2. Monomorphizes `main` against that context and walks every reachable function,
   calling `func(funcidx)` to emit each. New `FnDef(fndef, ctx)` pairs discovered
   during emission are appended to `funcidxs` and picked up by the loop.
3. Emits the data global, the linear memory, the `_start` function (calls `main`
   with the root context, then `proc_exit`), and assembles the module sections.

The driver is already written; the stubs are in the functions it calls.

## The object model

`Object` (an interned `IndexSet`, like the IR's nodes) is the **codegen-level
representation** of everything static — the analogue of IR `Node`s after
monomorphization. Key variants:

- **Types:** `TyI32`, `TyI64`, `TyTuple(range)`, `TyMemIdx`, `TyLitInt/Char/String`,
  `TyCtx` (a "type" standing in for a context).
- **Functions:** `FnInstr(Instruction)` (a Wasm instruction), `FnWasi(funcidx)` (an
  imported WASI P1 function), `FnDef(FndefId, ctx)` (a defined Moss function
  monomorphized in a context), `FnInt/FnChar/FnString` (literal realizers), `Sig`.
- **Values:** `ValU32(n)` (a compile-time-known `u32` constant), `ValDyn(ty, start)`
  (a runtime value living in Wasm locals `start..start+layout_len(ty)`), `ValStmt`
  (a statement's unit result).
- **Contexts:** `Ctx(range)` — a context as a tuple of slot `Object`s.

### Values and layout

A runtime value is a `ValDyn(ty, start)`: its Wasm representation is the locals
`[start, start+layout_len(ty))`. The helpers are already written:

- `layout(ty, f)` walks a type `Object` and calls `f` once per Wasm `ValType`
  (`TyI32 → I32`, `TyI64 → I64`, `TyTuple` flattens). `String` is `(i32 ptr, i32
  len)` — see `string()`, which writes literal bytes to the data section.
- `get(instr)` pushes a value's locals onto the stack; `set(ty)` pops the stack
  into fresh locals and returns a `ValDyn`. `make_locals`/`get_locals`/`set_locals`
  are the primitives.

So the body interpreter's job is mostly: materialize each instruction's value into
locals, using `get`/`set` and the layout helpers.

## The one missing abstraction: a monomorphizing evaluator

Every stub bottoms out on a single function that does not yet exist:

> **`eval(node, ctx) -> Object`** — given an IR `Node` (a type, value, context,
> `Apply`, `Get`, `Need*`, `Lit`, `Lambda`, `Sig`, `Tuple`, `Tagdef`, …) and a
> concrete context `Object`, produce the corresponding codegen `Object`.

This is the codegen analogue of lowering's `reduce`/`invoke`: it resolves
contextual references against the concrete `ctx` and reduces applications, but it
produces `Object`s (and, for values, may emit Wasm). Where each stub needs it:

- `interp`: both `let ty = todo!()` sites are "`eval` this IR type node to an
  `Object` in `ctx`" (for the `If` block type and each `Expr` instruction's type).
  Its final `todo!()` is the body's result object.
- `expr`: `Val` is "`eval` the val to an `Object`, emit its value"; `Call` is "`eval`
  `func` to an `FnDef`/`FnInstr`/`FnWasi`, emit the call (a defined call needs its
  `ctx` registered via `insert_func`/`get_funcidx` so the driver emits it)"; `Bind`
  is "`eval` the context node to a `Ctx`"; `Param`/`Nominal` likewise.
- `val_u32(ctx, valdef)`: the constant specialization — `eval` a val to a `ValU32`
  and return its `n`. Used pervasively by `wasm_instruction`/`memarg` for memidx,
  align, offset, etc.
- `wasi_ctx`: `eval` applied to the root — build the `Ctx` for `Wasi` by mapping
  each member (see below).
- `func`: needs the monomorphized signature (`eval` the fndef's `Sigdef` in `ctx`)
  for param/result layout, and the body to `interp`.

Build this first; the rest follow from it. Model its structure on `reduce`/`invoke`
in `lower.rs` — the cases are the same `Node` variants, and a `Get`/`Apply` over a
context resolves the same way, only the output type differs (`Object` not `NodeId`).

## `wasi_ctx`: the root context

`wasi_ctx` builds the `Object::Ctx` whose slots correspond, **in order**, to the
members of the `Wasi` context (see `lib/wasi.moss`). The slot order must match the
order `explode`/`Get { ctx, slot }` use in the IR, or projections will read the
wrong slot. Map each member to its `Object`:

- `I32 → TyI32`, `I64 → TyI64`, `MemIdx → TyMemIdx`
- the `lit::Literal*` types → `TyLitInt(..)`/`TyLitChar`/`TyLitString`
- the `lit::*_realize_*` functions → `FnInt(..)`/`FnChar`/`FnString`
- `Numerals[Number=I32]`, `Numerals[Number=I64]` → nested `Ctx`es of digit/radix
  `ValU32` constants
- `wasm::Wasm[Base]` → a nested `Ctx` of `FnInstr` (and the memidx/align/offset
  `ValU32`s `wasm_instruction` reads)
- `wasip1::WasiP1[Base]` → a nested `Ctx` of `FnWasi` imports; `wasip1_sigdefs()`
  already gives the imported functions, and `push_func_wasi()` registers one.

`empty()` is the empty context (`Ctx` over no slots); note its `&self` signature —
either intern the empty context up front or change it to `&mut self`.

## Relationship to lowering

The backend consumes the IR built by `lower.rs`. Useful parallels to read first:

- `reduce` (WHNF: β-reduce `Apply`, project `Get`, unfold transparent defs) — the
  evaluator does the same traversal.
- `invoke` / `resolve_need` / `explode` — how a contextual need is satisfied from a
  context; codegen resolves the same way but against a concrete `Object::Ctx`.
- `Body`/`Instr`/`Expr` (in `lower.rs`) — the runtime instruction stream `interp`
  walks. `Instr::Set/If/Else/EndIf/Loop/EndLoop/Br` are control/assignment;
  `Instr::Expr { ty, expr }` is a typed value.

## Design choices to make

- **Contextual value representation.** Compile-time-known contextual values
  (digit constants, `memidx`, `align`, `offset`) are `ValU32` and read statically
  via `val_u32`. Genuinely runtime contextual values (if any arise) would need to
  be threaded as Wasm globals or extra function parameters — pick one (or support
  both to compare). The current examples lean entirely on the static path.
- **Literal folding.** A numeric literal lowers to a tree of `add`/`mul` over digit
  `ValU32`s; the evaluator should constant-fold that to a single `ValU32` (and a
  string literal to a single data-segment write) rather than emit arithmetic. This
  is what keeps `i32.const 42` from becoming a runtime loop.

## Testing

Golden-file testing via `crates/moss-dev`: it runs each `examples/*.moss` and
compares stdout against `tests/examples/stdout/`. Drive the four that lower
(`hello` first, then `escape`/`params`/`reassign`) until they run and match.
`fizzbuzz` is the natural next target once operators/methods lower.
