# Self-hosting: what is left

The third goal of this branch is a Moss compiler written in Moss, under
[`src`](/src). Its front end is done; its middle and back ends are not.
This page is the plan for the rest, written while the front end was
fresh so the next session starts from a shape rather than a blank page.

## Where `src/` stands

Done, and running on both backends:

- [`lex.moss`](/src/lex.moss) — a loop-based lexer with a keyword trie.
- [`tree.moss`](/src/tree.moss) — the declaration AST as parallel arenas.
- [`intern.moss`](/src/intern.moss) — names as codepoint runs, so an id
  outlives the file it came from.
- [`parse.moss`](/src/parse.moss) — recursive descent over the
  declaration grammar, recording names and payload references.
- [`mods.moss`](/src/mods.moss), [`collect.moss`](/src/collect.moss) —
  the module graph: load a file and its imports transitively, then
  report duplicate declarations and out-of-scope references per module.
  Pointed at `lib/prelude.moss` and `src/main.moss` it explains every
  name in the compiler's own sources.

Missing: **lowering** and **codegen**. `src/lower.moss` is
previous-iteration code that does not parse under the MVP grammar (see
the errata in the decision log's §12), and there is no emitter at all.

## The idiom

There are no generic containers ([D51](../design/semantics.md)), so every data structure is a
*typed arena*: parallel `IntList`s indexed by an id, with a `context`
bundling them and accessor functions keyed on the id. `tree.moss` is
the worked example — copy its shape rather than inventing another.

`StrList` exists for the few places a real String must be kept (module
paths). Everything else should be an interned id or an arena index.

## Lowering

The Python original is [`bootstrap/mossc/lower.py`](/bootstrap/mossc/lower.py).
Its parts, roughly in dependency order:

1. **Types** — an arena of type nodes: unit, never, tuple, record,
   union, nominal, abstract. `bootstrap/mossc/types.py` is small and
   translates directly; the union-find lives in the env, not here.
2. **Env** — the requirement environment: the type map as a union-find
   ([D43](../design/semantics.md)), the val and method tables, and a parent link. Arenas again,
   with a stack of scopes rather than the Python object graph.
3. **Context flattening** — walking a `context` declaration's items into
   an env, merging consistently. Note the ordering constraint the
   bootstrap also has: an item whose signature mentions a type must come
   after that type.
4. **Needs** — each function's requirement list, and the call-site
   `needs_map` translating callee keys to caller keys.
5. **Method resolution** — attached lookup by (receiver, name), detached
   by name with the receiver's home module as fallback ([D36](../design/semantics.md), [D54](../design/semantics.md)).
6. **The core IR** — another arena, mirroring
   [`bootstrap/mossc/ir.py`](/bootstrap/mossc/ir.py), including the
   layout width each expression occupies ([D59](../design/semantics.md)).

A checkpoint that is testable at each stage, the way the front end was
built: a driver that prints what the stage computed, compared against
the bootstrap's own output for the same input.

## Codegen

[`bootstrap/mossc/build.py`](/bootstrap/mossc/build.py) is the original,
but a self-hosted emitter is smaller than its line count suggests: much
of that file is shims implementing the native `Std`, and `Std` is Moss
now ([`lib/wasistd.moss`](/lib/wasistd.moss)). What is genuinely needed
is LEB128 encoding, the section layout, and the instruction selection —
all of it appending bytes to an `IntList`.

**This is where [D48](../design/semantics.md) stops being deferrable.** A Wasm module names its
imports in its own bytes: `wasi_snapshot_preview1`, `fd_write`,
`_start`, `memory`. Those are strings the compiler must hold itself.
With the char constants now available in Moss they *can* be built
character by character, exactly as the keyword trie recognises keywords
without literals — possible, and unpleasant enough that it is worth
settling the decision before writing the emitter rather than after.

## Sharp edges already known

- The self-hosted parser records an attached method with the empty name,
  because it keys a fn by its last name while an attached method is
  keyed by receiver *and* name. Real resolution needs the pair.
- `collect.moss` resolves an import path by dropping a leading `./` and
  concatenating; `..` is not handled.
- A record field or tag payload wider than one scalar is rejected by the
  backend ([D59](../design/semantics.md)), so records do not nest yet.
- `src/std.moss`, `src/option.moss`, `src/range.moss`, `src/cell.moss`
  and `src/inner.moss` are sketches from earlier iterations, kept in the
  parse corpus but not used; `src/lower.moss` does not parse at all.
