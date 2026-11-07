# Standard library

The standard library lives in [`lib`](/lib), written in Moss. Its
declarations are almost all *abstract* — `type Char;`, `val a: Char;`,
`fn putchar(c: Char);` — and [`wasi.moss`](/lib/wasi.moss)'s `StdBase`
provides their implementations in Moss, over the primitive [`Base`](../../lib/base.moss)
context (below). (They were first
provided natively, decision [D38](../design/semantics.md), by the Python
bootstrap: its interpreter from Python values in `bootstrap/mossc/interp.py`,
its Wasm backend from instructions and WASI shims in
`bootstrap/mossc/build.py`, with `bootstrap/mossc/native.py` the single
source of truth for what "natively" meant. That compiler and its native
`Std` are gone, [D86](../design/semantics.md).)
[`lib/char.moss`](/lib/char.moss) is *generated*, by
[`crates/moss-tests/src/gensrc.rs`](/crates/moss-tests/src/gensrc.rs)
(`cargo run -p moss-tests --bin gensrc`), and a test fails if the file
drifts from its generator.

Layout:

- [`bool.moss`](/lib/bool.moss): the one fully concrete corner — `Bool` is
  a nominal tag over two units, with `true`/`false` as defined vals (D45,
  D50). `if` eliminates it.
- [`char.moss`](/lib/char.moss) (generated): abstract `Char`, one abstract
  val per named character, and the `Chars` context including per-receiver
  comparison methods.
- [`num.moss`](/lib/num.moss): the detached arithmetic/comparison methods
  that contexts provide per receiver type (`Int.add`, `Char.eq`, ... —
  D47). When operators land (D33), they desugar to these symbols.
- [`access.moss`](/lib/access.moss): `.get`, `.set`, `.length`, `.push`
  and `.read`, declared **once** each with the element type as a requirement,
  supplied per provision with a bracket application
  (`String.get[Elem=Char]`, `IntList.get[Elem=Int]`). Declaring them per
  container made each spelling several symbols, and by D44 one scope
  cannot name two of them — which a compiler calling `.length` on a
  `String` and on an `IntList` needs it to (D61).
- [`float.moss`](/lib/float.moss): abstract `Float` (D67), its
  operations as the `FloatOps` context, and the conversions to and from
  `Int` — `float(n)`, `.trunc()`, `.round()` — and `FloatList`, a
  growable packed list of `Float` made by `float_list()` (D69), with all
  of it as `Floats`. `StdBase` provides it at f64; `WasiFloats32` is the
  same at f32, list included, so binding it in a block makes that block's
  `float_list()` a list of f32s.
- [`int.moss`](/lib/int.moss), [`string.moss`](/lib/string.moss),
  [`cell.moss`](/lib/cell.moss), [`list.moss`](/lib/list.moss),
  [`strlist.moss`](/lib/strlist.moss), [`path.moss`](/lib/path.moss):
  abstract types with the operations that are their own, importing the
  shared accessors above; `IntList` is the native backing store for the
  typed-arena idiom (D51).
- [`std.moss`](/lib/std.moss): the `Std` context bundling all of the
  above.
- [`prelude.moss`](/lib/prelude.moss): what every file gets into *scope*
  automatically — including all thirty-four detached names the library
  has, because a detached method must be in scope to be called (D61).
  Having the things still takes `assume`, as the
  [Context lesson](../learn/context.md) explains.

Lists over `Wasi` are one implementation (D69): `lib/wasistd.moss`'s
`packed_*` functions over a `Layout` — the element type, its width in
4-byte words, and how to store and load one — with a layout functor per
element type (`NumLayout`, `SingleLayout`, `DoubleLayout`) and a nominal
receiver per element type (`Ints`, `Singles`, `Doubles`) whose five
one-line methods apply the layout and call the shared code. A list of
another type costs a layout and a receiver; see D69 for why the receiver
cannot yet be shared.

The milestone that retires the native table has been reached:
[`wasi.moss`](/lib/wasi.moss)'s `StdBase` implements the whole of `Std` in
Moss over the primitive `Base` context. Its components include the
`Wasm`/`Wasi` declarations (`lib/wasm.moss`, `lib/wasip1.moss`), the
branching bridge, and GC functors. The self-hosted compiler uses the
raw-memory implementation of the same `Std` interface. The native table stayed as long as the
Python bootstrap did, whose back end and interpreter provided `Std` that
way, faster by a factor of fifteen (see [selfhosting.md](selfhosting.md));
it went with that compiler ([D86](../design/semantics.md)), so `Std` is now
provided only in Moss, over `Wasm`/`Wasi`.
