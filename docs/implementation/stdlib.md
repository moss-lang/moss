# Standard library

The standard library lives in [`lib`](/lib), written in Moss. Its
declarations are almost all *abstract* — `type Char;`, `val a: Char;`,
`fn putchar(c: Char);` — and the compiler provides their implementations
natively (decision [D38](../design/semantics.md)): the interpreter from
Python values in `bootstrap/mossc/interp.py`, the Wasm backend from
instructions and WASI shims in `bootstrap/mossc/build.py`. The single
source of truth for what "natively" means is `bootstrap/mossc/native.py`;
[`lib/char.moss`](/lib/char.moss) is *generated* from it, and a test fails
if the two drift.

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
- [`access.moss`](/lib/access.moss): `.get`, `.length`, `.push` and
  `.read`, declared **once** each with the element type as a requirement,
  supplied per provision with a bracket application
  (`String.get[Elem=Char]`, `IntList.get[Elem=Int]`). Declaring them per
  container made each spelling several symbols, and by D44 one scope
  cannot name two of them — which a compiler calling `.length` on a
  `String` and on an `IntList` needs it to (D61).
- [`int.moss`](/lib/int.moss), [`string.moss`](/lib/string.moss),
  [`cell.moss`](/lib/cell.moss), [`list.moss`](/lib/list.moss),
  [`strlist.moss`](/lib/strlist.moss), [`path.moss`](/lib/path.moss):
  abstract types with the operations that are their own, importing the
  shared accessors above; `IntList` is the native backing store for the
  typed-arena idiom (D51).
- [`std.moss`](/lib/std.moss): the `Std` context bundling all of the
  above.
- [`prelude.moss`](/lib/prelude.moss): what every file gets into *scope*
  automatically — including all twenty-nine detached names the library
  has, because a detached method must be in scope to be called (D61).
  Having the things still takes `assume`, as the
  [Context lesson](../learn/context.md) explains.

The milestone that retires the native table has been reached:
[`wasi.moss`](/lib/wasi.moss)'s `WasiStd` implements the whole of `Std` in
Moss over the `Wasm`/`Wasi` declarations (`lib/wasm.moss`,
`lib/wasip1.moss`), so only those two contexts are primitive, and the
self-hosted compiler runs on it. The native table stays because the
bootstrap's own back end and interpreter still provide `Std` that way, and
because that is the faster of the two by a factor of fifteen — see
[selfhosting.md](selfhosting.md).
