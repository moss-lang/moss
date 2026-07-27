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
- [`int.moss`](/lib/int.moss), [`string.moss`](/lib/string.moss),
  [`cell.moss`](/lib/cell.moss), [`list.moss`](/lib/list.moss),
  [`path.moss`](/lib/path.moss): abstract types with their operations;
  `IntList` is the native backing store for the typed-arena idiom (D51).
- [`std.moss`](/lib/std.moss): the `Std` context bundling all of the
  above.
- [`prelude.moss`](/lib/prelude.moss): what every file gets into *scope*
  automatically (having the things still takes `assume`, as the
  [Context lesson](../learn/context.md) explains).

The milestone that retires the native table is implementing `Std` in Moss
on top of the `Wasm`/`Wasi` declarations (`src/wasm.moss`,
`src/wasip1.moss`), at which point only those two contexts are primitive.
