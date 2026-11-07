# Hello, world!

Create `hello.moss` with these contents:

```moss
#!/usr/bin/env moss
assume Base {
  fn main() {
    bind StdBase;
    println("Hello, world!");
  }
}
```

Run it with `moss hello.moss`. It prints:

```text
Hello, world!
```

The [prelude](../../lib/prelude.moss) brings `Base`, `StdBase`, and
`println` into scope automatically. Scope lets you name a symbol;
context lets you use what it requires.

[`Base`](../../lib/base.moss) groups all the primitives the compiler
provides: WebAssembly types and instructions, WASI calls, branching, and
GC allocation functors. `assume Base` declares the context that `main`
needs. The compiler supplies those primitives when it runs the program.

[`StdBase`](../../lib/wasi.moss) implements the standard library in Moss
over those primitives. `bind StdBase;` makes its `Std` context available
inside `main`, including strings, numbers, collections, and output.
Reusable functions can declare `assume Std` and use that implementation
when called from `main`.

[`println`](../../lib/string.moss) prints a string followed by a newline.
Quoted strings also use the implementation supplied by `StdBase`.

`fn main()` declares the program's entry point. It takes no parameters
and implicitly returns the unit type `()`. Other functions run when
called from it, directly or indirectly.
