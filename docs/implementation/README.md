# Moss implementation

This section describes the architecture of the Moss compiler.

The implementation is the compiler written in Moss under [`src`](/src),
which compiles itself, bootstrapped by the Rocq front end and interpreter
under [`bootstrap`](/bootstrap) (see its README for what it is and what of
it is proved). The working design record for the current iteration is the
[semantics decision log](../design/semantics.md).

- [**Testing**](testing.md): philosophy and structure of the testing infrastructure.
- [**Standard library**](stdlib.md): how `Std` is provided, and its move
  into Moss over the primitive `Wasm`/`Wasi` context.
- [**Self-hosting**](selfhosting.md): the Moss compiler written in Moss
  under [`src`](/src), which compiles itself to a fixpoint — how it was
  held to the Python bootstrap it replaced, what the last mile actually
  was, and what is still missing.
