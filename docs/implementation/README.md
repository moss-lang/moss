# Moss implementation

This section describes the architecture of the Moss compiler.

The implementation is the bootstrap compiler under
[`bootstrap`](/bootstrap) (see its README for the pipeline). The working
design record for the current iteration is the
[semantics decision log](../design/semantics.md).

- [**Testing**](testing.md): philosophy and structure of the testing infrastructure.
- [**Standard library**](stdlib.md): how `Std` is provided, and its move
  into Moss over the primitive `Wasm`/`Wasi` context.
- [**Self-hosting**](selfhosting.md): the Moss compiler written in Moss
  under [`src`](/src) — what it compiles, how it is held to the
  bootstrap, and what is still missing.
