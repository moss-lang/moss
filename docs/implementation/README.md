# Moss implementation

This section describes the architecture of the Moss compiler.

- [**Lowering**](lowering.md): how the contextual lowering phase works, the design decisions behind it, what currently lowers, and what's left.
- [**Backend**](backend.md): how Wasm codegen works (and the handoff for implementing it).
- [**Standard library**](stdlib.md): implementation choices for the standard library rather than the compiler itself.
- [**Testing**](testing.md): philosophy and structure of the testing infrastructure.
- [**Open questions**](questions.md): issues that must be resolved to make the implementation correct and/or complete.
