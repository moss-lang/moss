# Fixtures

Small Moss files the tests load off disk rather than build in
memory: the shared-`T` `IsCell` idiom of [D51](/docs/design/semantics.md)
(`cell.moss`, `inner.moss`) for the generics tests, and a mini module
graph (`prelude.moss` importing the other three) that the Python
bootstrap's loader tests used for their diamond and export cases; those
tests were about the Python loader and went with it (D86).

They live here rather than under `src/` because they are not part of the
self-hosted compiler; they were once sketches there, and the tests that
grew around them are worth keeping.
