# Fixtures

Small Moss files the bootstrap's tests load off disk rather than build in
memory: a mini module graph (`prelude.moss` importing the other three)
for the loader's diamond and export tests, and the shared-`T` `IsCell`
idiom of [D51](/docs/design/semantics.md) for the generics tests.

They live here rather than under `src/` because they are not part of the
self-hosted compiler; they were once sketches there, and the tests that
grew around them are worth keeping.
