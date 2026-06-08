# Semantic-probe tests

Each `.moss` file here is a *minimal program that behaves differently under two
or more plausible interpretations of an unsettled language rule* — either one
interpretation compiles and another fails, or both compile but print different
output. They exist to pin down semantics that the prose docs and the current
compiler don't yet agree on, and to become regression tests once each question
is answered.

These are **not** wired into the `moss-dev` golden runner (which scans
`examples/` and `tests/errors/`), because the lowering phase can't compile them
yet. Each file's header comment states the question and the expected behavior
under each interpretation; the intended answer should be recorded here once
decided.

| Probe | Question | Answer |
| --- | --- | --- |
| `literal_target_type.moss` | Is an unsuffixed literal's concrete type fixed by the literal, or chosen by the demanding context? | **Fixed by suffix** → compile error. (But how literals *lower* at all is still an open design problem.) |
| `overlapping_providers.moss` | Are two context bindings that satisfy the same need an error, or does one win? | **Error**, within a frame. Context is a *stack* of frames; `bind` pushes a frame to override lower ones. |
| `alias_transparency.moss` | Is `type X = T` transparently equal to `T`, or opaque? | **Transparent.** |

(A probe for `static` needs / "context smuggling" was removed: `static` is being
dropped for the prototype, and "smuggling" is an out-of-scope research problem.)
