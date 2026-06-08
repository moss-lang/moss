# Open questions

## Literals

Resolved: literals desugar into contextual expressions rather than being
primitive values of contextual types. See [the reference](/docs/reference/literals.md).
What remains is implementation, plus two deferred follow-ups noted there:
compile-time function evaluation (to fold literals of user-defined number types)
and size-indexed integer types (to turn literal overflow into a type error).

## Context smuggling

Out of scope for this prototype; this is a broader research concern not tied to
the current codebase.
