# Audit changes by "platform" (PlatformApplyOk)

Both changes are to `Platform.Family` (Audit.v, module Platform), which
says what a platform family's closure code is. Each is its own commit.

## 1. Run-time keys for a family's items (b39e1d5)

What: the family code's result list (`fc_outputs`) is
`map (rkey_of outputs) keys` and each binding is
`BPrimitive (rkey_of outputs key) op`, instead of the bare static keys.

Why: `ResultKeys` gives static keys (`mkey r m = IMth r m []`). A caller
reads a method provision under `rkey C r m`, whose instantiation is
`theta_of C a` for the item's recorded `AppOf` instantiation `a`. For
`Ref.deref` (in `assume Boxed { ... fn Ref.deref(): Boxed; }`), `Boxed`
is a requirement its signature mentions, so `a = [(Boxed, Some _)]` and
the key is `IMth Ref[..] deref [(Boxed, _)]`. The application filed the
primitive under `IMth Ref[..] deref []`, so the caller's context after
`bind Heap;` was not realized (and `r.deref()` would be stuck, `e_entry`).
PlatformApplyOk requires `Pre` at the result context whenever the step
completes, and it does complete, so the obligation was false.

Test: `bootstrap/examples/platform-family-keys.moss` (`bind Boxed = Int;
bind Heap; 42.boxed().deref()`). Oracle (`moss_m2.exe`, built at this
worktree): runs, exit 0. Native (`target/release/moss run`, with
MOSS_SEED from moss-safety's `target/moss.wasm`): runs, exit 0.

Rejects nothing accepted: the oracle's `platform_fun` (Elab.v) files each
item under `key_item_at efr k`, its instantiated key in the result
environment, which is what `rkey_of outputs` models; linked bodies
already use `rkey_of Cr` for their results (LinkBody).

Alternatives: keep static keys and make the caller's rkey drop the
instantiation for platform items (would change every method call), or
assume the correspondence away (the obligation is false, so no).

## 2. No type output (668565c)

What: `Platform.Family` requires `forall t, ~ FunOutputs W source t`.

Why: the family's code returns only its method items; with a type output
`t`, `apply_functor` calls `read_result_types d3 Dr [(t, gensof t)]` with
`Dr` holding no `ITy t`, which is `None`, so the application is
`Stuck e_compose`. Nothing else in `Linked` excluded such a functor (any
functor main assumes may be provided as a platform family if its keys are
platform methods), so PlatformApplyOk was false for it.

Test: none executable: no implementation accepts such a platform family.
The oracle's `platform_fun` rejects any non-method result item ("the
platform cannot provide a non-method item"), and a type output needs a
type result item (`SpecTypes`), so the change rejects nothing it accepts.

Alternative: `FamilyShape W source` (D84 family shape); it is stronger
than needed and not what the oracle checks (it allows contexts).
