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

## 3. A platform family's machine types are fixed inputs (MachinesFixed)

What: each item of a platform family satisfies
`Platform.MachinesFixed W source inputs outputs key`: every wasm machine
type symbol (I32, I64, F32, F64, StructRef, ArrayRef) its AtKey signature
mentions (receiver, parameters, result) is a fixed input of the source
functor (`Fixed`: mentioned by its result, not a bracket) and in its
context's table.

Why: a primitive is typed only at the machine types; the safety proof
reads the family's signature types through θ, and only a fixed input is
guaranteed to mean, at the application, what it meant where the closure
was provided (the platform's provision exists only at the machine types,
CPlatform's PlatformFixed). A family polymorphic in, say, I32 could be
applied where I32 means Bool (the oracle accepts `bind I32 = Bool;`), and
its `Array.get` would then be typed at Bool. Previously this was the
premise PlatformSigsFixed in safety_obligations.

Rejects nothing accepted: the oracle provides platform families only for
lib/wasm.moss's functors (Elab.v platform_fun), Heap[Boxed], Mutable[Boxed],
Arrays[Arrayed] and Memories[Memory], whose brackets are Boxed, Arrayed and
Memory, none a machine type, and whose result signatures mention the
machine types through their inputs (Wasm, I32 etc.).

Test: none executable (no implementation provides such a family).

## 4. A platform family's items read their raw handles as themselves (HandlesRead)

What: `Platform.HandlesRead` gains a method case (each type of a method
item's signature, its receiver among them, reads a handle tag's raw
requirement as itself, RawHandleRead), and `Platform.Family` asks
`HandlesRead W outputs key` of each item, as worldform's change asks it of
main's platform fns (`Platform.Entry`). The definitions moved above
`Family` (unchanged otherwise).

Why: the primitives read a handle's payload as the raw machine handle
(PublicPrimitiveSafety's HandleFormed); this was the premise
PlatformHandlesFormed.

Rejects nothing accepted: the oracle provides platform families only for
lib/wasm.moss's functors, whose result items are unbracketed methods
(platform_fun: `Spec (Some p) (Some dn) None`), read in the functor's own
context where each raw handle (StructRef, ArrayRef, I32) means itself; it
checks each with check_runtime_primitive.

Test: none executable (no implementation provides another family).
