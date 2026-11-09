# Audit changes by "chainagree" (ChainResultAgrees)

## 1. A platform family's items have distinct methods (MethodsDistinct)

What: `Platform.Family` (Audit.v, module Platform) gains a last conjunct
`MethodsDistinct W keys`: two items of the family whose methods are equal,
or both detached methods (`DetachedMethod`: declared `fn .m(...)`), are
the same item. So the items' methods are pairwise distinct and at most one
is detached.

Why: `ChainResultAgrees` (PublicPlatformApply) asks, for an application of
F whose closure is a copy chain F <- H1 <- ... <- H0 down to a platform
family H0, that every item of H0 is one of the caller's new methods
(ItemsAgree's clause B). `family_core` uses that clause to rule out a
primitive installed at a run-time key the caller's context does not list,
which could shadow one of its old methods. A copy step (`BFunCopy`, carried
in `CopyChain.CCStep`) gives only:
- `SameResult W u h`: the same leaves (receiver symbol, method name);
- `CopyOutputs`: each key of u's result (read at u's inputs) is a key of
  h's, with the same provision up to the fixed inputs. This is one
  direction only, as the oracle's `copy_outputs_match` (Elab.v
  `copy_output_check`) checks.
A leaf does not determine a key. u's item on a leaf resolves its method
by name in u's own module (`MethodNamed`: an attached method of the
receiver, else the detached scope there), and two leaves can resolve to
one method (a detached `.m` reached under two names, or two receivers
whose readings coincide), so u may have fewer distinct keys than h0 while
each is one of h0's. Then some item of h0 is not one of u's. With
MethodsDistinct the counting is direct: u's item on h0's leaf resolves to
h0's method there (the same attached method, the receiver's own; or a
detached method, which must be one of h0's since u's key is one of
h0's, so it is h0's only detached one). With distinct methods, a key of
h0 is fixed by its method, so every key of h0 is matched by u's.

Narrowest: the lead's suggestion was distinct (receiver symbol, method)
pairs and no brackets. Brackets do not matter to the argument (it goes by
method symbols); distinct leaves do not suffice (two receivers can share a
detached method). Distinct methods with at most one detached is what the
argument uses.

Rejects nothing accepted: the oracle builds a platform family only for an
abstract functor declared in the platform library (Elab.v
`platform_entry`'s `KyFun` case, guarded by `plat (gs_mod g)`), i.e.
lib/wasm.moss's Heap (`Boxed.boxed`, `Ref.deref`), Mutable (`Ref.set`),
Arrays (`Arrayed.arrayed`, `Array.length/get/set/fill/copy`) and Memories
(27 attached `Addr.*` methods, each its own declaration). Each family's
items are distinct declarations with one detached method at most
(`.boxed`, `.arrayed`). Native likewise provides only these.

Test: none executable. No implementation provides another platform family,
and these four satisfy the condition. The condition constrains only the
platform world, never user programs (like MachinesFixed, platform-AUDIT.md
section 3).

Alternatives considered: (1) two-directional CopyOutputs, a strengthening of
a user-facing check, which the oracle does not make; (3) tolerating extra
installed keys in `family_core`, which needs a coherence invariant. The lead
chose this option (famplat-STATUS.md, handoff).
