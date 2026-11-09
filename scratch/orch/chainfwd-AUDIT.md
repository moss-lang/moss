# Audit changes by "chainfwd"

## 1. A platform family is abstract, with distinct items (`Platform.Family`)

What: `Platform.Family` (Audit.v, module Platform) gains two last
conjuncts:
- `forall d ps a r b, w_decl W source = Some d ->
  functor_parts (d_ast d) = Some (ps, a, r, b) -> b = None`: the family
  is an abstract functor;
- `NoDup (map provision_id keys)`: its result's keys are distinct.

PublicPlatformApply's `FamilyData` mirrors them.

Why: these were the premises `PlatformAbstract` and
`PlatformKeysDistinct` of `PublicChainFwd.chain_forward_keys`. The proof
of the empty-chain case of `ChainForwardKeys` uses
`PublicFamilyAgree.items_agree_gen` at an application (AppPremises, with
Cf = C). That proof needs two things:
- every symbol the family's result mentions is one of its inputs.
  Implicit inputs exist only for an abstract functor (FunCtx).
- the caller's method entries are determined by their keys.
  AppPremises, unlike FamilyApplication, gives no `NoDup` of the
  caller's keys. With distinct family keys and `MethodsDistinct`, the
  caller's keys are distinct too.

Rejects nothing accepted: the conditions constrain only the platform
world, never user programs, as MethodsDistinct and MachinesFixed do.
- The oracle builds a platform family only in `platform_entry`'s
  `KyFun` case. That case requires `functor_parts g = (_, _, None)`, no
  body, and `plat (gs_mod g)`.
- lib/wasm.moss's Heap, Mutable, Arrays and Memories are declared with
  `;`, without a body.
- Their items are distinct declarations. Memories lists 27 `Addr.*`
  methods with no duplicate, checked with `sort | uniq -d`.
- Native provides only these.

Test: none executable, for the same reason as chainagree-AUDIT.md
section 1: no implementation provides another platform family.

Alternatives considered: keep them as named premises (the state at
e99e086). The coordinator chose to fold them into the platform world, as
MethodsDistinct was.
