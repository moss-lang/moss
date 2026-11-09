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

## 2. A kept output is unmentioned by the context (BApplyAbstract). REJECTS CORRECT PROGRAMS

**This change rejects two programs that both implementations accept and
run correctly:**
- `bootstrap/examples/lambda-declared-output.moss`: the oracle and native
  run it, exit 0;
- `bootstrap/examples/kept-output-family-method.moss`: both print 0, the
  right answer.

Audit is now stricter than the oracle and native, an incompleteness.
The coordinator decided to make this change.

What: BApplyAbstract's (and AppPremises') first output case gains a
conjunct. An output t not in force (`nlook t (c_ty C) = None`,
installed as itself) must also satisfy
`~ In t (context_gens C ++ key_receiver_gens C)`. That is,
`S.capture_gens C`: the generations the context mentions, plus method
keys' receivers. This is exactly the statement of the former premise
`PublicLinkResults.KeptOutputsUnmentioned`.

Why: that premise was false for the two programs above, both accepted
by Audit. A false premise of `safety_obligations` makes the theorem
vacuous. The result's write `T |-> m` re-grounds whatever mentions T. In
these programs, T is an unbound declared symbol that a family demand put
in method keys and signatures. The re-grounding is harmless by
parametricity, but that is unproved.

Proof: `PublicLinkResults.kept_outputs_unmentioned` proves the premise
from the new conjunct. `safety_obligations` no longer lists it.
`dest_scope` and BodySlice's case destructuring were adjusted.

Evidence that it rejects nothing else:
- An oracle variant adds the check to Elab.v's abstract application
  (`gens` in the `bind F;` case). An output not in force is rejected if
  any generation the env mentions is it. `env_mentions` covers table
  values, merges, val/fn/method provisions with receivers, locals, and
  instantiations. The variant is built in
  scratch/chainfwd/variant (a copy of bootstrap/ with the patch).
- It was compared with the oracle on tests/, examples/ and
  bootstrap/examples/, 378 files (`scratch/orch/chainfwd-cmp.sh`,
  output scratch/orch/chainfwd-cmp.out). 376 verdicts are identical.
  The only differences are the two programs above, which the variant
  rejects.
- The oracle itself (Elab.v) is not changed. Audit now rejects what
  the oracle accepts in these two cases.

**Alternative (not taken): the parametricity proof** of
results2-AUDIT.md section 2, estimated at days. A declared symbol T
that is unbound reaches a context only through family demands. It
occurs only in family-installed method provisions, whose closures are
parametric in the family's input. So re-grounding T changes only the
types at which parametric code runs. This would keep Audit complete
for these programs. If it is ever proved, revert this conjunct and
restore the premise's discharge from it.
