# Where the reference interpreter can go wrong

Working notes for semantic safety ([G05](proofs/safety.md#g05)), started at
`086a617`. They list every way the public reference interpreter
(`Audit.Semantics.Interpreter`) can produce a `Wrong` outcome. For each one
they name the source premise that has to rule it out. This is the
specification that the runtime invariant ([T02](proofs/safety.md#t02)) must
meet. The table is a reading of `Audit.v`, not a checked result.

## The reduction (checked)

[PublicSafety.v](theories/PublicSafety.v) proves `safety_from_entry`.
`Public.CORRECT.safety` follows from `entry_never_stuck`, which says that for
an admissible program and a compatible interface, `run_main`'s signal is never
`Stuck` at any fuel. No other premise is needed:

- `observe` maps exactly `Stuck` and `Breaking` to `Wrong`.
- `ret_call` turns a `Breaking` that escapes a call into `Stuck e_break`, so
  `run_main` never returns `Breaking` (`run_main_not_breaking`).
- Conversely, a `Stuck` entry signal is observed as `Wrong`
  (`stuck_entry_is_wrong`). The reduction is therefore exact, and T20 is
  precisely what remains.

## The entry (checked, conditional)

[PublicEntry.v](theories/PublicEntry.v) proves `safety_obligations`:
`CORRECT.safety` for any `Public` instance from exactly the remaining named
obligations (the functor and family obligations
`CapturedInForceOk`, and `ChainResultsOk` from
[PublicCopyApply.v](theories/PublicCopyApply.v) (itself proved,
`PublicChainCore.chain_results_of_new`, from `ChainNewOk`, the
formal/caller result correspondence for the result's new provisions
(selection, outputs, aliases, kinds and the provisions kept from the
caller are proved: `chain_core_ok`; ChainNewOk itself from the static
`ChainKeysOk` and `ChainRealizeOk`: `PublicChainNew.chain_new_ok`), and `KeptOutputsUnmentioned`, an
open premise for the user to decide: results2-AUDIT section 2), which with
`ChainPlatformOk` prove the slice's `CopyApplyOk`, `LinkResultsOk` and
`PlatformApplyOk`; for `ChainPlatformOk`, proved in
[PublicPlatformApply.v](theories/PublicPlatformApply.v), its platform
premise `ChainForwardKeys` (the caller's new methods filed under the root
family's run-time keys; the converse and the rest of `ChainResultAgrees`
proved, `chain_result_agrees`, `chain_forward_agrees`); for `FamilyStepOk`, proved in
[PublicFamilyStep.v](theories/PublicFamilyStep.v), `FamilyLinkResultsOk`
(its selection proved, `PublicChainCore.family_link_results_ok`, from
`FamilyLinkNewOk`: the family's new methods installed by the selection),
`FamilyCopyApplyOk`, and `FamilyKeysTyped` (for
`FamilyPlatformApplyOk`, proved in PublicPlatformApply.v); and the slice's
`FamilyCoherentOk`, `ExScopedOk`, `LitScopedOk`, `DemandScopedOk`), with the slice's elaboration and link facts read off
`ElaboratesIn` and `Linked`. The formation premises and main's `HandleFormed` are
proved (PublicWorldFormation, PublicFamilySig, `handles_formed`). It goes through `safety_public`, which is
`CORRECT.safety` from three premises:

- the slice's safety at every fuel, with the reference host's state
  invariant (`PublicPrimitiveSafety.state_ok`);
- context formation's premises (`MergeFormed`, `MentionsTyped`), so main
  starts in a formed context and frame;
- handle formation of the platform signatures in main's context
  (`HandleFormed`, a T29 fact).

The initial ghost heap is the empty one (`state_ok_initial`). Each platform
primitive is a typed call (`entry_primitive`, from `primitive_safe`): its
signature's machine types are read through main's initial type table,
which is canonical and scoped, extended by `initial_machine_subst`
(`machine_lookup`). `Compatible` supplies support for every primitive in
the initial environment. The steps on the way are `entry_safe`:
`entry_never_stuck` follows from three premises, stated for every
admissible program, compatible interface and initial state:

- the slice's safety at every fuel (`Safe`, which `safe_all` proves from
  the slice's remaining hypotheses);
- an initial ghost heap that the initial state realizes (`state_ok`);
- callable platform primitives at their `AtKey` signatures (T11/T17).

The rest is proved. `main_not_stuck` runs main's body from its frame, and
a `Breaking` cannot escape, because main's frame is not in a loop.
`entry_realizes` shows that the initial environment realizes main's
context. Merge absorption comes from `InitialTypes` (`entry_absorbs`). The
`I32` constants are typed by `initial_machine_subst`, since val types in a
function context are canonical. Main's context has no method needs,
because `Platform.Entry` has no method entries. Functor needs are platform
closures.

The slice now takes its interpreter instance as a parameter (`INTERP`, in
[PublicValueTyping.v](theories/PublicValueTyping.v)), and the safety
reduction, the primitives and the entry take the `Public` instance
(`PUBLIC`), since `CORRECT`'s obligations are stated inside one instance. The entry
instantiates it with `Public(Host).Reference`, the interpreter the safety
statement is about. Separate instances of `Interpreter` have distinct
`evaluation` types and cannot be related.

## Stuck sites

"Static" names the elaboration rule that produces the runtime construct and
the premise that excludes the failure. *d* is the runtime environment and *g*
the local store. *C* and *F* are the static context and frame.

| Site | Code | Fails when | Constructed by | Static premise that must exclude it |
| --- | --- | --- | --- | --- |
| `EVar x` | `e_var` (1) | `x` not in *g* | `ExLocal`, `ExThis` (`EVar 0`), lambda prologue `ELet` | `Named F n` and `n ∈ c_locals C` (`Numbering` gives `this = 0`). Invariant: every static local's `var_of F n` is bound in *g*, at its first occurrence. |
| `EVal v` | `e_val` (2) | `IVal v` absent or not a `ValueProvision` | `ExVal`; lambda capture prologue `EVal capval` | `nlook v (c_vals C)`. Invariant: each static val provision is realized by the **first** runtime entry at `IVal v`. |
| `ECall f` | `e_nobody` (4) | no body for `f` | `ExCallD`, `MCAttachedDefined`, `MPWrapped` body | `FnElab` for each defined fn gives `body_of P f`, and `program_of`'s `view_function` is `nlook` over `program_functions` (T30). |
| `ECall f … sm` | `e_compose` (3) | `select_provisions d sm` fails | same | `Satisfies C' Cf ns sm` gives `provides C' (at_site k)` and `sm`'s targets are `rkey_of C' (at_site C' k)`. Needs exact runtime-key agreement (T04). |
| `EInvoke f` | `e_entry` (6), `e_nobody` (4) | `IFn f` absent, not a closure, or its code missing | `ExCallA`, `FPForward` body `EInvoke g` | `nlook f (c_fns C)`. The closure carries code provenance (T30/T31). |
| `EMethod … prov` | `e_entry` (6), `e_nobody` (4) | `prov` absent, or not a function or primitive closure | `PrIn` (`rkey C ot m`), `PrFamily` (`rkey C' ot m`) | `mlook ot m (c_mths C)`. The runtime key includes θ = `theta_of C a` from `c_apps`, so the realization must file method provisions under exactly `rkey` (T04). |
| `EAssign x` | `e_assign` (9) | `x` not in *g* | `BlkAssign` | `slook n (c_locals C) = Some (_, LVar)` and `Named F n`; same local invariant as `EVar`. |
| `EIf`, `EWhile` | `e_bool` (10) | condition not `VTag bool (VTag true/false VUnit)` | `Cond` | `LangSym "bool" "Bool" b`, the condition fits `TNom b []`, and `program_bool P = b`. Needs value typing for Bool and `Layouts` (see the gap below). |
| `EField` | `e_field` (11) | field absent | `ExField` | The receiver is a tag with a record payload or a bare record, and the field is in `ftys`; uses `field_lookup_typed`. |
| `EMatch` | `e_pat` (14) | no arm, or `PBad` | `ArmsEmptyNever`, `ArmsSome` | Exhaustive: a catch-all, or every member's head covered. Abstract scrutinees need a catch-all, since `PatOf` heads are nominal. `PBad` is excluded by the payload record typing in `PRecord`/`FieldPats`; `never` has no values. |
| `ret_call` | `e_break` (5) | `Breaking` escapes a body | every call and `run_main` | `ExBreak` needs `fr_loop F`. `FnElab`, `LambdaProvider`, `LinkBody` and `ExConst` frames all start with `fr_loop = false`; `BApplyBody` keeps the caller's frame, but its code is inlined at that point. |
| `BFunction`, `BMethod`, `BFunctor` | `e_compose` (3) | capture `sm` selection fails | `FPDefined`/`MPAttached` (`Satisfies`), `LP` (keys provided by `Cl`), `BFunClosure` (`smc` keys provided) | Key agreement, as for calls (T04/T07). |
| `BCopyFunctor u u'` | `e_fun` (13) | `IFun u'` not a functor closure | `BFunCopy` | `nlook h (c_funs C)`. Invariant: functor provisions are realized by closures (T31). |
| `apply_functor` | `e_fun` (13), `e_nobody` (4) | no closure for `IFun u`, or no code | `BApplyAbstract`, `PrFamily`/`Demand` (`apply_expression`) | `nlook u (c_funs C)`, plus closure provenance (`LinkBody`'s `funbody_of`, or `PEFun`/`Platform.Family`). |
| `apply_functor` | `e_compose` (3) | input `sm` selection fails | same | `Satisfies C Cu ns sm`. |
| `apply_functor` | `e_chain` (8) | `adapt_provisions` along a copy chain fails | `BFunCopy` smaps over `incl nsh nsu` | Copy-chain provenance: each `sms` step selects keys provided by the previous step (T31, "link/copy-chain provenance"). |
| `apply_functor` | `e_compose` (3) | result items absent from the body environment | `LinkBody` `ResultRealizes`, `ProvideResult` | The body's binds provide every result item at its source key (`fc_sources`, T15/T16). |
| `apply_functor` | `e_compose` (3) | `result_environment` fails: an output is unreadable | `LinkBody` (outputs bound in the body) | The body's final environment has a meaning for each output (T16). An occupied destination is no longer Stuck: the writes shadow (results2, below). |
| primitive call | `e_missing_primitive` (15) | `supports interface p = false` | `BPrimitive`, platform entries, `Platform.Family` bodies | `Compatible` requires every operation in `required_primitives`. Invariant: every runtime `PrimitiveClosure` came from program code or the initial environment. |
| primitive result | interface laws | ill-shaped arguments, or a missing `gc_pre` | `AtKey`/`PrimitiveSignature`, `MethodPrimitive` | Arguments typed at the signature yield `value_shape` (T11); GC arguments satisfy `gc_pre` under the ghost heap (T17). |
| `run_main` | `e_nobody` (4) | no body for `main` | `ElaboratesIn` | `main` is a defined fn, so `FnElab` gives its body. |

`ETag` and `check_tag` only `Trap`; `ELet`, `ESeq`, `ELoop`, `EReturn`,
`ERecord` and `eval_arguments` only propagate.

## Observations so far

- **Lambda capture keeps method-key receivers' meanings (family2,
  coordinator-approved option (c)).** `lexical_capture C` now captures
  `context_gens C ++ key_receiver_gens C`. A callee requirement the caller
  leaves unbound is read at the call site as itself, so a family demand can
  key a method at a declared T that only that key mentions
  ([family-unbound-need.moss](examples/family-unbound-need.moss)). A lambda
  over that context used to drop T's meaning while keeping the key, so the
  captured provision was realized at another grounding whenever T had a
  meaning. In a body that is never the case; in a defined val's inlined
  initializer it can be
  ([const-family-unbound.moss](examples/const-family-unbound.moss)).
  Run-time only: nothing is rejected. The oracle captures the whole type
  environment (`ExecutionModel.xclose`) and keys methods symbolically, so
  this mirrors it. With it, `lambda_realizes_le` needs no key-scoping
  premise, and option (b)'s relaxed `KeysScoped` needs no run-time "no
  meaning" fact. Details: scratch/orch/family2-AUDIT.md.

- **Removed: b4f8279's runtime freshness check on functor results
  (results2, coordinator-approved).** `result_environment` was Stuck when
  an output destination already had a meaning in the caller
  (`fresh_result_types`, added by the user's commit b4f8279, "preserve
  type generations"). Audit accepts programs that reach it, and the
  oracle (which shadows: `dr ++ d`) runs them:
  [const-kept-output.moss](examples/const-kept-output.moss) (a defined
  val's initializer is elaborated in the empty context, so its `bind F;`
  keeps output T, and it is inlined where T already has a meaning; the
  oracle runs it, and native rejects the use of the val for an unrelated
  reason), and
  [lambda-declared-output.moss](examples/lambda-declared-output.moss) (a
  lambda's capture gives a meaning to a declared T its demand context
  mentions; oracle and native run it). The writes now shadow. The safety
  argument moves from the runtime check into the proof: `dest_scope`
  (PublicLinkResults) keeps every destination out of the caller's
  context and frame types, so no typed local, provision or return value
  is re-grounded (`ground_grow`). For outputs installed as themselves this
  still relies on the named premise `DeclaredScoped` (a declared symbol
  the context mentions is a table key). `family-unbound-need.moss` shows
  that premise is false, so the kept-output case needs its own fix: an
  Audit kept-output clause `~ In t (context_gens C)`, which would reject
  lambda-declared-output.moss, or a proof that re-grounding an unbound
  declared symbol is harmless. With the check gone, `MeaningsScoped` and
  the formation hypothesis planned for it are not needed.
  `PublicGenerationFacts` keeps generation preservation as a theorem
  under the freshness the static rules give. Its examples, and
  `PublicGenerationCallExample`'s, now check shadowing.
  scratch/orch/results2-AUDIT.md has the details.
- **A merge could carry a shadowed, unformed pair (fixed).**
  `MostGeneralMerge` constrained σ only through `app_subst`. A second pair
  for a generation is never read, but it stays in the context's `c_sub`,
  and formation (`cf_sub`) quantifies over every pair. So
  `PublicFormation.MergeFormed` was false by definition. Example:
  `s = [(g, TAbs g); (g, TUnion [TAbs g])]` merges `[NTy g]`.
  **Decided:** σ is a function (`In (g, t) σ -> lookup_subst g σ = Some t`),
  as the elaborator's union-find σ is. MergeFormed is now proved
  (PublicWorldFormation.merge_formed_of): most generality alone forces σ's
  values to be formed, by probing σ with a marking unifier. MentionsTyped is
  proved from WorldOf (mentions_typed_of). Details are in
  scratch/orch/worldform-AUDIT.md.
- **Counterexample: a platform fn could read its raw handle as another type (fixed).**
  [handle-rebind.moss](examples/handle-rebind.moss):
  `context Store = ..., i64_store[I32 = Addr[I32 = I64]]` gives main a
  platform i64_store at an Addr whose raw handle reads as I64. That is still
  a Platform signature for PI64Store (prim_msig). The oracle rejects the
  program (D20/D62 assumed-requirement coherence). Native accepts it, since
  main never calls i64_store.
  **Decided:** `Platform.Entry` asks each platform fn's signature to read
  its raw handles as themselves (`HandlesRead`), so `HandleFormed` is
  proved (PublicEntry.handles_formed).
- **Counterexample: FamilySigOk was false as stated.**
  [family-bracket-leaf.moss](examples/family-bracket-leaf.moss) is accepted
  by both implementations. A family's context leaf `Ctx[X = Foo]` carries a
  P(F) symbol into a signature that the key does not determine. A
  FamilyApplication whose matching is not read off the key then gives a
  non-ok signature. Every use already has FamilyReadOff, so the hypothesis
  is now `FamilySigReadOffOk`, proved in
  PublicFamilySig.family_sig_ok_readoff. Audit is unchanged.

- **Counterexample: a record pattern may bind one name twice.**
  `FieldPats` does not require its bound names to be distinct. In
  `Pair { a = y, b = y } => …`, the static context adds `y : Char` and then
  `y : Bool`, so `y` reads as `Bool`. At runtime, `bind_fields` puts
  `(y, a)` first, so `lookup_local` returns the `Char`. (The pun `b` in
  `Pair { a = b, b }` has the same problem.)
  [duplicate-field-binder.moss](examples/duplicate-field-binder.moss) is
  accepted by the oracle built at `386e418`, which then reports
  `stuck (XErr 10)`: `if y` reads a `Char` where the context promised a
  `Bool`. The public relation admitted the same derivation.
  **Decided:** `PRecord` now requires `NoDup` of its bound names
  (user-approved; [D34] in docs/design/semantics.md). The executable
  elaborators still accept the program; that is
  [E21](proofs/build-run.md#e21).

- **Counterexample: a provision's type could go stale before a call (fixed).**
  [stale-provision-type.moss](examples/stale-provision-type.moss): `val v: T`
  mentions `T`, but `T` is not one of `v`'s requirements (it has no assume
  nesting), so `v`'s provisions record no meaning for it. The caller binds
  `T = Char`, provides `v = char::x`, rebinds `T = Bool`, and calls
  `f(): T { v }`. `Satisfies` checks only that `v` is provided and that
  recorded requirement meanings agree, which is vacuous here. `f` reads `v`
  at the call's `T`, so it returns the `Char` as a `Bool`. The oracle
  accepts the program and reports `stuck (XErr 10)`. Functor applications
  select inputs through `Satisfies` too, so they are likely affected the
  same way.
  **Decided** (user-approved): `Satisfies` takes the instantiation `θ`
  (`CallTheta` at calls and providers, the input reading at functor
  applications). Each selected provision must have, canonically, the
  callee's type or signature read at `θ` (`sig_at`). Coherence reads
  through `θ` as well. `MostGeneralMerge` now states, as its comment
  always said, that σ maps only generations of the telescope to types over
  them. The implementation check is [E25](proofs/build-run.md#e25).
- **Counterexample: a bound attached method entered at the wrong receiver (fixed).**
  [bracket-method-receiver.moss](examples/bracket-method-receiver.moss):
  under `T = Char`, `bind Box.m[T=Bool] = Box.get;` files the attached
  `Box.get` under the key `Box[T=Bool].m`. `MPAttached` read the method's
  instantiation θ and its needs in the surrounding context, so `Box.get`'s
  need `T.k` was answered by `Char.k`, and only the receiver's head was
  checked. `b.m()` on `Box[T=Bool](true)` then calls `Char.k` on a `Bool`.
  The oracle accepts the program and reports `stuck (XErr 14)`. The native
  compiler prints `1`, reading `true` as a character code.
  **Decided** (user-approved): as at a direct call (`MCAttachedDefined`),
  the provider's `Satisfies`, `CallTheta` and signature are read in the
  context with the key receiver's arguments set as types. The
  implementation check is [E26](proofs/build-run.md#e26).
- **Captured vals are new symbols (precise freshness, no counterexample).**
  `LambdaProvider` required captured val symbols only to be undeclared
  (`Fresh W`), so a derivation could reuse a val an enclosing lambda
  captured. That is unobservable, since only a lambda's own prefix reads
  its captured vals, but it leaves the context listing a val whose runtime
  value has another type, which the realization invariant rules out. The
  rule now also requires each captured val to be unprovided in the current
  context (user-approved). Both implementations already use new symbols:
  the oracle draws them from one global counter, and the native compiler
  uses slots.
- **Lambda captures keep merge keys (no counterexample).** A lambda's
  closure keeps the meanings of `context_gens C`, the generations in the
  context's stored types. Those types are already canonical, so a key the
  context merged away (`T`, merged into `U`) was not captured. The closure
  then gave `U` its meaning but left `T` unbound. That is unobservable,
  but the realization invariant's merge absorption
  (`ground (canon t) = ground t` for every `t`) failed at `TAbs T`.
  `context_gens` now also lists each merge key and the generations of its
  value (user-approved). Capture and freshness share it, so fresh
  generations also avoid merge keys. Absorption for the closure then
  follows from the enclosing context's (`capture_absorbs`). A proof-side
  weakening of absorption was considered and rejected: at calls, θ is
  unconstrained for requirements the caller leaves unbound, so a scoped
  absorption would need new constraints on θ as well.
- **A wrapper calls its fn at the fn's own context (no counterexample).**
  `MPWrapped`'s wrapper called `g` at `theta_at W Cg g`, which reads only
  `w_reqs g`. Every other call reads θ over the callee's whole context
  type table. For an ordinary fn the two hold the same types in another
  order, but a fn in scope as `main` also has its root context's types,
  which `w_reqs` omits. [wrapped-main.moss](examples/wrapped-main.moss)
  wraps such a `main` that uses the root `Gc` functors; the oracle
  and the native compiler both run it correctly, so the gap was only in
  the relation. The wrapper now calls `g` at `theta_ctx Cg`, θ indexed by
  `g`'s context type table (user-approved). The proof shows the call's
  environment has exactly the outer call's meanings and provisions
  (`self_meanings`, `realizes_transfer`). The implementation follow-up is
  [E27](proofs/build-run.md#e27).
- **Counterexample: a functor body's `break` (fixed).**
  [functor-body-break.moss](examples/functor-body-break.moss): `bind G;`,
  for `G` with a body, runs `G`'s binds in place, in the caller's frame, so
  `G`'s `bind v = break;` leaves the loop around `bind G;`. `Breaks` was
  syntactic and counted `bind G;` as never breaking, so
  `fn f(): Int { loop { bind G; } }` typed the loop `never`. The oracle
  accepts it and reports `stuck (XErr 14)`. The native compiler counts the
  break and rejects it, and both run the well-typed variant correctly.
  **Decided** (user-approved): `Breaks`, `BlockBreaks` and `StmtBreaks`
  take the world and module, and a new `ItemBreaks` counts `bind F;` as
  breaking when an item of `F`'s body does (in `F`'s module). The oracle's
  follow-up is [E28](proofs/build-run.md#e28).
- **Gap: a primitive declared to return `|` could return (fixed).** The
  primitives proof (`PublicPrimitiveSafety.v`) found that `interface_laws`
  gave `unreachable` (`fn unreachable(): |;`) the result shape `ShapeUnit`.
  A compatible interface could then return `()` from it, and a program
  using `unreachable()` as, say, an I32 operand reaches a primitive with an
  ill-shaped argument, on which the laws allow `Stuck`. `PrimitiveSignature`
  special-cased `unreachable` and checked no result type.
  **Decided** (user-approved): a new shape `ShapeNever`, which no value has,
  corresponds to `|` (`ValueType`), and `prim_sig PUnreachable` is
  `([], ShapeNever)`. The general result law then forbids any primitive
  declared to return `|` from returning normally, and `unreachable` is
  checked like every other primitive. The implementation side is
  [E29](proofs/build-run.md#e29): the default host's `unreachable` must
  not return, and the link check must accept its `|` result.
- **A lambda's frame keeps the generations in scope (precise freshness).**
  A lambda's closure keeps the meanings of `context_gens C`, which can
  include generations an earlier `bind F;` made. `FreshFor` in the lambda's
  body only avoided the body frame's `fr_fresh` and the current context's
  generations, so after a bind dropped the last provision mentioning such a
  generation, an application there could make it anew, and at runtime
  `result_environment` finds it occupied (Stuck). Both implementations draw
  generations from a global counter. `LambdaProvider` now requires the body
  frame's `fr_fresh` to include `fr_fresh F` and `context_gens C`
  (user-approved; see Later cleanup).
- **Gap: a functor closure's captured needs were unchecked (fixed).**
  [captured-merge.moss](examples/captured-merge.moss): `bind F = G;`
  captures G's needs that F does not take from the binding site, and
  `BFunClosure` only required each to be provided there. G's context merges
  `X` with `Y`, here `X = Char` and `Y = Bool`, so G's body would call
  Bool's `not` on a Char. Both implementations reject the program (the
  oracle checks captured readings, D84 3b), but the relation accepted it.
  **Decided** (user-approved): the captured needs must satisfy G's context
  as a call's needs do (`Satisfies`, at G's inputs read here). Its merge
  clause covers every merge of G's context whose types are in force here.
  Also `LinkBody` now requires a functor body's frame to number its locals
  (`Numbering Fg None []`), as `FnElab` and `LambdaProvider` do; without it a
  value bind with nested `let`s could put two live locals in one slot. The
  implementation follow-up is [E30](proofs/build-run.md#e30).
- **Gap: 3b covered only captured values (fixed).**
  [captured-linked-fn.moss](examples/captured-linked-fn.moss): G captures
  `fn g(x: T): T` while F links T, so g, bound at T = Int, would be called at
  an application's T = Bool. `LinkBody`'s 3b only constrained captured
  values. Both implementations reject the program (the oracle's message is
  D84 3b for any captured item). **Decided** (user-approved): 3b now
  covers every captured item's type or signature, recorded instantiation
  and method receiver. Covered by [E30](proofs/build-run.md#e30).
- **Counterexample: a call could bind a callee's merged class inconsistently
  (fixed).**
  [unbound-merge-representative.moss](examples/unbound-merge-representative.moss):
  `f`'s context merges `X`, `Y` and `Z` into one type. The caller binds
  `X = Char` and `Y = Bool` and leaves `Z` unbound. The merge equations each
  mention the representative, and `Satisfies` skips an equation unless all of
  its generations are bound at the caller, so no equation is checked. `f`
  returns its `X` argument as a `Y`; the oracle built at `386e418` accepts
  the program and reports `stuck (XErr 10)` when `if` reads the `Char` as a
  `Bool`. Binding `Z = Char` as well makes the oracle reject the call. The
  implementation is affected, not only the audit relation.
- **A call can bind only part of a callee's merged class (resolved by `CallTheta`).**
  `theta_at` gives a callee requirement the caller's meaning of the same
  symbol, or the symbol itself when the caller has none. `Satisfies`
  checks the callee's merge equations only when every generation in the
  equation is in force at the caller. In
  [partially-bound-merge.moss](examples/partially-bound-merge.moss),
  `f`'s context merges `X` with `Y`, and the caller binds `X = Char` but not
  `Y`. If `f`'s recorded context represents the class by `Y`, then `f`'s
  parameter `x: X` reads as `Y`, which the call instantiates to the
  meaningless generation `Y` rather than `Char`. The oracle accepts the
  program and prints `k`; the behavior is fine here, since `f` only passes
  `x` through. But the invariant can no longer relate `f`'s view of the
  value to the caller's. A meaningless generation is also a single global
  name shared by every activation that leaves it unbound, which is the
  shape of the earlier saved-type bug.
- **A merge could be oriented either way (fixed).**
  `MostGeneralMerge` leaves a merge between two generations unoriented. In
  [merge_order.moss](../tests/wasi/merge_order.moss), `B` and `C` are
  merged, and both `σ = {B ↦ C}` and `σ = {C ↦ B}` are idempotent most
  general unifiers. `CtxOf` (and `FunCtx`) is chosen independently by each
  rule that mentions it: `FnElab` for the callee's body, and `ExCallD`,
  `FPDefined`, `MPAttached` and so on at each call or provider site.
  Method keys and needs are read under σ: a need is
  `mkey (app_subst σ r) m`, and the body's calls use `rkey C (canon C ot) m`.
  So a program whose call site installs `IMth C w θ` while the callee's
  body looks up `IMth B w θ` satisfies `ElaboratesIn`, and gets stuck with
  `e_entry` when the body calls `b.w()`. The oracle picks one orientation
  consistently, so it does not exhibit this; the gap is in the
  declarative relation that `safety` quantifies over. The same independence
  affects `FunCtx` between `LinkBody` and functor application sites, and
  `main`'s `CtxOf` between `ElaboratesIn` and `Platform.Linked`. Candidate
  repairs:
  **Decided** (user-approved): the world records each symbol's context and
  needs (`w_context`), and `CtxOf` and `FunCtx` require them. Merging stays
  symmetric, and any consistent orientation is allowed.
  `function_context_unique` and `functor_context_unique` are checked.
  Recording the implementation's contexts is
  [E23](proofs/build-run.md#e23).
- **Unit tags had no value typing (fixed).** `lib/bool.moss` declares `tag False;`
  and `tag True;`, and `Layouts` requires them to be `DKUnit`.
  `PublicValueTyping.value_typed` types tags only through
  `PayloadOf`, which covers `ADTag` declarations only. `VTag true VUnit` is
  therefore untypable at `TNom true []`, and no Bool value was typable.
  `TypedUnitTag` now covers unit declarations.
  [PublicBoolTyping.v](theories/PublicBoolTyping.v) proves `truthy_typed`:
  under `Layouts` and the Bool symbols that `ElaboratesIn` records, `truthy`
  reads every value typed at Bool. This closes the `e_bool` row, given a
  typed condition value.
- **Counterexample: a `break` in a `while` condition (fixed).** At runtime
  `EWhile` propagates a `Breaking` from its condition to the enclosing loop.
  But `StmtBreaks` had no `ASWhile` case, so `loop { while break {} }`
  satisfied `~ BlockBreaks` and `BlkLoopLast` typed it `never`.
  [while-condition-break.moss](examples/while-condition-break.moss) returns
  `()` from a `Bool` function, and the oracle reports `stuck (XErr 10)`.
  `SBWhile` now counts the condition. The implementations still accept the
  program; that is [E22](proofs/build-run.md#e22).
- **`else` branches are checked against the expected type.** `JoinExp`
  adopts the expected type without checking the `else` branch. A block or
  nested `if` checks its own type, and the parser produces nothing else
  there, but the relation admitted any expression. `ExIfElse` now requires
  `opt_fits et exp`, which rules out no parsed program.
- **Attached methods need a nominal receiver (Q5).** `FnElab` let an
  attached method's receiver be any symbol. A method attached to an
  abstract type would then see `this` as the nominal `TNom T []` while a
  caller passes a value of the abstract type `T`. The implementation rejects
  such declarations ([attached_on_abstract.moss](../tests/errors/attached_on_abstract.moss):
  "requires a nominal receiver (Q5)"). `FnElab` now requires the receiver to
  be a tag or unit declaration. This rules out no program the implementation
  accepts, and `WorldOf` is unchanged.
- **The receiver slot is reserved.** `ExThis` reads local 0, and `Numbering`
  allowed a name `"this"` to be numbered 0. A local named `this` would then
  shadow the receiver at a different type. The lexer makes `this` a keyword,
  so no parsed program does this, but the relation relied on that silently.
  `Numbering` now numbers no name 0 when there is a receiver. This rules out
  no parsed program and keeps the safety argument independent of the lexer.
- **Proof shape.** A defined function's closure carries its `FnElab`
  derivation and a realization of its context (T30/T31). The main lemma can
  then be proved by induction on fuel, inverting the derivation at each
  step, with no step-indexed logical relation. `BindItems … wrap` is stated
  in continuation-passing form: `wrap` maps continuations that are safe under
  `C'`/`F'` to expressions that are safe under `C`/`F`.
- **`ExConst`** inlines code elaborated in an empty context with its own
  numbering. It is closed: it reads no locals, vals or provisions, and has
  no `return` or `break`.
- **A family's matching is read off the demanded key.** `FamilyApplication`
  constrained the matching θ only to P(F) symbols. A symbol that the demanded
  key's receiver does not mention could be matched to any type, even an
  unformed one or one naming generations outside the context, and the
  application then installed provisions keyed by it. D84 rule 7 says θ is
  read off the key, and the oracle does exactly that
  (`family_candidate_step` in
  [FamilySelectTrace.v](theories/FamilySelectTrace.v)): it reads the
  receiver with each P(F) symbol as a marker and matches it against the key.
  The new `FamilyReadOff` (a premise of `PrFamily`, `FamilyStep` and
  `LitOpFamily`) states the same: θ's domain is exactly the P(F) symbols
  that the formal receiver mentions, with no duplicates, and
  `canon C (app_subst θ rf)` is the key's receiver. Each value is then a
  subterm of an ok, scoped receiver (`family_matching_scoped`), so
  `demand_formed_cx` no longer assumes this. It rules out no program the
  implementations accept.
- **Counterexample: a copy's extra input replaced a captured type
  (fixed).** `application_environment` read every input of a functor body
  from the application's θ first. After `bind F0 = G; bind F1 = F0;`
  where G captures `X` and `v: X` but only F1 takes `X`, applying F1 at
  `X = F64` gave G's body `X = F64` while `v` is an i32.
  [copy-capture.moss](examples/copy-capture.moss) then boxes `v` at F64,
  rebuilds it at `Ref[Boxed=F64]` and hands `f64_add` an i32, which is
  stuck under Audit. Both implementations keep the captured `X = I32`
  (the oracle's `runtime_provider_inputs` passes θ only for the linked
  inputs), so the rebuild traps with a cast failure. `functor_code` now
  records `fc_linked` (G's inputs that the functor it was bound to takes;
  all inputs for a platform family), and only those are read from θ.
  After discussion: a captured value has a fixed representation, and a
  copy should behave as its source does at the source's inputs.
- **Counterexamples: a linked body or a copy read F's needs differently
  (fixed).** `LinkBody` checked G's body only in G's terms. In
  [linked-merge.moss](examples/linked-merge.moss) G's context merges X,
  which F links, with Y, which G captures, so G reads the linked `vx: X`
  as a Y. Bound at X = Y = Bool and applied at X = Char, the body calls the
  captured Bool `not` on 'a'. This depends on which representative the
  merge picks: with X, 3b rejects the program. Copies had the same gap:
  `BFunCopy` compared needs only as keys, and in
  [copy-merge.moss](examples/copy-merge.moss) H merges X with Y while F
  does not. The oracle rejects both programs (`defined_link_inputs_compatible`,
  D84; `copy_output_compatible`, D78), and native rejects them with type
  errors. Now `LinkBody` requires each need F links to have the same
  provision in G's context as in F's, and G's readings of F's inputs to
  hold in F's context. `BFunCopy` requires the same of H's needs and
  inputs. An oracle variant with these simpler checks gives the same
  verdict as the oracle on all 353 test programs.
- **A family member is not bound directly (rule 7, now in Audit).**
  Rule 7 forbids binding a method key that a family in force produces
  (`family_direct`). Both implementations enforce it: the oracle in `Elab.v`
  ("is a member of the family … it cannot be bound directly"), native in
  the test `family_direct` (`crates/moss-tests/tests/suite/backend.rs`).
  Audit's `BMethBind` did not, so a family application could shadow a
  directly bound provision under the same key. `BMethBind` now requires
  `~ FamilyProduces` for every family, after discussion. It rejects nothing
  the implementations accept, and family applications no longer need an
  argument that the shadowed provision has the same signature.
- **Counterexamples: two closure-merge obligations are false (repaired in Audit, next entry).**
  `LinkMergesOk` and `BindMergesOk` (PublicBodySlice) claim that each
  merge of a linked body's context either stays among F's (non-fixed)
  inputs or involves only fixed inputs and symbols in force at the bind.
  [linked-merge-captured.moss](examples/linked-merge-captured.moss): G
  merges Y, which it captures, into X, which F links (merge Y := X), so
  LinkBody holds and 3b does not apply. [bind-merge-unbound.moss](examples/bind-merge-unbound.moss):
  G merges Y into Z, F takes neither, and Y is not in force where G is
  bound, so Satisfies' merge clause does not apply to it. Audit, the oracle
  and native accept both (they print 7). In each, an application's
  environment reads the merged-away Y from the bind site, not as its
  representative, so it does not realize G's context (absorption fails),
  although nothing in G's body reads Y. A premise rejecting these would
  reject programs both implementations accept; the proposed repair is in
  the runtime: `application_environment` should read each of G's inputs
  through G's table (its representative under the merge), after which
  `link_entry`'s merge clause holds by construction and both obligations
  can go. `CapturedInForceOk` as stated is false
  (`PublicLinkMerges.captured_in_force_unprovable`: nothing ties the bind
  context to a program); `captured_in_force_of` proves it at a bind
  context with `KeysScoped` (in `Pre`) and `DeclaredInForce` (every
  declared symbol the context mentions is in force), an invariant `Pre`
  does not carry yet (T29).
- **An application reads G's inputs through G's table (Audit change,
  repairs the two closure-merge counterexamples).** `functor_code` has a
  new field `fc_table`, and `application_environment` reads each input x
  as `app_subst base t` when the table maps x to t, where `base` is the
  old per-input meaning (θ for a linked input, the capture otherwise); an
  input the table omits reads as `base x`, as before. `LinkBody` records
  G's canonical table `c_ty Cg`; a platform family's code records `[]`, so
  its behaviour is unchanged by definition. A merged-away input now reads
  as its representative, so G's merges hold in the body's environment by
  construction (`link_entry`, via `GD : ground de t = app_subst base
  (canon Cg t)` and idempotence). This is unobservable by elaborated code:
  G's provisions and every type its body elaborates are canonical in G's
  table (`functx_canon_prov`), so they mention only representatives, whose
  meanings did not change. A premise would have been the wrong fix: the
  programs are safe, both implementations accept them, and the failure was
  only that the model's run-time environment ignored a merge the static
  context had made. `LinkMergesOk`, `BindMergesOk`, their hypotheses, and
  `ClosureOk`'s merge clause are gone. See scratch/orch/repair-AUDIT.md.
- **Counterexamples: a platform family filed its items under bare keys,
  and could have a type output (fixed).** `Platform.Family` installed each
  primitive, and returned the result, under the result's static key
  (`mkey`, no instantiation). A caller reads a method provision under its
  run-time key (`rkey`), which carries the instantiation the item was made
  at: `Ref.deref` mentions `Boxed`, so after `bind Heap;` at `Boxed = Int`
  the call `r.deref()` looks for `IMth Ref[..] deref [Boxed=Int]` and finds
  nothing ([platform-family-keys.moss](examples/platform-family-keys.moss);
  the oracle and native run it and exit 0, filing the items under
  `key_item_at`, `Elab.v` `platform_fun`). `PlatformApplyOk` was false.
  Now the family's code and result use `rkey_of outputs`, as a linked
  body's do. Separately, nothing forbade a type output, which the code
  cannot return (`read_result_types` is stuck); `Platform.Family` now
  requires none. The oracle provides method items only ("the platform
  cannot provide a non-method item"), so neither change rejects anything
  it accepts. Recorded in `scratch/orch/platform-AUDIT.md`.
- **Counterexample: a copy whose result reads differently (fixed).**
  `BFunCopy` compared results only leaf by leaf (`SameResult`). In
  [copy-output-order.moss](examples/copy-output-order.moss) F lists
  `out[P=U], out[P=V]` and H `out[P=V], out[P=U]`: the same leaves, but
  the last setter types `out` as V after `bind F;` and as U for H. Applying
  the copy runs G's body, which provides a U, so the caller's match on V is
  stuck. The oracle rejects the copy ("effective output interpretation
  differs (D78)", `copy_outputs_match`), and so does native (test
  `copied_functors_compare_final_output_interpretations`). `BFunCopy` now
  requires `CopyOutputs`: read in F's context with the same fresh output
  roles, H's result gives each of F's keys F's provision, F's fixed inputs
  read as they are in force. Details and the remaining differences from
  the oracle's check: scratch/orch/copy-AUDIT.md.
- **Counterexample: a copy's source with an extra result key (fixed).**
  `CopyOutputs` checked one direction only, as the oracle's
  `copy_outputs_match` does: each key of F's result is one of H's. Leaves
  drop a method item's brackets, so H may have two items on one leaf
  (`Box.m[T=Bool], Box.m[T=Char]`) where F has one. An application of F
  runs the root body and selects every key of the root's result
  (`fc_outputs`), so the extra key overwrites the caller's own provision
  of it. [copy-extra-key.moss](examples/copy-extra-key.moss) binds
  `Box.m[T=Char]` to 3 and applies F: native keeps exactly F's result
  (docs/reference/semantics.md, Functors) and prints 31; the oracle
  installs G's `Box.m[T=Char]` and prints 21. The caller's static context
  no longer describes its environment, and `ChainNewOk`'s first clause
  (every key the selection fills is a new provision's) was false.
  `CopyOutputs` now also requires every key of H's result to be one of
  F's. The oracle should check the same converse (task E32); over the
  corpus, an oracle with that converse gives the same verdicts. Native
  accepts the program (correctly), so Audit is stricter than native here.
  Details: scratch/orch/results4-AUDIT.md.
- **Counterexample: a result key spelled through an output's input
  meaning (open; fix decided).** F's merges can make an input X read as an input T
  that is also an output of F (`A.gimme[Foo=X], A.gimme[Foo=T]`, T the
  merge representative). F's result key `X.m`, read at F's inputs, is then
  `T.m` with the input T. `LinkBody`'s code renames the output roles back
  to the output symbols (`output_renaming`), so the code's key reads, at
  the application, as the new T, while the caller keys `X.m` at X's
  meaning (the old T). [output-merge-key.moss](examples/output-merge-key.moss):
  the reference interpreter on the oracle's program stops `stuck (XErr 6)`
  (e_entry) at `x.m()`; native prints 7; with the representative swapped
  the oracle prints 7. `ChainKeysOk` was false. A `LinkBody` premise forbidding such keys was
  tried and REVERTED: it rejects the corpus regression
  tests/regressions/functor_input_output_generations.moss (native `yn`;
  the oracle is stuck there, its ignored test, E06). Decided fix (pending):
  keep the output roles in the code's keys and map them at the
  application (results4-AUDIT.md, change 3). Until then Audit's
  application is wrong for this shape and `ChainKeysOk` is false for it.
  Oracle bug: E33. Details: scratch/orch/results4-AUDIT.md.
- **A copy's source takes no input its target lacks (now in Audit).**
  `BFunCopy` compared needs (`incl nsh nsu`), which do not list implicit
  inputs (types only H's result mentions). An application of F reads its
  root body's linked inputs from θ, which covers only F's inputs.
  `BFunCopy` now requires H's type table to be among F's, as the oracle's
  input interpretation check does (it fails on an input of H that F
  lacks). Details: scratch/orch/copy-AUDIT.md.
- **Refile shadowed a stale provision (now in Audit: no refile).**
  `LinkBody` used to file each result method again under F's key right
  after its bind (`refile`). F's key spells an output T as `TAbs T`; when
  T is also an input of G, the body can bind `T.out` before rebinding T,
  and that stale provision has the same runtime key.
  [examples/refile-stale-shadow.moss](examples/refile-stale-shadow.moss)
  binds `T.out`, then `Int.out`, then `T = Int`, and calls `seed.out()`
  in between: the reference interpreter on the oracle's program stops
  `Stuck` (`Int.out` on a Bool), native prints 7, so `RefileRunOk` was
  false. Now, as native behaves, a linked body runs only its own binds
  (`binds0`), and `functor_code` names each result item's source key
  (`fc_sources`, default the item itself): the application selects
  `result_smap θ' Dr sources`, reading F's key for a result method from
  the body's runtime key `rkey_of Cb (at_site Cb (output_renaming outs gen k))`.
  `RefileRunOk` and `refile_run` are gone; `app_step` runs the body with
  `chain_run`. The oracle still refiles at bind time (task E31).
  Details in scratch/orch/refile-AUDIT.md.

- **Counterexample: an output type left its block (fixed).** `BApplyAbstract`
  returned only the fresh generations (`newgens`) as made. An output not
  in force is installed as itself, and the application writes its meaning,
  but it was not made, so `BlkE` let a value of that type leave the block.
  [output-escape.moss](examples/output-escape.moss) does this. The oracle
  rejects it (D78), and native too ("a type created by a binding cannot
  escape its block"). `LinkResultsOk`'s `MeaningsGrow newgens` was false for such
  applications. The rule now makes every output destination
  (`map gensof outs`), and requires that the frame's return and receiver
  types not mention an output installed as itself (they are read where
  their symbols are in force, so this rejects nothing). Details are in
  scratch/orch/family-AUDIT.md.
- **`FamilyStepOk` was too general (statement restated).** It quantified
  over every `FamilyApplication` and every realized C0. (1) Without
  `FamilyReadOff`, the matching is arbitrary on P(F) symbols that the needs
  do not mention, for example `T = TAbs g` for a generation g outside C0's
  scope. The family's key is then not `KeysScoped`, so `Formed` fails
  after an application that completes. (2) `Pre` does not say that C0's
  method provisions are readings of the declared signature at their θ, so
  a family closure can shadow one of them at the same run-time key with
  another signature. `FamilyStepOk` now takes `FamilyReadOff` with an ok,
  in-scope receiver and C's table read in C0's scope, and its keep clause
  requires `FamilyCoherent`. It is proved. What the callers need moved to
  the slice's `family_coherent` and `*_scoped` hypotheses, each stated
  over formed contexts. Like `FamilyStepOk` before, they hold for
  elaborated contexts but not for every formed one. Proving them needs
  invariants in `Formed` (scoped local and provision types, signature
  coherence) preserved through the slice.
- **Gap: a linked body could not rebind an output its result mentions (fixed).**
  [result-output-meaning.moss](examples/result-output-meaning.moss): G binds
  `T = Bool` and provides F's `fn mk(): T`. G's body files `mk` at
  `() -> Bool`, but `ResultRealizes` compared it with F's promise renamed to
  `() -> T`, so `LinkBody` failed and Audit rejected the standard functor
  pattern, which both implementations accept. `ResultRealizes` now compares
  at the body's meanings of the outputs, as the oracle's
  `promised_result_type` does (scratch/orch/results-AUDIT.md).
- **A platform family's items have distinct methods (`Platform.MethodsDistinct`).**
  `ChainResultAgrees` needs every item of a closure's root platform
  family to be one of the caller's new methods (`ItemsAgree`'s clause B;
  `family_core` uses it to rule out a primitive shadowing an old method).
  A copy (`BFunCopy`) checks only one direction (`CopyOutputs`, as the
  oracle's `copy_outputs_match`): each of F's items is one of H's. With
  `SameResult` (same leaves), F has an item on each of H's leaves, but a
  leaf (receiver symbol, name) does not fix the key: F's module may
  resolve a detached `.m` to another method, and brackets add keys. So
  `Platform.Family` now asks that the family's items have pairwise
  distinct methods, at most one of them detached. Then the method F
  resolves on each leaf is H's (an attached method is global; the one
  detached method is the only detached candidate), and every root item is
  matched. It constrains only the platform world: the oracle provides
  platform families only for lib/wasm.moss's functors (Elab.v
  `platform_fun`, `plat`), and Heap, Mutable, Arrays and Memories each
  have distinct methods, one detached at most (`.boxed`, `.arrayed`).
  Details in scratch/orch/chainagree-AUDIT.md.

## Body-safety slice (checked, conditional)

[PublicBodySlice.v](theories/PublicBodySlice.v) proves `safe_all`. For
every fuel, the code of every `Ex`, `Blk`, `BlkE`, `Branch` and `Cond`
derivation, run from a state satisfying `Pre`, produces an `Outcome`: never
`Stuck`, a `Breaking` only where the source has a `break` leaving a loop,
and typed values, stores and returns otherwise. `Pre` combines frame
numbering, positional local typing, a valid state for a ghost heap, and
merge absorption (`ground d (canon C t) = ground d t`).

The proof is a strong induction on fuel. Each step is a mutual structural
fixpoint over the derivation, which handles blocks and inlined constants
that reuse the same code at the same fuel. The `while` and `loop` cases
are inner fuel inductions. The following remain explicit Section
hypotheses, each a named remaining obligation:

| Hypothesis | Covers | Task |
| --- | --- | --- |
| `family_step` | one family application, as a binding, its matching read off (`FamilyReadOff`) a receiver that is ok and in the code's scope: it completes with the family's provisions added, the code's table formed and realized, and every context among the code's provisions that agrees with the family at a shared run-time key (`FamilyCoherent`) still realized; or it stops. `demand_case`, `family_method_case` and `literal_family_case` are proved from it. Proved (`family_step_ok`, [PublicFamilyStep.v](theories/PublicFamilyStep.v)) from the abstract application machinery (`chain_run`) and family versions of the application obligations (`FamilyLinkResultsOk`, `FamilyCopyApplyOk`, `FamilyPlatformApplyOk`) | T15, T16 |
| `family_coherent` | the code's formed context agrees with a family applied around it at a shared run-time key (same receiver, method and θ, same signature). It is true of elaborated contexts (both are `SigOf` readings at θ), but `Pre` does not carry that as an invariant | T29 |
| `ex_scoped`, `lit_scoped`, `demand_scoped` | a demanded family key's receiver is in the code's scope: elaborated expression and literal types, and a callee need read at the call site, mention only the context's generations. `Formed` scopes neither locals' nor provisions' types (only ok), and `theta_at` reads a callee symbol the caller leaves unbound as itself | T29 |
| `link_results` | a completed linked body provides F's result at the application: result provisions selected under the caller's keys from their sources in the body (`fc_sources`), fresh output generations, the caller's context after it realized; its meanings grow only at the application's made list (every output destination, D78) | T15, T16 |
| `copy_apply` | applying a copy runs its source at the source's interface | T15 |
| `platform_apply` | applying a platform family installs its primitives, also through a copy chain. Proved as `ChainPlatformOk` ([PublicPlatformApply.v](theories/PublicPlatformApply.v), `chain_platform_ok`; `PlatformApplyOk` is its empty chain) from `ChainResultAgrees` (the caller's and the root family's ProvideResult list the same method items at θ, under the run-time keys the family files them at, signatures agreeing at the state's ground meanings, and F has no type output) and `ChainPrimsOk`. `ChainResultAgrees` is proved from its forward clause `ChainForwardKeys` (each new method is filed under a root item's run-time key, signatures agreeing): no outputs and methods only from the chain's `SameResult` (`PublicChainAgree.copy_news_methods`), and every root item a new method by counting methods (`PublicChainAgree.chain_cover`, from `Platform.MethodsDistinct` and `WorldOf`'s scopes). `ChainForwardKeys` needs the readings composed along the chain (open), which `PublicEntry.platform_prims_public` discharges for the reference host (`platform_handles_formed`, `platform_sigs_fixed`, from Platform.Family's `HandlesRead` and `MachinesFixed`). A platform closure records that its fixed machine inputs mean the machine types (`CPlatform`'s `PlatformFixed`), established at the entry from `Platform.Entry`'s `FixedMachinesRead`; a linked root never shares a family's primitives (`platform_not_linked`). The family application's version `FamilyPlatformApplyOk` is proved too (`family_platform_ok`), from `FamilyKeysTyped` (an extra key of the family context's table that a path names is a type symbol; with it `PublicFamilyAgree.family_items_agree` proves that Cr's new methods agree with the family's own items at θ, the shared `ItemsAgree` that `ChainResultAgrees` is also stated through) and `PrimsGroundOk` (the primitive half of `ChainPrimsOk`, for any environment reading the fixed machine inputs as the machine types; `platform_prims_public` now discharges it, and `chain_prims_of_ground` gives `ChainPrimsOk`). The machine grounding comes from `FixedAgreement` at the family context and the closure's `PlatformFixed` | T15, T11, T29 |

`copy_apply` is proved in [PublicCopyApply.v](theories/PublicCopyApply.v)
(`copy_apply_ok`): a closure's derivation is followed down its copy chain
(`CopyChain`, `chain_entry`), each copy's context realized where its
target's is (`copy_step_realizes`), to the root. A linked root's body runs
as in the linked case (`link_entry`, `chain_run`); what it provides at
the application, and a platform root, are the two obligations read
through the chain: `ChainResultsOk` (`link_results` with a copy chain in
between; `link_results_of_chain`) and `ChainPlatformOk` (`platform_apply`
likewise; `platform_apply_of_chain`). `LinkResultsOk` alone cannot serve:
it fixes the caller's functor to the body's own (its `LinkData`, closure
shape and `AppPremises` all name u), and a copy's caller has neither.
| `KeptOutputsUnmentioned` (PublicLinkResults) | an output `bind F;` installs as itself is not mentioned by the context (`~ In t (context_gens C)`). False only where an unbound declared T is mentioned by family method provisions, where re-grounding is harmless by parametricity (unproved); decide between an Audit clause, which rejects correct programs, and that proof (scratch/orch/results2-AUDIT.md section 2) | T16 |
| `ChainKeysOk` (PublicChainNew) | static, ChainNewOk's first clause (`chain_new_ok` proves that clause from it): at an application of F through a copy chain to a root h0, each key of h0's formal result, its output roles renamed back and substituted as the selection does (outputs to destinations, inputs by θ), is `rkey_of C'` of a key of F's result as the caller reads it. Needs `CopyOutputs` both ways (copy-extra-key.moss) | T15, T16 |
| `ChainRealizeOk` (PublicChainNew) | ChainNewOk's second clause: C''s new provisions are realized after the application (`EnvOkOn` the new keys). The caller's ProvideResult reading against the root's formal one through θ, the destinations and the chain's `CopyOutputs`, with the body's provisions at its meanings (`ResultRealizes`, `read_rename`, `body_provides`) | T15, T16 |
| `captured_in_force` | what a closure captures mentions, among G's inputs, only fixed inputs and types in force where it is bound | T29 |
| `merge_formed`, `mentions_typed`, `family_sig_ok` | context formation's own premises (PublicFormation's `MergeFormed`, `MentionsTyped`, `FamilySigReadOffOk`), proved: `PublicWorldFormation.merge_formed_of`, `mentions_typed_of`, `PublicFamilySig.family_sig_ok_readoff`; `Pre` carries `FormedCx`/`FrameFormed`, so formation of elaborated types, nominal receivers and method keys follows from PublicFormation's corollaries | T29 |
| `Pre` / `Absorbs` | merge absorption for the realized context | T29 |
| `Layouts`, `Representations`, `BoolSymbols` | platform layout, cast table and Bool symbols | from `Linked`/`ElaboratesIn` |

Arm selection is proved in [PublicArms.v](theories/PublicArms.v)
(`arms_select`): a value typed at a formed scrutinee type selects an arm
whose pattern binds typed, distinct, numbered locals, and exhaustiveness
rules out falling through. It relies on the distinct-binder repair.

Checked tag construction is proved in
[PublicTagCheck.v](theories/PublicTagCheck.v) (`tag_check`), discharging
T24's premises from the platform layouts. A cast tag (`Ref`/`Array`) reads
its payload at its raw-handle requirement (`raw_payload`), so the runtime
guard reads a complete argument, and an enabled guard makes the payload a
handle type. Unit declarations have no cast entry.

Provisions are realized semantically, indexed by fuel (`EnvOk n`): every
static val is a typed `ValueProvision` under its key, and every function or
method provision is a closure that is `CallableOk n`. That means running it
at any fuel below `n` on typed arguments never gets stuck and returns a
value typed at the caller's reading of the signature. Fuel only decreases,
so a closure available at fuel `n` is only ever invoked below `n`. A closure
created at fuel `S k` therefore needs only `Below k`, with no separate
provenance certificate and no recursion through captured environments. With
this, contextual vals (`val_step`), calls of provided functions
(`invoke_step`, function and primitive closures), and calls of provided
methods and operators (`provided_step`, `methcall_step`) are proved.
`CodeSafe n` (code safe at every fuel up to `n`) lets the generic call lemmas
(`invoke_code`, `method_code`) take any safe code as receiver and arguments.
Numeric literals use them directly (`lit_step`, `lit_code`, by induction on
fuel).

Direct calls of defined functions are proved (`call_core`). The callee's
body, entered at the call's `θ` with the satisfaction map's provisions, is
realized from the caller's (`callee_realizes`):
- every need is selected;
- the callee's type table agrees with the caller's (`call_agree`);
- the callee's merges hold in its environment (`call_absorbs`);
- every callee provision is the caller's, at the callee's type. This uses
  `Satisfies`' new type check, context formation (`ctxof_formed`,
  `ctx_provision_scoped`) and reading agreement (`tyof_rel`, `sig_rel`).

Bind statements compose their items (`items_safe`). Type, value and
function binds are proved (`type_bind_safe`, `val_bind_safe`,
`fn_bind_safe`; a bound function is a defined function's closure or a
forwarder, `provider_callable`). Defined attached methods are proved
like direct calls (`attached_step`, `body_call_method`, `receiver_agree`).

`state_ok` is abstract here: the slice performs no primitive steps, so it
only has to be preserved by heap extension.

## Check and build timings

Measured in the background on detached snapshots (`scratch/measure.sh`):

| Commit | Clean `check.sh` | `nix build .#oracle` | Bootstrap |
| --- | --- | --- | --- |
| 3a302ab | 169s (overlapped another full build) | 307s | 2237s (37 min, overlapping other builds) |
| 93a2b00 (check-time merged) | 110s (overlapped proof builds) | 211s | 2113s (35 min) |

A subagent remeasured on a quiet machine (32 cores, one core busy with the
bootstrap): 118s at both 086a617 (`main`) and 3a302ab. The branch adds about
6s of CPU, none of it on the critical path. The overrun is pre-existing, and
`main` has no headroom against the two-minute budget. The wall time is
bounded by a chain of 32 dependent files ending in `LinkSynthPreserve.v`
(14.4s, 9s of it one `all:` tactic block) and `LinkProgramPreserve.v`.
Starting the two assumption audits as soon as their own dependencies are
built, instead of after the whole build, would hide most of their 11s
without touching any proof.

## Later cleanup

- Freshness is stated locally in several places: `Fresh W` (undeclared),
  captured vals unprovided in the context, `FreshFor` (avoiding `fr_fresh`
  and the context's generations), and a lambda frame's `fr_fresh`
  inheriting the generations in scope where it is made. These overlap, and
  each was added because a local condition let a derivation reuse a name the
  runtime still holds. Both implementations draw new symbols and
  generations from one global counter. Once the safety proof is done,
  replace these with one global notion of a new symbol or generation, and
  look for similar redundancies the tightenings above introduced.

## Time log

- Checkpoint 1 (18:47 to 19:31, about 45 minutes): the reduction, the
  stuck-site table, unit-tag typing, type-reading lemmas, two
  counterexamples and their repairs, and the frame/local foundations. I had
  planned this as two hours.
- Checkpoint 2 (19:37 to 19:51, about 14 minutes): the slice induction
  (`safe_all`), with loops, records, fields, match, let, assignment and
  constants. I had estimated a few hours.
- Arm selection (19:51 to 19:56): `PublicArms.arms_select`.
- Tag checks (19:56 to 20:00): `PublicTagCheck.tag_check`.
- Semantic provision realization, vals, provided calls, methods and
  operators, `CodeSafe` and literals (20:00 to 20:10).
- Found the merge-orientation gap while planning direct calls (20:10 to 20:12).
- Recorded per-symbol contexts in the world, after approval.
- `Satisfies` type check after approval (20:58 to 21:03).
- Context formation, call agreement and `callee_realizes` (21:07 to 21:28).
- Bind framework, type/value/function binds, attached methods (21:33 to 22:05).
- `tyof_rel`, `body_call`, `call_core`, and the merged-class counterexample
  (20:23 to 20:41). `CallTheta` after approval (20:42 to 20:55).
- Bracketed attached-method counterexample, fix after approval, and
  `meth_bind_safe` for attached providers (22:10 to 22:27).
- Lambda capture freshness, the `context_gens` merge keys after approval,
  and lambda binds for fns and methods (`lambda_fn_bind_safe`,
  `lambda_meth_bind_safe`) (22:40 to 23:20).
- Wrapped `main` test, `theta_ctx` after approval, and wrapper method
  providers (`wrapped_callable`, `wrapped_meth_bind_safe`); method binds
  are now fully proved (23:21 to 23:35, by commit times).
- Functor design reading; functor-body `break` counterexample, `Breaks`
  with the world after approval, and `bind F;` for a functor with a body
  (23:35 to 00:01).
- Functor design (binding-level refactor needed), the `INTERP` refactor,
  and the entry assembly `entry_safe` (00:01 to 00:17).
- Primitives (T11/T17) by a background agent (`primitive_safe`), merged;
  `ShapeNever` after approval; realization without a fuel drop
  (`callee_realizes_le`, `lambda_realizes_le`) (00:17 to 01:23, mostly
  discussion of `ShapeNever`).
- `PUBLIC` refactor; primitives at the entry (`machine_lookup`,
  `entry_primitive`) and `safety_public` (01:23 to 01:27).
- Binding-level refactor: `BindStep`/`Chain` with generations made,
  `chain_wrap`/`chain_run`, every non-functor bind as a chain, functor
  hypotheses restated as steps (01:27 to 02:09, with the formation agent
  running in parallel). Earlier entries in this log written from my own
  estimate rather than commit times overstated durations; these are
  corrected to commit times.
- Functor invariant design (step-indexing problem, structural closure
  invariant), the captured-merge gap and functor body numbering after
  approval (02:09 to 02:45).
- Family matchings read off the key (`FamilyReadOff`), proving
  `demand_formed_cx` without its hypothesis (02:45 to 02:52).
- Binding steps capped by eval fuel and `AppAt` as an argument (02:52 to
  03:00); the copy-capture counterexample and `fc_linked` after discussion
  (to 03:12).
- `ClosureOk` in `EnvOk`, copies (`case_BFunCopy`), `AliasOk`, and closures
  (`case_BFunClosure`, with `captured_in_force`) (03:12 to 03:39); the
  check-time branch merged.
- The linked and copy interpretation gaps: tests, two oracle variants
  compared over the corpus, and the premises after approval (03:40 to
  04:11).
- Merges recorded in closures (`link_merges`, `bind_merges`), the body
  entry (`link_entry`), `apply_theta`, and the application step's structure
  (`case_BApplyAbstract` with `refile_run`, `link_results`, `copy_apply`,
  `platform_apply`) (04:11 to 04:34).
- Formation through `Pre`, discharging `ex_formed`, `ex_nominal_formed`,
  `bind_nominal_formed` and `lambda_keys_scoped` (04:34 to 04:51).
- Rule 7's `family_direct` in Audit after approval, the family cases from
  one `family_step`, and `safety_obligations` (04:51 to 05:03).
- Orchestration with obligation agents in separate worktrees (05:03 to
  06:36): worldform, copy, refile, family, platform, repair and results
  branches merged into `safety` in turn (per-agent notes in
  `scratch/orch/*-STATUS.md` and `*-AUDIT.md`); results handed to a fresh
  agent (results2), the family platform step to another (famplat).
- Orchestration continued (06:36 to 07:25): famplat, results2,
  results3 and family2 merged; Audit changes for result shadowing
  (removing `fresh_result_types`), `MachinesFixed`/`HandlesRead` in
  families, and key-receiver capture in lambdas; `KeptOutputsUnmentioned`
  left for the user; fresh agents results4, family3 and chainagree started.
