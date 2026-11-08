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
| `apply_functor` | `e_compose` (3) | result items absent from the body environment | `LinkBody` `ResultRealizes`, `ProvideResult` | The body's binds provide every refiled result key (T15/T16). |
| `apply_functor` | `e_compose` (3) | `result_environment` fails: an output is unreadable or its destination is occupied | `BApplyAbstract` `FreshFor` and the output clauses | Fresh destinations are absent from the caller's runtime `type_meanings` (T16). Needs runtime generations ⊆ static `context_gens`. |
| primitive call | `e_missing_primitive` (15) | `supports interface p = false` | `BPrimitive`, platform entries, `Platform.Family` bodies | `Compatible` requires every operation in `required_primitives`. Invariant: every runtime `PrimitiveClosure` came from program code or the initial environment. |
| primitive result | interface laws | ill-shaped arguments, or a missing `gc_pre` | `AtKey`/`PrimitiveSignature`, `MethodPrimitive` | Arguments typed at the signature yield `value_shape` (T11); GC arguments satisfy `gc_pre` under the ghost heap (T17). |
| `run_main` | `e_nobody` (4) | no body for `main` | `ElaboratesIn` | `main` is a defined fn, so `FnElab` gives its body. |

`ETag` and `check_tag` only `Trap`; `ELet`, `ESeq`, `ELoop`, `EReturn`,
`ERecord` and `eval_arguments` only propagate.

## Observations so far

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
| `demand_case` | family applications a call demands (`Demand` wrappers) | T15, T16 |
| `attached_defined_case` | defined attached methods (direct calls) | T03, T06, T14 |
| `family_method_case` | methods a family supplies at the call (functor application) | T15, T16 |
| `literal_family_case` | literal arithmetic a family supplies (functor application) | T15, T16 |
| `bind_case` | bind statements | T07, T15, T16 |
| `ex_formed` | elaborated types are formed (unions of distinct nominal heads), also after `canon` | T29 |
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

`state_ok` is abstract here: the slice performs no primitive steps, so it
only has to be preserved by heap extension.

## Check and build timings

Measured in the background on detached snapshots (`scratch/measure.sh`):

| Commit | Clean `check.sh` | `nix build .#oracle` | Bootstrap |
| --- | --- | --- | --- |
| 3a302ab | 169s (overlapped another full build) | 307s | 2237s (37 min, overlapping other builds) |

A subagent remeasured on a quiet machine (32 cores, one core busy with the
bootstrap): 118s at both 086a617 (`main`) and 3a302ab. The branch adds about
6s of CPU, none of it on the critical path. The overrun is pre-existing, and
`main` has no headroom against the two-minute budget. The wall time is
bounded by a chain of 32 dependent files ending in `LinkSynthPreserve.v`
(14.4s, 9s of it one `all:` tactic block) and `LinkProgramPreserve.v`.
Starting the two assumption audits as soon as their own dependencies are
built, instead of after the whole build, would hide most of their 11s
without touching any proof.

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
- `tyof_rel`, `body_call`, `call_core`, and the merged-class counterexample
  (20:23 to 20:41). `CallTheta` after approval (20:42 to 20:55).
