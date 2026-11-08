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
| `val_case` | contextual vals (`EVal`) | T04 |
| `call_defined_case`, `call_abstract_case` | calls, `Demand` wrappers | T06, T14 |
| `method_case`, `unary_case`, `binary_case` | method calls and operators | T06, T14 |
| `literal_case` | numeric literals through contextual arithmetic | T05, T06 |
| `bind_case` | bind statements | T07, T15, T16 |
| `tag_check` | checked tag construction from a typed payload | T24 premises |
| `arms_select` | arm selection and binder typing for a typed scrutinee | T08 |
| `Pre` / `Absorbs` | merge absorption for the realized context | T29 |
| `Layouts`, `BoolSymbols` | platform layout and Bool symbols | from `Linked`/`ElaboratesIn` |

`state_ok` is abstract here: the slice performs no primitive steps, so it
only has to be preserved by heap extension.

## Time log

- Checkpoint 1 (18:47 to 19:31, about 45 minutes): the reduction, the
  stuck-site table, unit-tag typing, type-reading lemmas, two
  counterexamples and their repairs, and the frame/local foundations. I had
  planned this as two hours.
- Checkpoint 2 (19:37 to 19:53, about 16 minutes): the slice induction
  (`safe_all`), with loops, records, fields, match, let, assignment and
  constants. I had estimated a few hours.
