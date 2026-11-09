# family4: Audit changes

## 1. A functor closure captures types through `lexical_capture`

**What.** `Audit.v`:
- `BFunctor` takes a `type_capture` (after the body symbol), like
  `BFunction`/`BMethod`;
- `eval_bindings` installs `FunctorClosure h [sm] (typed_capture d types dc)`
  (was `capture_environment d dc`, all of `d`'s meanings and aliases);
- `BFunClosure` elaborates to `bind_functor_expression u h (lexical_capture C) sm smc`.

`capture_environment` itself is unchanged (PublicCaptureFacts still states
facts about it). PublicClosurePrototype's compiler and the two fixtures
(PublicClosureExample, PublicFunctorScopeExample) follow; the fixtures'
results are unchanged.

**Why.** `CapturedInForceOk` (T29) said every symbol a captured need
mentions, among G's inputs that F does not take, is in force where G is
bound. That is false once declared symbols are admitted (option (b)):
- examples/functor-captured-unbound.moss: use_it's context lists T and
  `T.twice[A=T]`; main leaves T unbound, so the call demands family F at
  `A := T`. Its argument is checked in that demand context, and
  `bind H = P;` there captures P's need `T.twice`, keyed at T, which is not
  in the type table.
- Oracle (moss_m2): accepted, exit 0. Native (MOSS_SEED=... moss run):
  accepted, exit 0. So option (i), rejecting it, is out.
- What the slice needs (`case_BFunClosure`, `GroundGm`): G's application
  reads such an input x as `cmeaning dc x` (alias, else meaning), and that
  must equal `ground d (canon C (TAbs x))`, the meaning the need was
  provided at. With `dc` holding all of `d`, that is a run-time fact ("no
  alias of a symbol outside the table differs from its meaning"), false in
  a defined val's initializer, which is inlined in the caller's
  environment (family2-STATUS, "inlined consts").
- With `lexical_capture C`, aliases are exactly the table's readings and
  meanings are those of `capture_gens C`. A symbol not in force but
  mentioned by C (here through `T.twice`'s receiver) then reads as its
  meaning, so the readings agree by construction (`cmeaning_capture`).
  The static half, that such an x is in `capture_gens C`, is
  `PublicLinkMerges.captured_mentioned` (from 3b's Satisfies: x survives
  θ, so it is in the caller's provision's types, its instantiation, its
  merge, or its key's receiver).

**Restated obligation** (lead-approved): `CapturedInForceOk W` gains
`FunCtx W gg Cg nsg` and concludes
`nlook x (c_ty C) <> None \/ In x (capture_gens C)`. Proved
(`PublicEntry.captured_in_force_ok`, `PublicLinkMerges.LinkMerges.captured_in_force_ok`);
removed from `safety_obligations`.

**Evidence that it mirrors the implementations.** Run-time only, so Audit
rejects nothing new. The oracle's functor closures use
`ExecutionModel.xclose d dc = dc ++ xtype_env d` (XBindFun), the same as its
lambdas; it has a single type map, so its captured reading of a need's
receiver always matches the key it was installed at. Audit's lambdas
already capture through `lexical_capture` (family2-AUDIT §1); functor
closures now do the same. For a symbol in force the reading is unchanged
(`AliasOk`: the old alias was `ground d tx`, the new one
`ground d (canon C tx)`, equal by `Absorbs`). It differs only for an input
of G that is not in force and that nothing in C mentions, which no captured
need mentions (3b) and so no captured provision's typing depends on.

**Alternatives considered.**
- (i) An Audit 3b clause requiring those symbols in force: rejects a
  program both implementations accept.
- A run-time alias fact in `Pre`: false for const code.
- Keeping `capture_environment` and restricting only aliases: still needs
  a fact about `d`'s meanings for symbols outside the table.

## 2. A family's listed polymorphic inputs are type symbols

**What.** `Audit.v` `FamilyShape` gains a conjunct: when the functor lists
its polymorphic inputs (`F[A, B]`), each name resolves (if at all) to a
declared type symbol. `Family` (hence every family application) requires it.

**Why.** `FamilyKeysTyped` (PublicPlatformApply, goal 5): a key of the
family context's table that a path names and that F's inputs lack must be
a type symbol. Those keys are the context's own (declared types, now
`TysScoped`) or the matching's, which are P(F) symbols (`Poly`). An
unlisted P(F) is M(F), so it is an input of F. A listed one had no kind
constraint, so `F[A, g]` with `g` a fn put `g` in the matching.

**Test.** examples/family-poly-nontype.moss. Oracle: rejected ("`g`,
listed as a polymorphic input of `F`, is not a type in scope (D84)",
`Elab.functor_params`, whose soundness lemma is
`NestingTypesExec.functor_params_names`). Native: accepts, exit 0. D84
rule 1 makes P(F) type symbols, so native is the outlier: filed as
proof-plan E34. Audit now rejects the program (the demand finds no family).

**Mirrors.** Exactly the oracle's check, restricted to families (the
oracle checks every listed functor, at its first use as a family or an
application). Programs the oracle accepts are unaffected.

**Alternatives.** Reading the matching's keys off FamilyReadOff (a key's
receiver mentions only M(F) symbols): a long argument through
ProvideResult/RecvTy that would still need the kind of a listed but
unmentioned name.
