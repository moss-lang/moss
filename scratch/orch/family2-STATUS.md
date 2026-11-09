# family2: scoping up to declared symbols (option (b)) — STATUS

Branch `ob-family2`; `safety` 0fc4e33 is merged (a fast-forward; no proof
changes are committed on this branch). Time: about 2h.

**Result: stopped at goal 1, on a design question for the lead.** The
run-time "no meaning" fact that option (b) relies on is false for code
inlined from a defined val (`ExConst`). This is a proof-level obstruction.
I have not shown it is a safety counterexample (see "Is it a real bug?").
Nothing is proved yet. The work in progress is saved as a diff, and the
proof files are back to `safety`.

Goal 4 (`DeclaredScopedOk`) was dropped by the lead.

## What the WIP diff does (scratch/orch/family2/inert-wip.diff)

Apply it with `git apply` on 0fc4e33. It does not compile yet.

`PublicFormation.v` builds with it:
- `GenScoped C y := In y (scope_gens C) \/ kind_sym W y = Some DKType`,
  and `ScopedTy C t`.
- `KeysScoped` is relaxed to `ScopedTy C r` for each method key's receiver.
  - It now depends on W, so outside its section it is `KeysScoped W C`.
  - `Scoped`/`InScope` clash with Audit names.
- `set_provision_formed` and `family_application_ok` (matching values
  `ScopedTy`) are restated to match.
- `family_matching_scoped` (receiver `ScopedTy`), `demand_formed`,
  `demand_formed_cx`: restated to match.
- `output_entries_ok`/`provide_result_ok` take a predicate
  `K : nat -> Prop`, via `GensIn K l` and `subst_within`.
- `keys_scoped` gives `scope ∨ declared`, and `keys_scoped_context` gives
  `context_gens ∨ declared`.
- New helper lemmas: `scope_fold_set_ty`, `scope_app_ty`, `inscope_mono`,
  `scoped_ext`, `canon_scoped`.

`PublicBodySlice.v` is about 70% fixed:
- `Inert C d`: a declared type symbol outside `scope_gens C` has no
  meaning, i.e. `ground d (TAbs y) = TAbs y`. It is a fourth conjunct of
  `Realizes`, and destructuring patterns are updated to `[[Idem KM] Ine]`.
- `lambda_realizes_le` is proved under the relaxed key premise
  (`context_gens ∨ declared`) with `Inert`, via `ground_capture_inert`
  and `lookup_capture_none`. It also delivers `Inert Cl de`.
- `Inert` is proved at callee entry (`inert_callee`, from
  `table_formed_keys_scope`), at functor-body entry (the `Mde` case) and
  across `bind T = τ` (`bind_type` changes only aliases).
- It is also proved for `realizes_set`, `realizes_install_val`,
  `realizes_narrow` (new premise: the same table) and `realizes_ty_free`
  (new premise: scope inclusion).
- The build stops at the `ExConst` case of `ex_step`. That case is the
  obstruction below.

Not yet touched:
- The other files whose destructuring of `Realizes` or `KeysScoped`
  changes: PublicFamilyStep, PublicCopyApply, PublicPlatformApply,
  PublicLinkResults, PublicLinkMerges, PublicFamilySig, PublicEntry.
- Goals 2, 3 and 5.

## The obstruction: inlined consts break "no meaning"

`ExConst` elaborates a defined val's initializer in `Cx [] [] [] []`. The
code is inlined at the use site and evaluated in the **caller's**
environment `d`; the slice's const case reuses the caller's `d`, locals
and state for `Pre`. So `Pre` at the empty context needs `Inert (Cx [] [] [] []) d`:
- That says every declared type symbol has no meaning in `d`.
- It is false whenever the caller is a body with a requirement `T`
  (`callee_env` gives `T ↦ τ`).

Every weaker form I tried fails, for the same reason:
- **Restricting to symbols the context mentions.** The family step must
  then show that a declared, out-of-scope symbol of the demanded receiver
  `ot` has no meaning. `ot` can come from a call result read at an unbound
  callee requirement, which the context need not mention.
- **Restricting to method-key receivers.** The family step adds receivers
  from `ot`, so the same problem returns.
- **A static route.** This would show that a key's out-of-scope symbol is
  in the key's own provision gens, and so in `context_gens`, which lambda
  capture keeps. For methods, `AppOf` selects only `SigMentions`
  requirements, which exclude the receiver. So in
  family-unbound-need.moss, `T.foo`'s inst is `[]` and T occurs only in the
  key.

What lambda capture actually needs is narrow. It needs it for a symbol
that occurs only in a method key's receiver (not in `context_gens`), is
declared and out of scope, and still has a meaning in `d1`. Lambda
capture then drops that meaning (`lexical_capture` maps only
`context_gens`), so the captured key is realized at a different grounding.
In ordinary bodies this cannot happen: meanings are made only for table
keys (callee and functor entry), kept outputs (which become table keys)
and lambda captures. The WIP diff proves `Inert` is preserved by all of
these. Only inlined const code runs with a foreign `d`.

The scenario the proof cannot exclude:
- A function that assumes T, so T has a meaning at run time, uses a
  const.
- The const's initializer binds a functor (`bind F = G;`) and calls a
  function that needs `T, T.foo` with T unbound in the const's empty
  context.
- So family F is applied at `A := T`, giving the key `T.foo` with T
  meaning τ in d.
- The call has a lambda argument, which captures the demand context and
  drops T.

### Is it a real bug?

Probably not, but I have not shown it either way. In the lambda the
captured key's receiver grounds to `T` where it was installed at `τ`.
- No value is typed at `TAbs` (PublicValueTyping), so a receiver of exactly
  `T` makes the method's `CallableOk` vacuous.
- A mismatch could matter only for a nominal receiver such as `Box[T]`,
  and only if a payload made the argument relevant.
- Getting such a value in the lambda needs a call result read at the site,
  where `T` grounds to `T`, so it carries no `τ` value.

A `.moss` test of this shape would settle the oracle and native side. I
did not write one.

### Options for the lead

1. **Two-mode invariant.** Use `Inert` in bodies, and in code inlined from
   a const use only "no method key has an out-of-scope receiver symbol".
   - This needs either a premise that const initializers apply no family
     (excludes some accepted programs), or a proof that families in const
     code are harmless.
2. **A semantic argument.** Show the regrounding is harmless: a key
   receiver mentioning an out-of-scope declared symbol is only reached
   with values whose typing ignores that symbol. This is a value-typing
   lemma (phantom arguments), and it may be substantial.
3. **Audit/runtime change.** Lambda capture keeps the meanings of
   key-receiver symbols too (option (c) of family-STATUS: `context_gens`
   includes method-key receiver gens), or consts evaluate in an
   environment with no meanings. Either needs native and oracle evidence.
   The first mirrors what an implementation that captures by mention
   would do.

Option 3a removes the need for `Inert` in lambda capture altogether. With
it, the relaxed `KeysScoped` plus `context_gens` gives `lambda_realizes_le`
directly, and goals 2, 3 and 5 can proceed without a run-time fact.

## Note on lambda-declared-output.moss (coordinator's question)

This is consistent with `Inert` as defined here. After the lambda body's
`bind H;`, the kept output T is a table key (`(T, TAbs T)`), so it is in
`scope_gens`, and its new meaning is allowed. The separate question,
whether rewriting `T ↦ m` regrounds the provision `T.foo`, is results2's
`dest_scope` issue. I have not decided it.

## Handoff (if the lead picks an option)

- Option 1 or 2: apply the diff, finish the `ExConst` case per the choice,
  then fix the listed files. Destructuring of `Realizes` changes in each.
  Where `d` is unchanged, `conj (conj Idem KM) Ine` closes the goal. Where
  meanings are equal, use `inert_mono`.
- Option 3a: drop `Inert`. Revert `lambda_realizes_le` to its strict
  premise and derive it from `keys_scoped_context` plus the new runtime
  capture.
- Then goal 2: add a locals/provisions `ScopedTy` conjunct to `FormedCx`
  and an `ex_scoped` fixpoint alongside `ex_ok`. Restate `ExScopedOk`,
  `LitScopedOk` and `DemandScopedOk` with `ScopedTy`. `FamilyStepOk`'s
  `incl (gens ot) (scope_gens C0)` and `FamilyTableIn` relax to
  `ScopedTy`, and `PublicFamilyStep.family_formed` follows
  `family_application_ok` as restated in the diff.
