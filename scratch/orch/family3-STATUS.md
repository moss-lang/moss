# family3: scoping up to declared symbols — STATUS

Branch `ob-family3`. `safety` 93581f2 is merged. `check.sh` passes.

## Done

### Goal 1: `KeysScoped` relaxed (e9399bd)

- `PublicFormation`: the PublicFormation part of family2's WIP diff.
  - `GenScoped C y := In y (scope_gens C) \/ kind_sym W y = Some DKType`.
  - `ScopedTy C t`.
  - `KeysScoped` is up to declared symbols.
  - The comment no longer mentions the abandoned `Inert`.
- Downstream:
  - `formed_narrow`;
  - `family_formed`, now over `K := GenScoped W C0`;
  - `captured_in_force_of`, which keeps the strict key premise as an
    explicit hypothesis (prototype only, see goal 3).
- `envok_scoped` and `pre_meanings_free` (PublicLinkResults) read
  grounding over `S.capture_gens` (context_gens ++ key receivers), so they
  no longer need `KeysScoped`.
- Slice: `FamilyTableIn`, `FamilyStepOk`, `ExScopedOk`, `LitScopedOk` and
  `DemandScopedOk` use `ScopedTy`.

### Merge fix (2c86860): `KeptOutputsUnmentioned` covers key receivers

PublicChainCore (results3) read method-key receivers through the strict
`KeysScoped`. A receiver may now mention a declared symbol outside
`context_gens` (family-unbound-need.moss).
- So `KeptOutputsUnmentioned`'s conclusion is now
  `~ In t (S.capture_gens C)`. It was `~ In t (A.context_gens C)`.
- `dest_scope`'s second conjunct changed the same way. The fresh case uses
  the relaxed `KeysScoped` and `Fresh` (not declared).
- `ground_after` takes `capture_gens`.

**For the user:** this restates the user's premise. Its intent is
unchanged: the output must not be mentioned by the context, now including
a family key's receiver. The re-grounding it guards against is the same
parametricity question.

### Goal 2: `ExScopedOk`, `LitScopedOk`, `DemandScopedOk` discharged (a0a1af3)

New file `theories/PublicScoping.v`, a Certificate root.
- `CxScoped W F C`: locals, provision gens (`provision_gens`) and
  `fr_this F`, each `ScopedTy`/`GenScoped` in C.
- `ex_scoped`: a mutual fixpoint alongside `ex_ok`, with the same
  patterns.
  - An elaborated type is scoped.
  - A Blk's type is scoped or in `made`. `BlkE`'s escape check removes
    `made`.
  - Bind items give `BindScoped`: `CxScoped` after, scope grows, the new
    scope lies within old scope ++ made, and `fr_this` is kept.
- Family results:
  - `output_entries_gens` / `provide_result_gens`: entries'
    `EntryP (TyIn K)`;
  - `family_application_gens`, `family_result_cxscoped`;
  - `apply_abstract_scope`, `apply_abstract_cxscoped`.
- Entries:
  - `ctxof_cxscoped`, `functx_cxscoped` (via `entries_cx_scoped`);
  - `fn_body_cxscoped`, `attached_this_scoped`;
  - `lambda_cxscoped`, `cxscoped_narrow`.
- `literal_scoped`.
- `demand_at_site_scoped`. A need's receiver read at site X, where
  c_sub X = c_sub C and X's table is read in C's scope, is ok and scoped.
  - It uses `ctxof_keys_typed`: CtxOf table keys are DKType.
  - It also uses `need_provision`.
  - It rewrites the reading as one composed substitution.

The slice's `Formed` is now `FormedCx /\ FrameFormed /\ CxScoped`.
Updated sites:
- body, method, functor and const entries;
- lambda (`lambda_callable` has 3 new premises);
- match arms, let, `cap_formed`, `set_fun`, `item_formed`;
- `family_step_ok` (`pre_same_table` takes `CxScoped`);
- `PublicPlatformApply`, `PublicLinkResults.pre_after_result`,
  `PublicCopyApply`, `PublicEntry.entry_pre`.

Approved restatements:
- `ExScopedOk` at expected type `None`, which is its only use.
- `DemandScopedOk` gains `A.c_sub X = A.c_sub C`. Without it the
  statement is false: X's merge is unconstrained.

They are proved in PublicEntry (`ex_scoped_ok`, `lit_scoped_ok`,
`demand_scoped_ok`) and are gone from `safety_obligations`.

## Not done

### Goal 3, `CapturedInForceOk`: false under (b); needs the lead

Its conclusion is `nlook x (c_ty C) <> None`. Under option (b) the bind
context C can provide a captured need that mentions a declared symbol
that is not in force. The shape:
- Code calls a callee that needs `T, T.twice[A=T]`, leaving T unbound.
  This is family-unbound-need-sig.moss.
- The call's arguments are elaborated in the demand context C' (ExCallD:
  `Args ... C' ...`). C' has the family method `T.twice` keyed at T,
  and T is not in C''s table.
- An argument block runs `bind F = G;`. G (with a body) has input T, not
  an input of F, and needs `T.twice`.
- 3b holds: θ(T) = TAbs T, and C' provides `T.twice` at T. But T is not
  in force.

I have not written this as a .moss test. Whether the oracle and native
accept it is unknown.

What the slice needs is `case_BFunClosure`'s `GroundGm`. For such an x:
- `gm(x) = cmeaning dc x`, the caller's alias, else its meaning;
- this must equal `ground d (canon C (TAbs x))`.

For a not-in-force x (not merged) that is "no alias of x in d differs
from its meaning". That is a run-time fact. Code inlined from a const
(ExConst) runs with the caller's d, where T can have an alias, so the
fact needs the same care as family2's `Inert`.

Options:
- (i) Add an Audit clause to BFunClosure 3b: the captured needs' G-input
  mentions not among F's inputs are in force. This mirrors what
  CapturedInForceOk assumes. It needs native and oracle evidence.
- (ii) Restate with `\/ (lookup_subst x (c_sub C) = None /\ declared)`
  and add the run-time alias fact to `Pre`.

`PublicLinkMerges.captured_in_force_of` still uses the strict key
premise and `DeclaredInForce`. Both are false for accepted programs.

### Goal 4, `SigCoherent` and `FamilyCoherentOk`: not started

The plan in family-STATUS.md ("Suggested design" 3) still applies.

Adding a conjunct to `Formed` touches every `Pre`-concluding proof. This
change showed where those proofs are; grep for `PublicScoping.` in
the slice, CopyApply, PlatformApply, LinkResults, FamilyStep and Entry.
Each of them will need the same treatment.

### Goal 5, `PA.FamilyKeysTyped`: not started; findings

- Every table key the elaboration makes is a declared type symbol:
  - BTypeBind has `K`;
  - BApplyAbstract outputs (`outputs_typed`);
  - CtxOf (`PublicScoping.ctxof_keys_typed`);
  - FunCtx (`PublicLinkMerges.functx_keys_typed`).
  So "declared or fresh" can be just "declared".
- Two key sources are not obviously declared:
  - The matching's keys. These are P(F) symbols: `Poly`, the
    `Some names` case is `resolve_sym` of a parameter name, and I found
    no kind constraint on it.
  - An attached call's `rargs` keys (`w_reqs h`).
- Suggested route:
  - Add `KeysDecl C` (table keys DKType) to `CxScoped`. It holds at every
    entry and is kept by bind items (BTypeBind `K`, outputs typed).
  - Restate `FamilyKeysTyped` with `KeysDecl C` plus "matching keys are
    keys of Cu, or DKType". A Poly symbol is in u's FunCtx table: that
    needs a FunCtx lemma.
  - Thread `KeysDecl C` through `FamilyPlatformApplyOk`/`FamilyStepOk`.
    C is C0 or C0 with `w_reqs h`, so `w_reqs` must be declared too,
    which needs a world fact.

## Time

About 3h.
