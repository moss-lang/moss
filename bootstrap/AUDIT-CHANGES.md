# Changes to Audit.v during the `public_safety` proof (G05)

> [!NOTE]
> The commit hashes cited below, and the records `SAFETY-SITES.md` and
> `proof-plan.json` (with its E-numbers), are from the branch merged as
> [PR #49](https://github.com/moss-lang/moss/pull/49). GitHub keeps that
> PR's commits reachable as `refs/pull/49/head`; the two records, since
> deleted, are also in git history.

This document is for review. It lists every change made to
[theories/Audit.v](theories/Audit.v) on branch `safety`, from the baseline
086a617 ("Keep the proof DAG focused on stable obligations") to 55b1b15.
There are 56 non-merge commits (`git log --reverse --no-merges
086a617..55b1b15 -- bootstrap/theories/Audit.v`). They are grouped here
into **47 entries**. Reverted and superseded commits are folded into the
entry they belong to.

**Sources.**
- The commits themselves (`git show`).
- The agent records `scratch/orch/*-AUDIT.md`, with the comparison
  scripts and outputs `scratch/orch/*-cmp.{sh,out}`.
- `SAFETY-SITES.md`, "Observations so far".
- `proof-plan.json`, E21–E37.
- The header comments of [examples/](examples/).

**Reading notes.**
- "Oracle" means the bootstrap elaborator plus its reference interpreter
  (`moss_m2.exe`, `moss_ref`). "Native" means `moss run`.
- Audit is a relation, not a program. Its verdicts below are the ones the
  records state. They come from reading the rules or from an oracle
  *variant* that adds the new check. When a record says a verdict was
  argued and not run, this document says so.
- A *corpus comparison* builds an oracle variant that adds the new check
  and compares its verdicts with the plain oracle's over `tests/`,
  `examples/`, `bootstrap/examples/` and, sometimes, the hunt directories
  in `scratch/`. "N same / M diff" counts files.
- The `safety` commits are not pushed, so they are given as hashes only.
  `scratch/` is git-ignored, so its paths are plain text.
- Classes are the ones requested:
  - **A**: reject-only, mirroring both implementations;
  - **B**: rejects programs that some implementation accepts;
  - **C**: accepts programs that an implementation rejects;
  - **D**: changes run-time semantics;
  - **E**: a platform-world condition only.

  Where an entry fits none of these exactly, it says so.

## Summary

| # | Change | Commits | Class |
|---|---|---|---|
| 1 | Record patterns bind distinct names | 08fc526 | B |
| 2 | A `break` in a `while` condition leaves the loop | 8a023a7 | B |
| 3 | Call instantiation through the callee's merges (`CallTheta`) | f59b6f4, 6fd598a | B |
| 4 | Selected provisions have the callee's types (`Satisfies` at θ) | cb6ce7b | B |
| 5 | Bound attached methods read at the key's receiver | 465622d | B |
| 6 | Functor-body breaks count (`ItemBreaks`) | ae3c360 | B |
| 7 | 3b covers every captured item | 43cd6d9 | B |
| 8 | Main's platform fns read raw handles as themselves | 1f5f71d | B |
| 9 | A family's listed polymorphic inputs are types | f6ca908 | B |
| 10 | A kept output is unmentioned by the context; since option (b), a mentioned output is fresh | 642490e, proposals | B, then none |
| 11 | A linked input reads in G over F's inputs (cap3b) | 807bbbf | B |
| 12 | A method's θ records every signature type symbol (sigclose); M(F) counts a fn's too (#12b) | 61040f2, proposals | B; #12b a relaxation |
| 13 | A linked body does not keep an output at an input of F | 268bf3a, 4d55710 | B |
| 14 | `link_site`: result keys read inputs at G's table | 94d89ce, fc75de7 | C |
| 15 | Lambda captures keep merge keys | a5b081c | D |
| 16 | A wrapper calls its fn at the fn's own context | 77de8be | D |
| 17 | Only linked inputs come from θ (`fc_linked`) | de53d1d | D |
| 18 | Body inputs read through G's table (`fc_table`) | 206d739 | D |
| 19 | No refile; results read from source keys | 870db77 | D |
| 20 | **Results shadow an occupied destination (removes b4f8279's check)** | 712aecd | **D** |
| 21 | Lambda capture keeps method-key receivers | fb198f2 | D |
| 22 | Functor closures capture through `lexical_capture` | fb41ce0 | D |
| 23 | Install exactly F's declared result under the caller's keys | e029263, dbd51d0, 0e211e6, cff9949, baf6ea6 | D |
| 24 | Receiver slot reserved in numbering | c75eb2e | A |
| 25 | `else` branches checked against the expected type | 33dd6b5 | A |
| 26 | The world records each symbol's context (`w_context`) | 37d235f | A |
| 27 | Attached methods need a nominal receiver (Q5) | ee7fd05 | A |
| 28 | Captured vals are new symbols | c6f1b9d | A |
| 29 | A lambda's body frame keeps the generations in scope | e0d6f9c | A |
| 30 | A functor closure's captured needs satisfy G; body numbering | 64e2a92 | A |
| 31 | A family's matching is read off the key (`FamilyReadOff`); since item 6, uniquely | a12f7ab, proposals | A |
| 32 | Linked bodies and copies read F's needs as F does | d458480 | A |
| 33 | A family member is not bound directly (rule 7) | 1cd1ce5 | A |
| 34 | `MostGeneralMerge`: σ is a function | f17fb86 | A |
| 35 | `CopyOutputs`: a copy's result reads as its source's | 0a96d3f, ad3417e, a8ef896 | A |
| 36 | A copy's source takes no input its target lacks | a447443 | A |
| 37 | Every output destination is made (D78) | 914cd3c (3afe6ea, b2c4a90 superseded) | A |
| 38 | `ResultRealizes` at the body's output meanings | 671b6a5 | A (a relaxation) |
| 39 | Main reads a platform functor's fixed machine types as themselves | be77015 | A |
| 40 | A copy's target is abstract | cd1108d | A |
| 41 | `ShapeNever` for never-returning primitives | 44d5cfc | E |
| 42 | A platform family files items under run-time keys | b39e1d5 | E |
| 43 | A platform family has no type output | 668565c | E |
| 44 | A platform family's machine types are fixed inputs | f152c5b | E |
| 45 | A platform family's items read raw handles as themselves | f9d33bf | E |
| 46 | A platform family's items have distinct methods | 3544241 | E |
| 47 | A platform family is abstract, with distinct keys | cf21df4 | E |
| 48 | **No expanding cycle (`NoExpandingCycle`, D16/D30)** | branch ob-expand | **B** |
| 49 | Pins constrain only a linked body's own binds | proposals | relaxation (toward both) |
| 50 | A call's θ is principal (`CallTheta`) | coherence | A |
| 51 | F's congruence decides "same need" across contexts (`canon_item`) | 71be6ac (sameneed) | C and D (a relaxation) |
| 52 | A merge class is one vertex of the expanding-cycle check | coherence2 | B (no verdict found that changes) |
| 53 | Main's fixed machine types read by class (`FixedMachinesRead`) | coherence2 | A |
| 54 | `Satisfies` checks the callee's merges by class | coherence2 | relaxation (toward native) |
| 55 | A bracket application on a fn call is its expansion (`ExCallBr`, D89) | call-brackets | an extension made in both implementations (a relaxation toward both) |
| 56 | A copy composes its smap and positions into the closure (eager copies) | eagercopy | D (unobservable) |
| 57 | Assumed items bring their requirements and needs (`AssumesComplete`, D20) | deltafix | A (one test B: rejects a program native accepts) |
| 58 | Result items distinct in F stay distinct at an application, and H's at a copy (`CollidesOnlyAs`) | deltafix | B (coherence) |
| 59 | `RecvTy` reads no type symbol out of force as a nominal | deltafix | A (rejects a program native accepts: a native bug, E41) |
| 60 | **Rule B: a use needs the item's type requirements in force; demands read needs through θ; a closure's captured inputs in force** | lang-b | **B** (with the oracle; native E42) |
| 61 | Merging two entries of one item unifies their instantiations (Δ4, D43) | lang-b | B and C (coherence; with the oracle; native E43) |
| 70 | A defined method is not bound (`BMethBind`) | lang-q | B (native accepts; E70) |

Totals: 14 B, 1 C, 9 D, 17 A, 7 E (48 entries; #48 is after the range above). Entries from #49 on, and #10 option (b) and #12b, come from the proposals branch, except #50 (class A), from the coherence branch, #51, from the sameneed branch, #52 to #54, from the coherence2 branch, #55, from the call-brackets branch, #56, from the eagercopy branch, and #57 to #59, from the deltafix branch.

**Points that most need a decision.**
- #10 rejected two programs that both implementations accept and run
  correctly. Resolved by option (b): a mentioned output is fresh, and
  both are accepted again.
- #20 removes a run-time check from the user's own commit b4f8279.
- #14 makes Audit accept a `tests/errors` program that both implementations
  reject.
- #12 changed the oracle as well (Elab.v, ExecutionModel.v). It rejects
  `partially-bound-merge.moss`, which native and Audit (#50) accept; the
  oracle fix is designed in #50.
- #13 rejects a control program that the oracle runs correctly.

---

## B: rejects programs some implementation accepts

### 1. Record patterns bind distinct names (08fc526)
- **Rule now.** `PRecord` requires the names a record pattern binds to be
  distinct (D34).
- **Why.** `Pair { a = y, b = y }` was typed by its last field and read
  from its first, so the reference interpreter could get stuck.
- **Tests.** [duplicate-field-binder.moss](examples/duplicate-field-binder.moss):
  - oracle (built at 386e418): accepts, then `stuck (XErr 10)`;
  - native: the executable elaborators (Elab.v and src/) accept it (E21);
    no native run is recorded;
  - Audit: rejects.
- **Corpus.** None.
- **Decision.** User-approved.
- **Class B.** Both implementations accept the program. Follow-up: E21
  (open).

### 2. A `break` in a `while` condition leaves the loop (8a023a7)
- **Rule now.** `StmtBreaks` has an `SBWhile` case, so a `break` in a
  while condition counts as leaving the enclosing loop.
- **Why.** `loop { while break {} }` was typed `never` and could still
  return.
- **Tests.** [while-condition-break.moss](examples/while-condition-break.moss):
  - oracle: accepts, then `stuck (XErr 10)`;
  - native: accepts, per E22 and the commit message; no run is recorded;
  - Audit: rejects.
- **Corpus.** None.
- **Class B.** Both implementations accept the program. Follow-up: E22
  (open).

### 3. Calls are instantiated through the callee's merges (f59b6f4, 6fd598a)
- **Rule now.** A call's or provider's instantiation θ is any `CallTheta`:
  - θ is the caller's meaning wherever the caller binds a requirement;
  - every merge of the callee holds of θ, including for requirements the
    caller leaves unbound, so an unbound member of a bound merged class
    gets the class's meaning;
  - θ covers the callee's whole context type table (for `main`, its root
    types too).
  - since #50, θ is principal: a requirement in a callee class with no
    member the caller binds is the class itself (its representative).

  `Satisfies` maps each need to the caller's key through θ (`at_th`,
  added in 6fd598a).
- **Why.** `Satisfies` skipped a merge equation unless all of its symbols
  were bound at the caller, so a caller could bind two members of one
  merged class differently.
- **Tests.**
  - [unbound-merge-representative.moss](examples/unbound-merge-representative.moss):
    - oracle (at 386e418): accepts, then `stuck (XErr 10)`;
    - native: not recorded;
    - Audit: rejects.

    Since #12 the oracle rejects it too (D62).
  - [partially-bound-merge.moss](examples/partially-bound-merge.moss): the
    oracle printed `k` before #12. Under `CallTheta` it was meant to be
    accepted (E24 says it "must be accepted"). Audit accepts it (θ Y is
    read through the bound X; recorded in #50); native prints `k`.
- **Corpus.** None.
- **Decision.** User-approved (E24).
- **Class B.** The oracle at the time accepted unbound-merge-representative.
  Follow-up: E24 (open).

### 4. Selected provisions have the callee's types (cb6ce7b)
- **Rule now.** `Satisfies` takes the call's or application's
  instantiation θ. Each selected provision must have, canonically, the
  callee's type or signature read at θ (`sig_at`), and coherence reads
  through θ too. `MostGeneralMerge` now also states that σ maps only
  generations of the telescope.
- **Why.** In stale-provision-type, `val v: T` with T not a requirement
  was provided at T = Char and read by the callee at T = Bool.
- **Tests.** [stale-provision-type.moss](examples/stale-provision-type.moss):
  - oracle: accepts, then `stuck (XErr 10)`;
  - native: not recorded;
  - Audit: rejects.
- **Corpus.** None.
- **Decision.** User-approved.
- **Class B.** The oracle accepts the program. Follow-up: E25 (open).

### 5. Bound attached methods are read at the key's receiver (465622d)
- **Rule now.** For `bind Box.m[T=Bool] = Box.get;`, `MPAttached` reads the
  provider's `Satisfies`, `CallTheta` and signature with the receiver's
  arguments set as types, as a direct attached call does.
- **Why.** Under T = Char, `Box.get`'s need `T.k` was answered by
  `Char.k`, and then called on a Bool payload.
- **Tests.** [bracket-method-receiver.moss](examples/bracket-method-receiver.moss):
  - oracle: accepts, then `stuck (XErr 14)`;
  - native: accepts and prints `1`, reading `true` as a character code;
  - Audit: rejects.
- **Corpus.** None.
- **Decision.** User-approved.
- **Class B.** Both implementations accept the program, and both
  misbehave on it. Follow-up: E26 (open).

### 6. Functor-body breaks count (ae3c360)
- **Rule now.** `Breaks`, `BlockBreaks` and `StmtBreaks` take the world and
  module. `ItemBreaks` counts `bind F;`, for F with a body, as breaking
  when an item of F's body breaks.
- **Why.** `bind F;` runs F's binds in place, so a `break` among them
  leaves the loop around it. `loop { bind G; }` was typed `never`.
- **Tests.** [functor-body-break.moss](examples/functor-body-break.moss):
  - oracle: accepts, then `stuck (XErr 14)`;
  - native: rejects;
  - Audit: rejects.

  Both implementations run the well-typed variant correctly.
- **Corpus.** None.
- **Decision.** User-approved.
- **Class B.** The oracle accepts the program. Follow-up: E28 (open).

### 7. 3b covers every captured item (43cd6d9)
- **Rule now.** LinkBody's 3b says that no item G captures (val, fn,
  method or functor) mentions an input of F that each application
  supplies, unless that input is fixed. This covers the item's type or
  signature, its recorded instantiation and a method's receiver, read from
  G's canonical provisions.
- **Why.** 3b covered only captured values, so G could call a captured fn
  at a different meaning of a linked type.
- **Tests.**
  - [captured-linked-fn.moss](examples/captured-linked-fn.moss): both
    implementations reject (D84 3b); Audit rejects.
  - [link-merge-captured-fn.moss](examples/link-merge-captured-fn.moss),
    found later (807bbbf):
    - oracle: accepts, since its 3b reads one stored signature per fn;
      then `stuck (XErr 14)`;
    - native: rejects;
    - Audit: rejects. In the orientation Y := X this rule rejects it; in
      the other, #11 does.
- **Corpus.** None for this commit.
- **Decision.** User-approved.
- **Class B**, through link-merge-captured-fn, which the oracle accepts
  and runs unsafely. Oracle bug E36.

### 8. Main's platform fns read their raw handles as themselves (1f5f71d)
- **Rule now.** `Platform.Entry`'s `IFn` case also requires `HandlesRead`.
  Each handle tag (`Ref`, `Array`, `Addr`) in the fn's signature, in main's
  context, must read its raw-handle requirement (`StructRef`, `ArrayRef`,
  `I32`) as itself.
- **Why.** `PublicEntry` needed `HandleFormed` for every fn in main's
  context. A context item could bracket the requirement away
  (`i64_store[I32 = Addr[I32 = I64]]`).
- **Tests.** [handle-rebind.moss](examples/handle-rebind.moss):
  - oracle: rejects (D20/D62 assumed-requirement coherence);
  - native: accepts, exit 0, because main never calls `i64_store`; with a
    call, native rejects;
  - Audit: rejects.
- **Corpus.** None. The record argues that with the shipped libraries this
  rejects nothing the oracle accepts.
- **Class B.** Native accepts the program (an unused assume). This
  constrains main's context, which the user's program writes, so it is not
  class E.

### 9. A family's listed polymorphic inputs are type symbols (f6ca908)
- **Rule now.** `FamilyShape` requires each name in `F[...]` to resolve to
  a type symbol (D84 rule 1).
- **Why.** `FamilyKeysTyped` needs the matching's keys to be F's inputs or
  type symbols.
- **Tests.** [family-poly-nontype.moss](examples/family-poly-nontype.moss):
  - oracle: rejects ("not a type in scope (D84)");
  - native: accepts, exit 0;
  - Audit: rejects.
- **Corpus.** None. It mirrors the oracle's `functor_params` exactly,
  restricted to families.
- **Class B.** Native accepts the program. Native bug E34.

### 10. A kept output is unmentioned by the context (642490e); a mentioned output is fresh (proposals, option (b))
- **Rule now (option (b), owner-approved).** In `BApplyAbstract` an
  output is fresh (a new generation from `newgens`) when it is in force
  *or mentioned in scope*, that is in `context_gens C ++
  key_receiver_gens C` (the proof's `capture_gens`). Only an output that
  is neither is installed as itself. The former clause, "a kept output is
  unmentioned", now holds by construction, and the case split stays
  deterministic.
- **Criterion: stability** (criteria.md section 5). Writing a mentioned T
  as itself silently re-grounds what captured it (a family method key
  `T.foo`, its signature). A fresh generation leaves those meanings alone.
- **Rule before (642490e).** An output not in force had to be unmentioned,
  else the program was rejected.
- **Why the old rule.** The premise `KeptOutputsUnmentioned` was false for
  two programs Audit accepted, which made `safety_obligations` vacuous. In
  both, an unbound declared T appears in family method keys and
  signatures, and the result's write `T |-> m` re-grounds those.
- **Tests.** Audit rejected both programs from 642490e and accepts them
  again under (b); both implementations run them correctly:
  - [lambda-declared-output.moss](examples/lambda-declared-output.moss):
    oracle exit 0; native exit 0;
  - [kept-output-family-method.moss](examples/kept-output-family-method.moss):
    oracle prints 0; native prints 0. Under (b), y has the fresh type T'
    and `y.dup()` is demanded from F at A := T'. An oracle variant that
    gives a mentioned output a fresh generation
    (`scratch/oracle/keptfresh`) prints 0 too, as does the plain oracle
    (`MODE=run` comparison: same output on both programs).

  The neighbour [kept-output-captured-fn.moss](examples/kept-output-captured-fn.moss)
  is rejected by both implementations and by Audit (unchanged).
- **Corpus.** `scratch/orch/chainfwd-cmp.out` (old rule): 376 same, 2 diff.
  Under (b), an oracle variant with the old rule (`scratch/oracle/kept`,
  chainfwd's patch ported) against the plain oracle over `tests/`,
  `examples/`, `bootstrap/examples/` and the new test programs: 412 same,
  2 diff, exactly the two programs above, which (b) now accepts.
- **Proof.** `kept_outputs_unmentioned` (PublicLinkResults) is restated
  for an output kept as itself (`gensof t = t`): a fresh one is a fresh
  symbol, never a declared one (`funoutputs_declared`). BodySlice's
  `AppPremises`/`AppAt` repeat the new clause. No other proof changed.
- **Run time.** Audit writes the fresh generation where the
  implementations write T itself. Elaborated code reads T through the
  table, so this is a class D difference in generation naming only (as
  for #18), not observable.
- **Decision.** The owner's (option (b) of consolidation proposal 1).
- **Class.** Was B (rejected two programs both implementations run);
  now no longer an incompleteness for them.

### 11. A linked input reads in G as a type over F's inputs (807bbbf)
- **Rule now.** In LinkBody's input condition, "G's reading of an input x
  of F mentions only F's inputs" used to be a premise. It is now a
  conclusion, so G's merges may not send a linked input to a type that
  only G has.
- **Why.** `CN.ChainRealizeOk` was false. With the merge oriented X := Y,
  the σ-read 3b saw only Y, and the condition held vacuously. A closure
  then installed a captured Bool at the caller's Char.
- **Tests.**
  - [link-merge-captured.moss](examples/link-merge-captured.moss): oracle
    rejects (D84 3b); native rejects ("incompatible types"); Audit now
    rejects.
  - [link-merge-captured-fn.moss](examples/link-merge-captured-fn.moss):
    oracle accepts, then `stuck (XErr 14)`; native rejects; Audit rejects
    (this rule for X := Y, #7 for Y := X).
  - Neighbours in `scratch/cap3b/` (not committed) are listed in
    `cap3b-AUDIT.md`. One of them, n7b, is rejected by the oracle and
    accepted by native, and this rule rejects it as the oracle does.
- **Corpus.** `scratch/orch/cap3b-cmp.out`: 398 same, 2 diff. The
  differences are link-merge-captured.moss and its hunt original. The
  oracle rejects both.
- **Class B.** The oracle accepts link-merge-captured-fn.moss and runs it
  unsafely (E36). Native accepts n7b (scratch only).

### 12. A method's θ records every type symbol its signature mentions (61040f2, "sigclose")
- **Rule now.** A method provision's instantiation (`AppOf`) lists two
  things: first the requirements its signature mentions, in their fixed
  order; then every other type symbol the signature mentions, in ascending
  order (`SigOutside`, `Ascending`). `ResultMentions`' `RMMeth` counts every
  such symbol in M(F).
- **This commit also changed the oracle.** `ExecutionModel.v` changed the
  same way, and `Elab.signature_type_syms` appends the sorted remainder.
- **Why.** `S.FamilyCoherentOk` was false. With `fn .m(): U` under
  `assume A`, two provisions of `Int.m` read at U = Int and at U = Bool
  shared θ `[]` and so one run-time key.
- **Tests.**
  - [sig-nonreq-shadow.moss](examples/sig-nonreq-shadow.moss): the old
    oracle was `stuck (XErr 14)`; the new oracle prints 42; native prints
    42.
  - The corpus differences below.
- **Corpus.** `scratch/orch/sigclose-cmp.out` compares the old oracle with
  the new one over full runs: 381 same, 3 diff.
  - sig-nonreq-shadow: stuck, then 42.
  - unbound-merge-representative: stuck (XErr 10), then rejected (D62).
  - partially-bound-merge: printed `k`, then rejected (D62), because the
    caller leaves Y unbound. Adding `bind Y = Char;` makes it accepted
    again. Native still accepts the original. Native's analogue of the D62
    rejection was not checked (sigclose-STATUS.md).
- **Decision.** The user's (D20: a signature is closed over its
  requirements).
- **Class B.** It rejects partially-bound-merge.moss, which native accepts
  and the old oracle ran. It also changes run-time keys (a D aspect). E35
  is done.

- **#12b: M(F) counts every type symbol a fn's signature mentions
  (proposals, owner-approved).** `ResultMentions`' `RMValFn` now also
  counts, for a fn, every type symbol its signature mentions
  (`SigMentions`, as `RMMeth` does for a method). A fn's θ is *not*
  widened: its key `IFn f` carries no θ, and `Satisfies` already compares
  the signature at θ (`sig_at`).
  - **Why.** With `RMValFn` on `w_reqs` only, a functor whose result lists
    `fn mk(): U` (U outside mk's assume nesting) had no U among its inputs,
    so its result could not be read in its own context (ProvideResult's
    `SigOf` fails) and it was unusable. The oracle had the same asymmetry.
  - **Criterion: stability** (criteria.md section 5): U becomes a fixed
    input X(F), so it keeps the meaning it had where F was bound (rule 5),
    as native does.
  - **Oracle changed too.** `Elab.result_mentions_go` lists a fn's
    requirements followed by `signature_type_syms` (the same sorted
    remainder as for a method). The oracle-soundness proofs follow:
    `ResultMentionExec`/`ResultMentionsCover` restate the visitor,
    `here_valfn` (cover) and `here_fn_sound` (exact) handle the new symbols;
    PublicWorldFormation and XBridge take the new disjunct. `ExecutionModel.v`'s copy
    of `ResultMentions` changes alike.
  - **Native first.** Native already treats such a U as a fixed input of F:
    [fn-sig-nonreq.moss](examples/fn-sig-nonreq.moss) prints 7, and
    [fn-sig-nonreq-rebind.moss](examples/fn-sig-nonreq-rebind.moss) (U
    rebound before `bind F;`) is rejected ("the available binding was
    provided at a different type instantiation: `F`").
  - **Tests.**

    | program | oracle before | oracle now | native | Audit now |
    |---|---|---|---|---|
    | [fn-sig-nonreq.moss](examples/fn-sig-nonreq.moss) | rejects (U not available) | 7 | 7 | accepts (reading) |
    | [fn-sig-nonreq-rebind.moss](examples/fn-sig-nonreq-rebind.moss) | rejects (U not available) | rejects (rule 5) | rejects | rejects (rule 5) |

  - **Corpus.** The new oracle against the old over `tests/`, `examples/`,
    `bootstrap/examples/` and the new programs (elaboration verdicts): 412
    same, 2 diff, exactly the two programs above. Full runs (stdout,
    stderr, exit status): also 412 same, 2 diff, the same two.
  - **Class.** A relaxation toward native (Audit and the oracle accepted
    no such program before).

### 13. A linked body does not keep an output at an input of F (268bf3a, 4d55710)
- **Rule now.** `LinkBody` requires that nothing the body makes is an
  input of F (`forall t, In t made -> nlook t (c_ty Cu) = None`, 4d55710).
- **History.**
  - 268bf3a: `BApplyAbstract`'s kept-output case also required
    `~ In t (fr_pins F)`.
  - 4d55710 added the made clause, which implies the pin clause for a
    linked body's own binds. The pin clause was kept because it had been
    approved.
  - **Removed in consolidation (ob-consol, "Drop BApplyAbstract's pin
    clause").** Outside the body's own binds the pin clause was stricter
    than both implementations: a block nested in a linked body keeps its
    `made` to itself, but the frame's pins still reached it.
    [block-kept-output-pin.moss](examples/block-kept-output-pin.moss) was
    rejected by Audit only; the oracle and native both print 7. Audit now
    accepts it. This is a relaxation toward both implementations.
- **Why.** `CN.ChainRealizeOk` was false. G's `bind Make;` installed Make's
  output T (Char) as itself, where T is F's input, and G bound `T.m` on
  it. The caller, with T = Bool, filed it under `Bool.m`. The pins M(F)
  (D84 3d) stopped only `bind T`. The pin clause alone missed a kept
  output at an unmentioned merge representative.
- **Tests.**

  | program | oracle | native | Audit now |
  |---|---|---|---|
  | [link-kept-output-pin.moss](examples/link-kept-output-pin.moss) | accepts, then `stuck (XErr 14)` (E37) | rejects ("compilation failed: `T`") | rejects |
  | [link-kept-output-pin-control.moss](examples/link-kept-output-pin-control.moss) (caller's T = Char) | prints 48, 10 | rejects | rejects |
  | [link-kept-output-rep.moss](examples/link-kept-output-rep.moss) | rejects ("does not provide `X.m`") | rejects | rejects. Before: accepted for a world picking T, by hand reading |

- **Corpus.** `scratch/orch/realize2-cmp.out` compares against a variant
  whose `check_link_pins` and `check_link_inputs` reject a pin, or an input
  of F, that the body installs as itself: 401 same, 5 diff. All five are
  the new programs (the two examples and three `scratch/realize2` copies).
- **Class B.** The oracle runs the control correctly, and Audit and
  native reject it. The oracle also accepts link-kept-output-pin and runs
  it unsafely (E37).

## C: accepts programs an implementation rejects

### 14. `link_site`: a linked body's result keys read inputs at G's table (94d89ce, fc75de7)
- **Rule now.** `Audit.link_site Cg Cb outs gen` is the context at which a
  linked body's result keys and promised types are read. It is used in
  `ResultRealizes` and in the code's sources. Its table has two parts:
  - each output role `gen t` is read at the body's final table;
  - every symbol is read at G's own table, including an input occurrence
    of a symbol that is also an output.

  The keys keep the roles, which the site's table alone reads. (Until
  consolidation, LinkBody also applied `link_renaming`, the identity on
  the roles, to each key; it is gone.) Merges, provisions and locals are
  Cb's.
- **History.**
  - 94d89ce introduced `link_site`. It read the output *symbols* at Cb.
  - fc75de7 made only the *roles* read at Cb.
- **Why.**
  - 94d89ce (linknewpf): a body could rebind an unpinned merge
    representative of a mentioned input. `at_site Cb` then followed the
    rebind, while the input's meaning at an application stays the
    caller's.
  - fc75de7 (realize3): the input T of an F whose T is also an output was
    read at the body's rebinding.
- **Tests.**

  | program | oracle | native | Audit now |
  |---|---|---|---|
  | [link-merge-rep-rebind.moss](examples/link-merge-rep-rebind.moss) | rejects ("does not bind `X.m`") | rejects | rejects (before: accepted, then stuck) |
  | [link-merge-rep-rebind-both.moss](examples/link-merge-rep-rebind-both.moss) (= m5-XT) | rejects in this order (D84 3d); accepts the other order and exits 0 | accepts both orders, exit 0 | accepts; selects G's `X.m` |
  | [tests/errors/functor_output_input_changed.moss](../tests/errors/functor_output_input_changed.moss) | rejects (D84 3d) | rejects | accepted after 94d89ce (rejected before). The record argues this is sound. Still accepted after fc75de7, by a consolidation reading |
  | [link-output-input-rep.moss](examples/link-output-input-rep.moss) | rejects | rejects | rejects (before fc75de7: accepted, by hand reading) |
  | [link-output-input-rebind-control.moss](examples/link-output-input-rebind-control.moss) | prints 7 | prints 7 | accepts |
  | [output-merge-key.moss](examples/output-merge-key.moss) | accepts | accepts, prints 7 | accepted after fc75de7 (the old rule rejected it) |

- **Corpus.**
  - 94d89ce: `scratch/linknewpf/cmp.out`, against a variant whose pins
    leave out the receiver generations: 426 files, 423 same, 3 diff
    (link-merge-rep-rebind-both, m5-XT, functor_output_input_changed).
  - fc75de7: `scratch/orch/realize3-cmp.out`, against a variant reading as
    the old rule: 414 same, 1 diff (output-merge-key, which the oracle
    accepts).
- **Decision.** The coordinator chose the faithful option (b) over pinning
  merge representatives as the oracle does. That option would reject
  m5-XT, which native runs.
- **Class C.** After 94d89ce, Audit accepts
  `functor_output_input_changed.moss`, which both implementations reject.
  It also accepts m5-XT, which the oracle rejects and native accepts.
  After fc75de7 the first still holds, by a consolidation reading (not
  run): Target's result has no type item, so `T.out` is not a role and
  reads at G's table (see "Open points found in consolidation").

## D: run-time semantics changes

### 15. Lambda captures keep merge keys (a5b081c)
- **Rule now.** `context_gens` also lists each merge key and the
  generations of its value. Lambda capture and freshness share it, so a
  lambda's closure keeps the meaning of a key that the context merged
  away, and fresh generations avoid merge keys.
- **Why.** Merge absorption failed at a merged-away key in the closure.
  The record says this is unobservable.
- **Tests.** None. The record notes there is no counterexample.
- **Decision.** User-approved.
- **Class D.** Run-time capture changes. It is unobservable, and no
  program is rejected (freshness is an existential witness).

### 16. A wrapper calls its fn at the fn's own context (77de8be)
- **Rule now.** `MPWrapped`'s wrapper calls the fn it wraps at
  `theta_ctx Cg`, indexed by that fn's context type table, not at
  `theta_at` (requirements only).
- **Why.** A fn in scope as `main` also has its root context's types,
  which the wrapper left unbound.
- **Tests.** [wrapped-main.moss](examples/wrapped-main.moss) with
  [wrapped-main-root.moss](examples/wrapped-main-root.moss): both
  implementations run it correctly (expected: 2, twice).
- **Decision.** User-approved.
- **Class D.** Audit's wrapper semantics changes, toward what the
  implementations do. Follow-up: E27 (open).

### 17. Only linked inputs come from the application's θ (de53d1d)
- **Rule now.** `functor_code` records `fc_linked`: the inputs of the
  functor the body was bound to (all inputs, for a platform family). Only
  those inputs are read from θ at application. Others keep their captured
  meaning, as the oracle's `runtime_provider_inputs` does.
- **Why.** A copy whose target takes an input that the body captured
  replaced the captured type.
- **Tests.** [copy-capture.moss](examples/copy-capture.moss):
  - oracle and native: keep the captured X = I32, and the rebuild traps
    (cast failure);
  - old Audit: stuck (an `f64_add` on an i32);
  - Audit now: matches the implementations.
- **Corpus.** None.
- **Class D.** Audit's run-time semantics now match both implementations.

### 18. A functor body's inputs are read through G's table (206d739)
- **Rule now.** `functor_code` gains `fc_table`. `application_environment`
  reads an input x that the table maps to t as t over the inputs' base
  meanings, so a merged-away input reads as its representative.
  - `LinkBody` records G's canonical table.
  - Families record `[]`, which leaves their behaviour unchanged by
    computation.
- **Why.** `LinkMergesOk` and `BindMergesOk` were false on programs that
  all three accept. The body's environment did not absorb G's merge.
- **Tests.** Audit, the oracle and native all accept and print 7 on both:
  - [linked-merge-captured.moss](examples/linked-merge-captured.moss);
  - [bind-merge-unbound.moss](examples/bind-merge-unbound.moss).
- **Corpus.** None. No premise changed, so nothing is rejected.
- **Class D.** A run-time change. The record argues that elaborated code
  cannot observe it, since it reads only canonical types. Two obligations
  were deleted as a result.

### 19. No refile in a linked body; results are read from their source keys (870db77)
- **Rule now.** A linked body runs only its own binds (`binds0`). The
  application reads each result key from the body key the item was left
  under (`fc_sources`). #23 later changed `fc_sources`' shape, but kept
  this behaviour.
- **Why.** `RefileRunOk` was false. Refiling F's key right after the bind
  overwrote a stale provision that the body's context still listed.
- **Tests.**
  - [refile-stale-shadow.moss](examples/refile-stale-shadow.moss): oracle
    `stuck (XErr 14)` (8 with `() => 8`); native prints 7; Audit now
    behaves as native does.
  - `scratch/refile/v4.moss`: prints 6 on both implementations.
- **Corpus.** None. No premise was added, so nothing is rejected.
- **Decision.** The coordinator chose native's behaviour.
- **Class D.** Audit now matches native and differs from the oracle.
  Oracle bug E31.

### 20. Functor results shadow an occupied destination (712aecd), removing b4f8279's check
- **Rule now.** `result_environment` no longer returns Stuck when an
  output's destination already has a meaning in the caller. The writes are
  prepended and shadow it, as the oracle's `dr ++ d` does.
  `fresh_result_types` stays defined as a statement of the static
  freshness. The safety argument moved into the proof (`dest_scope`).
- **This removes a run-time check that the user added in b4f8279**
  ("Prove public loader/world boundaries and preserve type generations").
- **Why.** Audit accepted programs that reached the check, and its run was
  Stuck there. No `Pre` invariant can rule this out.
- **Tests.**
  - [const-kept-output.moss](examples/const-kept-output.moss): oracle exit
    0; native rejects the use of the val for an unrelated reason (it
    rejects every use of a defined val); old Audit stuck.
  - [lambda-declared-output.moss](examples/lambda-declared-output.moss):
    oracle exit 0; native exit 0; old Audit stuck. #10 rejected it until
    its option (b), which accepts it.

  The b4f8279 examples (saved-type-rebinding, saved-generic-rebinding,
  captured-output-scope, captured-type-input) do not mention the check.
  Its Coq examples were restated from "rejected" to "shadowed".
- **Corpus.** None.
- **Decision.** Coordinator-approved (option A). The rejected alternative
  was to keep the check and change const evaluation.
- **Class D.** Audit now matches the oracle.

### 21. Lambda capture keeps method-key receivers' meanings (fb198f2)
- **Rule now.** `lexical_capture C` maps
  `context_gens C ++ key_receiver_gens C` to themselves. The added part is
  the generations of every method key's receiver. `context_gens`, and so
  `FreshFor`, is unchanged.
- **Why.** A family demand can key a method at an unbound declared T. A
  lambda dropped T's meaning but kept the key, and inlined const code
  could make the two groundings differ.
- **Tests.**

  | program | oracle | native |
  |---|---|---|
  | [family-unbound-need.moss](examples/family-unbound-need.moss) | 0 | 0 |
  | [family-unbound-need-sig.moss](examples/family-unbound-need-sig.moss) | 0 | 0 |
  | [lambda-declared-output.moss](examples/lambda-declared-output.moss) | 0 | 0 |
  | [const-family-unbound.moss](examples/const-family-unbound.moss) | 0 | rejects (native rejects every defined-val use) |

- **Corpus.** None. The change is run-time only.
- **Decision.** Coordinator-approved (option c).
- **Class D.** Mirrors the oracle's `xclose`, which captures the whole
  type environment.

### 22. Functor closures capture types through `lexical_capture` (fb41ce0)
- **Rule now.** `BFunctor` carries a `type_capture`, and `BFunClosure`
  gives it `lexical_capture C`, as lambdas have. It used to capture all of
  `d`'s meanings and aliases.
- **Why.** `CapturedInForceOk` was false once declared symbols were
  admitted. With all of `d`, the captured reading of an input not in force
  was its alias, which in inlined const code need not equal its meaning.
  The obligation was restated (lead-approved), proved, and removed from
  `safety_obligations`.
- **Tests.** [functor-captured-unbound.moss](examples/functor-captured-unbound.moss):
  oracle exit 0; native exit 0.
- **Corpus.** None. The change is run-time only.
- **Class D.** The record argues the reading is unchanged for symbols in
  force.

### 23. An application installs exactly F's declared result under the caller's keys (e029263, dbd51d0, 0e211e6, cff9949, baf6ea6)
- **Rule now (baf6ea6).** An application files position i of F's declared
  result under the caller's key for it.
  - That key comes from `BApply`'s new field, `map (rkey_of C') ks`.
  - The value is read from the root body's key for that position
    (`fc_sources : list item`), through each copy's position map
    (`BCopyFunctor`'s new field, `FunctorClosure`'s `rms`,
    `chain_sources`; since #55, composed into one map at the copy,
    `closure_sources`).
  - The selection is `result_selection`. A later position wins, matching
    the caller's static last setter.
  - `CopyOutputs` records, for each of F's positions, the last position of
    H's result with the same provision id.
- **What happened on the way.**
  1. 0e211e6 (results4) added a converse to `CopyOutputs`: every key of
     H's result must be one of F's. It rejected
     [copy-extra-key.moss](examples/copy-extra-key.moss), which native
     runs (31) and the oracle runs wrongly (21). Corpus with that converse:
     373 same, per the record.
  2. e029263 added a LinkBody premise: no output symbol appears in a
     result key as an input. Its corpus check
     (`scratch/orch/results4-cmp-E33.out`) gave 375 same, 2 diff. One
     difference is the project regression
     [functor_input_output_generations.moss](../tests/regressions/functor_input_output_generations.moss),
     which native runs (`yn`).
  3. dbd51d0 reverted e029263 (the coordinator's decision).
  4. cff9949 (roles) kept output roles in the code's keys (`fc_roles`)
     and mapped them at the application.
  5. baf6ea6 (callerkeys) replaced roles. It removed `fc_roles`,
     `result_smap`, `inst_smap`, `role_outputs` and
     `output_destination`, and reverted 0e211e6's converse.
- **Why.** Three obligations were false because the root's keys were read
  in the caller's terms:
  - `ChainForwardCopyKeys`;
  - `ChainKeysOk`;
  - the key half of `FamilyCopyApplyOk`.

  Also, `ChainNewOk` was false for an extra key of a copy's source.
- **Tests.**

  | program | oracle | native | Audit now |
  |---|---|---|---|
  | [copy-extra-key.moss](examples/copy-extra-key.moss) | prints 21 (E32) | prints 31 | accepted; 31 |
  | [linked-copy-bracket.moss](examples/linked-copy-bracket.moss) | `stuck (XErr 6)` | prints 42 | the call finds the primitive |
  | [family-copy-linked-bracket.moss](examples/family-copy-linked-bracket.moss) | `stuck (XErr 6)`, exit 3 | exit 0 | the demand finds the key |
  | [output-merge-key.moss](examples/output-merge-key.moss) | `stuck (XErr 6)` (E33) | prints 7 | as native |
  | [functor_input_output_generations.moss](../tests/regressions/functor_input_output_generations.moss) | `n`, then stuck | `yn` | as native |

- **Corpus.** For the copy check's position matching,
  `scratch/orch/callerkeys2-cmp.out` gives 398 same, 0 diff.
- **Class D.** Audit now matches native and differs from the oracle
  (E32 and E33 are open). The final copy check accepts what the oracle
  accepts on the corpus.

## A: reject-only, mirroring both implementations

### 24. The receiver slot is reserved (c75eb2e)
- **Rule now.** When there is a receiver, `Numbering` numbers no name 0,
  so a local cannot shadow `this`.
- **Why.** `ExThis` reads local 0. The lexer already makes `this` a
  keyword.
- **Tests.** None. No parsed program is affected.
- **Class A.**

### 25. `else` branches are checked against the expected type (33dd6b5)
- **Rule now.** `ExIfElse` requires `opt_fits et exp`.
- **Why.** `JoinExp` adopted the expected type without checking an else
  branch that is neither a block nor an if. The parser produces only
  those two.
- **Tests.** None. No parsed program is affected.
- **Class A.**

### 26. The world records each symbol's context (37d235f)
- **Rule now.** `w_context` records each fn, method and functor symbol's
  context and needs, and `CtxOf` and `FunCtx` require them. Merging stays
  symmetric: any consistent orientation is allowed.
- **Why.** Independent orientation choices at a body and at its call sites
  could disagree on keys (`e_entry`). This was a gap in the relation, not
  in the oracle.
- **Tests.** None. [merge_order.moss](../tests/wasi/merge_order.moss)
  illustrates the two orientations.
- **Decision.** User-approved.
- **Class A.** It constrains derivations, not programs. Follow-up: E23
  (open).

### 27. Attached methods need a nominal receiver (ee7fd05)
- **Rule now.** `FnElab` requires an attached method's receiver to be a
  tag or unit declaration (Q5).
- **Why.** A method attached to an abstract type would see `this` as a
  nominal type that its callers do not pass.
- **Tests.** [tests/errors/attached_on_abstract.moss](../tests/errors/attached_on_abstract.moss):
  the implementation rejects it ("requires a nominal receiver (Q5)").
- **Class A.**

### 28. Captured vals are new symbols (c6f1b9d)
- **Rule now.** Each val that `LambdaProvider` captures must be unprovided
  in the current context.
- **Why.** Precise freshness for the realization invariant. The record
  says it is unobservable.
- **Tests.** None. Both implementations already allocate new symbols.
- **Decision.** User-approved.
- **Class A.**

### 29. A lambda's body frame keeps the generations in scope (e0d6f9c)
- **Rule now.** `LambdaProvider` requires the body frame's `fr_fresh` to
  include the enclosing frame's and `context_gens C`.
- **Why.** An application in the body could make a captured generation
  anew and find it occupied at run time.
- **Tests.** None. Both implementations draw generations from a global
  counter.
- **Decision.** User-approved. Listed under "Later cleanup" in
  SAFETY-SITES.
- **Class A.** It constrains derivations, not programs.

### 30. A functor closure's captured needs satisfy G; functor bodies number their locals (64e2a92)
- **Rule now.** In `bind F = G;`, the needs G captures must satisfy G's
  context as a call's needs do (`Satisfies`, at G's inputs read here).
  `LinkBody` also requires `Numbering Fg None []`.
- **Why.** G's merges could be violated by the captured needs.
- **Tests.** [captured-merge.moss](examples/captured-merge.moss): both
  implementations reject (the oracle by D84 3b); Audit rejects. The
  numbering clause has no test.
- **Decision.** User-approved.
- **Class A.** Follow-up: E30 (open).

### 31. A family's matching is read off the demanded key (a12f7ab)
- **Rule now.** `FamilyReadOff`, a premise of `PrFamily`, `FamilyStep` and
  `LitOpFamily`, says three things:
  - θ's domain is exactly the P(F) symbols that the formal receiver
    mentions;
  - there are no duplicates;
  - `canon C (app_subst θ rf)` is the key's receiver.
- **Why.** Matching values were free beyond what the key determines.
- **Tests.** None. The record argues it mirrors the oracle's
  `family_candidate_step` and rejects nothing the implementations accept.
- **Class A** (argued).
- **Since proposals item 6: the member is unique.** A fourth conjunct asks
  that every formal key with method m whose receiver matches the demanded
  receiver (same domain and read-off conditions) give the same matching,
  compared up to the merges in force (`option_map (canon C)` of each
  lookup). A key matching two items with different matchings is
  ambiguous and has no family read-off, so no derivation picks one.
  - **Criterion: coherence** (criteria.md section 2, which names this case):
    before, a derivation could pick either item, so the demand's θ, the
    provision and its run-time key were not determined.
  - **Mirrors** the oracle's `family_member` (Elab.v), which accepts a
    repeat with equal θ and otherwise fails ("... is two items of the
    family ... ambiguous (D84 rule 7)").
  - **Native first:** native rejects such a program too ("conflicting
    method bindings from a family: `m`").
  - **Test.** [family-ambiguous-member.moss](examples/family-ambiguous-member.moss):
    `F[A, B]: Std -> A.m, B.m` with A and B in force and the demand `k.m()`
    at Int. Oracle rejects (ambiguous); native rejects; Audit now rejects
    (before, a derivation could take either G's A.m, printing 1, or B.m,
    printing 2). An oracle variant without the ambiguity check prints 1.
  - **Corpus.** That variant (`scratch/oracle/noambig`) against the oracle:
    412 same, 2 diff: the new example (accepted by the variant) and
    tests/wasi/family_ambiguous.moss, rejected by both (the variant for a
    different reason: B is not in force). So in the corpus only the new
    example changes verdict.
  - **Proof.** The proofs use one read-off per demand; the four
    destructurings take the extra conjunct.
  - **Class A** (mirrors both implementations).

### 32. Linked bodies and copies read F's needs as F does (d458480)
- **Rule now.**
  - `LinkBody` requires each linked need's provision in G's context to
    equal F's, and G's readings of F's inputs to hold in F's context.
  - `BFunCopy` requires the same of the source's needs and inputs.
- **Why.** G's or H's own merges could read a linked need differently.
- **Tests.** Both programs are rejected by the oracle
  (`defined_link_inputs_compatible`, `copy_output_compatible`) and by
  native (type errors):
  - [linked-merge.moss](examples/linked-merge.moss);
  - [copy-merge.moss](examples/copy-merge.moss).
- **Corpus.** A variant with these checks gives the oracle's verdict on all
  353 programs. The record does not name the output file.
- **Class A.**

### 33. A family member is not bound directly (1cd1ce5)
- **Rule now.** `BMethBind` requires that no family in force produces the
  bound key (D84 rule 7).
- **Why.** Otherwise a family application could shadow a directly bound
  provision.
- **Tests.** No example file. The oracle rejects such a bind ("cannot be
  bound directly"), and native has the test `family_direct`.
- **Class A.**

### 34. `MostGeneralMerge`: σ is a function (f17fb86)
- **Rule now.** `In (g, t) σ -> lookup_subst g σ = Some t`.
- **Why.** A shadowed, unformed second pair made `MergeFormed` false by
  definition.
- **Tests.** None (the witness is at definition level). The elaborator's
  union-find σ is a function, and deduplicating σ keeps `app_subst` the
  same.
- **Class A.** Nothing is rejected.

### 35. `CopyOutputs`: a copy's result reads as its source's (0a96d3f, ad3417e, a8ef896)
- **Rule now.** For `bind F = H;`, `CopyOutputs` reads H's result in F's
  context with the same fresh output roles. For each of F's keys, it
  matches the last position of H's result with the same provision id
  (positions from #23). It then compares their provisions after
  `sigma ++ [y := cg y]`, where:
  - `sigma` is F's fixed inputs read at the copy site;
  - each `cg y` is a fresh symbol for a generation of F's input readings
    that sigma does not read (ad3417e).

  The `cg y` and the output roles must avoid the generations of sigma's
  image (a8ef896).
- **Why.**
  - 0a96d3f: leaf-only comparison let a reordered result type the caller
    wrongly.
  - ad3417e: sigma could identify the site's meaning of a symbol with a
    non-fixed input of F.
  - a8ef896: `cg y` could collide with a fresh generation in sigma's
    image.
- **Tests.** Every program below is rejected by the oracle (D78), by
  native, and now by Audit:
  - [copy-output-order.moss](examples/copy-output-order.moss): native's
    test is `copied_functors_compare_final_output_interpretations`;
  - [copy-bracket-nonfixed.moss](examples/copy-bracket-nonfixed.moss);
  - [copy-fixed-reads-poly.moss](examples/copy-fixed-reads-poly.moss):
    native rejects at the use of F;
  - [copy-fresh-collide.moss](examples/copy-fresh-collide.moss).

  The *role* half of a8ef896 is untested. The proof
  (`PublicCopyFresh.copy_outputs_at`) needs it.
- **Corpus.**
  - 0a96d3f: no mechanized comparison ("not done"; copies in the corpus
    were inspected).
  - ad3417e: `copyfresh-cmp.out`, 389 same, 4 diff (all hole witnesses).
  - a8ef896: `realizepf-cmp.out`, 397 same, 1 diff (the new example).
- **Class A.** A known residual, not introduced here, is listed under
  "Known divergences".

### 36. A copy's source takes no input its target lacks (a447443)
- **Rule now.** `BFunCopy` requires every key of H's type table to be a
  key of F's.
- **Why.** An application supplies θ only for F's inputs. `incl nsh nsu`
  does not cover implicit inputs.
- **Tests.** None. The record argues it mirrors the oracle's
  `copy_output_compatible` (`| None => false`) and that the corpus copies
  satisfy it. No variant was run.
- **Class A** (argued).

### 37. Every output destination is made (914cd3c; superseded 3afe6ea, b2c4a90)
- **Rule now.**
  - `BApplyAbstract`'s `made` is `map gensof outs`: every output's
    destination, including outputs installed as themselves.
  - An output installed as itself must not occur in the frame's return or
    receiver types.
  - #10 and #13 later added more clauses to this case.
- **What happened.** Two other branches made overlapping fixes, and both
  were dropped when merged with `safety`:
  - 3afe6ea (`MadeFor`) was dropped in merge 5f56f1d;
  - b2c4a90 (`made` plus `unforced`) was dropped in merge 1d4f5c5. Its
    example `block-output-escape.moss` is not at HEAD.

  The header of [kept-output-escape.moss](examples/kept-output-escape.moss)
  still says "MadeFor".
- **Why.** A value of an output's type could leave its block (D78), and
  `MeaningsGrow` was false.
- **Tests.** Both programs are rejected by the oracle ("a type `bind F;`
  made in it ... (D78)"), by native ("cannot escape its block") and by
  Audit:
  - [output-escape.moss](examples/output-escape.moss);
  - [kept-output-escape.moss](examples/kept-output-escape.moss).
- **Corpus.**
  - 914cd3c: none ("I did not run an oracle-variant comparison"). The
    frame premise is argued.
  - 3afe6ea's variant reportedly matched on 356 programs, but
    `scratch/orch/results/cmp.out` contains only `FINISHED`.
- **Class A** (the frame premise is argued).

### 38. `ResultRealizes` compares at the body's output meanings (671b6a5)
- **Rule now.** F's promised provisions are compared with each output
  role read through the body's type table (`role_reads`; before
  consolidation, `read_rename` at the identity `link_renaming`, an
  equivalent form, `PublicConsol.result_realizes_restated`), as the
  oracle's `promised_result_type` does.
- **Why.** A body that binds `T = Bool` and provides `mk(): T` could never
  match, so Audit rejected the standard functor pattern.
- **Tests.** [result-output-meaning.moss](examples/result-output-meaning.moss):
  both implementations print 1; Audit had rejected it and now accepts it.
- **Class A, as the closest fit.** It mirrors both implementations, but it
  is a relaxation, not a rejection. It accepts nothing known that an
  implementation rejects.

### 39. Main reads a platform functor's fixed machine types as themselves (be77015)
- **Rule now.** `Platform.Entry`'s `IFun` case requires
  `FixedMachinesRead`: each machine type among the functor's fixed
  readings in main's context reads canonically as itself.
- **Why.** The proof needed this, as the premise `MainMachinesUnmerged`.
- **Tests.** [platform-fixed-merge.moss](examples/platform-fixed-merge.moss):
  - oracle: rejects ("incompatible assumed fixed type",
    `check_platform_entry_view`);
  - native: rejects;
  - Audit: rejects.
- **Class A.**

### 40. A copy's target is abstract (cd1108d)
- **Rule now.** `BFunCopy` requires F to be declared without a body.
- **Why.** `ChainForwardCopySigs` needs every symbol of a copy's result to
  be an input, which holds only for an abstract functor.
- **Tests.** [copy-into-body.moss](examples/copy-into-body.moss): the
  oracle rejects (exit 64) and native rejects (exit 1). The control
  (scratch) prints 7 on both.
- **Class A.** It is exactly the oracle's `bind_functor` check.

## E: platform-world conditions only

None of these constrain user programs. The oracle provides platform
families only for lib/wasm.moss's Heap, Mutable, Arrays and Memories, and
each record argues those four satisfy the condition. There are no
executable tests unless noted.

### 41. `ShapeNever` for never-returning primitives (44d5cfc)
- **Rule now.** A new shape, `ShapeNever`, corresponds to `|`, and
  `prim_sig PUnreachable` is `([], ShapeNever)`.
- **Why.** The interface laws let `unreachable` return `()`.
- **Follow-up.** E29 (open).
- **Class E.**

### 42. A platform family files its items under run-time keys (b39e1d5)
- **Rule now.** The family code's outputs and bindings use
  `rkey_of outputs key`.
- **Why.** `PlatformApplyOk` was false: `r.deref()` found no provision.
- **Tests.** [platform-family-keys.moss](examples/platform-family-keys.moss):
  oracle exit 0; native exit 0. This mirrors the oracle's `key_item_at`.
- **Class E.**

### 43. A platform family has no type output (668565c)
- **Rule now.** `Platform.Family` requires no `FunOutputs`.
- **Why.** The family code cannot return a type, so `read_result_types` is
  stuck.
- **Evidence.** The oracle refuses non-method items.
- **Class E.**

### 44. A platform family's machine types are fixed inputs (f152c5b)
- **Rule now.** `MachinesFixed`: every machine type a family item's
  signature mentions is a fixed input of the functor and in its table.
- **Why.** This replaces the premise `PlatformSigsFixed`.
- **Class E.**

### 45. A platform family's items read their raw handles as themselves (f9d33bf)
- **Rule now.** `HandlesRead` has a method case, required of each family
  item.
- **Why.** This replaces the premise `PlatformHandlesFormed`.
- **Class E.**

### 46. A platform family's items have distinct methods (3544241)
- **Rule now.** `MethodsDistinct`: the items' methods are pairwise
  distinct, and at most one is detached.
- **Why.** `ChainResultAgrees`, clause B of `ItemsAgree`.
- **Decision.** The lead chose this over a two-way `CopyOutputs`.
- **Class E.**

### 47. A platform family is abstract, with distinct keys (cf21df4)
- **Rule now.** The family's declaration has no body, and
  `NoDup (map provision_id keys)`.
- **Why.** This discharges the premises `PlatformAbstract` and
  `PlatformKeysDistinct`.
- **Decision.** The coordinator chose to fold them into the world.
- **Class E.**

### 48. No expanding cycle (branch ob-expand, after this document's range)
- **Rule now.** `ElaboratesIn` ends with `NoExpandingCycle W P`, the
  planned resolution of D16's polymorphic-recursion exception
  (Kennedy and Pierce's expansive cycles, decided before specialization).
  It has two halves, each a level function that no edge raises and every
  strict edge lowers, which exists iff no cycle has a strict edge:
  - `InstantiationsBounded P`: vertices are (code node, type symbol),
    nodes being P's fns (lambdas, forwarders and method providers
    included) and core functor bodies. Edges come from `ECall θ` (plain
    calls, bracket applications, attached methods at the receiver's
    instantiation), from closures (`bind f = g`, `bind R.m = g`, lambdas,
    `bind F = G`) at their captured types, and from `BApply θ` (`bind F;`
    and family members demanded on demand) to every functor body's linked
    inputs. Within a node a symbol reads as itself, through a functor
    body's table, or, for an application's output, as strictly containing
    everything its θ mentions (the owner's choice (a)). Type binds in
    force count through the canonical θ and captures.
  - `TypesBounded W`: a tag's payload, read at its own requirements,
    gives an edge to every nominal instance it mentions, behind `Ref` too.
- **Why.** The planned monomorphization theorem needs finitely many
  instantiations; a falsification hunt (scratch/expand/mono) found Audit
  accepting polymorphic recursion through every channel above.
- **Tests.** bootstrap/examples/expand-*.moss. Rejected: polyrec,
  attached, functor-output, functor-double, family, lambda, fn-bind,
  type-direct (also D58 in the oracle), type-ref-rec, type-ref,
  functor-forge. Controls: control-loop is accepted; control-functor-const
  is **rejected** by choice (a), though native compiles it and prints 5.
  - oracle: accepts and runs all but type-direct (D58);
  - native: crashes at compile time on every growing one (wasm call
    stack exhausted; out-of-bounds memory for functor-double), including
    type-ref, which has no recursive fn.
- **Corpus.** An oracle variant with the check (scratch/expand/variant;
  scratch/expand/expand-cmp.sh): 395 same / 0 diff over `tests/`,
  `examples/`, `bootstrap/examples/` (before the expand examples), and it
  accepts `src/main.moss`, the self-hosted compiler.
- **Proof.** `public_safety` unchanged; PublicBoolTyping and
  AuditBuildExample destructure one more conjunct.
- **Class B.** Native crashes on everything it rejects except
  control-functor-const, which it compiles. Follow-up: E38 (open; the
  oracle needs the check for E11).

### 49. Pins constrain only a linked body's own binds (proposals, item 3)
- **Rule now.** `BlkECheck` checks a nested block in `without_pins F`, a
  frame with no pins. The pins M(F) (D84 3d, `BTypeBind`'s `~ In t
  (fr_pins F)`) therefore constrain only the linked body's own binds
  (LinkBody's `BindItems`, with `bind G;` bodies it inlines), as before,
  and no longer the binds of a block nested in the body. Lambda bodies were
  already unconstrained (LambdaProvider leaves their frame's pins free), so
  blocks and lambdas are now treated alike.
- **Why.** Only the body's own binds can reach its result (P2 of the
  consolidation spec), and a block's binds stay in the block. The old
  scope was incoherent: the same rebind was rejected in a block and
  accepted in a lambda.
- **Criterion: coherence and fidelity** (criteria.md sections 2 and 6).
- **Tests.**

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | [block-pin-rebind.moss](examples/block-pin-rebind.moss) (rebind in a nested block) | 7 | 7 | rejects | accepts |
  | [block-pin-rebind-top.moss](examples/block-pin-rebind-top.moss) (same rebind at the body's top) | 7 | rejects ("compilation failed: `T`") | rejects | rejects |

- **Corpus.** An oracle variant whose nested blocks run without pins
  (`scratch/oracle/pinscope`), against the oracle: 413 same, 0 diff (the
  oracle's own pins, P(F) ∩ M(F) less X(F), never rejected a nested
  rebind in the corpus). An attempt to give the oracle Audit's pin set
  (M(F)) was not faithful (it rejects `lib/wasistd.moss`'s top-level
  `bind size::Size`, which the oracle treats as a manifest pin of X(F)),
  so it was not used.
- **Proof.** PublicBodySlice's BlkE case runs the block in the pinless
  frame: `FrameSame F (without_pins F)`, and code safety reads no pins
  (`pre_frame`, `outcome_frame`).
- **Class.** A relaxation toward both implementations.

### 50. A call's θ is principal (coherence branch; proposals item 5)
- **Rule now.** `CallTheta` has a fourth clause. A requirement r of the
  callee that the caller leaves unbound, whose merge class in the
  callee's context (the generations `canon Cs` reads as one generation
  z) has no member the caller binds, is instantiated as itself, read
  through the class's representative: `θ r = canon C (TAbs z)`. No other
  instance is allowed. Where the class has a member the caller binds,
  the merge clause reads r through that member, as before; a requirement
  the callee merges with a structured type reads through its structure.
  For a singleton class, θ r is r itself. A class with several members
  and none bound reads through z, not each member as itself, because
  the merge clause must still hold of θ (each member as itself would
  reject every call whose callee merges two requirements the caller
  leaves unbound).
- **Why.** Before, θ was fixed only where the caller binds a
  requirement, so a derivation could guess an unbound requirement at any
  type that made `Satisfies` succeed (`ExCallD`, `FPDefined`,
  `MPAttached`, the attached-call rules).
- **Criterion: principality** (criteria.md section 3), which also gives
  coherence (section 2): θ is determined by the call.
- **Tests.**

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | [principal-theta-unbound.moss](examples/principal-theta-unbound.moss) (main binds `Concrete.foo`, calls a fn needing `T.foo` with T unbound) | rejects ("needs `T.foo`, which is not available") | rejects ("not available in the current context: `foo`") | accepts (θ T = Concrete) | rejects |
  | [partially-bound-merge.moss](examples/partially-bound-merge.moss) (f merges X with Y; the caller binds X only) | rejects (D62); the oracle fix below prints `k` | prints `k` | accepts (θ Y = Char, forced by the merge clause) | accepts (θ Y read through X) |
  | [unbound-merge-representative.moss](examples/unbound-merge-representative.moss) | rejects | (not recorded) | rejects | rejects |

- **Oracle.** Its `call_theta` already reads an unbound requirement as
  itself, so it never guessed; but it also reads a requirement as itself
  when a member of its callee class is bound, and so rejects
  partially-bound-merge (E24). The fix, `principal_env` in Elab.v (read a
  requirement not in force through a member of its callee class, under
  `fi_merges`, that is in force, before `require_needs`, `fn_sig` and
  `call_theta` at a direct or attached call), makes it print `k`. It is
  **not committed**: the oracle's specification
  (`ExecutionModel.Elaboration`, `ExCallD` and the attached-call rules)
  states a call at `theta_at W C f` and `Satisfies C`, and Elab's
  soundness proofs (CallDefined, MethodCallCore and the files over
  them) would need the call restated at the principal context. That is
  the remaining part (E24).
- **Corpus.** Oracle variants against the oracle at 9e2fb21, over
  tests/, examples/ and bootstrap/examples/ (with the new test):
  - the fix (`principal_env` at direct and attached calls and at fn
    providers): elaboration 412 same / 2 diff, full runs 412 same / 2
    diff. partially-bound-merge changes verdict (rejected, now prints
    `k`); unbound-merge-representative stays rejected with another
    message (now the merge check: Y and Z are Bool and Char).
  - the Audit-mirroring variant (the fix, plus: a wholly unbound class
    reads through its representative): 413 same / 2 diff against the
    oracle (the same two), 415 same / 0 diff against the fix.
  No program changes from accepted to rejected.
- **Proof.** `public_safety` unchanged. `apply_theta` (PublicBodySlice)
  proves the new clause vacuously (an application reads every input of
  F here); `call_theta_merged` and `merged_meaning_tf` destructure one
  more conjunct.
- **Class A** (reject-only, mirroring both implementations, which reject
  principal-theta-unbound), and it settles E24 for Audit: partially-bound-
  merge is accepted, as native does.

### 51. F's congruence decides "same need" across contexts (71be6ac, branch sameneed; PART-DESIGN §1.5 item 7, R2)
- **Rule now.** Where `bind F = G;` (BFunClosure, LinkBody) or
  `bind F = H;` (BFunCopy) relates two functor contexts, a need of G (or
  H) is one of F's when it is, read through F's merge
  (`canon_item Cu k`, the key with its receiver read by F's σ). Each site
  that compared G's canonical keys with F's syntactically is restated:

  | Rule | Before | Now |
  |---|---|---|
  | LinkBody 3b | `~ In k nsu` | `~ In (canon_item Cu k) nsu` |
  | LinkBody, linked provisions | `In k nsu -> ilook (provision_id k) Cg = ilook (provision_id k) Cu` | `In (canon_item Cu k) nsu -> option_map (canon_provision Cu) (ilook (provision_id k) Cg) = ilook (provision_id (canon_item Cu k)) Cu` |
  | BFunClosure, linked `sm` | `filter (existsb (item_eqb k) nsu) nsg`, mapped to `rkey_of Cu k` | `filter (existsb (item_eqb (canon_item Cu k)) nsu) nsg`, mapped to `rkey_of Cu (canon_item Cu k)` |
  | BFunClosure, captured needs (3b's `Satisfies`) | `negb (existsb (item_eqb k) nsu)` | `negb (existsb (item_eqb (canon_item Cu k)) nsu)` |
  | BFunCopy, H's needs among F's | `incl nsh nsu` | `forall k, In k nsh -> In (canon_item Cu k) nsu` |
  | BFunCopy, H reads its needs as F does | `ilook (provision_id k) Ch = ilook (provision_id k) Cu` | `option_map (canon_provision Cu) (ilook (provision_id k) Ch) = ilook (provision_id (canon_item Cu k)) Cu` |
  | BFunCopy, `sm` | `rkey_of Cu k` on an H-spelled `k` | `rkey_of Cu (canon_item Cu k)` |

  `canon_item` and `canon_provision` are new definitions next to `canon`.
  Every old condition implies the new one (F's needs and provisions are
  canonical in F), so nothing is newly rejected.
- **Why.** The two contexts choose their merges' representatives
  independently. When both merge X with Y but pick different
  representatives, a need F spells `Y.m` is `X.m` in G. Audit then
  treated it as captured, not linked, and compared the linked provisions
  that mention the class (`vx: Y` against `vx: X`) as different. Since
  Audit's orientation is a free choice in the world, the verdict and the
  meaning depended on that choice.
- **Owner's decision.** F's congruence decides "same need": F's needs are
  what each application supplies. If one need class of G meets two need
  classes of F, the program is ambiguous and rejected. This needed no
  new clause. LinkBody's input-reading clause (#11) and BFunCopy's
  (`canon Cu (TAbs x) = canon Cu tx`) already rule it out, and
  `PublicSameNeed.link_need_unambiguous` and `copy_need_unambiguous`
  prove it.
- **Criterion: coherence** (criteria.md section 2). Proved in
  [PublicSameNeed.v](theories/PublicSameNeed.v), all as certificate
  roots:
  - `need_reading_orientation_free_F`: whether a key, read through F's
    merge, is a need of F is the same for every most general merge of F's
    telescope (from `PublicOrientation.mgm_same_classes`);
  - `need_reading_orientation_free_G`: two orientations of G's merge
    whose readings of F's inputs hold in F (#11) give the same F reading
    of every key over F's inputs (from `mgm_reorient`).

  The G side is invariant only among orientations that #11 accepts. #11
  itself still depends on G's orientation when G merges an input of F
  with a type only G has (proposals item 4, "Change B"); this change does
  not touch that.
- **Tests.** In each pair, the second file is the first with the merge
  orders agreeing. Audit's verdicts are by reading.

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | [sameneed-link.moss](examples/sameneed-link.moss) (F links X.m, vx, A.gimme; opposite orders) | rejects (D84 3b: "captures `X.m`") | 7 | depends on the orientation: accepts when F and G choose the same representative, else rejects (linked provisions; 3b) | 7, every orientation |
  | [sameneed-link-agree.moss](examples/sameneed-link-agree.moss) | 7 | 7 | as above | 7 |
  | [sameneed-meaning.moss](examples/sameneed-meaning.moss) (X fixed; X.m rebound between bind and application) | 1 (captured) | 1 | accepts with two meanings: 2 (linked) or 1 (captured), by orientation | 2, every orientation |
  | [sameneed-meaning-agree.moss](examples/sameneed-meaning-agree.moss) | 2 | 1 | 2 or 1, by orientation | 2 |
  | [sameneed-copy.moss](examples/sameneed-copy.moss) (`bind F = H;`, opposite orders) | rejects (D78: "needs `X.m`, which `F` does not take") | rejects ("does not satisfy the required signature: `X`") | depends on the orientation (`incl nsh nsu`; provisions) | 7, every orientation |
  | [sameneed-copy-agree.moss](examples/sameneed-copy-agree.moss) | 7 | 7 | as above | 7 |
  | [sameneed-ambiguous.moss](examples/sameneed-ambiguous.moss) (G merges X, Y; F does not, and needs X.m and Y.m) | rejects ("linked input interpretation differs") | 7 | rejects (#11) | rejects (#11) |
  | [sameneed-ambiguous-other.moss](examples/sameneed-ambiguous-other.moss) | 7 | 7 | rejects (#11) | rejects (#11) |

  Native prints 1 on sameneed-meaning-agree.moss and also when no merge
  is involved, where the oracle and Audit print 2: native captures needs
  keyed at fixed inputs. That divergence predates #51.
- **Corpus.** An oracle variant whose `bind F = G;` and `bind F = H;`
  read G's (H's) need keys through F's union-find (`canon_key ef k`) in
  the linked and captured filters, the linked smap, the 3b check, the
  linked-input check and rule 4 (scratch/oracle/sameneed, scratch/oracle/corpus.sh),
  compared with the oracle over tests/, examples/ and bootstrap/examples/
  (with the new tests):
  - elaboration: 421 same / 2 diff (sameneed-link and sameneed-copy,
    rejected, now accepted);
  - full runs: 420 same / 3 diff (those two, now printing 7, and
    sameneed-meaning, 1 now 2).

  No corpus program changes verdict, and nothing changes from accepted to
  rejected. The variant leaves sameneed-ambiguous-other accepted, because
  the oracle has no counterpart of #11.
- **Proof.** `public_safety` unchanged. PublicBodySlice's `captured` and
  `linked_needs` take F's context and filter by `canon_item`. The linked
  case of the application's environment (`PublicBodySlice.link_entry`) and
  `PublicCopyApply.copy_step_realizes` read a linked provision through
  F's merge. A provision of G at type t is typed at `ground ce (canon Cu t)`,
  and `Absorbs` (the callee environment equates whatever F merges) gives
  `ground ce t`. A new helper, `Unc` (a type F reads over its table is over
  its table), keeps the scoping. The full re-orientation lemma was not
  needed: the proof never re-orients a context. It reads G's spelling
  through F's merge where F's provision is used.
- **Class C and D, a relaxation.** sameneed-copy.moss is accepted although
  both implementations reject it (they reject only this merge order, so
  coherence decides, criteria.md section 6). sameneed-meaning.moss's
  meaning changes from "1 or 2, by orientation" to 2. The oracle's
  orientation, reading order, gives 1 there.
### 52. A merge class is one vertex of the expanding-cycle check (coherence2 branch; PART-DESIGN §1.5 #8)
- **Rule now.** `InstantiationsBounded` takes the world, and its level
  also satisfies two constraints per code node n and its context C
  (`NodeCx`: a fn's `w_context`, a lambda provider's enclosing context,
  a linked functor body's G context):
  - every symbol v that `canon C (TAbs t)` mentions is at least t's
    level, strictly above it under a constructor (an edge v → t);
  - if `canon C (TAbs t) = TAbs z`, t is at least z's level (an edge
    t → z).

  So the members of a merge class share a level, and a class merged with
  a structure lies strictly above what the structure mentions.
- **Why.** The vertices are (node, symbol), and the edges come from the
  θ, tables and captures P spells. Which member spells a class is the
  merge's representative, an open choice (`MostGeneralMerge`). Without
  edges inside a class, a cycle could enter the class at one member and
  leave it at another, or at the structure the class is merged with, and
  then whether it is seen would depend on the spelling. The classes do
  not depend on the choice (`PublicOrientation.mgm_classes_iff`), and
  two orientations read a structure alike up to renaming inside classes
  (`mgm_reorient`), so the new constraints do not depend on it either.
  The future `mono` finiteness proof relies on this check.
- **Criterion: coherence** (criteria.md section 2), for specialization
  (section 4, part 2).
- **No counterexample today.** I found no program the old check misses.
  Audit spells every class canonically, θ covers every formal of the
  callee (`CallTheta`: `map fst th = map fst (c_ty Cs)`), and a caller
  gives all members of a callee class one meaning. A cycle through a
  non-representative member therefore also runs through the members the
  code mentions. The change makes this hold by construction. It also
  covers raw spellings, which PART-DESIGN's M5 (and R6) introduces and
  for which the old check would be unsound.
- **Tests.**

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | [expand-merged-xy.moss](examples/expand-merged-xy.moss) (nest merges X, Y; polymorphic recursion through the class) | prints 5 | crashes at compile time (call stack exhausted) | rejects | rejects |
  | [expand-merged-yx.moss](examples/expand-merged-yx.moss) (the same, merge listed the other way, so the oracle spells the class by Y) | prints 5 | crashes at compile time | rejects | rejects |
  | expand-*.moss (#48) | unchanged | unchanged | unchanged | unchanged |

  The oracle variant (below) rejects both merged tests with and without
  the merge edges, and keeps every expand-*.moss verdict.
- **Corpus.** The ob-expand oracle variant (expand.ml, expandty.ml)
  ported to this branch's oracle (`scratch/oracle/expand`), with
  `MOSS_EXPAND_MERGE=1` adding each fn's merge edges (its `fi_merges`,
  read through the oracle's union-find, as above). Over `tests/`,
  `examples/` and `bootstrap/examples/`: 415 same / 0 diff before the
  two new tests. Both new tests are rejected with and without the merge
  edges (for expand-merged-yx only the message's vertex differs). The variant has no lambda or functor-body merges; Audit
  has them too.
- **Proof.** `public_safety` unchanged: safety never reads the conjunct.
  `AuditBuildExample.main_bounded` proves the new constraints for the
  unit program (`node_cx_unmerged`: main's context merges nothing, and
  there is no lambda or linked body).
- **Class B** (a strengthening of #48's check, which the implementations
  lack). No program found whose verdict changes.

### 53. Main's fixed machine types are read by class (coherence2 branch; PART-DESIGN §1.5 #5)
- **Rule now.** `FixedMachinesRead` (Platform.Entry, for each platform
  functor in main's context) asks, for each machine type `symbol` the
  functor fixes at meaning t:
  - `canon C t = canon C (TAbs symbol)`: t is in the machine type's merge
    class;
  - `canon C (TAbs symbol)` is a symbol: the class has no structure;
  - every y with `canon C (TAbs y) = canon C (TAbs symbol)` is `symbol`:
    the class is the machine type alone.

  Before, it asked `canon C t = TAbs symbol`.
- **Why.** The old clause held exactly when the machine type represented
  its class. If main merges I32 with an abstract X, the clause passed with
  representative I32 and failed with X, so acceptance depended on an open
  choice. All three new conditions are about classes, and the classes do
  not depend on the choice (`PublicOrientation.mgm_classes_iff`; a class
  merged with a structure reads as a structure under every most general
  merge).
- **Form chosen.** PART-DESIGN proposed a permissive form: a class with
  no structure and no other *machine* member, with `InitialTypes` mapping
  every member to the machine. I chose the strict form instead:
  - Both implementations reject a machine type merged with an abstract
    symbol, in either order (tests below).
  - The permissive form would accept those programs (class C). Its proof
    would need `InitialTypes` and the platform proofs (`entry_realizes`,
    `entry_primitive`) restated at a non-machine representative.
  - The strict form implies the old clause in a canonical context
    (`PublicEntry.fixed_machine_self`), so the proof is unchanged in
    substance.

  The strict form agrees with design note 7.1's orientation rule ("main:
  a machine symbol") and with R1 (b).
- **Criterion: coherence** (criteria.md section 2), and fidelity
  (section 6).
- **Tests.**

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | [platform-fixed-merge-xi.moss](examples/platform-fixed-merge-xi.moss) (`context Two = X, i32_eqz[I32 = X], i32_eqz`) | rejects ("incompatible assumed fixed type") | rejects ("incompatible types: `Arrays`") | clause passes with representative I32, fails with X (argued) | rejects |
  | [platform-fixed-merge-ix.moss](examples/platform-fixed-merge-ix.moss) (the same merge, the other order) | rejects | rejects | the same | rejects |
  | [platform-fixed-merge.moss](examples/platform-fixed-merge.moss) (I32 = Bool, #39) | rejects | rejects | rejects | rejects |

- **Corpus.** An oracle variant whose `check_platform_entry_view` asks
  the same class condition (`scratch/oracle/fixedcls`: the class of the
  machine type, over the env's `en_uf` and `en_tn` symbols, is the type
  alone), against the oracle, over `tests/`, `examples/` and
  `bootstrap/examples/` (with the new tests): 419 same / 0 diff.
- **Remaining.** Other platform-entry clauses of main read types
  syntactically and so also depend on the representative: `Entry`'s
  I32 constant (`MachineType`), `AtKey` and `HandlesRead`. They are
  reached only after this clause, which now rejects every class that
  merges a machine type with anything, so for machine types these
  clauses now see the type itself. Main's structure-free classes with no
  machine member (R1) are untouched.
- **Proof.** `public_safety` unchanged. `entry_realizes` derives the old
  reading from the new clause (`fixed_machine_self`, by idempotence of
  main's merge).
- **Class A** (reject-only, mirroring both implementations).

### 54. `Satisfies` checks the callee's merges by class (coherence2 branch; PART-DESIGN §1.5 #6)
- **Rule now.** The merges clause of `Satisfies` checks each class of
  the callee's merge that is wholly in force here. That means all its
  members are in force, and so are the classes of the symbols of the
  structure it is merged with. For such a class, the caller reads a
  member g as it reads `canon Cs (TAbs g)`. Before, the clause checked
  each equation (g, t) of the callee's σ whose symbols g and `gens t`
  were in force.
- **Why.** Whether an equation mentions a symbol that is not in force
  depends on the representative. At a closure (`bind F = G;`, BFunClosure
  3b), take a G class {X, Y, Z} where X is linked and in force, Y is
  captured and in force, and Z is not in force.
  - With representative X, the clause checked Y = X and skipped Z = X.
  - With representative Z, every equation mentions Z and was skipped.

  So a class only partly in force was checked under one representative
  and not under another. Classes, and "wholly in force", do not depend on
  the choice (`PublicOrientation.mgm_classes_iff`).
  - At calls, `CallTheta`'s merge clause already holds of every class,
    so this clause adds nothing there.
  - At applications and families, every input is in force. There the
    class form gives every equation of σ
    (`PublicOrientation.merges_by_pairs`,
    `PublicMergeClass.functx_merges`), which is what the proofs use.
  - At closures, the permissive form is sufficient (PART-DESIGN): a
    class only partly in force has a linked member, and each application
    reads the class at θ.
- **Criterion: coherence** (criteria.md section 2).
- **Coordination.** Built on `sameneed` (#51, merged): the clause it
  restates sits next to 71be6ac's `canon_item` filters, which are
  unchanged.
- **Tests** (Audit verdicts argued from the rules, not run):

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | [closure-partial-class-xyz.moss](examples/closure-partial-class-xyz.moss) (X = Bool and Y = Char at the bind, Z unbound) | rejects ("linked input interpretation differs") | prints k | rejects (LinkBody forces representative X; Y = X fails) | accepts |
  | [closure-partial-class-zyx.moss](examples/closure-partial-class-zyx.moss) (the same, merge listed the other way) | prints k | prints k | rejects | accepts |
  | [closure-partial-unbound-xyz.moss](examples/closure-partial-unbound-xyz.moss) (X unbound at the bind) | rejects | prints k | accepts | accepts |
  | [closure-partial-unbound-zyx.moss](examples/closure-partial-unbound-zyx.moss) | prints k | prints k | accepts | accepts |

  The oracle is itself order-dependent on these programs. Its linked
  input interpretation check (`defined_link_inputs_compatible`) reads the
  class through its union-find representative. That is an oracle
  coherence bug, not fixed here.
- **Corpus.** No oracle variant changes acceptance. The oracle has no
  check of G's merges at a closure's bind: it checks only that captured
  keys are present and that their frozen readings agree
  (`check_captured_reading`). Audit's change is a relaxation, and it
  changes Audit's verdict only where Audit rejected before (as on
  closure-partial-class-*.moss). Neither implementation runs Audit, so
  no corpus verdict of theirs moves.
- **Proof.** `public_safety` unchanged. Ten sites that read the merges
  clause at an application, a family or a copy now derive the per-equation
  form from the class form through `PublicMergeClass.functx_merges`
  (new file, added to the certificate's roots): PublicBodySlice
  (`apply_theta`), PublicFamilyAgree (two), PublicChainFwd,
  PublicChainSigs, PublicChainRealize (two), PublicFamilyLink and
  PublicFamilyCopy (two).
- **Class:** a relaxation, toward native (and toward the oracle in one
  merge order).

### 55. A bracket application on a fn call is its expansion (`ExCallBr`, branch call-brackets; D89)
- **Rule now.** `Ex` (Audit.v, and ExecutionModel.v's oracle relation
  alike) has a rule `ExCallBr`: `f[A=T, ...](e1, ..., en)`, with `f` a fn,
  elaborates as `Syntax.call_block`, the block
  `{ let %arg0 = e1; ...; bind A = T, ...; f(%arg0, ...) }`, under
  `CallBrackets`: each binding names a type symbol among `w_reqs W f` and
  binds it to a type by name (`Spec (Some rp) None ra`). `%` starts no
  name a program can write. A tag's application keeps `ExTag`. Before,
  no rule read `AECall pa (Some bs) args` with `pa` a fn, so Audit
  rejected every such call.
- **Why.** D89 (docs/design/semantics.md): an element-free constructor
  such as `stack(): Stack` under `assume Item` could be called only as
  `{ bind Item = Float; stack() }`. Native ignored a fn call's brackets
  (checking `f[A=T](x)` as `f(x)`), and the oracle refused them.
- **Criteria.**
  - Fidelity (section 6): the oracle (`check_call_brackets`, then the
    expansion) and native (`lower::desugar_call_brackets`, a rewrite to the
    same block) implement the same expansion in the same branch.
  - Stability under rebinding (section 5): the arguments are evaluated
    before the binds, in the caller's terms, so a bracket cannot change
    what an argument means (as D84 rule 7's on-demand application does).
  - Principality and coherence (sections 2, 3): the expansion's call is an
    ordinary call, so its θ is `CallTheta`'s principal one (#50); the
    requirements the brackets do not name pass through.
  - Specialization (section 4): `NoExpandingCycle` is stated on the
    elaborated program, where a bracket call is the expansion's
    `BType` bind and `ECall θ`, so it needs no new case, in #48's form
    and in #52's (`InstantiationsBounded W P` with each node's `NodeCx`:
    the expansion adds no merge, and its call's θ gives the same strict
    edge). A bracket that grows the callee's own instantiation is
    rejected like any polymorphic recursion.
- **Tests.**

  | program | oracle | oracle variant with the expanding-cycle check (#48, #52) | native | Audit |
  |---|---|---|---|---|
  | `tests/wasi/call_brackets.moss` | `2`, `21` | `2`, `21` | `2`, `21` | accepts (by reading) |
  | `tests/wasi/call_brackets_unrequired.moss` (`stack[Elem=Float]()`, `Elem` not required) | rejects ("binds `Elem`, which is not a type `stack` requires") | rejects | rejects (`e_irrelevant`) | rejects (`CallBrackets`) |
  | [expand-call-brackets.moss](examples/expand-call-brackets.moss) (`nest[T=Wrap](Wrap(x), n - 1)` inside `nest`) | accepts, prints 5 | rejects: expanding cycle, nest's T grows into nest's T | crashes at compile time (wasm call stack exhausted), as on expand-polyrec | rejects (`InstantiationsBounded`: the strict edge (nest, T) -> (nest, T), by reading) |
  | `stack[Item=Float](1)`, `stack` taking no argument (scratch) | rejects ("`stack` takes 0 argument(s), got 1") | rejects | **invalid Wasm** ("values remaining on stack at end of block", reported as a compiler bug) | rejects (the expansion's call has no `Args` derivation: `ArgsNil` needs as many arguments as parameters) |
  | `stack[Item=Float, Int=Int]()` under `assume Std, Item` (scratch) | prints 0 | (not run) | prints 0 | accepts (`Int` is in `w_reqs`, through `Std`) |

  The variant is scratch/expand-variant2: this branch's extracted oracle
  with #52's port of the check (coherence2's scratch/oracle/expand:
  `Expand.check`, `Expandty.check`), run with and without
  `MOSS_EXPAND_MERGE=1`; both verdicts are the same, and the same as
  #48's original variant's. It also still rejects expand-polyrec and
  expand-merged-xy and accepts expand-control-loop.
- **Open review points.**
  - (a) A bracket may bind any type the callee requires, flattened
    through contexts, so every type a context such as `Std` brings in
    (`Int`, `Float`, ...) may be bracketed, as the last row shows. This is
    D22's rule for a bracket on any reference, kept by the owner's
    decision; a narrower rule (only the types the callee's own `assume`
    names) is not adopted.
  - (b) Native checks no arity, for a bracket call as for a plain one:
    `stack[Item=Float](1)` compiles to invalid Wasm. Audit and the oracle
    both reject it, so this is a native bug (a crash, not a wrong run),
    not a divergence of the semantics.
  - Native also accepts a bracket binding a type to one not in force,
    which the oracle rejects, as it does for a plain `bind`.
- **Proof.** `public_safety` gains no premise. `PublicBodySlice.ex_step`
  reads the expansion's derivation (`call_block_breaks`: only an argument
  of the expansion can break; its binds' `ItemBreaks` cannot be
  `IBApply`, whose item has no expression), and `PublicFormation.ex_ok`
  and `PublicScoping.ex_scoped` take the expansion's type. On the oracle
  side, `CallAbstract.brackets_code_sound` and `synth_brackets_sound`
  relate `check_call_brackets` (`BodyTy.check_call_brackets_spec`) and the
  expansion's elaboration to `ExCallBr`.
- **Class.** None of A to E exactly: an extension of the language that
  both implementations make in the same branch, so a relaxation toward
  both, like #49. Audit rejects two things native accepts (arity, and a
  binding to a type not in force), as it does for plain calls and binds.
### 56. A copy composes its smap and positions into the closure (eagercopy branch)
- **Rule now.** `bind F = H;` (`BCopyFunctor`) composes its smap and its
  result positions into H's closure when it is evaluated, so a closure
  carries one of each however many copies made it:
  - `FunctorClosure : usym -> option smap -> environment -> option (list (option nat)) -> provision_value`.
    `None` is the root's own: a platform family reads the application's
    needs as they are, and a root reads its sources (`fc_sources`) in
    order.
  - A copy installs `FunctorClosure h (compose_adaptation a sm) dc (compose_positions p rm)`.
    `compose_adaptation` reads each requirement of H's adaptation from
    sm's source for H's key (`smap_source`). `compose_positions` reads
    position j of F's result at entry `rm_j` of H's map; an entry out of
    range is a stray (`None`).
  - An application selects through one map each: `adapt_provisions`
    (now a `Definition` on `option smap`) and `closure_sources` (which
    replaces `chain_sources`) in `result_selection`.

  Before, the closure carried `list smap` and `list (list nat)`, a copy
  appended `sms ++ [sm]` and `rms ++ [rm]`, and an application chained
  through every entry (`adapt_provisions` as a fold, `chain_sources`).
  Elaboration is unchanged; this is run time only.
- **Why.** Simplicity of the run-time representation (owner's request):
  a closure value no longer records the history of the copies that
  made it.
- **Equivalence** ([EagerCopy.v](theories/EagerCopy.v), certificate
  roots). The file restates the old chained forms (`adapt_chain`,
  `chain_sources`) and the composed form of a chain (`smap_of`,
  `positions_of`, the copies composed one at a time from the root's).
  - Positions agree exactly, for every list of sources:
    `sources_compose` (the composition lemma: reading through
    `compose_positions p rm` is reading through p, then rm),
    `positions_chain` (`closure_sources (positions_of rms) s = chain_sources rms s`),
    and `selection_chain` (the application's selection is the chained
    one).
  - Needs agree whenever the chained selection succeeds: `adapt_compose`
    (the composition lemma: selecting through sm, then H's adaptation, is
    selecting through the composition) and `adapt_chain_sound`
    (`adapt_chain d sms = Some e -> adapt_provisions d (smap_of sms) = Some e`).
  - The converse fails only where the chain is stuck
    (`adapt_chain_strict`): a copy's smap entry that no later step reads
    still had to be found by the chain, and the composition does not
    look for it. No single smap can express that check without changing
    the selected environment. An accepted program is never stuck
    (`public_safety`), so the two semantics agree on every accepted
    program.
- **Tests and corpus.** The oracle does not extract Audit's interpreter:
  every extracted `.ml` file is byte-identical to `origin/main`'s.
  The oracle built at this change against `origin/main`'s (2926b06),
  over tests/, examples/ and bootstrap/examples/ (431 programs):
  elaboration (`--elab-only`, timing line removed) 431 same / 0 diff,
  full runs 431 same / 0 diff (scratch/tools/corpus.sh, elab1.sh).
- **Proof.** `public_safety` unchanged. The proofs follow the new shape:
  `ClosureOk`'s `CCopy` and `CopyChain`'s `CCStep` conclude at
  `compose_adaptation sms ...` and `compose_positions rms rm`; the root
  cases at `Some sm`/`None` and `None`/`None`. `PublicCopyApply.adapt_snoc`
  and `PublicCallerKeys.chain_sources_snoc` are replaced by
  `EagerCopy.adapt_compose` and `EagerCopy.sources_compose`, and the
  dead `PublicBodySlice.chain_sources_in` is deleted. Across the 13
  proof files touched, 12,501 lines become 12,479 (22 fewer), plus the
  120 lines of EagerCopy.v. The chain inductions (`CCopy`, `CopyChain`,
  `chain_sigs`, `chain_real`, `chain_sources_range`) stay. They go over
  the static facts each copy has at its own site (`CopyOutputs` at that
  site's fixed readings and fresh symbols, `SameResult`, `CopySite`'s
  context and environment), not over the run-time lists. Those facts do
  not compose into one copy from the root, since two copies' sites are
  different contexts.
- **Class D (unobservable).** Run-time representation only. It is
  observable only on a run the old semantics leaves stuck.

### 57. Assumed items bring their requirements and needs (`AssumesComplete`, deltafix branch; D20)
- **Rule now.** `ElaboratesIn` has a new last conjunct, `AssumesComplete W`.
  For every val, fn, method, context and functor symbol, its assume
  nesting forms (`AssumeCtx`: `CtxOf` without the world's record, since a
  functor's record is its inputs' context, `FunCtx`), and in that formed
  context C every need k naming a val, fn or method s (`need_item`; a
  functor's fixed inputs are already asked in force by `IEFun`) satisfies
  `AssumedComplete`:
  - **requirements:** every entry of k's recorded instantiation
    (`ilook k (c_apps C)`, `AppOf` completed from the whole list by
    `entries_cx`'s freeze) is `Some`: each requirement of s (for a method,
    each its signature mentions) is bound by a bracket on the item or
    assumed somewhere in the list;
  - **needs:** reading C with k's instantiation in force (Cv, `set_ty` of
    each recorded meaning), s's own needs are answered as a call of s at
    that instantiation would answer them: `CtxOf W s Cs nss`,
    `CallTheta W Cv Cs s th` and `Satisfies Cv Cs th nss sm` (presence,
    coherence of instantiations, the provision's signature, and the
    merges by class).

  Before, nothing checked an assumed item's requirements or needs where
  it was assumed: only a call of the item, or of something needing it,
  did. An abstract fn, a val, or a fn that never calls the item was
  accepted with the item incomplete.
- **Why.** Fidelity (criteria section 6): both implementations reject
  `tests/errors/assume_missing_requirement_type.moss` and
  `assume_missing_requirement_need.moss`, and the variants below; Audit
  accepted them. The oracle's check is `check_assumes` and
  `check_assumed_requirements` (Elab.v), over every val, fn, method,
  context and functor symbol's formed context (`fi_env`, `fi_needs`): each
  requirement (for a method, its signature's) in force with the item's
  brackets, then `require_needs` (presence, `incoherent`, `require_merges`).
- **Form chosen.** The check is over formed contexts, not over each item
  as it is read (`ItemEntries`), so `assume f, seed, T { ... }`
  (`tests/wasi/assume_requirements_same_list.moss`) stays accepted: the
  recorded instantiation is completed from the whole list. The needs half
  reuses `CallTheta` and `Satisfies` rather than restating the oracle's
  `require_needs`. Differences from the oracle, none seen in the corpus:
  - `Satisfies` also compares the selected provision's signature with
    the callee's view of it (its types clause, #4);
  - `CallTheta`'s merge clause checks every merge of the item's context
    at the principal θ, while `require_merges` skips a merge mentioning a
    symbol not in force. For a val or fn every requirement is in force
    (the first half), so the two agree; they can differ only for a
    method's requirement its signature does not mention;
  - every such symbol's nesting must form. The oracle forms each too
    (`fn_info`), but Audit's formation is not run, so a program whose
    unused abstract item's nesting forms in the oracle and not in Audit
    would newly be rejected. None is known.
- **Criterion: fidelity** (criteria.md section 6); the coherence half
  serves D62/D71 (criteria section 2: an item needing a provision at one
  instantiation is not assumed beside it at another).
- **Tests** (Audit verdicts argued from the rules, not run):

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | `tests/errors/assume_missing_requirement_type.moss` | rejects ("assuming `f` needs `T` in force (D20)") | rejects | accepts | rejects |
  | `tests/errors/assume_missing_requirement_need.moss` | rejects ("calling `f` needs `seed`") | rejects | accepts | rejects |
  | `tests/errors/assume_main_without_root.moss` (`assume main`, main imported as `start`) | rejects ("needs `StructRef` in force") | rejects | accepts | rejects (start's root needs Heap, Mutable, Arrays) |
  | [assumed-ctx-item](examples/assumed-ctx-item.moss), [assumed-main-calls](examples/assumed-main-calls.moss), [assumed-type-abstract](examples/assumed-type-abstract.moss), [assumed-type-use](examples/assumed-type-use.moss), [assumed-val-type](examples/assumed-val-type.moss) (a requirement missing) | rejects | rejects | accepts | rejects |
  | [assumed-meth-need](examples/assumed-meth-need.moss), [assumed-need-use](examples/assumed-need-use.moss), [assumed-val-need](examples/assumed-val-need.moss) (a need missing) | rejects | rejects | accepts | rejects |
  | [assumed-need-incoherent](examples/assumed-need-incoherent.moss) (f needs h at T = U; g has h at T = T) | rejects (D62) | accepts | accepts | rejects |
  | `tests/wasi/assume_requirements_same_list.moss` (`assume f, seed, T`) | accepts | accepts | accepts | accepts |
  | [assumed-bracket-ok](examples/assumed-bracket-ok.moss) (`f[T=U]` in a context) | accepts | accepts | accepts | accepts |
  | [assumed-ctx-decl-only](examples/assumed-ctx-decl-only.moss) (an unassumed context lists f) | accepts | accepts | accepts | accepts |
  | [assumed-functor-input](examples/assumed-functor-input.moss) (F's input f) | accepts | accepts | accepts | accepts |
  | [assumed-need-coherent](examples/assumed-need-coherent.moss) | accepts | accepts | accepts | accepts |

  The tests come from the Δ3 hunt (scratch/deltas, `d3_*`).
- **Corpus.** The oracle has the check, so the comparison is the oracle
  against a variant without it (`check_assumes` skipped in the extracted
  `elab_program`; scratch/variants/noassume), `--elab-only`, over
  `tests/`, `examples/` and `bootstrap/examples/` (with the new tests):
  434 same / 13 diff. The 13 are the 12 rejected programs of the table
  and [handle-rebind.moss](examples/handle-rebind.moss) (#8), which Audit
  already rejected through `HandlesRead`; the oracle's reason for
  rejecting it (i64_store needs Memories at another instantiation) is
  now Audit's too. No program both implementations accept is rejected.
  Audit's verdict changes on the 12 (accept to reject), and on no other
  corpus program by this comparison.
- **Proof.** `public_safety` unchanged: the conjunct is last and no proof
  reads it. `AuditBuildExample.main_assumes` proves it of the example
  world (main is the only symbol and its nesting is empty).
- **Class A**, except assumed-need-incoherent.moss, which native accepts
  (class B on that program). Native accepts it only because g never calls
  f; it does not check an assumed item's needs' instantiations.

### 58. Result items distinct in F stay distinct at an application, and H's at a copy (`CollidesOnlyAs`, deltafix branch; D84 rule 7)
- **Rule now.** `CollidesOnlyAs ks ks0`: two positions that `ks` files
  under one provision id are one id in `ks0` too, position by position.
  - `BApplyAbstract` (`bind F;`) also reads F's result in F's own input
    context, each output role itself (`ProvideResult W u Cu (fun t => TAbs t) Cf ksf`,
    beside the caller's `ks`), and asks `CollidesOnlyAs ks ksf`.
  - `BFunCopy` (`bind F = H;`) reads H's result at F's inputs (`ksr`, as
    `CopyOutputs` does) and at H's own (`ksh`), and asks
    `CollidesOnlyAs ksr ksh`.

  Before, two result items distinct in F (A.m and B.m) that the caller's
  meanings put under one key (A = B = Int) were both installed, and the
  application kept the last position's (`result_selection`). A copy whose
  target merges two of H's inputs read F's one key from H's last position
  with it (`CopyOutputs`' rm).
- **Why.** Coherence (criteria section 2): which item a key gets was a
  convention, and each implementation has its own. On
  result-collide-{ab,ba}{,-bindswap}.moss Audit printed the last result
  position's, the oracle the first's, and native the last one G binds,
  so the four programs give three different patterns. On the copy tests
  the oracle follows H's result order and Audit took the last position.
  When two derivations disagree and neither is more general, the program
  is ambiguous and is rejected. That is D84 rule 7 for families, which
  both implementations already apply (`FamilyApplication`'s `NoDup`;
  result-collide-family.moss).
- **Form chosen.** A plain `NoDup` of the caller's keys would reject a key F
  itself repeats (A.m, and a context listing A.m), which all three
  accept and run alike (result-repeat-in-f.moss). The check compares with
  F's own reading, so only a collision the caller makes is rejected, and
  F's merges decide what F counts as one item (the classes, not the
  representative: provision ids under F's σ).
  - Families need nothing new: `FamilyApplication` already asks
    `NoDup (map provision_id keys)`, which is stronger, and a family's
    items are distinct (`FamilyKeys`).
  - Copies need the check on H's side, which `BFunCopy` now asks.
    Applying a copied F goes through `BApplyAbstract` for F, so the
    first check covers the copy's application too.
- **Criterion: coherence** (criteria.md section 2).
- **Tests** (Audit verdicts argued from the rules, not run; oracle variant
  `scratch/variants/d7`, the extracted oracle with both checks added to
  `apply_abstract` and `copy_output_compatible`):

  | program | oracle | variant | native | Audit before | Audit now |
  |---|---|---|---|---|---|
  | [result-collide-ab](examples/result-collide-ab.moss) | a | rejects | b | b | rejects |
  | [result-collide-ba](examples/result-collide-ba.moss) | b | rejects | b | a | rejects |
  | [result-collide-ab-bindswap](examples/result-collide-ab-bindswap.moss) | a | rejects | a | b | rejects |
  | [result-collide-ba-bindswap](examples/result-collide-ba-bindswap.moss) | b | rejects | a | a | rejects |
  | [result-repeat-in-f](examples/result-repeat-in-f.moss) (control) | a | a | a | a | a |
  | [result-collide-family](examples/result-collide-family.moss) (control) | rejects | rejects | rejects | rejects | rejects |
  | [copy-result-collide](examples/copy-result-collide.moss) | a | rejects | rejects ("the functor does not satisfy the required signature") | b | rejects |
  | [copy-result-collide-hba](examples/copy-result-collide-hba.moss) | b | rejects | rejects | a | rejects |
  | [copy-result-collide-mba](examples/copy-result-collide-mba.moss) | a | rejects | rejects | b | rejects |
  | [copy-result-repeat](examples/copy-result-repeat.moss) (control) | a | a | a | a | a |

  The application tests come from the Δ7 hunt (scratch/deltas, `d7_*`).
- **Corpus.** The variant against the oracle, `--elab-only`, over
  `tests/`, `examples/` and `bootstrap/examples/` (with the new tests):
  450 same / 7 diff. The 7 are the rejected tests above. No other
  verdict changes, so no program both implementations accept is
  rejected: the four application tests are accepted by both, but
  differently, so criterion 6 does not hold them (section 6: where the
  implementations disagree, the first five criteria decide).
- **Proof.** `public_safety` unchanged: both are new last premises,
  which no proof reads (the patterns in PublicBodySlice, PublicFormation,
  PublicScoping and PublicLinkScope name the earlier premises only).
- **Class B.** The four application tests are accepted by both
  implementations and the copy tests by the oracle. Both
  implementations need the same check (E40 below).

### 59. `RecvTy` reads no type symbol out of force as a nominal (deltafix branch; D20)
- **Rule now.** `RecvTy`'s fallback, which reads a method item's receiver
  R as the nominal `TNom R args`, also asks `kind_sym W R <> Some DKType`.
  A type symbol has a receiver type only through its meaning in force.
  Before, `assume T.m, T { ... }` read T.m's receiver as `TNom T []` while
  T was not yet in force, so the list formed: T.m was filed at a nominal
  no value has, beside the type T.
- **Why.** Fidelity (criteria section 6) and D20 (a type an item mentions
  must already be in force; `TyOf` asks it for every other item). The
  oracle rejects the program ("type `T` is not available in the context
  here"). The Δ2 hunt (scratch/deltas/REPORT.md) found that Audit, the
  oracle and native all read signature types in list order, rejecting
  `assume f, T` with f mentioning T. This fallback was the one place
  where Audit did not.
- **Tests** (Audit verdicts argued from the rules, not run):

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | [assume-method-before-type](examples/assume-method-before-type.moss) (`assume T.m, T`) | rejects | **accepts** (E41) | accepts | rejects |
  | [assume-method-before-type-use](examples/assume-method-before-type-use.moss) (and g calls `x.m()`) | rejects | rejects ("no matching method is available for this receiver") | rejects (no m at x's type) | rejects |
  | [assume-method-after-type](examples/assume-method-after-type.moss) (`assume T, T.m`, control) | accepts | accepts | accepts | accepts |

- **Corpus.** The oracle already rejects every such list (its method
  item reads the receiver's meaning in force), so no oracle variant
  applies, and Audit now rejects a subset of what the oracle rejects. A
  scan of every `assume` list naming a method item `R.m` in `tests/`,
  `examples/`, `bootstrap/examples/` and `lib/` (34 lines) finds R in
  force before the item, or R a tag, in each. So no corpus verdict
  changes.
- **Proof.** `public_safety` unchanged. Three proofs that read `RecvTy`'s
  fallback skip the new conjunct: `PublicCallRealize.recv_scoped`,
  `PublicOutputSubst.recv_rel` and `PublicFormation.recv_ok`.
- **Class A** (reject-only, mirroring the oracle). Native accepts the
  first test, which is a native bug (E41), since native itself rejects
  the same program once g uses the method.

### 60. Rule B: a use needs the item's type requirements in force (lang-b branch; owner-approved)
- **Rule now.** To use an item (call a defined fn or attached method,
  bind it as a fn or method provider, or demand a family), each type
  requirement of the item must be in force where it is used
  (`RequirementsInForce C Cs`, `InForceThrough`): assumed or bound there,
  or in force through the item's merge class in its own context Cs (a
  member of the class in force, or a structured type all of whose symbols
  are in force). Premises: ExCallD, MCAttachedDefined, FPDefined,
  MPAttached. A family demand's matching is in force
  (`MatchingInForce`: FamilyStepApply, PrFamily, LitOpFamily).
  Two companions:
  - **Demands read needs through θ.** ExCallD's and MCAttachedDefined's
    `Demand` take the callee's needs read at θ (`at_th C th`), not at the
    call site (`at_site`), so a need `Y.m` of a callee that merges Y with
    an X the caller binds is demanded at X's meaning.
  - **Closures.** BFunClosure asks each of G's inputs that F does not
    take in force where G is bound, through G's merge class. Inputs F
    takes stay exempt (closure-partial-unbound-*).
- **Why.** Before, a requirement the caller left unbound was read as
  itself (`theta_at`, `at_site`): a family demand could key a method at a
  declared symbol out of force (family-unbound-need.moss), a lambda or
  closure then captured it (key-receiver capture, `captured_mentioned`),
  and scoping had to admit every declared type symbol. Rule B makes the
  instantiation of every use determined by the use.
- **Criteria:** principality (section 3: no requirement is read as an
  unbound symbol; θ is fixed by the use, up to the merges) and coherence
  (section 2). criteria.md section 3's example now states rule B.
- **Removed as dead** (each by proof, `public_safety` unchanged):
  - CallTheta's unbound-class clause (#50). It moves to `AssumedComplete`
    (#57): assuming an item is not a use, so rule B does not apply there,
    and the clause keeps its reading as before.
  - Method-key receivers in `lexical_capture` (key_receiver_gens stays,
    for #10's fresh-output clause, unchanged).
  - The declared-symbol disjunct of `PublicFormation.GenScoped`:
    `ScopedTy` and `KeysScoped` are now strict (in `scope_gens`).
    `keys_scoped` and `keys_scoped_context` are strengthened to match.
  - **Not removed:** most of `captured_mentioned`. It is dead only under a
    strict closure rule (each captured input in force itself), which
    rejects bind-merge-unbound.moss and closure-partial-class-zyx.moss,
    both accepted by both implementations. With the through-class rule an
    input can be out of force while a member of its class is in force, so
    the lemma stays; its key-receiver case now goes through the strict
    `KeysScoped`, and `CapturedInForceOk` takes `KeysScoped`.
- **Proof.** `DemandScopedOk` is restated for the θ reading and proved by
  `PublicScoping.demand_at_th_scoped`, from rule B and CallTheta's merge
  clause (`theta_canon_lift`, `PublicTypeRead.subst3_agree`). `cg_key`,
  `lambda_realizes` and `envok_scoped` take `KeysScoped`.
- **Oracle.** Elab.v: `requirements_in_force` at defined direct and
  attached calls and in `match_fn_sig` (fn and method providers);
  `family_inputs_in_force` in `apply_family`; the closure check in
  `bind_functor`; and `principal_env` (#50's oracle half, E24), now
  *before* `demand_needs`, so demands read needs principally. The
  oracle's specification states the call at a principal context
  (`ExecutionModel.PrincipalCx`: each requirement out of force read as the
  meaning of a type in force); its class condition is not stated, and
  `principal_env` reads only through a member in force, not through a
  structured class. Proofs: new PrincipalEnvExec.v and RuleBKeep.v;
  CallDefined*, MethodCallCore, AttachedCall*, BodyPending and the
  provider and family proofs restated. Direct calls check arguments in
  the principal env, as #50's patch did.
- **Tests** (Audit verdicts by reading):

  | program | Audit | oracle before | oracle now | native |
  |---|---|---|---|---|
  | [ruleb-call-unbound](examples/ruleb-call-unbound.moss) | rejects | accepts | rejects | accepts (E42) |
  | [ruleb-provider-unbound](examples/ruleb-provider-unbound.moss) | rejects | accepts | rejects | accepts (E42) |
  | [ruleb-closure-unbound](examples/ruleb-closure-unbound.moss) | rejects | accepts | rejects | accepts (E42) |
  | [ruleb-closure-bound](examples/ruleb-closure-bound.moss) (control) | accepts | accepts | accepts | accepts |
  | [ruleb-merge-family-demand](examples/ruleb-merge-family-demand.moss) | accepts | rejects (D62) | k | k |
  | [partially-bound-merge](examples/partially-bound-merge.moss) | accepts | rejects (D62, E24) | k | k |
  | family-unbound-need, family-unbound-need-sig, lambda-declared-output, kept-output-family-method, functor-captured-unbound, const-family-unbound | rejects (before: accepts) | accepts | rejects | accepts (E42; const-family-unbound: rejects, unrelated) |
  | bind-merge-unbound, closure-partial-class-zyx, closure-partial-unbound-zyx | accepts | accepts | accepts | accepts |

  The six edge-case tests are kept, as rejection tests (their headers say
  so); none was deleted. copy-result-collide{,-hba,-mba} (#58) assumed
  `Foo` in use_it although use_it binds it, so rule B rejected main's call;
  they no longer assume it and still exercise #58 (oracle a, b, a as
  before).
- **Corpus.** The oracle at c89de1f against the new oracle, `--elab-only`,
  over `tests/`, `examples/`, `bootstrap/examples/` and `src/main.moss`
  (with lib/), before the test edits above: 448 same / 13 diff.
  - accepted, now rejected: the six edge-case tests and
    copy-result-collide{,-hba,-mba} (since edited, see above);
  - rejected, now accepted: partially-bound-merge;
  - rejected before and now, other message: principal-theta-unbound,
    unbound-merge-representative (now the merge check: Y and Z are Bool
    and Char), tests/fixtures/imported_main/provider.moss.
  `src/main.moss`, `lib/` and every non-edge-case test keep their verdict.
- **Class B**: it rejects programs both implementations accept (the
  edge-case tests); owner-approved. Native needs rule B (E42).

### 61. Merging two entries of one item unifies their instantiations (lang-b branch; Δ4, D43)
- **Rule now.** `Unifies σ es` also asks two entries filed under one key
  to have one instantiation under σ (`entry_inst`, `inst_read`: a
  requirement not in force where the item was read, None, is read as
  itself, as the context's freeze completes it). Before, it compared only
  signatures, and the merged key kept the first entry's instantiation.
- **Why.** D43 says merging two entries unifies their instantiations. The
  Δ4 hunt (scratch/deltas) found the verdict depended on the order of the
  assume list: merge-inst-order-ab.moss was accepted and
  merge-inst-order-ba.moss rejected, by Audit, the oracle and native.
- **Criterion: coherence** (criteria section 2: an order in the rules must
  not change acceptance), and fidelity to D43.
- **Oracle.** Elab.v's assume walk (`unify_app`, before `setdefault_app`,
  for vals, fns and methods) unifies the recorded and the new
  instantiation pairwise, a None read as the requirement itself; the first
  recorded one is still kept. `ExecutionModel.Unifies` is strengthened the
  same way. Proofs: Merge.unify_app_spec; CtxWalk's `WS` gains `ws_insts`
  and `ws_apps_in`; CtxMethod*, CtxMerge.settled_unifies, ContextClean,
  ContextWalkClean, and the walk-support files (`AppsSupport`).
- **Proof (Audit).** `public_safety` unchanged.
  `PublicWorldFormation.unifies_mark` marks instantiations too
  (`inst_read_mark`); `PublicOrientation.mgm_unifies` and
  `mgm_classes_iff` hold as stated.
- **Tests** (Audit verdicts by reading):

  | program | Audit before | Audit now | oracle before | oracle now | native |
  |---|---|---|---|---|---|
  | [merge-inst-order-ab](examples/merge-inst-order-ab.moss) | accepts | rejects | accepts | rejects (A and B merged) | accepts (E43) |
  | [merge-inst-order-ba](examples/merge-inst-order-ba.moss) | rejects | rejects | rejects (D62) | rejects (A and B merged) | rejects |
  | [merge-inst-val](examples/merge-inst-val.moss) (v's type does not mention T) | rejects | accepts | rejects | accepts | rejects (E43) |
  | [merge-inst-fn](examples/merge-inst-fn.moss) | rejects | accepts | rejects | accepts | rejects (E43) |
  | [merge-inst-sig](examples/merge-inst-sig.moss) (control) | accepts | accepts | accepts | accepts | accepts |
  | [merge-inst-nomerge](examples/merge-inst-nomerge.moss) (control) | accepts | accepts | accepts | accepts | accepts |

- **Corpus.** The oracle at c89de1f against the oracle with #60 and #61,
  `--elab-only`, over `tests/`, `examples/`, `bootstrap/examples/` and
  `src/main.moss`: 451 same / 10 diff, all from #60 (the six edge-case
  tests, partially-bound-merge, and three changed messages); #61 changes
  no corpus verdict (besides its own tests above).
- **Class B and C.** It rejects merge-inst-order-ab.moss, which both
  implementations accept, and accepts merge-inst-val and merge-inst-fn,
  which both reject; coherence decides (criteria section 6). Native needs
  the same change (E43).

### 70. A defined method is not bound (`BMethBind`, lang-q branch; semantics.typ Q3)
- **Rule now.** `BMethBind` asks that the bind's method item `m` has no
  body: `~ (exists rc nm ps rt0 b, ast_of W m = Some (ADFn rc nm ps rt0 (Some b)))`.
  `R.n` on the left of a bind names R's attached method when its head has
  one (`MethodNamed`), so `bind Meter.describe = …` named the attached,
  defined `Meter.describe`. Such a provision can never be used: a call
  resolves to the definition (`MCAttachedDefined`), and the provision
  wins only for the *detached* method's key (`MCProvisionWins`, D77).
  - Method resolution is static, so the rule reads the item the bind
    names, nothing else. `bind Q.describe = …` with Q abstract names the
    detached `.describe` and stays legal after `bind Q = Meter`, and so
    does binding an abstract attached method (`fn Meter.describe();`, D36).
  - The oracle checks the same in `check_bind` (Elab.v), after
    `find_method_decl`: "`Meter.describe` is a defined method of `Meter`,
    which calls always reach; it cannot be bound". `BindMethodTrace` skips
    the new error branch. ExecutionModel's `BMethBind` is unchanged (it
    still allows the bind, so the elaborator's soundness is unaffected).
- **Why.** The owner's decision on semantics.typ's open question
  "binding a defined attached method" (example `06b-bind-attached`, which
  printed `mm`): a bind that cannot have an effect is an error.
- **Criterion: fidelity** (criteria.md section 6) in the sense that both
  implementations always call the definition, so the bind was dead code
  in both; and **coherence** (section 2): a provision under a key no call
  reads gives no derivation a choice.
- **Tests.**

  | program | oracle before | oracle now | native | Audit before | Audit now |
  |---|---|---|---|---|---|
  | [bind-attached-defined](examples/bind-attached-defined.moss) | mm | rejects | mm | accepts | rejects |
  | [bind-attached-detached-ok](examples/bind-attached-detached-ok.moss) (control: the detached method through an abstract Q) | mxm | mxm | mxm | accepts | accepts |
  | [bind-attached-abstract-ok](examples/bind-attached-abstract-ok.moss) (control: an abstract attached method) | x | x | x | accepts | accepts |

- **Corpus.** The oracle with the check (`scratch/lang-q/oracle-q3.exe`)
  against the oracle at c89de1f, `--elab-only`, over `src/main.moss`,
  `tests/`, `examples/` and `bootstrap/examples/` (with the new tests):
  463 same / 1 diff, the diff being bind-attached-defined. So no real code
  binds a defined method.
- **Proof.** `public_safety` unchanged: the premise is BMethBind's last,
  and no proof reads it.
- **Class B** (native accepts bind-attached-defined; E70 below).

---

## Consolidation (branch ob-consol)

After G05 closed, the functor-application rules were read end to end
against three principles (inputs of F read at G's own table, outputs at
the body's; an application installs exactly F's declared result under
the caller's keys, from the root's positions; copies are position maps
with fresh symbols that avoid the copy site's substitution). The full
spec and clause classification are in `scratch/consol-STATUS.md`
(git-ignored). Each removal below is its own commit, with `check.sh`
green.

| Removed | From | Effect on acceptance | Commit |
|---|---|---|---|
| `~ In t (fr_pins F)` in the kept-output case (#13, 268bf3a) | `BApplyAbstract` | relaxation toward both implementations: [block-kept-output-pin.moss](examples/block-kept-output-pin.moss) is now accepted (oracle 7, native 7) | "Drop BApplyAbstract's pin clause" |
| "G has a body" (`exists n ps a r b, ast_of W gg = Some (ADFunctor n ps a r (Some b))`) | `BFunClosure` | none: `LinkBody` states it (`w_decl W gg = Some dg`, `d_ast dg = ADFunctor n ps a r (Some bodies)`) | "BFunClosure: drop the body premise LinkBody states" |
| `Fresh W (gensof t)` in the fresh-output case | `BApplyAbstract` | none: the case asks `In (gensof t) newgens`, and `FreshFor W F C newgens` makes every member `Fresh W` (`PublicFormation.freshfor_fresh`) | "BApplyAbstract: a fresh output's freshness is FreshFor's" |
| `output_renaming` (unused definition) | Audit.v | none | "Audit: remove the unused output_renaming" |

Restatements (equivalence-preserving, each proved in
[PublicConsol.v](theories/PublicConsol.v) against the old definitions,
restated there verbatim):

| Restated | Old form | New form | Proof |
|---|---|---|---|
| `ResultRealizes` and LinkBody's result sources | each key renamed by `link_renaming outs gen` (the identity on the roles), promised types read through `read_rename` at it | keys used as they are; promised types read through `role_reads site outs gen` (each role at the site's table) | `result_realizes_restated`, `link_sources_restated` |
| BFunClosure's removed body premise | `exists n ps a r b, ast_of W gg = Some (ADFunctor n ps a r (Some b))` | implied by LinkBody | `link_body_has_body` |

Open points found in consolidation (not changed; the owner decides). The
details are in `scratch/consol-STATUS.md`.
- **Pins reach nested blocks but not lambda bodies.** Resolved by #49:
  a nested block runs without pins, as a lambda body does, so
  [block-pin-rebind.moss](examples/block-pin-rebind.moss) is accepted.
- **M(F) counts a method's non-requirement signature symbols (#12), not
  a fn's.** Resolved by #12b: `RMValFn` counts a fn's `SigMentions` too,
  in Audit and the oracle.
- **Leftovers, cleaned up on the proposals branch (item 7, one
  equivalence-preserving commit).** `result_environment` no longer takes
  the unused `caller` argument. `fc_outputs` holds only the type outputs
  (`map ITy outs` in LinkBody; `[]` for a platform family, which has no
  type output), the only items `read_result_types` reads. BFunClosure
  reads F's and G's contexts from the world (`w_context W u = Some (Cu,
  nsu)`, likewise for G) instead of restating `FunCtx`, which LinkBody
  states (`PublicBodySlice.link_body_functx`). `fresh_result_types` stays
  as the statement #20 refers to.
- **Owner-decided principles, analysed but not implemented** (proposals
  5–7 in `scratch/consol-STATUS.md`):
  - *Merge orientation* (**open**, proposals item 4; criterion:
    coherence, criteria.md section 2). LinkBody's 3b (#7) and #11 split
    one condition by orientation (G merging an input of F with a symbol
    only G has is rejected under one representative and can be accepted
    under the other), `fc_table` makes the run time follow the
    representative, and link_site spells F's keys with F's
    representative. The class-based restatement (design in
    PROPOSALS-STATUS.md: 3b and #11 by `canon Cg`-class, `fc_table`
    through a substitution `lr` sending each class that meets F's inputs
    to a linked member, link_site reading an input G lacks through a
    `canon Cu`-class member G has) was attempted on the coherence branch
    and stopped before any Audit change. What blocks it is the proof of
    `public_safety`, not acceptance:
    - under the orientation whose representative y is G's own, the
      class-based `fc_table` reads y at θ of the linked x, so a captured
      item typed at y (which the class form of 3b must allow when the
      class has a fixed input of F, since the x-rep orientation allows
      it today) is read at θ x. `PublicBodySlice.link_entry` types
      captured items at their captured meanings (`AppMeanings`), so it
      would need a new bind-time invariant: the captured meaning of y is
      the fixed reading of x. BFunClosure does not establish it (its 3b
      `Satisfies` checks G's merges only between symbols in force at the
      bind);
    - `PublicLinkRun.link_input_meaning` and the ChainRealize proofs
      read `fc_table = c_ty Cg` and "G's readings are over F's inputs"
      directly, and link_site's table (`lookup_site_in`) is used in
      PublicChainRealize's key-scoping proofs;
    - reducing the y-rep case to today's x-rep proof would need the body
      typing (`BindItems`) to be invariant under re-orienting Cg, a
      renaming lemma over every typing rule.
    Groundwork, proved: `PublicOrientation.mgm_classes_iff` (two most
    general merges of one telescope identify the same types, so a
    clause stated by class is orientation-invariant) and
    `mgm_reorient`. Native exits 0 on
    [link-merge-rep-rebind-both.moss](examples/link-merge-rep-rebind-both.moss)
    in both context orders; the oracle rejects the `[Foo=X], [Foo=T]`
    order (D84 3d, its pins include the representative) and exits 0 on
    the reversed one; Audit accepts both (by reading, as before).
  - *Principal θ:* **resolved by #50** (proposals item 5; criterion:
    principality). An unbound requirement reads through a bound member
    of its callee merge class, else as itself through the class's
    representative; partially-bound-merge.moss stays accepted, and
    principal-theta-unbound.moss (a guessed θ T = Concrete) is rejected,
    as both implementations reject it. The oracle fix is designed and
    corpus-checked but not committed (see #50).
  - *Ambiguous family read-off:* resolved by item 6 (#31): the
    matching item must be unique, as the oracle's `family_member` asks.
- **#14 after fc75de7, by reading (not run):** Audit still accepts
  `tests/errors/functor_output_input_changed.moss`. Target's result has
  no type item, so `outs = []` and link_site reads `T.out` at G's table
  as `T.out`. The body bound `T.out` before `bind Make;`, which gives T a
  fresh generation (T is in force in Provider), and that generation is no
  input of Target. The application reads the source key `T.out` from the
  body, so the "stale key" the test guards against (the oracle's refile)
  does not arise. Class C stands.

## Implementation bugs filed (E31–E37)

All of these come from `proof-plan.json`.

| ID | Title | Who must change | Status | Example | Entry |
|---|---|---|---|---|---|
| E31 | Read linked result methods from the body's keys (no bind-time refile) | oracle | open | [refile-stale-shadow.moss](examples/refile-stale-shadow.moss): oracle stuck XErr 14; native 7 | #19 |
| E32 | Install exactly F's result at a copy's application | oracle | open | [copy-extra-key.moss](examples/copy-extra-key.moss): oracle 21; native 31 | #23 |
| E33 | No result key spelled through an output's input meaning | oracle (ExecutionModel and link_body) | open | [output-merge-key.moss](examples/output-merge-key.moss): oracle stuck XErr 6; native 7. Also functor_input_output_generations (E06) | #23 |
| E34 | A family's listed polymorphic inputs are types | **native** | open | [family-poly-nontype.moss](examples/family-poly-nontype.moss): oracle rejects; native exit 0 | #9 |
| E35 | A method's θ records every signature type symbol | oracle | **done** (in 61040f2) | [sig-nonreq-shadow.moss](examples/sig-nonreq-shadow.moss): now 42 on both | #12 |
| E36 | Oracle 3b checks every reading of a captured fn or method | oracle | open | [link-merge-captured-fn.moss](examples/link-merge-captured-fn.moss): oracle accepts, stuck XErr 14; native rejects | #7, #11 |
| E37 | Oracle 3d: a linked body may not install an input of F as a kept output | oracle | open | [link-kept-output-pin.moss](examples/link-kept-output-pin.moss): oracle stuck XErr 14; native rejects | #13 |
| E40 | Reject result items distinct in F that an application files under one key, and H's items distinct in H that a copy reads as one key of F | oracle and native (copies: oracle) | open | [result-collide-ab.moss](examples/result-collide-ab.moss): oracle a, native b; [copy-result-collide.moss](examples/copy-result-collide.moss): oracle a, native rejects | #58 |
| E41 | Read an assume list in order for a method item's receiver: `assume T.m, T` needs T in force before T.m | native | open | [assume-method-before-type.moss](examples/assume-method-before-type.moss): oracle rejects; native exit 0 | #59 |
| E42 | Rule B: a use (call, provider bind, family demand) needs the item's type requirements in force, through its merge class; a closure's captured inputs F does not take likewise; demands read needs through θ | native | open | [ruleb-call-unbound.moss](examples/ruleb-call-unbound.moss): oracle rejects; native exit 0 | #60 |
| E43 | Merging two entries of one item unifies their instantiations (D43): `assume K1, K2` with `f[T=A]` and `f[T=B]` merges A with B, in either order | native | open | [merge-inst-order-ab.moss](examples/merge-inst-order-ab.moss): oracle rejects; native exit 0. [merge-inst-val.moss](examples/merge-inst-val.moss): oracle accepts; native rejects | #61 |
| E70 | Reject a bind of a defined method (`bind Meter.describe = …` where `Meter.describe` has a body) | native | open | [bind-attached-defined.moss](examples/bind-attached-defined.moss): oracle rejects; native mm | #70 |

The earlier follow-ups E21–E30 ask the implementations to match Audit
changes. All are open:

| ID | Entry | ID | Entry |
|---|---|---|---|
| E21 | #1 | E26 | #5 |
| E22 | #2 | E27 | #16 |
| E23 | #26 | E28 | #6 |
| E24 | #3 | E29 | #41 |
| E25 | #4 | E30 | #30 |

The roles record warns that fixing E31–E33 on the oracle means changing
ExecutionModel's functor code and every fast interpreter, plus their
proofs, or an oracle-only workaround in Elab.v. That is a large job.

## Known divergences between Audit and native

**Audit rejects; native accepts.**
- duplicate-field-binder (#1, E21) and while-condition-break (#2, E22).
  These are accepted per the proof plan; native runs are not recorded.
- bracket-method-receiver (#5). Native prints `1`, a wrong result.
- handle-rebind (#8). The platform fn is unused; native rejects it once it
  is called.
- family-poly-nontype (#9, E34).
- ~~lambda-declared-output and kept-output-family-method (#10)~~: Audit
  accepts both again since #10 option (b).
- ~~partially-bound-merge (#12)~~: Audit accepts it (#50: θ Y reads
  through the bound X), as native does (`k`). The oracle prints `k` since
  #60 (E24 done).
- scratch/cap3b n7b (#11, not committed). The oracle rejects it too.
- ~~block-pin-rebind.moss~~: accepted since #49.
- assumed-need-incoherent.moss (#57). The oracle rejects it too. Native
  does not check an assumed item's needs' instantiations; g never calls f.
- assume-method-before-type.moss (#59, E41). The oracle rejects it too.
- result-collide-{ab,ba}{,-bindswap}.moss (#58, E40). Both
  implementations accept them, with different outputs.
- sameneed-ambiguous.moss and sameneed-ambiguous-other.moss (#11; tests of
  #51). Native prints 7 on both, and the oracle accepts the second.
- bind-attached-defined.moss (#70, E70). Native prints `mm`; the bind
  has no effect there.
- Native's verdict is not recorded for stale-provision-type (#4) or
  unbound-merge-representative (#3).

**Audit accepts; native rejects.**
- sameneed-copy.moss (#51). The oracle rejects it too, but both accept the
  same program with the merge orders agreeing.
- tests/errors/functor_output_input_changed.moss (#14). Audit accepted it
  after 94d89ce, and still does after fc75de7 (consolidation reading).
- const-kept-output.moss and const-family-unbound.moss (#20, #21). Native
  rejects every use of a defined val, which is unrelated to these changes.
- scratch/copyfresh/r3c-target-merge-ok.moss (#35, the known residual).
  The oracle prints 97, native rejects at the copy, and Audit accepts.
  copyfresh-AUDIT.md says this predates its change. On related shapes,
  where F's context merges a fixed input into a non-fixed representative,
  Audit can be stricter than the oracle.

**Run-time.** On these programs Audit's semantics, as the records
describe it, matches native and not the oracle:
- copy-extra-key (31);
- refile-stale-shadow (7);
- output-merge-key (7);
- functor_input_output_generations (`yn`);
- linked-copy-bracket (42);
- family-copy-linked-bracket (exit 0).

On copy-capture, Audit matches both implementations.

On sameneed-meaning.moss (#51), Audit prints 2, while native and the
oracle print 1. On sameneed-meaning-agree.moss, Audit and the oracle print
2 and native prints 1.

**Stale documentation.**
- The header of kept-output-escape.moss says MadeFor (dropped).
- The header of output-merge-key.moss describes `fc_roles` (superseded by
  callerkeys).
- The SAFETY-SITES entry "a copy's source with an extra result key" still
  says `CopyOutputs` checks the converse. A later entry records that the
  converse was reverted.

## Deferred alternatives

- **Parametricity proof instead of the kept-output clause (#10).**
  - Source: results2-AUDIT.md §2 and chainfwd-AUDIT.md §2.
  - Idea: show that re-grounding an unbound declared T is harmless.
    - The small part: such a T reaches a context only through family
      demands, and so occurs only in family method provisions.
    - The large part: those closures are parametric in the family's
      input. This needs a logical-relation lemma for closure safety
      under renaming an abstract generation, or family obligations
      restated to give callability at every grounding.
  - Estimate: "days, not hours". Superseded by option (b) (a mentioned
    output is fresh), which accepts the two programs without it.
- **Drop the redundant pin clause (#13).** Done in consolidation (see
  #13).
- **Pin merge representatives as the oracle does (#14, option a).** This
  is smaller, but it rejects m5-XT, which native runs.
- **3b over every unmerged entry (#11, option a).** It rejects the same
  shapes, but needs entry-level proofs. E36 suggests the oracle could
  adopt either check.
- **Keep b4f8279's freshness check (#20, option B).** This needs a new
  const semantics, a frame-dependent `MeaningsScoped`, and more. It would
  still need lambda-declared-output rejected.
- **Mirror the oracle's D20 assumed-requirement coherence (#8)** instead
  of `HandlesRead`. This is larger, and native does not check it either.
  (#57 now adds the oracle's check beside `HandlesRead`, which stays.)
- **Widen `w_reqs` for fns and methods, or compare signatures at family
  application (#12).** These were not chosen by the user.
- **Tolerate extra installed keys in `family_core` (#46).** This was
  rejected in favour of `MethodsDistinct`.
- **ExConst clearing type meanings (#21).** This would make the inlined
  const problem go away, but it needs a new expression form (about 19
  files and the oracle).
- **Oracle-side fixes for E31–E33.** These are not attempted. See the size
  warning above.
