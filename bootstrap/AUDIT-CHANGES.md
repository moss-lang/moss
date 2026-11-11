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
  (`moss_m2.exe`, `moss_ref`), the old oracle, since deleted (`moss-ref`,
  Audit's own implementation in `bootstrap/run/`, replaced it). "Native" means
  `moss run`. Proof files these entries name may have been deleted with it.
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
| 71 | A body may pin a fixed input that agrees with the bind site (`LinkBody` pins Π = P(F) ∩ M(F), `BFunClosure` agreement) | lang-q | relaxation toward both implementations (Audit used to reject every program binding `StdBase`) |
| 72 | **Every call checks its arguments in the caller's context Γ** (`ExCallD`, as `MCAttachedDefined` and `PrFamily`; semantics.typ Q4) | lang-bq | relaxation, with the oracle (accepts a program both implementations reject; native E72) |
| 73 | A linked body's result keys read a pinned fixed input at its pin (`link_site`; a key the pins re-read is one the body made) | pinsite | relaxation toward both implementations (completes #71: Audit still rejected every program binding `StdBase`) |
| 74 | Every declared context forms as written (`DeclsForm`; criteria section 7) | n234 | A for cycles; B for an unused context naming what is not in scope |
| 75 | Every symbol's assume nesting forms, whatever its kind (`AssumesComplete`; criteria section 7) | n234 | B |
| 76 | Every other declaration forms as written: tag payloads, alias bodies, val types and initializers, fn and method signatures, functor arguments and results (`DeclsForm`; criteria section 7) | n234 | B |
| 77 | A functor with a body binds its whole result, by written leaf, applied or not (`FunctorTotal`; D55) | n234 | A (applied or linked; mirrors both); B for an unapplied functor |
| 80 | **Type equations as context and assume items** (`AIEq`/`CIEq`, `IsEq`, the entry `NEq` and `Unifies`' merge conjunct; D90) | tyeq | language extension (new syntax; every existing program keeps its parse, dump and verdict) |
| 90 | **Every tag has a finite layout** (`FiniteLayouts`, D58/D59; on definitions, with no case for `Ref`) | layout | A for used types; B for an unused cyclic tag, with the oracle and native changed to match |
| 91 | Main's platform provisions are Platform's contract (`PlatformEntry` moves from `ElaboratesIn` to `Platform.Linked`) | layout | none (`Builds` and `Ready` are unchanged) |
| 92 | The language items go through one table (`lang_item`, `LangItem`) | layout | none (definitional) |
| 93 | Recorded: a body may not pin a fixed input that G takes (the oracle plan's N5; fixed in #95) | layout | a fidelity gap of #71's (Audit rejects what both implementations run) |
| 94 | Recorded: a platform constant of another type than `I32` is rejected (the oracle plan's N6) | layout | none (Audit already rejects; the executable elaborator's gap) |
| 95 | A linked body may pin a fixed input that G takes (`LinkBody`: protected inputs P(F) ∩ M(F), pins every fixed input the body rebinds; closes N5) | n5 | relaxation toward both implementations (closes #93's fidelity gap) |
| 96 | A copy reads its source as F does at every application of F (`CopyInterpretation`: H's merges, needs and result compared at the copy site's fixed readings; corrects #32) | copyfix | relaxation toward both implementations |
| 97 | Recorded, not fixed: needs at a site-made merge are matched by syntax (BFunCopy since #96; `LinkBody` as strict as #32 was) | linkfix | a coherence gap (acceptance depends on a merge's representative) and a fidelity gap; folded into the representative-free refactor |
| 98 | Recorded: a mixed fixed/polymorphic copy and pins into a merged input class | merge baseline | a coherence gap and a fidelity control |
| 99 | Recorded: unbound assumption requirements and a main context's symbol-only class | merge baseline | an open representative reading and a fidelity control |
| 100 | Main's root context and Bool's layout are language items (`LIRoot`, `LangItemsFormed`) | sep S0 | none (definitional regrouping) |
| 101 | Machine types and their classes use a table keyed by declaration symbol | sep S1 | representation change |
| 102 | Machine values carry scalar bits or a reference header | sep S2 | representation change |
| 103 | Formed contexts carry merge classes beside their spelling section | merge refactor M2 | representation change |
| 104 | Primitive code and the interface use named operations | sep S3 | representation change |
| 105 | Base's host laws are independent of the public interface record | BaseLaws | none (definitional refactoring) |
| 106 | Base owns primitive argument and allocation-result adaptation | sep S4a | none (definitional) |
| 107 | Primitive execution receives its site through the host boundary | sep S4b | none for public execution (ABI relocation) |
| 108 | Primitive method bindings record their receiver independently of the operation | sep S4c | representation change; public observations preserved |
| 109 | Integrate scalar/reference carriers with the checked host ABI and merge semantics | sep S2 integration | representation change |
| 110 | Integrate named operations with the checked host laws and site-erasure proofs | sep S3 integration | representation change |
| 111 | Static linking consumes Tables, Provides, and EntryABI | sep S5a | none (equivalent regrouping) |
| 112 | Public contracts consume one platform at build and run time | sep S5b | none for the Base specialization |
| 113 | Separate concrete Base definitions into AuditBase.v | sep S5c | none (relocation) |
| 114 | Rule B reads each structured dependency through its merge class | M4a | C |
| 115 | Separate call and application typing from translation | typing/translation | none (equivalent factoring) |
| 116 | Scope fresh identities in applications and functor copies | names | none (equivalent factoring) |
| 117 | Call instantiation respects merge classes | M4a | none (equivalent on formed contexts) |
| 118 | Fixed inputs and lambda annotations compare by merge class | M4b | none (equivalent on formed contexts) |
| 119 | Copy needs match at the binding site with an explicit ambiguity check | M4c | B and C |
| 121 | A copy reads an atomic class through its fixed members | M4c | C |
| 122 | Type forwarding providers before assigning runtime code | typing/translation | none (equivalent factoring) |
| 124 | Check a closure binding separately from the linked body | M4c | none (equivalent factoring) |
| 125 | Translate the checked provision choices of a closure binding | M4c | none (equivalent factoring) |
| 126 | Compare a closure’s shared inputs and needs at the binding site | M4c | C |
| 127 | Read directly bound private inputs from the closure’s capture | M4c | C |
| 128 | Rank generated code at its recorded origin | expansion soundness | C |
| 129 | Check raw handles and the common array-copy held type in executable platform linking | platform soundness | A for the executable |

Totals: 14 B, 1 C, 9 D, 17 A, 7 E (48 entries; #48 is after the range above). Entries from #49 on, and #10 option (b) and #12b, come from the proposals branch, except #50 (class A), from the coherence branch, #51, from the sameneed branch, #52 to #54, from the coherence2 branch, #55, from the call-brackets branch, #56, from the eagercopy branch, and #57 to #59, from the deltafix branch. #74 to #77 come from the n234 branch, #80 from the tyeq branch, #95 from the n5 branch, #96 from the copyfix branch, and #97 from the linkfix branch.

**Points that most need a decision.**
- #10 rejected two programs that both implementations accept and run
  correctly. Resolved by option (b): a mentioned output is fresh, and
  both are accepted again.
- #20 removes a run-time check from the user's own commit b4f8279.
- #14 makes Audit accept a `tests/errors` program that both implementations
  reject.
- #12 changed the oracle as well (Elab.v, ExecutionModel.v). It rejects
  `tests/wasi/partially_bound_merge.moss`, which native and Audit (#50) accept; the
  oracle fix is designed in #50.
- #13 rejects a control program that the oracle runs correctly.

---

## B: rejects programs some implementation accepts

### 1. Record patterns bind distinct names (08fc526)
- **Rule now.** `PRecord` requires the names a record pattern binds to be
  distinct (D34).
- **Why.** `Pair { a = y, b = y }` was typed by its last field and read
  from its first, so the reference interpreter could get stuck.
- **Tests.** [tests/errors/duplicate_field_binder.moss](../tests/errors/duplicate_field_binder.moss):
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
- **Tests.** [tests/errors/while_condition_break.moss](../tests/errors/while_condition_break.moss):
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
  - [tests/errors/unbound_merge_representative.moss](../tests/errors/unbound_merge_representative.moss):
    - oracle (at 386e418): accepts, then `stuck (XErr 10)`;
    - native: not recorded;
    - Audit: rejects.

    Since #12 the oracle rejects it too (D62).
  - [tests/wasi/partially_bound_merge.moss](../tests/wasi/partially_bound_merge.moss): the
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
- **Tests.** [tests/errors/stale_provision_type.moss](../tests/errors/stale_provision_type.moss):
  - oracle: accepts, then `stuck (XErr 10)`;
  - native: not recorded;
  - Audit: rejects.
- **Corpus.** None.
- **Decision.** User-approved.
- **Class B.** The oracle accepts the program. Follow-up: E25 (open for
  the oracle; native rejects since 3f91e25, lang-all).

### 5. Bound attached methods are read at the key's receiver (465622d)
- **Rule now.** For `bind Box.m[T=Bool] = Box.get;`, `MPAttached` reads the
  provider's `Satisfies`, `CallTheta` and signature with the receiver's
  arguments set as types, as a direct attached call does.
- **Why.** Under T = Char, `Box.get`'s need `T.k` was answered by
  `Char.k`, and then called on a Bool payload.
- **Tests.** [tests/errors/bracket_method_receiver.moss](../tests/errors/bracket_method_receiver.moss):
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
- **Tests.** [tests/errors/functor_body_break.moss](../tests/errors/functor_body_break.moss):
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
  - [tests/errors/captured_linked_fn.moss](../tests/errors/captured_linked_fn.moss): both
    implementations reject (D84 3b); Audit rejects.
  - [tests/errors/link_merge_captured_fn.moss](../tests/errors/link_merge_captured_fn.moss),
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
- **Tests.** [tests/errors/handle_rebind.moss](../tests/errors/handle_rebind.moss):
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
- **Tests.** [tests/errors/family_poly_nontype.moss](../tests/errors/family_poly_nontype.moss):
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
  - [tests/errors/lambda_declared_output.moss](../tests/errors/lambda_declared_output.moss):
    oracle exit 0; native exit 0;
  - [tests/errors/kept_output_family_method.moss](../tests/errors/kept_output_family_method.moss):
    oracle prints 0; native prints 0. Under (b), y has the fresh type T'
    and `y.dup()` is demanded from F at A := T'. An oracle variant that
    gives a mentioned output a fresh generation
    (`scratch/oracle/keptfresh`) prints 0 too, as does the plain oracle
    (`MODE=run` comparison: same output on both programs).

  The neighbour [tests/errors/kept_output_captured_fn.moss](../tests/errors/kept_output_captured_fn.moss)
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
  - [tests/errors/link_merge_captured.moss](../tests/errors/link_merge_captured.moss): oracle
    rejects (D84 3b); native rejects ("incompatible types"); Audit now
    rejects.
  - [tests/errors/link_merge_captured_fn.moss](../tests/errors/link_merge_captured_fn.moss):
    oracle accepts, then `stuck (XErr 14)`; native rejects; Audit rejects
    (this rule for X := Y, #7 for Y := X).
  - Neighbours in `scratch/cap3b/` (not committed) are listed in
    `cap3b-AUDIT.md`. One of them, n7b, is rejected by the oracle and
    accepted by native, and this rule rejects it as the oracle does.
- **Corpus.** `scratch/orch/cap3b-cmp.out`: 398 same, 2 diff. The
  differences are tests/errors/link_merge_captured.moss and its hunt original. The
  oracle rejects both.
- **Class B.** The oracle accepts tests/errors/link_merge_captured_fn.moss and runs it
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
  - [tests/wasi/sig_nonreq_shadow.moss](../tests/wasi/sig_nonreq_shadow.moss): the old
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
- **Class B.** It rejects tests/wasi/partially_bound_merge.moss, which native accepts
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
    [tests/wasi/fn_sig_nonreq.moss](../tests/wasi/fn_sig_nonreq.moss) prints 7, and
    [tests/errors/fn_sig_nonreq_rebind.moss](../tests/errors/fn_sig_nonreq_rebind.moss) (U
    rebound before `bind F;`) is rejected ("the available binding was
    provided at a different type instantiation: `F`").
  - **Tests.**

    | program | oracle before | oracle now | native | Audit now |
    |---|---|---|---|---|
    | [tests/wasi/fn_sig_nonreq.moss](../tests/wasi/fn_sig_nonreq.moss) | rejects (U not available) | 7 | 7 | accepts (reading) |
    | [tests/errors/fn_sig_nonreq_rebind.moss](../tests/errors/fn_sig_nonreq_rebind.moss) | rejects (U not available) | rejects (rule 5) | rejects | rejects (rule 5) |

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
    [tests/wasi/block_kept_output_pin.moss](../tests/wasi/block_kept_output_pin.moss) was
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
  | [tests/errors/link_kept_output_pin.moss](../tests/errors/link_kept_output_pin.moss) | accepts, then `stuck (XErr 14)` (E37) | rejects ("compilation failed: `T`") | rejects |
  | [tests/errors/link_kept_output_pin_control.moss](../tests/errors/link_kept_output_pin_control.moss) (caller's T = Char) | prints 48, 10 | rejects | rejects |
  | [tests/errors/link_kept_output_rep.moss](../tests/errors/link_kept_output_rep.moss) | rejects ("does not provide `X.m`") | rejects | rejects. Before: accepted for a world picking T, by hand reading |

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
  | [tests/errors/link_merge_rep_rebind.moss](../tests/errors/link_merge_rep_rebind.moss) | rejects ("does not bind `X.m`") | rejects | rejects (before: accepted, then stuck) |
  | [tests/wasi/link_merge_rep_rebind_both.moss](../tests/wasi/link_merge_rep_rebind_both.moss) (= m5-XT) | rejects in this order (D84 3d); accepts the other order and exits 0 | accepts both orders, exit 0 | accepts; selects G's `X.m` |
  | [tests/errors/functor_output_input_changed.moss](../tests/errors/functor_output_input_changed.moss) | rejects (D84 3d) | rejects | accepted after 94d89ce (rejected before). The record argues this is sound. Still accepted after fc75de7, by a consolidation reading |
  | [tests/errors/link_output_input_rep.moss](../tests/errors/link_output_input_rep.moss) | rejects | rejects | rejects (before fc75de7: accepted, by hand reading) |
  | [tests/wasi/link_output_input_rebind_control.moss](../tests/wasi/link_output_input_rebind_control.moss) | prints 7 | prints 7 | accepts |
  | [tests/wasi/output_merge_key.moss](../tests/wasi/output_merge_key.moss) | accepts | accepts, prints 7 | accepted after fc75de7 (the old rule rejected it) |

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
- **Tests.** [tests/wasi/wrapped_main.moss](../tests/wasi/wrapped_main.moss) with
  [tests/wasi/wrapped_main_root.moss](../tests/wasi/wrapped_main_root.moss): both
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
- **Tests.** [tests/wasi/copy_capture.moss](../tests/wasi/copy_capture.moss):
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
  - [tests/wasi/linked_merge_captured.moss](../tests/wasi/linked_merge_captured.moss);
  - [tests/wasi/bind_merge_unbound.moss](../tests/wasi/bind_merge_unbound.moss).
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
  - [tests/wasi/refile_stale_shadow.moss](../tests/wasi/refile_stale_shadow.moss): oracle
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
  - [tests/wasi/const_kept_output.moss](../tests/wasi/const_kept_output.moss): oracle exit
    0; native rejects the use of the val for an unrelated reason (it
    rejects every use of a defined val); old Audit stuck.
  - [tests/errors/lambda_declared_output.moss](../tests/errors/lambda_declared_output.moss):
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
  | [tests/errors/family_unbound_need.moss](../tests/errors/family_unbound_need.moss) | 0 | 0 |
  | [tests/errors/family_unbound_need_sig.moss](../tests/errors/family_unbound_need_sig.moss) | 0 | 0 |
  | [tests/errors/lambda_declared_output.moss](../tests/errors/lambda_declared_output.moss) | 0 | 0 |
  | [tests/errors/const_family_unbound.moss](../tests/errors/const_family_unbound.moss) | 0 | rejects (native rejects every defined-val use) |

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
- **Tests.** [tests/errors/functor_captured_unbound.moss](../tests/errors/functor_captured_unbound.moss):
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
     [tests/wasi/copy_extra_key.moss](../tests/wasi/copy_extra_key.moss), which native
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
  | [tests/wasi/copy_extra_key.moss](../tests/wasi/copy_extra_key.moss) | prints 21 (E32) | prints 31 | accepted; 31 |
  | [tests/wasi/linked_copy_bracket.moss](../tests/wasi/linked_copy_bracket.moss) | `stuck (XErr 6)` | prints 42 | the call finds the primitive |
  | [tests/wasi/family_copy_linked_bracket.moss](../tests/wasi/family_copy_linked_bracket.moss) | `stuck (XErr 6)`, exit 3 | exit 0 | the demand finds the key |
  | [tests/wasi/output_merge_key.moss](../tests/wasi/output_merge_key.moss) | `stuck (XErr 6)` (E33) | prints 7 | as native |
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
- **Tests.** [tests/errors/captured_merge.moss](../tests/errors/captured_merge.moss): both
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
  - **Test.** [tests/errors/family_ambiguous_member.moss](../tests/errors/family_ambiguous_member.moss):
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
  - [tests/errors/linked_merge.moss](../tests/errors/linked_merge.moss);
  - [tests/errors/copy_merge.moss](../tests/errors/copy_merge.moss).
- **Corpus.** A variant with these checks gives the oracle's verdict on all
  353 programs. The record does not name the output file.
- **Class A** for `LinkBody`. **Corrected by #96 for `BFunCopy`:** the copy
  clause compared H's readings in F's declared context, which is stricter
  than both implementations. It rejected a copy whose merge F's fixed
  inputs make true (tests/wasi/copy_site_merge.moss), which the oracle and native
  accept and run.

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
- **Superseded by #96**, which folds `CopyOutputs` into `CopyInterpretation` and reads H's result in H's own context.
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
  - [tests/errors/copy_output_order.moss](../tests/errors/copy_output_order.moss): native's
    test is `copied_functors_compare_final_output_interpretations`;
  - [tests/errors/copy_bracket_nonfixed.moss](../tests/errors/copy_bracket_nonfixed.moss);
  - [tests/errors/copy_fixed_reads_poly.moss](../tests/errors/copy_fixed_reads_poly.moss):
    native rejects at the use of F;
  - [tests/errors/copy_fresh_collide.moss](../tests/errors/copy_fresh_collide.moss).

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

  The header of [tests/errors/kept_output_escape.moss](../tests/errors/kept_output_escape.moss)
  still says "MadeFor".
- **Why.** A value of an output's type could leave its block (D78), and
  `MeaningsGrow` was false.
- **Tests.** Both programs are rejected by the oracle ("a type `bind F;`
  made in it ... (D78)"), by native ("cannot escape its block") and by
  Audit:
  - [tests/errors/output_escape.moss](../tests/errors/output_escape.moss);
  - [tests/errors/kept_output_escape.moss](../tests/errors/kept_output_escape.moss).
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
- **Tests.** [tests/wasi/result_output_meaning.moss](../tests/wasi/result_output_meaning.moss):
  both implementations print 1; Audit had rejected it and now accepts it.
- **Class A, as the closest fit.** It mirrors both implementations, but it
  is a relaxation, not a rejection. It accepts nothing known that an
  implementation rejects.

### 39. Main reads a platform functor's fixed machine types as themselves (be77015)
- **Rule now.** `Platform.Entry`'s `IFun` case requires
  `FixedMachinesRead`: each machine type among the functor's fixed
  readings in main's context reads canonically as itself.
- **Why.** The proof needed this, as the premise `MainMachinesUnmerged`.
- **Tests.** [tests/errors/platform_fixed_merge.moss](../tests/errors/platform_fixed_merge.moss):
  - oracle: rejects ("incompatible assumed fixed type",
    `check_platform_entry_view`);
  - native: rejects;
  - Audit: rejects.
- **Class A.**

### 40. A copy's target is abstract (cd1108d)
- **Rule now.** `BFunCopy` requires F to be declared without a body.
- **Why.** `ChainForwardCopySigs` needs every symbol of a copy's result to
  be an input, which holds only for an abstract functor.
- **Tests.** [tests/errors/copy_into_body.moss](../tests/errors/copy_into_body.moss): the
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
- **Tests.** [tests/wasi/platform_family_keys.moss](../tests/wasi/platform_family_keys.moss):
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
- **Tests.** tests/errors/expand_*.moss and tests/wasi/expand_control_loop.moss (from bootstrap/examples/expand-*.moss). Rejected: polyrec,
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
  | [tests/wasi/block_pin_rebind.moss](../tests/wasi/block_pin_rebind.moss) (rebind in a nested block) | 7 | 7 | rejects | accepts |
  | [tests/errors/block_pin_rebind_top.moss](../tests/errors/block_pin_rebind_top.moss) (same rebind at the body's top) | 7 | rejects ("compilation failed: `T`") | rejects | rejects |

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
  | [tests/errors/principal_theta_unbound.moss](../tests/errors/principal_theta_unbound.moss) (main binds `Concrete.foo`, calls a fn needing `T.foo` with T unbound) | rejects ("needs `T.foo`, which is not available") | rejects ("not available in the current context: `foo`") | accepts (θ T = Concrete) | rejects |
  | [tests/wasi/partially_bound_merge.moss](../tests/wasi/partially_bound_merge.moss) (f merges X with Y; the caller binds X only) | rejects (D62); the oracle fix below prints `k` | prints `k` | accepts (θ Y = Char, forced by the merge clause) | accepts (θ Y read through X) |
  | [tests/errors/unbound_merge_representative.moss](../tests/errors/unbound_merge_representative.moss) | rejects | (not recorded) | rejects | rejects |

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
  | [tests/wasi/sameneed_link.moss](../tests/wasi/sameneed_link.moss) (F links X.m, vx, A.gimme; opposite orders) | rejects (D84 3b: "captures `X.m`") | 7 | depends on the orientation: accepts when F and G choose the same representative, else rejects (linked provisions; 3b) | 7, every orientation |
  | [tests/wasi/sameneed_link_agree.moss](../tests/wasi/sameneed_link_agree.moss) | 7 | 7 | as above | 7 |
  | [tests/wasi/sameneed_meaning.moss](../tests/wasi/sameneed_meaning.moss) (X fixed; X.m rebound between bind and application) | 1 (captured) | 1 | accepts with two meanings: 2 (linked) or 1 (captured), by orientation | 2, every orientation |
  | [tests/wasi/sameneed_meaning_agree.moss](../tests/wasi/sameneed_meaning_agree.moss) | 2 | 1 | 2 or 1, by orientation | 2 |
  | [tests/wasi/sameneed_copy.moss](../tests/wasi/sameneed_copy.moss) (`bind F = H;`, opposite orders) | rejects (D78: "needs `X.m`, which `F` does not take") | rejects ("does not satisfy the required signature: `X`") | depends on the orientation (`incl nsh nsu`; provisions) | 7, every orientation |
  | [tests/wasi/sameneed_copy_agree.moss](../tests/wasi/sameneed_copy_agree.moss) | 7 | 7 | as above | 7 |
  | [tests/errors/sameneed_ambiguous.moss](../tests/errors/sameneed_ambiguous.moss) (G merges X, Y; F does not, and needs X.m and Y.m) | rejects ("linked input interpretation differs") | 7 | rejects (#11) | rejects (#11) |
  | [tests/errors/sameneed_ambiguous_other.moss](../tests/errors/sameneed_ambiguous_other.moss) | 7 | 7 | rejects (#11) | rejects (#11) |

  Native prints 1 on tests/wasi/sameneed_meaning_agree.moss and also when no merge
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
- **Class C and D, a relaxation.** tests/wasi/sameneed_copy.moss is accepted although
  both implementations reject it (they reject only this merge order, so
  coherence decides, criteria.md section 6). tests/wasi/sameneed_meaning.moss's
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
  | [tests/errors/expand_merged_xy.moss](../tests/errors/expand_merged_xy.moss) (nest merges X, Y; polymorphic recursion through the class) | prints 5 | crashes at compile time (call stack exhausted) | rejects | rejects |
  | [tests/errors/expand_merged_yx.moss](../tests/errors/expand_merged_yx.moss) (the same, merge listed the other way, so the oracle spells the class by Y) | prints 5 | crashes at compile time | rejects | rejects |
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
  | [tests/errors/platform_fixed_merge_xi.moss](../tests/errors/platform_fixed_merge_xi.moss) (`context Two = X, i32_eqz[I32 = X], i32_eqz`) | rejects ("incompatible assumed fixed type") | rejects ("incompatible types: `Arrays`") | clause passes with representative I32, fails with X (argued) | rejects |
  | [tests/errors/platform_fixed_merge_ix.moss](../tests/errors/platform_fixed_merge_ix.moss) (the same merge, the other order) | rejects | rejects | the same | rejects |
  | [tests/errors/platform_fixed_merge.moss](../tests/errors/platform_fixed_merge.moss) (I32 = Bool, #39) | rejects | rejects | rejects | rejects |

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
  | [tests/errors/closure_partial_class_xyz.moss](../tests/errors/closure_partial_class_xyz.moss) (X = Bool and Y = Char at the bind, Z unbound) | rejects ("linked input interpretation differs") | prints k | rejects (LinkBody forces representative X; Y = X fails) | accepts |
  | [tests/wasi/closure_partial_class_zyx.moss](../tests/wasi/closure_partial_class_zyx.moss) (the same, merge listed the other way) | prints k | prints k | rejects | accepts |
  | [tests/errors/closure_partial_unbound_xyz.moss](../tests/errors/closure_partial_unbound_xyz.moss) (X unbound at the bind) | rejects | prints k | accepts | accepts |
  | [tests/wasi/closure_partial_unbound_zyx.moss](../tests/wasi/closure_partial_unbound_zyx.moss) | prints k | prints k | accepts | accepts |

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
  | [tests/errors/expand_call_brackets.moss](../tests/errors/expand_call_brackets.moss) (`nest[T=Wrap](Wrap(x), n - 1)` inside `nest`) | accepts, prints 5 | rejects: expanding cycle, nest's T grows into nest's T | crashes at compile time (wasm call stack exhausted), as on expand-polyrec | rejects (`InstantiationsBounded`: the strict edge (nest, T) -> (nest, T), by reading) |
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
  | [assumed-ctx-item](../tests/errors/assumed_ctx_item.moss), [assumed-main-calls](../tests/errors/assumed_main_calls.moss), [assumed-type-abstract](../tests/errors/assumed_type_abstract.moss), [assumed-type-use](../tests/errors/assumed_type_use.moss), [assumed-val-type](../tests/errors/assumed_val_type.moss) (a requirement missing) | rejects | rejects | accepts | rejects |
  | [assumed-meth-need](../tests/errors/assumed_meth_need.moss), [assumed-need-use](../tests/errors/assumed_need_use.moss), [assumed-val-need](../tests/errors/assumed_val_need.moss) (a need missing) | rejects | rejects | accepts | rejects |
  | [assumed-need-incoherent](../tests/errors/assumed_need_incoherent.moss) (f needs h at T = U; g has h at T = T) | rejects (D62) | accepts | accepts | rejects |
  | `tests/wasi/assume_requirements_same_list.moss` (`assume f, seed, T`) | accepts | accepts | accepts | accepts |
  | [assumed-bracket-ok](../tests/wasi/assumed_bracket_ok.moss) (`f[T=U]` in a context) | accepts | accepts | accepts | accepts |
  | [assumed-ctx-decl-only](../tests/wasi/assumed_ctx_decl_only.moss) (an unassumed context lists f) | accepts | accepts | accepts | accepts |
  | [assumed-functor-input](../tests/wasi/assumed_functor_input.moss) (F's input f) | accepts | accepts | accepts | accepts |
  | [assumed-need-coherent](../tests/wasi/assumed_need_coherent.moss) | accepts | accepts | accepts | accepts |

  The tests come from the Δ3 hunt (scratch/deltas, `d3_*`).
- **Corpus.** The oracle has the check, so the comparison is the oracle
  against a variant without it (`check_assumes` skipped in the extracted
  `elab_program`; scratch/variants/noassume), `--elab-only`, over
  `tests/`, `examples/` and `bootstrap/examples/` (with the new tests):
  434 same / 13 diff. The 13 are the 12 rejected programs of the table
  and [tests/errors/handle_rebind.moss](../tests/errors/handle_rebind.moss) (#8), which Audit
  already rejected through `HandlesRead`; the oracle's reason for
  rejecting it (i64_store needs Memories at another instantiation) is
  now Audit's too. No program both implementations accept is rejected.
  Audit's verdict changes on the 12 (accept to reject), and on no other
  corpus program by this comparison.
- **Proof.** `public_safety` unchanged: the conjunct is last and no proof
  reads it. `AuditBuildExample.main_assumes` proves it of the example
  world (main is the only symbol and its nesting is empty).
- **Class A**, except tests/errors/assumed_need_incoherent.moss, which native accepts
  (class B on that program). Native accepted it only because g never calls
  f; it did not check an assumed item's needs' instantiations. Fixed
  natively in f8c380c (lang-all).

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
  tests/errors/result_collide_family.moss).
- **Form chosen.** A plain `NoDup` of the caller's keys would reject a key F
  itself repeats (A.m, and a context listing A.m), which all three
  accept and run alike (tests/wasi/result_repeat_in_f.moss). The check compares with
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
  | [result-collide-ab](../tests/wasi/result_collide_ab.moss) | a | rejects | b | b | rejects |
  | [result-collide-ba](../tests/errors/result_collide_ba.moss) | b | rejects | b | a | rejects |
  | [result-collide-ab-bindswap](../tests/errors/result_collide_ab_bindswap.moss) | a | rejects | a | b | rejects |
  | [result-collide-ba-bindswap](../tests/errors/result_collide_ba_bindswap.moss) | b | rejects | a | a | rejects |
  | [result-repeat-in-f](../tests/wasi/result_repeat_in_f.moss) (control) | a | a | a | a | a |
  | [result-collide-family](../tests/errors/result_collide_family.moss) (control) | rejects | rejects | rejects | rejects | rejects |
  | [copy-result-collide](../tests/errors/copy_result_collide.moss) | a | rejects | rejects ("the functor does not satisfy the required signature") | b | rejects |
  | [copy-result-collide-hba](../tests/errors/copy_result_collide_hba.moss) | b | rejects | rejects | a | rejects |
  | [copy-result-collide-mba](../tests/errors/copy_result_collide_mba.moss) | a | rejects | rejects | b | rejects |
  | [copy-result-repeat](../tests/wasi/copy_result_repeat.moss) (control) | a | a | a | a | a |

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
  | [assume-method-before-type](../tests/errors/assume_method_before_type.moss) (`assume T.m, T`) | rejects | **accepts** (E41) | accepts | rejects |
  | [assume-method-before-type-use](../tests/errors/assume_method_before_type_use.moss) (and g calls `x.m()`) | rejects | rejects ("no matching method is available for this receiver") | rejects (no m at x's type) | rejects |
  | [assume-method-after-type](../tests/wasi/assume_method_after_type.moss) (`assume T, T.m`, control) | accepts | accepts | accepts | accepts |

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
  the same program once g uses the method. E41 is fixed natively
  (4071c84, lang-all).

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
  declared symbol out of force (tests/errors/family_unbound_need.moss), a lambda or
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
    rejects tests/wasi/bind_merge_unbound.moss and tests/wasi/closure_partial_class_zyx.moss,
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
  | [ruleb-call-unbound](../tests/errors/ruleb_call_unbound.moss) | rejects | accepts | rejects | accepts (E42) |
  | [ruleb-provider-unbound](../tests/errors/ruleb_provider_unbound.moss) | rejects | accepts | rejects | accepts (E42) |
  | [ruleb-closure-unbound](../tests/errors/ruleb_closure_unbound.moss) | rejects | accepts | rejects | accepts (E42) |
  | [ruleb-closure-bound](../tests/wasi/ruleb_closure_bound.moss) (control) | accepts | accepts | accepts | accepts |
  | [ruleb-merge-family-demand](../tests/wasi/ruleb_merge_family_demand.moss) | accepts | rejects (D62) | k | k |
  | [partially-bound-merge](../tests/wasi/partially_bound_merge.moss) | accepts | rejects (D62, E24) | k | k |
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
  assume list: tests/errors/merge_inst_order_ab.moss was accepted and
  tests/errors/merge_inst_order_ba.moss rejected, by Audit, the oracle and native.
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
  | [merge-inst-order-ab](../tests/errors/merge_inst_order_ab.moss) | accepts | rejects | accepts | rejects (A and B merged) | accepts (E43) |
  | [merge-inst-order-ba](../tests/errors/merge_inst_order_ba.moss) | rejects | rejects | rejects (D62) | rejects (A and B merged) | rejects |
  | [merge-inst-val](../tests/wasi/merge_inst_val.moss) (v's type does not mention T) | rejects | accepts | rejects | accepts | rejects (E43) |
  | [merge-inst-fn](../tests/wasi/merge_inst_fn.moss) | rejects | accepts | rejects | accepts | rejects (E43) |
  | [merge-inst-sig](../tests/wasi/merge_inst_sig.moss) (control) | accepts | accepts | accepts | accepts | accepts |
  | [merge-inst-nomerge](../tests/wasi/merge_inst_nomerge.moss) (control) | accepts | accepts | accepts | accepts | accepts |

- **Corpus.** The oracle at c89de1f against the oracle with #60 and #61,
  `--elab-only`, over `tests/`, `examples/`, `bootstrap/examples/` and
  `src/main.moss`: 451 same / 10 diff, all from #60 (the six edge-case
  tests, partially-bound-merge, and three changed messages); #61 changes
  no corpus verdict (besides its own tests above).
- **Class B and C.** It rejects tests/errors/merge_inst_order_ab.moss, which both
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
  | [bind-attached-defined](../tests/errors/bind_attached_defined.moss) | mm | rejects | mm | accepts | rejects |
  | [bind-attached-detached-ok](../tests/wasi/bind_attached_detached_ok.moss) (control: the detached method through an abstract Q) | mxm | mxm | mxm | accepts | accepts |
  | [bind-attached-abstract-ok](../tests/wasi/bind_attached_abstract_ok.moss) (control: an abstract attached method) | x | x | x | accepts | accepts |

- **Corpus.** The oracle with the check (`scratch/lang-q/oracle-q3.exe`)
  against the oracle at c89de1f, `--elab-only`, over `src/main.moss`,
  `tests/`, `examples/` and `bootstrap/examples/` (with the new tests):
  463 same / 1 diff, the diff being bind-attached-defined. So no real code
  binds a defined method.
- **Proof.** `public_safety` unchanged: the premise is BMethBind's last,
  and no proof reads it.
- **Class B** (native accepts bind-attached-defined; E70 below).

### 71. A body may pin a fixed input that agrees with the bind site (`LinkBody`, `BFunClosure`; lang-q branch; semantics.typ Q6)
- **Rule now.**
  - `LinkBody`'s pins are Π = P(F) ∩ M(F), plus the fixed inputs G takes
    itself: `In x (fr_pins Fg) <-> Mentions W u x /\ (Poly W u x \/ In x (map fst (c_ty Cg)))`.
    Before, Π was all of M(F).
  - So a body may now bind a fixed input of F that G does not take. Such
    a bind is a *pin*. `LinkBody` records the pins as a new index:
    `LinkBody W P u gg h pins`, where `(x, tx) ∈ pins` iff x is fixed
    in F, G does not take x, and the body leaves x meaning tx. A pin's type
    may mention only G's own inputs, each its own merge class and none made
    by the body, and any of them that is an input of F must be fixed in F.
  - `ResultRealizes` reads a pinned input at its pin: the reads are
    `role_reads … ++ pin_reads Cb pins`. A body that pins `Tok = Char`
    provides `tok(): Tok` as `tok(): Char`.
  - `BFunClosure` checks each pin against the bind site: the pinned
    input's meaning here equals the pin read with G's types as they are
    here, and every type the pin mentions is in force here. This is the
    oracle's check (`bind_functor`, "it binds `Tok` to …, and here `Tok`
    is …").
- **Why the agreeing pin is safe, and the disagreeing one is not.**
  `MakeTok[]` is fixed in `Tok`. Binding `MakeTok = CharTok` records
  what `Tok` means at the bind site. Every later application of
  `MakeTok` must see the same meaning (rule 5), or it is rejected
  (08c-fixed-stale). So everywhere `tok()` can be called through
  `MakeTok`, its type `Tok` means what it meant at the bind.
  - CharTok's body says `bind Tok = Char`, and its `tok` returns a
    character. In pin-fixed-agree, the bind site has `Tok = Char`, so the
    callers read `tok(): Char`, which is what the body provides.
  - In pin-fixed-disagree, the bind site has `Tok = Bool`. The callers
    then read `tok(): Bool`, but the body still returns a character.
    That would read a `Char` as a `Bool`, so the bind is rejected.
  - The pin is a promise that the body is only used where `Tok` means
    `Char`. Fixed inputs make "where it is used" the same as "where it
    was bound", so checking the promise once, at the bind, is enough.
  - A polymorphic input has no such guarantee: each application supplies
    its own meaning. So a pin of one is still rejected (pin-poly).
- **Criteria:** safety (section 1; proved, below), stability (section 5:
  a fixed input keeps its bind-site meaning, and the pin is checked
  against it), and fidelity (section 6: both implementations accept the
  agreeing pin and reject the other two tests).
- **Tests.**

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | [pin-fixed-agree](../tests/wasi/pin_fixed_agree.moss) (thesis 08b-fixed) | q | q | rejects | accepts (q) |
  | [pin-fixed-disagree](../tests/errors/pin_fixed_disagree.moss) (main binds `Tok = Bool` first) | rejects ("it binds `Tok` to `Chr[I32=I32]`, and here `Tok` is `Bool`") | rejects ("the functor does not satisfy the required signature: `Tok`") | rejects | rejects |
  | [pin-poly](../tests/errors/pin_poly.moss) (`MakeTok[Tok]`) | rejects ("the fixed set is not widened per binding") | rejects ("compilation failed: `Tok`") | rejects | rejects |

  Audit verdicts are argued from the rules, not run.
- **Corpus.** The oracle already has this rule (pins P(F) ∩ M(F) less
  X(F), and the agreement check), so no oracle verdict changes. To see
  which programs the change affects, an oracle variant that fails at
  every pin of a fixed input (`scratch/lang-q/oracle-pinfix.exe`) was
  compared with the oracle over `src/main.moss`, `tests/`, `examples/`
  and `bootstrap/examples/`. The result was 268 same and 203 diff:
  - 201 of the diffs are the standard library. `lib/wasistd.moss`'s
    `GcArrays`, bound to the family `Arrays` in `WasiRawStd`, pins
    `size::Size = Num` and `array::ArrayHandle = ArrayRef`, both fixed
    inputs of `Arrays`. So **before this change Audit rejected every
    program that binds `StdBase`**, including 08b-fixed. With this change
    those pins are allowed: they mention only types `GcArrays` takes
    (`Num`'s and `ArrayRef`'s Wasm types), and they agree with the bind
    site.
  - The other 2 diffs are tests/wasi/functor_in_result_pin.moss and
    functor_pin_through_input.moss.
- **What is still rejected.**
  - A pin of a fixed input that G takes itself.
  - A pin whose type mentions a type G does not take, a merged type, or a
    type the body makes.
  - A pin whose types are not in force at the bind site (the oracle's
    `ety_of` at the bind site fails there too).
- **Gap, fixed in #73.** Result *keys* were still read at G's table, so
  `GcArrays`'s `Array.get` (keyed through the pinned `ArrayHandle`) was
  "not provided", and Audit still rejected every program binding
  `StdBase`.
- **Proof.** `public_safety` is still proved, and `check.sh` is green.
  - `ClosureOk`'s `CLink` keeps the pins, with `PinsAgree u Cu Cg dc gfx pins`:
    under any application's meanings (`AppMeanings`), each pin means what
    the closure's fixed reading gives the pinned input.
    `case_BFunClosure` proves this from the new agreement check.
    `cmeaning_capture` covers the captured types the pin mentions, and
    `FixedMeans` covers the fixed ones.
  - At an application, `PublicLinkRun.link_own_meaning` reads a pin's
    types in the body's final environment. A linked type gets θ's reading,
    and a captured one gets the closure's. With the chain's fixed
    readings (`chain_fixed`, `FixT`), this gives each pin the caller's
    reading of the pinned input.
  - `root_level` (PublicChainRealize) reads a pinned input through its
    pin (`OkG2`). `r2` allows a pinned input in the promise.
    `family_link_new_ok` (PublicFamilyLink) does the same for a family's
    on-demand application (`R2sig`, `GE`, `PinRun`).
  - The obligation statements that carry a root's `LinkData`
    (`ChainRealizeOk`, `ChainNewOk`, `ChainCoreOk`, `ChainResultsOk`,
    `LinkResultsOk`, `FamilyLinkResultsOk`, `FamilyLinkNewOk`) gain its
    `pins` and that closure's `PinsAgree`. `PlatformNotLinked` takes the
    new index. All of them are still proved, in the same files.
- **Class.** A relaxation toward both implementations (C/none), and the
  agreement check mirrors the oracle. Native needs no change on these
  tests.

### 72. Every call checks its arguments in the caller's context (`ExCallD`; lang-bq branch; semantics.typ Q4, owner-approved "Γ everywhere")
- **Rule now.** A call's arguments are checked in the caller's context Γ,
  never in Γ ⊕ D, the context with the families the call demands in
  force. `ExCallD` (a defined fn) now checks `Args W P F C args ...` in
  the caller's `C`, not in `C'` (after `Demand`), as `MCAttachedDefined`
  (a defined attached method) and `PrFamily` (a method a family supplies
  on demand) already did. The callee's needs, signature and θ are still
  read in `C'`.

  | call | rule | arguments before | arguments now | native |
  |---|---|---|---|---|
  | a defined fn | `ExCallD` | Γ ⊕ D (`C'`) | Γ (`C`) | Γ ⊕ D (E72) |
  | a defined attached method | `MCAttachedDefined` | Γ | Γ | Γ ⊕ D (E72) |
  | a method a family supplies on demand | `PrFamily` | Γ | Γ | Γ |
  | an abstract fn, a provided method | `ExCallA`, `PrIn` | Γ (D is empty) | Γ | Γ |

- **Why** (the owner's decision: no "spooky action at a distance").
  `f(e)` means what `let a = e; f(a)` means: the argument is evaluated in
  the caller's terms, before the call and before what the call demands.
  What a call demands is the callee's business, and should not change
  what the caller's own expressions mean.
- **Criteria.**
  - Stability under rebinding (section 5): a family application that a
    call demands installs the family's whole result, so in Γ ⊕ D it could
    shadow what the caller itself provides
    ([args-context-shadow-call](../tests/wasi/args_context_shadow_call.moss):
    F's `Int.m`, read with U = Bool, shadowed the caller's `Int.m`). In Γ a
    demand cannot rebind anything an argument reads.
  - Locality: an argument's meaning depends only on the context it is
    written in, not on the needs of the callee it is passed to. D77
    already says that a family that would supply a key on demand is not a
    provision of it; with Γ ⊕ D an argument would see the demanded
    provision anyway
    ([args-context-provision-wins](../tests/wasi/args_context_provision_wins.moss)).
  - The D89 analogy (#55): a bracket call `f[A=T](e)` is
    `{ let %arg0 = e; bind A = T; f(%arg0) }`, its arguments evaluated
    before the binds. A call's demands are binds the call makes on the
    caller's behalf, so its arguments come first in the same way.
  - Coherence (section 2): before, `f(d.describe())` and
    `d.g(d.describe())` read the same argument differently (xm) because
    of which rule checked the call. Now every kind of call agrees.
- **Proof.** `PublicBodySlice.call_core` and `call_code` take the
  arguments' context `CA` separately from the callee's `C`, with a `Pre`
  for each; the defined-call case of `ex_code` passes `C'` for the callee
  and `C` for the arguments (`demand_case` gives both). `public_safety`
  is still proved and `check.sh` is green. This is lang-q's
  `scratch/lang-q/q4-gamma.patch`, adapted to rule B (#60): `ExCallD`
  keeps `RequirementsInForce` and its demand reads needs through θ.
- **Oracle.** `Elab.synth_call`, defined fn: `check_args` runs in the
  caller's `e`, not in the env after `principal_env` and `demand_needs`
  (the needs, signature, `layout_ok` and θ still read that env), as
  `synth_method` already did for a defined attached method. The oracle's
  specification (ExecutionModel.v) changes the same way: `ExCallD` and
  `ExCallDDemand` check `Args` in the caller's `C`, not in the principal
  `Cp` or the demand context `C'` (as `MCAttachedDefinedDemand` already
  did). The soundness proofs `CallDefinedCore`, `CallDefined`,
  `CallDefinedAny` and `CallDefinedDemand` follow; in the demand case the
  parameters read in the caller's env equal those read after the demand
  (`canon_uf`, `demand_types`).
- **Tests.**

  | program | Audit before | Audit now | oracle before | oracle now | native |
  |---|---|---|---|---|---|
  | [args-context-provision-wins](../tests/wasi/args_context_provision_wins.moss) | xm | mm | xm | mm | xx (E72) |
  | [args-context-shadow-call](../tests/wasi/args_context_shadow_call.moss) | rejects | 42 | rejects | 42 | rejects (E72) |
  | [args-context-family-call](../tests/wasi/args_context_family_call.moss) | m | m | m | m | m |
  | [args-context-alone](../tests/wasi/args_context_alone.moss) (control) | m | m | m | m | m |
  | [sig-nonreq-shadow](../tests/wasi/sig_nonreq_shadow.moss) (the method-call form) | 42 | 42 | 42 | 42 | 42 |
  | [functor-captured-unbound](../tests/errors/functor_captured_unbound.moss) | rejects (rule B, #60) | rejects | rejects (rule B) | rejects | exit 0 (E42) |

  Audit's verdicts are by reading the rules; the oracle's are run.
  lang-q found that Γ everywhere rejected functor-captured-unbound, which
  both implementations then accepted (its argument block binds a closure
  that captures a family method only the call's demand puts in force).
  Since rule B (#60) that program is rejected before its arguments are
  checked (the call needs `T` in force), so Γ rejects no program that
  either implementation accepts. The alternative, Γ ⊕ D everywhere
  (`scratch/lang-q/q4-gamma-plus-D.patch`), rejects sig-nonreq-shadow,
  which both implementations accept.
- **Corpus.** The oracle with this change against the oracle before it
  (lang-bq after Q3 and Q6), over `src/main.moss` (with `lib/`), `tests/`,
  `examples/` and `bootstrap/examples/`, `ulimit -s unlimited`:
  `--elab-only` 481 same / 1 diff (args-context-shadow-call, now
  accepted); run 480 same / 2 diff (args-context-shadow-call: 42;
  args-context-provision-wins: xm to mm). No program is newly rejected.
- **Class.** A relaxation (it accepts args-context-shadow-call, which both
  implementations rejected) and a run-time change on
  args-context-provision-wins, with the oracle. Native needs the same
  change (E72).

### 73. A linked body's result keys read a pinned fixed input at its pin (`link_site`, `LinkBody`; pinsite branch; completes #71)
- **The gap.** #71 let a body pin a fixed input of F that G does not
  take, and read the promised *types* at the pin (`pin_reads`). But
  `LinkBody` still read the promised *keys* at `link_site`, whose table
  was the output roles and G's own table (`c_ty Cg`). A pinned input is
  in neither, so a key's receiver read it as itself. `lib/wasistd.moss`'s
  `GcArrays`, bound to the family `Arrays`, pins `ArrayHandle = ArrayRef`
  and binds `Array.get` at `Array[ArrayHandle=ArrayRef, …]`. Arrays' key
  read as `Array[ArrayHandle=ArrayHandle, …].get`, which the body does not
  provide. So Audit still rejected every program that binds `StdBase`,
  `examples/hello.moss` included. The executable elaborator (branch
  `elab`, `--frontend audit`) found this: `link Arrays = GcArrays: … the
  body does not make F's result item Array[Item=…, ArrayHandle=…].get
  (not provided)`.
- **Rule now.**
  - `link_site Cg Cb pins outs gen` takes the pins. Its table is the
    roles, then the pins (each pinned input at the body's meaning of it),
    then `c_ty Cg`. These domains are disjoint (roles are fresh, and a pin
    is an input G does not take), so the order does not matter. So the
    promised key is read where the body bound it, and so are the run-time
    result keys in `FunctorCode` (the application's sources).
  - `LinkBody` gains one premise: a key that the pins re-read
    (`at_site (link_site … pins …) k <> at_site (link_site … [] …) k`)
    is none of G's own provisions (`ilook … (c_provisions Cg) = None`).
    So it is an item the body made, as both implementations require:
    they check a body's result by written leaf ("does not bind").
- **Why it is safe.** The pins are exactly the inputs whose meaning a
  body may fix and still have checked. `BFunClosure` checks that each pin
  agrees with the bind site, and a fixed input means at every
  application what it meant at the bind (rule 5). So the caller's key
  for the item, `Array[ArrayHandle=<the caller's ArrayHandle>, …].get`,
  is the one the body filed it under, with the pin's reading. Other
  inputs still read at G's table, so #14's reason for `link_site` (a
  rebind in the body writes only its alias) is untouched: a pin is not a
  rebind of an input G takes.
- **Criteria.**
  - Fidelity (section 6): both implementations accept pin-fixed-key and
    the whole prelude. Both reject pin-fixed-key-need, and the extra
    premise keeps Audit rejecting it: without it, reading through the pin
    alone would accept a result item that is only G's own captured need.
  - Stability (section 5): the key is read at the meaning that the
    fixed-input agreement and BFunClosure's pin check fix at the bind.
    It is not read at a later rebind.
- **Tests.**

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | [pin-fixed-key](../tests/wasi/pin_fixed_key.moss) (the prelude's pattern without the prelude; a family demand, then `bind Fam;`) | exit 0 | exit 0 | rejects (the key is not provided) | accepts |
  | [pin-fixed-key-need](../tests/errors/pin_fixed_key_need.moss) (the pin re-reads the key onto G's own need) | rejects ("functor `NeedGet` does not bind `Box.get`") | rejects ("the functor does not bind every item its result promises: `get`") | rejects (not provided) | rejects (the new premise) |
  | [pin-fixed-agree](../tests/wasi/pin_fixed_agree.moss) (#71; binds `StdBase`) | q | q | rejects (GcArrays) | accepts |

  The Audit verdicts are argued from the rules. They are confirmed by the
  executable elaborator (moss-elab 41b4e3c). Its faithful mode
  (`--frontend audit`, Audit before this change) rejects pin-fixed-key
  and pin-fixed-agree with the error above. Its `audit-compat` mode,
  which implements exactly this change (`s_compat_pins` in
  `EaFunctor.link_body`: the pins ahead of `link_site`'s table, and the
  same premise), accepts both and rejects pin-fixed-key-need
  ("the body does not bind `Box[…].get` (pinned; compat)").
  `examples/hello.moss` prints `Hello, world!` under compat and is
  rejected under faithful.
- **Corpus.** The elaborator's differential run over 460 files
  (`examples/`, `bootstrap/examples/`, `tests/**`; moss-elab
  `scratch/diff-run4.tsv`) is in effect this change's corpus comparison.
  Faithful and compat differ on 154 files, and every one of those
  faithful rejections is `link Arrays = GcArrays`. Compat rejects no file
  that faithful accepts, and the new premise rejects no corpus file.
- **Proof.** `public_safety` is still proved, and `check.sh` is green.
  - `PublicBodySlice`: `realizes_link_site`, `rkey_of_link_site` and
    `canon_link_site` take the pins. The site's merges and provisions are
    the body's, so the proofs are unchanged.
  - `PublicChainRealize`: `lookup_site_role` takes the pins.
    `lookup_site_in` also needs a symbol that is not pinned. The new
    `lookup_site_off` and `lookup_site_pin` read through the pins. `r2`'s
    receiver clause allows a pinned input, as its signature clause
    already did. In `root_level`, `RecvX` reads a pinned receiver symbol
    at its pin, using `PinT`, the same fact `KeyX` uses for the types.
  - `PublicFamilyLink.family_link_new_ok`: `R2rec` allows a pinned input,
    and `Recv` reads the receiver per symbol. A pin goes by `PinRun`, and
    one of G's inputs by `link_run_fact` and θ's merge, as before.
  - `PublicLinkRun`: `lookup_subst_out` and `lookup_subst_dom`, two small
    lemmas about substitutions. The other files (`PublicChainCore`,
    `PublicChainNew`, `PublicLinkResults`, `PublicCopyApply`,
    `PublicFamilyStep`, `PublicFamilyCopy`) only gain the pins argument
    in their statements. No proof reads the new premise.
- **The executable elaborator.** Faithful mode needs what compat already
  does: call `link_site Cg Cb pins outs gen`, and make the "(pinned;
  compat)" check unconditional. Then `s_compat_pins` can be dropped.
- **Class.** A relaxation toward both implementations, with one
  reject-only premise that mirrors both.

### 74. Every declared context forms as written (`DeclsForm`, n234 branch; criteria section 7)
- **The gap (N2).** Audit checked a context only when something formed
  it: a body's assume nesting (`CtxOf`), another declaration's nesting
  (`AssumesComplete`, #57, which forms each symbol's nesting but not a
  context's own items), or a functor's signature. So
  `tests/errors/context_cycle_unused.moss` (`context A = B; context B =
  A;`) and `context_cycle_nominal.moss` (the cycle traversed only by a
  tag's requirements) were accepted, while both implementations reject
  them. The executable elaborator (branch `elab`, `scratch/elab-STATUS.md`
  row N2) found this.
- **Rule now.** `ElaboratesIn` has a new last conjunct, `DeclsForm W P`:
  every symbol's declaration forms as written (`DeclForms`). For a
  context, its items form, read in its own module with each type symbol
  standing for itself (`as_written L`): `exists L es tm, ItemsEntries W
  (d_mod d) [] (as_written L) its es tm`. A context that nests in a cycle
  has no finite derivation, and an item that does not resolve, or whose
  brackets do not, has none either. Merging is not asked: a context's
  telescope is merged where it is formed, as before.
- **Why "as written".** A context is formed at each use with the user's
  type meanings (`tm`), so its own nesting is the wrong place to read it:
  `lib/wasistd.moss`'s `context Branch = i32_bool;` lists a fn whose
  signature mentions `I32`, which `Branch`'s empty nesting does not
  have. With each type symbol read as itself, every item forms that forms
  at some use, except one whose signature has a union member that is a
  type symbol (D15; see N4 under "Known divergences" below).
- **Criteria.** Section 7 (new): every declaration is checked as written,
  whether or not anything uses it. Fidelity (section 6) for the cycles:
  the oracle's `check_contexts` (`context_acyclic`, Elab.v) rejects a
  cycle at declaration, and native expands every context ("context
  expansion is recursive or nested too deeply"). The oracle and native
  check nothing else about an unused context, so the unresolved-item and
  bracket tests are class B, by section 7.
- **Tests.**

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | `tests/errors/context_cycle_unused.moss` | rejects ("nests in a cycle") | rejects | accepts | rejects |
  | `tests/errors/context_cycle_nominal.moss` | rejects | rejects | accepts | rejects |
  | [decl-ctx-cycle-functor](../tests/errors/decl_ctx_cycle_functor.moss) (the cycle only in an unused functor's result) | rejects | rejects | accepts | rejects |
  | [decl-ctx-unresolved](../tests/errors/decl_ctx_unresolved.moss) (`context A = nope;`) | accepts | accepts | accepts | rejects |
  | [decl-ctx-bracket](../tests/errors/decl_ctx_bracket.moss) (`f[Nope = T]`) | accepts | accepts | accepts | rejects |
  | [decl-ctx-outside-control](../tests/wasi/decl_ctx_outside_control.moss) (items mention a type outside the nesting) | accepts | accepts | accepts | accepts |

  Audit's verdicts are argued from the rules and run on an executable
  elaborator variant: moss-elab 64b606c (`--frontend audit`, Audit at
  #73) plus this change and #75 to #77 (scratch/elabv, `decls_form` in
  `EaMain.v`, each failure prefixed by the check that made it).
- **Corpus.** That variant against the plain elaborator (moss-elab
  `scratch/bin5-audit_run.exe`), `--elab-only`, over `examples/`,
  `bootstrap/examples/` (before this branch's tests), `tests/**` and
  `src/main.moss`: 512 same / 4 diff (scratch/cmp-run1.tsv). The four are
  the two context-cycle tests above and the two N3 tests of #77; all four
  are rejected by both implementations. Every `lib/*.moss` module, as an
  entry, reaches the same "no `main`" in both (scratch/cmp-lib.tsv), so
  every library declaration forms as written. `src/main.moss` is accepted
  by both.
- **Proof.** `public_safety` unchanged: the conjunct is last and no proof
  reads it. `AuditBuildExample.main_decls` proves it of the example world.
- **Class.** A for the cycles (mirrors both implementations); B for an
  unused context that names something not in scope, by section 7.

### 75. Every symbol's assume nesting forms (`AssumesComplete`, n234 branch; criteria section 7)
- **The gap.** #57's `AssumesComplete` formed the assume nesting of every
  val, fn, method, context and functor symbol, as the oracle's
  `check_assumes` does, and skipped types, units, tags and aliases. A
  nesting that holds only those was never formed:
  `assume Nope { type T; }` was accepted.
- **Rule now.** `AssumesComplete` asks it of every symbol, whatever its
  kind: its nesting forms (`AssumeCtx`) and its assumed items are
  complete (`AssumedComplete`). The list of kinds is gone.
- **Criteria.** Section 7: an assume nesting is part of what each
  declaration in it is written in, so it is checked as written whatever
  is declared there. Fidelity: both implementations accept the three
  rejected tests below, so they are class B, by section 7. In both, a
  nesting is formed when something in it is elaborated or assumed.
- **Tests.**

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | [decl-nest-type](../tests/errors/decl_nest_type.moss) (`assume Nope { type T; }`) | accepts | accepts | accepts | rejects |
  | [decl-nest-tag](../tests/errors/decl_nest_tag.moss) (only a tag in the nesting) | accepts | accepts | accepts | rejects |
  | [decl-nest-incomplete](../tests/errors/decl_nest_incomplete.moss) (a tag's nesting assumes f without f's requirement) | accepts | accepts | accepts | rejects |
  | [decl-nest-tag-control](../tests/wasi/decl_nest_tag_control.moss) | accepts | accepts | accepts | accepts |
  | `tests/errors/context_cycle_nominal.moss` (a tag's nesting through a cycle) | rejects | rejects | accepts | rejects (already by #74) |

  Audit's verdicts are argued from the rules and run on the elaborator
  variant of #74 (`assumes_complete` over every kind).
- **Corpus.** The comparison of #74 (same variant): no corpus program's
  verdict changes through this check alone. The one it reports
  (`context_cycle_nominal.moss`, "N2nest") #74 already rejects.
- **Proof.** No proof reads `AssumesComplete`. `main_assumes` loses its
  kind hypothesis.
- **Class.** B (both implementations accept the rejected tests), by
  criteria section 7.

### 76. Every other declaration forms as written (`DeclsForm`, n234 branch; criteria section 7)
- **The gap.** After #74 and #75, these were still checked only where
  something read them:
  - a tag's payload (`PayloadOf`, at a constructor or pattern) and an
    alias's body (`TyAlias`, where the alias is named);
  - an abstract val's type, and an abstract fn's or method's signature
    (`IEVal`, `IEFn`, `IEMeth`, where they are assumed; `BValBind`,
    `BFnBind`, `BMethBind`, where they are bound);
  - a defined val's initializer (`ExConst`, inlined at each use);
  - a functor's argument telescope (`FunCtx`) and result
    (`ProvideResult`), where it is assumed, applied, bound or copied.

  So an unused `tag Box(Nope);`, `type X = Y; type Y = X;`, `val x: () =
  nope;` or `functor F: nope -> x;` was accepted.
- **Rule now.** `DeclForms` covers them, each read as written (each type
  symbol standing for itself, `as_written L`):
  - tag, alias: the payload or body denotes a type (`TyOf`);
  - val: its type denotes a type, and a defined val's initializer
    elaborates as `ExConst` reads it (`ConstElab`: no assumptions, its type
    read in an empty context, the initializer in an empty context and
    frame);
  - fn: its signature (`SigOf`, no `This`); method: its signature at some
    receiver type;
  - functor: `FunCtx` forms, and its result forms over that context with
    each other type symbol standing for itself (`ProvideResult` at
    `c_ty C ++ as_written L`, each output role itself).
- **Form chosen.** Reading at the declaration's own assume nesting would
  reject the prelude: a top-level tag over an abstract type it does not
  assume (`decl-tag-outside-control`) is ordinary, and so is a context
  item mentioning a type outside the context's nesting (#74). The first
  variant also read a functor's result over `FunCtx` alone, and rejected
  every program: `lib/wasistd.moss`'s `WasiFloats: Wasm, Branch -> Floats`
  promises `float(n: Int)`, and `Int` is neither an input nor an output
  (its body binds `int::Int = Num`). So other type symbols stand for
  themselves there too. A functor with a body has no implicit inputs
  (`FunCtx`), so this is the one place the result needs them.
- **Not covered.** A functor body's binds are still elaborated only at an
  application (`BApplyBody`, in the caller's context, as both
  implementations do: the body is inlined, transparently) and at a link
  (`LinkBody`, in G's own terms). Checking an unapplied body would need a
  context of its own; the natural one is G's `FunCtx`, as `LinkBody`
  uses, but a body applied directly may use what its caller provides, so
  that check could reject programs both implementations accept and run.
  It is left open; #77 checks the part both implementations check by
  name, that the body binds its whole result. Also left: the
  requirements and needs of a context's items are checked where the
  context is assumed (#57), not where it is declared, since the items are
  read at the user's instantiation.
- **Criteria.** Section 7. Fidelity: the oracle and native read these only
  where used (both accept every rejected test below), so this is class B
  throughout, by section 7. D15 for the union test: a union's members are
  nominal with distinct heads where the union is written.
- **Tests.** In each "Audit before" accepts and the oracle and native
  accept, except the controls, which everyone accepts.

  | program | Audit now |
  |---|---|
  | [decl-tag-unresolved](../tests/errors/decl_tag_unresolved.moss) | rejects (`TyOf`: not in scope) |
  | [decl-tag-union-head](../tests/errors/decl_tag_union_head.moss) (`A[X=Colour] \| A[X=Red]`) | rejects (`TyUnion`: one head twice) |
  | [decl-alias-cycle](../tests/errors/decl_alias_cycle.moss) | rejects (no finite `TyAlias` derivation) |
  | [decl-alias-unresolved](../tests/errors/decl_alias_unresolved.moss) | rejects |
  | [decl-val-type](../tests/errors/decl_val_type.moss), [decl-fn-sig](../tests/errors/decl_fn_sig.moss), [decl-meth-sig](../tests/errors/decl_meth_sig.moss) | reject |
  | [decl-const-bad](../tests/errors/decl_const_bad.moss), [decl-const-mistyped](../tests/errors/decl_const_mistyped.moss) | reject (`ConstElab`) |
  | [decl-const-in-assume](../tests/errors/decl_const_in_assume.moss) (unusable: a use is rejected by all three) | rejects (`ConstElab`: assumptions) |
  | [decl-functor-arg](../tests/errors/decl_functor_arg.moss), [decl-functor-res](../tests/errors/decl_functor_res.moss) | reject |
  | [decl-tag-outside-control](../tests/wasi/decl_tag_outside_control.moss), [decl-const-control](../tests/wasi/decl_const_control.moss), [decl-functor-res-control](../tests/wasi/decl_functor_res_control.moss) | accept |

  Audit's verdicts are argued from the rules and run on the elaborator
  variant of #74.
- **Corpus.** The comparison of #74 (same variant): no corpus program's
  verdict changes through these checks, every `lib/*.moss` module's
  declarations form, and `src/main.moss` is accepted.
- **Proof.** No proof reads `DeclsForm`. `AuditBuildExample.main_decls`
  now shows main's signature forms.
- **Class.** B, by criteria section 7 (both implementations accept every
  rejected test).

### 77. A functor with a body binds its whole result (`FunctorTotal`, n234 branch; D55)
- **The gap (N3).** `BApplyBody` (`bind F;` for F with a body: F's binds,
  inlined) had no result check, so `tests/wasi/functor_partial.moss`
  (`Over` promises `Scalar.add` and binds only `Scalar = Int`) and
  `functor_partial_unreached.moss` were accepted; both implementations
  reject them ("functor `Over` does not bind `Scalar.add`"; "the functor
  does not bind every item its result promises"). The executable
  elaborator found this (`scratch/elab-STATUS.md`, row N3).
  `LinkBody` has the same gap: its `ResultRealizes` reads the promise at
  the link site, where `Scalar.add` reads `Int.add`, and G's own input
  `Std` provides that, so
  [functor-partial-link](../tests/errors/functor_partial_link.moss) (the same
  body, linked) was accepted too.
- **Rule now.** `DeclForms` asks of every functor with a body that it is
  total (`FunctorTotal`): each leaf of its declared result (`ResultLeaf`,
  as `SameResult` reads leaves) is bound by an item of its body, by
  written name (`BodyBinds`): `bind p = e` and `bind R.m = e` bind their
  own leaf, `bind G;` binds G's declared result's leaves. This is the
  oracle's `functor_total` (Elab.v) exactly, and native's check
  (`src/codegen.moss`, at `e_partial`).
- **Form chosen.** The task suggested an analogue of `LinkBody`'s
  `ResultRealizes` in `BApplyBody`: after the body's binds (C'), each
  promised key read at C' is provided there at a refining provision. That
  does not reject `functor_partial.moss`: at C', `Scalar.add` reads
  `Int.add`, which the caller's `Std` provides. A semantic form that does
  reject it would ask each key to be one some binding of the body writes;
  it would then accept `bind Scalar = Int; bind Int.add = f;`, which both
  implementations reject (they compare leaves by written name), and it
  needs the body's binding list in `BApplyBody`. The leaf check is
  simpler, mirrors both, and covers direct application and links alike.
  It is checked at the declaration, applied or not (criteria section 7),
  rather than at each application: the implementations check it at an
  application or link only, so an unapplied partial functor is class B.
  Transparency is unchanged: the caller still sees exactly the body's
  binds, and nothing installs F's promise.
- **Safety without the check.** Checked by reading Audit.v; the check is
  not needed for `public_safety`, which is why the proof is untouched.
  - `BApplyBody`'s output context C' is `BindItems`' output and nothing
    else: no `ProvideResult`, and its expression is the body's binds,
    `wrap`, with no `BApply` (so `apply_functor` never runs and no result
    is installed). The block continues in C' (`BlkBind`).
  - A later use of a promised item is checked against C' like any other:
    a call through `Provided` needs the key in C' (`PrIn`) or a family
    in force that produces it (`PrFamily`); `ExVal` needs `c_vals C'`;
    an fn call needs `c_fns C'`. If the caller lacks the provision that
    would back the promise, there is none in C', and the use has no
    derivation:
    [functor-partial-unbacked-use](../tests/errors/functor_partial_unbacked_use.moss)
    is rejected before this change ("no provision and no family for a
    method (D62)").
  - Without a use, nothing reads the missing provision at run time:
    [functor-partial-unbacked](../tests/errors/functor_partial_unbacked.moss) is
    accepted before this change and runs to exit 0 on Audit's interpreter
    (moss-elab `audit_run`).
  - For a link, the promise is read by `ResultRealizes` against the
    body's final context, so each installed key names a provision the
    body's environment has (bound by the body or supplied by the
    caller through G's linked inputs, which `Satisfies` asks of each
    application).

  So the unchecked promise was a fidelity gap, not a safety gap.
- **Criteria.** Fidelity (section 6) for applied and linked functors;
  section 7 for an unapplied one.
- **Tests.**

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | `tests/wasi/functor_partial.moss` | rejects | rejects | accepts (prints 5) | rejects |
  | `tests/wasi/functor_partial_unreached.moss` (applied in a fn nothing calls) | rejects | rejects | accepts | rejects |
  | [functor-partial-link](../tests/errors/functor_partial_link.moss) | rejects | rejects | accepts | rejects |
  | [functor-partial-unbacked](../tests/errors/functor_partial_unbacked.moss) (nothing backs the promise; no use) | rejects | rejects | accepts (exit 0) | rejects |
  | [functor-partial-unbacked-use](../tests/errors/functor_partial_unbacked_use.moss) (the use) | rejects | rejects | rejects (at the use) | rejects |
  | [functor-partial-unused](../tests/errors/functor_partial_unused.moss) (never applied) | accepts | accepts | accepts | rejects |
  | [functor-total-control](../tests/wasi/functor_total_control.moss) (binds by name and through `bind Inner;`) | accepts | accepts | accepts | accepts |

  Audit's verdicts are argued from the rules and run on the elaborator
  variant of #74 (its "N3total" check).
- **Corpus.** The comparison of #74: the two `tests/wasi` programs above
  are the only corpus programs whose verdict this changes.
- **Proof.** No proof reads `DeclsForm`; the example world has no functor.
- **Class.** A for applied and linked functors (mirrors both); B for an
  unapplied one, by criteria section 7.

### 80. Type equations as context and assume items (`AIEq`, `CIEq`, `IsEq`; tyeq branch; D90, owner-approved)
- **Rule now.**
  - *Syntax.* An assume item is `AIHead (p, d)` (as before) or `AIEq l r`,
    and a context item `CISpec s` (as before) or `CIEq l r`
    (`Input.aitem`, `Input.citem`). Each side is a type reference, `p` or
    `p[...]` (`Syntax.TypeRef`), and the `=` follows a whole reference
    (`Syntax.Equation`), so it is outside any brackets. `AssumeItem` and
    `CtxItem` keep the two readings apart: a head or spec is followed by
    neither `=` nor (for an assume head) `[`. Functor argument and result
    lists still take specs.
  - *Requirements.* `ty_heads` lists the references a side writes (its
    head, and its brackets' right sides, not their left sides) as
    unbracketed specs. `SpecTypes`' context case reads a context's items
    through `citem_specs` (a spec, or an equation's references), and
    `AssumeTypes` an assume nesting through `aitem_specs`. So every type
    symbol an equation writes is a requirement (rule B, #60).
  - *Formation.* `ItemsEntries` ranges over `citem`s. `IsEq` reads an
    equation's references as items (`ItemsEntries ... (map CISpec
    (ty_heads l ++ ty_heads r))`, so a type symbol among them is put in
    force as `IEType` puts one), reads both sides there (`TyOf`), and adds
    the entry `NEq tl tr`. `NEq` has no key, signature, instantiation or
    provision; `Unifies` gains the conjunct "an equation's sides are one
    type under σ", and `entry_gens (NEq l r) = gens l ++ gens r`. So
    `MostGeneralMerge` treats the equation as a direct merge constraint.
  - *Results.* A result's context holds no equation: `OIECtx`,
    `FamilyResult`'s and `FamilyKeyed`'s context cases ask `its = map
    CISpec sps`.
  - *Calls.* Nothing new: the equation's merge is in the callee's σ, so
    `Satisfies`' merges clause (by class, #54) asks the caller to have the
    two sides equal, and `CallTheta` and rule B treat it as any merge.
  - *Declaration checks (#74 to #77, #90).* Nothing new is needed:
    `DeclForms`' context case is `ItemsEntries` over the context's
    `citem`s, so an equation in a context is checked as written, like
    its other items (`IsEq` under `as_written L`: its references resolve
    and its sides denote types; the merge is not asked there, as for any
    context item). `AssumeCtx` (`AssumesComplete`) forms a nesting
    through `assume_spec`, so an equation in an assume list is formed
    and merged as in `CtxOf`; it adds no need, so `AssumedComplete` has
    nothing more to check. `FunctorTotal` reads `ResultLeaf`, whose
    context case reads `citem_specs` (an equation's references are
    leaves; a result with an equation is refused by `OIECtx` anyway).
    `FiniteLayouts` and D15 read tag payloads and declared types, which
    hold no equation.
- **Why.** The owner's decision (D90): the dummy-item workaround
  (`same[Same=Int], same[Same=Size]`) also adds a need for `same`; the
  equation adds only the merge. The syntax is `bind`'s `=`; the
  equation is symmetric; structured equations are allowed, one structure
  per class (Δ4's rule, #61); rule B applies.
- **Criteria.** Coherence (section 2): the two spellings give one merge
  class (`PublicEquation.eq_swap_classes`, from
  `PublicOrientation.mgm_classes_iff`; `same_swap_classes` for telescopes
  equal up to the order of entries and of an equation's sides, which
  covers the different order of the two spellings' references).
  Principality (section 3): the merge is a most general unifier with the
  equation's constraint, as for any merge.
- **Proof.** `public_safety` is still proved and `check.sh` is green. The
  public proofs gain the `IsEq` case (`entries_formed`, `entries_ok`,
  `entries_keys_decl`, `entries_read`, `entries_keys_typed'`, the
  `PublicPlatformApply` and `PublicLinkMerges` walks) and the `NEq` case
  of every entry function (`EntryP`, `unifies_mark`, `entry_ok_formed`,
  ...); `PublicChainAgree.output_no_eq` shows a result's reading has no
  equation entry.
- **Oracle.** `Parse.v` (`parse_assume_item`, `parse_ctx_item`,
  `parse_type_ref`), the dump (an equation is `(V l r)`, `Sx.dassume`,
  `Sx.dcitem`) and its round trip (`DumpRoundTrip`), `FrontEnd` and
  `FrontEndComplete` for the new items. `ExecutionModel.v` mirrors
  Audit's formation. `Elab.v`: `add_eq_merge` (the two sides' `ety_of`,
  then `unify`), the context walk over `citem`s (an equation's
  references through `add_spec`, then the merge), `add_assume_item`'s
  `AIEq`, `assume_specs := flat_map aitem_specs`, and the result walks
  (`provide_spec` refuses a result context with an equation, families
  are not families then, `ctx_specs` elsewhere). Proofs: `CtxWalk`
  (`specs_walk`, `eq_walk`, `merge_step`, `ws_eq_add`, and the walk
  invariant's new `ws_eqs`: with no new merge, an equation's sides are
  one up to the merges), `CtxMerge.settled_unifies`, `ContextWalkClean`,
  `ContextWalkSupport`, `WorldSound`, `ReqsTotal` and the family and
  result walks. `ContextWalkClean.add_eq_merge_eq` unfolds the merge by
  rewriting: unfolding it by conversion made the kernel unfold `unify 200`.
- **Native.** `src/`: the AST node `k_eq` (sides, and the run of the
  references they write); the parser (`parse_ctx_item`,
  `parse_assume_item`, `parse_equation`); the dump (`V`, the letter of
  `k_d_bad`, which no dump holds); `lower::add_eq_item` (the references
  through `add_spec`, then `unify`) in the assume and context walks;
  `nest_types` (requirements), `result_types` and `sig_items` read the
  references; the family, result-key and cycle walks skip or refuse an
  equation (`e_eq_result`).
- **Tests.**

  | program | Audit (by reading) | oracle | native |
  |---|---|---|---|
  | [merged_int_size](../tests/wasi/merged_int_size.moss) (`context IntIsSize = Int = Size;`, library code against the separate types, called from inside the merged context) | accepts | 21 | 21 |
  | [merged_size_int](../tests/wasi/merged_size_int.moss) (the same, `Size = Int`, in the assume list) | accepts | 21 | 21 |
  | [eq_caller_same](../tests/wasi/eq_caller_same.moss) (a caller with the sides equal; no need added) | accepts | exit 0 | exit 0 |
  | [eq_link_tx](../tests/wasi/eq_link_tx.moss) (`T = X` in a linked body's merged argument context) | accepts | exit 0 | exit 0 |
  | [eq_link_xt](../tests/wasi/eq_link_xt.moss) (the same, `X = T`) | accepts | rejects (D84 3d) | exit 0 |
  | [eq_clash](../tests/errors/eq_clash.moss) (`Buf = P, Buf = Q`) | rejects (no unifier) | rejects ("cannot merge `P` with `Q`") | rejects (`e_merge`) |
  | [eq_clash_structure](../tests/errors/eq_clash_structure.moss) (`Buf = Bool, Buf = Box[T=T]`) | rejects | rejects | rejects |
  | [eq_caller_differs](../tests/errors/eq_caller_differs.moss) (the caller binds the sides to `P` and `Q`) | rejects (merges clause) | rejects | rejects |
  | [eq_caller_unbound](../tests/errors/eq_caller_unbound.moss) (the caller has neither side in force) | rejects (rule B) | rejects | rejects |
  | [eq_result](../tests/errors/eq_result.moss) (a result context with an equation) | rejects (`OIECtx`) | rejects | rejects |
  | [decl_ctx_eq_unresolved](../tests/errors/decl_ctx_eq_unresolved.moss) (an unused context whose equation names `Nope`) | rejects (`DeclsForm`: no `IsEq`) | accepts (ORACLE_GAP_UNUSED_DECL) | rejects (`e_scope`) |
  | [decl_ctx_eq_control](../tests/wasi/decl_ctx_eq_control.moss) (an unused `context A = T = U, T.m;`, read as written) | accepts | accepts | accepts |

- **Orientation (both spellings).** The two spellings mean the same in
  Audit: the merge's orientation is left open (`MostGeneralMerge`), and
  the spellings' telescopes differ only in the order of their entries and
  of the equation's sides, which `Unifies` does not see; so Audit's
  verdict cannot depend on the spelling. Native merges toward the right
  side (`lower::unify`: an edge from the first bare symbol), the oracle
  too (`Elab.unify`), so the spelling picks the representative. On every
  program above but one the spellings agree in all three. The exception
  is the open item 4 of the consolidation (a linked body's checks follow
  the representative): `X = T` makes `T` the representative of the
  merged input class, the oracle's pins then include `T`, and it rejects
  the body's `bind T = Bool;` (D84 3d), while it runs `T = X`; native runs
  both. This is the oracle's behaviour on
  [link-merge-rep-rebind-both](../tests/wasi/link_merge_rep_rebind_both.moss)
  in the two context orders, not a new divergence; it is listed in
  `agreement.rs` (`ORACLE_GAP_ORIENTATION`) and not worked around.
- **Corpus.** Against the oracle of the branch's base, the oracle's
  `--dump` is identical on all 627 files of `src/`, `lib/`, `examples/`,
  `tests/` and `bootstrap/examples/` other than the 12 new tests (which
  the old parser refuses), and its `--elab-only` verdicts and messages
  are identical on all 608 such programs (the corpus without the
  compiler's own modules, and `src/main.moss`). The suite's dump
  comparison holds the native dump to the oracle's over the corpus, the
  new files included.
- **Class.** A language extension: new syntax, so no existing program
  changes; none of A–E applies.

### 90. Every tag has a finite layout (`FiniteLayouts`, `LayoutReaches`; layout branch; D58, D59)
- **The gap.** Audit had no notion of layout. It accepted
  [tests/wasi/cyclic.moss](../tests/wasi/cyclic.moss)
  (`tag Cell { head: I32, tail: Nil | Cell }`), which both implementations
  reject: a value is a run of scalars (D59), there is no heap between a
  type and what it contains (D58), so a type that contains itself unboxed
  has no finite layout. The comment on `TypeEdge` said this was "a separate
  check of the oracle's".
- **Rule now.** `ElaboratesIn` gains a last conjunct, `FiniteLayouts W`.
  - `LayoutReaches W τ n` says that computing τ's layout computes the
    nominal n's. A tag's layout is its payload's, read at its arguments
    (`PayloadOf W n args`). A record's is its fields' in sequence. A
    union's is its members' beside a discriminating word. Every other type
    is a leaf: a machine type, as the platform declares it (machine
    reference types included), is one scalar, and so is an abstract type.
  - `FiniteLayouts W`: no tag's payload, read at the tag's own requirements
    (each as itself), reaches the tag.
  - The check is on definitions. A cyclic tag is rejected whether or not
    the program builds or names it.
- **No special case for `Ref`** (criteria section 8). `Ref` is
  `tag Ref[Boxed](StructRef)`: a tag whose payload is a machine reference
  type. So `Ref[Boxed=Cell]`'s layout is `StructRef`'s one scalar, and the
  rule never looks at `Boxed`. `tag Cell { head: I32, tail: Nil | Ref[Boxed=Cell] }`
  has a layout. A user's own tag over `StructRef` gets the same treatment,
  and so does any library that names these things differently.
  - This is a different graph from `TypesBounded`'s `TypeEdge` (#48,
    #52), so the two are not shared. `TypeEdge` counts every nominal
    instance a payload mentions, behind a `Ref` too, because a WasmGC type
    names the types of its fields. A layout follows only what a payload
    holds unboxed: `Cell` mentions itself through `Ref[Boxed=Cell]`, which
    is a `TypeEdge` (not strict, so allowed) and no `LayoutReaches`.
- **Why an abstract type can be a leaf.** An abstract type is one scalar
  here, but at run time it is bound to some type. That type cannot make a
  layout infinite. A bind's right-hand side is read at the old meanings
  (D72, `BTypeBind` reads `τ` in `c_ty C` before `set_ty`), so
  `bind T = Cell` under `tag Cell { tail: Nil | T }` makes T mean
  `Cell[T=<T's old meaning>]`, a finite term. No sequence of binds builds a
  type that contains itself, so every ground type is a finite term.
  Laying out a finite term then terminates. "g's payload, read at g's own
  requirements, reaches h" is transitive (reaching is stable under
  substituting arguments for the requirements), so with no tag reaching
  itself it is a strict order on the finitely many tags. A nominal that a
  layout reaches through a tag's own payload text is below that tag in the
  order; one that it reaches through the tag's arguments is a subterm of
  the finite ground type being laid out. So no path of the layout
  computation is infinite. (This is an argument, not a Rocq theorem; the
  conjunct only rejects programs, so `public_safety` does not need it.)
  ([layout-opaque](../tests/wasi/layout_opaque.moss) binds `Link = LHold`,
  a tag holding a `Ref[Boxed=LNode]`, after `LNode { next: LNil | Link }`.)
- **Criteria.**
  - Fidelity (section 6), for every program that uses a type: both
    implementations compute layouts this way and reject a type that
    contains itself. Neither special-cases `Ref`: the oracle's `slots_go`
    (Elab.v) and native's `width` (src/types.moss) treat an abstract type
    as one scalar and follow a tag's payload at its arguments.
  - Specialization (section 4): every type has a finite run of scalars, so
    the compilers can lay it out.
  - Separation of the language from its platform library (section 8): the
    rule mentions no library name.
- **The definition-site choice, and the implementations.** Before this
  change both implementations checked a layout only where a type is used,
  so they accepted a cyclic tag that nothing builds. The owner chose the
  rule on definitions. The oracle and native now check every tag's
  definition the same way, so they agree with Audit again:
  - Oracle: `Elab.check_layouts`, run by `elaborate` and
    `elaborate_with_metadata` after `elab_program` accepts, so no other
    diagnostic changes. It reports "`X` contains itself, so it has no
    finite layout (D58)". A payload that does not read is not a cycle there
    (elaboration reports it if it matters). `layout_reaches` compares
    symbols, as `LayoutReaches` does, and treats running out of fuel (300
    levels) as a cycle, as `slots_go`'s depth limit does.
  - Native: `lower::check_layouts` (after `check_listed`), with
    `types::layout_reaches`, reporting `e_shape` ("this type has no finite
    runtime layout") at the tag. A payload that names an alias expanding
    to itself (`type Broken = Broken; tag Bad(Broken);`, the suite's
    `opaque_nominal_arguments_do_not_force_their_unused_payloads`) is not
    elaborated (`types::payload_expands`, a syntactic check), since
    elaborating it never stops; Audit has no `PayloadOf` for such a tag
    either.
  - One corpus program relied on the old reading:
    [tests/wasi/relevance_applied.moss](../tests/wasi/relevance_applied.moss)
    declared `tag A { b: B, t: T }` and `tag B { a: A }` only to name them
    ("neither type has a layout, so they are only named, never built").
    It tests D71's relevance fixpoint, not layouts, so `A` now holds `B`
    through a `Ref[Boxed=B]`. The mutual mention, and so the fixpoint, is
    unchanged (the relevance closure reads a bracket's right-hand side), and
    both implementations still print `y`.
- **Tests.**

  | program | oracle before | native before | Audit before | Audit now | oracle, native now |
  |---|---|---|---|---|---|
  | [tests/wasi/cyclic.moss](../tests/wasi/cyclic.moss) | rejects (D58) | rejects (no finite layout) | accepts | rejects | reject |
  | [layout-self](../tests/errors/layout_self.moss) (cyclic.moss without imports) | rejects | rejects | accepts | rejects | reject |
  | [layout-self-unused](../tests/errors/layout_self_unused.moss) (the same tag, never built) | accepts | accepts | accepts | rejects | reject |
  | [layout-generic-self](../tests/errors/layout_generic_self.moss) (through `LBox[LT=LC]`; a generic `LList[LT]`) | rejects | rejects | accepts | rejects | reject |
  | [layout-ref](../tests/wasi/layout_ref.moss) (recursion through `Ref[Boxed=LCell]`) | 6 | 6 | accepts | accepts | 6 |
  | [layout-opaque](../tests/wasi/layout_opaque.moss) (an abstract `Link` in force through a context and bound to a tag holding a `Ref` afterwards; a user's phantom tag over `StructRef`; a generic tag with abstract members) | 7 | 7 | accepts | accepts | 7 |
  | [tests/wasi/relevance_applied.moss](../tests/wasi/relevance_applied.moss), as it was | y | y | accepts | rejects | reject |
  | the same, through a `Ref` (as it is now) | y | y | accepts | accepts | y |

  Audit's verdicts are argued from the rules, not run.
- **Corpus.** The oracle with `check_layouts` against the oracle before it,
  `--elab-only`, over `src/main.moss`, `tests/`, `examples/` and
  `bootstrap/examples/` (521 files, `ulimit -s unlimited`): 519 same,
  2 diff. The two are layout-self-unused (new) and relevance_applied, as it
  was, both now rejected; nothing else changes. `src/main.moss`, with the
  whole library, is accepted.
- **Proof.** `public_safety` is still proved and `check.sh` is green. The
  conjunct only rejects programs, and no proof reads it.
  `AuditBuildExample.main_layouts` proves it of the example world (it has
  no tag). For the oracle, `Elab.elaborate_with_metadata_run` and
  `Elab.elaborate_run` recover `elab_program`'s result from an accepted
  bundle; `ElabFrame.elaborate_frame`, `FnCoverage`,
  `ReifyHeaderProgram` and `Certified` use them in place of unfolding
  `elaborate`.
- **Class.** A (reject-only, mirroring both implementations) for every
  type a program uses. For an unused cyclic tag, B against the
  implementations as they were; they now match.

### 91. Main's platform provisions are Platform's contract (`PlatformEntry`; layout branch; criteria section 8)
- **Rule now.** `platform_mod` and `PlatformEntry` (which library module
  may supply main's needs, `i32_zero` and `i32_one` as the I32 constants,
  a platform fn as `prim_of_name` of its name, an abstract functor of those
  modules as a platform family) move from `Elaboration` into the `Platform`
  section. `ElaboratesIn` keeps only the language's part of main's
  environment: main's context forms, the runtime types are `InitialTypes`,
  and the environment provides exactly main's needs, keyed by
  `rkey_of`. `Platform.Linked` gains the provisions' contract:
  `Forall (fun entry => PlatformEntry W P (fst entry) (snd entry)) entries`,
  beside `Forall Entry`, for the same `entries`.
- **Why.** These are facts about one platform (the names of `lib/wasm.moss`,
  `lib/wasip1.moss` and `lib/wasistd.moss`, and the primitive table), not
  rules of the language (criteria section 8). The language rule said, in
  the middle of `ElaboratesIn`, that a val named `i32_one` in `wasistd.moss`
  means 1.
- **Nothing is accepted or rejected differently.** `Builds`, `Ready` and so
  `Admissible` and `Compatible` all take `ElaboratesIn ∧ Linked`, and
  `Linked`'s `CtxOf` and `program_environment` are the same ones
  (`function_context_unique`), so the conjunction is the same proposition
  regrouped. Only `Elaborates` alone (no caller in the public statements)
  is weaker.
- **Proof.** `public_safety` is still proved and `check.sh` is green. No
  proof read `PlatformEntry`; `PublicEntry`'s destructuring of the two
  clauses and `AuditBuildExample`'s example world change by one pattern
  each.
- **Class.** None (a move).

### 92. The language items go through one table (`lang_item`, `lang_item_key`, `LangItem`; layout branch; criteria section 8)
- **Rule now.** After `LangSym`, `lang_item` lists the library items the
  language's own rules need: `Bool`, `True`, `False` (a condition's type
  and values, D45), `int::zero`, `int::one`, `num.moss`'s `add`, `mul` and
  `div` (a numeric literal's arithmetic, D68) and `float::float`.
  `lang_item_key` gives each its module, namespace and name, and
  `LangItem W i s` is `LangSym` at that key. `Literal`, `Cond`,
  `ElaboratesIn`'s Bool clause, and `Platform.ValueType`'s and
  `Platform.Layouts`' Bool name them through `LangItem`. The operators keep
  their own table, `operator_method` (D68, D85), and a string literal's
  desugaring names `byte::…` and `string_…` in scope.
- **Why.** A library name used by a language rule is visible in one place,
  so a reader can tell a language item from a leak (criteria section 8).
- **Nothing changes.** `LangItem W i s` reduces to the `LangSym` it
  replaced. The proofs go through unchanged; `AuditBuildExample.main_linked`
  unfolds `LangItem` once.
- **Class.** None (definitional).

### 93. Recorded, not fixed: a linked body may not pin a fixed input that G takes (the oracle plan's N5; layout branch)
- **The program.** `runtime_platform::private_checked_casts_preserve_chained_fixed_pin_meanings`
  (crates/moss-tests), and without its casts
  [pin-taken-chain](../tests/wasi/pin_taken_chain.moss) and
  [pin-taken-one](../tests/wasi/pin_taken_one.moss): `F[]` is fixed in `T`
  (and `U`, `V`), which its result mentions, and G *takes* T and binds it
  in its body, agreeing with the bind site.

  | program | oracle | native | Audit |
  |---|---|---|---|
  | pin-taken-one | exit 0 | exit 0 | rejects |
  | pin-taken-chain (`bind T=U; bind U=V; bind V=Old`) | exit 0 | exit 0 | rejects |
  | the crates test (the chain, with `Ref` casts) | exit 0 | exit 0 | rejects (the executable elaborator: "a pinned input of the bound functor (D84 3d)") |

- **Diagnosis.** Audit is stricter than intended; the executable
  elaborator reads it correctly. #71 set `fr_pins Fg` to
  `Mentions W u x /\ (Poly W u x \/ In x (map fst (c_ty Cg)))`: a fixed
  input that G takes and F's result mentions is still pinned, so
  `BTypeBind` (`~ In t (fr_pins F)`) refuses `bind T = …` in the body.
  #71 lists this under "what is still rejected". But the oracle's D84 3d
  forbids only P(F) ∩ M(F) and lets the body bind any member of X(F),
  whether or not G takes it, with the agreement check at the bind
  (Elab.v, "m4, D84 3d"); native accepts too. So this is the same family
  as #71 and #73 (pins of fixed inputs; Audit rejects what both
  implementations run), at the one case #71 left out.
- **The fix, and why it is not made here.** The rule would let `LinkBody`'s
  pins include a fixed input G takes: `fr_pins` becomes
  `Mentions /\ Poly`, and `(x, tx) ∈ pins` for every fixed x the body
  binds. `link_site`'s table already puts the pins before `c_ty Cg`, so a
  pinned input of G reads at its pin, and `BFunClosure`'s agreement is
  already per pin. The chain is sound: each pin agrees with the bind
  site, and fixed inputs mean at every application what they meant
  there. But the proofs assume that a pin is not one of G's inputs, and
  that a pin's type, read in the body's *final* environment, is its
  reading in input terms (`PublicFamilyLink.PinRun` and
  `PublicChainRealize.PinT` read it as `ground d3 tx`, d3 the body's final
  environment). Neither holds for a chain: `bind T = U; bind U = V` leaves T's pin
  `U` (input U), while U's final runtime meaning is input V's. The
  proofs would read a pin in the input meanings instead (θ, with the
  agreement giving the equalities), in `PublicChainRealize.root_level`,
  `PublicFamilyLink.family_link_new_ok` and `PublicLinkRun`, and the facts
  that a pinned input is not one of G's (`lookup_site_in`, the disjoint
  `link_site` table) would have to be revisited. That proof change was
  not made on this branch.
- **Class.** A fidelity gap (criteria section 6), recorded with its fix.
- **Fixed in #95**, with a smaller proof change than the one above: a
  pin read in the body's final environment is already its reading in
  input terms, since `bind T = U` writes only T's alias.

### 94. Recorded: a platform constant of another type than `I32` is rejected (the oracle plan's N6; layout branch)
- **The program.** `runtime_platform::machine_constants_have_machine_types`:
  a `lib/wasm.moss` with `val i32_one: (); context Wasm = i32_one;`, and a
  main that assumes `Wasm`. The oracle and native reject it ("platform I32
  constant has an incompatible declared type"). The oracle plan found the
  executable elaborator (branch `elab`) accepting it.
- **Audit already rejects it.** main needs `i32_one`, so `Platform.Linked`
  asks `Entry W P C (IVal v) (ValueProvision (VI32 _))`, which asks the
  val's type in main's context to be `MachineType W "I32" t`: an abstract
  type symbol in scope as `I32` in the library's `wasm.moss`. `()` is not
  one. `AuditExamples.a_unit_platform_constant_is_rejected` proves that no
  entry exists for a val of type `()` (a certificate root).
- **The gap is the executable elaborator's.** `EaPlatform.platform_entry`
  decides `PlatformEntry` (which constant) but not `Entry`'s type check
  (its header lists "Platform.Linked's remaining checks" as a TODO). Its
  `IVal` case should also check `machine_type "I32"` of the val's type in
  `C`, as its `IFn` case checks `at_key`.
- **Class.** None in Audit.

### 95. A linked body may pin a fixed input that G takes (`LinkBody`; n5 branch; closes N5, #93)
- **Rule now.**
  - `LinkBody`'s protected inputs are P(F) ∩ M(F):
    `In x (fr_pins Fg) <-> Mentions W u x /\ Poly W u x`. Before (#71)
    they also held the fixed inputs that G takes and F's result
    mentions, so `BTypeBind` refused `bind T = …` for them.
  - The pins are every fixed input of F that the body leaves at another
    reading than G's own, whether or not G takes it:
    `In (x, tx) pins <-> Fixed W u x /\ nlook x (c_ty Cb) = Some tx /\
    (In x (map fst (c_ty Cg)) -> nlook x (c_ty Cg) <> Some tx)`. For an
    input G does not take this is #71's clause, since `c_ty Cg` has no
    entry for it.
  - Nothing else changes. A pin's type is still a type over G's own
    unmerged inputs, none of them made by the body, and any of them
    that is an input of F is fixed in F. `BFunClosure`'s agreement check
    is already per pin, and `link_site`'s table already puts the pins
    before `c_ty Cg`, so a pinned input of G reads at its pin, in the
    keys (#73) as in the promised types (`pin_reads`).
- **A chain is read in G's input terms.** In pin-taken-chain, G's body
  is `bind T = U; bind U = V; bind V = Old`. Each bind reads its type
  where it is made and writes only its own entry, so the body's final
  table is T ↦ U, U ↦ V, V ↦ Old, each type in G's input terms. Those
  are the pins. The body's `q` is `(U, V, Old)`, and the promised
  `q(T, U, V)` read through the pins is the same `(U, V, Old)`. At the
  bind site T = U = V = Old, so each pin agrees: T is Old and U reads as
  Old, U is Old and V reads as Old, V is Old and so is Old. Every
  application reads F's fixed inputs as the bind site did (rule 5), so
  every caller reads `q` as `(Old, Old, Old)`, which is what the body
  provides.
- **Why it is safe.** As #71: each pin is a promise that the body is only
  used where the pinned input means what the pin says, and fixed inputs
  make "where it is used" the same as "where it was bound". Whether G
  takes the input makes no difference to the promise.
- **Criteria.** Fidelity (section 6): both implementations run
  pin-taken-one and pin-taken-chain and reject the two disagreeing
  variants. The oracle's D84 3d forbids only P(F) ∩ M(F) and checks the
  agreement for every member of X(F) the body binds. Stability
  (section 5): each pin is checked against the bind site's meaning.
  Safety (section 1): `public_safety` is still proved.
- **Tests.**

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | [pin-taken-one](../tests/wasi/pin_taken_one.moss) | exit 0 | exit 0 | rejects (`BTypeBind`) | accepts |
  | [pin-taken-chain](../tests/wasi/pin_taken_chain.moss) (`bind T=U; bind U=V; bind V=Old`) | exit 0 | exit 0 | rejects (`BTypeBind`) | accepts |
  | [pin-taken-one-disagree](../tests/errors/pin_taken_one_disagree.moss) (main has `T = New`) | rejects ("it binds `T` to `Old`, and here `T` is `New`") | rejects ("the functor does not satisfy the required signature: `T`") | rejects (`BTypeBind`) | rejects (`BFunClosure`'s agreement) |
  | [pin-taken-chain-disagree](../tests/errors/pin_taken_chain_disagree.moss) (main has `V = New`) | rejects ("it binds `U` to `New`, and here `U` is `Old`") | rejects ("… signature: `U`") | rejects (`BTypeBind`) | rejects (`BFunClosure`: the pin `U = V` reads as New, and U is Old) |
  | `runtime_platform::private_checked_casts_preserve_chained_fixed_pin_meanings` (crates/moss-tests; the chain, with `Wasm`, `raw` and a `Ref[Boxed=T]` cast) | exit 0 | exit 0 | rejects | accepts |
  | [pin-fixed-agree](../tests/wasi/pin_fixed_agree.moss), [pin-fixed-disagree](../tests/errors/pin_fixed_disagree.moss) (#71) | q, rejects | q, rejects | accepts, rejects | accepts, rejects |

  Audit's verdicts are argued from the rules. The executable elaborator
  confirms them. With the change below, its faithful mode (`audit_run
  --frontend audit`) accepts pin-taken-one and pin-taken-chain, rejects
  both disagreeing variants with "G pins a fixed input of F to another
  type than it has here (Q6)", and runs the crates test's program (with
  the full prelude) on Audit's interpreter with exit 0. Without the
  change it rejects all four examples and the crates program with
  "bind: a pinned input of the bound functor (D84 3d)".
- **Corpus.** The executable elaborator (branch `elab` at 5d902a6) and
  the same elaborator with the change below, `--frontend audit
  --elab-only`, over `src/main.moss`, `tests/`, `examples/` and
  `bootstrap/examples/` (525 files): 521 same and 4 diff. The diffs are
  the four pin-taken examples: pin-taken-one and pin-taken-chain are
  now accepted, and the two disagreeing variants are still rejected,
  now by the agreement check instead of `BTypeBind`. No file is newly
  rejected, so no corpus program binds a fixed input that G takes at a
  reading that disagrees with its bind site.
- **Proof.** `public_safety` is still proved, and `check.sh` is green.
  #93 expected a larger proof change, because `PinRun` and `PinT` read a
  pin's type in the body's final environment d3 (`ground d3 tx`), which
  #93 took to be wrong for a chain. It is not wrong. A run of binds writes
  type *meanings* only at the destinations of applications
  (`PublicChainCore.bindings_writes`), and `bind_type` writes only an
  alias. So d3's meaning of each of G's own unmade inputs is still the
  application's (`PublicLinkRun.link_own_meaning`), and `ground d3 tx` is
  the pin read in input terms, which `PinsAgree` (from `BFunClosure`'s
  check) equates with the closure's fixed reading of the pinned input.
  `PinT`'s and `PinRun`'s proofs never used that a pin is not one of G's
  inputs. What did use it:
  - `PublicChainRealize.root_level`, `KeyX` and `RecvX`, sent a symbol
    that is an input of both F and G to G's table and used #71's
    "not taken" clause to show it is not pinned. They now case on
    whether the symbol is pinned first. A pinned one goes by `PinT`,
    whichever table it is also in.
  - `PublicFamilyLink.family_link_new_ok`'s `GE` did the same for a
    family's on-demand application (via `PinOut`, now removed). It also
    cases on the pins first, then uses `PinRun`.
  - `PublicBodySlice.LinkData`'s pin clause is `LinkBody`'s new one.
  No obligation statement changes, and no new premise is needed.
  `lookup_site_in` keeps its "not pinned" premise, which the callers now
  have from the case split.
- **The executable elaborator** (branch `elab`, `EaFunctor.link_body`)
  needs the same two changes, which the corpus run used
  (`scratch/elab-n5` on the n5 branch, not committed there): the frame's
  pins are `filter (fun t => nmem t pl) (mentions W fuel u)`, without
  `|| bound_in Cg t`; and `pins` takes every `x` of `fu` with
  `nlook x (c_ty Cb) = Some v`, except when `nlook x (c_ty Cg) = Some w`
  with `ty_eqb w v`. Its soundness argument for `link_body` follows the
  new `LinkBody` clause.
- **Class.** A relaxation toward both implementations (C/none), closing
  the fidelity gap recorded in #93.

### 96. A copy reads its source as F does at every application of F (`CopyInterpretation`; copyfix branch; corrects #32)
- **Rule now.** `bind F = H;` (`BFunCopy`) checks one definition,
  `CopyInterpretation W sigma u h Cu nsh Ch rm`, which replaces
  `CopyOutputs` (#35) and the two input-reading clauses of #32.
  - `sigma` is F's fixed inputs read at the copy site, as before. Every
    other generation of F's table readings reads as a fresh symbol `cg y`.
    `copy_subst sigma ys cg` is that reading: what every application of F
    has in common, since each keeps F's fixed inputs (rule 5) and supplies
    the rest afresh.
  - H's terms are read through F's merge (`canon Cu`, H's inputs being
    among F's) and then at `copy_subst`. There, each of H's merges holds,
    each of H's needs has F's provision, and H's result gives each of F's
    keys F's provision.
  - H's result is read in its own context `Ch`, where its merges hold, and
    no longer in F's (`ProvideResult W h Cu`, which reads H's result as if
    F's merge were H's). A position j of H's serves F's key k when H's key
    there is k at `copy_subst`. Every position of H's with that key must be
    one key in H's own terms, and j is the last (the oracle's
    `find ... (rev gkeys)`). This replaces the `CollidesOnlyAs` clause on
    H's result read at `Cu`.
- **Why.** #32 compared H's readings with F's in F's declared context:
  `canon Cu (TAbs x) = canon Cu tx`. That asks H's merges to hold for every
  instantiation of F's inputs, but an application of F never changes F's
  fixed inputs. In `copied_input_merge_must_hold_for_every_target_application`
  (crates/moss-tests), H merges T with U and F does not. F's brackets are
  empty, so T and U are fixed, and where F is bound both are `Old`. Every
  application of F reads them as `Old`, so H's merge holds wherever F's
  binding runs. Both implementations accept the program, and #32 rejected it.
  The same over-strictness made the needs clause depend on H's merge
  orientation: with the merge {T, U} represented by U, H's need `v[P=U]`
  read through F's merge is not F's `v[P=T]`.
- **Criteria.** Fidelity (section 6): the oracle's `copy_output_compatible`
  compares inputs, needs and outputs under one `generic` substitution
  that solves F's fixed inputs at the copy site and reads every other
  generation as a fresh `@copy` symbol. That is `copy_subst`. Stability
  (section 5): a fixed input means what it meant at the copy site, which
  is what the comparison uses. Coherence (section 2): reading H's result
  in `Ch` takes H's merges as H takes them, and the comparison does not
  depend on either context's representatives. Safety (section 1):
  `public_safety` is still proved.
- **What still holds, and how it is proved.**
  - The safety argument used #32's syntactic clauses: a valuation T that
    satisfies F's table satisfies H's (`tableok_down`), and H's needs are
    F's needs read through F's merge. Both now hold only at an
    application's meanings, which read F's fixed inputs as the copy site
    did. `PublicCopyApply.copy_read_at` transfers the copy's comparison
    from `copy_subst` to any such T: every generation the compared terms
    mention is a generation of F's table readings (`canon_gens_read`), so it
    is a fixed one that `sigma` reads (meaning, at T, what the copy site's
    meaning gives it) or one of `ys`. `copy_step_realizes` then takes
    H's merges and needs at T (`prov_shape`).
  - The chain proofs (PublicChainSigs, PublicChainRealize) relate H's
    result at `Ch` with the copy's roles to H's result at `Ch` with its own
    roles, entry by entry (as they did for `Cu` and `Ch` before), and F's
    result to H's by the copy's equation at T. `PublicLinkRun.link_run_fact`
    now asks the root's merges only of the application's ground meanings,
    which `TableOk Tin` gives, so `chain_real` no longer needs θ to satisfy
    each level's table syntactically.
  - `PublicSameNeed.copy_need_unambiguous` is deleted. It is false now (a
    need class of H may meet two of F's needs that F's fixed inputs make
    one key), and nothing used it. Such a copy is still coherent in what it
    runs: the two needs are answered under one key at every application.
  - The substitution lemmas moved from PublicCopyFresh to the new
    PublicCopySubst, which PublicCopyApply can import.
- **Tests.**

  | program | oracle | native | Audit before | Audit now |
  |---|---|---|---|---|
  | [copy-site-merge](../tests/wasi/copy_site_merge.moss) (F fixes X, Y; both Char where F is bound) | prints 7 | prints 7 | rejects (#32) | accepts |
  | [copy-site-apart](../tests/errors/copy_site_apart.moss) (X, Y two types where F is bound) | rejects ("input interpretation differs") | rejects (`?X … copy X`) | rejects (#32) | rejects (the input reading at `copy_subst`) |
  | [copy-merge](../tests/errors/copy_merge.moss) (X, Y polymorphic in F) | rejects ("input interpretation differs") | rejects (`!P … F`) | rejects (#32) | rejects (X, Y read as distinct fresh symbols) |
  | `runtime_platform::copied_input_merge_must_hold_for_every_target_application`, `F[]` | exit 0 | exit 0 | rejects | accepts |
  | the same, `F[T,U]` | rejects ("input interpretation") | rejects | rejects | rejects |

  The executable elaborator confirms Audit's verdicts: a prototype of the
  change in `EaFunctor` (`copy_interpretation`, on sw-driver at 982db5f)
  accepts and runs copy-site-merge (prints 7), rejects copy-site-apart
  and copy-merge, and passes the whole suite (362 passed, 0 failed, 1
  ignored), including the test above, which failed before.
- **Class.** A relaxation toward both implementations.

---

### 97. Needs at a site-made merge were matched by syntax (linkfix branch)

This records the original verdicts. #119 fixes the copy pair. The linked
body case still needs site-aware matching.
- **The programs.** Four examples, each with a header giving every
  implementation's verdict:

  | program | oracle | native | Audit | moss-ref |
  |---|---|---|---|---|
  | [copy-site-method](../tests/wasi/copy_site_method.moss) (H's merge of T and U represented by U) | rejects | accepts | accepts | rejects |
  | [copy-site-method-flip](../tests/wasi/copy_site_method_flip.moss) (the same, H's entries swapped) | accepts | accepts | accepts | accepts |
  | [link-site-merge](../tests/wasi/link_site_merge.moss) (the closure analogue: G merges F's fixed T and U, and its body uses the merge) | rejects | prints 1006 | rejects | rejects |
  | [copy-unmentioned-apart](../tests/errors/copy_unmentioned_apart.moss) (inputs neither fixed nor mentioned, applied apart) | rejects | rejects | rejects | rejects |

- **Coherence (criteria section 2).** #96 asks H's merges to hold only at
  the copy site, no longer in F's declared context. BFunCopy's needs
  clause still asks `In (canon_item Cu k) nsu`, so H's need `T.m` reads as
  `U.m` or `T.m` according to the representative H's merge picks. Audit
  accepts both orders only because `Elaborates` chooses the world, so some
  orientation passes; an elaborator that fixes one orientation (Ea, the
  oracle) accepts one order and rejects the other. Before #96 the
  clause was orientation-free, as `need_reading_orientation_free_G` is
  for closures.
- **Fidelity (criteria section 6).** `LinkBody`'s linked-input clause still
  compares G's readings with F's in F's declared context, as #32 did for
  copies, so link-site-merge is rejected although every application reads
  T and U as the bind site does. Loosening it as #96 did would make the
  split between linked and captured needs (`BFunClosure`, and `LinkBody`'s
  3b and provision-agreement clauses) depend on the representative too.
  That changes behaviour, not only acceptance: a linked need is supplied
  at each application, a captured one keeps the bind site's value.
- **No soundness gap.** copy-unmentioned-apart and its closure analogue
  are rejected everywhere: `copy_subst` reads inputs F's result does not
  mention as fresh symbols.
- **The fix, and why it is not made here.** In both rules, match each need
  to the unique need of F it equals when read at the copy or bind site,
  and take the matched need, not `canon_item Cu k`. That is copyfix-sized
  proof work twice (`NeedsAmong`, `CopyInterpretation`'s needs clause,
  `LinkBody`'s clauses; PublicCopyApply, PublicBodySlice, PublicChainCore,
  PublicFamilyStep, PublicLinkRun, PublicFamilyLink, PublicChainRealize,
  PublicFamilyCopy; `PublicSameNeed.link_need_unambiguous` would become
  false). The representative-free refactor (merges as classes, compared
  up to the congruence) states exactly this matching, so the fix is
  folded into it, with these four examples among its acceptance tests.

### 98. Recorded: copy interpretation and pins in a merged class

These are baselines for the representative-free refactor. The flipped
reference differs from the default only in the result of the `s_orient`
callback in `EaBase.default_strategy`. The Audit rules are unchanged.

| program | native | default moss-ref | flipped moss-ref |
|---|---|---|---|
| [copy-mixed-class](../tests/wasi/copy_mixed_class.moss) | runs | rejects D78 | runs |
| [pin-merged-class-yz](../tests/wasi/pin_merged_class_yz.moss) | runs | runs | runs |
| [pin-merged-class-zy](../tests/wasi/pin_merged_class_zy.moss) | runs | runs | runs |

In the copy, F merges fixed X with polymorphic Y. H promises `out` at Old
for both inputs and the copy site fixes X to Old. `CopyInterpretation`
first canonicalizes F's types and then applies `copy_subst`. With Y as
the representative, the reading can use Y's fresh type and miss the
fixed X. The two reference verdicts demonstrate the coherence failure
(criteria section 2). The class reading should use the fixed meaning of
X at this site.

In the pin cases G pins T to Y from its captured Y = Z class.
`LinkBody` tests whether the pin's symbol has no outgoing substitution
row. The class representative meets that test even for a class of two.
The two orderings record a fidelity requirement (criteria section 6)
for the replacement rule. A rule requiring a singleton would reject
both examples.

For #97, flipping the reference swaps the verdicts of both copy-site
orders, both `closure_partial_class` orders and both
`closure_partial_unbound` orders. The other existing tests whose verdict
changes are `bind_merge_unbound`, `functor_capture_merged_fixed` and
`linked_merge_captured`. In the survey of 528 repository source files
under `tests` and `examples`, these are the nine exit-code differences.
Both orientations reject `link_site_merge` and `copy_unmentioned_apart`
today. The refactor's baseline is to accept both closure and copy
orders, run `link_site_merge` with output 1006, and keep rejecting
`copy_unmentioned_apart`.

### 99. Recorded: unbound assumption requirements and main's abstract merges

| program | native | default moss-ref | flipped moss-ref |
|---|---|---|---|
| [assumed-unbound-class](../tests/errors/assumed_unbound_class.moss) | rejects | rejects D20 | rejects D20 |
| [main-symbol-merge](../tests/wasi/main_symbol_merge.moss) | runs | runs | runs |

`AssumedComplete` specifies a reading through the representative when no
member of the class is in force. The assumption control is rejected at
D20 before it reaches that reading. It is evidence for the earlier
requirement check. A source witness that reaches the fallback is still
open. The planned class reading uses one fresh atom per class for that
case.

The main probe boxes a value at `Phantom[X=X]` and dereferences it at
`Phantom[X=Y]` under `assume X = Y`. All three runs succeed. A realization
using one fresh atom per abstract class must preserve that behaviour
(criteria sections 2 and 6).

### 100. The root context and Bool's layout are language items (`LIRoot`, `LangItemsFormed`; sep branch, S0; criteria section 8)
- **Rule now.**
  - `lang_item` gains `LIRoot`, keyed `("wasm", NSName, "Gc")`: main's
    root context (note 54, [D84] (8)). `RootCtx`'s two cases read
    `LangItem W LIRoot gc` where they read `LangSym W "wasm" NSName "Gc" gc`.
  - Bool's layout clause (Bool is a tag with no requirements whose payload
    is the union of True and False, two distinct units) leaves
    `Platform.Layouts` and becomes `Elaboration.LangItemsFormed W`, a
    conjunct of `ElaboratesIn` right after the Bool symbols clause.
    `Platform.Layouts` keeps only the wasm facts (its abstract types and
    the handle tags `Ref`, `Array`, `Addr`).
- **Why.** Both are facts the language's own rules need, not facts about
  one platform (criteria section 8). The root context is the language's
  notion of what main assumes implicitly; which item supplies it is the
  language-items table's job, as for Bool (#92). A platform parameter here
  would make `CtxOf`, and with it all of `Elaboration`, platform-dependent
  (the owner's decision 3 on the separation design). Bool's layout is what
  `Cond` and `truthy` rely on (D45), so a platform that named its library
  differently would still have to meet it. This is step S0 of the design
  in `scratch/SEP-DESIGN.md` (git-ignored), which moves the rest of
  `Platform` into a separate `Base` instance in later steps.
- **Fidelity.** `LangItem W LIRoot gc` reduces to the previous
  `LangSym` premise. `Builds`, `Ready`, `Admissible` and `Compatible`
  take `ElaboratesIn ∧ Platform.Linked`, with the same Bool premise
  before and after the regrouping. Ea reads the root through the same
  key and checks the same Bool layout at the same diagnostic position.

  | Program | Native before → after | moss-ref before → after | Audit before → after |
  |---|---|---|---|
  | [gc_tree](../tests/wasi/gc_tree.moss), implicit Gc and Bool branches | same output | byte-identical output | equivalent build and run predicates |
  | [gc_array](../tests/wasi/gc_array.moss), implicit Gc and arrays | same output | byte-identical output | equivalent build and run predicates |

- **Proof.** `public_safety` is still proved, and `check.sh` is green.
  `PublicBoolTyping.BoolSymbols` now carries `LangItemsFormed` beside the
  Bool symbols (`elaborates_bool_symbols` reads both off `ElaboratesIn`),
  so the body slice's `Bools` hypothesis supplies Bool's layout and
  `truthy_typed` no longer takes `Platform.Layouts`. `bool_value_shape`
  and `PublicPrimitiveSafety.bool_not_cast` take `LangItemsFormed`.
  `AuditBuildExample.main_lang_items` proves it of the example world (it
  loads no `lib/bool.moss`).
- **Class.** None (definitional, and a regrouping).

### 101. Machine types carry their declaration symbols (Base separation S1)

- **Rule.** `TMachine s` is an inert type identified by its declaration
  symbol `s`. The program and its execution view carry a table from
  machine type symbols to scalar sorts or reference kinds. `InitialTypes`
  substitutes `TMachine s` for each table key `s`. The platform's table
  identifies the four Wasm scalar types and the two GC reference types.
  Each cast entry checks the machine type of its own raw handle.
- **Why.** The language reads the table and uses declaration identities
  (criteria section 8). The platform supplies the Wasm names and classifies
  their symbols. The fixed-machine premises and the entry safety proof use
  the same table as the executable elaborator. The optimized interpreters
  retain it when transforming a program. The host adapter preserves a
  declaration symbol in the legacy machine's checked handle header.
- **Alternatives.** An abstract type represented as `TAbs s` would be read
  again under later substitutions. The separate constructor preserves
  its meaning across rebinding, including when a saved type appears in a
  reference header. The table associates that constructor with the
  platform's representation at the original declaration.
- **Fidelity.** On a linked program, the selected declarations are distinct
  by `Platform.Layouts`. Each former machine role corresponds to its own
  declaration symbol. The role of a header's type can therefore be
  recovered from the program table. The following checks exercise both
  kinds of reference and all four scalar representations.

  | Program | Native before → after | moss-ref before → after | Audit before → after |
  |---|---|---|---|
  | [float](../tests/wasi/float.moss) | same output | byte-identical output | scalar role → its declaration symbol |
  | [gc_tree](../tests/wasi/gc_tree.moss) | same output | byte-identical output | struct role → its declaration symbol |
  | [gc_array](../tests/wasi/gc_array.moss) | same output | byte-identical output | array role → its declaration symbol |

- **Proof.** `public_safety` uses the table for value typing, cast payload
  typing, primitive signatures, and grounding the fixed machine inputs of
  platform families. The certificate checks the exact public statement
  and its kernel-primitive assumptions. The unused
  `AuditRepresentation.v` relation for the old elaborator is removed.
- **Class.** Representation change (criterion 8, platform separation).

### 102. Scalar bits and reference headers use platform-neutral carriers (Base separation S2)

- **Rule.** `VScalar sort bits` carries a scalar's sort and bits.
  `VRef address header` carries an address and an optional header with
  its reference kind and held type. Cast entries and the heap laws use
  numeric kinds. Base supplies sorts 0 through 3 for I32, I64, F32 and F64,
  and kinds 0 and 1 for boxed and array references.
- **Why.** The language can carry values of the sorts and kinds in the
  program's machine table (criteria section 8). Value typing reads that
  table. A cast checks the kind and held type in a reference header against
  the cast entry. The host continues to supply primitive operations and
  heap behavior.
- **Alternative.** Separate scalar constructors expose Base's layout in
  the language's value type and require a language edit for each additional
  sort. A sort and a bit payload give platforms one shared representation.
  The reference executable uses Zarith integers so that I32 bits fit in a
  native integer. The OCaml adapter uses the legacy model's canonical NaN
  patterns and preserves the bits of other Wasm floats.
- **Fidelity.** The scalar encoding preserves the four Base sorts. A raw
  reference has an absent header, and the two prior Boolean kinds map to
  0 and 1. The fixtures below exercise both kinds, floating-point rounding,
  and a cast that rejects a handle at the wrong held type.

  | Program | Native before → after | moss-ref before → after | Audit before → after |
  |---|---|---|---|
  | [float](../tests/wasi/float.moss), [newton](../tests/wasi/newton.moss) | same output | byte-identical output | scalar constructor → sort and bits |
  | [gc_tree](../tests/wasi/gc_tree.moss), [gc_array](../tests/wasi/gc_array.moss) | same output | byte-identical output | Boolean header kind → numeric kind |
  | [saved_type_rebinding](../tests/wasi/saved_type_rebinding.moss) | rejects → rejects | trap → same trap | the encoded header fails the same held-type check |

- **Proof.** `public_safety` uses one typing rule for scalars and reads
  reference kinds from the machine table. The certificate checks its exact
  public statement and kernel-primitive assumptions. The lazy cast proof
  checks the same kind and held type as the reference cast.
- **Class.** Representation change (criterion 8, platform separation).
### 103. Formed contexts carry merge classes (merge refactor M2)

- **Rule.** `CtxOf`, `FunCtx`, and `AssumeCtx` pair the old most-general
  substitution with a class presentation `M`. A class records its
  generation members and, for a structured class, a type subterm written
  in the telescope. `MergeEq M` is the congruence generated by those
  equations. `Solved M` gives the classes disjoint membership and a rank
  that decreases along shape dependencies. `MergeOf` checks the
  telescope's equations and collisions and makes the resulting
  congruence principal: two types are equivalent exactly when every
  unifier of the telescope equates them. See
  [Audit.v](theories/Audit.v) and [PublicMerge.v](theories/PublicMerge.v).
- **Temporary section.** The context records `c_merge` beside `c_sub`
  and requires `SectionOf M c_sub`. The section reads each generation
  at an equivalent type and gives equal types the same spelling.
  `canon C t = canon C u` is therefore equivalent to
  `MergeEq (c_merge C) t u`. Derived contexts copy the presentation with
  the substitution, and `StaticOk` carries the section through the
  safety proof. The existing context tables, need keys, and run-time keys
  still use the old spelling in this stage. See
  [PublicContextFacts.v](theories/PublicContextFacts.v) and
  [PublicBodySlice.v](theories/PublicBodySlice.v).
- **Elaborator.** `EaCtx.merge` returns its original closed substitution
  alongside a class presentation computed from that substitution and
  the raw telescope. The empty substitution gives the empty presentation
  directly. For other substitutions the presentation groups symbols by
  their closed readings and finds a raw subterm with the right reading
  for each structured class. When the substitution is most general, the
  construction proves both `MergeOf` and `SectionOf`. See
  [EaCtx.v](theories/EaCtx.v) and
  [PublicMergePresent.v](theories/PublicMergePresent.v).

  The checked late-collision example has the following raw entries.

  ```text
  X = Old
  A = B
  A.m : () -> Y
  B.m : () -> Box[X]
  ```

  The first round makes the equations hold. The second sees the receiver
  keys collide and can store `Y = Box[Old]` after reading the signatures
  under the first round's closure. The raw shape is `Box[X]`. Computing
  `EaCtx.merge` in Rocq under either orientation gives a class for `Y` whose shape is `Box[X]`, and
  its equality checker equates `Y` with `Box[Old]`.
- **Checks.** The extracted reference matched all five recorded M0
  fixtures. Both pin orders and the main symbol merge run as before.
  The unbound assumption still fails at D20. The mixed copy retains its
  recorded D78 rejection in the reference and acceptance in the
  self-hosted compiler. The exact public safety certificate also passed.

  The same-source `--elab-only --time src/main.moss` comparison used
  foundation commit `8a61e32` and the M2 driver, with an unlimited stack
  for both. Each of the six runs reported 1,732 functions and five
  functor bodies. These are elapsed elaboration times in seconds.

  | Run | Foundation | M2 |
  |---|---:|---:|
  | 1 | 105.895 | 100.394 |
  | 2 | 101.192 | 104.171 |
  | 3 | 125.266 | 146.091 |

  The medians were 105.895 and 104.171 seconds.
- **Class.** Representation change. The old substitution and acceptance
  checks still drive elaboration in this stage.

### 104. Primitive code carries named operations (Base separation S3)

- **Rule.** An `Op` carries a name and platform operands. Bindings,
  primitive closures, and the interface use that operation identity.
  `WasmOps` contains the typed Wasm operations and their codec.
  `WasmSignatures` supplies their signatures. Allocations carry their tag
  symbol. The Base encoding for an unsupported operation carries its
  original diagnostic index in a reserved operand form.
- **Why.** Primitive code can use the same carrier across platforms
  (criteria section 8). Ea checks the existing Wasm signatures and
  encodes the operation when it emits the binding. The interpreter's
  current call adapter decodes the operation for the existing GC
  argument and result conventions.
- **Alternatives.** Resolving a name at each call repeats the same work.
  A shared hash cache avoids repeated parsing but still hashes hot keys.
  The extracted operation holds its immutable name and operands plus
  a typed memo slot. The executable resolves required operations before
  running, and the call adapter and host share the memoized result.
- **Fidelity.** The supported codec round-trips every Wasm operation.
  Unsupported operations retain their exact numeric outcome, including
  when two entries in a relational program's diagnostic table use the
  same name. The displayed name still comes from the table at that index.

  | Program | Native before → after | moss-ref before → after | Audit before → after |
  |---|---|---|---|
  | [float](../tests/wasi/float.moss) | same output | byte-identical output | Wasm scalar operation → its named encoding |
  | [gc_tree](../tests/wasi/gc_tree.moss), [gc_array](../tests/wasi/gc_array.moss) | same output | byte-identical output | allocation operation → name and tag operand |
  | [saved_type_rebinding](../tests/wasi/saved_type_rebinding.moss) | rejects → rejects | trap → same trap | same cast result through named allocations |
  | [unsupported-operation probes](../crates/moss-tests/tests/suite/runtime_platform.rs) | oracle fixture | each name reported with exit 4 | same unsupported index |

- **Proof.** `PublicOperationFacts.v` proves the supported round trip,
  the decoder's canonical inverse, and exact recovery of unsupported
  indices. The reference proof decodes before applying the existing
  primitive safety lemmas. The certificate checks the exact public
  safety statement and its kernel-primitive assumptions.
- **Class.** Representation change (criterion 8, platform separation).

### 105. Base's host laws are independent of the interface record

- **Rule.** `BaseLaws` defines the primitive-result predicate and the
  host laws, parameterized by the host's state, supported operations,
  primitive step and heap predicate. `Public.interface_laws` applies
  this contract to the interface's fields.
- **Why.** The primitive signatures and heap behavior describe Base's
  host contract ([criteria section 8](../docs/design/criteria.md#8-the-language-is-separate-from-its-platform-library)). Naming that contract directly
  prepares the laws component of the platform interface.
- **Fidelity.** The public predicates reduce to their previous bodies.
  Rocq accepts `eq_refl` witnesses comparing both predicates with the
  definitions read from 401e385. Their clauses cover the same primitive
  outcomes, unsupported indices and heap obligations. The clean extracted
  executable is byte-for-byte identical to the build from 401e385.

  | Program | Native before → after | moss-ref before → after | Audit before → after |
  |---|---|---|---|
  | [float](../tests/wasi/float.moss) | same output | same output | definitionally equal host contract |
  | [gc_tree](../tests/wasi/gc_tree.moss), [gc_array](../tests/wasi/gc_array.moss) | same output | same output | definitionally equal heap laws |
  | [saved_type_rebinding](../tests/wasi/saved_type_rebinding.moss) | rejects → rejects | trap → same trap | definitionally equal outcome laws |

- **Proof.** The certificate checks `public_safety` against the exact
  `Public.CORRECT.safety` type, with only kernel primitive assumptions.
  The extracted implementation uses the existing value and primitive
  representations. These relocated predicates have type `Prop`.
- **Alternative.** Parameterizing `Public` by a full platform also
  requires adapting its consumers and the interpreter interface. The
  state-parameterized law definition supplies that contract component
  while preserving the current public module interface by conversion.
- **Class.** None (definitional refactoring).

### 106. Base owns the primitive call adapter (Base separation S4a)

- **Change.** `BaseCall` owns `raw_receiver`, `primitive_arguments`,
  `allocation_result` and the callback-based `call` operation. The interpreter's
  `run_prim_at` is a transparent application of this operation to its primitive
  implementation. Primitive safety refers directly to the Base adapter.
- **Why.** Argument unwrapping and allocation headers are Base's calling
  convention (criterion 8). Keeping their implementation outside the language
  interpreter establishes the boundary needed for the generic site argument.
- **Equivalence.** The adapter's old body and the new `BaseCall.call` reduce to
  the same term for every host state type, primitive callback, program, operation,
  optional site, argument list and initial state. All signals and returned states
  are identical, including malformed arguments, unsupported operations and traps.
  The reduction check copies the old body from `ace4442` and closes by reflexivity.
- **Fidelity.** This step changes the location of the adapter. The interpreter's
  fuel, primitive closure construction and Base calling convention retain their
  existing definitions after unfolding the transparent entry.

  | Program | Native before → after | moss-ref before → after | Audit before → after |
  |---|---|---|---|
  | [gc_tree](../tests/wasi/gc_tree.moss), [gc_array](../tests/wasi/gc_array.moss) | same implementation | same call result by reduction | same result and state |
  | [saved_type_rebinding](../tests/wasi/saved_type_rebinding.moss) | same implementation | same cast trap by reduction | same result and state |
  | [float](../tests/wasi/float.moss) | same implementation | same primitive result by reduction | same result and state |

- **Proof.** The primitive-safety proofs use the adapter's new namespace. The
  pass proofs unfold its transparent call entry. The certificate retains the
  exact public safety statement and its assumption audit.
- **Alternatives.** Changing the primitive API and all closure-site storage in
  one step would couple this move to the FastPath simulation. Those changes are
  the next S4 slice, with their own checks.
- **Class.** Definitional relocation (criterion 8, platform separation).

### 107. Primitive execution receives its site through the host boundary (S4b)

- **Change.** `PRIMITIVES.run_prim` takes the optional type recorded in its
  closure. The interpreter forwards that site directly. `ReferenceHost` and the
  executable host apply `BaseCall.call` around their raw primitive step.
- **Why.** The language interpreter can forward the platform's site information
  without inspecting operation names or implementing Base's argument and result
  conventions (criterion 8). The external interface's raw primitive contract and
  host laws remain the same.
- **Equivalence.** For the public reference host, unfolding its new primitive
  entry gives exactly the previous `BaseCall.call` around the same raw callback.
  The closure site, arguments, state, outcome and fuel are preserved. The native
  host makes the same call around its existing conversion and raw MossX step.
- **Fidelity.** Existing closure construction still records the site for the two
  allocating operations. Generic site recording for other methods is a separate
  step because the FastPath proof currently uses those closures' absent sites.

  | Program | Native before → after | moss-ref before → after | Audit before → after |
  |---|---|---|---|
  | [gc_tree](../tests/wasi/gc_tree.moss), [gc_array](../tests/wasi/gc_array.moss) | same implementation | same adapter around the raw step | same reference result and state |
  | [saved_type_rebinding](../tests/wasi/saved_type_rebinding.moss) | same implementation | same cast trap | same reference result and state |
  | [float](../tests/wasi/float.moss) | same implementation | same raw numeric step | same reference result and state |

- **Proof.** Pass simulations quantify over every optional site. A shared
  `base_call_state` fact proves that adapting the result preserves the state
  returned by the callback, and the pass invariants use its preservation
  consequence. The primitive safety proof unfolds the host boundary directly.
  The exact public safety statement and kernel assumption audit are retained.
- **Alternatives.** Passing a raw callback into the interpreter would leave Base's
  decoding and reference-header convention there. A separate raw callback in the
  host keeps the interpreter's operational rules generic while retaining the
  existing external interface and its heap laws.
- **Class.** Calling-convention relocation (criterion 8, platform separation).

### 108. Primitive method bindings record their receiver independently of the operation (S4c)

- **Rule.** `primitive_closure` and its lazy/fast representations record
  `Some (ground receiver)` for every method key. Other keys record None.
  The interpreter no longer decodes or classifies a platform operation to
  construct its closure. Uniform recording includes unknown operations.
- **Fidelity.** Base's call adapter observes sites only for allocation. The
  checked same-fuel erasure theorem preserves every public interface's
  observations when other stored sites differ. Ground type meanings, aliases,
  saved generations, result reading and primitive availability are retained.
- **Fast path.** Its relation permits precisely equality after Base site
  erasure; allocation sites must still agree exactly. Primitive-body helpers
  track the actual emitted sites, with unchanged fuel cutoffs. The callback
  site law is explicit in the generic proof and is proved by BaseCall for
  every allowed raw interface, including missing/unsupported operations.
- **Safety.** HeldAt records the ground receiver for every method binding.
  Preserve the exact public safety statement, all-fuel interpreter agreement
  and public pass roots, with kernel primitives only. Scalar compilation and
  slot dispatch retain Some sites and remain eligible.
- **Class.** Runtime representation change; no static rule relaxed and no
  intended Moss-observable difference. No additional public interface premise.

### 109. Integrate scalar/reference carriers with the current language (S2)

- **Rule.** Apply the representation from #102 to the current merge-class
  semantics, retaining #105's transparent BaseLaws and #106/#107's site-aware
  BaseCall boundary. Scalars carry a platform sort and bits; references carry
  an address and an optional numeric-kind/type header. No static acceptance
  rule or failure signal is relaxed.
- **Execution.** The native adapter retains `MossX.run_prim_fast` and current
  interpreter extraction. Only value/float-bit/reference conversion changes.
  Uniform receiver-site storage (#108, pending) is excluded.
- **Proof.** Preserve the exact public safety certificate, all interpreter/pass
  agreement roots, and same-fuel site-erasure and primitive-block facts.
- **Class.** Representation change; no intended Moss-observable difference.

### 110. Integrate named operations with current host laws and erasure proofs (S3)

- **Rule.** Carry platform operation names and symbol operands as in #104.
  Preserve #105's transparent BaseLaws and the existing site-aware ABI;
  interpret supported Wasm operations through encode/decode. Malformed or
  unknown operations retain their explicit unsupported outcome and code.
- **Execution.** Retain the fast raw host, scalar argument packing and literal
  compiled-interpreter extraction. A decoder-private typed memo caches the
  pure decode of immutable operation fields. Uniform site storage stays out.
- **Proof.** Port same-fuel site erasure and primitive-block facts through
  decode, including decode=None. Keep the exact public safety certificate
  and all interpreter/pass roots; no new assumptions or relaxed rules.
- **Class.** Representation change; no intended Moss-observable difference.

### 111. Static linking consumes a platform's tables, provisions and entry ABI (S5a)

- **Rule.** `Linking` requires the platform's `EntryABI`, `Tables` and
  `Provides` contract. It still reads main's context and machine types and
  requires the same initial environment. `Platform.Linked` uses the
  concrete instance of that generic rule.
- **Fidelity.** Base's `Tables` regroups the old layout and representation
  clauses. `Provides` pairs the old two checks for each entry. The checked
  `platform_linked_fields` equivalence recovers exactly the previous
  conjunction, preserving every `Builds` and `Compatible` instance.
- **Boundary.** The public and host functor signatures, runtime and
  executable are unchanged. This separates static linking only; the full
  runtime platform interface and physical Base file remain future work.
- **Class.** Equivalent regrouping (criterion 8, platform separation).

### 112. Public contracts consume one platform at build and run time (S5b)

- **Rule.** `PLATFORM` combines the static linking contract with its host
  heap type, raw-host laws and primitive call adapter. `HOST_FOR` and
  `PublicFor` consume that contract. The existing `HOST` and `Public`
  are transparent specializations to `Base`.
- **Fidelity.** The certificate checks by reduction that Base's linking,
  readiness, interface laws and primitive dispatch are exactly the previous
  definitions. The five public statements retain their Base interpretation;
  the existing exact public safety and interpreter/pass certificates check.
- **Boundary.** No new host assumption or executable change. This factors
  the statement's platform interface. Generic safety and a physical
  `AuditBase.v` split remain separate work.
- **Class.** Definitional factoring for the Base specialization.

### 113. Separate concrete Base definitions into AuditBase.v (S5c)

- **Rule.** `Audit.v` remains self-contained for the generic language,
  interpreter, elaboration, platform contract and five public obligations.
  `AuditBase.v` imports it and owns the existing Wasm codec/signatures,
  entry/tables, heap, call adapter, laws and Base `HOST`/`Public` aliases.
- **Fidelity.** This relocates definitions without changing their bodies.
  Keep the exact Base specialization and safety/engine/pass certificates.
  Extraction and native callers follow the new namespace, preserving the
  fast host callback, typed decoder memo and scalar/literal extraction.
- **Boundary.** Both trusted definition files are checked proof-free.
  Base proof facts are in `BaseFacts.v`; generic proof helpers stay in
  `AuditFacts.v`. The safety proof still certifies the concrete Base instance.
- **Class.** File and namespace relocation; no acceptance or runtime change.


### 114. Rule B reads each structured dependency through its merge class (M4a)

- **Rule.** A requirement is in force if a member of its merge class is
  bound. For a structured type, expand its structure and apply that same
  test to each abstract generation it mentions.
- **Example.** [A = B](../tests/wasi/rule_b_shape_alias_ab.moss) and
  [B = A](../tests/wasi/rule_b_shape_alias_ba.moss) differ only in the
  direction of one equation. The function needs `R = Box` and `Box`
  needs `A`. The caller binds `A`. Both old frontends rejected `A = B`
  and accepted `B = A`. Both now compile and run either spelling. Removing
  the binding or binding the aliases to distinct types still fails.
- **Proof.** In a solved merge, the reference's section makes equal canonical
  readings equivalent to class equality. The checked decision theorem uses
  this equivalence. The safety proof uses the caller's substitution to
  transfer the bound member's meaning to the required leaf.
- **Class C** (criterion 2, coherence). Both executable frontends use the
  same class check. The public safety certificate checks the revised rule.

### 115. Separate call and application typing from translation

- **Typing.** `NeedsSatisfied` checks that the caller supplies a callee's
  needs with the required types and meanings. Direct calls use this check,
  including calls to defined attached methods. `TypesApplication` uses it
  to check an abstract functor application and its result. This includes
  the identities of the result types and the application scope.
  `TypesFamilyApplication` records the static checks for a family use.
  `Demand` records the checked uses in dependency order.
  `translate_demands` emits the runtime applications from those uses.
- **Translation.** `satisfaction_map` pairs callee and caller run-time keys.
  `translate_call` emits direct calls with that map. `translate_application`
  uses it and the checked result to emit the binding. The current encoding of output identities is shared by typing
  and translation.
- **Fidelity.** The previous application rule checked the same conditions
  and required its satisfaction map to equal the computed one. Direct
  calls and applications now compute that map directly. The safety proof
  consumes the split rules, and the executable elaborator is unchanged.

### 116. Scope fresh identities in abstract applications and functor copies

- **Rule.** `BApplyAbstract` extends its allocation scope with one batch of
  output generations. `CopyInterpretation` allocates one local batch for
  its hypothetical outputs and unknown inputs, reserving the generations
  of the copy site's fixed meanings.
- **Fidelity.** The application produces the same frame as `with_fresh` by
  reduction. `copy_allocation_contract` recovers exactly the previous
  copy conditions, including the collision check in
  [copy_fresh_collide.moss](../tests/errors/copy_fresh_collide.moss).
- **Proof.** The allocation laws preserve earlier allocations, keep
  successive batches disjoint and transport allocation along injective
  renamings. `fresh_tables_allocate` relates the executable counter's two
  table allocations to the copy's batch, given that the starting counter
  lies above its reserved identities.
- **Class.** Equivalent factoring of the fresh identity conditions.

### 117. Call instantiation respects merge classes (M4a)

- **Rule.** \`CallTheta\` sends the equations of the callee's merge classes
  to equal types in the caller's merge. The requirement order and bound
  meanings still determine the other two clauses.
- **Fidelity.** In a formed callee context, each stored row is an effective
  row of its section. The checked equivalence recovers the previous test
  when the caller has a section too. The reference still runs that test.
- **Proof.** The checked reference call produces \`Respects\` directly.
  Callee realization and demand scoping use preservation of class equality
  under the call substitution. Public safety still checks.
- **Class.** Equivalent comparison on formed contexts (criterion 2).


### 118. Fixed inputs and lambda annotations compare by merge class (M4b)

- **Rule.** A fixed input's current and recorded meanings belong to the same
  merge class. An optional lambda annotation agrees with the corresponding
  declared parameter or result by the same type equality.
- **Fidelity.** The current executable tests the two canonical readings.
  This is equivalent in a formed context. The fixed-agreement decision
  theorem also checks that the metadata reader enumerates exactly the
  functor's fixed inputs.
- **Proof.** Copy and family applications use the realizing environment's
  equality on merge classes to transport each fixed reading.
- **Class.** Equivalent comparison on formed contexts (criterion 2).

### 119. Copy needs match at the binding site (M4c)

- **Rule.** The selected target need must give the same key, signature
  and instantiation as the source at the copy site. If several candidates
  agree there, select the unique one already equal in the declared merges
  of the two functors. Reject if this comparison leaves a choice. The
  executable emits the validated target's runtime key.
- **Acceptance.** [Default order](../tests/wasi/copy_site_method.moss) and
  [flipped order](../tests/wasi/copy_site_method_flip.moss) bind two fixed
  inputs to Old. H merges those inputs, and F has just one need for the
  method. The old reference rejected the default order. Both now apply
  the copy and call the method. The self-hosted compiler accepts both.
- **Ambiguity.** Two keys can coincide where the copy is bound and still
  denote distinct methods in a function that assumes the inputs
  separately. [Distinct keys](../tests/wasi/copy_site_distinct_needs.moss)
  and [swapped needs](../tests/wasi/copy_site_distinct_needs_flip.moss)
  both print 1, preserving the method H declares. If H's own context
  equates those inputs, neither target is preferred. The old reference
  printed 2 or 1 depending on the orientation of H's equation.
  Both orientations now reject.
  [T = U](../tests/errors/copy_site_ambiguous_needs.moss),
  [U = T](../tests/errors/copy_site_ambiguous_needs_flip.moss).
  The self-hosted compiler rejects all four with a method lookup error.
- **Controls.** A copy whose extra input merge could fail at application
  still rejects, as does one that merges distinct output keys.
  [Unmentioned inputs](../tests/errors/copy_unmentioned_apart.moss),
  [polymorphic inputs](../tests/errors/copy_merge.moss),
  [output collision](../tests/errors/copy_result_collide.moss).
- **Proof.** The checked choice is unique. The combined merge preserves
  the equations of each declared context, independently of its
  presentation. The copy substitution transfers the matching receiver
  and provision agreement to every application, where the safety proof
  realizes the selected target key.
- **Classes B and C** (criterion 2). #98's mixed fixed and polymorphic
  class still uses the old site reading. Linked bodies still use #97's
  rule.

### 120. Assumptions give an absent atomic class a fresh reading (M4d)

- **Rule.** `AssumedComplete` assigns one local atom to each atomic class
  that occurs in an expanded requirement and has no member in force.
  Distinct classes get distinct atoms. Allocation reserves the caller's
  and callee's type support, and structured readings use the same atoms.
- **Proof.** `choose_assumption_atom_sound` checks the scope transition
  and exact class coverage. `assumption_theta_sound` checks the resulting
  readings and call instantiation. `assumes_complete_sound` connects them
  to the declaration sweep given the context and provision checks.
- **Fidelity.** The [Moss probes](theories/EaAssumptionExample.v) exercise
  both merge orientations. An unused detached method forms with a fresh
  reading in both. An omitted method need is still
  rejected and names the same source class member in both orientations.
- **Class.** Scoped assumption readings (criterion 2).



### 121. Copy readings use the fixed meaning of an atomic class (M4c)

- **Rule.** An atomic class reads at the copy site as the common meaning
  of its fixed members. It stays symbolic if it has no fixed reading or
  if its recorded fixed readings differ. The section supplies the current
  storage slot for that class.
- **Example.** F merges fixed X with polymorphic Y. X is Old where the
  copy is bound, so the output must read at Old even when the section
  spells the class as Y. The reference rejected that orientation. Both
  now apply the copy and call the result. The self-hosted compiler
  accepts both. [X = Y](../tests/wasi/copy_mixed_class.moss),
  [Y = X](../tests/wasi/copy_mixed_class_flip.moss).
- **Proof.** An effective fixed row has a source member of its class
  and one meaning per slot. The application reads the source member
  through its fixed capture and every equal member through the same
  class. Freshness still reserves the original fixed meanings.
- **Class C** (criterion 2). The existing input, provision and result
  comparisons now use the class reading.

### 122. Type forwarding providers before assigning runtime code

- **Rule.** `TypesForwarder` checks the abstract input and its signature in
  the context and identifies its generated forwarder by that input.
  `TranslatesForwarder` assigns a fresh number to the forwarder's body in
  the program. `FPForward` consumes these two judgments.
- **Proof.** The executable type reader is independent of the runtime
  slot. On a cache hit the translator reuses a slot holding the same
  forwarder. On a miss the shared counter allocates and fills a vacant
  slot. The successful assembly collects both code tables into the
  audited program.
- **Fidelity.** The [forwarder probe](theories/EaForwarderExample.v)
  parses Moss and elaborates the same source provider twice. The second
  translation returns the same slot and leaves the counter as it was.
- **Class.** Equivalent factoring of typing and translation.

### 123. Type lambda captures in an explicit allocation scope

- **Rule.** `TypesLambdaCaptures` finds the enclosing locals mentioned by a
  lambda and allocates their value identities in one batch. The scope
  reserves the enclosing context's value keys and the declaration names.
- **Translation.** The capture prefix emits the bindings that take the
  locals' current values. The generated body loads the captured values
  into its own numbered locals. The executable uses these same two
  translation definitions.
- **Proof.** The allocation condition is equivalent to the old distinctness,
  undeclared-symbol and unavailable-value checks. `lambda_captures_from_counter`
  verifies both the selected local names and their allocation when the
  counter lies above the support.
- **Class.** Equivalent factoring of capture typing and translation.

### 124. Closure bindings have separate typing evidence (M4c)

- **Rule.** `LinkBinding` checks capture scope and the agreement of linked
  inputs and provisions. `LinkBody` checks the body and its result. A
  closure requires both judgments. A linked body in the cycle graph still
  requires binding evidence.
- **Executable.** `check_link_binding` checks the source binding before
  looking up the linked body. The body remains memoized by the declared
  target and source pair. The emitted closure uses the body's typed slot.
- **Proof.** `check_link_binding_sound` establishes the three checked
  conditions and preserves the elaborator state. The safety proof takes
  binding evidence through the closure and application chain. The body
  preservation lemma takes equality of the actual input readings, which
  follows from the existing declared input condition.
- **Class.** Equivalent factoring. Binding conditions and runtime
  translation stay the same.

### 125. Closure translation takes the checked provision choices (M4c)

- **Rule.** `LinkBinding` records the source-to-target choices. The
  binding rule builds its linked runtime map and the list of captured
  needs from that one choice function. It still selects by the declared
  context and checks the same scope and provision conditions.
- **Executable.** `check_link_binding` returns the choice function.
  `translate_link_targets` reads it to emit the map using the existing
  provision lookup.
- **Proof.** The checker gives the declared target choices and the
  translator emits their audited map. The chosen keys are carried through
  closure typing and copy chains. The capture proof takes the checked
  subset of the source needs.
- **Class.** Equivalent factoring. The selected provisions and generated
  closure code stay the same.

### 126. Read a closure binding at its site (M4c)

- **Rule.** Shared inputs and linked provision choices are compared
  under the target's fixed readings and local atoms for the other
  classes. A selected provision must agree under this reading. When
  several candidates agree, the unique candidate already equal in the
  two declared merges supplies the provision. The capture check uses
  the complementary needs. Shared input readings are still restricted
  to the target's inputs. The body code is checked once per declared
  target and source pair.
- **Examples.** In [T before U](../tests/wasi/link_site_merge.moss) and
  [U before T](../tests/wasi/link_site_merge_flip.moss), the closure's
  merge holds because the target fixes both types to `Int`. Both cases
  print `1006`, using the `showT` supplied by the application.
  [Ambiguous methods](../tests/errors/link_site_ambiguous.moss) and
  [their reversal](../tests/errors/link_site_ambiguous_flip.moss) still
  reject. The two methods can denote separate provisions at application
  even when their receiver types coincide at the bind.
- **Proof.** The checker establishes each scoped choice and the
  allocation of the hypothetical classes. The closure proof transports
  input equations and provision readings to every application that
  keeps the fixed inputs. Entry and result preservation use those
  readings through direct, family, and copy-chain applications.
- **Class C** (criterion 2). The fixed-input merge programs now pass.

### 127. Read a shared input through a directly bound private input (M4c)

- **Rule.** A shared input's reading can mention a private input that is
  directly in force at the closure binding. The binding checks the
  shared reading against that captured meaning. The hypothetical fresh
  atoms avoid generations in the captured meanings as well as the fixed
  readings. Linked provision matching still uses the target's input scope.
- **Examples.** The target fixes `X` to `Int`, and the body takes a
  private `Y` also bound to `Int`. It returns the captured `seed : Y`
  where the target promises an `X`. [X = Y](../tests/wasi/link_fixed_captured.moss)
  and [Y = X](../tests/wasi/link_fixed_captured_flip.moss) both print `7`.
  The first ordering previously failed the shared-input check. A captured
  type that disagrees with the fixed type still fails.
- **Proof.** The closure reads the private row from its lexical capture.
  The shared row reads at the application. The body and result proofs
  use the combined reading and the body's preserved merge.
- **Class C** (criterion 2).

### 128. Rank generated code at its recorded origin

- **Example.** The reference accepts this program and emits a fresh
  function whose body is the unit expression.

  ```moss
  fn f();
  fn main() {
    bind f = () => ();
    f();
  }
  ```

  The old `NodeCx` admitted any lambda typing derivation that happened
  to emit the same unit expression at that number. Rocq checks that
  the unrelated merge reading `T ↦ {x: T}` gives that same body an
  impossible strict self edge.
- **Rule.** A source function with code uses its declared section. A
  generated lambda uses the section recorded by its translation, and a
  linked body uses its recorded provider's section. The lambda and
  linking typing rules require these annotations at their own slots.
- **Executable and proof.** Assembly attaches the selected readings
  and checks the expansion graph on the returned program. The rank
  proof uses the same selector. Successful elaboration establishes
  both type and code expansion bounds.
- **Class C.** The contract now ranks the contexts of the emitted
  program. The original lambda example runs with its empty section.

### 129. Check the platform provision types during executable linking

- The unsupported-function branch checks `HandlesRead`. A custom prelude
  declaring `unrecognized(x: Ref[StructRef = I32, Boxed = Boxed])` used to
  elaborate. Its `StructRef` argument violates the platform handle rule.
  An unsupported function with a valid signature still traps with its
  own name when called.
- Array copy requires one held type shared by the source and destination.
  A custom `Array.copy` whose source holds `Other` and whose receiver
  holds `Arrayed` used to elaborate. The executable now rejects it.
- Both checks implement existing clauses of the platform contract. The
  [custom prelude regressions](../crates/moss-tests/tests/suite/runtime_platform.rs)
  exercise the formerly accepted declarations.

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
| `~ In t (fr_pins F)` in the kept-output case (#13, 268bf3a) | `BApplyAbstract` | relaxation toward both implementations: [tests/wasi/block_kept_output_pin.moss](../tests/wasi/block_kept_output_pin.moss) is now accepted (oracle 7, native 7) | "Drop BApplyAbstract's pin clause" |
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
  [tests/wasi/block_pin_rebind.moss](../tests/wasi/block_pin_rebind.moss) is accepted.
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
    [tests/wasi/link_merge_rep_rebind_both.moss](../tests/wasi/link_merge_rep_rebind_both.moss)
    in both context orders; the oracle rejects the `[Foo=X], [Foo=T]`
    order (D84 3d, its pins include the representative) and exits 0 on
    the reversed one; Audit accepts both (by reading, as before).
  - *Principal θ:* **resolved by #50** (proposals item 5; criterion:
    principality). An unbound requirement reads through a bound member
    of its callee merge class, else as itself through the class's
    representative; tests/wasi/partially_bound_merge.moss stays accepted, and
    tests/errors/principal_theta_unbound.moss (a guessed θ T = Concrete) is rejected,
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
| E31 | Read linked result methods from the body's keys (no bind-time refile) | oracle | open | [tests/wasi/refile_stale_shadow.moss](../tests/wasi/refile_stale_shadow.moss): oracle stuck XErr 14; native 7 | #19 |
| E32 | Install exactly F's result at a copy's application | oracle | open | [tests/wasi/copy_extra_key.moss](../tests/wasi/copy_extra_key.moss): oracle 21; native 31 | #23 |
| E33 | No result key spelled through an output's input meaning | oracle (ExecutionModel and link_body) | open | [tests/wasi/output_merge_key.moss](../tests/wasi/output_merge_key.moss): oracle stuck XErr 6; native 7. Also functor_input_output_generations (E06) | #23 |
| E34 | A family's listed polymorphic inputs are types | **native** | open | [tests/errors/family_poly_nontype.moss](../tests/errors/family_poly_nontype.moss): oracle rejects; native exit 0 | #9 |
| E35 | A method's θ records every signature type symbol | oracle | **done** (in 61040f2) | [tests/wasi/sig_nonreq_shadow.moss](../tests/wasi/sig_nonreq_shadow.moss): now 42 on both | #12 |
| E36 | Oracle 3b checks every reading of a captured fn or method | oracle | open | [tests/errors/link_merge_captured_fn.moss](../tests/errors/link_merge_captured_fn.moss): oracle accepts, stuck XErr 14; native rejects | #7, #11 |
| E37 | Oracle 3d: a linked body may not install an input of F as a kept output | oracle | open | [tests/errors/link_kept_output_pin.moss](../tests/errors/link_kept_output_pin.moss): oracle stuck XErr 14; native rejects | #13 |
| E40 | Reject result items distinct in F that an application files under one key, and H's items distinct in H that a copy reads as one key of F | oracle and native (copies: oracle) | native **done** (9a62948, lang-all); oracle open | [tests/wasi/result_collide_ab.moss](../tests/wasi/result_collide_ab.moss): oracle a, native b (now rejects); [tests/errors/copy_result_collide.moss](../tests/errors/copy_result_collide.moss): oracle a, native rejects | #58 |
| E41 | Read an assume list in order for a method item's receiver: `assume T.m, T` needs T in force before T.m | native | **done** (4071c84, lang-all) | [tests/errors/assume_method_before_type.moss](../tests/errors/assume_method_before_type.moss): oracle rejects; native exit 0 (now rejects) | #59 |
| E42 | Rule B: a use (call, provider bind, family demand) needs the item's type requirements in force, through its merge class; a closure's captured inputs F does not take likewise; demands read needs through θ | native | **done** (2361b25, ca92046, lang-all) | [tests/errors/ruleb_call_unbound.moss](../tests/errors/ruleb_call_unbound.moss): oracle rejects; native exit 0 (now rejects); [tests/wasi/ruleb_merge_family_demand.moss](../tests/wasi/ruleb_merge_family_demand.moss): k on both | #60 |
| E43 | Merging two entries of one item unifies their instantiations (D43): `assume K1, K2` with `f[T=A]` and `f[T=B]` merges A with B, in either order | native | **done** (4934f50, lang-all) | [tests/errors/merge_inst_order_ab.moss](../tests/errors/merge_inst_order_ab.moss): oracle rejects; native exit 0 (now rejects). [tests/wasi/merge_inst_val.moss](../tests/wasi/merge_inst_val.moss): oracle accepts; native rejects (now accepts) | #61 |
| E70 | Reject a bind of a defined method (`bind Meter.describe = …` where `Meter.describe` has a body) | native | **done** (7d5bcd5, lang-all) | [tests/errors/bind_attached_defined.moss](../tests/errors/bind_attached_defined.moss): oracle rejects; native mm (now rejects) | #70 |
| E72 | Check every call's arguments in the caller's context Γ, never with the call's family demands in force: a defined fn call and a defined attached method call (a family-demanded call already does) | native | **done** (dff4b35, lang-all) | [tests/wasi/args_context_provision_wins.moss](../tests/wasi/args_context_provision_wins.moss): oracle mm; native xx (now mm). [tests/wasi/args_context_shadow_call.moss](../tests/wasi/args_context_shadow_call.moss): oracle 42; native rejects (incompatible types; now 42) | #72 |

Native bugs fixed on lang-all that had no E-number:

| Fix | Native bug | Example | Entry |
|---|---|---|---|
| 3f91e25, a55e060 | A selected provision's type was not compared with the callee's reading (the native half of E25) | [tests/errors/stale_provision_type.moss](../tests/errors/stale_provision_type.moss), `tests/wasi/stale_provision{,_fn}.moss`: native printed `t`, now rejects; the oracle still accepts and gets stuck (E25 open for the oracle) | #4 |
| f8c380c | An assumed item's needs were not checked at their instantiation | [tests/errors/assumed_need_incoherent.moss](../tests/errors/assumed_need_incoherent.moss): native accepted, now rejects, as the oracle does | #57 |
| 833d9a6 | A functor copy's captured vals were looked up under the original target's entry, so applying the copy in a frame without it failed ("no binding provides `seed`") | `tests/wasi/functor_copy_capture.moss`: native rejected, now 2454, as the oracle | D84 6 |

The earlier follow-ups E21–E30 ask the implementations to match Audit
changes. All are open, except E25's native half (above) and E24, done
for the oracle by #60:

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
- ~~tests/wasi/block_pin_rebind.moss~~: accepted since #49.
- ~~tests/errors/assumed_need_incoherent.moss (#57)~~: native rejects it since
  f8c380c (lang-all).
- ~~tests/errors/assume_method_before_type.moss (#59, E41)~~: native rejects it since
  4071c84 (lang-all).
- ~~result-collide-{ab,ba}{,-bindswap}.moss (#58, E40)~~: native rejects
  them since 9a62948 (lang-all); the oracle still accepts them.
- tests/errors/sameneed_ambiguous.moss and tests/errors/sameneed_ambiguous_other.moss (#11; tests of
  #51). Native prints 7 on both, and the oracle accepts the second.
- ~~tests/errors/bind_attached_defined.moss (#70, E70)~~: native rejects it since
  7d5bcd5 (lang-all).
- Native rejects stale-provision-type (#4) since 3f91e25 (lang-all).
  Its verdict is not recorded for unbound-merge-representative (#3).

**Audit accepts; native rejects.**
- ~~tests/wasi/args_context_shadow_call.moss (#72, E72)~~: native prints 42 since
  dff4b35 (lang-all).
- tests/wasi/sameneed_copy.moss (#51). The oracle rejects it too, but both accept the
  same program with the merge orders agreeing.
- tests/errors/functor_output_input_changed.moss (#14). Audit accepted it
  after 94d89ce, and still does after fc75de7 (consolidation reading).
- tests/wasi/const_kept_output.moss and tests/errors/const_family_unbound.moss (#20, #21). Native
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

On tests/wasi/args_context_provision_wins.moss (#72, E72), Audit, the oracle and
native print mm (native printed xx before dff4b35, lang-all).

On tests/wasi/sameneed_meaning.moss (#51), Audit prints 2, while native and the
oracle print 1. On tests/wasi/sameneed_meaning_agree.moss, Audit and the oracle print
2 and native prints 1.

**N4: a union with a type-symbol member ("latent identity"; n234 branch, no
Audit change).** The executable elaborator found that Audit has no reading
of `fn offered(): T | U` with T abstract (`TyUnion` asks nominal members),
so it rejects `tests/wasi/functor_latent_identity_copy.moss` and
`functor_latent_identity_reexport.moss`, which both implementations accept
(`scratch/elab-STATUS.md`, row N4).
- **The design.** [D15] (docs/design/semantics.md): union members "must be
  *nominal* types (units, tags) with distinct heads"; "an abstract type
  symbol may appear as a member *only if* the context the union is
  written in already constrains that symbol to a specific nominal type";
  a bare abstract member is illegal and gets a nominal wrapper instead.
  [D16] keeps it so: no post-monomorphization distinctness check. Audit's
  `TyUnion` is exactly this rule (a type symbol is read at its meaning
  where the union is written, and passes only if that is a nominal). The
  design log has no other entry on latent identity or disjointness.
- **The implementations.** The oracle enforces D15 wherever it reads a
  type for a body ("union member `T` is abstract and not constrained to a
  nominal type (D15)"), but not where it reads an abstract fn's signature
  for a functor's result or a copy, so it accepts the two latent tests.
  Native does not enforce D15 at all, and is unsound on it:
  [union-latent-overlap](../tests/errors/union_latent_overlap.moss) writes
  `T | A[X=Colour]`, binds T = A[X=I32] and passes a T holding 7; native
  matches it by head into the `A(y)` arm, reads the I32 as a Colour and
  exits 3. When T turns out to be U itself
  ([union-latent-identity](../tests/errors/union_latent_identity.moss)) native's
  head match is consistent (the value is a U) and exits 2. A scalar
  instantiation (T = I32) falls to the catch-all arm.
- **The narrowest safe rule.** Head matching goes wrong only when an
  instantiation of T shares a head with another member at another
  payload. A requirement-free nominal (`TNom n []`) is determined by its
  head, so one candidate is: a type-symbol member is allowed when every
  other member is a nominal without requirements (as U in both latent
  tests), and a match on such a union needs a catch-all arm for the
  type-symbol member. It is not small for the proof: `PublicArms` assumes
  `UnionFormed` (nominal members, distinct heads) of every scrutinee type,
  `tyof_subst` (PublicTypeRead) shows a union stays formed under
  substitution, which fails once T may become a member or a union, and
  `Arms`' exhaustiveness ignores headless members ("every member's head
  covered"), so a T value could fall through every arm. It would also
  reverse [D15], an owner decision.
- **Recommendation.** Keep `TyUnion` as it is (D15). Fix the
  implementations: the oracle should apply D15 in every type reading
  (functor results, copies, assumed signatures), and native should
  enforce D15 (its acceptance of union-latent-overlap is a miscompile).
  Rewrite the two latent tests with a nominal wrapper, which keeps what
  they test (an identity copy preserving an interface nothing inhabits):
  [union-latent-wrapped-copy](../tests/wasi/union_latent_wrapped_copy.moss) and
  [union-latent-wrapped-reexport](../tests/wasi/union_latent_wrapped_reexport.moss)
  are accepted by the oracle, native and Audit. Since #76 Audit rejects
  the latent tests at `offered`'s declaration rather than at the copy.
- **Native, done (impl-d15 branch).** Native enforces D15 where a union
  is written: at every declaration as written (`lower::check_unions`: tag
  payloads, alias bodies, val types, fn and method signatures, used or
  not), and in a body at the body's generic environment (`elab_ty`), so
  `bind T = U; let x: T | V` is accepted and a member whose meaning
  shares a head with another is refused. union-latent-overlap and
  union-latent-identity are rejected; the two latent tests are rewritten
  with a wrapper (tests/errors/union_latent_overlap.moss,
  union_abstract_member_let.moss, union_bound_member_clash.moss,
  tests/wasi/union_bound_member.moss).
- **Oracle, done (impl-d15 branch).** `check_assumes` (Elab.v) first runs
  `unions_written`: at every declaration as written (tag payload, alias
  body, val type, fn/method signature), a union member that names a type
  symbol directly is refused, with elab_type's D15 message. So a
  signature read only for a functor's result or a copy is checked too.
  The originals are now tests/errors/union_latent_copy.moss and
  union_latent_reexport.moss, rejected by both. A type symbol reached
  through an alias as written (`type A = T; ... A | U`) is not caught
  there; elab_type still catches it in a body.

**Audit rejects; both implementations accept (criteria section 7, #74 to
#77).** decl-ctx-unresolved, decl-ctx-bracket (#74); decl-nest-type,
decl-nest-tag, decl-nest-incomplete (#75); decl-tag-unresolved,
decl-tag-union-head, decl-alias-cycle, decl-alias-unresolved,
decl-val-type, decl-fn-sig, decl-meth-sig, decl-const-bad,
decl-const-mistyped, decl-const-in-assume, decl-functor-arg,
decl-functor-res (#76); functor-partial-unused (#77). Each is an unused
declaration that does not form. Both implementations check a declaration
where it is used (the oracle also checks context cycles, D73's listed
parameters and every defined fn at declaration; native expands every
context). By section 7 they are the ones to change.
- **Native, done (impl-d15 branch).** Native now rejects all 18:
  `lower::check_declarations` elaborates each tag payload, alias body,
  val type and fn or method signature as written (and finds alias cycles
  first, `e_alias_cycle`). It forms each context's items with every type
  symbol in force as itself, and each functor's arguments, and resolves
  its result items. `check_assumes` forms every symbol's nesting, whatever
  its kind. `check_functor_total` runs for every functor with a body.
  `check_const` emits each defined val's initializer against its type in
  an empty frame (`e_const_assumes` for one inside an assume). The
  controls are still accepted. tests/errors/decl_*.moss and
  functor_partial_unused.moss are the rejected ones (15 of the 18:
  decl-nest-tag and decl-alias-unresolved repeat decl-nest-type and
  decl-tag-unresolved, and decl-tag-union-head the union_shared_head
  tests). The controls are in tests/wasi/decl_*_control.moss.
  The oracle is unchanged (gaps listed in agreement.rs and errors.rs),
  except for D15 unions (N4).

**Stale documentation.**
- The header of tests/errors/kept_output_escape.moss says MadeFor (dropped).
- The header of tests/wasi/output_merge_key.moss describes `fc_roles` (superseded by
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
