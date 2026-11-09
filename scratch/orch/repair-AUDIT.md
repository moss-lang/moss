# repair: Audit.v changes

## 1. An application reads G's inputs through G's table (`fc_table`)

**What.** `functor_code` gains a field `fc_table : list (tsym * ty)`
(between `fc_linked` and `fc_outputs`). `application_environment` keeps
the old per-input meaning as `base x` (θ read in d for a linked input, the
capture's meaning otherwise, `TAbs x` if neither) and gives input x the
meaning

    match lookup_subst x table with
    | Some t => app_subst bases t      (bases = map (x, base x) inputs)
    | None   => base x
    end

`LinkBody` records `fc_table := c_ty Cg`, G's canonical table
(`c_ty Cg x = canon Cg (TAbs x)`). `Platform.Family` records `[]`, so a
platform family's environment is the old one by computation. The example
codes (AuditExamples, PublicClosureExample, PublicFunctorScopeExample,
PublicGenerationCallExample, PublicOutputTransferExample) use `[]` and
behave as before; their `vm_compute`/`reflexivity` checks still pass.

**Why.** `LinkMergesOk` and `BindMergesOk` are false on programs that Audit,
the oracle and native all accept and run (examples/linked-merge-captured.moss,
examples/bind-merge-unbound.moss, both print 7; see
scratch/orch/merges-STATUS.md). In both, the old environment read a
merged-away input Y from the bind site rather than as its representative,
so the body's environment did not absorb G's merge (`Absorbs Cg`), which
`Realizes Cg` at the body's entry needs. With the table, a merged-away x
reads as `app_subst bases (canon Cg x)`, and representatives read as
before. The merge then holds by construction:

    ground de t = app_subst bases (canon Cg t)          (GD, in link_entry)
    ground de (canon Cg t) = app_subst bases (canon Cg (canon Cg t))
                           = app_subst bases (canon Cg t) = ground de t

**Unobservable by elaborated code.** G's body is elaborated in G's context:
every provision of Cg is canonical (`functx_canon_prov`: entries_cx applies
σ, and σ is idempotent), and every type the body's code reads comes from
that table. Canonical types mention only representatives, whose meanings
are unchanged (`envok_transfer` moves the old EnvOk proof from the
base environment to the new one exactly on canonical types). Only an input
that G merged away reads differently, and nothing in the body mentions it.
The oracle and native are not affected (the oracle's interpreter is
ExecutionModel/MossX, not Audit's `application_environment`), and nothing
is rejected: this changes no premise.

**Alternatives considered.**
- A premise rejecting such merges: wrong, since the programs are safe and
  both implementations accept them (the previous agent checked this).
- Recording only G's merge (`c_sub Cg`) instead of its table: equivalent
  (the table is `canon Cg (TAbs x)`), but the table already exists and has
  every input as a key.
- Giving families `c_ty inputs`: semantically the same (θ already absorbs
  F's merges), but `[]` keeps the family code's environment identical by
  computation, so the platform/family obligations are unaffected.

**Evidence.** `cd bootstrap && ./check.sh` passes. The two counterexamples
are now covered by the proof: `link_entry` proves `Realizes Cg` for the
body's environment without any merge hypothesis.

**Interface change for other branches.** `FunctorCode` takes one more
argument (3rd position): add `[]` to any literal construction to keep its
behaviour. A proof that unfolds `application_environment` on a code whose
table is `[]` gets the old term after `cbn`/`cbv beta iota` (the `match
lookup_subst x [] with ... end` reduces to `base x`). On a `LinkBody` code
(`LinkData` now has `(A.c_ty Cg)` as the table), use
`PublicBodySlice.link_entry`, which no longer needs a merge clause.

## 2. Block escape of an output not in force (superseded by `safety`)

While checking Task C's `DeclaredInForce`, I found that it was false:
`let y = { bind F; make() };` with F's output T not in force left
`y : T` where T is not in force, because `BApplyAbstract` counted only
fresh generations as made. I fixed it in commit b2c4a90 (made := newgens
plus the outputs not in force, with a frame premise). The `safety` branch
(family agent, examples/output-escape.moss) had meanwhile made the same
repair (made := `map gensof outs`, the frame premise inside the outputs
clause), so the merge takes `safety`'s version and drops mine.

Extra evidence for that repair: native also rejects the program ("a type
created by a binding cannot escape its block: `T`"), as does the variant
`{ bind F; make() };` as a statement (oracle and native both). I ran
native as moss-safety's `target/release/moss run` from this worktree,
with moss-safety's `target/moss.wasm` copied into this worktree's
`target/` (ignored).
