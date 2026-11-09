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

## 2. An output not in force counts as made by `bind F;` (block escape)

**What.** `BApplyAbstract`'s `made` is now `newgens ++ unforced C outs`
(`unforced C ts`: the symbols of ts not in force in C, a new Definition
before `Ex`). It also takes a premise that the frame's types (`fr_ret`,
`fr_this`) do not mention those outputs, the analogue of `FreshFor`'s frame
clause for `newgens`.

**Why.** Task C needs `DeclaredInForce` (every declared symbol a context
mentions is in force) as an invariant of every context the slice reaches.
It was false: an output T of F not in force where `bind F;` runs stands for
itself (`gensof T = T`), so T was not among the generations the bind made,
and BlkE's escape check let a value of type T leave the block:
`let y = { bind F; make() };` left `y : T` in a context where T is not in
force. examples/block-output-escape.moss is this program. The oracle
rejects it ("this block yields `T`, which mentions `T`, a type `bind F;`
made in it ... (D78)"), and so does native ("a type created by a binding
cannot escape its block: `T`"); so does the statement variant
`{ bind F; make() };` in both. The oracle's own `made` (the checker's
`cx_fresh` growth) already includes such T, so Audit now mirrors it, and
BodySound (whose bind statements are pending) is unaffected.

The frame premise rejects nothing: the frame's types are read where the
body starts, so they mention only types in force there, and a body never
removes a key from its type table, so an output not in force at the bind
was not in force at the start either. It is what the slice needs to keep
`FrameAvoids made F` (a bind may change the meaning of what it made, and
the frame's return and receiver types must not move).

**Alternatives considered.**
- A BlkE premise "the block's type mentions only declared symbols in
  force around it": same verdicts, but BodySound constructs `BlkECheck`
  from the elaborator's escape check, which is phrased over made types,
  so it would need a new elaborator fact.
- Always making a fresh generation for an output (even when not in
  force): changes the meaning of programs both implementations accept
  (examples/captured-output-scope.moss reads T itself afterwards).

**Side note for the results branch.** `LinkResultsOk` concludes
`MeaningsGrow newgens d (append_environment result d)`. The result's
writes are keyed by `gensof t` for every output, so for an output not in
force (`gensof t = t`, not in newgens) that conclusion looks false; with
this change the slice only needs `MeaningsGrow (newgens ++ unforced C outs)`
(`stepout_more`), so the lead may want to weaken that obligation
accordingly.

**Evidence.** check.sh passes; oracle and native outputs above.
