# family2: Audit changes

## 1. Lambda capture keeps method-key receivers' meanings (option (c))

**What.** `Audit.v`: new `key_receiver_gens C` (the generations of every
`IMth r _ _` key's receiver among C's provisions). `lexical_capture C` now
maps `context_gens C ++ key_receiver_gens C` to themselves, not just
`context_gens C`. `context_gens` itself, and so `FreshFor`, is unchanged.

**Why.**
- Under option (b), method keys may mention a declared type symbol T
  outside the context's scope. This happens when a callee requirement
  the caller leaves unbound is read at the site as itself (`theta_at`,
  `at_site`) and a family demand keys `T.foo` (family-unbound-need.moss).
- If nothing else in the context mentions T, then T is not in
  `context_gens`. A lambda over the demand context (an argument of the
  call) then dropped T's meaning but kept the key `T.foo`. Its closure's
  provision was callable at `ground d1 T`, but the lambda's context
  expects it at `ground de T = T`.
- In a function body T has no meaning there, so the two agree; proving
  that needs a run-time "no meaning" invariant.
- A defined val's initializer, though, is elaborated in the empty context
  and inlined in the caller's environment, where T can mean something
  (const-family-unbound.moss). So that invariant is false there
  (family2-STATUS.md).
- Capturing receivers' meanings makes the captured grounding agree by
  construction.

**Tests.** Neither implementation rejects any of these programs, and
Audit rejects nothing new (the change is run-time only). The oracle
(moss_m2) and native (MOSS_SEED=... moss run) results:

| example | oracle | native |
|---|---|---|
| family-unbound-need.moss | 0 | 0 |
| family-unbound-need-sig.moss | 0 | 0 |
| lambda-declared-output.moss | 0 | 0 |
| const-family-unbound.moss | 0 | 1* |

\* Native rejects every use of a defined val ("no binding provides this
item"), including `val K: () = ();`, so it gives no evidence about const
code.

**Evidence that it mirrors the oracle.** The oracle's closures capture
the defining environment's whole type environment
(`ExecutionModel.xclose d dc = dc ++ xtype_env d`, used by
`XBindFn`/`XBindMth`). Its method keys are symbolic (`XBindMth tauk ...`).
So the oracle never drops a receiver's meaning, and a captured key cannot
miss.

**Alternatives considered.**
- (a) Audit rejects demands at unbound declared symbols. Rejected: both
  implementations accept these programs.
- (b) alone, with a run-time `Inert` invariant in `Realizes`. Rejected:
  false for inlined consts.
- ExConst clearing meanings. Rejected: it needs a new expression form,
  which is invasive (about 19 files and the oracle).
- Capturing all of `type_meanings d`. A larger change than needed.
- Adding receivers to `context_gens`. This would also make `FreshFor`
  stricter.
