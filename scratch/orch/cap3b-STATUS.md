# cap3b: status

Branch ob-cap3b, from safety 73234e7 and merged up to safety 5a4ea54.

## Done

1. **Audit change (commit 807bbbf).** `LinkBody`'s input condition now
   concludes, rather than assumes, that each input x of F that G also has
   reads in G (`c_ty Cg`) as a type over F's inputs. The canonical equation
   `canon Cu (TAbs x) = canon Cu tx` stays.
   - This is option (b) of the hunt report.
   - Option (a), mirroring the oracle's 3b exactly, was not taken. The
     oracle's 3b reads one stored signature per captured fn or method, so
     it is unsound:
     bootstrap/examples/link-merge-captured-fn.moss is accepted by the
     oracle, and the reference interpreter then gets stuck.
   - Tests:
     - bootstrap/examples/link-merge-captured.moss is the hunt
       counterexample. The oracle rejects it (D84 3b), native rejects it
       ("incompatible types"), and Audit now rejects it.
     - bootstrap/examples/link-merge-captured-fn.moss: the oracle accepts
       it and gets stuck; native rejects it, and so does Audit.
   - Corpus: an oracle variant with the check was compared with the oracle
     over 400 files (tests, examples, bootstrap/examples, scratch/hunt). It
     differs only on the two counterexample copies, which the oracle already
     rejects.
   - Details are in scratch/orch/cap3b-AUDIT.md, scratch/orch/cap3b-cmp.sh
     and scratch/orch/cap3b-cmp.out, and in a SAFETY-SITES Observation.
2. **Proofs.**
   - `PublicBodySlice.link_facts` projects the old form into `LinkData`,
     which is unchanged.
   - Nothing else needed repair. `case_BFunClosure`, `ClosureOk`/`CLink`,
     `link_3b` and `PublicLinkMerges.captured_mentioned` use only the 3b
     clause and `LinkData`.
   - `check.sh` passes. No new premises, no new obligations, and no
     statement of any `...Ok` changed.

## Why ChainRealizeOk should now hold for this shape (not proved)

**How an application reads G's types.** Audit's `application_environment`
gives each input s of G the meaning `app_subst bases (fc_table s)`.
- `fc_table` is `c_ty Cg`, G's merged readings.
- A linked generation (a G type that is also in Cu) has the base
  `ground d (θ s)`, the caller's reading.
- Every other generation has the captured meaning from the bind site.

**The counterexample.** `fc_table X = Y`, and Y is captured. So G's body
read the caller's X as the bind-time Y (Bool), while C' types `out` at the
caller's X (Char). ResultRealizes did not notice, because `c_sub Cb` sends
the required `out: X` to `Y` as well.

**After the change**, for a linked x with `c_ty Cg x = tx`:
- **(i)** Every generation of tx is in Cu. It is therefore linked, so x's
  meaning at an application is `ground d (θ tx)`, which involves no
  capture.
- **(ii)** `canon Cu x = canon Cu tx`. θ satisfies F's merges at the
  caller: AppPremises' `Satisfies C Cu θ`, and its merges clause holds
  for Cu's σ. So `ground d (θ tx) = ground d (θ x)`, which is the caller's
  meaning of x. That is what C' uses for every type of F's result that
  mentions x, since `output_cx` reads the result at θ and at the output
  roles.
- **(iii)** ResultRealizes compares, under `c_sub Cb` (the same σ as
  `c_sub Cg`; body rebinds only write aliases), the required provision over
  Cu's generations and the output roles with the body's actual one. By (i),
  σ sends Cu's generations to types over Cu's generations. So the
  canonical required type mentions only linked generations and roles, and
  the equal actual type does too. The value the body installed is typed
  (`Pre ... Cb d3` after `eval_bindings`) at the application meaning of
  that type. By (ii), that meaning is the caller's ground type for C''s
  provision. That is EnvOkOn for each new key.
- **(iv)** No captured type stands in for a linked one in either
  orientation.
  - Captured Y := linked x (x reads Y) is now refused by (i).
  - Linked x := captured Y (Y reads x): the σ-read 3b refuses every
    captured item whose σ-read type mentions x, because x is in Cu and
    not Fixed. So no captured value, fn or method is typed at a merged
    Y. Y itself is then only an alias of x, and the table reads it through
    x's linked base.
  - Fixed inputs X(F) are linked too, and θ reads them at the gfx fixed at
    the bind (FixedMeans), as before.

**Method variant** (F's result `X.m`, G binds `Y.m`):
- With X := Y, (i) refuses it, exactly as for the val.
- With Y := X, G's `bind Y.m = ...` is keyed at `at_site Cb` (receiver X)
  and so binds X.m. The closure it installs is created during the
  application's run of G's body. Its parameter and receiver types are
  the application meanings, so X means the caller's X by (i) and (ii).
  Anything it uses from the capture is refused by 3b if its type mentions
  X. The result selection files it under the caller's runtime key for X.m
  (ChainKeysOk: Θ(X) is the caller's X). Its CallableOk is at the caller's
  `X.m` signature, by (iii) applied to signatures.

Other neighbours:
- Merges between two linked inputs that F does not make: (ii) forces F to
  make them too.
- A linked input read as a structured type over F's inputs (for example
  `X := List[Z]`): (i) and (ii) cover it.
- Copies along the chain: CCopy already requires
  `canon Cu (TAbs x) = canon Cu tx` for H's readings, and that H's inputs
  are among F's. The linked root's facts transfer through θ unchanged.

**Proof note for whoever proves it.** ChainRealizeOk receives `S.LinkData`,
which keeps the old, premise-form condition, so the new conclusion is not
in its premises directly. It is reachable from `Ok0`:
- `ClosureOk ... h0 gf0 (FunctorClosure code ...)` inverts to `CLink`,
  which gives `A.LinkBody W Prog h0 gg' code`.
- `funbody_of Prog code` determines `c_ty Cg` (FunctorCode's third
  field), and `FunCtx` uniqueness (`functx_unique`) determines Cu.
- So the condition transfers to LinkData's Cg without changing any
  statement.
- Alternatively, LinkData could carry the strengthened conjunct. That is
  a Definition, not an obligation, but it would weaken ChainRealizeOk's
  meaning, so it was not done.

## Remaining

- `ChainRealizeOk` itself (to be hunted again first, per the task).
- The variant oracle is in scratch/cap3b/variant and is not committed.
  The neighbour programs n1–n7b are in scratch/cap3b/, also not
  committed; cap3b-AUDIT.md lists their verdicts.

## Time

About 25 minutes: 13:20 to 13:47 UTC, including a 4-minute check.sh and a
6-minute corpus run.
