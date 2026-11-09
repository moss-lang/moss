# cap3b: Audit changes

## 1. A linked input reads in G as a type over F's inputs (LinkBody)

**What.** `Audit.v`, `LinkBody` (constructor `LB`), the input condition. It was

    (forall x tx, nlook x (c_ty Cg) = Some tx -> nlook x (c_ty Cu) <> None ->
       (forall y, In y (gens tx) -> nlook y (c_ty Cu) <> None) ->
       canon Cu (TAbs x) = canon Cu tx)

and is now

    (forall x tx, nlook x (c_ty Cg) = Some tx -> nlook x (c_ty Cu) <> None ->
       (forall y, In y (gens tx) -> nlook y (c_ty Cu) <> None) /\
       canon Cu (TAbs x) = canon Cu tx)

The middle premise has become a conclusion. If x is an input of F that G also has,
G's merged reading of x must mention only F's inputs, and F must agree with it.
That is option (b) of scratch/hunt/hunt-chain/REPORT.md. The old premise made
the condition vacuous whenever G's merge sent x to a type only G has.

**Why.** `CN.ChainRealizeOk` is false
(scratch/hunt/hunt-chain/captured-rep-result.moss, now
bootstrap/examples/link-merge-captured.moss):
- G's context reads the captured `A.gimme` at X, then at Y, so X and Y merge.
- With the orientation X := Y (the oracle's for this order), three checks pass:
  - Audit's 3b reads the captured provision under σ, so it sees only Y and A,
    and neither is an input of F.
  - The input condition holds vacuously, since Y is not in Cu.
  - ResultRealizes compares `out: X` with `out: Y` through `c_sub Cb`, which
    sends X to Y.
- At an application, X is the caller's (Char), but G's body reads X as the
  captured Y (Bool). So `out = true` is installed where C' types `out` at Char.
- With this change, X reads Y and Y is not one of F's inputs, so LinkBody fails.

In the other orientation, Y := X, the existing σ-read 3b already rejects:
the captured `A.gimme` and `vy` then mention X. The two together are
orientation-independent. A linked input can no longer meet a captured-only type
through a merge. The one remaining kind of merge, Y := X with nothing captured
mentioning Y, makes Y an alias of X and is harmless.

**Option (a) not taken (3b on the unmerged entries).** The oracle's D84 3b does
not check every unmerged reading:
- For a fn or method it reads the one stored signature (`key_sig eg k`, the
  first reading, not canonicalized).
- For a val or functor it reads the canonical type.

So "mirroring the oracle exactly" would not be sound. See
bootstrap/examples/link-merge-captured-fn.moss: G reads the captured `gimme` at
Y, then at X.
- The oracle's 3b sees only Y. The oracle accepts the program, and the
  reference interpreter is stuck at run time (`moss_ref: stuck (XErr 14)`).
- Native rejects it ("incompatible types").
- Audit rejects it under either orientation: X := Y by this change, Y := X by
  the σ-read 3b.

A full "every unmerged entry" 3b would reject the same merge shapes as (b),
since the merging key's raw reading names x. It would also need entry-level
reasoning in the proofs. (b) is one conjunct, and it is the fact that
ChainRealizeOk needs (cap3b-STATUS.md).

**Tests.**
- bootstrap/examples/link-merge-captured.moss (the counterexample):
  - oracle (moss-ob-family4 moss_m2): rejects, "`G`, bound to `F`, captures
    `A.gimme`, which mentions a type each application of `F` supplies (D84 3b)";
  - native (seed 1h1710x0): rejects, "incompatible types" at `bind out = vy;`;
  - Audit: now rejects.
- bootstrap/examples/link-merge-captured-fn.moss (see above):
  - oracle: accepts, then stuck;
  - native: rejects;
  - Audit: rejects.
- Neighbours in scratch/cap3b/ (not committed):
  - n1/n3: the merging item is linked. The oracle rejects ("linked input
    interpretation differs" or D84 3b) and native rejects. Audit rejects under
    either orientation: the linked-provision equation for X := Y, the σ-read 3b
    on `vy` for Y := X.
  - n2: the reverse order. Oracle D84 3b on `vy`, native rejects.
  - n7/n7b: G merges X with Y, but F's result does not mention X.
    - n7 (order Y, X): both implementations accept it and print 7. Audit
      already rejected it before this change in the oracle's orientation
      Y := X (σ-read 3b on `gimme`). This change adds no rejection there, and
      the variant below accepts n7.
    - n7b (order X, Y): the oracle rejects (D84 3b) and native accepts. The
      change rejects it, as the oracle does.

**Evidence that it rejects nothing else.** An oracle variant adds the check to
Elab.v's `link_body`, just after `fun_env fg`/`fun_env bg`:
- each `x` in `en_tn eg` with `tn_get ef x` set must have
  `ety_gens (canon eg _)` within `ef`'s type names;
- `fun_env` names generations by their symbols, as `FunCtx` does.

It is built in scratch/cap3b/variant (a copy of bootstrap/ with the patch, not
committed). The script is scratch/orch/cap3b-cmp.sh, and the output is
scratch/orch/cap3b-cmp.out. It covers all of tests/, examples/,
bootstrap/examples/ and scratch/hunt/, 400 files. There are exactly 2 DIFFs:
link-merge-captured.moss and the hunt original captured-rep-result.moss. The
oracle rejects both (D84 3b), and the variant rejects them with the new
message. So on the corpus, the variant accepts exactly what the oracle accepts.

**Proofs.** `PublicBodySlice.link_facts` projects the old form (`proj2`) into
`LinkData`, which is unchanged. No other proof destructures the condition.
`case_BFunClosure`, `ClosureOk`/`CLink` and `PublicLinkMerges.captured_mentioned`
use only 3b and `LinkData`, and are unaffected. check.sh passes.
