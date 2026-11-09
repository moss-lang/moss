# hunt-chain report: CN.ChainKeysOk, CN.ChainRealizeOk (safety 73234e7)

## ChainRealizeOk: FALSE (survives the caller-keyed change)

Plain claim: after `bind F;` runs F's closure (copies down to a linked
root body), every provision the caller's C' adds is in the result
environment, realized at the caller's type for it.

Counterexample: `captured-rep-result.moss` (header has the full Audit
argument). G's context merges X, which F takes (linked), into Y, which
only G has (captured). The merge is oriented X := Y, an orientation the
world may choose (and the oracle's own for this order). F's result is
`val out: X`. G's body is `bind out = vy;` with the captured `vy: Y`.
- 3b passes. Audit reads the captured `A.gimme` under sigma, so its
  provision mentions Y and A. Neither is an input of F.
- LinkBody's input clause (G's readings of F's inputs hold in Cu) passes
  vacuously, because Y is not in Cu.
- ResultRealizes passes, because `c_sub Cb` sends X to Y on both sides.
- At the application, fc_table makes X read as the captured Y (Bool), so
  the selection installs `out = true`. The caller's C' types `out` at
  its own X (Char), so EnvOkOn fails. `c.code()` then applies Char's
  primitive to a Bool.
- Oracle: rejects (D84 3b, which it checks before the merge). Native:
  rejects (incompatible types). Audit: accepts.
- The key is `IVal out`, so this is a type failure, not a key failure,
  and caller-keyed install does not touch it. A method variant (`X.m`
  in F's result) installs G's Y.m closure, which takes Bool, at the
  caller's `Char.m`. That fails in the same way.
- Repair options:
  - (a) check 3b on the unmerged entries, as the oracle does;
  - (b) LinkBody: every linked input must read in Cg as a type over F's
    inputs. That means dropping the premise
    `forall y in gens tx, nlook y (c_ty Cu) <> None`, or requiring it.
    In both cases the canon-in-Cu equation must then hold.

Shapes checked and argued safe (no program built):
- An output that is also an input. A body rebind writes only aliases, so
  ResultRealizes compares at Cb's final reading of T, and
  read_result_types reads that alias. A `bind F2;` inside the body that
  outputs T gives a fresh generation and an alias. Both agree with
  read_rename.
- An output not in G's inputs. LinkBody requires the body to bind it.
- A method on an output that is bound before T is rebound. at_site Cb
  then names a different key. ResultRealizes fails, or it finds a
  captured provision with the promised signature, which is safe even
  though it differs from native.
- Duplicate keys and last setters. ilook and rkey (first in c_apps) and
  output_cx's `rev es` agree. In the caller-keyed design,
  `select_provisions` over `combine` makes the first position win, while
  the static type is the last setter's. That is harmless only while
  duplicate positions share one body source. That holds for linked
  roots, and platform families have NoDup ids. Keep it in mind when
  writing chain_sources.
- Copies whose F merges more than H, and the E33 shape on the copy
  side: both sides are read at Cu or at θ, and they agree.
- Kept outputs: `context_gens` includes `c_sub`'s domain, so a kept T
  cannot be re-read by the caller's canon.

## ChainKeysOk: FALSE today, true by construction after the change

The current failures are the known copy-with-bracket shape: keys equal
only in ground meaning (linked-copy-bracket.moss and
ChainForwardCopyKeys). For a linked root, I did not build a separate
counterexample. Doing that needs a bracket target that is concrete in H
but is no input of H. I found no other key failure under the roles fix.
captured-rep-result.moss does not break ChainKeysOk: its X.m variant
keys agree, because Θ(X) is the caller's X, and the error is the value.
