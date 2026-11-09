# hunt2-family report

Oracle: ~/github/moss-lang/moss-ob-sigclose/scratch/sigclose/var/moss_m2.exe (run.sh). All results re-run on it.

## FS.FamilyCopyRealizeOk: FALSE
Counterexample: v3-copy-fixed-reads-poly.moss. It is the family instance of hunt2-chain's
t3-copy-bracket-nonfixed hole. BFunCopy's CopyOutputs sigma is F's fixed readings at the copy site.
A fixed input Y read as `TAbs A`, where A is F's poly symbol, makes F's `A.m[U = A]` and
H's `A.m[U = Y]` equal after sigma. At a demand of Concrete.m, the caller types the method
as () -> Concrete, but H's body returns Y's value (Bool). The oracle rejects the copy (D78, fresh
@copy markers for non-fixed generations). Native does not compile the demand. Audit accepts.
Fix: read F's non-fixed inputs in CopyOutputs as fresh symbols (as the oracle does), or require
the fixed readings in sigma to mention no input of F.
Round-1 t11: Audit installs under caller keys now, so it is safe. The oracle is still stuck (XErr 6)
because it lacks callerkeys2.

## CC.FamilyLinkNewOk: LIKELY TRUE
LinkBody checks G's result in G's own terms (Cb), with no copy-site substitution. Linked inputs
read ground d (th x) on both sides, captured types are not F's inputs (3b), and cap3b stops linked
inputs merging with captured ones. So the BodyReal transfer reduces to th. Tried: v2 (fixed Y read as
the outer A, which shares the poly symbol's name: consistent, oracle/native 0), v4 (the same trick via
capture: rejected by ResultRealizes), u3 (poly bracket). Duplicate caller keys are rejected by
FamilyApplication's NoDup. Unbound declared receivers ground to TAbs T on both sides.
