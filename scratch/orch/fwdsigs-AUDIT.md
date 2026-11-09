# fwdsigs: Audit changes

## 1. BFunCopy: the copy's target is abstract

**What.** `Audit.BFunCopy` gains the premise
`(exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None))`, as
`BFunClosure` already has. `S.CopySite` carries it (last conjunct), so each
`CCStep` of a copy chain knows its copy is abstract.

**Why.** `CF.ChainForwardCopySigs` walks the chain: at each step `u <- h` it
relates h's result read at u's context Cu with h's result read at its own
context Ch, entry by entry (`PublicFamilyAgree.entries_rel`). That relation
asks that every symbol h's result mentions is a key of Ch's table
(`mentions_dom`), which holds only for an abstract functor (its implicit
inputs, FunCtx). Without the premise Audit admits copy targets with bodies,
whose mentions need not be inputs.

**Evidence.** bootstrap/examples/copy-into-body.moss (`bind F = H;` with F
having a body):
- oracle (ob-sigclose var moss_m2.exe): exit 64, "`F` has a body, so it is
  applied (`bind F;`), not bound; only an abstract functor is bound (D78)";
- native (ob-family moss, seed 1h1710x0): exit 1, "this construct is not
  supported by the compiler: `F`" at `bind F = H;`.
Control scratch/fwdsigs/copy-into-body-ok.moss (F abstract): both print 7.

**Rejects nothing the implementations accept:** the oracle's
`bind_functor` (Elab.v) errors on every target with a body before looking
at the provider, so the premise is exactly that check. No corpus run is
needed for that reason. `FunctorCopyExec` (oracle-to-Audit correspondence)
discharges the new premise from `gs_decl fg = ADFunctor ... None`.

**Alternatives.** Prove from WorldOf that a functor with a body mentions
only its inputs (a walk over its declaration; not available, and it would
not cover the abstract case's implicit inputs any more simply).
