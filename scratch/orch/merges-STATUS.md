# merges: LinkMergesOk, BindMergesOk, CapturedInForceOk

Branch `ob-merges`. No Audit.v change. No obligation discharged in
PublicEntry's `safety_obligations`: two of the three are false for
programs both implementations accept, and the third is false as stated.
The third does hold at a bind site under one invariant that `Pre` does not
carry yet.

## LinkMergesOk: false

Counterexample: `bootstrap/examples/linked-merge-captured.moss`. G's
context merges Y, which G captures, into X, which F links. G reads
`A.gimme[Foo=Y], A.gimme[Foo=X]`, so the merge is Y := X. The oracle
orients a merge from the earlier reading to the later one. Audit's
MostGeneralMerge allows either direction.

- LinkBody holds: the linked `A.gimme` reads as F's.
- 3b does not apply: no captured item mentions X.
- The merge {Y, X} has X among F's inputs, and X is not fixed. Y is not
  among F's inputs. So both disjuncts of LinkMergesOk fail.
- Oracle (`moss_m2.exe`): prints 7. Native (moss-safety
  `target/release/moss run`, with moss-safety's lib): prints 7.
- With the readings in the other order (merge X := Y), the oracle rejects
  the program ("linked input interpretation differs") and native accepts
  it. See `scratch/merges/lm-xy.moss` (not committed; scratch is ignored).

## BindMergesOk: false

Counterexample: `bootstrap/examples/bind-merge-unbound.moss`. G merges Y
into Z. F takes neither. Y is not in force where G is bound; Z is.

- Satisfies' merge clause binds only merges whose symbols are all in
  force, so it says nothing about this one.
- The premise of BindMergesOk holds vacuously, because F has no type
  inputs. Its conclusion, that Y is in force, fails.
- Oracle and native both print 7. With the readings reversed, the oracle
  rejects ("another frozen interpretation") and native accepts.

## Why both fail, and a proposed repair

In both programs an application's environment reads the merged-away Y
from the bind site (`application_environment`: captured inputs come from
`dc`). It does not read Y as Y's representative. So the body's
environment does not absorb G's merge, and `Realizes Cg` fails in
`link_entry`. The programs are still safe, because elaborated code reads
types through G's table, so nothing in the body reads Y directly.

A premise that rejected these programs would also reject programs both
implementations accept, so no Audit premise was added.

Proposed repair (in the semantics, not as a premise): FunctorCode records
G's table (or G's merge), and `application_environment` reads each input
x as `ground base (c_ty Cg x)`, where `base` gives the representatives'
meanings (θ for linked inputs, the capture otherwise). Then G's merges
hold in the body's environment by construction, which follows from
idempotence. `link_entry`'s merge clause becomes provable, and
`link_merges` and `bind_merges` can be dropped. This needs a decision: it
changes the interpreter and ripples into MeanL/MeanC, copies (CopyApplyOk)
and the elaboration relation's FunctorCode.

## CapturedInForceOk: false as stated; proved under one invariant

File: `bootstrap/theories/PublicLinkMerges.v`, registered in
`assumptions/Certificate.v`.

- `LinkMerges.captured_in_force_unprovable : ~ S.CapturedInForceOk`. The
  statement has no premise tying C to a program. A context that provides
  the captured `v : x` without x in force is a counterexample.
- `captured_in_force_of` proves the conclusion from these premises:
  - `MentionsTyped W` (a slice hypothesis);
  - `FunCtx W gg Cg nsg` (available at BFunClosure);
  - `KeysScoped C` (in `Pre`, through `Formed`);
  - `DeclaredInForce C`: every symbol in `context_gens C` that has a
    declaration is in force in C. Generations are exempt.

  The proof works because a key of G's context is a declared type, by the
  new lemma `functx_keys_typed`. If it is not in force at C, then θ reads
  it as itself. Through Satisfies' types, coherence and provision clauses,
  C's provision would then mention it, which contradicts
  `DeclaredInForce`.
- What remains: `DeclaredInForce` is a T29 invariant of every context the
  slice reaches. It holds of telescopes (CtxOf/FunCtx), and binds,
  applications, lambdas and families plausibly keep it. Adding it to
  `Pre` (or to `FormedCx` and `binditem_ok`) would let `case_BFunClosure`
  use `captured_in_force_of` and drop `captured_in_force`. That was not
  done here because it means extending formation's preservation proofs
  across every BindItem rule.

## Other

- `bootstrap/SAFETY-SITES.md` has a new Observations entry.
- `check.sh` passes (113 s build).
- Time: about 2 h.
