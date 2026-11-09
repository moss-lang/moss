# Status: ChainForwardKeys ("chainfwd")

Branch ob-chainfwd, from safety at 7b39518, with safety 67343d6 merged. check.sh passed at
step 1 (1188d54). After that only an example and this file changed.

## Step 1 (done): copy-chain facts

- `PublicBodySlice`:
  - `CopySite u h Cu gfx gfh`: the copy site C1 of `bind u = h;`, read
    syntactically from BFunCopy. It holds:
    - C1 is formed, absorbed by d, and canon C1 is idempotent;
    - fx is u's fixed readings in C1;
    - `CopyOutputs W (map (fun q => (fst q, canon C1 (snd q))) fx) u h Cu`;
    - `gfx = ground_fixed (type_meanings d) fx`;
    - `nlook h (c_funs C1) = Some fxh` and `FixedAgreement W C1 h`;
    - fxh covers h's fixed inputs, and gfh is fxh read in d.
  - `PlatformRead u gfx`: PlatformFixed syntactically. gfx is
    `ground_fixed gm fx` for a functional fx covering u's fixed inputs,
    and fx records each fixed machine-type input x as `TAbs x`.
  - `ClosureOk`: `CCopy` gains `CopySite`, and `CPlatform` gains
    `PlatformRead`. Established in `case_BFunCopy` and in PublicEntry's
    entry realization (from FixedMachinesRead), and threaded through
    `closure_lower`.
- `PublicCopyApply.CopyChain`: `CCStep` gains `S.CopySite`, established
  in `chain_entry`.
- The `CCStep` induction patterns in PublicPlatformApply gain a trailing `_`.

## Step 2: ChainForwardKeys is false as stated (counterexample)

`bootstrap/examples/linked-copy-bracket.moss`: F is a copy of Arrays with
`Array.length[I32 = Z]` (as in copy-fixed-bracket.moss), applied inside a
linked body G instead of main. Results:
- the oracle, built at safety 67343d6, elaborates it (`--elab-only`
  exits 0) and the run is **stuck (XErr 6)**;
- native (ob-family build, seed 1h1710x0...mossc.wasm, run on a copy
  that imports ob-family's lib/) prints 42;
- the variant without the bracket runs and prints 42 on both
  (`scratch/chainfwd/linked-copy-plain.moss`).

Why: ChainForwardKeys' first clause is a syntactic equality of run-time
keys, `θ(rkey_of outputs key) = rkey C' r m`. The caller's key for
`length` carries `I32 = canon C (C.ty Z)`. The root installs it at
`θ(I32) = canon C (C.ty I32)`. In main these coincide: F's record there
comes from the copy site, where Z reads as I32, and AppPremises' fixed
clause pins it. In a linked body's context (a FunCtx), F's record is the
self-reading (fx Z = Z), so the fixed clause holds trivially. Z and I32
are distinct symbols there. The copy site's CopyOutputs equated the two
only up to the copy site's readings (sg). Inside the body they are equal
only in ground meaning (CapturedFuns, FixedMeans), not syntactically.
More generally, a closure's syntactic facts (CopySite) are tied to the
current context's record of the functor only through ground meanings.
This is the "one gap" the chainagree handoff flagged (canon C vs canon
C1), and it is real.

Consequences:
- No proof of ChainForwardKeys exists. Audit accepts a program its
  semantics runs stuck, so safety itself fails here unless Audit
  changes.
- Native accepts the program, so per ORCH rule 2 a rejection is not
  allowed. The fix must be semantic. Options:
  (a) key the root's installs and the caller's lookups by ground
      meanings;
  (b) have the copy closure re-key its result, as sms re-keys needs: a
      result smap from the root's keys to the copy's, read at the copy
      site and carried in the closure;
  (c) record, per closure, the copy's own result keys and install
      under them.
  This needs a decision by the coordinator or user.

What still looks provable:
- The empty-chain case (u = h0, a direct `bind Heap;`/`bind Arrays;`),
  from a generalisation of PublicFamilyAgree.family_items_agree to
  AppPremises with Cf = C.
- Needed for that:
  - The section hypotheses replace FamilyApplication by its parts:
    th's shape, InF, the merge clause, functor body None, ProvideResult
    at Cf, and c_sub Cf = c_sub C.
  - NoDup (map provision_id ks) has to be replaced. AppPremises does not
    give it, and duplicate root items cannot be excluded without it.
    Use a "last pair" lookup lemma: with MethodsDistinct, caller keys
    are equal exactly when root keys are equal, so ilook on both sides
    hits the same pair.
  - KeysT for C (DomT's second clause): FormedCx's TysScoped is too
    weak alone. It also needs "a path names no generation" or a premise
    like FamilyKeysTyped.
  - Body None for a platform root: not derivable from Platform.Family
    as it stands.
- Not started, since the copy case is blocked on the decision above.

## Time

About 2 hours.
