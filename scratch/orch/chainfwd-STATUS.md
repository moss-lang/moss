# Status: ChainForwardKeys ("chainfwd")

Branch ob-chainfwd, from safety at 7b39518. check.sh passes.

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

## Remaining

Step 2 (ChainForwardKeys) is not started; see the chainagree handoff.
