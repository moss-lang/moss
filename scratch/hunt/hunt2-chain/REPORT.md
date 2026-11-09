# hunt2-chain report (safety 4f4ded0)

Round-1 counterexample (hunt-chain/captured-rep-result.moss, now
bootstrap/examples/link-merge-captured.moss): rejected by Audit. LinkBody's
input clause now concludes that every generation of G's reading of a
linked input is in Cu, and Y is not in Cu. The oracle and native reject it
too.

## CN.ChainRealizeOk: FALSE (t3-copy-bracket-nonfixed.moss)

Claim: after `bind F;`, where F's closure runs through copies down to a
linked root body, each new provision of the caller's C' is realized at
C''s type.

Counterexample: a copy whose fixed bracket target reads, at the copy site,
as one of F's non-fixed inputs.
- `H[I]: Std, I, vi -> kk, get` with `get(): I`; `F[]: Std, I, Z, vi -> kk,
  get[I = Z]`, so X(F) = {Z} and X(H) = {}.
- `demo` (assume I) runs: `bind Z = I; bind H = G; bind F = H;
  bind I = Bool; bind vi = true; bind F; get()`.
- BFunCopy's sigma is F's fixed readings at the site, [Z := TAbs I].
  CopyOutputs compares F's `get` (():Z, [I:=Z]) with H's (():I, [I:=I]) at
  Cu_F. After sigma they are equal, and the copy is accepted.
- The problem: sigma identifies the copy site's meaning of the symbol I
  with F's input I, which every application supplies afresh. The oracle's
  copy_output_compatible sends every unsolved generation of F to a fresh
  `@copy` symbol, so for it the two differ.
- At the application, the fixed clause holds syntactically (Z still reads
  TAbs I). The linked I gets base ground(θ I) = Bool, so the installed
  `get` returns `true`. C' types `get` as (): TAbs I, whose ground type is
  Char (demo's I). EnvOkOn fails for IFn get, and main's `c.code()` applies
  Char's primitive to a Bool.
- Results: the oracle rejects at the copy ("effective output
  interpretation differs (D78)"). Native rejects at the copy ("does not
  satisfy the required signature: `get`"). Audit accepts.
- Repair: build sigma as the oracle does (match F's fixed patterns against
  the site, and send every other generation of Cu_F to a fresh symbol), or
  require that sigma's image avoids F's non-fixed inputs.
- This breaks cap3b's "copies along the chain: the linked root's facts
  transfer through θ unchanged". The transfer goes through CopyOutputs'
  sigma, which is unsound as written. The same sigma is used for family
  copies (FS.FamilyCopyRealizeOk), and the hunt-family hunter should check
  it there.

Argued safe (no program built):
- Duplicate keys at the caller: last-wins selection agrees with
  output_cx's last setter.
- Keys equal at Cu stay equal at the caller, because Satisfies includes
  the merges.
- Duplicate ids in F or H paired by the last position, including bracketed
  ones (the agreement is taken at the last setter on both sides, and so is
  the root's source).
- Cu_F merging more than Cu_H: θ satisfies the merges.
- Captured functor items: 3b covers their fx.
- cap3b's (i), (ii) and (iv) for a linked root with no copy: I found no
  break.

## CF.ChainForwardCopySigs: LIKELY TRUE

The t3 shape needs H's signatures to mention an input that is neither in
a key nor fixed.
- A platform family's polymorphic inputs (Boxed, Arrayed, Memory) all
  occur in its receivers, so in its keys. A bracket on them changes F's key
  at Cu, and CopyOutputs then finds no position.
- Everything else H mentions is fixed in H, so fixed in F (BFunCopy). It
  is checked by FixedAgreement at the site and pinned at the application.
- Trying `I32 = Arrayed` at the site is refused by FixedAgreement
  (copy-fixed-rebind.moss).
- Caller collisions are impossible: Heap, Mutable, Arrays and Memories
  have one polymorphic input and distinct methods (MethodsDistinct).
- I did not try copies of copies separately. Each step is pinned the same
  way, through the FixedAgreement chain.
