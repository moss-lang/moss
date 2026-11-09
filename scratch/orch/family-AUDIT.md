# family: Audit changes

## BApplyAbstract: every output destination is made (D78)

**What.** In `BApplyAbstract` (Audit.v):
- The `made` list the rule returns is now `map gensof outs`, every output
  destination. Before, it was `newgens`, only the fresh generations for
  outputs already in force.
- The not-in-force disjunct of the outputs premise gains
  `~ In t (gens of fr_ret F ++ gens of fr_this F)`. The frame's return and
  receiver types do not mention an output that is installed as itself.
- The frame update (`with_fresh F newgens`) and `FreshFor W F C newgens`
  are unchanged.

**Why.** An output not in force is installed as itself (`gensof t = t`) and
`result_environment` writes its meaning. With only `newgens` as made,
`BlkE`'s escape check let a value of type `t` leave the block, outside the
scope of `t` and of its meaning. Under the old rule:
- the slice's `LinkResultsOk` (`MeaningsGrow newgens d d'`) was false for
  such applications;
- "elaborated types are scoped" was false.
With the new `made`, `MeaningsGrow made` is the right claim. `BindStep`
needs `FrameAvoids made F`, so a non-fresh output must not occur in the
frame's types. That is the new premise.

**Test.** `bootstrap/examples/output-escape.moss`. The oracle rejects it:
"this block yields `T`, which mentions `T`, a type `bind F;` made in it: a
value of that type cannot leave the block (D78)". Native was not run: the
moss-safety checkout has no built compiler (`MOSS_SEED` unset).

**Rejects nothing the implementations accept (argument).**
- Escape: the oracle already treats every output as made (the test
  above).
- Frame premise: `fr_ret`/`fr_this` are read (TyOf, D20) where every
  symbol they mention is in force. Within a body the table only grows, or
  is restored on leaving a block. Lambda and functor-body frames are read
  at their own context, and functor bodies have no return or receiver
  type. So a symbol the frame mentions is in force at every `bind F;` in
  the body, and so is never an output installed as itself.
- I did not run an oracle-variant comparison over the corpus.

**Proofs touched.**
- PublicFormation `binditem_ok`: destruct pattern.
- PublicBodySlice:
  - `AppPremises`/`AppAt`: the outputs clause gains the frame premise, and
    `AppAt`'s `BindStep` is made `map gensof outs`.
  - `app_step`: `FrameAvoids` from the new premise or `FreshFor`.
  - `LinkResultsOk`: `MeaningsGrow (map gensof outs)`.
  - `CopyApplyOk`/`PlatformApplyOk`: `StepOut ... (map gensof outs)`.
- The frame stays `with_fresh F newgens` everywhere.

**Alternatives considered.**
- Adding not-in-force outputs to `fr_fresh` as well. Not needed:
  `fr_fresh` only guards later fresh generations, and a declared symbol is
  never `Fresh`.
- Proving `FrameAvoids` from an invariant ("frame types mention only
  symbols in force") instead of the premise. That would be a new
  invariant through the whole slice.
