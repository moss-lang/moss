# Status: ChainResultAgrees ("chainagree")

Branch ob-chainagree, safety (93f37ba, with results3's PublicChainCore)
merged. check.sh passes.

## Audit change (own commit 3544241; scratch/orch/chainagree-AUDIT.md)

`Platform.Family` gains `MethodsDistinct W keys`: the family's items have
pairwise distinct methods, at most one of them detached
(`Platform.DetachedMethod`). It constrains only the platform world. The
oracle builds platform families only for lib/wasm.moss's functors
(`plat`, Elab.v `platform_fun`). Heap, Mutable, Arrays and Memories
satisfy it: each item is its own declaration, and only `.boxed` and
`.arrayed` are detached. No existing proof establishes `Platform.Family`
for the shipped world: `Platform.Linked` is a premise of the safety
theorem. The proofs that destructure `Family`/`FamilyData` were adjusted.
The lead's suggested "distinct (receiver symbol, method), no brackets" is
not what the argument needs. Brackets do not matter. Distinct leaves do
not suffice: two receivers can share a detached method, and a module can
alias a detached name.

## Proved

- `PublicPlatformApply`:
  - `ChainResultAgrees` restated, with Pre, the closure lookup and
    FixedMeans in hand. Its signature clause is now at the state's ground
    meanings (`SigAgreeG`/`ItemsAgreeG`). A copy step relates signatures
    only up to the copy site's readings of fixed inputs, which the state
    grounds as the application's. `family_core` only ever used it grounded.
  - `chain_result_agrees : WorldOf b W -> ChainForwardAgrees -> ChainResultAgrees`
    (clause B by counting).
  - `chain_forward_agrees : ChainForwardKeys -> ChainForwardAgrees`.
  - `chain_leaves`, `chain_leaves_back`, `chain_no_outputs`.
- `PublicChainAgree.v` (new, a Certificate root, no functor):
  - `scope_detached`: a module's detached scope holds detached methods
    (`WorldOf`'s `wo_scope`, by induction over `Exports`).
  - `entry_leaf` / `leaf_entry`: a reading's method entries correspond to
    the result's method leaves (through contexts).
  - `named_same`: two resolutions of one leaf are equal or both detached.
  - `chain_cover`: from the forward clause, every root key is a new
    method's run-time key at θ (MethodsDistinct).
  - `entry_plain_leaf`, `leaf_plain_entry`, `copy_news_methods`: a copy
    with the same leaves and no outputs as a method-only family adds
    methods only.
- `safety_obligations` lists `PA.ChainForwardKeys` in place of
  `PA.ChainResultAgrees`.

The `NoDup ks` question from the brief: not needed. AppPremises does not
give NoDup. With MethodsDistinct, every caller entry for method m pairs
with the one root item of method m, so duplicate caller keys are harmless
for clause B.

## Remaining: `ChainForwardKeys`

For each new method (r, m, g) of C' (`mlook r m (mths_of news) = Some g`),
it asks for a root key k0 with `θ(rkey_of outputs k0) = rkey C' r m`
(syntactic), and that the signatures agree at ground d. Plan:
1. Generalise `family_items_agree` to AppPremises (θ: u's Cu reading ->
   the caller's C reading). The FamilyApplication facts it uses all have
   AppPremises counterparts except NoDup ks, which MethodsDistinct makes
   unnecessary. `KeysT` for C should come from Pre's formation of C.
2. At each copy step u <- h, CopyOutputs: each u key at Cu is an h key at
   Cu, with provisions equal after sg.
3. Relate h at Cu to h at Ch with `family_entries_rel` (s1 = Ch's reading
   in Cu, from CCStep's ST/Sub; s2 = c_sub Cu). Then induct along the
   chain.

**Concern: the syntactic key may fail.** CopyOutputs equates the
instantiations (and signatures) only up to sg. sg reads the copy's fixed
inputs as at the copy site. The caller's run-time key needs the
instantiation syntactically.

Example: a copy F[Arrayed, Y] of Arrays has an item
`Array[I32 = Y].length`. The copy site binds `I32 = Y`, so sg I32 = Y.
Later, at the application, Y (polymorphic in F) is rebound, say
`bind Y = Int`. Then:
- the caller's instantiation reads I32 as Int;
- θ reads the root's I32 as C's entry for I32, TAbs Y.
The two agree at ground d but not syntactically, so the caller's lookup
at `rkey C' r m` would miss the installed primitive.

This is the saved-type-rebinding family of issues
(examples/saved-type-rebinding.moss). I have not checked whether
Audit/oracle/native accept such a program. If they do, ChainForwardKeys
is false for it, and the fix belongs in CopyOutputs (compare
instantiations without sg, or forbid a fixed input whose copy-site
reading mentions a polymorphic input of F). Check this before investing
in steps 1-3. results3's ChainNewOk (linked roots through copies) faces
the same converse and the same sg question. MethodsDistinct does not
help there (user code), so it needs option 1 or 3 from
famplat-STATUS.md.

## Time

About 3.5 hours.
