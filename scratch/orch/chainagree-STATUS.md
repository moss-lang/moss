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

### The edge case, settled (no Audit change)

Tested on the oracle (moss_m2.exe at this worktree) and native
(ob-family's `target/release/moss`):

- **`bootstrap/examples/copy-fixed-rebind.moss`.** A copy F[Arrayed, Y]
  of Arrays with `Array.length[I32 = Y]`, copied after `bind I32 = Y`,
  then Y rebound before `bind F;`. Both implementations reject the copy:
  - the oracle: "copying `Arrays` after rebinding its fixed input `I32`
    changes its interpretation (D84)";
  - native: "the available binding was provided at a different type
    instantiation: `Arrays`".

  Audit rejects it too, through BFunCopy's `FixedAgreement W C h`
  (syntactic, at canon C). A copy site reads h's fixed inputs exactly as
  h's recorded readings.
- **`bootstrap/examples/copy-fixed-bracket.moss`.** A valid variant: F's
  `Array.length[I32 = Z]`, Z fixed in F and read as I32 at the copy site.
  It runs and prints 42 on both. Rebinding Z before `bind F;` is
  rejected by both:
  - the oracle: "applying `F` here reads `Z` as `Bool`, but `F` is fixed
    in it ...";
  - native: "provided at a different type instantiation".

  This is AppPremises' fixed clause.

Why the mismatch cannot arise for a platform root:
- CopyOutputs compares receivers exactly (`provision_id`). The root's
  polymorphic inputs (Boxed, Arrayed, Memory) are its receivers or
  receiver parameters, so their instantiation entries are pinned on both
  sides.
- The other instantiation and signature positions hold fixed inputs of h.
  The copy site must read these exactly as recorded (FixedAgreement),
  back to the root's own readings (main's machine types).
- A fixed input of u is pinned at the application by AppPremises.

So for every position, sg's image never mentions a polymorphic input of
u, and equality up to sg becomes equality at θ, canonically in C.

### What the proof of ChainForwardKeys needs (not started)

The argument above uses syntactic facts that the copy chain does not
carry. `CCStep`/`ClosureOk.CCopy` record sg only grounded
(`gfx = ground_fixed m sg`). Before step 1 of the plan, extend
`CCopy`/`CCStep` (PublicBodySlice, PublicCopyApply; shared with results4,
so coordinate) with, at the copy site C1:
- `sg = map (fun q => (fst q, canon C1 (snd q))) fx`, where fx is the
  `c_funs` entry `set_fun` records for u. BFunCopy has it verbatim. In a
  formed C1 the entries are canonical, so sg = fx.
- h's `FixedAgreement` at C1, relating sg on h's fixed inputs to h's own
  recorded readings.
- The root's fixed readings, which are the machine types (PlatformFixed).
  This is syntactic at main through FixedMachinesRead and needs a
  syntactic version of CPlatform's PlatformFixed.

Then, at the application, AppPremises' fixed clause gives
`canon C (C.ty x) = canon C (fx x)`. With these, steps 1-3 of the plan
apply. One gap remains: the application's canon C versus the copy site's
canon C1 on the recorded readings. This needs `canon C (canon C1 t) =
canon C t` for recorded t, or that the recorded readings are canonical
at both sites, and has to be checked. results4's linked-root chains need
the same facts.

## Time

About 4.5 hours.
