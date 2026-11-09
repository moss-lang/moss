# callerkeys2: Audit change — the caller-keyed result install

(Round 2 of scratch/orch/roles-STATUS.md, started by the stopped
`callerkeys` agent, finished here.)

## What

An application installs exactly F's declared result, position by
position, under the caller's keys, reading each position from the root's
source for it through each copy's position map.

- `BApply u th outs sm rs`: `rs` is the caller's key for each position of
  F's declared result, `map (rkey_of C') ks` with `C'`, `ks` from the
  rule's `ProvideResult` at the caller (`BApplyAbstract`, `LitOpFamily`,
  the Provided family rule, `FamilyStep`/`Demand`).
- `functor_code.fc_sources : list item`: the root body's key for each
  position (`LinkBody`: `rkey_of Cb (at_site Cb (output_renaming outs gen
  k))` per key k of the root's result; `Platform.Family`: `rkey_of
  outputs key`). `fc_roles` is gone.
- `BCopyFunctor u h sm rm`, `FunctorClosure h sms dc rms`: a copy appends
  its position map `rm` (for each position i of F's result, a position j
  of H's). `chain_sources rms srcs` reads the root's sources through the
  maps.
- `apply_functor` selects `result_selection rs rms (fc_sources code) :=
  rev (combine rs (chain_sources rms (fc_sources code)))`. `result_smap`,
  `inst_smap`, `source_key`, `result_substitution`, `role_outputs`,
  `output_destination` are gone. `read_result_types` and
  `result_environment` are unchanged.
- `CopyOutputs W sigma u h Cu rm`: results4's converse conjunct (every key
  of H's result is one of F's) is reverted. Instead `length rm = length
  ks` and, for each position i of F's result with key k, `rm[i] = j` with
  `ks'[j]` of the same provision id and j the last such position. The
  existing clause (F's and H's provisions for each key agree after the
  copy site's substitution sigma) stays. `BFunCopy` picks `rm`
  existentially and emits it.

## Why

Three obligations were false because the root body's keys were read in
the caller's terms (θ and the output roles), which can differ
syntactically from the caller's own keys:

- `ChainForwardCopyKeys` (PublicChainFwd):
  bootstrap/examples/linked-copy-bracket.moss. A copy F of Arrays with
  `Array.length[I32 = Z]`, applied in a linked body where Z and I32 are
  distinct: the caller's key carries `I32 = Z`, the root installs at
  `θ(I32) = I32`.
- `CN.ChainKeysOk` (PublicChainNew): the same shape for a linked root.
- `FS.FamilyCopyApplyOk`'s key half (PublicFamilyStep):
  bootstrap/examples/family-copy-linked-bracket.moss (hunt-family t11):
  family copy F with `A.len[W = Z]` of H (`A.len`); the demand is keyed
  `[W := Z]`, the root installs at `[W := W]`.

And results4's converse `CopyOutputs` rejected
bootstrap/examples/copy-extra-key.moss, a copy native accepts and runs
(31), because the old application installed every key of the root's
result. With the caller-keyed install a key H has and F lacks is never
installed, so the converse is unnecessary.

## Position matching (settled)

Keys of a result read at Cu carry no instantiation (`entry_key` gives
`mkey`), so "the same provision id" is key equality at Cu; the
instantiations are compared through the provisions, after the copy
site's substitution (the kept clause). Choosing the *last* such position
makes repeated positions of F read one source in H (the oracle's own
`find ... (rev gkeys)`).

## Repeated caller keys

hunt-chain: with repeated positions, a first-wins selection would install
the first position's source while the caller's static context types the
key at its last setter (`output_cx`'s `rev es`). Two of F's keys can meet
only at the caller (A.m and B.m with θ(A) = θ(B)), so "duplicates share
one source" cannot be guaranteed by the chain alone. `result_selection`
is reversed instead: the last position wins, which is the static last
setter (`rkey_of C'` is injective on provision ids,
`PublicCallerKeys.rkey_of_inj`, and result keys are their own ids,
`result_key_id`). This rules the problem out at every caller.
`ChainForwardSigs`/`ChainForwardCopySigs` are stated at the last position
of each caller key (`selection_last`).

## Tests (bootstrap/examples, header comments)

| file | oracle (ob-family4 moss_m2) | native (ob-family build, seed 1h1710x0…) | Audit |
| --- | --- | --- | --- |
| copy-extra-key.moss | accepts, prints 21 (E32) | prints 31 | accepted again; its application installs only F's keys, so 31 |
| linked-copy-bracket.moss | elaborates, `stuck (XErr 6)` | prints 42 | the family primitive is filed under `Array.length[I32 = Z]`, the call finds it |
| family-copy-linked-bracket.moss (t11) | `stuck (XErr 6)`, exit 3 | exit 0 | filed under the caller's `[W := Z]` |
| output-merge-key.moss | `stuck (XErr 6)` | prints 7 | the body's key for the position is read, never respelled (E33) |
| tests/regressions/functor_input_output_generations.moss | `n`, then stuck | `yn` | same |

## Evidence that the copy check rejects nothing it accepted before E32

The pre-E32 `CopyOutputs` is the kept clause alone; the new rule adds
only that each of F's keys is a key of H's result (not just of Cu's
provisions). The oracle's `copy_outputs_match` already requires that,
matching keys after the copy substitution. An oracle variant that matches
by key equality first and then asks the substituted agreement
(scratch/orch/callerkeys2-cmp.sh, variant built from ob-family4's
extracted Elab.ml in scratch/ck/variant) gives the same verdict as the
oracle on all 398 corpus files (tests/, examples/, bootstrap/examples/,
scratch/hunt/; scratch/orch/callerkeys2-cmp.out: 398 SAME, 0 DIFF),
including every copy example.

## Alternatives considered

1. Keep results4's converse and only fix keys: still rejects
   copy-extra-key.moss, which native runs.
2. First-wins selection with "duplicates share one source" as an
   invariant: false when two of F's keys meet only at the caller.
3. Match positions by key agreement after sigma only (the oracle's
   order): two distinct keys of H at Cu could then both match, and a
   witness picking the wrong one would make the realization premise
   false (Audit is relational). Key equality at Cu plus the provision
   agreement after sigma avoids that; the corpus shows no difference.

## Oracle

Unchanged (E32, E33 updated in proof-plan.json: the oracle should
install F's declared result under the caller's keys too).
