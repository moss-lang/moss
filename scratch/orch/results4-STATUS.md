# results4: ChainNewOk / FamilyLinkNewOk — STATUS

Branch ob-results4, safety (1a7d125) merged. check.sh passes.

## Step 0: the CopyOutputs direction question — a real gap, fixed in Audit

`bootstrap/examples/copy-extra-key.moss`: F's result `kk, Box.m[T=Bool]`,
H's `kk, Box.m[T=Bool], Box.m[T=Char]` (same leaves). `bind F = H;` passed
the one-directional check. Applying F installs every key of the root's
result: the oracle prints 21 (G's Box[T=Char].m overwrote the caller's),
native prints 31 (keeps exactly F's result, as the spec says). No type
confusion: an extra key with a different signature has a different
run-time key (theta_of), so the effect is the wrong closure, not stuck —
but ChainNewOk's clause (a) was false.

Audit change (commit 0e211e6): `CopyOutputs` also requires every key of
H's result to have the provision id of one of F's. An extracted oracle
variant with the converse gave identical verdicts on all 373 corpus files.
Native accepts the program, so Audit is stricter than native here.
Oracle bug: E32. Record: scratch/orch/results4-AUDIT.md.

## The edge case (output that is also an input, via a merge) — real, fixed

`bootstrap/examples/output-merge-key.moss`: F's inputs merge X into T (T
the representative), T is an output, F's result has `X.m`. The code files
the key as T.m (input T), output_renaming makes it the output T, and the
application installs it under the new T; the caller's `x.m()` misses:
the oracle's program is `stuck (XErr 6)`. Native prints 7.
Audit change (commit e029263): `LinkBody` requires that `rkey_of Cr k`
mention no output symbol (only roles `gen t`). Rejects a program native
runs; not checked over the corpus. Oracle bug: E33. The faithful
alternative (keep roles distinct in the code's keys) is an interpreter
change, left to the user.

## Proved

`bootstrap/theories/PublicChainNew.v` (Certificate root), module
`ChainNew` (includes `CC := PublicChainCore.ChainCore`; PublicEntry now
takes `CC := CN.CC`):
- `chain_new_ok : ChainKeysOk -> ChainRealizeOk -> CC.ChainNewOk`:
  clause (a) of ChainNewOk is proved from the static `ChainKeysOk` (via
  `select_lookup`, `result_smap_in`, `provide_result_key_news`).
- `provide_result_key_news`: a ProvideResult's keys are its news' keys.
- `safety_obligations` lists `CN.ChainKeysOk W` and
  `CN.ChainRealizeOk W Prog argv read so` in place of `CC.ChainNewOk`.

## Remaining premises

- `ChainKeysOk` (static): for each k0 in the root's ks' (read at Ch0 with
  roles gen), `Θ (output_renaming outs' gen (rkey_of Cr k0)) = rkey_of C' k1`
  for some k1 in F's caller keys ks, Θ = result_substitution th (t ↦ gensof t).
  Plan, length-zero chain (h0 = u, Ch0 = Cu): famplat's
  `PublicFamilyAgree.family_entries_rel` with s1 = Θ ∘ ren, s2 = c_sub C,
  tables T1 = output_type_map Cu outs gen, T2 = output_type_map C outs
  gensof. Agree at an output t: Θ(ren(gen t)) = gensof t vs canon C
  (TAbs (gensof t)) (canon_abs_free; need gensof t ∉ context_gens C when
  gensof t = t — check). Agree at inputs: θ absorbs Cu's σ (th_sigma),
  and the new LinkBody premise (no raw output symbol in Cr's keys) is
  what makes the input entries' Θ = th. Then keys equal via InstSome as
  in `family_items_agree`; NoDup ks is not needed if every caller key is
  matched (chainagree's remark), but for user roots duplicates are
  possible — rkey_of C' reads the *first* news provision, check that the
  k1 chosen is that one (or use ilook).
  Copy steps: wait for chainfwd's exact copy-site facts on CCopy/CCStep
  (coordinator: do not add own versions). With CopyOutputs now
  two-directional, each step gives key-set equality at Cu.
- `ChainRealizeOk`: the new provisions realized. Body side: results3's
  `body_provides`; run fact: inputs still read as θ after the body
  (`bindings_writes` is the dynamic half; static walk using fr_pins, or
  thread `MeaningsGrow made de d3` from `chain_run` into ChainCoreOk —
  not done).
- `FamilyLinkNewOk`: untouched (no outputs; same static correspondence
  as famplat's `family_items_agree` at the linked root, plus the run fact).
- `KeptOutputsUnmentioned`: left for the user.

## Coordination

- chainagree: its sg concern was ruled out (FixedAgreement). It did not
  change CopyOutputs; my converse conjunct is the only CopyOutputs change.
- chainfwd will add copy-site facts to CCopy/CCStep.

## Time

About 3 hours.

## Round 2 (coordinator's list)

1. E33 corpus check: done. The LinkBody premise rejects
   tests/regressions/functor_input_output_generations.moss, an existing
   regression the project runs natively (`yn`) and ignores on the oracle
   (known generation-capture bug, E06/S11). Details and the size of the
   "separate output roles" alternative: results4-AUDIT.md (not small:
   FunctorCode gains the role map, apply_functor substitutes roles, proof
   repair across PublicLinkResults/ChainCore/PlatformApply/BodySlice;
   about half a day to a day). Reported; awaiting the decision, since
   ChainKeysOk's and ChainRealizeOk's statements (output_renaming, Θ)
   depend on it.
2-4. Not started (blocked on that decision for the key/type
   correspondence). Notes for whoever continues:
   - Threading the run fact: `app_step` (PublicBodySlice ~line 4767)
     destructs `chain_run`'s result as `[_ [E3 [_ [Pre3 Run0]]]]`; the
     third component is `MeaningsGrow made de d3` (de = the application
     environment). Adding it as a premise to `LinkResultsOk`,
     `ChainResultsOk` (PublicCopyApply; its copy-chain users run the root
     the same way), `ChainCoreOk`, `ChainNewOk`, `ChainRealizeOk` is
     mechanical. It still needs a static fact that `made` (BindItems'
     made list) avoids the root's inputs, and an alias fact for
     receivers read through at_site Cb (bindings_writes' alias half).
   - ChainRealizeOk needs the same static correspondence as ChainKeysOk
     (SigRel/TyRel from family_entries_rel generalised to outputs), plus
     the provisions' equality along copies (CopyOutputs' forward clause),
     so do ChainKeysOk's length-zero case first.
