# refile: status of `RefileRunOk`

## Verdict

`RefileRunOk` is false for a program both implementations accept, so it
cannot be proved for the current Audit. I proved it from a sharper
condition, `RefileShadowOk`, and `safety_obligations` now takes that
condition in place of `RefileRunOk`. Audit.v is unchanged: each fix I
found breaks a rule (see "Fix options"), so the choice is left to the
orchestrator.

## Counterexample

`bootstrap/examples/refile-stale-shadow.moss`. F is `Std, T, seed -> T,
T.out, r`. G has the same context and result, and its body is:

    bind T.out = () => 7;          # receiver TAbs T: T is still G's input
    bind Int.out = () => this + 1; # the body's T.out once T = Int
    bind r = seed.out();           # seed: T, Bool at run time
    bind T = Int;

LinkBody's pairs map the body key `IMth Int out` to F's key
`IMth (TAbs T) out []`. Right after `Int.out`, `refile` installs
`Int.out`'s closure under F's key. That is also the runtime key of the
first, stale `T.out`, which the context still lists (EnvOk requires it at
receiver T). `seed.out()` then runs `Int.out` on a Bool.

- oracle + reference interpreter (`moss_m2.exe FILE`):
  `[moss_ref: stuck (XErr 14)]`. With `() => 8` in place of
  `this + 1`, it prints 8, which shows the call reaches the refiled
  closure.
- native (`moss run`, moss-safety's binary, with lib imports pointing at
  moss-safety/lib): prints 7.
- In variant v4 (scratch/refile/v4.moss), the caller calls `made.out()`
  after `bind F`. Both implementations print 6, so the caller's `T.out` is
  `Int.out` in both. Native therefore files the result under F's key too,
  but the alias is not visible inside the body.

## What is proved (bootstrap/theories/PublicRefile.v, functor `Refile`)

- `refile_extras`, `refile_split`: `A.refile pairs b = b :: refile_extras pairs b`.
- `NoShadow pairs b C1`: no method provision of C1 has a runtime key
  (`A.rkey C1 r m`) equal to one of b's extra keys. `RChain` is `S.Chain`
  with `NoShadow` for the context each step leaves, and `rchain_chain`
  forgets that extra condition.
- `refile_rchain_run`: the analogue of chain_run over
  `flat_map (A.refile pairs) bs`. The run either completes in the chain's
  last context, with `Pre`, a heap extension and `MeaningsGrow`, or stops
  with an Outcome. Helper lemmas:
  - `realizes_install_free` and `pre_install_free`: an install under a
    free method key keeps `Realizes` and `Pre`.
  - `select_install`: a capture selection survives an install.
  - `extras_run`: runs the extra installs.

  The method step's resulting environment is read off the step's own run
  with an empty rest, which gives `install (IMth t m th) clo d`.
- `RefileShadowOk` (the remaining obligation): for `LinkData` and any
  `Chain` of the body, the chain is an `RChain` for the refile pairs.
- `refile_run_ok : RefileShadowOk -> S.RefileRunOk`.
- PublicEntry: `Module RF := PublicRefile.Refile ...; Module S := RF.S`, so
  there is one slice instance, because chains are inductives.
  `safety_obligations` takes `RF.RefileShadowOk` in place of
  `S.RefileRunOk`. Certificate.v roots now include `refile_run_ok` and
  `refile_rchain_run`.

## Not done

- The results lemma: the F-view key found in d3 with the method's closure.
  It does not follow from an abstract chain. Later steps (another method
  bind or a nested BApply) are known only through `BindStep`, which says
  nothing about keys their context does not list, so they may overwrite
  either key. The extra installs also re-select `sm` after the bind's own
  install. If `sm` selects the bind's own key, the extra closure's capture
  differs from the bind's. Stating that lemma needs a frame condition on
  steps (the keys a step writes), which `BindStep` does not carry.
- `RefileShadowOk` itself. It is false for the example above, so an Audit
  fix comes first. Even after a fix, it quantifies over arbitrary chains,
  whose intermediate contexts are constrained only by being realized. A
  proof would need either the slice to build an `RChain` directly in
  `item_chain`, with the `NoShadow` fact at each method bind taken from a
  LinkBody premise over the body's contexts, or a parametricity argument:
  remove the key from `d`, and since nothing writes it, no realized
  context can list it.

## Fix options (Audit), for the orchestrator

1. A LinkBody premise forbidding the shadow: no provision of the body's
   contexts has a refiled F-view key as its runtime key, except the
   method's own. This mirrors nothing either implementation checks, and it
   rejects the example, which native accepts and runs correctly
   (rule 2 says not to do that). It is the smallest change, and it fits
   the existing `link_output_certificate`: add a conjunct there.
2. Install the aliases after the body instead of after each bind, for
   example with a binding that copies a provision from one key to
   another. This matches native, and the body never sees F's keys. But
   the oracle's `link_trace_rebinds` refiles at bind time, and
   LinkBodySound's certificate equates the two, so the oracle would need
   the same change. Pre at Cb would also still need the stale provision's
   signature to agree with the alias at the end, or a Cb-only premise.
3. "Refile only when the key is new" is unsound. The caller would then
   select the stale `T.out`, which is typed at G's input T and not at the
   output.

## Time

About 2h15m (the investigation and counterexample took about 1h15m).
