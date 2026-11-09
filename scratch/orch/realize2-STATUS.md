# realize2: status

Branch ob-realize2, from safety 5e62c91. About 30 minutes.

## Stopped: CN.ChainRealizeOk is false for an accepted program (step 1)

`scratch/realize2/m8-kept-output-receiver-print.moss` is an observable
version of linknewpf's m7 (`git show ob-linknewpf:scratch/linknewpf/m7-kept-output-receiver.moss`).
It is the length-zero chain: F is linked directly to G.
- F: T, Std -> T.m, done. G: Make, Std -> T.m, done. G does not take T.
- G's body runs `bind Make;`. Make's output T is not in force in Cg, so
  BApplyAbstract's kept clause installs it as itself (gensof T = T).
  T is not in context_gens Cg or key_receiver_gens Cg, and Fg has no
  ret/this, so the clause holds. At run time T means Char (Maker).
- The body then runs `bind T.m = () => print_int(this.num())`. F's key T.m
  reads at `link_site Cg Cb []` as T.m, because T is not in c_ty Cg. Cb
  has that key, so ResultRealizes holds.
- The caller has `bind T = Bool`, so the result is filed under Bool.m.
  `true.m()` then runs Char's `code` on a Bool. EnvOkOn fails for that
  new method: the closure is callable at a Char receiver, not at
  ground d (th T) = Bool.
- The pins (fr_pins Fg = M(F), which contains T) stop only `bind T = ..`
  (BTypeBind). They do not stop an application's kept output.

Verdicts:
- **Oracle** (ob-sigclose var moss_m2): it elaborates the program, and
  the run gets **stuck (`[moss_ref: stuck (XErr 14)]`, exit 3)**. This is
  a real run-time failure.
- **Native** (ob-family, seed 1h1710x0): rejects at `bind F = G`
  ("compilation failed: `T`").
- **Control** (`scratch/realize2/m8-control.moss`): the same program with
  the caller's `bind T = Char`. The oracle runs it and prints 48 and 10.
- I checked Audit acceptance by hand (BApplyAbstract's kept clause, the
  pins and LinkBody 3b/cap3b), not by a derivation. linknewpf reached the
  same reading for m7.

This is exactly step 1 of the plan, "the body's new generations avoid F's
inputs (fr_pins)". It is false here, because made = [T] and T is in
fr_pins Fg.

FS.FamilyCopyRealizeOk is not affected. A family body allows only family
applications (FamilyBindings), and those have no outputs, so made = []
(PublicLinkWalk.family_binds_no_dests on ob-linknewpf).

## Suggested Audit repair (not made: the task says stop and report)

In BApplyAbstract's kept-output disjunct (`nlook t (c_ty C) = None /\
gensof t = t /\ ...`), add `~ In t (fr_pins F)`.
- Outside a linked body, pins = [], so nothing else changes. Nested
  `bind F;` bodies (BApplyBody, with_mod) keep the pins.
- A fresh destination (the other disjunct) is `Fresh W`, so it is
  undeclared and cannot be an input of F. With the repair, every
  destination the body makes avoids M(F). That is the static half of
  step 1 that the run fact needs: d3's meaning of each F-input equals
  the application environment's.
- Native rejects m7/m8 at `bind F = G`, so the repair mirrors native. The
  oracle accepts them and is unsafe on m8.
- Still needed: a corpus comparison. An oracle variant, or at least a
  grep of the corpus for linked bodies that apply a functor whose kept
  output is an input of the target.

## Proved

Nothing new. The starting lemmas from realizepf are in
bootstrap/theories/PublicCopyFresh.v (`copy_outputs_read`,
`result_gens`, `ground_app_subst`).

## Remaining (after the repair)

Unchanged from realizepf-STATUS steps 1-4, with step 1's static walk now
provable: by induction over BindItems, every element of `made` is fresh
or a kept output outside fr_pins. Then thread `MeaningsGrow made de d3`
out of `app_step`. For the family version, reuse linknewpf's
PublicLinkWalk/PublicLinkScope (on ob-linknewpf, not yet in safety).
