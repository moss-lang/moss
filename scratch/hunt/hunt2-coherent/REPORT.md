# hunt2-coherent: S.FamilyCoherentOk (PublicBodySlice.v:480), round 2

## Verdict
FALSE as stated: there is a formal counterexample, and no program is needed for it.
LIKELY TRUE for contexts that elaboration actually reaches: no accepted program
violates it.

## Formal counterexample
The obligation quantifies over every `Formed F C`. `Formed` is `FormedCx`
(TmOk, ProvOk, KeyOk, scoping) plus `FrameFormed` and `CxScoped`. None of these
says that a method provision's signature is a `SigOf` reading at its θ.
SAFETY-SITES row `family_coherent` already says "Pre does not carry that as an
invariant". Here is a context that is Formed and still fails the obligation.

Take W to be the world of `../hunt-coherent/control-same-u.moss`, which is
accepted and linked. Let `C1 = C` with:
- c_sub = [] and c_ty = [(U, Int)];
- provisions:
  - (IFun F, ProvidedFunctor [(U, Int)]);
  - (IVal u0, ProvidedVal Int []);
  - (mkey Int m, ProvidedMethod (FSig [] Bool) [(U, Some Int)]).

C is Formed, because Bool and Int are ok and mention no generations.
`FamilyApplication W C1 F [(A, Int)] th sm Cr ks` holds:
- F's only non-type need is u0, and Satisfies checks only needs;
- ProvideResult reads Int.m at U = Int as `() -> Int`, θ [(U, Int)].

So `FamilyCoherent C Cr ks` fails: the key is the same, θ is [(U, Int)] on both
sides, and the signatures are Bool and Int. `safety_obligations` needs
`S.FamilyCoherentOk W` for every elaborated W, so this obligation cannot be
discharged as written.

Fix: add a premise, or an invariant carried by Formed or Pre, such as
MethodReadings W C:

    forall r m g a, In (mkey r m, ProvidedMethod g a) (c_provisions C) ->
      exists tm rt g0 a0, SigOf W m tm (Some rt) g0 /\ AppOf W m tm a0 /\
        canon C rt = r /\ g = canon_sig C g0 /\ theta_of C a = theta_of C a0.

Prove it for each step that builds a context:
- entries_cx;
- BMethBind (set_mth/provide). The stored `sg` is raw, so this also needs c_ty
  to be canonical. Every reachable c_ty is canonical: entries_cx, set_ty∘canon
  and output_cx all canonicalize; family_context is only intermediate);
- output_cx (BApplyAbstract, FamilyStep through family_restore);
- lambda Cl, add_local, set_ty, set_val/set_fn.

FamilyCoherent then follows from one lemma: a TyOf reading, canonicalized,
depends only on the canonical meanings at FreeIn symbols (and `this`).

## Program level: why it holds, and what I tried
- Risk 1 (two merge substitutions) does not apply: `c_sub Cr = c_sub C1 = c_sub C`.
  output_cx keeps the c_sub of family_context C1, and set_ty keeps c_sub; the
  premise gives the rest. app_subst is structural (Audit.v:1144), so it commutes
  with TyOf.
- Risk 2 (provision_subst copies) does not apply: provision_subst only appears
  in CopyOutputs (BFunCopy) and ResultRealizes (LinkBody), which constrain the
  run-time bodies. The static Cr is always ProvideResult of F's declared result
  (OIEMeth: SigOf and AppOf at the same bs++ob++tm).
- I checked that FreeIn and TyOf read the same symbols:
  - FType and TyType;
  - FNom with TyNom, which reads all of w_reqs; brackets on a nominal must
    bind its requirements;
  - FBracket for bracket right sides, which are read at tm;
  - TyAlias, where w_reqs of an alias is FreeIn of its body (least closed, no
    listed params);
  - `this` is fixed by the key.

  Every mentioned symbol must be in force, so `known` never drops one side
  only. The θ order is a function of m.
- Fns are irrelevant to this obligation. FamilyResult allows method items
  only, FamilyCoherent is about method keys only, and a method signature cannot
  mention a fn. Fn keys carry no θ, but that is not this obligation's concern.
- Programs I ran. "Oracle" is ~/github/moss-lang/moss-ob-sigclose/scratch/sigclose/var/moss_m2.exe
  and native is the binary from the brief.
  - bracket-nonreq-family.moss (`A.m[U = Bool]` against the caller's Int.m at
    U = Int): 42 / 42. The keys are distinct.
  - nominal-arg-sig.moss (`.m(): Box`, where Box requires V; V is rebound
    before the demand): 42 / 42. θ records V.
  - merge-sig-types.moss (a telescope merges U and T through Int.m and
    Int.m[U = T]; T is rebound and then a family demand is made): 3 / 3.
  - rebind-back.moss (U = Int, then Bool, then Int again before the demand):
    oracle 5, native 42. It is type-safe and coherent. It is the round-1
    disagreement about which Int.m runs inside the demanded scope (see
    control-same-u), not a coherence failure.
- Round-1 programs: sig-nonreq-shadow gives 42 / 42 and control-same-u gives
  7 / 42.

## Note on tools
The oracle path in BRIEF2
(moss-ob-sigclose/bootstrap/_build/default/ocaml/moss_m2.exe) is the OLD build:
it is still stuck (XErr 14) on sig-nonreq-shadow. The E35 oracle is
moss-ob-sigclose/scratch/sigclose/var/moss_m2.exe.
