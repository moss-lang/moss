# sigclose: Audit change

## What

A method's provision instantiation (`AppOf`, Audit.v and ExecutionModel.v)
lists:
- the requirements (`w_reqs`, its assume nesting's type symbols) that its
  signature mentions, in their fixed order (as before);
- then every other type symbol its signature mentions (`SigMentions`,
  through nominals and bracket right sides), ascending by symbol number.

The new `Ascending` and `SigOutside` express the second part, and
`sig_outside_det` keeps `AppOf` deterministic. `ResultMentions`' `RMMeth`
drops its `In t (w_reqs W m)` premise, so a functor's M(F) counts every
type symbol a listed method's signature mentions.

The oracle mirrors the change. `Elab.signature_type_syms` returns
`filter (mentioned) rs +++ nsort (mentioned, not in rs)`, where `nsort` is
an insertion sort without repeats. It is used for a method's
`provision_app`, `check_assumed_requirements` and `result_mentions`.

## Why

hunt-coherent found `S.FamilyCoherentOk` false. Its counterexample is
`~/github/moss-lang/moss-safety/scratch/hunt/hunt-coherent/sig-nonreq-shadow.moss`,
copied here as bootstrap/examples/sig-nonreq-shadow.moss.

A method's θ did not determine its signature. A family result's `Int.m`
read at U = Bool and the caller's `Int.m` read at U = Int shared
mkey and θ `[]` and differed in signature. The oracle shared the defect:
it was stuck with XErr 14, while native prints 42.

The user chose to close signatures over requirements (D20), with every
type symbol a signature mentions counted as a requirement, so that θ
records it.

## Test and evidence

- bootstrap/examples/sig-nonreq-shadow.moss: the old oracle was stuck
  (XErr 14). The new oracle prints 42, exit 0, with distinct keys
  θ [U := Int] and θ [U := Bool]. Native prints 42.
- tests/wasi/merge.moss prints qbq. tests/wasi/bridged.moss, which goes
  through signature.moss, prints `!!`. Neither changed.
- Corpus comparison: scratch/orch/sigclose-cmp.sh runs the oracle at
  safety d2b1a68 against this one, over full runs of tests, examples and
  bootstrap/examples. The output is scratch/orch/sigclose-cmp.out: 381
  same, 3 different.
  - sig-nonreq-shadow.moss: stuck, then 42.
  - unbound-merge-representative.moss: stuck (XErr 10), then rejected
    (D62: "calling `f` needs `Char.w` at another instantiation"). It was
    already recorded as unsafe in SAFETY-SITES.
  - partially-bound-merge.moss: printed k, then rejected the same way.
    Its caller binds `C.w` at Foo = Char but leaves Y unbound, and f
    needs `C.w` at Foo = Y. Adding `bind Y = Char;` makes it accepted
    again (it prints k). This is the only previously working program the
    change rejects. Native still accepts it.

## Alternatives considered

1. Reject signatures that mention non-requirements, as first asked. The
   designer's D43 example tests/wasi/merge.moss (`fn .gimme(): Foo;` with
   `A.gimme[Foo=B]`), tests/wasi/signature.moss and about 28 other corpus
   files use that idiom, so I stopped and reported instead
   (scratch/sigclose/scan.out).
2. Widen `w_reqs` itself for fns and methods. Every user of `w_reqs` and
   the elaborator's requirement-cache proofs (ElabFrameCore ReqOK,
   WorldSound, ReqsTotal) would change, and fns' functor fixed inputs
   would grow.
   - For methods, `AppOf` is the only place requirements reach a key; a
     method item's brackets already bind any type symbol (note 56).
   - Fns' keys carry no θ.
   - So I widened only a method's θ (and M(F) through `RMMeth`), with the
     extra symbols ordered ascending rather than by a new world field.
3. Compare signatures at family application, or evaluate receivers first
   (hunt REPORT fixes 2 and 3). Not chosen by the user.
