# The Core Moss calculus

A distillation of the language pinned down by the
[semantics decision log](../semantics.md) into a core calculus, in the
style of a programming-languages research paper, plus a mechanization of
its definitions and metatheoretical statements.

- [`core-moss.tex`](core-moss.tex) — the paper: syntax, typing rules, and
  dynamic semantics of the core, with the full language reconstructed in
  its final section.
- [`CoreMoss.v`](CoreMoss.v) — the Rocq mechanization: all definitions
  and the full metatheory of the paper's §4, machine-checked end to end
  (every theorem, type soundness included, reports closed under the
  global context), abstract functors [D78] included, plus the paper's
  Figure 1 example, the merging examples of §12b, and an abstract
  allocator (§12c) encoded and executed by computation. The header
  comment lists the handful of mechanization-level deviations from the
  paper.
- [`Narrowing.v`](Narrowing.v) — [D71]: a tag's identity is its
  relevant parameters (the type symbols its payload mentions), not its
  whole assume nesting, and that is sound for the calculus: value typing
  cannot tell apart instantiations that agree on them
  (`vty_narrow_invariant`), every well-formed type at a narrowed
  signature is already narrowed (`wf_ty_narrowed`), and soundness there
  is an instance of the existing theorem (`narrowed_soundness`).
- [`KeyOnly.v`](KeyOnly.v) — why that depends on method items carrying
  their application θ: the same rules with method requirements matched
  by (receiver, method) alone admit a well-formed program that goes
  wrong, which the calculus rejects. Documentation of the coherence
  rule both compilers now check, not a model of the language.

`coqc` accepts `Admitted`, so compiling is not on its own evidence that
the metatheory is unconditional. §13 of `CoreMoss.v`, and the closing
`Print Assumptions` lines of the other two files, audit the assumptions
of every theorem, and the flake check enforces both halves: each audited
name must report *closed under the global context*, and the audited names
of each file must be exactly its top-level `Theorem`s and `Corollary`s,
so a theorem cannot be added without being audited. The two later files
import the first as `NM.CoreMoss` (`coqc -Q . NM`).

Both are built by flake checks, so CI keeps them compiling, and the
paper is an ordinary package:

```sh
nix build .#pdf                                     # writes result/core-moss.pdf
nix build .#checks.x86_64-linux.core-calculus-rocq  # (your system for x86_64-linux)
```

The PDF itself is not checked in; build it with the command above, or
run `pdflatex` twice in this directory with `mathpartir` on the TeX
search path.

## Consistent merging (D43), with the D62 repair: landed

The July 2026 plan for this section has been executed; the last
load-bearing mechanism is now inside the calculus, in both artifacts:

- **Paper**: named contexts stay meta-level sugar (flattening), and the
  new content landed on *telescope formation*: a flattened telescope may
  mention one key twice, and formation **merges** by unifying the
  colliding bindings — symmetric, no target primary — failing exactly
  when two distinct concrete heads would be identified [D43], or when
  the equations are cyclic (the occurs check; D43 is silent on cyclic
  merges — the anticipated finding, now recorded as repair (vii)).
  Satisfaction is judged against the merged telescope (S-Tel): totality
  per equivalence class, plus the merge equations, so "binding one binds
  them all" holds at every use site.
- **Mechanization**: the unifier is oriented (header deviation 9): the
  merge keeps the earlier occurrence of a key as the representative and
  returns substitution entries `C ↦ B` that ride the existing σ
  machinery unchanged. The unifier is fuel-indexed and its candidate is
  *verified* at formation (images identified, σ regular and idempotent),
  so no property of the unifier itself enters the metatheory.
- **Knock-ons** were as predicted — `sat`'s totality and coherence went
  per-equivalence-class, plus a handful of leaf lemmas (`merge_item_*`,
  `tele_sigma_regular`, `tele_ctx_tyreqs`); dynamics and phase
  separation untouched.
- **[D62]**: method dispatch uniqueness is judged modulo the merged σ
  (header deviation 10, T-Meth in the paper) — provisions are keyed by
  the receiver's (declaration, static bindings) identity, compared
  through σ, so two spellings of one merged atom are one provision.
- **Second finding, from doing both at once**: the *merge* key for a
  method item must be the receiver's type too, not its head. Head
  keying gives `Slot[Elem=Int].get` and `Slot[Elem=Char].get` one key,
  so a telescope needing the method at two instantiations would try to
  identify `Int` with `Char` and be rejected at formation — the same
  error D62 repairs in dispatch, one judgment earlier. D43's own example
  still merges: there the receiver is shared and it is the method's own
  application that differs. Both behaviours are pinned by computation
  in §12b.
- **Executed evidence**: §12b of `CoreMoss.v` computes the D43 example's
  merge (`C ↦ B`), the concrete-head clash, the occurs-check rejection,
  and runs a merged program (plus its erasure) to `()` by `vm_compute`.

## Abstract functors (D78), with type outputs: landed

[D78]'s calculus plan, including its type-outputs amendment, is in both
artifacts (paper §3.4, Figures 8–9; `CoreMoss.v` deviations 11–13):

- **Syntax.** An item `IFun φ` (an abstract functor in a telescope; its
  argument and result telescopes, and a body for a functor that has one,
  in the signature's new `g_fun`), the expressions `EBindFun φ g ς e`
  (bind to a functor with a body), `EBindFunA φ ψ ς e` (copy the binding
  of an abstract `ψ` in force) and `EApply φ ς e`, and functor bodies as
  runs of binds without continuation (`bnd`: the five expression-level
  binds plus a nested application).
- **Statics.** A binding is checked on signatures alone (`fun_link`:
  args(g) satisfied at the identity application in args(φ)'s merged
  context, with ς; res(g) = res(φ)), and needs nothing in force. A body is
  checked once, by `binds_ty`, against its own arguments alone, and must
  leave every result item in force (`wf_fun_decl`). An application needs
  φ in force and args(φ) available at the identity application; it opens
  res(φ) over the context with its types rigid and fresh, and its scope's
  type must not mention them — the existential's unpack.
- **Dynamics.** A binding's runtime entry is `DFun g ς̄` — the functor and
  its chain of re-keyings, no captured context. An application re-keys
  the caller's environment through ς and the chain to exactly g's
  arguments, runs g's binds there (`interp_binds` / `run_binds`), and
  keeps exactly res(φ) (`interp_apply` / `run_apply`).
- **Metatheory.** Every theorem is re-proved over the extension —
  determinism, interpreter adequacy, phase separation, soundness — and
  still reports closed under the global context. The application case is
  the "polymorphic provider" lemma, `safety_binds_apply`: each link of
  the chain is an instance of the defined-call case (`link_agree`,
  `chain_agree`, over `callee_env_agree`), the body runs safely by the
  same world lemmas as the expression-level binds (`world_bindty` …
  `world_bindfuna`, factored out of their cases), and the result agrees
  with the caller's grounding extended at the result's types by what the
  body made them — the existential's witness, invisible in the scope's
  type by the escape premise.
- **What the mechanization added to the plan.** Nothing unsound turned
  up; three side conditions were needed: type outputs fresh in the
  context (as `TBindTyR`'s, deviation 7 — so two applications in one
  scope are two generations, named apart by elaboration as D72 says of
  type binds), result items added without merging into the arguments,
  and results compared as one (flattened, elaborated) telescope.
- **Executed and typed evidence.** §12c binds an abstract `Alloc: Boxed
  -> Handle, alloc, Handle.load` to a boxing functor and to a bare one
  (copied through a second abstract functor), applies it in one generic
  `roundtrip`, and runs the program, and its erasure, by `vm_compute`;
  it also computes that an application keeps exactly the result (a
  body's extra val is gone). The same program is proved well-formed —
  both bodies, the unpack in `roundtrip`, the binds and the copy in
  `main` (`functor_example_ok`) — so the new rules are satisfiable, and
  soundness covers its runs (`functor_example_safe`); both are audited
  in §13.
