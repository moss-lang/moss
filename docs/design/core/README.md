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
  global context), plus the paper's Figure 1 example and the merging
  examples of §12b encoded and executed by computation. The header
  comment lists the handful of mechanization-level deviations from the
  paper.

`coqc` accepts `Admitted`, so compiling is not on its own evidence that
the metatheory is unconditional. §13 of the file audits the assumptions
of every theorem, and the flake check enforces both halves: each audited
name must report *closed under the global context*, and the audited names
must be exactly the file's top-level `Theorem`s, so a theorem cannot be
added without being audited.

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
