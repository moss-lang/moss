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
  global context), plus the paper's Figure 1 example encoded and
  executed by computation. The header comment lists the handful of
  mechanization-level deviations from the paper.

Both are built by flake checks, so CI keeps them compiling, and the
paper is an ordinary package:

```sh
nix build .#pdf                                     # writes result/core-moss.pdf
nix build .#checks.x86_64-linux.core-calculus-rocq  # (your system for x86_64-linux)
```

The PDF itself is not checked in; build it with the command above, or
run `pdflatex` twice in this directory with `mathpartir` on the TeX
search path.

## Next: consistent merging (D43), with the D62 repair

The one load-bearing mechanism still outside the calculus, planned as a
single focused session. Agreed design (July 2026):

- **Paper**: named contexts stay meta-level sugar (flattening). The new
  content lands on *telescope formation*: a flattened telescope may
  mention one key twice, and formation **merges** by unifying the
  colliding bindings — symmetric, no target primary — failing exactly
  when two distinct concrete heads would be identified [D43]. Add an
  occurs check (D43 is silent on cyclic merges; likely a finding).
- **Mechanization**: orient the unifier — the merge function returns
  (deduplicated items, substitution entries `C ↦ B` for a chosen
  representative) or a clash, and those entries ride the existing σ
  machinery unchanged. Record orientation as a deviation in the header
  (same style as deviations 1 and 5); upgrade to a congruence
  formulation only if representative-invariance ever matters.
- **Knock-ons** (localized, not a rewrite): `sat`'s totality and
  coherence go per-equivalence-class (θ instantiates representatives
  and must respect the equations — "binding one binds them all" falls
  out here, so the freshness discipline is untouched); a handful of
  leaf lemmas that the unifier preserves `sigma_regular`, idempotence,
  and the frees discipline; dynamics and phase separation untouched.
- **Do together with [D62]**: re-key method provisions by the
  receiver's (declaration, static bindings) identity, compared modulo
  the merged σ — merging is what makes identity keying practical, and
  D62's live bootstrap hole is the payoff.
- **Method**: the proof harness from the original effort — frozen-spec
  contract, statement-freeze guard over `(* BEGIN/END *)` FILL blocks,
  whole-file `coqc` as the gate, `Print Assumptions` audit on the six
  theorems — with leaf proofs delegated to subagents (Claude or Codex)
  and refutation reports treated as first-class deliverables.
