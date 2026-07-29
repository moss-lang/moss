# The Core Moss calculus

A distillation of the language pinned down by the
[semantics decision log](../semantics.md) into a core calculus, in the
style of a programming-languages research paper, plus a mechanization of
its definitions and metatheoretical statements.

- [`core-moss.tex`](core-moss.tex) — the paper: syntax, typing rules, and
  dynamic semantics of the core, with the full language reconstructed in
  its final section.
- [`CoreMoss.v`](CoreMoss.v) — the Rocq mechanization: all definitions,
  the metatheory of the paper's §4 stated precisely (Proposition 4.1
  proved; the rest `Admitted`), and the paper's Figure 1 example encoded
  and executed by computation. The header comment lists the handful of
  mechanization-level deviations from the paper.

Both are built by flake checks, so CI keeps them compiling:

```sh
nix build .#checks.x86_64-linux.core-calculus-pdf   # writes result/core-moss.pdf
nix build .#checks.x86_64-linux.core-calculus-rocq
```

(Substitute your system for `x86_64-linux`.) The PDF itself is not
checked in; build it with the command above, or run `pdflatex` twice in
this directory with `mathpartir` on the TeX search path.
