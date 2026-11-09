# realize3: Audit change (link_site reads input symbols at G's table)

Approved by the coordinator (repair 1 of realize3-STATUS.md, round 1).

## What

`Audit.link_site` (Audit.v) now takes the output roles. Its table is
`map (fun t => (gen t, <Cb's meaning of t>)) outs ++ c_ty Cg`.
- Output roles (`gen t`) read at the body's final table Cb.
- Every symbol reads at G's own table, including an input occurrence of a
  symbol that is also an output of F.

The renaming LinkBody uses is now `link_renaming outs gen = map (fun t =>
(gen t, TAbs (gen t))) outs`, the identity on the roles. It is used both
in `ResultRealizes` and in the code's sources (`fc_sources`). So only the
site's table reads the roles. Before, `map (fun t => (gen t, TAbs t)) outs`
renamed a role back to its output symbol, and the old site's table, which
was keyed by the output symbols, then read it at Cb. That conflated the
role with an input occurrence of the same symbol.

The mirrors are mechanical:
- `PublicBodySlice.link_sources` uses `link_renaming`.
- `LinkData` uses the new `ResultRealizes`.
- `realizes_link_site`, `rkey_of_link_site` and `canon_link_site` take
  `gen`.
- ChainCore, ChainNew, CopyApply, FamilyStep, LinkResults and FamilyLink
  write `A.link_site Cg Cb outs' gen`.

No proof needed more than that; check.sh passes.

## Why

`CN.ChainRealizeOk` (length-zero chain) was false for
bootstrap/examples/link-output-input-rep.moss, by hand reading. The
argument is in the file's header and in realize3-STATUS.md.
- T is an input and an output of F, and F's merge picks T as X's
  representative.
- F's key X.m reads at F's inputs as T.m, with the input T.
- G rebinds T = Char (BTypeBind, made = [], T not a pin).
- The old link_site read that input T at Cb as Char, so G's Char.m was
  accepted and the caller filed it under Bool.m.

Now the input T reads at G's table as T, and G does not bind T.m.

## Evidence

| Program | Oracle | Native | Audit (new) |
|---|---|---|---|
| link-output-input-rep.moss | rejects ("functor `G` does not bind `X.m`") | rejects at `bind F = G` | rejects |
| link-output-input-rebind-control.moss | prints 7 | prints 7 | accepts (the key's receiver is the role, read at Cb as Char) |

- Oracle: the ob-sigclose var moss_m2, and the base build below.
- Native: ob-family, seed 1h1710x0.

## The oracle already reads this way

In Elab.v's link check (`body_key`, `made_tys`), F's result is read with
fresh output roles (`fresh_target_output_view`). `made_tys` sends each
*role* to the body's meaning of its output, and the input symbols stay as
their generations (canonical at the body). The new link_site is the Audit
form of that.

## Corpus comparison

- Script: scratch/orch/realize3-cmp.sh. Output: scratch/orch/realize3-cmp.out.
- A is the oracle built from this tree (Elab.v as in safety).
- B is a variant (scratch/realize3/varsrc, ignored). Its `body_key` also
  sends each output *symbol* to the body's meaning. That reads an input
  occurrence of an output at the body's final table, as the old link_site
  did.

Over tests, examples, bootstrap/examples, scratch/copyfresh,
scratch/realizepf, scratch/realize2 and scratch/realize3: **414 same, 1
different**.
- The difference is bootstrap/examples/output-merge-key.moss, which A
  accepts and B rejects.
  - Its context order makes T the oracle's representative of X.
  - G binds X.m before rebinding T.
  - The old Audit rule read F's key (T.m, with the input T) at the body's
    T = Int and rejected it.
  - So the old rule rejected a program the oracle accepts, and the new
    rule removes that incompleteness.
- The two new examples are rejected by both A and B, by G's own result
  check.

## Alternatives considered

- **A LinkBody clause: an output that is also an input is no other
  input's merge representative.** It is smaller, but it does not match the
  oracle, and it would reject output-merge-key.moss.
- **Forbid rebinding an input-output in the body.** That rejects the
  control, which both implementations accept.
- **Constrain the merge representative to the oracle's.** That is
  assessed in realize3-STATUS.md. The oracle is not protected by its
  representative choice (output-merge-key.moss has representative T).
  It is protected because it never re-reads an input generation through a
  rebound symbol.
