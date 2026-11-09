# coherentpf: status

Branch ob-coherentpf, from safety e69a7cd, with safety d5dee03 merged
(it includes copyfresh 460a720 and linknewpf 94d89ce). check.sh is green.

## Proved

`S.FamilyCoherentOk` is proved and removed from `safety_obligations`
(PublicEntry.v). Its statement is unchanged. What changed is the meaning
of `Formed`: its `CxScoped` gained an invariant.

- **The invariant: `PublicScoping.MethsRead W C`**, the 4th conjunct of
  `CxScoped`. It has three parts:
  - the merge is idempotent: `canon C (canon C t) = canon C t`;
  - the table is canonical: `canon C v = v` for every `(x, v)` in `c_ty C`;
  - every `(mkey r m, ProvidedMethod g a)` in C satisfies `MethRead`. That
    is, there are tm, rt and g0 with `SigOf W m tm (Some rt) g0`,
    `canon C rt = r` and `g = canon_sig C g0`. And if m is a method symbol
    (`DKMeth`), then for every mentioned symbol x (`SigMentions`) with
    `nlook x tm = Some v`, `(x, canon C v)` is in `theta_of C a`.

  `MethRead` uses no `AppOf`, so a frozen instantiation (entries_cx) fits
  too.
- **Where it is established and kept** (all in PublicScoping):
  - `methsread_entries`, via `entries_cx_scoped`, from the merge's
    idempotence (`MostGeneralMerge`) and `entries_read`/`root_read`. This
    covers `ctxof_cxscoped` and `functx_cxscoped`.
  - `methsread_set_ty`: `set_ty_scoped` now takes `canon C v`, its only use
    (BTypeBind).
  - `methsread_set_provision` and `methsread_provide` (non-method keys);
    `set_provision_scoped` gained a premise, and `provide_scoped` now needs
    a non-method key.
  - `methsread_meth_bind` for BMethBind. The stored `sg` is canonical by
    `sigof_canon`/`tyof_canon`/`brackets_canon`, a reading at a canonical
    table being canonical.
  - `methsread_provide_result` and `provide_result_meths` for functor
    results (`apply_abstract_cxscoped`, `family_result_cxscoped`).
  - `methsread_narrow` (`cxscoped_narrow`).
  - `lambda_cxscoped`, which gained a `MethsRead W C` premise, as did the
    slice's `lambda_callable`.
  - The slice's empty const-initializer context.
- **The proof: PublicFamilyCoherent.v** (new, added to Certificate.v roots).
  - `tyof_on`/`br_on`: two readings of one type expression agree, under a
    substitution, when the tables agree at the symbols `FreeIn` reports.
    This needs `RelevanceClosed` (`wo_reqs_closed`) for aliases. `sig_on` is
    the same for signatures, over `SigMentions`.
  - `method_named_meth`: a method named in a loaded module is `DKMeth`,
    through `exports_detached`.
  - `family_member`: the family's provision at a key in ks is an OIEMeth
    reading (the new entries come first in `ilook`), at a `DKMeth` symbol
    (decl_loaded).
  - `family_coherent`: θ equality makes the two tables agree at the
    mentioned symbols, and the key fixes the receiver, so `sig_on` applies.
  - The instance is `PublicEntry.family_coherent_ok`.

## Statements that grew

No `...Ok` statement was edited. Any obligation that concludes `Pre` or
`Formed` now also concludes `MethsRead` for that context, since it is
inside `CxScoped`. That covers ChainRealizeOk, FamilyLinkNewOk,
FamilyCopyRealizeOk and ChainForwardCopySigs, if they do. Every proved
lemma that delivers Pre (family_step_ok, copy_apply_ok, link results,
platform apply) still compiles unchanged, because each goes through the
PublicScoping step lemmas. Whoever proves the remaining ones must supply
MethsRead for their contexts, usually via `cxscoped_narrow`,
`apply_abstract_cxscoped` or `family_result_cxscoped`.

No new premises. No Audit change.

## Remaining in safety_obligations

ChainRealizeOk, ChainForwardCopySigs, FamilyLinkNewOk and
FamilyCopyRealizeOk. They belong to other agents.

## Time

About 2.5 hours.
