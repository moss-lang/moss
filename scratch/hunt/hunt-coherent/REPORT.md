# hunt-coherent (summary written by the lead from the agent's final message)

S.FamilyCoherentOk: FALSE. sig-nonreq-shadow.moss: oracle elaborates, then
stuck (XErr 14); native prints 42; Audit accepts (traced by hand).
Cause: an abstract fn/method signature may mention non-requirement types
(`.m(): U` with requirements [A]), and method brackets may bind
non-requirements, so AppOf's θ does not determine SigOf. After `bind U =
Bool`, the family's `Int.m : () -> Bool` and the caller's `Int.m : () -> Int`
share key and θ. Survives the caller-keyed change. The planned SigCoherent
route fails for the same reason (BTypeBind breaks it).
Fix options: (1) close signatures over requirements, brackets bind only
requirements (recommended; D20); (2) compare signatures at family
application / reject keys C already provides; (3) evaluate receiver/args
before BApply (also settles control-same-u.moss: oracle 7, native 42).
