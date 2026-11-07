(* Copy maps are the runtime keys of the two genuinely formed contexts. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySoundCore BodyTr
 Merge CanonTypes CtxWalk CtxMerge FunctorContext SmapExec NeedExec.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Section CopyMap.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat).
Lemma copy_map_from_tables : forall ei eo Ci Co needs s sm s',
 FunEnvTables Fl ei Ci ->FunEnvTables Fl eo Co ->Tri(en_uf ei) ->Tri(en_uf eo) ->
 Forall(fun k=>key_ok k=true)needs ->
 smap_at ei eo(map(fun k=>(k,k))needs)s=Good(sm,s') ->Inv s ->Later w P R Fl s' ->
 sm=map(fun k=>(A.rkey_of Ci(key_tr Fl k),A.rkey_of Co(key_tr Fl k)))needs.
Proof.
 intros ei eo Ci Co needs s sm s' It Ot Ti To Keys Run Hi L.
 rewrite(@smap_at_context w P R Fl ei eo Ci Co _ s sm s' Run Hi L
    (fet_sub _ _ _ It)Ti(fet_sub _ _ _ Ot)To).
 - rewrite map_map;reflexivity.
 - apply Forall_forall;intros[k k']InPair;apply in_map_iff in InPair as(q&E&Inq).
   injection E as <- <-;cbn[fst snd].
   apply Forall_forall with(x:=q)in Keys;[|exact Inq].
   destruct q;cbn[key_apps];try(split;exact I).
   split;[exact(fet_apps _ _ _ It _ eq_refl Keys)|exact(fet_apps _ _ _ Ot _ eq_refl Keys)].
Qed.
End CopyMap.

Definition copy_needs_guard(mi:nat)(fg bg:gsym)(fneeds gneeds:list key):M unit :=
 miter(fun k=>if key_mem k fneeds then ret tt else let! shown:=show_key k in
 err mi ("`"++gs_name bg++"` cannot be bound to `"++gs_name fg++"`: it needs "++shown++
         ", which `"++gs_name fg++"` does not take (D78)")%string)gneeds.
Lemma copy_needs_guard_subset : forall mi fg bg fneeds gneeds s u s',
 copy_needs_guard mi fg bg fneeds gneeds s=Good(u,s') ->incl gneeds fneeds.
Proof.
 intros mi fg bg fneeds gneeds s u s' H k InK.
 destruct(miter_checked _ _ _ _ _ _ H k InK)as[sx[ux[sx' E]]].
 destruct(key_mem k fneeds)eqn:Mem.
 - apply key_mem_in;exact Mem.
 - apply bind_inv in E as(shown&st&Show&Bad);exfalso;eapply err_not_good;exact Bad.
Qed.

Definition copy_fixed_subset_guard(mi:nat)(fg bg:gsym)(xf xh:list nat):M unit :=
 miter(fun x=>if nmem x xf then ret tt else let! xg:=sym x in
 err mi ("`"++gs_name bg++"` cannot be bound to `"++gs_name fg++"`: it is fixed in `"++
 gs_name xg++"`, in which `"++gs_name fg++"` is polymorphic (D84 rule 4)")%string)xh.
Lemma copy_fixed_guard_subset : forall mi fg bg xf xh s u s',
 copy_fixed_subset_guard mi fg bg xf xh s=Good(u,s') ->incl xh xf.
Proof.
 intros mi fg bg xf xh s u s' H x InX.
 destruct(miter_checked _ _ _ _ _ _ H x InX)as[sx[ux[sx' E]]].
 destruct(nmem x xf)eqn:Mem.
 - apply nmem_in;exact Mem.
 - apply bind_inv in E as(xg&st&Sym&Bad);exfalso;eapply err_not_good;exact Bad.
Qed.
