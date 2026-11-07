(* Marked receiver readings specialize to a completed semantic type table. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit NMap Sx Elab BodyTr TyOfSubst SourceReadingExtend PMatchComplete PMatchExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition family_marker_types(marks:list(nat*nat)):list(nat*ty):=
 map(fun q=>(fst q,TAbs(snd q)))(rev marks).
Definition family_marker_subst(marks:list(nat*nat))(tm:list(nat*ty)):subst:=
 map(fun q=>(snd q,match A.nlook(fst q)tm with Some y=>y|None=>TAbs(snd q)end))marks.
Lemma family_nlook_nassoc:forall X k(xs:list(nat*X)),A.nlook k xs=nassoc k xs.
Proof.
 intros X k xs;induction xs as[|[j x]xs IH];[reflexivity|].
 cbn[A.nlook nassoc assoc];destruct(Nat.eqb k j);[reflexivity|exact IH].
Qed.
Lemma family_lookup_subst_nlook:forall g sub,lookup_subst g sub=A.nlook g sub.
Proof.
 intros g sub;induction sub as[|[j t]sub IH];[reflexivity|].
 cbn[lookup_subst A.nlook];destruct(Nat.eqb g j);[reflexivity|exact IH].
Qed.
Lemma family_marker_subst_lookup:forall marks tm p g,
 NoDup(map snd marks)->In(p,g)marks->
 A.nlook g(family_marker_subst marks tm)=Some(match A.nlook p tm with Some y=>y|None=>TAbs g end).
Proof.
 intros marks tm p g Nd Hin;rewrite family_nlook_nassoc.
 apply match_nassoc_member.
 - unfold family_marker_subst;rewrite map_map;exact Nd.
 - unfold family_marker_subst;apply in_map_iff;exists(p,g);split;[reflexivity|exact Hin].
Qed.
Theorem family_marked_subrel:forall marks base target,
 NoDup(map snd marks)->
 (forall k y,A.nlook k base=Some y->Audit.app_subst(family_marker_subst marks target)y=y)->
 (forall k,A.nlook k(family_marker_types marks)=None->A.nlook k target=A.nlook k base)->
 SubRel(family_marker_subst marks target)(family_marker_types marks++base)
   (overlay_missing(family_marker_types marks)target).
Proof.
 intros marks base target Nd Fixed Outside k;rewrite nlook_app',lookup_overlay_missing.
 destruct(A.nlook k(family_marker_types marks))as[y|]eqn:Marked.
 - pose proof Marked as InMarked;apply lookup_seed_member in InMarked.
   unfold family_marker_types in InMarked;apply in_map_iff in InMarked as([p g]&Eq&Hin).
   cbn[fst snd]in Eq;injection Eq as <- <-.
   apply in_rev in Hin.
   cbn[option_map Audit.app_subst];rewrite family_lookup_subst_nlook.
   rewrite(family_marker_subst_lookup marks target p g Nd Hin).
   destruct(A.nlook p target);reflexivity.
 - rewrite(Outside k Marked);destruct(A.nlook k base)as[y|]eqn:Base;cbn[option_map].
   + rewrite(Fixed k y Base);reflexivity.
   + reflexivity.
Qed.
Lemma family_marker_types_domain:forall marks tm W,
 Forall(fun p=>A.kind_sym W(fst p)=Some A.DKType)marks->
 @TypeExtension W tm(overlay_missing(family_marker_types marks)tm).
Proof.
 intros marks tm W Keys;apply overlay_type_extension.
 unfold family_marker_types;apply Forall_map;apply Forall_rev.
 eapply Forall_impl;[|exact Keys];intros[p g]H;exact H.
Qed.
Lemma family_marker_types_keys:forall marks,
 map fst(family_marker_types marks)=rev(map fst marks).
Proof. intros;unfold family_marker_types;rewrite map_map,map_rev;reflexivity. Qed.
Lemma family_nlook_none_keys:forall X k(xs:list(nat*X)),
 A.nlook k xs=None->~In k(map fst xs).
Proof.
 intros X k xs;induction xs as[|[p y]xs IH];[intros _[]|].
 cbn[A.nlook map fst];destruct(Nat.eqb_spec k p)as[->|Ne];[discriminate|].
 intros Read[Eq|Hin];[congruence|exact(IH Read Hin)].
Qed.
Lemma family_matching_outside:forall matching C k,
 Forall(fun q=>fst q<>k)matching->
 A.nlook k(A.c_ty(fold_left(fun C q=>A.set_ty C(fst q)(snd q))matching C))=A.nlook k(A.c_ty C).
Proof.
 intro matching;induction matching as[|[p y]matching IH];intros C k Outside;[reflexivity|].
 apply Forall_cons_iff in Outside as[Head Tail];cbn[fold_left fst snd];rewrite(IH _ _ Tail).
 cbn[fst]in Head;cbn[A.set_ty A.c_ty A.nlook];destruct(Nat.eqb_spec k p);[congruence|reflexivity].
Qed.
Theorem family_matching_target_outside:forall marks matching C,
 Forall(fun q=>In(fst q)(map fst marks))matching->
 (forall k y,A.nlook k(A.c_ty C)=Some y->A.canon C y=y)->
 forall k,A.nlook k(family_marker_types marks)=None->
 A.nlook k(smap(A.c_sub C)(A.c_ty(fold_left(fun C q=>A.set_ty C(fst q)(snd q))matching C)))=
 A.nlook k(A.c_ty C).
Proof.
 intros marks matching C Domain Stable k Absent.
 pose proof(family_nlook_none_keys _ k _ Absent)as NotMarked;rewrite family_marker_types_keys in NotMarked.
 assert(NotKey:~In k(map fst marks))by(intro H;apply NotMarked;apply(proj1(in_rev (map fst marks)k));exact H).
 rewrite nlook_smap,family_matching_outside.
 - destruct(A.nlook k(A.c_ty C))as[y|]eqn:Read;cbn[option_map];[|reflexivity].
   change(Some(A.canon C y)=Some y);f_equal;exact(Stable k y Read).
 - rewrite Forall_forall in Domain|-*;intros[p y]Hin Eq;cbn[fst]in Eq;subst p.
   exact(NotKey(Domain(k,y)Hin)).
Qed.
Lemma family_marker_cx_types:forall marks C,
 A.c_ty(fold_left(fun C q=>A.set_ty C(fst q)(TAbs(snd q)))marks C)=family_marker_types marks++A.c_ty C.
Proof.
 intro marks;induction marks as[|[p g]marks IH];intro C;[reflexivity|].
 cbn[fold_left fst snd];rewrite IH;unfold family_marker_types;cbn[rev].
 rewrite map_app;cbn[map fst snd A.set_ty A.c_ty];rewrite <-app_assoc;reflexivity.
Qed.
Lemma family_marker_subst_back:forall marks table,
 NoDup(map snd marks)->forall g p,
 nassoc g(map(fun q=>(snd q,fst q))marks)=Some p->
 lookup_subst g(family_marker_subst marks table)=
 Some(match A.nlook p table with Some y=>y|None=>TAbs g end).
Proof.
 intros marks table Nd g p Read;apply pmatch_lookup_member in Read.
 apply in_map_iff in Read as([p0 g0]&Eq&Hin);cbn[fst snd]in Eq;injection Eq as <- <-.
 rewrite family_lookup_subst_nlook;apply family_marker_subst_lookup;assumption.
Qed.
From NM Require Import CoreMoss.
Lemma family_marker_subst_other:forall marks table g,
 nassoc g(map(fun q=>(snd q,fst q))marks)=None->lookup_subst g(family_marker_subst marks table)=None.
Proof.
 intros marks table g Read;apply lookup_subst_none;intro Key.
 unfold family_marker_subst in Key;rewrite map_map in Key.
 apply(family_nlook_none_keys nat g(map(fun q=>(snd q,fst q))marks)).
 - rewrite family_nlook_nassoc;exact Read.
 - rewrite map_map;exact Key.
Qed.
