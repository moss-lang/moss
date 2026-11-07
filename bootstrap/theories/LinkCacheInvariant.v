(* Physical provenance of linked functor code. Entries retain actual creator
   executions; their source meaning is proved separately, not assumed here. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore ReqsSolve ReqsStable ElabKeeps InfoInvariant InfoPreserve FwdInvariant.
Import ListNotations.
Definition link_lookup (s : st) (target provider : nat) : option nat :=
 assoc (fun a b => Nat.eqb (fst a) (fst b) && Nat.eqb (snd a) (snd b))
   (target,provider) (st_links s).

(* Local-variable save/restore preserves this relation; full Fr also
   supplies it. Functor rows remain visible for the final-program bridge. *)
Record LinkFrame (s final : st) : Prop := {
 link_frame_source : FrL s final;
 link_frame_functors : forall row, In row (st_funs s) -> In row (st_funs final)
}.
Lemma link_frame_refl : forall s,LinkFrame s s.
Proof. intro s;constructor;[apply frl_of_fr;apply fr_refl|auto]. Qed.
Lemma link_frame_trans : forall a b c,LinkFrame a b -> LinkFrame b c -> LinkFrame a c.
Proof.
 intros a b c [AB RowsAB] [BC RowsBC];constructor;[eapply frl_trans;eassumption|auto].
Qed.
Lemma link_frame_of_fr : forall s final,Fr s final -> LinkFrame s final.
Proof.
 intros s final Frame;constructor;[apply frl_of_fr;exact Frame|].
 intros row Member;destruct(fx_funs _ _ (fr_x _ _ Frame))as[prefix Rows].
 rewrite Rows;apply in_or_app;right;exact Member.
Qed.
Lemma link_frame_vars : forall s vars count,LinkFrame s (s_vars vars count s).
Proof.
 intros s vars count;constructor.
 - constructor;cbn;auto;[exists[];now rewrite app_nil_r|apply ext_refl].
 - auto.
Qed.

Inductive LinkEntry (s : st) : nat -> nat -> nat -> Prop :=
| SourceLinkEntry : forall target provider code fuel fg bg before after,
    gs_id fg = target -> gs_id bg = provider ->
    Inv before -> InfoInv before -> FwdInv before ->
    nget (st_syms before) target = Some fg -> nget (st_syms before) provider = Some bg ->
    link_lookup before target provider = None ->
    link_body (S fuel) fg bg before = Good (code,after) ->
    LinkFrame after s ->
    LinkEntry s target provider code
| PlatformLinkEntry : forall target code fg before after,
    gs_id fg = target ->
    Inv before -> InfoInv before -> FwdInv before ->
    nget (st_syms before) target = Some fg ->
    link_lookup before target target = None ->
    platform_fun fg before = Good (code,after) ->
    LinkFrame after s ->
    LinkEntry s target target code.

Definition LinkInv (s : st) : Prop := forall target provider code,
 link_lookup s target provider = Some code -> LinkEntry s target provider code.

Lemma link_entry_keep : forall s final target provider code,
 LinkEntry s target provider code -> LinkFrame s final -> LinkEntry final target provider code.
Proof.
 intros s final target provider code Entry Frame;destruct Entry.
 - eapply SourceLinkEntry;try eassumption;eapply link_frame_trans;eassumption.
 - eapply PlatformLinkEntry;try eassumption;eapply link_frame_trans;eassumption.
Qed.
Lemma link_inv_init : forall w,LinkInv (s_init w).
Proof. intros w target provider code Hit;discriminate Hit. Qed.
Lemma source_link_cached_creator : forall s target provider code,
 LinkInv s -> link_lookup s target provider=Some code -> target<>provider ->
 exists fuel fg bg before after,
 gs_id fg=target /\ gs_id bg=provider /\
 Inv before /\ InfoInv before /\ FwdInv before /\
 nget(st_syms before)target=Some fg /\ nget(st_syms before)provider=Some bg /\
 link_lookup before target provider=None /\
 link_body(S fuel)fg bg before=Good(code,after) /\ LinkFrame after s.
Proof.
 intros s target provider code Linked Hit Different.
 destruct(Linked _ _ _ Hit).
 - do 5 eexists;repeat first[eassumption|split].
 - contradiction.
Qed.
Lemma link_inv_keep : forall s final,
 LinkInv s -> Fr s final -> st_links final = st_links s -> LinkInv final.
Proof.
 intros s final Linked Frame Same target provider code Hit.
 unfold link_lookup in Hit;rewrite Same in Hit.
 eapply link_entry_keep;[apply Linked;exact Hit|apply link_frame_of_fr;exact Frame].
Qed.
Lemma link_key_eqb_spec : forall a b,
 (Nat.eqb (fst a) (fst b) && Nat.eqb (snd a) (snd b)) = true <-> a = b.
Proof.
 intros [left_target left_provider] [right_target right_provider];cbn;rewrite andb_true_iff,!Nat.eqb_eq;split;[intros[-> ->];reflexivity|intro Eq;injection Eq;auto].
Qed.
Lemma link_lookup_install : forall s target provider code asked_target asked_provider,
 link_lookup (s_links (fun rows => ((target,provider),code)::rows) s) asked_target asked_provider =
 if Nat.eqb asked_target target && Nat.eqb asked_provider provider then Some code
 else link_lookup s asked_target asked_provider.
Proof. reflexivity. Qed.
Lemma link_inv_install : forall s target provider code,
 LinkInv s -> Fr s (s_links (fun rows => ((target,provider),code)::rows) s) ->
 LinkEntry (s_links (fun rows => ((target,provider),code)::rows) s) target provider code ->
 LinkInv (s_links (fun rows => ((target,provider),code)::rows) s).
Proof.
 intros s target provider code Linked Frame Created requested_target requested_provider result Hit.
 rewrite link_lookup_install in Hit.
 destruct (Nat.eqb requested_target target && Nat.eqb requested_provider provider) eqn:Same.
 - apply andb_true_iff in Same as [Target Provider];apply Nat.eqb_eq in Target,Provider;subst.
   injection Hit as <-;exact Created.
 - eapply link_entry_keep;[apply Linked;exact Hit|apply link_frame_of_fr;exact Frame].
Qed.
Lemma link_inv_write : forall s final target provider code,
 LinkInv s -> LinkFrame s final ->
 st_links final = ((target,provider),code)::st_links s ->
 LinkEntry final target provider code -> LinkInv final.
Proof.
 intros s final target provider code Linked Frame Rows Created requested_target requested_provider result Hit.
 unfold link_lookup in Hit;rewrite Rows in Hit.
 change((if Nat.eqb requested_target target && Nat.eqb requested_provider provider
 then Some code else link_lookup s requested_target requested_provider)=Some result)in Hit.
 destruct(Nat.eqb requested_target target && Nat.eqb requested_provider provider)eqn:Same.
 - apply andb_true_iff in Same as[Target Provider];apply Nat.eqb_eq in Target,Provider;subst.
   injection Hit as <-;exact Created.
 - eapply link_entry_keep;[apply Linked;exact Hit|exact Frame].
Qed.

Lemma source_link_entry : forall fuel fg bg before code after,
 Inv before -> InfoInv before -> FwdInv before ->
 nget (st_syms before) (gs_id fg) = Some fg -> nget (st_syms before) (gs_id bg) = Some bg ->
 link_lookup before (gs_id fg) (gs_id bg) = None ->
 link_body (S fuel) fg bg before = Good (code,after) ->
 LinkEntry after (gs_id fg) (gs_id bg) code.
Proof.
 intros;eapply SourceLinkEntry;try eassumption;try reflexivity;apply link_frame_refl.
Qed.

Lemma platform_link_entry : forall fg before code after,
 Inv before -> InfoInv before -> FwdInv before ->
 nget (st_syms before) (gs_id fg) = Some fg ->
 link_lookup before (gs_id fg) (gs_id fg) = None ->
 platform_fun fg before = Good (code,after) -> LinkEntry after (gs_id fg) (gs_id fg) code.
Proof. intros;eapply PlatformLinkEntry;try eassumption;try reflexivity;apply link_frame_refl. Qed.
Lemma link_inv_vars : forall vars count s,LinkInv s -> LinkInv(s_vars vars count s).
Proof.
 intros vars count s Linked target provider code Hit.
 eapply link_entry_keep;[apply Linked;exact Hit|apply link_frame_vars].
Qed.

(* The source traversal already preserves the information and forwarder
   invariants; reuse those results while retaining link creation history. *)
Definition okL {A}(m:M A):Prop:=
 okI m /\ okF m /\ forall s a final,
 Inv s -> InfoInv s -> FwdInv s -> LinkInv s -> m s=Good(a,final) -> LinkInv final.
Arguments okL:simpl never.
Lemma okL_keeps : forall A(m:M A),okI m -> okF m -> keeps st_links m -> okL m.
Proof.
 intros A m Info Forward Stable;split;[exact Info|];split;[exact Forward|].
 intros s a final Valid Cached Forwarded Linked Run.
 eapply link_inv_keep;[exact Linked|exact(proj2(proj1 Info _ _ _ Valid Run))|exact(Stable _ _ _ Run)].
Qed.
Lemma okL_ret : forall A(a:A),okL(ret a).
Proof. intros;apply okL_keeps;[apply okI_ret|apply okF_ret|apply keeps_ret]. Qed.
Lemma okL_bind : forall A B(m:M A)(k:A->M B),okL m -> (forall a,okL(k a)) -> okL(mbind m k).
Proof.
 intros A B m k [Im[Fm Lm]] K;split.
 - apply okI_bind;[exact Im|intro a;exact(proj1(K a))].
 - split.
   + apply okF_bind;[exact Fm|intro a;exact(proj1(proj2(K a)))].
   + intros s b final Valid Cached Forwarded Linked Run.
     apply bind_inv in Run as[a[mid[First Last]]].
     destruct(proj1 Im _ _ _ Valid First)as[Vm Frame].
     apply(proj2(proj2(K a))mid b final Vm (proj2 Im _ _ _ Valid Cached First)
       (proj2 Fm _ _ _ Valid Forwarded First) (Lm _ _ _ Valid Cached Forwarded Linked First)Last).
Qed.

Lemma okL_mmap : forall A B(f:A->M B)xs,
 (forall x,okL(f x)) -> okL(mmap f xs).
Proof.
 intros A B f xs Each;induction xs as[|x xs IH];cbn[mmap];[apply okL_ret|].
 apply okL_bind;[apply Each|intro;apply okL_bind;[exact IH|intro;apply okL_ret]].
Qed.
Lemma okL_mfold : forall A B(f:A->B->M A)xs acc,
 (forall a x,okL(f a x)) -> okL(mfold f acc xs).
Proof.
 intros A B f xs;induction xs as[|x xs IH];intros acc Each;cbn[mfold];[apply okL_ret|].
 apply okL_bind;[apply Each|intro;apply IH;exact Each].
Qed.
Lemma okL_miter : forall A(f:A->M unit)xs,
 (forall x,okL(f x)) -> okL(miter f xs).
Proof.
 intros A f xs Each;induction xs as[|x xs IH];cbn[miter];[apply okL_ret|].
 apply okL_bind;[apply Each|intro;exact IH].
Qed.
Lemma okL_with_vars : forall A init(m:M A),okL m -> okL(with_vars init m).
Proof.
 intros A init m[Info[Forward Linked]];split;[apply okI_with_vars;exact Info|].
 split;[apply okF_with_vars;exact Forward|].
 intros s a final Valid Cached Forwarded Links Run;unfold with_vars in Run.
 apply bind_inv in Run as[s0[s1[Get Run]]];injection Get as <- <-.
 apply bind_inv in Run as[u[s2[Store Run]]];unfold upd in Store;injection Store as <- <-.
 apply bind_inv in Run as[r[s3[Body Run]]].
 apply bind_inv in Run as[u'[s4[Restore Run]]];unfold upd in Restore;injection Restore as <- <-.
 injection Run as <- <-.
 assert(InnerValid:Inv(s_vars init(List.length init)s))by(destruct Valid;constructor;assumption).
 assert(InnerCached:InfoInv(s_vars init(List.length init)s)).
 { eapply info_step;[exact Cached|exact(link_frame_source _ _ (link_frame_vars s init(List.length init)))|reflexivity|reflexivity]. }
 apply link_inv_vars.
 eapply Linked;[exact InnerValid|exact InnerCached|exact Forwarded|apply link_inv_vars;exact Links|exact Body].
Qed.
