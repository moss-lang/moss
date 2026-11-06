(* Nominal support of semantic type readings and their source translations. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab BodyTr NominalSupport BodyTy.
Import ListNotations.
Module A := Audit.Elaboration.
Set Implicit Arguments.
Inductive TySupport (W:A.world) : ty -> Prop :=
| TSUnit : TySupport W TUnit
| TSAbs : forall x, TySupport W(TAbs x)
| TSNom : forall n xs,
 (A.kind_sym W n=Some A.DKUnit \/ A.kind_sym W n=Some A.DKTag) ->
 map fst xs=A.w_reqs W n -> Forall(fun p=>TySupport W(snd p))xs ->
 TySupport W(TNom n xs)
| TSRecord : forall fs, Forall(fun p=>TySupport W(snd p))fs -> TySupport W(TNom A.rec_head fs)
| TSUnion : forall ms, Forall(TySupport W)ms -> TySupport W(TUnion ms).

Lemma tysupport_members : forall W t, TySupport W t -> Forall(TySupport W)(A.members t).
Proof. intros W t T; destruct t; cbn[A.members]; try (constructor; [exact T|constructor]). inversion T; assumption. Qed.

Section Reading.
Variable W:A.world.
Definition TmSupport(tm:list(nat*ty)) := forall x t,A.nlook x tm=Some t -> TySupport W t.
Lemma tmsupport_app : forall l tm, Forall(fun p=>TySupport W(snd p))l -> TmSupport tm -> TmSupport(l++tm).
Proof.
 intros l tm L T x t. rewrite BodyTy.nlook_app.
 destruct(A.nlook x l) as [y|]eqn:E; [intro H; injection H as <-|exact(T x t)].
 clear -L E. induction l as [|[k z]l IH]; cbn[A.nlook]in E; [discriminate|].
 inversion L; subst. destruct(Nat.eqb x k); [injection E as <-; assumption|apply IH; assumption].
Qed.

Fixpoint tyof_support p tm th t tau(H:A.TyOf W p tm th t tau){struct H}:
 TmSupport tm -> (match th with Some y=>TySupport W y|None=>True end) -> TySupport W tau
with brackets_support p tm g a bs(H:A.Brackets W p tm g a bs){struct H}:
 TmSupport tm -> Forall(fun p=>TySupport W(snd p))bs.
Proof.
 - destruct H as [p tm th|p tm th|p tm t|p tm th fs ts F|p tm th ms ts flat F Ef Fm Nd
   |p tm th pa t tau Hr Hk Hl|p tm th pa a g k bs args Hr Hk Hkk B F
   |p tm th pa a g d n body bs tau Hr Hd Ha B T]; intros TM TH.
   + apply TSUnion; constructor.
   + constructor.
   + exact TH.
   + apply TSRecord. induction F as [|nt ft fs ts [Ek T] F IH]; constructor; [|exact IH].
     exact(tyof_support _ _ _ _ _ T TM TH).
   + apply TSUnion. subst flat. apply Forall_flat_map. clear Fm Nd.
     induction F as [|m ty ms ts T F IH]; constructor; [|exact IH].
     apply tysupport_members. exact(tyof_support _ _ _ _ _ T TM TH).
   + exact(TM _ _ Hl).
   + assert (TB : TmSupport(bs++tm)) by (apply tmsupport_app; [exact(brackets_support _ _ _ _ _ B TM)|exact TM]).
     apply TSNom.
     * destruct Hkk as [E|E]; subst k; auto.
     * clear -F. induction F as [|r q rs qs [Ek El]F IH]; cbn; [reflexivity|]. f_equal; assumption.
     * clear -F TB. induction F as [|r q rs qs [Ek El]F IH]; constructor; [exact(TB _ _ El)|exact IH].
   + apply(tyof_support _ _ _ _ _ T); [apply tmsupport_app; [exact(brackets_support _ _ _ _ _ B TM)|exact TM]|exact I].
 - destruct H as [p tm g|p tm g args l F]; intro TM; [constructor|].
   apply Forall_rev. induction F as [|b tb args l (sp & sa & E1 & E2 & E3 & E4 & T)F IH]; constructor; [|exact IH].
   exact(tyof_support _ _ _ _ _ T TM I).
Qed.
End Reading.

Lemma support_tr : forall W Fl t, Support W t -> TySupport W(tr Fl t).
Proof.
 intros W Fl t. induction t as [| |x|n xs IH|ms IH|fs IH]using ety_ind'; intro H;
   cbn[tr].
 - constructor.
 - apply TSUnion; constructor.
 - constructor.
 - inversion H; subst. apply TSNom.
   + assumption.
   + rewrite map_map. change(map fst xs=A.w_reqs W n). assumption.
   + apply Forall_map. rewrite Forall_forall in *. intros p Hp. apply IH; auto.
 - inversion H; subst. apply TSUnion. apply Forall_map. rewrite Forall_forall in *. intros p Hp. cbn [snd]. apply IH; auto.
 - inversion H; subst. apply TSRecord. apply Forall_map. rewrite Forall_forall in *. intros p Hp. cbn [snd]. apply IH; auto.
Qed.

Lemma support_of_tr : forall W Fl t,
 ety_ok t=true -> TySupport W(tr Fl t) -> Support W t.
Proof.
 intros W Fl t. induction t as [| |x|n xs IH|ms IH|fs IH]using ety_ind'; intros C T.
 - constructor.
 - constructor.
 - constructor.
 - cbn[ety_ok]in C. apply andb_true_iff in C as [Cn Cxs].
   cbn[tr]in T. inversion T as [| |n0 ys K Keys Supp|ys Supp|]; subst.
   + apply SupportNom; [exact K| |].
     * rewrite map_map in Keys. exact Keys.
     * rewrite Forall_forall in IH,Supp|-*. intros p Hp.
       apply IH; [exact Hp| |].
       -- exact(proj1(forallb_forall _ _) Cxs p Hp).
       -- apply(Supp (fst p,tr Fl(snd p))). apply in_map_iff. exists p; auto.
   + cbn[A.rec_head]in Cn. discriminate.
 - apply SupportUnion. cbn[tr]in T. inversion T as [| | | |ms0 Supp]; subst.
   rewrite Forall_map in Supp. rewrite Forall_forall in IH,Supp|-*.
   intros y Hy. apply IH; [exact Hy| |exact(Supp y Hy)].
   cbn[ety_ok]in C. destruct ms; [contradiction|]. pose proof(proj1(forallb_forall _ _) C y Hy)as Cy. destruct y; try discriminate Cy; exact Cy.
 - assert (Gen : Forall(fun p=>TySupport W(snd p))(map(fun p=>(Fl(fst p),tr Fl(snd p)))fs) -> Support W(YRec fs)).
   { intro Supp. apply SupportRec. rewrite Forall_map in Supp.
     rewrite Forall_forall in IH,Supp|-*. intros p Hp. apply IH; [exact Hp| |].
     - cbn[ety_ok]in C. exact(proj1(forallb_forall _ _) C p Hp).
     - exact(Supp p Hp). }
   cbn[tr]in T. inversion T; subst; apply Gen; assumption.
Qed.

Lemma support_clean : forall W t,
 A.kind_sym W A.rec_head=None -> Support W t -> WFty t -> ety_ok t=true.
Proof.
 intros W t. induction t as [| |x|n xs IH|ms IH|fs IH]using ety_ind'; intros Rec S Wt;
   cbn[ety_ok]; try reflexivity.
 - inversion S as [| | |n0 xs0 Kind Keys Supp| |]; subst. apply andb_true_iff; split.
   + apply negb_true_iff,Nat.eqb_neq. intro E; subst n. change (A.kind_sym W 0=None)in Rec. rewrite Rec in Kind. destruct Kind; discriminate.
   + apply forallb_forall. intros p Hp. rewrite Forall_forall in IH,Supp.
     apply IH; [exact Hp|exact Rec|exact(Supp p Hp)|].
     unfold WFty in Wt. cbn[ety_wf]in Wt. exact(proj1(forallb_forall _ _) Wt p Hp).
 - destruct ms as [|y ys]; [discriminate Wt|]. apply forallb_forall. intros y0 Hy.
   inversion S as [| | | |ms0 Supp|]; subst. rewrite Forall_forall in IH,Supp.
   unfold WFty in Wt. cbn[ety_wf]in Wt. pose proof(proj1(forallb_forall _ _) Wt y0 Hy)as Wy.
   destruct y0; try discriminate Wy. apply IH; [exact Hy|exact Rec|exact(Supp _ Hy)|exact Wy].
 - apply forallb_forall. intros p Hp. inversion S as [| | | | |fs0 Supp]; subst. rewrite Forall_forall in IH,Supp.
   apply IH; [exact Hp|exact Rec|exact(Supp p Hp)|].
   unfold WFty in Wt. cbn[ety_wf]in Wt. exact(proj1(forallb_forall _ _) Wt p Hp).
Qed.
