(* Source type judgments depend only on their relevant free type meanings. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit Sx BodyTy.
Import ListNotations.
Module A := Audit.Elaboration.
Set Implicit Arguments.
Section Restrict.
Variable W : A.world.
Variable Closed : A.RelevanceClosed W (A.w_reqs W).

Lemma nlook_in_defined : forall X x (l:list(nat*X)) y,
 In(x,y) l -> A.nlook x l<>None.
Proof.
 intros X x l; induction l as [|[k v] l IH]; intros y E; [contradiction|].
 cbn [A.nlook]. destruct (Nat.eqb x k) eqn:Ek; [discriminate|].
 destruct E as [E|E]; [injection E as -> ->; rewrite Nat.eqb_refl in Ek; discriminate|].
 exact (IH y E).
Qed.

Lemma bracket_missing : forall p tm g a bs,
 A.Brackets W p tm g a bs -> forall x, A.nlook x bs=None ->
 ~exists b, In b (match a with Some l=>l|None=>[] end) /\ A.resolve_sym W p (fst b)=Some x.
Proof.
 intros p tm g a bs B x Ex [b [Hb Er]]. destruct B as [p tm g|p tm g args l F]; [contradiction|].
 cbn in Hb. assert (Gen : forall args l, Forall2 (fun b tb =>
 exists sp sa, snd b=Spec(Some sp)None sa /\
 A.resolve_sym W p (fst b)=Some(fst tb) /\ A.kind_sym W(fst tb)=Some A.DKType /\
 (A.kind_sym W g<>Some A.DKMeth -> In(fst tb)(A.w_reqs W g)) /\
 A.TyOf W p tm None (ATRef sp sa)(snd tb)) args l ->
 In b args -> exists tb, In tb l /\ fst tb=x).
 { intros args0 l0 H. induction H as [|b0 tb args0 l0 H0 H IH]; intro Hin; [contradiction|].
   destruct Hin as [<-|Hin].
   - destruct H0 as (sp & sa & E1 & E2 & _). rewrite Er in E2. injection E2 as Ex2.
     exists tb; split; [left; reflexivity|symmetry; exact Ex2].
   - destruct (IH Hin) as [q [Hq Eq]]. exists q; split; [right; exact Hq|exact Eq]. }
 destruct (Gen args l F Hb) as [[k y] [Hq Eq]]. cbn in Eq. subst k.
 apply (nlook_in_defined x (rev l) y); [rewrite <- in_rev; exact Hq|exact Ex].
Qed.

Fixpoint tyof_restrict p tm th t tau (H:A.TyOf W p tm th t tau) {struct H} :
 forall tm', (forall x, A.FreeIn W (A.w_reqs W) p t x -> A.nlook x tm'=A.nlook x tm) ->
 A.TyOf W p tm' th t tau
with brackets_restrict p tm g a bs (H:A.Brackets W p tm g a bs) {struct H} :
 forall tm',
 (forall bp sp sa, In(bp,Spec(Some sp)None sa)(match a with Some l=>l|None=>[] end) ->
  forall x, A.FreeIn W (A.w_reqs W) p (ATRef sp sa) x -> A.nlook x tm'=A.nlook x tm) ->
 A.Brackets W p tm' g a bs.
Proof.
 - destruct H as [p tm th|p tm th|p tm t|p tm th fs ts F|p tm th ms ts flat F Ef Fm Nd
   |p tm th pa t tau Hr Hk Hl|p tm th pa a g k bs args Hr Hk Hkk B F
   |p tm th pa a g d n body bs tau Hr Hd Ha B T]; intros tm' Eq.
   + constructor.
   + constructor.
   + constructor.
   + constructor.
     assert (Gen : forall fs' ts', incl fs' fs ->
       Forall2(fun nt ft=>fst ft=A.w_fld W(fst nt) /\ A.TyOf W p tm th(snd nt)(snd ft))fs' ts' ->
       Forall2(fun nt ft=>fst ft=A.w_fld W(fst nt) /\ A.TyOf W p tm' th(snd nt)(snd ft))fs' ts').
     { intros fs' ts' Hin F0. induction F0 as [|[nn tt] ft fs0 ts0 [Ek T0] F0 IH]; constructor.
       - split; [exact Ek|]. apply (tyof_restrict _ _ _ _ _ T0 tm').
         intros x Fx. apply Eq. eapply A.FRec; [apply Hin; left; reflexivity|exact Fx].
       - apply IH. intros z Hz. apply Hin. right; exact Hz. }
     exact (Gen fs ts (incl_refl _) F).
   + eapply A.TyUnion; [|exact Ef|exact Fm|exact Nd].
     assert (Gen : forall ms' ts', incl ms' ms -> Forall2(A.TyOf W p tm th)ms' ts' ->
       Forall2(A.TyOf W p tm' th)ms' ts').
     { intros ms' ts' Hin F0. induction F0 as [|m ty ms0 ts0 T0 F0 IH]; constructor.
       - apply (tyof_restrict _ _ _ _ _ T0 tm'). intros x Fx. apply Eq.
         eapply A.FUnion; [apply Hin; left; reflexivity|exact Fx].
       - apply IH. intros z Hz. apply Hin. right; exact Hz. }
     exact (Gen ms ts (incl_refl _) F).
   + eapply A.TyType; [exact Hr|exact Hk|]. rewrite Eq; [exact Hl|].
     eapply A.FType; [exact Hr|exact Hk|]. intros [b [E _]]; exact E.
   + eapply A.TyNom; [exact Hr|exact Hk|exact Hkk| |].
     * apply (brackets_restrict _ _ _ _ _ B tm'). intros bp sp sa Hin x Fx. apply Eq.
       eapply A.FBracket; [rewrite Hr; discriminate|exact Hin|exact Fx].
     * assert (Enot : A.kind_sym W g<>Some A.DKType).
       { rewrite Hk. destruct Hkk as [Hk1|Hk1]; subst k; discriminate. }
       assert (Gen : forall rs qs, incl rs (A.w_reqs W g) ->
         Forall2(fun r q=>fst q=r /\ A.nlook r(bs++tm)=Some(snd q)) rs qs ->
         Forall2(fun r q=>fst q=r /\ A.nlook r(bs++tm')=Some(snd q)) rs qs).
       { intros rs qs Hin F0. induction F0 as [|r q rs qs [Ek El] F0 IH]; constructor.
         - split; [exact Ek|]. rewrite BodyTy.nlook_app. rewrite BodyTy.nlook_app in El.
           destruct(A.nlook r bs) as [y|] eqn:Ebs; [exact El|].
           rewrite Eq; [exact El|]. eapply A.FNom; [exact Hr|exact Enot|apply Hin; left; reflexivity|].
           exact (bracket_missing B Ebs).
         - apply IH. intros z Hz. apply Hin. right; exact Hz. }
       exact (Gen _ _ (incl_refl _) F).
   + eapply A.TyAlias; [exact Hr|exact Hd|exact Ha| |].
     * apply (brackets_restrict _ _ _ _ _ B tm'). intros bp sp sa Hin x Fx. apply Eq.
       eapply A.FBracket; [rewrite Hr; discriminate|exact Hin|exact Fx].
     * apply (tyof_restrict _ _ _ _ _ T (bs++tm')). intros x Fx.
       rewrite !BodyTy.nlook_app. destruct(A.nlook x bs) eqn:Eb; [reflexivity|].
       apply Eq. eapply A.FNom; [exact Hr| | |exact(bracket_missing B Eb)].
       -- unfold A.kind_sym. rewrite Hd,Ha. discriminate.
       -- exact (proj2(@Closed g d [] body Hd ltac:(rewrite Ha; reflexivity)) x Fx).
 - destruct H as [p tm g|p tm g args l F]; intros tm' Eq; [constructor|].
   constructor. assert (Gen : forall args' l', incl args' args ->
    Forall2(fun b tb=>exists sp sa,snd b=Spec(Some sp)None sa /\
     A.resolve_sym W p(fst b)=Some(fst tb) /\ A.kind_sym W(fst tb)=Some A.DKType /\
     (A.kind_sym W g<>Some A.DKMeth -> In(fst tb)(A.w_reqs W g)) /\
     A.TyOf W p tm None(ATRef sp sa)(snd tb)) args' l' ->
    Forall2(fun b tb=>exists sp sa,snd b=Spec(Some sp)None sa /\
     A.resolve_sym W p(fst b)=Some(fst tb) /\ A.kind_sym W(fst tb)=Some A.DKType /\
     (A.kind_sym W g<>Some A.DKMeth -> In(fst tb)(A.w_reqs W g)) /\
     A.TyOf W p tm' None(ATRef sp sa)(snd tb)) args' l').
   { intros args' l' Hin F0. induction F0 as [|[bp spc] tb args0 l0 H0 F0 IH]; constructor.
     - destruct H0 as (sp & sa & E1 & E2 & E3 & E4 & T). cbn in E1. subst spc.
       exists sp,sa. repeat split; try assumption.
       apply (tyof_restrict _ _ _ _ _ T tm'). intros x Fx.
       exact (Eq bp sp sa (Hin _ (or_introl eq_refl)) x Fx).
     - apply IH. intros z Hz. apply Hin. right; exact Hz. }
   exact (Gen args l (incl_refl _) F).
Qed.
End Restrict.
