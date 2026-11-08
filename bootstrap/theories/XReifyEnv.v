(* Transport along checked complete maps for first-order provision slots. *)
From Stdlib Require Import List Bool.
From NM Require Import ExecutionModel CoreMoss XTyping XEnv XRuntimeEnv ReifyMaps.
Import ListNotations.
Set Implicit Arguments.
Section Transport.
Variables (Sg:gsig)(Rp:nsym->option rep)(P:xprog)(Fb:usym->option(tele*tele*subst))(Hst:sty).
Definition first_order_map := full_item_map Sg (fun _ _ _ _ => False).
Lemma first_order_entry_transport : forall gt gs i j en,
 first_order_map gt gs i j -> xdentry_agree Sg Rp P Fb Hst gs j en ->
 xdentry_agree Sg Rp P Fb Hst gt i en.
Proof.
 intros gt gs i j en M H; inversion M; subst.
 - destruct en;cbn[xdentry_agree]in H|-*;tauto.
 - destruct en;cbn[xdentry_agree]in H|-*;try contradiction.
   rewrite H0;rewrite H1 in H;rewrite H2;exact H.
 - unfold grounded_call_signature in H2;injection H2 as Ep Er.
   destruct en;cbn[xdentry_agree]in H|-*;try contradiction;
   rewrite H0;rewrite H1 in H;rewrite Ep,Er;try exact H.
 - unfold method_call_signature in H2;injection H2 as Erecv Eparams Eret.
   destruct en;cbn[xdentry_agree]in H|-*;try contradiction;
   rewrite H0;rewrite H1 in H;rewrite Erecv,Eparams,Eret;exact H.
 - contradiction.
 - destruct en;cbn[xdentry_agree]in H;try contradiction;rewrite H3 in H;contradiction.
Qed.

Lemma first_order_type_shape : forall gt gs i j,
 first_order_map gt gs i j -> is_ty_item i=is_ty_item j.
Proof. intros gt gs i j M;inversion M;reflexivity. Qed.

Lemma lookup_xdenv_keys : forall d i, In i(map fst d) ->exists en,lookup_xdenv i d=Some en.
Proof.
 induction d as[|[j en]d IH];intros i Hin;cbn in Hin;[contradiction|].
 cbn[lookup_xdenv];destruct(item_eqb i j)eqn:E;[eauto|].
 apply IH;destruct Hin as[<-|Hin];[rewrite(proj2(item_eqb_eq j j)eq_refl)in E;discriminate|exact Hin].
Qed.

Lemma scope_entry_exists : forall g A d,
 xreify P=true ->xdenv_agree Sg Rp P Fb Hst g A d ->
 forall i,In i A ->exists en,lookup_xdenv i d=Some en.
Proof.
 intros g A d Reify [[Cover[Entries Deps]]Types]i Hin.
 destruct i as[t|v|f|r m th|u];try(exact(proj1(Forall_forall _ _)Cover _ Hin)).
 - destruct(Types Reify t Hin)as[tau[Lookup Eq]];eauto.
 - destruct(proj1(Forall_forall _ _)Cover(IVal v)Hin)as[E|H];[discriminate E|exact H].
 - destruct(proj1(Forall_forall _ _)Cover(IFn f)Hin)as[E|H];[discriminate E|exact H].
 - destruct(proj1(Forall_forall _ _)Cover(IMth r m th)Hin)as[E|H];[discriminate E|exact H].
 - destruct(proj1(Forall_forall _ _)Cover(IFun u)Hin)as[E|H];[discriminate E|exact H].
Qed.

Theorem first_order_map_composes : forall gt gs target source sm d,
 xreify P=true ->xdenv_agree Sg Rp P Fb Hst gs source d ->
 full_map_ok Sg(fun _ _ _ _=>False)gt gs target source sm ->
 Forall(fun i=>incl(dep_tys Sg i++item_ty_frees i++method_item_frees Sg i)(tyreqs target))target ->
 exists dr,xcompose d sm=Some dr /\ map fst dr=target /\
 xdenv_strong Sg Rp P Fb Hst gt target dr /\ types_match gt target dr.
Proof.
 intros gt gs target source sm d Reify Agree [Dom Maps]Closed.
 assert(Avail:Forall(fun p=>exists en,lookup_xdenv(snd p)d=Some en)sm).
 { apply Forall_forall;intros p InP;apply(scope_entry_exists Reify Agree).
   exact(proj1(proj1(Forall_forall _ _)Maps p InP)). }
 destruct(@xcompose_defined d sm Avail)as[dr Compose].
 assert(Keys:map fst dr=target)by(rewrite(@xcompose_keys d sm dr Compose);exact Dom).
 assert(Entries:Forall(fun p=>xdentry_agree Sg Rp P Fb Hst gt(fst p)(snd p))dr).
 { apply Forall_forall;intros[i en]InP.
   destruct(@xcompose_entry d sm dr(i,en)Compose InP)as[j[InM Lookup]].
   cbn[fst snd]in InM,Lookup|-*.
   destruct(proj1(Forall_forall _ _)Maps(i,j)InM)as[InJ Rel].
   cbn[fst snd]in InJ,Rel.
   eapply(@first_order_entry_transport gt gs i j en);[exact Rel|].
   destruct Agree as[[Cover[Typed Deps]]Types].
   destruct(is_ty_item j)eqn:Ty;[|exact(Typed j en InJ Ty Lookup)].
   destruct j;try discriminate Ty.
   destruct(Types Reify t InJ)as[tau[LookupT Eq]];rewrite Lookup in LookupT;injection LookupT as EqEn;subst en;exact I. }
 assert(Types:types_match gt target dr).
 { intros t InT.
   destruct(lookup_xdenv_keys dr(ITy t)ltac:(rewrite Keys;exact InT))as[en Lookup].
   destruct(@xcompose_entry d sm dr(ITy t,en)Compose(@lookup_xdenv_in (ITy t) dr en Lookup))as[j[InM LookJ]].
   cbn[fst snd]in InM,LookJ.
   destruct(proj1(Forall_forall _ _)Maps(ITy t,j)InM)as[InJ Rel].
   cbn[fst snd]in InJ,Rel.
   inversion Rel;subst.
   destruct(proj2 Agree Reify s InJ)as[tau[LookupS Eq]].
   rewrite LookJ in LookupS;injection LookupS as EqEn;subst en.
   exists tau;split;[exact Lookup|rewrite Eq;congruence]. }
 exists dr;split;[exact Compose|]. split;[exact Keys|]. split;[|exact Types].
 split.
 - apply Forall_forall;intros i Hi;right;apply lookup_xdenv_keys;rewrite Keys;exact Hi.
 - split;[exact Entries|]. split;[exact Closed|].
   apply Forall_forall;intros[i en]InP;cbn[fst snd].
   apply(proj1(Forall_forall _ _)Closed i).
   rewrite <-Keys;apply in_map with(f:=fst)in InP;exact InP.
Qed.
End Transport.
