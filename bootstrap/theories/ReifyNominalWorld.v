(* Static completion of lazy nominal information. The source world's real
   requirement telescope can describe a carried but unobserved head even
   when its payload was never elaborated. This introduces no payload. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import CoreMoss ReifySourceTypeFormation ReifyFormation.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition complete_nominal_signature(W:A.world)(base:gsig):gsig:=
 mkGsigInterfaces(g_ty base)(g_tag base)(g_val base)(g_fn base)(g_mth base)(g_fun base)
   (g_fixed base)(g_poly base)(g_cap base)
   (fun n=>if Nat.eqb n rec_nom then None else
     match g_tag base n with
     | Some _=>None
     | None=>match g_opaque base n with
       | Some D=>Some D
       | None=>match A.kind_sym W n with
         | Some A.DKTag|Some A.DKUnit=>Some(map ITy(A.w_reqs W n))
         | _=>None end
       end
     end) (g_opaque_fun base).
Lemma nominal_completion_payload_unchanged : forall W base n,
 g_tag(complete_nominal_signature W base)n=g_tag base n.
Proof. reflexivity. Qed.
Lemma nominal_completion_no_payload : forall W base n D,
 g_opaque(complete_nominal_signature W base)n=Some D->g_tag base n=None.
Proof.
 intros W base n D Read;cbn in Read;destruct(Nat.eqb n rec_nom);[discriminate|].
 destruct(g_tag base n);[discriminate|reflexivity].
Qed.
Lemma nominal_completion_record_opaque_unclaimed : forall W base,
 g_opaque(complete_nominal_signature W base)rec_nom=None.
Proof. reflexivity. Qed.
Lemma nominal_completion_preserves_known_opaque : forall W base n D,
 n<>rec_nom->g_tag base n=None->g_opaque base n=Some D->
 g_opaque(complete_nominal_signature W base)n=Some D.
Proof.
 intros W base n D Fresh Tag Opaque;cbn.
 apply Nat.eqb_neq in Fresh;rewrite Fresh,Tag,Opaque;reflexivity.
Qed.
Theorem nominal_completion_schemas : forall W base,
 (forall n k,A.kind_sym W n=Some k->(k=A.DKTag\/k=A.DKUnit)->n<>rec_nom)->
 (forall n k D payload,A.kind_sym W n=Some k->(k=A.DKTag\/k=A.DKUnit)->
    g_tag base n=Some(D,payload)->D=map ITy(A.w_reqs W n))->
 (forall n k D,A.kind_sym W n=Some k->(k=A.DKTag\/k=A.DKUnit)->
    g_opaque base n=Some D->D=map ITy(A.w_reqs W n))->
 source_nominal_schemas W(complete_nominal_signature W base).
Proof.
 intros W base Positive Known KnownOpaque n k Kind Nominal.
 destruct(g_tag base n)as[[D payload]|]eqn:Payload.
 - left;exists payload;cbn;rewrite Payload;f_equal;f_equal.
   eapply Known;eauto.
 - right;split;[exact Payload|].
   cbn;assert(NotRecord:Nat.eqb n rec_nom=false)by(apply Nat.eqb_neq;eapply Positive;eauto).
   rewrite NotRecord,Payload;destruct(g_opaque base n)as[D|]eqn:Opaque.
   + f_equal;eapply KnownOpaque;eauto.
   + rewrite Kind;destruct Nominal as[Same|Same];subst k;reflexivity.
Qed.
Lemma nominal_completion_opaque_origin : forall W base n D,
 g_opaque(complete_nominal_signature W base)n=Some D->
 (exists k,A.kind_sym W n=Some k/\(k=A.DKTag\/k=A.DKUnit)/\D=map ITy(A.w_reqs W n))\/
 g_opaque base n=Some D.
Proof.
 intros W base n D Read;cbn in Read;destruct(Nat.eqb n rec_nom);[discriminate|].
 destruct(g_tag base n);[discriminate|].
 destruct(g_opaque base n)as[Known|]eqn:Opaque.
 - inversion Read;subst;right;reflexivity.
 - destruct(A.kind_sym W n)as[k|]eqn:Kind;[|discriminate].
   destruct k;try discriminate.
   all:(inversion Read;subst;left;eexists;split;[reflexivity|];split;[auto|reflexivity]).
Qed.
Theorem nominal_completion_opaque_formed : forall W base,
 (forall n,NoDup(A.w_reqs W n))->
 (forall n k,A.kind_sym W n=Some k->(k=A.DKTag\/k=A.DKUnit)->
   Forall(fun t=>g_ty base t=true/\t<>this_sym)(A.w_reqs W n))->
 (forall n D,g_opaque base n=Some D->exists types,D=map ITy types/\
   NoDup types/\Forall(fun t=>g_ty base t=true/\t<>this_sym)types)->
 forall n D,g_opaque(complete_nominal_signature W base)n=Some D->
 exists Phi,wf_tele(complete_nominal_signature W base)empty_ctx D Phi.
Proof.
 intros W base Unique WorldTypes BaseTypes n D Read.
 destruct(nominal_completion_opaque_origin _ _ _ _ Read)as[[k[Kind[Nominal ->]]]|Existing].
 - eexists;apply formed_type_telescope;[apply Unique|].
   exact(WorldTypes n k Kind Nominal).
 - destruct(BaseTypes _ _ Existing)as[types[-> [Distinct Declared]]].
   eexists;apply formed_type_telescope;[exact Distinct|exact Declared].
Qed.
