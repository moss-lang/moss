(* Global declaration formation reduces to the actual finite emitted rows.
   The source base contributes only nominal/type declarations. Runtime fixed
   interpretations remain separately checked by the R contracts. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import CoreMoss ReifyData ReifySig.
Import ListNotations.
Definition formed_reify_declaration(Sg:gsig)(entry:item*reify_decl):Prop:=
 match entry with
 | (ITy t,RDType)=>t<>this_sym
 | (IVal _,RDValue D tau)=>exists Phi,wf_tele Sg empty_ctx D Phi /\wf_ty Sg Phi tau
 | (IFn _,RDFunction D signature)=>exists Phi,wf_tele Sg empty_ctx D Phi /\
     Forall(wf_ty Sg Phi)(fs_params signature)/\wf_ty Sg Phi(fs_ret signature)
 | (IMth _ _ _,RDMethod D signature)=>exists Phi,wf_tele Sg empty_ctx D Phi /\
     Forall(wf_ty Sg(add_item(ITy this_sym)Phi))(fs_params signature)/\
     wf_ty Sg(add_item(ITy this_sym)Phi)(fs_ret signature)
 | (IFun _,RDOpaqueFunctor _)=>True
 | (IFun _,RDFunctor schema)=>exists Phi,wf_tele Sg empty_ctx(rfd_inputs schema)Phi /\
     wf_tele Sg Phi(rfd_outputs schema)(mkCtx(cA Phi++rfd_outputs schema)(cS Phi))
 | _=>False end.
Lemma reify_declaration_lookup_member : forall declarations key declaration,
 lookup_reify_decl key declarations=Some declaration->In(key,declaration)declarations.
Proof.
 induction declarations as[|[first value]rest IH];intros key declaration E;cbn in E;[discriminate|].
 destruct(item_eqb key first)eqn:Same.
 - apply item_eqb_eq in Same;subst;inversion E;left;reflexivity.
 - right;eapply IH;exact E.
Qed.
Lemma reify_method_lookup_member : forall declarations method inputs signature,
 lookup_reify_method method declarations=Some(inputs,signature)->
 exists receiver theta,In(IMth receiver method theta,RDMethod inputs signature)declarations.
Proof.
 induction declarations as[|[first value]rest IH];intros method inputs signature E;[discriminate|].
 destruct first as[t|v|f|receiver name theta|u];
   destruct value as[|Dv tau|Df Sf|Dm Sm|schema|original];cbn[lookup_reify_method]in E;
   try(destruct(IH _ _ _ E)as[r[th Member]];exists r,th;right;exact Member).
 destruct(Nat.eqb method name)eqn:Same.
 - apply Nat.eqb_eq in Same;subst;inversion E;subst;exists receiver,theta;left;reflexivity.
 - destruct(IH _ _ _ E)as[r[th Member]];exists r,th;right;exact Member.
Qed.

Theorem finite_reified_signature_formed : forall base declarations,
 (forall v,g_val base v=None)->(forall f,g_fn base f=None)->
 (forall m,g_mth base m=None)->(forall u,g_fun base u=None)->
 Forall(formed_reify_declaration(reify_signature_overlay base declarations))declarations->
 (forall n D payload,g_tag base n=Some(D,payload)->exists Phi,
   wf_tele(reify_signature_overlay base declarations)empty_ctx D Phi /\
   incl(ty_frees payload)(tyreqs(cA Phi)))->
 (forall n D,g_opaque base n=Some D->exists Phi,
   wf_tele(reify_signature_overlay base declarations)empty_ctx D Phi)->
 wf_gsig(reify_signature_overlay base declarations).
Proof.
 intros base declarations NoVal NoFn NoMth NoFun Formed Nominal Opaque.
 pose proof Formed as Rows;rewrite Forall_forall in Rows.
 constructor.
 - intros n D payload Tag;exact(Nominal n D payload Tag).
 - intros v D tau Read;cbn in Read.
   destruct(lookup_reify_decl(IVal v)declarations)as[d|]eqn:E;
     [destruct d|];try(rewrite NoVal in Read;discriminate).
   inversion Read;subst;exact(Rows _ (reify_declaration_lookup_member _ _ _ E)).
 - intros f D signature body Read;cbn in Read.
   destruct(lookup_reify_decl(IFn f)declarations)as[d|]eqn:E;
     [destruct d|];try(rewrite NoFn in Read;discriminate).
   inversion Read;subst.
   pose proof(Rows _ (reify_declaration_lookup_member _ _ _ E))as Row;cbn in Row.
   destruct Row as[Phi[Tele[ParamTypes Result]]].
   exists Phi;split;[exact Tele|];split;[exact ParamTypes|];split;[exact Result|].
   intros b Impossible;discriminate.
 - intros m D signature Read;cbn in Read.
   destruct(lookup_reify_method m declarations)as[[inputs sig]|]eqn:E;[|rewrite NoMth in Read;discriminate].
   inversion Read;subst.
   destruct(reify_method_lookup_member _ _ _ _ E)as[r[th Member]].
   exact(Rows _ Member).
 - intros u Du Dr body Read;cbn in Read.
   destruct(lookup_reify_decl(IFun u)declarations)as[d|]eqn:L;
     [destruct d as[|Dv tau|Df Sf|Dm Sm|schema|original]|];
     try(rewrite NoFun in Read;discriminate);try discriminate.
   inversion Read;subst.
   assert(E:lookup_reify_functor u declarations=Some schema).
   { unfold lookup_reify_functor;rewrite L;reflexivity. }
   assert(Member:In(IFun u,RDFunctor schema)declarations).
   { eapply reify_declaration_lookup_member;exact L. }
   pose proof(Rows _ Member)as Row;cbn in Row;destruct Row as[Phi[Input Output]].
   exists Phi;split;[exact Input|];split.
   + intros _;split.
     * cbn;rewrite E;reflexivity.
     * split;[cbn;rewrite E;intros t Hin;contradiction|].
       split;[cbn;rewrite E;intros t Hin;contradiction|exact Output].
   + intros bs Impossible;discriminate.
 - intros n D Read;exact(Opaque n D Read).
Qed.
