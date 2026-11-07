(* The attached-body receiver read is a supported complete nominal type. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame InfoPreserve BodySound BodyTr BodyTy
 BodyDataHelpers CanonBodyTy CanonTypes ContextClean NominalSupport TagArgsExec FnBodyTrace FnBodyOps.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma ok_body_receiver:forall g e,ok(body_receiver g e).
Proof. exact FnBodyOps.ok_body_receiver. Qed.
Section Receiver.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Theorem body_receiver_valid:forall g e s this final,
 body_receiver g e s=Good(this,final)->Inv s->st_w s=w->
 ContextClean e->EnvSupport(BodySound.W w R Fl)e->Later w P R Fl final->
 match this with None=>True|Some t=>ety_ok t=true/\Support(BodySound.W w R Fl)t end.
Proof.
 intros g e s this final E I W Clean Supported Late;unfold body_receiver in E.
 destruct(gs_recv g)as[r|]eqn:Recv;[|injection E as <- <-;exact Logic.I].
 apply bind_inv in E as[rg[s1[Read E]]].
 destruct(kind_eqb(gs_kind rg)KUnit||kind_eqb(gs_kind rg)KTag)eqn:Kind;[|body_trace_noerr E].
 assert(Kinds:gs_kind rg=KTag\/gs_kind rg=KUnit).
 {apply orb_true_iff in Kind as[K|K];apply kind_eqb_spec in K;auto. }
 destruct(sym_nominal w b Hload s r rg s1 I W Read Kinds)as[Same Loaded];subst s1.
 apply bind_inv in E as[xs[s2[Args E]]];injection E as <- <-.
 pose proof(nominal_args_complete w R Fl _ _ _ _ _ _ Args I Late)as Complete.
 assert(Types:Forall(fun p=>ety_ok(snd p)=true)xs).
 {clear -Clean Complete;induction Complete as[|key p keys ps [Key At]Complete IH];constructor;[|exact IH].
  exact(context_clean_tymap e Clean key(snd p)At). }
 split.
 - cbn[ety_ok];apply andb_true_iff;split.
   + apply negb_true_iff,Nat.eqb_neq;eapply loader_nonzero;eassumption.
   + apply forallb_forall;intros p At;exact(proj1(Forall_forall _ _)Types p At).
 - apply SupportNom.
   + pose proof(kind_sym_read b w R Fl Hload r rg Loaded)as K.
     destruct Kinds as[HeadKind|HeadKind];rewrite HeadKind in K;[right|left];exact K.
   + change(map fst xs=R r).
     assert(Id:gs_id rg=r).
     {unfold sym in Read;destruct(nget(st_syms s)r)as[actual|]eqn:Lookup;[|discriminate].
      injection Read as <-;exact(iv_ids s I _ _ Lookup). }
     rewrite <-Id;clear -Complete;induction Complete as[|key p keys ps [Key At]Complete IH];cbn;[reflexivity|rewrite Key,IH;reflexivity].
   + eapply nominal_args_support;[exact Args|exact I|exact Late|].
     intros key y At;rewrite CanonBodyTy.nassoc_tymap in At;eapply tn_get_support;eassumption.
Qed.
End Receiver.
Lemma okI_body_receiver:forall g e,okI(body_receiver g e).
Proof. intros;unfold body_receiver,nominal_args;okIt. Qed.

Lemma kw_body_receiver:forall g e,keeps st_fwd(body_receiver g e).
Proof. intros;unfold body_receiver,nominal_args;keepst. Qed.
#[export]Hint Resolve kw_body_receiver:keepsdb.
