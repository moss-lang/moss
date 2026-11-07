(* Source type formation survives physical generation renaming. Nominal
   interfaces may be known or genuinely opaque; no payload is invented. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import CoreMoss ReifyFormation ReifyTypeTransport.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition source_nominal_schemas(W:A.world)(Sg:gsig):Prop:=
 forall n k,A.kind_sym W n=Some k->(k=A.DKTag\/k=A.DKUnit)->
 (exists payload,g_tag Sg n=Some(map ITy(A.w_reqs W n),payload))\/
 (g_tag Sg n=None/\g_opaque Sg n=Some(map ITy(A.w_reqs W n))).
Lemma source_meaning_lookup_app : forall (first second:subst) key value,
 A.nlook key(first++second)=Some value->
 A.nlook key first=Some value\/A.nlook key second=Some value.
Proof.
 induction first as[|[name meaning]rest IH];intros second key value Read;cbn in Read|-*.
 - right;exact Read.
 - destruct(Nat.eqb key name);[left;exact Read|].
   destruct(IH _ _ _ Read);auto.
Qed.
Lemma source_meaning_lookup_member : forall (meanings:subst) key value,
 A.nlook key meanings=Some value->In(key,value)meanings.
Proof.
 induction meanings as[|[name meaning]rest IH];intros key value Read;[discriminate|].
 cbn in Read;destruct(Nat.eqb key name)eqn:Same.
 - apply Nat.eqb_eq in Same;subst;inversion Read;left;reflexivity.
 - right;eapply IH;exact Read.
Qed.
Section Formation.
Variable W:A.world.
Variable Sg:gsig.
Variable rho:subst.
Hypothesis Nominal:source_nominal_schemas W Sg.
Hypothesis Unique:forall n,NoDup(A.w_reqs W n).
Hypothesis RecordHead:g_tag Sg rec_nom=None.
Definition source_meanings_formed(Phi:ctx)(meanings:subst):Prop:=
 forall name meaning,A.nlook name meanings=Some meaning->wf_ty Sg Phi(app_subst rho meaning).
Fixpoint source_type_formed p meanings receiver syntax tau(H:A.TyOf W p meanings receiver syntax tau){struct H}:
 forall Phi,source_meanings_formed Phi meanings->
 (forall t,receiver=Some t->wf_ty Sg Phi(app_subst rho t))->
 wf_ty Sg Phi(app_subst rho tau)
with source_brackets_formed p meanings original brackets bindings(H:A.Brackets W p meanings original brackets bindings){struct H}:
 forall Phi,source_meanings_formed Phi meanings->source_meanings_formed Phi bindings.
Proof.
 - destruct H as[p tm th|p tm th|p tm t|p tm th fields values Fields|
     p tm th astMembers types flat Types Flat Nominals Heads|
     p tm th path name tau Resolved Kind Meaning|
     p tm th path brackets name kind bindings args Resolved Kind NomKind Bracket Args|
     p tm th path brackets name declaration label body bindings tau Resolved Declaration Alias Bracket Body];
   intros Phi Meanings Receiver.
   + apply WfUnion;constructor.
   + constructor.
   + exact(Receiver t eq_refl).
   + cbn[app_subst];apply WfRec;[exact RecordHead|].
     induction Fields as[|nt ft fields values[Number Typed]Rest IH];cbn;constructor;[|exact IH].
     exact(source_type_formed _ _ _ _ _ Typed Phi Meanings Receiver).
   + rewrite app_subst_union;apply WfUnion.
     * assert(Formed:Forall(fun t=>wf_ty Sg Phi(app_subst rho t))types).
       { clear Flat Nominals Heads;induction Types as[|m t ms ts Typed Rest IH];constructor;[|exact IH].
         exact(source_type_formed _ _ _ _ _ Typed Phi Meanings Receiver). }
       subst flat;apply Forall_forall;intros value Member.
       apply in_map_iff in Member as[raw[<- Present]].
       apply in_flat_map in Present as[t[Present Member]].
       apply Forall_forall with(x:=t)in Formed;[|exact Present].
       destruct t as[|key|head args|members];cbn in Member;
         try(destruct Member as[<-|[]];exact Formed).
       rewrite app_subst_union in Formed;inversion Formed;subst.
       match goal with Known:Forall(wf_ty Sg Phi)(map _ members)|-_=>
         apply Forall_forall with(x:=app_subst rho raw)in Known;
           [exact Known|apply in_map;exact Member]end.
     * apply Forall_forall;intros value Member;apply in_map_iff in Member as[raw[<- Present]].
       apply Forall_forall with(x:=raw)in Nominals;[|exact Present].
       destruct Nominals as[n[xs[-> NonRecord]]].
       exists n,(map(fun p=>(fst p,app_subst rho(snd p)))xs);reflexivity.
     * assert(Noms:Forall is_nom flat).
       { eapply Forall_impl;[|exact Nominals];intros value[n[xs[-> _]]];exists n,xs;reflexivity. }
       rewrite(nominal_heads_renaming flat[](cS Phi)rho Noms).
       replace(heads_of [] flat)with(map A.head flat);[exact Heads|].
       unfold heads_of;apply map_ext_in;intros value Member.
       apply Forall_forall with(x:=value)in Nominals;[|exact Member].
       destruct Nominals as[n[xs[-> _]]];reflexivity.
   + exact(Meanings _ _ Meaning).
   + assert(Bindings:source_meanings_formed Phi bindings).
     { exact(source_brackets_formed _ _ _ _ _ Bracket Phi Meanings). }
     assert(Domain:map fst args=A.w_reqs W name).
     { clear -Args;induction Args as[|r pair rs pairs[Key Lookup]Rest IH];cbn;[reflexivity|rewrite Key,IH;reflexivity]. }
     assert(Satisfied:sat Sg Phi(map(fun p=>(fst p,app_subst rho(snd p)))args)(map ITy(A.w_reqs W name))).
     { apply type_telescope_sat;[apply Unique|rewrite map_map;exact Domain]. }
     assert(Values:Forall(fun p=>wf_ty Sg Phi(snd p))(map(fun p=>(fst p,app_subst rho(snd p)))args)).
     { apply Forall_forall;intros value Member;apply in_map_iff in Member as[[r tau][<- Present]].
       assert(Read:A.nlook r(bindings++tm)=Some tau).
       { clear -Args Present;induction Args as[|r0 pair rs pairs[Key Lookup]Rest IH];[contradiction|].
         destruct Present as[Same|Present];[inversion Same;subst;cbn in *;exact Lookup|apply IH;exact Present]. }
       destruct(source_meaning_lookup_app _ _ _ _ Read)as[ReadBinding|ReadMeaning];
         [exact(Bindings _ _ ReadBinding)|exact(Meanings _ _ ReadMeaning)]. }
     cbn[app_subst];destruct(Nominal name kind Kind NomKind)as[[payload Known]|[NoPayload Opaque]].
     * eapply WfTag;[exact Known|exact Satisfied|exact Values].
     * eapply WfOpaque;[exact NoPayload|exact Opaque|exact Satisfied| |exact Values].
       apply Forall_forall;intros i Member;apply in_map_iff in Member as[t[<- _]];reflexivity.
   + apply(source_type_formed _ _ _ _ _ Body Phi).
     * intros queried value Read;destruct(source_meaning_lookup_app _ _ _ _ Read)as[ReadBinding|ReadMeaning].
       -- exact(source_brackets_formed _ _ _ _ _ Bracket Phi Meanings _ _ ReadBinding).
       -- exact(Meanings _ _ ReadMeaning).
     * intros t Impossible;discriminate.
 - destruct H as[p tm original|p tm original brackets bindings Types];intros Phi Meanings name value Read;
     [discriminate|].
   apply source_meaning_lookup_member in Read;apply(proj2(in_rev _ _))in Read.
   induction Types as[|b pair bs pairs[path[args[_[_[_[_ Typed]]]]]]Rest IH];[contradiction|].
   destruct Read as[Same|Present];[|apply IH;exact Present].
   subst pair;exact(source_type_formed _ _ _ _ _ Typed Phi Meanings(fun t Impossible=>ltac:(discriminate))).
Qed.
End Formation.
