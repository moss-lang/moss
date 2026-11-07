(* Canonical source unions retain their checked nominal heads. This keeps
   physical type formation stronger than the older WFty shape invariant. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss Elab BodyTr MergeWf.
Import ListNotations.
Definition source_type_read(Fl:String.string->nat)(rho:subst)(raw:ety):ty:=
 Audit.app_subst rho(tr Fl raw).
Definition source_arguments_read(Fl:String.string->nat)(rho:subst)(args:list(nat*ety)):
 list(nat*ty):=map(fun p=>(fst p,source_type_read Fl rho(snd p)))args.
Lemma source_nominal_read : forall Fl rho n args,
 source_type_read Fl rho(YNom n args)=TNom n(source_arguments_read Fl rho args).
Proof. intros;unfold source_type_read,source_arguments_read;cbn[tr];rewrite app_subst_nom;unfold map_snd;rewrite map_map;reflexivity. Qed.
Lemma source_union_read : forall Fl rho members,
 source_type_read Fl rho(YUnion members)=TUnion(map(source_type_read Fl rho)members).
Proof. intros;unfold source_type_read;cbn[tr];rewrite app_subst_union,map_map;reflexivity. Qed.
Lemma source_record_read : forall Fl rho fields,
 source_type_read Fl rho(YRec fields)=TNom rec_nom
   (map(fun p=>(Fl(fst p),source_type_read Fl rho(snd p)))fields).
Proof. intros;unfold source_type_read;cbn[tr];rewrite app_subst_nom;unfold map_snd;rewrite map_map;reflexivity. Qed.
Lemma formed_nominal_argument : forall Sg Phi n args pair,
 wf_ty Sg Phi(TNom n args)->In pair args->wf_ty Sg Phi(snd pair).
Proof.
 intros Sg Phi n args pair Formed Member;inversion Formed;subst;
 match goal with Values:Forall(fun p=>wf_ty Sg Phi(snd p))args|-_=>
   exact(proj1(Forall_forall _ _)Values pair Member)end.
Qed.
Lemma formed_nominal_replace_arguments : forall Sg Phi n before after,
 tags_typeonly Sg->wf_ty Sg Phi(TNom n before)->map fst after=map fst before->
 Forall(fun p=>wf_ty Sg Phi(snd p))after->wf_ty Sg Phi(TNom n after).
Proof.
 intros Sg Phi n before after Tags Formed Keys Values;inversion Formed;subst.
 - eapply WfTag;[eassumption| |exact Values].
   eapply sat_typeonly;[eapply Tags;eassumption|eassumption|exact Keys].
 - apply WfRec;assumption.
 - eapply WfOpaque;[eassumption|eassumption| |eassumption|exact Values].
   eapply sat_typeonly;eassumption.
Qed.

Lemma canonical_nominal_heads : forall Fl rho uf fuel members,
 Forall(fun m=>exists n args,m=YNom n args)members->forall sigma,
 heads_of sigma(map(fun raw=>source_type_read Fl rho(canon_go fuel uf raw))members)=
 heads_of sigma(map(source_type_read Fl rho)members).
Proof.
 intros Fl rho uf fuel members Nominal sigma;induction Nominal as[|member rest[n[args ->]]Rest IH].
 - reflexivity.
 - destruct fuel as[|fuel];[reflexivity|].
   change(Some n::heads_of sigma(map(fun raw=>source_type_read Fl rho(canon_go(S fuel)uf raw))rest)=
     Some n::heads_of sigma(map(source_type_read Fl rho)rest)).
   f_equal;exact IH.
Qed.

Theorem canonical_source_type_formed : forall Sg Phi Fl rho uf,
 tags_typeonly Sg->
 (forall generation raw,nassoc generation uf=Some raw->WFty raw)->
 (forall generation raw,nassoc generation uf=Some raw->
   wf_ty Sg Phi(source_type_read Fl rho raw))->
 forall fuel raw,WFty raw->wf_ty Sg Phi(source_type_read Fl rho raw)->
 wf_ty Sg Phi(source_type_read Fl rho(canon_go fuel uf raw)).
Proof.
 intros Sg Phi Fl rho uf Tags UfShape UfFormed fuel;induction fuel as[|fuel IHfuel];
 [intros;exact H0|].
 intro raw;induction raw as[| |generation|head args IHargs|members IHmembers|fields IHfields]
   using ety_ind';intros Shape Formed.
 - exact Formed.
 - exact Formed.
 - cbn[canon_go];destruct(nassoc generation uf)as[meaning|]eqn:Read;[|exact Formed].
   destruct(ety_eqb meaning(YAbs generation));[exact Formed|].
   apply IHfuel;[exact(UfShape generation meaning Read)|apply UfFormed with(generation:=generation);exact Read].
 - rewrite source_nominal_read in Formed.
   cbn[canon_go];rewrite source_nominal_read.
   eapply formed_nominal_replace_arguments;[exact Tags|exact Formed| |].
   + unfold source_arguments_read;rewrite !map_map;apply map_ext;intros[k raw];reflexivity.
   + unfold source_arguments_read;rewrite map_map.
     apply Forall_forall;intros pair Member.
     apply in_map_iff in Member as[[key raw][<- Present]];cbn.
     apply Forall_forall with(x:=(key,raw))in IHargs;[|exact Present].
     apply IHargs.
     * unfold WFty in Shape;cbn[ety_wf]in Shape;rewrite forallb_forall in Shape.
       exact(Shape _ Present).
     * eapply(@formed_nominal_argument Sg Phi head (source_arguments_read Fl rho args)
         (key,source_type_read Fl rho raw));[exact Formed|].
       unfold source_arguments_read;apply in_map_iff;exists(key,raw);split;[reflexivity|exact Present].
 - rewrite source_union_read in Formed;inversion Formed;subst.
   assert(Nominal:Forall(fun m=>exists n args,m=YNom n args)members).
   { unfold WFty in Shape;destruct members as[|first rest];[discriminate|].
     cbn[ety_wf]in Shape;apply Forall_forall;intros m Present.
     rewrite forallb_forall in Shape;specialize(Shape _ Present).
     destruct m;try discriminate;eauto. }
   cbn[canon_go];rewrite source_union_read,map_map;apply WfUnion.
   + apply Forall_forall;intros value Member;apply in_map_iff in Member as[raw[<- Present]].
     apply Forall_forall with(x:=raw)in IHmembers;[|exact Present].
     apply IHmembers.
     * unfold WFty in Shape;destruct members as[|first rest];[contradiction|].
       cbn[ety_wf]in Shape;rewrite forallb_forall in Shape;specialize(Shape _ Present).
       destruct raw;try discriminate;exact Shape.
     * match goal with Known:Forall(wf_ty Sg Phi)(map(source_type_read Fl rho)members)|-_=>
         apply(proj1(Forall_forall _ _)Known);apply in_map;exact Present end.
   + apply Forall_forall;intros value Member;apply in_map_iff in Member as[raw[<- Present]].
     apply Forall_forall with(x:=raw)in Nominal;[|exact Present].
     destruct Nominal as[n[args ->]];rewrite source_nominal_read;exists n; eexists;reflexivity.
   + change(NoDup(heads_of(cS Phi)(map(fun raw=>source_type_read Fl rho(canon_go(S fuel)uf raw))members))).
     rewrite canonical_nominal_heads;[assumption|exact Nominal].
 - rewrite source_record_read in Formed.
   cbn[canon_go];rewrite source_record_read.
   eapply formed_nominal_replace_arguments;[exact Tags|exact Formed| |].
   + rewrite !map_map;apply map_ext;intros[k raw];reflexivity.
   + rewrite map_map;apply Forall_forall;intros pair Member;apply in_map_iff in Member as[[key raw][<- Present]];cbn.
     apply Forall_forall with(x:=(key,raw))in IHfields;[|exact Present].
     apply IHfields.
     * unfold WFty in Shape;cbn[ety_wf]in Shape;rewrite forallb_forall in Shape;exact(Shape _ Present).
     * eapply(@formed_nominal_argument Sg Phi rec_nom
         (map(fun p=>(Fl(fst p),source_type_read Fl rho(snd p)))fields)
         (Fl key,source_type_read Fl rho raw));[exact Formed|].
       apply in_map_iff;exists(key,raw);split;[reflexivity|exact Present].
Qed.
