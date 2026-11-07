(* A successful family leaf scan contains methods and context wrappers only. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTy.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Lemma false_family_fold:forall X(step:X->M bool)xs s,
 mfold(fun b x=>if negb b then ret false else step x)false xs s=Good(false,s).
Proof.
 intros X step xs;induction xs as[|x xs IH];intro s;cbn[mfold ret];[reflexivity|apply IH].
Qed.
Lemma true_family_fold:forall X(step:X->M bool)(I:st->Prop)(Q:X->Prop),
 (forall x si b sf,step x si=Good(b,sf)->I si->I sf)->
 (forall x si sf,step x si=Good(true,sf)->I si->Q x)->
 forall xs s s',mfold(fun b x=>if negb b then ret false else step x)true xs s=Good(true,s')->
 I s->Forall Q xs.
Proof.
 intros X step I Q Keep Sound xs;induction xs as[|x xs IH];intros s s' E Hi;[constructor|].
 cbn[mfold]in E;apply bind_inv in E as(b&sm&First&E).
 cbn[negb]in First.
 destruct b;[|rewrite false_family_fold in E;discriminate E].
 constructor;[eapply Sound;eauto|eapply IH;[exact E|eapply Keep;eauto]].
Qed.

Definition family_leaf_step(f mi:nat)(ps:list nat)(sp:spec):M bool:=
match sp with
        | Spec (Some p) (Some dn) a =>
            let! ro := resolve_sym mi p in
            match ro with
            | None => ret false
            | Some r =>
                let! mo := find_method_decl mi r dn in
                match mo with
                | None => ret false
                | Some m =>
                    let! keyed := free_type_syms 200 (ATRef p None) mi [] in
                    let bs := opt_default [] a in
                    let! bound := mmap (fun b => let! l := resolve_sym mi (fst b) in
                                                 ret (match l with Some x => [gs_id x] | None => [] end)) bs in
                    let bound := List.concat bound in
                    let! said := mfold (fun o b => match snd b with
                                                   | Spec (Some sp') _ sa => free_type_syms 200 (ATRef sp' sa) mi o
                                                   | _ => ret o
                                                   end) [] bs in
                    let! said := mfold (fun o q => free_type_syms 200 (snd q) (gs_mod m) o) said (fn_params m) in
                    let! said := match fn_ret m with Some t => free_type_syms 200 t (gs_mod m) said | None => ret said end in
                    let said := filter (fun t => negb (nmem t bound)) said ++
                                filter (fun t => nmem t bound && nmem t keyed) said in
                    ret (negb (existsb (fun t => nmem t ps && negb (nmem t keyed)) said))
                end
            end
        | Spec (Some p) None _ =>
            let! go := resolve_sym mi p in
            match go with
            | Some g => match gs_kind g, gs_decl g with
                        | KCtx, ADCtx _ its => family_leaves_ok f (gs_mod g) ps its
                        | _, _ => ret false
                        end
            | None => ret false
            end
        | _ => ret false
        end.
Local Strategy opaque[FUEL free_type_syms].
Lemma ok_family_leaf_step:forall f mi ps sp,ok(family_leaf_step f mi ps sp).
Proof. intros;unfold family_leaf_step;okt. Qed.
Lemma family_result_no_types:forall W p sp t,
 A.SpecTypes W p sp t->A.FamilyResult W p sp->False.
Proof.
 intros W p sp t H;induction H as[p pa a t Hr Hk|p pa a c d n its sp t Hr Hd Da Hin Hs IH];intro HF.
 - inversion HF as[|mi' pa' br' u d' n' its' Ru Du Au Fu];subst.
   assert(u=t)by congruence;subst u.
   unfold A.kind_sym in Hk;rewrite Du,Au in Hk;discriminate Hk.
 - inversion HF as[|mi' pa' br' u d' n' its' Ru Du Au Fu];subst.
   assert(u=c)by congruence;subst u.
   assert(d'=d)by congruence;subst d'.
   assert(its'=its)by congruence;subst its'.
   apply IH;rewrite Forall_forall in Fu;apply Fu;exact Hin.
Qed.
Lemma family_shape_no_outputs:forall W u,A.FamilyShape W u->forall t,~A.FunOutputs W u t.
Proof.
 intros W u(d&n&ps&args&res&Hd&D&F)t(d'&ps'&args'&res'&body&sp&Hd'&Parts&Hin&Types).
 assert(d'=d)by congruence;subst d'. rewrite D in Parts;cbn[A.functor_parts]in Parts.
 injection Parts as <- <- <- <-.
 eapply family_result_no_types;[exact Types|rewrite Forall_forall in F;apply F;exact Hin].
Qed.
Lemma family_no_outputs:forall W u,A.Family W u->forall t,~A.FunOutputs W u t.
Proof. intros W u H;apply family_shape_no_outputs;exact(proj1 H). Qed.
Section Shape.
Variable w:Elab.world. Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Notation path_of mi:=(gm_path(get_mod w mi)).
Theorem family_leaves_ok_shape:forall f mi ps specs s s',
 family_leaves_ok f mi ps specs s=Good(true,s')->
 Inv s->st_w s=w->In mi(w_order w)->Forall(A.FamilyResult W(path_of mi))specs.
Proof.
 induction f as[|f IH];intros mi ps specs s s' E Hi Hw Hmi;[discriminate E|].
 change(mfold(fun ok sp=>if negb ok then ret false else family_leaf_step f mi ps sp)true specs s=Good(true,s'))in E.
 eapply true_family_fold with(I:=fun ss=>Inv ss/\st_w ss=w)(step:=family_leaf_step f mi ps);[| |exact E|split;assumption].
 - intros sp si v sf H[Iv Wv]. destruct(ok_family_leaf_step f mi ps sp _ _ _ Iv H)as[If Fr].
   split;[exact If|rewrite(fr_w _ _ Fr);exact Wv].
 - intros [pa dn br]si sf H[Iv Wv].
   destruct pa as[pa|];[|cbn[family_leaf_step]in H;discriminate H].
   destruct dn as[dn|];[constructor|].
   cbn[family_leaf_step]in H;apply bind_inv in H as(go&ss&Resolve&H).
   destruct(esym_rel w R Fl b Hload mi pa si go ss Iv Wv Hmi Resolve)as[-> Rel].
   destruct go as[g|];[|discriminate H]. destruct Rel as[Rel Hg].
   destruct(gs_kind g);try discriminate H.
   destruct(gs_decl g)eqn:D;try discriminate H.
   eapply A.FRContext with(u:=gs_id g)(d:=A.Decl(path_of(gs_mod g))(gs_decl g)(gs_assumes g)).
   + exact Rel.
   + exact(w_decl_read w R Fl _ _ Hg).
   + cbn[A.d_ast];exact D.
   + eapply IH;[exact H|exact Iv|exact Wv|exact(loader_mod_in w R Fl b Hload _ _ Hg)].
Qed.
Theorem is_family_shape:forall g n names args res s s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 is_family g s=Good(true,s')->Inv s->st_w s=w->A.FamilyShape W(gs_id g).
Proof.
 intros g n names args res s s' Hg D E Hi Hw.
 unfold is_family,functor_parts in E;rewrite D in E.
 destruct(negb(kind_eqb(gs_kind g)KFunctor))eqn:K;[discriminate E|].
 apply bind_inv in E as(ps&sp&Params&E).
 assert(Op:ok(family_params g))by(unfold family_params,functor_params;okt).
 destruct(Op _ _ _ Hi Params)as[Ip Fp].
 apply bind_inv in E as(f&sf&Fuel&E).
 assert(Of:ok walk_fuel)by okt.
 destruct(Of _ _ _ Ip Fuel)as[If Ff].
 exists(A.Decl(path_of(gs_mod g))(gs_decl g)(gs_assumes g)),n,names,args,res.
 split;[exact(w_decl_read w R Fl _ _ Hg)|].
 split;[exact D|].
 eapply family_leaves_ok_shape;[exact E|exact If| |exact(loader_mod_in w R Fl b Hload _ _ Hg)].
 rewrite(fr_w _ _ Ff),(fr_w _ _ Fp);exact Hw.
Qed.
Theorem is_family_loaded_shape:forall g s s',
 nget(w_syms w)(gs_id g)=Some g->is_family g s=Good(true,s')->
 Inv s->st_w s=w->A.FamilyShape W(gs_id g).
Proof.
 intros g s s' Hg E Hi Hw.
 assert(K:gs_kind g=KFunctor).
 { unfold is_family in E.
   destruct(functor_parts g)as[[args res]body];destruct body;[discriminate E|].
   destruct(gs_kind g);cbn[kind_eqb negb]in E;try discriminate E;reflexivity. }
 pose proof(load_world_kind b w Hload _ _ Hg)as KO;unfold KindOK in KO;rewrite K in KO.
 assert(Parts:exists n names args res body,gs_decl g=ADFunctor n names args res body).
 { destruct(gs_decl g);cbn[A.kind_of kconv]in KO;try discriminate KO;try(match goal with rc:fnrecv|-_=>destruct rc;discriminate KO end);do 5 eexists;reflexivity. }
 destruct Parts as(n&names&args&res&body&D).
 destruct body as[body|].
 - unfold is_family,functor_parts in E;rewrite D in E;discriminate E.
 - eapply is_family_shape;eauto.
Qed.
End Shape.
