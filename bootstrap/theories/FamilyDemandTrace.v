(* Actual family applications collected and sequenced before a call. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx ReifyData Elab ElabSound ElabFrame BodySound ApplyAbstractExec.
Import ListNotations.
Local Open Scope list_scope.
Definition demand_group:=((gsym*list(nat*ety))*list item)%type.
Definition demand_collect(c:cx)(e:env)(info:fninfo):M(list demand_group):=
 let mi:=cx_mod c in
mfold (fun apps need =>
      match need with
      | KyMth r m =>
          let! mg := sym m in
          let! k := method_key e mi (instantiate e r) mg in
          if key_present e k then ret apps else
          match k with
          | KyMth r' _ =>
              let! fm := family_member mi e r' m in
              match fm with
              | Some (fg, th) =>
                  let! itk := key_item k in
                  let same q := Nat.eqb (gs_id (fst (fst q))) (gs_id fg) &&
                                Nat.eqb (List.length (snd (fst q))) (List.length th) &&
                                forallb (fun p => match nassoc (fst p) (snd (fst q)) with
                                                  | Some y => ety_eqb y (snd p) | None => false end) th in
                  if existsb same apps
                  then ret (map (fun q => if same q then (fst q, snd q ++ [itk]) else q) apps)
                  else ret (apps ++ [((fg, th), [itk])])
              | None => ret apps
              end
          | _ => ret apps
          end
      | _ => ret apps
      end) [] (fi_needs info).
Inductive FamilyApplications(f:nat)(c:cx):env->st->list demand_group->list xstmt->env->st->Prop:=
 | FANil:forall e s,FamilyApplications f c e s[][]e s
 | FACons:forall e s fg th wanted rest full sm e1 s1 pre e2 s2,
   apply_family f c e fg th wanted s=Good(([SApply(gs_id fg)full sm],e1),s1)->
   FamilyApplications f c e1 s1 rest pre e2 s2->
   FamilyApplications f c e s(((fg,th),wanted)::rest)(SApply(gs_id fg)full sm::pre)e2 s2.
Lemma apply_family_single:forall f c e fg th wanted s pre e' s',
 apply_family f c e fg th wanted s=Good((pre,e'),s')->exists full sm,pre=[SApply(gs_id fg)full sm].
Proof.
 intros[|f]c e fg th wanted s pre e' s' E;[discriminate E|].
 cbn[apply_family]in E.
 apply bind_inv in E as(e1&s1&Types&E).
 apply bind_inv in E as([[xs e2]c2]&s2&App&E).
 apply bind_inv in E as(mem&s3&Mem&E).
 apply bind_inv in E as([]&s4&Flag&E);injection E as <- _ _.
 destruct(apply_abstract_phases _ _ _ _ _ _ _ _ _ App)
 as(ef&needs&sf&xf&sx&sy&sz&pairs&sp&Av&Env&Fix&Guard&Input&Req&Tail).
 destruct(abstract_tail_phases _ _ _ _ _ _ _ _ _ _ _ Tail)
 as(rts&sr&gens&sg&full&st0&extra&st1&sm&ss&keys&so&R&G&T&T2&S&O&Meta&->&Ctx).
 exists(full++extra),sm;reflexivity.
Qed.
Definition demand_apply_step(f:nat)(c:cx)(acc:list xstmt*env)(q:demand_group):M(list xstmt*env):=
 let '(ss,e):=acc in
 let! (ys,e'):=apply_family f c e(fst(fst q))(snd(fst q))(snd q)in ret(ss++ys,e').
Lemma demand_apply_trace:forall f c groups acc e s pre e' s',
 mfold(demand_apply_step f c)(acc,e)groups s=Good((pre,e'),s')->
 exists fresh,pre=acc++fresh/\FamilyApplications f c e s groups fresh e' s'.
Proof.
 intros f c groups;induction groups as[|[[fg th]wanted]rest IH];intros acc e s pre e' s' E.
 - cbn[mfold]in E;injection E as <- <- <-;exists[];split;[rewrite app_nil_r;reflexivity|constructor].
 - cbn[mfold]in E;apply bind_inv in E as([ss e1]&s1&Step&E).
   unfold demand_apply_step in Step;cbn[fst snd]in Step.
   apply bind_inv in Step as([ys ey]&sy&App&Ret);injection Ret as <- <- <-.
   destruct(apply_family_single _ _ _ _ _ _ _ _ _ _ App)as(full&sm&->).
   destruct(IH _ _ _ _ _ _ E)as(fresh&Eq&Trace).
   exists(SApply(gs_id fg)full sm::fresh);split.
   + rewrite Eq,<-app_assoc;reflexivity.
   + eapply FACons;eauto.
Qed.
Theorem demand_needs_trace:forall f c e info s pre e' s',
 demand_needs(S f)c e info s=Good((pre,e'),s')->
 exists groups si,demand_collect c e info s=Good(groups,si)/\FamilyApplications f c e si groups pre e' s'.
Proof.
 intros f c e info s pre e' s' E;cbn[demand_needs]in E.
 apply bind_inv in E as(groups&si&Select&Apply).
 destruct(demand_apply_trace f c groups[]e si pre e' s' Apply)as(fresh&Eq&Trace).
 cbn[List.app]in Eq;subst pre;exists groups,si;auto.
Qed.
Lemma family_applications_length:forall f c e s groups pre e' s',
 FamilyApplications f c e s groups pre e' s'->List.length pre=List.length groups.
Proof. intros f c e s groups pre e' s' H;induction H;cbn;congruence. Qed.
