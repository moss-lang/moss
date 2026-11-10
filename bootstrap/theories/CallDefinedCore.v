(* Defined calls with every need already in force have no family prefix. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame FnInfoCore NeedMapExec MethodKeyTr InstantiateTr RuleBKeep.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Lemma mfold_identity_checked : forall X Y(f:X->Y->M X) l,
 (forall y,In y l->forall a s b s',Inv s->f a y s=Good(b,s')->b=a/\s'=s)->
 forall a s b s',Inv s->mfold f a l s=Good(b,s')->b=a/\s'=s.
Proof.
 intros X Y f l;induction l as[|y l IH];intros Step a s b s' Hi E.
 - injection E as <- <-;auto.
 - cbn[mfold]in E. apply bind_inv in E as[a1[s1[E1 E]]].
   destruct(Step _ (or_introl eq_refl)_ _ _ _ Hi E1)as[-> ->].
   exact(IH ltac:(intros; eapply Step;[right;eassumption|eassumption..]) _ _ _ _ Hi E).
Qed.
Theorem demand_needs_present : forall f c e info s pre e' s',
 Inv s->(forall k,In k(fi_needs info)->key_present e(site_key e k)=true)->
 demand_needs f c e info s=Good((pre,e'),s')->pre=[]/\e'=e/\s'=s.
Proof.
 intros[|f]c e info s pre e' s' Hi Present E;[discriminate E|].
 cbn[demand_needs]in E. apply bind_inv in E as[apps[sa[Eapps E]]].
 assert(Id:apps=[]/\sa=s).
 { eapply mfold_identity_checked;[|exact Hi|exact Eapps].
   intros k Hk acc sx out sy Ix Ex. destruct k as[v|n|r m|u];cbn beta iota in Ex;
    try(injection Ex as <- <-;split;reflexivity).
   apply bind_inv in Ex as[mg[sm[Es Ex]]].
   pose proof(sym_id_checked _ _ _ _ Ix Es)as Id.
   unfold sym in Es. destruct(nget(st_syms sx)m)as[gg|]eqn:Lookup;[|discriminate Es].
   injection Es as <- <-.
   apply bind_inv in Ex as[key[sk[Ek Ex]]].
   destruct(@method_key_value _ _ _ _ _ _ _ Ek)as[-> ->]. rewrite Id in Ex.
   pose proof(Present _ Hk)as Pres. cbn[site_key]in Pres. rewrite Pres in Ex. injection Ex as <- <-;split;reflexivity. }
 destruct Id as[-> ->]. injection E as <- <- <-;repeat split;reflexivity.
Qed.

Definition defined_call_code f c e (fid:nat)(g:gsym)args:M(xexpr*ety):=
 let! info:=fn_info g in
 requirements_in_force(cx_mod c)e g info "calling";;
 let ep:=principal_env e info in
 let! (pre,ed):=demand_needs f c ep info in
 let! pairs:=require_needs(cx_mod c)ed g info in
 let! sm:=smap_at(fi_env info)ed pairs in
 let! sg:=fn_sig g(tymap ed)ed None in
 let! xs:=check_args f c e(gs_name g)sg(map AA args)in
 layout_ok ed(sg_ret sg);;
 let! th:=call_theta ed g in
 ret(fold_stmts pre(XCallD fid th sm xs),sg_ret sg).
Lemma defined_call_code_exec : forall f c e fid g args s x t s',
 defined_call_code f c e fid g args s=Good((x,t),s')->
 exists info si pre ed sd pairs sn sm ss sg sgstate xs sa u sl th,
 fn_info g s=Good(info,si)/\demand_needs f c(principal_env e info)info si=Good((pre,ed),sd)/\
 require_needs(cx_mod c)ed g info sd=Good(pairs,sn)/\
 smap_at(fi_env info)ed pairs sn=Good(sm,ss)/\
 fn_sig g(tymap ed)ed None ss=Good(sg,sgstate)/\
 check_args f c e(gs_name g)sg(map AA args)sgstate=Good(xs,sa)/\
 layout_ok ed(sg_ret sg)sa=Good(u,sl)/\call_theta ed g sl=Good(th,s')/\
 x=fold_stmts pre(XCallD fid th sm xs)/\t=sg_ret sg.
Proof.
 intros f c e fid g args s x t s' E. unfold defined_call_code in E.
 apply bind_inv in E as[info[si[Ei E]]].
 apply bind_inv in E as[rbu[rbs[RBk E]]];pose proof(requirements_in_force_keep _ _ _ _ _ _ _ _ RBk)as ->.
 apply bind_inv in E as[[pre ed][sd[Ed E]]].
 apply bind_inv in E as[pairs[sn[En E]]].
 apply bind_inv in E as[sm[ss[Es E]]].
 apply bind_inv in E as[sg[sgstate[Eg E]]].
 apply bind_inv in E as[xs[sa[Ea E]]].
 apply bind_inv in E as[u[sl[El E]]].
 apply bind_inv in E as[th[st[Et E]]]. injection E as <- <- <-.
 exists info,si,pre,ed,sd,pairs,sn,sm,ss,sg,sgstate,xs,sa,u,sl,th;repeat split;assumption.
Qed.
