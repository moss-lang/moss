(* Operational decomposition of the actual method-call branches. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabMono BodySound InfoPreserve MethodSelectExec.
Import ListNotations.
Local Open Scope string_scope.
Local Strategy opaque[Nat.of_num_uint FUEL synth provided_call fn_info demand_needs check_args canon].
Definition method_after_receiver(f:nat)(c:cx)(e:env)(ox:xexpr)(oty:ety)(p:path)(args:list aexpr):M(xexpr*ety):=
 let mi:=cx_mod c in
  let h := head (canon e oty) in
  let nm := last p "" in
  let! here := the_mod mi in
  let! m0 := match p with
             | [_] => ret None
             | _ => let! d := rdetached mi p in
                    match d with
                    | Some s => let! g := sym s in ret (Some g)
                    | None => err mi ("`" ++ show_path p ++ "` is not a detached method in scope")
                    end
             end in
  let attached_in (hid : nat) : M (option gsym) :=
    let! hg := sym hid in
    let! hm := the_mod (gs_mod hg) in
    match assoc attached_eqb (hid, nm) (gm_attached hm) with
    | Some s => let! g := sym s in ret (Some g)
    | None => match assoc attached_eqb (hid, nm) (gm_attached here) with
              | Some s => let! g := sym s in ret (Some g)
              | None => ret None
              end
    end in
  let detached := sassoc nm (gm_detached here) in
  (* D77: a provision of the detached method in scope wins over an attached one *)
  let! m1 := match m0, p, h with
             | None, [_], Some hid =>
                 let! att := attached_in hid in
                 match att, detached with
                 | Some _, Some d =>
                     let! dg := sym d in
                     if is_some (get_mth e (canon e oty) d) then ret (Some dg) else ret None
                 | _, _ => ret None
                 end
             | _, _, _ => ret m0
             end in
  match m1, h, detached with
  | None, None, None => let! s := show oty in err mi ("`" ++ s ++ "` has no methods (it has no nominal head)")
  | _, _, _ =>
  let! direct := match m1, h with
                 | None, Some hid => attached_in hid
                 | _, _ => ret None
                 end in
  match direct with
  | Some att =>
      if is_some (fn_body att) then
        (* a defined attached method: a direct call, at its receiver's instantiation (D74) *)
        let! info := fn_info att in
        let receiver := canon e oty in
        let! e2 := match receiver with
                   | YNom _ ((_ :: _) as xs) => mfold (fun e q => let! st := fresh in ret (set_tn e (fst q) (snd q) st)) e xs
                   | _ => ret e
                   end in
        let! (pre, e2) := demand_needs f c e2 info in
        let! pairs := require_needs mi e2 att info in
        let! sm := smap_at (fi_env info) e2 pairs in
        let! sg := fn_sig att (tymap e2) e2 (Some oty) in
        let! xs := check_args f c e (gs_name att) sg (map AA args) in
        layout_ok e (sg_ret sg) ;;
        let! th := call_theta e2 att in
        ret (fold_stmts pre (XCallD (gs_id att) th sm (ox :: xs)), sg_ret sg)
      else provided_call f c e ox oty att (map AA args)
  | None =>
      match m1, p with
      | Some m, _ => provided_call f c e ox oty m (map AA args)
      | None, [_] =>
          match detached with
          | Some d => let! dg := sym d in provided_call f c e ox oty dg (map AA args)
          | None => err mi ("`." ++ nm ++ "` is not a detached method in scope; import it, or call it as `mod::m` (D61)")
          end
      | None, _ => err mi ("`" ++ show_path p ++ "` is not a detached method in scope")
      end
  end
  end.

Lemma synth_method_view:forall f c e o p args,
 synth_method(S f)c e o p args=
 (let! (ox,oty):=synth f c e o None in method_after_receiver f c e ox oty p args).
Proof. reflexivity. Qed.
Lemma rdetached_state:forall mi p s r s',rdetached mi p s=Good(r,s')->s'=s.
Proof.
 intros mi p s r s' H;unfold rdetached in H.
 destruct(Elab.resolve_detached(st_w s)mi p);[injection H;auto|discriminate H].
Qed.
Lemma sym_state:forall n s g s',sym n s=Good(g,s')->s'=s.
Proof. intros n s g s' H;unfold sym in H;destruct(nget(st_syms s)n);[injection H;auto|discriminate H]. Qed.
Lemma method_qualified_exec:forall f c e ox oty p args s x t s',
 List.length p>1->method_after_receiver f c e ox oty p args s=Good((x,t),s')->
 exists mid m,rdetached(cx_mod c)p s=Good(Some mid,s)/\sym mid s=Good(m,s)/\
 provided_call f c e ox oty m(map AA args)s=Good((x,t),s').
Proof.
 intros f c e ox oty p args s x t s' Hp H.
 destruct p as[|n[|n2 rest]];cbn in Hp;try lia.
 unfold method_after_receiver in H;cbn beta iota zeta in H.
 apply bind_inv in H as(here&s1&Eh&H);unfold the_mod in Eh;injection Eh as <- <-.
 apply bind_inv in H as(m0&s1&Em&H).
 apply bind_inv in Em as(mid&s2&Ed&Em).
 pose proof(rdetached_state _ _ _ _ _ Ed)as ->.
 destruct mid as[mid|];[|unfold err,modpath,mbind,fail in Em;discriminate Em].
 apply bind_inv in Em as(m&s2&Es&Em).
 pose proof(sym_state _ _ _ _ Es)as ->;injection Em as <- <-.
 exists mid,m;split;[exact Ed|split;[exact Es|]].
 cbn[mbind ret]in H.
 destruct(head(canon e oty));destruct(sassoc(last(n::n2::rest)"")(gm_detached(get_mod(st_w s)(cx_mod c))));exact H.
Qed.

Definition method_attached_code(f:nat)(c:cx)(e:env)(ox:xexpr)(oty:ety)(att:gsym)(args:list aexpr):M(xexpr*ety):=
 let mi:=cx_mod c in
if is_some (fn_body att) then
        (* a defined attached method: a direct call, at its receiver's instantiation (D74) *)
        let! info := fn_info att in
        let receiver := canon e oty in
        let! e2 := match receiver with
                   | YNom _ ((_ :: _) as xs) => mfold (fun e q => let! st := fresh in ret (set_tn e (fst q) (snd q) st)) e xs
                   | _ => ret e
                   end in
        let! (pre, e2) := demand_needs f c e2 info in
        let! pairs := require_needs mi e2 att info in
        let! sm := smap_at (fi_env info) e2 pairs in
        let! sg := fn_sig att (tymap e2) e2 (Some oty) in
        let! xs := check_args f c e (gs_name att) sg (map AA args) in
        layout_ok e (sg_ret sg) ;;
        let! th := call_theta e2 att in
        ret (fold_stmts pre (XCallD (gs_id att) th sm (ox :: xs)), sg_ret sg)
      else provided_call f c e ox oty att (map AA args).

Definition method_singleton_code(f:nat)(c:cx)(e:env)(ox:xexpr)(oty:ety)(n:name)(args:list aexpr):M(xexpr*ety):=
 let mi:=cx_mod c in
 let! here:=the_mod mi in
 let h:=head(canon e oty)in
 let detached:=sassoc n(gm_detached here)in
 let! att:=match h with Some hid=>MethodSelectExec.attached_call_lookup mi hid n|None=>ret None end in
 match att,detached with
 |Some selected,Some d=>
   let! dg:=sym d in
   if is_some(get_mth e(canon e oty)d)then provided_call f c e ox oty dg(map AA args)
   else method_attached_code f c e ox oty selected args
 |Some selected,None=>method_attached_code f c e ox oty selected args
 |None,Some d=>let! dg:=sym d in provided_call f c e ox oty dg(map AA args)
 |None,None=>match h with
   |None=>let! text:=show oty in err mi("`"++text++"` has no methods (it has no nominal head)")
   |Some _=>err mi("`."++n++"` is not a detached method in scope; import it, or call it as `mod::m` (D61)")end
 end.
Lemma ok_method_after_receiver:forall f c e ox r pa args,ok(method_after_receiver f c e ox r pa args).
Proof. intros;unfold method_after_receiver;okt. Qed.
Local Strategy opaque[layout_ok fn_sig call_theta require_needs smap_at mfold fresh].
Local Opaque provided_call fn_info demand_needs check_args layout_ok fn_sig call_theta require_needs smap_at mfold fresh show err get_mth head canon.
Lemma method_singleton_exec:forall f c e ox oty n args s x t s',
 method_after_receiver f c e ox oty[n]args s=Good((x,t),s')->
 method_singleton_code f c e ox oty n args s=Good((x,t),s').
Proof.
 intros f c e ox oty n args s x t s' H.
 unfold method_after_receiver in H;cbv beta iota zeta delta[last]in H.
 apply bind_inv in H as(here&s1&Eh&H);unfold the_mod in Eh;injection Eh as <- <-.
 apply bind_inv in H as(m0&s1&Em&H);injection Em as <- <-.
 unfold method_singleton_code;cbv beta iota zeta delta[the_mod mbind ret].
 destruct(head(canon e oty))as[h|]eqn:Head.
 - fold(attached_here_lookup(get_mod(st_w s)(cx_mod c))h n)in H.
   rewrite<-(attached_here_call(cx_mod c)h n s).
   destruct(attached_here_lookup(get_mod(st_w s)(cx_mod c))h n s)as[[att ss]|bad]eqn:Lk.
   2:{cbv beta iota zeta delta[mbind ret]in H;rewrite Lk in H;discriminate H. }
   pose proof(attached_here_state _ _ _ _ _ _ Lk)as ->.
   cbv beta iota zeta delta[mbind ret]in H;rewrite Lk in H.
   destruct att as[att|];destruct(sassoc n(gm_detached(get_mod(st_w s)(cx_mod c))))as[d|]eqn:Det;
   cbv beta iota zeta delta[mbind ret]in H|-*.
   + destruct(sym d s)as[[dg sd]|bad]eqn:Ds.
     2:{discriminate H. }
     pose proof(sym_state _ _ _ _ Ds)as ->.
     destruct(is_some(get_mth e(canon e oty)d))eqn:Win;
      cbv beta iota zeta delta[mbind ret]in H|-*;
      try rewrite Lk in H;cbv beta iota zeta delta[mbind ret]in H|-*;
      try unfold method_attached_code;exact H.
   + rewrite Lk in H;cbv beta iota zeta delta[mbind ret]in H.
     unfold method_attached_code;exact H.
   + rewrite Lk in H;cbv beta iota zeta delta[mbind ret]in H.
     exact H.
   + rewrite Lk in H;cbv beta iota zeta delta[mbind ret]in H;exact H.
 - cbv beta iota zeta delta[mbind ret]in H|-*.
   destruct(sassoc n(gm_detached(get_mod(st_w s)(cx_mod c))));exact H.
Qed.
Ltac method_noerr H:=exfalso;solve[repeat(cbn beta iota zeta in H;first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(?&?&?&H)])].
Lemma method_empty_impossible:forall f c e ox r args s x t s',
 method_after_receiver f c e ox r[]args s<>Good((x,t),s').
Proof.
 intros f c e ox r args s x t s' H;unfold method_after_receiver in H;cbn beta iota zeta in H.
 apply bind_inv in H as(here&s1&Eh&H);unfold the_mod in Eh;injection Eh as <- <-.
 apply bind_inv in H as(m0&s1&Em&H).
 cbn[rdetached Elab.resolve_detached removelast_path mbind ret]in Em;method_noerr Em.
Qed.
