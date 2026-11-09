(* A definitional view of the actual result-item visitor. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTy
 FreeVisitorCore ResultMentionsFold.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque [Nat.of_num_uint FUEL free_type_syms type_requirements reqs].
Module A:=ExecutionModel.Elaboration.
Definition mentions_add(l out:list nat):list nat:=
 fold_left(fun o x=>if nmem x o then o else o++[x])l out.
Definition mention_item(f self mi:nat)(out:list nat)(sp:spec):M(list nat):=

        match sp with
        | Spec (Some p) d a =>
            let bs := opt_default [] a in
            let! bound := mmap (fun b => let! l := resolve_sym mi (fst b) in
                                         ret (match l with Some x => [gs_id x] | None => [] end)) bs in
            let bound := List.concat bound in
            let! rhs := mfold (fun o b => match snd b with
                                         | Spec (Some sp') _ sa => free_type_syms 200 (ATRef sp' sa) mi o
                                         | _ => ret o
                                         end) [] bs in
            let! tg := resolve_sym mi p in
            match tg with
            | None => ret out
            | Some g =>
                let! here :=
                  match d with
                  | Some dn =>
                      let! mo := find_method_decl mi g dn in
                      let! rr := if kind_eqb (gs_kind g) KType then ret [gs_id g] else reqs (gs_id g) in
                      let! ms := match mo with
                                 | Some m => let! rs := reqs (gs_id m) in signature_type_syms m rs
                                 | None => ret []
                                 end in
                      ret (rr ++ ms)
                  | None =>
                      match gs_kind g, gs_decl g with
                      | KCtx, ADCtx _ its => result_mentions_go f self (gs_mod g) its []
                      | KVal, _ => reqs (gs_id g)
                      | KFn, _ => let! rs := reqs (gs_id g) in
                                  let! ms := signature_type_syms g rs in ret (rs ++ ms)
                      (* m4, D84 rule 1: an abstract functor item G mentions X(G) *)
                      | KFunctor, ADFunctor _ _ _ _ None =>
                          if Nat.eqb (gs_id g) self then ret [] else functor_fixed_go f g
                      | _, _ => ret []
                      end
                  end in
                ret (mentions_add (filter (fun x => negb (nmem x bound)) here ++ rhs) out)
            end
        | _ => ret out
        end.
Lemma result_mentions_view:forall f self mi specs out,
 result_mentions_go(S f)self mi specs out=mfold(mention_item f self mi)out specs.
Proof. reflexivity. Qed.
Local Strategy opaque [result_mentions_go functor_fixed_go].
Lemma ok_mention_item:forall f self mi out sp,ok(mention_item f self mi out sp).
Proof. intros;unfold mention_item;destruct sp;okt. Qed.
Section Rhs.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Definition rhs_type(q:path*spec):aty:=
 match snd q with Spec(Some p)_ a=>ATRef p a|_=>ATUnit end.
Theorem result_rhs_rel:forall mi bs out s found s',
 mfold(fun o b=>match snd b with Spec(Some sp)_ sa=>free_type_syms 200(ATRef sp sa)mi o|_=>ret o end)out bs s=Good(found,s')->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->
 forall t,In t found<->In t out\/(exists q,In q bs/\A.FreeIn WW R(gm_path(get_mod w mi))(rhs_type q)t).
Proof.
 intros mi bs out s found s' H Hi Hw Hmi L.
 apply(free_fold w (P:=P) R Fl _
  (fun q o=>match snd q with Spec(Some p)_ a=>free_type_syms 200(ATRef p a)mi o|_=>ret o end)
  rhs_type mi bs)with(s:=s)(s':=s');try assumption.
 - intros [p[pa d a]] o;destruct pa;[apply ok_free_type_syms|apply ok_ret].
 - intros[p[pa d a]]_ o s0 ys s0' E I0 W0 M0 L0.
   destruct pa as[sp|];cbn[rhs_type]in *.
   + exact(free_type_syms_rel w R Fl b Hload 200(ATRef sp a)mi o s0 ys s0' E I0 W0 M0 L0).
   + injection E as <- <-. intro t. split;[intro Ht;left;exact Ht|intros[Ht|Ht];[exact Ht|inversion Ht]].
Qed.
End Rhs.
