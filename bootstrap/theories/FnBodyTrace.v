(* Exact stages of a successful, previously absent source body. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound WorldSound ElabFrame FnBodyParameters.
Import ListNotations.
Local Open Scope string_scope.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL fn_info fn_sig nominal_args].
Fixpoint body_parameter_duplicate(names:list string):option string:=
 match names with[]=>None|n::rest=>if smem n rest then Some n else body_parameter_duplicate rest end.
Lemma body_parameter_duplicate_none:forall names,body_parameter_duplicate names=None->NoDup names.
Proof.
 induction names as[|n names IH];intro E;[constructor|]. cbn[body_parameter_duplicate]in E.
 destruct(smem n names)eqn:M;[discriminate|]. constructor;[exact(smem_false _ _ M)|apply IH;exact E].
Qed.
Ltac body_trace_noerr H:=exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(? &? &? &H)])].
Definition body_receiver(g:gsym)(e:env):M(option ety):=
 match gs_recv g with
 |Some r=>let! rg:=sym r in
   if kind_eqb(gs_kind rg)KUnit||kind_eqb(gs_kind rg)KTag then
     let! xs:=nominal_args(gs_mod g)rg(tymap e)in ret(Some(YNom r xs))
   else err(gs_mod g)("attached method `"++gs_name g++"` requires a nominal receiver (Q5); `"++gs_name rg++"` is abstract")
 |None=>ret None end.
Definition finish_function_body(g:gsym)(e:env)(needs:list key)(sg:fsig)(this:option ety)(x:xexpr):M unit:=
 (match this with Some t=>layout_ok e t|None=>ret tt end);;
 miter(fun p=>layout_ok e(snd p))(sg_params sg);;
 layout_ok e(sg_ret sg);;
 miter(fun k=>match k with KyVal v=>match nget(en_vals e)v with Some t=>layout_ok e t|None=>ret tt end|_=>ret tt end)needs;;
 emit_fn(gs_id g)x.
Lemma ok_finish_function_body:forall g e needs sg this x,ok(finish_function_body g e needs sg this x).
Proof. intros;unfold finish_function_body;okt. Qed.
Theorem lower_body_stages:forall fuel g s final b,
 fn_body g=Some b->nassoc(gs_id g)(st_fns s)=None->
 lower_body fuel g s=Good(tt,final)->
 exists f info si this sr sg ss marker registered x t bt bodyFinal,
 fuel=S f/\fn_info g s=Good(info,si)/\
 body_receiver g(fi_env info)si=Good(this,sr)/\
 fn_sig g(tymap(fi_env info))(fi_env info)this sr=Good(sg,ss)/\
 NoDup(map fst(body_parameter_slots this(map fst(sg_params sg))))/\
 emit_fn(gs_id g)XUnit ss=Good(tt,marker)/\
 register_reify_code(ROFunction(gs_id g))
   (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))
   (fi_needs info)(Some sg)marker=Good(tt,registered)/\
 in_reify_code(ROFunction(gs_id g))
  (with_vars(body_parameter_slots this(map fst(sg_params sg)))
   (synth_block f(Cx(gs_mod g)sg this 0[]""[][])
    (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))
    b(Some(sg_ret sg))))registered=Good((x,t,bt),bodyFinal)/\
 finish_function_body g
 (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))
 (fi_needs info)sg this x bodyFinal=Good(tt,final)/\
 nassoc(gs_id g)(st_fns final)=Some x.
Proof.
 intros[|f]g s final b HasBody Absent E;[discriminate|].
 cbn[lower_body]in E;apply bind_inv in E as[done[s0[Lowered E]]].
 cbn[lowered mbind get ret]in Lowered;rewrite Absent in Lowered;cbn[is_some]in Lowered;injection Lowered as <- <-.
 rewrite HasBody in E.
 apply bind_inv in E as[info[si[Info E]]].
 fold(body_receiver g(fi_env info))in E.
 apply bind_inv in E as[this[sr[Receiver E]]].
 apply bind_inv in E as[sg[ss[Signature E]]].
 fold body_parameter_duplicate in E.
 set(vars:=body_parameter_slots this(map fst(sg_params sg)))in *.
 assert(Len:List.length(map fst(sg_params sg))=List.length(sg_params sg))by apply map_length.
 unfold body_parameter_slots in vars.
 rewrite <-Len in E.
 fold vars in E.
 destruct(body_parameter_duplicate(map fst vars))as[bad|]eqn:Dup;[body_trace_noerr E|].
 apply bind_inv in E as[u[marker[Marker E]]].
 destruct u.
 apply bind_inv in E as[u[registered[Register E]]].
 destruct u.
 apply bind_inv in E as[[[x t]bt][bodyFinal[Body E]]].
 fold(finish_function_body g
 (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params sg)(fi_env info))
 (fi_needs info)sg this x)in E.
 exists f,info,si,this,sr,sg,ss,marker,registered,x,t,bt,bodyFinal.
 repeat split;try assumption;try reflexivity.
 - exact(body_parameter_duplicate_none _ Dup).
 - unfold finish_function_body in E;repeat apply bind_inv in E as[?[?[? E]]].
   unfold emit_fn,upd in E;injection E as <-;cbn[s_fns st_fns nassoc assoc];rewrite Nat.eqb_refl;reflexivity.
Qed.
