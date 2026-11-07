(* Do not demand interfaces of latent unused source providers. Keep actual
   profiles of reachable code and the marked constant occurrences it reads. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss Elab ReifyData Reify ReifySource ReifyReach
  ReifyFunctors ReifyFunctorProfiles ReifyProfileGraph ReifyOpaqueFunctors.
Import ListNotations.
Fixpoint expr_constant_occurs (wanted:vsym)(e:xexpr):bool:=
 match e with
 | XConst name body=>Nat.eqb wanted name||expr_constant_occurs wanted body
 | XVar _|XUnit|XVl _|XBreak=>false
 | XTag _ _ b|XBindTy _ _ b|XAssign _ b|XLoop b|XReturn b|XField b _
 | XBindFn _ _ _ b|XBindMth _ _ _ _ _ b|XBindFun _ _ _ _ b|XBindFunA _ _ _ b
 | XApply _ _ _ b|XApplyR _ _ _ b|XBindFunSym _ _ b|XBindFunCopy _ _ b|XBindPrim _ _ b=>expr_constant_occurs wanted b
 | XCallD _ _ _ args|XCallA _ args=>existsb(expr_constant_occurs wanted)args
 | XMeth receiver _ _ args=>expr_constant_occurs wanted receiver||existsb(expr_constant_occurs wanted)args
 | XMatch scrutinee arms=>expr_constant_occurs wanted scrutinee||
     existsb(fun a=>expr_constant_occurs wanted(snd a))arms
 | XLet _ a b|XBindVal _ a b|XSeq a b|XWhile a b=>expr_constant_occurs wanted a||expr_constant_occurs wanted b
 | XIf a b c=>expr_constant_occurs wanted a||expr_constant_occurs wanted b||expr_constant_occurs wanted c
 | XRec fields=>existsb(fun p=>expr_constant_occurs wanted(snd p))fields
 | XMatchP scrutinee arms=>expr_constant_occurs wanted scrutinee||
     existsb(fun a=>expr_constant_occurs wanted(snd a))arms
 | XApplyBr _ arms body=>expr_constant_occurs wanted body||
     existsb(fun a=>existsb(bind_constant_occurs wanted)(snd(fst a)))arms
 end
with bind_constant_occurs(wanted:vsym)(b:xbind):bool:=
 match b with
 | XBVal _ e=>expr_constant_occurs wanted e
 | XBApplyBr _ arms=>existsb(fun a=>existsb(bind_constant_occurs wanted)(snd(fst a)))arms
 | _=>false end.
Definition node_constant_occurs(raw:xprogram)(wanted:vsym)(node:reify_node):bool:=
 match node with
 | RnFunction f=>match reify_lookup_function f(xo_fns raw)with
     Some body=>expr_constant_occurs wanted body|None=>false end
 | RnFunctor h=>match reify_lookup_functor h(xo_funs raw)with
     Some(_,body)=>existsb(bind_constant_occurs wanted)body|None=>false end end.
Definition node_functor_profiles(source:st)(node:reify_node):list(usym*reify_exports ety key):=
 match node with
 | RnFunction f=>match assoc reify_owner_eqb(ROFunction f)(rm_codes(st_reify source))with
     Some code=>code_functor_profiles code|None=>[]end
 | RnFunctor h=>match reify_code_for_symbol source h with
     Some code=>code_functor_profiles code|None=>[]end end.
Definition supported_functor_profiles(source:st)(raw:xprogram):list(usym*reify_exports ety key):=
 let nodes:=reify_reachable_nodes raw in
 flat_map(node_functor_profiles source)nodes++
 flat_map(fun p=>if existsb(node_constant_occurs raw(fst p))nodes
   then flat_map event_functor_profile(snd p)else[])(rm_constants(st_reify source)).
Definition node_scope_functors(source:st)(node:reify_node):list usym:=
 let code:=match node with
 | RnFunction f=>assoc reify_owner_eqb(ROFunction f)(rm_codes(st_reify source))
 | RnFunctor h=>reify_code_for_symbol source h end in
 match code with
 | Some code=>flat_map(fun k=>match k with KyFun u=>[u]|_=>[]end)
     (rs_items ety key(rc_scope ety key code))
 | None=>[]end.
Definition prepare_supported_functor_templates(source:st)(raw:xprogram)(defaults:subst)
 (before:reify_state):option(functor_work*reify_state):=
 match complete_interface_graph source
   (flat_map(node_scope_functors source)(reify_reachable_nodes raw))
   (supported_functor_profiles source raw)with
 | Some(profiles,opaque)=>prepare_templates_with_opaque source defaults profiles opaque before
 | None=>None end.
