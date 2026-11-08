(* Fold small direct calls over LazyX's ordinary environment stream.

   Translation remaps each lookup into the caller's environment. Every source
   in a composed environment must be ordinary: type lookup can observe call
   instantiation, whereas ordinary lookup is unchanged by fcall_env. Checked
   tags and constructs outside the scalar subset keep the original code.

   Before running folded code, gather resolves all lookups, including every
   composition source, into a small shared cache. Missing entries, unsupported
   primitives, failed evaluation and insufficient fuel use the original code
   with the original state. LazyFoldFacts proves exact agreement for all fuel,
   environments and machine states, including reified runtime types. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import ExecutionModel MossX LazyX.
Import ListNotations.
Set Implicit Arguments.
Definition lk := option item.
Definition plan := list(item*lk).
Definition read (d:fenv)(i:lk):option fentry:=match i with Some key=>flookup key d|None=>None end.
Fixpoint remap (pl:plan)(i:item):lk:=match pl with
 |[]=>None|(req,src)::rest=>if item_eqb i req then src else remap rest i end.
Definition remapo (pl:option plan)(i:item):lk:=match pl with None=>Some i|Some ps=>remap ps i end.
Definition remap_plan (pl:option plan)(sm:smap):plan:=map(fun p=>(fst p,remapo pl(snd p)))sm.
Definition ordinary (i:item):bool:=negb(is_ty_item i).
Definition ordinary_sources(sm:smap):bool:=forallb(fun p=>ordinary(snd p))sm.
Definition checks (d:fenv)(pl:plan):bool:=forallb(fun p=>match read d(snd p)with Some _=>true|None=>false end)pl.
Inductive sexpr:=
| SVar(var:var)| SUnit | SVl(i:lk)
| STag(n:nsym)(body:sexpr)
| SPrim(i:lk)(args:list sexpr)
| SCall(guard:plan)(args:list sexpr)(body:sexpr)
| SMatch1(scrutinee:sexpr)(pat:xpat)(body:sexpr).
(* m bounds the translation's structural recursion; n bounds call unfolding. *)
Fixpoint tos(P:xprog)(m n:nat)(pl:option plan)(e:xexpr){struct m}:option sexpr:=
 match m with 0=>None|S m'=>match e with
 |XVar x=>Some(SVar x)|XUnit=>Some SUnit|XVl v=>Some(SVl(remapo pl(IVal v)))
 |XTag t th e=>match lookup_cast t(xp_cast P)with
   |Some _=>None|None=>option_map(STag t)(tos P m' n pl e)end
 |XCallA f args=>option_map(SPrim(remapo pl(IFn f)))(tosl P m' n pl args)
 |XCallD f th sm args=>if ordinary_sources sm then match n with
   |0=>None|S n'=>match xp_fn P f with
     |None=>None|Some body=>let guard:=remap_plan pl sm in
       match tosl P m' n pl args,tos P m' n'(Some guard)body with
       |Some ss,Some sb=>Some(SCall guard ss sb)|_,_=>None end end end else None
 |XMatchP e [(p,b)]=>match tos P m' n pl e,tos P m' n pl b with
   |Some s,Some sb=>Some(SMatch1 s p sb)|_,_=>None end
 |_=>None end end
with tosl(P:xprog)(m n:nat)(pl:option plan)(es:list xexpr){struct m}:option(list sexpr):=
 match m with 0=>None|S m'=>match es with []=>Some[]|e::rest=>
 match tos P m' n pl e,tosl P m' n pl rest with
 |Some s,Some ss=>Some(s::ss)|_,_=>None end end end.
(* Sufficient fuel for the original expression, before unfolding its calls. *)
Fixpoint sdepth(s:sexpr):nat:=match s with
 |SVar _|SUnit|SVl _=>1|STag _ s=>S(sdepth s)
 |SPrim _ ss=>S((fix needed ss:=match ss with []=>1|s::ss=>S(Nat.max(sdepth s)(needed ss))end)ss)
 |SCall _ ss body=>S(Nat.max((fix needed ss:=match ss with []=>1|s::ss=>S(Nat.max(sdepth s)(needed ss))end)ss)(sdepth body))
 |SMatch1 e _ b=>S(Nat.max(sdepth e)(sdepth b))end.
Fixpoint needed(ss:list sexpr):nat:=match ss with []=>1|s::ss=>S(Nat.max(sdepth s)(needed ss))end.
Fixpoint sev(P:xprog)(d:fenv)(g:xvenv)(st:mstate)(s:sexpr){struct s}:option(xvalue*mstate):=
 match s with
 |SVar x=>option_map(fun w=>(w,st))(lookup_xvenv x g)
 |SUnit=>Some(XVUnit,st)
 |SVl i=>match read d i with Some(FVal w)=>Some(w,st)|_=>None end
 |STag n e=>option_map(fun p=>(XVTag n(fst p),snd p))(sev P d g st e)
 |SPrim i args=>
  let ev:=(fix ev args st:=match args with []=>Some([],st)|s::rest=>
   match sev P d g st s with Some(w,st1)=>match ev rest st1 with
   |Some(ws,st2)=>Some(w::ws,st2)|None=>None end|None=>None end end)args st in
  match read d i with
  |Some(FPrim p)=>match ev with Some(ws,st1)=>match run_prim_at P p None ws st1 with
    |(XOk w,st2)=>Some(w,st2)|_=>None end|None=>None end
  |Some(FPrimAt p tau)=>match ev with Some(ws,st1)=>match run_prim_at P p(Some tau)ws st1 with
    |(XOk w,st2)=>Some(w,st2)|_=>None end|None=>None end
  |_=>None end
 |SCall guard args body=>if checks d guard then
   match (fix ev args st:=match args with []=>Some([],st)|s::rest=>
    match sev P d g st s with Some(w,st1)=>match ev rest st1 with
    |Some(ws,st2)=>Some(w::ws,st2)|None=>None end|None=>None end end)args st with
   |Some(ws,st1)=>sev P d(mk_xvenv ws)st1 body|None=>None end else None
 |SMatch1 e pat b=>match sev P d g st e with Some(w,st1)=>match match_pat pat w with
   |PMatch bs=>sev P d(bs++g)st1 b|_=>None end|None=>None end
 end.
Definition sevl P d g:list sexpr->mstate->option(list xvalue*mstate):=
 fix ev ss st:=match ss with
 |[]=>Some([],st)|s::rest=>match sev P d g st s with Some(w,st1)=>
   match ev rest st1 with Some(ws,st2)=>Some(w::ws,st2)|None=>None end
 |None=>None end end.

Fixpoint mkv_go(i:nat)(ws:list xvalue):xvenv:=match ws with
 |[]=>[]|w::r=>(i,w)::mkv_go(S i)r end.
Definition mkv:=mkv_go 0.
(* Cache keys include the composition guards even when a body never reads
   those entries. This preserves composition failure and permits kc to omit
   repeated presence checks once gather succeeds. *)
Definition lk_eqb(a b:lk):bool:=match a,b with
 |None,None=>true|Some i,Some j=>item_eqb i j|_,_=>false end.
Fixpoint reads(s:sexpr):list lk:=match s with
 |SVar _|SUnit=>[]|SVl i=>[i]|STag _ s=>reads s
 |SPrim i args=>i::flat_map reads args
 |SCall guard args b=>map snd guard++flat_map reads args++reads b
 |SMatch1 e _ b=>reads e++reads b end.
Definition read_keys(s:sexpr):list lk:=
 fold_right(fun key acc=>if existsb(lk_eqb key)acc then acc else key::acc)[](reads s).
Fixpoint key_pos(i:lk)(keys:list lk):option nat:=match keys with
 |[]=>None|j::rest=>if lk_eqb i j then Some 0 else option_map S(key_pos i rest)end.
Definition cache_lookup(keys:list lk)(i:lk):list fentry->option fentry:=
 match key_pos i keys with Some n=>fun entries=>nth_error entries n|None=>fun _=>None end.
Fixpoint gather(d:fenv)(keys:list lk):option(list fentry):=match keys with
 |[]=>Some[]|i::rest=>match read d i,gather d rest with
 |Some en,Some tail=>Some(en::tail)|_,_=>None end end.
Definition kcode:=list fentry->xvenv->mstate->option(xvalue*mstate).
Definition klcode:=list fentry->xvenv->mstate->option(list xvalue*mstate).
Definition knil:klcode:=fun _ _ st=>Some([],st).
Definition kcons(c:kcode)(rest:klcode):klcode:=fun slots g st=>
 match c slots g st with Some(w,st1)=>match rest slots g st1 with
 |Some(ws,st2)=>Some(w::ws,st2)|None=>None end|None=>None end.
(* Bind child code before building its parent closure. This also ensures the
   extracted, inlined kcons shares compilation work between invocations. *)
Fixpoint kc(P:xprog)(keys:list lk)(e:sexpr){struct e}:kcode:=match e with
 |SVar x=>fun _ g st=>option_map(fun w=>(w,st))(lookup_xvenv x g)
 |SUnit=>fun _ _ st=>Some(XVUnit,st)
 |SVl i=>let get:=cache_lookup keys i in fun slots _ st=>match get slots with Some(FVal w)=>Some(w,st)|_=>None end
 |STag n e=>let c:=kc P keys e in fun slots g st=>option_map(fun p=>(XVTag n(fst p),snd p))(c slots g st)
 |SPrim i args=>let get:=cache_lookup keys i in
  let ca:=fold_right(fun e cr=>let c:=kc P keys e in kcons c cr)knil args in
  fun slots g st=>match get slots with
  |Some(FPrim p)=>match ca slots g st with Some(ws,st1)=>match run_prim_at P p None ws st1 with
   |(XOk w,st2)=>Some(w,st2)|_=>None end|None=>None end
  |Some(FPrimAt p tau)=>match ca slots g st with Some(ws,st1)=>match run_prim_at P p(Some tau)ws st1 with
   |(XOk w,st2)=>Some(w,st2)|_=>None end|None=>None end
  |_=>None end
 |SCall guard args body=>
  let ca:=fold_right(fun e cr=>let c:=kc P keys e in kcons c cr)knil args in let cb:=kc P keys body in
  fun slots g st=>match ca slots g st with
  |Some(ws,st1)=>cb slots(mkv ws)st1|None=>None end
 |SMatch1 e p b=>let ce:=kc P keys e in let cb:=kc P keys b in fun slots g st=>
  match ce slots g st with Some(w,st1)=>match match_pat p w with
  |PMatch bs=>cb slots(bs++g)st1|_=>None end|None=>None end end.
Definition code:=nat->fenv->xvenv->mstate->xres.
Definition fold_call(P:xprog)(e:xexpr)(fallback:code):code:=
 match tos P 48 6 None e with
 |None=>fallback
 |Some s=>let depth:=sdepth s in let keys:=read_keys s in let fast:=kc P keys s in
  fun n d g st=>if Nat.leb depth n then match gather d keys with
  |Some slots=>match fast slots g st with
   |Some(w,st1)=>XR(XOk w)g st1|None=>fallback n d g st end
  |None=>fallback n d g st end else fallback n d g st end.
