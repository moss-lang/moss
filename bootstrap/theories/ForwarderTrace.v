(* Actual allocation and lowering traces for synthetic abstract-fn providers. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame.
Import ListNotations.
Local Open Scope string_scope.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Definition forwarder_decl(tg:gsym):adecl:=
 let ps:=fn_params tg in
 let names:=map(fun i=>"%"++nat_to_string i)(seq 0(List.length ps))in
 let call:=AECall[match gs_decl tg with ADFn _ n _ _ _=>n|_=>gs_name tg end]None
   (map(fun n=>AEPath[n]None)names)in
 ADFn RPlain(gs_name tg)(combine names(map snd ps))(fn_ret tg)(Some(ABlock[](Some call))).
Lemma new_sym_shape:forall mi name kind decl orig s id final,
 new_sym mi name kind decl orig s=Good(id,final)->
 id=st_next s /\st_next final=S id /\
 nget(st_syms final)id=Some(GSym id mi name kind decl None [] orig).
Proof.
 intros mi name kind decl orig s id final E.
 cbn[new_sym mbind fresh upd ret]in E;injection E as <- <-.
 split;[reflexivity|]. split;[reflexivity|]. cbn[s_syms st_syms]. rewrite nget_nset,Nat.eqb_refl;reflexivity.
Qed.
Theorem forwarder_miss_trace:forall fuel tg s g final,
 nget(st_fwd s)(gs_id tg)=None ->forwarder fuel tg s=Good(g,final)->
 exists f id allocated cached ready,
 fuel=S f /\id=st_next s /\
 new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))s=Good(id,allocated)/\
 upd(s_fwd(fun m=>nset m(gs_id tg)id))allocated=Good(tt,cached)/\
 sym id cached=Good(g,ready)/\lower_body f g ready=Good(tt,final).
Proof.
 intros[|f]tg s g final Missing E;[discriminate|]. cbn[forwarder]in E.
 apply bind_inv in E as[initial[s0[Read E]]];injection Read as <- <-.
 rewrite Missing in E.
 apply bind_inv in E as[id[allocated[New E]]].
 apply bind_inv in E as[u[cached[Cache E]]]. destruct u.
 apply bind_inv in E as[got[ready[Sym E]]].
 apply bind_inv in E as[u[last[Lower E]]]. destruct u;injection E as <- <-.
 exists f,id,allocated,cached,ready. split;[reflexivity|].
 split;[exact(proj1(new_sym_shape _ _ _ _ _ _ _ _ New))|].
 split;[exact New|]. split;[exact Cache|]. split;assumption.
Qed.
Theorem forwarder_hit_trace:forall fuel tg s g final id,
 nget(st_fwd s)(gs_id tg)=Some id ->forwarder fuel tg s=Good(g,final)->
 sym id s=Good(g,final).
Proof.
 intros[|f]tg s g final id Hit E;[discriminate|]. cbn[forwarder]in E.
 apply bind_inv in E as[initial[s0[Read E]]];injection Read as <- <-.
 rewrite Hit in E;exact E.
Qed.

(* A miss allocates exactly the promised synthetic declaration. *)
Theorem forwarder_miss_symbol:forall fuel tg s g final,
 nget(st_fwd s)(gs_id tg)=None ->forwarder fuel tg s=Good(g,final)->
 gs_id g=st_next s /\gs_origin g=OForwarder(gs_id tg)/\gs_decl g=forwarder_decl tg/\
 exists f ready,lower_body f g ready=Good(tt,final)/\st_next ready=S(gs_id g).
Proof.
 intros fuel tg s g final Missing Run.
 destruct(forwarder_miss_trace _ _ _ _ _ Missing Run)as(f&id&allocated&cached&ready&Ef&Id&New&Cache&Sym&Lower).
 destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id0[Next Lookup]].
 unfold upd in Cache;injection Cache as <-.
 unfold sym in Sym;cbn[s_fwd st_syms]in Sym;rewrite Lookup in Sym;injection Sym as <- <-.
 cbn[gs_id gs_origin gs_decl]. split;[exact Id|]. split;[reflexivity|]. split;[reflexivity|].
 exists f,(s_fwd(fun m=>nset m(gs_id tg)id)allocated). split;[exact Lower|exact Next].
Qed.
