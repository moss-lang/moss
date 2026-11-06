(* Exact operational main/platform/output receipts from the real program visitor. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame RuntimeBoolSupport.
Import ListNotations.
Local Open Scope string_scope.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL fn_info].
Definition main_platform_fold(info:fninfo):M(list(item*xdentry)*list string):=
 mfold(fun acc k=>let '(env0,prims):=acc in
   let! o:=platform_entry k prims in
   match o with
   |Some(en,prims')=>check_platform_entry_view(fi_env info)k en;;
      let! it:=key_item_at(fi_env info)k in ret(List.app env0[(it,en)],prims')
   |None=>let! sk:=show_key k in
      fail("unsupported: main assumes "++sk++", which only the bootstrap's native Std provides")
   end)([],[])(fi_needs info).
Definition main_provider_inputs(snapshot:st):M(list(nat*list nat)):=
 mmap(fun p=>let! ts:=runtime_provider_inputs(fst p)in ret(fst p,ts))(st_funs snapshot).
Definition main_runtime_types(e:env)(sc:runtime_schema)(base:nat):M(list(nat*ty)):=
 mmap(fun q=>let! tau:=ety_ty(canon e(snd q))in
 ret(fst q,Audit.app_subst(runtime_machines sc base)tau))(tymap e).
Theorem main_program_trace:forall initial p final,
 elab_program initial=Good(p,final)->
 exists listed contexts library lowered assumes mainid main info infos signature platform prims supplied sc inputstate inputs types,
 check_listed initial=Good(tt,listed)/\check_contexts listed=Good(tt,contexts)/\
 check_runtime_library contexts=Good(tt,library)/\lower_all library=Good(tt,lowered)/\
 check_assumes lowered=Good(tt,assumes)/\
 sassoc "main"(gm_names(get_mod(st_w assumes)(w_entry(st_w assumes))))=Some mainid/\
 sym mainid assumes=Good(main,assumes)/\is_defined_fn main=true/\
 fn_info main assumes=Good(info,infos)/\check_main_signature main info infos=Good(tt,signature)/\
 main_platform_fold info signature=Good((platform,prims),supplied)/\
 st_unsup supplied=[]/\runtime_symbols supplied=Good(sc,supplied)/\
 main_provider_inputs supplied supplied=Good(inputs,inputstate)/\
 main_runtime_types(fi_env info)sc(st_next inputstate)inputstate=Good(types,final)/\
 let '(boolid,trueid,falseid):=runtime_bool_tuple sc in
 p=XProgram(st_fns inputstate)(st_funs inputstate)boolid trueid falseid mainid
   (List.app(xinst[]types)platform)prims(runtime_casts sc(st_next inputstate))inputs[]false[][] .
Proof.
 intros initial p final Run;unfold elab_program in Run.
 apply bind_inv in Run as[u[listed[Listed Run]]];destruct u.
 apply bind_inv in Run as[u[contexts[Contexts Run]]];destruct u.
 apply bind_inv in Run as[u[library[Library Run]]];destruct u.
 apply bind_inv in Run as[u[lowered[Lowered Run]]];destruct u.
 apply bind_inv in Run as[u[assumes[Assumes Run]]];destruct u.
 apply bind_inv in Run as[s0[s1[Read Run]]];injection Read as <- <-.
 destruct(sassoc "main"(gm_names(get_mod(st_w assumes)(w_entry(st_w assumes)))))as[mainid|]eqn:Main;[|discriminate].
 apply bind_inv in Run as[main[s2[Symbol Run]]].
 unfold sym in Symbol;destruct(nget(st_syms assumes)mainid)as[physical|]eqn:At;[|discriminate].
 injection Symbol as <- <-.
 destruct(negb(is_defined_fn physical))eqn:Defined;[discriminate|].
 apply negb_false_iff in Defined.
 apply bind_inv in Run as[info[infos[Info Run]]].
 apply bind_inv in Run as[u[signature[Signature Run]]];destruct u.
 fold(main_platform_fold info)in Run.
 apply bind_inv in Run as[[platform prims][supplied[Platform Run]]].
 apply bind_inv in Run as[s0[s1[Read Run]]];injection Read as <- <-.
 apply bind_inv in Run as[u[s2[Memory Run]]].
 destruct(st_memory_bound supplied&&st_memory_used supplied);[discriminate Memory|injection Memory as <- <-].
 apply bind_inv in Run as[u[s2[Unsupported Run]]].
 destruct(st_unsup supplied)as[|why reasons]eqn:Unsup;[injection Unsupported as <- <-|discriminate Unsupported].
 apply bind_inv in Run as[sc[sruntime[Runtime Run]]].
 pose proof(runtime_symbols_exact supplied)as Identity;rewrite Runtime in Identity.
 injection Identity as Schema Same;subst sruntime.
 fold(main_provider_inputs supplied)in Run.
 destruct(runtime_bool_tuple sc)as[[boolid trueid]falseid]eqn:BoolIds.
 apply bind_inv in Run as[inputs[inputstate[Inputs Run]]].
 apply bind_inv in Run as[s0[s1[Read Run]]];injection Read as <- <-.
 fold(main_runtime_types(fi_env info)sc(st_next inputstate))in Run.
 apply bind_inv in Run as[types[typed[Types Run]]].
 injection Run as <- <-.
 exists listed,contexts,library,lowered,assumes,mainid,physical,info,infos,signature,platform,prims,supplied,sc,inputstate,inputs,types.
 repeat first[assumption|reflexivity|lazymatch goal with |- _ /\ _=>split end].
 - unfold sym;rewrite At;reflexivity.
 - rewrite BoolIds;reflexivity.
Qed.
