(* The temporary result roles come from actual distinct symbol allocations. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ForwarderTrace.
Import ListNotations.
Local Open Scope string_scope.
Definition link_output_allocate(fg:gsym)(ts:list nat):M(list(nat*nat)):=
 mmap(fun t=>let!tg:=sym t in
 let!g:=new_sym(gs_mod tg)(gs_name tg++"@result")KType(ADType(gs_name tg))(OFresh t(gs_id fg))in ret(t,g))ts.
Lemma link_output_allocate_rows:forall fg ts s roles final,
 link_output_allocate fg ts s=Good(roles,final)->
 map fst roles=ts /\ NoDup(map snd roles) /\
 (forall t g,In(t,g)roles->st_next s<=g<st_next final) /\ st_next s<=st_next final.
Proof.
 intros fg ts;induction ts as[|t ts IH];intros s roles final Run.
 - injection Run as <- <-;split;[reflexivity|];split;[constructor|];split;[intros t g Hit;contradiction|lia].
 - unfold link_output_allocate in Run;cbn[mmap]in Run.
   apply bind_inv in Run as[p[allocated[First Run]]].
   apply bind_inv in Run as[rest[tail[Rest Run]]];injection Run as <- <-.
   apply bind_inv in First as[tg[read[Read First]]].
   unfold sym in Read;destruct(nget(st_syms s)t)as[found|]eqn:Lookup;[|discriminate].
   injection Read as <- <-.
   apply bind_inv in First as[g[fresh[New First]]];injection First as <- <-.
   destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id[Next Symbol]].
   destruct(IH fresh rest tail Rest)as[Keys[Distinct[Bounds Mono]]].
   cbn[map fst snd];split;[rewrite Keys;reflexivity|];split.
   + constructor;[|exact Distinct]. intro Hit;apply in_map_iff in Hit as[[owner id][Eq Hit]].
     cbn[fst snd]in Eq;subst id;pose proof(Bounds owner g Hit);lia.
   + split.
     * intros owner id[Eq|Hit];[injection Eq as <- <-;lia|].
       pose proof(Bounds owner id Hit);lia.
     * lia.
Qed.
Lemma fresh_target_output_phases:forall e fg s promised keys roles final,
 fresh_target_output_view e fg s=Good((promised,keys,roles),final)->
 exists ts typed allocated provided,
 result_types fg s=Good(ts,typed) /\
 link_output_allocate fg ts typed=Good(roles,allocated) /\
 provide_result(target_input_types e)fg roles allocated=Good((promised,keys),provided) /\
 final=provided /\ map fst roles=ts /\ NoDup(map snd roles) /\
 (forall t g,In(t,g)roles->st_next typed<=g<st_next allocated).
Proof.
 intros e fg s promised keys roles final Run;unfold fresh_target_output_view in Run.
 apply bind_inv in Run as[ts[typed[Types Run]]].
 apply bind_inv in Run as[allocatedroles[allocated[Allocate Run]]].
 apply bind_inv in Run as[[view out][provided[Provide Run]]];injection Run as <- <- <- <-.
 destruct(link_output_allocate_rows fg ts typed allocatedroles allocated Allocate)as[Keys[Distinct[Bounds Mono]]].
 exists ts,typed,allocated,provided;repeat first[assumption|reflexivity|lazymatch goal with |- _ /\ _=>split end].
Qed.
