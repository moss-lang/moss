(* A successful real cache miss produces a completed, fresh forwarder body. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv FnReadonly
 FnBound ForwarderTrace ForwarderLowerCode.
Import ListNotations.
Local Open Scope string_scope.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Lemma combine_parameter_names:forall A(names:list string)(types:list A),
 List.length names=List.length types->map fst(combine names types)=names.
Proof. intros A names;induction names;intros[|t types]Len;cbn[List.length]in Len;try discriminate;[reflexivity|].
 cbn[combine map fst];f_equal;apply IHnames;lia. Qed.
Theorem forwarder_miss_completed:forall b w fuel target tn ps rt s g final,
 load_world b=Good w->nget(w_syms w)(gs_id target)=Some target->
 gs_decl target=ADFn RPlain tn ps rt None->gs_kind target=KFn->
 Inv s->FnBound s->st_w s=w->nget(st_fwd s)(gs_id target)=None->
 forwarder fuel target s=Good(g,final)->
 gs_id g=st_next s /\gs_origin g=OForwarder(gs_id target)/\gs_id g<st_next final /\
 nassoc(gs_id g)(st_fns final)=Some(XCallA(gs_id target)(map XVar(seq 0(List.length(fn_params g))))).
Proof.
 intros b w fuel target tn ps rt s g final Load Loaded TargetDecl TargetKind I Bnd World Missing Run.
 destruct(forwarder_miss_trace _ _ _ _ _ Missing Run)as[f[id[allocated[cached[ready[Fuel[Id[New[Cache[Sym Lower]]]]]]]]]].
 assert(Onew:ok(new_sym(gs_mod target)(gs_name target)KFn(forwarder_decl target)(OForwarder(gs_id target))))by okt.
 destruct(Onew _ _ _ I New)as[IA FA].
 destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id0[Next Lookup]].
 assert(Fns:st_fns allocated=st_fns s).
 {assert(Kf:keeps st_fns(new_sym(gs_mod target)(gs_name target)KFn(forwarder_decl target)(OForwarder(gs_id target))))by(unfold new_sym;keepst).
  exact(Kf _ _ _ New). }
 unfold upd in Cache;injection Cache as <-.
 unfold sym in Sym;cbn[s_fwd st_syms]in Sym;rewrite Lookup in Sym;injection Sym as <- <-.
 set(provider:=GSym id(gs_mod target)(gs_name target)KFn(forwarder_decl target)None[](OForwarder(gs_id target)))in *.
 set(start:=s_fwd(fun m=>nset m(gs_id target)id)allocated)in *.
 assert(IS:Inv start)by(unfold start;destruct IA;constructor;assumption).
 assert(WS:st_w start=w)by(unfold start;cbn[s_fwd st_w];rewrite(fr_w _ _ FA);exact World).
 assert(Absent:nassoc id(st_fns start)=None).
 {unfold start;cbn[s_fwd st_fns];rewrite Fns;eapply fn_bound_fresh_absent;[exact Bnd|lia]. }
 set(names:=map(fun i=>"%"++nat_to_string i)(seq 0(List.length ps))).
 assert(Decl:gs_decl provider=ADFn RPlain(gs_name target)(combine names(map snd ps))rt
  (Some(ABlock[](Some(AECall[tn]None(map(fun n=>AEPath[n]None)names)))))).
 {unfold provider;cbn[gs_decl];unfold forwarder_decl,fn_params,fn_ret;rewrite TargetDecl;reflexivity. }
 assert(Names:map fst(combine names(map snd ps))=names).
 {apply combine_parameter_names;unfold names;rewrite map_length,seq_length,map_length;reflexivity. }
 pose proof(lower_forwarder_code b w f provider target tn ps rt(gs_name target)(combine names(map snd ps))rt names
  start final Load Loaded TargetDecl TargetKind Decl Names eq_refl eq_refl IS WS Absent Lower)as Code.
 destruct(ok_lower_body f provider _ _ _ IS Lower)as[IF FF].
 assert(Below:id<st_next final).
 {pose proof(fr_next _ _ FF);unfold start in H;cbn[s_fwd st_next]in H;rewrite Next in H;lia. }
 split;[exact Id|]. split;[reflexivity|]. split;[exact Below|].
 unfold fn_params;rewrite Decl;exact Code.
Qed.
