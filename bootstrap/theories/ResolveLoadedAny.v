(* Every successful symbol resolution in a real loader world is loaded,
   even without assuming the queried module index was valid in advance. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL nget nset].
Lemma resolve_found_module:forall w mi pa key,
 resolve_path w mi pa=Good(Some(ESym key))->exists m,nget(w_mods w)mi=Some m.
Proof.
 intros w mi pa key E;unfold resolve_path,get_mod in E.
 destruct(nget(w_mods w)mi)as[m|]eqn:Mod;[exists m;reflexivity|].
 destruct pa;cbn[opt_default gm_names gm_aliases sassoc assoc]in E;discriminate.
Qed.
Theorem resolve_path_loaded_any:forall b w mi pa key,
 load_world b=Good w->resolve_path w mi pa=Good(Some(ESym key))->
 exists g,nget(w_syms w)key=Some g.
Proof.
 intros b w mi pa key Load Resolve.
 destruct(resolve_found_module _ _ _ _ Resolve)as[m Mod].
 unfold load_world in Load;apply rbind_good in Load as[[entry st][Run End]];injection End as <-.
 destruct(load_spec b _ _ _ _ _ _ Run(linv_init b))as[LI _].
 destruct(li_mods _ _ LI mi m Mod)as[p Path].
 pose proof(resolve_path_agrees b st entry LI(fun _=>[])(fun _=>0)p mi m pa(Some(ESym key))Path Mod Resolve)as R.
 cbn[option_map conv_ent]in R.
 exact(resolved_loader b st entry LI(fun _=>[])(fun _=>0)p pa key R).
Qed.
Theorem resolve_sym_loaded_any:forall b w mi pa s g final,
 load_world b=Good w->Inv s->st_w s=w->
 resolve_sym mi pa s=Good(Some g,final)->final=s /\nget(w_syms w)(gs_id g)=Some g.
Proof.
 intros b w mi pa s g final Load I World E;unfold resolve_sym in E.
 apply bind_inv in E as[found[mid[Path E]]].
 unfold rpath in Path;rewrite World in Path.
 destruct(resolve_path w mi pa)as[r|err]eqn:Resolve;[injection Path as <- <-|discriminate].
 destruct r as[[key|moduleid]|];try discriminate E.
 apply bind_inv in E as[got[last[Sym E]]];injection E as <- <-.
 unfold sym in Sym.
 destruct(resolve_path_loaded_any _ _ _ _ _ Load Resolve)as[loaded Lookup].
 assert(Available:nget(st_syms s)key=Some loaded)by(eapply iv_loader;[exact I|rewrite World;exact Load|rewrite World;exact Lookup]).
 rewrite Available in Sym;injection Sym as <- <-;split;[reflexivity|].
 rewrite(iv_ids s I key loaded Available);exact Lookup.
Qed.
Lemma loaded_abstract_from_kind:forall b w g,
 load_world b=Good w->nget(w_syms w)(gs_id g)=Some g->
 gs_kind g=KFn->fn_body g=None->exists n ps rt,gs_decl g=ADFn RPlain n ps rt None.
Proof.
 intros b w g Load Loaded Kind NoBody;unfold load_world in Load.
 apply rbind_good in Load as[[entry st][Run End]];injection End as <-.
 destruct(load_spec b _ _ _ _ _ _ Run(linv_init b))as[LI _].
 pose proof(li_kind _ _ LI _ _ Loaded)as K;unfold KindOK in K;rewrite Kind in K.
 unfold fn_body in NoBody;destruct(gs_decl g);cbn[ExecutionModel.Elaboration.kind_of]in K;try discriminate K.
 destruct f;try discriminate K. destruct o0;[discriminate NoBody|]. repeat eexists;reflexivity.
Qed.
