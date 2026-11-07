(* A loaded plain function's declared name resolves in its home module. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL nget nset].
Lemma loaded_plain_home_name:forall b w g n ps rt body,
 load_world b=Good w->nget(w_syms w)(gs_id g)=Some g->
 gs_decl g=ADFn RPlain n ps rt body->
 sassoc n(gm_names(get_mod w(gs_mod g)))=Some(gs_id g).
Proof.
 intros b w g n ps rt body Load Loaded Decl;unfold load_world in Load.
 apply rbind_good in Load as[[entry st][Run End]];injection End as <-.
 destruct(load_spec b _ _ _ _ _ _ Run(linv_init b))as[LI _].
 destruct(li_symfacts _ _ LI _ _ Loaded)as[m[p[am[Mod[Path[Module[Occurs[Kind[Named More]]]]]]]]].
 rewrite(fin_get_mod b st entry(gs_mod g)m Mod).
 apply Named;rewrite Decl;reflexivity.
Qed.
Lemma loaded_plain_home_rpath:forall b w g n ps rt body s,
 load_world b=Good w->nget(w_syms w)(gs_id g)=Some g->
 gs_decl g=ADFn RPlain n ps rt body->st_w s=w->
 rpath(gs_mod g)[n]s=Good(Some(ESym(gs_id g)),s).
Proof.
 intros b w g n ps rt body s Load Loaded Decl World.
 pose proof(loaded_plain_home_name _ _ _ _ _ _ _ Load Loaded Decl)as Name.
 unfold rpath;rewrite World;unfold resolve_path;rewrite Name;reflexivity.
Qed.
