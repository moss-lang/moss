(* Actual checked runtime selections read the source Bool namespace exactly. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame LibraryFacts RuntimeBoolSupport.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Open Scope string_scope.
Lemma selected_bool_scope:forall w R Fl p ns n x,
 A.LibraryModule(W_read w R Fl)"bool"p->
 A.w_scope(W_read w R Fl)p ns n=Some(A.ESym x)->
 exists mi,sassoc "bool"(lang_modules w)=Some mi/\p=gm_path(get_mod w mi)/\
 sassoc n(tbl(get_mod w mi)false ns)=Some x.
Proof.
 intros w R Fl p ns n x Library Scope.
 unfold A.LibraryModule in Library;cbn[A.w_library W_read]in Library.
 destruct(w_library w)as[dir|]eqn:Lib;[|contradiction];subst p.
 cbn[A.w_scope W_read]in Scope.
 destruct(find_mod w(String.append dir(String.append "bool" ".moss"))(w_order w))as[gm|]eqn:Found;[|discriminate].
 destruct(lookup_selected_module w dir "bool"(w_order w)gm Lib ltac:(cbn;auto 10)Found)as(mi&Selected&Module&Path).
 exists mi;split;[unfold lang_modules;rewrite Lib;exact Selected|].
 rewrite Module;split;[symmetry;exact Path|].
 destruct ns;cbn[tbl aent]in Scope|-*.
 - destruct(sassoc n(gm_names gm));[injection Scope as <-;reflexivity|discriminate].
 - destruct(sassoc n(gm_detached gm));[injection Scope as <-;reflexivity|discriminate].
 - destruct(sassoc n(gm_aliases gm));discriminate.
Qed.
Lemma runtime_bool_langsym:forall b w R Fl s name symbol,
 load_world b=Good w->st_w s=w->st_lang s=lang_modules w->
 (runtime_symbol_value s("bool",name)=Some symbol<->A.LangSym(W_read w R Fl)"bool"A.NSName name symbol).
Proof.
 intros b w R Fl s name symbol Load World Lang;split.
 - unfold runtime_symbol_value;cbn[fst snd];rewrite World,Lang.
   destruct(sassoc "bool"(lang_modules w))as[mi|]eqn:Selected;[|discriminate].
   intro Named;exact(lang_symbol_scope b w R Fl Load "bool"false name mi symbol Selected Named).
 - intros(d&p&Decl&Library&Scope).
   destruct(selected_bool_scope w R Fl p A.NSName name symbol Library Scope)as(mi&Selected&Path&Named).
   unfold runtime_symbol_value;cbn[fst snd];rewrite World,Lang,Selected;exact Named.
Qed.
Definition runtime_bool_shape(sc:runtime_schema):Prop:=
 runtime_id sc "bool" "Bool"=None\/
 exists b t f,runtime_id sc "bool" "Bool"=Some b/\runtime_id sc "bool" "True"=Some t/\runtime_id sc "bool" "False"=Some f.
Lemma checked_runtime_bool_shape:forall sc s sf,
 check_runtime_bool sc s=Good(tt,sf)->runtime_bool_shape sc.
Proof.
 intros sc s sf Run;unfold runtime_bool_shape.
 unfold check_runtime_bool in Run;destruct(runtime_id sc "bool" "Bool")as[b|]eqn:B;[right|left;reflexivity].
 apply bind_inv in Run as[g[s1[Symbol Run]]].
 apply bind_inv in Run as[rs[s2[Reqs Run]]].
 destruct(runtime_id sc "bool" "True")as[t|]eqn:T;
 destruct(runtime_id sc "bool" "False")as[f|]eqn:F.
 - exists b,t,f;auto.
 - destruct rs;exfalso;eapply err_not_good;exact Run.
 - destruct rs;exfalso;eapply err_not_good;exact Run.
 - destruct rs;exfalso;eapply err_not_good;exact Run.
Qed.
Lemma runtime_true_id:forall s,
 runtime_id(map(fun q=>(q,runtime_symbol_value s q))runtime_names)"bool" "True"=runtime_symbol_value s("bool","True").
Proof. reflexivity. Qed.
Lemma runtime_false_id:forall s,
 runtime_id(map(fun q=>(q,runtime_symbol_value s q))runtime_names)"bool" "False"=runtime_symbol_value s("bool","False").
Proof. reflexivity. Qed.
Theorem runtime_bool_source_contract:forall b w R Fl s sc,
 load_world b=Good w->st_w s=w->st_lang s=lang_modules w->
 runtime_symbols s=Good(sc,s)->runtime_bool_shape sc->
 let '(bid,tid,fid):=runtime_bool_tuple sc in
 (A.LangSym(W_read w R Fl)"bool"A.NSName "Bool"bid/\
  A.LangSym(W_read w R Fl)"bool"A.NSName "True"tid/\
  A.LangSym(W_read w R Fl)"bool"A.NSName "False"fid)\/
 ((forall x,~A.LangSym(W_read w R Fl)"bool"A.NSName "Bool"x)/\bid=0/\tid=0/\fid=0).
Proof.
 intros b w R Fl s sc Load World Lang Runtime Shape.
 rewrite runtime_symbols_exact in Runtime;injection Runtime as <-.
 destruct Shape as[Missing|(bid&tid&fid&B&T&F)];unfold runtime_bool_tuple.
 - rewrite Missing;right;split;[|auto].
   intros x Named;pose proof(proj2(runtime_bool_langsym b w R Fl s "Bool"x Load World Lang)Named)as Selected.
   change(runtime_symbol_value s("bool","Bool")=None)in Missing;congruence.
 - rewrite B,T,F;left;repeat split.
   + apply(proj1(runtime_bool_langsym b w R Fl s "Bool"bid Load World Lang));change(runtime_symbol_value s("bool","Bool")=Some bid)in B;exact B.
   + apply(proj1(runtime_bool_langsym b w R Fl s "True"tid Load World Lang));change(runtime_symbol_value s("bool","True")=Some tid)in T;exact T.
   + apply(proj1(runtime_bool_langsym b w R Fl s "False"fid Load World Lang));change(runtime_symbol_value s("bool","False")=Some fid)in F;exact F.
Qed.
Lemma runtime_schemas_same:forall a b left right,
 st_w a=st_w b->st_lang a=st_lang b->
 runtime_symbols a=Good(left,a)->runtime_symbols b=Good(right,b)->left=right.
Proof.
 intros a b left right World Lang L R;rewrite runtime_symbols_exact in L,R;injection L as <-;injection R as <-.
 change(map(fun q=>(q,runtime_symbol_value a q))runtime_names=map(fun q=>(q,runtime_symbol_value b q))runtime_names).
 apply map_ext;intros q;unfold runtime_symbol_value;rewrite World,Lang;reflexivity.
Qed.
Lemma checked_library_bool_shape:forall initial final,
 check_runtime_library initial=Good(tt,final)->
 runtime_bool_shape(map(fun q=>(q,runtime_symbol_value initial q))runtime_names).
Proof.
 intros initial final Run;unfold check_runtime_library in Run.
 apply bind_inv in Run as[sc[s0[Runtime Run]]].
 rewrite runtime_symbols_exact in Runtime;injection Runtime as <- <-.
 apply bind_inv in Run as[u[s1[Abstracts Run]]].
 apply bind_inv in Run as[u1[s2[Bool Run]]];destruct u1.
 eapply checked_runtime_bool_shape;exact Bool.
Qed.
