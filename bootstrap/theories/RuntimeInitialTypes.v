(* The initial runtime type scaffold represents the checked source context. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound
 LibraryFacts MethodLookup RuntimeBoolSupport PayloadInst EtyTyExec CtxMerge CanonBodyTy.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Open Scope string_scope.
Definition wasm_scope_symbol(W:A.world)(name:string):option nat:=
 match A.w_library W with
 |None=>None
 |Some dir=>match A.w_scope W(String.append dir "wasm.moss")A.NSName name with
   |Some(A.ESym symbol)=>Some symbol|_=>None end end.
Theorem runtime_wasm_symbol:forall b w R Fl s name,
 load_world b=Good w->st_w s=w->st_lang s=lang_modules w->
 runtime_symbol_value s("wasm",name)=wasm_scope_symbol(W_read w R Fl)name.
Proof.
 intros b w R Fl s name Load World Lang.
 unfold runtime_symbol_value,wasm_scope_symbol;cbn[fst snd];rewrite World,Lang.
 change(A.w_library(W_read w R Fl))with(w_library w).
 destruct(w_library w)as[dir|]eqn:Library.
 - destruct(sassoc "wasm"(lang_modules w))as[mi|]eqn:Selected.
   + destruct(lang_entry w "wasm" mi Selected)as[InOrder Path].
     unfold A.LibraryModule in Path;cbn[A.w_library W_read]in Path;rewrite Library in Path.
     pose proof(loaded_scope_table b w R Fl Load mi A.NSName name InOrder)as Scope.
     assert(PathExact:gm_path(get_mod w mi)=String.append dir "wasm.moss").
     {rewrite Path;clear -dir;induction dir;cbn[String.append];congruence. }
     rewrite PathExact in Scope;rewrite Scope;cbn[tbl aent];destruct(sassoc name(gm_names(get_mod w mi)));reflexivity.
   + destruct(A.w_scope(W_read w R Fl)(String.append dir "wasm.moss")A.NSName name)as[[symbol|path]|]eqn:Scope;[| |reflexivity].
     * destruct(selected_scope w R Fl _ _ _ _ ltac:(unfold A.LibraryModule;cbn[A.w_library W_read];rewrite Library;reflexivity)Scope)
       as[mi[Found Rest]];rewrite Selected in Found;discriminate.
     * cbn[A.w_scope W_read]in Scope.
       destruct(find_mod w(String.append dir "wasm.moss")(w_order w))as[selectedMod|];[|discriminate Scope].
       cbn[tbl aent]in Scope;destruct(sassoc name(gm_names selectedMod));discriminate Scope.
 - destruct(sassoc "wasm"(lang_modules w))as[mi|]eqn:Selected;[|reflexivity].
   destruct(lang_entry w "wasm" mi Selected)as[InOrder Impossible].
   unfold A.LibraryModule in Impossible;cbn[A.w_library W_read]in Impossible;rewrite Library in Impossible;contradiction.
Qed.
Lemma runtime_machine_id:forall s name,In name runtime_machine_names->
 runtime_id(map(fun q=>(q,runtime_symbol_value s q))runtime_names)"wasm"name=
 runtime_symbol_value s("wasm",name).
Proof. intros s name[<-|[<-|[<-|[<-|[<-|[<-|[]]]]]]];reflexivity. Qed.
Lemma initial_machine_subst_image:forall W base,
 A.initial_machine_subst W base=
 flat_map(fun q=>match wasm_scope_symbol W(snd q)with
 |Some symbol=>[(symbol,TNom(base+fst q)[])]|None=>[]end)
 (combine(seq 0 6)runtime_machine_names).
Proof.
 intros W base;unfold A.initial_machine_subst.
 apply payload_flat_map_ext;intros[slot name]InRow;cbn[fst snd].
 unfold wasm_scope_symbol;destruct(A.w_library W)as[dir|];[|reflexivity].
 destruct(A.w_scope W(String.append dir "wasm.moss")A.NSName name)as[[symbol|path]|];reflexivity.
Qed.
Theorem runtime_machine_subst_exact:forall b w R Fl s sc base,
 load_world b=Good w->st_w s=w->st_lang s=lang_modules w->
 runtime_symbols s=Good(sc,s)->runtime_machines sc base=A.initial_machine_subst(W_read w R Fl)base.
Proof.
 intros b w R Fl s sc base Load World Lang Run;rewrite runtime_symbols_exact in Run;injection Run as <-.
 rewrite initial_machine_subst_image;unfold runtime_machines.
 change(flat_map(fun q=>match runtime_id(map(fun p=>(p,runtime_symbol_value s p))runtime_names)"wasm"(snd q)with
 |Some t=>[(t,TNom(base+fst q)[])]|None=>[]end)(combine(seq 0 6)runtime_machine_names)=
 flat_map(fun q=>match wasm_scope_symbol(W_read w R Fl)(snd q)with
 |Some t=>[(t,TNom(base+fst q)[])]|None=>[]end)(combine(seq 0 6)runtime_machine_names)).
 apply payload_flat_map_ext;intros[slot name]InRow;cbn[fst snd].
 rewrite runtime_machine_id by(eapply in_combine_r;exact InRow).
 rewrite(runtime_wasm_symbol b w R Fl s name Load World Lang);reflexivity.
Qed.
Theorem runtime_machine_heads_fresh:forall b w R Fl s,
 load_world b=Good w->Inv s->st_w s=w->0<st_next s->
 forall slot,slot<6->A.Fresh(W_read w R Fl)(st_next s+slot).
Proof.
 intros b w R Fl s Load I World Positive slot Bound;split.
 - cbn[A.w_decl W_read]. destruct(nget(w_syms w)(st_next s+slot))as[g|]eqn:Loaded;[|reflexivity].
   pose proof(iv_loader s I b ltac:(rewrite World;exact Load)_ g ltac:(rewrite World;exact Loaded))as Present.
   rewrite(iv_above s I (st_next s+slot) ltac:(lia))in Present;discriminate.
 - lia.
Qed.

Section MainTypes.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Lemma runtime_main_types_read:forall e machines s types final,
 mmap(fun q=>let! tau:=ety_ty(canon e(snd q))in
 ret(fst q,Audit.app_subst machines tau))(tymap e)s=Good(types,final)->
 Inv s->Later w P R Fl final->
 types=map(fun q=>(fst q,Audit.app_subst machines(tr Fl(canon e(snd q)))))(tymap e).
Proof.
 intros e machines s types final Run I Late.
 eapply(mmap_pure _ _ _ _ (Later w P R Fl));[exact Run|exact I|exact Late| | |].
 - intro q;apply ok_bind;[apply ok_ety_ty|intro;apply ok_ret].
 - intros q si value sf InRow Ii Step Li;apply bind_inv in Step as[tau[mid[Read Ret]]].
   injection Ret as <- <-;rewrite(ety_ty_tr w P R Fl _ _ _ _ Read Ii Li);reflexivity.
 - intros si sf Frame L;exact(later_back w P R Fl _ _ Frame L).
Qed.
Lemma runtime_main_types_lookup:forall c e F C machines s types final,
 CxRel w R Fl c e F C->
 mmap(fun q=>let! tau:=ety_ty(canon e(snd q))in
 ret(fst q,Audit.app_subst machines tau))(tymap e)s=Good(types,final)->
 Inv s->Later w P R Fl final->forall key,
 A.nlook key types=option_map(Audit.app_subst machines)(A.nlook key(A.c_ty C)).
Proof.
 intros c e F C machines s types final Context Run I Late key.
 rewrite(runtime_main_types_read _ _ _ _ _ Run I Late).
 rewrite(cr_ty _ _ _ _ _ _ _ Context).
 unfold tymap,tn_get;generalize(en_tn e);intro rows.
 induction rows as[|[k [t stamp]]rows IH];[reflexivity|].
 cbn[map A.nlook nassoc assoc fst snd];destruct(Nat.eqb key k);[reflexivity|exact IH].
Qed.
End MainTypes.
Theorem runtime_initial_types:forall b w P R Fl c e F C s sc types final,
 load_world b=Good w->Inv s->st_w s=w->st_lang s=lang_modules w->0<st_next s->
 runtime_symbols s=Good(sc,s)->CxRel w R Fl c e F C->
 mmap(fun q=>let! tau:=ety_ty(canon e(snd q))in
 ret(fst q,Audit.app_subst(runtime_machines sc(st_next s))tau))(tymap e)s=Good(types,final)->
 Later w P R Fl final->A.InitialTypes(W_read w R Fl)C types.
Proof.
 intros b w P R Fl c e F C s sc types final Load I World Lang Positive Runtime Context Run Late.
 exists(st_next s);split.
 - exact(runtime_machine_heads_fresh b w R Fl s Load I World Positive).
 - rewrite <-(runtime_machine_subst_exact b w R Fl s sc(st_next s)Load World Lang Runtime).
   exact(runtime_main_types_lookup w P R Fl c e F C _ _ _ _ Context Run I Late).
Qed.
