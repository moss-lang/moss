(* Every symbol the real loader creates retains its source origin. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque[Nat.of_num_uint nget nset].
Definition SourceSyms(syms:nmap gsym):Prop:=forall x g,nget syms x=Some g->gs_origin g=OSource.
Lemma source_nset : forall syms x g,SourceSyms syms->gs_origin g=OSource->SourceSyms(nset syms x g).
Proof.
 intros syms x g S O y h E. rewrite nget_nset in E. destruct(Nat.eqb y x);[injection E as <-;exact O|exact(S _ _ E)].
Qed.
Lemma define_name_source : forall st n id st',define_name st n id=Good st'->SourceSyms(ds_syms st)->SourceSyms(ds_syms st').
Proof.
 intros st n id st' E S. unfold define_name in E. destruct(sassoc n(gm_names(ds_mod st)));[discriminate|].
 injection E as <-;exact S.
Qed.
Lemma declare_walk_source : forall fuel mi st asm ds st',
 declare_walk fuel mi st asm ds=Good st'->SourceSyms(ds_syms st)->SourceSyms(ds_syms st').
Proof.
 intros fuel;induction fuel as[|fuel IH];intros mi st asm ds st' E S;[discriminate E|].
 destruct ds as[|d rest];[injection E as <-;exact S|].
 cbn[declare_walk]in E. apply rbind_good in E as[st1[Ed E]].
 assert(S1:SourceSyms(ds_syms st1)).
 { destruct d as[its inner|nt|na aty|nt tps pay|nu|nv aty init|recv n ps rt body|nc its|nf ps a r body];
   cbn[decl_kind fresh_sym ds_next ds_syms]in Ed.
   - exact(IH _ _ _ _ _ Ed S).
   - eapply define_name_source;[exact Ed|]. apply source_nset;[exact S|reflexivity].
   - eapply define_name_source;[exact Ed|]. apply source_nset;[exact S|reflexivity].
   - eapply define_name_source;[exact Ed|]. apply source_nset;[exact S|reflexivity].
   - eapply define_name_source;[exact Ed|]. apply source_nset;[exact S|reflexivity].
   - eapply define_name_source;[exact Ed|]. apply source_nset;[exact S|reflexivity].
   - destruct recv.
     + eapply define_name_source;[exact Ed|]. apply source_nset;[exact S|reflexivity].
     + destruct body;[discriminate Ed|]. destruct(sassoc n(gm_detached(ds_mod st)));[discriminate Ed|].
       injection Ed as <-;apply source_nset;[exact S|reflexivity].
     + injection Ed as <-;exact S.
   - eapply define_name_source;[exact Ed|]. apply source_nset;[exact S|reflexivity].
   - eapply define_name_source;[exact Ed|]. apply source_nset;[exact S|reflexivity]. }
 exact(IH _ _ _ _ _ E S1).
Qed.
Lemma declare_attached_source : forall mi ps st st',
 declare_attached mi st ps=Good st'->SourceSyms(ds_syms st)->SourceSyms(ds_syms st').
Proof.
 intros mi ps;induction ps as[|[d asm]ps IH];intros st st' E S;[injection E as <-;exact S|].
 cbn[declare_attached]in E. apply rbind_good in E as[st1[Ed E]].
 apply(IH _ _ E). destruct d as[its inner|nt|na aty|nt tps pay|nu|nv aty init|recv n qs rt body|nc its|nf qs a r body];try(injection Ed as <-;exact S).
 destruct recv as[| |rn];try(injection Ed as <-;exact S).
 destruct(sassoc rn(gm_names(ds_mod st)))as[rid|];[|discriminate Ed].
 destruct(assoc attached_eqb(rid,n)(gm_attached(ds_mod st)));[discriminate Ed|].
 cbn[fresh_sym]in Ed. injection Ed as <-;apply source_nset;[exact S|reflexivity].
Qed.
Lemma declare_source : forall mi path f syms next m syms' next',
 declare mi path f syms next=Good(m,syms',next')->SourceSyms syms->SourceSyms syms'.
Proof.
 intros mi path f syms next m syms' next' E S. unfold declare in E.
 apply rbind_good in E as[st1[Ew E]]. apply rbind_good in E as[st2[Ea E]]. injection E as _ <- _.
 pose proof(declare_walk_source _ _ _ _ _ _ Ew S)as S1.
 exact(declare_attached_source _ _ _ _ Ea S1).
Qed.
Lemma go_deps_source : forall b k loading p,
 (forall loading q st i st',load k b loading q st=Good(i,st')->SourceSyms(ls_syms st)->SourceSyms(ls_syms st'))->
 forall ds st deps st',go_deps b k loading p ds st=Good(deps,st')->SourceSyms(ls_syms st)->SourceSyms(ls_syms st').
Proof.
 intros b k loading p Hload ds;induction ds as[|[tp imp]ds IH];intros st deps st' E S.
 - injection E as _ <-;exact S.
 - cbn[go_deps]in E. apply rbind_good in E as[[ti st1][E1 E]].
   apply rbind_good in E as[[rest st2][E2 E]]. injection E as _ <-.
   exact(IH _ _ _ E2(Hload _ _ _ _ _ E1 S)).
Qed.
Theorem load_source : forall k b loading p st i st',
 load k b loading p st=Good(i,st')->SourceSyms(ls_syms st)->SourceSyms(ls_syms st').
Proof.
 intros k;induction k as[|k IH];intros b loading p st i st' E S;[discriminate E|].
 cbn[load]in E. destruct(sassoc p(ls_loaded st))as[i0|];[injection E as _ <-;exact S|].
 destruct(smem p loading);[discriminate E|]. destruct(find_module p(b_mods b))as[am|];[|discriminate E].
 apply rbind_good in E as[[deps st1][Ed E]].
 change(go_deps b k loading p
   ((if am_prelude am then[(b_prelude b,AImport(b_prelude b)None true[])]else[])++
    combine(am_targets am)(f_imports(am_file am)))st=Good(deps,st1))in Ed.
 pose proof(go_deps_source b k loading p (IH b)_ _ _ _ Ed S)as S1.
 apply rbind_good in E as[[[m syms]next][Ec E]].
 apply rbind_good in E as[m1[El E]]. injection E as _ <-.
 exact(declare_source _ _ _ _ _ _ _ _ Ec S1).
Qed.
Theorem load_world_source : forall b w,load_world b=Good w->SourceSyms(w_syms w).
Proof.
 intros b w E. unfold load_world in E. apply rbind_good in E as[[entry st][El E]]. injection E as <-.
 eapply load_source;[exact El|]. intros x g H;rewrite nget_NE in H;discriminate.
Qed.
Corollary loaded_no_forwarder : forall b w g,
 load_world b=Good w->nget(w_syms w)(gs_id g)=Some g->forall k,gs_origin g<>OForwarder k.
Proof. intros b w g E Hg k. rewrite(load_world_source _ _ E _ _ Hg);discriminate. Qed.
