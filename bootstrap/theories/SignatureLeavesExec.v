(* Exact raw D78 signature leaves. Effective last output readings are a
   separate, stronger compatibility check. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTy.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Lemma leaf_eqb_eq : forall a b,leaf_eqb a b=true ->a=b.
Proof.
 intros [x dn] [y en] H;unfold leaf_eqb in H;cbn in H.
 apply andb_true_iff in H as[E F];apply Nat.eqb_eq in E.
 destruct dn as[dn|],en as[en|];cbn in F;try discriminate.
 - apply String.eqb_eq in F;congruence.
 - congruence.
Qed.
Lemma leaf_eqb_refl : forall a,leaf_eqb a a=true.
Proof. intros[x[dn|]];unfold leaf_eqb;cbn;rewrite ?Nat.eqb_refl,?String.eqb_refl;reflexivity. Qed.
Lemma leaf_aset_domain : forall (out:list(leaf*string)) (k:leaf) (text:string) (x:leaf),
 In x(map fst(aset leaf_eqb k text out)) <-> x=k\/In x(map fst out).
Proof.
 intros out;induction out as[|[h old]rest IH];intros k text x;cbn[aset map fst];[cbn [In];intuition congruence|].
 destruct(leaf_eqb k h)eqn:E.
 - apply leaf_eqb_eq in E;subst;cbn;cbn [In];intuition congruence.
 - cbn;rewrite IH;cbn [In];intuition congruence.
Qed.
Lemma leaf_fold_exact : forall
 (step:list(leaf*string)->spec->M(list(leaf*string)))
 (Meaning:spec->leaf->Prop) specs out s found s',
 (forall sp,In sp specs ->forall out found s',step out sp s=Good(found,s') ->
   s'=s /\ forall k,In k(map fst found)<->In k(map fst out)\/Meaning sp k) ->
 mfold step out specs s=Good(found,s') ->
 s'=s /\ forall k,In k(map fst found)<->In k(map fst out)\/
                       exists sp,In sp specs/\Meaning sp k.
Proof.
 intros step Meaning specs;induction specs as[|sp specs IH];intros out s found s' Steps H.
 - cbn[mfold]in H;injection H as <- <-;split;[reflexivity|]. intro k;cbn;split;[auto|intros[H|(?&[]&?)];exact H].
 - cbn[mfold]in H;apply bind_inv in H as(mid&s1&E&H).
   destruct(Steps sp (or_introl eq_refl) out mid s1 E)as[-> First].
   destruct(IH mid s found s')as[End Rest].
   + intros q Inq;eapply Steps;right;exact Inq.
   + exact H.
   + split;[exact End|]. intro k;rewrite Rest,First;cbn.
     split.
     * intros [[Old|Head]|(q&Here&Leaf)].
       -- left;exact Old.
       -- right;exists sp;split;[left;reflexivity|exact Head].
       -- right;exists q;split;[right;exact Here|exact Leaf].
     * intros [Old|(q&[Same|Here]&Leaf)].
       -- left;left;exact Old.
       -- subst q;left;right;exact Leaf.
       -- right;exists q;split;assumption.
Qed.

Lemma result_method_leaf : forall W p pa dn br s k,
 A.resolve_sym W p pa=Some s ->
 (A.ResultSpecLeaf W p(Spec(Some pa)(Some dn)br)k <->k=(s,Some dn)).
Proof.
 intros W p pa dn br s k R;split.
 - intros H;inversion H;subst;congruence.
 - intros ->;constructor;exact R.
Qed.
Lemma result_unresolved_leaf : forall W p pa dn br,
 A.resolve_sym W p pa=None ->forall k,~A.ResultSpecLeaf W p(Spec(Some pa)dn br)k.
Proof. intros W p pa dn br R k H;inversion H;subst;congruence. Qed.
Lemma result_empty_leaf : forall W p dn br k,~A.ResultSpecLeaf W p(Spec None dn br)k.
Proof. intros W p dn br k H;inversion H. Qed.
Lemma result_plain_leaf : forall W p pa br s d k,
 A.resolve_sym W p pa=Some s ->A.w_decl W s=Some d ->
 (forall n its,A.d_ast d<>ADCtx n its) ->
 (A.ResultSpecLeaf W p(Spec(Some pa)None br)k <->
   A.result_already_provided(A.d_ast d)=false /\k=(s,None)).
Proof.
 intros W p pa br s d k R D N;split.
 - intros H;inversion H;subst.
   + assert(s0=s)by congruence;subst s0;assert(d0=d)by congruence;subst d0;auto.
   + assert(c=s)by congruence;subst c;assert(d0=d)by congruence;subst d0;exfalso;eapply N;eassumption.
 - intros[Not ->];eapply A.RSLPlain;eauto.
Qed.
Lemma result_context_leaf : forall W p pa br c d n its k,
 A.resolve_sym W p pa=Some c ->A.w_decl W c=Some d ->A.d_ast d=ADCtx n its ->
 (A.ResultSpecLeaf W p(Spec(Some pa)None br)k <->
   exists sp,In sp(flat_map citem_specs its)/\A.ResultSpecLeaf W(A.d_mod d)sp k).
Proof.
 intros W p pa br c d n its k R D Ast;split.
 - intros H;inversion H;subst.
   + assert(s=c)by congruence;subst s;assert(d0=d)by congruence;subst d0;exfalso.
     match goal with N:forall n its,A.d_ast d<>ADCtx n its|-False=>exact(N n its Ast)end.
   + assert(c0=c)by congruence;subst c0;assert(d0=d)by congruence;subst d0.
     assert(its0=its)by congruence;subst its0;eauto.
 - intros(sp&Insp&Here);eapply A.RSLContext;eauto.
Qed.

Section Loaded.
Variable w:Elab.world. Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Notation path_of mi :=(gm_path(get_mod w mi)).
Lemma signature_loaded_kind : forall x g,nget(w_syms w)x=Some g ->KindOK g.
Proof.
 intros x g Hg. pose proof(kind_sym_read b w R Fl Hload x g Hg)as K.
 unfold ExecutionModel.Elaboration.kind_sym in K.
 rewrite(w_decl_read w R Fl x g Hg)in K;exact K.
Qed.
Lemma signature_already_provided : forall x g,nget(w_syms w)x=Some g ->
 already_provided g=A.result_already_provided(gs_decl g).
Proof.
 intros x g Hg;pose proof(signature_loaded_kind x g Hg)as K.
 unfold KindOK in K;unfold already_provided,fn_body.
 destruct(gs_decl g)eqn:D;destruct(gs_kind g)eqn:Knd;
 cbn[A.kind_of kconv A.result_already_provided]in *;try discriminate;try reflexivity.
 all:match goal with r:fnrecv|-_=>destruct r end;
 cbn[A.kind_of A.result_already_provided]in *;try discriminate;try reflexivity.
 all:match goal with o:option ablock|-_=>destruct o end;reflexivity.
Qed.
Theorem expand_spec_exact : forall fuel mi sp out s found s',
 expand_spec fuel mi sp out s=Good(found,s') ->Inv s ->st_w s=w ->In mi(w_order w) ->
 s'=s /\forall k,In k(map fst found)<->In k(map fst out)\/A.ResultSpecLeaf WW(path_of mi)sp k.
Proof.
 induction fuel as[|fuel IH];intros mi [pa dn br] out s found s' H Hi Hw Hmi;
 cbn[expand_spec]in H;[discriminate|].
 destruct pa as[pa|].
 2:{injection H as <- <-;split;[reflexivity|]. intros k;split;[auto|].
    intros[Old|Bad];[exact Old|exfalso;eapply result_empty_leaf;exact Bad]. }
 apply bind_inv in H as(tg&s1&Path&H).
 destruct(rpath_rel w R Fl b Hload mi pa s tg s1 Hw Hmi Path)as[-> Resolve].
 destruct tg as[[x|modid]|];cbn[option_map ent_of]in Resolve.
 2:{injection H as <- <-;split;[reflexivity|]. intros k;split;[auto|].
      intros[Old|Bad];[exact Old|exfalso;eapply result_unresolved_leaf;[unfold A.resolve_sym;rewrite Resolve;reflexivity|exact Bad]]. }
 2:{injection H as <- <-;split;[reflexivity|]. intros k;split;[auto|].
      intros[Old|Bad];[exact Old|exfalso;eapply result_unresolved_leaf;[unfold A.resolve_sym;rewrite Resolve;reflexivity|exact Bad]]. }
 apply bind_inv in H as(g&s2&Sym&H).
 destruct(sym_loader w R Fl b Hload _ _ x s g s2 Hi Hw Resolve Sym)as[-> Hg].
 assert(Rs:A.resolve_sym WW(path_of mi)pa=Some x).
 {unfold A.resolve_sym;rewrite Resolve;reflexivity. }
 pose proof(w_decl_read w R Fl x g Hg)as Dg.
 destruct dn as[dn|].
 - injection H as <- <-;split;[reflexivity|]. intro k.
   rewrite leaf_aset_domain,(result_method_leaf WW _ _ _ _ _ _ Rs);tauto.
 - destruct(gs_decl g)eqn:D.
   8:{destruct(leaf_fold_exact
       (fun o it=>expand_spec fuel(gs_mod g)it o)
       (A.ResultSpecLeaf WW(path_of(gs_mod g))) (flat_map citem_specs l) out s found s')as[End Fold].
      - intros sp InSp old found0 s0' Run.
        exact(IH (gs_mod g)sp old s found0 s0' Run Hi Hw
                    (loader_mod_in w R Fl b Hload x g Hg)).
      - exact H.
      - split;[exact End|]. intro k;rewrite Fold.
        rewrite(result_context_leaf WW _ _ _ _ _ _ _ _ Rs Dg ltac:(reflexivity));reflexivity. }
   all:assert(Nctx:forall n its,gs_decl g<>ADCtx n its)by(intros name0 its0;rewrite D;discriminate).
   all:pose proof(signature_already_provided x g Hg)as Ap.
   all:destruct(already_provided g)eqn:Provided.
   all:injection H as <- <-;split;[reflexivity|];intro k.
   all:rewrite(result_plain_leaf WW _ _ _ _ _ _ Rs Dg ltac:(cbn;rewrite <-D;exact Nctx)).
   all:rewrite D in Ap.
   all:cbn[A.d_ast]in Ap|-*.
   all:try rewrite <-Ap.
   all:try(rewrite leaf_aset_domain).
   all:clear -Ap;cbn;intuition discriminate.
Qed.

Theorem signature_items_exact : forall mi specs s found s',
 signature_items mi specs s=Good(found,s') ->Inv s ->st_w s=w ->In mi(w_order w) ->
 s'=s /\ forall k,In k(map fst found)<->
     exists sp,In sp specs/\A.ResultSpecLeaf WW(path_of mi)sp k.
Proof.
 intros mi specs s found s' H Hi Hw Hmi.
 unfold signature_items,walk_fuel in H.
 apply bind_inv in H as(n&s1&N&H).
 unfold get,ret,mbind in N;injection N as <- <-.
 destruct(leaf_fold_exact
   (fun o sp=>expand_spec (S(w_nsyms(st_w s)))mi sp o)
   (A.ResultSpecLeaf WW(path_of mi))specs[]s found s')as[End Rel].
 - intros sp InSp out found0 s0' Run.
   exact(expand_spec_exact _ mi sp out s found0 s0' Run Hi Hw Hmi).
 - exact H.
 - split;[exact End|]. intro k;specialize(Rel k);cbn in Rel;tauto.
Qed.
Theorem signature_functor_leaves : forall g n ps args res body s found s',
 nget(w_syms w)(gs_id g)=Some g ->gs_decl g=ADFunctor n ps args res body ->
 signature_items(gs_mod g)res s=Good(found,s') ->Inv s ->st_w s=w ->
 s'=s /\forall k,In k(map fst found)<->A.ResultLeaf WW(gs_id g)k.
Proof.
 intros g n ps args res body s found s' Hg Decl H Hi Hw.
 destruct(signature_items_exact (gs_mod g)res s found s' H Hi Hw
     (loader_mod_in w R Fl b Hload _ _ Hg))as[End Rel].
 assert(Dg:A.w_decl WW(gs_id g)=Some(A.Decl(path_of(gs_mod g))(gs_decl g)(gs_assumes g)))
   by exact(w_decl_read w R Fl _ _ Hg).
 split;[exact End|]. intro k;rewrite Rel;split.
 - intros(sp&InSp&Leaf);eapply A.RL;[exact Dg|cbn;exact Decl|exact InSp|exact Leaf].
 - intros Leaf;inversion Leaf as[uid d name poly ins rs bd sp key Wd Ast InSp Here];subst.
   rewrite Dg in Wd;injection Wd as <-;cbn in Ast,Here.
   rewrite Decl in Ast;injection Ast as <- <- <- <- <-.
   exists sp;auto.
Qed.
Lemma checked_leaf_sets_equal : forall (left right:list(leaf*string)),
 filter(fun q=>negb(existsb(fun r=>leaf_eqb(fst q)(fst r))right))left=[] ->
 filter(fun q=>negb(existsb(fun r=>leaf_eqb(fst q)(fst r))left))right=[] ->
 forall k,In k(map fst left)<->In k(map fst right).
Proof.
 assert(One:forall (xs ys:list(leaf*string)),
   filter(fun q=>negb(existsb(fun r=>leaf_eqb(fst q)(fst r))ys))xs=[] ->
   forall k,In k(map fst xs)->In k(map fst ys)).
 {intros xs ys Empty k Hin;apply in_map_iff in Hin as([key text]&E&InPair);cbn in E;subst key.
  destruct(existsb(fun r=>leaf_eqb k(fst r))ys)eqn:Find.
  - apply existsb_exists in Find as([other label]&InOther&Eq);cbn in Eq;apply leaf_eqb_eq in Eq;subst other.
    apply in_map_iff;exists(k,label);split;[reflexivity|exact InOther].
  - exfalso;assert(In(k,text)(filter(fun q=>negb(existsb(fun r=>leaf_eqb(fst q)(fst r))ys))xs)).
    {apply filter_In;split;[exact InPair|cbn;rewrite Find;reflexivity]. }
    rewrite Empty in H;exact H. }
 intros left right L R0 k;split;[exact(One left right L k)|exact(One right left R0 k)].
Qed.
Theorem signature_items_same_result : forall fg gn fn fp fa fr fb gname gp ga gr gb s fitems gitems,
 nget(w_syms w)(gs_id fg)=Some fg ->nget(w_syms w)(gs_id gn)=Some gn ->
 gs_decl fg=ADFunctor fn fp fa fr fb ->gs_decl gn=ADFunctor gname gp ga gr gb ->
 signature_items(gs_mod fg)fr s=Good(fitems,s) ->
 signature_items(gs_mod gn)gr s=Good(gitems,s) ->Inv s ->st_w s=w ->
 filter(fun q=>negb(existsb(fun r=>leaf_eqb(fst q)(fst r))gitems))fitems=[] ->
 filter(fun q=>negb(existsb(fun r=>leaf_eqb(fst q)(fst r))fitems))gitems=[] ->
 A.SameResult WW(gs_id fg)(gs_id gn).
Proof.
 intros fg gn fn fp fa fr fb gname gp ga gr gb s fitems gitems Fg Gg Fd Gd FRun GRun Hi Hw Missing More k.
 rewrite <- (proj2(signature_functor_leaves fg fn fp fa fr fb s fitems s Fg Fd FRun Hi Hw)k).
 rewrite <- (proj2(signature_functor_leaves gn gname gp ga gr gb s gitems s Gg Gd GRun Hi Hw)k).
 exact(checked_leaf_sets_equal fitems gitems Missing More k).
Qed.

Lemma signature_functor_decl : forall g,KindOK g ->gs_kind g=KFunctor ->
 exists n ps args res body,gs_decl g=ADFunctor n ps args res body.
Proof.
 intros g K Kind;unfold KindOK in K;rewrite Kind in K;cbn[kconv]in K.
 destruct(gs_decl g);cbn[A.kind_of]in K;try discriminate;
 try(match goal with r:fnrecv|-_=>destruct r;discriminate end).
 repeat eexists;reflexivity.
Qed.
Theorem bind_functor_result_leaves : forall fuel c e fg fname fps fargs fres pg s xs e' s',
 nget(w_syms w)(gs_id fg)=Some fg ->gs_decl fg=ADFunctor fname fps fargs fres None ->
 bind_functor fuel c e fg(AEPath pg None)s=Good((xs,e'),s') ->
 Inv s ->st_w s=w ->In(cx_mod c)(w_order w) ->
 exists bg,nget(w_syms w)(gs_id bg)=Some bg /\
 A.resolve_sym WW(path_of(cx_mod c))pg=Some(gs_id bg) /\A.SameResult WW(gs_id fg)(gs_id bg).
Proof.
 intros[|fuel] c e fg fname fps fargs fres pg s xs e' s' Fg Fdecl Run Hi Hw Hmi;
 cbn[bind_functor]in Run;[discriminate|].
 unfold functor_parts in Run;rewrite Fdecl in Run.
 apply bind_inv in Run as(bgopt&s1&Resolve&Run).
 destruct(esym_rel w R Fl b Hload(cx_mod c)pg s bgopt s1 Hi Hw Hmi Resolve)as[-> Resolved].
 destruct bgopt as[bg|];[|discriminate]. destruct Resolved as[Path Bg].
 destruct(negb(kind_eqb(gs_kind bg)KFunctor))eqn:Kind in Run;[discriminate|].
 apply negb_false_iff in Kind;apply kind_eqb_spec in Kind.
 destruct(signature_functor_decl bg(signature_loaded_kind _ _ Bg)Kind)
   as(gname&gps&gargs&gres&gbody&Gdecl).
 rewrite Gdecl in Run.
 apply bind_inv in Run as(u&s1&Avail&Run).
 destruct(negb(is_some gbody)&&negb(is_some(nget(en_funs e)(gs_id bg))))eqn:Aval in Avail;
 [discriminate|injection Avail as <- <-].
 apply bind_inv in Run as(fitems&s1&FRun&Run).
 destruct(signature_functor_leaves fg fname fps fargs fres None s fitems s1 Fg Fdecl FRun Hi Hw)as[-> FRel].
 apply bind_inv in Run as(gitems&s1&GRun&Run).
 destruct(signature_functor_leaves bg gname gps gargs gres gbody s gitems s1 Bg Gdecl GRun Hi Hw)as[-> GRel].
 apply bind_inv in Run as(u&s1&Guard&Run).
 destruct(filter(fun q=>negb(existsb(fun r=>leaf_eqb(fst q)(fst r))gitems))fitems)as[|q qs]eqn:Missing in Guard;
 [|discriminate].
 destruct(filter(fun q=>negb(existsb(fun r=>leaf_eqb(fst q)(fst r))fitems))gitems)as[|q qs]eqn:More in Guard;
 [|discriminate].
 exists bg;split;[exact Bg|]. split;[exact Path|]. intro k.
 rewrite <-FRel,<-GRel;exact(checked_leaf_sets_equal fitems gitems Missing More k).
Qed.

End Loaded.
