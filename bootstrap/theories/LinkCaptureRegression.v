(* A real loaded source context captures seed:T while F fixes T. The
   former LinkBody capture condition rejects that context; the corrected
   condition permits it. This regression checks the guard, not LinkBody's
   remaining body/code obligations. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel CoreMoss NMap Sx Elab ElabSound WorldSound BodySoundCore.
Import ListNotations.
Local Open Scope string_scope.
Local Open Scope list_scope.
Module A := ExecutionModel.Elaboration.
Module FixedCapture.
Definition pathspec n := Spec (Some [n]) None None.
Definition ttype := ATRef ["T"] None.
Definition captured_body := [[(pathspec "out",Some (AELam [] None (AEPath ["seed"] None))) ]].
Definition captured_decls :=
 [ADType "T"; ADAssume [(["T"],None)] [ADVal "seed" ttype None;ADFn RPlain "out" [] (Some ttype) None];
 ADFunctor "F" (Some []) [pathspec "T"] [pathspec "out"] None;
 ADFunctor "G" None [pathspec "T";pathspec "seed"] [pathspec "out"] (Some captured_body)].
Definition captured_bundle := Bundle "/capture.moss" "/prelude.moss"
 [AModule "/capture.moss" false [] (AFile [] captured_decls)].
Definition captured_loaded := Eval vm_compute in load_world captured_bundle.
Definition captured_world : Elab.world := ltac:(
 let loaded := eval vm_compute in captured_loaded in
 lazymatch loaded with Good ?w => exact w end).
Definition fixture_requirements (n:nat) := if Nat.eqb n 2 || Nat.eqb n 3 then [1] else [].
Definition model := W_read captured_world fixture_requirements (fun _=>0).
Example ids : map (fun n=>A.resolve_sym model "/capture.moss" [n]) ["T";"seed";"out";"F";"G"] =
 [Some 1;Some 2;Some 3;Some 4;Some 5].
Proof. vm_compute;reflexivity. Qed.
Definition type_decl := A.Decl "/capture.moss" (ADType "T") [].
Definition fdecl := A.Decl "/capture.moss" (ADFunctor "F" (Some []) [pathspec "T"] [pathspec "out"] None) [].
Lemma fdecl_read : A.w_decl model 4 = Some fdecl.
Proof. vm_compute;reflexivity. Qed.
Ltac capture_crunch := repeat match goal with
 | H : A.resolve_sym model "/capture.moss" ["T"] = Some ?t |- _ =>
     change (Some 1 = Some t) in H; injection H as <-
 | H : A.kind_sym model 1 = Some ?k |- _ =>
     change (Some A.DKType = Some k) in H; first [discriminate H|injection H as <-]
 | H : A.w_decl model 1 = Some ?d |- _ =>
     change (Some type_decl = Some d) in H; injection H as <-
 | H : A.d_ast type_decl = _ |- _ => cbn [type_decl A.d_ast] in H;discriminate H
 | H : A.DKType = A.DKUnit \/ _ |- _ => destruct H as [H|H];[discriminate H|]
 | H : A.DKType = A.DKTag \/ _ |- _ => destruct H as [H|H];[discriminate H|]
 | H : A.DKType = A.DKAlias \/ _ |- _ => destruct H as [H|H];[discriminate H|]
 | H : A.DKType = A.DKVal /\ _ |- _ => destruct H as [H _];discriminate H
 end.
Lemma target_type_item : forall ob tm es tm',
 A.ItemEntries model "/capture.moss" ob tm (pathspec "T") es tm' -> es = [A.NTy 1].
Proof. intros ob tm es tm' H;inversion H;subst;capture_crunch;reflexivity. Qed.
Lemma target_needs_no_value : forall C ns,
 A.FunCtx model 4 C ns -> ~ In (IVal 2) ns.
Proof.
 intros C ns (d&ps&args&res&body&es&tm&tm'&sigma&implicit&Decl&Parts&Items&Implicit&Types&Merge&Context&Needs) Hit.
 rewrite fdecl_read in Decl;injection Decl as <-.
 cbn [fdecl A.d_ast A.functor_parts] in Parts;injection Parts as <- <- <- <-.
 inversion Items;subst.
 match goal with One:A.ItemEntries _ _ _ _ _ _ _,Rest:A.ItemsEntries _ _ _ _ [] _ _ |- _ =>
   pose proof (target_type_item _ _ _ _ One) as OneShape;inversion Rest;subst end.
 destruct Needs as [_ Keys];apply (proj1 (Keys (IVal 2))) in Hit.
 destruct Hit as [entry [[Same|[]] Key]];subst entry;discriminate Key.
Qed.
Definition cg : A.cx := A.Cx [(1,TAbs 1)] [] [(2,TAbs 1)] [] [] []
 [(IVal 2,[(1,Some (TAbs 1))])] [].
Definition cf : A.cx := A.Cx [(1,TAbs 1)] [] [] [] [] [] [] [].
Definition seed_entries := [A.NTy 1; A.NVal 2 (TAbs 1) [(1,Some (TAbs 1))]].
Lemma seed_entries_unify : A.Unifies [] seed_entries.
Proof.
 intros e1 e2 k [<-|[<-|[]]] [<-|[<-|[]]] K1 K2;try discriminate;reflexivity.
Qed.
Lemma seed_entries_merge : A.MostGeneralMerge [] seed_entries.
Proof.
 split;[exact seed_entries_unify|]. split.
 - intro t;now rewrite !app_subst_nil.
 - intros sub U t;now rewrite app_subst_nil.
Qed.
Definition gdecl := A.Decl "/capture.moss"
 (ADFunctor "G" None [pathspec "T";pathspec "seed"] [pathspec "out"] (Some captured_body)) [].
Definition seeddecl := A.Decl "/capture.moss" (ADVal "seed" ttype None) [(["T"],None)].
Lemma gdecl_read : A.w_decl model 5 = Some gdecl.
Proof. vm_compute;reflexivity. Qed.
Lemma seeddecl_read : A.w_decl model 2 = Some seeddecl.
Proof. vm_compute;reflexivity. Qed.
Lemma seed_app : A.AppOf model 2 [(1,TAbs 1)] [(1,Some (TAbs 1))].
Proof.
 exists [1].
 split;[reflexivity|]. split;[reflexivity|intro Kind;discriminate Kind].
Qed.
Lemma seed_items : A.ItemsEntries model "/capture.moss" [] [] [pathspec "T";pathspec "seed"]
 seed_entries [(1,TAbs 1)].
Proof.
 eapply A.IsCons with(es1:=[A.NTy 1])(tm1:=[(1,TAbs 1)])
  (es2:=[A.NVal 2 (TAbs 1) [(1,Some (TAbs 1))]]).
 - eapply A.IEType;reflexivity.
 - eapply A.IsCons with(es1:=[A.NVal 2 (TAbs 1) [(1,Some (TAbs 1))]])(es2:=[])(tm1:=[(1,TAbs 1)]).
   + eapply A.IEVal with(s:=2)(d:=seeddecl)(bs:=[]).
     * reflexivity.
     * exact seeddecl_read.
     * reflexivity.
     * constructor.
     * apply A.TyType with(t:=1);reflexivity.
     * exact seed_app.
   + constructor.
Qed.
Lemma g_context : A.FunCtx model 5 cg [IVal 2].
Proof.
 exists gdecl,None,[pathspec "T";pathspec "seed"],[pathspec "out"],(Some captured_body),
 seed_entries,[(1,TAbs 1)],[(1,TAbs 1)],[],[].
 split;[exact gdecl_read|]. split;[reflexivity|]. split;[exact seed_items|]. split.
 - intro t;split;[intros[]|intros[Bad _];discriminate Bad].
 - split;[reflexivity|]. split;[exact seed_entries_merge|]. split;[reflexivity|].
   split;[constructor;[intros[]|constructor]|]. intro k;split.
   + intros[<-|[]];exists(A.NVal 2(TAbs 1)[(1,Some(TAbs 1))]);split;[right;left;reflexivity|reflexivity].
   + intros[entry[[<-|[<-|[]]] Eq]];[discriminate Eq|]. injection Eq as <-;left;reflexivity.
Qed.
Definition legacy_capture_guard := forall k t,In k [IVal 2] ->
 ~(exists C ns,A.FunCtx model 4 C ns /\ In k ns) ->
 match k with IVal v=>forall vt,A.nlook v(A.c_vals cg)=Some vt->In t(A.gens vt)->A.nlook t(A.c_ty cg)=None
 | _=>True end.
Example legacy_guard_rejects_fixed_capture : ~legacy_capture_guard.
Proof.
 intro Guard;specialize(Guard(IVal 2)1 ltac:(left;reflexivity) ltac:(intros[C[ns[Ctx Hit]]];exact(target_needs_no_value C ns Ctx Hit))
 (TAbs 1)eq_refl ltac:(left;reflexivity));discriminate Guard.
Qed.
Definition outdecl := A.Decl "/capture.moss" (ADFn RPlain "out" [] (Some ttype) None) [(["T"],None)].
Lemma outdecl_read : A.w_decl model 3 = Some outdecl.
Proof. vm_compute;reflexivity. Qed.
Lemma f_no_outputs : forall t,~ A.FunOutputs model 4 t.
Proof.
 intros t (d&ps&args&res&body&sp&Decl&Parts&Member&Read).
 rewrite fdecl_read in Decl;injection Decl as <-.
 cbn [fdecl A.d_ast A.functor_parts] in Parts;injection Parts as <- <- <- <-.
 destruct Member as [<-|[]];inversion Read;subst.
 all: repeat match goal with
 | R:A.resolve_sym model _ ["out"]=Some ?target |- _=>change(Some 3=Some target)in R;injection R as <-
 | K:A.kind_sym model 3=Some A.DKType|-_=>discriminate K
 | D:A.w_decl model 3=Some ?d|-_=>rewrite outdecl_read in D;injection D as <-
 | H:A.d_ast outdecl=ADCtx _ _|-_=>discriminate H
 end.
Qed.
Lemma f_fixed_type : A.Fixed model 4 1.
Proof.
 split.
 - exists fdecl,(Some []),[pathspec "T"],[pathspec "out"],None,(pathspec "out").
   split;[exact fdecl_read|]. split;[reflexivity|]. split;[left;reflexivity|]. split.
   + apply A.RMValFn with(s:=3);[reflexivity|right;reflexivity|left;reflexivity|intros[x[[] _]]].
   + apply f_no_outputs.
 - intros (d&name&names&args&res&Decl&Ast&Member).
   rewrite fdecl_read in Decl;injection Decl as <-.
   cbn [fdecl A.d_ast] in Ast;injection Ast as <- <- <- <-.
   destruct Member as [x [[] _]].
Qed.
Definition corrected_capture_guard := forall k t,In k [IVal 2] -> ~In k [] ->
 match k with IVal v=>forall vt,A.nlook v(A.c_vals cg)=Some vt->In t(A.gens vt)->
 A.nlook t(A.c_ty cf)<>None->A.Fixed model 4 t | _=>True end.
Example corrected_guard_allows_fixed_capture : corrected_capture_guard.
Proof.
 intros k t [<-|[]] Not vt Lookup Mention Input.
 injection Lookup as <-;destruct Mention as [<-|[]];exact f_fixed_type.
Qed.
Lemma f_mentions_only_type : forall t,A.Mentions model 4 t -> t=1.
Proof.
 intros t (d&ps&args&res&body&sp&Decl&Parts&Member&Read&Output).
 rewrite fdecl_read in Decl;injection Decl as <-.
 cbn [fdecl A.d_ast A.functor_parts] in Parts;injection Parts as <- <- <- <-.
 destruct Member as [<-|[]];inversion Read;subst.
 all: repeat match goal with
 | R:A.resolve_sym model _ ["out"]=Some ?target |- _=>change(Some 3=Some target)in R;injection R as <-
 | D:A.w_decl model 3=Some ?d|-_=>rewrite outdecl_read in D;injection D as <-
 | H:A.d_ast outdecl=ADCtx _ _|-_=>discriminate H
 | H:A.d_ast outdecl=ADFunctor _ _ _ _ _|-_=>discriminate H
 | H:In _ []|-_=>contradiction
 end.
 match goal with H:In t(A.w_reqs model 3)|-_=>change(In t[1])in H;destruct H as[<-|[]];reflexivity end.
Qed.
Lemma f_context : A.FunCtx model 4 cf [].
Proof.
 exists fdecl,(Some []),[pathspec "T"],[pathspec "out"],None,[A.NTy 1],[(1,TAbs 1)],[(1,TAbs 1)],[],[].
 split;[exact fdecl_read|]. split;[reflexivity|]. split.
 - eapply A.IsCons with(es1:=[A.NTy 1])(es2:=[])(tm1:=[(1,TAbs 1)]);[apply A.IEType with(t:=1);reflexivity|constructor].
 - split.
   + intro t;split;[intros[]|]. intros[_[Mention Absent]].
     apply f_mentions_only_type in Mention;subst t;discriminate Absent.
   + split;[reflexivity|]. split.
     * split.
       -- intros e1 e2 k [<-|[]] Member Bad;discriminate Bad.
       -- split;[intro t;now rewrite !app_subst_nil|intros sub U t;now rewrite app_subst_nil].
     * split;[reflexivity|]. split;[constructor|]. intro k;split;[intros[]|].
       intros[entry[[<-|[]]Bad]];discriminate Bad.
Qed.
Theorem fixed_capture_regression :
 A.FunCtx model 4 cf [] /\ A.FunCtx model 5 cg [IVal 2] /\ A.Fixed model 4 1 /\
 ~legacy_capture_guard /\ corrected_capture_guard.
Proof. exact (conj f_context (conj g_context (conj f_fixed_type (conj legacy_guard_rejects_fixed_capture corrected_guard_allows_fixed_capture)))). Qed.

End FixedCapture.
