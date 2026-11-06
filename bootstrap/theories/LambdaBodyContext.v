(* The actual lambda environment installs parameters then captures; the
   declared lexical context installs captures then parameters.  The successful
   frame-name check makes these two local views equivalent. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySound NominalSupport SourceClean CanonTypes
 LambdaCaptureFold LambdaTrace LambdaOwnContext LambdaCaptureTables LambdaCapturedLocals LambdaLocalOrder LambdaNumbering.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Definition lambda_parameter_rows(ps:list(name*option aty))(sg:fsig):list(string*(ety*lmut)):=
 map(fun q=>(fst q,(snd q,LLet)))(combine(map fst ps)(map snd(sg_params sg))).
Definition lambda_captured_rows(lets:list(string*nat*ety)):list(string*(ety*lmut)):=
 map(fun q=>let '(n,_,t) :=q in(n,(t,LCaptured)))lets.
Definition lambda_parameter_model(Fl:string->nat)(original:env)(ps:list(name*option aty))(sg:fsig)(C:A.cx):A.cx:=
 fold_left(fun C q=>A.add_local C(fst q)(snd q)A.LLet)
 (combine(map fst ps)(map(fun q=>tr Fl(canon original(snd q)))(sg_params sg)))C.
Lemma lambda_combine_property:forall X Y(P:Y->Prop)(xs:list X)ys,
 Forall P ys->Forall(fun q=>P(snd q))(combine xs ys).
Proof.
 intros X Y P xs;induction xs as[|x xs IH];intros[|y ys]Good;cbn;try solve[constructor].
 inversion Good;constructor;[assumption|apply IH;assumption].
Qed.
Lemma lambda_combine_names:forall X(names:list string)(ys:list X)n,
 In n(map fst(combine names ys))->In n names.
Proof.
 intros X names;induction names as[|name names IH];intros[|y ys]n Hit;cbn in *;try contradiction.
 destruct Hit as[Eq|Hit];[left;exact Eq|right;exact(IH _ _ Hit)].
Qed.
Lemma lambda_capture_rows_names:forall lets,map fst(lambda_captured_rows lets)=map capture_name lets.
Proof. intros;unfold lambda_captured_rows;rewrite map_map;apply map_ext;intros[[n v]t];reflexivity. Qed.
Lemma lambda_parameter_row_names:forall ps sg n,
 In n(map fst(lambda_parameter_rows ps sg))->In n(map fst ps).
Proof.
 intros ps sg n Hit;unfold lambda_parameter_rows in Hit;rewrite map_map in Hit.
 exact(lambda_combine_names ety(map fst ps)(map snd(sg_params sg))n Hit).
Qed.
Lemma lambda_names_in_slots:forall ps this n,In n(map fst ps)->In n(map fst(lambda_own_slots ps this)).
Proof.
 intros ps[ty|]n Hit;cbn[lambda_own_slots app map fst];rewrite lambda_slot_names;[right;exact Hit|exact Hit].
Qed.
Lemma lambda_nodup_disjoint:forall X(l r:list X),NoDup(l++r)->forall x,In x l->~In x r.
Proof.
 intros X l;induction l as[|first l IH];intros r Unique x Hit Other;[contradiction|].
 inversion Unique as[|n ns Absent Tail];subst;destruct Hit as[<-|Hit].
 - apply Absent;apply in_or_app;right;exact Other.
 - exact(IH r Tail x Hit Other).
Qed.
Lemma lambda_fold_left_map:forall X Y Z(f:Z->Y->Z)(g:X->Y)xs acc,
 fold_left f(map g xs)acc=fold_left(fun acc x=>f acc(g x))xs acc.
Proof. intros X Y Z f g xs;induction xs;intro acc;cbn;[reflexivity|apply IHxs]. Qed.
Lemma lambda_fold_left_ext:forall X Z(f g:Z->X->Z),
 (forall acc x,f acc x=g acc x)->forall xs acc,fold_left f xs acc=fold_left g xs acc.
Proof. intros X Z f g Eq xs;induction xs;intro acc;cbn;[reflexivity|rewrite Eq;apply IHxs]. Qed.
Lemma lambda_own_env_rows:forall ps sg this le lets,
 lambda_own_env ps sg this le lets=
 fold_left(fun en q=>set_local en(fst q)(fst(snd q))(snd(snd q)))(lambda_captured_rows lets)
 (fold_left(fun en q=>set_local en(fst q)(fst(snd q))(snd(snd q)))(lambda_parameter_rows ps sg)le).
Proof.
 intros;unfold lambda_own_env,lambda_captured_rows,lambda_parameter_rows.
 rewrite !lambda_fold_left_map;cbn[lambda_own_signature sg_params fst snd].
 apply lambda_fold_left_ext;intros en[[n v]t];reflexivity.
Qed.
Lemma lambda_typed_parameter_model:forall Fl original le ps sg C,
 en_uf le=en_uf original->
 lambda_add_locals(lambda_typed_locals Fl le(lambda_parameter_rows ps sg))C=
 lambda_parameter_model Fl original ps sg C.
Proof.
 intros Fl original le ps sg C Uf.
 unfold lambda_add_locals,lambda_typed_locals,lambda_parameter_rows,lambda_parameter_model.
 rewrite !map_map,!lambda_fold_left_map.
 generalize(map fst ps)as names;generalize(sg_params sg)as parameters;intros parameters;revert C.
 induction parameters as[|[old t]parameters IH];intros C[|n names];cbn[map combine fold_left fst snd tr_mut];try reflexivity.
 assert(E:canon le t=canon original t)by(unfold canon;now rewrite Uf).
 rewrite E;apply IH.
Qed.
Lemma lambda_typed_capture_model:forall Fl original le lets C,
 en_uf le=en_uf original->
 lambda_add_locals(lambda_typed_locals Fl le(lambda_captured_rows lets))C=
 fold_left(capture_local Fl original)lets C.
Proof.
 intros Fl original le lets;induction lets as[|[[n v]t]lets IH];intros C Uf;[reflexivity|].
 unfold lambda_add_locals,lambda_typed_locals,lambda_captured_rows in *;cbn[map fold_left fst snd capture_local].
 unfold canon in *;rewrite Uf in IH|-*;apply IH;reflexivity.
Qed.
Section Context.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySound.W w R Fl).
Theorem lambda_body_context:forall c original F Co le C ps sg this lets vars,
 CxRel w R Fl c original F Co->CxRel w R Fl c le F C->en_uf le=en_uf original->
 SigOK sg->SigSupport W sg->opt_wf this->opt_support W this->
 Forall(captured_row_available original)lets->
 NoDup(map fst(lambda_own_slots ps this)++map capture_name lets)->
 CxRel w R Fl(lambda_own_context c ps sg this)(lambda_own_env ps sg this le lets)
 (lambda_body_frame Fl original F sg this vars)
 (lambda_parameter_model Fl original ps sg(fold_left(capture_local Fl original)lets C)).
Proof.
 intros c original F Co le C ps sg this lets vars Horig HC Uf Clean Supported Wthis Sthis Available Unique.
 assert(Frame:lambda_body_frame Fl le F sg this vars=lambda_body_frame Fl original F sg this vars)
 by(unfold lambda_body_frame,canon;now rewrite Uf).
 pose proof(cxrel_lambda_frame w R Fl c le F C ps sg this vars HC
  (ety_ok_wf _(proj1(proj2 Clean)))(proj1(proj2 Supported))Wthis Sthis)as Own.
 rewrite Frame in Own.
 assert(Params:Forall(fun q=>WFty(fst(snd q))/\Support W(fst(snd q)))(lambda_parameter_rows ps sg)).
 {unfold lambda_parameter_rows;rewrite Forall_map;cbn[fst snd].
  apply(lambda_combine_property name ety(fun t=>WFty t/\Support W t)).
  pose proof(proj1 Clean)as Good;pose proof(proj1 Supported)as Support;rewrite Forall_map in Good.
  apply Forall_map,Forall_forall;intros q Hit;rewrite Forall_forall in Good,Support.
  split;[apply ety_ok_wf;exact(Good q Hit)|exact(Support q Hit)]. }
 assert(Captures:Forall(fun q=>WFty(fst(snd q))/\Support W(fst(snd q)))(lambda_captured_rows lets)).
 {unfold lambda_captured_rows;rewrite Forall_map;rewrite Forall_forall in Available|-*.
  intros[[n v]t]Hit;destruct(Available _ Hit)as[mu Lookup];cbn[fst snd].
  split;[exact(cr_locals_wf _ _ _ _ _ _ _ Horig n(t,mu)Lookup)|exact(@es_locals W original(cr_support _ _ _ _ _ _ _ Horig)n(t,mu)Lookup)]. }
 assert(Disjoint:forall n,In n(map fst(lambda_parameter_rows ps sg))->~In n(map fst(lambda_captured_rows lets))).
 {intros n Param Capture;rewrite lambda_capture_rows_names in Capture.
  eapply(lambda_nodup_disjoint _ _ _ Unique n);[apply lambda_names_in_slots;exact(lambda_parameter_row_names _ _ _ Param)|exact Capture]. }
 pose proof(lambda_disjoint_source_context w R Fl(lambda_parameter_rows ps sg)(lambda_captured_rows lets)
 (lambda_own_context c ps sg this)le(lambda_body_frame Fl original F sg this vars)C Own Params Captures Disjoint)as Body.
 rewrite <-(lambda_own_env_rows ps sg this le lets)in Body.
 rewrite(lambda_typed_parameter_model Fl original le ps sg _ Uf),
 (lambda_typed_capture_model Fl original le lets C Uf)in Body.
 exact Body.
Qed.
End Context.
