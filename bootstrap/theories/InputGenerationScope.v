(* Freeze an input environment, then rename only its abstract generations.
   Declaration names, nominal heads, argument labels, symbols and stamps retain
   their original identities. This is a snapshot operation, not a FunCtx rule. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab BodyTr ElabSound.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].

Definition generation_substitution := list (nat * nat).
Definition scoped_generation (rho : generation_substitution) (g : nat) : nat :=
 opt_default g (nassoc g rho).
Fixpoint scoped_type (rho : generation_substitution) (t : ety) : ety :=
 match t with
 | YAbs g => YAbs (scoped_generation rho g)
 | YNom h args => YNom h (map (fun p => (fst p,scoped_type rho (snd p))) args)
 | YUnion ts => YUnion (map (scoped_type rho) ts)
 | YRec fs => YRec (map (fun p => (fst p,scoped_type rho (snd p))) fs)
 | _ => t
 end.
Definition map_scope_key (f : ety -> ety) (k : key) : key :=
 match k with KyMth t m => KyMth (f t) m | _ => k end.
Definition map_scope_signature (f : ety -> ety) (sg : fsig) : fsig :=
 FSig (map (fun p => (fst p,f (snd p))) (sg_params sg))
      (f (sg_ret sg)) (option_map f (sg_this sg)).
Definition map_scope_inst (f : ety -> ety) (xs : inst) : inst :=
 map (fun p => (fst p,option_map f (snd p))) xs.
Fixpoint map_scope_nmap {A B} (f : A -> B) (m : nmap A) : nmap B :=
 match m with NE => NE | NN l v r =>
 NN (map_scope_nmap f l) (option_map f v) (map_scope_nmap f r) end.
Definition map_scope_env (f : ety -> ety) (e : env) : env :=
 Env (map (fun p => (fst p,(f (fst (snd p)),snd (snd p)))) (en_tn e)) []
  (map_scope_nmap f (en_vals e))
  (map_scope_nmap (map_scope_signature f) (en_fns e))
  (map_scope_nmap (map (fun p => (f (fst p),map_scope_signature f (snd p)))) (en_mths e))
  (en_funs e)
  (map (fun p => (fst p,(f (fst (snd p)),snd (snd p)))) (en_locals e))
  (map (fun p => (map_scope_key f (fst p),(map_scope_inst f (fst (snd p)),snd (snd p)))) (en_apps e)).
Definition materialize_input_env (e : env) : env := map_scope_env (canon e) e.
Definition private_input_env (rho : generation_substitution) (e : env) : env :=
 map_scope_env (scoped_type rho) (materialize_input_env e).

Lemma scope_nmap_read : forall A B (f:A->B) m n,
 nget (map_scope_nmap f m) n = option_map f (nget m n).
Proof.
 intros A B f m;induction m as[|l IHl v r IHr];intros [|n];cbn;try reflexivity.
 destruct (is_even_nat n);[apply IHl|apply IHr].
Qed.
Lemma scope_assoc_values : forall K V W (eqb:K->K->bool) (f:V->W) k xs,
 assoc eqb k (map (fun p=>(fst p,f(snd p))) xs)=option_map f(assoc eqb k xs).
Proof.
 intros K V W eqb f k xs;induction xs as[|[a b] xs IH];cbn;[reflexivity|].
 destruct(eqb k a);[reflexivity|exact IH].
Qed.
Lemma scope_name_read : forall f e name,
 tn_get (map_scope_env f e) name=option_map f(tn_get e name).
Proof.
 intros f e name;unfold tn_get.
 change (option_map fst (assoc Nat.eqb name
  (map(fun p=>(fst p,(fun q:ety*nat=>(f(fst q),snd q))(snd p)))(en_tn e))) = option_map f (option_map fst (nassoc name (en_tn e)))).
 rewrite scope_assoc_values.
 unfold nassoc,option_map;destruct(assoc Nat.eqb name(en_tn e))as[[t stamp]|];reflexivity.
Qed.
Lemma scope_value_read : forall f e name,
 nget(en_vals(map_scope_env f e))name=option_map f(nget(en_vals e)name).
Proof. intros;apply scope_nmap_read. Qed.
Lemma scope_function_read : forall f e name,
 nget(en_fns(map_scope_env f e))name=
 option_map(map_scope_signature f)(nget(en_fns e)name).
Proof. intros;apply scope_nmap_read. Qed.
Lemma scope_local_read : forall f e name,
 assoc String.eqb name(en_locals(map_scope_env f e))=
 option_map(fun p=>(f(fst p),snd p))(assoc String.eqb name(en_locals e)).
Proof.
 intros f e name;change (assoc String.eqb name
 (map(fun p=>(fst p,(fun q:ety*lmut=>(f(fst q),snd q))(snd p)))(en_locals e)) =
 option_map(fun p=>(f(fst p),snd p))(assoc String.eqb name(en_locals e))).
 apply scope_assoc_values.
Qed.
Lemma scope_type_snapshot : forall f e,
 tymap(map_scope_env f e)=map(fun p=>(fst p,f(snd p)))(tymap e).
Proof.
 intros;unfold tymap,map_scope_env;cbn;rewrite !map_map.
 apply map_ext;intros [n [t stamp]];reflexivity.
Qed.
Lemma private_name_read : forall rho e name,
 tn_get(private_input_env rho e)name=
 option_map(fun t=>scoped_type rho(canon e t))(tn_get e name).
Proof.
 intros rho e name;unfold private_input_env,materialize_input_env.
 rewrite !scope_name_read. destruct(tn_get e name);reflexivity.
Qed.
Lemma private_uf_empty : forall rho e,en_uf(private_input_env rho e)=[].
Proof. reflexivity. Qed.
Lemma private_canon : forall rho e t,canon(private_input_env rho e)t=t.
Proof. intros;apply canon_nil;reflexivity. Qed.
Lemma private_functor_stamps : forall rho e,
 en_funs(private_input_env rho e)=en_funs e.
Proof. reflexivity. Qed.
Lemma private_type_name_stamps : forall rho e,
 map(fun p=>(fst p,snd(snd p)))(en_tn(private_input_env rho e))=
 map(fun p=>(fst p,snd(snd p)))(en_tn e).
Proof.
 intros;unfold private_input_env,materialize_input_env,map_scope_env;cbn.
 rewrite !map_map;apply map_ext;intros [n [t stamp]];reflexivity.
Qed.

(* The inverse is required only on the finite support actually read. In
   particular a private fresh target need not be renamed again when it is
   absent from the original snapshot. *)
Definition scope_inverse_on (rho back:generation_substitution)(gs:list nat):Prop:=
 forall g,In g gs->scoped_generation back(scoped_generation rho g)=g.
Lemma scoped_type_roundtrip : forall rho back t,
 scope_inverse_on rho back (ety_gens t)->
 scoped_type back(scoped_type rho t)=t.
Proof.
 intros rho back t;induction t as[| |g|h args IH|ts IH|fs IH]using ety_ind';intro R;cbn;try reflexivity.
 - f_equal;apply R;left;reflexivity.
 - f_equal. revert R;induction IH as[|[label t] rest H IH IHr];intro R;cbn;[reflexivity|].
   f_equal.
   + f_equal;apply H;intros g G;apply R;cbn;apply in_or_app;left;exact G.
   + apply IHr;intros g G;apply R;cbn;apply in_or_app;right;exact G.
 - f_equal. revert R;induction IH as[|t rest H IH IHr];intro R;cbn;[reflexivity|].
   f_equal.
   + apply H;intros g G;apply R;cbn;apply in_or_app;left;exact G.
   + apply IHr;intros g G;apply R;cbn;apply in_or_app;right;exact G.
 - f_equal. revert R;induction IH as[|[label t] rest H IH IHr];intro R;cbn;[reflexivity|].
   f_equal.
   + f_equal;apply H;intros g G;apply R;cbn;apply in_or_app;left;exact G.
   + apply IHr;intros g G;apply R;cbn;apply in_or_app;right;exact G.
Qed.

Lemma scoped_type_eqb : forall rho back a b,
 scope_inverse_on rho back(ety_gens a)->scope_inverse_on rho back(ety_gens b)->
 ety_eqb(scoped_type rho a)(scoped_type rho b)=ety_eqb a b.
Proof.
 intros rho back a b A B;destruct(ety_eqb a b)eqn:E.
 - apply ety_eqb_true in E;subst;apply ety_eqb_refl.
 - destruct(ety_eqb(scoped_type rho a)(scoped_type rho b))eqn:F;[|reflexivity].
   apply ety_eqb_true in F;apply(f_equal(scoped_type back))in F.
   rewrite (scoped_type_roundtrip _ _ _ A),(scoped_type_roundtrip _ _ _ B) in F.
   subst;rewrite ety_eqb_refl in E;discriminate.
Qed.
Definition scope_key_gens(k:key):list nat:=
 match k with KyMth t _=>ety_gens t|_=>[]end.
Lemma scoped_key_eqb : forall rho back a b,
 scope_inverse_on rho back(scope_key_gens a)->
 scope_inverse_on rho back(scope_key_gens b)->
 key_eqb(map_scope_key(scoped_type rho)a)(map_scope_key(scoped_type rho)b)=key_eqb a b.
Proof.
 intros rho back a b A B;destruct a;destruct b;cbn;try reflexivity.
 rewrite(scoped_type_eqb rho back _ _ A B);reflexivity.
Qed.
Lemma scope_assoc_keys : forall K V W (eqb:K->K->bool)(f:K->K)(g:V->W) k xs,
 (forall p,In p xs->eqb(f k)(f(fst p))=eqb k(fst p))->
 assoc eqb(f k)(map(fun p=>(f(fst p),g(snd p)))xs)=option_map g(assoc eqb k xs).
Proof.
 intros K V W eqb f g k xs;induction xs as[|[a b] xs IH];intro E;cbn;[reflexivity|].
 pose proof(E(a,b)(or_introl eq_refl))as Eq;cbn in Eq;rewrite Eq.
 destruct(eqb k a);[reflexivity|].
 apply IH;intros p P;apply E;right;exact P.
Qed.
Lemma scope_methods_read : forall f e method,
 nget(en_mths(map_scope_env f e))method=
 option_map(map(fun p=>(f(fst p),map_scope_signature f(snd p))))(nget(en_mths e)method).
Proof. intros;apply scope_nmap_read. Qed.
Lemma private_method_read : forall rho back e receiver method,
 let frozen:=materialize_input_env e in
 scope_inverse_on rho back(ety_gens receiver)->
 (forall entries, nget(en_mths frozen)method=Some entries->
   forall t sg,In(t,sg)entries->scope_inverse_on rho back(ety_gens t))->
 get_mth(private_input_env rho e)(scoped_type rho receiver)method=
 option_map(map_scope_signature(scoped_type rho))(get_mth frozen receiver method).
Proof.
 intros rho back e receiver method frozen R Entries;unfold get_mth,private_input_env.
 rewrite scope_methods_read;fold frozen.
 destruct(nget(en_mths frozen)method)as[entries|]eqn:E;cbn [option_map];[|reflexivity].
 apply scope_assoc_keys;intros [t sg] I;cbn.
 apply scoped_type_eqb with(back:=back);[exact R|eapply Entries;eauto].
Qed.
Lemma private_application_read : forall rho back e k,
 let frozen:=materialize_input_env e in
 scope_inverse_on rho back(scope_key_gens k)->
 (forall old record,In(old,record)(en_apps frozen)->
   scope_inverse_on rho back(scope_key_gens old))->
 get_app(private_input_env rho e)(map_scope_key(scoped_type rho)k)=
 option_map(fun p=>(map_scope_inst(scoped_type rho)(fst p),snd p))(get_app frozen k).
Proof.
 intros rho back e k frozen R Entries;unfold get_app,private_input_env.
 change(assoc key_eqb(map_scope_key(scoped_type rho)k)
  (map(fun p=>(map_scope_key(scoped_type rho)(fst p),
   (fun q:inst*nat=>(map_scope_inst(scoped_type rho)(fst q),snd q))(snd p)))(en_apps frozen))=
  option_map(fun p=>(map_scope_inst(scoped_type rho)(fst p),snd p))(assoc key_eqb k(en_apps frozen))).
 apply scope_assoc_keys;intros [old record] I;cbn.
 apply scoped_key_eqb with(back:=back);[exact R|eapply Entries;eauto].
Qed.

Lemma scope_signature_parameter_names : forall f sg,
 map fst(sg_params(map_scope_signature f sg))=map fst(sg_params sg).
Proof.
 intros;unfold map_scope_signature;cbn;rewrite map_map.
 apply map_ext;intros [label t];reflexivity.
Qed.
Lemma scope_inst_labels : forall f xs,map fst(map_scope_inst f xs)=map fst xs.
Proof.
 intros;unfold map_scope_inst;rewrite map_map.
 apply map_ext;intros [label t];reflexivity.
Qed.
Lemma scope_key_symbol : forall f k,
 match map_scope_key f k,k with
 | KyVal a,KyVal b|KyFn a,KyFn b|KyFun a,KyFun b|KyMth _ a,KyMth _ b=>a=b
 | _,_=>False end.
Proof. intros f [];reflexivity. Qed.
Lemma scope_canonical_arguments : forall f e,
 reify_arguments(map_scope_env f e)=map(fun p=>(fst p,f(snd p)))(tymap e).
Proof.
 intros f e;unfold reify_arguments;rewrite scope_type_snapshot,map_map.
 apply map_ext;intros [name t].
 change ((name,canon(map_scope_env f e)(f t))=(name,f t)).
 rewrite canon_nil;[reflexivity|reflexivity].
Qed.
Lemma private_canonical_arguments : forall rho e,
 reify_arguments(private_input_env rho e)=
 map(fun p=>(fst p,scoped_type rho(snd p)))(reify_arguments e).
Proof.
 intros rho e;unfold private_input_env,materialize_input_env.
 rewrite scope_canonical_arguments,scope_type_snapshot.
 unfold reify_arguments;rewrite !map_map.
 apply map_ext;intros [name t];reflexivity.
Qed.
Lemma private_value_read : forall rho e name,
 nget(en_vals(private_input_env rho e))name=
 option_map(fun t=>scoped_type rho(canon e t))(nget(en_vals e)name).
Proof.
 intros rho e name;unfold private_input_env,materialize_input_env.
 rewrite !scope_value_read;destruct(nget(en_vals e)name);reflexivity.
Qed.
Lemma private_function_read : forall rho e name,
 nget(en_fns(private_input_env rho e))name=
 option_map(map_scope_signature(scoped_type rho))
  (option_map(map_scope_signature(canon e))(nget(en_fns e)name)).
Proof.
 intros;unfold private_input_env,materialize_input_env;rewrite !scope_function_read;reflexivity.
Qed.
Lemma private_application_stamps : forall rho e,
 map(fun p=>snd(snd p))(en_apps(private_input_env rho e))=
 map(fun p=>snd(snd p))(en_apps e).
Proof.
 intros;unfold private_input_env,materialize_input_env,map_scope_env;cbn.
 rewrite !map_map;apply map_ext;intros [k [xs stamp]];reflexivity.
Qed.

Lemma scope_pair_read : forall (pairs:list(nat*nat)) g fresh,
 NoDup(map fst pairs)->In(g,fresh)pairs->nassoc g pairs=Some fresh.
Proof.
 intros pairs;induction pairs as[|[k v]pairs IH];intros g fresh Unique Hit;[contradiction|].
 inversion Unique as[|x xs Absent Tail];subst.
 cbn[nassoc assoc fst snd];destruct Hit as[Eq|Hit].
 - injection Eq as <- <-;rewrite Nat.eqb_refl;reflexivity.
 - destruct(Nat.eqb g k)eqn:Eq;[|apply IH;assumption].
   apply Nat.eqb_eq in Eq;subst g;exfalso;apply Absent.
   apply in_map with(f:=@fst nat nat)in Hit;exact Hit.
Qed.
Definition inverse_generation_substitution(rho:generation_substitution):generation_substitution:=
 map(fun p=>(snd p,fst p))rho.
Lemma scope_inverse_pairs : forall rho,
 NoDup(map fst rho)->NoDup(map snd rho)->
 scope_inverse_on rho(inverse_generation_substitution rho)(map fst rho).
Proof.
 intros rho Domain Range g Hit;apply in_map_iff in Hit as[[old fresh][Eq Hit]];cbn in Eq;subst old.
 unfold scoped_generation.
 rewrite(scope_pair_read rho g fresh Domain Hit);cbn[opt_default].
 assert(Unique:NoDup(map fst(inverse_generation_substitution rho))).
 { unfold inverse_generation_substitution;rewrite map_map.
   change(NoDup(map snd rho));exact Range. }
 assert(Member:In(fresh,g)(inverse_generation_substitution rho)).
 { unfold inverse_generation_substitution;apply in_map_iff;exists(g,fresh);split;[reflexivity|exact Hit]. }
 rewrite(scope_pair_read _ _ _ Unique Member);reflexivity.
Qed.
Lemma scoped_type_generations : forall rho t,
 ety_gens(scoped_type rho t)=map(scoped_generation rho)(ety_gens t).
Proof.
 intros rho t;induction t as[| |g|h args IH|ts IH|fs IH]using ety_ind';cbn;try reflexivity.
 - induction IH as[|[label t]rest H IH IHr];cbn;[reflexivity|].
   cbn in H;rewrite H,IHr,map_app;reflexivity.
 - induction IH as[|t rest H IH IHr];cbn;[reflexivity|].
   cbn in H;rewrite H,IHr,map_app;reflexivity.
 - induction IH as[|[label t]rest H IH IHr];cbn;[reflexivity|].
   cbn in H;rewrite H,IHr,map_app;reflexivity.
Qed.

Definition semantic_generation_substitution(rho:generation_substitution):subst:=
 map(fun p=>(fst p,TAbs(snd p)))rho.
Lemma scope_semantic_lookup : forall rho g,
 lookup_subst g(semantic_generation_substitution rho)=option_map TAbs(nassoc g rho).
Proof.
 intros rho g;unfold semantic_generation_substitution;induction rho as[|[a b]rho IH];cbn[map lookup_subst nassoc assoc fst snd];[reflexivity|].
 destruct(Nat.eqb g a);[reflexivity|exact IH].
Qed.
Lemma scoped_type_read : forall Fl rho t,
 tr Fl(scoped_type rho t)=Audit.app_subst(semantic_generation_substitution rho)(tr Fl t).
Proof.
 intros Fl rho t;induction t as[| |g|h args IH|ts IH|fs IH]using ety_ind';cbn[scoped_type tr Audit.app_subst];try reflexivity.
 - rewrite scope_semantic_lookup;unfold scoped_generation;destruct(nassoc g rho);reflexivity.
 - f_equal;induction IH as[|[label t]rest H IH IHr];cbn;[reflexivity|].
   cbn in H;rewrite H,IHr;reflexivity.
 - f_equal;induction IH as[|t rest H IH IHr];cbn;[reflexivity|].
   rewrite H,IHr;reflexivity.
 - f_equal;induction IH as[|[label t]rest H IH IHr];cbn;[reflexivity|].
   cbn in H;rewrite H,IHr;reflexivity.
Qed.
