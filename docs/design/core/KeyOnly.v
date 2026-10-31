(* ------------------------------------------------------------------------ *)
(* KeyOnly.v — why coherence is keyed by θ [D71] (see Narrowing.v).         *)
(*                                                                          *)
(* A variant of the Core Moss statics in which a method requirement is      *)
(* matched by its *key* alone — (receiver type after σ, method symbol) —    *)
(* ignoring the method's own application θ, both where satisfaction checks  *)
(* availability and where ς's correctness is checked.  Every other item     *)
(* kind is compared exactly as in the core.  This is the rule the           *)
(* implementations used before D71; the definitions below are §6–§9 of      *)
(* CoreMoss.v copied verbatim with `avail`/`item_matches` replaced (suffix  *)
(* K).  It is documentation, not a model of the language: it exists to      *)
(* show that this one change admits a program that goes wrong.              *)
(* ------------------------------------------------------------------------ *)

From NM Require Import CoreMoss Narrowing.
From Stdlib Require Import List Bool Arith Lia.
Import ListNotations.

Set Implicit Arguments.

Definition key_eqb (s : subst) (i j : item) : bool :=
  match i, j with
  | IMth rt m _, IMth rt' m' _ =>
      Nat.eqb m m' && ty_eqb (app_subst s rt) (app_subst s rt')
  | _, _ => item_subst_eqb s i j
  end.

Definition availK (Phi : ctx) (i : item) : Prop :=
  existsb (fun i0 => key_eqb (cS Phi) i0 i) (cA Phi) = true.

Definition item_matchesK (s th : subst) (i i' : item) : Prop :=
  key_eqb s (app_subst_item th i) i' = true.

Definition sat_atK (Sg : gsig) (Phi : ctx) (th : subst) (PhiD : ctx) : Prop :=
  map fst th = tyreqs (cA PhiD) /\
  Forall (fun i =>
            is_ty_item i = true \/
            (availK Phi (app_subst_item th i) /\
             Forall (fun t =>
                       app_subst (cS Phi) (app_subst th (TAbs t))
                       = app_subst (cS Phi) (TAbs t))
                    (dep_tys Sg i))) (cA PhiD) /\
  merge_respects Phi th (cS PhiD).

Definition satK (Sg : gsig) (Phi : ctx) (th : subst) (D : tele) : Prop :=
  exists PhiD, tele_ctx D = Some PhiD /\ sat_atK Sg Phi th PhiD.

Definition sat0K (Sg : gsig) (Phi : ctx) (D : tele) : Prop :=
  exists PhiD, tele_ctx D = Some PhiD
  /\ sat_atK Sg Phi (id_app (cA PhiD)) PhiD
  /\ Forall (fun t => availK Phi (ITy t)) (tyreqs (cA PhiD)).
(* The last conjunct is the paper's S-Pass availability, which the
   elaborated-normal-form `satK` alone loses at identity applications: a
   batch-1 agent refuted tele_deps_closed and traced it exactly here. *)

Inductive wf_tyK (Sg : gsig) (Phi : ctx) : ty -> Prop :=
| WfUnitK : wf_tyK Sg Phi TUnit
| WfNeedK : forall t, availK Phi (ITy t) -> wf_tyK Sg Phi (TAbs t)
| WfTagK : forall n D tau0 th,
    g_tag Sg n = Some (D, tau0) ->
    satK Sg Phi th D ->
    Forall (fun p => wf_tyK Sg Phi (snd p)) th ->
    wf_tyK Sg Phi (TNom n th)
| WfUnionK : forall ms,
    Forall (wf_tyK Sg Phi) ms ->
    Forall is_nom ms ->
    NoDup (heads_of (cS Phi) ms) ->
    wf_tyK Sg Phi (TUnion ms).


(* ς's domain is the deduplicated telescope's non-type items: elaborated
   bodies reference the representatives, so those are the keys the callee's
   runtime context is built under. *)
Definition smap_okK (Phi : ctx) (th : subst) (sm : smap) (D : tele) : Prop :=
  exists PhiD, tele_ctx D = Some PhiD /\
  map fst sm = filter (fun i => negb (is_ty_item i)) (cA PhiD) /\
  Forall (fun p => In (snd p) (cA Phi)
                   /\ item_matchesK (cS Phi) th (fst p) (snd p)) sm.

Inductive has_tyK (Sg : gsig) : ctx -> tenv -> expr -> ty -> Prop :=
| TVarRK : forall Phi G x tau,
    lookup_tenv x G = Some tau ->
    has_tyK Sg Phi G (EVar x) tau
| TUnitRK : forall Phi G,
    has_tyK Sg Phi G EUnit TUnit
| TSubRK : forall Phi G e tau' tau,
    has_tyK Sg Phi G e tau' ->
    subty Phi tau' tau ->
    (* the target must be well-formed: subsumption may not inject into a
       union whose other members mention arbitrary symbols (regularity) *)
    wf_tyK Sg Phi tau ->
    has_tyK Sg Phi G e tau
| TValRK : forall Phi G v Dv tauv,
    g_val Sg v = Some (Dv, tauv) ->
    availK Phi (IVal v) ->
    has_tyK Sg Phi G (EVl v) (app_subst (cS Phi) tauv)
| TTagRK : forall Phi G n D tau0 th e,
    g_tag Sg n = Some (D, tau0) ->
    satK Sg Phi th D ->
    Forall (fun p => wf_tyK Sg Phi (snd p)) th ->
    has_tyK Sg Phi G e (app_subst (cS Phi) (app_subst th tau0)) ->
    has_tyK Sg Phi G (ETag n th e) (app_subst (cS Phi) (TNom n th))
| TNeedRK : forall Phi G f Df S args,
    g_fn Sg f = Some (Df, S, None) ->
    availK Phi (IFn f) ->
    Forall2 (fun e tau => has_tyK Sg Phi G e (app_subst (cS Phi) tau))
            args (fs_params S) ->
    has_tyK Sg Phi G (ECallA f args) (app_subst (cS Phi) (fs_ret S))
| TCallRK : forall Phi G f Df S body th sm args,
    g_fn Sg f = Some (Df, S, Some body) ->
    satK Sg Phi th Df ->
    Forall (fun p => wf_tyK Sg Phi (snd p)) th ->
    smap_okK Phi th sm Df ->
    Forall2 (fun e tau =>
               has_tyK Sg Phi G e (app_subst (cS Phi) (app_subst th tau)))
            args (fs_params S) ->
    has_tyK Sg Phi G (ECallD f th sm args)
           (app_subst (cS Phi) (app_subst th (fs_ret S)))
| TMethRK : forall Phi G e0 tau0 m Dm S rt0 th0 args,
    g_mth Sg m = Some (Dm, S) ->
    has_tyK Sg Phi G e0 tau0 ->
    In (IMth rt0 m th0) (cA Phi) ->
    (* the provision's receiver type is the receiver's type, after σ *)
    app_subst (cS Phi) rt0 = app_subst (cS Phi) tau0 ->
    (* dispatch demands exactly one hit [D1], judged modulo σ: provisions
       are keyed by the receiver's (declaration, static bindings) identity
       compared through the merged substitution [D62] — two spellings of
       one merged atom are one provision, not an ambiguity *)
    (forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) tau0 m i1 ->
                app_subst_item (cS Phi) i1
                = app_subst_item (cS Phi) (IMth rt0 m th0)) ->
    Forall2 (fun e tau =>
               has_tyK Sg Phi G e
                      (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
            args (fs_params S) ->
    has_tyK Sg Phi G (EMeth e0 m (IMth rt0 m th0) args)
           (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) (fs_ret S))
| TMatchRK : forall Phi G e0 tau0 ms arms tau,
    has_tyK Sg Phi G e0 tau0 ->
    (* scrutinee-type regularity, of the σ-image: elaboration always knows
       it, and it is what the proof inverts (the type as written may be an
       abstract symbol whose binding carries the members) *)
    wf_tyK Sg Phi (app_subst (cS Phi) tau0) ->
    members (app_subst (cS Phi) tau0) = Some ms ->
    (* regularity, made local: a wf scrutinee type has distinct heads, and
       find_arm needs that to pick the arm the injection was typed at *)
    NoDup (map member_head ms) ->
    Forall2 (arm_matches Sg) ms arms ->
    Forall2 (fun mem arm =>
               has_tyK Sg Phi ((arm_var arm, member_payload Sg mem) :: G)
                      (arm_body arm) tau) ms arms ->
    has_tyK Sg Phi G (EMatch e0 arms) tau
| TLetRK : forall Phi G x e1 e2 tau1 tau,
    has_tyK Sg Phi G e1 tau1 ->
    has_tyK Sg Phi ((x, tau1) :: G) e2 tau ->
    has_tyK Sg Phi G (ELet x e1 e2) tau
| TBindTyRK : forall Phi G t tau' e tau,
    g_ty Sg t = true ->
    (* This is reserved: binding the receiver symbol itself would let a
       program capture the receiver substitution (machine-refuted end to
       end: a well-typed main evaluating to Err) *)
    t <> this_sym ->
    (* freshness: no shadowing of an in-force type symbol (deviation 7) *)
    ~ In (ITy t) (cA Phi) ->
    wf_tyK Sg Phi tau' ->
    has_tyK Sg (bind_ty t tau' Phi) G e tau ->
    (* the result type may not escape the bind's scope: it must already be
       well-formed outside (a batch-2 agent refuted preservation without
       this — subsumption under the extended σ retypes the body at TAbs t,
       which then leaks) *)
    wf_tyK Sg Phi tau ->
    has_tyK Sg Phi G (EBindTy t tau' e) tau
| TBindValRK : forall Phi G v Dv tauv e1 e tau,
    g_val Sg v = Some (Dv, tauv) ->
    sat0K Sg Phi Dv ->
    has_tyK Sg Phi G e1 (app_subst (cS Phi) tauv) ->
    has_tyK Sg (add_item (IVal v) Phi) G e tau ->
    has_tyK Sg Phi G (EBindVal v e1 e) tau
| TBindFnRK : forall Phi G f Df S f' D' S' body sm e tau,
    g_fn Sg f = Some (Df, S, None) ->
    g_fn Sg f' = Some (D', S', Some body) ->
    sat0K Sg Phi Df ->
    sat0K Sg Phi D' ->
    smap_okK Phi (id_app D') sm D' ->
    (* signature match after the substitutions in force; never inferred [D27] *)
    sig_eq (cS Phi) S' S ->
    has_tyK Sg (add_item (IFn f) Phi) G e tau ->
    has_tyK Sg Phi G (EBindFn f f' sm e) tau
| TBindMthRK : forall Phi G tauk m Dm S th f' D' S' body sm tau0' rest e tau,
    g_mth Sg m = Some (Dm, S) ->
    wf_tyK Sg Phi tauk ->
    satK Sg Phi th Dm ->
    Forall (fun p => wf_tyK Sg Phi (snd p)) th ->
    g_fn Sg f' = Some (D', S', Some body) ->
    sat0K Sg Phi D' ->
    smap_okK Phi (id_app D') sm D' ->
    (* the provider's first parameter is the receiver *)
    fs_params S' = tau0' :: rest ->
    ty_eq (cS Phi) tau0' tauk ->
    map (app_subst (cS Phi)) rest
      = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params S) ->
    app_subst (cS Phi) (fs_ret S')
      = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret S) ->
    has_tyK Sg (add_item (IMth tauk m th) Phi) G e tau ->
    has_tyK Sg Phi G (EBindMth tauk m th f' sm e) tau.

Inductive item_okK (Sg : gsig) (Phi : ctx) : item -> Prop :=
| OkTyK : forall t, g_ty Sg t = true -> t <> this_sym ->
    item_okK Sg Phi (ITy t)
| OkValK : forall v Dv tauv,
    g_val Sg v = Some (Dv, tauv) -> sat0K Sg Phi Dv -> item_okK Sg Phi (IVal v)
| OkFnK : forall f Df S b,
    g_fn Sg f = Some (Df, S, b) -> sat0K Sg Phi Df -> item_okK Sg Phi (IFn f)
| OkMthK : forall rt m Dm S th,
    g_mth Sg m = Some (Dm, S) ->
    wf_tyK Sg Phi rt ->
    satK Sg Phi th Dm ->
    Forall (fun p => wf_tyK Sg Phi (snd p)) th ->
    item_okK Sg Phi (IMth rt m th).

(* The telescoping rule [D20], now with merging [D43]: each item is checked
   with only the items to its left in force (under the substitution merged
   so far), and then folded in — appended if its key is fresh, unified with
   the earlier occurrence otherwise.  The judgment relates a starting
   context to the context the whole telescope leaves in force. *)
Inductive wf_teleK (Sg : gsig) : ctx -> tele -> ctx -> Prop :=
| WNilK : forall Phi, wf_teleK Sg Phi [] Phi
| WConsK : forall Phi i D Phi' Phi'',
    item_okK Sg Phi i ->
    merge_item Phi i = Some Phi' ->
    wf_teleK Sg Phi' D Phi'' ->
    wf_teleK Sg Phi (i :: D) Phi''.

(* Formation computes exactly the merged context of §4b. *)
Record wf_gsigK (Sg : gsig) : Prop := {
  wf_tag_declK : forall n D taup,
      g_tag Sg n = Some (D, taup) ->
      exists PhiD, wf_teleK Sg empty_ctx D PhiD /\ wf_tyK Sg PhiD taup;
  wf_val_declK : forall v D tauv,
      g_val Sg v = Some (D, tauv) ->
      exists PhiD, wf_teleK Sg empty_ctx D PhiD /\ wf_tyK Sg PhiD tauv;
  wf_fn_declK : forall f D S b,
      g_fn Sg f = Some (D, S, b) ->
      exists PhiD, wf_teleK Sg empty_ctx D PhiD
      /\ Forall (wf_tyK Sg PhiD) (fs_params S)
      /\ wf_tyK Sg PhiD (fs_ret S)
      /\ (forall body, b = Some body ->
             has_tyK Sg PhiD (body_env S) body (fs_ret S));
  wf_mth_declK : forall m D S,
      g_mth Sg m = Some (D, S) ->
      exists PhiD, wf_teleK Sg empty_ctx D PhiD
      /\ Forall (wf_tyK Sg (add_item (ITy this_sym) PhiD)) (fs_params S)
      /\ wf_tyK Sg (add_item (ITy this_sym) PhiD) (fs_ret S)
}.

(* ⊢ P ok: a well-formed signature whose main is defined, closed, unit-valued. *)
Definition wf_progK (Sg : gsig) (main : fsym) : Prop :=
  wf_gsigK Sg /\
  exists body, g_fn Sg main = Some ([], mkSig [] TUnit, Some body).


(* ===================== The counterexample ================================ *)

(* Moss shape (p1 in scratch/narrow, with T's two instantiations spelled
   as two unit tags A and B, and no rebinding of T — the core forbids it):

     assume T { fn .m(): T; }              # m's telescope: [T]
     type W ();                            # narrowed: W mentions no T
     fn get(w: W): A { A () }              # provider, fixed at T = A
     assume T, W.m { fn use_it(w: W): T { w.m() } }
     fn main() {
       bind W.m[T=A] = get;
       match use_it[T=B](W ()) { B _ => () }   # only B's arm
     }

   Under key-only matching the requirement W.m[T=B] of use_it is answered
   by the provision W.m[T=A]: same receiver W, same method. *)
Module CE.

Definition T : tsym := 1.
Definition m : msym := 0.
Definition W : ty := TNom 0 [].
Definition A : ty := TNom 1 [].
Definition B : ty := TNom 2 [].

Definition req  : item := IMth W m [(T, TAbs T)].   (* use_it's requirement *)
Definition prov : item := IMth W m [(T, A)].        (* main's provision *)

Definition get_body : expr := ETag 1 [] EUnit.
Definition use_it_body : expr := EMeth (EVar 0) m req [].
Definition main_body : expr :=
  EBindMth W m [(T, A)] 0 []
    (EMatch (ECallD 1 [(T, B)] [(req, prov)] [ETag 0 [] EUnit])
            [(2, 0, EUnit)]).

Definition Sg : gsig := {|
  g_ty  := fun t => Nat.eqb t T;
  g_tag := fun n => match n with
                    | 0 | 1 | 2 => Some ([], TUnit)
                    | _ => None
                    end;
  g_val := fun _ => None;
  g_fn  := fun f => match f with
                    | 0 => Some ([], mkSig [W] A, Some get_body)
                    | 1 => Some ([ITy T; req], mkSig [W] (TAbs T), Some use_it_body)
                    | 2 => Some ([], mkSig [] TUnit, Some main_body)
                    | _ => None
                    end;
  g_mth := fun mm => match mm with
                     | 0 => Some ([ITy T], mkSig [] (TAbs T))
                     | _ => None
                     end;
  g_fun := fun _ => None
|}.

Definition main : fsym := 2.

Lemma conv : forall Phi G e t t',
  has_tyK Sg Phi G e t -> t = t' -> has_tyK Sg Phi G e t'.
Proof. intros; subst; assumption. Qed.

Lemma wf_nom0 : forall Phi n, n <= 2 -> wf_tyK Sg Phi (TNom n []).
Proof.
  intros Phi n Hn.
  assert (Hg : g_tag Sg n = Some ([], TUnit))
    by (destruct n as [|[|[|n]]]; [reflexivity | reflexivity | reflexivity | lia]).
  eapply WfTagK; [exact Hg | | constructor].
  exists empty_ctx. split; [reflexivity |].
  split; [reflexivity | split; constructor].
Qed.

Lemma sat0_nil : forall Phi, sat0K Sg Phi [].
Proof.
  intros Phi. exists empty_ctx. split; [reflexivity |].
  split; [| constructor]. split; [reflexivity | split; constructor].
Qed.

Definition Phi_T : ctx := mkCtx [ITy T] [].
Definition Phi_use : ctx := mkCtx [ITy T; req] [].
Definition Phi_main : ctx := add_item prov empty_ctx.

Lemma tele_use : wf_teleK Sg empty_ctx [ITy T; req] Phi_use.
Proof.
  eapply WConsK.
  - constructor; [reflexivity | discriminate].
  - reflexivity.
  - eapply WConsK; [| reflexivity | constructor].
    eapply OkMthK.
    + reflexivity.
    + apply wf_nom0; lia.
    + exists Phi_T. split; [reflexivity |].
      split; [reflexivity | split; [| constructor]].
      constructor; [left; reflexivity | constructor].
    + constructor; [| constructor]. constructor. reflexivity.
Qed.

Lemma tele_T : wf_teleK Sg empty_ctx [ITy T] Phi_T.
Proof.
  eapply WConsK; [| reflexivity | constructor].
  constructor; [reflexivity | discriminate].
Qed.

Lemma use_it_typed :
  has_tyK Sg Phi_use (body_env (mkSig [W] (TAbs T))) use_it_body (TAbs T).
Proof.
  eapply conv.
  - eapply TMethRK with (Dm := [ITy T]) (S := mkSig [] (TAbs T)) (tau0 := W).
    + reflexivity.
    + constructor. reflexivity.
    + simpl. right. left. reflexivity.
    + reflexivity.
    + intros i1 Hin [th' Hp]. simpl in Hin.
      destruct Hin as [<- | [<- | []]]; [discriminate | reflexivity].
    + constructor.
  - reflexivity.
Qed.

Lemma main_typed :
  has_tyK Sg empty_ctx (body_env (mkSig [] TUnit)) main_body TUnit.
Proof.
  eapply TBindMthRK with (Dm := [ITy T]) (S := mkSig [] (TAbs T))
    (D' := []) (S' := mkSig [W] A) (body := get_body) (tau0' := W) (rest := []).
  - reflexivity.
  - apply wf_nom0; lia.
  - exists Phi_T. split; [reflexivity |].
    split; [reflexivity | split; [| constructor]].
    constructor; [left; reflexivity | constructor].
  - constructor; [| constructor]. apply wf_nom0; lia.
  - reflexivity.
  - apply sat0_nil.
  - exists empty_ctx. split; [reflexivity | split; [reflexivity | constructor]].
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - (* the match on use_it[T=B](W ()) with B's arm only *)
    eapply TMatchRK with (tau0 := B) (ms := [B]).
    + eapply conv.
      * eapply TCallRK with (Df := [ITy T; req]) (S := mkSig [W] (TAbs T))
          (body := use_it_body).
        -- reflexivity.
        -- (* KEY-ONLY: the requirement W.m[T=B] is "available" because
              the provision W.m[T=A] has the same receiver and method *)
           exists Phi_use. split; [reflexivity |].
           split; [reflexivity | split; [| constructor]].
           constructor; [left; reflexivity |].
           constructor; [| constructor].
           right. split; [reflexivity | constructor].
        -- constructor; [| constructor]. apply wf_nom0; lia.
        -- exists Phi_use. split; [reflexivity | split; [reflexivity |]].
           constructor; [| constructor]. split; [left; reflexivity | reflexivity].
        -- constructor; [| constructor].
           eapply conv.
           ++ eapply TTagRK with (D := []) (tau0 := TUnit).
              ** reflexivity.
              ** exists empty_ctx. split; [reflexivity |].
                 split; [reflexivity | split; constructor].
              ** constructor.
              ** constructor.
           ++ reflexivity.
      * reflexivity.
    + apply wf_nom0; lia.
    + reflexivity.
    + constructor; [intros [] | constructor].
    + constructor; [| constructor]. split; [reflexivity |].
      exists [], TUnit. reflexivity.
    + constructor; [| constructor]. constructor.
Qed.

Lemma wf_sig : wf_gsigK Sg.
Proof.
  constructor.
  - intros n D taup H.
    destruct n as [|[|[|n]]]; simpl in H; try discriminate;
      injection H as <- <-; exists empty_ctx; split; constructor.
  - intros v D tauv H. discriminate.
  - intros f D S b H.
    destruct f as [|[|[|f]]]; simpl in H; try discriminate;
      injection H as <- <- <-.
    + exists empty_ctx. split; [constructor |].
      split; [constructor; [apply wf_nom0; lia | constructor] |].
      split; [apply wf_nom0; lia |].
      intros body Hb. injection Hb as <-.
      eapply conv.
      * eapply TTagRK with (D := []) (tau0 := TUnit).
        -- reflexivity.
        -- exists empty_ctx. split; [reflexivity |].
           split; [reflexivity | split; constructor].
        -- constructor.
        -- constructor.
      * reflexivity.
    + exists Phi_use. split; [exact tele_use |].
      split; [constructor; [apply wf_nom0; lia | constructor] |].
      split; [constructor; reflexivity |].
      intros body Hb. injection Hb as <-. exact use_it_typed.
    + exists empty_ctx. split; [constructor |].
      split; [constructor |]. split; [constructor |].
      intros body Hb. injection Hb as <-. exact main_typed.
  - intros mm D S H.
    destruct mm as [|mm]; simpl in H; try discriminate.
    injection H as <- <-.
    exists Phi_T. split; [exact tele_T |].
    split; [constructor |]. constructor. reflexivity.
Qed.

(* It goes wrong: use_it's call of .m runs `get`, producing an A, and the
   match — typed at B — finds no arm. *)
Lemma main_goes_wrong : interp 20 Sg [] [] main_body = Err.
Proof. vm_compute. reflexivity. Qed.

Theorem keyonly_counterexample :
  wf_progK Sg main
  /\ g_fn Sg main = Some ([], mkSig [] TUnit, Some main_body)
  /\ interp 20 Sg [] [] main_body = Err.
Proof.
  split; [split; [exact wf_sig | eexists; reflexivity] |].
  split; [reflexivity | exact main_goes_wrong].
Qed.

(* The real core rejects the same program — by its own soundness theorem. *)
Theorem counterexample_rejected_by_core : ~ wf_prog Sg main.
Proof.
  intros H.
  destruct (soundness_main 20 H (eq_refl : g_fn Sg main = _)) as [Hn _].
  exact (Hn main_goes_wrong).
Qed.

(* Precisely where: the core's availability compares the method's own
   application, so the requirement read at T = B is not the provision made
   at T = A, while the key-only reading conflates them. *)
Lemma core_availability_distinguishes :
  avail_b Phi_main (app_subst_item [(T, B)] req) = false.
Proof. reflexivity. Qed.

Lemma keyonly_availability_conflates :
  availK Phi_main (app_subst_item [(T, B)] req).
Proof. reflexivity. Qed.

(* Narrowing's role.  Under the D21 identity W would be declared inside
   `assume T` and carry T: requirement W[T=T].m, provision (made at T = A)
   W[T=A].m.  Then the *key* alone already separates the two instantiations
   — the receiver pins T — so key-only matching is not fooled by this
   program.  Narrowing W to its payload's symbols (none) erases exactly the
   entry that made the key informative. *)
Definition W21 (tau : ty) : ty := TNom 0 [(T, tau)].
Definition req21 : item := IMth (W21 (TAbs T)) m [(T, TAbs T)].
Definition prov21 : item := IMth (W21 A) m [(T, A)].

Lemma d21_key_distinguishes :
  key_eqb [] prov21 (app_subst_item [(T, B)] req21) = false.
Proof. reflexivity. Qed.

Lemma narrowed_key_conflates :
  key_eqb [] prov (app_subst_item [(T, B)] req) = true.
Proof. reflexivity. Qed.

(* The counterexample's signature is itself narrowed (every tag is a unit,
   R = ∅), so it is a program of the narrowed calculus: the narrowed core
   rejects it (counterexample_rejected_by_core), key-only matching accepts
   it and goes wrong. *)
Theorem sig_narrowed : narrowed_sig Sg (fun _ => []).
Proof.
  split.
  - intros n D taup H t Ht.
    destruct n as [|[|[|n]]]; simpl in H; try discriminate;
      injection H as <- <-; simpl in Ht; contradiction.
  - intros n D taup H.
    destruct n as [|[|[|n]]]; simpl in H; try discriminate;
      injection H as <- <-; (split; [reflexivity | apply NoDup_nil]).
Qed.

End CE.

Print Assumptions CE.sig_narrowed.
Print Assumptions CE.keyonly_counterexample.
Print Assumptions CE.counterexample_rejected_by_core.
