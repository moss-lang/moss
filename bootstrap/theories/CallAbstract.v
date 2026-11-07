(* The executable abstract-function branch, using the actual context table. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import InfoPreserve Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabMono BodySoundCore
 EtyTyExec CanonTypes NominalSupport CallArgs.
Import ListNotations.
Set Implicit Arguments.
Module AC := Audit.Elaboration.
Lemma kind_fn_ast : forall W fid, AC.kind_sym W fid=Some AC.DKFn ->
 exists n ps rt body, AC.ast_of W fid=Some(ADFn RPlain n ps rt body).
Proof.
 intros W fid H; unfold AC.kind_sym in H.
 destruct (AC.w_decl W fid) as [d|] eqn:D; [|discriminate].
 destruct (AC.d_ast d) eqn:E; cbn [AC.kind_of] in H; try discriminate.
 match type of E with _=ADFn ?rcv ?n ?ps ?rt ?body =>
 destruct rcv; try discriminate H; exists n,ps,rt,body;
 unfold AC.ast_of; rewrite D; cbn [option_map]; rewrite E; reflexivity end.
Qed.

Section Abstract.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).
Notation WW := (BodySoundCore.W w R Fl).
Notation tc := (fun e t => tr Fl (canon e t)).
Definition abstract_call_code fuel c e fid n sg args : M (xexpr * ety) :=
 let! xs := check_args fuel c e n sg (map AA args) in
 layout_ok e (sg_ret sg) ;;
 ret (XCallA fid xs,sg_ret sg).
Lemma abstract_call_sound : forall fuel, OK w P R Fl fuel ->
 forall c e fid n sg args s x t s' F C pa exp,
 nget (en_fns e) fid = Some sg ->
 abstract_call_code fuel c e fid n sg args s = Good((x,t),s') ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F ->
 AC.resolve_sym WW (AC.fr_mod F) pa = Some fid -> AC.kind_sym WW fid = Some AC.DKFn ->
 (exists fnm ps rt, AC.ast_of WW fid = Some(ADFn RPlain fnm ps rt None)) ->
 AC.Ex WW P F C (AECall pa None args) exp x (tc e t) /\ WFty t /\ Support WW t.
Proof.
 intros fuel HOK c e fid n sg args s x t s' F C pa exp Lookup H Hi Hii Hfi Hc Hp Resolve Kind Decl.
 unfold abstract_call_code in H.
 apply bind_inv in H as (xs & s1 & Hargs & H).
 apply bind_inv in H as (u & s2 & Hlayout & Hret); injection Hret as <- <- <-.
 destruct (ok_check_args fuel c e n sg (map AA args) s xs s1 Hi Hargs) as [Hi1 F1].
 destruct (ok_layout_ok e (sg_ret sg) s1 u s2 Hi1 Hlayout) as [Hi2 F2].
 pose proof (post_back w P R Fl _ _ F F2 Hp) as Hp1.
 destruct (cr_fns_clean _ _ _ _ _ _ _ Hc fid sg Lookup) as [Wps [Wret Wthis]].
 destruct (@es_fns WW e (cr_support _ _ _ _ _ _ _ Hc) fid sg Lookup) as [Sp [Sr Sthis]].
 assert (Both : Forall (fun p => WFty(snd p) /\ Support WW(snd p)) (sg_params sg)).
 { apply Forall_forall; intros p InP; split.
   - apply ety_ok_wf; exact (proj1(Forall_forall _ _) Wps (snd p) (in_map snd _ _ InP)).
   - exact (proj1(Forall_forall _ _) Sp p InP). }
 split; [|split; [exact(ety_ok_wf _ Wret)|exact Sr]].
 eapply AC.ExCallA with (g:=AC.FSig (map (fun p=>tc e(snd p))(sg_params sg)) (tc e(sg_ret sg)));
 [exact Resolve|exact Kind|exact Decl| |].
 - rewrite (cr_fns _ _ _ _ _ _ _ Hc fid), Lookup; reflexivity.
 - exact (@check_args_aa_ok w P R Fl fuel HOK c e n sg args s xs s1 F C Both Hargs Hi Hii Hfi Hc Hp1).
Qed.

Variable b : bundle.
Hypothesis Hload : load_world b = Good w.
Lemma memory_sym_state : forall s ms s', memory_sym s = Good(ms,s') -> s'=s.
Proof.
 intros s ms s' H; unfold memory_sym,lang,the_mod in H.
 cbn [mbind get ret] in H; destruct (sassoc "wasm"%string (st_lang s)); injection H as _ <-; reflexivity.
Qed.

Theorem synth_call_abstract_sound : forall fuel, OK w P R Fl fuel ->
 forall c e pa a args fid fnm ps rt s x t s' F C exp,
 synth_call (S fuel) c e pa a args s = Good((x,t),s') ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F -> st_w s=w ->
 AC.resolve_sym WW (AC.fr_mod F) pa=Some fid -> AC.kind_sym WW fid=Some AC.DKFn ->
 AC.ast_of WW fid=Some(ADFn RPlain fnm ps rt None) ->
 AC.Ex WW P F C (AECall pa a args) exp x (tc e t) /\ WFty t /\ Support WW t.
Proof.
 intros fuel HOK c e pa a args fid fnm ps rt s x t s' F C exp H Hi Hii Hfi Hc Hp Hw Resolve Kind Decl.
 cbn [synth_call] in H.
 apply bind_inv in H as (tg & sp & Hpath & H).
 destruct (rpath_rel w R Fl b Hload (cx_mod c) pa s tg sp Hw (cr_mod_in _ _ _ _ _ _ _ Hc) Hpath) as [-> Path].
 rewrite <- (cr_mod _ _ _ _ _ _ _ Hc) in Path.
 unfold AC.resolve_sym in Resolve; rewrite Path in Resolve.
 destruct tg as [[id|modid]|]; cbn [option_map ent_of] in Resolve; try discriminate Resolve.
 injection Resolve as ->.
 apply bind_inv in H as (g & ss & Hsym & H).
 cbn [option_map ent_of] in Path.
 destruct (sym_loader w R Fl b Hload (AC.fr_mod F) pa fid s g ss Hi Hw Path Hsym) as [-> Hg].
 pose proof (load_world_ids b w Hload fid g Hg) as Id.
 pose proof (kind_sym_read b w R Fl Hload fid g Hg) as Gkind.
 change (AC.kind_sym WW fid = Some(kconv(gs_kind g))) in Gkind.
 rewrite Kind in Gkind; destruct (gs_kind g) eqn:K; try discriminate Gkind.
 change (AC.ast_of (WorldSound.W_read w R Fl) fid=Some(ADFn RPlain fnm ps rt None)) in Decl.
 unfold AC.ast_of in Decl; rewrite (w_decl_read w R Fl fid g Hg) in Decl.
 cbn [option_map AC.d_ast] in Decl; injection Decl as D.
 cbn beta iota in H.
 destruct a as [bs|]; [exfalso; exact(err_not_good _ _ _ _ _ _ H)|].
 apply bind_inv in H as (mem & sm & Hmem & H).
 unfold is_memory_instruction in Hmem; injection Hmem as <- <-.
 apply bind_inv in H as (ms & sm & Hms & H).
 pose proof (@memory_sym_state s ms sm Hms) as ->.
 apply bind_inv in H as (um & su & Hum & H); injection Hum as <- <-.
 unfold is_defined_fn,fn_body in H; rewrite K,D in H; cbn in H.
 destruct (nget (en_fns e) fid) as [sg|] eqn:Lookup.
 - eapply abstract_call_sound; [exact HOK|exact Lookup| |exact Hi|exact Hii|exact Hfi|exact Hc|exact Hp| |exact Kind|].
   + unfold abstract_call_code; exact H.
   + unfold AC.resolve_sym; rewrite Path; reflexivity.
   + exists fnm,ps,rt; change (AC.ast_of (W_read w R Fl) fid=Some(ADFn RPlain fnm ps rt None)); unfold AC.ast_of; rewrite (w_decl_read w R Fl fid g Hg); cbn; rewrite D; reflexivity.
 - exfalso; exact(err_not_good _ _ _ _ _ _ H).
Qed.

Theorem synth_abstract_sound : forall fuel, OK w P R Fl fuel ->
 forall c e pa a args fid fnm ps rt exp s x t s' F C,
 synth (S fuel) c e (AECall pa a args) exp s=Good((x,t),s') ->
 Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> Post w P R Fl s' F -> st_w s=w ->
 AC.resolve_sym WW (AC.fr_mod F) pa=Some fid -> AC.kind_sym WW fid=Some AC.DKFn ->
 AC.ast_of WW fid=Some(ADFn RPlain fnm ps rt None) ->
 AC.Ex WW P F C (AECall pa a args) (option_map (fun t=>tc e t) exp) x (tc e t) /\
 WFty t /\ Support WW t.
Proof.
 intros fuel HOK c e pa a args fid fnm ps rt exp s x t s' F C H Hi Hii Hfi Hc Hp Hw Resolve Kind Decl.
 cbn [synth] in H.
 apply bind_inv in H as ([v vt] & s1 & Hcall & H).
 apply bind_inv in H as (u & s2 & Hinj & Hret); injection Hret as <- <- <-.
 destruct (ok_synth_call fuel c e pa a args s (v,vt) s1 Hi Hcall) as [Hi1 F1].
 destruct (ok_inject_check e vt exp s1 u s2 Hi1 Hinj) as [Hi2 F2].
 pose proof (post_back w P R Fl _ _ F F2 Hp) as Hp1.
 pose proof (mono_group fuel) as Mon; do 11 (apply proj2 in Mon); apply proj1 in Mon.
 eapply synth_call_abstract_sound; [exact HOK|exact (Mon c e pa a args s ((v,vt),s1) Hcall)|eassumption..].
Qed.

Definition DefinedFnStep (fuel : nat) : Prop :=
 forall c e pa a args exp s v t s',
 synth (S fuel) c e (AECall pa a args) exp s=Good((v,t),s') ->
 forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> opt_wf exp -> opt_support WW exp -> Post w P R Fl s' F ->
 (exists fid fnm ps rt body, AC.resolve_sym WW (AC.fr_mod F) pa=Some fid /\
 AC.kind_sym WW fid=Some AC.DKFn /\ AC.ast_of WW fid=Some(ADFn RPlain fnm ps rt (Some body))) ->
 AC.Ex WW P F C (AECall pa a args) (option_map (fun t=>tc e t)exp) v (tc e t) /\ WFty t /\ Support WW t.

Theorem fn_call_step : forall fuel, OK w P R Fl fuel -> DefinedFnStep fuel ->
 forall c e pa a args exp s v t s',
 synth (S fuel) c e (AECall pa a args) exp s=Good((v,t),s') ->
 forall F C, Inv s -> InfoInv s -> FwdInv s -> CxRel w R Fl c e F C -> opt_wf exp -> opt_support WW exp -> Post w P R Fl s' F ->
 (exists fid, AC.resolve_sym WW (AC.fr_mod F) pa=Some fid /\ AC.kind_sym WW fid=Some AC.DKFn) ->
 AC.Ex WW P F C (AECall pa a args) (option_map (fun t=>tc e t)exp) v (tc e t) /\ WFty t /\ Support WW t.
Proof.
 intros fuel HOK Def c e pa a args exp s v t s' H F C Hi Hii Hfi Hc Wx Sx Hp (fid &Resolve &Kind).
 destruct (@kind_fn_ast WW fid Kind) as (fnm &ps &rt &[body|] &Decl).
 - eapply Def; try eassumption. exists fid,fnm,ps,rt,body; auto.
 - assert (Hw : st_w s=w).
   { pose proof(proj2(ok_synth (S fuel) c e (AECall pa a args) exp s (v,t) s' Hi H))as Fr.
     exact(@lt_w w P R Fl s (later_back w P R Fl _ _ Fr (proj1 Hp))). }
   eapply synth_abstract_sound; eassumption.
Qed.
End Abstract.
