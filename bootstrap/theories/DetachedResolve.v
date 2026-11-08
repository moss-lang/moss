(* Detached-name resolution, including qualified module exports. *)
From Stdlib Require Import List String Bool.
From NM Require Import ExecutionModel NMap Sx Elab WorldSound.
Import ListNotations.
Set Implicit Arguments.
Module DR:=ExecutionModel.Elaboration.

Definition detached_final_world(b:bundle)(st:load_st)(entry:nat):Elab.world:=
 Elab.World(ls_mods st)(ls_order st)entry(ls_syms st)(ls_next_sym st)(ls_files st)
 (Loader.library_dir(b_prelude b)).
Section Final.
Variables(b:bundle)(st:load_st)(entry:nat).
Hypothesis HI:LInv b st.
Notation w:=(detached_final_world b st entry).

Lemma detached_go_target_loaded : forall m segs cur hd j,
 (match cur with EMod i=>exists p,In(p,i)(ls_loaded st)|_=>True end) ->
 go_res b st entry m cur hd segs=Good(Some(EMod j)) ->
 exists p,In(p,j)(ls_loaded st).
Proof.
 intros m segs;induction segs as[|seg rest IH];intros cur hd j Valid Run.
 - destruct cur as[s|i];cbn[go_res]in Run;[discriminate|].
   injection Run as <-;exact Valid.
 - cbn[go_res]in Run;destruct cur as[s|i];[unfold err_in in Run;discriminate|].
   destruct Valid as[p Loaded].
   destruct(@fin_path b st entry HI p i Loaded)as[cm[am[Found[Read[Path Module]]]]].
   rewrite Read in Run.
   destruct(sassoc seg(gm_exp_names cm))as[s|]eqn:Named.
   + eapply (IH(ESym s)seg j);[exact I|exact Run].
   + destruct(sassoc seg(gm_exp_aliases cm))as[k|]eqn:Alias;[|unfold err_in in Run;discriminate].
     eapply (IH(EMod k)seg j);[|exact Run].
     exact(@alias_loaded b st entry HI i p cm am Loaded Found Module true seg k Alias).
Qed.

Lemma detached_path_target_loaded : forall p i m pa j,
 In(p,i)(ls_loaded st) ->nget(ls_mods st)i=Some m ->
 Elab.resolve_path w i pa=Good(Some(EMod j)) ->exists q,In(q,j)(ls_loaded st).
Proof.
 intros p i m pa j Loaded Found Run.
 destruct(@fin_path b st entry HI p i Loaded)as[cm[am[Found'[Read[Path Module]]]]].
 rewrite Found in Found';injection Found' as <-.
 change(get_mod w i=m)in Read.
 unfold Elab.resolve_path in Run;rewrite Read in Run.
 destruct pa as[|hd rest].
 - discriminate Run.
 - destruct(sassoc hd(gm_names m))as[s|]eqn:Named.
   + eapply (@detached_go_target_loaded m rest(ESym s)hd j);[exact I|exact Run].
   + destruct(sassoc hd(gm_aliases m))as[k|]eqn:Alias;[|discriminate].
     eapply (@detached_go_target_loaded m rest(EMod k)hd j);[|exact Run].
     exact(@alias_loaded b st entry HI i p m am Loaded Found Module false hd k Alias).
Qed.
End Final.

Lemma removelast_path_view : forall pa,
 match removelast_path pa with
 |(pre,None)=>pa=[] /\pre=[]
 |(pre,Some n)=>pa=pre++[n]
 end.
Proof.
 induction pa as[|x rest IH];[cbn;auto|].
 destruct rest as[|y rest];[reflexivity|].
 change(match(let '(pre,last):=removelast_path(y::rest)in(x::pre,last))with
 |(pre,None)=>x::y::rest=[] /\pre=[]
 |(pre,Some n)=>x::y::rest=pre++[n]
 end).
 destruct(removelast_path(y::rest))as[pre[n|]]eqn:E.
 - cbn in IH;cbn;f_equal;exact IH.
 - cbn in IH;destruct IH as[Bad _];discriminate.
Qed.

Section Agreement.
Variables(b:bundle)(st:load_st)(entry:nat)(R:nat->list nat)(Fl:string->nat).
Hypothesis HI:LInv b st.
Notation w:=(detached_final_world b st entry).
Notation WW:=(W_read w R Fl).
Lemma detached_scope_read : forall p i n,
 In(p,i)(ls_loaded st) ->
 DR.w_scope WW p DR.NSDetached n=
 option_map DR.ESym(sassoc n(gm_detached(get_mod w i))).
Proof.
 intros p i n Loaded.
 destruct(@fin_path b st entry HI p i Loaded)as[m[am[Found[Read[Path Module]]]]].
 change(get_mod w i=m)in Read;rewrite Read.
 exact(@fin_scope b st entry HI R Fl p i m DR.NSDetached n Loaded Found).
Qed.
Lemma detached_export_read : forall j n,
 (exists q,In(q,j)(ls_loaded st)) ->
 DR.w_exp WW(gm_path(get_mod w j))DR.NSDetached n=
 option_map DR.ESym(sassoc n(gm_exp_detached(get_mod w j))).
Proof.
 intros j n[q Loaded].
 destruct(@fin_path b st entry HI q j Loaded)as[m[am[Found[Read[Path Module]]]]].
 change(get_mod w j=m)in Read;rewrite Read,Path.
 exact(@fin_exp b st entry HI R Fl q j m DR.NSDetached n Loaded Found).
Qed.
Lemma detached_path_read : forall p i pa out,
 In(p,i)(ls_loaded st) ->Elab.resolve_path w i pa=Good out ->
 DR.resolve WW p pa=option_map(ent_of w)out.
Proof.
 intros p i pa out Loaded Run.
 destruct(@fin_path b st entry HI p i Loaded)as[m[am[Found[Read[Path Module]]]]].
 exact(@resolve_path_agrees b st entry HI R Fl p i m pa out Loaded Found Run).
Qed.

Lemma detached_final_agrees : forall p i pa found,
 In(p,i)(ls_loaded st) ->Elab.resolve_detached w i pa=Good found ->
 DR.resolve_detached WW p pa=found.
Proof.
 intros p i [|hd[|next rest]] found Loaded Run.
 - cbn[Elab.resolve_detached]in Run;injection Run as <-;reflexivity.
 - cbn[Elab.resolve_detached]in Run;injection Run as <-.
   change(match DR.w_scope WW p DR.NSDetached hd with
    |Some(DR.ESym s)=>Some s|_=>None end=sassoc hd(gm_detached(get_mod w i))).
   rewrite(@detached_scope_read p i hd Loaded).
   destruct(sassoc hd(gm_detached(get_mod w i)));reflexivity.
 - pose proof(removelast_path_view(hd::next::rest))as View.
   unfold Elab.resolve_detached in Run.
   destruct(removelast_path(hd::next::rest))as[pre[last|]]eqn:Cut.
   2: { cbn in View;destruct View as[Bad _];discriminate. }
   cbn in View.
   assert(PreNN:pre<>[])by(intro E;rewrite E in View;cbn in View;discriminate).
   unfold DR.resolve_detached;rewrite View,rev_app_distr;cbn[rev].
   destruct(rev pre)as[|x xs]eqn:RP.
   { apply(f_equal(@rev string))in RP;rewrite rev_involutive in RP;cbn in RP;contradiction. }
   assert(Back:rev(x::xs)=pre).
   { apply(f_equal(@rev string))in RP;rewrite rev_involutive in RP;symmetry;exact RP. }
   change(match DR.resolve WW p(rev(x::xs))with
    |Some(DR.EMod q)=>match DR.w_exp WW q DR.NSDetached last with Some(DR.ESym s)=>Some s|_=>None end
    |_=>None end=found).
   rewrite Back.
   cbn[rbind]in Run.
   destruct(Elab.resolve_path w i pre)as[out|bad]eqn:PathRun;[|discriminate].
   rewrite(@detached_path_read p i pre out Loaded PathRun).
   destruct out as[[s|j]|];cbn[option_map ent_of rbind]in Run|-*;
     try(injection Run as <-;reflexivity).
   injection Run as <-.
   destruct(@fin_path b st entry HI p i Loaded)as[m[am[Found[Read[Path Module]]]]].
   pose proof(@detached_path_target_loaded b st entry HI p i m pre j Loaded Found PathRun)as Target.
   rewrite(@detached_export_read j last Target).
   destruct(sassoc last(gm_exp_detached(get_mod w j)));reflexivity.
Qed.
End Agreement.

Theorem load_world_detached_resolves : forall b w R Fl,
 load_world b=Good w ->forall i pa found,In i(w_order w) ->
 Elab.resolve_detached w i pa=Good found ->
 DR.resolve_detached(W_read w R Fl)(gm_path(get_mod w i))pa=found.
Proof.
 intros b w R Fl Load i pa found InMod Run;unfold load_world in Load.
 apply rbind_good in Load as([entry st]&Loaded&Load);injection Load as <-.
 destruct(load_spec b _ _ _ _ _ _ Loaded(linv_init b))as[HI _].
 cbn[w_order]in InMod;destruct(proj1(li_order _ _ HI i)InMod)as[p InLoaded].
 destruct(@fin_path b st entry HI p i InLoaded)as[m[am[Found[Read[Path Module]]]]].
 change(get_mod(detached_final_world b st entry)i=m)in Read.
 change(DR.resolve_detached(W_read(detached_final_world b st entry)R Fl)
   (gm_path(get_mod(detached_final_world b st entry)i))pa=found).
 rewrite Read,Path.
 exact(@detached_final_agrees b st entry R Fl HI p i pa found InLoaded Run).
Qed.

Theorem rdetached_rel : forall b w R Fl mi pa s found s',
 load_world b=Good w ->In mi(w_order w) ->st_w s=w ->
 rdetached mi pa s=Good(found,s') ->s'=s /\
 DR.resolve_detached(W_read w R Fl)(gm_path(get_mod w mi))pa=found.
Proof.
 intros b w R Fl mi pa s found s' Load InMod World Run.
 unfold rdetached in Run;rewrite World in Run.
 destruct(Elab.resolve_detached w mi pa)as[result|bad]eqn:E;[|discriminate].
 injection Run as <- <-;split;[reflexivity|].
 eapply load_world_detached_resolves;eassumption.
Qed.
