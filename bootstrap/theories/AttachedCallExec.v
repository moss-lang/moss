(* The source branch for a defined attached method, without inferred outcomes. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame MethodCallCore ReceiverCallEnv.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL canon provided_call fn_info demand_needs check_args].
Theorem attached_code_exec:forall f c e ox r att args s x t s' body,
 fn_body att=Some body->method_attached_code f c e ox r att args s=Good((x,t),s')->
 exists info sf e2 st pre ed sd pairs sn sm ss sg sgstate xs sa u sl th,
 fn_info att s=Good(info,sf)/\receiver_types(receiver_args e r)e sf=Good(e2,st)/\
 demand_needs f c e2 info st=Good((pre,ed),sd)/\
 require_needs(cx_mod c)ed att info sd=Good(pairs,sn)/\smap_at(fi_env info)ed pairs sn=Good(sm,ss)/\
 fn_sig att(tymap ed)ed(Some r)ss=Good(sg,sgstate)/\
 check_args f c e(gs_name att)sg(map AA args)sgstate=Good(xs,sa)/\
 layout_ok e(sg_ret sg)sa=Good(u,sl)/\call_theta ed att sl=Good(th,s')/\
 x=fold_stmts pre(XCallD(gs_id att)th sm(ox::xs))/\t=sg_ret sg.
Proof.
 intros f c e ox r att args s x t s' body Hb H.
 unfold method_attached_code in H;rewrite Hb in H;cbn[is_some]in H.
 apply bind_inv in H as(info&sf&Ei&H);apply bind_inv in H as(e2&st&Et&H).
 change(receiver_setup e r sf=Good(e2,st))in Et;rewrite receiver_setup_view in Et.
 apply bind_inv in H as([pre ed]&sd&Ed&H).
 apply bind_inv in H as(pairs&sn&En&H);apply bind_inv in H as(sm&ss&Es&H).
 apply bind_inv in H as(sg&sgstate&Eg&H);apply bind_inv in H as(xs&sa&Ea&H).
 apply bind_inv in H as(u&sl&El&H);apply bind_inv in H as(th&se&Ec&H);injection H as <- <- <-.
 exists info,sf,e2,st,pre,ed,sd,pairs,sn,sm,ss,sg,sgstate,xs,sa,u,sl,th.
 repeat split;assumption||reflexivity.
Qed.
