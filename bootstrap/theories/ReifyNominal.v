(* The nominal base keeps real source heads and parameter telescopes.
   Unobserved/unresolved payloads stay genuinely opaque. No substitute
   payload is invented. Synthetic machine heads are opaque nullary types. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss NMap BodyTr Elab ReifyData ReifySource ReifyPayload XTyping.
Import ListNotations Audit.Input.
Local Strategy opaque[FUEL ELAB_FUEL reqs skeleton_tag_payload].
Definition reify_machine_head(source:st)(head:nsym):bool:=
 existsb(Nat.eqb head)(seq(st_next source)6).
Definition source_nominal_interface(source:st)(head:nsym):option(tele*option ty):=
 if Nat.eqb head rec_nom then None else
 if reify_machine_head source head then Some([],None)else
 match nget(st_syms source)head with
 | None=>None
 | Some symbol=>
   match gs_kind symbol with
   | KUnit=>Some([],Some TUnit)
   | KTag=>match reqs head source with
       | Bad _=>None
       | Good(parameters,_)=>
           match skeleton_tag_payload head source with
           | Good(payload,_)=>Some(map ITy parameters,Some(tr(reify_field_number source)payload))
           | Bad _=>Some(map ITy parameters,None)
           end
       end
   | _=>None end
 end.
Definition reify_nominal_base(source:st):gsig:=
 mkGsigFull
   (fun t=>negb(Nat.eqb t this_sym)&&match nget(st_syms source)t with
     Some symbol=>kind_eqb(gs_kind symbol)KType|None=>false end)
   (fun n=>match source_nominal_interface source n with
     Some(D,Some payload)=>Some(D,payload)|_=>None end)
   (fun _=>None)(fun _=>None)(fun _=>None)(fun _=>None)
   (fun _=>[])(fun _=>[])(fun _=>([],[]))
   (fun n=>match source_nominal_interface source n with
     Some(D,None)=>Some D|_=>None end).
Definition reify_representation(source:st)(head:nsym):option rep:=
 let base:=st_next source in
 if Nat.eqb head base then Some RI32 else
 if Nat.eqb head(base+1)then Some RI64 else
 if Nat.eqb head(base+2)then Some RF32 else
 if Nat.eqb head(base+3)then Some RF64 else
 if Nat.eqb head(base+4)then Some(RHandle false)else
 if Nat.eqb head(base+5)then Some(RHandle true)else
 match lookup_cast head(runtime_casts(reify_runtime_schema source)base)with
 | Some spec=>Some(if xc_array spec then RArr(xc_held spec)else RRef(xc_held spec))
 | None=>None end.

Lemma nominal_base_no_values : forall source n,g_val(reify_nominal_base source)n=None.
Proof. reflexivity. Qed.
Lemma nominal_base_no_functions : forall source n,g_fn(reify_nominal_base source)n=None.
Proof. reflexivity. Qed.
Lemma nominal_base_no_methods : forall source n,g_mth(reify_nominal_base source)n=None.
Proof. reflexivity. Qed.
Lemma nominal_base_no_functors : forall source n,g_fun(reify_nominal_base source)n=None.
Proof. reflexivity. Qed.
Lemma nominal_base_record_unclaimed : forall source,
 g_tag(reify_nominal_base source)rec_nom=None/\g_opaque(reify_nominal_base source)rec_nom=None.
Proof. intros;split;reflexivity. Qed.
Lemma nominal_opaque_has_no_payload : forall source n D,
 g_opaque(reify_nominal_base source)n=Some D->g_tag(reify_nominal_base source)n=None.
Proof.
 intros source n D E;cbn in E |- *.
 destruct(source_nominal_interface source n)as[[inputs[payload|]]|];try discriminate;reflexivity.
Qed.

Lemma nominal_interface_type_only : forall source n D payload,
 source_nominal_interface source n=Some(D,payload)->Forall(fun i=>is_ty_item i=true)D.
Proof.
 intros source n D payload E;unfold source_nominal_interface in E.
 destruct(Nat.eqb n rec_nom);[discriminate|].
 destruct(reify_machine_head source n);[inversion E;constructor|].
 destruct(nget(st_syms source)n)as[symbol|];[|discriminate].
 destruct(gs_kind symbol);try discriminate.
 - inversion E;constructor.
 - destruct(reqs n source)as[[parameters after]|];[|discriminate].
   destruct(skeleton_tag_payload n source)as[[body final]|];inversion E;subst;
   apply Forall_forall;intros i Member;apply in_map_iff in Member as[t[<- _]];reflexivity.
Qed.
