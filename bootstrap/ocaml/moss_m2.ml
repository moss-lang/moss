(* moss_m2: the oracle, the Rocq front end and interpreter extracted to OCaml.
   Usage: moss_m2 [--pages N] [--argv0 A] [--elab-only] [--prelude P] PROGRAM [ARG...]
          moss_m2 --bundle-text [--prelude P] FILE.moss
          moss_m2 --dump OUTDIR FILE.moss...
   PROGRAM is FILE.moss, which is read, lexed, parsed and bundled by the
   Gallina front end (Parse.load_bundle; this driver only reads files), or
   BUNDLE.bx, a bundle already in ExecutionModel.v's s-expression form (Part I),
   which the OCaml tokenizer below reads.  The bundle is elaborated by the
   Gallina elaborator (Elab.elaborate_sx), and the result is given the
   family fast path (FastPath.fast_program), pruned
   (Prune.oracle_program) and run by an engine proved equal to
   MossX.run_main (MOSS_ENGINE, below; LazyClo.crun_main by default).  The prelude is P, else $MOSS_LIB/prelude.moss, else
   lib/prelude.moss (from the working directory).  --bundle-text prints the
   bundle; --dump writes each file's dump (Parse.print_sx) or `ERROR msg` to
   OUTDIR/<mangled path>.rocq.sx. Exit status: 63 the front end rejected
   the program (a lex, parse or load error), 64 the elaborator rejected it,
   65 a construct the oracle does not model; otherwise 0 normal, the code
   for proc_exit, 134 trap, 3 stuck, 4 unsupported primitive, 5 out of
   fuel. *)

open ExecutionModel
open MossX
open MossX.XS


(* tokens: atoms, "quoted", ( ... ) and [ ... ] *)
let tokenize (s : string) : Sx.sx =
  let n = String.length s in
  let pos = ref 0 in
  let rec skip () =
    if !pos < n then
      match s.[!pos] with
      | ' ' | '\n' | '\t' | '\r' -> incr pos; skip ()
      | _ -> () in
  let stack = ref [] and cur = ref [] and result = ref None in
  while !result = None do
    skip ();
    if !pos >= n then failwith "unexpected end of input";
    match s.[!pos] with
    | '(' | '[' -> incr pos; stack := !cur :: !stack; cur := []
    | ')' | ']' as c ->
        incr pos;
        let done_ = Sx.SL (c = ']', Stdlib.List.rev !cur) in
        (match !stack with
         | [] -> failwith "unbalanced"
         | [_] -> result := Some done_
         | top :: rest -> stack := rest; cur := done_ :: top)
    | '"' ->
        incr pos;
        let b = Buffer.create 16 in
        while s.[!pos] <> '"' do
          if s.[!pos] = '\\' then incr pos;
          Buffer.add_char b s.[!pos]; incr pos
        done;
        incr pos;
        cur := Sx.SA (Buffer.contents b) :: !cur
    | _ ->
        let start = !pos in
        while !pos < n && (match s.[!pos] with
                          | ' ' | '\n' | '\t' | '\r' | '(' | ')' | '[' | ']' -> false
                          | _ -> true) do incr pos done;
        cur := Sx.SA (String.sub s start (!pos - start)) :: !cur
  done;
  match !result with Some r -> r | None -> assert false

let bytes_of_string s = Stdlib.List.init (String.length s) (fun i -> Uint63.of_int (Char.code s.[i]))

(* MOSS_GCLOG=1: after each major cycle, print live and heap words *)
let () =
  if Sys.getenv_opt "MOSS_GCLOG" <> None then begin
    let t0 = Unix.gettimeofday () in
    ignore (Gc.create_alarm (fun () ->
      let s = Gc.stat () in
      Printf.eprintf "[gc %.1fs major=%d live=%dMB heap=%dMB top=%dMB]\n%!"
        (Unix.gettimeofday () -. t0) s.Gc.major_collections
        (s.Gc.live_words / 131072) (s.Gc.heap_words / 131072) (s.Gc.top_heap_words / 131072)))
  end

(* MOSS_MEMPROF=rate: sample allocations; at exit, report the call sites of
   the samples still alive, by their innermost frames *)
let memprof_report = ref (fun () -> ())
let () =
  match Sys.getenv_opt "MOSS_MEMPROF" with
  | None -> ()
  | Some r ->
    let live : (int, (int * Printexc.raw_backtrace)) Hashtbl.t = Hashtbl.create 100000 in
    let ctr = ref 0 in
    let depth = try int_of_string (Sys.getenv "MOSS_MEMPROF_DEPTH") with _ -> 6 in
    let track (info : Gc.Memprof.allocation) =
      incr ctr; let id = !ctr in
      Hashtbl.replace live id (info.n_samples, info.callstack); Some id in
    Gc.Memprof.start ~sampling_rate:(float_of_string r) ~callstack_size:depth
      { Gc.Memprof.null_tracker with
        alloc_minor = track; alloc_major = track;
        promote = (fun id -> Some id);
        dealloc_minor = (fun id -> Hashtbl.remove live id);
        dealloc_major = (fun id -> Hashtbl.remove live id) };
    memprof_report := (fun () ->
      Gc.full_major ();
      Gc.Memprof.stop ();
      let agg = Hashtbl.create 1000 in
      let tot = ref 0 in
      Hashtbl.iter (fun _ (w, bt) ->
        let key = match Printexc.backtrace_slots bt with
          | None -> "?"
          | Some sl -> String.concat " < " (Array.to_list (Array.map (fun s ->
              match Printexc.Slot.location s with
              | Some l -> Printf.sprintf "%s:%d" l.Printexc.filename l.Printexc.line_number
              | None -> "?") sl)) in
        tot := !tot + w;
        Hashtbl.replace agg key (w + (try Hashtbl.find agg key with Not_found -> 0))) live;
      let l = List.sort (fun (_, a) (_, b) -> compare b a) (Hashtbl.fold (fun k v acc -> (k, v) :: acc) agg []) in
      Printf.eprintf "[memprof: %d sampled words live (est %d MB)]\n" !tot (int_of_float (float !tot /. float_of_string r /. 131072.));
      List.iteri (fun i (k, w) -> if i < 40 then Printf.eprintf "%6.2f%% %s\n%!" (100. *. float w /. float !tot) k) l);
    (match Sys.getenv_opt "MOSS_MEMPROF_AT" with
     | Some t ->
       Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ ->
         let f = !memprof_report in memprof_report := (fun () -> ()); f ()));
       ignore (Unix.alarm (int_of_string t))
     | None -> ())

(* the front end's only effect: reading a file (the bundle's `read` oracle) *)
let read_file path =
  try Some (In_channel.with_open_bin path In_channel.input_all) with Sys_error _ -> None

let mangle f =
  let p = if Filename.is_relative f then Filename.concat (Sys.getcwd ()) f else f in
  let p = String.concat "_" (List.filter (fun s -> s <> "") (String.split_on_char '/' p)) in
  p

let () =
  let args = Array.to_list Sys.argv in
  let pages = ref 1 and argv0_override = ref None and elab_only = ref false in
  let prelude = ref (match Sys.getenv_opt "MOSS_LIB" with
                     | Some d -> Filename.concat d "prelude.moss" | None -> "lib/prelude.moss") in
  let bundle_text = ref false and dump_dir = ref None in
  let rec opts = function
    | "--pages" :: n :: rest -> pages := int_of_string n; opts rest
    | "--argv0" :: a :: rest -> argv0_override := Some a; opts rest
    | "--elab-only" :: rest -> elab_only := true; opts rest
    | "--prelude" :: p :: rest -> prelude := p; opts rest
    | "--bundle-text" :: rest -> bundle_text := true; opts rest
    | "--dump" :: d :: rest -> dump_dir := Some d; opts rest
    | rest -> rest in
  let rest = opts (match args with _ :: r -> r | [] -> []) in
  (match !dump_dir with
   | Some d ->
       Stdlib.List.iter (fun f ->
         let text = match read_file f with Some t -> t | None -> failwith ("cannot read " ^ f) in
         let out = match Parse.parse_moss text with
           | Sx.Good a -> Parse.print_sx (Sx.Dump.dump_file a) ^ "\n"
           | Sx.Bad m -> "ERROR " ^ m ^ "\n" in
         Out_channel.with_open_bin (Filename.concat d (mangle f ^ ".rocq.sx"))
           (fun oc -> output_string oc out)) rest;
       exit 0
   | None -> ());
  let file, prog_args = match rest with f :: a -> f, a | [] -> failwith "usage" in
  let t0 = Unix.gettimeofday () in
  let input =
    if Filename.check_suffix file ".moss" then begin
      match Parse.load_bundle read_file (Sys.getcwd ()) !prelude file with
      | Sx.Bad msg -> Printf.eprintf "[moss_m2 front end: %s]\n" msg; exit 63
      | Sx.Good ((entry, prel), ms) ->
          if !bundle_text then (print_string (Parse.print_bundle entry prel ms); exit 0);
          if Sys.getenv_opt "MOSS_REF_TIME" <> None then
            Printf.eprintf "[moss_m2: parsed %d modules in %.3fs]\n%!" (Stdlib.List.length ms) (Unix.gettimeofday () -. t0);
          Parse.bundle_sx entry prel ms
    end else tokenize (In_channel.with_open_bin file In_channel.input_all) in
  let p = match Elab.elaborate_sx input with
    | Sx.Good p -> p
    | Sx.Bad msg ->
        let uns = String.length msg >= 12 && String.sub msg 0 12 = "unsupported:" in
        Printf.eprintf "[moss_m2 elab: %s]\n" msg;
        exit (if uns then 65 else 64) in
  let t1 = Unix.gettimeofday () in
  if !elab_only then begin
    Printf.eprintf "[moss_m2: elaborated in %.3fs: %d fns, %d functor bodies]\n%!" (t1 -. t0)
      (Stdlib.List.length p.ExecutionModel.xo_fns) (Stdlib.List.length p.ExecutionModel.xo_funs);
    exit 0 end;
  (* MOSS_FASTPATH=0: run the elaborated program as is; default: each
     platform family applied keeping only the members used
     (FastPath.fast_program; FastPath.fast_run) *)
  let p = if Sys.getenv_opt "MOSS_FASTPATH" = Some "0" then p else begin
    let t = Unix.gettimeofday () in
    let p' = FastPath.fast_program p in
    if Sys.getenv_opt "MOSS_REF_TIME" <> None then
      Printf.eprintf "[moss_m2: fast path in %.3fs%s]\n%!" (Unix.gettimeofday () -. t)
        (if p' == p then " (no family accepted)" else "");
    p' end in
  (* MOSS_PRUNE=0: run the elaborated program as is; default: the pruned
     program (Prune.oracle_program; Prune.oracle_agrees) *)
  let p = if Sys.getenv_opt "MOSS_PRUNE" = Some "0" then p else begin
    let t = Unix.gettimeofday () in
    let p' = Prune.oracle_program p in
    if Sys.getenv_opt "MOSS_REF_TIME" <> None || p' == p then
      Printf.eprintf "[moss_m2: pruned in %.3fs%s]\n%!" (Unix.gettimeofday () -. t)
        (if p' == p then " (check FAILED: running UNPRUNED)" else " (check accepted)");
    p' end in
  (* MOSS_STATS=1: static context sizes (smap lengths) per node kind, in
     the program as run (pruned unless MOSS_PRUNE=0) *)
  if Sys.getenv_opt "MOSS_STATS" <> None then begin
    let tbl : (string, int list ref) Hashtbl.t = Hashtbl.create 16 in
    let add k sm = let r = try Hashtbl.find tbl k with Not_found -> let r = ref [] in Hashtbl.replace tbl k r; r in
      r := Stdlib.List.length sm :: !r in
    let rec ex e = match e with
      | XVar _ | XUnit | XBreak | XVl _ -> ()
      | XConst (_, e1) | XTag (_, _, e1) | XBindTy (_, _, e1) | XLoop e1 | XReturn e1 | XField (e1, _) | XAssign (_, e1)
      | XBindFunSym (_, _, e1) | XBindPrim (_, _, e1) | XBindFunCopy (_, _, e1) -> ex e1
      | XCallD (_, _, sm, a) -> add "call" sm; Stdlib.List.iter ex a
      | XCallA (_, a) -> Stdlib.List.iter ex a
      | XMeth (e0, _, _, a) -> ex e0; Stdlib.List.iter ex a
      | XMatch (e0, arms) -> ex e0; Stdlib.List.iter (fun (_, b) -> ex b) arms
      | XLet (_, a, b) | XBindVal (_, a, b) | XSeq (a, b) | XWhile (a, b) -> ex a; ex b
      | XIf (a, b, c) -> ex a; ex b; ex c
      | XBindFn (_, _, sm, e1) -> add "bindfn" sm; ex e1
      | XBindMth (_, _, _, _, sm, e1) -> add "bindmth" sm; ex e1
      | XBindFun (_, _, sm, smc, e1) -> add "bindfun" sm; add "bindfun.c" smc; ex e1
      | XBindFunA (_, _, sm, e1) -> add "bindfuna" sm; ex e1
      | XApply (_, _, sm, e1) -> add "apply(expr)" sm; ex e1
      | XApplyR (_, inputs, outputs, e1) -> add "applyr.in" inputs; add "applyr.out" outputs; ex e1
      | XRec fs -> Stdlib.List.iter (fun (_, x) -> ex x) fs
      | XMatchP (a, arms) -> ex a; Stdlib.List.iter (fun (_, b) -> ex b) arms
      | XApplyBr (_, brs, e1) -> Stdlib.List.iter (fun ((_, bs), pr) -> add "applybr(expr)" pr; Stdlib.List.iter bx bs) brs; ex e1
    and bx b = match b with
      | XBVal (_, e1) -> ex e1
      | XBFn (_, _, sm) -> add "xb.fn" sm
      | XBMth (_, _, _, _, sm) -> add "xb.mth" sm
      | XBFun (_, _, sm, smc) -> add "xb.fun" sm; add "xb.fun.c" smc
      | XBFunA (_, _, sm) -> add "xb.funa" sm
      | XBApply (_, _, sm) -> add "xb.apply" sm
      | XBApplyR (_, inputs, outputs) -> add "xb.applyr.in" inputs; add "xb.applyr.out" outputs
      | XBApplyBr (_, brs) -> Stdlib.List.iter (fun ((_, bs), pr) -> add "xb.applybr" pr; Stdlib.List.iter bx bs) brs
      | _ -> () in
    Stdlib.List.iter (fun (_, b) -> ex b) p.ExecutionModel.xo_fns;
    Stdlib.List.iter (fun (_, (_, bs)) -> Stdlib.List.iter bx bs) p.ExecutionModel.xo_funs;
    Stdlib.List.iter (fun (u, (dr, bs)) ->
      let kinds = String.concat " " (Stdlib.List.map (function
        | XBTy _ -> "ty" | XBVal _ -> "val" | XBFn (_,_,sm) -> Printf.sprintf "fn%d" (Stdlib.List.length sm)
        | XBMth (_,_,_,_,sm) -> Printf.sprintf "mth%d" (Stdlib.List.length sm)
        | XBFun (_,_,sm,smc) -> Printf.sprintf "fun%d/%d" (Stdlib.List.length sm) (Stdlib.List.length smc)
        | XBFunA _ -> "funa" | XBApply (v,th,sm) -> Printf.sprintf "apply(u%d,%d)" v (Stdlib.List.length sm)
        | XBApplyR _ -> "applyr"
        | XBFunSym _ -> "funsym" | XBFunCopy _ -> "funcopy" | XBApplyBr _ -> "applybr" | XBPrim _ -> "prim") bs) in
      Printf.eprintf "[moss_m2 stats: functor body u%d: |Dr|=%d binds: %s]\n" u (Stdlib.List.length dr) kinds) p.ExecutionModel.xo_funs;
    let rec sites e = match e with
      | XApply (u, th, sm, e1) -> Printf.eprintf "[moss_m2 stats: XApply u%d |th|=%d |sm|=%d body=%s]\n" u (Stdlib.List.length th) (Stdlib.List.length sm)
          (match e1 with XMeth _ -> "meth" | _ -> "other"); sites e1
      | XVar _ | XUnit | XBreak | XVl _ -> ()
      | XConst (_, e1) | XTag (_, _, e1) | XBindTy (_, _, e1) | XLoop e1 | XReturn e1 | XField (e1, _) | XAssign (_, e1)
      | XBindFunSym (_, _, e1) | XBindPrim (_, _, e1) | XBindFunCopy (_, _, e1) | XBindFn (_,_,_,e1) | XBindMth (_,_,_,_,_,e1)
      | XBindFun (_,_,_,_,e1) | XBindFunA (_,_,_,e1) | XApplyBr (_,_,e1) | XApplyR (_,_,_,e1) -> sites e1
      | XCallD (_, _, _, a) | XCallA (_, a) -> Stdlib.List.iter sites a
      | XMeth (e0, _, _, a) -> sites e0; Stdlib.List.iter sites a
      | XMatch (e0, arms) -> sites e0; Stdlib.List.iter (fun (_, b) -> sites b) arms
      | XLet (_, a, b) | XBindVal (_, a, b) | XSeq (a, b) | XWhile (a, b) -> sites a; sites b
      | XIf (a, b, c) -> sites a; sites b; sites c
      | XRec fs -> Stdlib.List.iter (fun (_, x) -> sites x) fs
      | XMatchP (a, arms) -> sites a; Stdlib.List.iter (fun (_, b) -> sites b) arms in
    if Sys.getenv_opt "MOSS_STATS" = Some "2" then Stdlib.List.iter (fun (_, b) -> sites b) p.ExecutionModel.xo_fns;
    Printf.eprintf "[moss_m2 stats: %d fns, %d functor bodies]\n" (Stdlib.List.length p.ExecutionModel.xo_fns) (Stdlib.List.length p.ExecutionModel.xo_funs);
    Hashtbl.iter (fun k r ->
      let a = Array.of_list !r in Array.sort compare a; let n = Array.length a in
      let tot = Array.fold_left (+) 0 a in
      Printf.eprintf "[moss_m2 stats: %-14s n=%6d mean=%7.1f median=%4d max=%5d]\n%!" k n
        (float tot /. float (max n 1)) (if n = 0 then 0 else a.(n / 2)) (if n = 0 then 0 else a.(n - 1))) tbl
  end;
  let fns = Hashtbl.create 4096 in
  Stdlib.List.iter (fun (f, b) -> Hashtbl.replace fns f b) (Stdlib.List.rev p.ExecutionModel.xo_fns);
  let funs = Hashtbl.create 64 in
  Stdlib.List.iter (fun (u, b) -> Hashtbl.replace funs u b) (Stdlib.List.rev p.ExecutionModel.xo_funs);
  let prims = Array.of_list p.ExecutionModel.xo_prims in
  let argv0 = match !argv0_override with
    | Some a -> a
    | None -> Filename.remove_extension (Filename.basename file) ^ ".wasm" in
  let prog = {
    xp_fn = (fun f -> Hashtbl.find_opt fns f);
    xp_fun = (fun u -> Hashtbl.find_opt funs u);
    xp_inputs = (fun h -> match Stdlib.List.assoc_opt h p.ExecutionModel.xo_inputs with Some ts -> ts | None -> []);
    xp_cast = p.ExecutionModel.xo_cast;
    xp_exports = (fun h -> match Stdlib.List.assoc_opt h p.ExecutionModel.xo_exports with Some sm -> sm | None -> []);
    xp_immutable = p.ExecutionModel.xo_immutable;
    xp_fixed_inst = (fun h -> match Stdlib.List.assoc_opt h p.ExecutionModel.xo_fixed_inst with Some th -> th | None -> []);
    xp_fixed_keys = (fun h -> match Stdlib.List.assoc_opt h p.ExecutionModel.xo_fixed_keys with Some keys -> keys | None -> []);
    xp_bool = p.ExecutionModel.xo_bool; xp_true = p.ExecutionModel.xo_true; xp_false = p.ExecutionModel.xo_false;
    xp_argv = Stdlib.List.map bytes_of_string (argv0 :: prog_args);
    xp_read = (fun name ->
      let path = String.init (Stdlib.List.length name)
                   (fun i -> Char.chr (snd (Uint63.to_int2 (Stdlib.List.nth name i)))) in
      try Some (bytes_of_string (In_channel.with_open_bin path In_channel.input_all))
      with Sys_error _ -> None);
  } in
  let st0 = init_state (Uint63.of_int !pages) in
  (* MOSS_ENGINE: which interpreter runs the program (pruned unless
     MOSS_PRUNE=0).  Each is proved equal to ExecutionModel's interpreter:
     - fold: the program pre-resolved (SlotX), its calls to small
       fns folded (FoldX) and closure-compiled (CloX), run by
       CloX.crun_main; CloX.engine_agrees, on the pruned program;
     - clo: the same without folding (CloX.clo_oracle_agrees);
     - slot: SlotX.rrun_main on the pre-resolved program
       (SlotX.slot_oracle_agrees);
     - ref: ExecutionModel's interpreter itself (XS.run_main);
     - fast: FastX.frun_main, map-keyed environments (FastX.frun_main_eq);
     - lazy (default): split environments and deferred type substitutions, compiled
       directly to closures (LazyCloFacts.crun_main_eq);
     - lazy-ref: the same environments with AST dispatch
       (LazyInterpFacts.frun_main_eq).
     The arrays below are trusted driver code, as the Hashtbl realizing
     program_of's xp_fn above is: they are the hypotheses of
     CloX.engine_agrees (README.md, What is trusted).  `table` realizes
     SlotX.rprog_of's `nlook f (compile_fns sh p)` (the first entry for a
     symbol wins, as nlook); tb is the static bodies of that program, the
     arrays afn'/afun' its bodies folded (R'), and tbl/ftbl the closure
     code of R', closures that read the very arrays they are stored in
     (the knot CloX.tbl_ok describes). *)
  let engine = match Sys.getenv_opt "MOSS_ENGINE" with Some e -> e | None -> "lazy" in
  let r = match engine with
    | "lazy-ref" -> LazyX.frun_main max_int prog p.ExecutionModel.xo_main p.ExecutionModel.xo_env st0
    | "lazy" ->
        (* LazyCloFacts.tbl_ok: the first body for a symbol wins, as in
           program_of's nlook. The compiled closures refer back to these
           arrays, which are completely populated before evaluation starts. *)
        let nf = 1 + List.fold_left (fun n (f,_) -> max n f) 0 p.ExecutionModel.xo_fns in
        let nu = 1 + List.fold_left (fun n (u,_) -> max n u) 0 p.ExecutionModel.xo_funs in
        let a = Array.make nf None and b = Array.make nu None in
        let tbl f = if f >= 0 && f < nf then a.(f) else None in
        let ftbl u = if u >= 0 && u < nu then b.(u) else None in
        List.iter (fun (f,e) -> a.(f) <- Some (LazyClo.cc prog tbl ftbl e)) (List.rev p.ExecutionModel.xo_fns);
        List.iter (fun (u,(dr,bs)) -> b.(u) <- Some (dr, LazyClo.ccbs prog tbl ftbl bs)) (List.rev p.ExecutionModel.xo_funs);
        LazyClo.crun_main tbl max_int p.ExecutionModel.xo_main p.ExecutionModel.xo_env st0
    | "fast" -> FastX.frun_main max_int prog p.ExecutionModel.xo_main p.ExecutionModel.xo_env st0
    | "ref" -> run_main max_int prog p.ExecutionModel.xo_main p.ExecutionModel.xo_env st0
    | "slot" | "clo" | "fold" ->
        let tc = Unix.gettimeofday () in
        let m = SlotX.shapes p in
        let sh f = FastX.tget m f in
        let table l =
          let n = Stdlib.List.fold_left (fun a (k, _) -> max a (k + 1)) 0 l in
          let a = Array.make n None in
          Stdlib.List.iter (fun (k, v) -> match a.(k) with None -> a.(k) <- Some v | Some _ -> ()) l;
          n, a in
        let nf, afn = table (SlotX.compile_fns sh p) in
        let nu, afun = table (SlotX.compile_funs sh p) in
        let rfn f = if f >= 0 && f < nf then afn.(f) else None in
        let rfun h = if h >= 0 && h < nu then afun.(h) else None in
        let rp = { SlotX.rp_x = prog; SlotX.rp_fn = rfn; SlotX.rp_fun = rfun } in
        if engine = "slot" then begin
          if Sys.getenv_opt "MOSS_REF_TIME" <> None then
            Printf.eprintf "[moss_m2: slot-compiled in %.3fs]\n%!" (Unix.gettimeofday () -. tc);
          SlotX.rrun_main max_int rp p.ExecutionModel.xo_main p.ExecutionModel.xo_env st0
        end else begin
          let afn, afun =
            if engine <> "fold" || ExecutionModel.xreify prog then afn, afun else begin
              let tb f = match rfn f with Some (bs, _) -> Some bs | None -> None in
              let m = 200 and n = 6 in
              Array.map (function
                | Some (bs, bd) -> Some (FoldX.fold_e tb m n bs, FoldX.fold_e tb m n bd) | None -> None) afn,
              Array.map (function
                | Some (dr, bs) -> Some (dr, Stdlib.List.map (FoldX.fold_b tb m n) bs) | None -> None) afun
            end in
          let rp = { SlotX.rp_x = prog;
                     SlotX.rp_fn = (fun f -> if f >= 0 && f < nf then afn.(f) else None);
                     SlotX.rp_fun = (fun h -> if h >= 0 && h < nu then afun.(h) else None) } in
          let tarr = Array.make nf None and farr = Array.make nu None in
          let tbl f = if f >= 0 && f < nf then tarr.(f) else None in
          let ftbl h = if h >= 0 && h < nu then farr.(h) else None in
          Array.iteri (fun f e -> match e with
            | Some (bs, bd) -> tarr.(f) <- Some (CachedClo.cc rp tbl ftbl bs, CachedClo.cc rp tbl ftbl bd)
            | None -> ()) afn;
          Array.iteri (fun h e -> match e with
            | Some (dr, bs) -> farr.(h) <- Some ((dr, CachedClo.ccbs rp tbl ftbl bs), CachedClo.prepare_scalar_cache dr bs)
            | None -> ()) afun;
          if Sys.getenv_opt "MOSS_REF_TIME" <> None then
            Printf.eprintf "[moss_m2: closure-compiled in %.3fs]\n%!" (Unix.gettimeofday () -. tc);
          CachedClo.crun_main tbl max_int p.ExecutionModel.xo_main p.ExecutionModel.xo_env st0
        end
    | e -> Printf.eprintf "[moss_m2: unknown MOSS_ENGINE %s]\n" e; exit 2 in
  let t2 = Unix.gettimeofday () in
  !memprof_report ();
  if Sys.getenv_opt "MOSS_REF_TIME" <> None then
    Printf.eprintf "[moss_m2: final pages %d, heap objects %d]\n" (snd (Uint63.to_int2 r.xr_st.ms_pages)) (snd (Uint63.to_int2 r.xr_st.ms_hnext));
  let out l =
    let b = Buffer.create 4096 in
    Stdlib.List.iter (fun c -> Buffer.add_char b (Char.chr (Uint63.to_int2 c |> snd))) (Stdlib.List.rev l);
    Buffer.contents b in
  print_string (out r.xr_st.ms_out);
  flush stdout;
  prerr_string (out r.xr_st.ms_err);
  if Sys.getenv_opt "MOSS_REF_TIME" <> None then
    Printf.eprintf "[moss_m2: elaborate %.3fs, run %.3fs]\n" (t1 -. t0) (t2 -. t1);
  let unsupported i = try prims.(i) with _ -> "?" in
  match r.xr_out with
  | XOk _ -> exit 0
  | XExit c -> exit (snd (Uint63.to_int2 c))
  | XTrap c ->
      Printf.eprintf "[moss_ref: trap %s]\n"
        (match c with 1 -> "unreachable" | 2 -> "integer divide by zero"
                    | 3 -> "integer overflow" | 4 -> "out of bounds memory access"
                    | 5 -> "invalid conversion to integer" | _ -> string_of_int c);
      exit 134
  | XErr c -> Printf.eprintf "[moss_ref: stuck (XErr %d)]\n" c; exit 3
  | XUns c -> Printf.eprintf "[moss_ref: unsupported primitive %s]\n" (unsupported c); exit 4
  | XFuel -> Printf.eprintf "[moss_ref: out of fuel]\n"; exit 5
  | XRet _ | XBrk -> Printf.eprintf "[moss_ref: stray control transfer]\n"; exit 3
