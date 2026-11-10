(* Concrete primitives and machine state used by the reference interpreter. *)
From Stdlib Require Import List Bool Arith Lia.
From Stdlib Require Import PrimInt63 Uint63 PrimFloat FloatClass PArray ZArith.
From NM Require Import ExecutionModel HTrie.
Import ListNotations.

Lemma xinst_map : forall d th,
  xinst d th = map (fun p => (ITy (fst p), XDType (xground d (snd p)))) th.
Proof. intros d [|p th]; reflexivity. Qed.

Set Implicit Arguments.

Local Open Scope nat_scope.

(* [M1_values] moved to ExecutionModel.v *)

(* [M2_prims] moved to ExecutionModel.v *)

(* [M3_syntax] moved to ExecutionModel.v *)

(* [M4_runtime] moved to ExecutionModel.v *)

(* The machine: a linear memory of 64 KiB pages, each a persistent array of
   its bytes packed four to an element (get_byte), and the output written so far (most recent byte first). *)
(* The GC heap's objects.  A GC array is its elements as a trie over its
   indices (HTrie.v) with a default for the indices never set: no primitive
   array, so the heap's laws hold without the kernel's array axioms
   (proved in XHeapC.v, deleted with the old oracle). *)
Definition garr : Type := (xvalue * trie xvalue)%type.
Definition garr_get (xs : garr) (i : int) : xvalue :=
  match tget (snd xs) i with Some v => v | None => fst xs end.
Definition garr_set (xs : garr) (i : int) (v : xvalue) : garr := (fst xs, tset (snd xs) i v).

Inductive hobj : Type :=
| HNone : hobj
| HBox  : xvalue -> hobj
| HArr  : garr -> int -> hobj.   (* elements, length *)

Record mstate : Type := MS {
  ms_mem   : array (array int);
  ms_pages : int;
  ms_out   : list int;
  ms_err   : list int;
  ms_heap  : trie hobj;           (* the GC heap, by address *)
  ms_hnext : int;
  ms_fds   : list (int * list int);   (* open files: descriptor, bytes left *)
  ms_nextfd : int
}.

(* [M4b_xprog] moved to ExecutionModel.v *)

(* [M4c_xout] moved to ExecutionModel.v *)

(* [M4d_xres] moved to ExecutionModel.v *)

(* ===================== 5. 32-bit arithmetic over int63 =================== *)

Local Open Scope uint63_scope.

Definition mask32 : int := 4294967295.
Definition two32 : int := 4294967296.
Definition two31 : int := 2147483648.
Definition w32 (x : int) : int := x land mask32.
(* the signed reading of an i32, as a two's-complement int63 *)
Definition sgn32 (x : int) : int := if x <? two31 then x else x - two32.
Definition b2i (b : bool) : int := if b then 1 else 0.

Fixpoint popcnt_go (n : nat) (x acc : int) : int :=
  match n with
  | O => acc
  | S k => popcnt_go k (x >> 1) (acc + (x land 1))
  end.

Definition i32_un (o : i32un) (a : int) : int :=
  match o with
  | UEqz => b2i (a =? 0)
  | UClz => if a =? 0 then 32 else head0 a - 31
  | UCtz => if a =? 0 then 32 else tail0 a
  | UPopcnt => popcnt_go 32 a 0
  | UExt8 => let x := a land 255 in if x <? 128 then x else w32 (x - 256)
  | UExt16 => let x := a land 65535 in if x <? 32768 then x else w32 (x - 65536)
  end.

Definition i32_cmp (o : i32cmp) (a b : int) : int :=
  b2i match o with
      | CEq => a =? b
      | CNe => negb (a =? b)
      | CLtS => ltsb (sgn32 a) (sgn32 b)
      | CLtU => a <? b
      | CGtS => ltsb (sgn32 b) (sgn32 a)
      | CGtU => b <? a
      | CLeS => lesb (sgn32 a) (sgn32 b)
      | CLeU => a <=? b
      | CGeS => lesb (sgn32 b) (sgn32 a)
      | CGeU => b <=? a
      end.

(* trap codes *)
Notation trap_unreachable := 1%nat (only parsing).
Notation trap_div0 := 2%nat (only parsing).
Notation trap_overflow := 3%nat (only parsing).
Notation trap_oob := 4%nat (only parsing).
Notation trap_conv := 5%nat (only parsing).

(* Some result, or None with a trap code *)
Definition i32_bin (o : i32bin) (a b : int) : int + nat :=
  let k := b land 31 in
  match o with
  | IAdd => inl (w32 (a + b))
  | ISub => inl (w32 (a - b))
  | IMul => inl (w32 (a * b))
  | IDivS => if b =? 0 then inr trap_div0
             else if (a =? two31) && (b =? mask32) then inr trap_overflow
             else inl (w32 (divs (sgn32 a) (sgn32 b)))
  | IDivU => if b =? 0 then inr trap_div0 else inl (a / b)
  | IRemS => if b =? 0 then inr trap_div0
             else inl (w32 (mods (sgn32 a) (sgn32 b)))
  | IRemU => if b =? 0 then inr trap_div0 else inl (a mod b)
  | IAnd => inl (a land b)
  | IOr => inl (a lor b)
  | IXor => inl (a lxor b)
  | IShl => inl (w32 (a << k))
  | IShrS => inl (w32 (asr (sgn32 a) k))
  | IShrU => inl (a >> k)
  | IRotl => inl (w32 ((a << k) lor (a >> (32 - k))))
  | IRotr => inl (w32 ((a >> k) lor (a << (32 - k))))
  end.

(* ===================== 6. Binary64 helpers =============================== *)

Local Open Scope float_scope.

Definition fshift : int := 2101.   (* PrimFloat's exponent shift *)
Definition two52f : float := 4503599627370496.

Definition is_nan (x : float) : bool := negb (PrimFloat.eqb x x).
Definition is_neg (x : float) : bool :=
  match classify x with
  | NNormal | NSubn | NZero | NInf => true
  | _ => false
  end.

(* The integer part of |x|, for finite |x| < 2^52, as an int63. *)
Definition int_part (x : float) : int :=
  let (m, e) := frshiftexp (PrimFloat.abs x) in
  (* |x| = mant * 2^(E - 53), E = e - shift, mant = normfr_mantissa m *)
  let mant := normfr_mantissa m in
  if (e <=? fshift)%uint63 then 0%uint63
  else (mant >> (53 - (e - fshift)))%uint63.

Definition ftrunc (x : float) : float :=
  if is_nan x then x
  else if two52f <=? PrimFloat.abs x then x
  else let t := of_uint63 (int_part x) in if is_neg x then -t else t.

Definition fcopysign (a b : float) : float :=
  if is_neg b then - (PrimFloat.abs a) else PrimFloat.abs a.

Definition ffloor (x : float) : float :=
  let t := ftrunc x in if x <? t then t - 1 else t.
Definition fceil (x : float) : float :=
  let t := ftrunc x in if t <? x then fcopysign (t + 1) x else t.
Definition is_even_int (f : float) : bool :=
  let h := ftrunc (f / 2) in h * 2 =? f.
Definition fnearest (x : float) : float :=
  if is_nan x then x
  else if two52f <=? PrimFloat.abs x then x
  else
    let f := ffloor x in
    let d := x - f in
    let r := if d <? 0.5 then f
             else if 0.5 <? d then f + 1
             else if is_even_int f then f else f + 1 in
    fcopysign r x.

Definition canon_nan : float := 0 / 0.

Definition fmin (a b : float) : float :=
  if is_nan a || is_nan b then canon_nan
  else if a <? b then a else if b <? a then b
  else if is_neg a then a else b.
Definition fmax (a b : float) : float :=
  if is_nan a || is_nan b then canon_nan
  else if a <? b then b else if b <? a then a
  else if is_neg a then b else a.

Definition f64_bin (o : f64bin) (a b : float) : float :=
  match o with
  | FAdd => a + b
  | FSub => a - b
  | FMul => a * b
  | FDiv => a / b
  | FMin => fmin a b
  | FMax => fmax a b
  | FCopysign => fcopysign a b
  end.

Definition f64_un (o : f64un) (a : float) : float :=
  match o with
  | FAbs => PrimFloat.abs a
  | FNeg => - a
  | FCeil => fceil a
  | FFloor => ffloor a
  | FTrunc => ftrunc a
  | FNearest => fnearest a
  | FSqrt => PrimFloat.sqrt a
  end.

Definition f64_cmp (o : f64cmp) (a b : float) : int :=
  b2i match o with
      | FEq => a =? b
      | FNe => negb (a =? b)
      | FLt => a <? b
      | FGt => b <? a
      | FLe => a <=? b
      | FGe => b <=? a
      end.

Definition two31f : float := 2147483648.
Definition two32f : float := 4294967296.

(* i32 from f64, Wasm's trunc and trunc_sat *)
Definition f2i_abs (t : float) : int := int_part t.  (* t integral, |t| < 2^52 *)

Definition conv_op (c : conv) (w : xvalue) : xvalue + nat :=
  match c, w with
  | CTruncS, XVF64 x =>
      if is_nan x then inr trap_conv
      else let t := ftrunc x in
           if (t <? - two31f) || (two31f <=? t) then inr trap_conv
           else let n := f2i_abs t in
                inl (XVI32 (if is_neg t then w32 (0 - n)%uint63 else n))
  | CTruncU, XVF64 x =>
      if is_nan x then inr trap_conv
      else let t := ftrunc x in
           if (t <=? -1) || (two32f <=? t) then inr trap_conv
           else inl (XVI32 (f2i_abs t))
  | CTruncSatS, XVF64 x =>
      if is_nan x then inl (XVI32 0%uint63)
      else let t := ftrunc x in
           if t <? - two31f then inl (XVI32 two31)
           else if two31f <=? t then inl (XVI32 2147483647%uint63)
           else let n := f2i_abs t in
                inl (XVI32 (if is_neg t then w32 (0 - n)%uint63 else n))
  | CTruncSatU, XVF64 x =>
      if is_nan x then inl (XVI32 0%uint63)
      else let t := ftrunc x in
           if t <=? -1 then inl (XVI32 0%uint63)
           else if two32f <=? t then inl (XVI32 mask32)
           else inl (XVI32 (f2i_abs t))
  | CConvS, XVI32 a =>
      inl (XVF64 (if (a <? two31)%uint63 then of_uint63 a
                  else - (of_uint63 (two32 - a)%uint63)))
  | CConvU, XVI32 a => inl (XVF64 (of_uint63 a))
  | _, _ => inr 0%nat
  end.

(* binary64 <-> its bit pattern, as two 32-bit halves (lo, hi) *)
Definition f64_bits (x : float) : int * int :=
  let s : int := if is_neg x then 2147483648%uint63 else 0%uint63 in
  match classify x with
  | PZero | NZero => (0%uint63, s)
  | PInf | NInf => (0%uint63, (s lor 2146435072)%uint63)
  | NaN => (0%uint63, 2146959360%uint63)
  | _ =>
      let (m, e) := frshiftexp (PrimFloat.abs x) in
      let mant := normfr_mantissa m in       (* 2^52 <= mant < 2^53 *)
      (* |x| = mant * 2^(e - shift - 53); biased exponent = e - shift + 1022 *)
      if (e + 1022 <=? fshift)%uint63 then
        (* subnormal: frac = mant >> (1 - (e - shift + 1022)) *)
        let sh := (fshift - 1021 - e)%uint63 in
        let frac := (mant >> sh)%uint63 in
        ((frac land mask32)%uint63, (s lor (frac >> 32))%uint63)
      else
        let be := (e - fshift + 1022)%uint63 in
        let frac := (mant land 4503599627370495)%uint63 in
        ((frac land mask32)%uint63,
         (s lor (be << 20) lor (frac >> 32))%uint63)
  end.

Definition f64_of_bits (lo hi : int) : float :=
  let neg := (2147483648 <=? hi)%uint63 in
  let be := ((hi >> 20) land 2047)%uint63 in
  let frac := (((hi land 1048575) << 32) lor lo)%uint63 in
  let mag :=
    if (be =? 2047)%uint63 then (if (frac =? 0)%uint63 then 1 / 0 else canon_nan)
    else if (be =? 0)%uint63 then ldshiftexp (of_uint63 frac) (fshift - 1074)%uint63
    else ldshiftexp (of_uint63 (frac lor 4503599627370496)%uint63)
                    (be + fshift - 1075)%uint63 in
  if neg then - mag else mag.

Local Close Scope float_scope.

(* ===================== 7. Memory ========================================= *)

Definition page_bytes : int := 65536.
Definition max_pages : int := 65536.

Definition in_bounds (st : mstate) (a n : int) : bool :=
  (a + n <=? (ms_pages st) * page_bytes)%uint63.

(* A page holds its 65536 bytes four to an element, little-endian: byte
   a is bits 8*(a mod 4) to 8*(a mod 4)+7 of element (a mod 65536)/4.  An
   element is an int63 that only ever holds 32 bits, so a page is 16384
   machine words rather than 65536, a quarter of the memory. *)
Definition get_byte (st : mstate) (a : int) : int :=
  let w := PArray.get (PArray.get (ms_mem st) (a >> 16)) ((a land 65535) >> 2) in
  (w >> ((a land 3) << 3)) land 255.

Definition set_byte (st : mstate) (a b : int) : mstate :=
  let p := a >> 16 in
  let pg := PArray.get (ms_mem st) p in
  let i := (a land 65535) >> 2 in
  let sh := (a land 3) << 3 in
  let w := PArray.get pg i in
  let w' := (w land (4294967295 lxor (255 << sh))) lor ((b land 255) << sh) in
  MS (PArray.set (ms_mem st) p (PArray.set pg i w'))
     (ms_pages st) (ms_out st) (ms_err st) (ms_heap st) (ms_hnext st) (ms_fds st) (ms_nextfd st).

(* little-endian, n bytes, no bounds check *)
Fixpoint load_le (st : mstate) (a : int) (n : nat) : int :=
  match n with
  | O => 0
  | S k => get_byte st a lor (load_le st (a + 1) k << 8)
  end.

Fixpoint store_le (st : mstate) (a v : int) (n : nat) : mstate :=
  match n with
  | O => st
  | S k => store_le (set_byte st a v) (a + 1) (v >> 8) k
  end.

Definition load32 (st : mstate) (a : int) : option int :=
  if in_bounds st a 4 then Some (load_le st a 4) else None.
Definition store32 (st : mstate) (a v : int) : option mstate :=
  if in_bounds st a 4 then Some (store_le st a v 4) else None.

(* a bounded loop over an int counter; the nat only bounds it *)
Definition loop_fuel : nat := Nat.pow 2 34.

Fixpoint int_loop {A : Type} (fuel : nat) (i n : int) (f : int -> A -> option A)
    (acc : A) : option A :=
  match fuel with
  | O => None
  | S k =>
      if (i <? n)%uint63 then
        match f i acc with
        | Some acc' => int_loop k (i + 1) n f acc'
        | None => None
        end
      else Some acc
  end.

Definition fresh_page (_ : unit) : array int := PArray.make 16384 0.

Definition mem_grow (st : mstate) (delta : int) : int * mstate :=
  let old := ms_pages st in
  if (max_pages <? old + delta)%uint63 then (mask32, st)
  else
    match int_loop loop_fuel old (old + delta)
            (fun p m => Some (PArray.set m p (fresh_page tt))) (ms_mem st) with
    | Some mem => (old, MS mem (old + delta) (ms_out st) (ms_err st) (ms_heap st) (ms_hnext st) (ms_fds st) (ms_nextfd st))
    | None => (mask32, st)
    end.

Definition copy_bytes (st : mstate) (dst src n : int) : mstate :=
  (* overlapping-safe: forwards when dst <= src, backwards otherwise *)
  match
    (if (dst <=? src)%uint63 then
       int_loop loop_fuel 0 n
         (fun i s => Some (set_byte s (dst + i) (get_byte s (src + i)))) st
     else
       int_loop loop_fuel 0 n
         (fun i s => let j := (n - 1 - i)%uint63 in
                     Some (set_byte s (dst + j) (get_byte s (src + j)))) st)
  with
  | Some s => s
  | None => st
  end.

Definition fill_bytes (st : mstate) (dst b n : int) : mstate :=
  match int_loop loop_fuel 0 n (fun i s => Some (set_byte s (dst + i) b)) st with
  | Some s => s
  | None => st
  end.

(* fd_write's gather: Some (state with output appended, total) or None (a
   trap: an iovec or its bytes out of bounds) *)
Definition emit_bytes (fd : int) (st : mstate) (base len : int) : option mstate :=
  if in_bounds st base len then
    int_loop loop_fuel 0 len
      (fun i s =>
         let b := get_byte s (base + i) in
         Some (if (fd =? 2)%uint63
               then MS (ms_mem s) (ms_pages s) (ms_out s) (b :: ms_err s) (ms_heap s) (ms_hnext s) (ms_fds s) (ms_nextfd s)
               else MS (ms_mem s) (ms_pages s) (b :: ms_out s) (ms_err s) (ms_heap s) (ms_hnext s) (ms_fds s) (ms_nextfd s)))
      st
  else None.

Definition fd_write (st : mstate) (fd iovs n nw : int) : option (int * mstate) :=
  if negb ((fd =? 1)%uint63 || (fd =? 2)%uint63) then Some (8%uint63, st)  (* EBADF *)
  else
    match int_loop loop_fuel 0 n
            (fun (i : int) (acc : mstate * int) =>
               let (s, tot) := acc in
               let ad := (iovs + 8 * i)%uint63 in
               match load32 s ad, load32 s (ad + 4)%uint63 with
               | Some base, Some len =>
                   match emit_bytes fd s base len with
                   | Some s' => Some (s', (tot + len)%uint63)
                   | None => None
                   end
               | _, _ => None
               end) (st, 0%uint63) with
    | Some (s, tot) =>
        match store32 s nw (w32 tot) with
        | Some s' => Some (0%uint63, s')
        | None => None
        end
    | None => None
    end.

Fixpoint store_bytes (st : mstate) (a : int) (bs : list int) : mstate :=
  match bs with
  | [] => st
  | b :: r => store_bytes (set_byte st a b) (a + 1) r
  end.

Fixpoint sum_lens (args : list (list int)) : int :=
  match args with
  | [] => 0
  | a :: r => (of_nat (List.length a) + 1 + sum_lens r)%uint63
  end.

Definition args_sizes_get (P : xprog) (st : mstate) (pc ps : int) : option mstate :=
  match store32 st pc (of_nat (List.length (xp_argv P))) with
  | Some s => store32 s ps (sum_lens (xp_argv P))
  | None => None
  end.

Fixpoint args_get_go (args : list (list int)) (st : mstate) (pv pb : int)
  : option mstate :=
  match args with
  | [] => Some st
  | a :: r =>
      let n := of_nat (List.length a) in
      if in_bounds st pb (n + 1) then
        match store32 st pv pb with
        | Some s =>
            let s1 := store_bytes s pb (a ++ [0]) in
            args_get_go r s1 (pv + 4) (pb + n + 1)
        | None => None
        end
      else None
  end.

Local Close Scope uint63_scope.

Local Open Scope uint63_scope.

(* WASI files, over xp_read: open reads the whole file (the filesystem is
   an input and does not change during a run), read hands out its bytes. *)
Definition read_bytes (st : mstate) (a n : int) : option (list int) :=
  if in_bounds st a n then
    match int_loop loop_fuel 0 n (fun i acc => Some (get_byte st (a + i) :: acc)) [] with
    | Some r => Some (rev r)
    | None => None
    end
  else None.

Definition set_fds (st : mstate) (fds : list (int * list int)) (next : int) : mstate :=
  MS (ms_mem st) (ms_pages st) (ms_out st) (ms_err st) (ms_heap st) (ms_hnext st) fds next.

Fixpoint lookup_fd (fd : int) (fds : list (int * list int)) : option (list int) :=
  match fds with
  | [] => None
  | (f, bs) :: r => if f =? fd then Some bs else lookup_fd fd r
  end.

Fixpoint update_fd (fd : int) (bs : list int) (fds : list (int * list int))
  : list (int * list int) :=
  match fds with
  | [] => []
  | (f, b) :: r => if f =? fd then (f, bs) :: r else (f, b) :: update_fd fd bs r
  end.

Definition path_open (P : xprog) (st : mstate) (path len ret : int) : option (int * mstate) :=
  match read_bytes st path len with
  | Some name =>
      match xp_read P name with
      | Some bytes =>
          let fd := ms_nextfd st in
          match store32 (set_fds st ((fd, bytes) :: ms_fds st) (fd + 1)) ret fd with
          | Some s => Some (0, s)
          | None => None
          end
      | None => Some (44, st)   (* ENOENT *)
      end
  | None => None
  end.

(* copy up to `len` bytes of `bs` to memory at `base`: the state, what is
   left, how many were copied *)
Definition fill_from (st : mstate) (base len : int) (bs : list int)
  : option (mstate * list int * int) :=
  if in_bounds st base len then
    int_loop loop_fuel 0 len
      (fun i acc =>
         match acc with
         | (s, b :: rest, n) => Some (set_byte s (base + i) b, rest, n + 1)
         | (s, [], n) => Some (s, [], n)
         end) (st, bs, 0)
  else None.

Definition fd_read (st : mstate) (fd iovs n nr : int) : option (int * mstate) :=
  match lookup_fd fd (ms_fds st) with
  | None => Some (8, st)   (* EBADF *)
  | Some bs =>
      match int_loop loop_fuel 0 n
              (fun i acc =>
                 match acc with
                 | (s, rest, tot) =>
                     let ad := iovs + 8 * i in
                     match load32 s ad, load32 s (ad + 4) with
                     | Some base, Some len =>
                         match fill_from s base len rest with
                         | Some (s', rest', k) => Some (s', rest', tot + k)
                         | None => None
                         end
                     | _, _ => None
                     end
                 end) (st, bs, 0) with
      | Some (s, rest, tot) =>
          let s1 := set_fds s (update_fd fd rest (ms_fds s)) (ms_nextfd s) in
          match store32 s1 nr tot with
          | Some s2 => Some (0, s2)
          | None => None
          end
      | None => None
      end
  end.

Definition fd_close (st : mstate) (fd : int) : int * mstate :=
  match lookup_fd fd (ms_fds st) with
  | Some _ => (0, set_fds st (filter (fun p => negb (fst p =? fd)) (ms_fds st)) (ms_nextfd st))
  | None => (8, st)
  end.

Local Close Scope uint63_scope.

(* ===================== 7b. binary32, i64, the GC heap ==================== *)

Local Open Scope float_scope.

(* Round a binary64 to the nearest binary32 value (ties to even), kept as a
   binary64.  For +, -, *, / and sqrt, rounding the binary64 result again is
   the correctly rounded binary32 result (53 >= 2*24 + 2). *)
Definition round32 (x : float) : float :=
  match classify x with
  | PNormal | NNormal | PSubn | NSubn =>
      let (m, e) := frshiftexp (PrimFloat.abs x) in
      let mant := normfr_mantissa m in
      (* binary32 normal iff e >= 1976; drop = 53 - precision kept *)
      let drop := if (1976 <=? e)%uint63 then 29%uint63 else (29 + (1976 - e))%uint63 in
      let r :=
        if (55 <=? drop)%uint63 then 0
        else
          let q := (mant >> drop)%uint63 in
          let rem := (mant - (q << drop))%uint63 in
          let half := (1 << (drop - 1))%uint63 in
          let q' := if (half <? rem)%uint63 || ((rem =? half)%uint63 && (q land 1 =? 1)%uint63)
                    then (q + 1)%uint63 else q in
          ldshiftexp (of_uint63 q') (e - 53 + drop)%uint63 in
      let r := if 340282356779733661637539395458142568448 <=? r then 1 / 0 else r in
      if is_neg x then - r else r
  | _ => x
  end.

Definition f32_bits (x : float) : int :=
  let s : int := if is_neg x then 2147483648%uint63 else 0%uint63 in
  match classify x with
  | PZero | NZero => s
  | PInf | NInf => (s lor 2139095040)%uint63
  | NaN => 2143289344%uint63
  | _ =>
      let (m, e) := frshiftexp (PrimFloat.abs x) in
      let mant := normfr_mantissa m in
      if (1976 <=? e)%uint63 then
        (s lor ((e - 1975) << 23) lor ((mant >> 29) land 8388607))%uint63
      else (s lor (mant >> (2005 - e)))%uint63
  end.

Definition f32_of_bits (b : int) : float :=
  let neg := (2147483648 <=? b)%uint63 in
  let be := ((b >> 23) land 255)%uint63 in
  let frac := (b land 8388607)%uint63 in
  let mag :=
    if (be =? 255)%uint63 then (if (frac =? 0)%uint63 then 1 / 0 else canon_nan)
    else if (be =? 0)%uint63 then ldshiftexp (of_uint63 frac) (fshift - 149)%uint63
    else ldshiftexp (of_uint63 (frac lor 8388608)%uint63) (be + fshift - 150)%uint63 in
  if neg then - mag else mag.

Local Close Scope float_scope.

(* i64 over Z, kept in [0, 2^64) *)
Local Open Scope Z_scope.
Definition two64 : Z := 18446744073709551616.
Definition two63z : Z := 9223372036854775808.
Definition two32z : Z := 4294967296.
Definition w64 (z : Z) : Z := z mod two64.
Definition sgn64 (z : Z) : Z := if z <? two63z then z else z - two64.
Definition zb2 (b : bool) : int := if b then 1%uint63 else 0%uint63.

Fixpoint zpopcnt (n : nat) (z acc : Z) : Z :=
  match n with
  | O => acc
  | S k => zpopcnt k (Z.shiftr z 1) (acc + (z mod 2))
  end.

Definition i64_bin (o : i32bin) (a b : Z) : Z + nat :=
  let k := b mod 64 in
  match o with
  | IAdd => inl (w64 (a + b))
  | ISub => inl (w64 (a - b))
  | IMul => inl (w64 (a * b))
  | IDivS => if b =? 0 then inr trap_div0
             else if (a =? two63z) && (b =? two64 - 1) then inr trap_overflow
             else inl (w64 (Z.quot (sgn64 a) (sgn64 b)))
  | IDivU => if b =? 0 then inr trap_div0 else inl (a / b)
  | IRemS => if b =? 0 then inr trap_div0 else inl (w64 (Z.rem (sgn64 a) (sgn64 b)))
  | IRemU => if b =? 0 then inr trap_div0 else inl (a mod b)
  | IAnd => inl (Z.land a b)
  | IOr => inl (Z.lor a b)
  | IXor => inl (Z.lxor a b)
  | IShl => inl (w64 (Z.shiftl a k))
  | IShrS => inl (w64 (Z.shiftr (sgn64 a) k))
  | IShrU => inl (Z.shiftr a k)
  | IRotl => inl (w64 (Z.lor (Z.shiftl a k) (Z.shiftr a (64 - k))))
  | IRotr => inl (w64 (Z.lor (Z.shiftr a k) (Z.shiftl a (64 - k))))
  end.

Definition i64_cmp (o : i32cmp) (a b : Z) : int :=
  zb2 match o with
      | CEq => a =? b
      | CNe => negb (a =? b)
      | CLtS => sgn64 a <? sgn64 b
      | CLtU => a <? b
      | CGtS => sgn64 b <? sgn64 a
      | CGtU => b <? a
      | CLeS => sgn64 a <=? sgn64 b
      | CLeU => a <=? b
      | CGeS => sgn64 b <=? sgn64 a
      | CGeU => b <=? a
      end.

Definition zext (bits : Z) (a : Z) : Z :=
  let x := a mod (2 ^ bits) in
  if x <? 2 ^ (bits - 1) then x else w64 (x - 2 ^ bits).

Definition i64_un (o : i64un) (a : Z) : xvalue :=
  match o with
  | U64Eqz => XVI32 (zb2 (a =? 0))
  | U64Clz => XVI64 (if a =? 0 then 64 else 63 - Z.log2 a)
  | U64Ctz => XVI64 (if a =? 0 then 64 else Z.log2 (Z.land a (- a)))
  | U64Popcnt => XVI64 (zpopcnt 64 a 0)
  | U64Ext8 => XVI64 (zext 8 a)
  | U64Ext16 => XVI64 (zext 16 a)
  | U64Ext32 => XVI64 (zext 32 a)
  end.

Definition z_of_halves (lo hi : int) : Z := to_Z lo + to_Z hi * two32z.
Definition lo_of (z : Z) : int := of_Z (z mod two32z).
Definition hi_of (z : Z) : int := of_Z (z / two32z).

(* the integer part of |x| for any finite x, as a Z *)
Definition zint_part (x : PrimFloat.float) : Z :=
  let (m, e) := frshiftexp (PrimFloat.abs x) in
  let mant := to_Z (normfr_mantissa m) in
  let ee := to_Z e - 2101 in
  if ee <=? 0 then 0
  else if ee <=? 53 then Z.shiftr mant (53 - ee) else Z.shiftl mant (ee - 53).

(* a Z of magnitude < 2^64, correctly rounded to binary64 *)
Definition float_of_z (z : Z) : PrimFloat.float :=
  let a := Z.abs z in
  let f := PrimFloat.add (PrimFloat.mul (of_uint63 (of_Z (a / two32z))) 4294967296%float)
                         (of_uint63 (of_Z (a mod two32z))) in
  if z <? 0 then PrimFloat.opp f else f.

(* a Z of magnitude < 2^64, correctly rounded (once) to binary32 *)
Definition float32_of_z (z : Z) : PrimFloat.float :=
  let a := Z.abs z in
  let f :=
    if a <? 16777216 then of_uint63 (of_Z a)
    else
      let drop := Z.log2 a + 1 - 24 in
      let q := Z.shiftr a drop in
      let rem := a - Z.shiftl q drop in
      let half := Z.shiftl 1 (drop - 1) in
      let q' := if (half <? rem) || ((rem =? half) && Z.odd q) then q + 1 else q in
      ldshiftexp (of_uint63 (of_Z q')) (of_Z (drop + 2101)) in
  if z <? 0 then PrimFloat.opp f else f.

Definition conv64_op (c : conv64) (w : xvalue) : xvalue + nat :=
  let fin x := negb (is_nan x) && negb (PrimFloat.eqb (PrimFloat.abs x) (1/0)%float) in
  match c, w with
  | C64Trunc f32 signed sat, (XVF64 x | XVF32 x) =>
      if negb (Bool.eqb f32 (match w with XVF32 _ => true | _ => false end)) then inr 0%nat
      else if is_nan x then (if sat then inl (XVI64 0) else inr trap_conv)
      else
        let t := ftrunc x in
        let mag := if fin x then zint_part t else two64 in
        let v := if is_neg t then - mag else mag in
        let lo := if signed then - two63z else 0 in
        let hi := if signed then two63z else two64 in
        if v <? lo then (if sat then inl (XVI64 (w64 lo)) else inr trap_conv)
        else if hi <=? v then (if sat then inl (XVI64 (w64 (hi - 1))) else inr trap_conv)
        else if (negb signed) && PrimFloat.leb t (-1)%float
             then (if sat then inl (XVI64 0) else inr trap_conv)
        else inl (XVI64 (w64 v))
  | C64Conv to32 signed, XVI64 z =>
      let v := if signed then sgn64 z else z in
      inl (if to32 then XVF32 (float32_of_z v) else XVF64 (float_of_z v))
  | _, _ => inr 0%nat
  end.
Local Close Scope Z_scope.

Definition i64_load (l : load64k) (st : mstate) (a : int) : option Z :=
  let n := match l with L64 => 8 | L64_8S | L64_8U => 1 | L64_16S | L64_16U => 2
                      | L64_32S | L64_32U => 4 end in
  if in_bounds st a (of_nat n) then
    let v := match n with
             | 8 => z_of_halves (load_le st a 4) (load_le st (a + 4)%uint63 4)
             | _ => to_Z (load_le st a n)
             end in
    Some (match l with
          | L64_8S => zext 8 v | L64_16S => zext 16 v | L64_32S => zext 32 v
          | _ => v
          end)
  else None.

Definition i64_store (s : store64k) (st : mstate) (a : int) (z : Z) : option mstate :=
  let n := match s with S64 => 8 | S64_8 => 1 | S64_16 => 2 | S64_32 => 4 end in
  if in_bounds st a (of_nat n) then
    Some (match n with
          | 8 => store_le (store_le st a (lo_of z) 4) (a + 4)%uint63 (hi_of z) 4
          | _ => store_le st a (lo_of z) n
          end)
  else None.

(* The heap: a trie of objects by address; ms_hnext is the next address. *)
Definition heap_max : int := 4194303%uint63.

Definition heap_alloc (st : mstate) (o : hobj) : option (int * mstate) :=
  let n := ms_hnext st in
  if (heap_max <=? n)%uint63 then None
  else match tget (ms_heap st) n with
       | Some _ => None   (* never: no address at or above ms_hnext is in use *)
       | None => Some (n, MS (ms_mem st) (ms_pages st) (ms_out st) (ms_err st)
                             (tset (ms_heap st) n o) (n + 1)%uint63 (ms_fds st) (ms_nextfd st))
       end.

Definition heap_get (st : mstate) (a : int) : hobj :=
  match tget (ms_heap st) a with Some o => o | None => HNone end.

Definition heap_set (st : mstate) (a : int) (o : hobj) : mstate :=
  MS (ms_mem st) (ms_pages st) (ms_out st) (ms_err st)
     (tset (ms_heap st) a o) (ms_hnext st) (ms_fds st) (ms_nextfd st).

Definition ref_of (w : xvalue) : option int :=
  match w with
  | XVRef a | XVTag _ (XVRef a) => Some a
  | _ => None
  end.

Notation trap_null := 6%nat (only parsing).
Notation trap_array_oob := 7%nat (only parsing).
Notation trap_oom := 8%nat (only parsing).

Definition arr_of (st : mstate) (w : xvalue) : option (int * garr * int) :=
  match ref_of w with
  | Some a => match heap_get st a with HArr xs n => Some (a, xs, n) | _ => None end
  | None => None
  end.

Definition gc_prim (p : prim) (ws : list xvalue) (st : mstate) : xout * mstate :=
  match p, ws with
  | PBox rf, [v] =>
      match heap_alloc st (HBox v) with
      | Some (a, s) => (XOk (XVTag rf (XVRef a)), s)
      | None => (XTrap trap_oom, st)
      end
  | PDeref, [r] =>
      match ref_of r with
      | Some a => match heap_get st a with
                  | HBox v => (XOk v, st)
                  | _ => (XTrap trap_null, st)
                  end
      | None => (XErr 0, st)
      end
  | PRefSet, [r; v] =>
      match ref_of r with
      | Some a => match heap_get st a with
                  | HBox _ => (XOk XVUnit, heap_set st a (HBox v))
                  | _ => (XTrap trap_null, st)
                  end
      | None => (XErr 0, st)
      end
  | PArrNew ar, [v; XVI32 n] =>
      if (heap_max <? n)%uint63 then (XTrap trap_oom, st)
      else match heap_alloc st (HArr (v, TE) n) with
           | Some (a, s) => (XOk (XVTag ar (XVRef a)), s)
           | None => (XTrap trap_oom, st)
           end
  | PArrLen, [r] =>
      match arr_of st r with
      | Some (_, _, n) => (XOk (XVI32 n), st)
      | None => (XTrap trap_null, st)
      end
  | PArrGet, [r; XVI32 i] =>
      match arr_of st r with
      | Some (_, xs, n) => if (i <? n)%uint63 then (XOk (garr_get xs i), st)
                           else (XTrap trap_array_oob, st)
      | None => (XTrap trap_null, st)
      end
  | PArrSet, [r; XVI32 i; v] =>
      match arr_of st r with
      | Some (a, xs, n) => if (i <? n)%uint63
                           then (XOk XVUnit, heap_set st a (HArr (garr_set xs i v) n))
                           else (XTrap trap_array_oob, st)
      | None => (XTrap trap_null, st)
      end
  | PArrFill, [r; XVI32 at0; v; XVI32 c] =>
      match arr_of st r with
      | Some (a, xs, n) =>
          if (n <? at0 + c)%uint63 then (XTrap trap_array_oob, st)
          else match int_loop loop_fuel 0 c (fun i ys => Some (garr_set ys (at0 + i)%uint63 v)) xs with
               | Some ys => (XOk XVUnit, heap_set st a (HArr ys n))
               | None => (XTrap trap_oom, st)
               end
      | None => (XTrap trap_null, st)
      end
  | PArrCopy, [r; XVI32 at0; r2; XVI32 start; XVI32 c] =>
      match arr_of st r, arr_of st r2 with
      | Some (a, xs, n), Some (_, ys, m) =>
          if (n <? at0 + c)%uint63 || (m <? start + c)%uint63 then (XTrap trap_array_oob, st)
          else
            (* read the source run first, then write: slice-assignment
               semantics (the source is persistent, so it is its own copy) *)
            let src := ys in
            match int_loop loop_fuel 0 c
                    (fun i zs => Some (garr_set zs (at0 + i)%uint63 (garr_get src (start + i)%uint63)))
                    (match arr_of st r with Some (_, xs', _) => xs' | None => xs end) with
            | Some zs => (XOk XVUnit, heap_set st a (HArr zs n))
            | None => (XTrap trap_oom, st)
            end
      | _, _ => (XTrap trap_null, st)
      end
  | _, _ => (XErr 0, st)
  end.

Definition f32v (x : PrimFloat.float) : xvalue := XVF32 (round32 x).

Definition ext_prim (p : prim) (ws : list xvalue) (st : mstate) : xout * mstate :=
  match p, ws with
  | PF32Bin o, [XVF32 a; XVF32 b] => (XOk (f32v (f64_bin o a b)), st)
  | PF32Cmp o, [XVF32 a; XVF32 b] => (XOk (XVI32 (f64_cmp o a b)), st)
  | PF32Un o, [XVF32 a] => (XOk (f32v (f64_un o a)), st)
  | PConv32 c, [w] =>
      let w' := match w with XVF32 x => XVF64 x | _ => w end in
      match conv_op c w' with
      | inl (XVF64 x) => (XOk (f32v x), st)
      | inl r => (XOk r, st)
      | inr 0 => (XErr 0, st)
      | inr t => (XTrap t, st)
      end
  | PF32Load, [XVI32 a] =>
      if in_bounds st a 4%uint63 then (XOk (XVF32 (f32_of_bits (load_le st a 4))), st)
      else (XTrap trap_oob, st)
  | PF32Store, [XVI32 a; XVF32 x] =>
      if in_bounds st a 4%uint63 then (XOk XVUnit, store_le st a (f32_bits x) 4)
      else (XTrap trap_oob, st)
  | PDemote, [XVF64 x] => (XOk (f32v x), st)
  | PPromote, [XVF32 x] => (XOk (XVF64 x), st)
  | PI32ReinterpF32, [XVF32 x] => (XOk (XVI32 (f32_bits x)), st)
  | PF32ReinterpI32, [XVI32 b] => (XOk (XVF32 (f32_of_bits b)), st)
  | PI64Bin o, [XVI64 a; XVI64 b] =>
      match i64_bin o a b with
      | inl r => (XOk (XVI64 r), st)
      | inr t => (XTrap t, st)
      end
  | PI64Cmp o, [XVI64 a; XVI64 b] => (XOk (XVI32 (i64_cmp o a b)), st)
  | PI64Un o, [XVI64 a] => (XOk (i64_un o a), st)
  | PWrap, [XVI64 a] => (XOk (XVI32 (lo_of a)), st)
  | PExtendS, [XVI32 a] => (XOk (XVI64 (w64 (to_Z (sgn32 a)))), st)
  | PExtendU, [XVI32 a] => (XOk (XVI64 (to_Z a)), st)
  | PI64Load l, [XVI32 a] =>
      match i64_load l st a with
      | Some z => (XOk (XVI64 z), st)
      | None => (XTrap trap_oob, st)
      end
  | PI64Store s, [XVI32 a; XVI64 z] =>
      match i64_store s st a z with
      | Some st' => (XOk XVUnit, st')
      | None => (XTrap trap_oob, st)
      end
  | PConv64 c, [w] =>
      match conv64_op c w with
      | inl r => (XOk r, st)
      | inr 0 => (XErr 0, st)
      | inr t => (XTrap t, st)
      end
  | PI64ReinterpF64, [XVF64 x] =>
      let (lo, hi) := f64_bits x in (XOk (XVI64 (z_of_halves lo hi)), st)
  | PF64ReinterpI64, [XVI64 z] => (XOk (XVF64 (f64_of_bits (lo_of z) (hi_of z))), st)
  | _, _ => gc_prim p ws st
  end.

(* ===================== 8. Running a primitive ============================ *)

Definition xbool (P : xprog) (b : bool) : xvalue :=
  XVTag (xp_bool P) (XVTag (if b then xp_true P else xp_false P) XVUnit).

Definition load_op (l : loadk) (st : mstate) (a : int) : option int :=
  let n := match l with L32 => 4 | L8S | L8U => 1 | L16S | L16U => 2 end in
  if in_bounds st a (of_nat n) then
    let v := load_le st a n in
    Some (match l with
          | L8S => i32_un UExt8 v
          | L16S => i32_un UExt16 v
          | _ => v
          end)
  else None.

Definition store_op (s : storek) (st : mstate) (a v : int) : option mstate :=
  let n := match s with S32 => 4 | S8 => 1 | S16 => 2 end in
  if in_bounds st a (of_nat n) then Some (store_le st a v n) else None.

Definition okv (w : xvalue) (st : mstate) : xout * mstate := (XOk w, st).
Definition unit_or_trap (o : option mstate) (st : mstate) : xout * mstate :=
  match o with Some s => (XOk XVUnit, s) | None => (XTrap trap_oob, st) end.

(* code 0 in XErr: wrong number or kind of arguments for a primitive *)
Definition run_prim0 (P : xprog) (p : prim) (ws : list xvalue) (st : mstate)
  : xout * mstate :=
  match p, ws with
  | PI32Bin o, [XVI32 a; XVI32 b] =>
      match i32_bin o a b with
      | inl r => okv (XVI32 r) st
      | inr t => (XTrap t, st)
      end
  | PI32Cmp o, [XVI32 a; XVI32 b] => okv (XVI32 (i32_cmp o a b)) st
  | PI32Un o, [XVI32 a] => okv (XVI32 (i32_un o a)) st
  | PF64Bin o, [XVF64 a; XVF64 b] => okv (XVF64 (f64_bin o a b)) st
  | PF64Cmp o, [XVF64 a; XVF64 b] => okv (XVI32 (f64_cmp o a b)) st
  | PF64Un o, [XVF64 a] => okv (XVF64 (f64_un o a)) st
  | PConv c, [w] =>
      match conv_op c w with
      | inl r => okv r st
      | inr 0 => (XErr 0, st)
      | inr t => (XTrap t, st)
      end
  | PLoad l, [XVI32 a] =>
      match load_op l st a with
      | Some v => okv (XVI32 v) st
      | None => (XTrap trap_oob, st)
      end
  | PStore s, [XVI32 a; XVI32 v] => unit_or_trap (store_op s st a v) st
  | PF64Load, [XVI32 a] =>
      if in_bounds st a 8%uint63 then
        okv (XVF64 (f64_of_bits (load_le st a 4) (load_le st (a + 4)%uint63 4))) st
      else (XTrap trap_oob, st)
  | PF64Store, [XVI32 a; XVF64 x] =>
      if in_bounds st a 8%uint63 then
        let (lo, hi) := f64_bits x in
        okv XVUnit (store_le (store_le st a lo 4) (a + 4)%uint63 hi 4)
      else (XTrap trap_oob, st)
  | PMemSize, [] => okv (XVI32 (ms_pages st)) st
  | PMemGrow, [XVI32 n] => let (r, s) := mem_grow st n in okv (XVI32 r) s
  | PMemCopy, [XVI32 dst; XVI32 src; XVI32 n] =>
      if in_bounds st dst n && in_bounds st src n
      then okv XVUnit (copy_bytes st dst src n) else (XTrap trap_oob, st)
  | PMemFill, [XVI32 dst; XVI32 b; XVI32 n] =>
      if in_bounds st dst n then okv XVUnit (fill_bytes st dst b n)
      else (XTrap trap_oob, st)
  | PUnreachable, [] => (XTrap trap_unreachable, st)
  | PFdWrite, [XVI32 fd; XVI32 iovs; XVI32 n; XVI32 nw] =>
      match fd_write st fd iovs n nw with
      | Some (r, s) => okv (XVI32 r) s
      | None => (XTrap trap_oob, st)
      end
  | PProcExit, [XVI32 c] => (XExit c, st)
  | PArgsSizesGet, [XVI32 pc; XVI32 ps] =>
      match args_sizes_get P st pc ps with
      | Some s => okv (XVI32 0%uint63) s
      | None => (XTrap trap_oob, st)
      end
  | PArgsGet, [XVI32 pv; XVI32 pb] =>
      match args_get_go (xp_argv P) st pv pb with
      | Some s => okv (XVI32 0%uint63) s
      | None => (XTrap trap_oob, st)
      end
  | PI32Bool, [XVI32 a] => okv (xbool P (negb (a =? 0)%uint63)) st
  | PPathOpen, [XVI32 _; XVI32 _; XVI32 path; XVI32 len; XVI32 _; XVI64 _; XVI64 _;
                XVI32 _; XVI32 ret] =>
      match path_open P st path len ret with
      | Some (r, s) => okv (XVI32 r) s
      | None => (XTrap trap_oob, st)
      end
  | PFdRead, [XVI32 fd; XVI32 iovs; XVI32 n; XVI32 nr] =>
      match fd_read st fd iovs n nr with
      | Some (r, s) => okv (XVI32 r) s
      | None => (XTrap trap_oob, st)
      end
  | PFdClose, [XVI32 fd] => let (r, s) := fd_close st fd in okv (XVI32 r) s
  | PUnsupported c, _ => (XUns c, st)
  | _, _ => ext_prim p ws st
  end.

(* [M5_interp] moved to ExecutionModel.v: the interpreter is ExecutionModel.XSem, a functor
   over the primitive semantics; here it is instantiated with the concrete
   one above. *)
(* m4, D79 as amended: the memory instructions are `Addr`'s methods (the
   family `Memories`), so the platform's provision of one receives its
   address as an `Addr` (a tag over the I32), and `memory.copy`'s source
   too; `memory.size` and `memory.grow` receive the receiver only to name
   the memory, which is the one this model has.  The primitive reads the
   bare address. *)
Definition unaddr (w : xvalue) : xvalue :=
  match w with XVTag _ (XVI32 a) => XVI32 a | _ => w end.

Definition mem_args (p : prim) (ws : list xvalue) : list xvalue :=
  match p with
  | PMemSize => match ws with [XVTag _ _] => [] | _ => ws end
  | PMemGrow => match ws with [XVTag _ _; n] => [n] | _ => ws end
  | PLoad _ | PStore _ | PF64Load | PF64Store | PF32Load | PF32Store
  | PI64Load _ | PI64Store _ | PMemCopy | PMemFill => map unaddr ws
  | _ => ws
  end.

(* Only the GC primitives touch the GC heap: every other one runs with the
   heap it was given handed back (the old oracle's XHeapC.v read its laws
   off this directly). *)
Definition is_gc_prim (p : prim) : bool :=
  match p with
  | PBox _ | PDeref | PRefSet | PArrNew _ | PArrLen | PArrGet | PArrSet
  | PArrFill | PArrCopy => true
  | _ => false
  end.

Definition keep_heap (st s : mstate) : mstate :=
  MS (ms_mem s) (ms_pages s) (ms_out s) (ms_err s) (ms_heap st) (ms_hnext st)
     (ms_fds s) (ms_nextfd s).

Definition run_prim (P : xprog) (p : prim) (ws : list xvalue) (st : mstate)
  : xout * mstate :=
  if is_gc_prim p then run_prim0 P p (mem_args p ws) st
  else let (o, s) := run_prim0 P p (mem_args p ws) st in (o, keep_heap st s).

(* Numeric operations return the incoming state. The executable can
   share that record rather than reconstructing it to preserve its heap. *)
Definition numeric_prim (p : prim) : bool :=
  match p with
  | PI32Bin _ | PI32Cmp _ | PI32Un _ | PI32Bool
  | PF64Bin _ | PF64Cmp _ | PF64Un _
  | PF32Bin _ | PF32Cmp _ | PF32Un _
  | PI64Bin _ | PI64Cmp _ | PI64Un _ => true
  | _ => false
  end.

Lemma numeric_prim_state : forall P p ws st,
  numeric_prim p = true -> snd (run_prim0 P p ws st) = st.
Proof.
  intros P p ws st Numeric. destruct p; cbn [numeric_prim] in Numeric; try discriminate.
  all: destruct ws as [|a [|b [|c rest]]]; cbn [run_prim0 ext_prim gc_prim]; try reflexivity.
  all: try destruct a; cbn [run_prim0 ext_prim gc_prim]; try reflexivity.
  all: try destruct b; cbn [run_prim0 ext_prim gc_prim]; try reflexivity.
  all: match goal with
    | |- context [i32_bin ?op ?a ?b] => destruct (i32_bin op a b)
    | |- context [i64_bin ?op ?a ?b] => destruct (i64_bin op a b)
    end; reflexivity.
Qed.

Definition run_prim_fast (P : xprog) (p : prim) (ws : list xvalue) (st : mstate) : xout * mstate :=
  if numeric_prim p then run_prim0 P p ws st else run_prim P p ws st.

Lemma run_prim_fast_spec : forall P p ws st, run_prim_fast P p ws st = run_prim P p ws st.
Proof.
  intros P p ws st. unfold run_prim_fast. destruct (numeric_prim p) eqn:Numeric; [|reflexivity].
  assert (Hargs : mem_args p ws = ws) by (destruct p; cbn [numeric_prim] in Numeric; try discriminate; reflexivity).
  assert (Hgc : is_gc_prim p = false) by (destruct p; cbn [numeric_prim] in Numeric; try discriminate; reflexivity).
  pose proof (@numeric_prim_state P p ws st Numeric) as Hstate.
  unfold run_prim. rewrite Hargs, Hgc.
  destruct (run_prim0 P p ws st) as [o st'].
  cbn in Hstate. subst st'. destruct st. reflexivity.
Qed.

Module ConcretePrims <: PRIM_SEMANTICS.
  Definition mstate := mstate.
  Definition run_prim := run_prim.
End ConcretePrims.

Module XS := XSem ConcretePrims.
Export XS.


Local Open Scope uint63_scope.
(* The machine `_start` leaves: `pages` fresh pages, the heap pointer at 16
   holding 1024 (both compilers' `_start`, and the bootstrap interpreter). *)
Definition init_state (pages : int) : mstate :=
  let st := MS (PArray.make max_pages (PArray.make 1 0)) 0 [] []
                TE 0 [] 4 in
  let st1 := snd (mem_grow st pages) in
  store_le st1 16 1024 4.
Local Close Scope uint63_scope.

(* [M6_primsig] moved to ExecutionModel.v *)

Ltac args_shape :=
  repeat match goal with
  | H : Forall2 _ _ (_ :: _) |- _ => inversion H; subst; clear H
  | H : Forall2 _ _ [] |- _ => inversion H; subst; clear H
  | H : xvty _ ?w _ |- _ => destruct w; simpl in H; try contradiction; clear H
  end.

Ltac split_matches :=
  repeat (simpl;
    match goal with
    | |- context [match ?x with _ => _ end] =>
        lazymatch x with
        | context [match _ with _ => _ end] => fail
        | _ => destruct x eqn:?
        end
    | |- context [if ?x then _ else _] => destruct x eqn:?
    end).

(* A primitive applied to arguments of its signature's types yields a value
   of its result type, a trap, or an exit: it never gets stuck. *)
Theorem prim_sound : forall P p ps r ws st,
  prim_sig p = Some (ps, r) ->
  Forall2 (xvty P) ws ps ->
  match fst (run_prim P p ws st) with
  | XOk w => xvty P w r
  | XTrap _ | XExit _ => True
  | _ => False
  end.
Proof.
  intros P p ps r ws st Hsig Hargs. unfold run_prim.
  destruct p; simpl in Hsig;
    repeat match type of Hsig with
    | context [match ?o with _ => _ end] => destruct o
    | context [if ?b then _ else _] => destruct b
    end;
    try discriminate;
    injection Hsig as <- <-;
    args_shape; simpl; unfold okv, unit_or_trap, xbool;
    split_matches; simpl; auto.
  all: try (split; [reflexivity|]; destruct (negb _); auto).
Qed.

(* Track B: the law at the memory primitives' method signatures (Part IV,
   prim_msig): an `Addr` argument is unwrapped by mem_args before the
   primitive reads it. *)
Ltac margs_shape :=
  repeat match goal with
  | H : Forall2 _ _ (_ :: _) |- _ => inversion H; subst; clear H
  | H : Forall2 _ _ [] |- _ => inversion H; subst; clear H
  | H : xvty _ ?w XTAddr |- _ =>
      destruct w as [| ? w | | | | | | |]; simpl in H; try contradiction;
      destruct w; simpl in H; try contradiction; clear H
  | H : xvty _ ?w _ |- _ => destruct w; simpl in H; try contradiction; clear H
  end.

Theorem prim_msound : forall P p ps r ws st,
  prim_msig p = Some (ps, r) ->
  Forall2 (xvty P) ws ps ->
  match fst (run_prim P p ws st) with
  | XOk w => xvty P w r
  | XTrap _ | XExit _ => True
  | _ => False
  end.
Proof.
  intros P p ps r ws st Hsig Hargs. unfold run_prim.
  destruct p; simpl in Hsig;
    repeat match type of Hsig with
    | context [match ?o with _ => _ end] => destruct o
    end;
    try discriminate;
    injection Hsig as <- <-;
    margs_shape; simpl; unfold okv, unit_or_trap, xbool;
    split_matches; simpl; auto.
Qed.

(* Track B: an unmodelled primitive stops the program with XUns. *)
Theorem prim_usound : forall P c ws st,
  fst (run_prim P (PUnsupported c) ws st) = XUns c.
Proof. intros P c ws st. unfold run_prim. simpl. destruct ws; reflexivity. Qed.
