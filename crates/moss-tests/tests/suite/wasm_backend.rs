//! The self-hosted back end, program by program: each program compiles to
//! a module that runs under Wasmtime to the expected output; where the
//! point is agreement, the oracle runs it to the same output.
//!
//! Ported from the Python bootstrap's `tests/test_build.py`, whose
//! `TestWasmBackend` held the Python back end (gone) to the interpreter;
//! the compiler under test is now the self-hosted one. A program whose
//! `main` assumes `Std` is wrapped (`std_main`) so that the platform's
//! `main` applies `WasiStd` (D52).

use moss_tests::*;

/// A whole program whose `main` assumes `Std`, as one the platform runs.
fn whole(text: &str) -> String {
    source(&std_main(text))
}

/// Both runners print `expected`.
fn agree(entry: &str, args: &[&str], expected: &str) {
    assert_eq!(run(entry, args), expected, "the self-hosted compiler on {entry}");
    assert_eq!(oracle_run(entry, args), expected, "the oracle on {entry}");
}

// What tests/wasi/records.moss prints, one property per line.
const RECORDS_OUT: &str = "ab\nba\nbaq\ncdez\ncd\ngf\nho\n";

// What tests/wasi/nested.moss prints.
const NESTED_OUT: &str = "bca\nfda\nghi\n";

/// What tests/wasi/float.moss prints, one property per line; the comments
/// there say why each is what it is.
pub const FLOAT_OUT: &str = "1414213562\n1414213504\n1414213562\n333333343\n333333333\n0100\n\
     0\n2147483647\n-2147483648\n0\n2147483647\n-3\n\
     2\n4\n-3\n-2\n-2\n-1\n1\n2\n\
     1065353216\n1\n1073741824\n1568669697\n-1\n\
     1\n333333344\n0\n2\n";

// `test_build.TestWasmBackend.test_examples_match_goldens` is
// `examples::examples_match_their_goldens`.

#[test]
fn arithmetic_and_loops() {
    let entry = whole(
        r"assume Std {
  fn triple(n: Int): Int { n.add(n).add(n) }
  fn main() {
    var i = zero;
    while i.lt(triple(one)) {
      putchar(char::w);
      i = i.add(one);
    }
    if true.not() { putchar(char::n) } else { putchar(char::y) }
    putchar(char::newline);
  }
}
",
    );
    assert_eq!(run(&entry, &[]), "wwwy\n");
}

#[test]
fn if_match_preserves_receiver_and_evaluates_condition_once() {
    let entry = whole(
        r"tag True;
tag False;
assume Std {
  tag Box(Char);
  fn Box.pick(c: Bool): Char {
    if c { match this { Box(value) => value } } else if false { char::n } else { char::b }
  }
  fn main() {
    var calls = zero;
    let b = Box(char::a);
    putchar(b.pick(({ calls = calls.add(one); true })));
    if calls.eq(one) { putchar(b.pick(false)); }
    putchar(char::newline);
  }
}
",
    );
    agree(&entry, &[], "ab\n");
}

#[test]
fn if_requires_bool_and_unit_for_an_omitted_else() {
    for text in [
        "fn main() { if () {} }",
        "tag Empty(()); fn main() { if true { Empty(()) }; }",
    ] {
        let entry = source(text);
        rejects(&entry);
        oracle_rejects(&entry);
    }
}

/// D67: `Std` has a `Float`, an f64 under `WasiStd`, and the compiled
/// program gets the same answers as the oracle. A function taking and
/// returning a `Float` is compiled with f64 parameters and results.
#[test]
fn native_float() {
    let entry = whole(
        r"assume Std {
  fn half(x: Float): Float { x.div(float(one.add(one))) }
  fn main() {
    let a = char::A.code();
    putchar(float(a.mul(a)).sqrt().trunc().char());
    let h = half(float(char::a.code()));
    putchar(h.round().char());
    putchar(h.add(float(one)).round().char());
    let nan = float(zero).div(float(zero));
    if nan.ne(nan) { putchar(char::y) } else { putchar(char::n) }
    if nan.ge(nan) { putchar(char::y) } else { putchar(char::n) }
    putchar(float(char::b.code()).neg().abs().trunc().char());
    putchar(char::newline);
  }
}
",
    );
    // 97 / 2 is 48.5, a tie that goes to the even 48; 49.5 goes to 50.
    agree(&entry, &[], "A02ynb\n");
}

/// `Std`'s `Int` is i32 under `WasiStd`, and the oracle computes exactly
/// what its instructions do: arithmetic wraps, division and remainder
/// truncate toward zero (`i32.div_s`, `i32.rem_s`), and a shift count is
/// taken mod 32.
#[test]
fn native_int_is_i32() {
    let entry = whole(
        r"assume Std {
  fn yes(b: Bool) { if b { putchar(char::y) } else { putchar(char::n) } }
  fn main() {
    yes(-7 / 2 == -3);
    yes(-7 % 2 == -1);
    yes(7 % -2 == 1);
    yes(2147483647 + 1 == -2147483647 - 1);
    yes(65536 * 65536 == 0);
    yes(1 << 33 == 2);
    yes(-8 >> 1 == -4);
    yes(-(-2147483647 - 1) == -2147483647 - 1);
    putchar(char::newline);
  }
}
",
    );
    agree(&entry, &[], "yyyyyyyy\n");
}

/// `i32.div_s` traps on the one quotient i32 cannot hold, and so does the
/// oracle.
#[test]
fn native_int_division_overflow_traps() {
    let entry = whole("assume Std {\n  fn main() { let x = (-2147483647 - 1) / -1; }\n}\n");
    assert_eq!(run_module_code(&build(&entry), &[], TRAP), "");
    let out = oracle(&entry, &[]);
    assert_eq!(out.code, TRAP, "{}", out.stderr);
}

/// Units/unions with match, tags with payload binders, and records with
/// field access and destructuring.
#[test]
fn boxed_values_and_match() {
    let cases = [
        (
            "colors",
            r"assume Std {
  tag Red;
  tag Green;
  tag Blue;
  type Color = Red | Green | Blue;
  fn code(c: Color): Char {
    match c { Red => char::r, Green => char::g, Blue => char::b, }
  }
  fn main() { putchar(code(Green)); putchar(code(Red)); putchar(char::newline); }
}
",
            "gr\n",
        ),
        (
            "wrap",
            r"assume Std {
  tag Wrapped(Char);
  tag Missing;
  type Maybe = Missing | Wrapped;
  fn show(m: Maybe) {
    match m { Wrapped(c) => putchar(c), Missing => putchar(char::question), }
    putchar(char::newline);
  }
  fn main() { show(Wrapped(char::w)); show(Missing); }
}
",
            "w\n?\n",
        ),
        (
            "point",
            r"assume Std {
  tag Point { x: Char, y: Char };
  fn Point.show() {
    putchar(char::lparen); putchar(this.x); putchar(char::comma);
    putchar(this.y); putchar(char::rparen); putchar(char::newline);
  }
  fn flip(p: Point): Point {
    match p { Point { x, y } => Point { x = y, y = x }, }
  }
  fn main() {
    let p = Point { x = char::a, y = char::b };
    p.show();
    flip(p).show();
  }
}
",
            "(a,b)\n(b,a)\n",
        ),
    ];
    for (name, text, expected) in cases {
        assert_eq!(run(&whole(text), &[]), expected, "{name}");
    }
}

/// D67: the float half of `Wasm`, as parameters, results, locals, record
/// fields and memory.
#[test]
fn f32_and_f64_are_scalar_types() {
    assert_eq!(run("tests/wasi/float.moss", &[]), FLOAT_OUT);
}

/// D52: a program can assume the primitive context directly, with no
/// `Std` and no prelude bridge — just Wasm instructions and WASI imports.
/// Writes "A\n" through fd_write, then exits 3.
#[test]
fn raw_wasi_program() {
    assert_eq!(run_module_code(&build("tests/wasi/raw.moss"), &[], 3), "A\n");
}

fn contains(module: &[u8], name: &[u8]) -> bool {
    module.windows(name.len()).any(|w| w == name)
}

/// The import section is not a fixed set: `proc_exit` shows up only in the
/// module that calls it, and every function index moves to accommodate
/// it. (The Python back end's native shims always imported `fd_write`,
/// `args_sizes_get` and `args_get`; here, neither program reads its
/// arguments, so neither imports the calls that would.)
#[test]
fn wasi_imports_are_only_what_is_called() {
    let raw = build("tests/wasi/raw.moss");
    let std = build(&whole("assume Std {\n  fn main() { putchar(char::a); }\n}\n"));
    assert!(contains(&raw, b"proc_exit"));
    assert!(!contains(&std, b"proc_exit"));
    for module in [&raw, &std] {
        assert!(contains(module, b"fd_write"));
        assert!(!contains(module, b"args_sizes_get"));
        assert!(!contains(module, b"args_get"));
    }
}

/// D59: a value is a sequence of Wasm scalars, and not every scalar is an
/// i32. `1 << 32` has an empty low half, which is only true if the shift
/// really happened in 64 bits — and the intermediate lives in an i64
/// local.
#[test]
fn i64_is_a_scalar_type() {
    assert_eq!(run("tests/wasi/int64.moss", &[]), "yy\n");
}

/// A bound fn is a direct call to a specialisation of the callee, not an
/// indirect one (D2). Two providers of the same key produce two
/// specialisations of everything below them, and a provider's captured
/// vals are threaded down to wherever the call actually happens — here
/// two frames below the bind.
#[test]
fn fn_binds_compile_to_specialisations() {
    let main = std_main(
        r#"import "./runner.moss" as runner;
assume Std {
  val c: Char;
  assume c { fn emit() { putchar(c); } }
  fn shout() { putchar(char::exclam); }
  fn main() {
    bind c=char::k;
    bind runner::task=emit;
    runner::go();
    bind runner::task=shout;
    runner::go();
    putchar(char::newline);
  }
}
"#,
    );
    let dir = sources(&[
        (
            "runner.moss",
            "fn task();\nassume task {\n  fn go() { twice(); }\n  fn twice() { task(); task(); }\n}\n",
        ),
        ("main.moss", &main),
    ]);
    let module = build(&format!("{dir}/main.moss"));
    assert_eq!(run_module(&module, &[]), "kk!!\n");
    assert!(!wasm_text(&module).contains("call_indirect"), "a fn bind compiled to an indirect call");
}

// `test_build.TestWasmBackend.test_fn_bind_capture_survives_rebinds` is
// `language::context::fn_bind_capture_survives_rebinds`.

/// `bind N.plus = Box.plus` is a fn bind too, and the call through it
/// resolves to `Box.plus` directly.
#[test]
fn method_provision_compiles() {
    let entry = whole(
        r"type N;
fn .plus(rhs: This): This;
val n0: N;
context NOps = N, n0, N.plus;
assume Std {
  tag Box(Char);
  fn Box.plus(rhs: Box): Box {
    match this { Box(a) => match rhs { Box(b) => Box(b) } }
  }
  fn putbox(b: Box) { match b { Box(c) => putchar(c) } }
  assume NOps { fn use_it(): N { n0.plus(n0) } }
  fn main() {
    bind N = Box;
    bind n0 = Box(char::k);
    bind N.plus = Box.plus;
    putbox(use_it());
    putchar(char::newline);
  }
}
",
    );
    assert_eq!(run(&entry, &[]), "k\n");
}

/// The shape D52 is aiming at, in miniature: user code written against a
/// signature alone, a module implementing that signature over the
/// primitive context, and `main` applying it with binds.
#[test]
fn bridge_from_wasi_to_a_signature() {
    assert_eq!(run("tests/wasi/bridged.moss", &[]), "!!");
}

/// Chars are i32 codepoints here, so `.code()` is an identity — but it
/// must still agree with the oracle.
#[test]
fn char_code_in_wasm() {
    let entry = whole(
        r"assume Std {
  fn main() {
    var i = zero;
    while i.lt(char::c.code().sub(char::a.code())) {
      putchar(char::dot);
      i = i.add(one);
    }
    if char::A.code().lt(char::a.code()) { putchar(char::u) }
    putchar(char::newline);
  }
}
",
    );
    agree(&entry, &[], "..u\n");
}

#[test]
fn intlist_in_wasm() {
    let entry = whole(
        r"assume Std {
  fn main() {
    let xs = int_list();
    var i = zero;
    let n = one.shl(one.add(one).add(one).add(one));
    while i.lt(n) { xs.push(i); i = i.add(one); }
    if xs.length().eq(n) { putchar(char::n) }
    if xs.get(zero).eq(zero) { putchar(char::z) }
    xs.set(zero, one);
    if xs.get(zero).eq(one) { putchar(char::s) }
    if xs.get(n.sub(one)).eq(n.sub(one)) { putchar(char::e) }
    putchar(char::newline);
  }
}
",
    );
    // 16 elements, past the initial capacity of 8.
    assert_eq!(run(&entry, &[]), "nzse\n");
}

#[test]
fn string_slice_in_wasm() {
    let entry = whole(
        r"assume Std {
  fn main() {
    let s = first_arg();
    let two = one.add(one);
    print(s.slice(one, two));
    print(s.slice(zero, s.length()));
    let t = s.slice(two, two);
    if t.length().eq(two) { putchar(char::y) } else { putchar(char::n) }
    putchar(t.get(zero));
    putchar(char::newline);
  }
}
",
    );
    assert_eq!(run(&entry, &["abcdef"]), "bcabcdefyc\n");
}

/// `concat` and `StrList`, which the self-hosted module loader needs to
/// hold and build file paths.
#[test]
fn strings_and_strlist_in_wasm() {
    let entry = whole(
        r"assume Std {
  fn main() {
    let s = first_arg();
    let xs = str_list();
    xs.push(s.concat(s));
    xs.push(s.slice(zero, one).concat(s.slice(one, one)));
    var i = zero;
    while i.lt(xs.length()) {
      print(xs.get(i));
      putchar(char::comma);
      i = i.add(one);
    }
    putchar(char::newline);
  }
}
",
    );
    assert_eq!(run(&entry, &["abc"]), "abcabc,ab,\n");
}

// `test_build.TestWasmBackend.test_interner_arena_in_wasm` is
// `language::interner::names_outlive_their_source`.

/// The module starts at two pages, so a program that allocates past 128K
/// must grow it rather than write off the end.
#[test]
fn allocation_grows_memory() {
    let entry = whole(
        r"assume Std {
  fn main() {
    let xs = int_list();
    var i = zero;
    let n = one.shl(one.shl(one.add(one).add(one).add(one)));
    while i.lt(n) { xs.push(i); i = i.add(one); }
    if xs.length().eq(n) { putchar(char::y) } else { putchar(char::n) }
    if xs.get(n.sub(one)).eq(n.sub(one)) { putchar(char::z) }
    putchar(char::newline);
  }
}
",
    );
    assert_eq!(run(&entry, &[]), "yz\n");
}

/// The retirement path, first bricks: `print` and `first_arg` are Moss
/// functions over Wasi, with a bump allocator also written in Moss,
/// installed by a functor. The code that calls them assumes `Console` and
/// knows nothing of either.
#[test]
fn std_implemented_in_moss_over_wasi() {
    assert_eq!(run("tests/wasi/console.moss", &["hi"]), "hihi");
}

// `test_build.TestWasmBackend.test_representation_follows_the_static_type`
// is `language::representation::injection_is_where_the_tag_appears`.

/// D59: the compiler has no heap. Records are one scalar per field and an
/// injected union is a discriminant beside its payload, so a loop
/// building both leaves the heap pointer where it was.
#[test]
fn records_and_unions_do_not_allocate() {
    assert_eq!(run("tests/wasi/noheap.moss", &[]), "bnbnbny\n");
}

/// D59 in full: fields wider than one scalar, record payloads in unions of
/// different widths, and a contextual val that is a record.
#[test]
fn records_of_any_width() {
    assert_eq!(run("tests/wasi/records.moss", &[]), RECORDS_OUT);
}

#[test]
fn temporaries_nested_anywhere() {
    assert_eq!(run("tests/wasi/nested.moss", &[]), NESTED_OUT);
}

/// D62: the hole that once compiled to an invalid module.
#[test]
fn a_provision_serves_only_its_instantiation() {
    rejects("tests/wasi/mismatch.moss");
    let theirs = oracle_rejects("tests/wasi/mismatch.moss");
    assert!(theirs.contains("is provided at `Box[Elem=Char]`"), "{theirs}");
}

/// The Python back end lowered each function once, so it refused one
/// instantiated at a wider type than it was lowered for. The self-hosted
/// back end specializes per instantiation and compiles these, and they
/// run as the oracle runs them.
#[test]
fn a_wider_instantiation_is_refused() {
    for entry in ["tests/wasi/provisions.moss", "tests/wasi/widened.moss"] {
        let expected = oracle_run(entry, &[]);
        assert!(!expected.is_empty(), "{entry} prints nothing");
        assert_eq!(run(entry, &[]), expected, "{entry}");
    }
}

#[test]
fn a_type_that_contains_itself_is_rejected() {
    rejects("tests/wasi/cyclic.moss");
    let theirs = oracle_rejects("tests/wasi/cyclic.moss");
    assert!(theirs.contains("`Cell` contains itself"), "{theirs}");
}

/// Each program exits with `code`, compiled and under the oracle.
fn both_exit(entry: &str, code: i32) {
    assert_eq!(run_module_code(&build(entry), &[], code), "", "the self-hosted compiler on {entry}");
    let theirs = oracle(entry, &[]);
    assert_eq!(theirs.code, code, "the oracle on {entry}: {}", theirs.stderr);
}

/// Binding a struct tag's payload gives a bare record type, which a
/// parameter can name and another tag of the same payload can take
/// (Audit.v note 35). A union may not have one as a member
/// (tests/errors/union_record_member.moss).
#[test]
fn a_bound_record_payload_is_a_bare_record() {
    both_exit("tests/wasi/record_payload_param.moss", 3);
    both_exit("tests/wasi/record_payload_shared.moss", 3);
}

#[test]
fn anonymous_records_and_blocks_share_braces() {
    both_exit("tests/wasi/anonymous_record.moss", 7);
    for program in [
        "fn main() { {} }",
        "fn main() { let r: {} = (); }",
        "fn main() { let r: { a: () } = { b = () }; }",
        "fn main() { let r = { a = (), a = () }; }",
    ] {
        let entry = source(program);
        rejects(&entry);
        oracle_rejects(&entry);
    }
}

/// Audit.v note 30: acyclic contexts nest to any depth. Cycles are
/// rejected at declaration, including when used only by a tag.
#[test]
fn contexts_nest_to_any_depth() {
    both_exit("tests/wasi/context_deep.moss", 2);
    both_exit("tests/wasi/context_shared.moss", 0);
}

#[test]
fn imported_main_gets_the_root_context() {
    both_exit("tests/wasi/imported_main.moss", 0);
}

/// The same, watched from underneath: constructing a wrapper leaves the
/// heap pointer where it was.
#[test]
fn nominal_wrapper_does_not_allocate() {
    assert_eq!(run("tests/wasi/noalloc.moss", &[]), "ky\n");
}

// `test_build.TestWasmBackend.test_the_whole_std_over_wasi` is
// `language::std_over_wasi::the_interpreter_runs_the_moss_std`.

/// `CellInt` and `IntList` in Moss, over the library's allocator. The list
/// is pushed past its initial capacity, so the grow-by-copy runs.
#[test]
fn containers_over_wasi() {
    assert_eq!(run("tests/wasi/containers.moss", &[]), "yyyy\n");
}

/// `put_bytes` in Moss: one `fd_write` for a whole buffer, where `putchar`
/// is a syscall per byte. Compiled and interpreted, which is the pair that
/// has to agree for the compiler to write the same module either way.
#[test]
fn a_bulk_write_over_wasi() {
    agree("tests/wasi/bulk.moss", &[], "hi!\n");
    assert_eq!(oracle("tests/wasi/bulk.moss", &[]).stdout, b"hi!\n");
}

/// Every element of a `put_bytes` buffer contributes its low byte, so the
/// third one — wider than a byte — holds `i32.store8` to it in both
/// runners. (Once the Python interpreter's native `Std` against the Python
/// back end's native shim; both are gone, and this is `WasiStd`'s.)
#[test]
fn put_bytes_agrees_with_the_native_shims() {
    let entry = whole(
        r"assume Std {
  fn eight(): Int {
    one.add(one).add(one).add(one).add(one).add(one).add(one).add(one)
  }
  fn main() {
    let xs = int_list();
    xs.push(char::h.code());
    xs.push(char::i.code());
    xs.push(one.shl(eight()).add(char::exclam.code()));
    xs.push(char::newline.code());
    put_bytes(xs);
    put_bytes(int_list());
  }
}
",
    );
    agree(&entry, &[], "hi!\n");
}

/// D51's generic container, compiled: one interface whose accessors are
/// keyed on an *abstract* receiver, instantiated at two element types in
/// one scope. The keys are (IntArr, get) and (CharArr, get), so D44 never
/// sees one name standing for two symbols — and the two `.push` calls in
/// `exercise` reach different providers, decided by the receiver's type
/// alone.
#[test]
fn a_generic_container_over_wasi() {
    assert_eq!(run("tests/wasi/generic.moss", &[]), "hiDCC\n");
}

/// D57 cleared by D59: `Path.read` is Moss — path_open with genuine i64
/// rights masks, then fd_read onto the top of the heap the library itself
/// manages.
#[test]
fn path_over_wasi() {
    assert_eq!(run("tests/wasi/files.moss", &["lib/bool.moss"]), read("lib/bool.moss"));
}

/// `String`'s methods and the char constants, in Moss over Wasi. The
/// wrapper that carries the methods costs nothing (D58).
#[test]
fn strings_and_chars_over_wasi() {
    assert_eq!(run("tests/wasi/strings.moss", &["ab"]), "ababby\n");
}

/// `Int`, `zero`, `one` and the arithmetic methods provided over Wasi
/// rather than natively, driving a loop written in ordinary `Std` terms.
#[test]
fn numerics_over_wasi() {
    assert_eq!(run("tests/wasi/numbers.moss", &[]), "AAA=");
}

/// `Path`: `pwd` is the empty path, `join` concatenates, and `read` is
/// path_open plus fd_read against the preopen.
#[test]
fn path_read_in_wasm() {
    let entry = whole(
        r"assume Std {
  fn main() {
    let text = pwd.join(first_arg()).read();
    print(text);
  }
}
",
    );
    assert_eq!(run(&entry, &["lib/bool.moss"]), read("lib/bool.moss"));
}

/// The self-hosted compiler, run on a program of its own with no prelude
/// (argument zero empty): the module it writes runs, and prints what the
/// oracle prints for the same program — two front ends, one program.
#[test]
fn self_hosted_compiler_compiles_to_wasm() {
    let module = build_with("tests/wasi/prim.moss", "");
    let mine = run_module(&module, &[]);
    assert_eq!(mine, "ABKDKJGG\n");
    assert_eq!(oracle_run("tests/wasi/prim.moss", &[]), mine);
}

/// Cached lookup must preserve shadowing, old heads, misses and hash collisions.
#[test]
fn compiler_environment_lookup_preserves_persistent_chains() {
    agree("tests/compiler/env_lookup.moss", &[], "ok\n");
}

// `test_build.TestWasmBackend.test_self_hosted_lexer_compiles_to_wasm` is
// `language::self_hosted_lexer::agrees_with_bootstrap_on_real_files` (which
// includes src/lex.moss).

/// The self-hosted encoder: tests/wasi/emit.moss writes a WASI module to
/// standard output, compiled it writes byte for byte the module the
/// oracle's run of it writes, and that module says hello.
#[test]
fn emitted_module_matches_the_interpreters_and_runs() {
    let compiled = build("tests/wasi/emit.moss");
    let out = wasmtime(&wasm_file(&compiled), None, &[]);
    assert_eq!(out.code, 0, "{}", out.stderr);
    let emitted = out.stdout;
    assert!(emitted.starts_with(b"\0asm"), "not a module");
    let theirs = oracle("tests/wasi/emit.moss", &[]);
    assert_eq!(theirs.code, 0, "{}", theirs.stderr);
    assert!(theirs.stdout == emitted, "the oracle's run of emit.moss writes another module");
    assert_eq!(run_module(&emitted, &[]), "Hello, world!\n");
}

#[test]
fn merged_branch_types_are_canonical() {
    agree("tests/wasi/merge_never.moss", &[], "ok\n");
    agree("tests/wasi/merge_join.moss", &[], "bcbc\n");
}

/// A user file named wasm.moss cannot shadow the selected library, even
/// when automatic prelude imports are disabled.
#[test]
fn library_identity_comes_from_the_prelude_directory() {
    both_exit("tests/wasi/library_identity.moss", 0);
    let module = build_with("tests/wasi/library_identity.moss", "");
    assert_eq!(run_module(&module, &[]), "");
}

#[test]
fn literal_arithmetic_applies_a_family_on_demand() {
    agree("tests/wasi/literal_family.moss", &[], "y");
}

#[test]
fn family_demand_instantiates_its_method_prerequisites() {
    agree("tests/wasi/family_poly_need.moss", &[], "y");
    agree("tests/wasi/family_context_result.moss", &[], "y");
}

#[test]
fn merges_do_not_hide_a_second_family_producer() {
    for equation in ["A = B", "B = A"] {
        let declarations = format!(
            r"type A;
type B;
tag R;
assume A {{ fn .m(): A; }}
functor F[A]: -> A.m;
context Equated = A, B, {equation};
"
        );
        let demand = source(&format!(
            r"{declarations}
functor H[B]: -> B.m[A=B];
assume Equated, F, H {{
  fn use_it(x: R) {{ let ignored = x.m(); }}
}}
fn main() {{}}
"
        ));
        let error = oracle_rejects(&demand);
        assert!(
            error.contains("is produced by two families in force, `F` and `H` (D84 rule 7, D44)"),
            "{equation}: {error}"
        );

        let bind = source(&format!(
            r"{declarations}
assume Equated, F {{
  fn use_it() {{ bind R.m[A=R] = () => R; }}
}}
fn main() {{}}
"
        ));
        let error = oracle_rejects(&bind);
        assert!(error.contains("it cannot be bound directly (D84 rule 7)"), "{equation}: {error}");
    }
}

#[test]
fn family_demand_groups_inferred_substitutions() {
    agree("tests/wasi/family_same_full_theta.moss", &[], "");
}

#[test]
fn unused_functor_parameters_do_not_create_implicit_inputs() {
    agree("tests/wasi/functor_unused_poly.moss", &[], "y");
}

#[test]
fn literal_arithmetic_requires_its_own_type_signature() {
    let entry = "tests/fixtures/literal_signature/main.moss";
    let prelude = "tests/fixtures/literal_signature/prelude.moss";
    assert!(!compile(entry, prelude).is_module());
    let checked = oracle_command(&["--prelude", prelude, "--elab-only", entry]);
    assert_eq!(checked.code, ORACLE_REJECTED, "{}", checked.stderr);
    assert!(checked.stderr.contains("must take and return its own type (D68)"), "{}", checked.stderr);
}

#[test]
fn merged_constructor_arguments_are_canonical() {
    agree("tests/wasi/merge_tag.moss", &[], "b\n");
}

#[test]
fn context_method_receiver_follows_alias() {
    agree("tests/wasi/context_alias_receiver.moss", &[], "");
}

/// Attached providers use the receiver after context merging.
#[test]
fn attached_provider_receiver_uses_merged_type() {
    agree("tests/wasi/method_provider_merged_receiver.moss", &[], "");
}

#[test]
fn merged_capture_can_use_a_fixed_target_type() {
    agree("tests/wasi/functor_capture_merged_fixed.moss", &[], "");
}

#[test]
fn functor_application_checks_merged_input_types() {
    let entry = "tests/wasi/functor_merge_apply.moss";
    assert!(!compile(entry, "lib/prelude.moss").is_module());
    let checked = oracle_command(&["--prelude", "lib/prelude.moss", "--elab-only", entry]);
    assert_eq!(checked.code, ORACLE_REJECTED, "{}", checked.stderr);
    assert!(checked.stderr.contains("which its context merged (D43)"), "{}", checked.stderr);
}

#[test]
fn functor_application_checks_input_provision_instantiations() {
    let entry = "tests/wasi/functor_incoherent_apply.moss";
    assert!(!compile(entry, "lib/prelude.moss").is_module());
    let checked = oracle_command(&["--prelude", "lib/prelude.moss", "--elab-only", entry]);
    assert_eq!(checked.code, ORACLE_REJECTED, "{}", checked.stderr);
    assert!(checked.stderr.contains("at another instantiation"), "{}", checked.stderr);
}

#[test]
fn calls_check_fixed_functor_captures() {
    let entry = "tests/wasi/functor_fixed_call.moss";
    assert!(!compile(entry, "lib/prelude.moss").is_module());
    let checked = oracle_command(&["--prelude", "lib/prelude.moss", "--elab-only", entry]);
    assert_eq!(checked.code, ORACLE_REJECTED, "{}", checked.stderr);
    assert!(checked.stderr.contains("at another instantiation"), "{}", checked.stderr);
    agree("tests/wasi/functor_fixed_call_ok.moss", &[], "0");
}

#[test]
fn attached_defined_methods_demand_families() {
    agree("tests/wasi/family_attached_demand.moss", &[], "");
}

/// Defined functions in the selected platform module retain their source
/// bodies even when main's initial bindings include primitive dispatch rows.
#[test]
fn selected_platform_functions_may_have_bodies() {
    let entry = "tests/fixtures/platform_defined/main.moss";
    let prelude = "tests/fixtures/platform_defined/prelude.moss";
    let module = build_with(entry, prelude);
    assert_eq!(run_module(&module, &[]), "");
    let checked = oracle_command(&["--prelude", prelude, entry]);
    assert_eq!(checked.code, 0, "{}", checked.stderr);
    assert!(checked.stdout.is_empty(), "{:?}", checked.stdout);
}
