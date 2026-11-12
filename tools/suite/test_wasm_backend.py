from suite.support import (
    ORACLE_REJECTED,
    TRAP,
    build,
    build_with,
    compile_source,
    oracle,
    oracle_command,
    oracle_rejects,
    oracle_run,
    read,
    rejects,
    run,
    run_module,
    run_module_code,
    source,
    sources,
    std_main,
    wasm_file,
    wasm_text,
    wasmtime,
)

# The self-hosted back end, program by program: each program compiles to
# a module that runs under Wasmtime to the expected output; where the
# point is agreement, the oracle runs it to the same output.
#
# Ported from the Python bootstrap's `tests/test_build.py`, whose
# `TestWasmBackend` held the Python back end (gone) to the interpreter;
# the compiler under test is now the self-hosted one. A program whose
# `main` assumes `Std` is wrapped (`std_main`) so that the platform's
# `main` applies `WasiStd` (D52).
# A whole program whose `main` assumes `Std`, as one the platform runs.


def whole(text):
    return source(std_main(text))


# Both runners print `expected`.


def agree(entry, args, expected):
    assert run(entry, args) == expected, f"the self-hosted compiler on {entry}"
    assert oracle_run(entry, args) == expected, f"the oracle on {entry}"


# What tests/wasi/records.moss prints, one property per line.

RECORDS_OUT = "ab\nba\nbaq\ncdez\ncd\ngf\nho\n"

# What tests/wasi/nested.moss prints.

NESTED_OUT = "bca\nfda\nghi\n"

# What tests/wasi/float.moss prints, one property per line; the comments
# there say why each is what it is.

FLOAT_OUT = """1414213562
1414213504
1414213562
333333343
333333333
0100
0
2147483647
-2147483648
0
2147483647
-3
2
4
-3
-2
-2
-1
1
2
1065353216
1
1073741824
1568669697
-1
1
333333344
0
2
"""

# `test_build.TestWasmBackend.test_examples_match_goldens` is
# `examples::examples_match_their_goldens`.


def test_arithmetic_and_loops():
    entry = whole("""assume Std {
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
""")
    assert run(entry, []) == "wwwy\n"


def test_if_match_preserves_receiver_and_evaluates_condition_once():
    entry = whole("""tag True;
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
""")
    agree(entry, [], "ab\n")


def test_if_requires_bool_and_unit_for_an_omitted_else():
    for text in ["fn main() { if () {} }", "tag Empty(()); fn main() { if true { Empty(()) }; }"]:
        entry = source(text)
        rejects(entry)
        oracle_rejects(entry)


def test_record_destructuring_in_parameters_and_locals():
    entry = whole("""assume Std {
  fn choose({left = {name = who: Char}, right: Char}, use_left: Bool): Char {
    if use_left { who } else { right }
  }
  fn choose_outer({left = {name = who: Char}, right}: {left: {name: Char}, right: Char}): Char {
    if true { who } else { right }
  }
  fn read({value: Char}): Char;
  fn main() {
    let row = {left = {name = char::a}, right = char::b};
    putchar(choose(row, true));
    putchar(choose(row, false));
    putchar(choose_outer(row));
    var calls = zero;
    let {left = {name = saved}, right} = ({ calls = calls.add(one); row });
    if calls.eq(one) { putchar(saved); putchar(right); }
    var {value,} = {value = char::n};
    value = char::c;
    bind read = ({value,}) => value;
    putchar(read({value,}));
    putchar(char::newline);
  }
}
""")
    agree(entry, [], "abaabc\n")


def test_record_destructuring_rejects_conflicting_annotations_and_duplicates():
    for text in [
        "fn f({x: ()}: {x: {}}) {} fn main() {}",
        "fn f({x: ()}: {x: {}}); fn main() {}",
        "fn f({x, y = x}: {x: (), y: ()}) {} fn main() {}",
        "fn f({x: (), x: ()}) {} fn main() {}",
        "fn f({x,}) {} fn main() {}",
    ]:
        entry = source(text)
        rejects(entry)
        oracle_rejects(entry)


# D67: `Std` has a `Float`, an f64 under `WasiStd`, and the compiled
# program gets the same answers as the oracle. A function taking and
# returning a `Float` is compiled with f64 parameters and results.


def test_native_float():
    entry = whole("""assume Std {
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
""")
    # 97 / 2 is 48.5, a tie that goes to the even 48; 49.5 goes to 50.
    agree(entry, [], "A02ynb\n")


# `Std`'s `Int` is i32 under `WasiStd`, and the oracle computes exactly
# what its instructions do: arithmetic wraps, division and remainder
# truncate toward zero (`i32.div_s`, `i32.rem_s`), and a shift count is
# taken mod 32.


def test_native_int_is_i32():
    entry = whole("""assume Std {
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
""")
    agree(entry, [], "yyyyyyyy\n")


# `i32.div_s` traps on the one quotient i32 cannot hold, and so does the
# oracle.


def test_native_int_division_overflow_traps():
    entry = whole("assume Std {\n  fn main() { let x = (-2147483647 - 1) / -1; }\n}\n")
    assert run_module_code(build(entry), [], TRAP) == ""
    out = oracle(entry, [])
    assert out.code == TRAP, f"{out.stderr}"


# Units/unions with match, tags with payload binders, and records with
# field access and destructuring.


def test_boxed_values_and_match():
    cases = [
        (
            "colors",
            """assume Std {
  tag Red;
  tag Green;
  tag Blue;
  type Color = Red | Green | Blue;
  fn code(c: Color): Char {
    match c { Red => char::r, Green => char::g, Blue => char::b, }
  }
  fn main() { putchar(code(Green)); putchar(code(Red)); putchar(char::newline); }
}
""",
            "gr\n",
        ),
        (
            "wrap",
            """assume Std {
  tag Wrapped(Char);
  tag Missing;
  type Maybe = Missing | Wrapped;
  fn show(m: Maybe) {
    match m { Wrapped(c) => putchar(c), Missing => putchar(char::question), }
    putchar(char::newline);
  }
  fn main() { show(Wrapped(char::w)); show(Missing); }
}
""",
            "w\n?\n",
        ),
        (
            "point",
            """assume Std {
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
""",
            "(a,b)\n(b,a)\n",
        ),
    ]
    for name, text, expected in cases:
        assert run(whole(text), []) == expected, f"{name}"


# D67: the float half of `Wasm`, as parameters, results, locals, record
# fields and memory.


def test_f32_and_f64_are_scalar_types():
    assert run("tests/wasi/float.moss", []) == FLOAT_OUT


# D52: a program can assume the primitive context directly, with no
# `Std` and no prelude bridge — just Wasm instructions and WASI imports.
# Writes "A\n" through fd_write, then exits 3.


def test_raw_wasi_program():
    assert run_module_code(build("tests/wasi/raw.moss"), [], 3) == "A\n"


def contains(module, name):
    return name in module


# The import section is not a fixed set: `proc_exit` shows up only in the
# module that calls it, and every function index moves to accommodate
# it. (The Python back end's native shims always imported `fd_write`,
# `args_sizes_get` and `args_get`; here, neither program reads its
# arguments, so neither imports the calls that would.)


def test_wasi_imports_are_only_what_is_called():
    raw = build("tests/wasi/raw.moss")
    std = build(whole("assume Std {\n  fn main() { putchar(char::a); }\n}\n"))
    assert contains(raw, b"proc_exit")
    assert not (contains(std, b"proc_exit"))
    for module in [raw, std]:
        assert contains(module, b"fd_write")
        assert not (contains(module, b"args_sizes_get"))
        assert not (contains(module, b"args_get"))


# D59: a value is a sequence of Wasm scalars, and not every scalar is an
# i32. `1 << 32` has an empty low half, which is only true if the shift
# really happened in 64 bits — and the intermediate lives in an i64
# local.


def test_i64_is_a_scalar_type():
    assert run("tests/wasi/int64.moss", []) == "yy\n"


# A bound fn is a direct call to a specialisation of the callee, not an
# indirect one (D2). Two providers of the same key produce two
# specialisations of everything below them, and a provider's captured
# vals are threaded down to wherever the call actually happens — here
# two frames below the bind.


def test_fn_binds_compile_to_specialisations():
    main = std_main("""import "./runner.moss" as runner;
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
""")
    dir = sources(
        [
            (
                "runner.moss",
                """fn task();
assume task {
  fn go() { twice(); }
  fn twice() { task(); task(); }
}
""",
            ),
            ("main.moss", main),
        ]
    )
    module = build(f"{dir}/main.moss")
    assert run_module(module, []) == "kk!!\n"
    assert not ("call_indirect" in wasm_text(module)), "a fn bind compiled to an indirect call"


# `test_build.TestWasmBackend.test_fn_bind_capture_survives_rebinds` is
# `language::context::fn_bind_capture_survives_rebinds`.
# `bind N.plus = Box.plus` is a fn bind too, and the call through it
# resolves to `Box.plus` directly.


def test_method_provision_compiles():
    entry = whole("""type N;
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
""")
    assert run(entry, []) == "k\n"


# The shape D52 is aiming at, in miniature: user code written against a
# signature alone, a module implementing that signature over the
# primitive context, and `main` applying it with binds.


def test_bridge_from_wasi_to_a_signature():
    assert run("tests/wasi/bridged.moss", []) == "!!"


# Chars are i32 codepoints here, so `.code()` is an identity — but it
# must still agree with the oracle.


def test_char_code_in_wasm():
    entry = whole("""assume Std {
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
""")
    agree(entry, [], "..u\n")


def test_intlist_in_wasm():
    entry = whole("""assume Std {
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
""")
    # 16 elements, past the initial capacity of 8.
    assert run(entry, []) == "nzse\n"


def test_string_slice_in_wasm():
    entry = whole("""assume Std {
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
""")
    assert run(entry, ["abcdef"]) == "bcabcdefyc\n"


# `concat` and `StrList`, which the self-hosted module loader needs to
# hold and build file paths.


def test_strings_and_strlist_in_wasm():
    entry = whole("""assume Std {
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
""")
    assert run(entry, ["abc"]) == "abcabc,ab,\n"


# `test_build.TestWasmBackend.test_interner_arena_in_wasm` is
# `language::interner::names_outlive_their_source`.
# The module starts at two pages, so a program that allocates past 128K
# must grow it rather than write off the end.


def test_allocation_grows_memory():
    entry = whole("""assume Std {
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
""")
    assert run(entry, []) == "yz\n"


# The retirement path, first bricks: `print` and `first_arg` are Moss
# functions over Wasi, with a bump allocator also written in Moss,
# installed by a functor. The code that calls them assumes `Console` and
# knows nothing of either.


def test_std_implemented_in_moss_over_wasi():
    assert run("tests/wasi/console.moss", ["hi"]) == "hihi"


# `test_build.TestWasmBackend.test_representation_follows_the_static_type`
# is `language::representation::injection_is_where_the_tag_appears`.
# D59: the compiler has no heap. Records are one scalar per field and an
# injected union is a discriminant beside its payload, so a loop
# building both leaves the heap pointer where it was.


def test_records_and_unions_do_not_allocate():
    assert run("tests/wasi/noheap.moss", []) == "bnbnbny\n"


# D59 in full: fields wider than one scalar, record payloads in unions of
# different widths, and a contextual val that is a record.


def test_records_of_any_width():
    assert run("tests/wasi/records.moss", []) == RECORDS_OUT


def test_temporaries_nested_anywhere():
    assert run("tests/wasi/nested.moss", []) == NESTED_OUT


# D62: the hole that once compiled to an invalid module.


def test_a_provision_serves_only_its_instantiation():
    rejects("tests/wasi/mismatch.moss")
    theirs = oracle_rejects("tests/wasi/mismatch.moss")
    assert "is provided at `Box[Elem=Char]`" in theirs, f"{theirs}"


# The Python back end lowered each function once, so it refused one
# instantiated at a wider type than it was lowered for. The self-hosted
# back end specializes per instantiation and compiles these, and they
# run as the oracle runs them.


def test_a_wider_instantiation_is_refused():
    for entry in ["tests/wasi/provisions.moss", "tests/wasi/widened.moss"]:
        expected = oracle_run(entry, [])
        assert not (not expected), f"{entry} prints nothing"
        assert run(entry, []) == expected, f"{entry}"


def test_a_type_that_contains_itself_is_rejected():
    rejects("tests/wasi/cyclic.moss")
    theirs = oracle_rejects("tests/wasi/cyclic.moss")
    assert "`Cell` contains itself" in theirs, f"{theirs}"


# Each program exits with `code`, compiled and under the oracle.


def both_exit(entry, code):
    assert run_module_code(build(entry), [], code) == "", f"the self-hosted compiler on {entry}"
    theirs = oracle(entry, [])
    assert theirs.code == code, f"the oracle on {entry}: {theirs.stderr}"


# Binding a struct tag's payload gives a bare record type, which a
# parameter can name and another tag of the same payload can take
# (Audit.v note 35). A union may not have one as a member
# (tests/errors/union_record_member.moss).


def test_a_bound_record_payload_is_a_bare_record():
    both_exit("tests/wasi/record_payload_param.moss", 3)
    both_exit("tests/wasi/record_payload_shared.moss", 3)


def test_anonymous_records_and_blocks_share_braces():
    both_exit("tests/wasi/anonymous_record.moss", 7)
    for program in [
        "fn main() { {} }",
        "fn main() { let r: {} = (); }",
        "fn main() { let r: { a: () } = { b = () }; }",
        "fn main() { let r = { a = (), a = () }; }",
    ]:
        entry = source(program)
        rejects(entry)
        oracle_rejects(entry)


def test_tuples_and_positional_tags():
    both_exit("tests/wasi/tuples.moss", 2)
    for program in [
        "fn main() { let pair = ((), ()); pair.0; }",
        "fn main() { let (a, b) = ((), (), ()); }",
        "fn main() { let (a, b) = ((),); }",
        "fn main() { let (a: (), b: {}): ((), ()) = ((), ()); }",
        "fn f((a, b): ((), (), ())) {} fn main() {}",
        "tag T((), ()); fn main() { T(()); }",
        "tag T(()); fn main() { T(); }",
        "tag T(); fn main() { T(()); }",
        "tag Z(); tag Pair(Z, Z); fn main() { Pair((), ()); }",
        "tag Z(); fn main() { let pair: (Z, Z) = ((), ()); }",
        "tag T((), (), ()); fn main() { match T((), (), ()) { T(a, b) => a } }",
        "fn main() { let pair: ((),) = {}; }",
    ]:
        entry = source(program)
        rejects(entry)
        oracle_rejects(entry)


def test_tuple_destructuring_in_parameters_and_locals():
    entry = whole("""assume Std {
  fn choose((first: Char, second: Char), {flag: Bool}): Char {
    if flag { first } else { second }
  }
  fn read(pair: (Char, Char)): Char;
  fn main() {
    let (a): Char = char::a;
    let ((b,), {c}) = ((char::b,), {c = char::c});
    putchar(choose((a, b), {flag = true}));
    bind read = ((left, right)) => right;
    putchar(read((a, b)));
    var one, two: Char = (a, b);
    two = c;
    putchar(two);
    putchar(char::newline);
  }
}
""")
    agree(entry, [], "abc\n")


def test_tag_destructuring_in_parameters_and_locals():
    entry = whole("""assume Std {
  tag Point { x: Char, y: Char };
  tag Pair(Char, Char);
  tag Wrapped(Point);
  tag Empty();
  tag Marker;
  fn first(Point { x, y }): Char { x }
  fn second(Pair(a, b)): Char { b }
  fn read(pair: Pair): Char;
  fn main() {
    let p = Point { x = char::a, y = char::b };
    putchar(first(p));
    var Point { x: Char, y = changed } = p;
    changed = char::c;
    let Wrapped(Point { x = left, y = right }) = Wrapped(p);
    putchar(second(Pair(left, right)));
    let Empty() = Empty();
    let Marker() = Marker;
    bind read = (Pair(a, b)) => a;
    putchar(read(Pair(changed, x)));
    putchar(char::newline);
  }
}
""")
    agree(entry, [], "abc\n")


def test_tag_destructuring_requires_the_single_declared_head_and_shape():
    for text in [
        "tag T { x: () }; fn main() { let {x} = T {x = ()}; }",
        "tag T { x: () }; fn main() { let {}: T = T {x = ()}; }",
        "tag T(()); tag U; fn main() { let value: T | U = T(()); let T(x) = value; }",
        "tag T(()); tag U(()); fn main() { let T(x) = U(()); }",
        "tag T((), ()); fn main() { let T(x) = T((), ()); }",
        "tag T(()); fn main() { let T() = T(()); }",
        "tag T({x: ()}); fn main() { let T {x} = T({x = ()}); }",
    ]:
        entry = source(text)
        rejects(entry)
        oracle_rejects(entry)


# Audit.v note 30: acyclic contexts nest to any depth. Cycles are
# rejected at declaration, including when used only by a tag.


def test_contexts_nest_to_any_depth():
    both_exit("tests/wasi/context_deep.moss", 2)
    both_exit("tests/wasi/context_shared.moss", 0)


def test_imported_main_gets_the_root_context():
    both_exit("tests/wasi/imported_main.moss", 0)


# The same, watched from underneath: constructing a wrapper leaves the
# heap pointer where it was.


def test_nominal_wrapper_does_not_allocate():
    assert run("tests/wasi/noalloc.moss", []) == "ky\n"


# `test_build.TestWasmBackend.test_the_whole_std_over_wasi` is
# `language::std_over_wasi::the_interpreter_runs_the_moss_std`.
# `CellInt` and `IntList` in Moss, over the library's allocator. The list
# is pushed past its initial capacity, so the grow-by-copy runs.


def test_containers_over_wasi():
    assert run("tests/wasi/containers.moss", []) == "yyyy\n"


# `put_bytes` in Moss: one `fd_write` for a whole buffer, where `putchar`
# is a syscall per byte. Compiled and interpreted, which is the pair that
# has to agree for the compiler to write the same module either way.


def test_a_bulk_write_over_wasi():
    agree("tests/wasi/bulk.moss", [], "hi!\n")
    assert oracle("tests/wasi/bulk.moss", []).stdout == b"hi!\n"


# Every element of a `put_bytes` buffer contributes its low byte, so the
# third one — wider than a byte — holds `i32.store8` to it in both
# runners. (Once the Python interpreter's native `Std` against the Python
# back end's native shim; both are gone, and this is `WasiStd`'s.)


def test_put_bytes_agrees_with_the_native_shims():
    entry = whole("""assume Std {
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
""")
    agree(entry, [], "hi!\n")


# D51's generic container, compiled: one interface whose accessors are
# keyed on an *abstract* receiver, instantiated at two element types in
# one scope. The keys are (IntArr, get) and (CharArr, get), so D44 never
# sees one name standing for two symbols — and the two `.push` calls in
# `exercise` reach different providers, decided by the receiver's type
# alone.


def test_a_generic_container_over_wasi():
    assert run("tests/wasi/generic.moss", []) == "hiDCC\n"


# D57 cleared by D59: `Path.read` is Moss — path_open with genuine i64
# rights masks, then fd_read onto the top of the heap the library itself
# manages.


def test_path_over_wasi():
    assert run("tests/wasi/files.moss", ["lib/bool.moss"]) == read("lib/bool.moss")


# `String`'s methods and the char constants, in Moss over Wasi. The
# wrapper that carries the methods costs nothing (D58).


def test_strings_and_chars_over_wasi():
    assert run("tests/wasi/strings.moss", ["ab"]) == "ababby\n"


# `Int`, `zero`, `one` and the arithmetic methods provided over Wasi
# rather than natively, driving a loop written in ordinary `Std` terms.


def test_numerics_over_wasi():
    assert run("tests/wasi/numbers.moss", []) == "AAA="


# `Path`: `pwd` is the empty path, `join` concatenates, and `read` is
# path_open plus fd_read against the preopen.


def test_path_read_in_wasm():
    entry = whole("""assume Std {
  fn main() {
    let text = pwd.join(first_arg()).read();
    print(text);
  }
}
""")
    assert run(entry, ["lib/bool.moss"]) == read("lib/bool.moss")


# The self-hosted compiler, run on a program of its own with no prelude
# (argument zero empty): the module it writes runs, and prints what the
# oracle prints for the same program — two front ends, one program.


def test_self_hosted_compiler_compiles_to_wasm():
    module = build_with("tests/wasi/prim.moss", "")
    mine = run_module(module, [])
    assert mine == "ABKDKJGG\n"
    assert oracle_run("tests/wasi/prim.moss", []) == mine


# Cached lookup must preserve shadowing, old heads, misses and hash collisions.


def test_compiler_environment_lookup_preserves_persistent_chains():
    agree("tests/compiler/env_lookup.moss", [], "ok\n")


# `test_build.TestWasmBackend.test_self_hosted_lexer_compiles_to_wasm` is
# `language::self_hosted_lexer::agrees_with_bootstrap_on_real_files` (which
# includes src/lex.moss).
# The self-hosted encoder: tests/wasi/emit.moss writes a WASI module to
# standard output, compiled it writes byte for byte the module the
# oracle's run of it writes, and that module says hello.


def test_emitted_module_matches_the_interpreters_and_runs():
    compiled = build("tests/wasi/emit.moss")
    out = wasmtime(wasm_file(compiled), None, [])
    assert out.code == 0, f"{out.stderr}"
    emitted = out.stdout
    assert emitted.startswith(b"\x00asm"), "not a module"
    theirs = oracle("tests/wasi/emit.moss", [])
    assert theirs.code == 0, f"{theirs.stderr}"
    assert theirs.stdout == emitted, "the oracle's run of emit.moss writes another module"
    assert run_module(emitted, []) == "Hello, world!\n"


def test_merged_branch_types_are_canonical():
    agree("tests/wasi/merge_never.moss", [], "ok\n")
    agree("tests/wasi/merge_join.moss", [], "bcbc\n")


# A user file named wasm.moss cannot shadow the selected library, even
# when automatic prelude imports are disabled.


def test_library_identity_comes_from_the_prelude_directory():
    both_exit("tests/wasi/library_identity.moss", 0)
    module = build_with("tests/wasi/library_identity.moss", "")
    assert run_module(module, []) == ""


def test_literal_arithmetic_applies_a_family_on_demand():
    agree("tests/wasi/literal_family.moss", [], "y")


def test_family_demand_instantiates_its_method_prerequisites():
    agree("tests/wasi/family_poly_need.moss", [], "y")
    agree("tests/wasi/family_context_result.moss", [], "y")


def test_merges_do_not_hide_a_second_family_producer():
    for equation in ["A = B", "B = A"]:
        declarations = f"""type A;
type B;
tag R;
assume A {{ fn .m(): A; }}
functor F[A]: -> A.m;
context Equated = A, B, {equation};
"""
        demand = source(f"""{declarations}
functor H[B]: -> B.m[A=B];
assume Equated, F, H {{
  fn use_it(x: R) {{ let ignored = x.m(); }}
}}
fn main() {{}}
""")
        error = oracle_rejects(demand)
        assert "is produced by two families in force, `F` and `H` (D84 rule 7, D44)" in error, (
            f"{equation}: {error}"
        )
        bind = source(f"""{declarations}
assume Equated, F {{
  fn use_it() {{ bind R.m[A=R] = () => R; }}
}}
fn main() {{}}
""")
        error = oracle_rejects(bind)
        assert "it cannot be bound directly (D84 rule 7)" in error, f"{equation}: {error}"


def test_family_demand_groups_inferred_substitutions():
    agree("tests/wasi/family_same_full_theta.moss", [], "")


def test_unused_functor_parameters_do_not_create_implicit_inputs():
    agree("tests/wasi/functor_unused_poly.moss", [], "y")


def test_literal_arithmetic_requires_its_own_type_signature():
    entry = "tests/fixtures/literal_signature/main.moss"
    prelude = "tests/fixtures/literal_signature/prelude.moss"
    assert not (compile_source(entry, prelude).is_module())
    checked = oracle_command(["--prelude", prelude, "--elab-only", entry])
    assert checked.code == ORACLE_REJECTED, f"{checked.stderr}"
    assert "must take and return its own type (D68)" in checked.stderr, f"{checked.stderr}"


def test_merged_constructor_arguments_are_canonical():
    agree("tests/wasi/merge_tag.moss", [], "b\n")


def test_context_method_receiver_follows_alias():
    agree("tests/wasi/context_alias_receiver.moss", [], "")


# Attached providers use the receiver after context merging.


def test_attached_provider_receiver_uses_merged_type():
    agree("tests/wasi/method_provider_merged_receiver.moss", [], "")


def test_merged_capture_can_use_a_fixed_target_type():
    agree("tests/wasi/functor_capture_merged_fixed.moss", [], "")


def test_functor_application_checks_merged_input_types():
    entry = "tests/wasi/functor_merge_apply.moss"
    assert not (compile_source(entry, "lib/prelude.moss").is_module())
    checked = oracle_command(["--prelude", "lib/prelude.moss", "--elab-only", entry])
    assert checked.code == ORACLE_REJECTED, f"{checked.stderr}"
    assert "which its context merged (D43)" in checked.stderr, f"{checked.stderr}"


def test_functor_application_checks_input_provision_instantiations():
    entry = "tests/wasi/functor_incoherent_apply.moss"
    assert not (compile_source(entry, "lib/prelude.moss").is_module())
    checked = oracle_command(["--prelude", "lib/prelude.moss", "--elab-only", entry])
    assert checked.code == ORACLE_REJECTED, f"{checked.stderr}"
    assert "at another instantiation" in checked.stderr, f"{checked.stderr}"


def test_calls_check_fixed_functor_captures():
    entry = "tests/wasi/functor_fixed_call.moss"
    assert not (compile_source(entry, "lib/prelude.moss").is_module())
    checked = oracle_command(["--prelude", "lib/prelude.moss", "--elab-only", entry])
    assert checked.code == ORACLE_REJECTED, f"{checked.stderr}"
    assert "at another instantiation" in checked.stderr, f"{checked.stderr}"
    agree("tests/wasi/functor_fixed_call_ok.moss", [], "0")


def test_attached_defined_methods_demand_families():
    agree("tests/wasi/family_attached_demand.moss", [], "")


# Defined functions in the selected platform module retain their source
# bodies even when main's initial bindings include primitive dispatch rows.


def test_selected_platform_functions_may_have_bodies():
    entry = "tests/fixtures/platform_defined/main.moss"
    prelude = "tests/fixtures/platform_defined/prelude.moss"
    module = build_with(entry, prelude)
    assert run_module(module, []) == ""
    checked = oracle_command(["--prelude", prelude, entry])
    assert checked.code == 0, f"{checked.stderr}"
    assert not checked.stdout, f"{checked.stdout!r}"
