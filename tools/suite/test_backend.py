import sys

from suite.support import (
    ORACLE_REJECTED,
    TRAP,
    build_with,
    compile_source,
    oracle,
    oracle_command,
    oracle_rejects,
    oracle_run,
    parallel_map,
    rejects_with,
    run_module,
    run_module_code,
    source,
    starts_with_rec_group,
    wasm_file,
    wasm_text,
    wasmtime,
)

# The self-hosted compiler, end to end: src/ reads a Moss program off
# disk, resolves it, and writes a WASI module that runs.
#
# The language it covers started as the primitive context of D52 —
# `Wasm` instructions, `Wasi` imports, `Bool` for `if` to eliminate,
# plain functions, `let`/`var`, `if`/`else`, `while`, `loop`/`break` and
# `return` — which is what tests/wasi/raw.moss and tests/wasi/prim.moss
# are written in, and is now the whole language over `Std`. Programs are
# compiled with the prelude or without it (`""`), as each test says, and
# where a program is also run or refused by the oracle, the two are held
# to the same behaviour and, for a refusal, the oracle to the Python
# bootstrap's message (which it reproduces).
#
# Ported from `TestSelfHostedBackEnd` in the Python bootstrap's
# `tests/test_run.py`; the Python interpreter and the Python Wasm back
# end, which were cross-checks there, are both the oracle here.

PRELUDE = "lib/prelude.moss"

# The oracle's message for a functor bound to an unbracketed abstract one
# that binds a type in its result (`WasiFloats`' `bind Int`, under `Fast`).

BIND_INT_CHANGES_RESULT = "`bind Int` in a functor bound to an abstract one changes a type its result mentions; the fixed set is not widened per binding (D84 3d)"

# The oracle's message for a `Float` that `bind Fast;` made leaving its
# block; the Python bootstrap said "this block yields `Float@Fast`, the
# type `bind Fast;` made in it: a value of that type cannot leave the
# block (D78)".

ESCAPES_FAST = "this block yields `Float@Fast`, which mentions `Float@Fast`, a type `bind F;` made in it: a value of that type cannot leave the block (D78)"

# The tail of the oracle's message for an incoherent provision.

INCOHERENT = "at another instantiation than the one in the context here was provided at (a provision serves only the instantiation it was made at; D62)"


def wasi(name):
    return f"tests/wasi/{name}.moss"


def errors(name):
    return f"tests/errors/{name}.moss"


# What the self-hosted compiler writes for `entry`: a module or a report.


def compile_with_moss(entry, prelude):
    return compile_source(entry, prelude).stdout


# The self-hosted compiler's module for `entry`, run, exiting with 0.


def run_moss(entry, prelude):
    return run_module(build_with(entry, prelude), [])


# A module run to its end, whatever its exit status.


def wasmtime_out(module):
    return wasmtime(wasm_file(module), None, [])


# The self-hosted compiler's report for `entry`, which it must refuse.


def rejection(entry, prelude):
    return rejects_with(entry, prelude)


# The oracle refuses `entry` with `message` among what it says.


def oracle_says(entry, message):
    said = oracle_rejects(entry)
    assert message in said, f"{entry}: the oracle said {said!r}, not {message!r}"


# The oracle traps on `entry`, after printing `stdout`, naming the trap.


def oracle_traps(entry, stdout, trap):
    out = oracle(entry, [])
    assert (out.code, out.text()) == (TRAP, stdout), f"{entry}: {out.stderr}"
    assert trap in out.stderr, f"{entry}: {out.stderr}"


# A report template's `{}` (or `{0}`) is the entry.


def fill(template, entry):
    return template.replace("{0}", entry).replace("{}", entry)


# Each `(name, message, report)`: the oracle refuses `tests/wasi/NAME`
# saying `message`, and the self-hosted compiler (with the prelude)
# writes a report that is `report` (`exact`) or starts with it.


def refused(cases, exact):
    for name, message, template in cases:
        entry = wasi(name)
        oracle_says(entry, message)
        out = rejection(entry, PRELUDE)
        expected = fill(template, entry)
        if exact:
            assert out == expected, f"{entry}"
        else:
            assert out.startswith(expected), f"{entry}: {out!r}"


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

# What tests/wasi/exponents.moss prints: the same report at f64, then f32.
# `1e39` is inf at f32 and `1e-46` zero.

EXPONENTS_OUT = """yyyyyyyyy
1500 125 1 250 -42 100000 602\x20
n nn yy
yyyyyyyyy
1500 125 1 250 -42 100000 602\x20
n yy yy
"""

# What tests/wasi/shadow.moss prints: f64, f32, f64, f32, f64.

SHADOW_OUT = "333333333\n333333344\n333333333\n333333344\n333333333\n"

# What tests/wasi/literals.moss prints: the same report at f64, then f32.
# A third to twenty-one places is not f32's nearest third: its digits were
# summed in f32 and rounded on the way (D68).

LITERALS_OUT = """0 7 10 100 1000 1234567 2147483647 -42 3 0\x20
3 -2 1 0 250 1 16777215\x20
3141593 333333333 ny
314159265 333333333\x20
0 7 10 100 1000 1234567 2147483647 -42 3 0\x20
3 -2 1 0 250 1 16777215\x20
3141593 333333344 yn
314159264 333333248\x20
"""

# What tests/wasi/lists.moss prints: an IntList beside a FloatList at f64,
# then the same at f32. A thousand 0.1s sum to 100 at f64 and to
# 99.999048 at f32; a third differs in the eighth digit.

LISTS_OUT = """1000 1000 100000000 332833500\x20
333333333 1 1 -7 251001\x20
1000 1000 99999048 332833500\x20
333333344 1 1 -7 251001\x20
"""

# What tests/wasi/overflow.moss prints before it traps: an oversized
# literal under wrapping i32 arithmetic, then one that fits, under it and
# under arithmetic that traps on overflow.

OVERFLOW_OUT = "1\n2147483647\n2147483647\n"

# D75: what tests/wasi/gc_tree.moss prints — the thesis's expression, a
# full tree of 2^10 ones and its depth, and the expression read through a
# contextual val, doubled — and what tests/wasi/gc_ast.moss prints, once
# over a pool of indices and once over the GC heap.

GC_TREE_OUT = "-10\n1024\n11\n-20\n"

GC_AST_OUT = "-10\n0\n4096\n-10\n0\n4096\n"

# ...and what tests/wasi/gc_two.moss prints, with `Heap` applied at two
# types in one scope.

GC_TWO_OUT = "3\n12\n7\n1\n"

# D81: what tests/wasi/gc_array.moss prints — an array of `Int`s written,
# filled and copied, read as digits, and its last element; `Point`s over
# two lanes; `Tagged`s over three, one of references; and an array of
# references, summed, and its last — and what tests/wasi/gc_list.moss
# prints, the same three lines from a packed list and from a list built
# on GC arrays, then a list of references.

GC_ARRAY_OUT = "8\n15071117\n7\n512\n4\n0\n-1\n47\n3\n46\n1\n"

GC_LIST_OUT = "21\n3475\n5\n21\n3475\n5\n15\n3\n"

# D78: what tests/wasi/functor_fast.moss prints — √2 and √3 scaled by a
# billion, at f64 and then at f32.

FUNCTOR_FAST_OUT = "1414213562\n1732050808\n1414213504\n1732050944\n"

# D78: what tests/wasi/functor_trace.moss prints — the notes of a traced
# subtree and the whole value, under each of two tracers.

FUNCTOR_TRACE_OUT = "2,7,4,3,3,6,\n7\n[2][7][4][3][3][6]\n7\n"

# What tests/wasi/ops.moss prints: D68's operators on `Int` and `Bool`.

OPS_OUT = """19 -7 3 1 -7 2 23 35 21 5 1 -1\x20
ynynnyny y
2 15 13 1024 128 -5\x20
nnynyyyyn
"""

# What tests/wasi/dual.moss prints, times a million: f(2), f'(2) and
# f''(2) for f(x) = x^3 - 2x - 5; Newton's iterates from 2 toward its
# root, 2.0945514815...; and d/dx (x * d/dy (x + y)) at 1.

DUAL_OUT = """-1000000
10000000
12000000
2100000
2094568
2094551
2094551
1000000
"""

# What tests/wasi/dual_std.moss prints, times a million: f(2), f'(2) and
# f''(2) for f(x) = sqrt(x + x^2 + x^3) — sqrt 14 = 3.7416573868,
# 17 / (2 sqrt 14) = 2.2717205563 and 103 / (56 sqrt 14) = 0.4915697842.

DUAL_STD_OUT = "3741657\n2271721\n491570\n"

# What tests/wasi/newton.moss prints: the same report at f64, then at f32.

NEWTON_OUT = """1500000000
1416666667
1414213562
1414213562
0
333333333
-100000000
16777217
24-2-2
2147483647
nynnyy0
1500000000
1416666752
1414213504
1414213504
0
333333344
-100000000
16777216
24-2-2
2147483647
nynnyy0
"""

# What tests/wasi/lambda.moss prints: each letter is named beside the
# line that prints it.

LAMBDA_OUT = "ABxCypqmnBwgdD4\n"

# What tests/wasi/size_rebind.moss and tests/wasi/size_std.moss print.

SIZE_REBIND_OUT = "1705032704\n-294967296\n-2\n"

SIZE_STD_OUT = "-294967296\n4\n2147483647\n4\n"


def test_raw_wasi_program():
    module = compile_with_moss(wasi("raw"), "")
    assert module[:8] == b"\x00asm\x01\x00\x00\x00"
    # proc_exit(3).
    assert run_module_code(module, [], 3) == "A\n"


def test_control_flow_and_i64():
    assert run_moss(wasi("prim"), "") == "ABKDKJGG\n"


# A function is compiled in the module that declared it: its names
# resolve there, not where it was called from.


def test_calls_across_modules():
    assert run_moss(wasi("across"), "") == "AC\n"


# D58 and D59's value model: a nominal tag over a scalar is that scalar,
# a union of units is one scalar of discriminant, and `match` is the only
# thing that has to tell them apart. Nothing here allocates.


def test_tags_units_and_match():
    assert run_moss(wasi("tags"), "") == "EBACEAB\n"


# D2: a val is the only context that exists at runtime, so a function
# that assumes one is implicitly parameterized by it and that
# parameterization is a real Wasm parameter. Covers a need threaded
# through a frame that does not use it, a `bind` shadowed inside a block,
# and one rebound at the same level.


def test_contextual_vals():
    assert run_moss(wasi("ctx"), "") == "CEECI\n"


# The whole of what a `bind` can do: a type, a val, a plain function and
# a detached method at a receiver, all provided at once by applying a
# functor (D55). Code written against the signature alone still compiles
# to a direct call (D2), which means the callee is compiled once per
# environment that reaches it, and the keys are compared after
# canonicalization through the bindings in force (D43) — the callee knows
# its receiver by a symbol the caller has since bound.


def test_functor_and_the_static_context():
    assert run_moss(wasi("functor"), "") == "ABD\n"


# The rest of D59's value model. `Read | Eof` is a discriminant beside
# room for the payload, which needs three things at once: a member
# injecting where it enters the union, a `match` that reads the
# discriminant and then the payload, and a function returning two
# scalars — a multi-result type. This is the shape src/lex.moss's
# `peek()` returns.


def test_a_union_wider_than_one_scalar():
    assert run_moss(wasi("wide"), "") == "ABCD\n"


# `put_bytes`, compiled by the compiler written in Moss — which is also
# how it writes its own output, so this is the one primitive whose cost
# the fixpoint test pays directly.


def test_a_bulk_write():
    assert run_moss(wasi("bulk"), "") == "hi!\n"


# A receiver an application binds. A context item may key a detached
# method on an *abstract* type — `context IsList = L.get;` — which
# `IsList[T=Int, L=IntArr]` then fixes, and the provision has to land at
# `IntArr` rather than at `L` or no call finds it. That is D51's generic
# container, and the same mechanism the library's own
# `String.get[Elem=Char]` uses with the receiver written concretely, so
# this is the case where the brackets actually have to be read.
#
# Two instantiations coexist here, which is the point: the keys are
# (IntArr, get) and (CharArr, get), distinct atoms, so nothing merges.


def test_a_generic_container():
    assert run_moss(wasi("generic"), "") == "hiDCC\n"


# D26: a bound provider runs with the vals in force where it was bound.
# Its captured values travel under keys of their own, so a rebind between
# the bind and the call — in the calling frame, a frame above it, or
# inside a provider that captured a bind of its own — cannot reach it,
# through a plain fn or a detached method.


def test_a_fn_bind_captures_at_the_bind_site():
    assert run_moss(wasi("capture"), "") == "CCCCE\n"


# D65: a bare block is an expression. The val and fn binds made inside it
# are gone at its end, a `let` inside it shadows only until then while an
# assignment to an outer `var` stays, and its value is its tail — as a
# `let` initializer and as an argument.


def test_a_bare_block_scopes_its_binds_and_lets():
    assert run_moss(wasi("block"), "") == "EDCBGAIEE\n"


# D66: `bind f = g;` with `g` abstract is a bind of a forwarder to `g`,
# so it captures `g` as provided at the bind — at a type a bind fixed,
# inside a functor written against the signature alone, and for a
# primitive. Rebinding `g` afterwards, in the same frame or deeper in the
# call tree, does not reach `f`.


def test_a_fn_bound_to_a_contextual_fn():
    assert run_moss(wasi("forward"), "") == "CCEDEC\n"


# D54: a plain function may provide a method, with the method's declared
# parameters — the receiver excluded — so it cannot see what the method
# was called on, though the call still evaluates the receiver. An
# attached provider takes it as `this`. The self-hosted compiler used to
# pass the receiver to a plain provider anyway (invalid Wasm here; a
# plain fn written to take it was accepted); it and the oracle now run it
# alike.


def test_a_plain_fn_provides_a_method():
    entry = wasi("plain_provider")
    assert oracle_run(entry, []) == "abrcd\n"
    assert run_moss(entry, PRELUDE) == "abrcd\n"


# D54, the other half: a plain provider written to take the receiver as a
# first parameter is a signature mismatch (the oracle's message;
# `e_signature`, `S`, named by the method, in the self-hosted compiler,
# which used to accept it), and `this` in one is an error (`e_this`, `T`).


def test_a_plain_fn_provider_cannot_take_or_see_the_receiver():
    refused(
        [
            (
                "plain_provider_receiver",
                "method bind signature mismatch: provider `second` has (Box[Char=Char]) -> Char, need () -> Char; a plain function providing a method takes the method's parameters, which exclude the receiver (D54)",
                "!S {} m\n",
            ),
            ("plain_provider_this", "`this` is only legal inside method bodies", "!T {}\n"),
        ],
        True,
    )


# D70: `(params) => body` as a fn or method bind's right side —
# unannotated and annotated, capturing a `let` and a `var` by value (an
# assignment after the bind is not seen), capturing context items as they
# are at the bind (a later rebind of a fn or a val is not seen), in a
# functor's body, as a method provider seeing the receiver as `this`
# through a wrapper and with none at all, with a block body, one inside
# another, and one yielding a float (whose body is a block yielding an
# f64). The oracle and the self-hosted compiler agree.


def test_anonymous_functions_at_binds():
    entry = wasi("lambda")
    assert oracle_run(entry, []) == LAMBDA_OUT
    assert run_moss(entry, PRELUDE) == LAMBDA_OUT


# D70's errors in both front ends: a parameter count or an annotation
# that is not the bound declaration's (`e_signature`, `S`, named by the
# bound fn), an assignment to a captured local and a lambda anywhere but
# a bind (`e_lambda`, `U`), `this` in one bound to a fn (`e_this`, `T`),
# and a lambda calling the fn it is bound to, which means what it meant
# at the bind — here nothing (`c_unbound`, `?Q`).


def test_anonymous_function_errors():
    refused(
        [
            (
                "lambda_arity",
                "the anonymous function bound to `step` takes 2 parameter(s), but `step` takes 1 (D70)",
                "!S {} step\n",
            ),
            (
                "lambda_annotation",
                "parameter `c` of the anonymous function bound to `step` is annotated `Int`, but `step` declares `Char` there (D70)",
                "!S {} step\n",
            ),
            (
                "lambda_assign",
                "`seen` is a local captured by an anonymous function, which captures it by value and may not assign it (D70)",
                "!U {} seen\n",
            ),
            (
                "lambda_outside",
                "an anonymous function may only be the right side of a fn or method bind (D70)",
                "!U {}\n",
            ),
            (
                "lambda_this",
                "`this` is only legal inside method bodies; an anonymous function bound to a fn has no receiver",
                "!T {}\n",
            ),
            (
                "lambda_recursive",
                "`step` is not available in the context here",
                "?Q {} step step\n",
            ),
        ],
        False,
    )


# D67: f32 and f64 are scalars like i32 and i64 — in locals, parameters,
# results, record fields and memory, through the whole of `Wasm`'s float
# half, f32 rounding to single precision.


def test_float_scalars():
    assert run_moss(wasi("float"), "") == FLOAT_OUT


# D68's operators, compiled as the calls of `num.moss`'s methods they
# are, at `Int` and at `Bool`.


def test_operators():
    assert run_moss(wasi("ops"), PRELUDE) == OPS_OUT


# D68's literals, at f64 and — in a block that binds `WasiFloats32` — at
# f32, from one `report` function.


def test_literals():
    assert run_moss(wasi("literals"), PRELUDE) == LITERALS_OUT


# D68's exponent notation, at f64 and at f32.


def test_exponents():
    assert run_moss(wasi("exponents"), PRELUDE) == EXPONENTS_OUT


# D69's packed lists: an `IntList` and a `FloatList` side by side, at f64
# and — in a block binding `WasiFloats32` — at f32, from one `report`
# function and one list implementation.


def test_float_lists():
    assert run_moss(wasi("lists"), PRELUDE) == LISTS_OUT


# tests/wasi/overflow.moss in the self-hosted compiler and the oracle:
# the wrapped literal, the checked one that fits, and then a trap
# (wasmtime's exit status for one is 134, as is the oracle's) where the
# checked one does not.


def test_an_oversized_literal_does_what_the_arithmetic_does():
    entry = wasi("overflow")
    oracle_traps(entry, OVERFLOW_OUT, "[trap unreachable]")
    out = wasmtime_out(build_with(entry, PRELUDE))
    assert (out.code, out.text()) == (TRAP, OVERFLOW_OUT)
    assert "unreachable" in out.stderr, f"{out.stderr}"


# D68: `c + c` where nothing provides `Char.add` is reported exactly as
# `c.add(c)` is — `e_method` naming `add` — by the self-hosted compiler,
# and refused by the oracle.


def test_an_operator_the_context_lacks_is_the_method_call_error():
    entry = wasi("unprovided")
    oracle_rejects(entry)
    report = rejection(entry, PRELUDE)
    assert "?K tests/wasi/unprovided.moss by_operator add\n" in report, f"{report}"
    assert "?K tests/wasi/unprovided.moss by_method add\n" in report, f"{report}"


def test_generic_code_does_not_mix_int_and_size():
    report = rejection(wasi("size_mix"), PRELUDE)
    assert "?S tests/wasi/size_mix.moss total\n" in report, f"{report}"


# D67's `Float`, written against `Std` and run under two of them:
# `WasiStd`, which provides it at f64, and one assembled the same way
# with `WasiFloats32`. One `report` function, two compiled
# specializations, two precisions.


def test_float():
    assert run_moss(wasi("newton"), PRELUDE) == NEWTON_OUT


# Two runners, one program: the self-hosted compiler's module and the
# oracle do the same.
def test_matches_the_bootstrap_on_behaviour():
    cases = [
        ("prim", "ABKDKJGG\n", ""),
        ("across", "AC\n", ""),
        ("tags", "EBACEAB\n", ""),
        ("ctx", "CEECI\n", ""),
        ("functor", "ABD\n", ""),
        ("wide", "ABCD\n", ""),
        ("generic", "hiDCC\n", ""),
        ("bulk", "hi!\n", ""),
        ("capture", "CCCCE\n", ""),
        ("block", "EDCBGAIEE\n", ""),
        ("forward", "CCEDEC\n", ""),
        ("float", FLOAT_OUT, ""),
        ("plain_provider", "abrcd\n", PRELUDE),
        ("lambda", LAMBDA_OUT, PRELUDE),
        ("phantom", "42\nz\n42 z\n", PRELUDE),
        ("packed", "3000000000\n", PRELUDE),
        ("concrete_list", "2\n", PRELUDE),
        ("lambda_list", "2\n7\n", PRELUDE),
        ("resolution", "91122\n", PRELUDE),
        ("gc_tree", GC_TREE_OUT, PRELUDE),
        ("gc_ast", GC_AST_OUT, PRELUDE),
        ("gc_two", GC_TWO_OUT, PRELUDE),
        ("gc_resolution", "42\n7\n42\n", PRELUDE),
        ("unit_beside_ref", "42\n7\n", PRELUDE),
        ("gc_array", GC_ARRAY_OUT, PRELUDE),
        ("iterate", "503217abc\n", PRELUDE),
        ("size_rebind", SIZE_REBIND_OUT, PRELUDE),
        ("size_std", SIZE_STD_OUT, PRELUDE),
        ("gc_list", GC_LIST_OUT, PRELUDE),
        ("gc_mutable", "0\n2\n7\n3\n42\n", PRELUDE),
        ("gc_array_memory", "6\n6\n", PRELUDE),
        ("records", RECORDS_OUT, PRELUDE),
        ("nested", NESTED_OUT, PRELUDE),
        ("newton", NEWTON_OUT, PRELUDE),
        ("shadow", SHADOW_OUT, PRELUDE),
        ("ops", OPS_OUT, PRELUDE),
        ("literals", LITERALS_OUT, PRELUDE),
        ("exponents", EXPONENTS_OUT, PRELUDE),
        ("lists", LISTS_OUT, PRELUDE),
    ]

    def verify(case):
        name, expected, prelude = case
        entry = wasi(name)
        assert run_moss(entry, prelude) == expected, entry
        assert oracle_run(entry, []) == expected, entry

    assert len(parallel_map(cases, verify)) == len(cases)
    print(f"behaviour agreement over {len(cases)} programs", file=sys.stderr)


# D75: a recursive tree through `Ref[Boxed=Expr]`, and the thesis's
# interpreter written against an abstract `ExprRef`, run over an index
# pool and then over the GC heap through the knot-tied `Node`, and an
# `Expr` and a `Stmt` boxed side by side in one scope, give the same
# output in the oracle and compiled. The compiled module has its struct
# types as a `rec` group, builds with `struct.new`, and — since no
# program forges a reference — contains no `ref.cast`.


def test_gc_references():
    for name, expected in [
        ("gc_tree", GC_TREE_OUT),
        ("gc_ast", GC_AST_OUT),
        ("gc_two", GC_TWO_OUT),
    ]:
        entry = wasi(name)
        assert oracle_run(entry, []) == expected, f"{entry}"
        module = build_with(entry, PRELUDE)
        assert starts_with_rec_group(module), f"{entry}"
        text = wasm_text(module)
        assert "struct.new" in text, f"{entry}"
        assert "struct.get" in text, f"{entry}"
        assert not ("ref.cast" in text), f"{entry}"
        assert run_module(module, []) == expected, f"{entry}"


# D81: arrays of one-scalar types (an `Int`, a reference) are one GC
# array, and of wider ones (a record, a record of an i64, an i32 and a
# reference) a struct of one array per scalar; a growable list built in
# Moss on them and on `Ref.set` runs the same generic code as `Std`'s
# packed list; `Ref.set` makes only the struct it is applied at mutable;
# and an array is used across a `Memory` bind. The same output in the
# oracle and compiled.


def test_gc_arrays():
    for name, expected in [
        ("gc_array", GC_ARRAY_OUT),
        ("gc_list", GC_LIST_OUT),
        ("gc_mutable", "0\n2\n7\n3\n42\n"),
        ("gc_array_memory", "6\n6\n"),
    ]:
        entry = wasi(name)
        assert oracle_run(entry, []) == expected, f"{entry}"
        module = build_with(entry, PRELUDE)
        assert starts_with_rec_group(module), f"{entry}"
        text = wasm_text(module)
        structs = [
            line
            for line in text.splitlines()
            if (line.strip().startswith("(type") and "(struct" in line)
        ]
        if name == "gc_mutable":
            # `Counter`'s struct is written, `Int`'s is not.
            assert sum("(mut" in line for line in structs) == 1, f"{entry}"
            assert "struct.set" in text, f"{entry}"
            assert not ("array.new" in text), f"{entry}"
        else:
            assert "array.new" in text, f"{entry}"
            assert "array.get" in text, f"{entry}"
        if name == "gc_array":
            for op in ["array.set", "array.fill", "array.copy", "array.len"]:
                assert op in text, f"{entry}: {op}"
            assert "(array (mut i64))" in text, f"{entry}"
        assert not ("ref.cast" in text), f"{entry}"
        assert run_module(module, []) == expected, f"{entry}"


# D81: an index past an array's length traps, as the instruction does;
# and an `Array` forged from a bare `ArrayRef` at another type than it
# was made at traps at the cast — `Char` is one i32 like `Int`, but an
# array type of its own. Compiled, after the line before; the oracle
# takes both traps after the same preceding output.


def test_gc_arrays_trap():
    for name, trap, casts in [
        ("gc_array_bounds", "out of bounds array access", 0),
        ("gc_array_forge", "cast failure", 2),
    ]:
        entry = wasi(name)
        if name == "gc_array_forge":
            oracle_traps(entry, "42\n", "[trap 37]")
        else:
            # The oracle names this trap only by its code (7, an array
            # index out of bounds): its driver has no message for it.
            oracle_traps(entry, "42\n", "[trap 7]")
        module = build_with(entry, PRELUDE)
        assert wasm_text(module).count("ref.cast") == casts, f"{entry}"
        out = wasmtime_out(module)
        assert (out.code, out.text()) == (TRAP, "42\n"), f"{entry}"
        assert trap in out.stderr, f"{entry}: {out.stderr}"


# D75: rebuilding a `Ref` from the bare `StructRef` it was taken apart
# into is the one cast. At the type the struct was made at it succeeds;
# at another it traps at the construction, compiled, before anything is
# read.


def test_a_forged_reference_traps_where_it_is_forged():
    entry = wasi("gc_forge")
    oracle_traps(entry, "3\n4\n", "[trap 37]")
    module = build_with(entry, PRELUDE)
    assert wasm_text(module).count("ref.cast") == 2
    out = wasmtime_out(module)
    assert (out.code, out.text()) == (TRAP, "3\n4\n")
    assert "cast failure" in out.stderr, f"{out.stderr}"


# D78: a functor with no body is an abstract functor, anywhere — a
# requirement that a scope provides by binding a functor to it, and that
# `bind F;` applies through whatever it is bound to. `Trace` bound to two
# tracers in one program and applied inside a generic function; `Fast`,
# whose result holds the type `Float`, bound to f64 and to f32 floats for
# one kernel; a library applying an abstract allocator whose result holds
# the handle type, for two types, bound to a GC-heap allocator in one
# block and to a pool of indices in another, each with fewer inputs than
# the allocator takes; an abstract functor producing a val, evaluated at
# each application; and D75's `gc_bodiless`, whose bodiless functor
# outside lib/wasm.moss is now legal and bound to `Heap`. In the oracle
# and compiled.


def test_abstract_functors():
    alloc = "21\n4\n14\n" * 2
    for name, expected in [
        ("functor_trace", FUNCTOR_TRACE_OUT),
        ("functor_fast", FUNCTOR_FAST_OUT),
        ("functor_alloc", alloc),
        ("functor_same_handle", "21\n4\n14\n"),
        ("functor_val", "10\n20\n30\n3\n5\n6\n0\n"),
        ("gc_bodiless", "42\n"),
    ]:
        entry = wasi(name)
        assert oracle_run(entry, []) == expected, f"{entry}"
        assert run_moss(entry, PRELUDE) == expected, f"{entry}"


# A fresh result type overrides its old input meaning when result methods
# are exported, while the other input keeps its original receiver.


def test_functor_outputs_shadow_input_type_arguments():
    entry = wasi("functor_output_input_shadow")
    assert oracle_run(entry, []) == "yn"
    assert run_moss(entry, PRELUDE) == "yn"


# Rebinding output T keeps the old, merged U generation usable.


def test_native_functor_outputs_preserve_merged_input_generations():
    entry = "tests/regressions/functor_input_output_generations.moss"
    assert run_moss(entry, PRELUDE) == "yn"


# The reference keeps the old input generation separate from the output.


def test_oracle_functor_outputs_preserve_merged_input_generations():
    entry = "tests/regressions/functor_input_output_generations.moss"
    assert oracle_run(entry, []) == "yn"


# D84's standing tests that run: a functor's own inputs read per
# application (`fn_input`), a captured val across frames, a captured
# memory, merged polymorphic inputs, arrays in a scratch memory, an
# abstract functor in a result, family members on demand (rule 7), for a
# call of the member and for a callee that needs it, and a polyfill of
# the platform's `Heap` (O2). The oracle and the self-hosted compiler
# agree, but for the two the oracle does not model.


def test_d84_functors_run():
    for name, expected in [
        ("functor_input_per_application", "22\n"),
        ("functor_capture_val", "55\n"),
        ("functor_capture_memory", "07\n"),
        ("functor_merge_bracketed", "tt\n"),
        ("arrays_in_scratch", "6\n"),
        ("functor_in_result_ok", "5\n"),
        ("arrays_on_demand", "33\n"),
        ("family_need", "55\n55\n6\n"),
        ("family_arg_capture", "73\n"),
        ("family_cross_frame", "55\n"),
        ("family_fixed_linked", "1\n"),
        ("functor_polyfill", "7\n"),
    ]:
        entry = wasi(name)
        assert oracle_run(entry, []) == expected, f"{entry}"
        assert run_moss(entry, PRELUDE) == expected, f"{entry}"


# D84's `dd_arrays`: the thesis's `sd` over `Array[Item=Float]` with
# `for`, under `DoubleDouble` and `Shadow`, which have no array lines.
# The oracle runs it, and so does the self-hosted compiler's module.


def test_d84_arrays_follow_a_rebound_float():
    entry = wasi("dd_arrays")
    expected = "0\n100000000\n1000000000\n"
    assert oracle_run(entry, []) == expected
    assert run_moss(entry, PRELUDE) == expected


# D84's standing tests that are refused, by the oracle and the
# self-hosted compiler: a fixed input rebound before an application
# (`StructRef`, `I32`), a copy narrower than its source, a captured item
# mentioning a type applications supply, a pin through a polymorphic
# input (the "held" closure), a pin inside a functor result, an
# unbracketed functor's binding that pins what it is polymorphic in, and
# rule 7's conditions: two families producing one key, a member bound
# directly, two items of one family sharing a key's instance, and a
# family binding applying a functor that is not a family.


def test_d84_functors_refused():
    for name, message in [
        (
            "functor_fixed_stale",
            "applying `Heap` here reads `StructRef` as `I32`, but `Heap` is fixed in it",
        ),
        (
            "functor_fixed_stale_i32",
            "applying `Arrays` here reads `I32` as `I64`, but `Arrays` is fixed in it",
        ),
        (
            "functor_copy_narrower",
            "it is fixed in `StructRef`, in which `Boxing` is polymorphic (D84 rule 4)",
        ),
        (
            "functor_capture_mentions_poly",
            "captures `mk0`, which mentions a type each application of `F` supplies (D84 3b)",
        ),
        ("functor_pin_through_input", "a type each application of `F` supplies (D84 3d)"),
        ("functor_in_result_pin", "it pins `K`, which `Outer` is fixed in"),
        ("functor_unbracketed_pin", BIND_INT_CHANGES_RESULT),
        ("family_collision", "`Int.m` is produced by two families in force"),
        ("family_direct", "is a member of the family `Arrays`"),
        ("family_ambiguous", "is two items of the family `F` here"),
        ("family_apply", "applies `Tick`, which is not a family"),
    ]:
        entry = wasi(name)
        oracle_says(entry, message)
        rejection(entry, PRELUDE)


# D78: `Ref[Boxed]`'s payload is the platform's abstract `StructRef`, so
# a scope can rebind it to an `I32` address and provide `.boxed` and
# `.deref` over linear memory. The same generic code boxes on the GC heap
# in one block and in an arena in the other, and the back end decides "is
# a reference" by `Ref`'s payload at the specialization, not by the `Ref`
# symbol — so the arena's `Ref(p)` is no cast, and its `Ref`s are `i32`s.


def test_ref_over_an_arena():
    entry = wasi("arena_ref")
    assert oracle_run(entry, []) == ("29\n42\n" * 2)
    module = build_with(entry, PRELUDE)
    text = wasm_text(module)
    assert "struct.new" in text
    assert not ("ref.cast" in text)
    assert run_module(module, []) == ("29\n42\n" * 2)


# D78: `bind F = G;` needs `G`'s inputs among `F`'s and `G`'s result
# exactly `F`'s, checked where the bind is written (more inputs, a
# missing output, an extra output); `G`'s binds may use only `G`'s
# inputs, whatever an application has; `bind F;` with nothing bound to
# `F` is the ordinary error for a requirement not in context; a functor
# bound to one whose result holds a type must provide that type too; each
# item of the result must be made at the type the application promises;
# and no value of a type `bind F;` made may leave its block. Rejected by
# the oracle and the self-hosted compiler.


def test_abstract_functor_binds_are_checked():
    refused(
        [
            (
                "functor_inputs",
                "`Counting`, bound to `Trace`, captures `counter`, which mentions a type each application of `Trace` supplies (D84 3b)",
                "!X {} counter\n",
            ),
            (
                "functor_missing",
                "`Half` cannot be bound to `Trace`: it does not provide `flush`, which `Trace` promises",
                "!X {} flush\n",
            ),
            (
                "functor_extra",
                "`Chatty` cannot be bound to `Trace`: it also provides `flush`, which `Trace` does not",
                "!X {} flush\n",
            ),
            (
                "functor_body",
                "calling `count` needs `counter`, which is not available in the context here",
                "!R {0} counter\n!P {0} counter\n",
            ),
            (
                "functor_unbound",
                "`Trace` is not available in the context here",
                "?Q {} demo Trace\n",
            ),
            (
                "functor_type",
                "`Twice` cannot be bound to `Pick`: it does not provide `Scalar`, which `Pick` promises",
                "!X {} Scalar\n",
            ),
            ("functor_promise", BIND_INT_CHANGES_RESULT, "?X {} main Int\n"),
            ("functor_promise_candidate", BIND_INT_CHANGES_RESULT, "?X {} other Int\n"),
            ("functor_body_type", "type `Int` is not available in the context here", "!X {} Int\n"),
            ("functor_escape", ESCAPES_FAST, "?W {} leak Float\n"),
        ],
        False,
    )


# D72 and D78: two generations of a type that specialize to one type are
# still two types, since a function is checked as written, against its
# generic types: `x`, a `T` from before `bind T = Int;`, used as a `T`
# after it where `T` was `Int` all along; and a value of the `Float`
# `bind Fast;` made leaving its block where that `Float` is the f64 the
# outer one is too. The self-hosted compiler used to check types as
# specialized, and compiled both. Rejected by both front ends.


def test_generations_that_specialize_alike_are_told_apart():
    refused(
        [
            ("generation_mix", "`y` is declared `Int` but initialized with `T`", "?S {} mix\n"),
            ("functor_escape_same", ESCAPES_FAST, "?W {} leak Float\n"),
        ],
        False,
    )


# Reference/scalar union payloads keep separate typed lanes, including in
# recursive GC boxes. The oracle evaluates the same tree without a layout.


def test_union_payloads_keep_distinct_reference_and_scalar_lanes():
    entry = wasi("gc_slots")
    assert run_moss(entry, PRELUDE) == "2\n"
    assert oracle_run(entry, []) == "2\n"


# The rest of D59's value model: a record is its fields' runs, one after
# another, so records nest, sit inside unions of different widths, travel
# as parameters, returns and contextual vals, and destructure by name —
# renamed, wildcarded, or whole.


def test_records():
    entry = wasi("records")
    assert run_moss(entry, PRELUDE) == RECORDS_OUT
    # The oracle too, which matches a brace tag with a binder pattern
    # (`Box(b)`, D83) by binding its fields as a record.
    assert oracle_run(entry, []) == RECORDS_OUT


# tests/wasi/order.moss: a call evaluates its receiver, then its
# arguments left to right — an attached method, a provided one, and an
# operator — compiled and in the oracle.


def test_receiver_first_then_arguments_left_to_right():
    entry = wasi("order")
    expected = "rab\nrab\nxy\n"
    assert run_moss(entry, PRELUDE) == expected
    assert oracle_run(entry, []) == expected


# tests/wasi/memory_bounds.moss: memory grows only by `memory_grow`, so a
# load past its end traps (wasmtime's exit status for one is 134, as is
# the oracle's), compiled and in the oracle.


def test_a_load_past_the_end_of_memory_traps():
    entry = wasi("memory_bounds")
    oracle_traps(entry, "a\n", "[trap out of bounds memory access]")
    out = wasmtime_out(build_with(entry, PRELUDE))
    assert (out.code, out.text()) == (TRAP, "a\n")
    assert "out of bounds memory access" in out.stderr, f"{out.stderr}"


# Inside `assume Std`, every function needs every `Std` val, and each
# provider bound to one of its fn needs brings its own along. Passing them
# all ran `bar` here past Wasm's thousand-parameter limit; the back end
# passes only the vals a function can reach.


def test_only_reachable_vals_are_passed():
    entry = wasi("levels")
    assert run_moss(entry, PRELUDE) == "1\n3\n"
    assert oracle_run(entry, []) == "1\n3\n"


# A recursive function that rebinds one of its own fn needs on its own
# recursive path. Keyed by everything in scope, each specialization
# captured the provision it shadowed and demanded another, without end;
# keyed by what is reached, there are two. Its `Float` result also has to
# be learned, not assumed an i32.


def test_rebinding_on_the_recursive_path_terminates():
    entry = wasi("rebind_recursive")
    assert run_moss(entry, PRELUDE) == "2\n1\n0\n11\n"
    assert oracle_run(entry, []) == "2\n1\n0\n11\n"


# A functor's binds are as if written in place: an anonymous function
# they bind can name the provision it shadows, and so needs it, with the
# val that provider captured, though the functor does not assume it.


def test_a_functor_provision_captures_the_one_it_shadows():
    entry = wasi("functor_shadowed_provision")
    assert run_moss(entry, PRELUDE) == "5\n1105\n"
    assert oracle_run(entry, []) == "5\n1105\n"


# D84 6: a copy keeps its source's capture. `bind Get2 = Get;` copies
# `Get`'s binding to `Sum`, which captured `seed = 3`, and applying
# `Get2` in another frame still reads that `seed`. The self-hosted
# compiler used to look the captured vals up under `Get`'s entry, which
# that frame does not have, and refused the application.


def test_a_functor_copy_keeps_its_capture():
    entry = wasi("functor_copy_capture")
    assert oracle_run(entry, []) == "2454\n"
    assert run_moss(entry, PRELUDE) == "2454\n"


# AUDIT-CHANGES #58 (E40, D84 rule 7): two result items distinct in
# `F`'s own reading stay distinct where it is applied. Binding `A` and
# `B` both to `Int` would file `A.m` and `B.m` under one key; the
# self-hosted compiler refuses it (`e_result_collide`, `^`) in `use_it`, named by the
# method, as the semantics and the oracle do. A key `F` itself repeats is
# one item, and runs.


def test_result_items_distinct_in_the_functor_stay_distinct():
    for name in ["result_collide_ab", "result_collide_bindswap"]:
        entry = wasi(name)
        assert rejection(entry, PRELUDE) == f"?^ {entry} use_it m\n"
        out = oracle(entry, [])
        assert out.code == ORACLE_REJECTED, f"the oracle on {entry}: {out.stderr}"
    entry = wasi("result_repeat")
    assert oracle_run(entry, []) == "a\n"
    assert run_moss(entry, PRELUDE) == "a\n"


# Δ4 (D43): merging two entries of one item also unifies their
# instantiations, so the order of an assume list does not decide which
# instantiation the merged key has. `g` assumes `f` at `T = A` and at
# `T = B`, so its context merges `A` with `B`, and `h`, where they are
# distinct, cannot call it (`e_merge`, `Y`), in either order (both are
# in tests/errors, with merge_inst_order_ba.moss); and a val's two entries
# make `A` and `B` one type where the val's own type does not mention
# `T`.


def test_merging_an_item_unifies_its_instantiations():
    entry = errors("merge_inst_order_ab")
    assert rejection(entry, PRELUDE).startswith(f"!Y {entry} A\n"), f"{entry}"
    build_with(wasi("merge_inst_val"), PRELUDE)
    # A fn whose signature does not mention `T` likewise (merge_inst_fn);
    # the controls merge nothing the program relies on, or merge through
    # the signature.
    for name in ["merge_inst_fn", "merge_inst_nomerge", "merge_inst_sig"]:
        build_with(wasi(name), PRELUDE)


# Q3 (AUDIT-CHANGES #70): `bind Meter.describe = ...` names the
# attached, defined `Meter.describe`, which calls always reach (D61
# rule 1), so the bind could never be used: refused (`e_bind_defined`,
# `_`), named by the method. Binding the detached method through an
# abstract receiver, or an abstract attached method, is still how they
# get a meaning.


def test_a_defined_method_is_not_bound():
    entry = errors("bind_attached_defined")
    assert rejection(entry, PRELUDE) == f"?_ {entry} demo describe\n"
    for name, out in [("bind_attached_detached", "mxm\n"), ("bind_attached_abstract", "x\n")]:
        entry = wasi(name)
        assert oracle_run(entry, []) == out, f"{entry}"
        assert run_moss(entry, PRELUDE) == out, f"{entry}"


# Rule B (AUDIT-CHANGES #60, E42): calling a defined fn, or binding it
# as a provider, needs each type its assume nesting requires in force
# where it is used, or in force through its merge class (`e_absent`,
# `P`, named by the type); and `bind H = P;` needs `P`'s inputs that `H`
# does not take in force where `P` is captured. A family demand at a
# requirement out of force is refused at the call that would make it.
# partially_bound_merge.moss is the merge-class control.


def test_a_use_needs_its_requirements_in_force():
    for name, expected in [
        ("ruleb_call_unbound", "!P {} T\n"),
        ("ruleb_provider_unbound", "!P {} T\n"),
        ("ruleb_family_unbound", "!P {} T\n"),
        ("ruleb_closure_unbound", "?P {} main T\n"),
    ]:
        entry = errors(name)
        assert rejection(entry, PRELUDE) == fill(expected, entry), f"{entry}"
    build_with(wasi("ruleb_closure_bound"), PRELUDE)


# Rule B through a merge class, for a family demand: `f`'s `Y`, which
# `demo` leaves unbound, is in force through the `X` it merges with, so
# the demand of `Y.foo` is made (tests/wasi/ruleb_merge_family_demand.moss;
# AUDIT-CHANGES #60). The self-hosted
# compiler read the demand's matching in `demo`'s context only, and
# refused it (`e_absent`, `Y`).


def test_a_family_demand_is_in_force_through_a_merge():
    entry = wasi("ruleb_merge_family_demand")
    assert oracle_run(entry, []) == "k\n"
    assert run_moss(entry, PRELUDE) == "k\n"


# Q6 (AUDIT-CHANGES #71): a body may pin a fixed input of the functor it
# is bound to when the pin agrees with the bind site's reading
# (pin_fixed_agree, `q`); tests/errors/pin_fixed_disagree.moss and
# tests/errors/pin_poly.moss are the refused variants.


def test_a_pin_of_a_fixed_input_agrees_with_the_bind_site():
    entry = wasi("pin_fixed_agree")
    assert oracle_run(entry, []) == "q\n"
    assert run_moss(entry, PRELUDE) == "q\n"


# Q4: a call's arguments are checked in the caller's context, not with
# the call's demanded families in force, so `d.describe()` passed to `f`
# or to `d.g` is the attached method both times (`mm`), on the oracle
# too (AUDIT-CHANGES #72; it printed `xm`, checking a fn call's arguments
# with the demand in force). So an argument a demand would shadow still
# has its caller's type (args_shadow_call, `42`), and a family-demanded
# method call already read its arguments so (args_family_call, `m`).


def test_arguments_are_read_in_the_callers_context():
    for name, out in [
        ("args_in_caller_context", "mm\n"),
        ("args_shadow_call", "42\n"),
        ("args_family_call", "m\n"),
    ]:
        entry = wasi(name)
        assert oracle_run(entry, []) == out, f"{entry}"
        assert run_moss(entry, PRELUDE) == out, f"{entry}"


# D73's listed parameters must be types the declaration requires, and
# D74's attached method is at its receiver's instantiation: a
# `Box[T=Int]`'s `.get()` is an `Int` wherever `T` has been rebound, and a
# `Cell[Item=Int]` read where the `Char` codec is in force needs its codec
# at `Int`. Both used to read an `Int` as a `Char`.


def test_phantoms_and_receivers_are_checked():
    refused(
        [
            (
                "phantom_unlisted",
                "`Cell` lists `Other`, which is not a type its assume nesting requires (D73)",
                "!V {} Other\n",
            ),
            (
                "receiver_rebound",
                "argument `c` of `putchar` is `Int`, expected `Char`",
                "?S {} demo\n",
            ),
            (
                "phantom_misread",
                "calling `Cell.first` needs `encode` at another instantiation than the one in the context here was provided at",
                "?S {} demo\n",
            ),
            ("gc_misread", "argument `c` of `putchar` is `Int`, expected `Char`", "?S {} demo\n"),
            (
                "gc_unheaped",
                "no method `.boxed` is available for `Int` in the context here",
                "?K {} demo boxed\n",
            ),
            (
                "gc_unarrayed",
                "no method `.arrayed` is available for `Int` in the context here",
                "?K {} demo arrayed\n",
            ),
            (
                "gc_immutable",
                "no method `.Ref.set` is available for `Ref[Boxed=Int",
                "?K {} demo set\n",
            ),
            (
                "gc_array_misread",
                "argument `c` of `putchar` is `Int`, expected `Char`",
                "?S {} demo\n",
            ),
            (
                "gc_needs_boxed",
                "applying `Heap` needs `Boxed`, which is not available in the context here",
                "?Q {} demo Boxed\n",
            ),
        ],
        False,
    )


# D79: a program names a second memory as a unit and binds `Memory` to it
# in a block, where the library's lists live in it. A list in each
# memory, summed; one memory's allocator reset, which reuses its own space
# and leaves the other's data alone; and text made in `Scratch` copied
# into the main memory to be printed, and back. Compiled, each module has
# two memories, exports the first, and puts the second's index on the
# instructions specialized at it. Both runtimes produce the same output.


def test_multiple_memories():
    for name, expected in [
        ("memories", "5050\n51225\n56275\n"),
        ("memory_reset", "51225\n55\n1024\n1\n5050\n3\n1\n"),
        ("memory_copy", "hi\nih\n"),
    ]:
        entry = wasi(name)
        assert oracle_run(entry, []) == expected, f"{entry}"
        module = build_with(entry, PRELUDE)
        text = wasm_text(module)
        assert "(memory $0 " in text, f"{entry}"
        assert "(memory $1 " in text, f"{entry}"
        assert not ("(memory $2 " in text), f"{entry}"
        assert '(export "memory" (memory $0))' in text, f"{entry}"
        assert "i32.load $1" in text, f"{entry}"
        assert "i32.store $1" in text, f"{entry}"
        assert run_module(module, []) == expected, f"{entry}"


# D79 under D84: the memory instructions are `Addr`'s methods, a family
# polymorphic in `Memory`, so binding `Memory` to any type — here `Num`
# and `Chr`, neither a unit — names a memory of its own, and nothing is
# refused after checking (D16).


def test_any_type_names_a_memory():
    entry = wasi("memory_any_type")
    expected = "5050\n51225\n1275\n5050\n"
    assert oracle_run(entry, []) == expected, f"{entry}"
    module = build_with(entry, PRELUDE)
    text = wasm_text(module)
    assert "(memory $2 " in text
    assert not ("(memory $3 " in text)
    assert "i32.store $2" in text
    assert run_module(module, []) == expected, f"{entry}"


# D78 and D79: an abstract functor whose result holds `Memory` and the
# containers over it, bound in `main` to one that puts them in a second
# memory, is an arena: each `bind Arena;` makes fresh types, so the
# trajectories built there are dropped (the allocator reset) at the end
# of each block, and the numbers kept from them live in the main memory.
# Compiled, with the temporaries' loads and stores in memory 1. A list
# made in the arena leaving its block — as the block's value, assigned to
# an outer `var`, or inside a record — is refused by the oracle and the
# self-hosted compiler.


def test_an_arena_whose_data_cannot_outlive_it():
    entry = wasi("arena")
    expected = "6 8 16\n7 16 52\n27 111 9232\n97 118 9232\n"
    assert oracle_run(entry, []) == expected, f"{entry}"
    module = build_with(entry, PRELUDE)
    text = wasm_text(module)
    assert "(memory $1 " in text
    assert not ("(memory $2 " in text)
    assert "i32.load $1" in text
    assert "i32.store $1" in text
    assert run_module(module, []) == expected, f"{entry}"
    refused(
        [
            (
                "arena_escape",
                "this block yields `IntList@Arena`, which mentions `IntList@Arena`, a type `bind F;` made in it: a value of that type cannot leave the block (D78)",
                "?W {} report IntList\n",
            ),
            ("arena_assign", "cannot assign `IntList@Arena` to `kept: IntList`", "?S {} report\n"),
            (
                "arena_wrapped",
                "this block yields `Run[Int=Int, IntList=IntList@Arena]`, which mentions `IntList@Arena`, a type `bind F;` made in it: a value of that type cannot leave the block (D78)",
                "?W {} report IntList\n",
            ),
        ],
        False,
    )


# D72 and D78: two applications of one abstract functor in force at once
# make two generations of each type in its result, and a type error
# between them names them apart (`IntList@Arena#2`), rather than
# "`IntList@Arena`, expected `IntList@Arena`". The self-hosted compiler's
# type errors carry no detail.


def test_two_applications_print_as_two_types():
    entry = wasi("arena_twice")
    # Known oracle gap: its type errors do not tell generations apart, so
    # it says the very thing this test is about; the Python bootstrap said
    # "argument `xs` of `total` is `IntList@Arena`, expected
    # `IntList@Arena#2`".
    oracle_says(entry, "argument `xs` of `total` is `IntList@Arena`, expected `IntList@Arena`]")
    assert rejection(entry, PRELUDE) == f"?S {entry} report\n"


# D79 changes nothing for a program that binds no `Memory`: the module
# has one memory, as before.


def test_one_memory_is_one_memory():
    text = wasm_text(build_with(wasi("lists"), PRELUDE))
    assert "(memory $0 " in text
    assert not ("(memory $1 " in text)
    assert not ("i32.load $1" in text)


# D79: a list or a string lists the memory it is in, so one made in
# `Scratch` is not the main memory's, though both are an address: passing
# a `Scratch` list where `main`'s `IntList` is expected, and printing a
# `Scratch` string as it is (WASI reads only the main memory), are type
# errors in both front ends; and calling WASI where `Memory` is rebound
# is incoherent.


def test_a_handle_is_typed_by_its_memory():
    refused(
        [
            (
                "memory_mismatch",
                "argument `xs` of `sum` is `Packed[Item=Num[I32=I32], Memory=Scratch, I32=I32]`, expected `Packed[Item=Num[I32=I32], Memory=Memory, I32=I32]`",
                "?S {} main\n",
            ),
            (
                "memory_print",
                "argument `s` of `print` is `Str[Memory=Scratch, I32=I32]`, expected `Str[Memory=Memory, I32=I32]`",
                "?S {} main\n",
            ),
            (
                "memory_wasi",
                "calling `wasi_putchar` needs `args_get` at another instantiation than the one in the context here was provided at",
                "!R {} fd_write\n",
            ),
        ],
        False,
    )


# D72: what was typed before a bind of `Int` keeps the `Int` it had, in a
# function generic over it, while a value made after the bind adds with
# the new one's `+`.


def test_a_type_bind_starts_a_generation():
    entry = wasi("generations")
    assert run_moss(entry, PRELUDE) == "-2\n40\n"
    assert oracle_run(entry, []) == "-2\n40\n"


# D72: `bind Float = Dual` over the `Float` in force, inside functions
# generic over `Float`, differentiates code written against it, and twice
# gives a second derivative. Nested differentiation is correct (1)
# because the outer variable has to be lifted into the inner generation;
# added as it is, it is a type error, not perturbation confusion (2). The
# oracle runs the one and rejects the other.


def test_dual_numbers_by_rebinding_float():
    entry = wasi("dual")
    assert run_moss(entry, PRELUDE) == DUAL_OUT
    assert oracle_run(entry, []) == DUAL_OUT
    entry = wasi("perturbation")
    oracle_says(
        entry,
        "argument `rhs` of `.add` is `Dual[Scalar=Dual[Scalar=Float]]`, expected `Dual[Scalar=Float]`",
    )
    out = rejection(entry, PRELUDE)
    assert out.startswith(f"?S {entry} nested\n"), f"{out}"


# D68's exponent literals under a library `Float`: inside `Dualize`,
# `2.5e-1` and `1.5e2` are built from `constant` and `Dual`'s own `.mul`,
# `.add` and `.div`, constants with no derivative, so g(x) = 2.5e-1 x^2 +
# 1.5e2 x has g'(2) = 151. The oracle runs it too.


def test_exponents_under_dual_numbers():
    entry = wasi("exponents_dual")
    assert run_moss(entry, PRELUDE) == "301000000\n151000000\n"
    assert oracle_run(entry, []) == "301000000\n151000000\n"


# D72 as numeric code meets it: `f` assumes all of `Std`, and `Dualize`
# provides all of float.moss's `Floats` at duals — the comparisons,
# `.sqrt`, `.trunc` and `.round`, and `FloatList` as two lists of the base
# `Float` — applied once for f' and inside itself for f''. Every one of
# those providers is an anonymous function made with every one bound
# before it in force, and the self-hosted compiler used to pass each
# function what all of them captured: a module with a function of 4101
# parameters for f', and no end to compiling f''. The oracle runs it too.


def test_dual_numbers_under_all_of_std():
    entry = wasi("dual_std")
    assert run_moss(entry, PRELUDE) == DUAL_STD_OUT
    assert oracle_run(entry, []) == DUAL_STD_OUT


# A method a function assumes but cannot reach by name passes nothing to
# it, as an unreached plain fn already did: `f` assumes twelve methods,
# each provided by an anonymous function that reads `one`, and calls one
# of them. It used to take `one` once per subset of the twelve providers,
# 4095 parameters, which no engine would load.


def test_a_function_takes_only_the_providers_it_reaches():
    entry = wasi("unreached_providers")
    assert run_moss(entry, PRELUDE) == "6\n"
    assert oracle_run(entry, []) == "6\n"


# D26, D70: an anonymous provider captures a val whose type mentions a
# type bound beside the bind (`Holder[Item]`, `Item = Float`, and then a
# record of two `Int`s), and a function generic over the provided fn
# takes the capture to pass on, directly and through a second provider's
# capture. Its type is read where it was captured; read in the callee's
# environment, which knows nothing of `Item`, it was one scalar, and the
# module did not validate.


def test_a_capture_is_laid_out_where_it_was_captured():
    for name, expected in [("capture_width", "15\n29\n15\n"), ("capture_wide", "15\n29\n")]:
        entry = wasi(name)
        assert run_moss(entry, PRELUDE) == expected, f"{entry}"
        assert oracle_run(entry, []) == expected, f"{entry}"


# D55: a functor with binds must bind each item of its result itself, or
# apply a functor that provides it — a type bind does not bind a method
# the type then resolves to (`bind Scalar = Int;` for a promised
# `Scalar.add`) — checked at the functor wherever it is applied, in a
# function nothing calls too. The self-hosted compiler reports
# `e_partial` (`Z`), named by the item; it used to accept both.


def test_a_functor_binds_everything_it_promises():
    refused(
        [
            ("functor_partial", "functor `Over` does not bind `Scalar.add`", "!Z {} add\n"),
            (
                "functor_partial_unreached",
                "functor `Greeter` does not bind `greeting`",
                "!Z {} greeting\n",
            ),
        ],
        True,
    )


# A local comes from emission itself, so a construct that needs a
# temporary works however deeply it is nested: a `match` in a call's
# arguments, as a method's receiver, in a record field or an unannotated
# `let`, and a field of a record that is not in a local. A separate walk
# used to create locals ahead of emission and never looked inside a
# call's arguments.


def test_temporaries_nested_anywhere():
    assert run_moss(wasi("nested"), PRELUDE) == NESTED_OUT


# No heap means no finite layout for a type that contains itself, so it
# is `e_shape` (`N`), naming the type, rather than a compiler that never
# stops laying it out.


def test_a_type_that_contains_itself_is_reported():
    report = rejection(wasi("cyclic"), "")
    assert report.startswith("!N "), f"{report}"
    assert "Cell" in report, f"{report}"


# A path the host refuses -- absent here, but equally one that lies
# outside the directory it opened -- is `e_read` (`O`). The read itself
# cannot report it: `Path.read` has no `String` to return and traps,
# which is why `parse_module` asks `Path.readable` first. Before that, a
# mistyped import came out of the driver as a Wasm backtrace.


def test_a_module_that_will_not_open_is_reported():
    report = rejection(wasi("absent"), "")
    assert report.startswith("!O "), f"{report}"
    assert "tests/wasi/nosuch.moss" in report, f"{report}"


# The D17 check, wired into the back end: a value of one nominal type
# where another is declared, both one scalar wide, so nothing but
# `types::fits` would notice. The oracle rejects it in elaboration; the
# self-hosted compiler reports `c_bad_type` (`?S`) and writes nothing.


def test_a_misfitting_type_is_reported():
    entry = wasi("misfit")
    oracle_rejects(entry)
    report = rejection(entry, "")
    assert report.startswith("?S "), f"{report}"


# Each: (entry, the oracle's message, the self-hosted report, prelude).

UNCALLED = [
    (
        "tests/wasi/uncalled.moss",
        "argument `a` of `f` is `B[I32=I32]`, expected `A[I32=I32]`",
        "?S tests/wasi/uncalled.moss unused\n",
        "",
    ),
    (
        "tests/wasi/uncalled_generic.moss",
        "block yields `T`, expected `I32`",
        "?S tests/wasi/uncalled_generic.moss unused\n",
        "",
    ),
    (
        "tests/wasi/uncalled_lambda.moss",
        "block yields `Int`, expected `Char`",
        "?S tests/wasi/uncalled_lambda.moss step\n",
        PRELUDE,
    ),
    (
        "tests/wasi/functor_unapplied.moss",
        "calling `count` needs `counter`, which is not available in the context here",
        """!R tests/wasi/functor_unapplied.moss counter
!P tests/wasi/functor_unapplied.moss counter
""",
        PRELUDE,
    ),
]

# D16: no post-monomorphization checks, so a function is checked whether
# or not anything calls it — a concrete one, a generic one, of which no
# specialization exists, an anonymous one, and the binds of a functor
# bound to an abstract one that nothing applies. The oracle elaborates
# every function once, against its generic types.


def test_an_uncalled_function_is_checked_by_the_bootstrap():
    for entry, message, _, _ in UNCALLED:
        oracle_says(entry, message)


# The same programs. The self-hosted compiler types a body as it emits
# it, so after the passes that emit what the program reaches, a check
# pass walks everything they did not, generically, and reports what it
# finds, naming the frame.


def test_an_uncalled_function_is_checked_by_the_self_hosted_compiler():
    for entry, _, expected, prelude in UNCALLED:
        assert rejection(entry, prelude) == expected, f"{entry}"


# Each: (entry, the oracle's message, the self-hosted report), compiled
# without the prelude.

MATCH_ARMS = [
    (
        "tests/wasi/match_unreached.moss",
        "match arm yields `B[I32=I32]`, expected `I32`",
        "?S tests/wasi/match_unreached.moss unused\n",
    ),
    (
        "tests/wasi/match_generic.moss",
        "match arm yields `B[I32=I32]`, expected `I32`",
        "?S tests/wasi/match_generic.moss pick\n",
    ),
    (
        "tests/wasi/match_generic_union.moss",
        "match arm yields `B[I32=I32]`, expected `I32`",
        "?S tests/wasi/match_generic_union.moss pick\n",
    ),
    (
        "tests/wasi/match_tag_abstract.moss",
        "`Box` is not a member of `T`",
        "?S tests/wasi/match_tag_abstract.moss pick Box\n",
    ),
]


def oracle_says_each(cases):
    for entry, message, _ in cases:
        oracle_says(entry, message)


def reports_each(cases):
    for entry, _, expected in cases:
        assert rejection(entry, "") == expected, f"{entry}"


# D16 and D34: every arm of a `match` is checked against the scrutinee's
# type as written, whichever arms a specialization would run — on a
# scrutinee of abstract type `T`, whose one head is `T` itself, so that a
# binder or `_` covers it and anything after one is never run, in a
# function nothing calls, in one reached where `T` has one head and where
# it is a union; and a tag pattern on it names no member of `T`.


def test_every_match_arm_is_checked_by_the_bootstrap():
    oracle_says_each(MATCH_ARMS)


# The same programs. The self-hosted compiler takes a match's shape from
# the specialization — one head runs the first arm, and a union stops at
# the first arm that matches anything — and checks the arms it does not
# run all the same, emitting them nowhere.


def test_every_match_arm_is_checked_by_the_self_hosted_compiler():
    reports_each(MATCH_ARMS)


MATCH_PATTERNS = [
    (
        "tests/wasi/match_unit_abstract.moss",
        "`Red` is not a member of `T`",
        "?S tests/wasi/match_unit_abstract.moss pick Red\n",
    ),
    (
        "tests/wasi/match_unit_not_member.moss",
        "`Blue` is not a member of `Red | Green`",
        "?S tests/wasi/match_unit_not_member.moss pick Blue\n",
    ),
]

# D34: a unit or tag pattern, with or without a payload, names a member
# of the scrutinee's type as written — so on a scrutinee of abstract type,
# whose one head is the type itself, only a binder or `_` is a pattern,
# and what a match means cannot depend on what the type is specialized to
# (D16). A bare unit pattern on a `T` bound to `Colour` and to `Green`,
# which the runners used to disagree on, and one naming a unit outside the
# union.


def test_a_pattern_names_a_member_by_the_bootstrap():
    oracle_says_each(MATCH_PATTERNS)


# The same programs.


def test_a_pattern_names_a_member_by_the_self_hosted_compiler():
    reports_each(MATCH_PATTERNS)


MATCH_PATTERN_FORMS = [
    (
        "tests/wasi/match_not_pattern.moss",
        "`red` is not a matchable pattern",
        "?S tests/wasi/match_not_pattern.moss pick red\n",
    ),
    (
        "tests/wasi/match_unit_payload.moss",
        "`Red` has no explicit payload",
        "?S tests/wasi/match_unit_payload.moss pick Red\n",
    ),
]

# D34: a single name is a binder only when it names nothing in scope, and
# otherwise must be a unit or a tag; a pattern with a payload, or a record
# of fields, opens a tag.


def test_a_pattern_is_well_formed_by_the_bootstrap():
    oracle_says_each(MATCH_PATTERN_FORMS)


# The same programs. The self-hosted compiler bound a name that resolves
# to a function, and a unit's payload, which is `()`.


def test_a_pattern_is_well_formed_by_the_self_hosted_compiler():
    reports_each(MATCH_PATTERN_FORMS)


MATCH_EXHAUSTIVE = [
    (
        "tests/wasi/match_not_exhaustive.moss",
        "match is not exhaustive; missing Green",
        "?S tests/wasi/match_not_exhaustive.moss pick Green\n",
    ),
    (
        "tests/wasi/match_empty.moss",
        "match is not exhaustive; missing Red, Green",
        "?S tests/wasi/match_empty.moss pick Red\n",
    ),
]

# D34: without a binder or `_`, a match's arms cover every head of its
# scrutinee's type as written, whichever ones a run reaches; and `match e
# {}` is for an `e` of type `|` alone.


def test_a_match_is_exhaustive_by_the_bootstrap():
    oracle_says_each(MATCH_EXHAUSTIVE)


# The same programs, each reported by the first head missing.


def test_a_match_is_exhaustive_by_the_self_hosted_compiler():
    reports_each(MATCH_EXHAUSTIVE)


BRANCHES_DISAGREE = [
    (
        "tests/wasi/if_disagree.moss",
        "branches disagree: `I32` vs `B[I32=I32]`",
        "?S tests/wasi/if_disagree.moss main\n",
    ),
    (
        "tests/wasi/match_disagree.moss",
        "branches disagree: `I32` vs `B[I32=I32]`",
        "?S tests/wasi/match_disagree.moss pick\n",
    ),
    (
        "tests/wasi/if_disagree_tailless.moss",
        "branches disagree: `()` vs `I32`",
        "?S tests/wasi/if_disagree_tailless.moss main\n",
    ),
    (
        "tests/wasi/if_without_else.moss",
        "an `if` without `else` yields `()`, but its branch yields `I32`",
        "?S tests/wasi/if_without_else.moss main\n",
    ),
]

# With nothing expected of it, an `if`'s branches and a `match`'s arms
# must yield one type, unless one diverges — by its value: a branch block
# without a tail yields `()`, however its last statement ends.


def test_branches_agree_by_the_bootstrap():
    oracle_says_each(BRANCHES_DISAGREE)


# The same programs, compared as written (D80).


def test_branches_agree_by_the_self_hosted_compiler():
    reports_each(BRANCHES_DISAGREE)


# A `return`, a `break`, or a call of a function returning `|`, as the
# value of a branch, beside a branch of any type, with and without an
# expected type. In the oracle.


def test_a_diverging_branch_agrees_by_the_bootstrap():
    assert oracle_run(wasi("branches_diverge"), []) == "DEEIAB\n"


# The same program. A diverging branch is typed `|`, and where the `if`
# or `match` yields a value, a branch that cannot fall through still has
# to validate as one that does.


def test_a_diverging_branch_agrees_by_the_self_hosted_compiler():
    assert run_moss(wasi("branches_diverge"), "") == "DEEIAB\n"


# D16 and D78: `bind F;` in a function nothing calls is checked against
# every functor `F` is bound to anywhere — each must make each item of
# `F`'s result at the type the application promises
# (functor_promise.moss, with `kernel` unreached).


def test_an_uncalled_abstract_application_is_checked_by_the_bootstrap():
    # The oracle's message, in lib/wasistd.moss at `WasiFloats`' `bind
    # Int`; the Python bootstrap said "`WasiFloats` cannot be bound to
    # `Fast`: it binds `Int` to `Num[I32=I32@Fast]`, and `Fast` is
    # polymorphic in `Int` (D84)".
    oracle_says(wasi("functor_promise_unreached"), BIND_INT_CHANGES_RESULT)


# The same program. D84: each binding is checked where it is made,
# generic in what applications supply, whether or not any application is
# reached: `WasiFloats` makes each of `Floats`' items over `Int` at its
# own, which an unbracketed `Fast` is polymorphic in.


def test_an_uncalled_abstract_application_is_checked_by_the_self_hosted_compiler():
    entry = wasi("functor_promise_unreached")
    # The post-body pin check names the changed input before checking
    # signatures of the promised result items.
    assert rejection(entry, PRELUDE) == f"?X {entry} main Int\n"


# D20 and D16: a call needs everything the callee's declaration assumes,
# read off the declaration, not off what its body happens to use: `quiet`
# assumes `X.gimme` and never calls it, and calling it with no `X.gimme`
# in context is rejected by both front ends. The self-hosted compiler
# used to find a need missing only when emitting a use of it.


def test_a_callee_requires_what_it_assumes_and_does_not_use():
    entry = wasi("unused_need")
    oracle_says(
        entry,
        "calling `quiet` needs `X[Char=Char].gimme`, which is not available in the context here",
    )
    assert rejection(entry, PRELUDE) == f"!P {entry} gimme\n"


# Coherence: a callee's val, fn or method requirement is met only by a
# provision made at the instantiation the callee means — a method item
# carries its application, and a val or fn its ambient one.
# Each of these was once accepted by the Python bootstrap,
# which matched the requirement by its key alone, and read a `Char` as an
# `Int` or a `Point`. Both front ends reject each at the call: the oracle
# with the message below, the self-hosted compiler with `e_incoherent`
# (`R`), named by the item. (It used to report `c_bad_type`, `?S`, from
# the back end's `types::fits` on a specialization.)


def test_a_provision_answers_only_its_instantiation():
    for name, callee, needs, made, item in [
        ("incoherent_method", "use_it", "`X[Char=Char].gimme` at [Foo=Int]", "[Foo=Char]", "gimme"),
        ("incoherent_bind_application", "use_it", "`W[Char=Char].m` at [T=Int]", "[T=Char]", "m"),
        ("incoherent_rebound", "use_it", "`W[Char=Char].m` at [T=Int]", "[T=Char]", "m"),
        (
            "incoherent_core_shape",
            "use_it",
            "`W[Char=Char].m` at [T=Point[Char=Char]]",
            "[T=Char]",
            "m",
        ),
        ("incoherent_val_fn", "get_seed", "`seed` at [T=Int]", "[T=Char]", "seed"),
        ("incoherent_fn", "get_made", "`make` at [T=Int]", "[T=Char]", "make"),
    ]:
        entry = wasi(name)
        # The oracle's message names neither instantiation, where the
        # Python bootstrap said "calling `{callee}` needs {needs}, but the
        # one in the context here was provided at {made}".
        _ = made
        needs = needs.split(" at ", 1)[0]
        oracle_says(entry, f"calling `{callee}` needs {needs} {INCOHERENT}")
        out = rejection(entry, PRELUDE)
        assert out.startswith(f"!R {entry} {item}\n"), f"{out}"


# A val assumed at the ambient `T` does not serve a call made after `bind
# T = Int` in the same body. Both front ends reject it. The self-hosted
# compiler used to accept it and read a `Char` as an `Int`: the rebinding
# is the same width, so its back end's `types::fits` on the
# specialization saw nothing wrong.


def test_a_provision_from_before_a_rebind_is_refused():
    entry = wasi("incoherent_shadowed")
    # The oracle's message names neither instantiation, where the Python
    # bootstrap said "needs `seed` at [T=Int], but the one in the context
    # here was provided at [T as assumed, before a later `bind T`]".
    oracle_says(entry, f"calling `get_seed` needs `seed` {INCOHERENT}")
    assert rejection(entry, PRELUDE) == f"!R {entry} seed\n"


# AUDIT-CHANGES #4 (E25): a selected provision has the type, or the
# signature, its callee reads it at. `v` and `g` mention `T` without
# requiring it, so their provisions are at no instantiation; provided
# while `T` meant `Char`, they are read by `f` after `T` was rebound to
# `Bool`. The self-hosted compiler refuses both (`e_incoherent`, `R`),
# as the semantics and the oracle do; it used to print `t`, reading a
# `Char` as a `Bool`.


def test_a_provision_read_at_another_type_is_refused():
    for name, item in [("stale_provision", "v"), ("stale_provision_fn", "g")]:
        entry = wasi(name)
        assert rejection(entry, PRELUDE) == f"!R {entry} {item}\n"
        out = oracle(entry, [])
        assert out.code == ORACLE_REJECTED, f"the oracle on {entry}: {out.stderr}"


# The same check reads the callee's types through the call's principal
# θ (#50): `f`'s `Y`, which `demo` leaves unbound, is the `X` it merges
# with, so `C.w`'s provision at `Foo = Char` has the type `f` reads.


def test_a_provision_is_read_through_the_principal_instantiation():
    entry = wasi("partially_bound_merge")
    assert oracle_run(entry, []) == "k\n"
    assert run_moss(entry, PRELUDE) == "k\n"


# An item's whole declared context must be supplied by its prefix.
def test_assumptions_check_their_entire_context_in_order():
    def check(text, accepted):
        entry = source(text)
        native = compile_source(entry, "")
        reference = oracle_command(["--prelude", "", "--elab-only", entry])
        assert native.is_module() == accepted, f"{text}: {native!r}"
        assert reference.code == (0 if accepted else ORACLE_REJECTED), f"{text}: {reference.stderr}"

    for params in ("", "x: T"):
        for items, accepted in (("T, seed, f", True), ("f, seed, T", False), ("T, f, seed", False)):
            check(
                f"type T; fn seed(); assume T, seed {{ fn f({params}); }} "
                f"assume {items} {{ fn g() {{}} }} fn main() {{}}",
                accepted,
            )
    for inputs, accepted in (("T, seed, f", True), ("T, f, seed", False)):
        check(
            "type T; fn seed(); assume T, seed { fn f(); } fn h(); "
            f"functor F: {inputs} -> h; fn main() {{}}",
            accepted,
        )
    for items, accepted in (("A, B, A=B, NeedF", True), ("A, B, NeedF, A=B", False)):
        check(
            "type A; type B; assume A=B { fn f(); } context NeedF=f; "
            f"assume {items} {{ fn g() {{}} }} fn main() {{}}",
            accepted,
        )


def test_concrete_declarations_are_not_context_inputs():
    for declaration in ["tag C;", "tag C(());", "type C=();", "val C: () = ();"]:
        for input in ["assume C { fn f() {} }", "context Inputs=C;", "functor F: C -> T;"]:
            entry = source(f"type T; {declaration} {input} fn main() {{}}")
            assert not (compile_source(entry, "").is_module()), f"{declaration} {input}"
            reference = oracle_command(["--prelude", "", "--elab-only", entry])
            assert reference.code == ORACLE_REJECTED, f"{declaration} {input}: {reference.stderr}"
            assert "is concrete and cannot be listed as a context input" in reference.stderr
    entry = source("type T; tag C; type Alias=C; assume T=Alias { fn f(): T { C } } fn main() {}")
    assert compile_source(entry, "").is_module()
    reference = oracle_command(["--prelude", "", "--elab-only", entry])
    assert reference.code == 0, f"{reference.stderr}"


# The preceding `T` instantiates `seed` even though its signature omits it.


def test_an_assumed_provision_uses_the_preceding_type():
    entry = wasi("coherent_assume_order")
    assert oracle_run(entry, []) == "c\n"
    assert run_moss(entry, PRELUDE) == "c\n"


# The control: provide again at the new `T`, and both run it.


def test_reproviding_after_a_rebind_is_coherent():
    entry = wasi("coherent_rebound")
    assert oracle_run(entry, []) == "ab\n"
    assert run_moss(entry, PRELUDE) == "ab\n"


# D20: assuming `W.m`, or `seed` and `make`, whose signatures mention
# `T`, is legal only where `T` is assumed or bound. The oracle says the
# type is not available; the self-hosted compiler reports `e_absent`
# (`P`), naming the type.


def test_an_assume_needs_the_types_its_item_mentions():
    for name in ["assume_without_type", "assume_without_type_val_fn"]:
        entry = wasi(name)
        oracle_says(entry, "type `T` is not available in the context here")
        out = rejection(entry, PRELUDE)
        assert out.startswith(f"!P {entry} T\n"), f"{out}"


# D71: `tag A { b: B, t: T }; tag B { a: A };` — both are parameterized
# by `T`, whichever is computed first, so `get_b(): B` mentions `T` and
# assuming it without `T` is D20's error. Each compiler once answered by
# whichever declaration it reached first, and accepted the A-first
# program.


def test_relevance_is_a_least_fixpoint():
    for order in ["a_first", "b_first"]:
        entry = wasi(f"relevance_{order}")
        oracle_says(entry, "type `T` (required by `B`) is not available in the context here")
        out = rejection(entry, PRELUDE)
        assert out.startswith(f"!P {entry} T\n"), f"{out}"


# D71: brackets bind only a nominal's relevant parameters. A unit has
# none (`Stop[Int=Char]`), and `W[T=Char]` binds a symbol `W`'s payload
# does not mention. The oracle names the application; the self-hosted
# compiler reports `e_irrelevant` (`Q`), named by the symbol bound. The
# Python bootstrap used to accept both, ignoring the binding, and the
# self-hosted compiler the second.


def test_applying_an_irrelevant_parameter():
    for name, nominal, bound, params in [
        ("irrelevant_unit", "Stop", "Int", "none"),
        ("irrelevant_tag", "W", "T", "`Char`"),
    ]:
        entry = wasi(name)
        oracle_says(
            entry,
            f"application of `{nominal}` binds `{bound}`, which is not one of its parameters (the type symbols its payload mentions: {params}; D71)",
        )
        assert rejection(entry, PRELUDE) == f"!Q {entry} {bound}\n"


# `B[T=Int]` is legal: `T` reaches `B` through `A`, and is relevant to
# `B` even when `A` is computed first. Both accept it (and the
# self-hosted compiler now reads the brackets on a type bind's right-hand
# side rather than dropping them).


def test_applying_a_relevant_parameter_found_by_fixpoint():
    entry = wasi("relevance_applied")
    assert oracle_run(entry, []) == "y\n"
    assert run_moss(entry, PRELUDE) == "y\n"


# D43, the designer's example: a context with `A.gimme[Foo=B]` and
# `A.gimme[Foo=C]` (and a val at both) merges the two bindings, so `B` and
# `C` are one type inside it and may be used interchangeably. The
# self-hosted compiler used to keep the first of the two assumptions, and
# found `C` where `B` was expected. Merging does not depend on the order
# of the assume list (`merge_order`, which the Python bootstrap used to
# reject), and is done in a functor's argument signature too
# (`merge_functor`).


def test_a_merge_makes_two_types_one():
    for name, expected in [("merge", "qbq\n"), ("merge_order", "k\n"), ("merge_functor", "tt\n")]:
        entry = wasi(name)
        assert oracle_run(entry, []) == expected, f"{entry}"
        assert run_moss(entry, PRELUDE) == expected, f"{entry}"


# D43: a merge that would identify two distinct concrete types —
# `A.gimme[Foo=U1]` with `A.gimme[Foo=U2]` — is an error where the context
# is assumed, though nothing calls the function assuming it, or in a
# functor's argument signature. The self-hosted compiler reports
# `e_merge` (`Y`), named by the item whose bindings would not merge; it
# used to keep the first and accept the program.


def test_an_inconsistent_merge():
    for name in ["merge_inconsistent", "merge_functor_inconsistent"]:
        entry = wasi(name)
        oracle_says(entry, "cannot merge `U1` with `U2` for `.gimme` (D43)")
        assert rejection(entry, PRELUDE) == f"!Y {entry} gimme\n"


# D43: a merged context requires the merged types to be one, so a
# function assuming one may be called only where they are: not with `B`
# and `C` bound to `Char` and `Int` (`merge_caller`, and
# `merge_methods_caller`, where the merge also makes `B.m` and `C.m` one
# key), and not from a function generic over `B` and `C` apart, though
# every specialization of it binds both to `Char`
# (`merge_caller_generic`, D80). The self-hosted compiler reports
# `e_merge` (`Y`), named by the merged type.


def test_a_caller_binds_merged_types_alike():
    for name, here in [
        ("merge_caller", "`Char` and `Int`"),
        ("merge_methods_caller", "`Char` and `Int`"),
        ("merge_caller_generic", "`B` and `C`"),
    ]:
        entry = wasi(name)
        oracle_says(
            entry,
            f"needs `B` and `C` to be one type, which its context merged (D43), but here they are {here}",
        )
        assert rejection(entry, PRELUDE) == f"!Y {entry} B\n"


# D18: a nominal type's identity is its declaration and its static
# bindings, so `Box[Elem=I32]` substitutes into the payload. The oracle
# and the self-hosted compiler run tests/wasi/applied.moss.


def test_applying_a_type():
    entry = wasi("applied")
    assert oracle_run(entry, []) == "A\n"
    assert run_moss(entry, "") == "A\n"


# D89: a bracket application on a function call runs the call under
# those binds, its arguments evaluated first, in the caller's terms.
# tests/wasi/call_brackets.moss constructs a `Stack` that holds no
# elements, so that only the bracket fixes `Item` (`stack[Item=Float]()`),
# and passes `grown[Item=Float]` an argument checked with the caller's
# `Item = Char`, which would not fit under the bracket's `Item = Float`.


def test_a_call_under_brackets():
    entry = wasi("call_brackets")
    assert oracle_run(entry, []) == "2\n21\n"
    assert run_moss(entry, PRELUDE) == "2\n21\n"


# D89, D22: each binding of a call's brackets names a type the callee
# requires; `stack[Elem=Float]()` binds `Elem`, which `stack` does not
# require. The self-hosted compiler reports `e_irrelevant` (`Q`), named
# by the symbol. With no bracket binding `Item`, which `go` does not
# have either, the call also lacks it in force (rule B, `e_absent`).


def test_a_call_bracket_names_a_requirement():
    entry = wasi("call_brackets_unrequired")
    oracle_says(
        entry,
        "a bracket application on a call of `stack` binds `Elem`, which is not a type `stack` requires (D22, D89)",
    )
    assert rejection(entry, PRELUDE) == f"!Q {entry} Elem\n!P {entry} Item\n"


# A record whose fields are typed by an abstract `T` is laid out per
# instantiation, so a method on it compiles at `T = Char` and at `T =
# Point` alike.


def test_a_record_at_a_wider_instantiation():
    assert run_moss(wasi("widened"), PRELUDE) == "ba\nefd\n"


# D62: a provision serves only the instantiation it was made at, so two
# of one method at two instantiations coexist, each compiled for its own
# type — and a receiver at a third finds none.


def test_provisions_are_keyed_by_instantiation():
    assert run_moss(wasi("provisions"), PRELUDE) == "ac\n"
    report = rejection(wasi("mismatch"), PRELUDE)
    assert report.startswith("?K "), f"{report}"
    assert "peek" in report, f"{report}"


# D70: applying a defined functor does not capture its caller's locals;
# a same-name local inside the lambda belongs to the lambda itself.


def test_anonymous_function_in_functor_shadows_caller_local():
    entry = wasi("lambda_functor_shadow")
    assert oracle_run(entry, []) == ""
    assert run_moss(entry, PRELUDE) == ""


# D70: a lambda body may return one member of its declared result union.


def test_anonymous_function_result_can_be_subtype():
    entry = wasi("lambda_result_subtype")
    assert oracle_run(entry, []) == ""
    assert run_moss(entry, PRELUDE) == ""


# A method bind reads bracket arguments in its caller's type context.


def test_method_bind_brackets_read_caller_types():
    entry = wasi("method_bind_caller_brackets")
    assert oracle_run(entry, []) == ""
    assert run_moss(entry, PRELUDE) == ""


# D15: a type symbol may be a union member where the context the union
# is written in binds it to a nominal type (`bind T = U; let x: T | V`).
# Unbound, the same `let` is refused, and so is a member whose meaning
# shares a head with another (tests/errors/union_abstract_member_let.moss,
# union_bound_member_clash.moss and union_latent_overlap.moss, which the
# self-hosted compiler used to run, reading an I32 as a Colour).


def test_a_union_member_bound_to_a_nominal():
    entry = wasi("union_bound_member")
    assert run_module_code(build_with(entry, ""), [], 2) == ""
    assert oracle(entry, []).code == 2


# Copying a functor to itself preserves an untouched latent interface;
# an inhabited interface still applies with the original fixed reading.


def test_functor_identity_copy_preserves_latent_interfaces():
    for name in [
        "functor_latent_identity_copy",
        "functor_latent_identity_reexport",
        "functor_identity_copy_applied",
    ]:
        entry = wasi(name)
        assert oracle_run(entry, []) == "", f"{name}"
        assert run_moss(entry, PRELUDE) == "", f"{name}"
    stale = wasi("functor_identity_copy_stale")
    rejects_with(stale, PRELUDE)
    assert "fixed" in oracle_rejects(stale)


# D90: a type equation as a context or assume item requires the
# equation and merges its two sides, as D43 merges two bindings of one
# key. merged_int_size.moss is the convenience context merging `Int` and
# `Size` (`context IntIsSize = Int = Size;`), with library code written
# against the separate types and called from inside the merged context;
# merged_size_int.moss is the same program with the equation the other
# way round, in the assume list itself. Both spellings print the same, on
# both front ends. eq_link_tx.moss and eq_link_xt.moss put the equation,
# in either spelling, in the argument context of a linked functor body,
# and both front ends run both: the spellings give one merge class
# (PublicEquation.eq_swap_classes), whichever member the merge keeps. A clash
# (two structures in one class), a caller where the two sides differ,
# and a caller without the sides in force are refused (tests/errors).


def test_type_equations():
    for name in ["merged_int_size", "merged_size_int"]:
        entry = wasi(name)
        assert oracle_run(entry, []) == "21\n", f"{entry}"
        assert run_moss(entry, PRELUDE) == "21\n", f"{entry}"
    for name in ["eq_link_tx", "eq_link_xt", "eq_caller_same"]:
        entry = wasi(name)
        assert run_moss(entry, PRELUDE) == "", f"{entry}"
        assert oracle_run(entry, []) == "", f"{entry}"
    for name in [
        "eq_clash",
        "eq_clash_structure",
        "eq_caller_differs",
        "eq_caller_unbound",
        "eq_result",
    ]:
        entry = errors(name)
        assert not (compile_source(entry, PRELUDE).is_module()), f"{entry}"
        oracle_rejects(entry)
