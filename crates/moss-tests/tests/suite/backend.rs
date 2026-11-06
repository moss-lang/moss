//! The self-hosted compiler, end to end: src/ reads a Moss program off
//! disk, resolves it, and writes a WASI module that runs.
//!
//! The language it covers started as the primitive context of D52 —
//! `Wasm` instructions, `Wasi` imports, `Bool` for `if` to eliminate,
//! plain functions, `let`/`var`, `if`/`else`, `while`, `loop`/`break` and
//! `return` — which is what tests/wasi/raw.moss and tests/wasi/prim.moss
//! are written in, and is now the whole language over `Std`. Programs are
//! compiled with the prelude or without it (`""`), as each test says, and
//! where a program is also run or refused by the oracle, the two are held
//! to the same behaviour and, for a refusal, the oracle to the Python
//! bootstrap's message (which it reproduces).
//!
//! Ported from `TestSelfHostedBackEnd` in the Python bootstrap's
//! `tests/test_run.py`; the Python interpreter and the Python Wasm back
//! end, which were cross-checks there, are both the oracle here.

use moss_tests::{
    build_with, compile, oracle, oracle_rejects, oracle_run, rejects_with,
    run_module, run_module_code, starts_with_rec_group, wasm_file, wasm_text, wasmtime, Out,
    ORACLE_UNSUPPORTED, TRAP,
};

const PRELUDE: &str = "lib/prelude.moss";

/// The oracle's message for a functor bound to an unbracketed abstract one
/// that binds a type in its result (`WasiFloats`' `bind Int`, under `Fast`).
const BIND_INT_CHANGES_RESULT: &str = "`bind Int` in a functor bound to an abstract one changes \
     a type its result mentions; the fixed set is not widened per binding (D84 3d)";

/// The oracle's message for a `Float` that `bind Fast;` made leaving its
/// block; the Python bootstrap said "this block yields `Float@Fast`, the
/// type `bind Fast;` made in it: a value of that type cannot leave the
/// block (D78)".
const ESCAPES_FAST: &str = "this block yields `Float@Fast`, which mentions `Float@Fast`, \
     a type `bind F;` made in it: a value of that type cannot leave the block (D78)";

/// The tail of the oracle's message for an incoherent provision.
const INCOHERENT: &str = "at another instantiation than the one in the context here was \
     provided at (a provision serves only the instantiation it was made at; D62)";

fn wasi(name: &str) -> String {
    format!("tests/wasi/{name}.moss")
}

/// What the self-hosted compiler writes for `entry`: a module or a report.
fn compile_with_moss(entry: &str, prelude: &str) -> Vec<u8> {
    compile(entry, prelude).stdout
}

/// The self-hosted compiler's module for `entry`, run, exiting with 0.
fn run_moss(entry: &str, prelude: &str) -> String {
    run_module(&build_with(entry, prelude), &[])
}

/// A module run to its end, whatever its exit status.
fn wasmtime_out(module: &[u8]) -> Out {
    wasmtime(&wasm_file(module), None, &[])
}

/// The self-hosted compiler's report for `entry`, which it must refuse.
fn report(entry: &str, prelude: &str) -> String {
    rejects_with(entry, prelude)
}

/// The oracle refuses `entry` with `message` among what it says.
fn oracle_says(entry: &str, message: &str) {
    let said = oracle_rejects(entry);
    assert!(said.contains(message), "{entry}: the oracle said {said:?}, not {message:?}");
}

/// The oracle reports a construct of `entry` it does not model.
fn oracle_unsupported(entry: &str) {
    let out = oracle(entry, &[]);
    assert_eq!(out.code, ORACLE_UNSUPPORTED, "{entry}: {}", out.stderr);
}


/// The oracle traps on `entry`, after printing `stdout`, naming the trap.
fn oracle_traps(entry: &str, stdout: &str, trap: &str) {
    let out = oracle(entry, &[]);
    assert_eq!((out.code, out.text().as_str()), (TRAP, stdout), "{entry}: {}", out.stderr);
    assert!(out.stderr.contains(trap), "{entry}: {}", out.stderr);
}

/// A report template's `{}` (or `{0}`) is the entry.
fn fill(template: &str, entry: &str) -> String {
    template.replace("{0}", entry).replace("{}", entry)
}

/// Each `(name, message, report)`: the oracle refuses `tests/wasi/NAME`
/// saying `message`, and the self-hosted compiler (with the prelude)
/// writes a report that is `report` (`exact`) or starts with it.
fn refused(cases: &[(&str, &str, &str)], exact: bool) {
    for (name, message, template) in cases {
        let entry = wasi(name);
        oracle_says(&entry, message);
        let out = report(&entry, PRELUDE);
        let expected = fill(template, &entry);
        if exact {
            assert_eq!(out, expected, "{entry}");
        } else {
            assert!(out.starts_with(&expected), "{entry}: {out:?}");
        }
    }
}

// What tests/wasi/records.moss prints, one property per line.
const RECORDS_OUT: &str = "ab\nba\nbaq\ncdez\ncd\ngf\nho\n";

// What tests/wasi/nested.moss prints.
const NESTED_OUT: &str = "bca\nfda\nghi\n";

// What tests/wasi/float.moss prints, one property per line; the comments
// there say why each is what it is.
const FLOAT_OUT: &str = "1414213562\n1414213504\n1414213562\n333333343\n333333333\n0100\n\
     0\n2147483647\n-2147483648\n0\n2147483647\n-3\n\
     2\n4\n-3\n-2\n-2\n-1\n1\n2\n\
     1065353216\n1\n1073741824\n1568669697\n-1\n\
     1\n333333344\n0\n2\n";

// What tests/wasi/exponents.moss prints: the same report at f64, then f32.
// `1e39` is inf at f32 and `1e-46` zero.
const EXPONENTS_OUT: &str = "yyyyyyyyy\n\
     1500 125 1 250 -42 100000 602 \n\
     n nn yy\n\
     yyyyyyyyy\n\
     1500 125 1 250 -42 100000 602 \n\
     n yy yy\n";

// What tests/wasi/shadow.moss prints: f64, f32, f64, f32, f64.
const SHADOW_OUT: &str = "333333333\n333333344\n333333333\n333333344\n333333333\n";

// What tests/wasi/literals.moss prints: the same report at f64, then f32.
// A third to twenty-one places is not f32's nearest third: its digits were
// summed in f32 and rounded on the way (D68).
const LITERALS_OUT: &str = "0 7 10 100 1000 1234567 2147483647 -42 3 0 \n\
     3 -2 1 0 250 1 16777215 \n\
     3141593 333333333 ny\n\
     314159265 333333333 \n\
     0 7 10 100 1000 1234567 2147483647 -42 3 0 \n\
     3 -2 1 0 250 1 16777215 \n\
     3141593 333333344 yn\n\
     314159264 333333248 \n";

// What tests/wasi/lists.moss prints: an IntList beside a FloatList at f64,
// then the same at f32. A thousand 0.1s sum to 100 at f64 and to
// 99.999048 at f32; a third differs in the eighth digit.
const LISTS_OUT: &str = "1000 1000 100000000 332833500 \n\
     333333333 1 1 -7 251001 \n\
     1000 1000 99999048 332833500 \n\
     333333344 1 1 -7 251001 \n";

// What tests/wasi/overflow.moss prints before it traps: an oversized
// literal under wrapping i32 arithmetic, then one that fits, under it and
// under arithmetic that traps on overflow.
const OVERFLOW_OUT: &str = "1\n2147483647\n2147483647\n";

// D75: what tests/wasi/gc_tree.moss prints — the thesis's expression, a
// full tree of 2^10 ones and its depth, and the expression read through a
// contextual val, doubled — and what tests/wasi/gc_ast.moss prints, once
// over a pool of indices and once over the GC heap.
const GC_TREE_OUT: &str = "-10\n1024\n11\n-20\n";
const GC_AST_OUT: &str = "-10\n0\n4096\n-10\n0\n4096\n";
// ...and what tests/wasi/gc_two.moss prints, with `Heap` applied at two
// types in one scope.
const GC_TWO_OUT: &str = "3\n12\n7\n1\n";

// D81: what tests/wasi/gc_array.moss prints — an array of `Int`s written,
// filled and copied, read as digits, and its last element; `Point`s over
// two lanes; `Tagged`s over three, one of references; and an array of
// references, summed, and its last — and what tests/wasi/gc_list.moss
// prints, the same three lines from a packed list and from a list built
// on GC arrays, then a list of references.
const GC_ARRAY_OUT: &str = "8\n15071117\n7\n512\n4\n0\n-1\n47\n3\n46\n1\n";
const GC_LIST_OUT: &str = "21\n3475\n5\n21\n3475\n5\n15\n3\n";

// D78: what tests/wasi/functor_fast.moss prints — √2 and √3 scaled by a
// billion, at f64 and then at f32.
const FUNCTOR_FAST_OUT: &str = "1414213562\n1732050808\n1414213504\n1732050944\n";

// D78: what tests/wasi/functor_trace.moss prints — the notes of a traced
// subtree and the whole value, under each of two tracers.
const FUNCTOR_TRACE_OUT: &str = "2,7,4,3,3,6,\n7\n[2][7][4][3][3][6]\n7\n";

// What tests/wasi/ops.moss prints: D68's operators on `Int` and `Bool`.
const OPS_OUT: &str = "19 -7 3 1 -7 2 23 35 21 5 1 -1 \n\
     ynynnyny y\n\
     2 15 13 1024 128 -5 \n\
     nnynyyyyn\n";

// What tests/wasi/dual.moss prints, times a million: f(2), f'(2) and
// f''(2) for f(x) = x^3 - 2x - 5; Newton's iterates from 2 toward its
// root, 2.0945514815...; and d/dx (x * d/dy (x + y)) at 1.
const DUAL_OUT: &str = "-1000000\n10000000\n12000000\n\
     2100000\n2094568\n2094551\n2094551\n\
     1000000\n";

// What tests/wasi/dual_std.moss prints, times a million: f(2), f'(2) and
// f''(2) for f(x) = sqrt(x + x^2 + x^3) — sqrt 14 = 3.7416573868,
// 17 / (2 sqrt 14) = 2.2717205563 and 103 / (56 sqrt 14) = 0.4915697842.
const DUAL_STD_OUT: &str = "3741657\n2271721\n491570\n";

// What tests/wasi/newton.moss prints: the same report at f64, then at f32.
const NEWTON_OUT: &str = "1500000000\n1416666667\n1414213562\n1414213562\n0\n333333333\n\
     -100000000\n16777217\n24-2-2\n2147483647\nnynnyy0\n\
     1500000000\n1416666752\n1414213504\n1414213504\n0\n333333344\n\
     -100000000\n16777216\n24-2-2\n2147483647\nnynnyy0\n";

// What tests/wasi/lambda.moss prints: each letter is named beside the
// line that prints it.
const LAMBDA_OUT: &str = "ABxCypqmnBwgdD4\n";

// What tests/wasi/size_rebind.moss and tests/wasi/size_std.moss print.
const SIZE_REBIND_OUT: &str = "1705032704\n-294967296\n-2\n";
const SIZE_STD_OUT: &str = "-294967296\n4\n2147483647\n4\n";

#[test]
fn raw_wasi_program() {
    let module = compile_with_moss(&wasi("raw"), "");
    assert_eq!(&module[..8], b"\0asm\x01\0\0\0");
    // proc_exit(3).
    assert_eq!(run_module_code(&module, &[], 3), "A\n");
}

#[test]
fn control_flow_and_i64() {
    assert_eq!(run_moss(&wasi("prim"), ""), "ABKDKJGG\n");
}

/// A function is compiled in the module that declared it: its names
/// resolve there, not where it was called from.
#[test]
fn calls_across_modules() {
    assert_eq!(run_moss(&wasi("across"), ""), "AC\n");
}

/// D58 and D59's value model: a nominal tag over a scalar is that scalar,
/// a union of units is one scalar of discriminant, and `match` is the only
/// thing that has to tell them apart. Nothing here allocates.
#[test]
fn tags_units_and_match() {
    assert_eq!(run_moss(&wasi("tags"), ""), "EBACEAB\n");
}

/// D2: a val is the only context that exists at runtime, so a function
/// that assumes one is implicitly parameterized by it and that
/// parameterization is a real Wasm parameter. Covers a need threaded
/// through a frame that does not use it, a `bind` shadowed inside a block,
/// and one rebound at the same level.
#[test]
fn contextual_vals() {
    assert_eq!(run_moss(&wasi("ctx"), ""), "CEECI\n");
}

/// The whole of what a `bind` can do: a type, a val, a plain function and
/// a detached method at a receiver, all provided at once by applying a
/// functor (D55). Code written against the signature alone still compiles
/// to a direct call (D2), which means the callee is compiled once per
/// environment that reaches it, and the keys are compared after
/// canonicalization through the bindings in force (D43) — the callee knows
/// its receiver by a symbol the caller has since bound.
#[test]
fn functor_and_the_static_context() {
    assert_eq!(run_moss(&wasi("functor"), ""), "ABD\n");
}

/// The rest of D59's value model. `Read | Eof` is a discriminant beside
/// room for the payload, which needs three things at once: a member
/// injecting where it enters the union, a `match` that reads the
/// discriminant and then the payload, and a function returning two
/// scalars — a multi-result type. This is the shape src/lex.moss's
/// `peek()` returns.
#[test]
fn a_union_wider_than_one_scalar() {
    assert_eq!(run_moss(&wasi("wide"), ""), "ABCD\n");
}

/// `put_bytes`, compiled by the compiler written in Moss — which is also
/// how it writes its own output, so this is the one primitive whose cost
/// the fixpoint test pays directly.
#[test]
fn a_bulk_write() {
    assert_eq!(run_moss(&wasi("bulk"), ""), "hi!\n");
}

/// A receiver an application binds. A context item may key a detached
/// method on an *abstract* type — `context IsList = L.get;` — which
/// `IsList[T=Int, L=IntArr]` then fixes, and the provision has to land at
/// `IntArr` rather than at `L` or no call finds it. That is D51's generic
/// container, and the same mechanism the library's own
/// `String.get[Elem=Char]` uses with the receiver written concretely, so
/// this is the case where the brackets actually have to be read.
///
/// Two instantiations coexist here, which is the point: the keys are
/// (IntArr, get) and (CharArr, get), distinct atoms, so nothing merges.
#[test]
fn a_generic_container() {
    assert_eq!(run_moss(&wasi("generic"), ""), "hiDCC\n");
}

/// D26: a bound provider runs with the vals in force where it was bound.
/// Its captured values travel under keys of their own, so a rebind between
/// the bind and the call — in the calling frame, a frame above it, or
/// inside a provider that captured a bind of its own — cannot reach it,
/// through a plain fn or a detached method.
#[test]
fn a_fn_bind_captures_at_the_bind_site() {
    assert_eq!(run_moss(&wasi("capture"), ""), "CCCCE\n");
}

/// D65: a bare block is an expression. The val and fn binds made inside it
/// are gone at its end, a `let` inside it shadows only until then while an
/// assignment to an outer `var` stays, and its value is its tail — as a
/// `let` initializer and as an argument.
#[test]
fn a_bare_block_scopes_its_binds_and_lets() {
    assert_eq!(run_moss(&wasi("block"), ""), "EDCBGAIEE\n");
}

/// D66: `bind f = g;` with `g` abstract is a bind of a forwarder to `g`,
/// so it captures `g` as provided at the bind — at a type a bind fixed,
/// inside a functor written against the signature alone, and for a
/// primitive. Rebinding `g` afterwards, in the same frame or deeper in the
/// call tree, does not reach `f`.
#[test]
fn a_fn_bound_to_a_contextual_fn() {
    assert_eq!(run_moss(&wasi("forward"), ""), "CCEDEC\n");
}

/// D54: a plain function may provide a method, with the method's declared
/// parameters — the receiver excluded — so it cannot see what the method
/// was called on, though the call still evaluates the receiver. An
/// attached provider takes it as `this`. The self-hosted compiler used to
/// pass the receiver to a plain provider anyway (invalid Wasm here; a
/// plain fn written to take it was accepted); it and the oracle now run it
/// alike.
#[test]
fn a_plain_fn_provides_a_method() {
    let entry = wasi("plain_provider");
    assert_eq!(oracle_run(&entry, &[]), "abrcd\n");
    assert_eq!(run_moss(&entry, PRELUDE), "abrcd\n");
}

/// D54, the other half: a plain provider written to take the receiver as a
/// first parameter is a signature mismatch (the oracle's message;
/// `e_signature`, `S`, named by the method, in the self-hosted compiler,
/// which used to accept it), and `this` in one is an error (`e_this`, `T`).
#[test]
fn a_plain_fn_provider_cannot_take_or_see_the_receiver() {
    refused(
        &[
            (
                "plain_provider_receiver",
                "method bind signature mismatch: provider `second` has \
                 (Box[Char=Char]) -> Char, need () -> Char; a plain function \
                 providing a method takes the method's parameters, which \
                 exclude the receiver (D54)",
                "!S {} m\n",
            ),
            ("plain_provider_this", "`this` is only legal inside method bodies", "!T {}\n"),
        ],
        true,
    );
}

/// D70: `(params) => body` as a fn or method bind's right side —
/// unannotated and annotated, capturing a `let` and a `var` by value (an
/// assignment after the bind is not seen), capturing context items as they
/// are at the bind (a later rebind of a fn or a val is not seen), in a
/// functor's body, as a method provider seeing the receiver as `this`
/// through a wrapper and with none at all, with a block body, one inside
/// another, and one yielding a float (whose body is a block yielding an
/// f64). The oracle and the self-hosted compiler agree.
#[test]
fn anonymous_functions_at_binds() {
    let entry = wasi("lambda");
    assert_eq!(oracle_run(&entry, &[]), LAMBDA_OUT);
    assert_eq!(run_moss(&entry, PRELUDE), LAMBDA_OUT);
}

/// D70's errors in both front ends: a parameter count or an annotation
/// that is not the bound declaration's (`e_signature`, `S`, named by the
/// bound fn), an assignment to a captured local and a lambda anywhere but
/// a bind (`e_lambda`, `U`), `this` in one bound to a fn (`e_this`, `T`),
/// and a lambda calling the fn it is bound to, which means what it meant
/// at the bind — here nothing (`c_unbound`, `?Q`).
#[test]
fn anonymous_function_errors() {
    refused(
        &[
            (
                "lambda_arity",
                "the anonymous function bound to `step` takes 2 parameter(s), \
                 but `step` takes 1 (D70)",
                "!S {} step\n",
            ),
            (
                "lambda_annotation",
                "parameter `c` of the anonymous function bound to `step` is \
                 annotated `Int`, but `step` declares `Char` there (D70)",
                "!S {} step\n",
            ),
            (
                "lambda_assign",
                "`seen` is a local captured by an anonymous function, which \
                 captures it by value and may not assign it (D70)",
                "!U {} seen\n",
            ),
            (
                "lambda_outside",
                "an anonymous function may only be the right side of a fn or \
                 method bind (D70)",
                "!U {}\n",
            ),
            (
                "lambda_this",
                "`this` is only legal inside method bodies; an anonymous \
                 function bound to a fn has no receiver",
                "!T {}\n",
            ),
            ("lambda_recursive", "`step` is not available in the context here", "?Q {} step step\n"),
        ],
        false,
    );
}

/// D67: f32 and f64 are scalars like i32 and i64 — in locals, parameters,
/// results, record fields and memory, through the whole of `Wasm`'s float
/// half, f32 rounding to single precision.
#[test]
fn float_scalars() {
    assert_eq!(run_moss(&wasi("float"), ""), FLOAT_OUT);
}

/// D68's operators, compiled as the calls of `num.moss`'s methods they
/// are, at `Int` and at `Bool`.
#[test]
fn operators() {
    assert_eq!(run_moss(&wasi("ops"), PRELUDE), OPS_OUT);
}

/// D68's literals, at f64 and — in a block that binds `WasiFloats32` — at
/// f32, from one `report` function.
#[test]
fn literals() {
    assert_eq!(run_moss(&wasi("literals"), PRELUDE), LITERALS_OUT);
}

/// D68's exponent notation, at f64 and at f32.
#[test]
fn exponents() {
    assert_eq!(run_moss(&wasi("exponents"), PRELUDE), EXPONENTS_OUT);
}

/// D69's packed lists: an `IntList` and a `FloatList` side by side, at f64
/// and — in a block binding `WasiFloats32` — at f32, from one `report`
/// function and one list implementation.
#[test]
fn float_lists() {
    assert_eq!(run_moss(&wasi("lists"), PRELUDE), LISTS_OUT);
}

/// tests/wasi/overflow.moss in the self-hosted compiler and the oracle:
/// the wrapped literal, the checked one that fits, and then a trap
/// (wasmtime's exit status for one is 134, as is the oracle's) where the
/// checked one does not.
#[test]
fn an_oversized_literal_does_what_the_arithmetic_does() {
    let entry = wasi("overflow");
    oracle_traps(&entry, OVERFLOW_OUT, "[moss_ref: trap unreachable]");
    let out = wasmtime_out(&build_with(&entry, PRELUDE));
    assert_eq!((out.code, out.text().as_str()), (TRAP, OVERFLOW_OUT));
    assert!(out.stderr.contains("unreachable"), "{}", out.stderr);
}

/// D68: `c + c` where nothing provides `Char.add` is reported exactly as
/// `c.add(c)` is — `e_method` naming `add` — by the self-hosted compiler,
/// and refused by the oracle.
#[test]
fn an_operator_the_context_lacks_is_the_method_call_error() {
    let entry = wasi("unprovided");
    oracle_rejects(&entry);
    let report = report(&entry, PRELUDE);
    assert!(report.contains("?K tests/wasi/unprovided.moss by_operator add\n"), "{report}");
    assert!(report.contains("?K tests/wasi/unprovided.moss by_method add\n"), "{report}");
}

#[test]
fn generic_code_does_not_mix_int_and_size() {
    let report = report(&wasi("size_mix"), PRELUDE);
    assert!(report.contains("?S tests/wasi/size_mix.moss total\n"), "{report}");
}

/// D67's `Float`, written against `Std` and run under two of them:
/// `WasiStd`, which provides it at f64, and one assembled the same way
/// with `WasiFloats32`. One `report` function, two compiled
/// specializations, two precisions.
#[test]
fn float() {
    assert_eq!(run_moss(&wasi("newton"), PRELUDE), NEWTON_OUT);
}

/// Two runners, one program: the self-hosted compiler's module and the
/// oracle do the same.
#[test]
fn matches_the_bootstrap_on_behaviour() {
    let cases: &[(&str, &str, &str)] = &[
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
        // An anonymous function reaching an `IntList` method in a function
        // generic over `Std`: the context's provision, not `Packed`'s
        // attached method (D70, D77).
        ("lambda_list", "2\n7\n", PRELUDE),
        ("resolution", "91122\n", PRELUDE),
        ("gc_tree", GC_TREE_OUT, PRELUDE),
        ("gc_ast", GC_AST_OUT, PRELUDE),
        ("gc_two", GC_TWO_OUT, PRELUDE),
        // D75 under D77: `Ref`'s attached `.deref` unless a detached
        // `.deref` in scope is provided at the receiver.
        ("gc_resolution", "42\n7\n42\n", PRELUDE),
        // A unit beside a reference in one union, and a trapping arm.
        ("unit_beside_ref", "42\n7\n", PRELUDE),
        // D81: arrays, `Ref.set`, a growable list built on them, and an
        // array across a `Memory` bind.
        ("gc_array", GC_ARRAY_OUT, PRELUDE),
        // Draft: `for`, and `Size` across a rebinding of `Int`.
        ("iterate", "503217abc\n", PRELUDE),
        ("size_rebind", SIZE_REBIND_OUT, PRELUDE),
        ("size_std", SIZE_STD_OUT, PRELUDE),
        ("gc_list", GC_LIST_OUT, PRELUDE),
        ("gc_mutable", "0\n2\n7\n3\n42\n", PRELUDE),
        ("gc_array_memory", "6\n6\n", PRELUDE),
        // Written against `Std`, so it needs the prelude in scope.
        ("records", RECORDS_OUT, PRELUDE),
        ("nested", NESTED_OUT, PRELUDE),
        ("newton", NEWTON_OUT, PRELUDE),
        ("shadow", SHADOW_OUT, PRELUDE),
        ("ops", OPS_OUT, PRELUDE),
        ("literals", LITERALS_OUT, PRELUDE),
        ("exponents", EXPONENTS_OUT, PRELUDE),
        ("lists", LISTS_OUT, PRELUDE),
    ];
    for (name, expected, prelude) in cases {
        let entry = wasi(name);
        assert_eq!(run_moss(&entry, prelude), *expected, "{entry}");
        if *name == "gc_array_memory" {
            // Oracle gap (D79): one linear memory, so a `Memory` bind is
            // unsupported.
            oracle_unsupported(&entry);
        } else {
            assert_eq!(oracle_run(&entry, &[]), *expected, "{entry}");
        }
    }
}

/// D75: a recursive tree through `Ref[Boxed=Expr]`, and the thesis's
/// interpreter written against an abstract `ExprRef`, run over an index
/// pool and then over the GC heap through the knot-tied `Node`, and an
/// `Expr` and a `Stmt` boxed side by side in one scope, give the same
/// output in the oracle and compiled. The compiled module has its struct
/// types as a `rec` group, builds with `struct.new`, and — since no
/// program forges a reference — contains no `ref.cast`.
#[test]
fn gc_references() {
    for (name, expected) in [("gc_tree", GC_TREE_OUT), ("gc_ast", GC_AST_OUT), ("gc_two", GC_TWO_OUT)] {
        let entry = wasi(name);
        assert_eq!(oracle_run(&entry, &[]), expected, "{entry}");
        let module = build_with(&entry, PRELUDE);
        assert!(starts_with_rec_group(&module), "{entry}");
        let text = wasm_text(&module);
        assert!(text.contains("struct.new"), "{entry}");
        assert!(text.contains("struct.get"), "{entry}");
        assert!(!text.contains("ref.cast"), "{entry}");
        assert_eq!(run_module(&module, &[]), expected, "{entry}");
    }
}

/// D81: arrays of one-scalar types (an `Int`, a reference) are one GC
/// array, and of wider ones (a record, a record of an i64, an i32 and a
/// reference) a struct of one array per scalar; a growable list built in
/// Moss on them and on `Ref.set` runs the same generic code as `Std`'s
/// packed list; `Ref.set` makes only the struct it is applied at mutable;
/// and an array is used across a `Memory` bind. The same output in the
/// oracle and compiled.
#[test]
fn gc_arrays() {
    for (name, expected) in [
        ("gc_array", GC_ARRAY_OUT),
        ("gc_list", GC_LIST_OUT),
        ("gc_mutable", "0\n2\n7\n3\n42\n"),
        ("gc_array_memory", "6\n6\n"),
    ] {
        let entry = wasi(name);
        if name == "gc_array_memory" {
            // Oracle gap (D79): one linear memory, so a `Memory` bind is
            // unsupported.
            oracle_unsupported(&entry);
        } else {
            assert_eq!(oracle_run(&entry, &[]), expected, "{entry}");
        }
        let module = build_with(&entry, PRELUDE);
        assert!(starts_with_rec_group(&module), "{entry}");
        let text = wasm_text(&module);
        let structs: Vec<&str> = text
            .lines()
            .filter(|line| line.trim().starts_with("(type") && line.contains("(struct"))
            .collect();
        if name == "gc_mutable" {
            // `Counter`'s struct is written, `Int`'s is not.
            assert_eq!(structs.iter().filter(|line| line.contains("(mut")).count(), 1, "{entry}");
            assert!(text.contains("struct.set"), "{entry}");
            assert!(!text.contains("array.new"), "{entry}");
        } else {
            assert!(text.contains("array.new"), "{entry}");
            assert!(text.contains("array.get"), "{entry}");
        }
        if name == "gc_array" {
            for op in ["array.set", "array.fill", "array.copy", "array.len"] {
                assert!(text.contains(op), "{entry}: {op}");
            }
            assert!(text.contains("(array (mut i64))"), "{entry}");
        }
        assert!(!text.contains("ref.cast"), "{entry}");
        assert_eq!(run_module(&module, &[]), expected, "{entry}");
    }
}

/// D81: an index past an array's length traps, as the instruction does;
/// and an `Array` forged from a bare `ArrayRef` at another type than it
/// was made at traps at the cast — `Char` is one i32 like `Int`, but an
/// array type of its own. Compiled, after the line before; the oracle
/// takes both traps after the same preceding output.
#[test]
fn gc_arrays_trap() {
    for (name, trap, casts) in [
        ("gc_array_bounds", "out of bounds array access", 0),
        ("gc_array_forge", "cast failure", 2),
    ] {
        let entry = wasi(name);
        if name == "gc_array_forge" {
            oracle_traps(&entry, "42\n", "[moss_ref: trap 37]");
        } else {
            // The oracle names this trap only by its code (7, an array
            // index out of bounds): its driver has no message for it.
            oracle_traps(&entry, "42\n", "[moss_ref: trap 7]");
        }
        let module = build_with(&entry, PRELUDE);
        assert_eq!(wasm_text(&module).matches("ref.cast").count(), casts, "{entry}");
        let out = wasmtime_out(&module);
        assert_eq!((out.code, out.text().as_str()), (TRAP, "42\n"), "{entry}");
        assert!(out.stderr.contains(trap), "{entry}: {}", out.stderr);
    }
}

/// D75: rebuilding a `Ref` from the bare `StructRef` it was taken apart
/// into is the one cast. At the type the struct was made at it succeeds;
/// at another it traps at the construction, compiled, before anything is
/// read.
#[test]
fn a_forged_reference_traps_where_it_is_forged() {
    let entry = wasi("gc_forge");
    oracle_traps(&entry, "3\n4\n", "[moss_ref: trap 37]");
    let module = build_with(&entry, PRELUDE);
    assert_eq!(wasm_text(&module).matches("ref.cast").count(), 2);
    let out = wasmtime_out(&module);
    assert_eq!((out.code, out.text().as_str()), (TRAP, "3\n4\n"));
    assert!(out.stderr.contains("cast failure"), "{}", out.stderr);
}

/// D78: a functor with no body is an abstract functor, anywhere — a
/// requirement that a scope provides by binding a functor to it, and that
/// `bind F;` applies through whatever it is bound to. `Trace` bound to two
/// tracers in one program and applied inside a generic function; `Fast`,
/// whose result holds the type `Float`, bound to f64 and to f32 floats for
/// one kernel; a library applying an abstract allocator whose result holds
/// the handle type, for two types, bound to a GC-heap allocator in one
/// block and to a pool of indices in another, each with fewer inputs than
/// the allocator takes; an abstract functor producing a val, evaluated at
/// each application; and D75's `gc_bodiless`, whose bodiless functor
/// outside lib/wasm.moss is now legal and bound to `Heap`. In the oracle
/// and compiled.
#[test]
fn abstract_functors() {
    let alloc = "21\n4\n14\n".repeat(2);
    for (name, expected) in [
        ("functor_trace", FUNCTOR_TRACE_OUT),
        ("functor_fast", FUNCTOR_FAST_OUT),
        ("functor_alloc", alloc.as_str()),
        // Two applications whose bindings make one concrete handle type:
        // two generations, so two `.load`s (the self-hosted compiler used
        // to refuse it rather than conflate them).
        ("functor_same_handle", "21\n4\n14\n"),
        ("functor_val", "10\n20\n30\n3\n5\n6\n0\n"),
        ("gc_bodiless", "42\n"),
    ] {
        let entry = wasi(name);
        assert_eq!(oracle_run(&entry, &[]), expected, "{entry}");
        assert_eq!(run_moss(&entry, PRELUDE), expected, "{entry}");
    }
}

/// D84's standing tests that run: a functor's own inputs read per
/// application (`fn_input`), a captured val across frames, a captured
/// memory, merged polymorphic inputs, arrays in a scratch memory, an
/// abstract functor in a result, family members on demand (rule 7), for a
/// call of the member and for a callee that needs it, and a polyfill of
/// the platform's `Heap` (O2). The oracle and the self-hosted compiler
/// agree, but for the two the oracle does not model.
#[test]
fn d84_functors_run() {
    for (name, expected) in [
        ("functor_input_per_application", "22\n"),
        ("functor_capture_val", "55\n"),
        ("functor_capture_memory", "07\n"),
        ("functor_merge_bracketed", "tt\n"),
        ("arrays_in_scratch", "6\n"),
        ("functor_in_result_ok", "5\n"),
        ("arrays_on_demand", "33\n"),
        ("family_need", "55\n55\n6\n"),
        ("functor_polyfill", "7\n"),
    ] {
        let entry = wasi(name);
        if matches!(name, "functor_capture_memory" | "arrays_in_scratch") {
            // Oracle gap (D79): one linear memory, so a `Memory` bind is
            // unsupported.
            oracle_unsupported(&entry);
        } else {
            assert_eq!(oracle_run(&entry, &[]), expected, "{entry}");
        }
        assert_eq!(run_moss(&entry, PRELUDE), expected, "{entry}");
    }
}

/// D84's `dd_arrays`: the thesis's `sd` over `Array[Item=Float]` with
/// `for`, under `DoubleDouble` and `Shadow`, which have no array lines.
/// The oracle runs it, and so does the self-hosted compiler's module.
#[test]
fn d84_arrays_follow_a_rebound_float() {
    let entry = wasi("dd_arrays");
    let expected = "0\n100000000\n1000000000\n";
    assert_eq!(oracle_run(&entry, &[]), expected);
    assert_eq!(run_moss(&entry, PRELUDE), expected);
}

/// D84's standing tests that are refused, by the oracle and the
/// self-hosted compiler: a fixed input rebound before an application
/// (`StructRef`, `I32`), a copy narrower than its source, a captured item
/// mentioning a type applications supply, a pin through a polymorphic
/// input (the "held" closure), a pin inside a functor result, an
/// unbracketed functor's binding that pins what it is polymorphic in, and
/// rule 7's conditions: two families producing one key, a member bound
/// directly, two items of one family sharing a key's instance, and a
/// family binding applying a functor that is not a family.
#[test]
fn d84_functors_refused() {
    for (name, message) in [
        // The oracle's messages differ from the Python bootstrap's where
        // marked; the Python message follows each.
        // Python: "where the types it is fixed in mean what they meant".
        ("functor_fixed_stale", "applying `Heap` here reads `StructRef` as `I32`, but `Heap` is fixed in it"),
        ("functor_fixed_stale_i32", "applying `Arrays` here reads `I32` as `I64`, but `Arrays` is fixed in it"),
        // Python: "promises to be polymorphic in `StructRef`".
        ("functor_copy_narrower", "it is fixed in `StructRef`, in which `Boxing` is polymorphic (D84 rule 4)"),
        // Python: "which each application of `F` supplies".
        ("functor_capture_mentions_poly", "captures `mk0`, which mentions a type each application of `F` supplies (D84 3b)"),
        ("functor_pin_through_input", "a type each application of `F` supplies (D84 3d)"),
        ("functor_in_result_pin", "it pins `K`, which `Outer` is fixed in"),
        // Python: "and `Fast` is polymorphic in `Int` (D84)"; the oracle
        // reports it in lib/wasistd.moss, at `WasiFloats`' `bind Int`.
        ("functor_unbracketed_pin", BIND_INT_CHANGES_RESULT),
        // Python: "are families that produce the same method key".
        ("family_collision", "`Int.m` is produced by two families in force"),
        ("family_direct", "is a member of the family `Arrays`"),
        ("family_ambiguous", "is two items of the family `F` here"),
        // Python: "its bindings apply no functor but a family".
        ("family_apply", "applies `Tick`, which is not a family"),
    ] {
        let entry = wasi(name);
        oracle_says(&entry, message);
        report(&entry, PRELUDE);
    }
}

/// D78: `Ref[Boxed]`'s payload is the platform's abstract `StructRef`, so
/// a scope can rebind it to an `I32` address and provide `.boxed` and
/// `.deref` over linear memory. The same generic code boxes on the GC heap
/// in one block and in an arena in the other, and the back end decides "is
/// a reference" by `Ref`'s payload at the specialization, not by the `Ref`
/// symbol — so the arena's `Ref(p)` is no cast, and its `Ref`s are `i32`s.
#[test]
fn ref_over_an_arena() {
    let entry = wasi("arena_ref");
    assert_eq!(oracle_run(&entry, &[]), "29\n42\n".repeat(2));
    let module = build_with(&entry, PRELUDE);
    let text = wasm_text(&module);
    assert!(text.contains("struct.new"));
    assert!(!text.contains("ref.cast"));
    assert_eq!(run_module(&module, &[]), "29\n42\n".repeat(2));
}

/// D78: `bind F = G;` needs `G`'s inputs among `F`'s and `G`'s result
/// exactly `F`'s, checked where the bind is written (more inputs, a
/// missing output, an extra output); `G`'s binds may use only `G`'s
/// inputs, whatever an application has; `bind F;` with nothing bound to
/// `F` is the ordinary error for a requirement not in context; a functor
/// bound to one whose result holds a type must provide that type too; each
/// item of the result must be made at the type the application promises;
/// and no value of a type `bind F;` made may leave its block. Rejected by
/// the oracle and the self-hosted compiler.
#[test]
fn abstract_functor_binds_are_checked() {
    refused(
        &[
            // D84 (draft, corrected): an input mentioning a type each
            // application supplies must be an input of `Trace`. D84: the
            // self-hosted compiler checks the binding where it is made, at
            // fresh types: the provider it captures is refused there, in
            // its lambda.
            // The oracle's message; the Python bootstrap said "`Counting`
            // cannot be bound to `Trace`: it needs `counter`, which mentions
            // `CellInt`, which each application of `Trace` supplies, so it
            // must be an input of `Trace` (D84)".
            (
                "functor_inputs",
                "`Counting`, bound to `Trace`, captures `counter`, which \
                 mentions a type each application of `Trace` supplies (D84 3b)",
                "?S {} note\n",
            ),
            (
                "functor_missing",
                "`Half` cannot be bound to `Trace`: it does not provide \
                 `flush`, which `Trace` promises",
                "!X {} flush\n",
            ),
            (
                "functor_extra",
                "`Chatty` cannot be bound to `Trace`: it also provides \
                 `flush`, which `Trace` does not",
                "!X {} flush\n",
            ),
            // A bound functor relies only on its own argument signature,
            // though every application here has more. D84: refused also
            // where it is bound (`count` is checked at the types each
            // application supplies).
            // The oracle's message is the plain one for a requirement not in
            // context; the Python bootstrap prefixed "functor `Tally`, bound
            // to an abstract functor, may rely only on its own argument
            // signature (D78): ".
            (
                "functor_body",
                "calling `count` needs `counter`, which is not available in the context here",
                "!R {0} counter\n!P {0} counter\n",
            ),
            ("functor_unbound", "`Trace` is not available in the context here", "?Q {} demo Trace\n"),
            // A type in the result is provided like any other item.
            (
                "functor_type",
                "`Twice` cannot be bound to `Pick`: it does not provide \
                 `Scalar`, which `Pick` promises",
                "!X {} Scalar\n",
            ),
            // A bound functor making a result item at another type than the
            // application promises (its own `Int`, inside). D84 (draft,
            // corrected): refused where it is bound, since an unbracketed
            // `Fast` is polymorphic in `Int`; at the bind, in `main`.
            // The oracle's message, in lib/wasistd.moss at `WasiFloats`'
            // `bind Int`; the Python bootstrap said "`WasiFloats` cannot be
            // bound to `Fast`: it binds `Int` to `Num[I32=I32@Fast]`, and
            // `Fast` is polymorphic in `Int` (D84)".
            (
                "functor_promise",
                BIND_INT_CHANGES_RESULT,
                "?X {0} main float\n?X {0} main trunc\n?X {0} main round\n",
            ),
            // Only a functor `Pick` is bound to where nothing reaches makes
            // `make` at another type than promised, and the application is
            // checked against it all the same. D84: at the bind, which
            // nothing reaches.
            // The oracle's message; the Python bootstrap said "`Bad` cannot
            // be bound to `Pick`: it binds `Int` to `Num[I32=I32@Pick]`, and
            // `Pick` is polymorphic in `Int` (D84)".
            (
                "functor_promise_candidate",
                BIND_INT_CHANGES_RESULT,
                "?X {} other make\n",
            ),
            // A bound functor relies only on its own argument signature for
            // the types its result mentions too.
            // The oracle's message has no "functor `Good`, bound to an
            // abstract functor, may rely only on its own argument signature
            // (D78): " in front, as the Python bootstrap's did.
            (
                "functor_body_type",
                "type `Int` is not available in the context here",
                "!X {} Int\n",
            ),
            // A value of a type `bind F;` made leaving its block, where the
            // type made differs from the one outside.
            (
                "functor_escape",
                ESCAPES_FAST,
                "?W {} leak Float\n",
            ),
        ],
        false,
    );
}

/// D72 and D78: two generations of a type that specialize to one type are
/// still two types, since a function is checked as written, against its
/// generic types: `x`, a `T` from before `bind T = Int;`, used as a `T`
/// after it where `T` was `Int` all along; and a value of the `Float`
/// `bind Fast;` made leaving its block where that `Float` is the f64 the
/// outer one is too. The self-hosted compiler used to check types as
/// specialized, and compiled both. Rejected by both front ends.
#[test]
fn generations_that_specialize_alike_are_told_apart() {
    refused(
        &[
            ("generation_mix", "`y` is declared `Int` but initialized with `T`", "?S {} mix\n"),
            (
                "functor_escape_same",
                ESCAPES_FAST,
                "?W {} leak Float\n",
            ),
        ],
        false,
    );
}

/// Reference/scalar union payloads keep separate typed lanes, including in
/// recursive GC boxes. The oracle evaluates the same tree without a layout.
#[test]
fn union_payloads_keep_distinct_reference_and_scalar_lanes() {
    let entry = wasi("gc_slots");
    assert_eq!(run_moss(&entry, PRELUDE), "2\n");
    assert_eq!(oracle_run(&entry, &[]), "2\n");
}

/// The rest of D59's value model: a record is its fields' runs, one after
/// another, so records nest, sit inside unions of different widths, travel
/// as parameters, returns and contextual vals, and destructure by name —
/// renamed, wildcarded, or whole.
#[test]
fn records() {
    let entry = wasi("records");
    assert_eq!(run_moss(&entry, PRELUDE), RECORDS_OUT);
    // The oracle too, which matches a brace tag with a binder pattern
    // (`Box(b)`, D83) by binding its fields as a record.
    assert_eq!(oracle_run(&entry, &[]), RECORDS_OUT);
}

/// tests/wasi/order.moss: a call evaluates its receiver, then its
/// arguments left to right — an attached method, a provided one, and an
/// operator — compiled and in the oracle.
#[test]
fn receiver_first_then_arguments_left_to_right() {
    let entry = wasi("order");
    let expected = "rab\nrab\nxy\n";
    assert_eq!(run_moss(&entry, PRELUDE), expected);
    assert_eq!(oracle_run(&entry, &[]), expected);
}

/// tests/wasi/memory_bounds.moss: memory grows only by `memory_grow`, so a
/// load past its end traps (wasmtime's exit status for one is 134, as is
/// the oracle's), compiled and in the oracle.
#[test]
fn a_load_past_the_end_of_memory_traps() {
    let entry = wasi("memory_bounds");
    oracle_traps(&entry, "a\n", "[moss_ref: trap out of bounds memory access]");
    let out = wasmtime_out(&build_with(&entry, PRELUDE));
    assert_eq!((out.code, out.text().as_str()), (TRAP, "a\n"));
    assert!(out.stderr.contains("out of bounds memory access"), "{}", out.stderr);
}

/// Inside `assume Std`, every function needs every `Std` val, and each
/// provider bound to one of its fn needs brings its own along. Passing them
/// all ran `bar` here past Wasm's thousand-parameter limit; the back end
/// passes only the vals a function can reach.
#[test]
fn only_reachable_vals_are_passed() {
    let entry = wasi("levels");
    assert_eq!(run_moss(&entry, PRELUDE), "1\n3\n");
    assert_eq!(oracle_run(&entry, &[]), "1\n3\n");
}

/// A recursive function that rebinds one of its own fn needs on its own
/// recursive path. Keyed by everything in scope, each specialization
/// captured the provision it shadowed and demanded another, without end;
/// keyed by what is reached, there are two. Its `Float` result also has to
/// be learned, not assumed an i32.
#[test]
fn rebinding_on_the_recursive_path_terminates() {
    let entry = wasi("rebind_recursive");
    assert_eq!(run_moss(&entry, PRELUDE), "2\n1\n0\n11\n");
    assert_eq!(oracle_run(&entry, &[]), "2\n1\n0\n11\n");
}

/// D73's listed parameters must be types the declaration requires, and
/// D74's attached method is at its receiver's instantiation: a
/// `Box[T=Int]`'s `.get()` is an `Int` wherever `T` has been rebound, and a
/// `Cell[Item=Int]` read where the `Char` codec is in force needs its codec
/// at `Int`. Both used to read an `Int` as a `Char`.
#[test]
fn phantoms_and_receivers_are_checked() {
    refused(
        &[
            (
                "phantom_unlisted",
                "`Cell` lists `Other`, which is not a type its assume nesting \
                 requires (D73)",
                "!V {} Other\n",
            ),
            ("receiver_rebound", "argument `c` of `putchar` is `Int`, expected `Char`", "?S {} demo\n"),
            // The oracle's message names neither instantiation, and names
            // `encode`, the first incoherent item, where the Python
            // bootstrap said "calling `Cell.first` needs `decode` at
            // [Item=Int], but the one in the context here was provided at
            // [Item=Char]".
            (
                "phantom_misread",
                "calling `Cell.first` needs `encode` at another instantiation \
                 than the one in the context here was provided at",
                "?S {} demo\n",
            ),
            // D75: a reference's `.deref()` is at what it holds.
            ("gc_misread", "argument `c` of `putchar` is `Int`, expected `Char`", "?S {} demo\n"),
            // D75: boxing is a context item, which only the platform's
            // `Heap` provides; code that has not applied it cannot box.
            (
                "gc_unheaped",
                "no method `.boxed` is available for `Int` in the context here",
                "?K {} demo boxed\n",
            ),
            // D81: so is making an array, which only `Arrays` provides...
            (
                "gc_unarrayed",
                "no method `.arrayed` is available for `Int` in the context here",
                "?K {} demo arrayed\n",
            ),
            // ...and writing through a reference, which only `Mutable` does.
            ("gc_immutable", "no method `.Ref.set` is available for `Ref[Boxed=Int", "?K {} demo set\n"),
            // ...and an array's `.get` is at what it holds (D74).
            ("gc_array_misread", "argument `c` of `putchar` is `Int`, expected `Char`", "?S {} demo\n"),
            // ...and applying `Heap` needs its argument signature here.
            (
                "gc_needs_boxed",
                "applying `Heap` needs `Boxed`, which is not available in \
                 the context here",
                "?Q {} demo Boxed\n",
            ),
        ],
        false,
    );
}

/// D79: a program names a second memory as a unit and binds `Memory` to it
/// in a block, where the library's lists live in it. A list in each
/// memory, summed; one memory's allocator reset, which reuses its own space
/// and leaves the other's data alone; and text made in `Scratch` copied
/// into the main memory to be printed, and back. Compiled, each module has
/// two memories, exports the first, and puts the second's index on the
/// instructions specialized at it. The oracle has one memory.
#[test]
fn multiple_memories() {
    for (name, expected) in [
        ("memories", "5050\n51225\n56275\n"),
        ("memory_reset", "51225\n55\n1024\n1\n5050\n"),
        ("memory_copy", "hi\nih\n"),
    ] {
        let entry = wasi(name);
        // Oracle gap (D79): one linear memory, so a `Memory` bind is
        // unsupported.
        oracle_unsupported(&entry);
        let module = build_with(&entry, PRELUDE);
        let text = wasm_text(&module);
        assert!(text.contains("(memory $0 "), "{entry}");
        assert!(text.contains("(memory $1 "), "{entry}");
        assert!(!text.contains("(memory $2 "), "{entry}");
        assert!(text.contains("(export \"memory\" (memory $0))"), "{entry}");
        assert!(text.contains("i32.load $1"), "{entry}");
        assert!(text.contains("i32.store $1"), "{entry}");
        assert_eq!(run_module(&module, &[]), expected, "{entry}");
    }
}

/// D79 under D84: the memory instructions are `Addr`'s methods, a family
/// polymorphic in `Memory`, so binding `Memory` to any type — here `Num`
/// and `Chr`, neither a unit — names a memory of its own, and nothing is
/// refused after checking (D16).
#[test]
fn any_type_names_a_memory() {
    let entry = wasi("memory_any_type");
    // Oracle gap (D79): one linear memory.
    oracle_unsupported(&entry);
    let module = build_with(&entry, PRELUDE);
    let text = wasm_text(&module);
    assert!(text.contains("(memory $2 "));
    assert!(!text.contains("(memory $3 "));
    assert!(text.contains("i32.store $2"));
    assert_eq!(run_module(&module, &[]), "5050\n51225\n1275\n5050\n");
}

/// D78 and D79: an abstract functor whose result holds `Memory` and the
/// containers over it, bound in `main` to one that puts them in a second
/// memory, is an arena: each `bind Arena;` makes fresh types, so the
/// trajectories built there are dropped (the allocator reset) at the end
/// of each block, and the numbers kept from them live in the main memory.
/// Compiled, with the temporaries' loads and stores in memory 1. A list
/// made in the arena leaving its block — as the block's value, assigned to
/// an outer `var`, or inside a record — is refused by the oracle and the
/// self-hosted compiler.
#[test]
fn an_arena_whose_data_cannot_outlive_it() {
    let entry = wasi("arena");
    // Oracle gap (D79): one linear memory.
    oracle_unsupported(&entry);
    let module = build_with(&entry, PRELUDE);
    let text = wasm_text(&module);
    assert!(text.contains("(memory $1 "));
    assert!(!text.contains("(memory $2 "));
    assert!(text.contains("i32.load $1"));
    assert!(text.contains("i32.store $1"));
    assert_eq!(run_module(&module, &[]), "6 8 16\n7 16 52\n27 111 9232\n97 118 9232\n");

    refused(
        &[
            // The oracle's messages name the functor `F`, whatever it is;
            // the Python bootstrap said "this block yields `IntList@Arena`,
            // the type `bind Arena;` made in it: a value of that type cannot
            // leave the block (D78)", and for arena_wrapped "..., which
            // mentions `IntList@Arena`, the type `bind Arena;` made in it:
            // ...".
            (
                "arena_escape",
                "this block yields `IntList@Arena`, which mentions `IntList@Arena`, \
                 a type `bind F;` made in it: a value of that type cannot leave \
                 the block (D78)",
                "?W {} report IntList\n",
            ),
            // D84 (draft): `report` is generic over `Std` now, so the outer
            // list is `Std`'s `IntList`.
            ("arena_assign", "cannot assign `IntList@Arena` to `kept: IntList`", "?S {} report\n"),
            (
                "arena_wrapped",
                "this block yields `Run[Int=Int, IntList=IntList@Arena]`, \
                 which mentions `IntList@Arena`, a type `bind F;` made in \
                 it: a value of that type cannot leave the block (D78)",
                "?W {} report IntList\n",
            ),
        ],
        false,
    );
}

/// D72 and D78: two applications of one abstract functor in force at once
/// make two generations of each type in its result, and a type error
/// between them names them apart (`IntList@Arena#2`), rather than
/// "`IntList@Arena`, expected `IntList@Arena`". The self-hosted compiler's
/// type errors carry no detail.
#[test]
fn two_applications_print_as_two_types() {
    let entry = wasi("arena_twice");
    // Known oracle gap: its type errors do not tell generations apart, so
    // it says the very thing this test is about; the Python bootstrap said
    // "argument `xs` of `total` is `IntList@Arena`, expected
    // `IntList@Arena#2`".
    oracle_says(&entry, "argument `xs` of `total` is `IntList@Arena`, expected `IntList@Arena`]");
    assert_eq!(report(&entry, PRELUDE), format!("?S {entry} report\n"));
}

/// D79 changes nothing for a program that binds no `Memory`: the module
/// has one memory, as before.
#[test]
fn one_memory_is_one_memory() {
    let text = wasm_text(&build_with(&wasi("lists"), PRELUDE));
    assert!(text.contains("(memory $0 "));
    assert!(!text.contains("(memory $1 "));
    assert!(!text.contains("i32.load $1"));
}

/// D79: a list or a string lists the memory it is in, so one made in
/// `Scratch` is not the main memory's, though both are an address: passing
/// a `Scratch` list where `main`'s `IntList` is expected, and printing a
/// `Scratch` string as it is (WASI reads only the main memory), are type
/// errors in both front ends; and calling WASI where `Memory` is rebound
/// is incoherent.
#[test]
fn a_handle_is_typed_by_its_memory() {
    refused(
        &[
            (
                "memory_mismatch",
                "argument `xs` of `sum` is `Packed[Item=Num[I32=I32], \
                 Memory=Scratch, I32=I32]`, expected `Packed[Item=Num[I32=I32], \
                 Memory=Memory, I32=I32]`",
                "?S {} main\n",
            ),
            (
                "memory_print",
                "argument `s` of `print` is `Str[Memory=Scratch, I32=I32]`, \
                 expected `Str[Memory=Memory, I32=I32]`",
                "?S {} main\n",
            ),
            // WASI preview 1 reads only the first memory, so its functions
            // are provided at the root's `Memory` alone: a WASI-calling
            // functor applied where `Memory` is rebound is incoherent, where
            // it used to write into `Scratch` and print `AA`. Every item of
            // `Wasi` is incoherent there, and the one named is the one
            // `wasi_putchar` reaches, not the first `Wasi` lists.
            //
            // The oracle's message (in lib/wasistd.moss) names neither
            // instantiation nor the path, and names `args_get`, the first
            // item of `Wasi`, where the Python bootstrap said "calling
            // `wasi_putchar` needs `fd_write` at [Memory=Scratch], reached
            // through `write_all`".
            (
                "memory_wasi",
                "calling `wasi_putchar` needs `args_get` at another \
                 instantiation than the one in the context here was provided at",
                "!R {} fd_write\n",
            ),
        ],
        false,
    );
}

/// D72: what was typed before a bind of `Int` keeps the `Int` it had, in a
/// function generic over it, while a value made after the bind adds with
/// the new one's `+`.
#[test]
fn a_type_bind_starts_a_generation() {
    let entry = wasi("generations");
    assert_eq!(run_moss(&entry, PRELUDE), "-2\n40\n");
    assert_eq!(oracle_run(&entry, &[]), "-2\n40\n");
}

/// D72: `bind Float = Dual` over the `Float` in force, inside functions
/// generic over `Float`, differentiates code written against it, and twice
/// gives a second derivative. Nested differentiation is correct (1)
/// because the outer variable has to be lifted into the inner generation;
/// added as it is, it is a type error, not perturbation confusion (2). The
/// oracle runs the one and rejects the other.
#[test]
fn dual_numbers_by_rebinding_float() {
    let entry = wasi("dual");
    assert_eq!(run_moss(&entry, PRELUDE), DUAL_OUT);
    assert_eq!(oracle_run(&entry, &[]), DUAL_OUT);

    let entry = wasi("perturbation");
    oracle_says(
        &entry,
        "argument `rhs` of `.add` is `Dual[Base=Dual[Base=Float]]`, \
         expected `Dual[Base=Float]`",
    );
    let out = report(&entry, PRELUDE);
    assert!(out.starts_with(&format!("?S {entry} nested\n")), "{out}");
}

/// D68's exponent literals under a library `Float`: inside `Dualize`,
/// `2.5e-1` and `1.5e2` are built from `constant` and `Dual`'s own `.mul`,
/// `.add` and `.div`, constants with no derivative, so g(x) = 2.5e-1 x^2 +
/// 1.5e2 x has g'(2) = 151. The oracle runs it too.
#[test]
fn exponents_under_dual_numbers() {
    let entry = wasi("exponents_dual");
    assert_eq!(run_moss(&entry, PRELUDE), "301000000\n151000000\n");
    assert_eq!(oracle_run(&entry, &[]), "301000000\n151000000\n");
}

/// D72 as numeric code meets it: `f` assumes all of `Std`, and `Dualize`
/// provides all of float.moss's `Floats` at duals — the comparisons,
/// `.sqrt`, `.trunc` and `.round`, and `FloatList` as two lists of the base
/// `Float` — applied once for f' and inside itself for f''. Every one of
/// those providers is an anonymous function made with every one bound
/// before it in force, and the self-hosted compiler used to pass each
/// function what all of them captured: a module with a function of 4101
/// parameters for f', and no end to compiling f''. The oracle runs it too.
#[test]
fn dual_numbers_under_all_of_std() {
    let entry = wasi("dual_std");
    assert_eq!(run_moss(&entry, PRELUDE), DUAL_STD_OUT);
    assert_eq!(oracle_run(&entry, &[]), DUAL_STD_OUT);
}

/// A method a function assumes but cannot reach by name passes nothing to
/// it, as an unreached plain fn already did: `f` assumes twelve methods,
/// each provided by an anonymous function that reads `one`, and calls one
/// of them. It used to take `one` once per subset of the twelve providers,
/// 4095 parameters, which no engine would load.
#[test]
fn a_function_takes_only_the_providers_it_reaches() {
    let entry = wasi("unreached_providers");
    assert_eq!(run_moss(&entry, PRELUDE), "6\n");
    assert_eq!(oracle_run(&entry, &[]), "6\n");
}

/// D26, D70: an anonymous provider captures a val whose type mentions a
/// type bound beside the bind (`Holder[Item]`, `Item = Float`, and then a
/// record of two `Int`s), and a function generic over the provided fn
/// takes the capture to pass on, directly and through a second provider's
/// capture. Its type is read where it was captured; read in the callee's
/// environment, which knows nothing of `Item`, it was one scalar, and the
/// module did not validate.
#[test]
fn a_capture_is_laid_out_where_it_was_captured() {
    for (name, expected) in [("capture_width", "15\n29\n15\n"), ("capture_wide", "15\n29\n")] {
        let entry = wasi(name);
        assert_eq!(run_moss(&entry, PRELUDE), expected, "{entry}");
        assert_eq!(oracle_run(&entry, &[]), expected, "{entry}");
    }
}

/// D55: a functor with binds must bind each item of its result itself, or
/// apply a functor that provides it — a type bind does not bind a method
/// the type then resolves to (`bind Base = Int;` for a promised
/// `Base.add`) — checked at the functor wherever it is applied, in a
/// function nothing calls too. The self-hosted compiler reports
/// `e_partial` (`Z`), named by the item; it used to accept both.
#[test]
fn a_functor_binds_everything_it_promises() {
    refused(
        &[
            ("functor_partial", "functor `Over` does not bind `Base.add`", "!Z {} add\n"),
            ("functor_partial_unreached", "functor `Greeter` does not bind `greeting`", "!Z {} greeting\n"),
        ],
        true,
    );
}

/// A local comes from emission itself, so a construct that needs a
/// temporary works however deeply it is nested: a `match` in a call's
/// arguments, as a method's receiver, in a record field or an unannotated
/// `let`, and a field of a record that is not in a local. A separate walk
/// used to create locals ahead of emission and never looked inside a
/// call's arguments.
#[test]
fn temporaries_nested_anywhere() {
    assert_eq!(run_moss(&wasi("nested"), PRELUDE), NESTED_OUT);
}

/// No heap means no finite layout for a type that contains itself, so it
/// is `e_shape` (`N`), naming the type, rather than a compiler that never
/// stops laying it out.
#[test]
fn a_type_that_contains_itself_is_reported() {
    let report = report(&wasi("cyclic"), "");
    assert!(report.starts_with("!N "), "{report}");
    assert!(report.contains("Cell"), "{report}");
}

/// A path the host refuses -- absent here, but equally one that lies
/// outside the directory it opened -- is `e_read` (`O`). The read itself
/// cannot report it: `Path.read` has no `String` to return and traps,
/// which is why `parse_module` asks `Path.readable` first. Before that, a
/// mistyped import came out of the driver as a Wasm backtrace.
#[test]
fn a_module_that_will_not_open_is_reported() {
    let report = report(&wasi("absent"), "");
    assert!(report.starts_with("!O "), "{report}");
    assert!(report.contains("tests/wasi/nosuch.moss"), "{report}");
}

/// The D17 check, wired into the back end: a value of one nominal type
/// where another is declared, both one scalar wide, so nothing but
/// `types::fits` would notice. The oracle rejects it in elaboration; the
/// self-hosted compiler reports `c_bad_type` (`?S`) and writes nothing.
#[test]
fn a_misfitting_type_is_reported() {
    let entry = wasi("misfit");
    oracle_rejects(&entry);
    let report = report(&entry, "");
    assert!(report.starts_with("?S "), "{report}");
}

/// Each: (entry, the oracle's message, the self-hosted report, prelude).
const UNCALLED: &[(&str, &str, &str, &str)] = &[
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
    // An anonymous function a reached bind makes, never called.
    (
        "tests/wasi/uncalled_lambda.moss",
        "block yields `Int`, expected `Char`",
        "?S tests/wasi/uncalled_lambda.moss step\n",
        PRELUDE,
    ),
    // A functor bound to an abstract one that nothing applies (D78).
    (
        "tests/wasi/functor_unapplied.moss",
        // The oracle's message; the Python bootstrap prefixed "functor
        // `Tally`, bound to an abstract functor, may rely only on its own
        // argument signature (D78): ".
        "calling `count` needs `counter`, which is not available in the context here",
        "!R tests/wasi/functor_unapplied.moss counter\n\
         !P tests/wasi/functor_unapplied.moss counter\n",
        PRELUDE,
    ),
];

/// D16: no post-monomorphization checks, so a function is checked whether
/// or not anything calls it — a concrete one, a generic one, of which no
/// specialization exists, an anonymous one, and the binds of a functor
/// bound to an abstract one that nothing applies. The oracle elaborates
/// every function once, against its generic types.
#[test]
fn an_uncalled_function_is_checked_by_the_bootstrap() {
    for (entry, message, _, _) in UNCALLED {
        oracle_says(entry, message);
    }
}

/// The same programs. The self-hosted compiler types a body as it emits
/// it, so after the passes that emit what the program reaches, a check
/// pass walks everything they did not, generically, and reports what it
/// finds, naming the frame.
#[test]
fn an_uncalled_function_is_checked_by_the_self_hosted_compiler() {
    for (entry, _, expected, prelude) in UNCALLED {
        assert_eq!(report(entry, prelude), *expected, "{entry}");
    }
}

/// Each: (entry, the oracle's message, the self-hosted report), compiled
/// without the prelude.
const MATCH_ARMS: &[(&str, &str, &str)] = &[
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
];

fn oracle_says_each(cases: &[(&str, &str, &str)]) {
    for (entry, message, _) in cases {
        oracle_says(entry, message);
    }
}

fn reports_each(cases: &[(&str, &str, &str)]) {
    for (entry, _, expected) in cases {
        assert_eq!(report(entry, ""), *expected, "{entry}");
    }
}

/// D16 and D34: every arm of a `match` is checked against the scrutinee's
/// type as written, whichever arms a specialization would run — on a
/// scrutinee of abstract type `T`, whose one head is `T` itself, so that a
/// binder or `_` covers it and anything after one is never run, in a
/// function nothing calls, in one reached where `T` has one head and where
/// it is a union; and a tag pattern on it names no member of `T`.
#[test]
fn every_match_arm_is_checked_by_the_bootstrap() {
    oracle_says_each(MATCH_ARMS);
}

/// The same programs. The self-hosted compiler takes a match's shape from
/// the specialization — one head runs the first arm, and a union stops at
/// the first arm that matches anything — and checks the arms it does not
/// run all the same, emitting them nowhere.
#[test]
fn every_match_arm_is_checked_by_the_self_hosted_compiler() {
    reports_each(MATCH_ARMS);
}

const MATCH_PATTERNS: &[(&str, &str, &str)] = &[
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
];

/// D34: a unit or tag pattern, with or without a payload, names a member
/// of the scrutinee's type as written — so on a scrutinee of abstract type,
/// whose one head is the type itself, only a binder or `_` is a pattern,
/// and what a match means cannot depend on what the type is specialized to
/// (D16). A bare unit pattern on a `T` bound to `Colour` and to `Green`,
/// which the runners used to disagree on, and one naming a unit outside the
/// union.
#[test]
fn a_pattern_names_a_member_by_the_bootstrap() {
    oracle_says_each(MATCH_PATTERNS);
}

/// The same programs.
#[test]
fn a_pattern_names_a_member_by_the_self_hosted_compiler() {
    reports_each(MATCH_PATTERNS);
}

const MATCH_PATTERN_FORMS: &[(&str, &str, &str)] = &[
    (
        "tests/wasi/match_not_pattern.moss",
        "`red` is not a matchable pattern",
        "?S tests/wasi/match_not_pattern.moss pick red\n",
    ),
    (
        "tests/wasi/match_unit_payload.moss",
        "`Red` is not a tag",
        "?S tests/wasi/match_unit_payload.moss pick Red\n",
    ),
];

/// D34: a single name is a binder only when it names nothing in scope, and
/// otherwise must be a unit or a tag; a pattern with a payload, or a record
/// of fields, opens a tag.
#[test]
fn a_pattern_is_well_formed_by_the_bootstrap() {
    oracle_says_each(MATCH_PATTERN_FORMS);
}

/// The same programs. The self-hosted compiler bound a name that resolves
/// to a function, and a unit's payload, which is `()`.
#[test]
fn a_pattern_is_well_formed_by_the_self_hosted_compiler() {
    reports_each(MATCH_PATTERN_FORMS);
}

const MATCH_EXHAUSTIVE: &[(&str, &str, &str)] = &[
    (
        "tests/wasi/match_not_exhaustive.moss",
        // The oracle lists the heads bare; Python printed `['Green']`.
        "match is not exhaustive; missing Green",
        "?S tests/wasi/match_not_exhaustive.moss pick Green\n",
    ),
    (
        "tests/wasi/match_empty.moss",
        "match is not exhaustive; missing Red, Green",
        "?S tests/wasi/match_empty.moss pick Red\n",
    ),
];

/// D34: without a binder or `_`, a match's arms cover every head of its
/// scrutinee's type as written, whichever ones a run reaches; and `match e
/// {}` is for an `e` of type `|` alone.
#[test]
fn a_match_is_exhaustive_by_the_bootstrap() {
    oracle_says_each(MATCH_EXHAUSTIVE);
}

/// The same programs, each reported by the first head missing.
#[test]
fn a_match_is_exhaustive_by_the_self_hosted_compiler() {
    reports_each(MATCH_EXHAUSTIVE);
}

const BRANCHES_DISAGREE: &[(&str, &str, &str)] = &[
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
];

/// With nothing expected of it, an `if`'s branches and a `match`'s arms
/// must yield one type, unless one diverges — by its value: a branch block
/// without a tail yields `()`, however its last statement ends.
#[test]
fn branches_agree_by_the_bootstrap() {
    oracle_says_each(BRANCHES_DISAGREE);
}

/// The same programs, compared as written (D80).
#[test]
fn branches_agree_by_the_self_hosted_compiler() {
    reports_each(BRANCHES_DISAGREE);
}

/// A `return`, a `break`, or a call of a function returning `|`, as the
/// value of a branch, beside a branch of any type, with and without an
/// expected type. In the oracle.
#[test]
fn a_diverging_branch_agrees_by_the_bootstrap() {
    assert_eq!(oracle_run(&wasi("branches_diverge"), &[]), "DEEIAB\n");
}

/// The same program. A diverging branch is typed `|`, and where the `if`
/// or `match` yields a value, a branch that cannot fall through still has
/// to validate as one that does.
#[test]
fn a_diverging_branch_agrees_by_the_self_hosted_compiler() {
    assert_eq!(run_moss(&wasi("branches_diverge"), ""), "DEEIAB\n");
}

/// D16 and D78: `bind F;` in a function nothing calls is checked against
/// every functor `F` is bound to anywhere — each must make each item of
/// `F`'s result at the type the application promises
/// (functor_promise.moss, with `kernel` unreached).
#[test]
fn an_uncalled_abstract_application_is_checked_by_the_bootstrap() {
    // The oracle's message, in lib/wasistd.moss at `WasiFloats`' `bind
    // Int`; the Python bootstrap said "`WasiFloats` cannot be bound to
    // `Fast`: it binds `Int` to `Num[I32=I32@Fast]`, and `Fast` is
    // polymorphic in `Int` (D84)".
    oracle_says(&wasi("functor_promise_unreached"), BIND_INT_CHANGES_RESULT);
}

/// The same program. D84: each binding is checked where it is made,
/// generic in what applications supply, whether or not any application is
/// reached: `WasiFloats` makes each of `Floats`' items over `Int` at its
/// own, which an unbracketed `Fast` is polymorphic in.
#[test]
fn an_uncalled_abstract_application_is_checked_by_the_self_hosted_compiler() {
    let entry = wasi("functor_promise_unreached");
    // D84: `Floats` has no list now.
    let expected: String =
        ["float", "trunc", "round"].iter().map(|item| format!("?X {entry} main {item}\n")).collect();
    assert_eq!(report(&entry, PRELUDE), expected);
}

/// D20 and D16: a call needs everything the callee's declaration assumes,
/// read off the declaration, not off what its body happens to use: `quiet`
/// assumes `X.gimme` and never calls it, and calling it with no `X.gimme`
/// in context is rejected by both front ends. The self-hosted compiler
/// used to find a need missing only when emitting a use of it.
#[test]
fn a_callee_requires_what_it_assumes_and_does_not_use() {
    let entry = wasi("unused_need");
    oracle_says(
        &entry,
        "calling `quiet` needs `X[Char=Char].gimme`, which is not \
         available in the context here",
    );
    assert_eq!(report(&entry, PRELUDE), format!("!P {entry} gimme\n"));
}

/// Coherence: a callee's val, fn or method requirement is met only by a
/// provision made at the instantiation the callee means — a method item
/// carries its application, and a val or fn its ambient one (CoreMoss.v
/// deviation 8). Each of these was once accepted by the Python bootstrap,
/// which matched the requirement by its key alone, and read a `Char` as an
/// `Int` or a `Point`. Both front ends reject each at the call: the oracle
/// with the message below, the self-hosted compiler with `e_incoherent`
/// (`R`), named by the item. (It used to report `c_bad_type`, `?S`, from
/// the back end's `types::fits` on a specialization.)
#[test]
fn a_provision_answers_only_its_instantiation() {
    for (name, callee, needs, made, item) in [
        ("incoherent_method", "use_it", "`X[Char=Char].gimme` at [Foo=Int]", "[Foo=Char]", "gimme"),
        ("incoherent_bind_application", "use_it", "`W[Char=Char].m` at [T=Int]", "[T=Char]", "m"),
        ("incoherent_rebound", "use_it", "`W[Char=Char].m` at [T=Int]", "[T=Char]", "m"),
        ("incoherent_core_shape", "use_it", "`W[Char=Char].m` at [T=Point[Char=Char]]", "[T=Char]", "m"),
        ("incoherent_val_fn", "get_seed", "`seed` at [T=Int]", "[T=Char]", "seed"),
        ("incoherent_fn", "get_made", "`make` at [T=Int]", "[T=Char]", "make"),
    ] {
        let entry = wasi(name);
        // The oracle's message names neither instantiation, where the
        // Python bootstrap said "calling `{callee}` needs {needs}, but the
        // one in the context here was provided at {made}".
        let _ = made;
        let needs = needs.split(" at ").next().unwrap();
        oracle_says(&entry, &format!("calling `{callee}` needs {needs} {INCOHERENT}"));
        let out = report(&entry, PRELUDE);
        assert!(out.starts_with(&format!("!R {entry} {item}\n")), "{out}");
    }
}

/// A val assumed at the ambient `T` does not serve a call made after `bind
/// T = Int` in the same body. Both front ends reject it. The self-hosted
/// compiler used to accept it and read a `Char` as an `Int`: the rebinding
/// is the same width, so its back end's `types::fits` on the
/// specialization saw nothing wrong.
#[test]
fn a_provision_from_before_a_rebind_is_refused() {
    let entry = wasi("incoherent_shadowed");
    // The oracle's message names neither instantiation, where the Python
    // bootstrap said "needs `seed` at [T=Int], but the one in the context
    // here was provided at [T as assumed, before a later `bind T`]".
    oracle_says(&entry, &format!("calling `get_seed` needs `seed` {INCOHERENT}"));
    assert_eq!(report(&entry, PRELUDE), format!("!R {entry} seed\n"));
}

/// `assume seed, T` — the type after the item — still puts `seed` at the
/// function's `T`, so two functions of that block call each other. The
/// Python bootstrap's first coherence check read `T` as not in force for
/// `seed`, and refused the call.
#[test]
fn an_assumed_provision_is_at_the_whole_ambient_instantiation() {
    let entry = wasi("coherent_assume_order");
    assert_eq!(oracle_run(&entry, &[]), "c\n");
    assert_eq!(run_moss(&entry, PRELUDE), "c\n");
}

/// The control: provide again at the new `T`, and both run it.
#[test]
fn reproviding_after_a_rebind_is_coherent() {
    let entry = wasi("coherent_rebound");
    assert_eq!(oracle_run(&entry, &[]), "ab\n");
    assert_eq!(run_moss(&entry, PRELUDE), "ab\n");
}

/// D20: assuming `W.m`, or `seed` and `make`, whose signatures mention
/// `T`, is legal only where `T` is assumed or bound. The oracle says the
/// type is not available; the self-hosted compiler reports `e_absent`
/// (`P`), naming the type.
#[test]
fn an_assume_needs_the_types_its_item_mentions() {
    for name in ["assume_without_type", "assume_without_type_val_fn"] {
        let entry = wasi(name);
        oracle_says(&entry, "type `T` is not available in the context here");
        let out = report(&entry, PRELUDE);
        assert!(out.starts_with(&format!("!P {entry} T\n")), "{out}");
    }
}

/// D71: `tag A { b: B, t: T }; tag B { a: A };` — both are parameterized
/// by `T`, whichever is computed first, so `get_b(): B` mentions `T` and
/// assuming it without `T` is D20's error. Each compiler once answered by
/// whichever declaration it reached first, and accepted the A-first
/// program.
#[test]
fn relevance_is_a_least_fixpoint() {
    for order in ["a_first", "b_first"] {
        let entry = wasi(&format!("relevance_{order}"));
        oracle_says(&entry, "type `T` (required by `B`) is not available in the context here");
        let out = report(&entry, PRELUDE);
        assert!(out.starts_with(&format!("!P {entry} T\n")), "{out}");
    }
}

/// D71: brackets bind only a nominal's relevant parameters. A unit has
/// none (`Stop[Int=Char]`), and `W[T=Char]` binds a symbol `W`'s payload
/// does not mention. The oracle names the application; the self-hosted
/// compiler reports `e_irrelevant` (`Q`), named by the symbol bound. The
/// Python bootstrap used to accept both, ignoring the binding, and the
/// self-hosted compiler the second.
#[test]
fn applying_an_irrelevant_parameter() {
    for (name, nominal, bound, params) in
        [("irrelevant_unit", "Stop", "Int", "none"), ("irrelevant_tag", "W", "T", "`Char`")]
    {
        let entry = wasi(name);
        oracle_says(
            &entry,
            &format!(
                "application of `{nominal}` binds `{bound}`, which is not one of \
                 its parameters (the type symbols its payload mentions: {params}; D71)"
            ),
        );
        assert_eq!(report(&entry, PRELUDE), format!("!Q {entry} {bound}\n"));
    }
}

/// `B[T=Int]` is legal: `T` reaches `B` through `A`, and is relevant to
/// `B` even when `A` is computed first. Both accept it (and the
/// self-hosted compiler now reads the brackets on a type bind's right-hand
/// side rather than dropping them).
#[test]
fn applying_a_relevant_parameter_found_by_fixpoint() {
    let entry = wasi("relevance_applied");
    assert_eq!(oracle_run(&entry, &[]), "y\n");
    assert_eq!(run_moss(&entry, PRELUDE), "y\n");
}

/// D43, the designer's example: a context with `A.gimme[Foo=B]` and
/// `A.gimme[Foo=C]` (and a val at both) merges the two bindings, so `B` and
/// `C` are one type inside it and may be used interchangeably. The
/// self-hosted compiler used to keep the first of the two assumptions, and
/// found `C` where `B` was expected. Merging does not depend on the order
/// of the assume list (`merge_order`, which the Python bootstrap used to
/// reject), and is done in a functor's argument signature too
/// (`merge_functor`).
#[test]
fn a_merge_makes_two_types_one() {
    for (name, expected) in [("merge", "qbq\n"), ("merge_order", "k\n"), ("merge_functor", "tt\n")] {
        let entry = wasi(name);
        assert_eq!(oracle_run(&entry, &[]), expected, "{entry}");
        assert_eq!(run_moss(&entry, PRELUDE), expected, "{entry}");
    }
}

/// D43: a merge that would identify two distinct concrete types —
/// `A.gimme[Foo=U1]` with `A.gimme[Foo=U2]` — is an error where the context
/// is assumed, though nothing calls the function assuming it, or in a
/// functor's argument signature. The self-hosted compiler reports
/// `e_merge` (`Y`), named by the item whose bindings would not merge; it
/// used to keep the first and accept the program.
#[test]
fn an_inconsistent_merge() {
    for name in ["merge_inconsistent", "merge_functor_inconsistent"] {
        let entry = wasi(name);
        oracle_says(&entry, "cannot merge `U1` with `U2` for `.gimme` (D43)");
        assert_eq!(report(&entry, PRELUDE), format!("!Y {entry} gimme\n"));
    }
}

/// D43: a merged context requires the merged types to be one, so a
/// function assuming one may be called only where they are: not with `B`
/// and `C` bound to `Char` and `Int` (`merge_caller`, and
/// `merge_methods_caller`, where the merge also makes `B.m` and `C.m` one
/// key), and not from a function generic over `B` and `C` apart, though
/// every specialization of it binds both to `Char`
/// (`merge_caller_generic`, D80). The self-hosted compiler reports
/// `e_merge` (`Y`), named by the merged type.
#[test]
fn a_caller_binds_merged_types_alike() {
    for (name, here) in [
        ("merge_caller", "`Char` and `Int`"),
        ("merge_methods_caller", "`Char` and `Int`"),
        ("merge_caller_generic", "`B` and `C`"),
    ] {
        let entry = wasi(name);
        oracle_says(
            &entry,
            &format!(
                "needs `B` and `C` to be one type, which its context merged \
                 (D43), but here they are {here}"
            ),
        );
        assert_eq!(report(&entry, PRELUDE), format!("!Y {entry} B\n"));
    }
}

/// D18: a nominal type's identity is its declaration and its static
/// bindings, so `Box[Elem=I32]` substitutes into the payload. The oracle
/// and the self-hosted compiler run tests/wasi/applied.moss.
#[test]
fn applying_a_type() {
    let entry = wasi("applied");
    assert_eq!(oracle_run(&entry, &[]), "A\n");
    assert_eq!(run_moss(&entry, ""), "A\n");
}

/// A record whose fields are typed by an abstract `T` is laid out per
/// instantiation, so a method on it compiles at `T = Char` and at `T =
/// Point` alike.
#[test]
fn a_record_at_a_wider_instantiation() {
    assert_eq!(run_moss(&wasi("widened"), PRELUDE), "ba\nefd\n");
}

/// D62: a provision serves only the instantiation it was made at, so two
/// of one method at two instantiations coexist, each compiled for its own
/// type — and a receiver at a third finds none.
#[test]
fn provisions_are_keyed_by_instantiation() {
    assert_eq!(run_moss(&wasi("provisions"), PRELUDE), "ac\n");
    let report = report(&wasi("mismatch"), PRELUDE);
    assert!(report.starts_with("?K "), "{report}");
    assert!(report.contains("peek"), "{report}");
}

/// D70: applying a defined functor does not capture its caller's locals;
/// a same-name local inside the lambda belongs to the lambda itself.
#[test]
fn anonymous_function_in_functor_shadows_caller_local() {
    let entry = wasi("lambda_functor_shadow");
    assert_eq!(oracle_run(&entry, &[]), "");
    assert_eq!(run_moss(&entry, PRELUDE), "");
}

/// D70: a lambda body may return one member of its declared result union.
#[test]
fn anonymous_function_result_can_be_subtype() {
    let entry = wasi("lambda_result_subtype");
    assert_eq!(oracle_run(&entry, &[]), "");
    assert_eq!(run_moss(&entry, PRELUDE), "");
}

/// A method bind reads bracket arguments in its caller's type context.
#[test]
fn method_bind_brackets_read_caller_types() {
    let entry = wasi("method_bind_caller_brackets");
    assert_eq!(oracle_run(&entry, &[]), "");
    assert_eq!(run_moss(&entry, PRELUDE), "");
}

/// Copying a functor to itself preserves an untouched latent interface;
/// an inhabited interface still applies with the original fixed reading.
#[test]
fn functor_identity_copy_preserves_latent_interfaces() {
    for name in [
        "functor_latent_identity_copy",
        "functor_latent_identity_reexport",
        "functor_identity_copy_applied",
    ] {
        let entry = wasi(name);
        assert_eq!(oracle_run(&entry, &[]), "", "{name}");
        assert_eq!(run_moss(&entry, PRELUDE), "", "{name}");
    }
    let stale = wasi("functor_identity_copy_stale");
    rejects_with(&stale, PRELUDE);
    assert!(oracle_rejects(&stale).contains("fixed"));
}
