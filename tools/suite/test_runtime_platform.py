import shutil

from suite.support import (
    ORACLE_REJECTED,
    ORACLE_UNSUPPORTED,
    build_with,
    compile_source,
    fresh,
    oracle_command,
    rel,
    repo,
    run_module,
    source,
    sources,
    wasm_file,
    wasm_text,
    wasmtime,
)

# The oracle checks its selected library's runtime contracts. Choosing a
# custom prelude does not make arbitrary declarations valid primitives.


def refused(files, message):
    dir = sources(files)
    prelude = f"{dir}/lib/prelude.moss"
    entry = f"{dir}/main.moss"
    out = oracle_command(["--prelude", prelude, "--elab-only", entry])
    assert out.code == ORACLE_REJECTED, f"{out.stderr}"
    assert message in out.stderr, f"{out.stderr}"


def test_primitive_arity_and_types_are_checked():
    for declaration in [
        "fn i32_add(x: ()): (); context Wasm = i32_add;",
        "type I32; assume I32 { fn i32_add(x: I32, y: I32): (); } context Wasm = I32, i32_add;",
    ]:
        refused(
            [
                ("lib/wasm.moss", declaration),
                ("lib/prelude.moss", 'import "./wasm.moss" use Wasm;'),
                ("main.moss", "assume Wasm { fn main() {} }"),
            ],
            "platform primitive has an incompatible declared signature",
        )


def test_main_requires_implemented_platform_operations_even_when_unused():
    dir = sources(
        [
            ("lib/wasm.moss", "fn probe();"),
            ("lib/prelude.moss", ""),
            ("main.moss", 'import "./lib/wasm.moss" use probe; assume probe { fn main() {} }'),
        ]
    )
    prelude = f"{dir}/lib/prelude.moss"
    entry = f"{dir}/main.moss"
    out = oracle_command(["--prelude", prelude, "--elab-only", entry])
    assert out.code == ORACLE_UNSUPPORTED, f"{out.stderr}"
    assert "main assumes" in out.stderr and "probe" in out.stderr, f"{out.stderr}"
    assert "which the reference host does not implement" in out.stderr, f"{out.stderr}"
    assert not out.stdout


def test_full_wasi_is_available_to_compiled_programs():
    dir = sources(
        [
            (
                "main.moss",
                """
            import "wasm.moss" as w use Wasm;
            import "wasip1.moss" use Wasi, random_get;
            assume Wasm, Wasi {
              fn main() { random_get(w::i32_zero, w::i32_one); }
            }
        """,
            ),
            (
                "unused.moss",
                """
            import "wasm.moss" use I32, Memory;
            import "wasip1.moss" use random_get;
            assume I32, Memory, random_get { fn main() {} }
        """,
            ),
        ]
    )
    entry = f"{dir}/main.moss"
    out = oracle_command(["--elab-only", entry])
    assert out.code == ORACLE_UNSUPPORTED, f"{out.stderr}"
    assert "which the reference host does not implement" in out.stderr, f"{out.stderr}"
    unused = f"{dir}/unused.moss"
    out = oracle_command(["--elab-only", unused])
    assert out.code == ORACLE_UNSUPPORTED, f"{out.stderr}"
    assert "random_get" in out.stderr, f"{out.stderr}"
    module = build_with(entry, "lib/prelude.moss")
    assert '"wasi_snapshot_preview1" "random_get"' in wasm_text(module)
    assert run_module(module, []) == ""


def test_platform_array_copy_uses_one_held_type():
    refused(
        [
            (
                "lib/wasm.moss",
                """type I32; type ArrayRef; type Arrayed; type Other;
                 assume ArrayRef, Arrayed { tag Array[Arrayed](ArrayRef); }
                 assume I32, ArrayRef, Arrayed, Other {
                   fn Array.copy(at: I32, from: Array[Arrayed = Other], start: I32, count: I32);
                 }
                 functor Arrays[Arrayed, Other]: I32, ArrayRef, Arrayed, Other -> Array.copy;
                 context Probe = I32, ArrayRef, Arrays;""",
            ),
            ("lib/prelude.moss", 'import "./wasm.moss" use Probe;'),
            ("main.moss", "assume Probe { fn main() {} }"),
        ],
        "platform primitive has an incompatible declared signature",
    )


def test_machine_constants_have_machine_types():
    refused(
        [
            ("lib/wasm.moss", "val i32_one: (); context Wasm = i32_one;"),
            ("lib/prelude.moss", 'import "./wasm.moss" use Wasm;'),
            ("main.moss", "assume Wasm { fn main() {} }"),
        ],
        "platform I32 constant has an incompatible declared type",
    )


def test_machine_scalar_names_must_be_distinct_abstract_types():
    refused(
        [
            ("lib/wasm.moss", "tag I32;"),
            ("lib/prelude.moss", 'import "./wasm.moss" as w;'),
            ("main.moss", "fn main() {}"),
        ],
        "must be a distinct abstract type",
    )


def test_bool_payload_has_exactly_its_two_units():
    refused(
        [
            ("lib/bool.moss", "tag Bool { value: () }; tag True; tag False;"),
            ("lib/prelude.moss", 'import "./bool.moss" use Bool, True, False;'),
            ("main.moss", "fn main() { if (Bool { value = () }) {} else {} }"),
        ],
        "platform Bool must contain exactly True and False",
    )


def test_special_reference_tags_have_their_declared_handle_payloads():
    for declaration, expected in [
        (
            "type StructRef; type Boxed; assume Boxed { tag Ref[Boxed](()); }",
            "platform tag `Ref` has an incompatible payload",
        ),
        (
            "type ArrayRef; type Arrayed; assume Arrayed { tag Array[Arrayed](()); }",
            "platform tag `Array` has an incompatible payload",
        ),
        (
            "type I32; type Memory; assume Memory { tag Addr[Memory](()); }",
            "platform tag `Addr` has an incompatible payload",
        ),
    ]:
        refused(
            [
                ("lib/wasm.moss", declaration),
                ("lib/prelude.moss", 'import "./wasm.moss" as w;'),
                ("main.moss", "fn main() {}"),
            ],
            expected,
        )


def test_generated_gc_primitive_bindings_are_checked():
    refused(
        [
            (
                "lib/wasm.moss",
                """type Boxed; type StructRef;
                 assume Boxed { tag Ref[Boxed](StructRef); fn .boxed(x: ()): Ref; }
                 functor Heap[Boxed]: Boxed, StructRef -> Boxed.boxed;
                 context Gc = Heap;""",
            ),
            ("lib/prelude.moss", 'import "./wasm.moss" as w;'),
            ("main.moss", "fn main() {}"),
        ],
        "platform primitive has an incompatible declared signature",
    )


def test_a_well_shaped_custom_platform_still_runs():
    dir = sources(
        [
            (
                "lib/wasm.moss",
                """type I32;
             assume I32 { val i32_one: I32; fn i32_add(x: I32, y: I32): I32; }
             context Wasm = I32, i32_one, i32_add;""",
            ),
            ("lib/prelude.moss", 'import "./wasm.moss" use Wasm, i32_one, i32_add;'),
            ("main.moss", "assume Wasm { fn main() { i32_add(i32_one, i32_one); } }"),
        ]
    )
    prelude = f"{dir}/lib/prelude.moss"
    entry = f"{dir}/main.moss"
    out = oracle_command(["--prelude", prelude, entry])
    assert out.code == 0, f"{out.stderr}"


def copy_library():
    library = fresh("library") / "lib"
    library.mkdir(parents=True)
    for module in (repo() / "lib").glob("*.moss"):
        shutil.copyfile(module, library / module.name)
    return rel(library)


def cast_probe(decls, body, traps):
    lib = copy_library()
    entry = f"{lib}/cast_probe.moss"
    (repo() / entry).write_text(
        f"""import "./wasm.moss" as w use Wasm, Gc, Boxed, Arrayed, StructRef, .boxed, .arrayed;
assume Wasm, Gc {{ {decls} fn main() {{ {body} }} }}""",
    )
    bytes = build_with(entry, f"{lib}/prelude.moss")
    out = wasmtime(wasm_file(bytes), None, [])
    if traps:
        assert "cast failure" in out.stderr, f"{out.code} {out.stderr}"
    else:
        assert out.code == 0, f"{out.stderr}"
    reference = oracle_command(["--prelude", f"{lib}/prelude.moss", entry])
    if traps:
        assert reference.code == 134, f"{reference.stderr}"
    else:
        assert reference.code == 0, f"{reference.stderr}"


def test_a_precise_wasm_reference_requires_a_base_operation():
    dir = sources(
        [
            (
                "main.moss",
                """
        import "wasm.moss" as w use Wasm, Gc, Boxed, .boxed;
        assume Wasm, Gc {
          fn main() {
            bind Boxed = w::I32;
            bind w::Heap;
            let value = w::i32_one.boxed();
            let raw = match value { w::Ref(handle) => handle };
            w::Ref(raw);
          }
        }
    """,
            )
        ]
    )
    entry = f"{dir}/main.moss"
    reference = oracle_command(["--elab-only", entry])
    assert reference.code == ORACLE_REJECTED, f"{reference.stderr}"
    assert "a Wasm reference must be made by Base" in reference.stderr
    assert not (compile_source(entry, "lib/prelude.moss").is_module())


def test_checked_cast_projection_identity():
    cast_probe(
        "tag A(w::I32); tag B(w::I32);",
        "bind Boxed=A; bind w::Heap; let r=A(w::i32_one).boxed(); let h=match r {w::Ref(h)=>h}; bind Boxed=B; let b=w::StructCast(h).cast(); b.deref();",
        True,
    )
    cast_probe(
        "type A=w::I32; type B=w::I32;",
        "bind Boxed=A; bind w::Heap; let r=w::i32_one.boxed(); let h=match r {w::Ref(h)=>h}; bind Boxed=B; let b=w::StructCast(h).cast(); b.deref();",
        False,
    )
    cast_probe(
        "tag A(w::I32);",
        "bind Boxed=A; bind w::Heap; let r=A(w::i32_one).boxed(); let h=match r {w::Ref(h)=>h}; let b=w::StructCast(h).cast(); b.deref();",
        False,
    )
    cast_probe(
        "tag A(w::I32); tag B(w::I32);",
        "bind Arrayed=A; bind w::Arrays; let r=A(w::i32_one).arrayed(w::i32_one); let h=match r {w::Array(h)=>h}; bind Arrayed=B; let b=w::ArrayCast(h).cast(); b.get(w::i32_zero);",
        True,
    )
    cast_probe(
        "tag A {x:w::I32,y:w::I32}; tag B {x:w::I32,y:w::I32};",
        "bind Arrayed=A; bind w::Arrays; let r=A{x=w::i32_one,y=w::i32_one}.arrayed(w::i32_one); let h=match r {w::Array(h)=>h}; bind Arrayed=B; let b=w::ArrayCast(h).cast(); b.get(w::i32_zero);",
        True,
    )
    cast_probe(
        "",
        "bind w::StructRef=w::I32; bind Boxed=w::F64; let r=w::Ref(w::i32_one); let a=match r {w::Ref(a)=>a}; w::i32_add(a,a);",
        False,
    )
    cast_probe(
        "",
        "bind Boxed=w::I32; bind w::Heap; let inner=w::i32_one.boxed(); bind Boxed=w::Ref[Boxed=w::I32]; bind w::Heap; let outer=inner.boxed(); let again=outer.deref(); again.deref();",
        False,
    )
    cast_probe(
        "assume Boxed { fn make(x:Boxed):w::Ref {bind w::Heap; x.boxed()} }",
        "bind Boxed=w::I32; let r=make(w::i32_one); let h=match r {w::Ref(h)=>h}; let b=w::StructCast(h).cast(); b.deref();",
        False,
    )
    cast_probe(
        "type T; assume T {tag A[T](T);}",
        "bind T=w::I32; bind Boxed=A[T=w::I32]; bind w::Heap; let r=A(w::i32_one).boxed(); let h=match r {w::Ref(h)=>h}; bind Boxed=A[T=w::I32]; let b=w::StructCast(h).cast(); b.deref();",
        False,
    )
    # A provider retains the allocation type from the point where it was bound.
    cast_probe(
        "",
        "bind Boxed=w::I32; bind w::Heap; bind Boxed=w::F64; let r=w::i32_one.boxed(); let h=match r {w::Ref(h)=>h}; bind Boxed=w::I32; let b=w::StructCast(h).cast(); b.deref();",
        False,
    )
    cast_probe(
        "",
        "bind Arrayed=w::I32; bind w::Arrays; bind Arrayed=w::F64; let r=w::i32_one.arrayed(w::i32_one); let h=match r {w::Array(h)=>h}; bind Arrayed=w::I32; let b=w::ArrayCast(h).cast(); b.get(w::i32_zero);",
        False,
    )
    cast_probe(
        "type Fresh; assume Fresh {val fresh:Fresh;} functor F[]:Wasm -> Fresh,fresh; functor Make:Wasm -> Fresh,fresh {bind Fresh=w::I32; bind fresh=w::i32_one;}",
        "bind F=Make; bind F; bind Boxed=Fresh; bind w::Heap; let r=fresh.boxed(); let h=match r {w::Ref(h)=>h}; bind Boxed=w::I32; let b=w::StructCast(h).cast(); b.deref();",
        False,
    )


def test_copied_functor_retains_private_captured_type():
    decls = "type T; assume StructRef {val raw:StructRef;} fn note(); functor F[]:Wasm -> note; functor H[]:Wasm,T -> note; functor G:Wasm,Gc,T,raw -> note {bind note=()=>{let r=w::StructCast[Boxed=T](raw).cast();};}"
    body = "bind T=w::I32; bind Boxed=w::I32; bind w::Heap; let r=w::i32_one.boxed(); bind raw=match r {w::Ref(h)=>h}; bind F=G; bind H=F; bind T=w::F64; bind H; note();"
    cast_probe(decls, body.replace("bind T=w::F64;", ""), False)
    cast_probe(decls, body.replace("bind F=G; bind H=F;", "bind H=G;"), True)
    cast_probe(decls, body, False)


def test_rebound_array_handle_is_an_ordinary_wrapper():
    cast_probe(
        "",
        "bind w::ArrayRef=w::I32; bind Arrayed=w::I32; let a=w::Array(w::i32_one); let raw=match a {w::Array(raw)=>raw}; w::i32_add(raw,raw);",
        False,
    )
    cast_probe(
        "type ScalarAlias=w::I32;",
        "bind w::ArrayRef=ScalarAlias; bind Arrayed=w::I32; let a=w::Array(w::i32_one); let raw=match a {w::Array(raw)=>raw}; w::i32_add(raw,raw);",
        False,
    )
    cast_probe(
        "tag Scalar(w::I32);",
        "bind w::ArrayRef=Scalar; bind Arrayed=w::I32; let a=w::Array(Scalar(w::i32_one)); let raw=match a {w::Array(raw)=>raw}; let value=match raw {Scalar(value)=>value}; w::i32_add(value,value);",
        False,
    )


def test_entry_signature_matches_native_zero_lane_abi():
    for text, message in [
        ("fn main(x: ()): () { x }", "main must take no parameters"),
        ("fn main(x: ()) {}", "main must take no parameters"),
        ("tag U; fn main(): U { U }", "main must return a type with no runtime lanes"),
        (
            "tag U; tag Z {x: U}; fn main(): Z { Z{x=U} }",
            "main must return a type with no runtime lanes",
        ),
        (
            "tag U; tag V; type R=U|V; fn main(): R { U }",
            "main must return a type with no runtime lanes",
        ),
    ]:
        entry = source(text)
        native = compile_source(entry, "")
        assert not (native.is_module()), f"native accepted {text}"
        reference = oracle_command(["--prelude", "", "--elab-only", entry])
        assert reference.code == ORACLE_REJECTED, f"{reference.stderr}"
        assert message in reference.stderr, f"{reference.stderr}"
    for text in [
        "fn main() {}",
        "type Z=(); fn main(): Z { () }",
        "tag Z(()); fn main(): Z { Z(()) }",
        "tag Z {}; fn main(): Z { Z{} }",
        "tag Z {x: ()}; fn main(): Z { Z{x=()} }",
    ]:
        entry = source(text)
        bytes = build_with(entry, "")
        assert wasmtime(wasm_file(bytes), None, []).code == 0
        reference = oracle_command(["--prelude", "", entry])
        assert reference.code == 0, f"{reference.stderr}"
    # Never also erases; validate acceptance without executing the loop.
    entry = source("fn main(): (|) { loop {} }")
    _ = build_with(entry, "")
    reference = oracle_command(["--prelude", "", "--elab-only", entry])
    assert reference.code == 0, f"{reference.stderr}"


def test_output_generation_overrides_old_input_merge():
    # Merge equates the input generations T and U. Providing a new T
    # must not carry that old equation into the type of a new result.
    for declaration, result, bad, good in [
        (
            "assume T {val result:T;}",
            "result",
            "bind result=old; bind T=New;",
            "bind T=New; bind result=New;",
        ),
        (
            "assume T {fn result():T;}",
            "result",
            "bind result=()=>old; bind T=New;",
            "bind T=New; bind result=()=>New;",
        ),
        (
            "tag R; assume T {fn R.result():T;}",
            "R.result",
            "bind R.result=()=>old; bind T=New;",
            "bind T=New; bind R.result=()=>New;",
        ),
    ]:

        def program(body):
            return f"type T; type U; type P; tag Old; tag New; assume P {{val seed:P;}} context Merge=T,U,seed[P=T],seed[P=U]; assume U {{val old:U;}} {declaration} functor F[]:Merge,old->T,{result}; functor G:Merge,old->T,{result} {{{body}}} fn main() {{bind T=Old; bind U=Old; bind P=Old; bind seed=Old; bind old=Old; bind F=G; bind F;}}"

        bad_entry = source(program(bad))
        assert not (compile_source(bad_entry, "").is_module())
        rejected = oracle_command(["--prelude", "", "--elab-only", bad_entry])
        assert rejected.code == ORACLE_REJECTED, f"{rejected.stderr}"
        assert "another type" in rejected.stderr, f"{rejected.stderr}"
        good_entry = source(program(good))
        bytes = build_with(good_entry, "")
        assert wasmtime(wasm_file(bytes), None, []).code == 0
        accepted = oracle_command(["--prelude", "", good_entry])
        assert accepted.code == 0, f"{accepted.stderr}"
    # A result which explicitly reads the *other input* U stays old U.
    entry = source(
        "type T; type U; type P; tag Old; tag New; assume P {val seed:P;} context Merge=T,U,seed[P=T],seed[P=U]; assume U {val old:U; val result:U;} functor F[]:Merge,old->T,result; functor G:Merge,old->T,result {bind result=old; bind T=New;} fn main(){bind T=Old; bind U=Old; bind P=Old; bind seed=Old; bind old=Old; bind F=G; bind F; match result {Old=>()};}"
    )
    bytes = build_with(entry, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", entry])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_returned_functor_preserves_fixed_capture():
    def program(body):
        return f"type T; tag Old; tag New; assume T {{val x:T; fn consume(v:T);}} functor Inner[]:->x; functor Provider:T,x->x {{bind x=x;}} functor Outer[]:T,x->T,Inner,consume; functor Actual:T,x->T,Inner,consume {{{body} bind consume=(v)=>match v {{New=>()}};}} fn main(){{bind T=Old;bind x=Old;bind Outer=Actual;bind Outer; bind Inner;consume(x);}}"

    bad = source(program("bind Inner=Provider;bind T=New;"))
    assert not (compile_source(bad, "").is_module())
    refused = oracle_command(["--prelude", "", "--elab-only", bad])
    assert refused.code == ORACLE_REJECTED, f"{refused.stderr}"
    assert "fixed interpretation" in refused.stderr, f"{refused.stderr}"
    good = source(program("bind T=New;bind x=New;bind Inner=Provider;"))
    bytes = build_with(good, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", good])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_platform_bindings_check_the_merged_entry_view():
    for body in [
        "tag U; context Bad=Wasm,w::i32_one[w::I32=U]; assume Bad {fn main(){match w::i32_one{U=>()};}}",
        "tag U; context Bad=Wasm,w::i32_add[w::I32=U]; assume Bad {fn main(){w::i32_add(U,U);}}",
        'import "./wasip1.moss" as p use WasiCore; tag U; context Bad=w::Memory,WasiCore,p::fd_write[w::I32=U,w::Memory=w::Memory]; assume Bad {fn main(){p::fd_write(U,U,U,U);}}',
        "type X; context Bad=X,Wasm,w::i32_add[w::I32=X],w::i32_add[w::I32=w::F64]; assume Bad {fn main(){}}",
    ]:
        lib = copy_library()
        entry = f"{lib}/entry_view.moss"
        (repo() / entry).write_text(f'import "./wasm.moss" as w use Wasm;\n{body}')
        prelude = f"{lib}/prelude.moss"
        assert not (compile_source(entry, prelude).is_module())
        checked = oracle_command(["--prelude", prelude, "--elab-only", entry])
        assert checked.code == ORACLE_REJECTED, f"{checked.stderr}"
        assert "its context merged" in checked.stderr, f"{checked.stderr}"


def test_entry_requires_every_assumed_type_and_operation():
    for program, code, message in [
        ("type T; assume T {fn main() {}}", ORACLE_REJECTED, "needs `T` in force"),
        ("fn f(); assume f {fn main() {}}", ORACLE_UNSUPPORTED, "main assumes `f`"),
    ]:
        entry = source(program)
        assert not (compile_source(entry, "").is_module())
        reference = oracle_command(["--prelude", "", "--elab-only", entry])
        assert reference.code == code, f"{reference.stderr}"
        assert message in reference.stderr, f"{reference.stderr}"


def test_fixed_platform_content_can_be_a_user_type():
    lib = copy_library()
    wasm = (repo() / lib) / "wasm.moss"
    declarations = wasm.read_text().replace("functor Heap[Boxed]", "functor Heap[]")
    wasm.write_text(
        f"""{declarations}
type P; assume I32, P {{fn proc_exit(code: I32);}}
""",
    )
    entry = f"{lib}/fixed_content.moss"
    (repo() / entry).write_text(
        """
import "./wasm.moss" as w use Wasm, .boxed;
tag Held(w::I32);
context HeldHeap=Wasm,w::Boxed,w::proc_exit[w::P=w::Boxed],w::proc_exit[w::P=Held],w::Heap;
assume HeldHeap {
  fn main() {
    bind w::Heap;
    let r=Held(w::i32_one).boxed();
    match r.deref() {Held(payload)=>()};
  }
}
""",
    )
    prelude = f"{lib}/prelude.moss"
    bytes = build_with(entry, prelude)
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    reference = oracle_command(["--prelude", prelude, entry])
    assert reference.code == 0, f"{reference.stderr}"


def test_copied_functors_compare_final_output_interpretations():
    def program(target, matched):
        return f"type P;tag U;tag V;assume P {{val out:P;}} functor F[]:->{target};functor H[]:->out[P=V],out[P=U]; functor G:->out[P=V],out[P=U]{{bind P=U;bind out=U;}} fn main(){{bind H=G;bind F=H;bind F;match out{{{matched}=>()}};}}"

    bad = source(program("out[P=U],out[P=V]", "V"))
    assert not (compile_source(bad, "").is_module())
    rejected = oracle_command(["--prelude", "", "--elab-only", bad])
    assert rejected.code == ORACLE_REJECTED, f"{rejected.stderr}"
    assert "effective output interpretation" in rejected.stderr, f"{rejected.stderr}"
    good = source(program("out[P=V],out[P=U]", "U"))
    bytes = build_with(good, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", good])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_copied_functors_keep_fixed_inputs_current_and_available():
    def program(target, provider, binds):
        return f"type T;tag Old;tag New;assume T {{fn out():T;fn make():T;}} fn old():Old{{Old}} functor F{target}:T,make->out;functor H{provider}:T,make->out; functor G:T,make->out{{bind out=make;}}fn main(){{{binds}}}"

    for target, provider, binds, message in [
        ("[]", "", "bind H=G;bind F=H;bind T=Old;bind make=old;bind F;", "fixed input"),
        (
            "[]",
            "[]",
            "bind T=Old;bind make=old;bind H=G;bind T=New;bind F=H;",
            "rebinding its fixed input",
        ),
        ("[T]", "[]", "bind T=Old;bind make=old;bind H=G;bind F=H;", "polymorphic"),
    ]:
        bad = source(program(target, provider, binds))
        assert not (compile_source(bad, "").is_module())
        rejected = oracle_command(["--prelude", "", "--elab-only", bad])
        assert rejected.code == ORACLE_REJECTED, f"{rejected.stderr}"
        assert message in rejected.stderr, f"{rejected.stderr}"
    good = source(
        program("[]", "", "bind H=G;bind T=Old;bind F=H;bind make=old;bind F;match out(){Old=>()};")
    )
    bytes = build_with(good, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", good])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_result_brackets_reach_native_value_types_through_contexts():
    for result in ["out[P=V]", "Outputs"]:
        entry = source(
            f"type P;tag U;tag V;assume P {{val out:P;}}context Outputs=out[P=V]; functor F[]:->{result};functor G:->{result}{{bind P=V;bind out=V;}} fn main(){{bind P=U;bind F=G;bind F;match out{{V=>()}};}}"
        )
        bytes = build_with(entry, "")
        assert wasmtime(wasm_file(bytes), None, []).code == 0
        accepted = oracle_command(["--prelude", "", entry])
        assert accepted.code == 0, f"{accepted.stderr}"


def test_new_result_method_receiver_escapes_old_input_merge():
    entry = source(
        "type T;type U;type P;tag Old;tag New;assume P{val seed:P;} context Merge=T,U,seed[P=T],seed[P=U];fn T.result(); functor F[]:Merge->T,T.result; functor G:Merge->T,T.result{bind T=New;bind T.result=()=>();} fn main(){bind T=Old;bind U=Old;bind P=Old;bind seed=Old;bind F=G;bind F;}"
    )
    bytes = build_with(entry, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", entry])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_copied_input_merge_must_hold_for_every_target_application():
    def program(parameters):
        return f"type P;type T;type U;tag Old;assume P{{val v:P;}} assume T,U{{fn out(a:T,b:U);}} functor F{parameters}:T,U,v[P=T]->out; functor H{parameters}:T,U,v[P=T],v[P=U]->out; functor G:T,U,v[P=T],v[P=U]->out{{bind out=(a,b)=>();}} fn main(){{bind T=Old;bind U=Old;bind P=Old;bind v=Old;bind H=G;bind F=H;}}"

    # Current equality of T and U cannot promise their equality in a
    # future polymorphic application of F.
    bad = source(program("[T,U]"))
    assert not (compile_source(bad, "").is_module())
    rejected = oracle_command(["--prelude", "", "--elab-only", bad])
    assert rejected.code == ORACLE_REJECTED, f"{rejected.stderr}"
    assert "input interpretation" in rejected.stderr, f"{rejected.stderr}"
    good = source(program("[]"))
    bytes = build_with(good, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", good])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_result_leaf_identity_expands_contexts_and_skips_concrete_items():
    entry = source(
        "tag Skip;type Alias=Skip;val constant:Skip=Skip; fn helper():Skip{Skip}fn out(); context Inner=out;context Outputs=Inner; functor F[]:->Outputs,Skip,Alias,constant,helper;functor H[]:->out; functor G:->out{bind out=()=>();} fn main(){bind H=G;bind F=H;bind F;out();}"
    )
    bytes = build_with(entry, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", entry])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_defined_functor_inputs_hold_for_every_target_application():
    def program(parameters):
        return f"type T;type P;tag Old;tag New;assume P{{val seed:P;}}assume T{{val x:T;}} fn out();functor F{parameters}:T,seed[P=T],x->out; functor G:T,seed[P=T],seed[P=Old],x->out{{bind out=()=>();}} fn main(){{bind T=Old;bind P=Old;bind seed=Old;bind x=Old;bind F=G;bind F;out();}}"

    bad = source(program("[T]"))
    # A body that ignores G's extra equation can compile natively;
    # the source guard enforces G's full declared input interpretation.
    assert compile_source(bad, "").is_module()
    rejected = oracle_command(["--prelude", "", "--elab-only", bad])
    assert rejected.code == ORACLE_REJECTED, f"{rejected.stderr}"
    good = source(program("[]").replace("F[]:T,seed[P=T],x", "F[]:T,seed[P=T],seed[P=Old],x"))
    bytes = build_with(good, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", good])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_captured_functor_fixed_types_cannot_depend_on_target_polymorphism():
    def program(parameters):
        return f"type T;tag Old;assume T{{val old:T;fn out():T;}} functor J[]:T->out;functor MakeJ:T,old->out{{bind out=()=>old;}} functor F{parameters}:T->out;functor G:T,J->out{{bind J;}} fn main(){{bind T=Old;bind old=Old;bind J=MakeJ;bind F=G;bind F;match out(){{Old=>()}};}}"

    bad = source(program("[T]"))
    assert not (compile_source(bad, "").is_module())
    rejected = oracle_command(["--prelude", "", "--elab-only", bad])
    assert rejected.code == ORACLE_REJECTED, f"{rejected.stderr}"
    good = source(program("[]"))
    bytes = build_with(good, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", good])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_constant_initializer_keeps_own_type_scope():
    # The cached lambda annotation has no type arguments. The reader's
    # extra T binding belongs to the caller, outside the initializer.
    entry = source("type T; tag A; fn q(); val v:()={bind q=()=>();}; fn main(){bind T=A;v;v;}")
    accepted = oracle_command(["--prelude", "", entry])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_captured_values_keep_their_frozen_types_at_functor_binding():
    def program(old_capture):
        return "type T;tag Old;tag New;assume T{{val x:T;fn consume(v:T);fn q();}} functor F[]:T,consume->q;functor G:T,x,consume->q{{bind q=()=>consume(x);}} fn main(){{bind T={};bind x={};bind T=New; bind consume=(v)=>match v{{New=>()}};bind F=G;bind F;q();}}".format(
            ("Old" if old_capture else "New"), ("Old" if old_capture else "New")
        )

    bad = source(program(True))
    assert not (compile_source(bad, "").is_module())
    rejected = oracle_command(["--prelude", "", "--elab-only", bad])
    assert rejected.code == ORACLE_REJECTED, f"{rejected.stderr}"
    assert "frozen interpretation" in rejected.stderr, f"{rejected.stderr}"
    good = source(program(False))
    bytes = build_with(good, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", good])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_captured_functors_keep_their_frozen_fixed_interpretations():
    def program(rebind):
        return f"type T;tag Old;tag New;assume T{{val old:T;fn out():T;}} functor J[]:T->out;functor MakeJ:T,old->out{{bind out=()=>old;}} functor F[]:T->out;functor G:T,J->out{{bind J;}} fn main(){{bind T=Old;bind old=Old;bind J=MakeJ;{rebind}bind F=G;bind F;}}"

    bad = source(program("bind T=New;"))
    assert not (compile_source(bad, "").is_module())
    rejected = oracle_command(["--prelude", "", "--elab-only", bad])
    assert rejected.code == ORACLE_REJECTED, f"{rejected.stderr}"
    good = source(program(""))
    bytes = build_with(good, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", good])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_fixed_pins_use_the_actual_post_body_type_view():
    for output in ["Old", "New"]:
        entry = source(
            f"type T;type U;tag Old;tag New;assume U{{val x:U;}}assume T{{fn q():T;}} functor H[]:->U,x;functor P:->U,x{{bind U={output};bind x={output};}} functor F[]:T->q;functor G:T,U->q{{bind H=P;bind H;bind T=U;bind q=()=>x;}} fn main(){{bind T=Old;bind U=Old;bind F=G;bind F;match q(){{Old=>()}};}}"
        )
        assert not (compile_source(entry, "").is_module())
        rejected = oracle_command(["--prelude", "", "--elab-only", entry])
        assert rejected.code == ORACLE_REJECTED, f"{rejected.stderr}"
        assert "actual body" in rejected.stderr, f"{rejected.stderr}"
    good = source(
        "type T;tag Old;assume T{fn q():T;}functor F[]:T->q; functor G:T->q{bind T=Old;bind q=()=>Old;} fn main(){bind T=Old;bind F=G;bind F;match q(){Old=>()};}"
    )
    bytes = build_with(good, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", good])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_provider_and_constant_returns_cannot_escape_their_effect_boundary():
    for program in [
        "tag Old;tag New;val x:();functor F[]:->x;functor G:->x{bind x={return;()};} fn run():Old|New{bind F=G;bind F;Old}fn main(){match run(){Old=>(),New=>()};}",
        "tag Old;tag New;val x:();functor F[]:->x; functor Inner:->x{bind x={return;()};}functor G:->x{bind Inner;} fn run():Old|New{bind F=G;bind F;Old}fn main(){match run(){Old=>(),New=>()};}",
        "tag Old;tag New;val value:()={return;()};fn run():Old|New{value;Old} fn main(){match run(){Old=>(),New=>()};}",
    ]:
        bad = source(program)
        assert not (compile_source(bad, "").is_module())
        rejected = oracle_command(["--prelude", "", "--elab-only", bad])
        assert rejected.code == ORACLE_REJECTED, f"{rejected.stderr}"
        assert "cannot return" in rejected.stderr, f"{rejected.stderr}"
    good = source(
        "fn q();functor F[]:->q;functor G:->q{bind q=()=>{return;};} fn plain(){return;}fn annotated():(){return;} fn main(){bind F=G;bind F;q();plain();annotated();}"
    )
    bytes = build_with(good, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    accepted = oracle_command(["--prelude", "", good])
    assert accepted.code == 0, f"{accepted.stderr}"


def test_private_checked_casts_preserve_chained_fixed_pin_meanings():
    cast_probe(
        "type T;type U;type V;tag Old; assume StructRef{val raw:StructRef;} assume T{val private:w::Ref[Boxed=T];} assume T,U,V{fn q(a:T,b:U,c:V);} functor F[]:Wasm,Gc,T,U,V,raw->q; functor G:Wasm,Gc,T,U,V,raw->q{ bind private=w::StructCast[Boxed=T](raw).cast(); bind T=U;bind U=V;bind V=Old;bind q=(a,b,c)=>();}",
        "bind Boxed=Old;bind w::Heap; let handle=match Old.boxed(){w::Ref(handle)=>handle};bind raw=handle; bind T=Old;bind U=Old;bind V=Old;bind F=G;bind F;q(Old,Old,Old);",
        False,
    )


def test_opaque_nominal_arguments_do_not_force_their_unused_payloads():
    # `Bad`'s payload was an unformable one (`type Broken=Broken;`, an alias
    # cycle). Every declaration is now checked as written (criteria section
    # 7), and every tag must have a finite layout (D58), so no such payload
    # is left to write. The payloads are ordinary; what remains is that a
    # phantom argument's payload is not read, and the cast still traps.
    entry = source(
        "tag Bad(()); type T;assume T{tag Phantom[T](());}fn main(){Phantom[T=Bad](());}"
    )
    bytes = build_with(entry, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    reference = oracle_command(["--prelude", "", entry])
    assert reference.code == 0, f"{reference.stderr}"
    cast_probe(
        "tag Bad(());tag Other(()); type Element;assume Element{tag Phantom[Element](());}",
        "bind Boxed=Phantom[Element=Bad];bind w::Heap; let boxed=Phantom[Element=Bad](()).boxed(); let raw=match boxed{w::Ref(raw)=>raw}; bind Boxed=Phantom[Element=Other];w::StructCast(raw).cast().deref();",
        True,
    )


def test_latent_unformable_functor_capability_does_not_force_a_fake_interface():
    entry = source(
        "type T;tag U;assume T{tag Wrap[T](T);fn offered():Wrap|U;} functor Inner[T]:T->offered;val x:(); functor Outer[]:Inner->x;functor G:Inner->x{bind x=();} fn main(){bind Outer=G;}"
    )
    bytes = build_with(entry, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    reference = oracle_command(["--prelude", "", entry])
    assert reference.code == 0, f"{reference.stderr}"


def test_fresh_functor_output_generations_retain_their_runtime_types():
    entry = "tests/regressions/functor_output_generation_cast.moss"
    bytes = build_with(entry, "lib/prelude.moss")
    native = wasmtime(wasm_file(bytes), None, [])
    assert native.code == 0, f"{native.stderr}"
    # The proved passes keep Runs: the program runs alike with and without
    # them (`--debug-no-passes`).
    for options in [[][:], ["--debug-no-passes"][:]]:
        args = options
        args.append(entry)
        reference = oracle_command(args)
        assert reference.code == 0, f"{options!r}: {reference.stderr}"
