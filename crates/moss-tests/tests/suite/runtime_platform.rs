//! The oracle checks its selected library's runtime contracts. Choosing a
//! custom prelude does not make arbitrary declarations valid primitives.
use moss_tests::{oracle_command, sources, ORACLE_REJECTED, ORACLE_UNSUPPORTED};

fn refused(files: &[(&str, &str)], message: &str) {
    let dir = sources(files);
    let prelude = format!("{dir}/lib/prelude.moss");
    let entry = format!("{dir}/main.moss");
    let out = oracle_command(&["--prelude", &prelude, "--elab-only", &entry]);
    assert_eq!(out.code, ORACLE_REJECTED, "{}", out.stderr);
    assert!(out.stderr.contains(message), "{}", out.stderr);
}

#[test]
fn primitive_arity_and_types_are_checked() {
    for declaration in [
        "fn i32_add(x: ()): (); context Wasm = i32_add;",
        "type I32; assume I32 { fn i32_add(x: I32, y: I32): (); } context Wasm = I32, i32_add;",
    ] {
        refused(
            &[
                ("lib/wasm.moss", declaration),
                ("lib/prelude.moss", "import \"./wasm.moss\" use Wasm;"),
                ("main.moss", "assume Wasm { fn main() {} }"),
            ],
            "platform primitive has an incompatible declared signature",
        );
    }
}

#[test]
fn main_requires_implemented_platform_operations_even_when_unused() {
    let dir = sources(&[
        ("lib/wasm.moss", "fn probe();"),
        ("lib/prelude.moss", ""),
        ("main.moss", "import \"./lib/wasm.moss\" use probe; assume probe { fn main() {} }"),
    ]);
    let prelude = format!("{dir}/lib/prelude.moss");
    let entry = format!("{dir}/main.moss");
    let out = oracle_command(&["--prelude", &prelude, "--elab-only", &entry]);
    assert_eq!(out.code, ORACLE_UNSUPPORTED, "{}", out.stderr);
    assert!(out.stderr.contains("main assumes") && out.stderr.contains("probe"), "{}", out.stderr);
    assert!(out.stderr.contains("which the reference host does not implement"), "{}", out.stderr);
    assert!(out.stdout.is_empty());
}

#[test]
fn full_wasi_is_available_to_compiled_programs() {
    let dir = sources(&[
        ("main.moss", r#"
            import "wasm.moss" as w use Wasm;
            import "wasip1.moss" use Wasi, random_get;
            assume Wasm, Wasi {
              fn main() { random_get(w::i32_zero, w::i32_one); }
            }
        "#),
        ("unused.moss", r#"
            import "wasm.moss" use I32, Memory;
            import "wasip1.moss" use random_get;
            assume I32, Memory, random_get { fn main() {} }
        "#),
    ]);
    let entry = format!("{dir}/main.moss");
    let out = oracle_command(&["--elab-only", &entry]);
    assert_eq!(out.code, ORACLE_UNSUPPORTED, "{}", out.stderr);
    assert!(out.stderr.contains("which the reference host does not implement"), "{}", out.stderr);

    let unused = format!("{dir}/unused.moss");
    let out = oracle_command(&["--elab-only", &unused]);
    assert_eq!(out.code, ORACLE_UNSUPPORTED, "{}", out.stderr);
    assert!(out.stderr.contains("random_get"), "{}", out.stderr);

    let module = build_with(&entry, "lib/prelude.moss");
    assert!(wasm_text(&module).contains("\"wasi_snapshot_preview1\" \"random_get\""));
    assert_eq!(run_module(&module, &[]), "");
}

#[test]
fn platform_array_copy_uses_one_held_type() {
    refused(
        &[
            (
                "lib/wasm.moss",
                "type I32; type ArrayRef; type Arrayed; type Other;
                 assume ArrayRef, Arrayed { tag Array[Arrayed](ArrayRef); }
                 assume I32, ArrayRef, Arrayed, Other {
                   fn Array.copy(at: I32, from: Array[Arrayed = Other], start: I32, count: I32);
                 }
                 functor Arrays[Arrayed, Other]: I32, ArrayRef, Arrayed, Other -> Array.copy;
                 context Probe = I32, ArrayRef, Arrays;",
            ),
            ("lib/prelude.moss", "import \"./wasm.moss\" use Probe;"),
            ("main.moss", "assume Probe { fn main() {} }"),
        ],
        "platform primitive has an incompatible declared signature",
    );
}

#[test]
fn machine_constants_have_machine_types() {
    refused(
        &[
            ("lib/wasm.moss", "val i32_one: (); context Wasm = i32_one;"),
            ("lib/prelude.moss", "import \"./wasm.moss\" use Wasm;"),
            ("main.moss", "assume Wasm { fn main() {} }"),
        ],
        "platform I32 constant has an incompatible declared type",
    );
}

#[test]
fn machine_scalar_names_must_be_distinct_abstract_types() {
    refused(
        &[
            ("lib/wasm.moss", "tag I32;"),
            ("lib/prelude.moss", "import \"./wasm.moss\" as w;"),
            ("main.moss", "fn main() {}"),
        ],
        "must be a distinct abstract type",
    );
}

#[test]
fn bool_payload_has_exactly_its_two_units() {
    refused(
        &[
            ("lib/bool.moss", "tag Bool { value: () }; tag True; tag False;"),
            ("lib/prelude.moss", "import \"./bool.moss\" use Bool, True, False;"),
            ("main.moss", "fn main() { if (Bool { value = () }) {} else {} }"),
        ],
        "platform Bool must contain exactly True and False",
    );
}

#[test]
fn special_reference_tags_have_their_declared_handle_payloads() {
    for (declaration, expected) in [
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
    ] {
        refused(
            &[
                ("lib/wasm.moss", declaration),
                ("lib/prelude.moss", "import \"./wasm.moss\" as w;"),
                ("main.moss", "fn main() {}"),
            ],
            expected,
        );
    }
}

#[test]
fn generated_gc_primitive_bindings_are_checked() {
    refused(
        &[
            (
                "lib/wasm.moss",
                "type Boxed; type StructRef;
                 assume Boxed { tag Ref[Boxed](StructRef); fn .boxed(x: ()): Ref; }
                 functor Heap[Boxed]: Boxed, StructRef -> Boxed.boxed;
                 context Gc = Heap;",
            ),
            ("lib/prelude.moss", "import \"./wasm.moss\" as w;"),
            ("main.moss", "fn main() {}"),
        ],
        "platform primitive has an incompatible declared signature",
    );
}

#[test]
fn a_well_shaped_custom_platform_still_runs() {
    let dir = sources(&[
        (
            "lib/wasm.moss",
            "type I32;
             assume I32 { val i32_one: I32; fn i32_add(x: I32, y: I32): I32; }
             context Wasm = I32, i32_one, i32_add;",
        ),
        (
            "lib/prelude.moss",
            "import \"./wasm.moss\" use Wasm, i32_one, i32_add;",
        ),
        ("main.moss", "assume Wasm { fn main() { i32_add(i32_one, i32_one); } }"),
    ]);
    let prelude = format!("{dir}/lib/prelude.moss");
    let entry = format!("{dir}/main.moss");
    let out = oracle_command(&["--prelude", &prelude, &entry]);
    assert_eq!(out.code, 0, "{}", out.stderr);
}
use moss_tests::*;
use std::fs;
fn cast_probe(decls: &str, body: &str, traps: bool) {
    let dir = sources(&[]);
    let lib = format!("{dir}/lib");
    fs::create_dir_all(repo().join(&lib)).unwrap();
    for f in fs::read_dir(repo().join("lib")).unwrap() {
        let f = f.unwrap();
        if f.path().extension().is_some_and(|x| x == "moss") {
            fs::copy(f.path(), repo().join(&lib).join(f.file_name())).unwrap();
        }
    }
    let entry=format!("{lib}/cast_probe.moss");
    fs::write(repo().join(&entry),format!("import \"./wasm.moss\" as w use Wasm, Gc, Boxed, Arrayed, StructRef, .boxed, .arrayed;\nassume Wasm, Gc {{ {decls} fn main() {{ {body} }} }}")).unwrap();
    let bytes=build_with(&entry,&format!("{lib}/prelude.moss"));
    let out=wasmtime(&wasm_file(&bytes),None,&[]);
    if traps { assert!(out.stderr.contains("cast failure"),"{} {}",out.code,out.stderr); }
    else { assert_eq!(out.code,0,"{}",out.stderr); }
    let reference=oracle_command(&["--prelude", &format!("{lib}/prelude.moss"), &entry]);
    if traps { assert_eq!(reference.code,134,"{}",reference.stderr); }
    else { assert_eq!(reference.code,0,"{}",reference.stderr); }
}
#[test]
fn a_precise_wasm_reference_requires_a_base_operation() {
    let dir = sources(&[("main.moss", r#"
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
    "#)]);
    let entry = format!("{dir}/main.moss");
    let reference = oracle_command(&["--elab-only", &entry]);
    assert_eq!(reference.code, ORACLE_REJECTED, "{}", reference.stderr);
    assert!(reference.stderr.contains("a Wasm reference must be made by Base"));
    assert!(!compile(&entry, "lib/prelude.moss").is_module());
}

#[test]
fn checked_cast_projection_identity() {
    cast_probe("tag A(w::I32); tag B(w::I32);", "bind Boxed=A; bind w::Heap; let r=A(w::i32_one).boxed(); let h=match r {w::Ref(h)=>h}; bind Boxed=B; let b=w::StructCast(h).cast(); b.deref();",true);
    cast_probe("type A=w::I32; type B=w::I32;", "bind Boxed=A; bind w::Heap; let r=w::i32_one.boxed(); let h=match r {w::Ref(h)=>h}; bind Boxed=B; let b=w::StructCast(h).cast(); b.deref();",false);
    cast_probe("tag A(w::I32);", "bind Boxed=A; bind w::Heap; let r=A(w::i32_one).boxed(); let h=match r {w::Ref(h)=>h}; let b=w::StructCast(h).cast(); b.deref();",false);
    cast_probe("tag A(w::I32); tag B(w::I32);", "bind Arrayed=A; bind w::Arrays; let r=A(w::i32_one).arrayed(w::i32_one); let h=match r {w::Array(h)=>h}; bind Arrayed=B; let b=w::ArrayCast(h).cast(); b.get(w::i32_zero);",true);
    cast_probe("tag A {x:w::I32,y:w::I32}; tag B {x:w::I32,y:w::I32};", "bind Arrayed=A; bind w::Arrays; let r=A{x=w::i32_one,y=w::i32_one}.arrayed(w::i32_one); let h=match r {w::Array(h)=>h}; bind Arrayed=B; let b=w::ArrayCast(h).cast(); b.get(w::i32_zero);",true);
    cast_probe("", "bind w::StructRef=w::I32; bind Boxed=w::F64; let r=w::Ref(w::i32_one); let a=match r {w::Ref(a)=>a}; w::i32_add(a,a);",false);
    cast_probe("", "bind Boxed=w::I32; bind w::Heap; let inner=w::i32_one.boxed(); bind Boxed=w::Ref[Boxed=w::I32]; bind w::Heap; let outer=inner.boxed(); let again=outer.deref(); again.deref();",false);

    cast_probe("assume Boxed { fn make(x:Boxed):w::Ref {bind w::Heap; x.boxed()} }", "bind Boxed=w::I32; let r=make(w::i32_one); let h=match r {w::Ref(h)=>h}; let b=w::StructCast(h).cast(); b.deref();",false);
    cast_probe("type T; assume T {tag A[T](T);}", "bind T=w::I32; bind Boxed=A[T=w::I32]; bind w::Heap; let r=A(w::i32_one).boxed(); let h=match r {w::Ref(h)=>h}; bind Boxed=A[T=w::I32]; let b=w::StructCast(h).cast(); b.deref();",false);

    // A provider retains the allocation type from the point where it was bound.
    cast_probe("", "bind Boxed=w::I32; bind w::Heap; bind Boxed=w::F64; let r=w::i32_one.boxed(); let h=match r {w::Ref(h)=>h}; bind Boxed=w::I32; let b=w::StructCast(h).cast(); b.deref();", false);
    cast_probe("", "bind Arrayed=w::I32; bind w::Arrays; bind Arrayed=w::F64; let r=w::i32_one.arrayed(w::i32_one); let h=match r {w::Array(h)=>h}; bind Arrayed=w::I32; let b=w::ArrayCast(h).cast(); b.get(w::i32_zero);", false);

    cast_probe("type Fresh; assume Fresh {val fresh:Fresh;} functor F[]:Wasm -> Fresh,fresh; functor Make:Wasm -> Fresh,fresh {bind Fresh=w::I32; bind fresh=w::i32_one;}", "bind F=Make; bind F; bind Boxed=Fresh; bind w::Heap; let r=fresh.boxed(); let h=match r {w::Ref(h)=>h}; bind Boxed=w::I32; let b=w::StructCast(h).cast(); b.deref();", false);

}

#[test]
fn copied_functor_retains_private_captured_type() {
    let decls = "type T; assume StructRef {val raw:StructRef;} fn note(); functor F[]:Wasm -> note; functor H[]:Wasm,T -> note; functor G:Wasm,Gc,T,raw -> note {bind note=()=>{let r=w::StructCast[Boxed=T](raw).cast();};}";
    let body = "bind T=w::I32; bind Boxed=w::I32; bind w::Heap; let r=w::i32_one.boxed(); bind raw=match r {w::Ref(h)=>h}; bind F=G; bind H=F; bind T=w::F64; bind H; note();";
    cast_probe(decls, &body.replace("bind T=w::F64;", ""), false);
    cast_probe(decls, &body.replace("bind F=G; bind H=F;", "bind H=G;"), true);
    cast_probe(decls, body, false);
}

#[test]
fn rebound_array_handle_is_an_ordinary_wrapper() {
    cast_probe("", "bind w::ArrayRef=w::I32; bind Arrayed=w::I32; let a=w::Array(w::i32_one); let raw=match a {w::Array(raw)=>raw}; w::i32_add(raw,raw);", false);
    cast_probe("type ScalarAlias=w::I32;", "bind w::ArrayRef=ScalarAlias; bind Arrayed=w::I32; let a=w::Array(w::i32_one); let raw=match a {w::Array(raw)=>raw}; w::i32_add(raw,raw);", false);
    cast_probe("tag Scalar(w::I32);", "bind w::ArrayRef=Scalar; bind Arrayed=w::I32; let a=w::Array(Scalar(w::i32_one)); let raw=match a {w::Array(raw)=>raw}; let value=match raw {Scalar(value)=>value}; w::i32_add(value,value);", false);
}

#[test]
fn entry_signature_matches_native_zero_lane_abi() {
    for (text, message) in [
        ("fn main(x: ()): () { x }", "main must take no parameters"),
        ("fn main(x: ()) {}", "main must take no parameters"),
        ("tag U; fn main(): U { U }", "main must return a type with no runtime lanes"),
        ("tag U; tag Z {x: U}; fn main(): Z { Z{x=U} }", "main must return a type with no runtime lanes"),
        ("tag U; tag V; type R=U|V; fn main(): R { U }", "main must return a type with no runtime lanes"),
    ] {
        let entry = source(text);
        let native = compile(&entry, "");
        assert!(!native.is_module(), "native accepted {text}");
        let reference = oracle_command(&["--prelude", "", "--elab-only", &entry]);
        assert_eq!(reference.code, ORACLE_REJECTED, "{}", reference.stderr);
        assert!(reference.stderr.contains(message), "{}", reference.stderr);
    }
    for text in [
        "fn main() {}",
        "type Z=(); fn main(): Z { () }",
        "tag Z(()); fn main(): Z { Z(()) }",
        "tag Z {}; fn main(): Z { Z{} }",
        "tag Z {x: ()}; fn main(): Z { Z{x=()} }",
    ] {
        let entry = source(text);
        let bytes = build_with(&entry, "");
        assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
        let reference = oracle_command(&["--prelude", "", &entry]);
        assert_eq!(reference.code, 0, "{}", reference.stderr);
    }
    // Never also erases; validate acceptance without executing the loop.
    let entry = source("fn main(): (|) { loop {} }");
    let _ = build_with(&entry, "");
    let reference = oracle_command(&["--prelude", "", "--elab-only", &entry]);
    assert_eq!(reference.code, 0, "{}", reference.stderr);
}

#[test]
fn output_generation_overrides_old_input_merge() {
    // Merge equates the input generations T and U. Providing a new T
    // must not carry that old equation into the type of a new result.
    for (declaration, result, bad, good) in [
        ("assume T {val result:T;}", "result", "bind result=old; bind T=New;", "bind T=New; bind result=New;"),
        ("assume T {fn result():T;}", "result", "bind result=()=>old; bind T=New;", "bind T=New; bind result=()=>New;"),
        ("tag R; assume T {fn R.result():T;}", "R.result", "bind R.result=()=>old; bind T=New;", "bind T=New; bind R.result=()=>New;"),
    ] {
        let program = |body: &str| format!(
            "type T; type U; type P; tag Old; tag New; \
             assume P {{val seed:P;}} \
             context Merge=T,U,seed[P=T],seed[P=U]; \
             assume U {{val old:U;}} {declaration} \
             functor F[]:Merge,old->T,{result}; \
             functor G:Merge,old->T,{result} {{{body}}} \
             fn main() {{bind T=Old; bind U=Old; bind P=Old; \
             bind seed=Old; bind old=Old; bind F=G; bind F;}}"
        );
        let bad_entry = source(&program(bad));
        assert!(!compile(&bad_entry, "").is_module());
        let rejected = oracle_command(&["--prelude", "", "--elab-only", &bad_entry]);
        assert_eq!(rejected.code, ORACLE_REJECTED, "{}", rejected.stderr);
        assert!(rejected.stderr.contains("another type"), "{}", rejected.stderr);

        let good_entry = source(&program(good));
        let bytes = build_with(&good_entry, "");
        assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
        let accepted = oracle_command(&["--prelude", "", &good_entry]);
        assert_eq!(accepted.code, 0, "{}", accepted.stderr);
    }

    // A result which explicitly reads the *other input* U stays old U.
    let entry = source("type T; type U; type P; tag Old; tag New; \
        assume P {val seed:P;} context Merge=T,U,seed[P=T],seed[P=U]; \
        assume U {val old:U; val result:U;} \
        functor F[]:Merge,old->T,result; \
        functor G:Merge,old->T,result {bind result=old; bind T=New;} \
        fn main(){bind T=Old; bind U=Old; bind P=Old; bind seed=Old; \
        bind old=Old; bind F=G; bind F; match result {Old=>()};}");
    let bytes = build_with(&entry, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &entry]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn returned_functor_preserves_fixed_capture() {
    let program = |body: &str| format!(
        "type T; tag Old; tag New; \
         assume T {{val x:T; fn consume(v:T);}} \
         functor Inner[]:->x; \
         functor Provider:T,x->x {{bind x=x;}} \
         functor Outer[]:T,x->T,Inner,consume; \
         functor Actual:T,x->T,Inner,consume {{{body} \
           bind consume=(v)=>match v {{New=>()}};}} \
         fn main(){{bind T=Old;bind x=Old;bind Outer=Actual;bind Outer; \
           bind Inner;consume(x);}}"
    );
    let bad = source(&program("bind Inner=Provider;bind T=New;"));
    assert!(!compile(&bad, "").is_module());
    let refused = oracle_command(&["--prelude", "", "--elab-only", &bad]);
    assert_eq!(refused.code, ORACLE_REJECTED, "{}", refused.stderr);
    assert!(refused.stderr.contains("fixed interpretation"), "{}", refused.stderr);

    let good = source(&program("bind T=New;bind x=New;bind Inner=Provider;"));
    let bytes = build_with(&good, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &good]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn platform_bindings_check_the_merged_entry_view() {
    for body in [
        "tag U; context Bad=Wasm,w::i32_one[w::I32=U]; assume Bad {fn main(){match w::i32_one{U=>()};}}",
        "tag U; context Bad=Wasm,w::i32_add[w::I32=U]; assume Bad {fn main(){w::i32_add(U,U);}}",
        "import \"./wasip1.moss\" as p use WasiCore; tag U; context Bad=w::Memory,WasiCore,p::fd_write[w::I32=U,w::Memory=w::Memory]; assume Bad {fn main(){p::fd_write(U,U,U,U);}}",
        "type X; context Bad=X,Wasm,w::i32_add[w::I32=X],w::i32_add[w::I32=w::F64]; assume Bad {fn main(){}}",
    ] {
        let dir = sources(&[]);
        let lib = format!("{dir}/lib");
        fs::create_dir_all(repo().join(&lib)).unwrap();
        for file in fs::read_dir(repo().join("lib")).unwrap() {
            let file = file.unwrap();
            if file.path().extension().is_some_and(|x| x == "moss") {
                fs::copy(file.path(), repo().join(&lib).join(file.file_name())).unwrap();
            }
        }
        let entry = format!("{lib}/entry_view.moss");
        fs::write(repo().join(&entry), format!("import \"./wasm.moss\" as w use Wasm;\n{body}")).unwrap();
        let prelude = format!("{lib}/prelude.moss");
        assert!(!compile(&entry, &prelude).is_module());
        let checked = oracle_command(&["--prelude", &prelude, "--elab-only", &entry]);
        assert_eq!(checked.code, ORACLE_REJECTED, "{}", checked.stderr);
        assert!(checked.stderr.contains("its context merged"), "{}", checked.stderr);
    }
}

#[test]
fn entry_requires_every_assumed_type_and_operation() {
    for (program, code, message) in [
        ("type T; assume T {fn main() {}}", ORACLE_REJECTED, "needs `T` in force"),
        ("fn f(); assume f {fn main() {}}", ORACLE_UNSUPPORTED, "main assumes `f`"),
    ] {
        let entry = source(program);
        assert!(!compile(&entry, "").is_module());
        let reference = oracle_command(&["--prelude", "", "--elab-only", &entry]);
        assert_eq!(reference.code, code, "{}", reference.stderr);
        assert!(reference.stderr.contains(message), "{}", reference.stderr);
    }
}

#[test]
fn fixed_platform_content_can_be_a_user_type() {
    let dir = sources(&[]);
    let lib = format!("{dir}/lib");
    fs::create_dir_all(repo().join(&lib)).unwrap();
    for file in fs::read_dir(repo().join("lib")).unwrap() {
        let file = file.unwrap();
        if file.path().extension().is_some_and(|x| x == "moss") {
            fs::copy(file.path(), repo().join(&lib).join(file.file_name())).unwrap();
        }
    }
    let wasm = repo().join(&lib).join("wasm.moss");
    let declarations = fs::read_to_string(&wasm).unwrap().replace("functor Heap[Boxed]", "functor Heap[]");
    fs::write(&wasm, format!("{declarations}\ntype P; assume I32, P {{fn proc_exit(code: I32);}}\n")).unwrap();
    let entry = format!("{lib}/fixed_content.moss");
    fs::write(repo().join(&entry), r#"
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
"#).unwrap();
    let prelude = format!("{lib}/prelude.moss");
    let bytes = build_with(&entry, &prelude);
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let reference = oracle_command(&["--prelude", &prelude, &entry]);
    assert_eq!(reference.code, 0, "{}", reference.stderr);
}


#[test]
fn copied_functors_compare_final_output_interpretations() {
    let program = |target: &str, matched: &str| format!(
        "type P;tag U;tag V;assume P {{val out:P;}} \
         functor F[]:->{target};functor H[]:->out[P=V],out[P=U]; \
         functor G:->out[P=V],out[P=U]{{bind P=U;bind out=U;}} \
         fn main(){{bind H=G;bind F=H;bind F;match out{{{matched}=>()}};}}"
    );
    let bad = source(&program("out[P=U],out[P=V]", "V"));
    assert!(!compile(&bad, "").is_module());
    let rejected = oracle_command(&["--prelude", "", "--elab-only", &bad]);
    assert_eq!(rejected.code, ORACLE_REJECTED, "{}", rejected.stderr);
    assert!(rejected.stderr.contains("effective output interpretation"), "{}", rejected.stderr);
    let good = source(&program("out[P=V],out[P=U]", "U"));
    let bytes = build_with(&good, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &good]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn copied_functors_keep_fixed_inputs_current_and_available() {
    let program = |target: &str, provider: &str, binds: &str| format!(
        "type T;tag Old;tag New;assume T {{fn out():T;fn make():T;}} \
         fn old():Old{{Old}} \
         functor F{target}:T,make->out;functor H{provider}:T,make->out; \
         functor G:T,make->out{{bind out=make;}}fn main(){{{binds}}}"
    );
    for (target, provider, binds, message) in [
        ("[]", "", "bind H=G;bind F=H;bind T=Old;bind make=old;bind F;", "fixed input"),
        ("[]", "[]", "bind T=Old;bind make=old;bind H=G;bind T=New;bind F=H;", "rebinding its fixed input"),
        ("[T]", "[]", "bind T=Old;bind make=old;bind H=G;bind F=H;", "polymorphic"),
    ] {
        let bad = source(&program(target, provider, binds));
        assert!(!compile(&bad, "").is_module());
        let rejected = oracle_command(&["--prelude", "", "--elab-only", &bad]);
        assert_eq!(rejected.code, ORACLE_REJECTED, "{}", rejected.stderr);
        assert!(rejected.stderr.contains(message), "{}", rejected.stderr);
    }
    let good = source(&program("[]", "", "bind H=G;bind T=Old;bind F=H;bind make=old;bind F;match out(){Old=>()};"));
    let bytes = build_with(&good, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &good]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn result_brackets_reach_native_value_types_through_contexts() {
    for result in ["out[P=V]", "Outputs"] {
        let entry = source(&format!(
            "type P;tag U;tag V;assume P {{val out:P;}}context Outputs=out[P=V]; \
             functor F[]:->{result};functor G:->{result}{{bind P=V;bind out=V;}} \
             fn main(){{bind P=U;bind F=G;bind F;match out{{V=>()}};}}"
        ));
        let bytes = build_with(&entry, "");
        assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
        let accepted = oracle_command(&["--prelude", "", &entry]);
        assert_eq!(accepted.code, 0, "{}", accepted.stderr);
    }
}

#[test]
fn new_result_method_receiver_escapes_old_input_merge() {
    let entry = source("type T;type U;type P;tag Old;tag New;assume P{val seed:P;} \
        context Merge=T,U,seed[P=T],seed[P=U];fn T.result(); \
        functor F[]:Merge->T,T.result; \
        functor G:Merge->T,T.result{bind T=New;bind T.result=()=>();} \
        fn main(){bind T=Old;bind U=Old;bind P=Old;bind seed=Old;bind F=G;bind F;}");
    let bytes = build_with(&entry, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &entry]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn copied_input_merge_must_hold_for_every_target_application() {
    let program = |parameters: &str| format!(
        "type P;type T;type U;tag Old;assume P{{val v:P;}} \
         assume T,U{{fn out(a:T,b:U);}} \
         functor F{parameters}:T,U,v[P=T]->out; \
         functor H{parameters}:T,U,v[P=T],v[P=U]->out; \
         functor G:T,U,v[P=T],v[P=U]->out{{bind out=(a,b)=>();}} \
         fn main(){{bind T=Old;bind U=Old;bind P=Old;bind v=Old;bind H=G;bind F=H;}}"
    );
    // Current equality of T and U cannot promise their equality in a
    // future polymorphic application of F.
    let bad = source(&program("[T,U]"));
    assert!(!compile(&bad, "").is_module());
    let rejected = oracle_command(&["--prelude", "", "--elab-only", &bad]);
    assert_eq!(rejected.code, ORACLE_REJECTED, "{}", rejected.stderr);
    assert!(rejected.stderr.contains("input interpretation"), "{}", rejected.stderr);
    let good = source(&program("[]"));
    let bytes = build_with(&good, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &good]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}
#[test]
fn result_leaf_identity_expands_contexts_and_skips_concrete_items() {
    let entry = source("tag Skip;type Alias=Skip;val constant:Skip=Skip; \
        fn helper():Skip{Skip}fn out(); \
        context Inner=out,Skip,Alias,constant,helper;context Outputs=Inner; \
        functor F[]:->Outputs;functor H[]:->out; \
        functor G:->out{bind out=()=>();} \
        fn main(){bind H=G;bind F=H;bind F;out();}");
    let bytes = build_with(&entry, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &entry]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn defined_functor_inputs_hold_for_every_target_application() {
    let program = |parameters: &str| format!(
        "type T;type P;tag Old;tag New;assume P{{val seed:P;}}assume T{{val x:T;}} \
         fn out();functor F{parameters}:T,seed[P=T],x->out; \
         functor G:T,seed[P=T],seed[P=Old],x->out{{bind out=()=>();}} \
         fn main(){{bind T=Old;bind P=Old;bind seed=Old;bind x=Old;bind F=G;bind F;out();}}"
    );
    let bad = source(&program("[T]"));
    // A body that ignores G's extra equation can compile natively;
    // the source guard enforces G's full declared input interpretation.
    assert!(compile(&bad, "").is_module());
    let rejected = oracle_command(&["--prelude", "", "--elab-only", &bad]);
    assert_eq!(rejected.code, ORACLE_REJECTED, "{}", rejected.stderr);
    let good = source(&program("[]").replace("F[]:T,seed[P=T],x",
        "F[]:T,seed[P=T],seed[P=Old],x"));
    let bytes = build_with(&good, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &good]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn captured_functor_fixed_types_cannot_depend_on_target_polymorphism() {
    let program = |parameters: &str| format!(
        "type T;tag Old;assume T{{val old:T;fn out():T;}} \
         functor J[]:T->out;functor MakeJ:T,old->out{{bind out=()=>old;}} \
         functor F{parameters}:T->out;functor G:T,J->out{{bind J;}} \
         fn main(){{bind T=Old;bind old=Old;bind J=MakeJ;bind F=G;bind F;match out(){{Old=>()}};}}"
    );
    let bad = source(&program("[T]"));
    assert!(!compile(&bad, "").is_module());
    let rejected = oracle_command(&["--prelude", "", "--elab-only", &bad]);
    assert_eq!(rejected.code, ORACLE_REJECTED, "{}", rejected.stderr);
    let good = source(&program("[]"));
    let bytes = build_with(&good, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &good]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn constant_initializer_keeps_own_type_scope() {
    // The cached lambda annotation has no type arguments. The reader's
    // extra T binding belongs to the caller, outside the initializer.
    let entry = source("type T; tag A; fn q(); \
        val v:()={bind q=()=>();}; \
        fn main(){bind T=A;v;v;}");
    let accepted = oracle_command(&["--prelude", "", &entry]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn captured_values_keep_their_frozen_types_at_functor_binding() {
    let program = |old_capture: bool| format!(
        "type T;tag Old;tag New;assume T{{val x:T;fn consume(v:T);fn q();}} \
         functor F[]:T,consume->q;functor G:T,x,consume->q{{bind q=()=>consume(x);}} \
         fn main(){{bind T={};bind x={};bind T=New; \
         bind consume=(v)=>match v{{New=>()}};bind F=G;bind F;q();}}",
        if old_capture { "Old" } else { "New" },
        if old_capture { "Old" } else { "New" }
    );
    let bad = source(&program(true));
    assert!(!compile(&bad, "").is_module());
    let rejected = oracle_command(&["--prelude", "", "--elab-only", &bad]);
    assert_eq!(rejected.code, ORACLE_REJECTED, "{}", rejected.stderr);
    assert!(rejected.stderr.contains("frozen interpretation"), "{}", rejected.stderr);
    let good = source(&program(false));
    let bytes = build_with(&good, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &good]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn captured_functors_keep_their_frozen_fixed_interpretations() {
    let program = |rebind: &str| format!(
        "type T;tag Old;tag New;assume T{{val old:T;fn out():T;}} \
         functor J[]:T->out;functor MakeJ:T,old->out{{bind out=()=>old;}} \
         functor F[]:T->out;functor G:T,J->out{{bind J;}} \
         fn main(){{bind T=Old;bind old=Old;bind J=MakeJ;{rebind}bind F=G;bind F;}}"
    );
    let bad = source(&program("bind T=New;"));
    assert!(!compile(&bad, "").is_module());
    let rejected = oracle_command(&["--prelude", "", "--elab-only", &bad]);
    assert_eq!(rejected.code, ORACLE_REJECTED, "{}", rejected.stderr);
    let good = source(&program(""));
    let bytes = build_with(&good, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &good]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn fixed_pins_use_the_actual_post_body_type_view() {
    for output in ["Old", "New"] {
        let entry = source(&format!(
            "type T;type U;tag Old;tag New;assume U{{val x:U;}}assume T{{fn q():T;}} \
             functor H[]:->U,x;functor P:->U,x{{bind U={output};bind x={output};}} \
             functor F[]:T->q;functor G:T,U->q{{bind H=P;bind H;bind T=U;bind q=()=>x;}} \
             fn main(){{bind T=Old;bind U=Old;bind F=G;bind F;match q(){{Old=>()}};}}"
        ));
        assert!(!compile(&entry, "").is_module());
        let rejected = oracle_command(&["--prelude", "", "--elab-only", &entry]);
        assert_eq!(rejected.code, ORACLE_REJECTED, "{}", rejected.stderr);
        assert!(rejected.stderr.contains("actual body"), "{}", rejected.stderr);
    }
    let good = source("type T;tag Old;assume T{fn q():T;}functor F[]:T->q; \
        functor G:T->q{bind T=Old;bind q=()=>Old;} \
        fn main(){bind T=Old;bind F=G;bind F;match q(){Old=>()};}");
    let bytes = build_with(&good, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &good]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn provider_and_constant_returns_cannot_escape_their_effect_boundary() {
    for program in [
        "tag Old;tag New;val x:();functor F[]:->x;functor G:->x{bind x={return;()};} \
         fn run():Old|New{bind F=G;bind F;Old}fn main(){match run(){Old=>(),New=>()};}",
        "tag Old;tag New;val x:();functor F[]:->x; \
         functor Inner:->x{bind x={return;()};}functor G:->x{bind Inner;} \
         fn run():Old|New{bind F=G;bind F;Old}fn main(){match run(){Old=>(),New=>()};}",
        "tag Old;tag New;val value:()={return;()};fn run():Old|New{value;Old} \
         fn main(){match run(){Old=>(),New=>()};}",
    ] {
        let bad = source(program);
        assert!(!compile(&bad, "").is_module());
        let rejected = oracle_command(&["--prelude", "", "--elab-only", &bad]);
        assert_eq!(rejected.code, ORACLE_REJECTED, "{}", rejected.stderr);
        assert!(rejected.stderr.contains("cannot return"), "{}", rejected.stderr);
    }
    let good = source("fn q();functor F[]:->q;functor G:->q{bind q=()=>{return;};} \
        fn plain(){return;}fn annotated():(){return;} \
        fn main(){bind F=G;bind F;q();plain();annotated();}");
    let bytes = build_with(&good, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let accepted = oracle_command(&["--prelude", "", &good]);
    assert_eq!(accepted.code, 0, "{}", accepted.stderr);
}

#[test]
fn private_checked_casts_preserve_chained_fixed_pin_meanings() {
    cast_probe(
        "type T;type U;type V;tag Old; \
         assume StructRef{val raw:StructRef;} \
         assume T{val private:w::Ref[Boxed=T];} \
         assume T,U,V{fn q(a:T,b:U,c:V);} \
         functor F[]:Wasm,T,U,V,raw->q; \
         functor G:Wasm,Gc,T,U,V,raw->q{ \
           bind private=w::StructCast[Boxed=T](raw).cast(); \
           bind T=U;bind U=V;bind V=Old;bind q=(a,b,c)=>();}",
        "bind Boxed=Old;bind w::Heap; \
         let handle=match Old.boxed(){w::Ref(handle)=>handle};bind raw=handle; \
         bind T=Old;bind U=Old;bind V=Old;bind F=G;bind F;q(Old,Old,Old);",
        false,
    );
}

#[test]
fn opaque_nominal_arguments_do_not_force_their_unused_payloads() {
    // `Bad`'s payload was an unformable one (`type Broken=Broken;`, an alias
    // cycle). Every declaration is now checked as written (criteria section
    // 7), and every tag must have a finite layout (D58), so no such payload
    // is left to write. The payloads are ordinary; what remains is that a
    // phantom argument's payload is not read, and the cast still traps.
    let entry = source("tag Bad(()); \
        type T;assume T{tag Phantom[T](());}fn main(){Phantom[T=Bad](());}");
    let bytes = build_with(&entry, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let reference = oracle_command(&["--prelude", "", &entry]);
    assert_eq!(reference.code, 0, "{}", reference.stderr);
    cast_probe(
        "tag Bad(());tag Other(()); \
         type Element;assume Element{tag Phantom[Element](());}",
        "bind Boxed=Phantom[Element=Bad];bind w::Heap; \
         let boxed=Phantom[Element=Bad](()).boxed(); \
         let raw=match boxed{w::Ref(raw)=>raw}; \
         bind Boxed=Phantom[Element=Other];w::StructCast(raw).cast().deref();",
        true,
    );
}

#[test]
fn latent_unformable_functor_capability_does_not_force_a_fake_interface() {
    let entry = source("type T;tag U;assume T{tag Wrap[T](T);fn offered():Wrap|U;} \
        functor Inner[T]:T->offered;val x:(); \
        functor Outer[]:Inner->x;functor G:Inner->x{bind x=();} \
        fn main(){bind Outer=G;}");
    let bytes = build_with(&entry, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let reference = oracle_command(&["--prelude", "", &entry]);
    assert_eq!(reference.code, 0, "{}", reference.stderr);
}

#[test]
fn fresh_functor_output_generations_retain_their_runtime_types() {
    use moss_tests::{build_with, oracle_command, wasm_file, wasmtime};

    let entry = "tests/regressions/functor_output_generation_cast.moss";
    let bytes = build_with(entry, "lib/prelude.moss");
    let native = wasmtime(&wasm_file(&bytes), None, &[]);
    assert_eq!(native.code, 0, "{}", native.stderr);

    // The proved passes keep Runs: the program runs alike with and without
    // them (`--debug-no-passes`).
    for options in [&[][..], &["--debug-no-passes"][..]] {
        let mut args = options.to_vec();
        args.push(entry);
        let reference = oracle_command(&args);
        assert_eq!(reference.code, 0, "{options:?}: {}", reference.stderr);
    }
}
