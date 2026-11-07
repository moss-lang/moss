//! The native compiler checks its selected library's runtime contracts. Choosing a
//! custom prelude does not make arbitrary declarations valid primitives.
use moss_tests::{compile, run_module, sources};

fn refused(files: &[(&str, &str)], _message: &str) {
    let dir = sources(files);
    let prelude = format!("{dir}/lib/prelude.moss");
    let entry = format!("{dir}/main.moss");
    let out = compile(&entry, &prelude);
    assert!(!out.is_module(), "malformed custom platform accepted");
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
    let out = compile(&entry, &prelude);
    assert!(out.is_module(), "{}", String::from_utf8_lossy(&out.stdout));
    assert_eq!(run_module(&out.stdout, &[]), "");
}

#[test]
fn unused_ordinary_declarations_do_not_become_primitive_bindings() {
    let dir = sources(&[
        ("lib/wasm.moss", "fn i32_add(x: ()): ();"),
        ("lib/prelude.moss", "import \"./wasm.moss\" as w;"),
        ("main.moss", "fn main() {}"),
    ]);
    let out = compile(&format!("{dir}/main.moss"), &format!("{dir}/lib/prelude.moss"));
    assert!(out.is_module(), "{}", String::from_utf8_lossy(&out.stdout));
}

#[test]
fn wasi_bindings_have_their_host_signature() {
    refused(&[
        ("lib/wasip1.moss", "fn fd_write(x: ()); context Wasi = fd_write;"),
        ("lib/prelude.moss", "import \"./wasip1.moss\" use Wasi;"),
        ("main.moss", "assume Wasi { fn main() {} }"),
    ], "signature");
}
