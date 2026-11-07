//! Bindings retain the fully qualified source provider path.
use moss_tests::*;

#[test]
fn a_defined_provider_can_be_named_through_a_module_alias() {
    let dir = sources(&[
        ("provider.moss", "tag Item;fn make():Item{Item}"),
        (
            "main.moss",
            "import \"./provider.moss\" as p;fn supplied():p::Item; \
             fn main(){bind supplied=p::make;match supplied(){p::Item=>()};}",
        ),
    ]);
    let entry = format!("{dir}/main.moss");
    let bytes = build_with(&entry, "");
    assert_eq!(wasmtime(&wasm_file(&bytes), None, &[]).code, 0);
    let checked = oracle_command(&["--prelude", "", &entry]);
    assert_eq!(checked.code, 0, "{}", checked.stderr);
}
