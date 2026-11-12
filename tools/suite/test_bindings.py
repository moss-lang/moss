from suite.support import (
    build_with,
    oracle_command,
    sources,
    wasm_file,
    wasmtime,
)

# Bindings retain the fully qualified source provider path.


def test_a_defined_provider_can_be_named_through_a_module_alias():
    dir = sources(
        [
            ("provider.moss", "tag Item;fn make():Item{Item}"),
            (
                "main.moss",
                'import "./provider.moss" as p;fn supplied():p::Item; fn main(){bind supplied=p::make;match supplied(){p::Item=>()};}',
            ),
        ]
    )
    entry = f"{dir}/main.moss"
    bytes = build_with(entry, "")
    assert wasmtime(wasm_file(bytes), None, []).code == 0
    checked = oracle_command(["--prelude", "", entry])
    assert checked.code == 0, f"{checked.stderr}"
