//! `wasm_file`, which every module the suite runs passes through, refuses
//! an invalid one before anything reads it.

use moss_tests::try_wasm_file;

/// One function type of `params` i32 parameters (fewer than 16384, for a
/// two-byte count), and one function of it.
fn module(params: usize) -> Vec<u8> {
    let count = [0x80 | (params & 0x7F) as u8, (params >> 7) as u8];
    let mut types = vec![0x01, 0x60];
    types.extend(count);
    types.extend(std::iter::repeat_n(0x7f, params));
    types.push(0x00);
    let size = types.len();
    let mut out = b"\0asm\x01\0\0\0".to_vec();
    out.push(0x01);
    out.extend([0x80 | (size & 0x7F) as u8, (size >> 7) as u8]);
    out.extend(types);
    out.extend([0x03, 0x02, 0x01, 0x00]);
    out.extend([0x0a, 0x04, 0x01, 0x02, 0x00, 0x0b]);
    out
}

#[test]
fn a_valid_module_is_written() {
    try_wasm_file(&module(1000)).unwrap();
}

/// The shadow example's first design had 4,477: valid by the
/// specification, refused by every engine, read by Binaryen.
#[test]
fn too_many_parameters_are_refused() {
    let error = try_wasm_file(&module(4477)).unwrap_err();
    assert!(error.contains("function params size is out of bounds"), "{error}");
}

#[test]
fn only_the_listed_features() {
    let shared_memory = b"\0asm\x01\0\0\0\x05\x04\x01\x03\x01\x01";
    let error = try_wasm_file(shared_memory).unwrap_err();
    assert!(error.contains("threads"), "{error}");
}
