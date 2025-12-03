use std::{
    ffi::{c_char, c_int, c_void},
    ptr, slice,
};

use anyhow::bail;
use libc::free;

#[derive(Clone, Copy)]
pub struct Opt {
    pub optimize_level: c_int,
    pub shrink_level: c_int,
}

pub fn parse_opt(value: &str) -> Result<Opt, String> {
    match value {
        "0" => Ok(Opt {
            optimize_level: 0,
            shrink_level: 0,
        }),
        "1" => Ok(Opt {
            optimize_level: 1,
            shrink_level: 0,
        }),
        "2" => Ok(Opt {
            optimize_level: 2,
            shrink_level: 0,
        }),
        "3" => Ok(Opt {
            optimize_level: 3,
            shrink_level: 0,
        }),
        "s" | "Os" => Ok(Opt {
            optimize_level: 2,
            shrink_level: 1,
        }),
        "z" | "Oz" => Ok(Opt {
            optimize_level: 2,
            shrink_level: 2,
        }),
        other => Err(format!(
            "invalid opt level `{other}`; expected one of 0,1,2,3,s,z"
        )),
    }
}

pub fn optimize_wasm(mut bytes: Vec<u8>, opt: Opt) -> anyhow::Result<Vec<u8>> {
    unsafe {
        let module = BinaryenModuleReadWithFeatures(
            bytes.as_mut_ptr() as *mut c_char,
            bytes.len(),
            BinaryenFeatureAll(),
        );
        if module.is_null() {
            bail!("Binaryen could not read module");
        }

        BinaryenSetOptimizeLevel(opt.optimize_level);
        BinaryenSetShrinkLevel(opt.shrink_level);
        BinaryenSetDebugInfo(false);
        BinaryenModuleOptimize(module);

        let result = BinaryenModuleAllocateAndWrite(module, ptr::null());
        BinaryenModuleDispose(module);

        if result.binary.is_null() || result.binaryBytes == 0 {
            bail!("Binaryen failed to emit optimized module");
        }
        let optimized =
            slice::from_raw_parts(result.binary as *const u8, result.binaryBytes).to_vec();

        // The Binaryen C API allocates with malloc().
        free(result.binary);
        if !result.sourceMap.is_null() {
            free(result.sourceMap as *mut c_void);
        }
        Ok(optimized)
    }
}

#[repr(C)]
#[allow(non_snake_case)]
struct BinaryenModuleAllocateAndWriteResult {
    binary: *mut c_void,
    binaryBytes: usize,
    sourceMap: *mut c_char,
}

type BinaryenFeatures = u32;
type BinaryenModuleRef = *mut c_void;

unsafe extern "C" {
    fn BinaryenFeatureAll() -> BinaryenFeatures;

    fn BinaryenSetOptimizeLevel(level: c_int);
    fn BinaryenSetShrinkLevel(level: c_int);
    fn BinaryenSetDebugInfo(on: bool);

    fn BinaryenModuleReadWithFeatures(
        input: *mut c_char,
        input_size: usize,
        feature_set: BinaryenFeatures,
    ) -> BinaryenModuleRef;
    fn BinaryenModuleDispose(module: BinaryenModuleRef);
    fn BinaryenModuleOptimize(module: BinaryenModuleRef);
    fn BinaryenModuleAllocateAndWrite(
        module: BinaryenModuleRef,
        source_map_url: *const c_char,
    ) -> BinaryenModuleAllocateAndWriteResult;
}
