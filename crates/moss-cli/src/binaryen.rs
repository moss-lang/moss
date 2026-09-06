use std::{
    ffi::{c_char, c_int, c_void},
    ptr, slice,
};

use anyhow::{anyhow, bail, Result};
use libc::free;

#[derive(Clone, Copy, Debug)]
pub struct Opt {
    optimize_level: c_int,
    shrink_level: c_int,
}

impl Opt {
    #[cfg(not(moss_embedded_compiler))]
    pub const fn new(optimize_level: c_int, shrink_level: c_int) -> Self {
        Self {
            optimize_level,
            shrink_level,
        }
    }
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
        "4" => Ok(Opt {
            optimize_level: 4,
            shrink_level: 0,
        }),
        "s" => Ok(Opt {
            optimize_level: 2,
            shrink_level: 1,
        }),
        "z" => Ok(Opt {
            optimize_level: 2,
            shrink_level: 2,
        }),
        other => Err(format!(
            "invalid optimization level `{other}`; expected 0, 1, 2, 3, 4, s, or z"
        )),
    }
}

/// The Wasm features Binaryen may read and write, by their `wasm-opt
/// --enable-` names: the one list the flake and the bootstrap tests read
/// too, so no caller has to be kept in step by hand. The file says what it
/// covers, and why `BinaryenFeatureAll` is not an option.
const WASM_FEATURES: &str = include_str!("../../../wasm-features.txt");

fn features() -> Result<BinaryenFeatures> {
    let mut features = unsafe { BinaryenFeatureMVP() };
    for line in WASM_FEATURES.lines() {
        let name = line.trim();
        if name.is_empty() || name.starts_with('#') {
            continue;
        }
        features |= unsafe { feature(name) }.ok_or_else(|| {
            anyhow!("wasm-features.txt names a Wasm feature this driver does not know: `{name}`")
        })?;
    }
    Ok(features)
}

/// One feature by name, with whatever `wasm-opt --enable-<name>` implies.
unsafe fn feature(name: &str) -> Option<BinaryenFeatures> {
    Some(match name {
        // Wasm 2.0.
        "mutable-globals" => BinaryenFeatureMutableGlobals(),
        "nontrapping-float-to-int" => BinaryenFeatureNontrappingFPToInt(),
        "sign-ext" => BinaryenFeatureSignExt(),
        "multivalue" => BinaryenFeatureMultivalue(),
        // Binaryen keeps `memory.copy` and `memory.fill` as a feature of
        // their own, which `--enable-bulk-memory` switches on as well.
        "bulk-memory" => BinaryenFeatureBulkMemory() | BinaryenFeatureBulkMemoryOpt(),
        "bulk-memory-opt" => BinaryenFeatureBulkMemoryOpt(),
        "reference-types" => BinaryenFeatureReferenceTypes(),
        "call-indirect-overlong" => BinaryenFeatureCallIndirectOverlong(),
        "simd" => BinaryenFeatureSIMD128(),
        // Wasm 3.0.
        "tail-call" => BinaryenFeatureTailCall(),
        "extended-const" => BinaryenFeatureExtendedConst(),
        "multimemory" => BinaryenFeatureMultiMemory(),
        "memory64" => BinaryenFeatureMemory64(),
        "gc" => BinaryenFeatureGC(),
        "exception-handling" => BinaryenFeatureExceptionHandling(),
        "relaxed-simd" => BinaryenFeatureRelaxedSIMD(),
        "threads" => BinaryenFeatureAtomics(),
        _ => return None,
    })
}

pub fn optimize_wasm(mut bytes: Vec<u8>, opt: Opt) -> Result<Vec<u8>> {
    let features = features()?;
    unsafe {
        let module =
            BinaryenModuleReadWithFeatures(bytes.as_mut_ptr().cast(), bytes.len(), features);
        if module.is_null() {
            bail!("Binaryen could not read the compiler output");
        }
        BinaryenSetOptimizeLevel(opt.optimize_level);
        BinaryenSetShrinkLevel(opt.shrink_level);
        BinaryenSetDebugInfo(false);
        BinaryenModuleOptimize(module);
        let result = BinaryenModuleAllocateAndWrite(module, ptr::null());
        BinaryenModuleDispose(module);
        if result.binary.is_null() || result.binary_bytes == 0 {
            bail!("Binaryen failed to emit the optimized module");
        }
        let optimized =
            slice::from_raw_parts(result.binary.cast::<u8>(), result.binary_bytes).to_vec();
        free(result.binary);
        if !result.source_map.is_null() {
            free(result.source_map.cast());
        }
        Ok(optimized)
    }
}

#[repr(C)]
struct BinaryenModuleAllocateAndWriteResult {
    binary: *mut c_void,
    binary_bytes: usize,
    source_map: *mut c_char,
}

type BinaryenFeatures = u32;
type BinaryenModuleRef = *mut c_void;

extern "C" {
    fn BinaryenFeatureMVP() -> BinaryenFeatures;
    fn BinaryenFeatureMutableGlobals() -> BinaryenFeatures;
    fn BinaryenFeatureNontrappingFPToInt() -> BinaryenFeatures;
    fn BinaryenFeatureSignExt() -> BinaryenFeatures;
    fn BinaryenFeatureMultivalue() -> BinaryenFeatures;
    fn BinaryenFeatureBulkMemory() -> BinaryenFeatures;
    fn BinaryenFeatureBulkMemoryOpt() -> BinaryenFeatures;
    fn BinaryenFeatureReferenceTypes() -> BinaryenFeatures;
    fn BinaryenFeatureCallIndirectOverlong() -> BinaryenFeatures;
    fn BinaryenFeatureSIMD128() -> BinaryenFeatures;
    fn BinaryenFeatureTailCall() -> BinaryenFeatures;
    fn BinaryenFeatureExtendedConst() -> BinaryenFeatures;
    fn BinaryenFeatureMultiMemory() -> BinaryenFeatures;
    fn BinaryenFeatureMemory64() -> BinaryenFeatures;
    fn BinaryenFeatureGC() -> BinaryenFeatures;
    fn BinaryenFeatureExceptionHandling() -> BinaryenFeatures;
    fn BinaryenFeatureRelaxedSIMD() -> BinaryenFeatures;
    fn BinaryenFeatureAtomics() -> BinaryenFeatures;
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
