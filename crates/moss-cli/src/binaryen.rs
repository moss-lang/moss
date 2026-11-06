use std::{
    ffi::{c_char, c_int, c_void},
    ptr, slice,
    sync::Mutex,
};

use anyhow::{anyhow, bail, Result};
use libc::free;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum StaticBytes {
    #[default]
    Memory,
    Globals,
}

pub fn parse_static_bytes(value: &str) -> Result<StaticBytes, String> {
    match value {
        "memory" => Ok(StaticBytes::Memory),
        "globals" => Ok(StaticBytes::Globals),
        _ => Err("expected memory or globals".into()),
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Opt {
    optimize_level: c_int,
    shrink_level: c_int,
    static_bytes: StaticBytes,
}

impl Opt {
    pub const fn new(optimize_level: c_int, shrink_level: c_int) -> Self {
        Self {
            optimize_level,
            shrink_level,
            static_bytes: StaticBytes::Memory,
        }
    }

    pub const fn with_static_bytes(mut self, storage: StaticBytes) -> Self {
        self.static_bytes = storage;
        self
    }
}

pub fn parse_opt(value: &str) -> Result<Opt, String> {
    let (optimize, shrink) = match value {
        "0" => (0, 0),
        "1" => (1, 0),
        "2" => (2, 0),
        "3" => (3, 0),
        "4" => (4, 0),
        "s" => (2, 1),
        "z" => (2, 2),
        other => {
            return Err(format!(
                "invalid optimization level `{other}`; expected 0, 1, 2, 3, 4, s, or z"
            ))
        }
    };
    Ok(Opt::new(optimize, shrink))
}

/// The Wasm features Binaryen may read and write, by their `wasm-opt
/// --enable-` names: the one list the flake and the test suite read
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
    // Binaryen's optimization settings are process-global.
    static OPTIMIZER: Mutex<()> = Mutex::new(());
    let _lock = OPTIMIZER
        .lock()
        .map_err(|_| anyhow!("Binaryen optimizer lock was poisoned"))?;
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
        if opt.optimize_level >= 2 {
            let passes = [
                b"inlining-optimizing\0".as_ptr().cast(),
                b"gufa-optimizing\0".as_ptr().cast(),
                b"dae-optimizing\0".as_ptr().cast(),
                b"simplify-locals\0".as_ptr().cast(),
                b"vacuum\0".as_ptr().cast(),
            ];
            // Expose small construction helpers even when the selected level
            // normally declines inlining them (notably -O2, -Os and -Oz).
            let inline_limit = BinaryenGetAlwaysInlineMaxSize();
            BinaryenSetAlwaysInlineMaxSize(inline_limit.max(20));
            BinaryenModuleRunPasses(module, passes.as_ptr(), passes.len() as u32);
            BinaryenSetAlwaysInlineMaxSize(inline_limit);
            let sites = match opt.static_bytes {
                StaticBytes::Memory => moss_pool_static_bytes(module),
                StaticBytes::Globals => moss_pool_static_byte_globals(module),
            };
            if sites != usize::MAX
                && sites > 0
                && opt.static_bytes == StaticBytes::Globals
                && moss_static_array_types_closed(module)
            {
                // Pool first, while source type distinctions still aid analysis.
                let closed = BinaryenGetClosedWorld();
                BinaryenSetClosedWorld(true);
                let merged = [
                    b"type-merging\0".as_ptr().cast(),
                    b"duplicate-function-elimination\0".as_ptr().cast(),
                    b"inlining-optimizing\0".as_ptr().cast(),
                    b"gufa-optimizing\0".as_ptr().cast(),
                    b"dae-optimizing\0".as_ptr().cast(),
                    b"precompute\0".as_ptr().cast(),
                    b"vacuum\0".as_ptr().cast(),
                    b"coalesce-locals\0".as_ptr().cast(),
                ];
                BinaryenModuleRunPasses(module, merged.as_ptr(), merged.len() as u32);
                let ssa = [
                    b"precompute\0".as_ptr().cast(),
                    b"vacuum\0".as_ptr().cast(),
                    b"ssa\0".as_ptr().cast(),
                ];
                let cleanup = [
                    b"optimize-instructions\0".as_ptr().cast(),
                    b"simplify-locals\0".as_ptr().cast(),
                    b"precompute\0".as_ptr().cast(),
                    b"vacuum\0".as_ptr().cast(),
                    b"dae-optimizing\0".as_ptr().cast(),
                    b"duplicate-function-elimination\0".as_ptr().cast(),
                    b"coalesce-locals\0".as_ptr().cast(),
                ];
                let mut failed = false;
                // Parameter contraction exposes tuple-result contraction, and
                // vice versa. Keep this staging bounded. SSA is deliberately
                // after read factoring: local coalescing first makes equivalent
                // indices syntactically identical; SSA then proves captures.
                for _ in 0..3 {
                    let factored = moss_simplify_array_dispatch(module);
                    let first = moss_coalesce_array_unions(module);
                    if factored == usize::MAX || first == usize::MAX {
                        failed = true;
                        break;
                    }
                    BinaryenModuleRunPasses(module, ssa.as_ptr(), ssa.len() as u32);
                    let second = moss_coalesce_array_unions(module);
                    if second == usize::MAX {
                        failed = true;
                        break;
                    }
                    BinaryenModuleRunPasses(module, cleanup.as_ptr(), cleanup.len() as u32);
                    if factored == 0 && first == 0 && second == 0 {
                        break;
                    }
                }
                BinaryenSetClosedWorld(closed);
                if failed {
                    BinaryenModuleDispose(module);
                    bail!("array union contraction failed; this is a compiler bug");
                }
            }
            if sites == usize::MAX || !BinaryenModuleValidate(module) {
                BinaryenModuleDispose(module);
                bail!("static byte lowering failed; this is a compiler bug");
            }
        }
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

#[cfg(test)]
#[path = "static_bytes_tests.rs"]
mod static_bytes_tests;

extern "C" {
    fn moss_pool_static_bytes(module: BinaryenModuleRef) -> usize;
    fn moss_pool_static_byte_globals(module: BinaryenModuleRef) -> usize;
    fn moss_simplify_array_dispatch(module: BinaryenModuleRef) -> usize;
    fn moss_coalesce_array_unions(module: BinaryenModuleRef) -> usize;
    fn moss_static_array_types_closed(module: BinaryenModuleRef) -> bool;
    fn BinaryenGetClosedWorld() -> bool;
    fn BinaryenSetClosedWorld(on: bool);
    fn BinaryenModuleValidate(module: BinaryenModuleRef) -> bool;
    fn BinaryenModuleRunPasses(module: BinaryenModuleRef, passes: *const *const c_char, count: u32);
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
    fn BinaryenGetAlwaysInlineMaxSize() -> u32;
    fn BinaryenSetAlwaysInlineMaxSize(size: u32);
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
