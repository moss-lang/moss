use std::{ffi::CString, ptr};

use llvm_sys::{
    analysis::{LLVMVerifierFailureAction, LLVMVerifierFailureAction::LLVMAbortProcessAction},
    core::{
        LLVMContextCreate, LLVMContextDispose, LLVMCreateMemoryBufferWithMemoryRangeCopy,
        LLVMDisposeMemoryBuffer, LLVMDisposeMessage, LLVMDumpModule, LLVMGetBufferSize,
        LLVMGetBufferStart, LLVMSetTarget,
    },
    execution_engine::{
        LLVMCreateExecutionEngineForModule, LLVMDisposeExecutionEngine, LLVMGetFunctionAddress,
        LLVMLinkInMCJIT,
    },
    ir_reader::LLVMParseIRInContext,
    target::{
        LLVM_InitializeAllAsmParsers, LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllDisassemblers,
        LLVM_InitializeAllTargetInfos, LLVM_InitializeAllTargetMCs, LLVM_InitializeAllTargets,
        LLVM_InitializeNativeAsmParser, LLVM_InitializeNativeAsmPrinter,
        LLVM_InitializeNativeDisassembler, LLVM_InitializeNativeTarget,
    },
    target_machine::{
        LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine,
        LLVMDisposeTargetMachine, LLVMGetDefaultTargetTriple, LLVMGetTargetFromTriple,
        LLVMRelocMode, LLVMTargetMachineEmitToMemoryBuffer, LLVMTargetRef,
    },
};

use crate::{
    lower::{FndefId, IR, Names},
    prelude::Lib,
};

use std::os::raw::c_char;

const HELLO_LL: &str = r#"declare i32 @puts(ptr noundef)
@.str = private unnamed_addr constant [15 x i8] c"Hello, world!\0A\00"

define i32 @main() {
entry:
  %0 = getelementptr inbounds [15 x i8], ptr @.str, i32 0, i32 0
  %1 = call i32 @puts(ptr %0)
  ret i32 0
}
"#;

fn c(msg: &str) -> CString {
    CString::new(msg).expect("CString")
}

fn init_llvm() {
    // Initialize all native components so MCJIT and codegen can work without external tools.
    unsafe {
        LLVMLinkInMCJIT();
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmPrinters();
        LLVM_InitializeAllAsmParsers();
        LLVM_InitializeAllDisassemblers();
        LLVM_InitializeNativeTarget();
        LLVM_InitializeNativeAsmPrinter();
        LLVM_InitializeNativeAsmParser();
        LLVM_InitializeNativeDisassembler();
    }
}

fn with_module<T>(
    ll: &str,
    f: impl FnOnce(*mut llvm_sys::LLVMModule) -> Result<T, String>,
) -> Result<T, String> {
    unsafe {
        let context = LLVMContextCreate();
        let mut module = ptr::null_mut();
        let buf = LLVMCreateMemoryBufferWithMemoryRangeCopy(
            ll.as_ptr() as *const i8,
            ll.len(),
            c("moss_native_buffer").as_ptr(),
        );
        let mut err: *mut c_char = ptr::null_mut();
        if LLVMParseIRInContext(context, buf, &mut module, &mut err) != 0 {
            let message = if err.is_null() {
                "unknown error parsing IR".to_string()
            } else {
                let s = CString::from_raw(err);
                let owned = s.to_string_lossy().into_owned();
                LLVMDisposeMessage(err);
                owned
            };
            LLVMDisposeMemoryBuffer(buf);
            LLVMContextDispose(context);
            return Err(message);
        }
        LLVMDisposeMemoryBuffer(buf);

        let target_triple = LLVMGetDefaultTargetTriple();
        LLVMSetTarget(module, target_triple);

        let result = f(module);

        LLVMContextDispose(context);
        result
    }
}

fn module_target(triple: *const i8) -> Result<LLVMTargetRef, String> {
    unsafe {
        let mut target = ptr::null_mut();
        let mut err: *mut c_char = ptr::null_mut();
        if LLVMGetTargetFromTriple(triple, &mut target, &mut err) != 0 {
            let message = if err.is_null() {
                "unknown error getting target from triple".to_string()
            } else {
                let s = CString::from_raw(err);
                let owned = s.to_string_lossy().into_owned();
                LLVMDisposeMessage(err);
                owned
            };
            return Err(message);
        }
        Ok(target)
    }
}

fn emit_object(module: *mut llvm_sys::LLVMModule) -> Result<Vec<u8>, String> {
    unsafe {
        let triple = LLVMGetDefaultTargetTriple();
        let target = module_target(triple)?;
        let cpu = c("");
        let features = c("");
        let tm = LLVMCreateTargetMachine(
            target,
            triple,
            cpu.as_ptr(),
            features.as_ptr(),
            LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
            LLVMRelocMode::LLVMRelocStatic,
            LLVMCodeModel::LLVMCodeModelDefault,
        );
        if tm.is_null() {
            return Err("failed to create target machine".to_string());
        }

        let mut err: *mut c_char = ptr::null_mut();
        let mut buf: *mut llvm_sys::LLVMMemoryBuffer = ptr::null_mut();
        let codegen_result = LLVMTargetMachineEmitToMemoryBuffer(
            tm,
            module,
            LLVMCodeGenFileType::LLVMObjectFile,
            &mut err,
            &mut buf,
        );
        LLVMDisposeTargetMachine(tm);
        if codegen_result != 0 {
            let message = if err.is_null() {
                "LLVM codegen failed".to_string()
            } else {
                let s = CString::from_raw(err);
                let owned = s.to_string_lossy().into_owned();
                LLVMDisposeMessage(err);
                owned
            };
            if !buf.is_null() {
                LLVMDisposeMemoryBuffer(buf);
            }
            return Err(message);
        }
        let start = LLVMGetBufferStart(buf) as *const u8;
        let len = LLVMGetBufferSize(buf);
        let slice = std::slice::from_raw_parts(start, len);
        let out = slice.to_vec();
        LLVMDisposeMemoryBuffer(buf);
        Ok(out)
    }
}

fn run_main(module: *mut llvm_sys::LLVMModule) -> Result<i32, String> {
    unsafe {
        let mut engine = ptr::null_mut();
        let mut err: *mut c_char = ptr::null_mut();
        if LLVMCreateExecutionEngineForModule(&mut engine, module, &mut err) != 0 {
            let message = if err.is_null() {
                "failed to create execution engine".to_string()
            } else {
                let s = CString::from_raw(err);
                let owned = s.to_string_lossy().into_owned();
                LLVMDisposeMessage(err);
                owned
            };
            return Err(message);
        }
        let addr = LLVMGetFunctionAddress(engine, c("main").as_ptr());
        if addr == 0 {
            LLVMDisposeExecutionEngine(engine);
            return Err("main function not found".to_string());
        }
        let func: extern "C" fn() -> i32 = std::mem::transmute(addr);
        let ret = func();
        LLVMDisposeExecutionEngine(engine);
        Ok(ret)
    }
}

/// A minimal LLVM-native backend that ignores the Moss IR and materializes a
/// fixed "Hello, world!" module for both running (via MCJIT) and emitting an
/// object file.
pub struct NativeModule {
    ir: &'static str,
}

impl NativeModule {
    pub fn emit_object(&self) -> Result<Vec<u8>, String> {
        init_llvm();
        with_module(self.ir, |module| {
            unsafe {
                // Give helpful diagnostics if the hardcoded IR ever regresses.
                let action: LLVMVerifierFailureAction = LLVMAbortProcessAction;
                let failed = llvm_sys::analysis::LLVMVerifyModule(module, action, ptr::null_mut());
                if failed != 0 {
                    LLVMDumpModule(module);
                    return Err("verification failed".to_string());
                }
            }
            emit_object(module)
        })
    }

    pub fn run(&self) -> Result<i32, String> {
        init_llvm();
        with_module(self.ir, |module| run_main(module))
    }
}

pub fn native(_: &IR, _: &Names, _: Lib, _: FndefId) -> NativeModule {
    NativeModule { ir: HELLO_LL }
}
