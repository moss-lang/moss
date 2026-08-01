//! A small embedding of Wasmtime through its C API.
//!
//! We link the Wasmtime C API rather than building the `wasmtime` crate, so
//! Nix supplies the engine just as it supplies Binaryen. The C API covers WASI
//! preview 1, which is all the Moss compiler and the programs it emits use.
//!
//! The bindings at the bottom of this file mirror the headers of Wasmtime 44,
//! the version the flake pins; re-read them when that moves to a new major
//! version.

#![allow(non_camel_case_types)]

use std::{
    ffi::{c_char, c_int, c_void, CString},
    path::Path,
    ptr, slice,
    sync::{Arc, Mutex},
};

use anyhow::{anyhow, bail, Result};

/// A compilation environment and its compiled code.
pub struct Engine {
    raw: *mut wasm_engine_t,
}

impl Engine {
    pub fn new() -> Result<Self> {
        let raw = unsafe { wasm_engine_new() };
        if raw.is_null() {
            bail!("could not create a Wasmtime engine");
        }
        Ok(Self { raw })
    }
}

impl Drop for Engine {
    fn drop(&mut self) {
        unsafe { wasm_engine_delete(self.raw) }
    }
}

pub struct Module {
    raw: *mut wasmtime_module_t,
}

impl Module {
    /// Compiles a WebAssembly binary.
    pub fn new(engine: &Engine, wasm: &[u8]) -> Result<Self> {
        let mut raw = ptr::null_mut();
        unsafe {
            check(wasmtime_module_new(
                engine.raw,
                wasm.as_ptr(),
                wasm.len(),
                &mut raw,
            ))?;
        }
        Ok(Self { raw })
    }

    /// Loads code that was compiled ahead of time.
    ///
    /// # Safety
    ///
    /// `bytes` must come from `wasmtime compile` run with the same Wasmtime
    /// version, configuration, and target as the library this binary links.
    #[cfg(moss_embedded_compiler)]
    pub unsafe fn deserialize(engine: &Engine, bytes: &[u8]) -> Result<Self> {
        let mut raw = ptr::null_mut();
        check(wasmtime_module_deserialize(
            engine.raw,
            bytes.as_ptr(),
            bytes.len(),
            &mut raw,
        ))?;
        Ok(Self { raw })
    }

    /// Loads code that was compiled ahead of time, mapping it from disk.
    ///
    /// Wasmtime reads the file itself, so it can map the code in rather than
    /// copy it, which `deserialize` has to do for bytes it does not own.
    ///
    /// # Safety
    ///
    /// `path` must name a file that `wasmtime compile` wrote with the same
    /// Wasmtime version, configuration, and target as the library this binary
    /// links.
    #[cfg(not(moss_embedded_compiler))]
    pub unsafe fn deserialize_file(engine: &Engine, path: &Path) -> Result<Self> {
        let mut raw = ptr::null_mut();
        let c_path = CString::new(
            path.to_str()
                .ok_or_else(|| anyhow!("path is not valid UTF-8: {}", path.display()))?,
        )?;
        check(wasmtime_module_deserialize_file(
            engine.raw,
            c_path.as_ptr(),
            &mut raw,
        ))?;
        Ok(Self { raw })
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe { wasmtime_module_delete(self.raw) }
    }
}

/// What a preopened directory allows.
#[derive(Clone, Copy)]
pub enum Access {
    Read,
    ReadWrite,
}

/// The WASI environment a module runs in.
pub struct Wasi {
    raw: *mut wasi_config_t,
}

impl Wasi {
    /// Builds an environment whose `argv` is `args`, with no environment
    /// variables, no open files, and no directories.
    pub fn new(args: &[String]) -> Result<Self> {
        let raw = unsafe { wasi_config_new() };
        if raw.is_null() {
            bail!("could not create a WASI configuration");
        }
        let wasi = Self { raw };
        let argv = args
            .iter()
            .map(|arg| CString::new(arg.as_str()))
            .collect::<Result<Vec<_>, _>>()?;
        let pointers: Vec<*const c_char> = argv.iter().map(|arg| arg.as_ptr()).collect();
        // The configuration copies these strings.
        if !unsafe { wasi_config_set_argv(wasi.raw, pointers.len(), pointers.as_ptr()) } {
            bail!("could not set the WASI arguments");
        }
        Ok(wasi)
    }

    /// Grants access to `host`, which the module sees as `guest`.
    pub fn preopen(&mut self, host: &Path, guest: &str, access: Access) -> Result<()> {
        let (dir, file) = match access {
            Access::Read => (DIR_PERMS_READ, FILE_PERMS_READ),
            Access::ReadWrite => (
                DIR_PERMS_READ | DIR_PERMS_WRITE,
                FILE_PERMS_READ | FILE_PERMS_WRITE,
            ),
        };
        let path = CString::new(
            host.to_str()
                .ok_or_else(|| anyhow!("path is not valid UTF-8: {}", host.display()))?,
        )?;
        let name = CString::new(guest)?;
        if !unsafe { wasi_config_preopen_dir(self.raw, path.as_ptr(), name.as_ptr(), dir, file) } {
            bail!("could not open directory: {}", host.display());
        }
        Ok(())
    }

    pub fn inherit_stdin(&mut self) {
        unsafe { wasi_config_inherit_stdin(self.raw) }
    }

    pub fn inherit_stdout(&mut self) {
        unsafe { wasi_config_inherit_stdout(self.raw) }
    }

    pub fn inherit_stderr(&mut self) {
        unsafe { wasi_config_inherit_stderr(self.raw) }
    }

    /// Collects what the module writes to stdout instead of forwarding it.
    pub fn capture_stdout(&mut self) -> Capture {
        let buffer = Arc::new(Mutex::new(Vec::new()));
        // The finalizer releases this reference when the store goes away.
        let data = Arc::into_raw(Arc::clone(&buffer)).cast_mut().cast();
        unsafe { wasi_config_set_stdout_custom(self.raw, Some(write), data, Some(release)) };
        Capture { buffer }
    }

    /// Hands the configuration to a store, which takes ownership of it.
    fn into_raw(self) -> *mut wasi_config_t {
        let raw = self.raw;
        std::mem::forget(self);
        raw
    }
}

impl Drop for Wasi {
    fn drop(&mut self) {
        unsafe { wasi_config_delete(self.raw) }
    }
}

type Buffer = Mutex<Vec<u8>>;

extern "C" fn write(data: *mut c_void, bytes: *const u8, len: usize) -> isize {
    let buffer = unsafe { &*data.cast::<Buffer>() };
    match buffer.lock() {
        Ok(mut buffer) => {
            buffer.extend_from_slice(unsafe { slice::from_raw_parts(bytes, len) });
            len as isize
        }
        // The negative return values are OS error codes; this is `EIO`.
        Err(_) => -5,
    }
}

extern "C" fn release(data: *mut c_void) {
    drop(unsafe { Arc::from_raw(data.cast::<Buffer>()) });
}

/// The stdout of a module that ran with [`Wasi::capture_stdout`].
pub struct Capture {
    buffer: Arc<Buffer>,
}

impl Capture {
    pub fn into_bytes(self) -> Vec<u8> {
        std::mem::take(&mut *self.buffer.lock().expect("captured stdout was poisoned"))
    }
}

/// Runs `module`'s `_start` and returns the status it exited with.
pub fn run(engine: &Engine, module: &Module, wasi: Wasi) -> Result<i32> {
    unsafe {
        let linker = wasmtime_linker_new(engine.raw);
        let store = wasmtime_store_new(engine.raw, ptr::null_mut(), None);
        let context = wasmtime_store_context(store);
        let result = start(linker, context, module, wasi);
        wasmtime_store_delete(store);
        wasmtime_linker_delete(linker);
        result
    }
}

unsafe fn start(
    linker: *mut wasmtime_linker_t,
    context: *mut wasmtime_context_t,
    module: &Module,
    wasi: Wasi,
) -> Result<i32> {
    check(wasmtime_linker_define_wasi(linker))?;
    check(wasmtime_context_set_wasi(context, wasi.into_raw()))?;

    let mut instance = wasmtime_instance_t {
        store_id: 0,
        private: 0,
    };
    let mut trap = ptr::null_mut();
    check(wasmtime_linker_instantiate(
        linker,
        context,
        module.raw,
        &mut instance,
        &mut trap,
    ))?;
    trapped(trap)?;

    let name = "_start";
    let mut export = wasmtime_extern_t::empty();
    if !wasmtime_instance_export_get(
        context,
        &instance,
        name.as_ptr().cast(),
        name.len(),
        &mut export,
    ) {
        bail!("module does not export `_start`");
    }
    let kind = export.kind;
    let func = export.of.func;
    wasmtime_extern_delete(&mut export);
    if kind != EXTERN_FUNC {
        bail!("`_start` is not a function");
    }

    let mut trap = ptr::null_mut();
    let error = wasmtime_func_call(
        context,
        &func,
        ptr::null(),
        0,
        ptr::null_mut(),
        0,
        &mut trap,
    );
    if !error.is_null() {
        // Calling `proc_exit` is how a WASI command stops.
        let mut status = 0;
        if wasmtime_error_exit_status(error, &mut status) {
            wasmtime_error_delete(error);
            return Ok(status);
        }
    }
    check(error)?;
    trapped(trap)?;
    Ok(0)
}

unsafe fn check(error: *mut wasmtime_error_t) -> Result<()> {
    if error.is_null() {
        return Ok(());
    }
    let mut message = wasm_byte_vec_t::empty();
    wasmtime_error_message(error, &mut message);
    let text = take_message(&mut message);
    wasmtime_error_delete(error);
    Err(anyhow!(text))
}

unsafe fn trapped(trap: *mut wasm_trap_t) -> Result<()> {
    if trap.is_null() {
        return Ok(());
    }
    let mut message = wasm_byte_vec_t::empty();
    wasm_trap_message(trap, &mut message);
    let text = take_message(&mut message);
    wasm_trap_delete(trap);
    Err(anyhow!(text))
}

unsafe fn take_message(message: &mut wasm_byte_vec_t) -> String {
    let bytes = if message.data.is_null() {
        &[][..]
    } else {
        slice::from_raw_parts(message.data, message.size)
    };
    // Trap messages are NUL-terminated byte vectors.
    let text = String::from_utf8_lossy(bytes)
        .trim_end_matches('\0')
        .to_owned();
    wasm_byte_vec_delete(message);
    text
}

// Everything below mirrors Wasmtime's C headers: `wasm.h`, `wasi.h`, and the
// `wasmtime/` directory.

#[repr(C)]
struct wasm_engine_t {
    _private: [u8; 0],
}

#[repr(C)]
struct wasm_trap_t {
    _private: [u8; 0],
}

#[repr(C)]
struct wasmtime_module_t {
    _private: [u8; 0],
}

#[repr(C)]
struct wasmtime_linker_t {
    _private: [u8; 0],
}

#[repr(C)]
struct wasmtime_store_t {
    _private: [u8; 0],
}

#[repr(C)]
struct wasmtime_context_t {
    _private: [u8; 0],
}

#[repr(C)]
struct wasmtime_error_t {
    _private: [u8; 0],
}

#[repr(C)]
struct wasi_config_t {
    _private: [u8; 0],
}

#[repr(C)]
struct wasm_byte_vec_t {
    size: usize,
    data: *mut u8,
}

impl wasm_byte_vec_t {
    fn empty() -> Self {
        Self {
            size: 0,
            data: ptr::null_mut(),
        }
    }
}

#[repr(C)]
struct wasmtime_instance_t {
    store_id: u64,
    private: usize,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct wasmtime_func_t {
    store_id: u64,
    private: *mut c_void,
}

#[repr(C)]
union wasmtime_extern_union_t {
    func: wasmtime_func_t,
    /// Wasmtime's widest union member is 24 bytes; this reserves enough room
    /// that a library with a wider one cannot write past the end.
    reserved: [u64; 8],
}

#[repr(C)]
struct wasmtime_extern_t {
    kind: u8,
    of: wasmtime_extern_union_t,
}

impl wasmtime_extern_t {
    fn empty() -> Self {
        Self {
            kind: u8::MAX,
            of: wasmtime_extern_union_t { reserved: [0; 8] },
        }
    }
}

const EXTERN_FUNC: u8 = 0;

type wasi_dir_perms = usize;
const DIR_PERMS_READ: wasi_dir_perms = 1;
const DIR_PERMS_WRITE: wasi_dir_perms = 2;

type wasi_file_perms = usize;
const FILE_PERMS_READ: wasi_file_perms = 1;
const FILE_PERMS_WRITE: wasi_file_perms = 2;

type Writer = extern "C" fn(*mut c_void, *const u8, usize) -> isize;
type Finalizer = extern "C" fn(*mut c_void);

extern "C" {
    fn wasm_engine_new() -> *mut wasm_engine_t;
    fn wasm_engine_delete(engine: *mut wasm_engine_t);

    fn wasmtime_module_new(
        engine: *mut wasm_engine_t,
        wasm: *const u8,
        len: usize,
        module: *mut *mut wasmtime_module_t,
    ) -> *mut wasmtime_error_t;
    #[cfg(moss_embedded_compiler)]
    fn wasmtime_module_deserialize(
        engine: *mut wasm_engine_t,
        bytes: *const u8,
        len: usize,
        module: *mut *mut wasmtime_module_t,
    ) -> *mut wasmtime_error_t;
    #[cfg(not(moss_embedded_compiler))]
    fn wasmtime_module_deserialize_file(
        engine: *mut wasm_engine_t,
        path: *const c_char,
        module: *mut *mut wasmtime_module_t,
    ) -> *mut wasmtime_error_t;
    fn wasmtime_module_delete(module: *mut wasmtime_module_t);

    fn wasmtime_store_new(
        engine: *mut wasm_engine_t,
        data: *mut c_void,
        finalizer: Option<Finalizer>,
    ) -> *mut wasmtime_store_t;
    fn wasmtime_store_context(store: *mut wasmtime_store_t) -> *mut wasmtime_context_t;
    fn wasmtime_store_delete(store: *mut wasmtime_store_t);
    fn wasmtime_context_set_wasi(
        context: *mut wasmtime_context_t,
        wasi: *mut wasi_config_t,
    ) -> *mut wasmtime_error_t;

    fn wasmtime_linker_new(engine: *mut wasm_engine_t) -> *mut wasmtime_linker_t;
    fn wasmtime_linker_delete(linker: *mut wasmtime_linker_t);
    fn wasmtime_linker_define_wasi(linker: *mut wasmtime_linker_t) -> *mut wasmtime_error_t;
    fn wasmtime_linker_instantiate(
        linker: *const wasmtime_linker_t,
        context: *mut wasmtime_context_t,
        module: *const wasmtime_module_t,
        instance: *mut wasmtime_instance_t,
        trap: *mut *mut wasm_trap_t,
    ) -> *mut wasmtime_error_t;

    fn wasmtime_instance_export_get(
        context: *mut wasmtime_context_t,
        instance: *const wasmtime_instance_t,
        name: *const c_char,
        len: usize,
        item: *mut wasmtime_extern_t,
    ) -> bool;
    fn wasmtime_extern_delete(item: *mut wasmtime_extern_t);

    fn wasmtime_func_call(
        context: *mut wasmtime_context_t,
        func: *const wasmtime_func_t,
        args: *const c_void,
        args_len: usize,
        results: *mut c_void,
        results_len: usize,
        trap: *mut *mut wasm_trap_t,
    ) -> *mut wasmtime_error_t;

    fn wasi_config_new() -> *mut wasi_config_t;
    fn wasi_config_delete(config: *mut wasi_config_t);
    fn wasi_config_set_argv(
        config: *mut wasi_config_t,
        argc: usize,
        argv: *const *const c_char,
    ) -> bool;
    fn wasi_config_inherit_stdin(config: *mut wasi_config_t);
    fn wasi_config_inherit_stdout(config: *mut wasi_config_t);
    fn wasi_config_inherit_stderr(config: *mut wasi_config_t);
    fn wasi_config_set_stdout_custom(
        config: *mut wasi_config_t,
        writer: Option<Writer>,
        data: *mut c_void,
        finalizer: Option<Finalizer>,
    );
    fn wasi_config_preopen_dir(
        config: *mut wasi_config_t,
        host: *const c_char,
        guest: *const c_char,
        dir_perms: wasi_dir_perms,
        file_perms: wasi_file_perms,
    ) -> bool;

    fn wasmtime_error_message(error: *const wasmtime_error_t, message: *mut wasm_byte_vec_t);
    fn wasmtime_error_exit_status(error: *const wasmtime_error_t, status: *mut c_int) -> bool;
    fn wasmtime_error_delete(error: *mut wasmtime_error_t);

    fn wasm_trap_message(trap: *const wasm_trap_t, message: *mut wasm_byte_vec_t);
    fn wasm_trap_delete(trap: *mut wasm_trap_t);

    fn wasm_byte_vec_delete(vec: *mut wasm_byte_vec_t);
}
