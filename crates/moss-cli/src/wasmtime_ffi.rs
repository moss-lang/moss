#![allow(non_camel_case_types)]

use std::mem::ManuallyDrop;
use std::os::raw::{c_char, c_int, c_void};

pub type wasmtime_val_t = c_void;

#[repr(C)]
pub struct wasm_engine_t {
    _private: [u8; 0],
}

#[repr(C)]
pub struct wasm_config_t {
    _private: [u8; 0],
}

#[repr(C)]
pub struct wasm_trap_t {
    _private: [u8; 0],
}

#[repr(C)]
pub struct wasm_byte_vec_t {
    pub size: usize,
    pub data: *mut u8,
}

pub type wasm_name_t = wasm_byte_vec_t;

#[repr(C)]
pub struct wasmtime_module_t {
    _private: [u8; 0],
}

#[repr(C)]
pub struct wasmtime_linker_t {
    _private: [u8; 0],
}

#[repr(C)]
pub struct wasmtime_store_t {
    _private: [u8; 0],
}

#[repr(C)]
pub struct wasmtime_context_t {
    _private: [u8; 0],
}

#[repr(C)]
pub struct wasmtime_error_t {
    _private: [u8; 0],
}

#[repr(C)]
pub struct wasi_config_t {
    _private: [u8; 0],
}

#[repr(C)]
pub struct wasmtime_func_t {
    pub store_id: u64,
    pub __private: *mut c_void,
}

#[repr(C)]
pub struct wasmtime_instance_t {
    pub store_id: u64,
    pub __private: usize,
}

#[repr(C)]
pub union wasmtime_extern_union_t {
    pub func: ManuallyDrop<wasmtime_func_t>,
}

#[repr(C)]
pub struct wasmtime_extern_t {
    pub kind: u8,
    pub of: wasmtime_extern_union_t,
}

pub const WASMTIME_EXTERN_FUNC: u8 = 0;

pub type wasi_dir_perms = usize;
pub const WASMTIME_WASI_DIR_PERMS_READ: wasi_dir_perms = 1;
pub const WASMTIME_WASI_DIR_PERMS_WRITE: wasi_dir_perms = 2;

pub type wasi_file_perms = usize;
pub const WASMTIME_WASI_FILE_PERMS_READ: wasi_file_perms = 1;
pub const WASMTIME_WASI_FILE_PERMS_WRITE: wasi_file_perms = 2;

unsafe extern "C" {
    pub fn wasm_config_new() -> *mut wasm_config_t;
    pub fn wasm_engine_new_with_config(config: *mut wasm_config_t) -> *mut wasm_engine_t;
    pub fn wasm_engine_delete(engine: *mut wasm_engine_t);

    pub fn wasmtime_store_new(
        engine: *mut wasm_engine_t,
        data: *mut c_void,
        finalizer: Option<extern "C" fn(*mut c_void)>,
    ) -> *mut wasmtime_store_t;
    pub fn wasmtime_store_delete(store: *mut wasmtime_store_t);
    pub fn wasmtime_store_context(store: *mut wasmtime_store_t) -> *mut wasmtime_context_t;

    pub fn wasmtime_linker_new(engine: *mut wasm_engine_t) -> *mut wasmtime_linker_t;
    pub fn wasmtime_linker_delete(linker: *mut wasmtime_linker_t);
    pub fn wasmtime_linker_define_wasi(linker: *mut wasmtime_linker_t) -> *mut wasmtime_error_t;

    pub fn wasmtime_module_new(
        engine: *mut wasm_engine_t,
        wasm: *const u8,
        wasm_len: usize,
        ret: *mut *mut wasmtime_module_t,
    ) -> *mut wasmtime_error_t;
    pub fn wasmtime_module_delete(module: *mut wasmtime_module_t);

    pub fn wasmtime_linker_instantiate(
        linker: *const wasmtime_linker_t,
        store: *mut wasmtime_context_t,
        module: *const wasmtime_module_t,
        instance: *mut wasmtime_instance_t,
        trap: *mut *mut wasm_trap_t,
    ) -> *mut wasmtime_error_t;
    pub fn wasmtime_linker_module(
        linker: *mut wasmtime_linker_t,
        store: *mut wasmtime_context_t,
        name: *const c_char,
        name_len: usize,
        module: *const wasmtime_module_t,
    ) -> *mut wasmtime_error_t;

    pub fn wasmtime_instance_export_get(
        store: *mut wasmtime_context_t,
        instance: *const wasmtime_instance_t,
        name: *const c_char,
        name_len: usize,
        item: *mut wasmtime_extern_t,
    ) -> bool;

    pub fn wasmtime_func_call(
        store: *mut wasmtime_context_t,
        func: *const wasmtime_func_t,
        args: *const wasmtime_val_t,
        nargs: usize,
        results: *mut wasmtime_val_t,
        nresults: usize,
        trap: *mut *mut wasm_trap_t,
    ) -> *mut wasmtime_error_t;
    pub fn wasmtime_linker_get_default(
        linker: *const wasmtime_linker_t,
        store: *mut wasmtime_context_t,
        name: *const c_char,
        name_len: usize,
        func: *mut wasmtime_func_t,
    ) -> *mut wasmtime_error_t;

    pub fn wasmtime_context_set_wasi(
        context: *mut wasmtime_context_t,
        wasi: *mut wasi_config_t,
    ) -> *mut wasmtime_error_t;

    pub fn wasi_config_new() -> *mut wasi_config_t;
    pub fn wasi_config_delete(config: *mut wasi_config_t);
    pub fn wasi_config_set_argv(
        config: *mut wasi_config_t,
        argc: usize,
        argv: *const *const c_char,
    ) -> bool;
    pub fn wasi_config_inherit_argv(config: *mut wasi_config_t);
    pub fn wasi_config_inherit_env(config: *mut wasi_config_t);
    pub fn wasi_config_inherit_stdin(config: *mut wasi_config_t);
    pub fn wasi_config_inherit_stdout(config: *mut wasi_config_t);
    pub fn wasi_config_inherit_stderr(config: *mut wasi_config_t);
    pub fn wasi_config_preopen_dir(
        config: *mut wasi_config_t,
        host_path: *const c_char,
        guest_path: *const c_char,
        dir_perms: wasi_dir_perms,
        file_perms: wasi_file_perms,
    ) -> bool;

    pub fn wasmtime_error_message(error: *const wasmtime_error_t, message: *mut wasm_name_t);
    pub fn wasmtime_error_exit_status(error: *const wasmtime_error_t, status: *mut c_int) -> bool;
    pub fn wasmtime_error_delete(error: *mut wasmtime_error_t);

    pub fn wasm_trap_message(trap: *const wasm_trap_t, out: *mut wasm_name_t);
    pub fn wasm_trap_delete(trap: *mut wasm_trap_t);

    pub fn wasm_byte_vec_delete(vec: *mut wasm_name_t);

    pub fn wasmtime_extern_delete(val: *mut wasmtime_extern_t);
}
