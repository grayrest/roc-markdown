//! G7 gate-zero: a MINIMAL Rust host compiled to wasm32-unknown-unknown and
//! linked (by `roc build --target=wasm32`, via wasm-ld) with a Roc app.
//!
//! This isolates the one untested chain the plan flags at G7 gate zero
//! (plans/2026-07-16-v1-architecture.md "### G7", design log open item 4):
//! Roc -> wasm32 object emission + link against a Rust wasm host through the
//! platform ABI. It deliberately pulls in NONE of the real host
//! (solid-signals / winit / wgpu) — only the direct-symbol roc ABI shims
//! (roc_alloc/dealloc/realloc/dbg/expect_failed/crashed) that every host must
//! provide, delegated to the SAME generated `DefaultAllocators` /
//! `DefaultHandlers` the native host uses (crates/host/src/lib.rs), so the
//! link this proves is representative of the production shims.
//!
//! Observability is wasm-bindgen-FREE: `wasm_main` calls the Roc entrypoint
//! `roc_app`, stashes the returned record, and returns a pointer into linear
//! memory; the JS/wasmtime driver reads the bytes directly (see run.mjs).

#[path = "roc_platform_abi.rs"]
#[allow(warnings)]
mod abi;

use core::ffi::c_void;
use core::ptr;
use core::sync::atomic::{AtomicUsize, Ordering};

/// Allocation gauge (mirrors the native host's ROC_ALLOCS) so the driver can
/// confirm the app's runtime string concat actually hit the host allocator on
/// wasm32 — i.e. the alloc ABI is not just linked but exercised.
static ROC_ALLOCS: AtomicUsize = AtomicUsize::new(0);

/// Host-internal helper context (for `decref`), never seen by compiled Roc.
fn roc_host() -> abi::RocHost {
    abi::make_roc_host(ptr::null_mut())
}

// ---------------------------------------------------------------------------
// Direct symbol ABI required by compiled Roc code (same shims as the native
// host, delegated to the generated defaults).
// ---------------------------------------------------------------------------

#[no_mangle]
pub extern "C" fn roc_alloc(length: usize, alignment: usize) -> *mut c_void {
    ROC_ALLOCS.fetch_add(1, Ordering::Relaxed);
    abi::DefaultAllocators::roc_alloc(ptr::null_mut(), length, alignment)
}

/// # Safety
/// `ptr_` must have come from `roc_alloc` with the same `alignment`.
#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(ptr_: *mut c_void, alignment: usize) {
    abi::DefaultAllocators::roc_dealloc(ptr::null_mut(), ptr_, alignment)
}

/// # Safety
/// `ptr_` must have come from `roc_alloc` with the same `alignment`.
#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    ptr_: *mut c_void,
    new_length: usize,
    alignment: usize,
) -> *mut c_void {
    abi::DefaultAllocators::roc_realloc(ptr::null_mut(), ptr_, new_length, alignment)
}

/// # Safety
/// `bytes` must point to `len` readable bytes for the call.
#[no_mangle]
pub unsafe extern "C" fn roc_dbg(bytes: *const u8, len: usize) {
    abi::DefaultHandlers::roc_dbg(ptr::null_mut(), bytes, len)
}

/// # Safety
/// `bytes` must point to `len` readable bytes for the call.
#[no_mangle]
pub unsafe extern "C" fn roc_expect_failed(bytes: *const u8, len: usize) {
    abi::DefaultHandlers::roc_expect_failed(ptr::null_mut(), bytes, len)
}

/// # Safety
/// `bytes` must point to `len` readable bytes for the call.
#[no_mangle]
pub unsafe extern "C" fn roc_crashed(bytes: *const u8, len: usize) {
    abi::DefaultHandlers::roc_crashed(ptr::null_mut(), bytes, len)
}

// ---------------------------------------------------------------------------
// Hosted symbol (platform `hosted` section): the Roc->Rust call direction.
// ---------------------------------------------------------------------------

/// Hosted `Host.seed!`. A runtime value the compiler cannot const-fold, so the
/// app's downstream string build hits `roc_alloc` for real.
#[no_mangle]
pub extern "C" fn roc_host_seed() -> i64 {
    21
}

// ---------------------------------------------------------------------------
// wasm-bindgen-free observability surface for the driver.
// ---------------------------------------------------------------------------

/// The last app record returned by `roc_app`, kept alive so `wasm_main`'s
/// returned pointer stays valid until the next call (small strings live inline
/// in this struct; big strings live on the roc heap this struct refers to).
static mut LAST: Option<abi::AppForHost> = None;

/// Call the Roc entrypoint and return a pointer to the message bytes in linear
/// memory. Pair with `wasm_result_len`.
#[no_mangle]
pub extern "C" fn wasm_main() -> *const u8 {
    let app = unsafe { abi::roc_app() };
    let ptr = app.message.as_slice().as_ptr();
    unsafe {
        LAST = Some(app);
    }
    ptr
}

/// Length in bytes of the message from the last `wasm_main` call.
#[no_mangle]
pub extern "C" fn wasm_result_len() -> usize {
    unsafe {
        match &*ptr::addr_of!(LAST) {
            Some(app) => app.message.as_slice().len(),
            None => 0,
        }
    }
}

/// The integer field `n` from the last `wasm_main` call (proves scalar +
/// struct-field crossing alongside the heap Str).
#[no_mangle]
pub extern "C" fn wasm_n() -> i64 {
    unsafe {
        match &*ptr::addr_of!(LAST) {
            Some(app) => app.n,
            None => 0,
        }
    }
}

/// Number of `roc_alloc` calls so far (proves the alloc ABI was exercised).
#[no_mangle]
pub extern "C" fn wasm_alloc_count() -> usize {
    ROC_ALLOCS.load(Ordering::Relaxed)
}

/// Release the retained app record (exercises the decref/`roc_dealloc` path).
#[no_mangle]
pub extern "C" fn wasm_release() {
    let host = roc_host();
    unsafe {
        if let Some(app) = (*ptr::addr_of_mut!(LAST)).take() {
            app.decref(&host);
        }
    }
}
