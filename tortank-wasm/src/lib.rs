#![cfg(target_arch = "wasm32")]

use lol_alloc::{AssumeSingleThreaded, FreeListAllocator};

// SAFETY: wasm32 without threads is single threaded.
#[global_allocator]
static ALLOCATOR: AssumeSingleThreaded<FreeListAllocator> =
    unsafe { AssumeSingleThreaded::new(FreeListAllocator::new()) };

pub mod common;
pub mod legacy;
pub mod rdfjs;
