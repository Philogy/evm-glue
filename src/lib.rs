#![allow(clippy::needless_doctest_main)]
#![doc = include_str!("../README.md")]

pub mod assembler;
pub use assembler::{assemble_full, validate_asm, AssembleError, AssembleResult};

pub mod assembly;
pub mod opcodes;
pub mod utils;
