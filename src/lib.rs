#![allow(clippy::needless_doctest_main)]
#![doc = include_str!("../README.md")]

pub mod assembler;
pub use assembler::{assemble_maximized, assemble_minimized};

pub mod assembly;
pub mod opcodes;
pub mod utils;
