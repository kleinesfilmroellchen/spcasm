#![doc = include_str!("../README.md")]
#![feature(
	test,
	iterator_try_collect,
	if_let_guard,
	get_mut_unchecked,
	iter_intersperse,
	const_for,
	exact_size_is_empty,
	const_trait_impl,
	extend_one,
	try_blocks,
	macro_derive,
	maybe_uninit_array_assume_init,
	adt_const_params
)]

#[allow(unused)]
use flexstr::{IntoSharedStr, SharedStr, ToSharedStr, shared_str};

#[macro_use] extern crate lalrpop_util;

pub(crate) use change::Change;
#[allow(clippy::wildcard_imports)]
pub use common::*;
pub use segments::Segments;
pub use source::AssemblyCode;

pub mod assembler;
pub mod brr;
mod change;
pub mod cli;
mod common;
mod default_hacks;
mod directive;
#[cfg(feature = "binaries")]
pub mod elf;
mod error;
pub mod parser;
mod segments;
pub mod sema;
mod source;

// can't use the shadow_rs macro for this purpose since it doesn't include documentation on all its elements and we
// therefore have to allow missing docs.
#[cfg(feature = "binaries")]
#[allow(missing_docs, clippy::all, clippy::nursery, clippy::pedantic)]
pub mod buildinfo {
	include!(concat!(env!("OUT_DIR"), "/shadow.rs"));
}

#[cfg(test)] mod test;
