//! SPC700 assembler.

#![allow(stable_features)]
#![feature(
	test,
	result_flattening,
	is_some_and,
	get_mut_unchecked,
	iterator_try_collect,
	if_let_guard,
	int_log,
	const_option_ext,
	const_for,
	iter_intersperse,
	let_chains,
	option_result_contains,
	slice_as_chunks,
	exact_size_is_empty,
	maybe_uninit_uninit_array,
	const_trait_impl,
	drain_filter,
	maybe_uninit_array_assume_init
)]
#![allow(non_upper_case_globals, unused)]
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]

#[macro_use] extern crate lalrpop_util;

#[allow(clippy::wildcard_imports)]
pub use common::*;
pub use segments::Segments;

#[deny(missing_docs)] pub mod assembler;
#[deny(missing_docs)] pub mod brr;
#[deny(missing_docs)] pub mod cli;
#[deny(missing_docs)] mod common;
#[deny(missing_docs)] mod default_hacks;
#[deny(missing_docs)] mod directive;
#[cfg(feature = "binaries")]
#[deny(missing_docs)]
pub mod elf;
#[deny(missing_docs)] mod error;
#[deny(missing_docs)]
mod lalrpop_adaptor;
#[deny(missing_docs)] pub mod parser;
#[deny(missing_docs)] mod segments;
lalrpop_mod!(
	#[allow(clippy::all, clippy::pedantic, clippy::nursery, missing_docs)]
	asm
);

shadow_rs::shadow!(buildinfo);

#[cfg(test)] mod test;
