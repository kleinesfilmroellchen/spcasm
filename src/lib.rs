//! SPC700 assembler.

#![allow(stable_features)]
#![feature(
	test,
	result_flattening,
	is_some_with,
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
	maybe_uninit_array_assume_init
)]
#![deny(missing_docs)]
#![allow(non_upper_case_globals, unused, clippy::all, clippy::pedantic, clippy::nursery)]

#[macro_use] extern crate lalrpop_util;

#[allow(clippy::wildcard_imports)]
pub use common::*;

pub mod assembler;
pub mod brr;
mod cli;
mod common;
mod default_hacks;
#[cfg(feature = "binaries")]
pub mod elf;
mod error;
mod lalrpop_adaptor;
mod mcro;
pub mod parser;
lalrpop_mod!(asm);

#[cfg(test)] mod test;
