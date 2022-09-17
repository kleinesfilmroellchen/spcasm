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
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![deny(missing_docs)]
#![allow(non_upper_case_globals, unused)]

#[macro_use] extern crate lalrpop_util;

use std::cmp::min;

pub mod assembler;
pub mod brr;
pub mod elf;
mod error;
mod lalrpop_adaptor;
mod mcro;
pub mod parser;
lalrpop_mod!(asm);

pub use error::{AssemblyCode, AssemblyError};
pub use mcro::Macro;

fn pretty_hex(bytes: &[u8]) -> String {
	let mut string = String::new();
	// need approximately high nibble + low nibble + ' ' per byte
	string.reserve(bytes.len() * 3);
	let mut index = 0;
	while index * 16 < bytes.len() {
		let section = &bytes[index * 16 .. min((index + 1) * 16, bytes.len())];
		for byte in section {
			string += &format!(" {:02X}", byte);
		}
		string.push('\n');
		index += 1;
	}
	string
}
