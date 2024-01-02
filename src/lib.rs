#![doc = include_str!("../README.md")]
#![feature(
	test,
	result_flattening,
	iterator_try_collect,
	if_let_guard,
	get_mut_unchecked,
	iter_intersperse,
	const_option_ext,
	const_for,
	let_chains,
	slice_as_chunks,
	exact_size_is_empty,
	maybe_uninit_uninit_array,
	const_trait_impl,
	extract_if,
	extend_one,
	try_blocks,
	option_get_or_insert_default,
	maybe_uninit_array_assume_init
)]
#![allow(non_upper_case_globals)]

#[allow(unused)]
use flexstr::{shared_str, IntoSharedStr, SharedStr, ToSharedStr};

#[macro_use] extern crate lalrpop_util;

pub(crate) use change::Change;
#[allow(clippy::wildcard_imports)]
pub use common::*;
pub use segments::Segments;
pub use source::AssemblyCode;

/// Just like -Werror on C(++) compilers, make ALL THE WARNINGS INTO ERRORS!
#[macro_export]
macro_rules! w_error {
	($vis:vis mod $modname:ident) => {
		#[deny(missing_docs, unused, clippy::all, clippy::pedantic, clippy::nursery, rustdoc::all)]
		$vis mod $modname;
	};
}

w_error!(pub mod assembler);
w_error!(pub mod brr);
w_error!(pub mod cli);
w_error!(mod common);
w_error!(mod default_hacks);
w_error!(mod directive);
#[cfg(feature = "binaries")]
w_error!(pub mod elf);
w_error!(mod error);
w_error!(pub mod sema);
w_error!(pub mod parser);
w_error!(mod segments);
w_error!(mod source);
w_error!(mod change);

#[cfg(feature = "binaries")]
shadow_rs::shadow!(buildinfo);

#[cfg(test)]
w_error!(mod test);

#[cfg(feature = "binaries")]
w_error!(mod spcasm);

#[cfg(feature = "binaries")]
#[allow(unused)]
fn main() -> miette::Result<()> {
	spcasm::main()
}
