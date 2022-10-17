//! SPC700 assembler.

#![allow(stable_features)]
#![feature(
	test,
	result_flattening,
	is_some_and,
	iterator_try_collect,
	if_let_guard,
	int_log,
	get_mut_unchecked,
	iter_intersperse,
	const_option_ext,
	const_for,
	let_chains,
	option_result_contains,
	slice_as_chunks,
	exact_size_is_empty,
	maybe_uninit_uninit_array,
	const_trait_impl,
	drain_filter,
	maybe_uninit_array_assume_init
)]
#![deny(missing_docs)]
#![allow(non_upper_case_globals, unused, clippy::all, clippy::pedantic, clippy::nursery)]

#[macro_use] extern crate lalrpop_util;

use std::fs::File;
use std::io::Write;

#[allow(clippy::wildcard_imports)]
pub use common::*;

pub mod assembler;
pub mod brr;
pub mod cli;
mod common;
mod default_hacks;
#[cfg(feature = "binaries")]
pub mod elf;
mod error;
mod lalrpop_adaptor;
mod mcro;
pub mod parser;
lalrpop_mod!(asm);

#[cfg(feature = "binaries")]
fn main() -> miette::Result<()> {
	use clap::Parser;

	use crate::cli::BackendOptions;

	miette::set_hook(Box::new(|_| {
		Box::new(
			miette::MietteHandlerOpts::new().unicode(true).context_lines(3).tab_width(4).with_cause_chain().build(),
		)
	}))?;

	let mut args = cli::SpcasmCli::parse();
	args.warning_flags.expand_all();
	let file_name = args.input;

	let (_, assembled) = run_assembler(&file_name.to_string_lossy(), std::sync::Arc::new(args.warning_flags))?;

	if let Some(outfile) = args.output {
		let mut outfile: Box<dyn Write> = if outfile.to_string_lossy() == "-" {
			Box::new(std::io::stdout())
		} else {
			Box::new(
				File::options()
					.create(true)
					.truncate(true)
					.write(true)
					.open(outfile)
					.expect("Couldn't open output file"),
			)
		};
		match args.output_format {
			cli::OutputFormat::Elf => elf::write_to_elf(&mut outfile, &assembled).unwrap(),
			cli::OutputFormat::Plain => outfile.write_all(&assembled).unwrap(),
			cli::OutputFormat::HexDump => outfile.write_fmt(format_args!("{}", crate::pretty_hex(&assembled))).unwrap(),
		};
	}
	Ok(())
}
