//! SPC700 assembler.

#![allow(stable_features)]
#![feature(
	test,
	result_flattening,
	is_some_with,
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
	maybe_uninit_array_assume_init
)]
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![deny(missing_docs)]
#![allow(non_upper_case_globals, unused_mut)]

#[macro_use] extern crate lalrpop_util;

use std::cmp::min;
use std::env::args;
use std::fs::File;

pub mod assembler;
pub mod brr;
pub mod elf;
mod error;
mod lalrpop_adaptor;
mod mcro;
pub mod parser;
lalrpop_mod!(asm);

use error::AssemblyCode;
pub use mcro::Macro;
use parser::Environment;

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

type AssemblyResult = miette::Result<(std::sync::Arc<std::cell::RefCell<Environment>>, Vec<u8>)>;

fn main() -> miette::Result<()> {
	miette::set_hook(Box::new(|_| {
		Box::new(miette::MietteHandlerOpts::new().unicode(true).context_lines(2).tab_width(4).build())
	}))?;

	if args().nth(1).expect("No file name given") == "--help" {
		eprintln!("Usage: spcasm [--help] INFILE OUTFILE");
		std::process::exit(0);
	}
	let file_name = unsafe { args().nth(1).unwrap_unchecked() };
	let output = args().nth(2).expect("No output file given");

	let (_, assembled) = run_assembler(&file_name)?;
	println!("{}", pretty_hex(&assembled));
	let outfile =
		File::options().create(true).truncate(true).write(true).open(output).expect("Couldn't open output file");
	elf::write_to_elf(&mut std::io::BufWriter::new(outfile), &assembled).unwrap();
	Ok(())
}

fn run_assembler(file_name: &str) -> AssemblyResult {
	let source_code = AssemblyCode::from_file(file_name).expect("Couldn't read file contents");
	let mut env = parser::Environment::new();
	let tokens = parser::lexer::lex(source_code.clone())?;
	let program = parser::Environment::parse(&env, tokens, &source_code)?;
	let assembled = assembler::assemble(&program)?;
	Ok((env, assembled))
}

#[cfg(test)]
mod test {
	extern crate test;
	use std::cmp::min;
	use std::path::PathBuf;

	use test::Bencher;

	use crate::parser::ProgramElement;
	use crate::pretty_hex;

	#[bench]
	fn all_opcodes(bencher: &mut Bencher) {
		bencher.iter(|| test_file("examples/test.spcasm"));
	}

	#[test]
	#[cfg(feature = "test_bootrom")]
	fn boot_rom() {
		test_file("examples/bootrom.spcasm");
	}

	#[test]
	fn parser() {
		test_file("examples/parse.spcasm");
	}

	#[test]
	fn labels() {
		test_file("examples/labels.spcasm");
	}

	#[bench]
	fn source_include(bencher: &mut Bencher) {
		bencher.iter(|| test_file("examples/multifile.spcasm"));
	}

	#[bench]
	fn binary_include(bencher: &mut Bencher) {
		bencher.iter(|| test_file("examples/include.spcasm"));
	}

	#[test]
	fn errors() {
		let error_sources = std::fs::read_dir("examples/errors").unwrap();
		for error_source in error_sources {
			let error_source = error_source.unwrap().path();
			let error_source = &*error_source.to_string_lossy();
			let result = super::run_assembler(error_source);
			println!("running {}...\n{:?}", error_source, result);
			assert!(result.is_err());
		}
	}

	#[bench]
	fn brr_integration(bencher: &mut Bencher) {
		bencher.iter(|| test_file("examples/brr.spcasm"));
	}

	fn test_file(file: &str) {
		let (parsed, assembled) = super::run_assembler(file).unwrap();
		let expected_binary = assemble_expected_binary(
			parsed.borrow().files.get(&PathBuf::from(file).canonicalize().unwrap()).unwrap().borrow().content.clone(),
		);
		for (byte, (expected, actual)) in expected_binary.iter().zip(assembled.iter()).enumerate() {
			if let Some(expected) = expected {
				assert_eq!(
					expected,
					actual,
					"Expected and actual assembly differ at byte {:04X}:\n\texpected: {:02X}\n\tactual:   \
					 {:02X}\nhint: the bytes before and after are:\n\t{}",
					byte,
					expected,
					actual,
					pretty_hex(&assembled[byte.saturating_sub(4) .. min(assembled.len(), byte + 5)])
				);
			}
		}
	}

	/// Assembles the contents of the expected value comments, which is what the file should assemble to.
	fn assemble_expected_binary(instructions: Vec<ProgramElement>) -> Vec<Option<u8>> {
		let mut filtered_instructions = Vec::new();
		for program_element in instructions {
			match program_element {
				ProgramElement::Instruction(instruction) => filtered_instructions.push(instruction),
				ProgramElement::Macro(crate::Macro { value: crate::mcro::MacroValue::End, .. }) => break,
				_ => (),
			}
		}
		filtered_instructions
			.into_iter()
			.flat_map(|instruction| {
				println!("< {:?}", instruction);
				instruction.expected_value.clone().map_or(
					vec![
						None;
						instruction
							.assembled_size
							.unwrap_or_else(|| panic!(
								"Instruction {:?} received no size at assembly time",
								instruction
							))
							.into()
					],
					|value| value.iter().map(|b| Some(*b)).collect(),
				)
			})
			.collect()
	}
}
