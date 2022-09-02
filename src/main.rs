//! SPC700 assembler.

#![allow(stable_features)]
#![feature(
	test,
	result_flattening,
	is_some_with,
	get_mut_unchecked,
	iterator_try_collect,
	if_let_guard,
	let_chains,
	option_result_contains
)]
#![deny(clippy::all, clippy::pedantic, clippy::nursery)]
#![deny(missing_docs)]

use std::cmp::min;
use std::env::args;
use std::fs::{read_to_string, File};
use std::sync::Arc;

use error::AssemblyCode;

pub mod assembler;
pub mod elf;
mod error;
pub mod instruction;
mod label;
pub mod lexer;
mod r#macro;
pub mod parser;
mod program;
mod register;
mod token;
pub use program::ProgramElement;
pub use r#macro::Macro;
pub use register::Register;
pub use token::Token;

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

	let (_, assembled) = run_assembler(file_name)?;
	println!("{}", pretty_hex(&assembled));
	let outfile =
		File::options().create(true).truncate(true).write(true).open(output).expect("Couldn't open output file");
	elf::write_to_elf(&mut std::io::BufWriter::new(outfile), &assembled).unwrap();
	// outfile.write_all(&assembled).expect("I/O error while writing");
	Ok(())
}

fn run_assembler(file_name: String) -> miette::Result<(Vec<ProgramElement>, Vec<u8>)> {
	let contents = read_to_string(file_name.clone()).expect("Couldn't read file contents");
	let source_code = Arc::new(AssemblyCode { name: file_name, text: contents });
	let lexed = lexer::lex(source_code.clone())?;
	let mut env = parser::Environment::new(source_code);
	let mut parsed = env.parse(&lexed)?;
	let assembled = assembler::assemble(&env, &mut parsed)?;
	Ok((parsed, assembled))
}

#[cfg(test)]
mod test {
	extern crate test;
	use std::cmp::min;

	use test::Bencher;

	use crate::pretty_hex;

	#[bench]
	fn test_all_opcodes(bencher: &mut Bencher) {
		bencher.iter(|| test_file("examples/test.spcasm"));
	}

	#[test]
	#[cfg(feature = "test_bootrom")]
	fn test_boot_rom() {
		test_file("examples/bootrom.spcasm");
	}

	#[test]
	fn test_parser() {
		test_file("examples/parse.spcasm");
	}

	fn test_file(file: &str) {
		let (parsed, assembled) = super::run_assembler(file.to_owned()).unwrap();
		let expected_binary = assemble_expected_binary(parsed);
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
	#[allow(clippy::match_wildcard_for_single_variants)]
	fn assemble_expected_binary(instructions: Vec<crate::ProgramElement>) -> Vec<Option<u8>> {
		instructions
			.into_iter()
			.filter_map(|program_element| match program_element {
				crate::ProgramElement::Instruction(instruction) => Some(instruction),
				_ => None,
			})
			.flat_map(|instruction| {
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
