#![deny(clippy::all, clippy::pedantic, clippy::nursery)]

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
				"Expected and actual assembly differ at byte {:04X}:\n\texpected: {:02X}\n\tactual:   {:02X}\nhint: \
				 the bytes before and after are:\n\t{}",
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
						.unwrap_or_else(|| panic!("Instruction {:?} received no size at assembly time", instruction))
						.into()
				],
				|value| value.iter().map(|b| Some(*b)).collect(),
			)
		})
		.collect()
}
