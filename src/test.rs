#![deny(clippy::all, clippy::pedantic, clippy::nursery)]

extern crate test;
use std::cmp::min;
use std::path::PathBuf;

use test::Bencher;

use crate::cli::default_backend_options;
use crate::parser::instruction::MemoryAddress;
use crate::parser::ProgramElement;
use crate::{pretty_hex, Segments};

#[bench]
fn all_opcodes(bencher: &mut Bencher) {
	bencher.iter(|| test_file("examples/test.spcasm"));
}

#[test]
fn boot_rom() {
	test_file("examples/bootrom.spcasm");
}

#[test]
fn parser() {
	test_file("examples/parse.spcasm");
}

#[test]
fn references() {
	test_file("examples/references.spcasm");
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
		let result = super::run_assembler_with_default_options(error_source);
		println!("running {}...\n{:?}", error_source, result);
		assert!(result.is_err());
	}
}

#[bench]
fn brr_integration(bencher: &mut Bencher) {
	bencher.iter(|| test_file("examples/brr.spcasm"));
}

#[test]
fn cli() {
	trycmd::TestCases::new().case("examples/cli/*.trycmd");
}

fn test_file(file: &str) {
	let (parsed, assembled) = super::run_assembler_into_segments(
		&crate::AssemblyCode::from_file_or_assembly_error(file).unwrap(),
		default_backend_options(),
	)
	.unwrap();
	let expected_binary = assemble_expected_binary(parsed);
	for ((parsed_segment_start, expected_segment), (assembled_segment_start, assembled)) in
		expected_binary.segments.iter().zip(assembled.segments.iter())
	{
		assert_eq!(
			parsed_segment_start, assembled_segment_start,
			"Assembly and AST differ in segments; something has gone wrong!"
		);
		// dbg!(&expected_segment, &assembled);
		for (byte, (expected, actual)) in expected_segment.iter().zip(assembled.iter()).enumerate() {
			if let Some(expected) = expected {
				assert_eq!(
					expected,
					actual,
					"In segment {}: Expected and actual assembly differ at byte {:04X}:\n\texpected: \
					 {:02X}\n\tactual:   {:02X}\nhint: the bytes before and after are:\n\t{}",
					assembled_segment_start,
					byte as MemoryAddress + assembled_segment_start,
					expected,
					actual,
					pretty_hex(&assembled[byte.saturating_sub(4) .. min(assembled.len(), byte + 5)], Some(4))
				);
			}
		}
	}
}

/// Assembles the contents of the expected value comments, which is what the file should assemble to.
fn assemble_expected_binary(instructions: Segments<ProgramElement>) -> Segments<Option<u8>> {
	instructions
		.try_map_segments(|_, program_elements| {
			Ok::<_, ()>(
				program_elements
					.into_iter()
					.flat_map(|program_element| {
						match program_element {
							ProgramElement::Instruction(ref instruction) => instruction.expected_value.clone(),
							ProgramElement::Directive(ref directive) => directive.expected_value.clone(),
							_ => None,
						}
						.map_or_else(
							|| vec![None; program_element.assembled_size()],
							|value| value.iter().map(|b| Some(*b)).collect(),
						)
					})
					.collect(),
			)
		})
		.unwrap() // safe because we can never fail in the mapper function
}
