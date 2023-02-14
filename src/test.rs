extern crate test;
use std::cmp::min;

use test::Bencher;

use crate::cli::default_backend_options;
use crate::parser::instruction::MemoryAddress;
use crate::parser::ProgramElement;
use crate::{pretty_hex, Segments};

#[bench]
fn all_opcodes(bencher: &mut Bencher) {
	bencher.iter(|| test_file("tests/opcodes.s"));
}

#[test]
fn boot_rom() {
	test_file("include/bootrom.s");
}

#[test]
fn assembler() {
	let sources = std::fs::read_dir("tests").unwrap();
	for source in sources {
		let source = source.unwrap().path();
		let source = &*source.to_string_lossy();
		if source.ends_with(".spcasmtest") {
			println!("assembling {} ...", source);
			test_file(source);
		} else {
			println!("skipping file {} (not a test)", source);
		}
	}
}

#[test]
fn errors() {
	let error_sources = std::fs::read_dir("tests/errors").unwrap();
	for error_source in error_sources {
		let error_source = error_source.unwrap().path();
		let error_source = &*error_source.to_string_lossy();
		if error_source.ends_with(".spcasmtest") {
			let result = super::run_assembler_with_default_options(error_source);
			println!("checking {} for errors ...\n{:?}", error_source, result);
			assert!(result.is_err());
		} else {
			println!("skipping file {} (not an error test)", error_source);
		}
	}
}

#[bench]
fn brr_integration(bencher: &mut Bencher) {
	bencher.iter(|| test_file("tests/brr.spcasmtest"));
}

#[test]
fn spcasm_cli() {
	trycmd::TestCases::new().case("tests/cli/*.trycmd");
}

#[test]
fn documented_cli()  {
	trycmd::TestCases::new().case("doc/src/usage.md");	
	trycmd::TestCases::new().case("README.md");	
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
					"In segment {:04X}: Expected and actual assembly differ at byte {:04X}:\n\texpected: \
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
